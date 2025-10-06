//! Virtual Memory Manager. Maps virtual pages to physical pages. Assumes that
//! 4-level 4KiB paging is used. Based on Volume 3, Chapter 4 of the Intel SDM.
//! 
//! Below you will find some context on the terms that are being used throughout
//! this implementation.
//! 
//! Every address that this or any other program presents to a CPU is called a
//! virtual address. An address that the CPU presents to RAM or other devices on
//! the memory bus is called a physical address. Every address goes through a
//! virtual-to-physical conversion. On x86, this is mainly done via paging.
//! Both the virtual and the physical address spaces are split into equally
//! sized chunks called pages (this OS assumes 4KiB), and the OS is responsible
//! for telling the CPU how to translate each virtual page into a physical one.
//! 
//! This module is responsible for setting up the structures in memory that a
//! CPU expects to see in order to perform this translation. In the eyes of this
//! module, each memory page is either a plain page or one of the four types of
//! translation tables (called, in order of descending level, PML4, PDPT, PD and
//! PT). Each paging table is split into 512 64-bit entries with some flags and
//! the physical address of the corresponding descending table (for PML4s, PDPTs
//! and PDs) or the final page (for PTs).
//! 
//! For every address space, there must be one PML4 and could be up to 512 
//! PDPTs, 512^2 PDs, 512^3 PTs and 512^4 final pages. The physical address of
//! the PML4 goes into a special register called CR3 (Control Register 3). Thus,
//! switching between different values of CR3 switches between different address
//! spaces.
//! 
//! Every virtual address is broken into five parts: four 9-bit indices into
//! each of the four levels of page tables, and a 12-bit offset into the page.
//! When translating an address, the CPU first takes the first index and looks
//! up the entry in the PML4 (the address of which it knows thanks to CR3) with
//! that index. If that entry is marked as present, it follows the physical
//! pointer and arrives at a PDPT. It then repeats this operation for the
//! remaining 3 indices and tables before hopefully arriving at the end page.
//! 
//! There are three layers to this implementation:
//!   - Structural: [`TableEntry`], [`CR3`] describe the structure of the data
//!     that the CPU expects to see;
//!   - Raw: [`RawTable`] creates an unsafe abstraction over tables that has
//!     some useful context not stored in the tables themselves;
//!   - Public: [`AddrSpace`], [`SubSpace`] create a somewhat safe and
//!     nice-to-use abstraction over [`RawTable`]s.
//! 
//! For more information, refer to the Intel Software Developer's Manual. It's
//! got some nice diagrams to help you visualize how this process works.

#![allow(dead_code)]

use core::{
    arch::asm,
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    mem::size_of,
    ops::{Deref, DerefMut, Range, RangeInclusive},
};

use alloc::borrow::ToOwned;
use bitfield_struct::bitfield;
use spin::Mutex;
use strum::VariantArray;

use crate::util::dyn_arr::DynArr;

use crate::target::interface::memmgr::{
    PhysAddr as IfPhysAddr,
    VirtAddr as IfVirtAddr,
    PhysAlloc as IfPhysAlloc,
    AddrSpace as IfAddrSpace,
    AddrSpaceGuard as IfAddrSpaceGuard,
    MemoryParameters as IfMemoryParameters,
    Access,
    AllocReturn,
    Result,
    Error,
    Caching,
};
use crate::target::current::memmgr::{
    PhysAddr,
    VirtAddr,
    MemoryParameters,
    PhysAlloc,
};

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

/// Entries per page table
const ENTRY_CNT: usize = PAGE_SIZE / size_of::<u64>(); // 512
/// Bits per entry index in a virtual address
const BITS_PER_INDEX: usize = 9;
/// Bits per offset into page in a virtual addresses
const BITS_FOR_PAGE_OFFSET: usize = 12;
/// Bit mask for entry indices in a virtual address
const INDEX_MASK: usize = (1 << BITS_PER_INDEX) - 1;
/// Bit mask for offset into page in a virtual address
const PAGE_OFFSET_MASK: usize = (1 << BITS_FOR_PAGE_OFFSET) - 1;

/// Used to represent entries of tables of all 4 levels.
/// 
/// For fields described below, UDD stands for Unless Disallowed Downstream
#[bitfield(u64, debug = false)]
// `#[repr(transparent)]` is set by `bitfield`, so the implied transmutation
// to/from `u64` in `RawTable` is safe
struct TableEntry {
    /// P: 1 = Entry present, 0 = Entry ignored
    present: bool,
    /// W: 1 = Writes allowed UDD, 0 = Writes disallowed
    write: bool,
    /// U/S: 1 = User (CPL=3) access allowed UDD, 0 = Supervisor (CPL<3) access only
    user: bool,
    /// PWT,PCD: Cache access mode for downstream tables (PML4E-PDE) or page (PTE)
    #[bits(2)]
    cache_mode: u8,
    /// A: 1 = Entry has been used for address translation
    accessed: bool,
    /// D: 1 = Software has written to the page (PTE only)
    dirty: bool,
    /// PAT: Extra bit 2 for cache_mode (PTE only)
    #[bits(1)]
    cache_mode_2: u8,
    /// G: Page is global (PTE only)
    global: bool,
    /// Ignored
    #[bits(3)]
    _ign1: u8,
    /// Upper 40 bits of the physical address
    #[bits(40)]
    addr: usize,
    /// Ignored
    #[bits(11)]
    _ign2: u16,
    /// XD: 0 = Execution allowed UDD, 1 = Execution disallowed
    exec_disable: bool,
}

impl Debug for TableEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.present() {
            return write!(f, "0x---------------- present=false")
        }
        write!(f, "{:?} write={} user={} cache={} accessed={} dirty={} global={} xd={}",
            PhysAddr(self.addr() << 12), self.write(), self.user(),
            self.cache_mode() | (self.cache_mode_2() << 2),
            self.accessed(), self.dirty(), self.global(),
            self.exec_disable())
    }
}

/// The value of Control Register 3. Each distinct CR3 value is associated with
/// a different address space.
#[bitfield(u64, debug = false)]
#[derive(PartialEq, Eq)]
struct CR3 {
    /// Ignored
    #[bits(3)]
    _ign1: u8,
    /// PCD,PWT: Cache access mode for downstream PML4
    #[bits(2)]
    cache_mode: u8,
    /// Ignored
    #[bits(7)]
    _ign2: u8,
    /// Upper 40 bits of the physical address of downstream PML4
    #[bits(40)]
    addr: usize,
    /// Reserved
    #[bits(12)]
    _rsvd: u16,
}

impl Debug for CR3 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[32ms\x1b[0m{:#018x}", self.addr() << 12)
    }
}

/// Table attributes stored in the entries referencing those tables.
/// 
/// UDD = Unless Disallowed Downstream
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct TableAttrs {
    /// 1 = Writes allowed UDD, 0 = Writes disallowed
    pub write: bool,
    /// 1 = User (CPL=3) access allowed UDD, 0 = Supervisor (CPL<3) access only
    pub user: bool,
    /// Cache mode
    pub cache: u8,
    /// 1 = Execution allowed UDD, 0 = Execution disallowed
    pub execute: bool,
    /// 0 = Not present in any other mapping, 1 = present in other mappings
    pub global: bool,
}

impl Default for TableAttrs {
    fn default() -> Self {
        TableAttrs {
            write: true,
            user: false,
            cache: 0,
            execute: true,
            global: false
        }
    }
}

impl TableAttrs {
    fn from_entry(entry: &TableEntry) -> Self {
        Self {
            write: entry.write(),
            user: entry.user(),
            cache: entry.cache_mode() | (entry.cache_mode_2() << 2),
            execute: !entry.exec_disable(),
            global: entry.global(),
        }
    }

    fn from_access(access: Access) -> Self {
        Self {
            write: access.write,
            user: access.user,
            cache: 0,
            execute: access.execute,
            global: false,
        }
    }

    fn to_access(self) -> Access {
        Access {
            write: self.write,
            user: self.user,
            execute: self.execute,
            caching: Caching::Uncacheable,
        }
    }

    fn write_to_entry(&self, entry: &mut TableEntry) {
        entry.set_present(true);
        entry.set_write(self.write);
        entry.set_user(self.user);
        entry.set_cache_mode(self.cache & 0b11);
        entry.set_cache_mode_2((self.cache & 0b100) >> 2);
        entry.set_exec_disable(!self.execute);
        entry.set_global(self.global);
    }

    /// This method exists because upstream pages' access bits must be at least
    /// as permissive as those of their downstream counterparts.
    fn bubble_upstream(&mut self, downstream: Self) {
        self.write |= downstream.write;
        self.user |= downstream.user;
        self.execute |= downstream.execute;
    }
}

/// The four levels of tables and the end `Page`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[derive(strum::VariantArray)]
pub enum TableKind {
    Page = 0,
    Pt = 1,
    Pd = 2,
    Pdpt = 3,
    Pml4 = 4,
}

impl TableKind {
    /// Determines the LSB of the virtual address index that this table kind handles
    const fn index_at(&self) -> Option<usize> {
        match *self {
            Self::Page => None,
            _ => Some(BITS_FOR_PAGE_OFFSET + ((*self as usize - 1) * BITS_PER_INDEX))
        }
    }

    /// Determines the bit range in a virtual address this table kind handles
    const fn bit_range(&self) -> Option<Range<usize>> {
        let Some(idx) = self.index_at() else { return None }; // ? isn't allowed in const :/
        Some(idx..(idx + BITS_PER_INDEX + 1))
    }

    /// Extracts the entry index that this table kind would use
    fn entry_idx(&self, addr: VirtAddr) -> Option<usize> {
        let idx = self.index_at()?;
        Some((addr.to_usize() >> idx) & INDEX_MASK)
    }

    /// Determines the table kind the entries of this kind reference
    const fn downstream(&self) -> Option<TableKind> {
        match *self {
            Self::Page => None,
            _ => Some(Self::VARIANTS[*self as usize - 1]),
        }
    }

    /// Determines the table kind the entries of this kind are referenced by
    const fn upstream(&self) -> Option<TableKind> {
        match *self {
            Self::Pml4 => None,
            _ => Some(Self::VARIANTS[*self as usize + 1]),
        }
    }
}

/// Wrapper struct for a table pointer with some additional context not stored
/// in the table itself
#[derive(Eq)]
struct RawTable {
    /// The first virtual address that this table handles
    lowest_addr: VirtAddr,
    /// Physical address of the page
    phys_addr: PhysAddr,
    /// Pointer to the serialized table
    entries: Option<*mut [TableEntry; ENTRY_CNT]>,
    /// The kind of this table
    kind: TableKind,
    /// The attributes of this table
    attrs: TableAttrs,
}

impl PartialEq for RawTable {
    fn eq(&self, other: &Self) -> bool {
        self.lowest_addr == other.lowest_addr &&
        self.phys_addr == other.phys_addr &&
        self.kind == other.kind &&
        self.attrs == other.attrs
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FreeMode {
    TablesOnly,
    TablesAndPages,
}

impl RawTable {
    /// Allocates a new table
    fn new(
        alloc: &mut impl IfPhysAlloc,
        kind: TableKind,
        lowest_addr: VirtAddr,
        access: Access,
    ) -> Result<Self> {
        // allocate page for table
        let p_page = alloc.allocate()?.next().ok_or(Error::OutOfMemory)?;
        let v_page: VirtAddr = p_page.try_into()?;
        unsafe { v_page.clear_page()? };

        // ensure that we've been given a viable starting address
        // it's valid only if all indices starting with the one that we're
        // responsible for are set to zero
        let mut valid = true;
        let mut current_kind = kind;
        while let Some(next_kind) = current_kind.downstream() {
            valid &= current_kind.entry_idx(lowest_addr).unwrap() == 0;
            current_kind = next_kind;
        }
        // the page offset has to be zero too
        valid &= lowest_addr.to_usize() & PAGE_OFFSET_MASK == 0;
        if !valid { return Err(Error::MalformedAddress); }

        Ok(Self {
            lowest_addr,
            phys_addr: p_page,
            entries: if kind == TableKind::Page { None } else { Some(v_page.to_mut_ptr()) },
            kind,
            attrs: TableAttrs::from_access(access),
        })
    }

    /// Deallocates a table and its descendants
    unsafe fn free(&mut self, alloc: &mut impl IfPhysAlloc, mode: FreeMode) -> Result<()> {
        if mode == FreeMode::TablesOnly && self.kind == TableKind::Page { return Ok(()) };
        for i in 0..ENTRY_CNT {
            if let Some(mut ds) = self.get_downstream_by_idx(i) {
                ds.free(alloc, mode)?;
            }
        }
        alloc.deallocate(core::iter::once(self.phys_addr))
    }

    /// Returns an entry that can be used to reference this table
    fn to_parent_entry(&self) -> TableEntry {
        let mut entry = TableEntry::new()
            .with_addr(self.phys_addr.to_usize() >> 12)
            .with_present(true);
        self.attrs.write_to_entry(&mut entry);
        entry
    }

    /// Gets a downstream table by its entry index. Returns None if the table
    /// cannot have children or if the entry is not present.
    unsafe fn get_downstream_by_idx(&self, idx: usize) -> Option<Self> {
        // check entry presence
        let entry: TableEntry = (*self.entries?)[idx];
        if !entry.present() { return None; }
        let ds_kind = self.kind.downstream()?; // this also ensures that we can have children

        // return table
        let phys_addr = PhysAddr(entry.addr() << 12);
        let virt_addr: VirtAddr = phys_addr.try_into().unwrap();
        let entries = if ds_kind == TableKind::Page { None } else { Some(virt_addr.to_mut_ptr()) };
        Some(Self {
            lowest_addr: VirtAddr::from_usize(self.lowest_addr.to_usize() | (idx << self.kind.index_at().unwrap())).unwrap(),
            phys_addr,
            entries,
            kind: ds_kind,
            attrs: TableAttrs::from_entry(&entry),
        })
    }
    
    /// Sets a reference to a downstream table by its entry index. Returns an
    /// error if the table cannot have children or if an incorrect kind was
    /// supplied.
    unsafe fn set_downstream_by_idx(&mut self, idx: usize, table: Option<&RawTable>) -> Result<()> {
        // ensure that we can have children
        let ds_kind = self.kind.downstream().ok_or(Error::UnknownPlatformSpecific)?;
        let Some(entries) = self.entries else { return Err(Error::UnknownPlatformSpecific) };

        // check if None was supplied
        let Some(table) = table else {
            (*entries)[idx] = TableEntry::new().with_present(false);
            return Ok(());
        };

        // check kinds
        if table.kind != ds_kind { return Err(Error::UnknownPlatformSpecific) }

        // set entry
        (*entries)[idx] = table.to_parent_entry();
        Ok(())
    }

    /// Computes the highest address that this table is responsible for
    fn highest_addr(&self) -> VirtAddr {
        // sets all indices in the address that correspond to the current and
        // all downstream kinds to their highest value
        let mut highest_addr = self.lowest_addr.to_usize();
        let mut current_kind = self.kind;
        while let Some(next_kind) = current_kind.downstream() {
            highest_addr |= INDEX_MASK << current_kind.index_at().unwrap();
            current_kind = next_kind;
        }
        highest_addr |= PAGE_OFFSET_MASK;
        if (highest_addr >> 47) & 1 == 1 { highest_addr |= 0xffff_0000_0000_0000 };
        VirtAddr::from_usize(highest_addr).unwrap()
    }

    /// Given a range of virtual addresses, returns the indices of entries in
    /// the current table that handle that address range. The need for this
    /// operation is a bit hard to explain without an example.
    /// 
    /// Suppose that we have a 2-level paging scheme (with only PDs and PTs)
    /// where the tables only hold 4 entries. Virtual addresses in this scheme
    /// consist of three parts: 2 bits for the PDE index, 2 bits for the PTE
    /// index and n bits for an index into the page, but we will skip it as it's
    /// of no interest to us. Suppose also that we want to map the virtual
    /// address range `0b0101 ..= 0b1010` (the index into the page is omitted).
    /// 
    /// In this address range, there are 6 pages that we need to map: `0b0101`,
    /// `0b0110`, `0b0111`, `0b1000`, `0b1001` and `0b1010`. In the following
    /// diagram, table entries of interest to this mapping operation are marked
    /// with an X:
    /// ```diagram
    ///       PD                     PT                   Pages
    ///                                        +----->+------------+
    ///                                        |      |            |
    ///                                        |      +------------+
    ///                                        |
    /// +---- PD ----+    +--->+--- PT 1 ---+  | +--->+------------+
    /// | 0          |    |    | 0          |  | |    |            |
    /// +------------+    |    +------------+  | |    +------------+
    /// | 1        X |----+    | 1        X |--+ |
    /// +------------+         +------------+    | +->+------------+
    /// | 2        X |----+    | 2        X |----+ |  |            |
    /// +------------+    |    +------------+      |  +------------+
    /// | 3          |    |    | 3        X |------+
    /// +------------+    |    +------------+         +------------+
    ///                   |                      +--->|            |
    ///                   +--->+--- PT 2 ---+    |    +------------+
    ///                        | 0        X |----+
    ///                        +------------+         +------------+
    ///                        | 1        X |-------->|            |
    ///                        +------------+         +------------+
    ///                        | 2        X |----+
    ///                        +------------+    |    +------------+
    ///                        | 3          |    +--->|            |
    ///                        +------------+         +------------+
    /// ```
    /// This method determines the range of entry indices that must be processed
    /// in some way if the address range is to be processed. For example, it
    /// will return `1..=2` when invoked on the only PD, `1..=3` when invoked on
    /// PT 1, and `0..=2` when invoked on PT 2.
    /// 
    /// If the table cannot have children or if it is not at all responsible for
    /// the given range, `None` is returned. 
    fn downstream_idx_range(&self, addresses: RangeInclusive<VirtAddr>) -> Option<RangeInclusive<usize>> {
        // this function does a really simple thing but the logic involved is
        // kinda crazy for some reason

        // if we're a plain Page, we cannot have children
        if self.kind == TableKind::Page { return None; }
        
        // make sure that there's overlap between the ranges
        let responsible_for = self.lowest_addr..=self.highest_addr();
        let overlap = responsible_for.start() <= addresses.end() || addresses.start() <= responsible_for.end(); // https://stackoverflow.com/questions/3269434/whats-the-most-efficient-way-to-test-if-two-ranges-overlap
        if !overlap {
            return None;
        }

        // the input range may not be fully contained within our range,
        // it might start before us
        let low_idx = if responsible_for.contains(addresses.start()) {
            self.kind.entry_idx(*addresses.start()).unwrap(/* we've ensured that we're not a TableKind::Page, but TableKind::entry_idx is None */)
        } else {
            0
        };

        // similarly, it might end after us
        let high_idx = if responsible_for.contains(addresses.end()) {
            self.kind.entry_idx(*addresses.end()).unwrap(/* same cause */)
        } else {
            INDEX_MASK // the mask is coincidentally the highest possible index
        };

        Some(low_idx..=high_idx)
    }
}

impl Debug for RawTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?} ({:?} through {:?})", self.kind, self.phys_addr,
            self.lowest_addr, self.highest_addr())?;
        
        if self.kind != TableKind::Page {
            writeln!(f, " [")?;
            for i in 0..512 {
                if let Some(entry) = unsafe { self.get_downstream_by_idx(i) } {
                    let entry = entry.to_parent_entry();
                    writeln!(f, "\t{i:#03}: {entry:?}")?;
                }
            }
            write!(f, "]")
        } else {
            Ok(())
        }
    }
}

const MAX_RANGES: usize = 4;

pub struct AddrSpace<'alloc> {
    /// The entire address space, not just the portion that we handle
    cr3: CR3,
    /// The ranges that we're responsible for
    ranges: [Option<RangeInclusive<VirtAddr>>; MAX_RANGES],
    /// Physical memory allocator
    alloc: &'alloc Mutex<PhysAlloc>,
}

impl<'alloc> AddrSpace<'alloc> {
    /// Iterates over all tables involved in the translation of the supplied
    /// address range. For every table that's at least partially responsible for
    /// translating any address in the given range (except the PML4) this method
    /// calls the supplied closure with:
    ///   - a reference to the table currently under consideration (could be
    ///     None);
    ///   - the kind that the table under consideration should be of;
    ///   - the first virtual address that the table under consideration should
    ///     be handling, even if the reference to it is `None`.
    fn iterate_over_range(
        &self,
        addresses: RangeInclusive<VirtAddr>,
        mut callback: impl FnMut(&Option<RawTable>, TableKind, VirtAddr)
    ) -> Result<()> {
        if !self.does_handle_range(&addresses) { return Err(Error::AddressNotHandledBySpace) };

        fn iterate_table(
            table: &RawTable,
            addresses: RangeInclusive<VirtAddr>,
            callback: &mut impl FnMut(&Option<RawTable>, TableKind, VirtAddr)
        ) {
            if table.kind.downstream().is_some() { // ensure that we can have children
                for i in table.downstream_idx_range(addresses.clone()).unwrap() {
                    let downstream = unsafe { table.get_downstream_by_idx(i) };

                    // call callback
                    let ds_low_addr = VirtAddr::from_usize(table.lowest_addr.to_usize() | (i << table.kind.index_at().unwrap())).unwrap();
                    callback(&downstream, table.kind.downstream().unwrap(), ds_low_addr);

                    // iterate over child
                    if let Some(ref ds) = downstream {
                        iterate_table(ds, addresses.clone(), callback);
                    }
                }
            }
        }

        let root = self.root();
        iterate_table(&root, addresses, &mut callback);
        Ok(())
    }

    /// Returns the root PML4 table of the address space.
    fn root(&self) -> RawTable {
        let attrs = TableAttrs { cache: self.cr3.cache_mode(), ..Default::default() };
        let phys_addr = PhysAddr(self.cr3.addr() << BITS_FOR_PAGE_OFFSET);
        let pml4_addr: VirtAddr = phys_addr.try_into().unwrap();
        RawTable {
            attrs,
            phys_addr,
            entries: Some(pml4_addr.to_mut_ptr()),
            lowest_addr: VirtAddr::from_usize(0).unwrap(),
            kind: TableKind::Pml4,
        }
    }

    /// Returns a slice of ranges that covers the entire address space
    fn get_full_space() -> [Option<RangeInclusive<VirtAddr>>; MAX_RANGES] {
        let mut ranges = [const { None }; MAX_RANGES];
        ranges[0] = Some(VirtAddr::from_usize(0x0000_0000_0000_0000).unwrap() ..= VirtAddr::from_usize(0x0000_7fff_ffff_ffff).unwrap());
        ranges[1] = Some(VirtAddr::from_usize(0xffff_8000_0000_0000).unwrap() ..= VirtAddr::from_usize(0xffff_ffff_ffff_ffff).unwrap());
        ranges
    }

    fn sort_ranges(&mut self) {
        self.ranges
            .sort_by(|a, b| match (a, b) {
                (Some(a), Some(b)) => a.start().cmp(b.start()),
                (None, Some(_)) => Ordering::Greater,
                (Some(_), None) => Ordering::Less,
                _ => Ordering::Equal,
            });
    }

    /// Normalizes the list of ranges, i.e. ensures that:
    ///   - all `None`s are placed at the end
    ///   - neighboring ranges are merged
    ///   - the ranges are sorted
    fn normalize_ranges(&mut self) {
        self.sort_ranges();

        // merge neighboring ranges
        for i in 0 .. (MAX_RANGES - 1) {
            let (a, b) = self.ranges.split_at_mut(i + 1);
            let a = &mut a[i];
            let b = &mut b[0];

            let (Some(a_range), Some(b_range)) = (a.to_owned(), b.to_owned()) else { continue };
            assert!(a_range.end() < b_range.start());

            if a_range.end().to_usize() + 1 == b_range.start().to_usize() {
                *a = Some(a_range.start().to_owned() ..= b_range.end().to_owned());
                *b = None;
            }
        }

        self.sort_ranges();
    }

    fn insert_range(&mut self, range: RangeInclusive<VirtAddr>) -> Result<()> {
        let slot = self
            .ranges
            .iter_mut()
            .find(|x| x.is_none())
            .ok_or(Error::TooManyRanges)?;
        *slot = Some(range);
        Ok(())
    }

    pub(crate) fn get_cr3(&self) -> u64 {
        self.cr3.into()
    }
}

impl<'alloc> IfAddrSpace<'alloc> for AddrSpace<'alloc> {
    const MAX_RANGES: usize = MAX_RANGES;

    unsafe fn get_current(alloc: &'alloc Mutex<PhysAlloc>) -> Self {
        let mut cr3: u64;
        asm!("mov {cr3}, cr3", cr3 = out(reg) cr3);
        Self {
            cr3: cr3.into(),
            ranges: Self::get_full_space(),
            alloc,
        }
    }

    unsafe fn set_as_current(&mut self) {
        let cr3: u64 = self.cr3.into();
        asm!("mov cr3, {cr3}", cr3 = in(reg) cr3);
    }

    fn is_current(&self) -> bool {
        let mut cr3: u64;
        unsafe { asm!("mov {cr3}, cr3", cr3 = out(reg) cr3) };
        cr3 == self.cr3.into()
    }

    /// Creates a new empty address space
    fn new(alloc: &'alloc Mutex<PhysAlloc>) -> Result<Self> {
        let table = RawTable::new(alloc.lock().deref_mut(), TableKind::Pml4, VirtAddr::from_usize(0).unwrap(), Default::default())?;
        Ok(Self {
            cr3: CR3::new().with_addr(table.phys_addr.to_usize() >> BITS_FOR_PAGE_OFFSET),
            ranges: Self::get_full_space(),
            alloc,
        })
    }

    fn ranges(&self) -> impl Iterator<Item = RangeInclusive<VirtAddr>> {
        self.ranges
            .iter()
            .filter_map(|x| x.to_owned())
    }

    fn does_handle_range(&self, range: &RangeInclusive<VirtAddr>) -> bool {
        self.ranges()
            .any(|r| range.start() >= r.start() && range.end() <= r.end())
    }

    fn take_portion(&mut self, request: &RangeInclusive<VirtAddr>) -> Result<Self> {
        let start = request.start().to_usize();
        let end = request.end().to_usize();
        if start & PAGE_OFFSET_MASK != 0 { return Err(Error::InvalidAddrRange) };
        if end & PAGE_OFFSET_MASK != PAGE_OFFSET_MASK { return Err(Error::InvalidAddrRange) };

        let next_page = VirtAddr::from_usize(end + 1).unwrap();
        let last_b_of_prev_page = VirtAddr::from_usize(start - 1).unwrap();

        let (index_to_replace, in_place_replacement, new_entry) = self
            .ranges
            .iter()
            .enumerate()
            .filter_map(|(i, x)| x.as_ref().map(|v| (i, v)))
            .filter_map(|(i, slot)| {
                if slot.start() <= request.start() && slot.end() >= request.end() {
                    let (in_place_replacement, new_entry) = if slot.start() == request.start() {
                        // Slot:       |-----------|
                        // Request:    |-----|
                        // Slot repl.:       |-----|
                        // New slot:   None
                        (next_page ..= slot.end().to_owned(), None)
                    } else if slot.end() == request.end() {
                        // Slot:       |-----------|
                        // Request:         |------|
                        // Slot repl.: |----|
                        // New slot:   None
                        (slot.start().to_owned() ..= last_b_of_prev_page, None)
                    } else {
                        // Slot:       |-----------|
                        // Request:       |-----|
                        // Slot repl.: |--|
                        // New slot:            |--|
                        (
                            slot.start().to_owned() ..= last_b_of_prev_page,
                            Some(request.end().to_owned() ..= slot.end().to_owned())
                        )
                    };
                    Some((i, in_place_replacement, new_entry))
                } else {
                    None
                }
            })
            .next()
            .ok_or(Error::AddressNotHandledBySpace)?;

        if let Some(new_entry) = new_entry {
            self.insert_range(new_entry)?;
        }
        self.ranges[index_to_replace] = Some(in_place_replacement);
        self.normalize_ranges();

        let mut new_ranges = [const { None }; MAX_RANGES];
        new_ranges[0] = Some(request.to_owned());

        Ok(Self {
            cr3: self.cr3,
            ranges: new_ranges,
            alloc: self.alloc,
        })
    }

    fn merge_portion(&mut self, other: Self) -> Result<()> {
        for range in other.ranges() {
            self.insert_range(range)?;
        }
        self.normalize_ranges();
        Ok(())
    }

    fn modify<'guard>(&'guard mut self) -> impl IfAddrSpaceGuard<'alloc> {
        AddrSpaceGuard {
            active: self.is_current(),
            space: self,
            buffer: Default::default(),
            overflow: false,
        }
    }

    fn copy_into(&self, other: &mut Self, range: &RangeInclusive<VirtAddr>) -> Result<()> {
        let mut guard = other.modify();
        guard.unmap_range(range.clone())?;

        self.iterate_over_range(range.clone(), |table, kind, _addr| {
            if kind != TableKind::Page { return; }
            let Some(table) = table else { return };
            guard.map_range(table.lowest_addr, table.phys_addr, 1, table.attrs.to_access(), AllocReturn::Start).unwrap();
        }).unwrap();

        Ok(())
    }
}

impl Debug for AddrSpace<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}: [", self.cr3)?;

        for range in self.ranges() {
            writeln!(f, "\t{range:?}: [")?;

            let mut start_addr: Option<(VirtAddr, PhysAddr)> = None;
            let mut last_addr: Option<VirtAddr> = None;

            self.iterate_over_range(range, |table, kind, addr| {
                // writeln!(f, "{table:?}");
                if kind != TableKind::Page || table.is_none() { return; }
                if start_addr.is_none() {
                    start_addr = Some((addr, table.as_ref().unwrap().phys_addr));
                }
                // writeln!(f, "st={start_addr:?} la={last_addr:?} ad={addr:?}");
                if let Some((start_virt, start_phys)) = start_addr && let Some(last) = last_addr && addr.to_usize() - last.to_usize() > PAGE_SIZE {
                    let pages = ((last_addr.unwrap().to_usize() - start_virt.to_usize()) / PAGE_SIZE) + 1;
                    let _ = writeln!(f, "\t\t{:?}..{:?} -> {:?}..{:?}",
                        start_virt, VirtAddr::from_usize(start_virt.to_usize() + (pages * PAGE_SIZE)).unwrap(),
                        start_phys, PhysAddr(start_phys.to_usize() + (pages * PAGE_SIZE)));
                    start_addr = Some((addr, table.as_ref().unwrap().phys_addr));
                }
                last_addr = Some(addr);
            }).unwrap();

            if let Some((start_virt, start_phys)) = start_addr {
                let pages = ((last_addr.unwrap().to_usize() - start_virt.to_usize()) / PAGE_SIZE) + 1;
                writeln!(f, "\t\t{:?}..{:?} -> {:?}..{:?}",
                    start_virt, VirtAddr::from_usize(start_virt.to_usize() + (pages * PAGE_SIZE)).unwrap(),
                    start_phys, PhysAddr(start_phys.to_usize() + (pages * PAGE_SIZE)))?;
            }
            writeln!(f, "\t]")?;
        }

        write!(f, "]")
    }
}

/// Short-lived instance that depends on [`AddrSpace`] and collects
/// modifications over its lifetime, committing changes to the TLB when it's
/// dropped.
pub struct AddrSpaceGuard<'guard, 'alloc: 'guard> {
    /// Whether the guard belongs to an active (i.e. current) address space
    active: bool,
    /// Reference to parent struct
    space: &'guard mut AddrSpace<'alloc>,
    /// List of pages that `invlpg` will be used on when the guard is dropped
    buffer: DynArr<VirtAddr>,
    /// If this is set to true, too many pages have been changed to fit into a
    /// `DynArr`. A CR3 refresh will be performed instead.
    overflow: bool,
}

#[derive(Debug, PartialEq, Eq)]
enum TableOperation {
    LeaveAsIs,
    Free(FreeMode),
    Replace(FreeMode, RawTable),
}

impl<'guard, 'alloc: 'guard> AddrSpaceGuard<'guard, 'alloc> {
    fn mark_for_invalidation(&mut self, addr: VirtAddr) -> bool {
        if !self.active { return false; }
        if self.overflow { return false; }
        if self.buffer.push(addr).is_err() {
            self.overflow = true;
            return false;
        }
        true
    }

    /// Searches for a table of the given kind responsible for the given virtual
    /// address
    fn get_table(origin: RawTable, addr: VirtAddr, kind: TableKind) -> RawTable {
        if origin.kind == kind { return origin; }
        let origin = unsafe { origin.get_downstream_by_idx(origin.kind.entry_idx(addr).unwrap()).unwrap() };
        Self::get_table(origin, addr, kind)
    }

    /// Iterates over all tables involved in the translation of the supplied
    /// address range. For every table that's at least partially responsible for
    /// translating any address in the given range (except the PML4) this method
    /// calls the supplied function with:
    ///   - a reference to the table currently under consideration (could be
    ///     None);
    ///   - the kind that the table under consideration should be of;
    ///   - the first virtual address that the table under consideration should
    ///     be handling, even if the reference to it is `None`.
    /// 
    /// The callback should return a [`TableOperation`] to perform, or an error
    /// that will be immediately propagated.
    fn iterate_over_range_mut(
        &mut self,
        addresses: RangeInclusive<VirtAddr>,
        mut callback: impl FnMut(&Option<RawTable>, TableKind, VirtAddr) -> Result<TableOperation>
    ) -> Result<()> {
        if !self.space.does_handle_range(&addresses) { return Err(Error::AddressNotHandledBySpace) };

        fn iterate_table(
            guard: &mut AddrSpaceGuard<'_, '_>,
            table: &mut RawTable,
            addresses: RangeInclusive<VirtAddr>,
            callback: &mut impl FnMut(&Option<RawTable>, TableKind, VirtAddr) -> Result<TableOperation>
        ) -> Result<()> {
            if table.kind.downstream().is_some() { // ensure that we can have children
                for i in table.downstream_idx_range(addresses.clone()).unwrap() {
                    let downstream = unsafe { table.get_downstream_by_idx(i) };

                    // call callback
                    let ds_low_addr = VirtAddr::from_usize(table.lowest_addr.to_usize() | (i << table.kind.index_at().unwrap())).unwrap();
                    let operation = callback(&downstream, table.kind.downstream().unwrap(), ds_low_addr)?;
                    // log::trace!("cb({:?}, {:?}, {:?}) = {:?}", downstream, table.kind.downstream().unwrap(), ds_low_addr, operation);

                    // update entry
                    let mut alloc = guard.space.alloc.lock();
                    let mut downstream = match operation {
                        TableOperation::LeaveAsIs => downstream,

                        TableOperation::Free(mode) if let Some(mut ds) = downstream => {
                            unsafe { ds.free(alloc.deref_mut(), mode)? };
                            unsafe { table.set_downstream_by_idx(i, None)? };
                            None
                        },
                        TableOperation::Free(_) /* if already None */ => panic!(),

                        TableOperation::Replace(mode, new_ds) if let Some(mut ds) = downstream => {
                            unsafe { ds.free(alloc.deref_mut(), mode)? };
                            unsafe { table.set_downstream_by_idx(i, Some(&new_ds))? };
                            Some(new_ds)
                        },
                        TableOperation::Replace(_, new_ds) /* if None */ => {
                            unsafe { table.set_downstream_by_idx(i, Some(&new_ds))? };
                            Some(new_ds)
                        },
                    };
                    drop(alloc);

                    // iterate over child
                    if let Some(ref mut ds) = downstream {
                        iterate_table(guard, ds, addresses.clone(), callback)?;
                    }
                }
            }
            Ok(())
        }

        let mut root = self.space.root();
        iterate_table(self, &mut root, addresses.clone(), &mut callback)?;

        let low = addresses.start().to_usize();
        let high = addresses.end().to_usize();
        for page in (low..=high).step_by(PAGE_SIZE) {
            if !self.mark_for_invalidation(VirtAddr::from_usize(page).unwrap()) { break };
        }

        Ok(())
    }
}

impl<'guard, 'alloc: 'guard> IfAddrSpaceGuard<'alloc> for AddrSpaceGuard<'guard, 'alloc> {
    fn allocate_range(
        &mut self,
        start: VirtAddr,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<VirtAddr> {
        let end = VirtAddr::from_usize(start.to_usize() + (count * PAGE_SIZE) - 1).unwrap();

        self.iterate_over_range_mut(start..=end, |child, kind, low_addr| {
            match *child {
                Some(_) => Ok(TableOperation::LeaveAsIs),
                None => Ok(TableOperation::Replace(
                    FreeMode::TablesAndPages,
                    RawTable::new(self.space.alloc.lock().deref_mut(), kind, low_addr, access)?
                ))
            }
        }).map(|_| if ret == AllocReturn::End {
            VirtAddr::from_usize(end.to_usize() + 1).unwrap()
        } else {
            start
        })
    }

    fn allocate_anywhere(
        &mut self,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<VirtAddr> {
        let phys_start = self.space.alloc.lock().allocate_contiguous(count)?;
        self.map_range(phys_start.try_into()?, phys_start, count, access, ret)
    }

    fn map_range(
        &mut self,
        start_virt: VirtAddr,
        start_phys: PhysAddr,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<VirtAddr> {
        let end_virt = VirtAddr::from_usize(start_virt.to_usize() + (count * PAGE_SIZE) - 1).unwrap();
        let mut page_ctr = 0;

        self.iterate_over_range_mut(start_virt ..= end_virt, |child, kind, low_addr| {
            match *child {
                _ if kind == TableKind::Page => {
                    page_ctr += 1;
                    Ok(TableOperation::Replace(
                        FreeMode::TablesOnly,
                        RawTable {
                            kind: TableKind::Page,
                            lowest_addr: low_addr,
                            phys_addr: PhysAddr(start_phys.to_usize() + ((page_ctr - 1) * PAGE_SIZE)),
                            entries: None,
                            attrs: TableAttrs::from_access(access),
                        }
                    ))
                },
                Some(_) => Ok(TableOperation::LeaveAsIs),
                None => Ok(TableOperation::Replace(
                    FreeMode::TablesOnly,
                    RawTable::new(self.space.alloc.lock().deref_mut(), kind, low_addr, access)?
                ))
            }
        }).map(|_| if ret == AllocReturn::End {
            VirtAddr::from_usize(end_virt.to_usize() + 1).unwrap()
        } else {
            start_virt
        })
    }

    fn deallocate_range(&mut self, range: RangeInclusive<VirtAddr>) -> Result<()> {
        self.iterate_over_range_mut(range.clone(), |child, _kind, _low_addr| {
            Ok(match *child {
                Some(ref child) => {
                    let table_range = child.lowest_addr ..= child.highest_addr();
                    if table_range.start() >= range.start() && table_range.end() <= range.end() {
                        TableOperation::Free(FreeMode::TablesAndPages)
                    } else {
                        TableOperation::LeaveAsIs
                    }
                },
                None => TableOperation::LeaveAsIs,
            })
        })
    }

    fn unmap_range(&mut self, range: RangeInclusive<VirtAddr>) -> Result<()> {
        self.iterate_over_range_mut(range.clone(), |child, _kind, _low_addr| {
            Ok(match *child {
                Some(ref child) => {
                    let table_range = child.lowest_addr ..= child.highest_addr();
                    if table_range.start() >= range.start() && table_range.end() <= range.end() {
                        TableOperation::Free(FreeMode::TablesOnly)
                    } else {
                        TableOperation::LeaveAsIs
                    }
                },
                None => TableOperation::LeaveAsIs,
            })
        })
    }
}

impl<'guard, 'alloc> Drop for AddrSpaceGuard<'guard, 'alloc> {
    /// Commit all recorded changes to the TLB
    fn drop(&mut self) {
        if !self.active { return; }

        if self.overflow {
            unsafe {
                asm!(
                    "mov {tmp}, cr3",
                    "mov cr3, {tmp}",
                    tmp = out(reg) _,
                );
            }
            return;
        }

        for addr in self.buffer.iter() {
            unsafe {
                asm!(
                    "invlpg [{ptr}]",
                    ptr = in(reg) addr.to_usize() as u64,
                );
            }
        }
    }
}

impl<'guard, 'alloc> Deref for AddrSpaceGuard<'guard, 'alloc> {
    type Target = AddrSpace<'alloc>;
    fn deref(&self) -> &Self::Target {
        self.space
    }
}
