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
//!   - Public: [`AddressSpace`], [`SubSpace`] create a somewhat safe and
//!     nice-to-use abstraction over [`RawTable`]s.
//! 
//! For more information, refer to the Intel Software Developer's Manual. It's
//! got some nice diagrams to help you visualize how this process works.

#![allow(dead_code)]

use core::{
    arch::asm,
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
    mem::size_of,
    ops::{Range, RangeInclusive}
};

use bitfield_struct::bitfield;
use spin::{Mutex, MutexGuard, RwLock};
use strum::VariantArray;

use crate::util::dyn_arr::DynArr;
use super::{
    phys, MemMgrError, VirtAddr, PhysAddr, PAGE_SIZE,
};

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

/// Bits reserved for the subspace ID in each table entry
const SUBSPACE_ID_LEN: usize = 2;
/// Maximum subspaces available
const MAX_SUBSPACES: usize = (1 << SUBSPACE_ID_LEN) - 1; // value 0 is reserved for "not a subspace"

/// Subspaces are a way to safely share a subtree of an address space between
/// multiple address spaces for efficiency reasons. This mechanism allows us,
/// for example, to share the exact same upper half between all address spaces
/// by simply having all appropriate PML4 entries pointing to the same PDPTs.
pub struct SubSpace(usize);

/// The global subspace registry. In order to prevent race conditions between
/// threads that are trying to modify address spaces that share a subspace,
/// every subspace is given a unique ID that is stored in the corresponding
/// entry. This ID refers to a lock that is stored in this variable.
static SUBSPACE_REGISTRY: [Mutex<()>; MAX_SUBSPACES] = [const { Mutex::new(()) }; MAX_SUBSPACES];
static SUBSPACE_REGISTRY_SLOTS: RwLock<[bool; MAX_SUBSPACES]> = RwLock::new([false; MAX_SUBSPACES]);

/// Used to represent entries of tables of all 4 levels.
/// 
/// For fields described below, UDD stands for Unless Disallowed DownstreamRwLock::new(Default::default())
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
    // FIXME: the following two hardcoded bit lengths should instead be based on
    // `SUBSPACE_ID_LEN`. Unfortunately, `bitfield` does not seem to support
    // expressions in the `bits` attribute
    /// Subspace ID (ignored by the CPU)
    #[bits(2)]
    subspace_id: u16,
    /// Ignored
    #[bits(9)]
    _ign2: u16,
    /// XD: 0 = Execution allowed UDD, 1 = Execution disallowed
    exec_disable: bool,
}

impl Debug for TableEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.present() {
            return write!(f, "0x---------------- present=false")
        }
        write!(f, "{:?} write={} user={} cache={} accessed={} dirty={} global={} subspace={} xd={}",
            PhysAddr(self.addr() << 12), self.write(), self.user(),
            self.cache_mode() | (self.cache_mode_2() << 2),
            self.accessed(), self.dirty(), self.global(),
            self.subspace_id(), self.exec_disable())
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
#[derive(PartialEq, Clone, Copy)]
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
#[derive(strum_macros::VariantArray)]
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
    const fn entry_idx(&self, addr: VirtAddr) -> Option<usize> {
        let Some(idx) = self.index_at() else { return None };
        Some((addr.0 >> idx) & INDEX_MASK)
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

/// Wrapper struct for `TableRef`s that has some additional context
struct RawTable<'s> {
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
    /// Subspace lock guard and ID
    subspace: Option<(MutexGuard<'s, ()>, u16)>,
}

impl<'s> RawTable<'s> {
    /// Allocates a new table
    fn new(kind: TableKind, lowest_addr: VirtAddr, attrs: TableAttrs) -> Result<RawTable<'s>, MemMgrError> {
        // allocate page for table
        let pages = phys::allocate(1);
        if pages.is_empty() { return Err(MemMgrError::OutOfMemory); }
        let page: VirtAddr = pages[0].into();

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
        valid &= lowest_addr.0 & PAGE_OFFSET_MASK == 0;
        if !valid { return Err(MemMgrError::InvalidLowestAddr); }

        Ok(Self {
            lowest_addr,
            phys_addr: pages[0],
            entries: if kind == TableKind::Page { None } else { Some(page.into()) },
            kind,
            attrs,
            subspace: None,
        })
    }

    /// Deallocates a table
    unsafe fn free(&mut self) -> Result<(), MemMgrError> {
        let mut pages = DynArr::<PhysAddr>::new();
        pages.push(self.phys_addr).unwrap();
        match phys::deallocate(pages) {
            Ok(()) => Ok(()),
            Err(_) => Err(MemMgrError::UnrecognizedDealloc),
        }
    }

    /// Returns an entry that can be used to reference this table
    fn to_parent_entry(&self) -> TableEntry {
        let mut entry = TableEntry::new()
            .with_addr(self.phys_addr.0 >> 12)
            .with_subspace_id(self.subspace.as_ref().map_or(0, |(_, id)| *id))
            .with_present(true);
        self.attrs.write_to_entry(&mut entry);
        entry
    }

    /// Gets a downstream table by its entry index. Returns None if the table
    /// cannot have children or if the entry is not present.
    unsafe fn get_downstream_by_idx(&self, idx: usize) -> Option<RawTable<'s>> {
        // check entry presence
        let Some(entries) = self.entries?;
        let entry: TableEntry = (*entries)[idx];
        if !entry.present() { return None; }
        let ds_kind = self.kind.downstream()?; // this also ensures that we can have children

        // acquire subspace lock if present
        let subspace = if entry.subspace_id() > 0 {
            let id = entry.subspace_id() - 1;
            let mutex = &SUBSPACE_REGISTRY[id as usize];
            Some((mutex.lock(), id))
        } else {
            None
        };

        // return table
        let phys_addr = PhysAddr(entry.addr() << 12);
        let virt_addr: VirtAddr = phys_addr.into();
        let entries = if ds_kind == TableKind::Page { None } else { Some(virt_addr.into()) };
        Some(Self {
            lowest_addr: VirtAddr::from_usize(self.lowest_addr.0 | (idx << self.kind.index_at().unwrap())),
            phys_addr,
            entries,
            kind: ds_kind,
            attrs: TableAttrs::from_entry(&entry),
            subspace,
        })
    }
    
    /// Sets a reference to a downstream table by its entry index. Returns an
    /// error if the table cannot have children or if an incorrect kind was
    /// supplied.
    unsafe fn set_downstream_by_idx(&mut self, idx: usize, table: Option<&mut RawTable>) -> Result<(), ()> {
        // ensure that we can have children
        let ds_kind = self.kind.downstream().ok_or(())?;
        let Some(entries) = self.entries else { return Err(()) };

        // check if None was supplied
        let Some(table) = table else {
            (*entries)[idx] = TableEntry::new().with_present(false);
            return Ok(());
        };

        // check kinds
        if table.kind != ds_kind { return Err(()) }

        // set entry
        (*entries)[idx] = table.to_parent_entry();
        Ok(())
    }

    /// Computes the highest address that this table is responsible for
    fn highest_addr(&self) -> VirtAddr {
        // sets all indices in the address that correspond to the current and
        // all downstream kinds to their highest value
        let mut highest_addr = self.lowest_addr;
        let mut current_kind = self.kind;
        while let Some(next_kind) = current_kind.downstream() {
            highest_addr = VirtAddr::from_usize(highest_addr.0 | (INDEX_MASK << current_kind.index_at().unwrap()));
            current_kind = next_kind;
        }
        highest_addr.0 |= PAGE_OFFSET_MASK;
        highest_addr
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
    /// address range `0b0101 ..= 0b1010` (the last part of each bound is
    /// omitted).
    /// 
    /// In this address range, there are 6 pages that we need to map: `0b0101`,
    /// `0b0110`, `0b0111`, `0b1000`, `0b1001` and `0b1010`. In the following
    /// diagram table entries of interest to this mapping operation are marked
    /// with an X:
    /// ```
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
            self.kind.entry_idx(*addresses.start()).expect("we ensured that we're not a TableKind::Page, but TableKind::entry_idx is None")
        } else {
            0
        };

        // similarly, it might end after us
        let high_idx = if responsible_for.contains(addresses.end()) {
            self.kind.entry_idx(*addresses.end()).expect("we ensured that we're not a TableKind::Page, but TableKind::entry_idx is None")
        } else {
            INDEX_MASK // the mask is coincidentally the highest possible index
        };

        Some(low_idx..=high_idx)
    }
}

impl Debug for RawTable<'_> {
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

/// TLB-aware wrapper for operations on address spaces.
/// 
/// TLB stands for Translation Lookaside Buffer and is basically a special kind
/// of cache that stores virtual-to-physical mappings. If the address space that
/// is being modified is active, the CPU has to be explicitly notified of these
/// changes - it is not a transparent cache.
/// 
/// The `modify()` method of this struct returns a guard that guarantees that
/// the CPU will be aware of all changes applied over the lifetime of that guard
/// once it's dropped, but it does _not_ guarantee that the CPU won't become
/// aware of them on its own, before the guard is dropped.
#[derive(PartialEq, Eq)]
pub struct AddressSpace {
    cr3: CR3,
}

impl AddressSpace {
    /// Gets the current address space from the `CR3` register of the CPU.
    /// 
    /// # Safety
    /// Ensure that there is only one `AddressSpace` wrapper per CR3 value,
    /// otherwise TLB consistency is not guaranteed.
    pub unsafe fn get_current() -> AddressSpace {
        let mut cr3: u64;
        asm!("mov {cr3}, cr3", cr3 = out(reg) cr3);
        AddressSpace { cr3: cr3.into() }
    }

    /// Switches to an address space, consuming the wrapper.
    /// 
    /// # Safety
    /// Safe if:
    ///   - only safe modifications of the address space have been applied, and
    ///   - there are no undropped guards belonging to any address space.
    /// 
    /// If the second point is violated, the TLB may become inconsistent.
    pub unsafe fn set_as_current(&self) {
        let cr3: u64 = self.cr3.into();
        asm!("mov cr3, {cr3}", cr3 = in(reg) cr3);
    }

    /// Creates a new empty address space
    pub fn new() -> Result<AddressSpace, MemMgrError> {
        let table = RawTable::new(TableKind::Pml4, VirtAddr::from_usize(0), Default::default())?;
        Ok(AddressSpace {
            cr3: CR3::new().with_addr(table.phys_addr.0 >> BITS_FOR_PAGE_OFFSET),
        })
    }

    /// Determines whether the address space is active.
    pub fn is_current(&self) -> bool {
        *self == unsafe { Self::get_current() }
    }

    /// Creates a guard that can be used to modify the address space. When this
    /// guard is dropped, all changes made during its lifetime will be committed
    /// to the TLB.
    pub fn modify<'guard, 'r: 'guard>(&'r mut self) -> AddressSpaceGuard<'guard, 'r> {
        AddressSpaceGuard {
            phantom: PhantomData,
            active: self.is_current(),
            space: self,
            buffer: Default::default(),
            overflow: false,
        }
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
    fn iterate_over_range(
        &self,
        addresses: RangeInclusive<VirtAddr>,
        mut callback: impl FnMut(&Option<RawTable<'_>>, TableKind, VirtAddr)
    ) {
        fn iterate_table(
            table: &RawTable<'_>,
            addresses: RangeInclusive<VirtAddr>,
            callback: &mut impl FnMut(&Option<RawTable<'_>>, TableKind, VirtAddr)
        ) {
            if table.kind.downstream().is_some() { // ensure that we can have children
                for i in table.downstream_idx_range(addresses.clone()).unwrap() {
                    let downstream = unsafe { table.get_downstream_by_idx(i) };

                    // call callback
                    let ds_low_addr = VirtAddr::from_usize(table.lowest_addr.0 | (i << table.kind.index_at().unwrap()));
                    callback(&downstream, table.kind.downstream().unwrap(), ds_low_addr);

                    // iterate over child
                    if let Some(ref ds) = downstream {
                        iterate_table(ds, addresses.clone(), callback);
                    }
                }
            }
        }

        let root = self.root();
        iterate_table(&root, addresses, &mut callback)
    }

    /// Returns the root PML4 table of the address space.
    fn root<'b>(&self) -> RawTable<'b> {
        let attrs = TableAttrs { cache: self.cr3.cache_mode(), ..Default::default() };
        let phys_addr = PhysAddr(self.cr3.addr() << BITS_FOR_PAGE_OFFSET);
        let pml4_addr: VirtAddr = phys_addr.into();
        RawTable {
            attrs,
            phys_addr,
            entries: Some(pml4_addr.into()),
            lowest_addr: VirtAddr::from_usize(0),
            kind: TableKind::Pml4,
            subspace: None,
        }
    }
}

impl Debug for AddressSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}: [", self.cr3)?;

        let mut start_addr: Option<(VirtAddr, PhysAddr)> = None;
        let mut last_addr: Option<VirtAddr> = None;

        self.iterate_over_range(VirtAddr::from_usize(0)..=VirtAddr::from_usize(usize::MAX), |table, kind, addr| {
            // writeln!(f, "{table:?}");
            if kind != TableKind::Page || table.is_none() { return; }
            if start_addr.is_none() {
                start_addr = Some((addr, table.as_ref().unwrap().phys_addr));
            }
            // writeln!(f, "st={start_addr:?} la={last_addr:?} ad={addr:?}");
            if let Some((start_virt, start_phys)) = start_addr && let Some(last) = last_addr && addr.0 - last.0 > PAGE_SIZE {
                let pages = ((last_addr.unwrap().0 - start_virt.0) / PAGE_SIZE) + 1;
                let _ = writeln!(f, "\t{:?}..{:?} -> {:?}..{:?}",
                    start_virt, VirtAddr::from_usize(start_virt.0 + (pages * PAGE_SIZE)),
                    start_phys, PhysAddr(start_phys.0 + (pages * PAGE_SIZE)));
                start_addr = Some((addr, table.as_ref().unwrap().phys_addr));
            }
            last_addr = Some(addr);
        });

        if start_addr.is_some() {
            let (start_virt, start_phys) = start_addr.unwrap();
            let pages = ((last_addr.unwrap().0 - start_virt.0) / PAGE_SIZE) + 1;
            writeln!(f, "\t{:?}..{:?} -> {:?}..{:?}",
                start_virt, VirtAddr::from_usize(start_virt.0 + (pages * PAGE_SIZE)),
                start_phys, PhysAddr(start_phys.0 + (pages * PAGE_SIZE)))?;
        }

        write!(f, "]")
    }
}

/// Short-lived instance that depends on `AddressSpace` and collects
/// modifications over its lifetime, committing changes to the TLB when it's
/// dropped.
pub struct AddressSpaceGuard<'guard, 'r: 'guard> {
    phantom: PhantomData<&'guard ()>,
    /// Whether the guard belongs to an active (i.e. current) address space
    active: bool,
    /// Reference to parent struct
    space: &'r mut AddressSpace,
    /// List of pages that `invlpg` will be used on when the guard is dropped
    buffer: DynArr<VirtAddr>,
    /// If this is set to true, too many pages have been changed to fit into a
    /// `DynArr`. A CR3 refresh will be performed instead.
    overflow: bool,
}

impl<'guard, 'r> AddressSpaceGuard<'guard, 'r> {
    fn remember(&mut self, addr: VirtAddr) {
        if !self.active { return; }
        if self.overflow { return; }
        if self.buffer.push(addr).is_err() {
            self.overflow = true;
        }
    }

    /// Searches for a table of the given kind responsible for the given virtual
    /// address
    fn get_table(origin: RawTable<'_>, addr: VirtAddr, kind: TableKind) -> RawTable<'_> {
        if origin.kind == kind { return origin; }
        let origin = unsafe { origin.get_downstream_by_idx(origin.kind.entry_idx(addr).unwrap()).unwrap() };
        Self::get_table(origin, addr, kind)
    }

    /// Iterates over all tables involved in the translation of the supplied
    /// address range. For every table that's at least partially responsible for
    /// translating any address in the given range (except the PML4) this method
    /// calls the supplied function with:
    ///   - a mutable reference to the table currently under consideration
    ///     (could be None);
    ///   - the kind that the table under consideration should be of;
    ///   - the first virtual address that the table under consideration should
    ///     be handling, even if the reference to it is `None`.
    /// 
    /// The callback is expected to apply modifications to the table as it
    /// desires. If must return a boolean that signals whether any changes were
    /// made. Returning `false` when a change has been made is a logic error
    /// that may leave the memory map in an inconsistent state.
    fn iterate_over_range_mut(
        &mut self,
        addresses: RangeInclusive<VirtAddr>,
        mut callback: impl FnMut(&mut Option<RawTable<'_>>, TableKind, VirtAddr) -> bool,
        mut deletion_callback: impl FnMut(&mut RawTable<'_>) -> bool
    ) -> Result<(), MemMgrError> {
        fn iterate_table(
            guard: &mut AddressSpaceGuard<'_, '_>,
            table: &mut RawTable<'_>,
            addresses: RangeInclusive<VirtAddr>,
            callback: &mut impl FnMut(&mut Option<RawTable<'_>>, TableKind, VirtAddr) -> bool,
            deletion_callback: &mut impl FnMut(&mut RawTable<'_>) -> bool
        ) -> Result<TableAttrs, MemMgrError> {
            let mut most_permissive_attrs: TableAttrs = Default::default();

            if table.kind.downstream().is_some() { // ensure that we can have children
                for i in table.downstream_idx_range(addresses.clone()).unwrap() {
                    let mut downstream = unsafe { table.get_downstream_by_idx(i) };
                    let original_address = downstream.as_ref().map(|t| t.phys_addr);
                    let original_virt_addr = downstream.as_ref().map(|t| t.lowest_addr);

                    // call callback
                    let ds_low_addr = VirtAddr::from_usize(table.lowest_addr.0 | (i << table.kind.index_at().unwrap()));
                    let mut changes_applied = callback(&mut downstream, table.kind.downstream().unwrap(), ds_low_addr);

                    if let Some(ref mut ds) = downstream {
                        // iterate over child
                        let new_attrs = iterate_table(guard, ds, addresses.clone(), callback, deletion_callback)?;
                        most_permissive_attrs.bubble_upstream(ds.attrs);
                        most_permissive_attrs.bubble_upstream(new_attrs);

                        // ask if should delete child
                        let delete = deletion_callback(ds);

                        if delete {
                            unsafe { ds.phys_addr.clear_page().unwrap() }; // mark all entries as not present
                            downstream = None;
                            changes_applied = true;
                        }
                    }

                    // update entry if table changed
                    if changes_applied {
                        table.attrs.bubble_upstream(most_permissive_attrs);
                        unsafe { table.set_downstream_by_idx(i, downstream.as_mut()).unwrap() };
                        // TLB awareness
                        if let Some(address) = downstream.as_ref().map(|t| t.lowest_addr).or(original_virt_addr) {
                            guard.remember(address);
                        }
                    }

                    // deallocate old table
                    let table_removed = changes_applied && original_address.is_some() && downstream.is_none();
                    let address_changed = changes_applied
                                       && original_address.is_some()
                                       && downstream.as_ref().map(|t| t.phys_addr) != original_address;
                    let is_end_page = downstream.as_ref().map(|t| t.kind) == Some(TableKind::Page);
                    if (table_removed || address_changed) && !is_end_page {
                        let mut pages = DynArr::<PhysAddr>::new();
                        pages.push(original_address.unwrap()).unwrap();
                        phys::deallocate(pages).or(Err(MemMgrError::UnrecognizedDealloc))?;
                    }
                }
            }

            Ok(most_permissive_attrs)
        }

        let mut root = self.space.root();
        iterate_table(self, &mut root, addresses, &mut callback, &mut deletion_callback).map(|_|())
    }

    /// Allocates consecutive virtual pages. If `return_end` is `true`, returns
    /// the final byte of the range instead of the start (useful for stacks).
    pub fn allocate_range(&mut self, start: VirtAddr, count: usize, attrs: TableAttrs, return_end: bool) -> Result<VirtAddr, MemMgrError> {
        let end = VirtAddr::from_usize(start.0 + (count * PAGE_SIZE) - 1);
        let mut error: Option<MemMgrError> = None;

        self.iterate_over_range_mut(start..=end, |child, kind, low_addr| {
            match *child {
                Some(_) => false,
                None => match RawTable::new(kind, low_addr, attrs) {
                    Ok(tab) => { *child = Some(tab); true },
                    Err(e) => { error = Some(e); false }
                }
            }
        }, |_| false)?;

        match error {
            None => Ok(if return_end { VirtAddr::from_usize(end.0 + 1) } else { start }),
            Some(e) => Err(e)
        }
    }

    /// Maps consecutive virtual pages to consecutive physical pages. If
    /// `return_end` is `true`, returns the final byte of the range instead of
    /// the start (useful for stacks).
    pub fn map_range(&mut self, start_virt: VirtAddr, start_phys: PhysAddr, count: usize, attrs: TableAttrs, return_end: bool) -> Result<VirtAddr, MemMgrError> {
        let end_virt = VirtAddr::from_usize(start_virt.0 + (count * PAGE_SIZE) - 1);
        let mut page_ctr = 0;
        let mut error: Option<MemMgrError> = None;

        self.iterate_over_range_mut(start_virt..=end_virt, |child, kind, low_addr| {
            if kind == TableKind::Page {
                assert_eq!(low_addr, VirtAddr::from_usize(start_virt.0 + (page_ctr * PAGE_SIZE))); // sanity check
                // if considering an end page, force-assign a new physical address
                *child = Some(RawTable {
                    kind: TableKind::Page,
                    lowest_addr: low_addr,
                    phys_addr: PhysAddr(start_phys.0 + (page_ctr * PAGE_SIZE)),
                    entries: None,
                    attrs,
                    subspace: None,
                });
                page_ctr += 1;
                true
            } else {
                match *child {
                    None => match RawTable::new(kind, low_addr, attrs) {
                        Ok(tab) => { *child = Some(tab); true },
                        Err(e) => { error = Some(e); false }
                    }
                    Some(_) => false
                }
            }
        }, |_| false)?;

        match error {
            None => Ok(if return_end { VirtAddr::from_usize(end_virt.0 + 1) } else { start_virt }),
            Some(e) => Err(e)
        }
    }

    pub fn unmap_range(&mut self, range: RangeInclusive<VirtAddr>) -> Result<(), MemMgrError> {
        self.iterate_over_range_mut(range.clone(), |_,_,_| false, |table| {
            match table.downstream_idx_range(range.clone()) {
                Some(idx_range) => *idx_range.start() == 0 && *idx_range.end() == INDEX_MASK,
                None => false,
            }
        })
    }

    pub fn deallocate_range(&mut self, range: RangeInclusive<VirtAddr>) -> Result<(), MemMgrError> {
        self.iterate_over_range_mut(range.clone(), |_,_,_| false, |table| {
            match table.downstream_idx_range(range.clone()) {
                Some(idx_range) => *idx_range.start() == 0 && *idx_range.end() == INDEX_MASK,
                None => true,
            }
        })
    }

    /// Returns a subspace that can be shared with another address space
    pub fn make_subspace(&mut self, range: RangeInclusive<VirtAddr>) -> Result<SubSpace, MemMgrError> {
        // tests whether the given range would fully cover a table of the given kind
        let is_valid_range = |mut kind: TableKind| {
            let mut valid = true;
            while let Some(next_kind) = kind.downstream() {
                valid &= kind.entry_idx(*range.start()).unwrap() == 0;
                valid &= kind.entry_idx(*range.end()).unwrap() == INDEX_MASK;
                kind = next_kind;
            }
            valid &= range.start().0 & PAGE_OFFSET_MASK == 0;
            valid &= range.end().0 & PAGE_OFFSET_MASK == PAGE_OFFSET_MASK;
            valid
        };

        // determine the table that is to be shared
        let mut kind: Option<TableKind> = None;
        let mut current_kind = TableKind::Pml4;
        while let Some(next_kind) = current_kind.downstream() {
            if kind.is_none() && is_valid_range(current_kind) {
                kind = Some(current_kind);
            }
            current_kind = next_kind;
        }

        // check for invalid values
        let kind = match kind {
            None => return Err(MemMgrError::InvalidAddrRange),
            Some(TableKind::Pml4) => return Err(MemMgrError::InvalidAddrRange),
            Some(kind) => kind,
        };

        // find unused mutex slot
        let mut slot: Option<(usize, &Mutex<()>)> = None;
        let mut guard = SUBSPACE_REGISTRY_SLOTS.write();
        for (i, entry) in SUBSPACE_REGISTRY.iter().enumerate().take(MAX_SUBSPACES) {
            if !(*guard)[i] {
                (*guard)[i] = true;
                slot = Some((i, entry));
                break;
            }
        }
        let (idx, lock) = match slot {
            None => return Err(MemMgrError::MaxSubspacesReached),
            Some(val) => val,
        };

        // write id to entry
        let child_idx = kind.upstream().unwrap().entry_idx(*range.start()).unwrap();
        let mut parent = Self::get_table(self.space.root(), *range.start(), kind.upstream().unwrap());
        let mut child = unsafe { parent.get_downstream_by_idx(child_idx).unwrap() };
        child.subspace = Some((lock.lock(), idx as u16));
        unsafe { parent.set_downstream_by_idx(child_idx, Some(&mut child)).unwrap() };

        Ok(SubSpace(idx))
    }

    /// TODO:
    pub fn assign_subspace(&mut self, _sub: SubSpace) {
        todo!()
    }
}

impl<'guard, 'wrap> Drop for AddressSpaceGuard<'guard, 'wrap> {
    /// Commit all recorded changes to the TLB
    fn drop(&mut self) {
        // if we're not modifying the active address space, there's nothing that
        // we need to do
        if !self.active { return; }

        // if the buffer has overflown, just set cr3 to itself, effectively
        // flushing the whole TLB
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

        // apply `invlpg`
        for addr in self.buffer.iter() {
            unsafe {
                asm!(
                    "invlpg [{ptr}]",
                    ptr = in(reg) addr.0 as u64,
                );
            }
        }
    }
}
