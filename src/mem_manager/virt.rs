//! Virtual Memory Manager. Maps virtual pages to physical pages. Assumes that
//! 4-level paging is used. Based on Volume 3, Chapter 4 of the Intel SDM.

use crate::util::dyn_arr::DynArr;

use super::{
    phys, AllocError, VirtAddr, PhysAddr, PAGE_SIZE,
};
use bitfield_struct::bitfield;
use core::{
    arch::asm,
    fmt::{self, Debug, Formatter},
    mem::size_of, ops::{Range, IndexMut, Index}, marker::PhantomData
};

/// Entries per page
const ENTRY_CNT: usize = PAGE_SIZE / size_of::<u64>(); // 512

/// Used to represent the entries of all 4 table levels: PML4E, PDPTE, PDE and
/// PTE.
/// 
/// UDD = Unless Disallowed Downstream
#[bitfield(u64, debug = false)]
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

    fn merge(&mut self, other: &Self) {
        self.write |= other.write;
        self.user |= other.user;
        self.execute |= other.execute;
        // caching and global purposefully ignored
    }
}

/// The actual table in actual memory as a slice
type TableRef<'a> = &'a mut [u64; ENTRY_CNT];

/// The four levels of tables and the end `Page`
#[derive(Debug, PartialEq)]
enum TableKind {
    Pml4, Pdpt, Pd, Pt, Page
}

impl TableKind {
    /// Determines the bit range in a virtual address this table kind handles
    fn bit_range(&self) -> Option<Range<usize>> {
        match *self {
            Self::Page => None,
            Self::Pt => Some(12..21),
            Self::Pd => Some(21..30),
            Self::Pdpt => Some(30..39),
            Self::Pml4 => Some(39..48),
        }
    }

    /// Extracts the entry index that this table kind would use
    fn entry_idx(&self, addr: VirtAddr) -> Option<usize> {
        let range = self.bit_range()?;
        let size = range.end - range.start; // 9
        let mask = (1 << size) - 1; // 9 LSBs set
        Some((addr.0 >> range.start) & mask)
    }

    /// Determines the table kind the entries of this kind reference
    fn downstream(&self) -> Option<TableKind> {
        match *self {
            Self::Page => None,
            Self::Pt => Some(Self::Page),
            Self::Pd => Some(Self::Pt),
            Self::Pdpt => Some(Self::Pd),
            Self::Pml4 => Some(Self::Pdpt),
        }
    }
}

/// Wrapper struct for `TableRef`s
#[derive(PartialEq)]
pub struct Table<'a> {
    reference: TableRef<'a>,
    kind: TableKind,
    attrs: TableAttrs,
}

impl<'a> Table<'a> {
    /// Creates a table reference using the upper 40 bits of its physical
    /// address.
    /// 
    /// There are no locks. Care must be taken to ensure that only one processor
    /// is trying to access a page table at any given time.
    fn from_addr(addr: usize, kind: TableKind, attrs: TableAttrs) -> Self {
        let reference = unsafe { &mut *((addr << 12) as *mut [u64; ENTRY_CNT]) };
        Self { reference, kind, attrs }
    }

    /// Returns the physical address of the table
    fn addr(&self) -> PhysAddr {
        PhysAddr(self.reference as *const [u64; ENTRY_CNT] as usize)
    }

    /// Allocates a table
    fn allocate(kind: TableKind, attrs: TableAttrs) -> Result<Self, AllocError> {
        match phys::allocate(1).iter().next() {
            None => Err(AllocError::OutOfMemory),
            Some(page) => Ok(Self::from_addr(page.0 >> 12, kind, attrs)),
        }
    }

    /// Deallocates a table, optionally deallocating its descendants. PTs will
    /// not deallocate pages they point to.
    pub fn deallocate(&self, recurse: bool) -> Result<(), AllocError> {
        // deallocate descendants
        if self.kind != TableKind::Pt && recurse {
            for entry in self.iter() {
                let entry: TableEntry = entry.into();
                if !entry.present() { continue; }
                let table = Self::from_addr(entry.addr(), self.kind.downstream().unwrap(), TableAttrs::from_entry(&entry));
                table.deallocate(recurse)?;
            }
        }

        // deallocate self
        let mut pages = DynArr::new();
        pages.push(self.addr()).unwrap();
        match phys::deallocate(pages) {
            Ok(()) => Ok(()),
            Err(_) => Err(AllocError::OutOfMemory),
        }
    }

    /// Returns an entry that can be used to reference this table
    fn to_entry(&self) -> TableEntry {
        let mut entry = TableEntry::new()
            .with_addr((self.reference as *const [u64; ENTRY_CNT] as usize) >> 12)
            .with_present(true);
        self.attrs.write_to_entry(&mut entry);
        entry
    }

    /// Creates an iterator for the table
    pub fn iter(&self) -> TableIter {
        TableIter {
            position: 0,
            table: self
        }
    }

    /// Gets the downstream table for a virtual address
    pub fn downstream(&self, addr: VirtAddr) -> Option<Table<'a>> {
        // check entry presence
        let idx = self.kind.entry_idx(addr)?;
        let entry: TableEntry = self[idx].into();
        if !entry.present() { return None; }

        // return table
        Some(Self::from_addr(entry.addr(), self.kind.downstream()?, TableAttrs::from_entry(&entry)))
    }
    
    /// Adds a reference to a downstream table to this one
    fn set_downstream(&mut self, addr: VirtAddr, table: Option<&mut Table<'a>>) -> Result<(), ()> {
        // check if None was supplied
        let Some(table) = table else {
            let idx = self.kind.entry_idx(addr).ok_or(())?;
            self[idx] = TableEntry::new().with_present(false).into();
            return Ok(());
        };

        // check kinds
        if table.kind != self.kind.downstream().ok_or(())? {
            return Err(())
        }

        // set entry
        let idx = self.kind.entry_idx(addr).ok_or(())?;
        self[idx] = table.to_entry().into();
        Ok(())
    }

    /// Given a range of virtual addresses, returns the indices of entries in
    /// the current table that handle that address range. The need for this
    /// operation is a bit hard to explain without an example.
    /// 
    /// Let's illustrate this operation with an example. Suppose that we have a
    /// 2-level paging scheme (with only PDs and PTs) where the tables only hold
    /// 4 entries. Virtual addresses in this scheme consist of three parts: 2
    /// bits for the PDE index, 2 bits for the PTE index and n bits for an index
    /// into the page, but we will skip it as it's of no interest to us. Suppose
    /// also that we want to map the virtual address range `0b0101 .. 0b1011`
    /// (the last part of each bound is omitted).
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
    /// will return `1..3` when invoked on the only PD, `1..4` when invoked on
    /// PT 1, and `0..2` when invoked on PT 2.
    /// 
    /// In order to determine this range, however, tables below PML4 need to
    /// know their index in the upstream table. If the `Table` is not a PML4 and
    /// `upstream_idx` is `None`, `None` is returned.
    fn downstream_idx_range(&self, addresses: Range<VirtAddr>, upstream_idx: Option<usize>) -> Option<Range<usize>> {
        // if we're a PML4, we don't need to deal with overlaps
        // if self.kind == TableKind::Pml4 {
        //     return Some(
        //         self.kind.entry_idx(addresses.start)
        //         ..
        //         self.kind.entry_idx(addresses.end)
        //     );
        // }
        todo!();

        // 
    }
}

impl Index<usize> for Table<'_> {
    type Output = u64;
    /// Gets an entry using its index
    fn index(&self, index: usize) -> &Self::Output {
        &self.reference[index]
    }
}
impl IndexMut<usize> for Table<'_> {
    /// Gets an entry using its index
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.reference[index]
    }
}

pub struct TableIter<'a, 'b> {
    position: usize,
    table: &'a Table<'b>
}

impl Iterator for TableIter<'_, '_> {
    type Item = u64;
    fn next(&mut self) -> Option<Self::Item> {
        if self.position == ENTRY_CNT { return None; }
        self.position += 1;
        Some(self.table[self.position - 1])
    }
}

impl Debug for Table<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let addr = PhysAddr(self.reference as *const [u64; ENTRY_CNT] as usize);

        // ordinary page
        if self.kind == TableKind::Page {
            return write!(f, "Table: Page at {:?}", addr)
        }

        // page table
        writeln!(f, "Table: {:?} at {:?} [", self.kind, addr)?;
        for (i, entry) in self.iter().enumerate() {
            let entry: TableEntry = entry.into();
            if entry.present() {
                writeln!(f, "\t{i:#03}: {entry:?}")?;
            }
        }
        write!(f, "]")
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
        write!(f, "s{:#018x}", self.addr() << 12)
    }
}

impl CR3 {
    /// Gets the current address space
    fn get() -> CR3 {
        unsafe {
            let out: u64;
            asm!("mov rax, cr3", out("rax") out);
            out.into()
        }
    }

    /// Sets the current address space
    unsafe fn switch(&self) {
        let cr3: u64 = (*self).into();
        asm!("mov cr3, {0}", in(reg) cr3)
    }
}

/// TLB-aware wrapper for operations on address spaces.
/// 
/// TLB stands for Translation Lookaside Buffer and is basically a special kind
/// of cache that stores virtual-to-physical mappings. If the address space that
/// is being modified is active, the CPU has to be explicitly notified of these
/// changes using the `invlpg` instruction.
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
    /// Gets the current address space from the `CR3` register of the CPU. Care
    /// must be taken by the API user to ensure that there is only one
    /// `AddressSpace` wrapper per CR3 value, otherwise TLB consistency is not
    /// guaranteed.
    pub unsafe fn get_current() -> AddressSpace {
        AddressSpace {
            cr3: CR3::get(),
        }
    }

    /// Creates a new address space.
    pub fn new() -> Result<AddressSpace, super::AllocError> {
        let table = Table::allocate(TableKind::Pml4, TableAttrs {
            execute: true, write: true, user: true, cache: 0, global: false
        })?;
        Ok(AddressSpace {
            cr3: CR3::new().with_addr(table.reference as *const [u64; ENTRY_CNT] as usize >> 12),
        })
    }

    /// Determines whether the address space is active.
    pub fn is_current(&self) -> bool {
        *self == unsafe { Self::get_current() }
    }

    /// Returns the root PML4 table of the address space.
    pub fn root(&self) -> Table<'_> {
        let attrs = TableAttrs { cache: self.cr3.cache_mode(), ..Default::default() };
        Table::from_addr(self.cr3.addr(), TableKind::Pml4, attrs)
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

    /// Switches to an address space, consuming the wrapper. This function is
    /// only safe to call if:
    ///   - only safe modifications of the address space have been applied, and
    ///   - there are no undropped guards belonging to any address space.
    /// 
    /// If the second point is violated, the TLB may become inconsistent.
    pub unsafe fn set_as_current(&self) {
        self.cr3.switch();
    }

    /// Allocates a stack and returns the top address
    pub fn allocate_stack(&mut self, base: VirtAddr, page_cnt: usize, attrs: TableAttrs) -> Result<VirtAddr, AllocError> {
        // allocate physical pages
        let pages = phys::allocate(page_cnt);
        if pages.len() < page_cnt {
            let _ = phys::deallocate(pages);
            return Err(AllocError::OutOfMemory);
        }
    
        // map pages
        {
            let mut guard = self.modify();
            for (i, page) in pages.iter().enumerate() {
                // calculate virtual address
                let virt = base + VirtAddr(i * PAGE_SIZE);

                // map page
                guard.map_page(virt, page, attrs)?;
            }
        }
    
        // return top
        Ok(base + VirtAddr(pages.len() * PAGE_SIZE))
    }
}

impl Debug for AddressSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.cr3)
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
        if self.buffer.push(addr) == Err(()) {
            self.overflow = true;
        }
    }

    /// Maps consecutive virtual pages to consecutive physical pages
    pub fn map_range(&mut self, virt_start: VirtAddr, phys_start: PhysAddr, count: usize, attrs: TableAttrs) -> Result<(), AllocError> {
        // begin with PML4
        todo!();
    }

    /// Maps a virtual page to a physical page in an address space
    pub fn map_page(&mut self, virt: VirtAddr, phys: PhysAddr, attrs: TableAttrs) -> Result<(), AllocError> {
        // begin with PML4
        let mut table = self.space.root();

        // get or create PDPT, PD and PT
        loop {
            // stop at PT
            if table.kind == TableKind::Pt { break; };
            
            match table.downstream(virt) {
                // get downstream table
                Some(mut ds) => {
                    // apply new attributes
                    ds.attrs.merge(&attrs);
                    table.set_downstream(virt, Some(&mut ds)).unwrap();
                    table = ds;
                },
                // create downstream table
                None => {
                    let mut new_table = Table::allocate(table.kind.downstream().unwrap(), attrs)?;
                    table.set_downstream(virt, Some(&mut new_table)).unwrap();
                    table = new_table;
                },
            };
        }

        // set page entry
        table.set_downstream(virt, Some(&mut Table::from_addr(phys.0 >> 12, TableKind::Page, attrs))).unwrap();
        self.remember(virt);

        Ok(())
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
                // totally harmless
                let mut tmp = 0u64;
                asm!(
                    "mov {tmp}, cr3",
                    "mov cr3, {tmp}",
                    tmp = inout(reg) tmp => _,
                );
            }
            return;
        }

        // apply `invlpg`
        for addr in self.buffer.iter() {
            unsafe {
                // totally harmless
                asm!(
                    "invlpg [{ptr}]",
                    ptr = in(reg) addr.0 as u64,
                );
            }
        }
    }
}
