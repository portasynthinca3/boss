//! Virtual Memory Manager. Maps virtual pages to physical pages. Assumes that
//! 4-level paging is used. Based on Volume 3, Chapter 4 of the Intel SDM.

use crate::mem_manager::{phys, VirtAddr, PhysAddr, PAGE_SIZE, Relocatable};
use bitfield_struct::bitfield;
use uefi::table::boot::MemoryType;
use core::{
    arch::asm,
    fmt::{self, Debug, Formatter},
    mem::size_of, ops::{Range, IndexMut, Index}
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
#[derive(Default, PartialEq, Clone, Copy)]
pub struct TableAttrs {
    /// 1 = Writes allowed UDD, 0 = Writes disallowed
    pub write: bool,
    /// 1 = User (CPL=3) access allowed UDD, 0 = Supervisor (CPL<3) access only
    pub user: bool,
    /// Cache mode
    pub cache: u8,
    /// 0 = Execution allowed UDD, 1 = Execution disallowed
    pub exec_disable: bool,
}

impl TableAttrs {
    fn from_entry(entry: &TableEntry) -> Self {
        Self {
            write: entry.write(),
            user: entry.user(),
            cache: entry.cache_mode() | (entry.cache_mode_2() << 2),
            exec_disable: entry.exec_disable(),
        }
    }

    fn write_to_entry(&self, entry: &mut TableEntry) {
        entry.set_write(self.write);
        entry.set_user(self.user);
        entry.set_cache_mode(self.cache & 0b11);
        entry.set_cache_mode_2((self.cache & 0b100) >> 2);
        entry.set_exec_disable(self.exec_disable);
    }

    fn merge(&mut self, other: &Self) {
        self.write |= other.write;
        self.user |= other.user;
        // caching and XD are purposefully ignored
    }
}

/// The actual table in actual memory as a slice
type TableRef<'a> = &'a mut [u64; ENTRY_CNT];

/// The four levels of tables and the end `Page`
#[derive(Debug, PartialEq)]
enum TableKind {
    PML4, PDPT, PD, PT, Page
}

impl TableKind {
    /// Determines the bit range in a virtual address this table kind handles
    fn bit_range(&self) -> Option<Range<usize>> {
        match *self {
            Self::Page => None,
            Self::PT => Some(12..21),
            Self::PD => Some(21..30),
            Self::PDPT => Some(30..39),
            Self::PML4 => Some(39..48),
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
            Self::PT => Some(Self::Page),
            Self::PD => Some(Self::PT),
            Self::PDPT => Some(Self::PD),
            Self::PML4 => Some(Self::PDPT),
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

    /// Allocates a table
    fn allocate(kind: TableKind) -> Result<Self, &'static str> {
        match phys::allocate(1).iter().next() {
            None => return Err("failed to allocate physical page for page table"),
            Some(page) => Ok(Self::from_addr(page.0 >> 12, kind, Default::default()))
        }
    }

    /// Returns an entry that can be used to reference this table
    fn to_entry(&mut self) -> TableEntry {
        let mut entry = TableEntry::new()
            .with_addr((self.reference as *mut [u64; ENTRY_CNT] as usize) >> 12)
            .with_present(true);
        self.attrs.write_to_entry(&mut entry);
        entry
    }

    /// Creates an iterator for the table
    fn iter(&self) -> TableIter {
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
    fn set_downstream(&mut self, addr: VirtAddr, table: Option<&mut Table<'a>>) -> Result<(), &'static str> {
        // check if None was supplied
        let Some(table) = table else {
            let idx = self.kind.entry_idx(addr).ok_or("the Table is actually a plain page")?;
            self[idx] = TableEntry::new().with_present(false).into();
            return Ok(());
        };

        // check kinds
        if table.kind != self.kind.downstream().ok_or("the Table is actually a plain page")? {
            return Err("downstream table kind mismatch")
        }

        // set entry
        let idx = self.kind.entry_idx(addr).ok_or("the Table is actually a plain page")?;
        self[idx] = table.to_entry().into();
        Ok(())
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

struct TableIter<'a, 'b> {
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
        write!(f, "Table: {:?} at {:?} [\n", self.kind, addr)?;
        for (i, entry) in self.iter().enumerate() {
            let entry: TableEntry = entry.into();
            if entry.present() {
                write!(f, "\t{i:#03}: {entry:?}\n")?;
            }
        }
        write!(f, "]")
    }
}

/// The value of Control Register 3. Each distinct CR3 value is associated with
/// a different address space.
#[bitfield(u64, debug = false)]
pub struct CR3 {
    /// Ignored
    #[bits(3)]
    _ign1: u8,
    /// PCD,PWT: Cache access mode for downstream PML4
    #[bits(2)]
    cache_mode: u8,
    /// Ignored
    #[bits(7)]
    _ign2: u8,
    /// Upper 40 bits of the physical address
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
    /// Creates an address space and returns the CR3 associated with it
    pub fn create() -> Result<Self, &'static str> {
        // allocate a page for the PML4
        let page = Table::allocate(TableKind::PML4)?;

        // return cr3
        Ok(CR3::new().with_addr(page.reference as *const [u64; ENTRY_CNT] as usize >> 12))
    }

    /// Gets the current address space
    pub fn get() -> CR3 {
        unsafe {
            let out: u64;
            asm!("mov rax, cr3", out("rax") out);
            out.into()
        }
    }

    /// Returns the root PML4 table
    pub fn root(&self) -> Table<'_> {
        let attrs = TableAttrs { cache: self.cache_mode(), ..Default::default() };
        Table::from_addr(self.addr(), TableKind::PML4, attrs)
    }

    /// Sets the current address space
    pub unsafe fn switch(&self) {
        let cr3: u64 = (*self).into();
        asm!("mov cr3, rax", in("rax") cr3)
    }

    /// Adds a page to the memory map
    pub fn map_page(&self, virt: VirtAddr, phys: PhysAddr, attrs: TableAttrs) -> Result<(), &'static str> {
        // begin with PML4
        let mut table = self.root();

        // get or create PDPT, PD and PT
        loop {
            // stop at PT
            if table.kind == TableKind::PT { break; };
            
            match table.downstream(virt) {
                // get downstream table
                Some(mut ds) => {
                    // apply new attributes
                    ds.attrs.merge(&attrs);
                    table.set_downstream(virt, Some(&mut ds))?;
                    table = ds;
                },
                // create downstream table
                None => {
                    let mut new_table = Table::allocate(table.kind.downstream().unwrap())?;
                    new_table.attrs.merge(&attrs);
                    table.set_downstream(virt, Some(&mut new_table))?;
                    table = new_table;
                },
            };
        }

        // set page entry
        table.set_downstream(virt, Some(&mut Table::from_addr(phys.0 >> 12, TableKind::Page, attrs)))?;

        Ok(())
    }
}

/// Initializes the VMM, entering phase 1 of the upper half migration
pub fn init(uefi_map: &uefi::table::boot::MemoryMap) -> CR3 {
    // create a new address space
    let cr3 = CR3::create().unwrap();

    // map blocks as reported by UEFI
    // this is to avoid the deadlock on `ALL_RANGES`
    for entry in uefi_map.entries() {
        let attrs = match entry.ty {
            // data
            MemoryType::CONVENTIONAL |
            MemoryType::BOOT_SERVICES_CODE |
            MemoryType::BOOT_SERVICES_DATA |
            MemoryType::LOADER_DATA => TableAttrs { write: true, user: false, cache: 0, exec_disable: true },
            // code
            MemoryType::LOADER_CODE => TableAttrs { write: true, user: false, cache: 0, exec_disable: false },
            _ => continue,
        };
        for page in 0..entry.page_count {
            let phys = PhysAddr(entry.phys_start as usize + (page as usize * PAGE_SIZE));
            let mut virt = VirtAddr(phys.0);
            cr3.map_page(virt, phys, attrs).unwrap();
            virt.relocate();
            cr3.map_page(virt, phys, attrs).unwrap();
        }
    }

    log::info!("vmm: created new address space {:?}", cr3);

    // switch to the new address space
    unsafe { cr3.switch(); }

    log::info!("vmm: switched to new address space");

    cr3
}
