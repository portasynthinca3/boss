//! Implements basic parsing of Advanced Configuration and Power Interface
//! data tables (those that contain no ACPI Machine Language bytecode).
//! 
//! Based on ACPI specification version 6.5

use core::{fmt::Debug, ptr::{self, Pointee}, str};
use spin::Once;

use crate::target::memmgr::{PhysAddr, VirtAddr};
use crate::util::boot_stage;

/// Headers of all ACPI tables (except XSDP)
#[repr(C, packed)]
pub struct TableHeader {
    signature: [u8; 4],
    length: u32,
    revision: u8,
    checksum: u8,
    oem_id: [u8; 6],
    oem_table_id: [u8; 8],
    oem_revision: u32,
    compiler_id: u32,
    compiler_revision: u32,
}

impl TableHeader {
    fn from_address(addr: PhysAddr) -> &'static Self {
        let addr: VirtAddr = addr.into();
        let header: *const Self = addr.into();
        unsafe { header.as_ref() }.unwrap()
    }
}

impl Debug for TableHeader {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let signature = str::from_utf8(&self.signature).unwrap();
        let rev = self.revision;
        let oem = str::from_utf8(&self.oem_id).unwrap();
        let oem_table_id = str::from_utf8(&self.oem_table_id).unwrap();
        let oem_rev = self.oem_revision;
        write!(f, "{signature} rev {rev}: OEM \"{oem}\", OEM ID \"{oem_table_id}\", rev {oem_rev}")
    }
}

/// Trait for all tables except the XSDP
pub trait Table {
    const SIGNATURE: &str;
    fn get_header(&self) -> &TableHeader;

    /// Most tables are dynamically sized structs with a trailing array. This
    /// method is called by the provided `from_address` function to figure out
    /// the metadata of the struct based on the length of the table's contents
    /// in bytes. In most cases this metadata would be the length (in elements)
    /// of the trailing array.
    fn get_dst_metadata(contents_length: usize) -> <Self as Pointee>::Metadata;

    /// Creates a safe reference to a table from its physical address. Makes
    /// sure that the table is well-formed.
    fn from_address(addr: PhysAddr) -> Option<&'static Self> {
        let header = TableHeader::from_address(addr);
        let addr: VirtAddr = addr.into();

        let signature = core::str::from_utf8(&header.signature).ok()?;
        if signature != Self::SIGNATURE { return None };

        let entire_table: *const u8 = addr.into();
        // SAFETY: the major concern here is aliasing, which in the case of
        // shared references is OK
        let entire_table = unsafe { core::slice::from_raw_parts(entire_table, header.length as usize) };
        let sum = entire_table
            .iter()
            .fold(0usize, |acc, element| acc + (*element as usize))
            & 0xff;
        if sum != 0 { return None };

        let contents_length = header.length as usize - size_of::<TableHeader>();
        let metadata = Self::get_dst_metadata(contents_length);
        let typed_table: *const Self = core::ptr::from_raw_parts(<VirtAddr as Into<*const ()>>::into(addr), metadata);
        // SAFETY: shared aliasing is OK
        let typed_table = unsafe { typed_table.as_ref() }?;
        Some(typed_table)
    }
}

/// Extended System Descriptor Table: points to all the other tables
#[repr(C, packed)]
struct Xsdt {
    header: TableHeader,
    other_tables: [PhysAddr],
}
impl Table for Xsdt {
    const SIGNATURE: &str = "XSDT";
    fn get_header(&self) -> &TableHeader { &self.header }
    fn get_dst_metadata(contents_length: usize) -> <Self as Pointee>::Metadata {
        contents_length / size_of::<PhysAddr>()
    }
}

struct XsdtIter<'tab> {
    position: usize,
    xsdt: &'tab Xsdt,
}

impl<'tab> Iterator for XsdtIter<'tab> {
    type Item = (&'tab TableHeader, PhysAddr);

    fn next(&mut self) -> Option<Self::Item> {
        if self.position == self.xsdt.len() { return None; }
        self.position += 1;
        let addr = self.xsdt.other_tables[self.position - 1];
        Some((TableHeader::from_address(addr), addr))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.xsdt.len() - self.position;
        (size, Some(size))
    }
}

impl Xsdt {
    pub fn len(&self) -> usize {
        // go try self.other_tables.len()!
        // god, rust is annoying sometimes.
        ptr::metadata(self)
    }

    pub fn iter(&self) -> XsdtIter {
        XsdtIter {
            position: 0,
            xsdt: self,
        }
    }
}

/// Extended System Descriptor Table Pointer: points to the only XSDT
#[repr(C, packed)]
struct Xsdp {
    signature: [u8; 8],
    checksum: u8,
    oem_id: [u8; 6],
    revision: u8,
    rsdt_address: u32,
    length: u32,
    xsdt_address: PhysAddr,
    extended_checksum: u8,
    reserved: [u8; 3],
}

static XSDT: Once<&Xsdt> = Once::new();

/// Initializes ACPI using the XSDP structure pointer
/// 
/// # Safety
/// The provided pointer must point to an XSDP table. This function must not
/// have been called beforehand.
pub unsafe fn init(xsdp: PhysAddr) {
    boot_stage::new_level();

    boot_stage::same_level("Parsing XSDP");
    let xsdp: *const Xsdp = <PhysAddr as Into<VirtAddr>>::into(xsdp).into();
    let xsdp: &Xsdp = xsdp.as_ref().unwrap();
    assert_eq!(str::from_utf8(&xsdp.signature).unwrap(), "RSD PTR ");

    let oem_id = core::str::from_utf8(&xsdp.oem_id).unwrap();
    log::info!("ACPI OEM: \"{oem_id}\"");

    boot_stage::same_level("Parsing XSDT");
    let xsdt = Xsdt::from_address(xsdp.xsdt_address).unwrap();
    log::trace!("{:?}", xsdt.get_header());
    for (header, _addr) in xsdt.iter() {
        log::trace!("{header:?}");
    }

    boot_stage::end_level();
    XSDT.call_once(|| xsdt);
}

pub fn find_table<T: Table + ?Sized>() -> Option<&'static T> {
    let addr = XSDT
        .get()
        .unwrap()
        .iter()
        .find(|(header, _addr)| str::from_utf8(&header.signature).unwrap() == T::SIGNATURE)?
        .1;
    T::from_address(addr)
}
