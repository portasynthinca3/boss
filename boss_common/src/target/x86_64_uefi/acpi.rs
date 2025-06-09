use core::{fmt::Debug, ptr::{self, Pointee}, str};
use spin::Once;

use crate::target::memmgr::{PhysAddr, VirtAddr};

#[repr(C, packed)]
struct TableHeader {
    signature: [u8; 4],
    length: u32,
    revision: u8,
    checksum: u8,
    oem_id: [u8; 6],
    oem_table_id: [u8; 8],
    oem_revision: u32,
    creator_id: u32,
    creator_revision: u32,
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

trait Table {
    const SIGNATURE: &str;
    fn get_header(&self) -> &TableHeader;
    fn get_metadata(contents_length: usize) -> <Self as Pointee>::Metadata;

    /// Creates a safe reference to a table from its physical address. Makes
    /// sure that the table is well-formed.
    fn from_address(addr: PhysAddr) -> Option<&'static Self> {
        let header = TableHeader::from_address(addr);
        let addr: VirtAddr = addr.into();

        let signature = core::str::from_utf8(&header.signature).ok()?;
        if signature != Self::SIGNATURE { return None };

        let entire_table: *const u8 = addr.into();
        let entire_table = unsafe { core::slice::from_raw_parts(entire_table, header.length as usize) };
        let sum = entire_table
            .iter()
            .fold(0usize, |acc, element| acc + (*element as usize))
            & 0xff;
        if sum != 0 { return None };

        let contents_length = header.length as usize - size_of::<TableHeader>();
        let metadata = Self::get_metadata(contents_length);
        let typed_table: *const Self = core::ptr::from_raw_parts(<VirtAddr as Into<*const ()>>::into(addr), metadata);
        let typed_table = unsafe { typed_table.as_ref() }?;
        Some(typed_table)
    }
}

#[repr(C, packed)]
struct Xsdt {
    header: TableHeader,
    other_tables: [PhysAddr],
}
impl Table for Xsdt {
    const SIGNATURE: &str = "XSDT";
    fn get_header(&self) -> &TableHeader { &self.header }
    fn get_metadata(contents_length: usize) -> <Self as Pointee>::Metadata {
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
    fn len(&self) -> usize {
        // go try self.other_tables.len()!
        // god, rust is annoying sometimes.
        ptr::metadata(self)
    }

    fn iter(&self) -> XsdtIter {
        XsdtIter {
            position: 0,
            xsdt: self,
        }
    }
}

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
/// SAFETY: the provided pointer must point to an XSDP table. This function must
/// not have been called beforehand.
pub unsafe fn init(xsdp: PhysAddr) {
    let xsdp: *const Xsdp = <PhysAddr as Into<VirtAddr>>::into(xsdp).into();
    let xsdp: &Xsdp = xsdp.as_ref().unwrap();
    assert_eq!(xsdp.signature, [b'R', b'S', b'D', b' ', b'P', b'T', b'R', b' ']);

    let oem_id = core::str::from_utf8(&xsdp.oem_id).unwrap();
    log::debug!("XSDP OEM: \"{oem_id}\"");

    let xsdt = Xsdt::from_address(xsdp.xsdt_address).unwrap();
    log::trace!("{:?}", xsdt.get_header());
    for (header, addr) in xsdt.iter() {
        log::trace!("{header:?}");
    }

    XSDT.call_once(|| xsdt);
}

pub fn find_table<T: Table>() -> Option<&'static T> {
    let addr = XSDT
        .get()
        .unwrap()
        .iter()
        .find(|(header, addr)| str::from_utf8(&header.signature).unwrap() == T::SIGNATURE)?
        .1;
    T::from_address(addr)
}
