//! During early boot, the emulator moves all its virtual addresses, pointers
//! and references to the upper half of the virtual address space in an
//! operation called relocation. This module deals with this operation.

use core::{arch::asm, slice, mem::{self, size_of}};

use bitfield_struct::bitfield;
use uefi::table::boot::{MemoryMap, MemoryType};

use super::{
    PhysAddr, VirtAddr,
    virt::{AddressSpace, TableAttrs},
    EMULATOR_BASE, PAGE_SIZE,
};
use crate::checkpoint::{self, Checkpoint};

/// Trait for types that must be relocated in order to be valid past the
/// `RelocDone` checkpoint.
pub trait Relocatable {
    /// Relocates a `Relocatable` type, moving it into the upper half of the
    /// memory address space.
    fn relocate(&mut self);
}

/// Determines the offset for the relocation of an address
fn reloc_offset(addr: usize) -> usize {
    // check if the address has already been relocated
    if addr >= EMULATOR_BASE.0 { return 0 };
    // return the offset
    EMULATOR_BASE.0
}

impl From<VirtAddr> for PhysAddr {
    fn from(value: VirtAddr) -> Self {
        if value.0 < EMULATOR_BASE.0 {
            Self(value.0)
        } else {
            Self(value.0 - EMULATOR_BASE.0)
        }
    }
}
impl From<PhysAddr> for VirtAddr {
    fn from(value: PhysAddr) -> Self {
        let mut addr = VirtAddr(value.0);
        if checkpoint::get() >= Checkpoint::RelocDualMapping {
            addr.relocate();
        }
        addr
    }
}

impl Relocatable for VirtAddr {
    fn relocate(&mut self) {
        self.0 += reloc_offset(self.0);
    }
}

impl<T: ?Sized> Relocatable for *const T {
    fn relocate(&mut self) {
        *self = unsafe { self.byte_add(reloc_offset(*self as *const () as usize)) };
    }
}
impl<T: ?Sized> Relocatable for *mut T {
    fn relocate(&mut self) {
        *self = unsafe { self.byte_add(reloc_offset(*self as *mut () as usize)) };
    }
}

impl<T: ?Sized> Relocatable for &T {
    fn relocate(&mut self) {
        *self = unsafe {
            let ptr = *self as *const T;
            &*ptr.byte_add(reloc_offset(ptr as *const () as usize))
        }
    }
}
impl<T: ?Sized> Relocatable for &mut T {
    fn relocate(&mut self) {
        *self = unsafe {
            let ptr = *self as *mut T;
            &mut *ptr.byte_add(reloc_offset(ptr as *mut () as usize))
        }
    }
}

impl<T: Relocatable> Relocatable for Option<T> {
    fn relocate(&mut self) {
        match self.as_mut() {
            None => (),
            Some(val) => val.relocate(),
        }
    }
}

/// Makes a dual-mapped address space
pub fn make_dual_map(uefi_map: &MemoryMap) -> AddressSpace {
    // create a new address space
    let mut space = AddressSpace::new().unwrap();

    // map blocks as reported by UEFI
    // we're not querying the PMM to avoid the deadlock on its `ALL_RANGES` lock
    {
        let mut mut_space = space.modify();
        for entry in uefi_map.entries() {
            // determine attributes
            let attrs = match entry.ty {
                // data
                MemoryType::CONVENTIONAL |
                MemoryType::BOOT_SERVICES_CODE |
                MemoryType::BOOT_SERVICES_DATA |
                MemoryType::LOADER_DATA => TableAttrs { write: true, ..Default::default() },
                // code
                MemoryType::LOADER_CODE => TableAttrs { write: true, execute: true, ..Default::default() },
                // unusable
                _ => continue,
            };

            // map range to both halves
            let mut virt = VirtAddr(entry.phys_start as usize);
            mut_space.map_range(virt, PhysAddr(entry.phys_start as usize), entry.page_count as usize, attrs, false).unwrap();
            virt.relocate();
            mut_space.map_range(virt, PhysAddr(entry.phys_start as usize), entry.page_count as usize, attrs, false).unwrap();
        }
    }

    log::debug!("created dual-mapped address space: {space:?}");

    // switch to the new address space
    unsafe { space.set_as_current(); }
    checkpoint::advance(Checkpoint::RelocDualMapping).unwrap();

    space
}

/// Performs an instruction pointer jump to the upper half, calling `to` with
/// the argument `arg`. Since a new stack is created, `arg` must be a reference
/// to a heap object.
pub unsafe fn execute_jump<Arg>(
    stack_top: VirtAddr,
    to: extern "C" fn(&Arg) -> !,
    mut arg: &Arg
) -> ! {
    // relocate references
    assert!(checkpoint::get() >= Checkpoint::RelocDualMapping);
    let mut jump_ptr = to as *const ();
    jump_ptr.relocate();
    arg.relocate();

    asm!(
        "mov rsp, {stack}",
        "mov rbp, {stack}",
        "push {arg}",
        "call {jump_ptr}",
        stack = in(reg) stack_top.0,
        arg = in(reg) arg,
        jump_ptr = in(reg) jump_ptr,
    );

    // we will never end up here
    panic!("how");
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[derive(strum_macros::FromRepr)]
enum PeRelocType {
    Skip = 0,
    High16bits = 1,
    Low16bits = 2,
    Low32bits = 3,
    High16bitsAndAdjustLow = 4,
    All64bits = 10,
}

#[bitfield(u64)]
struct PeRelocBlkHdr {
    page: u32,
    size: u32,
}

#[bitfield(u16)]
struct PeRelocEntry {
    #[bits(12)]
    page_offset: u16,
    #[bits(4)]
    e_type: u8,
}

/// Relocates all pointers that were created during compilation, such as statics
/// and function pointers. Also relocates pointers created at runtime that
/// reside in static variables. Does not relocate other references and pointers
/// that were generated at runtime - they need to be relocated manually, for
/// example using the [Relocatable] trait.
pub fn relocate_pe(image_info: (*const core::ffi::c_void, u64)) {
    let image = image_info.0 as *mut u8;
    let img_len = image_info.1 as usize;

    // The binary has a special marker page, followed by one or more pages with
    // relocation data in the PE format, finally followed by the end of the
    // image. That marker page is 1024 bytes of random data, followed by the
    // same random data, followed by a description of the `.data` section.
    // Because there's just that much random data used as the marker, it is just
    // as likely for us to mistake other data for the marker as it is for
    // someone to guess 32 AES-256 keys at once.

    // find start of relocation data
    // SAFETY: image is static, an allocation boundary is not crossed. Other
    // contract points are trivial to prove.
    let mut reloc_data = unsafe { image.byte_add(img_len - PAGE_SIZE) };
    let data_sect_desc;
    loop {
        if reloc_data == image {
            panic!("reached start of image searching for magic marker page");
        }

        // check if page is a magic marker (read large comment for explanation)
        // SAFETY: an allocation boundary is not crossed, ptr is aligned by 4096
        // bytes, other contract points are trivial to prove.
        let first_half = unsafe { slice::from_raw_parts(reloc_data, PAGE_SIZE / 4) };
        // SAFETY: combines points from the previous safety declarations
        let second_half = unsafe { slice::from_raw_parts(reloc_data.byte_add(PAGE_SIZE / 4), PAGE_SIZE / 4) };
        if first_half == second_half {
            // found magic marker page, relocation info starts after it
            // SAFETY: read first safety declaration
            data_sect_desc = unsafe { reloc_data.byte_add(PAGE_SIZE / 2) };
            // SAFETY: read first safety declaration
            reloc_data = unsafe { reloc_data.byte_add(PAGE_SIZE) };
            break;
        }

        // SAFETY: read first safety declaration
        reloc_data = unsafe { reloc_data.byte_offset(-(PAGE_SIZE as isize)) };
    }

    let relocation_offset = EMULATOR_BASE.0;
    assert!(relocation_offset & 0xFFFF_FFFF == 0);
    log::trace!(".reloc section at {reloc_data:#x?}");

    // read relocation data
    loop {
        // read block header (describes one full page)
        // SAFETY: image is static, an allocation boundary is not crossed;
        // pointer is aligned by at least two bytes; data is of that type thanks
        // to the PE spec and build system.
        let blk_header = unsafe { mem::transmute::<*mut u8, *const PeRelocBlkHdr>(reloc_data).read_unaligned() };
        // SAFETY: read previous safety declaration
        reloc_data = unsafe { reloc_data.byte_add(size_of::<PeRelocBlkHdr>()) };
        let entry_cnt = (blk_header.size() as usize - size_of::<PeRelocBlkHdr>()) / size_of::<PeRelocEntry>();

        // detect end of section by seeing if header is invalid
        // (this is quite janky tbh)
        if blk_header.size() == 0
        || entry_cnt > PAGE_SIZE / 4
        || blk_header.page() > img_len as u32 {
            log::trace!("stopping at {reloc_data:#x?}: {blk_header:x?}");
            break;
        }

        // read entries. an entry may occupy two slots, thus for is not used
        let mut entry_idx = 0;
        loop {
            if entry_idx >= entry_cnt { break; }

            // SAFETY: read previous safety declaration
            let entry = unsafe { *mem::transmute::<*mut u8, *const PeRelocEntry>(reloc_data) };
            let entry_type = PeRelocType::from_repr(entry.e_type().into());
            // SAFETY: read previous safety declaration
            let field_ptr = unsafe { image.byte_add(blk_header.page() as usize + entry.page_offset() as usize) };
            if (field_ptr as usize) >= 0x460b000 && (field_ptr as usize) < 0x460c000 {
                // log::trace!("{field_ptr:?}");
            }

            match entry_type {
                None => panic!("invalid relocation type {}", entry.e_type()),
                Some(PeRelocType::Skip) |
                // a no-op thanks to the fact that these sub-fields are zero
                // (read assert in the middle of the function)
                Some(PeRelocType::High16bits) |
                Some(PeRelocType::Low16bits) |
                Some(PeRelocType::Low32bits) => (),
                Some(PeRelocType::High16bitsAndAdjustLow) => unsafe {
                    let field_ptr = field_ptr as *mut u32; // safe
                    // SAFETY: read safety declaration at beginning of outer loop
                    let adjust_by = reloc_data.byte_add(size_of::<u16>()) as *mut u16;
                    // SAFETY: read previous safety declaration
                    *field_ptr += *adjust_by as u32;
                },
                Some(PeRelocType::All64bits) => unsafe {
                    let field_ptr = field_ptr as *mut u64; // safe
                    // SAFETY: read previous safety declaration
                    *field_ptr += relocation_offset as u64;
                }
            }

            // next entry
            let entry_occupies_slots = if matches!(entry_type, Some(PeRelocType::High16bitsAndAdjustLow)) { 2 } else { 1 };
            // SAFETY: read safety declaration at beginning of outer loop
            reloc_data = unsafe { reloc_data.byte_add(size_of::<PeRelocEntry>() * entry_occupies_slots) };
            entry_idx += entry_occupies_slots;
        }
    }

    // Here's the really scary part: relocating pointers stored in `static`s
    // that were created at runtime.
    //
    // This is really bad. If something breaks, this is the first place to look
    // at. This is very bad. This is extraordinarily bad.
    //
    // I won't write any `SAFETY` declarations because this is unsafe and bad.
    // There's just no other way to put it. I'm sorry, I've been fighting the
    // linker for the better part of the last 24 hours in order to do this
    // less wrongly, but I failed.
    //
    // Here's the problem: I'm stupid and I want the kernel to relocate itself
    // to the upper half of the memory space. Usually this is done by the
    // bootloader, but I'm stupid and lazy. Relocation is actually performed by
    // the firmware at load time using the same data that we used in the huge
    // block of code above - that's fine. The problem is that rustc starting
    // with version `nightly-2024-04-17` generates code that generates pointers
    // to statics and puts them in other statics. I was able to pin it down to a
    // specific commit:
    //     https://github.com/rust-lang/rust/commit/38104f3a8838f8662ad3cccc4d7262a96bf9724e
    // ...but it seems unrelated. It's about ZSTs? I dunno. But it broke my
    // code. It's not at fault - I think that no toolchain for any language on
    // this beautiful earth supports what I want to do.
    //
    // The proper solution is to write or use a bootloader that will relocate
    // the kernel before it even starts executing. But no. If you are not me and
    // you are terrified with what I'm about to do a few lines below, you can
    // submit a PR. Thanks for sticking around till the end of this comment.

    // Aight. There's a line in the marker page taken straight from `objdump`
    // describing the `.data` section. We need to parse it, extracting the addr
    // and size of that section.
    let data = unsafe { core::slice::from_raw_parts(data_sect_desc, PAGE_SIZE / 2) };
    let objdump_line = crate::util::from_null_term(data).expect("no string describing .data section");
    let mut columns = objdump_line.split(' ').filter(|c| !c.is_empty());
    let data_size = usize::from_str_radix(columns.nth(2).expect("no size column"), 16).expect("bad data in size column");
    let data_start = usize::from_str_radix(columns.next().expect("no address column"), 16).expect("bad data in address column")
        - 0x140000000 + image as usize;
    let statics = unsafe { core::slice::from_raw_parts_mut(data_start as *mut usize, data_size / size_of::<usize>()) };

    // The `statics` variable now contains a view on all `static` variables as a
    // usize slice. See which numbers point to memory in the kernel image and
    // relocate them.
    for number in statics.iter_mut() {
        if *number >= image as usize && *number < (image as usize + img_len) {
            log::debug!("footgun: {number:#x} at {:?}", number as *const usize);
            *number += relocation_offset;
        }
    }
}
