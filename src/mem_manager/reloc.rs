//! During early boot, the emulator moves all its virtual addresses, pointers
//! and references to the upper half of the virtual address space in an
//! operation called relocation. This module deals with this operation.

use core::arch::asm;
use uefi::table::boot::{MemoryMap, MemoryType};
use super::{PhysAddr, VirtAddr, virt::{AddressSpace, TableAttrs}, PAGE_SIZE};
use crate::checkpoint::{self, Checkpoint};

/// The base address in the upper half of the virtual address space at which
/// the emulator resides (post-relocation).
pub const EMULATOR_BASE: VirtAddr = VirtAddr(0xffff_8000_0000_0000);
/// The base address of the emulator stack in the upper half. Each processor has
/// its own stack, though all of them are located at the same virtual address.
pub const EMULATOR_STK_BASE: VirtAddr = VirtAddr(0xffff_ff80_0000_0000);

/// Trait for types that must be relocated in order to be valid past the
/// `RelocDone` checkpoint.
pub trait Relocatable {
    /// Relocates a `Relocatable` type, moving it into the upper half of the
    /// memory address space.
    fn relocate(&mut self);
}

/// Determines the offset for the relocation of an address
fn reloc_offset(addr: usize) -> usize {
    // check whether relocation is safe
    // assert!(checkpoint::get() >= Checkpoint::RelocDualMapping);
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
        addr.relocate();
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
            for page in 0..entry.page_count {
                let phys = PhysAddr(entry.phys_start as usize + (page as usize * PAGE_SIZE));
                let mut virt = VirtAddr(phys.0);
                mut_space.map_page(virt, phys, attrs).unwrap();
                virt.relocate();
                mut_space.map_page(virt, phys, attrs).unwrap();
            }
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
