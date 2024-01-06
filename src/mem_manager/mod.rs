use core::fmt::{Display, Debug, Formatter, Result};
use derive_more::{Add, Sub, AddAssign, SubAssign, Into};

pub mod phys;

/// Represents a physical address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign, Into)]
pub struct PhysAddr(usize);
/// Represents a virtual address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign, Into)]
pub struct VirtAddr(usize);

impl Debug for PhysAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "p{:#018x}", self.0)
    }
}
impl Debug for VirtAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "v{:#018x}", self.0)
    }
}

/// The base address in the upper half of the virtual address space at which
/// the emulator resides (post-relocation). It is assumed that the base address
/// pre-relocation is 0.
const EMULATOR_BASE: VirtAddr = VirtAddr(0xffff_0000_0000_0000);

/// During early boot, the emulator will move all its virtual addresses,
/// pointers and references to the upper half of the virtual address space in
/// an operation called relocation. This trait unifies data types that must be
/// relocated.
pub trait Relocatable {
    fn relocate(&mut self);
}

impl Relocatable for VirtAddr {
    fn relocate(&mut self) {
        *self += EMULATOR_BASE;
    }
}

impl<T> Relocatable for *const T {
    fn relocate(&mut self) {
        *self = unsafe { self.byte_add(EMULATOR_BASE.0) }
    }
}
impl<T> Relocatable for *mut T {
    fn relocate(&mut self) {
        *self = unsafe { self.byte_add(EMULATOR_BASE.0) }
    }
}

impl<T> Relocatable for &T {
    fn relocate(&mut self) {
        *self = unsafe { &*((*self as *const T).byte_add(EMULATOR_BASE.0)) }
    }
}
impl<T> Relocatable for &mut T {
    fn relocate(&mut self) {
        *self = unsafe { &mut *((*self as *mut T).byte_add(EMULATOR_BASE.0)) }
    }
}

/// Display-friendly byte size type
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign)]
struct ByteSize(usize);

impl Display for ByteSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.0 >= 1024 * 1024 {
            write!(f, "{} MiB", self.0 / 1024 / 1024)
        } else if self.0 >= 1024 {
            write!(f, "{} KiB", self.0 / 1024)
        } else {
            write!(f, "{} bytes", self.0)
        }
    }
}
