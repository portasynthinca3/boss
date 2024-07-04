use core::fmt::{Debug, Formatter, self};
use derive_more::{Add, Sub, AddAssign, SubAssign, Into};

pub mod phys;
pub mod virt;
pub mod reloc;
// pub mod malloc;

// The following constants outline the memory map:
//   - `0` - `0xffff_8000_0000_0000`: NIF memory, identity mapped physical
//     memory before the `RelocDualMapping` checkpoint;
//   - `0xffff_8000_0000_0000` - `0xffff_c000_0000_0000`: identity mapped
//     physical memory past the `RelocDualMapping` checkpoint;
//   - `0xffff_c000_0000_0000` - `0xffff_c000_0001_0000`: emulator stack;
//   - `0xffff_c000_0001_0000` - `0xffff_ffff_ffff_ffff`: emulator heap.

/// The size of the page. This MUST be set to 4K unless there's some major
/// tomfoolery going on.
pub const PAGE_SIZE: usize = 4096;
/// The base address in the upper half of the virtual address space at which
/// the emulator resides (post-relocation).
pub const EMULATOR_BASE: VirtAddr = VirtAddr(0xffff_8000_0000_0000);
/// The base address of the emulator stack in the upper half. Each processor has
/// its own stack, though all of them are located at the same virtual address.
pub const EMULATOR_STK_BASE: VirtAddr = VirtAddr(0xffff_c000_0000_0000);
/// How many pages the emulator's stack has.
pub const EMULATOR_STK_PAGES: usize = 16;
/// The base address in the upper half of the virtual address space at which
/// the emulator heap resides.
pub const EMULATOR_HEAP: VirtAddr = VirtAddr(EMULATOR_STK_BASE.0 + (EMULATOR_STK_PAGES * PAGE_SIZE));

/// Represents a physical address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign, Into)]
pub struct PhysAddr(pub usize);
/// Represents a virtual address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct VirtAddr(usize);

/// Allocator error
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MemMgrError {
    /// Not enough memory to perform the allocation
    OutOfMemory,
    /// Object failed to deallocate because it does not belong to the allocator
    UnrecognizedDealloc,
    /// Invalid `lowest_addr` supplied to [`virt::RawTable::new`]
    InvalidLowestAddr,
    /// The maximum number of registered subspaces allowed has been reached
    MaxSubspacesReached,
    /// It was unclear what was the supplied address range meant
    InvalidAddrRange,
}

impl PhysAddr {
    /// Clears the page pointed to by the address
    pub unsafe fn clear_page(&self) -> Result<(), ()> {
        if self.0 % PAGE_SIZE != 0 { return Err(()); } // not pointing to the start of a page
        unsafe {
            let virt: VirtAddr = (*self).into();
            let ptr = virt.0 as *mut u8;
            ptr.write_bytes(0, PAGE_SIZE);
        }
        Ok(())
    }
}

impl VirtAddr {
    /// Sign extends the integer and forms a virtual address
    pub const fn from_usize(mut int: usize) -> VirtAddr {
        int &= 0x0000_ffff_ffff_ffff;
        if (int >> 47) & 1 == 1 {
            int |= 0xffff_0000_0000_0000;
        }
        VirtAddr(int)
    }

    /// Forms a virtual address.
    /// 
    /// # Safety
    /// The supplied integer must be correctly sign extended
    pub unsafe fn from_usize_unchecked(int: usize) -> VirtAddr {
        VirtAddr(int)
    }
}

impl Debug for PhysAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[33mp\x1b[0m{:#018x}", self.0)
    }
}
impl Debug for VirtAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[36mv\x1b[0m{:#018x}", self.0)
    }
}

impl<T> From<&T> for VirtAddr {
    fn from(value: &T) -> Self {
        VirtAddr(value as *const T as usize)
    }
}
impl<T> From<&mut T> for VirtAddr {
    fn from(value: &mut T) -> Self {
        VirtAddr(value as *const T as usize)
    }
}

impl From<VirtAddr> for usize {
    fn from(value: VirtAddr) -> Self {
        value.0
    }
}
impl<T> From<VirtAddr> for *mut T {
    fn from(value: VirtAddr) -> Self {
        value.0 as *mut T
    }
}

impl<T> From<*mut T> for VirtAddr {
    fn from(value: *mut T) -> Self {
        VirtAddr(value as usize)
    }
}

impl From<usize> for VirtAddr {
    fn from(value: usize) -> Self {
        VirtAddr::from_usize(value)
    }
}
