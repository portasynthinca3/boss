use core::fmt::{Debug, Formatter, self};
use derive_more::{Add, Sub, AddAssign, SubAssign, Into};

pub mod phys;
pub mod virt;
pub mod reloc;

/// The size of the page. This MUST be set to 4K unless there's some major
/// tomfoolery going on.
pub const PAGE_SIZE: usize = 4096;

/// Represents a physical address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign, Into)]
pub struct PhysAddr(pub usize);
/// Represents a virtual address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign, Into)]
pub struct VirtAddr(pub usize);

/// Allocator error
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AllocError {
    /// Not enough memory to perform the allocation
    OutOfMemory,
    /// Object failed to deallocate because it does not belong to the allocator
    Unrecognized,
}

impl PhysAddr {
    /// Clears the page pointed to by the address
    unsafe fn clear_page(&self) -> Result<(), ()> {
        if self.0 % PAGE_SIZE != 0 { return Err(()); } // not pointing to the start of a page
        unsafe {
            let ptr = self.0 as *mut u8;
            ptr.write_bytes(0, PAGE_SIZE);
        }
        Ok(())
    }
}

impl VirtAddr {
    // i'm so funny and quirky omg
    /// Sign Extend. This method is safe.
    fn sex(&mut self) {
        self.0 &= 0x0000_ffff_ffff_ffff;
        if (self.0 >> 47) & 1 == 1 {
            self.0 |= 0xffff_0000_0000_0000;
        }
    }
}

impl Debug for PhysAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "p{:#018x}", self.0)
    }
}
impl Debug for VirtAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v{:#018x}", self.0)
    }
}
