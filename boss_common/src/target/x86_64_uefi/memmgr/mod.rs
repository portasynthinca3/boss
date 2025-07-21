use core::fmt::{self, Debug, Formatter};
use derive_more::{Add, Sub, AddAssign, SubAssign};
use super::runtime_cfg::{self, CfgFlags};
use crate::impl_equally_sized;

pub mod phys;
pub mod virt;
pub mod malloc;

/// Represents a physical address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign)]
#[repr(transparent)]
pub struct PhysAddr(pub usize);

/// Represents a virtual address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign)]
#[repr(transparent)]
pub struct VirtAddr(usize);

impl_equally_sized!(PhysAddr, VirtAddr);

/// The size of the page. This MUST be set to 4K unless there's some major
/// tomfoolery going on.
pub const PAGE_SIZE: usize = 4096;

pub mod layout {
    // The following constants outline the memory map:
    //   - `0x0000_0000_0000_0000` - `0x0000_7fff_ffff_ffff`: NIF memory (TBD) or identity-mapped physical memory;
    //   - `0xffff_8000_0000_0000` - `0xffff_c000_0000_0000`: identity mapped (w/ offset) physical memory;
    //   - `0xffff_c000_0000_0000` - `0xffff_cfff_fffe_ffff`: emulator image;
    //   - `0xffff_cfff_ffff_0000` - `0xffff_cfff_ffff_ffff`: bootloader-to-emulator glue;
    //   - `0xffff_d000_0000_0000` - `0xffff_dfff_ffff_ffff`: base image;
    //   - `0xffff_e000_0000_0000` - `0xffff_efff_ffff_ffff`: emulator stack;
    //   - `0xffff_f000_0000_0000` - `0xffff_ffff_ffff_ffff`: emulator heap.

    use super::VirtAddr;

    pub const NIF_TOP: VirtAddr = VirtAddr(0x0000_7fff_0000_0000);
    pub const IDENTITY_BASE: VirtAddr = VirtAddr(0xffff_8000_0000_0000);
    pub const EMULATOR_IMG_BASE: VirtAddr = VirtAddr(0xffff_c000_0000_0000);
    pub const GLUE_BASE: VirtAddr = VirtAddr(0xffff_cfff_ffff_0000);
    pub const BASE_IMG_BASE: VirtAddr = VirtAddr(0xffff_d000_0000_0000);
    pub const EMULATOR_STACK_BASE: VirtAddr = VirtAddr(0xffff_e000_0000_0000);
    pub const EMULATOR_HEAP_BASE: VirtAddr = VirtAddr(0xffff_f000_0000_0000);

    pub const EMULATOR_STK_PAGES: usize = 16;

    #[derive(PartialEq, Eq, Clone, Copy)]
    pub enum Region {
        Unknown,
        NifOrIdentity,
        IdentityPhysical,
        Glue,
        EmulatorImage,
        BaseImage,
        EmulatorStack,
        EmulatorHeap,
    }

    pub fn determine_region(addr: VirtAddr) -> Region {
        if addr < NIF_TOP { return Region::NifOrIdentity };
        if addr < EMULATOR_IMG_BASE { return Region::IdentityPhysical };
        if addr < GLUE_BASE { return Region::EmulatorImage };
        if addr < BASE_IMG_BASE { return Region::Glue };
        if addr < EMULATOR_STACK_BASE { return Region::BaseImage };
        if addr < EMULATOR_HEAP_BASE { return Region::EmulatorStack };
        Region::EmulatorHeap
    }
}

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
    /// An improperly aligned address was supplied
    BadAlignment,
}

impl PhysAddr {
    /// Clears the page pointed to by the address
    /// 
    /// # Safety
    /// There must not be any references, shared or exclusive, to any content
    /// within the page
    pub unsafe fn clear_page(&self) -> Result<(), MemMgrError> {
        if self.0 % PAGE_SIZE != 0 { return Err(MemMgrError::BadAlignment); }
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
    /// The supplied integer must be correctly sign extended, otherwise the CPU
    /// will throw an exception when it is used.
    pub const unsafe fn from_usize_unchecked(int: usize) -> VirtAddr {
        VirtAddr(int)
    }
}

impl From<VirtAddr> for PhysAddr {
    fn from(value: VirtAddr) -> Self {
        let in_upper_half = runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf);
        match layout::determine_region(value) {
            layout::Region::IdentityPhysical => Self((value - layout::IDENTITY_BASE).0),
            layout::Region::NifOrIdentity if !in_upper_half => Self(value.0),
            _ => panic!("non-identity mapped VAddr: {value:?}"),
        }
    }
}

impl From<PhysAddr> for VirtAddr {
    fn from(value: PhysAddr) -> Self {
        let mut addr = VirtAddr(value.0);
        if runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf) {
            addr += layout::IDENTITY_BASE;
        }
        addr
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
impl<T> From<VirtAddr> for *const T {
    fn from(value: VirtAddr) -> Self {
        value.0 as *const T
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

impl<T> From<*const T> for VirtAddr {
    fn from(value: *const T) -> Self {
        VirtAddr(value as usize)
    }
}

impl From<usize> for VirtAddr {
    fn from(value: usize) -> Self {
        VirtAddr::from_usize(value)
    }
}
