use bincode::{Encode, Decode};

use core::mem::variant_count;
use core::ops::RangeInclusive;
use crate::target::runtime_cfg::{self, CfgFlags};
use crate::target::interface::memmgr as interface;

pub use interface::{
    Error,
    Region,
    PhysMemUsability,
    PhysMemRange,
    PhysMemStats,
    Caching,
    Access,
    AllocReturn,
    LinkedListAllocator,
    LateLocalAlloc,
    initialize_global_alloc,
    PhysAlloc as IfPhysAlloc,
    AddrSpace as IfAddrSpace,
    AddrSpaceGuard as IfAddressSpaceGuard,
    VirtAddr as IfVirtAddr,
    PhysAddr as IfPhysAddr,
    MemoryParameters as IfMemoryParameters,
};
pub use interface::phys::GenericPhysAlloc as PhysAlloc;
pub use virt::*;

pub mod virt;

/// Represents a physical address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Encode, Decode)]
#[repr(transparent)]
pub struct PhysAddr(usize);
impl interface::PhysAddr for PhysAddr {
    fn from_usize(n: usize) -> interface::Result<Self> {
        Ok(PhysAddr(n))
    }
    unsafe fn from_usize_unchecked(n: usize) -> Self {
        PhysAddr(n)
    }
    fn to_usize(self) -> usize {
        self.0
    }
}

/// Represents a virtual address
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Encode, Decode)]
#[repr(transparent)]
pub struct VirtAddr(usize);
impl interface::VirtAddr for VirtAddr {
    fn from_usize(n: usize) -> interface::Result<Self> {
        // in x86 with 4-level paging, the top 16 bits must be of the same value as the 47th bit
        let upper_bits = if (n >> 47) & 1 == 1 { 0xffff } else { 0x0000 };
        Ok(VirtAddr(n | (upper_bits << 48)))
    }
    unsafe fn from_usize_unchecked(n: usize) -> Self {
        VirtAddr(n)
    }
    fn to_usize(self) -> usize {
        self.0
    }
}

pub struct MemoryParameters { }

use interface::Region::*;

impl interface::MemoryParameters for MemoryParameters {
    const PAGE_SIZE: usize = 4096;
    const EMULATOR_STK_PAGES: usize = 16;

    const RANGES: [(interface::Region, RangeInclusive<VirtAddr>); variant_count::<interface::Region>()] = [
        (NifOrIdentity,  VirtAddr(0x0000_0000_0000_0000) ..= VirtAddr(0x0000_7fff_ffff_ffff)), // 128 TiB
        (LinearPhysical, VirtAddr(0xffff_8000_0000_0000) ..= VirtAddr(0xffff_bfff_ffff_ffff)), // 64 TiB
        (EmulatorImage,  VirtAddr(0xffff_c000_0000_0000) ..= VirtAddr(0xffff_cfff_fffe_ffff)), // 16 TiB - 64 KiB
        (LocalContext,   VirtAddr(0xffff_cfff_ffff_0000) ..= VirtAddr(0xffff_cfff_ffff_efff)), // 60 KiB
        (EmuParams,      VirtAddr(0xffff_cfff_ffff_f000) ..= VirtAddr(0xffff_cfff_ffff_ffff)), // 4 KiB
        (BaseImage,      VirtAddr(0xffff_d000_0000_0000) ..= VirtAddr(0xffff_dfff_ffff_ffff)), // 16 TiB
        (LocalStack,     VirtAddr(0xffff_e000_0000_0000) ..= VirtAddr(0xffff_e000_000f_ffff)), // 1 MiB
        (LocalHeap,      VirtAddr(0xffff_e000_0010_0000) ..= VirtAddr(0xffff_efff_ffff_ffff)), // 16 TiB - 1 MiB
        (SharedHeap,     VirtAddr(0xffff_f000_0000_0000) ..= VirtAddr(0xffff_ffff_ffff_ffff)), // 16 TiB
    ];
}

impl TryFrom<VirtAddr> for PhysAddr {
    type Error = interface::Error;
    fn try_from(value: VirtAddr) -> Result<Self, Self::Error> {
        let in_upper_half = runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf);
        let region = MemoryParameters::determine_region(value).unwrap();
        let region_start = MemoryParameters::range(region).start().to_usize();
        let n = value.to_usize();
        match region {
            interface::Region::LinearPhysical => Self::from_usize(n - region_start),
            interface::Region::NifOrIdentity if !in_upper_half => Self::from_usize(n),
            _ => Err(interface::Error::MappingError)
        }
    }
}

impl TryFrom<PhysAddr> for VirtAddr {
    type Error = interface::Error;
    fn try_from(value: PhysAddr) -> Result<Self, Self::Error> {
        let region = if runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf) {
            interface::Region::LinearPhysical
        } else {
            interface::Region::NifOrIdentity
        };
        let base = MemoryParameters::range(region).start().to_usize();
        VirtAddr::from_usize(base + value.to_usize())
    }
}
