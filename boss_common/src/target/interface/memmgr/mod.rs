//! Abstract memory manager definitions
//! 
//! There are three layers to memory management:
//!   - Physical memory. It's addresses that the CPU presents to its memory bus,
//!     where actual devices are connected; including RAM, of course. Our
//!     programs don't necessarily refer to devices using their physical
//!     addresses.
//!   - Virtual memory. It's addresses that our program presents to the CPU. The
//!     CPU is asked with translating these addresses into physical ones, and
//!     our platform abstraction is tasked with setting up the translation
//!     tables to guide the CPU.
//!   - Heap memory. The two previous layers dealt with units of memory called
//!     pages, which are usually on the scale of kilobytes. Out program needs
//!     units smaller than that, which are allocated by the heap memory
//!     allocator.

use bincode::{Encode, Decode};
use core::alloc::GlobalAlloc;
use core::fmt::{self, Formatter, Debug};
use core::marker::PhantomData;
use core::ops::Deref;
use core::{ops::RangeInclusive, mem::variant_count, ptr::{self, NonNull}};
use core::alloc::{Allocator, Layout};
use alloc::borrow::ToOwned;
use spin::Mutex;

use crate::util::byte_size::ByteSize;
use crate::target::current::{
    memmgr as concrete,
    smp::*,
};

pub mod phys;
pub mod heap;

pub use heap::{
    LinkedListAllocator,
    initialize_global_alloc,
};

// ===========
// Foundations
// ===========

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    /// Failed to convert one type of address to another
    MappingError,
    /// Malformed address
    MalformedAddress,
    /// Address belongs to unknown range
    UnknownRange,
    /// Not enough memory to perform the allocation
    OutOfMemory,
    /// Object failed to deallocate because it does not belong to the allocator
    UnrecognizedDealloc,
    /// It was unclear what was the supplied address range meant
    InvalidAddrRange,
    /// An improperly aligned address was supplied
    BadAlignment,
    /// Extending a physical memory range would overwrite data
    ExtensionImpossible,
    /// The physical memory range already contains the `all_ranges` slice
    AlreadyContainsAllRanges,
    /// The physical memory range has data where the `all_ranges` slice is supposed to go
    AllRangesCreationImpossible,
    /// The requested address is not handled by that particular address space handle
    AddressNotHandledBySpace,
    /// Reached maximum number of ranges that an address space can handle
    TooManyRanges,

    UnknownPlatformSpecific,
}

pub type Result<T> = core::result::Result<T, Error>;

/// Physical memory address, i.e. the one that the CPU puts on the memory bus
pub trait PhysAddr
where
    Self: TryFrom<concrete::VirtAddr, Error = Error>,
    Self: Copy + Eq + Ord,
    Self: Encode + Decode<()>,
    concrete::VirtAddr: TryFrom<Self, Error = Error>
{
    /// Creates a [PhysAddr] from a number, checking if the provided number is
    /// a valid address on the current platform.
    fn from_usize(n: usize) -> Result<Self>;

    /// Creates a [PhysAddr] from a number without checking if the provided
    /// number is a valid physical address on the current platform.
    /// 
    /// # Safety
    /// You should not be trying to access an invalid address.
    unsafe fn from_usize_unchecked(n: usize) -> Self;

    /// Converts the [PhysAddr] to a number.
    fn to_usize(self) -> usize;

    /// Clears the page pointed to by the [PhysAddr].
    /// 
    /// # Safety
    /// There must not be any references, shared or exclusive, to any content
    /// within the page
    unsafe fn clear_page(&self) -> Result<()> {
        if self.to_usize() % concrete::MemoryParameters::PAGE_SIZE != 0 { return Err(Error::BadAlignment); }
        let virt: concrete::VirtAddr = (*self).try_into()?;
        virt.clear_page()
    }
}

/// Virtual memory address, i.e. the one that a program presents to the CPU
pub trait VirtAddr
where
    Self: TryFrom<concrete::PhysAddr, Error = Error>,
    Self: Copy + Eq + Ord,
    concrete::PhysAddr: TryFrom<Self, Error = Error>
{
    /// Creates a [VirtAddr] from a number, checking if the provided number is
    /// a valid address on the current platform.
    fn from_usize(n: usize) -> Result<Self>;

    /// Creates a [VirtAddr] from a number without checking if the provided
    /// number is a valid virtual address on the current platform.
    /// 
    /// # Safety
    /// You should not be trying to access an invalid address.
    unsafe fn from_usize_unchecked(n: usize) -> Self;

    /// Converts the [VirtAddr] to a number.
    fn to_usize(self) -> usize;

    /// Clears the page pointed to by the [VirtAddr].
    /// 
    /// # Safety
    /// There must not be any references, shared or exclusive, to any content
    /// within the page
    unsafe fn clear_page(self) -> Result<()> {
        let num = self.to_usize();
        if num % concrete::MemoryParameters::PAGE_SIZE != 0 { return Err(Error::BadAlignment); }
        let ptr = num as *mut u8;
        ptr.write_bytes(0, concrete::MemoryParameters::PAGE_SIZE);
        Ok(())
    }

    fn from_ptr<T>(ptr: *const T) -> Result<Self> {
        Self::from_usize(ptr as usize)
    }
    fn from_mut_ptr<T>(ptr: *mut T) -> Result<Self> {
        Self::from_usize(ptr as usize)
    }
    fn from_ref<T>(r: &T) -> Result<Self> {
        Self::from_ptr(r as *const T)
    }
    fn from_mut_ref<T>(r: &mut T) -> Result<Self> {
        Self::from_ptr(r as *mut T)
    }
    fn to_ptr<T>(self) -> *const T {
        self.to_usize() as *const T
    }
    fn to_mut_ptr<T>(self) -> *mut T {
        self.to_usize() as *mut T
    }
}

impl Debug for concrete::PhysAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        #[cfg(target_pointer_width = "64")]
        return write!(f, "\x1b[33mp\x1b[0m{:#018x}", self.to_usize());
        #[cfg(target_pointer_width = "32")]
        return write!(f, "\x1b[33mp\x1b[0m{:#010x}", self.to_usize());
    }
}

impl Debug for concrete::VirtAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        #[cfg(target_pointer_width = "64")]
        return write!(f, "\x1b[36mv\x1b[0m{:#018x}", self.to_usize());
        #[cfg(target_pointer_width = "32")]
        return write!(f, "\x1b[36mv\x1b[0m{:#010x}", self.to_usize());
    }
}

/// Memory region enumeration
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Region {
    // All of the following items reside in non-shared virtual memory, i.e.
    // their mapping will change from one CPU to another:

    /// Natively implemented Erlang functions (unprivileged code)
    /// or identity-mapped physical memory (during early boot)
    NifOrIdentity,
    /// Data passed from bootloader to emulator
    EmuParams,
    /// CPU-local context
    LocalContext,
    /// Emulator stack
    LocalStack,
    /// Emulator heap
    LocalHeap,

    // The following items have the same mapping across CPUs:

    /// Decompressed and loaded emulator image
    EmulatorImage,
    /// Decompressed base image
    BaseImage,
    /// Linearly mapped physical memory
    LinearPhysical,
    /// Heap that's shared across CPUs
    SharedHeap,
}

/// Layout of the virtual address space
pub trait MemoryParameters {
    const PAGE_SIZE: usize;
    const EMULATOR_STK_PAGES: usize;
    const RANGES: [(Region, RangeInclusive<concrete::VirtAddr>); variant_count::<Region>()];

    /// Determines the region that a virtual address belongs to
    fn determine_region(addr: concrete::VirtAddr) -> Result<Region> {
        Self::RANGES
            .iter()
            .find_map(|(region, range)| if range.contains(&addr) { Some(*region) } else { None })
            .ok_or(Error::UnknownRange)
    }

    /// Returns the address range of a region
    fn range(region: Region) -> RangeInclusive<concrete::VirtAddr> {
        Self::RANGES
            .iter()
            .find_map(|(reg, range)| if region == *reg { Some(range.to_owned()) } else { None })
            .unwrap()
    }
}

// ===============
// Physical memory
// ===============

/// Usability of the physical memory range
/// 
/// Each of the next steps requires all the previous steps
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Encode, Decode)]
pub enum PhysMemUsability {
    /// Usable right from the start
    Always,
    /// Usable after control has been transferred from the bootloader to the
    /// emulator
    AfterBootloaderExit,
    /// Usable after firmware has been deinitialized
    AfterFirmwareDeinit,
    /// Usable after the ACPI tables have been read and parsed
    AfterAcpiReclaim,
}

/// Range of usable physical memory
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Encode, Decode)]
pub struct PhysMemRange {
    pub start: concrete::PhysAddr,
    pub page_count: usize,
    pub usability: PhysMemUsability,
}

/// Physical memory allocator statistics
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[derive(Encode, Decode)]
pub struct PhysMemStats {
    /// Total memory available to the allocator
    pub total: ByteSize,
    /// Memory used internally by the allocator
    pub internal: ByteSize,
    /// Total memory currently allocated and not used internally
    pub allocated: ByteSize,

    /// Total bytes allocated over the entire lifetime
    pub cumulative_allocated: ByteSize,
    /// Total bytes deallocated over the entire lifetime
    pub cumulative_deallocated: ByteSize,
}

/// Allocates pages of physical memory
pub trait PhysAlloc: Sized {
    /// Creates a new physical memory allocator, given the ranges of physical
    /// memory that it can operate in.
    /// 
    /// # Safety
    /// The supplied memory ranges must not be referenced by anything else.
    unsafe fn new(ranges: impl Iterator<Item = PhysMemRange>) -> Result<Self>;

    /// Allocates pages of physical memory that may not necessarily be
    /// contiguous.
    fn allocate(&mut self) -> Result<impl Iterator<Item = concrete::PhysAddr>>;

    /// Allocates contiguous pages of physical memory.
    fn allocate_contiguous(&mut self, pages: usize) -> Result<concrete::PhysAddr>;

    /// Allocates contiguous pages of physical memory such that the starting
    /// address satisfies the provided `selector` closure.
    fn allocate_select(&mut self, pages: usize, selector: impl Clone + Fn(concrete::PhysAddr) -> bool) -> Result<concrete::PhysAddr>;

    /// Deallocates pages of physical memory.
    fn deallocate(&mut self, pages: impl Iterator<Item = concrete::PhysAddr>) -> Result<()>;

    /// Adds new pages to the physical memory allocator.
    /// 
    /// # Safety
    /// The supplied memory ranges must not be referenced by anything else.
    unsafe fn add_ranges(&mut self, ranges: impl Iterator<Item = PhysMemRange>);

    /// Returns memory statistics
    fn stats(&self) -> PhysMemStats;
}

// ==============
// Virtual memory
// ==============

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Caching {
    Uncacheable,
}

/// Memory access attributes
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Access {
    /// 1 = Writes allowed, 0 = Writes disallowed
    pub write: bool,
    /// 1 = User access allowed, 0 = Supervisor access only
    pub user: bool,
    /// 1 = Execution allowed, 0 = Execution disallowed
    pub execute: bool,
    /// Cache mode
    pub caching: Caching,
}

impl Default for Access {
    fn default() -> Self {
        Self { write: true, user: false, execute: true, caching: Caching::Uncacheable }
    }
}

/// Virtual address space. This wrapper may handle the entire address space, or
/// just a portion of it.
pub trait AddrSpace<'alloc>: Debug + Sized {
    const MAX_RANGES: usize;

    /// Gets the entire address space that the current program is running in on
    /// the current logical CPU.
    /// 
    /// # Safety
    /// This function must not be called while there are any other active (i.e.
    /// not yet [drop]ped) [`AddrSpace`] wrappers for the current address space.
    unsafe fn get_current(alloc: &'alloc Mutex<concrete::PhysAlloc>) -> Self;

    /// Instructs the current logical CPU to use the mapping provided by this
    /// [AddrSpace] for address translation.
    /// 
    /// # Safety
    /// Inherently, changing the virtual address space mapping is very
    /// dangerous. Please don't do any stupid things with the mapping.
    unsafe fn set_as_current(&mut self);

    /// Checks whether this [AddrSpace] is presently used by the current logical
    /// CPU for address translation.
    fn is_current(&self) -> bool;

    /// Creates a new virtual address space.
    fn new(alloc: &'alloc Mutex<concrete::PhysAlloc>) -> Result<Self>;

    /// Gets the address ranges that this [AddrSpace] handles
    fn ranges(&self) -> impl Iterator<Item = RangeInclusive<concrete::VirtAddr>>;

    /// Determines if this [AddrSpace] fully handles the supplied address range
    fn does_handle_range(&self, range: &RangeInclusive<concrete::VirtAddr>) -> bool;

    /// Splits out a portion of the address space from the current one. After
    /// this operation, the current wrapper will no longer be able to manage the
    /// requested option, but the returned wrapper will.
    fn take_portion(&mut self, range: &RangeInclusive<concrete::VirtAddr>) -> Result<Self>;

    /// Merges a portion of the address space with the current one.
    fn merge_portion(&mut self, other: Self) -> Result<()>;

    /// Creates a guard that allows modifications to the [AddrSpace].
    fn modify(&mut self) -> impl AddrSpaceGuard<'alloc>;

    fn copy_into(&self, other: &mut Self, range: &RangeInclusive<concrete::VirtAddr>) -> Result<()>;
}

/// What address to return for virtual allocations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocReturn {
    Start,
    End,
}

/// Allows modifications to an address space
pub trait AddrSpaceGuard<'alloc>: Deref<Target = concrete::AddrSpace<'alloc>> {
    /// Allocates `count` continuous pages of virtual memory, starting at
    /// `start`
    fn allocate_range(
        &mut self,
        start: concrete::VirtAddr,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<concrete::VirtAddr>;

    /// Allocates `count` continuous pages of virtual memory, starting at an
    /// arbitrary location in the virtual address space
    fn allocate_anywhere(
        &mut self,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<concrete::VirtAddr>;

    /// Deallocates the provided range of virtual memory
    fn deallocate_range(&mut self, range: RangeInclusive<concrete::VirtAddr>) -> Result<()>;

    /// Maps `count` continuous pages of virtual memory, mapping `start_virt` to
    /// `start_phys`
    fn map_range(
        &mut self,
        start_virt: concrete::VirtAddr,
        start_phys: concrete::PhysAddr,
        count: usize,
        access: Access,
        ret: AllocReturn
    ) -> Result<concrete::VirtAddr>;

    /// Unmaps the provided range of virtual memory
    fn unmap_range(&mut self, range: RangeInclusive<concrete::VirtAddr>) -> Result<()>;
}

// ===========
// Heap memory
// ===========

/// A heap allocator that becomes available halfway into the program and is
/// stored in an `SmpContext`
pub struct LateLocalAlloc<T: Allocator + 'static>(PhantomData<T>);

struct LocalAllocRecord<T: Allocator + 'static>(T);

impl<T: Allocator> LateLocalAlloc<T> {
    pub(crate) const fn new() -> Self {
        Self(PhantomData)
    }

    pub(crate) fn initialize(alloc: T) {
        SmpContext::get().create_record(LocalAllocRecord(alloc));
    }

    fn get() -> &'static LocalAllocRecord<T> {
        SmpContext::get().find_record().unwrap()
    }
}

unsafe impl<T: Allocator> GlobalAlloc for LateLocalAlloc<T> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let allocator = &Self::get().0;
        match allocator.allocate(layout) {
            Err(_) => ptr::null_mut::<u8>(),
            Ok(p) => p.as_mut_ptr(),
        }
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let allocator = &Self::get().0;
        if let Some(ptr) = NonNull::new(ptr) {
            allocator.deallocate(ptr, layout);
        }
    }
}

// ==========
// Test suite
// ==========

/// Memory manager test suite
pub mod tests {
    use crate::target::current::{
        device::wall_clock::*,
        memmgr::*,
    };
    use crate::{tests::*, util::byte_size::ByteSize};
    use alloc::{
        borrow::ToOwned,
        vec::Vec,
        boxed::Box,
    };
    use itertools::Itertools;

    const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

    /// Basic tests for the physical allocator
    fn simple_alloc_dealloc(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 1000;
        const ROUND_SIZE: usize = 8;

        let mut alloc = env.phys_alloc.lock();
        let before_stats = alloc.stats();

        for _round in 0..ROUNDS {
            let before_round_stats = alloc.stats();
            let pages = alloc.allocate().unwrap().take(ROUND_SIZE).collect_array::<ROUND_SIZE>().unwrap();

            for (i, page) in pages.iter().enumerate() {
                const PAGE_OFFSET_BITMASK: usize = PAGE_SIZE - 1;
                assert!(page.to_usize() & PAGE_OFFSET_BITMASK == 0);
                assert!(!pages[0..i].contains(page));
            }

            let after_round_stats = alloc.stats();
            let expected_diff = ByteSize(ROUND_SIZE * PAGE_SIZE);
            assert!(after_round_stats.allocated - before_round_stats.allocated == expected_diff);

            alloc.deallocate(pages.iter().map(|p| p.to_owned())).unwrap();
        }

        let after_stats = alloc.stats();
        let expected_diff = ByteSize(ROUNDS * ROUND_SIZE * PAGE_SIZE);
        assert!(after_stats.cumulative_allocated - before_stats.cumulative_allocated == expected_diff);
        assert!(after_stats.cumulative_deallocated - before_stats.cumulative_deallocated == expected_diff);
        assert!(after_stats.allocated == before_stats.allocated);
        assert!(after_stats.total == before_stats.total);
    }

    /// Deallocates pages in a different order than the allocation
    fn unordered_dealloc(env: &mut TestEnv<'_>) {
        let mut alloc = env.phys_alloc.lock();

        let mut deferred_pages = [PhysAddr::from_usize(0).unwrap(); 16];

        for deferred_page in &mut deferred_pages {
            let [first, second] = alloc.allocate().unwrap().take(2).collect_array().unwrap();
            alloc.deallocate(core::iter::once(first)).unwrap();
            *deferred_page = second;
        }

        alloc.deallocate(deferred_pages.iter().map(|p| p.to_owned())).unwrap();
    }

    /// Measures the speed of page allocation
    fn alloc_speed(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 100;
        const ROUND_SIZE: usize = 128;
        let mut alloc = env.phys_alloc.lock();

        let (mut total_alloc_ns, mut total_dealloc_ns) = (0, 0);
        for _round in 0..ROUNDS {
            let (pages, delta) = env.wall_clock.delta_time(|| {
                alloc.allocate().unwrap().take(ROUND_SIZE).collect_array::<ROUND_SIZE>().unwrap()
            });
            total_alloc_ns += delta.ns();

            let (_, delta) = env.wall_clock.delta_time(|| {
                alloc.deallocate(pages.iter().map(|p| p.to_owned())).unwrap();
            });
            total_dealloc_ns += delta.ns();
        }

        for (title, ns) in [("alloc", total_alloc_ns), ("dealloc", total_dealloc_ns)] {
            let ns_per_page = ns as f64 / (ROUNDS * ROUND_SIZE) as f64;
            let pages_per_s = 1_000_000_000 * ROUNDS * ROUND_SIZE / (ns as usize);
            let bytes_per_s = ByteSize(pages_per_s * PAGE_SIZE);
            log::info!("      {title} speed: {ns_per_page:.3} ns/page, {pages_per_s} pages/s, {bytes_per_s}/s");
        }
    }

    pub static PHYS_ALLOC_TESTS: TestSuite = &[
        ("simple_alloc_dealloc", simple_alloc_dealloc),
        ("unordered_dealloc", unordered_dealloc),
        ("alloc_speed", alloc_speed),
    ];



    fn simple_virt_alloc_dealloc(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 1000;
        const WINDOW_SIZE: usize = 128;

        for round in 0..ROUNDS {
            let base = VirtAddr::from_usize(env.scratchpad_area.start().to_usize() + (round * PAGE_SIZE)).unwrap();
            let ret = env.addr_space
                .modify()
                .allocate_range(base, WINDOW_SIZE, Default::default(), AllocReturn::Start)
                .unwrap();

            assert!(base == ret);
            let ptr = ret.to_mut_ptr::<u8>();
            unsafe { ptr.write_bytes(0x55, WINDOW_SIZE * PAGE_SIZE) }; // should crash here if the mapping went wrong

            let range = base ..= VirtAddr::from_usize(base.to_usize() + (WINDOW_SIZE * PAGE_SIZE) - 1).unwrap();
            env.addr_space.modify().deallocate_range(range).unwrap();
        }
    }

    fn simple_virt_map_unmap(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 1000;
        const WINDOW_SIZE: usize = 128;

        for round in 0..ROUNDS {
            let base = VirtAddr::from_usize(env.scratchpad_area.start().to_usize() + (round * PAGE_SIZE)).unwrap();
            let ret = env.addr_space
                .modify()
                .map_range(base, PhysAddr::from_usize(0).unwrap(), WINDOW_SIZE, Default::default(), AllocReturn::Start)
                .unwrap();

            assert!(base == ret);

            let range = base ..= VirtAddr::from_usize(base.to_usize() + (WINDOW_SIZE * PAGE_SIZE) - 1).unwrap();
            env.addr_space.modify().unmap_range(range).unwrap();
        }
    }

    fn virt_map_speed(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 100;
        const WINDOW_SIZE: usize = 128;

        let (mut total_map_ns, mut total_unmap_ns) = (0, 0);
        for round in 0..ROUNDS {
            let base = VirtAddr::from_usize(env.scratchpad_area.start().to_usize() + (round * PAGE_SIZE)).unwrap();
            let (ret, delta) = env.wall_clock.delta_time(|| {
                env.addr_space
                    .modify()
                    .map_range(base, PhysAddr::from_usize(0).unwrap(), WINDOW_SIZE, Default::default(), AllocReturn::Start)
                    .unwrap()
            });
            assert!(base == ret);
            total_map_ns += delta.ns();

            let range = base ..= VirtAddr::from_usize(base.to_usize() + (WINDOW_SIZE * PAGE_SIZE) - 1).unwrap();
            let (_, delta) = env.wall_clock.delta_time(|| {
                env.addr_space.modify().unmap_range(range).unwrap();
            });
            total_unmap_ns += delta.ns();
        }

        for (title, ns) in [("map", total_map_ns), ("unmap", total_unmap_ns)] {
            let ns_per_page = ns as f64 / (ROUNDS * WINDOW_SIZE) as f64;
            let pages_per_s = 1_000_000_000 * ROUNDS * WINDOW_SIZE / (ns as usize);
            let bytes_per_s = ByteSize(pages_per_s * PAGE_SIZE);
            log::info!("      {title} speed: {ns_per_page:.3} ns/page, {pages_per_s} pages/s, {bytes_per_s}/s");
        }
    }

    pub static VIRT_MEM_TESTS: TestSuite = &[
        ("simple_virt_alloc_dealloc", simple_virt_alloc_dealloc),
        ("simple_virt_map_unmap", simple_virt_map_unmap),
        ("virt_map_speed", virt_map_speed),
    ];



    fn with_ll_alloc(env: &mut TestEnv<'_>, callback: impl FnOnce(&LinkedListAllocator<'_>)) {
        let subspace = env.addr_space.take_portion(&env.scratchpad_area).unwrap();
        let alloc = LinkedListAllocator::new(subspace).unwrap();
        callback(&alloc);
        let subspace = alloc.destroy();
        env.addr_space.merge_portion(subspace).unwrap();
    }

    fn ll_simple_box(env: &mut TestEnv<'_>) {
        with_ll_alloc(env, |alloc| {
            let my_box = Box::new_in([1, 2, 3], alloc);
            assert!(*my_box == [1, 2, 3]);
        });
    }

    fn ll_simple_vec(env: &mut TestEnv<'_>) {
        with_ll_alloc(env, |alloc| {
            let mut my_vec = Vec::new_in(alloc);
            my_vec.push(1);
            my_vec.push(2);
            my_vec.push(3);
            assert!(my_vec[0..3] == [1, 2, 3]);
        });
    }

    fn ll_stress(env: &mut TestEnv<'_>) {
        with_ll_alloc(env, |alloc| {
            let boxes: [_; 128] = core::array::from_fn(|_| Box::new_in([0; 666], alloc));
            for b in boxes {
                assert!(*b == [0; 666]);
            }
        });
    }

    fn ll_speed(env: &mut TestEnv<'_>) {
        const ROUNDS: usize = 100;
        const BLOCK_SIZE: usize = 256;

        with_ll_alloc(env, |alloc| {
            let (mut total_alloc_ns, mut total_free_ns) = (0, 0);
            for _ in 0..ROUNDS {
                let (my_box, delta) = env.wall_clock.delta_time(|| {
                    Box::new_in([0u8; BLOCK_SIZE], alloc)
                });
                total_alloc_ns += delta.ns();

                let (_, delta) = env.wall_clock.delta_time(move || {
                    drop(my_box);
                });
                total_free_ns += delta.ns();
            }

            for (title, ns) in [("alloc", total_alloc_ns), ("free", total_free_ns)] {
                let ns_per_op = ns as f64 / ROUNDS as f64;
                let ops_per_s = 1_000_000_000 * ROUNDS / (ns as usize);
                let bytes_per_s = ByteSize(ops_per_s * BLOCK_SIZE);
                log::info!("      {title} speed: {ns_per_op:.3} ns/op, {ops_per_s} op/s, {bytes_per_s}/s");
            }
        });
    }

    pub static HEAP_TESTS: TestSuite = &[
        ("ll_simple_box", ll_simple_box),
        ("ll_simple_vec", ll_simple_vec),
        ("ll_stress", ll_stress),
        ("ll_speed", ll_speed),
    ];
}
