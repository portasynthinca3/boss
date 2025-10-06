//! Symmetric Multiprocessing interface

use crate::target::current::{
    interrupt::*,
    memmgr::*,
    acpi::*,
    device::{
        wall_clock::*,
    },
};

use alloc::{
    alloc::Allocator,
    sync::Arc,
    boxed::Box,
    vec::Vec,
};

use spin::Mutex;

use crate::target::current::smp as concrete;

// =============
// Shared memory
// =============

pub trait SharedAlloc: Allocator { }

/// Marker trait for items that only reference items in the shared space
/// (in memory managed by [SharedAlloc])
pub unsafe auto trait Share { }

unsafe impl Share for u8 { }
unsafe impl Share for u16 { }
unsafe impl Share for u32 { }
unsafe impl Share for u64 { }
unsafe impl Share for u128 { }
unsafe impl Share for i8 { }
unsafe impl Share for i16 { }
unsafe impl Share for i32 { }
unsafe impl Share for i64 { }
unsafe impl Share for i128 { }

impl<T> !Share for &T { }
impl<T> !Share for &mut T { }

unsafe impl<T: Share> Share for Box<T, concrete::SharedAlloc> { }
unsafe impl<T: Share> Share for Arc<T, concrete::SharedAlloc> { }
unsafe impl<T: Share> Share for Vec<T, concrete::SharedAlloc> { }

// ==============
// SMP management
// ==============

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Error {
    NotBootstrap,
    Timeout,
    MessengerExists,
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CpuId(pub(crate) u32);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Destination {
    Unicast(CpuId),
    BroadcastWithSelf,
    BroadcastWithoutSelf,
}

pub trait SmpManager<'m, 'a: 'm> {
    unsafe fn new(
        intr: &'m IntrMgr,
        acpi: Option<&Acpi<'a>>,
        addr_space: &mut AddrSpace<'_>,
        phys_alloc: &Mutex<PhysAlloc>,
        clock: &'m WallClock,
    ) -> Self;

    fn this_cpu(&self) -> CpuId;

    fn cpus(&self) -> impl Iterator<Item = CpuId>;

    fn initialize_all_aps(
        &mut self,
        addr_space: &AddrSpace<'_>,
        alloc: &Mutex<PhysAlloc>,
        main: extern "C" fn() -> !,
    ) -> Result<()>;

    unsafe fn messenger<T: Share>(&mut self) -> Result<concrete::SmpMessenger<T>>;
}

pub trait SmpMessenger<T: Share> {
    fn send(&mut self, dest: Destination, message: Arc<T, concrete::SharedAlloc>) -> Result<()>;

    fn receive(&mut self, callback: impl FnMut(Arc<T, concrete::SharedAlloc>)) -> Result<()>;
}
