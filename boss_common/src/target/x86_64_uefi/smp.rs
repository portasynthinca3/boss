//! Symmetric Multiprocessing support for x86-64
//! 
//! Based on Chapters 10, 12 of Intel SDM vol. 3A

#![allow(unused)]

pub use crate::target::interface::smp::{
    CpuId,
    Destination,
    Error,
    Result,
    Share,
    SharedAlloc as IfSharedAlloc,
    SmpManager as IfSmpManager,
    SmpMessenger as IfSmpMessenger,
};

use crate::target::current::{
    memmgr::*,
    acpi::*,
    interrupt::*,
    device::{
        wall_clock::*,
    },
    ll::{
        apic::*,
    },
};

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

use alloc::alloc::{Allocator, AllocError};
use core::{
    arch::global_asm,
    sync::atomic::{Ordering, AtomicU8}
};

use spin::Mutex;

// AP startup code
global_asm!(include_str!("ap_boot.S"));
unsafe extern "C" {
    fn smp_ap_boot_start() -> !;
    fn smp_ap_status_block() -> !;
    fn smp_ap_boot_end() -> !;
}

pub struct SharedAlloc {

}

impl IfSharedAlloc for SharedAlloc {

}

unsafe impl Allocator for SharedAlloc {
    fn allocate(&self, layout: core::alloc::Layout) -> core::result::Result<core::ptr::NonNull<[u8]>, AllocError> {
        todo!()
    }

    unsafe fn deallocate(&self, ptr: core::ptr::NonNull<u8>, layout: core::alloc::Layout) {
        todo!()
    }
}

#[repr(C)]
struct BootStatusBlock {
    real_mode_up: AtomicU8,
}

pub struct SmpManager<'m, 'a: 'm> {
    boot_code: &'m [u8],
    boot_status_block: *mut BootStatusBlock,
    boot_vector: u8,
    lapic: &'m Lapic,
    clock: &'m WallClock,
    this: CpuId,
    madt: &'a Madt,
}

const LOW_MEM_TOP: usize = 0x100 * PAGE_SIZE;

impl<'m, 'a: 'm> IfSmpManager<'m, 'a> for SmpManager<'m, 'a> {
    unsafe fn new(
        intr: &'m IntrMgr,
        acpi: Option<&Acpi<'a>>,
        addr_space: &mut AddrSpace<'_>,
        phys_alloc: &Mutex<PhysAlloc>,
        clock: &'m WallClock,
    ) -> Self {
        let boot_start: *const u8 = smp_ap_boot_start as _;
        let boot_status_block: *const u8 = smp_ap_status_block as _;
        let boot_end: *const u8 = smp_ap_boot_end as _;
        let boot_size = unsafe { boot_end.offset_from_unsigned(boot_start) };
        let boot_pages = boot_size.div_ceil(PAGE_SIZE);
        let boot_code: &'static _ = unsafe { core::slice::from_raw_parts(boot_start, boot_size) };

        let boot_in_low_mem = phys_alloc.lock().allocate_select(boot_pages, |page| page.to_usize() <= LOW_MEM_TOP).unwrap();
        let boot_vector = (boot_in_low_mem.to_usize() / PAGE_SIZE) as _;
        let boot_in_low_mem: VirtAddr = boot_in_low_mem.try_into().unwrap();
        let boot_in_low_mem = unsafe { core::slice::from_raw_parts_mut(boot_in_low_mem.to_mut_ptr(), boot_size) };
        boot_in_low_mem.copy_from_slice(boot_code);
        log::trace!("boot vector: {boot_vector:#04x}");

        let lapic = intr.get_lapic();
        let id = lapic.id();

        let madt = acpi.unwrap().find_table().unwrap();

        Self {
            boot_code: boot_in_low_mem,
            boot_status_block: boot_in_low_mem.as_ptr().add(unsafe { boot_status_block.offset_from_unsigned(boot_start) }) as _,
            boot_vector,
            lapic,
            clock,
            this: CpuId(id.0),
            madt
        }
    }

    fn this_cpu(&self) -> CpuId {
        self.this
    }

    fn cpus(&self) -> impl Iterator<Item = CpuId> {
        self.madt
            .iter()
            .filter_map(|entry| if let MadtDescriptor::LocalApic { id, .. } = entry {
                Some(CpuId(id.0))
            } else {
                None
            })
    }

    fn initialize_all_aps(&mut self) -> Result<()> {
        let ap_count = self.cpus().count() - 1;
        let status = unsafe { &*self.boot_status_block };

        self.lapic.send_ipi(ApicIpi::Init, ApicIpiDest::AllExcludingSelf);
        self.clock.delay(Duration::from_ms(10));

        let mut up: usize = 0;
        const ATTEMPTS: usize = 2;
        for attempt in 0..ATTEMPTS {
            self.lapic.send_ipi(ApicIpi::Start(self.boot_vector), ApicIpiDest::AllExcludingSelf);
            self.clock.delay(Duration::from_us(200));

            up = status.real_mode_up.load(Ordering::Relaxed) as _;
            log::trace!("SIPI {}/{ATTEMPTS}: {up}/{ap_count} APs up", attempt + 1);
            if up == ap_count { break };
        }

        if up != ap_count {
            log::error!("only {up}/{ap_count} APs up");
        }

        Ok(())
    }

    unsafe fn messenger<T: Share>(&mut self) -> Result<self::SmpMessenger<T>> {
        todo!()
    }
}

pub struct SmpMessenger<T: Share> {
    _a: core::marker::PhantomData<T>
}

impl<T: Share> IfSmpMessenger<T> for SmpMessenger<T> {
    fn send(&mut self, dest: Destination, message: alloc::sync::Arc<T, self::SharedAlloc>) -> Result<()> {
        todo!()
    }

    fn receive(&mut self, callback: impl FnMut(alloc::sync::Arc<T, self::SharedAlloc>)) -> Result<()> {
        todo!()
    }
}
