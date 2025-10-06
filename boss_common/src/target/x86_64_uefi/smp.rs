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
    sync::atomic::{Ordering, AtomicU8, AtomicU32, AtomicU64},
};

use spin::Mutex;

static AP_BOOT_CODE: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/../.build/ap_boot.bin"));

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
    long_mode_up: AtomicU8,
    _setup_sem: AtomicU8,
    setup_command: AtomicU8,

    addr_space: AtomicU32,
    stack_top: AtomicU64,
    main: AtomicU64,
}

impl BootStatusBlock {
    fn configure_arguments(&self, space: AddrSpace<'_>, stack_top: VirtAddr, main: extern "C" fn() -> !) {
        let cr3 = space.get_cr3();
        assert!(cr3 < 0xffff_ffff);
        self.addr_space.store(space.get_cr3() as _, Ordering::Relaxed);
        self.stack_top.store(stack_top.to_usize() as _, Ordering::Relaxed);
        self.main.store(main as *const () as usize as _, Ordering::Relaxed);
    }

    fn issue_setup_command(&self) {
        loop {
            let result = self.setup_command.compare_exchange(0, 1, Ordering::Relaxed, Ordering::Relaxed);
            if result.is_ok() { break };
        }
    }

    fn wait_for_long_mode(&self, total_aps: u8) {
        loop {
            let result = self.long_mode_up.load(Ordering::Relaxed);
            if result == total_aps { break; }
        }
    }
}

pub struct SmpManager<'m, 'a: 'm> {
    boot_code: &'m [u8],
    boot_status_block: &'m BootStatusBlock,
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
        let boot_size = AP_BOOT_CODE.len();
        let boot_pages = boot_size.div_ceil(PAGE_SIZE);

        let boot_in_low_mem = phys_alloc.lock().allocate_select(boot_pages, |page| page.to_usize() <= LOW_MEM_TOP).unwrap();
        let boot_vector = (boot_in_low_mem.to_usize() / PAGE_SIZE) as _;
        let boot_in_low_mem: VirtAddr = boot_in_low_mem.try_into().unwrap();
        let boot_in_low_mem = unsafe { core::slice::from_raw_parts_mut(boot_in_low_mem.to_mut_ptr(), boot_size) };
        boot_in_low_mem.copy_from_slice(AP_BOOT_CODE);
        log::trace!("boot vector: {boot_vector:#04x}");

        let boot_status_block = unsafe { boot_in_low_mem.as_ptr().byte_add(2) };
        let boot_status_block = unsafe { &*(boot_status_block as *const BootStatusBlock) };

        let lapic = intr.get_lapic();
        let id = lapic.id();

        let madt = acpi.unwrap().find_table().unwrap();

        Self {
            boot_code: boot_in_low_mem,
            boot_status_block,
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

    fn initialize_all_aps(
        &mut self,
        addr_space: &AddrSpace<'_>,
        alloc: &Mutex<PhysAlloc>,
        main: extern "C" fn() -> !,
    ) -> Result<()> {
        let bsp = self.this_cpu();
        let ap_count = self.cpus().count() - 1;

        log::trace!("CPUs:");
        for cpu in self.cpus() {
            log::trace!("  {cpu:?} - {}", if cpu == bsp { "BSP" } else { "AP" });
        }

        // boot APs to real mode

        self.lapic.send_ipi(ApicIpi::Init, ApicIpiDest::AllExcludingSelf);
        self.clock.delay(Duration::from_ms(10));

        let mut up: usize = 0;
        const ATTEMPTS: usize = 4;
        for attempt in 0..ATTEMPTS {
            self.lapic.send_ipi(ApicIpi::Start(self.boot_vector), ApicIpiDest::AllExcludingSelf);
            self.clock.delay(Duration::from_us(200));

            up = self.boot_status_block.real_mode_up.load(Ordering::Relaxed) as _;
            log::trace!("SIPI {}/{ATTEMPTS}: {up}/{ap_count} APs up in real mode", attempt + 1);
            if up >= ap_count { break };
        }

        if up != ap_count {
            log::error!("{up}/{ap_count} APs up");
            return Err(Error::Timeout);
        }

        // pass arguments to each AP one by one

        for ap in 0..ap_count {
            let mut ap_space = AddrSpace::new(alloc).unwrap();
            for region in [Region::EmulatorImage, Region::BaseImage, Region::LinearPhysical] {
                let range = MemoryParameters::range(region);
                addr_space.copy_into(&mut ap_space, &range);
            }

            let boot_code_virt = VirtAddr::from_usize((self.boot_vector as usize) * PAGE_SIZE).unwrap();
            let boot_code_phys = PhysAddr::from_usize((self.boot_vector as usize) * PAGE_SIZE).unwrap();
            ap_space.modify().map_range(boot_code_virt, boot_code_phys, self.boot_code.len(), Default::default(), AllocReturn::Start).unwrap();

            // log::trace!("{addr_space:?}");
            // log::trace!("{ap_space:?}");

            let stack = MemoryParameters::range(Region::LocalStack);
            let stk_pages = (stack.end().to_usize() - stack.start().to_usize()).div_ceil(PAGE_SIZE);
            let stack = ap_space.modify().allocate_range(stack.start().clone(), stk_pages, Default::default(), AllocReturn::End).unwrap();

            self.boot_status_block.configure_arguments(ap_space, stack, main);
            self.boot_status_block.issue_setup_command();
            self.boot_status_block.wait_for_long_mode((ap + 1) as _);
        }

        log::trace!("all APs up in long mode");

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
