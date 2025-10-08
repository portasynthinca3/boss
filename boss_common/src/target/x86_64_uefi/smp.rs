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
    SmpContext as IfSmpContext,
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
        msr::*,
    },
};

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

use alloc::alloc::{Allocator, AllocError};
use core::{
    arch::global_asm,
    sync::atomic::{Ordering, AtomicU8, AtomicU32, AtomicU64},
    any::TypeId,
};

use spin::{Mutex, Once};

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

pub struct BootCode<'a> {
    alloc: &'a Mutex<PhysAlloc>,
    code: *const u8,
    phys_addr: PhysAddr,
    vector: u8,
}

static AP_BOOT_CODE: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/../.build/ap_boot.bin"));

impl<'a> BootCode<'a> {
    const PAGE_COUNT: usize = AP_BOOT_CODE.len().div_ceil(PAGE_SIZE);
    const LOW_MEM_TOP: usize = 0x100 * PAGE_SIZE;
    const STATUS_BLOCK_OFFSET: usize = 2;

    fn new(alloc: &'a Mutex<PhysAlloc>) -> Self {
        let phys_addr = alloc.lock().allocate_select(Self::PAGE_COUNT, |page| page.to_usize() <= Self::LOW_MEM_TOP).unwrap();
        let vector = (phys_addr.to_usize() / PAGE_SIZE) as _;
        let in_low_mem: VirtAddr = phys_addr.try_into().unwrap();
        let in_low_mem = unsafe { core::slice::from_raw_parts_mut(in_low_mem.to_mut_ptr(), AP_BOOT_CODE.len()) };
        in_low_mem.copy_from_slice(AP_BOOT_CODE);
        log::trace!("boot vector: {vector:#04x}");

        Self {
            alloc,
            code: in_low_mem.as_ptr(),
            phys_addr,
            vector,
        }
    }

    fn get_status_block(&self) -> &BootStatusBlock {
        let ptr = unsafe { self.code.byte_add(Self::STATUS_BLOCK_OFFSET) };
        unsafe { &*(ptr as *const BootStatusBlock) }
    }
}

impl Drop for BootCode<'_> {
    fn drop(&mut self) {
        let starting_page_idx = self.phys_addr.to_usize() / PAGE_SIZE;
        let pages = (starting_page_idx .. (starting_page_idx + Self::PAGE_COUNT))
                    .map(|idx| PhysAddr::from_usize(idx * PAGE_SIZE).unwrap());
        self.alloc.lock().deallocate(pages).unwrap();
    }
}

pub struct SmpManager<'m, 'a> {
    lapic: &'m Lapic,
    clock: &'m WallClock,
    this: CpuId,
    madt: &'a Madt,
}

impl<'m, 'a> IfSmpManager<'m, 'a> for SmpManager<'m, 'a> {
    unsafe fn new(
        intr: &'m IntrMgr,
        acpi: Option<&Acpi<'a>>,
        clock: &'m WallClock,
        addr_space: &mut AddrSpace<'_>,
    ) -> Self {
        let lapic = intr.get_lapic();
        let this = CpuId(lapic.id().0);
        let madt = acpi.unwrap().find_table().unwrap();

        if SmpContext::maybe_get().is_none() {
            // TODO: PageCnt trait
            let range = MemoryParameters::range(Region::LocalContext);
            let page_cnt = (range.end().to_usize() - range.start().to_usize()).div_ceil(PAGE_SIZE);
            let local_context = addr_space.modify().allocate_range(*range.start(), page_cnt, Default::default(), AllocReturn::Start).unwrap();
            SmpContext::new_at(local_context.to_mut_ptr());
        }

        SmpContext::get().create_record(this);

        Self { lapic, clock, this, madt }
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
        addr_space: &mut AddrSpace<'_>,
        alloc: &Mutex<PhysAlloc>,
        main: extern "C" fn() -> !,
    ) -> Result<()> {
        let bsp = self.this_cpu();
        let ap_count = self.cpus().count() - 1;

        log::trace!("{} CPUs, {ap_count} APs, BSP is {bsp:?}", self.cpus().count());

        let boot_code = BootCode::new(alloc);
        let status_blk = boot_code.get_status_block();

        // boot APs to real mode

        self.lapic.send_ipi(ApicIpi::Init, ApicIpiDest::AllExcludingSelf);
        self.clock.delay(Duration::from_ms(10));

        let mut up: usize = 0;
        const ATTEMPTS: usize = 4;
        for attempt in 0..ATTEMPTS {
            self.lapic.send_ipi(ApicIpi::Start(boot_code.vector), ApicIpiDest::AllExcludingSelf);
            self.clock.delay(Duration::from_us(200));

            up = status_blk.real_mode_up.load(Ordering::Relaxed) as _;
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
            for region in [Region::EmulatorImage, Region::BaseImage, Region::LinearPhysical, Region::SharedHeap] {
                let range = MemoryParameters::range(region);
                addr_space.copy_into(&mut ap_space, &range);
            }

            let boot_code_virt = VirtAddr::from_usize((boot_code.vector as usize) * PAGE_SIZE).unwrap();
            let boot_code_phys = PhysAddr::from_usize((boot_code.vector as usize) * PAGE_SIZE).unwrap();
            ap_space.modify().map_range(boot_code_virt, boot_code_phys, AP_BOOT_CODE.len(), Default::default(), AllocReturn::Start).unwrap();

            let cpu_context = MemoryParameters::range(Region::LocalContext);
            let ctx_pages = (cpu_context.end().to_usize() - cpu_context.start().to_usize()).div_ceil(PAGE_SIZE);
            ap_space.modify().allocate_range(*cpu_context.start(), ctx_pages, Default::default(), AllocReturn::Start).unwrap();

            let stack = MemoryParameters::range(Region::LocalStack);
            let stk_pages = (stack.end().to_usize() - stack.start().to_usize()).div_ceil(PAGE_SIZE);
            let stack = ap_space.modify().allocate_range(*stack.start(), stk_pages, Default::default(), AllocReturn::End).unwrap();
            let stack = VirtAddr::from_usize(stack.to_usize() - 8).unwrap();

            status_blk.configure_arguments(ap_space, stack, main);
            status_blk.issue_setup_command();
            status_blk.wait_for_long_mode((ap + 1) as _);
        }

        log::trace!("all APs up in long mode");

        Ok(())
    }

    unsafe fn messenger<T: Share>(&mut self) -> Result<self::SmpMessenger<T>> {
        todo!()
    }
}

pub struct SmpContext {
    mutex: Mutex<()>,
}

struct RecordHeader {
    valid: bool,
    type_id: TypeId,
    length: usize,
}

static SMP_CONTEXT_AVAILABLE: Once<()> = Once::new();

impl SmpContext {
    const ALIGN: usize = align_of::<usize>();
    const HDR_PAYLOAD_OFFSET: usize = size_of::<RecordHeader>().div_ceil(Self::ALIGN) * Self::ALIGN;

    fn new_at(addr: *mut Self) {
        let area = MemoryParameters::range(Region::LocalContext);
        let area_size = area.end().to_usize() - area.start().to_usize() + 1;
        unsafe { addr.write_bytes(0, area_size) };
        unsafe { addr.write(Self { mutex: Mutex::new(()) }) };
        SMP_CONTEXT_AVAILABLE.call_once(|| ());
    }

    fn record_space(&self) -> *mut RecordHeader {
        let ptr = self as *const Self;
        let ptr = unsafe { ptr.byte_add(Self::ALIGN) } as *const RecordHeader;
        ptr as *mut _
    }
}

impl IfSmpContext for SmpContext {
    fn maybe_get() -> Option<&'static Self> {
        let addr = MemoryParameters::range(Region::LocalContext).start().to_ptr::<Self>();
        if SMP_CONTEXT_AVAILABLE.get().is_none() { return None };
        Some(unsafe { &*addr })
    }

    fn create_record<T: 'static>(&self, value: T) {
        assert!(align_of::<T>() <= Self::ALIGN);
        let _lock = self.mutex.lock();
        
        let mut current_record = self.record_space();
        loop {
            let record = unsafe { &mut *current_record };
            if !record.valid {
                record.valid = true;
                record.type_id = TypeId::of::<T>();
                record.length = Self::HDR_PAYLOAD_OFFSET + (size_of::<T>().div_ceil(Self::ALIGN) * Self::ALIGN);
                let value_ptr = unsafe { current_record.byte_add(Self::HDR_PAYLOAD_OFFSET) } as *mut T;
                unsafe { value_ptr.write(value) }
                return;
            }
            current_record = unsafe { current_record.byte_add(record.length) };
        }

        panic!()
    }

    fn find_record<T: 'static>(&self) -> Option<&'static T> {
        assert!(align_of::<T>() <= Self::ALIGN);
        let _lock = self.mutex.lock();

        let mut current_record = self.record_space();
        loop {
            let record = unsafe { &*current_record };
            if !record.valid { break };
            if record.type_id == TypeId::of::<T>() {
                let value_ptr = unsafe { current_record.byte_add(Self::HDR_PAYLOAD_OFFSET) } as *const T;
                return Some(unsafe { &*value_ptr });
            }
            current_record = unsafe { current_record.byte_add(record.length) };
        }

        None
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
