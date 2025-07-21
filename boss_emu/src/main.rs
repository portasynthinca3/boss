#![no_main]
#![no_std]
#![feature(
    never_type,
    if_let_guard,
    generic_const_exprs,
    let_chains,
)]
#![allow(incomplete_features)]

extern crate alloc;

pub mod vm;

use core::{arch::asm, panic::PanicInfo};
use spin::Once;

use boss_common::{
    target::{
        glue::Glue,
        interrupt,
        memmgr::{
            layout, malloc, phys, virt::AddressSpace, VirtAddr
        },
        runtime_cfg::{self, CfgFlags},
        hal::wall_clock,
        acpi,
        apic,
    },
    util::{serial_logger::SerialLogger, tar::TarFile, boot_stage},
};

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("EMULATOR PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message());
    log::error!("runtime_cfg: {:?}", runtime_cfg::get());
    loop {
        unsafe { asm!("hlt"); }
    }
}

static LOGGER: Once<SerialLogger> = Once::new();

/// Entry point. Performs initialization of all the components
#[no_mangle]
extern "C" fn _start(glue: &Glue) -> ! {
    wall_clock::calibrate();
    log::set_logger(LOGGER.call_once(|| SerialLogger::new(0))).unwrap();

    log::set_max_level(log::LevelFilter::Info);
    #[cfg(feature = "log-debug")]
    log::set_max_level(log::LevelFilter::Debug);
    #[cfg(feature = "log-trace")]
    log::set_max_level(log::LevelFilter::Trace);

    boot_stage::same_level("Emulator started");
    log::info!("boss_emu started");
    runtime_cfg::set_flags(CfgFlags::ExecutingInUpperHalf, true);

    // initialize memory management
    boot_stage::same_level("Physical memory");
    unsafe { phys::import(glue.pmm_state) };
    let mut addr_space = unsafe { AddressSpace::get_current() };

    // initialize interrupts
    boot_stage::same_level("Interrupts");
    let (mut interrupt_mgr, _segments) = interrupt::Manager::new(&mut addr_space);
    unsafe { interrupt_mgr.set_as_current(); }

    boot_stage::same_level("Drop lower half");
    addr_space.modify().unmap_range(VirtAddr::from_usize(0)..=layout::NIF_TOP).unwrap();

    // initialize ACPI
    boot_stage::same_level("ACPI");
    unsafe { acpi::init(glue.acpi_xsdp) };

    // initialize APIC and multiprocessing
    boot_stage::same_level("APIC");
    let madt = acpi::find_table::<apic::Madt>().unwrap();
    apic::init(madt, &mut interrupt_mgr, &mut addr_space);

    // initialize global allocator
    boot_stage::same_level("Heap");
    unsafe { malloc::initialize_default(addr_space) };

    // start the VM
    boot_stage::same_level("VM");
    boot_stage::new_level();
    boot_stage::same_level("Loading base image");
    let base_image = TarFile::new(glue.data_image);
    vm::init(&base_image).unwrap()
}
