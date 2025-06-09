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
    },
    util::{serial_logger::SerialLogger, tar::TarFile},
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

static mut LOGGER: Option<SerialLogger> = None;

/// Entry point. Performs initialization of all the components
#[no_mangle]
extern "C" fn _start(glue: &Glue) -> ! {
    wall_clock::calibrate();
    // init serial logger
    unsafe {
        // SAFETY: this static var is initialized and subsequently borrowed
        // exactly once
        LOGGER = Some(SerialLogger::new(0));
        log::set_logger(LOGGER.as_ref().unwrap()).unwrap();
    }
    #[cfg(feature = "log-trace")]
    log::set_max_level(log::LevelFilter::Trace);
    #[cfg(not(feature = "log-trace"))]
    log::set_max_level(log::LevelFilter::Info);

    log::info!("boss_emu started");
    runtime_cfg::set_flags(CfgFlags::ExecutingInUpperHalf, true);

    // initialize memory management
    unsafe { phys::import(glue.pmm_state) };
    let mut addr_space = unsafe { AddressSpace::get_current() };

    // initialize interrupts
    let (interrupt_mgr, _segments) = interrupt::Manager::new(&mut addr_space);
    unsafe { interrupt_mgr.set_as_current(); }

    addr_space.modify().unmap_range(VirtAddr::from_usize(0)..=layout::NIF_TOP).unwrap();

    // initialize global allocator
    unsafe { malloc::initialize_default(addr_space) };

    // initialize ACPI
    unsafe { acpi::init(glue.acpi_xsdp) };

    // start the VM
    let base_image = TarFile::new(glue.data_image);
    vm::init(&base_image).unwrap()
}
