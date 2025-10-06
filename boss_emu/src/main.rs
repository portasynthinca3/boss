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
use boss_common::target::interface::firmware::FirmwareServices;
use spin::{Mutex, Once};

use boss_common::emu_params::EmuParams;
use boss_common::util::{tar::TarFile, serial_logger::SerialLogger};
use boss_common::target::runtime_cfg::{self, CfgFlags};
use boss_common::target::current::{
    memmgr::*,
    device::wall_clock::*,
    interrupt::*,
    acpi::*,
    smp::*,
};

#[cfg(feature = "run-tests")]
use boss_common::tests::{self, TestEnv};

#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("EMULATOR PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message());
    log::error!("runtime_cfg: {:?}", runtime_cfg::get());
    loop {
        unsafe { asm!("hlt"); }
    }
}

static WALL_CLOCK: Once<WallClock> = Once::new();
static LOGGER: Once<SerialLogger> = Once::new();
static PHYS_ALLOC: Once<Mutex<PhysAlloc>> = Once::new();

/// Entry point. Performs initialization of all the components
#[unsafe(no_mangle)]
extern "C" fn _start(params_location: usize, params_size: usize) -> ! {
    // get parameters from bootloader
    let params_slice = unsafe { core::slice::from_raw_parts(params_location as *const u8, params_size) };
    let (EmuParams {
        mut firmware,
        data_image,
        phys_alloc,
        wall_clock,
    }, _) = bincode::decode_from_slice(params_slice, bincode::config::standard()).unwrap();

    // set up logging
    let wall_clock = WALL_CLOCK.call_once(move || wall_clock);
    log::set_logger(LOGGER.call_once(|| SerialLogger::new(0, wall_clock))).unwrap();

    log::set_max_level(log::LevelFilter::Info);
    #[cfg(feature = "log-debug")]
    log::set_max_level(log::LevelFilter::Debug);
    #[cfg(feature = "log-trace")]
    log::set_max_level(log::LevelFilter::Trace);

    log::info!("boss_emu started");
    log::debug!("clock resolution: {} ps", wall_clock.resolution_ps());
    runtime_cfg::set_flags(CfgFlags::ExecutingInUpperHalf, true);

    log::debug!("memory management init");
    let phys_alloc = PHYS_ALLOC.call_once(move || Mutex::new(phys_alloc));
    let mut addr_space = unsafe { AddrSpace::get_current(phys_alloc) };
    addr_space.modify().unmap_range(MemoryParameters::range(Region::NifOrIdentity)).unwrap();

    log::debug!("acpi init");
    let acpi = unsafe { firmware.take_acpi().ok() };
    if acpi.is_none() { log::debug!("no ACPI") };

    log::debug!("interrupts init");
    let mut interrupt_mgr = IntrMgr::new(&mut addr_space, acpi.as_ref());
    unsafe { interrupt_mgr.set_as_current(); }

    #[cfg(feature = "run-tests")]
    {
        let mut test_env = TestEnv {
            phys_alloc,
            wall_clock,
            scratchpad_area: MemoryParameters::range(Region::LocalHeap),
            addr_space: &mut addr_space,
            intr_mgr: &mut interrupt_mgr,
        };
        tests::run_all(&mut test_env);
    }

    log::debug!("smp init");
    let mut smp = unsafe { SmpManager::new(&interrupt_mgr, acpi.as_ref(), &mut addr_space, phys_alloc, wall_clock) };
    smp.initialize_all_aps();

    log::debug!("heap alloc init");
    initialize_global_alloc(addr_space.take_portion(&MemoryParameters::range(Region::LocalHeap)).unwrap());

    log::debug!("vm start");
    let data_image = unsafe { core::slice::from_raw_parts(data_image.0.to_ptr(), data_image.1) };
    let base_image = TarFile::new(data_image);
    vm::init(&base_image).unwrap()
}
