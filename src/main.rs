//! The Emulator.

#![no_main]
#![no_std]
#![feature(panic_info_message)]

mod mem_manager;
mod hal;
mod bosbaima;
mod util;

use core::panic::PanicInfo;
use lazy_static::lazy_static;
use uefi::{
    prelude::*,
    table::boot::MemoryType,
};
use hal::serial::SerialLogger;

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("EMULATOR PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message().unwrap().clone());
    loop { }
}

lazy_static! {
    static ref LOGGER: SerialLogger = SerialLogger::new(0);
}

#[entry]
fn main(image_handle: Handle, system_table: SystemTable<Boot>) -> Status {
    // set up serial logger
    let _ = log::set_logger(&*LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Trace);

    // load base image
    // (contains base BEAM modules and VM options)
    let _base_image = bosbaima::load(image_handle, &system_table)
        .expect("failed to load base image");
    log::info!("loaded base image (BOSBAIMA.TAR)");

    // get memory map and exit boot services
    let (_sys_table, mut mem_map) = system_table.exit_boot_services(MemoryType::LOADER_DATA);
    mem_map.sort();
    
    // initialize PMM
    mem_manager::phys::init(&mem_map);

    loop { }
}
