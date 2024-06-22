//! The Emulator entry point

#![no_main]
#![no_std]
#![feature(panic_info_message, abi_x86_interrupt)]
#![allow(dead_code)]

use core::{arch::asm, panic::PanicInfo};
use uefi::{
    prelude::*,
    table::boot::MemoryType,
};
use hal::serial::SerialLogger;
use mem_manager::*;
use checkpoint::Checkpoint;

mod checkpoint;
mod mem_manager;
mod hal;
mod bosbaima;
mod util;

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("EMULATOR PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message().unwrap().clone());
    log::error!("checkpoint: {:?}", checkpoint::get());
    loop {
        unsafe { asm!("hlt"); }
    }
}

static mut LOGGER: Option<SerialLogger> = None;

/// Entry point. Performs initialization of all the components
#[entry]
fn main(image_handle: Handle, system_table: SystemTable<Boot>) -> Status {
    // init serial logger
    unsafe {
        // actually safe because this static var is initialized and subsequently
        // borrowed exactly once
        LOGGER = Some(SerialLogger::new(0));
        log::set_logger(LOGGER.as_ref().unwrap()).unwrap();
    }
    log::set_max_level(log::LevelFilter::Debug);

    // load base image
    // (contains base BEAM modules and VM options)
    bosbaima::load(image_handle, &system_table).unwrap();

    // get memory map and exit boot services
    let (_sys_table, mut mem_map) = system_table.exit_boot_services(MemoryType::LOADER_DATA);
    checkpoint::advance(Checkpoint::BootServicesExited).unwrap();
    mem_map.sort();
    
    // init memory management
    phys::init(&mem_map);
    let mut addr_space = reloc::make_dual_map(&mem_map);

    // relocate everything to the upper half
    phys::relocate();

    // create new stack
    // 10 pages = 40 KiB
    let stack_top = addr_space.allocate_stack(reloc::EMULATOR_STK_BASE, 10, Default::default()).unwrap();

    // continue execution with the instruction pointer in the upper half
    unsafe { reloc::execute_jump(stack_top, after_reloc, &()); }
}

extern "C" fn after_reloc(_data: &()) -> ! {
    // jumped successfully
    checkpoint::advance(Checkpoint::RelocUpperExec).unwrap();

    // unmap lower half
    let mut addr_space = unsafe { virt::AddressSpace::get_current() };
    addr_space.root().deallocate(true).unwrap();
    checkpoint::advance(Checkpoint::RelocDone).unwrap();

    loop {
        unsafe { asm!("hlt"); }
    }
}
