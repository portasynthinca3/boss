//! The Emulator entry point

#![no_main]
#![no_std]
#![feature(
    panic_info_message,
    naked_functions,
    allocator_api,
    inline_const,
    let_chains,
    ptr_as_uninit,
    slice_ptr_get,
    non_null_convenience,
    alloc_layout_extra,
    generic_arg_infer,
    generic_const_exprs,
    never_type,
    slice_as_chunks,
)]
#![allow(dead_code)]

extern crate alloc;

use core::{arch::asm, panic::PanicInfo};
use uefi::{
    prelude::*,
    table::boot::MemoryType,
    proto::loaded_image::LoadedImage,   
};

use hal::serial::SerialLogger;
use mem_manager::*;
use checkpoint::Checkpoint;

mod checkpoint;
mod mem_manager;
mod hal;
mod bosbaima;
mod util;
mod interrupt;
mod segment;
mod vm;
mod ll;

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
    log::set_max_level(log::LevelFilter::Info);

    // print our address
    let loaded_image = system_table.boot_services()
        .open_protocol_exclusive::<LoadedImage>(image_handle)
        .expect("Failed to open LoadedImage protocol")
        .info();
    log::info!("emulator image base={:#x} size={:#x}", loaded_image.0 as usize, loaded_image.1);

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
    reloc::relocate_pe(loaded_image);
    phys::relocate();

    // create new stack
    let stack_top = {
        let mut mut_space = addr_space.modify();
        mut_space.allocate_range(EMULATOR_STK_BASE, EMULATOR_STK_PAGES, Default::default(), true).unwrap()
    };

    // continue execution with the instruction pointer in the upper half
    unsafe { reloc::execute_jump(stack_top, after_reloc, &()); }
}

extern "C" fn after_reloc(_data: &()) -> ! {
    // jumped successfully
    checkpoint::advance(Checkpoint::RelocUpperExec).unwrap();
    let mut addr_space = unsafe { virt::AddressSpace::get_current() };

    // initialize interrupts
    let (interrupt_mgr, _segments) = interrupt::Manager::new(&mut addr_space);
    unsafe { interrupt_mgr.set_as_current(); }
    checkpoint::advance(Checkpoint::Interrupts).unwrap();

    // unmap lower half
    addr_space.modify().unmap_range(VirtAddr::from_usize(0)..=VirtAddr::from_usize(0x0000_7fff_ffff_ffff)).unwrap();
    checkpoint::advance(Checkpoint::RelocDone).unwrap();

    // initialize global allocator
    unsafe { malloc::initialize_default(addr_space) };
    checkpoint::advance(Checkpoint::Heap).unwrap();

    // start the VM
    vm::init(bosbaima::get()).unwrap();
}
