//! BOSS Bootloader.
//! 
//! Decompresses the emulator executable image (Erlang VM implementation) and
//! OS base image (Erlang code), then jumps to the emulator.

#![no_main]
#![no_std]

use core::{panic::PanicInfo, slice};
use miniz_oxide::inflate::{self, TINFLStatus, core::inflate_flags::*};
use spin::{Mutex, Once};

use boss_common::emu_params::EmuParams;

use boss_common::target::{self, runtime_cfg};

use boss_common::target::current::{
    device::{
        wall_clock::*,
    },
    memmgr::*,
    cpu::*,
    firmware::*,
};

use boss_common::util::{
    serial_logger::SerialLogger,
    elf::{ElfFile, ElfLoadedProgramInfo},
    byte_size::ByteSize,
};

pub mod boot;

#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("BOOT PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message());
    log::error!("runtime_cfg: {:?}", runtime_cfg::get());
    loop {
        Cpu::wait_for_interrupt();
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ImageType {
    ElfExecutable(VirtAddr),
    Data,
}

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

/// Initializes the virtual memory manager by creating a new address space from
/// the entire physical memory map
fn init_virt_memmgr(
    alloc: &Mutex<PhysAlloc>,
    mem_map: impl Iterator<Item = PhysMemRange>
) -> core::result::Result<AddrSpace, target::interface::memmgr::Error> {
    let mut addr_space = AddrSpace::new(alloc)?;
    let mut guard = addr_space.modify();

    let offsets = [
        MemoryParameters::range(Region::NifOrIdentity).start().to_usize(),
        MemoryParameters::range(Region::LinearPhysical).start().to_usize(),
    ];

    for entry in mem_map {
        for offset in offsets {
            guard.map_range(
                VirtAddr::from_usize(entry.start.to_usize() + offset).unwrap(),
                entry.start,
                entry.page_count,
                Default::default(),
                AllocReturn::Start,
            )?;
        }
    }

    drop(guard);
    Ok(addr_space)
}

/// Decompresses an image at a fixed location in virtual memory
fn decompress_image(
    mut compressed: &[u8],
    addr_space: &mut AddrSpace,
    access: Access,
    destination: VirtAddr,
) -> &'static mut [u8] {
    // We don't know the size of the inflated data;
    // try exponentially larger buffers, then compact.
    let mut current_allocated_pages = 0;

    // SAFETY: although `destination` is invalid right now, the size is zero
    let mut destination_buf = unsafe { slice::from_raw_parts_mut::<u8>(destination.to_mut_ptr(), 0) };

    let mut alloc_and_map_pages = |count: usize, total_allocated: &mut usize, buffer: &mut &mut [u8]| {
        let dest_for_new_pages = VirtAddr::from_usize(destination.to_usize() + (*total_allocated * PAGE_SIZE)).unwrap();
        log::trace!("allocating {count} pages at {dest_for_new_pages:?}");
        let _ = addr_space
            .modify()
            .allocate_range(dest_for_new_pages, count, access, AllocReturn::Start)
            .unwrap();
        *total_allocated += count;

        let total_size = *total_allocated * PAGE_SIZE;
        // SAFETY: there's memory at this address with exactly this many bytes allocated
        *buffer = unsafe { slice::from_raw_parts_mut::<u8>(destination.to_mut_ptr(), total_size) };
    };

    alloc_and_map_pages(1, &mut current_allocated_pages, &mut destination_buf);

    let mut decompressor = inflate::core::DecompressorOxide::new();
    let mut output_pos = 0;
    let flags = TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF | TINFL_FLAG_PARSE_ZLIB_HEADER;

    loop {
        let (status, in_consumed, out_filled) =
            inflate::core::decompress(&mut decompressor, compressed, destination_buf, output_pos, flags);
        output_pos += out_filled;

        match status {
            TINFLStatus::Done => break,
            TINFLStatus::HasMoreOutput => (),
            error => panic!("decompression error: {error:?}"),
        }

        compressed = &compressed[in_consumed..];
        alloc_and_map_pages(current_allocated_pages, &mut current_allocated_pages, &mut destination_buf); // effectively doubles the size
    };

    let decompressed_size = output_pos;
    // SAFETY: there's allocated memory at this address with at least this many bytes allocated
    destination_buf = unsafe { slice::from_raw_parts_mut(destination.to_mut_ptr(), decompressed_size) };

    // remove unneeded pages
    let size_in_pages = decompressed_size.div_ceil(PAGE_SIZE);
    if size_in_pages < current_allocated_pages {
        log::trace!("deallocating {} extra pages", current_allocated_pages - size_in_pages);
        addr_space.modify().deallocate_range(
            VirtAddr::from_usize(destination.to_usize() + (size_in_pages * PAGE_SIZE)).unwrap()
            ..=
            VirtAddr::from_usize(destination.to_usize() + ((current_allocated_pages - 1) * PAGE_SIZE)).unwrap()
        ).unwrap();
    }

    log::info!("decompressed {} to {destination:?}", ByteSize(decompressed_size));
    destination_buf
}

static WALL_CLOCK: Once<WallClock> = Once::new();
static LOGGER: Once<SerialLogger> = Once::new();

fn generic_entry(firmware: UninitializedFirmware) -> ! {
    let wall_clock = WALL_CLOCK.call_once(WallClock::calibrate_new);
    log::set_logger(LOGGER.call_once(|| SerialLogger::new(0, wall_clock))).unwrap();

    log::set_max_level(log::LevelFilter::Info);
    #[cfg(feature = "log-trace")]
    log::set_max_level(log::LevelFilter::Trace);

    log::info!("boss_boot started");

    let mut firmware = firmware.init();

    // initialize PMM
    let usable_memory = firmware.get_memory().unwrap().filter(|r| r.usability == PhysMemUsability::Always);
    let phys_alloc = Mutex::new(unsafe { PhysAlloc::new(usable_memory).unwrap() });
    log::info!("available memory: {:?}", phys_alloc.lock().stats().total);

    // create new address space with the same mapping
    let mut addr_space = init_virt_memmgr(&phys_alloc, firmware.get_memory().unwrap()).unwrap();
    unsafe { addr_space.set_as_current() };

    let mut program_info: Option<ElfLoadedProgramInfo> = None;
    let mut data_image: Option<&[u8]> = None;

    let compressed_images: [(ImageType, VirtAddr, &[u8]); 2] = [
        (
            ImageType::ElfExecutable(*MemoryParameters::range(Region::EmulatorImage).start()),
            *MemoryParameters::range(Region::EmulatorHeap).start(),
            include_bytes!("../../.build/boss_emu.elf.zlib"),
        ),
        (
            ImageType::Data,
            *MemoryParameters::range(Region::BaseImage).start(),
            include_bytes!("../../.build/bosbaima.tar.zlib"),
        ),
    ];

    // decompress images
    for (img_type, load_addr, compressed_data) in compressed_images.iter() {
        let access = Access { execute: matches!(img_type, ImageType::ElfExecutable(_)), ..Default::default() };
        let image = decompress_image(compressed_data, &mut addr_space, access, *load_addr);

        if let ImageType::ElfExecutable(load_address) = img_type {
            let elf_file = ElfFile::new(image).unwrap();
            let info = elf_file.load_program(&mut addr_space, *load_address).unwrap();
            log::debug!("{info:?}");
            program_info = Some(info);

            // unload now unneeded compressed image
            let (image_base, image_len) = (image.as_ptr(), image.len());
            let image_base: VirtAddr = VirtAddr::from_ptr(image_base).unwrap();
            addr_space.modify().deallocate_range(image_base ..= VirtAddr::from_usize(image_base.to_usize() + image_len - 1).unwrap()).unwrap();
        } else {
            data_image = Some(image);
        }
    }

    let program_info = program_info.unwrap();
    let data_image = data_image.unwrap();
    let data_image = (VirtAddr::from_ptr(data_image.as_ptr()).unwrap(), data_image.len());

    // allocate stack
    let stack_range = MemoryParameters::range(Region::EmulatorStack);
    let stack_pages = (stack_range.end().to_usize() - stack_range.start().to_usize()) / PAGE_SIZE;
    let stack_top = addr_space.modify().allocate_range(
        *stack_range.start(),
        stack_pages,
        Access { execute: false, ..Default::default() },
        AllocReturn::End,
    ).unwrap();

    // compose parameters to pass to executable
    let params_range = MemoryParameters::range(Region::EmuParams);
    let params_location = addr_space
        .modify()
        .allocate_range(*params_range.start(), 1, Access { execute: false, ..Default::default() }, AllocReturn::Start)
        .unwrap()
        .to_mut_ptr::<u8>();
    let params_slice = unsafe { core::slice::from_raw_parts_mut(params_location, PAGE_SIZE) };
    let params = EmuParams {
        firmware,
        data_image,
        phys_alloc: phys_alloc.into_inner(),
        wall_clock: *wall_clock,
    };
    let params_len = bincode::encode_into_slice(params, params_slice, bincode::config::standard()).unwrap();

    log::debug!("jumping to {:?} stack={:?} params={:?} len={:?}", program_info.entry_point, stack_top, params_location, params_len);
    unsafe {
        Cpu::jump_to(program_info.entry_point, stack_top, &[
            params_location as usize,
            params_len
        ]);
    }
}
