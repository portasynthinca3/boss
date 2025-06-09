#![no_main]
#![no_std]

use core::{arch::asm, panic::PanicInfo, slice};
use miniz_oxide::inflate::{self, TINFLStatus, core::inflate_flags::*};
use uefi::{
    prelude::*, table::{boot::{MemoryMap, MemoryType}, cfg::ACPI2_GUID},
};

use boss_common::{
    target::{
        memmgr::{
            layout, phys, virt::{AddressSpace, TableAttrs}, MemMgrError, PhysAddr, VirtAddr, PAGE_SIZE
        },
        runtime_cfg,
        glue::Glue,
        hal::{video::Video, wall_clock},
    },
    util::{byte_size::ByteSize, elf::{ElfFile, ElfLoadedProgramInfo}, serial_logger::SerialLogger},
};

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(info: &PanicInfo) -> ! {
    log::error!("BOOT PANIC at {}:", info.location().unwrap().clone());
    log::error!("{}", info.message());
    log::error!("runtime_cfg: {:?}", runtime_cfg::get());
    loop {
        unsafe { asm!("hlt"); }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ImageType {
    ElfExecutable(VirtAddr),
    Data,
}

/// Where the ELF file will be decompressed to
const EMULATOR_ELF_LOCATION: VirtAddr = VirtAddr::from_usize(0xffff_b000_0000_0000);

/// Which images need to be decompressed and where
const COMPRESSED_IMAGES: [(ImageType, VirtAddr, &[u8]); 2] = [
    (
        ImageType::ElfExecutable(layout::EMULATOR_IMG_BASE),
        EMULATOR_ELF_LOCATION,
        include_bytes!("../../.build/boss_emu.elf.zlib")
    ),
    (
        ImageType::Data,
        layout::BASE_IMG_BASE,
        include_bytes!("../../.build/bosbaima.tar.zlib")
    ),
];

static mut LOGGER: Option<SerialLogger> = None;

/// Initializes the virtual memory manager by creating an address space from the UEFI mapping
fn init_virt_memmgr(mem_map: &MemoryMap<'_>) -> Result<AddressSpace, MemMgrError> {
    let mut addr_space = AddressSpace::new()?;
    let mut guard = addr_space.modify();
    for entry in mem_map.entries() {
        if entry.ty == MemoryType::RESERVED { continue };
        guard.map_range(
            VirtAddr::from_usize(entry.phys_start as usize),
            PhysAddr(entry.phys_start as usize),
            entry.page_count as usize,
            Default::default(),
            false
        )?;
        guard.map_range(
            VirtAddr::from_usize(entry.phys_start as usize) + layout::IDENTITY_BASE,
            PhysAddr(entry.phys_start as usize),
            entry.page_count as usize,
            Default::default(),
            false
        )?;
    }
    drop(guard);
    Ok(addr_space)
}

/// Decompresses an image at a fixed location in virtual memory
fn decompress_image(
    mut compressed: &[u8],
    addr_space: &mut AddressSpace,
    attrs: TableAttrs,
    destination: VirtAddr
) -> &'static mut [u8] {
    // We don't know the size of the inflated data; try exponentially larger buffers, then contract.
    let mut current_allocated_pages = 0;

    // SAFETY: although `destination` is invalid right now, the size is zero
    let mut destination_buf = unsafe { slice::from_raw_parts_mut::<u8>(destination.into(), 0) };

    let mut alloc_and_map_pages = |count: usize, total_allocated: &mut usize, buffer: &mut &mut [u8]| {
        let dest_for_new_pages = destination + VirtAddr::from_usize(*total_allocated * PAGE_SIZE);
        log::trace!("allocating {count} pages at {dest_for_new_pages:?}");
        let _ = addr_space
            .modify()
            .allocate_range(dest_for_new_pages, count, attrs, false)
            .unwrap();
        *total_allocated += count;

        let total_size = *total_allocated * PAGE_SIZE;
        // SAFETY: there's memory at this address with exactly this many bytes allocated
        *buffer = unsafe { slice::from_raw_parts_mut::<u8>(destination.into(), total_size) };
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
            _ => panic!(),
        }

        compressed = &compressed[in_consumed..];
        alloc_and_map_pages(current_allocated_pages, &mut current_allocated_pages, &mut destination_buf); // effectively doubles the size
    };

    let decompressed_size = output_pos;
    // SAFETY: there's allocated memory at this address with at least this many bytes allocated
    destination_buf = unsafe { slice::from_raw_parts_mut::<u8>(destination.into(), decompressed_size) };

    // remove unneeded pages
    let size_in_pages = decompressed_size.div_ceil(PAGE_SIZE);
    if size_in_pages < current_allocated_pages {
        log::trace!("deallocating {} extra pages", current_allocated_pages - size_in_pages);
        addr_space.modify().deallocate_range(
            destination + VirtAddr::from_usize(size_in_pages * PAGE_SIZE)
            ..=
            destination + VirtAddr::from_usize((current_allocated_pages - 1) * PAGE_SIZE)
        ).unwrap();
    }

    log::info!("decompressed {} to {destination:?}", ByteSize(decompressed_size));
    destination_buf
}

fn run_executable(info: &ElfLoadedProgramInfo, stack_top: VirtAddr, glue: *const Glue) -> ! {
    log::debug!("jumping to {:?} with stack={stack_top:?} glue={:#x}", info.entry_point, glue as usize);

    let stack_top: usize = stack_top.into();
    let entry_point: usize = info.entry_point.into();

    unsafe {
        asm!(
            "mov rsp, {stack}",
            "mov rbp, {stack}",
            "push {jump_ptr}",
            "ret",
            stack = in(reg) stack_top,
            jump_ptr = in(reg) entry_point,
            in("rdi") glue,
            options(noreturn),
        );
    }
}

/// Bootloader entry point
#[cfg_attr(not(test), entry)]
fn main(image_handle: Handle, system_table: SystemTable<Boot>) -> Status {
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

    log::info!("boss_boot started");

    let acpi_xsdp: VirtAddr = system_table
        .config_table()
        .iter()
        .find(|entry| entry.guid == ACPI2_GUID)
        .unwrap()
        .address
        .into();
    let acpi_xsdp: PhysAddr = acpi_xsdp.into();

    // video has to be initialized before exiting boot services
    let video = Video::new(&system_table);

    // initialize physical memory manager
    let (_system_table, mem_map) = system_table.exit_boot_services(MemoryType::LOADER_DATA);
    phys::init(&mem_map);

    // create new address space with the same mapping
    let mut addr_space = init_virt_memmgr(&mem_map).unwrap();
    // SAFETY: we've created a copy of the space we're running in, it's safe.
    unsafe { addr_space.set_as_current() };

    let mut program_info: Option<ElfLoadedProgramInfo> = None;
    let mut data_image: Option<&[u8]> = None;

    // decompress images
    for (img_type, load_addr, compressed_data) in COMPRESSED_IMAGES.iter() {
        let attrs = TableAttrs { execute: matches!(img_type, ImageType::ElfExecutable(_)), ..Default::default() };
        let image = decompress_image(compressed_data, &mut addr_space, attrs, *load_addr);

        if let ImageType::ElfExecutable(load_address) = img_type {
            let elf_file = ElfFile::new(image).unwrap();
            let info = elf_file.load_program(&mut addr_space, *load_address).unwrap();
            log::debug!("{:?}", info);
            program_info = Some(info);

            // unload now unneeded compressed image
            let (image_base, image_len) = (image.as_ptr(), image.len());
            let image_base: VirtAddr = image_base.into();
            addr_space.modify().deallocate_range(image_base ..= (image_base + VirtAddr::from_usize(image_len))).unwrap();
        } else {
            data_image = Some(image);
        }
    }

    let Some(program_info) = program_info else { panic!("no executable set") };
    let Some(data_image) = data_image else { panic!("no data set") };

    // allocate stack
    let stack_top = addr_space.modify().allocate_range(
        layout::EMULATOR_STACK_BASE,
        layout::EMULATOR_STK_PAGES,
        TableAttrs { execute: false, ..Default::default() },
        true
    ).unwrap();

    // compose parameters to pass to executable
    let glue = Glue {
        acpi_xsdp,
        pmm_state: phys::export(),
        video,
        data_image,
    };

    let glue_size_pages = size_of::<Glue>().div_ceil(PAGE_SIZE);
    let glue_location: *mut Glue = addr_space
        .modify()
        .allocate_range(layout::GLUE_BASE, glue_size_pages, TableAttrs { execute: false, ..Default::default() }, false)
        .unwrap()
        .into();
    unsafe { glue_location.write(glue) };

    run_executable(&program_info, stack_top, glue_location);
}
