//! Loads and parses the BOSS Base Image (BBI or BOSBAIMA.TAR).
//! 
//! The image is loaded using the UEFI Simple Filesystem protocol.
//! 
//! The base image contains VM arguments and the bare minimum of BEAM modules
//! (and possibly NIFs) needed to get to a point where other modules could be
//! loaded from disk by the OS on its own, without using the UEFI boot services.

use uefi::{
    prelude::*,
    table::boot::{
        OpenProtocolParams,
        OpenProtocolAttributes,
        MemoryType
    },
    proto::{
        loaded_image::LoadedImage,
        media::{
            fs::SimpleFileSystem,
            file::{ File, FileMode, FileAttribute, FileInfo }
        }
    },
};
use spin::Once;

use crate::{reloc::Relocatable, util::{byte_size::ByteSize, tar::TarFile}};
use crate::{checkpoint, checkpoint::Checkpoint};

/// Holds the base image
static IMAGE: Once<TarFile<'static>> = Once::new();

/// Loads the BBI into memory and returns a static slice.
/// 
/// It does so by allocating a memory chunk using the UEFI Boot Services and
/// calling the UEFI Simple Filesystem Protocol.
pub fn load(executable_handle: Handle, system_table: &SystemTable<Boot>) -> uefi::Result<()> {
    // get loaded image protocol handle
    let image_protocol_handle = unsafe {
        system_table.boot_services().open_protocol::<LoadedImage>(OpenProtocolParams {
            agent: executable_handle,
            controller: None,
            handle: executable_handle,
        }, OpenProtocolAttributes::Exclusive)?
    };
    log::trace!("opened EFI_LOADED_IMAGE_PROTOCOL");

    // get filesystem protocol handle
    let device_handle = image_protocol_handle.device().unwrap();
    let mut fs_protocol_handle = unsafe {
        system_table.boot_services().open_protocol::<SimpleFileSystem>(OpenProtocolParams {
            agent: executable_handle,
            controller: None,
            handle: device_handle,
        }, OpenProtocolAttributes::Exclusive)?
    };
    log::trace!("opened EFI_SIMPLE_FILE_SYSTEM_PROTOCOL");

    // open root directory
    let mut root_dir = fs_protocol_handle.open_volume()?;
    log::trace!("opened root directory");

    // open image file
    let mut bosbaima = root_dir.open(cstr16!("BOSS\\BOSBAIMA.TAR"), FileMode::Read, FileAttribute::empty())?
        .into_regular_file()
        .ok_or(uefi::Error::new(Status::NOT_FOUND, ()))?;
    log::trace!("opened esp/BOSS/BOSBAIMA.TAR");

    // get file size
    let mut buffer = [0u8; 256];
    let bbi_info = bosbaima.get_info::<FileInfo>(&mut buffer)
        .map_err(|err| err.status())?;
    let bbi_size = bbi_info.file_size() as usize;
    log::info!("base image size: {}", ByteSize(bbi_size));

    // allocate memory for and read file
    let mut file_ptr = system_table.boot_services().allocate_pool(MemoryType::LOADER_DATA, bbi_size)?;
    let file_buf = unsafe { core::slice::from_raw_parts_mut(file_ptr, bbi_size) };
    bosbaima.read(file_buf)?;

    // relocate and assign file buffer
    file_ptr.relocate();
    let file_buf = unsafe { core::slice::from_raw_parts(file_ptr, bbi_size) };
    IMAGE.call_once(|| TarFile::new(file_buf));
    
    checkpoint::advance(Checkpoint::BaseImageLoaded).unwrap();
    Ok(())
}

pub fn get() -> &'static TarFile<'static> {
    IMAGE.get().unwrap()
}
