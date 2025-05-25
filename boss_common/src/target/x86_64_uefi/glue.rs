use uefi::table::{Runtime, SystemTable};
use super::memmgr::phys::PmmState;
use super::hal::video::Video;

/// The interface between the bootloader and the emulator. The former uses this
/// struct to pass data to the latter.
pub struct Glue {
    pub system_table: SystemTable<Runtime>,
    pub pmm_state: PmmState,
    pub video: Option<Video>,
    pub data_image: &'static [u8],
}
