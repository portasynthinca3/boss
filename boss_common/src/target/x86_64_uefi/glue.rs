use super::memmgr::{phys::PmmState, PhysAddr};
use super::hal::video::Video;

/// The interface between the bootloader and the emulator. The former uses this
/// struct to pass data to the latter.
pub struct Glue {
    pub acpi_xsdp: PhysAddr,
    pub pmm_state: PmmState,
    pub video: Option<Video>,
    pub data_image: &'static [u8],
}
