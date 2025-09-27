//! Generic platform-independent interface for a firmware

use crate::target::interface::{memmgr::*, device::video::*, acpi::*};
use crate::target::current::firmware as concrete;

/// Firmware interface error
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    NotSupported,
    AlreadyTaken,
}

pub type Result<T> = core::result::Result<T, Error>;

/// Allows getting access to firmware's services.
pub trait UninitializedFirmware {
    /// Initialize the firmware. It is _highly advised_ to set up a logger
    /// beforehand.
    fn init(self) -> concrete::FirmwareServices;
}

/// Generic interface for interacting with the platform's firmware.
/// 
/// All of the functions in the `take_` family are not required to be callable
/// more than once. Subsequent calls are allowed to return
/// [`Error::AlreadyTaken`].
pub trait FirmwareServices {
    /// Gets the physical memory map
    fn get_memory(&mut self) -> Result<impl Iterator<Item = PhysMemRange>>;

    /// Gets the ACPI wrapper if the platform supports ACPI
    /// 
    /// # Safety
    /// You must [`drop`] the returned ACPI wrapper before reclaiming ACPI
    /// memory
    unsafe fn take_acpi<'t>(&mut self) -> Result<Acpi<'t>>;

    /// Gets the video adapter if the platform supports one
    fn take_video(&mut self) -> Result<impl Video>;
}
