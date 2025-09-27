use bincode::{Encode, Decode};

use crate::target::current::{
    memmgr::*,
    firmware::*,
    device::wall_clock::*,
};

/// Emulator parameters, passed from the bootloader to the emulator
#[derive(Encode, Decode)]
pub struct EmuParams {
    pub firmware: FirmwareServices,
    pub data_image: (VirtAddr, usize),
    pub phys_alloc: PhysAlloc,
    pub wall_clock: WallClock,
}
