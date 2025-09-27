#[cfg(boss_target = "x86_64-uefi")]
pub mod current {
    use uefi::prelude::*;
    use crate::generic_entry;
    use boss_common::target::current::firmware::UninitializedFirmware;

    #[entry]
    fn main(image_handle: Handle, system_table: SystemTable<Boot>) -> Status {
        let firmware = UninitializedFirmware::new(system_table);
        generic_entry(firmware)
    }
}
