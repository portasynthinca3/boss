//! Firmware interface implementation for UEFI

use crate::target::current::{
    acpi::*, device::video::*, memmgr::*
};

pub use crate::target::interface::firmware::{
    Error,
    Result,
    UninitializedFirmware as IfUninitializedFirmware,
    FirmwareServices as IfFirmwareServices,
};

use alloc::borrow::ToOwned;
use uefi::{
    prelude::*,
    table::{
        cfg::ACPI2_GUID,
        boot::{MemoryMap, MemoryType},
    }
};

use bincode::{Encode, Decode};

/// Uninitialized firmware
pub struct UninitializedFirmware {
    system_table: SystemTable<Boot>,
}

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;
const MAX_MEM_RANGES: usize = 32;

/// Initialized firmware that is ready to provide services
#[derive(Encode, Decode)]
pub struct FirmwareServices {
    mem_map: ([PhysMemRange; MAX_MEM_RANGES], usize),
    acpi_xsdp: Option<PhysAddr>,
    video: Option<Option<Video>>,
}

impl UninitializedFirmware {
    pub fn new(system_table: SystemTable<Boot>) -> UninitializedFirmware {
        UninitializedFirmware {
            system_table,
        }
    }
}

fn uefi_mem_map_to_our_mem_map(input: MemoryMap<'static>) -> ([PhysMemRange; MAX_MEM_RANGES], usize) {
    let invalid_range = PhysMemRange {
        start: PhysAddr::from_usize(0).unwrap(),
        page_count: 0,
        usability: PhysMemUsability::Always,
    };

    let mut mem_map = ([invalid_range; MAX_MEM_RANGES], 0);
    let (ref mut entries, ref mut count) = mem_map;

    for descriptor in input.entries() {
        let usability = match descriptor.ty {
            MemoryType::CONVENTIONAL => Some(PhysMemUsability::Always),

            MemoryType::BOOT_SERVICES_CODE |
            MemoryType::BOOT_SERVICES_DATA |
            MemoryType::RUNTIME_SERVICES_CODE |
            MemoryType::RUNTIME_SERVICES_DATA => Some(PhysMemUsability::AfterFirmwareDeinit),

            MemoryType::LOADER_CODE |
            MemoryType::LOADER_DATA => Some(PhysMemUsability::AfterBootloaderExit),

            MemoryType::ACPI_RECLAIM => Some(PhysMemUsability::AfterAcpiReclaim),

            _ => None,
        };

        #[cfg(feature = "trace-pmm")]
        log::trace!("{:#018x} {:?} {:?} -> {usability:?}", descriptor.phys_start, ByteSize(descriptor.page_count as usize * PAGE_SIZE), descriptor.ty);

        if let Some(usability) = usability {
            // try to extend last range
            let prev_range_extended = 'xtd: {
                if *count == 0 { break 'xtd false; }

                let prev_range = &mut entries[*count - 1];
                let prev_end = prev_range.start.to_usize() + (prev_range.page_count * PAGE_SIZE);
                // log::trace!("{:#018x} {:?} {:#018x}", end, prev_range.start, descriptor.phys_start);
                if prev_end != descriptor.phys_start as usize { break 'xtd false; }
                if prev_range.usability != usability { break 'xtd false; }

                prev_range.page_count += descriptor.page_count as usize;
                true
            };

            if !prev_range_extended {
                assert!(*count < MAX_MEM_RANGES);
                entries[*count] = PhysMemRange {
                    start: PhysAddr::from_usize(descriptor.phys_start as usize).unwrap(),
                    page_count: descriptor.page_count as usize,
                    usability,
                };
                // log::trace!("{:?}", entries[*count]);
                *count += 1;
            }
        }
    }

    log::debug!("compacted {} entries to {}", input.entries().count(), count);

    mem_map
}

impl IfUninitializedFirmware for UninitializedFirmware {
    fn init(self) -> FirmwareServices {
        let acpi_xsdp = VirtAddr::from_ptr(
            self.system_table
            .config_table()
            .iter()
            .find(|entry| entry.guid == ACPI2_GUID)
            .unwrap()
            .address).unwrap();
        let acpi_xsdp: PhysAddr = acpi_xsdp.try_into().unwrap();

        let video = Video::new(&self.system_table);

        let (_system_table, mut mem_map) = self.system_table.exit_boot_services(MemoryType::LOADER_DATA);
        mem_map.sort();

        FirmwareServices {
            mem_map: uefi_mem_map_to_our_mem_map(mem_map),
            acpi_xsdp: Some(acpi_xsdp),
            video: Some(video),
        }
    }
}

impl IfFirmwareServices for FirmwareServices {
    fn get_memory(&mut self) -> Result<impl Iterator<Item = PhysMemRange>> {
        let (ref entries, count) = self.mem_map;
        Ok(entries
            .iter()
            .take(count)
            .map(|r| r.to_owned()))
    }

    unsafe fn take_acpi<'t>(&mut self) -> Result<Acpi<'t>> {
        let xsdp = self.acpi_xsdp.take().ok_or(Error::AlreadyTaken)?;
        Ok(Acpi::new(xsdp))
    }

    fn take_video(&mut self) -> Result<impl IfVideo> {
        self.video
            .take().ok_or(Error::AlreadyTaken)?
            .take().ok_or(Error::NotSupported)
    }
}
