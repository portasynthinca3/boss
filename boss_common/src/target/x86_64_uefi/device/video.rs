//! Implements a simple video interface. In this case, using the UEFI GOP
//! (Graphics Output Protocol)

use uefi::{proto::console::gop::{GraphicsOutput, ModeInfo, PixelFormat as UefiPixFormat}, table::{Boot, SystemTable}};
use crate::target::current::memmgr::*;
pub use crate::target::interface::device::video::{
    BitConfiguration,
    PixelFormat,
    FORMAT_RGBX32_LE,
    FORMAT_BGRX32_LE,
    Mode,
    Video as IfVideo,
};

use bincode::{Encode, Decode};

#[derive(Encode, Decode)]
#[allow(dead_code)]
pub struct Video {
    mode: Mode,
    framebuffer: PhysAddr,
    framebuffer_size: usize,
}

impl From<ModeInfo> for Mode {
    fn from(value: ModeInfo) -> Self {
        let format = match value.pixel_format() {
            UefiPixFormat::Rgb => FORMAT_RGBX32_LE,
            UefiPixFormat::Bgr => FORMAT_BGRX32_LE,
            UefiPixFormat::Bitmask => {
                let mask = value.pixel_bitmask().unwrap();
                PixelFormat {
                    unit: 4,
                    r: BitConfiguration::from_mask(mask.red as u64).unwrap(),
                    g: BitConfiguration::from_mask(mask.green as u64).unwrap(),
                    b: BitConfiguration::from_mask(mask.blue as u64).unwrap(),
                }
            },
            _ => panic!(),
        };

        Mode {
            resolution: value.resolution(),
            format,
            line_stride: value.stride(),
        }
    }
}

impl Video {
    pub fn new(system_table: &SystemTable<Boot>) -> Option<Video> {
        let boot_services = system_table.boot_services();
        let gop_handle = boot_services.get_handle_for_protocol::<GraphicsOutput>().ok()?;
        let mut gop = boot_services.open_protocol_exclusive::<GraphicsOutput>(gop_handle).ok()?;

        #[cfg(feature = "trace-hal-video")]
        for mode in gop.modes(boot_services) {
            let info = mode.info();
            let (w, h) = info.resolution();
            log::trace!("{w}x{h} {:?}", info.pixel_format());
        }

        let current_mode = gop.current_mode_info();
        let (w, h) = current_mode.resolution();
        log::trace!("current: {w}x{h} {:?}", current_mode.pixel_format());
        if current_mode.pixel_format() == UefiPixFormat::BltOnly { return None };

        let mut framebuffer = gop.frame_buffer();
        let virt_addr = VirtAddr::from_mut_ptr(framebuffer.as_mut_ptr()).unwrap();

        Some(Video {
            mode: current_mode.into(),
            framebuffer: virt_addr.try_into().unwrap(),
            framebuffer_size: framebuffer.size(),
        })
    }
}

impl IfVideo for Video {
    fn mode(&self) -> Mode {
        self.mode.clone()
    }

    fn buffer(&mut self) -> (*mut (), usize) {
        let virt_addr: VirtAddr = self.framebuffer.try_into().unwrap();
        (virt_addr.to_mut_ptr(), self.framebuffer_size)
    }
}
