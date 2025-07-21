//! Implements a simple video interface. In this case, using the UEFI GOP
//! (Graphics Output Protocol)

use uefi::{proto::console::gop::{GraphicsOutput, ModeInfo, PixelFormat}, table::{Boot, SystemTable}};

#[allow(dead_code)]
pub struct Video {
    mode: ModeInfo,
    framebuffer: *mut u8,
    framebuffer_size: usize,
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
        if current_mode.pixel_format() == PixelFormat::BltOnly { return None };

        let mut framebuffer = gop.frame_buffer();

        Some(Video {
            mode: current_mode,
            framebuffer: framebuffer.as_mut_ptr(),
            framebuffer_size: framebuffer.size(),
        })
    }
}
