//! Generic platform-independent interface for 2D graphics output

use bincode::{Encode, Decode};

/// Describes a color bit field within a single pixel
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Encode, Decode)]
pub struct BitConfiguration {
    /// Color offset within the pixel, counted from LSb
    pub offset: u8,
    /// Color width within the pixel
    pub width: u8,
}

impl BitConfiguration {
    /// Converts a bitmask to a [`BitConfiguration`], e.g.:
    /// 
    /// `0b01111000` -> `BitConfiguration { offset: 3, width: 4 }`
    /// 
    /// Returns `None` if the mask is non-contiguous, e.g. `0b10111000`.
    pub const fn from_mask(mask: u64) -> Option<BitConfiguration> {
        let trailing = mask.trailing_zeros();
        let leading = mask.leading_zeros();
        let ones = mask.count_ones();

        if leading + ones + trailing != u64::BITS { return None };
        Some(BitConfiguration { offset: trailing as u8, width: ones as u8 })
    }

    pub const fn to_mask(self) -> u64 {
        let main_part = (0b1u64 << self.width) - 1;
        main_part << self.offset
    }
}

/// Describes the pixel format
#[derive(Debug, Clone, PartialEq, Eq)]
#[derive(Encode, Decode)]
pub struct PixelFormat {
    /// Width of a pixel unit in bytes, including actual color data and reserved
    /// space
    pub unit: u8,
    /// Bit configuration of the red portion
    pub r: BitConfiguration,
    /// Bit configuration of the green portion
    pub g: BitConfiguration,
    /// Bit configuration of the blue portion
    pub b: BitConfiguration,
}

pub const FORMAT_RGBX32_LE: PixelFormat = PixelFormat {
    unit: 4,
    r: BitConfiguration { offset: 0, width: 8 },
    g: BitConfiguration { offset: 8, width: 8 },
    b: BitConfiguration { offset: 16, width: 8 },
};

pub const FORMAT_BGRX32_LE: PixelFormat = PixelFormat {
    unit: 4,
    b: BitConfiguration { offset: 0, width: 8 },
    g: BitConfiguration { offset: 8, width: 8 },
    r: BitConfiguration { offset: 16, width: 8 },
};

/// Describes complete video mode parameters
#[derive(Debug, Clone, PartialEq, Eq)]
#[derive(Encode, Decode)]
pub struct Mode {
    /// Resolution: `(width, height)`
    pub resolution: (usize, usize),
    /// Pixel format
    pub format: PixelFormat,
    /// Number of pixels per line of video memory. This might be larger than the
    /// horizontal resolution.
    pub line_stride: usize,
}

pub trait Video {
    /// Gets the current video mode
    fn mode(&self) -> Mode;

    /// Gets the framebuffer. This function is not unsafe, but using the
    /// returned pointer is. Here's a few considerations:
    ///   - The returned tuple contains the size of the buffer in bytes. You
    ///     must not cross the buffer bound.
    ///   - The element type for the buffer depends on the value of
    ///     [`PixelFormat::unit`].
    ///   - You must use the [`core::ptr::write_volatile`] function to access
    ///     this buffer.
    ///   - It's a good idea to set memory caching to Write-Combining for
    ///     performance.
    ///   - For the same reason, it's also a good idea not to read from the
    ///     buffer.
    fn buffer(&mut self) -> (*mut (), usize);
}
