use core::fmt::{Debug, Display, Formatter, self};

use derive_more::{Add, Sub, AddAssign, SubAssign};
use bincode::{Encode, Decode};

/// [Display]-friendly byte size type
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[derive(Add, Sub, AddAssign, SubAssign)]
#[derive(Encode, Decode)]
pub struct ByteSize(pub usize);

impl Display for ByteSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 >= 1024 * 1024 {
            write!(f, "{:.1}MiB", self.0 as f64 / 1024f64 / 1024f64)
        } else if self.0 >= 1024 {
            write!(f, "{}KiB", self.0 / 1024)
        } else {
            write!(f, "{}B", self.0)
        }
    }
}

impl Debug for ByteSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
