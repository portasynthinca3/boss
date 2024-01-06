use core::{
    fmt::{Display, Formatter, Result},
    ops::AddAssign
};

pub mod phys;

type PhysAddr = usize;
type VirtAddr = usize;

#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ByteSize(usize);

impl AddAssign<usize> for ByteSize {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Display for ByteSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.0 >= 1024 * 1024 {
            write!(f, "{} MiB", self.0 / 1024 / 1024)
        } else if self.0 >= 1024 {
            write!(f, "{} KiB", self.0 / 1024)
        } else {
            write!(f, "{} bytes", self.0)
        }
    }
}
