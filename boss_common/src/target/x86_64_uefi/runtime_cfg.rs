//! Some components of the emulator support operation in different modes
//! depending on what status the system is in. This module enables this global
//! configuration tracking.

use spin::RwLock;
use bitflags::bitflags;

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct CfgFlags: u64 {
        const ExecutingInUpperHalf = (1 << 0);
    }
}

static FLAGS: RwLock<CfgFlags> = RwLock::new(CfgFlags::empty());

/// Sets one or multiple configuration flags. It is expected that 
pub fn set_flags(mask: CfgFlags, value: bool) {
    let mut guard = FLAGS.write();
    guard.set(mask, value);
}

/// Gets the current flags
pub fn get() -> CfgFlags {
    let guard = FLAGS.read();
    *guard
}
