//! Low-level abstractions for platform-specific components.
//! 
//! This module is `pub(crate)`. Only the HAL in this crate is expected to use
//! this module.

pub mod io_port;
pub mod msr;
pub mod descriptors;
pub mod apic;
