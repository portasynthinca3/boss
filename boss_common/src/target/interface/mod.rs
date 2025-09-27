//! Target abstractions.
//! 
//! This module contains platform-independent abstractions for all modules that
//! platform implementations are supposed to implement.

pub mod memmgr;
pub mod device;
pub mod firmware;
pub mod acpi;
pub mod cpu;
pub mod interrupt;
