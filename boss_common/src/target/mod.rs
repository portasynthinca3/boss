//! Platform (target) abstractions and implementations.
//! 
//! This module has two main submodules:
//!   - [`interface`] contains a) traits that target implementations must
//!     implement, and b) generic implementations of things that are generally
//!     available on and work well across targets;
//!   - [`current`] contains target-specific definitions for the target that
//!     this crate is being currently compiled for, as selected by the
//!     `boss_target` config attribute.
//! 
//! When importing things from this module, you need to import both a) the
//! trait, so that its methods are usable, and b) the implementation itself.
//! Components from the [`current`] module will re-export appropriate items
//! from the [`interface`] module to bring them in scope, with the following
//! renaming convention:
//!   - traits from the [`interface`] submodule are re-exported under an alias
//!     beginning with an `If` (short for "interface");
//!   - structs from the [`interface`] submodule are re-exported under their
//!     actual names.
//! 
//! This is done for end user importing convenience. Here's an example:
//! ```
//! use boss_common::target::current::{
//!     device::{
//!         wall_clock::*,
//!     },
//!     memmgr::*,
//!     firmware::*,
//! };
//! ```
//! 
//! When porting to a new target, implement basic interfaces in the following
//! order:
//!   - [`interface::cpu`]
//!   - [`interface::device::wall_clock`] (for the serial logger)
//!   - [`interface::device::serial`] (for the serial logger)
//!   - [`interface::memmgr`] (for pretty much everything else)
//!   - [`interface::device::video`] (for firmware)
//!   - [`interface::acpi`] (for firmware)
//!   - [`interface::firmware`] (for entry code)
//! 
//! The modules above should be sufficient to compile the bootloader. For the
//! emulator to work, implement the rest of the modules.

pub mod interface;
pub mod runtime_cfg;

pub mod current {
    #[cfg(any(boss_target = "x86_64-uefi", test))]
    include!("x86_64_uefi/mod.rs");
}
