#![no_std]
#![feature(
    ptr_as_uninit,
    let_chains,
    allocator_api,
    alloc_layout_extra,
    slice_ptr_get,
    ptr_metadata,
    variant_count,
    if_let_guard,
    auto_traits,
    negative_impls,
)]

extern crate alloc;

pub mod target;
pub mod util;
pub mod emu_params;
pub mod tests;
