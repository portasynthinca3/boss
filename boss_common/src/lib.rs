#![no_std]
#![feature(
    ptr_as_uninit,
    let_chains,
    allocator_api,
    alloc_layout_extra,
    slice_ptr_get,
    ptr_metadata,
)]

extern crate alloc;

pub mod target;
pub mod util;
