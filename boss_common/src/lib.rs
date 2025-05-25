#![no_std]
#![feature(
    ptr_as_uninit,
    let_chains,
    naked_functions,
    allocator_api,
    alloc_layout_extra,
    slice_ptr_get,
)]

extern crate alloc;

pub mod target;
pub mod util;
