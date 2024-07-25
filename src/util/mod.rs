//! Various utility functions

use core::str::{from_utf8, Utf8Error};

pub mod dyn_arr;
pub mod byte_size;
pub mod tar;
pub mod cursor;

/// Converts a null-terminated string representation into a string slice
fn from_null_term(source: &[u8]) -> Result<&str, Utf8Error> {
    let end = source.iter()
        .position(|&c| c == 0)
        .unwrap_or(source.len());
    from_utf8(&source[0..end])
}
