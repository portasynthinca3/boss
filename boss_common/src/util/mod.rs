//! Various utility functions

use core::str::{from_utf8, Utf8Error};

pub mod dyn_arr;
pub mod byte_size;
pub mod tar;
pub mod cursor;
pub mod serial_logger;
pub mod elf;

/// Converts a null-terminated string representation into a string slice
pub fn from_null_term(source: &[u8]) -> Result<&str, Utf8Error> {
    let end = source.iter()
        .position(|&c| c == 0)
        .unwrap_or(source.len());
    from_utf8(&source[0..end])
}

/// Implies that two types are of the same size.
/// 
/// # Safety
/// `convert_slice_in_place` depends on this trait to only allow safe
/// operations. If this trait is implemented for types that aren't actually
/// equally sized, using that function is unsafe. To avoid making any mistakes,
/// use the `impl_equally_sized` macro, which will check that this is upheld at
/// compile time.
pub unsafe trait EquallySized<T> {
    const _DUMMY: [(); 1];
}

#[macro_export]
macro_rules! impl_equally_sized {
    ($t:ty, $u:ty) => {
        unsafe impl $crate::util::EquallySized<$t> for $u {
            const _DUMMY: [(); 1] = [(); (core::mem::size_of::<$t>() == core::mem::size_of::<$u>()) as usize];
        }
        unsafe impl $crate::util::EquallySized<$u> for $t {
            const _DUMMY: [(); 1] = [(); 1];
        }
    }
}

/// Converts a slice of `T` into a slice of `U` in place
pub fn convert_slice_in_place<T, U>(source: &mut [T]) -> &mut [U]
where
    T: Sized,
    U: Sized,
    T: Into<U>,
    T: EquallySized<U>,
{
    let source_ptr = source.as_mut_ptr();
    let dest_ptr = source_ptr as *mut U;

    for i in 0..source.len() {
        unsafe {
            let source_element = source_ptr.add(i).read();
            let dest_element = source_element.into();
            dest_ptr.add(i).write(dest_element);
        }
    }

    unsafe { core::slice::from_raw_parts_mut(dest_ptr, source.len()) }
}
