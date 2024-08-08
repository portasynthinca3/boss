#![allow(unused)]
#![cfg_attr(any(target_os = "none", target_os = "uefi"), no_std, no_main)]

#[cfg_attr(any(target_os = "none", target_os = "uefi"), panic_handler)]
fn panic(_info: &::core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

#[cfg_attr(any(target_os = "none", target_os = "uefi"), export_name = "efi_main")]
fn main() {}
