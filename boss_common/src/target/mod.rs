#[cfg(any(boss_target = "x86_64-uefi", test))]
include!("x86_64_uefi/mod.rs");
