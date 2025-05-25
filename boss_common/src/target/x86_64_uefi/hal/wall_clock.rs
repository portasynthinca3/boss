//! Reads nanoseconds since boot.
//! 
//! Calibrates the Time-Stamp Counter (Volume 3B, Section 18.17 of the Intel
//! SDM) using the Programmable Interval Timer (Intel 8254 datasheet).

use core::arch::asm;

use super::io_port::Port;

/// How many ticks there are in a microsecond
static mut TICKS_IN_US: Option<usize> = None;

/// Reads the time stamp counter without any serialization
fn rdtsc() -> u64 {
    let rax: u64;
    let rdx: u64;
    // SAFETY: the RDTSC instruction cannot cause any memory errors
    unsafe { asm!("rdtsc", out("rax") rax, out("rdx") rdx); }
    (rax & 0xffff_ffff) | (rdx << 32)
}

/// Reads the time stamp counter with serializing instructions in place, meaning
/// that all loads and stores that happen before and after this function is
/// called will be globally visible.
fn rdtsc_serializing() -> u64 {
    let rax: u64;
    let rdx: u64;
    // SAFETY: the MFENCE, LFENCE and RDTSC instructions cannot cause any memory
    // errors
    unsafe {
        asm!(
            "mfence", "lfence",
            "rdtsc",
            "lfence",
            out("rax") rax, out("rdx") rdx
        );
    }
    (rax & 0xffff_ffff) | (rdx << 32)
}

pub fn calibrate() {
    // SAFETY: this code may cause race conditions. However, it is assumed that
    // the containing codebase does not use the keyboard controller and the PIT
    // while this function 
    let kbd_port_b = unsafe { Port::new(0x61) };
    let pit_mc = unsafe { Port::new(0x43) };
    let pit_ch2 = unsafe { Port::new(0x42) };
}
