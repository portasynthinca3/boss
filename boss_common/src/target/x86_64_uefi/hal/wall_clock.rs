//! Reads nanoseconds since boot.
//! 
//! Calibrates the Time-Stamp Counter (Volume 3B, Section 19.17 of the Intel
//! SDM) using the Programmable Interval Timer (Intel 8254 datasheet) and
//! Programmable Interrupt Controller (Intel 8259 datasheet).

use core::arch::asm;
use spin::Once;

use super::io_port::Port;

// TSC offset, TSC ticks per uS
static TSC_CALIBRATION: Once<(usize, usize)> = Once::new();

/// Reads the time stamp counter without any serialization
fn rdtsc() -> usize {
    let rax: usize;
    let rdx: usize;
    // SAFETY: the RDTSC instruction cannot cause any memory errors
    unsafe { asm!("rdtsc", out("rax") rax, out("rdx") rdx); }
    (rax & 0xffff_ffff) | (rdx << 32)
}

/// Reads the time stamp counter with serializing instructions in place, meaning
/// that all loads and stores that happen before and after this function is
/// called will be globally visible.
fn rdtsc_serializing() -> usize {
    let rax: usize;
    let rdx: usize;
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

#[allow(clippy::unusual_byte_groupings)]
pub fn calibrate() {
    // SAFETY: this code may cause race conditions. However, it is assumed that
    // the containing codebase does not use the PIC and the PIT while this
    // function is running
    let pic_master = unsafe { Port::new(0x21) };
    let pic_slave = unsafe { Port::new(0xa1) };
    let pit_mc = unsafe { Port::new(0x43) };
    let pit_ch0 = unsafe { Port::new(0x40) };

    const PIT_FREQ: usize = 1193182;
    const CALIB_TIME_USEC: usize = 1000;
    const CALIB_TIME_PIT_CYCLES: usize = PIT_FREQ / CALIB_TIME_USEC;

    // disable PIC
    pic_master.write(0xffu8);
    pic_slave.write(0xffu8);

    // initialize PIT
    pit_mc.write(0b_00_11_000_0_u8); // ch0, lo/hi, mode 0, binary
    pit_ch0.write((CALIB_TIME_PIT_CYCLES & 0xff) as u8);
    pit_ch0.write(((CALIB_TIME_PIT_CYCLES >> 8) & 0xff) as u8);

    let tsc_before_calib = rdtsc_serializing();
    loop {
        // read PIT ch0 output
        pit_mc.write(0b_11_1_0_001_0_u8); // read back, w/o count, w/ status, ch0 only
        let status: u8 = pit_ch0.read();
        if (status >> 7) == 1 {
            break;
        }
    }
    let tsc_after_calib = rdtsc_serializing();

    let ticks_per_us = (tsc_after_calib - tsc_before_calib) / CALIB_TIME_USEC;
    let calibration = (tsc_before_calib, ticks_per_us);

    if *TSC_CALIBRATION.call_once(|| calibration) != calibration {
        panic!("cannot initialize wall clock more than once");
    }
}

pub fn get_us_since_boot(serializing: bool) -> usize {
    let tsc = if serializing { rdtsc_serializing() } else { rdtsc() };
    let (offset, rate) = TSC_CALIBRATION.get().unwrap();
    (tsc - offset) / rate
}
