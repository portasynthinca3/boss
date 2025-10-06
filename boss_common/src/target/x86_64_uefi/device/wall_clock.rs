//! Calibrates the Time-Stamp Counter (Volume 3B, Section 19.17 of the Intel
//! SDM) using the Programmable Interval Timer (Intel 8254 datasheet) and
//! Programmable Interrupt Controller (Intel 8259 datasheet).

use bincode::{Encode, Decode};

use core::arch::asm;

pub use crate::target::interface::device::wall_clock::{
    Duration,
    WallClock as IfWallClock,
};
use crate::target::current::ll::io_port::Port;

#[derive(Debug, Clone, Copy)]
#[derive(Encode, Decode)]
pub struct WallClock {
    /// Clock initialization time
    tsc_tick_offset: u64,
    /// TSC rate in ticks per uS
    tsc_ticks_per_us: u64,
}

#[allow(unused)]
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

impl IfWallClock for WallClock {
    #[allow(clippy::unusual_byte_groupings)]
    fn calibrate_new() -> Self {
        let tsc_at_call_time = rdtsc_serializing();

        // SAFETY: this code may cause race conditions. However, it is assumed that
        // the containing codebase does not use the PIC and the PIT while this
        // function is running
        let pic_master = unsafe { Port::new(0x21) };
        let pic_slave = unsafe { Port::new(0xa1) };
        let pit_mc = unsafe { Port::new(0x43) };
        let pit_ch0 = unsafe { Port::new(0x40) };

        const PIT_FREQ: u64 = 1193182;
        const CALIB_TIME_USEC: u64 = 1000;
        const CALIB_TIME_PIT_CYCLES: u64 = PIT_FREQ / CALIB_TIME_USEC;

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

        WallClock {
            tsc_tick_offset: tsc_at_call_time,
            tsc_ticks_per_us: ticks_per_us,
        }
    }

    fn new_with_calib(calib_source: &Self) -> Self {
        Self {
            tsc_tick_offset: rdtsc_serializing(),
            tsc_ticks_per_us: calib_source.tsc_ticks_per_us,
        }
    }

    fn resolution_ps(&self) -> u64 {
        1_000_000 / self.tsc_ticks_per_us
    }

    fn abs_time(&self) -> Duration {
        let ps = ((rdtsc_serializing() as u128) - (self.tsc_tick_offset as u128)) * 1_000_000u128 / (self.tsc_ticks_per_us as u128);
        Duration(ps)
    }

    fn delta_time<T>(&self, f: impl FnOnce() -> T) -> (T, Duration) {
        let start = self.abs_time();
        let ret = f();
        let end = self.abs_time();
        (ret, Duration(end.ps() - start.ps()))
    }

    fn delay(&self, duration: Duration) {
        let end = Duration(self.abs_time().0 + duration.0);
        while self.abs_time() < end { }
    }
}
