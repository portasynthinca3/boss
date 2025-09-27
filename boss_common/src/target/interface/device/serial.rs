//! Generic interface for a serial port

use core::{fmt::Write, ops::{Deref, DerefMut}};

/// Number of data bits (not counting start, parity and stop bits)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataBits {
    Eight,
    Seven,
    Six,
    Five,
}

/// Length of stop bit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StopBits {
    /// 1 bit time
    One,
    /// 1.5 bit times
    OneAndHalf,
    /// 2 bit times
    Two,
}

/// Serial port parity bit
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Parity {
    /// No parity bit
    None,
    /// Odd number of 1s in frame
    Odd,
    /// Even number of 1s in frame
    Even,
    /// Parity bit is always 1
    Mark,
    /// Parity bit is always 0
    Space,
}

/// Complete serial port configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub data_bits: DataBits,
    pub stop_bits: StopBits,
    pub parity: Parity,
    /// Baud rate (in bit times per second)
    pub baud_rate: u32,
}

/// Default serial port config
pub const DEFAULT_CONFIG: Config = Config {
    data_bits: DataBits::Eight,
    stop_bits: StopBits::One,
    parity: Parity::None,
    baud_rate: 230400,
};

/// Serial port
pub trait Port {
    /// Total number of serial ports in system
    const PORT_CNT: usize;

    /// Creates a new serial port and applies the default configuration
    fn new(number: usize) -> Self;
    /// Configures the serial port
    fn configure(&mut self, config: Config);
    /// Writes data to the serial port
    fn write(&mut self, data: &[u8]);
}

pub struct PortWrite<T: Port>(pub T);

impl<T: Port> Write for PortWrite<T> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for byte in s.bytes() {
            self.write(&[byte]);
        }
        Ok(())
    }
}

impl<T: Port> Deref for PortWrite<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T: Port> DerefMut for PortWrite<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
