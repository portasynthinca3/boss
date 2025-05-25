use core::fmt::Write;
use super::io_port::Port;

/// Base IO ports for the 8 serial ports
const PORT_BASES: [u16; 8] = [0x3f8, 0x2f8, 0x3e8, 0x2e8, 0x5f8, 0x4f8, 0x5e8, 0x4e8];

/// Parity options
#[derive(Clone, Copy)]
#[repr(u8)]
#[allow(dead_code)]
pub enum SerialParity {
    None = 0b000,
    Odd = 0b001,
    Even = 0b011,
    Mark = 0b101,
    Space = 0b111,
}

/// Full serial port configuration
pub struct SerialConfig {
    data_bits: u8,
    two_stop_bits: bool,
    parity: SerialParity,
    baud_rate: u32,
}

/// Serial port structure
pub struct SerialPort {
    io_ports: [Port<u8>; 8],
}

impl SerialPort {
    /// Creates a serial port and applies the default configuration
    pub fn new(number: usize) -> SerialPort {
        // create array of IO ports 
        let base = PORT_BASES[number];
        let io_ports: [Port<u8>; 8] = core::array::from_fn(|i| unsafe { Port::new(base + i as u16) });

        // create port and apply default configuration
        let mut port = SerialPort { io_ports };
        port.configure(&SerialConfig {
            data_bits: 8,
            two_stop_bits: false,
            parity: SerialParity::None,
            baud_rate: 115200
        });

        port
    }

    /// Reconfigures a serial port
    pub fn configure(&mut self, config: &SerialConfig) {
        // set baud rate
        let divider = 115200 / config.baud_rate;
        self.io_ports[3].write(0x80);
        self.io_ports[0].write((divider & 0xff) as u8);
        self.io_ports[1].write((divider >> 8)   as u8);

        // set line control
        let data_bits = match config.data_bits {
            8 => 0b11,
            7 => 0b10,
            6 => 0b01,
            5 => 0b00,
            _ => panic!("incorrect number of data bits ({}) supplied. expected 5-8", config.data_bits)
        };
        let stop_bits = if config.two_stop_bits { 0 } else { 1 };
        let line_control = data_bits | (stop_bits << 2) | ((config.parity as u8) << 3);
        self.io_ports[3].write(line_control);

        // disable all interrupts
        self.io_ports[1].write(0);

        // disable modem control
        self.io_ports[4].write(0);
    }

    /// Writes a byte to a serial port
    pub fn write(&self, data: u8) {
        self.io_ports[0].write(data);
    }
}

impl Write for SerialPort {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            self.write(b);
        }
        Ok(())
    }
}
