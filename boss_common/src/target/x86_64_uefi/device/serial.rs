//! Implements the Intel 8250 UART chip

use crate::target::current::ll::io_port::Port as IoPort;
pub use crate::target::interface::device::serial::{
    Config,
    Parity,
    DataBits,
    StopBits,
    DEFAULT_CONFIG,
    Port as IfPort
};

const SERIAL_PORT_CNT: usize = 8;
const IO_PORTS_PER_SERIAL_PORT: usize = 8;

/// Base IO ports for the 8 serial ports
const PORT_BASES: [u16; SERIAL_PORT_CNT] = [0x3f8, 0x2f8, 0x3e8, 0x2e8, 0x5f8, 0x4f8, 0x5e8, 0x4e8];

/// Serial port structure
pub struct Port {
    io_ports: [IoPort<u8>; IO_PORTS_PER_SERIAL_PORT],
}

impl IfPort for Port {
    const PORT_CNT: usize = SERIAL_PORT_CNT;

    /// Creates a serial port and applies the default configuration
    fn new(number: usize) -> Self {
        let base = PORT_BASES[number];
        let io_ports = core::array::from_fn(|i| unsafe { IoPort::new(base + i as u16) });

        let mut port = Port { io_ports };
        port.configure(DEFAULT_CONFIG);

        port
    }

    /// Configures the serial port
    fn configure(&mut self, config: Config) {
        // set baud rate
        let divider = 115200 / config.baud_rate;
        self.io_ports[3].write(0x80);
        self.io_ports[0].write((divider & 0xff) as u8);
        self.io_ports[1].write((divider >> 8)   as u8);

        // set line control
        let data_bits = match config.data_bits {
            DataBits::Eight => 0b11,
            DataBits::Seven => 0b10,
            DataBits::Six => 0b01,
            DataBits::Five => 0b00,
        };
        let stop_bits = match config.stop_bits {
            StopBits::One => 1,
            StopBits::OneAndHalf => panic!("1.5 stop bits not supported"),
            StopBits::Two => 0,
        };
        let parity = match config.parity {
            Parity::None => 0b000,
            Parity::Odd => 0b001,
            Parity::Even => 0b011,
            Parity::Mark => 0b101,
            Parity::Space => 0b111,
        };
        let line_control = data_bits | (stop_bits << 2) | (parity << 3);
        self.io_ports[3].write(line_control);

        // disable all interrupts
        self.io_ports[1].write(0);

        // disable modem control
        self.io_ports[4].write(0);
    }

    /// Writes data to the serial port
    fn write(&mut self, data: &[u8]) {
        for byte in data {
            self.io_ports[0].write(*byte);
        }
    }
}
