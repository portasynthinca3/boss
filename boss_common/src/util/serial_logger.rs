use core::fmt::Write;
use log::{Record, Level, Metadata};
use spin::Mutex;
use crate::target::hal::serial::SerialPort;

pub struct SerialLogger {
    port: Mutex<SerialPort>,
}

impl SerialLogger {
    pub fn new(port_number: usize) -> SerialLogger {
        SerialLogger {
            port: Mutex::new(SerialPort::new(port_number)),
        }
    }

    const fn get_level_string(level: Level) -> &'static str {
        match level {
            Level::Error => "\x1b[31m err", // red
            Level::Warn =>  "\x1b[33mwarn", // yellow
            Level::Info =>  "\x1b[34minfo", // blue
            Level::Debug => "\x1b[36mdebg", // cyan
            Level::Trace => "\x1b[37mtrac", // white
        }
    }
}

impl log::Log for SerialLogger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            // gather arguments
            let level = Self::get_level_string(record.level());
            let args = record.args();
            let module = record.module_path();
            let line = record.line();
            let mod_color = "\x1b[38;5;238m"; // gray
            let reset = "\x1b[0;0m";

            // write!
            let mut guard = self.port.lock();
            match (module, line) {
                (Some(module), Some(line)) =>
                    guard.write_fmt(format_args!("[{level} {mod_color}{module}:{line}{reset}] {args}\r\n")),
                (Some(module), None) =>
                    guard.write_fmt(format_args!("[{level} {mod_color}{module}{reset}] {args}\r\n")),
                _ =>
                    guard.write_fmt(format_args!("[{level}{reset}] {args}\r\n")),
            }.unwrap();
        }
    }

    fn flush(&self) {}
}
