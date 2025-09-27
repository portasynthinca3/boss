use core::fmt::Write;
use log::{Record, Level, Metadata};
use spin::Mutex;
use crate::target::interface::device::{serial::{Port as IfPort, PortWrite}, wall_clock::{Duration, WallClock as IfWallClock}};
use crate::target::current::device::{serial::Port, wall_clock::WallClock};

pub struct SerialLogger<'c> {
    port: Mutex<PortWrite<Port>>,
    clock: &'c WallClock,
}

impl SerialLogger<'_> {
    pub fn new(port_number: usize, clock: &WallClock) -> SerialLogger {
        SerialLogger {
            port: Mutex::new(PortWrite(Port::new(port_number))),
            clock,
        }
    }

    const fn get_level_string(level: Level) -> &'static str {
        match level {
            Level::Error => "\x1b[31merr", // red
            Level::Warn =>  "\x1b[33mwrn", // yellow
            Level::Info =>  "\x1b[34mnfo", // blue
            Level::Debug => "\x1b[35mdbg", // magenta
            Level::Trace => "\x1b[37mtrc", // white
        }
    }
}

impl log::Log for SerialLogger<'_> {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            // gather arguments
            let time = self.clock.abs_time().us();
            let sec = time / 1_000_000;
            let usec = time % 1_000_000;
            let level = Self::get_level_string(record.level());
            let args = record.args();
            let module = record.module_path();
            let line = record.line();
            let mod_color = "\x1b[38;5;238m"; // gray
            let reset = "\x1b[0;0m";

            // write!
            let mut guard = self.port.lock();
            match (module, line) {
                (Some(module), Some(line)) => {
                    let module = module.split("::").last().unwrap();
                    guard.write_fmt(format_args!("[{sec}.{usec:06} {level} {mod_color}{module}:{line}{reset}] {args}\r\n"))
                },
                (Some(module), None) =>
                    guard.write_fmt(format_args!("[{sec}.{usec:06} {level} {mod_color}{module}{reset}] {args}\r\n")),
                _ =>
                    guard.write_fmt(format_args!("[{sec}.{usec:06} {level}{reset}] {args}\r\n")),
            }.unwrap();
        }
    }

    fn flush(&self) {}
}
