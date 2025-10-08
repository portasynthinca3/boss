use core::fmt::Write;
use log::{Record, Level, Metadata};
use spin::Mutex;
use crate::target::current::{
    device::{
        serial::*,
        wall_clock::*
    },
    smp::*,
};

pub struct SerialLogger<'c> {
    port: Mutex<PortWrite<Port>>,
    clock: &'c WallClock,
}

impl SerialLogger<'_> {
    pub fn new(port_number: usize, clock: &WallClock) -> SerialLogger<'_> {
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

    pub fn force_unlock(&self) {
        unsafe { self.port.force_unlock() };
    }
}

impl log::Log for SerialLogger<'_> {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let mut guard = self.port.lock();

            // time
            let time = self.clock.abs_time().us();
            let sec = time / 1_000_000;
            let usec = time % 1_000_000;
            guard.write_fmt(format_args!("[{sec}.{usec:06}")).unwrap();

            // CPU ID
            let cpu = SmpContext::maybe_get()
                .and_then(|ctx| ctx.find_record::<CpuId>());
            if let Some(cpu) = cpu {
                guard.write_fmt(format_args!(" {cpu:02}")).unwrap();
            } else {
                guard.write_str(" ??").unwrap();
            }

            // level
            let level = Self::get_level_string(record.level());
            guard.write_fmt(format_args!(" {level}")).unwrap();

            // code location
            let mod_color = "\x1b[38;5;238m"; // gray
            let module = record.module_path();
            let line = record.line();
            match (module, line) {
                (Some(module), Some(line)) => {
                    let module = module.split("::").last().unwrap();
                    guard.write_fmt(format_args!(" {mod_color}{module}:{line}")).unwrap();
                },
                (Some(module), None) => guard.write_fmt(format_args!(" {mod_color}{module}")).unwrap(),
                _ => (),
            };

            // rest
            let reset = "\x1b[0;0m";
            let args = record.args();
            guard.write_fmt(format_args!("{reset}] {args}\r\n")).unwrap();
        }
    }

    fn flush(&self) {}
}
