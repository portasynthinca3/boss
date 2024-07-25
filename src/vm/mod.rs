//! The Erlang Virtual Machine. The thing that this entire project (which was
//! 3000 lines long at the time of creation of this module) strives to
//! implement. Everything else around this module and its descendants is more or
//! less just a generic microkernel.

use alloc::{format, rc::Rc};

use app::Application;
use hashbrown::HashMap;
use port::LogPort;
use interpreter::{BeamInterpreter, BeamInterpreterMakeError};
use module::{Module, LoadError};
use scheduler::{PrimitiveScheduler, Schedule};
use state::LocalContext;
use term::{LocalTerm, MapTerm, TermError};
use crate::util::tar::TarFile;

pub const CURRENT_OPCODE_MAX: usize = 178;

pub mod term;
pub mod module;
pub mod scheduler;
pub mod interpreter;
pub mod app;
pub mod state;
pub mod port;

/// Virtual machine initialization error
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InitError {
    /// No `ebin/base.app` file in base image
    NoBaseApp,
    /// No `ebin/{module}.beam` file as requested by the app descriptor
    NoBaseModule,
    /// Bad data in `ebin/base.app`
    BadSpec(TermError),
    /// Bad data in `ebin/{module}.beam`
    BadModule(LoadError),
    /// Interpreter initialization error
    Interpreter(BeamInterpreterMakeError)
}
impl From<TermError> for InitError {
    fn from(value: TermError) -> Self {
        Self::BadSpec(value)
    }
}
impl From<LoadError> for InitError {
    fn from(value: LoadError) -> Self {
        Self::BadModule(value)
    }
}
impl From<BeamInterpreterMakeError> for InitError {
    fn from(value: BeamInterpreterMakeError) -> Self {
        Self::Interpreter(value)
    }
}

/// Initializes an Erlang virtual machine given a base image
pub fn init(base_image: &TarFile) -> Result<!, InitError> {
    // create context
    let mut context: LocalContext = Default::default();
    let base_atom = context.atom_table.get_or_make_atom("base");
    let platform_atom = context.atom_table.get_or_make_atom("platform");
    let log_port_atom = context.atom_table.get_or_make_atom("log_port");
    let x86_64_uefi_atom = context.atom_table.get_or_make_atom("x86_64-uefi");
    let main_atom = context.atom_table.get_or_make_atom("main");

    // load base application and modules
    let base_app = base_image.read_file("ebin/base.app").ok_or(InitError::NoBaseApp)?;
    let mut base_app = Application::new(base_app, &mut context)?;
    for (name, module) in base_app.modules.iter_mut() {
        let path = format!("ebin/{name}.beam");
        let module_data = base_image.read_file(path.as_str()).ok_or(InitError::NoBaseModule)?;
        *module = Some(Rc::new(Module::new(module_data, &mut context)?));
    }
    context.applications.insert(base_atom.clone(), base_app);

    // create a scheduler and initial ports
    let mut scheduler = PrimitiveScheduler::new(context, 0);
    let log_port = scheduler.add::<LogPort>(&(), 0).unwrap();

    // construct initial arguments to 'base:main':main/2
    let config = LocalTerm::Map(MapTerm(HashMap::from([
        (platform_atom.into(), x86_64_uefi_atom.into())
    ])));
    let ports = LocalTerm::Map(MapTerm(HashMap::from([
        (log_port_atom.into(), log_port.into())
    ])));

    // Run 'base:main':main/2
    let arguments: &[LocalTerm] = &[config, ports];
    let interpreter_init = (base_atom.clone(), main_atom.clone(), main_atom.clone(), arguments);
    scheduler.add::<BeamInterpreter>(&interpreter_init, 0)?;

    // here we go!
    scheduler.run();
}
