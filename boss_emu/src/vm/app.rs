//! Unlike in normal Erlang/OTP, apps exist at the emulator level in this
//! implementation. This is used to strengthen security through emulator-level
//! isolation of applications. This behavior is unwarranted and unexpected in
//! traditional OTP code, and thus a compatibility mode is needed.

use alloc::{boxed::Box, format, rc::Rc};

use hashbrown::HashMap;

use super::{module::{self, Module}, state::{LocalAtomRef, LocalContext}, term::{LocalTerm, TermError}};
use boss_common::util::tar::TarFile;

/// Application package load error
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LoadError {
    /// No app metadata file in app package
    NoSpec,
    /// Invalid app metadata file in app package
    BadSpec(TermError),
    /// No module in app package
    NoModule(LocalAtomRef),
    /// Invalid module data in app package
    BadModule(LocalAtomRef, module::LoadError),
}

/// Named collection of related modules
/// 
/// In OTP, there can only be one instance of an app. In BOSS, multiple versions
/// of an app could be loaded at the same time, and multiple instances if each
/// of those versions could be running at the same time.
#[derive(Debug)]
pub struct Application {
    /// Name of application as an atom
    pub name: LocalAtomRef,
    /// Version of application
    pub version: Box<str>,
    /// Modules belonging to this version of the application. A module may not
    /// yet be loaded.
    pub modules: HashMap<LocalAtomRef, Option<Rc<Module>>>,
}

impl Application {
    /// Parses a binary `.app` specification
    pub fn from_app_file(data: &[u8], context: &mut LocalContext) -> Result<Application, TermError> {
        // parse ETF
        let term = LocalTerm::from_etf(data, context)?;

        // deconstruct term
        let [name, properties] = term.get_tagged_tuple("application", context)?;
        let name = name.get_atom()?;
        let module_names = properties.get_proplist_value("modules", context)?.get_list()?;
        let version = properties.get_proplist_value("vsn", context)?.get_charlist()?;

        let mut modules = HashMap::with_capacity(module_names.len());
        for name in module_names {
            let name = name.get_atom()?;
            modules.insert(name, None);
        }

        Ok(Application { name, version, modules })
    }

    /// Parses a BOSS package (`.bop`) application file
    pub fn from_bop_file(data: &[u8], context: &mut LocalContext) -> Result<Application, LoadError> {
        let package = TarFile::new(data);
        let spec = package.read_file("ebin/app").ok_or(LoadError::NoSpec)?;
        let mut app = Self::from_app_file(spec, context).map_err(LoadError::BadSpec)?;

        for (name, module) in app.modules.iter_mut() {
            let name_str = name.get_str();
            let path = format!("ebin/{name_str}.beam");
            let data = package.read_file(path.as_str()).ok_or(LoadError::NoModule(name.clone()))?;
            *module = Some(Module::new(data, context).map_err(|e| LoadError::BadModule(name.clone(), e))?.into());
        }

        Ok(app)
    }
}
