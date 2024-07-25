//! Unlike in normal Erlang/OTP, apps exist at the emulator level in this
//! implementation. This is used to strengthen security through emulator-level
//! isolation of applications. This behavior is unwarranted and unexpected in
//! traditional OTP code, and thus a compatibility mode is needed.

use alloc::{rc::Rc, boxed::Box};
use hashbrown::HashMap;

use super::{module::Module, state::{LocalAtomRef, LocalContext}, term::{LocalTerm, TermError}};

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
    pub fn new(data: &[u8], context: &mut LocalContext) -> Result<Application, TermError> {
        // parse ETF
        let term = match LocalTerm::from_etf(data, context) {
            Ok(term) => term,
            Err(e) => return Err(e),
        };

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
}
