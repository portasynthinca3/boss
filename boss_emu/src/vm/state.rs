//! Common VM state structures

use core::{fmt::Debug, hash::Hash};
use alloc::rc::Rc;

use hashbrown::HashMap;

use super::{app::Application, scheduler::LocalTransferAgent, term::LocalTerm};

/// Opaque reference to an entry in the atom table. For more information, refer
/// to [LocalTerm::Atom]
#[derive(Clone)]
pub struct LocalAtomRef(usize, Rc<str>);

impl LocalAtomRef {
    pub fn get_str(&self) -> Rc<str> {
        Rc::clone(&self.1)
    }
}

impl From<&LocalAtomRef> for Rc<str> {
    fn from(value: &LocalAtomRef) -> Self {
        Rc::clone(&value.1)
    }
}

// explicit implementation because `Rc` is apparently not `PartialEq`, and we
// don't need to compare it anyways
impl PartialEq for LocalAtomRef {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for LocalAtomRef { }

impl Hash for LocalAtomRef {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0);
    }
}

impl core::fmt::Display for LocalAtomRef {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl core::fmt::Debug for LocalAtomRef {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "LocalAtomRef({})", self)
    }
}

impl PartialOrd for LocalAtomRef {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LocalAtomRef {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        use core::cmp::Ordering;
        if self == other { return Ordering::Equal };
        if self.get_str() > other.get_str() { return Ordering::Greater };
        Ordering::Less
    }
}

/// Two-way garbage collected mapping between local atom identifiers and their
/// values. For more information about atoms, go to [LocalTerm::Atom].
/// 
/// In OTP, atoms are not garbage collected, but they are GC-ed in this
/// implementation due to the expected dynamic nature of constant code loads and
/// unloads that an operating system faces.
#[derive(Default)]
pub struct AtomTable {
    forward_map: HashMap<usize, Rc<str>>,
    backward_map: HashMap<Rc<str>, usize>,
    next_id: usize,
}

impl AtomTable {
    /// Gets a [LocalAtomRef] corresponding to its value
    pub fn get_existing_atom(&self, name: &str) -> Option<LocalAtomRef> {
        let id = self.backward_map.get(name).copied()?;
        let counter = self.forward_map.get(&id)
            .expect("inconsistent atom table state: backward mapping exists, but no forward mapping exists");
        Some(LocalAtomRef(id, Rc::clone(counter)))
    }

    /// Gets a [LocalAtomRef], creating a new entry in the atom table if it
    /// doesn't exist
    pub fn get_or_make_atom(&mut self, name: &str) -> LocalAtomRef {
        if let Some(existing) = self.get_existing_atom(name) {
            return existing;
        }
        
        let id = self.next_id;
        self.next_id += 1;
        let counter = Rc::from(name);
        self.forward_map.insert(id, Rc::clone(&counter));
        self.backward_map.insert(Rc::clone(&counter), id);
        LocalAtomRef(id, counter)
    }

    // /// Performs garbage collection
    // pub fn gc(&mut self) {
    //     let mut freed = 0;
    //     self.forward_map.retain(|_, counter| {
    //         if Rc::weak_count(counter) == 0 {
    //             self.backward_map.remove(&(**counter));
    //             freed += 1;
    //             false
    //         } else {
    //             true
    //         }
    //     });
    //     self.forward_map.shrink_to_fit();
    //     self.backward_map.shrink_to_fit();
    //     log::trace!("atom_gc: freed {freed} atoms");
    // }
}

impl Debug for AtomTable {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut pretty = f.debug_map();
        for (id, name) in self.forward_map.iter() {
            pretty.entry(id, &(name, Rc::strong_count(name) - 2)); // the two references in the table itself
        }
        pretty.finish()
    }
}

/// Context local to a particular scheduler
#[derive(Default)]
pub struct LocalContext {
    pub atom_table: AtomTable,
    pub applications: HashMap<LocalAtomRef, Application>,
    pub next_ref: usize,
    pub messenger: Option<LocalTransferAgent>,
}

impl LocalContext {
    pub fn make_ref(&mut self) -> LocalTerm {
        // TODO: random number as last element
        let r = LocalTerm::Reference([0, self.next_ref, 0]);
        self.next_ref += 1;
        r
    }
}
