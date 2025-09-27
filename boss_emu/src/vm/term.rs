//! Erlang Term implementation

#[allow(deprecated)]
use core::hash::SipHasher;

use core::{array::TryFromSliceError, hash::{Hash, Hasher}};
use alloc::{boxed::Box, string::String, vec::Vec};

use hashbrown::HashMap;
use num_bigint::BigInt;
use strum_macros::FromRepr;

use super::{scheduler::{Eid, PORT_START}, state::{LocalAtomRef, LocalContext}};
use boss_common::util::cursor::Cursor;

// /// An `f64` that implements `Eq`, `Ord` and `Hash`. Yes, I'm aware of the
// /// dangers.
// #[derive(Debug, Clone, Copy, PartialOrd)]
// pub struct FloatTerm(f64);
// impl FloatTerm {
//     fn key(&self) -> u64 {
//         self.0.to_bits()
//     }
// }
// impl Hash for FloatTerm {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.key().hash(state)
//     }
// }
// impl PartialEq for FloatTerm {
//     fn eq(&self, other: &FloatTerm) -> bool {
//         self.0 == other.0
//     }
// }
// impl Eq for FloatTerm {}
// impl Ord for FloatTerm {
//     fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        
//     }
// }
// impl Deref for FloatTerm {
//     type Target = f64;
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
// impl core::fmt::Display for FloatTerm {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

/// A map between local terms
#[derive(Clone, PartialEq, Eq)]
pub struct MapTerm(pub HashMap<LocalTerm, LocalTerm>);

impl Hash for MapTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut vec = self.0.iter().collect::<Vec<_>>();
        vec.sort();
        for (k, v) in vec {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl core::fmt::Debug for MapTerm {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut pretty = f.debug_map();
        for (k, v) in self.0.iter() {
            pretty.entry(k, v);
        }
        pretty.finish()
    }
}

impl PartialOrd for MapTerm {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MapTerm {
    fn cmp(&self, _other: &Self) -> core::cmp::Ordering {
        todo!()
    }
}

/// An Erlang term that is local to a particular logical CPU. It assumes some
/// context which needs to be communicated explicitly when a term is shared with
/// another logical CPU or over a network.
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum LocalTerm {
    /// Integer. All integers support long arithmetic.
    Integer(BigInt),
    /// Double-precision floating point number.
    // Float(FloatTerm),
    /// Usually short immutable string that has two main purposes in Erlang: as
    /// a flag used for signaling something (`ok` and `error` are popular) and
    /// as names for functions and modules. Comparing two atoms should be as
    /// fast as possible, so they're stored as small unique identifiers instead
    /// of full strings.        
    Atom(LocalAtomRef),
    /// A term that is unique among all processors and nodes in the system. It
    /// is opaque and unforgeable - in Erlang code there is no way to get the
    /// constituent parts of a reference or create one from its components.
    /// This property makes references useful as tokens for capability-based
    /// security.
    /// 
    /// The three constituent parts in this implementation are:
    ///   - Node ID
    ///   - Scheduler-local sequence number
    ///   - Random number
    Reference([usize; 3]),
    // TODO: fun
    /// Port identifier. A port is like a process in the sense that one can send
    /// and receive messages to/from a port. The difference is that a process
    /// runs Erlang code and a port is an abstraction for talking to the outside
    /// world implemented using native code.
    Pid(Eid),
    /// Process identifier.
    Port(Eid),
    /// Tuple.
    Tuple(Vec<LocalTerm>),
    /// List. Normally represented as a cons list, in this implementation
    /// represented as a slice of all the elements and an improper tail. Erlang
    /// allows placing terms that are not a list in the tail of a list,
    /// producing an abomination called an "improper list".
    List(Box<[LocalTerm]>, Option<Box<LocalTerm>>),
    /// Mapping between arbitrary terms
    Map(MapTerm),
    /// A binary array that has a bit length that is not necessarily a multiple
    /// of 8.
    BitString(usize, Vec<u8>),
}

impl core::fmt::Debug for LocalTerm {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use LocalTerm::*;
        match *self {
            Atom(ref reference) => {
                let str: alloc::rc::Rc<str> = reference.into();
                write!(f, "{str}")
            },
            Integer(ref int) => write!(f, "{int}"),
            // Float(float) => write!(f, "{float}"),
            BitString(len, ref data) => {
                // try to print it as a UTF-8 string first
                if len % 8 == 0 && let Ok(utf8) = core::str::from_utf8(data) {
                    write!(f, "<<\"{utf8}\">>")
                } else {
                    write!(f, "<<")?;
                    for byte in &data[0 .. data.len() - 1] {
                        write!(f, "{byte}, ")?;
                    }
                    let last_bits = len % 8;
                    let last_byte = data[data.len() - 1];
                    if last_bits == 0 {
                        write!(f, "{last_byte}")?;
                    } else {
                        write!(f, "{last_byte}:{last_bits}")?;
                    }
                    write!(f, ">>")
                }
            }
            Reference(parts) => {
                #[allow(deprecated)]
                let mut hash = SipHasher::default();
                parts.hash(&mut hash);
                write!(f, "#Ref<{:08x}>", hash.finish() & 0xffffffff)
            },
            Pid(Eid(sched, seq)) => write!(f, "<{sched}.{seq}>"),
            Port(Eid(sched, seq)) => write!(f, "#Port<{sched}.{seq}>"),
            Tuple(ref tuple) => {
                write!(f, "{{")?;
                for (idx, elem) in tuple.iter().enumerate() {
                    write!(f, "{elem:?}")?;
                    if idx != tuple.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            },
            List(ref elements, ref improper_tail) => {
                // try to print it as a string first
                let is_probable_charlist = elements.iter().all(|elem| {
                    let LocalTerm::Integer(int) = elem else { return false };
                    let Ok(int): Result<usize, _> = int.try_into() else { return false };
                    (32..=127).contains(&int)
                });

                if is_probable_charlist {
                    write!(f, "\"")?;
                    for char in elements.iter() {
                        // validity of this data was ensured beforehand
                        let LocalTerm::Integer(char) = char else { panic!("invariant violated") };
                        let Ok(char): Result<u32, _> = char.try_into() else { panic!("invariant violated") };
                        let char = core::char::from_u32(char).expect("invariant violated");
                        write!(f, "{char}")?;
                    }
                    write!(f, "\"")
                } else {
                    // normal (could be improper) list
                    write!(f, "[")?;
                    for (idx, elem) in elements.iter().enumerate() {
                        write!(f, "{elem:?}")?;
                        if idx != elements.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    if let Some(tail) = improper_tail {
                        write!(f, " | {tail:?}")?;
                    }
                    write!(f, "]")
                }
            },
            LocalTerm::Map(ref map) => write!(f, "{map:?}"),
        }
    }
}

impl LocalTerm {
    pub fn nil() -> LocalTerm {
        Self::List(Box::new([]), None)
    }
}

impl From<BigInt> for LocalTerm {
    fn from(value: BigInt) -> Self {
        Self::Integer(value)
    }
}
impl From<isize> for LocalTerm {
    fn from(value: isize) -> Self {
        Self::Integer(value.into())
    }
}

impl From<LocalAtomRef> for LocalTerm {
    fn from(value: LocalAtomRef) -> Self {
        Self::Atom(value)
    }
}

impl From<Eid> for LocalTerm {
    fn from(value: Eid) -> Self {
        if value.1 >= PORT_START {
            Self::Port(value)
        } else {
            Self::Pid(value)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, FromRepr, Debug)]
pub enum EtfTag {
    SmallInteger = 97,
    Integer = 98,
    Float = 99,
    Port = 102,
    NewPort = 89,
    V4Port = 120,
    Pid = 103,
    NewPid = 88,
    SmallTuple = 104,
    LargeTuple = 105,
    Map = 116,
    Nil = 106,
    String = 107,
    List = 108,
    Binary = 109,
    SmallBigint = 110,
    LargeBigint = 111,
    NewReference = 114,
    NewerReference = 90,
    Fun = 117,
    NewFun = 112,
    Export = 113,
    BitBinary = 77,
    NewFloat = 70,
    AtomUtf8 = 118,
    SmallAtomUtf8 = 119,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TermError {
    /// A feature is not implemented
    NotImplemented,
    /// An ETF opcode is not implemented
    EtfNotImplemented(EtfTag),
    /// Invalid data in ETF encoding
    FormatError,
    /// Term was not of expected type when deconstructing
    TypeError,
    /// Term had an unexpected length when deconstructing
    LengthError,
    /// Tuple had unexpected tag when deconstructing
    TagError,
    /// No value associated with given key in proplist
    KeyError,
}

/// Convenience functions for deconstructing terms
impl LocalTerm {
    /// Returns a slice representing a tuple
    /// ```
    /// let term = make_term!({1, 2});
    /// let [one, two] = term.get_tuple();
    /// assert_eq!(one, LocalTerm::Integer(1));
    /// assert_eq!(two, LocalTerm::Integer(2));
    /// ```
    pub fn get_tuple<const S: usize>(&self) -> Result<&[LocalTerm; S], TermError> {
        if let Self::Tuple(vec) = self {
            vec.as_slice().try_into().map_err(|_: TryFromSliceError| TermError::LengthError)
        } else {
            Err(TermError::TypeError)
        }
    }

    /// Returns a slice representing the fields of a tagged tuple
    /// ```
    /// let term = make_term!({ok, 1});
    /// let [one] = term.get_tagged_tuple("ok");
    /// assert_eq!(one, LocalTerm::Integer(1));
    /// ```
    pub fn get_tagged_tuple<const S: usize>(&self, tag: &str, context: &LocalContext) -> Result<&[LocalTerm; S], TermError>
    where [(); S + 1]: Sized
    {
        let tuple: &[LocalTerm; S + 1] = self.get_tuple()?;

        let expected_tag = context.atom_table.get_existing_atom(tag)
            .ok_or(TermError::TagError)?; // if the key that we're looking for is not in the atom table,
                                          // it's not in any possible term (including the tag of our tuple)
        let tag = tuple[0].get_atom()?;
        if tag != expected_tag {
            return Err(TermError::TagError);
        }

        Ok((&tuple[1 .. tuple.len()]).try_into().unwrap())
    }

    /// Returns an atom reference if the term is an atom
    pub fn get_atom(&self) -> Result<LocalAtomRef, TermError> {
        if let Self::Atom(atom) = self {
            Ok(atom.clone())
        } else {
            Err(TermError::TypeError)
        }
    }

    /// Returns an slice representing the elements of a proper list
    pub fn get_list(&self) -> Result<&[LocalTerm], TermError> {
        if let Self::List(elements, None) = self {
            Ok(&**elements)
        } else {
            Err(TermError::TypeError)
        }
    }

    /// Returns the value associated with a key in a proplist
    /// ```
    /// let term = make_term!([{key1, 1}, {key2, 2}]);
    /// let two = term.get_proplist_value("key2");
    /// assert_eq!(two, LocalTerm::Integer(2));
    /// ```
    pub fn get_proplist_value(&self, key: &str, context: &LocalContext) -> Result<&LocalTerm, TermError> {
        let list = self.get_list()?;
        let tag = context.atom_table.get_existing_atom(key)
            .ok_or(TermError::KeyError)?; // if the key that we're looking for is not in the atom table,
                                          // it's not in any possible term (including the one we're searching through)
        for element in list.iter() {
            let [key, value] = element.get_tuple()?;
            let key = key.get_atom()?;
            if key == tag {
                return Ok(value);
            }
        }
        Err(TermError::KeyError)
    }

    /// Returns the string value that a charlist represents
    pub fn get_charlist(&self) -> Result<Box<str>, TermError> {
        let chars = self.get_list()?;
        let mut string = String::new();
        for element in chars {
            let LocalTerm::Integer(int) = element else { return Err(TermError::TypeError) };
            let int = int.try_into().map_err(|_| TermError::TypeError)?;
            let char = core::char::from_u32(int).ok_or(TermError::TypeError)?;
            string.push(char);
        }
        Ok(string.into_boxed_str())
    }
}

/// Constructs a term using the Erlang syntax
/// ```
/// let first_term = make_term!({hello, world});
/// let term = make_term!({some_atom, 1, (@first_term)});
/// assert_eq!(term, LocalTerm::Tuple(vec![
///     LocalTerm::atom("some_atom"),
///     LocalTerm::Integer(1),
///     LocalTerm::Tuple(vec![
///         LocalTerm::atom("hello"),
///         LocalTerm::atom("world"),
///     ]),
/// ]));
/// ```
#[macro_export]
macro_rules! local_term {
    ((@$sub_term:expr)) => { $sub_term };
    ($atom:ident) => {
        { LocalTerm::atom(stringify!(atom)) }
    };
    ($literal:literal) => {
        {
            let term: LocalTerm = $literal.into();
            term
        }
    };
    ({$element:tt}) => {
        {
            use alloc::vec; 
            LocalTerm::Tuple(vec![make_term!($element)])
        }
    };
    ({$first:tt, $($rest:tt),+}) => {
        {
            use alloc::vec;
            let mut vector = vec![make_term!($first)];
            let LocalTerm::Tuple(mut rest) = make_term!({$($rest),+}) else {
                panic!("internal error: make_term!({{...}}) returned something other than LocalTerm::Tuple")
            };
            vector.append(&mut rest);
            LocalTerm::Tuple(vector)
        }
    };
}

/// ETF (External Term Format, the standard binary serialization format in the
/// BEAM world) implementation
impl LocalTerm {
    /// Creates a [LocalTerm] from its ETF (External Term Format)
    /// representation. Data is passed as a slice of bytes.
    pub fn from_etf(etf: &[u8], context: &mut LocalContext) -> Result<LocalTerm, TermError> {
        let mut cursor = Cursor::new(etf);
        Self::from_etf_cursor(&mut cursor, context)
    }

    /// Creates a [LocalTerm] from its ETF (External Term Format)
    /// representation. Data is passed as a cursor.
    pub fn from_etf_cursor(cursor: &mut Cursor<'_>, context: &mut LocalContext) -> Result<LocalTerm, TermError> {
        // specification: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html

        // read tag, skip optional header
        let mut tag = cursor.read_u8();
        if tag == 131 {
            tag = cursor.read_u8();
        }

        match EtfTag::from_repr(tag as usize) {
            Some(EtfTag::SmallTuple) => {
                let arity = cursor.read_u8() as usize;
                let mut elements = Vec::with_capacity(arity);
                for _ in 0..arity {
                    elements.push(Self::from_etf_cursor(cursor, context)?);
                }
                Ok(LocalTerm::Tuple(elements))
            },

            Some(EtfTag::Nil) => {
                Ok(LocalTerm::List(Box::new([]), None))
            },

            Some(EtfTag::String) => {
                let length = cursor.read_u16_be() as usize;
                let mut characters = Vec::with_capacity(length);
                for _ in 0..length {
                    characters.push(LocalTerm::Integer(cursor.read_u8().into()));
                }
                Ok(LocalTerm::List(characters.into_boxed_slice(), None))
            },
            
            Some(EtfTag::List) => {
                let length = cursor.read_u32_be() as usize;
                let mut elements = Vec::with_capacity(length);
                for _ in 0..length {
                    elements.push(Self::from_etf_cursor(cursor, context)?);
                }
                let improper_tail = Self::from_etf_cursor(cursor, context)?;
                let improper_tail = match improper_tail {
                    LocalTerm::List(elements, None) if elements.is_empty() => None,
                    term => Some(Box::new(term)),
                };
                Ok(LocalTerm::List(elements.into_boxed_slice(), improper_tail))
            },

            Some(EtfTag::Binary) => {
                let length = cursor.read_u32_be() as usize;
                let data = cursor.read_slice(length);
                Ok(LocalTerm::BitString(length * 8, data.to_vec()))
            },

            Some(EtfTag::SmallAtomUtf8) => {
                let length = cursor.read_u8() as usize;
                let string = cursor.read_utf8(length).map_err(|_| TermError::FormatError)?;
                Ok(LocalTerm::Atom(context.atom_table.get_or_make_atom(string)))
            },

            Some(EtfTag::SmallInteger) => {
                let value = cursor.read_u8() as usize;
                Ok(LocalTerm::Integer(value.into()))
            },

            None => Err(TermError::FormatError),
            Some(tag) => Err(TermError::EtfNotImplemented(tag))
        }
    }
}

// TODO: NodeTerm
