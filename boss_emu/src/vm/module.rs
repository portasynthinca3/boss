//! Parses BEAM modules from their binary representation in memory. Based on
//! Chapter 6 of the BEAM Book, the BEAM source code and just general googling.

use alloc::{borrow::ToOwned, boxed::Box, vec::Vec};

use hashbrown::HashMap;
use num_bigint::{BigInt, Sign};

include!(concat!(env!("OUT_DIR"), "/genop.rs"));
use super::{state::{LocalAtomRef, LocalContext}, term::{LocalTerm, TermError}, CURRENT_OPCODE_MAX};
use boss_common::util::cursor::Cursor;

/// Maximum number of operands for an opcode
pub const MAX_OPERANDS: usize = 8;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FormatError {
    ListOperandLengthType,
    NumberOperandTooBig,
    LiteralOperandIndexType,
    VeryLargeNumberOperandLengthType,
    OpcodeOutOfBounds,
    InvalidUtf8,
    MissingTable(&'static str),
    InvalidEtf(TermError),
    LabelOperandType,
    LabelOperandNotSequential,
    NonZeroDecompressedLitSize,
    AllocListOperandTagType,
    AllocListOperandInvalidTag,
    AllocListOperandItemCountType,
    AtomTableOperandType,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LoadError {
    /// Invalid BEAM file signature
    NotBEAMFile,
    /// A feature is not implemented
    NotImplemented,
    /// Invalid data inside BEAM file
    FormatError(FormatError),
    /// The BEAM file is too new for this VM
    UnsupportedMaxOpcode(usize),
    /// An undefined opcode was encountered
    UndefinedOpcode(u8),
}

/// Operands for bytecode instructions
#[derive(Clone)]
pub enum Operand {
    Number(BigInt),
    Atom(LocalAtomRef),
    XReg(usize),
    YReg(usize),
    Label(usize),
    FpReg(usize),
    AllocList(usize, usize, usize), // words, floats, funs
    Literal(LocalTerm),
    List(Box<[Operand]>),
    Nil,
}

impl Operand {
    pub fn read(cursor: &mut Cursor<'_>, module_atoms: &[LocalAtomRef], literal_map: &[LocalTerm]) -> Result<Operand, LoadError> {
        // depending on the operand's value, it can be encoded in 4 different ways
        let tag = cursor.read_u8();
        if tag & 0b111 == 0b111 {
            // "extended" value
            match (tag >> 4) & 0xf {
                1 => {
                    // list
                    let Operand::Number(length) = Self::read(cursor, module_atoms, literal_map)? else {
                        return Err(LoadError::FormatError(FormatError::ListOperandLengthType));
                    };
                    let length: usize = length.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                    let mut elements = Vec::with_capacity(length);
                    for _ in 0..length {
                        let operand = Self::read(cursor, module_atoms, literal_map)?;
                        elements.push(operand);
                    }
                    Ok(Operand::List(elements.into_boxed_slice()))
                },
                3 => {
                    // allocation list
                    let Operand::Number(item_count) = Self::read(cursor, module_atoms, literal_map)? else {
                        return Err(LoadError::FormatError(FormatError::AllocListOperandItemCountType));
                    };
                    let item_count: usize = item_count.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                    let (mut words, mut floats, mut funs) = (0usize, 0usize, 0usize);
                    for _ in 0..item_count {
                        let Operand::Number(item_type) = Self::read(cursor, module_atoms, literal_map)? else {
                            return Err(LoadError::FormatError(FormatError::AllocListOperandTagType));
                        };
                        let Operand::Number(item_value) = Self::read(cursor, module_atoms, literal_map)? else {
                            return Err(LoadError::FormatError(FormatError::AllocListOperandTagType));
                        };
                        let item_type: usize = item_type.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                        let item_value: usize = item_value.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                        match item_type {
                            0 => words = item_value,
                            1 => floats = item_value,
                            2 => funs = item_value,
                            _ => return Err(LoadError::FormatError(FormatError::AllocListOperandInvalidTag)),
                        }
                    }
                    Ok(Operand::AllocList(words, floats, funs))
                },
                4 => {
                    // literal
                    if let Operand::Number(index) = Self::read(cursor, module_atoms, literal_map)? {
                        let index: usize = index.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                        Ok(Operand::Literal(literal_map[index].clone()))
                    } else {
                        Err(LoadError::FormatError(FormatError::LiteralOperandIndexType))
                    }
                },
                5 => {
                    // typed register
                    let operand = Self::read(cursor, module_atoms, literal_map)?;
                    let _ = Self::read(cursor, module_atoms, literal_map)?; // type hint, ignore
                    Ok(operand)
                },
                val => todo!("compact term format extended tag {val}"),
            }
        } else {
            // not an "extended value"
            // size depends on middle two bits
            let size_tag = (tag >> 3) & 0b11;
            let value: BigInt = match size_tag {
                0b00|0b10 => {
                    // value smaller than 16, contained in the upper 4 bits
                    ((tag >> 4) & 0xf).into()
                },
                0b01 => {
                    // value smaller than 2048, contained in the next byte and
                    // in the upper 3 bits of the current byte
                    let next = cursor.read_u8() as usize;
                    (next | ((tag as usize >> 5) & 0b111)).into()
                },
                0b11 => {
                    // large or very large value
                    let size = ((tag >> 5) & 0b111) as usize;
                    let size = if size == 0b111 {
                        // very large value (>= 9 bytes)
                        // size is contained in nested unsigned operand
                        if let Operand::Number(size) = Self::read(cursor, module_atoms, literal_map)? {
                            size + BigInt::from(9)
                        } else {
                            return Err(LoadError::FormatError(FormatError::VeryLargeNumberOperandLengthType));
                        }
                    } else {
                        // large value (2 to 8 bytes)
                        (size + 2).into()
                    };
                    let bytes = cursor.read_slice(size.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?);
                    BigInt::from_bytes_be(Sign::NoSign, bytes)
                },
                _ => unreachable!(),
            };

            // depending on the tag, the value represents different things
            // but we don't really care about that now
            Ok(match tag & 0b111 {
                0b000|0b001|0b110 => Operand::Number(value),
                0b010 if value == 0.into() => Operand::Nil,
                0b010 => {
                    let atom_idx: usize = value.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
                    Operand::Atom(module_atoms[atom_idx - 1].clone())
                },
                0b011 => Operand::XReg(value.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?),
                0b100 => Operand::YReg(value.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?),
                0b101 => Operand::Label(value.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?),
                _ => unreachable!(),
            })
        }
    }
}

impl core::fmt::Debug for Operand {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Operand::Atom(reference) => {
                let atom: alloc::rc::Rc<str> = reference.into();
                write!(f, "{atom}")
            },
            Operand::Label(index) => write!(f, "L{index}"),
            Operand::Literal(lit) => write!(f, "{lit:?}"),
            Operand::Nil => write!(f, "[]"),
            Operand::Number(n) => write!(f, "{n}"),
            Operand::XReg(reg) => write!(f, "X{reg}"),
            Operand::YReg(reg) => write!(f, "Y{reg}"),
            Operand::List(list) => {
                write!(f, "[")?;
                for (i, element) in list.iter().enumerate() {
                    write!(f, "{element:?}")?;
                    if i != list.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            },
            Operand::AllocList(words, floats, funs) => {
                write!(f, "[{words} wrd, {floats} flt, {funs} fun]")
            },
            _ => todo!(),
        }
    }
}

#[derive(Clone)]
/// Complete instruction representation
pub struct Instruction {
    source: Option<Box<[u8]>>,
    pub opcode: Opcode,
    pub operands: [Option<Operand>; MAX_OPERANDS],
}

impl Instruction {
    pub fn read(cursor: &mut Cursor<'_>, module_atoms: &[LocalAtomRef], literal_map: &[LocalTerm]) -> Result<Instruction, LoadError> {
        #[cfg(feature = "trace-beam")]
        let mut start_cur = cursor.clone();

        // read opcode
        let opcode = cursor.read_u8() as usize;
        if opcode > CURRENT_OPCODE_MAX { return Err(LoadError::FormatError(FormatError::OpcodeOutOfBounds)); }
        let opcode = Opcode::from_repr(opcode).ok_or(LoadError::UndefinedOpcode(opcode as u8))?;

        // read operands
        let mut insn = Instruction { opcode, operands: [const { None }; MAX_OPERANDS], source: None };
        for i in 0..opcode.arity() {
            let operand = Operand::read(cursor, module_atoms, literal_map)?;
            insn.operands[i] = Some(operand);
        }

        // remember entire source of instruction for debugging
        #[cfg(feature = "trace-beam")]
        {
            let insn_len = cursor.position - start_cur.position;
            insn.source = Some(Box::from(start_cur.read_slice(insn_len)));
        }

        Ok(insn)
    }
}

impl core::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        const BYTECODE_WIDTH: isize = 30;

        // source bytecode
        if f.alternate() {
            if let Some(ref source) = self.source {
                let mut length = 0;
                for byte in source.iter() {
                    write!(f, "{byte:02x} ")?;
                    length += 3;
                }
                // padding
                for _ in 0..(BYTECODE_WIDTH - length).max(0) {
                    write!(f, " ")?;
                }
            }
        }

        // special case: label
        if self.opcode == Opcode::Label {
            return write!(f, "\x1b[33mL{:?}\x1b[0;0m:", self.operands[0].as_ref().expect("instruction operand within arity is None"))
        }

        // opcode
        if f.alternate() {
            write!(f, "\x1b[34m{:?}\x1b[0;0m ", self.opcode)?;
        } else {
            write!(f, "{:?} ", self.opcode)?;
        }

        // operands
        for i in 0..self.opcode.arity() {
            write!(f, "{:?}", self.operands[i].as_ref().expect("instruction operand within arity is None"))?;
            let last = i == self.opcode.arity() - 1;
            if !last {
                if f.alternate() { write!(f, "\x1b[36m,\x1b[0;0m ") } else { write!(f, ", ") }?;
            }
        }

        Ok(())
    }
}

/// Named collection of related functions
pub struct Module {
    /// Name of the module
    name: LocalAtomRef,
    /// List of imported external and far functions (module, function and arity)
    pub imports: Box<[(LocalAtomRef, LocalAtomRef, usize)]>,
    /// Map of functions (name and arity) to the entry label
    pub exports: HashMap<(LocalAtomRef, usize), usize>,
    /// Defined literal terms
    #[allow(unused)]
    literals: Box<[LocalTerm]>,
    /// Instruction pointers of labels
    pub labels: Box<[usize]>,
    /// Instruction stream
    pub instructions: Box<[Instruction]>,
}

impl Module {
    /// Parses a BEAM module from its raw binary representation
    pub fn new(data: &[u8], context: &mut LocalContext) -> Result<Module, LoadError> {
        // verify signatures
        if data[0..4] != [b'F', b'O', b'R', b'1']
        || data[8..12] != [b'B', b'E', b'A', b'M'] {
            return Err(LoadError::NotBEAMFile);
        }

        // load chunks
        let mut chunks = HashMap::new();
        let mut cursor = Cursor::new(data);
        cursor.skip(12);
        loop {
            if cursor.reached_end() { break; }
            let name = cursor.read_utf8(4).map_err(|_| LoadError::FormatError(FormatError::InvalidUtf8))?;
            let size = cursor.read_u32_be() as usize;
            let chunk_data = cursor.read_slice(size);
            let size_w_padding = size.div_ceil(4) * 4;
            cursor.skip(size_w_padding - size);
            chunks.insert(name, chunk_data);
        }
        let chunks = chunks;

        // parse atom table (AtU8 chunk, Atom not supported)
        let mut cursor = Cursor::new(chunks.get("AtU8").ok_or(LoadError::NotImplemented)?.to_owned()); // probably only contains legacy Atom table
        let atom_cnt = -cursor.read_i32_be() as usize; // https://github.com/erlang/otp/blob/359e254aba76c1986b671b45fd320c6cc6720ca8/lib/compiler/src/beam_asm.erl#L301
        let mut module_atoms = Vec::with_capacity(atom_cnt);
        loop {
            if cursor.reached_end() { break; }
            let Operand::Number(atom_size) = Operand::read(&mut cursor, &[], &[])? else {
                return Err(LoadError::FormatError(FormatError::AtomTableOperandType))
            };
            let atom_size: usize = atom_size.try_into().map_err(|_| LoadError::FormatError(FormatError::NumberOperandTooBig))?;
            let atom_name = cursor.read_utf8(atom_size).map_err(|_| LoadError::FormatError(FormatError::InvalidUtf8))?;
            let atom = context.atom_table.get_or_make_atom(atom_name);
            module_atoms.push(atom);
        }
        let module_atoms = module_atoms.into_boxed_slice();

        // parse export table (ExpT chunk)
        let mut cursor = Cursor::new(chunks.get("ExpT").ok_or(LoadError::FormatError(FormatError::MissingTable("ExpT")))?.to_owned());
        let expt_cnt = cursor.read_u32_be() as usize;
        let mut exports = HashMap::with_capacity(expt_cnt);
        loop {
            if cursor.reached_end() { break; }
            let name = cursor.read_u32_be() as usize;
            let arity = cursor.read_u32_be() as usize;
            let label = cursor.read_u32_be() as usize;
            exports.insert((module_atoms[name - 1].to_owned(), arity), label);
        }
        let exports = exports;

        // parse import table (ImpT chunk)
        let mut cursor = Cursor::new(chunks.get("ImpT").ok_or(LoadError::FormatError(FormatError::MissingTable("ImpT")))?.to_owned());
        let import_cnt = cursor.read_u32_be() as usize;
        let mut imports = Vec::with_capacity(import_cnt);
        loop {
            if cursor.reached_end() { break; }
            let module = module_atoms[cursor.read_u32_be() as usize - 1].to_owned();
            let function = module_atoms[cursor.read_u32_be() as usize - 1].to_owned();
            let arity = cursor.read_u32_be() as usize;
            imports.push((module, function, arity));
        }
        let imports = imports.into_boxed_slice();

        // parse Code chunk
        let mut cursor = Cursor::new(chunks.get("Code").ok_or(LoadError::FormatError(FormatError::MissingTable("CodeT")))?.to_owned());
        let info_size = cursor.read_u32_be() as usize;
        let instruction_set = cursor.read_u32_be() as usize;
        let opcode_max = cursor.read_u32_be() as usize;
        let label_cnt = cursor.read_u32_be() as usize;
        let function_cnt = cursor.read_u32_be() as usize;
        cursor.skip(info_size - 16);
        let code = cursor.read_slice(cursor.remaining());
        if instruction_set != 0 || opcode_max > CURRENT_OPCODE_MAX {
            return Err(LoadError::UnsupportedMaxOpcode(opcode_max)); 
        }

        // parse literals (LitT chunk)
        let literals = if let Some(lit_chunk) = chunks.get("LitT") {
            let mut cursor = Cursor::new(lit_chunk);
            let decompressed_sz = cursor.read_u32_be() as usize;
            if decompressed_sz != 0 {
                return Err(LoadError::FormatError(FormatError::NonZeroDecompressedLitSize));
            }
            let literal_cnt = cursor.read_u32_be() as usize;
            let mut literals = Vec::with_capacity(literal_cnt);
            loop {
                if cursor.reached_end() { break; }
                let literal_size = cursor.read_u32_be() as usize;
                let literal_etf = cursor.read_slice(literal_size);
                let literal = LocalTerm::from_etf(literal_etf, context).map_err(|e| LoadError::FormatError(FormatError::InvalidEtf(e)))?;
                literals.push(literal);
            }
            literals.into_boxed_slice()
        } else {
            Box::new([])
        };

        // parse instruction stream within Code chunk
        let mut instructions = Vec::new();
        let mut labels = Vec::with_capacity(label_cnt);
        labels.push(0);
        let mut cursor = Cursor::new(code);
        let mut instruction_ctr = 0;
        #[cfg(feature = "trace-beam")]
        log::trace!("module '{}' disassembly:", module_atoms[0]);
        loop {
            // read next instruction
            if cursor.reached_end() { break; }
            let instruction = Instruction::read(&mut cursor, &module_atoms, &literals)?;
            #[cfg(feature = "trace-beam")]
            log::trace!("\x1b[38;5;238m{instruction_ctr: >4}: {instruction:#?}");

            // remember labels
            if instruction.opcode == Opcode::Label {
                let Some(Operand::Number(ref index)) = instruction.operands[0] else {
                    return Err(LoadError::FormatError(FormatError::LabelOperandType));
                };
                let index: usize = index.try_into().unwrap();
                if index != labels.len() {
                    return Err(LoadError::FormatError(FormatError::LabelOperandNotSequential));
                }
                labels.push(instruction_ctr);
            }

            instruction_ctr += 1;
            instructions.push(instruction);
        }
        let instructions = instructions.into_boxed_slice();
        let labels = labels.into_boxed_slice();

        log::trace!("loaded '{}' fns={function_cnt}, labels={label_cnt}, isa={instruction_set}, max_opcode={opcode_max}", module_atoms[0]);
        Ok(Module {
            name: module_atoms[0].clone(),
            imports,
            exports,
            literals,
            instructions,
            labels
        })
    }
}

impl core::fmt::Debug for Module {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Module")
            .field("name", &self.name)
            .field("exports", &self.exports.keys())
            .finish()
    }
}
