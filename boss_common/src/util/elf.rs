//! Read-only ELF file format parser and loader.

use core::ops::Range;
use crate::target::current::memmgr::*;
use strum::FromRepr;
use super::cursor::Cursor;
use super::from_null_term;

const ELF_SIGNATURE: [u8; 4] = [0x7f, b'E', b'L', b'F'];
const WORD_LENGTH_64BIT: u8 = 2;
const ENDIANNESS_LITTLE: u8 = 1;
const ARCHITECTURE_X86_64: u16 = 0x3E;
const ELF_PROGRAM_LOAD: usize = 1;
const R_AMD64_RELATIVE: usize = 8;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ElfError {
    BadSignature,
    UnsupportedWordLength,
    UnsupportedEndianness,
    UnsupportedArchitecture,
    BadVersion,
    UnsupportedAlignment,
    MemMgr(Error),
    UnsupportedSection,
    StringError,
    UnexpectedEmptySection,
    UnsupportedRelocation,
}

/// Read-only ELF file format parser and loader.
#[derive(Clone, Debug)]
pub struct ElfFile<'d> {
    data: &'d [u8],
    entry_point: usize,

    program_table: &'d [u8],
    program_entry_size: usize,

    section_table: &'d [u8],
    section_entry_size: usize,

    string_section_index: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, FromRepr)]
enum ElfSectionType {
    Null = 0,
    ProgBits = 1,
    SymTab = 2,
    StrTab = 3,
    RelA = 4,
    Hash = 5,
    Dynamic = 6,
    Note = 7,
    NoBits = 8,
    Rel = 10,
    DynSym = 11,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
struct ElfSection<'d> {
    name: &'d str,
    contents: Option<&'d [u8]>,
    sec_type: ElfSectionType,
    size: usize,
    address: usize,
    linked_section: usize,
    info: usize,
    align: usize,
    entry_size: usize,
}

#[derive(Clone, Debug)]
struct ElfProgramBitLoadInfo<'d> {
    data: &'d [u8],
    mem_size: usize,
    virt_addr: usize,
}

#[derive(Clone, Debug)]
pub struct ElfLoadedProgramInfo {
    pub entry_point: VirtAddr,
    pub address_span: Range<VirtAddr>,
}

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

impl<'d> ElfFile<'d> {
    pub fn new(data: &'d [u8]) -> Result<ElfFile<'d>, ElfError> {
        let mut cursor = Cursor::new(data);

        let magic = cursor.read_slice(4);
        if magic != ELF_SIGNATURE { Err(ElfError::BadSignature)? };
        let [word_length, endianness, version] = cursor.read_slice(3) else { panic!() };
        if *word_length != WORD_LENGTH_64BIT { Err(ElfError::UnsupportedWordLength)? };
        if *endianness != ENDIANNESS_LITTLE { Err(ElfError::UnsupportedEndianness)? };
        if *version != 1 { Err(ElfError::BadVersion)? };
        cursor.skip(9); // ABI and padding

        let _obj_type = cursor.read_u16_le();
        let architecture = cursor.read_u16_le();
        let version2 = cursor.read_u32_le();
        if architecture != ARCHITECTURE_X86_64 { Err(ElfError::UnsupportedArchitecture)? };
        if version2 != 1 { Err(ElfError::BadVersion)? };

        let entry_point = cursor.read_u64_le() as usize;
        let pgm_table_offset = cursor.read_u64_le() as usize;
        let sec_table_offset = cursor.read_u64_le() as usize;

        cursor.skip(6); // architecture flags, elf header size

        let pgm_entry_size = cursor.read_u16_le() as usize;
        let pgm_entry_count = cursor.read_u16_le() as usize;
        let program_table = &data[pgm_table_offset .. (pgm_table_offset + (pgm_entry_size * pgm_entry_count))];

        let sec_entry_size = cursor.read_u16_le() as usize;
        let sec_entry_count = cursor.read_u16_le() as usize;
        let section_table = &data[sec_table_offset .. (sec_table_offset + (sec_entry_size * sec_entry_count))];

        let string_section_index = cursor.read_u16_le() as usize;

        Ok(ElfFile {
            data,
            entry_point,
            program_table,
            program_entry_size: pgm_entry_size,
            section_table,
            section_entry_size: sec_entry_size,
            string_section_index
        })
    }

    fn section_by_idx(&self, index: usize) -> &'d [u8] {
        &self.section_table[(index * self.section_entry_size) .. ((index + 1) * self.section_entry_size)]
    }

    fn parse_section(&self, index: usize) -> Result<ElfSection<'d>, ElfError> {
        let raw_header = self.section_by_idx(index);

        let mut cursor = Cursor::new(raw_header);
        let name_offset = cursor.read_u32_le() as usize;
        let sec_type_num = cursor.read_u32_le() as usize;
        let _flags = cursor.read_u64_le() as usize;
        let address = cursor.read_u64_le() as usize;
        let offset = cursor.read_u64_le() as usize;
        let size = cursor.read_u64_le() as usize;
        let linked_section = cursor.read_u32_le() as usize;
        let info = cursor.read_u32_le() as usize;
        let align = cursor.read_u64_le() as usize;
        let entry_size = cursor.read_u64_le() as usize;

        let Some(sec_type) = ElfSectionType::from_repr(sec_type_num) else {
            return Err(ElfError::UnsupportedSection)
        };
        let contents = if sec_type != ElfSectionType::NoBits {
            Some(&self.data[offset .. (offset + size)])
        } else {
            None
        };

        let string_table_contents = if index == self.string_section_index {
            contents.ok_or(ElfError::UnexpectedEmptySection)?
        } else {
            self.parse_section(self.string_section_index)
                .map_err(|_| ElfError::UnexpectedEmptySection)?
                .contents
                .ok_or(ElfError::UnexpectedEmptySection)?
        };

        Ok(ElfSection {
            name: from_null_term(&string_table_contents[name_offset .. string_table_contents.len()])
                .map_err(|_| ElfError::StringError)?,
            sec_type,
            contents,
            size,
            address,
            linked_section,
            info,
            align,
            entry_size,
        })
    }

    fn parse_program_entry(&self, index: usize) -> Option<ElfProgramBitLoadInfo<'_>> {
        let raw_data = &self.program_table[
            (index * self.program_entry_size)
            ..
            ((index + 1) * self.program_entry_size)
        ];
        let mut cursor = Cursor::new(raw_data);

        let entry_type = cursor.read_u32_le() as usize;
        if entry_type != ELF_PROGRAM_LOAD { return None };

        let _flags = cursor.read_u32_le() as usize;
        let offset = cursor.read_u64_le() as usize;
        let virt_addr = cursor.read_u64_le() as usize;
        let _phys_addr = cursor.read_u64_le() as usize;
        let file_size = cursor.read_u64_le() as usize;
        let mem_size = cursor.read_u64_le() as usize;
        let _align = cursor.read_u64_le() as usize;

        Some(ElfProgramBitLoadInfo {
            data: &self.data[offset .. (offset + file_size)],
            mem_size,
            virt_addr,
        })
    }

    fn apply_relocations(&self, program: &mut [u8], rela_section: &[u8], addr: VirtAddr) -> Result<(), ElfError> {
        // assuming we're on x86_64
        let addr = addr.to_usize();
        let mut cursor = Cursor::new(rela_section);
        loop {
            let offset = cursor.read_u64_le() as usize;
            let info = cursor.read_u64_le() as usize;
            let addend = cursor.read_i64_le() as usize;
            if info != R_AMD64_RELATIVE { return Err(ElfError::UnsupportedRelocation) };

            let slot = &mut program[offset .. offset + 8];
            let value = (addr + addend) as u64;
            slot.copy_from_slice(&value.to_le_bytes());

            if cursor.reached_end() { break; }
        }
        Ok(())
    }

    pub fn load_program(
        &self,
        addr_space: &mut AddrSpace,
        addr: VirtAddr
    ) -> Result<ElfLoadedProgramInfo, ElfError> {
        let addr_num = addr.to_usize();
        let mut guard = addr_space.modify();
        let mut space_allocated = 0usize;
        let mut next_alloc_at = addr;

        // load program bits
        for index in 0 .. (self.program_table.len() / self.program_entry_size) {
            let Some(entry) = self.parse_program_entry(index) else { continue };

            log::trace!("load {}B -> {}B @ {:#x}", entry.data.len(), entry.mem_size, entry.virt_addr);

            let new_required_size = entry.virt_addr + entry.mem_size;
            if new_required_size > space_allocated {
                let new_pages = new_required_size.div_ceil(PAGE_SIZE) - space_allocated.div_ceil(PAGE_SIZE);
                guard.allocate_range(next_alloc_at, new_pages, Default::default(), AllocReturn::Start).map_err(ElfError::MemMgr)?;
                space_allocated = new_required_size;
                next_alloc_at = VirtAddr::from_usize(next_alloc_at.to_usize() + (new_pages * PAGE_SIZE)).unwrap();
            }

            let load_addr = VirtAddr::from_usize(addr_num + entry.virt_addr).unwrap();
            let target = unsafe { core::slice::from_raw_parts_mut::<u8>(load_addr.to_mut_ptr(), entry.data.len()) };
            target.copy_from_slice(entry.data);
        }

        let program = unsafe { core::slice::from_raw_parts_mut::<u8>(addr.to_mut_ptr(), space_allocated) };

        // apply relocations
        for index in 0 .. (self.section_table.len() / self.section_entry_size) {
            let Ok(entry) = self.parse_section(index) else { continue };
            log::trace!("section \"{}\": {:?}", entry.name, entry.sec_type);

            #[allow(clippy::single_match)]
            match entry.sec_type {
                ElfSectionType::RelA => {
                    self.apply_relocations(program, entry.contents.ok_or(ElfError::UnexpectedEmptySection)?, addr)?;
                },
                _ => (),
            };
        }

        Ok(ElfLoadedProgramInfo {
            entry_point: VirtAddr::from_usize(addr_num + self.entry_point).unwrap(),
            address_span: addr..next_alloc_at,
        })
    }
}
