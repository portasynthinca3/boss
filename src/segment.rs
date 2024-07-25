//! Implements segments and descriptor tables. Based on Volume 3, Chapter 5 of
//! the Intel SDM.
//! 
//! The GDT was a useful memory structure in 16- and 32-bit mode, but once
//! 64-bit CPUs came about, it became crippled along with the whole idea of
//! memory segmentation and hardware task switching. The GDT (implemented here)
//! and the TSS (implemented in `interrupt.rs`) are now useless, annoying levels
//! of indirection for 1-10 pointers to ISR stacks that could've been stored in
//! a MSR somewhere.
//! 
//! I dunno, I love low-level dev, but this kind of stuff annoys and saddens me.

use core::mem::size_of;
use core::arch::asm;

use bitfield_struct::bitfield;

use crate::{phys, reloc::Relocatable, VirtAddr, PAGE_SIZE};

/// Basically a fat pointer to a descriptor table (GDT, LDT or IDT).
#[bitfield(u128)]
// `#[repr(transparent)]` is set by `bitfield`, so the implied transmutation
// to/from `u128` in the implementation is safe.
pub struct DescTableRegister {
    /// Limit: size - 1
    limit: u16,
    /// Base: starting address (8-byte alignment is strongly recommended)
    base: u64,
    #[bits(48)]
    _zero: u64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DescTableKind {
    Global, Local, Interrupt
}

impl DescTableRegister {
    pub fn make(base: VirtAddr, limit: u16) -> DescTableRegister {
        let base: usize = base.into();
        DescTableRegister::new().with_base(base as u64).with_limit(limit)
    }
    
    /// Loads the pointer into one of the table registers
    pub unsafe fn load(&self, kind: DescTableKind) {
        let address = self as *const Self as usize;
        if address % 8 != 0 {
            log::warn!("8-byte alignment is strongly recommended for performance");
        }
        match kind {
            DescTableKind::Global => asm!("lgdt [{0}]", in(reg) address),
            DescTableKind::Local => asm!("lldt [{0}]", in(reg) address),
            DescTableKind::Interrupt => asm!("lidt [{0}]", in(reg) address),
        }
    }
}

pub enum SystemSegmentType {
    Ldt = 0b0010,
    TssAvailable = 0b1001,
    TssBusy = 0b1011,
    CallGate = 0b1100,
    IntrGate = 0b1110,
    TrapGate = 0b1111,
}

/// An entry in a descriptor table (GDT or LDT). We're only concerning
/// ourselves with the GDT and 64-bit mode (not legacy 8 byte long descriptors)
#[bitfield(u128, debug=false)]
// This layout with the intertwined parts of multi-byte numbers and single-bit
// flags is batshit insane. If you hadn't figured it out by now, i am slightly
// dissati- no, i HATE the GDT. It's absolutely useless in 64-bit mode.
struct Descriptor {
    /// Segment limit (size - 1), bits 15:00
    limit_0: u16,
    /// Segment base address, bits 23:00
    #[bits(24)]
    base_0: u32,
    /// Descriptor type
    #[bits(4)]
    d_type: u8,
    /// 0 = System segment, 1 = Code/data segment
    system: bool,
    /// Privilege level
    #[bits(2)]
    dpl: u8,
    /// 1 = Present, 0 = Not present
    present: bool,
    /// Segment limit (size - 1), bits 19:16
    #[bits(4)]
    limit_1: u8,
    _zero_1: bool,
    /// 1 = 64-bit code segment; 0 = other segment
    bits_64: bool,
    _zero_2: bool,
    granularity: bool,
    /// Segment base address, bits 63:24
    #[bits(40)]
    base_1: u64,
    _zero_3: u32,
}

/// The maximum number of GDT entries that this implementation permits
/// (including the invalid zeroth one).
const GDT_ENTRY_CNT: usize = 16; // really, 7 is enough.

struct RawGdt(*mut [Descriptor; GDT_ENTRY_CNT]);

impl RawGdt {
    fn new() -> RawGdt {
        assert!(size_of::<[Descriptor; GDT_ENTRY_CNT]>() <= PAGE_SIZE);
        let pages = phys::allocate(1);
        let mut gdt: VirtAddr = pages[0].into();
        gdt.relocate();
        let gdt: *mut [Descriptor; GDT_ENTRY_CNT] = gdt.into();
        RawGdt(gdt)
    }

    unsafe fn get_descriptor(&self, idx: usize) -> Option<Descriptor> {
        let descriptor = (*self.0)[idx];
        if descriptor.present() { Some(descriptor) } else { None }
    }

    unsafe fn set_descriptor(&mut self, idx: usize, descriptor: Option<Descriptor>) {
        match descriptor {
            None => (*self.0)[idx] = Descriptor::new().with_present(false),
            Some(descriptor) => (*self.0)[idx] = descriptor,
        }
    }
}

pub struct Gdt(RawGdt);

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Selector(u16);
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct CommonSelectors {
    pub emu_code: Selector,
    pub emu_data: Selector,
    pub proc_code: Selector,
    pub proc_data: Selector,
    pub nif_code: Selector,
    pub nif_data: Selector,
    pub tss: Selector,
}

impl Gdt {
    pub fn new() -> Gdt {
        Gdt(RawGdt::new())
    }

    /// Instructs the CPU to use this global descriptor table.
    /// # Safety
    /// It is unsafe for a table with unsound values to be loaded.
    pub unsafe fn set_as_current(&self) {
        let addr: VirtAddr = self.0.0.into();
        let addr: usize = addr.into();
        let gdtr = DescTableRegister::new()
            .with_base(addr as u64)
            .with_limit((size_of::<[Descriptor; GDT_ENTRY_CNT]>() - 1) as u16);
        gdtr.load(DescTableKind::Global);
    }

    /// Allocates and selects a GDT with the standard selectors and returns
    /// them.
    pub fn do_annoying_boilerplate_thing(tss: VirtAddr) -> CommonSelectors {
        let tss: usize = tss.into();
        let mut gdt = Self::new();
        unsafe { gdt.set_as_current(); }

        let generic_descriptor = Descriptor::new()
            .with_present(true)
            .with_base_0(0).with_base_1(0)
            .with_system(true)
            .with_limit_0(0xFFFF).with_limit_1(0xF).with_granularity(true)
            .with_bits_64(true);

        let tss_descriptor = Descriptor::new()
            .with_present(true)
            .with_base_0((tss & 0xFFFFFF) as u32).with_base_1((tss >> 24) as u64)
            .with_limit_0(PAGE_SIZE as u16).with_limit_1(0).with_granularity(false);

        // SAFETY: we're performing sound modifications here
        unsafe {
            gdt.0.set_descriptor(1, Some(generic_descriptor.with_dpl(0).with_d_type(0b1010))); // execute-read
            gdt.0.set_descriptor(2, Some(generic_descriptor.with_dpl(0).with_d_type(0b0010))); // read-write
            gdt.0.set_descriptor(3, Some(generic_descriptor.with_dpl(1).with_d_type(0b1010)));
            gdt.0.set_descriptor(4, Some(generic_descriptor.with_dpl(1).with_d_type(0b0010)));
            gdt.0.set_descriptor(5, Some(generic_descriptor.with_dpl(3).with_d_type(0b1010)));
            gdt.0.set_descriptor(6, Some(generic_descriptor.with_dpl(3).with_d_type(0b0010)));
            gdt.0.set_descriptor(7, Some(tss_descriptor));
            gdt.set_as_current();
        }

        CommonSelectors {
            emu_code: Selector(0x10), emu_data: Selector(0x20),
            proc_code: Selector(0x30), proc_data: Selector(0x40),
            nif_code: Selector(0x50), nif_data: Selector(0x60),
            tss: Selector(0x70),
        }
    }
}

pub enum SelectorRegister {
    Code, Data, Stack,
}

impl Selector {
    pub fn value(&self) -> u16 { self.0 }
    pub unsafe fn set(&self, kind: SelectorRegister) {
        match kind {
            SelectorRegister::Code => asm!(
                // push selector and address of instruction after this asm block
                "push {0:r}",
                "lea {1}, [0f]",
                "mov {2}, 0xffff800000000000", // relocate
                "or {1}, {2}",
                "push {1}",
                // perform far return, effectively loading new selector
                "retfq",
                "0:",
                in(reg) self.0,
                out(reg) _,
                out(reg) _),
            SelectorRegister::Data => asm!("mov ds, {0:x}", in(reg) self.0),
            SelectorRegister::Stack => asm!("mov ss, {0:x}", in(reg) self.0),
        }
    }
}
