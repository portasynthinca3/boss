//! Interrupt manager. Based on Volume 3, Chapters 6 and 8 of the Intel SDM.
//! 
//! Below you will find some context on the terms that are being used throughout
//! this implementation.
//! 
//! An interrupt is basically a mechanism for signaling various events. When an
//! interrupt is fired, the CPU (a logical CPU, that is) stops normal execution
//! and temporarily transfers control to another piece of code to handle that
//! event. Every interrupt is assigned a unique number called the interrupt
//! vector. On x86, there are 256 available vectors.
//! 
//! Interrupts can be internal and external to the CPU. Vectors from 0 through
//! 31 are internal and are called exceptions - they're fired when the CPU
//! detects something abnormal about the code; for example, vector 0 is reserved
//! for signaling division by zero errors. Vectors from 32 onwards are generated
//! by various hardware around the CPU.
//! 
//! When an interrupt fires, the CPU looks up the function pointer associated
//! with its vector in a table called the IDT (Interrupt Descriptor Table) -
//! that function is called the ISR (Interrupt Service Routine). The CPU saves
//! some of the state of the program that was interrupted, but not all - it is
//! our job to save and then restore the rest.
//! 
//! The CPU does not directly tell us which of the interrupts got invoked - that
//! information is conveyed via the particular function pointer (all of which we
//! control) that it chose. This requires the use of small stubs that are aware
//! of where they're located so that they can tell a more generic ISR which
//! vector got invoked. Those stubs are called trampolines.
//! 
//! There are three layers to this implementation:
//!   - Structural: [`ExecutionState`], [`IdtEntry`] and [`RawTss`] describe the
//!     structure of the data that the CPU expects to see;
//!   - Raw: [`RawIdt`], [`_intr_common_handler`], [`_intr_reg_wrapper`] and
//!     [`make_trampoline`] create an unsafe abstraction over those raw
//!     structures.
//!   - Public: [`Manager`] exposes a somewhat safe and nice-to-use abstraction.

use bitfield_struct::bitfield;
use core::mem::size_of;
use core::arch::asm;
use strum::VariantArray;

use crate::mem_manager::{phys, reloc::Relocatable, PAGE_SIZE, VirtAddr, virt::AddressSpace};
use crate::segment::*;
use crate::virt::TableAttrs;

/// Disables interrupts. It is recommended to only keep them that way for a very
/// short amount of time, enabling them back with [`enable_external`] as soon as
/// possible.
/// 
/// Internal interrupts cannot be masked, and this function is not any more
/// powerful than the CPU, so it does not do that.
pub fn mask_external() {
    unsafe { asm!("cli"); }
}

/// Enables interrupts after they have been disabled by [`mask_external`].
pub fn enable_external() {
    unsafe { asm!("sti"); }
}

/// There are 256 interrupt vectors on x86, but this implementation considers it
/// unnecessary to support more than 128.
const MAX_INTERRUPT_CNT: usize = 128;

/// The state that the CPU was in when it was interrupted. Contains:
///   - General-purpose registers (`RAX`-`RDX`, `RSP`, `RBP`, `RSI`, `RDI`,
///     `R8`-`R15`);
///   - Segment registers (`CS`, `SS`);
///   - Return pointer and flags (`RIP`, `RFLAGS`);
///   - The vector number, error code (only useful in the case of some
///     exceptions), and pointer to the handler function.
// FIXME: currently SIMD registers are not saved.
#[repr(C)]
#[derive(Clone)]
pub struct ExecutionState {
    // pushed by _intr_reg_wrapper:
    pub r15: u64, pub r14: u64, pub r13: u64, pub r12: u64,
    pub r11: u64, pub r10: u64, pub r9: u64, pub r8: u64,
    pub rbp: u64, pub rdi: u64, pub rsi: u64, // note the missing RSP: it's saved by the CPU
    pub rdx: u64, pub rcx: u64, pub rbx: u64, // note the missing RAX: look one line down
    // pushed by one of the trampolines
    pub vector: u64, pub handler: u64, pub rax: u64,
    // pushed by the CPU or a trampoline for consistency
    pub err_code: u64,
    // pushed by the CPU:
    pub rip: u64, pub cs: u64,
    pub rflags: u64,
    pub rsp: u64, pub ss: u64,
}

/// External callback type. See also: [`Manager::set_handler`]
pub type Handler = fn(&mut ExecutionState) -> bool;

/// Look-up table for converting exceptions to nice names
const EXCEPTION_NAMES: [&str; 22] = [
    "#DE, divide fault",
    "#DB, debug trap",
    "", // NMI
    "#BP, breakpoint trap",
    "#OF, overflow trap",
    "#BR, bound fault",
    "#UD, undefined opcode fault",
    "#NM, x87 not present fault",
    "#DF, double fault abort",
    "", // reserved
    "#TS, invalid tss fault",
    "#NP, segment not present fault",
    "#SS, stack segment fault",
    "#GP, general protection fault",
    "#PF, page fault",
    "", // reserved
    "#MF, x87 fault",
    "#AC, alignment fault",
    "#MC, machine check abort",
    "#XM, SIMD fault",
    "#VE, virtualization fault",
    "#CP, control fault",
];

/// CPU exception types
#[derive(Clone, Copy, PartialEq, Eq)]
#[derive(strum_macros::VariantArray)] // TODO: convert to FromRepr
pub enum ExceptionType {
    DivideError = 0,
    DebugException = 1,
    Breakpoint = 3,
    Overflow = 4,
    BoundRangeExceeded = 5,
    UndefinedOpcode = 6,
    NoMathCoprocessor = 7,
    DoubleFault = 8,
    InvalidTss = 10,
    SegmentNotPresent = 11,
    StackSegmentFault = 12,
    GeneralProtectionFault = 13,
    PageFault = 14,
    MathFault = 16,
    AlignmentCheck = 17,
    MachineCheck = 18,
    SimdException = 19,
    VirtualizationException = 20,
    ControlProtectionException = 21,
}
impl TryFrom<u8> for ExceptionType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        for i in Self::VARIANTS {
            if *i as u8 == value { return Ok(*i) }
        }
        Err(())
    }
}
impl From<ExceptionType> for u8 {
    fn from(value: ExceptionType) -> Self {
        value as u8
    }
}

/// Interrupt vectors, including CPU exceptions. In this context, a vector is
/// just a number that's used to know what happened: a division by zero, a timer
/// firing or something else.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Vector {
    Internal(ExceptionType),
    External(u8),
}
impl TryFrom<u8> for Vector {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if let Ok(intr) = value.try_into() {
            Ok(Self::Internal(intr))
        } else if value >= 32 && value < MAX_INTERRUPT_CNT as u8 {
            Ok(Self::External(value))
        } else {
            Err(())
        }
    }
}
impl From<Vector> for u8 {
    fn from(value: Vector) -> Self {
        match value {
            Vector::Internal(intr) => intr.into(),
            Vector::External(intr) => intr,
        }
    }
}
impl core::fmt::Debug for Vector {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::External(ex) => write!(f, "external vector {ex}"),
            Self::Internal(int) => write!(f, "{}", EXCEPTION_NAMES[*int as u8 as usize]),
        }
    }
}
impl Vector {
    /// Some CPU exceptions push an error code, some do not. When generating a
    /// trampoline for an exception which does not push an error code, we emit
    /// a push of zero for consistency.
    fn cpu_pushes_errcode(self) -> bool {
        matches!(self,
            Self::Internal(ExceptionType::DoubleFault) |
            Self::Internal(ExceptionType::InvalidTss) |
            Self::Internal(ExceptionType::SegmentNotPresent) |
            Self::Internal(ExceptionType::StackSegmentFault) |
            Self::Internal(ExceptionType::GeneralProtectionFault) |
            Self::Internal(ExceptionType::PageFault) |
            Self::Internal(ExceptionType::AlignmentCheck) |
            Self::Internal(ExceptionType::ControlProtectionException))
    }
}

/// This is the final handling "entry point", so to speak. There are 256
/// functions that the CPU is aware of - we call them trampolines, because all
/// they do is push some values to the stack and jump to [`_intr_reg_wrapper`].
/// That latter function pushes the remaining CPU state (that was not pushed by
/// either the CPU itself or a trampoline) and calls this function with a
/// pointer to the cumulative state that has been captured. Our job now is to
/// call a handler that our trampoline has given us and provide it a safe
/// interface for interacting with the captured CPU state. Once we return
/// (assuming we hadn't panicked, which we very well might), we're going to pop
/// back into assembly code which is going to restore the (possibly modified)
/// state.
#[no_mangle]
// The ABI is specified explicitly because we need to feed this function a
// parameter from assembly. sysv64 specifically just because I prefer it.
extern "sysv64" fn _intr_common_handler(state_ptr: *mut ExecutionState) {
    // SAFETY: what we've been given is essentially a pointer to a large stack
    // variable. We're the sole owners of it, no race conditions are going to
    // occur.
    // The pointer type itself implies transmutation to/from a collection of
    // u64s. It is rendered safe thanks to repr(C).
    let mut state = unsafe { (*state_ptr).clone() };

    if let Ok(vector) = (state.vector as u8).try_into() {
        // print vector and errcode
        let code = state.err_code;
        let rip: VirtAddr = (state.rip as usize).into();
        log::trace!("got {vector:?} (errcode {code}) at RIP {rip:?}");

        // call handler
        let is_handled = match state.handler {
            0 => false,
            _ => {
                let handler = state.handler as *const ();
                // SAFETY: the number that we're transmuting into a function
                // pointer has been given to us by [Manager]. It is unique to
                // this CPU and the manager ensures that the trampoline (and
                // this pointer that's contained within) is always valid and
                // not half-overwritten by a handler change in progress.
                let handler: Handler = unsafe { core::mem::transmute(handler) };
                handler(&mut state)
            }
        };

        // dump and panic on unhandled exception
        let is_exception = matches!(vector, Vector::Internal(_));
        if !is_handled && is_exception {
            log::error!("unhandled {vector:?}; errcode: {code}");
            if vector == Vector::Internal(ExceptionType::PageFault) {
                let cr2: u64;
                // SAFETY: getting the CR2 (which contains the accessed address
                // that caused the fault) is safe
                unsafe { asm!("mov {0}, cr2", out(reg) cr2) };
                log::error!("CR2={cr2:#018x}");
            }
            log::error!("RAX={:#018x}  RBX={:#018x}  RCX={:#018x}  RDX={:#018x}", state.rax, state.rbx, state.rcx, state.rdx);
            log::error!("RBP={:#018x}  RSP={:#018x}  RSI={:#018x}  RDI={:#018x}", state.rbp, state.rsp, state.rsi, state.rdi);
            log::error!("R8=={:#018x}  R9=={:#018x}  R10={:#018x}  R11={:#018x}", state.r8, state.r9, state.r10, state.r11);
            log::error!("R12={:#018x}  R13={:#018x}  R14={:#018x}  R15={:#018x}", state.r12, state.r13, state.r14, state.r15);
            log::error!("RIP={:#018x}  RFL={:#018x}", state.rip, state.rflags);
            log::error!("CS={:#06x}  SS={:#06x}", state.cs, state.ss);
            panic!("unhandled exception in emulator code: {vector:?}");
        }
    } else {
        log::warn!("received invalid vector {}", state.vector)
    };

    // apply updated state
    // SAFETY: read beginning of this function.
    unsafe { *state_ptr = state };
}

/// Called by one of the trampolines. Saves the not-yet-saved registers and
/// calls [`_intr_common_handler`] with a pointer to the cumulative state
/// structure.
#[naked]
#[no_mangle]
unsafe extern "C" fn _intr_reg_wrapper() -> ! {
    asm!(
        // save registers
        // note: rax saved by trampoline
        "push rbx",
        "push rcx",
        "push rdx",
        "push rsi",
        "push rdi",
        "push rbp",
        "push r8",
        "push r9",
        "push r10",
        "push r11",
        "push r12",
        "push r13",
        "push r14",
        "push r15",
        // call handler
        "mov rdi, rsp", // sysv64: rdi contains first argument
        "call _intr_common_handler|0xffff800000000000", // relocate fn ptr to upper half at compile time
        // restore registers
        "pop r15",
        "pop r14",
        "pop r13",
        "pop r12",
        "pop r11",
        "pop r10",
        "pop r9",
        "pop r8",
        "pop rbp",
        "pop rdi",
        "pop rsi",
        "pop rdx",
        "pop rcx",
        "pop rbx",
        "add rsp, 16", // vector and handler
        "pop rax",
        "add rsp, 8", // error code
        // return from interrupt
        "iretq",
        options(noreturn)
    );
}

/// The double fault is the only exception which does not have a standard
/// trampoline that jumps to an overridable handler. If a double fault happens,
/// something must have gone TERRIBLY wrong with the memory map.
/// 
/// This handler just prints "\n#DF" to the serial port without using memory and
/// loops forever.
#[naked]
unsafe extern "C" fn double_fault_isr() -> ! {
    asm!(
        "mov dx, 0x3f8",
        "mov al, '\n'", "out dx, al",
        "mov al, '#'",  "out dx, al",
        "mov al, 'D'",  "out dx, al",
        "mov al, 'F'",  "out dx, al",
        "2:",
        "jmp 2b",
        options(noreturn)
    );
}

const TRAMPOLINE_SIZE: usize = 28;
/// The base machine code for generating trampolines. Values that are meant to
/// be replaced are marked with a decimal zero.
const TRAMPOLINE_SKELETON: [u8; TRAMPOLINE_SIZE] = [
    // yeah baby, fuck assembly! let's write machine code by hand!
    0x6a, 0x00,                   // push 0             1. (pushes the error code for consistency, omitted during generation if not needed)
    0x50,                         // push rax           2. saves rax before it is clobbered
    0x48, 0xb8, 0,0,0,0,0,0,0,0,  // mov rax, qword 0   3. these two push the handler address
    0x50,                         // push rax           3. (the zeroes are replaced)
    0x6a, 0,                      // push byte 0        4. pushes the vector (replaced)
    0x48, 0xb8, 0,0,0,0,0,0,0,0,  // mov rax, qword 0   5. these two jump to
    0xff, 0xe0,                   // jmp rax            5. [`_intr_reg_wrapper`]
];
const NOP_INSTRUCTION: u8 = 0x90;

/// Generates executable code that pushes the vector and handle pointer and
/// jumps to [`_intr_reg_wrapper`]. This is the code that the CPU calls directly
/// when an interrupt occurs.
fn make_trampoline(vector: u8, cpu_pushes_errcode: bool, handler: Option<Handler>) -> [u8; TRAMPOLINE_SIZE] {
    // get handler address
    let handler_addr: usize = match handler {
        None => 0,
        Some(handler) => {
            let mut handler_addr: VirtAddr = (handler as *const () as usize).into();
            handler_addr.relocate();
            handler_addr.into()
        },
    };

    // get wrapper address
    let mut wrapper_addr: VirtAddr = (_intr_reg_wrapper as *const () as usize).into();
    wrapper_addr.relocate();
    let wrapper_addr: usize = wrapper_addr.into();

    // insert addresses
    let mut code = TRAMPOLINE_SKELETON;
    code[5..13].copy_from_slice(&handler_addr.to_le_bytes());
    code[18..26].copy_from_slice(&wrapper_addr.to_le_bytes());
    code[15] = vector;

    if cpu_pushes_errcode {
        // remove the first instruction (two bytes)
        code[0] = NOP_INSTRUCTION;
        code[1] = NOP_INSTRUCTION;
        code.rotate_left(2);
    }

    code
}

/// An entry in the Interrupt Descriptor Table.
#[bitfield(u128, debug = false)]
// `#[repr(transparent)]` is set by `bitfield`, so the implied transmutation
// to/from `u128` in [`RawIdt`] is safe
struct IdtEntry {
    handler_offset_0: u16,
    cs: u16,
    #[bits(3)]
    intr_stack_table: u8,
    #[bits(5)]
    _zero_1: u8,
    #[bits(4)]
    gate_type: u8,
    _zero_2: bool,
    #[bits(2)]
    dpl: u8,
    present: bool,
    handler_offset_1: u16,
    handler_offset_2: u32,
    _rsvd: u32,
}

#[repr(packed)]
struct RawTss {
    _rsvd0: u32,
    rsp: [VirtAddr; 3],
    _rsvd1: u64,
    ist: [VirtAddr; 7],
    _rsvd2: u64,
    _rsvd3: u16,
    iopb: u16,
}

struct RawIdt(*mut [IdtEntry; MAX_INTERRUPT_CNT], *mut RawTss);

impl RawIdt {
    fn new() -> (RawIdt, CommonSelectors) {
        // allocate space for both tables
        assert!(size_of::<[IdtEntry; MAX_INTERRUPT_CNT]>() <= PAGE_SIZE);
        assert!(size_of::<RawTss>() <= PAGE_SIZE);
        let pages = phys::allocate(2);
        let mut pages: (VirtAddr, VirtAddr) = (pages[0].into(), pages[1].into());
        pages.0.relocate();
        pages.1.relocate();
        let idt: *mut [IdtEntry; MAX_INTERRUPT_CNT] = pages.0.into();
        let tss: *mut RawTss = pages.1.into();

        // do the annoying GDT thing
        let selectors = Gdt::do_annoying_boilerplate_thing(tss.into());

        (RawIdt(idt, tss), selectors)
    }

    unsafe fn set_as_current(&self) {
        let idtr = DescTableRegister::make(self.0.into(), (size_of::<[IdtEntry; MAX_INTERRUPT_CNT]>() - 1) as u16);
        idtr.load(DescTableKind::Interrupt);
    }

    unsafe fn set_raw_isr(&mut self, emu_code: Selector, vector: u8, raw_isr: Option<unsafe extern "C" fn() -> !>) {
        match raw_isr {
            None => (*self.0)[vector as usize] = IdtEntry::new().with_present(false),
            Some(raw_isr) => {
                let mut isr_addr: VirtAddr = (raw_isr as usize).into();
                isr_addr.relocate();
                let handler_addr: usize = isr_addr.into();
                (*self.0)[vector as usize] = IdtEntry::new()
                    .with_present(true)
                    .with_gate_type(SystemSegmentType::IntrGate as u8)
                    .with_dpl(0)
                    .with_intr_stack_table(0)
                    .with_cs(emu_code.value())
                    .with_handler_offset_0((handler_addr & 0xFFFF) as u16)
                    .with_handler_offset_1(((handler_addr >> 16) & 0xFFFF) as u16)
                    .with_handler_offset_2(((handler_addr >> 32) & 0xFFFFFFFF) as u32);
            }
        }
    }
}

/// Public interface for managing interrupts, one per CPU.
pub struct Manager {
    idt: RawIdt,
    selectors: CommonSelectors,
    trampolines: &'static mut [[u8; TRAMPOLINE_SIZE]],
}

impl Manager {
    /// Makes a new interrupt manager. Though several can be created per CPU,
    /// this is not very useful as their lifetimes are static and only one 
    /// them can be active at a time. Having one per CPU is the intended use
    /// case.
    pub fn new(addr_space: &mut AddressSpace) -> (Manager, CommonSelectors) {
        // make GDT, IDT and TSS
        let (mut idt, selectors) = RawIdt::new();

        // make default trampolines
        assert!(size_of::<[[u8; TRAMPOLINE_SIZE]; MAX_INTERRUPT_CNT]>() <= PAGE_SIZE);
        let pages = phys::allocate(1);
        let phys_page = pages[0];
        let mut virt_page: VirtAddr = phys_page.into();
        virt_page.relocate();
        let tramp: *mut [u8; TRAMPOLINE_SIZE] = virt_page.into();
        // SAFETY: this buffer has just been allocated by us, is of the correct
        // size, and is static.
        let tramp = unsafe { core::slice::from_raw_parts_mut(tramp, MAX_INTERRUPT_CNT) };

        for (i, tramp) in tramp.iter_mut().enumerate() {
            if let Ok(vec) = (i as u8).try_into() {
                let vec: Vector = vec;
                if vec == Vector::Internal(ExceptionType::DoubleFault) {
                    // double faults are special
                    unsafe {
                        // SAFETY: the pointer points to valid executable code
                        idt.set_raw_isr(selectors.emu_code, i as u8, Some(double_fault_isr));
                    }
                } else {
                    let code = make_trampoline(vec.into(), vec.cpu_pushes_errcode(), None);
                    *tramp = code;
                    let f_ptr = tramp as *mut u8;
                    unsafe {
                        // SAFETY: transmuting a thin pointer into another thin pointer is safe
                        let f_ptr: unsafe extern "C" fn() -> ! = core::mem::transmute(f_ptr);
                        // SAFETY: the pointer points to valid executable code
                        idt.set_raw_isr(selectors.emu_code, i as u8, Some(f_ptr));
                    }
                }
            }
        }

        // allow execution in the page with trampolines
        addr_space.modify().map_range(
            virt_page,
            phys_page,
            1,
            TableAttrs { execute: true, ..Default::default() },
            false
        ).unwrap();

        (Manager { idt, selectors, trampolines: tramp }, selectors)
    }

    /// Selects this interrupt manager as the active one for the CPU this method
    /// is executed on.
    /// 
    /// # Safety
    /// It is unsafe for an interrupt table with handlers that do bad things to
    /// be selected. Interrupt handlers registered with this manager have the
    /// ability to modify the state of other running code at will - it's very
    /// powerful, but inherently very dangerous. Extreme care must be taken when
    /// writing interrupt handlers.
    /// 
    /// Weird, nonsensical, time-dependent bugs **_will_** pop up if a handler
    /// modifies the state that it has been given in a bad way. My
    /// _recommendation_ is to either not fiddle with the state at all, or
    /// completely replace it with a cloned copy of another state structure for
    /// the purposes of task switching.
    pub unsafe fn set_as_current(&self) {
        self.idt.set_as_current();
        self.selectors.emu_code.set(SelectorRegister::Code);
        self.selectors.emu_data.set(SelectorRegister::Data);
        self.selectors.emu_data.set(SelectorRegister::Stack);
    }

    /// Sets the function that will be called when an interrupt occurs. This
    /// function is provided with a mutable reference to the state that the CPU
    /// was in when it was interrupted. The handler is free to modify it; any
    /// changes that it makes will be applied once it returns.
    /// 
    /// If the vector is for an internal interrupt (i.e. an exception), then the
    /// return value of the provided function signifies whether the handling of
    /// the exception was successful. If it returns `false`, the emulator will
    /// dump the CPU state and panic. The return value is ignored in the case of
    /// external interrupts.
    /// 
    /// This function will return an error if `vector` is [Vector::Internal()]
    /// of [ExceptionType::DoubleFault], as there's a special handler for double
    /// faults that cannot be overridden.
    /// 
    /// This method does not accept closures, only functions and simple lambdas
    /// (closures which don't capture anything).
    pub fn set_handler(&mut self, vector: Vector, handler: Option<Handler>) -> Result<(), ()> {
        if vector == Vector::Internal(ExceptionType::DoubleFault) { return Err(()) }

        let trampoline = make_trampoline(vector.into(), vector.cpu_pushes_errcode(), handler);
        mask_external();
        let vector: u8 = vector.into();
        self.trampolines[vector as usize] = trampoline;
        enable_external();
        Ok(())
    }
}
