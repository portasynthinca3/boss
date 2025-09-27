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
//!   - Structural: [`CpuState`], [`IdtEntry`] and [`RawTss`] describe the
//!     structure of the data that the CPU expects to see;
//!   - Raw: [`RawIdt`], [`_intr_common_handler`], [`_intr_reg_wrapper`] and
//!     [`make_trampoline`] create an unsafe abstraction over those raw
//!     structures.
//!   - Public: [`IntrMgr`] exposes a somewhat safe and nice-to-use abstraction.

use core::fmt::{Debug, Formatter};
use core::mem::size_of;
use core::arch::{asm, naked_asm};

use crate::target::current::{
    ll::descriptors::*,
    memmgr::*,
};
pub use crate::target::interface::interrupt::{
    IntrVector,
    AccessError,
    ExceptionInfo,
    IntrHandler,
    Error,
    Result,
    CpuState as IfCpuState,
    IntrMgr as IfIntrMgr,
};

use itertools::Either;
use bitfield_struct::bitfield;

pub const INTERRUPT_CNT: usize = 256;

// FIXME: currently SIMD registers are not saved.
#[repr(C)]
#[derive(Clone)]
pub struct CpuState {
    // pushed by _intr_reg_wrapper:
    pub ds: u64, pub cr2: u64,
    pub r15: u64, pub r14: u64, pub r13: u64, pub r12: u64,
    pub r11: u64, pub r10: u64, pub r9: u64, pub r8: u64,
    pub rbp: u64, pub rdi: u64, pub rsi: u64, // note the missing RSP: it's saved by the CPU
    pub rdx: u64, pub rcx: u64, pub rbx: u64, // note the missing RAX: look two lines down
    // pushed by one of the trampolines:
    pub vector: u64, pub handler: u64, pub rax: u64,
    // pushed by the CPU or a trampoline for consistency:
    pub err_code: u64,
    // pushed by the CPU:
    pub rip: u64, pub cs: u64,
    pub rflags: u64,
    pub rsp: u64, pub ss: u64,
}

/// Look-up table for converting exceptions to nice names
static EXCEPTION_NAMES: [&str; 22] = [
    "#DE, divide fault",
    "#DB, debug trap",
    "NMI, non-maskable interrupt",
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

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(strum::FromRepr)]
enum TargetException {
    DivideError = 0,
    DebugException = 1,
    Nmi = 2,
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
    ControlProtection = 21,
}

impl From<TargetException> for &'static str {
    fn from(value: TargetException) -> Self {
        EXCEPTION_NAMES[value as usize]
    }
}

static ARITH_EXCEPTS: &[u8] = &[
    TargetException::DivideError as _,
    TargetException::NoMathCoprocessor as _,
    TargetException::MathFault as _,
    TargetException::SimdException as _,
];
static BREAKPOINT_EXCEPTS: &[u8] = &[
    TargetException::DebugException as _,
    TargetException::Breakpoint as _,
];
static HARD_FAULT_EXCEPTS: &[u8] = &[
    TargetException::Nmi as _,
    TargetException::MachineCheck as _,
];
static ACCESS_EXCEPTS: &[u8] = &[
    TargetException::GeneralProtectionFault as _,
    TargetException::PageFault as _,
    TargetException::AlignmentCheck as _,
    TargetException::ControlProtection as _,
];

/// First vector number that is allowed to be triggered by an external interrupt
const MIN_EXTERNAL: u8 = 32;

/// Error code generated by the #PF Page Fault exception
#[bitfield(u64)]
struct PfErrorCode {
    present: bool,
    write: bool,
    user_mode: bool,
    reserved_bit_used: bool,
    protection_key: bool,
    shadow_stack: bool,
    hlat: bool,
    sgx: bool,
    #[bits(7)]
    _rsvd: u8,
    secure_guard: bool,
    #[bits(48)]
    _rsvd2: u64,
}

impl IfCpuState for CpuState {
    fn exception_info(&self) -> Option<ExceptionInfo> {
        let vector = self.vector as u8;
        if vector >= MIN_EXTERNAL {
            return None;
        }
        let Some(exception) = TargetException::from_repr(vector) else {
            return Some(ExceptionInfo::PlatformSpecific(vector));
        };
        Some(match exception {
            TargetException::DivideError |
            TargetException::NoMathCoprocessor |
            TargetException::MathFault |
            TargetException::SimdException => ExceptionInfo::ArithmeticError,
            TargetException::DebugException |
            TargetException::Breakpoint => ExceptionInfo::Breakpoint,
            TargetException::Nmi |
            TargetException::MachineCheck => ExceptionInfo::HardwareFailure,
            TargetException::GeneralProtectionFault => ExceptionInfo::InvalidAccess(None, None),
            TargetException::PageFault => {
                let code = PfErrorCode::from_bits(self.err_code);
                let error = if code.user_mode() {
                    AccessError::UserMode { write: code.write(), page_present: code.present() }
                } else {
                    AccessError::SupervisorMode { write: code.write(), page_present: code.present() }
                };
                ExceptionInfo::InvalidAccess(Some(error), VirtAddr::from_usize(self.cr2 as usize).ok())
            },
            TargetException::AlignmentCheck => ExceptionInfo::InvalidAccess(Some(AccessError::Alignment), None),
            TargetException::ControlProtection => ExceptionInfo::InvalidAccess(Some(AccessError::StackCorruption), None),
            _ => ExceptionInfo::PlatformSpecific(vector),
        })
    }

    fn platform_exception_name(&self) -> Option<&'static str> {
        let vector = self.vector as u8;
        if vector >= MIN_EXTERNAL {
            return None;
        }
        TargetException::from_repr(vector)?;
        Some(EXCEPTION_NAMES[vector as usize])
    }

    fn program_counter(&self) -> Option<VirtAddr> {
        VirtAddr::from_usize(self.rip as usize).ok()
    }
}

impl Debug for CpuState {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "RAX={:#018x}  RBX={:#018x}  RCX={:#018x}  RDX={:#018x}", self.rax, self.rbx, self.rcx, self.rdx)?;
        writeln!(f, "RBP={:#018x}  RSP={:#018x}  RSI={:#018x}  RDI={:#018x}", self.rbp, self.rsp, self.rsi, self.rdi)?;
        writeln!(f, "R8=={:#018x}  R9=={:#018x}  R10={:#018x}  R11={:#018x}", self.r8, self.r9, self.r10, self.r11)?;
        writeln!(f, "R12={:#018x}  R13={:#018x}  R14={:#018x}  R15={:#018x}", self.r12, self.r13, self.r14, self.r15)?;
        writeln!(f, "RIP={:#018x}  RFL={:#018x}  CR2={:#018x}  VEC={:#018x}  ERR={:#010x}", self.rip, self.rflags, self.cr2, self.vector, self.err_code)?;
        writeln!(f, "CS={:#06x}  SS={:#06x}  DS={:#06x}", self.cs, self.ss, self.ds)?;
        writeln!(f, "Exception: {:?} {:?}", self.exception_info(), self.platform_exception_name())?;
        write!(f, "Handler: {:#018x}", self.handler)
    }
}

/// One vector in our abstraction may correspond to multiple hardware vectors
impl IntrVector {
    fn to_platform_vectors(self) -> Either<&'static [u8], u8> {
        match self {
            IntrVector::ArithmeticError => Either::Left(ARITH_EXCEPTS),
            IntrVector::Breakpoint => Either::Left(BREAKPOINT_EXCEPTS),
            IntrVector::HardwareFailure => Either::Left(HARD_FAULT_EXCEPTS),
            IntrVector::InvalidAccess => Either::Left(ACCESS_EXCEPTS),
            IntrVector::IllegalInstruction => Either::Right(TargetException::UndefinedOpcode as _),
            IntrVector::PlatformSpecificException(e) => Either::Right(e),
            IntrVector::External(e) if e >= MIN_EXTERNAL => Either::Right(e),
            IntrVector::External(_) /* if internal */ => panic!(),
        }
    }
}

fn cpu_pushes_errcode(vector: u8) -> bool {
    let exc = TargetException::from_repr(vector);
    let Some(exc) = exc else { return false };
    matches!(exc,
        TargetException::DoubleFault |
        TargetException::InvalidTss |
        TargetException::SegmentNotPresent |
        TargetException::StackSegmentFault |
        TargetException::GeneralProtectionFault |
        TargetException::PageFault |
        TargetException::AlignmentCheck |
        TargetException::ControlProtection)
}

/// This is the final handling entry point in this module. There are 256
/// functions that the CPU is aware of - we call them trampolines, because all
/// they do is push some values to the stack and jump to [`_intr_reg_wrapper`].
/// That latter function pushes the remaining CPU state (that was not pushed by
/// either the CPU itself or a trampoline) and calls this function with a
/// pointer to the cumulative state that has been captured. Our job now is to
/// call a handler that our trampoline has given us and provide it a safe
/// interface for interacting with the captured CPU state. Once we return, we're
/// going to pop back into assembly code which is going to restore the
/// possibly modified state.
#[no_mangle]
// The ABI is specified explicitly because we need to feed this function a
// parameter from assembly. sysv64 specifically just because I prefer it.
extern "sysv64" fn _intr_common_handler(state_ptr: *mut CpuState) {
    // SAFETY: what we've been given is essentially a pointer to a large stack
    // variable, created just for us. Data at that pointer will not be accessed
    // until we return.
    let state = unsafe { &mut *state_ptr };
    let handler_fn = state.handler as *const ();
    let is_exception = state.exception_info().is_some();

    if handler_fn.is_null() {
        if is_exception {
            log::error!("{state:?}");
            panic!("unhandled exception");
        } else {
            log::warn!("unhandled interrupt {:?}", state.vector);
            return;
        }
    }

    let handler: IntrHandler = unsafe { core::mem::transmute(handler_fn) };
    handler(state);
}

/// Called by one of the trampolines. Saves the not-yet-saved registers and
/// calls [`_intr_common_handler`] with a pointer to the cumulative state
/// structure.
#[unsafe(naked)]
#[no_mangle]
unsafe extern "C" fn _intr_reg_wrapper() -> ! {
    naked_asm!(
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
        "mov rax, cr2",
        "push rax",
        "mov rax, ds",
        "push rax",
        // call handler
        "mov rdi, rsp", // sysv64: rdi contains first argument
        "call _intr_common_handler",
        // restore registers
        "pop rax",
        "mov ds, rax",
        "pop rax", // pop cr2 into nowhere
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
        "pop rax", // was saved by trampoline
        "add rsp, 8", // error code
        // return from interrupt
        "iretq",
    );
}

/// The double fault is the only exception which does not have a standard
/// trampoline that jumps to an overridable handler. If a double fault happens,
/// something must have gone TERRIBLY wrong with the memory map.
/// 
/// This handler just prints "\n#DF" to the serial port without using memory and
/// loops forever.
#[unsafe(naked)]
unsafe extern "C" fn double_fault_isr() -> ! {
    naked_asm!(
        "mov dx, 0x3f8",
        "mov al, '\n'", "out dx, al",
        "mov al, '#'",  "out dx, al",
        "mov al, 'D'",  "out dx, al",
        "mov al, 'F'",  "out dx, al",
        "2:",
        "jmp 2b",
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
fn make_trampoline(vector: u8, handler: Option<IntrHandler>) -> [u8; TRAMPOLINE_SIZE] {
    // get function handler addresses
    let handler_addr: usize = match handler {
        None => 0,
        Some(handler) => handler as *const () as usize,
    };
    let wrapper_addr = _intr_reg_wrapper as *const () as usize;

    // insert addresses
    let mut code = TRAMPOLINE_SKELETON;
    code[5..13].copy_from_slice(&handler_addr.to_le_bytes());
    code[18..26].copy_from_slice(&wrapper_addr.to_le_bytes());
    code[15] = vector;

    if cpu_pushes_errcode(vector) {
        // remove the first instruction (two bytes)
        code[0] = NOP_INSTRUCTION; 
        code[1] = NOP_INSTRUCTION;
    }

    code
}

/// Checks that the handler address contained in a trampoline is non-zero
fn trampoline_handler_installed(trampoline: &[u8; TRAMPOLINE_SIZE]) -> bool {
    let address = u64::from_ne_bytes(trampoline[5..13].try_into().unwrap());
    address > 0
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

type AllIdtEntries = [IdtEntry; INTERRUPT_CNT];

#[allow(dead_code)]
#[repr(C, packed)]
struct RawTss {
    _rsvd0: u32,
    rsp: [VirtAddr; 3],
    _rsvd1: u64,
    ist: [VirtAddr; 7],
    _rsvd2: u64,
    _rsvd3: u16,
    iopb: u16,
}

#[allow(dead_code)]
struct RawIdt(*mut AllIdtEntries, *mut RawTss, CommonSelectors);

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

impl RawIdt {
    fn new(addr_space: &mut AddrSpace) -> RawIdt {
        // allocate IDT and TSS
        let idt = addr_space.modify().allocate_anywhere(
            size_of::<AllIdtEntries>().div_ceil(PAGE_SIZE),
            Default::default(),
            AllocReturn::Start
        ).unwrap().to_mut_ptr::<AllIdtEntries>();

        let tss = addr_space.modify().allocate_anywhere(
            size_of::<RawTss>().div_ceil(PAGE_SIZE),
            Default::default(),
            AllocReturn::Start
        ).unwrap().to_mut_ptr::<RawTss>();

        let selectors = Gdt::do_annoying_boilerplate_thing(addr_space, VirtAddr::from_mut_ptr(tss).unwrap());

        RawIdt(idt, tss, selectors)
    }

    unsafe fn set_as_current(&self) {
        let idtr = DescTableRegister::make(VirtAddr::from_mut_ptr(self.0).unwrap(), (size_of::<[IdtEntry; INTERRUPT_CNT]>() - 1) as u16);
        idtr.load(DescTableKind::Interrupt);
    }

    unsafe fn set_raw_isr(&mut self, emu_code: Selector, vector: u8, raw_isr: Option<unsafe extern "C" fn() -> !>) {
        match raw_isr {
            None => (*self.0)[vector as usize] = IdtEntry::new().with_present(false),
            Some(raw_isr) => {
                let handler_addr = raw_isr as usize;
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
pub struct IntrMgr {
    idt: RawIdt,
    selectors: CommonSelectors,
    trampolines: &'static mut [[u8; TRAMPOLINE_SIZE]],
}

impl IfIntrMgr for IntrMgr {
    /// Makes a new interrupt manager. Though several can be created per CPU,
    /// this is not very useful as their lifetimes are static and only one of
    /// them can be active at a time. Having one per CPU is the intended use
    /// case.
    fn new(addr_space: &mut AddrSpace) -> IntrMgr {
        let mut idt = RawIdt::new(addr_space);

        let pages = size_of::<[[u8; TRAMPOLINE_SIZE]; INTERRUPT_CNT]>().div_ceil(PAGE_SIZE);
        let tramp = addr_space.modify().allocate_anywhere(
            pages,
            Default::default(),
            AllocReturn::Start
        ).unwrap().to_mut_ptr::<[u8; TRAMPOLINE_SIZE]>();
        // SAFETY: this buffer has just been allocated by us, is of the correct
        // size, and is static.
        let tramp = unsafe { core::slice::from_raw_parts_mut(tramp, INTERRUPT_CNT) };

        for (i, tramp) in tramp.iter_mut().enumerate() {
            if i == (TargetException::DoubleFault as usize) {
                unsafe {
                    // SAFETY: the pointer points to valid executable code
                    idt.set_raw_isr(idt.2.emu_code, i as u8, Some(double_fault_isr));
                }
            } else {
                let code = make_trampoline(i as u8, None);
                *tramp = code;
                let f_ptr = tramp as *mut u8;
                unsafe {
                    // SAFETY: transmuting a thin pointer into another thin pointer
                    let f_ptr: unsafe extern "C" fn() -> ! = core::mem::transmute(f_ptr);
                    // SAFETY: the pointer points to valid executable code
                    idt.set_raw_isr(idt.2.emu_code, i as u8, Some(f_ptr));
                }
            }
        }

        let selectors = idt.2;
        IntrMgr { idt, selectors, trampolines: tramp }
    }

    unsafe fn set_as_current(&self) {
        self.idt.set_as_current();
        self.selectors.emu_code.set(SelectorRegister::Code);
        self.selectors.emu_data.set(SelectorRegister::Data);
        self.selectors.emu_data.set(SelectorRegister::Stack);
    }

    fn set_handler(&mut self, vector: IntrVector, handler: Option<IntrHandler>) -> Result<()> {
        let vectors = vector.to_platform_vectors();
        match vectors {
            Either::Right(x) => {
                self.trampolines[x as usize] = make_trampoline(x, handler);
            },
            Either::Left(multiple) => {
                for x in multiple {
                    self.trampolines[*x as usize] = make_trampoline(*x, handler);
                }
            }
        }

        Ok(())
    }

    fn allocate_external_line(&mut self, handler: IntrHandler) -> Result<IntrVector> {
        for i in (MIN_EXTERNAL as usize) .. INTERRUPT_CNT {
            if !trampoline_handler_installed(&self.trampolines[i]) {
                let vector = IntrVector::External(i as u8);
                self.set_handler(vector, Some(handler))?;
                return Ok(vector);
            }
        }
        Err(Error::NoFreeLines)
    }

    fn disable_external(&mut self) {
        unsafe { asm!("cli") };
    }

    fn enable_external(&mut self) {
        unsafe { asm!("sti") };
    }
}

/// Non-UB platform-specific footguns. Used by [`interface::tests`] to test
/// exception handling.
pub(crate) mod throw_exception {
    // All these things are only considered UB in higher-level languages such as
    // C and Rust. When written in assembly, the behavior is very much defined
    // by the ISA specification. These things throw exceptions which the
    // [`tests`] module tests for.

    use core::arch::asm;
    use super::CpuState;

    pub fn skip_faulting_instruction(state: &mut CpuState) {
        state.rip += state.rcx;
    }

    pub fn divide_by_zero() {
        unsafe {
            // SAFETY: read module comment
            asm!(
                "xor rax, rax",
                "div rax",
                out("rax") _,
                out("rdx") _,
                in("rcx") 3,
            );
        }
    }

    pub fn dereference_null() {
        unsafe {
            // SAFETY: read module comment
            asm!(
                "xor {0:r}, {0:r}",
                "mov {0:r}, [{0:r}]",
                out(reg) _,
                in("rcx") 3,
            );
        }
    }

    pub fn illegal_instruction() {
        unsafe {
            // SAFETY: read module comment
            asm!(
                "ud2",
                in("rcx") 2,
            );
        }
    }
}
