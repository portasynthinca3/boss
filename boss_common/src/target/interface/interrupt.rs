//! Abstract interrupt manager interface

use crate::target::current::{
    memmgr::*,
};

use crate::target::current::interrupt as concrete;

/// Interrupt vector number
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrVector {
    ArithmeticError,
    Breakpoint,
    InvalidAccess,
    HardwareFailure,
    IllegalInstruction,
    PlatformSpecificException(u8),
    External(u8),
}

/// Complete CPU state when an interrupt is fired
pub trait CpuState: core::fmt::Debug + Clone {
    /// Information about the CPU exception that happened. Returns `None` if the
    /// interrupt is external, not an exception.
    fn exception_info(&self) -> Option<ExceptionInfo>;

    /// Human-readable name of the CPU exception that happened, as defined by
    /// the platform. Returns `None` if the interrupt is external, not an
    /// exception.
    fn platform_exception_name(&self) -> Option<&'static str>;

    /// Gets the program counter value at interrupt time
    fn program_counter(&self) -> Option<VirtAddr>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessError {
    UserMode { write: bool, page_present: bool },
    SupervisorMode { write: bool, page_present: bool },
    Alignment,
    StackCorruption,
}

#[derive(Debug, Clone, Copy)]
pub enum ExceptionInfo {
    ArithmeticError,
    Breakpoint,
    InvalidAccess(Option<AccessError>, Option<VirtAddr>),
    HardwareFailure,
    IllegalInstruction,
    PlatformSpecific(u8),
}

/// An interrupt handler function. Must not be a closure with captured data.
/// The handler can modify the provided CPU state, which will be applied once it
/// returns.
pub type IntrHandler = fn(cpu_state: &mut concrete::CpuState);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    UnsupportedOnPlatform,
    NoFreeLines,
}

pub type Result<T> = core::result::Result<T, Error>;

pub trait IntrMgr {
    fn new(addr_space: &mut AddrSpace) -> Self;

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
    unsafe fn set_as_current(&self);

    /// Sets a handler for an interrupt
    fn set_handler(&mut self, vector: IntrVector, handler: Option<IntrHandler>) -> Result<()>;

    /// Atomically allocates an external interrupt number and assigns the
    /// provided interrupt handler
    fn allocate_external_line(&mut self, handler: IntrHandler) -> Result<IntrVector>;

    /// Disables external interrupts. It is recommended to only keep them that
    /// way for a very short amount of time, enabling them back with
    /// [`enable_external`][`IntrMgr::enable_external`] as soon as possible. Or
    /// use [`with_disabled_external`][`IntrMgr::with_disabled_external`], which
    /// will manage this for you.
    fn disable_external(&mut self);

    /// Enables external interrupts after they have been disabled by
    /// [`disable_external`][`IntrMgr::disable_external`].
    fn enable_external(&mut self);

    /// Runs the provided closure with disabled external interrupts
    fn with_disabled_external<T>(&mut self, f: impl FnOnce() -> T) -> T {
        self.disable_external();
        let ret = f();
        self.enable_external();
        ret
    }
}

// ==========
// Test suite
// ==========

/// Interrupt manager test suite
pub mod tests {
    use crate::tests::*;
    use crate::target::current::interrupt::throw_exception;
    use super::*;
    use spin::Mutex;

    static SUCCESS: Mutex<bool> = Mutex::new(false);

    fn test_exception(env: &mut TestEnv<'_>, vector: IntrVector, thrower: fn()) {
        *SUCCESS.lock() = false;
        env.intr_mgr.set_handler(vector, Some(|state| {
            *SUCCESS.lock() = true;
            throw_exception::skip_faulting_instruction(state);
        })).unwrap();
        thrower();
        assert!(*SUCCESS.lock());
        env.intr_mgr.set_handler(vector, None).unwrap();
    }

    fn zero_div(env: &mut TestEnv<'_>) {
        test_exception(env, IntrVector::ArithmeticError, throw_exception::divide_by_zero);
    }

    fn null_deref(env: &mut TestEnv<'_>) {
        test_exception(env, IntrVector::InvalidAccess, throw_exception::dereference_null);
    }

    fn illegal_insn(env: &mut TestEnv<'_>) {
        test_exception(env, IntrVector::IllegalInstruction, throw_exception::illegal_instruction);
    }

    pub static INTERRUPT_TESTS: TestSuite = &[
        ("zero_div", zero_div),
        ("null_deref", null_deref),
        ("illegal_insn", illegal_insn),
    ];
}
