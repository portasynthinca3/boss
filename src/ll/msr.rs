//! Model-Specific Registers
//! 
//! These registers are mostly used for CPU configuration and thus are
//! inherently  unsafe.

use core::arch::asm;

use num_enum::IntoPrimitive;

/// Trait for convenience enums which represent MSRs
pub trait Msr: Sized + Into<u32> {
    /// Writes a 64-bit value into an MSR
    /// # Safety
    /// Safety depends on what register is being written to. Some registers are
    /// harmless and will not cause undefined behavior when accessed (such as
    /// `IA32_TSC_ADJUST`), others fundamentally change the configuration of the
    /// CPU and thus are very hazardous (e.g. `IA32_EFER`), and others still
    /// BOSS uses to store its state (e.g. `IA32_GS_BASE`).
    unsafe fn write(self, value: u64) {
        let address: u32 = self.into();
        // SAFETY: read contract in docstring
        asm!(
            "wrmsr",
            in("ecx") address,
            in("eax") value & 0xffff_ffff,
            in("edx") (value >> 32) & 0xffff_ffff,
        );
    }

    /// Reads a 64-bit value from an MSR
    fn read(self) -> u64 {
        let address: u32 = self.into();
        let value_lo: u64;
        let value_hi: u64;
        // SAFETY: reading an MSR does not cause any side effects and thus
        // cannot cause memory safety errors
        unsafe {
            asm!(
                "rdmsr",
                in("ecx") address,
                out("rax") value_lo,
                out("rdx") value_hi,
            );
        }
        (value_hi << 32) | value_lo
    }
}

/// The so-called "architectural" "model-specific" registers. They're not really
/// model-specific, they're present on every CPU that this code can execute on.
#[repr(u32)]
#[derive(Clone, Copy, PartialEq, IntoPrimitive)]
pub enum IA32Msr {
    /// `IA32_FS_BASE`: FS segment base address
    FsBase = 0xC000_0100,
    /// `IA32_GS_BASE`: GS segment base address
    GsBase = 0xC000_0101,
}
impl Msr for IA32Msr { }
