//! Processor-local storage
//! 
//! Like many other kernels, the emulator stores a pointer to the per-processor
//! structure in the `IA32_GS_BASE` MSR. Other kernels also typically use the GS
//! segment register override prefix to fetch data from the struct located at
//! the address contained in the MSR, but that is not done here.

use alloc::boxed::Box;
use crate::ll::msr::{IA32Msr, Msr};

/// Processor-local storage
pub struct Pls {
    
}

impl Pls {
    /// Initializes the processor-local storage
    /// # Safety
    /// This method must be called **exactly once** before [Self::get] is
    /// called. Calling [Self::get] before this method is called in unsafe.
    /// Calling [Self::init] again after it has already been called is unsafe.
    unsafe fn init() {
        let pls: &'static Pls = Box::leak(Box::new(Pls { }));
        let pointer = pls as *const Pls;
        // SAFETY: as long as this register is only being used by us (here and
        // in [Self::get]), overwriting this register is safe.
        IA32Msr::GsBase.write(pointer as u64);
    }

    /// Gets a shared reference to the processor-local storage
    fn get() -> &'static Self {
        let pointer = IA32Msr::GsBase.read() as *const Pls;
        unsafe {
            // SAFETY: the pointer that we fetched is valid if [Self::init]
            // initialized it. If it didn't, tough luck \(-_-)/
            pointer.as_uninit_ref().unwrap().assume_init_ref()
        }
    }
}
