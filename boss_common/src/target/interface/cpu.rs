use crate::target::current::memmgr::*;

pub trait Cpu {
    /// Blocks until an interrupt is fired
    fn wait_for_interrupt();

    /// Calls a non-returning function at a fixed location in memory, with a new
    /// stack and a list of arguments
    /// 
    /// # Safety
    /// The CPU will start executing code from whatever address was provided
    /// here. The entire list of safety requirements would be quite tedious to
    /// outline, so don't do any stupid stuff.
    unsafe fn jump_to(entry: VirtAddr, stack: VirtAddr, args: &[usize]) -> !;
}
