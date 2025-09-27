use crate::target::current::memmgr::*;
pub use crate::target::interface::cpu::Cpu as IfCpu;

use core::arch::asm;

pub struct Cpu;

impl IfCpu for Cpu {
    fn wait_for_interrupt() {
        unsafe { asm!("hlt") };
    }

    unsafe fn jump_to(entry: VirtAddr, stack: VirtAddr, args: &[usize]) -> ! {
        let stack = stack.to_usize();
        let entry = entry.to_usize();

        let mut all_args = [0usize; 6];
        assert!(args.len() <= all_args.len());
        for (i, arg) in args.iter().enumerate() {
            all_args[i] = *arg;
        }

        asm!(
            "mov rsp, {stack}", // in("rsp") and in("rbp") not allowed :(
            "mov rbp, {stack}",
            "push {jump_ptr}",
            "ret",
            stack = in(reg) stack,
            jump_ptr = in(reg) entry,
            in("rdi") all_args[0],
            in("rsi") all_args[1],
            in("rdx") all_args[2],
            in("rcx") all_args[3],
            in("r8") all_args[4],
            in("r9") all_args[5],
            options(noreturn),
        );
    }
}
