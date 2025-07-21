use core::ops::Deref;
use spin::Mutex;

struct BootStageStack {
    stack: [usize; 4],
    level: usize,
}

static STAGE_STACK: Mutex<BootStageStack> = Mutex::new(BootStageStack { stack: [0, 0, 0, 0], level: 0 });

fn log_stage(stack: &BootStageStack, stage: &str) {
    let [a, b, c, d] = stack.stack;

    // i'm sorry
    match stack.level {
        0 => log::debug!("{a}. {stage}"),
        1 => log::debug!("{a}.{b}. {stage}"),
        2 => log::debug!("{a}.{b}.{c}. {stage}"),
        3 => log::debug!("{a}.{b}.{c}.{d}. {stage}"),
        _ => panic!("max depth exceeded"),
    }
}

pub fn new_level() {
    let mut stack = STAGE_STACK.lock();

    stack.level += 1;
    let depth = stack.level;
    stack.stack[depth] = 1;
}

pub fn same_level(stage: &str) {
    let mut stack = STAGE_STACK.lock();
    let depth = stack.level;
    stack.stack[depth] += 1;
    log_stage(stack.deref(), stage);
}

pub fn end_level() {
    let mut stack = STAGE_STACK.lock();
    stack.level -= 1;
}
