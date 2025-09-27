//! BOSS target abstractions and utility tests

use core::ops::RangeInclusive;

use spin::Mutex;

use crate::target::{current::{
    device::wall_clock::*, interrupt::*, memmgr::*
}, interface::memmgr::tests::HEAP_TESTS};

use crate::target::interface::{
    memmgr::tests::{PHYS_ALLOC_TESTS, VIRT_MEM_TESTS},
    interrupt::tests::INTERRUPT_TESTS,
};

pub struct TestEnv<'r> {
    pub phys_alloc: &'r Mutex<PhysAlloc>,
    pub wall_clock: &'r WallClock,
    pub scratchpad_area: RangeInclusive<VirtAddr>,
    pub addr_space: &'r mut AddrSpace<'static>,
    pub intr_mgr: &'r mut IntrMgr,
}

/// Test functions must either return on success, or panic on failure. Use
/// regular [`assert!`] family macros.
/// 
/// This design choice was made for ease of debugging. Whenever a test panics,
/// it's easy to connect a debugger to QEMU and examine the state of the system.
pub type TestFn = fn(env: &mut TestEnv<'_>);
pub type TestSuite = &'static [(&'static str, TestFn)];

static SUITES: &[(&str, TestSuite)] = &[
    ("phys_alloc", PHYS_ALLOC_TESTS),
    ("virt_mem", VIRT_MEM_TESTS),
    ("interrupt", INTERRUPT_TESTS),
    ("heap", HEAP_TESTS),
];

pub fn run_all(env: &mut TestEnv<'_>) {
    let suites = SUITES.len();
    let total = SUITES.iter().flat_map(|(_, tests)| tests.iter()).count();
    log::info!("to run: {suites} suites, {total} tests");

    let mut test_idx = 0;
    for (suite, (s_name, tests)) in SUITES.iter().enumerate() {
        let suite = suite + 1;
        log::info!("  suite {suite}/{suites}: {s_name}:");

        let in_suite = tests.len();
        for (test, (t_name, t_fn)) in tests.iter().enumerate() {
            let test = test + 1;
            test_idx += 1;
            log::info!("    [{test_idx}/{total}] test {test}/{in_suite}: {t_name}");
            t_fn(env);
        }
    }

    log::info!("{total} / {total} tests PASSED");
}
