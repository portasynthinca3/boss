use spin::RwLock;

/// Some components of the emulator support operation in different modes
/// depending on what other components have been initialized. A checkpoint
/// represents a change in status during boot that these components might be
/// interested in.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Checkpoint {
    /// Start of the initialization process
    EmuLoaded,
    /// Base image loaded as a binary buffer, but not yet parsed
    BaseImageLoaded,
    /// Exited boot services
    BootServicesExited,
    /// Physical page allocator available
    PhysMemMgr,
    /// Relocation phase 1 (dual mapping)
    RelocDualMapping,
    /// Relocation phase 2 (execution in upper half)
    RelocUpperExec,
    /// Interrupts available
    Interrupts,
    /// Relocation done (lower half dropped)
    RelocDone,
    /// Heap allocation available
    Heap,
    /// All CPUs online
    SmpOnline,
    /// BEAM structures initialized
    BeamInitd,
    /// Initial argument constructed
    BeamInitArg,
    /// Created process running `main:main/1` from the base image
    Running,
}

/// This status is global and advances as the bootstrap processor initializes
/// the emulator.
static CHECKPOINT: RwLock<Checkpoint> = RwLock::new(Checkpoint::EmuLoaded);

/// Advances the checkpoint. It is expected that the status supplied to this
/// function is a logical continuation of the current one.
pub fn advance(new: Checkpoint) -> Result<(), ()> {
    let guard = CHECKPOINT.upgradeable_read();
    if *guard > new { return Err(()); }
    if *guard == new { return Ok(()); }

    let mut guard = guard.upgrade();
    *guard = new;
    log::info!("checkpoint: {new:?}");

    Ok(())
}

/// Gets the current checkpoint
pub fn get() -> Checkpoint {
    let guard = CHECKPOINT.read();
    *guard
}
