//! Generic interface for a clock

/// Represents a time duration
pub trait Duration: Sized + Copy {
    /// Converts the duration to picoseconds
    fn ps(self) -> u128;
    /// Converts the duration to nanoseconds
    fn ns(self) -> u64;
    /// Converts the duration to microseconds
    fn us(self) -> u64;
    /// Converts the duration to milliseconds
    fn ms(self) -> u64;
}

/// Counts absolute time since initialization
pub trait WallClock: Copy {
    /// Creates a new clock
    /// 
    /// This operation will usually calibrate the clock using another source, so
    /// it's only recommended to call this function once, then use the resulting
    /// object in other places.
    fn calibrate_new() -> Self;
    /// Creates a new clock
    /// 
    /// Unlike [`WallClock::calibrate_new`], takes calibration data from the
    /// provided clock. Like the aforementioned function, counts time relative
    /// to the creation of the clock.
    fn new_with_calib(calib_source: &Self) -> Self;

    /// Gets clock resolution in picoseconds
    /// 
    /// Resolution does not mean accuracy.
    fn resolution_ps(&self) -> u64;

    /// Gets absolute time since the clock was created
    fn abs_time(&self) -> impl Duration;

    /// Measures the time taken to execute the provided closure
    fn delta_time<T>(&self, f: impl FnOnce() -> T) -> (T, impl Duration);
}
