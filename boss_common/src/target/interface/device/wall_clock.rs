//! Generic interface for a clock

/// Represents a time duration
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration(pub(crate) u128);
impl Duration {
    pub fn from_ps(ps: u128) -> Self { Self(ps) }
    pub fn from_ns(ns: u64) -> Self { Self((ns as u128) * 1_000u128) }
    pub fn from_us(us: u64) -> Self { Self((us as u128) * 1_000_000u128) }
    pub fn from_ms(ms: u64) -> Self { Self((ms as u128) * 1_000_000_000u128) }
    pub fn ps(self) -> u128 { self.0 }
    pub fn ns(self) -> u64 { (self.0 / 1_000u128) as _ }
    pub fn us(self) -> u64 { (self.0 / 1_000_000u128) as _ }
    pub fn ms(self) -> u64 { (self.0 / 1_000_000_000u128) as _ }
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
    fn abs_time(&self) -> Duration;

    /// Measures the time taken to execute the provided closure
    fn delta_time<T>(&self, f: impl FnOnce() -> T) -> (T, Duration);

    fn delay(&self, duration: Duration);
}
