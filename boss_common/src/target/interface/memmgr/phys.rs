//! Implements the generic physical memory allocator that's expected to work on
//! all platforms.

use bincode::{Encode, Decode};
use core::ops::Range;
use crate::target::{current::memmgr as concrete, interface::memmgr::{self as interface, MemoryParameters, PhysMemStats}};
use crate::util::byte_size::ByteSize;
use alloc::borrow::ToOwned;
use interface::PhysMemRange;
use interface::{VirtAddr as IfVirtAddr, PhysAddr as IfPhysAddr};
use concrete::{PhysAddr, VirtAddr};

/// This struct appears at the start of every page range and contains, mainly,
/// the bitmap of the range. This bitmap directly follows this struct in memory,
/// but its size may be extended while the PMM is initializing, so it's not kept
/// in the struct directly.
/// 
/// Range 0 also contains the `all_ranges` slice, containing physical addresses
/// of all the ranges, including itself. A reference to this slice is kept in
/// the physical allocator struct, not in the page range header struct. However,
/// this struct contains a flag that's set if the range contains this slice. It
/// is used to make extend() safer by refusing the operation if the resulting
/// bitmap would overlap this slice.
/// 
/// This is how the page range looks:
/// ```not-code
/// Page boundaries:
/// +-----------------------+-----------------------+-------------
/// |                       |                       |  ...
/// +-----------------------+-----------------------+-------------
/// Data:
/// +-----------+-----+--+-------------+------------+-------------
/// | PageRange | Bit |xx| ALL_RANGES  |xx unused xx|  ...
/// | Header    | map |xx| (for rng.0) |xxxxxxxxxxxx|
/// +-----------+-----+--+-------------+------------+-------------
///                      ^ PRE_AR_GAP
/// 
/// |---------------- HEADER PAGES -----------------|--- ALLOCATED ...
/// ```
struct PageRangeHeader {
    /// Size of the range, including this header, in pages
    pages: usize,
    /// Size of this header in pages
    header_pages: usize,
    /// Bitmap for this range
    bitmap: PhysAddr,
    /// Whether this range contains the `all_ranges` slice
    has_all_ranges: bool,
}

type BitmapUnit = usize;
const BM_UNIT_BITS: usize = BitmapUnit::BITS as usize;
const BM_ALL_BITS_SET: BitmapUnit = BitmapUnit::MAX;

const PAGE_SIZE: usize = concrete::MemoryParameters::PAGE_SIZE;
/// The gap, in bytes, that will be kept between the start of the page range
/// and the `ALL_RANGES` slice it contains. Effectively determines how many
/// pages (at most) the first range may contain.
const PRE_AR_GAP: usize = PAGE_SIZE / 2;
const MAX_RANGES: usize = (PAGE_SIZE - PRE_AR_GAP) / size_of::<PhysAddr>();

impl PageRangeHeader {
    /// Gets the bitmap of an arbitrary range
    fn get_bitmap_at<'a>(addr: PhysAddr, pages: usize) -> &'a mut [BitmapUnit] {
        let virt: VirtAddr = addr.try_into().unwrap();
        let units = pages.div_ceil(BM_UNIT_BITS);
        unsafe { core::slice::from_raw_parts_mut(virt.to_mut_ptr(), units) }
    }

    /// Gets the bitmap of the range
    fn get_bitmap(&mut self) -> &mut [BitmapUnit] {
        Self::get_bitmap_at(self.bitmap, self.pages)
    }

    /// Sets a bit in a bitmap to a value
    fn bitmap_set(bitmap: &mut [BitmapUnit], idx: usize, value: bool) {
        let unit = idx / BM_UNIT_BITS;
        let bit = idx % BM_UNIT_BITS;
        bitmap[unit] = (bitmap[unit] & !(1usize << bit)) | ((value as usize) << bit);
    }

    /// Gets the bit in a bitmap
    fn bitmap_is_set(bitmap: &mut [BitmapUnit], idx: usize) -> bool {
        let unit = idx / BM_UNIT_BITS;
        let bit = idx % BM_UNIT_BITS;
        (bitmap[unit] >> bit) & 1 == 1
    }

    /// Creates a header and the bitmap it needs
    fn new(desc: &PhysMemRange) -> Option<&'static mut PageRangeHeader> {
        // construct bitmap
        let bitmap_start = PhysAddr::from_usize(desc.start.to_usize() + size_of::<PageRangeHeader>()).unwrap();
        let bitmap = Self::get_bitmap_at(bitmap_start, desc.page_count);
        bitmap.fill(0);

        // mark header pages as used
        let header_pages = (size_of::<PageRangeHeader>() + size_of_val(bitmap)).div_ceil(PAGE_SIZE);
        for i in 0..header_pages {
            Self::bitmap_set(bitmap, i, true);
        }

        if header_pages < desc.page_count {
            // construct header and place it at the start of the range
            let header = PageRangeHeader {
                pages: desc.page_count,
                header_pages,
                bitmap: bitmap_start,
                has_all_ranges: false,
            };
            let header_ptr: VirtAddr = desc.start.try_into().unwrap();
            let header_ptr = header_ptr.to_mut_ptr();
            unsafe {
                *header_ptr = header;
                Some(header_ptr.as_uninit_mut().unwrap().assume_init_mut())
            }
        } else {
            None // no point in having a range that's just entirely metadata
        }
    }

    /// Returns the starting address of the range
    pub fn start(&self) -> PhysAddr {
        VirtAddr::from_ref(self).unwrap().try_into().unwrap()
    }

    /// Returns the end address of the range (not inclusive)
    pub fn end(&self) -> PhysAddr {
        PhysAddr::from_usize(self.start().to_usize() + (self.pages * PAGE_SIZE)).unwrap()
    }

    /// Returns the range of allocatable page indices
    fn allocatable_idxs(&self) -> Range<isize> {
        self.header_pages as isize .. self.pages as isize
    }

    /// Extends a page range by a number of pages
    fn extend(&mut self, xtd_by: usize) -> Result<(), interface::Error> {
        // calculate sizes
        let new_bm_size = (self.pages + xtd_by).div_ceil(BM_UNIT_BITS);
        let max_allowed_size = if self.has_all_ranges {
            PRE_AR_GAP
        } else {
            let mut consecutive_free_pages_after_header = 0usize;
            let range = self.header_pages..self.pages;
            let old_bitmap = self.get_bitmap();
            for i in range {
                if !Self::bitmap_is_set(old_bitmap, i) {
                    consecutive_free_pages_after_header += 1;
                } else {
                    break;
                }
            }
            consecutive_free_pages_after_header
        };

        // extended bitmap
        if size_of::<Self>() + new_bm_size >= max_allowed_size {
            return Err(interface::Error::ExtensionImpossible);
        }
        self.header_pages = (size_of::<PageRangeHeader>() + new_bm_size).div_ceil(PAGE_SIZE);
        self.pages += xtd_by;

        // mark new header pages as used
        let range = 0..self.header_pages;
        let new_bitmap = self.get_bitmap();
        for i in range {
            Self::bitmap_set(new_bitmap, i, true);
        }

        Ok(())
    }

    /// Creates the `all_ranges` slice in this range header
    fn create_all_ranges(&mut self) -> Result<PhysAddr, interface::Error> {
        if self.has_all_ranges {
            return Err(interface::Error::AlreadyContainsAllRanges);
        }

        let header_size = size_of::<Self>() + (self.pages.div_ceil(BM_UNIT_BITS) * size_of::<usize>());
        if header_size > PRE_AR_GAP {
            return Err(interface::Error::AllRangesCreationImpossible);
        }

        self.has_all_ranges = true;
        let start = self.start().to_usize();
        PhysAddr::from_usize(start + PRE_AR_GAP)
    }

    /// Streams page allocations, returning their starting addresses.
    fn allocate(&mut self) -> impl Iterator<Item = PhysAddr> + use<'_> {
        let start_index = self.header_pages;
        RangeAllocIter {
            range: self,
            page_index: start_index,
        }
    }

    fn allocate_contiguous(&mut self, requested_pages: usize, selector: impl Fn(PhysAddr) -> bool) -> Option<PhysAddr> {
        let mut page_idx = self.header_pages;
        let mut current_run: Option<(usize, usize)> = None;
        let range_start = self.start().to_usize();
        let pages = self.pages;
        let bitmap = self.get_bitmap();

        loop {
            if page_idx >= pages { break; }
            if let Some((_, length)) = current_run && length >= requested_pages { break; }

            let unit = page_idx / BM_UNIT_BITS;
            let bit = page_idx % BM_UNIT_BITS;
            if (bit == 0) && (bitmap[unit] == BM_ALL_BITS_SET) {
                page_idx = page_idx.div_ceil(BM_UNIT_BITS) * BM_UNIT_BITS;
                current_run = None;
                continue;
            }

            if Self::bitmap_is_set(bitmap, page_idx) {
                page_idx += 1;
                current_run = None;
                continue;
            }

            if let Some((start, length)) = current_run {
                current_run = Some((start, length + 1));
            } else {
                let addr = PhysAddr::from_usize(range_start + (page_idx * PAGE_SIZE)).unwrap();
                if selector(addr) {
                    current_run = Some((page_idx, 1));
                }
            }
            page_idx += 1;
        }

        if let Some((start, length)) = current_run && length >= requested_pages {
            for page in start .. (start + length) {
                Self::bitmap_set(bitmap, page, true);
            }
            let addr = PhysAddr::from_usize(range_start + (start * PAGE_SIZE)).unwrap();
            #[cfg(feature = "trace-pmm")]
            log::trace!("alloc_cont {addr:?}+{requested_pages:?} from range {:?}+{:?}", self.start(), self.pages);
            Some(addr)
        } else {
            None
        }
    }

    /// Gets `(internally_used_memory, total_memory)`
    fn stats(&self) -> (ByteSize, ByteSize) {
        (
            ByteSize(self.header_pages * PAGE_SIZE),
            ByteSize(self.pages * PAGE_SIZE),
        )
    }

    /// Tries to deallocate a page
    fn deallocate_page(&mut self, page: PhysAddr) -> bool {
        let page_idx = (page.to_usize() as isize - self.start().to_usize() as isize) / PAGE_SIZE as isize;

        if !self.allocatable_idxs().contains(&page_idx) { return false };

        #[cfg(feature = "trace-pmm")]
        log::trace!("dealloc {page:?} from range {:?}+{:?}", self.start(), self.pages);
        Self::bitmap_set(self.get_bitmap(), page_idx.try_into().unwrap(), false);
        true
    }
}

/// An iterator that allocates pages within one range
struct RangeAllocIter<'r> {
    range: &'r mut PageRangeHeader,
    page_index: usize,
}

impl<'r> Iterator for RangeAllocIter<'r> {
    type Item = PhysAddr;

    fn next(&mut self) -> Option<Self::Item> {
        let pages = self.range.pages;
        let start = self.range.start().to_usize();
        let bitmap = self.range.get_bitmap();

        loop {
            if self.page_index >= pages {
                return None;
            }

            // skip entirely full 64-page ranges
            let unit = self.page_index / BM_UNIT_BITS;
            if bitmap[unit] == BM_ALL_BITS_SET {
                self.page_index = self.page_index.div_ceil(BM_UNIT_BITS) * BM_UNIT_BITS + 1;
                continue;
            }

            // skip individual full pages
            if PageRangeHeader::bitmap_is_set(bitmap, self.page_index) {
                self.page_index += 1;
                continue;
            }

            PageRangeHeader::bitmap_set(bitmap, self.page_index, true);
            let offset_from_range = self.page_index * PAGE_SIZE;
            let addr = PhysAddr::from_usize(start + offset_from_range).unwrap();
            self.page_index += 1;

            #[cfg(feature = "trace-pmm")]
            log::trace!("alloc {addr:?} from range {:?}+{:?}", self.range.start(), self.range.pages);

            return Some(addr)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.range.pages - self.page_index))
    }
}

/// A dynamic array of references to `PageRangeHeader`s
#[derive(Encode, Decode)]
struct MultiRangeHolder {
    all_ranges: Option<PhysAddr>,
    range_count: usize,
}

impl MultiRangeHolder {    
    fn get_ranges_slice(&mut self) -> Option<&mut [PhysAddr]> {
        let virt: VirtAddr = self.all_ranges?.to_owned().try_into().ok()?;
        let slice = unsafe { core::slice::from_raw_parts_mut(virt.to_mut_ptr(), self.range_count) };
        Some(slice)
    }

    fn get_all_ranges(&mut self) -> Option<impl Iterator<Item = &mut PageRangeHeader>> {
        Some(self
            .get_ranges_slice()?
            .iter()
            .map(|phys| {
                let virt: VirtAddr = phys.to_owned().try_into().unwrap();
                let ptr: *mut PageRangeHeader = virt.to_mut_ptr();
                unsafe { &mut *ptr }
            }))
    }

    fn register_range(&mut self, range: &mut PageRangeHeader) {
        assert!(self.range_count < MAX_RANGES);
        let ranges = self.all_ranges.get_or_insert_with(|| range.create_all_ranges().unwrap()).to_owned();
        let ranges: VirtAddr = ranges.try_into().unwrap();
        let ranges: *mut PhysAddr = ranges.to_mut_ptr();
        unsafe { ranges.add(self.range_count).write(range.start()) };
        self.range_count += 1;
    }
}

/// Generic physical allocator, expected to work on all platforms
#[derive(Encode, Decode)]
pub struct GenericPhysAlloc {
    ranges: MultiRangeHolder,
    stats: PhysMemStats,
}

impl super::PhysAlloc for GenericPhysAlloc {
    unsafe fn new(ranges: impl Iterator<Item = PhysMemRange>) -> interface::Result<Self> {
        let mut allocator = GenericPhysAlloc {
            ranges: MultiRangeHolder {
                all_ranges: None,
                range_count: 0,
            },
            stats: Default::default(),
        };

        unsafe { allocator.add_ranges(ranges) };
        Ok(allocator)
    }

    fn allocate(&mut self) -> interface::Result<impl Iterator<Item = PhysAddr>> {
        Ok(self.ranges.get_all_ranges()
            .ok_or(interface::Error::OutOfMemory)?
            .flat_map(|range| range.allocate())
            .inspect(|_| {
                let add_bytes = ByteSize(PAGE_SIZE);
                self.stats.allocated += add_bytes;
                self.stats.cumulative_allocated += add_bytes;
            }))
    }

    fn allocate_contiguous(&mut self, pages: usize) -> interface::Result<PhysAddr> {
        self.allocate_select(pages, |_| true)
    }

    fn allocate_select(&mut self, pages: usize, selector: impl Clone + Fn(PhysAddr) -> bool) -> interface::Result<concrete::PhysAddr> {
        self.ranges.get_all_ranges()
            .ok_or(interface::Error::OutOfMemory)?
            .find_map(|range| range.allocate_contiguous(pages, selector.clone()))
            .ok_or(interface::Error::OutOfMemory)
            .inspect(|_| {
                let add_bytes = ByteSize(PAGE_SIZE * pages);
                self.stats.allocated += add_bytes;
                self.stats.cumulative_allocated += add_bytes;
            })
    }

    fn deallocate(&mut self, pages: impl Iterator<Item = PhysAddr>) -> interface::Result<()> {
        let mut result = Ok(());
        let mut deallocated = 0usize;

        for page in pages {
            let mut dealloced_this_page = false;
            for range in self.ranges.get_all_ranges().unwrap() {
                if range.deallocate_page(page) {
                    deallocated += 1;
                    dealloced_this_page = true;
                    break;
                }
            }
            if !dealloced_this_page {
                result = Err(interface::Error::UnrecognizedDealloc);
            }
        }

        let free_bytes = ByteSize(PAGE_SIZE * deallocated);
        self.stats.allocated -= free_bytes;
        self.stats.cumulative_deallocated += free_bytes;

        result
    }

    unsafe fn add_ranges(&mut self, ranges: impl Iterator<Item = PhysMemRange>) {
        let phys_null = PhysAddr::from_usize(0).unwrap();

        for mut range in ranges {
            // we can't use the null page :(
            if range.start == phys_null {
                #[cfg(feature = "trace-pmm")]
                log::trace!("null page skipped");
                range.start = PhysAddr::from_usize(range.start.to_usize() + PAGE_SIZE).unwrap();
                range.page_count -= 1;
            }

            // try to extend last range
            let add_new_range = if let Some(all_ranges) = self.ranges.get_all_ranges() {
                let last_range = all_ranges.last().unwrap();
                if range.start == last_range.end() {
                    #[cfg(feature = "trace-pmm")]
                    log::trace!("extending last by {} pages", range.page_count);

                    let (internal, total) = last_range.stats();

                    if last_range.extend(range.page_count).is_ok() {
                        self.stats.internal -= internal;
                        self.stats.total -= total;
                        let (internal, total) = last_range.stats();
                        self.stats.internal += internal;
                        self.stats.total += total;
                        false
                    } else {
                        true
                    }
                } else {
                    true
                }
            } else {
                true
            };

            // add a new range if we have to
            if add_new_range {
                #[cfg(feature = "trace-pmm")]
                log::trace!("adding {range:?}");

                if let Some(range) = PageRangeHeader::new(&range) {
                    self.ranges.register_range(range);
                    let (internal, total) = range.stats();
                    self.stats.internal += internal;
                    self.stats.total += total;
                } else {
                    #[cfg(feature = "trace-pmm")]
                    log::trace!("useless range");
                }
            }
        }
    }

    fn stats(&self) -> PhysMemStats {
        self.stats.to_owned()
    }
}
