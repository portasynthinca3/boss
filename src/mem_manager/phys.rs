//! Physical Memory Manager. Allocates and frees physical pages using a memory
//! map supplied to it by the main module.

// FIXME: there's a mishmash of physical addresses, virtual addresses and
// pointers and references based on the latter. This would have to be fixed once
// higher-half migration is implemented.

use core::{mem::{size_of, size_of_val}, ops::Range};
use uefi::table::boot::{MemoryMap, MemoryDescriptor, MemoryType};
use spin::RwLock;
use crate::mem_manager::{ByteSize, PhysAddr};

/// This struct appears at the start of every page range and contains, mainly,
/// the bitmap of the range. This bitmap directly follows this struct in memory,
/// but its size may be extended while the PMM is initializing, so it's not kept
/// in the struct directly.
/// 
/// Range 0 also contains the `ALL_RANGES` slice, containing mutable references
/// to all the ranges, including itself. The reference to this slice is kept in
/// a static variable behind a lock, not in the header struct. However, this
/// struct contains a flag that's set if the range contains this slice. It is
/// used to make extend() a little safer by refusing the operation if the
/// resulting bitmap would overlap this slice.
/// 
/// This is how the page range looks:
/// ```
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
    /// size of the range, including this header, in pages
    pages: usize,
    /// size of this header in pages
    header_pages: usize,
    /// bitmap for this range
    bitmap: &'static mut [u8],
    /// whether this range contains the `ALL_RANGES` slice
    has_all_ranges: bool,
}

/// Reference to all the page ranges
type AllRanges = &'static mut [&'static mut PageRangeHeader];
static ALL_RANGES: RwLock<Option<AllRanges>> = RwLock::new(None);

// The following constants are settings for the allocator.

/// The gap, in bytes, that will be kept between the start of the page range
/// and the `ALL_RANGES` slice it contains.
const PRE_AR_GAP: usize = 2048;
/// The maximum number of pages that can be processed by `allocate()` or
/// `deallocate()`
const MAX_ALLOC_BATCH: usize = 16;
/// The size of the page. This MUST be set to 4K unless there's some major
/// tomfoolery going on.
const PAGE_SIZE: usize = 4096;
/// Invalid physical address marker for debugging
const INVALID_PHYS: PhysAddr = 0xffffdeadcafebabe;

/// Since the PMM cannot access the VMM, this struct serves as a makeshift `Vec`
/// that allows `allocate` and `deallocate` to process multiple pages at once.
/// 
/// The contents of `buffer` starting with element number `count` are undefined.
#[derive(Clone)]
pub struct AllocPages {
    buffer: [PhysAddr; MAX_ALLOC_BATCH],
    count: usize
}

impl AllocPages {
    fn merge(&mut self, other: &AllocPages) {
        let to_move = core::cmp::min(other.count, MAX_ALLOC_BATCH - self.count);
        for i in 0..to_move {
            self.buffer[self.count + i] = other.buffer[i];
        }
        self.count += to_move;
    }
}

impl core::fmt::Debug for AllocPages {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "AllocPages[")?;
        for i in 0..self.count {
            write!(f, "{:#018x}", self.buffer[i])?;
            if i != self.count - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

impl PageRangeHeader {
    /// Sets a bit in a bitmap
    fn bitmap_set(bitmap: &mut [u8], idx: usize, value: bool) {
        let byte = idx / 8;
        let bit = idx % 8;
        if value {
            bitmap[byte] |= 1 << bit;
        } else {
            bitmap[byte] &= !(1 << bit);
        }
    }

    /// Creates a header and the bitmap it needs
    fn new(desc: &MemoryDescriptor) -> &'static mut PageRangeHeader {
        // construct bitmap
        let bm_size = desc.page_count.div_ceil(8);
        let bm_ptr = (desc.phys_start as usize + size_of::<PageRangeHeader>()) as *mut u8;
        let bitmap = unsafe { core::slice::from_raw_parts_mut(bm_ptr, bm_size as usize) };

        // mark header pages as used
        let header_pages = (size_of::<PageRangeHeader>() + bm_size as usize).div_ceil(PAGE_SIZE);
        for i in 0..header_pages {
            Self::bitmap_set(bitmap, i, true);
        }

        // construct header and place it at the start of the range
        let header = PageRangeHeader {
            pages: desc.page_count as usize,
            header_pages,
            bitmap,
            has_all_ranges: false,
        };
        let header_ptr = desc.phys_start as *mut PageRangeHeader;
        unsafe {
            *header_ptr = header;
            &mut *header_ptr
        }
    }

    /// Returns the starting address of the range
    fn start(&self) -> PhysAddr {
        self as *const Self as PhysAddr
    }

    /// Returns the range of allocatable page indices
    fn allocatable_idxs(&self) -> Range<isize> {
        return self.header_pages as isize .. self.pages as isize - 1;
    }

    /// Extends a page range by a number of pages. This operation is refused in
    /// case it requires the bitmap to be extended by such an amount that it
    /// will overwrite the `ALL_PAGES` slice (if the range has one)
    fn extend(&mut self, xtd_by: usize) -> Result<(), ()> {
        // extend bitmap
        let bm_size = (self.pages + xtd_by).div_ceil(8);
        if size_of::<Self>() + bm_size >= PRE_AR_GAP {
            return Err(());
        }
        let new_bitmap = unsafe { core::slice::from_raw_parts_mut(self.bitmap.as_mut_ptr(), bm_size) };

        // mark new header pages as used
        let new_header_pages = (size_of::<PageRangeHeader>() + bm_size).div_ceil(PAGE_SIZE);
        for i in 0..new_header_pages {
            Self::bitmap_set(new_bitmap, i, true);
        }

        // modify header fields
        self.bitmap = new_bitmap;
        self.header_pages = new_header_pages;
        self.pages += xtd_by;
        return Ok(());
    }

    /// Creates the `ALL_RANGES` slice. This operation is refused in case the
    /// slice has already been created.
    fn create_all_ranges(&mut self) -> Result<(), ()> {
        // check if the slice has already been created
        let guard = ALL_RANGES.upgradeable_read();
        match *guard {
            None => (),
            _ => return Err(()),
        }

        // create the slice
        let self_size = size_of::<Self>() + self.bitmap.len();
        let ar_addr = ((self as *const Self as usize) + self_size).div_ceil(PRE_AR_GAP) * PRE_AR_GAP;
        let ar_ptr = ar_addr as *mut &'static mut PageRangeHeader;
        let all_ranges = unsafe { core::slice::from_raw_parts_mut(ar_ptr, 0) };
        
        // make it globally known
        self.has_all_ranges = true;
        let mut guard = guard.upgrade();
        *guard = Some(all_ranges);
        return Ok(());
    }

    /// Makes the page range available by adding it to the global `ALL_RANGES`
    /// slice.
    fn make_available(&'static mut self) {
        // obtain the previous slice
        let mut guard = ALL_RANGES.write();
        let ranges = guard.as_mut().expect("can't make a page available: ALL_RANGES is None");

        // extend the slice
        let ar_ptr = (*ranges).as_mut_ptr();
        let ranges = unsafe { core::slice::from_raw_parts_mut(ar_ptr, ranges.len() + 1) };
        ranges[ranges.len() - 1] = self;

        // write the new slice
        *guard = Some(ranges);
    }

    /// Marks the pages used by the `ALL_RANGES` slice as used. This operation
    /// is refused in case the range doesn't contain that slice.
    fn extend_header(&mut self, size_of_ar: usize) -> Result<(), ()> {
        if !self.has_all_ranges {
            return Err(());
        }

        let new_header_pages = (
            (size_of::<PageRangeHeader>()
            + self.bitmap.len() as usize).div_ceil(PRE_AR_GAP) * PRE_AR_GAP
            + size_of_ar
        ).div_ceil(PAGE_SIZE);

        for i in 0..new_header_pages {
            Self::bitmap_set(self.bitmap, i, true);
        }

        self.header_pages = new_header_pages;
        return Ok(());
    }

    /// Tries to allocate several pages, returning their starting addresses.
    fn allocate(&mut self, mut count: usize) -> AllocPages {
        let mut result = AllocPages { buffer: [INVALID_PHYS; MAX_ALLOC_BATCH], count: 0 };
        let start = self.start();

        // walk through the bitmap
        'outer: for (byte_idx, content) in self.bitmap.iter_mut().enumerate() {
            'inner: loop {
                if *content == 0xff { break 'inner; } // move to the next byte
                if count == 0 { break 'outer; } // we're done

                // find the free page
                let free_bit = content.trailing_ones() as usize;
                let page_idx = byte_idx * 8 + free_bit;
                if page_idx >= self.pages { break 'outer; } // can't 

                // mark the bit as used
                *content |= 1 << free_bit;
                count -= 1;

                // add the page to the result
                result.buffer[result.count] = start + (page_idx * PAGE_SIZE);
                result.count += 1;
                if result.count == MAX_ALLOC_BATCH { break 'outer; } // can't return any more results
            }
        }

        result
    }

    /// Tries to deallocate several pages. Returns addresses that do not belong
    /// to this range.
    fn deallocate(&mut self, pages: AllocPages) -> AllocPages {
        let mut result = AllocPages { buffer: [INVALID_PHYS; MAX_ALLOC_BATCH], count: 0 };

        for idx in 0..pages.count {
            // find what page the address is referring to
            let page = pages.buffer[idx];
            let page_idx = (page as isize - self.start() as isize) / PAGE_SIZE as isize;

            // if it's not in our allocatable range, return and skip it
            if !self.allocatable_idxs().contains(&page_idx) {
                result.buffer[result.count] = page;
                result.count += 1;
                continue;
            }

            // mark page as unused
            Self::bitmap_set(self.bitmap, page_idx.try_into().unwrap(), false);
        }

        result
    }
}

/// Initializes the PMM
pub fn init(mem_map: &MemoryMap) {
    let (mut avail_already, mut add_after_reclaim, mut used_by_pmm): (ByteSize, ByteSize, ByteSize) = Default::default();

    for entry in mem_map.entries() {
        // print entry
        let size = entry.page_count as usize * PAGE_SIZE;
        log::trace!("memory map: {:#018x} to {:#018x} {:?}",
            entry.phys_start, entry.phys_start as usize + size, entry.ty);
        
        // check whether the range is usable
        match entry.ty {
            MemoryType::CONVENTIONAL |
            MemoryType::BOOT_SERVICES_CODE |
            MemoryType::BOOT_SERVICES_DATA =>
                avail_already += size as usize,
            MemoryType::ACPI_RECLAIM => {
                add_after_reclaim += size as usize;
                continue;
            },
            _ => continue,
        }

        // check if maybe the last range could be extended
        let mut guard = ALL_RANGES.write();
        match guard.as_mut() {
            None => {
                drop(guard);
                // there are no ranges - create the range and the range list
                let range = PageRangeHeader::new(entry);
                range.create_all_ranges().unwrap();
                range.make_available();
            },
            Some(all_ranges) => {
                let last = all_ranges.last_mut().unwrap();
                
                // try to extend the range. if that isn't possible,
                // create a new range
                if last.start() + (last.pages * PAGE_SIZE) == entry.phys_start as PhysAddr {
                    if last.extend(entry.page_count as usize) == Ok(()) {
                        continue;
                    }
                }

                drop(guard);
                let range = PageRangeHeader::new(entry);
                range.make_available();
            }
        }
    }

    // finalize by extending the first range's header
    let mut guard = ALL_RANGES.write();
    let ar_size = size_of_val((*guard).as_ref().unwrap());
    let ranges = (*guard).as_mut().unwrap();
    let first_range = ranges.first_mut().unwrap();
    first_range.extend_header(ar_size).unwrap();

    // count memory used by the PMM
    for range in ranges.iter() {
        log::info!("usable range: {:#018x} to {:#018x} ({} hdr pages, {} usable pages)",
            range.start(), range.start() + (range.pages * PAGE_SIZE), range.header_pages, range.pages - range.header_pages);
        used_by_pmm += range.header_pages * PAGE_SIZE;
    }

    log::info!("memory: {} available, -{} used by PMM, +{} after ACPI reclaim",
        avail_already, used_by_pmm, add_after_reclaim);
}

/// Tries to allocate a number of physical pages, returning their starting
/// addresses.
pub fn allocate(count: usize) -> AllocPages {
    let mut result = AllocPages { buffer: [INVALID_PHYS; MAX_ALLOC_BATCH], count: 0 };

    for range in (*ALL_RANGES.write()).as_mut().unwrap().iter_mut() {
        let range_result = range.allocate(count);
        result.merge(&range_result);

        if result.count == MAX_ALLOC_BATCH { break; } // can't return any more results
    }

    // Print the result
    log::trace!("pmm allocate({}) = {:?}", count, result);

    result
}

/// Deallocates a number of physical pages using their physical addresses.
/// Returns unrecognized addresses that failed to deallocate.
pub fn deallocate(mut pages: AllocPages) -> Result<(), AllocPages> {
    let orig = pages.clone(); // for tracing

    for range in (*ALL_RANGES.write()).as_mut().unwrap().iter_mut() {
        pages = range.deallocate(pages);
        if pages.count == 0 { break; } // all pages deallocated
    }

    log::trace!("pmm deallocate({orig:?}) = {pages:?}");

    if pages.count == 0 {
        Ok(())
    } else {
        Err(pages)
    }
}
