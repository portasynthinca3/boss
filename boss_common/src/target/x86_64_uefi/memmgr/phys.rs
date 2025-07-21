//! Physical Memory Manager. Allocates and frees physical pages using a memory
//! map supplied to it by the main module.

use core::{mem::{size_of, size_of_val}, ops::Range};

use uefi::table::boot::{MemoryMap, MemoryDescriptor, MemoryType};
use spin::RwLock;

use super::{PhysAddr, VirtAddr, PAGE_SIZE};
use crate::{
    target::runtime_cfg::{self, CfgFlags},
    util::{
        byte_size::ByteSize,
        dyn_arr::{DynArr, DYN_ARR_CAPACITY},
        convert_slice_in_place,
        boot_stage,
    },
};

/// This struct appears at the start of every page range and contains, mainly,
/// the bitmap of the range. This bitmap directly follows this struct in memory,
/// but its size may be extended while the PMM is initializing, so it's not kept
/// in the struct directly.
/// 
/// Range 0 also contains the `ALL_RANGES` slice, containing physical addresses
/// of all the ranges, including itself. The reference to this slice is kept in
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
pub struct PageRangeHeader {
    /// Size of the range, including this header, in pages
    pub pages: usize,
    /// Size of this header in pages
    header_pages: usize,
    /// Bitmap for this range
    bitmap: &'static mut [u8],
    /// Whether this range contains the `ALL_RANGES` slice
    has_all_ranges: bool,
}

/// Reference to all the page ranges
type AllRanges = &'static mut [&'static mut PageRangeHeader];
pub static ALL_RANGES: RwLock<Option<AllRanges>> = RwLock::new(None);

/// The gap, in bytes, that will be kept between the start of the page range
/// and the `ALL_RANGES` slice it contains. Effectively determines how many
/// pages (at most) the first range may contain.
const PRE_AR_GAP: usize = 2048;

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
        bitmap.fill(0);

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
            header_ptr.as_uninit_mut().unwrap().assume_init_mut()
        }
    }

    /// Returns the starting address of the range
    pub fn start(&self) -> PhysAddr {
        let virt: VirtAddr = self.into();
        virt.into()
    }

    /// Returns the range of allocatable page indices
    fn allocatable_idxs(&self) -> Range<isize> {
        self.header_pages as isize .. self.pages as isize
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
        Ok(())
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
        Ok(())
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
            + self.bitmap.len()).div_ceil(PRE_AR_GAP) * PRE_AR_GAP
            + size_of_ar
        ).div_ceil(PAGE_SIZE);

        for i in 0..new_header_pages {
            Self::bitmap_set(self.bitmap, i, true);
        }

        self.header_pages = new_header_pages;
        Ok(())
    }

    /// Tries to allocate several pages, returning their starting addresses.
    fn allocate(&mut self, mut count: usize) -> DynArr<PhysAddr> {
        let mut result: DynArr<PhysAddr> = Default::default();
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

                // add the page to the result
                let page_addr = start + PhysAddr(page_idx * PAGE_SIZE);
                if result.push(page_addr).is_err() { break 'outer; } // can't return any more results

                // mark the bit as used
                *content |= 1 << free_bit;
                count -= 1;
            }
        }

        result
    }

    /// Tries to deallocate several pages. Returns addresses that do not belong
    /// to this range.
    fn deallocate(&mut self, pages: DynArr<PhysAddr>) -> DynArr<PhysAddr> {
        let mut result: DynArr<PhysAddr> = Default::default();

        for page in pages.iter() {
            // find what page the address is referring to
            let page_idx = (page.0 as isize - self.start().0 as isize) / PAGE_SIZE as isize;

            // if it's not in our allocatable range, return and skip it
            if !self.allocatable_idxs().contains(&page_idx) {
                result.push(page).unwrap();
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
    let (
        mut avail_already,
        mut add_after_reclaim,
        mut used_by_pmm,
    ): (ByteSize, ByteSize, ByteSize) = Default::default();

    for original_entry in mem_map.entries() {
        let mut entry = *original_entry;
        // print entry
        let size = entry.page_count as usize * PAGE_SIZE;
        log::debug!("uefi: {:#018x} to {:#018x} {:?}",
            entry.phys_start, entry.phys_start as usize + size, entry.ty);

        // we can't use page zero because of UB, even though from the hardware
        // point of view it is perfectly usable.
        // 4KiB lost because of all the abstractions. oh well. too bad!
        if entry.phys_start == 0 {
            entry.phys_start += PAGE_SIZE as u64;
            entry.page_count -= 1;
            if entry.page_count == 0 { continue; }
        }

        // check whether the range is usable
        match entry.ty {
            MemoryType::CONVENTIONAL => avail_already += ByteSize(size),
            MemoryType::BOOT_SERVICES_CODE | MemoryType::BOOT_SERVICES_DATA |
            MemoryType::LOADER_CODE | MemoryType::LOADER_DATA |
            MemoryType::ACPI_RECLAIM => {
                add_after_reclaim += ByteSize(size);
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
                let range = PageRangeHeader::new(&entry);
                range.create_all_ranges().unwrap();
                range.make_available();
            },
            Some(all_ranges) => {
                let last = all_ranges.last_mut().unwrap();
                
                // try to extend the range. if that isn't possible,
                // create a new range
                if last.start() + PhysAddr(last.pages * PAGE_SIZE) == PhysAddr(entry.phys_start as usize)
                   && last.extend(entry.page_count as usize) == Ok(()) {
                    continue;
                }

                drop(guard);
                let range = PageRangeHeader::new(&entry);
                range.make_available();
            }
        }
    }

    // finalize by extending the first range's header
    let mut guard = ALL_RANGES.write();
    let ar_size = size_of_val(*(*guard).as_ref().unwrap());
    let ranges = (*guard).as_mut().unwrap();
    let first_range = ranges.first_mut().unwrap();
    first_range.extend_header(ar_size).unwrap();

    // count memory used by the PMM
    for range in ranges.iter() {
        log::debug!("usable: {:?} to {:?} ({} hdr, {} alloc)",
            range.start(), range.start() + PhysAddr(range.pages * PAGE_SIZE), range.header_pages, range.pages - range.header_pages);
        used_by_pmm += ByteSize(range.header_pages * PAGE_SIZE);
    }

    log::info!("memory: {avail_already} available, -{used_by_pmm} used by PMM, +{add_after_reclaim} after full reclaim");
    // checkpoint::advance(Checkpoint::PhysMemMgr).unwrap();
}

pub fn dump() {
    log::trace!("PMM dump:");
    for range in (*ALL_RANGES.write()).as_mut().unwrap().iter_mut() {
        let start = range.start();
        let size = range.pages;
        let mut used = 0;
        for byte in range.bitmap.iter() {
            used += byte.count_ones();
        }
        log::trace!("range_start={start:?} used={used}/{size}");
    }
}

/// Tries to allocate a number of physical pages, returning their starting
/// addresses.
pub fn allocate(mut count: usize) -> DynArr<PhysAddr> {
    #[cfg(feature = "trace-pmm")]
    let given_count = count;
    let mut result: DynArr<PhysAddr> = Default::default();

    for range in (*ALL_RANGES.write()).as_mut().unwrap().iter_mut() {
        let range_result = range.allocate(count);
        count -= range_result.len();
        result += range_result;

        if result.len() == DYN_ARR_CAPACITY || count == 0 { break; } // can't return any more results
    }

    // clear all pages
    for page in result.iter() {
        unsafe { page.clear_page().unwrap(); }
    }

    #[cfg(feature = "trace-pmm")]
    log::trace!("allocate({given_count}) = {result:?}");

    result
}

/// Deallocates a number of physical pages using their physical addresses.
/// Returns unrecognized addresses that failed to deallocate.
#[allow(clippy::result_large_err)]
pub fn deallocate(mut pages: DynArr<PhysAddr>) -> Result<(), DynArr<PhysAddr>> {
    #[cfg(feature = "trace-pmm")]
    let orig = pages.clone();

    for range in (*ALL_RANGES.write()).as_mut().unwrap().iter_mut() {
        pages = range.deallocate(pages);
        if pages.is_empty() { break; } // all pages deallocated
    }

    #[cfg(feature = "trace-pmm")]
    log::trace!("deallocate({orig:?}) = {pages:?}");

    if pages.is_empty() {
        Ok(())
    } else {
        Err(pages)
    }
}

/// Returns a tuple containing used and available physical memory.
pub fn stats() -> (ByteSize, ByteSize) {
    let mut used = ByteSize(0);
    let mut available = ByteSize(0);

    for range in (*ALL_RANGES.read()).as_ref().unwrap().iter() {
        available += ByteSize(range.pages * PAGE_SIZE);
        #[allow(clippy::useless_asref)] // false positive
        for b in range.bitmap.as_ref() {
            used += ByteSize(b.count_ones() as usize * PAGE_SIZE);
        }
    }

    (used, available)
}

#[derive(Clone, Copy)]
pub struct PmmState {
    ranges: PhysAddr,
    range_count: usize,
}

/// Returns an opaque object that can be imported back in a different program
/// launched in the same environment to have the same state of the PMM. Useful
/// for passing PMM state between the bootloader and the emulator.
pub fn export() -> PmmState {
    assert!(!runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf));

    let mut guard = ALL_RANGES.write();
    let ranges = (*guard).as_mut().unwrap();
    let ranges_addr: PhysAddr = Into::<VirtAddr>::into(ranges.as_ptr()).into();
    PmmState { ranges: ranges_addr, range_count: ranges.len() }
}

/// Imports an opaque object to initialize the PMM using a previously exported
/// state. Useful for passing PMM state between the bootloader and the emulator.
/// 
/// # Safety
/// Nobody else should be using the state except you.
pub unsafe fn import(state: PmmState) {
    boot_stage::new_level();
    assert!(runtime_cfg::get().contains(CfgFlags::ExecutingInUpperHalf));

    let guard = ALL_RANGES.upgradeable_read();
    if (*guard).as_ref().is_some() { panic!("pmm already initialized") };

    boot_stage::same_level("Getting ranges");
    let ranges_ptr: *mut PhysAddr = Into::<VirtAddr>::into(state.ranges).into();
    let phys_ranges: &'static mut [PhysAddr] = core::slice::from_raw_parts_mut(ranges_ptr, state.range_count);
    let virt_ranges: &'static mut [VirtAddr] = convert_slice_in_place(phys_ranges);
    let ranges: AllRanges = core::mem::transmute(virt_ranges);

    boot_stage::same_level("Fixing addresses");
    for range in ranges.iter_mut() {
        let bitmap_len = range.bitmap.len();
        let bitmap_phys_addr: PhysAddr = core::mem::transmute(range.bitmap.as_mut_ptr());
        let new_bitmap_ptr: *mut u8 = Into::<VirtAddr>::into(bitmap_phys_addr).into();
        let new_bitmap = core::slice::from_raw_parts_mut(new_bitmap_ptr, bitmap_len);
        range.bitmap = new_bitmap;
    }

    *guard.upgrade() = Some(ranges);
    boot_stage::end_level();
}
