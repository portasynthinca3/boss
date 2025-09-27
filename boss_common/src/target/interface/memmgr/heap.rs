//! Extremely primitive linked list allocator that works on top of a physical
//! page allocator.

use core::{mem::{align_of, size_of}, ptr::{self, NonNull}};
use alloc::alloc::{AllocError, Allocator, Layout};

use crate::target::current::memmgr::*;
use crate::util::byte_size::ByteSize;

use spin::Mutex;

// Two kinds of padding are referred to in this module: "pre" and "post".
// "Pre" padding is inserted between the block header and the allocated data.
// "Post" padding is inserted between the allocated data and the next block.
// Here's an illustration:
//
// -----+----------+-------------+----------------+--------------+----------+----
//  ... | BlockHdr | pre_padding | allocated data | post_padding | BlockHdr | ...
// -----+----------+-------------+----------------+--------------+----------+----

const PAGE_SIZE: usize = MemoryParameters::PAGE_SIZE;

// These strings are easy to spot in a hexdump
const ALLOCATED_SIGNATURE: [u8; 8] = [b'U', b's', b'e', b'd', b'B', b'l', b'o', b'k'];
const HEAD_SIGNATURE: [u8; 8] = [b'A', b'l', b'o', b'c', b'H', b'e', b'a', b'd'];
const FREE_SIGNATURE: [u8; 8] = [b'F', b'r', b'e', b'e', b'B', b'l', b'o', b'k'];

/// Block header. Present at the start of both free and non-free blocks.
struct BlockHdr {
    signature: [u8; 8],
    /// Total block size, including this header and all paddings
    size: usize,
    next: Option<&'static mut BlockHdr>,
}

impl BlockHdr {
    /// [Layout] of [BlockHdr]
    fn layout() -> Layout {
        Layout::from_size_align(size_of::<Self>(), align_of::<Self>()).unwrap()
    }

    /// Gets the virtual address of a block header
    fn addr(&self) -> VirtAddr {
        VirtAddr::from_ref(self).unwrap()
    }

    /// Allocates a specific layout, possibly inserting a new free block after
    /// the one this method was called on.
    fn allocate_possibly_split(&mut self, layout: Layout) -> Option<NonNull<[u8]>> {
        let pre_padding = Self::layout().padding_needed_for(layout.align());
        // includes all sizes and paddings
        let block = Layout::from_size_align(Self::layout().size() + pre_padding + layout.size(), Self::layout().align()).unwrap().pad_to_align();
        if self.is_used() || self.size < block.size() {
            return None;
        }

        self.signature = ALLOCATED_SIGNATURE;
        // get address of data to return
        let blk_addr = self as *mut _ as usize;
        let data_addr = blk_addr + Self::layout().size() + pre_padding;
        let data = ptr::slice_from_raw_parts_mut(data_addr as *mut u8, layout.size());

        // insert new free block if there's space
        let block_and_next_hdr = Layout::from_size_align(block.size() + Self::layout().size(), Self::layout().align()).unwrap().pad_to_align();
        if self.size > block_and_next_hdr.size() {
            let next = self.next.take();
            let new_blk = unsafe {
                // SAFETY: the value is properly aligned thanks to the various
                // paddings that we added
                let new_blk = (blk_addr as *mut BlockHdr).byte_add(block.size());
                // SAFETY: valid for writes, properly aligned
                *new_blk = BlockHdr {
                    signature: FREE_SIGNATURE,
                    next,
                    size: self.size - block.size(),
                };
                // SAFETY: just initialized the value
                new_blk.as_uninit_mut().unwrap().assume_init_mut()
            };
            self.next = Some(new_blk);
            self.size = block.size();
        }

        NonNull::new(data)
    }

    fn is_used(&self) -> bool {
        if self.signature == ALLOCATED_SIGNATURE { return true };
        if self.signature == HEAD_SIGNATURE { return true };
        if self.signature == FREE_SIGNATURE { return false };
        panic!("memory corruption");
    }
}

impl core::fmt::Debug for BlockHdr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let addr: VirtAddr = VirtAddr::from_ref(self).unwrap();
        f.debug_struct("Block")
            .field("address", &addr)
            .field("used", &self.is_used())
            .field("size", &ByteSize(self.size))
            .finish()
    }
}

pub struct LinkedListAllocator<'p> {
    first_block: Mutex<BlockHdr>,
    addr_space: Mutex<Option<AddrSpace<'p>>>,
}

impl<'p> LinkedListAllocator<'p> {
    /// Creates a new allocator. Takes ownership of the entire provided address
    /// space, and operates within it.
    pub fn new(addr_space: AddrSpace<'p>) -> Result<Self, Error> {
        let allocator = LinkedListAllocator {
            first_block: Mutex::new(BlockHdr { signature: HEAD_SIGNATURE, size: 0, next: None }),
            addr_space: Mutex::new(Some(addr_space)),
        };

        let mut block_guard = allocator.first_block.lock();
        let mut last_block = &mut *block_guard;
        let mut space_guard = allocator.addr_space.lock();
        let mut addr_space = space_guard.as_mut().unwrap().modify();

        for i in 0..addr_space.ranges().count() {
            let range = addr_space.ranges().nth(i).unwrap();
            let start = addr_space.allocate_range(*range.start(), 1, Default::default(), AllocReturn::Start)?;
            let block = start.to_mut_ptr();
            let block = unsafe {
                *block = BlockHdr {
                    signature: FREE_SIGNATURE,
                    next: None,
                    size: PAGE_SIZE,
                };
                block.as_uninit_mut().unwrap().assume_init_mut()
            };

            last_block.next = Some(block);
            last_block = last_block.next.as_mut().unwrap();
        }

        #[cfg(feature = "trace-malloc")]
        log::trace!("new LinkedListAllocator {:?}", addr_space.deref());

        drop(block_guard);
        drop(addr_space);
        drop(space_guard);
        Ok(allocator)
    }

    /// Consumes the allocator, freeing all allocated data and returning the
    /// address space it was assigned to.
    pub fn destroy(mut self) -> AddrSpace<'p> {
        self.do_destroy();
        (*self.addr_space.lock()).take().unwrap()
    }

    fn do_destroy(&mut self) {
        let mut space_guard = self.addr_space.lock();
        let Some(addr_space) = space_guard.as_mut() else { return };

        for i in 0..addr_space.ranges().count() {
            let range = addr_space.ranges().nth(i).unwrap();
            addr_space.modify().deallocate_range(range).unwrap();
        }
    }

    fn compact_free_blocks(&self) {
        let mut guard = self.first_block.lock();
        let mut block = &mut *guard;

        loop {
            if !block.is_used() && let Some(ref mut next) = block.next && !next.is_used() {
                block.size += next.size;
                block.next = next.next.take();
            } else if block.next.is_some() {
                // block = next;
                block = block.next.as_mut().unwrap();
            } else {
                break;
            }
        }
    }
}

impl core::fmt::Debug for LinkedListAllocator<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "LinkedListAllocator:")?;
        let guard = self.first_block.lock();
        let mut previous = &*guard;

        let mut display_used = ByteSize(0);
        let mut display_capacity = ByteSize(0);
        while let Some(ref current) = previous.next {
            let display_size = ByteSize(current.size);
            let block_addr = current.addr();
            write!(f, "{display_size} @ {block_addr:?} ")?;
            for _ in 0 .. BlockHdr::layout().size() / 8 {
                write!(f, "h")?;
            }
            for _ in 0 .. (current.size / 8).min(64) {
                if current.is_used() {
                    write!(f, "x")?;
                } else {
                    write!(f, "-")?;
                }
            }
            if current.size > 64 * 8 {
                write!(f, "...")?;
            }
            writeln!(f)?;
            display_capacity += display_size;
            if current.is_used() { display_used += display_size };
            previous = previous.next.as_ref().unwrap();
        }

        f.debug_struct("Stats")
            .field("used", &display_used)
            .field("capacity", &display_capacity)
            .finish()
    }
}

unsafe impl Allocator for LinkedListAllocator<'_> {
    fn allocate(&self, layout: Layout) -> core::result::Result<NonNull<[u8]>, AllocError> {
        // find suitable block
        let mut guard = self.first_block.lock();
        let mut previous = &mut *guard;
        while let Some(ref mut current) = previous.next {
            if let Some(ptr) = current.allocate_possibly_split(layout) {
                #[cfg(feature = "trace-malloc")]
                {
                    drop(guard);
                    log::trace!("allocate({layout:?}) = {ptr:?}");
                    log::trace!("{self:#?}");
                }
                return Ok(ptr);
            } else {
                // no sufficiently big free blocks found
                previous = previous.next.as_mut().unwrap();
            }
        }

        // at this point we need to either extend the last block (if it's free)
        // or allocate a new one

        // start of extension
        let new_start = unsafe { (previous as *mut BlockHdr).byte_add(previous.size) };
        let new_start = VirtAddr::from_mut_ptr(new_start).unwrap();
        assert!(new_start.to_usize() % PAGE_SIZE == 0, "last block not ended on page boundary");
        const { assert!(align_of::<BlockHdr>() <= PAGE_SIZE) };
        
        let pre_padding = BlockHdr::layout().padding_needed_for(layout.align());
        let post_padding = layout.padding_needed_for(BlockHdr::layout().align());

        let previous = if previous.is_used() {
            // last block is used: need to allocate a new one
            let page_cnt = (BlockHdr::layout().size() + pre_padding + layout.size()).div_ceil(PAGE_SIZE);
            #[cfg(feature = "trace-malloc")]
            log::trace!("adding new free block of {} at {previous:?} {new_start:?}", ByteSize(page_cnt * PAGE_SIZE));

            let blk_ptr = self.addr_space
                .lock().as_mut().unwrap()
                .modify()
                .allocate_range(new_start, page_cnt, Default::default(), AllocReturn::Start)
                .inspect_err(|err| log::error!("failed to add new block: {err:?}"))
                .map_err(|_| AllocError)?
                .to_mut_ptr();

            let blk_ref = unsafe {
                // SAFETY: valid for write, aligned
                *blk_ptr = BlockHdr {
                    signature: FREE_SIGNATURE,
                    size: page_cnt * PAGE_SIZE,
                    next: None,
                };
                // SAFETY: just initialized the value
                blk_ptr.as_uninit_mut().unwrap().assume_init_mut()
            };

            previous.next = Some(blk_ref);
            previous.next.as_mut().unwrap()
        } else {
            // last block is free but small: need to extend it
            let target_size = BlockHdr::layout().size() + pre_padding + layout.size() + post_padding;
            let increase_by = target_size - previous.size;
            let increase_by_pages = increase_by.div_ceil(PAGE_SIZE);
            #[cfg(feature = "trace-malloc")]
            log::trace!("extending last block by {} pages from {} to {}", increase_by_pages, ByteSize(previous.size), ByteSize(target_size));

            self.addr_space
                .lock().as_mut().unwrap()
                .modify()
                .allocate_range(new_start, increase_by_pages, Default::default(), AllocReturn::Start)
                .inspect_err(|err| log::error!("failed to add new block: {err:?}"))
                .map_err(|_| AllocError)?;

            previous.size += increase_by_pages * PAGE_SIZE;
            previous
        };

        // try allocating again now that there's definitely enough space
        let result = previous.allocate_possibly_split(layout);
        #[cfg(feature = "trace-malloc")]
        {
            drop(guard);
            log::trace!("{self:#?}");
        }
        Ok(result.unwrap())
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        #[cfg(feature = "trace-malloc")]
        log::trace!("deallocate({ptr:?}, {layout:?})");
        // obtain block reference
        let blk_layout = BlockHdr::layout();
        let down_offs = (layout.align() as isize - blk_layout.align() as isize).max(blk_layout.size() as isize);
        let block = ptr.byte_sub(down_offs as usize).cast::<BlockHdr>().as_uninit_mut().assume_init_mut();

        block.signature = FREE_SIGNATURE;
        self.compact_free_blocks();

        // TODO: return pages to physical allocator
        // currently that's only done in `Drop` all at once

        #[cfg(feature = "trace-malloc")]
        log::trace!("{self:#?}");
    }
}

impl Drop for LinkedListAllocator<'_> {
    fn drop(&mut self) {
        self.do_destroy();
    }
}

/// The default global allocator
#[global_allocator]
static HEAP_ALLOCATOR: LateAllocator<LinkedListAllocator> = LateAllocator::new();

/// Initializes the default global allocator
pub fn initialize_global_alloc(addr_space: AddrSpace<'static>) {
    let allocator = LinkedListAllocator::new(addr_space)
        .expect("failed to create global allocator");
    HEAP_ALLOCATOR.initialize(allocator);
}

