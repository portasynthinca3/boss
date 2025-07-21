//! Extremely primitive linked list allocator that works on top of a physical
//! page allocator.

use core::{mem::{align_of, size_of}, ptr::{self, NonNull}};
use alloc::alloc::{AllocError, Allocator, GlobalAlloc, Layout};

use spin::Mutex;

use crate::util::byte_size::ByteSize;
use super::{MemMgrError, virt::AddressSpace, VirtAddr, phys, layout, PAGE_SIZE};

pub type Result<T> = core::result::Result<T, MemMgrError>;

// Two kinds of padding are referred to in this module: "pre" and "post".
// "Pre" padding is inserted between the block header and the allocated data.
// "Post" padding is inserted between the allocated data and the next block.
// Here's an illustration:
//
// -----+----------+-------------+----------------+--------------+----------+----
//  ... | BlockHdr | pre_padding | allocated data | post_padding | BlockHdr | ...
// -----+----------+-------------+----------------+--------------+----------+----

/// Block header. Present at the start of both free and non-free blocks.
struct BlockHdr {
    used: bool,
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
        self.into()
    }

    /// Allocates a specific layout, possibly inserting a new free block after
    /// the one this method was called on.
    fn allocate_possibly_split(&mut self, layout: Layout) -> Option<NonNull<[u8]>> {
        let pre_padding = Self::layout().padding_needed_for(layout.align());
        // includes all sizes and paddings
        let block = Layout::from_size_align(Self::layout().size() + pre_padding + layout.size(), Self::layout().align()).unwrap().pad_to_align();
        if self.used || self.size < block.size() {
            return None;
        }

        self.used = true;
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
                    used: false,
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
}

impl core::fmt::Debug for BlockHdr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let addr: VirtAddr = self.into();
        f.debug_struct("Block")
            .field("address", &addr)
            .field("used", &self.used)
            .field("size", &ByteSize(self.size))
            .finish()
    }
}

pub struct LinkedListAllocator {
    first_block: Mutex<BlockHdr>,
    address_space: Mutex<AddressSpace>, // FIXME: design: we shouldn't be able to control the entire address space
}

impl LinkedListAllocator {
    /// Creates a new allocator.
    /// 
    /// # Safety
    /// The function is unsafe in case there are other objects at any address
    /// above `bottom`.
    pub unsafe fn new(bottom: VirtAddr, mut address_space: AddressSpace) -> Result<LinkedListAllocator> {
        // place first block header
        let pages = phys::allocate(1);
        if pages.is_empty() { return Err(MemMgrError::OutOfMemory); }
        let first_block: VirtAddr = bottom;
        address_space.modify().map_range(first_block, pages[0], 1, Default::default(), false).unwrap();
        let first_block = first_block.0 as *mut BlockHdr;
        *first_block = BlockHdr {
            used: false,
            next: None,
            size: PAGE_SIZE,
        };
        let first_block = first_block.as_uninit_mut().unwrap().assume_init_mut();

        #[cfg(feature = "trace-malloc")]
        log::trace!("new LinkedListAllocator: bottom={bottom:?}, address_space={address_space:?}");

        // return allocator
        Ok(LinkedListAllocator {
            first_block: Mutex::new(BlockHdr { used: true, size: 0, next: Some(first_block) }),
            address_space: Mutex::new(address_space),
        })
    }
}

impl core::fmt::Debug for LinkedListAllocator {
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
                if current.used {
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
            if current.used { display_used += display_size };
            previous = previous.next.as_ref().unwrap();
        }

        f.debug_struct("Stats")
            .field("used", &display_used)
            .field("capacity", &display_capacity)
            .finish()
    }
}

unsafe impl Allocator for LinkedListAllocator {
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
        let new_start: VirtAddr = new_start.into();
        assert!(new_start.0 % PAGE_SIZE == 0, "last block not ended on page boundary");
        const { assert!(align_of::<BlockHdr>() <= PAGE_SIZE); }
        
        let pre_padding = BlockHdr::layout().padding_needed_for(layout.align());
        let post_padding = layout.padding_needed_for(BlockHdr::layout().align());

        let previous = if previous.used {
            // last block is used: need to allocate a new one
            let mut page_cnt = (BlockHdr::layout().size() + pre_padding + layout.size()).div_ceil(PAGE_SIZE);
            #[cfg(feature = "trace-malloc")]
            log::trace!("adding new free block of {} at {previous:?} {new_start:?}", ByteSize(page_cnt * PAGE_SIZE));

            // the physical allocator may be unable to handle all of the pages once
            // TODO: AddressSpaceGuard::allocate_range or sumn
            let mut v_addr = new_start;
            while page_cnt > 0 {
                let mut guard = self.address_space.lock();
                let mut modifier = guard.modify();
                let pages = phys::allocate(page_cnt);
                for p_addr in pages.iter() {
                    modifier.map_range(v_addr, p_addr, 1, Default::default(), false).unwrap();
                    #[cfg(feature = "trace-malloc")]
                    log::trace!("map {v_addr:?} -> {p_addr:?}");
                    page_cnt -= 1;
                    v_addr = VirtAddr(v_addr.0 + PAGE_SIZE);
                }
                if pages.is_empty() {
                    #[cfg(feature = "trace-malloc")]
                    log::trace!("allocate({layout:?}) = no memory!");
                    return Err(AllocError)
                }
            }

            // place block header
            let blk_ptr: *mut BlockHdr = new_start.into();
            let blk_ref = unsafe {
                // SAFETY: valid for write, aligned
                *blk_ptr = BlockHdr {
                    used: false,
                    size: (v_addr - new_start).into(),
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
            let mut increase_by_pages = increase_by.div_ceil(PAGE_SIZE);
            #[cfg(feature = "trace-malloc")]
            log::trace!("extending last block by {} pages from {} to {}", increase_by_pages, ByteSize(previous.size), ByteSize(target_size));

            // the physical allocator may be unable to handle all of them at once
            let mut v_addr = new_start;
            while increase_by_pages > 0 {
                let mut guard = self.address_space.lock();
                let mut modifier = guard.modify();
                let pages = phys::allocate(increase_by_pages);
                for p_addr in pages.iter() {
                    modifier.map_range(v_addr, p_addr, 1, Default::default(), false).unwrap();
                    #[cfg(feature = "trace-malloc")]
                    log::trace!("map {v_addr:?} -> {p_addr:?}");
                    increase_by_pages -= 1;
                    previous.size += PAGE_SIZE;
                    v_addr = VirtAddr(v_addr.0 + PAGE_SIZE);
                }
                if pages.is_empty() {
                    #[cfg(feature = "trace-malloc")]
                    log::trace!("allocate({layout:?}) = no memory!");
                    return Err(AllocError)
                }
            }

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

        // free block
        block.used = false;
        // join next block if also free
        match block.next {
            None => (),
            Some(ref mut next) => {
                if !next.used {
                    block.size += next.size;
                    let next = next.next.take();
                    block.next = next;
                }
            },
        }

        #[cfg(feature = "trace-malloc")]
        log::trace!("{self:#?}");
    }
}

/// An allocator that becomes available halfway into the program
struct LateAllocator<T: Allocator>(Option<T>);

unsafe impl<T: Allocator> GlobalAlloc for LateAllocator<T> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let allocator = self.0.as_ref().expect("LateAllocator invoked before it was initialized");
        match allocator.allocate(layout) {
            Err(_) => ptr::null_mut::<u8>(),
            Ok(p) => p.as_mut_ptr(),
        }
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let allocator = self.0.as_ref().expect("LateAllocator invoked before it was initialized");
        if let Some(ptr) = NonNull::new(ptr) {
            allocator.deallocate(ptr, layout);
        }
    }
}

/// The default global allocator
#[global_allocator]
static mut ALLOCATOR: LateAllocator<LinkedListAllocator> = LateAllocator(None);

/// Initializes the default global allocator
/// 
/// # Safety
/// This function must only be called once
pub unsafe fn initialize_default(addr_space: AddressSpace) {
    let allocator = LinkedListAllocator::new(layout::EMULATOR_HEAP_BASE, addr_space)
        .expect("failed to create global allocator");
    ALLOCATOR = LateAllocator(Some(allocator));
}
