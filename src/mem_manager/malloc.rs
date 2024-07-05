//! Extremely primitive linked list allocator that works on top of a physical
//! page allocator.

use spin::Mutex;
use core::{mem::{align_of, size_of}, ptr::{self, NonNull}};
use alloc::alloc::{AllocError, Allocator, GlobalAlloc, Layout};

use crate::util::byte_size::ByteSize;
use super::{MemMgrError, virt::AddressSpace, VirtAddr, phys, PAGE_SIZE};

pub type Result<T> = core::result::Result<T, MemMgrError>;

struct Block {
    used: bool,
    size: usize,
    next: Option<&'static mut Block>,
}

impl Block {
    fn layout() -> Layout {
        Layout::from_size_align(size_of::<Self>(), align_of::<Self>()).unwrap()
    }

    fn allocate_possibly_split(&mut self, layout: Layout) -> Option<NonNull<[u8]>> {
        let padding = Self::layout().padding_needed_for(layout.align());
        let total_layout = Layout::from_size_align(Self::layout().size() + padding + layout.size(), Self::layout().align()).unwrap().pad_to_align();
        if self.used || self.size < total_layout.size() {
            return None;
        }

        self.used = true;
        // get address of data to return
        let blk_addr = self as *mut _ as usize;
        let data_addr = blk_addr + Self::layout().size() + padding;
        let data = ptr::slice_from_raw_parts_mut(data_addr as *mut u8, layout.size());

        // log::trace!("{total_layout:?}");
        if self.size >= total_layout.size() {
            // insert new free block
            let next = self.next.take();
            let new_blk = unsafe {
                let new_blk = (blk_addr as *mut Block).byte_add(total_layout.size());
                *new_blk = Block {
                    used: false,
                    next,
                    size: self.size - total_layout.size() - Self::layout().size(),
                };
                new_blk.as_uninit_mut().unwrap().assume_init_mut()
            };
            self.next = Some(new_blk);
            self.size = total_layout.size();
        }

        NonNull::new(data)
    }
}

impl core::fmt::Debug for Block {
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
    first_block: Mutex<Block>,
    address_space: Mutex<AddressSpace>, // FIXME: we shouldn't be able to control the entire address space
}

impl LinkedListAllocator {
    /// Creates a new allocator.
    /// 
    /// # Safety
    /// The function is unsafe in case there are other allocators managing the
    /// memory region defined by `bottom` and `max_size`. It is also unsafe if
    /// there are any active references to that memory region, or, more
    /// generally, any other active objects.
    pub unsafe fn new<'a>(bottom: VirtAddr, mut address_space: AddressSpace) -> Result<LinkedListAllocator> {
        // place first block
        let pages = phys::allocate(1);
        if pages.len() < 1 { return Err(MemMgrError::OutOfMemory); }
        let first_block: VirtAddr = bottom;
        address_space.modify().map_range(first_block, pages[0], 1, Default::default(), false).unwrap();
        let first_block = first_block.0 as *mut Block;
        *first_block = Block {
            used: false,
            next: None,
            size: PAGE_SIZE - size_of::<Block>(),
        };
        let first_block = first_block.as_uninit_mut().unwrap().assume_init_mut();

        let top = VirtAddr(bottom.0 + PAGE_SIZE);
        log::trace!("new LinkedListAllocator: bottom={bottom:?}, address_space={address_space:?}");

        // return allocator
        Ok(LinkedListAllocator {
            first_block: Mutex::new(Block { used: true, size: 0, next: Some(first_block) }),
            address_space: Mutex::new(address_space),
        })
    }
}

impl core::fmt::Debug for LinkedListAllocator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "LinkedListAllocator")?;
        let guard = self.first_block.lock();
        let mut previous = &*guard;

        let mut used = ByteSize(0);
        let mut capacity = ByteSize(0);
        let mut list = f.debug_list();
        while let Some(ref current) = previous.next {
            list.entry(current);
            let block_size = ByteSize(current.size + size_of::<Block>());
            capacity += block_size;
            if current.used { used += block_size };
            previous = previous.next.as_ref().unwrap();
        }
        list.finish()?;

        f.debug_struct("")
            .field("used", &used)
            .field("capacity", &capacity)
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
                log::trace!("allocate({layout:?}) = {ptr:?}");
                return Ok(ptr);
            } else {
                previous = previous.next.as_mut().unwrap();
            }
        }

        // no sufficiently big free blocks found
        let previous = if previous.used {
            // last block is used: need to allocate a new one
            todo!();
        } else {
            // last block is free but small: need to extend it
            let padding = Block::layout().padding_needed_for(layout.align());
            let target_size = layout.size() + padding;
            let increase_by = target_size - previous.size;
            let mut increase_by_pages = increase_by.div_ceil(PAGE_SIZE);
            let mut extension_start: VirtAddr = previous.into();
            extension_start = VirtAddr(extension_start.0 + PAGE_SIZE - (extension_start.0 % PAGE_SIZE));
            log::trace!("extending last block by {} pages to {}", increase_by_pages, ByteSize(target_size));

            // the physical allocator may be unable to handle all of them at once
            while increase_by_pages > 0 {
                let mut guard = self.address_space.lock();
                let mut modifier = guard.modify();
                let pages = phys::allocate(increase_by_pages);
                for p_addr in pages.iter() {
                    modifier.map_range(extension_start, p_addr, 1, Default::default(), false).unwrap();
                    increase_by_pages -= 1;
                    previous.size += PAGE_SIZE;
                    extension_start = VirtAddr(extension_start.0 + PAGE_SIZE);
                }
                if pages.len() == 0 {
                    log::trace!("allocate({layout:?}) = no memory!");
                    return Err(AllocError)
                }
            }

            previous
        };

        // try allocating again
        Ok(previous.allocate_possibly_split(layout).unwrap())
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        log::trace!("deallocate({ptr:?}, {layout:?})");
        // obtain block reference
        let blk_layout = Block::layout();
        let down_offs = (layout.align() as isize - blk_layout.align() as isize).max(blk_layout.size() as isize);
        // log::trace!("{ptr:?} {down_offs}");
        let block = ptr.byte_sub(down_offs as usize).cast::<Block>().as_uninit_mut().assume_init_mut();
        // log::trace!("{block:?}");

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
pub unsafe fn initialize_default(addr_space: AddressSpace) {
    let allocator = LinkedListAllocator::new(super::EMULATOR_HEAP, addr_space)
        .expect("failed to create global allocator");
    ALLOCATOR = LateAllocator(Some(allocator));
}

pub fn dump_default() {
    let allocator = unsafe { &ALLOCATOR }.0.as_ref().unwrap();
    log::trace!("{allocator:#?}");
}
