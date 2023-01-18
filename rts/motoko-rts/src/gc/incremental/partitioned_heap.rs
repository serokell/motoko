//! Partitioned heap used in incremental GC for compacting evacuation.
//! The heap is divided in equal sized partitions of a large size `PARTITION_SIZE`.
//! The first partition(s) may contains a static heap space with static objects that are never moved.
//! Beyond the static objects of a partition, the dyanmic heap space starts with `dynamic_size`.
//!
//! Heap layout, with N = `MAX_PARTITIONS`:
//! ┌───────────────┬───────────────┬───────────────┬───────────────┐
//! │  partition 0  │  partition 1  |      ...      | partition N-1 |
//! └───────────────┴───────────────┴───────────────┴───────────────┘
//!
//! Partition layout:
//! ┌───────────────┬───────────────┬───────────────┐
//! │ static_space  │ dynamic_space |  free_space   |
//! └───────────────┴───────────────┴───────────────┘
//!
//! The heap defines an allocation partition that is the target for subsequent object allocations
//! by using efficient bump allocation inside the allocation partition.
//! Whenever a partition is full or has insufficient space to accomodate a new allocation,
//! a new empty partition is selected for allocation.
//!
//! On garbage collection, the high-garbage partitions are selected for evacuation, such that
//! their live objects are moved out to other remaining partitions (through allocation).
//! Therefore, allocation partitions must not be evacuated.
//!
//! Open aspect:
//! * Huge objects: Large allocations > `PARTITION_SIZE` are not yet supported.

use core::array::from_fn;

use crate::types::*;

pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;
// For simplicity, leave the last partition unused, to avoid partition end address overflow
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;

/// Heap partition of size `PARTITION_SIZE`.
pub struct Partition {
    index: usize,
    free: bool,
    marked_space: usize,
    static_size: usize,
    dynamic_size: usize,
    evacuate: bool,
}

impl Partition {
    pub fn get_index(&self) -> usize {
        self.index
    }

    fn start_address(&self) -> usize {
        self.index * PARTITION_SIZE
    }

    fn end_address(&self) -> usize {
        self.start_address() + PARTITION_SIZE
    }

    pub fn dynamic_space_start(&self) -> usize {
        self.start_address() + self.static_size
    }

    pub fn dynamic_space_end(&self) -> usize {
        self.dynamic_space_start() + self.dynamic_size
    }

    pub fn is_free(&self) -> bool {
        self.free
    }

    pub fn to_be_evacuated(&self) -> bool {
        self.evacuate
    }

    #[cfg(debug_assertions)]
    unsafe fn clear_free_remainder(&self) {
        use crate::constants::WORD_SIZE;
        assert!(self.dynamic_space_end() <= self.end_address());
        let remaining_space = self.end_address() - self.dynamic_space_end();
        assert!(remaining_space % WORD_SIZE as usize == 0);
        if remaining_space == 0 {
            return;
        }
        let block = self.dynamic_space_end() as *mut Tag;
        if remaining_space == WORD_SIZE as usize {
            *block = TAG_ONE_WORD_FILLER;
        } else {
            *block = TAG_FREE_SPACE;
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes(remaining_space as u32).to_words(); // includes header
                                                                            // Clear the remainder of the free space.
            let header_size = size_of::<FreeSpace>().to_bytes().as_usize();
            let clear_start = free_space as usize + header_size;
            let clear_length = Bytes((remaining_space - header_size) as u32);
            crate::mem_utils::memzero(clear_start, clear_length.to_words());
        }
    }

    pub unsafe fn free(&mut self) {
        assert!(!self.free);
        assert!(self.evacuate);
        assert_eq!(self.marked_space, 0);
        self.free = true;
        self.dynamic_size = 0;
        self.evacuate = false;

        #[cfg(debug_assertions)]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        assert!(self.marked_space <= dynamic_heap_space);
        self.marked_space as f64 / dynamic_heap_space as f64
    }
}

/// Iterator state that can be stored between GC increments.
pub struct HeapIteratorState {
    partition_index: usize,
    current_address: usize,
}

impl HeapIteratorState {
    pub fn new() -> HeapIteratorState {
        HeapIteratorState {
            partition_index: 0,
            current_address: 0,
        }
    }

    pub fn completed(&self) -> bool {
        assert!(self.partition_index <= MAX_PARTITIONS);
        self.partition_index == MAX_PARTITIONS
    }
}

/// Instantiated per GC increment, operating on a stored `HeapIteratorState`.
pub struct PartitionedHeapIterator<'a> {
    heap: &'a PartitionedHeap,
    partition_index: &'a mut usize,
    current_address: &'a mut usize,
}

// Different to the standard iterator to allow resuming between GC increment pauses.
impl<'a> PartitionedHeapIterator<'a> {
    pub unsafe fn resume(
        heap: &'a PartitionedHeap,
        state: &'a mut HeapIteratorState,
    ) -> PartitionedHeapIterator<'a> {
        let mut iterator = PartitionedHeapIterator {
            heap,
            partition_index: &mut state.partition_index,
            current_address: &mut state.current_address,
        };
        iterator.start();
        iterator
    }

    unsafe fn start(&mut self) {
        if *self.partition_index == 0 && *self.current_address == 0 {
            *self.partition_index = self.heap.base_address() / PARTITION_SIZE;
            *self.current_address = self.heap.base_address();
        }
        if *self.partition_index < MAX_PARTITIONS {
            self.skip_free_space();
        }
    }

    pub fn current_partition(&self) -> Option<&Partition> {
        if *self.partition_index < MAX_PARTITIONS {
            Some(self.heap.get_partition(*self.partition_index))
        } else {
            assert_eq!(*self.current_address, usize::MAX);
            None
        }
    }

    pub fn current_object(&self) -> Option<*mut Obj> {
        if *self.current_address < usize::MAX {
            Some(*self.current_address as *mut Obj)
        } else {
            assert_eq!(*self.partition_index, MAX_PARTITIONS);
            None
        }
    }

    pub fn is_inside_partition(&self, partition_index: usize) -> bool {
        self.current_partition().is_some()
            && self.current_partition().unwrap().get_index() == partition_index
    }

    fn partition_scan_start(&self) -> usize {
        self.current_partition().unwrap().dynamic_space_start()
    }

    fn partition_scan_end(&self) -> usize {
        self.current_partition().unwrap().dynamic_space_end()
    }

    unsafe fn skip_free_space(&mut self) {
        self.skip_free_blocks();
        // Implicitly also skips free partitions as the dynamic size is zero.
        while *self.current_address == self.partition_scan_end() {
            self.start_next_partition();
            if *self.partition_index == MAX_PARTITIONS {
                return;
            }
            self.skip_free_blocks();
        }
    }

    unsafe fn skip_free_blocks(&mut self) {
        while *self.current_address < self.partition_scan_end()
            && !Value::from_ptr(*self.current_address).is_obj()
        {
            let size = block_size(*self.current_address);
            *self.current_address += size.to_bytes().as_usize();
        }
    }

    fn start_next_partition(&mut self) {
        assert!(*self.partition_index < MAX_PARTITIONS);
        *self.partition_index += 1;
        if *self.partition_index < MAX_PARTITIONS {
            *self.current_address = self.partition_scan_start();
        } else {
            *self.current_address = usize::MAX
        }
    }

    pub unsafe fn next_partition(&mut self) {
        self.start_next_partition();
        self.skip_free_space();
    }

    pub unsafe fn next_object(&mut self) {
        assert!(*self.current_address >= self.partition_scan_start());
        assert!(*self.current_address < self.partition_scan_end());
        let size = block_size(*self.current_address);
        *self.current_address += size.to_bytes().as_usize();
        assert!(*self.current_address <= self.partition_scan_end());
        self.skip_free_space();
    }
}

/// Partitioned heap used with the incremental GC.
pub struct PartitionedHeap {
    partitions: [Partition; MAX_PARTITIONS],
    heap_base: usize,
    allocation_index: usize, // Index of the partition currently used for allocations.
}

impl PartitionedHeap {
    pub fn new(heap_base: usize) -> PartitionedHeap {
        let allocation_index = heap_base / PARTITION_SIZE;
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            marked_space: 0,
            static_size: if index < allocation_index {
                PARTITION_SIZE
            } else if index == allocation_index {
                heap_base % PARTITION_SIZE
            } else {
                0
            },
            dynamic_size: 0,
            evacuate: false,
        });
        PartitionedHeap {
            partitions,
            heap_base,
            allocation_index,
        }
    }

    pub fn base_address(&self) -> usize {
        self.heap_base
    }

    pub fn get_partition(&self, index: usize) -> &Partition {
        &self.partitions[index]
    }

    pub fn plan_evacuations(&mut self) {
        for partition in &mut self.partitions {
            assert!(!partition.evacuate);
            partition.evacuate = self.allocation_index != partition.index
                && !partition.is_free()
                && partition.dynamic_space_start() < partition.end_address()
                && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD;
        }
    }

    pub unsafe fn free_evacuated_partitions(&mut self) {
        for partition in &mut self.partitions {
            partition.marked_space = 0;
            if partition.to_be_evacuated() {
                assert!(partition.index != self.allocation_index);
                partition.free();
            }
        }
    }

    fn allocation_partition(&mut self) -> &mut Partition {
        &mut self.partitions[self.allocation_index]
    }

    pub fn is_allocation_partition(&self, index: usize) -> bool {
        self.allocation_index == index
    }

    fn allocate_free_partition(&mut self) -> &Partition {
        for partition in &mut self.partitions {
            if partition.free {
                assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                return partition;
            }
        }
        panic!("Out of memory, no free partition available");
    }

    pub fn occupied_size(&self) -> Bytes<u32> {
        let occupied_size: usize = self
            .partitions
            .iter()
            .map(|partition| {
                if partition.free {
                    partition.static_size
                } else if partition.index == self.allocation_index {
                    partition.static_size + partition.dynamic_size
                } else {
                    PARTITION_SIZE
                }
            })
            .sum();
        Bytes(occupied_size as u32)
    }

    pub unsafe fn record_marked_space(&mut self, object: *mut Obj) {
        let address = object as usize;
        let size = block_size(address);
        let partition = &mut self.partitions[address / PARTITION_SIZE];
        assert!(address >= partition.dynamic_space_start());
        assert!(address + size.to_bytes().as_usize() <= partition.dynamic_space_end());
        partition.marked_space += size.to_bytes().as_usize();
    }

    pub unsafe fn prepare_allocation_partition(
        &mut self,
        heap_pointer: &mut u32,
        allocation_size: Bytes<u32>,
    ) {
        if allocation_size.as_usize() > PARTITION_SIZE {
            panic!("Allocation exceeds partition size");
        }
        let mut allocation_partition = self.allocation_partition();
        assert!(!allocation_partition.free);
        assert!(
            (*heap_pointer as usize) >= allocation_partition.dynamic_space_start()
                && (*heap_pointer as usize) <= allocation_partition.dynamic_space_end()
        );
        assert!(allocation_size.as_usize() <= allocation_partition.end_address());
        if *heap_pointer as usize > allocation_partition.end_address() - allocation_size.as_usize()
        {
            self.open_new_allocation_partition(heap_pointer);
            allocation_partition = self.allocation_partition();
        }
        (*allocation_partition).dynamic_size += allocation_size.as_usize();
    }

    unsafe fn open_new_allocation_partition(&mut self, heap_pointer: &mut u32) {
        let old_partition = self.allocation_partition();
        assert_eq!(*heap_pointer as usize, old_partition.dynamic_space_end());

        #[cfg(debug_assertions)]
        old_partition.clear_free_remainder();

        let new_partition = self.allocate_free_partition();
        *heap_pointer = new_partition.dynamic_space_start() as u32;
        self.allocation_index = new_partition.index;
    }
}