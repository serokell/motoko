//! Partitioned heap used in incremental GC for compacting evacuation.
//! The heap is divided in equal sized partitions of a large size `PARTITION_SIZE`.
//! The first partition(s) may contain a static heap space with static objects that are never moved.
//! Beyond the static objects of a partition, the dynamic heap space starts with `dynamic_size`.
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
//! Whenever a partition is full or has insufficient space to accommodate a new allocation,
//! a new empty partition is selected for allocation.
//!
//! On garbage collection, partitions are selected for evacuation by prioritizing high-garbage
//! partitions. The live objects of the partitions selected for evacuation are moved out to
//! other remaining partitions (through allocation). Thereby, objects from different evacuated
//! partitions can be allocated to a common partition. Allocation partitions must not be evacuated.
//!
//! To prevent that the evacuation phase runs out of free space, the number of evacuated
//! partitions is limited to the free space that is available and needed for the evacuations.
//!
//! Large allocations:
//! Huge objects with a size > PARTITION_SIZE are allocated across multiple contiguous free
//! partitions. For this purpose, a corresponding sequence of contiguous free partitions needs
//! to be searched. Huge objects stay in their partitions for their entire lifetime, i.e. they
//! are never evacuated. When becoming garbage, the underlying partitions of a huge object are
//! immediately freed. Large object allocation may be prone to external fragmentation problems,
//! i.e. that no sufficient contiguous free partitions are available on allocation. Currently,
//! this external fragmentation problem is not handled by moving other partitions which would
//! require a special blocking full GC collection. Moreover, for simplicity, the remainder
//! of the last partition of a huge object is not used for further small object allocations,
//! which implies limited internal fragmentation.

use core::{array::from_fn, ops::Range, ptr::null_mut};

use crate::{
    constants::WASM_MEMORY_BYTE_SIZE, gc::incremental::mark_bitmap::BITMAP_ITERATION_END,
    memory::Memory, rts_trap_with, types::*,
};

use super::{
    mark_bitmap::{BitmapIterator, MarkBitmap, BITMAP_SIZE, DEFAULT_MARK_BITMAP},
    sort::sort,
    time::BoundedTime,
};

/// Size of each partition.
/// Select the size of the power of two with the smallest WASM memory size in the benchmark.
/// -> Small partitions below 32 MB are inefficient in terms of both memory and runtime costs
///    due to the increased frequency of large object handling.
/// -> Large partitions above 32 MB are a waste for small programs, since the WASM memory is
///    allocated in that granularity and GC is then triggered later.
pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;

/// Total number of partitions in the memory.
/// For simplicity, the last partition is left unused, to avoid a numeric overflow when
/// computing the end address of the last partition.
const MAX_PARTITIONS: usize = (WASM_MEMORY_BYTE_SIZE.0 / PARTITION_SIZE as u64) as usize - 1;

/// Partitions are only evacuated if the space occupation of alive objects in the partition
/// is greater than this threshold.
/// Based on benchmark measurements, this rate is tuned to optimize the following metrics
/// in the order of occurrence:
/// 1. Lowest heap size (most reclamation).
/// 2. Lowest WASM memory size (i.e. for minimum heap size).
pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.85;

/// Heap partition of size `PARTITION_SIZE`.
pub struct Partition {
    index: usize,        // Index of the partition `0..MAX_PARTITIONS`.
    free: bool,          // Denotes a free partition (which may still contain static space).
    large_content: bool, // Specifies whether a large object is contained that spans multiple partitions.
    marked_size: usize,  // Total amount of marked object space in the dynamic space.
    static_size: usize,  // Size of the static space.
    dynamic_size: usize, // Size of the dynamic space.
    bitmap: MarkBitmap,  // Mark bitmap used for marking objects inside this partition.
    temporary: bool,     // Specifies a temporary partition used during a GC run to store bitmaps.
    evacuate: bool,      // Specifies whether the partition is to be evacuated or being evacuated.
    update: bool,        // Specifies whether the pointers in the partition have to be updated.
}

/// Optimization: Avoiding `Option` or `Lazy`.
const UNINITIALIZED_PARTITION: Partition = Partition {
    index: usize::MAX,
    free: false,
    large_content: false,
    marked_size: 0,
    static_size: 0,
    dynamic_size: 0,
    bitmap: DEFAULT_MARK_BITMAP,
    temporary: false,
    evacuate: false,
    update: false,
};

impl Partition {
    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn start_address(&self) -> usize {
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

    pub fn dynamic_size(&self) -> usize {
        self.dynamic_size
    }

    pub fn occuped_size(&self) -> usize {
        self.dynamic_size + self.static_size
    }

    pub fn marked_size(&self) -> usize {
        self.marked_size
    }

    pub fn free_size(&self) -> usize {
        self.end_address() - self.dynamic_space_end()
    }

    pub fn garbage_amount(&self) -> usize {
        debug_assert!(self.marked_size <= self.dynamic_size);
        self.dynamic_size - self.marked_size
    }

    pub fn is_free(&self) -> bool {
        self.free
    }

    pub fn is_evacuation_candidate(&self) -> bool {
        !self.is_free()
            && !self.has_large_content()
            && self.dynamic_size() > 0
            && self.survival_rate() <= SURVIVAL_RATE_THRESHOLD
    }

    pub fn to_be_evacuated(&self) -> bool {
        self.evacuate
    }

    pub fn to_be_updated(&self) -> bool {
        self.update
    }

    pub fn is_temporary(&self) -> bool {
        self.temporary
    }

    pub fn has_dynamic_space(&self) -> bool {
        !self.free && !self.temporary && self.static_size != PARTITION_SIZE
    }

    pub fn get_bitmap(&self) -> &MarkBitmap {
        &self.bitmap
    }

    pub fn mutable_bitmap(&mut self) -> &mut MarkBitmap {
        &mut self.bitmap
    }

    #[cfg(feature = "memory_check")]
    unsafe fn clear_free_remainder(&self) {
        use crate::constants::WORD_SIZE;
        debug_assert!(self.dynamic_space_end() <= self.end_address());
        let remaining_space = self.end_address() - self.dynamic_space_end();
        debug_assert_eq!(remaining_space % WORD_SIZE as usize, 0);
        debug_assert!(remaining_space <= PARTITION_SIZE);
        if remaining_space == 0 {
            return;
        }
        let block = self.dynamic_space_end() as *mut Tag;
        if remaining_space == WORD_SIZE as usize {
            *block = TAG_ONE_WORD_FILLER;
        } else {
            *block = TAG_FREE_SPACE;
            let header_size = size_of::<FreeSpace>().to_bytes().as_usize();
            debug_assert!(remaining_space >= header_size);
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes((remaining_space - header_size) as u32).to_words();
            // Clear the remainder of the free space.
            let clear_start = free_space as usize + header_size;
            let clear_length = Bytes((remaining_space - header_size) as u32);
            crate::mem_utils::memzero(clear_start, clear_length.to_words());
            debug_assert_eq!(free_space.size().to_bytes().as_usize(), remaining_space);
        }
    }

    pub unsafe fn free(&mut self) {
        debug_assert!(!self.free);
        debug_assert!(self.evacuate || self.large_content || self.temporary);
        debug_assert_eq!(self.marked_size, 0);
        debug_assert!(!self.update);
        self.free = true;
        self.dynamic_size = 0;
        self.evacuate = false;
        self.large_content = false;
        self.temporary = false;

        #[cfg(feature = "memory_check")]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        debug_assert!(self.marked_size <= dynamic_heap_space);
        self.marked_size as f64 / dynamic_heap_space as f64
    }

    pub fn has_large_content(&self) -> bool {
        self.large_content
    }

    pub fn is_completely_free(&self) -> bool {
        self.free && self.free_size() == PARTITION_SIZE
    }
}

/// Iterates over all partitions and their contained marked objects, by skipping
/// free partitions, the subsequent partitions of large objects, and unmarked objects.
pub struct PartitionedHeapIterator {
    partition_index: usize,
    bitmap_iterator: Option<BitmapIterator>,
    visit_large_object: bool,
}

impl PartitionedHeapIterator {
    pub fn new(heap: &PartitionedHeap) -> PartitionedHeapIterator {
        let mut iterator = PartitionedHeapIterator {
            partition_index: 0,
            bitmap_iterator: None,
            visit_large_object: false,
        };
        iterator.skip_empty_partitions(heap);
        iterator.start_object_iteration(heap);
        iterator
    }

    fn skip_empty_partitions(&mut self, heap: &PartitionedHeap) {
        loop {
            if self.partition_index == MAX_PARTITIONS {
                return;
            }
            let partition = heap.get_partition(self.partition_index);
            if partition.has_dynamic_space() {
                return;
            }
            self.partition_index += 1;
        }
    }

    pub fn has_partition(&self) -> bool {
        self.partition_index < MAX_PARTITIONS
    }

    pub fn current_partition<'a>(&self, heap: &'a PartitionedHeap) -> &'a Partition {
        debug_assert!(self.partition_index < MAX_PARTITIONS);
        heap.get_partition(self.partition_index)
    }

    pub unsafe fn next_partition(&mut self, heap: &PartitionedHeap) {
        debug_assert!(self.partition_index < MAX_PARTITIONS);
        let partition = heap.get_partition(self.partition_index);
        let number_of_partitions = if partition.has_large_content() {
            let large_object = partition.dynamic_space_start() as *mut Obj;
            PartitionedHeap::partitions_length(large_object)
        } else {
            1
        };
        self.partition_index += number_of_partitions;
        self.skip_empty_partitions(heap);
        self.start_object_iteration(heap)
    }

    fn start_object_iteration(&mut self, heap: &PartitionedHeap) {
        debug_assert!(self.partition_index <= MAX_PARTITIONS);
        if self.partition_index == MAX_PARTITIONS {
            self.bitmap_iterator = None;
            self.visit_large_object = false;
        } else {
            let partition = heap.get_partition(self.partition_index);
            if partition.has_large_content() {
                self.bitmap_iterator = None;
                self.visit_large_object = partition.marked_size() > 0
            } else {
                self.bitmap_iterator = Some(partition.get_bitmap().iterate());
                self.visit_large_object = false;
            }
        }
    }

    pub fn has_object(&self) -> bool {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            offset != BITMAP_ITERATION_END
        } else {
            self.visit_large_object
        }
    }

    pub fn current_object(&self) -> *mut Obj {
        let partition_start = self.partition_index * PARTITION_SIZE;
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            debug_assert_ne!(offset, BITMAP_ITERATION_END);
            let address = partition_start + offset;
            address as *mut Obj
        } else {
            debug_assert!(self.visit_large_object);
            partition_start as *mut Obj
        }
    }

    pub fn next_object(&mut self) {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_mut().unwrap();
            debug_assert_ne!(iterator.current_marked_offset(), BITMAP_ITERATION_END);
            iterator.next();
        } else {
            debug_assert!(self.visit_large_object);
            self.visit_large_object = false;
        }
    }
}

/// Partitioned heap used by the incremental GC.
pub struct PartitionedHeap {
    partitions: [Partition; MAX_PARTITIONS],
    heap_base: usize,
    allocation_index: usize, // Index of the partition currently used for allocations.
    free_partitions: usize,  // Number of free partitions.
    evacuating: bool,
    reclaimed: u64,
    bitmap_allocation_pointer: usize, // Free pointer for allocating the next mark bitmap.
    gc_running: bool, // Create bitmaps for partitions when allocated during active GC.
    precomputed_heap_size: usize, // Occupied heap size, excluding the dynamic heap in the allocation partition.
    evacuated_size: usize, // Size of all evacuated objects during a GC run. Serves for accurate total allocation statistics.
}

/// Optimization: Avoiding `Option` or `LazyCell`.
pub const UNINITIALIZED_HEAP: PartitionedHeap = PartitionedHeap {
    partitions: [UNINITIALIZED_PARTITION; MAX_PARTITIONS],
    heap_base: 0,
    allocation_index: 0,
    free_partitions: 0,
    evacuating: false,
    reclaimed: 0,
    bitmap_allocation_pointer: 0,
    gc_running: false,
    precomputed_heap_size: 0,
    evacuated_size: 0,
};

impl PartitionedHeap {
    pub unsafe fn new<M: Memory>(mem: &mut M, heap_base: usize) -> PartitionedHeap {
        let allocation_index = heap_base / PARTITION_SIZE;
        mem.grow_memory(((allocation_index + 1) * PARTITION_SIZE) as u64);
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            large_content: false,
            marked_size: 0,
            static_size: if index < allocation_index {
                PARTITION_SIZE
            } else if index == allocation_index {
                heap_base % PARTITION_SIZE
            } else {
                0
            },
            dynamic_size: 0,
            bitmap: MarkBitmap::new(),
            temporary: false,
            evacuate: false,
            update: false,
        });
        debug_assert!(allocation_index <= MAX_PARTITIONS);
        let free_partitions = MAX_PARTITIONS - allocation_index - 1;
        PartitionedHeap {
            partitions,
            heap_base,
            allocation_index,
            free_partitions,
            evacuating: false,
            reclaimed: 0,
            bitmap_allocation_pointer: 0,
            gc_running: false,
            precomputed_heap_size: heap_base,
            evacuated_size: 0,
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.partitions[0].index == 0
    }

    pub fn base_address(&self) -> usize {
        self.heap_base
    }

    pub fn get_partition(&self, index: usize) -> &Partition {
        &self.partitions[index]
    }

    fn mutable_partition(&mut self, index: usize) -> &mut Partition {
        &mut self.partitions[index]
    }

    unsafe fn allocate_temporary_partition(&mut self) -> &mut Partition {
        for partition in &mut self.partitions {
            if partition.is_completely_free() {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                partition.temporary = true;
                debug_assert!(self.free_partitions > 0);
                self.free_partitions -= 1;
                return partition;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    /// The returned bitmap address is guaranteed to be 64-bit-aligned.
    unsafe fn allocate_bitmap<M: Memory>(&mut self, mem: &mut M) -> *mut u8 {
        if self.bitmap_allocation_pointer % PARTITION_SIZE == 0 {
            let partition = self.allocate_temporary_partition();
            mem.grow_memory(partition.end_address() as u64);
            self.bitmap_allocation_pointer = partition.start_address();
        }
        let bitmap_address = self.bitmap_allocation_pointer as *mut u8;
        self.bitmap_allocation_pointer += BITMAP_SIZE;
        debug_assert_eq!(
            bitmap_address as usize % size_of::<u64>().to_bytes().as_usize(),
            0
        );
        bitmap_address
    }

    // Optimization: Returns true if the object transitioned from unmarked to marked.
    pub unsafe fn mark_object(&mut self, object: *mut Obj) -> bool {
        let address = object as usize;
        let partition_index = address / PARTITION_SIZE;
        let partition = self.mutable_partition(partition_index);
        if partition.has_large_content() {
            return self.mark_large_object(object);
        }
        let bitmap = partition.mutable_bitmap();
        let offset = address % PARTITION_SIZE;
        if bitmap.is_marked(offset) {
            return false;
        }
        bitmap.mark(offset);
        partition.marked_size += block_size(address).to_bytes().as_usize();
        true
    }

    #[cfg(debug_assertions)]
    pub unsafe fn is_object_marked(&self, object: *mut Obj) -> bool {
        let address = object as usize;
        let partition_index = address / PARTITION_SIZE;
        let partition = self.get_partition(partition_index);
        if partition.has_large_content() {
            return self.is_large_object_marked(object);
        }
        let bitmap = partition.get_bitmap();
        let offset = address % PARTITION_SIZE;
        bitmap.is_marked(offset)
    }

    pub unsafe fn start_collection<M: Memory>(&mut self, mem: &mut M, time: &mut BoundedTime) {
        self.check_occupied_size();
        debug_assert_eq!(self.bitmap_allocation_pointer, 0);
        debug_assert!(!self.gc_running);
        self.gc_running = true;
        for partition_index in 0..MAX_PARTITIONS {
            let partition = self.get_partition(partition_index);
            if partition.has_dynamic_space() && !partition.has_large_content() {
                let bitmap_address = self.allocate_bitmap(mem);
                self.mutable_partition(partition_index)
                    .bitmap
                    .assign(bitmap_address);
                time.advance(Bytes(BITMAP_SIZE as u32).to_words().as_usize());
            }
        }
    }

    pub fn plan_evacuations(&mut self) {
        let ranked_partitions = self.rank_partitions_by_garbage();
        debug_assert_eq!(
            self.partitions
                .iter()
                .filter(|partition| partition.is_free())
                .count(),
            self.free_partitions
        );
        // Do not use all free partitions for evacuation.
        // Leave a reserve for mutator allocations during a GC run.
        const EVACUATION_FRACTION: usize = 2;
        let reserved_partitions =
            (self.free_partitions + EVACUATION_FRACTION - 1) / EVACUATION_FRACTION;
        let mut evacuation_space = reserved_partitions * PARTITION_SIZE;
        for index in ranked_partitions {
            if index != self.allocation_index && self.get_partition(index).is_evacuation_candidate()
            {
                let partition = self.mutable_partition(index);
                if evacuation_space < partition.marked_size() {
                    // Limit the evacuations to the available free space for the current GC run.
                    return;
                }
                evacuation_space -= partition.marked_size();
                partition.evacuate = true;
                self.evacuating = true;
                debug_assert_eq!(self.evacuated_size, 0);
            }
        }
    }

    fn rank_partitions_by_garbage(&self) -> [usize; MAX_PARTITIONS] {
        let mut ranked_partitions: [usize; MAX_PARTITIONS] = from_fn(|index| index);
        sort(&mut ranked_partitions, &|left, right| {
            self.get_partition(left)
                .garbage_amount()
                .cmp(&self.get_partition(right).garbage_amount())
                .reverse()
        });
        ranked_partitions
    }

    pub fn plan_updates(&mut self) {
        for partition in &mut self.partitions {
            debug_assert!(!partition.update);
            partition.update = !partition.is_free() && !partition.evacuate;
        }
    }

    pub unsafe fn complete_collection(&mut self) {
        for partition in &mut self.partitions {
            let marked_size = partition.marked_size;
            partition.update = false;
            partition.marked_size = 0;
            partition.bitmap.release();
            if partition.to_be_evacuated() {
                debug_assert!(partition.index != self.allocation_index);
                debug_assert!(partition.dynamic_size >= marked_size);
                self.reclaimed += (partition.dynamic_size - marked_size) as u64;
            }
            if partition.to_be_evacuated() || partition.temporary {
                self.precomputed_heap_size -= partition.dynamic_size;
                partition.free();
                self.free_partitions += 1;
            }
        }
        self.evacuating = false;
        self.evacuated_size = 0;
        self.bitmap_allocation_pointer = 0;
        debug_assert!(self.gc_running);
        self.gc_running = false;
        self.check_occupied_size();
    }

    pub fn updates_needed(&self) -> bool {
        self.evacuating
    }

    fn allocation_partition(&self) -> &Partition {
        &self.partitions[self.allocation_index]
    }

    fn mut_allocation_partition(&mut self) -> &mut Partition {
        &mut self.partitions[self.allocation_index]
    }

    pub fn is_allocation_partition(&self, index: usize) -> bool {
        self.allocation_index == index
    }

    unsafe fn allocate_free_partition<M: Memory>(
        &mut self,
        mem: &mut M,
        requested_space: usize,
    ) -> &mut Partition {
        let bitmap_address = if self.gc_running {
            self.allocate_bitmap(mem)
        } else {
            null_mut()
        };
        for partition in &mut self.partitions {
            if partition.free && partition.free_size() >= requested_space {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                debug_assert!(self.free_partitions > 0);
                self.free_partitions -= 1;
                if bitmap_address != null_mut() {
                    partition.bitmap.assign(bitmap_address);
                }
                return partition;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    fn check_occupied_size(&self) {
        debug_assert_eq!(
            self.partitions
                .iter()
                .map(|partition| partition.static_size + partition.dynamic_size)
                .sum::<usize>(),
            self.occupied_size().as_usize()
        );
    }

    pub fn occupied_size(&self) -> Bytes<u32> {
        Bytes((self.precomputed_heap_size + self.allocation_partition().dynamic_size) as u32)
    }

    pub fn reclaimed_size(&self) -> Bytes<u64> {
        Bytes(self.reclaimed)
    }

    pub fn increase_evacuated_size(&mut self, size: Words<u32>) {
        self.evacuated_size += size.to_bytes().as_usize();
    }

    pub fn total_allocated_size(&self) -> Bytes<u64> {
        debug_assert!(self.evacuated_size <= self.occupied_size().as_usize());
        let heap_size_without_evacuations = self.occupied_size().as_usize() - self.evacuated_size;
        Bytes(heap_size_without_evacuations as u64) + self.reclaimed_size()
    }

    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M, words: Words<u32>) -> Value {
        let size = words.to_bytes().as_usize();
        if size <= PARTITION_SIZE {
            self.allocate_normal_object(mem, size)
        } else {
            self.allocate_large_object(mem, size)
        }
    }

    unsafe fn allocate_normal_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        debug_assert!(size <= PARTITION_SIZE);
        let mut allocation_partition = self.mut_allocation_partition();
        debug_assert!(!allocation_partition.free);
        let heap_pointer = allocation_partition.dynamic_space_end();
        debug_assert!(size <= allocation_partition.end_address());
        if heap_pointer <= allocation_partition.end_address() - size {
            (*allocation_partition).dynamic_size += size;
            Value::from_ptr(heap_pointer)
        } else {
            self.allocate_in_new_partition(mem, size)
        }
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_in_new_partition<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        #[cfg(feature = "memory_check")]
        self.allocation_partition().clear_free_remainder();

        self.precomputed_heap_size += self.allocation_partition().dynamic_size;

        let new_partition = self.allocate_free_partition(mem, size);
        mem.grow_memory(new_partition.end_address() as u64);
        let heap_pointer = new_partition.dynamic_space_end();
        new_partition.dynamic_size += size;
        self.allocation_index = new_partition.index;
        Value::from_ptr(heap_pointer)
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_large_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        if size > usize::MAX - PARTITION_SIZE {
            panic!("Too large allocation");
        }
        let number_of_partitions = (size + PARTITION_SIZE - 1) / PARTITION_SIZE;
        debug_assert!(number_of_partitions > 0);

        let first_index = self.find_large_space(number_of_partitions);
        let last_index = first_index + number_of_partitions - 1;

        debug_assert!(self.free_partitions >= number_of_partitions);
        self.free_partitions -= number_of_partitions;

        let end_address = self.get_partition(last_index).end_address();
        mem.grow_memory(end_address as u64);
        for index in first_index..last_index + 1 {
            let partition = self.mutable_partition(index);
            debug_assert!(partition.free);
            debug_assert!(!partition.large_content);
            partition.free = false;
            partition.large_content = true;
            debug_assert_eq!(partition.static_size, 0);
            debug_assert_eq!(partition.dynamic_size, 0);
            debug_assert_eq!(partition.marked_size, 0);
            if index == last_index {
                partition.dynamic_size = size - (number_of_partitions - 1) * PARTITION_SIZE;

                #[cfg(feature = "memory_check")]
                partition.clear_free_remainder();
            } else {
                partition.dynamic_size = PARTITION_SIZE;
            }
            self.precomputed_heap_size += partition.dynamic_size;
        }
        let first_partition = self.mutable_partition(first_index);
        Value::from_ptr(first_partition.dynamic_space_start())
    }

    unsafe fn find_large_space(&self, number_of_partitions: usize) -> usize {
        let mut start_of_free_range = 0;
        for index in 0..MAX_PARTITIONS {
            // Invariant: [start_of_free_range .. index) contains only free partitions.
            if self.get_partition(index).is_completely_free() {
                if index + 1 - start_of_free_range >= number_of_partitions {
                    return start_of_free_range;
                }
            } else {
                start_of_free_range = index + 1;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    unsafe fn occupied_partition_range(large_object: *mut Obj) -> Range<usize> {
        debug_assert_eq!(large_object as usize % PARTITION_SIZE, 0);
        let start_partition = large_object as usize / PARTITION_SIZE;
        let number_of_partitions = Self::partitions_length(large_object);
        start_partition..start_partition + number_of_partitions
    }

    unsafe fn partitions_length(large_object: *mut Obj) -> usize {
        let size = block_size(large_object as usize).to_bytes().as_usize();
        debug_assert!(size > PARTITION_SIZE);
        (size + PARTITION_SIZE - 1) / PARTITION_SIZE
    }

    pub unsafe fn collect_large_objects(&mut self) {
        let mut index = 0;
        while index < MAX_PARTITIONS {
            let partition = self.get_partition(index);
            if partition.has_large_content() {
                debug_assert!(!partition.free);
                let object = partition.dynamic_space_start() as *mut Obj;
                let number_of_partitions = Self::partitions_length(object);
                if partition.marked_size == 0 {
                    self.free_large_object(object);
                }
                index += number_of_partitions;
            } else {
                index += 1;
            }
        }
    }

    unsafe fn free_large_object(&mut self, object: *mut Obj) {
        let occupied_range = Self::occupied_partition_range(object);
        self.free_partitions += occupied_range.len();
        for index in occupied_range {
            let partition = self.mutable_partition(index);
            debug_assert!(partition.large_content);
            let size = partition.dynamic_size;
            partition.update = false;
            partition.free();
            self.reclaimed += size as u64;
            self.precomputed_heap_size -= size;
        }
    }

    // Significant performance gain by not inlining.
    // Optimization: Returns true if it has not yet been marked before.
    #[inline(never)]
    unsafe fn mark_large_object(&mut self, object: *mut Obj) -> bool {
        let range = Self::occupied_partition_range(object);
        if self.partitions[range.start].marked_size > 0 {
            return false;
        }
        for index in range.start..range.end - 1 {
            self.partitions[index].marked_size = PARTITION_SIZE;
        }
        let object_size = block_size(object as usize).to_bytes().as_usize();
        self.partitions[range.end - 1].marked_size = object_size % PARTITION_SIZE;
        true
    }

    #[cfg(debug_assertions)]
    unsafe fn is_large_object_marked(&self, object: *mut Obj) -> bool {
        let range = Self::occupied_partition_range(object);
        self.partitions[range.start].marked_size > 0
    }
}
