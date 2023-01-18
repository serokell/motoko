use std::{
    io::{stdout, Write},
    mem::size_of,
};

use motoko_rts::{
    gc::incremental::partitioned_heap::{
        HeapIteratorState, Partition, PartitionedHeap, PartitionedHeapIterator, PARTITION_SIZE,
        SURVIVAL_RATE_THRESHOLD,
    },
    memory::{alloc_array, alloc_blob, Memory},
    types::{
        is_marked, unmark, Array, Blob, Bytes, FreeSpace, OneWordFiller, Tag, Value, Words,
        TAG_ARRAY, TAG_BLOB, TAG_FREE_SPACE, TAG_ONE_WORD_FILLER,
    },
};

use crate::gc::utils::WORD_SIZE;

const NUMBER_OF_OBJECTS: usize = 2 * PARTITION_SIZE / 16;
const HEAP_SIZE: usize = 3 * PARTITION_SIZE;

pub unsafe fn test() {
    println!("  Testing partitioned heap...");

    let mut heap = create_test_heap();
    let occupied_partitions = 1 + heap.heap_pointer as usize / PARTITION_SIZE;
    test_allocation_partitions(&heap.inner, occupied_partitions);
    test_iteration(&heap.inner, 1024, occupied_partitions);
    test_evacuation_plan(&mut heap.inner, occupied_partitions);
    test_freeing_partitions(&mut heap.inner, occupied_partitions);
    test_reallocations(&mut heap);
    test_evacuation_plan(&mut heap.inner, occupied_partitions);
    test_freeing_partitions(&mut heap.inner, occupied_partitions);
    test_close_partition(&mut heap);
    test_survival_rate(&mut heap.inner);
}

fn test_allocation_partitions(heap: &PartitionedHeap, number_of_partitions: usize) {
    println!("    Test allocation partitions...");
    assert!(number_of_partitions >= 1);
    assert!(heap.is_allocation_partition(number_of_partitions - 1));
    for index in 0..number_of_partitions - 1 {
        assert!(!heap.is_allocation_partition(index));
    }
}

unsafe fn test_iteration(
    heap: &PartitionedHeap,
    break_step_size: usize,
    occupied_partitions: usize,
) {
    println!("    Test heap iteration...");
    let mut iterator_state = HeapIteratorState::new();
    let mut count = 0;
    let mut previous_partition = None;
    loop {
        let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
        while iterator.current_object().is_some() {
            assert!(iterator.current_partition().is_some());
            let object = iterator.current_object().unwrap();
            let array = Value::from_ptr(object as usize).as_array();
            let content = array.get(0).get_scalar();
            assert_eq!(content as usize, count);
            count += 1;
            progress(count, NUMBER_OF_OBJECTS);
            let partition = iterator.current_partition().unwrap();
            assert!(!partition.is_free());
            assert!(!partition.to_be_evacuated());
            assert_eq!(partition.get_index(), object as usize / PARTITION_SIZE);
            assert!(partition.get_index() < occupied_partitions);
            if previous_partition.is_none() || partition.get_index() != previous_partition.unwrap()
            {
                check_partition_index(&iterator, Some(partition.get_index()));
                previous_partition = Some(partition.get_index());
            }
            iterator.next_object();
            if count % break_step_size == 0 {
                break;
            }
        }
        if iterator.current_object().is_none() {
            assert!(iterator.current_partition().is_none());
            assert!(!iterator.is_inside_partition(0));
            assert!(!iterator.is_inside_partition(usize::MAX / PARTITION_SIZE));
            break;
        }
    }
    assert_eq!(count, NUMBER_OF_OBJECTS);
    reset_progress();
}

unsafe fn check_partition_index(
    iterator: &PartitionedHeapIterator,
    expected_partition_index: Option<usize>,
) {
    for index in 0..usize::MAX / PARTITION_SIZE {
        if iterator.is_inside_partition(index) {
            assert_eq!(expected_partition_index.unwrap(), index);
        } else {
            assert!(
                expected_partition_index.is_none() || expected_partition_index.unwrap() != index
            );
        }
    }
}

unsafe fn test_evacuation_plan(heap: &mut PartitionedHeap, occupied_partitions: usize) {
    println!("    Test evacuation plan...");
    heap.plan_evacuations();
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    let mut allocation_partition_present = false;
    while iterator.current_partition().is_some() {
        assert!(iterator.current_object().is_some());
        let partition = iterator.current_partition().unwrap();
        assert!(partition.get_index() < occupied_partitions);
        assert!(!partition.is_free());
        if heap.is_allocation_partition(partition.get_index()) {
            allocation_partition_present = true;
            assert!(!partition.to_be_evacuated());
        } else {
            assert!(partition.to_be_evacuated());
        }
        iterator.next_partition();
    }
    assert!(allocation_partition_present);
}

unsafe fn test_freeing_partitions(heap: &mut PartitionedHeap, occupied_partitions: usize) {
    println!("    Test freeing partitions...");
    heap.free_evacuated_partitions();
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    let partition = iterator.current_partition().unwrap();
    assert!(partition.get_index() < occupied_partitions);
    assert!(!partition.is_free());
    assert!(!partition.to_be_evacuated());
    assert!(heap.is_allocation_partition(partition.get_index()));
    iterator.next_partition();
    assert!(iterator.current_partition().is_none());
    assert!(heap.occupied_size().as_usize() < PARTITION_SIZE);
}

const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;

unsafe fn test_reallocations(heap: &mut PartitionedTestHeap) {
    println!("    Test reallocations...");
    let remaining_objects = count_objects(&heap.inner);
    allocate_objects(heap);
    assert!(heap.inner.occupied_size().as_usize() >= NUMBER_OF_OBJECTS * OBJECT_SIZE);
    let final_objects = count_objects(&heap.inner);
    assert_eq!(final_objects, remaining_objects + NUMBER_OF_OBJECTS);
}

unsafe fn count_objects(heap: &PartitionedHeap) -> usize {
    let mut count = 0;
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    while iterator.current_object().is_some() {
        assert!(iterator.current_partition().is_some());
        let object = iterator.current_object().unwrap();
        let array = Value::from_ptr(object as usize).as_array();
        let content = array.get(0).get_scalar() as usize;
        assert!(content < NUMBER_OF_OBJECTS);
        count += 1;
        if count <= NUMBER_OF_OBJECTS {
            progress(count, NUMBER_OF_OBJECTS);
        }
        let partition = iterator.current_partition().unwrap();
        assert!(!partition.is_free());
        assert!(!partition.to_be_evacuated());
        assert_eq!(partition.get_index(), object as usize / PARTITION_SIZE);
        iterator.next_object();
    }
    assert!(iterator.current_partition().is_none());
    reset_progress();
    count
}

fn test_close_partition(heap: &mut PartitionedTestHeap) {
    println!("    Test close partition...");
    test_close_partition_with_free_space(heap);
    test_close_partition_with_one_wordfiller(heap);
}

fn test_close_partition_with_free_space(heap: &mut PartitionedTestHeap) {
    let old_heap_pointer = heap.heap_pointer as usize;
    let old_partition = old_heap_pointer / PARTITION_SIZE;
    let remainder = PARTITION_SIZE - old_heap_pointer % PARTITION_SIZE;
    let blob = heap.allocate_blob(remainder);
    assert_ne!(heap.heap_pointer as usize / PARTITION_SIZE, old_partition);
    assert_ne!(blob.get_ptr(), old_heap_pointer);
    unsafe {
        let free_space = old_heap_pointer as *const FreeSpace;
        assert_eq!((*free_space).tag, TAG_FREE_SPACE);
        assert_eq!((*free_space).words.to_bytes().as_usize(), remainder);
    }
}

fn test_close_partition_with_one_wordfiller(heap: &mut PartitionedTestHeap) {
    let old_heap_pointer = heap.heap_pointer as usize;
    let old_partition = old_heap_pointer / PARTITION_SIZE;
    let remainder = PARTITION_SIZE - old_heap_pointer % PARTITION_SIZE;
    assert!(remainder > size_of::<Blob>());
    let old_partition_blob = heap.allocate_blob(remainder - size_of::<Blob>() - WORD_SIZE);
    assert_eq!(old_partition_blob.get_ptr(), old_heap_pointer);
    let filler_address = heap.heap_pointer as usize;
    let new_partition_blob = heap.allocate_blob(0);
    assert_ne!(heap.heap_pointer as usize / PARTITION_SIZE, old_partition);
    assert_ne!(new_partition_blob.get_ptr(), filler_address);
    unsafe {
        let filler = filler_address as *const OneWordFiller;
        assert_eq!((*filler).tag, TAG_ONE_WORD_FILLER);
    }
}

unsafe fn mark_all_objects(heap: &mut PartitionedHeap) {
    let mut all_objects = vec![];
    let mut count = 0;
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    while iterator.current_object().is_some() {
        assert!(iterator.current_partition().is_some());
        let object = iterator.current_object().unwrap();
        all_objects.push(object);
        count += 1;
        if count <= NUMBER_OF_OBJECTS {
            progress(count, NUMBER_OF_OBJECTS);
        }
        iterator.next_object();
    }
    assert!(iterator.current_partition().is_none());
    reset_progress();
    for object in all_objects.iter() {
        object.mark();
        heap.record_marked_space(*object);
    }
}

unsafe fn test_survival_rate(heap: &mut PartitionedHeap) {
    println!("    Test survival rate...");
    mark_all_objects(heap);
    heap.plan_evacuations();
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    while iterator.current_partition().is_some() {
        let partition = iterator.current_partition().unwrap();
        let dynamic_partition_size =
            PARTITION_SIZE - partition.dynamic_space_start() % PARTITION_SIZE;
        let expected_survival_rate =
            occupied_space(partition) as f64 / dynamic_partition_size as f64;
        assert!(f64::abs(partition.survival_rate() - expected_survival_rate) < 1e6);
        let expected_evacuation = !heap.is_allocation_partition(partition.get_index())
            && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD;
        assert_eq!(partition.to_be_evacuated(), expected_evacuation);
        iterator.next_partition();
    }
}

unsafe fn occupied_space(partition: &Partition) -> usize {
    let mut sweep_line = partition.dynamic_space_start();
    let mut occupied_space = 0;
    while sweep_line < partition.dynamic_space_end() {
        let block = sweep_line as *const Tag;
        let size = block_size(block);
        let tag = *block;
        if tag != TAG_ONE_WORD_FILLER && tag != TAG_FREE_SPACE {
            assert!(is_marked(tag));
            occupied_space += size;
        }
        sweep_line += size;
        assert!(sweep_line <= partition.dynamic_space_end());
    }
    occupied_space
}

fn create_test_heap() -> PartitionedTestHeap {
    println!("    Create test heap...");
    let mut heap = PartitionedTestHeap::new(HEAP_SIZE);
    allocate_objects(&mut heap);
    let heap_size = heap.inner.occupied_size().as_usize();
    assert_eq!(heap_size, heap.heap_pointer as usize);
    assert_eq!(
        heap_size,
        heap.heap_base as usize + NUMBER_OF_OBJECTS * OBJECT_SIZE
    );
    heap
}

fn allocate_objects(heap: &mut PartitionedTestHeap) {
    for index in 0..NUMBER_OF_OBJECTS {
        progress(index + 1, NUMBER_OF_OBJECTS);
        let value = Value::from_scalar(index as u32);
        heap.allocate_array(&[value]);
    }
    reset_progress();
}

fn progress(count: usize, max: usize) {
    if count % (max / 100) == 0 || count == max {
        let percentage = count * 100 / max;
        print!("{percentage}/100\r");
        Write::flush(&mut stdout()).unwrap();
    }
}

fn reset_progress() {
    print!("       \r");
    Write::flush(&mut stdout()).unwrap();
}

pub struct PartitionedTestHeap {
    memory: Box<[u8]>,
    heap_base: u32,
    heap_pointer: u32,
    inner: PartitionedHeap,
}

impl PartitionedTestHeap {
    pub fn new(size: usize) -> PartitionedTestHeap {
        let memory = vec![0u8; size].into_boxed_slice();
        let heap_base = memory.as_ptr() as u32;
        PartitionedTestHeap {
            memory,
            heap_base,
            heap_pointer: heap_base,
            inner: PartitionedHeap::new(heap_base as usize),
        }
    }

    pub fn allocate_array(&mut self, elements: &[Value]) -> Value {
        unsafe {
            let array = alloc_array(self, elements.len() as u32);
            for index in 0..elements.len() {
                let raw_array = array.as_array();
                raw_array.set_scalar(index as u32, elements[index]);
            }
            array
        }
    }

    pub fn allocate_blob(&mut self, size: usize) -> Value {
        unsafe { alloc_blob(self, Bytes(size as u32)) }
    }
}

unsafe fn block_size(block: *const Tag) -> usize {
    let tag = unmark(*block);
    match tag {
        TAG_ARRAY => {
            size_of::<Array>() + (block as *const Array).len() as usize * WORD_SIZE as usize
        }
        TAG_BLOB => size_of::<Blob>() + (block as *const Blob).len().as_usize(),
        TAG_FREE_SPACE => (*(block as *const FreeSpace)).words.to_bytes().as_usize(),
        TAG_ONE_WORD_FILLER => WORD_SIZE,
        _ => unimplemented!(),
    }
}

impl Memory for PartitionedTestHeap {
    unsafe fn alloc_words(&mut self, size: Words<u32>) -> Value {
        self.inner
            .prepare_allocation_partition(&mut self.heap_pointer, size.to_bytes());
        assert!(self.heap_pointer >= self.heap_base);
        assert!(
            self.heap_pointer + size.to_bytes().as_u32()
                <= self.heap_base + self.memory.len() as u32
        );
        let result = Value::from_ptr(self.heap_pointer as usize);
        self.heap_pointer += size.to_bytes().as_u32();
        result
    }
}