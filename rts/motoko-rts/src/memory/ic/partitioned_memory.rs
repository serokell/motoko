use super::{IcMemory, Memory};
use crate::types::*;

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    crate::gc::incremental::get_partitioned_heap().reclaimed_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    crate::gc::incremental::get_partitioned_heap().total_allocated_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    crate::gc::incremental::get_partitioned_heap().occupied_size()
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        crate::gc::incremental::get_partitioned_heap().allocate(self, n)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: u64) {
        super::grow_memory(ptr);
    }
}
