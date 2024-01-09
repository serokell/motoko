//! Graph-copy-based stabilzation on upgrades, serializing the entire stable object graph into
//! stable memory by using a defined long-term stable storage format.
//!
//! Use cases:
//! - Classical model of volatile main memory: Preserve the stable variables and their reachable
//!   objects across upgrades.
//! - New model of enhanced orthogonal persistence: Support upgrades in the presence of complex
//!   main memory layout changes.
//!
//! A memory compatibility check has to be performed before allowing upgrade: This checks whether
//! the stored stable object graph is compatible with the new program version. For this purpose, the
//! type tables are compared, similar to the IDL-subtype check but customized to the allowed implicit
//! conversion with regard to the stable storage format.
//!
//! Versioned stable storage format to permit future evolutions of the format.
//!  
//! See `GraphCopyStabilization.md` for the stable format specification and the employed algorithm.

use motoko_rts_macros::ic_mem_fn;

use core::cmp::min;

use crate::{
    constants::WASM_PAGE_SIZE,
    memory::Memory,
    rts_trap_with,
    stabilization::{
        layout::{deserialize, serialize},
        scan_stack::STACK_EMPTY,
    },
    stable_mem::{self, ic0_stable64_write, PAGE_SIZE},
    types::{block_size, FwdPtr, Tag, Value, TAG_CLOSURE, TAG_FWD_PTR},
    visitor::visit_pointer_fields,
};

use self::{
    layout::{scan_serialized, StableToSpace, StableValue, STABLE_NULL_POINTER},
    scan_stack::ScanStack,
    stable_memory_access::StableMemoryAccess,
    stable_memory_stream::{ScanStream, StableMemoryStream},
    time::BoundedTime,
};

#[cfg(feature = "ic")]
mod compatibility;
#[cfg(feature = "ic")]
mod metadata;

mod layout;

mod scan_stack;

pub mod stable_memory_access;
pub mod stable_memory_stream;
mod time;

extern "C" {
    pub fn moc_null_singleton() -> Value;
}

/// Generic graph copy from main memory (from-space) to stable memory (to-space).
/// The direction of copying is fixed but the memory layout used in the from-space
/// and the to-space flips when switching between serialization and deserialization.
/// `S`: Source address type (from-space, main memory).
/// `T`: Target address type (to-space, stable memory).
/// `P`: Pointer encoding type (e.g. `u32` or `u64`).
/// During serialization:
/// * Main memory = main memory layout, S = Value.
/// * Stable memory = stable memory layout, T = StableMemoryAddress.
/// During derialization:
/// * Main memory = stable memory layout, S = StableMemoryAddress.
/// * Stable memory = main memory layout, T = Value.
pub trait GraphCopy<S: Copy, T: Copy, P: Copy + Default> {
    /// Start the entire graph copy algorithm: Copy the object graph reachable from the `root` pointer.
    /// Use this as follows:
    /// ```
    /// copy_algorithm.start();
    /// while !copy_algorithm.is_completed() {
    ///     copy_algorthm.copy_increment();
    /// }
    /// ```
    fn start(&mut self, root: S) {
        self.evacuate(root);
    }

    /// Determin whether the copy algorithm is completed,
    /// i.e. a sufficient amount of copy increments has been invoked.
    fn is_completed(&self) -> bool;

    /// Copy reachable objects in a time-bounded work step. with a synthetic time bound.
    /// This allows to spread the incremtnal graph copy where the work is
    /// split in multiple increments over multiple IC messages.
    fn copy_increment(&mut self) {
        self.reset_time();
        while !self.is_completed() && !self.time_over() {
            self.scan();
        }
    }

    /// Reset the time at the beginning of a new copy increment.
    fn reset_time(&mut self);

    /// Determine whether the time of copy increment has been exceeded.
    fn time_over(&self) -> bool;

    /// Lazy evacuation of a single object.
    /// Triggered for each pointer that is patched in the `scan()` function.
    /// Determines whether the object has already been copied before, and if not,
    /// copies it to the target space.
    /// Returns the new target address of the object.
    fn evacuate(&mut self, object: S) -> T {
        match self.get_forward_address(object) {
            Some(target) => target,
            None => {
                let target = self.copy(object);
                self.set_forward_address(object, target);
                target
            }
        }
    }

    /// Check if the object has been forwarded.
    /// Returns `None` if not forwarded, or otherwise, the new target address.
    fn get_forward_address(&self, object: S) -> Option<T>;

    /// Mark the object as forwarded and record its new target address.
    fn set_forward_address(&mut self, object: S, target: T);

    /// Allocate the object in the to-space by bumping the `free` pointer.
    /// Copy its content to that target location using the encoding of the target layout.
    /// Notes:
    /// * The pointer values in the field are retained as source addresses.
    /// * The source and target layout must use the same size for addresses, e.g. 32-bit.
    /// * The allocator must be contiguously growing. Free space must be inserted when the
    ///   allocator uses internal fragmentation, e.g. for the partitioned heap.
    fn copy(&mut self, object: S) -> T;

    /// Read an object at the `scan` position in the to-space, and patch all the pointer fields
    /// by translating the source pointer to the corresponding new target pointer by calling
    /// `evacuate()`.
    fn scan(&mut self);
}

// Dummy value used for non-stable objects that are potentially reachable from
// stable variable because of structural subtyping or `Any`-subtyping.
// Must be a non-skewed value such that the GC also ignores this value.
const DUMMY_VALUE: StableValue = StableValue::from_raw(0);

const COPY_TIME_LIMIT: usize = 10_000;

pub struct Serialization {
    to_space: StableMemoryStream,
    time: BoundedTime,
}

/// Graph-copy-based serialization.
/// Notes:
/// - Invalidates the heap by replacing reachable stable object by forwarding objects:
/// The heap is finally no longer usable by mutator or GC.
/// - `copy` and partially also `scan` depends on the heap layout. Adjust these functions
/// whenever the heap layout is changed.
/// Usage:
/// ```
/// let serialization = Serialization::start(root, stable_start);
/// while !serialization.is_completed() {
///     serialization.copy_increment();
/// }
/// ```
impl Serialization {
    /// Start the graph-copy-based heap serialization from the stable `root` object
    /// by writing the serialized data to the stable memory at offset `stable_start`.
    /// The start is followed by a series of copy increments before the serialization is completed.
    pub fn start(root: Value, stable_start: u64) -> Serialization {
        let to_space = StableMemoryStream::open(stable_start);
        let time = BoundedTime::new(COPY_TIME_LIMIT);
        let mut serialization = Serialization { time, to_space };
        serialization.start(root);
        serialization
    }

    /// Complete the serialization. Returns the byte size of the serialized data in stable memory.
    pub fn complete(&mut self) -> u64 {
        self.to_space.close();
        self.to_space.written_length()
    }

    fn is_null(field_value: Value) -> bool {
        unsafe {
            debug_assert!(!moc_null_singleton().is_forwarded());
        }
        field_value == unsafe { moc_null_singleton() }
    }

    fn encode_null() -> StableValue {
        STABLE_NULL_POINTER
    }

    /// Resolve the Brooks forwarding pointer of the incremental GC by considering potential
    /// forwarding objects (`FwdPtr`) used in Cheney's algorithm for stabilization.
    unsafe fn resolve_gc_forwarding(object: Value) -> Value {
        let tag = Self::read_object_tag(object);
        if tag == TAG_FWD_PTR {
            object
        } else {
            object.forward()
        }
    }
    /// Read the object tag by considering potential forwarding objects (`FwdPtr`).
    unsafe fn read_object_tag(object: Value) -> Tag {
        // Do not call `tag()` as it dereferences the Brooks forwarding pointer of the incremental GC,
        // which does not exist for the forwarding objects (`FwdPtr`) used by the Cheney's algorithm.
        *(object.get_ptr() as *const Tag)
    }

    fn has_non_stable_type(old_field: Value) -> bool {
        unsafe { old_field.tag() == TAG_CLOSURE }
    }
}

impl GraphCopy<Value, StableValue, u32> for Serialization {
    fn get_forward_address(&self, object: Value) -> Option<StableValue> {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            let tag = Self::read_object_tag(object);
            match tag {
                TAG_FWD_PTR => {
                    let new_location = (*(object.get_ptr() as *mut FwdPtr)).fwd;
                    Some(StableValue::serialize(new_location))
                }
                _ => None,
            }
        }
    }

    fn set_forward_address(&mut self, object: Value, target: StableValue) {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            debug_assert!(object.is_obj());
            let fwd = object.get_ptr() as *mut FwdPtr;
            (*fwd).tag = TAG_FWD_PTR;
            (*fwd).fwd = target.deserialize();
        }
    }

    fn copy(&mut self, object: Value) -> StableValue {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            debug_assert!(object.is_obj());
            let address = self.to_space.written_length();
            serialize(&mut self.to_space, object);
            debug_assert!(self.to_space.written_length() >= address);
            let size = self.to_space.written_length() - address;
            debug_assert!(size <= usize::MAX as u64);
            self.time.advance(size as usize);
            StableValue::from_stable_address(address)
        }
    }

    fn scan(&mut self) {
        scan_serialized(self, &|context, original| {
            context.time.tick();
            let old_value = original.deserialize();
            if old_value.is_ptr() {
                if Self::is_null(old_value) {
                    Self::encode_null()
                } else if Self::has_non_stable_type(old_value) {
                    // Due to structural subtyping or `Any`-subtyping, a non-stable object (such as a closure) may be
                    // be dynamically reachable from a stable varibale. The value is not accessible in the new program version.
                    // Therefore, the content of these fields can serialized with a dummy value that is also ignored by the GC.
                    DUMMY_VALUE
                } else {
                    context.evacuate(old_value)
                }
            } else {
                original
            }
        });
    }

    fn is_completed(&self) -> bool {
        self.to_space.scan_completed()
    }

    fn time_over(&self) -> bool {
        self.time.is_over()
    }

    fn reset_time(&mut self) {
        self.time.reset();
    }
}

impl StableToSpace for Serialization {
    fn to_space(&mut self) -> &mut StableMemoryStream {
        &mut self.to_space
    }
}

pub struct Deserialization<'a, M: Memory + 'a> {
    mem: &'a mut M,
    from_space: StableMemoryAccess,
    scan_stack: ScanStack,
    stable_start: u64,
    stable_size: u64,
    stable_root: Option<Value>,
    time: BoundedTime,
}

/// Graph-copy-based deserialization.
/// Usage:
/// ```
/// let deserialization = Deserialization::start(mem, stable_start, stable_size);
/// while !deserialization.is_completed() {
///     deserialization.copy_increment();
/// }
/// ```
impl<'a, M: Memory + 'a> Deserialization<'a, M> {
    /// Start the deserialization, followed by a series of copy increments.
    pub fn start(mem: &'a mut M, stable_start: u64, stable_size: u64) -> Deserialization<'a, M> {
        let from_space = StableMemoryAccess::open(stable_start, stable_size);
        let scan_stack = unsafe { ScanStack::new(mem) };
        let time = BoundedTime::new(COPY_TIME_LIMIT);
        let mut deserialization = Deserialization {
            mem,
            from_space,
            scan_stack,
            stable_start,
            stable_size,
            stable_root: None,
            time,
        };
        deserialization.start(StableValue::serialize(Value::from_ptr(0)));
        deserialization
    }

    pub fn complete(&mut self) -> Value {
        clear_stable_memory(self.stable_start, self.stable_size);
        self.stable_root.unwrap()
    }

    fn is_null(value: StableValue) -> bool {
        value == STABLE_NULL_POINTER
    }

    fn encode_null() -> Value {
        unsafe { moc_null_singleton() }
    }

    unsafe fn scan_deserialized<C, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
        target_object: Value,
        translate: &F,
    ) {
        debug_assert!(target_object.is_obj());
        visit_pointer_fields(
            context,
            target_object.as_obj(),
            target_object.tag(),
            0,
            |context, field_address| {
                *field_address = translate(context, *field_address);
            },
            |_, _, array| array.len(),
        );
    }
}

impl<'a, M: Memory + 'a> GraphCopy<StableValue, Value, u32> for Deserialization<'a, M> {
    fn get_forward_address(&self, stable_object: StableValue) -> Option<Value> {
        let address = stable_object.to_stable_address();
        let tag = self.from_space.read::<Tag>(address);
        match tag {
            TAG_FWD_PTR => {
                let forward_object = self.from_space.read::<FwdPtr>(address);
                Some(forward_object.fwd)
            }
            _ => None,
        }
    }

    fn set_forward_address(&mut self, stable_object: StableValue, target: Value) {
        let address = stable_object.to_stable_address();
        let forward_object = FwdPtr {
            tag: TAG_FWD_PTR,
            fwd: target,
        };
        self.from_space.write(address, &forward_object);
    }

    fn copy(&mut self, stable_object: StableValue) -> Value {
        unsafe {
            let target = deserialize(self.mem, &mut self.from_space, stable_object);
            if self.stable_root.is_none() {
                self.stable_root = Some(target);
            }
            self.scan_stack.push(self.mem, target);
            let size = block_size(target.get_ptr() as usize).to_bytes().as_usize();
            self.time.advance(size);
            target
        }
    }

    /// Note:
    /// * The deserialized memory may contain free space at a partition end.
    fn scan(&mut self) {
        let target_object = unsafe { self.scan_stack.pop() };
        debug_assert!(target_object != STACK_EMPTY);
        unsafe {
            Self::scan_deserialized(self, target_object, &|context, original| {
                context.time.tick();
                let old_value = StableValue::serialize(original);
                if Self::is_null(old_value) {
                    Self::encode_null()
                } else if original.is_ptr() {
                    context.evacuate(old_value)
                } else {
                    original
                }
            });
        }
    }

    fn is_completed(&self) -> bool {
        unsafe { self.scan_stack.is_empty() }
    }

    fn time_over(&self) -> bool {
        self.time.is_over()
    }

    fn reset_time(&mut self) {
        self.time.reset();
    }
}

fn clear_stable_memory(start: u64, length: u64) {
    const CHUNK_SIZE: usize = WASM_PAGE_SIZE.as_usize();
    let empty_chunk = [0u8; CHUNK_SIZE];
    let mut position = start;
    let end = start + length;
    while position < end {
        let size = min(end - position, CHUNK_SIZE as u64);
        unsafe {
            ic0_stable64_write(position, &empty_chunk as *const u8 as u64, size);
        }
        position += size;
    }
}

fn grant_stable_space(byte_size: u64) {
    debug_assert!(byte_size < u64::MAX - PAGE_SIZE - 1);
    let required_pages = (byte_size + PAGE_SIZE - 1) / PAGE_SIZE;
    let available_pages = stable_mem::size();
    if required_pages > available_pages {
        let additional_pages = required_pages - available_pages;
        debug_assert_ne!(additional_pages, u64::MAX);
        let result = stable_mem::grow(additional_pages);
        if result == u64::MAX {
            unsafe {
                rts_trap_with("Insufficient stable memory");
            }
        }
    }
}

#[cfg(feature = "ic")]
extern "C" {
    fn ic0_performance_counter(number: u32) -> u64;
    fn set_upgrade_instructions(instructions: u64);
}

/// Pre-upgrade operation for graph-copy-based program upgrades:
/// All objects inside main memory that are transitively reachable from stable variables are
/// serialized into stable memory by using a graph copy algorithm.
/// `stable_actor`: Root object for stabilization containing all stable variables of the actor.
/// The remaining parameters encode the type table of the current program version:
/// `old_candid_data`: A blob encoding the Candid type as a table.
/// `old_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Implementation:
/// * Algorithm: Cheney's algorithm using main memory as from-space and stable memory as to-space.
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in
///   the stable object graph layout (see `GraphCopyStabilization.md`).
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn stabilize(stable_actor: Value, old_candid_data: Value, old_type_offsets: Value) {
    use crate::stabilization::metadata::StabilizationMetadata;
    use compatibility::TypeDescriptor;

    let stable_memory_pages = stable_mem::size();
    let serialized_data_start = stable_memory_pages * PAGE_SIZE;

    let mut serialization = Serialization::start(stable_actor, serialized_data_start);
    while !serialization.is_completed() {
        serialization.copy_increment();
    }
    let serialized_data_length = serialization.complete();

    let type_descriptor = TypeDescriptor::new(old_candid_data, old_type_offsets, 0);
    let metadata = StabilizationMetadata {
        stable_memory_pages,
        serialized_data_start,
        serialized_data_length,
        type_descriptor,
    };
    metadata.store();
}

/// Post-upgrade operation for graph-copy-based program upgrades:
/// Deserialize the object graph stored in stable memory back into main memory by using a graph
/// copy algorithm. Checks whether the new program version is compatible to the stored state by
/// comparing the type tables of both the old and the new program version.
/// The parameters encode the type table of the new program version to which that data is to be upgraded.
/// `new_candid_data`: A blob encoding the Candid type as a table.
/// `new_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Returns the root object containing all restored stable variables of the actor.
/// Traps if the stable state is incompatible with the new program version and the upgrade is not
/// possible.
/// Implementation:
/// * Algorithm: Cheney's algorithm using stable memory as from-space and main memory as to-space.
/// * Encoding: The from-space uses the stable memory layout, while the to-space is to be encoded in
///   main memory layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn destabilize<M: Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) -> Value {
    use crate::{rts_trap_with, stable_mem::moc_stable_mem_set_size};
    use compatibility::{memory_compatible, TypeDescriptor};
    use metadata::StabilizationMetadata;

    let mut new_type_descriptor = TypeDescriptor::new(new_candid_data, new_type_offsets, 0);
    let (metadata, statistics) = StabilizationMetadata::load(mem);
    let mut old_type_descriptor = metadata.type_descriptor;
    if !memory_compatible(mem, &mut old_type_descriptor, &mut new_type_descriptor) {
        rts_trap_with("Memory-incompatible program upgrade");
    }

    let mut deserialization = Deserialization::start(
        mem,
        metadata.serialized_data_start,
        metadata.serialized_data_length,
    );
    while !deserialization.is_completed() {
        deserialization.copy_increment();
    }
    let stable_root = deserialization.complete();

    moc_stable_mem_set_size(metadata.stable_memory_pages);
    set_upgrade_instructions(statistics.stabilization_instructions + ic0_performance_counter(0));
    stable_root
}

#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn use_new_destabilization() -> bool {
    metadata::StabilizationMetadata::matching_version()
}
