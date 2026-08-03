//! Runtime intrinsic symbols emitted by the lowering pass.
//!
//! These are declared as external `Call` targets in the generated LIR.
//! `fp-interpret` (or another LIR consumer) must provide implementations.

/// Allocate a tuple with `n` elements on the managed heap.
/// Takes `(count: u64, elem_0: u64, ..., elem_n-1: u64)` and returns a handle.
pub(crate) const INTRINSIC_MAKE_TUPLE: &str = "__bc_make_tuple";

/// Allocate a fixed-size array.
pub(crate) const INTRINSIC_MAKE_ARRAY: &str = "__bc_make_array";

/// Allocate a growable list.
pub(crate) const INTRINSIC_MAKE_LIST: &str = "__bc_make_list";

/// Allocate a key-value map.
/// Takes `(count: u64, k0, v0, k1, v1, ...)` and returns a handle.
pub(crate) const INTRINSIC_MAKE_MAP: &str = "__bc_make_map";

/// Read the element at `index` from a tuple.
/// Takes `(handle: u64, index: u64)` → `u64`.
pub(crate) const INTRINSIC_TUPLE_GET: &str = "__bc_tuple_get";

/// Return a new tuple with `value` written at `index`.
/// Takes `(handle: u64, index: u64, value: u64)` → new handle.
pub(crate) const INTRINSIC_TUPLE_SET: &str = "__bc_tuple_set";

/// Read an element from an array-like container by index.
/// Takes `(handle: u64, index: u64)` → `u64`.
pub(crate) const INTRINSIC_ARRAY_GET: &str = "__bc_array_get";

/// Return the length of a container.
/// Takes `(handle: u64)` → `u64`.
pub(crate) const INTRINSIC_CONTAINER_LEN: &str = "__bc_container_len";

/// Allocate a string of the given length on the managed heap.
/// Takes `(len: u64)` and returns a handle.  The caller is expected to
/// write the string bytes into the returned buffer.
pub(crate) const INTRINSIC_STR_ALLOC: &str = "__bc_str_alloc";

/// Map a bytecode [`IntrinsicKind`] to its runtime symbol name.
///
/// Returns `"__bc_unknown"` for kinds not yet wired up.
pub(crate) fn intrinsic_to_runtime_name(kind: fp_bytecode::IntrinsicKind) -> &'static str {
    use fp_bytecode::IntrinsicKind;
    match kind {
        IntrinsicKind::Println => "__bc_println",
        IntrinsicKind::Print => "__bc_print",
        IntrinsicKind::Format => "__bc_format",
        IntrinsicKind::Len => INTRINSIC_CONTAINER_LEN,
        IntrinsicKind::TimeNow => "__bc_time_now",
        _ => "__bc_unknown",
    }
}
