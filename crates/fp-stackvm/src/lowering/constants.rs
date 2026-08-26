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

/// Return a new array/list with `value` written at `index`.
pub(crate) const INTRINSIC_ARRAY_SET: &str = "__bc_array_set";

/// Return the length of a container.
/// Takes `(handle: u64)` → `u64`.
pub(crate) const INTRINSIC_CONTAINER_LEN: &str = "__bc_container_len";

/// Return a managed slice of a string or sequence.
pub(crate) const INTRINSIC_SLICE: &str = "__bc_slice";

/// Materialise a UTF-8 string from byte arguments.
pub(crate) const INTRINSIC_STR_CONST: &str = "__bc_str_const";

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
        IntrinsicKind::Slice => INTRINSIC_SLICE,
        IntrinsicKind::TimeNow => "__bc_time_now",
        IntrinsicKind::Panic => "__bc_panic",
        IntrinsicKind::JsonParse => "__bc_json_parse",
        IntrinsicKind::CatchUnwind => "__bc_catch_unwind",
        IntrinsicKind::FsExists => "__bc_fs_exists",
        IntrinsicKind::FsIsFile => "__bc_fs_is_file",
        IntrinsicKind::FsIsDir => "__bc_fs_is_dir",
        IntrinsicKind::FsReadToString => "__bc_fs_read_to_string",
        IntrinsicKind::FsWriteString => "__bc_fs_write_string",
        IntrinsicKind::FsAppendString => "__bc_fs_append_string",
        IntrinsicKind::FsCreateDirAll => "__bc_fs_create_dir_all",
        IntrinsicKind::FsRemoveFile => "__bc_fs_remove_file",
        IntrinsicKind::FsRemoveDirAll => "__bc_fs_remove_dir_all",
        IntrinsicKind::FsReadDir => "__bc_fs_read_dir",
        IntrinsicKind::FsWalkDir => "__bc_fs_walk_dir",
        IntrinsicKind::FsGlob => "__bc_fs_glob",
        IntrinsicKind::PathJoin => "__bc_path_join",
        IntrinsicKind::PathParent => "__bc_path_parent",
        IntrinsicKind::PathFileName => "__bc_path_file_name",
        IntrinsicKind::PathExtension => "__bc_path_extension",
        IntrinsicKind::PathStem => "__bc_path_stem",
        IntrinsicKind::PathNormalize => "__bc_path_normalize",
        IntrinsicKind::PathIsAbsolute => "__bc_path_is_absolute",
        IntrinsicKind::EnvCurrentDir => "__bc_env_current_dir",
        IntrinsicKind::EnvTempDir => "__bc_env_temp_dir",
        IntrinsicKind::EnvHomeDir => "__bc_env_home_dir",
        IntrinsicKind::EnvVar => "__bc_env_var",
        IntrinsicKind::EnvVarExists => "__bc_env_var_exists",
        IntrinsicKind::IoReadStdinToString => "__bc_io_read_stdin_to_string",
        IntrinsicKind::IoWriteStdout => "__bc_io_write_stdout",
        IntrinsicKind::IoWriteStderr => "__bc_io_write_stderr",
        IntrinsicKind::DebugAssertions => "__bc_debug_assertions",
        IntrinsicKind::Input => "__bc_input",
        IntrinsicKind::TypeName => "__bc_type_name",
        IntrinsicKind::TypeOf => "__bc_type_of",
        IntrinsicKind::Sleep => "__bc_sleep",
        IntrinsicKind::Spawn => "__bc_spawn",
        IntrinsicKind::Join => "__bc_join",
        IntrinsicKind::Select => "__bc_select",
        IntrinsicKind::Yield => "__bc_yield",
        IntrinsicKind::SizeOf => "__bc_size_of",
        IntrinsicKind::FieldCount => "__bc_field_count",
        IntrinsicKind::FieldNameAt => "__bc_field_name_at",
        IntrinsicKind::HasField => "__bc_has_field",
        IntrinsicKind::HasMethod => "__bc_has_method",
        IntrinsicKind::MethodCount => "__bc_method_count",
        IntrinsicKind::FieldType => "__bc_field_type",
        IntrinsicKind::StructSize => "__bc_struct_size",
        IntrinsicKind::ReflectFields => "__bc_reflect_fields",
        IntrinsicKind::CreateStruct => "__bc_create_struct",
        IntrinsicKind::YamlToJson => "__bc_yaml_to_json",
        IntrinsicKind::ShellExec => "__bc_shell_exec",
        IntrinsicKind::ProcMacroTokenStreamFromStr => "__bc_proc_macro_token_stream_from_str",
        _ => "__bc_unknown",
    }
}
