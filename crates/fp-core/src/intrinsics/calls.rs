/// Symbolic identifier for intrinsic calls recognised by the front-end.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum IntrinsicCallKind {
    /// `std::io::println` variants (newline appended).
    Println,
    /// `std::io::print` variants (no trailing newline).
    Print,
    /// Build a formatted string at runtime.
    Format,
    /// Length query for collections/strings.
    Len,
    /// Build a slice from a base, start, and end (exclusive) index.
    Slice,
    /// Debug assertions state (mapped from build profile).
    DebugAssertions,
    /// Input prompt/read intrinsic.
    Input,
    /// Abort execution with a panic payload.
    Panic,
    /// Execute a callable and catch panic flows, returning success state.
    CatchUnwind,
    /// Get the current time as seconds since the Unix epoch.
    TimeNow,

    // Metaprogramming intrinsics
    /// `sizeof!` - get size of a type in bytes
    SizeOf,
    /// `reflect_fields!` - get field information of a struct
    ReflectFields,
    /// `hasmethod!` - check if a type has a specific method
    HasMethod,
    /// `type_name!` - get the name of a type as a string
    TypeName,
    /// `type_of!` - get the type of an expression as a type value
    TypeOf,
    /// `create_struct!` - create a new struct type dynamically
    CreateStruct,
    /// `clone_struct!` - clone an existing struct type
    CloneStruct,
    /// `addfield!` - add a field to a struct type
    AddField,
    /// `hasfield!` - check if a struct has a specific field
    HasField,
    /// `field_count!` - get the number of fields in a struct
    FieldCount,
    /// `method_count!` - get the number of methods in a struct
    MethodCount,
    /// `field_type!` - get the type of a specific field
    FieldType,
    /// `struct_size!` - get the size of a struct in bytes
    StructSize,
    /// `generate_method!` - generate a method for a type
    GenerateMethod,
    /// `compile_error!` - generate a compile-time error
    CompileError,
    /// `compile_warning!` - generate a compile-time warning
    CompileWarning,

    // Proc-macro helpers
    /// Parse a string into a TokenStream.
    ProcMacroTokenStreamFromStr,
    /// Render a TokenStream into a string.
    ProcMacroTokenStreamToString,
}
