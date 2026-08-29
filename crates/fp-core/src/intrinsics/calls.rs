use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Arc;

/// How a portable op's result type relates to its call arguments — the
/// data-driven replacement for what used to be a hand-grouped `match` over
/// `OpKind` in `fp-typing::hir_typeck::check_high_level_op`. Adding a new
/// portable op only ever means adding one `PortableOpDef` (see
/// `PortableOpRegistry::builtin`) with the right rule here — no match arms
/// to touch in `fp-typing`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ResultTypeRule {
    /// Result type is exactly argument `N`'s type (e.g. `x.as_ref()` drops
    /// to `x`'s own type; `x.unwrap_or(default)` unifies with `default`'s).
    SameAsArg(usize),
    /// Result is always `bool` (e.g. `x.is_none()`).
    AlwaysBool,
    /// Result is always the target language's native string type (e.g.
    /// `x.to_string()`, `String::from_utf8(..)`).
    TargetNativeString,
    /// Result wraps argument `N` in the source language's `Result` success
    /// constructor. Targets that support result values must preserve that
    /// wrapper instead of erasing it into the successful value.
    ResultSuccess(usize),
    /// Result wraps argument `N` in the source language's `Result` failure
    /// constructor. This is not `Never`: constructing an error result is an
    /// ordinary expression and may be returned, bound, or matched.
    ResultFailure(usize),
    /// Result never produces a value normally (e.g. `Err(e)` → `error(e)`,
    /// unifies with whatever the caller expects, like `panic!`).
    Never,
    /// The real result type depends on a stdlib generic parameter this call
    /// site can't recover (the original callee path/DefId was discarded by
    /// portable-op recognition) — fails loudly as a "missing feature"
    /// rather than fabricating a type.
    NotStaticallyKnowable,
}

/// Call-shape expectations for a portable op — mirrors what
/// `try_promote_op`'s callargs construction already assumes implicitly per
/// call-site syntax (`Path`/`Struct`/`Call`/`MethodCall`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ArityShape {
    /// Whether argument 0 is a receiver (i.e. this op is method-call sugar,
    /// `x.op(..)`, not a free function/constructor call).
    pub receiver: bool,
    /// Minimum number of arguments (including the receiver, if any).
    pub min_args: usize,
}

/// A portable op's full definition, as stored in a `PortableOpRegistry`.
#[derive(Debug, Clone)]
pub struct PortableOpDef {
    pub name: Arc<str>,
    pub arity: ArityShape,
    pub result_rule: ResultTypeRule,
}

/// A resolved portable-op identity, carried on
/// `CallKind::Op`/`hir::HirPackage::op_defs`/etc. Deliberately NOT a bare
/// string: the only way to construct one is `PortableOpRegistry::resolve`,
/// which looks the name up against the central registry and hands back the
/// full definition — so every `PortableOp` in flight already carries its
/// `arity`/`result_rule`, with no separate "look the name up later and hope
/// someone remembered to check" step for consumers to skip or forget.
///
/// Derives structural `PartialEq`/`Eq`/`Hash` over all three fields (not
/// just `name`) so `CallKind` (which embeds this) can itself derive them —
/// needed for `CallKind`'s many pattern-position `const` shortcuts
/// (`matches!(call.kind, CallKind::Println)`) across target-language
/// backends, which require Rust's structural-match marker traits. This is
/// equivalent in practice to name-only identity: `arity`/`result_rule` are
/// always the same for a given `name` (both are resolved from the same
/// registry entry — see `PortableOpRegistry::resolve`), so two
/// same-named `PortableOp`s never actually differ in their other fields.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PortableOp {
    pub name: Arc<str>,
    pub arity: ArityShape,
    pub result_rule: ResultTypeRule,
}

// `Arc<str>` has no `serde` impl (no `rc` feature enabled workspace-wide);
// (de)serialize `name` via a plain `String` instead.
impl serde::Serialize for PortableOp {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(serde::Serialize)]
        struct Repr<'a> {
            name: &'a str,
            arity: ArityShape,
            result_rule: ResultTypeRule,
        }
        Repr {
            name: &self.name,
            arity: self.arity,
            result_rule: self.result_rule,
        }
        .serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for PortableOp {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        struct Repr {
            name: String,
            arity: ArityShape,
            result_rule: ResultTypeRule,
        }
        let repr = Repr::deserialize(deserializer)?;
        Ok(PortableOp {
            name: Arc::from(repr.name),
            arity: repr.arity,
            result_rule: repr.result_rule,
        })
    }
}

impl PortableOp {
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// The central, language-agnostic portable-op registry: given a canonical
/// name, hands back the op's full definition. Every source/target
/// language's own `#[op(...)]`/`@Op(...)` tag is expected to spell its name
/// identically to an entry here (no fuzzy/synonym matching) — a mismatch is
/// a straightforward lookup miss, surfaced by the caller as a "missing
/// feature"/"unknown portable op" diagnostic, never silently ignored.
#[derive(Debug, Clone, Default)]
pub struct PortableOpRegistry {
    defs: HashMap<Arc<str>, PortableOpDef>,
}

impl PortableOpRegistry {
    pub fn from_defs(defs: impl IntoIterator<Item = PortableOpDef>) -> Self {
        Self {
            defs: defs.into_iter().map(|d| (d.name.clone(), d)).collect(),
        }
    }

    pub fn resolve(&self, name: &str) -> Option<PortableOp> {
        self.defs.get(name).map(|def| PortableOp {
            name: def.name.clone(),
            arity: def.arity,
            result_rule: def.result_rule,
        })
    }

    pub fn contains(&self, name: &str) -> bool {
        self.defs.contains_key(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &PortableOpDef> {
        self.defs.values()
    }

    /// The builtin, canonical registry — formalizes what used to be the
    /// closed `OpKind` enum's variant list as data instead of code. Names
    /// are unchanged from the old enum's `CallKind::name()` output (no
    /// renaming, to avoid conflating a naming migration with this
    /// representation rewrite).
    pub fn builtin() -> Self {
        Self::from_defs(builtin_portable_op_defs())
    }
}

fn def(name: &'static str, receiver: bool, min_args: usize, rule: ResultTypeRule) -> PortableOpDef {
    PortableOpDef {
        name: Arc::from(name),
        arity: ArityShape { receiver, min_args },
        result_rule: rule,
    }
}

fn builtin_portable_op_defs() -> Vec<PortableOpDef> {
    use ResultTypeRule::*;
    vec![
        def("option_some", false, 1, NotStaticallyKnowable),
        def("option_none", false, 0, NotStaticallyKnowable),
        def("option_unwrap", true, 1, NotStaticallyKnowable),
        // `Ok(x)` and `Err(e)` construct result values. A target decides how
        // to represent `Result`, but it must retain the success/failure
        // distinction through materialization and cannot erase errors.
        def("result_ok", false, 1, ResultSuccess(0)),
        def("result_err", false, 1, ResultFailure(0)),
        def("result_map", true, 2, NotStaticallyKnowable),
        def("result_map_err", true, 2, NotStaticallyKnowable),
        def("result_is_ok", true, 1, AlwaysBool),
        def("result_is_err", true, 1, AlwaysBool),
        def("result_ok_value", true, 1, NotStaticallyKnowable),
        def("result_err_value", true, 1, NotStaticallyKnowable),
        def("result_unwrap_or", true, 2, SameAsArg(1)),
        def("vec_new", false, 0, NotStaticallyKnowable),
        def("vec_push", true, 2, SameAsArg(0)),
        def("vec_extend", true, 2, SameAsArg(0)),
        def("vec_from_iter", false, 1, NotStaticallyKnowable),
        def("clone", true, 1, SameAsArg(0)),
        def("as_ref", true, 1, SameAsArg(0)),
        def("map_or", true, 3, SameAsArg(1)),
        def("iter", true, 1, SameAsArg(0)),
        def("collect", true, 1, NotStaticallyKnowable),
        def("filter", true, 2, NotStaticallyKnowable),
        def("find", true, 2, NotStaticallyKnowable),
        def("unwrap_or", true, 2, SameAsArg(1)),
        def("to_owned", true, 1, SameAsArg(0)),
        def("as_str", true, 1, SameAsArg(0)),
        def("to_string", true, 1, TargetNativeString),
        def("and_then", true, 2, NotStaticallyKnowable),
        def("trim_end", true, 1, SameAsArg(0)),
        def("trim_start", true, 1, SameAsArg(0)),
        def("trim", true, 1, SameAsArg(0)),
        def("split_whitespace", true, 1, NotStaticallyKnowable),
        def("split", true, 2, NotStaticallyKnowable),
        def("lines", true, 1, NotStaticallyKnowable),
        def("starts_with", true, 2, AlwaysBool),
        def("ends_with", true, 2, AlwaysBool),
        def("as_deref", true, 1, SameAsArg(0)),
        def("position", true, 2, NotStaticallyKnowable),
        def("is_none", true, 1, AlwaysBool),
        def("string_from_utf8_lossy", false, 1, TargetNativeString),
        def("string_from_utf8", false, 1, TargetNativeString),
    ]
}

/// Known type descriptors that serializers map to target-ecosystem equivalents.
/// Each variant represents a semantic type category that has a well-known
/// portable representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum KnownClass {
    /// A filesystem path (PathBuf, Path, OsStr, CStr)
    Path,
    /// A timestamp (Instant)
    Instant,
    /// A timespan (Duration)
    Duration,
    /// A local wall-clock datetime
    LocalDateTime,
    /// A UTC datetime
    UtcDateTime,
    /// A calendar date
    Date,
    /// An IPv4/v6 address
    IpAddr,
    /// A TCP stream socket
    TcpStream,
    /// A TCP listen socket
    TcpListener,
    /// A UDP datagram socket
    UdpSocket,
    /// A filesystem file handle
    FileHandle,
    /// A standard I/O stream
    IoStream,
    /// A child process handle
    ChildProcess,
    /// A process exit code (integer)
    ExitCode,
}

impl KnownClass {
    /// Resolve a source-language type name to its portable KnownClass.
    /// This is the bridge between source-specific type names (like PathBuf)
    /// and portable type descriptors. Implemented in fp-core because it
    /// encodes source-language knowledge.
    pub fn from_source_type(name: &str) -> Option<Self> {
        use KnownClass::*;
        match name {
            "PathBuf" | "Path" | "OsString" | "OsStr" | "CString" | "CStr" => Some(Path),
            "Instant" => Some(Instant),
            "Duration" => Some(Duration),
            "Local" => Some(LocalDateTime),
            "Utc" => Some(UtcDateTime),
            "NaiveDate" => Some(Date),
            "NaiveDateTime" => Some(LocalDateTime),
            "Ipv4Addr" | "Ipv6Addr" | "SocketAddr" => Some(IpAddr),
            "TcpStream" => Some(TcpStream),
            "TcpListener" => Some(TcpListener),
            "UdpSocket" => Some(UdpSocket),
            "File" => Some(FileHandle),
            "Stdin" | "Stdout" | "Stderr" => Some(IoStream),
            "Child" => Some(ChildProcess),
            "ExitStatus" | "Output" => Some(ExitCode),
            _ => None,
        }
    }
}

/// Commonly known middle-layer packages that serializers can map to
/// their target ecosystem. Only used in transpile mode (ops enabled).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum KnownPackage {
    /// std::collections → Kotlin/TS built-in collections
    StdCollections,
    /// std::path → java.nio.file / Node path
    StdPath,
    /// std::process → ProcessBuilder / child_process
    StdProcess,
    /// std::sync → Kotlin built-in (no Arc), TS has no equivalent
    StdSync,
    /// std::fs → java.io / Node fs
    StdFs,
    /// std::io → Kotlin I/O / Node streams
    StdIo,
    /// std::str → built-in String
    StdStr,
    /// std::option → built-in nullable
    StdOption,
    /// serde → kotlinx.serialization / skip
    Serde,
    /// winnow → skip (parser combinator lib)
    Winnow,
    /// thiserror → skip (derive macro crate, no runtime dependency)
    ThisError,
    /// tracing → skip (structured logging, handled by target-native logging)
    Tracing,
    /// async_trait → skip (syntax extension, normalised away)
    AsyncTrait,
    /// anyhow → skip (error-handling, normalised to exceptions)
    Anyhow,
    /// Crate with no safe target-language equivalent without a real runtime
    /// dependency (e.g. toml, serde_json, tokio) — calls into it should render as
    /// an explicit unsupported-call stub rather than a broken identifier reference.
    Unsupported,
    /// Local/unknown package — name is the portable path
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum IntrinsicKind {
    Println,
    Print,
    Format,
    Len,
    Slice,
    DebugAssertions,
    Input,
    Panic,
    CatchUnwind,
    CatchUnwindResult,
    TimeNow,
    FsReadToString,
    FsWriteString,
    FsAppendString,
    FsExists,
    FsIsDir,
    FsIsFile,
    FsReadDir,
    FsWalkDir,
    FsCreateDirAll,
    FsRemoveFile,
    FsRemoveDirAll,
    FsGlob,
    EnvCurrentDir,
    EnvTempDir,
    EnvHomeDir,
    EnvVar,
    EnvVarExists,
    PathJoin,
    PathParent,
    PathFileName,
    PathExtension,
    PathStem,
    PathIsAbsolute,
    PathNormalize,
    IoReadStdinToString,
    IoWriteStdout,
    IoWriteStderr,
    YamlToJson,
    JsonParse,
    SerdeJsonFromStr,
    SerdeJsonToString,
    TomlFromStr,
    TokioTcpConnect,
    TokioTcpWriteAll,
    TestCommandMockTakeCalls,
    TestCommandMockApply,
    Sleep,
    Spawn,
    Join,
    Select,
    Yield,
    SizeOf,
    ReflectFields,
    HasMethod,
    TypeName,
    TypeOf,
    CreateStruct,
    AddField,
    CloneStruct,
    BuildType,
    PrimitiveType,
    HasField,
    FieldCount,
    MethodCount,
    FieldType,
    VecType,
    FieldNameAt,
    StructSize,
    GenerateMethod,
    CompileError,
    CompileWarning,
    ProcMacroTokenStreamFromStr,
    ProcMacroTokenStreamToString,
    ShellExec,
    ShellFileCopy,
    ShellFileTemplate,
    ShellFileRsync,
    Unionify,
    HostPtrRead,
    HostPtrWrite,
    HostPtrOffset,
}

impl IntrinsicKind {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Println => "println",
            Self::Print => "print",
            Self::Format => "format",
            Self::Len => "len",
            Self::Slice => "slice",
            Self::DebugAssertions => "debug_assertions",
            Self::Input => "input",
            Self::Panic => "panic",
            Self::CatchUnwind => "catch_unwind",
            Self::CatchUnwindResult => "catch_unwind_result",
            Self::TimeNow => "time_now",
            Self::FsReadToString => "fs_read_to_string",
            Self::FsWriteString => "fs_write_string",
            Self::FsAppendString => "fs_append_string",
            Self::FsExists => "fs_exists",
            Self::FsIsDir => "fs_is_dir",
            Self::FsIsFile => "fs_is_file",
            Self::FsReadDir => "fs_read_dir",
            Self::FsWalkDir => "fs_walk_dir",
            Self::FsCreateDirAll => "fs_create_dir_all",
            Self::FsRemoveFile => "fs_remove_file",
            Self::FsRemoveDirAll => "fs_remove_dir_all",
            Self::FsGlob => "fs_glob",
            Self::EnvCurrentDir => "env_current_dir",
            Self::EnvTempDir => "env_temp_dir",
            Self::EnvHomeDir => "env_home_dir",
            Self::EnvVar => "env_var",
            Self::EnvVarExists => "env_var_exists",
            Self::PathJoin => "path_join",
            Self::PathParent => "path_parent",
            Self::PathFileName => "path_file_name",
            Self::PathExtension => "path_extension",
            Self::PathStem => "path_stem",
            Self::PathIsAbsolute => "path_is_absolute",
            Self::PathNormalize => "path_normalize",
            Self::IoReadStdinToString => "io_read_stdin_to_string",
            Self::IoWriteStdout => "io_write_stdout",
            Self::IoWriteStderr => "io_write_stderr",
            Self::YamlToJson => "yaml_to_json",
            Self::JsonParse => "json_parse",
            Self::TestCommandMockTakeCalls => "test_command_mock_take_calls",
            Self::TestCommandMockApply => "test_command_mock_apply",
            Self::Sleep => "sleep",
            Self::Spawn => "spawn",
            Self::Join => "join",
            Self::Select => "select",
            Self::Yield => "yield",
            Self::SizeOf => "size_of",
            Self::ReflectFields => "reflect_fields",
            Self::HasMethod => "has_method",
            Self::TypeName => "type_name",
            Self::TypeOf => "type_of",
            Self::CreateStruct => "create_struct",
            Self::AddField => "add_field",
            Self::CloneStruct => "clone_struct",
            Self::BuildType => "build_type",
            Self::PrimitiveType => "primitive_type",
            Self::HasField => "has_field",
            Self::FieldCount => "field_count",
            Self::MethodCount => "method_count",
            Self::FieldType => "field_type",
            Self::VecType => "vec_type",
            Self::FieldNameAt => "field_name_at",
            Self::StructSize => "struct_size",
            Self::GenerateMethod => "generate_method",
            Self::CompileError => "compile_error",
            Self::CompileWarning => "compile_warning",
            Self::ProcMacroTokenStreamFromStr => "token_stream_from_str",
            Self::ProcMacroTokenStreamToString => "token_stream_to_string",
            Self::ShellExec => "shell_exec",
            Self::ShellFileCopy => "shell_file_copy",
            Self::ShellFileTemplate => "shell_file_template",
            Self::ShellFileRsync => "shell_file_rsync",
            Self::Unionify => "unionify",
            Self::HostPtrRead => "host_ptr_read",
            Self::HostPtrWrite => "host_ptr_write",
            Self::HostPtrOffset => "host_ptr_offset",
        }
    }
}

/// Intrinsic calls carry the closed intrinsic identity directly.
pub type CallKind = IntrinsicKind;
