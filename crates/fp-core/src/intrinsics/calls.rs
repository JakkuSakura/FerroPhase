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
        // `Ok(x)` → `x` (Kotlin has no `Result<T, E>` with an arbitrary
        // error type; the function's own return type is unwrapped the same
        // way, `Result<T, E>` → `T` — see `kotlin_type_from_ty`).
        def("result_ok", false, 1, SameAsArg(0)),
        // `Err(e)` → `error(e)` — never produces a value, unifies with
        // whatever the surrounding context expects.
        def("result_err", false, 1, Never),
        def("vec_new", false, 0, NotStaticallyKnowable),
        def("clone", true, 1, SameAsArg(0)),
        def("as_ref", true, 1, SameAsArg(0)),
        def("map_or", true, 3, SameAsArg(1)),
        def("iter", true, 1, SameAsArg(0)),
        def("collect", true, 1, NotStaticallyKnowable),
        def("find", true, 2, NotStaticallyKnowable),
        def("unwrap_or", true, 2, SameAsArg(1)),
        def("to_owned", true, 1, SameAsArg(0)),
        def("as_str", true, 1, SameAsArg(0)),
        def("to_string", true, 1, TargetNativeString),
        def("and_then", true, 2, NotStaticallyKnowable),
        def("trim_end", true, 1, SameAsArg(0)),
        def("trim_start", true, 1, SameAsArg(0)),
        def("split_whitespace", true, 1, NotStaticallyKnowable),
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
    TestCommandMockReset,
    TestCommandMockPush,
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
            Self::TestCommandMockReset => "test_command_mock_reset",
            Self::TestCommandMockPush => "test_command_mock_push",
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
        }
    }
}

/// A recognized portable call — either a high-level "stdlib idiom differs
/// per language" op (`Op`, a `PortableOp` resolved from the central
/// registry — see `PortableOpRegistry`) or a low-level compiler intrinsic
/// with a fixed, closed set of variants and no meaningful "target doesn't
/// have this" case (`Intrinsic`). These two stay deliberately asymmetric:
/// `IntrinsicKind` is closed because its members are genuine compiler
/// primitives (println, fs read, reflection, ...); `Op`'s openness is the
/// whole point of the portable-op system.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum CallKind {
    Op(PortableOp),
    Intrinsic(IntrinsicKind),
}

impl From<PortableOp> for CallKind {
    fn from(op: PortableOp) -> Self {
        Self::Op(op)
    }
}

impl From<IntrinsicKind> for CallKind {
    fn from(kind: IntrinsicKind) -> Self {
        Self::Intrinsic(kind)
    }
}

// Associated constants below are deliberately named to mirror `IntrinsicKind`'s
// own PascalCase variant names 1:1 (e.g. `CallKind::Println`), not
// SCREAMING_CASE, for call-site readability.
#[allow(non_upper_case_globals)]
impl CallKind {
    /// `Some(kind)` if this call is a genuine low-level intrinsic;
    /// `None` for a portable `Op` (portable ops never overlap with
    /// `IntrinsicKind` — a call that maps straight to one is represented as
    /// `CallKind::Intrinsic` directly, never wrapped in `Op` first).
    pub fn intrinsic_kind(&self) -> Option<IntrinsicKind> {
        match self {
            Self::Intrinsic(kind) => Some(*kind),
            Self::Op(_) => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::Op(op) => op.name().to_string(),
            Self::Intrinsic(kind) => kind.name().to_string(),
        }
    }

    // Ergonomic shortcuts for every `IntrinsicKind` variant, used pervasively
    // across target-language backends (`matches!(call.kind, CallKind::X)`,
    // `match call.kind { CallKind::X => .. }`). All genuine intrinsics go
    // straight to `Self::Intrinsic` — none of these ever need the portable-op
    // `Op` representation (see `intrinsic_kind`'s doc comment).
    pub const Println: Self = Self::Intrinsic(IntrinsicKind::Println);
    pub const Print: Self = Self::Intrinsic(IntrinsicKind::Print);
    pub const Format: Self = Self::Intrinsic(IntrinsicKind::Format);
    pub const Len: Self = Self::Intrinsic(IntrinsicKind::Len);
    pub const Slice: Self = Self::Intrinsic(IntrinsicKind::Slice);
    pub const DebugAssertions: Self = Self::Intrinsic(IntrinsicKind::DebugAssertions);
    pub const Input: Self = Self::Intrinsic(IntrinsicKind::Input);
    pub const Panic: Self = Self::Intrinsic(IntrinsicKind::Panic);
    pub const CatchUnwind: Self = Self::Intrinsic(IntrinsicKind::CatchUnwind);
    pub const CatchUnwindResult: Self = Self::Intrinsic(IntrinsicKind::CatchUnwindResult);
    pub const TimeNow: Self = Self::Intrinsic(IntrinsicKind::TimeNow);
    pub const FsReadToString: Self = Self::Intrinsic(IntrinsicKind::FsReadToString);
    pub const FsWriteString: Self = Self::Intrinsic(IntrinsicKind::FsWriteString);
    pub const FsAppendString: Self = Self::Intrinsic(IntrinsicKind::FsAppendString);
    pub const FsExists: Self = Self::Intrinsic(IntrinsicKind::FsExists);
    pub const FsIsDir: Self = Self::Intrinsic(IntrinsicKind::FsIsDir);
    pub const FsIsFile: Self = Self::Intrinsic(IntrinsicKind::FsIsFile);
    pub const FsReadDir: Self = Self::Intrinsic(IntrinsicKind::FsReadDir);
    pub const FsWalkDir: Self = Self::Intrinsic(IntrinsicKind::FsWalkDir);
    pub const FsCreateDirAll: Self = Self::Intrinsic(IntrinsicKind::FsCreateDirAll);
    pub const FsRemoveFile: Self = Self::Intrinsic(IntrinsicKind::FsRemoveFile);
    pub const FsRemoveDirAll: Self = Self::Intrinsic(IntrinsicKind::FsRemoveDirAll);
    pub const FsGlob: Self = Self::Intrinsic(IntrinsicKind::FsGlob);
    pub const EnvCurrentDir: Self = Self::Intrinsic(IntrinsicKind::EnvCurrentDir);
    pub const EnvTempDir: Self = Self::Intrinsic(IntrinsicKind::EnvTempDir);
    pub const EnvHomeDir: Self = Self::Intrinsic(IntrinsicKind::EnvHomeDir);
    pub const EnvVar: Self = Self::Intrinsic(IntrinsicKind::EnvVar);
    pub const EnvVarExists: Self = Self::Intrinsic(IntrinsicKind::EnvVarExists);
    pub const PathJoin: Self = Self::Intrinsic(IntrinsicKind::PathJoin);
    pub const PathParent: Self = Self::Intrinsic(IntrinsicKind::PathParent);
    pub const PathFileName: Self = Self::Intrinsic(IntrinsicKind::PathFileName);
    pub const PathExtension: Self = Self::Intrinsic(IntrinsicKind::PathExtension);
    pub const PathStem: Self = Self::Intrinsic(IntrinsicKind::PathStem);
    pub const PathIsAbsolute: Self = Self::Intrinsic(IntrinsicKind::PathIsAbsolute);
    pub const PathNormalize: Self = Self::Intrinsic(IntrinsicKind::PathNormalize);
    pub const IoReadStdinToString: Self = Self::Intrinsic(IntrinsicKind::IoReadStdinToString);
    pub const IoWriteStdout: Self = Self::Intrinsic(IntrinsicKind::IoWriteStdout);
    pub const IoWriteStderr: Self = Self::Intrinsic(IntrinsicKind::IoWriteStderr);
    pub const YamlToJson: Self = Self::Intrinsic(IntrinsicKind::YamlToJson);
    pub const JsonParse: Self = Self::Intrinsic(IntrinsicKind::JsonParse);
    pub const TestCommandMockReset: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockReset);
    pub const TestCommandMockPush: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockPush);
    pub const TestCommandMockTakeCalls: Self =
        Self::Intrinsic(IntrinsicKind::TestCommandMockTakeCalls);
    pub const TestCommandMockApply: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockApply);
    pub const Sleep: Self = Self::Intrinsic(IntrinsicKind::Sleep);
    pub const Spawn: Self = Self::Intrinsic(IntrinsicKind::Spawn);
    pub const Join: Self = Self::Intrinsic(IntrinsicKind::Join);
    pub const Select: Self = Self::Intrinsic(IntrinsicKind::Select);
    pub const Yield: Self = Self::Intrinsic(IntrinsicKind::Yield);
    pub const SizeOf: Self = Self::Intrinsic(IntrinsicKind::SizeOf);
    pub const ReflectFields: Self = Self::Intrinsic(IntrinsicKind::ReflectFields);
    pub const HasMethod: Self = Self::Intrinsic(IntrinsicKind::HasMethod);
    pub const TypeName: Self = Self::Intrinsic(IntrinsicKind::TypeName);
    pub const TypeOf: Self = Self::Intrinsic(IntrinsicKind::TypeOf);
    pub const CreateStruct: Self = Self::Intrinsic(IntrinsicKind::CreateStruct);
    pub const AddField: Self = Self::Intrinsic(IntrinsicKind::AddField);
    pub const CloneStruct: Self = Self::Intrinsic(IntrinsicKind::CloneStruct);
    pub const BuildType: Self = Self::Intrinsic(IntrinsicKind::BuildType);
    pub const PrimitiveType: Self = Self::Intrinsic(IntrinsicKind::PrimitiveType);
    pub const HasField: Self = Self::Intrinsic(IntrinsicKind::HasField);
    pub const FieldCount: Self = Self::Intrinsic(IntrinsicKind::FieldCount);
    pub const MethodCount: Self = Self::Intrinsic(IntrinsicKind::MethodCount);
    pub const FieldType: Self = Self::Intrinsic(IntrinsicKind::FieldType);
    pub const VecType: Self = Self::Intrinsic(IntrinsicKind::VecType);
    pub const FieldNameAt: Self = Self::Intrinsic(IntrinsicKind::FieldNameAt);
    pub const StructSize: Self = Self::Intrinsic(IntrinsicKind::StructSize);
    pub const GenerateMethod: Self = Self::Intrinsic(IntrinsicKind::GenerateMethod);
    pub const CompileError: Self = Self::Intrinsic(IntrinsicKind::CompileError);
    pub const CompileWarning: Self = Self::Intrinsic(IntrinsicKind::CompileWarning);
    pub const ProcMacroTokenStreamFromStr: Self =
        Self::Intrinsic(IntrinsicKind::ProcMacroTokenStreamFromStr);
    pub const ProcMacroTokenStreamToString: Self =
        Self::Intrinsic(IntrinsicKind::ProcMacroTokenStreamToString);
    pub const ShellExec: Self = Self::Intrinsic(IntrinsicKind::ShellExec);
    pub const ShellFileCopy: Self = Self::Intrinsic(IntrinsicKind::ShellFileCopy);
    pub const ShellFileTemplate: Self = Self::Intrinsic(IntrinsicKind::ShellFileTemplate);
    pub const ShellFileRsync: Self = Self::Intrinsic(IntrinsicKind::ShellFileRsync);
}
