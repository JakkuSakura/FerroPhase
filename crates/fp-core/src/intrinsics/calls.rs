#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum OpKind {
    Println,
    Print,
    Format,
    Input,
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
    IoReadStdinToString,
    IoWriteStdout,
    IoWriteStderr,
    YamlToJson,
    JsonParse,
    Sleep,
    Spawn,
    Join,
    Select,
    ShellExec,
    ShellFileCopy,
    ShellFileTemplate,
    ShellFileRsync,
    OptionSome,
    OptionNone,
    OptionUnwrap,
    VecNew,
    Clone,
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
        CallKind::Intrinsic(self).name()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum CallKind {
    Op(OpKind),
    Intrinsic(IntrinsicKind),
}

impl From<OpKind> for CallKind {
    fn from(kind: OpKind) -> Self {
        Self::Op(kind)
    }
}

impl From<IntrinsicKind> for CallKind {
    fn from(kind: IntrinsicKind) -> Self {
        Self::Intrinsic(kind)
    }
}

impl CallKind {
    pub const fn intrinsic_kind(self) -> Option<IntrinsicKind> {
        match self {
            Self::Op(OpKind::Println) => Some(IntrinsicKind::Println),
            Self::Op(OpKind::Print) => Some(IntrinsicKind::Print),
            Self::Op(OpKind::Format) => Some(IntrinsicKind::Format),
            Self::Op(OpKind::Input) => Some(IntrinsicKind::Input),
            Self::Op(OpKind::TimeNow) => Some(IntrinsicKind::TimeNow),
            Self::Op(OpKind::FsReadToString) => Some(IntrinsicKind::FsReadToString),
            Self::Op(OpKind::FsWriteString) => Some(IntrinsicKind::FsWriteString),
            Self::Op(OpKind::FsAppendString) => Some(IntrinsicKind::FsAppendString),
            Self::Op(OpKind::FsExists) => Some(IntrinsicKind::FsExists),
            Self::Op(OpKind::FsIsDir) => Some(IntrinsicKind::FsIsDir),
            Self::Op(OpKind::FsIsFile) => Some(IntrinsicKind::FsIsFile),
            Self::Op(OpKind::FsReadDir) => Some(IntrinsicKind::FsReadDir),
            Self::Op(OpKind::FsWalkDir) => Some(IntrinsicKind::FsWalkDir),
            Self::Op(OpKind::FsCreateDirAll) => Some(IntrinsicKind::FsCreateDirAll),
            Self::Op(OpKind::FsRemoveFile) => Some(IntrinsicKind::FsRemoveFile),
            Self::Op(OpKind::FsRemoveDirAll) => Some(IntrinsicKind::FsRemoveDirAll),
            Self::Op(OpKind::FsGlob) => Some(IntrinsicKind::FsGlob),
            Self::Op(OpKind::EnvCurrentDir) => Some(IntrinsicKind::EnvCurrentDir),
            Self::Op(OpKind::EnvTempDir) => Some(IntrinsicKind::EnvTempDir),
            Self::Op(OpKind::EnvHomeDir) => Some(IntrinsicKind::EnvHomeDir),
            Self::Op(OpKind::EnvVar) => Some(IntrinsicKind::EnvVar),
            Self::Op(OpKind::EnvVarExists) => Some(IntrinsicKind::EnvVarExists),
            Self::Op(OpKind::IoReadStdinToString) => Some(IntrinsicKind::IoReadStdinToString),
            Self::Op(OpKind::IoWriteStdout) => Some(IntrinsicKind::IoWriteStdout),
            Self::Op(OpKind::IoWriteStderr) => Some(IntrinsicKind::IoWriteStderr),
            Self::Op(OpKind::YamlToJson) => Some(IntrinsicKind::YamlToJson),
            Self::Op(OpKind::JsonParse) => Some(IntrinsicKind::JsonParse),
            Self::Op(OpKind::Sleep) => Some(IntrinsicKind::Sleep),
            Self::Op(OpKind::Spawn) => Some(IntrinsicKind::Spawn),
            Self::Op(OpKind::Join) => Some(IntrinsicKind::Join),
            Self::Op(OpKind::Select) => Some(IntrinsicKind::Select),
            Self::Op(OpKind::ShellExec) => Some(IntrinsicKind::ShellExec),
            Self::Op(OpKind::ShellFileCopy) => Some(IntrinsicKind::ShellFileCopy),
            Self::Op(OpKind::ShellFileTemplate) => Some(IntrinsicKind::ShellFileTemplate),
            Self::Op(OpKind::ShellFileRsync) => Some(IntrinsicKind::ShellFileRsync),
            Self::Op(OpKind::OptionSome) => None,
            Self::Op(OpKind::OptionNone) => None,
            Self::Op(OpKind::OptionUnwrap) => None,
            Self::Op(OpKind::VecNew) => None,
            Self::Op(OpKind::Clone) => None,
            Self::Intrinsic(kind) => Some(kind),
        }
    }

    pub const fn op_kind(self) -> Option<OpKind> {
        match self {
            Self::Op(kind) => Some(kind),
            Self::Intrinsic(IntrinsicKind::Println) => Some(OpKind::Println),
            Self::Intrinsic(IntrinsicKind::Print) => Some(OpKind::Print),
            Self::Intrinsic(IntrinsicKind::Format) => Some(OpKind::Format),
            Self::Intrinsic(IntrinsicKind::Input) => Some(OpKind::Input),
            Self::Intrinsic(IntrinsicKind::TimeNow) => Some(OpKind::TimeNow),
            Self::Intrinsic(IntrinsicKind::FsReadToString) => Some(OpKind::FsReadToString),
            Self::Intrinsic(IntrinsicKind::FsWriteString) => Some(OpKind::FsWriteString),
            Self::Intrinsic(IntrinsicKind::FsAppendString) => Some(OpKind::FsAppendString),
            Self::Intrinsic(IntrinsicKind::FsExists) => Some(OpKind::FsExists),
            Self::Intrinsic(IntrinsicKind::FsIsDir) => Some(OpKind::FsIsDir),
            Self::Intrinsic(IntrinsicKind::FsIsFile) => Some(OpKind::FsIsFile),
            Self::Intrinsic(IntrinsicKind::FsReadDir) => Some(OpKind::FsReadDir),
            Self::Intrinsic(IntrinsicKind::FsWalkDir) => Some(OpKind::FsWalkDir),
            Self::Intrinsic(IntrinsicKind::FsCreateDirAll) => Some(OpKind::FsCreateDirAll),
            Self::Intrinsic(IntrinsicKind::FsRemoveFile) => Some(OpKind::FsRemoveFile),
            Self::Intrinsic(IntrinsicKind::FsRemoveDirAll) => Some(OpKind::FsRemoveDirAll),
            Self::Intrinsic(IntrinsicKind::FsGlob) => Some(OpKind::FsGlob),
            Self::Intrinsic(IntrinsicKind::EnvCurrentDir) => Some(OpKind::EnvCurrentDir),
            Self::Intrinsic(IntrinsicKind::EnvTempDir) => Some(OpKind::EnvTempDir),
            Self::Intrinsic(IntrinsicKind::EnvHomeDir) => Some(OpKind::EnvHomeDir),
            Self::Intrinsic(IntrinsicKind::EnvVar) => Some(OpKind::EnvVar),
            Self::Intrinsic(IntrinsicKind::EnvVarExists) => Some(OpKind::EnvVarExists),
            Self::Intrinsic(IntrinsicKind::IoReadStdinToString) => {
                Some(OpKind::IoReadStdinToString)
            }
            Self::Intrinsic(IntrinsicKind::IoWriteStdout) => Some(OpKind::IoWriteStdout),
            Self::Intrinsic(IntrinsicKind::IoWriteStderr) => Some(OpKind::IoWriteStderr),
            Self::Intrinsic(IntrinsicKind::YamlToJson) => Some(OpKind::YamlToJson),
            Self::Intrinsic(IntrinsicKind::JsonParse) => Some(OpKind::JsonParse),
            Self::Intrinsic(IntrinsicKind::Sleep) => Some(OpKind::Sleep),
            Self::Intrinsic(IntrinsicKind::Spawn) => Some(OpKind::Spawn),
            Self::Intrinsic(IntrinsicKind::Join) => Some(OpKind::Join),
            Self::Intrinsic(IntrinsicKind::Select) => Some(OpKind::Select),
            Self::Intrinsic(IntrinsicKind::ShellExec) => Some(OpKind::ShellExec),
            Self::Intrinsic(IntrinsicKind::ShellFileCopy) => Some(OpKind::ShellFileCopy),
            Self::Intrinsic(IntrinsicKind::ShellFileTemplate) => Some(OpKind::ShellFileTemplate),
            Self::Intrinsic(IntrinsicKind::ShellFileRsync) => Some(OpKind::ShellFileRsync),
            Self::Intrinsic(_) => None,
        }
    }

    pub const fn name(self) -> &'static str {
        match self {
            Self::Op(OpKind::Println) | Self::Intrinsic(IntrinsicKind::Println) => "println",
            Self::Op(OpKind::Print) | Self::Intrinsic(IntrinsicKind::Print) => "print",
            Self::Op(OpKind::Format) | Self::Intrinsic(IntrinsicKind::Format) => "format",
            Self::Intrinsic(IntrinsicKind::Len) => "len",
            Self::Intrinsic(IntrinsicKind::Slice) => "slice",
            Self::Intrinsic(IntrinsicKind::DebugAssertions) => "debug_assertions",
            Self::Op(OpKind::Input) | Self::Intrinsic(IntrinsicKind::Input) => "input",
            Self::Intrinsic(IntrinsicKind::Panic) => "panic",
            Self::Intrinsic(IntrinsicKind::CatchUnwind) => "catch_unwind",
            Self::Intrinsic(IntrinsicKind::CatchUnwindResult) => "catch_unwind_result",
            Self::Op(OpKind::TimeNow) | Self::Intrinsic(IntrinsicKind::TimeNow) => "time_now",
            Self::Op(OpKind::FsReadDir) | Self::Intrinsic(IntrinsicKind::FsReadDir) => {
                "fs_read_dir"
            }
            Self::Op(OpKind::FsWalkDir) | Self::Intrinsic(IntrinsicKind::FsWalkDir) => {
                "fs_walk_dir"
            }
            Self::Op(OpKind::FsReadToString) | Self::Intrinsic(IntrinsicKind::FsReadToString) => {
                "fs_read_to_string"
            }
            Self::Op(OpKind::FsWriteString) | Self::Intrinsic(IntrinsicKind::FsWriteString) => {
                "fs_write_string"
            }
            Self::Op(OpKind::FsAppendString) | Self::Intrinsic(IntrinsicKind::FsAppendString) => {
                "fs_append_string"
            }
            Self::Op(OpKind::FsExists) | Self::Intrinsic(IntrinsicKind::FsExists) => "fs_exists",
            Self::Op(OpKind::FsIsDir) | Self::Intrinsic(IntrinsicKind::FsIsDir) => "fs_is_dir",
            Self::Op(OpKind::FsIsFile) | Self::Intrinsic(IntrinsicKind::FsIsFile) => "fs_is_file",
            Self::Op(OpKind::FsCreateDirAll) | Self::Intrinsic(IntrinsicKind::FsCreateDirAll) => {
                "fs_create_dir_all"
            }
            Self::Op(OpKind::FsRemoveFile) | Self::Intrinsic(IntrinsicKind::FsRemoveFile) => {
                "fs_remove_file"
            }
            Self::Op(OpKind::FsRemoveDirAll) | Self::Intrinsic(IntrinsicKind::FsRemoveDirAll) => {
                "fs_remove_dir_all"
            }
            Self::Op(OpKind::FsGlob) | Self::Intrinsic(IntrinsicKind::FsGlob) => "fs_glob",
            Self::Op(OpKind::EnvCurrentDir) | Self::Intrinsic(IntrinsicKind::EnvCurrentDir) => {
                "env_current_dir"
            }
            Self::Op(OpKind::EnvTempDir) | Self::Intrinsic(IntrinsicKind::EnvTempDir) => {
                "env_temp_dir"
            }
            Self::Op(OpKind::EnvHomeDir) | Self::Intrinsic(IntrinsicKind::EnvHomeDir) => {
                "env_home_dir"
            }
            Self::Op(OpKind::EnvVar) | Self::Intrinsic(IntrinsicKind::EnvVar) => "env_var",
            Self::Op(OpKind::EnvVarExists) | Self::Intrinsic(IntrinsicKind::EnvVarExists) => {
                "env_var_exists"
            }
            Self::Intrinsic(IntrinsicKind::PathJoin) => "path_join",
            Self::Intrinsic(IntrinsicKind::PathParent) => "path_parent",
            Self::Intrinsic(IntrinsicKind::PathFileName) => "path_file_name",
            Self::Intrinsic(IntrinsicKind::PathExtension) => "path_extension",
            Self::Intrinsic(IntrinsicKind::PathStem) => "path_stem",
            Self::Intrinsic(IntrinsicKind::PathIsAbsolute) => "path_is_absolute",
            Self::Intrinsic(IntrinsicKind::PathNormalize) => "path_normalize",
            Self::Op(OpKind::IoReadStdinToString)
            | Self::Intrinsic(IntrinsicKind::IoReadStdinToString) => "io_read_stdin_to_string",
            Self::Op(OpKind::IoWriteStdout) | Self::Intrinsic(IntrinsicKind::IoWriteStdout) => {
                "io_write_stdout"
            }
            Self::Op(OpKind::IoWriteStderr) | Self::Intrinsic(IntrinsicKind::IoWriteStderr) => {
                "io_write_stderr"
            }
            Self::Op(OpKind::YamlToJson) | Self::Intrinsic(IntrinsicKind::YamlToJson) => {
                "yaml_to_json"
            }
            Self::Op(OpKind::JsonParse) | Self::Intrinsic(IntrinsicKind::JsonParse) => "json_parse",
            Self::Intrinsic(IntrinsicKind::TestCommandMockReset) => "test_command_mock_reset",
            Self::Intrinsic(IntrinsicKind::TestCommandMockPush) => "test_command_mock_push",
            Self::Intrinsic(IntrinsicKind::TestCommandMockTakeCalls) => {
                "test_command_mock_take_calls"
            }
            Self::Intrinsic(IntrinsicKind::TestCommandMockApply) => "test_command_mock_apply",
            Self::Op(OpKind::Sleep) | Self::Intrinsic(IntrinsicKind::Sleep) => "sleep",
            Self::Op(OpKind::Spawn) | Self::Intrinsic(IntrinsicKind::Spawn) => "spawn",
            Self::Op(OpKind::Join) | Self::Intrinsic(IntrinsicKind::Join) => "join",
            Self::Op(OpKind::Select) | Self::Intrinsic(IntrinsicKind::Select) => "select",
            Self::Intrinsic(IntrinsicKind::Yield) => "yield",
            Self::Intrinsic(IntrinsicKind::SizeOf) => "size_of",
            Self::Intrinsic(IntrinsicKind::ReflectFields) => "reflect_fields",
            Self::Intrinsic(IntrinsicKind::HasMethod) => "has_method",
            Self::Intrinsic(IntrinsicKind::TypeName) => "type_name",
            Self::Intrinsic(IntrinsicKind::TypeOf) => "type_of",
            Self::Intrinsic(IntrinsicKind::CreateStruct) => "create_struct",
            Self::Intrinsic(IntrinsicKind::AddField) => "add_field",
            Self::Intrinsic(IntrinsicKind::CloneStruct) => "clone_struct",
            Self::Intrinsic(IntrinsicKind::BuildType) => "build_type",
            Self::Intrinsic(IntrinsicKind::HasField) => "has_field",
            Self::Intrinsic(IntrinsicKind::FieldCount) => "field_count",
            Self::Intrinsic(IntrinsicKind::MethodCount) => "method_count",
            Self::Intrinsic(IntrinsicKind::FieldType) => "field_type",
            Self::Intrinsic(IntrinsicKind::VecType) => "vec_type",
            Self::Intrinsic(IntrinsicKind::FieldNameAt) => "field_name_at",
            Self::Intrinsic(IntrinsicKind::StructSize) => "struct_size",
            Self::Intrinsic(IntrinsicKind::GenerateMethod) => "generate_method",
            Self::Intrinsic(IntrinsicKind::CompileError) => "compile_error",
            Self::Intrinsic(IntrinsicKind::CompileWarning) => "compile_warning",
            Self::Intrinsic(IntrinsicKind::ProcMacroTokenStreamFromStr) => "token_stream_from_str",
            Self::Intrinsic(IntrinsicKind::ProcMacroTokenStreamToString) => {
                "token_stream_to_string"
            }
            Self::Op(OpKind::ShellExec) | Self::Intrinsic(IntrinsicKind::ShellExec) => "shell_exec",
            Self::Op(OpKind::ShellFileCopy) | Self::Intrinsic(IntrinsicKind::ShellFileCopy) => {
                "shell_file_copy"
            }
            Self::Op(OpKind::ShellFileTemplate)
            | Self::Intrinsic(IntrinsicKind::ShellFileTemplate) => "shell_file_template",
            Self::Op(OpKind::ShellFileRsync) | Self::Intrinsic(IntrinsicKind::ShellFileRsync) => {
                "shell_file_rsync"
            }
            Self::Op(OpKind::OptionSome) => "option_some",
            Self::Op(OpKind::OptionNone) => "option_none",
            Self::Op(OpKind::OptionUnwrap) => "option_unwrap",
            Self::Op(OpKind::VecNew) => "vec_new",
            Self::Op(OpKind::Clone) => "clone",
        }
    }

    pub const Println: Self = Self::Op(OpKind::Println);
    pub const Print: Self = Self::Op(OpKind::Print);
    pub const Format: Self = Self::Op(OpKind::Format);
    pub const Len: Self = Self::Intrinsic(IntrinsicKind::Len);
    pub const Slice: Self = Self::Intrinsic(IntrinsicKind::Slice);
    pub const DebugAssertions: Self = Self::Intrinsic(IntrinsicKind::DebugAssertions);
    pub const Input: Self = Self::Op(OpKind::Input);
    pub const Panic: Self = Self::Intrinsic(IntrinsicKind::Panic);
    pub const CatchUnwind: Self = Self::Intrinsic(IntrinsicKind::CatchUnwind);
    pub const CatchUnwindResult: Self = Self::Intrinsic(IntrinsicKind::CatchUnwindResult);
    pub const TimeNow: Self = Self::Op(OpKind::TimeNow);
    pub const FsReadToString: Self = Self::Op(OpKind::FsReadToString);
    pub const FsWriteString: Self = Self::Op(OpKind::FsWriteString);
    pub const FsAppendString: Self = Self::Op(OpKind::FsAppendString);
    pub const FsExists: Self = Self::Op(OpKind::FsExists);
    pub const FsIsDir: Self = Self::Op(OpKind::FsIsDir);
    pub const FsIsFile: Self = Self::Op(OpKind::FsIsFile);
    pub const FsReadDir: Self = Self::Op(OpKind::FsReadDir);
    pub const FsWalkDir: Self = Self::Op(OpKind::FsWalkDir);
    pub const FsCreateDirAll: Self = Self::Op(OpKind::FsCreateDirAll);
    pub const FsRemoveFile: Self = Self::Op(OpKind::FsRemoveFile);
    pub const FsRemoveDirAll: Self = Self::Op(OpKind::FsRemoveDirAll);
    pub const FsGlob: Self = Self::Op(OpKind::FsGlob);
    pub const EnvCurrentDir: Self = Self::Op(OpKind::EnvCurrentDir);
    pub const EnvTempDir: Self = Self::Op(OpKind::EnvTempDir);
    pub const EnvHomeDir: Self = Self::Op(OpKind::EnvHomeDir);
    pub const EnvVar: Self = Self::Op(OpKind::EnvVar);
    pub const EnvVarExists: Self = Self::Op(OpKind::EnvVarExists);
    pub const PathJoin: Self = Self::Intrinsic(IntrinsicKind::PathJoin);
    pub const PathParent: Self = Self::Intrinsic(IntrinsicKind::PathParent);
    pub const PathFileName: Self = Self::Intrinsic(IntrinsicKind::PathFileName);
    pub const PathExtension: Self = Self::Intrinsic(IntrinsicKind::PathExtension);
    pub const PathStem: Self = Self::Intrinsic(IntrinsicKind::PathStem);
    pub const PathIsAbsolute: Self = Self::Intrinsic(IntrinsicKind::PathIsAbsolute);
    pub const PathNormalize: Self = Self::Intrinsic(IntrinsicKind::PathNormalize);
    pub const IoReadStdinToString: Self = Self::Op(OpKind::IoReadStdinToString);
    pub const IoWriteStdout: Self = Self::Op(OpKind::IoWriteStdout);
    pub const IoWriteStderr: Self = Self::Op(OpKind::IoWriteStderr);
    pub const YamlToJson: Self = Self::Op(OpKind::YamlToJson);
    pub const JsonParse: Self = Self::Op(OpKind::JsonParse);
    pub const TestCommandMockReset: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockReset);
    pub const TestCommandMockPush: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockPush);
    pub const TestCommandMockTakeCalls: Self =
        Self::Intrinsic(IntrinsicKind::TestCommandMockTakeCalls);
    pub const TestCommandMockApply: Self = Self::Intrinsic(IntrinsicKind::TestCommandMockApply);
    pub const Sleep: Self = Self::Op(OpKind::Sleep);
    pub const Spawn: Self = Self::Op(OpKind::Spawn);
    pub const Join: Self = Self::Op(OpKind::Join);
    pub const Select: Self = Self::Op(OpKind::Select);
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
    pub const ShellExec: Self = Self::Op(OpKind::ShellExec);
    pub const ShellFileCopy: Self = Self::Op(OpKind::ShellFileCopy);
    pub const ShellFileTemplate: Self = Self::Op(OpKind::ShellFileTemplate);
    pub const ShellFileRsync: Self = Self::Op(OpKind::ShellFileRsync);
}
