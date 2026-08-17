use super::CallKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangIntrinsicCapability {
    Portable,
    ConstOnly,
    RuntimeOnly,
    InterpreterOnly,
    BackendLimited,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangIntrinsic {
    TimeNow,
    CreateStruct,
    AddField,
    BuildType,
    FsReadDir,
    FsWalkDir,
    FsReadToString,
    FsWriteString,
    FsAppendString,
    FsExists,
    FsIsDir,
    FsIsFile,
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
    CatchUnwind,
    CatchUnwindResult,
    Print,
    Println,
    Spawn,
    Join,
    Select,
    ProcMacroTokenStreamFromStr,
    ProcMacroTokenStreamToString,
    FieldType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LangIntrinsicSpec {
    pub intrinsic: LangIntrinsic,
    pub lang_item: &'static str,
    pub capability: LangIntrinsicCapability,
    pub call_kind: Option<CallKind>,
}

const LANG_INSTRINSTICS: &[LangIntrinsicSpec] = &[
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::TimeNow,
        lang_item: "time_now",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::TimeNow),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::CreateStruct,
        lang_item: "create_struct",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::CreateStruct),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::AddField,
        lang_item: "addfield",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::AddField),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::BuildType,
        lang_item: "build_type",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::BuildType),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsReadDir,
        lang_item: "fs_read_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsReadDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsWalkDir,
        lang_item: "fs_walk_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsWalkDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsReadToString,
        lang_item: "fs_read_to_string",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsReadToString),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsWriteString,
        lang_item: "fs_write_string",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsWriteString),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsAppendString,
        lang_item: "fs_append_string",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsAppendString),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsExists,
        lang_item: "fs_exists",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsExists),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsIsDir,
        lang_item: "fs_is_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsIsDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsIsFile,
        lang_item: "fs_is_file",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsIsFile),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsCreateDirAll,
        lang_item: "fs_create_dir_all",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsCreateDirAll),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsRemoveFile,
        lang_item: "fs_remove_file",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsRemoveFile),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsRemoveDirAll,
        lang_item: "fs_remove_dir_all",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsRemoveDirAll),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FsGlob,
        lang_item: "fs_glob",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsGlob),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::EnvCurrentDir,
        lang_item: "env_current_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvCurrentDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::EnvTempDir,
        lang_item: "env_temp_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvTempDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::EnvHomeDir,
        lang_item: "env_home_dir",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvHomeDir),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::EnvVar,
        lang_item: "env_var",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvVar),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::EnvVarExists,
        lang_item: "env_var_exists",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvVarExists),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathJoin,
        lang_item: "path_join",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathJoin),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathParent,
        lang_item: "path_parent",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathParent),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathFileName,
        lang_item: "path_file_name",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathFileName),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathExtension,
        lang_item: "path_extension",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathExtension),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathStem,
        lang_item: "path_stem",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathStem),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathIsAbsolute,
        lang_item: "path_is_absolute",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathIsAbsolute),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::PathNormalize,
        lang_item: "path_normalize",
        capability: LangIntrinsicCapability::Portable,
        call_kind: Some(CallKind::PathNormalize),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::IoReadStdinToString,
        lang_item: "io_read_stdin_to_string",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoReadStdinToString),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::IoWriteStdout,
        lang_item: "io_write_stdout",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoWriteStdout),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::IoWriteStderr,
        lang_item: "io_write_stderr",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoWriteStderr),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::YamlToJson,
        lang_item: "yaml_to_json",
        capability: LangIntrinsicCapability::BackendLimited,
        call_kind: Some(CallKind::YamlToJson),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::JsonParse,
        lang_item: "json_parse",
        capability: LangIntrinsicCapability::BackendLimited,
        call_kind: Some(CallKind::JsonParse),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::TestCommandMockReset,
        lang_item: "test_command_mock_reset",
        capability: LangIntrinsicCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockReset),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::TestCommandMockPush,
        lang_item: "test_command_mock_push",
        capability: LangIntrinsicCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockPush),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::TestCommandMockTakeCalls,
        lang_item: "test_command_mock_take_calls",
        capability: LangIntrinsicCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockTakeCalls),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::TestCommandMockApply,
        lang_item: "test_command_mock_apply",
        capability: LangIntrinsicCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockApply),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::CatchUnwind,
        lang_item: "catch_unwind",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::CatchUnwind),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::CatchUnwindResult,
        lang_item: "catch_unwind_result",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::CatchUnwindResult),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::Print,
        lang_item: "print",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::Print),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::Println,
        lang_item: "println",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::Println),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::Spawn,
        lang_item: "spawn",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::Spawn),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::Join,
        lang_item: "join",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::Join),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::Select,
        lang_item: "select",
        capability: LangIntrinsicCapability::RuntimeOnly,
        call_kind: Some(CallKind::Select),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::ProcMacroTokenStreamFromStr,
        lang_item: "proc_macro_token_stream_from_str",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::ProcMacroTokenStreamFromStr),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::ProcMacroTokenStreamToString,
        lang_item: "proc_macro_token_stream_to_string",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::ProcMacroTokenStreamToString),
    },
    LangIntrinsicSpec {
        intrinsic: LangIntrinsic::FieldType,
        lang_item: "field_type",
        capability: LangIntrinsicCapability::ConstOnly,
        call_kind: Some(CallKind::FieldType),
    },
];

pub fn lang_intrinsic_spec(intrinsic: LangIntrinsic) -> &'static LangIntrinsicSpec {
    LANG_INSTRINSTICS
        .iter()
        .find(|spec| spec.intrinsic == intrinsic)
        .expect("lang intrinsic spec must exist")
}

pub fn lang_intrinsic_for_lang_item(name: &str) -> Option<LangIntrinsic> {
    LANG_INSTRINSTICS
        .iter()
        .find(|spec| spec.lang_item == name)
        .map(|spec| spec.intrinsic)
}

pub fn lang_intrinsic_call_kind(intrinsic: LangIntrinsic) -> Option<CallKind> {
    lang_intrinsic_spec(intrinsic).call_kind
}

pub fn lang_intrinsic_capability(intrinsic: LangIntrinsic) -> LangIntrinsicCapability {
    lang_intrinsic_spec(intrinsic).capability
}

pub fn lang_intrinsic_lang_item(intrinsic: LangIntrinsic) -> &'static str {
    lang_intrinsic_spec(intrinsic).lang_item
}
