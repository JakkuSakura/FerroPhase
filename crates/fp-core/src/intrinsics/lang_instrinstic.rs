use super::CallKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangInstrinsticCapability {
    Portable,
    ConstOnly,
    RuntimeOnly,
    InterpreterOnly,
    BackendLimited,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangInstrinstic {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LangInstrinsticSpec {
    pub intrinsic: LangInstrinstic,
    pub lang_item: &'static str,
    pub capability: LangInstrinsticCapability,
    pub call_kind: Option<CallKind>,
}

const LANG_INSTRINSTICS: &[LangInstrinsticSpec] = &[
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TimeNow,
        lang_item: "time_now",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::TimeNow),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::CreateStruct,
        lang_item: "create_struct",
        capability: LangInstrinsticCapability::ConstOnly,
        call_kind: Some(CallKind::CreateStruct),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::AddField,
        lang_item: "addfield",
        capability: LangInstrinsticCapability::ConstOnly,
        call_kind: Some(CallKind::AddField),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::BuildType,
        lang_item: "build_type",
        capability: LangInstrinsticCapability::ConstOnly,
        call_kind: Some(CallKind::BuildType),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsReadDir,
        lang_item: "fs_read_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsReadDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsWalkDir,
        lang_item: "fs_walk_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsWalkDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsReadToString,
        lang_item: "fs_read_to_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsReadToString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsWriteString,
        lang_item: "fs_write_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsWriteString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsAppendString,
        lang_item: "fs_append_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsAppendString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsExists,
        lang_item: "fs_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsExists),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsDir,
        lang_item: "fs_is_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsIsDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsFile,
        lang_item: "fs_is_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsIsFile),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsCreateDirAll,
        lang_item: "fs_create_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsCreateDirAll),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveFile,
        lang_item: "fs_remove_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsRemoveFile),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveDirAll,
        lang_item: "fs_remove_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsRemoveDirAll),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsGlob,
        lang_item: "fs_glob",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::FsGlob),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvCurrentDir,
        lang_item: "env_current_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvCurrentDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvTempDir,
        lang_item: "env_temp_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvTempDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvHomeDir,
        lang_item: "env_home_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvHomeDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVar,
        lang_item: "env_var",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvVar),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVarExists,
        lang_item: "env_var_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::EnvVarExists),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathJoin,
        lang_item: "path_join",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathJoin),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathParent,
        lang_item: "path_parent",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathParent),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathFileName,
        lang_item: "path_file_name",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathFileName),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathExtension,
        lang_item: "path_extension",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathExtension),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathStem,
        lang_item: "path_stem",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathStem),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathIsAbsolute,
        lang_item: "path_is_absolute",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathIsAbsolute),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathNormalize,
        lang_item: "path_normalize",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(CallKind::PathNormalize),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoReadStdinToString,
        lang_item: "io_read_stdin_to_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoReadStdinToString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStdout,
        lang_item: "io_write_stdout",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoWriteStdout),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStderr,
        lang_item: "io_write_stderr",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(CallKind::IoWriteStderr),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::YamlToJson,
        lang_item: "yaml_to_json",
        capability: LangInstrinsticCapability::BackendLimited,
        call_kind: Some(CallKind::YamlToJson),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::JsonParse,
        lang_item: "json_parse",
        capability: LangInstrinsticCapability::BackendLimited,
        call_kind: Some(CallKind::JsonParse),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockReset,
        lang_item: "test_command_mock_reset",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockReset),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockPush,
        lang_item: "test_command_mock_push",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockPush),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockTakeCalls,
        lang_item: "test_command_mock_take_calls",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockTakeCalls),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockApply,
        lang_item: "test_command_mock_apply",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(CallKind::TestCommandMockApply),
    },
];

pub fn lang_instrinstic_spec(intrinsic: LangInstrinstic) -> &'static LangInstrinsticSpec {
    LANG_INSTRINSTICS
        .iter()
        .find(|spec| spec.intrinsic == intrinsic)
        .expect("lang instrinstic spec must exist")
}

pub fn lang_instrinstic_for_lang_item(name: &str) -> Option<LangInstrinstic> {
    LANG_INSTRINSTICS
        .iter()
        .find(|spec| spec.lang_item == name)
        .map(|spec| spec.intrinsic)
}

pub fn lang_instrinstic_call_kind(intrinsic: LangInstrinstic) -> Option<CallKind> {
    lang_instrinstic_spec(intrinsic).call_kind
}

pub fn lang_instrinstic_capability(intrinsic: LangInstrinstic) -> LangInstrinsticCapability {
    lang_instrinstic_spec(intrinsic).capability
}

pub fn lang_instrinstic_lang_item(intrinsic: LangInstrinstic) -> &'static str {
    lang_instrinstic_spec(intrinsic).lang_item
}
