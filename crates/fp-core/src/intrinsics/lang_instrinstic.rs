use super::IntrinsicCallKind;

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
    pub call_kind: Option<IntrinsicCallKind>,
}

const LANG_INSTRINSTICS: &[LangInstrinsticSpec] = &[
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TimeNow,
        lang_item: "time_now",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::TimeNow),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::CreateStruct,
        lang_item: "create_struct",
        capability: LangInstrinsticCapability::ConstOnly,
        call_kind: Some(IntrinsicCallKind::CreateStruct),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::AddField,
        lang_item: "addfield",
        capability: LangInstrinsticCapability::ConstOnly,
        call_kind: Some(IntrinsicCallKind::AddField),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsReadDir,
        lang_item: "fs_read_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsReadDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsWalkDir,
        lang_item: "fs_walk_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsWalkDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsReadToString,
        lang_item: "fs_read_to_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsReadToString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsWriteString,
        lang_item: "fs_write_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsWriteString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsAppendString,
        lang_item: "fs_append_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsAppendString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsExists,
        lang_item: "fs_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsExists),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsDir,
        lang_item: "fs_is_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsIsDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsFile,
        lang_item: "fs_is_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsIsFile),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsCreateDirAll,
        lang_item: "fs_create_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsCreateDirAll),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveFile,
        lang_item: "fs_remove_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsRemoveFile),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveDirAll,
        lang_item: "fs_remove_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsRemoveDirAll),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsGlob,
        lang_item: "fs_glob",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::FsGlob),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvCurrentDir,
        lang_item: "env_current_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::EnvCurrentDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvTempDir,
        lang_item: "env_temp_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::EnvTempDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvHomeDir,
        lang_item: "env_home_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::EnvHomeDir),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVar,
        lang_item: "env_var",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::EnvVar),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVarExists,
        lang_item: "env_var_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::EnvVarExists),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathJoin,
        lang_item: "path_join",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathJoin),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathParent,
        lang_item: "path_parent",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathParent),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathFileName,
        lang_item: "path_file_name",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathFileName),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathExtension,
        lang_item: "path_extension",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathExtension),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathStem,
        lang_item: "path_stem",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathStem),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathIsAbsolute,
        lang_item: "path_is_absolute",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathIsAbsolute),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathNormalize,
        lang_item: "path_normalize",
        capability: LangInstrinsticCapability::Portable,
        call_kind: Some(IntrinsicCallKind::PathNormalize),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoReadStdinToString,
        lang_item: "io_read_stdin_to_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::IoReadStdinToString),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStdout,
        lang_item: "io_write_stdout",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::IoWriteStdout),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStderr,
        lang_item: "io_write_stderr",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: Some(IntrinsicCallKind::IoWriteStderr),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::YamlToJson,
        lang_item: "yaml_to_json",
        capability: LangInstrinsticCapability::BackendLimited,
        call_kind: Some(IntrinsicCallKind::YamlToJson),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::JsonParse,
        lang_item: "json_parse",
        capability: LangInstrinsticCapability::BackendLimited,
        call_kind: Some(IntrinsicCallKind::JsonParse),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockReset,
        lang_item: "test_command_mock_reset",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(IntrinsicCallKind::TestCommandMockReset),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockPush,
        lang_item: "test_command_mock_push",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(IntrinsicCallKind::TestCommandMockPush),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockTakeCalls,
        lang_item: "test_command_mock_take_calls",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(IntrinsicCallKind::TestCommandMockTakeCalls),
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::TestCommandMockApply,
        lang_item: "test_command_mock_apply",
        capability: LangInstrinsticCapability::InterpreterOnly,
        call_kind: Some(IntrinsicCallKind::TestCommandMockApply),
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

pub fn lang_instrinstic_call_kind(intrinsic: LangInstrinstic) -> Option<IntrinsicCallKind> {
    lang_instrinstic_spec(intrinsic).call_kind
}

pub fn lang_instrinstic_capability(intrinsic: LangInstrinstic) -> LangInstrinsticCapability {
    lang_instrinstic_spec(intrinsic).capability
}

pub fn lang_instrinstic_lang_item(intrinsic: LangInstrinstic) -> &'static str {
    lang_instrinstic_spec(intrinsic).lang_item
}
