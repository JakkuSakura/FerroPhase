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
    ProcessRun,
    ProcessOk,
    ProcessOutput,
    ProcessStatus,
    ProcessRunArgv,
    ProcessOkArgv,
    ProcessOutputArgv,
    ProcessStatusArgv,
    ProcessRunArgvIn,
    ProcessOkArgvIn,
    ProcessOutputArgvIn,
    ProcessStatusArgvIn,
    YamlToJson,
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
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsWalkDir,
        lang_item: "fs_walk_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
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
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsAppendString,
        lang_item: "fs_append_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsExists,
        lang_item: "fs_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsDir,
        lang_item: "fs_is_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsIsFile,
        lang_item: "fs_is_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsCreateDirAll,
        lang_item: "fs_create_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveFile,
        lang_item: "fs_remove_file",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsRemoveDirAll,
        lang_item: "fs_remove_dir_all",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::FsGlob,
        lang_item: "fs_glob",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvCurrentDir,
        lang_item: "env_current_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvTempDir,
        lang_item: "env_temp_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvHomeDir,
        lang_item: "env_home_dir",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVar,
        lang_item: "env_var",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::EnvVarExists,
        lang_item: "env_var_exists",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathJoin,
        lang_item: "path_join",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathParent,
        lang_item: "path_parent",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathFileName,
        lang_item: "path_file_name",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathExtension,
        lang_item: "path_extension",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathStem,
        lang_item: "path_stem",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathIsAbsolute,
        lang_item: "path_is_absolute",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::PathNormalize,
        lang_item: "path_normalize",
        capability: LangInstrinsticCapability::Portable,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoReadStdinToString,
        lang_item: "io_read_stdin_to_string",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStdout,
        lang_item: "io_write_stdout",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::IoWriteStderr,
        lang_item: "io_write_stderr",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessRun,
        lang_item: "process_run",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOk,
        lang_item: "process_ok",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOutput,
        lang_item: "process_output",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessStatus,
        lang_item: "process_status",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessRunArgv,
        lang_item: "process_run_argv",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOkArgv,
        lang_item: "process_ok_argv",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOutputArgv,
        lang_item: "process_output_argv",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessStatusArgv,
        lang_item: "process_status_argv",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessRunArgvIn,
        lang_item: "process_run_argv_in",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOkArgvIn,
        lang_item: "process_ok_argv_in",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessOutputArgvIn,
        lang_item: "process_output_argv_in",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::ProcessStatusArgvIn,
        lang_item: "process_status_argv_in",
        capability: LangInstrinsticCapability::RuntimeOnly,
        call_kind: None,
    },
    LangInstrinsticSpec {
        intrinsic: LangInstrinstic::YamlToJson,
        lang_item: "yaml_to_json",
        capability: LangInstrinsticCapability::BackendLimited,
        call_kind: None,
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
