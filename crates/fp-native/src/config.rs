use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitKind {
    Object,
    Executable,
}

#[derive(Debug, Clone)]
pub struct NativeConfig {
    pub emit: EmitKind,
    pub output_path: PathBuf,
    pub linker_args: Vec<String>,
    pub keep_object: bool,
}

impl NativeConfig {
    pub fn executable(output_path: impl Into<PathBuf>) -> Self {
        Self {
            emit: EmitKind::Executable,
            output_path: output_path.into(),
            linker_args: Vec::new(),
            keep_object: false,
        }
    }

    pub fn object(output_path: impl Into<PathBuf>) -> Self {
        Self {
            emit: EmitKind::Object,
            output_path: output_path.into(),
            linker_args: Vec::new(),
            keep_object: true,
        }
    }

    pub fn with_linker_args(mut self, args: Vec<String>) -> Self {
        self.linker_args.extend(args);
        self
    }

    pub fn with_keep_object(mut self, keep: bool) -> Self {
        self.keep_object = keep;
        self
    }
}

