mod emit;

use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct UrclConfig {
    pub output_path: PathBuf,
}

impl UrclConfig {
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
        }
    }
}

pub struct UrclEmitter {
    config: UrclConfig,
}

impl UrclEmitter {
    pub fn new(config: UrclConfig) -> Self {
        Self { config }
    }

    pub fn emit(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;
        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
        }
        let text = emit::emit_program(&lir_program)?;
        std::fs::write(&self.config.output_path, text).map_err(fp_core::error::Error::from)?;
        Ok(self.config.output_path.clone())
    }
}
