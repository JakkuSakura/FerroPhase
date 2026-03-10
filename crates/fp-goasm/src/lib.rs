pub mod config;
mod emit;
mod parse;

use crate::config::{GoAsmConfig, GoAsmTarget};
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

pub use parse::parse_program;

pub struct GoAsmEmitter {
    config: GoAsmConfig,
}

impl GoAsmEmitter {
    pub fn new(config: GoAsmConfig) -> Self {
        Self { config }
    }

    pub fn emit(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;
        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
        }
        let target = self
            .config
            .target
            .unwrap_or_else(|| GoAsmTarget::resolve(self.config.target_triple.as_deref()));
        let text = emit::emit_program(&lir_program, target)?;
        std::fs::write(&self.config.output_path, text).map_err(fp_core::error::Error::from)?;
        Ok(self.config.output_path.clone())
    }
}
