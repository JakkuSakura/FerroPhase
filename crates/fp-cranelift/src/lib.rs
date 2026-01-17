pub mod config;
mod codegen;

use crate::config::{CraneliftConfig, EmitKind};
use crate::codegen::CraneliftBackend;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

/// Cranelift-backed compiler entry point.
///
/// Current scope: wiring only. LIR lowering is not implemented yet.
pub struct CraneliftEmitter {
    config: CraneliftConfig,
}

impl CraneliftEmitter {
    pub fn new(config: CraneliftConfig) -> Self {
        Self { config }
    }

    /// Emit LIR into an object or executable.
    pub fn emit(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;

        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let backend = CraneliftBackend::new(&self.config)?;
        let object_bytes = backend.emit_object(&lir_program)?;

        let output = match self.config.emit {
            EmitKind::Object => self.config.output_path.clone(),
            EmitKind::Executable => self.config.output_path.clone(),
        };

        std::fs::write(&output, object_bytes)?;
        Ok(output)
    }

    /// Back-compat for older callers.
    pub fn compile(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        self.emit(lir_program, source_file)
    }
}

pub type CraneliftCompiler = CraneliftEmitter;
