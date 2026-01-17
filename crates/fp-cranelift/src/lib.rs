pub mod config;

use crate::config::{CraneliftConfig, EmitKind};
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
        let _ = (lir_program, source_file);

        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        match self.config.emit {
            EmitKind::Object | EmitKind::Executable => {
                Err("fp-cranelift is wired in but LIR lowering is not implemented yet")?
            }
        }
    }

    /// Back-compat for older callers.
    pub fn compile(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        self.emit(lir_program, source_file)
    }
}

pub type CraneliftCompiler = CraneliftEmitter;

