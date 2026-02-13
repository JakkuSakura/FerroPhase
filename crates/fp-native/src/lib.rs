pub mod config;
pub mod emit;
pub mod ffi;
pub mod jit;
pub mod link;

use crate::config::{EmitKind, NativeConfig};
use crate::emit::detect_target;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

/// Native (LLVM-free) compiler entry point.
///
/// Current scope: minimal native backend that can emit a tiny binary stub for
/// Mach-O/ELF/PE targets, then link it into an executable in-process.
///
/// This is intended as an incremental replacement for `fp-llvm`.
pub struct NativeEmitter {
    config: NativeConfig,
}

impl NativeEmitter {
    pub fn new(config: NativeConfig) -> Self {
        Self { config }
    }

    /// Emit LIR into an object or executable.
    pub fn emit(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;

        // Ensure output directory exists.
        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
        }

        self.emit_impl(&lir_program)
    }

    /// Back-compat for older callers.
    pub fn compile(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        self.emit(lir_program, source_file)
    }

    fn emit_impl(&self, lir_program: &LirProgram) -> Result<PathBuf> {
        let out = self.config.output_path.clone();

        let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

        let plan = emit::emit_plan(lir_program, format, arch)?;
        if let Some(path) = self.config.asm_dump.as_ref() {
            emit::dump_asm(path, &plan)?;
        }

        match self.config.emit {
            EmitKind::Object => emit::write_object(&out, &plan)?,
            EmitKind::Executable => emit::write_executable(&out, &plan)?,
        }
        Ok(out)
    }
}

pub type NativeCompiler = NativeEmitter;
