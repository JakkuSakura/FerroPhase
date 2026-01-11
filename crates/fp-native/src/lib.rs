pub mod config;
pub mod emitter;
pub mod link;

use crate::config::{EmitKind, NativeConfig};
use crate::emitter::emit_object_macho_minimal;
use crate::link::link_with_clang;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

/// Native (LLVM-free) compiler entry point.
///
/// Current scope: macOS-only prototype backend that can emit a minimal Mach-O object
/// containing a program entry that returns 0, then link it into an executable
/// via the system toolchain.
///
/// This is intended as an incremental replacement for `fp-llvm`.
pub struct NativeCompiler {
    config: NativeConfig,
}

impl NativeCompiler {
    pub fn new(config: NativeConfig) -> Self {
        Self { config }
    }

    /// Compile LIR into an object or executable.
    ///
    /// Note: this initial implementation ignores most of `lir_program`; it is a
    /// plumbing + format prototype.
    pub fn compile(&self, _lir_program: LirProgram, _source_file: Option<&Path>) -> Result<PathBuf> {
        let out = self.config.output_path.clone();
        match self.config.emit {
            EmitKind::Object => {
                emit_object_macho_minimal(&out)?;
                Ok(out)
            }
            EmitKind::Executable => {
                let obj_path = out.with_extension("o");
                emit_object_macho_minimal(&obj_path)?;
                link_with_clang(&obj_path, &out, &self.config.linker_args)?;
                if !self.config.keep_object {
                    let _ = std::fs::remove_file(&obj_path);
                }
                Ok(out)
            }
        }
    }
}

