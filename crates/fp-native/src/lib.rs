pub mod abi;
pub mod archive;
pub mod asm;
pub mod asmir;
pub mod binary;
pub mod config;
pub mod container;
pub mod emit;
pub mod ffi;
pub mod intrinsic_materializer;
pub mod jit;
pub mod libc;
pub mod link;
pub mod system_api;

use crate::config::{EmitKind, NativeConfig};
use crate::emit::{detect_target, resolve_native_target};
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

pub use crate::intrinsic_materializer::NativeIntrinsicMaterializer;
pub use crate::jit::{
    HostScalar, JitEngine, JitModule, validate_host_program, validate_native_program,
};

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
}

/// `NativeEmitter` already carries its own fully-resolved output path via
/// `NativeConfig` (constructed by the caller with the exact artifact path
/// before the emitter itself), so unlike the AST-emitting backends this
/// doesn't need a separate `BackendConfig` — the existing config already is
/// the "where to write" state `TargetBackend`'s design calls for.
impl fp_core::backend::TargetBackend for NativeEmitter {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> Result<()> {
        let lir = workspace.merged_lir_program(package_id)?;
        self.emit(lir, None)?;
        Ok(())
    }

    fn exec(&self) -> Result<()> {
        let path = &self.config.output_path;
        let status = std::process::Command::new(path)
            .status()
            .map_err(|e| fp_core::error::Error::from(format!("failed to execute '{}': {e}", path.display())))?;
        if !status.success() {
            let code = status.code().unwrap_or(-1);
            return Err(fp_core::error::Error::from(format!(
                "process exited with status {code}"
            )));
        }
        Ok(())
    }
}

impl NativeEmitter {
    fn emit_impl(&self, lir_program: &LirProgram) -> Result<PathBuf> {
        let out = self.config.output_path.clone();
        resolve_native_target(
            self.config.native_target,
            self.config.target_triple.as_deref(),
        )?;

        let (format, arch) = detect_target(self.config.target_triple.as_deref())?;

        let plan = emit::emit_plan(lir_program, format, arch)?;
        if let Some(path) = self.config.asm_dump.as_ref() {
            emit::dump_asm(path, &plan)?;
        }

        match self.config.emit {
            EmitKind::Object => emit::write_object(&out, &plan)?,
            EmitKind::Executable => emit::write_executable(&out, &plan)?,
            EmitKind::AssemblyText => {
                return Err(fp_core::error::Error::from(
                    "fp-native does not support textual assembly emission",
                ));
            }
        }
        Ok(out)
    }
}

pub type NativeCompiler = NativeEmitter;
