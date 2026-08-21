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
use fp_core::ast::path::QualifiedPath;
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
    /// The module a `main` entrypoint should be resolved and renamed from
    /// when driven through `TargetBackend::compile_package` (see
    /// `fp_core::workspace::WorkspaceContext::merged_lir_program`) — `None`
    /// for callers (tests, `fp-cli`'s container pipeline) that drive
    /// `emit`/`compile` directly with an already-flattened `LirProgram`
    /// and have no module to resolve one from.
    module_path: Option<QualifiedPath>,
}

impl NativeEmitter {
    pub fn new(config: NativeConfig) -> Self {
        Self {
            config,
            module_path: None,
        }
    }

    pub fn with_module_path(mut self, module_path: QualifiedPath) -> Self {
        self.module_path = Some(module_path);
        self
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
        let entrypoint = self
            .module_path
            .as_ref()
            .map(|module_path| (module_path, "main", "main"));
        let lir = workspace.merged_lir_program(package_id, entrypoint)?;
        self.emit(lir, None)?;
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
