mod codegen;
pub mod config;

use crate::codegen::CraneliftBackend as CraneliftCodegenBackend;
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
        let _ = source_file;

        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let backend = CraneliftCodegenBackend::new(&self.config)?;
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

/// `TargetBackend` for the `cranelift` target. Reads a package's merged LIR
/// straight off the shared `WorkspaceContext` (mirroring
/// `fp_native::NativeEmitter`) rather than re-driving an independent
/// compile from source, then shells out to `clang`/`clang++` to link the
/// final binary — that final linking step lives here (an OS-toolchain
/// concern) rather than in `fp-cli`.
pub struct CraneliftBackend {
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub save_intermediates: bool,
}

impl fp_core::backend::TargetBackend for CraneliftBackend {
    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::workspace::WorkspaceContext,
        package_id: &fp_core::ast::package::PackageId,
    ) -> Result<()> {
        let lir = workspace.merged_lir_program(package_id)?;

        let object_path = self
            .output
            .with_extension(if is_windows_target(self.target_triple.as_deref()) {
                "obj"
            } else {
                "o"
            });
        if let Some(parent) = object_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let mut cfg = config::CraneliftConfig::object(&object_path)
            .with_target_triple(self.target_triple.clone())
            .with_target_cpu(self.target_cpu.clone())
            .with_target_features(self.target_features.clone())
            .with_sysroot(self.target_sysroot.clone())
            .with_linker_driver(self.linker.clone())
            .with_fuse_ld(self.target_linker.clone())
            .with_release(self.release)
            .with_keep_object(self.save_intermediates);
        if self.save_intermediates {
            cfg = cfg.with_asm_dump(Some(self.output.with_extension("clif")));
        }

        CraneliftEmitter::new(cfg)
            .emit(lir, None)
            .map_err(|e| fp_core::error::Error::from(format!("fp-cranelift failed: {e}")))?;

        let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/fp_cranelift_runtime.c");
        link_object_with_clang(
            &object_path,
            &self.output,
            self.target_triple.as_deref(),
            self.target_sysroot.as_deref(),
            self.linker.as_deref(),
            self.target_linker.as_deref(),
            self.release,
            &[runtime_path],
        )?;

        if !self.save_intermediates {
            let _ = std::fs::remove_file(&object_path);
        }
        Ok(())
    }

    fn exec(&self) -> Result<()> {
        let status = std::process::Command::new(&self.output).status().map_err(|e| {
            fp_core::error::Error::from(format!("failed to execute '{}': {e}", self.output.display()))
        })?;
        if !status.success() {
            let code = status.code().unwrap_or(-1);
            return Err(fp_core::error::Error::from(format!(
                "process exited with status {code}"
            )));
        }
        Ok(())
    }
}

fn link_object_with_clang(
    object_path: &Path,
    binary_path: &Path,
    target_triple: Option<&str>,
    sysroot: Option<&Path>,
    linker: Option<&str>,
    target_linker: Option<&Path>,
    release: bool,
    extra_inputs: &[PathBuf],
) -> Result<()> {
    use std::process::Command;

    if let Some(parent) = binary_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let linker = linker.unwrap_or("clang");
    let mut cmd = Command::new(linker);
    cmd.arg(object_path);
    for input in extra_inputs {
        cmd.arg(input);
    }
    if !extra_inputs.is_empty() {
        cmd.arg("-lm");
    }
    if let Some(target_triple) = target_triple {
        cmd.arg("--target").arg(target_triple);
    }
    if let Some(sysroot) = sysroot {
        cmd.arg("--sysroot").arg(sysroot);
    }
    if let Some(linker_path) = target_linker {
        cmd.arg(format!("-fuse-ld={}", linker_path.display()));
    }
    cmd.arg("-o").arg(binary_path);
    if release {
        cmd.arg("-O2");
    }

    let output = cmd.output().map_err(fp_core::error::Error::from)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut message = stderr.trim().to_string();
        if message.is_empty() {
            message = stdout.trim().to_string();
        }
        if message.is_empty() {
            message = "clang failed without diagnostics".to_string();
        }
        return Err(fp_core::error::Error::from(format!("clang failed: {message}")));
    }
    Ok(())
}

fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}
