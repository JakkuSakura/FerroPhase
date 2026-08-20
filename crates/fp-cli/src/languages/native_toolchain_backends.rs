//! `TargetBackend` impls for the LLVM/Cranelift codegen backends. Both read
//! a package's merged LIR straight off the shared `WorkspaceContext`
//! (mirroring `NativeEmitter`) rather than re-driving an independent
//! compile from source, then shell out to `clang`/`clang++` to link the
//! final binary — that final linking step stays here (an OS-toolchain
//! concern, not codegen-crate domain logic) rather than moving into
//! `fp-llvm`/`fp-cranelift` themselves.

use std::path::{Path, PathBuf};
use std::process::Command;

use fp_core::ast::path::QualifiedPath;
use fp_core::backend::TargetBackend;
use fp_core::error::{Error, Result};
use fp_core::package::PackageId;
use fp_core::workspace::WorkspaceContext;

#[cfg(feature = "llvm")]
pub struct LlvmBackend {
    pub module_path: Option<QualifiedPath>,
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub debug_info: bool,
    pub module_name: String,
    pub save_intermediates: bool,
}

#[cfg(feature = "llvm")]
impl TargetBackend for LlvmBackend {
    fn compile_package(&self, workspace: &WorkspaceContext, package_id: &PackageId) -> Result<()> {
        let entrypoint = self
            .module_path
            .as_ref()
            .map(|module_path| (module_path, "main", "main"));
        let lir = workspace.merged_lir_program(package_id, entrypoint)?;

        let llvm_output = if self.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            self.output.clone()
        } else {
            self.output.with_extension("ll")
        };
        if let Some(parent) = llvm_output.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut target = if let Some(triple) = self.target_triple.as_deref() {
            fp_llvm::target::TargetConfig::for_triple(triple)
        } else {
            fp_llvm::target::TargetConfig::default()
        };
        if let Some(cpu) = self.target_cpu.as_deref() {
            target = target.with_cpu(cpu);
        }
        if let Some(features) = self.target_features.as_deref() {
            target = target.with_features(features);
        }

        let mut linker = fp_llvm::linking::LinkerConfig::executable(&llvm_output);
        if self.release {
            linker = linker.with_size_optimization();
        }

        let config = fp_llvm::LlvmConfig::new()
            .with_target(target)
            .with_linker(linker)
            .with_debug_info(self.debug_info)
            .with_module_name(self.module_name.clone());

        let compiler = fp_llvm::LlvmCompiler::new(config);
        let (_ir_path, ir_text) = compiler
            .compile_to_string(lir, None)
            .map_err(|e| Error::from(e.to_string()))?;

        if self.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            return Ok(());
        }

        link_llvm_ir_with_clang(
            &llvm_output,
            &self.output,
            &ir_text,
            self.target_triple.as_deref(),
            self.target_sysroot.as_deref(),
            self.linker.as_deref(),
            self.target_linker.as_deref(),
            self.release,
        )?;

        if !self.save_intermediates {
            let _ = std::fs::remove_file(&llvm_output);
        }
        Ok(())
    }
}

#[cfg(feature = "cranelift")]
pub struct CraneliftBackend {
    pub module_path: Option<QualifiedPath>,
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

#[cfg(feature = "cranelift")]
impl TargetBackend for CraneliftBackend {
    fn compile_package(&self, workspace: &WorkspaceContext, package_id: &PackageId) -> Result<()> {
        let entrypoint = self
            .module_path
            .as_ref()
            .map(|module_path| (module_path, "main", "main"));
        let lir = workspace.merged_lir_program(package_id, entrypoint)?;

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
        let mut cfg = fp_cranelift::config::CraneliftConfig::object(&object_path)
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

        fp_cranelift::CraneliftEmitter::new(cfg)
            .emit(lir, None)
            .map_err(|e| Error::from(format!("fp-cranelift failed: {e}")))?;

        let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../crates/fp-cranelift/runtime/fp_cranelift_runtime.c");
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
}

#[cfg(feature = "llvm")]
fn link_llvm_ir_with_clang(
    llvm_ir_path: &Path,
    binary_path: &Path,
    llvm_ir_text: &str,
    target_triple: Option<&str>,
    sysroot: Option<&Path>,
    linker: Option<&str>,
    target_linker: Option<&Path>,
    release: bool,
) -> Result<()> {
    if let Some(parent) = binary_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let requires_eh = llvm_ir_text.contains("landingpad") || llvm_ir_text.contains("invoke");
    let default_linker = if requires_eh { "clang++" } else { "clang" };
    let linker = match linker {
        Some("clang") if requires_eh => "clang++",
        Some(other) => other,
        None => default_linker,
    };

    let mut cmd = Command::new(linker);
    cmd.arg(llvm_ir_path);
    if requires_eh {
        let runtime_path =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../../crates/fp-llvm/runtime/fp_unwind.cc");
        cmd.arg(runtime_path);
        cmd.arg("-fexceptions");
        if is_apple_target(target_triple) {
            cmd.arg("-lc++");
            cmd.arg("-lc++abi");
        }
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

    let output = cmd.output()?;
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
        return Err(Error::from(format!("clang failed: {message}")));
    }
    Ok(())
}

#[cfg(feature = "cranelift")]
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

    let output = cmd.output()?;
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
        return Err(Error::from(format!("clang failed: {message}")));
    }
    Ok(())
}

#[cfg(feature = "llvm")]
fn is_apple_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(any(target_os = "macos", target_os = "ios")),
    };
    triple.contains("apple") || triple.contains("darwin") || triple.contains("macos")
}

#[cfg(feature = "cranelift")]
fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}
