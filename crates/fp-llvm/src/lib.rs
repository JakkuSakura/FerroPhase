pub mod llvm;
pub use llvm::{codegen, context, intrinsics};
pub mod debug_info;
pub mod linking;
pub mod pretty;
pub mod runtime;
pub mod runtime_symbols;
pub mod target;

use crate::codegen::LirCodegen;
use crate::context::LlvmContext;
use crate::debug_info::DebugInfoBuilder;
use crate::linking::LinkerConfig;
use crate::target::{TargetCodegen, TargetConfig};
use anyhow::Context as AnyhowContext;
use fp_core::diagnostics::report_error;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

/// Configuration for LLVM compilation
#[derive(Debug, Clone)]
pub struct LlvmConfig {
    pub target: TargetConfig,
    pub linker: LinkerConfig,
    pub enable_debug_info: bool,
    pub producer_name: String,
    pub module_name: String,
    pub allow_unresolved_globals: bool,
}

impl Default for LlvmConfig {
    fn default() -> Self {
        Self {
            target: TargetConfig::default(),
            linker: LinkerConfig::default(),
            enable_debug_info: true,
            producer_name: "fp-compiler".to_string(),
            module_name: "main".to_string(),
            allow_unresolved_globals: false,
        }
    }
}

impl LlvmConfig {
    /// Create a new LLVM config with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Configure for executable output
    pub fn executable(output_path: impl Into<PathBuf>) -> Self {
        Self {
            linker: LinkerConfig::executable(output_path),
            ..Self::default()
        }
    }

    /// Configure for static library output
    pub fn static_library(output_path: impl Into<PathBuf>) -> Self {
        Self {
            linker: LinkerConfig::static_library(output_path),
            ..Self::default()
        }
    }

    /// Configure for dynamic library output
    pub fn dynamic_library(output_path: impl Into<PathBuf>) -> Self {
        Self {
            linker: LinkerConfig::dynamic_library(output_path),
            ..Self::default()
        }
    }

    /// Set target configuration
    pub fn with_target(mut self, target: TargetConfig) -> Self {
        self.target = target;
        self
    }

    /// Set linker configuration
    pub fn with_linker(mut self, linker: LinkerConfig) -> Self {
        self.linker = linker;
        self
    }

    /// Enable or disable debug information
    pub fn with_debug_info(mut self, enable: bool) -> Self {
        self.enable_debug_info = enable;
        self
    }

    /// Set the producer name for debug info
    pub fn with_producer(mut self, producer: impl Into<String>) -> Self {
        self.producer_name = producer.into();
        self
    }

    /// Set the module name
    pub fn with_module_name(mut self, name: impl Into<String>) -> Self {
        self.module_name = name.into();
        self
    }

    /// Allow unresolved globals during codegen (fallback).
    pub fn with_allow_unresolved_globals(mut self, allow: bool) -> Self {
        self.allow_unresolved_globals = allow;
        self
    }
}

/// Main LLVM compilation interface
pub struct LlvmCompiler {
    config: LlvmConfig,
}

impl LlvmCompiler {
    /// Create a new LLVM compiler with the given configuration
    pub fn new(config: LlvmConfig) -> Self {
        Self { config }
    }

    /// Compile a LIR program to native code (generates LLVM IR for now)
    pub fn compile(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        // Create LLVM context
        let mut llvm_ctx = LlvmContext::new(&self.config.module_name);

        // Initialize target machine
        llvm_ctx
            .init_target_machine_with_config(&self.config.target)
            .map_err(fp_core::error::Error::from)?;

        // Create target codegen
        let _target_codegen = TargetCodegen::new(self.config.target.clone())
            .with_context(|| "Failed to create target codegen")
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        // Create debug info builder if enabled
        let debug_builder = if self.config.enable_debug_info {
            let source_path = source_file.unwrap_or_else(|| Path::new("unknown.fp"));
            Some(
                DebugInfoBuilder::new(&llvm_ctx.module, source_path, &self.config.producer_name)
                    .with_context(|| "Failed to create debug info builder")
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
            )
        } else {
            None
        };
        let mut global_map = std::collections::HashMap::new();
        for global in &lir_program.globals {
            let initializer = global.initializer.clone().ok_or_else(|| {
                report_error(format!(
                    "[lir→llvm] Global '{}' is missing an initializer before LLVM codegen",
                    global.name
                ))
            })?;
            global_map.insert(String::from(global.name.clone()), initializer);
        }
        let mut codegen = LirCodegen::new(
            &mut llvm_ctx,
            global_map,
            self.config.allow_unresolved_globals,
        );

        if let Err(err) = codegen.generate_program(lir_program) {
            return Err(fp_core::error::Error::from(format!(
                "LIR→LLVM codegen failed: {}",
                err
            )));
        }

        tracing::debug!(
            "LLVM module contains {} functions and {} globals",
            llvm_ctx.module.get_functions().count(),
            llvm_ctx.module.get_globals().count()
        );

        // Finalize debug info
        if let Some(ref debug_info) = debug_builder {
            debug_info.finalize();
        }

        // Verify the module
        llvm_ctx.verify_module().map_err(|e| {
            tracing::error!("[fp-llvm] module verification failed: {}", e);
            fp_core::error::Error::from(format!("LLVM module verification failed: {}", e))
        })?;

        // Persist LLVM IR to file for downstream tools (llc/clang)
        let output_path = self.config.linker.output_path.clone();
        llvm_ctx
            .write_to_file(&output_path)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        // TODO: Once native object emission is supported, reinstate ModuleLinker
        // and target codegen to produce object files directly.

        Ok(output_path)
    }

    /// Compile and return the LLVM IR text along with the output path, avoiding
    /// reading the file back from disk.
    pub fn compile_to_string(
        &self,
        lir_program: LirProgram,
        source_file: Option<&Path>,
    ) -> Result<(PathBuf, String)> {
        // Create LLVM context
        let mut llvm_ctx = LlvmContext::new(&self.config.module_name);

        // Initialize target machine
        llvm_ctx
            .init_target_machine_with_config(&self.config.target)
            .map_err(fp_core::error::Error::from)?;

        // Create target codegen
        let _target_codegen = TargetCodegen::new(self.config.target.clone())
            .with_context(|| "Failed to create target codegen")
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        // Create debug info builder if enabled
        let debug_builder = if self.config.enable_debug_info {
            let source_path = source_file.unwrap_or_else(|| Path::new("unknown.fp"));
            Some(
                DebugInfoBuilder::new(&llvm_ctx.module, source_path, &self.config.producer_name)
                    .with_context(|| "Failed to create debug info builder")
                    .map_err(|e| fp_core::error::Error::from(e.to_string()))?,
            )
        } else {
            None
        };
        let mut global_map = std::collections::HashMap::new();
        for global in &lir_program.globals {
            let initializer = global.initializer.clone().ok_or_else(|| {
                report_error(format!(
                    "[lir→llvm] Global '{}' is missing an initializer before LLVM codegen",
                    global.name
                ))
            })?;
            global_map.insert(String::from(global.name.clone()), initializer);
        }
        let mut codegen = LirCodegen::new(
            &mut llvm_ctx,
            global_map,
            self.config.allow_unresolved_globals,
        );

        codegen
            .generate_program(lir_program)
            .with_context(|| "Failed to generate LLVM IR from LIR")
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        // Finalize debug info
        if let Some(ref debug_info) = debug_builder {
            debug_info.finalize();
        }

        // Verify the module
        llvm_ctx.verify_module().map_err(|e| {
            tracing::error!("[fp-llvm] module verification failed: {}", e);
            fp_core::error::Error::from(e.to_string())
        })?;

        // Obtain IR text in-memory
        let ir_text = llvm_ctx.print_to_string();

        // Also write it to file for downstream linkers
        let output_path = self.config.linker.output_path.clone();
        llvm_ctx
            .write_to_file(&output_path)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        Ok((output_path, ir_text))
    }

    /// Get the configuration
    pub fn config(&self) -> &LlvmConfig {
        &self.config
    }
}

/// Check if LLVM backend is available
pub fn is_available() -> bool {
    // For now, always return true since inkwell is configured in Cargo.
    true
}

/// `TargetBackend` for the `llvm-binary`/`llvm-text` targets. Reads a
/// package's merged LIR straight off the shared `WorkspaceContext`
/// (mirroring `fp_native::NativeEmitter`) rather than re-driving an
/// independent compile from source, then — unless `text_only` — shells out
/// to `clang`/`clang++` to link the final binary. That final linking step
/// lives here (an OS-toolchain concern) rather than in `fp-cli`, so
/// `fp-cli` has zero knowledge of how this target turns LIR into output.
pub struct LlvmBackend {
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub debug_info: bool,
    pub save_intermediates: bool,
    /// `llvm-text`: always stop after writing the `.ll` file, never link.
    pub text_only: bool,
}

impl fp_core::backend::TargetBackend for LlvmBackend {
    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::workspace::WorkspaceContext,
        package_id: &fp_core::ast::package::PackageId,
    ) -> Result<()> {
        let lir = workspace.merged_lir_program(package_id)?;

        let llvm_output = if self.text_only || self.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
            self.output.clone()
        } else {
            self.output.with_extension("ll")
        };
        if let Some(parent) = llvm_output.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut target = if let Some(triple) = self.target_triple.as_deref() {
            target::TargetConfig::for_triple(triple)
        } else {
            target::TargetConfig::default()
        };
        if let Some(cpu) = self.target_cpu.as_deref() {
            target = target.with_cpu(cpu);
        }
        if let Some(features) = self.target_features.as_deref() {
            target = target.with_features(features);
        }

        let mut linker = linking::LinkerConfig::executable(&llvm_output);
        if self.release {
            linker = linker.with_size_optimization();
        }

        let config = LlvmConfig::new()
            .with_target(target)
            .with_linker(linker)
            .with_debug_info(self.debug_info)
            .with_module_name(package_id.as_str());

        let compiler = LlvmCompiler::new(config);
        let (_ir_path, ir_text) = compiler
            .compile_to_string(lir, None)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        if self.text_only || self.output.extension().and_then(|ext| ext.to_str()) == Some("ll") {
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

    fn exec(&self) -> Result<()> {
        if self.text_only {
            return Err(fp_core::error::Error::from(
                "--exec is not supported for `llvm-text` output".to_string(),
            ));
        }
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
    use std::process::Command;

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
        let runtime_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("runtime/fp_unwind.cc");
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

fn is_apple_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(any(target_os = "macos", target_os = "ios")),
    };
    triple.contains("apple") || triple.contains("darwin") || triple.contains("macos")
}
