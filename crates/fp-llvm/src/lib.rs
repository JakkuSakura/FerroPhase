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

    /// Allow unresolved globals during codegen (bootstrap fallback)
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
            .init_target_machine()
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
            .init_target_machine()
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
