use clap::ValueEnum;
use std::collections::HashMap;
use std::path::PathBuf;

/// Configuration for pipeline execution
#[derive(Debug, Clone)]
pub struct PipelineOptions {
    /// Compilation target
    pub target: BackendKind,
    /// Code generation backend to use when producing native artifacts (e.g. "llvm", "native").
    pub backend: Option<String>,
    /// Target triple for codegen (defaults to host when unset)
    pub target_triple: Option<String>,
    /// Target CPU for codegen (optional)
    pub target_cpu: Option<String>,
    /// Target feature string for codegen (optional)
    pub target_features: Option<String>,
    /// Target sysroot for linking (optional)
    pub target_sysroot: Option<PathBuf>,
    /// Linker driver to invoke (e.g. "clang", "clang++", "gcc", "ld"). Defaults to clang.
    pub linker: Option<String>,
    /// Explicit linker override for target (optional)
    pub target_linker: Option<PathBuf>,
    /// Runtime configuration
    pub runtime: RuntimeConfig,
    /// Explicit source language override (defaults to detection)
    pub source_language: Option<String>,
    /// Optimization level (0-3)
    pub optimization_level: u8,
    /// Whether to save intermediate files
    pub save_intermediates: bool,
    /// Base path for output files (derived from input)
    pub base_path: Option<PathBuf>,
    /// Debug options
    pub debug: DebugOptions,
    /// Error tolerance options
    pub error_tolerance: ErrorToleranceOptions,
    /// Whether the current build is in release mode
    pub release: bool,
    /// Execute `main` during const evaluation instead of running backend
    pub execute_main: bool,
    /// Disable specific pipeline stages by name (best-effort; mainly for debugging).
    pub disabled_stages: Vec<String>,
}

/// Compilation targets
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum BackendKind {
    /// Interpret the code directly
    Interpret,
    /// Generate Rust source code
    Rust,
    /// Generate LLVM IR
    Llvm,
    /// Generate binary executable
    Binary,
    /// Generate bytecode for the virtual machine backend
    Bytecode,
    /// Generate human-readable bytecode text
    TextBytecode,
    Wasm,
}

impl BackendKind {
    pub fn as_str(self) -> &'static str {
        match self {
            BackendKind::Binary => "binary",
            BackendKind::Rust => "rust",
            BackendKind::Llvm => "llvm",
            BackendKind::Wasm => "wasm",
            BackendKind::Bytecode => "bytecode",
            BackendKind::TextBytecode => "text-bytecode",
            BackendKind::Interpret => "interpret",
        }
    }
}

/// Runtime configuration
#[derive(Debug, Clone)]
pub struct RuntimeConfig {
    /// Runtime type ("literal", "rust")
    pub runtime_type: String,
    /// Runtime-specific options
    pub options: HashMap<String, String>,
}

/// Debug options
#[derive(Debug, Clone)]
pub struct DebugOptions {
    /// Print AST during compilation
    pub print_ast: bool,
    /// Print optimization passes
    pub print_passes: bool,
    /// Enable verbose output
    pub verbose: bool,
}

/// Error tolerance configuration
#[derive(Debug, Clone)]
pub struct ErrorToleranceOptions {
    /// Enable error tolerance mode (collect multiple errors instead of early exit)
    pub enabled: bool,
    /// Maximum number of errors to collect before giving up (0 = unlimited)
    pub max_errors: usize,
    /// Show all errors vs progressive disclosure
    pub show_all_errors: bool,
    /// Continue compilation through non-fatal errors
    pub continue_on_error: bool,
}

impl Default for ErrorToleranceOptions {
    fn default() -> Self {
        Self {
            enabled: false,
            max_errors: 10,
            show_all_errors: false,
            continue_on_error: true,
        }
    }
}

impl Default for PipelineOptions {
    fn default() -> Self {
        Self {
            target: BackendKind::Interpret,
            backend: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            target_sysroot: None,
            linker: None,
            target_linker: None,
            runtime: RuntimeConfig {
                runtime_type: "literal".to_string(),
                options: HashMap::new(),
            },
            source_language: None,
            optimization_level: 0,
            save_intermediates: false,
            base_path: None,
            debug: DebugOptions {
                print_ast: false,
                print_passes: false,
                verbose: false,
            },
            error_tolerance: ErrorToleranceOptions::default(),
            release: false,
            execute_main: false,
            disabled_stages: Vec::new(),
        }
    }
}
