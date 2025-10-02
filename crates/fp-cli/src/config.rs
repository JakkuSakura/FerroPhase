use std::collections::HashMap;
use std::path::PathBuf;

/// Configuration for pipeline execution
#[derive(Debug, Clone)]
pub struct PipelineOptions {
    /// Compilation target
    pub target: PipelineTarget,
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
}

/// Compilation targets
#[derive(Debug, Clone)]
pub enum PipelineTarget {
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
            enabled: false, // Disabled by default for backward compatibility
            max_errors: 10,
            show_all_errors: false,
            continue_on_error: true,
        }
    }
}

impl Default for PipelineOptions {
    fn default() -> Self {
        Self {
            target: PipelineTarget::Interpret,
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
        }
    }
}

/// Legacy pipeline configuration
#[derive(Debug)]
pub struct PipelineConfig {
    pub optimization_level: u8,
    pub print_ast: bool,
    pub print_passes: bool,
    pub target: String,
    pub runtime: String,
}

impl From<&PipelineConfig> for PipelineOptions {
    fn from(config: &PipelineConfig) -> Self {
        let target = match config.target.as_str() {
            "rust" => PipelineTarget::Rust,
            "llvm" => PipelineTarget::Llvm,
            "binary" => PipelineTarget::Binary,
            "bytecode" => PipelineTarget::Bytecode,
            _ => PipelineTarget::Interpret,
        };

        Self {
            target,
            runtime: RuntimeConfig {
                runtime_type: config.runtime.clone(),
                options: HashMap::new(),
            },
            source_language: None,
            optimization_level: config.optimization_level,
            save_intermediates: true, // Default for legacy compatibility
            base_path: None,
            debug: DebugOptions {
                print_ast: config.print_ast,
                print_passes: config.print_passes,
                verbose: false,
            },
            error_tolerance: ErrorToleranceOptions::default(),
            release: false,
            execute_main: false,
        }
    }
}
