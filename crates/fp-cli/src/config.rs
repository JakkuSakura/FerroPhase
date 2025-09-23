use std::collections::HashMap;
use std::path::PathBuf;

/// Configuration for pipeline execution
#[derive(Debug, Clone)]
pub struct PipelineOptions {
    /// Compilation target
    pub target: PipelineTarget,
    /// Runtime configuration
    pub runtime: RuntimeConfig,
    /// Optimization level (0-3)
    pub optimization_level: u8,
    /// Whether to save intermediate files
    pub save_intermediates: bool,
    /// Base path for output files (derived from input)
    pub base_path: Option<PathBuf>,
    /// Debug options
    pub debug: DebugOptions,
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

impl Default for PipelineOptions {
    fn default() -> Self {
        Self {
            target: PipelineTarget::Interpret,
            runtime: RuntimeConfig {
                runtime_type: "literal".to_string(),
                options: HashMap::new(),
            },
            optimization_level: 0,
            save_intermediates: false,
            base_path: None,
            debug: DebugOptions {
                print_ast: false,
                print_passes: false,
                verbose: false,
            },
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
            _ => PipelineTarget::Interpret,
        };

        Self {
            target,
            runtime: RuntimeConfig {
                runtime_type: config.runtime.clone(),
                options: HashMap::new(),
            },
            optimization_level: config.optimization_level,
            save_intermediates: true, // Default for legacy compatibility
            base_path: None,
            debug: DebugOptions {
                print_ast: config.print_ast,
                print_passes: config.print_passes,
                verbose: false,
            },
        }
    }
}
