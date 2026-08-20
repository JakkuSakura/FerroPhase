use clap::ValueEnum;
use fp_jit::JitOptions;
use std::collections::HashMap;
use std::path::PathBuf;

/// Configuration for compilation-style execution.
#[derive(Debug, Clone)]
pub struct PipelineOptions {
    pub target: BackendKind,
    pub backend: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub native_target: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub runtime: RuntimeConfig,
    pub source_language: Option<String>,
    pub optimization_level: u8,
    pub save_intermediates: bool,
    pub base_path: Option<PathBuf>,
    pub debug: DebugOptions,
    pub lossy: LossyOptions,
    pub release: bool,
    pub execute_main: bool,
    pub disabled_stages: Vec<String>,
    pub jit: Option<JitOptions>,
}

/// The fixed set of built-in `--backend` kinds. Unlike `--target` (which
/// resolves through `crate::languages::registry` too, so an externally
/// registered `TargetBackend` can be selected by name at runtime), this is
/// still a closed `clap::ValueEnum` — `--backend` can't yet name a
/// runtime-registered backend the way `--target` can. Closing that gap
/// (letting `--backend <name>` also fall back to the registry, the same
/// way `resolve_compile_target` already does for `--target`) is deliberately
/// deferred, not an oversight: it needs `--backend` to stop being a fixed
/// `ValueEnum` first, which ripples into every `CompileArgs` construction
/// site (tests included).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum BackendKind {
    Interpret,
    Rust,
    Llvm,
    Binary,
    Ebpf,
    Cil,
    Dotnet,
    Bytecode,
    TextBytecode,
    JvmBytecode,
    Wasm,
}

impl BackendKind {
    pub fn as_str(self) -> &'static str {
        match self {
            BackendKind::Binary => "binary",
            BackendKind::Ebpf => "ebpf",
            BackendKind::Cil => "cil",
            BackendKind::Dotnet => "dotnet",
            BackendKind::Rust => "rust",
            BackendKind::Llvm => "llvm",
            BackendKind::Wasm => "wasm",
            BackendKind::Bytecode => "bytecode",
            BackendKind::TextBytecode => "text-bytecode",
            BackendKind::JvmBytecode => "jvm-bytecode",
            BackendKind::Interpret => "interpret",
        }
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeConfig {
    pub runtime_type: String,
    pub options: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct DebugOptions {
    pub print_ast: bool,
    pub print_passes: bool,
    pub verbose: bool,
}

#[derive(Debug, Clone)]
pub struct LossyOptions {
    pub enabled: bool,
    pub max_errors: usize,
    pub show_all_errors: bool,
    pub continue_on_error: bool,
}

impl Default for LossyOptions {
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
            native_target: None,
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
            lossy: LossyOptions::default(),
            release: false,
            execute_main: false,
            disabled_stages: Vec::new(),
            jit: None,
        }
    }
}
