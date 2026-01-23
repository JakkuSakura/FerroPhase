//! Backend transpilation command.
//!
//! This is a shorthand for compiling FerroPhase sources to a backend code target
//! (for example Rust). For AST-level (syntax) transpilation without backend
//! lowering, use `syntax-transpile`.

use crate::{
    Result,
    cli::CliConfig,
    commands::compile::{CompileArgs, EmitterKind, compile_command},
    pipeline::BackendKind,
};
use clap::{ArgAction, Args};
use std::path::PathBuf;

/// Arguments for the transpile command.
///
/// This command is intentionally a thin wrapper around `compile` with a
/// different default emitter.
#[derive(Debug, Clone, Args)]
pub struct TranspileArgs {
    /// Input file(s) to transpile
    #[arg(required = true)]
    pub input: Vec<PathBuf>,

    /// Emitter target (rust, llvm, bytecode, binary, interpret)
    #[arg(short = 'b', long)]
    pub backend: Option<BackendKind>,

    /// Output file or directory
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Optimization level (0, 1, 2, 3)
    #[arg(short = 'O', long, default_value_t = 2)]
    pub opt_level: u8,

    /// Enable debug information
    #[arg(short, long)]
    pub debug: bool,

    /// Treat build as release (disables debug assertions)
    #[arg(long)]
    pub release: bool,

    /// Persist intermediate representations to disk
    #[arg(long)]
    pub save_intermediates: bool,

    /// Enable lossy mode during compilation
    #[arg(long)]
    pub lossy: bool,

    /// Maximum number of errors to collect when lossy mode is enabled (0 = unlimited)
    #[arg(long, default_value_t = 50)]
    pub max_errors: usize,

    /// Override automatic source language detection (e.g. "typescript")
    #[arg(long = "lang", alias = "language")]
    pub source_language: Option<String>,

    /// Disable pipeline stages by name (repeatable).
    #[arg(long = "disable-stage", action = ArgAction::Append)]
    pub disable_stage: Vec<String>,
}

pub async fn transpile_command(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    let compile_args = CompileArgs {
        input: args.input,
        backend: args.backend.unwrap_or(BackendKind::Rust),
        emitter: EmitterKind::Llvm,
        target_triple: None,
        target_cpu: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: args.output,
        package_graph: None,
        opt_level: args.opt_level,
        debug: args.debug,
        release: args.release,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        save_intermediates: args.save_intermediates,
        lossy: args.lossy,
        max_errors: args.max_errors,
        source_language: args.source_language,
        disable_stage: args.disable_stage,
    };

    compile_command(compile_args, config).await
}
