//! Run command implementation.

use crate::commands::compile::{CompileArgs, EmitterKind, compile_command};
use crate::commands::interpret::{InterpretArgs, interpret_command};
use crate::pipeline::BackendKind;
use crate::{Result, cli::CliConfig};
use clap::{Args, ValueEnum};
use std::path::PathBuf;
use tracing::info;

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum RunMode {
    Compile,
    Interpret,
}

/// Arguments for the run command
#[derive(Debug, Clone, Args)]
pub struct RunArgs {
    /// FerroPhase file to run
    pub file: PathBuf,
    /// Execution mode (compile or interpret)
    #[arg(long, default_value = "compile")]
    pub mode: RunMode,
    /// Backend to use in compile mode
    #[arg(long, default_value = "binary")]
    pub backend: BackendKind,
    /// Codegen emitter to use in compile mode
    #[arg(long, default_value = "native")]
    pub emitter: EmitterKind,
    /// Native target override for compile mode
    #[arg(long = "native-target")]
    pub native_target: Option<String>,
    /// Explicit output artifact path in compile mode (for example `app.exe` or `app.dll`)
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    /// Optimization level in compile mode (0-3)
    #[arg(short = 'O', long, default_value_t = 2)]
    pub opt_level: u8,
    /// Enable debug information in compile mode
    #[arg(short, long)]
    pub debug: bool,
    /// Build with release settings in compile mode
    #[arg(long)]
    pub release: bool,
}

pub async fn run_command(args: RunArgs, config: &CliConfig) -> Result<()> {
    info!("Running file '{}'", args.file.display());

    crate::commands::validate_paths_exist(&[args.file.clone()], true, "run")?;

    if matches!(args.mode, RunMode::Interpret) {
        let interpret_args = InterpretArgs { input: args.file };
        return interpret_command(interpret_args, config).await;
    }

    let compile_args = CompileArgs {
        input: vec![args.file],
        backend: args.backend,
        target: None,
        emitter: args.emitter,
        target_triple: None,
        target_cpu: None,
        native_target: args.native_target,
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
        exec: true,
        save_intermediates: false,
        lossy: false,
        max_errors: 50,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(compile_args, config).await
}
