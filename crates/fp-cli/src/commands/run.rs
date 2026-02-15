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
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: None,
        target_cpu: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: None,
        package_graph: None,
        opt_level: 2,
        debug: false,
        release: false,
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
