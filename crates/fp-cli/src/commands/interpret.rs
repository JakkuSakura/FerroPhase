//! Interpret FerroPhase source or bytecode files.

use crate::commands::compile::build_module_resolution_context;
use crate::pipeline::{BackendKind, PipelineOptions};
use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
use clap::Args;
use fp_jit::JitOptions;
use std::path::{Path, PathBuf};

/// Arguments for bytecode interpretation.
#[derive(Debug, Clone, Args)]
pub struct InterpretArgs {
    /// Input file to interpret (.fp, .ftbc, .fbc)
    #[arg(required = true)]
    pub input: PathBuf,
    /// Path to a workspace graph (JSON) for dependency resolution
    #[arg(long = "graph")]
    pub graph: Option<PathBuf>,
    /// Enable the JIT for interpreter execution
    #[arg(long)]
    pub jit: bool,
    /// Hot call threshold before JIT compilation
    #[arg(long, requires = "jit")]
    pub jit_hot_threshold: Option<u32>,
}

pub async fn interpret_command(args: InterpretArgs, _config: &CliConfig) -> Result<()> {
    crate::commands::validate_paths_exist(&[args.input.clone()], true, "interpret")?;
    if let Some(graph) = args.graph.as_ref() {
        crate::commands::validate_paths_exist(&[graph.clone()], true, "interpret")?;
    }
    let path = &args.input;
    let ext = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
    match ext {
        "fp" => interpret_source(path, &args).await,
        "ftbc" => interpret_text_bytecode(path),
        "fbc" => interpret_binary_bytecode(path),
        _ => Err(CliError::InvalidInput(format!(
            "Unsupported input extension: {} (expected .fp, .ftbc, or .fbc)",
            path.display()
        ))),
    }
}

async fn interpret_source(path: &Path, args: &InterpretArgs) -> Result<()> {
    let mut options = PipelineOptions::default();
    options.target = BackendKind::Interpret;
    options.save_intermediates = false;
    options.optimization_level = 0;
    if let Some(graph) = args.graph.as_ref() {
        options.module_resolution = Some(build_module_resolution_context(graph, path)?);
    }
    if args.jit {
        let mut jit_options = JitOptions::default();
        jit_options.enabled = true;
        if let Some(threshold) = args.jit_hot_threshold {
            jit_options.hot_threshold = threshold;
        }
        options.jit = Some(jit_options);
    }

    let mut pipeline = Pipeline::new();
    let output = pipeline
        .execute_with_options(PipelineInput::File(path.to_path_buf()), options)
        .await?;

    match output {
        PipelineOutput::Value(_) | PipelineOutput::RuntimeValue(_) => Ok(()),
        PipelineOutput::Code(_) | PipelineOutput::Binary(_) => Err(CliError::Compilation(
            "Expected interpret output from pipeline".to_string(),
        )),
    }
}

fn interpret_text_bytecode(path: &Path) -> Result<()> {
    let text = std::fs::read_to_string(path).map_err(CliError::Io)?;
    let program = fp_bytecode::parse_program(&text)
        .map_err(|err| CliError::Compilation(format!("Failed to parse text bytecode: {}", err)))?;
    run_bytecode_program(program)
}

fn interpret_binary_bytecode(path: &Path) -> Result<()> {
    let bytes = std::fs::read(path).map_err(CliError::Io)?;
    let file = fp_bytecode::decode_file(&bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to decode bytecode: {}", err)))?;
    run_bytecode_program(file.program)
}

fn run_bytecode_program(program: fp_bytecode::BytecodeProgram) -> Result<()> {
    let vm = fp_stackvm::Vm::new(program);
    vm.run_main()
        .map_err(|err| CliError::Compilation(format!("Bytecode interpretation failed: {}", err)))?;
    Ok(())
}
