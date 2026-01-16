//! Interpret FerroPhase source or bytecode files.

use crate::pipeline::{BackendKind, PipelineOptions};
use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
use clap::Args;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Arguments for bytecode interpretation.
#[derive(Debug, Clone, Args)]
pub struct InterpretArgs {
    /// Input file to interpret (.fp, .ftbc, .fbc)
    #[arg(required = true)]
    pub input: PathBuf,
}

pub async fn interpret_command(args: InterpretArgs, _config: &CliConfig) -> Result<()> {
    let path = &args.input;
    let ext = path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
    match ext {
        "fp" => interpret_source(path).await,
        "ftbc" => interpret_text_bytecode(path),
        "fbc" => interpret_binary_bytecode(path),
        _ => Err(CliError::InvalidInput(format!(
            "Unsupported input extension: {} (expected .fp, .ftbc, or .fbc)",
            path.display()
        ))),
    }
}

async fn interpret_source(path: &Path) -> Result<()> {
    let temp_dir = TempDir::new().map_err(CliError::Io)?;
    let output = temp_dir.path().join("interpret.fbc");

    let mut options = PipelineOptions::default();
    options.target = BackendKind::Bytecode;
    options.base_path = Some(output.clone());
    options.save_intermediates = false;
    options.optimization_level = 0;

    let mut pipeline = Pipeline::new();
    let output = pipeline
        .execute_with_options(PipelineInput::File(path.to_path_buf()), options)
        .await?;

    let bytecode_path = match output {
        PipelineOutput::Binary(path) => path,
        _ => {
            return Err(CliError::Compilation(
                "Expected bytecode output from pipeline".to_string(),
            ));
        }
    };

    interpret_binary_bytecode(&bytecode_path)
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
