//! Interpret bytecode files.

use crate::{CliError, Result, cli::CliConfig};
use clap::Args;
use std::path::{Path, PathBuf};

/// Arguments for bytecode interpretation.
#[derive(Debug, Clone, Args)]
pub struct InterpretArgs {
    /// Input file to interpret (.ftbc, .fbc)
    #[arg(required = true)]
    pub input: PathBuf,
    /// Path to a workspace graph (JSON) for dependency resolution
    #[arg(long = "graph")]
    pub graph: Option<PathBuf>,
    /// Package name used to qualify source identities
    #[arg(long = "package", required = true)]
    pub package: String,
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
        "ftbc" => interpret_text_bytecode(path),
        "fbc" => interpret_binary_bytecode(path),
        _ => Err(CliError::InvalidInput(format!(
            "Unsupported input extension: {} (expected .ftbc or .fbc)",
            path.display()
        ))),
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
