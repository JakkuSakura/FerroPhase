//! Interpret bytecode files produced by `fp compile --backend bytecode`.

use crate::{CliError, Result, cli::CliConfig};
use clap::Args;
use std::path::PathBuf;

/// Arguments for bytecode interpretation.
#[derive(Debug, Clone, Args)]
pub struct InterpretArgs {
    /// Bytecode file to interpret (.fbc)
    #[arg(required = true)]
    pub input: PathBuf,
}

pub async fn interpret_command(args: InterpretArgs, _config: &CliConfig) -> Result<()> {
    let bytes = std::fs::read(&args.input).map_err(CliError::Io)?;
    let file = fp_bytecode::decode_file(&bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to decode bytecode: {}", err)))?;
    let vm = fp_stackvm::Vm::new(file.program);
    vm.run_main().map_err(|err| {
        CliError::Compilation(format!("Bytecode interpretation failed: {}", err))
    })?;
    Ok(())
}
