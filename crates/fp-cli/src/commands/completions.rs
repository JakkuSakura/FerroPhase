//! Shell completions command implementation

use crate::{Result, cli::CliConfig};
use clap::{Args, Command};
use clap_complete::{Shell, generate};
use std::io;

/// Arguments for the completions command
#[derive(Debug, Clone, Args)]
pub struct CompletionsArgs {
    /// Shell to generate completions for
    #[arg(value_enum)]
    pub shell: Shell,
}

/// Execute the completions command
pub async fn completions_command(args: CompletionsArgs, _config: &CliConfig) -> Result<()> {
    // Define the CLI structure for completion generation
    let mut cmd = Command::new("fp")
        .version(env!("CARGO_PKG_VERSION"))
        .about("FerroPhase: Meta-compilation framework with multi-language comptime superpowers");

    generate(args.shell, &mut cmd, "fp", &mut io::stdout());

    Ok(())
}
