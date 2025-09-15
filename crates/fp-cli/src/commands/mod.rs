//! Command implementations for the FerroPhase CLI

pub mod compile;
pub mod eval;
pub mod check;
pub mod init;
pub mod repl;
pub mod optimize;
pub mod format;
pub mod lsp;
pub mod info;
pub mod completions;

// Re-export command functions
pub use compile::compile_command;
pub use eval::eval_command;
pub use check::check_command;
pub use init::init_command;
pub use repl::repl_command;
pub use optimize::optimize_command;
pub use format::format_command;
pub use lsp::lsp_command;
pub use info::info_command;
pub use completions::completions_command;

use crate::{cli::CliConfig, Result};

/// Common trait for all commands
pub trait Command {
    type Args;
    
    async fn execute(args: Self::Args, config: &CliConfig) -> Result<()>;
}