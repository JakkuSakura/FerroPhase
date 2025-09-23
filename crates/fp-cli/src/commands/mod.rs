//! Command implementations for the FerroPhase CLI

pub mod check;
pub mod compile;
pub mod completions;
pub mod eval;
pub mod info;
pub mod init;
pub mod parse;
pub mod run;
pub mod transpile;

// Re-export command functions
pub use check::check_command;
pub use compile::compile_command;
pub use completions::completions_command;
pub use eval::eval_command;
pub use info::info_command;
pub use init::init_command;
pub use parse::parse_command;
pub use run::run_command;
pub use transpile::transpile_command;

use crate::{Result, cli::CliConfig};

/// Common trait for all commands
pub trait Command {
    type Args;

    fn execute(
        args: Self::Args,
        config: &CliConfig,
    ) -> impl std::future::Future<Output = Result<()>> + Send;
}
