//! Command implementations for the FerroPhase CLI

pub mod check;
pub mod common;
pub mod compile;
pub mod completions;
pub mod eval;
pub mod interpret;
pub mod parse;
pub mod run;
pub mod syntax_transpile;
pub mod transpile;

// Re-export command functions
pub use check::check_command;
pub use compile::compile_command;
pub use completions::completions_command;
pub use eval::eval_command;
pub use interpret::interpret_command;
pub use parse::parse_command;
pub use run::run_command;
pub use syntax_transpile::syntax_transpile_command;
pub use transpile::transpile_command;
// Re-export shared helpers for convenience
pub use common::{
    format_value_brief, ownership_label, print_runtime_result, setup_progress_bar,
    validate_paths_exist,
};

use crate::{Result, cli::CliConfig};

/// Common trait for all commands
pub trait Command {
    type Args;

    fn execute(
        args: Self::Args,
        config: &CliConfig,
    ) -> impl std::future::Future<Output = Result<()>> + Send;
}
