//! Expression evaluation command implementation

use crate::{CliError, Result, cli::CliConfig, commands::format_value_brief, compiler};
use clap::Args;
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;
use tracing::info;

/// Arguments for the eval command
#[derive(Debug, Clone, Args)]
pub struct EvalArgs {
    /// Source text parsed as one top-level ScriptBlock
    #[arg(value_name = "SCRIPT")]
    pub script: String,

    /// Print the AST representation
    #[arg(long)]
    pub print_ast: bool,

    /// Print optimization passes
    #[arg(long)]
    pub print_passes: bool,

    /// Whether to print the final result
    #[arg(skip = true)]
    pub print_result: bool,

    /// Runtime to use (literal, rust)
    #[arg(long, default_value = "literal")]
    pub runtime: Option<String>,
}

/// Execute the eval command
pub async fn eval_command(mut args: EvalArgs, _config: &CliConfig) -> Result<()> {
    args.print_result = true;
    ensure_compiler_eval_supported(&args)?;

    info!("Evaluating ScriptBlock");
    let frontend = FerroFrontend::new();
    let script = frontend
        .parse_script(&args.script)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    let value = compiler::eval_script(script)?;
    print_eval_value(&value, &args, None)
}

fn ensure_compiler_eval_supported(args: &EvalArgs) -> Result<()> {
    if args.print_ast || args.print_passes {
        return Err(CliError::InvalidInput(
            "--print-ast and --print-passes are not supported on the fp-compiler eval path"
                .to_string(),
        ));
    }

    if args.runtime.as_deref().unwrap_or("literal") != "literal" {
        return Err(CliError::InvalidInput(
            "only --runtime literal is currently supported on the fp-compiler eval path"
                .to_string(),
        ));
    }

    Ok(())
}

fn print_eval_value(
    value: &fp_core::ast::Value,
    args: &EvalArgs,
    label: Option<&std::path::Path>,
) -> Result<()> {
    let prefix = match label {
        Some(path) => format!("{} ", path.display()),
        None => String::new(),
    };

    if args.print_result {
        println!(
            "{}{} {}",
            prefix,
            console::style("Result:").green().bold(),
            format_value_brief(value)
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_eval_simple_expression() {
        let config = CliConfig::default();
        let args = EvalArgs {
            script: "1 + 2 * 3".to_string(),
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        eval_command(args, &config).await.unwrap();
    }

    #[tokio::test]
    async fn test_eval_script_block_with_statements() {
        let config = CliConfig::default();
        let args = EvalArgs {
            script: "let value = 2; value + 3".to_string(),
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        eval_command(args, &config).await.unwrap();
    }

}
