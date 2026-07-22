//! Expression evaluation command implementation

use crate::{
    CliError, Result,
    cli::CliConfig,
    commands::format_value_brief,
    compiler,
};
use clap::{ArgAction, Args};
use tracing::info;

/// Arguments for the eval command
#[derive(Debug, Clone, Args)]
pub struct EvalArgs {
    /// Expression to evaluate
    #[arg(short, long, conflicts_with = "file")]
    pub expr: Option<String>,

    /// File(s) containing code to evaluate
    #[arg(short, long, action = ArgAction::Append)]
    pub file: Vec<std::path::PathBuf>,

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

    if let Some(expr) = &args.expr {
        let description = format!("expression: {}", expr);
        info!("Evaluating {}", description);
        let value = compiler::eval_expr(expr)?;
        return print_eval_value(&value, &args, None);
    }

    if !args.file.is_empty() {
        crate::commands::validate_paths_exist(&args.file, true, "eval")?;
        for file in &args.file {
            let description = format!("file '{}'", file.display());
            info!("Evaluating {}", description);
            let value = compiler::eval_file(file, None)?;
            let label = if args.file.len() > 1 {
                Some(file.as_path())
            } else {
                None
            };
            print_eval_value(&value, &args, label)?;
        }
        return Ok(());
    }

    Err(CliError::InvalidInput(
        "Either --expr or --file must be provided".to_string(),
    ))
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
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[tokio::test]
    async fn test_eval_simple_expression() {
        let config = CliConfig::default();
        let args = EvalArgs {
            expr: Some("1 + 2 * 3".to_string()),
            file: Vec::new(),
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        let _result = eval_command(args, &config).await;
    }

    #[tokio::test]
    async fn test_eval_from_file() {
        let config = CliConfig::default();

        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "fn main() {{ 1 + 2 }}").unwrap();

        let args = EvalArgs {
            expr: None,
            file: vec![temp_file.path().to_path_buf()],
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        let _result = eval_command(args, &config).await;
    }
}
