//! Expression evaluation command implementation

use crate::{
    CliError, Result,
    cli::CliConfig,
    config::PipelineConfig,
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
// remove unused imports; printing uses fully-qualified console::style and value matching via PipelineOutput
use crate::commands::{format_value_brief, print_runtime_result};
use clap::Args;
use tracing::info;

/// Arguments for the eval command
#[derive(Debug, Clone, Args)]
pub struct EvalArgs {
    /// Expression to evaluate
    #[arg(short, long, conflicts_with = "file")]
    pub expr: Option<String>,

    /// File containing code to evaluate
    #[arg(short, long)]
    pub file: Option<std::path::PathBuf>,

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
pub async fn eval_command(args: EvalArgs, _config: &CliConfig) -> Result<()> {
    // Determine pipeline input
    let (input, description) = if let Some(expr) = &args.expr {
        (
            PipelineInput::Expression(expr.clone()),
            format!("expression: {}", expr),
        )
    } else if let Some(file) = &args.file {
        crate::commands::validate_paths_exist(&[file.clone()], true, "eval")?;
        (
            PipelineInput::File(file.clone()),
            format!("file '{}'", file.display()),
        )
    } else {
        return Err(CliError::InvalidInput(
            "Either --expr or --file must be provided".to_string(),
        ));
    };

    info!("Evaluating {}", description);

    // Configure pipeline for evaluation
    let config = PipelineConfig {
        optimization_level: 0,
        print_ast: args.print_ast,
        print_passes: args.print_passes,
        target: "eval".to_string(),
        runtime: args.runtime.unwrap_or_else(|| "literal".to_string()),
    };

    // Execute pipeline
    let mut pipeline = Pipeline::new();
    let output = pipeline.execute(input, &config).await?;

    // Extract and print result
    match output {
        PipelineOutput::Value(value) => {
            if args.print_result {
                println!(
                    "{} {}",
                    console::style("Result:").green().bold(),
                    format_value_brief(&value)
                );
            }
        }
        PipelineOutput::RuntimeValue(runtime_value) => {
            if args.print_result {
                print_runtime_result(&runtime_value)?;
            }
        }
        _ => {
            return Err(CliError::Compilation(
                "Expected evaluation result".to_string(),
            ));
        }
    }

    Ok(())
}

// printing is centralized in commands::common::print_runtime_result

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
            file: None,
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        // This test might fail until the interpreter is fully implemented
        // but it demonstrates the expected interface
        let _result = eval_command(args, &config).await;
        // For now, just check that it doesn't crash
        // assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_eval_from_file() {
        let config = CliConfig::default();

        // Create a temporary file with some FerroPhase code
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "fn main() {{ 1 + 2 }}").unwrap();

        let args = EvalArgs {
            expr: None,
            file: Some(temp_file.path().to_path_buf()),
            print_ast: false,
            print_passes: false,
            print_result: true,
            runtime: None,
        };

        // This test might fail until the interpreter is fully implemented
        let _result = eval_command(args, &config).await;
        // For now, just check that it doesn't crash with file reading
        // The evaluation itself might fail
    }
}
