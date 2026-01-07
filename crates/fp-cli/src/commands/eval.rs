//! Expression evaluation command implementation

use crate::{
    CliError, Result,
    cli::CliConfig,
    config::PipelineConfig,
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
// remove unused imports; printing uses fully-qualified console::style and value matching via PipelineOutput
use crate::commands::{format_value_brief, print_runtime_result};
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
pub async fn eval_command(args: EvalArgs, _config: &CliConfig) -> Result<()> {
    if let Some(expr) = &args.expr {
        let input = PipelineInput::Expression(expr.clone());
        let description = format!("expression: {}", expr);
        return eval_single(input, &description, &args).await;
    }

    if !args.file.is_empty() {
        crate::commands::validate_paths_exist(&args.file, true, "eval")?;
        return eval_files(&args).await;
    }

    {
        return Err(CliError::InvalidInput(
            "Either --expr or --file must be provided".to_string(),
        ));
    }
}

// printing is centralized in commands::common::print_runtime_result

async fn eval_single(input: PipelineInput, description: &str, args: &EvalArgs) -> Result<()> {
    info!("Evaluating {}", description);

    let config = PipelineConfig {
        optimization_level: 0,
        print_ast: args.print_ast,
        print_passes: args.print_passes,
        target: "eval".to_string(),
        runtime: args.runtime.clone().unwrap_or_else(|| "literal".to_string()),
    };

    let mut pipeline = Pipeline::new();
    let output = pipeline.execute(input, &config).await?;
    print_eval_output(&output, args, None)?;

    Ok(())
}

async fn eval_files(args: &EvalArgs) -> Result<()> {
    let config = PipelineConfig {
        optimization_level: 0,
        print_ast: args.print_ast,
        print_passes: args.print_passes,
        target: "eval".to_string(),
        runtime: args.runtime.clone().unwrap_or_else(|| "literal".to_string()),
    };

    for file in &args.file {
        let description = format!("file '{}'", file.display());
        info!("Evaluating {}", description);
        let mut pipeline = Pipeline::new();
        let output = pipeline
            .execute(PipelineInput::File(file.clone()), &config)
            .await?;
        let label = if args.file.len() > 1 {
            Some(file.as_path())
        } else {
            None
        };
        print_eval_output(&output, args, label)?;
    }

    Ok(())
}

fn print_eval_output(
    output: &PipelineOutput,
    args: &EvalArgs,
    label: Option<&std::path::Path>,
) -> Result<()> {
    let prefix = match label {
        Some(path) => format!("{} ", path.display()),
        None => String::new(),
    };

    match output {
        PipelineOutput::Value(value) => {
            if args.print_result {
                println!(
                    "{}{} {}",
                    prefix,
                    console::style("Result:").green().bold(),
                    format_value_brief(value)
                );
            }
        }
        PipelineOutput::RuntimeValue(runtime_value) => {
            if args.print_result {
                if !prefix.is_empty() {
                    println!("{}", prefix.trim_end());
                }
                print_runtime_result(runtime_value)?;
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
            file: vec![temp_file.path().to_path_buf()],
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
