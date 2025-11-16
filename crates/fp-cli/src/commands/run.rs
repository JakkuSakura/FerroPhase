//! Run command implementation - executes FerroPhase files directly through the unified pipeline

use crate::config::{PipelineOptions, PipelineTarget};
use crate::pipeline::{Pipeline, PipelineInput, PipelineOutput};
use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::{RuntimeValue, Value};
use crate::commands::{format_value_brief, ownership_label};
use fp_core::pretty::{PrettyOptions, pretty};
use std::path::PathBuf;
use tracing::info;

/// Arguments for the run command
pub struct RunArgs {
    pub file: PathBuf,
    pub print_ast: bool,
    pub print_passes: bool,
    pub runtime: Option<String>, // Runtime to use (literal, rust)
}

/// Execute the run command by funnelling source code through the same pipeline that `compile`
/// uses, stopping at the interpretation stage.
pub async fn run_command(args: RunArgs, config: &CliConfig) -> Result<()> {
    info!("Running file '{}'", args.file.display());

    let runtime = args.runtime.as_deref().unwrap_or("literal").to_string();
    validate_runtime(&runtime)?;

    let source = read_source(&args.file)?;

    if args.print_ast {
        print_ast_representation(&source, &runtime)?;
    }

    let options = build_pipeline_options(&runtime, &args, config);

    let output = execute_pipeline(&runtime, &options, PipelineInput::Expression(source)).await?;

    handle_pipeline_output(output, &args)?;

    Ok(())
}

fn validate_runtime(runtime: &str) -> Result<()> {
    match runtime {
        "literal" | "rust" => Ok(()),
        other => Err(CliError::InvalidInput(format!(
            "Unknown runtime: {}",
            other
        ))),
    }
}

fn read_source(path: &PathBuf) -> Result<String> {
    std::fs::read_to_string(path).map_err(|e| {
        CliError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to read file {}: {}", path.display(), e),
        ))
    })
}

fn build_pipeline_options(runtime: &str, args: &RunArgs, config: &CliConfig) -> PipelineOptions {
    let mut options = PipelineOptions::default();
    options.target = PipelineTarget::Interpret;
    options.runtime.runtime_type = runtime.to_string();
    options.debug.print_ast = args.print_ast;
    options.debug.print_passes = args.print_passes;
    options.debug.verbose = config.compilation.debug;
    options.optimization_level = config.compilation.default_opt_level;
    options.save_intermediates = false;
    options.release = !config.compilation.debug;
    options
}

async fn execute_pipeline(
    runtime: &str,
    options: &PipelineOptions,
    input: PipelineInput,
) -> Result<PipelineOutput> {
    let mut pipeline = Pipeline::with_runtime(runtime);
    pipeline.execute_with_options(input, options.clone()).await
}

fn handle_pipeline_output(output: PipelineOutput, args: &RunArgs) -> Result<()> {
    match output {
        PipelineOutput::Value(value) => {
            if !matches!(value, Value::Unit(_)) {
                println!(
                    "{} {}",
                    style("Result:").green().bold(),
                    format_value_brief(&value)
                );
            }
            Ok(())
        }
        PipelineOutput::RuntimeValue(value) => {
            if args.print_passes {
                print_runtime_result(&value)
            } else {
                Ok(())
            }
        }
        PipelineOutput::Code(_) | PipelineOutput::Binary(_) => Err(CliError::Compilation(
            "Run pipeline produced an unexpected artifact".to_string(),
        )),
    }
}

fn print_ast_representation(source: &str, runtime: &str) -> Result<()> {
    let mut pipeline = Pipeline::with_runtime(runtime);
    let ast = pipeline.parse_source_public(source, None)?;

    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;

    println!(
        "{} {}",
        style("AST:").blue().bold(),
        pretty(&ast, pretty_opts)
    );

    Ok(())
}

fn print_runtime_result(result: &RuntimeValue) -> Result<()> {
    let value = result.get_value();
    let ownership_info = ownership_label(result);
    println!(
        "{} {} [{}]",
        style("Result:").green().bold(),
        style(format!("{}", value)).cyan(),
        style(ownership_info).dim()
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[tokio::test]
    async fn test_run_simple_expression_file() {
        let config = CliConfig::default();

        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "1 + 2 * 3").unwrap();

        let args = RunArgs {
            file: temp_file.path().to_path_buf(),
            print_ast: false,
            print_passes: false,
            runtime: Some("literal".to_string()),
        };

        let result = run_command(args, &config).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_main_function_file() {
        let config = CliConfig::default();

        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "fn main() {{ println!(\"Hello, world!\"); }}").unwrap();

        let args = RunArgs {
            file: temp_file.path().to_path_buf(),
            print_ast: false,
            print_passes: false,
            runtime: Some("literal".to_string()),
        };

        let result = run_command(args, &config).await;
        assert!(result.is_ok());
    }
}
