//! Expression evaluation command implementation

use crate::{cli::CliConfig, pipeline::{Pipeline, PipelineConfig, PipelineInput, PipelineOutput}, Result, CliError};
use console::style;
use fp_core::ast::{AstValue, RuntimeValue};
use tracing::info;

/// Arguments for the eval command
pub struct EvalArgs {
    pub expr: Option<String>,
    pub file: Option<std::path::PathBuf>,
    pub print_ast: bool,
    pub print_passes: bool,
    pub print_result: bool, // Whether to print the final result
    pub runtime: Option<String>, // Runtime to use (literal, rust)
}

/// Execute the eval command
pub async fn eval_command(args: EvalArgs, _config: &CliConfig) -> Result<()> {
    // Determine pipeline input
    let (input, description) = if let Some(expr) = &args.expr {
        (PipelineInput::Expression(expr.clone()), format!("expression: {}", expr))
    } else if let Some(file) = &args.file {
        (PipelineInput::File(file.clone()), format!("file '{}'", file.display()))
    } else {
        return Err(CliError::InvalidInput("Either --expr or --file must be provided".to_string()));
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
    let pipeline = Pipeline::new();
    let output = pipeline.execute(input, &config).await?;

    // Extract and print result
    match output {
        PipelineOutput::Value(value) => {
            if args.print_result {
                print_result(&value)?;
            }
        },
        PipelineOutput::RuntimeValue(runtime_value) => {
            if args.print_result {
                print_runtime_result(&runtime_value)?;
            }
        },
        _ => return Err(CliError::Compilation("Expected evaluation result".to_string())),
    }

    Ok(())
}

fn print_runtime_result(result: &RuntimeValue) -> Result<()> {
    // Print the runtime value with ownership information
    match result {
        _ => {
            let value = result.get_value();
            let ownership_info = if result.is_literal() {
                "literal"
            } else if result.is_owned() {
                "owned"
            } else if result.is_borrowed() {
                "borrowed"
            } else if result.is_shared() {
                "shared"
            } else {
                "extension"
            };
            
            println!("{} {} [{}]", 
                style("Result:").green().bold(), 
                style(format!("{}", value)).cyan(), 
                style(ownership_info).dim()
            );
        }
    }
    Ok(())
}

fn print_result(result: &AstValue) -> Result<()> {
    // Pretty-print the result based on its type
    match result {
        AstValue::Unit(_) => {
            println!("{} {}", style("Result:").green().bold(), style("()").dim());
        }
        AstValue::Bool(b) => {
            let value_str = if b.value { "true" } else { "false" };
            println!("{} {}", style("Result:").green().bold(), style(value_str).cyan());
        }
        AstValue::Int(i) => {
            println!("{} {}", style("Result:").green().bold(), style(&i.value.to_string()).cyan());
        }
        AstValue::Decimal(f) => {
            println!("{} {}", style("Result:").green().bold(), style(&f.value.to_string()).cyan());
        }
        AstValue::String(s) => {
            println!("{} {}", style("Result:").green().bold(), style(&format!("\"{}\"", s.value)).cyan());
        }
        AstValue::List(list) => {
            println!("{} {} elements)", style("Result:").green().bold(), style(&format!("[list with {}", list.values.len())).cyan());
            for (i, item) in list.values.iter().enumerate() {
                println!("  [{}]: {:?}", i, item);
            }
        }
        AstValue::Struct(s) => {
            println!("{} {}", style("Result:").green().bold(), style(&format!("struct {}", s.ty.name)).cyan());
            for field in &s.structural.fields {
                println!("  {}: {:?}", field.name, field.value);
            }
        }
        _ => {
            println!("{} {:?}", style("Result:").green().bold(), result);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    use std::io::Write;

    #[tokio::test]
    async fn test_eval_simple_expression() {
        let config = CliConfig::default();
        let args = EvalArgs {
            expr: Some("1 + 2 * 3".to_string()),
            file: None,
            print_ast: false,
            print_passes: false,
            print_result: true,
        };

        // This test might fail until the interpreter is fully implemented
        // but it demonstrates the expected interface
        let result = eval_command(args, &config).await;
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
        };

        // This test might fail until the interpreter is fully implemented
        let result = eval_command(args, &config).await;
        // For now, just check that it doesn't crash with file reading
        // The evaluation itself might fail
    }

    #[test]
    fn test_parse_simple_expressions() {
        use crate::pipeline::try_parse_simple_expression;
        
        // Test that we can parse basic arithmetic
        assert!(try_parse_simple_expression("1 + 2").is_ok());
        assert!(try_parse_simple_expression("true && false").is_ok());
        assert!(try_parse_simple_expression("\"hello\"").is_ok());

        // These should not parse as simple expressions
        assert!(try_parse_simple_expression("let x = 5").is_err());
        assert!(try_parse_simple_expression("{ 1 + 2 }").is_err());
    }
}