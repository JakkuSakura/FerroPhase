//! Run command implementation - executes FerroPhase files directly using AST interpretation

use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::{BExpr, RuntimeValue, Value, register_threadlocal_serializer};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;
use std::path::PathBuf;
use std::sync::Arc;
use tracing::info;

/// Arguments for the run command
pub struct RunArgs {
    pub file: PathBuf,
    pub print_ast: bool,
    pub print_passes: bool,
    pub runtime: Option<String>, // Runtime to use (literal, rust)
}

/// Execute the run command - simplified pipeline that only uses AST + interpretation
pub async fn run_command(args: RunArgs, _config: &CliConfig) -> Result<()> {
    info!("Running file '{}'", args.file.display());

    // Read the source file
    let source = std::fs::read_to_string(&args.file).map_err(|e| {
        CliError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to read file {}: {}", args.file.display(), e),
        ))
    })?;

    // Parse source to AST (no HIR or later stages)
    let ast = parse_source_to_ast(&source)?;

    if args.print_ast {
        print_ast(&ast)?;
    }

    // Execute directly using AST interpretation (bypass HIR/MIR/LIR)
    let runtime = args.runtime.unwrap_or_else(|| "literal".to_string());
    match runtime.as_str() {
        "literal" => {
            let result = interpret_ast_literal(&ast).await?;
            // For run command, we don't print the result value unless it's needed for output
            if let Value::Unit(_) = result {
                // Don't print unit results
            } else {
                println!(
                    "{} {}",
                    style("Result:").green().bold(),
                    format_result(&result)
                );
            }
        }
        "rust" => {
            let result = interpret_ast_runtime(&ast, &runtime).await?;
            // Runtime values handle their own output through println! etc.
            if args.print_passes {
                print_runtime_result(&result)?;
            }
        }
        _ => {
            return Err(CliError::InvalidInput(format!(
                "Unknown runtime: {}",
                runtime
            )));
        }
    }

    Ok(())
}

/// Parse source code to AST without using HIR or later compilation stages
fn parse_source_to_ast(source: &str) -> Result<BExpr> {
    let parser = RustParser::new();

    // Strip shebang line if present
    let cleaned_source = if source.starts_with("#!") {
        source.lines().skip(1).collect::<Vec<_>>().join("\n")
    } else {
        source.to_string()
    };

    // Try parsing as file first
    if let Ok(ast) = try_parse_as_file(&parser, &cleaned_source) {
        return Ok(ast);
    }

    // Try parsing as block expression
    if let Ok(ast) = try_parse_block_expression(&parser, &cleaned_source) {
        return Ok(ast);
    }

    // Try parsing as simple expression
    try_parse_simple_expression(&parser, &cleaned_source)
}

fn try_parse_as_file(parser: &RustParser, source: &str) -> Result<BExpr> {
    // Parse as a syn::File first
    let syn_file: syn::File = syn::parse_str(source)
        .map_err(|e| CliError::Compilation(format!("Failed to parse as file: {}", e)))?;

    // Parse the file using RustParser
    let ast_file = parser
        .parse_file_content(PathBuf::from("input.fp"), syn_file)
        .map_err(|e| CliError::Compilation(format!("Failed to parse file: {}", e)))?;

    // Find main function
    for item in ast_file.items {
        if let Some(func) = item.as_function() {
            if func.name.name == "main" {
                return Ok(func.body.clone());
            }
        }
    }

    // No main function found, treat as expression block
    Err(CliError::Compilation("No main function found".to_string()))
}

fn try_parse_block_expression(parser: &RustParser, source: &str) -> Result<BExpr> {
    let wrapped_source = format!("{{\n{}\n}}", source);
    let syn_expr: syn::Expr = syn::parse_str(&wrapped_source)
        .map_err(|e| CliError::Compilation(format!("Failed to parse as block: {}", e)))?;

    let ast_expr = parser
        .parse_expr(syn_expr)
        .map_err(|e| CliError::Compilation(format!("Failed to convert to AST: {}", e)))?;

    Ok(Box::new(ast_expr))
}

fn try_parse_simple_expression(parser: &RustParser, source: &str) -> Result<BExpr> {
    let syn_expr: syn::Expr = syn::parse_str(source)
        .map_err(|e| CliError::Compilation(format!("Failed to parse as expression: {}", e)))?;

    let ast_expr = parser
        .parse_expr(syn_expr)
        .map_err(|e| CliError::Compilation(format!("Failed to convert to AST: {}", e)))?;

    Ok(Box::new(ast_expr))
}

/// Interpret AST using literal semantics (basic const evaluation)
async fn interpret_ast_literal(ast: &BExpr) -> Result<Value> {
    let _ = ast;
    Err(CliError::Compilation(
        "Typed interpreter path is not implemented yet".to_string(),
    ))
}

/// Interpret AST using runtime semantics (ownership tracking)
async fn interpret_ast_runtime(ast: &BExpr, runtime_name: &str) -> Result<RuntimeValue> {
    let _ = (ast, runtime_name);
    Err(CliError::Compilation(
        "Runtime typed interpreter is not implemented yet".to_string(),
    ))
}

fn print_ast(ast: &BExpr) -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;

    println!(
        "{} {}",
        style("AST:").blue().bold(),
        pretty(ast.as_ref(), pretty_opts)
    );
    Ok(())
}

fn print_runtime_result(result: &RuntimeValue) -> Result<()> {
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

    println!(
        "{} {} [{}]",
        style("Result:").green().bold(),
        style(format!("{}", value)).cyan(),
        style(ownership_info).dim()
    );
    Ok(())
}

fn format_result(result: &Value) -> String {
    match result {
        Value::Unit(_) => "()".to_string(),
        Value::Bool(b) => if b.value { "true" } else { "false" }.to_string(),
        Value::Int(i) => i.value.to_string(),
        Value::Decimal(f) => f.value.to_string(),
        Value::String(s) => format!("\"{}\"", s.value),
        Value::List(list) => format!("[list with {} elements]", list.values.len()),
        Value::Struct(s) => format!("struct {}", s.ty.name),
        _ => format!("{:?}", result),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[tokio::test]
    async fn test_run_simple_expression_file() {
        let config = CliConfig::default();

        // Create a temporary file with simple expression
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

        // Create a temporary file with main function
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
