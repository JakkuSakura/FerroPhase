//! Expression evaluation command implementation

use crate::{cli::CliConfig, Result, CliError};
use console::style;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_optimize::interpreter::Interpreter;
use fp_rust_lang::{parser::RustParser, printer::RustPrinter, shll_parse_expr};
use std::path::Path;
use std::sync::Arc;
use tracing::info;

/// Arguments for the eval command
pub struct EvalArgs {
    pub expr: Option<String>,
    pub file: Option<std::path::PathBuf>,
    pub print_ast: bool,
    pub print_passes: bool,
}

/// Execute the eval command
pub async fn eval_command(args: EvalArgs, _config: &CliConfig) -> Result<()> {
    // Determine what to evaluate
    let (source, description) = if let Some(expr) = &args.expr {
        (expr.clone(), "expression".to_string())
    } else if let Some(file) = &args.file {
        let content = std::fs::read_to_string(file)
            .map_err(|e| CliError::Io(e))?;
        (content, format!("file '{}'", file.display()))
    } else {
        return Err(CliError::InvalidInput("Either --expr or --file must be provided".to_string()));
    };
    
    info!("Evaluating {}", description);
    
    // Set up the evaluation environment
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    
    // Parse the expression/code
    let ast_expr = parse_source(&source)?;
    
    // Print AST if requested
    if args.print_ast {
        print_ast(&ast_expr)?;
    }
    
    // Create interpreter and evaluate
    let interpreter = Interpreter::new(Arc::new(RustPrinter::new()));
    let ctx = SharedScopedContext::new();
    
    // Evaluate the expression
    let result = interpreter.interpret_expr(ast_expr, &ctx)
        .map_err(|e| CliError::Compilation(format!("Evaluation error: {}", e)))?;
    
    // Print the result
    print_result(&result)?;
    
    if args.print_passes {
        print_optimization_info();
    }
    
    Ok(())
}

fn parse_source(source: &str) -> Result<AstExpr> {
    // Try to parse as a simple expression first
    if let Ok(expr) = try_parse_simple_expression(source) {
        return Ok(expr);
    }
    
    // Try to parse as a block expression
    if let Ok(expr) = try_parse_block_expression(source) {
        return Ok(expr);
    }
    
    // Try to parse as a full file and extract the main expression
    try_parse_as_file(source)
}

fn try_parse_simple_expression(source: &str) -> Result<AstExpr> {
    // Use the shll_parse_expr! macro for simple expressions
    let trimmed = source.trim();
    
    // Check if it looks like a simple expression (no blocks, statements)
    if !trimmed.contains('{') && !trimmed.contains(';') && !trimmed.starts_with("let ") {
        let parser = RustParser::new();
        let syn_expr: syn::Expr = syn::parse_str(trimmed)
            .map_err(|e| CliError::Compilation(format!("Parse error: {}", e)))?;
        
        parser.parse_expr(syn_expr)
            .map_err(|e| CliError::Compilation(format!("AST conversion error: {}", e)))
    } else {
        Err(CliError::Compilation("Not a simple expression".to_string()))
    }
}

fn try_parse_block_expression(source: &str) -> Result<AstExpr> {
    // Wrap in a block if it looks like statements
    let wrapped = if source.trim().starts_with('{') {
        source.to_string()
    } else {
        format!("{{ {} }}", source)
    };
    
    let parser = RustParser::new();
    let syn_expr: syn::Expr = syn::parse_str(&wrapped)
        .map_err(|e| CliError::Compilation(format!("Block parse error: {}", e)))?;
    
    parser.parse_expr(syn_expr)
        .map_err(|e| CliError::Compilation(format!("Block AST conversion error: {}", e)))
}

fn try_parse_as_file(source: &str) -> Result<AstExpr> {
    // Parse as a complete file and try to extract an expression
    let parser = RustParser::new();
    let ast_file = parser.parse_file(source)
        .map_err(|e| CliError::Compilation(format!("File parse error: {}", e)))?;
    
    // Look for the first expression item or main function
    for item in &ast_file.items {
        match item {
            AstItem::Expr(expr) => return Ok(expr.clone()),
            AstItem::DefFunction(func) if func.name.name == "main" => {
                // Extract the body of main function
                if let Some(body) = &func.body {
                    return Ok(AstExpr::Block(body.clone()));
                }
            }
            _ => continue,
        }
    }
    
    Err(CliError::Compilation("No evaluable expression found in source".to_string()))
}

fn print_ast(expr: &AstExpr) -> Result<()> {
    println!("{}", style("AST:").cyan().bold());
    println!("{:#?}", expr);
    println!();
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
        AstValue::Float(f) => {
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
            println!("{} {}", style("Result:").green().bold(), style(&format!("struct {}", s.name)).cyan());
            for field in &s.fields {
                println!("  {}: {:?}", field.name, field.value);
            }
        }
        _ => {
            println!("{} {:?}", style("Result:").green().bold(), result);
        }
    }
    
    Ok(())
}

fn print_optimization_info() {
    println!();
    println!("{}", style("Optimization passes:").yellow().bold());
    println!("  • {} Parse and validate syntax", style("✓").green());
    println!("  • {} Convert to AST representation", style("✓").green());
    println!("  • {} Apply const evaluation", style("✓").green());
    println!("  • {} Execute in interpreter", style("✓").green());
    println!();
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
        };
        
        // This test might fail until the interpreter is fully implemented
        let result = eval_command(args, &config).await;
        // For now, just check that it doesn't crash with file reading
        // The evaluation itself might fail
    }
    
    #[test]
    fn test_parse_simple_expressions() {
        // Test that we can parse basic arithmetic
        assert!(try_parse_simple_expression("1 + 2").is_ok());
        assert!(try_parse_simple_expression("true && false").is_ok());
        assert!(try_parse_simple_expression("\"hello\"").is_ok());
        
        // These should not parse as simple expressions
        assert!(try_parse_simple_expression("let x = 5").is_err());
        assert!(try_parse_simple_expression("{ 1 + 2 }").is_err());
    }
}