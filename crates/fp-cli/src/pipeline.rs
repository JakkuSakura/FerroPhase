use crate::CliError;
use fp_core::ast::{AstValue, BExpr, AstExpr};
use fp_core::context::SharedScopedContext;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust_lang::parser::RustParser;
use fp_rust_lang::printer::RustPrinter;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug)]
pub enum PipelineInput {
    Expression(String),
    File(PathBuf),
}

#[derive(Debug)]
pub struct PipelineConfig {
    pub optimization_level: u8,
    pub print_ast: bool,
    pub print_passes: bool,
    pub target: String,
}

#[derive(Debug)]
pub enum PipelineOutput {
    Value(AstValue),
    Code(String),
}

pub struct Pipeline {
    parser: RustParser,
}

impl Pipeline {
    pub fn new() -> Self {
        Self {
            parser: RustParser::new(),
        }
    }

    pub async fn execute(&self, input: PipelineInput, _config: &PipelineConfig) -> Result<PipelineOutput, CliError> {
        let source = match input {
            PipelineInput::Expression(expr) => expr,
            PipelineInput::File(path) => {
                std::fs::read_to_string(&path)
                    .map_err(|e| CliError::Io(std::io::Error::new(std::io::ErrorKind::Other, format!("Failed to read file {}: {}", path.display(), e))))?
            }
        };

        let ast = self.parse_source(&source)?;
        let result = self.interpret_ast(&ast).await?;
        
        Ok(PipelineOutput::Value(result))
    }

    pub fn parse_source_public(&self, source: &str) -> Result<BExpr, CliError> {
        self.parse_source(source)
    }

    fn parse_source(&self, source: &str) -> Result<BExpr, CliError> {
        // Strip shebang line if present
        let cleaned_source = if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        };

        // Try parsing as file first
        if let Ok(ast) = self.try_parse_as_file(&cleaned_source) {
            return Ok(ast);
        }

        // Try parsing as block expression
        if let Ok(ast) = self.try_parse_block_expression(&cleaned_source) {
            return Ok(ast);
        }

        // Try parsing as simple expression
        self.try_parse_simple_expression(&cleaned_source)
    }

    fn try_parse_as_file(&self, source: &str) -> Result<BExpr, CliError> {
        // Parse as a syn::File first
        let syn_file: syn::File = syn::parse_str(source)
            .map_err(|e| CliError::Compilation(format!("Failed to parse as file: {}", e)))?;

        let ast_file = self.parser.parse_file_content(PathBuf::from("input.fp"), syn_file)
            .map_err(|e| CliError::Compilation(format!("Failed to convert to AST: {}", e)))?;

        // Find main function and const declarations
        let mut const_items = Vec::new();
        let mut main_body = None;
        
        for item in ast_file.items {
            if let Some(func) = item.as_function() {
                if func.name.name == "main" {
                    main_body = Some(func.body.clone());
                }
            } else {
                // Keep const declarations and other items
                const_items.push(fp_core::ast::BlockStmt::Item(Box::new(item)));
            }
        }
        
        // If we found a main function, create a block with const items + main body
        if let Some(body) = main_body {
            // Add the main body as the final expression
            const_items.push(fp_core::ast::BlockStmt::Expr(fp_core::ast::BlockStmtExpr {
                expr: body,
                semicolon: None,
            }));
            
            Ok(Box::new(AstExpr::Block(fp_core::ast::ExprBlock {
                stmts: const_items,
            })))
        } else {
            // No main function, just execute all items
            Ok(Box::new(AstExpr::Block(fp_core::ast::ExprBlock {
                stmts: const_items,
            })))
        }
    }

    fn try_parse_block_expression(&self, source: &str) -> Result<BExpr, CliError> {
        let wrapped_source = format!("{{\n{}\n}}", source);
        let syn_expr: syn::Expr = syn::parse_str(&wrapped_source)
            .map_err(|e| CliError::Compilation(format!("Failed to parse as block: {}", e)))?;

        let ast_expr = self.parser.parse_expr(syn_expr)
            .map_err(|e| CliError::Compilation(format!("Failed to convert to AST: {}", e)))?;

        Ok(Box::new(ast_expr))
    }

    fn try_parse_simple_expression(&self, source: &str) -> Result<BExpr, CliError> {
        let syn_expr: syn::Expr = syn::parse_str(source)
            .map_err(|e| CliError::Compilation(format!("Failed to parse as expression: {}", e)))?;

        let ast_expr = self.parser.parse_expr(syn_expr)
            .map_err(|e| CliError::Compilation(format!("Failed to convert to AST: {}", e)))?;

        Ok(Box::new(ast_expr))
    }

    async fn interpret_ast(&self, ast: &BExpr) -> Result<AstValue, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());
        let orchestrator = InterpretationOrchestrator::new(serializer);
        let context = SharedScopedContext::new();
        
        let result = orchestrator.interpret_expr(ast, &context)
            .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))?;

        // Retrieve and print any println! outputs
        let outputs = context.take_outputs();
        for output in outputs {
            print!("{}", output);
        }

        Ok(result)
    }
}

pub fn try_parse_simple_expression(source: &str) -> Result<BExpr, CliError> {
    let pipeline = Pipeline::new();
    pipeline.try_parse_simple_expression(source)
}