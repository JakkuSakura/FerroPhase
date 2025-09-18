use crate::CliError;
use fp_core::ast::{AstValue, BExpr, AstExpr, RuntimeValue};
use fp_core::context::SharedScopedContext;
use fp_core::passes::{RuntimePass, LiteralRuntimePass, RustRuntimePass};
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust_lang::parser::RustParser;
use fp_rust_lang::printer::RustPrinter;
use fp_core::ast::register_threadlocal_serializer;
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
    pub runtime: String,
}

#[derive(Debug)]
pub enum PipelineOutput {
    Value(AstValue),
    RuntimeValue(RuntimeValue),
    Code(String),
}

pub struct Pipeline {
    parser: RustParser,
    runtime_pass: Arc<dyn RuntimePass>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self {
            parser: RustParser::new(),
            runtime_pass: Arc::new(LiteralRuntimePass::default()),
        }
    }
    
    pub fn with_runtime(runtime_name: &str) -> Self {
        let runtime_pass: Arc<dyn RuntimePass> = match runtime_name {
            "rust" => Arc::new(RustRuntimePass::new()),
            "literal" | _ => Arc::new(LiteralRuntimePass::default()),
        };
        
        Self {
            parser: RustParser::new(),
            runtime_pass,
        }
    }
    
    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.runtime_pass = match runtime_name {
            "rust" => Arc::new(RustRuntimePass::new()),
            "literal" | _ => Arc::new(LiteralRuntimePass::default()),
        };
    }

    pub async fn execute(&self, input: PipelineInput, config: &PipelineConfig) -> Result<PipelineOutput, CliError> {
        let source = match input {
            PipelineInput::Expression(expr) => expr,
            PipelineInput::File(path) => {
                std::fs::read_to_string(&path)
                    .map_err(|e| CliError::Io(std::io::Error::new(std::io::ErrorKind::Other, format!("Failed to read file {}: {}", path.display(), e))))?
            }
        };

        let ast = self.parse_source(&source)?;
        
        // Choose execution mode based on runtime configuration
        match config.runtime.as_str() {
            "literal" => {
                let result = self.interpret_ast(&ast).await?;
                Ok(PipelineOutput::Value(result))
            },
            "rust" | _ => {
                let result = self.interpret_ast_runtime(&ast, &config.runtime).await?;
                Ok(PipelineOutput::RuntimeValue(result))
            }
        }
    }
    
    /// Execute with runtime semantics
    pub async fn execute_runtime(&self, input: PipelineInput, runtime_name: &str) -> Result<RuntimeValue, CliError> {
        let source = match input {
            PipelineInput::Expression(expr) => expr,
            PipelineInput::File(path) => {
                std::fs::read_to_string(&path)
                    .map_err(|e| CliError::Io(std::io::Error::new(std::io::ErrorKind::Other, format!("Failed to read file {}: {}", path.display(), e))))?
            }
        };

        let ast = self.parse_source(&source)?;
        self.interpret_ast_runtime(&ast, runtime_name).await
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
        // Parse as a syn::File first, but be more permissive with errors
        let syn_file: syn::File = syn::parse_str(source)
            .map_err(|e| CliError::Compilation(format!("Failed to parse as file: {}", e)))?;

        // Try to parse the file, but handle errors more gracefully for transpilation
        match self.parser.parse_file_content(PathBuf::from("input.fp"), syn_file) {
            Ok(ast_file) => {
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
                    // No main function, create a minimal structure for transpilation
                    if const_items.is_empty() {
                        // Create an empty block for transpilation purposes
                        Ok(Box::new(AstExpr::Block(fp_core::ast::ExprBlock {
                            stmts: vec![],
                        })))
                    } else {
                        // Just use all parsed items
                        Ok(Box::new(AstExpr::Block(fp_core::ast::ExprBlock {
                            stmts: const_items,
                        })))
                    }
                }
            },
            Err(_e) => {
                // For transpilation mode, if parsing fails, try to extract just the struct definitions
                self.try_parse_structs_only(source)
            }
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

    fn try_parse_structs_only(&self, source: &str) -> Result<BExpr, CliError> {
        // Try to parse individual items from the source, filtering out problematic ones
        let syn_file: syn::File = syn::parse_str(source)
            .map_err(|e| CliError::Compilation(format!("Failed to parse source: {}", e)))?;
        
        let mut parsed_items = Vec::new();
        
        // Try to parse each item individually, skipping ones that fail
        for item in syn_file.items {
            match item {
                syn::Item::Struct(_) | syn::Item::Enum(_) | syn::Item::Type(_) => {
                    // Try to parse struct/enum definitions
                    if let Ok(ast_item) = self.parser.parse_item(item) {
                        parsed_items.push(fp_core::ast::BlockStmt::Item(Box::new(ast_item)));
                    }
                },
                syn::Item::Fn(_) => {
                    // Try to parse function definitions, but don't fail if they use unsupported features
                    if let Ok(ast_item) = self.parser.parse_item(item) {
                        parsed_items.push(fp_core::ast::BlockStmt::Item(Box::new(ast_item)));
                    }
                },
                syn::Item::Const(_) => {
                    // Try to parse const definitions
                    if let Ok(ast_item) = self.parser.parse_item(item) {
                        parsed_items.push(fp_core::ast::BlockStmt::Item(Box::new(ast_item)));
                    }
                },
                _ => {
                    // Skip other items like use statements, impl blocks, etc.
                    continue;
                }
            }
        }
        
        // Create a block with whatever we managed to parse
        Ok(Box::new(fp_core::ast::AstExpr::Block(fp_core::ast::ExprBlock {
            stmts: parsed_items,
        })))
    }

    async fn interpret_ast_runtime(&self, ast: &BExpr, runtime_name: &str) -> Result<RuntimeValue, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(serializer.clone());
        
        // Create runtime pass based on name
        let runtime_pass: Arc<dyn RuntimePass> = match runtime_name {
            "rust" => Arc::new(RustRuntimePass::new()),
            "literal" | _ => Arc::new(LiteralRuntimePass::default()),
        };

        // Create an orchestrator with the runtime pass
        let orchestrator = InterpretationOrchestrator::new(serializer)
            .with_runtime_pass(runtime_pass);

        // Create a context for interpretation
        let context = SharedScopedContext::new();

        // Interpret with runtime semantics
        orchestrator.interpret_expr_runtime(ast, &context)
            .map_err(|e| CliError::Compilation(format!("Runtime interpretation failed: {}", e)))
    }

    async fn interpret_ast(&self, ast: &BExpr) -> Result<AstValue, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());
        
        // Register the serializer for thread-local access
        register_threadlocal_serializer(serializer.clone());
        
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