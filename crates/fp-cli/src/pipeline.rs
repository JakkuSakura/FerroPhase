use crate::codegen::CodeGenerator;
use crate::compilation::BinaryCompiler;
use crate::config::{PipelineOptions, PipelineTarget, RuntimeConfig};

// Re-export for backward compatibility
use crate::CliError;
pub use crate::config::PipelineConfig;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{AstNode, AstValue, BExpr, RuntimeValue};
use fp_core::context::SharedScopedContext;
use fp_core::passes::{LiteralRuntimePass, RuntimePass, RustRuntimePass};
use fp_llvm;
use fp_optimize::ConstEvaluationOrchestrator;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_optimize::transformations::{
    HirGenerator, IrTransform, LirGenerator, MirGenerator, ThirGenerator,
};
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug)]
pub enum PipelineInput {
    Expression(String),
    File(PathBuf),
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

    /// Unified execution method using PipelineOptions
    pub async fn execute_with_options(
        &self,
        input: PipelineInput,
        mut options: PipelineOptions,
    ) -> Result<PipelineOutput, CliError> {
        let (source, base_path, input_path) = match input {
            PipelineInput::Expression(expr) => (expr, PathBuf::from("expression"), None),
            PipelineInput::File(path) => {
                let source = std::fs::read_to_string(&path).map_err(|e| {
                    CliError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to read file {}: {}", path.display(), e),
                    ))
                })?;
                let base_path = path.with_extension("");
                (source, base_path, Some(path))
            }
        };

        // Set base path for intermediate files
        options.base_path = Some(base_path.clone());

        let ast = self.parse_source(&source)?;

        // Execute based on target
        match options.target {
            PipelineTarget::Rust => {
                let rust_code = CodeGenerator::generate_rust_code(&ast)?;
                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Llvm => {
                let llvm_ir = self.compile_to_llvm_ir(&ast, &options, input_path.as_deref())?;
                Ok(PipelineOutput::Code(llvm_ir.to_str().unwrap().to_string()))
            }
            PipelineTarget::Binary => {
                let binary_result =
                    self.compile_to_binary(&ast, &options, input_path.as_deref())?;
                // For binary compilation, print the result and return success
                // The binary files have already been created by compile_to_binary
                println!("{}", binary_result);
                Ok(PipelineOutput::Value(AstValue::string(
                    "Binary compilation completed".to_string(),
                )))
            }
            PipelineTarget::Interpret => match options.runtime.runtime_type.as_str() {
                "literal" => {
                    let result = self.interpret_ast(&ast).await?;
                    Ok(PipelineOutput::Value(result))
                }
                "rust" | _ => {
                    let result = self
                        .interpret_ast_runtime(&ast, &options.runtime.runtime_type)
                        .await?;
                    Ok(PipelineOutput::RuntimeValue(result))
                }
            },
        }
    }

    /// Unified compilation method that handles intermediate file saving
    fn compile_to_llvm_ir(
        &self,
        ast: &BExpr,
        options: &PipelineOptions,
        file_path: Option<&std::path::Path>,
    ) -> Result<PathBuf, CliError> {
        let base_path = options.base_path.as_ref().unwrap();

        // Register serializer for AST operations
        let serializer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(serializer.clone());

        let context = SharedScopedContext::new();

        // Save intermediate files if requested
        if options.save_intermediates {
            // Step 1: Save AST (Abstract Syntax Tree)
            std::fs::write(base_path.with_extension("ast"), format!("{:#?}", ast)).ok();
        }

        // Step 2: Const Evaluation
        let mut const_evaluator = ConstEvaluationOrchestrator::new(serializer.clone());
        let mut evaluated_node = AstNode::Expr((**ast).clone());

        const_evaluator
            .evaluate(&mut evaluated_node, &context)
            .map_err(|e| CliError::Compilation(format!("Const evaluation failed: {}", e)))?;

        if options.save_intermediates {
            std::fs::write(
                base_path.with_extension("east"),
                format!("{:#?}", evaluated_node),
            )
            .ok();
        }

        let evaluated_ast = match evaluated_node {
            AstNode::Expr(expr) => Box::new(expr),
            AstNode::Item(_) | AstNode::File(_) => {
                return Err(CliError::Compilation(
                    "Const evaluation produced unsupported node for compile pipeline".to_string(),
                ));
            }
        };

        // Step 3: AST → HIR (High-level IR)
        let mut hir_generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };
        let hir_program = hir_generator
            .transform(evaluated_ast.as_ref())
            .map_err(|e| {
                CliError::Compilation(format!("AST to HIR transformation failed: {}", e))
            })?;

        if options.save_intermediates {
            std::fs::write(
                base_path.with_extension("hir"),
                format!("{:#?}", hir_program),
            )
            .ok();
        }

        // Step 4: HIR → THIR (Typed HIR)
        let mut thir_generator = ThirGenerator::new();
        let thir_program = thir_generator.transform(hir_program).map_err(|e| {
            CliError::Compilation(format!("HIR to THIR transformation failed: {}", e))
        })?;

        if options.save_intermediates {
            std::fs::write(
                base_path.with_extension("thir"),
                format!("{:#?}", thir_program),
            )
            .ok();
        }

        // Step 5: THIR → MIR (Mid-level IR)
        let mut mir_generator = MirGenerator::new();
        let mir_program = mir_generator.transform(thir_program).map_err(|e| {
            CliError::Compilation(format!("THIR to MIR transformation failed: {}", e))
        })?;

        if options.save_intermediates {
            std::fs::write(
                base_path.with_extension("mir"),
                format!("{:#?}", mir_program),
            )
            .ok();
        }

        // Step 6: MIR → LIR (Low-level IR)
        let mut lir_generator = LirGenerator::new();
        let lir_program = lir_generator.transform(mir_program).map_err(|e| {
            CliError::Compilation(format!("MIR to LIR transformation failed: {}", e))
        })?;

        if options.save_intermediates {
            std::fs::write(
                base_path.with_extension("lir"),
                format!("{:#?}", lir_program),
            )
            .ok();
        }

        // Step 7: LIR → LLVM IR
        let llvm_config = fp_llvm::LlvmConfig::new();
        let llvm_compiler = fp_llvm::LlvmCompiler::new(llvm_config);

        // Pass the global const map to LLVM compiler
        let llvm_ir = llvm_compiler
            .compile(lir_program, None)
            .map_err(|e| CliError::Compilation(format!("LLVM IR generation failed: {}", e)))?;

        Ok(llvm_ir)
    }

    /// Unified binary compilation method using llc + lld
    fn compile_to_binary(
        &self,
        ast: &BExpr,
        options: &PipelineOptions,
        file_path: Option<&std::path::Path>,
    ) -> Result<String, CliError> {
        let base_path = options.base_path.as_ref().unwrap();

        // First generate LLVM IR. the write path is already decided -- want to control all here
        let llvm_ir = self.compile_to_llvm_ir(ast, options, file_path)?;

        // Step 1: Compile LLVM IR to object file using llc
        let obj_path = base_path.with_extension("o");
        let llc_result = BinaryCompiler::run_llc(&llvm_ir, &obj_path, options)?;

        // Step 2: Link object file to binary (clang preferred, lld/ld fallback)
        let binary_extension = if cfg!(windows) { "exe" } else { "out" };
        let binary_path = base_path.with_extension(binary_extension);
        let link_result = BinaryCompiler::link_binary(&obj_path, &binary_path, options)?;

        // Return information about the compilation
        Ok(format!(
            "Binary compiled successfully:\n  LLVM IR: {}\n  Object: {}\n  Binary: {}\n  LLC: {}\n  Linker: {}",
            llvm_ir.display(),
            obj_path.display(),
            binary_path.display(),
            llc_result,
            link_result
        ))
    }

    /// Legacy execute method for backward compatibility
    pub async fn execute(
        &self,
        input: PipelineInput,
        config: &PipelineConfig,
    ) -> Result<PipelineOutput, CliError> {
        // Convert legacy config to new options
        let options = PipelineOptions::from(config);

        // Execute using the unified method
        self.execute_with_options(input, options).await
    }

    /// Legacy execute_runtime method for backward compatibility
    pub async fn execute_runtime(
        &self,
        input: PipelineInput,
        runtime_name: &str,
    ) -> Result<RuntimeValue, CliError> {
        let options = PipelineOptions {
            target: PipelineTarget::Interpret,
            runtime: RuntimeConfig {
                runtime_type: runtime_name.to_string(),
                options: std::collections::HashMap::new(),
            },
            ..Default::default()
        };

        match self.execute_with_options(input, options).await? {
            PipelineOutput::RuntimeValue(val) => Ok(val),
            _ => Err(CliError::Compilation("Expected runtime value".to_string())),
        }
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
        if let Ok(ast) = self
            .parser
            .try_parse_as_file(&cleaned_source)
            .map_err(|e| CliError::Compilation(e.to_string()))
        {
            return Ok(ast);
        }

        // Try parsing as block expression
        if let Ok(ast) = self
            .parser
            .try_parse_block_expression(&cleaned_source)
            .map_err(|e| CliError::Compilation(e.to_string()))
        {
            return Ok(ast);
        }

        // Try parsing as simple expression
        self.parser
            .try_parse_simple_expression(&cleaned_source)
            .map_err(|e| CliError::Compilation(e.to_string()))
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &BExpr,
        runtime_name: &str,
    ) -> Result<RuntimeValue, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(serializer.clone());

        // Create runtime pass based on name
        let runtime_pass: Arc<dyn RuntimePass> = match runtime_name {
            "rust" => Arc::new(RustRuntimePass::new()),
            "literal" | _ => Arc::new(LiteralRuntimePass::default()),
        };

        // Create an orchestrator with the runtime pass
        let orchestrator =
            InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass);

        // Create a context for interpretation
        let context = SharedScopedContext::new();

        // Interpret with runtime semantics
        orchestrator
            .interpret_expr_runtime(ast, &context)
            .map_err(|e| CliError::Compilation(format!("Runtime interpretation failed: {}", e)))
    }

    async fn interpret_ast(&self, ast: &BExpr) -> Result<AstValue, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());

        // Register the serializer for thread-local access
        register_threadlocal_serializer(serializer.clone());

        let orchestrator = InterpretationOrchestrator::new(serializer);
        let context = SharedScopedContext::new();

        let result = orchestrator
            .interpret_expr(ast, &context)
            .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))?;

        // Retrieve and print any println! outputs
        let outputs = context.take_outputs();
        for output in outputs {
            print!("{}", output);
        }

        Ok(result)
    }
}
