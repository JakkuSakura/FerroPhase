use crate::codegen::CodeGenerator;
use crate::compilation::BinaryCompiler;
use crate::config::{PipelineOptions, PipelineTarget, RuntimeConfig};

// Re-export for backward compatibility
use crate::CliError;
pub use crate::config::PipelineConfig;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{BExpr, File, Node, RuntimeValue, Value};
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
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info_span};

#[derive(Debug)]
pub enum PipelineInput {
    Expression(String),
    File(PathBuf),
}

#[derive(Debug)]
pub enum PipelineOutput {
    Value(Value),
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
        let read_span = info_span!("pipeline.read_input");
        let _enter_read = read_span.enter();
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
        drop(_enter_read);
        debug!(path = ?input_path, "loaded input source");

        // Set base path for intermediate files
        options.base_path = Some(base_path.clone());

        let parse_span = info_span!("pipeline.parse", path = ?input_path);
        let _enter_parse = parse_span.enter();
        let (ast_expr, ast_file) = self.parse_source_variants(&source, input_path.as_deref())?;
        drop(_enter_parse);
        debug!(has_file = ast_file.is_some(), "parsed source");
        let ast_node = if let Some(file) = ast_file {
            Node::File(file)
        } else {
            Node::Expr((*ast_expr).clone())
        };

        // Execute based on target
        match options.target {
            PipelineTarget::Rust => {
                let rust_span = info_span!("pipeline.codegen", target = "rust");
                let _enter_rust = rust_span.enter();
                let rust_code = CodeGenerator::generate_rust_code(&ast_node)?;
                drop(_enter_rust);
                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Llvm => {
                let llvm_span = info_span!("pipeline.codegen", target = "llvm");
                let _enter_llvm = llvm_span.enter();
                let llvm_ir =
                    self.compile_to_llvm_ir(&ast_node, &options, input_path.as_deref())?;
                drop(_enter_llvm);
                Ok(PipelineOutput::Code(llvm_ir.to_str().unwrap().to_string()))
            }
            PipelineTarget::Binary => {
                let binary_span = info_span!("pipeline.codegen", target = "binary");
                let _enter_binary = binary_span.enter();
                let binary_result =
                    self.compile_to_binary(&ast_node, &options, input_path.as_deref())?;
                drop(_enter_binary);
                // For binary compilation, print the result and return success
                // The binary files have already been created by compile_to_binary
                println!("{}", binary_result);
                Ok(PipelineOutput::Value(Value::string(
                    "Binary compilation completed".to_string(),
                )))
            }
            PipelineTarget::Interpret => match options.runtime.runtime_type.as_str() {
                "literal" => {
                    let interpret_span = info_span!("pipeline.interpret", runtime = "literal");
                    let _enter_interp = interpret_span.enter();
                    let result = self.interpret_ast(&ast_node).await?;
                    drop(_enter_interp);
                    Ok(PipelineOutput::Value(result))
                }
                "rust" | _ => {
                    let interpret_span = info_span!(
                        "pipeline.interpret",
                        runtime = %options.runtime.runtime_type
                    );
                    let _enter_interp = interpret_span.enter();
                    let result = self
                        .interpret_ast_runtime(&ast_node, &options.runtime.runtime_type)
                        .await?;
                    drop(_enter_interp);
                    Ok(PipelineOutput::RuntimeValue(result))
                }
            },
        }
    }

    /// Unified compilation method that handles intermediate file saving
    fn compile_to_llvm_ir(
        &self,
        ast: &Node,
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
            debug!(path = ?base_path.with_extension("ast"), "persisting AST intermediate");
            std::fs::write(base_path.with_extension("ast"), format!("{:#?}", ast)).ok();
        }

        // Step 2: Const Evaluation
        let const_span = info_span!("pipeline.const_eval");
        let _enter_const = const_span.enter();
        let mut const_evaluator = ConstEvaluationOrchestrator::new(serializer.clone());
        let mut evaluated_node = ast.clone();

        const_evaluator
            .evaluate(&mut evaluated_node, &context)
            .map_err(|e| CliError::Compilation(format!("Const evaluation failed: {}", e)))?;
        drop(_enter_const);

        if options.save_intermediates {
            debug!(path = ?base_path.with_extension("east"), "persisting EAST intermediate");
            std::fs::write(
                base_path.with_extension("east"),
                format!("{:#?}", evaluated_node),
            )
            .ok();
        }

        // Step 3: AST → HIR (High-level IR)
        let hir_span = info_span!("pipeline.lower.hir");
        let _enter_hir = hir_span.enter();
        let hir_program = match &evaluated_node {
            Node::Expr(expr) => {
                let mut hir_generator = match file_path {
                    Some(path) => HirGenerator::with_file(path),
                    None => HirGenerator::new(),
                };
                hir_generator.transform(expr).map_err(|e| {
                    CliError::Compilation(format!("AST to HIR transformation failed: {}", e))
                })?
            }
            Node::File(file) => {
                let mut hir_generator = HirGenerator::with_file(&file.path);
                hir_generator.transform(file).map_err(|e| {
                    CliError::Compilation(format!("AST to HIR transformation failed: {}", e))
                })?
            }
            Node::Item(_) => {
                return Err(CliError::Compilation(
                    "Top-level items are not supported for compilation".to_string(),
                ));
            }
        };
        drop(_enter_hir);

        if options.save_intermediates {
            debug!(path = ?base_path.with_extension("hir"), "persisting HIR intermediate");
            std::fs::write(
                base_path.with_extension("hir"),
                format!("{:#?}", hir_program),
            )
            .ok();
        }

        // Step 4: HIR → THIR (Typed HIR)
        let thir_span = info_span!("pipeline.lower.thir");
        let _enter_thir = thir_span.enter();
        let mut thir_generator = ThirGenerator::new();
        let thir_program = thir_generator.transform(hir_program).map_err(|e| {
            CliError::Compilation(format!("HIR to THIR transformation failed: {}", e))
        })?;
        drop(_enter_thir);

        if options.save_intermediates {
            debug!(path = ?base_path.with_extension("thir"), "persisting THIR intermediate");
            std::fs::write(
                base_path.with_extension("thir"),
                format!("{:#?}", thir_program),
            )
            .ok();
        }

        // Step 5: THIR → MIR (Mid-level Intermediate Representation; SSA CFG)
        let mir_span = info_span!("pipeline.lower.mir");
        let _enter_mir = mir_span.enter();
        let mut mir_generator = MirGenerator::new();
        let mir_program = mir_generator.transform(thir_program).map_err(|e| {
            CliError::Compilation(format!("THIR to MIR transformation failed: {}", e))
        })?;
        drop(_enter_mir);

        if options.save_intermediates {
            debug!(path = ?base_path.with_extension("mir"), "persisting MIR intermediate");
            std::fs::write(
                base_path.with_extension("mir"),
                format!("{:#?}", mir_program),
            )
            .ok();
        }

        // Step 6: MIR → LIR (Low-level IR)
        let lir_span = info_span!("pipeline.lower.lir");
        let _enter_lir = lir_span.enter();
        let mut lir_generator = LirGenerator::new();
        let lir_program = lir_generator.transform(mir_program).map_err(|e| {
            CliError::Compilation(format!("MIR to LIR transformation failed: {}", e))
        })?;
        drop(_enter_lir);

        if options.save_intermediates {
            debug!(path = ?base_path.with_extension("lir"), "persisting LIR intermediate");
            std::fs::write(
                base_path.with_extension("lir"),
                format!("{:#?}", lir_program),
            )
            .ok();
        }

        // Step 7: LIR → LLVM IR
        let llvm_span = info_span!("pipeline.lower.llvm");
        let _enter_llvm = llvm_span.enter();
        let llvm_config = fp_llvm::LlvmConfig::new();
        let llvm_compiler = fp_llvm::LlvmCompiler::new(llvm_config);

        // Pass the global const map to LLVM compiler
        let llvm_ir = llvm_compiler
            .compile(lir_program, None)
            .map_err(|e| CliError::Compilation(format!("LLVM IR generation failed: {}", e)))?;
        drop(_enter_llvm);

        Ok(llvm_ir)
    }

    /// Unified binary compilation method using llc + lld
    fn compile_to_binary(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&std::path::Path>,
    ) -> Result<String, CliError> {
        let base_path = options.base_path.as_ref().unwrap();

        // First generate LLVM IR. the write path is already decided -- want to control all here
        let binary_span = info_span!("pipeline.compile.binary");
        let _enter_binary = binary_span.enter();
        let llvm_ir = self.compile_to_llvm_ir(ast, options, file_path)?;
        drop(_enter_binary);

        // Step 1: Compile LLVM IR to object file using llc
        let obj_path = base_path.with_extension("o");
        debug!(path = ?obj_path, "invoking llc");
        let llc_result = BinaryCompiler::run_llc(&llvm_ir, &obj_path, options)?;

        // Step 2: Link object file to binary (clang preferred, lld/ld fallback)
        let binary_extension = if cfg!(windows) { "exe" } else { "out" };
        let binary_path = base_path.with_extension(binary_extension);
        debug!(path = ?binary_path, "linking final binary");
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
        let cleaned_source = self.clean_source(source);

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

    fn parse_source_variants(
        &self,
        source: &str,
        file_path: Option<&Path>,
    ) -> Result<(BExpr, Option<File>), CliError> {
        let expr = self.parse_source(source)?;
        let file_ast = match file_path {
            Some(path) => Some(self.parse_source_file(source, path)?),
            None => None,
        };
        Ok((expr, file_ast))
    }

    fn parse_source_file(&self, source: &str, path: &Path) -> Result<File, CliError> {
        let cleaned_source = self.clean_source(source);
        let syn_file: syn::File = syn::parse_file(&cleaned_source).map_err(|e| {
            CliError::Compilation(format!("Failed to parse {} as file: {}", path.display(), e))
        })?;

        self.parser
            .parse_file_content(path.to_path_buf(), syn_file)
            .map_err(|e| {
                CliError::Compilation(format!("Failed to lower file {}: {}", path.display(), e))
            })
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &Node,
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

        // Interpret with runtime semantics (expressions only for now)
        match ast {
            Node::Expr(expr) => orchestrator
                .interpret_expr_runtime(expr, &context)
                .map_err(|e| {
                    CliError::Compilation(format!("Runtime interpretation failed: {}", e))
                }),
            Node::File(_) | Node::Item(_) => Err(CliError::Compilation(
                "Runtime interpretation currently supports expressions only".to_string(),
            )),
        }
    }

    async fn interpret_ast(&self, ast: &Node) -> Result<Value, CliError> {
        // Create a serializer using RustPrinter
        let serializer = Arc::new(RustPrinter::new());

        // Register the serializer for thread-local access
        register_threadlocal_serializer(serializer.clone());

        let orchestrator = InterpretationOrchestrator::new(serializer);
        let context = SharedScopedContext::new();

        let result = match ast {
            Node::Expr(expr) => orchestrator
                .interpret_expr(expr, &context)
                .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))?,
            Node::File(file) => orchestrator
                .interpret_items(&file.items, &context)
                .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))?,
            Node::Item(item) => orchestrator
                .interpret_item(item, &context)
                .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))?,
        };

        // Retrieve and print any println! outputs
        let outputs = context.take_outputs();
        for output in outputs {
            print!("{}", output);
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};

    #[test]
    fn parse_source_variants_yields_file_when_path_provided() {
        let pipeline = Pipeline::new();
        let source = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            fn main() {
                let p = Point { x: 1, y: 2 };
                println!("{} {}", p.x, p.y);
            }
        "#;

        let (_expr, file_opt) = pipeline
            .parse_source_variants(source, Some(Path::new("example.fp")))
            .expect("parsing succeeds");

        let file = file_opt.expect("file AST returned");
        assert_eq!(file.path, PathBuf::from("example.fp"));
        assert!(file.items.len() >= 2);
    }

    #[test]
    fn parse_source_variants_without_path_returns_expression_only() {
        let pipeline = Pipeline::new();
        let source = "1 + 2";

        let (_expr, file_opt) = pipeline
            .parse_source_variants(source, None)
            .expect("parsing succeeds");

        assert!(file_opt.is_none());
    }
}
