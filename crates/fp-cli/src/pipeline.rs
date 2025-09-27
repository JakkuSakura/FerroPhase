use crate::CliError;
use crate::codegen::CodeGenerator;
use crate::compilation::BinaryCompiler;
use crate::config::{PipelineOptions, PipelineTarget, RuntimeConfig};
use crate::frontend::{
    FrontendRegistry, FrontendResult, FrontendSnapshot, LanguageFrontend, RustFrontend,
};
use crate::languages;
use crate::languages::detect_source_language;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{AstSerializer, Node, RuntimeValue, Value};
use fp_core::context::SharedScopedContext;
use fp_core::hir::typed as thir;
use fp_core::passes::{LiteralRuntimePass, RuntimePass, RustRuntimePass};
use fp_core::reporting::{Diagnostic, DiagnosticLevel, Stage, StageReport};
use fp_optimize::ConstEvaluationOrchestrator;
use fp_optimize::ir::{hir, lir, mir};
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_optimize::transformations::{
    HirGenerator, IrTransform, LirGenerator, MirGenerator, ThirGenerator,
};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info_span};

pub use crate::config::PipelineConfig;
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

#[derive(Clone)]
pub struct CompilationContext {
    pub const_results: HashMap<String, Value>,
    pub globals: HashMap<String, Value>,
    pub types: HashMap<String, String>,
    pub imports: HashMap<String, String>,
    pub serializer: Option<Arc<dyn AstSerializer>>,
    pub source_language: Option<String>,
    pub frontend_snapshot: Option<FrontendSnapshot>,
}

impl Default for CompilationContext {
    fn default() -> Self {
        Self {
            const_results: HashMap::new(),
            globals: HashMap::new(),
            types: HashMap::new(),
            imports: HashMap::new(),
            serializer: None,
            source_language: None,
            frontend_snapshot: None,
        }
    }
}

/// Pipeline-specific stage report that includes compilation context
#[derive(Clone)]
pub struct PipelineStageReport<T> {
    pub report: StageReport<T>,
    pub context: CompilationContext,
}

impl<T> PipelineStageReport<T> {
    fn success(
        value: T,
        context: CompilationContext,
        stage: Stage,
    ) -> Self {
        Self {
            report: StageReport::success(value, stage),
            context,
        }
    }

    fn success_with_diagnostics(
        value: T,
        context: CompilationContext,
        stage: Stage,
        diagnostics: Vec<Diagnostic>,
    ) -> Self {
        Self {
            report: StageReport::success_with_diagnostics(value, stage, diagnostics),
            context,
        }
    }

    fn failure(context: CompilationContext, stage: Stage, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            report: StageReport::failure(stage, diagnostics),
            context,
        }
    }

    fn into_result(
        self,
        collected: &mut Vec<Diagnostic>,
    ) -> Result<(T, CompilationContext), CliError> {
        collected.extend(self.report.diagnostics.clone());
        if let Some(value) = self.report.value {
            Ok((value, self.context))
        } else {
            Err(CliError::Compilation(format!(
                "{} stage failed; see diagnostics for details",
                self.report.stage
            )))
        }
    }
}

struct LoweringResult {
    llvm_ir: PathBuf,
    diagnostics: Vec<Diagnostic>,
}

pub struct Pipeline {
    frontends: Arc<FrontendRegistry>,
    default_runtime: String,
}

impl Pipeline {
    pub fn new() -> Self {
        let mut registry = FrontendRegistry::new();
        let rust_frontend: Arc<dyn LanguageFrontend> = Arc::new(RustFrontend::new());
        registry.register(rust_frontend);

        Self {
            frontends: Arc::new(registry),
            default_runtime: "literal".to_string(),
        }
    }

    pub fn with_runtime(runtime_name: &str) -> Self {
        let mut pipeline = Self::new();
        pipeline.default_runtime = runtime_name.to_string();
        pipeline
    }

    pub fn with_frontend_registry(registry: Arc<FrontendRegistry>) -> Self {
        Self {
            frontends: registry,
            default_runtime: "literal".to_string(),
        }
    }

    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.default_runtime = runtime_name.to_string();
    }

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

        options.base_path = Some(base_path.clone());

        let language = options
            .source_language
            .clone()
            .or_else(|| {
                input_path
                    .as_ref()
                    .and_then(|path| detect_source_language(path).map(|lang| lang.name.to_string()))
            })
            .unwrap_or_else(|| languages::FERROPHASE.to_string());

        let frontend = self.frontends.get(&language).ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        let parse_span = info_span!("pipeline.frontend", language = %language);
        let _enter_parse = parse_span.enter();
        let FrontendResult {
            ast: ast_node,
            serializer,
            snapshot,
        } = frontend.parse(&source, input_path.as_deref())?;
        drop(_enter_parse);

        let mut context = CompilationContext::default();
        context.serializer = Some(serializer.clone());
        context.source_language = Some(language.clone());
        context.frontend_snapshot = snapshot;

        match options.target {
            PipelineTarget::Rust => {
                let rust_span = info_span!("pipeline.codegen", target = "rust");
                let _enter_rust = rust_span.enter();
                let rust_code = CodeGenerator::generate_rust_code(&ast_node)?;
                drop(_enter_rust);
                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Interpret => {
                let serializer = context.serializer.clone().ok_or_else(|| {
                    CliError::Compilation(
                        "Frontend did not register serializer for interpretation".to_string(),
                    )
                })?;

                let runtime = if options.runtime.runtime_type.is_empty() {
                    self.default_runtime.clone()
                } else {
                    options.runtime.runtime_type.clone()
                };

                match runtime.as_str() {
                    "literal" => {
                        let interpret_span = info_span!("pipeline.interpret", runtime = "literal");
                        let _enter_interp = interpret_span.enter();
                        let result = self.interpret_ast(&ast_node, serializer).await?;
                        drop(_enter_interp);
                        Ok(PipelineOutput::Value(result))
                    }
                    _ => {
                        let interpret_span = info_span!(
                            "pipeline.interpret",
                            runtime = %runtime
                        );
                        let _enter_interp = interpret_span.enter();
                        let result = self
                            .interpret_ast_runtime(&ast_node, &runtime, serializer)
                            .await?;
                        drop(_enter_interp);
                        Ok(PipelineOutput::RuntimeValue(result))
                    }
                }
            }
            PipelineTarget::Llvm => {
                let lowering =
                    self.compile_to_llvm_ir(ast_node, &options, input_path.as_deref(), context)?;
                self.emit_diagnostics(&lowering.diagnostics, &options);
                Ok(PipelineOutput::Code(
                    lowering.llvm_ir.to_str().unwrap_or_default().to_string(),
                ))
            }
            PipelineTarget::Binary => {
                let (message, diagnostics) =
                    self.compile_to_binary(ast_node, &options, input_path.as_deref(), context)?;
                self.emit_diagnostics(&diagnostics, &options);
                println!("{}", message);
                Ok(PipelineOutput::Value(Value::string(
                    "Binary compilation completed".to_string(),
                )))
            }
            PipelineTarget::Bytecode => {
                let bytecode_span = info_span!("pipeline.bytecode");
                let _enter_bytecode = bytecode_span.enter();
                Err(CliError::Compilation(
                    "Bytecode target is not implemented yet".to_string(),
                ))
            }
        }
    }

    fn compile_to_binary(
        &self,
        ast: Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        context: CompilationContext,
    ) -> Result<(String, Vec<Diagnostic>), CliError> {
        let lowering = self.compile_to_llvm_ir(ast, options, file_path, context)?;

        let base_path = options.base_path.as_ref().ok_or_else(|| {
            CliError::Compilation("Missing base path for binary output".to_string())
        })?;

        let obj_path = base_path.with_extension("o");
        debug!(path = ?obj_path, "invoking llc");
        let llc_result = BinaryCompiler::run_llc(&lowering.llvm_ir, &obj_path, options)?;

        let binary_extension = if cfg!(windows) { "exe" } else { "out" };
        let binary_path = base_path.with_extension(binary_extension);
        debug!(path = ?binary_path, "linking final binary");
        let link_result = BinaryCompiler::link_binary(&obj_path, &binary_path, options)?;

        let mut diagnostics = lowering.diagnostics;
        diagnostics.push(Diagnostic::info(
            Stage::BinaryLink,
            format!("Linked binary to {}", binary_path.display()),
        ));

        let summary = format!(
            "Binary compiled successfully:\n  LLVM IR: {}\n  Object: {}\n  Binary: {}\n  LLC: {}\n  Linker: {}",
            lowering.llvm_ir.display(),
            obj_path.display(),
            binary_path.display(),
            llc_result,
            link_result
        );

        Ok((summary, diagnostics))
    }

    fn compile_to_llvm_ir(
        &self,
        ast: Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        mut context: CompilationContext,
    ) -> Result<LoweringResult, CliError> {
        let base_path = options.base_path.as_ref().ok_or_else(|| {
            CliError::Compilation("Missing base path for compilation".to_string())
        })?;

        let mut diagnostics = Vec::new();

        let const_report = self.run_const_eval_stage(ast, context, options, base_path)?;
        self.emit_diagnostics(&const_report.report.diagnostics, options);
        let (evaluated_ast, updated_context) =
            const_report.into_result(&mut diagnostics)?;
        context = updated_context;

        let hir_report =
            self.run_hir_stage(&evaluated_ast, context, options, file_path, base_path)?;
        self.emit_diagnostics(&hir_report.report.diagnostics, options);
        let (hir_program, updated_context) =
            hir_report.into_result(&mut diagnostics)?;
        context = updated_context;

        let thir_report = self.run_thir_stage(hir_program, context, options, base_path)?;
        self.emit_diagnostics(&thir_report.report.diagnostics, options);
        let (thir_program, updated_context) =
            thir_report.into_result(&mut diagnostics)?;
        context = updated_context;

        let mir_report = self.run_mir_stage(thir_program, context, options, base_path)?;
        self.emit_diagnostics(&mir_report.report.diagnostics, options);
        let (mir_program, updated_context) =
            mir_report.into_result(&mut diagnostics)?;
        context = updated_context;

        let lir_report = self.run_lir_stage(mir_program, context, options, base_path)?;
        self.emit_diagnostics(&lir_report.report.diagnostics, options);
        let (lir_program, updated_context) =
            lir_report.into_result(&mut diagnostics)?;
        context = updated_context;

        let llvm_report = self.run_llvm_stage(lir_program, context, base_path)?;
        self.emit_diagnostics(&llvm_report.report.diagnostics, options);
        let (llvm_ir, _context) =
            llvm_report.into_result(&mut diagnostics)?;

        Ok(LoweringResult {
            llvm_ir,
            diagnostics,
        })
    }

    fn run_const_eval_stage(
        &self,
        ast: Node,
        mut context: CompilationContext,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<PipelineStageReport<Node>, CliError> {
        let serializer = context.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut const_evaluator = ConstEvaluationOrchestrator::new(serializer.clone());
        let mut evaluated_node = ast;

        if let Err(e) = const_evaluator.evaluate(&mut evaluated_node, &shared_context) {
            let diagnostic = Diagnostic::error(
                Stage::ConstEval,
                format!("Const evaluation failed: {}", e),
            );
            return Ok(PipelineStageReport::failure(context, Stage::ConstEval, vec![diagnostic]));
        }

        context.const_results = const_evaluator.get_results();

        if options.save_intermediates {
            if let Err(err) = fs::write(
                base_path.with_extension("east"),
                format!("{:#?}", evaluated_node),
            ) {
                debug!(error = %err, "failed to persist EAST intermediate");
            }
        }

        Ok(PipelineStageReport::success(evaluated_node, context, Stage::ConstEval))
    }

    fn run_hir_stage(
        &self,
        ast: &Node,
        context: CompilationContext,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<PipelineStageReport<hir::Program>, CliError> {
        let mut generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if options.error_tolerance.enabled {
            generator.enable_error_tolerance(options.error_tolerance.max_errors);
        }

        if matches!(ast, Node::Item(_)) {
            let diag = Diagnostic::error(
                Stage::AstToHir,
                "Top-level items are not supported; provide a file or expression",
            );
            return Ok(PipelineStageReport::failure(context, Stage::AstToHir, vec![diag]));
        }

        let result = match ast {
            Node::Expr(expr) => generator.transform(expr),
            Node::File(file) => generator.transform(file),
            Node::Item(_) => unreachable!(),
        };

        let (errors, warnings) = generator.take_diagnostics();
        let mut diagnostics = Vec::new();

        for warning in warnings {
            let mut diag = Diagnostic::warning(
                Stage::AstToHir,
                format!("{}", warning.message),
            );
            diag.span = warning.span.map(|span| span.to_string());
            diagnostics.push(diag);
        }

        if let Err(e) = &result {
            diagnostics.push(Diagnostic::error(
                Stage::AstToHir,
                format!("AST→HIR transformation failed: {}", e),
            ));
        }

        if !errors.is_empty() {
            for error in &errors {
                let mut diag = Diagnostic::error(
                    Stage::AstToHir,
                    format!("{}", error.message),
                );
                diag.span = error.span.map(|span| span.to_string());
                diag.suggestions = error.suggestions.clone();
                diagnostics.push(diag);
            }
        }

        match result {
            Ok(program) if errors.is_empty() => {
                if options.save_intermediates {
                    if let Err(err) =
                        fs::write(base_path.with_extension("hir"), format!("{:#?}", program))
                    {
                        debug!(error = %err, "failed to persist HIR intermediate");
                    }
                }

                Ok(PipelineStageReport::success_with_diagnostics(program, context, Stage::AstToHir, diagnostics))
            }
            _ => Ok(PipelineStageReport::failure(context, Stage::AstToHir, diagnostics)),
        }
    }

    fn run_thir_stage(
        &self,
        hir_program: hir::Program,
        context: CompilationContext,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<PipelineStageReport<thir::Program>, CliError> {
        let mut generator = ThirGenerator::new();
        let result = generator.transform(hir_program);
        let mut diagnostics = Vec::new();

        if let Err(e) = &result {
            diagnostics.push(Diagnostic::error(
                Stage::HirToThir,
                format!("HIR→THIR transformation failed: {}", e),
            ));
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    if let Err(err) =
                        fs::write(base_path.with_extension("thir"), format!("{:#?}", program))
                    {
                        debug!(error = %err, "failed to persist THIR intermediate");
                    }
                }

                Ok(PipelineStageReport::success_with_diagnostics(program, context, Stage::HirToThir, diagnostics))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                if diagnostics.is_empty() {
                    diagnostics.push(Diagnostic::error(
                        Stage::HirToThir,
                        format!("HIR→THIR transformation failed: {}", e),
                    ));
                }
                Ok(PipelineStageReport::failure(context, Stage::HirToThir, diagnostics))
            },
        }
    }

    fn run_mir_stage(
        &self,
        thir_program: thir::Program,
        context: CompilationContext,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<PipelineStageReport<mir::Program>, CliError> {
        let mut generator = MirGenerator::new();
        let result = generator.transform(thir_program);
        let mut diagnostics = Vec::new();

        if let Err(e) = &result {
            diagnostics.push(Diagnostic::error(
                Stage::ThirToMir,
                format!("THIR→MIR transformation failed: {}", e),
            ));
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    if let Err(err) =
                        fs::write(base_path.with_extension("mir"), format!("{:#?}", program))
                    {
                        debug!(error = %err, "failed to persist MIR intermediate");
                    }
                }

                Ok(PipelineStageReport::success_with_diagnostics(program, context, Stage::ThirToMir, diagnostics))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                if diagnostics.is_empty() {
                    diagnostics.push(Diagnostic::error(
                        Stage::ThirToMir,
                        format!("THIR→MIR transformation failed: {}", e),
                    ));
                }
                Ok(PipelineStageReport::failure(context, Stage::ThirToMir, diagnostics))
            },
        }
    }

    fn run_lir_stage(
        &self,
        mir_program: mir::Program,
        context: CompilationContext,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<PipelineStageReport<lir::LirProgram>, CliError> {
        let mut generator = LirGenerator::new(context.const_results.clone());
        let result = generator.transform(mir_program);
        let mut diagnostics = Vec::new();

        if let Err(e) = &result {
            diagnostics.push(Diagnostic::error(
                Stage::MirToLir,
                format!("MIR→LIR transformation failed: {}", e),
            ));
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    if let Err(err) =
                        fs::write(base_path.with_extension("lir"), format!("{:#?}", program))
                    {
                        debug!(error = %err, "failed to persist LIR intermediate");
                    }
                }

                Ok(PipelineStageReport::success_with_diagnostics(program, context, Stage::MirToLir, diagnostics))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                if diagnostics.is_empty() {
                    diagnostics.push(Diagnostic::error(
                        Stage::MirToLir,
                        format!("MIR→LIR transformation failed: {}", e),
                    ));
                }
                Ok(PipelineStageReport::failure(context, Stage::MirToLir, diagnostics))
            },
        }
    }

    fn run_llvm_stage(
        &self,
        lir_program: lir::LirProgram,
        context: CompilationContext,
        base_path: &Path,
    ) -> Result<PipelineStageReport<PathBuf>, CliError> {
        let llvm_output = base_path.with_extension("ll");
        let llvm_config = fp_llvm::LlvmConfig::executable(&llvm_output);
        let llvm_compiler = fp_llvm::LlvmCompiler::new(llvm_config);

        let mut diagnostics = Vec::new();

        let result = llvm_compiler.compile(lir_program, None);
        if let Err(e) = &result {
            diagnostics.push(Diagnostic::error(
                Stage::LirToLlvm,
                format!("LLVM IR generation failed: {}", e),
            ));
        }

        let llvm_ir = match result {
            Ok(path) => path,
            Err(_) => return Ok(PipelineStageReport::failure(context, Stage::LirToLlvm, diagnostics)),
        };

        Ok(PipelineStageReport::success_with_diagnostics(llvm_ir, context, Stage::LirToLlvm, diagnostics))
    }

    pub async fn execute(
        &self,
        input: PipelineInput,
        config: &PipelineConfig,
    ) -> Result<PipelineOutput, CliError> {
        let options = PipelineOptions::from(config);
        self.execute_with_options(input, options).await
    }

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

    pub fn parse_source_public(&self, source: &str) -> Result<fp_core::ast::BExpr, CliError> {
        let frontend = self
            .frontends
            .get(languages::FERROPHASE)
            .ok_or_else(|| CliError::Compilation("Rust frontend not registered".to_string()))?;
        let FrontendResult { ast, .. } = frontend.parse(source, None)?;

        match ast {
            Node::Expr(expr) => Ok(Box::new(expr)),
            _ => Err(CliError::Compilation(
                "Expected expression input when parsing source".to_string(),
            )),
        }
    }

    async fn interpret_ast(
        &self,
        ast: &Node,
        serializer: Arc<dyn AstSerializer>,
    ) -> Result<Value, CliError> {
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

        for output in context.take_outputs() {
            print!("{}", output);
        }

        Ok(result)
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &Node,
        runtime_name: &str,
        serializer: Arc<dyn AstSerializer>,
    ) -> Result<RuntimeValue, CliError> {
        register_threadlocal_serializer(serializer.clone());

        let runtime_pass: Arc<dyn RuntimePass> = match runtime_name {
            "rust" => Arc::new(RustRuntimePass::new()),
            _ => Arc::new(LiteralRuntimePass::default()),
        };

        let orchestrator =
            InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass);
        let context = SharedScopedContext::new();

        match ast {
            Node::Expr(expr) => orchestrator
                .interpret_expr_runtime(expr, &context)
                .map_err(|e| {
                    CliError::Compilation(format!("Runtime interpretation failed: {}", e))
                }),
            _ => Err(CliError::Compilation(
                "Runtime interpretation currently supports expressions only".to_string(),
            )),
        }
    }

    fn emit_diagnostics(&self, diagnostics: &[Diagnostic], options: &PipelineOptions) {
        if diagnostics.is_empty() {
            return;
        }

        for diagnostic in diagnostics {
            match diagnostic.level {
                DiagnosticLevel::Error => {
                    eprintln!("❌ [{}] {}", diagnostic.stage, diagnostic.message);
                }
                DiagnosticLevel::Warning => {
                    eprintln!("⚠️  [{}] {}", diagnostic.stage, diagnostic.message);
                }
                DiagnosticLevel::Info => {
                    if options.debug.verbose {
                        eprintln!("ℹ️  [{}] {}", diagnostic.stage, diagnostic.message);
                    }
                }
            }

            if let Some(span) = &diagnostic.span {
                eprintln!("   at {}", span);
            }

            for suggestion in &diagnostic.suggestions {
                eprintln!("   💡 {}", suggestion);
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_frontend_parses_expression() {
        let pipeline = Pipeline::new();
        let expr = "1 + 2";
        assert!(pipeline.parse_source_public(expr).is_ok());
    }
}
