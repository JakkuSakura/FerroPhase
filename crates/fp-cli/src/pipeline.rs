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
use fp_core::diagnostics::{
    Diagnostic, DiagnosticDisplayOptions, DiagnosticManager, DiagnosticReport, DiagnosticTemplate,
};
use fp_core::hir::typed as thir;
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::{hir, lir, mir};
use fp_optimize::orchestrators::const_evaluation::ConstEvalOutcome;
use fp_optimize::transformations::{
    HirGenerator, IrTransform, LirGenerator, MirGenerator, ThirDetyper, ThirGenerator,
};
use fp_optimize::{ConstEvaluationOrchestrator, InterpretationOrchestrator, InterpreterMode};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info_span};

const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_HIR_TO_THIR: &str = "hir→thir";
const STAGE_THIR_TO_MIR: &str = "thir→mir";
const STAGE_MIR_TO_LIR: &str = "mir→lir";
const STAGE_LIR_TO_LLVM: &str = "lir→llvm";
const STAGE_BINARY: &str = "binary";
const STAGE_INTERPRET: &str = "interpret";
const STAGE_THIR_TO_HIR_DETYPE: &str = "thir→hir(detyped)";
const STAGE_DETYPED_HIR_TO_THIR: &str = "detyped-hir→thir";

const EXT_HIR: &str = "hir";
const EXT_THIR: &str = "thi";
const EXT_CONST_THIR: &str = "tce";
const EXT_DETYPED_HIR: &str = "dhi";
const EXT_MIR: &str = "mir";
const EXT_LIR: &str = "lir";

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

struct CompilationArtifacts {
    llvm_ir: PathBuf,
    diagnostics: Vec<Diagnostic>,
}

struct ConstEvalArtifacts {
    thir_program: thir::Program,
    outcome: ConstEvalOutcome,
}

pub struct Pipeline {
    frontends: Arc<FrontendRegistry>,
    default_runtime: String,
    diagnostic_template: DiagnosticTemplate,
    serializer: Option<Arc<dyn AstSerializer>>,
    source_language: Option<String>,
    frontend_snapshot: Option<FrontendSnapshot>,
}

impl Pipeline {
    pub fn new() -> Self {
        let mut registry = FrontendRegistry::new();
        let rust_frontend: Arc<dyn LanguageFrontend> = Arc::new(RustFrontend::new());
        registry.register(rust_frontend);

        Self {
            frontends: Arc::new(registry),
            default_runtime: "literal".to_string(),
            diagnostic_template: DiagnosticTemplate::Pretty,
            serializer: None,
            source_language: None,
            frontend_snapshot: None,
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
            diagnostic_template: DiagnosticTemplate::Pretty,
            serializer: None,
            source_language: None,
            frontend_snapshot: None,
        }
    }

    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.default_runtime = runtime_name.to_string();
    }

    pub fn with_diagnostic_template(mut self, template: DiagnosticTemplate) -> Self {
        self.diagnostic_template = template;
        self
    }

    pub fn set_diagnostic_template(&mut self, template: DiagnosticTemplate) {
        self.diagnostic_template = template;
    }

    pub async fn execute_with_options(
        &mut self,
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

        self.serializer = None;
        self.source_language = None;
        self.frontend_snapshot = None;

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

        self.source_language = Some(language.clone());

        let frontend = self.frontends.get(&language).ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        let parse_span = info_span!("pipeline.frontend", language = %language);
        let _enter_parse = parse_span.enter();
        let FrontendResult {
            last: _last,
            ast: ast_node,
            serializer,
            snapshot,
        } = frontend.parse(&source, input_path.as_deref())?;
        drop(_enter_parse);

        register_threadlocal_serializer(serializer.clone());

        self.serializer = Some(serializer.clone());
        self.frontend_snapshot = snapshot;

        match options.target {
            PipelineTarget::Rust => {
                let rust_span = info_span!("pipeline.codegen", target = "rust");
                let _enter_rust = rust_span.enter();
                let rust_code = CodeGenerator::generate_rust_code(&ast_node)?;
                drop(_enter_rust);
                Ok(PipelineOutput::Code(rust_code))
            }
            PipelineTarget::Interpret => {
                let runtime = if options.runtime.runtime_type.is_empty() {
                    self.default_runtime.clone()
                } else {
                    options.runtime.runtime_type.clone()
                };

                match runtime.as_str() {
                    "literal" => {
                        let interpret_span = info_span!("pipeline.interpret", runtime = "literal");
                        let _enter_interp = interpret_span.enter();
                        let result = self
                            .interpret_ast(&ast_node, &options, input_path.as_deref())
                            .await?;
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
                            .interpret_ast_runtime(
                                &ast_node,
                                &runtime,
                                &options,
                                input_path.as_deref(),
                            )
                            .await?;
                        drop(_enter_interp);
                        Ok(PipelineOutput::RuntimeValue(result))
                    }
                }
            }
            PipelineTarget::Llvm => {
                let artifacts = self.compile(ast_node, &options, input_path.as_deref())?;
                self.emit_diagnostics(&artifacts.diagnostics, None, &options);
                Ok(PipelineOutput::Code(
                    artifacts.llvm_ir.to_str().unwrap_or_default().to_string(),
                ))
            }
            PipelineTarget::Binary => {
                let mut artifacts = self.compile(ast_node, &options, input_path.as_deref())?;
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    CliError::Compilation("Missing base path for binary output".to_string())
                })?;

                let obj_path = base_path.with_extension("o");
                debug!(path = ?obj_path, "invoking llc");
                let llc_result = BinaryCompiler::run_llc(&artifacts.llvm_ir, &obj_path, &options)?;

                let binary_extension = if cfg!(windows) { "exe" } else { "out" };
                let binary_path = base_path.with_extension(binary_extension);
                debug!(path = ?binary_path, "linking final binary");
                let link_result = BinaryCompiler::link_binary(&obj_path, &binary_path, &options)?;

                artifacts.diagnostics.push(
                    Diagnostic::info(format!("Linked binary to {}", binary_path.display()))
                        .with_source_context(STAGE_BINARY),
                );

                self.emit_diagnostics(&artifacts.diagnostics, None, &options);
                println!(
                    "Binary compiled successfully:\n  LLVM IR: {}\n  Object: {}\n  Binary: {}\n  LLC: {}\n  Linker: {}",
                    artifacts.llvm_ir.display(),
                    obj_path.display(),
                    binary_path.display(),
                    llc_result,
                    link_result
                );

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

    fn compile(
        &self,
        ast: Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
    ) -> Result<CompilationArtifacts, CliError> {
        let base_path = options.base_path.as_ref().ok_or_else(|| {
            CliError::Compilation("Missing base path for compilation".to_string())
        })?;

        let diagnostic_manager = DiagnosticManager::new();

        let hir_report = self.run_hir_stage(&ast, options, file_path, base_path)?;
        let hir_program =
            self.collect_stage(STAGE_AST_TO_HIR, hir_report, &diagnostic_manager, options)?;

        let thir_report = self.run_thir_stage(hir_program, options, base_path)?;
        let thir_program =
            self.collect_stage(STAGE_HIR_TO_THIR, thir_report, &diagnostic_manager, options)?;

        let const_report = self.run_const_eval_stage(thir_program, options, base_path)?;
        let ConstEvalArtifacts {
            thir_program: evaluated_thir,
            outcome: _const_outcome,
        } = self.collect_stage(STAGE_CONST_EVAL, const_report, &diagnostic_manager, options)?;

        let detype_report = self.run_detype_stage(evaluated_thir, options, base_path)?;
        let detyped_hir = self.collect_stage(
            STAGE_THIR_TO_HIR_DETYPE,
            detype_report,
            &diagnostic_manager,
            options,
        )?;

        let thir_from_detyped = self.run_thir_stage(detyped_hir, options, base_path)?;
        let thir_program = self.collect_stage(
            STAGE_DETYPED_HIR_TO_THIR,
            thir_from_detyped,
            &diagnostic_manager,
            options,
        )?;

        let mir_report = self.run_mir_stage(thir_program, options, base_path)?;
        let mir_program =
            self.collect_stage(STAGE_THIR_TO_MIR, mir_report, &diagnostic_manager, options)?;

        let lir_report = self.run_lir_stage(mir_program, options, base_path)?;
        let lir_program =
            self.collect_stage(STAGE_MIR_TO_LIR, lir_report, &diagnostic_manager, options)?;

        let llvm_report = self.run_llvm_stage(lir_program, base_path)?;
        let llvm_ir =
            self.collect_stage(STAGE_LIR_TO_LLVM, llvm_report, &diagnostic_manager, options)?;

        let diagnostics = diagnostic_manager.get_diagnostics();

        Ok(CompilationArtifacts {
            llvm_ir,
            diagnostics,
        })
    }

    fn run_const_eval_stage(
        &self,
        thir_program: thir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<ConstEvalArtifacts>, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut const_evaluator = ConstEvaluationOrchestrator::new(serializer.clone());

        let outcome = match const_evaluator.evaluate(&thir_program, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                return Ok(DiagnosticReport::failure(vec![diagnostic]));
            }
        };

        if options.save_intermediates {
            let mut pretty_opts = PrettyOptions::default();
            pretty_opts.show_spans = options.debug.verbose;
            let rendered = format!("{}", pretty(&thir_program, pretty_opts));
            if let Err(err) = fs::write(base_path.with_extension(EXT_CONST_THIR), rendered) {
                debug!(
                    error = %err,
                    "failed to persist evaluated THIR (tce) intermediate after const eval"
                );
            }
        }

        Ok(DiagnosticReport::success(ConstEvalArtifacts {
            thir_program,
            outcome,
        }))
    }

    fn run_detype_stage(
        &self,
        thir_program: thir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<hir::Program>, CliError> {
        let mut detyper = ThirDetyper::new();
        match detyper.transform(thir_program) {
            Ok(hir_program) => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&hir_program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_DETYPED_HIR), rendered)
                    {
                        debug!(
                            error = %err,
                            "failed to persist detyped HIR intermediate"
                        );
                    }
                }
                Ok(DiagnosticReport::success(hir_program))
            }
            Err(err) => {
                let diagnostic = Diagnostic::error(format!("THIR→HIR detyping failed: {}", err))
                    .with_source_context(STAGE_THIR_TO_HIR_DETYPE);
                Ok(DiagnosticReport::failure(vec![diagnostic]))
            }
        }
    }

    fn run_hir_stage(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<DiagnosticReport<hir::Program>, CliError> {
        let mut generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if options.error_tolerance.enabled {
            generator.enable_error_tolerance(options.error_tolerance.max_errors);
        }

        if matches!(ast, Node::Item(_)) {
            let diag = Diagnostic::error(
                "Top-level items are not supported; provide a file or expression".to_string(),
            )
            .with_source_context(STAGE_AST_TO_HIR);
            return Ok(DiagnosticReport::failure(vec![diag]));
        }

        let result = match ast {
            Node::Expr(expr) => generator.transform(expr),
            Node::File(file) => generator.transform(file),
            Node::Item(_) => unreachable!(),
        };

        let (errors, warnings) = generator.take_diagnostics();
        let diagnostic_manager = DiagnosticManager::new();

        diagnostic_manager.add_diagnostics(warnings.clone());

        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("AST→HIR transformation failed: {}", e))
                    .with_source_context(STAGE_AST_TO_HIR),
            );
        }

        if !errors.is_empty() {
            diagnostic_manager.add_diagnostics(errors.clone());
        }

        match result {
            Ok(program) if errors.is_empty() => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_HIR), rendered) {
                        debug!(error = %err, "failed to persist HIR intermediate");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::success_with_diagnostics(
                    program,
                    diagnostics,
                ))
            }
            _ => {
                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::failure(diagnostics))
            }
        }
    }

    fn run_thir_stage(
        &self,
        hir_program: hir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<thir::Program>, CliError> {
        let mut generator = ThirGenerator::new();
        let result = generator.transform(hir_program);
        let diagnostic_manager = DiagnosticManager::new();

        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("HIR→THIR transformation failed: {}", e))
                    .with_source_context(STAGE_HIR_TO_THIR),
            );
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_THIR), rendered) {
                        debug!(error = %err, "failed to persist THIR intermediate");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::success_with_diagnostics(
                    program,
                    diagnostics,
                ))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                let mut diagnostics = diagnostic_manager.get_diagnostics();
                if diagnostics.is_empty() {
                    diagnostics.push(
                        Diagnostic::error(format!("HIR→THIR transformation failed: {}", e))
                            .with_source_context(STAGE_HIR_TO_THIR),
                    );
                }
                Ok(DiagnosticReport::failure(diagnostics))
            }
        }
    }

    fn run_mir_stage(
        &self,
        thir_program: thir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<mir::Program>, CliError> {
        let mut generator = MirGenerator::new();
        let result = generator.transform(thir_program);
        let diagnostic_manager = DiagnosticManager::new();

        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("THIR→MIR transformation failed: {}", e))
                    .with_source_context(STAGE_THIR_TO_MIR),
            );
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_MIR), rendered) {
                        debug!(error = %err, "failed to persist MIR intermediate");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::success_with_diagnostics(
                    program,
                    diagnostics,
                ))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                let mut diagnostics = diagnostic_manager.get_diagnostics();
                if diagnostics.is_empty() {
                    diagnostics.push(
                        Diagnostic::error(format!("THIR→MIR transformation failed: {}", e))
                            .with_source_context(STAGE_THIR_TO_MIR),
                    );
                }
                Ok(DiagnosticReport::failure(diagnostics))
            }
        }
    }

    fn run_lir_stage(
        &self,
        mir_program: mir::Program,
        options: &PipelineOptions,
        base_path: &Path,
    ) -> Result<DiagnosticReport<lir::LirProgram>, CliError> {
        let mut generator = LirGenerator::new();
        let result = generator.transform(mir_program);
        let diagnostic_manager = DiagnosticManager::new();

        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("MIR→LIR transformation failed: {}", e))
                    .with_source_context(STAGE_MIR_TO_LIR),
            );
        }

        match result {
            Ok(program) => {
                if options.save_intermediates {
                    let mut pretty_opts = PrettyOptions::default();
                    pretty_opts.show_spans = options.debug.verbose;
                    let rendered = format!("{}", pretty(&program, pretty_opts));
                    if let Err(err) = fs::write(base_path.with_extension(EXT_LIR), rendered) {
                        debug!(error = %err, "failed to persist LIR intermediate");
                    }
                }

                let diagnostics = diagnostic_manager.get_diagnostics();
                Ok(DiagnosticReport::success_with_diagnostics(
                    program,
                    diagnostics,
                ))
            }
            Err(e) => {
                // Ensure the error is captured in diagnostics if it wasn't already
                let mut diagnostics = diagnostic_manager.get_diagnostics();
                if diagnostics.is_empty() {
                    diagnostics.push(
                        Diagnostic::error(format!("MIR→LIR transformation failed: {}", e))
                            .with_source_context(STAGE_MIR_TO_LIR),
                    );
                }
                Ok(DiagnosticReport::failure(diagnostics))
            }
        }
    }

    fn run_llvm_stage(
        &self,
        lir_program: lir::LirProgram,
        base_path: &Path,
    ) -> Result<DiagnosticReport<PathBuf>, CliError> {
        let llvm_output = base_path.with_extension("ll");
        let llvm_config = fp_llvm::LlvmConfig::executable(&llvm_output);
        let llvm_compiler = fp_llvm::LlvmCompiler::new(llvm_config);

        let diagnostic_manager = DiagnosticManager::new();

        let result = llvm_compiler.compile(lir_program, None);
        if let Err(e) = &result {
            diagnostic_manager.add_diagnostic(
                Diagnostic::error(format!("LLVM IR generation failed: {}", e))
                    .with_source_context(STAGE_LIR_TO_LLVM),
            );
        }

        let llvm_ir = match result {
            Ok(path) => path,
            Err(_) => {
                let diagnostics = diagnostic_manager.get_diagnostics();
                return Ok(DiagnosticReport::failure(diagnostics));
            }
        };

        let diagnostics = diagnostic_manager.get_diagnostics();

        Ok(DiagnosticReport::success_with_diagnostics(
            llvm_ir,
            diagnostics,
        ))
    }

    pub async fn execute(
        &mut self,
        input: PipelineInput,
        config: &PipelineConfig,
    ) -> Result<PipelineOutput, CliError> {
        let options = PipelineOptions::from(config);
        self.execute_with_options(input, options).await
    }

    pub async fn execute_runtime(
        &mut self,
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
        let FrontendResult {
            ast, serializer, ..
        } = frontend.parse(source, None)?;
        register_threadlocal_serializer(serializer);

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
        options: &PipelineOptions,
        file_path: Option<&Path>,
    ) -> Result<Value, CliError> {
        let (value, _) = self
            .interpret_ast_with_mode(ast, options, file_path, InterpreterMode::Const)
            .await?;
        Ok(value)
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &Node,
        _runtime_name: &str,
        options: &PipelineOptions,
        file_path: Option<&Path>,
    ) -> Result<RuntimeValue, CliError> {
        let (value, runtime_value) = self
            .interpret_ast_with_mode(ast, options, file_path, InterpreterMode::Runtime)
            .await?;
        Ok(runtime_value.unwrap_or_else(|| value.to_runtime_owned()))
    }

    async fn interpret_ast_with_mode(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        mode: InterpreterMode,
    ) -> Result<(Value, Option<RuntimeValue>), CliError> {
        let base_path = options.base_path.as_ref().ok_or_else(|| {
            CliError::Compilation("Missing base path for interpretation".to_string())
        })?;

        let diagnostic_manager = DiagnosticManager::new();

        let hir_report = self.run_hir_stage(ast, options, file_path, base_path)?;
        let hir_program =
            self.collect_stage(STAGE_AST_TO_HIR, hir_report, &diagnostic_manager, options)?;

        let thir_report = self.run_thir_stage(hir_program, options, base_path)?;
        let thir_program =
            self.collect_stage(STAGE_HIR_TO_THIR, thir_report, &diagnostic_manager, options)?;

        let const_report = self.run_const_eval_stage(thir_program, options, base_path)?;
        let ConstEvalArtifacts {
            thir_program,
            outcome,
        } = self.collect_stage(STAGE_CONST_EVAL, const_report, &diagnostic_manager, options)?;

        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation(
                "Frontend did not register serializer for interpretation".to_string(),
            )
        })?;
        register_threadlocal_serializer(serializer.clone());

        let mut interpreter = InterpretationOrchestrator::new(mode);
        let interpreter_diagnostics = Arc::new(DiagnosticManager::new());
        interpreter.set_diagnostics(Some(interpreter_diagnostics.clone()));

        let value =
            self.evaluate_program_entry(&thir_program, &mut interpreter, &outcome.values)?;

        let runtime_value = if matches!(mode, InterpreterMode::Runtime) {
            Some(interpreter.finalize_runtime_value(value.clone()))
        } else {
            None
        };

        let interp_diags = interpreter_diagnostics.get_diagnostics();
        self.emit_diagnostics(&interp_diags, Some(STAGE_INTERPRET), options);
        diagnostic_manager.add_diagnostics(interp_diags);

        Ok((value, runtime_value))
    }

    fn emit_diagnostics(
        &self,
        diagnostics: &[Diagnostic],
        stage_context: Option<&str>,
        options: &PipelineOptions,
    ) {
        if diagnostics.is_empty() {
            return;
        }

        let display_options = self.diagnostic_display_options(options);
        DiagnosticManager::emit(diagnostics, stage_context, &display_options);
    }

    fn collect_stage<T>(
        &self,
        stage: &'static str,
        report: DiagnosticReport<T>,
        manager: &DiagnosticManager,
        options: &PipelineOptions,
    ) -> Result<T, CliError> {
        let DiagnosticReport { value, diagnostics } = report;
        self.emit_diagnostics(&diagnostics, Some(stage), options);
        manager.add_diagnostics(diagnostics.clone());

        value.ok_or_else(|| {
            CliError::Compilation(format!(
                "{} stage failed; see diagnostics for details",
                stage
            ))
        })
    }

    fn diagnostic_display_options(&self, options: &PipelineOptions) -> DiagnosticDisplayOptions {
        DiagnosticDisplayOptions::with_template(
            self.diagnostic_template.clone(),
            options.debug.verbose,
        )
    }

    fn evaluate_program_entry(
        &self,
        program: &thir::Program,
        interpreter: &mut InterpretationOrchestrator,
        const_values: &std::collections::HashMap<thir::ty::DefId, Value>,
    ) -> Result<Value, CliError> {
        let (_body_id, body) = self.find_entry_body(program).ok_or_else(|| {
            CliError::Compilation("No executable entry point found in THIR program".to_string())
        })?;

        let shared_context = SharedScopedContext::new();

        interpreter
            .evaluate_body(body, program, &shared_context, const_values)
            .map_err(|e| CliError::Compilation(format!("Interpretation failed: {}", e)))
    }

    fn find_entry_body<'a>(
        &self,
        program: &'a thir::Program,
    ) -> Option<(thir::BodyId, &'a thir::Body)> {
        let mut fallback = None;

        for item in &program.items {
            if let thir::ItemKind::Function(function) = &item.kind {
                if let Some(body_id) = function.body_id {
                    if let Some(body) = program.bodies.get(&body_id) {
                        if !function.is_const {
                            return Some((body_id, body));
                        }
                        if fallback.is_none() {
                            fallback = Some((body_id, body));
                        }
                    }
                }
            }
        }

        fallback
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
