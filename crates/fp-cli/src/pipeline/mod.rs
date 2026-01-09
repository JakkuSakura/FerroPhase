use crate::CliError;
use crate::codegen::CodeGenerator;
use crate::languages::frontend::{
    FerroFrontend, FlatbuffersFrontend, FrontendResult, FrontendSnapshot, JsonSchemaFrontend,
    LanguageFrontend, PrqlFrontend, SqlFrontend, TypeScriptFrontend, WitFrontend,
};
use crate::languages::{self, detect_source_language};
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{
    AstSerializer, File, Ident, Item, ItemChunk, ItemImportTree, ItemKind, Module, Node, NodeKind,
    RuntimeValue, Value, Visibility,
};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{
    Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager,
};
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::pretty::{PrettyOptions, pretty};
use fp_core::{hir, lir};
use fp_interpret::const_eval::ConstEvalOutcome;
use fp_interpret::engine::{
    AstInterpreter, InterpreterMode, InterpreterOptions, InterpreterOutcome,
};
use fp_llvm::{LlvmCompiler, LlvmConfig, linking::LinkerConfig};
use fp_optimize::transformations::{HirGenerator, LirGenerator, MirLowering};
use fp_bytecode;
use fp_pipeline::{
    PipelineBuilder, PipelineConfig, PipelineDiagnostics, PipelineError, PipelineOptions,
    PipelineStage, PipelineTarget,
};
use fp_typescript::frontend::TsParseMode;
use fp_typing::TypingDiagnosticLevel;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info_span, warn};

// Begin internal submodules extracted for clarity
mod artifacts;
mod diagnostics;
mod stages;
use self::diagnostics as diag;
use artifacts::LlvmArtifacts;

const STAGE_FRONTEND: &str = "frontend";
const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_HIR_TO_MIR: &str = "hir→mir";
const STAGE_MIR_TO_LIR: &str = "mir→lir";
const STAGE_RUNTIME_MATERIALIZE: &str = "materialize-runtime";
const STAGE_TYPE_POST_MATERIALIZE: &str = "ast→typed(post-materialize)";
const STAGE_LINK_BINARY: &str = "link-binary";
const STAGE_INTRINSIC_NORMALIZE: &str = "intrinsic-normalize";
const STAGE_AST_INTERPRET: &str = "ast-interpret";

fn stage_enabled(options: &PipelineOptions, stage: &str) -> bool {
    !options.disabled_stages.iter().any(|s| s == stage)
}

const EXT_AST: &str = "ast";
const EXT_AST_TYPED: &str = "ast-typed";
const EXT_AST_EVAL: &str = "ast-eval";
const EXT_HIR: &str = "hir";

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
    Binary(PathBuf),
}

// LlvmArtifacts moved to pipeline::artifacts

pub struct Pipeline {
    frontends: HashMap<String, Arc<dyn LanguageFrontend>>,
    default_runtime: String,
    serializer: Option<Arc<dyn AstSerializer>>,
    intrinsic_normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
    source_language: Option<String>,
    frontend_snapshot: Option<FrontendSnapshot>,
    last_const_eval: Option<ConstEvalOutcome>,
    typescript_frontend: Option<Arc<TypeScriptFrontend>>,
    typescript_parse_mode: TsParseMode,
}

#[derive(Debug, Clone, Default)]
pub struct TranspilePreparationOptions {
    pub run_const_eval: bool,
    pub save_intermediates: bool,
    pub base_path: Option<PathBuf>,
}

impl Pipeline {
    pub fn new() -> Self {
        let mut frontends: HashMap<String, Arc<dyn LanguageFrontend>> = HashMap::new();
        let mut register = |fe: Arc<dyn LanguageFrontend>| {
            let key = fe.language().to_lowercase();
            frontends.insert(key, fe.clone());
            for ext in fe.extensions() {
                frontends.insert(ext.to_lowercase(), fe.clone());
            }
        };
        let ferro_frontend: Arc<dyn LanguageFrontend> = Arc::new(FerroFrontend::new());
        register(ferro_frontend);
        let wit_frontend: Arc<dyn LanguageFrontend> = Arc::new(WitFrontend::new());
        register(wit_frontend);
        let ts_frontend_concrete = Arc::new(TypeScriptFrontend::new(TsParseMode::Loose));
        let ts_frontend: Arc<dyn LanguageFrontend> = ts_frontend_concrete.clone();
        register(ts_frontend);
        let sql_frontend: Arc<dyn LanguageFrontend> = Arc::new(SqlFrontend::new());
        register(sql_frontend);
        let prql_frontend: Arc<dyn LanguageFrontend> = Arc::new(PrqlFrontend::new());
        register(prql_frontend);
        let jsonschema_frontend: Arc<dyn LanguageFrontend> = Arc::new(JsonSchemaFrontend::new());
        register(jsonschema_frontend);
        let flatbuffers_frontend: Arc<dyn LanguageFrontend> = Arc::new(FlatbuffersFrontend::new());
        register(flatbuffers_frontend);
        Self {
            frontends,
            default_runtime: "literal".to_string(),
            serializer: None,
            intrinsic_normalizer: None,
            source_language: None,
            frontend_snapshot: None,
            last_const_eval: None,
            typescript_frontend: Some(ts_frontend_concrete),
            typescript_parse_mode: TsParseMode::Loose,
        }
    }

    pub fn with_runtime(runtime_name: &str) -> Self {
        let mut pipeline = Self::new();
        pipeline.default_runtime = runtime_name.to_string();
        pipeline
    }

    // with_frontend_registry removed; pipeline owns its registered frontends

    pub fn set_serializer(&mut self, serializer: Arc<dyn AstSerializer>) {
        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer);
    }

    pub fn set_runtime(&mut self, runtime_name: &str) {
        self.default_runtime = runtime_name.to_string();
    }

    pub fn last_const_eval_outcome(&self) -> Option<&ConstEvalOutcome> {
        self.last_const_eval.as_ref()
    }

    pub fn set_typescript_parse_mode(&mut self, mode: TsParseMode) {
        if let Some(frontend) = &self.typescript_frontend {
            frontend.set_parse_mode(mode);
        }
        self.typescript_parse_mode = mode;
    }

    pub fn typescript_parse_mode(&self) -> TsParseMode {
        self.typescript_parse_mode
    }

    pub fn typescript_frontend(&self) -> Option<Arc<TypeScriptFrontend>> {
        self.typescript_frontend.as_ref().map(Arc::clone)
    }

    pub fn take_last_const_eval_stdout(&mut self) -> Option<Vec<String>> {
        self.last_const_eval
            .as_mut()
            .map(|outcome| std::mem::take(&mut outcome.stdout))
    }

    pub async fn execute(
        &mut self,
        input: PipelineInput,
        config: &PipelineConfig,
    ) -> Result<PipelineOutput, CliError> {
        let options: PipelineOptions = config.into();
        self.execute_with_options(input, options).await
    }

    pub fn parse_source_public(
        &mut self,
        source: &str,
        path: Option<&Path>,
    ) -> Result<Node, CliError> {
        let language_key = path
            .and_then(|path| detect_source_language(path).map(|lang| lang.name))
            .unwrap_or(languages::FERROPHASE);

        let frontend = self
            .frontends
            .get(language_key)
            .cloned()
            .or_else(|| self.frontends.get(languages::FERROPHASE).cloned())
            .ok_or_else(|| CliError::Compilation("No suitable frontend registered".to_string()))?;

        let FrontendResult {
            ast,
            serializer,
            intrinsic_normalizer,
            snapshot,
            diagnostics,
            ..
        } = frontend
            .parse(source, path)
            .map_err(|err| CliError::Compilation(err.to_string()))?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        if !collected_diagnostics.is_empty() {
            DiagnosticManager::emit(
                &collected_diagnostics,
                Some(STAGE_FRONTEND),
                &DiagnosticDisplayOptions::default(),
            );
        }

        let has_errors = collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        let treat_as_errors = if language_key == languages::TYPESCRIPT
            && matches!(self.typescript_parse_mode, TsParseMode::Loose)
        {
            false
        } else {
            has_errors
        };

        if treat_as_errors {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.frontend_snapshot = snapshot;
        self.source_language = Some(frontend.language().to_string());

        Ok(ast)
    }

    fn parse_input_source(
        &mut self,
        options: &PipelineOptions,
        source: &str,
        input_path: Option<&PathBuf>,
    ) -> Result<Node, CliError> {
        let language = self.resolve_language(options, input_path);
        let frontend = self.frontends.get(&language).cloned().ok_or_else(|| {
            CliError::Compilation(format!("Unsupported source language: {}", language))
        })?;

        let ast =
            self.parse_with_frontend(&frontend, source, input_path.map(|p| p.as_path()), options)?;
        if language == languages::FERROPHASE {
            if let Some(path) = input_path {
                return self.resolve_file_modules(options, &frontend, ast, path);
            }
        }
        Ok(ast)
    }

    #[cfg(test)]
    pub fn parse_source_with_path_for_tests(
        &mut self,
        source: &str,
        path: &Path,
    ) -> Result<Node, CliError> {
        self.parse_source_public(source, Some(path))
    }

    pub async fn execute_with_options(
        &mut self,
        input: PipelineInput,
        mut options: PipelineOptions,
    ) -> Result<PipelineOutput, CliError> {
        let explicit_base_path = options.base_path.clone();
        let (source, default_base_path, input_path) = self.read_input(input)?;
        options.base_path = explicit_base_path.or_else(|| Some(default_base_path.clone()));

        self.reset_state();

        let ast = {
            let path_ref = input_path.as_ref();
            self.parse_input_source(&options, &source, path_ref)?
        };

        match options.target {
            PipelineTarget::Interpret => {
                self.execute_interpret_target(ast, &options, input_path.as_deref())
                    .await
            }
            ref target => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    let msg = match target {
                        PipelineTarget::Rust => "Missing base path for transpilation",
                        PipelineTarget::Llvm => "Missing base path for LLVM generation",
                        PipelineTarget::Binary => "Missing base path for binary generation",
                        PipelineTarget::Bytecode => "Missing base path for bytecode generation",
                        PipelineTarget::Interpret => unreachable!(),
                    };
                    CliError::Compilation(msg.to_string())
                })?;

                if let NodeKind::Workspace(_) = ast.kind() {
                    return Err(CliError::Compilation(
                        "Workspace documents are not supported by fp-cli".to_string(),
                    ));
                }

                self.execute_compilation_target(
                    target,
                    ast,
                    &options,
                    base_path,
                    input_path.as_deref(),
                )
            }
        }
    }

    fn read_input(
        &self,
        input: PipelineInput,
    ) -> Result<(String, PathBuf, Option<PathBuf>), CliError> {
        let span = info_span!("pipeline.read_input");
        let _enter = span.enter();

        let (source, base_path, path) = match input {
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

        debug!(path = ?path, "loaded input source");
        Ok((source, base_path, path))
    }

    fn reset_state(&mut self) {
        self.serializer = None;
        self.intrinsic_normalizer = None;
        self.source_language = None;
        self.frontend_snapshot = None;
        self.last_const_eval = None;
    }

    fn resolve_language(
        &mut self,
        options: &PipelineOptions,
        input_path: Option<&PathBuf>,
    ) -> String {
        let language = options
            .source_language
            .clone()
            .or_else(|| {
                input_path
                    .and_then(|path| detect_source_language(path).map(|lang| lang.name.to_string()))
            })
            .unwrap_or_else(|| languages::FERROPHASE.to_string());

        self.source_language = Some(language.clone());
        language
    }

    fn parse_with_frontend(
        &mut self,
        frontend: &Arc<dyn LanguageFrontend>,
        source: &str,
        input_path: Option<&Path>,
        options: &PipelineOptions,
    ) -> Result<Node, CliError> {
        let span = info_span!("pipeline.frontend", language = %frontend.language());
        let _enter = span.enter();

        let FrontendResult {
            last: _last,
            ast,
            serializer,
            intrinsic_normalizer,
            snapshot,
            diagnostics,
        } = frontend.parse(source, input_path)?;

        let collected_diagnostics = diagnostics.get_diagnostics();
        diag::emit(&collected_diagnostics, Some(STAGE_FRONTEND), options);

        let has_errors = collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        if has_errors {
            return Err(CliError::Compilation(
                "frontend stage failed; see diagnostics for details".to_string(),
            ));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.frontend_snapshot = snapshot;

        Ok(ast)
    }

    fn resolve_file_modules(
        &mut self,
        options: &PipelineOptions,
        frontend: &Arc<dyn LanguageFrontend>,
        ast: Node,
        input_path: &PathBuf,
    ) -> Result<Node, CliError> {
        let NodeKind::File(file) = ast.kind().clone() else {
            return Ok(ast);
        };

        let root_dir = input_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));

        let mut loader = FileModuleLoader::new(self, root_dir, options, frontend);
        let module_tree = loader.collect_modules(&file, Vec::new(), input_path)?;
        if module_tree.children.is_empty() {
            return Ok(Node::file(file));
        }

        let merged_items = merge_module_items(&file.items, module_tree);
        Ok(Node::file(File {
            path: file.path,
            items: merged_items,
        }))
    }

    fn parse_module_file(
        &mut self,
        options: &PipelineOptions,
        frontend: &Arc<dyn LanguageFrontend>,
        path: &Path,
    ) -> Result<Node, CliError> {
        let source = std::fs::read_to_string(path).map_err(|err| {
            CliError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to read file {}: {err}", path.display()),
            ))
        })?;
        self.parse_with_frontend(frontend, &source, Some(path), options)
    }

    pub fn prepare_for_transpile(
        &mut self,
        ast: &mut Node,
        options: &TranspilePreparationOptions,
    ) -> Result<(), CliError> {
        let mut pipeline_options = PipelineOptions::default();
        pipeline_options.save_intermediates = options.save_intermediates;
        pipeline_options.base_path = options.base_path.clone();

        if stage_enabled(&pipeline_options, STAGE_INTRINSIC_NORMALIZE) {
            self.stage_normalize_intrinsics(ast, &pipeline_options)?;
        }

        // For transpilation we prefer to run const-eval before type checking so that
        // quote/splice expansion and other compile-time rewrites are reflected in the
        // typed AST.
        if options.run_const_eval && stage_enabled(&pipeline_options, STAGE_CONST_EVAL) {
            let outcome = self.stage_const_eval(ast, &pipeline_options)?;
            self.last_const_eval = Some(outcome.clone());
        }

        if stage_enabled(&pipeline_options, STAGE_TYPE_ENRICH) {
            self.stage_type_check(ast, STAGE_TYPE_ENRICH, &pipeline_options)?;
        }

        if options.run_const_eval && stage_enabled(&pipeline_options, STAGE_CONST_EVAL) {
            if options.save_intermediates {
                if let Some(base_path) = pipeline_options.base_path.as_ref() {
                    self.save_pretty(ast, base_path, EXT_AST_EVAL, &pipeline_options)?;
                }
            }
        }

        Ok(())
    }

    async fn execute_interpret_target(
        &mut self,
        mut ast: Node,
        options: &PipelineOptions,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        if stage_enabled(options, STAGE_INTRINSIC_NORMALIZE) {
            self.stage_normalize_intrinsics(&mut ast, options)?;
        }

        if stage_enabled(options, STAGE_CONST_EVAL) {
            self.stage_const_eval(&mut ast, options)?;
        }

        if stage_enabled(options, STAGE_TYPE_ENRICH) {
            self.stage_type_check(&mut ast, STAGE_TYPE_ENRICH, options)?;
        }
        let runtime = if options.runtime.runtime_type.is_empty() {
            self.default_runtime.clone()
        } else {
            options.runtime.runtime_type.clone()
        };

        match runtime.as_str() {
            "literal" => {
                let span = info_span!("pipeline.interpret", runtime = "literal");
                let _enter = span.enter();
                let value = self.interpret_ast(&ast, options, input_path).await?;
                Ok(PipelineOutput::Value(value))
            }
            _ => {
                let span = info_span!("pipeline.interpret", runtime = %runtime);
                let _enter = span.enter();
                let value = self
                    .interpret_ast_runtime(&ast, &runtime, options, input_path)
                    .await?;
                Ok(PipelineOutput::RuntimeValue(value))
            }
        }
    }

    fn execute_compilation_target(
        &mut self,
        target: &PipelineTarget,
        mut ast: Node,
        options: &PipelineOptions,
        base_path: &Path,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        if stage_enabled(options, STAGE_INTRINSIC_NORMALIZE) {
            self.stage_normalize_intrinsics(&mut ast, options)?;
        }

        let did_const_eval = stage_enabled(options, STAGE_CONST_EVAL);
        let outcome = if did_const_eval {
            self.stage_const_eval(&mut ast, options)?
        } else {
            ConstEvalOutcome::default()
        };
        self.last_const_eval = Some(outcome.clone());

        if matches!(target, PipelineTarget::Llvm | PipelineTarget::Binary) && !did_const_eval {
            self.inject_runtime_std(&mut ast, options)?;
        }

        if stage_enabled(options, STAGE_TYPE_ENRICH) {
            self.stage_type_check(&mut ast, STAGE_TYPE_ENRICH, options)?;
        }

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST, options)?;
            self.save_pretty(&ast, base_path, EXT_AST_TYPED, options)?;
        }

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST_EVAL, options)?;
        }

        if stage_enabled(options, STAGE_RUNTIME_MATERIALIZE) {
            self.stage_materialize_runtime_intrinsics(&mut ast, target, options)?;
        }

        if stage_enabled(options, STAGE_TYPE_POST_MATERIALIZE) {
            self.stage_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE, options)?;
        }

        let output = if matches!(target, PipelineTarget::Rust) {
            let span = info_span!("pipeline.codegen", target = "rust");
            let _enter = span.enter();
            let code = CodeGenerator::generate_rust_code(&ast)?;
            PipelineOutput::Code(code)
        } else {
            let hir_program = self.stage_hir_generation(&ast, options, input_path, base_path)?;

            match target {
                PipelineTarget::Llvm => {
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    let lir = self.stage_mir_to_lir(&mir.mir_program, options, base_path)?;

                    let llvm = self.generate_llvm_artifacts(
                        &lir.lir_program,
                        base_path,
                        input_path,
                        false,
                        options,
                    )?;
                    PipelineOutput::Code(llvm.ir_text)
                }
                PipelineTarget::Binary => {
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    let lir = self.stage_mir_to_lir(&mir.mir_program, options, base_path)?;

                    let llvm = self.generate_llvm_artifacts(
                        &lir.lir_program,
                        base_path,
                        input_path,
                        true,
                        options,
                    )?;

                    let binary_path = self.stage_link_binary(&llvm.ir_path, base_path, options)?;

                    PipelineOutput::Binary(binary_path)
                }
                PipelineTarget::Bytecode => {
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    let bytecode = fp_bytecode::lower_program(&mir.mir_program).map_err(|err| {
                        CliError::Compilation(format!("MIR→Bytecode lowering failed: {}", err))
                    })?;
                    let bytecode_path = base_path.to_path_buf();
                    if let Some(parent) = bytecode_path.parent() {
                        fs::create_dir_all(parent)?;
                    }
                    if bytecode_path
                        .extension()
                        .and_then(|ext| ext.to_str())
                        == Some("ftbc")
                    {
                        let rendered = fp_bytecode::format_program(&bytecode);
                        fs::write(&bytecode_path, rendered)?;
                    } else {
                        let bytes = fp_bytecode::encode_file(&bytecode).map_err(|err| {
                            CliError::Compilation(format!("Bytecode encoding failed: {}", err))
                        })?;
                        fs::write(&bytecode_path, bytes)?;
                    }

                    PipelineOutput::Binary(bytecode_path)
                }
                PipelineTarget::Rust | PipelineTarget::Interpret => unreachable!(),
            }
        };

        Ok(output)
    }

    fn inject_runtime_std(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        let mut diagnostics = PipelineDiagnostics::default();
        for std_path in runtime_std_paths() {
            let source = match fs::read_to_string(&std_path) {
                Ok(source) => source,
                Err(err) => {
                    diagnostics.push(
                        Diagnostic::error(format!(
                            "Failed to read runtime std module at {}: {}",
                            std_path.display(),
                            err
                        ))
                        .with_source_context(STAGE_RUNTIME_MATERIALIZE),
                    );
                    diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE, options);
                    return Err(Self::stage_failure(STAGE_RUNTIME_MATERIALIZE));
                }
            };
            let std_node = self.parse_source_public(&source, Some(&std_path))?;
            let merged = merge_std_module(
                ast.clone(),
                std_node,
                &mut diagnostics,
                STAGE_RUNTIME_MATERIALIZE,
            )
            .map_err(|_| {
                diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE, options);
                Self::stage_failure(STAGE_RUNTIME_MATERIALIZE)
            })?;
            *ast = merged;
        }
        diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE, options);
        Ok(())
    }

    fn save_pretty(
        &self,
        ast: &Node,
        base_path: &Path,
        extension: &str,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        if !options.save_intermediates {
            return Ok(());
        }

        use fp_core::pretty::{PrettyOptions, pretty};

        let pretty_options = PrettyOptions {
            show_types: extension != EXT_AST,
            show_spans: options.debug.verbose,
            indent_size: 2,
        };
        let rendered = format!("{}", pretty(ast, pretty_options));

        if let Err(err) = fs::write(base_path.with_extension(extension), rendered) {
            debug!(
                error = %err,
                extension = extension,
                "failed to persist {} intermediate",
                extension
            );
        }

        Ok(())
    }

    fn run_ast_interpreter(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        mode: InterpreterMode,
    ) -> Result<(Value, InterpreterOutcome), CliError> {
        let mut working_ast = ast.clone();
        let ctx = SharedScopedContext::new();
        let interpreter_opts = InterpreterOptions {
            mode,
            debug_assertions: !options.release,
            diagnostics: None,
            diagnostic_context: STAGE_AST_INTERPRET,
            module_resolution: None,
        };
        let mut interpreter = AstInterpreter::new(&ctx, interpreter_opts);

        let value = match working_ast.kind_mut() {
            NodeKind::File(_) => {
                interpreter.interpret(&mut working_ast);
                interpreter.execute_main().unwrap_or_else(Value::unit)
            }
            NodeKind::Expr(expr) => interpreter.evaluate_expression(expr),
            NodeKind::Item(_) => {
                return Err(CliError::Compilation(
                    "Standalone item interpretation is not supported".to_string(),
                ));
            }
            NodeKind::Query(_) => {
                return Err(CliError::Compilation(
                    "Query documents cannot be interpreted".to_string(),
                ));
            }
            NodeKind::Schema(_) => {
                return Err(CliError::Compilation(
                    "Schema documents cannot be interpreted".to_string(),
                ));
            }
            NodeKind::Workspace(_) => {
                return Err(CliError::Compilation(
                    "Workspace documents cannot be interpreted".to_string(),
                ));
            }
        };

        let outcome = interpreter.take_outcome();
        diag::emit(&outcome.diagnostics, Some(STAGE_AST_INTERPRET), options);

        if outcome.has_errors {
            return Err(CliError::Compilation(
                "AST interpretation failed; see diagnostics for details".to_string(),
            ));
        }

        Ok((value, outcome))
    }

    async fn interpret_ast(
        &self,
        ast: &Node,
        options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<Value, CliError> {
        let (value, mut outcome) =
            self.run_ast_interpreter(ast, options, InterpreterMode::RunTime)?;

        if !outcome.stdout.is_empty() {
            for chunk in outcome.stdout.drain(..) {
                print!("{}", chunk);
            }
            let _ = io::stdout().flush();
        }

        Ok(value)
    }

    async fn interpret_ast_runtime(
        &self,
        ast: &Node,
        runtime_name: &str,
        options: &PipelineOptions,
        _file_path: Option<&Path>,
    ) -> Result<RuntimeValue, CliError> {
        match runtime_name {
            "rust" | "literal" => {
                let (value, mut outcome) =
                    self.run_ast_interpreter(ast, options, InterpreterMode::RunTime)?;

                if !outcome.stdout.is_empty() {
                    for chunk in outcome.stdout.drain(..) {
                        print!("{}", chunk);
                    }
                    let _ = io::stdout().flush();
                }

                Ok(RuntimeValue::literal(value))
            }
            other => Err(CliError::Compilation(format!(
                "Unsupported interpreter runtime: {}",
                other
            ))),
        }
    }

    fn run_pipeline_stage<Src, Dst, S>(
        &self,
        stage_label: &'static str,
        stage: S,
        context: Src,
        options: &PipelineOptions,
    ) -> Result<Dst, CliError>
    where
        S: PipelineStage<SrcCtx = Src, DstCtx = Dst> + 'static,
        Src: 'static,
        Dst: 'static,
    {
        let mut diagnostics = PipelineDiagnostics::default();
        let pipeline = PipelineBuilder::<Src, Src>::new().add_stage(stage).build();
        match pipeline.run(context, &mut diagnostics, options) {
            Ok(output) => Ok(output),
            Err(_) => Err(Self::stage_failure(stage_label)),
        }
    }

    fn stage_failure(stage: &str) -> CliError {
        CliError::Compilation(format!(
            "{} stage failed; see diagnostics for details",
            stage
        ))
    }

    // moved to pipeline::diagnostics
}

fn runtime_std_paths() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    vec![
        root.join("../../src/std/collection/hashmap.fp"),
        root.join("../../src/std/test/mod.fp"),
    ]
}

fn merge_std_module(
    ast: Node,
    std_node: Node,
    diagnostics: &mut PipelineDiagnostics,
    stage: &'static str,
) -> Result<Node, PipelineError> {
    let Node { ty, kind } = ast;
    let Node { kind: std_kind, .. } = std_node;
    let NodeKind::File(mut file) = kind else {
        diagnostics.push(
            Diagnostic::error("std injection expects a file AST".to_string())
                .with_source_context(stage),
        );
        return Err(PipelineError::new(
            stage,
            "std injection expects a file AST",
        ));
    };
    let NodeKind::File(std_file) = std_kind else {
        diagnostics.push(
            Diagnostic::error("std module must be a file".to_string()).with_source_context(stage),
        );
        return Err(PipelineError::new(stage, "std module must be a file"));
    };
    let mut std_module = None;
    let mut std_items = Vec::new();
    for item in std_file.items {
        if let fp_core::ast::ItemKind::Module(module) = item.kind() {
            if module.name.as_str() == "std" {
                std_module = Some(module.clone());
                continue;
            }
        }
        std_items.push(item);
    }
    let Some(mut std_module) = std_module else {
        diagnostics.push(
            Diagnostic::error("std file must define module std".to_string())
                .with_source_context(stage),
        );
        return Err(PipelineError::new(stage, "std file must define module std"));
    };
    std_module.items.extend(std_items);

    let mut merged_into_existing = false;
    for item in &mut file.items {
        if let fp_core::ast::ItemKind::Module(existing) = item.kind_mut() {
            if existing.name.as_str() == "std" {
                existing.items.extend(std::mem::take(&mut std_module.items));
                merged_into_existing = true;
                break;
            }
        }
    }
    if !merged_into_existing {
        let mut items = Vec::with_capacity(file.items.len() + 1);
        items.push(Item::from(fp_core::ast::ItemKind::Module(std_module)));
        items.append(&mut file.items);
        file.items = items;
    }
    Ok(Node {
        ty,
        kind: NodeKind::File(file),
    })
}

#[derive(Default, Clone)]
struct ModuleTree {
    items: Option<ItemChunk>,
    children: BTreeMap<String, ModuleTree>,
}

impl ModuleTree {
    fn insert(&mut self, path: &[String], items: ItemChunk) {
        if path.is_empty() {
            return;
        }
        let head = path[0].clone();
        let child = self
            .children
            .entry(head)
            .or_insert_with(ModuleTree::default);
        if path.len() == 1 {
            if child.items.is_none() {
                child.items = Some(items);
            }
            return;
        }
        child.insert(&path[1..], items);
    }
}

fn merge_module_items(existing: &ItemChunk, tree: ModuleTree) -> ItemChunk {
    let mut remaining_children = tree.children;
    let mut out = Vec::new();

    for item in existing {
        if let ItemKind::Module(module) = item.kind() {
            if let Some(child) = remaining_children.remove(module.name.as_str()) {
                let merged_items = merge_module_items(&module.items, child);
                let merged_module = Module {
                    name: module.name.clone(),
                    items: merged_items,
                    visibility: module.visibility.clone(),
                };
                out.push(Item {
                    ty: item.ty.clone(),
                    kind: ItemKind::Module(merged_module),
                });
            } else {
                out.push(item.clone());
            }
        } else {
            out.push(item.clone());
        }
    }

    for (name, child) in remaining_children {
        let items = child.items.clone().unwrap_or_default();
        let merged_items = merge_module_items(&items, child);
        let module = Module {
            name: Ident::new(name),
            items: merged_items,
            visibility: Visibility::Public,
        };
        out.push(Item::new(ItemKind::Module(module)));
    }

    out
}

struct FileModuleLoader<'a> {
    pipeline: &'a mut Pipeline,
    root_dir: PathBuf,
    options: &'a PipelineOptions,
    frontend: &'a Arc<dyn LanguageFrontend>,
    visited_files: HashSet<PathBuf>,
    visited_modules: HashSet<Vec<String>>,
    inline_modules: HashSet<Vec<String>>,
}

impl<'a> FileModuleLoader<'a> {
    fn new(
        pipeline: &'a mut Pipeline,
        root_dir: PathBuf,
        options: &'a PipelineOptions,
        frontend: &'a Arc<dyn LanguageFrontend>,
    ) -> Self {
        Self {
            pipeline,
            root_dir,
            options,
            frontend,
            visited_files: HashSet::new(),
            visited_modules: HashSet::new(),
            inline_modules: HashSet::new(),
        }
    }

    fn collect_modules(
        &mut self,
        file: &File,
        module_path: Vec<String>,
        entry_path: &PathBuf,
    ) -> Result<ModuleTree, CliError> {
        self.register_inline_modules(&file.items, &module_path);
        let mut tree = ModuleTree::default();
        let mut queue = Vec::new();
        self.collect_import_module_paths(&file.items, &module_path, &mut queue);
        self.load_modules_from_queue(&mut tree, &mut queue, entry_path)?;
        Ok(tree)
    }

    fn register_inline_modules(&mut self, items: &ItemChunk, module_path: &[String]) {
        for item in items {
            if let ItemKind::Module(module) = item.kind() {
                let mut path = module_path.to_vec();
                path.push(module.name.as_str().to_string());
                self.inline_modules.insert(path.clone());
                self.register_inline_modules(&module.items, &path);
            }
        }
    }

    fn collect_import_module_paths(
        &mut self,
        items: &ItemChunk,
        module_path: &[String],
        queue: &mut Vec<Vec<String>>,
    ) {
        for item in items {
            match item.kind() {
                ItemKind::Import(import) => {
                    let entries = expand_import_tree(&import.tree, module_path);
                    for path in entries {
                        if let Some(first) = path.first() {
                            if matches!(first.as_str(), "std" | "core" | "alloc" | "fp_rust") {
                                continue;
                            }
                        }
                        for prefix_len in 1..=path.len() {
                            let prefix = path[..prefix_len].to_vec();
                            if self.inline_modules.contains(&prefix) {
                                continue;
                            }
                            if self.visited_modules.insert(prefix.clone()) {
                                queue.push(prefix);
                            }
                        }
                    }
                }
                ItemKind::Module(module) => {
                    let mut path = module_path.to_vec();
                    path.push(module.name.as_str().to_string());
                    self.collect_import_module_paths(&module.items, &path, queue);
                }
                _ => {}
            }
        }
    }

    fn load_modules_from_queue(
        &mut self,
        tree: &mut ModuleTree,
        queue: &mut Vec<Vec<String>>,
        entry_path: &PathBuf,
    ) -> Result<(), CliError> {
        while let Some(module_path) = queue.pop() {
            if self.inline_modules.contains(&module_path) {
                continue;
            }
            let Some(module_file) = resolve_module_file(&self.root_dir, &module_path) else {
                continue;
            };
            let canonical = module_file
                .canonicalize()
                .unwrap_or_else(|_| module_file.clone());
            if !self.visited_files.insert(canonical.clone()) {
                continue;
            }

            let ast = self
                .pipeline
                .parse_module_file(self.options, self.frontend, &canonical)?;
            let NodeKind::File(file) = ast.kind() else {
                continue;
            };

            self.register_inline_modules(&file.items, &module_path);
            tree.insert(&module_path, file.items.clone());

            let mut nested_imports = Vec::new();
            self.collect_import_module_paths(&file.items, &module_path, &mut nested_imports);
            for import_path in nested_imports {
                if self.inline_modules.contains(&import_path) {
                    continue;
                }
                if self.visited_modules.insert(import_path.clone()) {
                    queue.push(import_path);
                }
            }

            if canonical == *entry_path {
                continue;
            }
        }
        Ok(())
    }
}

fn resolve_module_file(root_dir: &Path, module_path: &[String]) -> Option<PathBuf> {
    if module_path.is_empty() {
        return None;
    }
    let mut base = root_dir.to_path_buf();
    for segment in module_path {
        base.push(segment);
    }
    let flat_path = base.with_extension("fp");
    if flat_path.is_file() {
        return Some(flat_path);
    }
    let mod_path = base.join("mod.fp");
    if mod_path.is_file() {
        return Some(mod_path);
    }
    None
}

fn expand_import_tree(tree: &ItemImportTree, module_path: &[String]) -> Vec<Vec<String>> {
    expand_import_tree_with_base(tree, Vec::new(), module_path)
}

fn expand_import_tree_with_base(
    tree: &ItemImportTree,
    base: Vec<String>,
    module_path: &[String],
) -> Vec<Vec<String>> {
    match tree {
        ItemImportTree::Path(path) => expand_import_segments(&path.segments, base, module_path),
        ItemImportTree::Group(group) => {
            let mut results = Vec::new();
            for item in &group.items {
                results.extend(expand_import_tree_with_base(
                    item,
                    base.clone(),
                    module_path,
                ));
            }
            results
        }
        ItemImportTree::Root => expand_import_segments(&[], Vec::new(), module_path),
        ItemImportTree::SelfMod => expand_import_segments(&[], module_path.to_vec(), module_path),
        ItemImportTree::SuperMod => {
            let mut parent = module_path.to_vec();
            parent.pop();
            expand_import_segments(&[], parent, module_path)
        }
        ItemImportTree::Crate => expand_import_segments(&[], Vec::new(), module_path),
        ItemImportTree::Glob => Vec::new(),
        _ => expand_import_segments(std::slice::from_ref(tree), base, module_path),
    }
}

fn expand_import_segments(
    segments: &[ItemImportTree],
    base: Vec<String>,
    module_path: &[String],
) -> Vec<Vec<String>> {
    if segments.is_empty() {
        return Vec::new();
    }

    let first = &segments[0];
    let rest = &segments[1..];
    match first {
        ItemImportTree::Ident(ident) => {
            let name = ident.name.as_str();
            let mut new_base = base;
            match name {
                "self" => new_base = module_path.to_vec(),
                "super" => {
                    let mut parent = module_path.to_vec();
                    parent.pop();
                    new_base = parent;
                }
                "crate" => new_base = Vec::new(),
                _ => new_base.push(ident.name.clone()),
            }

            if rest.is_empty() && !matches!(name, "self" | "super" | "crate") {
                vec![new_base]
            } else if rest.is_empty() {
                Vec::new()
            } else {
                expand_import_segments(rest, new_base, module_path)
            }
        }
        ItemImportTree::Rename(rename) => {
            if !rest.is_empty() {
                return Vec::new();
            }
            let mut new_base = base;
            new_base.push(rename.from.name.clone());
            vec![new_base]
        }
        ItemImportTree::Group(group) => {
            let mut results = Vec::new();
            for item in &group.items {
                results.extend(expand_import_tree_with_base(
                    item,
                    base.clone(),
                    module_path,
                ));
            }
            if rest.is_empty() {
                results
            } else {
                let mut final_results = Vec::new();
                for path_segments in results {
                    let mut more = expand_import_segments(rest, path_segments.clone(), module_path);
                    if more.is_empty() {
                        final_results.push(path_segments);
                    } else {
                        final_results.append(&mut more);
                    }
                }
                final_results
            }
        }
        ItemImportTree::Path(path) => {
            let nested = expand_import_segments(&path.segments, base.clone(), module_path);
            if rest.is_empty() {
                nested
            } else {
                let mut results = Vec::new();
                for path_segments in nested {
                    let mut more = expand_import_segments(rest, path_segments.clone(), module_path);
                    if more.is_empty() {
                        results.push(path_segments);
                    } else {
                        results.append(&mut more);
                    }
                }
                results
            }
        }
        ItemImportTree::Root => expand_import_segments(rest, Vec::new(), module_path),
        ItemImportTree::SelfMod => expand_import_segments(rest, module_path.to_vec(), module_path),
        ItemImportTree::SuperMod => {
            let mut parent = module_path.to_vec();
            parent.pop();
            expand_import_segments(rest, parent, module_path)
        }
        ItemImportTree::Crate => expand_import_segments(rest, Vec::new(), module_path),
        ItemImportTree::Glob => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::artifacts::LirArtifacts;
    use fp_core::intrinsics::IntrinsicCallKind;
    use fp_core::{ast, hir, lir};
    use fp_pipeline::PipelineTarget;
    use std::collections::HashSet;
    use std::path::{Path, PathBuf};

    struct PipelineHarness {
        pipeline: Pipeline,
        options: PipelineOptions,
    }

    impl PipelineHarness {
        fn new(target: PipelineTarget) -> Self {
            let pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = target.clone();
            options.base_path = Some(PathBuf::from("unit_test_output"));
            Self { pipeline, options }
        }

        fn parse(&mut self, source: &str) -> Node {
            self.pipeline
                .parse_source_with_path_for_tests(source, Path::new("unit_test.fp"))
                .expect("frontend should succeed")
        }

        fn fail_with_error(&self, stage: &str, err: CliError) -> ! {
            panic!("{} must succeed: {:?}", stage, err);
        }

        fn normalize(&self, ast: &mut Node) {
            if let Err(err) = self.pipeline.stage_normalize_intrinsics(ast, &self.options) {
                self.fail_with_error("intrinsic normalization", err);
            }
        }

        fn type_check(&mut self, ast: &mut Node) {
            if let Err(err) = self.pipeline.stage_type_check_for_tests(ast) {
                self.fail_with_error("type checking", err);
            }
        }

        fn rerun_type_check(&mut self, ast: &mut Node, stage: &'static str) {
            if let Err(err) = self.pipeline.stage_type_check(ast, stage, &self.options) {
                self.fail_with_error(stage, err);
            }
        }

        fn materialize_runtime(&self, ast: &mut Node, target: PipelineTarget) {
            if let Err(err) =
                self.pipeline
                    .stage_materialize_runtime_intrinsics(ast, &target, &self.options)
            {
                self.fail_with_error("runtime materialisation", err);
            }
        }

        fn const_eval(&mut self, ast: &mut Node) -> ConstEvalOutcome {
            let previous = self.options.execute_main;
            self.options.execute_main = true;
            let outcome = match self.pipeline.stage_const_eval(ast, &self.options) {
                Ok(outcome) => outcome,
                Err(err) => self.fail_with_error("const evaluation", err),
            };
            self.options.execute_main = previous;
            outcome
        }

        fn hir(&mut self, ast: &Node) -> hir::Program {
            match self.pipeline.stage_hir_generation(
                ast,
                &self.options,
                None,
                Path::new("unit_test"),
            ) {
                Ok(program) => program,
                Err(err) => self.fail_with_error("AST→HIR lowering", err),
            }
        }

        fn backend(&self, hir: &hir::Program) -> LirArtifacts {
            let mir =
                match self
                    .pipeline
                    .stage_hir_to_mir(hir, &self.options, Path::new("unit_test"))
                {
                    Ok(artifacts) => artifacts,
                    Err(err) => self.fail_with_error("HIR→MIR lowering", err),
                };
            match self.pipeline.stage_mir_to_lir(
                &mir.mir_program,
                &self.options,
                Path::new("unit_test"),
            ) {
                Ok(artifacts) => artifacts,
                Err(err) => self.fail_with_error("MIR→LIR lowering", err),
            }
        }

        fn ensure_no_errors(&self) {
            // Pipeline stages already emit diagnostics and return errors on failure.
        }
    }

    fn stdout_lines(outcome: &ConstEvalOutcome) -> Vec<String> {
        outcome.stdout.clone()
    }

    fn assert_stdout_contains(outcome: &ConstEvalOutcome, expected: &str) {
        let lines = stdout_lines(outcome);
        assert!(
            lines.iter().any(|line| line.contains(expected)),
            "expected stdout to contain {:?}, got {:?}",
            expected,
            lines
        );
    }

    fn find_intrinsic_calls(ast: &ast::Node) -> Vec<ast::ExprIntrinsicCall> {
        struct Collector(Vec<ast::ExprIntrinsicCall>);

        impl Collector {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                match expr.kind() {
                    ast::ExprKind::IntrinsicCall(call) => self.0.push(call.clone()),
                    ast::ExprKind::Block(block) => {
                        for stmt in &block.stmts {
                            match stmt {
                                ast::BlockStmt::Expr(expr_stmt) => {
                                    self.visit_expr(expr_stmt.expr.as_ref())
                                }
                                ast::BlockStmt::Let(stmt_let) => {
                                    if let Some(init) = &stmt_let.init {
                                        self.visit_expr(init);
                                    }
                                    if let Some(on_drop) = &stmt_let.diverge {
                                        self.visit_expr(on_drop);
                                    }
                                }
                                ast::BlockStmt::Item(item) => self.visit_item(item),
                                ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
                            }
                        }
                    }
                    ast::ExprKind::If(expr_if) => {
                        self.visit_expr(&expr_if.cond);
                        self.visit_expr(&expr_if.then);
                        if let Some(elze) = &expr_if.elze {
                            self.visit_expr(elze);
                        }
                    }
                    ast::ExprKind::Loop(expr_loop) => self.visit_expr(&expr_loop.body),
                    ast::ExprKind::While(expr_while) => {
                        self.visit_expr(&expr_while.cond);
                        self.visit_expr(&expr_while.body);
                    }
                    ast::ExprKind::Match(expr_match) => {
                        for case in &expr_match.cases {
                            self.visit_expr(&case.cond);
                            self.visit_expr(&case.body);
                        }
                    }
                    ast::ExprKind::Let(expr_let) => self.visit_expr(&expr_let.expr),
                    ast::ExprKind::Assign(assign) => {
                        self.visit_expr(&assign.target);
                        self.visit_expr(&assign.value);
                    }
                    ast::ExprKind::Invoke(invoke) => {
                        for arg in &invoke.args {
                            self.visit_expr(arg);
                        }
                    }
                    ast::ExprKind::Struct(struct_expr) => {
                        self.visit_expr(struct_expr.name.as_ref());
                        for field in &struct_expr.fields {
                            if let Some(expr) = &field.value {
                                self.visit_expr(expr);
                            }
                        }
                    }
                    ast::ExprKind::Structural(structural_expr) => {
                        for field in &structural_expr.fields {
                            if let Some(expr) = &field.value {
                                self.visit_expr(expr);
                            }
                        }
                    }
                    ast::ExprKind::IntrinsicContainer(collection) => match collection {
                        ast::ExprIntrinsicContainer::VecElements { elements } => {
                            for elem in elements {
                                self.visit_expr(elem);
                            }
                        }
                        ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                            self.visit_expr(elem);
                            self.visit_expr(len);
                        }
                        ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                            for entry in entries {
                                self.visit_expr(&entry.key);
                                self.visit_expr(&entry.value);
                            }
                        }
                    },
                    ast::ExprKind::Array(array_expr) => {
                        for elem in &array_expr.values {
                            self.visit_expr(elem);
                        }
                    }
                    ast::ExprKind::ArrayRepeat(repeat) => {
                        self.visit_expr(&repeat.elem);
                        self.visit_expr(&repeat.len);
                    }
                    ast::ExprKind::Tuple(tuple_expr) => {
                        for elem in &tuple_expr.values {
                            self.visit_expr(elem);
                        }
                    }
                    ast::ExprKind::BinOp(binop) => {
                        self.visit_expr(&binop.lhs);
                        self.visit_expr(&binop.rhs);
                    }
                    ast::ExprKind::UnOp(unop) => self.visit_expr(&unop.val),
                    ast::ExprKind::Reference(reference) => self.visit_expr(&reference.referee),
                    ast::ExprKind::Dereference(deref) => self.visit_expr(&deref.referee),
                    ast::ExprKind::Select(select) => self.visit_expr(&select.obj),
                    ast::ExprKind::Index(index_expr) => {
                        self.visit_expr(&index_expr.obj);
                        self.visit_expr(&index_expr.index);
                    }
                    ast::ExprKind::Cast(cast_expr) => self.visit_expr(&cast_expr.expr),
                    ast::ExprKind::Closure(closure) => self.visit_expr(&closure.body),
                    ast::ExprKind::Closured(closured) => {
                        self.visit_expr(closured.expr.as_ref());
                    }
                    ast::ExprKind::Try(expr_try) => self.visit_expr(&expr_try.expr),
                    ast::ExprKind::Paren(paren) => self.visit_expr(&paren.expr),
                    ast::ExprKind::FormatString(format) => {
                        for arg in &format.args {
                            self.visit_expr(arg);
                        }
                    }
                    ast::ExprKind::Quote(q) => {
                        for stmt in &q.block.stmts {
                            match stmt {
                                ast::BlockStmt::Expr(expr_stmt) => {
                                    self.visit_expr(expr_stmt.expr.as_ref())
                                }
                                ast::BlockStmt::Let(stmt_let) => {
                                    if let Some(init) = &stmt_let.init {
                                        self.visit_expr(init);
                                    }
                                    if let Some(on_drop) = &stmt_let.diverge {
                                        self.visit_expr(on_drop);
                                    }
                                }
                                ast::BlockStmt::Item(item) => self.visit_item(item),
                                ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
                            }
                        }
                    }
                    ast::ExprKind::Splice(s) => self.visit_expr(&s.token),
                    ast::ExprKind::Value(_)
                    | ast::ExprKind::Locator(_)
                    | ast::ExprKind::Id(_)
                    | ast::ExprKind::Item(_)
                    | ast::ExprKind::Any(_) => {}
                    ast::ExprKind::Splat(splat) => self.visit_expr(&splat.iter),
                    ast::ExprKind::SplatDict(splat) => self.visit_expr(&splat.dict),
                    ast::ExprKind::Range(range) => {
                        if let Some(start) = &range.start {
                            self.visit_expr(start);
                        }
                        if let Some(end) = &range.end {
                            self.visit_expr(end);
                        }
                        if let Some(step) = &range.step {
                            self.visit_expr(step);
                        }
                    }
                    _ => {}
                }
            }

            fn visit_item(&mut self, item: &ast::Item) {
                match item.kind() {
                    ast::ItemKind::DefFunction(function) => self.visit_expr(function.body.as_ref()),
                    ast::ItemKind::DefConst(def) => self.visit_expr(&def.value),
                    ast::ItemKind::DefStatic(def) => self.visit_expr(&def.value),
                    ast::ItemKind::Impl(_) => {}
                    ast::ItemKind::Module(module) => {
                        for child in &module.items {
                            self.visit_item(child);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut collector = Collector(Vec::new());
        match ast.kind() {
            ast::NodeKind::File(file) => {
                for item in &file.items {
                    collector.visit_item(item);
                }
            }
            ast::NodeKind::Expr(expr) => collector.visit_expr(expr),
            ast::NodeKind::Item(item) => collector.visit_item(item),
            ast::NodeKind::Query(_) | ast::NodeKind::Schema(_) | ast::NodeKind::Workspace(_) => {}
        }
        collector.0
    }

    #[test]
    fn rust_frontend_parses_expression() {
        let mut pipeline = Pipeline::new();
        assert!(pipeline.parse_source_public("1 + 2", None).is_ok());
    }

    #[test]
    fn example01_const_blocks_and_arithmetic() {
        let source = r#"
fn main() {
    const BUFFER: i64 = 1024 * 4;
    const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
    let optimized = const { BUFFER / 1024 };
    let strategy = const {
        if FACTORIAL_5 > 100 { "large" } else { "small" }
    };
    println!("{} {}", optimized, strategy);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        assert!(matches!(ast.kind(), ast::NodeKind::File(_)));
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "4 large");
        harness.ensure_no_errors();
    }

    #[test]
    fn example02_string_processing_len_checks() {
        let source = r#"
fn main() {
    const NAME: &str = "FerroPhase";
    const VERSION: &str = "0.1.0";
    const NAME_LEN: usize = 10;
    const VERSION_LEN: usize = 5;

    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);

    const IS_EMPTY: bool = NAME_LEN == 0;
    const IS_LONG: bool = NAME_LEN > 5;
    println!("empty={}, long={}", IS_EMPTY, IS_LONG);

    const BANNER: &str = "FerroPhase v0.1.0";
    println!("banner='{}'", BANNER);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "name='FerroPhase' len=10");
        assert_stdout_contains(&outcome, "version='0.1.0' len=5");
        assert_stdout_contains(&outcome, "empty=false, long=true");
        assert_stdout_contains(&outcome, "banner='FerroPhase v0.1.0'");
        harness.ensure_no_errors();
    }

    #[test]
    fn example03_control_flow_const_evaluation() {
        let source = r#"
fn main() {
    const TEMP: i64 = 25;
    const WEATHER: &str = if TEMP > 30 { "hot" } else if TEMP > 20 { "warm" } else { "cold" };
    const ACTIVITY: &str = if WEATHER == "warm" { "outdoor" } else { "indoor" };
    println!("{} {}", WEATHER, ACTIVITY);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "warm outdoor");
        harness.ensure_no_errors();
    }

    #[test]
    fn example04_struct_introspection_intrinsics() {
        let source = r#"
struct Point {
    x: f64,
    y: f64,
}

const SIZE: usize = sizeof!(Point);
const FIELDS: usize = field_count!(Point);
const HAS_X: bool = hasfield!(Point, "x");
const METHODS: usize = method_count!(Point);

fn main() {
    println!("{} {} {} {}", SIZE, FIELDS, HAS_X, METHODS);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "16 2 true 0");
        harness.ensure_no_errors();
    }

    #[test]
    fn example05_struct_generation_with_const_switches() {
        let source = r#"
struct Config {
    x: i64,
    y: i64,
}

const FLAG_A: bool = true;
const FLAG_B: bool = false;

const CONFIG: Config = Config {
    x: if FLAG_A { 100 } else { 10 },
    y: if FLAG_B { 200 } else { 20 },
};

fn main() {
    println!("{} {}", CONFIG.x, CONFIG.y);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "100 20");
        harness.ensure_no_errors();
    }

    #[test]
    fn example06_struct_methods_and_impls() {
        let source = r#"
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn translate(&mut self, dx: i64, dy: i64) {
        self.x += dx;
        self.y += dy;
    }

    fn distance2(&self) -> i64 {
        self.x * self.x + self.y * self.y
    }
}

fn main() {
    let mut p = Point { x: 3, y: 4 };
    p.translate(1, 2);
    println!("{}", p.distance2());
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_methods.fp"))
            .expect("frontend should parse struct methods");
        let mut options = PipelineOptions::default();
        options.execute_main = true;

        pipeline
            .stage_normalize_intrinsics(&mut ast, &options)
            .expect("normalization should succeed for struct methods");
        pipeline
            .stage_type_check_for_tests(&mut ast)
            .expect("type checking should succeed for struct methods");

        let result = pipeline.stage_const_eval(&mut ast, &options);
        assert!(
            result.is_err(),
            "method calls are not yet supported in const eval"
        );
    }

    #[test]
    fn example07_compile_time_validation_flags() {
        let source = r#"
struct Data {
    a: i64,
    b: i64,
    c: [u8; 16],
}

const SIZE: usize = sizeof!(Data);
const FIELDS: usize = field_count!(Data);
const SIZE_OK: bool = SIZE <= 64;
const IS_ALIGNED: bool = SIZE % 8 == 0;

fn main() {
    println!("{} {} {} {}", SIZE, FIELDS, SIZE_OK, IS_ALIGNED);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "24 3 true true");
        harness.ensure_no_errors();
    }

    #[test]
    fn example08_metaprogramming_constants() {
        let source = r#"
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

const FIELD_COUNT: usize = field_count!(Point3D);
const TYPE_NAME: &str = type_name!(Point3D);

fn main() {
    println!("{} {}", TYPE_NAME, FIELD_COUNT);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "struct Point3D");
        assert_stdout_contains(&outcome, " 3");
        harness.ensure_no_errors();
    }

    #[test]
    fn example09_higher_order_functions_lowering() {
        let source = r#"
fn apply(a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    op(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    println!("{}", apply(3, 4, add));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        let backend = harness.backend(&hir);

        let mut function_refs = HashSet::new();
        for function in &backend.lir_program.functions {
            for block in &function.basic_blocks {
                for instr in &block.instructions {
                    if let lir::LirInstructionKind::Call { function, .. } = &instr.kind {
                        if let lir::LirValue::Function(name) = function {
                            function_refs.insert(name.clone());
                        }
                    }
                }
            }
        }

        assert!(
            backend
                .lir_program
                .functions
                .iter()
                .any(|function| function.name.as_str().contains("add")),
            "expected lowered program to contain function named 'add'"
        );
        assert!(
            function_refs.iter().any(|name| name.contains("apply")),
            "expected call sites to reference the apply wrapper"
        );
        harness.ensure_no_errors();
    }

    #[test]
    fn example10_print_intrinsic_types() {
        let source = r#"
fn main() {
    println!("value: {}", 42);
    println!();
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);

        let intrinsic_calls = find_intrinsic_calls(&ast);
        assert!(
            intrinsic_calls
                .iter()
                .any(|call| call.kind == IntrinsicCallKind::Println)
        );

        let outcome = harness.const_eval(&mut ast);
        assert!(
            stdout_lines(&outcome)
                .iter()
                .any(|line| line.trim().is_empty())
        );
        harness.ensure_no_errors();
    }

    #[test]
    fn example11_function_specialisation_patterns() {
        let source = r#"
fn add(a: i64, b: i64) -> i64 { a + b }
fn double(x: i64) -> i64 { x * 2 }
fn compose(x: i64) -> i64 { double(add(x, 1)) }

fn main() {
    println!("{}", compose(10));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        harness.backend(&hir);
        harness.ensure_no_errors();
    }

    #[test]
    fn example12_pattern_matching_guards() {
        let source = r#"
fn classify(n: i64) -> &'static str {
    if n == 0 {
        "zero"
    } else if n < 0 {
        "negative"
    } else if n % 2 == 0 {
        "even"
    } else {
        "odd"
    }
}

fn main() {
    println!("{} {} {}", classify(-5), classify(0), classify(7));
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "negative zero odd");
        harness.ensure_no_errors();
    }

    #[test]
    fn example13_loops_and_breaks() {
        let source = r#"
fn main() {
    let mut sum = 0;
    let mut i = 0;
    while i < 5 {
        sum = sum + i;
        i = i + 1;
    }
    println!("{}", sum);
}
"#;

        let mut harness = PipelineHarness::new(PipelineTarget::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, PipelineTarget::Llvm);
        harness.rerun_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE);
        let hir = harness.hir(&ast);
        harness.backend(&hir);
        harness.ensure_no_errors();
    }

    #[test]
    fn example14_type_arithmetic_struct_merge() {
        let source = r#"
type Foo = t! {
    struct {
        a: i64,
        b: i64,
    }
};

struct Bar {
    c: i64,
    d: i64,
}

type FooPlusBar = t! {
    Foo + Bar
};

fn main() {
    let value = FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
    println!("{} {} {} {}", value.a, value.b, value.c, value.d);
}
"#;
        let mut pipeline = Pipeline::new();
        let options = PipelineOptions::default();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_type_arith.fp"))
            .expect("frontend should parse type arithmetic placeholders");

        pipeline
            .stage_normalize_intrinsics(&mut ast, &options)
            .expect("normalization for type arithmetic example should succeed");

        let result = pipeline.stage_type_check_for_tests(&mut ast);
        if let Err(err) = result {
            panic!("type arithmetic example failed to type-check: {}", err);
        }
    }

    #[test]
    fn example15_enums_and_discriminants() {
        let source = r#"
enum Value {
    A = 1,
    B = 2,
    C = 5,
}

fn main() {
    let val = Value::C;
    println!("{}", val as i32);
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_enum.fp"))
            .expect("frontend should produce an AST even for enums");
        let mut options = PipelineOptions::default();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &options)
            .expect("normalization should succeed before const-eval");
        if let Err(err) = pipeline.stage_type_check_for_tests(&mut ast) {
            panic!("type checking should succeed before hitting interpreter limitations: {err:?}");
        }

        options.execute_main = true;

        pipeline
            .stage_const_eval(&mut ast, &options)
            .expect("enum const-eval should succeed");
    }

    #[test]
    fn example16_traits_with_default_methods() {
        let source = r#"
trait Shape {
    fn area(&self) -> f64;

    fn describe(&self) {
        println!("{:.2}", self.area());
    }
}

struct Circle {
    radius: f64,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        3.14159 * self.radius * self.radius
    }
}

fn main() {
    let circle = Circle { radius: 5.0 };
    circle.describe();
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_traits.fp"))
            .expect("frontend should run even for unsupported traits");
        let options = PipelineOptions::default();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &options)
            .expect("intrinsic normalization should still succeed");

        pipeline
            .stage_type_check_for_tests(&mut ast)
            .expect("trait typing should succeed");
    }

    #[test]
    fn example17_generics_and_trait_bounds() {
        let source = r#"
fn identity<T>(value: T) -> T {
    value
}

fn main() {
    println!("{}", identity(42));
}
"#;
        let mut harness = PipelineHarness::new(PipelineTarget::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        let outcome = harness.const_eval(&mut ast);
        assert_stdout_contains(&outcome, "42");
        harness.ensure_no_errors();
    }

    #[test]
    fn example18_comptime_collections() {
        let source = r#"
fn main() {
    const THIRD: i64 = const {
        let items = [1, 2, 3, 4];
        items[2]
    };
    println!("{}", THIRD);
}
"#;
        let mut pipeline = Pipeline::new();
        let mut ast = pipeline
            .parse_source_with_path_for_tests(source, Path::new("unit_test_collections.fp"))
            .expect("frontend should handle const block arrays");
        let mut options = PipelineOptions::default();

        pipeline
            .stage_normalize_intrinsics(&mut ast, &options)
            .expect("normalization must succeed before const-eval");
        pipeline
            .stage_type_check_for_tests(&mut ast)
            .expect("type checking should succeed for const array usage");

        options.execute_main = true;

        let result = pipeline.stage_const_eval(&mut ast, &options);
        assert!(
            result.is_err(),
            "array indexing is not yet supported during const eval"
        );
    }
}
