use crate::CliError;
use crate::codegen::CodeGenerator;
use crate::languages::frontend::{
    FerroFrontend, FlatbuffersFrontend, FrontendResult, FrontendSnapshot, HclFrontend,
    JsonFrontend, JsonSchemaFrontend, LanguageFrontend, PrqlFrontend, PythonFrontend, GoFrontend,
    SqlFrontend, TomlFrontend, TypeScriptFrontend, WitFrontend,
};
use crate::languages::{self, detect_source_language};
use fp_backend::transformations::{HirGenerator, LirGenerator, MirLowering};
use fp_bytecode;
use fp_core::ast::register_threadlocal_serializer;
use fp_core::ast::{
    AstSerializer, File, Ident, Item, ItemChunk, ItemKind, MacroExpansionParser, Module, Node,
    NodeKind, RuntimeValue, Value, Visibility,
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
use fp_pipeline::{PipelineBuilder, PipelineDiagnostics, PipelineError, PipelineStage};
use fp_typescript::frontend::TsParseMode;
use fp_typing::TypingDiagnosticLevel;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info, info_span};

// Begin internal submodules extracted for clarity
mod artifacts;
mod diagnostics;
mod options;
mod stages;
use self::diagnostics as diag;
use artifacts::LlvmArtifacts;
pub use options::{
    BackendKind, DebugOptions, LossyOptions, PipelineOptions, RuntimeConfig,
};

const STAGE_FRONTEND: &str = "frontend";
const STAGE_CONST_EVAL: &str = "const-eval";
const STAGE_TYPE_ENRICH: &str = "ast→typed";
const STAGE_AST_TO_HIR: &str = "ast→hir";
const STAGE_HIR_TO_MIR: &str = "hir→mir";
const STAGE_MIR_TO_LIR: &str = "mir→lir";
const STAGE_RUNTIME_MATERIALIZE: &str = "materialize-runtime";
const STAGE_TYPE_POST_MATERIALIZE: &str = "ast→typed(post-materialize)";
const STAGE_LINK_BINARY: &str = "link-binary";
const STAGE_EMIT_WASM: &str = "emit-wasm";
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
    macro_parser: Option<Arc<dyn MacroExpansionParser>>,
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
        let json_frontend: Arc<dyn LanguageFrontend> = Arc::new(JsonFrontend::new());
        register(json_frontend);
        let flatbuffers_frontend: Arc<dyn LanguageFrontend> = Arc::new(FlatbuffersFrontend::new());
        register(flatbuffers_frontend);
        let toml_frontend: Arc<dyn LanguageFrontend> = Arc::new(TomlFrontend::new());
        register(toml_frontend);
        let hcl_frontend: Arc<dyn LanguageFrontend> = Arc::new(HclFrontend::new());
        register(hcl_frontend);
        let python_frontend: Arc<dyn LanguageFrontend> = Arc::new(PythonFrontend::new());
        register(python_frontend);
        let go_frontend: Arc<dyn LanguageFrontend> = Arc::new(GoFrontend::new());
        register(go_frontend);
        Self {
            frontends,
            default_runtime: "literal".to_string(),
            serializer: None,
            intrinsic_normalizer: None,
            macro_parser: None,
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
            macro_parser,
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
        self.serializer = Some(serializer);
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.macro_parser = macro_parser;
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
        let frontend = match self.frontends.get(&language).cloned() {
            Some(frontend) => frontend,
            None => {
                return Err(Self::emit_stage_error(
                    STAGE_FRONTEND,
                    options,
                    format!("Unsupported source language: {}", language),
                ));
            }
        };

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
            BackendKind::Interpret => {
                self.execute_interpret_target(ast, &options, input_path.as_deref())
                    .await
            }
            ref target => {
                let base_path = options.base_path.as_ref().ok_or_else(|| {
                    let msg = match target {
                        BackendKind::Rust => "Missing base path for transpilation",
                        BackendKind::Llvm => "Missing base path for LLVM generation",
                        BackendKind::Binary => "Missing base path for binary generation",
                        BackendKind::Bytecode | BackendKind::TextBytecode => {
                            "Missing base path for bytecode generation"
                        }
                        BackendKind::Wasm => "Missing base path for wasm generation",
                        BackendKind::Interpret => unreachable!(),
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
        self.macro_parser = None;
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
            macro_parser,
            snapshot,
            diagnostics,
        } = match frontend.parse(source, input_path) {
            Ok(result) => result,
            Err(err) => {
                if let fp_core::error::Error::Diagnostic(diag) = &err {
                    let diag = diag
                        .clone()
                        .with_source_context(STAGE_FRONTEND.to_string());
                    diag::emit(&[diag], Some(STAGE_FRONTEND), options);
                    return Err(Self::stage_failure(STAGE_FRONTEND));
                }
                return Err(Self::emit_stage_error(
                    STAGE_FRONTEND,
                    options,
                    format!("frontend parse failed: {}", err),
                ));
            }
        };

        let collected_diagnostics = diagnostics.get_diagnostics();
        diag::emit(&collected_diagnostics, Some(STAGE_FRONTEND), options);

        let has_errors = collected_diagnostics
            .iter()
            .any(|diag| diag.level == DiagnosticLevel::Error);

        if has_errors {
            return Err(Self::stage_failure(STAGE_FRONTEND));
        }

        register_threadlocal_serializer(serializer.clone());
        self.serializer = Some(serializer.clone());
        self.intrinsic_normalizer = intrinsic_normalizer;
        self.macro_parser = macro_parser;
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

        let mut loader = FileModuleLoader::new(self, options, frontend);
        let merged_items = loader.resolve_items(&file.items, &root_dir)?;
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
        target: &BackendKind,
        mut ast: Node,
        options: &PipelineOptions,
        base_path: &Path,
        input_path: Option<&Path>,
    ) -> Result<PipelineOutput, CliError> {
        let started_at = std::time::Instant::now();
        info!("pipeline: starting compilation target {:?}", target);
        if stage_enabled(options, STAGE_INTRINSIC_NORMALIZE) {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_INTRINSIC_NORMALIZE);
            self.stage_normalize_intrinsics(&mut ast, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_INTRINSIC_NORMALIZE,
                stage_started.elapsed()
            );
        }

        let did_const_eval = stage_enabled(options, STAGE_CONST_EVAL);
        let outcome = if did_const_eval {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_CONST_EVAL);
            let outcome = self.stage_const_eval(&mut ast, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_CONST_EVAL,
                stage_started.elapsed()
            );
            outcome
        } else {
            ConstEvalOutcome::default()
        };
        self.last_const_eval = Some(outcome.clone());

        if matches!(
            target,
            BackendKind::Llvm | BackendKind::Binary | BackendKind::Wasm
        ) && !did_const_eval
        {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_RUNTIME_MATERIALIZE);
            self.inject_runtime_std(&mut ast, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_RUNTIME_MATERIALIZE,
                stage_started.elapsed()
            );
        }

        if stage_enabled(options, STAGE_TYPE_ENRICH) {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_TYPE_ENRICH);
            self.stage_type_check(&mut ast, STAGE_TYPE_ENRICH, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_TYPE_ENRICH,
                stage_started.elapsed()
            );
        }

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST, options)?;
            self.save_pretty(&ast, base_path, EXT_AST_TYPED, options)?;
        }

        if options.save_intermediates {
            self.save_pretty(&ast, base_path, EXT_AST_EVAL, options)?;
        }

        if stage_enabled(options, STAGE_RUNTIME_MATERIALIZE) {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_RUNTIME_MATERIALIZE);
            self.stage_materialize_runtime_intrinsics(&mut ast, target, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_RUNTIME_MATERIALIZE,
                stage_started.elapsed()
            );
        }

        if stage_enabled(options, STAGE_TYPE_POST_MATERIALIZE) {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_TYPE_POST_MATERIALIZE);
            self.stage_type_check(&mut ast, STAGE_TYPE_POST_MATERIALIZE, options)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_TYPE_POST_MATERIALIZE,
                stage_started.elapsed()
            );
        }

        let output = if matches!(target, BackendKind::Rust) {
            let span = info_span!("pipeline.codegen", target = "rust");
            let _enter = span.enter();
            let stage_started = std::time::Instant::now();
            info!("pipeline: start codegen-rust");
            let code = CodeGenerator::generate_rust_code(&ast)?;
            info!(
                "pipeline: finished codegen-rust in {:.2?}",
                stage_started.elapsed()
            );
            PipelineOutput::Code(code)
        } else {
            let stage_started = std::time::Instant::now();
            info!("pipeline: start {}", STAGE_AST_TO_HIR);
            let hir_program = self.stage_hir_generation(&ast, options, input_path, base_path)?;
            info!(
                "pipeline: finished {} in {:.2?}",
                STAGE_AST_TO_HIR,
                stage_started.elapsed()
            );

            match target {
                BackendKind::Llvm => {
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_HIR_TO_MIR);
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_HIR_TO_MIR,
                        stage_started.elapsed()
                    );
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_MIR_TO_LIR);
                    let lir = self.stage_mir_to_lir(&mir.mir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_MIR_TO_LIR,
                        stage_started.elapsed()
                    );

                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start emit-llvm");
                    let llvm = self.generate_llvm_artifacts(
                        &lir.lir_program,
                        base_path,
                        input_path,
                        false,
                        options,
                    )?;
                    info!(
                        "pipeline: finished emit-llvm in {:.2?}",
                        stage_started.elapsed()
                    );
                    PipelineOutput::Code(llvm.ir_text)
                }
                BackendKind::Binary => {
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_HIR_TO_MIR);
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_HIR_TO_MIR,
                        stage_started.elapsed()
                    );
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_MIR_TO_LIR);
                    let lir = self.stage_mir_to_lir(&mir.mir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_MIR_TO_LIR,
                        stage_started.elapsed()
                    );

                    let backend = options
                        .backend
                        .as_deref()
                        .unwrap_or("native")
                        .to_lowercase();

                    if backend == "native" || backend == "fp-native" {
                        let stage_started = std::time::Instant::now();
                        info!("pipeline: start {}", STAGE_LINK_BINARY);
                        let binary_path =
                            self.stage_link_binary_native(&lir.lir_program, base_path, options)?;
                        info!(
                            "pipeline: finished {} in {:.2?}",
                            STAGE_LINK_BINARY,
                            stage_started.elapsed()
                        );
                        PipelineOutput::Binary(binary_path)
                    } else if backend == "cranelift" || backend == "fp-cranelift" {
                        let stage_started = std::time::Instant::now();
                        info!("pipeline: start {}", STAGE_LINK_BINARY);
                        let binary_path = self.stage_link_binary_cranelift(
                            &lir.lir_program,
                            base_path,
                            options,
                        )?;
                        info!(
                            "pipeline: finished {} in {:.2?}",
                            STAGE_LINK_BINARY,
                            stage_started.elapsed()
                        );
                        PipelineOutput::Binary(binary_path)
                    } else {
                        let stage_started = std::time::Instant::now();
                        info!("pipeline: start emit-llvm");
                        let llvm = self.generate_llvm_artifacts(
                            &lir.lir_program,
                            base_path,
                            input_path,
                            true,
                            options,
                        )?;
                        info!(
                            "pipeline: finished emit-llvm in {:.2?}",
                            stage_started.elapsed()
                        );
                        let stage_started = std::time::Instant::now();
                        info!("pipeline: start {}", STAGE_LINK_BINARY);
                        let binary_path =
                            self.stage_link_binary(&llvm.ir_path, base_path, options)?;
                        info!(
                            "pipeline: finished {} in {:.2?}",
                            STAGE_LINK_BINARY,
                            stage_started.elapsed()
                        );
                        PipelineOutput::Binary(binary_path)
                    }
                }
                BackendKind::Bytecode | BackendKind::TextBytecode => {
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_HIR_TO_MIR);
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_HIR_TO_MIR,
                        stage_started.elapsed()
                    );
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start bytecode-lower");
                    let bytecode = fp_bytecode::lower_program(&mir.mir_program).map_err(|err| {
                        CliError::Compilation(format!("MIR→Bytecode lowering failed: {}", err))
                    })?;
                    info!(
                        "pipeline: finished bytecode-lower in {:.2?}",
                        stage_started.elapsed()
                    );
                    let output_path = base_path.to_path_buf();
                    if let Some(parent) = output_path.parent() {
                        fs::create_dir_all(parent)?;
                    }
                    let wants_text =
                        output_path.extension().and_then(|ext| ext.to_str()) == Some("ftbc");

                    if options.save_intermediates || wants_text {
                        let rendered = fp_bytecode::format_program(&bytecode);
                        let text_path = output_path.with_extension("ftbc");
                        fs::write(&text_path, rendered)?;
                    }

                    if !wants_text || options.save_intermediates {
                        let bytes = fp_bytecode::encode_file(&bytecode).map_err(|err| {
                            CliError::Compilation(format!("Bytecode encoding failed: {}", err))
                        })?;
                        let binary_path = if wants_text {
                            output_path.with_extension("fbc")
                        } else {
                            output_path.clone()
                        };
                        fs::write(&binary_path, bytes)?;
                    }

                    PipelineOutput::Binary(output_path)
                }
                BackendKind::Wasm => {
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_HIR_TO_MIR);
                    let mir = self.stage_hir_to_mir(&hir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_HIR_TO_MIR,
                        stage_started.elapsed()
                    );
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_MIR_TO_LIR);
                    let lir = self.stage_mir_to_lir(&mir.mir_program, options, base_path)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_MIR_TO_LIR,
                        stage_started.elapsed()
                    );
                    let stage_started = std::time::Instant::now();
                    info!("pipeline: start {}", STAGE_EMIT_WASM);
                    let wasm_path = self.stage_emit_wasm(&lir.lir_program, base_path, options)?;
                    info!(
                        "pipeline: finished {} in {:.2?}",
                        STAGE_EMIT_WASM,
                        stage_started.elapsed()
                    );
                    PipelineOutput::Binary(wasm_path)
                }
                BackendKind::Rust | BackendKind::Interpret => unreachable!(),
            }
        };

        info!(
            "pipeline: finished compilation target {:?} in {:.2?}",
            target,
            started_at.elapsed()
        );
        Ok(output)
    }

    fn inject_runtime_std(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        let mut diagnostics = PipelineDiagnostics::default();
        diagnostics.set_display_options(diag::display_options(options));
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
                    diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE);
                    return Err(Self::stage_failure(STAGE_RUNTIME_MATERIALIZE));
                }
            };
            let std_node = self.parse_input_source(options, &source, Some(&std_path))?;
            let merged = merge_std_module(
                ast.clone(),
                std_node,
                &mut diagnostics,
                STAGE_RUNTIME_MATERIALIZE,
            )
            .map_err(|_| {
                diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE);
                Self::stage_failure(STAGE_RUNTIME_MATERIALIZE)
            })?;
            *ast = merged;
        }
        diagnostics.emit_stage(STAGE_RUNTIME_MATERIALIZE);
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
        let stdout_mode = if options.target == BackendKind::Interpret {
            fp_interpret::engine::StdoutMode::Inherit
        } else {
            fp_interpret::engine::StdoutMode::Capture
        };
        let interpreter_opts = InterpreterOptions {
            mode,
            debug_assertions: !options.release,
            diagnostics: None,
            diagnostic_context: STAGE_AST_INTERPRET,
            module_resolution: None,
            macro_parser: self.macro_parser.clone(),
            intrinsic_normalizer: self.intrinsic_normalizer.clone(),
            stdout_mode,
        };
        let mut interpreter = AstInterpreter::new(&ctx, interpreter_opts);
        interpreter.enable_incremental_typing(&working_ast);

        let value = match working_ast.kind_mut() {
            NodeKind::File(_) => {
                interpreter.interpret(&mut working_ast);
                interpreter.execute_main().unwrap_or_else(Value::unit)
            }
            NodeKind::Expr(expr) => interpreter.evaluate_expression(expr),
            NodeKind::Item(_) => {
                return Err(Self::emit_stage_error(
                    STAGE_AST_INTERPRET,
                    options,
                    "Standalone item interpretation is not supported",
                ));
            }
            NodeKind::Query(_) => {
                return Err(Self::emit_stage_error(
                    STAGE_AST_INTERPRET,
                    options,
                    "Query documents cannot be interpreted",
                ));
            }
            NodeKind::Schema(_) => {
                return Err(Self::emit_stage_error(
                    STAGE_AST_INTERPRET,
                    options,
                    "Schema documents cannot be interpreted",
                ));
            }
            NodeKind::Workspace(_) => {
                return Err(Self::emit_stage_error(
                    STAGE_AST_INTERPRET,
                    options,
                    "Workspace documents cannot be interpreted",
                ));
            }
        };

        let outcome = interpreter.take_outcome();
        diag::emit(&outcome.diagnostics, Some(STAGE_AST_INTERPRET), options);

        if outcome.has_errors {
            return Err(Self::emit_stage_error(
                STAGE_AST_INTERPRET,
                options,
                "AST interpretation failed; see diagnostics for details",
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
        diagnostics.set_display_options(diag::display_options(options));
        let pipeline = PipelineBuilder::<Src, Src>::new().add_stage(stage).build();
        match pipeline.run(context, &mut diagnostics) {
            Ok(output) => Ok(output),
            Err(_) => {
                diagnostics.emit_stage(stage_label);
                Err(Self::stage_failure(stage_label))
            }
        }
    }

    fn stage_failure(stage: &str) -> CliError {
        CliError::Compilation(format!(
            "{} stage failed; see diagnostics for details",
            stage
        ))
    }

    fn emit_stage_error(
        stage: &'static str,
        options: &PipelineOptions,
        message: impl Into<String>,
    ) -> CliError {
        let diagnostic = Diagnostic::error(message.into()).with_source_context(stage);
        diag::emit(&[diagnostic], Some(stage), options);
        Self::stage_failure(stage)
    }

    // moved to pipeline::diagnostics
}

fn runtime_std_paths() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    vec![root.join("../../src/std/mod.fp")]
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
    let mut std_module = match std_module {
        Some(mut module) => {
            module.items.extend(std_items);
            module
        }
        None => Module {
            attrs: Vec::new(),
            name: Ident::new("std"),
            items: std_items,
            visibility: Visibility::Public,
            is_external: false,
        },
    };

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

struct FileModuleLoader<'a> {
    pipeline: &'a mut Pipeline,
    options: &'a PipelineOptions,
    frontend: &'a Arc<dyn LanguageFrontend>,
    loaded_files: HashMap<PathBuf, ItemChunk>,
}

impl<'a> FileModuleLoader<'a> {
    fn new(
        pipeline: &'a mut Pipeline,
        options: &'a PipelineOptions,
        frontend: &'a Arc<dyn LanguageFrontend>,
    ) -> Self {
        Self {
            pipeline,
            options,
            frontend,
            loaded_files: HashMap::new(),
        }
    }

    fn resolve_items(&mut self, items: &ItemChunk, base_dir: &Path) -> Result<ItemChunk, CliError> {
        let mut resolved = Vec::with_capacity(items.len());

        for item in items {
            match item.kind() {
                ItemKind::Module(module) => {
                    let child_dir = base_dir.join(module.name.as_str());
                    let resolved_module = if module.is_external {
                        let module_items = self.load_module_items(base_dir, &module.name)?;
                        let nested_items = self.resolve_items(&module_items, &child_dir)?;
                        Module {
                            attrs: module.attrs.clone(),
                            name: module.name.clone(),
                            items: nested_items,
                            visibility: module.visibility.clone(),
                            is_external: false,
                        }
                    } else {
                        let nested_items = self.resolve_items(&module.items, &child_dir)?;
                        Module {
                            attrs: module.attrs.clone(),
                            name: module.name.clone(),
                            items: nested_items,
                            visibility: module.visibility.clone(),
                            is_external: false,
                        }
                    };
                    resolved.push(Item {
                        ty: item.ty.clone(),
                        span: item.span,
                        kind: ItemKind::Module(resolved_module),
                    });
                }
                _ => resolved.push(item.clone()),
            }
        }

        Ok(resolved)
    }

    fn load_module_items(
        &mut self,
        base_dir: &Path,
        module_name: &Ident,
    ) -> Result<ItemChunk, CliError> {
        let module_path = resolve_module_file(base_dir, module_name.as_str())?;
        let canonical = module_path
            .canonicalize()
            .unwrap_or_else(|_| module_path.clone());
        if let Some(items) = self.loaded_files.get(&canonical) {
            return Ok(items.clone());
        }

        let ast = self
            .pipeline
            .parse_module_file(self.options, self.frontend, &canonical)?;
        let NodeKind::File(file) = ast.kind() else {
            return Err(CliError::Compilation(format!(
                "module file {} did not parse as a file",
                canonical.display()
            )));
        };

        self.loaded_files
            .insert(canonical.clone(), file.items.clone());
        Ok(file.items.clone())
    }
}

fn resolve_module_file(base_dir: &Path, module_name: &str) -> Result<PathBuf, CliError> {
    let flat_path = base_dir.join(format!("{module_name}.fp"));
    let mod_path = base_dir.join(module_name).join("mod.fp");
    let flat_exists = flat_path.is_file();
    let mod_exists = mod_path.is_file();

    match (flat_exists, mod_exists) {
        (true, true) => Err(CliError::Compilation(format!(
            "module `{}` is ambiguous; found both {} and {}",
            module_name,
            flat_path.display(),
            mod_path.display()
        ))),
        (true, false) => Ok(flat_path),
        (false, true) => Ok(mod_path),
        (false, false) => Err(CliError::Compilation(format!(
            "module `{}` not found; expected {} or {}",
            module_name,
            flat_path.display(),
            mod_path.display()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::BackendKind;
    use super::*;
    use crate::pipeline::artifacts::LirArtifacts;
    use fp_core::intrinsics::IntrinsicCallKind;
    use fp_core::{ast, hir, lir};
    use std::collections::HashSet;
    use std::fs;
    use std::path::{Path, PathBuf};
    use tempfile::TempDir;

    struct PipelineHarness {
        pipeline: Pipeline,
        options: PipelineOptions,
    }

    impl PipelineHarness {
        fn new(target: BackendKind) -> Self {
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

        fn materialize_runtime(&mut self, ast: &mut Node, target: BackendKind) {
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

    #[test]
    fn lang_item_intrinsics_normalize_calls() {
        let source = r#"
mod std {
    mod time {
        #[lang = "time_now"]
        fn now() -> f64 {
            std::time::now()
        }
    }
}

fn main() {
    std::time::now();
}
"#;

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        let intrinsic_calls = find_intrinsic_calls(&ast);
        assert!(
            intrinsic_calls
                .iter()
                .any(|call| call.kind == IntrinsicCallKind::TimeNow),
            "expected time_now intrinsic from lang item"
        );
    }

    #[test]
    fn external_mod_declarations_load_files() {
        let temp_dir = TempDir::new().expect("tempdir");
        let root = temp_dir.path();
        let main_path = root.join("main.fp");
        let module_path = root.join("foo.fp");

        fs::write(&main_path, "mod foo;\n\nfn main() { foo::bar(); }\n").expect("write main");
        fs::write(&module_path, "fn bar() {}\n").expect("write module");

        let mut pipeline = Pipeline::new();
        let options = PipelineOptions::default();
        let source = fs::read_to_string(&main_path).expect("read main");
        let ast = pipeline
            .parse_input_source(&options, &source, Some(&main_path))
            .expect("parse main");
        let ast::NodeKind::File(file) = ast.kind() else {
            panic!("expected file AST");
        };

        let module = file.items.iter().find_map(|item| match item.kind() {
            ast::ItemKind::Module(module) if module.name.as_str() == "foo" => Some(module),
            _ => None,
        });

        let Some(module) = module else {
            panic!("expected module foo");
        };
        assert!(!module.items.is_empty(), "expected foo to be populated");
        assert!(
            module
                .items
                .iter()
                .any(|item| matches!(item.kind(), ast::ItemKind::DefFunction(_)))
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
                        let _ = format;
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let outcome = pipeline
            .stage_const_eval(&mut ast, &options)
            .expect("method calls should now be supported in const eval");
        assert_stdout_contains(&outcome, "52");
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, BackendKind::Llvm);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, BackendKind::Llvm);
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

        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let mut harness = PipelineHarness::new(BackendKind::Llvm);
        let mut ast = harness.parse(source);
        harness.normalize(&mut ast);
        harness.type_check(&mut ast);
        harness.materialize_runtime(&mut ast, BackendKind::Llvm);
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
        let mut harness = PipelineHarness::new(BackendKind::Interpret);
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

        let outcome = pipeline
            .stage_const_eval(&mut ast, &options)
            .expect("array indexing should now be supported during const eval");
        assert_stdout_contains(&outcome, "3");
    }
}
