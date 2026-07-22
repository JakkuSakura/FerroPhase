use std::path::{Path, PathBuf};

use fp_compiler::{
    AstId, CompilerDriver, CompilerWork, ConstValueId, ExecutionMode, FullyQualifiedPath, LirId,
    LirConsumer, MirId, RuntimeValueId, ScopeId,
};
use fp_core::{
    ast::{Node, Value},
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendResult, LanguageFrontend},
};
use fp_lang::FerroFrontend;
use fp_typing::{TypingDiagnostic, TypingDiagnosticLevel};
use fp_goasm::config::GoAsmTarget;

use crate::{CliError, Result};

pub fn check_path(path: &Path, syntax_only: bool) -> Result<()> {
    let ast = parse_file(path)?;
    if syntax_only {
        return Ok(());
    }

    let identity = CompilerIdentity::for_file(path);
    let mut driver = CompilerDriver::new();
    driver.state.insert_ast(identity.ast_id.clone(), ast);
    driver.scheduler.submit(CompilerWork::TypeAst {
        ast: identity.ast_id.clone(),
        scope: identity.scope_id(),
        path: identity.path.clone(),
        consumers: Vec::new(),
    });
    drain_driver(&mut driver)
}

pub fn eval_expr(source: &str) -> Result<Value> {
    let ast = parse_expr(source)?;
    execute_ast(ast, CompilerIdentity::for_expr(), ExecutionMode::Comptime)
}

pub fn eval_file(path: &Path) -> Result<Value> {
    let ast = parse_file(path)?;
    execute_ast(ast, CompilerIdentity::for_file(path), ExecutionMode::Runtime)
}

pub fn interpret_file(path: &Path) -> Result<Value> {
    let ast = parse_file(path)?;
    execute_ast(ast, CompilerIdentity::for_file(path), ExecutionMode::Runtime)
}

pub struct NativeCompileOptions {
    pub emitter: NativeEmitterKind,
    pub output: PathBuf,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub native_target: Option<String>,
    pub target_features: Option<String>,
    pub target_sysroot: Option<PathBuf>,
    pub linker: Option<String>,
    pub target_linker: Option<PathBuf>,
    pub release: bool,
    pub save_intermediates: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeEmitterKind {
    Native,
    GoAsm,
    Urcl,
}

pub struct BytecodeCompileOptions {
    pub output: PathBuf,
    pub emit_text: bool,
    pub save_intermediates: bool,
}

pub struct JvmCompileOptions {
    pub output: PathBuf,
    pub save_intermediates: bool,
    pub class_name_hint: Option<String>,
}

pub struct WasmCompileOptions {
    pub output: PathBuf,
}

pub struct EbpfCompileOptions {
    pub output: PathBuf,
}

pub fn compile_native_file(path: &Path, options: &NativeCompileOptions) -> Result<PathBuf> {
    let lowered = lower_file(path)?;
    let lir = lowered.lir()?;

    match options.emitter {
        NativeEmitterKind::Native => {
            let native_target =
                match options.native_target.as_deref() {
                    Some(value) => Some(fp_native::config::NativeTarget::resolve(
                        value,
                        options.target_triple.as_deref(),
                    )
                    .ok_or_else(|| {
                        CliError::Compilation(format!("Unsupported fp-native target: {}", value))
                    })?),
                    None => None,
                };

            let mut cfg = fp_native::config::NativeConfig::executable(&options.output)
                .with_target_triple(options.target_triple.clone())
                .with_target_cpu(options.target_cpu.clone())
                .with_native_target(native_target)
                .with_target_features(options.target_features.clone())
                .with_sysroot(options.target_sysroot.clone())
                .with_fuse_ld(options.target_linker.clone())
                .with_linker_driver(options.linker.clone())
                .with_release(options.release);
            if options.save_intermediates {
                cfg = cfg.with_asm_dump(Some(options.output.with_extension("asm")));
            }
            fp_native::NativeEmitter::new(cfg)
                .emit(lir, None)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
        NativeEmitterKind::GoAsm => {
            let target = Some(GoAsmTarget::resolve(options.target_triple.as_deref()));
            let cfg = fp_goasm::config::GoAsmConfig::new(&options.output)
                .with_target(target)
                .with_target_triple(options.target_triple.clone());
            fp_goasm::GoAsmEmitter::new(cfg)
                .emit(lir, None)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
        NativeEmitterKind::Urcl => fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(
            &options.output,
        ))
        .emit(lir, None)
        .map_err(|err| CliError::Compilation(err.to_string())),
    }
}

pub fn compile_bytecode_file(path: &Path, options: &BytecodeCompileOptions) -> Result<PathBuf> {
    let lowered = lower_file(path)?;
    let mir = lowered.mir()?;
    let bytecode = fp_bytecode::lower_program(&mir)
        .map_err(|err| CliError::Compilation(format!("MIR→Bytecode lowering failed: {}", err)))?;

    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let wants_text =
        options.emit_text || options.output.extension().and_then(|ext| ext.to_str()) == Some("ftbc");

    if options.save_intermediates || wants_text {
        let rendered = fp_bytecode::format_program(&bytecode);
        let text_path = if wants_text {
            options.output.clone()
        } else {
            options.output.with_extension("ftbc")
        };
        std::fs::write(&text_path, rendered).map_err(CliError::Io)?;
    }

    if !wants_text || options.save_intermediates {
        let bytes = fp_bytecode::encode_file(&bytecode)
            .map_err(|err| CliError::Compilation(format!("Bytecode encoding failed: {}", err)))?;
        let binary_path = if wants_text {
            options.output.with_extension("fbc")
        } else {
            options.output.clone()
        };
        std::fs::write(binary_path, bytes).map_err(CliError::Io)?;
    }

    Ok(options.output.clone())
}

pub fn compile_jvm_file(path: &Path, options: &JvmCompileOptions) -> Result<PathBuf> {
    let lowered = lower_file(path)?;
    let mir = lowered.mir()?;
    let class_stem = options
        .class_name_hint
        .as_deref()
        .unwrap_or("Main");
    let jvm_options = fp_jvm::JvmBackendOptions {
        class_name: fp_jvm::derive_class_name(class_stem),
        emit_java_entrypoint: true,
    };
    let program = fp_jvm::lower_program(&mir, &jvm_options)
        .map_err(|err| CliError::Compilation(format!("MIR→JVM lowering failed: {}", err)))?;
    let mut classes = fp_jvm::emit_class_files(&program)
        .map_err(|err| CliError::Compilation(format!("JVM class emission failed: {}", err)))?;

    if classes.len() != 1 {
        return Err(CliError::Compilation(
            "JVM backend currently expects exactly one emitted class".to_string(),
        ));
    }

    let class = classes.remove(0);
    let wants_jar = options.output.extension().and_then(|ext| ext.to_str()) == Some("jar");
    let output_path = if wants_jar {
        options.output.clone()
    } else {
        options.output.with_extension("class")
    };
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    let class_bytes = class.bytes.clone();
    let class_path = if wants_jar {
        output_path.with_extension("class")
    } else {
        output_path.clone()
    };

    if wants_jar {
        if options.save_intermediates {
            std::fs::write(&class_path, &class_bytes).map_err(CliError::Io)?;
        }

        let jar = fp_jvm::emit_executable_jar(&[class], &program.class.name)
            .map_err(|err| CliError::Compilation(format!("JAR packaging failed: {}", err)))?;
        std::fs::write(&output_path, jar).map_err(CliError::Io)?;
    } else {
        std::fs::write(&output_path, class_bytes).map_err(CliError::Io)?;
    }

    Ok(output_path)
}

pub fn compile_wasm_file(path: &Path, options: &WasmCompileOptions) -> Result<PathBuf> {
    let lowered = lower_file(path)?;
    let lir = lowered.lir()?;
    let wasm_bytes = fp_wasm::emit_wasm(&lir)
        .map_err(|err| CliError::Compilation(format!("Failed to emit wasm: {}", err)))?;
    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    std::fs::write(&options.output, wasm_bytes).map_err(CliError::Io)?;
    Ok(options.output.clone())
}

pub fn compile_ebpf_file(path: &Path, options: &EbpfCompileOptions) -> Result<PathBuf> {
    let lowered = lower_file(path)?;
    let lir = lowered.lir()?;
    if let Some(parent) = options.output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    if options.output.extension().and_then(|ext| ext.to_str()) == Some("o") {
        let object_bytes = fp_ebpf::emit_object(&lir)
            .map_err(|err| CliError::Compilation(format!("eBPF object emission failed: {}", err)))?;
        std::fs::write(&options.output, object_bytes).map_err(CliError::Io)?;
    } else {
        let text = fp_ebpf::emit_assembly(&lir)
            .map_err(|err| CliError::Compilation(format!("eBPF assembly emission failed: {}", err)))?;
        std::fs::write(&options.output, text).map_err(CliError::Io)?;
    }
    Ok(options.output.clone())
}

pub fn compile_cil_file(path: &Path) -> Result<String> {
    let ast = parse_file(path)?;
    compile_cil_ast(&ast)
}

pub fn compile_dotnet_file(path: &Path, output: &Path, save_intermediates: bool) -> Result<PathBuf> {
    let ast = parse_file(path)?;
    compile_dotnet_ast(&ast, output, save_intermediates)
}

fn compile_cil_ast(ast: &Node) -> Result<String> {
    #[cfg(feature = "lang-dotnet")]
    {
        fp_dotnet::emit_cil(ast)
            .map_err(|err| CliError::Compilation(format!("CIL emit failed: {}", err)))
    }
    #[cfg(not(feature = "lang-dotnet"))]
    {
        let _ = ast;
        Err(CliError::MissingDependency(
            "Feature 'lang-dotnet' is disabled; enable it to use CIL emission.".to_string(),
        ))
    }
}

fn compile_dotnet_ast(ast: &Node, output: &Path, save_intermediates: bool) -> Result<PathBuf> {
    #[cfg(feature = "lang-dotnet")]
    {
        fp_dotnet::emit_assembly(ast, output, save_intermediates)
            .map_err(|err| CliError::Compilation(format!(".NET assembly emit failed: {}", err)))
    }
    #[cfg(not(feature = "lang-dotnet"))]
    {
        let _ = (ast, output, save_intermediates);
        Err(CliError::MissingDependency(
            "Feature 'lang-dotnet' is disabled; enable it to use .NET assembly emission."
                .to_string(),
        ))
    }
}

fn execute_ast(ast: Node, identity: CompilerIdentity, mode: ExecutionMode) -> Result<Value> {
    let value_key = identity.path.to_key();
    let consumer = match mode {
        ExecutionMode::Comptime => LirConsumer::ExecuteComptime,
        ExecutionMode::Runtime => LirConsumer::ExecuteRuntime,
    };
    let mut driver = lower_ast(ast, &identity, vec![consumer])?;
    drain_driver(&mut driver)?;

    match mode {
        ExecutionMode::Comptime => driver
            .state
            .const_value(&ConstValueId::new(format!("const_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
        ExecutionMode::Runtime => driver
            .state
            .runtime_value(&RuntimeValueId::new(format!("runtime_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
    }
}

fn lower_file(path: &Path) -> Result<LoweredProgram> {
    let ast = parse_file(path)?;
    let identity = CompilerIdentity::for_file(path);
    let path_key = identity.path.to_key();
    let mut driver = lower_ast(ast, &identity, Vec::new())?;
    drain_driver(&mut driver)?;
    Ok(LoweredProgram { driver, path_key })
}

fn lower_ast(
    ast: Node,
    identity: &CompilerIdentity,
    consumers: Vec<LirConsumer>,
) -> Result<CompilerDriver> {
    let ast_id = identity.ast_id.clone();
    let scope_id = identity.scope_id();
    let path = identity.path.clone();
    let mut driver = CompilerDriver::new();
    driver.state.insert_ast(ast_id.clone(), ast);
    driver.scheduler.submit(CompilerWork::TypeAst {
        ast: ast_id,
        scope: scope_id,
        path,
        consumers,
    });
    Ok(driver)
}

fn drain_driver(driver: &mut CompilerDriver) -> Result<()> {
    while driver
        .run_next()
        .map_err(|err| CliError::Compilation(err.to_string()))?
        .is_some()
    {}
    emit_typing_diagnostics(driver.state.typing_diagnostics())
}

fn parse_expr(source: &str) -> Result<Node> {
    let frontend = FerroFrontend::new();
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_expr(source)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics())?;
    Ok(ast)
}

fn parse_file(path: &Path) -> Result<Node> {
    let frontend = FerroFrontend::new();
    let source = std::fs::read_to_string(path).map_err(CliError::Io)?;
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_file(&source, path)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics())?;
    Ok(ast)
}

fn emit_frontend_diagnostics(diagnostics: &[Diagnostic]) -> Result<()> {
    DiagnosticManager::emit(
        diagnostics,
        Some("frontend"),
        &DiagnosticDisplayOptions::default(),
    );
    if diagnostics
        .iter()
        .any(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
    {
        return Err(CliError::Compilation(
            "frontend stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn emit_typing_diagnostics(diagnostics: &[TypingDiagnostic]) -> Result<()> {
    let rendered: Vec<Diagnostic<String>> = diagnostics.iter().map(as_core_diagnostic).collect();
    DiagnosticManager::emit(
        &rendered,
        Some("typing"),
        &DiagnosticDisplayOptions::default(),
    );
    if diagnostics
        .iter()
        .any(|diagnostic| matches!(diagnostic.level, TypingDiagnosticLevel::Error))
    {
        return Err(CliError::Compilation(
            "typing stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
}

fn as_core_diagnostic(diagnostic: &TypingDiagnostic) -> Diagnostic<String> {
    let mut rendered = match diagnostic.level {
        TypingDiagnosticLevel::Error => Diagnostic::error(diagnostic.message.clone()),
        TypingDiagnosticLevel::Warning => Diagnostic::warning(diagnostic.message.clone()),
    }
    .with_source_context("typing".to_string());

    if let Some(span) = diagnostic.span {
        rendered = rendered.with_span(span);
    }

    rendered
}

struct CompilerIdentity {
    path: FullyQualifiedPath,
    ast_id: AstId,
}

struct LoweredProgram {
    driver: CompilerDriver,
    path_key: String,
}

impl LoweredProgram {
    fn mir(&self) -> Result<fp_core::mir::Program> {
        self.driver
            .state
            .mir(&MirId::new(format!("mir:{}", self.path_key)))
            .map(|program| program.clone())
            .map_err(|err| CliError::Compilation(err.to_string()))
    }

    fn lir(&self) -> Result<fp_core::lir::LirProgram> {
        self.driver
            .state
            .lir(&LirId::new(format!("lir:{}", self.path_key)))
            .map(|program| program.clone())
            .map_err(|err| CliError::Compilation(err.to_string()))
    }
}

impl CompilerIdentity {
    fn for_expr() -> Self {
        Self::new(vec!["cli".to_string(), "eval_expr".to_string()])
    }

    fn for_file(path: &Path) -> Self {
        let canonical = path.canonicalize().unwrap_or_else(|_| PathBuf::from(path));
        Self::new(vec!["cli".to_string(), canonical.display().to_string()])
    }

    fn new(segments: Vec<String>) -> Self {
        let path = FullyQualifiedPath::from_segments(segments);
        let ast_id = AstId::new(format!("ast:{}", path.to_key()));
        Self { path, ast_id }
    }

    fn scope_id(&self) -> ScopeId {
        ScopeId::new(self.path.to_key())
    }
}
