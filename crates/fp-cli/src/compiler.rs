use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_compiler::{
    CompilerDriver, CompilerExecutor, CompilerSession, ConstValueId, FullyQualifiedPath, LirId,
    PipelineMode,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::package::provider::PackageProvider;
use fp_core::package::PackageId;
use fp_core::{
    ast::{
        Expr, ExprBlock, File, Ident, Item, ItemDefConst, ItemDefFunction, ItemKind, ScriptBlock,
        Value, Visibility,
    },
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendParseMode, FrontendResult, LanguageFrontend},
    lir::LirDataLayout,
};
use fp_lang::FerroFrontend;
use fp_typing::{TypingDiagnostic, TypingDiagnosticLevel};

use crate::languages::in_memory::in_memory_provider;
use crate::languages::package_provider_registry::provider_for_language;
use crate::languages::{self, detect_source_language};
use crate::{CliError, Result};

pub(crate) fn data_layout() -> LirDataLayout {
    LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid CLI data layout")
}

pub fn eval_script(script: ScriptBlock) -> Result<Value> {
    let body = ExprBlock::new_stmts(script.stmts);
    let eval_const = ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: Visibility::Private,
        name: Ident::new("__eval_result"),
        ty: None,
        value: Expr::block(body.clone()).into(),
    };
    let main = ItemDefFunction::new_simple(Ident::new("main"), ExprBlock::new_expr(Expr::unit()));
    let ast = File {
        path: PathBuf::from("<eval>"),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![
            Item::new(ItemKind::DefFunction(main)),
            Item::new(ItemKind::DefConst(eval_const)),
        ],
    };
    let identity = CompilerIdentity::for_script();
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        SourceInput::InMemory(ast),
        languages::FERROPHASE,
        &identity,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver)?;
    if let Some((_, value)) = driver
        .state
        .borrow()
        .typing_ctx
        .resolved_consts
        .borrow()
        .iter()
        .find(|(key, _)| key.contains("__eval_result"))
    {
        return Ok(value.clone());
    }
    driver
        .state
        .borrow()
        .const_value(&ConstValueId::new(format!(
            "const_value:{}",
            identity.path.to_key()
        )))
        .map(|value| value.clone())
        .map_err(|error| CliError::Compilation(error.to_string()))
}

pub fn interpret_file(path: &Path, package: &str) -> Result<Value> {
    let language = resolve_source_language(path, None)?;
    execute_ast(
        SourceInput::Path(path.to_path_buf()),
        &language,
        CompilerIdentity::for_file(package, path),
        fp_core::context::ExecutionMode::Runtime,
    )
}

fn execute_ast(
    input: SourceInput,
    language: &str,
    identity: CompilerIdentity,
    mode: fp_core::context::ExecutionMode,
) -> Result<Value> {
    let value_key = identity.path.to_key();
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        input,
        language,
        &identity,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver)?;

    match mode {
        fp_core::context::ExecutionMode::CompileTime => driver
            .state
            .borrow()
            .const_value(&ConstValueId::new(format!("const_value:{value_key}")))
            .map(|value| value.clone())
            .map_err(|err| CliError::Compilation(err.to_string())),
        fp_core::context::ExecutionMode::Runtime => {
            let package_id = PackageId::new(identity.path.path().head().ok_or_else(|| {
                CliError::Compilation("source file has no package identity".to_string())
            })?);
            let lir_id = LirId::new(format!("lir:{}:{}", package_id.as_str(), value_key));
            driver
                .execute_runtime(&lir_id)
                .map_err(|err| CliError::Compilation(err.to_string()))
        }
    }
}

/// `"std"`/`"libc"` resolve against different providers depending on the
/// active source language: `fp_lang`'s hand-written `.fp` reimplementation
/// for `.fp`-dialect projects, or real rustc source (`fp-rust`'s
/// `RustStdProvider`, see `docs/RustStd.md`) for real `.rs`/Cargo projects.
/// Panics on an unrecognized language rather than silently defaulting —
/// wiring up std resolution for a new source language is a deliberate step,
/// not something to fall through to FerroPhase's `.fp` std by accident.
fn std_provider_for(language: &str) -> Arc<dyn fp_core::package::provider::PackageProvider> {
    match language {
        l if l == languages::FERROPHASE => Arc::new(fp_lang::provider::FerroPhaseProvider),
        l if l == languages::RUST => Arc::new(fp_rust::RustStdProvider),
        // A native object/archive/asm-text/goasm/URCL/JVM-bytecode/CIL
        // package has no std/libc dependency at all.
        l if l == languages::NATIVE_OBJECT
            || l == languages::NATIVE_ARCHIVE
            || l == languages::NATIVE_ASM
            || l == languages::GOASM
            || l == languages::URCL
            || l == languages::JVM_BYTECODE
            || l == languages::CIL =>
        {
            Arc::new(fp_core::package::provider::EmptyProvider)
        }
        other => panic!("std_provider_for: no std/libc provider wired up for language {other:?}"),
    }
}

/// Package/provider discovery shared by every single-file compiler entry
/// point — `resolve_input_package` below, and `commands::compile`'s
/// `provider_and_package_for_input`. A single file is a package with one
/// member — this only kicks in when `input` actually lives inside a
/// discoverable multi-file package (a `Cargo.toml`/`Magnet.toml` manifest
/// somewhere above it *and* a declared package under that manifest whose
/// root actually contains `input`), so sibling modules/imports resolve
/// correctly instead of `input` being (incorrectly) treated as an isolated
/// standalone file. Returns `None` both when no manifest is found at all,
/// and when a manifest is found but doesn't actually cover `input` (e.g. a
/// standalone script sitting under an unrelated workspace root) — either
/// way, callers fall back to wrapping `input` as its own single-member
/// package.
pub fn find_manifest_package(
    input: &Path,
    language: &str,
) -> Result<Option<(Arc<dyn PackageProvider>, PackageId, PathBuf)>> {
    let input_abs = input.canonicalize().unwrap_or_else(|_| input.to_path_buf());
    let Some(root) = fp_lang::project::find_manifest(&input_abs) else {
        return Ok(None);
    };
    let provider = crate::languages::package_provider_registry::provider_for_language(language, &root)
        .ok_or_else(|| {
            CliError::Compilation(format!("no package provider for source language: {language}"))
        })?;
    let packages = provider
        .list_packages()
        .map_err(|e| CliError::Compilation(e.to_string()))?;
    let mut found = None;
    for package_id in &packages {
        let metadata = provider
            .load_package_metadata(package_id)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        let package_root = metadata.root.to_path_buf();
        let package_root_abs = package_root
            .canonicalize()
            .unwrap_or_else(|_| package_root.clone());
        if input_abs.starts_with(&package_root_abs) {
            found = Some((package_id.clone(), package_root_abs));
            break;
        }
    }
    // A manifest existing somewhere above `input` doesn't mean it's *for*
    // `input` — e.g. a standalone `.fp` script can sit anywhere under a
    // large Cargo workspace root without being part of any of that
    // workspace's own crates (this repo's own `examples/*.fp` next to its
    // Rust-workspace `Cargo.toml` is exactly this case). Match the same
    // "no manifest at all" fallback above instead of treating "found a
    // manifest, but no declared package covers this file" as an error —
    // both mean the same thing to the caller: wrap `input` as its own
    // single-member package.
    let Some((package_id, package_root_abs)) = found else {
        return Ok(None);
    };
    Ok(Some((provider, package_id, package_root_abs)))
}

/// Computes the `PackageItem::module_path` tag a package's own provider would tag
/// `input` with, given its package root — the one implementation shared by
/// `resolve_input_package`'s single-file resolution and
/// `commands::compile::provider_and_package_for_input`'s `--target` path,
/// instead of two independent per-language guesses. No fallback: an
/// unsupported language is a real error, not a silent drop to some default
/// estimator.
pub(crate) fn module_path_for_language(
    language: &str,
    package_root: &Path,
    input: &Path,
) -> Result<QualifiedPath> {
    match language {
        "rust" | "rs" => {
            let rel = input.strip_prefix(package_root.join("src")).map_err(|_| {
                CliError::Compilation(format!(
                    "{} is not inside {}'s src/ directory",
                    input.display(),
                    package_root.display()
                ))
            })?;
            Ok(fp_rust::provider::rs_relative_to_module_path(
                &rel.display().to_string(),
            ))
        }
        "ferrophase" | "fp" => {
            let rel = input.strip_prefix(package_root.join("src")).map_err(|_| {
                CliError::Compilation(format!(
                    "{} is not inside {}'s src/ directory",
                    input.display(),
                    package_root.display()
                ))
            })?;
            Ok(fp_lang::magnet_provider::module_path_from_relative(
                &rel.display().to_string(),
            ))
        }
        "typescript" | "ts" | "javascript" | "js" => {
            module_path_for_typescript(package_root, input)
        }
        other => Err(CliError::Compilation(format!(
            "no module-path estimator for source language: {other}"
        ))),
    }
}

#[cfg(feature = "lang-typescript")]
fn module_path_for_typescript(package_root: &Path, input: &Path) -> Result<QualifiedPath> {
    Ok(QualifiedPath::new(fp_typescript::package::estimate_module_path(
        package_root,
        input,
    )))
}

#[cfg(not(feature = "lang-typescript"))]
fn module_path_for_typescript(_package_root: &Path, _input: &Path) -> Result<QualifiedPath> {
    Err(CliError::Compilation(
        "typescript support not compiled into this build".to_string(),
    ))
}

/// A single compiler input: either a real on-disk file (the common case —
/// parsed lazily, once a `PackageProvider` actually asks for its source), or
/// an already-built in-memory `File` with no path to read from (e.g.
/// `eval_script`'s synthetic `"<eval>"` script). Two genuinely different
/// kinds of input, not one file-focused path with a bolted-on exception.
enum SourceInput {
    Path(PathBuf),
    InMemory(File),
}

/// Resolves any compiler input to `(provider, package_id, module_path)` —
/// the real enclosing package if `input` lives inside a discoverable
/// multi-file package (a `Cargo.toml`/`Magnet.toml` manifest above it), else
/// a synthetic one-member package. Either way, everything downstream goes
/// through the same `PackageProvider`-shaped pipeline; parsing only ever
/// happens lazily, inside whichever provider is returned, never eagerly here.
fn resolve_input_package(
    input: SourceInput,
    language: &str,
    identity: &CompilerIdentity,
) -> Result<(Arc<dyn PackageProvider>, PackageId, QualifiedPath)> {
    match input {
        SourceInput::Path(path) => {
            if let Some((provider, package_id, package_root_abs)) =
                find_manifest_package(&path, language)?
            {
                let input_abs = path.canonicalize().unwrap_or_else(|_| path.clone());
                let module_path = module_path_for_language(language, &package_root_abs, &input_abs)?;
                Ok((provider, package_id, module_path))
            } else {
                // A standalone file with no discoverable enclosing
                // project — each language's own provider treats it as a
                // one-member package (e.g. `RustPackageProvider`/
                // `MagnetWorkspaceProvider`'s `MemberRoot::File`,
                // `NativeObjectPackageProvider` for a native object with
                // no text to parse at all), so `resolve_input_package`
                // itself doesn't need to know per-language parsing
                // details — it just asks for that package's own id.
                let provider = provider_for_language(language, &path).ok_or_else(|| {
                    CliError::Compilation(format!(
                        "no provider for language {language} (or it doesn't support single-file input)"
                    ))
                })?;
                let package_id = provider
                    .list_packages()
                    .map_err(|e| CliError::Compilation(e.to_string()))?
                    .into_iter()
                    .next()
                    .ok_or_else(|| {
                        CliError::Compilation(format!(
                            "provider for language {language} produced no packages for {}",
                            path.display()
                        ))
                    })?;
                let module_path = QualifiedPath::new(Vec::new());
                Ok((provider, package_id, module_path))
            }
        }
        SourceInput::InMemory(source) => {
            let package_id = PackageId::new(identity.path.path().head().ok_or_else(|| {
                CliError::Compilation("source file has no package identity".to_string())
            })?);
            let module_path = identity.path.path().clone();
            let provider = in_memory_provider(package_id.clone(), module_path.clone(), source)
                .map_err(|e| CliError::Compilation(e.to_string()))?;
            Ok((provider, package_id, module_path))
        }
    }
}

/// Resolves a real on-disk `path` to `(provider, package_id, module_path)` —
/// the real enclosing package if discoverable, else a single-member
/// package — for callers outside this module (`commands::compile`'s
/// `--target` pipeline) that need the same resolution `compile_source_file`
/// uses, instead of maintaining a second implementation.
pub fn resolve_source_package(
    path: &Path,
    language: &str,
    package: &str,
) -> Result<(Arc<dyn PackageProvider>, PackageId, QualifiedPath)> {
    let identity = CompilerIdentity::for_file(package, path);
    resolve_input_package(SourceInput::Path(path.to_path_buf()), language, &identity)
}

fn compile_source_file(
    input: SourceInput,
    language: &str,
    identity: &CompilerIdentity,
    executor: &CompilerExecutor,
    pipeline: PipelineMode,
) -> Result<CompilerDriver> {
    let (input_provider, package_id, module_path) =
        resolve_input_package(input, language, identity)?;

    let std_provider = std_provider_for(language);
    let provider = Arc::new(fp_core::package::provider::CompositeProvider::new(
        vec![std_provider],
        input_provider,
    ));
    let workspace = std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new(provider));
    let mut session = CompilerSession::new(data_layout(), executor, workspace);
    session.driver().pipeline = pipeline;
    executor
        .run(session.driver().compile_package(&package_id))
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    session
        .driver()
        .focus_package(package_id.clone())
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    // Only evaluate comptime LIR for full native compilation
    if pipeline == PipelineMode::Native {
        executor
            .run(session.driver().compile_package_module_native(
                &package_id,
                &module_path,
                "main",
            ))
            .map_err(|err| CliError::Compilation(err.to_string()))?;
    }
    Ok(session.into_driver())
}

pub fn drain_driver(driver: &mut CompilerDriver) -> Result<()> {
    emit_typing_diagnostics(&driver.state.borrow().typing_ctx.diagnostics.borrow())
}

pub fn parse_expr_with_mode(source: &str, parse_mode: FrontendParseMode) -> Result<File> {
    let frontend = FerroFrontend::new();
    frontend.set_parse_mode(parse_mode);
    let FrontendResult {
        ast, diagnostics, ..
    } = frontend
        .parse_expr(source)
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    emit_frontend_diagnostics(&diagnostics.get_diagnostics())?;
    Ok(ast)
}

#[derive(Debug, Clone)]
pub struct FrontendBundle {
    pub source_language: String,
}

#[derive(Debug, Clone)]
pub struct MirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: fp_core::hir::Program,
    pub mir_program: fp_core::mir::Program,
}

#[derive(Debug, Clone)]
pub struct LirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: fp_core::hir::Program,
    pub mir_program: fp_core::mir::Program,
    pub lir_program: fp_core::lir::LirProgram,
}

pub fn compile_file_to_lir_bundle(
    path: &Path,
    package: &str,
    source_language: Option<&str>,
) -> Result<LirBundle> {
    let language = resolve_source_language(path, source_language)?;
    let identity = CompilerIdentity::for_file(package, path);
    let executor = CompilerExecutor::new();
    let mut driver = compile_source_file(
        SourceInput::Path(path.to_path_buf()),
        &language,
        &identity,
        &executor,
        PipelineMode::Native,
    )?;
    drain_driver(&mut driver)?;
    let lowered = LoweredProgram {
        driver,
        package_id: PackageId::new(identity.path.path().head().ok_or_else(|| {
            CliError::Compilation("source file has no package identity".to_string())
        })?),
        executor,
    };
    Ok(LirBundle {
        frontend: FrontendBundle {
            source_language: language,
        },
        hir_program: lowered.hir()?,
        mir_program: lowered.mir()?,
        lir_program: lowered.lir()?,
    })
}

/// Builds the executor/provider/workspace/session a typechecking compile
/// needs — shared by `compile_emit_target`'s single-package path and
/// `compile_project`'s/`compile_project_external`'s (`fp-cli/src/commands/
/// compile.rs`) whole-workspace path, so a workspace compile builds this
/// once for every member instead of once per member. Callers compile via
/// `session.driver().compile_package`/`compile_workspace`, then read back
/// each package's typed `PackageSource` via `WorkspaceContext::package_source`
/// — never by hand-extracting it themselves.
pub fn build_workspace_session(
    provider: Arc<dyn PackageProvider>,
    language: &str,
    capabilities: fp_core::capabilities::LanguageCapabilities,
) -> (CompilerExecutor, CompilerSession) {
    let executor = CompilerExecutor::new();
    let std_provider = std_provider_for(language);
    let combined = Arc::new(fp_core::package::provider::CompositeProvider::new(
        vec![std_provider],
        provider,
    ));
    let workspace = std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new(combined));
    let mut session = CompilerSession::new(data_layout(), &executor, workspace);
    session.driver().pipeline = PipelineMode::TypecheckedTranspile;
    session.driver().state.borrow_mut().set_capabilities(capabilities);
    (executor, session)
}

/// Resolves the effective source language for `path`: an explicit
/// `source_language` override, else extension-based detection. No silent
/// default — an undetectable language (unknown/missing extension, no
/// override) is a real error, not a guess at FerroPhase.
pub(crate) fn resolve_source_language(path: &Path, source_language: Option<&str>) -> Result<String> {
    if let Some(lang) = source_language {
        return Ok(lang.trim().to_ascii_lowercase());
    }
    detect_source_language(path)
        .map(|lang| lang.name.to_ascii_lowercase())
        .ok_or_else(|| {
            CliError::InvalidInput(format!(
                "cannot detect source language for {}: pass --source-language explicitly",
                path.display()
            ))
        })
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
    diagnostic.as_core_diagnostic()
}

struct CompilerIdentity {
    path: FullyQualifiedPath,
}

struct LoweredProgram {
    driver: CompilerDriver,
    package_id: PackageId,
    executor: CompilerExecutor,
}


impl LoweredProgram {
    fn hir(&self) -> Result<fp_core::hir::Package> {
        let package = self.compiled_package()?;
        let package = package.borrow();
        package.hir_program.clone().ok_or_else(|| {
            CliError::Compilation(format!(
                "compiled package `{}` contains no HIR program",
                self.package_id
            ))
        })
    }

    fn mir(&self) -> Result<fp_core::mir::Program> {
        let package = self.compiled_package()?;
        let package = package.borrow();
        package.mir_program.clone().ok_or_else(|| {
            CliError::Compilation(format!(
                "compiled package `{}` contains no MIR program",
                self.package_id
            ))
        })
    }

    /// Native/LLVM/Cranelift emitters all consume a single flattened
    /// `LirProgram` merging every dependency's compiled LIR workspace in
    /// before this package's own (mirroring the same merge
    /// `evaluate_comptime_lir` already does for comptime execution — a
    /// cross-package call type-checks and lowers fine on just this
    /// package's own workspace, since the callee's *signature* is
    /// predeclared into this package's generator, but without the
    /// dependency's workspace folded in too, its function *body* never
    /// reaches the emitted binary), then best-effort resolves and renames
    /// a `main` entrypoint the same way `CompilerDriver::select_entrypoint`
    /// does — this path builds its own `LirProgram` straight from the
    /// workspace rather than going through `select_entrypoint`, so a
    /// mangled `main` needs the same rename here too. See
    /// `fp_core::workspace::WorkspaceContext::merged_lir_program`, which
    /// owns the actual merge/rename logic this delegates to (package-based,
    /// not module-based — see that method's doc comment).
    fn lir(&self) -> Result<fp_core::lir::LirProgram> {
        let workspace = self.compiled_workspace()?;
        workspace
            .merged_lir_program(&self.package_id)
            .map_err(|error| CliError::Compilation(error.to_string()))
    }

    fn compiled_package(
        &self,
    ) -> Result<std::rc::Rc<std::cell::RefCell<fp_core::package::CompiledPackage>>> {
        self.driver
            .state
            .borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&self.package_id)
            .ok_or_else(|| {
                CliError::Compilation(format!(
                    "compiled package `{}` is unavailable",
                    self.package_id
                ))
            })
    }

    /// Every package this run's driver state knows about (dependencies and
    /// this package itself), as a `WorkspaceContext` — the input every
    /// `TargetBackend` reads from.
    fn compiled_workspace(&self) -> Result<std::rc::Rc<fp_core::workspace::WorkspaceContext>> {
        Ok(self.driver.state.borrow().typing_ctx.env_ctx.clone())
    }

    #[allow(dead_code)]
    fn bytecode(&mut self) -> Result<fp_bytecode::BytecodeProgram> {
        self.executor
            .run(self.driver.compile_bytecode(&self.package_id))
            .map_err(|err| CliError::Compilation(err.to_string()))
    }
}

impl CompilerIdentity {
    fn for_script() -> Self {
        Self::new(vec!["cli".to_string(), "eval_script".to_string()])
    }

    fn for_file(package: &str, path: &Path) -> Self {
        let module = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_owned)
            .unwrap_or_else(|| "module".to_string());
        Self::new(vec![package.to_string(), module])
    }

    fn new(segments: Vec<String>) -> Self {
        let path = FullyQualifiedPath::from_segments(segments);
        Self { path }
    }
}
