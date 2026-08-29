use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_compiler::{
    CompilerDriver, CompilerExecutor, CompilerSession, FullyQualifiedPath, PipelineMode,
};
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::path::QualifiedPath;
use fp_core::{
    ast::File,
    diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel, DiagnosticManager},
    frontend::{FrontendParseMode, FrontendResult, LanguageFrontend},
    lir::LirDataLayout,
};
use fp_lang::FerroFrontend;

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

/// `"std"`/`"libc"` resolve against different providers depending on the
/// active source language: `fp_lang`'s hand-written `.fp` reimplementation
/// for `.fp`-dialect projects, or real rustc source (`fp-rust`'s
/// `RustStdProvider`, see `docs/RustStd.md`) for real `.rs`/Cargo projects.
/// Languages whose providers do not use FerroPhase's std packages receive an
/// empty dependency provider. Source-language validation remains the
/// responsibility of provider/backend discovery, so adding a new registered
/// language cannot turn a valid compile into a late panic here.
fn std_provider_for(language: &str) -> Arc<dyn fp_core::ast::package::provider::PackageProvider> {
    match language {
        l if l == languages::FERROPHASE => Arc::new(fp_lang::provider::FerroPhaseProvider),
        l if l == languages::RUST => Arc::new(fp_rust::RustStdProvider),
        // A native object/archive/asm-text/goasm/URCL/JVM-bytecode/CIL
        // package has no std/libc dependency at all.
        l if l == languages::NATIVE_OBJECT
            || l == languages::NATIVE_ARCHIVE
            || l == languages::NATIVE_ASM
            || l == "x86_64-asm"
            || l == "aarch64-asm"
            || l == "asm-x86_64"
            || l == "asm-aarch64"
            || l == "x86asm"
            || l == "aarch64asm"
            || l == languages::GOASM
            || l == languages::URCL
            || l == languages::JVM_BYTECODE
            || l == languages::CIL =>
        {
            Arc::new(fp_core::ast::package::provider::EmptyProvider)
        }
        _ => Arc::new(fp_core::ast::package::provider::EmptyProvider),
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
    let Some(manifest_root) = fp_lang::project::find_manifest(&input_abs) else {
        return Ok(None);
    };
    let root = if language == languages::RUST || language == "rs" {
        cargo_workspace_root(&manifest_root)
    } else {
        manifest_root
    };
    let provider =
        crate::languages::package_provider_registry::provider_for_language(language, &root)
            .ok_or_else(|| {
                CliError::Compilation(format!(
                    "no package provider for source language: {language}"
                ))
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

/// Resolve the Cargo workspace root for a package manifest. Cargo first uses
/// an explicit `[package].workspace` path when present; otherwise it searches
/// ancestors for the nearest manifest containing `[workspace]`. A standalone
/// package with neither declaration remains rooted at its own manifest.
fn cargo_workspace_root(package_root: &Path) -> PathBuf {
    let manifest_path = package_root.join("Cargo.toml");
    if let Ok(content) = std::fs::read_to_string(&manifest_path) {
        if let Ok(manifest) = toml::from_str::<toml::Table>(&content) {
            if let Some(workspace) = manifest
                .get("package")
                .and_then(|package| package.get("workspace"))
                .and_then(toml::Value::as_str)
            {
                let explicit_root = package_root.join(workspace);
                if explicit_root.join("Cargo.toml").is_file() {
                    return explicit_root.canonicalize().unwrap_or(explicit_root);
                }
            }
        }
    }

    let mut current = package_root.to_path_buf();
    loop {
        let manifest_path = current.join("Cargo.toml");
        if let Ok(content) = std::fs::read_to_string(manifest_path) {
            if let Ok(manifest) = toml::from_str::<toml::Table>(&content) {
                if manifest.get("workspace").is_some() {
                    return current;
                }
            }
        }
        if !current.pop() {
            return package_root.to_path_buf();
        }
    }
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
    Ok(QualifiedPath::new(
        fp_typescript::package::estimate_module_path(package_root, input),
    ))
}

#[cfg(not(feature = "lang-typescript"))]
fn module_path_for_typescript(_package_root: &Path, _input: &Path) -> Result<QualifiedPath> {
    Err(CliError::Compilation(
        "typescript support not compiled into this build".to_string(),
    ))
}

/// A single compiler input: either a real on-disk file (the common case —
/// parsed lazily, once a `PackageProvider` actually asks for its source), or
/// an already-built in-memory `File` with no path to read from. Two
/// genuinely different kinds of input, not one file-focused path with a
/// bolted-on exception.
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
                let module_path =
                    module_path_for_language(language, &package_root_abs, &input_abs)?;
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
    let provider = Arc::new(fp_core::ast::package::provider::CompositeProvider::new(
        vec![std_provider],
        input_provider,
    ));
    let workspace = std::rc::Rc::new(fp_core::ast::program::AstProgram::new(provider));
    let mut session = CompilerSession::new(data_layout(), executor, workspace);
    session.driver().pipeline = pipeline;
    executor
        .run(session.driver().compile_package(&package_id))
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    // Only evaluate comptime LIR for full native compilation
    if pipeline == PipelineMode::Native {
        executor
            .run(
                session
                    .driver()
                    .compile_package_module_native(&package_id, &module_path, "main"),
            )
            .map_err(|err| CliError::Compilation(err.to_string()))?;
    }
    Ok(session.into_driver())
}

pub fn drain_driver(driver: &mut CompilerDriver) -> Result<()> {
    // Typing diagnostics live on each compiled package's own `HirPackage`
    // (see its `diagnostics` field's doc comment), not on the driver's
    // scratch, per-package `TypingShared` — that's discarded the moment
    // each package's compile finishes, before this ever runs.
    let diagnostics: Vec<_> = driver
        .state
        .borrow()
        .all_packages()
        .into_iter()
        .flat_map(|package| package.borrow().diagnostics.get_diagnostics())
        .collect();
    let result = emit_typing_diagnostics(&diagnostics);
    if driver.pipeline == PipelineMode::Transpile {
        return Ok(());
    }
    result
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
    pub hir_program: fp_core::hir::HirPackage,
    pub mir_program: fp_core::mir::MirCodeUnit,
}

#[derive(Debug, Clone)]
pub struct LirBundle {
    pub frontend: FrontendBundle,
    pub hir_program: fp_core::hir::HirPackage,
    pub mir_program: fp_core::mir::MirCodeUnit,
    pub lir_program: fp_core::lir::LirBlob,
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
/// each package's typed `AstPackage` via `AstProgram::package_source`
/// — never by hand-extracting it themselves.
pub fn build_workspace_session(
    provider: Arc<dyn PackageProvider>,
    language: &str,
    backend_capabilities: fp_core::capabilities::LanguageCapabilities,
) -> (CompilerExecutor, CompilerSession) {
    let executor = CompilerExecutor::new();
    let std_provider = std_provider_for(language);
    let combined = Arc::new(fp_core::ast::package::provider::CompositeProvider::new(
        vec![std_provider],
        provider,
    ));
    let workspace = std::rc::Rc::new(fp_core::ast::program::AstProgram::new(combined));
    let mut session = CompilerSession::new(data_layout(), &executor, workspace);
    session.driver().pipeline = PipelineMode::Transpile;
    session
        .driver()
        .state
        .borrow_mut()
        .set_backend_capabilities(backend_capabilities);
    (executor, session)
}

/// Resolves the effective source language for `path`: an explicit
/// `source_language` override, else extension-based detection. No silent
/// default — an undetectable language (unknown/missing extension, no
/// override) is a real error, not a guess at FerroPhase.
pub(crate) fn resolve_source_language(
    path: &Path,
    source_language: Option<&str>,
) -> Result<String> {
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

fn emit_typing_diagnostics(diagnostics: &[Diagnostic]) -> Result<()> {
    DiagnosticManager::emit(
        diagnostics,
        Some("typing"),
        &DiagnosticDisplayOptions::default(),
    );
    if diagnostics
        .iter()
        .any(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
    {
        return Err(CliError::Compilation(
            "typing stage failed; see diagnostics for details".to_string(),
        ));
    }
    Ok(())
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
    fn hir(&self) -> Result<fp_core::hir::HirPackage> {
        let package = self.compiled_package()?;
        let hir_package_id = package.borrow().hir_package_id.clone();
        self.driver.state.borrow().hir(hir_package_id).map_err(|_| {
            CliError::Compilation(format!(
                "compiled package `{}` contains no HIR program",
                self.package_id
            ))
        })
    }

    fn mir(&self) -> Result<fp_core::mir::MirCodeUnit> {
        let mut mir = fp_core::mir::MirCodeUnit::new();
        {
            let state = self.driver.state.borrow();
            if let Some(package) = state.mir_program().package(&self.package_id) {
                let package = package.borrow();
                mir.items.extend(package.items().cloned());
                mir.bodies
                    .extend(package.bodies().map(|(id, body)| (*id, body.clone())));
            }
        }
        if mir.items.is_empty() {
            return Err(CliError::Compilation(format!(
                "compiled package `{}` contains no MIR program",
                self.package_id
            )));
        }
        Ok(mir)
    }

    /// Native/LLVM/Cranelift emitters all consume a single flattened
    /// `LirBlob` merging every dependency's compiled LIR in before this
    /// package's own (mirroring the same merge `evaluate_comptime_lir`
    /// already does for comptime execution — a cross-package call
    /// type-checks and lowers fine on just this package's own workspace,
    /// since the callee's *signature* is predeclared into this package's
    /// generator, but without the dependency's LIR folded in too, its
    /// function *body* never reaches the emitted binary), then
    /// best-effort resolves and renames a `main` entrypoint the same way
    /// `CompilerDriver::select_entrypoint` does — this path builds its own
    /// `LirBlob` directly rather than going through `select_entrypoint`,
    /// so a mangled `main` needs the same rename here too. See
    /// `fp_core::lir::LirProgram::merged_blob_for_package`, which owns the
    /// actual merge logic this delegates to (package-based, not
    /// module-based — see `fp_core::ast::package::resolve_entrypoint_def_id`'s
    /// doc comment).
    fn lir(&self) -> Result<fp_core::lir::LirBlob> {
        let mut blob = self
            .driver
            .state
            .borrow()
            .lir_program()
            .merged_blob_for_package(&self.package_id)
            .map_err(|error| CliError::Compilation(error.to_string()))?;
        if let Ok(hir_package) = self.hir() {
            if let Ok(def_id) = fp_core::ast::package::resolve_entrypoint_def_id(
                &self.package_id,
                &hir_package,
                "main",
            ) {
                fp_core::ast::package::rename_lir_function(&mut blob, def_id, "main");
            }
        }
        Ok(blob)
    }

    fn compiled_package(
        &self,
    ) -> Result<std::rc::Rc<std::cell::RefCell<fp_core::ast::package::CompiledPackage>>> {
        self.driver
            .state
            .borrow()
            .workspace
            .compiled_package(&self.package_id)
            .ok_or_else(|| {
                CliError::Compilation(format!(
                    "compiled package `{}` is unavailable",
                    self.package_id
                ))
            })
    }

    /// Every package this run's driver state knows about (dependencies and
    /// this package itself), as a `AstProgram` — the input every
    /// `TargetBackend` reads from.
    fn compiled_workspace(&self) -> Result<std::rc::Rc<fp_core::ast::program::AstProgram>> {
        Ok(self.driver.state.borrow().workspace.clone())
    }

    #[allow(dead_code)]
    fn bytecode(&mut self) -> Result<fp_bytecode::BytecodeProgram> {
        self.executor
            .run(self.driver.compile_bytecode(&self.package_id))
            .map_err(|err| CliError::Compilation(err.to_string()))
    }
}

impl CompilerIdentity {
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

#[cfg(test)]
mod tests {
    use super::{cargo_workspace_root, resolve_source_package, std_provider_for};
    use std::fs;
    use std::path::Path;
    use tempfile::tempdir;

    #[test]
    fn languages_without_ferrophase_std_use_empty_provider() {
        for language in ["c", "python", "typescript", "future-language"] {
            assert!(
                std_provider_for(language)
                    .list_packages()
                    .unwrap()
                    .is_empty()
            );
        }
    }

    #[test]
    fn cargo_workspace_root_honors_explicit_package_workspace() {
        let temp = tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        let package = workspace.join("crates/member");
        fs::create_dir_all(&package).unwrap();
        fs::write(
            workspace.join("Cargo.toml"),
            "[workspace]\nmembers = [\"crates/member\"]\n",
        )
        .unwrap();
        fs::write(
            package.join("Cargo.toml"),
            "[package]\nname = \"member\"\nworkspace = \"../..\"\n",
        )
        .unwrap();

        assert_eq!(
            cargo_workspace_root(&package),
            workspace.canonicalize().unwrap()
        );
    }

    #[test]
    fn cargo_workspace_root_infers_nearest_workspace_manifest() {
        let temp = tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        let package = workspace.join("crates/member");
        fs::create_dir_all(&package).unwrap();
        fs::write(
            workspace.join("Cargo.toml"),
            "[workspace]\nmembers = [\"crates/member\"]\n",
        )
        .unwrap();
        fs::write(package.join("Cargo.toml"), "[package]\nname = \"member\"\n").unwrap();

        assert_eq!(cargo_workspace_root(&package), workspace);
    }

    #[test]
    fn cargo_workspace_root_keeps_standalone_package_root() {
        let temp = tempdir().unwrap();
        let package = temp.path().join("package");
        fs::create_dir_all(&package).unwrap();
        fs::write(
            package.join("Cargo.toml"),
            "[package]\nname = \"package\"\n",
        )
        .unwrap();

        assert_eq!(cargo_workspace_root(&package), package);
    }

    #[test]
    fn resolve_source_package_selects_member_from_workspace_provider() {
        let temp = tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        let member = workspace.join("crates/member");
        let sibling = workspace.join("crates/sibling");
        fs::create_dir_all(member.join("src")).unwrap();
        fs::create_dir_all(sibling.join("src")).unwrap();
        fs::write(
            workspace.join("Cargo.toml"),
            "[workspace]\nmembers = [\"crates/member\", \"crates/sibling\"]\n",
        )
        .unwrap();
        fs::write(
            member.join("Cargo.toml"),
            "[package]\nname = \"selected-member\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            sibling.join("Cargo.toml"),
            "[package]\nname = \"sibling\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let input = member.join("src/lib.rs");
        fs::write(&input, "pub fn selected() {}\n").unwrap();

        let (provider, package_id, module_path) =
            resolve_source_package(Path::new(&input), "rust", "cli").unwrap();

        assert_eq!(package_id.as_str(), "selected-member");
        assert!(module_path.segments.is_empty());
        let packages = provider.list_packages().unwrap();
        assert!(packages.iter().any(|id| id.as_str() == "selected-member"));
        assert!(packages.iter().any(|id| id.as_str() == "sibling"));
    }
}
