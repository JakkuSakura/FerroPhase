//! Compilation command implementation

use crate::commands::{setup_progress_bar, validate_paths_exist};
use crate::compiler;
use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::File;
use fp_core::package::{PackageId, PackageSource};
use std::path::{Path, PathBuf};
use tracing::{info, warn};

use clap::Args;
/// Arguments for the compile command (also used by Clap)
#[derive(Debug, Clone, Args)]
pub struct CompileArgs {
    /// Input file(s) to compile
    #[arg(required = true)]
    pub input: Vec<PathBuf>,
    /// Package name used to qualify source identities (auto-detected for projects)
    #[arg(long = "package")]
    pub package: Option<String>,

    /// Output target: a codegen backend (native, goasm, urcl, llvm-binary,
    /// llvm-text, cranelift, ebpf, cil, dotnet, bytecode, text-bytecode,
    /// jvm-bytecode, wasm, interpret) or a language target (fp, typescript,
    /// javascript, python, go, gdscript, zig, sycl, rust, wit, ...) or a
    /// runtime-registered target — all just `TargetBackend` impls looked up
    /// by name, no separate protocol.
    #[arg(short = 't', long = "target", default_value = "native")]
    pub target: String,

    /// Target triple for codegen (defaults to host if omitted)
    #[arg(long = "target-triple")]
    pub target_triple: Option<String>,

    /// Target CPU for codegen (optional)
    #[arg(long = "target-cpu")]
    pub target_cpu: Option<String>,

    /// Native target ISA/dialect override (for `--target native`).
    #[arg(long = "native-target")]
    pub native_target: Option<String>,

    /// Target feature string for codegen (optional)
    #[arg(long = "target-features")]
    pub target_features: Option<String>,

    /// Target sysroot for linking (optional)
    #[arg(long = "sysroot")]
    pub target_sysroot: Option<PathBuf>,

    /// Linker driver to invoke (defaults to `clang`).
    ///
    /// Examples: `clang`, `clang++`, `gcc`.
    #[arg(long = "linker", default_value = "clang")]
    pub linker: String,

    /// Explicit link editor override (passed as `-fuse-ld=<path>` to clang).
    #[arg(long = "fuse-ld")]
    pub target_linker: Option<PathBuf>,

    /// Output file or directory
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Optimization level (0, 1, 2, 3)
    #[arg(short = 'O', long, default_value_t = 2)]
    pub opt_level: u8,

    /// Enable debug information
    #[arg(short, long)]
    pub debug: bool,

    /// Treat build as release (disables debug assertions)
    #[arg(long)]
    pub release: bool,

    /// Additional include directories
    #[arg(short = 'I', long)]
    pub include: Vec<PathBuf>,

    /// Define constants for compilation
    #[arg(short = 'D', long)]
    pub define: Vec<String>,

    /// Execute the compiled binary using exec clib function
    #[arg(short, long)]
    pub exec: bool,

    /// Link native object/binary inputs into an executable (without running it).
    ///
    /// This is primarily useful for native container inputs such as ELF/PE/Mach-O,
    /// where the default transpile output is an object file (`.o`).
    #[arg(long)]
    pub link: bool,

    /// Persist intermediate representations to disk
    #[arg(long)]
    pub save_intermediates: bool,

    /// Override automatic source language detection (e.g. "typescript")
    #[arg(long = "lang", alias = "language")]
    pub source_language: Option<String>,

    /// Generate type definitions for TypeScript target.
    #[arg(long)]
    pub type_defs: bool,

    /// Generate a single WIT world instead of per-package worlds.
    #[arg(long)]
    pub single_world: bool,
}

fn target_triple_matches_host(target_triple: &str) -> bool {
    let target_triple = target_triple.to_ascii_lowercase();
    let (host_arch, host_os) = (std::env::consts::ARCH, std::env::consts::OS);
    let arch_ok = match host_arch {
        "x86_64" => target_triple.starts_with("x86_64-"),
        "aarch64" => target_triple.starts_with("aarch64-") || target_triple.starts_with("arm64-"),
        other => target_triple.starts_with(&format!("{other}-")),
    };
    if !arch_ok {
        return false;
    }

    match host_os {
        "macos" => target_triple.contains("darwin"),
        "linux" => target_triple.contains("linux"),
        "windows" => target_triple.contains("windows"),
        other => target_triple.contains(other),
    }
}


/// Execute the compile command
pub async fn compile_command(args: CompileArgs, config: &CliConfig) -> Result<()> {
    validate_compile_target(&args.target)?;
    info!("Starting compilation with target: {}", args.target);

    // Validate inputs
    validate_inputs(&args)?;

    compile_once(args, config).await
}

async fn compile_once(args: CompileArgs, config: &CliConfig) -> Result<()> {
    let progress = setup_progress_bar(args.input.len());

    let target = args.target.as_str();
    let emit_text_bytecode = target == "text-bytecode";

    // `--exec` implies "link/emit as a runnable artifact" (`link_requested`)
    // regardless of whether it can actually run on this host — a
    // cross-target-triple `--exec` build still wants the fully-linked
    // artifact (e.g. a Windows PE with its import table), just not to
    // actually be executed here. Whether it's *run* is gated separately by
    // a target-triple/host match, below.
    let link_requested = args.link || args.exec;

    // A target-triple/host mismatch silently drops running the artifact
    // (with a warning) rather than failing the whole compile — the
    // compile itself is still valid cross-compile output, just not
    // runnable here.
    let exec = args.exec
        && match args.target_triple.as_deref() {
            None => true,
            Some(triple) => {
                let matches = target_triple_matches_host(triple);
                if !matches {
                    warn!(
                        "Skipping `--exec`: target triple `{}` does not match host",
                        triple
                    );
                }
                matches
            }
        };

    let container_registry = crate::container::ContainerRegistry::new();
    let output_is_dir = args
        .output
        .as_ref()
        .is_some_and(|path| args.input.len() > 1 || path.is_dir());

    for input_file in &args.input {
        progress.set_message(format!("Compiling {}", input_file.display()));

        // Classified exactly once per input, then threaded through both
        // output-path derivation and the actual compile/transpile dispatch
        // below — instead of each independently re-detecting (and, for the
        // byte-sniffed case, re-reading) the same input.
        let input_class =
            container_registry.classify_input(input_file, args.source_language.as_deref());

        let output_file = determine_output_path(
            input_file,
            args.output.as_ref(),
            target,
            args.target_triple.as_deref(),
            input_class,
            emit_text_bytecode,
            output_is_dir,
            link_requested,
            args.exec,
        )?;

        compile_file(input_file, &output_file, &args, config, input_class, exec)
            .await?;
        progress.inc(1);
    }

    progress.finish_with_message(format!(
        "{} Compiled {} file(s) successfully",
        style("✓").green(),
        args.input.len()
    ));

    Ok(())
}

// Note: former compile watch loop removed intentionally.

async fn compile_file(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    _config: &CliConfig,
    input_class: crate::container::InputClass,
    exec: bool,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());

    // `run_named_target` already handles directory vs. single-file input
    // itself (its own package discovery branches on it), so this only
    // needs one thing to decide up front: is this a container format
    // (archive/JVM/CIL/goasm/URCL) `maybe_transpile_container` still
    // hand-rolls its own binary-to-binary rewrite for? Everything else —
    // including native-object and native-asm-text input — flows through
    // `run_named_target`'s ordinary language-provider pipeline, the same
    // as any other language (`"object"`/`"native-asm"` resolve to
    // `fp_native::NativeObjectPackageProvider` there, not a special case
    // here).
    let container_kind = match input_class {
        crate::container::InputClass::Container(kind) => Some(kind),
        _ => None,
    };
    if let Some(artifact) =
        crate::container::maybe_transpile_container(input, output, args, _config, container_kind, exec)
            .await?
    {
        return Ok(Some(artifact));
    }

    run_named_target(input, output, args, &args.target, exec).await?;
    Ok(Some(output.to_path_buf()))
}

/// A single file is a package with one member — find (and prefer) the real
/// one if `input` belongs to a discoverable multi-file package; otherwise
/// wrap it as a synthetic single-member package. Delegates to
/// `compiler::resolve_source_package`, the same resolution
/// `compile_source_file` uses, instead of maintaining a second
/// independent implementation of "find (or synthesize) input's package".
fn provider_and_package_for_input(
    input: &Path,
    language: &str,
) -> Result<(
    std::sync::Arc<dyn fp_core::package::provider::PackageProvider>,
    PackageId,
    fp_core::ast::path::QualifiedPath,
)> {
    compiler::resolve_source_package(input, language, "cli")
}

/// Runs a `--target <name>` compile — built-in (`backend_for_target`) or
/// registered at runtime via `crate::languages::backend_registry` — for either a
/// whole directory/project or a single file, through the same package/
/// workspace discovery, typecheck, and `TargetBackend::compile_package`/
/// `write_workspace_files` pipeline either way. A single file is just the
/// trivial one-package case of the same discovery a directory input goes
/// through, not a separate code path — including a native object file
/// given directly as input (`"object"` resolves like any other language;
/// see `fp_native::NativeObjectPackageProvider` and
/// `compiler::resolve_input_package`), so `--exec` falls out of this
/// function's existing `backend.exec()` call for free, no bespoke runner
/// needed.
async fn run_named_target(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target_name: &str,
    exec: bool,
) -> Result<()> {
    if is_tsconfig(input) {
        return Err(CliError::Compilation(
            "fp compile --target requires source files, not tsconfig".to_string(),
        ));
    }

    // Constructed up front (before package discovery/typecheck) so the
    // pre-typecheck provider wrapping below can ask the backend for its own
    // materializer (`backend.materializer()`) instead of fp-cli keeping a
    // second, parallel name->materializer dispatch table.
    let root_name = input
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("workspace")
        .to_string();

    use crate::languages::detect_project_language;
    use crate::languages::package_provider_registry::provider_for_language;

    let (provider, packages, lang): (
        std::sync::Arc<dyn fp_core::package::provider::PackageProvider>,
        Vec<PackageId>,
        String,
    ) = if input.is_dir() {
        let lang = args
            .source_language
            .as_deref()
            .map(|l| l.trim().to_ascii_lowercase())
            .or_else(|| detect_project_language(input).map(|l| l.name.to_string()))
            .ok_or_else(|| {
                CliError::Compilation(format!(
                    "could not detect source language for project at {}: no Cargo.toml or Magnet.toml found; pass --source-language explicitly",
                    input.display()
                ))
            })?;
        let provider = provider_for_language(&lang, input)
            .ok_or_else(|| CliError::Compilation(format!("no provider for language: {lang}")))?;
        let packages = provider
            .list_packages()
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        (provider, packages, lang)
    } else {
        let lang = compiler::resolve_source_language(input, args.source_language.as_deref())?;
        let (provider, package_id, _tag) = provider_and_package_for_input(input, &lang)?;
        (provider, vec![package_id], lang)
    };

    // A container-input compile (e.g. a native object file given directly
    // as input) can legitimately just retarget the object without linking
    // it (`--link`/`--exec` both absent) — every ordinary source compile
    // always wants a runnable executable regardless of `--link`, matching
    // today's behavior.
    let link_requested = if lang == crate::languages::NATIVE_OBJECT || lang == crate::languages::NATIVE_ASM {
        args.link || args.exec
    } else {
        true
    };
    // Native asm-text input that isn't being linked/exec'd should stay as
    // human-readable assembly rather than getting reassembled to an
    // object it was never asked to produce.
    let emit_text = lang == crate::languages::NATIVE_ASM && !link_requested;
    let backend_config = fp_core::backend::BackendConfig::new(output.to_path_buf())
        .with_target_triple(args.target_triple.clone())
        .with_target_cpu(args.target_cpu.clone())
        .with_native_target(args.native_target.clone())
        .with_target_features(args.target_features.clone())
        .with_target_sysroot(args.target_sysroot.clone())
        .with_linker(args.linker.clone())
        .with_target_linker(args.target_linker.clone())
        .with_release(args.release)
        .with_debug_info(args.debug)
        .with_save_intermediates(args.save_intermediates)
        .with_type_defs(args.type_defs)
        .with_single_world(args.single_world)
        .with_root_name(root_name.clone())
        .with_link_requested(link_requested)
        .with_emit_text(emit_text);
    let backend = backend_for_target(target_name, backend_config)?;

    run_compile_pipeline(
        input,
        output,
        target_name,
        provider,
        packages,
        &lang,
        backend,
        &root_name,
        exec,
    )
    .await
}

/// Shared tail of every named-target compile: wraps `provider` with the
/// target-language materialize/normalize transforms, typechecks every
/// package in one `CompilerDriver::compile_workspace` call, hands each one
/// to `backend.compile_package`, then `write_workspace_files`/`exec`.
async fn run_compile_pipeline(
    input: &Path,
    output: &Path,
    target_name: &str,
    provider: std::sync::Arc<dyn fp_core::package::provider::PackageProvider>,
    packages: Vec<PackageId>,
    lang: &str,
    backend: Box<dyn fp_core::backend::TargetBackend>,
    root_name: &str,
    exec: bool,
) -> Result<()> {
    info!(
        "Project: {} package(s), language: {} (target: {})",
        packages.len(),
        lang,
        target_name
    );

    let mut file_count = 0;

    let normalizer = crate::languages::normalizer::normalizer_for_language(lang);
    let materializer = backend.materializer();

    // Wrap the real provider so `load_package_source` also applies the
    // target-language materialize + normalize transforms. Registered
    // as-is (not pre-resolved into a single snapshot) with the typechecker
    // below, so the driver can still do genuine resolution for any package
    // id it asks about (e.g. `std`), not just the one being typechecked.
    let materializing_provider: std::sync::Arc<dyn fp_core::package::provider::PackageProvider> =
        std::sync::Arc::new(TranspileMaterializingPackageProvider::new(
            provider.clone(),
            materializer.clone(),
            normalizer,
        ));

    // Phase 1: load + materialize + normalize + typecheck every package before
    // serializing any of them. A struct's fields can be defined in one
    // package and mutated through a `&mut` reference in another (e.g.
    // skln-core's `FileChange` mutated from skln-git's diff parser) — Kotlin
    // needs to know which fields are ever mutated *anywhere in the workspace*
    // to decide `val` vs `var` when emitting the struct, so that has to be
    // computed from every package's fully-processed AST, not just the one
    // currently being serialized.
    //
    // Every member is compiled through ONE `CompilerDriver::compile_workspace`
    // call, which treats the members as if they were dependencies of a
    // synthetic root package and walks them via `compile_package`'s own
    // recursive, cached, cycle-safe dependency machinery — so `std`/`libc`
    // and any inter-member dependency (a workspace path dependency) is
    // compiled exactly once for the whole workspace, not once per member. A
    // typecheck error in any one member fails the whole workspace compile
    // (not just that member) — a package's own dependencies already work
    // this way (an unresolvable dependency fails its dependent), so treating
    // members the same way needs no special per-member recovery bookkeeping
    // in the shared driver.
    let capabilities = crate::languages::backend::capabilities_for_target(target_name);
    let root_id = PackageId::new(format!("{root_name}::__workspace_root__"));
    let (executor, mut session) =
        compiler::build_workspace_session(materializing_provider.clone(), lang, capabilities);
    executor
        .run(session.driver().compile_workspace(&root_id, &packages))
        .map_err(|e| {
            CliError::Compilation(format!(
                "typecheck failed for project at {}: {}",
                input.display(),
                e
            ))
        })?;
    compiler::drain_driver(session.driver())?;
    let workspace = session.driver().state.borrow().typing_ctx.env_ctx.clone();

    let prepared: Vec<(PackageId, PackageSource)> = packages
        .iter()
        .map(|package_id| {
            workspace
                .package_source(package_id)
                .map(|source| (package_id.clone(), source))
                .map_err(|e| CliError::Compilation(e.to_string()))
        })
        .collect::<Result<Vec<_>>>()?;

    // Phase 2: serialize + write every package now that the workspace-wide
    // mutability set (and any other cross-package info) is complete.
    // Snapshotted so codegen-time diagnostics (e.g. a Kotlin function that
    // couldn't be transpiled — see `fp_kotlin`'s `report_untranspilable`)
    // get surfaced below instead of silently accumulating in the global
    // `DiagnosticManager` with nothing ever reading them back.
    let diagnostics_snapshot = fp_core::diagnostics::diagnostic_manager().snapshot();
    for (package_id, _source) in &prepared {
        // Any post-typecheck op materialization the backend needs (e.g.
        // Kotlin's portable-op -> Kotlin-idiom pass) happens inside
        // `compile_package` itself now, not here.
        backend
            .compile_package(&workspace, package_id)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        file_count += 1;
    }

    backend
        .write_workspace_files(&workspace)
        .map_err(|e| CliError::Compilation(e.to_string()))?;

    if exec {
        backend
            .exec()
            .map_err(|e| CliError::Compilation(e.to_string()))?;
    }

    let codegen_diagnostics =
        fp_core::diagnostics::diagnostic_manager().diagnostics_since(diagnostics_snapshot);
    fp_core::diagnostics::DiagnosticManager::emit(
        &codegen_diagnostics,
        Some(input.display().to_string().as_str()),
        &fp_core::diagnostics::DiagnosticDisplayOptions::default(),
    );

    info!(
        "Transpiled {} package(s) to {}",
        file_count,
        output.display()
    );
    Ok(())
}

/// Constructs the `TargetBackend` for `name` — a plain lookup into the
/// shared target-backend registry (`crate::languages::backend_registry`),
/// which built-in and externally `register_target_backend`-ed targets sit
/// in identically; no separate "is this built-in" branch here.
fn backend_for_target(
    name: &str,
    config: fp_core::backend::BackendConfig,
) -> Result<Box<dyn fp_core::backend::TargetBackend>> {
    crate::languages::backend_registry::backend_for_target(name, config)
}

fn is_tsconfig(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| {
            let lower = name.to_ascii_lowercase();
            lower == "tsconfig.json" || lower.ends_with(".tsconfig.json")
        })
        .unwrap_or(false)
}

/// Validates `--target <name>` up front, before the actual package
/// discovery/typecheck/backend-construction pipeline runs.
fn validate_compile_target(target: &str) -> Result<()> {
    if crate::languages::backend_registry::is_known_target(target) {
        Ok(())
    } else {
        Err(CliError::InvalidInput(format!("Unsupported target: {target}")))
    }
}

fn validate_inputs(args: &CompileArgs) -> Result<()> {
    let has_dir = args.input.iter().any(|p| p.is_dir());
    validate_paths_exist(&args.input, !has_dir, "compile")?;

    // Validate optimization level
    if args.opt_level > 3 {
        return Err(CliError::InvalidInput(
            "Optimization level must be 0-3".to_string(),
        ));
    }

    Ok(())
}

/// True when `target` should write to (or derive a name from) `output`/
/// `input` as-is, rather than applying the normal extension defaulting
/// below — i.e. every raw/foreign-artifact re-emission case a codegen
/// target must preserve verbatim (goasm/urcl always; `native` only when
/// re-emitting a foreign container input, since a fresh source compile
/// still wants the normal `.out`/`.exe` default).
fn is_raw_binary_passthrough(target: &str, input_class: crate::container::InputClass) -> bool {
    match target {
        "goasm" | "urcl" => true,
        "native" => !matches!(input_class, crate::container::InputClass::Source),
        _ => false,
    }
}

/// The default filename extension for `target`'s output, given which
/// raw/foreign artifact `input` classified as (if any, `native` only).
/// `native_link_requested` only matters for object/archive container
/// inputs re-emitted as native — deriving a bare `input.<ext>` path (no
/// `--output` given) always uses the unlinked extension regardless of
/// `--link`/`--exec` (pass `false`), while writing under an explicit
/// output *directory* respects it.
fn output_extension_for(
    target: &str,
    input_class: crate::container::InputClass,
    target_triple: Option<&str>,
    emit_text_bytecode: bool,
    native_link_requested: bool,
    exec_requested: bool,
) -> &'static str {
    use crate::container::{ContainerInputKind, InputClass};
    match target {
        "goasm" => "s",
        "urcl" => "urcl",
        "native" => match input_class {
            InputClass::NativeAsm(_) => "s",
            InputClass::Container(ContainerInputKind::NativeObject) => {
                if native_link_requested { "out" } else { "o" }
            }
            InputClass::Container(ContainerInputKind::NativeArchive) => {
                if native_link_requested { "out" } else { "a" }
            }
            InputClass::Container(
                ContainerInputKind::Urcl
                | ContainerInputKind::GoAsm
                | ContainerInputKind::Cil
                | ContainerInputKind::JvmBytecode,
            ) => "o",
            InputClass::Source => {
                if is_windows_target(target_triple) { "exe" } else { "out" }
            }
        },
        "llvm-binary" | "cranelift" => {
            if is_windows_target(target_triple) { "exe" } else { "out" }
        }
        "llvm-text" => "ll",
        "ebpf" => {
            if exec_requested { "o" } else { "ebpf" }
        }
        "cil" => "il",
        "dotnet" => "exe",
        "rust" | "rs" => "rs",
        "wasm" => "wasm",
        "bytecode" | "text-bytecode" => {
            if emit_text_bytecode { "ftbc" } else { "fbc" }
        }
        "jvm-bytecode" => "class",
        "interpret" => "out",
        _ => crate::languages::backend::DEFAULT_TARGET_OUTPUT_EXTENSION,
    }
}

fn determine_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: &str,
    target_triple: Option<&str>,
    input_class: crate::container::InputClass,
    emit_text_bytecode: bool,
    output_is_dir: bool,
    native_link_requested: bool,
    exec_requested: bool,
) -> Result<PathBuf> {
    if let Some(output) = output {
        if output_is_dir {
            let extension = output_extension_for(
                target,
                input_class,
                target_triple,
                emit_text_bytecode,
                native_link_requested,
                exec_requested,
            );
            let stem = input
                .file_stem()
                .and_then(|s| s.to_str())
                .ok_or_else(|| CliError::InvalidInput("Invalid input filename".to_string()))?;
            let mut path = output.join(stem);
            path.set_extension(extension);
            return Ok(path);
        }

        if is_raw_binary_passthrough(target, input_class) {
            return Ok(output.clone());
        }

        // Respect explicit `-o <path>.<ext>` even when the extension differs
        // from the target's default. Only fill the extension when the user
        // did not provide one.
        let mut path = output.clone();
        if path.extension().is_none() {
            let extension = output_extension_for(
                target,
                input_class,
                target_triple,
                emit_text_bytecode,
                native_link_requested,
                exec_requested,
            );
            path.set_extension(extension);
        }
        return Ok(path);
    }

    if target == "interpret" {
        return Err(CliError::InvalidInput(
            "Unknown target for output extension: interpret".to_string(),
        ));
    }

    let extension = output_extension_for(
        target,
        input_class,
        target_triple,
        emit_text_bytecode,
        native_link_requested,
        exec_requested,
    );
    Ok(input.with_extension(extension))
}

/// Wraps a real, already-discovered `PackageProvider` (e.g. `RustPackageProvider`,
/// covering an entire workspace) and applies the target-language materialize
/// + source-normalize transforms to every item `load_package_source` returns.
///
/// Exists so whole-package typechecking (`typecheck_package`) can register a
/// provider that does genuine resolution for *any* package id — including
/// `std`/dependencies the driver asks about internally — instead of a
/// one-off shim that only knows how to answer for a single pre-baked
/// `PackageSource` snapshot. `list_packages`/`load_package_metadata`/`refresh`
/// delegate straight through; only `load_package_source` adds work.
///
/// Used only by the `Transpile`/`TypecheckedTranspile` paths in this module
/// — `PipelineMode::Native` needs none of this (portable ops already
/// resolve to real std functions there; see `fp_native::NativeIntrinsicMaterializer`'s
/// doc comment), so it stays on the plain, unwrapped provider in
/// `compiler::compile_source_file`.
struct TranspileMaterializingPackageProvider {
    inner: std::sync::Arc<dyn fp_core::package::provider::PackageProvider>,
    materializer: Option<std::sync::Arc<dyn fp_core::intrinsics::IntrinsicMaterializer>>,
    normalizer: Option<std::sync::Arc<dyn fp_core::intrinsics::IntrinsicNormalizer>>,
}

impl TranspileMaterializingPackageProvider {
    fn new(
        inner: std::sync::Arc<dyn fp_core::package::provider::PackageProvider>,
        materializer: Option<std::sync::Arc<dyn fp_core::intrinsics::IntrinsicMaterializer>>,
        normalizer: Option<std::sync::Arc<dyn fp_core::intrinsics::IntrinsicNormalizer>>,
    ) -> Self {
        Self {
            inner,
            materializer,
            normalizer,
        }
    }
}

impl fp_core::package::provider::PackageProvider for TranspileMaterializingPackageProvider {
    fn list_packages(
        &self,
    ) -> fp_core::package::provider::ProviderResult<Vec<fp_core::package::PackageId>> {
        self.inner.list_packages()
    }

    fn workspace_packages(
        &self,
    ) -> fp_core::package::provider::ProviderResult<Vec<fp_core::package::PackageId>> {
        self.inner.workspace_packages()
    }

    fn load_package_metadata(
        &self,
        id: &fp_core::package::PackageId,
    ) -> fp_core::package::provider::ProviderResult<std::sync::Arc<fp_core::package::PackageDescriptor>>
    {
        self.inner.load_package_metadata(id)
    }

    fn refresh(&self) -> fp_core::package::provider::ProviderResult<()> {
        self.inner.refresh()
    }

    fn load_package_source(
        &self,
        id: &fp_core::package::PackageId,
    ) -> fp_core::package::provider::ProviderResult<PackageSource> {
        let mut source = self.inner.load_package_source(id)?;

        if let Some(ref mat) = self.materializer {
            for pkg_item in &mut source.items {
                let file = File {
                    path: PathBuf::new(),
                    attrs: vec![],
                    collected_items: vec![],
                    items: vec![pkg_item.item.clone()],
                };
                let file = fp_core::intrinsics::materialize_file(file, mat.as_ref())
                    .map_err(|e| fp_core::package::provider::ProviderError::other(e.to_string()))?;
                if let Some(item) = file.items.into_iter().next() {
                    pkg_item.item = item;
                }
            }
        }

        if let Some(ref norm) = self.normalizer {
            for pkg_item in &mut source.items {
                fp_lang::normalization::normalize_items(
                    std::slice::from_mut(&mut pkg_item.item),
                    norm.as_ref(),
                )
                .map_err(|e| fp_core::package::provider::ProviderError::other(e.to_string()))?;
            }
        }

        Ok(source)
    }
}

fn is_windows_target(target_triple: Option<&str>) -> bool {
    let triple = match target_triple {
        Some(triple) => triple,
        None => return cfg!(target_os = "windows"),
    };
    triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw")
}

// Progress bar helper moved to commands::common
