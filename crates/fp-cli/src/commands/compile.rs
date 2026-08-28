//! Compilation command implementation

use crate::commands::setup_progress_bar;
use crate::compiler;
use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::package::PackageId;
use std::path::{Path, PathBuf};
use tracing::{info, warn};

use clap::Args;
/// Arguments for the compile command (also used by Clap)
#[derive(Debug, Clone, Args)]
pub struct CompileArgs {
    /// Input file or directory to compile
    #[arg(required = true)]
    pub input: PathBuf,
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
    /// This is primarily useful for foreign-artifact inputs such as
    /// ELF/PE/Mach-O objects, where the default retargeted output is an
    /// unlinked object file (`.o`).
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
pub async fn compile_command(args: CompileArgs, _config: &CliConfig) -> Result<()> {
    info!("Starting compilation with target: {}", args.target);

    let progress = setup_progress_bar(1);

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

    let input_file = &args.input;

    progress.set_message(format!("Compiling {}", input_file.display()));

    // Resolve `-o`'s effective path from pure user intent — no target
    // knowledge here. Extension defaulting (if any) is each target's own
    // concern, decided by its own factory closure in
    // `crate::languages::backend_registry::builtin_target_backends` once it
    // has the resolved `BackendConfig`.
    let output_file = match args.output.as_ref() {
        Some(output) if output.is_dir() => {
            let stem = input_file
                .file_stem()
                .ok_or_else(|| CliError::InvalidInput("Invalid input filename".to_string()))?;
            output.join(stem)
        }
        Some(output) => output.clone(),
        None => input_file.with_extension(""),
    };

    compile_workspace_entrypoint(input_file, &output_file, &args, exec).await?;
    progress.inc(1);

    progress.finish_with_message(format!("{} Compiled successfully", style("✓").green()));

    Ok(())
}

// Note: former compile watch loop removed intentionally.

/// `input` is a workspace entrypoint, not necessarily a literal single
/// file — a directory resolves to that project's whole workspace, and a
/// single file resolves to a synthetic one-package workspace with that file
/// as its sole member (see `run_named_target`'s own directory/file split).
/// Either way this always ends up compiling a workspace, just picking a
/// different entry package depending on what `input` was — including a
/// foreign artifact (native object/archive/asm text, goasm, URCL, JVM
/// bytecode, CIL/.NET), which resolves like any other language through
/// its own `PackageProvider` (`fp_native::NativeObjectPackageProvider`
/// and friends), not a separate code path.
async fn compile_workspace_entrypoint(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    exec: bool,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());
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
    std::sync::Arc<dyn fp_core::ast::package::provider::PackageProvider>,
    PackageId,
    fp_core::ast::path::QualifiedPath,
)> {
    compiler::resolve_source_package(input, language, "cli")
}

/// Runs a `--target <name>` compile — built-in or runtime-registered
/// (`crate::languages::backend_registry`) — for a directory or a single
/// file, through the same discovery/typecheck/`TargetBackend::
/// emit_package_artifact`/`write_workspace_files` pipeline either way. A
/// single file (including a foreign artifact like a native object) is
/// just the trivial one-package case of the same discovery, not a
/// separate code path — so `--exec` falls out of `backend.exec()` for
/// free.
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

    // The source project's own name — feeds both a synthetic root package
    // id for typecheck (`root_id`, below) and, for Kotlin specifically,
    // `settings.gradle.kts`'s `rootProject.name` (`BackendConfig::root_name`).
    let root_name = input
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("workspace")
        .to_string();

    use crate::languages::detect_project_language;
    use crate::languages::package_provider_registry::provider_for_language;

    let (provider, packages, lang): (
        std::sync::Arc<dyn fp_core::ast::package::provider::PackageProvider>,
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

    // CIL text can be lifted into the compiler IR, but a compiled .NET
    // binary has no disassembler/transpilation path. Keep this diagnostic
    // tied to the declared input format and extension: test fixtures and
    // partially invalid binaries do not necessarily carry a valid PE header.
    if !input.is_dir()
        && lang == crate::languages::CIL
        && matches!(
            input.extension().and_then(|extension| extension.to_str()),
            Some(extension) if extension.eq_ignore_ascii_case("dll")
                || extension.eq_ignore_ascii_case("exe")
        )
        && target_name == "native"
    {
        return Err(CliError::Compilation(
            "binary .dll/.exe -> native transpilation is not implemented yet".to_string(),
        ));
    }

    // A foreign-artifact-input compile (a native object file, or asm/goasm/
    // URCL text, given directly as input) can legitimately just retarget
    // it without linking (`--link`/`--exec` both absent) — every ordinary
    // source compile always wants a runnable executable regardless of
    // `--link`, matching today's behavior.
    let is_foreign_artifact = matches!(
        lang.as_str(),
        l if l == crate::languages::NATIVE_OBJECT
            || l == crate::languages::NATIVE_ARCHIVE
            || l == crate::languages::NATIVE_ASM
            || l == "x86_64-asm"
            || l == "aarch64-asm"
            || l == crate::languages::GOASM
            || l == crate::languages::URCL
            || l == crate::languages::JVM_BYTECODE
            || l == crate::languages::CIL
    );
    let link_requested = if is_foreign_artifact {
        args.link || args.exec
    } else {
        true
    };
    // Native asm-text input that isn't being linked/exec'd should stay as
    // human-readable assembly rather than getting reassembled to an
    // object it was never asked to produce.
    let emit_text = matches!(
        lang.as_str(),
        crate::languages::NATIVE_ASM | "x86_64-asm" | "aarch64-asm"
    ) && !link_requested;
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
        .with_single_file_output(
            if !input.is_dir()
                && matches!(
                    target_name,
                    "typescript" | "ts" | "javascript" | "js" | "rust" | "gdscript"
                )
            {
                Some(output.to_path_buf())
            } else {
                None
            },
        )
        .with_root_name(root_name.clone())
        .with_link_requested(link_requested)
        .with_emit_text(emit_text)
        .with_exec_requested(args.exec);
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

/// Shared tail of every named-target compile: typechecks every package in
/// one `CompilerDriver::compile_workspace` call, hands each one to
/// `backend.emit_package_artifact`, then `write_workspace_files`/`exec`.
async fn run_compile_pipeline(
    input: &Path,
    output: &Path,
    target_name: &str,
    provider: std::sync::Arc<dyn fp_core::ast::package::provider::PackageProvider>,
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

    // Phase 1: load + typecheck every package before
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
    let backend_capabilities = backend.capabilities();
    let root_id = PackageId::new(format!("{root_name}::__workspace_root__"));
    let (executor, mut session) =
        compiler::build_workspace_session(provider.clone(), lang, backend_capabilities);
    // Source serializers consume lifted HIR, while native IR backends need
    // the HIR -> MIR -> LIR stages populated before emission.
    if matches!(
        target_name,
        "cil" | "dotnet" | "jvm-bytecode" | "urcl" | "ebpf" | "goasm"
    ) {
        session.driver().pipeline = fp_compiler::PipelineMode::Native;
    }
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
    if matches!(
        target_name,
        "native" | "cil" | "dotnet" | "jvm-bytecode" | "urcl" | "ebpf" | "goasm"
    ) {
        for package_id in &packages {
            executor
                .run(session.driver().lower_package_native_lir(package_id))
                .map_err(|error| CliError::Compilation(error.to_string()))?;
        }
    }
    if matches!(target_name, "bytecode" | "text-bytecode") {
        session.driver().pipeline = fp_compiler::PipelineMode::Native;
        session
            .driver()
            .state
            .borrow_mut()
            .set_bytecode_comptime(true);
        for package_id in &packages {
            executor
                .run(session.driver().compile_bytecode(package_id))
                .map_err(|error| CliError::Compilation(error.to_string()))?;
        }
    }
    let workspace = session.driver().state.borrow().workspace.clone();

    // Phase 2: serialize + write every package now that the workspace-wide
    // mutability set (and any other cross-package info) is complete.
    for package_id in &packages {
        // Any op materialization the backend needs (e.g. Kotlin's
        // portable-op -> Kotlin-idiom pass) happens inside
        // emit_package_artifact itself, not here.
        let mir_module = {
            let state = session.driver().state.borrow();
            let mut unit = fp_core::mir::MirCodeUnit::new();
            if let Some(package) = state.mir_program().package(package_id) {
                let package = package.borrow();
                unit.items.extend(package.items().cloned());
                unit.bodies
                    .extend(package.bodies().map(|(id, body)| (*id, body.clone())));
            }
            unit
        };
        let lir_blob = {
            let state = session.driver().state.borrow();
            state.lir_program().merged_blob_for_package(package_id).ok()
        }
        .map(|mut blob| {
            // Best-effort: resolve and rename `package_id`'s `main` to the
            // bare symbol name native/asm emitters look for (see
            // `fp_core::ast::package::resolve_entrypoint_def_id`/
            // `rename_lir_function`'s own doc comments) — silently left
            // unrenamed if `package_id` has no `main` (e.g. a library).
            if let Some(ast_package) = workspace.compiled_package(package_id) {
                let hir_package_id = ast_package.borrow().hir_package_id.clone();
                if let Ok(hir_package) = session.driver().state.borrow().hir(hir_package_id) {
                    if let Ok(def_id) = fp_core::ast::package::resolve_entrypoint_def_id(
                        package_id,
                        &hir_package,
                        "main",
                    ) {
                        fp_core::ast::package::rename_lir_function(&mut blob, def_id, "main");
                    }
                }
            }
            blob
        });
        backend
            .emit_package_artifact(&workspace, package_id, &mir_module, lir_blob.as_ref())
            .map_err(|e| CliError::Compilation(e.to_string()))?;
    }

    backend
        .write_workspace_files(&workspace)
        .map_err(|e| CliError::Compilation(e.to_string()))?;

    if exec {
        backend
            .exec()
            .map_err(|e| CliError::Compilation(e.to_string()))?;
    }

    info!(
        "Compiled {} package(s) to {}",
        packages.len(),
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
