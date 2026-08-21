//! Compilation command implementation

use crate::commands::{setup_progress_bar, validate_paths_exist};
use crate::container::NativeAsmSource;
use crate::compiler;
use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::File;
use fp_core::package::{PackageId, PackageSource};
use fp_native::asm::{aarch64::AsmAarch64Program, x86_64::AsmX86_64Program};
use fp_native::asmir::{lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64};
use fp_native::emit::{self, TargetArch};
use object::Object as _;
use std::io;
use std::path::{Path, PathBuf};
use tokio::{fs as async_fs, process::Command};
use tracing::{info, warn};

use clap::{ArgAction, Args};
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

    /// Disable pipeline stages by name (repeatable).
    #[arg(long = "disable-stage", action = ArgAction::Append)]
    pub disable_stage: Vec<String>,

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

        compile_file(
            input_file,
            &output_file,
            &args,
            config,
            input_class,
            link_requested,
            exec,
        )
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
    link_requested: bool,
    exec: bool,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());

    if input.is_dir() {
        run_named_target(input, output, args, &args.target, exec).await?;
        return Ok(Some(output.to_path_buf()));
    }

    let native_asm_kind = match input_class {
        crate::container::InputClass::NativeAsm(kind) => Some(kind),
        _ => None,
    };
    let container_kind = match input_class {
        crate::container::InputClass::Container(kind) => Some(kind),
        _ => None,
    };

    if let Some(artifact) =
        maybe_transpile_native_asm(input, output, args, native_asm_kind, exec).await?
    {
        return Ok(Some(artifact));
    }

    if let Some(artifact) = crate::container::maybe_transpile_container(
        input,
        output,
        args,
        _config,
        container_kind,
        link_requested,
        exec,
    )
    .await?
    {
        return Ok(Some(artifact));
    }

    if !args.disable_stage.is_empty() {
        warn!(
            "--disable-stage is ignored on the fp-compiler compile path: {}",
            args.disable_stage.join(", ")
        );
    }

    run_named_target(input, output, args, &args.target, exec).await?;
    Ok(Some(output.to_path_buf()))
}

async fn maybe_transpile_native_asm(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    source_kind: Option<crate::container::NativeAsmSource>,
    exec: bool,
) -> Result<Option<PathBuf>> {
    let Some(source_kind) = source_kind else {
        return Ok(None);
    };

    if args.target != "native" {
        return Err(CliError::InvalidInput(
            "native asm input currently requires `--target native`".to_string(),
        ));
    }
    if exec {
        return Err(CliError::InvalidInput(
            "`--exec` is not supported for native asm transpilation".to_string(),
        ));
    }

    let text = async_fs::read_to_string(input).await.map_err(|err| {
        CliError::Io(io::Error::other(format!("Failed to read asm input: {err}")))
    })?;
    let source_program = parse_native_asm_source(&text, source_kind)?;
    let (_, target_arch) = emit::detect_target(args.target_triple.as_deref())
        .map_err(|err| CliError::Compilation(err.to_string()))?;

    let output_program = match source_program {
        ParsedNativeAsm::X86_64(program) => {
            if matches!(target_arch, TargetArch::X86_64) {
                program.to_text()
            } else {
                let mut target_program = lift_from_x86_64(&program)
                    .map_err(|err| CliError::Compilation(err.to_string()))?;
                target_program.target.architecture = fp_core::asmir::AsmArchitecture::Aarch64;
                fp_native::asmir::normalize_for_target(&mut target_program);
                lower_to_aarch64(&target_program).to_text()
            }
        }
        ParsedNativeAsm::Aarch64(program) => {
            if matches!(target_arch, TargetArch::Aarch64) {
                program.to_text()
            } else {
                let mut target_program = lift_from_aarch64(&program)
                    .map_err(|err| CliError::Compilation(err.to_string()))?;
                target_program.target.architecture = fp_core::asmir::AsmArchitecture::X86_64;
                fp_native::asmir::normalize_for_target(&mut target_program);
                lower_to_x86_64(&target_program).to_text()
            }
        }
    };

    let output_path = if args.output.is_none() {
        input.with_extension("s")
    } else {
        output.to_path_buf()
    };
    async_fs::write(&output_path, output_program)
        .await
        .map_err(|err| {
            CliError::Io(io::Error::other(format!(
                "Failed to write asm output: {err}"
            )))
        })?;
    Ok(Some(output_path))
}

enum ParsedNativeAsm {
    X86_64(AsmX86_64Program),
    Aarch64(AsmAarch64Program),
}

fn parse_native_asm_source(text: &str, source: NativeAsmSource) -> Result<ParsedNativeAsm> {
    match source {
        NativeAsmSource::X86_64 => AsmX86_64Program::parse_text(text)
            .map(ParsedNativeAsm::X86_64)
            .map_err(|err| CliError::Compilation(format!("Failed to parse x86_64 asm: {err}"))),
        NativeAsmSource::Aarch64 => AsmAarch64Program::parse_text(text)
            .map(ParsedNativeAsm::Aarch64)
            .map_err(|err| CliError::Compilation(format!("Failed to parse aarch64 asm: {err}"))),
        NativeAsmSource::Auto => match AsmX86_64Program::parse_text(text) {
            Ok(program) => Ok(ParsedNativeAsm::X86_64(program)),
            Err(x86_err) => match AsmAarch64Program::parse_text(text) {
                Ok(program) => Ok(ParsedNativeAsm::Aarch64(program)),
                Err(aarch64_err) => Err(CliError::Compilation(format!(
                    "Failed to detect native asm dialect; x86_64: {x86_err}; aarch64: {aarch64_err}"
                ))),
            },
        },
    }
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
/// registered at runtime via `crate::languages::registry` — for either a
/// whole directory/project or a single file, through the same package/
/// workspace discovery, typecheck, and `TargetBackend::compile_package`/
/// `write_workspace_files` pipeline either way. A single file is just the
/// trivial one-package case of the same discovery a directory input goes
/// through, not a separate code path.
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

    use crate::languages::detect_project_language;
    use crate::languages::discovery::provider_for_language;

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
    let lang = lang.as_str();

    info!(
        "Project: {} package(s), language: {} (target: {})",
        packages.len(),
        lang,
        target_name
    );

    let mut file_count = 0;

    let normalizer = crate::languages::normalizer::normalizer_for_language(lang);
    let materializer = crate::languages::materializer::materializer_for_language(target_name);

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
    let root_name = input
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("workspace")
        .to_string();
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
        .with_single_world(args.single_world);
    let backend = resolve_target_backend(target_name, backend_config, root_name)?;

    // Phase 2: serialize + write every package now that the workspace-wide
    // mutability set (and any other cross-package info) is complete.
    // Snapshotted so codegen-time diagnostics (e.g. a Kotlin function that
    // couldn't be transpiled — see `fp_kotlin`'s `report_untranspilable`)
    // get surfaced below instead of silently accumulating in the global
    // `DiagnosticManager` with nothing ever reading them back.
    let diagnostics_snapshot = fp_core::diagnostics::diagnostic_manager().snapshot();
    for (package_id, _source) in &prepared {
        // Materialize portable ops (`IntrinsicCall(CallKind::Op(_))`) into
        // this target's real shape (`Some(x)` -> `x`, `Vec::new()` -> an
        // empty list literal, ...) *after* typechecked lifting produced
        // them — the pre-typecheck `TranspileMaterializingPackageProvider`
        // wrapping above only ever sees raw, pre-HIR source, so it never
        // observes ops that `HirToAstLifter` classifies post-typecheck
        // (`program.op_defs`, resolved by real `DefId`, not by name). The
        // lifter's own job stops at producing the bare op node; turning it
        // into this target's real code is this materializer's job.
        //
        // `prepared`'s own `PackageSource.items` must stay un-materialized
        // — Kotlin's cross-package mutability scan (read lazily from the
        // workspace by `KotlinBackend::ensure_scan`) needs the
        // pre-materialize shape — so this mutates the compiled package's
        // items in place instead of cloning into a throwaway workspace.
        if let Some(mat) = &materializer {
            let compiled_package = workspace.compiled_package(package_id).ok_or_else(|| {
                CliError::Compilation(format!(
                    "package `{package_id}` is unavailable for materialization"
                ))
            })?;
            let mut compiled_package = compiled_package.borrow_mut();
            for pkg_item in &mut compiled_package.items {
                pkg_item.item =
                    fp_core::intrinsics::materialize_item(pkg_item.item.clone(), mat.as_ref())
                        .map_err(|e| CliError::Compilation(e.to_string()))?;
            }
        }

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

/// Resolves `name` to a `TargetBackend`, trying built-ins first
/// (`backend_for_target`) then the runtime registry
/// (`crate::languages::registry::find_registered_target_backend`) — the
/// registry entry's `root_name` is unused (a registered backend already
/// captured whatever it needs at registration time), but every call site
/// builds it uniformly since most callers don't know in advance which
/// side will answer.
fn resolve_target_backend(
    name: &str,
    config: fp_core::backend::BackendConfig,
    root_name: String,
) -> Result<Box<dyn fp_core::backend::TargetBackend>> {
    if let Some(result) = backend_for_target(name, config, root_name) {
        return result;
    }
    crate::languages::registry::find_registered_target_backend(name)
        .map(|backend| -> Box<dyn fp_core::backend::TargetBackend> {
            struct Shared(std::sync::Arc<dyn fp_core::backend::TargetBackend>);
            impl fp_core::backend::TargetBackend for Shared {
                fn compile_package(
                    &self,
                    workspace: &fp_core::workspace::WorkspaceContext,
                    package_id: &PackageId,
                ) -> fp_core::error::Result<()> {
                    self.0.compile_package(workspace, package_id)
                }
                fn write_workspace_files(
                    &self,
                    workspace: &fp_core::workspace::WorkspaceContext,
                ) -> fp_core::error::Result<()> {
                    self.0.write_workspace_files(workspace)
                }
            }
            Box::new(Shared(backend))
        })
        .ok_or_else(|| CliError::InvalidInput(format!("Unsupported target: {name}")))
}

/// Error returned by an AST-target arm whose crate is gated behind a
/// disabled optional `lang-*` feature (see e.g. `lang-typescript` in this
/// crate's `Cargo.toml`).
fn disabled_feature_error(feature: &str, what: &str) -> CliError {
    CliError::InvalidInput(format!(
        "{what} requires the \"{feature}\" feature, which is disabled in this build"
    ))
}

/// Constructs the `TargetBackend` for a built-in target name — called
/// exactly once per invocation. Returns `None` for a name this function
/// doesn't recognize at all (so `resolve_target_backend` can fall through
/// to the runtime registry), `Some(Err(_))` for a recognized name whose
/// crate is a disabled optional feature.
#[allow(unused_variables)]
fn backend_for_target(
    name: &str,
    config: fp_core::backend::BackendConfig,
    root_name: String,
) -> Option<Result<Box<dyn fp_core::backend::TargetBackend>>> {
    let output = config.workspace_root.clone();
    Some(match name.to_lowercase().as_str() {
        "native" => {
            let native_target = match config.native_target.as_deref() {
                Some(value) => Some(
                    fp_native::config::NativeTarget::resolve(value, config.target_triple.as_deref())
                        .ok_or_else(|| {
                            CliError::Compilation(format!("Unsupported fp-native target: {value}"))
                        }),
                ),
                None => None,
            };
            match native_target.transpose() {
                Ok(native_target) => {
                    let mut cfg = fp_native::config::NativeConfig::executable(&output)
                        .with_target_triple(config.target_triple.clone())
                        .with_target_cpu(config.target_cpu.clone())
                        .with_native_target(native_target)
                        .with_target_features(config.target_features.clone())
                        .with_sysroot(config.target_sysroot.clone())
                        .with_fuse_ld(config.target_linker.clone())
                        .with_linker_driver(Some(config.linker.clone()))
                        .with_release(config.release);
                    if config.save_intermediates {
                        cfg = cfg.with_asm_dump(Some(output.with_extension("asm")));
                    }
                    let emitter = fp_native::NativeEmitter::new(cfg);
                    Ok(Box::new(emitter) as Box<dyn fp_core::backend::TargetBackend>)
                }
                Err(e) => Err(e),
            }
        }
        "goasm" => {
            let target = Some(fp_goasm::config::GoAsmTarget::resolve(
                config.target_triple.as_deref(),
            ));
            let cfg = fp_goasm::config::GoAsmConfig::new(&output)
                .with_target(target)
                .with_target_triple(config.target_triple.clone());
            let emitter = fp_goasm::GoAsmEmitter::new(cfg);
            Ok(Box::new(emitter))
        }
        "urcl" => {
            let emitter = fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(&output));
            Ok(Box::new(emitter))
        }
        "llvm-binary" | "llvm-text" => {
            #[cfg(feature = "llvm")]
            {
                Ok(Box::new(fp_llvm::LlvmBackend {
                    output: output.clone(),
                    target_triple: config.target_triple.clone(),
                    target_cpu: config.target_cpu.clone(),
                    target_features: config.target_features.clone(),
                    target_sysroot: config.target_sysroot.clone(),
                    linker: Some(config.linker.clone()),
                    target_linker: config.target_linker.clone(),
                    release: config.release,
                    debug_info: config.debug_info,
                    save_intermediates: config.save_intermediates,
                    text_only: name.eq_ignore_ascii_case("llvm-text"),
                }))
            }
            #[cfg(not(feature = "llvm"))]
            {
                Err(CliError::MissingDependency(
                    "Feature 'llvm' is disabled; enable it to use the LLVM backend.".to_string(),
                ))
            }
        }
        "cranelift" => {
            #[cfg(feature = "cranelift")]
            {
                Ok(Box::new(fp_cranelift::CraneliftBackend {
                    output: output.clone(),
                    target_triple: config.target_triple.clone(),
                    target_cpu: config.target_cpu.clone(),
                    target_features: config.target_features.clone(),
                    target_sysroot: config.target_sysroot.clone(),
                    linker: Some(config.linker.clone()),
                    target_linker: config.target_linker.clone(),
                    release: config.release,
                    save_intermediates: config.save_intermediates,
                }))
            }
            #[cfg(not(feature = "cranelift"))]
            {
                Err(CliError::MissingDependency(
                    "Feature 'cranelift' is disabled; enable it to use the Cranelift backend."
                        .to_string(),
                ))
            }
        }
        "bytecode" | "text-bytecode" => Ok(Box::new(fp_stackvm_bytecode::BytecodeBackend {
            output: output.clone(),
            emit_text: name.eq_ignore_ascii_case("text-bytecode")
                || output.extension().and_then(|ext| ext.to_str()) == Some("ftbc"),
            save_intermediates: config.save_intermediates,
        })),
        "jvm-bytecode" => Ok(Box::new(fp_jvm::JvmBackend {
            output: output.clone(),
            save_intermediates: config.save_intermediates,
        })),
        "wasm" => Ok(Box::new(fp_wasm::WasmBackend {
            output: output.clone(),
        })),
        "ebpf" => Ok(Box::new(fp_ebpf::EbpfBackend {
            output: output.clone(),
        })),
        "cil" => Ok(Box::new(fp_dotnet::CilBackend {
            output: output.clone(),
        })),
        "dotnet" => Ok(Box::new(fp_dotnet::DotnetBackend {
            output: output.clone(),
            save_intermediates: config.save_intermediates,
        })),
        "interpret" => Ok(Box::new(fp_interpret::InterpreterBackend)),
        "fp" | "ferro" | "ferrophase" => Ok(Box::new(fp_c::FerroPhaseAstBackend::new(config))),
        "typescript" | "ts" => {
            #[cfg(feature = "lang-typescript")]
            {
                Ok(Box::new(fp_typescript::TypeScriptBackend::new(config)))
            }
            #[cfg(not(feature = "lang-typescript"))]
            {
                Err(disabled_feature_error(
                    "lang-typescript",
                    "TypeScript package emission",
                ))
            }
        }
        "javascript" | "js" => {
            #[cfg(feature = "lang-typescript")]
            {
                Ok(Box::new(fp_typescript::JavaScriptBackend::new(config)))
            }
            #[cfg(not(feature = "lang-typescript"))]
            {
                Err(disabled_feature_error(
                    "lang-typescript",
                    "JavaScript package emission",
                ))
            }
        }
        "csharp" | "cs" | "c#" => {
            #[cfg(feature = "lang-csharp")]
            {
                Ok(Box::new(fp_csharp::CSharpBackend::new(config)))
            }
            #[cfg(not(feature = "lang-csharp"))]
            {
                Err(disabled_feature_error("lang-csharp", "C# package emission"))
            }
        }
        "kotlin" | "kt" => {
            #[cfg(feature = "lang-kotlin")]
            {
                Ok(Box::new(fp_kotlin::KotlinBackend::new(config, root_name)))
            }
            #[cfg(not(feature = "lang-kotlin"))]
            {
                Err(disabled_feature_error("lang-kotlin", "Kotlin package emission"))
            }
        }
        "python" | "py" => {
            #[cfg(feature = "lang-python")]
            {
                Ok(Box::new(fp_python::PythonBackend::new(config)))
            }
            #[cfg(not(feature = "lang-python"))]
            {
                Err(disabled_feature_error(
                    "lang-python",
                    "Python package emission",
                ))
            }
        }
        "go" | "golang" => {
            #[cfg(feature = "lang-golang")]
            {
                Ok(Box::new(fp_golang::GoBackend::new(config)))
            }
            #[cfg(not(feature = "lang-golang"))]
            {
                Err(disabled_feature_error("lang-golang", "Go package emission"))
            }
        }
        "gdscript" | "gd" => {
            #[cfg(feature = "lang-godot")]
            {
                Ok(Box::new(fp_godot::GdscriptBackend::new(config)))
            }
            #[cfg(not(feature = "lang-godot"))]
            {
                Err(disabled_feature_error(
                    "lang-godot",
                    "GDScript package emission",
                ))
            }
        }
        "zig" => {
            #[cfg(feature = "lang-zig")]
            {
                Ok(Box::new(fp_zig::ZigBackend::new(config)))
            }
            #[cfg(not(feature = "lang-zig"))]
            {
                Err(disabled_feature_error("lang-zig", "Zig package emission"))
            }
        }
        "sycl" => {
            #[cfg(feature = "lang-sycl")]
            {
                Ok(Box::new(fp_sycl::SyclBackend::new(config)))
            }
            #[cfg(not(feature = "lang-sycl"))]
            {
                Err(disabled_feature_error("lang-sycl", "SYCL package emission"))
            }
        }
        "rust" | "rs" => Ok(Box::new(fp_lang::RustBackend::new(config))),
        "wit" => {
            #[cfg(feature = "lang-wit")]
            {
                Ok(Box::new(fp_wit::WitBackend::new(config)))
            }
            #[cfg(not(feature = "lang-wit"))]
            {
                Err(disabled_feature_error("lang-wit", "WIT package emission"))
            }
        }
        "c" => Ok(Box::new(fp_c::codegen::CBackend::new(config))),
        _ => return None,
    })
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
    if is_known_builtin_target(target)
        || crate::languages::registry::find_registered_target_backend(target).is_some()
    {
        Ok(())
    } else {
        Err(CliError::InvalidInput(format!("Unsupported target: {target}")))
    }
}

/// Whether `name` is one of `backend_for_target`'s recognized target names
/// (regardless of whether its crate is compiled into this build).
fn is_known_builtin_target(name: &str) -> bool {
    matches!(
        name.to_lowercase().as_str(),
        "native" | "goasm" | "urcl" | "llvm-binary" | "llvm-text" | "cranelift"
            | "ebpf" | "cil" | "dotnet"
            | "bytecode" | "text-bytecode" | "jvm-bytecode"
            | "wasm" | "interpret"
            | "fp" | "ferro" | "ferrophase"
            | "typescript" | "ts"
            | "javascript" | "js"
            | "csharp" | "cs" | "c#"
            | "kotlin" | "kt"
            | "python" | "py"
            | "go" | "golang"
            | "gdscript" | "gd"
            | "zig"
            | "sycl"
            | "rust" | "rs"
            | "wit"
            | "c"
    )
}

pub(crate) async fn exec_compiled_binary(path: &Path) -> Result<()> {
    let extension_allows_exec = path
        .extension()
        .map_or(false, |ext| ext == "out" || ext == "exe")
        || (cfg!(unix) && path.extension().is_none());

    let header_allows_exec = if extension_allows_exec {
        true
    } else {
        // Native transpilation supports emitting executables with arbitrary suffixes
        // (e.g. `ls.aarch64`). Use header sniffing so `--exec` does not depend on
        // naming conventions.
        match tokio::fs::read(path).await {
            Ok(bytes) => match object::File::parse(bytes.as_slice()) {
                Ok(file) => file.kind() == object::ObjectKind::Executable,
                Err(_) => false,
            },
            Err(_) => false,
        }
    };

    if !header_allows_exec {
        return Err(CliError::Compilation(format!(
            "Refusing to execute '{}': unsupported binary extension",
            path.display()
        )));
    }

    info!("🚀 Executing compiled binary: {}", path.display());

    let output = Command::new(path).output().await.map_err(|e| {
        CliError::Compilation(format!("Failed to execute '{}': {}", path.display(), e))
    })?;

    if !output.stdout.is_empty() {
        print!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }

    if !output.status.success() {
        let code = output.status.code().unwrap_or(-1);
        if std::env::var("FP_ALLOW_EXEC_FAILURE").as_deref() == Ok("1") {
            warn!("Process exited with status {}", code);
        } else {
            return Err(CliError::Compilation(format!(
                "Process exited with status {}",
                code
            )));
        }
    }

    Ok(())
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
