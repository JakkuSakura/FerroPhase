//! Compilation command implementation

use crate::commands::{setup_progress_bar, validate_paths_exist};
use crate::pipeline::{
    AstPreparationOptions, BackendKind, DebugOptions, LossyOptions, PipelineOptions, RuntimeConfig,
};
use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
use console::style;
use fp_core::ast::{AstTarget, AstTargetOutput, Node};
use fp_core::config;
use fp_csharp::CSharpSerializer;
use fp_godot::GdscriptSerializer;
use fp_golang::GoSerializer;
use fp_lang::PrettyAstSerializer;
use fp_native::asm::{aarch64::AsmAarch64Program, x86_64::AsmX86_64Program};
use fp_native::asmir::{lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64};
use fp_native::emit::{self, TargetArch};
use fp_python::PythonSerializer;
use fp_sycl::SyclSerializer;
use fp_typescript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_wit::{WitOptions, WitSerializer, WorldMode};
use fp_zig::ZigSerializer;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use tokio::{fs as async_fs, process::Command};
use tracing::{info, warn};

use clap::{ArgAction, Args, ValueEnum};

/// Arguments for the compile command (also used by Clap)
#[derive(Debug, Clone, Args)]
pub struct CompileArgs {
    /// Input file(s) to compile
    #[arg(required = true)]
    pub input: Vec<PathBuf>,

    /// Output backend (binary, ebpf, cil, dotnet, rust, llvm, wasm, bytecode, text-bytecode, jvm-bytecode, interpret)
    #[arg(short = 'b', long = "backend", default_value = "binary")]
    pub backend: BackendKind,

    /// Explicit output target (typescript, javascript, python, go, gdscript, zig, sycl, rust, wit)
    #[arg(short = 't', long = "target")]
    pub target: Option<String>,

    /// Codegen emitter engine (e.g. "llvm", "native", "cranelift").
    ///
    /// This is only used for native codegen targets (like `--backend binary`).
    /// Default is `native`.
    #[arg(long = "emitter", default_value = "native")]
    pub emitter: EmitterKind,

    /// Target triple for codegen (defaults to host if omitted)
    #[arg(long = "target-triple")]
    pub target_triple: Option<String>,

    /// Target CPU for codegen (optional)
    #[arg(long = "target-cpu")]
    pub target_cpu: Option<String>,

    /// Native target ISA/dialect override (for `--emitter native`).
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

    /// Path to a precomputed package graph (JSON) for dependency resolution
    #[arg(long = "package-graph")]
    pub package_graph: Option<PathBuf>,

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

    /// Persist intermediate representations to disk
    #[arg(long)]
    pub save_intermediates: bool,

    /// Enable lossy mode during compilation
    #[arg(long)]
    pub lossy: bool,

    /// Maximum number of errors to collect when lossy mode is enabled (0 = unlimited)
    #[arg(long, default_value_t = 50)]
    pub max_errors: usize,

    /// Override automatic source language detection (e.g. "typescript")
    #[arg(long = "lang", alias = "language")]
    pub source_language: Option<String>,

    /// Disable pipeline stages by name (repeatable).
    #[arg(long = "disable-stage", action = ArgAction::Append)]
    pub disable_stage: Vec<String>,

    /// Perform const evaluation before AST target emission.
    #[arg(long, default_value_t = true)]
    pub const_eval: bool,

    /// Generate type definitions for TypeScript target.
    #[arg(long)]
    pub type_defs: bool,

    /// Generate a single WIT world instead of per-package worlds.
    #[arg(long)]
    pub single_world: bool,
}

async fn maybe_transpile_urcl(input: &Path, output: &Path, args: &CompileArgs) -> Result<Option<PathBuf>> {
    if detect_container_transpile_source(args.source_language.as_deref(), input)
        != Some(ContainerTranspileSource::Urcl)
    {
        return Ok(None);
    }

    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "URCL input currently supports only `--backend binary`".to_string(),
        ));
    }
    if args.exec {
        return Err(CliError::InvalidInput(
            "`--exec` is not supported for URCL transpilation".to_string(),
        ));
    }

    let text = async_fs::read_to_string(input)
        .await
        .map_err(|err| CliError::Io(io::Error::other(format!("Failed to read URCL input: {err}"))))?;
    let lir_program = fp_urcl::parse_program(&text)
        .map_err(|err| CliError::Compilation(format!("Failed to parse URCL: {err}")))?;

    let output_path = if args.output.is_none() {
        match args.emitter {
            EmitterKind::Goasm => input.with_extension("s"),
            EmitterKind::Urcl => input.with_extension("urcl"),
            _ => input.with_extension("o"),
        }
    } else {
        output.to_path_buf()
    };

    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    match args.emitter {
        EmitterKind::Urcl => {
            let emitter = fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(&output_path));
            emitter
                .emit(lir_program, Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit URCL: {err}")))?;
        }
        EmitterKind::Goasm => {
            let config = fp_goasm::config::GoAsmConfig::new(&output_path)
                .with_target_triple(args.target_triple.clone());
            let emitter = fp_goasm::GoAsmEmitter::new(config);
            emitter
                .emit(lir_program, Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit Go asm: {err}")))?;
        }
        EmitterKind::Native => {
            let (format, arch) = emit::detect_target(args.target_triple.as_deref())
                .map_err(|err| CliError::Compilation(err.to_string()))?;
            let plan = fp_native::emit::emit_plan(&lir_program, format, arch)
                .map_err(|err| CliError::Compilation(format!("Failed to emit native object: {err}")))?;
            fp_native::emit::write_object(&output_path, &plan)
                .map_err(|err| CliError::Compilation(format!("Failed to write object output: {err}")))?;
        }
        other => {
            return Err(CliError::InvalidInput(format!(
                "URCL input does not support `--emitter {}` yet",
                other.as_str()
            )));
        }
    }

    Ok(Some(output_path))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContainerTranspileSource {
    GoAsm,
    Urcl,
    JvmBytecode,
    Cil,
}

impl ContainerTranspileSource {
    fn label(self) -> &'static str {
        match self {
            ContainerTranspileSource::GoAsm => "goasm",
            ContainerTranspileSource::Urcl => "urcl",
            ContainerTranspileSource::JvmBytecode => "jvm-bytecode",
            ContainerTranspileSource::Cil => "cil",
        }
    }
}

fn detect_container_transpile_source(
    source_language: Option<&str>,
    input: &Path,
) -> Option<ContainerTranspileSource> {
    if let Some(lang) = source_language.map(|lang| lang.trim().to_ascii_lowercase()) {
        match lang.as_str() {
            "goasm" | "go-asm" => return Some(ContainerTranspileSource::GoAsm),
            "urcl" => return Some(ContainerTranspileSource::Urcl),
            "jvm" | "jvm-bytecode" | "bytecode-jvm" | "class" => {
                return Some(ContainerTranspileSource::JvmBytecode);
            }
            "cil" | "msil" | "dotnet-cil" => return Some(ContainerTranspileSource::Cil),
            _ => {}
        }
    }

    match input
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .as_deref()
    {
        Some("goasm") => Some(ContainerTranspileSource::GoAsm),
        Some("urcl") => Some(ContainerTranspileSource::Urcl),
        Some("class") => Some(ContainerTranspileSource::JvmBytecode),
        Some("dll" | "exe") => Some(ContainerTranspileSource::Cil),
        _ => None,
    }
}

fn maybe_transpile_container_placeholder(
    input: &Path,
    args: &CompileArgs,
) -> Result<Option<PathBuf>> {
    let Some(source) = detect_container_transpile_source(args.source_language.as_deref(), input)
    else {
        return Ok(None);
    };

    if source == ContainerTranspileSource::Urcl {
        return Ok(None);
    }

    return Err(CliError::InvalidInput(format!(
        "transpiling from `{}` is not implemented yet; this is a placeholder for the generic container transpiler",
        source.label()
    )));
}

#[derive(Debug, Clone, Copy)]
enum CompileTarget {
    Backend(BackendKind),
    Ast(crate::languages::backend::AstLanguageTarget),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitterKind {
    Native,
    Goasm,
    Urcl,
    Llvm,
    Cranelift,
}

impl EmitterKind {
    pub fn as_str(self) -> &'static str {
        match self {
            EmitterKind::Native => "native",
            EmitterKind::Goasm => "goasm",
            EmitterKind::Urcl => "urcl",
            EmitterKind::Llvm => "llvm",
            EmitterKind::Cranelift => "cranelift",
        }
    }
}

/// Execute the compile command
pub async fn compile_command(args: CompileArgs, config: &CliConfig) -> Result<()> {
    let target = resolve_compile_target(&args)?;
    let target_label = match target {
        CompileTarget::Backend(backend) => backend.as_str().to_string(),
        CompileTarget::Ast(ast_target) => format!("{:?}", ast_target),
    };
    info!("Starting compilation with target: {}", target_label);

    // Validate inputs
    validate_inputs(&args)?;

    compile_once(args, config).await
}

async fn compile_once(args: CompileArgs, config: &CliConfig) -> Result<()> {
    let progress = setup_progress_bar(args.input.len());

    let mut compiled_files = Vec::new();
    let target = resolve_compile_target(&args)?;
    let goasm_text_target = matches!(target, CompileTarget::Backend(BackendKind::Binary))
        && args.emitter == EmitterKind::Goasm;
    let urcl_text_target = matches!(target, CompileTarget::Backend(BackendKind::Binary))
        && args.emitter == EmitterKind::Urcl;

    let is_text_backend = matches!(target, CompileTarget::Backend(BackendKind::TextBytecode));
    let target_backend = match target {
        CompileTarget::Backend(backend) => {
            if is_text_backend {
                BackendKind::Bytecode
            } else {
                backend
            }
        }
        CompileTarget::Ast(_) => BackendKind::Interpret,
    };
    let emit_text_bytecode = is_text_backend;

    let output_is_dir = args
        .output
        .as_ref()
        .is_some_and(|path| args.input.len() > 1 || path.is_dir());

    for (_i, input_file) in args.input.iter().enumerate() {
        progress.set_message(format!("Compiling {}", input_file.display()));

        let urcl_container_input =
            detect_container_transpile_source(args.source_language.as_deref(), input_file)
                == Some(ContainerTranspileSource::Urcl);

        let output_file = determine_output_path(
            input_file,
            args.output.as_ref(),
            target,
            args.emitter,
            args.target_triple.as_deref(),
            detect_native_asm_source(args.source_language.as_deref(), input_file).is_some(),
            detect_native_object_source(args.source_language.as_deref(), input_file),
            urcl_container_input,
            emit_text_bytecode,
            output_is_dir,
            args.exec,
        )?;

        // Compile single file
        if let Some(artifact_path) =
            compile_file(input_file, &output_file, &args, target, config).await?
        {
            compiled_files.push(artifact_path);
        }
        progress.inc(1);
    }

    progress.finish_with_message(format!(
        "{} Compiled {} file(s) successfully",
        style("✓").green(),
        args.input.len()
    ));

    // Execute if requested
    if args.exec {
        if matches!(target, CompileTarget::Ast(_)) {
            return Err(CliError::InvalidInput(
                "--exec is not supported for AST targets".to_string(),
            ));
        }
        match target_backend {
            BackendKind::Binary => match compiled_files.as_slice() {
                [] => {
                    warn!("No compiled binaries available to execute");
                }
                [path] => {
                    if goasm_text_target || urcl_text_target {
                        return Err(CliError::InvalidInput(
                            "--exec is not supported for text assembly emitters; choose a native binary emitter instead"
                                .to_string(),
                        ));
                    }
                    exec_compiled_binary(path).await?;
                }
                _ => {
                    return Err(CliError::Compilation(
                        "--exec currently supports compiling a single binary at a time".to_string(),
                    ));
                }
            },
            BackendKind::Bytecode => match compiled_files.as_slice() {
                [] => {
                    warn!("No compiled bytecode available to execute");
                }
                [path] => {
                    if emit_text_bytecode {
                        warn!("--exec is not supported for text-bytecode output");
                    } else {
                        exec_compiled_bytecode(path)?;
                    }
                }
                _ => {
                    return Err(CliError::Compilation(
                        "--exec currently supports compiling a single bytecode file at a time"
                            .to_string(),
                    ));
                }
            },
            BackendKind::Ebpf => match compiled_files.as_slice() {
                [] => {
                    warn!("No compiled eBPF artifacts available to execute");
                }
                [path] => {
                    exec_ebpf_artifact(path).await?;
                }
                _ => {
                    return Err(CliError::Compilation(
                        "--exec currently supports compiling a single eBPF artifact at a time"
                            .to_string(),
                    ));
                }
            },
            BackendKind::Cil => {
                warn!("--exec is not supported for CIL artifacts");
            }
            BackendKind::Dotnet => match compiled_files.as_slice() {
                [] => {
                    warn!("No compiled .NET assembly available to execute");
                }
                [path] => {
                    exec_dotnet_assembly(path).await?;
                }
                _ => {
                    return Err(CliError::Compilation(
                        "--exec currently supports compiling a single .NET assembly at a time"
                            .to_string(),
                    ));
                }
            },
            _ => {
                warn!("--exec is only supported for binary or bytecode targets");
            }
        }
    }

    Ok(())
}

// Note: former compile watch loop removed intentionally.

async fn compile_file(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target: CompileTarget,
    _config: &CliConfig,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());

    if let Some(artifact) = maybe_transpile_native_object(input, output, args).await? {
        return Ok(Some(artifact));
    }

    if let Some(artifact) = maybe_transpile_native_asm(input, output, args).await? {
        return Ok(Some(artifact));
    }

    if let Some(artifact) = maybe_transpile_urcl(input, output, args).await? {
        return Ok(Some(artifact));
    }

    if let Some(artifact) = maybe_transpile_container_placeholder(input, args)? {
        return Ok(Some(artifact));
    }

    if let CompileTarget::Ast(ast_target) = target {
        compile_ast_target(input, output, args, ast_target).await?;
        return Ok(Some(output.to_path_buf()));
    }

    let backend = match target {
        CompileTarget::Backend(backend) => backend,
        CompileTarget::Ast(_) => unreachable!("AST target should return early"),
    };

    // Configure pipeline for compilation with new options
    let target = match backend {
        BackendKind::TextBytecode => BackendKind::Bytecode,
        other => other,
    };

    let execute_const_main = false;

    let mut disabled_stages = args.disable_stage.clone();
    if !disabled_stages
        .iter()
        .any(|stage| stage == "ast→typed(post-closure)")
    {
        disabled_stages.push("ast→typed(post-closure)".to_string());
    }

    let lossy_mode = config::lossy_mode();
    let pipeline_options = PipelineOptions {
        target,
        backend: Some(args.emitter.as_str().to_string()),
        target_triple: args.target_triple.clone(),
        target_cpu: args.target_cpu.clone(),
        native_target: args.native_target.clone(),
        target_features: args.target_features.clone(),
        target_sysroot: args.target_sysroot.clone(),
        linker: Some(args.linker.clone()),
        target_linker: args.target_linker.clone(),
        runtime: RuntimeConfig {
            runtime_type: "literal".to_string(),
            options: std::collections::HashMap::new(),
        },
        source_language: args.source_language.clone(),
        optimization_level: args.opt_level,
        save_intermediates: args.save_intermediates,
        base_path: Some(output.to_path_buf()),
        debug: DebugOptions {
            print_ast: false,
            print_passes: false,
            verbose: args.debug,
        },
        lossy: LossyOptions {
            enabled: args.lossy || lossy_mode,
            max_errors: if args.max_errors == 0 {
                50
            } else {
                args.max_errors
            }, // Default cap
            show_all_errors: true,
            continue_on_error: true,
        },
        release: args.release,
        execute_main: execute_const_main,
        disabled_stages,
    };

    // Execute pipeline with new options
    let mut pipeline = Pipeline::new();
    let pipeline_output = pipeline
        .execute_with_options(PipelineInput::File(input.to_path_buf()), pipeline_options)
        .await?;

    if execute_const_main {
        if let Some(stdout_chunks) = pipeline.take_last_const_eval_stdout() {
            for chunk in stdout_chunks {
                print!("{}", chunk);
            }
            let _ = io::stdout().flush();
        }
    }

    // Write output to file.
    let artifact = match pipeline_output {
        PipelineOutput::Code(code) => {
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent).map_err(|e| CliError::Io(e))?;
            }

            std::fs::write(output, &code).map_err(|e| CliError::Io(e))?;

            info!("Generated code: {}", output.display());
            Some(output.to_path_buf())
        }
        PipelineOutput::Binary(path) => {
            let binary_path = path;
            if binary_path != *output {
                if let Some(parent) = output.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| CliError::Io(e))?;
                }
                async_fs::copy(&binary_path, output)
                    .await
                    .map_err(|e| CliError::Io(e))?;
                if !args.save_intermediates {
                    let _ = async_fs::remove_file(&binary_path).await;
                }
            }
            info!("Generated binary: {}", output.display());
            Some(output.to_path_buf())
        }
        PipelineOutput::Value(_) => {
            // For interpret target or binary target (already compiled), we don't write to file
            info!("Operation completed");
            None
        }
        PipelineOutput::RuntimeValue(_) => {
            // For runtime interpretation, we don't write to file
            info!("Runtime interpretation completed");
            None
        }
    };

    Ok(artifact)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeAsmSource {
    Auto,
    X86_64,
    Aarch64,
}

fn detect_native_object_source(source_language: Option<&str>, input: &Path) -> bool {
    if let Some(lang) = source_language.map(|lang| lang.trim().to_ascii_lowercase()) {
        if matches!(
            lang.as_str(),
            "object" | "native-object" | "obj" | "native-obj" | "o"
        ) {
            return true;
        }
    }

    matches!(
        input
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_ascii_lowercase())
            .as_deref(),
        Some("o" | "obj")
    )
}

async fn maybe_transpile_native_object(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
) -> Result<Option<PathBuf>> {
    if !detect_native_object_source(args.source_language.as_deref(), input) {
        return Ok(None);
    }

    if args.emitter != EmitterKind::Native {
        return Err(CliError::InvalidInput(
            "native object input currently requires `--emitter native`".to_string(),
        ));
    }
    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "native object input currently only supports `--backend binary` transpilation"
                .to_string(),
        ));
    }
    if args.exec {
        return Err(CliError::InvalidInput(
            "`--exec` is not supported for native object transpilation".to_string(),
        ));
    }

    let bytes = async_fs::read(input).await.map_err(|err| {
        CliError::Io(io::Error::other(format!(
            "Failed to read object input: {err}"
        )))
    })?;
    let asmir = fp_native::binary::lift_object_to_asmir(&bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to lift object file: {err}")))?;

    let (format, arch) = emit::detect_target(args.target_triple.as_deref())
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    let plan = fp_native::emit::emit_plan_from_asmir(asmir, format, arch)
        .map_err(|err| CliError::Compilation(format!("Failed to emit target object: {err}")))?;

    let output_path = if args.output.is_none() {
        input.with_extension("o")
    } else {
        output.to_path_buf()
    };

    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    fp_native::emit::write_object(&output_path, &plan)
        .map_err(|err| CliError::Compilation(format!("Failed to write object output: {err}")))?;
    Ok(Some(output_path))
}

async fn maybe_transpile_native_asm(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
) -> Result<Option<PathBuf>> {
    let Some(source_kind) = detect_native_asm_source(args.source_language.as_deref(), input) else {
        return Ok(None);
    };

    if args.emitter != EmitterKind::Native {
        return Err(CliError::InvalidInput(
            "native asm input currently requires `--emitter native`".to_string(),
        ));
    }
    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "native asm input currently only supports `--backend binary` transpilation".to_string(),
        ));
    }
    if args.exec {
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
                let mut target_program = lift_from_x86_64(&program);
                target_program.target.architecture = fp_core::asmir::AsmArchitecture::Aarch64;
                lower_to_aarch64(&target_program).to_text()
            }
        }
        ParsedNativeAsm::Aarch64(program) => {
            if matches!(target_arch, TargetArch::Aarch64) {
                program.to_text()
            } else {
                let mut target_program = lift_from_aarch64(&program);
                target_program.target.architecture = fp_core::asmir::AsmArchitecture::X86_64;
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

fn detect_native_asm_source(
    source_language: Option<&str>,
    input: &Path,
) -> Option<NativeAsmSource> {
    match source_language.map(|lang| lang.trim().to_ascii_lowercase()) {
        Some(lang)
            if matches!(
                lang.as_str(),
                "x86_64-asm" | "asm-x86_64" | "x86asm" | "x86_64asm"
            ) =>
        {
            return Some(NativeAsmSource::X86_64);
        }
        Some(lang)
            if matches!(
                lang.as_str(),
                "aarch64-asm" | "asm-aarch64" | "arm64-asm" | "aarch64asm"
            ) =>
        {
            return Some(NativeAsmSource::Aarch64);
        }
        Some(lang) if matches!(lang.as_str(), "asm" | "native-asm") => {
            return Some(NativeAsmSource::Auto);
        }
        Some(_) => return None,
        None => {}
    }

    match input
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
    {
        Some(ext) if ext == "s" || ext == "asm" => Some(NativeAsmSource::Auto),
        _ => None,
    }
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

async fn compile_ast_target(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target: crate::languages::backend::AstLanguageTarget,
) -> Result<()> {
    if is_package_manifest(input) || is_tsconfig(input) {
        return Err(CliError::Compilation(
            "fp compile --target only accepts source files; use magnet for package manifests"
                .to_string(),
        ));
    }

    let mut pipeline = Pipeline::new();
    use crate::languages::frontend::{LanguageSource, detect_language_source_by_path};
    let detected = detect_language_source_by_path(input);
    let is_wit_input = matches!(detected, Some(LanguageSource::Wit));
    let is_typescript_input = matches!(
        detected,
        Some(LanguageSource::TypeScript | LanguageSource::JavaScript)
    );

    let source = std::fs::read_to_string(input).map_err(CliError::Io)?;
    let mut ast = pipeline.parse_source_public(&source, Some(input))?;

    let prep_options = AstPreparationOptions {
        run_const_eval: args.const_eval,
        save_intermediates: false,
        base_path: None,
    };
    if !is_wit_input && !is_typescript_input {
        pipeline.prepare_for_ast_target(&mut ast, &prep_options)?;
    }

    let result = emit_ast_target(&ast, target, args.type_defs, input, args.single_world)?;

    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    std::fs::write(output, &result.code).map_err(CliError::Io)?;

    for side_file in result.side_files {
        let mut side_path = output.to_path_buf();
        let file_stem = side_path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| CliError::InvalidInput("Invalid output file name".to_string()))?;
        side_path.set_file_name(format!("{}.{}", file_stem, side_file.extension));
        std::fs::write(side_path, side_file.contents).map_err(CliError::Io)?;
    }

    info!("Generated AST target output: {}", output.display());
    Ok(())
}

fn emit_ast_target(
    node: &Node,
    target: crate::languages::backend::AstLanguageTarget,
    emit_type_defs: bool,
    input: &Path,
    single_world: bool,
) -> Result<AstTargetOutput> {
    match target {
        crate::languages::backend::AstLanguageTarget::TypeScript => {
            let serializer = TypeScriptSerializer::new(emit_type_defs);
            let mut result = serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))?;
            if let Some(defs) = serializer.take_type_defs() {
                result.side_files.push(fp_core::ast::AstTargetSideFile {
                    extension: "d.ts".to_string(),
                    contents: defs,
                });
            }
            Ok(result)
        }
        crate::languages::backend::AstLanguageTarget::JavaScript => {
            let serializer = JavaScriptSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::CSharp => {
            let serializer = CSharpSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Python => {
            let serializer = PythonSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Go => {
            let serializer = GoSerializer::default();
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Gdscript => {
            let serializer = GdscriptSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Zig => {
            let serializer = ZigSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Sycl => {
            let serializer = SyclSerializer;
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Rust => {
            let serializer = PrettyAstSerializer::new();
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
        crate::languages::backend::AstLanguageTarget::Wit => {
            let serializer = WitSerializer::with_options(build_wit_options(input, single_world));
            serializer
                .emit_node(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))
        }
    }
}

fn build_wit_options(input: &Path, single_world: bool) -> WitOptions {
    let namespace = input
        .parent()
        .and_then(|dir| dir.file_name())
        .and_then(|os| os.to_str())
        .map(sanitize_wit_component)
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "ferrophase".to_string());

    let interface = input
        .file_stem()
        .and_then(|stem| stem.to_str())
        .map(sanitize_wit_component)
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "module".to_string());

    let mut options = WitOptions::default();
    options.package = format!("{namespace}:{interface}");
    options.root_interface = interface.clone();
    if single_world {
        options.world_mode = WorldMode::Single {
            world_name: interface,
        };
    }
    options
}

fn sanitize_wit_component(raw: &str) -> String {
    let mut result = String::new();
    for ch in raw.chars() {
        match ch {
            'a'..='z' | '0'..='9' => result.push(ch),
            'A'..='Z' => result.push(ch.to_ascii_lowercase()),
            '_' | '-' => result.push('_'),
            '/' | ':' | '.' | '@' => result.push('_'),
            _ => {}
        }
    }
    if result.is_empty() {
        result.push_str("module");
    }
    if result
        .chars()
        .next()
        .map(|ch| ch.is_ascii_digit())
        .unwrap_or(false)
    {
        result.insert(0, '_');
    }
    result
}

fn is_package_manifest(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| {
            let lower = name.to_ascii_lowercase();
            matches!(
                lower.as_str(),
                "cargo.toml" | "package.json" | "magnet.toml"
            )
        })
        .unwrap_or(false)
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

fn resolve_compile_target(args: &CompileArgs) -> Result<CompileTarget> {
    if let Some(target) = args.target.as_deref() {
        let ast_target = crate::languages::backend::parse_ast_target(target)?;
        return Ok(CompileTarget::Ast(ast_target));
    }
    Ok(CompileTarget::Backend(args.backend))
}

async fn exec_compiled_binary(path: &Path) -> Result<()> {
    let is_executable = path
        .extension()
        .map_or(false, |ext| ext == "out" || ext == "exe")
        || (cfg!(unix) && path.extension().is_none());

    if !is_executable {
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

async fn exec_ebpf_artifact(path: &Path) -> Result<()> {
    let is_object = path.extension().map_or(false, |ext| ext == "o");
    if !is_object {
        return Err(CliError::Compilation(format!(
            "Refusing to execute '{}': eBPF execution requires an ELF object (.o)",
            path.display()
        )));
    }

    let runtime = std::env::var("FP_EBPF_RUNTIME").map_err(|_| {
        CliError::Compilation(
            "Missing eBPF user-mode runtime: set FP_EBPF_RUNTIME to an external runner executable such as fp-ebpf-runtime"
                .to_string(),
        )
    })?;
    let runtime_args = std::env::var("FP_EBPF_RUNTIME_ARGS").unwrap_or_default();

    info!(
        "🚀 Executing eBPF artifact via external runtime: {} {}",
        runtime,
        path.display()
    );

    let mut command = Command::new(&runtime);
    for arg in split_runtime_args(&runtime_args) {
        command.arg(arg);
    }
    command.arg(path);

    let output = command.output().await.map_err(|e| {
        CliError::Compilation(format!(
            "Failed to execute eBPF runtime '{}' for '{}': {}",
            runtime,
            path.display(),
            e
        ))
    })?;

    if !output.stdout.is_empty() {
        print!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }

    if !output.status.success() {
        let code = output.status.code().unwrap_or(-1);
        return Err(CliError::Compilation(format!(
            "eBPF runtime exited with status {}",
            code
        )));
    }

    Ok(())
}

fn split_runtime_args(raw: &str) -> Vec<String> {
    raw.split_whitespace()
        .map(|part| part.to_string())
        .collect()
}

async fn exec_dotnet_assembly(path: &Path) -> Result<()> {
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .ok_or_else(|| {
            CliError::Compilation(format!(
                "Refusing to execute '{}': unsupported .NET assembly extension",
                path.display()
            ))
        })?;

    let mut command = if cfg!(windows) && extension == "exe" {
        Command::new(path)
    } else if command_available("mono") {
        ensure_command_available("mono", path)?;
        let mut command = Command::new("mono");
        command.arg(path);
        command
    } else if extension == "dll" {
        ensure_command_available("dotnet", path)?;
        let mut command = Command::new("dotnet");
        command.arg(path);
        command
    } else {
        return Err(CliError::Compilation(format!(
            "Refusing to execute '{}': unsupported .NET assembly extension",
            path.display()
        )));
    };

    info!("🚀 Executing .NET assembly: {}", path.display());

    let output = command.output().await.map_err(|e| {
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
                ".NET process exited with status {}",
                code
            )));
        }
    }

    Ok(())
}

fn ensure_command_available(command: &str, path: &Path) -> Result<()> {
    let found = command_available(command);
    if found {
        Ok(())
    } else {
        Err(CliError::Compilation(format!(
            "Cannot execute '{}': required command '{}' is not available on PATH",
            path.display(),
            command
        )))
    }
}

fn command_available(command: &str) -> bool {
    let path_var = std::env::var_os("PATH").unwrap_or_default();
    std::env::split_paths(&path_var)
        .map(|entry| entry.join(command))
        .any(|candidate| candidate.is_file())
}

fn exec_compiled_bytecode(path: &Path) -> Result<()> {
    let bytes = std::fs::read(path).map_err(CliError::Io)?;
    let file = fp_bytecode::decode_file(&bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to decode bytecode: {}", err)))?;
    let vm = fp_stackvm::Vm::new(file.program);
    vm.run_main()
        .map_err(|err| CliError::Compilation(format!("Bytecode execution failed: {}", err)))?;
    Ok(())
}

fn validate_inputs(args: &CompileArgs) -> Result<()> {
    validate_paths_exist(&args.input, true, "compile")?;
    if let Some(graph) = args.package_graph.as_ref() {
        validate_paths_exist(&[graph.clone()], true, "compile")?;
    }

    // Validate optimization level
    if args.opt_level > 3 {
        return Err(CliError::InvalidInput(
            "Optimization level must be 0-3".to_string(),
        ));
    }

    Ok(())
}

fn determine_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: CompileTarget,
    emitter: EmitterKind,
    target_triple: Option<&str>,
    native_asm_input: bool,
    native_object_input: bool,
    urcl_container_input: bool,
    emit_text_bytecode: bool,
    output_is_dir: bool,
    exec_requested: bool,
) -> Result<PathBuf> {
    let backend = match target {
        CompileTarget::Backend(backend) => backend,
        CompileTarget::Ast(ast_target) => {
            let extension = crate::languages::backend::ast_output_extension_for(ast_target);
            if let Some(output) = output {
                if output_is_dir {
                    let stem = input.file_stem().and_then(|s| s.to_str()).ok_or_else(|| {
                        CliError::InvalidInput("Invalid input filename".to_string())
                    })?;
                    let mut path = output.join(stem);
                    path.set_extension(extension);
                    return Ok(path);
                }
                return Ok(output.clone());
            }
            return Ok(input.with_extension(extension));
        }
    };

    let goasm_text_target = matches!(backend, BackendKind::Binary) && emitter == EmitterKind::Goasm;
    let urcl_text_target = matches!(backend, BackendKind::Binary) && emitter == EmitterKind::Urcl;
    let native_asm_text_target = matches!(backend, BackendKind::Binary)
        && emitter == EmitterKind::Native
        && native_asm_input;
    let native_object_target = matches!(backend, BackendKind::Binary)
        && emitter == EmitterKind::Native
        && native_object_input;
    let urcl_object_target = matches!(backend, BackendKind::Binary)
        && emitter == EmitterKind::Native
        && urcl_container_input;

    if let Some(output) = output {
        if output_is_dir {
            let extension = match backend {
                BackendKind::Binary => {
                    if goasm_text_target {
                        "s"
                    } else if urcl_text_target {
                        "urcl"
                    } else if native_asm_text_target {
                        "s"
                    } else if native_object_target {
                        "o"
                    } else if urcl_object_target {
                        "o"
                    } else if is_windows_target(target_triple) {
                        "exe"
                    } else {
                        "out"
                    }
                }
                BackendKind::Ebpf => {
                    if exec_requested {
                        "o"
                    } else {
                        "ebpf"
                    }
                }
                BackendKind::Cil => "il",
                BackendKind::Dotnet => "exe",
                BackendKind::Rust => "rs",
                BackendKind::Llvm => "ll",
                BackendKind::Wasm => "wasm",
                BackendKind::Bytecode | BackendKind::TextBytecode => {
                    if emit_text_bytecode {
                        "ftbc"
                    } else {
                        "fbc"
                    }
                }
                BackendKind::JvmBytecode => "class",
                BackendKind::Interpret => "out",
            };
            let stem = input
                .file_stem()
                .and_then(|s| s.to_str())
                .ok_or_else(|| CliError::InvalidInput("Invalid input filename".to_string()))?;
            let mut path = output.join(stem);
            path.set_extension(extension);
            return Ok(path);
        }

        if matches!(backend, BackendKind::Binary)
            && !goasm_text_target
            && !urcl_text_target
            && !native_asm_text_target
            && !native_object_target
            && !urcl_object_target
        {
            let mut path = output.clone();
            let desired_ext = if is_windows_target(target_triple) {
                "exe"
            } else {
                "out"
            };

            let needs_update = path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext != desired_ext)
                .unwrap_or(true);

            if needs_update {
                path.set_extension(desired_ext);
            }

            return Ok(path);
        }

        if native_asm_text_target {
            return Ok(output.clone());
        }

        if urcl_object_target {
            return Ok(output.clone());
        }

        if matches!(backend, BackendKind::Bytecode) && emit_text_bytecode {
            let mut path = output.clone();
            path.set_extension("ftbc");
            return Ok(path);
        }

        if matches!(backend, BackendKind::Dotnet) {
            let mut path = output.clone();
            let desired_ext = match path.extension().and_then(|ext| ext.to_str()) {
                Some(ext) if ext.eq_ignore_ascii_case("dll") => "dll",
                Some(ext) if ext.eq_ignore_ascii_case("exe") => "exe",
                _ => "exe",
            };
            let needs_update = path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| !ext.eq_ignore_ascii_case(desired_ext))
                .unwrap_or(true);
            if needs_update {
                path.set_extension(desired_ext);
            }
            return Ok(path);
        }

        Ok(output.clone())
    } else {
        let extension = match backend {
            BackendKind::Binary => {
                if goasm_text_target {
                    "s"
                } else if urcl_text_target {
                    "urcl"
                } else if native_asm_text_target {
                    "s"
                } else if urcl_object_target {
                    "o"
                } else if is_windows_target(target_triple) {
                    "exe"
                } else {
                    "out" // Use .out extension on Unix systems for clarity
                }
            }
            BackendKind::Ebpf => {
                if exec_requested {
                    "o"
                } else {
                    "ebpf"
                }
            }
            BackendKind::Cil => "il",
            BackendKind::Dotnet => "exe",
            BackendKind::Rust => "rs",
            BackendKind::Llvm => "ll",
            BackendKind::JvmBytecode => "class",
            BackendKind::Wasm => "wasm",
            BackendKind::Bytecode | BackendKind::TextBytecode => {
                if emit_text_bytecode {
                    "ftbc"
                } else {
                    "fbc"
                }
            }
            BackendKind::Interpret => {
                return Err(CliError::InvalidInput(format!(
                    "Unknown backend for output extension: {}",
                    backend.as_str()
                )));
            }
        };

        Ok(input.with_extension(extension))
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
