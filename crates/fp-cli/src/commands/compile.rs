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
use fp_golang::GoSerializer;
use fp_lang::PrettyAstSerializer;
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

    /// Output backend (binary, rust, llvm, wasm, bytecode, text-bytecode, interpret)
    #[arg(short = 'b', long = "backend", default_value = "binary")]
    pub backend: BackendKind,

    /// Explicit output target (typescript, javascript, python, go, zig, sycl, rust, wit)
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

#[derive(Debug, Clone, Copy)]
enum CompileTarget {
    Backend(BackendKind),
    Ast(crate::languages::backend::AstLanguageTarget),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitterKind {
    Native,
    Llvm,
    Cranelift,
}

impl EmitterKind {
    pub fn as_str(self) -> &'static str {
        match self {
            EmitterKind::Native => "native",
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

        let output_file = determine_output_path(
            input_file,
            args.output.as_ref(),
            target,
            args.target_triple.as_deref(),
            emit_text_bytecode,
            output_is_dir,
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
    target_triple: Option<&str>,
    emit_text_bytecode: bool,
    output_is_dir: bool,
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

    if let Some(output) = output {
        if output_is_dir {
            let extension = match backend {
                BackendKind::Binary => {
                    if is_windows_target(target_triple) {
                        "exe"
                    } else {
                        "out"
                    }
                }
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

        if matches!(backend, BackendKind::Binary) {
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

        if matches!(backend, BackendKind::Bytecode) && emit_text_bytecode {
            let mut path = output.clone();
            path.set_extension("ftbc");
            return Ok(path);
        }

        Ok(output.clone())
    } else {
        let extension = match backend {
            BackendKind::Binary => {
                // Use platform-specific executable extension
                if is_windows_target(target_triple) {
                    "exe"
                } else {
                    "out" // Use .out extension on Unix systems for clarity
                }
            }
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
