//! Compilation command implementation

use crate::commands::{setup_progress_bar, validate_paths_exist};
use crate::compile_options::BackendKind;
use crate::container::NativeAsmSource;
use crate::compiler::{
    self, BytecodeCompileOptions, CraneliftCompileOptions, EbpfCompileOptions, JvmCompileOptions,
    LlvmCompileOptions, NativeCompileOptions, NativeEmitterKind, WasmCompileOptions,
};
use crate::{CliError, Result, cli::CliConfig};
use console::style;
use fp_core::ast::{AstTargetOutput, File, Item};
use fp_core::package::{PackageId, PackageSource};
#[cfg(feature = "lang-csharp")]
use fp_csharp::CSharpSerializer;
#[cfg(feature = "lang-godot")]
use fp_godot::GdscriptSerializer;
#[cfg(feature = "lang-golang")]
use fp_golang::GoSerializer;
#[cfg(feature = "lang-kotlin")]
use fp_kotlin::KotlinSerializer;
use fp_lang::PrettyAstSerializer;
use fp_native::asm::{aarch64::AsmAarch64Program, x86_64::AsmX86_64Program};
use fp_native::asmir::{lift_from_aarch64, lift_from_x86_64, lower_to_aarch64, lower_to_x86_64};
use fp_native::emit::{self, TargetArch};
#[cfg(feature = "lang-python")]
use fp_python::PythonSerializer;
#[cfg(feature = "lang-sycl")]
use fp_sycl::SyclSerializer;
#[cfg(feature = "lang-typescript")]
use fp_typescript::{JavaScriptSerializer, TypeScriptSerializer};
#[cfg(feature = "lang-wit")]
use fp_wit::WitSerializer;
#[cfg(feature = "lang-zig")]
use fp_zig::ZigSerializer;
use object::Object as _;
use std::io;
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
    /// Package name used to qualify source identities (auto-detected for projects)
    #[arg(long = "package")]
    pub package: Option<String>,

    /// Output backend (binary, ebpf, cil, dotnet, rust, llvm, wasm, bytecode, text-bytecode, jvm-bytecode, interpret)
    #[arg(short = 'b', long = "backend", default_value = "binary")]
    pub backend: BackendKind,

    /// Explicit output target (fp, typescript, javascript, python, go, gdscript, zig, sycl, rust, wit)
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

impl CompileArgs {
    fn package(&self) -> &str {
        self.package.as_deref().unwrap_or("unnamed")
    }
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

#[derive(Clone)]
enum CompileTarget {
    Backend(BackendKind),
    Ast(crate::languages::backend::BuiltinLanguageTarget),
    /// A target registered at runtime via `crate::languages::registry`
    /// (e.g. `skln-fp-graph`'s `fp-graph` binary) — see that module's doc
    /// comment for why this can't just be another `BuiltinLanguageTarget`
    /// variant.
    External(std::sync::Arc<dyn crate::languages::registry::LanguageTarget>),
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
    let target_label = match &target {
        CompileTarget::Backend(backend) => backend.as_str().to_string(),
        CompileTarget::Ast(ast_target) => format!("{:?}", ast_target),
        CompileTarget::External(external) => format!("external:{}", external.name()),
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
        CompileTarget::External(_) => BackendKind::Interpret,
    };
    let emit_text_bytecode = is_text_backend;

    let container_registry = crate::container::ContainerRegistry::new();
    let output_is_dir = args
        .output
        .as_ref()
        .is_some_and(|path| args.input.len() > 1 || path.is_dir());

    for (_i, input_file) in args.input.iter().enumerate() {
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
            target.clone(),
            args.emitter,
            args.target_triple.as_deref(),
            input_class,
            emit_text_bytecode,
            output_is_dir,
            args.link || args.exec,
            args.exec,
        )?;

        // Compile single file
        if let Some(artifact_path) =
            compile_file(input_file, &output_file, &args, target.clone(), config, input_class)
                .await?
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
        if let Some(target_triple) = args.target_triple.as_deref() {
            if !target_triple_matches_host(target_triple) {
                warn!(
                    "Skipping `--exec`: target triple `{}` does not match host",
                    target_triple
                );
                return Ok(());
            }
        }
        if matches!(target, CompileTarget::Ast(_) | CompileTarget::External(_)) {
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
    input_class: crate::container::InputClass,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());

    if input.is_dir() {
        match target {
            CompileTarget::Ast(ast_target) => {
                compile_project(input, output, args, ast_target).await?;
                return Ok(Some(output.to_path_buf()));
            }
            CompileTarget::External(external) => {
                compile_project_external(input, output, args, external).await?;
                return Ok(Some(output.to_path_buf()));
            }
            CompileTarget::Backend(_) => {
                return Err(CliError::InvalidInput(
                    "directory input requires an AST target (--target kotlin, typescript, etc.)"
                        .to_string(),
                ));
            }
        }
    }

    let native_asm_kind = match input_class {
        crate::container::InputClass::NativeAsm(kind) => Some(kind),
        _ => None,
    };
    let container_kind = match input_class {
        crate::container::InputClass::Container(kind) => Some(kind),
        _ => None,
    };

    if let Some(artifact) = maybe_transpile_native_asm(input, output, args, native_asm_kind).await?
    {
        return Ok(Some(artifact));
    }

    if let Some(artifact) =
        crate::container::maybe_transpile_container(input, output, args, _config, container_kind)
            .await?
    {
        return Ok(Some(artifact));
    }

    if let CompileTarget::Ast(ast_target) = target {
        compile_emit_target(input, output, args, ast_target).await?;
        return Ok(Some(output.to_path_buf()));
    }

    // A lone source file is just the trivial one-package case of the same
    // package/workspace discovery an `External` target's directory input
    // goes through above — not a separate code path (see
    // `compile_project_external`'s doc comment).
    if let CompileTarget::External(external) = target {
        compile_project_external(input, output, args, external).await?;
        return Ok(Some(output.to_path_buf()));
    }

    let backend = match target {
        CompileTarget::Backend(backend) => backend,
        CompileTarget::Ast(_) => unreachable!("AST target should return early"),
        CompileTarget::External(_) => unreachable!("External target should return early"),
    };

    if !args.disable_stage.is_empty() {
        warn!(
            "--disable-stage is ignored on the fp-compiler compile path: {}",
            args.disable_stage.join(", ")
        );
    }

    try_compile_with_compiler(input, output, args, backend).await
}

async fn try_compile_with_compiler(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    backend: BackendKind,
) -> Result<Option<PathBuf>> {
    match backend {
        BackendKind::Binary => {
            let emitter = match args.emitter {
                EmitterKind::Native => NativeEmitterKind::Native,
                EmitterKind::Goasm => NativeEmitterKind::GoAsm,
                EmitterKind::Urcl => NativeEmitterKind::Urcl,
                EmitterKind::Llvm => {
                    let artifact = compiler::compile_llvm_file(
                        input,
                        args.package(),
                        args.source_language.as_deref(),
                        &LlvmCompileOptions {
                            output: output.to_path_buf(),
                            target_triple: args.target_triple.clone(),
                            target_cpu: args.target_cpu.clone(),
                            target_features: args.target_features.clone(),
                            target_sysroot: args.target_sysroot.clone(),
                            linker: Some(args.linker.clone()),
                            target_linker: args.target_linker.clone(),
                            release: args.release,
                            debug_info: args.debug,
                            module_name: input
                                .file_stem()
                                .and_then(|stem| stem.to_str())
                                .unwrap_or("main")
                                .to_string(),
                            save_intermediates: args.save_intermediates,
                        },
                    )?;
                    return Ok(Some(artifact));
                }
                EmitterKind::Cranelift => {
                    let artifact = compiler::compile_cranelift_file(
                        input,
                        args.package(),
                        args.source_language.as_deref(),
                        &CraneliftCompileOptions {
                            output: output.to_path_buf(),
                            target_triple: args.target_triple.clone(),
                            target_cpu: args.target_cpu.clone(),
                            target_features: args.target_features.clone(),
                            target_sysroot: args.target_sysroot.clone(),
                            linker: Some(args.linker.clone()),
                            target_linker: args.target_linker.clone(),
                            release: args.release,
                            save_intermediates: args.save_intermediates,
                        },
                    )?;
                    return Ok(Some(artifact));
                }
            };
            let artifact = compiler::compile_native_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &NativeCompileOptions {
                    emitter,
                    output: output.to_path_buf(),
                    target_triple: args.target_triple.clone(),
                    target_cpu: args.target_cpu.clone(),
                    native_target: args.native_target.clone(),
                    target_features: args.target_features.clone(),
                    target_sysroot: args.target_sysroot.clone(),
                    linker: Some(args.linker.clone()),
                    target_linker: args.target_linker.clone(),
                    release: args.release,
                    save_intermediates: args.save_intermediates,
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Bytecode | BackendKind::TextBytecode => {
            let artifact = compiler::compile_bytecode_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &BytecodeCompileOptions {
                    output: output.to_path_buf(),
                    emit_text: matches!(backend, BackendKind::TextBytecode),
                    save_intermediates: args.save_intermediates,
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::JvmBytecode => {
            let class_name_hint = input
                .file_stem()
                .and_then(|stem| stem.to_str())
                .map(|stem| stem.to_string());
            let artifact = compiler::compile_jvm_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &JvmCompileOptions {
                    output: output.to_path_buf(),
                    save_intermediates: args.save_intermediates,
                    class_name_hint,
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Wasm => {
            let artifact = compiler::compile_wasm_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &WasmCompileOptions {
                    output: output.to_path_buf(),
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Ebpf => {
            let artifact = compiler::compile_ebpf_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &EbpfCompileOptions {
                    output: output.to_path_buf(),
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Cil => {
            let code = compiler::compile_cil_file(input)?;
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent).map_err(CliError::Io)?;
            }
            std::fs::write(output, code).map_err(CliError::Io)?;
            Ok(Some(output.to_path_buf()))
        }
        BackendKind::Dotnet => {
            let artifact = compiler::compile_dotnet_file(
                input,
                args.source_language.as_deref(),
                output,
                args.save_intermediates,
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Llvm => {
            let artifact = compiler::compile_llvm_file(
                input,
                args.package(),
                args.source_language.as_deref(),
                &LlvmCompileOptions {
                    output: output.to_path_buf(),
                    target_triple: args.target_triple.clone(),
                    target_cpu: args.target_cpu.clone(),
                    target_features: args.target_features.clone(),
                    target_sysroot: args.target_sysroot.clone(),
                    linker: Some(args.linker.clone()),
                    target_linker: args.target_linker.clone(),
                    release: args.release,
                    debug_info: args.debug,
                    module_name: input
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .unwrap_or("main")
                        .to_string(),
                    save_intermediates: true,
                },
            )?;
            Ok(Some(artifact))
        }
        BackendKind::Rust => {
            // Reuse the same AST-target Rust transpile path `--target rust`
            // already goes through (`fp_lang::PrettyAstSerializer`) instead
            // of a second Rust-emission implementation.
            compile_emit_target(input, output, args, crate::languages::backend::BuiltinLanguageTarget::Rust)
                .await?;
            Ok(Some(output.to_path_buf()))
        }
        _ => Err(CliError::Compilation(format!(
            "fp-compiler does not support backend {} on this path",
            backend.as_str()
        ))),
    }
}

/// `source_kind` is the classification `compile_once` already computed once
/// for this input via `ContainerRegistry::classify_input` — not re-detected
/// here.
async fn maybe_transpile_native_asm(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    source_kind: Option<crate::container::NativeAsmSource>,
) -> Result<Option<PathBuf>> {
    let Some(source_kind) = source_kind else {
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

async fn compile_emit_target(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target: crate::languages::backend::BuiltinLanguageTarget,
) -> Result<()> {
    if is_tsconfig(input) {
        return Err(CliError::Compilation(
            "fp compile --target requires source files, not tsconfig".to_string(),
        ));
    }

    let language = compiler::resolve_source_language(input, args.source_language.as_deref())?;

    let ast = {
        let (provider, package_id, tag) = provider_and_package_for_input(input, &language)?;
        let materializer =
            crate::languages::materializer::materializer_for_language(
                &crate::languages::backend::output_extension_for(target),
            );
        let normalizer = crate::languages::normalizer::normalizer_for_language(&language);
        let wrapped: std::sync::Arc<dyn fp_core::package::provider::PackageProvider> =
            std::sync::Arc::new(TranspileMaterializingPackageProvider::new(
                provider,
                materializer.clone(),
                normalizer,
            ));
        let capabilities = crate::languages::backend::capabilities_for_target(target);
        let (executor, mut session) =
            compiler::build_workspace_session(wrapped, &language, capabilities);
        executor
            .run(session.driver().compile_package(&package_id))
            .map_err(|e| {
                CliError::Compilation(format!("typecheck failed for {}: {}", input.display(), e))
            })?;
        compiler::drain_driver(session.driver())?;
        let workspace = session.driver().state.borrow().typing_ctx.env_ctx.clone();
        let source = workspace
            .package_source(&package_id)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        // Materialize portable ops post-typechecked-lifting too (see the
        // matching comment in `compile_project`'s phase 2) — the wrapping
        // above only materializes pre-HIR source; `HirToAstLifter`'s
        // `program.op_defs`-based classification happens after that.
        let items: Vec<Item> = source
            .items
            .into_iter()
            .filter(|pkg_item| pkg_item.module_path == tag)
            .map(|pkg_item| match &materializer {
                Some(mat) => fp_core::intrinsics::materialize_item(pkg_item.item, mat.as_ref())
                    .map_err(|e| CliError::Compilation(e.to_string())),
                None => Ok(pkg_item.item),
            })
            .collect::<Result<Vec<Item>>>()?;
        File {
            path: input.to_path_buf(),
            attrs: vec![],
            collected_items: vec![],
            items,
        }
    };

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

async fn compile_project(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target: crate::languages::backend::BuiltinLanguageTarget,
) -> Result<()> {
    use crate::languages::detect_project_language;
    use crate::languages::discovery::provider_for_language;

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
    let lang = lang.as_str();

    let provider = provider_for_language(lang, input)
        .ok_or_else(|| CliError::Compilation(format!("no provider for language: {lang}")))?;

    let packages = provider
        .list_packages()
        .map_err(|e| CliError::Compilation(e.to_string()))?;
    let workspace_packages: std::collections::HashSet<String> =
        packages.iter().map(|p| p.as_str().to_string()).collect();

    info!("Project: {} package(s), language: {}", packages.len(), lang);

    let mut file_count = 0;

    let normalizer = crate::languages::normalizer::normalizer_for_language(lang);
    let materializer = crate::languages::materializer::materializer_for_language(
        &crate::languages::backend::output_extension_for(target)
    );

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
    let capabilities = crate::languages::backend::capabilities_for_target(target);
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

    // Constructed once here (not per package below) — Kotlin's
    // `KotlinWorkspaceContext::collect` walks every item of every package,
    // so per-package construction would be an N× regression on an
    // N-package workspace.
    let sources: Vec<PackageSource> = prepared.iter().map(|(_, src)| src.clone()).collect();
    let backend_config = fp_core::backend::BackendConfig::new(output.to_path_buf());
    let backend = backend_for_target(target, args, backend_config, &sources, workspace_packages.clone(), root_name)?;

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
        // `prepared`'s own `PackageSource.items` (read further up to build
        // `sources` for `backend_for_target`) must stay un-materialized —
        // Kotlin's cross-package mutability scan needs the pre-materialize
        // shape — so this mutates the compiled package's items in place
        // instead of cloning into a throwaway workspace.
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

/// Runs a registry-provided (`CompileTarget::External`) target through the
/// same package/workspace discovery + typecheck pipeline `compile_project`
/// uses for built-in targets (see `crate::languages::registry`'s doc
/// comment for why an externally-registered target can't just be another
/// `crate::languages::backend::BuiltinLanguageTarget` match arm inside
/// `compile_project` itself). Unlike built-in targets — which serialize and
/// write one file per package — an `External` target *collects* facts
/// across every package via `LanguageTarget::visit_package` and produces one
/// accumulated `AstTargetOutput` via `LanguageTarget::finish` at the end, so
/// this writes a single output file (+ side files), the same single-write
/// tail `compile_emit_target` uses for single files — not a directory tree
/// of per-package files like `compile_project`'s built-in-target path.
///
/// A lone source file is handled here too, as the trivial one-package case
/// of the same discovery (`provider_and_package_for_input`/
/// `compiler::resolve_source_package`) rather than a separate code path —
/// see `compile_file`'s dispatch.
async fn compile_project_external(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    target: std::sync::Arc<dyn crate::languages::registry::LanguageTarget>,
) -> Result<()> {
    use crate::languages::detect_project_language;
    use crate::languages::discovery::provider_for_language;
    use crate::languages::registry::{LanguageTargetContext, LanguageTargetPackage};

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

    info!(
        "Project: {} package(s), language: {} (external target: {})",
        packages.len(),
        lang,
        target.name()
    );

    // `materializer_for_language` only recognizes built-in target names
    // (e.g. "kotlin"/"kt" for `output_extension_for(Kotlin) == "kt"`) — an
    // externally-registered target has no such target-language op
    // materializer, so this always resolves to `None` here (unlike
    // `compile_project`'s built-in-target path). Source-language
    // normalization (`normalizer_for_language`) still applies exactly as it
    // does for every other target, keyed off the *source* language, not the
    // target.
    let materializer =
        crate::languages::materializer::materializer_for_language(target.output_extension());
    let normalizer = crate::languages::normalizer::normalizer_for_language(&lang);

    let materializing_provider: std::sync::Arc<dyn fp_core::package::provider::PackageProvider> =
        std::sync::Arc::new(TranspileMaterializingPackageProvider::new(
            provider.clone(),
            materializer,
            normalizer,
        ));

    let ctx = LanguageTargetContext {
        project_root: input.to_path_buf(),
    };

    // Externally-registered targets have no declared `LanguageCapabilities`
    // of their own (the trait in `registry.rs` doesn't expose one) — use the
    // same conservative default `capabilities_for_target` falls back to for
    // every built-in target it doesn't special-case.
    let capabilities = fp_core::capabilities::LanguageCapabilities::NATIVE;
    let root_name = input
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("workspace");
    let root_id = PackageId::new(format!("{root_name}::__workspace_root__"));
    let (executor, mut session) =
        compiler::build_workspace_session(materializing_provider.clone(), &lang, capabilities);
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

    for package_id in &packages {
        let source = workspace
            .package_source(package_id)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
        let pkg = LanguageTargetPackage {
            package_id,
            items: &source.items,
        };
        target
            .visit_package(&pkg, &ctx)
            .map_err(|e| CliError::Compilation(e.to_string()))?;
    }

    let result = target
        .finish()
        .map_err(|e| CliError::Compilation(e.to_string()))?;

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

    info!(
        "Generated external target output ({}): {}",
        target.name(),
        output.display()
    );
    Ok(())
}

/// Error returned by an AST-target arm whose crate is gated behind a
/// disabled optional `lang-*` feature (see e.g. `lang-typescript` in this
/// crate's `Cargo.toml`).
fn disabled_feature_error(feature: &str, what: &str) -> CliError {
    CliError::InvalidInput(format!(
        "{what} requires the \"{feature}\" feature, which is disabled in this build"
    ))
}

/// Constructs the `TargetBackend` for a `compile_project` run — called
/// exactly once per invocation (not once per package: Kotlin's
/// `KotlinWorkspaceContext::collect` walks every item of every package, so
/// per-package construction would be an N× regression on an N-package
/// workspace). Same per-arm `#[cfg(feature = "lang-...")]` gating as the
/// serialization match this replaces.
#[allow(unused_variables, clippy::too_many_arguments)]
fn backend_for_target(
    target: crate::languages::backend::BuiltinLanguageTarget,
    args: &CompileArgs,
    config: fp_core::backend::BackendConfig,
    sources: &[PackageSource],
    workspace_packages: std::collections::HashSet<String>,
    root_name: String,
) -> Result<Box<dyn fp_core::backend::TargetBackend>> {
    match target {
        crate::languages::backend::BuiltinLanguageTarget::FerroPhase => {
            Ok(Box::new(fp_c::FerroPhaseAstBackend::new(config)))
        }
        crate::languages::backend::BuiltinLanguageTarget::TypeScript => {
            #[cfg(feature = "lang-typescript")]
            {
                Ok(Box::new(fp_typescript::TypeScriptBackend::new(
                    config,
                    args.type_defs,
                )))
            }
            #[cfg(not(feature = "lang-typescript"))]
            {
                Err(disabled_feature_error(
                    "lang-typescript",
                    "TypeScript package emission",
                ))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::JavaScript => {
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
        crate::languages::backend::BuiltinLanguageTarget::CSharp => {
            #[cfg(feature = "lang-csharp")]
            {
                Ok(Box::new(fp_csharp::CSharpBackend::new(config)))
            }
            #[cfg(not(feature = "lang-csharp"))]
            {
                Err(disabled_feature_error("lang-csharp", "C# package emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Kotlin => {
            #[cfg(feature = "lang-kotlin")]
            {
                Ok(Box::new(fp_kotlin::KotlinBackend::new(
                    config,
                    sources,
                    workspace_packages,
                    root_name,
                )))
            }
            #[cfg(not(feature = "lang-kotlin"))]
            {
                Err(disabled_feature_error("lang-kotlin", "Kotlin package emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Python => {
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
        crate::languages::backend::BuiltinLanguageTarget::Go => {
            #[cfg(feature = "lang-golang")]
            {
                Ok(Box::new(fp_golang::GoBackend::new(config)))
            }
            #[cfg(not(feature = "lang-golang"))]
            {
                Err(disabled_feature_error("lang-golang", "Go package emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Gdscript => {
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
        crate::languages::backend::BuiltinLanguageTarget::Zig => {
            #[cfg(feature = "lang-zig")]
            {
                Ok(Box::new(fp_zig::ZigBackend::new(config)))
            }
            #[cfg(not(feature = "lang-zig"))]
            {
                Err(disabled_feature_error("lang-zig", "Zig package emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Sycl => {
            #[cfg(feature = "lang-sycl")]
            {
                Ok(Box::new(fp_sycl::SyclBackend::new(config)))
            }
            #[cfg(not(feature = "lang-sycl"))]
            {
                Err(disabled_feature_error("lang-sycl", "SYCL package emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Rust => {
            Ok(Box::new(fp_lang::RustBackend::new(config)))
        }
        crate::languages::backend::BuiltinLanguageTarget::Wit => {
            #[cfg(feature = "lang-wit")]
            {
                Ok(Box::new(fp_wit::WitBackend::new(config, args.single_world)))
            }
            #[cfg(not(feature = "lang-wit"))]
            {
                Err(disabled_feature_error("lang-wit", "WIT package emission"))
            }
        }
    }
}

#[allow(unused_variables)]
fn emit_ast_target(
    node: &File,
    target: crate::languages::backend::BuiltinLanguageTarget,
    emit_type_defs: bool,
    input: &Path,
    single_world: bool,
) -> Result<AstTargetOutput> {
    match target {
        crate::languages::backend::BuiltinLanguageTarget::FerroPhase => {
            let serializer = fp_c::CSerializer;
            let code = serializer
                .serialize_file(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))?;
            Ok(fp_core::ast::AstTargetOutput {
                code,
                side_files: Vec::new(),
            })
        }
        crate::languages::backend::BuiltinLanguageTarget::TypeScript => {
            #[cfg(feature = "lang-typescript")]
            {
                let serializer = TypeScriptSerializer::new(emit_type_defs);
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                let mut result = fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                };
                if let Some(defs) = serializer.take_type_defs() {
                    result.side_files.push(fp_core::ast::AstTargetSideFile {
                        extension: "d.ts".to_string(),
                        contents: defs,
                    });
                }
                Ok(result)
            }
            #[cfg(not(feature = "lang-typescript"))]
            {
                Err(disabled_feature_error(
                    "lang-typescript",
                    "TypeScript/JavaScript AST emission",
                ))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::JavaScript => {
            #[cfg(feature = "lang-typescript")]
            {
                let serializer = JavaScriptSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-typescript"))]
            {
                Err(disabled_feature_error(
                    "lang-typescript",
                    "JavaScript AST emission",
                ))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::CSharp => {
            #[cfg(feature = "lang-csharp")]
            {
                let serializer = CSharpSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-csharp"))]
            {
                Err(disabled_feature_error("lang-csharp", "C# AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Kotlin => {
            #[cfg(feature = "lang-kotlin")]
            {
                let serializer = KotlinSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-kotlin"))]
            {
                Err(disabled_feature_error("lang-kotlin", "Kotlin AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Python => {
            #[cfg(feature = "lang-python")]
            {
                let serializer = PythonSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-python"))]
            {
                Err(disabled_feature_error("lang-python", "Python AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Go => {
            #[cfg(feature = "lang-golang")]
            {
                let serializer = GoSerializer::default();
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-golang"))]
            {
                Err(disabled_feature_error("lang-golang", "Go AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Gdscript => {
            #[cfg(feature = "lang-godot")]
            {
                let serializer = GdscriptSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-godot"))]
            {
                Err(disabled_feature_error(
                    "lang-godot",
                    "GDScript AST emission",
                ))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Zig => {
            #[cfg(feature = "lang-zig")]
            {
                let serializer = ZigSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-zig"))]
            {
                Err(disabled_feature_error("lang-zig", "Zig AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Sycl => {
            #[cfg(feature = "lang-sycl")]
            {
                let serializer = SyclSerializer;
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-sycl"))]
            {
                Err(disabled_feature_error("lang-sycl", "SYCL AST emission"))
            }
        }
        crate::languages::backend::BuiltinLanguageTarget::Rust => {
            let serializer = PrettyAstSerializer::new();
            let code = serializer
                .serialize_file(node)
                .map_err(|e| CliError::TargetEmit(e.to_string()))?;
            Ok(fp_core::ast::AstTargetOutput {
                code,
                side_files: Vec::new(),
            })
        }
        crate::languages::backend::BuiltinLanguageTarget::Wit => {
            #[cfg(feature = "lang-wit")]
            {
                let serializer =
                    WitSerializer::with_options(fp_wit::build_wit_options(input, single_world));
                let code = serializer
                    .serialize_file(node)
                    .map_err(|e| CliError::TargetEmit(e.to_string()))?;
                Ok(fp_core::ast::AstTargetOutput {
                    code,
                    side_files: Vec::new(),
                })
            }
            #[cfg(not(feature = "lang-wit"))]
            {
                Err(disabled_feature_error("lang-wit", "WIT AST emission"))
            }
        }
    }
}

#[allow(dead_code)]
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
        if let Ok(ast_target) = crate::languages::backend::parse_language_target(target) {
            return Ok(CompileTarget::Ast(ast_target));
        }
        if let Some(external) = crate::languages::registry::find_registered_language_target(target)
        {
            return Ok(CompileTarget::External(external));
        }
        // Re-run `parse_language_target` for its real error message (unknown
        // built-in name) now that the registry fallback has also missed —
        // avoids duplicating `CliError::InvalidInput` construction here.
        crate::languages::backend::parse_language_target(target)?;
        unreachable!("parse_language_target either succeeds or returns Err above");
    }
    Ok(CompileTarget::Backend(args.backend))
}

async fn exec_compiled_binary(path: &Path) -> Result<()> {
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

/// The default filename extension for `--backend binary` output, given
/// which raw/foreign artifact `input` classified as (if any). `link_requested`
/// only matters for object/archive container inputs re-emitted as native —
/// deriving a bare `input.<ext>` path (no `--output` given) always uses the
/// unlinked extension regardless of `--link`/`--exec` (pass `false`), while
/// writing under an explicit output *directory* respects it.
fn native_binary_extension(
    input_class: crate::container::InputClass,
    emitter: EmitterKind,
    link_requested: bool,
    target_triple: Option<&str>,
) -> &'static str {
    use crate::container::{ContainerInputKind, InputClass};
    match emitter {
        EmitterKind::Goasm => return "s",
        EmitterKind::Urcl => return "urcl",
        EmitterKind::Native => match input_class {
            InputClass::NativeAsm(_) => return "s",
            InputClass::Container(ContainerInputKind::NativeObject) => {
                return if link_requested { "out" } else { "o" };
            }
            InputClass::Container(ContainerInputKind::NativeArchive) => {
                return if link_requested { "out" } else { "a" };
            }
            InputClass::Container(
                ContainerInputKind::Urcl
                | ContainerInputKind::GoAsm
                | ContainerInputKind::Cil
                | ContainerInputKind::JvmBytecode,
            ) => return "o",
            InputClass::Source => {}
        },
        EmitterKind::Llvm | EmitterKind::Cranelift => {}
    }
    if is_windows_target(target_triple) {
        "exe"
    } else {
        "out"
    }
}

/// True when `--backend binary` should write to (or derive a name from)
/// `output`/`input` as-is, rather than applying the normal linked-binary
/// extension defaulting below — i.e. every raw/foreign-artifact re-emission
/// case `native_binary_extension` above also special-cases.
fn is_raw_binary_passthrough(input_class: crate::container::InputClass, emitter: EmitterKind) -> bool {
    match emitter {
        EmitterKind::Goasm | EmitterKind::Urcl => true,
        EmitterKind::Native => !matches!(input_class, crate::container::InputClass::Source),
        EmitterKind::Llvm | EmitterKind::Cranelift => false,
    }
}

fn determine_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: CompileTarget,
    emitter: EmitterKind,
    target_triple: Option<&str>,
    input_class: crate::container::InputClass,
    emit_text_bytecode: bool,
    output_is_dir: bool,
    native_link_requested: bool,
    exec_requested: bool,
) -> Result<PathBuf> {
    let backend = match target {
        CompileTarget::Backend(backend) => backend,
        CompileTarget::Ast(ast_target) => {
            let extension = crate::languages::backend::output_extension_for(ast_target);
            if let Some(output) = output {
                // A directory input (a whole project/package) always compiles into
                // `output` as a directory root — never derive a single filename+
                // extension from it. `output_is_dir` only reflects whether the
                // *output* path happens to already exist as a directory (e.g. from
                // a prior run), which is unrelated and previously caused a second
                // transpile into an existing output dir to nest everything under a
                // spurious `<input-dir-name>.<ext>` subdirectory instead of
                // overwriting in place.
                if output_is_dir && !input.is_dir() {
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
        CompileTarget::External(external) => {
            let extension = external.output_extension();
            if let Some(output) = output {
                // A directory input (a whole project/package) always compiles into
                // `output` as a directory root — never derive a single filename+
                // extension from it. `output_is_dir` only reflects whether the
                // *output* path happens to already exist as a directory (e.g. from
                // a prior run), which is unrelated and previously caused a second
                // transpile into an existing output dir to nest everything under a
                // spurious `<input-dir-name>.<ext>` subdirectory instead of
                // overwriting in place.
                if output_is_dir && !input.is_dir() {
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
                    native_binary_extension(input_class, emitter, native_link_requested, target_triple)
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

        if matches!(backend, BackendKind::Binary) {
            if is_raw_binary_passthrough(input_class, emitter) {
                return Ok(output.clone());
            }

            let mut path = output.clone();
            let desired_ext = if is_windows_target(target_triple) {
                "exe"
            } else {
                "out"
            };

            // Respect explicit `-o <path>.<ext>` even when the extension differs
            // from the default (`.out`/`.exe`). Only fill the extension when the
            // user did not provide one.
            if path.extension().is_none() {
                path.set_extension(desired_ext);
            }

            return Ok(path);
        }

        if matches!(backend, BackendKind::Bytecode) && emit_text_bytecode {
            let mut path = output.clone();
            if path.extension().is_none() {
                path.set_extension("ftbc");
            }
            return Ok(path);
        }

        if matches!(backend, BackendKind::Dotnet) {
            let mut path = output.clone();
            let desired_ext = match path.extension().and_then(|ext| ext.to_str()) {
                Some(ext) if ext.eq_ignore_ascii_case("dll") => "dll",
                Some(ext) if ext.eq_ignore_ascii_case("exe") => "exe",
                _ => "exe",
            };
            if path.extension().is_none() {
                path.set_extension(desired_ext);
            }
            return Ok(path);
        }

        Ok(output.clone())
    } else {
        let extension = match backend {
            BackendKind::Binary => native_binary_extension(input_class, emitter, false, target_triple),
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
