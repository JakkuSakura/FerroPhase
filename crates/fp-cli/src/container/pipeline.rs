use crate::cli::CliConfig;
use crate::commands::compile::CompileArgs;
use crate::error::{CliError, Result};
use fp_core::container::ContainerReader as _;
use std::path::{Path, PathBuf};

use fp_native::emit;

use super::registry::{ContainerInputKind, ContainerRegistry};

/// Whether `target` is one of the codegen targets that reads (merged) LIR
/// and can plausibly emit a native-binary-shaped artifact — used to decide
/// which container inputs a given `--target` is allowed to re-emit.
fn is_binary_producing_target(target: &str) -> bool {
    matches!(
        target,
        "native" | "goasm" | "urcl" | "llvm-binary" | "llvm-text" | "cranelift"
    )
}

/// `kind` is the classification `compile_once` already computed once for
/// this input via `ContainerRegistry::classify_input` — not re-detected
/// here, so the input is only ever read from disk for its actual payload,
/// never re-sniffed.
pub(crate) async fn maybe_transpile_container(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    _config: &CliConfig,
    kind: Option<ContainerInputKind>,
    exec: bool,
) -> Result<Option<PathBuf>> {
    let Some(kind) = kind else {
        return Ok(None);
    };
    // None of the remaining bespoke-pipeline container kinds support
    // `--exec` (native objects, the one kind that did, are a real
    // registered language now — see `fp_native::NativeObjectPackageProvider`
    // — and never reach this function at all).
    if exec {
        return Err(CliError::InvalidInput(
            "--exec is not supported for this container input".to_string(),
        ));
    }
    let registry = ContainerRegistry::new();

    let payload = tokio::fs::read(input).await.map_err(|err| {
        CliError::Io(std::io::Error::other(format!(
            "Failed to read container input: {err}"
        )))
    })?;
    let read = registry.read_container(kind, payload)?;

    match read.kind {
        ContainerInputKind::NativeArchive => {
            transpile_native_archive(input, output, args, &read.payload).await
        }
        ContainerInputKind::JvmBytecode => {
            transpile_jvm_bytecode(input, output, args, &read.payload).await
        }
        ContainerInputKind::Cil => transpile_cil(input, output, args, &read.payload).await,
    }
}

async fn transpile_native_archive(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    if args.target != "native" {
        return Err(CliError::InvalidInput(
            "native archive input currently requires `--target native`".to_string(),
        ));
    }

    let object_reader = fp_native::container::ObjectContainerReader::new();
    let (format, arch) = emit::detect_target(args.target_triple.as_deref())
        .map_err(|err| CliError::Compilation(err.to_string()))?;

    let members = fp_native::archive::read_archive_members(bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to parse archive input: {err}")))?;

    let mut out_members = Vec::with_capacity(members.len());
    for member in members {
        if member.data.is_empty() || !object_reader.can_read(&member.data) {
            out_members.push(member);
            continue;
        }

        let asmir = fp_native::binary::lift_object_to_asmir(&member.data)
            .map_err(|err| CliError::Compilation(format!("Failed to lift object member: {err}")))?;
        let plan = fp_native::emit::emit_plan_from_asmir(asmir, format, arch)
            .map_err(|err| CliError::Compilation(format!("Failed to emit target object: {err}")))?;
        let out_bytes = fp_native::emit::write_object_bytes(&plan)
            .map_err(|err| CliError::Compilation(format!("Failed to write object bytes: {err}")))?;

        out_members.push(fp_native::archive::ArchiveMember {
            name: member.name,
            data: out_bytes,
        });
    }

    let archive_bytes = fp_native::archive::write_gnu_archive(&out_members)
        .map_err(|err| CliError::Compilation(format!("Failed to write archive output: {err}")))?;

    let output_path = if args.output.is_none() {
        input.with_extension("a")
    } else {
        output.to_path_buf()
    };
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }
    tokio::fs::write(&output_path, archive_bytes)
        .await
        .map_err(|err| {
            CliError::Io(std::io::Error::other(format!(
                "Failed to write archive output: {err}"
            )))
        })?;

    Ok(Some(output_path))
}

async fn transpile_jvm_bytecode(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    let extension = input
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase());
    let is_jar = matches!(extension.as_deref(), Some("jar"));
    if !is_jar && !bytes.starts_with(&[0xCA, 0xFE, 0xBA, 0xBE]) {
        return Err(CliError::InvalidInput(
            "invalid .class input (missing CAFEBABE header)".to_string(),
        ));
    }

    let output_path = output.to_path_buf();
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    match args.target.as_str() {
        "jvm-bytecode" => {
            let out_ext = output_path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext.to_ascii_lowercase());
            match out_ext.as_deref() {
                Some("jar") => {
                    if is_jar {
                        tokio::fs::write(&output_path, bytes).await.map_err(|err| {
                            CliError::Io(std::io::Error::other(format!(
                                "Failed to write jar output: {err}"
                            )))
                        })?;
                    } else {
                        let stem = input.file_stem().and_then(|s| s.to_str()).ok_or_else(|| {
                            CliError::InvalidInput("Invalid input filename".to_string())
                        })?;
                        let jar = fp_jvm::emit_executable_jar(
                            &[fp_jvm::EmittedClass {
                                internal_name: stem.to_string(),
                                bytes: bytes.to_vec(),
                            }],
                            stem,
                        )
                        .map_err(|err| {
                            CliError::Compilation(format!("Failed to emit jar: {err}"))
                        })?;
                        tokio::fs::write(&output_path, jar).await.map_err(|err| {
                            CliError::Io(std::io::Error::other(format!(
                                "Failed to write jar output: {err}"
                            )))
                        })?;
                    }
                }
                _ => {
                    if is_jar {
                        return Err(CliError::InvalidInput(
                            "JAR input requires output extension `.jar` when using `--target jvm-bytecode`"
                                .to_string(),
                        ));
                    }
                    tokio::fs::write(&output_path, bytes).await.map_err(|err| {
                        CliError::Io(std::io::Error::other(format!(
                            "Failed to write class output: {err}"
                        )))
                    })?;
                }
            }
        }
        target if is_binary_producing_target(target) => {
            let lir_program = if is_jar {
                let classes = fp_jvm::extract_class_files_from_jar(bytes)
                    .map_err(|err| CliError::Compilation(format!("Failed to parse jar: {err}")))?;
                let mut merged: Option<fp_core::lir::LirProgram> = None;
                for class in classes {
                    let program = fp_jvm::parse_class_to_lir(&class.bytes).map_err(|err| {
                        CliError::Compilation(format!(
                            "Failed to parse classfile {}: {err}",
                            class.internal_name
                        ))
                    })?;
                    if let Some(merged_program) = merged.as_mut() {
                        merged_program.extend(program).map_err(|err| {
                            CliError::Compilation(format!(
                                "Cannot merge classfile LIR programs with different target layouts: {err}"
                            ))
                        })?;
                    } else {
                        merged = Some(program);
                    }
                }
                merged.ok_or_else(|| {
                    CliError::Compilation("JAR contains no class files".to_string())
                })?
            } else {
                fp_jvm::parse_class_to_lir(bytes).map_err(|err| {
                    CliError::Compilation(format!("Failed to parse classfile: {err}"))
                })?
            };

            emit_lir_program(&lir_program, input, &output_path, args)?;
        }
        other => {
            return Err(CliError::InvalidInput(format!(
                "JVM bytecode input currently supports only `--target jvm-bytecode` or a native codegen target (got {other})"
            )));
        }
    }

    Ok(Some(output_path))
}

#[cfg(feature = "lang-dotnet")]
async fn transpile_cil(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    let extension = input
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase());
    let is_binary_pe = matches!(extension.as_deref(), Some("dll" | "exe"));

    let text = if is_binary_pe {
        String::new()
    } else {
        String::from_utf8(bytes.to_vec())
            .map_err(|_| CliError::InvalidInput("CIL input must be valid UTF-8".to_string()))?
    };

    match args.target.as_str() {
        "cil" => {
            if is_binary_pe {
                return Err(CliError::InvalidInput(
                    "`--target cil` currently expects textual `.il` input".to_string(),
                ));
            }
            let output_path = if args.output.is_none() {
                input.with_extension("il")
            } else {
                output.to_path_buf()
            };
            if let Some(parent) = output_path.parent() {
                std::fs::create_dir_all(parent).map_err(CliError::Io)?;
            }
            tokio::fs::write(&output_path, text).await.map_err(|err| {
                CliError::Io(std::io::Error::other(format!(
                    "Failed to write CIL output: {err}"
                )))
            })?;
            Ok(Some(output_path))
        }
        "dotnet" => {
            let output_path = if args.output.is_none() {
                input.with_extension("exe")
            } else {
                output.to_path_buf()
            };
            if let Some(parent) = output_path.parent() {
                std::fs::create_dir_all(parent).map_err(CliError::Io)?;
            }
            if is_binary_pe {
                tokio::fs::copy(input, &output_path).await.map_err(|err| {
                    CliError::Io(std::io::Error::other(format!(
                        "Failed to copy dotnet assembly: {err}"
                    )))
                })?;
            } else {
                fp_dotnet::assemble_cil_text(&text, &output_path).map_err(|err| {
                    CliError::Compilation(format!("Failed to assemble CIL: {err}"))
                })?;
            }
            Ok(Some(output_path))
        }
        target if is_binary_producing_target(target) => {
            if is_binary_pe {
                return Err(CliError::InvalidInput(
                    "binary .dll/.exe -> native transpilation is not implemented yet".to_string(),
                ));
            }
            let lir_program = fp_dotnet::parse_cil_program(&text)
                .map_err(|err| CliError::Compilation(format!("Failed to parse CIL: {err}")))?;

            let output_path = if args.output.is_none() {
                match target {
                    "goasm" => input.with_extension("s"),
                    "urcl" => input.with_extension("urcl"),
                    _ => input.with_extension("o"),
                }
            } else {
                output.to_path_buf()
            };
            if let Some(parent) = output_path.parent() {
                std::fs::create_dir_all(parent).map_err(CliError::Io)?;
            }
            emit_lir_program(&lir_program, input, &output_path, args)?;
            Ok(Some(output_path))
        }
        other => Err(CliError::InvalidInput(format!(
            "CIL input currently supports only `--target cil` or `--target dotnet` (got {other})"
        ))),
    }
}

#[cfg(not(feature = "lang-dotnet"))]
async fn transpile_cil(
    _input: &Path,
    _output: &Path,
    _args: &CompileArgs,
    _bytes: &[u8],
) -> Result<Option<PathBuf>> {
    Err(CliError::MissingDependency(
        "Feature 'lang-dotnet' is disabled; CIL/NET transpilation is unavailable.".to_string(),
    ))
}


fn emit_lir_program(
    lir_program: &fp_core::lir::LirProgram,
    input: &Path,
    output_path: &Path,
    args: &CompileArgs,
) -> Result<()> {
    match args.target.as_str() {
        "native" => {
            let (format, arch) = emit::detect_target(args.target_triple.as_deref())
                .map_err(|err| CliError::Compilation(err.to_string()))?;
            let plan = fp_native::emit::emit_plan(lir_program, format, arch).map_err(|err| {
                CliError::Compilation(format!("Failed to emit native object: {err}"))
            })?;
            fp_native::emit::write_object(output_path, &plan).map_err(|err| {
                CliError::Compilation(format!("Failed to write object output: {err}"))
            })?;
        }
        "goasm" => {
            let config = fp_goasm::config::GoAsmConfig::new(output_path)
                .with_target_triple(args.target_triple.clone());
            let emitter = fp_goasm::GoAsmEmitter::new(config);
            emitter
                .emit(lir_program.clone(), Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit Go asm: {err}")))?;
        }
        "urcl" => {
            let emitter = fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(output_path));
            emitter
                .emit(lir_program.clone(), Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit URCL: {err}")))?;
        }
        other => {
            return Err(CliError::InvalidInput(format!(
                "container transpilation does not support `--target {other}` yet"
            )));
        }
    }
    Ok(())
}
