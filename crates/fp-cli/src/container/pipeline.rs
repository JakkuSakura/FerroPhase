use crate::cli::CliConfig;
use crate::commands::compile::{CompileArgs, EmitterKind};
use crate::error::{CliError, Result};
use crate::pipeline::BackendKind;
use fp_core::container::ContainerReader as _;
use std::path::{Path, PathBuf};

use fp_native::emit;

use super::registry::{ContainerInputKind, ContainerRegistry};

pub(crate) async fn maybe_transpile_container(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    _config: &CliConfig,
) -> Result<Option<PathBuf>> {
    let registry = ContainerRegistry::new();
    let Some(kind) = registry.detect_input_kind(input, args.source_language.as_deref()) else {
        return Ok(None);
    };

    let payload = tokio::fs::read(input).await.map_err(|err| {
        CliError::Io(std::io::Error::other(format!(
            "Failed to read container input: {err}"
        )))
    })?;
    let read = registry.read_container(kind, payload)?;

    match read.kind {
        ContainerInputKind::NativeObject => {
            transpile_native_object(input, output, args, &read.payload).await
        }
        ContainerInputKind::NativeArchive => {
            transpile_native_archive(input, output, args, &read.payload).await
        }
        ContainerInputKind::JvmBytecode => {
            transpile_jvm_bytecode(input, output, args, &read.payload).await
        }
        ContainerInputKind::Cil => transpile_cil(input, output, args, &read.payload).await,
        ContainerInputKind::GoAsm => transpile_goasm(input, output, args, &read.payload).await,
        ContainerInputKind::Urcl => transpile_urcl(input, output, args, &read.payload).await,
    }
}

async fn transpile_native_object(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
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

    let asmir = fp_native::binary::lift_object_to_asmir(bytes)
        .map_err(|err| CliError::Compilation(format!("Failed to lift object file: {err}")))?;
    let (format, arch) = emit::detect_target(args.target_triple.as_deref())
        .map_err(|err| CliError::Compilation(err.to_string()))?;
    let plan = fp_native::emit::emit_plan_from_asmir(asmir, format, arch)
        .map_err(|err| CliError::Compilation(format!("Failed to emit target object: {err}")))?;

    let output_path = if args.output.is_none() {
        if args.exec {
            input.with_extension("out")
        } else {
            input.with_extension("o")
        }
    } else {
        output.to_path_buf()
    };
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(CliError::Io)?;
    }

    if args.exec {
        fp_native::emit::write_executable(&output_path, &plan).map_err(|err| {
            CliError::Compilation(format!("Failed to write executable output: {err}"))
        })?;
    } else {
        fp_native::emit::write_object(&output_path, &plan).map_err(|err| {
            CliError::Compilation(format!("Failed to write object output: {err}"))
        })?;
    }

    Ok(Some(output_path))
}

async fn transpile_native_archive(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    if args.emitter != EmitterKind::Native {
        return Err(CliError::InvalidInput(
            "native archive input currently requires `--emitter native`".to_string(),
        ));
    }
    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "native archive input currently only supports `--backend binary` transpilation"
                .to_string(),
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

    match args.backend {
        BackendKind::JvmBytecode => {
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
                            "JAR input requires output extension `.jar` when using `--backend jvm-bytecode`"
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
        BackendKind::Binary => {
            let lir_program = if is_jar {
                let classes = fp_jvm::extract_class_files_from_jar(bytes)
                    .map_err(|err| CliError::Compilation(format!("Failed to parse jar: {err}")))?;
                let mut merged = fp_core::lir::LirProgram::new();
                for class in classes {
                    let program = fp_jvm::parse_class_to_lir(&class.bytes).map_err(|err| {
                        CliError::Compilation(format!(
                            "Failed to parse classfile {}: {err}",
                            class.internal_name
                        ))
                    })?;
                    merged.extend(program);
                }
                merged
            } else {
                fp_jvm::parse_class_to_lir(bytes).map_err(|err| {
                    CliError::Compilation(format!("Failed to parse classfile: {err}"))
                })?
            };

            emit_lir_program(&lir_program, input, &output_path, args)?;
        }
        other => {
            return Err(CliError::InvalidInput(format!(
                "JVM bytecode input currently supports only `--backend jvm-bytecode` or `--backend binary` (got {})",
                other.as_str()
            )));
        }
    }

    Ok(Some(output_path))
}

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

    match args.backend {
        BackendKind::Cil => {
            if is_binary_pe {
                return Err(CliError::InvalidInput(
                    "`--backend cil` currently expects textual `.il` input".to_string(),
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
        BackendKind::Dotnet => {
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
        BackendKind::Binary => {
            if is_binary_pe {
                return Err(CliError::InvalidInput(
                    "binary .dll/.exe -> native transpilation is not implemented yet".to_string(),
                ));
            }
            let lir_program = fp_dotnet::parse_cil_program(&text)
                .map_err(|err| CliError::Compilation(format!("Failed to parse CIL: {err}")))?;

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
            emit_lir_program(&lir_program, input, &output_path, args)?;
            Ok(Some(output_path))
        }
        other => Err(CliError::InvalidInput(format!(
            "CIL input currently supports only `--backend cil` or `--backend dotnet` (got {})",
            other.as_str()
        ))),
    }
}

async fn transpile_goasm(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "Go asm input currently supports only `--backend binary`".to_string(),
        ));
    }

    let text = String::from_utf8(bytes.to_vec())
        .map_err(|_| CliError::InvalidInput("goasm input must be valid UTF-8".to_string()))?;
    let (lir_program, _source_target) = fp_goasm::parse_program(&text)
        .map_err(|err| CliError::Compilation(format!("Failed to parse goasm: {err}")))?;

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
    emit_lir_program(&lir_program, input, &output_path, args)?;
    Ok(Some(output_path))
}

async fn transpile_urcl(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    bytes: &[u8],
) -> Result<Option<PathBuf>> {
    if args.backend != BackendKind::Binary {
        return Err(CliError::InvalidInput(
            "URCL input currently supports only `--backend binary`".to_string(),
        ));
    }
    let text = String::from_utf8(bytes.to_vec())
        .map_err(|_| CliError::InvalidInput("URCL input must be valid UTF-8".to_string()))?;
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
    emit_lir_program(&lir_program, input, &output_path, args)?;
    Ok(Some(output_path))
}

fn emit_lir_program(
    lir_program: &fp_core::lir::LirProgram,
    input: &Path,
    output_path: &Path,
    args: &CompileArgs,
) -> Result<()> {
    match args.emitter {
        EmitterKind::Native => {
            let (format, arch) = emit::detect_target(args.target_triple.as_deref())
                .map_err(|err| CliError::Compilation(err.to_string()))?;
            let plan = fp_native::emit::emit_plan(lir_program, format, arch).map_err(|err| {
                CliError::Compilation(format!("Failed to emit native object: {err}"))
            })?;
            fp_native::emit::write_object(output_path, &plan).map_err(|err| {
                CliError::Compilation(format!("Failed to write object output: {err}"))
            })?;
        }
        EmitterKind::Goasm => {
            let config = fp_goasm::config::GoAsmConfig::new(output_path)
                .with_target_triple(args.target_triple.clone());
            let emitter = fp_goasm::GoAsmEmitter::new(config);
            emitter
                .emit(lir_program.clone(), Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit Go asm: {err}")))?;
        }
        EmitterKind::Urcl => {
            let emitter = fp_urcl::UrclEmitter::new(fp_urcl::UrclConfig::new(output_path));
            emitter
                .emit(lir_program.clone(), Some(input))
                .map_err(|err| CliError::Compilation(format!("Failed to emit URCL: {err}")))?;
        }
        other => {
            return Err(CliError::InvalidInput(format!(
                "container transpilation does not support `--emitter {}` yet",
                other.as_str()
            )));
        }
    }
    Ok(())
}
