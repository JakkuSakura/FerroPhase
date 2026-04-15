use crate::cli::CliConfig;
use crate::commands::compile::{CompileArgs, EmitterKind};
use crate::error::{CliError, Result};
use crate::pipeline::BackendKind;
use fp_core::container::ContainerReader as _;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

use fp_native::emit;
use tokio::process::Command;

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

    let link_requested = args.exec || args.link;

    let output_path = if args.output.is_none() {
        if link_requested {
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

    if args.save_intermediates {
        let dump_path = output_path.with_extension("asm");
        fp_native::emit::dump_asm(&dump_path, &plan).map_err(|err| {
            CliError::Compilation(format!("Failed to dump native AsmIR plan: {err}"))
        })?;
    }

    if link_requested {
        let needs_external_link = format == emit::TargetFormat::MachO
            && plan_has_undefined_symbols(&plan)
            && args.linker != "";

        if needs_external_link {
            link_native_object_with_clang(&output_path, args, bytes, &plan, format, arch).await?;
        } else if let Err(err) = fp_native::emit::write_executable(&output_path, &plan) {
            if format == emit::TargetFormat::MachO {
                link_native_object_with_clang(&output_path, args, bytes, &plan, format, arch)
                    .await?;
            } else {
                return Err(CliError::Compilation(format!(
                    "Failed to write executable output: {err}"
                )));
            }
        }
    } else {
        fp_native::emit::write_object(&output_path, &plan).map_err(|err| {
            CliError::Compilation(format!("Failed to write object output: {err}"))
        })?;
    }

    Ok(Some(output_path))
}

fn plan_has_undefined_symbols(plan: &emit::EmitPlan) -> bool {
    let mut defined = HashSet::new();
    defined.extend(plan.symbols.keys().map(|name| name.as_str()));
    defined.extend(plan.rodata_symbols.keys().map(|name| name.as_str()));

    plan.relocs
        .iter()
        .any(|reloc| !defined.contains(reloc.symbol.as_str()))
}

async fn link_native_object_with_clang(
    output_path: &Path,
    args: &CompileArgs,
    input_bytes: &[u8],
    plan: &emit::EmitPlan,
    format: emit::TargetFormat,
    arch: emit::TargetArch,
) -> Result<()> {
    const DARWIN_LINUX_MAIN_WRAPPER: &str = r#"
#include <stdint.h>

// Minimal wrapper for lifted Linux SysV entrypoints.
//
// The lifted `fp_lifted_main` may contain x86_64 stack-realignment prologues
// that read from the incoming stack pointer. Touch the stack in native code
// first to ensure the mapping is fault-free.

extern int fp_lifted_main(int argc, char **argv, char **envp);

int main(int argc, char **argv, char **envp) {
  volatile uint8_t probe[4096];
  probe[0] = 0;
  return fp_lifted_main(argc, argv, envp);
}
"#;

    let tmp_dir = std::env::temp_dir().join(format!("fp-link-{}", std::process::id()));
    std::fs::create_dir_all(&tmp_dir).map_err(CliError::Io)?;
    let object_path = tmp_dir.join("input.o");
    let wrapper_c_path = tmp_dir.join("wrapper.c");
    let wrapper_object_path = tmp_dir.join("wrapper.o");

    // Prefer reusing the emitted plan (it already includes relocations).
    fp_native::emit::write_object(&object_path, plan).map_err(|err| {
        CliError::Compilation(format!(
            "Failed to write temporary object for linking: {err}"
        ))
    })?;

    if matches!(format, emit::TargetFormat::MachO)
        && matches!(arch, emit::TargetArch::Aarch64 | emit::TargetArch::X86_64)
    {
        let needs_main_wrapper =
            !plan.symbols.contains_key("main") && plan.symbols.contains_key("fp_lifted_main");
        if needs_main_wrapper {
            std::fs::write(&wrapper_c_path, DARWIN_LINUX_MAIN_WRAPPER).map_err(CliError::Io)?;

            let mut cc = Command::new(&args.linker);
            if let Some(sysroot) = &args.target_sysroot {
                cc.arg(format!("--sysroot={}", sysroot.display()));
            }
            if let Some(ld) = &args.target_linker {
                cc.arg(format!("-fuse-ld={}", ld.display()));
            }
            match arch {
                emit::TargetArch::Aarch64 => cc.args(["-arch", "arm64"]),
                emit::TargetArch::X86_64 => cc.args(["-arch", "x86_64"]),
            };
            cc.args(["-c", "-x", "c"]);
            cc.arg(&wrapper_c_path);
            cc.arg("-o").arg(&wrapper_object_path);

            let output = cc.output().await.map_err(|err| {
                CliError::Compilation(format!(
                    "Failed to invoke compiler '{}': {err}",
                    args.linker
                ))
            })?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                let stdout = String::from_utf8_lossy(&output.stdout);
                return Err(CliError::Compilation(format!(
                    "Failed to compile Darwin main wrapper (status {:?}).\nstdout:\n{stdout}\nstderr:\n{stderr}",
                    output.status.code()
                )));
            }
        }
    }

    let mut cmd = Command::new(&args.linker);
    if let Some(sysroot) = &args.target_sysroot {
        cmd.arg(format!("--sysroot={}", sysroot.display()));
    }
    if let Some(ld) = &args.target_linker {
        cmd.arg(format!("-fuse-ld={}", ld.display()));
    }

    match (format, arch) {
        (emit::TargetFormat::MachO, emit::TargetArch::Aarch64) => {
            cmd.args(["-arch", "arm64"]);
            cmd.arg("-Wl,-undefined,dynamic_lookup");
            cmd.arg("-Wl,-no_dead_strip_inits_and_terms");
        }
        (emit::TargetFormat::MachO, emit::TargetArch::X86_64) => {
            cmd.args(["-arch", "x86_64"]);
            cmd.arg("-Wl,-undefined,dynamic_lookup");
            cmd.arg("-Wl,-no_dead_strip_inits_and_terms");
        }
        _ => {}
    }

    // Use the platform CRT entrypoint so constructors and runtime init run.
    // The transpiled object is expected to provide `_main`.
    cmd.arg("-o").arg(output_path);
    if wrapper_object_path.exists() {
        cmd.arg(&wrapper_object_path);
    }
    cmd.arg(&object_path);

    let output = cmd.output().await.map_err(|err| {
        CliError::Compilation(format!("Failed to invoke linker '{}': {err}", args.linker))
    })?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(CliError::Compilation(format!(
            "External linker failed (status {:?}).\nstdout:\n{stdout}\nstderr:\n{stderr}",
            output.status.code()
        )));
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if let Ok(metadata) = std::fs::metadata(output_path) {
            let mut perms = metadata.permissions();
            perms.set_mode(0o755);
            let _ = std::fs::set_permissions(output_path, perms);
        }
    }

    // Best-effort cleanup.
    let _ = std::fs::remove_file(&object_path);
    let _ = std::fs::remove_file(&wrapper_object_path);
    let _ = std::fs::remove_file(&wrapper_c_path);
    let _ = std::fs::remove_dir(&tmp_dir);

    let _ = input_bytes;
    Ok(())
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
