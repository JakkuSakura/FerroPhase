use crate::cli::CliConfig;
use crate::commands::compile::CompileArgs;
use crate::error::{CliError, Result};
use fp_core::container::ContainerReader as _;
use std::path::{Path, PathBuf};

use fp_native::emit;

use super::registry::{ContainerInputKind, ContainerRegistry};

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
    // `NativeArchive` (the one kind still on this bespoke pipeline)
    // doesn't support `--exec` — an archive is never itself a runnable
    // artifact.
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
