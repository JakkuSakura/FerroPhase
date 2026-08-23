use std::path::{Path, PathBuf};
use std::process::Command;

use eyre::{Result as EyreResult, bail};
use fp_core::error::Result;
use tempfile::TempDir;

/// Lowers `mir` into CIL assembly text — mirrors `fp_jvm::lower_program`+
/// `emit_class_files`'s MIR→bytecode shape (CIL is stack-based bytecode
/// like JVM's, not a source-level transpile target like Kotlin's; walking
/// the typed AST directly, as this used to, was the wrong level).
/// Not yet implemented.
pub fn emit_cil(_mir: &fp_core::mir::MirModule) -> Result<String> {
    todo!("lower MIR to CIL assembly text")
}

pub fn emit_assembly(
    mir: &fp_core::mir::MirModule,
    output_path: &Path,
    keep_cil: bool,
) -> Result<PathBuf> {
    let cil = emit_cil(mir)?;
    assemble_cil(&cil, output_path, keep_cil).map_err(fp_core::error::Error::from)
}

pub fn assemble_cil_text(cil: &str, output_path: &Path) -> Result<PathBuf> {
    assemble_cil(cil, output_path, false).map_err(fp_core::error::Error::from)
}

fn assemble_cil(cil: &str, output_path: &Path, keep_cil: bool) -> EyreResult<PathBuf> {
    let assembly_kind = assembly_kind_for(output_path);
    let cil_path = if keep_cil {
        let cil_path = output_path.with_extension("il");
        std::fs::write(&cil_path, cil)?;
        cil_path
    } else {
        let temp_dir = TempDir::new()?;
        let cil_path = temp_dir.path().join("ferrophase.il");
        std::fs::write(&cil_path, cil)?;
        run_ilasm(&cil_path, output_path, assembly_kind)?;
        return Ok(output_path.to_path_buf());
    };

    run_ilasm(&cil_path, output_path, assembly_kind)?;
    Ok(output_path.to_path_buf())
}

fn run_ilasm(
    cil_path: &Path,
    output_path: &Path,
    assembly_kind: DotnetAssemblyKind,
) -> EyreResult<()> {
    let ilasm = find_ilasm().ok_or_else(|| {
        eyre::eyre!(
            "unable to locate `ilasm`; install Mono ilasm or provide it on PATH to emit .NET assemblies"
        )
    })?;

    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let mut command = Command::new(ilasm);
    command.arg(cil_path);
    command.arg(format!("/output:{}", output_path.display()));
    command.arg(match assembly_kind {
        DotnetAssemblyKind::Exe => "/exe",
        DotnetAssemblyKind::Dll => "/dll",
    });

    let output = command.output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        bail!(
            "ilasm failed for `{}`: {}{}{}",
            cil_path.display(),
            stdout,
            if !stdout.is_empty() && !stderr.is_empty() {
                "\n"
            } else {
                ""
            },
            stderr
        );
    }

    Ok(())
}

fn find_ilasm() -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|entry| entry.join("ilasm"))
        .find(|candidate| candidate.is_file())
}

#[derive(Clone, Copy)]
enum DotnetAssemblyKind {
    Exe,
    Dll,
}

fn assembly_kind_for(output_path: &Path) -> DotnetAssemblyKind {
    match output_path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) if ext.eq_ignore_ascii_case("dll") => DotnetAssemblyKind::Dll,
        _ => DotnetAssemblyKind::Exe,
    }
}
