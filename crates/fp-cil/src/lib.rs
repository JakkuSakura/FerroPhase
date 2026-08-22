mod cil;
pub mod package;
mod parse;

pub use cil::assemble_cil_text;
pub use cil::emit_assembly;
pub use cil::emit_cil;
pub use parse::parse_cil_program;

/// Reads `package_id`'s already-compiled MIR straight off the shared
/// workspace's `CompiledPackage` — same source `fp_jvm::JvmBackend` reads
/// from, since CIL (like JVM bytecode) is a stack-based bytecode target,
/// not a source-level transpile target; it lowers from MIR, not from the
/// typed AST a Kotlin/Python-style backend would walk.
fn package_mir(
    workspace: &fp_core::workspace::WorkspaceContext,
    package_id: &fp_core::package::PackageId,
) -> fp_core::error::Result<fp_core::mir::Program> {
    let package = workspace.compiled_package(package_id).ok_or_else(|| {
        fp_core::error::Error::from(format!("package `{package_id}` is unavailable"))
    })?;
    package.borrow().mir_program.clone().ok_or_else(|| {
        fp_core::error::Error::from(format!("package `{package_id}` has no MIR program"))
    })
}

/// `TargetBackend` for both `--target cil` (`assemble: false`) and
/// `--target dotnet` (`assemble: true`) — the only difference between the
/// two is whether the emitted CIL text gets a further `ilasm` assembly
/// pass into a real, runnable .NET binary, so one backend with an option
/// covers both instead of two near-identical structs.
pub struct CilBackend {
    pub output: std::path::PathBuf,
    /// `false` (`--target cil`): write the emitted CIL assembly text
    /// itself. `true` (`--target dotnet`): assemble it into a real
    /// `.exe`/`.dll` via `ilasm`.
    pub assemble: bool,
    /// Only consulted when `assemble` is set — keep the intermediate
    /// `.il` text alongside the assembled binary.
    pub save_intermediates: bool,
}

impl fp_core::backend::TargetBackend for CilBackend {
    fn emit_package_artifact(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> fp_core::error::Result<()> {
        // CIL text or an assembled PE given directly as input (see
        // `fp_core::ast::ItemKind::PrecompiledArtifact`'s doc comment)
        // writes/assembles itself back out instead of going through MIR.
        if let Ok(source) = workspace.package_source(package_id) {
            let artifact = source.items.iter().find_map(|pkg_item| match pkg_item.item.kind() {
                fp_core::ast::ItemKind::PrecompiledArtifact(bytes) => Some(bytes.clone()),
                _ => None,
            });
            if let Some(bytes) = artifact {
                return self.write_passthrough(&bytes);
            }
        }

        let mir = package_mir(workspace, package_id)?;
        if self.assemble {
            emit_assembly(&mir, &self.output, self.save_intermediates).map_err(|e| {
                fp_core::error::Error::from(format!(".NET assembly emit failed: {e}"))
            })?;
            return Ok(());
        }
        let code = emit_cil(&mir)
            .map_err(|e| fp_core::error::Error::from(format!("CIL emit failed: {e}")))?;
        if let Some(parent) = self.output.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&self.output, code)?;
        Ok(())
    }

    fn exec(&self) -> fp_core::error::Result<()> {
        if !self.assemble {
            return Err(fp_core::error::Error::from(
                "--exec is not supported for --target cil (no assembled binary; use --target dotnet)"
                    .to_string(),
            ));
        }
        let extension = self
            .output
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_ascii_lowercase());

        let mut command = if cfg!(windows) && extension.as_deref() == Some("exe") {
            std::process::Command::new(&self.output)
        } else if command_available("mono") {
            let mut command = std::process::Command::new("mono");
            command.arg(&self.output);
            command
        } else if extension.as_deref() == Some("dll") {
            let mut command = std::process::Command::new("dotnet");
            command.arg(&self.output);
            command
        } else {
            return Err(fp_core::error::Error::from(format!(
                "Refusing to execute '{}': unsupported .NET assembly extension",
                self.output.display()
            )));
        };

        let output = command.output().map_err(|e| {
            fp_core::error::Error::from(format!("failed to execute '{}': {e}", self.output.display()))
        })?;
        if !output.status.success() {
            let code = output.status.code().unwrap_or(-1);
            return Err(fp_core::error::Error::from(format!(
                ".NET process exited with status {code}"
            )));
        }
        Ok(())
    }
}

impl CilBackend {
    /// Writes an already-compiled CIL text/PE's raw bytes back out —
    /// `assemble: false` (`--target cil`) writes textual CIL verbatim and
    /// rejects binary PE input (no disassembler); `assemble: true`
    /// (`--target dotnet`) writes a PE verbatim, or assembles textual CIL
    /// via `ilasm`, matching the previous bespoke pipeline's exact
    /// per-target behavior.
    fn write_passthrough(&self, bytes: &[u8]) -> fp_core::error::Result<()> {
        let is_pe = bytes.starts_with(b"MZ");
        if let Some(parent) = self.output.parent() {
            std::fs::create_dir_all(parent)?;
        }
        if !self.assemble {
            if is_pe {
                return Err(fp_core::error::Error::from(
                    "`--target cil` currently expects textual `.il` input".to_string(),
                ));
            }
            std::fs::write(&self.output, bytes)?;
            return Ok(());
        }
        if is_pe {
            std::fs::write(&self.output, bytes)?;
        } else {
            let text = String::from_utf8(bytes.to_vec()).map_err(|_| {
                fp_core::error::Error::from("CIL input must be valid UTF-8".to_string())
            })?;
            assemble_cil_text(&text, &self.output)
                .map_err(|e| fp_core::error::Error::from(format!("Failed to assemble CIL: {e}")))?;
        }
        Ok(())
    }
}

fn command_available(command: &str) -> bool {
    let path_var = std::env::var_os("PATH").unwrap_or_default();
    std::env::split_paths(&path_var)
        .map(|entry| entry.join(command))
        .any(|candidate| candidate.is_file())
}
