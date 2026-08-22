mod cil;
mod parse;

pub use cil::assemble_cil_text;
pub use cil::emit_assembly;
pub use cil::emit_cil;
pub use parse::parse_cil_program;

fn package_ast(
    workspace: &fp_core::workspace::WorkspaceContext,
    package_id: &fp_core::package::PackageId,
) -> fp_core::error::Result<fp_core::ast::File> {
    let source = workspace.package_source(package_id)?;
    Ok(fp_core::ast::File {
        path: std::path::PathBuf::new(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: source.items.into_iter().map(|item| item.item).collect(),
    })
}

/// `TargetBackend` for the `--target cil` target — reads the package's
/// typed AST off the shared workspace instead of re-parsing (untyped) from
/// source.
pub struct CilBackend {
    pub output: std::path::PathBuf,
}

impl fp_core::backend::TargetBackend for CilBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let ast = package_ast(workspace, package_id)?;
        let code = emit_cil(&ast)
            .map_err(|e| fp_core::error::Error::from(format!("CIL emit failed: {e}")))?;
        if let Some(parent) = self.output.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&self.output, code)?;
        Ok(())
    }
}

/// `TargetBackend` for the `--target dotnet` target — reads the package's
/// typed AST off the shared workspace instead of re-parsing (untyped) from
/// source.
pub struct DotnetBackend {
    pub output: std::path::PathBuf,
    pub save_intermediates: bool,
}

impl fp_core::backend::TargetBackend for DotnetBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let ast = package_ast(workspace, package_id)?;
        emit_assembly(&ast, &self.output, self.save_intermediates)
            .map_err(|e| fp_core::error::Error::from(format!(".NET assembly emit failed: {e}")))?;
        Ok(())
    }

    fn exec(&self) -> fp_core::error::Result<()> {
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

fn command_available(command: &str) -> bool {
    let path_var = std::env::var_os("PATH").unwrap_or_default();
    std::env::split_paths(&path_var)
        .map(|entry| entry.join(command))
        .any(|candidate| candidate.is_file())
}
