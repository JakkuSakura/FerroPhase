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

/// `TargetBackend` for the `--backend cil` target — reads the package's
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

/// `TargetBackend` for the `--backend dotnet` target — reads the package's
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
}
