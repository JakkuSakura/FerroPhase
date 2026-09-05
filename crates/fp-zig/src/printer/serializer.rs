use fp_core::ast::File;

use super::ZigEmitter;

/// Public entry point used by the CLI target emitter.
pub struct ZigSerializer;

impl ZigSerializer {
    pub fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = ZigEmitter::new();
        emitter.emit_file(file)?;
        Ok(emitter.finish())
    }

    /// Serializes a package into one Zig source file per module.
    /// Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::ast::package::AstPackage,
    ) -> fp_core::error::Result<Vec<(String, String)>> {
        std::iter::once(source.module.clone())
            .map(|module| {
                let rel_path = module.relative_path();
                let file = File {
                    path: std::path::PathBuf::from(&rel_path),
                    attrs: Vec::new(),
                    items: module.items,
                };
                let code = self.serialize_file(&file)?;
                Ok((rel_path, code))
            })
            .collect()
    }
}

pub struct ZigBackend {
    config: fp_core::backend::BackendConfig,
}

impl ZigBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self { config }
    }
}

impl fp_core::backend::TargetBackend for ZigBackend {
    fn plan(&self) -> fp_core::backend::BackendPlan { fp_core::backend::BackendPlan::transpile() }

    fn emit(&self, context: &fp_core::backend::BackendContext) -> fp_core::error::Result<()> {
        for package_id in &context.emitted_packages {
            let mir = context.mir_program.package(package_id).map(|package| {
                let package = package.borrow();
                let mut unit = fp_core::mir::MirCodeUnit::new();
                unit.items.extend(package.items().cloned());
                unit.bodies.extend(package.bodies().map(|(id, body)| (*id, body.clone())));
                unit
            }).unwrap_or_else(fp_core::mir::MirCodeUnit::new);
            let lir = context.lir_program.merged_blob_for_package(package_id).ok();
            self.emit_package(context.ast_program.as_ref(), package_id, &mir, lir.as_ref())?;
        }
        Ok(())
    }

    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }



}

impl ZigBackend {

    fn emit_package(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> fp_core::error::Result<()> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = ZigSerializer.serialize_package(package)?;
        let writer =
            fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            let rel = if rel_path.contains('.') {
                rel_path
            } else {
                format!("{rel_path}.zig")
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }
}

