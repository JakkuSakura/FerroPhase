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
        source
            .modules
            .clone()
            .into_iter()
            .map(|module| {
                let rel_path = module.relative_path();
                let file = File {
                    path: std::path::PathBuf::from(&rel_path),
                    attrs: Vec::new(),
                    collected_items: Vec::new(),
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
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
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
