use fp_core::ast::File;

use super::SyclEmitter;

/// Public entry point used by the CLI target emitter.
pub struct SyclSerializer;

impl SyclSerializer {
    pub fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut emitter = SyclEmitter::new();
        emitter.emit_file(file)?;
        Ok(emitter.finish())
    }

    /// Serializes a package into one SYCL source file per module.
    /// Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::ast::package::PackageSource,
    ) -> fp_core::error::Result<Vec<(String, String)>> {
        fp_core::ast::package::split_package_into_modules(source)
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

pub struct SyclBackend {
    config: fp_core::backend::BackendConfig,
}

impl SyclBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self { config }
    }
}

impl fp_core::backend::TargetBackend for SyclBackend {
    fn emit_package_artifact(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::ast::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = SyclSerializer.serialize_package(package)?;
        let writer = fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            let rel = if rel_path.contains('.') {
                rel_path
            } else {
                format!("{rel_path}.cpp")
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }
}
