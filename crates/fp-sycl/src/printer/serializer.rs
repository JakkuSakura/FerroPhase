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
        source: &fp_core::package::PackageSource,
    ) -> fp_core::error::Result<Vec<(String, String)>> {
        fp_core::package::split_package_into_modules(source)
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
