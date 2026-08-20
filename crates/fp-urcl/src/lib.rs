mod emit;
mod parse;

use fp_core::ast::path::QualifiedPath;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

pub use parse::parse_program;

#[derive(Debug, Clone)]
pub struct UrclConfig {
    pub output_path: PathBuf,
}

impl UrclConfig {
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
        }
    }
}

pub struct UrclEmitter {
    config: UrclConfig,
    /// See `fp_native::NativeEmitter::module_path`'s doc comment — same
    /// role, `None` for direct `emit`/`compile` callers.
    module_path: Option<QualifiedPath>,
}

impl UrclEmitter {
    pub fn new(config: UrclConfig) -> Self {
        Self {
            config,
            module_path: None,
        }
    }

    pub fn with_module_path(mut self, module_path: QualifiedPath) -> Self {
        self.module_path = Some(module_path);
        self
    }

    pub fn emit(&self, lir_program: LirProgram, source_file: Option<&Path>) -> Result<PathBuf> {
        let _ = source_file;
        if let Some(parent) = self.config.output_path.parent() {
            std::fs::create_dir_all(parent).map_err(fp_core::error::Error::from)?;
        }
        let text = emit::emit_program(&lir_program)?;
        std::fs::write(&self.config.output_path, text).map_err(fp_core::error::Error::from)?;
        Ok(self.config.output_path.clone())
    }
}

impl fp_core::backend::TargetBackend for UrclEmitter {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> Result<()> {
        let entrypoint = self
            .module_path
            .as_ref()
            .map(|module_path| (module_path, "main", "main"));
        let lir = workspace.merged_lir_program(package_id, entrypoint)?;
        self.emit(lir, None)?;
        Ok(())
    }
}
