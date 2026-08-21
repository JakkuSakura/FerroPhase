pub mod config;
mod emit;
pub mod package;
mod parse;

use crate::config::{GoAsmConfig, GoAsmTarget};
use fp_core::ast::path::QualifiedPath;
use fp_core::error::Result;
use fp_core::lir::LirProgram;
use std::path::{Path, PathBuf};

pub use parse::parse_program;

pub struct GoAsmEmitter {
    config: GoAsmConfig,
    /// See `fp_native::NativeEmitter::module_path`'s doc comment — same
    /// role, `None` for direct `emit`/`compile` callers.
    module_path: Option<QualifiedPath>,
}

impl GoAsmEmitter {
    pub fn new(config: GoAsmConfig) -> Self {
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
        let target = self
            .config
            .target
            .unwrap_or_else(|| GoAsmTarget::resolve(self.config.target_triple.as_deref()));
        let text = emit::emit_program(&lir_program, target)?;
        std::fs::write(&self.config.output_path, text).map_err(fp_core::error::Error::from)?;
        Ok(self.config.output_path.clone())
    }
}

impl fp_core::backend::TargetBackend for GoAsmEmitter {
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
