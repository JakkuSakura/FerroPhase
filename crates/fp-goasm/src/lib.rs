pub mod config;
mod emit;
pub mod package;
mod parse;

use crate::config::{GoAsmConfig, GoAsmTarget};
use fp_core::error::Result;
use fp_core::lir::LirBlob;
use std::path::{Path, PathBuf};

pub use parse::parse_program;

pub struct GoAsmEmitter {
    config: GoAsmConfig,
}

impl GoAsmEmitter {
    pub fn new(config: GoAsmConfig) -> Self {
        Self { config }
    }

    pub fn emit(&self, lir_program: LirBlob, source_file: Option<&Path>) -> Result<PathBuf> {
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
    fn plan(&self) -> fp_core::backend::BackendPlan { fp_core::backend::BackendPlan::native() }

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

impl GoAsmEmitter {

    fn emit_package(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> Result<()> {
        let _ = mir;
        let lir = lir
            .ok_or_else(|| {
                fp_core::error::Error::from(format!("package `{package_id}` has no compiled LIR"))
            })?
            .clone();
        self.emit(lir, None)?;
        Ok(())
    }
}

