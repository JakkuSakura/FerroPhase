pub mod ast_to_hir;
pub mod hir_to_thir;
pub mod mir_to_lir;
pub mod thir_to_mir;

use fp_core::error::Result;

/// Shared interface for lowering steps between IR stages.
pub trait IrTransform<Src, Dest> {
    fn transform(&mut self, source: Src) -> Result<Dest>;
}

pub use ast_to_hir::*;
pub use hir_to_thir::*;
pub use mir_to_lir::*;
pub use thir_to_mir::*;
