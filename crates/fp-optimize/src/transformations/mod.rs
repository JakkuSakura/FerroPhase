pub mod ast_to_hir;
pub mod hir_to_thir;
pub mod mir_to_lir;
pub mod thir;
pub mod thir_to_hir;
pub mod thir_to_mir;
pub mod thir_to_tast;

use fp_core::error::Result;

/// Shared interface for lowering steps between IR stages.
///
/// NOTE: This trait is being enhanced to support error tolerance while maintaining
/// backward compatibility. All transformations now support collecting multiple errors
/// instead of early termination.
pub trait IrTransform<Src, Dest> {
    fn transform(&mut self, source: Src) -> Result<Dest>;
}

pub use ast_to_hir::*;
pub use hir_to_thir::*;
pub use mir_to_lir::*;
pub use thir::*;
pub use thir_to_hir::*;
pub use thir_to_mir::*;
pub use thir_to_tast::*;
