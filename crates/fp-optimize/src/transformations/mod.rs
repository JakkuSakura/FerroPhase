pub mod ast_to_hir;
pub mod closure_lowering;
pub mod hir_to_mir;
pub mod materialize_intrinsics;
pub mod mir_to_lir;
pub mod normalize_intrinsics;

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
pub use closure_lowering::*;
pub use hir_to_mir::*;
pub use materialize_intrinsics::*;
pub use mir_to_lir::*;
pub use normalize_intrinsics::*;
