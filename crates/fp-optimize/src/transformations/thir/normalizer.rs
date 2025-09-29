use fp_core::hir::typed as thir;

use super::transformer::{IdentityThirTransformer, ThirTransform};

/// Produces a canonical, backend-agnostic THIR snapshot.
///
/// At the moment the AST→HIR lowering already emits canonical helpers, so this
/// transformer is effectively a marker pass. Keeping it in place makes it easy
/// to extend normalisation without disturbing the rest of the pipeline.
#[derive(Default)]
pub struct SymbolicThirNormalizer {
    inner: IdentityThirTransformer,
}

impl SymbolicThirNormalizer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn normalize(&mut self, program: thir::Program) -> thir::Program {
        self.inner.transform_program(program)
    }
}
