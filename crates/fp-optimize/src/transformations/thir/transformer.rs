use fp_core::hir::typed as thir;

/// Minimal trait shared by THIR normalisation/materialisation passes.
///
/// For now transformations can simply return the incoming programme unchanged;
/// concrete rewrites will extend this with recursive visiting logic.
pub trait ThirTransform {
    #[allow(unused_variables)]
    fn transform_program(&mut self, program: thir::Program) -> thir::Program {
        program
    }
}

/// No-op transformer handy for tests and scaffolding.
#[derive(Default)]
pub struct IdentityThirTransformer;

impl ThirTransform for IdentityThirTransformer {}
