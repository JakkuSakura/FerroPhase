use fp_core::ast::typed as tast;
use fp_core::ast::Node;
use fp_core::hir::typed as thir;

/// Resugars a THIR program back into a typed AST (TAST).
///
/// For now this is a structural placeholder that clones the canonical AST
/// emitted by the frontend. Once richer resugaring metadata is available we can
/// rebuild the tree directly from THIR.
#[derive(Default)]
pub struct TastResugar;

impl TastResugar {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn resugar(&mut self, original_ast: &Node, _thir: &thir::Program) -> tast::Program {
        tast::Program::new(original_ast.clone())
    }
}
