use fp_core::ast::{Expr, ExprKind, ExprMacro};
use fp_core::config;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::intrinsics::{IntrinsicNormalizer, NormalizeOutcome};

mod macro_lowering;

type Diagnostics<'a> = Option<&'a DiagnosticManager>;

const NORMALIZATION_CONTEXT: &str = "normalization";

fn lossy_normalization_mode() -> bool {
    config::lossy_mode()
}

pub fn lower_macro_for_ast(
    macro_expr: &ExprMacro,
    diagnostics: Option<&fp_core::diagnostics::DiagnosticManager>,
) -> Expr {
    macro_lowering::lower_macro_expression(macro_expr, diagnostics)
}

/// Frontend-provided normalizer that plugs language-specific macro lowering
/// into the shared intrinsic normalization pass.
#[derive(Debug, Default, Clone, Copy)]
pub struct RustIntrinsicNormalizer;

impl IntrinsicNormalizer for RustIntrinsicNormalizer {
    fn normalize_macro(&self, expr: Expr) -> fp_core::Result<NormalizeOutcome<Expr>> {
        let (ty, kind) = expr.into_parts();
        match kind {
            ExprKind::Macro(macro_expr) => Ok(NormalizeOutcome::Normalized(
                macro_lowering::lower_macro_expression(&macro_expr, None).with_ty_slot(ty),
            )),
            other => Ok(NormalizeOutcome::Ignored(Expr::from_parts(ty, other))),
        }
    }
}
