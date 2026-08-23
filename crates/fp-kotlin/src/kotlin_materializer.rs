use fp_core::ast::{Expr, ExprInvoke, TySlot};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicMaterializer;

/// Kotlin-specific materializer.
///
/// `CallKind::Op` was retired, so this no longer receives promoted
/// `IntrinsicCall(CallKind::Op(_))` nodes — portable-op recognition now
/// belongs to target backends directly (temporarily, by bare name), not a
/// shared `IntrinsicMaterializer` hook keyed on a retired enum variant.
pub struct KotlinMaterializer;

impl IntrinsicMaterializer for KotlinMaterializer {
    fn materialize_invoke(
        &self,
        _invoke: &mut ExprInvoke,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_call(
        &self,
        _call: &mut fp_core::ast::ExprIntrinsicCall,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }
}
