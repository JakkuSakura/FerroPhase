use fp_core::ast::{Expr, ExprInvoke, ExprIntrinsicCall, ExprKind, TySlot, Value};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicMaterializer, OpKind, CallKind};

/// Kotlin-specific materializer: converts portable ops to Kotlin idioms.
/// Runs after the FerroPhase normalizer has converted source patterns to
/// portable ops. Materializes portable ops into Kotlin-specific forms
/// before the serializer.
pub struct KotlinMaterializer;

impl IntrinsicMaterializer for KotlinMaterializer {
    fn materialize_invoke(
        &self,
        invoke: &mut ExprInvoke,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        // Materialize `IntrinsicCall(OptionUnwrap, [x])` → `x!!` 
        // This is handled by the serializer's IntrinsicCall case now.
        // The materializer's job is pre-serializer rewrites.
        // For now: just handle method ops that need Kotlin-specific rewriting.
        Ok(None)
    }

    fn materialize_call(
        &self,
        call: &mut ExprIntrinsicCall,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        // Convert portable intrinsic calls to Kotlin-specific expressions
        match call.kind {
            CallKind::Op(OpKind::OptionUnwrap) => {
                if let Some(expr) = call.args.first().cloned() {
                    Ok(Some(Expr::from_parts(0, None, None,
                        expr.kind().clone())))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}
