use fp_core::ast::{
    Expr, ExprInvoke, ExprIntrinsicCall, ExprIntrinsicContainer, ExprKind, TySlot, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicMaterializer, OpKind, CallKind};

/// Kotlin-specific materializer: converts portable ops to Kotlin idioms.
///
/// Consumes the bare `IntrinsicCall(CallKind::Op(_))` nodes
/// `HirToAstLifter` produces post-typecheck (`program.op_defs`, resolved by
/// real `DefId` — see its doc comment) — the lifter's job stops at
/// classifying a call as a portable op; giving that op real Kotlin shape is
/// this materializer's job. Kotlin models `Option<T>` as a nullable `T?`
/// and `Vec<T>` as `MutableList<T>`, so most of these are either "drop the
/// wrapper" (the value underneath is already the right shape) or a direct
/// literal.
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
        call: &mut ExprIntrinsicCall,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        match call.kind {
            // `Some(x)`/`x.clone()`/`x.unwrap()` — Kotlin's `T?` needs no
            // wrapper for a present value, `.copy()`-vs-share is handled
            // elsewhere by the serializer's own type-driven heuristic, and
            // `!!` is only needed at the *use* site, not construction —
            // all three just become the inner value itself.
            CallKind::Op(OpKind::OptionSome | OpKind::OptionUnwrap | OpKind::Clone) => {
                Ok(Some(match call.args.first() {
                    Some(expr) => expr.clone(),
                    None => Expr::value(Value::Null(Default::default())),
                }))
            }
            // `None` — Kotlin's absent value is simply `null`.
            CallKind::Op(OpKind::OptionNone) => {
                Ok(Some(Expr::value(Value::Null(Default::default()))))
            }
            // `Vec::new()` — an empty `MutableList` literal.
            CallKind::Op(OpKind::VecNew) => Ok(Some(Expr::new(ExprKind::IntrinsicContainer(
                ExprIntrinsicContainer::VecElements { elements: vec![] },
            )))),
            // `x.as_ref()`/`x.iter()`/`x.to_owned()`/`x.as_str()` — Kotlin
            // collections/strings are already shared references with no
            // borrow-checker distinction, so the receiver alone is correct.
            CallKind::Op(OpKind::AsRef | OpKind::Iter | OpKind::ToOwned | OpKind::AsStr) => {
                Ok(Some(match call.args.first() {
                    Some(expr) => expr.clone(),
                    None => Expr::value(Value::Null(Default::default())),
                }))
            }
            _ => Ok(None),
        }
    }
}
