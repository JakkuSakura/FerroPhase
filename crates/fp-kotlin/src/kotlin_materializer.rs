use fp_core::ast::{
    Expr, ExprInvoke, ExprInvokeTarget, ExprIntrinsicCall, ExprIntrinsicContainer, ExprKind,
    Name, TySlot, Value,
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
            // `Ok(x)` — same "unwrap the payload" treatment as `Some(x)`.
            // The enclosing function's own `Result<T, E>` return type is
            // separately unwrapped to plain `T` (`kotlin_type_from_ty`),
            // so a bare value in that position is already correct — except
            // `Ok(())` specifically (`fmt::Result`'s only real value,
            // `Result<T, E>` → `T` having mapped `()` → `Unit`): the raw
            // `Value::Unit` payload otherwise renders as `null` (its
            // general-purpose "no value" rendering, shared with
            // `Option::None`), which isn't a valid `Unit` in Kotlin —
            // `Unit` is the one real spelling for that.
            CallKind::Op(OpKind::ResultOk) => Ok(Some(match call.args.first() {
                Some(expr) if matches!(expr.kind(), ExprKind::Value(v) if matches!(**v, Value::Unit(_))) => {
                    Expr::name(Name::ident("Unit"))
                }
                Some(expr) => expr.clone(),
                None => Expr::value(Value::Null(Default::default())),
            })),
            // `Err(e)` — Kotlin's `error(message: Any): Nothing` throws
            // `IllegalStateException` and is valid in any expression
            // position (`Nothing` is a subtype of everything), the same
            // role `Err` plays as a `Result`-typed value in Rust: the
            // enclosing (now plain-`T`-returning) function simply never
            // returns normally on this path, and the exception propagates
            // up through every caller exactly like `?` already does
            // (`ExprKind::Try` renders as its inner expression only).
            CallKind::Op(OpKind::ResultErr) => {
                let arg = call.args.first().cloned().unwrap_or_else(|| {
                    Expr::value(Value::string(String::new()))
                });
                Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Function(Name::ident("error")),
                    args: vec![arg],
                    kwargs: Vec::new(),
                }))))
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
