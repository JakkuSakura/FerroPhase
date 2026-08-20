use fp_core::ast::{
    Expr, ExprBinOp, ExprInvoke, ExprInvokeTarget, ExprIntrinsicCall, ExprIntrinsicContainer,
    ExprKind, ExprSelect, ExprSelectType, Ident, Name, TySlot, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{CallKind, IntrinsicMaterializer};
use fp_core::ops::BinOpKind;

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
        let CallKind::Op(op) = &call.kind else {
            return Ok(None);
        };
        match op.name() {
            // `Some(x)`/`x.clone()`/`x.unwrap()` — Kotlin's `T?` needs no
            // wrapper for a present value, `.copy()`-vs-share is handled
            // elsewhere by the serializer's own type-driven heuristic, and
            // `!!` is only needed at the *use* site, not construction —
            // all three just become the inner value itself.
            "option_some" | "option_unwrap" | "clone" => {
                Ok(Some(match call.args.first() {
                    Some(expr) => expr.clone(),
                    None => Expr::value(Value::Null(Default::default())),
                }))
            }
            // `None` — Kotlin's absent value is simply `null`.
            "option_none" => {
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
            "result_ok" => Ok(Some(match call.args.first() {
                // `()` doesn't lower to a `Value::Unit` literal node — HIR
                // represents it as an empty block (`{ }`, evaluating to
                // unit), so that's the shape actually reaching here for
                // `Ok(())`. Recognize both: `Value::Unit` for whichever
                // callers do construct a literal one directly, and an
                // empty `ExprKind::Block` for the shape a real `()`
                // argument actually takes. Missing the latter left `Ok(())`
                // rendering as its raw (empty) block argument instead of
                // `Unit`, which the Kotlin serializer's block-hoisting path
                // then emits as a broken, contentless `run { }`.
                Some(expr)
                    if matches!(expr.kind(), ExprKind::Value(v) if matches!(**v, Value::Unit(_)))
                        || matches!(expr.kind(), ExprKind::Block(b) if b.stmts.is_empty()) =>
                {
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
            "result_err" => {
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
            "vec_new" => Ok(Some(Expr::new(ExprKind::IntrinsicContainer(
                ExprIntrinsicContainer::VecElements { elements: vec![] },
            )))),
            // `x.as_ref()`/`x.iter()`/`x.to_owned()`/`x.as_str()` — Kotlin
            // collections/strings are already shared references with no
            // borrow-checker distinction, so the receiver alone is correct.
            "as_ref" | "iter" | "to_owned" | "as_str" => {
                Ok(Some(match call.args.first() {
                    Some(expr) => expr.clone(),
                    None => Expr::value(Value::Null(Default::default())),
                }))
            }
            // `x.trim_end()`/`x.trim_start()` — Kotlin's `trimEnd()`/`trimStart()`.
            "trim_end" | "trim_start" => {
                let Some(receiver) = call.args.first().cloned() else {
                    return Ok(Some(Expr::value(Value::Null(Default::default()))));
                };
                let method = if op.name() == "trim_end" {
                    "trimEnd"
                } else {
                    "trimStart"
                };
                Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Method(ExprSelect {
                        span: Default::default(),
                        obj: Box::new(receiver),
                        field: Ident::new(method),
                        select: ExprSelectType::Method,
                    }),
                    args: vec![],
                    kwargs: Vec::new(),
                }))))
            }
            // `Option<T>::as_deref()` — same "no borrow-checker distinction
            // in Kotlin" treatment as `as_ref`.
            "as_deref" => Ok(Some(match call.args.first() {
                Some(expr) => expr.clone(),
                None => Expr::value(Value::Null(Default::default())),
            })),
            // `x.is_none()` — Kotlin's nullable-equality check.
            "is_none" => {
                let Some(receiver) = call.args.first().cloned() else {
                    return Ok(Some(Expr::value(Value::bool(true))));
                };
                Ok(Some(Expr::new(ExprKind::BinOp(ExprBinOp {
                    span: Default::default(),
                    kind: BinOpKind::Eq,
                    lhs: Box::new(receiver),
                    rhs: Box::new(Expr::value(Value::Null(Default::default()))),
                }))))
            }
            // `x.position(predicate)` — Kotlin's `.indexOfFirst(predicate)`.
            // Returns `-1` rather than Rust's absent-value `None`, unlike
            // this op's own `Option<usize>` result type — an exact `Option`
            // translation would need to wrap the result in a null check,
            // which needs the call's own result type (not available here);
            // left as the direct `Int` value for now.
            "position" => {
                let mut args = call.args.drain(..);
                let Some(receiver) = args.next() else {
                    return Ok(Some(Expr::value(Value::Null(Default::default()))));
                };
                let predicate: Vec<Expr> = args.collect();
                Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Method(ExprSelect {
                        span: Default::default(),
                        obj: Box::new(receiver),
                        field: Ident::new("indexOfFirst"),
                        select: ExprSelectType::Method,
                    }),
                    args: predicate,
                    kwargs: Vec::new(),
                }))))
            }
            // `x.split_whitespace()` — Kotlin has no direct equivalent;
            // approximate with a whitespace-regex split (empty runs aren't
            // filtered out, unlike Rust's version, since that needs a
            // trailing-lambda `.filter { }` this AST shape can't build
            // without a closure to splice in).
            "split_whitespace" => {
                let Some(receiver) = call.args.first().cloned() else {
                    return Ok(Some(Expr::value(Value::Null(Default::default()))));
                };
                let regex = Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Function(Name::ident("Regex")),
                    args: vec![Expr::value(Value::string("\\s+".to_string()))],
                    kwargs: Vec::new(),
                }));
                Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Method(ExprSelect {
                        span: Default::default(),
                        obj: Box::new(receiver),
                        field: Ident::new("split"),
                        select: ExprSelectType::Method,
                    }),
                    args: vec![regex],
                    kwargs: Vec::new(),
                }))))
            }
            // `String::from_utf8_lossy(bytes)`/`String::from_utf8(bytes)` —
            // Kotlin's `String(bytes, Charsets.UTF_8)` constructor.
            "string_from_utf8_lossy" | "string_from_utf8" => {
                let arg = call.args.first().cloned().unwrap_or_else(|| {
                    Expr::value(Value::Null(Default::default()))
                });
                Ok(Some(Expr::new(ExprKind::Invoke(ExprInvoke {
                    span: Default::default(),
                    target: ExprInvokeTarget::Function(Name::ident("String")),
                    args: vec![arg, Expr::new(ExprKind::Select(ExprSelect {
                        span: Default::default(),
                        obj: Box::new(Expr::name(Name::ident("Charsets"))),
                        field: Ident::new("UTF_8"),
                        select: ExprSelectType::Field,
                    }))],
                    kwargs: Vec::new(),
                }))))
            }
            _ => Ok(None),
        }
    }
}
