use fp_core::ast::{
    Expr, ExprBinOp, ExprIntrinsicContainer, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprPortableOpCall, ExprSelect, ExprSelectType, Ident, Name, TySlot, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicMaterializer;
use fp_core::ops::BinOpKind;

/// Materializes fp-lang portable operations into Kotlin AST constructs.
///
/// The Rust frontend supplies operation identity via `#[op]`; this target
/// pass selects Kotlin's nullable, collection, and exception equivalents.
pub struct KotlinMaterializer;

impl IntrinsicMaterializer for KotlinMaterializer {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        crate::CAPABILITIES
    }

    fn materialize_portable_op(
        &self,
        call: &mut ExprPortableOpCall,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        let receiver = || {
            call.args
                .first()
                .cloned()
                .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
        };
        match call.op.name() {
            "option_some" | "option_unwrap" | "clone" | "as_ref" | "iter" | "to_owned"
            | "as_str" | "as_deref" => Ok(Some(receiver())),
            "option_none" => Ok(Some(Expr::value(Value::Null(Default::default())))),
            "result_ok" => Ok(Some(match call.args.first() {
                Some(expr)
                    if matches!(expr.kind(), ExprKind::Value(v) if matches!(**v, Value::Unit(_)))
                        || matches!(expr.kind(), ExprKind::Block(b) if b.stmts.is_empty()) =>
                {
                    Expr::name(Name::ident("Unit"))
                }
                Some(expr) => expr.clone(),
                None => Expr::value(Value::Null(Default::default())),
            })),
            "result_err" => {
                let arg = call
                    .args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Expr::value(Value::string(String::new())));
                Ok(Some(invoke_function("error", vec![arg])))
            }
            "vec_new" => Ok(Some(Expr::new(ExprKind::IntrinsicContainer(
                ExprIntrinsicContainer::VecElements { elements: vec![] },
            )))),
            "trim_end" | "trim_start" => Ok(Some(invoke_method(
                receiver(),
                if call.op.name() == "trim_end" {
                    "trimEnd"
                } else {
                    "trimStart"
                },
                Vec::new(),
            ))),
            "is_none" => Ok(Some(Expr::new(ExprKind::BinOp(ExprBinOp {
                span: Default::default(),
                kind: BinOpKind::Eq,
                lhs: Box::new(receiver()),
                rhs: Box::new(Expr::value(Value::Null(Default::default()))),
            })))),
            "position" => {
                let mut args = call.args.drain(..);
                let receiver = args
                    .next()
                    .unwrap_or_else(|| Expr::value(Value::Null(Default::default())));
                Ok(Some(invoke_method(
                    receiver,
                    "indexOfFirst",
                    args.collect(),
                )))
            }
            "split_whitespace" => Ok(Some(invoke_method(
                receiver(),
                "split",
                vec![invoke_function(
                    "Regex",
                    vec![Expr::value(Value::string("\\s+".to_string()))],
                )],
            ))),
            "string_from_utf8_lossy" | "string_from_utf8" => Ok(Some(invoke_function(
                "String",
                vec![
                    receiver(),
                    Expr::new(ExprKind::Select(ExprSelect {
                        span: Default::default(),
                        obj: Box::new(Expr::name(Name::ident("Charsets"))),
                        field: Ident::new("UTF_8"),
                        generic_args: Vec::new(),
                        select: ExprSelectType::Field,
                    })),
                ],
            ))),
            _ => Ok(None),
        }
    }
}

fn invoke_function(name: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident(name)),
        args,
        kwargs: Vec::new(),
    }))
}

fn invoke_method(receiver: Expr, method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprSelect {
            span: Default::default(),
            obj: Box::new(receiver),
            field: Ident::new(method),
            generic_args: Vec::new(),
            select: ExprSelectType::Method,
        }),
        args,
        kwargs: Vec::new(),
    }))
}
