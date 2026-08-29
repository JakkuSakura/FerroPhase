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
            "result_ok" => Ok(Some(invoke_static_method(
                "Result",
                "success",
                vec![result_constructor_arg(call)],
            ))),
            "result_err" => Ok(Some(invoke_static_method(
                "Result",
                "failure",
                vec![result_constructor_arg(call)],
            ))),
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

fn invoke_static_method(receiver: &str, method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprSelect {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::ident(receiver))),
            field: Ident::new(method),
            generic_args: Vec::new(),
            select: ExprSelectType::Method,
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn result_constructor_arg(call: &ExprPortableOpCall) -> Expr {
    call.args
        .first()
        .cloned()
        .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
}

#[cfg(test)]
mod tests {
    use fp_core::ast::{ExprKind, ExprPortableOpCall, Value};
    use fp_core::intrinsics::PortableOpRegistry;

    use super::*;

    #[test]
    fn materializes_result_constructors_without_erasing_them() {
        let registry = PortableOpRegistry::builtin();
        let mut ok = ExprPortableOpCall {
            span: Default::default(),
            op: registry.resolve("result_ok").expect("registered result_ok"),
            args: vec![Expr::value(Value::string("value".to_string()))],
            kwargs: Vec::new(),
        };
        let mut err = ExprPortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_err")
                .expect("registered result_err"),
            args: vec![Expr::value(Value::string("failure".to_string()))],
            kwargs: Vec::new(),
        };

        let materializer = KotlinMaterializer;
        let ok = materializer
            .materialize_portable_op(&mut ok, &None)
            .expect("materialize Ok")
            .expect("Ok replacement");
        let err = materializer
            .materialize_portable_op(&mut err, &None)
            .expect("materialize Err")
            .expect("Err replacement");

        assert!(matches!(ok.kind(), ExprKind::Invoke(_)));
        assert!(matches!(err.kind(), ExprKind::Invoke(_)));
        assert_eq!(render_invoke_name(&ok), "Result.success");
        assert_eq!(render_invoke_name(&err), "Result.failure");
    }

    fn render_invoke_name(expr: &Expr) -> String {
        let ExprKind::Invoke(invoke) = expr.kind() else {
            panic!("expected invocation");
        };
        let ExprInvokeTarget::Method(select) = &invoke.target else {
            panic!("expected static method invocation");
        };
        let ExprKind::Name(Name::Ident(receiver)) = select.obj.kind() else {
            panic!("expected static receiver");
        };
        format!("{}.{}", receiver.name, select.field.name)
    }
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
