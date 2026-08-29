use fp_core::ast::{
    Expr, ExprBinOp, ExprClosure, ExprIntrinsicCall, ExprIntrinsicContainer, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprPortableOpCall, ExprSelect, ExprSelectType, Ident, Name, Path,
    TySlot, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{CallKind, IntrinsicMaterializer};
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
                &["Result"],
                "success",
                vec![result_constructor_arg(call)],
            ))),
            "result_err" => Ok(Some(invoke_static_method(
                &["Result"],
                "failure",
                vec![result_constructor_arg(call)],
            ))),
            "result_map" => Ok(Some(invoke_method(
                receiver(),
                "map",
                portable_op_args_after_receiver(call),
            ))),
            "result_map_err" => Ok(Some(runtime_method(
                "mapError",
                std::iter::once(receiver())
                    .chain(portable_op_args_after_receiver(call))
                    .collect(),
            ))),
            "result_is_ok" => Ok(Some(invoke_method(receiver(), "isSuccess", Vec::new()))),
            "result_is_err" => Ok(Some(invoke_method(receiver(), "isFailure", Vec::new()))),
            "result_ok_value" => Ok(Some(invoke_method(receiver(), "getOrNull", Vec::new()))),
            "result_err_value" => Ok(Some(invoke_method(
                receiver(),
                "exceptionOrNull",
                Vec::new(),
            ))),
            "result_unwrap_or" => Ok(Some(invoke_method(
                receiver(),
                "getOrDefault",
                portable_op_args_after_receiver(call),
            ))),
            "vec_new" => Ok(Some(Expr::new(ExprKind::IntrinsicContainer(
                ExprIntrinsicContainer::VecElements { elements: vec![] },
            )))),
            "vec_push" => Ok(Some(invoke_method(
                receiver(),
                "add",
                portable_op_args_after_receiver(call),
            ))),
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

    fn materialize_call(&self, call: &mut ExprIntrinsicCall, _ty: &TySlot) -> Result<Option<Expr>> {
        let args = call.args.clone();
        let replacement = match call.kind {
            CallKind::FsReadToString => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "readString",
                args,
            )),
            CallKind::FsWriteString => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "write",
                args,
            )),
            CallKind::FsExists => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "exists",
                args,
            )),
            CallKind::FsCreateDirAll => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "createDirectories",
                args,
            )),
            CallKind::FsRemoveFile => run_catching(invoke_static_method(
                &["java", "nio", "file", "Files"],
                "delete",
                args,
            )),
            CallKind::FsRemoveDirAll => run_catching(runtime_method("deleteRecursively", args)),
            _ => return Ok(None),
        };
        Ok(Some(replacement))
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

fn runtime_method(method: &str, args: Vec<Expr>) -> Expr {
    invoke_static_method(&["RustKotlinRuntime"], method, args)
}

fn invoke_static_method(receiver: &[&str], method: &str, args: Vec<Expr>) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Method(ExprSelect {
            span: Default::default(),
            obj: Box::new(Expr::name(Name::path(Path::plain(
                receiver
                    .iter()
                    .map(|segment| Ident::new(*segment))
                    .collect(),
            )))),
            field: Ident::new(method),
            generic_args: Vec::new(),
            select: ExprSelectType::Method,
        }),
        args,
        kwargs: Vec::new(),
    }))
}

fn run_catching(body: Expr) -> Expr {
    Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Default::default(),
        target: ExprInvokeTarget::Function(Name::ident("runCatching")),
        args: vec![Expr::new(ExprKind::Closure(ExprClosure {
            span: Default::default(),
            params: Vec::new(),
            ret_ty: None,
            movability: None,
            body: Box::new(body),
        }))],
        kwargs: Vec::new(),
    }))
}

fn result_constructor_arg(call: &ExprPortableOpCall) -> Expr {
    call.args
        .first()
        .cloned()
        .unwrap_or_else(|| Expr::value(Value::Null(Default::default())))
}

fn portable_op_args_after_receiver(call: &ExprPortableOpCall) -> Vec<Expr> {
    call.args.iter().skip(1).cloned().collect()
}

#[cfg(test)]
mod tests {
    use fp_core::ast::{ExprIntrinsicCall, ExprKind, ExprPortableOpCall, Value};
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

    #[test]
    fn materializes_resolved_filesystem_calls_to_run_catching() {
        let mut call = ExprIntrinsicCall {
            span: Default::default(),
            kind: CallKind::FsReadToString,
            args: vec![Expr::name(Name::ident("path"))],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_call(&mut call, &None)
            .expect("materialize filesystem call")
            .expect("filesystem replacement");
        assert_eq!(render_invoke_name(&materialized), "runCatching");
    }

    #[test]
    fn materializes_result_error_mapping_through_the_runtime() {
        let registry = PortableOpRegistry::builtin();
        let mut call = ExprPortableOpCall {
            span: Default::default(),
            op: registry
                .resolve("result_map_err")
                .expect("registered result_map_err"),
            args: vec![
                Expr::name(Name::ident("result")),
                Expr::name(Name::ident("convert_error")),
            ],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize result map_err")
            .expect("result map_err replacement");
        assert_eq!(
            render_invoke_name(&materialized),
            "RustKotlinRuntime.mapError"
        );
    }

    #[test]
    fn materializes_vec_push_as_mutable_list_add() {
        let registry = PortableOpRegistry::builtin();
        let mut call = ExprPortableOpCall {
            span: Default::default(),
            op: registry.resolve("vec_push").expect("registered vec_push"),
            args: vec![Expr::name(Name::ident("items")), Expr::value(Value::int(1))],
            kwargs: Vec::new(),
        };

        let materialized = KotlinMaterializer
            .materialize_portable_op(&mut call, &None)
            .expect("materialize vec push")
            .expect("vec push replacement");
        assert_eq!(render_invoke_name(&materialized), "items.add");
    }

    fn render_invoke_name(expr: &Expr) -> String {
        let ExprKind::Invoke(invoke) = expr.kind() else {
            panic!("expected invocation");
        };
        match &invoke.target {
            ExprInvokeTarget::Function(Name::Ident(name)) => return name.name.clone(),
            ExprInvokeTarget::Method(select) => {
                let ExprKind::Name(Name::Ident(receiver)) = select.obj.kind() else {
                    panic!("expected static receiver");
                };
                format!("{}.{}", receiver.name, select.field.name)
            }
            _ => panic!("expected function or static method invocation"),
        }
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
