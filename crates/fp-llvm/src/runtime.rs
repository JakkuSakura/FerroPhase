use fp_core::ast::{
    DecimalType, Expr, ExprFormatString, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind,
    FormatArgRef, FormatTemplatePart, Ty, TySlot, TypeInt, TypePrimitive, TypeUnit, Value,
};
use fp_core::ast::{Ident, Locator};
use fp_core::error::Result;
use fp_core::intrinsics::runtime::{
    ensure_function_decl, FunctionDecl, ParamSpec, RuntimeIntrinsicStrategy,
};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use tracing::info;

/// Backend strategy that lowers FerroPhase print intrinsics to `printf` calls for LLVM.
pub struct LlvmRuntimeIntrinsicStrategy;

impl RuntimeIntrinsicStrategy for LlvmRuntimeIntrinsicStrategy {
    fn prepare_file(&self, file: &mut fp_core::ast::File) {
        ensure_function_decl(
            file,
            FunctionDecl::new(
                "printf",
                vec![ParamSpec::string("fmt"), ParamSpec::any_tuple("args")],
                Ty::Unit(TypeUnit),
            ),
        );
    }

    fn rewrite_intrinsic(
        &self,
        call: &ExprIntrinsicCall,
        expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        if matches!(
            call.kind,
            IntrinsicCallKind::Print | IntrinsicCallKind::Println
        ) {
            Ok(Some(build_printf_invoke(expr_ty.clone(), call.clone())?))
        } else {
            Ok(None)
        }
    }
}

fn build_printf_invoke(expr_ty: TySlot, call: ExprIntrinsicCall) -> Result<Expr> {
    let newline = matches!(call.kind, IntrinsicCallKind::Println);
    let payload = match call.payload {
        IntrinsicCallPayload::Format { template } => template,
        IntrinsicCallPayload::Args { .. } => {
            return Err(fp_core::error::Error::from(
                "printf lowering requires format payload; found positional args".to_string(),
            ));
        }
    };

    if !payload.kwargs.is_empty() {
        return Err(fp_core::error::Error::from(
            "named arguments are not supported in runtime printf lowering".to_string(),
        ));
    }

    let printf_format = build_printf_format(&payload, newline)?;
    let mut args = payload.args;

    let mut invoke_args = Vec::with_capacity(args.len() + 1);
    invoke_args.push(make_string_literal_expr(printf_format));
    invoke_args.append(&mut args);

    let target = ExprInvokeTarget::Function(Locator::ident(Ident::new("printf")));
    let invoke = ExprKind::Invoke(ExprInvoke {
        target,
        args: invoke_args,
    });

    Ok(Expr::with_ty(invoke, expr_ty))
}

fn make_string_literal_expr(literal: String) -> Expr {
    let mut expr = Expr::value(Value::string(literal));
    expr.set_ty(Ty::Primitive(TypePrimitive::String));
    expr
}

fn build_printf_format(template: &ExprFormatString, newline: bool) -> Result<String> {
    info!("Building printf format for template: {:?}", template);
    let mut result = String::new();
    let mut implicit_index = 0usize;

    for part in &template.parts {
        match part {
            FormatTemplatePart::Literal(text) => result.push_str(text),
            FormatTemplatePart::Placeholder(placeholder) => {
                let arg_index = match &placeholder.arg_ref {
                    FormatArgRef::Implicit => {
                        let current = implicit_index;
                        implicit_index += 1;
                        current
                    }
                    FormatArgRef::Positional(index) => *index,
                    FormatArgRef::Named(name) => {
                        return Err(fp_core::error::Error::from(format!(
                            "named argument '{name}' is not supported in printf lowering"
                        )));
                    }
                };

                let arg = template.args.get(arg_index).ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "format placeholder references missing argument at index {arg_index}"
                    ))
                })?;

                let spec = if let Some(explicit) = placeholder.format_spec.clone() {
                    if !explicit.trim().starts_with('%') {
                        return Err(fp_core::error::Error::from(format!(
                            "format spec '{explicit}' is not a printf spec (expected leading '%')"
                        )));
                    }
                    explicit
                } else {
                    infer_printf_spec(arg.ty())?
                };

                result.push_str(&spec);
            }
        }
    }

    if newline {
        result.push('\n');
    }

    Ok(result)
}

fn infer_printf_spec(ty: Option<&Ty>) -> Result<String> {
    let ty = ty.ok_or_else(|| {
        fp_core::error::Error::from("missing type information for printf argument".to_string())
    })?;
    let spec = match ty {
        Ty::Primitive(TypePrimitive::Int(TypeInt::I8))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::I16))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::I32))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::I64))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::U8))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::U16))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::U32))
        | Ty::Primitive(TypePrimitive::Int(TypeInt::U64)) => "%d".to_string(),
        Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32))
        | Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)) => "%f".to_string(),
        Ty::Primitive(TypePrimitive::Bool) => "%d".to_string(),
        Ty::Primitive(TypePrimitive::Char) => "%c".to_string(),
        Ty::Primitive(TypePrimitive::String) => "%s".to_string(),
        Ty::Reference(reference) => infer_printf_spec(Some(&reference.ty))?,
        Ty::Any(_) => "%s".to_string(),
        Ty::Struct(struct_ty) => {
            return Err(fp_core::error::Error::from(format!(
                "struct '{}' does not have a printf representation",
                struct_ty.name
            )));
        }
        Ty::Structural(_) => {
            return Err(fp_core::error::Error::from(
                "structural literals are not supported in printf lowering".to_string(),
            ));
        }
        Ty::Enum(enum_ty) => {
            return Err(fp_core::error::Error::from(format!(
                "enum '{}' does not have a printf representation",
                enum_ty.name
            )));
        }
        Ty::Tuple(_) | Ty::Vec(_) | Ty::Slice(_) => {
            return Err(fp_core::error::Error::from(
                "aggregate values are not supported in printf lowering".to_string(),
            ));
        }
        Ty::Function(_) => {
            return Err(fp_core::error::Error::from(
                "cannot print function values with printf".to_string(),
            ));
        }
        Ty::Unit(_) => {
            return Err(fp_core::error::Error::from(
                "cannot format unit type with printf".to_string(),
            ));
        }
        other => {
            return Err(fp_core::error::Error::from(format!(
                "printf argument type could not be inferred: {:?}",
                other
            )));
        }
    };
    Ok(spec)
}
