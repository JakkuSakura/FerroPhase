use fp_core::ast::{
    DecimalType, Expr, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, FormatArgRef,
    FormatTemplatePart, FunctionParam, Ty, TySlot, TypeAny, TypeInt, TypePrimitive, TypeUnit,
    Value,
};
use fp_core::ast::{Ident, Locator};
use fp_core::error::Result;
use fp_core::intrinsics::{ensure_function_decl, make_function_decl, IntrinsicMaterializer};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

/// Backend strategy that lowers FerroPhase print intrinsics to `printf` calls for LLVM.
pub struct LlvmRuntimeIntrinsicMaterializer;

impl IntrinsicMaterializer for LlvmRuntimeIntrinsicMaterializer {
    fn prepare_file(&self, file: &mut fp_core::ast::File) {
        let mut fmt_param =
            FunctionParam::new(Ident::new("fmt"), Ty::Primitive(TypePrimitive::String));
        fmt_param.ty_annotation = Some(fmt_param.ty.clone());

        let mut args_param = FunctionParam::new(Ident::new("args"), Ty::Any(TypeAny));
        args_param.ty_annotation = Some(args_param.ty.clone());
        args_param.as_tuple = true;

        let decl = make_function_decl("printf", vec![fmt_param, args_param], Ty::Unit(TypeUnit));
        ensure_function_decl(file, decl);
    }

    fn materialize_call(
        &self,
        call: &mut ExprIntrinsicCall,
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

    let fp_core::ast::ExprFormatString { parts, args, .. } = payload;
    let mut args = args;
    let printf_format = build_printf_format(&parts, &mut args, newline)?;

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

fn build_printf_format(
    parts: &[FormatTemplatePart],
    args: &mut [Expr],
    newline: bool,
) -> Result<String> {
    // info!("Building printf format for template: {:?}", template);
    let mut result = String::new();
    let mut implicit_index = 0usize;

    for part in parts {
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

                let arg = args.get_mut(arg_index).ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "format placeholder references missing argument at index {arg_index}"
                    ))
                })?;

                let (spec, replacement) = if let Some(explicit) = placeholder.format_spec.clone()
                {
                    let trimmed = explicit.trim();
                    if trimmed.starts_with('%') {
                        (explicit, None)
                    } else {
                        (format!("%{}", explicit), None)
                    }
                } else {
                    infer_printf_spec_with_replacement(arg.ty())?
                };

                if let Some(replacement) = replacement {
                    *arg = replacement;
                }

                result.push_str(&spec);
            }
        }
    }

    if newline {
        result.push('\n');
    }

    Ok(result)
}

fn infer_printf_spec_with_replacement(ty: Option<&Ty>) -> Result<(String, Option<Expr>)> {
    let ty = ty.ok_or_else(|| {
        fp_core::error::Error::from("missing type information for printf argument".to_string())
    })?;
    let spec = match ty {
        Ty::Primitive(TypePrimitive::Int(TypeInt::I8)) => "%hhd".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::I16)) => "%hd".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::I32)) => "%d".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::I64)) => "%lld".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::U8)) => "%hhu".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::U16)) => "%hu".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::U32)) => "%u".to_string(),
        Ty::Primitive(TypePrimitive::Int(TypeInt::U64)) => "%llu".to_string(),
        Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32))
        | Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)) => "%f".to_string(),
        Ty::Primitive(TypePrimitive::Bool) => "%d".to_string(),
        Ty::Primitive(TypePrimitive::Char) => "%c".to_string(),
        Ty::Primitive(TypePrimitive::String) => "%s".to_string(),
        Ty::Reference(reference) => infer_printf_spec_with_replacement(Some(&reference.ty))?.0,
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
        Ty::Unit(_) | Ty::Nothing(_) | Ty::Unknown(_) => "%s".to_string(),
        other => {
            return Err(fp_core::error::Error::from(format!(
                "printf argument type could not be inferred: {:?}",
                other
            )));
        }
    };
    let replacement = match ty {
        Ty::Unit(_) => Some(make_string_literal_expr("()".to_string())),
        Ty::Nothing(_) => Some(make_string_literal_expr("<none>".to_string())),
        Ty::Unknown(_) => Some(make_string_literal_expr("<unknown>".to_string())),
        _ => None,
    };
    Ok((spec, replacement))
}
