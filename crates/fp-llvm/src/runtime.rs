use fp_core::ast::{
    DecimalType, Expr, ExprDereference, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind,
    ExprSelect, ExprStringTemplate, FormatArgRef, FormatTemplatePart, FunctionParam, Ty, TySlot,
    TypeAny, TypeInt, TypeNothing, TypePrimitive, TypeUnit, Value,
};
use fp_core::ast::{Ident, Locator};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::intrinsics::{ensure_function_decl, make_function_decl, IntrinsicMaterializer};
use fp_core::span::Span;

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
            let Some((_template, args, kwargs)) = extract_format_call(call) else {
                return Ok(None);
            };
            if !kwargs.is_empty() {
                return Ok(None);
            }
            if args.iter().any(|arg| arg.ty().is_none()) {
                return Ok(None);
            }
            if args.iter().any(|arg| is_missing_printf_type_info(arg)) {
                return Ok(None);
            }
            match build_printf_invoke(expr_ty.clone(), call.clone()) {
                Ok(expr) => Ok(Some(expr)),
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }
}

fn build_printf_invoke(expr_ty: TySlot, call: ExprIntrinsicCall) -> Result<Expr> {
    let newline = matches!(call.kind, IntrinsicCallKind::Println);
    let (template, args, kwargs) = extract_format_call(&call).ok_or_else(|| {
        fp_core::error::Error::from(
            "printf lowering requires format template as the first argument".to_string(),
        )
    })?;
    if !kwargs.is_empty() {
        return Err(fp_core::error::Error::from(
            "named arguments are not supported in runtime printf lowering".to_string(),
        ));
    }

    let mut args = args.to_vec();
    let printf_format = build_printf_format(&template.parts, &mut args, newline)?;

    let mut invoke_args = Vec::with_capacity(args.len() + 1);
    invoke_args.push(make_string_literal_expr(printf_format));
    invoke_args.append(&mut args);

    let target = ExprInvokeTarget::Function(Locator::ident(Ident::new("printf")));
    let invoke = ExprKind::Invoke(ExprInvoke {
        target,
        args: invoke_args,
        span: Span::null(),
    });

    Ok(Expr::with_ty(invoke, expr_ty))
}

fn extract_format_call(
    call: &ExprIntrinsicCall,
) -> Option<(&ExprStringTemplate, &[Expr], &[fp_core::ast::ExprKwArg])> {
    let first = call.args.first()?;
    let template = match first.kind() {
        ExprKind::FormatString(format) => format,
        _ => return None,
    };
    Some((template, &call.args[1..], &call.kwargs))
}

fn make_string_literal_expr(literal: String) -> Expr {
    let mut expr = Expr::value(Value::string(literal));
    expr.set_ty(Ty::Primitive(TypePrimitive::String));
    expr
}

fn is_missing_printf_type_info(expr: &Expr) -> bool {
    if let Some(ty) = expr.ty() {
        match ty {
            Ty::Primitive(_) | Ty::Value(_) => return false,
            Ty::Reference(reference) => {
                return !is_string_like_ty(reference.ty.as_ref());
            }
            Ty::Expr(expr_ty) => {
                if let ExprKind::Locator(locator) = expr_ty.kind() {
                    if let Locator::Ident(ident) = locator {
                        let known =
                            matches!(ident.as_str(), "Int" | "Bool" | "String" | "Str" | "Char");
                        return !known;
                    }
                }
                return true;
            }
            Ty::Any(_) | Ty::Unknown(_) => {}
            _ => return true,
        }
    }

    match expr.kind() {
        ExprKind::Value(value) => !matches!(
            value.as_ref(),
            Value::Int(_) | Value::Decimal(_) | Value::Bool(_) | Value::Char(_) | Value::String(_)
        ),
        ExprKind::Cast(_) => false,
        ExprKind::Select(select) => select
            .obj
            .ty()
            .map_or(true, |ty| matches!(ty, Ty::Any(_) | Ty::Unknown(_))),
        ExprKind::Reference(reference) => reference
            .referee
            .ty()
            .map_or(true, |ty| matches!(ty, Ty::Any(_) | Ty::Unknown(_))),
        _ => true,
    }
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

                let (spec, replacement) = if let Some(explicit) = placeholder.format_spec.as_ref() {
                    let trimmed = explicit.raw.trim();
                    if trimmed.starts_with('%') {
                        (explicit.raw.clone(), None)
                    } else if trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                        (format!("%{}", trimmed), None)
                    } else {
                        let (inferred, replacement) =
                            infer_printf_spec_with_replacement_from_expr(arg)?;
                        let suffix = inferred.trim_start_matches('%');
                        (format!("%{}{}", trimmed, suffix), replacement)
                    }
                } else {
                    infer_printf_spec_with_replacement_from_expr(arg)?
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

fn infer_printf_spec_with_replacement_from_expr(expr: &Expr) -> Result<(String, Option<Expr>)> {
    let ty = expr
        .ty()
        .filter(|ty| !matches!(ty, Ty::Any(_) | Ty::Unknown(_)));
    if let Some(ty) = ty {
        if let Ty::Reference(reference) = ty {
            let inner = reference.ty.as_ref();
            if is_string_like_ty(inner) {
                return Ok(("%s".to_string(), None));
            }
            let (spec, _) = infer_printf_spec_with_replacement(Some(inner))?;
            let mut deref = Expr::new(ExprKind::Dereference(ExprDereference {
                referee: Box::new(expr.clone()),
                span: Span::null(),
            }));
            deref.set_ty((*reference.ty).clone());
            return Ok((spec, Some(deref)));
        }
        return infer_printf_spec_with_replacement(Some(ty));
    }

    match expr.kind() {
        ExprKind::Value(value) => infer_printf_spec_for_value(value.as_ref()),
        ExprKind::Select(select) => infer_printf_spec_for_select(select),
        ExprKind::Cast(cast) => infer_printf_spec_with_replacement(Some(&cast.ty)),
        ExprKind::Reference(reference) => match reference.referee.ty() {
            Some(ty) => infer_printf_spec_with_replacement(Some(ty)),
            None => Err(fp_core::error::Error::from(
                "missing type information for printf argument".to_string(),
            )),
        },
        _ => Err(fp_core::error::Error::from(
            "missing type information for printf argument".to_string(),
        )),
    }
}

fn infer_printf_spec_for_select(select: &ExprSelect) -> Result<(String, Option<Expr>)> {
    let Some(obj_ty) = select.obj.ty() else {
        return Err(fp_core::error::Error::from(
            "missing type information for printf argument".to_string(),
        ));
    };
    match obj_ty {
        Ty::Struct(struct_ty) => {
            let field = struct_ty
                .fields
                .iter()
                .find(|field| field.name == select.field)
                .ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "missing field `{}` for printf argument",
                        select.field
                    ))
                })?;
            infer_printf_spec_with_replacement(Some(&field.value))
        }
        Ty::Structural(struct_ty) => {
            let field = struct_ty
                .fields
                .iter()
                .find(|field| field.name == select.field)
                .ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "missing field `{}` for printf argument",
                        select.field
                    ))
                })?;
            infer_printf_spec_with_replacement(Some(&field.value))
        }
        Ty::Reference(reference) => {
            let inner = reference.ty.as_ref();
            if is_string_like_ty(inner) {
                return Ok(("%s".to_string(), None));
            }
            let (spec, _) = infer_printf_spec_with_replacement(Some(inner))?;
            let mut deref = Expr::new(ExprKind::Dereference(ExprDereference {
                referee: Box::new(Expr::new(ExprKind::Select(select.clone()))),
                span: Span::null(),
            }));
            deref.set_ty((*reference.ty).clone());
            Ok((spec, Some(deref)))
        }
        _ => Err(fp_core::error::Error::from(
            "printf argument type could not be inferred from field select".to_string(),
        )),
    }
}

fn infer_printf_spec_for_value(value: &Value) -> Result<(String, Option<Expr>)> {
    let ty = match value {
        Value::Int(_) => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        Value::Bool(_) => Ty::Primitive(TypePrimitive::Bool),
        Value::Decimal(_) => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
        Value::Char(_) => Ty::Primitive(TypePrimitive::Char),
        Value::String(_) => Ty::Primitive(TypePrimitive::String),
        Value::Unit(_) => Ty::Unit(TypeUnit),
        Value::Null(_) | Value::None(_) => Ty::Nothing(TypeNothing),
        _ => {
            return Err(fp_core::error::Error::from(
                "printf argument type could not be inferred from literal".to_string(),
            ))
        }
    };
    infer_printf_spec_with_replacement(Some(&ty))
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
        Ty::Reference(reference) => {
            if is_string_like_ty(reference.ty.as_ref()) {
                "%s".to_string()
            } else {
                return Err(fp_core::error::Error::from(
                    "printf does not support reference values; dereference first".to_string(),
                ));
            }
        }
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
        Ty::Value(value) => {
            return infer_printf_spec_for_value(value.value.as_ref());
        }
        Ty::Expr(expr) => {
            if let ExprKind::Locator(locator) = expr.kind() {
                if let Locator::Ident(ident) = locator {
                    let spec = match ident.as_str() {
                        "Int" => "%lld",
                        "Bool" => "%d",
                        "String" | "Str" => "%s",
                        "Char" => "%c",
                        _ => {
                            return Err(fp_core::error::Error::from(format!(
                                "printf argument type could not be inferred: {:?}",
                                expr
                            )));
                        }
                    };
                    return Ok((spec.to_string(), None));
                }
            }
            return Err(fp_core::error::Error::from(format!(
                "printf argument type could not be inferred: {:?}",
                expr
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
        Ty::Nothing(_) => Some(make_string_literal_expr("null".to_string())),
        Ty::Unknown(_) => Some(make_string_literal_expr("<unknown>".to_string())),
        _ => None,
    };
    Ok((spec, replacement))
}

fn is_string_like_ty(ty: &Ty) -> bool {
    matches!(ty, Ty::Primitive(TypePrimitive::String))
}
