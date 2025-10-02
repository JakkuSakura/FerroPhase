use fp_core::ast::{
    BlockStmt, DecimalType, Expr, ExprBlock, ExprFormatString, ExprInvoke, ExprInvokeTarget,
    ExprKind, FormatArgRef, FormatTemplatePart, Item, ItemKind, Node, NodeKind, Ty, TySlot,
    TypeInt, TypePrimitive, Value,
};
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

use crate::error::optimization_error;

/// Materialize runtime intrinsics (e.g. `print!`, `println!`) into concrete runtime
/// calls so downstream lowering no longer needs to special-case them.
pub fn materialize_runtime_intrinsics(ast: &mut Node) -> Result<()> {
    match ast.kind_mut() {
        NodeKind::File(file) => {
            for item in &mut file.items {
                materialize_item(item)?;
            }
        }
        NodeKind::Item(item) => {
            materialize_item(item)?;
        }
        NodeKind::Expr(expr) => {
            materialize_expr(expr)?;
        }
    }

    Ok(())
}

fn materialize_item(item: &mut Item) -> Result<()> {
    match item.kind_mut() {
        ItemKind::Module(module) => {
            for child in &mut module.items {
                materialize_item(child)?;
            }
        }
        ItemKind::Impl(impl_block) => {
            for child in &mut impl_block.items {
                materialize_item(child)?;
            }
        }
        ItemKind::DefFunction(func) => {
            materialize_expr(func.body.as_mut())?;
        }
        ItemKind::DefConst(def) => {
            materialize_expr(def.value.as_mut())?;
        }
        ItemKind::DefStatic(def) => {
            materialize_expr(def.value.as_mut())?;
        }
        ItemKind::DefStruct(_)
        | ItemKind::DefStructural(_)
        | ItemKind::DefEnum(_)
        | ItemKind::DefType(_)
        | ItemKind::DeclConst(_)
        | ItemKind::DeclStatic(_)
        | ItemKind::DeclFunction(_)
        | ItemKind::DeclType(_)
        | ItemKind::Import(_)
        | ItemKind::DefTrait(_)
        | ItemKind::Any(_) => {}
        ItemKind::Expr(expr) => {
            materialize_expr(expr)?;
        }
    }

    Ok(())
}

fn materialize_block(block: &mut ExprBlock) -> Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            BlockStmt::Expr(expr_stmt) => {
                materialize_expr(expr_stmt.expr.as_mut())?;
            }
            BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    materialize_expr(init)?;
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    materialize_expr(diverge)?;
                }
            }
            BlockStmt::Item(item) => {
                materialize_item(item.as_mut())?;
            }
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    Ok(())
}

fn materialize_expr(expr: &mut Expr) -> Result<()> {
    let expr_ty = expr.ty.clone();
    let mut replacement: Option<Expr> = None;

    match expr.kind_mut() {
        ExprKind::Block(block) => materialize_block(block)?,
        ExprKind::If(expr_if) => {
            materialize_expr(expr_if.cond.as_mut())?;
            materialize_expr(expr_if.then.as_mut())?;
            if let Some(else_expr) = expr_if.elze.as_mut() {
                materialize_expr(else_expr)?;
            }
        }
        ExprKind::Loop(expr_loop) => {
            materialize_expr(expr_loop.body.as_mut())?;
        }
        ExprKind::While(expr_while) => {
            materialize_expr(expr_while.cond.as_mut())?;
            materialize_expr(expr_while.body.as_mut())?;
        }
        ExprKind::Match(match_expr) => {
            for case in &mut match_expr.cases {
                materialize_expr(case.cond.as_mut())?;
                materialize_expr(case.body.as_mut())?;
            }
        }
        ExprKind::Let(expr_let) => {
            materialize_expr(expr_let.expr.as_mut())?;
        }
        ExprKind::Assign(expr_assign) => {
            materialize_expr(expr_assign.target.as_mut())?;
            materialize_expr(expr_assign.value.as_mut())?;
        }
        ExprKind::Invoke(invoke) => {
            materialize_invoke_target(&mut invoke.target)?;
            for arg in &mut invoke.args {
                materialize_expr(arg)?;
            }
        }
        ExprKind::Select(select) => {
            materialize_expr(select.obj.as_mut())?;
        }
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    materialize_expr(value)?;
                }
            }
        }
        ExprKind::Structural(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    materialize_expr(value)?;
                }
            }
        }
        ExprKind::Array(array_expr) => {
            for value in &mut array_expr.values {
                materialize_expr(value)?;
            }
        }
        ExprKind::Tuple(tuple_expr) => {
            for value in &mut tuple_expr.values {
                materialize_expr(value)?;
            }
        }
        ExprKind::BinOp(binop) => {
            materialize_expr(binop.lhs.as_mut())?;
            materialize_expr(binop.rhs.as_mut())?;
        }
        ExprKind::UnOp(unop) => {
            materialize_expr(unop.val.as_mut())?;
        }
        ExprKind::Reference(reference) => {
            materialize_expr(reference.referee.as_mut())?;
        }
        ExprKind::Dereference(expr_deref) => {
            materialize_expr(expr_deref.referee.as_mut())?;
        }
        ExprKind::Index(expr_index) => {
            materialize_expr(expr_index.obj.as_mut())?;
            materialize_expr(expr_index.index.as_mut())?;
        }
        ExprKind::Splat(expr_splat) => {
            materialize_expr(expr_splat.iter.as_mut())?;
        }
        ExprKind::SplatDict(expr_splat) => {
            materialize_expr(expr_splat.dict.as_mut())?;
        }
        ExprKind::Try(expr_try) => {
            materialize_expr(expr_try.expr.as_mut())?;
        }
        ExprKind::Closure(closure) => {
            materialize_expr(closure.body.as_mut())?;
        }
        ExprKind::Closured(closured) => {
            materialize_expr(closured.expr.as_mut())?;
        }
        ExprKind::Paren(paren) => {
            materialize_expr(paren.expr.as_mut())?;
        }
        ExprKind::FormatString(format) => {
            for arg in &mut format.args {
                materialize_expr(arg)?;
            }
            for kwarg in &mut format.kwargs {
                materialize_expr(&mut kwarg.value)?;
            }
        }
        ExprKind::Item(item) => {
            materialize_item(item.as_mut())?;
        }
        ExprKind::Value(value) => {
            if let Value::Expr(inner) = value.as_mut() {
                materialize_expr(inner.as_mut())?;
            }
        }
        ExprKind::IntrinsicCall(call) => {
            match &mut call.payload {
                IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        materialize_expr(arg)?;
                    }
                    for kwarg in template.kwargs.iter_mut() {
                        materialize_expr(&mut kwarg.value)?;
                    }
                }
                IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        materialize_expr(arg)?;
                    }
                }
            }

            if matches!(
                call.kind,
                IntrinsicCallKind::Print | IntrinsicCallKind::Println
            ) {
                replacement = Some(build_printf_invoke(expr_ty, call.clone())?);
            }
        }
        _ => {}
    }

    if let Some(new_expr) = replacement {
        *expr = new_expr;
    }

    Ok(())
}

fn materialize_invoke_target(target: &mut ExprInvokeTarget) -> Result<()> {
    match target {
        ExprInvokeTarget::Method(select) => materialize_expr(select.obj.as_mut())?,
        ExprInvokeTarget::Expr(expr) => materialize_expr(expr.as_mut())?,
        ExprInvokeTarget::Closure(closure) => materialize_expr(closure.body.as_mut())?,
        ExprInvokeTarget::Function(_) | ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {}
    }
    Ok(())
}

fn build_printf_invoke(expr_ty: TySlot, call: fp_core::ast::ExprIntrinsicCall) -> Result<Expr> {
    let newline = matches!(call.kind, IntrinsicCallKind::Println);
    let payload = match call.payload {
        IntrinsicCallPayload::Format { template } => template,
        IntrinsicCallPayload::Args { .. } => {
            return Err(optimization_error(
                "printf lowering requires format payload; found positional args",
            ));
        }
    };

    if !payload.kwargs.is_empty() {
        return Err(optimization_error(
            "named arguments are not supported in runtime printf lowering",
        ));
    }

    let printf_format = build_printf_format(&payload, &payload.args, newline)?;
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

fn build_printf_format(
    template: &ExprFormatString,
    args: &[Expr],
    newline: bool,
) -> Result<String> {
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
                        return Err(optimization_error(format!(
                            "named argument '{name}' is not supported in printf lowering",
                        )))
                    }
                };

                let arg = args.get(arg_index).ok_or_else(|| {
                    optimization_error(format!(
                        "format placeholder references missing argument at index {arg_index}"
                    ))
                })?;

                let spec = if let Some(explicit) = placeholder.format_spec.clone() {
                    if !explicit.trim().starts_with('%') {
                        return Err(optimization_error(format!(
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

    if newline && !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

fn infer_printf_spec(ty: Option<&Ty>) -> Result<String> {
    let ty =
        ty.ok_or_else(|| optimization_error("missing type information for printf argument"))?;
    match ty {
        Ty::Primitive(prim) => match prim {
            TypePrimitive::Bool => Ok("%d".to_string()),
            TypePrimitive::Char => Ok("%c".to_string()),
            TypePrimitive::String => Ok("%s".to_string()),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => Ok("%hhd".to_string()),
                TypeInt::U8 => Ok("%hhu".to_string()),
                TypeInt::I16 => Ok("%hd".to_string()),
                TypeInt::U16 => Ok("%hu".to_string()),
                TypeInt::I32 => Ok("%d".to_string()),
                TypeInt::U32 => Ok("%u".to_string()),
                TypeInt::I64 => Ok("%lld".to_string()),
                TypeInt::U64 => Ok("%llu".to_string()),
                TypeInt::BigInt => Err(optimization_error(
                    "big integer values are not yet supported in printf lowering",
                )),
            },
            TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
                DecimalType::F32 | DecimalType::F64 | DecimalType::Decimal { .. } => {
                    Ok("%f".to_string())
                }
                DecimalType::BigDecimal => Err(optimization_error(
                    "big decimal values are not yet supported in printf lowering",
                )),
            },
            TypePrimitive::List => Err(optimization_error(
                "list values are not supported in printf lowering",
            )),
        },
        Ty::Reference(reference) => infer_printf_spec(Some(reference.ty.as_ref())),
        Ty::Slice(slice) => {
            let elem_ty = slice.elem.as_ref();
            match elem_ty {
                Ty::Primitive(TypePrimitive::Int(TypeInt::U8))
                | Ty::Primitive(TypePrimitive::Int(TypeInt::I8)) => Ok("%s".to_string()),
                _ => Err(optimization_error(
                    "slice values are not supported in printf lowering",
                )),
            }
        }
        Ty::Unit(_) => Err(optimization_error(
            "cannot format unit type with printf; provide an explicit string",
        )),
        Ty::Tuple(_) => Err(optimization_error(
            "tuple values are not supported in printf lowering",
        )),
        Ty::Struct(struct_ty) => Err(optimization_error(format!(
            "struct '{}' does not have a printf representation",
            struct_ty.name
        ))),
        Ty::Structural(_) => Err(optimization_error(
            "structural literals are not supported in printf lowering",
        )),
        Ty::Enum(enum_ty) => Err(optimization_error(format!(
            "enum '{}' does not have a printf representation",
            enum_ty.name
        ))),
        Ty::Function(_) => Err(optimization_error(
            "cannot print function values with printf",
        )),
        Ty::Any(_) | Ty::Unknown(_) | Ty::Nothing(_) | Ty::AnyBox(_) => Err(optimization_error(
            "printf argument type could not be inferred",
        )),
        Ty::Value(_) | Ty::Type(_) | Ty::ImplTraits(_) | Ty::TypeBounds(_) | Ty::Expr(_) => Err(
            optimization_error("unsupported value type in printf lowering"),
        ),
        Ty::Vec(_) => Err(optimization_error(
            "vector values are not supported in printf lowering",
        )),
    }
}
