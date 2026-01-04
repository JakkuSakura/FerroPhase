use std::fmt::{self, Formatter};

use crate::pretty::{escape_char, escape_string, PrettyCtx, PrettyPrintable};

use super::{
    Body, BodyId, Const, Expr, ExprKind, FormatTemplatePart, Function, Impl, ImplItemKind, Item,
    ItemKind, Lit, Mutability, Pat, PatKind, Program, Struct, Ty, VariantData,
};

impl PrettyPrintable for Program {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "thir::Program {")?;
        ctx.with_indent(|ctx| {
            if !self.items.is_empty() {
                ctx.writeln(f, "items:")?;
                ctx.with_indent(|ctx| {
                    for (idx, item) in self.items.iter().enumerate() {
                        write_item(item, f, ctx)?;
                        if idx + 1 < self.items.len() {
                            writeln!(f)?;
                        }
                    }
                    Ok(())
                })?;
            }

            if !self.bodies.is_empty() {
                ctx.writeln(f, "bodies:")?;
                let mut body_ids: Vec<_> = self.bodies.keys().copied().collect();
                body_ids.sort_by_key(|id| id.0);
                ctx.with_indent(|ctx| {
                    for body_id in &body_ids {
                        if let Some(body) = self.bodies.get(body_id) {
                            write_body(*body_id, body, f, ctx)?;
                        }
                    }
                    Ok(())
                })?;
            }

            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}

fn write_item(item: &Item, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    let ty_suffix = if ctx.options.show_types {
        format!(" : {}", format_ty(&item.ty))
    } else {
        String::new()
    };
    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    match &item.kind {
        ItemKind::Function(func) => write_function(func, &ty_suffix, &span_suffix, f, ctx),
        ItemKind::Struct(strukt) => write_struct(strukt, &ty_suffix, &span_suffix, f, ctx),
        ItemKind::Const(konst) => write_const(konst, &ty_suffix, &span_suffix, f, ctx),
        ItemKind::Impl(imp) => write_impl(imp, &ty_suffix, &span_suffix, f, ctx),
    }
}

fn write_function(
    func: &Function,
    ty_suffix: &str,
    span_suffix: &str,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let path = if func.path.is_empty() {
        String::from(func.name.clone())
    } else {
        format!(
            "{}::{}",
            func.path
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join("::"),
            func.name
        )
    };
    let params = func
        .sig
        .inputs
        .iter()
        .enumerate()
        .map(|(idx, ty)| format!("arg{}: {}", idx, format_ty(ty)))
        .collect::<Vec<_>>()
        .join(", ");
    let ret = if ctx.options.show_types {
        format!(" -> {}", format_ty(&func.sig.output))
    } else {
        String::new()
    };
    let variadic = if func.sig.c_variadic {
        " (c-variadic)"
    } else {
        ""
    };
    let body_ref = func
        .body_id
        .map(|id| format_body_id(id))
        .unwrap_or_else(|| "<none>".into());

    ctx.writeln(
        f,
        format!(
            "fn {}({}){}{} [body: {}]{}{}",
            path, params, ret, variadic, body_ref, ty_suffix, span_suffix
        ),
    )
}

fn write_struct(
    strukt: &Struct,
    ty_suffix: &str,
    span_suffix: &str,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let variant = match &strukt.variant_data {
        VariantData::Struct(_, _) => "struct".to_string(),
        VariantData::Tuple(fields) => format!("tuple({})", fields.len()),
        VariantData::Unit => "unit".into(),
    };
    ctx.writeln(
        f,
        format!(
            "struct (fields: {}) [{}]{}{}",
            strukt.fields.len(),
            variant,
            ty_suffix,
            span_suffix
        ),
    )
}

fn write_const(
    konst: &Const,
    ty_suffix: &str,
    span_suffix: &str,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let body = format_body_id(konst.body_id);
    ctx.writeln(
        f,
        format!("const{} [body: {}]{}", ty_suffix, body, span_suffix),
    )
}

fn write_impl(
    imp: &Impl,
    ty_suffix: &str,
    span_suffix: &str,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let self_ty = if ctx.options.show_types {
        format_ty(&imp.self_ty)
    } else {
        "<self>".into()
    };

    let trait_info = imp.trait_ref.as_ref().map(|_| " for <trait>").unwrap_or("");

    let suffix = format!("{}{}", ty_suffix, span_suffix);
    ctx.writeln(f, format!("impl {}{}{}", self_ty, trait_info, suffix))?;
    ctx.with_indent(|ctx| {
        for (idx, item) in imp.items.iter().enumerate() {
            match &item.kind {
                ImplItemKind::Method(func) => write_function(func, "", "", f, ctx)?,
                ImplItemKind::AssocConst(konst) => write_const(konst, "", "", f, ctx)?,
            }
            if idx + 1 < imp.items.len() {
                writeln!(f)?;
            }
        }
        Ok(())
    })
}

fn write_body(
    body_id: BodyId,
    body: &Body,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let header = format!(
        "{} (params: {}, locals: {})",
        format_body_id(body_id),
        body.params.len(),
        body.locals.len()
    );
    ctx.writeln(f, format!("{} {{", header))?;
    ctx.with_indent(|ctx| {
        if !body.params.is_empty() {
            ctx.writeln(f, "params:")?;
            ctx.with_indent(|ctx| {
                for (idx, param) in body.params.iter().enumerate() {
                    let name = param
                        .pat
                        .as_ref()
                        .map(|pat| summarize_pat(pat))
                        .unwrap_or_else(|| format!("arg{}", idx));
                    ctx.writeln(f, format!("{}: {}", name, format_ty(&param.ty)))?;
                }
                Ok(())
            })?;
        }

        if !body.locals.is_empty() {
            ctx.writeln(f, "locals:")?;
            ctx.with_indent(|ctx| {
                for (idx, local) in body.locals.iter().enumerate() {
                    let mut line = format!("_{}: {}", idx, format_ty(&local.ty));
                    if local.internal {
                        line.push_str(" (internal)");
                    }
                    if ctx.options.show_spans {
                        line.push_str(&format!(" // span: {:?}", local.source_info));
                    }
                    ctx.writeln(f, line)?;
                }
                Ok(())
            })?;
        }

        ctx.writeln(f, format!("value: {}", summarize_expr(&body.value, ctx)))
    })?;
    ctx.writeln(f, "}")
}

fn summarize_expr(expr: &Expr, ctx: &PrettyCtx<'_>) -> String {
    let core = match &expr.kind {
        ExprKind::Literal(lit) => summarize_lit(lit),
        ExprKind::Local(id) => format!("local(_{})", id),
        ExprKind::VarRef { id } => format!("var_ref(_{})", id),
        ExprKind::Path(item_ref) => String::from(item_ref.name.clone()),
        ExprKind::Binary(op, lhs, rhs) => format!(
            "{:?}({}, {})",
            op,
            summarize_simple(lhs, ctx),
            summarize_simple(rhs, ctx)
        ),
        ExprKind::Unary(op, expr) => format!("{:?}({})", op, summarize_simple(expr, ctx)),
        ExprKind::Call { fun, args, .. } => {
            let args = args
                .iter()
                .map(|arg| summarize_simple(arg, ctx))
                .collect::<Vec<_>>()
                .join(", ");
            format!("call({}, [{}])", summarize_simple(fun, ctx), args)
        }
        ExprKind::IntrinsicCall(call) => match &call.payload {
            crate::intrinsics::IntrinsicCallPayload::Format { template } => {
                let args = template
                    .args
                    .iter()
                    .map(|arg| summarize_simple(arg, ctx))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "std::{:?}({}, [{}])",
                    call.kind,
                    summarize_format_parts(&template.parts),
                    args
                )
            }
            crate::intrinsics::IntrinsicCallPayload::Args { args } => {
                let args = args
                    .iter()
                    .map(|arg| summarize_simple(arg, ctx))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("std::{:?}([{}])", call.kind, args)
            }
        },
        ExprKind::Borrow { borrow_kind, arg } => {
            format!("borrow({:?}, {})", borrow_kind, summarize_simple(arg, ctx))
        }
        ExprKind::AddressOf { mutability, arg } => {
            format!("&{:?} {}", mutability, summarize_simple(arg, ctx))
        }
        ExprKind::Assign { lhs, rhs } => format!(
            "assign({}, {})",
            summarize_simple(lhs, ctx),
            summarize_simple(rhs, ctx)
        ),
        ExprKind::AssignOp { op, lhs, rhs } => format!(
            "assign_op({:?}, {}, {})",
            op,
            summarize_simple(lhs, ctx),
            summarize_simple(rhs, ctx)
        ),
        ExprKind::Return { value } => value
            .as_ref()
            .map(|val| format!("return {}", summarize_simple(val, ctx)))
            .unwrap_or_else(|| "return".into()),
        ExprKind::Break { value } => value
            .as_ref()
            .map(|val| format!("break {}", summarize_simple(val, ctx)))
            .unwrap_or_else(|| "break".into()),
        ExprKind::Continue => "continue".into(),
        ExprKind::Block(block) => {
            let stmt_count = block.stmts.len();
            format!("block({} stmts)", stmt_count)
        }
        ExprKind::LogicalOp { op, lhs, rhs } => format!(
            "{:?}({}, {})",
            op,
            summarize_simple(lhs, ctx),
            summarize_simple(rhs, ctx)
        ),
        ExprKind::Let { expr, pat } => format!(
            "let {} = {}",
            summarize_pat(pat),
            summarize_simple(expr, ctx)
        ),
        other => format!("{:?}", other),
    };

    if ctx.options.show_types {
        format!("{} : {}", core, format_ty(&expr.ty))
    } else {
        core
    }
}

fn summarize_simple(expr: &Expr, _ctx: &PrettyCtx<'_>) -> String {
    match &expr.kind {
        ExprKind::Literal(lit) => summarize_lit(lit),
        ExprKind::Local(id) => format!("_{}", id),
        ExprKind::Path(item_ref) => String::from(item_ref.name.clone()),
        _ => format!("{:?}", expr.kind),
    }
}

fn summarize_lit(lit: &Lit) -> String {
    match lit {
        Lit::Bool(value) => value.to_string(),
        Lit::Int(value, _) => value.to_string(),
        Lit::Uint(value, _) => value.to_string(),
        Lit::Float(value, _) => value.to_string(),
        Lit::Char(ch) => format!("'{}'", escape_char(*ch)),
        Lit::Str(s) => format!("\"{}\"", escape_string(s)),
        other => format!("{:?}", other),
    }
}

fn summarize_format_parts(parts: &[FormatTemplatePart]) -> String {
    let mut buf = String::new();
    for part in parts {
        match part {
            FormatTemplatePart::Literal(text) => buf.push_str(text),
            FormatTemplatePart::Placeholder(_) => buf.push_str("{..}"),
        }
    }
    buf
}

fn summarize_pat(pat: &Pat) -> String {
    match &pat.kind {
        PatKind::Wild => "_".into(),
        PatKind::Binding {
            name, mutability, ..
        } => match mutability {
            Mutability::Mut => format!("mut {}", name),
            Mutability::Not => String::from(name.clone()),
        },
        other => format!("{:?}", other),
    }
}

fn format_ty(ty: &Ty) -> String {
    format!("{}", ty)
}

fn format_body_id(id: BodyId) -> String {
    format!("body{}", id.0)
}
