use crate::intrinsics::IntrinsicCallKind;
use crate::pretty::{escape_char, escape_string, PrettyCtx, PrettyPrintable};
use std::fmt::{self, Formatter};

use super::{
    BinOp, Block, Body, Const, Enum, Expr, ExprKind, FormatArgRef, FormatTemplatePart, Function,
    GenericArg, GenericParamKind, Generics, Impl, ImplItemKind, Item, ItemKind, Lit, Pat, PatKind,
    Path, Program, Stmt, StmtKind, Struct, TypeExpr, TypeExprKind, UnOp, Visibility,
};

impl PrettyPrintable for Program {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "hir::Program {")?;
        ctx.with_indent(|ctx| {
            for (idx, item) in self.items.iter().enumerate() {
                write_item(item, f, ctx)?;
                if idx + 1 < self.items.len() {
                    writeln!(f)?;
                }
            }
            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}

fn write_item(item: &Item, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    match &item.kind {
        ItemKind::Function(func) => write_function(item, func, f, ctx),
        ItemKind::Struct(strukt) => write_struct(item, strukt, f, ctx),
        ItemKind::Enum(enm) => write_enum(item, enm, f, ctx),
        ItemKind::Const(konst) => write_const(item, konst, f, ctx),
        ItemKind::Impl(imp) => write_impl(item, imp, f, ctx),
    }
}

fn write_function(
    item: &Item,
    func: &Function,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let vis = fmt_visibility(&item.visibility);
    let const_flag = if func.is_const { "const " } else { "" };
    let generics = fmt_generics(&func.sig.generics, ctx);
    let params = fmt_params(&func.sig.inputs, ctx);
    let ret = if ctx.options.show_types {
        let ty = fmt_type_expr(&func.sig.output, ctx);
        if ty.is_empty() {
            String::new()
        } else {
            format!(" -> {}", ty)
        }
    } else {
        String::new()
    };

    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    let header = format!(
        "{}{}fn {}{}({}){}{}",
        vis,
        const_flag,
        func.sig.name,
        generics,
        params.join(", "),
        ret,
        span_suffix
    );

    if let Some(body) = &func.body {
        ctx.writeln(f, format!("{} {{", header))?;
        ctx.with_indent(|ctx| write_body(body, f, ctx))?;
        ctx.writeln(f, "}")
    } else {
        ctx.writeln(f, format!("{};", header))
    }
}

fn write_struct(
    item: &Item,
    strukt: &Struct,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let vis = fmt_visibility(&item.visibility);
    let generics = fmt_generics(&strukt.generics, ctx);
    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    ctx.writeln(
        f,
        format!("{}struct {}{} {{", vis, strukt.name, generics) + &span_suffix,
    )?;
    ctx.with_indent(|ctx| {
        for field in &strukt.fields {
            let field_vis = fmt_visibility(&field.vis);
            let ty = if ctx.options.show_types {
                format!(": {}", fmt_type_expr(&field.ty, ctx))
            } else {
                String::new()
            };
            let mut line = format!("{}{}{}", field_vis, field.name, ty);
            line.push(',');
            ctx.writeln(f, line)?;
        }
        Ok(())
    })?;
    ctx.writeln(f, "}")
}

fn write_enum(
    item: &Item,
    enm: &Enum,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let vis = fmt_visibility(&item.visibility);
    let generics = fmt_generics(&enm.generics, ctx);
    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    ctx.writeln(
        f,
        format!("{}enum {}{} {{", vis, enm.name, generics) + &span_suffix,
    )?;
    ctx.with_indent(|ctx| {
        for variant in &enm.variants {
            let mut line = String::from(variant.name.clone());
            if let Some(expr) = &variant.discriminant {
                line.push_str(" = ");
                line.push_str(&format_expr_inline(expr, ctx));
            }
            line.push(',');
            ctx.writeln(f, line)?;
        }
        Ok(())
    })?;
    ctx.writeln(f, "}")
}

fn write_const(
    item: &Item,
    konst: &Const,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let vis = fmt_visibility(&item.visibility);
    let ty = if ctx.options.show_types {
        format!(": {}", fmt_type_expr(&konst.ty, ctx))
    } else {
        String::new()
    };
    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    ctx.writeln(
        f,
        format!("{}const {}{} =", vis, konst.name, ty) + &span_suffix,
    )?;
    ctx.with_indent(|ctx| write_const_expr(&konst.body, f, ctx))
}

fn write_impl(
    item: &Item,
    imp: &Impl,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let vis = fmt_visibility(&item.visibility);
    let self_ty = if ctx.options.show_types {
        fmt_type_expr(&imp.self_ty, ctx)
    } else {
        String::from("<impl>")
    };

    let trait_part = imp
        .trait_ty
        .as_ref()
        .filter(|_| ctx.options.show_types)
        .map(|path| format!("{} for ", fmt_type_expr(path, ctx)))
        .unwrap_or_default();

    let span_suffix = if ctx.options.show_spans {
        format!(" // span: {:?}", item.span)
    } else {
        String::new()
    };

    ctx.writeln(
        f,
        format!("{}impl {}{} {{", vis, trait_part, self_ty) + &span_suffix,
    )?;
    ctx.with_indent(|ctx| {
        for (idx, impl_item) in imp.items.iter().enumerate() {
            match &impl_item.kind {
                ImplItemKind::Method(func) => write_impl_method(func, f, ctx)?,
                ImplItemKind::AssocConst(konst) => write_impl_const(konst, f, ctx)?,
            }
            if idx + 1 < imp.items.len() {
                writeln!(f)?;
            }
        }
        Ok(())
    })?;
    ctx.writeln(f, "}")
}

fn write_body(body: &Body, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    if ctx.options.show_types && !body.params.is_empty() {
        let params = body
            .params
            .iter()
            .map(|param| format_param(param, ctx))
            .collect::<Vec<_>>()
            .join(", ");
        ctx.writeln(f, format!("// params: [{}]", params))?;
    }

    write_expr(&body.value, f, ctx)
}

fn write_const_expr(body: &Body, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    match &body.value.kind {
        ExprKind::Block(block) => write_block(block, f, ctx),
        _ => ctx.writeln(f, format!("{};", format_expr_inline(&body.value, ctx))),
    }
}

fn write_impl_method(
    func: &Function,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    let generics = fmt_generics(&func.sig.generics, ctx);
    let params = fmt_params(&func.sig.inputs, ctx);
    let ret = if ctx.options.show_types {
        let ty = fmt_type_expr(&func.sig.output, ctx);
        if ty.is_empty() {
            String::new()
        } else {
            format!(" -> {}", ty)
        }
    } else {
        String::new()
    };

    let header = format!(
        "fn {}{}({}){}",
        func.sig.name,
        generics,
        params.join(", "),
        ret
    );

    if let Some(body) = &func.body {
        ctx.writeln(f, format!("{} {{", header))?;
        ctx.with_indent(|ctx| write_body(body, f, ctx))?;
        ctx.writeln(f, "}")
    } else {
        ctx.writeln(f, format!("{};", header))
    }
}

fn write_impl_const(konst: &Const, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    let ty = if ctx.options.show_types {
        format!(": {}", fmt_type_expr(&konst.ty, ctx))
    } else {
        String::new()
    };

    ctx.writeln(f, format!("const {}{} =", konst.name, ty))?;
    ctx.with_indent(|ctx| write_const_expr(&konst.body, f, ctx))
}

fn write_block(block: &Block, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    ctx.writeln(f, "{")?;
    ctx.with_indent(|ctx| {
        for stmt in &block.stmts {
            write_stmt(stmt, f, ctx)?;
        }
        if let Some(expr) = &block.expr {
            write_expr(expr, f, ctx)?;
        }
        Ok(())
    })?;
    ctx.writeln(f, "}")
}

fn write_stmt(stmt: &Stmt, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    match &stmt.kind {
        StmtKind::Local(local) => {
            let pat = format_pat(&local.pat, ctx);
            let ty = if ctx.options.show_types {
                local
                    .ty
                    .as_ref()
                    .map(|ty| format!(": {}", fmt_type_expr(ty, ctx)))
                    .unwrap_or_default()
            } else {
                String::new()
            };
            let init = local
                .init
                .as_ref()
                .map(|expr| format!(" = {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default();
            ctx.writeln(f, format!("let {}{}{};", pat, ty, init))
        }
        StmtKind::Item(item) => write_item(item, f, ctx),
        StmtKind::Expr(expr) => write_expr(expr, f, ctx),
        StmtKind::Semi(expr) => ctx.writeln(f, format!("{};", format_expr_inline(expr, ctx))),
    }
}

fn write_expr(expr: &Expr, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
    match &expr.kind {
        ExprKind::Block(block) => write_block(block, f, ctx),
        ExprKind::If(cond, then_branch, else_branch) => {
            ctx.writeln(f, format!("if ({})", format_expr_inline(cond, ctx)))?;
            ctx.with_indent(|ctx| write_expr(then_branch, f, ctx))?;
            if let Some(else_expr) = else_branch {
                ctx.writeln(f, "else")?;
                ctx.with_indent(|ctx| write_expr(else_expr, f, ctx))?;
            }
            Ok(())
        }
        ExprKind::Loop(block) => {
            ctx.writeln(f, "loop")?;
            ctx.with_indent(|ctx| write_block(block, f, ctx))
        }
        ExprKind::While(cond, block) => {
            ctx.writeln(f, format!("while ({})", format_expr_inline(cond, ctx)))?;
            ctx.with_indent(|ctx| write_block(block, f, ctx))
        }
        ExprKind::Match(scrutinee, arms) => {
            ctx.writeln(f, format!("match ({})", format_expr_inline(scrutinee, ctx)))?;
            ctx.writeln(f, "{")?;
            ctx.with_indent(|ctx| {
                for arm in arms {
                    let guard = arm
                        .guard
                        .as_ref()
                        .map(|expr| format!(" if {}", format_expr_inline(expr, ctx)))
                        .unwrap_or_default();
                    let pat = format_pat(&arm.pat, ctx);
                    ctx.writeln(f, format!("{}{} =>", pat, guard))?;
                    ctx.with_indent(|ctx| write_expr(&arm.body, f, ctx))?;
                }
                Ok(())
            })?;
            ctx.writeln(f, "}")
        }
        ExprKind::FormatString(template) => {
            let format_text = summarize_format_parts(&template.parts);
            ctx.writeln(
                f,
                format!("format_string(\"{}\")", escape_string(&format_text)),
            )
        }
        ExprKind::Return(value) => {
            let suffix = value
                .as_ref()
                .map(|expr| format!(" {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default();
            ctx.writeln(f, format!("return{};", suffix))
        }
        ExprKind::Break(value) => {
            let suffix = value
                .as_ref()
                .map(|expr| format!(" {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default();
            ctx.writeln(f, format!("break{};", suffix))
        }
        ExprKind::Continue => ctx.writeln(f, "continue;"),
        _ => ctx.writeln(f, format_expr_inline(expr, ctx)),
    }
}

fn format_expr_inline(expr: &Expr, ctx: &PrettyCtx<'_>) -> String {
    match &expr.kind {
        ExprKind::Literal(lit) => format_lit(lit),
        ExprKind::Path(path) => fmt_path(path, ctx),
        ExprKind::FormatString(template) => {
            let format_text = summarize_format_parts(&template.parts);
            format!("format_string(\"{}\")", escape_string(&format_text))
        }
        ExprKind::Binary(op, lhs, rhs) => format!(
            "({} {} {})",
            format_expr_inline(lhs, ctx),
            fmt_bin_op(op),
            format_expr_inline(rhs, ctx)
        ),
        ExprKind::Unary(op, inner) => {
            format!("({}{})", fmt_un_op(op), format_expr_inline(inner, ctx))
        }
        ExprKind::Call(callee, args) => {
            let args = args
                .iter()
                .map(|arg| format!("{} = {}", arg.name, format_expr_inline(&arg.value, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", format_expr_inline(callee, ctx), args)
        }
        ExprKind::MethodCall(receiver, name, args) => {
            let args = args
                .iter()
                .map(|arg| format!("{} = {}", arg.name, format_expr_inline(&arg.value, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}.{}({})", format_expr_inline(receiver, ctx), name, args)
        }
        ExprKind::Cast(expr, ty) => {
            let ty_str = fmt_type_expr(ty, ctx);
            format!("({} as {})", format_expr_inline(expr, ctx), ty_str)
        }
        ExprKind::FieldAccess(base, field) => {
            format!("{}.{}", format_expr_inline(base, ctx), field)
        }
        ExprKind::Index(base, index) => format!(
            "{}[{}]",
            format_expr_inline(base, ctx),
            format_expr_inline(index, ctx)
        ),
        ExprKind::Struct(path, fields) => {
            let fields = fields
                .iter()
                .map(|field| format!("{}: {}", field.name, format_expr_inline(&field.expr, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} {{ {} }}", fmt_path(path, ctx), fields)
        }
        ExprKind::Array(elements) => {
            let elems = elements
                .iter()
                .map(|elem| format_expr_inline(elem, ctx))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", elems)
        }
        ExprKind::ArrayRepeat { elem, len } => {
            format!(
                "[{}; {}]",
                format_expr_inline(elem, ctx),
                format_expr_inline(len, ctx)
            )
        }
        ExprKind::Match(scrutinee, arms) => {
            let arms = arms
                .iter()
                .map(|arm| {
                    let guard = arm
                        .guard
                        .as_ref()
                        .map(|expr| format!(" if {}", format_expr_inline(expr, ctx)))
                        .unwrap_or_default();
                    format!(
                        "{}{} => {}",
                        format_pat(&arm.pat, ctx),
                        guard,
                        format_expr_inline(&arm.body, ctx)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "match {} {{ {} }}",
                format_expr_inline(scrutinee, ctx),
                arms
            )
        }
        ExprKind::IntrinsicCall(call) => {
            let args = call
                .callargs
                .iter()
                .map(|arg| format!("{} = {}", arg.name, format_expr_inline(&arg.value, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("std::{}({})", render_intrinsic_kind(call.kind), args)
        }
        ExprKind::Let(pat, ty, value) => {
            let pat_str = format_pat(pat, ctx);
            let ty_str = if ctx.options.show_types {
                format!(": {}", fmt_type_expr(ty, ctx))
            } else {
                String::new()
            };
            let value_str = value
                .as_ref()
                .map(|expr| format!(" = {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default();
            format!("let {}{}{}", pat_str, ty_str, value_str)
        }
        ExprKind::Assign(lhs, rhs) => format!(
            "{} = {}",
            format_expr_inline(lhs, ctx),
            format_expr_inline(rhs, ctx)
        ),
        ExprKind::Return(value) => format!(
            "return{}",
            value
                .as_ref()
                .map(|expr| format!(" {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default()
        ),
        ExprKind::Break(value) => format!(
            "break{}",
            value
                .as_ref()
                .map(|expr| format!(" {}", format_expr_inline(expr, ctx)))
                .unwrap_or_default()
        ),
        ExprKind::Continue => "continue".into(),
        ExprKind::Loop(_) | ExprKind::If(_, _, _) | ExprKind::Block(_) | ExprKind::While(_, _) => {
            "<control-flow>".into()
        }
    }
}

fn format_lit(lit: &Lit) -> String {
    match lit {
        Lit::Bool(value) => value.to_string(),
        Lit::Integer(value) => value.to_string(),
        Lit::Float(value) => value.to_string(),
        Lit::Str(value) => format!("\"{}\"", escape_string(value)),
        Lit::Char(value) => format!("'{}'", escape_char(*value)),
        Lit::Null => "null".to_string(),
    }
}

fn summarize_format_parts(parts: &[FormatTemplatePart]) -> String {
    let mut buf = String::new();
    for part in parts {
        match part {
            FormatTemplatePart::Literal(text) => buf.push_str(text),
            FormatTemplatePart::Placeholder(placeholder) => {
                buf.push('{');
                buf.push_str(&format_arg_ref(&placeholder.arg_ref));
                if let Some(spec) = &placeholder.format_spec {
                    buf.push(':');
                    buf.push_str(&spec.raw);
                }
                buf.push('}');
            }
        }
    }
    buf
}

fn format_arg_ref(arg_ref: &FormatArgRef) -> String {
    match arg_ref {
        FormatArgRef::Implicit => String::new(),
        FormatArgRef::Positional(index) => index.to_string(),
        FormatArgRef::Named(name) => name.clone(),
    }
}

fn render_intrinsic_kind(kind: IntrinsicCallKind) -> &'static str {
    match kind {
        IntrinsicCallKind::Println => "println",
        IntrinsicCallKind::Print => "print",
        IntrinsicCallKind::Format => "format",
        IntrinsicCallKind::Len => "len",
        IntrinsicCallKind::Slice => "slice",
        IntrinsicCallKind::DebugAssertions => "debug_assertions",
        IntrinsicCallKind::Input => "input",
        IntrinsicCallKind::Panic => "panic",
        IntrinsicCallKind::CatchUnwind => "catch_unwind",
        IntrinsicCallKind::TimeNow => "time_now",
        IntrinsicCallKind::SizeOf => "size_of",
        IntrinsicCallKind::ReflectFields => "reflect_fields",
        IntrinsicCallKind::HasMethod => "has_method",
        IntrinsicCallKind::TypeName => "type_name",
        IntrinsicCallKind::TypeOf => "type_of",
        IntrinsicCallKind::CreateStruct => "create_struct",
        IntrinsicCallKind::CloneStruct => "clone_struct",
        IntrinsicCallKind::AddField => "add_field",
        IntrinsicCallKind::HasField => "has_field",
        IntrinsicCallKind::FieldCount => "field_count",
        IntrinsicCallKind::MethodCount => "method_count",
        IntrinsicCallKind::FieldType => "field_type",
        IntrinsicCallKind::FieldNameAt => "field_name_at",
        IntrinsicCallKind::StructSize => "struct_size",
        IntrinsicCallKind::GenerateMethod => "generate_method",
        IntrinsicCallKind::CompileError => "compile_error",
        IntrinsicCallKind::CompileWarning => "compile_warning",
        IntrinsicCallKind::ProcMacroTokenStreamFromStr => "token_stream_from_str",
        IntrinsicCallKind::ProcMacroTokenStreamToString => "token_stream_to_string",
    }
}

fn format_pat(pat: &Pat, ctx: &PrettyCtx<'_>) -> String {
    match &pat.kind {
        PatKind::Wild => "_".into(),
        PatKind::Binding { name, mutable } => {
            if *mutable {
                format!("mut {}", name)
            } else {
                String::from(name.clone())
            }
        }
        PatKind::Struct(path, fields, has_rest) => {
            let fields = fields
                .iter()
                .map(|field| format!("{}: {}", field.name, format_pat(&field.pat, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            let rest = if *has_rest { ".." } else { "" };
            let separator = if fields.is_empty() || rest.is_empty() {
                ""
            } else {
                ", "
            };
            format!(
                "{} {{ {}{}{} }}",
                fmt_path(path, ctx),
                fields,
                separator,
                rest
            )
        }
        PatKind::TupleStruct(path, parts) => {
            let parts = parts
                .iter()
                .map(|pat| format_pat(pat, ctx))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", fmt_path(path, ctx), parts)
        }
        PatKind::Variant(path) => fmt_path(path, ctx),
        PatKind::Tuple(parts) => {
            let parts = parts
                .iter()
                .map(|pat| format_pat(pat, ctx))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", parts)
        }
        PatKind::Lit(lit) => format_lit(lit),
    }
}

fn fmt_path(path: &Path, ctx: &PrettyCtx<'_>) -> String {
    let mut segments = Vec::new();
    for segment in &path.segments {
        let mut text = String::from(segment.name.clone());
        if let Some(args) = &segment.args {
            let args = args
                .args
                .iter()
                .map(|arg| match arg {
                    GenericArg::Type(ty) => fmt_type_expr(ty, ctx),
                    GenericArg::Const(expr) => format_expr_inline(expr, ctx),
                })
                .collect::<Vec<_>>()
                .join(", ");
            text.push_str(&format!("<{}>", args));
        }
        segments.push(text);
    }
    segments.join("::")
}

fn fmt_generics(generics: &Generics, ctx: &PrettyCtx<'_>) -> String {
    if generics.params.is_empty() {
        return String::new();
    }

    let params = generics
        .params
        .iter()
        .map(|param| match &param.kind {
            GenericParamKind::Type { default } => {
                if ctx.options.show_types {
                    if let Some(default) = default {
                        format!("{} = {}", param.name, fmt_type_expr(default, ctx))
                    } else {
                        String::from(param.name.clone())
                    }
                } else {
                    String::from(param.name.clone())
                }
            }
            GenericParamKind::Const { ty } => {
                if ctx.options.show_types {
                    format!("const {}: {}", param.name, fmt_type_expr(ty, ctx))
                } else {
                    format!("const {}", param.name)
                }
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("<{}>", params)
}

fn fmt_params(params: &[super::Param], ctx: &PrettyCtx<'_>) -> Vec<String> {
    params
        .iter()
        .map(|param| format_param(param, ctx))
        .collect()
}

fn format_param(param: &super::Param, ctx: &PrettyCtx<'_>) -> String {
    let pat = format_pat(&param.pat, ctx);
    if ctx.options.show_types {
        let ty = fmt_type_expr(&param.ty, ctx);
        format!("{}: {}", pat, ty)
    } else {
        pat
    }
}

fn fmt_type_expr(ty: &TypeExpr, ctx: &PrettyCtx<'_>) -> String {
    match &ty.kind {
        TypeExprKind::Primitive(prim) => fmt_type_primitive(prim),
        TypeExprKind::Path(path) => fmt_path(path, ctx),
        TypeExprKind::Structural(structural) => {
            let fields = structural
                .fields
                .iter()
                .map(|field| format!("{}: {}", field.name, fmt_type_expr(&field.ty, ctx)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("struct {{ {} }}", fields)
        }
        TypeExprKind::TypeBinaryOp(op) => format!(
            "{} {} {}",
            fmt_type_expr(&op.lhs, ctx),
            fmt_type_binary_op_kind(op.kind),
            fmt_type_expr(&op.rhs, ctx)
        ),
        TypeExprKind::Tuple(elems) => {
            let elems = elems
                .iter()
                .map(|elem| fmt_type_expr(elem, ctx))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", elems)
        }
        TypeExprKind::Array(elem, len) => {
            let len_str = len
                .as_ref()
                .map(|expr| format_expr_inline(expr, ctx))
                .unwrap_or_else(|| "_".into());
            format!("[{}; {}]", fmt_type_expr(elem, ctx), len_str)
        }
        TypeExprKind::Slice(elem) => format!("[{}]", fmt_type_expr(elem, ctx)),
        TypeExprKind::Ptr(inner) => format!("*{}", fmt_type_expr(inner, ctx)),
        TypeExprKind::Ref(inner) => format!("&{}", fmt_type_expr(inner, ctx)),
        TypeExprKind::FnPtr(fn_ptr) => {
            let inputs = fn_ptr
                .inputs
                .iter()
                .map(|ty| fmt_type_expr(ty, ctx))
                .collect::<Vec<_>>();
            let inputs_str = inputs.join(", ");
            let output_str = fmt_type_expr(&fn_ptr.output, ctx);
            format!("fn({}) -> {}", inputs_str, output_str)
        }
        TypeExprKind::Never => "!".into(),
        TypeExprKind::Infer => "_".into(),
        TypeExprKind::Error => "<error>".into(),
    }
}

fn fmt_type_binary_op_kind(kind: crate::ast::TypeBinaryOpKind) -> &'static str {
    match kind {
        crate::ast::TypeBinaryOpKind::Add => "+",
        crate::ast::TypeBinaryOpKind::Intersect => "&",
        crate::ast::TypeBinaryOpKind::Subtract => "-",
        crate::ast::TypeBinaryOpKind::Union => "|",
    }
}

fn fmt_visibility(vis: &Visibility) -> &'static str {
    match vis {
        Visibility::Public => "pub ",
        Visibility::Private => "",
    }
}

fn fmt_bin_op(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Rem => "%",
        BinOp::And => "&&",
        BinOp::Or => "||",
        BinOp::BitXor => "^",
        BinOp::BitAnd => "&",
        BinOp::BitOr => "|",
        BinOp::Shl => "<<",
        BinOp::Shr => ">>",
        BinOp::Eq => "==",
        BinOp::Ne => "!=",
        BinOp::Lt => "<",
        BinOp::Le => "<=",
        BinOp::Gt => ">",
        BinOp::Ge => ">=",
    }
}

fn fmt_un_op(op: &UnOp) -> &'static str {
    match op {
        UnOp::Not => "!",
        UnOp::Neg => "-",
        UnOp::Deref => "*",
    }
}

fn fmt_type_primitive(prim: &crate::ast::TypePrimitive) -> String {
    use crate::ast::{DecimalType, TypeInt, TypePrimitive};

    match prim {
        TypePrimitive::Int(int_ty) => match int_ty {
            TypeInt::I64 => "i64",
            TypeInt::U64 => "u64",
            TypeInt::I32 => "i32",
            TypeInt::U32 => "u32",
            TypeInt::I16 => "i16",
            TypeInt::U16 => "u16",
            TypeInt::I8 => "i8",
            TypeInt::U8 => "u8",
            TypeInt::BigInt => "BigInt",
        }
        .to_string(),
        TypePrimitive::Decimal(dec_ty) => match dec_ty {
            DecimalType::F64 => "f64",
            DecimalType::F32 => "f32",
            DecimalType::BigDecimal => "BigDecimal",
            DecimalType::Decimal { precision, scale } => {
                return format!("Decimal({}, {})", precision, scale);
            }
        }
        .to_string(),
        TypePrimitive::Bool => "bool".to_string(),
        TypePrimitive::Char => "char".to_string(),
        TypePrimitive::String => "&str".to_string(),
        TypePrimitive::List => "List".to_string(),
    }
}
