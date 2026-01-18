//! Language-agnostic AST pretty printer.
//!
//! Provides structured formatting for AST nodes without relying on the
//! thread-local serializer or `Debug` output. The goal is to offer a stable and
//! human-friendly tree representation for diagnostics and debugging utilities.

use std::fmt::{self, Formatter, Write};

use crate::ast;
use crate::ast::{
    Pattern, PatternKind, PatternStructField, SchemaDocument, SchemaKind, SchemaNode,
};
use crate::intrinsics::IntrinsicCallKind;
use crate::pretty::{escape_char, escape_string, PrettyCtx, PrettyPrintable};
use crate::query;

impl PrettyPrintable for ast::Expr {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let suffix = ty_suffix(self.ty.as_ref(), ctx);

        match &self.kind {
            ast::ExprKind::Id(id) => ctx.writeln(f, format!("id({}){}", id, suffix)),
            ast::ExprKind::Locator(locator) => {
                ctx.writeln(f, format!("locator {}{}", locator, suffix))
            }
            ast::ExprKind::Value(value) => ctx.writeln(
                f,
                format!("value {}{}", summarize_value(value.as_ref()), suffix),
            ),
            ast::ExprKind::Block(block) => {
                let count = block.stmts.len();
                let plural = if count == 1 { "" } else { "s" };
                ctx.writeln(f, format!("block ({} stmt{}){}", count, plural, suffix))?;
                ctx.with_indent(|ctx| {
                    for stmt in &block.stmts {
                        fmt_block_stmt(stmt, f, ctx)?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::Match(m) => {
                ctx.writeln(f, format!("match{}", suffix))?;
                ctx.with_indent(|ctx| {
                    for (idx, case) in m.cases.iter().enumerate() {
                        ctx.writeln(f, format!("case #{}", idx))?;
                        ctx.with_indent(|ctx| {
                            ctx.writeln(f, "cond:")?;
                            ctx.with_indent(|ctx| case.cond.fmt_pretty(f, ctx))?;
                            ctx.writeln(f, "body:")?;
                            ctx.with_indent(|ctx| case.body.fmt_pretty(f, ctx))
                        })?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::If(expr_if) => {
                ctx.writeln(f, format!("if{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "cond:")?;
                    ctx.with_indent(|ctx| expr_if.cond.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "then:")?;
                    ctx.with_indent(|ctx| expr_if.then.fmt_pretty(f, ctx))?;
                    if let Some(elze) = &expr_if.elze {
                        ctx.writeln(f, "else:")?;
                        ctx.with_indent(|ctx| elze.fmt_pretty(f, ctx))?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::Loop(expr_loop) => {
                let label = expr_loop
                    .label
                    .as_ref()
                    .map(|ident| format!(" {}", ident))
                    .unwrap_or_default();
                ctx.writeln(f, format!("loop{}{}", label, suffix))?;
                ctx.with_indent(|ctx| expr_loop.body.fmt_pretty(f, ctx))
            }
            ast::ExprKind::While(expr_while) => {
                ctx.writeln(f, format!("while{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "cond:")?;
                    ctx.with_indent(|ctx| expr_while.cond.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "body:")?;
                    ctx.with_indent(|ctx| expr_while.body.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Invoke(invoke) => {
                ctx.writeln(f, format!("invoke{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(
                        f,
                        format!("target: {}", render_invoke_target(&invoke.target)),
                    )?;
                    if !invoke.args.is_empty() {
                        ctx.writeln(f, "args:")?;
                        ctx.with_indent(|ctx| {
                            for arg in &invoke.args {
                                arg.fmt_pretty(f, ctx)?;
                            }
                            Ok(())
                        })?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::IntrinsicContainer(collection) => {
                ctx.writeln(f, format!("intrinsic_container{}", suffix))?;
                let expanded = collection.clone().into_const_expr();
                ctx.with_indent(|ctx| expanded.fmt_pretty(f, ctx))
            }
            ast::ExprKind::BinOp(binop) => {
                ctx.writeln(f, format!("binop {}{}", binop.kind, suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "lhs:")?;
                    ctx.with_indent(|ctx| binop.lhs.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "rhs:")?;
                    ctx.with_indent(|ctx| binop.rhs.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::For(for_expr) => {
                ctx.writeln(f, format!("for{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "iter:")?;
                    ctx.with_indent(|ctx| for_expr.iter.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "body:")?;
                    ctx.with_indent(|ctx| for_expr.body.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Async(async_expr) => {
                ctx.writeln(f, format!("async{}", suffix))?;
                ctx.with_indent(|ctx| async_expr.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::UnOp(unop) => {
                ctx.writeln(f, format!("unop {}{}", unop.op, suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "value:")?;
                    ctx.with_indent(|ctx| unop.val.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Assign(assign) => {
                ctx.writeln(f, format!("assign{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "target:")?;
                    ctx.with_indent(|ctx| assign.target.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "value:")?;
                    ctx.with_indent(|ctx| assign.value.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Select(select) => {
                let selector = render_select_kind(&select.select);
                ctx.writeln(
                    f,
                    format!("select .{} [{}]{}", select.field, selector, suffix),
                )?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "object:")?;
                    ctx.with_indent(|ctx| select.obj.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Index(idx) => {
                ctx.writeln(f, format!("index{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "value:")?;
                    ctx.with_indent(|ctx| idx.obj.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "index:")?;
                    ctx.with_indent(|ctx| idx.index.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Struct(expr_struct) => {
                ctx.writeln(
                    f,
                    format!(
                        "struct {}{}",
                        render_expr_inline(expr_struct.name.as_ref()),
                        suffix
                    ),
                )?;
                ctx.with_indent(|ctx| {
                    fmt_expr_fields(&expr_struct.fields, f, ctx)?;
                    if let Some(update) = &expr_struct.update {
                        ctx.writeln(f, "..")?;
                        ctx.with_indent(|ctx| update.fmt_pretty(f, ctx))?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::Structural(expr_structural) => {
                ctx.writeln(f, format!("structural{}", suffix))?;
                ctx.with_indent(|ctx| fmt_expr_fields(&expr_structural.fields, f, ctx))
            }
            ast::ExprKind::Reference(reference) => {
                let mutability = match reference.mutable {
                    Some(true) => "mut",
                    Some(false) => "const",
                    None => "unspecified",
                };
                ctx.writeln(f, format!("reference (mutable: {}){}", mutability, suffix))?;
                ctx.with_indent(|ctx| reference.referee.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Dereference(deref) => {
                ctx.writeln(f, format!("deref{}", suffix))?;
                ctx.with_indent(|ctx| deref.referee.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Tuple(tuple) => {
                ctx.writeln(
                    f,
                    format!("tuple ({} values){}", tuple.values.len(), suffix),
                )?;
                ctx.with_indent(|ctx| {
                    for value in &tuple.values {
                        value.fmt_pretty(f, ctx)?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::Try(expr_try) => {
                ctx.writeln(f, format!("try{}", suffix))?;
                ctx.with_indent(|ctx| expr_try.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Let(expr_let) => {
                ctx.writeln(
                    f,
                    format!("let {}{}", render_pattern(expr_let.pat.as_ref()), suffix),
                )?;
                ctx.with_indent(|ctx| expr_let.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Closure(closure) => {
                let params = closure
                    .params
                    .iter()
                    .map(render_pattern)
                    .collect::<Vec<_>>()
                    .join(", ");
                let movability = closure
                    .movability
                    .map(|flag| if flag { "move " } else { "" })
                    .unwrap_or_default();
                let ret_ty = closure
                    .ret_ty
                    .as_ref()
                    .map(|ty| format!(" -> {}", render_ty_brief(ty)))
                    .unwrap_or_default();
                ctx.writeln(
                    f,
                    format!("closure {}({}){}{}", movability, params, ret_ty, suffix),
                )?;
                ctx.with_indent(|ctx| closure.body.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Array(array) => {
                ctx.writeln(
                    f,
                    format!("array ({} values){}", array.values.len(), suffix),
                )?;
                ctx.with_indent(|ctx| {
                    for value in &array.values {
                        value.fmt_pretty(f, ctx)?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::ArrayRepeat(array) => {
                ctx.writeln(f, format!("array_repeat{}", suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "elem:")?;
                    ctx.with_indent(|ctx| array.elem.fmt_pretty(f, ctx))?;
                    ctx.writeln(f, "len:")?;
                    ctx.with_indent(|ctx| array.len.fmt_pretty(f, ctx))
                })
            }
            ast::ExprKind::Await(await_expr) => {
                ctx.writeln(f, format!("await{}", suffix))?;
                ctx.with_indent(|ctx| await_expr.base.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Cast(cast) => {
                ctx.writeln(
                    f,
                    format!("cast{} -> {}", suffix, render_ty_brief(&cast.ty)),
                )?;
                ctx.with_indent(|ctx| cast.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Return(ret) => {
                ctx.writeln(f, format!("return{}", suffix))?;
                if let Some(value) = &ret.value {
                    ctx.with_indent(|ctx| value.fmt_pretty(f, ctx))
                } else {
                    Ok(())
                }
            }
            ast::ExprKind::Break(brk) => {
                ctx.writeln(f, format!("break{}", suffix))?;
                if let Some(value) = &brk.value {
                    ctx.with_indent(|ctx| value.fmt_pretty(f, ctx))
                } else {
                    Ok(())
                }
            }
            ast::ExprKind::Continue(_) => ctx.writeln(f, format!("continue{}", suffix)),
            ast::ExprKind::ConstBlock(block) => {
                ctx.writeln(f, format!("const_block{}", suffix))?;
                ctx.with_indent(|ctx| block.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::IntrinsicCall(call) => {
                ctx.writeln(
                    f,
                    format!("intrinsic {}{}", render_intrinsic_kind(call.kind), suffix),
                )?;
                ctx.with_indent(|ctx| {
                    if call.args.is_empty() {
                        ctx.writeln(f, "args: []")?;
                    } else {
                        ctx.writeln(f, "args:")?;
                        ctx.with_indent(|ctx| {
                            for arg in &call.args {
                                arg.fmt_pretty(f, ctx)?;
                            }
                            Ok(())
                        })?;
                    }

                    if call.kwargs.is_empty() {
                        ctx.writeln(f, "kwargs: []")
                    } else {
                        ctx.writeln(f, "kwargs:")?;
                        ctx.with_indent(|ctx| {
                            for arg in &call.kwargs {
                                ctx.writeln(f, format!("{} =", arg.name))?;
                                ctx.with_indent(|ctx| arg.value.fmt_pretty(f, ctx))?;
                            }
                            Ok(())
                        })
                    }
                })
            }
            ast::ExprKind::Quote(q) => {
                let kind = q.kind.map(|k| format!(" {:?}", k)).unwrap_or_default();
                ctx.writeln(f, format!("quote{}{}", kind, suffix))?;
                ctx.with_indent(|ctx| ast::Expr::block(q.block.clone()).fmt_pretty(f, ctx))
            }
            ast::ExprKind::Splice(s) => {
                ctx.writeln(f, format!("splice{}", suffix))?;
                ctx.with_indent(|ctx| s.token.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Closured(closured) => {
                ctx.writeln(f, format!("closured{}", suffix))?;
                ctx.with_indent(|ctx| closured.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Paren(paren) => {
                ctx.writeln(f, format!("paren{}", suffix))?;
                ctx.with_indent(|ctx| paren.expr.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Range(range) => {
                let limit = match range.limit {
                    ast::ExprRangeLimit::Inclusive => "inclusive",
                    ast::ExprRangeLimit::Exclusive => "exclusive",
                };
                ctx.writeln(f, format!("range [{}]{}", limit, suffix))?;
                ctx.with_indent(|ctx| {
                    if let Some(start) = &range.start {
                        ctx.writeln(f, "start:")?;
                        ctx.with_indent(|ctx| start.fmt_pretty(f, ctx))?;
                    }
                    if let Some(end) = &range.end {
                        ctx.writeln(f, "end:")?;
                        ctx.with_indent(|ctx| end.fmt_pretty(f, ctx))?;
                    }
                    if let Some(step) = &range.step {
                        ctx.writeln(f, "step:")?;
                        ctx.with_indent(|ctx| step.fmt_pretty(f, ctx))?;
                    }
                    Ok(())
                })
            }
            ast::ExprKind::FormatString(fmt_string) => ctx.writeln(
                f,
                format!(
                    "format_string {}{}",
                    render_format_template(fmt_string),
                    suffix
                ),
            ),
            ast::ExprKind::Splat(splat) => {
                ctx.writeln(f, format!("splat{}", suffix))?;
                ctx.with_indent(|ctx| splat.iter.fmt_pretty(f, ctx))
            }
            ast::ExprKind::SplatDict(splat_dict) => {
                ctx.writeln(f, format!("splat_dict{}", suffix))?;
                ctx.with_indent(|ctx| splat_dict.dict.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Macro(mac) => {
                ctx.writeln(f, format!("macro {}{}", mac.invocation.path, suffix))?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(
                        f,
                        format!(
                            "delimiter: {:?}, tokens: {}",
                            mac.invocation.delimiter, mac.invocation.tokens
                        ),
                    )
                })
            }
            ast::ExprKind::Item(item) => {
                ctx.writeln(f, format!("item_expr{}", suffix))?;
                ctx.with_indent(|ctx| item.fmt_pretty(f, ctx))
            }
            ast::ExprKind::Any(_) => ctx.writeln(f, format!("expr.any{}", suffix)),
        }
    }
}

impl PrettyPrintable for ast::Item {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let suffix = ty_suffix(self.ty.as_ref(), ctx);

        match &self.kind {
            ast::ItemKind::Module(module) => {
                let mut header = format!(
                    "{}module {}",
                    visibility_prefix(&module.visibility),
                    module.name
                );
                header.push_str(&suffix);
                if module.is_external {
                    ctx.writeln(f, format!("{};", header))
                } else {
                    ctx.writeln(f, format!("{} {{", header))?;
                    ctx.with_indent(|ctx| {
                        for item in &module.items {
                            item.fmt_pretty(f, ctx)?;
                        }
                        Ok(())
                    })?;
                    ctx.writeln(f, "}")
                }
            }
            ast::ItemKind::Macro(mac) => {
                ctx.writeln(
                    f,
                    format!(
                        "macro item {} (delim: {:?})",
                        mac.invocation.path, mac.invocation.delimiter
                    ),
                )?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, format!("tokens: {}{}", mac.invocation.tokens, suffix))
                })
            }
            ast::ItemKind::DefStruct(def) => {
                ctx.writeln(
                    f,
                    format!(
                        "{}struct {}{} {{",
                        visibility_prefix(&def.visibility),
                        def.name,
                        suffix
                    ),
                )?;
                ctx.with_indent(|ctx| {
                    for field in &def.value.fields {
                        ctx.writeln(
                            f,
                            format!("{}: {}", field.name, render_ty_brief(&field.value)),
                        )?;
                    }
                    Ok(())
                })?;
                ctx.writeln(f, "}")
            }
            ast::ItemKind::DefStructural(def) => {
                ctx.writeln(
                    f,
                    format!(
                        "{}structural {}{}",
                        visibility_prefix(&def.visibility),
                        def.name,
                        suffix
                    ),
                )?;
                ctx.with_indent(|ctx| {
                    for field in &def.value.fields {
                        ctx.writeln(
                            f,
                            format!("{}: {}", field.name, render_ty_brief(&field.value)),
                        )?;
                    }
                    Ok(())
                })
            }
            ast::ItemKind::DefEnum(def) => {
                ctx.writeln(
                    f,
                    format!(
                        "{}enum {}{} {{",
                        visibility_prefix(&def.visibility),
                        def.name,
                        suffix
                    ),
                )?;
                ctx.with_indent(|ctx| {
                    for variant in &def.value.variants {
                        let mut line = String::new();
                        let _ = write!(&mut line, "{}", variant.name);
                        line.push_str(": ");
                        line.push_str(&render_ty_brief(&variant.value));
                        if let Some(expr) = &variant.discriminant {
                            line.push_str(" = ");
                            line.push_str(&render_expr_inline(expr));
                        }
                        ctx.writeln(f, line)?;
                    }
                    Ok(())
                })?;
                ctx.writeln(f, "}")
            }
            ast::ItemKind::DefType(def) => ctx.writeln(
                f,
                format!(
                    "{}type {} = {}{}",
                    visibility_prefix(&def.visibility),
                    def.name,
                    render_ty_brief(&def.value),
                    suffix
                ),
            ),
            ast::ItemKind::DefConst(def) => {
                let ty_display = def
                    .ty_annotation()
                    .or(def.ty.as_ref())
                    .map(|ty| render_ty_brief(ty));
                let mut line = format!(
                    "{}const {}{}",
                    visibility_prefix(&def.visibility),
                    if def.mutable.unwrap_or(false) {
                        "mut "
                    } else {
                        ""
                    },
                    def.name
                );
                if let Some(ty) = ty_display {
                    line.push_str(": ");
                    line.push_str(&ty);
                }
                line.push_str(&suffix);
                ctx.writeln(f, line)?;
                ctx.with_indent(|ctx| def.value.fmt_pretty(f, ctx))
            }
            ast::ItemKind::DefStatic(def) => {
                let ty_display = def
                    .ty_annotation()
                    .map(|ty| render_ty_brief(ty))
                    .unwrap_or_else(|| render_ty_brief(&def.ty));
                let line = format!(
                    "{}static {}: {}{}",
                    visibility_prefix(&def.visibility),
                    def.name,
                    ty_display,
                    suffix
                );
                ctx.writeln(f, line)?;
                ctx.with_indent(|ctx| def.value.fmt_pretty(f, ctx))
            }
            ast::ItemKind::DefFunction(def) => {
                let mut header = String::new();
                write!(
                    &mut header,
                    "{}{}{}",
                    visibility_prefix(&def.visibility),
                    if def.attrs.is_empty() { "" } else { "[attrs] " },
                    render_function_signature(&def.sig)
                )
                .unwrap_or(());
                if let Some(ty) = def.ty.as_ref() {
                    header.push_str(" : ");
                    header.push_str(&render_type_function(ty));
                }
                header.push_str(&suffix);
                ctx.writeln(f, header)?;
                ctx.with_indent(|ctx| def.body.fmt_pretty(f, ctx))
            }
            ast::ItemKind::DefTrait(def) => {
                let bounds = render_type_bounds(&def.bounds);
                let mut header =
                    format!("{}trait {}", visibility_prefix(&def.visibility), def.name);
                if !bounds.is_empty() {
                    header.push_str(": ");
                    header.push_str(&bounds);
                }
                header.push_str(&suffix);
                ctx.writeln(f, format!("{} {{", header))?;
                ctx.with_indent(|ctx| {
                    for item in &def.items {
                        item.fmt_pretty(f, ctx)?;
                    }
                    Ok(())
                })?;
                ctx.writeln(f, "}")
            }
            ast::ItemKind::DeclType(decl) => {
                let bounds = render_type_bounds(&decl.bounds);
                let mut line = format!("declare type {}", decl.name);
                if !bounds.is_empty() {
                    line.push_str(": ");
                    line.push_str(&bounds);
                }
                if let Some(ty) = decl.ty_annotation.as_ref() {
                    line.push_str(" = ");
                    line.push_str(&render_ty_brief(ty));
                }
                line.push_str(&suffix);
                ctx.writeln(f, line)
            }
            ast::ItemKind::DeclConst(decl) => {
                let mut line =
                    format!("declare const {}: {}", decl.name, render_ty_brief(&decl.ty));
                if let Some(annotation) = decl.ty_annotation.as_ref() {
                    line.push_str(" (annotation ");
                    line.push_str(&render_ty_brief(annotation));
                    line.push(')');
                }
                line.push_str(&suffix);
                ctx.writeln(f, line)
            }
            ast::ItemKind::DeclStatic(decl) => {
                let mut line = format!(
                    "declare static {}: {}",
                    decl.name,
                    render_ty_brief(&decl.ty)
                );
                if let Some(annotation) = decl.ty_annotation.as_ref() {
                    line.push_str(" (annotation ");
                    line.push_str(&render_ty_brief(annotation));
                    line.push(')');
                }
                line.push_str(&suffix);
                ctx.writeln(f, line)
            }
            ast::ItemKind::DeclFunction(decl) => {
                let mut line = format!("declare {}", render_function_signature(&decl.sig));
                if let Some(annotation) = decl.ty_annotation.as_ref() {
                    line.push_str(" : ");
                    line.push_str(&render_ty_brief(annotation));
                }
                line.push_str(&suffix);
                ctx.writeln(f, line)
            }
            ast::ItemKind::Import(import) => ctx.writeln(
                f,
                format!(
                    "{}import {}{}",
                    visibility_prefix(&import.visibility),
                    import.tree,
                    suffix
                ),
            ),
            ast::ItemKind::Impl(item_impl) => {
                let generics = render_generic_params(&item_impl.generics_params);
                let trait_part = item_impl
                    .trait_ty
                    .as_ref()
                    .map(|locator| locator.to_string())
                    .unwrap_or_default();
                let mut header = if generics.is_empty() {
                    String::from("impl ")
                } else {
                    format!("impl{} ", generics)
                };
                if !trait_part.is_empty() {
                    header.push_str(&trait_part);
                    header.push_str(" for ");
                }
                header.push_str(&render_expr_inline(&item_impl.self_ty));
                header.push_str(&suffix);
                ctx.writeln(f, format!("{} {{", header))?;
                ctx.with_indent(|ctx| {
                    for item in &item_impl.items {
                        item.fmt_pretty(f, ctx)?;
                    }
                    Ok(())
                })?;
                ctx.writeln(f, "}")
            }
            ast::ItemKind::Expr(expr) => {
                ctx.writeln(f, format!("expr_item{}", suffix))?;
                ctx.with_indent(|ctx| expr.fmt_pretty(f, ctx))
            }
            ast::ItemKind::Any(_) => ctx.writeln(f, format!("item.any{}", suffix)),
        }
    }
}

impl PrettyPrintable for ast::File {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "ast::File {")?;
        ctx.with_indent(|ctx| {
            if !self.items.is_empty() {
                ctx.writeln(f, "items:")?;
                ctx.with_indent(|ctx| {
                    for item in &self.items {
                        item.fmt_pretty(f, ctx)?;
                    }
                    Ok(())
                })?;
            }
            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}

impl PrettyPrintable for ast::Node {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        match &self.kind {
            ast::NodeKind::File(file) => file.fmt_pretty(f, ctx),
            ast::NodeKind::Item(item) => item.fmt_pretty(f, ctx),
            ast::NodeKind::Expr(expr) => expr.fmt_pretty(f, ctx),
            ast::NodeKind::Query(query) => query.fmt_pretty(f, ctx),
            ast::NodeKind::Schema(schema) => schema.fmt_pretty(f, ctx),
            ast::NodeKind::Workspace(workspace) => {
                ctx.writeln(f, format!("workspace {}", workspace.manifest))?;
                ctx.with_indent(|ctx| {
                    for package in &workspace.packages {
                        let version = package
                            .version
                            .as_deref()
                            .map(|v| format!(" {}", v))
                            .unwrap_or_default();
                        ctx.writeln(f, format!("package {}{}", package.name, version))?;
                        ctx.with_indent(|ctx| {
                            ctx.writeln(f, format!("manifest: {}", package.manifest_path))?;
                            ctx.writeln(f, format!("root: {}", package.root))?;
                            if !package.features.is_empty() {
                                ctx.writeln(
                                    f,
                                    format!("features: {}", package.features.join(", ")),
                                )?;
                            }
                            if !package.dependencies.is_empty() {
                                let deps = package
                                    .dependencies
                                    .iter()
                                    .map(|dep| {
                                        dep.kind
                                            .as_deref()
                                            .map(|kind| format!("{} ({kind})", dep.name))
                                            .unwrap_or_else(|| dep.name.clone())
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                ctx.writeln(f, format!("dependencies: {}", deps))?;
                            }
                            if !package.modules.is_empty() {
                                ctx.writeln(f, "modules:")?;
                                ctx.with_indent(|ctx| {
                                    for module in &package.modules {
                                        let language =
                                            module.language.as_deref().unwrap_or("unknown");
                                        ctx.writeln(f, format!("{} [{}]", module.path, language))?;
                                    }
                                    Ok(())
                                })?;
                            }
                            Ok(())
                        })?;
                    }
                    Ok(())
                })
            }
        }
    }
}

fn ty_suffix(ty: Option<&ast::Ty>, ctx: &PrettyCtx<'_>) -> String {
    if ctx.options.show_types {
        if let Some(ty) = ty {
            format!(" : {}", render_ty_brief(ty))
        } else {
            " : _".to_string()
        }
    } else {
        String::new()
    }
}

fn render_ty_brief(ty: &ast::Ty) -> String {
    match ty {
        ast::Ty::Primitive(primitive) => match primitive {
            ast::TypePrimitive::Int(int_ty) => match int_ty {
                ast::TypeInt::I64 => "i64".into(),
                ast::TypeInt::U64 => "u64".into(),
                ast::TypeInt::I32 => "i32".into(),
                ast::TypeInt::U32 => "u32".into(),
                ast::TypeInt::I16 => "i16".into(),
                ast::TypeInt::U16 => "u16".into(),
                ast::TypeInt::I8 => "i8".into(),
                ast::TypeInt::U8 => "u8".into(),
                ast::TypeInt::BigInt => "bigint".into(),
            },
            ast::TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
                ast::DecimalType::F64 => "f64".into(),
                ast::DecimalType::F32 => "f32".into(),
                ast::DecimalType::BigDecimal => "bigdecimal".into(),
                ast::DecimalType::Decimal { precision, scale } => {
                    format!("decimal({}, {})", precision, scale)
                }
            },
            ast::TypePrimitive::Bool => "bool".into(),
            ast::TypePrimitive::Char => "char".into(),
            ast::TypePrimitive::String => "string".into(),
            ast::TypePrimitive::List => "list".into(),
        },
        ast::Ty::Struct(struct_ty) => {
            let mut result = format!("{}", struct_ty.name);
            if !struct_ty.fields.is_empty() {
                result.push('{');
                let entries = struct_ty
                    .fields
                    .iter()
                    .map(|field| format!("{}: {}", field.name, render_ty_brief(&field.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                result.push_str(&entries);
                result.push('}');
            }
            result
        }
        ast::Ty::Structural(structural) => {
            let mut result = String::from("structural{");
            let entries = structural
                .fields
                .iter()
                .map(|field| format!("{}: {}", field.name, render_ty_brief(&field.value)))
                .collect::<Vec<_>>()
                .join(", ");
            result.push_str(&entries);
            result.push('}');
            result
        }
        ast::Ty::Enum(enum_ty) => {
            let variants = enum_ty
                .variants
                .iter()
                .map(|variant| format!("{}: {}", variant.name, render_ty_brief(&variant.value)))
                .collect::<Vec<_>>()
                .join(" | ");
            format!("{} [{}]", enum_ty.name, variants)
        }
        ast::Ty::Function(func_ty) => {
            let generics = render_generic_params(&func_ty.generics_params);
            let params = func_ty
                .params
                .iter()
                .map(render_ty_brief)
                .collect::<Vec<_>>()
                .join(", ");
            let ret = func_ty
                .ret_ty
                .as_ref()
                .map(|ty| render_ty_brief(ty))
                .unwrap_or_else(|| "()".into());
            if generics.is_empty() {
                format!("fn({}) -> {}", params, ret)
            } else {
                format!("fn{}({}) -> {}", generics, params, ret)
            }
        }
        ast::Ty::ImplTraits(bounds) => format!("impl {}", render_type_bounds(&bounds.bounds)),
        ast::Ty::TypeBounds(bounds) => render_type_bounds(bounds),
        ast::Ty::Value(value) => format!("value {}", summarize_value(value.value.as_ref())),
        ast::Ty::Tuple(tuple) => {
            let content = tuple
                .types
                .iter()
                .map(render_ty_brief)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", content)
        }
        ast::Ty::Vec(vec_ty) => format!("Vec<{}>", render_ty_brief(vec_ty.ty.as_ref())),
        ast::Ty::Array(array_ty) => format!(
            "[{}; {}]",
            render_ty_brief(array_ty.elem.as_ref()),
            render_expr_inline(array_ty.len.as_ref())
        ),
        ast::Ty::Any(_) => "any".into(),
        ast::Ty::Unit(_) => "()".into(),
        ast::Ty::Unknown(_) => "unknown".into(),
        ast::Ty::Nothing(_) => "!".into(),
        ast::Ty::Type(_) => "type".into(),
        ast::Ty::Reference(reference) => {
            let mut out = String::from("&");
            if let Some(lifetime) = &reference.lifetime {
                let _ = write!(&mut out, "'{} ", lifetime);
            }
            if reference.mutability.unwrap_or(false) {
                out.push_str("mut ");
            }
            out.push_str(&render_ty_brief(reference.ty.as_ref()));
            out
        }
        ast::Ty::Slice(slice) => format!("[{}]", render_ty_brief(slice.elem.as_ref())),
        ast::Ty::Expr(expr) => format!("Expr({})", render_expr_inline(expr)),
        ast::Ty::Quote(quote) => {
            let kind = match quote.kind {
                ast::QuoteFragmentKind::Expr => "expr",
                ast::QuoteFragmentKind::Stmt => "stmt",
                ast::QuoteFragmentKind::Item => "item",
                ast::QuoteFragmentKind::Type => "type",
            };
            let item = match quote.item {
                Some(ast::QuoteItemKind::Function) => ":fn",
                Some(ast::QuoteItemKind::Struct) => ":struct",
                Some(ast::QuoteItemKind::Enum) => ":enum",
                Some(ast::QuoteItemKind::Trait) => ":trait",
                Some(ast::QuoteItemKind::Impl) => ":impl",
                Some(ast::QuoteItemKind::Type) => ":type",
                Some(ast::QuoteItemKind::Const) => ":const",
                Some(ast::QuoteItemKind::Static) => ":static",
                Some(ast::QuoteItemKind::Module) => ":mod",
                Some(ast::QuoteItemKind::Use) => ":use",
                Some(ast::QuoteItemKind::Macro) => ":macro",
                None => "",
            };
            if let Some(inner) = &quote.inner {
                format!(
                    "Quote<{}{}>({})",
                    kind,
                    item,
                    render_ty_brief(inner.as_ref())
                )
            } else {
                format!("Quote<{}{}>", kind, item)
            }
        }
        ast::Ty::TypeBinaryOp(_) => "TypeBinaryOp".into(),
        ast::Ty::AnyBox(_) => "AnyBox".into(),
    }
}

impl PrettyPrintable for query::QueryDocument {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let name_suffix = self
            .name
            .as_ref()
            .map(|name| format!(" \"{}\"", name))
            .unwrap_or_default();
        match &self.kind {
            query::QueryKind::Sql(sql) => {
                let header = format!("query.sql[{}]{}", sql.dialect, name_suffix);
                ctx.writeln(f, header)?;
                ctx.with_indent(|ctx| {
                    if let Some(raw) = &sql.raw {
                        ctx.writeln(f, raw.trim())?;
                    } else if !sql.statements.is_empty() {
                        for (idx, stmt) in sql.statements.iter().enumerate() {
                            ctx.writeln(f, format!("{}: {}", idx, stmt))?;
                        }
                    } else {
                        ctx.writeln(f, "<empty>")?;
                    }
                    Ok(())
                })
            }
            query::QueryKind::Prql(prql) => {
                let target_suffix = prql
                    .target
                    .as_ref()
                    .map(|target| format!(" -> {}", target))
                    .unwrap_or_default();
                let header = format!("query.prql{}{}", target_suffix, name_suffix);
                ctx.writeln(f, header)?;
                ctx.with_indent(|ctx| {
                    if prql.pipeline.trim().is_empty() {
                        ctx.writeln(f, "<empty>")?;
                    } else {
                        ctx.writeln(f, prql.pipeline.trim())?;
                    }
                    if !prql.compiled.is_empty() {
                        ctx.writeln(f, "compiled:")?;
                        ctx.with_indent(|ctx| {
                            for (idx, stmt) in prql.compiled.iter().enumerate() {
                                ctx.writeln(f, format!("{}: {}", idx, stmt))?;
                            }
                            Ok(())
                        })?;
                    }
                    Ok(())
                })
            }
            query::QueryKind::Any(any) => {
                ctx.writeln(f, format!("query.any{} {:?}", name_suffix, any))
            }
        }
    }
}

impl PrettyPrintable for SchemaDocument {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let title = self.title.as_deref().unwrap_or("<schema>");
        ctx.writeln(f, format!("schema {title}"))?;
        ctx.with_indent(|ctx| self.root.fmt_pretty(f, ctx))
    }
}

impl PrettyPrintable for SchemaNode {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        if let Some(description) = &self.description {
            ctx.writeln(f, format!("description: {description}"))?;
        }
        match &self.kind {
            SchemaKind::Any => ctx.writeln(f, "type: any"),
            SchemaKind::Null => ctx.writeln(f, "type: null"),
            SchemaKind::Boolean => ctx.writeln(f, "type: boolean"),
            SchemaKind::Number => ctx.writeln(f, "type: number"),
            SchemaKind::Integer => ctx.writeln(f, "type: integer"),
            SchemaKind::String => ctx.writeln(f, "type: string"),
            SchemaKind::Reference(reference) => ctx.writeln(f, format!("ref: {}", reference.path)),
            SchemaKind::Array(array) => {
                ctx.writeln(f, "type: array")?;
                ctx.with_indent(|ctx| {
                    ctx.writeln(f, "items:")?;
                    ctx.with_indent(|ctx| array.items.fmt_pretty(f, ctx))
                })
            }
            SchemaKind::Object(object) => {
                ctx.writeln(f, "type: object")?;
                if !object.properties.is_empty() {
                    ctx.writeln(f, "properties:")?;
                    ctx.with_indent(|ctx| {
                        for (name, schema) in &object.properties {
                            ctx.writeln(f, format!("{name}:"))?;
                            ctx.with_indent(|ctx| schema.fmt_pretty(f, ctx))?;
                        }
                        Ok(())
                    })?;
                }
                if !object.required.is_empty() {
                    ctx.writeln(f, format!("required: {:?}", object.required))?;
                }
                if !object.additional_properties {
                    ctx.writeln(f, "additional_properties: false")?;
                }
                Ok(())
            }
        }
    }
}

fn render_type_bounds(bounds: &ast::TypeBounds) -> String {
    bounds
        .bounds
        .iter()
        .map(render_expr_inline)
        .collect::<Vec<_>>()
        .join(" + ")
}

fn render_generic_params(params: &[ast::GenericParam]) -> String {
    if params.is_empty() {
        String::new()
    } else {
        let inner = params
            .iter()
            .map(|param| {
                let mut line = param.name.to_string();
                let bounds = render_type_bounds(&param.bounds);
                if !bounds.is_empty() {
                    line.push_str(": ");
                    line.push_str(&bounds);
                }
                line
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("<{}>", inner)
    }
}

fn render_type_function(func: &ast::TypeFunction) -> String {
    let generics = render_generic_params(&func.generics_params);
    let params = func
        .params
        .iter()
        .map(render_ty_brief)
        .collect::<Vec<_>>()
        .join(", ");
    let ret = func
        .ret_ty
        .as_ref()
        .map(|ty| render_ty_brief(ty.as_ref()))
        .unwrap_or_else(|| "()".into());
    if generics.is_empty() {
        format!("fn({}) -> {}", params, ret)
    } else {
        format!("fn{}({}) -> {}", generics, params, ret)
    }
}

fn render_function_signature(sig: &ast::FunctionSignature) -> String {
    let name = sig
        .name
        .as_ref()
        .map(|ident| ident.to_string())
        .unwrap_or_else(|| "<anon>".into());
    let generics = render_generic_params(&sig.generics_params);
    let mut params = Vec::new();
    if let Some(receiver) = sig.receiver.as_ref() {
        params.push(render_function_receiver(receiver));
    }
    params.extend(sig.params.iter().map(render_function_param));
    let params = params.join(", ");
    let ret = sig
        .ret_ty
        .as_ref()
        .map(|ty| format!(" -> {}", render_ty_brief(ty)))
        .unwrap_or_default();
    let const_prefix = if sig.is_const { "const " } else { "" };
    format!("{}fn{} {}({}){}", const_prefix, generics, name, params, ret)
}

fn render_function_param(param: &ast::FunctionParam) -> String {
    let mut parts = String::new();
    if param.is_const {
        parts.push_str("const ");
    }
    if param.as_tuple {
        parts.push('*');
    }
    parts.push_str(param.name.as_str());
    parts.push_str(": ");
    parts.push_str(&render_ty_brief(&param.ty));
    if let Some(default) = &param.default {
        parts.push_str(" = ");
        parts.push_str(&summarize_value(default));
    }
    parts
}

fn render_function_receiver(receiver: &ast::FunctionParamReceiver) -> String {
    match receiver {
        ast::FunctionParamReceiver::Implicit => "self".into(),
        ast::FunctionParamReceiver::Value => "self".into(),
        ast::FunctionParamReceiver::MutValue => "mut self".into(),
        ast::FunctionParamReceiver::Ref => "&self".into(),
        ast::FunctionParamReceiver::RefStatic => "&'static self".into(),
        ast::FunctionParamReceiver::RefMut => "&mut self".into(),
        ast::FunctionParamReceiver::RefMutStatic => "&'static mut self".into(),
    }
}

fn summarize_value(value: &ast::Value) -> String {
    match value {
        ast::Value::Int(int_val) => int_val.value.to_string(),
        ast::Value::BigInt(int_val) => format!("{}ib", int_val.value),
        ast::Value::Bool(bool_val) => bool_val.value.to_string(),
        ast::Value::Decimal(decimal) => decimal.value.to_string(),
        ast::Value::BigDecimal(decimal) => format!("{}fb", decimal.value),
        ast::Value::Char(ch) => format!("'{}'", escape_char(ch.value)),
        ast::Value::String(string) => format!("\"{}\"", escape_string(&string.value)),
        ast::Value::List(list) => format!("[{} values]", list.values.len()),
        ast::Value::Map(map) => format!("{{{} entries}}", map.entries.len()),
        ast::Value::Bytes(bytes) => format!("bytes(len={})", bytes.value.len()),
        ast::Value::Pointer(ptr) => format!("ptr({})", ptr.value),
        ast::Value::Offset(offset) => format!("offset({})", offset.value),
        ast::Value::Unit(_) => "()".into(),
        ast::Value::Null(_) => "null".into(),
        ast::Value::Undefined(_) => "undefined".into(),
        ast::Value::None(_) => "None".into(),
        ast::Value::Some(some) => format!("Some({})", summarize_value(some.value.as_ref())),
        ast::Value::Option(option) => option
            .value
            .as_ref()
            .map(|inner| summarize_value(inner.as_ref()))
            .map(|inner| format!("Option({})", inner))
            .unwrap_or_else(|| "Option(None)".into()),
        ast::Value::Escaped(escaped) => {
            format!("escaped(size={}, align={})", escaped.size, escaped.align)
        }
        ast::Value::Type(ty) => format!("type {}", render_ty_brief(ty)),
        ast::Value::Struct(struct_val) => format!(
            "{} {{ {} fields }}",
            struct_val.ty.name,
            struct_val.structural.fields.len()
        ),
        ast::Value::Structural(structural) => {
            format!("structural {{ {} fields }}", structural.fields.len())
        }
        ast::Value::Function(func) => render_function_signature(&func.sig),
        ast::Value::Tuple(tuple) => {
            let inner = tuple
                .values
                .iter()
                .map(summarize_value)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        ast::Value::QuoteToken(token) => {
            let kind = match token.kind {
                ast::QuoteFragmentKind::Expr => "expr",
                ast::QuoteFragmentKind::Stmt => "stmt",
                ast::QuoteFragmentKind::Item => "item",
                ast::QuoteFragmentKind::Type => "type",
            };
            format!("quote<{}>", kind)
        }
        ast::Value::Expr(expr) => format!("expr({})", render_expr_inline(expr)),
        ast::Value::BinOpKind(kind) => format!("operator {}", kind),
        ast::Value::UnOpKind(kind) => format!("operator {}", kind),
        ast::Value::Any(_) => "<any>".into(),
    }
}

fn render_expr_inline(expr: &ast::Expr) -> String {
    match &expr.kind {
        ast::ExprKind::Id(id) => format!("id({})", id),
        ast::ExprKind::Locator(locator) => locator.to_string(),
        ast::ExprKind::Value(value) => summarize_value(value.as_ref()),
        ast::ExprKind::BinOp(binop) => format!(
            "({} {} {})",
            render_expr_inline(binop.lhs.as_ref()),
            binop.kind,
            render_expr_inline(binop.rhs.as_ref())
        ),
        ast::ExprKind::UnOp(unop) => {
            format!("({}{})", unop.op, render_expr_inline(unop.val.as_ref()))
        }
        ast::ExprKind::Assign(assign) => format!(
            "{} = {}",
            render_expr_inline(assign.target.as_ref()),
            render_expr_inline(assign.value.as_ref())
        ),
        ast::ExprKind::Select(select) => format!(
            "{}.{}",
            render_expr_inline(select.obj.as_ref()),
            select.field
        ),
        ast::ExprKind::Index(index) => format!(
            "{}[{}]",
            render_expr_inline(index.obj.as_ref()),
            render_expr_inline(index.index.as_ref())
        ),
        ast::ExprKind::Invoke(invoke) => {
            let args = invoke
                .args
                .iter()
                .map(render_expr_inline)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", render_invoke_target(&invoke.target), args)
        }
        ast::ExprKind::Struct(expr_struct) => {
            let update = if expr_struct.update.is_some() {
                " .."
            } else {
                ""
            };
            format!(
                "{} {{ ...{} }}",
                render_expr_inline(expr_struct.name.as_ref()),
                update
            )
        }
        ast::ExprKind::Tuple(tuple) => tuple
            .values
            .iter()
            .map(render_expr_inline)
            .collect::<Vec<_>>()
            .join(", "),
        ast::ExprKind::Array(array) => format!(
            "[{}]",
            array
                .values
                .iter()
                .map(render_expr_inline)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ast::ExprKind::ArrayRepeat(array) => format!(
            "[{}; {}]",
            render_expr_inline(array.elem.as_ref()),
            render_expr_inline(array.len.as_ref())
        ),
        ast::ExprKind::Await(await_expr) => {
            format!("await {}", render_expr_inline(await_expr.base.as_ref()))
        }
        ast::ExprKind::Cast(cast) => format!(
            "({}) as {}",
            render_expr_inline(cast.expr.as_ref()),
            render_ty_brief(&cast.ty)
        ),
        ast::ExprKind::IntrinsicContainer(collection) => {
            render_expr_inline(&collection.clone().into_const_expr())
        }
        ast::ExprKind::Range(range) => {
            let start = range
                .start
                .as_ref()
                .map(|expr| render_expr_inline(expr.as_ref()))
                .unwrap_or_default();
            let end = range
                .end
                .as_ref()
                .map(|expr| render_expr_inline(expr.as_ref()))
                .unwrap_or_default();
            format!("{}..{}", start, end)
        }
        ast::ExprKind::FormatString(template) => render_format_template(template),
        ast::ExprKind::Return(_) => "return <expr>".into(),
        ast::ExprKind::Break(_) => "break <expr>".into(),
        ast::ExprKind::Continue(_) => "continue".into(),
        ast::ExprKind::ConstBlock(_) => "const { ... }".into(),
        ast::ExprKind::Async(_) => "async <expr>".into(),
        ast::ExprKind::For(_) => "for <expr>".into(),
        ast::ExprKind::Macro(mac) => format!("macro {}", mac.invocation.path),
        ast::ExprKind::Block(_)
        | ast::ExprKind::Match(_)
        | ast::ExprKind::If(_)
        | ast::ExprKind::Loop(_)
        | ast::ExprKind::While(_)
        | ast::ExprKind::Try(_)
        | ast::ExprKind::Let(_)
        | ast::ExprKind::Quote(_)
        | ast::ExprKind::Splice(_)
        | ast::ExprKind::Closure(_)
        | ast::ExprKind::IntrinsicCall(_)
        | ast::ExprKind::Closured(_)
        | ast::ExprKind::Paren(_)
        | ast::ExprKind::Splat(_)
        | ast::ExprKind::SplatDict(_)
        | ast::ExprKind::Item(_)
        | ast::ExprKind::Structural(_)
        | ast::ExprKind::Reference(_)
        | ast::ExprKind::Dereference(_)
        | ast::ExprKind::Any(_) => "<expr>".into(),
    }
}

fn fmt_block_stmt(
    stmt: &ast::BlockStmt,
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    match stmt {
        ast::BlockStmt::Item(item) => item.fmt_pretty(f, ctx),
        ast::BlockStmt::Let(stmt_let) => {
            ctx.writeln(f, format!("let {}", render_pattern(&stmt_let.pat)))?;
            ctx.with_indent(|ctx| {
                if let Some(init) = &stmt_let.init {
                    ctx.writeln(f, "init:")?;
                    ctx.with_indent(|ctx| init.fmt_pretty(f, ctx))?;
                }
                if let Some(diverge) = &stmt_let.diverge {
                    ctx.writeln(f, "diverge:")?;
                    ctx.with_indent(|ctx| diverge.fmt_pretty(f, ctx))?;
                }
                Ok(())
            })
        }
        ast::BlockStmt::Expr(expr_stmt) => {
            let semicolon = match expr_stmt.semicolon {
                Some(true) => ";",
                Some(false) => "(value)",
                None => "",
            };
            ctx.writeln(f, format!("expr_stmt {}", semicolon))?;
            ctx.with_indent(|ctx| expr_stmt.expr.fmt_pretty(f, ctx))
        }
        ast::BlockStmt::Noop => ctx.writeln(f, "noop"),
        ast::BlockStmt::Any(_) => ctx.writeln(f, "stmt.any"),
    }
}

fn render_pattern(pattern: &Pattern) -> String {
    let mut base = match pattern.kind() {
        PatternKind::Ident(ident) => {
            if ident.mutability.unwrap_or(false) {
                format!("mut {}", ident.ident)
            } else {
                ident.ident.to_string()
            }
        }
        PatternKind::Tuple(tuple) => {
            let inner = tuple
                .patterns
                .iter()
                .map(render_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        PatternKind::TupleStruct(tuple_struct) => {
            let inner = tuple_struct
                .patterns
                .iter()
                .map(render_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", render_locator(&tuple_struct.name), inner)
        }
        PatternKind::Struct(struct_pat) => {
            let fields = struct_pat
                .fields
                .iter()
                .map(render_pattern_field)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} {{ {} }}", struct_pat.name, fields)
        }
        PatternKind::Structural(structural) => {
            let fields = structural
                .fields
                .iter()
                .map(render_pattern_field)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {} }}", fields)
        }
        PatternKind::Box(bx) => format!("box {}", render_pattern(&bx.pattern)),
        PatternKind::Variant(variant) => {
            let mut out = render_expr_inline(&variant.name);
            if let Some(inner) = &variant.pattern {
                out.push('(');
                out.push_str(&render_pattern(inner));
                out.push(')');
            }
            out
        }
        PatternKind::Bind(bind) => {
            format!(
                "{} @ {}",
                render_pattern(&Pattern::from(PatternKind::Ident(bind.ident.clone()))),
                render_pattern(&bind.pattern)
            )
        }
        PatternKind::Quote(quote) => {
            let kind = match quote.fragment {
                ast::QuoteFragmentKind::Expr => "expr",
                ast::QuoteFragmentKind::Stmt => "stmt",
                ast::QuoteFragmentKind::Item => match quote.item {
                    Some(ast::QuoteItemKind::Function) => "fn",
                    Some(ast::QuoteItemKind::Struct) => "struct",
                    Some(ast::QuoteItemKind::Enum) => "enum",
                    Some(ast::QuoteItemKind::Trait) => "trait",
                    Some(ast::QuoteItemKind::Impl) => "impl",
                    Some(ast::QuoteItemKind::Type) => "type",
                    Some(ast::QuoteItemKind::Const) => "const",
                    Some(ast::QuoteItemKind::Static) => "static",
                    Some(ast::QuoteItemKind::Module) => "mod",
                    Some(ast::QuoteItemKind::Use) => "use",
                    Some(ast::QuoteItemKind::Macro) => "macro",
                    None => "item",
                },
                ast::QuoteFragmentKind::Type => "type",
            };
            if quote.fields.is_empty() {
                format!("quote<{}>", kind)
            } else {
                let mut parts = Vec::new();
                for field in &quote.fields {
                    parts.push(render_pattern_field(field));
                }
                if quote.has_rest {
                    parts.push("..".into());
                }
                format!("quote<{}> {{ {} }}", kind, parts.join(", "))
            }
        }
        PatternKind::QuotePlural(quote) => {
            let kind = match quote.fragment {
                ast::QuoteFragmentKind::Expr => "exprs",
                ast::QuoteFragmentKind::Stmt => "stmts",
                ast::QuoteFragmentKind::Item => "items",
                ast::QuoteFragmentKind::Type => "types",
            };
            let parts = quote
                .patterns
                .iter()
                .map(render_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("quote<{}>({})", kind, parts)
        }
        PatternKind::Type(typed) => {
            format!(
                "{}: {}",
                render_pattern(typed.pat.as_ref()),
                render_ty_brief(&typed.ty)
            )
        }
        PatternKind::Wildcard(_) => "_".into(),
    };
    if let Some(ty) = pattern.ty() {
        base.push_str(" : ");
        base.push_str(&render_ty_brief(ty));
    }
    base
}

fn render_locator(locator: &ast::Locator) -> String {
    locator.to_string()
}

fn render_pattern_field(field: &PatternStructField) -> String {
    if let Some(rename) = &field.rename {
        format!("{}: {}", field.name, render_pattern(rename))
    } else {
        field.name.to_string()
    }
}

fn render_invoke_target(target: &ast::ExprInvokeTarget) -> String {
    match target {
        ast::ExprInvokeTarget::Function(locator) => locator.to_string(),
        ast::ExprInvokeTarget::Type(ty) => render_ty_brief(ty),
        ast::ExprInvokeTarget::Method(select) => format!(
            "{}.{}",
            render_expr_inline(select.obj.as_ref()),
            select.field
        ),
        ast::ExprInvokeTarget::Closure(func) => render_function_signature(&func.sig),
        ast::ExprInvokeTarget::BinOp(kind) => format!("operator {}", kind),
        ast::ExprInvokeTarget::Expr(expr) => render_expr_inline(expr.as_ref()),
    }
}

fn fmt_expr_fields(
    fields: &[ast::ExprField],
    f: &mut Formatter<'_>,
    ctx: &mut PrettyCtx<'_>,
) -> fmt::Result {
    for field in fields {
        if let Some(value) = &field.value {
            ctx.writeln(f, format!("{}:", field.name))?;
            ctx.with_indent(|ctx| value.fmt_pretty(f, ctx))?;
        } else {
            ctx.writeln(f, format!("{} (shorthand)", field.name))?;
        }
    }
    Ok(())
}

fn render_select_kind(kind: &ast::ExprSelectType) -> &'static str {
    match kind {
        ast::ExprSelectType::Unknown => "unknown",
        ast::ExprSelectType::Field => "field",
        ast::ExprSelectType::Method => "method",
        ast::ExprSelectType::Function => "function",
        ast::ExprSelectType::Const => "const",
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
        IntrinsicCallKind::StructSize => "struct_size",
        IntrinsicCallKind::GenerateMethod => "generate_method",
        IntrinsicCallKind::CompileError => "compile_error",
        IntrinsicCallKind::CompileWarning => "compile_warning",
    }
}

fn render_format_template(template: &ast::ExprStringTemplate) -> String {
    let mut out = String::new();
    out.push('"');
    for part in &template.parts {
        out.push_str(&render_format_part(part));
    }
    out.push('"');
    out
}

fn render_format_part(part: &ast::FormatTemplatePart) -> String {
    match part {
        ast::FormatTemplatePart::Literal(text) => escape_string(text),
        ast::FormatTemplatePart::Placeholder(placeholder) => {
            format!("{{{}}}", render_format_placeholder(placeholder))
        }
    }
}

fn render_format_placeholder(placeholder: &ast::FormatPlaceholder) -> String {
    let mut out = render_format_arg_ref(&placeholder.arg_ref);
    if let Some(spec) = &placeholder.format_spec {
        out.push(':');
        out.push_str(&spec.raw);
    }
    out
}

fn render_format_arg_ref(arg_ref: &ast::FormatArgRef) -> String {
    match arg_ref {
        ast::FormatArgRef::Implicit => String::new(),
        ast::FormatArgRef::Positional(index) => index.to_string(),
        ast::FormatArgRef::Named(name) => name.clone(),
    }
}

fn visibility_prefix(vis: &ast::Visibility) -> &'static str {
    match vis {
        ast::Visibility::Public => "pub ",
        ast::Visibility::Crate => "pub(crate) ",
        ast::Visibility::Restricted(_) => "pub(in …) ",
        ast::Visibility::Private => "priv ",
        ast::Visibility::Inherited => "",
    }
}
