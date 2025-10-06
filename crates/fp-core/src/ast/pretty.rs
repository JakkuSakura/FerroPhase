use std::fmt::{self, Formatter};
use std::result::Result;

use crate::ast::{try_get_threadlocal_serializer, AstSerializer, Expr, File, Item, Node, NodeKind};
use crate::pretty::{PrettyCtx, PrettyPrintable};

fn render_with_serializer(
    render: impl FnOnce(&dyn AstSerializer) -> Result<String, crate::Error>,
) -> Result<String, fmt::Error> {
    let serializer = try_get_threadlocal_serializer().ok_or(fmt::Error)?;
    render(serializer.as_ref()).map_err(|_| fmt::Error)
}

fn emit_lines(ctx: &PrettyCtx<'_>, f: &mut Formatter<'_>, text: &str) -> fmt::Result {
    let mut lines = text.lines();
    if let Some(first) = lines.next() {
        ctx.write_indent(f)?;
        f.write_str(first)?;
        for line in lines {
            writeln!(f)?;
            ctx.write_indent(f)?;
            f.write_str(line)?;
        }
    }
    Ok(())
}

impl PrettyPrintable for Expr {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let rendered = render_with_serializer(|serializer| serializer.serialize_expr(self))?;
        emit_lines(ctx, f, rendered.as_str())
    }
}

impl PrettyPrintable for Item {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let rendered = render_with_serializer(|serializer| serializer.serialize_item(self))?;
        emit_lines(ctx, f, rendered.as_str())
    }
}

impl PrettyPrintable for File {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        let rendered = render_with_serializer(|serializer| serializer.serialize_file(self))?;
        emit_lines(ctx, f, rendered.as_str())
    }
}

impl PrettyPrintable for Node {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        match self.kind() {
            NodeKind::Item(item) => item.fmt_pretty(f, ctx),
            NodeKind::Expr(expr) => expr.fmt_pretty(f, ctx),
            NodeKind::File(file) => file.fmt_pretty(f, ctx),
        }
    }
}
