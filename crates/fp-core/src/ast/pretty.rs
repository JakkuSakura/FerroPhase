//! Language-agnostic AST pretty printer
//!
//! This printer wraps the existing serializer and adds optional type annotations.

use std::fmt::{self, Formatter};
use std::result::Result;

use crate::ast::{try_get_threadlocal_serializer, AstSerializer, Expr, File, Item, Node, NodeKind};
use crate::pretty::{PrettyCtx, PrettyPrintable};

/// Configuration for the AST printer
#[derive(Debug, Clone)]
pub struct AstPrinterConfig {
    /// Number of spaces per indentation level
    pub indent_size: usize,
    /// Whether to print type annotations
    pub show_types: bool,
    /// Whether to print source spans
    pub show_spans: bool,
    /// Maximum depth to print (None = unlimited)
    pub max_depth: Option<usize>,
}

impl Default for AstPrinterConfig {
    fn default() -> Self {
        Self {
            indent_size: 2,
            show_types: true,
            show_spans: false,
            max_depth: None,
        }
    }
}

/// AST pretty printer
pub struct AstPrinter {
    config: AstPrinterConfig,
}

impl AstPrinter {
    pub fn new(config: AstPrinterConfig) -> Self {
        Self { config }
    }

    pub fn print_node(&mut self, node: &Node) -> String {
        let serializer = match try_get_threadlocal_serializer() {
            Some(s) => s,
            None => return format!("{:?}", node),
        };

        let base_output = match node.kind() {
            NodeKind::File(file) => serializer.serialize_file(file),
            NodeKind::Item(item) => serializer.serialize_item(item),
            NodeKind::Expr(expr) => serializer.serialize_expr(expr),
        };

        match base_output {
            Ok(output) => {
                if self.config.show_types {
                    self.add_type_annotations(&output, node)
                } else {
                    output
                }
            }
            Err(_) => format!("{:?}", node),
        }
    }

    fn add_type_annotations(&self, output: &str, node: &Node) -> String {
        // For now, just append type info at the end if available
        if let Some(ty) = node.ty() {
            format!("{}\n// Type: {}", output, ty)
        } else {
            output.to_string()
        }
    }
}

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
        let rendered = render_with_serializer(|serializer| match self.kind() {
            NodeKind::File(file) => serializer.serialize_file(file),
            NodeKind::Item(item) => serializer.serialize_item(item),
            NodeKind::Expr(expr) => serializer.serialize_expr(expr),
        })?;
        emit_lines(ctx, f, rendered.as_str())
    }
}

/// Convenience function to print a node with default configuration
pub fn print_ast(node: &Node) -> String {
    let mut printer = AstPrinter::new(AstPrinterConfig::default());
    printer.print_node(node)
}

/// Convenience function to print a node without type annotations
pub fn print_ast_no_types(node: &Node) -> String {
    let config = AstPrinterConfig {
        show_types: false,
        ..Default::default()
    };
    let mut printer = AstPrinter::new(config);
    printer.print_node(node)
}

/// Convenience function to print a node with limited depth
pub fn print_ast_shallow(node: &Node, max_depth: usize) -> String {
    let config = AstPrinterConfig {
        max_depth: Some(max_depth),
        ..Default::default()
    };
    let mut printer = AstPrinter::new(config);
    printer.print_node(node)
}
