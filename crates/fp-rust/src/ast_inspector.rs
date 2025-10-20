/// AST inspection utilities for debugging typed ASTs
use fp_core::ast::*;
use std::fmt::Write;

pub struct TypedAstInspector;

impl TypedAstInspector {
    /// Inspect a node and print all types
    pub fn inspect_node(node: &Node) -> String {
        let mut output = String::new();
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    Self::inspect_item(item, &mut output, 0);
                }
            }
            NodeKind::Item(item) => {
                Self::inspect_item(item, &mut output, 0);
            }
            NodeKind::Expr(expr) => {
                Self::inspect_expr(expr, &mut output, 0);
            }
            NodeKind::Query(query) => {
                writeln!(output, "Query document: {}", query.to_string_render()).ok();
            }
            NodeKind::Schema(schema) => {
                writeln!(
                    output,
                    "Schema document: {}",
                    schema.title.as_deref().unwrap_or("<schema>")
                )
                .ok();
            }
            NodeKind::Workspace(workspace) => {
                writeln!(
                    output,
                    "Workspace manifest: {} ({} package(s))",
                    workspace.manifest,
                    workspace.packages.len()
                )
                .ok();
            }
        }
        output
    }

    pub fn inspect_file(file: &File) -> String {
        let mut output = String::new();
        for item in &file.items {
            Self::inspect_item(item, &mut output, 0);
        }
        output
    }

    fn inspect_item(item: &Item, output: &mut String, indent: usize) {
        let ind = "  ".repeat(indent);
        match item.kind() {
            ItemKind::DefFunction(func) => {
                writeln!(output, "{}fn {} -> {:?}", ind, func.name, func.sig.ret_ty).ok();
                writeln!(output, "{}  Parameters:", ind).ok();
                for param in &func.sig.params {
                    writeln!(output, "{}    {}: {}", ind, param.name, param.ty).ok();
                }
                writeln!(output, "{}  Body:", ind).ok();
                Self::inspect_expr(&func.body, output, indent + 2);
            }
            _ => {
                writeln!(output, "{}{:?}", ind, item.kind()).ok();
            }
        }
    }

    fn inspect_expr(expr: &Expr, output: &mut String, indent: usize) {
        let ind = "  ".repeat(indent);
        let ty_str = expr.ty().map(|t| format!(" : {}", t)).unwrap_or_default();

        match expr.kind() {
            ExprKind::Locator(loc) => {
                writeln!(output, "{}Locator({}){}", ind, loc, ty_str).ok();
            }
            ExprKind::Invoke(invoke) => {
                writeln!(output, "{}Invoke{}", ind, ty_str).ok();
                writeln!(output, "{}  target:", ind).ok();
                match &invoke.target {
                    ExprInvokeTarget::Function(loc) => {
                        writeln!(output, "{}    Function({})", ind, loc).ok();
                    }
                    _ => {}
                }
                writeln!(output, "{}  args:", ind).ok();
                for arg in &invoke.args {
                    Self::inspect_expr(arg, output, indent + 2);
                }
            }
            ExprKind::Block(block) => {
                writeln!(output, "{}Block{}", ind, ty_str).ok();
                for stmt in &block.stmts {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        Self::inspect_expr(&expr_stmt.expr, output, indent + 1);
                    }
                }
            }
            _ => {
                writeln!(output, "{}{:?}{}", ind, expr.kind(), ty_str).ok();
            }
        }
    }
}
