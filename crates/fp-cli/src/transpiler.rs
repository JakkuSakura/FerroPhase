use std::path::PathBuf;

use fp_core::ast::{
    self, AstSerializer, BlockStmt, Expr, ExprBlock, ExprKind, Ident, Item, ItemKind, Node,
    NodeKind, Visibility,
};
use fp_core::error::Result;
use fp_csharp::CSharpSerializer;
use fp_javascript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_python::PythonSerializer;
use fp_rust::printer::RustPrinter;

/// Result of a transpilation run.
pub struct TranspileResult {
    pub code: String,
    pub type_defs: Option<String>,
}

/// Front-end selector that delegates to language-specific serializers.
pub struct Transpiler {
    target: TranspileTarget,
    emit_type_defs: bool,
}

impl Transpiler {
    pub fn new(target: TranspileTarget, emit_type_defs: bool) -> Self {
        Self {
            target,
            emit_type_defs,
        }
    }

    pub fn transpile(&self, node: &Node) -> Result<TranspileResult> {
        match self.target {
            TranspileTarget::TypeScript => {
                let serializer = TypeScriptSerializer::new(self.emit_type_defs);
                let code = serializer.serialize_node(node)?;
                let type_defs = serializer.take_type_defs();
                Ok(TranspileResult { code, type_defs })
            }
            TranspileTarget::JavaScript => {
                let serializer = JavaScriptSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::CSharp => {
                let serializer = CSharpSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::Python => {
                let serializer = PythonSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::Rust => render_rust(node),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TranspileTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Rust,
}

fn render_rust(node: &Node) -> Result<TranspileResult> {
    let printer = RustPrinter::new();
    let code = match node.kind() {
        NodeKind::File(file) => printer.serialize_file(file)?,
        NodeKind::Item(item) => printer.serialize_item(item)?,
        NodeKind::Expr(expr) => match expr.kind() {
            ExprKind::Block(block) => render_rust_from_block(&printer, block)?,
            _ => printer.serialize_expr(expr)?,
        },
    };

    Ok(TranspileResult {
        code,
        type_defs: None,
    })
}

fn render_rust_from_block(printer: &RustPrinter, block: &ExprBlock) -> Result<String> {
    let mut items: Vec<Item> = Vec::new();
    let mut pending_stmts: Vec<BlockStmt> = Vec::new();
    let mut saw_main = false;

    for stmt in &block.stmts {
        match stmt {
            BlockStmt::Item(item) => {
                let cloned = item.as_ref().clone();
                if let ItemKind::DefFunction(func) = cloned.kind() {
                    if func.name.name == "main" {
                        saw_main = true;
                    }
                }
                items.push(cloned);
            }
            BlockStmt::Let(_) | BlockStmt::Expr(_) => pending_stmts.push(stmt.clone()),
            BlockStmt::Noop => {}
            BlockStmt::Any(_) => {
                return Err(fp_core::error::Error::from(
                    "Normalization cannot process placeholder statements during transpile",
                ));
            }
        }
    }

    if !pending_stmts.is_empty() && !saw_main {
        let main_block = ExprBlock::new_stmts(pending_stmts);
        let body: ast::BExpr = Expr::block(main_block).into();
        let mut func = ast::ItemDefFunction::new_simple(Ident::new("main"), body);
        func.visibility = Visibility::Private;
        items.push(Item::new(ItemKind::DefFunction(func)));
    } else if !pending_stmts.is_empty() && saw_main {
        for stmt in pending_stmts {
            if let BlockStmt::Expr(expr_stmt) = stmt {
                items.push(Item::new(ItemKind::Expr((*expr_stmt.expr).clone())));
            }
        }
    }

    let file = ast::File {
        items,
        path: PathBuf::new(),
    };
    printer.serialize_file(&file)
}
