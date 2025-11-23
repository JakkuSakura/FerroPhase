use fp_core::ast::{BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprKind, ExprQuote, ExprSplice,
                   QuoteFragmentKind, Value};
use fp_core::cst::{CstKind, CstNode};

/// Lower a CST node directly into an fp-core AST expression.
///
/// This initial implementation focuses on the quoting surface:
/// - `quote { ... }` -> `ExprQuote { block, kind: None }`
/// - `splice ( ... )` -> `ExprSplice { token: <expr> }`
///
/// For any other shapes, this currently returns `None` and the
/// frontend falls back to the legacy `cst_to_source` pipeline.
pub fn lower_expr_from_cst(cst: &CstNode) -> Option<Expr> {
    match cst.kind {
        CstKind::Quote => Some(lower_quote(cst)),
        CstKind::Splice => Some(lower_splice(cst)),
        CstKind::Block => Some(lower_block(cst)),
        CstKind::Token => lower_token(cst),
        CstKind::Root | CstKind::ConstBlock => None,
    }
}

fn lower_quote(cst: &CstNode) -> Expr {
    // A quote node wraps a single block child in the current CST
    // grammar. We translate it into an ExprQuote with a block
    // expression body.
    let block = lower_block_child(cst).unwrap_or_else(ExprBlock::new);
    ExprKind::Quote(ExprQuote { block, kind: Some(QuoteFragmentKind::Expr) }).into()
}

fn lower_splice(cst: &CstNode) -> Expr {
    // Splice nodes wrap either a parenthesised expression, a
    // nested quote, or a block. For now we treat the first child
    // as the token expression and wrap it in ExprSplice.
    let token_expr = if let Some(first) = cst.children.first() {
        lower_expr_from_cst(first).unwrap_or_else(Expr::unit)
    } else {
        Expr::unit()
    };
    ExprKind::Splice(ExprSplice { token: Box::new(token_expr) }).into()
}

fn lower_block(cst: &CstNode) -> Expr {
    let mut stmts = Vec::new();
    for child in &cst.children {
        if let Some(expr) = lower_expr_from_cst(child) {
            let stmt = BlockStmt::Expr(BlockStmtExpr::new(expr));
            stmts.push(stmt);
        }
    }
    let block = ExprBlock::new_stmts(stmts);
    ExprKind::Block(block).into()
}

fn lower_block_child(cst: &CstNode) -> Option<ExprBlock> {
    cst.children
        .iter()
        .find(|child| matches!(child.kind, CstKind::Block))
        .map(lower_block_to_block)
}

fn lower_block_to_block(cst: &CstNode) -> ExprBlock {
    let mut stmts = Vec::new();
    for child in &cst.children {
        if let Some(expr) = lower_expr_from_cst(child) {
            stmts.push(BlockStmt::Expr(BlockStmtExpr::new(expr)));
        }
    }
    ExprBlock::new_stmts(stmts)
}

fn lower_token(cst: &CstNode) -> Option<Expr> {
    let text = cst.text.as_ref()?;
    // Very minimal literal lowering: integers and bare identifiers.
    if let Ok(value) = text.parse::<i64>() {
        return Some(Expr::value(Value::int(value)));
    }
    // For now, treat other tokens as opaque; in the quote surface
    // they will typically be wrapped in a block and interpreted
    // later.
    None
}

