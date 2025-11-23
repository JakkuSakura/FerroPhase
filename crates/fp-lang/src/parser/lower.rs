use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprIntrinsicCall, ExprKind, ExprQuote, ExprSplice,
    Ident, Locator, QuoteFragmentKind, Value,
};
use fp_core::cst::{CstKind, CstNode};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use crate::lexer::winnow::{is_ident_start, is_ident_continue};

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
        CstKind::Root => {
            // If the root wraps a single child, attempt to
            // lower that child directly.
            if cst.children.len() == 1 {
                lower_expr_from_cst(&cst.children[0])
            } else {
                None
            }
        }
        CstKind::Quote => Some(lower_quote(cst)),
        CstKind::Splice => Some(lower_splice(cst)),
        CstKind::Block => Some(lower_block(cst)),
        CstKind::Token => lower_token(cst),
        CstKind::ConstBlock => Some(lower_const_block(cst)),
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

fn lower_const_block(cst: &CstNode) -> Expr {
    // Lower `const { ... }` into an intrinsic const-block call that
    // carries the inner block as its single argument, mirroring the
    // Rust-based parser's behaviour.
    let block = lower_block_child(cst).unwrap_or_else(ExprBlock::new);
    let block_expr = Expr::block(block);
    let call = ExprIntrinsicCall::new(
        IntrinsicCallKind::ConstBlock,
        IntrinsicCallPayload::Args { args: vec![block_expr] },
    );
    ExprKind::IntrinsicCall(call).into()
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
    // Integers
    if let Ok(value) = text.parse::<i64>() {
        return Some(Expr::value(Value::int(value)));
    }
    // String literals – very approximate handling: strip quotes.
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        let inner = text.trim_matches('"').to_string();
        return Some(Expr::value(Value::string(inner)));
    }
    // Bare identifiers
    let mut chars = text.chars();
    if let Some(first) = chars.next() {
        if is_ident_start(first) && chars.all(is_ident_continue) {
            let ident = Ident::new(text.clone());
            let locator = Locator::Ident(ident);
            return Some(Expr::locator(locator));
        }
    }
    // Otherwise, treat as opaque for now.
    None
}
