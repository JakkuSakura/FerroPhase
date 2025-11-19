use fp_core::ast::*;
use fp_typing::{AstTypeInferencer, TypingDiagnosticLevel};

fn make_quote_block(stmts: Vec<BlockStmt>, last_expr: Option<Expr>) -> Expr {
    let block = match last_expr {
        Some(e) if stmts.is_empty() => ExprBlock::new_expr(e),
        Some(e) => ExprBlock::new_stmts_expr(stmts, e),
        None => ExprBlock::new_stmts(stmts),
    };
    Expr::from(ExprKind::Quote(ExprQuote { block, kind: None }))
}

#[test]
fn quote_without_kind_infers_expr_when_trailing_expr_present() {
    let quote_expr = make_quote_block(vec![], Some(Expr::value(Value::int(42))));
    let mut node = Node::new(NodeKind::Expr(quote_expr));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors);
    match node.ty().expect("ty") {
        Ty::QuoteToken(qt) => {
            assert!(matches!(qt.kind, QuoteFragmentKind::Expr), "expected expr quote kind");
            assert!(qt.inner.is_some(), "expr quote token should carry inner type");
        }
        other => panic!("unexpected type for quote: {:?}", other),
    }
}

#[test]
fn quote_without_kind_infers_stmt_when_no_trailing_expr() {
    let quote_expr = make_quote_block(vec![BlockStmt::Noop], None);
    let mut node = Node::new(NodeKind::Expr(quote_expr));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors);
    match node.ty().expect("ty") {
        Ty::QuoteToken(qt) => {
            assert!(matches!(qt.kind, QuoteFragmentKind::Stmt), "expected stmt quote kind");
            assert!(qt.inner.is_none(), "stmt quote token should not carry inner type");
        }
        other => panic!("unexpected type for quote: {:?}", other),
    }
}

#[test]
fn splice_in_expr_requires_expr_quote_token() {
    // Build a splice with explicit expr token
    let block = ExprBlock::new_expr(Expr::value(Value::int(1)));
    let expr_token = Expr::from(ExprKind::Quote(ExprQuote { block, kind: Some(QuoteFragmentKind::Expr) }));
    let splice_ok = Expr::from(ExprKind::Splice(ExprSplice { token: Box::new(expr_token) }));
    let mut node_ok = Node::new(NodeKind::Expr(splice_ok));
    let mut typer = AstTypeInferencer::new();
    let out_ok = typer.infer(&mut node_ok).expect("infer ok");
    assert!(!out_ok.has_errors, "splice with expr token should type-check");

    // Build a splice with stmt token (no trailing expr)
    let stmt_token = make_quote_block(vec![BlockStmt::Noop], None);
    let splice_bad = Expr::from(ExprKind::Splice(ExprSplice { token: Box::new(stmt_token) }));
    let mut node_bad = Node::new(NodeKind::Expr(splice_bad));
    let mut typer2 = AstTypeInferencer::new();
    let out_bad = typer2.infer(&mut node_bad).expect("infer bad");
    assert!(out_bad.has_errors, "splice with non-expr token should error");
    assert!(out_bad.diagnostics.iter().any(|d| matches!(d.level, TypingDiagnosticLevel::Error)));
}
