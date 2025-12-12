use fp_core::ast::*;
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

#[test]
fn closure_param_typed_simple_path() {
    let fe = FerroFrontend::new();
    let res = fe.parse("let f = |x: foo::Bar| x;", None).expect("parse");

    let expr = match res.ast.kind() {
        NodeKind::Expr(e) => e,
        NodeKind::File(file) => file
            .items
            .first()
            .and_then(|it| match it.kind() {
                ItemKind::Expr(e) => Some(e),
                _ => None,
            })
            .expect("expected expr item"),
        other => panic!("expected Expr or File node, found {:?}", other),
    };

    let closure = match expr.kind() {
        ExprKind::Closure(c) => c,
        ExprKind::Block(b) => match b.stmts.first() {
            Some(BlockStmt::Let(stmt)) => match stmt.init.as_ref().unwrap().kind() {
                ExprKind::Closure(c) => c,
                other => panic!("expected closure in let init, got {:?}", other),
            },
            other => panic!("expected let stmt, got {:?}", other),
        },
        other => panic!("expected closure expr, found {:?}", other),
    };

    assert_eq!(closure.params.len(), 1);
    let pat = &closure.params[0];
    let ident = pat
        .as_ident()
        .expect("expected closure param to be an ident pattern");
    assert_eq!(ident.as_str(), "x");
}

#[test]
fn closure_param_typed_with_crate_super_segments() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("let f = |_: crate::foo::super::Bar| 0;", None)
        .expect("parse");

    let expr = match res.ast.kind() {
        NodeKind::Expr(e) => e,
        NodeKind::File(file) => file
            .items
            .first()
            .and_then(|it| match it.kind() {
                ItemKind::Expr(e) => Some(e),
                _ => None,
            })
            .expect("expected expr item"),
        other => panic!("expected Expr or File node, found {:?}", other),
    };

    let closure = match expr.kind() {
        ExprKind::Closure(c) => c,
        ExprKind::Block(b) => match b.stmts.first() {
            Some(BlockStmt::Let(stmt)) => match stmt.init.as_ref().unwrap().kind() {
                ExprKind::Closure(c) => c,
                other => panic!("expected closure in let init, got {:?}", other),
            },
            other => panic!("expected let stmt, got {:?}", other),
        },
        other => panic!("expected closure expr, found {:?}", other),
    };

    assert_eq!(closure.params.len(), 1);
}
