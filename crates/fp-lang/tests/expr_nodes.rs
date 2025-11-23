use fp_core::ast::*;
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

fn unwrap_expr(expr_node: &Node) -> &Expr {
    match expr_node.kind() {
        NodeKind::Expr(e) => e,
        other => panic!("expected NodeKind::Expr, found {:?}", other),
    }
}

#[test]
fn parses_array_literal_elements() {
    let fe = FerroFrontend::new();
    let res = fe.parse("[1, 2, 3]", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let array = match e.kind() {
        ExprKind::Array(a) => a,
        other => panic!("expected ExprKind::Array, found {:?}", other),
    };
    assert_eq!(array.values.len(), 3);
}

#[test]
fn parses_array_literal_repeat() {
    let fe = FerroFrontend::new();
    let res = fe.parse("[0; 16]", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    match e.kind() {
        ExprKind::ArrayRepeat(repeat) => {
            // Just ensure both sides are present; detailed semantics are
            // validated in downstream passes.
            assert!(matches!(repeat.len.kind(), ExprKind::Value(_) | ExprKind::BinOp(_)));
        }
        other => panic!("expected ExprKind::ArrayRepeat, found {:?}", other),
    }
}

#[test]
fn parses_tuple_literal() {
    let fe = FerroFrontend::new();
    let res = fe.parse("(1, 2)", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let tuple = match e.kind() {
        ExprKind::Tuple(t) => t,
        other => panic!("expected ExprKind::Tuple, found {:?}", other),
    };
    assert_eq!(tuple.values.len(), 2);
}

#[test]
fn parens_still_group_not_tuple() {
    let fe = FerroFrontend::new();
    let res = fe.parse("(1 + 2)", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    // Ensure we did not introduce a tuple here.
    assert!(!matches!(e.kind(), ExprKind::Tuple(_)));
}

#[test]
fn parses_block_expression() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("{ let x = 1; x }", None)
        .expect("parse");
    let e = unwrap_expr(&res.ast);
    match e.kind() {
        ExprKind::Block(block) => {
            assert!(block.last_expr().is_some(), "block should have trailing expr");
        }
        other => panic!("expected ExprKind::Block, found {:?}", other),
    }
}

#[test]
fn parses_async_block_expression() {
    let fe = FerroFrontend::new();
    let res = fe.parse("async { 1 }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    // `async` is currently syntax sugar; ensure we still get a block.
    assert!(matches!(e.kind(), ExprKind::Block(_)));
}

#[test]
fn parses_struct_literal_with_explicit_values() {
    let fe = FerroFrontend::new();
    let res = fe.parse("Point { x: 1, y: 2 }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let s = match e.kind() {
        ExprKind::Struct(st) => st,
        other => panic!("expected ExprKind::Struct, found {:?}", other),
    };
    assert_eq!(s.fields.len(), 2);
    assert_eq!(s.fields[0].name.as_str(), "x");
    assert_eq!(s.fields[1].name.as_str(), "y");
}

#[test]
fn parses_struct_literal_with_shorthand_fields() {
    let fe = FerroFrontend::new();
    let res = fe.parse("Self { x, y }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let s = match e.kind() {
        ExprKind::Struct(st) => st,
        other => panic!("expected ExprKind::Struct, found {:?}", other),
    };
    assert_eq!(s.fields.len(), 2);
    assert_eq!(s.fields[0].name.as_str(), "x");
    assert_eq!(s.fields[1].name.as_str(), "y");
}

#[test]
fn parses_structural_literal_with_explicit_and_shorthand_fields() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("struct { x: 1, y }", None)
        .expect("parse");
    let e = unwrap_expr(&res.ast);
    let s = match e.kind() {
        ExprKind::Structural(st) => st,
        other => panic!("expected ExprKind::Structural, found {:?}", other),
    };
    assert_eq!(s.fields.len(), 2);
    assert_eq!(s.fields[0].name.as_str(), "x");
    assert_eq!(s.fields[1].name.as_str(), "y");
}

#[test]
fn parses_empty_structural_literal() {
    let fe = FerroFrontend::new();
    let res = fe.parse("struct {}", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let s = match e.kind() {
        ExprKind::Structural(st) => st,
        other => panic!("expected ExprKind::Structural, found {:?}", other),
    };
    assert_eq!(s.fields.len(), 0);
}

#[test]
fn parses_try_operator_on_identifier() {
    let fe = FerroFrontend::new();
    let res = fe.parse("value?", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    match e.kind() {
        ExprKind::Try(t) => {
            // Inner expression should be the original identifier.
            assert!(matches!(t.expr.kind(), ExprKind::Locator(_)));
        }
        other => panic!("expected ExprKind::Try, found {:?}", other),
    }
}
