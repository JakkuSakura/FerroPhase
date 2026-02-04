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
            assert!(matches!(
                repeat.len.kind(),
                ExprKind::Value(_) | ExprKind::BinOp(_)
            ));
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
    assert_eq!(tuple.span, e.span());
    assert!(!tuple.span.is_null());
}

#[test]
fn parens_still_group_not_tuple() {
    let fe = FerroFrontend::new();
    let res = fe.parse("(1 + 2)", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    // Ensure we did not introduce a tuple here.
    assert!(!matches!(e.kind(), ExprKind::Tuple(_)));
    assert!(!e.span().is_null());
    if let ExprKind::Paren(paren) = e.kind() {
        let inner = paren.expr.span();
        let outer = e.span();
        assert!(outer.lo <= inner.lo, "outer span should start before inner");
        assert!(outer.hi >= inner.hi, "outer span should end after inner");
    }
}

#[test]
fn parses_block_expression() {
    let fe = FerroFrontend::new();
    let res = fe.parse("{ let x = 1; x }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    match e.kind() {
        ExprKind::Block(block) => {
            assert!(
                block.last_expr().is_some(),
                "block should have trailing expr"
            );
        }
        other => panic!("expected ExprKind::Block, found {:?}", other),
    }
}

#[test]
fn parses_tuple_destructuring_let() {
    let fe = FerroFrontend::new();
    let res = fe.parse("{ let (a, b) = (1, 2); a }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let block = match e.kind() {
        ExprKind::Block(block) => block,
        other => panic!("expected ExprKind::Block, found {:?}", other),
    };
    let first = block
        .stmts
        .first()
        .expect("expected at least one statement");
    let BlockStmt::Let(stmt) = first else {
        panic!("expected let statement, found {:?}", first);
    };
    assert!(matches!(stmt.pat.kind(), PatternKind::Tuple(_)));
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
fn parses_struct_literal_single_field_without_trailing_comma() {
    let fe = FerroFrontend::new();
    let res = fe.parse("Foo { x }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    let s = match e.kind() {
        ExprKind::Struct(st) => st,
        other => panic!("expected ExprKind::Struct, found {:?}", other),
    };
    assert_eq!(s.fields.len(), 1);
    assert_eq!(s.fields[0].name.as_str(), "x");
}

#[test]
fn block_in_if_else_not_misparsed_as_struct_literal() {
    let fe = FerroFrontend::new();
    let res = fe.parse("if a > b { a } else { b }", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    assert!(
        matches!(e.kind(), ExprKind::If(_)),
        "should parse as if-expression, got {:?}",
        e.kind()
    );
}

#[test]
fn parses_structural_literal_with_explicit_and_shorthand_fields() {
    let fe = FerroFrontend::new();
    let res = fe.parse("struct { x: 1, y }", None).expect("parse");
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

// Negative cases: invalid structural/struct literal syntax should error.

fn expect_parse_err(src: &str) {
    let fe = FerroFrontend::new();
    let res = fe.parse(src, None);
    assert!(res.is_err(), "expected parse error for source:\n{src}");
}

#[test]
fn struct_literal_missing_colon_errors() {
    expect_parse_err("Point { x 1 }");
}

#[test]
fn structural_literal_missing_field_name_errors() {
    expect_parse_err("struct { : i32 }");
}

#[test]
fn parses_try_operator_on_identifier() {
    let fe = FerroFrontend::new();
    let res = fe.parse("value?", None).expect("parse");
    let e = unwrap_expr(&res.ast);
    match e.kind() {
        ExprKind::Try(t) => {
            // Inner expression should be the original identifier.
            assert!(matches!(t.expr.kind(), ExprKind::Name(_)));
        }
        other => panic!("expected ExprKind::Try, found {:?}", other),
    }
}
