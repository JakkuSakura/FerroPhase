use fp_core::ast::{Expr, ExprKind};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_lang::ast::FerroPhaseParser;

fn parse_expr(src: &str) -> Expr {
    let parser = FerroPhaseParser::new();
    parser
        .parse_expr_ast(src)
        .unwrap_or_else(|e| panic!("parse failed for `{src}`: {e}"))
}

fn is_locator(expr: &Expr, name: &str) -> bool {
    match expr.kind() {
        ExprKind::Locator(loc) => loc
            .as_ident()
            .map(|id| id.as_str() == name)
            .unwrap_or(false),
        _ => false,
    }
}

#[test]
fn precedence_mul_over_add() {
    let expr = parse_expr("1 + 2 * 3");
    let ExprKind::BinOp(add) = expr.kind() else {
        panic!("expected add");
    };
    assert!(matches!(add.kind, BinOpKind::Add));
    match add.rhs.kind() {
        ExprKind::BinOp(mul) => assert!(matches!(mul.kind, BinOpKind::Mul)),
        other => panic!("rhs should be mul, got {:?}", other),
    }
}

#[test]
fn left_associative_subtraction() {
    let expr = parse_expr("1 - 2 - 3");
    let ExprKind::BinOp(sub1) = expr.kind() else {
        panic!("expected outer sub");
    };
    assert!(matches!(sub1.kind, BinOpKind::Sub));
    match sub1.lhs.kind() {
        ExprKind::BinOp(sub2) => assert!(matches!(sub2.kind, BinOpKind::Sub)),
        other => panic!("lhs should be nested sub, got {:?}", other),
    }
}

#[test]
fn left_associative_division() {
    let expr = parse_expr("8 / 4 / 2");
    let ExprKind::BinOp(div1) = expr.kind() else {
        panic!("expected outer div");
    };
    assert!(matches!(div1.kind, BinOpKind::Div));
    match div1.lhs.kind() {
        ExprKind::BinOp(div2) => assert!(matches!(div2.kind, BinOpKind::Div)),
        other => panic!("lhs should be nested div, got {:?}", other),
    }
}

#[test]
fn parentheses_override_precedence() {
    let expr = parse_expr("(1 + 2) * 3");
    let ExprKind::BinOp(mul) = expr.kind() else {
        panic!("expected mul");
    };
    assert!(matches!(mul.kind, BinOpKind::Mul));
    match mul.lhs.kind() {
        ExprKind::BinOp(add) => assert!(matches!(add.kind, BinOpKind::Add)),
        other => panic!("lhs should be add, got {:?}", other),
    }
}

#[test]
fn assignment_is_right_associative() {
    let expr = parse_expr("a = b = c");
    let ExprKind::Assign(outer) = expr.kind() else {
        panic!("expected assign");
    };
    assert!(is_locator(&outer.target, "a"));
    match outer.value.kind() {
        ExprKind::Assign(inner) => {
            assert!(is_locator(&inner.target, "b"));
            assert!(is_locator(&inner.value, "c"));
        }
        other => panic!("rhs should be nested assign, got {:?}", other),
    }
}

#[test]
fn compound_assign_is_right_associative() {
    let expr = parse_expr("a += b += c");
    let ExprKind::Assign(outer) = expr.kind() else {
        panic!("expected assign");
    };
    assert!(is_locator(&outer.target, "a"));
    // The current implementation parses `a += b += c` as `a += (a + (b = b + c))`.
    // Only assert that `b += c` is wrapped as a value, to avoid assuming left-associativity.
    match outer.value.kind() {
        ExprKind::BinOp(add) => {
            assert!(matches!(add.kind, BinOpKind::Add));
            match add.rhs.kind() {
                ExprKind::Assign(inner) => {
                    assert!(is_locator(&inner.target, "b"));
                }
                other => panic!("expected inner assign on rhs, got {:?}", other),
            }
        }
        other => panic!("assign value should contain inner assign, got {:?}", other),
    }
}

#[test]
fn logical_and_or_precedence() {
    let expr = parse_expr("a || b && c");
    let ExprKind::BinOp(or) = expr.kind() else {
        panic!("expected or");
    };
    assert!(matches!(or.kind, BinOpKind::Or));
    match or.rhs.kind() {
        ExprKind::BinOp(and) => assert!(matches!(and.kind, BinOpKind::And)),
        other => panic!("rhs should be and, got {:?}", other),
    }
}

#[test]
fn comparison_vs_logical() {
    let expr = parse_expr("a == b && c != d");
    let ExprKind::BinOp(and) = expr.kind() else {
        panic!("expected and");
    };
    assert!(matches!(and.kind, BinOpKind::And));
    assert!(matches!(and.lhs.kind(), ExprKind::BinOp(_))); // eq
    assert!(matches!(and.rhs.kind(), ExprKind::BinOp(_))); // ne
}

#[test]
fn bitand_over_bitor() {
    let expr = parse_expr("a | b & c");
    let ExprKind::BinOp(bitor_) = expr.kind() else {
        panic!("expected bitor");
    };
    assert!(matches!(bitor_.kind, BinOpKind::BitOr));
    match bitor_.rhs.kind() {
        ExprKind::BinOp(bitand_) => assert!(matches!(bitand_.kind, BinOpKind::BitAnd)),
        other => panic!("rhs should be bitand, got {:?}", other),
    }
}

#[test]
fn shift_lower_than_add() {
    let expr = parse_expr("a + b << c");
    let ExprKind::BinOp(shift) = expr.kind() else {
        panic!("expected shift");
    };
    assert!(matches!(shift.kind, BinOpKind::Shl));
    match shift.lhs.kind() {
        ExprKind::BinOp(add) => assert!(matches!(add.kind, BinOpKind::Add)),
        other => panic!("lhs should be add, got {:?}", other),
    }
}

#[test]
fn add_higher_than_shift_rhs() {
    let expr = parse_expr("a << b + c");
    let ExprKind::BinOp(shift) = expr.kind() else {
        panic!("expected shift");
    };
    assert!(matches!(shift.kind, BinOpKind::Shl));
    match shift.rhs.kind() {
        ExprKind::BinOp(add) => assert!(matches!(add.kind, BinOpKind::Add)),
        other => panic!("rhs should be add, got {:?}", other),
    }
}

#[test]
fn cast_binds_tighter_than_mul_and_add() {
    let expr = parse_expr("a + b as i64 * c");
    let ExprKind::BinOp(add) = expr.kind() else {
        panic!("expected add");
    };
    assert!(matches!(add.kind, BinOpKind::Add));
    match add.rhs.kind() {
        ExprKind::BinOp(mul) => {
            assert!(matches!(mul.kind, BinOpKind::Mul));
            match mul.lhs.kind() {
                ExprKind::Cast(_) => {}
                other => panic!("mul lhs should be cast, got {:?}", other),
            }
        }
        other => panic!("rhs should be mul, got {:?}", other),
    }
}

#[test]
fn try_operator_before_binary_ops() {
    let expr = parse_expr("foo? || bar");
    let ExprKind::BinOp(or) = expr.kind() else {
        panic!("expected or");
    };
    assert!(matches!(or.kind, BinOpKind::Or));
    match or.lhs.kind() {
        ExprKind::Try(_) => {}
        other => panic!("lhs should be try, got {:?}", other),
    }
}

#[test]
fn await_then_and() {
    let expr = parse_expr("await foo && bar");
    let ExprKind::BinOp(and) = expr.kind() else {
        panic!("expected and");
    };
    assert!(matches!(and.kind, BinOpKind::And));
    match and.lhs.kind() {
        ExprKind::Await(_) => {}
        other => panic!("lhs should be await, got {:?}", other),
    }
}

#[test]
fn unary_vs_binary_precedence() {
    let expr = parse_expr("-1 * 2");
    let ExprKind::BinOp(mul) = expr.kind() else {
        panic!("expected mul");
    };
    assert!(matches!(mul.kind, BinOpKind::Mul));
    match mul.lhs.kind() {
        ExprKind::UnOp(un) => assert!(matches!(un.op, UnOpKind::Neg)),
        other => panic!("lhs should be unary neg, got {:?}", other),
    }
}

#[test]
fn unary_not_over_unary_neg() {
    let expr = parse_expr("!-a");
    let ExprKind::UnOp(not) = expr.kind() else {
        panic!("expected not");
    };
    assert!(matches!(not.op, UnOpKind::Not));
    match not.val.kind() {
        ExprKind::UnOp(neg) => assert!(matches!(neg.op, UnOpKind::Neg)),
        other => panic!("inner should be neg, got {:?}", other),
    }
}
