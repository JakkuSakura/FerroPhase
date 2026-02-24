#![allow(dead_code)]
use fp_core::hir::{self, ExprKind};

pub fn assert_hir_integer(expr: &hir::Expr, expected: i64) {
    match &expr.kind {
        ExprKind::Literal(hir::Lit::Integer(value)) => assert_eq!(*value, expected),
        _ => panic!("expected integer literal, got {:?}", expr.kind),
    }
}
