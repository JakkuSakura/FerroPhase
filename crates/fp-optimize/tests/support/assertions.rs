use fp_core::hir::{self, HirExprKind};

pub fn assert_hir_integer(expr: &hir::HirExpr, expected: i64) {
    match &expr.kind {
        HirExprKind::Literal(hir::HirLit::Integer(value)) => assert_eq!(*value, expected),
        _ => panic!("expected integer literal, got {:?}", expr.kind),
    }
}
