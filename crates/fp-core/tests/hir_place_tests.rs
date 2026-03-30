use fp_core::hir::{
    CallArg, Expr, ExprKind, IntrinsicCallExpr, Path, PathSegment, SliceExpr, Symbol,
};
use fp_core::hir_place::{project_hir_assign_target, HirAssignTargetProjection};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::span::Span;

fn span() -> Span {
    Span::null()
}

fn path_expr(hir_id: u32, name: &str) -> Expr {
    Expr::new(
        hir_id,
        ExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
            }],
            res: None,
        }),
        span(),
    )
}

#[test]
fn projects_hir_slice_expr_syntax_directly() {
    let base = path_expr(1, "values");
    let start = path_expr(2, "i");
    let end = path_expr(3, "j");

    let slice_expr = Expr::new(
        4,
        ExprKind::Slice(SliceExpr {
            hir_id: 5,
            base: Box::new(base),
            start: Some(Box::new(start)),
            end: Some(Box::new(end)),
            inclusive: true,
        }),
        span(),
    );

    let projected =
        project_hir_assign_target(&slice_expr).expect("slice expression should project");
    let projection = projected
        .projections
        .last()
        .expect("slice projection present");

    let HirAssignTargetProjection::Slice(slice) = projection else {
        panic!("expected slice projection, got {projection:?}");
    };

    assert!(slice.start.is_some());
    assert!(slice.end.is_some());
    assert!(slice.inclusive);
}

#[test]
fn projects_intrinsic_slice_compatibility_path() {
    let base = path_expr(10, "values");
    let start = path_expr(11, "start");
    let end = path_expr(12, "end");

    let slice_call = Expr::new(
        13,
        ExprKind::IntrinsicCall(IntrinsicCallExpr {
            kind: IntrinsicCallKind::Slice,
            callargs: vec![
                CallArg {
                    name: Symbol::new("base"),
                    value: base,
                },
                CallArg {
                    name: Symbol::new("start"),
                    value: start,
                },
                CallArg {
                    name: Symbol::new("end"),
                    value: end,
                },
            ],
        }),
        span(),
    );

    let projected =
        project_hir_assign_target(&slice_call).expect("intrinsic slice should project");
    let projection = projected
        .projections
        .last()
        .expect("slice projection present");

    let HirAssignTargetProjection::Slice(slice) = projection else {
        panic!("expected slice projection, got {projection:?}");
    };

    assert!(slice.start.is_some());
    assert!(slice.end.is_some());
    assert!(!slice.inclusive);
}

