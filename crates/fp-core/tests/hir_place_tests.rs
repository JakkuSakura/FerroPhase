use fp_core::hir::place::{HirAssignTargetProjection, project_hir_assign_target};
use fp_core::hir::{
    CallArg, Expr, ExprKind, IntrinsicCallExpr, Path, PathSegment, Res, SliceExpr, Symbol,
};
use fp_core::intrinsics::{CallKind, IntrinsicKind};
use fp_core::span::Span;

fn span() -> Span {
    Span::null()
}

fn test_pkg() -> fp_core::hir::PackageId {
    fp_core::hir::PackageId::new("test")
}

fn hid(index: u32) -> fp_core::hir::HirId {
    fp_core::hir::HirId::new(fp_core::hir::OwnerId::root(test_pkg()), index)
}

fn path_expr(hir_id: u32, name: &str) -> Expr {
    Expr::new(
        hid(hir_id),
        ExprKind::Path(Path {
            segments: vec![PathSegment {
                name: Symbol::new(name),
                args: None,
            }],
            res: Res::Error,
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
        hid(4),
        ExprKind::Slice(SliceExpr {
            hir_id: hid(5),
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
        hid(13),
        ExprKind::IntrinsicCall(IntrinsicCallExpr {
            kind: IntrinsicKind::Slice,
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

    let projected = project_hir_assign_target(&slice_call).expect("intrinsic slice should project");
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

#[test]
fn deep_projection_is_bounded() {
    let mut expr = path_expr(1000, "value");
    for index in 0..512 {
        expr = Expr::new(
            hid(1001 + index),
            ExprKind::FieldAccess(Box::new(expr), Symbol::new("field")),
            span(),
        );
    }

    let projected = project_hir_assign_target(&expr).expect("deep projection should terminate");
    assert_eq!(projected.projections.len(), 512);
}
