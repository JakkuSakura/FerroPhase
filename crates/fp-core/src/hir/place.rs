//! TODO: merge into hir::place
use crate::hir::{self, Expr, ExprKind, Symbol};
use crate::intrinsics::IntrinsicKind;
use crate::place::{
    AssignTargetBaseKind, AssignTargetProjectionKind, AssignTargetSliceKind,
    ProjectedAssignTargetKind,
};

pub type HirAssignTargetBase = AssignTargetBaseKind<hir::Path, Expr>;
pub type HirAssignTargetSlice = AssignTargetSliceKind<Expr>;
pub type HirAssignTargetProjection = AssignTargetProjectionKind<Symbol, Expr>;
pub type HirProjectedAssignTarget = ProjectedAssignTargetKind<hir::Path, Symbol, Expr>;

impl HirProjectedAssignTarget {
    pub fn from_path(path: hir::Path, span: crate::span::Span) -> Self {
        Self::new(HirAssignTargetBase::Name(path), span)
    }

    pub fn from_expr(expr: Expr) -> Self {
        let span = expr.span;
        Self::new(HirAssignTargetBase::Expr(Box::new(expr)), span)
    }
}

pub fn project_hir_assign_target(expr: &Expr) -> Option<HirProjectedAssignTarget> {
    let mut current = expr;
    let mut projections = Vec::new();
    let base = loop {
        match &current.kind {
            ExprKind::Path(hir::QPath::Resolved(_, path)) => {
                break HirAssignTargetBase::Name(path.clone());
            }
            ExprKind::Path(hir::QPath::TypeRelative(_, _)) => {
                break HirAssignTargetBase::Expr(Box::new(current.clone()));
            }
            ExprKind::FieldAccess(base, field) => {
                projections.push(HirAssignTargetProjection::Field(field.clone()));
                current = base.as_ref();
            }
            ExprKind::Index(base, index) => {
                projections.push(HirAssignTargetProjection::Index(index.clone()));
                current = base.as_ref();
            }
            ExprKind::Slice(slice) => {
                projections.push(HirAssignTargetProjection::Slice(HirAssignTargetSlice {
                    start: slice.start.clone(),
                    end: slice.end.clone(),
                    inclusive: slice.inclusive,
                }));
                current = slice.base.as_ref();
            }
            ExprKind::IntrinsicCall(call) if call.kind == IntrinsicKind::Slice => {
                let base = call
                    .callargs
                    .iter()
                    .find(|arg| arg.name.as_str() == "base")?;
                let start = call
                    .callargs
                    .iter()
                    .find(|arg| arg.name.as_str() == "start")?;
                let end = call
                    .callargs
                    .iter()
                    .find(|arg| arg.name.as_str() == "end")?;
                projections.push(HirAssignTargetProjection::Slice(HirAssignTargetSlice {
                    start: Some(Box::new(start.value.clone())),
                    end: Some(Box::new(end.value.clone())),
                    inclusive: false,
                }));
                current = &base.value;
            }
            ExprKind::Unary(hir::UnOp::Deref, inner) => {
                projections.push(HirAssignTargetProjection::Deref);
                current = inner.as_ref();
            }
            _ => break HirAssignTargetBase::Expr(Box::new(current.clone())),
        }
    };

    projections.reverse();
    let mut target = HirProjectedAssignTarget::new(base, expr.span);
    for projection in projections {
        target.push(projection);
    }
    Some(target)
}
