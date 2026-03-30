use crate::hir::{self, Expr, ExprKind, Symbol};
use crate::intrinsics::IntrinsicCallKind;
use crate::place::{
    AssignTargetBaseKind, AssignTargetProjectionKind, AssignTargetSliceKind, ProjectedAssignTargetKind,
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
    match &expr.kind {
        ExprKind::Path(path) => Some(HirProjectedAssignTarget::from_path(path.clone(), expr.span)),
        ExprKind::FieldAccess(base, field) => {
            let mut target = project_hir_assign_target(base.as_ref())
                .unwrap_or_else(|| HirProjectedAssignTarget::from_expr(base.as_ref().clone()));
            target.push(HirAssignTargetProjection::Field(field.clone()));
            target.span = expr.span;
            Some(target)
        }
        ExprKind::Index(base, index) => {
            let mut target = project_hir_assign_target(base.as_ref())
                .unwrap_or_else(|| HirProjectedAssignTarget::from_expr(base.as_ref().clone()));
            target.push(HirAssignTargetProjection::Index(index.clone()));
            target.span = expr.span;
            Some(target)
        }
        ExprKind::Slice(slice) => {
            let mut target = project_hir_assign_target(slice.base.as_ref())
                .unwrap_or_else(|| HirProjectedAssignTarget::from_expr(slice.base.as_ref().clone()));
            target.push(HirAssignTargetProjection::Slice(HirAssignTargetSlice {
                start: slice.start.clone(),
                end: slice.end.clone(),
                inclusive: slice.inclusive,
            }));
            target.span = expr.span;
            Some(target)
        }
        ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Slice => {
            let base = call.callargs.iter().find(|arg| arg.name.as_str() == "base")?;
            let start = call.callargs.iter().find(|arg| arg.name.as_str() == "start")?;
            let end = call.callargs.iter().find(|arg| arg.name.as_str() == "end")?;

            let mut target = project_hir_assign_target(&base.value)
                .unwrap_or_else(|| HirProjectedAssignTarget::from_expr(base.value.clone()));
            target.push(HirAssignTargetProjection::Slice(HirAssignTargetSlice {
                start: Some(Box::new(start.value.clone())),
                end: Some(Box::new(end.value.clone())),
                inclusive: false,
            }));
            target.span = expr.span;
            Some(target)
        }
        ExprKind::Unary(hir::UnOp::Deref, inner) => {
            let mut target = project_hir_assign_target(inner.as_ref())
                .unwrap_or_else(|| HirProjectedAssignTarget::from_expr(inner.as_ref().clone()));
            target.push(HirAssignTargetProjection::Deref);
            target.span = expr.span;
            Some(target)
        }
        _ => None,
    }
}
