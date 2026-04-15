use crate::ast::{Expr, ExprKind, Ident, Name};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum AssignTargetBaseKind<TName, TExpr> {
    Name(TName),
    Expr(Box<TExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignTargetSliceKind<TExpr> {
    pub start: Option<Box<TExpr>>,
    pub end: Option<Box<TExpr>>,
    pub inclusive: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignTargetProjectionKind<TField, TExpr> {
    Deref,
    Field(TField),
    Index(Box<TExpr>),
    Slice(AssignTargetSliceKind<TExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProjectedAssignTargetKind<TName, TField, TExpr> {
    pub base: AssignTargetBaseKind<TName, TExpr>,
    pub projections: Vec<AssignTargetProjectionKind<TField, TExpr>>,
    pub span: Span,
}

impl<TName, TField, TExpr> ProjectedAssignTargetKind<TName, TField, TExpr> {
    pub fn new(base: AssignTargetBaseKind<TName, TExpr>, span: Span) -> Self {
        Self {
            base,
            projections: Vec::new(),
            span,
        }
    }

    pub fn push(&mut self, projection: AssignTargetProjectionKind<TField, TExpr>) {
        self.projections.push(projection);
    }
}

pub type AssignTargetBase = AssignTargetBaseKind<Name, Expr>;
pub type AssignTargetSlice = AssignTargetSliceKind<Expr>;
pub type AssignTargetProjection = AssignTargetProjectionKind<Ident, Expr>;
pub type ProjectedAssignTarget = ProjectedAssignTargetKind<Name, Ident, Expr>;

impl ProjectedAssignTarget {
    pub fn from_name(name: Name, span: Span) -> Self {
        Self::new(AssignTargetBase::Name(name), span)
    }

    pub fn from_expr(expr: Expr) -> Self {
        let span = expr.span();
        Self::new(AssignTargetBase::Expr(Box::new(expr)), span)
    }
}

pub fn project_assign_target(expr: &Expr) -> Option<ProjectedAssignTarget> {
    match expr.kind() {
        ExprKind::Name(name) => Some(ProjectedAssignTarget::from_name(name.clone(), expr.span())),
        ExprKind::Paren(paren) => project_assign_target(paren.expr.as_ref()),
        ExprKind::Select(select) => {
            let mut target = project_assign_target(select.obj.as_ref())
                .unwrap_or_else(|| ProjectedAssignTarget::from_expr(select.obj.as_ref().clone()));
            target.push(AssignTargetProjection::Field(select.field.clone()));
            target.span = expr.span();
            Some(target)
        }
        ExprKind::Index(index) => {
            let mut target = project_assign_target(index.obj.as_ref())
                .unwrap_or_else(|| ProjectedAssignTarget::from_expr(index.obj.as_ref().clone()));
            match index.index.kind() {
                ExprKind::Range(range) => {
                    target.push(AssignTargetProjection::Slice(AssignTargetSlice {
                        start: range.start.clone(),
                        end: range.end.clone(),
                        inclusive: matches!(range.limit, crate::ast::ExprRangeLimit::Inclusive),
                    }))
                }
                _ => target.push(AssignTargetProjection::Index(index.index.clone())),
            }
            target.span = expr.span();
            Some(target)
        }
        ExprKind::Dereference(deref) => {
            let mut target = project_assign_target(deref.referee.as_ref()).unwrap_or_else(|| {
                ProjectedAssignTarget::from_expr(deref.referee.as_ref().clone())
            });
            target.push(AssignTargetProjection::Deref);
            target.span = expr.span();
            Some(target)
        }
        _ => None,
    }
}
