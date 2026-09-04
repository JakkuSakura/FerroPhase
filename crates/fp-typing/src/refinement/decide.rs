//! Exact evaluation of fully-concrete refinement predicates — mirrors Lean
//! 4's `decide` tactic: works whenever the proposition is literally
//! computable from what's already known, no case-splitting or symbolic
//! reasoning involved.

use fp_core::hir::{BinOp, Expr, ExprKind, Lit, UnOp};
use std::collections::HashMap;

/// The result of evaluating a closed (or `env`-closed) predicate/value
/// expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstVal {
    Int(i64),
    Bool(bool),
}

impl ConstVal {
    pub fn as_bool(self) -> Option<bool> {
        match self {
            ConstVal::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_int(self) -> Option<i64> {
        match self {
            ConstVal::Int(i) => Some(i),
            _ => None,
        }
    }
}

/// Evaluate `expr` exactly, resolving any variable reference against `env`.
/// Returns `None` for anything not fully concrete (a name missing from
/// `env`, a call, or any other construct outside literals/arithmetic/
/// comparisons/booleans) — the caller falls back to `omega` in that case.
pub fn try_decide(expr: &Expr, env: &HashMap<String, i64>) -> Option<ConstVal> {
    match &expr.kind {
        ExprKind::Literal(Lit::Integer(v)) => Some(ConstVal::Int(*v)),
        ExprKind::Literal(Lit::Bool(b)) => Some(ConstVal::Bool(*b)),
        ExprKind::Path(path) => {
            let name = path.segments().last()?.ident.as_str();
            env.get(name).copied().map(ConstVal::Int)
        }
        ExprKind::Unary(UnOp::Neg, inner) => {
            try_decide(inner, env)?.as_int().map(|v| ConstVal::Int(-v))
        }
        ExprKind::Unary(UnOp::Not, inner) => try_decide(inner, env)?
            .as_bool()
            .map(|b| ConstVal::Bool(!b)),
        ExprKind::Binary(op, lhs, rhs) => {
            let l = try_decide(lhs, env)?;
            let r = try_decide(rhs, env)?;
            eval_binop(op, l, r)
        }
        _ => None,
    }
}

fn eval_binop(op: &BinOp, l: ConstVal, r: ConstVal) -> Option<ConstVal> {
    match op {
        BinOp::Add => Some(ConstVal::Int(l.as_int()?.checked_add(r.as_int()?)?)),
        BinOp::Sub => Some(ConstVal::Int(l.as_int()?.checked_sub(r.as_int()?)?)),
        BinOp::Mul => Some(ConstVal::Int(l.as_int()?.checked_mul(r.as_int()?)?)),
        BinOp::Div => {
            let (lv, rv) = (l.as_int()?, r.as_int()?);
            (rv != 0).then(|| ConstVal::Int(lv / rv))
        }
        BinOp::Rem => {
            let (lv, rv) = (l.as_int()?, r.as_int()?);
            (rv != 0).then(|| ConstVal::Int(lv % rv))
        }
        BinOp::Eq => Some(ConstVal::Bool(values_eq(l, r)?)),
        BinOp::Ne => Some(ConstVal::Bool(!values_eq(l, r)?)),
        BinOp::Lt => Some(ConstVal::Bool(l.as_int()? < r.as_int()?)),
        BinOp::Le => Some(ConstVal::Bool(l.as_int()? <= r.as_int()?)),
        BinOp::Gt => Some(ConstVal::Bool(l.as_int()? > r.as_int()?)),
        BinOp::Ge => Some(ConstVal::Bool(l.as_int()? >= r.as_int()?)),
        BinOp::And => Some(ConstVal::Bool(l.as_bool()? && r.as_bool()?)),
        BinOp::Or => Some(ConstVal::Bool(l.as_bool()? || r.as_bool()?)),
        _ => None,
    }
}

fn values_eq(l: ConstVal, r: ConstVal) -> Option<bool> {
    match (l, r) {
        (ConstVal::Int(a), ConstVal::Int(b)) => Some(a == b),
        (ConstVal::Bool(a), ConstVal::Bool(b)) => Some(a == b),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::refinement::test_support::{binop, lit_int, path};

    #[test]
    fn decides_literal_comparison() {
        let expr = binop(BinOp::Ge, lit_int(5), lit_int(0));
        assert_eq!(
            try_decide(&expr, &HashMap::new()),
            Some(ConstVal::Bool(true))
        );
    }

    #[test]
    fn decides_false_literal_comparison() {
        let expr = binop(BinOp::Ge, lit_int(-1), lit_int(0));
        assert_eq!(
            try_decide(&expr, &HashMap::new()),
            Some(ConstVal::Bool(false))
        );
    }

    #[test]
    fn returns_none_for_unbound_variable() {
        assert_eq!(try_decide(&path("x"), &HashMap::new()), None);
    }
}
