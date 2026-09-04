//! Real (but intentionally basic) refinement-type checking:
//! `{binder : base // predicate}` (Lean 4's `Subtype` notation) is
//! discharged at compile time by two self-contained decision procedures
//! modeled on Lean 4's own automation — `decide` (exact evaluation of
//! fully-concrete values) and `omega` (a linear-arithmetic decision
//! procedure for symbolic values) — never by an SMT solver, and never by
//! deferring to a runtime check. See `docs`/the design plan for the
//! rationale: real refinement-type systems (Liquid Haskell, F*, Flux, and
//! Lean itself) restrict predicates to a decidable fragment specifically
//! so every coercion is settled at compile time, one way or the other.

pub mod decide;
pub mod omega;

use fp_core::hir;
use fp_core::hir::ty::Ty;
use std::collections::HashMap;

/// Cached on `HirPackage` (see `fp_core::hir::refinement`'s doc comment for
/// why the type itself lives in `fp-core`), so re-exported here as this
/// crate's own name for it — every discharge site in `fp-typing` still
/// spells it `crate::refinement::RefinementHint`.
pub use fp_core::hir::RefinementHint;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefinementOutcome {
    /// `decide` or `omega` proved the predicate holds.
    ProvenTrue,
    /// `decide` found a concrete violation, or `omega` proved
    /// `hypotheses ∧ ¬predicate` satisfiable (a counterexample exists).
    ProvenFalse,
    /// The predicate or a hypothesis uses a construct outside the
    /// supported linear-arithmetic fragment.
    Undecidable,
}

/// Implicit facts that follow purely from `base`'s own representation —
/// currently just "an unsigned integer type's value is always `>= 0`".
/// Deliberately does not include path-sensitive (branch-condition)
/// hypotheses — see the design plan's documented v1 scope limitation.
pub fn implicit_hypotheses(base: &Ty, binder: &hir::Symbol) -> Vec<hir::Expr> {
    use fp_core::hir::ty::TyKind;
    use fp_core::span::Span;

    if matches!(base.kind, TyKind::Uint(_)) {
        let path_expr = path_expr(binder.as_str());
        let zero = literal_int(0);
        vec![hir::Expr::new(
            hir::HirId::new(hir::OwnerId::root(hir::PackageId::new("test")), 0),
            hir::ExprKind::Binary(hir::BinOp::Ge, Box::new(path_expr), Box::new(zero)),
            Span::default(),
        )]
    } else {
        Vec::new()
    }
}

fn path_expr(name: &str) -> hir::Expr {
    hir::Expr::new(
        hir::HirId::new(hir::OwnerId::root(hir::PackageId::new("test")), 0),
        hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
            segments: vec![hir::PathSegment {
                ident: name.into(),
                args: None,
                infer_args: true,
                res: fp_core::hir::Res::Error,
            }],
            res: fp_core::hir::Res::Error,
        })),
        fp_core::span::Span::default(),
    )
}

fn literal_int(v: i64) -> hir::Expr {
    hir::Expr::new(
        hir::HirId::new(hir::OwnerId::root(hir::PackageId::new("test")), 0),
        hir::ExprKind::Literal(hir::Lit::Integer(v)),
        fp_core::span::Span::default(),
    )
}

/// Discharge `hypotheses ⊢ predicate[binder := value_expr]`.
pub fn discharge(
    binder: &hir::Symbol,
    predicate: &hir::Expr,
    value_expr: &hir::Expr,
    hypotheses: &[hir::Expr],
) -> RefinementOutcome {
    // Fast path: a fully-concrete value — evaluate exactly, Lean-`decide`-style.
    if let Some(value) = decide::try_decide(value_expr, &HashMap::new()).and_then(|v| v.as_int()) {
        let mut env = HashMap::new();
        env.insert(binder.as_str().to_string(), value);
        if let Some(result) = decide::try_decide(predicate, &env) {
            return match result.as_bool() {
                Some(true) => RefinementOutcome::ProvenTrue,
                Some(false) => RefinementOutcome::ProvenFalse,
                None => RefinementOutcome::Undecidable,
            };
        }
    }

    // Symbolic path: omega.
    let value_term = match omega::normalize(value_expr) {
        Ok(term) => term,
        Err(_) => return RefinementOutcome::Undecidable,
    };
    let binder_term = omega::LinearTerm::var(binder.as_str());
    let mut base_constraints = vec![omega::LinearConstraint {
        term: binder_term.add(&value_term.negate()),
        rel: omega::Rel::Eq,
    }];
    for hyp in hypotheses {
        match omega::to_conjuncts(hyp) {
            Ok(mut cs) => base_constraints.append(&mut cs),
            Err(_) => return RefinementOutcome::Undecidable,
        }
    }
    let goal = match omega::to_conjuncts(predicate) {
        Ok(cs) => cs,
        Err(_) => return RefinementOutcome::Undecidable,
    };

    // UNSAT of `hypotheses ∧ ¬(g1 ∧ ... ∧ gn)` iff every
    // `hypotheses ∧ ¬gi` is UNSAT — check each conjunct of the goal
    // separately rather than needing general DNF negation.
    for g in &goal {
        let negated = omega::LinearConstraint {
            term: g.term.clone(),
            rel: omega::negate_rel(g.rel),
        };
        let mut trial = base_constraints.clone();
        trial.push(negated);
        if omega::is_satisfiable(&trial) {
            return RefinementOutcome::ProvenFalse;
        }
    }
    RefinementOutcome::ProvenTrue
}

/// Shared test-only expression-building helpers for `decide`'s and
/// `omega`'s unit tests.
#[cfg(test)]
pub(crate) mod test_support {
    use fp_core::hir::{
        BinOp, Expr, ExprKind, HirId, Lit, OwnerId, PackageId, Path, PathSegment, QPath, Res,
    };
    use fp_core::span::Span;

    pub fn lit_int(v: i64) -> Expr {
        Expr::new(
            HirId::new(OwnerId::root(PackageId::new("test")), 0),
            ExprKind::Literal(Lit::Integer(v)),
            Span::default(),
        )
    }

    pub fn path(name: &str) -> Expr {
        Expr::new(
            HirId::new(OwnerId::root(PackageId::new("test")), 0),
            ExprKind::Path(QPath::resolved(Path {
                segments: vec![PathSegment {
                    ident: name.into(),
                    args: None,
                    infer_args: true,
                    res: Res::Error,
                }],
                res: Res::Error,
            })),
            Span::default(),
        )
    }

    pub fn binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::new(
            HirId::new(OwnerId::root(PackageId::new("test")), 0),
            ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
            Span::default(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::test_support::{binop, lit_int, path};
    use super::*;
    use fp_core::hir::BinOp;

    #[test]
    fn discharges_literal_value_against_literal_predicate() {
        let binder: hir::Symbol = "x".into();
        let predicate = binop(BinOp::Ge, path("x"), lit_int(0));
        let value = lit_int(5);
        assert_eq!(
            discharge(&binder, &predicate, &value, &[]),
            RefinementOutcome::ProvenTrue
        );
    }

    #[test]
    fn rejects_violated_literal_predicate() {
        let binder: hir::Symbol = "x".into();
        let predicate = binop(BinOp::Ge, path("x"), lit_int(0));
        let value = lit_int(-1);
        assert_eq!(
            discharge(&binder, &predicate, &value, &[]),
            RefinementOutcome::ProvenFalse
        );
    }

    #[test]
    fn discharges_symbolic_value_from_hypothesis() {
        // A parameter `y` known (hypothesis) to be `>= 5` satisfies `x >= 0`
        // when coerced through `x := y`.
        let binder: hir::Symbol = "x".into();
        let predicate = binop(BinOp::Ge, path("x"), lit_int(0));
        let value = path("y");
        let hypothesis = binop(BinOp::Ge, path("y"), lit_int(5));
        assert_eq!(
            discharge(&binder, &predicate, &value, &[hypothesis]),
            RefinementOutcome::ProvenTrue
        );
    }

    #[test]
    fn rejects_symbolic_value_without_sufficient_hypothesis() {
        let binder: hir::Symbol = "x".into();
        let predicate = binop(BinOp::Ge, path("x"), lit_int(0));
        let value = path("y");
        // No hypothesis at all about `y` — cannot prove `y >= 0`.
        assert_eq!(
            discharge(&binder, &predicate, &value, &[]),
            RefinementOutcome::ProvenFalse
        );
    }
}
