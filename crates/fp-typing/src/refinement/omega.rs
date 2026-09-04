//! A linear-arithmetic decision procedure for symbolic refinement
//! predicates — mirrors Lean 4's `omega` tactic: a dedicated, self-
//! contained decision procedure for quantifier-free linear arithmetic over
//! integers, with no external SMT solver involved.
//!
//! Implemented as Fourier–Motzkin elimination over the *rationals*
//! (represented as `f64`). This is a sound under-approximation for proving
//! validity over integers: if a system is unsatisfiable over the (larger)
//! rational solution space, it is certainly unsatisfiable over the
//! (smaller) integer solution space. The converse doesn't hold in general
//! (pure divisibility facts can be rationally satisfiable yet integrally
//! impossible), which costs completeness — some true integer-only facts
//! will be reported `Undecidable`/not-proven — but never soundness: this
//! never reports a violated predicate as proven. Upgrading to a full
//! integer-exact Omega test (Pugh's algorithm) later is a drop-in
//! replacement behind the same `is_satisfiable` interface.

use fp_core::hir::{BinOp, Expr, ExprKind, Lit, UnOp};
use std::collections::HashMap;

const EPS: f64 = 1e-9;

/// `sum(coeff_i * var_i) + constant`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct LinearTerm {
    pub coeffs: HashMap<String, f64>,
    pub constant: f64,
}

impl LinearTerm {
    pub fn constant(v: f64) -> Self {
        Self {
            coeffs: HashMap::new(),
            constant: v,
        }
    }

    pub fn var(name: &str) -> Self {
        let mut coeffs = HashMap::new();
        coeffs.insert(name.to_string(), 1.0);
        Self {
            coeffs,
            constant: 0.0,
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        let mut coeffs = self.coeffs.clone();
        for (k, v) in &other.coeffs {
            let entry = coeffs.entry(k.clone()).or_insert(0.0);
            *entry += v;
            if entry.abs() < EPS {
                coeffs.remove(k);
            }
        }
        Self {
            coeffs,
            constant: self.constant + other.constant,
        }
    }

    pub fn scale(&self, factor: f64) -> Self {
        Self {
            coeffs: self
                .coeffs
                .iter()
                .map(|(k, v)| (k.clone(), v * factor))
                .collect(),
            constant: self.constant * factor,
        }
    }

    pub fn negate(&self) -> Self {
        self.scale(-1.0)
    }
}

/// Failure to normalize an expression into the supported linear-arithmetic
/// fragment: a nonlinear term, a call, or any other unsupported construct.
#[derive(Debug, Clone)]
pub struct NotLinear(pub String);

/// Normalize an arithmetic expression into a `LinearTerm`. Any bare `Path`
/// becomes its own term-variable (named by the path's last segment) —
/// there's no need to track which names are "in scope" here, since an
/// out-of-scope reference would already have failed name resolution
/// earlier in the pipeline, before this ever runs.
pub fn normalize(expr: &Expr) -> Result<LinearTerm, NotLinear> {
    match &expr.kind {
        ExprKind::Literal(Lit::Integer(v)) => Ok(LinearTerm::constant(*v as f64)),
        ExprKind::Path(path) => {
            let name = path
                .segments()
                .last()
                .map(|s| s.name.as_str())
                .unwrap_or_default();
            Ok(LinearTerm::var(name))
        }
        ExprKind::Unary(UnOp::Neg, inner) => Ok(normalize(inner)?.negate()),
        ExprKind::Binary(op, lhs, rhs) => {
            let l = normalize(lhs)?;
            match op {
                BinOp::Add => Ok(l.add(&normalize(rhs)?)),
                BinOp::Sub => Ok(l.add(&normalize(rhs)?.negate())),
                BinOp::Mul => {
                    let r = normalize(rhs)?;
                    if l.coeffs.is_empty() {
                        Ok(r.scale(l.constant))
                    } else if r.coeffs.is_empty() {
                        Ok(l.scale(r.constant))
                    } else {
                        Err(NotLinear(
                            "nonlinear term (variable * variable) is outside the supported \
                             linear-arithmetic fragment"
                                .into(),
                        ))
                    }
                }
                BinOp::Div => {
                    let r = normalize(rhs)?;
                    if r.coeffs.is_empty() && r.constant.abs() > EPS {
                        Ok(l.scale(1.0 / r.constant))
                    } else {
                        Err(NotLinear(
                            "division by a non-constant is outside the supported \
                             linear-arithmetic fragment"
                                .into(),
                        ))
                    }
                }
                _ => Err(NotLinear(format!("{op:?} is not an arithmetic operator"))),
            }
        }
        _ => Err(NotLinear(
            "expression is outside the supported linear-arithmetic fragment".into(),
        )),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rel {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

pub fn negate_rel(rel: Rel) -> Rel {
    match rel {
        Rel::Lt => Rel::Ge,
        Rel::Le => Rel::Gt,
        Rel::Gt => Rel::Le,
        Rel::Ge => Rel::Lt,
        Rel::Eq => Rel::Ne,
        Rel::Ne => Rel::Eq,
    }
}

/// `term REL 0`.
#[derive(Debug, Clone)]
pub struct LinearConstraint {
    pub term: LinearTerm,
    pub rel: Rel,
}

fn is_comparison(op: &BinOp) -> bool {
    matches!(
        op,
        BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::Eq | BinOp::Ne
    )
}

fn rel_of(op: &BinOp) -> Rel {
    match op {
        BinOp::Lt => Rel::Lt,
        BinOp::Le => Rel::Le,
        BinOp::Gt => Rel::Gt,
        BinOp::Ge => Rel::Ge,
        BinOp::Eq => Rel::Eq,
        BinOp::Ne => Rel::Ne,
        _ => unreachable!("guarded by is_comparison"),
    }
}

/// Normalize a predicate into its top-level conjuncts (comparisons joined
/// by `&&` only — `||` and non-comparison boolean atoms are outside the
/// supported fragment for v1 and return `NotLinear`).
pub fn to_conjuncts(expr: &Expr) -> Result<Vec<LinearConstraint>, NotLinear> {
    match &expr.kind {
        ExprKind::Binary(BinOp::And, lhs, rhs) => {
            let mut out = to_conjuncts(lhs)?;
            out.extend(to_conjuncts(rhs)?);
            Ok(out)
        }
        ExprKind::Binary(op, lhs, rhs) if is_comparison(op) => {
            let term = normalize(lhs)?.add(&normalize(rhs)?.negate());
            Ok(vec![LinearConstraint {
                term,
                rel: rel_of(op),
            }])
        }
        _ => Err(NotLinear(
            "predicate must be a conjunction of linear comparisons (comparisons + `&&` only)"
                .into(),
        )),
    }
}

#[derive(Debug, Clone)]
struct Elementary {
    term: LinearTerm,
    strict: bool, // `term < 0` if true, `term <= 0` otherwise
}

/// Is this conjunction of (possibly `=`/`!=`) linear constraints
/// satisfiable over the rationals?
pub fn is_satisfiable(constraints: &[LinearConstraint]) -> bool {
    expand(constraints)
        .into_iter()
        .any(|conjunct| fm_satisfiable(conjunct))
}

/// `=`/`!=` aren't directly representable as one elementary `<=`/`<`
/// constraint; expand them (an equality into two non-strict inequalities,
/// a disequality into a disjunction of two strict ones) into a disjunction
/// of elementary-only conjunctions.
fn expand(constraints: &[LinearConstraint]) -> Vec<Vec<Elementary>> {
    let mut disjuncts: Vec<Vec<Elementary>> = vec![vec![]];
    for c in constraints {
        let options: Vec<Vec<Elementary>> = match c.rel {
            Rel::Le => vec![vec![Elementary {
                term: c.term.clone(),
                strict: false,
            }]],
            Rel::Lt => vec![vec![Elementary {
                term: c.term.clone(),
                strict: true,
            }]],
            Rel::Ge => vec![vec![Elementary {
                term: c.term.negate(),
                strict: false,
            }]],
            Rel::Gt => vec![vec![Elementary {
                term: c.term.negate(),
                strict: true,
            }]],
            Rel::Eq => vec![vec![
                Elementary {
                    term: c.term.clone(),
                    strict: false,
                },
                Elementary {
                    term: c.term.negate(),
                    strict: false,
                },
            ]],
            Rel::Ne => vec![
                vec![Elementary {
                    term: c.term.clone(),
                    strict: true,
                }],
                vec![Elementary {
                    term: c.term.negate(),
                    strict: true,
                }],
            ],
        };
        let mut next = Vec::with_capacity(disjuncts.len() * options.len());
        for existing in &disjuncts {
            for option in &options {
                let mut combined = existing.clone();
                combined.extend(option.clone());
                next.push(combined);
            }
        }
        disjuncts = next;
    }
    disjuncts
}

/// Fourier–Motzkin elimination: repeatedly eliminate one variable at a time
/// until only constant constraints remain, then check those hold.
fn fm_satisfiable(mut constraints: Vec<Elementary>) -> bool {
    loop {
        let Some(var) = constraints
            .iter()
            .flat_map(|c| c.term.coeffs.keys())
            .next()
            .cloned()
        else {
            return constraints.iter().all(|c| {
                if c.strict {
                    c.term.constant < -EPS
                } else {
                    c.term.constant <= EPS
                }
            });
        };

        let mut lowers = Vec::new(); // v >= bound
        let mut uppers = Vec::new(); // v <= bound
        let mut others = Vec::new();
        for c in &constraints {
            let coeff = *c.term.coeffs.get(&var).unwrap_or(&0.0);
            if coeff.abs() < EPS {
                others.push(c.clone());
                continue;
            }
            let mut rest = c.term.clone();
            rest.coeffs.remove(&var);
            let bound = rest.scale(-1.0 / coeff);
            if coeff > 0.0 {
                uppers.push((bound, c.strict));
            } else {
                lowers.push((bound, c.strict));
            }
        }

        if uppers.is_empty() || lowers.is_empty() {
            constraints = others;
            continue;
        }

        for (upper, u_strict) in &uppers {
            for (lower, l_strict) in &lowers {
                // Require lower <= upper (i.e. lower - upper <= 0).
                others.push(Elementary {
                    term: lower.add(&upper.negate()),
                    strict: *u_strict || *l_strict,
                });
            }
        }
        constraints = others;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::refinement::test_support::{binop, lit_int, path};

    fn conj(exprs: Vec<Expr>) -> Vec<LinearConstraint> {
        exprs
            .iter()
            .flat_map(|e| to_conjuncts(e).unwrap())
            .collect()
    }

    #[test]
    fn simple_range_is_satisfiable() {
        // x >= 0 && x <= 100
        let c1 = binop(BinOp::Ge, path("x"), lit_int(0));
        let c2 = binop(BinOp::Le, path("x"), lit_int(100));
        assert!(is_satisfiable(&conj(vec![c1, c2])));
    }

    #[test]
    fn contradictory_range_is_unsatisfiable() {
        // x >= 5 && x <= 0
        let c1 = binop(BinOp::Ge, path("x"), lit_int(5));
        let c2 = binop(BinOp::Le, path("x"), lit_int(0));
        assert!(!is_satisfiable(&conj(vec![c1, c2])));
    }

    #[test]
    fn hypothesis_proves_goal() {
        // hyp: x >= 5 ; goal negated: x < 0  => hyp ∧ ¬goal should be UNSAT,
        // meaning x >= 5 proves x >= 0.
        let hyp = binop(BinOp::Ge, path("x"), lit_int(5));
        let negated_goal = binop(BinOp::Lt, path("x"), lit_int(0));
        assert!(!is_satisfiable(&conj(vec![hyp, negated_goal])));
    }

    #[test]
    fn hypothesis_does_not_prove_unrelated_goal() {
        // hyp: x >= 5 ; negated goal: x < 100 (i.e. does x >= 5 prove x >= 100? no)
        let hyp = binop(BinOp::Ge, path("x"), lit_int(5));
        let negated_goal = binop(BinOp::Lt, path("x"), lit_int(100));
        assert!(is_satisfiable(&conj(vec![hyp, negated_goal])));
    }
}
