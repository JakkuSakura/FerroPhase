use fp_core::ast::{
    Expr, ExprIntrinsicCall, ExprInvokeTarget, ExprKind, ExprMatch, ExprStruct,
    ExprIntrinsicContainer, Value, TySlot, ExprIf, ExprLet, PatternKind,
};
use fp_core::span::Span;
use fp_core::error::Result;
use fp_core::intrinsics::OperationMaterializer;

/// FerroPhase/Rust operation materializer for transpile mode.
/// Rewrites Some(x) → inline, None → null, Vec::new() → [], method ops → inline.
pub struct FerroOperationMaterializer;

impl OperationMaterializer for FerroOperationMaterializer {
    fn materialize_invoke(
        &self,
        invoke: &mut fp_core::ast::ExprInvoke,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        match &invoke.target {
            ExprInvokeTarget::Function(name) => {
                let s = name.to_string();
                match s.as_str() {
                    "Some" => Ok(Some(invoke.args.first().cloned().unwrap_or_else(null_expr))),
                    "None" => Ok(Some(null_expr())),
                    "Vec::new" | "Vec" => Ok(Some(Expr::from_parts(0, None, None,
                        ExprKind::IntrinsicContainer(
                            ExprIntrinsicContainer::VecElements { elements: vec![] }
                        ),
                    ))),
                    _ => Ok(None),
                }
            }
            ExprInvokeTarget::Method(sel) => {
                let method = sel.field.name.as_str();
                match method {
                    "as_ref" | "as_str" | "to_owned" | "iter" => {
                        Ok(Some(sel.obj.as_ref().clone()))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    fn materialize_match(
        &self,
        m: &mut ExprMatch,
        _ty: &TySlot,
    ) -> Result<Option<Expr>> {
        if m.cases.len() != 2 { return Ok(None); }

        let (binding_case, _) = match (&m.cases[0].pat, &m.cases[1].pat) {
            (Some(p0), Some(p1)) => {
                let p0_trivial = is_trivial_pattern(&p0.kind);
                let p1_trivial = is_trivial_pattern(&p1.kind);
            if p0_trivial && !p1_trivial { eprintln!("  → binding=case1"); (&m.cases[1], &m.cases[0]) }
            else if !p0_trivial && p1_trivial { eprintln!("  → binding=case0"); (&m.cases[0], &m.cases[1]) }
            else { eprintln!("  → skip (both trivial or both non-trivial)"); return Ok(None); }
            }
            _ => return Ok(None),
        };

        let pat = match &binding_case.pat {
            Some(p) => p,
            None => return Ok(None),
        };
        if matches!(&pat.kind, PatternKind::Wildcard(_) | PatternKind::Ident(_)) { return Ok(None); }

        let scrutinee = match &m.scrutinee {
            Some(s) => s.as_ref().clone(),
            None => return Ok(None),
        };

        // Build: if (scrutinee != null) { val <binding> = scrutinee!!; body }
        let binding = match_binding_name(pat);
        let body = if let Some(b) = binding {
            let let_expr = Expr::from_parts(0, None, None,
                ExprKind::Let(ExprLet {
                    span: Span::default(),
                    pat: pat.clone(),
                    expr: Box::new(build_force_unwrap(&scrutinee)),
                })
            );
            Expr::from_parts(0, None, None,
                ExprKind::Block(fp_core::ast::ExprBlock {
                    span: Span::default(),
                    collected_items: vec![],
                    stmts: vec![
                        fp_core::ast::BlockStmt::Expr(fp_core::ast::BlockStmtExpr {
                            expr: Box::new(let_expr),
                            semicolon: Some(true),
                        }),
                        fp_core::ast::BlockStmt::Expr(fp_core::ast::BlockStmtExpr {
                            expr: binding_case.body.clone(),
                            semicolon: Some(false),
                        }),
                    ],
                })
            )
        } else {
            binding_case.body.as_ref().clone()
        };

        let if_expr = Expr::from_parts(0, None, None,
            ExprKind::If(ExprIf {
                span: Span::default(),
                cond: Box::new(build_not_null_check(&scrutinee)),
                then: Box::new(body),
                elze: None,
            })
        );
        Ok(Some(if_expr))
    }
}

fn null_expr() -> Expr {
    Expr::from_parts(0, None, None,
        ExprKind::Value(Box::new(Value::Null(Default::default()))),
    )
}

/// Whether this pattern is a trivial/fallthrough arm (Wildcard, None ident, etc.)
fn is_trivial_pattern(kind: &PatternKind) -> bool {
    match kind {
        PatternKind::Wildcard(_) => true,
        PatternKind::Ident(id) => id.ident.name == "None" || id.ident.name == "Err",
        _ => false,
    }
}

fn match_binding_name(pat: &fp_core::ast::Pattern) -> Option<String> {
    match &pat.kind {
        PatternKind::Ident(id) => Some(id.ident.name.clone()),
        PatternKind::Struct(s) => s.fields.first().map(|f| f.name.name.clone()),
        PatternKind::TupleStruct(ts) => ts.patterns.first()
            .and_then(|p| match_binding_name(p)),
        _ => None,
    }
}

fn build_not_null_check(expr: &Expr) -> Expr {
    Expr::from_parts(0, None, None,
        ExprKind::BinOp(fp_core::ast::ExprBinOp {
            span: Span::default(),
            kind: fp_core::ops::BinOpKind::Ne,
            lhs: Box::new(expr.clone()),
            rhs: Box::new(null_expr()),
        })
    )
}

fn build_force_unwrap(expr: &Expr) -> Expr {
    // x!! → use IntrinsicCall with OptionUnwrap op
    Expr::from_parts(0, None, None,
        ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            fp_core::intrinsics::CallKind::Op(fp_core::intrinsics::calls::OpKind::OptionUnwrap),
            vec![expr.clone()],
            vec![],
        ))
    )
}
