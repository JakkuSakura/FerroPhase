use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBinOp, ExprBlock, ExprIf, ExprIntrinsicCall, ExprKind,
    ExprUnOp, Ident, StmtLet, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::{
    IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload, IntrinsicNormalizer, NormalizeOutcome,
};
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_rust::normalization::RustIntrinsicNormalizer;

use crate::ast::expr::{lower_expr_from_cst, lower_type_from_cst};
use crate::cst::{parse_expr_lexemes_prefix_to_cst, parse_type_lexemes_prefix_to_cst};
use crate::lexer::lexeme::LexemeKind;
use crate::lexer::tokenizer::lex_lexemes;

/// FerroPhase intrinsic normalizer that adds `t!` macro lowering for type expressions,
/// delegating all other macros to the Rust normalizer.
#[derive(Debug, Default, Clone, Copy)]
pub struct FerroIntrinsicNormalizer;

impl IntrinsicNormalizer for FerroIntrinsicNormalizer {
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        let (ty_slot, kind) = expr.into_parts();
        let ExprKind::Macro(macro_expr) = kind else {
            return Ok(NormalizeOutcome::Ignored(Expr::from_parts(ty_slot, kind)));
        };

        if let Some(name) = macro_expr.invocation.path.segments.last() {
            if name.as_str() == "t" {
                let ty = parse_type_macro_tokens(&macro_expr.invocation.tokens)?;
                let replacement = Expr::value(Value::Type(ty)).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if name.as_str() == "assert" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 1 {
                    return Err(fp_core::error::Error::from(
                        "assert! requires exactly one argument",
                    ));
                }
                let replacement =
                    assert_macro(args.into_iter().next().unwrap(), "assertion failed")
                        .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if name.as_str() == "assert_eq" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_eq! requires exactly two arguments",
                    ));
                }
                let replacement = assert_compare_macro(
                    args[0].clone(),
                    args[1].clone(),
                    BinOpKind::Eq,
                    "assertion failed: left != right",
                )
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if name.as_str() == "assert_ne" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() != 2 {
                    return Err(fp_core::error::Error::from(
                        "assert_ne! requires exactly two arguments",
                    ));
                }
                let replacement = assert_compare_macro(
                    args[0].clone(),
                    args[1].clone(),
                    BinOpKind::Ne,
                    "assertion failed: left == right",
                )
                .with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
            if name.as_str() == "panic" {
                let args = parse_expr_macro_tokens(&macro_expr.invocation.tokens)?;
                if args.len() > 1 {
                    return Err(fp_core::error::Error::from(
                        "panic! accepts at most one argument",
                    ));
                }
                let replacement = panic_macro(args).with_ty_slot(ty_slot);
                return Ok(NormalizeOutcome::Normalized(replacement));
            }
        }

        let fallback = Expr::from_parts(ty_slot, ExprKind::Macro(macro_expr));
        RustIntrinsicNormalizer::default().normalize_macro(fallback)
    }
}

fn parse_type_macro_tokens(tokens: &str) -> Result<fp_core::ast::Ty> {
    let lexemes = lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let (ty_cst, consumed) =
        parse_type_lexemes_prefix_to_cst(&lexemes, 0, &[])
            .map_err(|err| fp_core::error::Error::from(err.to_string()))?;

    if lexemes[consumed..]
        .iter()
        .any(|lex| lex.kind == LexemeKind::Token)
    {
        return Err(fp_core::error::Error::from(
            "t! macro tokens contain trailing input",
        ));
    }

    lower_type_from_cst(&ty_cst).map_err(|err| fp_core::error::Error::from(err.to_string()))
}

fn parse_expr_macro_tokens(tokens: &str) -> Result<Vec<Expr>> {
    let lexemes = lex_lexemes(tokens).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
    let mut idx = 0;
    let mut args = Vec::new();
    while idx < lexemes.len() {
        while idx < lexemes.len() && lexemes[idx].kind != LexemeKind::Token {
            idx += 1;
        }
        if idx >= lexemes.len() {
            break;
        }
        if lexemes[idx].text == "," {
            idx += 1;
            continue;
        }
        let (expr_cst, consumed) =
            parse_expr_lexemes_prefix_to_cst(&lexemes[idx..], 0).map_err(|err| {
                fp_core::error::Error::from(format!("assert macro parse error: {}", err))
            })?;
        let expr =
            lower_expr_from_cst(&expr_cst).map_err(|err| fp_core::error::Error::from(err.to_string()))?;
        args.push(expr);
        idx += consumed;
    }
    Ok(args)
}

fn assert_macro(cond: Expr, message: &str) -> Expr {
    let panic_expr = panic_call_with_message(message);
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        op: UnOpKind::Not,
        val: cond.into(),
    }));
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(if_expr).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn assert_compare_macro(left: Expr, right: Expr, op: BinOpKind, message: &str) -> Expr {
    let left_ident = Ident::new("__fp_assert_left");
    let right_ident = Ident::new("__fp_assert_right");
    let left_binding = BlockStmt::Let(StmtLet::new_simple(left_ident.clone(), left));
    let right_binding = BlockStmt::Let(StmtLet::new_simple(right_ident.clone(), right));

    let comparison = Expr::new(ExprKind::BinOp(ExprBinOp {
        kind: op,
        lhs: Expr::ident(left_ident).into(),
        rhs: Expr::ident(right_ident).into(),
    }));
    let negated = Expr::new(ExprKind::UnOp(ExprUnOp {
        op: UnOpKind::Not,
        val: comparison.into(),
    }));
    let panic_expr = panic_call_with_message(message);
    let if_expr = Expr::new(ExprKind::If(ExprIf {
        cond: negated.into(),
        then: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(panic_expr).with_semicolon(true),
        )]))
        .into(),
        elze: None,
    }));

    Expr::block(ExprBlock::new_stmts_expr(
        vec![
            left_binding,
            right_binding,
            BlockStmt::Expr(BlockStmtExpr::new(if_expr).with_semicolon(true)),
        ],
        Expr::unit(),
    ))
}

fn panic_macro(args: Vec<Expr>) -> Expr {
    let message = if args.is_empty() {
        panic_call_with_message("panic! macro triggered")
    } else {
        Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
            IntrinsicCallKind::Panic,
            IntrinsicCallPayload::Args { args },
        )))
    };
    Expr::block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Expr(
            BlockStmtExpr::new(message).with_semicolon(true),
        )],
        Expr::unit(),
    ))
}

fn panic_call_with_message(message: &str) -> Expr {
    Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
        IntrinsicCallKind::Panic,
        IntrinsicCallPayload::Args {
            args: vec![Expr::value(Value::string(message.to_string()))],
        },
    )))
}
