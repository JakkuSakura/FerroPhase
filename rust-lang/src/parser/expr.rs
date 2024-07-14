use crate::parser::item::parse_item;
use crate::parser::ty::parse_member;
use crate::{parser, RawExpr, RawExprMacro, RawStmtMacro};
use common::warn;
use eyre::bail;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::ops::{BinOpKind, UnOpKind};
use lang_core::utils::anybox::AnyBox;
use quote::ToTokens;

pub fn parse_expr(expr: syn::Expr) -> eyre::Result<AstExpr> {
    let expr = match expr {
        syn::Expr::Binary(b) => parse_expr_binary(b)?,
        syn::Expr::Unary(u) => parse_unary(u)?.into(),
        syn::Expr::Block(b) if b.label.is_none() => AstExpr::block(parse_block(b.block)?),
        syn::Expr::Call(c) => AstExpr::Invoke(parse_expr_call(c)?.into()),
        syn::Expr::If(i) => AstExpr::If(parse_expr_if(i)?),
        syn::Expr::Loop(l) => AstExpr::Loop(parse_expr_loop(l)?),
        syn::Expr::Lit(l) => AstExpr::value(parse_literal(l.lit)?),
        syn::Expr::Macro(m) => AstExpr::any(RawExprMacro { raw: m }),
        syn::Expr::MethodCall(c) => AstExpr::Invoke(parse_expr_method_call(c)?.into()),
        syn::Expr::Index(i) => AstExpr::Index(parse_expr_index(i)?),
        syn::Expr::Path(p) => AstExpr::path(parser::parse_path(p.path)?),
        syn::Expr::Reference(r) => AstExpr::Reference(parse_expr_reference(r)?.into()),
        syn::Expr::Tuple(t) => AstExpr::Tuple(parse_expr_tuple(t)?),
        syn::Expr::Struct(s) => AstExpr::Struct(parse_expr_struct(s)?.into()),
        syn::Expr::Paren(p) => AstExpr::Paren(parse_expr_paren(p)?),
        syn::Expr::Range(r) => AstExpr::Range(parse_expr_range(r)?),
        raw => {
            warn!("RawExpr {:?}", raw);
            AstExpr::Any(AnyBox::new(RawExpr { raw }))
        } // x => bail!("Expr not supported: {:?}", x),
    };
    Ok(expr)
}
pub fn parse_literal(lit: syn::Lit) -> eyre::Result<AstValue> {
    Ok(match lit {
        syn::Lit::Int(i) => AstValue::Int(ValueInt::new(i.base10_parse()?)),
        syn::Lit::Float(i) => AstValue::Decimal(ValueDecimal::new(i.base10_parse()?)),
        syn::Lit::Str(s) => AstValue::String(ValueString::new_ref(s.value())),
        syn::Lit::Bool(b) => AstValue::Bool(ValueBool::new(b.value)),
        _ => bail!("Lit not supported: {:?}", lit.to_token_stream()),
    })
}

pub fn parse_unary(u: syn::ExprUnary) -> eyre::Result<ExprUnOp> {
    let expr = parse_expr(*u.expr)?;
    let op = match u.op {
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        syn::UnOp::Not(_) => UnOpKind::Not,
        _ => bail!("Unary op not supported: {:?}", u.op),
    };
    Ok(ExprUnOp {
        op,
        val: expr.into(),
    })
}

/// returns: statement, with_semicolon
pub fn parse_stmt(stmt: syn::Stmt) -> eyre::Result<(BlockStmt, bool)> {
    Ok(match stmt {
        syn::Stmt::Local(l) => (
            BlockStmt::Let(StmtLet {
                pat: parser::parse_pat(l.pat)?,
                init: l
                    .init
                    .map(|i| parse_expr(*i.expr))
                    .transpose()?
                    .map(|x| x.into()),
            }),
            true,
        ),
        syn::Stmt::Item(tm) => (parse_item(tm).map(BlockStmt::item)?, true),
        syn::Stmt::Expr(e, semicolon) => {
            if let syn::Expr::Verbatim(v) = &e {
                if v.is_empty() {
                    return Ok((BlockStmt::noop().into(), semicolon.is_some()));
                }
            }
            (BlockStmt::Expr(parse_expr(e)?.into()), semicolon.is_some())
        }
        syn::Stmt::Macro(raw) => (BlockStmt::any(RawStmtMacro { raw }), true),
    })
}

pub fn parse_block(block: syn::Block) -> eyre::Result<ExprBlock> {
    // info!("Parsing block {:?}", block);
    let mut stmts = vec![];
    let mut last_with_semicolon = true;
    for stmt in block.stmts.into_iter() {
        let (stmt, with_semicolon) = parse_stmt(stmt)?;
        stmts.push(stmt);
        last_with_semicolon = with_semicolon;
    }
    let expr = if !last_with_semicolon {
        let expr = match stmts.pop().unwrap() {
            BlockStmt::Expr(e) => e,
            x => bail!("Last statement should be expr, but got {:?}", x),
        };
        Some(expr.into())
    } else {
        None
    };
    Ok(ExprBlock { stmts, expr })
}

pub fn parse_expr_reference(item: syn::ExprReference) -> eyre::Result<ExprReference> {
    Ok(ExprReference {
        referee: parse_expr(*item.expr)?.into(),
        mutable: Some(item.mutability.is_some()),
    })
}

pub fn parse_expr_call(call: syn::ExprCall) -> eyre::Result<ExprInvoke> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(ExprInvoke {
        target: ExprInvokeTarget::expr(fun),
        args,
    })
}

pub fn parse_expr_method_call(call: syn::ExprMethodCall) -> eyre::Result<ExprInvoke> {
    Ok(ExprInvoke {
        target: ExprInvokeTarget::Method(
            ExprSelect {
                obj: parse_expr(*call.receiver)?.into(),
                field: parser::parse_ident(call.method),
                select: ExprSelectType::Method,
            }
            .into(),
        )
        .into(),
        args: call.args.into_iter().map(parse_expr).try_collect()?,
    })
}

pub fn parse_expr_index(i: syn::ExprIndex) -> eyre::Result<ExprIndex> {
    Ok(ExprIndex {
        expr: parse_expr(*i.expr)?.into(),
        index: parse_expr(*i.index)?.into(),
    })
}

pub fn parse_expr_if(i: syn::ExprIf) -> eyre::Result<ExprIf> {
    let cond = parse_expr(*i.cond)?.into();
    let then = parse_block(i.then_branch)?;
    let elze;
    if let Some((_, e)) = i.else_branch {
        elze = Some(parse_expr(*e)?.into());
    } else {
        elze = None;
    }
    Ok(ExprIf {
        cond,
        then: AstExpr::block(then).into(),
        elze,
    })
}

pub fn parse_expr_loop(l: syn::ExprLoop) -> eyre::Result<ExprLoop> {
    Ok(ExprLoop {
        label: l.label.map(|x| parser::parse_ident(x.name.ident)),
        body: AstExpr::block(parse_block(l.body)?).into(),
    })
}

pub fn parse_expr_binary(b: syn::ExprBinary) -> eyre::Result<AstExpr> {
    let lhs = parse_expr(*b.left)?.into();
    let rhs = parse_expr(*b.right)?.into();
    let (kind, _flatten) = match b.op {
        syn::BinOp::Add(_) => (BinOpKind::Add, true),
        syn::BinOp::Mul(_) => (BinOpKind::Mul, true),
        syn::BinOp::Sub(_) => (BinOpKind::Sub, false),
        syn::BinOp::Div(_) => (BinOpKind::Div, false),
        syn::BinOp::Gt(_) => (BinOpKind::Gt, false),
        syn::BinOp::Ge(_) => (BinOpKind::Ge, false),
        syn::BinOp::Le(_) => (BinOpKind::Le, false),
        syn::BinOp::Lt(_) => (BinOpKind::Lt, false),
        syn::BinOp::Eq(_) => (BinOpKind::Eq, false),
        syn::BinOp::Ne(_) => (BinOpKind::Ne, false),
        syn::BinOp::BitOr(_) => (BinOpKind::BitOr, true),
        syn::BinOp::BitAnd(_) => (BinOpKind::BitAnd, true),
        syn::BinOp::BitXor(_) => (BinOpKind::BitXor, true),
        syn::BinOp::Or(_) => (BinOpKind::Or, true),
        syn::BinOp::And(_) => (BinOpKind::And, true),
        _ => bail!("Op not supported {:?}", b.op),
    };

    Ok(ExprBinOp { kind, lhs, rhs }.into())
}

pub fn parse_expr_tuple(t: syn::ExprTuple) -> eyre::Result<ExprTuple> {
    let mut values = vec![];
    for e in t.elems {
        let expr = parse_expr(e)?;
        values.push(expr);
    }

    Ok(ExprTuple { values })
}

pub fn parse_expr_field_value(fv: syn::FieldValue) -> eyre::Result<ExprField> {
    Ok(ExprField {
        name: parse_member(fv.member)?,
        value: parse_expr(fv.expr)?.into(),
    })
}

pub fn parse_expr_struct(s: syn::ExprStruct) -> eyre::Result<ExprStruct> {
    Ok(ExprStruct {
        name: AstExpr::path(parser::parse_path(s.path)?).into(),
        fields: s
            .fields
            .into_iter()
            .map(|x| parse_expr_field_value(x))
            .try_collect()?,
    })
}
pub fn parse_expr_paren(p: syn::ExprParen) -> eyre::Result<ExprParen> {
    Ok(ExprParen {
        expr: parse_expr(*p.expr)?.into(),
    })
}
pub fn parse_expr_range(r: syn::ExprRange) -> eyre::Result<ExprRange> {
    let start = r
        .start
        .map(|x| parse_expr(*x))
        .transpose()?
        .map(|x| x.into());
    let limit = match r.limits {
        syn::RangeLimits::HalfOpen(_) => ExprRangeLimit::Exclusive,
        syn::RangeLimits::Closed(_) => ExprRangeLimit::Inclusive,
    };
    let end = r.end.map(|x| parse_expr(*x)).transpose()?.map(|x| x.into());
    Ok(ExprRange {
        start,
        limit,
        end,
        step: None,
    })
}
