use super::pattern_expansion::{expand_pattern_alternatives, parse_pattern_alternatives};
use super::*;
use fp_core::ast::ExprLet;
use fp_core::ast::PatternBind;
use fp_core::ast::PatternRef;
use fp_core::ast::path::PathPrefix;
use winnow::Parser;

mod control_flow;
mod macro_expr;
use control_flow::*;
use macro_expr::*;
pub(crate) use macro_expr::{parse_macro_expr, parse_macro_group, parse_macro_path};
pub(crate) mod patterns;
use patterns::*;

pub(crate) fn parse_expr_winnow(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_assignment(input, file)
}

pub(crate) fn parse_expr_winnow_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_assignment_no_struct(input, file)
}

fn parse_assignment(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let lhs = parse_range(input, file)?;
    parse_assignment_tail(input, file, lhs, parse_assignment)
}

/// Continue a parsed left-hand side into a full assignment expression if an
/// assignment operator follows, otherwise return `lhs` unchanged — shared by
/// `parse_assignment`'s own top-level LHS and `parse_block_stmt_entry`'s
/// block-like-statement branch (real `std::sys::pal::sgx::waitqueue::
/// unsafe_list`'s own `unsafe { self.head_tail.as_mut() }.next = self.
/// head_tail;`), whose LHS is built via `parse_primary` + postfix suffixes
/// rather than through `parse_assignment`'s own `parse_range` entry point,
/// so it would otherwise never get a chance to look for a trailing `=`.
fn parse_assignment_tail(
    input: &mut &[Token],
    file: FileId,
    lhs: Expr,
    parse_rhs: fn(&mut &[Token], FileId) -> ModalResult<Expr>,
) -> ModalResult<Expr> {
    let Some(op) = peek_symbol(input) else {
        return Ok(lhs);
    };
    if !matches!(
        op,
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="
    ) {
        return Ok(lhs);
    }
    let op = op.to_string();
    skip_symbol(input, &op)?;
    let rhs = parse_rhs(input, file)?;
    if op == "=" {
        return Ok(ExprKind::Assign(ExprAssign {
            span: union_exprs(&lhs, &rhs),
            target: Box::new(lhs),
            value: Box::new(rhs),
        })
        .into());
    }
    let kind = match op.as_str() {
        "+=" => BinOpKind::Add,
        "-=" => BinOpKind::Sub,
        "*=" => BinOpKind::Mul,
        "/=" => BinOpKind::Div,
        "%=" => BinOpKind::Mod,
        "^=" => BinOpKind::BitXor,
        "&=" => BinOpKind::BitAnd,
        "|=" => BinOpKind::BitOr,
        "<<=" => BinOpKind::Shl,
        ">>=" => BinOpKind::Shr,
        _ => unreachable!(),
    };
    let target_clone = lhs.clone();
    let value = ExprKind::BinOp(ExprBinOp {
        span: union_exprs(&target_clone, &rhs),
        kind,
        lhs: Box::new(target_clone),
        rhs: Box::new(rhs),
    })
    .into();
    Ok(ExprKind::Assign(ExprAssign {
        span: union_exprs(&lhs, &value),
        target: Box::new(lhs),
        value: Box::new(value),
    })
    .into())
}

fn parse_assignment_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let lhs = parse_range_no_struct(input, file)?;
    let Some(op) = peek_symbol(input) else {
        return Ok(lhs);
    };
    if !matches!(
        op,
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" | "<<=" | ">>="
    ) {
        return Ok(lhs);
    }
    let op = op.to_string();
    skip_symbol(input, &op)?;
    let rhs = parse_assignment_no_struct(input, file)?;
    if op == "=" {
        return Ok(ExprKind::Assign(ExprAssign {
            span: union_exprs(&lhs, &rhs),
            target: Box::new(lhs),
            value: Box::new(rhs),
        })
        .into());
    }
    let kind = match op.as_str() {
        "+=" => BinOpKind::Add,
        "-=" => BinOpKind::Sub,
        "*=" => BinOpKind::Mul,
        "/=" => BinOpKind::Div,
        "%=" => BinOpKind::Mod,
        "^=" => BinOpKind::BitXor,
        "&=" => BinOpKind::BitAnd,
        "|=" => BinOpKind::BitOr,
        "<<=" => BinOpKind::Shl,
        ">>=" => BinOpKind::Shr,
        _ => unreachable!(),
    };
    let target_clone = lhs.clone();
    let value = ExprKind::BinOp(ExprBinOp {
        span: union_exprs(&target_clone, &rhs),
        kind,
        lhs: Box::new(target_clone),
        rhs: Box::new(rhs),
    })
    .into();
    Ok(ExprKind::Assign(ExprAssign {
        span: union_exprs(&lhs, &value),
        target: Box::new(lhs),
        value: Box::new(value),
    })
    .into())
}

fn parse_range(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    if let Some(op) = peek_symbol(input) {
        let limit = match op {
            ".." => Some(ExprRangeLimit::Exclusive),
            "..=" => Some(ExprRangeLimit::Inclusive),
            _ => None,
        };
        if let Some(limit) = limit {
            skip_symbol(input, op)?;
            let rhs = if terminates_expr(input) {
                None
            } else {
                Some(parse_binary(input, file, 0)?)
            };
            let span = rhs
                .as_ref()
                .map(|expr| span_from_expr(expr))
                .unwrap_or_else(Span::null);
            return Ok(ExprKind::Range(ExprRange {
                span,
                start: None,
                limit,
                end: rhs.map(Box::new),
                step: None,
            })
            .into());
        }
    }
    let lhs = parse_binary(input, file, 0)?;
    let Some(op) = peek_symbol(input) else {
        return Ok(lhs);
    };
    let limit = match op {
        ".." => ExprRangeLimit::Exclusive,
        "..=" => ExprRangeLimit::Inclusive,
        _ => return Ok(lhs),
    };
    let op = op.to_string();
    skip_symbol(input, &op)?;
    let rhs = if terminates_expr(input) {
        None
    } else {
        Some(parse_binary(input, file, 0)?)
    };
    let span = rhs
        .as_ref()
        .map(|expr| union_exprs(&lhs, expr))
        .unwrap_or_else(|| span_from_expr(&lhs));
    Ok(ExprKind::Range(ExprRange {
        span,
        start: Some(Box::new(lhs)),
        limit,
        end: rhs.map(Box::new),
        step: None,
    })
    .into())
}

fn parse_range_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    if let Some(op) = peek_symbol(input) {
        let limit = match op {
            ".." => Some(ExprRangeLimit::Exclusive),
            "..=" => Some(ExprRangeLimit::Inclusive),
            _ => None,
        };
        if let Some(limit) = limit {
            skip_symbol(input, op)?;
            let rhs = if terminates_expr(input) {
                None
            } else {
                Some(parse_binary_no_struct(input, file, 0)?)
            };
            let span = rhs
                .as_ref()
                .map(|expr| span_from_expr(expr))
                .unwrap_or_else(Span::null);
            return Ok(ExprKind::Range(ExprRange {
                span,
                start: None,
                limit,
                end: rhs.map(Box::new),
                step: None,
            })
            .into());
        }
    }
    let lhs = parse_binary_no_struct(input, file, 0)?;
    let Some(op) = peek_symbol(input) else {
        return Ok(lhs);
    };
    let limit = match op {
        ".." => ExprRangeLimit::Exclusive,
        "..=" => ExprRangeLimit::Inclusive,
        _ => return Ok(lhs),
    };
    let op = op.to_string();
    skip_symbol(input, &op)?;
    let rhs = if terminates_expr(input) {
        None
    } else {
        Some(parse_binary_no_struct(input, file, 0)?)
    };
    let span = rhs
        .as_ref()
        .map(|expr| union_exprs(&lhs, expr))
        .unwrap_or_else(|| span_from_expr(&lhs));
    Ok(ExprKind::Range(ExprRange {
        span,
        start: Some(Box::new(lhs)),
        limit,
        end: rhs.map(Box::new),
        step: None,
    })
    .into())
}

fn parse_binary(input: &mut &[Token], file: FileId, min_prec: u8) -> ModalResult<Expr> {
    let mut lhs = parse_cast(input, file)?;
    loop {
        let Some((op, prec, kind)) = peek_binary_op(input) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        consume_binary_op(input, op)?;
        let rhs = parse_binary(input, file, prec + 1)?;
        lhs = ExprKind::BinOp(ExprBinOp {
            span: union_exprs(&lhs, &rhs),
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .into();
    }
    Ok(lhs)
}

fn parse_binary_no_struct(input: &mut &[Token], file: FileId, min_prec: u8) -> ModalResult<Expr> {
    let mut lhs = parse_cast_no_struct(input, file)?;
    loop {
        let Some((op, prec, kind)) = peek_binary_op(input) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        consume_binary_op(input, op)?;
        let rhs = parse_binary_no_struct(input, file, prec + 1)?;
        lhs = ExprKind::BinOp(ExprBinOp {
            span: union_exprs(&lhs, &rhs),
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
        .into();
    }
    Ok(lhs)
}

fn parse_cast(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut expr = parse_prefix(input, file)?;
    loop {
        let mut probe = *input;
        if skip_keyword(&mut probe, Keyword::As).is_err() {
            break;
        }
        let ty = parse_simple_type(&mut probe)?;
        *input = probe;
        // `expr as type<_>` or `expr as type<Concrete>` → intrinsic call
        if let Ty::Type(_) = ty {
            expr = ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                CallKind::BuildType,
                vec![expr.clone()],
                Vec::new(),
            ))
            .into();
        } else {
            let span = span_from_expr(&expr);
            expr = ExprKind::Cast(ExprCast {
                span,
                expr: Box::new(expr),
                ty,
            })
            .into();
        }
    }
    Ok(expr)
}

/// `pub(crate)`, not module-private: `types.rs`'s `parse_type_arg` needs
/// this exact precedence level (unary/postfix/`as`-cast, below any binary
/// operator) to parse a const-generic type argument's value (`Foo<char,
/// 3>`) without also trying to continue past it into a binary comparison
/// — see that call site's own doc comment.
pub(crate) fn parse_cast_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut expr = parse_prefix_no_struct(input, file)?;
    loop {
        let mut probe = *input;
        if skip_keyword(&mut probe, Keyword::As).is_err() {
            break;
        }
        let ty = parse_simple_type(&mut probe)?;
        *input = probe;
        if let Ty::Type(_) = ty {
            expr = ExprKind::IntrinsicCall(ExprIntrinsicCall::new(
                CallKind::BuildType,
                vec![expr.clone()],
                Vec::new(),
            ))
            .into();
        } else {
            let span = span_from_expr(&expr);
            expr = ExprKind::Cast(ExprCast {
                span,
                expr: Box::new(expr),
                ty,
            })
            .into();
        }
    }
    Ok(expr)
}

fn parse_prefix(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    // An arbitrary expression in value position may carry its own
    // attribute (real `std::panicking`'s own `let write = #[optimize(size)]
    // |err: &mut dyn ..| { .. };`) — carries no meaning this checker models
    // for an arbitrary expression, so it's dropped here, same as call
    // arguments/match arms/etc. already do. Idempotent if the caller
    // already skipped attributes itself.
    skip_outer_attrs_before_expr(input, file)?;
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Splice).is_ok() {
        if let Ok(inner) =
            parse_prefix(&mut probe, file).or_else(|_| parse_primary(&mut probe, file))
        {
            *input = probe;
            let token = match inner.kind().clone() {
                ExprKind::Paren(paren) => *paren.expr,
                _ => inner,
            };
            if matches!(token.kind(), ExprKind::Quote(quote) if matches!(quote.kind, Some(QuoteFragmentKind::Item)))
            {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            return Ok(ExprKind::Splice(ExprSplice {
                span: span_from_expr(&token),
                token: Box::new(token),
            })
            .into());
        }
    }
    let mut probe = *input;
    if let Ok(emit_token) = expect_keyword(&mut probe, Keyword::Emit) {
        let emit_span = token_span_to_span(&emit_token);
        if skip_symbol(&mut probe, "!").is_ok() && peek_symbol(probe) == Some("{") {
            let block = parse_balanced_quote_block(&mut probe, file)?;
            *input = probe;
            let quote_expr = Expr::new(ExprKind::Quote(ExprQuote {
                span: block.span,
                block,
                collected_items: Vec::new(),
                kind: None,
            }));
            return Ok(ExprKind::Splice(ExprSplice {
                span: emit_span,
                token: Box::new(quote_expr),
            })
            .into());
        }
    }
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Await).is_ok() {
        let base = parse_prefix(&mut probe, file)?;
        *input = probe;
        return Ok(ExprKind::Await(ExprAwait {
            span: span_from_expr(&base),
            base: Box::new(base),
        })
        .into());
    }

    // `&&expr` — a double reference (`&(&expr)`), not the boolean `&&`
    // operator, whenever it appears in prefix/operand position (real
    // `core::array`'s own `fmt::Debug::fmt(&&self[..], f)`). The
    // tokenizer emits `&&` as one lexeme (same as the logical-and binary
    // operator), so this must be split apart here rather than relying on
    // two separate `&` tokens.
    if try_eat_symbol(input, "&&") {
        let is_mut_ref = skip_keyword(input, Keyword::Mut).is_ok();
        let inner_value = parse_prefix(input, file)?;
        let inner = ExprKind::Reference(ExprReference {
            span: span_from_expr(&inner_value),
            referee: Box::new(inner_value),
            mutable: is_mut_ref.then_some(true),
            raw: false,
        })
        .into();
        return Ok(ExprKind::Reference(ExprReference {
            span: span_from_expr(&inner),
            referee: Box::new(inner),
            mutable: None,
            raw: false,
        })
        .into());
    }

    if let Some(op) = peek_symbol(input) {
        if matches!(op, "!" | "-" | "*" | "&") {
            let op = op.to_string();
            skip_symbol(input, &op)?;
            // `raw` is an identifier rather than a lexer keyword, so raw
            // address-of syntax must be recognized positionally.
            let is_raw_ref = op == "&"
                && matches!(
                    input.first(),
                    Some(token) if token.kind == TokenKind::Ident && token.lexeme == "raw"
                )
                && matches!(
                    input.get(1).map(|t| &t.kind),
                    Some(TokenKind::Keyword(Keyword::Const | Keyword::Mut))
                );
            if is_raw_ref {
                *input = &input[1..]; // `raw`
            }
            let is_mut_ref = op == "&" && skip_keyword(input, Keyword::Mut).is_ok();
            let _ = op == "&" && skip_keyword(input, Keyword::Const).is_ok();
            let value = parse_prefix(input, file)?;
            if op == "&" {
                return Ok(ExprKind::Reference(ExprReference {
                    span: span_from_expr(&value),
                    referee: Box::new(value),
                    mutable: is_mut_ref.then_some(true),
                    raw: is_raw_ref,
                })
                .into());
            }
            let op = match op.as_str() {
                "!" => UnOpKind::Not,
                "-" => UnOpKind::Neg,
                "*" => UnOpKind::Deref,
                _ => unreachable!(),
            };
            return Ok(ExprKind::UnOp(ExprUnOp {
                span: span_from_expr(&value),
                op,
                val: Box::new(value),
            })
            .into());
        }
    }

    parse_postfix(input, file)
}

fn parse_prefix_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    skip_outer_attrs_before_expr(input, file)?;
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Splice).is_ok() {
        if let Ok(inner) = parse_prefix_no_struct(&mut probe, file)
            .or_else(|_| parse_primary_no_struct(&mut probe, file))
        {
            *input = probe;
            let token = match inner.kind().clone() {
                ExprKind::Paren(paren) => *paren.expr,
                _ => inner,
            };
            if matches!(token.kind(), ExprKind::Quote(quote) if matches!(quote.kind, Some(QuoteFragmentKind::Item)))
            {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            return Ok(ExprKind::Splice(ExprSplice {
                span: span_from_expr(&token),
                token: Box::new(token),
            })
            .into());
        }
    }
    let mut probe = *input;
    if let Ok(emit_token) = expect_keyword(&mut probe, Keyword::Emit) {
        let emit_span = token_span_to_span(&emit_token);
        if skip_symbol(&mut probe, "!").is_ok() && peek_symbol(probe) == Some("{") {
            let block = parse_balanced_quote_block(&mut probe, file)?;
            *input = probe;
            let quote_expr = Expr::new(ExprKind::Quote(ExprQuote {
                span: block.span,
                block,
                collected_items: Vec::new(),
                kind: None,
            }));
            return Ok(ExprKind::Splice(ExprSplice {
                span: emit_span,
                token: Box::new(quote_expr),
            })
            .into());
        }
    }
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Await).is_ok() {
        let base = parse_prefix_no_struct(&mut probe, file)?;
        *input = probe;
        return Ok(ExprKind::Await(ExprAwait {
            span: span_from_expr(&base),
            base: Box::new(base),
        })
        .into());
    }

    if try_eat_symbol(input, "&&") {
        let is_mut_ref = skip_keyword(input, Keyword::Mut).is_ok();
        let inner_value = parse_prefix_no_struct(input, file)?;
        let inner = ExprKind::Reference(ExprReference {
            span: span_from_expr(&inner_value),
            referee: Box::new(inner_value),
            mutable: is_mut_ref.then_some(true),
            raw: false,
        })
        .into();
        return Ok(ExprKind::Reference(ExprReference {
            span: span_from_expr(&inner),
            referee: Box::new(inner),
            mutable: None,
            raw: false,
        })
        .into());
    }

    if let Some(op) = peek_symbol(input) {
        if matches!(op, "!" | "-" | "*" | "&") {
            let op = op.to_string();
            skip_symbol(input, &op)?;
            let is_raw_ref = op == "&"
                && matches!(
                    input.first(),
                    Some(token) if token.kind == TokenKind::Ident && token.lexeme == "raw"
                )
                && matches!(
                    input.get(1).map(|t| &t.kind),
                    Some(TokenKind::Keyword(Keyword::Const | Keyword::Mut))
                );
            if is_raw_ref {
                *input = &input[1..]; // `raw`
            }
            let is_mut_ref = op == "&" && skip_keyword(input, Keyword::Mut).is_ok();
            let _ = op == "&" && skip_keyword(input, Keyword::Const).is_ok();
            let value = parse_prefix_no_struct(input, file)?;
            if op == "&" {
                return Ok(ExprKind::Reference(ExprReference {
                    span: span_from_expr(&value),
                    referee: Box::new(value),
                    mutable: is_mut_ref.then_some(true),
                    raw: is_raw_ref,
                })
                .into());
            }
            let op = match op.as_str() {
                "!" => UnOpKind::Not,
                "-" => UnOpKind::Neg,
                "*" => UnOpKind::Deref,
                _ => unreachable!(),
            };
            return Ok(ExprKind::UnOp(ExprUnOp {
                span: span_from_expr(&value),
                op,
                val: Box::new(value),
            })
            .into());
        }
    }

    let base = parse_primary_no_struct(input, file)?;
    let suffixes: Vec<Postfix> = repeat(0.., |input: &mut &[Token]| {
        parse_postfix_suffix(input, file)
    })
    .parse_next(input)?;
    Ok(apply_postfixes(base, suffixes))
}

fn parse_postfix(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let base = parse_primary(input, file)?;
    let suffixes: Vec<Postfix> = repeat(0.., |input: &mut &[Token]| {
        parse_postfix_suffix(input, file)
    })
    .parse_next(input)?;
    let expr = apply_postfixes(base, suffixes);
    parse_struct_literal_after_expr(input, file, expr)
}

fn parse_keyword_name_expr_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let base = parse_name_expr(input)?;
    let suffixes: Vec<Postfix> = repeat(0.., |input: &mut &[Token]| {
        parse_postfix_suffix(input, file)
    })
    .parse_next(input)?;
    Ok(apply_postfixes(base, suffixes))
}

fn parse_postfix_suffix(input: &mut &[Token], file: FileId) -> ModalResult<Postfix> {
    alt((
        parse_try_suffix,
        parse_field_suffix,
        parse_scope_field_suffix,
        parse_turbofish_suffix,
        |input: &mut &[Token]| parse_call_suffix(input, file),
        |input: &mut &[Token]| parse_index_suffix(input, file),
    ))
    .parse_next(input)
}

fn parse_try_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    skip_symbol(input, "?")?;
    Ok(Postfix::Try)
}

fn parse_field_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    skip_symbol(input, ".")?;
    let field = match input.split_first() {
        Some((token, rest)) if token.kind == TokenKind::Number => {
            *input = rest;
            Ident::new(token.lexeme.clone())
        }
        _ => ident_like(input)?,
    };
    Ok(Postfix::Field(field))
}

fn parse_scope_field_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    let mut probe = *input;
    skip_symbol(&mut probe, "::")?;
    let field = ident_like(&mut probe)?;
    *input = probe;
    Ok(Postfix::ConstField(field))
}

fn parse_turbofish_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    let mut probe = *input;
    if skip_symbol(&mut probe, "::").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let args = parse_optional_type_args(&mut probe)?;
    if args.is_empty() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    *input = probe;
    Ok(Postfix::Turbofish(args))
}

fn parse_call_suffix(input: &mut &[Token], file: FileId) -> ModalResult<Postfix> {
    skip_symbol(input, "(")?;
    let (args, kwargs) = parse_call_args(input, file, ")")?;
    skip_symbol(input, ")")?;
    Ok(Postfix::Call(args, kwargs))
}

fn parse_index_suffix(input: &mut &[Token], file: FileId) -> ModalResult<Postfix> {
    skip_symbol(input, "[")?;
    let index = parse_expr_winnow(input, file)?;
    skip_symbol(input, "]")?;
    Ok(Postfix::Index(index))
}

fn parse_primary(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    alt((
        |input: &mut &[Token]| parse_labeled_expr(input, file),
        alt((
            |input: &mut &[Token]| parse_closure_expr(input, file),
            |input: &mut &[Token]| parse_if_expr(input, file),
            |input: &mut &[Token]| parse_let_expr(input, file),
            |input: &mut &[Token]| parse_loop_expr(input, file),
            |input: &mut &[Token]| parse_while_expr(input, file),
            |input: &mut &[Token]| parse_for_expr(input, file),
            |input: &mut &[Token]| parse_with_expr(input, file),
            |input: &mut &[Token]| parse_unsafe_block_expr(input, file),
            |input: &mut &[Token]| parse_async_expr(input, file),
            |input: &mut &[Token]| parse_const_block_expr(input, file),
        )),
        alt((
            |input: &mut &[Token]| parse_return_expr(input, file),
            |input: &mut &[Token]| parse_break_expr(input, file),
            parse_continue_expr,
            |input: &mut &[Token]| parse_try_structured(input, file),
            |input: &mut &[Token]| parse_match_expr(input, file),
            |input: &mut &[Token]| parse_quote_expr(input, file),
            |input: &mut &[Token]| parse_block_expr(input, file),
            |input: &mut &[Token]| parse_struct_expr(input, file),
        )),
        alt((
            parse_macro_expr,
            parse_number,
            |input: &mut &[Token]| parse_string(input, file),
            |input: &mut &[Token]| parse_array_expr(input, file),
            |input: &mut &[Token]| parse_grouped(input, file),
            |input: &mut &[Token]| parse_qualified_path_expr(input, file),
            parse_name_expr,
        )),
    ))
    .parse_next(input)
}

/// A UFCS-disambiguated qualified path (`<Type as Trait>::assoc_item`,
/// or the trait-less `<Type>::assoc_item`) — real `alloc::boxed`'s own
/// `<T as SizedTypeProperties>::method(..)` needs this. This checker has
/// The parsed `as Trait` disambiguator is retained in `Name::qself` and the
/// trait path is kept in the ordinary path segments, matching rustc's AST
/// shape. Selection of a concrete impl remains a type-checking concern, and
/// the ordinary postfix chain (`::field`, calls, ...) continues from there.
fn parse_qualified_path_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if !try_eat_symbol(&mut probe, "<") {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let ty = parse_type_expr(&mut probe)?;
    let trait_ty = if skip_keyword(&mut probe, Keyword::As).is_ok() {
        Some(parse_type_expr(&mut probe)?)
    } else {
        None
    };
    skip_symbol(&mut probe, ">")?;
    let _ = file;
    let mut assoc_probe = probe;
    if skip_symbol(&mut assoc_probe, "::").is_ok() {
        let assoc = parse_name(&mut assoc_probe)?;
        let (mut prefix, mut segments) = match assoc {
            Name { path, .. } => (path.prefix, path.segments),
        };
        let trait_path = trait_ty.as_ref().and_then(|trait_ty| match trait_ty {
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(Name { qself: None, path }) => Some(path),
                _ => None,
            },
            _ => None,
        });
        // `position` is where the qself is inserted in the complete path,
        // i.e. immediately after the trait path and before its associated
        // item.  This is the rustc AST convention (`<T as Trait>::Item`
        // has position 1 for the `Trait::Item` path).
        let position = trait_path.map_or(0, |path| path.segments.len());
        if let Some(trait_path) = trait_path {
            prefix = trait_path.prefix;
            let mut qualified = trait_path.segments.clone();
            qualified.append(&mut segments);
            segments = qualified;
        }
        *input = assoc_probe;
        return Ok(Expr::name(Name {
            qself: Some(fp_core::ast::QSelf {
                ty: Box::new(ty),
                path_span: trait_ty.as_ref().map(Ty::span).unwrap_or_else(Span::null),
                position,
            }),
            path: Path::new(prefix, segments),
        }));
    }
    *input = probe;
    Ok(type_to_expr(&ty))
}

fn parse_primary_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    alt((
        |input: &mut &[Token]| parse_labeled_expr(input, file),
        alt((
            |input: &mut &[Token]| parse_closure_expr(input, file),
            |input: &mut &[Token]| parse_if_expr(input, file),
            |input: &mut &[Token]| parse_let_expr_no_struct(input, file),
            |input: &mut &[Token]| parse_loop_expr(input, file),
            |input: &mut &[Token]| parse_while_expr(input, file),
            |input: &mut &[Token]| parse_for_expr(input, file),
            |input: &mut &[Token]| parse_with_expr(input, file),
            |input: &mut &[Token]| parse_unsafe_block_expr(input, file),
            |input: &mut &[Token]| parse_async_expr(input, file),
            |input: &mut &[Token]| parse_const_block_expr(input, file),
        )),
        alt((
            |input: &mut &[Token]| parse_return_expr(input, file),
            |input: &mut &[Token]| parse_break_expr(input, file),
            parse_continue_expr,
            |input: &mut &[Token]| parse_try_structured(input, file),
            |input: &mut &[Token]| parse_match_expr(input, file),
            |input: &mut &[Token]| parse_quote_expr(input, file),
            |input: &mut &[Token]| parse_block_expr(input, file),
        )),
        alt((
            parse_macro_expr,
            parse_number,
            |input: &mut &[Token]| parse_string(input, file),
            |input: &mut &[Token]| parse_array_expr(input, file),
            |input: &mut &[Token]| parse_grouped(input, file),
            |input: &mut &[Token]| parse_qualified_path_expr(input, file),
            parse_name_expr,
        )),
    ))
    .parse_next(input)
}

fn parse_array_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    skip_symbol(input, "[")?;
    if peek_symbol(input) == Some("]") {
        skip_symbol(input, "]")?;
        return Ok(ExprKind::Array(ExprArray {
            span: Span::null(),
            values: Vec::new(),
        })
        .into());
    }

    skip_outer_attrs_before_expr(input, file)?;
    let first = parse_expr_winnow(input, file)?;
    if skip_symbol(input, ";").is_ok() {
        let len = parse_expr_winnow(input, file)?;
        skip_symbol(input, "]")?;
        return Ok(ExprKind::ArrayRepeat(ExprArrayRepeat {
            span: union_exprs(&first, &len),
            elem: Box::new(first),
            len: Box::new(len),
        })
        .into());
    }

    let mut values = vec![first];
    while skip_symbol(input, ",").is_ok() {
        if peek_symbol(input) == Some("]") {
            break;
        }
        skip_outer_attrs_before_expr(input, file)?;
        values.push(parse_expr_winnow(input, file)?);
    }
    skip_symbol(input, "]")?;
    Ok(ExprKind::Array(ExprArray {
        span: Span::null(),
        values,
    })
    .into())
}

fn parse_grouped(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let open = expect_symbol(input, "(")?;
    if peek_symbol(input) == Some(")") {
        let close = expect_symbol(input, ")")?;
        let mut expr = Expr::value(Value::unit());
        expr.span = Some(Span::union([
            token_span_to_span(&open),
            token_span_to_span(&close),
        ]));
        return Ok(expr);
    }
    let expr = parse_expr_winnow(input, file)?;
    if skip_symbol(input, ",").is_ok() {
        let mut values = vec![expr];
        if peek_symbol(input) != Some(")") {
            loop {
                values.push(parse_expr_winnow(input, file)?);
                if skip_symbol(input, ",").is_err() {
                    break;
                }
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        let close = expect_symbol(input, ")")?;
        let span = Span::union(
            [token_span_to_span(&open), token_span_to_span(&close)]
                .into_iter()
                .chain(values.iter().map(Expr::span)),
        );
        return Ok(ExprKind::Tuple(ExprTuple { span, values }).into());
    }
    let close = expect_symbol(input, ")")?;
    Ok(ExprKind::Paren(ExprParen {
        span: Span::union([
            token_span_to_span(&open),
            expr.span(),
            token_span_to_span(&close),
        ]),
        expr: Box::new(expr),
    })
    .into())
}

fn parse_number(input: &mut &[Token]) -> ModalResult<Expr> {
    let token = token_kind(input, TokenKind::Number)?;
    let (value, ty) = parse_numeric_literal_local(&token.lexeme)
        .map_err(|_| ErrMode::Cut(ContextError::new()))?;
    let node = Expr::value(value).with_span(token_span_to_span(&token));
    let _ = ty;
    Ok(node)
}

fn parse_string(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let token = token_kind(input, TokenKind::StringLiteral)?;
    if token.lexeme.starts_with('f') {
        return parse_f_string_literal_local(&token.lexeme, file)
            .map_err(|_| ErrMode::Cut(ContextError::new()));
    }
    // `b'x'` (a `u8` byte literal) and `'x'` (a `char` literal) are single-quote
    // delimited — distinguish them from double-quoted strings before decoding,
    // or both collapse into an indistinguishable `Value::String`.
    if let Some(inner) = token
        .lexeme
        .strip_prefix("b'")
        .and_then(|rest| rest.strip_suffix('\''))
    {
        if let Some(ch) = decode_single_char_literal(inner) {
            let byte = u32::from(ch).min(u8::MAX as u32) as u64;
            let ty = Ty::Primitive(TypePrimitive::Int(TypeInt::U8));
            // TODO(ty-removal): see `parse_number`'s matching TODO.
            let _ = ty;
            return Ok(Expr::value(Value::UInt(ValueUInt::new(byte)))
                .with_span(token_span_to_span(&token)));
        }
    }
    if let Some(inner) = token
        .lexeme
        .strip_prefix('\'')
        .and_then(|rest| rest.strip_suffix('\''))
    {
        if let Some(ch) = decode_single_char_literal(inner) {
            let ty = Ty::Primitive(TypePrimitive::Char);
            // TODO(ty-removal): see `parse_number`'s matching TODO.
            let _ = ty;
            return Ok(
                Expr::value(Value::Char(ValueChar::new(ch))).with_span(token_span_to_span(&token))
            );
        }
    }
    // `b"..."`/`c"..."` — real byte-string / C-string literals, typed as
    // `&[u8; N]`/`&std::ffi::CStr` respectively (matching rustc; see
    // `decode_bytes_literal`'s doc comment for what it does and doesn't
    // decode). NUL-termination for `c"..."` is left to the existing
    // runtime FFI marshaling (`fp-native/src/ffi.rs`), which already
    // NUL-terminates any `&CStr`-typed argument at the call site — the
    // literal itself carries only its content bytes.
    if token.lexeme.starts_with('b') || token.lexeme.starts_with('c') {
        let is_cstr = token.lexeme.starts_with('c');
        let bytes =
            decode_bytes_literal(&token.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        let ty = if is_cstr {
            Ty::Reference(TypeReference {
                ty: Box::new(Ty::path(Path::plain(vec![
                    Ident::new("std"),
                    Ident::new("ffi"),
                    Ident::new("CStr"),
                ]))),
                mutability: None,
                lifetime: None,
            })
        } else {
            Ty::Reference(TypeReference {
                ty: Box::new(Ty::Array(
                    TypeArray {
                        elem: Box::new(Ty::Primitive(TypePrimitive::Int(TypeInt::U8))),
                        len: Box::new(Expr::value(Value::int(bytes.len() as i64))),
                    }
                    .into(),
                )),
                mutability: None,
                lifetime: None,
            })
        };
        let node = Expr::value(Value::Bytes(ValueBytes::from(bytes.as_slice())))
            .with_span(token_span_to_span(&token));
        let _ = ty;
        return Ok(node);
    }
    let value =
        decode_string_literal(&token.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    let ty = Ty::Reference(TypeReference {
        ty: Box::new(Ty::Primitive(TypePrimitive::String)),
        mutability: None,
        lifetime: None,
    });
    // TODO(ty-removal): see `parse_number`'s matching TODO.
    let _ = ty;
    Ok(Expr::value(Value::string(value)).with_span(token_span_to_span(&token)))
}

fn parse_name_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let span = input
        .first()
        .map(token_span_to_span)
        .unwrap_or_else(Span::null);
    let name = parse_name(input)?;
    match name.as_ident().map(Ident::as_str) {
        // TODO(ty-removal): see `parse_number`'s matching TODO.
        Some("true") => Ok(Expr::value(Value::bool(true)).with_span(span)),
        Some("false") => Ok(Expr::value(Value::bool(false)).with_span(span)),
        Some("null") => Ok(Expr::value(Value::null()).with_span(span)),
        _ => Ok(Expr::name(name).with_span(span)),
    }
}

#[derive(Debug)]
enum Postfix {
    Try,
    Field(Ident),
    /// A `::name` postfix (`parse_scope_field_suffix`) — syntactically the
    /// same "select a name off the preceding expression" shape as `.name`,
    /// but semantically a *path* continuation (`u8::MAX`, `Map::new`), never
    /// a runtime field access. Kept distinct from `Field` from parsing
    /// onward so `apply_postfixes`/AST-to-HIR lowering can tell them apart
    /// instead of only being able to distinguish them once resolved.
    ConstField(Ident),
    Turbofish(Vec<Ty>),
    Call(Vec<Expr>, Vec<ExprKwArg>),
    Index(Expr),
}

fn apply_postfixes(mut expr: Expr, suffixes: Vec<Postfix>) -> Expr {
    for suffix in suffixes {
        expr = match suffix {
            Postfix::Try => ExprKind::Try(ExprTry {
                span: span_from_expr(&expr),
                expr: Box::new(expr),
                catches: Vec::new(),
                elze: None,
                finally: None,
            })
            .into(),
            Postfix::Field(field) => ExprKind::FieldAccess(ExprFieldAccess {
                span: span_from_expr(&expr),
                obj: Box::new(expr),
                field,
                generic_args: Vec::new(),
            })
            .into(),
            Postfix::ConstField(field) => {
                let span = span_from_expr(&expr);
                match expr.kind {
                    ExprKind::Name(name) => {
                        let mut name = name;
                        name.path.segments.push(field.into());
                        Expr::new(ExprKind::Name(name)).with_span(span)
                    }
                    _ => ExprKind::FieldAccess(ExprFieldAccess {
                        span,
                        obj: Box::new(expr),
                        field,
                        generic_args: Vec::new(),
                    })
                    .into(),
                }
            }
            Postfix::Turbofish(args) => match expr.kind {
                ExprKind::FieldAccess(mut select) => {
                    select.generic_args = args;
                    expr = Expr::new(ExprKind::FieldAccess(select));
                    expr
                }
                _ => expr,
            },
            Postfix::Call(args, kwargs) => ExprKind::Invoke(ExprInvoke {
                span: span_from_expr(&expr),
                target: ExprInvokeTarget::expr(expr),
                args,
                kwargs,
            })
            .into(),
            Postfix::Index(index) => ExprKind::Index(ExprIndex {
                span: union_exprs(&expr, &index),
                obj: Box::new(expr),
                index: Box::new(index),
            })
            .into(),
        };
    }
    expr
}

fn parse_struct_literal_after_expr(
    input: &mut &[Token],
    file: FileId,
    name: Expr,
) -> ModalResult<Expr> {
    if !expr_can_start_struct_literal(&name) {
        return Ok(name);
    }
    let mut probe = *input;
    if skip_symbol(&mut probe, "{").is_err() {
        return Ok(name);
    }
    let (fields, update) = parse_struct_literal_fields(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::Struct(ExprStruct {
        span: span_from_expr(&name),
        name: Box::new(name),
        fields,
        update,
    })
    .into())
}

fn expr_can_start_struct_literal(expr: &Expr) -> bool {
    match expr.kind() {
        ExprKind::Name(_) => true,
        ExprKind::FieldAccess(select) => expr_can_start_struct_literal(&select.obj),
        _ => false,
    }
}

fn parse_struct_literal_fields(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<(Vec<ExprField>, Option<Box<Expr>>)> {
    let mut fields = Vec::new();
    let mut update = None;
    while peek_symbol(*input) != Some("}") {
        skip_outer_attrs_in_struct_literal(input)?;
        if skip_symbol(input, "..").is_ok() {
            if update.is_some() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            update = Some(Box::new(parse_expr_winnow(input, file)?));
            let mut comma_probe = *input;
            if skip_symbol(&mut comma_probe, ",").is_ok() {
                *input = comma_probe;
                if peek_symbol(*input) == Some("}") {
                    break;
                }
                continue;
            }
            break;
        }
        // A numeric field name (real `std::sys::time::xous`'s own
        // `Instant { 0: Duration::from_millis(..), 1: .. }`) — tuple-struct
        // literal syntax naming positional fields by index rather than by
        // an identifier. `ExprField.name` is an `Ident`, so the digits are
        // just reused as its text, same as any other field name.
        let field = if let Some(token) = input.first().filter(|t| t.kind == TokenKind::Number) {
            let ident = Ident::new(token.lexeme.clone());
            *input = &input[1..];
            ident
        } else {
            ident_like(input)?
        };
        let value = if skip_symbol(input, ":").is_ok() {
            Some(parse_expr_winnow(input, file)?)
        } else {
            None
        };
        fields.push(ExprField {
            span: Span::null(),
            name: field,
            value,
        });
        let mut comma_probe = *input;
        if skip_symbol(&mut comma_probe, ",").is_ok() {
            *input = comma_probe;
            if peek_symbol(*input) == Some("}") {
                break;
            }
        } else {
            break;
        }
    }
    skip_symbol(input, "}")?;
    Ok((fields, update))
}

fn skip_outer_attrs_in_struct_literal(input: &mut &[Token]) -> ModalResult<()> {
    loop {
        let mut probe = *input;
        if skip_symbol(&mut probe, "#").is_err() {
            return Ok(());
        }
        skip_symbol(&mut probe, "[")?;
        let mut depth = 1usize;
        while let Some((token, rest)) = probe.split_first() {
            probe = rest;
            if token.kind == TokenKind::Symbol {
                match token.lexeme.as_str() {
                    "[" => depth += 1,
                    "]" => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
        if depth != 0 {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        *input = probe;
    }
}

fn parse_call_args(
    input: &mut &[Token],
    file: FileId,
    terminator: &str,
) -> ModalResult<(Vec<Expr>, Vec<ExprKwArg>)> {
    let mut args = Vec::new();
    let mut kwargs = Vec::new();
    let mut saw_kwarg = false;
    if peek_symbol(input) == Some(terminator) {
        return Ok((args, kwargs));
    }

    loop {
        // A call argument may carry its own attribute (real
        // `std::thread::current`'s own `get().unwrap_or_else(\n #[cold]
        // \n || { .. })`) — carries no meaning this checker models for an
        // arbitrary expression, so it's skipped before parsing the
        // argument itself, same as attributes are already dropped before
        // match arms/struct fields/etc.
        skip_outer_attrs_before_expr(input, file)?;
        let mut probe = *input;
        if let Ok(name) = parse_kwarg_name(&mut probe) {
            if skip_symbol(&mut probe, "=").is_ok() {
                let value = parse_expr_or_type_value(&mut probe, file, terminator)?;
                *input = probe;
                if kwargs.iter().any(|existing| existing.name == name) {
                    return Err(ErrMode::Cut(ContextError::new()));
                }
                kwargs.push(ExprKwArg { name, value });
                saw_kwarg = true;
            } else {
                if saw_kwarg {
                    return Err(ErrMode::Cut(ContextError::new()));
                }
                let expr = parse_expr_or_type_value(input, file, terminator)?;
                args.push(expr);
            }
        } else {
            if saw_kwarg {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            let expr = parse_expr_or_type_value(input, file, terminator)?;
            args.push(expr);
        }

        let mut comma_probe = *input;
        if skip_symbol(&mut comma_probe, ",").is_err() {
            break;
        }
        *input = comma_probe;
        if peek_symbol(input) == Some(terminator) {
            break;
        }
    }

    Ok((args, kwargs))
}

fn parse_expr_or_type_value(
    input: &mut &[Token],
    file: FileId,
    terminator: &str,
) -> ModalResult<Expr> {
    let mut probe = *input;
    if let Ok(expr) = parse_expr_winnow(&mut probe, file) {
        let next = peek_symbol(probe);
        if next == Some(",") || next == Some(terminator) {
            *input = probe;
            return Ok(expr);
        }
    }

    let ty = parse_type_expr(input)?;
    Ok(Expr::value(Value::Type(ty)))
}

fn parse_kwarg_name(input: &mut &[Token]) -> ModalResult<String> {
    let ident = ident_like(input)?;
    Ok(ident.name)
}

pub(crate) fn parse_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    Ok(ExprKind::Block(parse_block(input, file)?).into())
}

pub(crate) fn parse_block(input: &mut &[Token], file: FileId) -> ModalResult<ExprBlock> {
    skip_symbol(input, "{")?;
    let mut stmts = Vec::new();
    while peek_symbol(input) != Some("}") {
        // An `extern`/`unsafe extern "ABI" { .. }` block (real
        // `core::panicking`'s local `unsafe extern "Rust" { fn
        // panic_impl(..) -> !; }`, resolving to the `#[panic_handler]`)
        // can appear inside a function body, not just at file scope —
        // same special-case `parse_items_tokens`/`parse_file_tokens`/
        // `parse_script_tokens` already need, since it expands to
        // multiple items, not the single `BlockStmt` `parse_block_stmt_
        // entry`'s signature returns one of.
        if looks_like_extern_block(input) {
            let items = parse_extern_block_items(input, file)?;
            stmts.extend(
                items
                    .into_iter()
                    .map(|item| BlockStmt::Item(Box::new(item))),
            );
            continue;
        }
        if starts_unsafe_extern_block(input) {
            let items = parse_prefixed_unsafe_extern_block_items(input, file)?;
            stmts.extend(
                items
                    .into_iter()
                    .map(|item| BlockStmt::Item(Box::new(item))),
            );
            continue;
        }
        stmts.push(parse_block_stmt_entry(input, file)?);
    }
    skip_symbol(input, "}")?;
    Ok(ExprBlock::new_stmts(stmts))
}

/// Parse one `BlockStmt` entry (item / let / defer / trailing expr), the
/// same dispatch `parse_block_expr` uses per loop iteration. Shared with
/// `parse_script_tokens` (top-level `ScriptBlock` parsing), which loops this
/// same dispatch until end-of-input instead of a closing `}`. A missing
/// trailing semicolon is only tolerated when this is the last entry before
/// the block's `}` *or* before end-of-input — both are "nothing more to
/// parse here" in their respective contexts.
pub(crate) fn parse_block_stmt_entry(input: &mut &[Token], file: FileId) -> ModalResult<BlockStmt> {
    if peek_symbol(*input) == Some("#") {
        let mut attr_probe = *input;
        let attrs = crate::ast::items::parse_outer_attrs(&mut attr_probe, file)?;
        if !attrs.is_empty() {
            let mut item_probe = *input;
            if let Ok(item) = parse_block_item(&mut item_probe, file) {
                *input = item_probe;
                return Ok(BlockStmt::Item(Box::new(item)));
            }
            *input = attr_probe;
        }
    }
    if starts_block_item(*input) {
        let mut item_probe = *input;
        if let Ok(item) = parse_block_item(&mut item_probe, file) {
            *input = item_probe;
            return Ok(BlockStmt::Item(Box::new(item)));
        }
    }
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Let).is_ok() {
        let mut pat = if skip_keyword(&mut probe, Keyword::Mut).is_ok() {
            let name = ident_like(&mut probe)?;
            Pattern::new(PatternKind::Ident(PatternIdent {
                ident: name,
                mutability: Some(true),
            }))
        } else {
            parse_general_pattern(&mut probe)?
        };
        if skip_symbol(&mut probe, ":").is_ok() {
            let ty = parse_simple_type(&mut probe)?;
            pat = Pattern::new(PatternKind::Type(PatternType::new(pat, ty)));
        }
        if skip_symbol(&mut probe, "=").is_err() {
            if skip_symbol(&mut probe, ";").is_ok() {
                *input = probe;
                return Ok(BlockStmt::Let(StmtLet::new(pat, None, None)));
            }
            return Err(ErrMode::Cut(ContextError::new()));
        }
        let init = parse_expr_winnow(&mut probe, file)?;
        let diverge = if skip_keyword(&mut probe, Keyword::Else).is_ok() {
            Some(parse_block_expr(&mut probe, file)?)
        } else {
            None
        };
        let had_semi = skip_symbol(&mut probe, ";").is_ok();
        if !had_semi {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        *input = probe;
        return Ok(BlockStmt::Let(StmtLet::new(pat, Some(init), diverge)));
    }
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Defer).is_ok() {
        let expr = parse_expr_winnow(&mut probe, file)?;
        let had_semi = skip_symbol(&mut probe, ";").is_ok();
        if !had_semi {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        *input = probe;
        return Ok(BlockStmt::Defer(StmtDefer {
            span: span_from_expr(&expr),
            expr: Box::new(expr),
        }));
    }

    // A statement that *starts* with a block-like expression (`if`, `match`,
    // `loop`, `while`, `for`, `unsafe { }`, a bare block, ...) is a complete
    // statement on its own — same rule as real Rust. Parsing it through the
    // general postfix-continuing expression parser would let a following
    // `(...)` or `[...]` get misread as a call/index on it (e.g. `if c {}
    // (a, b)` — two statements — would otherwise become one `Invoke` whose
    // "callee" is the if-expression), silently swallowing the next
    // statement into bogus call arguments.
    let expr = if starts_block_like_stmt_expr(*input) {
        let block_expr = parse_primary(input, file)?;
        // Unlike a bare `(`/`[` immediately following (ambiguous with a new
        // statement, per the comment above), a leading `.` or `?` right
        // after a block-like statement expression is never ambiguous —
        // nothing else can start a new statement with `.`/`?` — so real
        // Rust still allows `unsafe { x() }.method()?` here (a common idiom
        // for a block-like tail expression), and once postfix parsing has
        // consumed that leading `.`/`?`, any `(...)`/`[...]` after it
        // unambiguously belongs to the method call/index, not to a new
        // statement, so the full postfix grammar (including calls/indexing)
        // is safe to resume from there.
        if matches!(peek_symbol(input), Some(".") | Some("?")) {
            let suffixes: Vec<Postfix> = repeat(0.., |input: &mut &[Token]| {
                parse_postfix_suffix(input, file)
            })
            .parse_next(input)?;
            let postfixed = apply_postfixes(block_expr, suffixes);
            // The postfix chain may itself be an assignment target (real
            // `std::sys::pal::sgx::waitqueue::unsafe_list`'s own `unsafe {
            // self.head_tail.as_mut() }.next = self.head_tail;`) — give it
            // the same chance a normal expression-statement's LHS gets to
            // continue into a full assignment, or the trailing `=` is left
            // unconsumed and mistaken for a missing statement terminator.
            parse_assignment_tail(input, file, postfixed, parse_expr_winnow)?
        } else {
            block_expr
        }
    } else {
        parse_expr_winnow(input, file)?
    };
    let mut semicolon = false;
    let mut probe = *input;
    if skip_symbol(&mut probe, ";").is_ok() {
        *input = probe;
        semicolon = true;
    } else if !expr_can_omit_semicolon_in_block(&expr)
        && peek_symbol(input) != Some("}")
        && !input.is_empty()
    {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    Ok(BlockStmt::Expr(
        BlockStmtExpr::new(expr).with_semicolon(semicolon),
    ))
}

/// See `parse_block_stmt_entry`'s use of this: a leading token that starts a
/// block-like expression means the *whole* statement is just that
/// expression, parsed via `parse_primary` (no postfix/binary continuation),
/// mirroring real Rust's statement-boundary rule for `ExpressionWithBlock`.
fn starts_block_like_stmt_expr(input: &[Token]) -> bool {
    match input {
        [first, ..] if first.lexeme == "{" => true,
        [first, second, ..]
            if matches!(
                first.kind,
                TokenKind::Keyword(Keyword::Const | Keyword::Async)
            ) && second.lexeme == "{" =>
        {
            true
        }
        [first, ..] => matches!(
            first.kind,
            TokenKind::Keyword(
                Keyword::If
                    | Keyword::Match
                    | Keyword::Loop
                    | Keyword::While
                    | Keyword::For
                    | Keyword::Unsafe
                    | Keyword::With
            )
        ),
        [] => false,
    }
}

fn expr_can_omit_semicolon_in_block(expr: &Expr) -> bool {
    matches!(
        expr.kind(),
        ExprKind::Block(_)
            | ExprKind::If(_)
            | ExprKind::Loop(_)
            | ExprKind::While(_)
            | ExprKind::For(_)
            | ExprKind::With(_)
            | ExprKind::Async(_)
            | ExprKind::ConstBlock(_)
            | ExprKind::Match(_)
            | ExprKind::Try(_)
            | ExprKind::Macro(_)
    )
}

fn starts_block_item(input: &[Token]) -> bool {
    match input {
        [first, ..] if first.lexeme == "#" => true,
        // `unsafe fn`/`unsafe extern "ABI" fn`/`unsafe impl` as a
        // function-local item statement (real
        // `std::sys::fs::windows::copy`'s own local `unsafe extern "system"
        // fn callback(..) { .. }`) — distinguished from an `unsafe { .. }`
        // *expression* (a normal statement, not a local item) by whether
        // the modifier run actually leads to `fn`/`impl`.
        [first, ..]
            if first.kind == TokenKind::Keyword(Keyword::Unsafe)
                && (super::skips_modifiers_to_fn(input)
                    || crate::ast::items::starts_unsafe_impl(input)) =>
        {
            true
        }
        [first, second, ..] if first.kind == TokenKind::Keyword(Keyword::Const) => {
            matches!(second.kind, TokenKind::Ident | TokenKind::Keyword(_))
        }
        [first, ..]
            if matches!(
                first.kind,
                TokenKind::Keyword(
                    Keyword::Use
                        | Keyword::Extern
                        | Keyword::Const
                        | Keyword::Static
                        | Keyword::Type
                        | Keyword::Struct
                        | Keyword::Union
                        | Keyword::Enum
                        | Keyword::Mod
                        | Keyword::Trait
                        | Keyword::Impl
                        | Keyword::Fn
                        | Keyword::Async
                        | Keyword::Quote
                )
            ) =>
        {
            true
        }
        // A local `macro_rules! name { .. }` definition as a
        // function-local statement (real `core::str::validations`'s own
        // `while .. { macro_rules! err { .. } .. }`). Deliberately
        // narrower than "any item-position macro invocation" — an
        // ordinary macro *call* used as part of a larger expression
        // (`vec![1, 2, 3].len()`, `some_macro!(x)?`) must still fall
        // through to expression-statement parsing so its trailing
        // `.method()`/`?`/etc. aren't left as orphaned tokens.
        [first, second, ..]
            if first.kind == TokenKind::Ident
                && first.lexeme == "macro_rules"
                && second.kind == TokenKind::Symbol
                && second.lexeme == "!" =>
        {
            true
        }
        // A local `macro name(..) { .. }` "macro 2.0" definition as a
        // function-local statement (real `std::ffi::os_str`'s own `fn
        // push(..) { .. macro spec_str($T:ty) { .. } .. }`) — same
        // narrower-than-any-macro-call reasoning as `macro_rules!` above.
        _ if crate::ast::items::starts_macro_2_def(input) => true,
        _ => false,
    }
}

fn parse_block_item(input: &mut &[Token], file: FileId) -> ModalResult<Item> {
    let start = *input;
    let (item, consumed) =
        parse_item_prefix_tokens(start, file).map_err(|_| ErrMode::Cut(ContextError::new()))?;
    *input = &start[consumed..];
    Ok(item)
}

fn parse_closure_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    // `const || ..`/`const move || ..` (nightly const closures) — `const`
    // carries no meaning this checker models for a closure (no notion of
    // restricting it to a const-evaluable body beyond what's already
    // checked), so it's dropped like the other no-op safety/const
    // modifiers elsewhere.
    let _is_const = skip_keyword(&mut probe, Keyword::Const).is_ok();
    let movability = if skip_keyword(&mut probe, Keyword::Move).is_ok() {
        Some(true)
    } else {
        None
    };
    if skip_symbol(&mut probe, "||").is_err() {
        if skip_symbol(&mut probe, "|").is_err() {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        let mut params = Vec::new();
        if peek_symbol(probe) != Some("|") {
            loop {
                params.push(parse_closure_param(&mut probe)?);
                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
            }
        }
        skip_symbol(&mut probe, "|")?;
        let ret_ty = if skip_symbol(&mut probe, "->").is_ok() {
            Some(Box::new(parse_type_expr(&mut probe)?))
        } else {
            None
        };
        let body = if peek_symbol(probe) == Some("{") {
            parse_block_expr(&mut probe, file)?
        } else {
            parse_expr_winnow(&mut probe, file)?
        };
        *input = probe;
        return Ok(ExprKind::Closure(ExprClosure {
            span: body.span(),
            params,
            ret_ty,
            movability,
            body: Box::new(body),
        })
        .into());
    }
    let params = Vec::new();
    let ret_ty = if skip_symbol(&mut probe, "->").is_ok() {
        Some(Box::new(parse_type_expr(&mut probe)?))
    } else {
        None
    };
    let body = parse_expr_winnow(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::Closure(ExprClosure {
        span: body.span(),
        params,
        ret_ty,
        movability,
        body: Box::new(body),
    })
    .into())
}

fn parse_closure_param(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut pat = parse_general_pattern(input)?;
    if let PatternKind::TupleStruct(tuple) = pat.kind() {
        if tuple.patterns.len() == 1 && tuple.patterns[0].as_ident().is_some() {
            pat = tuple.patterns[0].clone();
        }
    }
    let mut probe = *input;
    if skip_symbol(&mut probe, ":").is_ok() {
        let ty = parse_closure_param_type(&mut probe)?;
        *input = probe;
        pat = Pattern::new(PatternKind::Type(PatternType::new(pat, ty)));
    }
    Ok(pat)
}

fn parse_closure_param_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut angle_depth = 0usize;
    let mut consumed = 0usize;
    for (idx, token) in input.iter().enumerate() {
        if token.kind == TokenKind::Symbol {
            match token.lexeme.as_str() {
                "(" => paren_depth += 1,
                ")" => paren_depth = paren_depth.saturating_sub(1),
                "[" => bracket_depth += 1,
                "]" => bracket_depth = bracket_depth.saturating_sub(1),
                "{" => brace_depth += 1,
                "}" => brace_depth = brace_depth.saturating_sub(1),
                "<" => angle_depth += 1,
                ">" => angle_depth = angle_depth.saturating_sub(1),
                ">>" => angle_depth = angle_depth.saturating_sub(2),
                "|" | ","
                    if paren_depth == 0
                        && bracket_depth == 0
                        && brace_depth == 0
                        && angle_depth == 0 =>
                {
                    break;
                }
                _ => {}
            }
        }
        consumed = idx + 1;
    }
    if consumed == 0 {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    let ty = crate::ast::parse_type_tokens(&input[..consumed], 0)
        .map_err(|_| ErrMode::Cut(ContextError::new()))?;
    *input = &input[consumed..];
    Ok(ty)
}

fn parse_quote_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Quote).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut kind = None;
    if skip_symbol(&mut probe, "<").is_ok() {
        if skip_symbol(&mut probe, "[").is_ok() {
            let ident = ident_like(&mut probe)?;
            if ident.as_str() != "item" {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            skip_symbol(&mut probe, "]")?;
            kind = Some(QuoteFragmentKind::Item);
        } else {
            let ident = ident_like(&mut probe)?;
            kind = Some(match ident.as_str() {
                "expr" => QuoteFragmentKind::Expr,
                "stmt" => QuoteFragmentKind::Stmt,
                "item" | "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static"
                | "mod" | "use" | "macro" => QuoteFragmentKind::Item,
                "type" => QuoteFragmentKind::Type,
                _ => return Err(ErrMode::Cut(ContextError::new())),
            });
        }
        skip_symbol(&mut probe, ">")?;
    } else if let Ok(ident) = ident_like(&mut probe) {
        if ident.as_str() == "item"
            || ident.as_str() == "expr"
            || ident.as_str() == "stmt"
            || ident.as_str() == "type"
        {
            kind = Some(match ident.as_str() {
                "expr" => QuoteFragmentKind::Expr,
                "stmt" => QuoteFragmentKind::Stmt,
                "type" => QuoteFragmentKind::Type,
                _ => QuoteFragmentKind::Item,
            });
        } else {
            probe = *input;
            skip_keyword(&mut probe, Keyword::Quote)?;
        }
    }
    let block = if matches!(kind, Some(QuoteFragmentKind::Item)) {
        parse_balanced_quote_block(&mut probe, file)?
    } else {
        let body = parse_block_expr(&mut probe, file)?;
        let ExprKind::Block(block) = body.kind().clone() else {
            return Err(ErrMode::Cut(ContextError::new()));
        };
        block
    };
    *input = probe;
    Ok(ExprKind::Quote(ExprQuote {
        span: block.span,
        block,
        collected_items: Vec::new(),
        kind,
    })
    .into())
}

pub(crate) fn parse_balanced_quote_block(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<ExprBlock> {
    skip_symbol(input, "{")?;
    let mut depth = 1usize;
    let start = *input;
    let mut token_count = 0usize;
    while let Some((token, rest)) = input.split_first() {
        *input = rest;
        token_count += 1;
        if token.kind != TokenKind::Symbol {
            continue;
        }
        match token.lexeme.as_str() {
            "{" => depth += 1,
            "}" => {
                depth -= 1;
                if depth == 0 {
                    let inner = &start[..token_count - 1];
                    if inner.is_empty() {
                        return Ok(ExprBlock::new());
                    }
                    match crate::ast::parse_items_tokens(inner, file) {
                        Ok(items) => {
                            let mut block = ExprBlock::new();
                            for item in items {
                                block.stmts.push(BlockStmt::Item(Box::new(item)));
                            }
                            return Ok(block);
                        }
                        Err(_) => return Ok(ExprBlock::new()),
                    }
                }
            }
            _ => {}
        }
    }
    Err(ErrMode::Cut(ContextError::new()))
}

fn parse_try_structured(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Try).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let expr = parse_block_expr(&mut probe, file)?;
    let mut catches = Vec::new();
    loop {
        let mut clause_probe = probe;
        if skip_keyword(&mut clause_probe, Keyword::Catch).is_err() {
            break;
        }
        let (pat, body) = parse_catch_pattern_and_body(&mut clause_probe, file)?;
        catches.push(ExprTryCatch {
            span: union_spans(pat.span(), body.span()),
            pat: Some(Box::new(pat)),
            body: Box::new(body),
        });
        probe = clause_probe;
    }

    let mut elze = None;
    let mut else_probe = probe;
    if skip_keyword(&mut else_probe, Keyword::Else).is_ok() {
        let body = parse_block_expr(&mut else_probe, file)?;
        elze = Some(Box::new(body));
        probe = else_probe;
    }

    let mut finally = None;
    let mut finally_probe = probe;
    if skip_keyword(&mut finally_probe, Keyword::Finally).is_ok() {
        let body = parse_block_expr(&mut finally_probe, file)?;
        finally = Some(Box::new(body));
        probe = finally_probe;
    }

    *input = probe;
    Ok(ExprKind::Try(ExprTry {
        span: span_from_expr(&expr),
        expr: Box::new(expr),
        catches,
        elze,
        finally,
    })
    .into())
}

fn parse_catch_pattern_and_body(
    input: &mut &[Token],
    file: FileId,
) -> ModalResult<(Pattern, Expr)> {
    let original = *input;
    let mut best: Option<(Pattern, usize)> = None;
    for idx in 0..original.len() {
        let Some(token) = original.get(idx) else {
            break;
        };
        if token.kind != TokenKind::Symbol || token.lexeme != "{" {
            continue;
        }
        let Ok((pat, consumed)) = parse_pattern_prefix_tokens(&original[..idx]) else {
            continue;
        };
        if consumed == idx {
            best = Some((pat, idx));
        }
    }
    let Some((pat, consumed)) = best else {
        return Err(ErrMode::Cut(ContextError::new()));
    };
    let mut body_input = &original[consumed..];
    let body = parse_block_expr(&mut body_input, file)?;
    *input = body_input;
    Ok((pat, body))
}

pub(crate) fn parse_pattern_prefix_tokens(
    tokens: &[Token],
) -> Result<(Pattern, usize), DirectParseError> {
    let mut input = tokens;
    let pat = parse_general_pattern(&mut input).map_err(|err| map_err(err, input))?;
    let consumed = tokens.len() - input.len();
    Ok((pat, consumed))
}

fn parse_match_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Match).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let scrutinee_start = probe;
    // A bare scrutinee that's just a real-Rust identifier colliding with
    // this checker's own `quote`/`splice` metaprogramming keywords (real
    // `std::sys::args::windows`'s own `match quote { .. }`, naming a
    // local `quote: Quote` variable) gets greedily swallowed whole by
    // `parse_quote_expr`'s bare-`quote { .. }` fallback, which either
    // consumes the match's own arm-list brace as its quote block's body
    // (when that body happens to parse as a valid block on its own) or
    // hard-errors trying to (when it doesn't, e.g. `1 => 10` isn't a
    // valid block statement) — either way leaving nothing usable here.
    // `if let`'s scrutinee parsing already has this exact retry (see its
    // own `parse_keyword_name_expr_no_struct` fallback, used the same
    // way below); reuse the same recovery here, on both the hard-error
    // and the leftover-tokens outcome.
    let scrutinee = match parse_expr_winnow_no_struct(&mut probe, file) {
        Ok(scrutinee) if peek_symbol(probe) == Some("{") => scrutinee,
        _ => {
            probe = scrutinee_start;
            parse_keyword_name_expr_no_struct(&mut probe, file)?
        }
    };
    skip_symbol(&mut probe, "{")?;
    let mut cases = Vec::new();
    while peek_symbol(probe) != Some("}") {
        let mut arm_probe = probe;
        skip_outer_attrs_before_match_arm(&mut arm_probe, file)?;
        let mut patterns = vec![parse_general_pattern(&mut arm_probe)?];
        while skip_symbol(&mut arm_probe, "|").is_ok() {
            patterns.push(parse_general_pattern(&mut arm_probe)?);
        }
        let mut guard = None;
        let mut guard_probe = arm_probe;
        if skip_keyword(&mut guard_probe, Keyword::If).is_ok() {
            let guard_expr = parse_expr_winnow(&mut guard_probe, file)?;
            guard = Some(Box::new(guard_expr));
            arm_probe = guard_probe;
        }
        skip_symbol(&mut arm_probe, "=>")?;
        let body = if peek_symbol(arm_probe) == Some("{") {
            parse_block_expr(&mut arm_probe, file)?
        } else {
            parse_expr_winnow(&mut arm_probe, file)?
        };
        let mut comma_probe = arm_probe;
        if skip_symbol(&mut comma_probe, ",").is_ok() {
            arm_probe = comma_probe;
        }
        probe = arm_probe;
        // `A | B => body` (top-level or nested, e.g. `(A | B, C)`)
        // desugars into one `ExprMatchCase` per alternative in the
        // cartesian expansion, all sharing the same guard/body — mirrors
        // `build_if_let_match`'s handling of `if let A | B = x {...}`.
        let full_pattern = if patterns.len() == 1 {
            patterns.into_iter().next().unwrap()
        } else {
            Pattern::new(PatternKind::Or(PatternOr { patterns }))
        };
        for pat in expand_pattern_alternatives(&full_pattern) {
            cases.push(fp_core::ast::ExprMatchCase {
                span: union_spans(pat.span(), body.span()),
                pat: Some(Box::new(pat)),
                cond: Box::new(Expr::value(Value::bool(true))),
                guard: guard.clone(),
                body: Box::new(body.clone()),
            });
        }
    }
    skip_symbol(&mut probe, "}")?;
    *input = probe;
    Ok(ExprKind::Match(fp_core::ast::ExprMatch {
        span: span_from_expr(&scrutinee),
        scrutinee: Some(Box::new(scrutinee)),
        cases,
    })
    .into())
}

fn skip_outer_attrs_before_match_arm(input: &mut &[Token], file: FileId) -> ModalResult<()> {
    skip_outer_attrs_before_expr(input, file)
}

fn skip_outer_attrs_before_expr(input: &mut &[Token], file: FileId) -> ModalResult<()> {
    loop {
        let mut probe = *input;
        let attrs = crate::ast::items::parse_outer_attrs(&mut probe, file)?;
        if attrs.is_empty() {
            return Ok(());
        }
        *input = probe;
    }
}
