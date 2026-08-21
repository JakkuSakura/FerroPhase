use super::*;
use fp_core::ast::ExprLet;
use fp_core::ast::PatternBind;
use fp_core::ast::PatternRef;
use fp_core::ast::path::PathPrefix;
use winnow::Parser;

pub(crate) fn parse_expr_winnow(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_assignment(input, file)
}

pub(crate) fn parse_expr_winnow_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_assignment_no_struct(input, file)
}

fn parse_assignment(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let lhs = parse_range(input, file)?;
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
    let rhs = parse_assignment(input, file)?;
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

fn parse_cast_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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
                collected_items: Vec::new(),
                block,
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

    if let Some(op) = peek_symbol(input) {
        if matches!(op, "!" | "-" | "*" | "&") {
            let op = op.to_string();
            skip_symbol(input, &op)?;
            let is_mut_ref = op == "&" && skip_keyword(input, Keyword::Mut).is_ok();
            let value = parse_prefix(input, file)?;
            if op == "&" {
                return Ok(ExprKind::Reference(ExprReference {
                    span: span_from_expr(&value),
                    referee: Box::new(value),
                    mutable: is_mut_ref.then_some(true),
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
                collected_items: Vec::new(),
                block,
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

    if let Some(op) = peek_symbol(input) {
        if matches!(op, "!" | "-" | "*" | "&") {
            let op = op.to_string();
            skip_symbol(input, &op)?;
            let is_mut_ref = op == "&" && skip_keyword(input, Keyword::Mut).is_ok();
            let value = parse_prefix_no_struct(input, file)?;
            if op == "&" {
                return Ok(ExprKind::Reference(ExprReference {
                    span: span_from_expr(&value),
                    referee: Box::new(value),
                    mutable: is_mut_ref.then_some(true),
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
    Ok(Postfix::Field(field))
}

fn parse_turbofish_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    let mut probe = *input;
    if skip_symbol(&mut probe, "::").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    if skip_symbol(&mut probe, "<").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut depth = 1usize;
    while let Some((token, rest)) = probe.split_first() {
        probe = rest;
        if token.kind != TokenKind::Symbol {
            continue;
        }
        match token.lexeme.as_str() {
            "<" => depth += 1,
            ">" => {
                depth -= 1;
                if depth == 0 {
                    *input = probe;
                    return Ok(Postfix::Turbofish);
                }
            }
            _ => {}
        }
    }
    Err(ErrMode::Cut(ContextError::new()))
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
            parse_name_expr,
        )),
    ))
    .parse_next(input)
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
    // TODO(ty-removal): parse-time numeric-suffix type (`ty`) no longer
    // has anywhere to attach to on `Expr` directly — the removed
    // `Expr.ty` cache field. The real typechecker (`fp-typing`) re-derives
    // this literal's type independently during HIR typecheck, and that
    // resolved type is what `HirToAstLifter` records into the
    // `resolved_expr_types` side-table backends actually read from, so
    // dropping this parse-time attachment should be behavior-preserving
    // for every typechecked pipeline. Left as a TODO in case some
    // never-typechecked path (e.g. a raw-AST-only tool) relied on it.
    let _ = ty;
    Ok(Expr::value(value).with_span(token_span_to_span(&token)))
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
            return Ok(Expr::value(Value::Char(ValueChar::new(ch)))
                .with_span(token_span_to_span(&token)));
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
        // Unlike the other literal kinds in this function, this parse-time
        // type genuinely needs to survive to AST->HIR lowering
        // (`ast_to_hir::exprs::transform_bytes_value_to_hir`, which reads it
        // back to distinguish `b"..."` from `c"..."` — `ValueBytes` itself
        // carries no such flag) with no annotation-shaped AST position to
        // hold it, so it's recorded in the resolved-expr-type side-table
        // (`fp_core::ast::set_resolved_expr_type`) keyed by this node's own
        // freshly-assigned id.
        let node = Expr::value(Value::Bytes(ValueBytes::from(bytes.as_slice())))
            .with_span(token_span_to_span(&token));
        fp_core::ast::set_resolved_expr_type(node.id(), ty);
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
    Ok(Expr::value(Value::string(value))
        .with_span(token_span_to_span(&token)))
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
    Turbofish,
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
            Postfix::Field(field) => ExprKind::Select(ExprSelect {
                span: span_from_expr(&expr),
                obj: Box::new(expr),
                field,
                select: ExprSelectType::Field,
            })
            .into(),
            Postfix::Turbofish => expr,
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
        ExprKind::Select(select) => expr_can_start_struct_literal(&select.obj),
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
        let field = ident_like(input)?;
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
            stmts.extend(items.into_iter().map(|item| BlockStmt::Item(Box::new(item))));
            continue;
        }
        if starts_unsafe_extern_block(input) {
            let items = parse_prefixed_unsafe_extern_block_items(input, file)?;
            stmts.extend(items.into_iter().map(|item| BlockStmt::Item(Box::new(item))));
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
            apply_postfixes(block_expr, suffixes)
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
            if matches!(first.kind, TokenKind::Keyword(Keyword::Const | Keyword::Async))
                && second.lexeme == "{" =>
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
        collected_items: Vec::new(),
        block,
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

fn parse_macro_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut probe = *input;
    let path = parse_macro_path(&mut probe)?;
    skip_symbol(&mut probe, "!")?;
    let (delimiter, group_span, token_trees, text) = parse_macro_group(&mut probe)?;
    *input = probe;
    Ok(ExprKind::Macro(ExprMacro::new(
        MacroInvocation::new(path, delimiter, text)
            .with_token_trees(token_trees)
            .with_span(group_span),
    ))
    .into())
}

fn parse_struct_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    let is_structural = skip_keyword(&mut probe, Keyword::Struct).is_ok();
    let name = if is_structural {
        None
    } else {
        Some(parse_name(&mut probe)?)
    };
    if skip_symbol(&mut probe, "{").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let (fields, update) = parse_struct_literal_fields(&mut probe, file)?;
    *input = probe;
    if is_structural {
        return Ok(ExprKind::Structural(ExprStructural {
            span: Span::null(),
            fields,
        })
        .into());
    }
    Ok(ExprKind::Struct(ExprStruct {
        span: Span::null(),
        name: Box::new(Expr::name(name.expect("named struct literal"))),
        fields,
        update,
    })
    .into())
}

pub(crate) fn parse_macro_path(input: &mut &[Token]) -> ModalResult<Path> {
    parse_module_path(input)
}

pub(crate) fn parse_macro_group(
    input: &mut &[Token],
) -> ModalResult<(MacroDelimiter, Span, Vec<MacroTokenTree>, String)> {
    let (delimiter, open, close) = match peek_symbol(input) {
        Some("(") => (MacroDelimiter::Parenthesis, "(", ")"),
        Some("[") => (MacroDelimiter::Bracket, "[", "]"),
        Some("{") => (MacroDelimiter::Brace, "{", "}"),
        _ => return Err(ErrMode::Backtrack(ContextError::new())),
    };
    let open_token = expect_symbol(input, open)?;
    let mut inner = Vec::new();
    loop {
        if peek_symbol(input) == Some(close) {
            break;
        }
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        inner.push(parse_macro_token_tree(input)?);
    }
    let close_token = expect_symbol(input, close)?;
    let span = Span::union([
        token_span_to_span(&open_token),
        token_span_to_span(&close_token),
    ]);
    let text = macro_token_trees_to_text(&inner);
    Ok((delimiter, span, inner, text))
}

fn parse_macro_token_tree(input: &mut &[Token]) -> ModalResult<MacroTokenTree> {
    if matches!(peek_symbol(input), Some("(") | Some("[") | Some("{")) {
        let (delimiter, span, token_trees, _) = parse_macro_group(input)?;
        return Ok(MacroTokenTree::Group(MacroGroup {
            delimiter,
            tokens: token_trees,
            span,
        }));
    }
    let Some((token, rest)) = input.split_first() else {
        return Err(ErrMode::Backtrack(ContextError::new()));
    };
    *input = rest;
    Ok(MacroTokenTree::Token(MacroToken {
        text: token.lexeme.clone(),
        span: token_span_to_span(token),
    }))
}

fn macro_token_trees_to_text(tokens: &[MacroTokenTree]) -> String {
    fn is_ident_like(text: &str) -> bool {
        text.chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
    }

    fn needs_space(prev: &str, next: &str) -> bool {
        is_ident_like(prev) && is_ident_like(next)
    }

    let mut out = String::new();
    let mut prev: Option<String> = None;
    for token in flatten_macro_tokens(tokens) {
        if let Some(prev_text) = prev.as_deref() {
            if needs_space(prev_text, token.as_str()) {
                out.push(' ');
            }
        }
        out.push_str(&token);
        prev = Some(token);
    }
    out
}

fn flatten_macro_tokens(tokens: &[MacroTokenTree]) -> Vec<String> {
    let mut out = Vec::new();
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    MacroDelimiter::Parenthesis => ("(", ")"),
                    MacroDelimiter::Bracket => ("[", "]"),
                    MacroDelimiter::Brace => ("{", "}"),
                };
                out.push(open.to_string());
                out.extend(flatten_macro_tokens(&group.tokens));
                out.push(close.to_string());
            }
        }
    }
    out
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
    let scrutinee = parse_expr_winnow_no_struct(&mut probe, file)?;
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

fn parse_match_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut probe = *input;
    if skip_symbol(&mut probe, "&").is_ok() {
        let mutability = skip_keyword(&mut probe, Keyword::Mut).is_ok();
        let pattern = parse_general_pattern(&mut probe)?;
        *input = probe;
        return Ok(Pattern::new(PatternKind::Ref(PatternRef {
            mutability: mutability.then_some(true),
            pattern: Box::new(pattern),
        })));
    }
    if skip_keyword(&mut probe, Keyword::Mut).is_ok() {
        let mut pat = parse_match_pattern(&mut probe)?;
        pat.make_mut();
        *input = probe;
        return Ok(pat);
    }
    if peek_ident_like(probe) == Some("ref") {
        let _ = ident_like(&mut probe)?;
        if starts_ref_pattern_target(probe) {
            let pattern = parse_general_pattern(&mut probe)?;
            *input = probe;
            return Ok(Pattern::new(PatternKind::Ref(PatternRef {
                mutability: None,
                pattern: Box::new(pattern),
            })));
        }
    }
    if skip_keyword(&mut probe, Keyword::Quote).is_ok() {
        let mut item = None;
        let mut fragment = QuoteFragmentKind::Item;
        if skip_symbol(&mut probe, "<").is_ok() {
            let ident = ident_like(&mut probe)?;
            item = match ident.as_str() {
                "fn" => Some(QuoteItemKind::Function),
                "struct" => Some(QuoteItemKind::Struct),
                "enum" => Some(QuoteItemKind::Enum),
                "trait" => Some(QuoteItemKind::Trait),
                "impl" => Some(QuoteItemKind::Impl),
                "const" => Some(QuoteItemKind::Const),
                "static" => Some(QuoteItemKind::Static),
                "mod" => Some(QuoteItemKind::Module),
                "use" => Some(QuoteItemKind::Use),
                "macro" => Some(QuoteItemKind::Macro),
                "item" => None,
                "expr" => {
                    fragment = QuoteFragmentKind::Expr;
                    None
                }
                "stmt" => {
                    fragment = QuoteFragmentKind::Stmt;
                    None
                }
                "type" => {
                    fragment = QuoteFragmentKind::Type;
                    None
                }
                _ => return Err(ErrMode::Cut(ContextError::new())),
            };
            skip_symbol(&mut probe, ">")?;
        }
        *input = probe;
        return Ok(Pattern::new(PatternKind::Quote(PatternQuote {
            fragment,
            item,
            fields: Vec::new(),
            has_rest: false,
        })));
    }

    let mut literal_probe = *input;
    if let Ok(expr) = parse_literal_pattern_expr(&mut literal_probe) {
        let mut range_probe = literal_probe;
        if let Some(op) = peek_symbol(range_probe) {
            let limit = match op {
                ".." => Some(ExprRangeLimit::Exclusive),
                "..=" => Some(ExprRangeLimit::Inclusive),
                _ => None,
            };
            if let Some(limit) = limit {
                skip_symbol(&mut range_probe, op)?;
                let end = parse_literal_pattern_expr(&mut range_probe)
                    .map_err(|_| ErrMode::Cut(ContextError::new()))?;
                let span = union_exprs(&expr, &end);
                *input = range_probe;
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: ExprKind::Range(ExprRange {
                        span,
                        start: Some(Box::new(expr)),
                        limit,
                        end: Some(Box::new(end)),
                        step: None,
                    })
                    .into(),
                    pattern: None,
                })));
            }
        }
        *input = literal_probe;
        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
            name: expr,
            pattern: None,
        })));
    }

    let mut array_probe = *input;
    if skip_symbol(&mut array_probe, "[").is_ok() {
        let mut patterns = Vec::new();
        let mut has_rest = false;
        if peek_symbol(array_probe) != Some("]") {
            loop {
                if skip_symbol(&mut array_probe, "..").is_ok() {
                    has_rest = true;
                    let mut comma_probe = array_probe;
                    if skip_symbol(&mut comma_probe, ",").is_ok() {
                        array_probe = comma_probe;
                        if peek_symbol(array_probe) == Some("]") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(&mut array_probe)?);
                let mut comma_probe = array_probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                array_probe = comma_probe;
                if peek_symbol(array_probe) == Some("]") {
                    break;
                }
            }
        }
        skip_symbol(&mut array_probe, "]")?;
        *input = array_probe;
        if !has_rest {
            let values = patterns
                .iter()
                .map(array_pattern_to_expr)
                .collect::<Option<Vec<_>>>();
            if let Some(values) = values {
                return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                    name: ExprKind::Array(ExprArray {
                        span: Span::null(),
                        values,
                    })
                    .into(),
                    pattern: None,
                })));
            }
        }
        return Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })));
    }

    let name = parse_name(input)?;
    if let Some(ident) = name.as_ident().cloned() {
        let mut bind_probe = *input;
        if skip_symbol(&mut bind_probe, "@").is_ok() {
            let pattern = if skip_symbol(&mut bind_probe, "..").is_ok() {
                Pattern::new(PatternKind::Wildcard(PatternWildcard {}))
            } else {
                parse_general_pattern(&mut bind_probe)?
            };
            *input = bind_probe;
            return Ok(Pattern::new(PatternKind::Bind(PatternBind {
                ident: PatternIdent::new(ident),
                pattern: Box::new(pattern),
            })));
        }
    }
    if let Some(ident) = name.as_ident() {
        if ident.as_str() == "_" {
            return Ok(Pattern::new(PatternKind::Wildcard(PatternWildcard {})));
        }
        if ident.as_str() == "true" || ident.as_str() == "false" {
            return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
                name: Expr::value(Value::bool(ident.as_str() == "true")),
                pattern: None,
            })));
        }
    }

    let mut probe = *input;
    if skip_symbol(&mut probe, "{").is_ok() {
        let mut fields = Vec::new();
        let mut has_rest = false;
        if peek_symbol(probe) != Some("}") {
            loop {
                if skip_symbol(&mut probe, "..").is_ok() {
                    has_rest = true;
                    break;
                }

                let field_rename = if peek_ident_like(probe) == Some("ref")
                    || matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Mut))
                {
                    let saw_ref = if peek_ident_like(probe) == Some("ref") {
                        let _ = ident_like(&mut probe)?;
                        true
                    } else {
                        false
                    };
                    let saw_mut = skip_keyword(&mut probe, Keyword::Mut).is_ok();
                    let field_name = ident_like(&mut probe)?;
                    let mut pattern = Pattern::new(PatternKind::Ident(PatternIdent {
                        ident: field_name.clone(),
                        mutability: saw_mut.then_some(true),
                    }));
                    if saw_ref {
                        pattern = Pattern::new(PatternKind::Ref(PatternRef {
                            mutability: None,
                            pattern: Box::new(pattern),
                        }));
                    }
                    Some((field_name, Some(Box::new(pattern))))
                } else {
                    None
                };
                let (field_name, rename) = if let Some((field_name, rename)) = field_rename {
                    (field_name, rename)
                } else {
                    let field_name = ident_like(&mut probe)?;
                    let rename = if skip_symbol(&mut probe, ":").is_ok() {
                        Some(Box::new(parse_pattern_alternatives(&mut probe)?))
                    } else {
                        None
                    };
                    (field_name, rename)
                };
                fields.push(fp_core::ast::PatternStructField {
                    name: field_name,
                    rename,
                });

                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some("}") {
                    break;
                }
            }
        }
        skip_symbol(&mut probe, "}")?;
        *input = probe;
        let struct_name = name
            .to_path()
            .segments
            .last()
            .cloned()
            .or_else(|| name.as_ident().cloned())
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        return Ok(Pattern::new(PatternKind::Struct(
            fp_core::ast::PatternStruct {
                name: struct_name,
                fields,
                has_rest,
            },
        )));
    }

    let mut probe = *input;
    if skip_symbol(&mut probe, "(").is_ok() {
        let mut patterns = Vec::new();
        if peek_symbol(probe) != Some(")") {
            loop {
                if skip_symbol(&mut probe, "..").is_ok() {
                    let mut comma_probe = probe;
                    if skip_symbol(&mut comma_probe, ",").is_ok() {
                        probe = comma_probe;
                        if peek_symbol(probe) == Some(")") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(&mut probe)?);
                let mut comma_probe = probe;
                if skip_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(&mut probe, ")")?;
        *input = probe;
        return Ok(Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
            name,
            patterns,
        })));
    }

    if matches!(name.as_ident().map(Ident::as_str), Some("true" | "false")) {
        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
            name: Expr::name(name),
            pattern: None,
        })));
    }

    let is_plain_ident_pattern = match &name {
        Name::Ident(_) => true,
        Name::Path(path) => path.prefix == PathPrefix::Plain && path.segments.len() == 1,
        _ => false,
    };
    if !is_plain_ident_pattern {
        return Ok(Pattern::new(PatternKind::Variant(PatternVariant {
            name: Expr::name(name),
            pattern: None,
        })));
    }

    let ident = name
        .as_ident()
        .cloned()
        .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    Ok(Pattern::new(PatternKind::Ident(PatternIdent::new(ident))))
}

fn parse_literal_pattern_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut probe = *input;
    if let Ok(minus) = expect_symbol(&mut probe, "-") {
        let value = parse_number(&mut probe)?;
        let span = Span::union([token_span_to_span(&minus), value.span()]);
        *input = probe;
        return Ok(ExprKind::UnOp(ExprUnOp {
            span,
            op: UnOpKind::Neg,
            val: Box::new(value),
        })
        .into());
    }
    parse_string(input, 0).or_else(|_| parse_number(input))
}

fn parse_pattern_alternatives(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut alternatives = vec![parse_general_pattern(input)?];
    while skip_symbol(input, "|").is_ok() {
        alternatives.push(parse_general_pattern(input)?);
    }
    if alternatives.len() == 1 {
        return Ok(alternatives.into_iter().next().unwrap());
    }
    Ok(Pattern::new(PatternKind::Or(PatternOr {
        patterns: alternatives,
    })))
}

/// Recursively expands every `PatternKind::Or` node in `pat`, at any
/// nesting depth, into the cartesian product of concrete, `Or`-free
/// patterns — e.g. `(Some(1) | Some(2), y)` becomes `(Some(1), y)` and
/// `(Some(2), y)`. A pattern containing no `Or` anywhere returns itself,
/// unchanged, as the sole element.
fn expand_pattern_alternatives(pat: &Pattern) -> Vec<Pattern> {
    match pat.kind() {
        PatternKind::Or(or_pat) => or_pat
            .patterns
            .iter()
            .flat_map(expand_pattern_alternatives)
            .collect(),
        PatternKind::Tuple(tuple) => cartesian_patterns(&tuple.patterns)
            .into_iter()
            .map(|patterns| Pattern::new(PatternKind::Tuple(PatternTuple { patterns })))
            .collect(),
        PatternKind::TupleStruct(tuple_struct) => cartesian_patterns(&tuple_struct.patterns)
            .into_iter()
            .map(|patterns| {
                Pattern::new(PatternKind::TupleStruct(PatternTupleStruct {
                    name: tuple_struct.name.clone(),
                    patterns,
                }))
            })
            .collect(),
        PatternKind::Struct(struct_pat) => cartesian_struct_fields(&struct_pat.fields)
            .into_iter()
            .map(|fields| {
                Pattern::new(PatternKind::Struct(PatternStruct {
                    name: struct_pat.name.clone(),
                    fields,
                    has_rest: struct_pat.has_rest,
                }))
            })
            .collect(),
        PatternKind::Structural(structural) => cartesian_struct_fields(&structural.fields)
            .into_iter()
            .map(|fields| {
                Pattern::new(PatternKind::Structural(PatternStructural {
                    fields,
                    has_rest: structural.has_rest,
                }))
            })
            .collect(),
        PatternKind::Box(box_pat) => expand_pattern_alternatives(&box_pat.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Box(PatternBox {
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Ref(reference) => expand_pattern_alternatives(&reference.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Ref(PatternRef {
                    mutability: reference.mutability,
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Bind(bind) => expand_pattern_alternatives(&bind.pattern)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Bind(PatternBind {
                    ident: bind.ident.clone(),
                    pattern: Box::new(inner),
                }))
            })
            .collect(),
        PatternKind::Type(pattern_type) => expand_pattern_alternatives(&pattern_type.pat)
            .into_iter()
            .map(|inner| {
                Pattern::new(PatternKind::Type(PatternType {
                    pat: Box::new(inner),
                    ty: pattern_type.ty.clone(),
                }))
            })
            .collect(),
        PatternKind::Variant(variant) => match &variant.pattern {
            Some(nested) => expand_pattern_alternatives(nested)
                .into_iter()
                .map(|inner| {
                    Pattern::new(PatternKind::Variant(PatternVariant {
                        name: variant.name.clone(),
                        pattern: Some(Box::new(inner)),
                    }))
                })
                .collect(),
            None => vec![pat.clone()],
        },
        PatternKind::Ident(_)
        | PatternKind::Quote(_)
        | PatternKind::QuotePlural(_)
        | PatternKind::Wildcard(_) => vec![pat.clone()],
    }
}

/// Cartesian product of each pattern's own expansion — `patterns[i]`'s
/// alternatives are independent of every other element's.
fn cartesian_patterns(patterns: &[Pattern]) -> Vec<Vec<Pattern>> {
    patterns.iter().fold(vec![Vec::new()], |acc, pat| {
        let alts = expand_pattern_alternatives(pat);
        acc.into_iter()
            .flat_map(|prefix| {
                alts.iter().map(move |alt| {
                    let mut next = prefix.clone();
                    next.push(alt.clone());
                    next
                })
            })
            .collect()
    })
}

/// Same idea as `cartesian_patterns`, but for struct/structural pattern
/// fields, whose `Or`-bearing part (if any) lives in `field.rename`.
fn cartesian_struct_fields(
    fields: &[fp_core::ast::PatternStructField],
) -> Vec<Vec<fp_core::ast::PatternStructField>> {
    fields.iter().fold(vec![Vec::new()], |acc, field| {
        let alts: Vec<Option<Box<Pattern>>> = match &field.rename {
            Some(rename) => expand_pattern_alternatives(rename)
                .into_iter()
                .map(|p| Some(Box::new(p)))
                .collect(),
            None => vec![None],
        };
        acc.into_iter()
            .flat_map(|prefix| {
                alts.iter().map(move |rename| {
                    let mut next = prefix.clone();
                    next.push(fp_core::ast::PatternStructField {
                        name: field.name.clone(),
                        rename: rename.clone(),
                    });
                    next
                })
            })
            .collect()
    })
}

fn starts_ref_pattern_target(input: &[Token]) -> bool {
    matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            ..
        }) | Some(Token {
            kind: TokenKind::Number,
            ..
        })
    ) || matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }) if matches!(lexeme.as_str(), "_" | "&" | "(" | "{" | "[")
    )
}

fn array_pattern_to_expr(pattern: &Pattern) -> Option<Expr> {
    match pattern.kind() {
        PatternKind::Variant(PatternVariant {
            name,
            pattern: None,
        }) => Some(name.clone()),
        PatternKind::Wildcard(_) => Some(Expr::name(Name::from_ident(Ident::new("_")))),
        _ => None,
    }
}

pub(crate) fn parse_general_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    if peek_symbol(input) == Some("(") {
        skip_symbol(input, "(")?;
        let mut patterns = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                if skip_symbol(input, "..").is_ok() {
                    let mut probe = *input;
                    if skip_symbol(&mut probe, ",").is_ok() {
                        *input = probe;
                        if peek_symbol(input) == Some(")") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                patterns.push(parse_pattern_alternatives(input)?);
                let mut probe = *input;
                if skip_symbol(&mut probe, ",").is_err() {
                    break;
                }
                *input = probe;
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        skip_symbol(input, ")")?;
        return Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })));
    }
    parse_match_pattern(input)
}

fn parse_if_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::If).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut let_probe = probe;
    if skip_keyword(&mut let_probe, Keyword::Let).is_ok() {
        let mut patterns = vec![parse_general_pattern(&mut let_probe)?];
        while skip_symbol(&mut let_probe, "|").is_ok() {
            patterns.push(parse_general_pattern(&mut let_probe)?);
        }
        skip_symbol(&mut let_probe, "=")?;
        let scrutinee_start = let_probe;
        let scrutinee = parse_expr_winnow_no_struct(&mut let_probe, file)?;
        let then_expr = match parse_block_expr(&mut let_probe, file) {
            Ok(expr) => expr,
            Err(_) => {
                let mut retry_probe = scrutinee_start;
                let scrutinee = parse_keyword_name_expr_no_struct(&mut retry_probe, file)?;
                let then_expr = parse_block_expr(&mut retry_probe, file)?;
                return build_if_let_expr(input, patterns, scrutinee, then_expr, retry_probe, file);
            }
        };
        let mut elze = None;
        let mut else_probe = let_probe;
        if skip_keyword(&mut else_probe, Keyword::Else).is_ok() {
            let else_expr = parse_expr_winnow(&mut else_probe, file)?;
            elze = Some(Box::new(else_expr));
            let_probe = else_probe;
        }
        return build_if_let_match(patterns, scrutinee, then_expr, elze, let_probe, input);
    }
    let cond_start = probe;
    let cond = match parse_expr_winnow(&mut probe, file) {
        Ok(cond) => cond,
        Err(_) => {
            return parse_if_expr_no_struct_condition(input, file, cond_start);
        }
    };
    let then_expr = match parse_block_expr(&mut probe, file) {
        Ok(expr) => expr,
        Err(_) => {
            return parse_if_expr_no_struct_condition(input, file, cond_start);
        }
    };
    let mut elze = None;
    let mut else_probe = probe;
    if skip_keyword(&mut else_probe, Keyword::Else).is_ok() {
        let else_expr = parse_expr_winnow(&mut else_probe, file)?;
        elze = Some(Box::new(else_expr));
        probe = else_probe;
    }
    *input = probe;
    Ok(ExprKind::If(ExprIf {
        span: union_spans(cond.span(), then_expr.span()),
        cond: Box::new(cond),
        then: Box::new(then_expr),
        elze,
    })
    .into())
}

fn build_if_let_expr<'a>(
    input: &mut &'a [Token],
    patterns: Vec<Pattern>,
    scrutinee: Expr,
    then_expr: Expr,
    mut probe: &'a [Token],
    file: FileId,
) -> ModalResult<Expr> {
    let mut elze = None;
    let mut else_probe = probe;
    if skip_keyword(&mut else_probe, Keyword::Else).is_ok() {
        let else_expr = parse_expr_winnow(&mut else_probe, file)?;
        elze = Some(Box::new(else_expr));
        probe = else_probe;
    }
    build_if_let_match(patterns, scrutinee, then_expr, elze, probe, input)
}

fn build_if_let_match<'a>(
    patterns: Vec<Pattern>,
    scrutinee: Expr,
    then_expr: Expr,
    elze: Option<Box<Expr>>,
    probe: &'a [Token],
    input: &mut &'a [Token],
) -> ModalResult<Expr> {
    *input = probe;
    let else_span = elze
        .as_ref()
        .map(|expr| expr.span())
        .unwrap_or_else(Span::null);
    let else_body = elze.unwrap_or_else(|| Box::new(Expr::unit()));
    let full_pattern = if patterns.len() == 1 {
        patterns.into_iter().next().unwrap()
    } else {
        Pattern::new(PatternKind::Or(PatternOr { patterns }))
    };
    let mut cases = expand_pattern_alternatives(&full_pattern)
        .into_iter()
        .map(|pat| fp_core::ast::ExprMatchCase {
            span: union_spans(pat.span(), then_expr.span()),
            pat: Some(Box::new(pat)),
            cond: Box::new(Expr::value(Value::bool(true))),
            guard: None,
            body: Box::new(then_expr.clone()),
        })
        .collect::<Vec<_>>();
    cases.push(fp_core::ast::ExprMatchCase {
        span: else_span,
        pat: Some(Box::new(Pattern::new(PatternKind::Wildcard(
            PatternWildcard {},
        )))),
        cond: Box::new(Expr::value(Value::bool(true))),
        guard: None,
        body: else_body,
    });
    Ok(ExprKind::Match(fp_core::ast::ExprMatch {
        span: union_spans(scrutinee.span(), then_expr.span()),
        scrutinee: Some(Box::new(scrutinee)),
        cases,
    })
    .into())
}

fn parse_if_expr_no_struct_condition<'a>(
    input: &mut &'a [Token],
    file: FileId,
    cond_start: &'a [Token],
) -> ModalResult<Expr> {
    let mut probe = cond_start;
    let cond = parse_expr_winnow_no_struct(&mut probe, file)?;
    let then_expr = parse_block_expr(&mut probe, file)?;
    let mut elze = None;
    let mut else_probe = probe;
    if skip_keyword(&mut else_probe, Keyword::Else).is_ok() {
        let else_expr = parse_expr_winnow(&mut else_probe, file)?;
        elze = Some(Box::new(else_expr));
        probe = else_probe;
    }
    *input = probe;
    Ok(ExprKind::If(ExprIf {
        span: union_spans(cond.span(), then_expr.span()),
        cond: Box::new(cond),
        then: Box::new(then_expr),
        elze,
    })
    .into())
}

fn parse_let_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_let_expr_impl(input, file, false)
}

/// `parse_let_expr`, but for a `let` reached through
/// [`parse_primary_no_struct`] (an `if`/`while` condition's `let`-chain,
/// e.g. `if let Some(x) = a && let Some(y) = b { .. }`). The scrutinee after
/// `=` must itself stay in no-struct mode too — otherwise, when a let-chain
/// has more than one nested `let` (each one's own scrutinee is parsed by
/// recursing back into this same primary-expression grammar), the innermost
/// `let`'s scrutinee can reach all the way up to the chain's final bare
/// identifier (e.g. `&& z > y` in a longer chain) and misparse `y {` as the
/// start of a struct literal instead of stopping there for the condition's
/// own block to be parsed — since `parse_let_expr`'s scrutinee parse used to
/// always call the struct-permitting `parse_expr_winnow` regardless of which
/// context (`parse_primary` vs `parse_primary_no_struct`) reached it.
fn parse_let_expr_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_let_expr_impl(input, file, true)
}

fn parse_let_expr_impl(input: &mut &[Token], file: FileId, no_struct: bool) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Let).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let pat = parse_general_pattern(&mut probe)?;
    skip_symbol(&mut probe, "=")?;
    let expr = if no_struct {
        parse_expr_winnow_no_struct(&mut probe, file)?
    } else {
        parse_expr_winnow(&mut probe, file)?
    };
    *input = probe;
    Ok(ExprKind::Let(ExprLet {
        span: union_spans(pat.span(), expr.span()),
        pat: Box::new(pat),
        expr: Box::new(expr),
    })
    .into())
}

fn parse_loop_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Loop).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::Loop(ExprLoop {
        span: body.span(),
        label: None,
        body: Box::new(body),
    })
    .into())
}

fn parse_while_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::While).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut let_probe = probe;
    if skip_keyword(&mut let_probe, Keyword::Let).is_ok() {
        let pat = parse_general_pattern(&mut let_probe)?;
        skip_symbol(&mut let_probe, "=")?;
        let scrutinee_start = let_probe;
        let scrutinee = parse_expr_winnow_no_struct(&mut let_probe, file)?;
        let body = match parse_block_expr(&mut let_probe, file) {
            Ok(body) => body,
            Err(_) => {
                let mut retry_probe = scrutinee_start;
                let scrutinee = parse_keyword_name_expr_no_struct(&mut retry_probe, file)?;
                let body = parse_block_expr(&mut retry_probe, file)?;
                *input = retry_probe;
                return Ok(build_while_let_loop(pat, scrutinee, body));
            }
        };
        *input = let_probe;
        return Ok(build_while_let_loop(pat, scrutinee, body));
    }
    let cond_start = probe;
    let cond = match parse_expr_winnow(&mut probe, file) {
        Ok(cond) => cond,
        Err(_) => {
            return parse_while_expr_no_struct_condition(input, file, cond_start);
        }
    };
    let body = match parse_block_expr(&mut probe, file) {
        Ok(body) => body,
        Err(_) => {
            return parse_while_expr_no_struct_condition(input, file, cond_start);
        }
    };
    *input = probe;
    Ok(ExprKind::While(ExprWhile {
        span: union_spans(cond.span(), body.span()),
        cond: Box::new(cond),
        body: Box::new(body),
    })
    .into())
}

fn build_while_let_loop(pat: Pattern, scrutinee: Expr, body: Expr) -> Expr {
    let mut cases = expand_pattern_alternatives(&pat)
        .into_iter()
        .map(|pat| fp_core::ast::ExprMatchCase {
            span: union_spans(pat.span(), body.span()),
            pat: Some(Box::new(pat)),
            cond: Box::new(Expr::value(Value::bool(true))),
            guard: None,
            body: Box::new(body.clone()),
        })
        .collect::<Vec<_>>();
    cases.push(fp_core::ast::ExprMatchCase {
        span: Span::null(),
        pat: Some(Box::new(Pattern::new(PatternKind::Wildcard(
            PatternWildcard {},
        )))),
        cond: Box::new(Expr::value(Value::bool(true))),
        guard: None,
        body: Box::new(Expr::new(ExprKind::Break(ExprBreak {
            span: Span::null(),
            value: None,
        }))),
    });
    let match_expr = Expr::new(ExprKind::Match(fp_core::ast::ExprMatch {
        span: union_spans(scrutinee.span(), body.span()),
        scrutinee: Some(Box::new(scrutinee)),
        cases,
    }));
    let loop_block = ExprBlock::new_stmts(vec![BlockStmt::Expr(BlockStmtExpr::new(match_expr))]);
    ExprKind::Loop(ExprLoop {
        span: body.span(),
        label: None,
        body: Box::new(Expr::block(loop_block)),
    })
    .into()
}

fn parse_while_expr_no_struct_condition<'a>(
    input: &mut &'a [Token],
    file: FileId,
    cond_start: &'a [Token],
) -> ModalResult<Expr> {
    let mut probe = cond_start;
    let cond = parse_expr_winnow_no_struct(&mut probe, file)?;
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::While(ExprWhile {
        span: union_spans(cond.span(), body.span()),
        cond: Box::new(cond),
        body: Box::new(body),
    })
    .into())
}

fn parse_for_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::For).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let pat = parse_general_pattern(&mut probe)?;
    skip_keyword(&mut probe, Keyword::In)?;
    let iter_start = probe;
    let iter = match parse_expr_winnow(&mut probe, file) {
        Ok(iter) => iter,
        Err(_) => {
            return parse_for_expr_no_struct_iter(input, file, pat, iter_start);
        }
    };
    let body = match parse_block_expr(&mut probe, file) {
        Ok(body) => body,
        Err(_) => {
            return parse_for_expr_no_struct_iter(input, file, pat, iter_start);
        }
    };
    *input = probe;
    Ok(ExprKind::For(ExprFor {
        span: union_spans(iter.span(), body.span()),
        pat: Box::new(pat),
        iter: Box::new(iter),
        body: Box::new(body),
    })
    .into())
}

fn parse_for_expr_no_struct_iter<'a>(
    input: &mut &'a [Token],
    file: FileId,
    pat: Pattern,
    iter_start: &'a [Token],
) -> ModalResult<Expr> {
    let mut probe = iter_start;
    let iter = parse_expr_winnow_no_struct(&mut probe, file)?;
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::For(ExprFor {
        span: union_spans(iter.span(), body.span()),
        pat: Box::new(pat),
        iter: Box::new(iter),
        body: Box::new(body),
    })
    .into())
}

fn parse_with_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::With).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let context_start = probe;
    let context = match parse_expr_winnow(&mut probe, file) {
        Ok(context) => context,
        Err(_) => {
            return parse_with_expr_no_struct_context(input, file, context_start);
        }
    };
    let body = match parse_block_expr(&mut probe, file) {
        Ok(body) => body,
        Err(_) => {
            return parse_with_expr_no_struct_context(input, file, context_start);
        }
    };
    *input = probe;
    Ok(ExprKind::With(ExprWith {
        span: union_spans(context.span(), body.span()),
        context: Box::new(context),
        body: Box::new(body),
    })
    .into())
}

fn parse_with_expr_no_struct_context<'a>(
    input: &mut &'a [Token],
    file: FileId,
    context_start: &'a [Token],
) -> ModalResult<Expr> {
    let mut probe = context_start;
    let context = parse_expr_winnow_no_struct(&mut probe, file)?;
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::With(ExprWith {
        span: union_spans(context.span(), body.span()),
        context: Box::new(context),
        body: Box::new(body),
    })
    .into())
}

fn parse_unsafe_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Unsafe).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(body)
}

fn parse_async_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Async).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let _ = expect_keyword(&mut probe, Keyword::Move);
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::Async(fp_core::ast::ExprAsync {
        span: body.span(),
        expr: Box::new(body),
    })
    .into())
}

fn parse_const_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Const).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::ConstBlock(ExprConstBlock {
        span: body.span(),
        collected_items: Vec::new(),
        expr: Box::new(body),
    })
    .into())
}

fn parse_return_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Return).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let value = if terminates_expr(probe) {
        None
    } else if matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::If))
    {
        let mut if_probe = probe;
        match parse_if_expr(&mut if_probe, file) {
            Ok(expr) => {
                probe = if_probe;
                Some(Box::new(expr))
            }
            Err(_) => Some(Box::new(parse_expr_winnow(&mut probe, file)?)),
        }
    } else {
        Some(Box::new(parse_expr_winnow(&mut probe, file)?))
    };
    *input = probe;
    Ok(ExprKind::Return(ExprReturn {
        span: value
            .as_ref()
            .map(|expr| expr.span())
            .unwrap_or_else(Span::null),
        value,
    })
    .into())
}

fn parse_break_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Break).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    if matches!(peek_ident_like(probe), Some(label) if label.starts_with('\'')) {
        let _ = ident_like(&mut probe)?;
    }
    let value = if terminates_expr(probe) {
        None
    } else {
        Some(Box::new(parse_expr_winnow(&mut probe, file)?))
    };
    *input = probe;
    Ok(ExprKind::Break(ExprBreak {
        span: value
            .as_ref()
            .map(|expr| expr.span())
            .unwrap_or_else(Span::null),
        value,
    })
    .into())
}

fn parse_continue_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Continue).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    if matches!(peek_ident_like(probe), Some(label) if label.starts_with('\'')) {
        let _ = ident_like(&mut probe)?;
    }
    *input = probe;
    Ok(ExprKind::Continue(ExprContinue { span: Span::null() }).into())
}

fn parse_labeled_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    let Some(label) = peek_ident_like(probe) else {
        return Err(ErrMode::Backtrack(ContextError::new()));
    };
    if !label.starts_with('\'') {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let _ = ident_like(&mut probe)?;
    if skip_symbol(&mut probe, ":").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }

    let expr = if matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Loop))
    {
        let mut loop_probe = probe;
        let mut expr = parse_loop_expr(&mut loop_probe, file)?;
        if let ExprKind::Loop(loop_expr) = expr.kind_mut() {
            loop_expr.label = Some(Ident::new(label));
        }
        probe = loop_probe;
        expr
    } else if matches!(
        probe.first(),
        Some(token) if token.kind == TokenKind::Keyword(Keyword::While)
    ) {
        let mut while_probe = probe;
        let expr = parse_while_expr(&mut while_probe, file)?;
        probe = while_probe;
        expr
    } else if matches!(
        probe.first(),
        Some(token) if token.kind == TokenKind::Keyword(Keyword::For)
    ) {
        let mut for_probe = probe;
        let expr = parse_for_expr(&mut for_probe, file)?;
        probe = for_probe;
        expr
    } else if peek_symbol(probe) == Some("{") {
        let mut block_probe = probe;
        let expr = parse_block_expr(&mut block_probe, file)?;
        probe = block_probe;
        expr
    } else {
        return Err(ErrMode::Backtrack(ContextError::new()));
    };

    *input = probe;
    Ok(expr)
}

fn terminates_expr(input: &[Token]) -> bool {
    matches!(
        peek_symbol(input),
        Some(";") | Some("}") | Some(")") | Some("]") | Some(",")
    )
}
