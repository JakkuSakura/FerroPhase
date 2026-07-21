use super::*;
use fp_core::ast::PatternRef;
use fp_core::ast::PatternBind;
use fp_core::ast::ExprLet;
use fp_core::module::path::PathPrefix;
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
    if !matches!(op, "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=") {
        return Ok(lhs);
    }
    let op = op.to_string();
    expect_symbol(input, &op)?;
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
    if !matches!(op, "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=") {
        return Ok(lhs);
    }
    let op = op.to_string();
    expect_symbol(input, &op)?;
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
            expect_symbol(input, op)?;
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
    expect_symbol(input, &op)?;
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
            expect_symbol(input, op)?;
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
    expect_symbol(input, &op)?;
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
        let Some(op) = peek_symbol(input) else {
            break;
        };
        let Some((prec, kind)) = binary_op(op) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        let op = op.to_string();
        expect_symbol(input, &op)?;
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
        let Some(op) = peek_symbol(input) else {
            break;
        };
        let Some((prec, kind)) = binary_op(op) else {
            break;
        };
        if prec < min_prec {
            break;
        }
        let op = op.to_string();
        expect_symbol(input, &op)?;
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
        if expect_keyword(&mut probe, Keyword::As).is_err() {
            break;
        }
        let ty = parse_simple_type(&mut probe)?;
        *input = probe;
        let span = span_from_expr(&expr);
        expr = ExprKind::Cast(ExprCast {
            span,
            expr: Box::new(expr),
            ty,
        })
        .into();
    }
    Ok(expr)
}

fn parse_cast_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut expr = parse_prefix_no_struct(input, file)?;
    loop {
        let mut probe = *input;
        if expect_keyword(&mut probe, Keyword::As).is_err() {
            break;
        }
        let ty = parse_simple_type(&mut probe)?;
        *input = probe;
        let span = span_from_expr(&expr);
        expr = ExprKind::Cast(ExprCast {
            span,
            expr: Box::new(expr),
            ty,
        })
        .into();
    }
    Ok(expr)
}

fn parse_prefix(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::Splice).is_ok() {
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
        if expect_symbol(&mut probe, "!").is_ok() && peek_symbol(probe) == Some("{") {
            let block = parse_balanced_quote_block(&mut probe)?;
            *input = probe;
            let quote_expr = Expr::new(ExprKind::Quote(ExprQuote {
                span: block.span,
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
    if expect_keyword(&mut probe, Keyword::Await).is_ok() {
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
            expect_symbol(input, &op)?;
            let is_mut_ref = op == "&" && expect_keyword(input, Keyword::Mut).is_ok();
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
    if expect_keyword(&mut probe, Keyword::Splice).is_ok() {
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
        if expect_symbol(&mut probe, "!").is_ok() && peek_symbol(probe) == Some("{") {
            let block = parse_balanced_quote_block(&mut probe)?;
            *input = probe;
            let quote_expr = Expr::new(ExprKind::Quote(ExprQuote {
                span: block.span,
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
    if expect_keyword(&mut probe, Keyword::Await).is_ok() {
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
            expect_symbol(input, &op)?;
            let is_mut_ref = op == "&" && expect_keyword(input, Keyword::Mut).is_ok();
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
    expect_symbol(input, "?")?;
    Ok(Postfix::Try)
}

fn parse_field_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    expect_symbol(input, ".")?;
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
    expect_symbol(&mut probe, "::")?;
    let field = ident_like(&mut probe)?;
    *input = probe;
    Ok(Postfix::Field(field))
}

fn parse_turbofish_suffix(input: &mut &[Token]) -> ModalResult<Postfix> {
    let mut probe = *input;
    if expect_symbol(&mut probe, "::").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    if expect_symbol(&mut probe, "<").is_err() {
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
    expect_symbol(input, "(")?;
    let (args, kwargs) = parse_call_args(input, file, ")")?;
    expect_symbol(input, ")")?;
    Ok(Postfix::Call(args, kwargs))
}

fn parse_index_suffix(input: &mut &[Token], file: FileId) -> ModalResult<Postfix> {
    expect_symbol(input, "[")?;
    let index = parse_expr_winnow(input, file)?;
    expect_symbol(input, "]")?;
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
    expect_symbol(input, "[")?;
    if peek_symbol(input) == Some("]") {
        expect_symbol(input, "]")?;
        return Ok(ExprKind::Array(ExprArray {
            span: Span::null(),
            values: Vec::new(),
        })
        .into());
    }

    let first = parse_expr_winnow(input, file)?;
    if expect_symbol(input, ";").is_ok() {
        let len = parse_expr_winnow(input, file)?;
        expect_symbol(input, "]")?;
        return Ok(ExprKind::ArrayRepeat(ExprArrayRepeat {
            span: union_exprs(&first, &len),
            elem: Box::new(first),
            len: Box::new(len),
        })
        .into());
    }

    let mut values = vec![first];
    while expect_symbol(input, ",").is_ok() {
        if peek_symbol(input) == Some("]") {
            break;
        }
        values.push(parse_expr_winnow(input, file)?);
    }
    expect_symbol(input, "]")?;
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
    if expect_symbol(input, ",").is_ok() {
        let mut values = vec![expr];
        if peek_symbol(input) != Some(")") {
            loop {
                values.push(parse_expr_winnow(input, file)?);
                if expect_symbol(input, ",").is_err() {
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
    Ok(Expr::value(value)
        .with_ty_slot(ty)
        .with_span(token_span_to_span(&token)))
}

fn parse_string(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let token = token_kind(input, TokenKind::StringLiteral)?;
    if token.lexeme.starts_with('f') {
        return parse_f_string_literal_local(&token.lexeme, file)
            .map_err(|_| ErrMode::Cut(ContextError::new()));
    }
    let value =
        decode_string_literal(&token.lexeme).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    Ok(Expr::value(Value::string(value))
        .with_ty_slot(Some(Ty::Primitive(TypePrimitive::String)))
        .with_span(token_span_to_span(&token)))
}

fn parse_name_expr(input: &mut &[Token]) -> ModalResult<Expr> {
    let span = input
        .first()
        .map(token_span_to_span)
        .unwrap_or_else(Span::null);
    let name = parse_name(input)?;
    match name.as_ident().map(Ident::as_str) {
        Some("true") => Ok(Expr::value(Value::bool(true))
            .with_ty_slot(Some(Ty::Primitive(TypePrimitive::Bool)))
            .with_span(span)),
        Some("false") => Ok(Expr::value(Value::bool(false))
            .with_ty_slot(Some(Ty::Primitive(TypePrimitive::Bool)))
            .with_span(span)),
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
            if expect_symbol(&mut probe, "=").is_ok() {
                let value = parse_expr_winnow(&mut probe, file)?;
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
                let expr = parse_expr_winnow(input, file)?;
                args.push(expr);
            }
        } else {
            if saw_kwarg {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            let expr = parse_expr_winnow(input, file)?;
            args.push(expr);
        }

        let mut comma_probe = *input;
        if expect_symbol(&mut comma_probe, ",").is_err() {
            break;
        }
        *input = comma_probe;
        if peek_symbol(input) == Some(terminator) {
            break;
        }
    }

    Ok((args, kwargs))
}

fn parse_kwarg_name(input: &mut &[Token]) -> ModalResult<String> {
    let ident = ident_like(input)?;
    Ok(ident.name)
}

pub(crate) fn parse_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    expect_symbol(input, "{")?;
    let mut stmts = Vec::new();
    while peek_symbol(input) != Some("}") {
        if peek_symbol(input) == Some("#") {
            let mut attr_probe = *input;
            let attrs = crate::ast::items::parse_outer_attrs(&mut attr_probe, file)?;
            if !attrs.is_empty() {
                let mut item_probe = *input;
                if let Ok(item) = parse_block_item(&mut item_probe, file) {
                    *input = item_probe;
                    stmts.push(BlockStmt::Item(Box::new(item)));
                    continue;
                }
                *input = attr_probe;
            }
        }
        if starts_block_item(*input) {
            let mut item_probe = *input;
            if let Ok(item) = parse_block_item(&mut item_probe, file) {
                *input = item_probe;
                stmts.push(BlockStmt::Item(Box::new(item)));
                continue;
            }
        }
        let mut probe = *input;
        if expect_keyword(&mut probe, Keyword::Let).is_ok() {
            #[cfg(test)]
            eprintln!(
                "parse_block_expr let stmt start at {:?}",
                probe.first().map(|token| (
                    &token.kind,
                    token.lexeme.as_str(),
                    token.span.start,
                    token.span.end
                ))
            );
            let mut pat = if expect_keyword(&mut probe, Keyword::Mut).is_ok() {
                let name = ident_like(&mut probe)?;
                Pattern::new(PatternKind::Ident(PatternIdent {
                    ident: name,
                    mutability: Some(true),
                }))
            } else {
                parse_general_pattern(&mut probe)?
            };
            #[cfg(test)]
            eprintln!(
                "parse_block_expr let pat done next {:?}",
                probe.first().map(|token| (
                    &token.kind,
                    token.lexeme.as_str(),
                    token.span.start,
                    token.span.end
                ))
            );
            if expect_symbol(&mut probe, ":").is_ok() {
                let ty = parse_simple_type(&mut probe)?;
                pat = Pattern::new(PatternKind::Type(PatternType::new(pat, ty)));
            }
            if expect_symbol(&mut probe, "=").is_err() {
                if expect_symbol(&mut probe, ";").is_ok() {
                    *input = probe;
                    stmts.push(BlockStmt::Let(StmtLet::new(pat, None, None)));
                    continue;
                }
                return Err(ErrMode::Cut(ContextError::new()));
            }
            #[cfg(test)]
            eprintln!(
                "parse_block_expr let init start {:?}",
                probe.first().map(|token| (
                    &token.kind,
                    token.lexeme.as_str(),
                    token.span.start,
                    token.span.end
                ))
            );
            let init = parse_expr_winnow(&mut probe, file)?;
            #[cfg(test)]
            eprintln!(
                "parse_block_expr let init done next {:?}",
                probe.first().map(|token| (
                    &token.kind,
                    token.lexeme.as_str(),
                    token.span.start,
                    token.span.end
                ))
            );
            let diverge = if expect_keyword(&mut probe, Keyword::Else).is_ok() {
                Some(parse_block_expr(&mut probe, file)?)
            } else {
                None
            };
            let had_semi = expect_symbol(&mut probe, ";").is_ok();
            if !had_semi {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            *input = probe;
            stmts.push(BlockStmt::Let(StmtLet::new(pat, Some(init), diverge)));
            continue;
        }
        let mut probe = *input;
        if expect_keyword(&mut probe, Keyword::Defer).is_ok() {
            let expr = parse_expr_winnow(&mut probe, file)?;
            let had_semi = expect_symbol(&mut probe, ";").is_ok();
            if !had_semi {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            *input = probe;
            stmts.push(BlockStmt::Defer(StmtDefer {
                span: span_from_expr(&expr),
                expr: Box::new(expr),
            }));
            continue;
        }

        let expr = parse_expr_winnow(input, file)?;
        let mut semicolon = false;
        let mut probe = *input;
        if expect_symbol(&mut probe, ";").is_ok() {
            *input = probe;
            semicolon = true;
        } else if !expr_can_omit_semicolon_in_block(&expr) && peek_symbol(input) != Some("}") {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        stmts.push(BlockStmt::Expr(
            BlockStmtExpr::new(expr).with_semicolon(semicolon),
        ));
    }
    expect_symbol(input, "}")?;
    Ok(ExprKind::Block(ExprBlock::new_stmts(stmts)).into())
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
    let movability = if expect_keyword(&mut probe, Keyword::Move).is_ok() {
        Some(true)
    } else {
        None
    };
    if expect_symbol(&mut probe, "||").is_err() {
        if expect_symbol(&mut probe, "|").is_err() {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        let mut params = Vec::new();
        if peek_symbol(probe) != Some("|") {
            loop {
                params.push(parse_closure_param(&mut probe)?);
                let mut comma_probe = probe;
                if expect_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
            }
        }
        expect_symbol(&mut probe, "|")?;
        let ret_ty = if expect_symbol(&mut probe, "->").is_ok() {
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
    let ret_ty = if expect_symbol(&mut probe, "->").is_ok() {
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
    if expect_symbol(&mut probe, ":").is_ok() {
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
    if expect_keyword(&mut probe, Keyword::Quote).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut kind = None;
    if expect_symbol(&mut probe, "<").is_ok() {
        let ident = ident_like(&mut probe)?;
        kind = Some(match ident.as_str() {
            "expr" => QuoteFragmentKind::Expr,
            "stmt" => QuoteFragmentKind::Stmt,
            "item" | "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static" | "mod"
            | "use" | "macro" => QuoteFragmentKind::Item,
            "type" => QuoteFragmentKind::Type,
            _ => return Err(ErrMode::Cut(ContextError::new())),
        });
        expect_symbol(&mut probe, ">")?;
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
            expect_keyword(&mut probe, Keyword::Quote)?;
        }
    }
    let block = if matches!(kind, Some(QuoteFragmentKind::Item)) {
        parse_balanced_quote_block(&mut probe)?
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
        kind,
    })
    .into())
}

fn parse_balanced_quote_block(input: &mut &[Token]) -> ModalResult<ExprBlock> {
    expect_symbol(input, "{")?;
    let mut depth = 1usize;
    while let Some((token, rest)) = input.split_first() {
        *input = rest;
        if token.kind != TokenKind::Symbol {
            continue;
        }
        match token.lexeme.as_str() {
            "{" => depth += 1,
            "}" => {
                depth -= 1;
                if depth == 0 {
                    return Ok(ExprBlock::new());
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
    expect_symbol(&mut probe, "!")?;
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
    let is_structural = expect_keyword(&mut probe, Keyword::Struct).is_ok();
    let name = if is_structural {
        None
    } else {
        Some(parse_name(&mut probe)?)
    };
    if expect_symbol(&mut probe, "{").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut fields = Vec::new();
    let mut update = None;
    while peek_symbol(probe) != Some("}") {
        if expect_symbol(&mut probe, "..").is_ok() {
            update = Some(Box::new(parse_expr_winnow(&mut probe, file)?));
            break;
        }
        let field = ident_like(&mut probe)?;
        let value = if expect_symbol(&mut probe, ":").is_ok() {
            Some(parse_expr_winnow(&mut probe, file)?)
        } else {
            None
        };
        fields.push(ExprField {
            span: Span::null(),
            name: field,
            value,
        });
        let mut comma_probe = probe;
        if expect_symbol(&mut comma_probe, ",").is_ok() {
            probe = comma_probe;
            if peek_symbol(probe) == Some("}") {
                break;
            }
        } else {
            break;
        }
    }
    expect_symbol(&mut probe, "}")?;
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
    if expect_keyword(&mut probe, Keyword::Try).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let expr = parse_block_expr(&mut probe, file)?;
    let mut catches = Vec::new();
    loop {
        let mut clause_probe = probe;
        if expect_keyword(&mut clause_probe, Keyword::Catch).is_err() {
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
    if expect_keyword(&mut else_probe, Keyword::Else).is_ok() {
        let body = parse_block_expr(&mut else_probe, file)?;
        elze = Some(Box::new(body));
        probe = else_probe;
    }

    let mut finally = None;
    let mut finally_probe = probe;
    if expect_keyword(&mut finally_probe, Keyword::Finally).is_ok() {
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

fn parse_catch_pattern_and_body(input: &mut &[Token], file: FileId) -> ModalResult<(Pattern, Expr)> {
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

fn parse_pattern_prefix_tokens(tokens: &[Token]) -> Result<(Pattern, usize), DirectParseError> {
    let mut input = tokens;
    let pat = parse_general_pattern(&mut input).map_err(|err| map_err(err, input))?;
    let consumed = tokens.len() - input.len();
    Ok((pat, consumed))
}

fn parse_match_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::Match).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let scrutinee = parse_expr_winnow_no_struct(&mut probe, file)?;
    expect_symbol(&mut probe, "{")?;
    let mut cases = Vec::new();
    while peek_symbol(probe) != Some("}") {
        let mut arm_probe = probe;
        let pat = parse_general_pattern(&mut arm_probe)?;
        while expect_symbol(&mut arm_probe, "|").is_ok() {
            let _ = parse_general_pattern(&mut arm_probe)?;
        }
        let mut guard = None;
        let mut guard_probe = arm_probe;
        if expect_keyword(&mut guard_probe, Keyword::If).is_ok() {
            let guard_expr = parse_expr_winnow(&mut guard_probe, file)?;
            guard = Some(Box::new(guard_expr));
            arm_probe = guard_probe;
        }
        expect_symbol(&mut arm_probe, "=>")?;
        let body = if peek_symbol(arm_probe) == Some("{") {
            parse_block_expr(&mut arm_probe, file)?
        } else {
            parse_expr_winnow(&mut arm_probe, file)?
        };
        let mut comma_probe = arm_probe;
        if expect_symbol(&mut comma_probe, ",").is_ok() {
            arm_probe = comma_probe;
        }
        probe = arm_probe;
        cases.push(fp_core::ast::ExprMatchCase {
            span: union_spans(pat.span(), body.span()),
            pat: Some(Box::new(pat)),
            cond: Box::new(Expr::value(Value::bool(true))),
            guard,
            body: Box::new(body),
        });
    }
    expect_symbol(&mut probe, "}")?;
    *input = probe;
    Ok(ExprKind::Match(fp_core::ast::ExprMatch {
        span: span_from_expr(&scrutinee),
        scrutinee: Some(Box::new(scrutinee)),
        cases,
    })
    .into())
}

fn parse_match_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    let mut probe = *input;
    if expect_symbol(&mut probe, "&").is_ok() {
        let mutability = expect_keyword(&mut probe, Keyword::Mut).is_ok();
        let pattern = parse_general_pattern(&mut probe)?;
        *input = probe;
        return Ok(Pattern::new(PatternKind::Ref(PatternRef {
            mutability: mutability.then_some(true),
            pattern: Box::new(pattern),
        })));
    }
    if expect_keyword(&mut probe, Keyword::Mut).is_ok() {
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
    if expect_keyword(&mut probe, Keyword::Quote).is_ok() {
        let mut item = None;
        let mut fragment = QuoteFragmentKind::Item;
        if expect_symbol(&mut probe, "<").is_ok() {
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
            expect_symbol(&mut probe, ">")?;
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
    if let Ok(expr) = parse_string(&mut literal_probe, 0).or_else(|_| parse_number(&mut literal_probe))
    {
        let mut range_probe = literal_probe;
        if let Some(op) = peek_symbol(range_probe) {
            let limit = match op {
                ".." => Some(ExprRangeLimit::Exclusive),
                "..=" => Some(ExprRangeLimit::Inclusive),
                _ => None,
            };
            if let Some(limit) = limit {
                expect_symbol(&mut range_probe, op)?;
                let end = parse_string(&mut range_probe, 0)
                    .or_else(|_| parse_number(&mut range_probe))
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
    if expect_symbol(&mut array_probe, "[").is_ok() {
        let mut patterns = Vec::new();
        let mut has_rest = false;
        if peek_symbol(array_probe) != Some("]") {
            loop {
                if expect_symbol(&mut array_probe, "..").is_ok() {
                    has_rest = true;
                    break;
                }
                patterns.push(parse_general_pattern(&mut array_probe)?);
                let mut comma_probe = array_probe;
                if expect_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                array_probe = comma_probe;
                if peek_symbol(array_probe) == Some("]") {
                    break;
                }
            }
        }
        expect_symbol(&mut array_probe, "]")?;
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
        if expect_symbol(&mut bind_probe, "@").is_ok() {
            let pattern = if expect_symbol(&mut bind_probe, "..").is_ok() {
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
    if expect_symbol(&mut probe, "{").is_ok() {
        let mut fields = Vec::new();
        let mut has_rest = false;
        if peek_symbol(probe) != Some("}") {
            loop {
                if expect_symbol(&mut probe, "..").is_ok() {
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
                    let saw_mut = expect_keyword(&mut probe, Keyword::Mut).is_ok();
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
                    let rename = if expect_symbol(&mut probe, ":").is_ok() {
                        Some(Box::new(parse_general_pattern(&mut probe)?))
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
                if expect_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some("}") {
                    break;
                }
            }
        }
        expect_symbol(&mut probe, "}")?;
        *input = probe;
        let struct_name = name
            .to_path()
            .segments
            .last()
            .cloned()
            .or_else(|| name.as_ident().cloned())
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        return Ok(Pattern::new(PatternKind::Struct(fp_core::ast::PatternStruct {
            name: struct_name,
            fields,
            has_rest,
        })));
    }

    let mut probe = *input;
    if expect_symbol(&mut probe, "(").is_ok() {
        let mut patterns = Vec::new();
        if peek_symbol(probe) != Some(")") {
            loop {
                patterns.push(parse_general_pattern(&mut probe)?);
                let mut comma_probe = probe;
                if expect_symbol(&mut comma_probe, ",").is_err() {
                    break;
                }
                probe = comma_probe;
                if peek_symbol(probe) == Some(")") {
                    break;
                }
            }
        }
        expect_symbol(&mut probe, ")")?;
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
        PatternKind::Variant(PatternVariant { name, pattern: None }) => Some(name.clone()),
        PatternKind::Wildcard(_) => Some(Expr::name(Name::from_ident(Ident::new("_")))),
        _ => None,
    }
}

pub(crate) fn parse_general_pattern(input: &mut &[Token]) -> ModalResult<Pattern> {
    if peek_symbol(input) == Some("(") {
        expect_symbol(input, "(")?;
        let mut patterns = Vec::new();
        if peek_symbol(input) != Some(")") {
            loop {
                patterns.push(parse_general_pattern(input)?);
                let mut probe = *input;
                if expect_symbol(&mut probe, ",").is_err() {
                    break;
                }
                *input = probe;
                if peek_symbol(input) == Some(")") {
                    break;
                }
            }
        }
        expect_symbol(input, ")")?;
        return Ok(Pattern::new(PatternKind::Tuple(PatternTuple { patterns })));
    }
    parse_match_pattern(input)
}

fn parse_if_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::If).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut let_probe = probe;
    if expect_keyword(&mut let_probe, Keyword::Let).is_ok() {
        let mut patterns = vec![parse_general_pattern(&mut let_probe)?];
        while expect_symbol(&mut let_probe, "|").is_ok() {
            patterns.push(parse_general_pattern(&mut let_probe)?);
        }
        expect_symbol(&mut let_probe, "=")?;
        let scrutinee = parse_expr_winnow_no_struct(&mut let_probe, file)?;
        let then_expr = parse_block_expr(&mut let_probe, file)?;
        let mut elze = None;
        let mut else_probe = let_probe;
        if expect_keyword(&mut else_probe, Keyword::Else).is_ok() {
            let else_expr = parse_expr_winnow(&mut else_probe, file)?;
            elze = Some(Box::new(else_expr));
            let_probe = else_probe;
        }
        *input = let_probe;
        let else_span = elze.as_ref().map(|expr| expr.span()).unwrap_or_else(Span::null);
        let else_body = elze.unwrap_or_else(|| Box::new(Expr::unit()));
        let mut cases = patterns
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
        return Ok(ExprKind::Match(fp_core::ast::ExprMatch {
            span: union_spans(scrutinee.span(), then_expr.span()),
            scrutinee: Some(Box::new(scrutinee)),
            cases,
        })
        .into());
    }
    let cond_start = probe;
    let cond = match parse_expr_winnow(&mut probe, file) {
        Ok(cond) => cond,
        Err(err) => return Err(err),
    };
    let then_expr = match parse_block_expr(&mut probe, file) {
        Ok(expr) => expr,
        Err(_) => {
            probe = cond_start;
            let cond = parse_expr_winnow_no_struct(&mut probe, file)?;
            let then_expr = parse_block_expr(&mut probe, file)?;
            let mut elze = None;
            let mut else_probe = probe;
            if expect_keyword(&mut else_probe, Keyword::Else).is_ok() {
                let else_expr = parse_expr_winnow(&mut else_probe, file)?;
                elze = Some(Box::new(else_expr));
                probe = else_probe;
            }
            *input = probe;
            return Ok(ExprKind::If(ExprIf {
                span: union_spans(cond.span(), then_expr.span()),
                cond: Box::new(cond),
                then: Box::new(then_expr),
                elze,
            })
            .into());
        }
    };
    let mut elze = None;
    let mut else_probe = probe;
    if expect_keyword(&mut else_probe, Keyword::Else).is_ok() {
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
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::Let).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let pat = parse_general_pattern(&mut probe)?;
    expect_symbol(&mut probe, "=")?;
    let expr = parse_expr_winnow(&mut probe, file)?;
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
    if expect_keyword(&mut probe, Keyword::Loop).is_err() {
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
    if expect_keyword(&mut probe, Keyword::While).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let mut let_probe = probe;
    if expect_keyword(&mut let_probe, Keyword::Let).is_ok() {
        let pat = parse_general_pattern(&mut let_probe)?;
        expect_symbol(&mut let_probe, "=")?;
        let scrutinee = parse_expr_winnow_no_struct(&mut let_probe, file)?;
        let body = parse_block_expr(&mut let_probe, file)?;
        *input = let_probe;
        let match_expr = Expr::new(ExprKind::Match(fp_core::ast::ExprMatch {
            span: union_spans(scrutinee.span(), body.span()),
            scrutinee: Some(Box::new(scrutinee)),
            cases: vec![
                fp_core::ast::ExprMatchCase {
                    span: union_spans(pat.span(), body.span()),
                    pat: Some(Box::new(pat)),
                    cond: Box::new(Expr::value(Value::bool(true))),
                    guard: None,
                    body: Box::new(body.clone()),
                },
                fp_core::ast::ExprMatchCase {
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
                },
            ],
        }));
        let loop_block = ExprBlock::new_stmts(vec![BlockStmt::Expr(BlockStmtExpr::new(match_expr))]);
        return Ok(ExprKind::Loop(ExprLoop {
            span: body.span(),
            label: None,
            body: Box::new(Expr::block(loop_block)),
        })
        .into());
    }
    let cond_start = probe;
    let cond = match parse_expr_winnow(&mut probe, file) {
        Ok(cond) => cond,
        Err(err) => return Err(err),
    };
    let body = match parse_block_expr(&mut probe, file) {
        Ok(body) => body,
        Err(_) => {
            probe = cond_start;
            let cond = parse_expr_winnow_no_struct(&mut probe, file)?;
            let body = parse_block_expr(&mut probe, file)?;
            *input = probe;
            return Ok(ExprKind::While(ExprWhile {
                span: union_spans(cond.span(), body.span()),
                cond: Box::new(cond),
                body: Box::new(body),
            })
            .into());
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

fn parse_for_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::For).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let pat = parse_general_pattern(&mut probe)?;
    expect_keyword(&mut probe, Keyword::In)?;
    let iter_start = probe;
    let iter = match parse_expr_winnow(&mut probe, file) {
        Ok(iter) => iter,
        Err(err) => return Err(err),
    };
    let body = match parse_block_expr(&mut probe, file) {
        Ok(body) => body,
        Err(_) => {
            probe = iter_start;
            let iter = parse_expr_winnow_no_struct(&mut probe, file)?;
            let body = parse_block_expr(&mut probe, file)?;
            *input = probe;
            return Ok(ExprKind::For(ExprFor {
                span: union_spans(iter.span(), body.span()),
                pat: Box::new(pat),
                iter: Box::new(iter),
                body: Box::new(body),
            })
            .into());
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

fn parse_with_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::With).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let context = parse_expr_winnow(&mut probe, file)?;
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
    if expect_keyword(&mut probe, Keyword::Unsafe).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(body)
}

fn parse_async_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::Async).is_err() {
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
    if expect_keyword(&mut probe, Keyword::Const).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(ExprKind::ConstBlock(ExprConstBlock {
        span: body.span(),
        expr: Box::new(body),
    })
    .into())
}

fn parse_return_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if expect_keyword(&mut probe, Keyword::Return).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let value = if terminates_expr(probe) {
        None
    } else if matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::If)) {
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
    if expect_keyword(&mut probe, Keyword::Break).is_err() {
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
    if expect_keyword(&mut probe, Keyword::Continue).is_err() {
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
    if expect_symbol(&mut probe, ":").is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }

    let expr = if matches!(probe.first(), Some(token) if token.kind == TokenKind::Keyword(Keyword::Loop)) {
        let mut loop_probe = probe;
        let mut expr = parse_loop_expr(&mut loop_probe, file)?;
        if let ExprKind::Loop(loop_expr) = expr.kind_mut() {
            loop_expr.label = Some(Ident::new(label));
        }
        probe = loop_probe;
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
