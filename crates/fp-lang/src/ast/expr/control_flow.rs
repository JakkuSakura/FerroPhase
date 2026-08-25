use super::*;

pub(super) fn parse_if_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn build_if_let_expr<'a>(
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

pub(super) fn build_if_let_match<'a>(
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

pub(super) fn parse_if_expr_no_struct_condition<'a>(
    input: &mut &'a [Token],
    file: FileId,
    cond_start: &'a [Token],
) -> ModalResult<Expr> {
    let mut probe = cond_start;
    // Same `quote`/`splice`-keyword-vs-real-identifier collision
    // `parse_match_expr` retries around (see its own comment) — a plain
    // `if quote { .. }` condition (real `std::sys::args::windows`'s own
    // `if quote { cmd.push('"' as u16); }`) hits it identically: the
    // condition parse either swallows the if's own body as a bare
    // `quote { .. }` block or hard-errors trying to.
    let cond = match parse_expr_winnow_no_struct(&mut probe, file) {
        Ok(cond) if peek_symbol(probe) == Some("{") => cond,
        _ => {
            probe = cond_start;
            parse_keyword_name_expr_no_struct(&mut probe, file)?
        }
    };
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

pub(super) fn parse_let_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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
pub(super) fn parse_let_expr_no_struct(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    parse_let_expr_impl(input, file, true)
}

pub(super) fn parse_let_expr_impl(
    input: &mut &[Token],
    file: FileId,
    no_struct: bool,
) -> ModalResult<Expr> {
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

pub(super) fn parse_loop_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_while_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn build_while_let_loop(pat: Pattern, scrutinee: Expr, body: Expr) -> Expr {
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

pub(super) fn parse_while_expr_no_struct_condition<'a>(
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

pub(super) fn parse_for_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_for_expr_no_struct_iter<'a>(
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

pub(super) fn parse_with_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_with_expr_no_struct_context<'a>(
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

pub(super) fn parse_unsafe_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
    let mut probe = *input;
    if skip_keyword(&mut probe, Keyword::Unsafe).is_err() {
        return Err(ErrMode::Backtrack(ContextError::new()));
    }
    let body = parse_block_expr(&mut probe, file)?;
    *input = probe;
    Ok(body)
}

pub(super) fn parse_async_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_const_block_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_return_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_break_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn parse_continue_expr(input: &mut &[Token]) -> ModalResult<Expr> {
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

pub(super) fn parse_labeled_expr(input: &mut &[Token], file: FileId) -> ModalResult<Expr> {
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

pub(super) fn terminates_expr(input: &[Token]) -> bool {
    matches!(
        peek_symbol(input),
        Some(";") | Some("}") | Some(")") | Some("]") | Some(",")
            // An open-ended range immediately followed by a body block
            // (real `std::sys::env::uefi`'s own `for i in 0.. { .. }`) —
            // the `{` here is unambiguously the loop's own body, not the
            // start of the range end's struct literal (this checker has
            // no separate no-struct variant of range-end parsing itself
            // the way whole conditions/scrutinees do), so it terminates
            // the range's end the same way `;`/`}`/etc. already do.
            | Some("{")
    )
}
