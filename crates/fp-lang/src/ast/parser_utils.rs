use super::*;
use winnow::combinator::opt;
use winnow::Parser;

use crate::ast::lower_common::split_path_prefix;

pub(crate) fn starts_const_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Const)
                && second.kind == TokenKind::Keyword(Keyword::Fn)
    )
}

pub(crate) fn starts_async_fn(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Async)
                && second.kind == TokenKind::Keyword(Keyword::Fn)
    )
}

pub(crate) fn starts_const_struct(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, ..]
            if first.kind == TokenKind::Keyword(Keyword::Const)
                && second.kind == TokenKind::Keyword(Keyword::Struct)
    )
}

pub(crate) fn looks_like_extern_block(input: &[Token]) -> bool {
    matches!(
        input,
        [first, second, third, ..]
            if first.kind == TokenKind::Keyword(Keyword::Extern)
                && second.kind == TokenKind::StringLiteral
                && third.kind == TokenKind::Symbol
                && third.lexeme == "{"
    )
}

pub(crate) fn parse_name(input: &mut &[Token]) -> ModalResult<Name> {
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let first = ident_like(input)?;
    let first_args = parse_optional_type_args(input)?;
    let mut segments = vec![ParameterPathSegment::new(first, first_args)];
    loop {
        let mut probe = *input;
        if expect_symbol(&mut probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        let args = parse_optional_type_args(&mut probe)?;
        *input = probe;
        segments.push(ParameterPathSegment::new(next, args));
    }
    let has_args = segments.iter().any(|segment| !segment.args.is_empty());
    let idents: Vec<Ident> = segments
        .iter()
        .map(|segment| segment.ident.clone())
        .collect();
    let (prefix, plain_segments) = split_path_prefix(idents, saw_root);
    if has_args {
        let stripped_len = plain_segments.len();
        let segments = segments
            .into_iter()
            .rev()
            .take(stripped_len)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        Ok(Name::parameter_path(ParameterPath::new(prefix, segments)))
    } else {
        Ok(Name::path(Path::new(prefix, plain_segments)))
    }
}

pub(crate) fn parse_module_path(input: &mut &[Token]) -> ModalResult<Path> {
    let saw_root = opt(|input: &mut &[Token]| expect_symbol(input, "::"))
        .parse_next(input)?
        .is_some();
    let mut segments = vec![ident_like(input)?];
    loop {
        let mut probe = *input;
        if expect_symbol(&mut probe, "::").is_err() {
            break;
        }
        let Ok(next) = ident_like(&mut probe) else {
            break;
        };
        *input = probe;
        segments.push(next);
    }
    let (prefix, segments) = split_path_prefix(segments, saw_root);
    Ok(Path::new(prefix, segments))
}

pub(crate) fn token_kind(input: &mut &[Token], kind: TokenKind) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest)) if token.kind == kind => {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

pub(crate) fn expect_keyword(input: &mut &[Token], expected: Keyword) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest)) if token.kind == TokenKind::Keyword(expected) => {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

pub(crate) fn expect_symbol(input: &mut &[Token], expected: &str) -> ModalResult<Token> {
    match input.split_first() {
        Some((token, rest))
            if token.kind == TokenKind::Symbol && token.lexeme.as_str() == expected =>
        {
            *input = rest;
            Ok(token.clone())
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

pub(crate) fn ident_like(input: &mut &[Token]) -> ModalResult<Ident> {
    match input.split_first() {
        Some((token, rest)) if matches!(token.kind, TokenKind::Ident | TokenKind::Keyword(_)) => {
            *input = rest;
            Ok(Ident::new(token.lexeme.clone()))
        }
        _ => Err(ErrMode::Backtrack(ContextError::new())),
    }
}

pub(crate) fn peek_symbol(input: &[Token]) -> Option<&str> {
    match input.first() {
        Some(token) if token.kind == TokenKind::Symbol => Some(token.lexeme.as_str()),
        _ => None,
    }
}

pub(crate) fn peek_ident_like(input: &[Token]) -> Option<&str> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            lexeme,
            ..
        }) => Some(lexeme.as_str()),
        _ => None,
    }
}

pub(crate) fn binary_op(symbol: &str) -> Option<(u8, BinOpKind)> {
    Some(match symbol {
        "||" => (1, BinOpKind::Or),
        "&&" => (2, BinOpKind::And),
        "==" => (3, BinOpKind::Eq),
        "!=" => (3, BinOpKind::Ne),
        "<" => (4, BinOpKind::Lt),
        "<=" => (4, BinOpKind::Le),
        ">" => (4, BinOpKind::Gt),
        ">=" => (4, BinOpKind::Ge),
        "|" => (5, BinOpKind::BitOr),
        "^" => (6, BinOpKind::BitXor),
        "&" => (7, BinOpKind::BitAnd),
        "<<" => (8, BinOpKind::Shl),
        ">>" => (8, BinOpKind::Shr),
        "+" => (9, BinOpKind::Add),
        "-" => (9, BinOpKind::Sub),
        "*" => (10, BinOpKind::Mul),
        "/" => (10, BinOpKind::Div),
        "%" => (10, BinOpKind::Mod),
        _ => return None,
    })
}

pub(crate) fn map_err(err: ErrMode<ContextError>, input: &[Token]) -> DirectParseError {
    let message = match err {
        ErrMode::Backtrack(_) | ErrMode::Cut(_) => "failed to parse expression directly",
        ErrMode::Incomplete(_) => "incomplete expression input",
    };
    error_at_current(input, message)
}

pub(crate) fn error_at_current(input: &[Token], message: impl Into<String>) -> DirectParseError {
    DirectParseError::Message {
        message: message.into(),
        span: input.first().map(token_span_to_span),
    }
}

pub(crate) fn token_span_to_span(token: &Token) -> Span {
    Span::new(0, token.span.start as u32, token.span.end as u32)
}

pub(crate) fn span_from_expr(expr: &Expr) -> Span {
    expr.span()
}

pub(crate) fn union_spans(a: Span, b: Span) -> Span {
    Span::union([a, b])
}

pub(crate) fn union_exprs(a: &Expr, b: &Expr) -> Span {
    union_spans(span_from_expr(a), span_from_expr(b))
}
