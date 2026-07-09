use eyre::Result;

use crate::lexer::{Keyword, Span, Token, TokenKind};

pub(crate) fn lower_tokens(tokens: Vec<Token>) -> Result<Vec<Token>> {
    let tokens = lower_emit(tokens)?;
    let tokens = lower_trailing_dot_numbers(tokens);
    lower_generic_closing_shifts(tokens)
}

fn lower_trailing_dot_numbers(tokens: Vec<Token>) -> Vec<Token> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0usize;
    while i < tokens.len() {
        if let Some(merged) = try_merge_trailing_dot_number(&tokens, i) {
            out.push(merged);
            i += 2;
            continue;
        }
        out.push(tokens[i].clone());
        i += 1;
    }
    out
}

fn lower_generic_closing_shifts(tokens: Vec<Token>) -> Result<Vec<Token>> {
    // The lexer tokenizes `>>` as a single symbol, but in generic contexts it can represent
    // two consecutive `>` tokens (e.g. `Foo<Bar<Baz>>`).
    let mut out = Vec::with_capacity(tokens.len());
    let mut angle_depth = 0i32;
    for tok in tokens {
        if tok.kind == TokenKind::Symbol {
            match tok.lexeme.as_str() {
                "<" => {
                    angle_depth += 1;
                    out.push(tok);
                    continue;
                }
                ">" => {
                    if angle_depth > 0 {
                        angle_depth -= 1;
                    }
                    out.push(tok);
                    continue;
                }
                ">>" if angle_depth > 0 => {
                    out.push(synth(">", tok.span)?);
                    out.push(synth(">", tok.span)?);
                    angle_depth = (angle_depth - 2).max(0);
                    continue;
                }
                _ => {}
            }
        }
        if matches!(tok.kind, TokenKind::StringLiteral) {
            out.push(tok);
            continue;
        }
        out.push(tok);
    }
    Ok(out)
}

fn lower_emit(tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0usize;
    while i < tokens.len() {
        if let Some((group_end, replacement)) = try_lower_emit(&tokens, i)? {
            out.extend(replacement);
            i = group_end + 1;
            continue;
        }

        out.push(tokens[i].clone());
        i += 1;
    }
    Ok(out)
}

fn try_merge_trailing_dot_number(tokens: &[Token], idx: usize) -> Option<Token> {
    let Some(current) = tokens.get(idx) else {
        return None;
    };
    if !is_number(current) {
        return None;
    }
    let dot = tokens.get(idx + 1)?;
    if !is_symbol(dot, ".") {
        return None;
    }
    let next_is_field_like = tokens
        .get(idx + 2)
        .is_some_and(|tok| is_field_like_after_dot(tok));
    if next_is_field_like {
        return None;
    }

    let mut merged = current.clone();
    merged.lexeme.push('.');
    merged.span.end = dot.span.end;
    Some(merged)
}

fn try_lower_emit(tokens: &[Token], idx: usize) -> Result<Option<(usize, Vec<Token>)>> {
    let Some(current) = tokens.get(idx) else {
        return Ok(None);
    };
    if !is_keyword(current, Keyword::Emit) {
        return Ok(None);
    }
    if !tokens.get(idx + 1).is_some_and(|t| is_symbol(t, "!")) {
        return Ok(None);
    }
    if !tokens.get(idx + 2).is_some_and(|t| is_symbol(t, "{")) {
        return Ok(None);
    }

    // TODO(semantic constraints): `emit! { ... }` is currently desugared in this token
    // rewrite pass into `splice(quote { ... })`, without validating where it appears.
    //
    // If we ever want diagnostics like “emit! only allowed inside const blocks”, that
    // belongs in a later semantic/typing stage (after AST), because a token pass cannot
    // reliably understand surrounding context.
    let (group_end, group_tokens) = consume_balanced_group(tokens, idx + 2)
        .ok_or_else(|| eyre::eyre!("unterminated emit! {{...}} group"))?;

    // TODO: synthesized token spans are currently anchored to the `emit` span.
    // This is convenient but yields coarse diagnostics. Possible follow-ups:
    // - pick better spans per synthesized token (e.g. around `!` / `{`)
    // - or add a synthetic/derived marker to Token for diagnostics.
    let emit_span = tokens[idx].span;
    let bang_span = tokens[idx + 1].span;
    let group_span = tokens[idx + 2].span;

    let mut replacement = Vec::with_capacity(group_tokens.len() + 4);
    replacement.push(synth("splice", emit_span)?);
    replacement.push(synth("(", bang_span)?);
    replacement.push(synth("quote", group_span)?);
    replacement.extend(group_tokens.iter().cloned());
    replacement.push(synth(")", group_span)?);

    Ok(Some((group_end, replacement)))
}

fn is_field_like_after_dot(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Ident | TokenKind::Number | TokenKind::Keyword(_)
    )
}

fn is_keyword(token: &Token, keyword: Keyword) -> bool {
    matches!(token.kind, TokenKind::Keyword(kw) if kw == keyword)
}

fn is_number(token: &Token) -> bool {
    token.kind == TokenKind::Number
}

fn is_symbol(token: &Token, lexeme: &str) -> bool {
    token.kind == TokenKind::Symbol && token.lexeme == lexeme
}

fn consume_balanced_group<'a>(
    tokens: &'a [Token],
    open_idx: usize,
) -> Option<(usize, &'a [Token])> {
    let open = tokens.get(open_idx)?;
    if open.kind != TokenKind::Symbol {
        return None;
    }
    let expected_close = match open.lexeme.as_str() {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        _ => return None,
    };

    let mut stack: Vec<&'static str> = Vec::new();
    for (idx, tok) in tokens.iter().enumerate().skip(open_idx) {
        if tok.kind == TokenKind::Symbol {
            match tok.lexeme.as_str() {
                "(" => stack.push(")"),
                "{" => stack.push("}"),
                "[" => stack.push("]"),
                ")" | "}" | "]" => {
                    let expected = stack.pop()?;
                    if tok.lexeme != expected {
                        return None;
                    }
                    if stack.is_empty() {
                        // The initial opener's matching closer.
                        if tok.lexeme == expected_close {
                            return Some((idx, &tokens[open_idx..=idx]));
                        }
                        return None;
                    }
                }
                _ => {}
            }
        }
    }
    None
}

fn synth(text: &str, span: Span) -> Result<Token> {
    let (kind, lexeme) = crate::lexer::tokenizer::classify_and_normalize_lexeme(text)
        .ok_or_else(|| eyre::eyre!("failed to classify synthesized token {:?}", text))?;
    Ok(Token { kind, lexeme, span })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lowers_emit_into_splice_of_quote_token_stream() {
        let src = "emit! { 1 + 2 }";
        let tokens = crate::lexer::lex(src).expect("lex");

        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Emit))));

        let lowered = lower_tokens(tokens).expect("lower_tokens");
        assert!(lowered
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Splice))));
        assert!(lowered
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Quote))));
        assert!(!lowered
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Emit))));
    }
}
