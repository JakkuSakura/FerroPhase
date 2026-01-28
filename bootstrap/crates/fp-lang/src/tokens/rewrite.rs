use fp_core::Result;

use crate::lexer::{Keyword, Span, Token, TokenKind};

pub(crate) fn lower_tokens(tokens: Vec<Token>) -> Result<Vec<Token>> {
    let tokens = lower_emit(tokens)?;
    lower_fn_generic_closing_shifts(tokens)
}

fn lower_fn_generic_closing_shifts(tokens: Vec<Token>) -> Result<Vec<Token>> {
    // The lexer tokenizes `>>` as a single symbol, but in generic contexts it can represent
    // two consecutive `>` tokens (e.g. `Foo<Bar<Baz>>`).
    //
    // This pass only splits `>>` while we're inside a function generic parameter list.
    let mut out = Vec::with_capacity(tokens.len());
    let mut in_fn_header = false;
    let mut saw_fn_name = false;
    let mut in_generics = false;
    let mut angle_depth: i32 = 0;

    for tok in tokens {
        if matches!(tok.kind, TokenKind::Keyword(Keyword::Fn)) {
            in_fn_header = true;
            saw_fn_name = false;
            in_generics = false;
            angle_depth = 0;
            out.push(tok);
            continue;
        }
        if in_fn_header {
            if tok.kind == TokenKind::Symbol && tok.lexeme == "{" {
                in_fn_header = false;
                saw_fn_name = false;
                in_generics = false;
                angle_depth = 0;
                out.push(tok);
                continue;
            }
            if !saw_fn_name && tok.kind == TokenKind::Ident {
                saw_fn_name = true;
                out.push(tok);
                continue;
            }
            if saw_fn_name && !in_generics && tok.kind == TokenKind::Symbol && tok.lexeme == "<" {
                in_generics = true;
                angle_depth = 1;
                out.push(tok);
                continue;
            }
            if in_generics {
                if tok.kind == TokenKind::Symbol && tok.lexeme == "<" {
                    angle_depth += 1;
                    out.push(tok);
                    continue;
                }
                if tok.kind == TokenKind::Symbol && tok.lexeme == ">" {
                    angle_depth -= 1;
                    if angle_depth <= 0 {
                        in_generics = false;
                        angle_depth = 0;
                    }
                    out.push(tok);
                    continue;
                }
                if tok.kind == TokenKind::Symbol && tok.lexeme == ">>" {
                    // Split into two consecutive `>` tokens.
                    out.push(synth(">", tok.span)?);
                    out.push(synth(">", tok.span)?);
                    angle_depth -= 2;
                    if angle_depth <= 0 {
                        in_generics = false;
                        angle_depth = 0;
                    }
                    continue;
                }
            }
        }

        out.push(tok);
    }

    Ok(out)
}

fn lower_emit(tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0usize;
    while i < tokens.len() {
        if matches!(tokens[i].kind, TokenKind::Keyword(Keyword::Emit))
            && tokens
                .get(i + 1)
                .is_some_and(|t| t.kind == TokenKind::Symbol && t.lexeme == "!")
            && tokens
                .get(i + 2)
                .is_some_and(|t| t.kind == TokenKind::Symbol && t.lexeme == "{")
        {
            // TODO(semantic constraints): `emit! { ... }` is currently desugared in this token
            // rewrite pass into `splice(quote { ... })`, without validating where it appears.
            //
            // If we ever want diagnostics like “emit! only allowed inside const blocks”, that
            // belongs in a later semantic/typing stage (after AST), because a token pass cannot
            // reliably understand surrounding context.
            let (group_end, group_tokens) = consume_balanced_group(&tokens, i + 2)
                .ok_or_else(|| fp_core::Error::from("unterminated emit! {...} group"))?;

            // TODO: synthesized token spans are currently anchored to the `emit` span.
            // This is convenient but yields coarse diagnostics. Possible follow-ups:
            // - pick better spans per synthesized token (e.g. around `!` / `{`)
            // - or add a synthetic/derived marker to Token for diagnostics.
            let emit_span = tokens[i].span;
            let bang_span = tokens[i + 1].span;
            let group_span = tokens[i + 2].span;
            out.push(synth("splice", emit_span)?);
            out.push(synth("(", bang_span)?);
            out.push(synth("quote", group_span)?);
            out.extend(group_tokens.into_iter().cloned());
            out.push(synth(")", group_span)?);

            i = group_end + 1;
            continue;
        }

        out.push(tokens[i].clone());
        i += 1;
    }
    Ok(out)
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
        .ok_or_else(|| fp_core::Error::from(format!("failed to classify synthesized token {:?}", text)))?;
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
