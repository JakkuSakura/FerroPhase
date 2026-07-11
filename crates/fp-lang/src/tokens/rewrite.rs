use crate::lexer::{Span, Token, TokenKind};

pub(crate) fn lower_tokens(tokens: Vec<Token>) -> Vec<Token> {
    lower_generic_closing_shifts(tokens)
}

fn lower_generic_closing_shifts(tokens: Vec<Token>) -> Vec<Token> {
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
                    out.push(synth(">", tok.span));
                    out.push(synth(">", tok.span));
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
    out
}

fn synth(text: &str, span: Span) -> Token {
    let (kind, lexeme) = crate::lexer::tokenizer::classify_and_normalize_lexeme(text)
        .unwrap_or_else(|| panic!("failed to classify synthesized token {:?}", text));
    Token { kind, lexeme, span }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_is_parsed_as_splice_of_quote() {
        let src = "emit! { 1 + 2 }";
        let tokens = crate::lexer::lex(src).expect("lex");
        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Keyword(crate::lexer::Keyword::Emit))));

        let expr = crate::ast::parse_expr_tokens(&tokens, 0).expect("parse");
        let kind = expr.kind();
        assert!(
            matches!(kind, fp_core::ast::ExprKind::Splice(_)),
            "emit! should produce Splice, got {:?}",
            kind
        );
    }
}
