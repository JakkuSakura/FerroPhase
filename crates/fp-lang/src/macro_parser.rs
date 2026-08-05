use fp_core::ast::MacroTokenTree;
use fp_core::span::Span;

use crate::ast::lower_common::{lex_span_from_span, lex_spans_for_group};
use crate::lexer::tokenizer::{classify_and_normalize_lexeme, Token, TokenKind};
use crate::lexer::Span as TokSpan;

pub(crate) fn macro_token_trees_to_tokens(tokens: &[MacroTokenTree]) -> Vec<Token> {
    let mut out = Vec::new();
    append_macro_tokens(tokens, &mut out);
    out
}

fn append_macro_tokens(tokens: &[MacroTokenTree], out: &mut Vec<Token>) {
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => {
                let (kind, lexeme) = classify_and_normalize_lexeme(&tok.text)
                    .unwrap_or((TokenKind::Symbol, tok.text.clone()));
                out.push(make_token(kind, lexeme, lex_span_from_span(tok.span)));
            }
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    fp_core::ast::MacroDelimiter::Parenthesis => ("(", ")"),
                    fp_core::ast::MacroDelimiter::Bracket => ("[", "]"),
                    fp_core::ast::MacroDelimiter::Brace => ("{", "}"),
                };
                let (open_span, close_span) = lex_spans_for_group(group.span);
                push_symbol_token(out, open, open_span);
                append_macro_tokens(&group.tokens, out);
                push_symbol_token(out, close, close_span);
            }
        }
    }
}

fn make_token(kind: TokenKind, lexeme: String, span: TokSpan) -> Token {
    Token { kind, lexeme, span }
}

fn push_symbol_token(out: &mut Vec<Token>, symbol: &str, span: TokSpan) {
    out.push(make_token(TokenKind::Symbol, symbol.to_string(), span));
}

pub(crate) fn tokens_to_top_level_slices(tokens: &[Token]) -> Vec<&[Token]> {
    let mut out = Vec::new();
    let mut start = 0usize;
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;

    for (idx, token) in tokens.iter().enumerate() {
        if token.kind == TokenKind::Symbol {
            match token.lexeme.as_str() {
                "(" => paren += 1,
                ")" => paren = paren.saturating_sub(1),
                "[" => bracket += 1,
                "]" => bracket = bracket.saturating_sub(1),
                "{" => brace += 1,
                "}" => brace = brace.saturating_sub(1),
                "," if paren == 0 && bracket == 0 && brace == 0 => {
                    if start < idx {
                        out.push(&tokens[start..idx]);
                    }
                    start = idx + 1;
                }
                _ => {}
            }
        }
    }

    if start < tokens.len() {
        out.push(&tokens[start..]);
    }
    out
}

pub(crate) fn wrap_tokens_in_group(
    inner: &[Token],
    open: &str,
    close: &str,
    span: Span,
) -> Vec<Token> {
    let (open_span, close_span) = lex_spans_for_group(span);
    let mut out = Vec::with_capacity(inner.len() + 2);
    out.push(make_token(TokenKind::Symbol, open.to_string(), open_span));
    out.extend_from_slice(inner);
    out.push(make_token(TokenKind::Symbol, close.to_string(), close_span));
    out
}
