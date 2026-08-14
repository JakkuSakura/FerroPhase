//! Minimal, self-contained Kotlin tokenizer built on the `winnow` parser-
//! combinator crate directly (no dependency on fp-lang — fp-lang is the
//! FerroPhase `.fp` dialect crate, not a shared toolkit for other source
//! languages; `winnow` itself is a plain external library, safe to depend
//! on independently the same way fp-lang does). Covers just enough of
//! Kotlin's lexical grammar for declaration-only parsing: identifiers
//! (incl. backtick-escaped), numbers, string literals (single- and
//! triple-quoted, with `${...}` template-expression regions tracked by
//! brace depth so an interior `"`/`}` doesn't end the literal early),
//! line/block comments (block comments may nest, per Kotlin's own
//! grammar), and punctuation.

use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
use winnow::token::take_while;
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident,
    Number,
    StringLiteral,
    Symbol,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub pos: usize,
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("kotlin lexer error at byte {pos}: {message}")]
pub struct LexError {
    pub message: String,
    pub pos: usize,
}

const TRIPLE_PUNCT: &[&str] = &["===", "!==", "..<"];
const DOUBLE_PUNCT: &[&str] = &[
    "->", "::", "..", "==", "!=", "<=", ">=", "&&", "||", "++", "--", "+=", "-=", "*=", "/=",
    "%=", "?:", "?.", "!!",
];
const SINGLE_PUNCT: &str = "=+-*/%&|^!~@?:;,.()[]{}<>$#";

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let mut input = src;
    let mut tokens = Vec::new();
    let mut angle_depth: i32 = 0;

    loop {
        ws_and_comments(&mut input).map_err(|err| to_lex_error(src, input, err))?;
        if input.is_empty() {
            break;
        }
        let start = src.len() - input.len();
        let kind = token_parser()
            .parse_next(&mut input)
            .map_err(|err| to_lex_error(src, input, err))?;
        let end = src.len() - input.len();
        let text = src[start..end].to_string();

        if kind == TokenKind::Symbol && (text == ">>" || text == ">>>") && angle_depth > 0 {
            tokens.push(Token { kind: TokenKind::Symbol, text: ">".to_string(), pos: start });
            angle_depth -= 1;
            input = &src[start + 1..];
            continue;
        }
        if kind == TokenKind::Symbol {
            match text.as_str() {
                "<" => angle_depth += 1,
                ">" => {
                    if angle_depth > 0 {
                        angle_depth -= 1;
                    }
                }
                _ => {}
            }
        }
        tokens.push(Token { kind, text, pos: start });
    }

    Ok(tokens)
}

fn to_lex_error(src: &str, remaining: &str, _err: ErrMode<ContextError>) -> LexError {
    let pos = src.len() - remaining.len();
    LexError { message: "unrecognized token".to_string(), pos }
}

fn ws_and_comments(input: &mut &str) -> ModalResult<()> {
    loop {
        let before = input.len();
        let _: ModalResult<&str> = take_while(1.., char::is_whitespace).parse_next(input);
        if input.starts_with("//") {
            let nl = input.find('\n').unwrap_or(input.len());
            *input = &input[nl..];
            continue;
        }
        if input.starts_with("/*") {
            skip_nested_block_comment(input);
            continue;
        }
        if input.len() == before {
            break;
        }
    }
    Ok(())
}

fn skip_nested_block_comment(input: &mut &str) {
    *input = &input[2..];
    let mut depth = 1i32;
    let bytes = input.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() && depth > 0 {
        if input[i..].starts_with("/*") {
            depth += 1;
            i += 2;
        } else if input[i..].starts_with("*/") {
            depth -= 1;
            i += 2;
        } else {
            i += input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
        }
    }
    *input = &input[i..];
}

fn token_parser<'a>() -> impl Parser<&'a str, TokenKind, ContextError> {
    alt((
        backtick_ident_token,
        triple_quoted_string_token,
        string_token,
        char_literal_token,
        number_token,
        ident_token,
        symbol_token,
    ))
}

fn backtick_ident_token(input: &mut &str) -> ModalResult<TokenKind> {
    if !input.starts_with('`') {
        return Err(backtrack_err());
    }
    let rest = &input[1..];
    let end = rest.find('`').ok_or_else(backtrack_err)?;
    *input = &rest[end + 1..];
    Ok(TokenKind::Ident)
}

fn triple_quoted_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    if !input.starts_with("\"\"\"") {
        return Err(backtrack_err());
    }
    let mut rest = &input[3..];
    scan_template_aware(&mut rest, true)?;
    *input = rest;
    Ok(TokenKind::StringLiteral)
}

fn string_token(input: &mut &str) -> ModalResult<TokenKind> {
    if !input.starts_with('"') {
        return Err(backtrack_err());
    }
    let mut rest = &input[1..];
    scan_template_aware(&mut rest, false)?;
    *input = rest;
    Ok(TokenKind::StringLiteral)
}

/// Scans a string body (already past the opening quote(s)) up to and
/// including its closing quote(s), tracking `${ ... }` template-expression
/// brace depth so an interior `"`/`}` doesn't end the literal early.
fn scan_template_aware(input: &mut &str, triple: bool) -> ModalResult<()> {
    loop {
        if input.is_empty() {
            return Err(backtrack_err());
        }
        if !triple && input.starts_with('\\') {
            *input = &input[input.chars().take(2).map(char::len_utf8).sum::<usize>().max(1)..];
            continue;
        }
        if input.starts_with("${") {
            *input = &input[2..];
            let mut depth = 1i32;
            while depth > 0 {
                if input.is_empty() {
                    return Err(backtrack_err());
                }
                if input.starts_with('{') {
                    depth += 1;
                    *input = &input[1..];
                } else if input.starts_with('}') {
                    depth -= 1;
                    *input = &input[1..];
                } else if input.starts_with('"') {
                    *input = &input[1..];
                    let _ = scan_template_aware(input, false);
                } else {
                    let c = input.chars().next().unwrap();
                    *input = &input[c.len_utf8()..];
                }
            }
            continue;
        }
        if !triple && input.starts_with('"') {
            *input = &input[1..];
            return Ok(());
        }
        if triple && input.starts_with("\"\"\"") {
            *input = &input[3..];
            return Ok(());
        }
        let c = input.chars().next().unwrap();
        *input = &input[c.len_utf8()..];
    }
}

fn char_literal_token(input: &mut &str) -> ModalResult<TokenKind> {
    if !input.starts_with('\'') {
        return Err(backtrack_err());
    }
    let mut rest = &input[1..];
    if rest.starts_with('\\') {
        let skip = rest.chars().take(2).map(char::len_utf8).sum::<usize>();
        rest = &rest[skip..];
    } else {
        let c = rest.chars().next().ok_or_else(backtrack_err)?;
        rest = &rest[c.len_utf8()..];
    }
    if !rest.starts_with('\'') {
        return Err(backtrack_err());
    }
    *input = &rest[1..];
    Ok(TokenKind::StringLiteral)
}

fn number_token(input: &mut &str) -> ModalResult<TokenKind> {
    let s = *input;
    let mut chars = s.char_indices().peekable();
    match chars.peek() {
        Some(&(_, c)) if c.is_ascii_digit() => {}
        _ => return Err(backtrack_err()),
    }
    let mut end = 0usize;
    while let Some(&(idx, c)) = chars.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            end = idx + c.len_utf8();
            chars.next();
        } else if c == '.' && !s[idx..].starts_with("..") {
            if let Some((_, next)) = { let mut it = chars.clone(); it.next(); it.next() } {
                if next.is_ascii_digit() {
                    end = idx + c.len_utf8();
                    chars.next();
                    continue;
                }
            }
            break;
        } else {
            break;
        }
    }
    *input = &s[end..];
    Ok(TokenKind::Number)
}

fn ident_token(input: &mut &str) -> ModalResult<TokenKind> {
    let s = *input;
    let mut chars = s.char_indices();
    match chars.next() {
        Some((_, c)) if c == '_' || c.is_alphabetic() => {}
        _ => return Err(backtrack_err()),
    }
    let mut end = s.chars().next().map(char::len_utf8).unwrap_or(0);
    for (idx, c) in s.char_indices().skip(1) {
        if c == '_' || c.is_alphanumeric() {
            end = idx + c.len_utf8();
        } else {
            break;
        }
    }
    *input = &s[end..];
    Ok(TokenKind::Ident)
}

fn symbol_token(input: &mut &str) -> ModalResult<TokenKind> {
    for sym in TRIPLE_PUNCT {
        if let Some(rest) = input.strip_prefix(sym) {
            *input = rest;
            return Ok(TokenKind::Symbol);
        }
    }
    for sym in DOUBLE_PUNCT {
        if let Some(rest) = input.strip_prefix(sym) {
            *input = rest;
            return Ok(TokenKind::Symbol);
        }
    }
    let c = input.chars().next().ok_or_else(backtrack_err)?;
    if SINGLE_PUNCT.contains(c) {
        *input = &input[c.len_utf8()..];
        return Ok(TokenKind::Symbol);
    }
    Err(backtrack_err())
}

fn backtrack_err() -> ErrMode<ContextError> {
    ErrMode::Backtrack(ContextError::new())
}
