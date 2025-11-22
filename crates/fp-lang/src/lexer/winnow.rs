use winnow::combinator::{alt, cut_err, opt, repeat};
use winnow::error::{ContextError, ErrMode};
use winnow::token::{literal, take_till, take_until, take_while};
use winnow::{ModalResult, Parser};

pub(crate) const MULTI_PUNCT: &[&str] = &[
    "..=", "...", "..", "::", "=>", "->", "==", "!=", "<=", ">=", "&&", "||", "<<", ">>", "+=",
    "-=", "*=", "/=", "%=", "&=", "|=", "^=",
];
pub(crate) const SINGLE_PUNCT: &str = "=+-*/%&|^!~@#$?:;,.()[]{}<>";

pub(crate) fn ws(input: &mut &str) -> ModalResult<()> {
    repeat::<_, _, (), _, _>(0.., alt((whitespace, line_comment, block_comment)))
        .parse_next(input)?;
    Ok(())
}

pub(crate) fn whitespace(input: &mut &str) -> ModalResult<()> {
    take_while(1.., char::is_whitespace)
        .map(|_| ())
        .parse_next(input)
}

pub(crate) fn line_comment(input: &mut &str) -> ModalResult<()> {
    literal("//").parse_next(input)?;
    take_till(0.., |c: char| c == '\n').parse_next(input)?;
    opt(literal("\n")).parse_next(input)?;
    Ok(())
}

pub(crate) fn block_comment(input: &mut &str) -> ModalResult<()> {
    literal("/*").parse_next(input)?;
    cut_err(take_until(0.., "*/")).parse_next(input)?;
    literal("*/").parse_next(input)?;
    Ok(())
}

pub(crate) fn parse_cooked_string_literal(input: &mut &str, prefix: &str) -> ModalResult<String> {
    if !input.starts_with(prefix) {
        return Err(backtrack_err());
    }
    let slice = *input;
    let start = prefix.len();
    if slice[start..].chars().next().map_or(true, |c| c != '"') {
        return Err(backtrack_err());
    }
    let bytes = slice.as_bytes();
    let mut idx = start + 1;
    let mut escape = false;
    while idx < bytes.len() {
        let b = bytes[idx];
        idx += 1;
        if b == b'\\' && !escape {
            escape = true;
            continue;
        }
        if b == b'"' && !escape {
            let literal = slice[..idx].to_string();
            *input = &slice[idx..];
            return Ok(literal);
        }
        escape = false;
    }
    Err(ErrMode::Cut(ContextError::new()))
}

pub(crate) fn parse_raw_string_literal(input: &mut &str, byte: bool) -> ModalResult<String> {
    let slice = *input;
    if slice.is_empty() {
        return Err(backtrack_err());
    }
    let bytes = slice.as_bytes();
    let mut idx = if byte {
        if slice.starts_with("br") {
            2
        } else {
            return Err(backtrack_err());
        }
    } else if slice.starts_with('r') {
        1
    } else {
        return Err(backtrack_err());
    };

    let mut hashes = 0usize;
    while idx < bytes.len() && bytes[idx] == b'#' {
        hashes += 1;
        idx += 1;
    }
    if idx >= bytes.len() || bytes[idx] != b'"' {
        return Err(backtrack_err());
    }
    idx += 1; // opening quote
    let mut cursor = idx;
    while cursor < bytes.len() {
        if bytes[cursor] == b'"' {
            if matches_hashes(bytes, cursor + 1, hashes) {
                let end = cursor + 1 + hashes;
                let literal = slice[..end].to_string();
                *input = &slice[end..];
                return Ok(literal);
            }
        }
        cursor += 1;
    }
    Err(ErrMode::Cut(ContextError::new()))
}

pub(crate) fn parse_raw_identifier(input: &mut &str) -> ModalResult<String> {
    let original = *input;
    literal("r#").parse_next(input)?;
    take_while(1.., is_ident_start).parse_next(input)?;
    take_while(0.., is_ident_continue).parse_next(input)?;
    let consumed = original.len() - input.len();
    Ok(original[..consumed].to_string())
}

pub(crate) fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

pub(crate) fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

pub(crate) fn is_delimiter(ch: char) -> bool {
    "{}()[];,+-*/=:.".contains(ch)
}

pub(crate) fn backtrack_err() -> ErrMode<ContextError> {
    ErrMode::Backtrack(ContextError::new())
}

fn matches_hashes(bytes: &[u8], start: usize, count: usize) -> bool {
    if start + count > bytes.len() {
        return false;
    }
    for idx in 0..count {
        if bytes[start + idx] != b'#' {
            return false;
        }
    }
    true
}
