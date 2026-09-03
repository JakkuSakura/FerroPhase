use crate::lexer::lexeme::Lexeme;
use crate::lexer::tokenizer::Span as LexSpan;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::{Ident, MacroDelimiter, MacroTokenTree, PathSegment};
use fp_core::span::Span;

pub(crate) fn split_path_prefix(
    mut segments: Vec<Ident>,
    saw_root: bool,
) -> (PathPrefix, Vec<Ident>) {
    if saw_root {
        return (PathPrefix::Root, segments);
    }
    // A bare single-segment `self`/`crate`/`super` (no trailing `::segment`) is a
    // value-position identifier (e.g. the method receiver `self`), not a module-path
    // prefix — only treat these as prefixes when they actually lead into a longer path.
    if segments.len() < 2 {
        return (PathPrefix::Plain, segments);
    }
    let Some(first) = segments.first().map(|ident| ident.as_str()) else {
        return (PathPrefix::Plain, segments);
    };
    match first {
        "crate" => {
            segments.remove(0);
            (PathPrefix::Crate, segments)
        }
        "self" => {
            segments.remove(0);
            (PathPrefix::SelfMod, segments)
        }
        "super" => {
            let mut depth = 0;
            while segments
                .first()
                .is_some_and(|ident| ident.as_str() == "super")
            {
                segments.remove(0);
                depth += 1;
            }
            (PathPrefix::Super(depth), segments)
        }
        _ => (PathPrefix::Plain, segments),
    }
}

pub(crate) fn split_path_prefix_segments(
    mut segments: Vec<PathSegment>,
    saw_root: bool,
) -> (PathPrefix, Vec<PathSegment>) {
    if saw_root {
        return (PathPrefix::Root, segments);
    }
    // See split_path_prefix: a bare single-segment `self`/`crate`/`super` is a
    // value-position identifier, not a module-path prefix.
    if segments.len() < 2 {
        return (PathPrefix::Plain, segments);
    }
    let Some(first) = segments.first().map(|seg| seg.ident.as_str()) else {
        return (PathPrefix::Plain, segments);
    };
    match first {
        "crate" => {
            segments.remove(0);
            (PathPrefix::Crate, segments)
        }
        "self" => {
            segments.remove(0);
            (PathPrefix::SelfMod, segments)
        }
        "super" => {
            let mut depth = 0;
            while segments
                .first()
                .is_some_and(|seg| seg.ident.as_str() == "super")
            {
                segments.remove(0);
                depth += 1;
            }
            (PathPrefix::Super(depth), segments)
        }
        _ => (PathPrefix::Plain, segments),
    }
}

/// Decode a single-character/byte literal's escaped inner text (e.g. the `x` in
/// `'x'`, or `\n` in `'\n'`) into one `char`. Returns `None` if the escape isn't
/// recognized or the literal doesn't contain exactly one character/escape.
pub(crate) fn decode_single_char_literal(inner: &str) -> Option<char> {
    let mut chars = inner.chars();
    let first = chars.next()?;
    if first != '\\' {
        return if chars.next().is_none() {
            Some(first)
        } else {
            None
        };
    }
    let esc = chars.next()?;
    let decoded = match esc {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        '\\' => '\\',
        '\'' => '\'',
        '"' => '"',
        'x' => {
            let hex: String = chars.by_ref().take(2).collect();
            u8::from_str_radix(&hex, 16).ok()? as char
        }
        _ => return None,
    };
    if chars.next().is_none() {
        Some(decoded)
    } else {
        None
    }
}

pub(crate) fn decode_string_literal(raw: &str) -> Option<String> {
    fn unescape_cooked(s: &str) -> Option<String> {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c != '\\' {
                out.push(c);
                continue;
            }
            let esc = chars.next()?;
            match esc {
                'n' => out.push('\n'),
                'r' => out.push('\r'),
                't' => out.push('\t'),
                '0' => out.push('\0'),
                '\\' => out.push('\\'),
                '"' => out.push('"'),
                // `\xHH` in a `&str`/`char` literal — like real Rust, only
                // ASCII (<=0x7F) is a valid single-byte-equals-one-char
                // escape here; a byte-string literal's escape (which can
                // use the full 0x00-0xFF range) is decoded separately by
                // `decode_bytes_literal` below.
                'x' => {
                    let hi = chars.next()?.to_digit(16)?;
                    let lo = chars.next()?.to_digit(16)?;
                    let byte = (hi * 16 + lo) as u8;
                    if byte > 0x7F {
                        return None;
                    }
                    out.push(byte as char);
                }
                other => {
                    out.push('\\');
                    out.push(other);
                }
            }
        }
        Some(out)
    }

    let raw = raw.strip_prefix('c').unwrap_or(raw);
    if raw.starts_with('\'') && raw.ends_with('\'') && raw.len() >= 2 {
        let inner = &raw[1..raw.len() - 1];
        return unescape_cooked(inner);
    }
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        let inner = &raw[1..raw.len() - 1];
        return unescape_cooked(inner);
    }
    if let Some(rest) = raw.strip_prefix('b') {
        if rest.starts_with('\'') && rest.ends_with('\'') && rest.len() >= 2 {
            let inner = &rest[1..rest.len() - 1];
            return unescape_cooked(inner);
        }
        if rest.starts_with('"') && rest.ends_with('"') && rest.len() >= 2 {
            let inner = &rest[1..rest.len() - 1];
            return unescape_cooked(inner);
        }
    }

    let (prefix, rest) = if let Some(r) = raw.strip_prefix("br") {
        ("br", r)
    } else if let Some(r) = raw.strip_prefix('r') {
        ("r", r)
    } else {
        return None;
    };
    let hash_count = rest.chars().take_while(|c| *c == '#').count();
    let after_hashes = &rest[hash_count..];
    let Some(after_quote) = after_hashes.strip_prefix('"') else {
        return None;
    };
    let closing = format!("\"{}", "#".repeat(hash_count));
    let Some(end_idx) = after_quote.rfind(&closing) else {
        return None;
    };
    if end_idx + closing.len() != after_quote.len() {
        return None;
    }
    let inner = &after_quote[..end_idx];
    let _ = prefix;
    Some(inner.to_string())
}

/// Decodes a `b"..."`/`c"..."` literal's raw lexeme into its byte content
/// (not a `String` — unlike `decode_string_literal`, the full 0x00-0xFF
/// range is valid here, which isn't always valid UTF-8). Does not append
/// `c"..."`'s implicit trailing NUL; callers needing it add it themselves.
pub(crate) fn decode_bytes_literal(raw: &str) -> Option<Vec<u8>> {
    fn unescape_bytes(s: &str) -> Option<Vec<u8>> {
        let mut out = Vec::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c != '\\' {
                let mut buf = [0u8; 4];
                out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                continue;
            }
            let esc = chars.next()?;
            match esc {
                'n' => out.push(b'\n'),
                'r' => out.push(b'\r'),
                't' => out.push(b'\t'),
                '0' => out.push(0),
                '\\' => out.push(b'\\'),
                '"' => out.push(b'"'),
                '\'' => out.push(b'\''),
                'x' => {
                    let hi = chars.next()?.to_digit(16)?;
                    let lo = chars.next()?.to_digit(16)?;
                    out.push((hi * 16 + lo) as u8);
                }
                other => {
                    out.push(b'\\');
                    let mut buf = [0u8; 4];
                    out.extend_from_slice(other.encode_utf8(&mut buf).as_bytes());
                }
            }
        }
        Some(out)
    }

    if let Some(rest) = raw.strip_prefix("br") {
        // Raw byte string (`br"..."`/`br#"..."#`/`br##"..."##`/...) — no
        // escape processing at all, same as `decode_string_literal`'s `br`
        // handling, just returning bytes instead of a `String`.
        let hash_count = rest.chars().take_while(|c| *c == '#').count();
        let after_hashes = &rest[hash_count..];
        let after_quote = after_hashes.strip_prefix('"')?;
        let closing = format!("\"{}", "#".repeat(hash_count));
        let end_idx = after_quote.rfind(&closing)?;
        if end_idx + closing.len() != after_quote.len() {
            return None;
        }
        return Some(after_quote[..end_idx].as_bytes().to_vec());
    }

    let rest = raw.strip_prefix('b').or_else(|| raw.strip_prefix('c'))?;
    if rest.starts_with('\'') && rest.ends_with('\'') && rest.len() >= 2 {
        return unescape_bytes(&rest[1..rest.len() - 1]);
    }
    if rest.starts_with('"') && rest.ends_with('"') && rest.len() >= 2 {
        return unescape_bytes(&rest[1..rest.len() - 1]);
    }
    None
}

pub(crate) fn macro_token_trees_to_lexemes(tokens: &[MacroTokenTree]) -> Vec<Lexeme> {
    let mut out = Vec::new();
    append_macro_lexemes(tokens, &mut out);
    out
}

pub(crate) fn macro_tokens_file_id(tokens: &[MacroTokenTree]) -> u64 {
    for tree in tokens {
        if let Some(file) = token_tree_file(tree) {
            return file;
        }
    }
    0
}

pub(crate) fn lex_span_from_span(span: Span) -> LexSpan {
    LexSpan {
        start: span.lo as usize,
        end: span.hi as usize,
    }
}

pub(crate) fn lex_spans_for_group(span: Span) -> (LexSpan, LexSpan) {
    let open_start = span.lo;
    let open_end = if span.hi > span.lo {
        span.lo.saturating_add(1)
    } else {
        span.lo
    };
    let close_start = span.hi.saturating_sub(1);
    let close_end = span.hi;
    (
        LexSpan {
            start: open_start as usize,
            end: open_end as usize,
        },
        LexSpan {
            start: close_start as usize,
            end: close_end as usize,
        },
    )
}

fn append_macro_lexemes(tokens: &[MacroTokenTree], out: &mut Vec<Lexeme>) {
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => {
                if tok.text == "::<" {
                    let span = lex_span_from_span(tok.span);
                    out.push(Lexeme::token("::".to_string(), span));
                    out.push(Lexeme::token("<".to_string(), span));
                } else {
                    out.push(Lexeme::token(
                        tok.text.clone(),
                        lex_span_from_span(tok.span),
                    ));
                }
            }
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    MacroDelimiter::Parenthesis => ("(", ")"),
                    MacroDelimiter::Bracket => ("[", "]"),
                    MacroDelimiter::Brace => ("{", "}"),
                };
                let (open_span, close_span) = lex_spans_for_group(group.span);
                out.push(Lexeme::token(open.to_string(), open_span));
                append_macro_lexemes(&group.tokens, out);
                out.push(Lexeme::token(close.to_string(), close_span));
            }
        }
    }
}

fn token_tree_file(tree: &MacroTokenTree) -> Option<u64> {
    match tree {
        MacroTokenTree::Token(tok) => Some(tok.span.file),
        MacroTokenTree::Group(group) => {
            if group.span.file != 0 {
                return Some(group.span.file);
            }
            for inner in &group.tokens {
                if let Some(file) = token_tree_file(inner) {
                    return Some(file);
                }
            }
            None
        }
    }
}
