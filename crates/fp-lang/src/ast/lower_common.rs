use crate::lexer::lexeme::Lexeme;
use crate::lexer::tokenizer::Span as LexSpan;
use crate::syntax::{SyntaxElement, SyntaxNode};
use fp_core::ast::{Ident, MacroDelimiter, MacroTokenTree};
use fp_core::module::path::PathPrefix;
use fp_core::span::Span;

pub(crate) struct PathTokens {
    pub(crate) saw_root: bool,
    pub(crate) segments: Vec<Ident>,
    pub(crate) saw_generic_start: bool,
    pub(crate) generic_segment_index: Option<usize>,
}

pub(crate) fn split_path_prefix(
    mut segments: Vec<Ident>,
    saw_root: bool,
) -> (PathPrefix, Vec<Ident>) {
    if saw_root {
        return (PathPrefix::Root, segments);
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

pub(crate) fn collect_path_tokens_with_generics(node: &SyntaxNode) -> PathTokens {
    let mut segments = Vec::new();
    let mut saw_root = false;
    let mut saw_first_token = false;
    let mut saw_generic_start = false;
    let mut generic_segment_index = None;
    let mut generic_depth: i32 = 0;

    for child in &node.children {
        let SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        if !saw_first_token {
            saw_first_token = true;
            if tok.text == "::" {
                saw_root = true;
                continue;
            }
        }
        if tok.text == "::" {
            continue;
        }
        if tok.text == "<" {
            if generic_depth == 0 && !segments.is_empty() {
                saw_generic_start = true;
                if generic_segment_index.is_none() {
                    generic_segment_index = segments.len().checked_sub(1);
                }
            }
            generic_depth += 1;
            continue;
        }
        if tok.text == ">" {
            if generic_depth > 0 {
                generic_depth -= 1;
            }
            continue;
        }
        if tok.text == ">>" {
            if generic_depth > 0 {
                generic_depth = (generic_depth - 2).max(0);
            }
            continue;
        }
        if generic_depth > 0 {
            continue;
        }
        if matches!(tok.text.as_str(), "," | "=") {
            continue;
        }
        if tok
            .text
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '\'')
        {
            segments.push(Ident::new(tok.text.clone()));
        }
    }

    PathTokens {
        saw_root,
        segments,
        saw_generic_start,
        generic_segment_index,
    }
}

pub(crate) fn collect_path_tokens_until_generics(node: &SyntaxNode) -> PathTokens {
    let mut segments = Vec::new();
    let mut saw_root = false;
    let mut saw_first_token = false;
    let mut saw_generic_start = false;

    for child in &node.children {
        let SyntaxElement::Token(tok) = child else {
            continue;
        };
        if tok.is_trivia() {
            continue;
        }
        match tok.text.as_str() {
            "::" if !saw_first_token => {
                saw_root = true;
                saw_first_token = true;
                continue;
            }
            "::" => {
                saw_first_token = true;
                continue;
            }
            "<" => {
                saw_generic_start = true;
                break;
            }
            _ => {
                if tok
                    .text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_' || c == '\'')
                {
                    segments.push(Ident::new(tok.text.clone()));
                }
            }
        }
        saw_first_token = true;
    }

    PathTokens {
        saw_root,
        segments,
        saw_generic_start,
        generic_segment_index: None,
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
                other => {
                    // Conservative fallback: keep the escape as-is.
                    out.push('\\');
                    out.push(other);
                }
            }
        }
        Some(out)
    }

    let raw = raw.strip_prefix('c').unwrap_or(raw);

    // Cooked string literals: "..." and b"..."
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        let inner = &raw[1..raw.len() - 1];
        return unescape_cooked(inner);
    }
    if let Some(rest) = raw.strip_prefix('b') {
        if rest.starts_with('"') && rest.ends_with('"') && rest.len() >= 2 {
            let inner = &rest[1..rest.len() - 1];
            return unescape_cooked(inner);
        }
    }

    // Raw string literals: r"...", r#"..."#, br"...", br#"..."#
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

    // `br"..."` is a byte string in Rust; FerroPhase currently models strings as UTF-8 `&str`.
    // Keep the contents as-is.
    let _ = prefix;
    Some(inner.to_string())
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
