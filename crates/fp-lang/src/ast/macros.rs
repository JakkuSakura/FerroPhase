use fp_core::ast::{MacroDelimiter, MacroGroup, MacroToken, MacroTokenTree};
use fp_core::span::Span;

use crate::syntax::{collect_tokens, SyntaxNode, SyntaxToken};

pub(crate) struct MacroTokens {
    pub delimiter: MacroDelimiter,
    pub token_trees: Vec<MacroTokenTree>,
    pub text: String,
}

pub(crate) fn macro_group_tokens(node: &SyntaxNode) -> Option<MacroTokens> {
    // Find the first delimiter token after '!'.
    let mut seen_bang = false;
    let mut open_idx = None;
    let mut close_idx = None;
    let mut tokens: Vec<&SyntaxToken> = Vec::new();
    collect_tokens(node, &mut tokens);
    for (idx, tok) in tokens.iter().enumerate() {
        if tok.is_trivia() {
            continue;
        }
        if !seen_bang {
            if tok.text == "!" {
                seen_bang = true;
            }
            continue;
        }
        if open_idx.is_none() {
            if matches!(tok.text.as_str(), "(" | "{" | "[") {
                open_idx = Some(idx);
            }
            continue;
        }
        if matches!(tok.text.as_str(), ")" | "}" | "]") {
            close_idx = Some(idx);
        }
    }

    let open_idx = open_idx?;
    let close_idx = close_idx?;
    let open = tokens[open_idx].text.as_str();
    let delimiter = match open {
        "(" => MacroDelimiter::Parenthesis,
        "{" => MacroDelimiter::Brace,
        "[" => MacroDelimiter::Bracket,
        _ => return None,
    };

    let inner_tokens: Vec<&SyntaxToken> = tokens[(open_idx + 1)..close_idx]
        .iter()
        .copied()
        .filter(|tok| !tok.is_trivia())
        .collect();
    let file = node.span.file;
    let token_trees = parse_token_trees(&inner_tokens, 0, None, file).0;
    let text = token_trees_to_text(&token_trees);

    Some(MacroTokens {
        delimiter,
        token_trees,
        text,
    })
}

fn parse_token_trees(
    tokens: &[&SyntaxToken],
    mut idx: usize,
    stop: Option<&str>,
    file: fp_core::span::FileId,
) -> (Vec<MacroTokenTree>, usize) {
    let mut out = Vec::new();
    while idx < tokens.len() {
        let tok = tokens[idx];
        if let Some(stop) = stop {
            if tok.text == stop {
                idx += 1;
                break;
            }
        }
        match tok.text.as_str() {
            "(" | "{" | "[" => {
                let (delimiter, close) = match tok.text.as_str() {
                    "(" => (MacroDelimiter::Parenthesis, ")"),
                    "{" => (MacroDelimiter::Brace, "}"),
                    "[" => (MacroDelimiter::Bracket, "]"),
                    _ => unreachable!(),
                };
                let open_span = span_with_file(tok.span, file);
                let (inner, next_idx) = parse_token_trees(tokens, idx + 1, Some(close), file);
                let close_span = if next_idx > 0 && next_idx - 1 < tokens.len() {
                    span_with_file(tokens[next_idx - 1].span, file)
                } else {
                    span_with_file(tok.span, file)
                };
                let span = Span::union(
                    [open_span, close_span]
                        .into_iter()
                        .chain(inner.iter().map(token_tree_span)),
                );
                out.push(MacroTokenTree::Group(MacroGroup {
                    delimiter,
                    tokens: inner,
                    span,
                }));
                idx = next_idx;
            }
            ")" | "}" | "]" => {
                // Unbalanced; stop to avoid infinite loop.
                break;
            }
            _ => {
                out.push(MacroTokenTree::Token(MacroToken {
                    text: tok.text.clone(),
                    span: span_with_file(tok.span, file),
                }));
                idx += 1;
            }
        }
    }
    (out, idx)
}

fn token_trees_to_text(tokens: &[MacroTokenTree]) -> String {
    fn is_ident_like(text: &str) -> bool {
        text.chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
    }
    fn needs_space(prev: &str, next: &str) -> bool {
        is_ident_like(prev) && is_ident_like(next)
    }

    let mut out = String::new();
    let mut prev: Option<String> = None;
    for token in flatten_tokens(tokens) {
        if let Some(prev_text) = prev.as_deref() {
            if needs_space(prev_text, token.as_str()) {
                out.push(' ');
            }
        }
        out.push_str(&token);
        prev = Some(token);
    }
    out
}

fn flatten_tokens(tokens: &[MacroTokenTree]) -> Vec<String> {
    let mut out = Vec::new();
    for token in tokens {
        match token {
            MacroTokenTree::Token(tok) => out.push(tok.text.clone()),
            MacroTokenTree::Group(group) => {
                let (open, close) = match group.delimiter {
                    MacroDelimiter::Parenthesis => ("(", ")"),
                    MacroDelimiter::Bracket => ("[", "]"),
                    MacroDelimiter::Brace => ("{", "}"),
                };
                out.push(open.to_string());
                out.extend(flatten_tokens(&group.tokens));
                out.push(close.to_string());
            }
        }
    }
    out
}

fn token_tree_span(tree: &MacroTokenTree) -> Span {
    match tree {
        MacroTokenTree::Token(tok) => tok.span,
        MacroTokenTree::Group(group) => group.span,
    }
}

fn span_with_file(span: Span, file: fp_core::span::FileId) -> Span {
    if span.file == 0 && !span.is_null() && file != 0 {
        Span::new(file, span.lo, span.hi)
    } else {
        span
    }
}
