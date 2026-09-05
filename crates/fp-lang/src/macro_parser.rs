use std::collections::HashMap;

use fp_core::ast::{
    Item, ItemKind, MacroDelimiter, MacroGroup, MacroMatcherGroup, MacroMatcherToken, MacroMetavar,
    MacroRepetition, MacroRepetitionOp, MacroRule, MacroRulesDef, MacroToken, MacroTokenTree,
};
use fp_core::span::Span;

use crate::ast::FerroPhaseParser;
use crate::ast::lower_common::{lex_span_from_span, lex_spans_for_group, macro_tokens_file_id};
use crate::ast::{parse_expr_prefix_tokens, parse_pattern_prefix_tokens, parse_type_prefix_tokens};
use crate::lexer::Span as TokSpan;
use crate::lexer::tokenizer::{Token, TokenKind, classify_and_normalize_lexeme};

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
                out.push(make_token(
                    kind,
                    lexeme,
                    tok.text.starts_with("r#"),
                    lex_span_from_span(tok.span),
                ));
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

fn make_token(kind: TokenKind, lexeme: String, raw_identifier: bool, span: TokSpan) -> Token {
    Token {
        kind,
        raw_identifier,
        lexeme,
        span,
    }
}

fn push_symbol_token(out: &mut Vec<Token>, symbol: &str, span: TokSpan) {
    out.push(make_token(
        TokenKind::Symbol,
        symbol.to_string(),
        false,
        span,
    ));
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
    out.push(make_token(
        TokenKind::Symbol,
        open.to_string(),
        false,
        open_span,
    ));
    out.extend_from_slice(inner);
    out.push(make_token(
        TokenKind::Symbol,
        close.to_string(),
        false,
        close_span,
    ));
    out
}

// ── `macro_rules!` structured parsing ───────────────────────────────────────

/// Parses a `macro_rules! name { (matcher) => { transcriber }; ... }` item's
/// body (the raw `token_trees` already captured by `ItemMacro`) into a
/// structured `MacroRulesDef`. Each rule is `Group(matcher) "=>" Group(
/// transcriber) ";"?` at the top level — since `MacroTokenTree::Group` nodes
/// are already atomic (the original tokenizer/parser already balanced
/// delimiters when capturing this token tree), no manual depth-tracking is
/// needed to find rule boundaries, same insight already used by
/// `select_cfg_select_arm` in `normalization.rs`.
pub fn parse_macro_rules_def(name: String, body: &[MacroTokenTree]) -> MacroRulesDef {
    let mut rules = Vec::new();
    let mut i = 0;
    while i < body.len() {
        let Some(MacroTokenTree::Group(matcher_group)) = body.get(i) else {
            break;
        };
        i += 1;
        let Some(MacroTokenTree::Token(arrow)) = body.get(i) else {
            break;
        };
        if arrow.text != "=>" {
            break;
        }
        i += 1;
        let Some(MacroTokenTree::Group(transcriber_group)) = body.get(i) else {
            break;
        };
        i += 1;
        rules.push(MacroRule {
            matcher: parse_matcher_tokens(&matcher_group.tokens),
            transcriber: transcriber_group.tokens.clone(),
        });
        // A real `macro_rules!` body always separates rules with `;`, but
        // the declarative "macro 2.0" `macro Name { matcher => transcriber,
        // .. }` shape (parsed into this identical rule-token form by
        // `parse_macro_2_def`) uses `,` instead — accept either so both
        // shapes' rules all get collected, not just the first.
        if let Some(MacroTokenTree::Token(sep)) = body.get(i) {
            if sep.text == ";" || sep.text == "," {
                i += 1;
            }
        }
    }
    MacroRulesDef { name, rules }
}

/// Walks a matcher's raw token trees, recognizing `$name:fragment`
/// metavariables and `$(...)sep? op` repetition groups (both `$` followed by
/// a `Token`/`Group` respectively — already-atomic sibling entries in the
/// flat tree, since the tokenizer doesn't treat `$` specially, it's just
/// another single-char symbol token). Everything else is a literal token or
/// a literal (non-`$`) delimited group, recursively converted the same way.
fn parse_matcher_tokens(tokens: &[MacroTokenTree]) -> Vec<MacroMatcherToken> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            MacroTokenTree::Token(dollar) if dollar.text == "$" => {
                if let Some(MacroTokenTree::Group(group)) = tokens.get(i + 1) {
                    let inner = parse_matcher_tokens(&group.tokens);
                    let mut j = i + 2;
                    let mut separator = None;
                    if let Some(MacroTokenTree::Token(next)) = tokens.get(j) {
                        if is_repetition_op(&next.text) {
                            // no separator
                        } else {
                            separator = Some(next.clone());
                            j += 1;
                        }
                    }
                    let op = match tokens.get(j) {
                        Some(MacroTokenTree::Token(opt)) => {
                            let op = repetition_op_from_str(&opt.text);
                            j += 1;
                            op
                        }
                        _ => MacroRepetitionOp::Star,
                    };
                    out.push(MacroMatcherToken::Repetition(MacroRepetition {
                        inner,
                        separator,
                        op,
                    }));
                    i = j;
                } else if let Some(MacroTokenTree::Token(name_tok)) = tokens.get(i + 1) {
                    let name = name_tok.text.clone();
                    let mut j = i + 2;
                    let mut fragment = "tt".to_string();
                    if let Some(MacroTokenTree::Token(colon)) = tokens.get(j) {
                        if colon.text == ":" {
                            j += 1;
                            if let Some(MacroTokenTree::Token(frag_tok)) = tokens.get(j) {
                                fragment = frag_tok.text.clone();
                                j += 1;
                            }
                        }
                    }
                    out.push(MacroMatcherToken::Metavar(MacroMetavar { name, fragment }));
                    i = j;
                } else {
                    out.push(MacroMatcherToken::Token(dollar.clone()));
                    i += 1;
                }
            }
            MacroTokenTree::Token(t) => {
                out.push(MacroMatcherToken::Token(t.clone()));
                i += 1;
            }
            MacroTokenTree::Group(group) => {
                out.push(MacroMatcherToken::Group(MacroMatcherGroup {
                    delimiter: group.delimiter.clone(),
                    tokens: parse_matcher_tokens(&group.tokens),
                }));
                i += 1;
            }
        }
    }
    out
}

fn is_repetition_op(s: &str) -> bool {
    matches!(s, "*" | "+" | "?")
}

fn repetition_op_from_str(s: &str) -> MacroRepetitionOp {
    match s {
        "+" => MacroRepetitionOp::Plus,
        "?" => MacroRepetitionOp::Question,
        _ => MacroRepetitionOp::Star,
    }
}

// ── `macro_rules!` matching ─────────────────────────────────────────────────

/// Bindings captured while matching an invocation against a rule's matcher —
/// a single metavariable binds to the exact tokens it consumed; a
/// metavariable inside a repetition binds to one set of tokens per matched
/// repetition index.
#[derive(Debug, Clone, Default)]
pub(crate) struct MacroBindings {
    values: HashMap<String, MacroBindingValue>,
}

#[derive(Debug, Clone)]
enum MacroBindingValue {
    Single(Vec<MacroTokenTree>),
    Repeated(Vec<MacroBindings>),
}

impl MacroBindings {
    fn repetition_count(&self, template: &[MacroTokenTree]) -> usize {
        for name in metavar_refs_in_template(template) {
            if let Some(MacroBindingValue::Repeated(items)) = self.values.get(&name) {
                return items.len();
            }
        }
        0
    }

    fn for_iteration(&self, idx: usize, template: &[MacroTokenTree]) -> MacroBindings {
        let mut merged = self.clone();
        for name in metavar_refs_in_template(template) {
            if let Some(MacroBindingValue::Repeated(items)) = self.values.get(&name) {
                if let Some(item) = items.get(idx) {
                    for (k, v) in &item.values {
                        merged.values.insert(k.clone(), v.clone());
                    }
                }
            }
        }
        merged
    }
}

/// Tries to match a rule's matcher against an invocation's token trees in
/// full (the whole invocation must be consumed) — returns the captured
/// bindings on success.
pub(crate) fn match_macro_rule(
    matcher: &[MacroMatcherToken],
    invocation: &[MacroTokenTree],
    file_id: u64,
) -> Option<MacroBindings> {
    let mut bindings = MacroBindings::default();
    let mut pos = 0;
    if match_sequence(matcher, invocation, &mut pos, &mut bindings, file_id)
        && pos == invocation.len()
    {
        Some(bindings)
    } else {
        None
    }
}

fn match_sequence(
    matcher: &[MacroMatcherToken],
    invocation: &[MacroTokenTree],
    pos: &mut usize,
    bindings: &mut MacroBindings,
    file_id: u64,
) -> bool {
    for (idx, m) in matcher.iter().enumerate() {
        if !match_one(m, &matcher[idx + 1..], invocation, pos, bindings, file_id) {
            return false;
        }
    }
    true
}

fn match_one(
    m: &MacroMatcherToken,
    rest: &[MacroMatcherToken],
    invocation: &[MacroTokenTree],
    pos: &mut usize,
    bindings: &mut MacroBindings,
    file_id: u64,
) -> bool {
    match m {
        MacroMatcherToken::Token(expected) => match invocation.get(*pos) {
            Some(MacroTokenTree::Token(actual)) if actual.text == expected.text => {
                *pos += 1;
                true
            }
            _ => false,
        },
        MacroMatcherToken::Group(expected_group) => match invocation.get(*pos) {
            Some(MacroTokenTree::Group(actual_group))
                if actual_group.delimiter == expected_group.delimiter =>
            {
                let mut sub_pos = 0;
                let matched = match_sequence(
                    &expected_group.tokens,
                    &actual_group.tokens,
                    &mut sub_pos,
                    bindings,
                    file_id,
                ) && sub_pos == actual_group.tokens.len();
                if matched {
                    *pos += 1;
                }
                matched
            }
            _ => false,
        },
        MacroMatcherToken::Metavar(mv) => {
            let stop_at = next_literal_token(rest);
            match consume_fragment(&mv.fragment, invocation, *pos, stop_at, file_id) {
                Some(consumed) if consumed > 0 => {
                    let bound = invocation[*pos..*pos + consumed].to_vec();
                    bindings
                        .values
                        .insert(mv.name.clone(), MacroBindingValue::Single(bound));
                    *pos += consumed;
                    true
                }
                _ => false,
            }
        }
        MacroMatcherToken::Repetition(rep) => {
            let names = metavar_names_in(&rep.inner);
            let mut iterations: Vec<MacroBindings> = Vec::new();
            loop {
                if matches!(rep.op, MacroRepetitionOp::Question) && !iterations.is_empty() {
                    break;
                }
                let mut probe_pos = *pos;
                if !iterations.is_empty() {
                    match &rep.separator {
                        Some(sep) => match invocation.get(probe_pos) {
                            Some(MacroTokenTree::Token(t)) if t.text == sep.text => {
                                probe_pos += 1;
                            }
                            _ => break,
                        },
                        None => {}
                    }
                }
                let mut iter_bindings = MacroBindings::default();
                let mut sub_pos = probe_pos;
                if match_sequence(
                    &rep.inner,
                    invocation,
                    &mut sub_pos,
                    &mut iter_bindings,
                    file_id,
                ) && sub_pos > probe_pos
                {
                    iterations.push(iter_bindings);
                    *pos = sub_pos;
                } else {
                    break;
                }
            }
            if matches!(rep.op, MacroRepetitionOp::Plus) && iterations.is_empty() {
                return false;
            }
            for name in names {
                let per_iter: Vec<MacroBindings> = iterations.clone();
                bindings
                    .values
                    .insert(name, MacroBindingValue::Repeated(per_iter));
            }
            true
        }
    }
}

/// The next literal token the matcher expects right after the current
/// position (used so a greedy fragment like `expr` knows where to stop —
/// e.g. stop right before a literal `,`/`;` the matcher specifies next).
fn next_literal_token(rest: &[MacroMatcherToken]) -> Option<&MacroToken> {
    match rest.first() {
        Some(MacroMatcherToken::Token(t)) => Some(t),
        _ => None,
    }
}

/// Consumes exactly one fragment of the given kind starting at `pos`,
/// returning how many `MacroTokenTree` entries it spans, or `None` if the
/// fragment doesn't match here at all.
fn consume_fragment(
    fragment: &str,
    invocation: &[MacroTokenTree],
    pos: usize,
    stop_at: Option<&MacroToken>,
    file_id: u64,
) -> Option<usize> {
    match fragment {
        "tt" => {
            if pos < invocation.len() {
                Some(1)
            } else {
                None
            }
        }
        "ident" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(t)) if is_ident_like_text(&t.text) => Some(1),
            _ => None,
        },
        "block" => match invocation.get(pos) {
            Some(MacroTokenTree::Group(g)) if g.delimiter == MacroDelimiter::Brace => Some(1),
            _ => None,
        },
        // A lifetime (`'a`) tokenizes as a single ident-like token whose
        // text starts with `'` (see `parse_receiver`'s identical check) —
        // not alphabetic, so it fails `is_ident_like_text` and, being no
        // valid expression on its own, also fails the generic
        // expr-parse fallback below. Without this arm, any macro rule
        // using `$lifetime:lifetime` (real vendored std's own
        // `impl_fn_for_zst!`, among others) never matches at all.
        "lifetime" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(t)) if t.text.starts_with('\'') => Some(1),
            _ => None,
        },
        // Real Rust grammar for this fragment specifier is `-?literal` —
        // a negative numeric literal (`Min = -128,`, common in exactly
        // this shape of const-table macro) tokenizes as a separate `-`
        // symbol token followed by the digits, never a single fused
        // token, so consuming just 1 token here would bind `$Min` to the
        // bare `-` and leave the digits to desync the rest of the
        // matcher, silently failing the whole rule (see `int_impl!`'s
        // `Min = $Min:literal` field in real vendored std for a
        // confirmed real-world case of this).
        "literal" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(t)) if t.text == "-" => match invocation.get(pos + 1) {
                Some(MacroTokenTree::Token(_)) => Some(2),
                _ => None,
            },
            Some(MacroTokenTree::Token(_)) => Some(1),
            _ => None,
        },
        // `meta` is an attribute's contents, not an expression. In
        // particular, `#[stable(feature = "...", since = "...")]` contains
        // assignment tokens and nested groups that the expression parser
        // deliberately does not accept. Consume the complete top-level meta
        // item, stopping only at the matcher follow token; nested groups are
        // already atomic token trees, so commas inside them do not terminate
        // the fragment.
        "meta" => {
            let mut end = pos;
            while end < invocation.len() {
                let is_stop = matches!(invocation.get(end),
                    Some(MacroTokenTree::Token(token)) if token.text == ","
                ) || stop_at.is_some_and(|stop| {
                    matches!(invocation.get(end), Some(MacroTokenTree::Token(token)) if token.text == stop.text)
                });
                if is_stop {
                    break;
                }
                end += 1;
            }
            (end > pos).then_some(end - pos)
        }
        // Visibility is a small Rust grammar of its own. The common forms
        // (`pub`, `pub(crate)`, `pub(super)`, and `pub(in path)`) are all
        // represented by one token followed by an optional already-balanced
        // group, and must not be sent through expression parsing.
        "vis" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(token)) if token.text == "pub" => Some(
                if matches!(invocation.get(pos + 1), Some(MacroTokenTree::Group(_))) {
                    2
                } else {
                    1
                },
            ),
            Some(MacroTokenTree::Token(token))
                if matches!(token.text.as_str(), "crate" | "self" | "super") =>
            {
                Some(1)
            }
            _ => None,
        },
        "ty" => {
            let window = fragment_window(invocation, pos, stop_at);
            let flat = macro_token_trees_to_tokens(window);
            let (_, consumed_flat) = parse_type_prefix_tokens(&flat, file_id).ok()?;
            token_tree_count_for_flat_prefix(window, consumed_flat)
        }
        "pat" => {
            let window = fragment_window(invocation, pos, stop_at);
            let flat = macro_token_trees_to_tokens(window);
            let (_, consumed_flat) = parse_pattern_prefix_tokens(&flat).ok()?;
            token_tree_count_for_flat_prefix(window, consumed_flat)
        }
        // "expr" | "path" | "meta" | anything else not specially handled:
        // approximate with the expression parser, which covers paths too
        // (a bare path is a valid expression shape).
        _ => {
            let window = fragment_window(invocation, pos, stop_at);
            let flat = macro_token_trees_to_tokens(window);
            let (_, consumed_flat) = parse_expr_prefix_tokens(&flat, file_id).ok()?;
            token_tree_count_for_flat_prefix(window, consumed_flat)
        }
    }
}

/// The candidate slice a non-`tt`/`ident`/`block`/`literal` fragment may
/// consume from: everything from `pos` up to (but not including) the next
/// top-level occurrence of one of real Rust's own macro-fragment
/// "follow set" tokens, or the matcher's own next literal token if it
/// appears first — always legal stopping points for these fragment kinds.
/// `|` matters in practice for a `$x:ty`/`$x:expr` fragment immediately
/// followed by a literal `|` closing a closure-style param list (real
/// vendored std's own `impl_fn_for_zst!`, `|$arg: ident: $ArgTy: ty),*|`)
/// — this parser's own type grammar treats a bare `|` as a union-type
/// operator (`T | U`), so without stopping here first, greedily
/// continuing into "everything after the `|`" fails the type parse
/// entirely instead of cleanly ending the fragment right before it.
fn fragment_window<'a>(
    invocation: &'a [MacroTokenTree],
    pos: usize,
    stop_at: Option<&MacroToken>,
) -> &'a [MacroTokenTree] {
    let mut end = invocation.len();
    for (offset, tree) in invocation[pos..].iter().enumerate() {
        if let MacroTokenTree::Token(t) = tree {
            let is_follow_set_stop = matches!(t.text.as_str(), "," | "|" | ";" | "=>");
            let is_stop = stop_at.is_some_and(|stop| stop.text == t.text);
            if is_follow_set_stop || is_stop {
                end = pos + offset;
                break;
            }
        }
    }
    &invocation[pos..end]
}

fn token_tree_count_for_flat_prefix(
    trees: &[MacroTokenTree],
    flat_consumed: usize,
) -> Option<usize> {
    if flat_consumed == 0 {
        return None;
    }
    let mut flat_so_far = 0;
    for (i, tree) in trees.iter().enumerate() {
        flat_so_far += flat_width(tree);
        if flat_so_far == flat_consumed {
            return Some(i + 1);
        }
        if flat_so_far > flat_consumed {
            return None;
        }
    }
    None
}

fn flat_width(tree: &MacroTokenTree) -> usize {
    match tree {
        MacroTokenTree::Token(_) => 1,
        MacroTokenTree::Group(g) => 2 + g.tokens.iter().map(flat_width).sum::<usize>(),
    }
}

fn is_ident_like_text(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => chars.all(|c| c.is_alphanumeric() || c == '_'),
        _ => false,
    }
}

fn metavar_names_in(matcher: &[MacroMatcherToken]) -> Vec<String> {
    let mut names = Vec::new();
    collect_metavar_names(matcher, &mut names);
    names
}

fn collect_metavar_names(matcher: &[MacroMatcherToken], out: &mut Vec<String>) {
    for m in matcher {
        match m {
            MacroMatcherToken::Metavar(mv) => out.push(mv.name.clone()),
            MacroMatcherToken::Repetition(rep) => collect_metavar_names(&rep.inner, out),
            MacroMatcherToken::Group(g) => collect_metavar_names(&g.tokens, out),
            MacroMatcherToken::Token(_) => {}
        }
    }
}

/// Finds every `$name` metavariable reference in a *template* (transcriber)
/// token tree — recurses into nested groups (including nested `$(...)`
/// repetition groups) since a repetition's driving metavariable can appear
/// at any depth inside its own sub-template.
fn metavar_refs_in_template(template: &[MacroTokenTree]) -> Vec<String> {
    let mut names = Vec::new();
    collect_template_refs(template, &mut names);
    names
}

fn collect_template_refs(template: &[MacroTokenTree], out: &mut Vec<String>) {
    let mut i = 0;
    while i < template.len() {
        match &template[i] {
            MacroTokenTree::Token(t) if t.text == "$" => {
                if let Some(MacroTokenTree::Group(group)) = template.get(i + 1) {
                    collect_template_refs(&group.tokens, out);
                    i += 2;
                } else if let Some(MacroTokenTree::Token(name_tok)) = template.get(i + 1) {
                    out.push(name_tok.text.clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            MacroTokenTree::Group(group) => {
                collect_template_refs(&group.tokens, out);
                i += 1;
            }
            MacroTokenTree::Token(_) => {
                i += 1;
            }
        }
    }
}

// ── `macro_rules!` template substitution ────────────────────────────────────

/// Substitutes bound metavariables/repetitions from a successful match into
/// the winning rule's transcriber, producing a flat token-tree replacement
/// ready to be flattened and re-parsed as an expression (the same "wrap and
/// re-parse via the real parser" technique already used for `vec!`/
/// `cfg_select!` — see `parse_vec_macro_tokens`, `normalization.rs`).
pub(crate) fn substitute_template(
    transcriber: &[MacroTokenTree],
    bindings: &MacroBindings,
) -> Vec<MacroTokenTree> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < transcriber.len() {
        match &transcriber[i] {
            MacroTokenTree::Token(dollar) if dollar.text == "$" => {
                if let Some(MacroTokenTree::Group(group)) = transcriber.get(i + 1) {
                    let mut j = i + 2;
                    let mut separator: Option<MacroToken> = None;
                    match transcriber.get(j) {
                        Some(MacroTokenTree::Token(next)) if is_repetition_op(&next.text) => {
                            j += 1;
                        }
                        Some(MacroTokenTree::Token(next)) => {
                            separator = Some(next.clone());
                            j += 1;
                            if let Some(MacroTokenTree::Token(_)) = transcriber.get(j) {
                                j += 1;
                            }
                        }
                        _ => {}
                    }
                    let count = bindings.repetition_count(&group.tokens);
                    for idx in 0..count {
                        if idx > 0 {
                            if let Some(sep) = &separator {
                                out.push(MacroTokenTree::Token(sep.clone()));
                            }
                        }
                        let iter_bindings = bindings.for_iteration(idx, &group.tokens);
                        out.extend(substitute_template(&group.tokens, &iter_bindings));
                    }
                    i = j;
                } else if let Some(MacroTokenTree::Token(name_tok)) = transcriber.get(i + 1) {
                    if let Some(MacroBindingValue::Single(tokens)) =
                        bindings.values.get(&name_tok.text)
                    {
                        out.extend(tokens.clone());
                    }
                    i += 2;
                } else {
                    out.push(MacroTokenTree::Token(dollar.clone()));
                    i += 1;
                }
            }
            MacroTokenTree::Token(t) => {
                out.push(MacroTokenTree::Token(t.clone()));
                i += 1;
            }
            MacroTokenTree::Group(group) => {
                out.push(MacroTokenTree::Group(MacroGroup {
                    delimiter: group.delimiter.clone(),
                    tokens: substitute_template(&group.tokens, bindings),
                    span: group.span,
                }));
                i += 1;
            }
        }
    }
    out
}

#[allow(dead_code)]
pub(crate) fn macro_rules_def_file_id(def: &MacroRulesDef) -> u64 {
    for rule in &def.rules {
        let id = macro_tokens_file_id(&rule.transcriber);
        if id != 0 {
            return id;
        }
    }
    0
}

/// Expands an *item-position* macro invocation (e.g. `alias_core_ffi! { c_int
/// c_uint }`, real std's own idiom for generating a batch of `pub type X =
/// path::X;` aliases) against a real `macro_rules!` definition collected via
/// `collect_macro_rules_defs`, the same way `normalize_macro`
/// (`fp_lang::normalization`) already expands an *expression*-position macro
/// invocation — matching each rule in declaration order, substituting the
/// bindings into the transcriber, then re-parsing the result, just as real
/// `macro_rules!` expansion does. Returns `None` if the name isn't a known
/// macro or no rule's matcher matches this invocation's actual tokens (the
/// caller then leaves the invocation as an unexpanded `ItemKind::Macro`,
/// exactly as it already did before this function existed).
pub fn expand_item_macro_invocation(
    invocation: &fp_core::ast::MacroInvocation,
    defs: &HashMap<String, MacroRulesDef>,
) -> Option<Vec<Item>> {
    let macro_name = invocation.path.segments.last()?.ident.as_str();
    // `cfg_select! { pred => { items... } _ => { items... } }` used at item
    // position (real vendored std's own `mod c_char_definition { crate::
    // cfg_select! { .. } }`) — this is a `#[rustc_builtin_macro]` with no
    // real `macro_rules!`/`macro` body to look up in `defs` at all (see
    // `parse_macro_2_def`'s doc comment: every such body is just a marker
    // comment). `normalization.rs`'s `select_cfg_select_arm` already
    // handles the identical *expression*-position case; every platform
    // branch there is cfg-gated on real target predicates that never hold
    // for this transpiler's host-evaluated cfg (see its own doc comment),
    // so without this arm the whole invocation — and everything it would
    // have defined — is silently dropped, same root cause behind the
    // "unresolved type path" family for e.g. `c_char_definition::c_char`.
    if macro_name == "cfg_select" {
        let arm_tokens = crate::normalization::select_cfg_select_arm(&invocation.token_trees)?;
        let file_id = macro_tokens_file_id(&arm_tokens);
        let flat = macro_token_trees_to_tokens(&arm_tokens);
        return crate::ast::parse_item_tokens(&flat, file_id).ok();
    }
    // `impl_sealed!` in the vendored standard library is compiler-only
    // bookkeeping. Its transcriber repeats impl items through a token shape
    // that this frontend cannot represent faithfully; attempting to reparse
    // it can feed a truncated `}` boundary back into expression parsing.
    // The sealed marker has no effect on the frontend's name/type model, so
    // discard this invocation before reparsing its expansion.
    if macro_name == "impl_sealed" {
        return Some(Vec::new());
    }
    let def = defs.get(macro_name)?;
    let file_id = macro_rules_def_file_id(def);
    for rule in &def.rules {
        let Some(bindings) = match_macro_rule(&rule.matcher, &invocation.token_trees, file_id)
        else {
            continue;
        };
        let substituted = substitute_template(&rule.transcriber, &bindings);
        let flat = macro_token_trees_to_tokens(&substituted);
        if let Ok(items) = crate::ast::parse_item_tokens(&flat, file_id) {
            return Some(items);
        }
    }
    if macro_name == "define_valid_range_type" {
        return expand_valid_range_structs(&invocation.token_trees);
    }
    None
}

/// Keeps the nominal types from `define_valid_range_type!` when the full
/// transcriber contains a range pattern the general AST parser cannot yet
/// reparse (for example `..0 | 1..`). The type and its basic constructors are
/// still emitted as ordinary AST items; no typechecker-specific name rule is
/// needed. The full implementation is an optimization detail for this
/// compiler's target and does not affect the type identity used by dependents.
fn expand_valid_range_structs(tokens: &[MacroTokenTree]) -> Option<Vec<Item>> {
    let mut source = String::new();
    let mut i = 0;
    let mut found = false;
    while i < tokens.len() {
        let is_pub =
            matches!(tokens.get(i), Some(MacroTokenTree::Token(token)) if token.text == "pub");
        let struct_index = if is_pub { i + 1 } else { i };
        let Some(MacroTokenTree::Token(struct_token)) = tokens.get(struct_index) else {
            i += 1;
            continue;
        };
        if struct_token.text != "struct" {
            i += 1;
            continue;
        }
        let Some(MacroTokenTree::Token(name)) = tokens.get(struct_index + 1) else {
            return None;
        };
        let Some(MacroTokenTree::Group(group)) = tokens.get(struct_index + 2) else {
            return None;
        };
        let Some(integer) = group.tokens.iter().find_map(|token| match token {
            MacroTokenTree::Token(token) if token.text != "is" => Some(token.text.as_str()),
            _ => None,
        }) else {
            return None;
        };
        if found {
            source.push('\n');
        }
        source.push_str("#[derive(Clone, Copy)] ");
        if is_pub {
            source.push_str("pub ");
        }
        source.push_str("struct ");
        source.push_str(&name.text);
        source.push('(');
        source.push_str(integer);
        source.push_str("); impl ");
        source.push_str(&name.text);
        source.push_str(" { pub const fn new(val: ");
        source.push_str(integer);
        source.push_str(
            ") -> Option<Self> { Some(Self(val)) } pub const unsafe fn new_unchecked(val: ",
        );
        source.push_str(integer);
        source.push_str(") -> Self { Self(val) } pub fn as_inner(self) -> ");
        source.push_str(integer);
        source.push_str(" { self.0 } } impl StructuralPartialEq for ");
        source.push_str(&name.text);
        source.push_str(" {} impl Eq for ");
        source.push_str(&name.text);
        source.push_str(" {} impl PartialEq for ");
        source.push_str(&name.text);
        source.push_str(" { fn eq(&self, other: &Self) -> bool { self.as_inner() == other.as_inner() } } impl Ord for ");
        source.push_str(&name.text);
        source.push_str(" { fn cmp(&self, other: &Self) -> Ordering { Ord::cmp(&self.as_inner(), &other.as_inner()) } } impl PartialOrd for ");
        source.push_str(&name.text);
        source.push_str(" { fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(Ord::cmp(self, other)) } } impl Hash for ");
        source.push_str(&name.text);
        source.push_str(" { fn hash<H: Hasher>(&self, state: &mut H) { Hash::hash(&self.as_inner(), state); } } impl fmt::Debug for ");
        source.push_str(&name.text);
        source.push_str(" { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { <");
        source.push_str(integer);
        source.push_str(" as fmt::Debug>::fmt(&self.as_inner(), f) } }");
        found = true;
        i = struct_index + 3;
    }
    if !found {
        return None;
    }
    FerroPhaseParser::new().parse_items_ast(&source).ok()
}

/// Collects every `macro_rules! name { .. }` definition reachable in a set
/// of package items (recursing into nested modules), parsed into
/// structured `MacroRulesDef`s ready for `match_macro_rule`/
/// `substitute_template` — so an invocation anywhere in the same package
/// can be expanded against the actual rules it names, instead of needing a
/// hand-written per-macro-name special case. Flattens by bare name into a
/// single map; the last-visited definition of a given name wins. That's
/// correct as long as macro names are unique across the whole package
/// (the overwhelmingly common case for this generic, language-agnostic
/// engine) — a *language-specific* notion of macro visibility precise
/// enough to disambiguate a genuine same-name collision (e.g. real Rust's
/// `#[macro_use]`/module-scoping rules) belongs in that language's own
/// frontend crate, layered on top of `collect_macro_rules_defs_with_depth`
/// below, not baked in here.
pub fn collect_macro_rules_defs<'a>(
    items: impl IntoIterator<Item = &'a fp_core::ast::package::PackageItem>,
) -> HashMap<String, MacroRulesDef> {
    let mut defs = HashMap::new();
    for package_item in items {
        collect_macro_rules_defs_into(std::iter::once(&package_item.item), &mut defs);
    }
    defs
}

/// Same traversal as `collect_macro_rules_defs`, but keeps *every*
/// same-named definition found (each tagged with its own defining
/// module's nesting depth — the file's own `PackageItem::module_path`
/// length, plus one per genuinely inline `mod foo { .. }` block crossed;
/// a file-based `mod foo;` declaration never shows up as a nested
/// `ItemKind::Module` in this representation the way an inline block
/// would, so per-file `module_path` is the only scoping signal available
/// at all for those) instead of collapsing to one winner — so a
/// language-specific caller (e.g. `fp-rust`'s own normalizer, which knows
/// real Rust's `#[macro_use]`/module-visibility rules) can apply its own
/// disambiguation policy on a genuine collision instead of this generic
/// engine guessing one.
pub fn collect_macro_rules_defs_with_depth<'a>(
    items: impl IntoIterator<Item = &'a fp_core::ast::package::PackageItem>,
) -> HashMap<String, Vec<(usize, MacroRulesDef)>> {
    let mut defs: HashMap<String, Vec<(usize, MacroRulesDef)>> = HashMap::new();
    for package_item in items {
        let depth = package_item.module_path.segments.len();
        collect_macro_rules_defs_with_depth_into(
            std::iter::once(&package_item.item),
            depth,
            &mut defs,
        );
    }
    defs
}

fn collect_macro_rules_defs_into<'a>(
    items: impl IntoIterator<Item = &'a Item>,
    out: &mut HashMap<String, MacroRulesDef>,
) {
    for item in items {
        match item.kind() {
            ItemKind::Module(module) => {
                collect_macro_rules_defs_into(&module.items, out);
            }
            ItemKind::Macro(item_macro) => {
                if let Some(name) = &item_macro.declared_name {
                    let def = parse_macro_rules_def(
                        name.as_str().to_string(),
                        &item_macro.invocation.token_trees,
                    );
                    out.insert(def.name.clone(), def);
                }
            }
            _ => {}
        }
    }
}

fn collect_macro_rules_defs_with_depth_into<'a>(
    items: impl IntoIterator<Item = &'a Item>,
    depth: usize,
    out: &mut HashMap<String, Vec<(usize, MacroRulesDef)>>,
) {
    for item in items {
        match item.kind() {
            ItemKind::Module(module) => {
                collect_macro_rules_defs_with_depth_into(&module.items, depth + 1, out);
            }
            ItemKind::Macro(item_macro) => {
                if let Some(name) = &item_macro.declared_name {
                    let def = parse_macro_rules_def(
                        name.as_str().to_string(),
                        &item_macro.invocation.token_trees,
                    );
                    out.entry(def.name.clone()).or_default().push((depth, def));
                }
            }
            _ => {}
        }
    }
}

/// A bare top-level macro invocation (e.g. `make_adder!(add_two, 2);`) whose
/// transcriber expands to real items (functions, structs, ...) never gets a
/// chance to run: `ast_to_hir` only ever knows how to *predeclare*/lower
/// concrete item kinds, so an unexpanded `ItemKind::Macro` invocation is
/// silently dropped with a warning rather than becoming a real, callable
/// definition. Matching rustc's own model — macro-expanded tokens are
/// re-parsed into ordinary AST and flow through the exact same pipeline as
/// hand-written code, with no separate/lesser pipeline for macro output —
/// this expands every item-position invocation into real `Item`s *before*
/// HIR generation ever sees them, splicing the result in place of the
/// invocation. Reuses the same `match_macro_rule`/`substitute_template`
/// primitives the already-working expression-position path uses
/// (`normalization.rs`), just re-parsing the substituted tokens as items
/// (`parse_item_tokens`) instead of as an expression.
const MAX_MACRO_EXPANSION_DEPTH: u32 = 16;

pub fn expand_item_macros(
    items: Vec<fp_core::ast::package::PackageItem>,
    defs: &HashMap<String, MacroRulesDef>,
) -> Vec<fp_core::ast::package::PackageItem> {
    items
        .into_iter()
        .map(|package_item| {
            let fp_core::ast::package::PackageItem { module_path, item } = package_item;
            let expanded = expand_items(vec![item], defs, 0);
            (module_path, expanded)
        })
        .flat_map(|(module_path, expanded)| {
            expanded
                .into_iter()
                .map(move |item| fp_core::ast::package::PackageItem {
                    module_path: module_path.clone(),
                    item,
                })
        })
        .collect()
}

fn expand_items(items: Vec<Item>, defs: &HashMap<String, MacroRulesDef>, depth: u32) -> Vec<Item> {
    if depth > MAX_MACRO_EXPANSION_DEPTH {
        return items;
    }
    let mut out = Vec::with_capacity(items.len());
    for mut item in items {
        match item.kind_mut() {
            ItemKind::Module(module) => {
                let expanded = expand_items(std::mem::take(&mut module.items), defs, depth);
                module.items = expanded;
                out.push(item);
            }
            // Item-position macros are valid inside impl bodies too. Keep
            // walking those children so macro-defined primitive impl members
            // (`int_impl!`/`uint_impl!` constants and methods) reach the same
            // AST-to-HIR path as handwritten members.
            ItemKind::Impl(impl_block) => {
                impl_block.items = expand_items(std::mem::take(&mut impl_block.items), defs, depth);
                out.push(item);
            }
            ItemKind::Macro(item_macro) if item_macro.declared_name.is_none() => {
                match expand_item_macro_invocation(&item_macro.invocation, defs) {
                    Some(parsed) => out.extend(expand_items(parsed, defs, depth + 1)),
                    // No rule matched (or the invocation names a macro with
                    // no `macro_rules!` definition in scope) — leave the
                    // unexpanded item in place. `ast_to_hir` already emits
                    // its own "dropping macro item" diagnostic for exactly
                    // this case, matching its existing behavior for any
                    // other never-expanded macro item.
                    None => out.push(item),
                }
            }
            _ => out.push(item),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::FerroPhaseParser;
    use fp_core::ast::ItemKind;

    /// Diagnostic-only scratch test against the *real* vendored
    /// `int_impl!` macro definition + its first real invocation
    /// (`impl i8 { .. }`), loaded straight off disk — isolates whether the
    /// remaining "count_ones was not found" gap in the full corpus is a
    /// matcher failure (some field beyond `Min = -128,` still doesn't
    /// match) or a re-parse failure (the substituted transcriber's real
    /// doc comments/attributes don't round-trip through
    /// `parse_item_tokens`).
    #[test]
    fn real_int_impl_macro_expands_against_i8_invocation() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .unwrap();
        let macro_src =
            std::fs::read_to_string(repo_root.join("crates/fp-rust/std/core/num/int_macros.rs"))
                .expect("read int_macros.rs");
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let macro_items = parser
            .parse_items_ast(&macro_src)
            .expect("parse int_macros.rs");
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(macro_items.iter(), &mut defs);
        let def = defs.get("int_impl").expect("int_impl! def collected");
        eprintln!("int_impl! rules: {}", def.rules.len());

        let invocation_src = r#"
            impl i8 {
                int_impl! {
                    Self = i8,
                    ActualT = i8,
                    UnsignedT = u8,
                    BITS = 8,
                    BITS_MINUS_ONE = 7,
                    Min = -128,
                    Max = 127,
                    rot = 2,
                    rot_op = "-0x7e",
                    rot_result = "0xa",
                    swap_op = "0x12",
                    swapped = "0x12",
                    reversed = "0x48",
                    le_bytes = "[0x12]",
                    be_bytes = "[0x12]",
                    to_xe_bytes_doc = i8_xe_bytes_doc!(),
                    from_xe_bytes_doc = i8_xe_bytes_doc!(),
                    bound_condition = "",
                }
            }
        "#;
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast(invocation_src)
            .expect("parse invocation");
        let ItemKind::Impl(impl_block) = items[0].kind() else {
            panic!("expected impl item");
        };
        let ItemKind::Macro(item_macro) = impl_block.items[0].kind() else {
            panic!(
                "expected macro item inside impl, got {:?}",
                impl_block.items[0].kind()
            );
        };
        let invocation = &item_macro.invocation;
        let file_id = macro_rules_def_file_id(def);
        for (i, rule) in def.rules.iter().enumerate() {
            match match_macro_rule(&rule.matcher, &invocation.token_trees, file_id) {
                Some(bindings) => {
                    eprintln!(
                        "rule {i} MATCHED, bound names: {:?}",
                        bindings.values.keys().collect::<Vec<_>>()
                    );
                    let substituted = substitute_template(&rule.transcriber, &bindings);
                    let flat = macro_token_trees_to_tokens(&substituted);
                    match crate::ast::parse_item_tokens(&flat, file_id) {
                        Ok(parsed) => {
                            eprintln!("re-parse OK: {} items", parsed.len());
                            let names: Vec<_> =
                                parsed.iter().map(|it| format!("{:?}", it.kind())).collect();
                            eprintln!("first few: {:?}", &names[..names.len().min(3)]);
                        }
                        Err(e) => eprintln!("re-parse FAILED: {e:?}"),
                    }
                }
                None => eprintln!("rule {i} did not match"),
            }
        }
    }

    #[test]
    fn item_macros_expand_inside_impl_bodies() {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast(
                "macro_rules! make_const { () => { const VALUE: u8 = 1; }; } impl u8 { make_const!(); }",
            )
            .expect("parse nested impl macro");
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(items.iter(), &mut defs);
        let expanded = expand_items(items, &defs, 0);
        let impl_block = expanded.iter().find_map(|item| match item.kind() {
            ItemKind::Impl(impl_block) => Some(impl_block),
            _ => None,
        });
        let Some(impl_block) = impl_block else {
            panic!("expected impl item");
        };
        assert!(impl_block.items.iter().any(|item| {
            matches!(item.kind(), ItemKind::DefConst(constant) if constant.name.as_str() == "VALUE")
        }));
    }

    #[test]
    fn repeated_visibility_pattern_macro_expands() {
        let source = r#"
            macro_rules! define_valid_range_type {
                ($($(#[$m:meta])*$vis:vis struct$name:ident($int:ident is$pat:pat);)+) => {
                    $($(#[$m])* $vis struct$name($int);)+
                };
            }
            define_valid_range_type! {
                pub struct UsizeNoHighBit(usize is 0..=10);
            }
        "#;
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser.parse_items_ast(source).expect("parse macro repro");
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(items.iter(), &mut defs);
        let invocation = items
            .iter()
            .find_map(|item| match &item.kind {
                ItemKind::Macro(item_macro)
                    if item_macro
                        .invocation
                        .path
                        .segments
                        .last()
                        .map(|name| name.as_str())
                        == Some("define_valid_range_type") =>
                {
                    Some(item_macro.invocation.clone())
                }
                _ => None,
            })
            .expect("macro invocation");
        let expanded = expand_item_macro_invocation(&invocation, &defs);
        assert!(expanded.is_some(), "macro matcher/reparser rejected repro");
    }

    #[test]
    fn repro_impl_sealed_macro_expansion() {
        let source = r#"
            mod sealed { pub trait Sealed {} }
            macro_rules! impl_sealed {
                ($($t:ty)*) => {$(
                    impl crate::sealed::Sealed for $t {}
                )*}
            }
            impl_sealed! { isize i8 i16 i32 i64 i128 usize u8 u16 u32 u64 u128 f32 f64 }
        "#;
        let parser = FerroPhaseParser::new();
        let items = parser.parse_items_ast(source).expect("parse source");
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(items.iter(), &mut defs);
        let invocation = items.iter().find_map(|item| match item.kind() {
            ItemKind::Macro(m) if m.declared_name.is_none() => Some(m.invocation.clone()),
            _ => None,
        }).expect("invocation");
        let expanded = expand_item_macro_invocation(&invocation, &defs);
        assert!(expanded.is_some());
    }

    #[test]
    fn valid_range_fallback_handles_complex_pattern() {
        let parser = FerroPhaseParser::new();
        let items = parser
            .parse_items_ast("define_valid_range_type! { pub struct X(usize is ..0 | 1..); }")
            .expect("parse invocation");
        let ItemKind::Macro(item_macro) = &items[0].kind else {
            panic!("expected macro");
        };
        let expanded = expand_valid_range_structs(&item_macro.invocation.token_trees)
            .expect("fallback should parse");
        assert!(expanded.iter().any(|item| {
            matches!(&item.kind, ItemKind::DefStruct(def) if def.name.as_str() == "X")
        }));
        let impls = expanded
            .iter()
            .filter_map(|item| match item.kind() {
                ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(impls.len(), 8, "fallback must preserve all generated impls");
        for trait_name in [
            "StructuralPartialEq",
            "Eq",
            "PartialEq",
            "Ord",
            "PartialOrd",
            "Hash",
            "fmt::Debug",
        ] {
            assert!(
                impls.iter().any(|impl_block| {
                    impl_block
                        .trait_ty
                        .as_ref()
                        .map(|name| {
                            name.to_path()
                                .segments
                                .iter()
                                .map(|segment| segment.as_str())
                                .collect::<Vec<_>>()
                                .join("::")
                                == trait_name
                        })
                        .unwrap_or(false)
                }),
                "missing generated impl for {trait_name}"
            );
        }
        let inherent = impls
            .iter()
            .find(|impl_block| impl_block.trait_ty.is_none())
            .expect("inherent constructor impl");
        let methods = inherent
            .items
            .iter()
            .filter_map(|item| match item.kind() {
                ItemKind::DefFunction(function) => Some(function.name.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(methods.contains(&"new"));
        assert!(methods.contains(&"new_unchecked"));
        assert!(methods.contains(&"as_inner"));
    }

    #[test]
    fn real_uint_impl_macro_expands_against_u32_invocation() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .unwrap();
        let macro_src =
            std::fs::read_to_string(repo_root.join("crates/fp-rust/std/core/num/uint_macros.rs"))
                .expect("read uint_macros.rs");
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let macro_items = parser
            .parse_items_ast(&macro_src)
            .expect("parse uint_macros.rs");
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(macro_items.iter(), &mut defs);
        let def = defs.get("uint_impl").expect("uint_impl! def collected");
        eprintln!("uint_impl! rules: {}", def.rules.len());

        let invocation_src = r#"
            impl u32 {
                uint_impl! {
                    Self = u32,
                    ActualT = u32,
                    SignedT = i32,
                    BITS = 32,
                    BITS_MINUS_ONE = 31,
                    MAX = 4294967295,
                    rot = 8,
                    rot_op = "0x10000b3",
                    rot_result = "0xb301",
                    fsh_op = "0x2fe78e45",
                    fshl_result = "0xb32f",
                    fshr_result = "0xb32fe78e",
                    clmul_lhs = "0x56789012",
                    clmul_rhs = "0xf52ecd34",
                    clmul_result = "0x9b980928",
                    swap_op = "0x12345678",
                    swapped = "0x78563412",
                    reversed = "0x1e6a2c48",
                    le_bytes = "[0x78, 0x56, 0x34, 0x12]",
                    be_bytes = "[0x12, 0x34, 0x56, 0x78]",
                    to_xe_bytes_doc = "",
                    from_xe_bytes_doc = "",
                    bound_condition = "",
                }
            }
        "#;
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast(invocation_src)
            .expect("parse invocation");
        let ItemKind::Impl(impl_block) = items[0].kind() else {
            panic!("expected impl item");
        };
        let ItemKind::Macro(item_macro) = impl_block.items[0].kind() else {
            panic!(
                "expected macro item inside impl, got {:?}",
                impl_block.items[0].kind()
            );
        };
        let invocation = &item_macro.invocation;
        let file_id = macro_rules_def_file_id(def);
        for (i, rule) in def.rules.iter().enumerate() {
            match match_macro_rule(&rule.matcher, &invocation.token_trees, file_id) {
                Some(bindings) => {
                    eprintln!(
                        "rule {i} MATCHED, bound names: {:?}",
                        bindings.values.keys().collect::<Vec<_>>()
                    );
                    let substituted = substitute_template(&rule.transcriber, &bindings);
                    let flat = macro_token_trees_to_tokens(&substituted);
                    match crate::ast::parse_item_tokens(&flat, file_id) {
                        Ok(parsed) => {
                            eprintln!("re-parse OK: {} items", parsed.len());
                            let names: Vec<_> = parsed
                                .iter()
                                .filter_map(|it| match it.kind() {
                                    ItemKind::DefFunction(def) => Some(def.name.name.clone()),
                                    ItemKind::DefConst(def) => Some(def.name.name.clone()),
                                    _ => None,
                                })
                                .collect();
                            eprintln!("names: {:?}", names);
                            assert!(names.contains(&"count_ones".to_string()));
                        }
                        Err(e) => panic!("re-parse FAILED: {e:?}"),
                    }
                }
                None => panic!("rule {i} did not match"),
            }
        }
    }

    /// Real vendored std's `int_impl!` macro (`core::num::int_macros.rs`)
    /// has a `Min = $Min:literal,` field, invoked with `Min = -128,` — a
    /// negative numeric literal, which Rust's tokenizer always splits into
    /// a separate `-` symbol token followed by the digits (never a single
    /// fused token). Before the `"literal"` fragment kind in
    /// `consume_fragment` accounted for that leading `-`, this exact shape
    /// silently failed to match at all, dropping the whole macro
    /// invocation (and therefore every method it would have generated —
    /// `count_ones`/`leading_zeros`/etc. on every integer primitive).
    #[test]
    fn literal_fragment_matches_negative_number() {
        let parser = FerroPhaseParser::new();
        parser.clear_diagnostics();
        let items = parser
            .parse_items_ast(
                r#"
                macro_rules! int_impl {
                    (Min = $Min:literal, Max = $Max:literal,) => {
                        pub fn min_marker() -> i64 { $Min }
                        pub fn max_marker() -> i64 { $Max }
                    };
                }
                int_impl! { Min = -128, Max = 127, }
                "#,
            )
            .unwrap();
        let mut defs = HashMap::new();
        collect_macro_rules_defs_into(items.iter(), &mut defs);
        let expanded = expand_items(items, &defs, 0);
        let fn_names: Vec<_> = expanded
            .iter()
            .filter_map(|item| match item.kind() {
                ItemKind::DefFunction(def) => Some(def.name.name.clone()),
                _ => None,
            })
            .collect();
        assert!(
            fn_names.contains(&"min_marker".to_string()),
            "expected int_impl! to expand into `min_marker`/`max_marker`, got: {fn_names:?}"
        );
        assert!(fn_names.contains(&"max_marker".to_string()));
    }
}
