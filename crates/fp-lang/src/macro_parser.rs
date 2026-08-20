use std::collections::HashMap;

use fp_core::ast::{
    Item, ItemKind, MacroDelimiter, MacroGroup, MacroMatcherGroup, MacroMatcherToken, MacroMetavar,
    MacroRepetition, MacroRepetitionOp, MacroRule, MacroRulesDef, MacroToken, MacroTokenTree,
};
use fp_core::span::Span;

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

// ── `macro_rules!` structured parsing ───────────────────────────────────────

/// Parses a `macro_rules! name { (matcher) => { transcriber }; ... }` item's
/// body (the raw `token_trees` already captured by `ItemMacro`) into a
/// structured `MacroRulesDef`. Each rule is `Group(matcher) "=>" Group(
/// transcriber) ";"?` at the top level — since `MacroTokenTree::Group` nodes
/// are already atomic (the original tokenizer/parser already balanced
/// delimiters when capturing this token tree), no manual depth-tracking is
/// needed to find rule boundaries, same insight already used by
/// `select_cfg_select_arm` in `normalization.rs`.
pub(crate) fn parse_macro_rules_def(name: String, body: &[MacroTokenTree]) -> MacroRulesDef {
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
        if let Some(MacroTokenTree::Token(semi)) = body.get(i) {
            if semi.text == ";" {
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
                }
                else if let Some(MacroTokenTree::Token(name_tok)) = tokens.get(i + 1) {
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
    if match_sequence(matcher, invocation, &mut pos, &mut bindings, file_id) && pos == invocation.len()
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
                if match_sequence(&rep.inner, invocation, &mut sub_pos, &mut iter_bindings, file_id)
                    && sub_pos > probe_pos
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
            if pos < invocation.len() { Some(1) } else { None }
        }
        "ident" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(t)) if is_ident_like_text(&t.text) => Some(1),
            _ => None,
        },
        "block" => match invocation.get(pos) {
            Some(MacroTokenTree::Group(g)) if g.delimiter == MacroDelimiter::Brace => Some(1),
            _ => None,
        },
        "literal" => match invocation.get(pos) {
            Some(MacroTokenTree::Token(_)) => Some(1),
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
/// top-level literal comma, or the matcher's own next literal token if it
/// appears first — both are always legal stopping points for these
/// fragment kinds in real Rust grammar.
fn fragment_window<'a>(
    invocation: &'a [MacroTokenTree],
    pos: usize,
    stop_at: Option<&MacroToken>,
) -> &'a [MacroTokenTree] {
    let mut end = invocation.len();
    for (offset, tree) in invocation[pos..].iter().enumerate() {
        if let MacroTokenTree::Token(t) = tree {
            let is_comma = t.text == ",";
            let is_stop = stop_at.is_some_and(|stop| stop.text == t.text);
            if is_comma || is_stop {
                end = pos + offset;
                break;
            }
        }
    }
    &invocation[pos..end]
}

fn token_tree_count_for_flat_prefix(trees: &[MacroTokenTree], flat_consumed: usize) -> Option<usize> {
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

/// Collects every `macro_rules! name { ... }` definition reachable in a set
/// of top-level items (recursing into nested modules), parsed into
/// structured `MacroRulesDef`s ready for `match_macro_rule`/
/// `substitute_template` — so an invocation anywhere in the same package can
/// be expanded against the actual rules it names, instead of needing a
/// hand-written per-macro-name special case.
pub fn collect_macro_rules_defs<'a>(
    items: impl IntoIterator<Item = &'a Item>,
) -> HashMap<String, MacroRulesDef> {
    let mut defs = HashMap::new();
    collect_macro_rules_defs_into(items, &mut defs);
    defs
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
    let macro_name = invocation.path.segments.last()?.name.as_str();
    let def = defs.get(macro_name)?;
    let file_id = macro_rules_def_file_id(def);
    for rule in &def.rules {
        let Some(bindings) = match_macro_rule(&rule.matcher, &invocation.token_trees, file_id)
        else {
            continue;
        };
        let substituted = substitute_template(&rule.transcriber, &bindings);
        let flat = macro_token_trees_to_tokens(&substituted);
        if let Ok(items) = crate::ast::parse_items_tokens(&flat, file_id) {
            return Some(items);
        }
    }
    None
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
/// (`parse_items_tokens`) instead of as an expression.
const MAX_MACRO_EXPANSION_DEPTH: u32 = 16;

pub fn expand_item_macros(
    items: Vec<fp_core::package::PackageItem>,
    defs: &HashMap<String, MacroRulesDef>,
) -> Vec<fp_core::package::PackageItem> {
    items
        .into_iter()
        .map(|package_item| {
            let fp_core::package::PackageItem { path, item } = package_item;
            let expanded = expand_items(vec![item], defs, 0);
            (path, expanded)
        })
        .flat_map(|(path, expanded)| {
            expanded
                .into_iter()
                .map(move |item| fp_core::package::PackageItem {
                    path: path.clone(),
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
            ItemKind::Macro(item_macro) if item_macro.declared_name.is_none() => {
                let Some(macro_name) = item_macro.invocation.path.segments.last() else {
                    out.push(item);
                    continue;
                };
                let macro_name = macro_name.as_str().trim_end_matches('!').to_string();
                let Some(def) = defs.get(&macro_name) else {
                    out.push(item);
                    continue;
                };
                let invocation_tokens = &item_macro.invocation.token_trees;
                let file_id = macro_tokens_file_id(invocation_tokens);
                let mut expanded_items = None;
                for rule in &def.rules {
                    let Some(bindings) = match_macro_rule(&rule.matcher, invocation_tokens, file_id)
                    else {
                        continue;
                    };
                    let substituted = substitute_template(&rule.transcriber, &bindings);
                    let flat = macro_token_trees_to_tokens(&substituted);
                    if let Ok(parsed) = crate::ast::parse_items_tokens(&flat, file_id) {
                        expanded_items = Some(parsed);
                        break;
                    }
                }
                match expanded_items {
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
