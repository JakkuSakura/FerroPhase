use std::collections::HashMap;

use fp_core::ast::{MacroDelimiter, MacroInvocation, MacroToken, MacroTokenTree};
use fp_core::error::{Error, Result};
use fp_core::span::Span;

#[derive(Debug, Clone)]
pub struct MacroRulesDefinition {
    pub name: String,
    pub rules: Vec<MacroRule>,
}

#[derive(Debug, Clone)]
pub struct MacroRule {
    matcher: Vec<MacroMatcher>,
    expansion: Vec<MacroTemplate>,
}

#[derive(Debug, Clone)]
enum MacroMatcher {
    Token(String),
    Group(MacroDelimiter, Vec<MacroMatcher>),
    MetaVar {
        name: String,
        kind: FragmentKind,
    },
    Repeat {
        inner: Vec<MacroMatcher>,
        separator: Option<String>,
        op: RepeatOp,
    },
}

#[derive(Debug, Clone)]
enum MacroTemplate {
    Token(MacroToken),
    Group(MacroDelimiter, Vec<MacroTemplate>, Span),
    Var(String),
    Repeat {
        inner: Vec<MacroTemplate>,
        separator: Option<String>,
        op: RepeatOp,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RepeatOp {
    ZeroOrMore,
    OneOrMore,
    Optional,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FragmentKind {
    Ident,
    Expr,
    Ty,
    Path,
    Item,
    Stmt,
    Pat,
    Meta,
    Tt,
}

#[derive(Debug, Default, Clone)]
struct Captures {
    vars: HashMap<String, Vec<Vec<MacroTokenTree>>>,
}

impl Captures {
    fn merge(&mut self, other: Captures) -> Result<()> {
        for (name, values) in other.vars {
            if let Some(existing) = self.vars.get_mut(&name) {
                if existing.len() == 1 && values.len() == 1 && existing[0] != values[0] {
                    return Err(Error::from(format!(
                        "macro variable `${}` captured different tokens in the same match",
                        name
                    )));
                }
                if existing.is_empty() {
                    *existing = values;
                }
            } else {
                self.vars.insert(name, values);
            }
        }
        Ok(())
    }

    fn push_repetition(&mut self, other: Captures) -> Result<()> {
        for (name, mut values) in other.vars {
            let mut iter = values.drain(..);
            let Some(capture) = iter.next() else {
                continue;
            };
            let entry = self.vars.entry(name).or_default();
            entry.push(capture);
        }
        Ok(())
    }
}

pub fn parse_macro_rules(invocation: &MacroInvocation, name: &str) -> Result<MacroRulesDefinition> {
    let rules = parse_rules(&invocation.token_trees)?;
    Ok(MacroRulesDefinition {
        name: name.to_string(),
        rules,
    })
}

pub fn expand_macro(
    rules: &MacroRulesDefinition,
    invocation: &MacroInvocation,
) -> Result<Vec<MacroTokenTree>> {
    for rule in &rules.rules {
        if let Some(captures) = match_rule(&rule.matcher, &invocation.token_trees)? {
            return expand_template(&rule.expansion, &captures);
        }
    }
    Err(Error::from(format!(
        "no macro_rules! match for `{}` in `{}`",
        invocation.path, rules.name
    )))
}

fn parse_rules(tokens: &[MacroTokenTree]) -> Result<Vec<MacroRule>> {
    let mut idx = 0;
    let mut rules = Vec::new();
    while idx < tokens.len() {
        while matches!(tokens.get(idx), Some(MacroTokenTree::Token(tok)) if tok.text == ";") {
            idx += 1;
        }
        if idx >= tokens.len() {
            break;
        }
        let Some(MacroTokenTree::Group(group)) = tokens.get(idx) else {
            return Err(Error::from("macro_rules! expected matcher group"));
        };
        let pattern_tokens = &group.tokens;
        idx += 1;
        let Some(MacroTokenTree::Token(tok)) = tokens.get(idx) else {
            return Err(Error::from("macro_rules! expected => after matcher"));
        };
        if tok.text != "=>" {
            return Err(Error::from("macro_rules! expected => after matcher"));
        }
        idx += 1;
        let Some(MacroTokenTree::Group(group)) = tokens.get(idx) else {
            return Err(Error::from("macro_rules! expected expansion group"));
        };
        let expansion_tokens = &group.tokens;
        let delim = group.delimiter.clone();
        idx += 1;
        let matcher = parse_matchers(pattern_tokens)?;
        let expansion = parse_templates(expansion_tokens, delim)?;
        rules.push(MacroRule { matcher, expansion });
        if matches!(tokens.get(idx), Some(MacroTokenTree::Token(tok)) if tok.text == ";") {
            idx += 1;
        }
    }
    Ok(rules)
}

fn parse_matchers(tokens: &[MacroTokenTree]) -> Result<Vec<MacroMatcher>> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            MacroTokenTree::Token(tok) if tok.text == "$" => {
                idx += 1;
                let Some(next) = tokens.get(idx) else {
                    return Err(Error::from("macro matcher `$` without identifier"));
                };
                match next {
                    MacroTokenTree::Group(group) => {
                        let inner = &group.tokens;
                        let inner = parse_matchers(inner)?;
                        idx += 1;
                        let (separator, op, consumed) = parse_repeat_tail(tokens, idx)?;
                        idx += consumed;
                        out.push(MacroMatcher::Repeat {
                            inner,
                            separator,
                            op,
                        });
                    }
                    MacroTokenTree::Token(name_tok) => {
                        let name = name_tok.text.clone();
                        idx += 1;
                        let kind = if matches!(tokens.get(idx), Some(MacroTokenTree::Token(tok)) if tok.text == ":")
                        {
                            idx += 1;
                            let Some(MacroTokenTree::Token(kind_tok)) = tokens.get(idx) else {
                                return Err(Error::from("macro matcher missing fragment kind"));
                            };
                            idx += 1;
                            parse_fragment_kind(&kind_tok.text)?
                        } else {
                            FragmentKind::Tt
                        };
                        out.push(MacroMatcher::MetaVar { name, kind });
                    }
                }
            }
            MacroTokenTree::Group(group) => {
                let inner = parse_matchers(&group.tokens)?;
                out.push(MacroMatcher::Group(group.delimiter.clone(), inner));
                idx += 1;
            }
            MacroTokenTree::Token(tok) => {
                out.push(MacroMatcher::Token(tok.text.clone()));
                idx += 1;
            }
        }
    }
    Ok(out)
}

fn parse_templates(tokens: &[MacroTokenTree], delim: MacroDelimiter) -> Result<Vec<MacroTemplate>> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            MacroTokenTree::Token(tok) if tok.text == "$" => {
                idx += 1;
                let Some(next) = tokens.get(idx) else {
                    return Err(Error::from("macro expansion `$` without identifier"));
                };
                match next {
                    MacroTokenTree::Group(group) => {
                        let inner = parse_templates(&group.tokens, delim.clone())?;
                        idx += 1;
                        let (separator, op, consumed) = parse_repeat_tail(tokens, idx)?;
                        idx += consumed;
                        out.push(MacroTemplate::Repeat {
                            inner,
                            separator,
                            op,
                        });
                    }
                    MacroTokenTree::Token(name_tok) => {
                        out.push(MacroTemplate::Var(name_tok.text.clone()));
                        idx += 1;
                    }
                }
            }
            MacroTokenTree::Group(group) => {
                let inner = parse_templates(&group.tokens, group.delimiter.clone())?;
                out.push(MacroTemplate::Group(
                    group.delimiter.clone(),
                    inner,
                    group.span,
                ));
                idx += 1;
            }
            MacroTokenTree::Token(tok) => {
                out.push(MacroTemplate::Token(tok.clone()));
                idx += 1;
            }
        }
    }

    if matches!(delim, MacroDelimiter::Brace) && out.is_empty() {
        Ok(Vec::new())
    } else {
        Ok(out)
    }
}

fn parse_repeat_tail(
    tokens: &[MacroTokenTree],
    idx: usize,
) -> Result<(Option<String>, RepeatOp, usize)> {
    let Some(token) = tokens.get(idx) else {
        return Err(Error::from("macro repetition missing operator"));
    };
    if let MacroTokenTree::Token(tok) = token {
        if let Some(op) = parse_repeat_op(&tok.text) {
            return Ok((None, op, 1));
        }
    }
    if let Some(MacroTokenTree::Token(sep)) = tokens.get(idx) {
        if let Some(MacroTokenTree::Token(op_tok)) = tokens.get(idx + 1) {
            if let Some(op) = parse_repeat_op(&op_tok.text) {
                return Ok((Some(sep.text.clone()), op, 2));
            }
        }
    }
    Err(Error::from("macro repetition missing operator"))
}

fn parse_repeat_op(text: &str) -> Option<RepeatOp> {
    match text {
        "*" => Some(RepeatOp::ZeroOrMore),
        "+" => Some(RepeatOp::OneOrMore),
        "?" => Some(RepeatOp::Optional),
        _ => None,
    }
}

fn parse_fragment_kind(text: &str) -> Result<FragmentKind> {
    Ok(match text {
        "ident" => FragmentKind::Ident,
        "expr" => FragmentKind::Expr,
        "ty" => FragmentKind::Ty,
        "path" => FragmentKind::Path,
        "item" => FragmentKind::Item,
        "stmt" => FragmentKind::Stmt,
        "pat" => FragmentKind::Pat,
        "meta" => FragmentKind::Meta,
        "tt" => FragmentKind::Tt,
        other => {
            return Err(Error::from(format!(
                "unsupported macro fragment kind: {other}"
            )));
        }
    })
}

fn match_rule(matcher: &[MacroMatcher], tokens: &[MacroTokenTree]) -> Result<Option<Captures>> {
    let mut captures = Captures::default();
    if match_sequence(matcher, tokens, 0, &mut captures)?.is_some() {
        Ok(Some(captures))
    } else {
        Ok(None)
    }
}

fn match_sequence(
    matcher: &[MacroMatcher],
    tokens: &[MacroTokenTree],
    start: usize,
    captures: &mut Captures,
) -> Result<Option<usize>> {
    let mut idx = start;
    for item in matcher {
        match item {
            MacroMatcher::Token(text) => {
                let Some(MacroTokenTree::Token(tok)) = tokens.get(idx) else {
                    return Ok(None);
                };
                if &tok.text != text {
                    return Ok(None);
                }
                idx += 1;
            }
            MacroMatcher::Group(delim, inner) => {
                let Some(MacroTokenTree::Group(group)) = tokens.get(idx) else {
                    return Ok(None);
                };
                if group.delimiter != *delim {
                    return Ok(None);
                }
                let mut inner_caps = Captures::default();
                let Some(consumed) = match_sequence(inner, &group.tokens, 0, &mut inner_caps)?
                else {
                    return Ok(None);
                };
                if consumed != group.tokens.len() {
                    return Ok(None);
                }
                captures.merge(inner_caps)?;
                idx += 1;
            }
            MacroMatcher::MetaVar { name, kind } => {
                let Some((capture, next_idx)) =
                    match_fragment(kind, tokens, idx, matcher, captures)?
                else {
                    return Ok(None);
                };
                let mut inner_caps = Captures::default();
                inner_caps.vars.insert(name.clone(), vec![capture]);
                captures.merge(inner_caps)?;
                idx = next_idx;
            }
            MacroMatcher::Repeat {
                inner,
                separator,
                op,
            } => {
                let mut count = 0usize;
                loop {
                    let mut inner_caps = Captures::default();
                    let Some(consumed) = match_sequence(inner, tokens, idx, &mut inner_caps)?
                    else {
                        break;
                    };
                    if consumed == idx {
                        break;
                    }
                    idx = consumed;
                    captures.push_repetition(inner_caps)?;
                    count += 1;

                    if let Some(sep) = separator {
                        let Some(MacroTokenTree::Token(tok)) = tokens.get(idx) else {
                            break;
                        };
                        if &tok.text != sep {
                            break;
                        }
                        idx += 1;
                    }
                }
                match op {
                    RepeatOp::OneOrMore if count == 0 => return Ok(None),
                    RepeatOp::Optional if count > 1 => return Ok(None),
                    _ => {}
                }
            }
        }
    }
    Ok(Some(idx))
}

fn match_fragment(
    kind: &FragmentKind,
    tokens: &[MacroTokenTree],
    idx: usize,
    matcher: &[MacroMatcher],
    captures: &Captures,
) -> Result<Option<(Vec<MacroTokenTree>, usize)>> {
    match kind {
        FragmentKind::Ident => match tokens.get(idx) {
            Some(MacroTokenTree::Token(tok)) if is_ident(&tok.text) => {
                Ok(Some((vec![MacroTokenTree::Token(tok.clone())], idx + 1)))
            }
            _ => Ok(None),
        },
        FragmentKind::Tt => match tokens.get(idx) {
            Some(tree) => Ok(Some((vec![tree.clone()], idx + 1))),
            None => Ok(None),
        },
        _ => match_variable_length(tokens, idx, matcher, captures),
    }
}

fn match_variable_length(
    tokens: &[MacroTokenTree],
    idx: usize,
    matcher: &[MacroMatcher],
    captures: &Captures,
) -> Result<Option<(Vec<MacroTokenTree>, usize)>> {
    for end in (idx + 1)..=tokens.len() {
        let mut temp_caps = captures.clone();
        if match_sequence(matcher, tokens, end, &mut temp_caps)?.is_some() {
            let slice = tokens[idx..end].to_vec();
            return Ok(Some((slice, end)));
        }
    }
    Ok(None)
}

fn expand_template(
    templates: &[MacroTemplate],
    captures: &Captures,
) -> Result<Vec<MacroTokenTree>> {
    let mut out = Vec::new();
    for item in templates {
        match item {
            MacroTemplate::Token(tok) => out.push(MacroTokenTree::Token(tok.clone())),
            MacroTemplate::Group(delim, inner, span) => {
                let inner = expand_template(inner, captures)?;
                out.push(MacroTokenTree::Group(fp_core::ast::MacroGroup {
                    delimiter: delim.clone(),
                    tokens: inner,
                    span: *span,
                }));
            }
            MacroTemplate::Var(name) => {
                let Some(values) = captures.vars.get(name) else {
                    return Err(Error::from(format!(
                        "macro expansion missing capture for `${}`",
                        name
                    )));
                };
                if let Some(first) = values.first() {
                    out.extend(first.clone());
                }
            }
            MacroTemplate::Repeat {
                inner,
                separator,
                op,
            } => {
                let names = collect_template_vars(inner);
                let mut iterations = 0usize;
                for name in &names {
                    if let Some(values) = captures.vars.get(name) {
                        iterations = iterations.max(values.len());
                    }
                }
                match op {
                    RepeatOp::OneOrMore if iterations == 0 => {
                        return Err(Error::from("macro expansion expected repetitions"));
                    }
                    RepeatOp::Optional => {
                        iterations = iterations.min(1);
                    }
                    _ => {}
                }
                for idx in 0..iterations {
                    let mut segment = expand_template_with_index(inner, captures, idx)?;
                    out.append(&mut segment);
                    if let Some(sep) = separator {
                        if idx + 1 < iterations {
                            out.push(MacroTokenTree::Token(MacroToken {
                                text: sep.clone(),
                                span: Span::null(),
                            }));
                        }
                    }
                }
            }
        }
    }
    Ok(out)
}

fn expand_template_with_index(
    templates: &[MacroTemplate],
    captures: &Captures,
    idx: usize,
) -> Result<Vec<MacroTokenTree>> {
    let mut out = Vec::new();
    for item in templates {
        match item {
            MacroTemplate::Token(tok) => out.push(MacroTokenTree::Token(tok.clone())),
            MacroTemplate::Group(delim, inner, span) => {
                let inner = expand_template_with_index(inner, captures, idx)?;
                out.push(MacroTokenTree::Group(fp_core::ast::MacroGroup {
                    delimiter: delim.clone(),
                    tokens: inner,
                    span: *span,
                }));
            }
            MacroTemplate::Var(name) => {
                let Some(values) = captures.vars.get(name) else {
                    return Err(Error::from(format!(
                        "macro expansion missing capture for `${}`",
                        name
                    )));
                };
                let slice = values.get(idx).or_else(|| values.first());
                if let Some(tokens) = slice {
                    out.extend(tokens.clone());
                }
            }
            MacroTemplate::Repeat {
                inner,
                separator,
                op,
            } => {
                let nested = MacroTemplate::Repeat {
                    inner: inner.clone(),
                    separator: separator.clone(),
                    op: *op,
                };
                let nested_out = expand_template(&[nested], captures)?;
                out.extend(nested_out);
            }
        }
    }
    Ok(out)
}

fn collect_template_vars(templates: &[MacroTemplate]) -> Vec<String> {
    let mut out = Vec::new();
    for item in templates {
        match item {
            MacroTemplate::Var(name) => out.push(name.clone()),
            MacroTemplate::Group(_, inner, _) => out.extend(collect_template_vars(inner)),
            MacroTemplate::Repeat { inner, .. } => out.extend(collect_template_vars(inner)),
            _ => {}
        }
    }
    out
}

fn is_ident(text: &str) -> bool {
    text.chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
}
