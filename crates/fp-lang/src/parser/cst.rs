use fp_core::cst::{CstNode, CstResult};
use winnow::combinator::{alt, repeat};
use winnow::token::take_while;
use winnow::{ModalResult, Parser};

use crate::lexer::winnow::{
    backtrack_err, is_delimiter, parse_cooked_string_literal, parse_raw_identifier,
    parse_raw_string_literal, ws, MULTI_PUNCT, SINGLE_PUNCT,
};
use super::winnow::{keyword, map_err_mode, offset, span as make_span, symbol, try_parse};

pub fn parse(source: &str) -> CstResult<CstNode> {
    let mut input = source;
    let origin_len = input.len();
    root(&mut input, origin_len).map_err(map_err_mode)
}

fn root(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    let nodes = repeat(0.., |i: &mut &str| element(i, origin_len)).parse_next(input)?;
    ws.parse_next(input)?;
    let end = offset(*input, origin_len);
    Ok(CstNode::root(nodes).with_span(make_span(start, end)))
}

fn element(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    ws.parse_next(input)?;
    if let Some(node) = try_parse(input, |i| parse_quote(i, origin_len))? {
        return Ok(node);
    }
    if let Some(node) = try_parse(input, |i| parse_const_block(i, origin_len))? {
        return Ok(node);
    }
    if let Some(node) = try_parse(input, |i| parse_emit_macro(i, origin_len))? {
        return Ok(node);
    }
    if let Some(node) = try_parse(input, |i| parse_splice(i, origin_len))? {
        return Ok(node);
    }
    if let Some(node) = try_parse(input, |i| parse_block(i, origin_len))? {
        return Ok(node);
    }
    parse_token(input, origin_len)
}

fn parse_block(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    symbol("{").parse_next(input)?;
    let mut children = Vec::new();
    loop {
        ws.parse_next(input)?;
        if input.starts_with('}') {
            *input = &input['}'.len_utf8()..];
            break;
        }
        children.push(element(input, origin_len)?);
    }
    let end = offset(*input, origin_len);
    Ok(CstNode::block(children).with_span(make_span(start, end)))
}

fn parse_quote(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    keyword("quote").parse_next(input)?;
    ws.parse_next(input)?;
    let body = parse_block(input, origin_len)?;
    let end = offset(*input, origin_len);
    Ok(CstNode::quote(vec![body]).with_span(make_span(start, end)))
}

fn parse_const_block(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    keyword("const").parse_next(input)?;
    ws.parse_next(input)?;
    let block = parse_block(input, origin_len)?;
    let end = offset(*input, origin_len);
    Ok(CstNode::const_block(vec![block]).with_span(make_span(start, end)))
}

fn parse_emit_macro(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    keyword("emit").parse_next(input)?;
    symbol("!").parse_next(input)?;
    ws.parse_next(input)?;
    let body = parse_block(input, origin_len)?;
    let end = offset(*input, origin_len);
    Ok(CstNode::splice(vec![CstNode::quote(vec![body])]).with_span(make_span(start, end)))
}

fn parse_splice(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    keyword("splice").parse_next(input)?;
    let start = offset(*input, origin_len);
    ws.parse_next(input)?;

    if let Some(children) = try_parse(input, |i| parse_parenthesized(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(CstNode::splice(children).with_span(make_span(start, end)));
    }
    if let Some(q) = try_parse(input, |i| parse_quote(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(CstNode::splice(vec![q]).with_span(make_span(start, end)));
    }
    if let Some(b) = try_parse(input, |i| parse_block(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(CstNode::splice(vec![b]).with_span(make_span(start, end)));
    }
    let tok = parse_token(input, origin_len)?;
    let end = offset(*input, origin_len);
    Ok(CstNode::splice(vec![tok]).with_span(make_span(start, end)))
}

fn parse_parenthesized(input: &mut &str, origin_len: usize) -> ModalResult<Vec<CstNode>> {
    symbol("(").parse_next(input)?;
    let mut nodes = Vec::new();
    loop {
        ws.parse_next(input)?;
        if input.starts_with(')') {
            *input = &input[')'.len_utf8()..];
            break;
        }
        nodes.push(element(input, origin_len)?);
        ws.parse_next(input)?;
        if input.starts_with(',') {
            *input = &input[','.len_utf8()..];
            continue;
        }
        if input.starts_with(')') {
            *input = &input[')'.len_utf8()..];
            break;
        }
    }
    Ok(nodes)
}

fn parse_token(input: &mut &str, origin_len: usize) -> ModalResult<CstNode> {
    let start = offset(*input, origin_len);
    if let Some(node) = try_parse(input, |i| raw_byte_string_literal(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| raw_string_literal(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| byte_string_literal(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| string_literal(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| raw_identifier(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| punct_token(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    if let Some(node) = try_parse(input, |i| word_token(i, origin_len))? {
        let end = offset(*input, origin_len);
        return Ok(node.with_span(make_span(start, end)));
    }
    Err(backtrack_err())
}

fn string_literal(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    parse_cooked_string_literal(input, "").map(CstNode::token)
}

fn byte_string_literal(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    parse_cooked_string_literal(input, "b").map(CstNode::token)
}

fn raw_string_literal(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    parse_raw_string_literal(input, false).map(CstNode::token)
}

fn raw_byte_string_literal(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    parse_raw_string_literal(input, true).map(CstNode::token)
}

fn punct_token(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    alt((
        punct_multi.map(CstNode::token),
        single_punct.map(CstNode::token),
    ))
    .parse_next(input)
}

fn punct_multi(input: &mut &str) -> ModalResult<String> {
    for symbol in MULTI_PUNCT {
        if let Some(rest) = input.strip_prefix(symbol) {
            let matched = symbol.to_string();
            *input = rest;
            return Ok(matched);
        }
    }
    Err(backtrack_err())
}

fn single_punct(input: &mut &str) -> ModalResult<String> {
    if let Some(ch) = input.chars().next() {
        if SINGLE_PUNCT.contains(ch) {
            *input = &input[ch.len_utf8()..];
            return Ok(ch.to_string());
        }
    }
    Err(backtrack_err())
}

fn raw_identifier(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    parse_raw_identifier(input).map(CstNode::token)
}

fn word_token(input: &mut &str, _origin_len: usize) -> ModalResult<CstNode> {
    take_while(1.., |c: char| !c.is_whitespace() && !is_delimiter(c))
        .map(|word: &str| CstNode::token(word.to_string()))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::cst::CstKind;

    fn contains_quote(node: &CstNode) -> bool {
        matches!(node.kind, CstKind::Quote) || node.children.iter().any(contains_quote)
    }

    #[test]
    fn parses_quote_inside_block() {
        let src = "fn main() { let _ = quote { 1 + 2 }; }";
        let cst = parse(src).expect("parse");
        assert!(contains_quote(&cst));
    }
}
