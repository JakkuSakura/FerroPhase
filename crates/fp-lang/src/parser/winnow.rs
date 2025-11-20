use fp_core::cst::{CstError, CstNode, CstResult};
use winnow::combinator::{alt, preceded, repeat};
use winnow::error::{ContextError, ParseError};
use winnow::token::{literal, take_while};
use winnow::{ModalResult, Parser};

use super::lex::{
    backtrack_err, is_delimiter, parse_cooked_string_literal, parse_raw_identifier,
    parse_raw_string_literal, ws, MULTI_PUNCT, SINGLE_PUNCT,
};

pub fn parse(source: &str) -> CstResult<CstNode> {
    root.parse(source)
        .map_err(|err: ParseError<&str, ContextError>| map_err(err.into_inner()))
}

fn root(input: &mut &str) -> ModalResult<CstNode> {
    let nodes = repeat(0.., element).parse_next(input)?;
    ws.parse_next(input)?;
    Ok(CstNode::root(nodes))
}

fn element(input: &mut &str) -> ModalResult<CstNode> {
    preceded(
        ws,
        alt((
            parse_quote,
            parse_const_block,
            parse_emit_macro,
            parse_splice,
            parse_block,
            parse_token,
        )),
    )
    .parse_next(input)
}

fn parse_block(input: &mut &str) -> ModalResult<CstNode> {
    symbol("{").parse_next(input)?;
    let mut children = Vec::new();
    loop {
        ws.parse_next(input)?;
        if input.starts_with('}') {
            *input = &input['}'.len_utf8()..];
            break;
        }
        children.push(element.parse_next(input)?);
    }
    Ok(CstNode::block(children))
}

fn parse_quote(input: &mut &str) -> ModalResult<CstNode> {
    keyword("quote").parse_next(input)?;
    ws.parse_next(input)?;
    let body = parse_block(input)?;
    Ok(CstNode::quote(vec![body]))
}

fn parse_const_block(input: &mut &str) -> ModalResult<CstNode> {
    keyword("const").parse_next(input)?;
    ws.parse_next(input)?;
    let block = parse_block(input)?;
    Ok(CstNode::const_block(vec![block]))
}

fn parse_emit_macro(input: &mut &str) -> ModalResult<CstNode> {
    keyword("emit").parse_next(input)?;
    symbol("!").parse_next(input)?;
    ws.parse_next(input)?;
    let body = parse_block(input)?;
    Ok(CstNode::splice(vec![CstNode::quote(vec![body])]))
}

fn parse_splice(input: &mut &str) -> ModalResult<CstNode> {
    keyword("splice").parse_next(input)?;
    preceded(
        ws,
        alt((
            parse_parenthesized,
            parse_quote.map(|q| vec![q]),
            parse_block.map(|b| vec![b]),
            parse_token.map(|t| vec![t]),
        )),
    )
    .map(CstNode::splice)
    .parse_next(input)
}

fn parse_parenthesized(input: &mut &str) -> ModalResult<Vec<CstNode>> {
    symbol("(").parse_next(input)?;
    let mut nodes = Vec::new();
    loop {
        ws.parse_next(input)?;
        if input.starts_with(')') {
            *input = &input[')'.len_utf8()..];
            break;
        }
        nodes.push(element.parse_next(input)?);
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

fn parse_token(input: &mut &str) -> ModalResult<CstNode> {
    alt((
        raw_byte_string_literal,
        raw_string_literal,
        byte_string_literal,
        string_literal,
        raw_identifier,
        punct_token,
        word_token,
    ))
    .parse_next(input)
}

fn string_literal(input: &mut &str) -> ModalResult<CstNode> {
    parse_cooked_string_literal(input, "").map(CstNode::token)
}

fn byte_string_literal(input: &mut &str) -> ModalResult<CstNode> {
    parse_cooked_string_literal(input, "b").map(CstNode::token)
}

fn raw_string_literal(input: &mut &str) -> ModalResult<CstNode> {
    parse_raw_string_literal(input, false).map(CstNode::token)
}

fn raw_byte_string_literal(input: &mut &str) -> ModalResult<CstNode> {
    parse_raw_string_literal(input, true).map(CstNode::token)
}

fn punct_token(input: &mut &str) -> ModalResult<CstNode> {
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

fn raw_identifier(input: &mut &str) -> ModalResult<CstNode> {
    parse_raw_identifier(input).map(CstNode::token)
}

fn word_token(input: &mut &str) -> ModalResult<CstNode> {
    take_while(1.., |c: char| !c.is_whitespace() && !is_delimiter(c))
        .map(|word: &str| CstNode::token(word.to_string()))
        .parse_next(input)
}

fn keyword<'a>(kw: &'static str) -> impl Parser<&'a str, (), ContextError> {
    move |input: &mut &str| {
        literal(kw).parse_next(input)?;
        if input
            .chars()
            .next()
            .map_or(false, super::lex::is_ident_continue)
        {
            Err(backtrack_err())
        } else {
            Ok(())
        }
    }
}

fn symbol<'a>(sym: &'static str) -> impl Parser<&'a str, (), ContextError> {
    move |input: &mut &str| {
        literal(sym).parse_next(input)?;
        Ok(())
    }
}

fn map_err(err: ContextError) -> CstError {
    CstError::Message(err.to_string())
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
