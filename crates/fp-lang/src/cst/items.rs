use thiserror::Error;
#[allow(deprecated)] // ErrorKind required by winnow 0.6 FromExternalError API.
use winnow::error::{ContextError, ErrMode, ErrorKind, FromExternalError};
use winnow::ModalResult;

use crate::cst;
use crate::lexer::{Keyword, Lexeme, LexerError, Token, TokenKind};
use crate::syntax::{
    span_for_children, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, SyntaxTokenKind,
};
use fp_core::span::FileId;
use fp_core::span::Span;
use std::cell::Cell;

#[derive(Debug, Error)]
pub enum ItemParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexerError),
    #[error("parse error: {message}")]
    Parse { message: String, span: Option<Span> },
}

#[derive(Debug)]
struct ItemParseMessage(String);

impl std::fmt::Display for ItemParseMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ItemParseMessage {}

#[allow(deprecated)] // ErrorKind required by winnow 0.6 FromExternalError API.
fn cut_message<T>(input: &mut &[Token], message: impl Into<String>) -> ModalResult<T> {
    Err(ErrMode::Cut(<ContextError as FromExternalError<
        &[Token],
        _,
    >>::from_external_error(
        input,
        ErrorKind::Verify,
        ItemParseMessage(message.into()),
    )))
}

impl ItemParseError {
    fn from_err_with_span(
        err: ErrMode<ContextError>,
        input: &[Token],
        full_input: &[Token],
        context: Option<&str>,
    ) -> Self {
        let mut message = match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => ctx.to_string(),
            ErrMode::Incomplete(_) => "incomplete input".to_string(),
        };
        let (token_hint, span) = match input.first() {
            Some(token) => (
                format!("next token {:?} '{}'", token.kind, token.lexeme),
                Some(token_span_to_core(token)),
            ),
            None => match full_input.last() {
                Some(token) => (
                    format!(
                        "unexpected end of input after {:?} '{}'",
                        token.kind, token.lexeme
                    ),
                    Some(span_at_eof(token)),
                ),
                None => ("unexpected end of input".to_string(), None),
            },
        };
        if message.trim().is_empty() {
            message = token_hint;
        } else {
            message = format!("{message} ({token_hint})");
        }
        if let Some(context) = context {
            message = format!("while parsing {context}: {message}");
        }
        ItemParseError::Parse { message, span }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            ItemParseError::Parse { span, .. } => *span,
            _ => None,
        }
    }
}

#[allow(dead_code)]
pub fn parse_items_tokens_to_cst(tokens: &[Token]) -> Result<SyntaxNode, ItemParseError> {
    parse_items_tokens_to_cst_with_file(tokens, 0)
}

pub fn parse_items_tokens_to_cst_with_file(
    tokens: &[Token],
    file: FileId,
) -> Result<SyntaxNode, ItemParseError> {
    let mut input: &[Token] = tokens;
    with_items_file(file, || {
        let mut children: Vec<SyntaxElement> = Vec::new();
        let inner_attrs = match parse_inner_attrs_cst(&mut input) {
            Ok(attrs) => attrs,
            Err(err) => {
                return Err(ItemParseError::from_err_with_span(err, input, tokens, None));
            }
        };
        for attr in inner_attrs {
            children.push(SyntaxElement::Node(Box::new(attr)));
        }
        while !input.is_empty() {
            // Skip stray semicolons.
            if matches_symbol(input.first(), ";") {
                input = &input[1..];
                continue;
            }

            let mut item_children = Vec::new();
            if let Some(attr) = match parse_inner_attr_cst(&mut input) {
                Ok(attr) => attr,
                Err(err) => {
                    return Err(ItemParseError::from_err_with_span(err, input, tokens, None));
                }
            } {
                children.push(SyntaxElement::Node(Box::new(attr)));
                continue;
            }
            let attrs = match parse_outer_attrs_cst(&mut input) {
                Ok(attrs) => attrs,
                Err(err) => {
                    return Err(ItemParseError::from_err_with_span(err, input, tokens, None))
                }
            };
            for attr in attrs {
                item_children.push(SyntaxElement::Node(Box::new(attr)));
            }
            let visibility = match parse_visibility_cst(&mut input) {
                Ok(vis) => vis,
                Err(err) => {
                    return Err(ItemParseError::from_err_with_span(err, input, tokens, None))
                }
            };
            if let Some(vis) = visibility {
                item_children.push(SyntaxElement::Node(Box::new(vis)));
            }
            let item = match parse_item_cst(&mut input, item_children) {
                Ok(item) => item,
                Err(err) => {
                    return Err(ItemParseError::from_err_with_span(
                        err,
                        input,
                        tokens,
                        Some("item"),
                    ))
                }
            };
            children.push(SyntaxElement::Node(Box::new(item)));
        }

        let span = span_for_children(&children);
        Ok(SyntaxNode::new(SyntaxKind::ItemList, children, span))
    })
}

#[allow(dead_code)]
pub fn parse_item_tokens_prefix_to_cst(
    tokens: &[Token],
) -> Result<(SyntaxNode, usize), ItemParseError> {
    parse_item_tokens_prefix_to_cst_with_file(tokens, 0)
}

pub fn parse_item_tokens_prefix_to_cst_with_file(
    tokens: &[Token],
    file: FileId,
) -> Result<(SyntaxNode, usize), ItemParseError> {
    let mut input: &[Token] = tokens;
    with_items_file(file, || {
        let mut item_children: Vec<SyntaxElement> = Vec::new();
        let attrs = match parse_outer_attrs_cst(&mut input) {
            Ok(attrs) => attrs,
            Err(err) => return Err(ItemParseError::from_err_with_span(err, input, tokens, None)),
        };
        for attr in attrs {
            item_children.push(SyntaxElement::Node(Box::new(attr)));
        }
        let visibility = match parse_visibility_cst(&mut input) {
            Ok(vis) => vis,
            Err(err) => return Err(ItemParseError::from_err_with_span(err, input, tokens, None)),
        };
        if let Some(vis) = visibility {
            item_children.push(SyntaxElement::Node(Box::new(vis)));
        }
        let item = match parse_item_cst(&mut input, item_children) {
            Ok(item) => item,
            Err(err) => return Err(ItemParseError::from_err_with_span(err, input, tokens, None)),
        };
        Ok((item, tokens.len() - input.len()))
    })
}

fn parse_item_cst(
    input: &mut &[Token],
    mut children: Vec<SyntaxElement>,
) -> ModalResult<SyntaxNode> {
    let Some(head) = input.first().cloned() else {
        return Err(ErrMode::Cut(ContextError::new()));
    };

    match head.kind {
        TokenKind::Keyword(Keyword::Use) => {
            advance(input);
            let tree = parse_use_tree_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(tree)));
            expect_symbol(input, ";")?;
            Ok(node(SyntaxKind::ItemUse, children))
        }
        TokenKind::Keyword(Keyword::Unsafe) => {
            let unsafe_token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&unsafe_token)));

            if match_keyword(input, Keyword::Extern) {
                if match_keyword(input, Keyword::Crate) {
                    let crate_name = expect_ident_token(input)?;
                    children.push(SyntaxElement::Token(crate_name.clone()));
                    if match_keyword(input, Keyword::As) {
                        let alias = expect_ident_token(input)?;
                        let rename = node(
                            SyntaxKind::UseTreeRename,
                            vec![
                                SyntaxElement::Token(crate_name),
                                SyntaxElement::Token(alias),
                            ],
                        );
                        children.push(SyntaxElement::Node(Box::new(rename)));
                    }
                    expect_symbol(input, ";")?;
                    return Ok(node(SyntaxKind::ItemExternCrate, children));
                }

                let abi = expect_string_literal_token(input)?;
                children.push(SyntaxElement::Token(abi));

                if match_symbol(input, "{") {
                    let mut extern_children = Vec::new();
                    while !matches_symbol(input.first(), "}") {
                        if input.is_empty() {
                            return Err(ErrMode::Cut(ContextError::new()));
                        }
                        if matches_symbol(input.first(), ";") {
                            advance(input);
                            continue;
                        }
                        let mut member_children = Vec::new();
                        let attrs = parse_outer_attrs_cst(input)?;
                        for attr in attrs {
                            member_children.push(SyntaxElement::Node(Box::new(attr)));
                        }
                        if let Some(vis) = parse_visibility_cst(input)? {
                            member_children.push(SyntaxElement::Node(Box::new(vis)));
                        }
                        loop {
                            match input.first() {
                                Some(Token {
                                    kind: TokenKind::Keyword(Keyword::Unsafe),
                                    ..
                                }) => {
                                    let unsafe_token = advance(input)
                                        .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                    member_children.push(SyntaxElement::Token(
                                        syntax_token_from_token(&unsafe_token),
                                    ));
                                    continue;
                                }
                                Some(Token {
                                    kind: TokenKind::Ident,
                                    lexeme,
                                    ..
                                }) if lexeme == "safe" => {
                                    let safe_token = advance(input)
                                        .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                    member_children.push(SyntaxElement::Token(
                                        syntax_token_from_token(&safe_token),
                                    ));
                                    continue;
                                }
                                Some(Token {
                                    kind: TokenKind::Keyword(Keyword::Async),
                                    ..
                                }) => {
                                    let async_token = advance(input)
                                        .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                    member_children.push(SyntaxElement::Token(
                                        syntax_token_from_token(&async_token),
                                    ));
                                    continue;
                                }
                                _ => {}
                            }
                            break;
                        }

                        if match_keyword(input, Keyword::Fn) {
                            let sig = parse_fn_sig_cst(input)?;
                            member_children.push(SyntaxElement::Node(Box::new(sig)));
                            expect_symbol(input, ";")?;
                            extern_children.push(node(SyntaxKind::ItemExternFnDecl, member_children));
                            continue;
                        }

                        if match_keyword(input, Keyword::Static) {
                            if match_keyword(input, Keyword::Mut) {
                                member_children.push(SyntaxElement::Token(token_text("mut")));
                            }
                            let name = expect_ident_token(input)?;
                            member_children.push(SyntaxElement::Token(name));
                            expect_symbol(input, ":")?;
                            let ty = parse_type_prefix_from_tokens(input, &[";"])?;
                            member_children.push(SyntaxElement::Node(Box::new(ty)));
                            expect_symbol(input, ";")?;
                            extern_children.push(node(
                                SyntaxKind::ItemExternStaticDecl,
                                member_children,
                            ));
                            continue;
                        }

                        return Err(ErrMode::Cut(ContextError::new()));
                    }
                    expect_symbol(input, "}")?;
                    children.extend(
                        extern_children
                            .into_iter()
                            .map(|child| SyntaxElement::Node(Box::new(child))),
                    );
                    return Ok(node(SyntaxKind::ItemExternBlock, children));
                }

                expect_keyword(input, Keyword::Fn)?;
                let sig = parse_fn_sig_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(sig)));
                if match_symbol(input, ";") {
                    return Ok(node(SyntaxKind::ItemExternFnDecl, children));
                }
                let body = parse_block_expr_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(body)));
                return Ok(node(SyntaxKind::ItemFn, children));
            }

            if match_keyword(input, Keyword::Impl) {
                if matches_symbol(input.first(), "<") {
                    let gen = parse_generic_params_cst(input)?;
                    children.push(SyntaxElement::Node(Box::new(gen)));
                }

                let mut is_negative = false;
                if match_symbol(input, "!") {
                    is_negative = true;
                }
                let first_ty = parse_type_prefix_from_tokens(input, &["for", "{"])?;
                let first_ty = if is_negative {
                    node(
                        SyntaxKind::TyNot,
                        vec![
                            SyntaxElement::Token(token_text("!")),
                            SyntaxElement::Node(Box::new(first_ty)),
                        ],
                    )
                } else {
                    first_ty
                };
                children.push(SyntaxElement::Node(Box::new(first_ty)));

                if match_keyword(input, Keyword::For) {
                    let self_ty = parse_type_prefix_from_tokens(input, &["{"])?;
                    children.push(SyntaxElement::Node(Box::new(self_ty)));
                }

                consume_where_clause(input);
                expect_symbol(input, "{")?;
                let inner = parse_items_in_braces_to_item_list(input)?;
                children.push(SyntaxElement::Node(Box::new(inner)));
                return Ok(node(SyntaxKind::ItemImpl, children));
            }

            if match_keyword(input, Keyword::Trait) {
                let name = expect_ident_token(input)?;
                children.push(SyntaxElement::Token(name));

                if matches_symbol(input.first(), "<") {
                    let gen = parse_generic_params_cst(input)?;
                    children.push(SyntaxElement::Node(Box::new(gen)));
                }

                if match_symbol(input, ":") {
                    loop {
                        let bound = parse_type_bound_from_tokens(input, &["+", "{"])?;
                        children.push(SyntaxElement::Node(Box::new(bound)));
                        if match_symbol(input, "+") {
                            continue;
                        }
                        if matches_symbol(input.first(), "!") {
                            continue;
                        }
                        break;
                    }
                }

                consume_where_clause(input);
                expect_symbol(input, "{")?;
                while !matches_symbol(input.first(), "}") {
                    if input.is_empty() {
                        return Err(ErrMode::Cut(ContextError::new()));
                    }
                    let member = parse_trait_member_cst(input)?;
                    children.push(SyntaxElement::Node(Box::new(member)));
                }
                expect_symbol(input, "}")?;
                return Ok(node(SyntaxKind::ItemTrait, children));
            }

            if match_keyword(input, Keyword::Fn) {
                let sig = parse_fn_sig_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(sig)));
                consume_where_clause(input);
                let body = parse_block_expr_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(body)));
                return Ok(node(SyntaxKind::ItemFn, children));
            }

            Err(ErrMode::Cut(ContextError::new()))
        }
        TokenKind::Keyword(Keyword::Extern) => {
            advance(input);
            if match_keyword(input, Keyword::Crate) {
                let crate_name = expect_ident_token(input)?;
                children.push(SyntaxElement::Token(crate_name.clone()));
                if match_keyword(input, Keyword::As) {
                    let alias = expect_ident_token(input)?;
                    let rename = node(
                        SyntaxKind::UseTreeRename,
                        vec![
                            SyntaxElement::Token(crate_name),
                            SyntaxElement::Token(alias),
                        ],
                    );
                    children.push(SyntaxElement::Node(Box::new(rename)));
                }
                expect_symbol(input, ";")?;
                return Ok(node(SyntaxKind::ItemExternCrate, children));
            }

            let abi = expect_string_literal_token(input)?;
            children.push(SyntaxElement::Token(abi));

            if match_symbol(input, "{") {
                let mut extern_children = Vec::new();
                while !matches_symbol(input.first(), "}") {
                    if input.is_empty() {
                        return Err(ErrMode::Cut(ContextError::new()));
                    }
                    if matches_symbol(input.first(), ";") {
                        advance(input);
                        continue;
                    }
                    let mut member_children = Vec::new();
                    let attrs = parse_outer_attrs_cst(input)?;
                    for attr in attrs {
                        member_children.push(SyntaxElement::Node(Box::new(attr)));
                    }
                    if let Some(vis) = parse_visibility_cst(input)? {
                        member_children.push(SyntaxElement::Node(Box::new(vis)));
                    }
                    loop {
                        match input.first() {
                            Some(Token {
                                kind: TokenKind::Keyword(Keyword::Unsafe),
                                ..
                            }) => {
                                let unsafe_token =
                                    advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                member_children.push(SyntaxElement::Token(syntax_token_from_token(
                                    &unsafe_token,
                                )));
                                continue;
                            }
                            Some(Token {
                                kind: TokenKind::Ident,
                                lexeme,
                                ..
                            }) if lexeme == "safe" => {
                                let safe_token = advance(input)
                                    .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                member_children.push(SyntaxElement::Token(syntax_token_from_token(
                                    &safe_token,
                                )));
                                continue;
                            }
                            Some(Token {
                                kind: TokenKind::Keyword(Keyword::Async),
                                ..
                            }) => {
                                let async_token =
                                    advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                                member_children.push(SyntaxElement::Token(syntax_token_from_token(
                                    &async_token,
                                )));
                                continue;
                            }
                            _ => {}
                        }
                        break;
                    }

                    if match_keyword(input, Keyword::Fn) {
                        let sig = parse_fn_sig_cst(input)?;
                        member_children.push(SyntaxElement::Node(Box::new(sig)));
                        expect_symbol(input, ";")?;
                        extern_children.push(node(SyntaxKind::ItemExternFnDecl, member_children));
                        continue;
                    }

                    if match_keyword(input, Keyword::Static) {
                        if match_keyword(input, Keyword::Mut) {
                            member_children.push(SyntaxElement::Token(token_text("mut")));
                        }
                        let name = expect_ident_token(input)?;
                        member_children.push(SyntaxElement::Token(name));
                        expect_symbol(input, ":")?;
                        let ty = parse_type_prefix_from_tokens(input, &[";"])?;
                        member_children.push(SyntaxElement::Node(Box::new(ty)));
                        expect_symbol(input, ";")?;
                        extern_children.push(node(
                            SyntaxKind::ItemExternStaticDecl,
                            member_children,
                        ));
                        continue;
                    }

                    return Err(ErrMode::Cut(ContextError::new()));
                }
                expect_symbol(input, "}")?;
                children.extend(
                    extern_children
                        .into_iter()
                        .map(|child| SyntaxElement::Node(Box::new(child))),
                );
                return Ok(node(SyntaxKind::ItemExternBlock, children));
            }

            expect_keyword(input, Keyword::Fn)?;
            let sig = parse_fn_sig_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(sig)));
            if match_symbol(input, ";") {
                return Ok(node(SyntaxKind::ItemExternFnDecl, children));
            }
            let body = parse_block_expr_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(body)));
            Ok(node(SyntaxKind::ItemFn, children))
        }
        TokenKind::Keyword(Keyword::Mod) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            if match_symbol(input, ";") {
                Ok(node(SyntaxKind::ItemMod, children))
            } else {
                expect_symbol(input, "{")?;
                let inner = parse_items_in_braces_to_item_list(input)?;
                children.push(SyntaxElement::Node(Box::new(inner)));
                Ok(node(SyntaxKind::ItemMod, children))
            }
        }
        TokenKind::Keyword(Keyword::Trait) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));

            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }

            if match_symbol(input, ":") {
                loop {
                    let bound = parse_type_bound_from_tokens(input, &["+", "{"])?;
                    children.push(SyntaxElement::Node(Box::new(bound)));
                    if match_symbol(input, "+") {
                        continue;
                    }
                    if matches_symbol(input.first(), "!") {
                        continue;
                    }
                    break;
                }
            }

            consume_where_clause(input);
            expect_symbol(input, "{")?;
            while !matches_symbol(input.first(), "}") {
                if input.is_empty() {
                    return Err(ErrMode::Cut(ContextError::new()));
                }
                let member = parse_trait_member_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(member)));
            }
            expect_symbol(input, "}")?;
            Ok(node(SyntaxKind::ItemTrait, children))
        }
        TokenKind::Keyword(Keyword::Impl) => {
            advance(input);
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }

            let mut is_negative = false;
            if match_symbol(input, "!") {
                is_negative = true;
            }
            let first_ty = parse_type_prefix_from_tokens(input, &["for", "{"])?;
            let first_ty = if is_negative {
                node(
                    SyntaxKind::TyNot,
                    vec![
                        SyntaxElement::Token(token_text("!")),
                        SyntaxElement::Node(Box::new(first_ty)),
                    ],
                )
            } else {
                first_ty
            };
            children.push(SyntaxElement::Node(Box::new(first_ty)));

            if match_keyword(input, Keyword::For) {
                let self_ty = parse_type_prefix_from_tokens(input, &["{"])?;
                children.push(SyntaxElement::Node(Box::new(self_ty)));
            }

            consume_where_clause(input);
            expect_symbol(input, "{")?;
            let inner = parse_items_in_braces_to_item_list(input)?;
            children.push(SyntaxElement::Node(Box::new(inner)));
            Ok(node(SyntaxKind::ItemImpl, children))
        }
        TokenKind::Keyword(Keyword::Union) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }
            if matches!(input.first(), Some(Token { kind: TokenKind::Keyword(Keyword::Where), .. })) {
                consume_where_clause(input);
            }
            expect_symbol(input, "{")?;
            while !matches_symbol(input.first(), "}") {
                if match_symbol(input, "}") {
                    break;
                }
                let mut field_children = Vec::new();
                let attrs = parse_outer_attrs_cst(input)?;
                for attr in attrs {
                    field_children.push(SyntaxElement::Node(Box::new(attr)));
                }
                if let Some(vis) = parse_visibility_cst(input)? {
                    field_children.push(SyntaxElement::Node(Box::new(vis)));
                }
                let field_name = expect_ident_token(input)?;
                field_children.push(SyntaxElement::Token(field_name));
                if matches_symbol(input.first(), "?") {
                    let optional_tok =
                        advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                    field_children
                        .push(SyntaxElement::Token(syntax_token_from_token(&optional_tok)));
                }
                expect_symbol(input, ":")?;
                let ty = parse_type_prefix_from_tokens(input, &[",", "}"])?;
                field_children.push(SyntaxElement::Node(Box::new(ty)));
                let field = node(SyntaxKind::StructFieldDecl, field_children);
                children.push(SyntaxElement::Node(Box::new(field)));
                if match_symbol(input, ",") {
                    continue;
                }
                break;
            }
            expect_symbol(input, "}")?;
            Ok(node(SyntaxKind::ItemUnion, children))
        }
        TokenKind::Keyword(Keyword::Struct) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }
            if matches!(input.first(), Some(Token { kind: TokenKind::Keyword(Keyword::Where), .. })) {
                consume_where_clause(input);
            }
            if match_symbol(input, "{") {
                while !matches_symbol(input.first(), "}") {
                    if match_symbol(input, "}") {
                        break;
                    }
                    let mut field_children = Vec::new();
                    let attrs = parse_outer_attrs_cst(input)?;
                    for attr in attrs {
                        field_children.push(SyntaxElement::Node(Box::new(attr)));
                    }
                    if let Some(vis) = parse_visibility_cst(input)? {
                        field_children.push(SyntaxElement::Node(Box::new(vis)));
                    }
                    let field_name = expect_ident_token(input)?;
                    field_children.push(SyntaxElement::Token(field_name));
                    if matches_symbol(input.first(), "?") {
                        let optional_tok =
                            advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                        field_children
                            .push(SyntaxElement::Token(syntax_token_from_token(&optional_tok)));
                    }
                    expect_symbol(input, ":")?;
                    let ty = parse_type_prefix_from_tokens(input, &[",", "}"])?;
                    field_children.push(SyntaxElement::Node(Box::new(ty)));
                    let field = node(SyntaxKind::StructFieldDecl, field_children);
                    children.push(SyntaxElement::Node(Box::new(field)));
                    if match_symbol(input, ",") {
                        continue;
                    }
                    break;
                }
                expect_symbol(input, "}")?;
                if matches!(input.first(), Some(Token { kind: TokenKind::Keyword(Keyword::Where), .. })) {
                    consume_where_clause(input);
                }
            } else if match_symbol(input, "(") {
                let mut index = 0usize;
                while !matches_symbol(input.first(), ")") {
                    if input.is_empty() {
                        return Err(ErrMode::Cut(ContextError::new()));
                    }
                    let mut field_children = Vec::new();
                    let attrs = parse_outer_attrs_cst(input)?;
                    for attr in attrs {
                        field_children.push(SyntaxElement::Node(Box::new(attr)));
                    }
                    if let Some(vis) = parse_visibility_cst(input)? {
                        field_children.push(SyntaxElement::Node(Box::new(vis)));
                    }
                    let ty = parse_type_prefix_from_tokens(input, &[",", ")"])?;
                    let field_name = SyntaxToken {
                        kind: SyntaxTokenKind::Token,
                        text: index.to_string(),
                        span: fp_core::span::Span::null(),
                    };
                    index += 1;
                    field_children.push(SyntaxElement::Token(field_name));
                    field_children.push(SyntaxElement::Node(Box::new(ty)));
                    let field = node(SyntaxKind::StructFieldDecl, field_children);
                    children.push(SyntaxElement::Node(Box::new(field)));
                    if match_symbol(input, ",") {
                        continue;
                    }
                    break;
                }
                expect_symbol(input, ")")?;
                if matches!(input.first(), Some(Token { kind: TokenKind::Keyword(Keyword::Where), .. })) {
                    consume_where_clause(input);
                }
                if match_symbol(input, ";") {
                    // optional
                }
            } else if match_symbol(input, ";") {
                // unit struct
            }
            Ok(node(SyntaxKind::ItemStruct, children))
        }
        TokenKind::Keyword(Keyword::Enum) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }
            consume_where_clause(input);
            expect_symbol(input, "{")?;
            while !matches_symbol(input.first(), "}") {
                if match_symbol(input, "}") {
                    break;
                }
                let mut var_children = Vec::new();
                let attrs = parse_outer_attrs_cst(input)?;
                for attr in attrs {
                    var_children.push(SyntaxElement::Node(Box::new(attr)));
                }
                let variant_name = expect_ident_token(input)?;
                var_children.push(SyntaxElement::Token(variant_name));

                // Payload types.
                if match_symbol(input, ":") {
                    let ty = parse_type_prefix_from_tokens(input, &[",", "}"])?;
                    var_children.push(SyntaxElement::Node(Box::new(ty)));
                } else if match_symbol(input, "(") {
                    let mut types = Vec::new();
                    if !match_symbol(input, ")") {
                        loop {
                            let attrs = parse_outer_attrs_cst(input)?;
                            for attr in attrs {
                                types.push(SyntaxElement::Node(Box::new(attr)));
                            }
                            let ty = parse_type_prefix_from_tokens(input, &[",", ")"])?;
                            types.push(SyntaxElement::Node(Box::new(ty)));
                            if match_symbol(input, ")") {
                                break;
                            }
                            expect_symbol(input, ",")?;
                            if match_symbol(input, ")") {
                                break;
                            }
                        }
                    }
                    let span = span_for_children(&types);
                    var_children.push(SyntaxElement::Node(Box::new(SyntaxNode::new(
                        SyntaxKind::TyTuple,
                        types,
                        span,
                    ))));
                } else if match_symbol(input, "{") {
                    let mut fields = Vec::new();
                    while !matches_symbol(input.first(), "}") {
                        if match_symbol(input, "}") {
                            break;
                        }
                        let mut field_children = Vec::new();
                        let attrs = parse_outer_attrs_cst(input)?;
                        for attr in attrs {
                            field_children.push(SyntaxElement::Node(Box::new(attr)));
                        }
                        if let Some(vis) = parse_visibility_cst(input)? {
                            field_children.push(SyntaxElement::Node(Box::new(vis)));
                        }
                        let fname = expect_ident_token(input)?;
                        field_children.push(SyntaxElement::Token(fname));
                        expect_symbol(input, ":")?;
                        let fty = parse_type_prefix_from_tokens(input, &[",", "}"])?;
                        field_children.push(SyntaxElement::Node(Box::new(fty)));
                        let field = node(SyntaxKind::TyField, field_children);
                        fields.push(SyntaxElement::Node(Box::new(field)));
                        if match_symbol(input, ",") {
                            continue;
                        }
                        break;
                    }
                    expect_symbol(input, "}")?;
                    let span = span_for_children(&fields);
                    var_children.push(SyntaxElement::Node(Box::new(SyntaxNode::new(
                        SyntaxKind::TyStructural,
                        fields,
                        span,
                    ))));
                }

                // Discriminant: `= expr`
                if match_symbol(input, "=") {
                    let expr = parse_expr_prefix_from_tokens(input)?;
                    var_children.push(SyntaxElement::Node(Box::new(expr)));
                }

                children.push(SyntaxElement::Node(Box::new(node(
                    SyntaxKind::EnumVariantDecl,
                    var_children,
                ))));

                if match_symbol(input, ",") {
                    continue;
                }
                break;
            }
            expect_symbol(input, "}")?;
            Ok(node(SyntaxKind::ItemEnum, children))
        }
        TokenKind::Keyword(Keyword::Type) => {
            advance(input);
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }
            consume_where_clause(input);
            expect_symbol(input, "=")?;
            let ty = parse_type_prefix_from_tokens(input, &[";"])?;
            children.push(SyntaxElement::Node(Box::new(ty)));
            expect_symbol(input, ";")?;
            Ok(node(SyntaxKind::ItemTypeAlias, children))
        }
        TokenKind::Keyword(Keyword::Opaque) => {
            advance(input);
            expect_keyword(input, Keyword::Type)?;
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            expect_symbol(input, ";")?;
            Ok(node(SyntaxKind::ItemOpaqueType, children))
        }
        TokenKind::Keyword(Keyword::Const) => {
            if matches!(
                input.get(1),
                Some(Token {
                    kind: TokenKind::Symbol,
                    lexeme,
                    ..
                }) if lexeme == "{"
            ) {
                let expr = parse_expr_prefix_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(expr)));
                if match_symbol(input, ";") {
                    // optional
                }
                return Ok(node(SyntaxKind::ItemExpr, children));
            }
            if matches!(
                input.get(1),
                Some(Token {
                    kind: TokenKind::Keyword(Keyword::Struct),
                    ..
                })
            ) {
                children.push(SyntaxElement::Token(syntax_token_from_token(
                    &advance(input).unwrap(),
                )));
                advance(input);
                let name = expect_ident_token(input)?;
                children.push(SyntaxElement::Token(name));
                if matches_symbol(input.first(), "<") {
                    let gen = parse_generic_params_cst(input)?;
                    children.push(SyntaxElement::Node(Box::new(gen)));
                }
                consume_where_clause(input);
                if match_symbol(input, "{") {
                    while !matches_symbol(input.first(), "}") {
                        if match_symbol(input, "}") {
                            break;
                        }
                        let mut field_children = Vec::new();
                        let attrs = parse_outer_attrs_cst(input)?;
                        for attr in attrs {
                            field_children.push(SyntaxElement::Node(Box::new(attr)));
                        }
                        if let Some(vis) = parse_visibility_cst(input)? {
                            field_children.push(SyntaxElement::Node(Box::new(vis)));
                        }
                        let field_name = expect_ident_token(input)?;
                        field_children.push(SyntaxElement::Token(field_name));
                        expect_symbol(input, ":")?;
                        let ty = parse_type_prefix_from_tokens(input, &[",", "}"])?;
                        field_children.push(SyntaxElement::Node(Box::new(ty)));
                        let field = node(SyntaxKind::StructFieldDecl, field_children);
                        children.push(SyntaxElement::Node(Box::new(field)));
                        if match_symbol(input, ",") {
                            continue;
                        }
                        break;
                    }
                    expect_symbol(input, "}")?;
                } else if match_symbol(input, "(") {
                    let mut index = 0usize;
                    while !matches_symbol(input.first(), ")") {
                        if input.is_empty() {
                            return Err(ErrMode::Cut(ContextError::new()));
                        }
                        let mut field_children = Vec::new();
                        let attrs = parse_outer_attrs_cst(input)?;
                        for attr in attrs {
                            field_children.push(SyntaxElement::Node(Box::new(attr)));
                        }
                        if let Some(vis) = parse_visibility_cst(input)? {
                            field_children.push(SyntaxElement::Node(Box::new(vis)));
                        }
                        let ty = parse_type_prefix_from_tokens(input, &[",", ")"])?;
                        let field_name = SyntaxToken {
                            kind: SyntaxTokenKind::Token,
                            text: index.to_string(),
                            span: fp_core::span::Span::null(),
                        };
                        index += 1;
                        field_children.push(SyntaxElement::Token(field_name));
                        field_children.push(SyntaxElement::Node(Box::new(ty)));
                        let field = node(SyntaxKind::StructFieldDecl, field_children);
                        children.push(SyntaxElement::Node(Box::new(field)));
                        if match_symbol(input, ",") {
                            continue;
                        }
                        break;
                    }
                    expect_symbol(input, ")")?;
                    if match_symbol(input, ";") {
                        // optional
                    }
                } else if match_symbol(input, ";") {
                    // unit const struct
                }
                return Ok(node(SyntaxKind::ItemStruct, children));
            }
            // const fn ...
            if matches!(
                (input.get(1), input.get(2), input.get(3)),
                (
                    Some(Token {
                        kind: TokenKind::Keyword(Keyword::Fn),
                        ..
                    }),
                    Some(Token {
                        kind: TokenKind::Ident,
                        ..
                    }),
                    Some(Token {
                        kind: TokenKind::Symbol,
                        lexeme,
                        ..
                    })
                ) if lexeme == "(" || lexeme == "<"
            ) {
                // consume const + fn
                children.push(SyntaxElement::Token(syntax_token_from_token(
                    &advance(input).unwrap(),
                )));
                advance(input);
                let sig = parse_fn_sig_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(sig)));
                consume_where_clause(input);
                let body = parse_block_expr_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(body)));
                Ok(node(SyntaxKind::ItemFn, children))
            } else {
                advance(input);
                if match_keyword(input, Keyword::Mut) {
                    children.push(SyntaxElement::Token(token_text("mut")));
                }
                let name = expect_ident_token(input)?;
                children.push(SyntaxElement::Token(name));
                if match_symbol(input, ":") {
                    let ty = parse_type_prefix_from_tokens(input, &["="])?;
                    children.push(SyntaxElement::Node(Box::new(ty)));
                }
                expect_symbol(input, "=")?;
                let expr = parse_expr_prefix_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(expr)));
                expect_symbol(input, ";")?;
                Ok(node(SyntaxKind::ItemConst, children))
            }
        }
        TokenKind::Keyword(Keyword::Quote) => {
            if matches!(
                input.get(1),
                Some(Token {
                    kind: TokenKind::Keyword(Keyword::Fn),
                    ..
                })
            ) {
                children.push(SyntaxElement::Token(syntax_token_from_token(
                    &advance(input).unwrap(),
                )));
                advance(input);
                let sig = parse_fn_sig_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(sig)));
                consume_where_clause(input);
                let body = parse_block_expr_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(body)));
                Ok(node(SyntaxKind::ItemFn, children))
            } else {
                Err(ErrMode::Cut(ContextError::new()))
            }
        }
        TokenKind::Keyword(Keyword::Static) => {
            advance(input);
            if match_keyword(input, Keyword::Mut) {
                children.push(SyntaxElement::Token(token_text("mut")));
            }
            let name = expect_ident_token(input)?;
            children.push(SyntaxElement::Token(name));
            expect_symbol(input, ":")?;
            let ty = parse_type_prefix_from_tokens(input, &["="])?;
            children.push(SyntaxElement::Node(Box::new(ty)));
            expect_symbol(input, "=")?;
            let expr = parse_expr_prefix_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
            expect_symbol(input, ";")?;
            Ok(node(SyntaxKind::ItemStatic, children))
        }
        TokenKind::Keyword(Keyword::Async) => {
            let async_token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&async_token)));
            expect_keyword(input, Keyword::Fn)?;
            let sig = parse_fn_sig_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(sig)));
            consume_where_clause(input);
            let body = parse_block_expr_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(body)));
            Ok(node(SyntaxKind::ItemFn, children))
        }
        TokenKind::Keyword(Keyword::Fn) => {
            advance(input);
            let sig = parse_fn_sig_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(sig)));
            consume_where_clause(input);
            let body = parse_block_expr_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(body)));
            Ok(node(SyntaxKind::ItemFn, children))
        }
        TokenKind::Keyword(Keyword::Let) => {
            // Expression-mode compatibility: allow top-level `let` by wrapping it into a block.
            let block = parse_let_stmt_as_block(input)?;
            children.push(SyntaxElement::Node(Box::new(block)));
            Ok(node(SyntaxKind::ItemExpr, children))
        }
        TokenKind::Ident if matches!(input.get(1), Some(Token { kind: TokenKind::Symbol, lexeme, .. }) if lexeme == "!") =>
        {
            let name = expect_ident_token(input)?;
            let name_text = name.text.clone();
            children.push(SyntaxElement::Token(name));
            let bang = expect_symbol_token(input)?;
            if bang.text != "!" {
                return cut_message(input, "expected symbol '!'");
            }
            children.push(SyntaxElement::Token(bang));
            if name_text == "macro_rules" {
                if matches!(
                    input.first(),
                    Some(Token {
                        kind: TokenKind::Ident,
                        ..
                    })
                ) && (matches_symbol(input.get(1), "(")
                    || matches_symbol(input.get(1), "{")
                    || matches_symbol(input.get(1), "["))
                {
                    let declared = expect_ident_token(input)?;
                    children.push(SyntaxElement::Token(declared));
                }
            }
            let open = expect_symbol_token(input)?;
            let open_text = open.text.clone();
            children.push(SyntaxElement::Token(open));
            let group_tokens = consume_balanced_group_tokens(input, open_text.as_str())?;
            for tok in group_tokens {
                children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
            }
            if match_symbol(input, ";") {
                // optional
            }
            Ok(node(SyntaxKind::ItemMacro, children))
        }
        _ => {
            // Expression item.
            let expr = parse_expr_prefix_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(expr)));
            if match_symbol(input, ";") {
                // optional
            }
            Ok(node(SyntaxKind::ItemExpr, children))
        }
    }
}

fn parse_items_in_braces_to_item_list(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut children = Vec::new();
    let inner_attrs = parse_inner_attrs_cst(input)?;
    for attr in inner_attrs {
        children.push(SyntaxElement::Node(Box::new(attr)));
    }
    while !matches_symbol(input.first(), "}") {
        if input.is_empty() {
            return Err(ErrMode::Cut(ContextError::new()));
        }
        if matches_symbol(input.first(), ";") {
            advance(input);
            continue;
        }
        let mut item_children = Vec::new();
        if let Some(attr) = parse_inner_attr_cst(input)? {
            children.push(SyntaxElement::Node(Box::new(attr)));
            continue;
        }
        let attrs = parse_outer_attrs_cst(input)?;
        for attr in attrs {
            item_children.push(SyntaxElement::Node(Box::new(attr)));
        }
        if let Some(vis) = parse_visibility_cst(input)? {
            item_children.push(SyntaxElement::Node(Box::new(vis)));
        }
        let it = parse_item_cst(input, item_children)?;
        children.push(SyntaxElement::Node(Box::new(it)));
    }
    expect_symbol(input, "}")?;
    Ok(node(SyntaxKind::ItemList, children))
}

fn parse_fn_sig_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut children = Vec::new();
    if matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Splice),
            ..
        })
    ) {
        let splice_tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&splice_tok)));
        let open = expect_symbol_token(input)?;
        if open.text != "(" {
            return cut_message(input, "expected '(' after splice");
        }
        children.push(SyntaxElement::Token(open));
        let name = expect_ident_token(input)?;
        children.push(SyntaxElement::Token(name));
        let close = expect_symbol_token(input)?;
        if close.text != ")" {
            return cut_message(input, "expected ')' after splice name");
        }
        children.push(SyntaxElement::Token(close));
    } else {
        let name = expect_ident_token(input)?;
        children.push(SyntaxElement::Token(name));
    }
    if matches_symbol(input.first(), "<") {
        let gen = parse_generic_params_cst(input)?;
        children.push(SyntaxElement::Node(Box::new(gen)));
    }
    expect_symbol(input, "(")?;
    while !matches_symbol(input.first(), ")") {
        if match_symbol(input, ")") {
            break;
        }
        if match_symbol(input, ",") {
            continue;
        }
        if matches_symbol(input.first(), "...") {
            let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            let param = node(
                SyntaxKind::FnParam,
                vec![SyntaxElement::Token(syntax_token_from_token(&tok))],
            );
            children.push(SyntaxElement::Node(Box::new(param)));
            if match_symbol(input, ",") {
                continue;
            }
            continue;
        }
        if match_symbol(input, "/") {
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: "/".to_string(),
                span: fp_core::span::Span::null(),
            }));
            continue;
        }
        if matches_symbol(input.first(), "*")
            && !matches!(input.get(1), Some(Token { kind: TokenKind::Symbol, lexeme, .. }) if lexeme == "*")
            && !matches!(
                input.get(1),
                Some(Token {
                    kind: TokenKind::Ident,
                    ..
                })
            )
        {
            let _ = advance(input);
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: "*".to_string(),
                span: fp_core::span::Span::null(),
            }));
            continue;
        }

        // Receiver.
        if is_receiver(input) {
            let recv = parse_receiver_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(recv)));
        } else {
            let param = parse_param_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(param)));
        }
        if match_symbol(input, ",") {
            continue;
        }
    }
    expect_symbol(input, ")")?;

    if match_symbol(input, "->") {
        let ty = parse_type_prefix_from_tokens(input, &["where", "{"])?;
        let ret = node(SyntaxKind::FnRet, vec![SyntaxElement::Node(Box::new(ty))]);
        children.push(SyntaxElement::Node(Box::new(ret)));
    }

    Ok(node(SyntaxKind::FnSig, children))
}

fn parse_param_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut children: Vec<SyntaxElement> = Vec::new();
    if match_context_marker(input) {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "context".to_string(),
            span: fp_core::span::Span::null(),
        }));
    }
    if match_keyword(input, Keyword::Const) {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "const".to_string(),
            span: fp_core::span::Span::null(),
        }));
    }
    if match_keyword(input, Keyword::Mut) {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "mut".to_string(),
            span: fp_core::span::Span::null(),
        }));
    }
    if match_symbol(input, "*") {
        if match_symbol(input, "*") {
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: "**".to_string(),
                span: fp_core::span::Span::null(),
            }));
        } else {
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: "*".to_string(),
                span: fp_core::span::Span::null(),
            }));
        }
    }
    let pattern = parse_pattern_from_tokens(input)?;
    children.push(SyntaxElement::Node(Box::new(pattern)));
    expect_symbol(input, ":")?;
    if matches_symbol(input.first(), "...") {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        let ty = node(
            SyntaxKind::TyUnknown,
            vec![SyntaxElement::Token(syntax_token_from_token(&tok))],
        );
        children.push(SyntaxElement::Node(Box::new(ty)));
    } else {
        let ty = parse_type_prefix_from_tokens(input, &["=", ",", ")"])?;
        children.push(SyntaxElement::Node(Box::new(ty)));
    }
    if match_symbol(input, "=") {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "=".to_string(),
            span: fp_core::span::Span::null(),
        }));
        let expr = parse_expr_prefix_from_tokens(input)?;
        children.push(SyntaxElement::Node(Box::new(expr)));
    }
    Ok(node(SyntaxKind::FnParam, children))
}

fn parse_receiver_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut children: Vec<SyntaxElement> = Vec::new();
    if match_symbol(input, "&") {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "&".to_string(),
            span: fp_core::span::Span::null(),
        }));
        if match_keyword(input, Keyword::Mut) {
            children.push(SyntaxElement::Token(SyntaxToken {
                kind: SyntaxTokenKind::Token,
                text: "mut".to_string(),
                span: fp_core::span::Span::null(),
            }));
        }
    } else if match_keyword(input, Keyword::Mut) {
        children.push(SyntaxElement::Token(SyntaxToken {
            kind: SyntaxTokenKind::Token,
            text: "mut".to_string(),
            span: fp_core::span::Span::null(),
        }));
    }

    // self token
    let self_tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    children.push(SyntaxElement::Token(syntax_token_from_token(&self_tok)));
    Ok(node(SyntaxKind::FnReceiver, children))
}

fn parse_trait_member_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let Some(head) = input.first() else {
        return Err(ErrMode::Cut(ContextError::new()));
    };
    match head.kind {
        TokenKind::Keyword(Keyword::Async)
        | TokenKind::Keyword(Keyword::Unsafe)
        | TokenKind::Keyword(Keyword::Extern)
        | TokenKind::Keyword(Keyword::Fn) => {
            let mut children = Vec::new();

            loop {
                match input.first() {
                    Some(Token {
                        kind: TokenKind::Keyword(Keyword::Async),
                        ..
                    }) => {
                        let tok =
                            advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
                    }
                    Some(Token {
                        kind: TokenKind::Keyword(Keyword::Unsafe),
                        ..
                    }) => {
                        let tok =
                            advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
                    }
                    Some(Token {
                        kind: TokenKind::Keyword(Keyword::Extern),
                        ..
                    }) => {
                        let tok =
                            advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
                        if matches!(
                            input.first(),
                            Some(Token {
                                kind: TokenKind::StringLiteral,
                                ..
                            })
                        ) {
                            let abi = advance(input)
                                .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                            children.push(SyntaxElement::Token(syntax_token_from_token(&abi)));
                        }
                    }
                    _ => break,
                }
            }

            expect_keyword(input, Keyword::Fn)?;
            children.push(SyntaxElement::Token(token_text("fn")));
            let sig = parse_fn_sig_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(sig)));
            consume_where_clause(input);
            if match_symbol(input, ";") {
                return Ok(node(SyntaxKind::TraitMember, children));
            }
            // Default method body.
            if matches_symbol(input.first(), "{") {
                let body = parse_block_expr_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(body)));
                return Ok(node(SyntaxKind::TraitMember, children));
            }
            Err(ErrMode::Cut(ContextError::new()))
        }
        TokenKind::Keyword(Keyword::Const) => {
            advance(input);
            let name = expect_ident_token(input)?;
            expect_symbol(input, ":")?;
            let ty = parse_type_prefix_from_tokens(input, &[";"])?;
            expect_symbol(input, ";")?;
            Ok(node(
                SyntaxKind::TraitMember,
                vec![
                    SyntaxElement::Token(token_text("const")),
                    SyntaxElement::Token(name),
                    SyntaxElement::Node(Box::new(ty)),
                ],
            ))
        }
        TokenKind::Keyword(Keyword::Type) => {
            advance(input);
            let name = expect_ident_token(input)?;
            let mut children = vec![
                SyntaxElement::Token(token_text("type")),
                SyntaxElement::Token(name),
            ];
            if matches_symbol(input.first(), "<") {
                let gen = parse_generic_params_cst(input)?;
                children.push(SyntaxElement::Node(Box::new(gen)));
            }
            if match_symbol(input, ":") {
                loop {
                    let bound = parse_type_bound_from_tokens(input, &["+", "=", ";"])?;
                    children.push(SyntaxElement::Node(Box::new(bound)));
                    if match_symbol(input, "+") {
                        continue;
                    }
                    if matches_symbol(input.first(), "!") {
                        continue;
                    }
                    break;
                }
            }
            if match_symbol(input, "=") {
                let ty = parse_type_prefix_from_tokens(input, &[";"])?;
                children.push(SyntaxElement::Node(Box::new(ty)));
            }
            expect_symbol(input, ";")?;
            Ok(node(SyntaxKind::TraitMember, children))
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn parse_generic_params_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let (split_tokens, token_to_input_end) = split_angle_tokens_for_generics(input);
    let mut cursor: &[Token] = &split_tokens;
    expect_symbol(&mut cursor, "<")?;
    let mut children = Vec::new();
    while !matches_symbol(cursor.first(), ">") {
        let mut param_children = Vec::new();
        let attrs = parse_outer_attrs_cst(&mut cursor)?;
        for attr in attrs {
            param_children.push(SyntaxElement::Node(Box::new(attr)));
        }
        if matches!(cursor.first(), Some(Token { kind: TokenKind::Keyword(Keyword::Const), .. })) {
            let tok = advance(&mut cursor).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            param_children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
            let name = expect_ident_token(&mut cursor)?;
            param_children.push(SyntaxElement::Token(name));
            expect_symbol(&mut cursor, ":")?;
            let ty = parse_type_prefix_from_tokens(&mut cursor, &["=", ",", ">"])?;
            param_children.push(SyntaxElement::Node(Box::new(ty)));
            if match_symbol(&mut cursor, "=") {
                let expr = parse_expr_prefix_from_tokens(&mut cursor)?;
                param_children.push(SyntaxElement::Node(Box::new(expr)));
            }
        } else {
            let name = match cursor.first() {
                Some(Token {
                    kind: TokenKind::Ident,
                    ..
                }) => expect_ident_token(&mut cursor)?,
                Some(Token {
                    kind: TokenKind::Keyword(_),
                    lexeme,
                    ..
                }) if lexeme.starts_with('\'') => {
                    let tok = advance(&mut cursor).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                    syntax_token_from_token(&tok)
                }
                Some(Token {
                    kind: TokenKind::Ident,
                    lexeme,
                    ..
                }) if lexeme.starts_with('\'') => expect_ident_token(&mut cursor)?,
                _ => return Err(ErrMode::Cut(ContextError::new())),
            };
            param_children.push(SyntaxElement::Token(name));
            if match_symbol(&mut cursor, ":") {
                loop {
                    let bound = parse_type_bound_from_tokens(&mut cursor, &["+", ",", ">"])?;
                    param_children.push(SyntaxElement::Node(Box::new(bound)));
                    if match_symbol(&mut cursor, "+") {
                        continue;
                    }
                    if matches_symbol(cursor.first(), "!") {
                        continue;
                    }
                    break;
                }
            }
            if match_symbol(&mut cursor, "=") {
                let ty = parse_type_prefix_from_tokens(&mut cursor, &[",", ">"])?;
                param_children.push(SyntaxElement::Node(Box::new(ty)));
            }
        }
        children.push(SyntaxElement::Node(Box::new(node(
            SyntaxKind::GenericParam,
            param_children,
        ))));
        if match_symbol(&mut cursor, ",") {
            if matches_symbol(cursor.first(), ">") {
                break;
            }
            continue;
        }
        break;
    }
    expect_symbol(&mut cursor, ">")?;
    let consumed_split = split_tokens.len() - cursor.len();
    let consumed_tokens = if consumed_split == 0 {
        0
    } else {
        *token_to_input_end
            .get(consumed_split - 1)
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?
    };
    *input = &input[consumed_tokens..];
    Ok(node(SyntaxKind::GenericParams, children))
}

fn split_angle_tokens_for_generics(tokens: &[Token]) -> (Vec<Token>, Vec<usize>) {
    let mut out = Vec::new();
    let mut token_to_input_end = Vec::new();

    for (idx, token) in tokens.iter().enumerate() {
        if token.kind == TokenKind::Symbol && token.lexeme.len() > 1 {
            let mut chars = token.lexeme.chars();
            if let Some(first) = chars.next() {
                if (first == '<' || first == '>')
                    && chars.clone().all(|ch| ch == first)
                {
                    let count = token.lexeme.len();
                    for pos in 0..count {
                        let mut split = token.clone();
                        split.lexeme = first.to_string();
                        out.push(split);
                        let end = if pos + 1 == count { idx + 1 } else { idx };
                        token_to_input_end.push(end);
                    }
                    continue;
                }
            }
        }

        out.push(token.clone());
        token_to_input_end.push(idx + 1);
    }

    (out, token_to_input_end)
}

fn parse_use_tree_cst(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    if match_symbol(input, "{") {
        let mut children = Vec::new();
        while !matches_symbol(input.first(), "}") {
            if match_symbol(input, "}") {
                break;
            }
            let tree = parse_use_tree_cst(input)?;
            children.push(SyntaxElement::Node(Box::new(tree)));
            if match_symbol(input, ",") {
                continue;
            }
            break;
        }
        expect_symbol(input, "}")?;
        return Ok(node(SyntaxKind::UseTreeGroup, children));
    }

    // Leading root `::`
    let mut root = None;
    if match_symbol(input, "::") {
        root = Some(node(SyntaxKind::UseTreeRoot, Vec::new()));
    }

    let mut path_children: Vec<SyntaxElement> = Vec::new();
    if let Some(root) = root {
        path_children.push(SyntaxElement::Node(Box::new(root)));
    }

    loop {
        if match_keyword(input, Keyword::Crate) {
            path_children.push(SyntaxElement::Token(token_text("crate")));
        } else if match_keyword(input, Keyword::Super) {
            path_children.push(SyntaxElement::Node(Box::new(node(
                SyntaxKind::UseTreeSuper,
                Vec::new(),
            ))));
        } else if matches!(
            input.first(),
            Some(Token { kind: TokenKind::Ident, lexeme, .. }) if lexeme == "self"
        ) {
            advance(input);
            path_children.push(SyntaxElement::Node(Box::new(node(
                SyntaxKind::UseTreeSelf,
                Vec::new(),
            ))));
        } else {
            let seg = expect_ident_token(input)?;
            path_children.push(SyntaxElement::Token(seg));
        }

        if match_keyword(input, Keyword::As) {
            let alias = expect_ident_token(input)?;
            let from = path_children
                .pop()
                .and_then(|c| match c {
                    SyntaxElement::Token(t) if !t.is_trivia() => Some(t),
                    _ => None,
                })
                .ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            path_children.push(SyntaxElement::Node(Box::new(node(
                SyntaxKind::UseTreeRename,
                vec![SyntaxElement::Token(from), SyntaxElement::Token(alias)],
            ))));
            break;
        }

        if match_symbol(input, "::") {
            if match_symbol(input, "*") {
                path_children.push(SyntaxElement::Node(Box::new(node(
                    SyntaxKind::UseTreeGlob,
                    Vec::new(),
                ))));
                break;
            }
            if matches_symbol(input.first(), "{") {
                let group = parse_use_tree_cst(input)?;
                path_children.push(SyntaxElement::Node(Box::new(group)));
                break;
            }
            continue;
        }

        break;
    }

    Ok(node(SyntaxKind::UseTreePath, path_children))
}

fn parse_outer_attrs_cst(input: &mut &[Token]) -> ModalResult<Vec<SyntaxNode>> {
    let mut attrs = Vec::new();
    while let Some(attr) = parse_attr_cst(input, false)? {
        attrs.push(attr);
    }
    Ok(attrs)
}

fn parse_inner_attrs_cst(input: &mut &[Token]) -> ModalResult<Vec<SyntaxNode>> {
    let mut attrs = Vec::new();
    while let Some(attr) = parse_attr_cst(input, true)? {
        attrs.push(attr);
    }
    Ok(attrs)
}

fn parse_inner_attr_cst(input: &mut &[Token]) -> ModalResult<Option<SyntaxNode>> {
    parse_attr_cst(input, true)
}

fn parse_attr_cst(input: &mut &[Token], is_inner: bool) -> ModalResult<Option<SyntaxNode>> {
    let mut cursor = *input;
    if !matches_symbol(cursor.first(), "#") {
        return Ok(None);
    }
    cursor = &cursor[1..];
    let has_bang = matches_symbol(cursor.first(), "!");
    if has_bang != is_inner {
        return Ok(None);
    }
    if has_bang {
        cursor = &cursor[1..];
    }
    if !matches_symbol(cursor.first(), "[") {
        return Ok(None);
    }

    // Consume '#', optional '!', '['.
    let mut children = Vec::new();
    children.push(SyntaxElement::Token(syntax_token_from_token(
        &advance(input).unwrap(),
    )));
    if has_bang {
        children.push(SyntaxElement::Token(syntax_token_from_token(
            &advance(input).unwrap(),
        )));
    }
    let open = advance(input).unwrap();
    children.push(SyntaxElement::Token(syntax_token_from_token(&open)));

    while !matches_symbol(input.first(), "]") {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
    }
    let close = advance(input).unwrap();
    children.push(SyntaxElement::Token(syntax_token_from_token(&close)));

    Ok(Some(node(
        if is_inner {
            SyntaxKind::AttrInner
        } else {
            SyntaxKind::AttrOuter
        },
        children,
    )))
}

fn parse_visibility_cst(input: &mut &[Token]) -> ModalResult<Option<SyntaxNode>> {
    let mut cursor = *input;
    if !match_keyword(&mut cursor, Keyword::Pub) {
        return Ok(None);
    }

    // pub(...)
    if match_symbol(&mut cursor, "(") {
        if match_keyword(&mut cursor, Keyword::Crate) {
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            return Ok(Some(node(SyntaxKind::VisibilityCrate, Vec::new())));
        }
        if match_keyword(&mut cursor, Keyword::Super) {
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            let path = node(
                SyntaxKind::UseTreePath,
                vec![SyntaxElement::Node(Box::new(node(
                    SyntaxKind::UseTreeSuper,
                    Vec::new(),
                )))],
            );
            return Ok(Some(node(
                SyntaxKind::VisibilityRestricted,
                vec![SyntaxElement::Node(Box::new(path))],
            )));
        }
        if match_keyword(&mut cursor, Keyword::In) {
            let path = parse_use_tree_cst(&mut cursor)?;
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            return Ok(Some(node(
                SyntaxKind::VisibilityRestricted,
                vec![SyntaxElement::Node(Box::new(path))],
            )));
        }
        if matches!(
            cursor.first(),
            Some(Token { kind: TokenKind::Ident, lexeme, .. }) if lexeme == "self"
        ) {
            advance(&mut cursor);
            expect_symbol(&mut cursor, ")")?;
            *input = cursor;
            let path = node(
                SyntaxKind::UseTreePath,
                vec![SyntaxElement::Node(Box::new(node(
                    SyntaxKind::UseTreeSelf,
                    Vec::new(),
                )))],
            );
            return Ok(Some(node(
                SyntaxKind::VisibilityRestricted,
                vec![SyntaxElement::Node(Box::new(path))],
            )));
        }

        // Fallback: treat unknown pub(...) as public.
        while !matches_symbol(cursor.first(), ")") {
            if advance(&mut cursor).is_none() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
        }
        expect_symbol(&mut cursor, ")")?;
        *input = cursor;
        return Ok(Some(node(SyntaxKind::VisibilityPublic, Vec::new())));
    }

    // bare pub
    *input = cursor;
    Ok(Some(node(SyntaxKind::VisibilityPublic, Vec::new())))
}

fn parse_let_stmt_as_block(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut let_children = Vec::new();
    let let_tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match let_tok.kind {
        TokenKind::Keyword(Keyword::Let) => {
            let_children.push(SyntaxElement::Token(syntax_token_from_token(&let_tok)));
        }
        _ => return cut_message(input, "expected keyword Let"),
    }

    let pattern = parse_pattern_from_tokens(input)?;
    let_children.push(SyntaxElement::Node(Box::new(pattern)));

    if match_symbol(input, ":") {
        let ty = parse_type_prefix_from_tokens(input, &["=", ";"])?;
        let_children.push(SyntaxElement::Node(Box::new(ty)));
    }
    if match_symbol(input, "=") {
        let expr = parse_expr_prefix_from_tokens(input)?;
        let_children.push(SyntaxElement::Node(Box::new(expr)));
    }
    if match_keyword(input, Keyword::Else) {
        let diverge = parse_expr_prefix_from_tokens(input)?;
        let_children.push(SyntaxElement::Node(Box::new(diverge)));
    }
    expect_symbol(input, ";")?;
    let let_node = node(SyntaxKind::BlockStmtLet, let_children);

    Ok(node(
        SyntaxKind::ExprBlock,
        vec![SyntaxElement::Node(Box::new(let_node))],
    ))
}

fn parse_pattern_from_tokens(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut pat = parse_pattern_atom_from_tokens(input)?;
    if matches_symbol(input.first(), "@") {
        let mut children = vec![SyntaxElement::Node(Box::new(pat))];
        let at = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&at)));
        let rhs = parse_pattern_from_tokens(input)?;
        children.push(SyntaxElement::Node(Box::new(rhs)));
        let span = span_for_children(&children);
        return Ok(SyntaxNode::new(SyntaxKind::PatternBind, children, span));
    }
    Ok(pat)
}

fn parse_pattern_atom_from_tokens(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let mut children = Vec::new();
    if matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            lexeme,
            ..
        }) if lexeme == "ref"
    ) {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        if matches!(
            input.first(),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Mut),
                ..
            })
        ) {
            let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        }
    } else if matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Mut),
            ..
        })
    ) {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        if matches!(
            input.first(),
            Some(Token {
                kind: TokenKind::Ident | TokenKind::Keyword(_),
                lexeme,
                ..
            }) if lexeme == "ref"
        ) {
            let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        }
    }

    if matches_symbol(input.first(), "&") {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        if matches!(
            input.first(),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Mut),
                ..
            })
        ) {
            let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        }
        let inner = parse_pattern_from_tokens(input)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        return Ok(node(SyntaxKind::PatternRef, children));
    }
    if matches_symbol(input.first(), "..") {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        return Ok(node(SyntaxKind::PatternRest, children));
    }

    if matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Ident | TokenKind::Keyword(_),
            lexeme,
            ..
        }) if lexeme == "box"
    ) {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        let inner = parse_pattern_from_tokens(input)?;
        children.push(SyntaxElement::Node(Box::new(inner)));
        return Ok(node(SyntaxKind::PatternBox, children));
    }

    if matches_symbol(input.first(), "(") {
        let open = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if open.kind != TokenKind::Symbol || open.lexeme != "(" {
            return cut_message(input, "expected '(' in tuple pattern");
        }
        children.push(SyntaxElement::Token(syntax_token_from_token(&open)));

        if matches_symbol(input.first(), ")") {
            let close = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&close)));
            return Ok(node(SyntaxKind::PatternTuple, children));
        }

        let first = parse_pattern_from_tokens(input)?;
        children.push(SyntaxElement::Node(Box::new(first)));

        let mut is_tuple = false;
        if matches_symbol(input.first(), ",") {
            is_tuple = true;
            let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
            while !matches_symbol(input.first(), ")") {
                let pat = parse_pattern_from_tokens(input)?;
                children.push(SyntaxElement::Node(Box::new(pat)));
                if matches_symbol(input.first(), ",") {
                    let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                    children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
                    if matches_symbol(input.first(), ")") {
                        break;
                    }
                    continue;
                }
                break;
            }
        }

        if !is_tuple && match_keyword(input, Keyword::If) {
            children.push(SyntaxElement::Token(token_text("if")));
            let guard = parse_expr_prefix_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(guard)));
        }

        let close = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if close.kind != TokenKind::Symbol || close.lexeme != ")" {
            return cut_message(input, "expected ')' to close tuple pattern");
        }
        children.push(SyntaxElement::Token(syntax_token_from_token(&close)));
        return Ok(node(
            if is_tuple {
                SyntaxKind::PatternTuple
            } else {
                SyntaxKind::PatternParen
            },
            children,
        ));
    }

    if matches_symbol(input.first(), "[") {
        let open = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        children.push(SyntaxElement::Token(syntax_token_from_token(&open)));
        while !matches_symbol(input.first(), "]") {
            if input.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            let pat = parse_pattern_from_tokens(input)?;
            children.push(SyntaxElement::Node(Box::new(pat)));
            if matches_symbol(input.first(), ",") {
                let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
                if matches_symbol(input.first(), "]") {
                    break;
                }
                continue;
            }
            break;
        }
        let close = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if close.kind != TokenKind::Symbol || close.lexeme != "]" {
            return cut_message(input, "expected ']' to close slice pattern");
        }
        children.push(SyntaxElement::Token(syntax_token_from_token(&close)));
        return Ok(node(SyntaxKind::PatternSlice, children));
    }

    let mut path_children = children;
    let mut saw_colon = false;
    if matches_symbol(input.first(), "::") {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        path_children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        saw_colon = true;
    }
    let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match tok.kind {
        TokenKind::Ident | TokenKind::Keyword(_) => {
            path_children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        }
        _ => return cut_message(input, "expected pattern"),
    }
    while matches_symbol(input.first(), "::") {
        saw_colon = true;
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        path_children.push(SyntaxElement::Token(syntax_token_from_token(&tok)));
        let seg = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        match seg.kind {
            TokenKind::Ident | TokenKind::Keyword(_) => {
                path_children.push(SyntaxElement::Token(syntax_token_from_token(&seg)));
            }
            _ => return cut_message(input, "expected path segment"),
        }
    }

    let path_span = span_for_children(&path_children);
    let path_node = SyntaxNode::new(SyntaxKind::PatternPath, path_children, path_span);

    if matches_symbol(input.first(), "{") {
        let mut struct_children = vec![SyntaxElement::Node(Box::new(path_node))];
        let open = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        struct_children.push(SyntaxElement::Token(syntax_token_from_token(&open)));
        while !matches_symbol(input.first(), "}") {
            if input.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            if matches_symbol(input.first(), "..") {
                let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                let rest = node(
                    SyntaxKind::PatternRest,
                    vec![SyntaxElement::Token(syntax_token_from_token(&tok))],
                );
                struct_children.push(SyntaxElement::Node(Box::new(rest)));
                if matches_symbol(input.first(), ",") {
                    let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                    struct_children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
                }
                continue;
            }
            let name = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
            match name.kind {
                TokenKind::Ident | TokenKind::Keyword(_) => {}
                _ => return cut_message(input, "expected field name in struct pattern"),
            }
            let mut field_children = vec![SyntaxElement::Token(syntax_token_from_token(&name))];
            if matches_symbol(input.first(), ":") {
                let colon = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                field_children.push(SyntaxElement::Token(syntax_token_from_token(&colon)));
                let pat = parse_pattern_from_tokens(input)?;
                field_children.push(SyntaxElement::Node(Box::new(pat)));
            }
            struct_children.push(SyntaxElement::Node(Box::new(node(
                SyntaxKind::StructField,
                field_children,
            ))));
            if matches_symbol(input.first(), ",") {
                let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                struct_children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
                if matches_symbol(input.first(), "}") {
                    break;
                }
                continue;
            }
            break;
        }
        let close = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        struct_children.push(SyntaxElement::Token(syntax_token_from_token(&close)));
        return Ok(node(SyntaxKind::PatternStruct, struct_children));
    }

    if matches_symbol(input.first(), "(") {
        let mut tuple_children = vec![SyntaxElement::Node(Box::new(path_node))];
        let open = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        tuple_children.push(SyntaxElement::Token(syntax_token_from_token(&open)));
        while !matches_symbol(input.first(), ")") {
            if input.is_empty() {
                return Err(ErrMode::Cut(ContextError::new()));
            }
            let pat = parse_pattern_from_tokens(input)?;
            tuple_children.push(SyntaxElement::Node(Box::new(pat)));
            if matches_symbol(input.first(), ",") {
                let comma = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
                tuple_children.push(SyntaxElement::Token(syntax_token_from_token(&comma)));
                if matches_symbol(input.first(), ")") {
                    break;
                }
                continue;
            }
            break;
        }
        let close = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        tuple_children.push(SyntaxElement::Token(syntax_token_from_token(&close)));
        return Ok(node(SyntaxKind::PatternTupleStruct, tuple_children));
    }

    if saw_colon {
        Ok(path_node)
    } else {
        Ok(SyntaxNode::new(
            SyntaxKind::PatternIdent,
            path_node.children,
            path_node.span,
        ))
    }
}

fn consume_where_clause(input: &mut &[Token]) {
    if !match_keyword(input, Keyword::Where) {
        return;
    }
    let mut angle_depth: i32 = 0;
    let mut paren_depth: i32 = 0;
    let mut bracket_depth: i32 = 0;
    while let Some(tok) = input.first() {
        let at_top = angle_depth == 0 && paren_depth == 0 && bracket_depth == 0;
        if at_top && (matches_symbol(Some(tok), "{") || matches_symbol(Some(tok), ";") || matches_symbol(Some(tok), "=")) {
            break;
        }
        if tok.kind == TokenKind::Symbol {
            match tok.lexeme.as_str() {
                "<" => angle_depth += 1,
                ">" => angle_depth = (angle_depth - 1).max(0),
                "(" => paren_depth += 1,
                ")" => paren_depth = (paren_depth - 1).max(0),
                "[" => bracket_depth += 1,
                "]" => bracket_depth = (bracket_depth - 1).max(0),
                _ => {}
            }
        }
        if advance(input).is_none() {
            break;
        }
    }
}

fn consume_balanced_group_tokens(input: &mut &[Token], opener: &str) -> ModalResult<Vec<Token>> {
    let closer = match opener {
        "(" => ")",
        "[" => "]",
        "{" => "}",
        _ => return Err(ErrMode::Cut(ContextError::new())),
    };
    let mut depth = 1i32;
    let mut out = Vec::new();
    while depth > 0 {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if tok.kind == TokenKind::Symbol {
            if tok.lexeme == opener {
                depth += 1;
            } else if tok.lexeme == closer {
                depth -= 1;
            }
        }
        out.push(tok);
    }
    Ok(out)
}

#[allow(deprecated)] // ErrorKind required by winnow 0.6 FromExternalError API.
fn parse_block_expr_from_tokens(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    if !matches_symbol(input.first(), "{") {
        return cut_message(input, "expected '{' block");
    }
    let open = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    let mut tokens = Vec::new();
    tokens.push(open);
    let mut body = consume_balanced_group_tokens(input, "{")?;
    tokens.append(&mut body);
    let mut slice: &[Token] = &tokens;
    let node = parse_expr_prefix_from_tokens(&mut slice)?;
    if !slice.is_empty() {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    Ok(node)
}

#[allow(deprecated)] // ErrorKind required by winnow 0.6 FromExternalError API.
fn parse_expr_prefix_from_tokens(input: &mut &[Token]) -> ModalResult<SyntaxNode> {
    let (lexemes, lexeme_to_token_end) = lexemes_from_tokens_for_expr(input);
    let (node, consumed) = cst::parse_expr_lexemes_prefix_to_cst(&lexemes, current_items_file())
        .map_err(|err| {
            ErrMode::Cut(
                <ContextError as FromExternalError<Vec<Lexeme>, _>>::from_external_error(
                    &lexemes,
                    ErrorKind::Fail,
                    err,
                ),
            )
        })?;

    let consumed_tokens = if consumed == 0 {
        0
    } else {
        *lexeme_to_token_end
            .get(consumed - 1)
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?
    };
    let input_len = input.len();
    if consumed_tokens > input_len {
        return Err(ErrMode::Cut(ContextError::new()));
    }
    *input = &input[consumed_tokens..];
    Ok(node)
}

#[allow(deprecated)] // ErrorKind required by winnow 0.6 FromExternalError API.
fn parse_type_prefix_from_tokens(input: &mut &[Token], stops: &[&str]) -> ModalResult<SyntaxNode> {
    let (lexemes, lexeme_to_token_end) = lexemes_from_tokens_for_type(input);
    let (node, consumed) =
        cst::parse_type_lexemes_prefix_to_cst(&lexemes, current_items_file(), stops).map_err(
            |err| {
                ErrMode::Cut(
                    <ContextError as FromExternalError<Vec<Lexeme>, _>>::from_external_error(
                        &lexemes,
                        ErrorKind::Fail,
                        err,
                    ),
                )
            },
        )?;

    let consumed_tokens = if consumed == 0 {
        0
    } else {
        *lexeme_to_token_end
            .get(consumed - 1)
            .ok_or_else(|| ErrMode::Cut(ContextError::new()))?
    };
    *input = &input[consumed_tokens..];
    Ok(node)
}

fn lexemes_from_tokens_for_type(tokens: &[Token]) -> (Vec<Lexeme>, Vec<usize>) {
    let mut lexemes = Vec::new();
    let mut lexeme_to_token_end = Vec::new();

    for (idx, token) in tokens.iter().enumerate() {
        if token.kind == TokenKind::Symbol && token.lexeme == "::<" {
            lexemes.push(Lexeme::token("::".to_string(), token.span));
            lexeme_to_token_end.push(idx);
            lexemes.push(Lexeme::token("<".to_string(), token.span));
            lexeme_to_token_end.push(idx + 1);
            continue;
        }

        if token.kind == TokenKind::Symbol && token.lexeme.len() > 1 {
            if token.lexeme.chars().all(|ch| ch == '>') {
                let count = token.lexeme.len();
                for pos in 0..count {
                    lexemes.push(Lexeme::token(">".to_string(), token.span));
                    // Allow partial consumption so a trailing `>` from `>>` stays in the token stream.
                    // Only the last split `>` consumes the original token.
                    let end = if pos + 1 == count { idx + 1 } else { idx };
                    lexeme_to_token_end.push(end);
                }
                continue;
            }
            if token.lexeme.chars().all(|ch| ch == '<') {
                let count = token.lexeme.len();
                for pos in 0..count {
                    lexemes.push(Lexeme::token("<".to_string(), token.span));
                    // Allow partial consumption so a trailing `<` from `<<` stays in the token stream.
                    // Only the last split `<` consumes the original token.
                    let end = if pos + 1 == count { idx + 1 } else { idx };
                    lexeme_to_token_end.push(end);
                }
                continue;
            }
        }

        let text = match token.kind {
            TokenKind::Keyword(keyword) => keyword.as_str().to_string(),
            _ => token.lexeme.clone(),
        };
        lexemes.push(Lexeme::token(text, token.span));
        lexeme_to_token_end.push(idx + 1);
    }

    (lexemes, lexeme_to_token_end)
}

fn lexemes_from_tokens_for_expr(tokens: &[Token]) -> (Vec<Lexeme>, Vec<usize>) {
    let mut lexemes = Vec::new();
    let mut lexeme_to_token_end = Vec::new();
    let mut generic_depth: i32 = 0;
    let mut prev_was_coloncolon = false;

    for (idx, token) in tokens.iter().enumerate() {
        if token.kind == TokenKind::Symbol && token.lexeme == "::<" {
            lexemes.push(Lexeme::token("::".to_string(), token.span));
            lexeme_to_token_end.push(idx);
            lexemes.push(Lexeme::token("<".to_string(), token.span));
            lexeme_to_token_end.push(idx + 1);
            generic_depth = generic_depth.saturating_add(1);
            prev_was_coloncolon = false;
            continue;
        }

        if token.kind == TokenKind::Symbol && token.lexeme == "::" {
            lexemes.push(Lexeme::token("::".to_string(), token.span));
            lexeme_to_token_end.push(idx + 1);
            prev_was_coloncolon = true;
            continue;
        }

        if token.kind == TokenKind::Symbol && token.lexeme.len() > 1 {
            if token.lexeme.chars().all(|ch| ch == '<') {
                let should_split = prev_was_coloncolon || generic_depth > 0;
                prev_was_coloncolon = false;
                if should_split {
                    let count = token.lexeme.len();
                    for pos in 0..count {
                        lexemes.push(Lexeme::token("<".to_string(), token.span));
                        let end = if pos + 1 == count { idx + 1 } else { idx };
                        lexeme_to_token_end.push(end);
                    }
                    generic_depth = generic_depth.saturating_add(count as i32);
                    continue;
                }
            }

            if token.lexeme.chars().all(|ch| ch == '>') {
                if generic_depth > 0 {
                    let count = token.lexeme.len();
                    for pos in 0..count {
                        lexemes.push(Lexeme::token(">".to_string(), token.span));
                        let end = if pos + 1 == count { idx + 1 } else { idx };
                        lexeme_to_token_end.push(end);
                    }
                    generic_depth = (generic_depth - count as i32).max(0);
                    prev_was_coloncolon = false;
                    continue;
                }
            }
        }

        prev_was_coloncolon = false;
        let text = match token.kind {
            TokenKind::Keyword(keyword) => keyword.as_str().to_string(),
            _ => token.lexeme.clone(),
        };
        lexemes.push(Lexeme::token(text, token.span));
        lexeme_to_token_end.push(idx + 1);
    }

    (lexemes, lexeme_to_token_end)
}

fn parse_type_bound_from_tokens(input: &mut &[Token], stops: &[&str]) -> ModalResult<SyntaxNode> {
    if match_symbol(input, "?") {
        let bound = parse_type_prefix_from_tokens(input, stops)?;
        return Ok(node(
            SyntaxKind::TyNot,
            vec![
                SyntaxElement::Token(token_text("?")),
                SyntaxElement::Node(Box::new(bound)),
            ],
        ));
    }
    if match_symbol(input, "!") {
        let bound = parse_type_prefix_from_tokens(input, stops)?;
        return Ok(node(
            SyntaxKind::TyNot,
            vec![
                SyntaxElement::Token(token_text("!")),
                SyntaxElement::Node(Box::new(bound)),
            ],
        ));
    }
    parse_type_prefix_from_tokens(input, stops)
}

fn is_receiver(input: &[Token]) -> bool {
    match input {
        [Token {
            kind: TokenKind::Symbol,
            lexeme,
            ..
        }, rest @ ..]
            if lexeme == "&" =>
        {
            matches!(rest.first(), Some(Token { kind: TokenKind::Ident, lexeme, .. }) if lexeme == "self")
                || matches!(rest, [Token { kind: TokenKind::Keyword(Keyword::Mut), .. }, Token { kind: TokenKind::Ident, lexeme, .. }, ..] if lexeme == "self")
        }
        [Token {
            kind: TokenKind::Keyword(Keyword::Mut),
            ..
        }, Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }, ..]
            if lexeme == "self" =>
        {
            true
        }
        [Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }, ..]
            if lexeme == "self" =>
        {
            true
        }
        _ => false,
    }
}

fn node(kind: SyntaxKind, children: Vec<SyntaxElement>) -> SyntaxNode {
    let span = span_for_children(&children);
    SyntaxNode::new(kind, children, span)
}

fn token_text(text: &str) -> SyntaxToken {
    SyntaxToken {
        kind: SyntaxTokenKind::Token,
        text: text.to_string(),
        span: fp_core::span::Span::null(),
    }
}

fn syntax_token_from_token(tok: &Token) -> SyntaxToken {
    SyntaxToken {
        kind: SyntaxTokenKind::Token,
        text: tok.lexeme.clone(),
        span: fp_core::span::Span::new(
            current_items_file(),
            tok.span.start as u32,
            tok.span.end as u32,
        ),
    }
}

fn token_span_to_core(tok: &Token) -> Span {
    Span::new(
        current_items_file(),
        tok.span.start as u32,
        tok.span.end as u32,
    )
}

fn span_at_eof(tok: &Token) -> Span {
    Span::new(
        current_items_file(),
        tok.span.end as u32,
        tok.span.end as u32,
    )
}

thread_local! {
    static ITEMS_FILE_ID: Cell<FileId> = Cell::new(0);
}

fn with_items_file<T>(file: FileId, f: impl FnOnce() -> T) -> T {
    ITEMS_FILE_ID.with(|cell| {
        let prev = cell.get();
        cell.set(file);
        let out = f();
        cell.set(prev);
        out
    })
}

fn current_items_file() -> FileId {
    ITEMS_FILE_ID.with(|cell| cell.get())
}

fn match_keyword(input: &mut &[Token], keyword: Keyword) -> bool {
    matches!(input.first(), Some(Token { kind: TokenKind::Keyword(k), .. }) if *k == keyword) && {
        *input = &input[1..];
        true
    }
}

fn match_context_marker(input: &mut &[Token]) -> bool {
    matches!(
        input,
        [
            Token { lexeme, .. },
            Token { kind: TokenKind::Ident | TokenKind::Keyword(_), .. },
            Token { lexeme: colon, .. },
            ..
        ] if lexeme == "context" && colon == ":"
    ) && {
        *input = &input[1..];
        true
    }
}

fn expect_keyword(input: &mut &[Token], keyword: Keyword) -> ModalResult<()> {
    if match_keyword(input, keyword) {
        Ok(())
    } else {
        cut_message(input, format!("expected keyword {keyword:?}"))
    }
}

fn match_symbol(input: &mut &[Token], sym: &str) -> bool {
    let Some(tok) = input.first() else {
        return false;
    };
    if tok.lexeme == sym
        || (sym == ">" && tok.lexeme.len() > 1 && tok.lexeme.chars().all(|ch| ch == '>'))
    {
        *input = &input[1..];
        return true;
    }
    false
}

fn expect_symbol(input: &mut &[Token], sym: &str) -> ModalResult<()> {
    if match_symbol(input, sym) {
        Ok(())
    } else {
        cut_message(input, format!("expected symbol '{sym}'"))
    }
}

fn matches_symbol(tok: Option<&Token>, sym: &str) -> bool {
    match tok {
        Some(Token { lexeme, .. }) if lexeme == sym => true,
        Some(Token { lexeme, .. })
            if sym == ">" && lexeme.len() > 1 && lexeme.chars().all(|ch| ch == '>') =>
        {
            true
        }
        _ => false,
    }
}

fn expect_symbol_token(input: &mut &[Token]) -> ModalResult<SyntaxToken> {
    let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    if tok.kind != TokenKind::Symbol {
        return cut_message(input, "expected symbol");
    }
    Ok(syntax_token_from_token(&tok))
}

fn expect_ident_token(input: &mut &[Token]) -> ModalResult<SyntaxToken> {
    let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    match tok.kind {
        TokenKind::Ident => Ok(syntax_token_from_token(&tok)),
        TokenKind::Keyword(_) if tok.lexeme == "_" => Ok(syntax_token_from_token(&tok)),
        _ => cut_message(input, "expected identifier"),
    }
}

fn expect_string_literal_token(input: &mut &[Token]) -> ModalResult<SyntaxToken> {
    let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    if tok.kind != TokenKind::StringLiteral {
        return cut_message(input, "expected string literal");
    }
    Ok(syntax_token_from_token(&tok))
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let tok = input.first().cloned()?;
    *input = &input[1..];
    Some(tok)
}
