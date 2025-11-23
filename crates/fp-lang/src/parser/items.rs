use fp_core::ast::{
    AttrMeta, AttrStyle, Attribute, EnumTypeVariant, Expr, ExprKind, Ident, Item,
    Item as AstItem, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStatic, ItemDefStruct,
    ItemDefType, ItemImport, ItemImportPath, ItemImportTree, ItemKind, ItemMacro, MacroDelimiter,
    MacroInvocation, Path, StructuralField, Ty, TypeEnum, Visibility,
};
use thiserror::Error;
use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;
use winnow::Parser;

use crate::lexer::{self, Keyword, LexerError, Token, TokenKind};
use crate::lexer::winnow::backtrack_err;
use super::expr;

#[derive(Debug, Error)]
pub enum ItemParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexerError),
    #[error("parse error: {0}")]
    Parse(String),
}

impl From<ErrMode<ContextError>> for ItemParseError {
    fn from(err: ErrMode<ContextError>) -> Self {
        match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => ItemParseError::Parse(ctx.to_string()),
            ErrMode::Incomplete(_) => ItemParseError::Parse("incomplete input".to_string()),
        }
    }
}

pub fn parse_items(source: &str) -> Result<Vec<AstItem>, ItemParseError> {
    let tokens = lexer::lex(source)?;
    let mut input: &[Token] = tokens.as_slice();
    let mut items = Vec::new();
    while !input.is_empty() {
        // skip stray semicolons
        if matches_symbol(input.first(), ";") {
            input = &input[1..];
            continue;
        }
        let attrs = parse_outer_attrs(&mut input)?;
        let mut item = parse_item(&mut input).map_err(ItemParseError::from)?;
        if !attrs.is_empty() {
            if let ItemKind::DefFunction(def) = item.kind_mut() {
                def.attrs = attrs;
            }
        }
        items.push(item);
    }
    Ok(items)
}

fn parse_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    alt((
        parse_use_item,
        parse_struct_item,
        parse_enum_item,
        parse_type_item,
        parse_const_item,
        parse_static_item,
        parse_fn_item,
        parse_item_macro,
        parse_expr_item,
    ))
    .parse_next(input)
}

fn parse_use_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Use).parse_next(input)?;
    let tree = parse_use_path(input)?;
    expect_symbol(input, ";")?;
    let import = ItemImport {
        visibility: Visibility::Public,
        tree,
    };
    Ok(Item::from(ItemKind::Import(import)))
}

fn parse_struct_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Struct).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "{")?;
    let mut fields = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            let field_name = expect_ident(input)?;
            expect_symbol(input, ":")?;
            let ty = parse_type(input)?;
            fields.push(StructuralField::new(Ident::new(field_name), ty));
            if match_symbol(input, "}") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }
    let def = ItemDefStruct::new(name.clone(), fields);
    Ok(Item::from(ItemKind::DefStruct(def)))
}

fn parse_const_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Const).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);

    let ty = if match_symbol(input, ":") {
        Some(parse_type(input)?)
    } else {
        None
    };

    expect_symbol(input, "=")?;
    let value_expr: Expr = expr::parse_expr_prec(input, 0)?;
    expect_symbol(input, ";")?;

    let mut def = ItemDefConst {
        ty_annotation: None,
        visibility: Visibility::Public,
        name,
        ty,
        value: Box::new(value_expr),
    };

    Ok(Item::from(ItemKind::DefConst(def)))
}

fn parse_static_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Static).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, ":")?;
    let ty = parse_type(input)?;
    expect_symbol(input, "=")?;
    let value_expr: Expr = expr::parse_expr_prec(input, 0)?;
    expect_symbol(input, ";")?;

    let def = ItemDefStatic {
        ty_annotation: None,
        visibility: Visibility::Public,
        name,
        ty,
        value: Box::new(value_expr),
    };

    Ok(Item::from(ItemKind::DefStatic(def)))
}

fn parse_type_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Type).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "=")?;
    let ty = parse_type(input)?;
    expect_symbol(input, ";")?;

    let def = ItemDefType {
        visibility: Visibility::Public,
        name,
        value: ty,
    };

    Ok(Item::from(ItemKind::DefType(def)))
}

fn parse_enum_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Enum).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "{")?;

    let mut variants = Vec::new();
    if matches_symbol(input.first(), "}") {
        expect_symbol(input, "}")?;
    } else {
        loop {
            let variant_name = Ident::new(expect_ident(input)?);
            let ty = if match_symbol(input, ":") {
                parse_type(input)?
            } else {
                Ty::any()
            };
            let variant = EnumTypeVariant {
                name: variant_name,
                value: ty,
                discriminant: None,
            };
            variants.push(variant);
            if match_symbol(input, "}") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }

    let type_enum = TypeEnum { name: name.clone(), variants };
    let def = ItemDefEnum {
        visibility: Visibility::Public,
        name,
        value: type_enum,
    };

    Ok(Item::from(ItemKind::DefEnum(def)))
}

fn parse_fn_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    keyword_parser(Keyword::Fn).parse_next(input)?;
    let name = Ident::new(expect_ident(input)?);
    expect_symbol(input, "(")?;
    let mut params = Vec::new();
    if matches_symbol(input.first(), ")") {
        expect_symbol(input, ")")?;
    } else {
        loop {
            let param_name = expect_ident(input)?;
            expect_symbol(input, ":")?;
            let ty = parse_type(input)?;
            params.push((Ident::new(param_name), ty));
            if match_symbol(input, ")") {
                break;
            }
            expect_symbol(input, ",")?;
        }
    }
    let body_block = expr::parse_block(input)?;
    let body_expr: Expr = ExprKind::Block(body_block).into();
    let def = ItemDefFunction::new_simple(name, body_expr.into()).with_params(params);
    Ok(Item::from(ItemKind::DefFunction(def)))
}

fn parse_expr_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    // Fallback: treat expression as an item expression (useful for tests)
    let expr_ast = expr::parse_expr_prec(input, 0)?;
    Ok(Item::from(ItemKind::Expr(expr_ast)))
}

fn parse_use_path(input: &mut &[Token]) -> ModalResult<ItemImportTree> {
    let mut path = ItemImportPath::new();
    loop {
        let token = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        let seg = match token.kind {
            TokenKind::Ident => ItemImportTree::Ident(Ident::new(token.lexeme)),
            TokenKind::Keyword(Keyword::Crate) => ItemImportTree::Crate,
            TokenKind::Keyword(Keyword::Super) => ItemImportTree::SuperMod,
            TokenKind::Keyword(Keyword::Use) => return Err(ErrMode::Cut(ContextError::new())),
            TokenKind::Symbol if token.lexeme == "::" => ItemImportTree::Root,
            _ => return Err(ErrMode::Cut(ContextError::new())),
        };
        path.push(seg);
        if match_symbol(input, "::") {
            continue;
        }
        break;
    }
    Ok(ItemImportTree::Path(path))
}

fn parse_item_macro(input: &mut &[Token]) -> ModalResult<AstItem> {
    // Look for a leading path followed by '!'
    // We reuse the type-path parser but require a trailing '!'.
    let snapshot = *input;
    let mut segments = Vec::new();
    if let Ok(seg) = expect_type_ident_segment(input) {
        segments.push(seg);
    } else {
        *input = snapshot;
        return Err(backtrack_err());
    }
    while match_symbol(input, "::") {
        segments.push(expect_type_ident_segment(input)?);
    }

    if !match_symbol(input, "!") {
        *input = snapshot;
        return Err(backtrack_err());
    }

    let path = Path::new(segments);
    // Determine delimiter and capture body similarly to expression macros
    let next = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
    let (delimiter, open, close) = match next.kind {
        TokenKind::Symbol => match next.lexeme.as_str() {
            "(" => (MacroDelimiter::Parenthesis, "(", ")"),
            "[" => (MacroDelimiter::Bracket, "[", "]"),
            "{" => (MacroDelimiter::Brace, "{", "}"),
            _ => return Err(ErrMode::Cut(ContextError::new())),
        },
        _ => return Err(ErrMode::Cut(ContextError::new())),
    };

    let mut depth = 1i32;
    let mut pieces = Vec::new();
    while depth > 0 {
        let tok = advance(input).ok_or_else(|| ErrMode::Cut(ContextError::new()))?;
        if let TokenKind::Symbol = tok.kind {
            if tok.lexeme == open {
                depth += 1;
                pieces.push(tok.lexeme);
                continue;
            }
            if tok.lexeme == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
                pieces.push(tok.lexeme);
                continue;
            }
        }
        pieces.push(tok.lexeme);
    }
    let tokens = pieces.join(" ");
    let invocation = MacroInvocation::new(path, delimiter, tokens);
    Ok(Item::from(ItemKind::Macro(ItemMacro::new(invocation))))
}

fn parse_outer_attrs(input: &mut &[Token]) -> ModalResult<Vec<Attribute>> {
    let mut attrs = Vec::new();
    loop {
        // Look for '#[' pattern
        match input.split_first() {
            Some((Token { kind: TokenKind::Symbol, lexeme, .. }, rest)) if lexeme == "#" => {
                if let Some(Token { kind: TokenKind::Symbol, lexeme: l2, .. }) = rest.first() {
                    if l2 != "[" {
                        break;
                    }
                } else {
                    break;
                }
            }
            _ => break,
        }

        // consume '#' and '['
        expect_symbol(input, "#")?;
        expect_symbol(input, "[")?;

        // parse simple path meta: #[foo::bar]
        let mut segments = Vec::new();
        segments.push(expect_type_ident_segment(input)?);
        while match_symbol(input, "::") {
            segments.push(expect_type_ident_segment(input)?);
        }
        let path = Path::new(segments);
        expect_symbol(input, "]")?;

        let meta = AttrMeta::Path(path);
        attrs.push(Attribute { style: AttrStyle::Outer, meta });
    }
    Ok(attrs)
}

fn parse_type(input: &mut &[Token]) -> ModalResult<Ty> {
    let mut segments = Vec::new();
    segments.push(expect_type_ident_segment(input)?);
    while match_symbol(input, "::") {
        segments.push(expect_type_ident_segment(input)?);
    }
    let path = Path::new(segments);
    Ok(Ty::path(path))
}

fn expect_type_ident_segment(input: &mut &[Token]) -> ModalResult<Ident> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let name = lexeme.clone();
            *input = &input[1..];
            Ok(Ident::new(name))
        }
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Crate),
            ..
        }) => {
            *input = &input[1..];
            Ok(Ident::new("crate".to_string()))
        }
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Super),
            ..
        }) => {
            *input = &input[1..];
            Ok(Ident::new("super".to_string()))
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn expect_ident(input: &mut &[Token]) -> ModalResult<String> {
    match input.first() {
        Some(Token {
            kind: TokenKind::Ident,
            lexeme,
            ..
        }) => {
            let name = lexeme.clone();
            *input = &input[1..];
            Ok(name)
        }
        _ => Err(ErrMode::Cut(ContextError::new())),
    }
}

fn expect_symbol<'a>(input: &mut &'a [Token], sym: &'a str) -> ModalResult<()> {
    symbol_parser(sym)
        .parse_next(input)
        .map_err(|_| ErrMode::Cut(ContextError::new()))
}

fn match_symbol<'a>(input: &mut &'a [Token], sym: &'a str) -> bool {
    symbol_parser(sym).parse_next(input).is_ok()
}

fn matches_symbol(tok: Option<&Token>, sym: &str) -> bool {
    matches!(tok, Some(Token { kind: TokenKind::Symbol, lexeme, .. }) if lexeme == sym)
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let tok = input.first().cloned()?;
    *input = &input[1..];
    Some(tok)
}

fn keyword_parser<'a>(keyword: Keyword) -> impl Parser<&'a [Token], (), ContextError> {
    move |input: &mut &[Token]| {
        match input.first() {
            Some(Token { kind: TokenKind::Keyword(k), .. }) if *k == keyword => {
                *input = &input[1..];
                Ok(())
            }
            _ => Err(backtrack_err()),
        }
    }
}

fn symbol_parser<'a>(sym: &'a str) -> impl Parser<&'a [Token], (), ContextError> {
    let sym_owned = sym.to_string();
    move |input: &mut &[Token]| {
        if matches_symbol(input.first(), &sym_owned) {
            *input = &input[1..];
            Ok(())
        } else {
            Err(backtrack_err())
        }
    }
}
