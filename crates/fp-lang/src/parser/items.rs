use fp_core::ast::{
    Expr, ExprKind, Ident, Item, Item as AstItem, ItemDefFunction, ItemDefStruct, ItemImport,
    ItemImportPath, ItemImportTree, ItemKind, Visibility,
};
use thiserror::Error;
use winnow::error::{ContextError, ErrMode};
use winnow::ModalResult;

use super::expr;
use super::lexer::{self, Keyword, LexerError, Token, TokenKind};

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
        let item = parse_item(&mut input).map_err(ItemParseError::from)?;
        items.push(item);
    }
    Ok(items)
}

fn parse_item(input: &mut &[Token]) -> ModalResult<AstItem> {
    if match_keyword(input, Keyword::Use) {
        let tree = parse_use_path(input)?;
        expect_symbol(input, ";")?;
        let import = ItemImport {
            visibility: Visibility::Public,
            tree,
        };
        return Ok(Item::from(ItemKind::Import(import)));
    }
    if match_keyword(input, Keyword::Struct) {
        let name = Ident::new(expect_ident(input)?);
        // struct fields skipped for now; accept empty body
        expect_symbol(input, "{")?;
        expect_symbol(input, "}")?;
        let def = ItemDefStruct::new(name.clone(), Vec::new());
        return Ok(Item::from(ItemKind::DefStruct(def)));
    }
    if match_keyword(input, Keyword::Fn) {
        let name = Ident::new(expect_ident(input)?);
        expect_symbol(input, "(")?;
        expect_symbol(input, ")")?;
        let body_block = expr::parse_block_tokens(input)?;
        let body_expr: Expr = ExprKind::Block(body_block).into();
        let def = ItemDefFunction::new_simple(name, body_expr.into());
        return Ok(Item::from(ItemKind::DefFunction(def)));
    }
    // Fallback: treat expression as an item expression (useful for tests)
    let expr_ast = expr::parse_expr_tokens(input)?;
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

fn parse_expr_tokens(input: &mut &[Token]) -> ModalResult<Expr> {
    expr::parse_expr_prec(input, 0)
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

fn match_keyword(input: &mut &[Token], keyword: Keyword) -> bool {
    if peek_keyword(input, keyword) {
        *input = &input[1..];
        true
    } else {
        false
    }
}

fn peek_keyword(input: &[Token], keyword: Keyword) -> bool {
    matches!(
        input.first(),
        Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) if *k == keyword
    )
}

fn expect_symbol(input: &mut &[Token], sym: &str) -> ModalResult<()> {
    if match_symbol(input, sym) {
        Ok(())
    } else {
        Err(ErrMode::Cut(ContextError::new()))
    }
}

fn match_symbol(input: &mut &[Token], sym: &str) -> bool {
    if matches_symbol(input.first(), sym) {
        *input = &input[1..];
        true
    } else {
        false
    }
}

fn matches_symbol(tok: Option<&Token>, sym: &str) -> bool {
    matches!(tok, Some(Token { kind: TokenKind::Symbol, lexeme, .. }) if lexeme == sym)
}

fn advance(input: &mut &[Token]) -> Option<Token> {
    let tok = input.first().cloned()?;
    *input = &input[1..];
    Some(tok)
}
