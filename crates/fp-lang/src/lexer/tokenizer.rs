use super::winnow::{
    backtrack_err, is_ident_continue, is_ident_start, parse_cooked_string_literal,
    parse_raw_identifier, parse_raw_string_literal, ws, MULTI_PUNCT, SINGLE_PUNCT,
};
use thiserror::Error;
use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
use winnow::token::take_while;
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Quote,
    Splice,
    Const,
    Emit,
    Let,
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
    In,
    Match,
    Mut,
    Await,
    Async,
    Return,
    Break,
    Continue,
    Struct,
    Enum,
    Type,
    Static,
    Mod,
    Trait,
    Impl,
    Use,
    Super,
    Crate,
}

impl Keyword {
    fn from_lexeme(lexeme: &str) -> Option<Self> {
        match lexeme {
            "quote" => Some(Self::Quote),
            "splice" => Some(Self::Splice),
            "const" => Some(Self::Const),
            "emit" => Some(Self::Emit),
            "let" => Some(Self::Let),
            "fn" => Some(Self::Fn),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "loop" => Some(Self::Loop),
            "while" => Some(Self::While),
            "for" => Some(Self::For),
            "in" => Some(Self::In),
            "match" => Some(Self::Match),
            "mut" => Some(Self::Mut),
            "await" => Some(Self::Await),
            "async" => Some(Self::Async),
            "return" => Some(Self::Return),
            "break" => Some(Self::Break),
            "continue" => Some(Self::Continue),
            "struct" => Some(Self::Struct),
            "enum" => Some(Self::Enum),
            "type" => Some(Self::Type),
            "static" => Some(Self::Static),
            "mod" => Some(Self::Mod),
            "trait" => Some(Self::Trait),
            "impl" => Some(Self::Impl),
            "use" => Some(Self::Use),
            "super" => Some(Self::Super),
            "crate" => Some(Self::Crate),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Number,
    StringLiteral,
    Symbol,
    Keyword(Keyword),
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("lexer error: {0}")]
    Message(String),
}

impl From<ContextError> for LexerError {
    fn from(err: ContextError) -> Self {
        LexerError::Message(err.to_string())
    }
}

impl From<ErrMode<ContextError>> for LexerError {
    fn from(err: ErrMode<ContextError>) -> Self {
        match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => LexerError::from(ctx),
            ErrMode::Incomplete(_) => LexerError::Message("incomplete input".to_string()),
        }
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut input = source;
    let mut tokens = Vec::new();
    while !input.is_empty() {
        ws.parse_next(&mut input).map_err(LexerError::from)?;
        if input.is_empty() {
            break;
        }
        let start = source.len() - input.len();
        let kind = match token_parser().parse_next(&mut input) {
            Ok(kind) => kind,
            Err(err) => return Err(LexerError::from(err)),
        };
        let end = source.len() - input.len();
        let mut lexeme = source[start..end].to_string();
        let kind = match kind {
            TokenKind::Ident => {
                if let Some(keyword) = Keyword::from_lexeme(&lexeme) {
                    TokenKind::Keyword(keyword)
                } else {
                    if let Some(stripped) = lexeme.strip_prefix("r#") {
                        lexeme = stripped.to_string();
                    }
                    TokenKind::Ident
                }
            }
            other => other,
        };
        tokens.push(Token {
            kind,
            lexeme,
            span: Span { start, end },
        });
    }
    Ok(tokens)
}

fn token_parser<'a>() -> impl Parser<&'a str, TokenKind, ContextError> {
    alt((
        raw_byte_string_token,
        raw_string_token,
        byte_string_token,
        string_token,
        raw_identifier_token,
        number_token,
        ident_token,
        symbol_token,
    ))
}

fn string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_cooked_string_literal(input, "").map(|_| TokenKind::StringLiteral)
}

fn byte_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_cooked_string_literal(input, "b").map(|_| TokenKind::StringLiteral)
}

fn raw_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_raw_string_literal(input, false).map(|_| TokenKind::StringLiteral)
}

fn raw_byte_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_raw_string_literal(input, true).map(|_| TokenKind::StringLiteral)
}

fn raw_identifier_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_raw_identifier(input).map(|_| TokenKind::Ident)
}

fn number_token(input: &mut &str) -> ModalResult<TokenKind> {
    (
        take_while(1.., |c: char| c.is_ascii_digit()),
        take_while(0.., |c: char| c.is_ascii_digit() || c == '_'),
    )
        .map(|_| TokenKind::Number)
        .parse_next(input)
}

fn ident_token(input: &mut &str) -> ModalResult<TokenKind> {
    (
        take_while(1.., is_ident_start),
        take_while(0.., is_ident_continue),
    )
        .parse_next(input)
        .map(|_| TokenKind::Ident)
}

fn symbol_token(input: &mut &str) -> ModalResult<TokenKind> {
    alt((
        multi_punct_token.map(|_| TokenKind::Symbol),
        single_punct_token.map(|_| TokenKind::Symbol),
    ))
    .parse_next(input)
}

fn multi_punct_token(input: &mut &str) -> ModalResult<&'static str> {
    for sym in MULTI_PUNCT {
        if let Some(rest) = input.strip_prefix(sym) {
            *input = rest;
            return Ok(*sym);
        }
    }
    Err(backtrack_err())
}

fn single_punct_token(input: &mut &str) -> ModalResult<char> {
    take_while(1..=1, |c: char| SINGLE_PUNCT.contains(c))
        .map(|s: &str| s.chars().next().unwrap())
        .parse_next(input)
}
