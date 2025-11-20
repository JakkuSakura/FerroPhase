use super::lex::{
    backtrack_err, is_ident_continue, is_ident_start, parse_cooked_string_literal,
    parse_raw_identifier, parse_raw_string_literal, ws, MULTI_PUNCT, SINGLE_PUNCT,
};
use thiserror::Error;
use winnow::combinator::alt;
use winnow::error::{ContextError, ErrMode};
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
    Return,
    Break,
    Continue,
    Struct,
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
            "return" => Some(Self::Return),
            "break" => Some(Self::Break),
            "continue" => Some(Self::Continue),
            "struct" => Some(Self::Struct),
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
        let kind = match next_raw_token(&mut input) {
            Ok(kind) => kind,
            Err(err) => return Err(err),
        };
        let end = source.len() - input.len();
        let mut lexeme = source[start..end].to_string();
        let kind = match kind {
            RawTokenKind::Ident => {
                if let Some(keyword) = Keyword::from_lexeme(&lexeme) {
                    TokenKind::Keyword(keyword)
                } else {
                    if let Some(stripped) = lexeme.strip_prefix("r#") {
                        lexeme = stripped.to_string();
                    }
                    TokenKind::Ident
                }
            }
            RawTokenKind::Number => TokenKind::Number,
            RawTokenKind::StringLiteral => TokenKind::StringLiteral,
            RawTokenKind::Symbol => TokenKind::Symbol,
        };
        tokens.push(Token {
            kind,
            lexeme,
            span: Span { start, end },
        });
    }
    Ok(tokens)
}

fn next_raw_token(input: &mut &str) -> Result<RawTokenKind, LexerError> {
    token_parser().parse_next(input).map_err(LexerError::from)
}

fn token_parser<'a>() -> impl Parser<&'a str, RawTokenKind, ContextError> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RawTokenKind {
    Ident,
    Number,
    StringLiteral,
    Symbol,
}

fn string_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    parse_cooked_string_literal(input, "").map(|_| RawTokenKind::StringLiteral)
}

fn byte_string_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    parse_cooked_string_literal(input, "b").map(|_| RawTokenKind::StringLiteral)
}

fn raw_string_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    parse_raw_string_literal(input, false).map(|_| RawTokenKind::StringLiteral)
}

fn raw_byte_string_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    parse_raw_string_literal(input, true).map(|_| RawTokenKind::StringLiteral)
}

fn raw_identifier_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    parse_raw_identifier(input).map(|_| RawTokenKind::Ident)
}

fn number_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    let mut chars = input.char_indices();
    let Some((mut idx, ch)) = chars.next() else {
        return Err(backtrack_err());
    };
    if !ch.is_ascii_digit() {
        return Err(backtrack_err());
    }
    idx += ch.len_utf8();
    while let Some((next_idx, ch)) = chars.next() {
        if ch.is_ascii_digit() || ch == '_' {
            idx = next_idx + ch.len_utf8();
        } else {
            break;
        }
    }
    *input = &input[idx..];
    Ok(RawTokenKind::Number)
}

fn ident_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    let mut chars = input.char_indices();
    let Some((mut idx, ch)) = chars.next() else {
        return Err(backtrack_err());
    };
    if !is_ident_start(ch) {
        return Err(backtrack_err());
    }
    idx += ch.len_utf8();
    while let Some((next_idx, ch)) = chars.next() {
        if is_ident_continue(ch) {
            idx = next_idx + ch.len_utf8();
        } else {
            break;
        }
    }
    *input = &input[idx..];
    Ok(RawTokenKind::Ident)
}

fn symbol_token(input: &mut &str) -> ModalResult<RawTokenKind> {
    for sym in MULTI_PUNCT {
        if let Some(rest) = input.strip_prefix(sym) {
            *input = rest;
            return Ok(RawTokenKind::Symbol);
        }
    }
    if let Some(ch) = input.chars().next() {
        if SINGLE_PUNCT.contains(ch) {
            *input = &input[ch.len_utf8()..];
            return Ok(RawTokenKind::Symbol);
        }
    }
    Err(backtrack_err())
}
