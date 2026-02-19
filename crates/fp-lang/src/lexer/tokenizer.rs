use super::lexeme::Lexeme;
#[cfg(test)]
use super::lexeme::LexemeKind;
use super::winnow::{
    backtrack_err, block_comment, is_ident_continue, is_ident_start, line_comment,
    parse_cooked_string_literal, parse_raw_identifier, parse_raw_string_literal, whitespace, ws,
    MULTI_PUNCT, SINGLE_PUNCT,
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
    Move,
    Struct,
    Enum,
    Type,
    Static,
    Mod,
    Trait,
    Impl,
    Where,
    Use,
    Extern,
    Super,
    Crate,
    As,
    Pub,
}

impl Keyword {
    pub(crate) fn from_lexeme(lexeme: &str) -> Option<Self> {
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
            "move" => Some(Self::Move),
            "struct" => Some(Self::Struct),
            "enum" => Some(Self::Enum),
            "type" => Some(Self::Type),
            "static" => Some(Self::Static),
            "mod" => Some(Self::Mod),
            "trait" => Some(Self::Trait),
            "impl" => Some(Self::Impl),
            "where" => Some(Self::Where),
            "use" => Some(Self::Use),
            "extern" => Some(Self::Extern),
            "super" => Some(Self::Super),
            "crate" => Some(Self::Crate),
            "as" => Some(Self::As),
            "pub" => Some(Self::Pub),
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
    #[error("lexer error: {message}")]
    Message { message: String, span: Option<Span> },
}

impl LexerError {
    pub fn span(&self) -> Option<Span> {
        match self {
            LexerError::Message { span, .. } => *span,
        }
    }

    fn with_span(message: String, span: Span) -> Self {
        LexerError::Message {
            message,
            span: Some(span),
        }
    }
}

// Strip a type suffix from a number lexeme (e.g., "10i64" -> "10", "1.2f32" -> "1.2").
// Suffix is defined as the first alphabetic character that is *not* part of an exponent,
// followed by alphanumeric/underscore chars.
//
// NOTE: Transitional helper before the "literal semantics" TODO is addressed.
// The lexer/token layer preserves the raw literal text, while the parser/value layer currently
// parses only the numeric portion into i64/f64 and ignores any suffix.
pub(crate) fn strip_number_suffix(lexeme: &str) -> &str {
    let mut numeric_part = lexeme;
    for (idx, ch) in lexeme.char_indices() {
        if ch.is_ascii_alphabetic() {
            // Treat exponent markers (`e`/`E` [+/-]digits) as numeric, not suffix start.
            let is_exponent = (ch == 'e' || ch == 'E')
                && idx > 0
                && lexeme[idx + ch.len_utf8()..]
                    .chars()
                    .next()
                    .map(|next| {
                        if next == '+' || next == '-' {
                            lexeme[idx + ch.len_utf8() + next.len_utf8()..]
                                .chars()
                                .next()
                                .map(|c| c.is_ascii_digit())
                                .unwrap_or(false)
                        } else {
                            next.is_ascii_digit()
                        }
                    })
                    .unwrap_or(false);
            if is_exponent {
                continue;
            }
            numeric_part = &lexeme[..idx];
            break;
        }
    }
    numeric_part
}

pub(crate) fn classify_lexeme(lexeme: &str) -> Option<TokenKind> {
    let mut input = lexeme;
    let kind = token_parser().parse_next(&mut input).ok()?;
    if input.is_empty() {
        Some(kind)
    } else {
        None
    }
}

pub(crate) fn classify_and_normalize_lexeme(lexeme: &str) -> Option<(TokenKind, String)> {
    let kind = classify_lexeme(lexeme)?;
    let mut normalized = lexeme.to_string();
    let kind = match kind {
        TokenKind::Ident => {
            if let Some(keyword) = Keyword::from_lexeme(&normalized) {
                TokenKind::Keyword(keyword)
            } else {
                if let Some(stripped) = normalized.strip_prefix("r#") {
                    normalized = stripped.to_string();
                }
                TokenKind::Ident
            }
        }
        TokenKind::Number => TokenKind::Number,
        other => other,
    };
    Some((kind, normalized))
}

impl From<ContextError> for LexerError {
    fn from(err: ContextError) -> Self {
        LexerError::Message {
            message: err.to_string(),
            span: None,
        }
    }
}

impl From<ErrMode<ContextError>> for LexerError {
    fn from(err: ErrMode<ContextError>) -> Self {
        match err {
            ErrMode::Backtrack(ctx) | ErrMode::Cut(ctx) => LexerError::from(ctx),
            ErrMode::Incomplete(_) => LexerError::Message {
                message: "incomplete input".to_string(),
                span: None,
            },
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
            Err(err) => {
                let span = Span {
                    start,
                    end: (start + 1).min(source.len()),
                };
                return Err(LexerError::with_span(err.to_string(), span));
            }
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

pub fn lex_lexemes(source: &str) -> Result<Vec<Lexeme>, LexerError> {
    let mut input = source;
    let mut out = Vec::new();
    while !input.is_empty() {
        let before = input;
        let start = source.len() - before.len();

        if whitespace.parse_next(&mut input).is_ok() {
            let end = source.len() - input.len();
            out.push(Lexeme::trivia_whitespace(
                source[start..end].to_string(),
                Span { start, end },
            ));
            continue;
        }
        if line_comment.parse_next(&mut input).is_ok() {
            let end = source.len() - input.len();
            out.push(Lexeme::trivia_line_comment(
                source[start..end].to_string(),
                Span { start, end },
            ));
            continue;
        }
        if block_comment.parse_next(&mut input).is_ok() {
            let end = source.len() - input.len();
            out.push(Lexeme::trivia_block_comment(
                source[start..end].to_string(),
                Span { start, end },
            ));
            continue;
        }

        token_parser().parse_next(&mut input).map_err(|err| {
            let span = Span {
                start,
                end: (start + 1).min(source.len()),
            };
            LexerError::with_span(err.to_string(), span)
        })?;
        let end = source.len() - input.len();
        out.push(Lexeme::token(
            source[start..end].to_string(),
            Span { start, end },
        ));
    }
    Ok(out)
}

fn token_parser<'a>() -> impl Parser<&'a str, TokenKind, ContextError> {
    alt((
        raw_byte_string_token,
        raw_string_token,
        byte_string_token,
        f_string_token,
        t_string_token,
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

fn f_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_cooked_string_literal(input, "f").map(|_| TokenKind::StringLiteral)
}

fn t_string_token(input: &mut &str) -> ModalResult<TokenKind> {
    parse_cooked_string_literal(input, "t").map(|_| TokenKind::StringLiteral)
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
    // Simple decimal number literal with optional fractional part.
    // Grammar: [0-9][0-9_]*( "." [0-9][0-9_]* )?
    let s = *input;
    let mut end; // end of the numeric part (without suffix)
    let mut iter = s.char_indices().peekable();

    // first digit
    match iter.peek() {
        Some(&(idx, ch)) if ch.is_ascii_digit() => {
            end = idx + ch.len_utf8();
            iter.next();
        }
        _ => return Err(backtrack_err()),
    }
    // integer tail
    while let Some(&(idx, ch)) = iter.peek() {
        if ch.is_ascii_digit() || ch == '_' {
            end = idx + ch.len_utf8();
            iter.next();
        } else {
            break;
        }
    }
    // optional fractional part, but avoid consuming range operator `..`
    if let Some(&(dot_idx, '.')) = iter.peek() {
        if !s[dot_idx..].starts_with("..") {
            // look ahead one more char to ensure it's a digit
            if let Some((_, next_ch)) = { iter.clone().nth(1) } {
                if next_ch.is_ascii_digit() {
                    // consume '.'
                    end = dot_idx + 1;
                    iter.next();
                    // consume fraction digits / underscores
                    while let Some(&(idx, ch)) = iter.peek() {
                        if ch.is_ascii_digit() || ch == '_' {
                            end = idx + ch.len_utf8();
                            iter.next();
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }

    // Optional type suffix: alphabetic start, then alphanumeric/underscore.
    let mut suffix_end = end;
    let mut suffix_iter = s[end..].char_indices().peekable();
    if let Some(&(off, ch)) = suffix_iter.peek() {
        if ch.is_ascii_alphabetic() {
            suffix_end = end + off + ch.len_utf8();
            suffix_iter.next();
            while let Some(&(off, ch)) = suffix_iter.peek() {
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    suffix_end = end + off + ch.len_utf8();
                    suffix_iter.next();
                } else {
                    break;
                }
            }
        }
    }

    let _lexeme = &s[..suffix_end];
    *input = &s[suffix_end..];
    Ok(TokenKind::Number)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn snapshot_from_lexemes(src: &str) -> Vec<(TokenKind, String)> {
        lex_lexemes(src)
            .expect("lex_lexemes")
            .into_iter()
            .filter(|l| !l.is_trivia())
            .map(|l| classify_and_normalize_lexeme(&l.text).expect("classify_lexeme"))
            .collect()
    }

    fn snapshot_from_lexer(src: &str) -> Vec<(TokenKind, String)> {
        lex(src)
            .expect("lex")
            .into_iter()
            .map(|t| (t.kind, t.lexeme))
            .collect()
    }

    #[test]
    fn lexemes_match_lexer_tokens_ignoring_trivia() {
        let src = r#"
            fn main(){ // hello
              let r#type=10i64;
              let y=a>>b;
              let z = quote { splice ( token ) };
              let k = const { 1 };
              let f = 2.5;
              let g = 1.5f64;
              let r = 0..10;
              /* block */
            }
        "#;
        assert_eq!(snapshot_from_lexemes(src), snapshot_from_lexer(src));
    }

    #[test]
    fn lexemes_preserve_trivia_text() {
        let src = "a /*x*/ b //y\nc";
        let lexemes = lex_lexemes(src).expect("lex_lexemes");
        assert!(
            lexemes
                .iter()
                .any(|l| l.text == "/*x*/" && l.kind == LexemeKind::TriviaBlockComment),
            "missing block comment trivia"
        );
        assert!(
            lexemes
                .iter()
                .any(|l| l.text == "//y\n" && l.kind == LexemeKind::TriviaLineComment),
            "missing line comment trivia"
        );
    }
}
