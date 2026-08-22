//! Hand-rolled lexer for the basic Lean 4 subset. ASCII identifiers only
//! (trailing `'` allowed, matching idiomatic Lean primed names); no
//! Unicode — a documented simplification vs. real Lean 4.

use crate::error::LeanParseError;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Def,
    Let,
    If,
    Then,
    Else,
    True,
    False,
    // Literals / identifiers
    Ident(String),
    Int(i64),
    Str(String),
    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    Colon,
    ColonEq,
    Comma,
    Semi,
    SlashSlash, // `//` inside a refinement type: `{x : T // P}`
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Lt,
    Le,
    Gt,
    Ge,
    EqEq,
    NotEq,
    AndAnd,
    OrOr,
    Bang,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub fn lex(source: &str) -> Result<Vec<Token>, LeanParseError> {
    let chars: Vec<char> = source.chars().collect();
    let mut i = 0usize;
    let mut tokens = Vec::new();

    while i < chars.len() {
        let c = chars[i];
        match c {
            ' ' | '\t' | '\r' | '\n' => {
                i += 1;
            }
            '-' if chars.get(i + 1) == Some(&'-') => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '/' if chars.get(i + 1) == Some(&'-') => {
                let start = i;
                i += 2;
                loop {
                    if i + 1 >= chars.len() {
                        let _ = start;
                        return Err(LeanParseError::UnterminatedBlockComment);
                    }
                    if chars[i] == '-' && chars[i + 1] == '/' {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    pos: i,
                });
                i += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    pos: i,
                });
                i += 1;
            }
            '{' => {
                tokens.push(Token {
                    kind: TokenKind::LBrace,
                    pos: i,
                });
                i += 1;
            }
            '}' => {
                tokens.push(Token {
                    kind: TokenKind::RBrace,
                    pos: i,
                });
                i += 1;
            }
            ',' => {
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    pos: i,
                });
                i += 1;
            }
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::Semi,
                    pos: i,
                });
                i += 1;
            }
            ':' if chars.get(i + 1) == Some(&'=') => {
                tokens.push(Token {
                    kind: TokenKind::ColonEq,
                    pos: i,
                });
                i += 2;
            }
            ':' => {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    pos: i,
                });
                i += 1;
            }
            '/' if chars.get(i + 1) == Some(&'/') => {
                tokens.push(Token {
                    kind: TokenKind::SlashSlash,
                    pos: i,
                });
                i += 2;
            }
            '+' => {
                tokens.push(Token {
                    kind: TokenKind::Plus,
                    pos: i,
                });
                i += 1;
            }
            '-' => {
                tokens.push(Token {
                    kind: TokenKind::Minus,
                    pos: i,
                });
                i += 1;
            }
            '*' => {
                tokens.push(Token {
                    kind: TokenKind::Star,
                    pos: i,
                });
                i += 1;
            }
            '/' => {
                tokens.push(Token {
                    kind: TokenKind::Slash,
                    pos: i,
                });
                i += 1;
            }
            '<' if chars.get(i + 1) == Some(&'=') => {
                tokens.push(Token {
                    kind: TokenKind::Le,
                    pos: i,
                });
                i += 2;
            }
            '<' => {
                tokens.push(Token {
                    kind: TokenKind::Lt,
                    pos: i,
                });
                i += 1;
            }
            '>' if chars.get(i + 1) == Some(&'=') => {
                tokens.push(Token {
                    kind: TokenKind::Ge,
                    pos: i,
                });
                i += 2;
            }
            '>' => {
                tokens.push(Token {
                    kind: TokenKind::Gt,
                    pos: i,
                });
                i += 1;
            }
            '=' if chars.get(i + 1) == Some(&'=') => {
                tokens.push(Token {
                    kind: TokenKind::EqEq,
                    pos: i,
                });
                i += 2;
            }
            '!' if chars.get(i + 1) == Some(&'=') => {
                tokens.push(Token {
                    kind: TokenKind::NotEq,
                    pos: i,
                });
                i += 2;
            }
            '!' => {
                tokens.push(Token {
                    kind: TokenKind::Bang,
                    pos: i,
                });
                i += 1;
            }
            '&' if chars.get(i + 1) == Some(&'&') => {
                tokens.push(Token {
                    kind: TokenKind::AndAnd,
                    pos: i,
                });
                i += 2;
            }
            '|' if chars.get(i + 1) == Some(&'|') => {
                tokens.push(Token {
                    kind: TokenKind::OrOr,
                    pos: i,
                });
                i += 2;
            }
            '"' => {
                let start = i;
                i += 1;
                let mut s = String::new();
                loop {
                    match chars.get(i) {
                        None => return Err(LeanParseError::UnterminatedString),
                        Some('"') => {
                            i += 1;
                            break;
                        }
                        Some('\\') => {
                            i += 1;
                            match chars.get(i) {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('\\') => s.push('\\'),
                                Some('"') => s.push('"'),
                                _ => return Err(LeanParseError::UnterminatedString),
                            }
                            i += 1;
                        }
                        Some(c) => {
                            s.push(*c);
                            i += 1;
                        }
                    }
                }
                let _ = start;
                tokens.push(Token {
                    kind: TokenKind::Str(s),
                    pos: start,
                });
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let text: String = chars[start..i].iter().collect();
                let value = text
                    .parse::<i64>()
                    .map_err(|_| LeanParseError::UnexpectedChar(chars[start]))?;
                tokens.push(Token {
                    kind: TokenKind::Int(value),
                    pos: start,
                });
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                let start = i;
                while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '\'')
                {
                    i += 1;
                }
                let text: String = chars[start..i].iter().collect();
                let kind = match text.as_str() {
                    "def" => TokenKind::Def,
                    "let" => TokenKind::Let,
                    "if" => TokenKind::If,
                    "then" => TokenKind::Then,
                    "else" => TokenKind::Else,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    _ => TokenKind::Ident(text),
                };
                tokens.push(Token { kind, pos: start });
            }
            other => return Err(LeanParseError::UnexpectedChar(other)),
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        pos: chars.len(),
    });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_colon_eq_as_one_token() {
        let tokens = lex(":=").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::ColonEq);
    }

    #[test]
    fn strips_line_and_block_comments() {
        let tokens = lex("-- comment\n/- block -/ def").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Def);
        assert_eq!(tokens[1].kind, TokenKind::Eof);
    }

    #[test]
    fn lexes_multi_char_operators() {
        let tokens = lex("<= == != && ||").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Le,
                TokenKind::EqEq,
                TokenKind::NotEq,
                TokenKind::AndAnd,
                TokenKind::OrOr,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn lexes_keywords_and_identifiers() {
        let tokens = lex("def x_1 x'").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Def);
        assert_eq!(tokens[1].kind, TokenKind::Ident("x_1".into()));
        assert_eq!(tokens[2].kind, TokenKind::Ident("x'".into()));
    }
}
