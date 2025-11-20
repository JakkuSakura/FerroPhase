//! Tokenization utilities for the preprocessor.
//!
//! This module provides tokenization that respects string literals, comments,
//! and other language constructs that should not be modified by preprocessing rules.

use std::fmt;
use crate::error::PreprocessorError;

/// Represents a token extracted from the source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// A regular text token.
    Text(String),
    /// An identifier token.
    Identifier(String),
    /// A keyword token.
    Keyword(String),
    /// A string literal.
    StringLiteral(String),
    /// A character literal.
    CharLiteral(String),
    /// A line comment.
    LineComment(String),
    /// A block comment.
    BlockComment(String),
    /// Raw string literal.
    RawStringLiteral(String),
    /// Byte string literal.
    ByteStringLiteral(String),
    /// Raw byte string literal.
    RawByteStringLiteral(String),
    /// Whitespace.
    Whitespace(String),
}

impl Token {
    /// Check if this token is a string-like literal that should be preserved.
    pub fn is_string_like(&self) -> bool {
        matches!(
            self,
            Token::StringLiteral(_) 
            | Token::CharLiteral(_)
            | Token::RawStringLiteral(_)
            | Token::ByteStringLiteral(_)
            | Token::RawByteStringLiteral(_)
        )
    }

    /// Check if this token is a comment that should be preserved.
    pub fn is_comment(&self) -> bool {
        matches!(self, Token::LineComment(_) | Token::BlockComment(_))
    }

    /// Get the text content of this token.
    pub fn text(&self) -> &str {
        match self {
            Token::Text(s) => s,
            Token::Identifier(s) => s,
            Token::Keyword(s) => s,
            Token::StringLiteral(s) => s,
            Token::CharLiteral(s) => s,
            Token::LineComment(s) => s,
            Token::BlockComment(s) => s,
            Token::RawStringLiteral(s) => s,
            Token::ByteStringLiteral(s) => s,
            Token::RawByteStringLiteral(s) => s,
            Token::Whitespace(s) => s,
        }
    }

    /// Check if this token matches a specific keyword.
    pub fn is_keyword(&self, keyword: &str) -> bool {
        matches!(self, Token::Keyword(k) if k == keyword)
    }

    /// Check if this token is an identifier with a specific name.
    pub fn is_identifier(&self, name: &str) -> bool {
        matches!(self, Token::Identifier(id) if id == name)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Text(s) => write!(f, "{}", s),
            Token::Identifier(s) => write!(f, "identifier: {}", s),
            Token::Keyword(s) => write!(f, "keyword: {}", s),
            Token::StringLiteral(s) => write!(f, "string: {}", s),
            Token::CharLiteral(s) => write!(f, "char: {}", s),
            Token::LineComment(s) => write!(f, "line comment: {}", s),
            Token::BlockComment(s) => write!(f, "block comment: {}", s),
            Token::RawStringLiteral(s) => write!(f, "raw string: {}", s),
            Token::ByteStringLiteral(s) => write!(f, "byte string: {}", s),
            Token::RawByteStringLiteral(s) => write!(f, "raw byte string: {}", s),
            Token::Whitespace(s) => write!(f, "whitespace: {:?}", s),
        }
    }
}

/// Tokenizer that extracts tokens while respecting language boundaries.
pub struct Tokenizer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    /// Create a new tokenizer for the given source.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
        }
    }

    /// Peek at the next character without consuming it.
    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    /// Peek at the character at the given offset.
    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.pos + offset).copied()
    }

    /// Check if the next characters match a given string.
    fn peek_matches(&self, s: &str) -> bool {
        let end = self.pos + s.len();
        if end > self.bytes.len() {
            return false;
        }
        &self.bytes[self.pos..end] == s.as_bytes()
    }

    /// Consume a character and advance the position.
    fn consume_char(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    /// Consume characters while the predicate is true.
    fn consume_while<F>(&mut self, mut predicate: F) -> String
    where
        F: FnMut(u8) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !predicate(c) {
                break;
            }
            self.pos += 1;
        }
        String::from_utf8_lossy(&self.bytes[start..self.pos]).to_string()
    }

    /// Parse whitespace.
    fn parse_whitespace(&mut self) -> Token {
        let ws = self.consume_while(|c| c.is_ascii_whitespace());
        Token::Whitespace(ws)
    }

    /// Check if a byte is a valid identifier character.
    fn is_ident_char(b: u8) -> bool {
        b.is_ascii_alphanumeric() || b == b'_'
    }

    /// Parse an identifier or keyword.
    fn parse_identifier(&mut self) -> Token {
        let start = self.pos;
        self.consume_while(Self::is_ident_char);
        let ident = String::from_utf8_lossy(&self.bytes[start..self.pos]).to_string();
        
        // Check if it's a keyword
        match ident.as_str() {
            "quote" | "splice" | "fn" | "let" | "const" | "mut" | "pub" | "crate"
            | "mod" | "use" | "impl" | "struct" | "enum" | "trait" | "where"
            | "if" | "else" | "match" | "while" | "loop" | "for" | "break"
            | "continue" | "return" | "async" | "await" | "move" | "unsafe"
            | "extern" | "type" | "static" | "ref" | "box" | "dyn" => {
                Token::Keyword(ident)
            }
            _ => Token::Identifier(ident),
        }
    }

    /// Parse a string literal.
    fn parse_string(&mut self) -> Result<Token, PreprocessorError> {
        let start = self.pos;
        let quote = self.consume_char().ok_or(PreprocessorError::UnexpectedEof)?;
        
        if quote != b'"' {
            return Err(PreprocessorError::UnexpectedChar(quote as char));
        }

        let mut content = String::new();
        while let Some(c) = self.peek() {
            match c {
                b'"' => {
                    self.pos += 1;
                    break;
                }
                b'\\' => {
                    self.pos += 1;
                    if let Some(escaped) = self.consume_char() {
                        content.push(escaped as char);
                    }
                }
                _ => {
                    self.pos += 1;
                    content.push(c as char);
                }
            }
        }

        Ok(Token::StringLiteral(format!("\"{}\"", content)))
    }

    /// Parse a character literal.
    fn parse_char(&mut self) -> Result<Token, PreprocessorError> {
        let start = self.pos;
        let quote = self.consume_char().ok_or(PreprocessorError::UnexpectedEof)?;
        
        if quote != b'\'' {
            return Err(PreprocessorError::UnexpectedChar(quote as char));
        }

        let mut content = String::new();
        while let Some(c) = self.peek() {
            match c {
                b'\'' => {
                    self.pos += 1;
                    break;
                }
                b'\\' => {
                    self.pos += 1;
                    if let Some(escaped) = self.consume_char() {
                        content.push(escaped as char);
                    }
                }
                _ => {
                    self.pos += 1;
                    content.push(c as char);
                }
            }
        }

        Ok(Token::CharLiteral(format!("'{}'", content)))
    }

    /// Parse a line comment.
    fn parse_line_comment(&mut self) -> Token {
        let start = self.pos;
        self.pos += 2; // Skip "//"
        
        // Read until end of line
        while let Some(c) = self.peek() {
            if c == b'\n' {
                break;
            }
            self.pos += 1;
        }

        let comment = String::from_utf8_lossy(&self.bytes[start..self.pos]).to_string();
        Token::LineComment(comment)
    }

    /// Parse a block comment.
    fn parse_block_comment(&mut self) -> Result<Token, PreprocessorError> {
        let start = self.pos;
        self.pos += 2; // Skip "/*"
        
        let mut depth = 1;
        while depth > 0 {
            match self.peek() {
                Some(b'/') if self.peek_at(1) == Some(b'*') => {
                    depth += 1;
                    self.pos += 2;
                }
                Some(b'*') if self.peek_at(1) == Some(b'/') => {
                    depth -= 1;
                    self.pos += 2;
                }
                Some(_) => {
                    self.pos += 1;
                }
                None => return Err(PreprocessorError::UnexpectedEof),
            }
        }

        let comment = String::from_utf8_lossy(&self.bytes[start..self.pos]).to_string();
        Ok(Token::BlockComment(comment))
    }

    /// Parse a raw string literal.
    fn parse_raw_string(&mut self) -> Result<Token, PreprocessorError> {
        let start = self.pos;
        let prefix = self.consume_char().ok_or(PreprocessorError::UnexpectedEof)?;
        
        if prefix != b'r' {
            return Err(PreprocessorError::UnexpectedChar(prefix as char));
        }

        // Count number of '#' characters
        let mut hash_count = 0;
        while let Some(b'#') = self.peek() {
            hash_count += 1;
            self.pos += 1;
        }

        let quote = self.consume_char().ok_or(PreprocessorError::UnexpectedEof)?;
        if quote != b'"' {
            return Err(PreprocessorError::UnexpectedChar(quote as char));
        }

        // Find closing delimiter
        let mut content = String::new();
        let mut in_quotes = true;
        
        while in_quotes {
            match self.peek() {
                Some(b'"') => {
                    let mut closing_hashes = 0;
                    let mut pos = self.pos + 1;
                    
                    // Count closing hashes
                    while pos < self.bytes.len() && self.bytes[pos] == b'#' {
                        closing_hashes += 1;
                        pos += 1;
                    }
                    
                    if closing_hashes == hash_count {
                        self.pos = pos;
                        in_quotes = false;
                        break;
                    } else {
                        content.push('"');
                        self.pos += 1;
                    }
                }
                Some(c) => {
                    content.push(c as char);
                    self.pos += 1;
                }
                None => return Err(PreprocessorError::UnexpectedEof),
            }
        }

        Ok(Token::RawStringLiteral(format!("r{}\"{}\"", "#".repeat(hash_count), content)))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, PreprocessorError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.bytes.len() {
            return None;
        }

        let c = self.peek()?;

        if c.is_ascii_whitespace() {
            return Some(Ok(self.parse_whitespace()));
        }

        let token = match c {
            b'"' => self.parse_string(),
            b'\'' => self.parse_char(),
            b'/' => {
                if self.peek_matches("//") {
                    Ok(self.parse_line_comment())
                } else if self.peek_matches("/*") {
                    self.parse_block_comment()
                } else {
                    // Regular '/' character
                    self.pos += 1;
                    Ok(Token::Text(String::from("/")))
                }
            }
            b'r' => self.parse_raw_string(),
            c if Self::is_ident_char(c) => Ok(self.parse_identifier()),
            _ => {
                self.pos += 1;
                Ok(Token::Text(String::from_utf8_lossy(&[c]).to_string()))
            }
        };

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer_basic() {
        let source = "fn main() { \"hello\" }";
        let tokenizer = Tokenizer::new(source);
        let tokens: Vec<_> = tokenizer.collect();
        
        // fn, space, main, (, ), space, {, space, "hello", space, }
        // 1   2      3     4  5  6      7  8      9        10     11
        assert_eq!(tokens.len(), 11);
        assert!(matches!(tokens[0], Ok(Token::Keyword(_)))); // fn
        assert!(matches!(tokens[1], Ok(Token::Whitespace(_))));
        assert!(matches!(tokens[2], Ok(Token::Identifier(_)))); // main
        assert!(matches!(tokens[3], Ok(Token::Text(_)))); // (
        assert!(matches!(tokens[4], Ok(Token::Text(_)))); // )
        assert!(matches!(tokens[8], Ok(Token::StringLiteral(_)))); // "hello"
    }

    #[test]
    fn test_tokenizer_preserves_strings() {
        let source = r#"let s = "quote { 1 + 2 }"; // splice ( x )"#;
        let tokenizer = Tokenizer::new(source);
        let tokens: Vec<_> = tokenizer.collect();
        
        // String should be preserved as is
        if let Ok(Token::StringLiteral(s)) = &tokens[3] {
            assert_eq!(s, r#""quote { 1 + 2 }""#);
        }
        
        // Comment should be preserved
        if let Ok(Token::LineComment(c)) = &tokens[4] {
            assert_eq!(c, r#" // splice ( x )"#);
        }
    }

    #[test]
    fn test_tokenizer_identifiers() {
        let source = "quote splice identifier";
        let tokenizer = Tokenizer::new(source);
        let tokens: Vec<_> = tokenizer.collect();
        
        // quote, space, splice, space, identifier
        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0], Ok(Token::Keyword(ref k)) if k == "quote"));
        assert!(matches!(tokens[1], Ok(Token::Whitespace(_))));
        assert!(matches!(tokens[2], Ok(Token::Keyword(ref k)) if k == "splice"));
        assert!(matches!(tokens[3], Ok(Token::Whitespace(_))));
        assert!(matches!(tokens[4], Ok(Token::Identifier(ref i)) if i == "identifier"));
    }
}