//! Pattern matching for preprocessor rules.
//!
//! This module provides declarative pattern matching capabilities that allow
//! rules to specify what syntax they should match in a clear, composable way.

use crate::parser::preprocessor::token::Token;
use crate::error::PreprocessorError;
use std::fmt::Debug;

/// A pattern that can match against sequences of tokens.
pub trait Pattern: Send + Sync + Debug {
    /// Check if this pattern matches the given sequence of tokens starting at the given position.
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize>;
    
    /// Get a human-readable description of this pattern.
    fn description(&self) -> String;
}

/// A pattern that matches a specific keyword.
#[derive(Debug)]
pub struct KeywordPattern {
    keyword: &'static str,
}

impl KeywordPattern {
    /// Create a new pattern that matches the given keyword.
    pub fn new(keyword: &'static str) -> Self {
        Self { keyword }
    }
}

impl Pattern for KeywordPattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        if pos >= tokens.len() {
            return None;
        }
        
        match &tokens[pos] {
            Token::Keyword(k) if k == self.keyword => Some(pos + 1),
            _ => None,
        }
    }
    
    fn description(&self) -> String {
        format!("keyword: {}", self.keyword)
    }
}

/// A pattern that matches exact text.
#[derive(Debug)]
pub struct TextPattern {
    text: String,
}

impl TextPattern {
    /// Create a new pattern that matches exact text.
    pub fn new(text: impl Into<String>) -> Self {
        Self { text: text.into() }
    }
}

impl Pattern for TextPattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        if pos >= tokens.len() {
            return None;
        }
        
        match &tokens[pos] {
            Token::Text(s) if s == &self.text => Some(pos + 1),
            _ => None,
        }
    }
    
    fn description(&self) -> String {
        format!("text: '{}'", self.text)
    }
}

/// A pattern that matches an identifier with a specific name.
#[derive(Debug)]
pub struct IdentifierPattern {
    identifier: &'static str,
}

impl IdentifierPattern {
    /// Create a new pattern that matches the given identifier.
    pub fn new(identifier: &'static str) -> Self {
        Self { identifier }
    }
}

impl Pattern for IdentifierPattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        if pos >= tokens.len() {
            return None;
        }
        
        match &tokens[pos] {
            Token::Identifier(id) if id == self.identifier => Some(pos + 1),
            _ => None,
        }
    }
    
    fn description(&self) -> String {
        format!("identifier: {}", self.identifier)
    }
}

/// A pattern that matches any identifier.
#[derive(Debug)]
pub struct AnyIdentifierPattern;

impl Pattern for AnyIdentifierPattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        if pos >= tokens.len() {
            return None;
        }
        
        matches!(&tokens[pos], Token::Identifier(_)).then_some(pos + 1)
    }
    
    fn description(&self) -> String {
        "any identifier".to_string()
    }
}

/// A pattern that matches an opening bracket and finds its matching closing bracket.
#[derive(Debug)]
pub struct BalancedBracketPattern {
    open_char: char,
    close_char: char,
}

impl BalancedBracketPattern {
    /// Create a new pattern that matches balanced brackets.
    pub fn new(open_char: char, close_char: char) -> Self {
        Self { open_char, close_char }
    }
}

impl Pattern for BalancedBracketPattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        if pos >= tokens.len() {
            return None;
        }
        
        // Find the opening bracket token
        let _open_token = match &tokens[pos] {
            Token::Text(s) if s.chars().next() == Some(self.open_char) => s,
            _ => return None,
        };
        
        let mut current_pos = pos + 1;
        let mut depth = 1;
        
        while current_pos < tokens.len() && depth > 0 {
            let token = &tokens[current_pos];
            
            match token {
                Token::Text(s) => {
                    for c in s.chars() {
                        match c {
                            _ if c == self.open_char => depth += 1,
                            _ if c == self.close_char => {
                                depth -= 1;
                                if depth == 0 {
                                    return Some(current_pos + 1);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            
            current_pos += 1;
        }
        
        None
    }
    
    fn description(&self) -> String {
        format!("balanced brackets: {}{{...}}{}", self.open_char, self.close_char)
    }
}

/// A pattern that matches a sequence of patterns in order.
#[derive(Debug)]
pub struct SequencePattern {
    patterns: Vec<Box<dyn Pattern>>,
}

impl SequencePattern {
    /// Create a new pattern that matches a sequence of sub-patterns.
    pub fn new(patterns: Vec<Box<dyn Pattern>>) -> Self {
        Self { patterns }
    }
    
    /// Add a pattern to the sequence.
    pub fn add_pattern(mut self, pattern: Box<dyn Pattern>) -> Self {
        self.patterns.push(pattern);
        self
    }
}

impl Pattern for SequencePattern {
    fn matches(&self, tokens: &[Token], mut pos: usize) -> Option<usize> {
        for pattern in &self.patterns {
            pos = pattern.matches(tokens, pos)?;
        }
        Some(pos)
    }
    
    fn description(&self) -> String {
        format!("sequence: [{}]", 
            self.patterns.iter()
                .map(|p| p.description())
                .collect::<Vec<_>>()
                .join(", "))
    }
}

/// A pattern that matches one of several alternative patterns.
#[derive(Debug)]
pub struct AlternativePattern {
    patterns: Vec<Box<dyn Pattern>>,
}

impl AlternativePattern {
    /// Create a new pattern that matches any of the given alternatives.
    pub fn new(patterns: Vec<Box<dyn Pattern>>) -> Self {
        Self { patterns }
    }
    
    /// Add an alternative pattern.
    pub fn add_alternative(mut self, pattern: Box<dyn Pattern>) -> Self {
        self.patterns.push(pattern);
        self
    }
}

impl Pattern for AlternativePattern {
    fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        for pattern in &self.patterns {
            if let Some(new_pos) = pattern.matches(tokens, pos) {
                return Some(new_pos);
            }
        }
        None
    }
    
    fn description(&self) -> String {
        format!("alternative: [{}]", 
            self.patterns.iter()
                .map(|p| p.description())
                .collect::<Vec<_>>()
                .join(" | "))
    }
}

/// A pattern that matches zero or more of a given pattern.
#[derive(Debug)]
pub struct ZeroOrMorePattern {
    pattern: Box<dyn Pattern>,
}

impl ZeroOrMorePattern {
    /// Create a new pattern that matches zero or more repetitions of the given pattern.
    pub fn new(pattern: Box<dyn Pattern>) -> Self {
        Self { pattern }
    }
}

impl Pattern for ZeroOrMorePattern {
    fn matches(&self, tokens: &[Token], mut pos: usize) -> Option<usize> {
        while let Some(new_pos) = self.pattern.matches(tokens, pos) {
            pos = new_pos;
        }
        Some(pos)
    }
    
    fn description(&self) -> String {
        format!("zero or more: {}", self.pattern.description())
    }
}

/// A pattern that matches one or more of a given pattern.
#[derive(Debug)]
pub struct OneOrMorePattern {
    pattern: Box<dyn Pattern>,
}

impl OneOrMorePattern {
    /// Create a new pattern that matches one or more repetitions of the given pattern.
    pub fn new(pattern: Box<dyn Pattern>) -> Self {
        Self { pattern }
    }
}

impl Pattern for OneOrMorePattern {
    fn matches(&self, tokens: &[Token], mut pos: usize) -> Option<usize> {
        let mut first_match = false;
        
        while let Some(new_pos) = self.pattern.matches(tokens, pos) {
            first_match = true;
            pos = new_pos;
        }
        
        first_match.then_some(pos)
    }
    
    fn description(&self) -> String {
        format!("one or more: {}", self.pattern.description())
    }
}

/// A pattern that matches optional whitespace.
#[derive(Debug)]
pub struct OptionalWhitespacePattern;

impl Pattern for OptionalWhitespacePattern {
    fn matches(&self, tokens: &[Token], mut pos: usize) -> Option<usize> {
        while pos < tokens.len() {
            match &tokens[pos] {
                Token::Whitespace(_) => {
                    pos += 1;
                }
                _ => break,
            }
        }
        Some(pos)
    }
    
    fn description(&self) -> String {
        "optional whitespace".to_string()
    }
}

/// Helper functions for creating common patterns.
pub mod patterns {
    use super::*;
    
    /// Create a pattern that matches a specific keyword.
    pub fn keyword(keyword: &'static str) -> KeywordPattern {
        KeywordPattern::new(keyword)
    }
    
    /// Create a pattern that matches exact text.
    pub fn text(text: impl Into<String>) -> TextPattern {
        TextPattern::new(text)
    }

    /// Create a pattern that matches a specific identifier.
    pub fn identifier(identifier: &'static str) -> IdentifierPattern {
        IdentifierPattern::new(identifier)
    }
    
    /// Create a pattern that matches any identifier.
    pub fn any_identifier() -> AnyIdentifierPattern {
        AnyIdentifierPattern
    }
    
    /// Create a pattern that matches balanced brackets.
    pub fn balanced_brackets(open: char, close: char) -> BalancedBracketPattern {
        BalancedBracketPattern::new(open, close)
    }
    
    /// Create a pattern that matches a sequence of patterns.
    pub fn sequence(patterns: Vec<Box<dyn Pattern>>) -> SequencePattern {
        SequencePattern::new(patterns)
    }
    
    /// Create a pattern that matches alternative patterns.
    pub fn alternative(patterns: Vec<Box<dyn Pattern>>) -> AlternativePattern {
        AlternativePattern::new(patterns)
    }
    
    /// Create a pattern that matches zero or more of a pattern.
    pub fn zero_or_more(pattern: Box<dyn Pattern>) -> ZeroOrMorePattern {
        ZeroOrMorePattern::new(pattern)
    }
    
    /// Create a pattern that matches one or more of a pattern.
    pub fn one_or_more(pattern: Box<dyn Pattern>) -> OneOrMorePattern {
        OneOrMorePattern::new(pattern)
    }
    
    /// Create a pattern that matches optional whitespace.
    pub fn optional_whitespace() -> OptionalWhitespacePattern {
        OptionalWhitespacePattern
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_pattern() {
        let tokens = vec![
            Token::Keyword("quote".to_string()),
            Token::Text("{".to_string()),
            Token::Text("1".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let pattern = KeywordPattern::new("quote");
        assert_eq!(pattern.matches(&tokens, 0), Some(1));
        assert_eq!(pattern.matches(&tokens, 1), None);
    }

    #[test]
    fn test_identifier_pattern() {
        let tokens = vec![
            Token::Identifier("my_var".to_string()),
            Token::Text("=".to_string()),
        ];
        
        let pattern = IdentifierPattern::new("my_var");
        assert_eq!(pattern.matches(&tokens, 0), Some(1));
        assert_eq!(pattern.matches(&tokens, 1), None);
    }

    #[test]
    fn test_sequence_pattern() {
        let tokens = vec![
            Token::Keyword("quote".to_string()),
            Token::Text("{".to_string()),
            Token::Text("1".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let pattern = SequencePattern::new(vec![
            Box::new(KeywordPattern::new("quote")),
            Box::new(TextPattern::new("{")),
        ]);
        
        assert_eq!(pattern.matches(&tokens, 0), Some(2));
        assert_eq!(pattern.matches(&tokens, 1), None);
    }

    #[test]
    fn test_alternative_pattern() {
        let tokens1 = vec![
            Token::Keyword("quote".to_string()),
            Token::Text("{".to_string()),
        ];
        
        let tokens2 = vec![
            Token::Keyword("splice".to_string()),
            Token::Text("(".to_string()),
        ];
        
        let pattern = AlternativePattern::new(vec![
            Box::new(KeywordPattern::new("quote")),
            Box::new(KeywordPattern::new("splice")),
        ]);
        
        assert_eq!(pattern.matches(&tokens1, 0), Some(1));
        assert_eq!(pattern.matches(&tokens2, 0), Some(1));
        assert_eq!(pattern.matches(&tokens1, 1), None);
    }

    #[test]
    fn test_balanced_bracket_pattern() {
        let tokens = vec![
            Token::Text("{".to_string()),
            Token::Text("1".to_string()),
            Token::Text("+".to_string()),
            Token::Text("2".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let pattern = BalancedBracketPattern::new('{', '}');
        assert_eq!(pattern.matches(&tokens, 0), Some(5));
    }
}