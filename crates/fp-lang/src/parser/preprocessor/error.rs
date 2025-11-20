//! Error types for the preprocessor.

use std::fmt;
use thiserror::Error;

/// Preprocessor errors.
#[derive(Debug, Error, Clone, PartialEq)]
pub enum PreprocessorError {
    /// Unexpected end of input.
    #[error("Unexpected end of input")]
    UnexpectedEof,
    
    /// Unexpected character.
    #[error("Unexpected character: '{0}'")]
    UnexpectedChar(char),
    
    /// Invalid token.
    #[error("Invalid token: {0}")]
    InvalidToken(String),
    
    /// Pattern matching failed.
    #[error("Pattern matching failed: {0}")]
    PatternMatchFailed(String),
    
    /// Transformation failed.
    #[error("Transformation failed: {0}")]
    TransformationFailed(String),
    
    /// Rule not found.
    #[error("Rule not found: {0}")]
    RuleNotFound(String),
    
    /// Invalid rule configuration.
    #[error("Invalid rule configuration: {0}")]
    InvalidRuleConfig(String),
    
    /// Registry error.
    #[error("Registry error: {0}")]
    RegistryError(String),
    
    /// Tokenization error.
    #[error("Tokenization error: {0}")]
    TokenizationError(String),
    
    /// Configuration error.
    #[error("Configuration error: {0}")]
    ConfigError(String),
}

impl PreprocessorError {
    /// Create a new unexpected EOF error.
    pub fn unexpected_eof() -> Self {
        Self::UnexpectedEof
    }
    
    /// Create a new unexpected character error.
    pub fn unexpected_char(c: char) -> Self {
        Self::UnexpectedChar(c)
    }
    
    /// Create a new invalid token error.
    pub fn invalid_token(token: impl Into<String>) -> Self {
        Self::InvalidToken(token.into())
    }
    
    /// Create a new pattern matching failed error.
    pub fn pattern_match_failed(msg: impl Into<String>) -> Self {
        Self::PatternMatchFailed(msg.into())
    }
    
    /// Create a new transformation failed error.
    pub fn transformation_failed(msg: impl Into<String>) -> Self {
        Self::TransformationFailed(msg.into())
    }
    
    /// Create a new rule not found error.
    pub fn rule_not_found(name: impl Into<String>) -> Self {
        Self::RuleNotFound(name.into())
    }
    
    /// Create a new invalid rule configuration error.
    pub fn invalid_rule_config(msg: impl Into<String>) -> Self {
        Self::InvalidRuleConfig(msg.into())
    }
    
    /// Create a new registry error.
    pub fn registry_error(msg: impl Into<String>) -> Self {
        Self::RegistryError(msg.into())
    }
    
    /// Create a new tokenization error.
    pub fn tokenization_error(msg: impl Into<String>) -> Self {
        Self::TokenizationError(msg.into())
    }
    
    /// Create a new configuration error.
    pub fn config_error(msg: impl Into<String>) -> Self {
        Self::ConfigError(msg.into())
    }
}

/// Result type for preprocessor operations.
pub type PreprocessorResult<T> = Result<T, PreprocessorError>;

/// Diagnostic information about preprocessing.
#[derive(Debug, Clone, PartialEq)]
pub struct PreprocessorDiagnostic {
    /// The error/warning message.
    message: String,
    /// The severity level.
    level: DiagnosticLevel,
    /// The position in the source where this occurred.
    position: Option<usize>,
    /// Context information.
    context: Option<String>,
    /// Suggestions for fixing the issue.
    suggestions: Vec<String>,
}

/// Diagnostic severity levels.
#[derive(Debug, Clone, PartialEq, Copy, Eq)]
pub enum DiagnosticLevel {
    /// Informational message.
    Info,
    /// Warning that doesn't prevent processing.
    Warning,
    /// Error that prevents processing.
    Error,
}

impl PreprocessorDiagnostic {
    /// Create a new diagnostic.
    pub fn new(message: String, level: DiagnosticLevel) -> Self {
        Self {
            message,
            level,
            position: None,
            context: None,
            suggestions: Vec::new(),
        }
    }
    
    /// Create an info diagnostic.
    pub fn info(message: impl Into<String>) -> Self {
        Self::new(message.into(), DiagnosticLevel::Info)
    }
    
    /// Create a warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(message.into(), DiagnosticLevel::Warning)
    }
    
    /// Create an error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(message.into(), DiagnosticLevel::Error)
    }
    
    /// Set the position of this diagnostic.
    pub fn with_position(mut self, position: usize) -> Self {
        self.position = Some(position);
        self
    }
    
    /// Set the context for this diagnostic.
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }
    
    /// Add a suggestion for fixing this issue.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }
    
    /// Add multiple suggestions.
    pub fn with_suggestions(mut self, suggestions: Vec<impl Into<String>>) -> Self {
        self.suggestions.extend(suggestions.into_iter().map(Into::into));
        self
    }
    
    /// Get the message.
    pub fn message(&self) -> &str {
        &self.message
    }
    
    /// Get the level.
    pub fn level(&self) -> DiagnosticLevel {
        self.level
    }
    
    /// Get the position.
    pub fn position(&self) -> Option<usize> {
        self.position
    }
    
    /// Get the context.
    pub fn context(&self) -> Option<&str> {
        self.context.as_deref()
    }
    
    /// Get the suggestions.
    pub fn suggestions(&self) -> &[String] {
        &self.suggestions
    }
    
    /// Check if this is an error.
    pub fn is_error(&self) -> bool {
        matches!(self.level, DiagnosticLevel::Error)
    }
    
    /// Check if this is a warning.
    pub fn is_warning(&self) -> bool {
        matches!(self.level, DiagnosticLevel::Warning)
    }
    
    /// Check if this is informational.
    pub fn is_info(&self) -> bool {
        matches!(self.level, DiagnosticLevel::Info)
    }
}

impl fmt::Display for PreprocessorDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.level {
            DiagnosticLevel::Error => write!(f, "Error: ")?,
            DiagnosticLevel::Warning => write!(f, "Warning: ")?,
            DiagnosticLevel::Info => write!(f, "Info: ")?,
        }
        
        write!(f, "{}", self.message)?;
        
        if let Some(pos) = self.position {
            write!(f, " at position {}", pos)?;
        }
        
        if let Some(ctx) = &self.context {
            write!(f, "\n  Context: {}", ctx)?;
        }
        
        if !self.suggestions.is_empty() {
            write!(f, "\n  Suggestions:")?;
            for suggestion in &self.suggestions {
                write!(f, "\n    - {}", suggestion)?;
            }
        }
        
        Ok(())
    }
}

/// A collection of diagnostics.
#[derive(Debug, Clone, Default)]
pub struct DiagnosticCollection {
    diagnostics: Vec<PreprocessorDiagnostic>,
}

impl DiagnosticCollection {
    /// Create a new empty diagnostic collection.
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Add a diagnostic.
    pub fn add(&mut self, diagnostic: PreprocessorDiagnostic) {
        self.diagnostics.push(diagnostic);
    }
    
    /// Add multiple diagnostics.
    pub fn add_many(&mut self, diagnostics: Vec<PreprocessorDiagnostic>) {
        self.diagnostics.extend(diagnostics);
    }
    
    /// Get all diagnostics.
    pub fn diagnostics(&self) -> &[PreprocessorDiagnostic] {
        &self.diagnostics
    }
    
    /// Get all errors.
    pub fn errors(&self) -> Vec<&PreprocessorDiagnostic> {
        self.diagnostics.iter().filter(|d| d.is_error()).collect()
    }
    
    /// Get all warnings.
    pub fn warnings(&self) -> Vec<&PreprocessorDiagnostic> {
        self.diagnostics.iter().filter(|d| d.is_warning()).collect()
    }
    
    /// Get all info messages.
    pub fn infos(&self) -> Vec<&PreprocessorDiagnostic> {
        self.diagnostics.iter().filter(|d| d.is_info()).collect()
    }
    
    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.errors().is_empty()
    }
    
    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        self.warnings().is_empty()
    }
    
    /// Check if the collection is empty.
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
    
    /// Clear all diagnostics.
    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }
    
    /// Get the number of diagnostics.
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }
}

impl fmt::Display for DiagnosticCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.diagnostics.is_empty() {
            write!(f, "No diagnostics")?;
            return Ok(());
        }
        
        for diagnostic in &self.diagnostics {
            writeln!(f, "{}", diagnostic)?;
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocessor_error() {
        let error = PreprocessorError::unexpected_char('x');
        assert_eq!(error.to_string(), "Unexpected character: 'x'");
        
        let error = PreprocessorError::rule_not_found("test_rule");
        assert_eq!(error.to_string(), "Rule not found: test_rule");
    }

    #[test]
    fn test_preprocessor_diagnostic() {
        let diagnostic = PreprocessorDiagnostic::error("Test error")
            .with_position(10)
            .with_context("Some context")
            .with_suggestion("Fix this");
        
        assert!(diagnostic.is_error());
        assert_eq!(diagnostic.position(), Some(10));
        assert_eq!(diagnostic.context(), Some("Some context"));
        assert_eq!(diagnostic.suggestions(), &["Fix this"]);
    }

    #[test]
    fn test_diagnostic_collection() {
        let mut collection = DiagnosticCollection::new();
        
        collection.add(PreprocessorDiagnostic::warning("Test warning"));
        collection.add(PreprocessorDiagnostic::error("Test error"));
        
        assert_eq!(collection.len(), 2);
        assert!(!collection.has_errors());
        assert_eq!(collection.errors().len(), 1);
        assert_eq!(collection.warnings().len(), 1);
    }
}