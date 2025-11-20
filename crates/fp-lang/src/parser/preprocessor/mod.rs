//! FerroPhase Preprocessor
//!
//! A simplified and extensible preprocessor that transforms FerroPhase syntax sugar
//! into standard macro invocations that can be parsed by the Rust grammar.
//!
//! ## Architecture
//!
//! The preprocessor consists of:
//! - **Tokenizers**: Extract tokens from source while respecting string/comment boundaries
//! - **Patterns**: Define match patterns for syntax sugar
//! - **Transformers**: Convert matched patterns to target macro syntax
//! - **Registry**: Manages available preprocessing rules and their configuration
//!
//! ## Usage
//!
//! ```rust
//! use fp_lang::parser::preprocessor::{PreprocessorBuilder, registry::RuleRegistry};
//!
//! // Create preprocessor with default rules
//! let preprocessor = PreprocessorBuilder::with_default_rules().build();
//!
//! // Apply preprocessing
//! let source = "quote { 1 + 2 }";
//! let result = preprocessor.apply_until_stable(source, 2).unwrap();
//! assert!(result.contains("fp_quote!"));
//! ```

pub mod token;
pub mod pattern;
pub mod transformer;
pub mod registry;
pub mod error;

use std::sync::Arc;
use error::PreprocessorError;
use token::Tokenizer;
use pattern::Pattern;
use transformer::Transformer;
use registry::RuleRegistry;

/// Preprocessor that applies transformation rules to source code.
pub struct Preprocessor {
    registry: Arc<RuleRegistry>,
    max_passes: usize,
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::with_default_config()
    }
}

impl Preprocessor {
    /// Create a new preprocessor with default configuration.
    pub fn with_default_config() -> Self {
        let registry = RuleRegistry::with_default_rules();
        Self {
            registry: Arc::new(registry),
            max_passes: 2,
        }
    }

    /// Create a new preprocessor with a custom registry.
    pub fn with_registry(registry: RuleRegistry, max_passes: usize) -> Self {
        Self {
            registry: Arc::new(registry),
            max_passes,
        }
    }

    /// Apply all rules once to the source.
    pub fn apply(&self, source: &str) -> Result<String, PreprocessorError> {
        let tokenizer = Tokenizer::new(source);
        let mut tokens = Vec::new();
        
        for token in tokenizer {
            tokens.push(token?);
        }
        
        self.registry.transform_tokens(&tokens)
    }

    /// Apply rules repeatedly until no changes occur or max_passes is reached.
    pub fn apply_until_stable(&self, source: &str, max_passes: usize) -> Result<String, PreprocessorError> {
        let mut passes = 0;
        let mut current = source.to_string();
        
        while passes < max_passes {
            let next = self.apply(&current)?;
            if next == current {
                break; // Stable state reached
            }
            current = next;
            passes += 1;
        }
        
        Ok(current)
    }

    /// Get the current rule registry.
    pub fn registry(&self) -> &Arc<RuleRegistry> {
        &self.registry
    }

    /// Set the maximum number of passes for stabilization.
    pub fn with_max_passes(mut self, max_passes: usize) -> Self {
        self.max_passes = max_passes;
        self
    }
}

/// Builder for creating preprocessor instances with custom configuration.
pub struct PreprocessorBuilder {
    registry: RuleRegistry,
    max_passes: usize,
}

impl PreprocessorBuilder {
    /// Create a new builder with default rules.
    pub fn new() -> Self {
        Self {
            registry: RuleRegistry::with_default_rules(),
            max_passes: 2,
        }
    }

    /// Create a builder with default rules (convenience method).
    pub fn with_default_rules() -> Self {
        Self::new()
    }

    /// Add a custom pattern and transformer pair.
    pub fn add_rule<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        mut self,
        pattern: P,
        transformer: T,
    ) -> Self {
        self.registry.add_rule(pattern, transformer);
        self
    }

    /// Add a named rule with a specific priority.
    pub fn add_named_rule<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        mut self,
        name: &'static str,
        priority: i32,
        pattern: P,
        transformer: T,
    ) -> Self {
        self.registry.add_named_rule(name, priority, pattern, transformer);
        self
    }

    /// Set the maximum number of passes for stabilization.
    pub fn with_max_passes(mut self, max_passes: usize) -> Self {
        self.max_passes = max_passes;
        self
    }

    /// Build the preprocessor.
    pub fn build(self) -> Preprocessor {
        Preprocessor::with_registry(self.registry, self.max_passes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocessor_builder() {
        let preprocessor = PreprocessorBuilder::new()
            .with_max_passes(3)
            .build();
        
        assert_eq!(preprocessor.max_passes, 3);
    }

    #[test]
    fn test_apply_until_stable() {
        let preprocessor = Preprocessor::with_default_config();
        let source = "quote { 1 + 2 }";
        let result = preprocessor.apply_until_stable(source, 2).unwrap();
        
        assert!(result.contains("fp_quote!("));
        assert!(result.contains("1 + 2"));
    }
}