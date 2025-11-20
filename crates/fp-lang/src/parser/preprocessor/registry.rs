//! Rule registry for managing preprocessing rules.
//!
//! This module provides a registry system that manages available preprocessing rules,
//! their priorities, and allows for dynamic configuration.

use std::collections::HashMap;
use std::sync::Arc;
use crate::parser::preprocessor::token::Token;
use crate::parser::preprocessor::pattern::Pattern;
use crate::parser::preprocessor::transformer::Transformer;
use crate::error::PreprocessorError;

/// A preprocessing rule that combines a pattern with a transformer.
#[derive(Debug, Clone)]
pub struct Rule {
    name: Option<String>,
    priority: i32,
    pattern: Arc<Box<dyn Pattern>>,
    transformer: Arc<Box<dyn Transformer>>,
}

impl Rule {
    /// Create a new rule with the given pattern and transformer.
    pub fn new<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        pattern: P,
        transformer: T,
    ) -> Self {
        Self {
            name: None,
            priority: 0,
            pattern: Arc::new(Box::new(pattern)),
            transformer: Arc::new(Box::new(transformer)),
        }
    }
    
    /// Create a new named rule with the given priority.
    pub fn named<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        name: String,
        priority: i32,
        pattern: P,
        transformer: T,
    ) -> Self {
        Self {
            name: Some(name),
            priority,
            pattern: Arc::new(Box::new(pattern)),
            transformer: Arc::new(Box::new(transformer)),
        }
    }
    
    /// Check if this rule matches the given tokens starting at the given position.
    pub fn matches(&self, tokens: &[Token], pos: usize) -> Option<usize> {
        self.pattern.matches(tokens, pos)
    }
    
    /// Transform the matched tokens.
    pub fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError> {
        self.transformer.transform(tokens, start, end)
    }
    
    /// Get the rule's name.
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }
    
    /// Get the rule's priority.
    pub fn priority(&self) -> i32 {
        self.priority
    }
    
    /// Get a description of the rule.
    pub fn description(&self) -> String {
        format!("Rule[{}] (priority: {}): {} -> {}", 
            self.name.as_deref().unwrap_or("unnamed"),
            self.priority,
            self.pattern.description(),
            self.transformer.description())
    }
}

/// A registry that manages preprocessing rules.
#[derive(Debug, Default)]
pub struct RuleRegistry {
    rules: Vec<Rule>,
    rule_map: HashMap<String, usize>, // name -> index mapping
}

impl RuleRegistry {
    /// Create a new empty rule registry.
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create a rule registry with default FerroPhase rules.
    pub fn with_default_rules() -> Self {
        let mut registry = Self::new();
        
        use crate::parser::preprocessor::pattern::patterns::*;
        use crate::parser::preprocessor::transformer::transformers::*;
        
        // Quote pattern: quote [kind] { ... }
        // Complex pattern matching:
        // 1. quote { ... }
        // 2. quote kind { ... }
        
        let quote_simple = sequence(vec![
            Box::new(keyword("quote")),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('{', '}')),
        ]);
        
        // Note: This patterns assumes the optional identifier is an Identifier token.
        // But now we must be careful about whitespaces.
        let quote_kind = sequence(vec![
            Box::new(keyword("quote")),
            Box::new(optional_whitespace()),
            Box::new(any_identifier()),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('{', '}')),
        ]);
        
        let quote_pattern = alternative(vec![
            Box::new(quote_kind),
            Box::new(quote_simple),
        ]);

        // Add quote rule
        registry.add_rule(
            quote_pattern,
            quote(false),
        );
        
        // Splice pattern: splice (...) OR splice fp_quote!(...) OR splice quote { ... } OR splice identifier
        // Note: Complex patterns require care.
        // For now, we'll use a simpler pattern and let the transformer handle details if possible,
        // BUT the transformer logic relies on receiving the full match.
        // So we must match fully.
        
        let splice_paren = sequence(vec![
            Box::new(keyword("splice")),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('(', ')')),
        ]);
        
        let splice_quote_macro = sequence(vec![
            Box::new(keyword("splice")),
            Box::new(optional_whitespace()),
            Box::new(text("fp_quote")),
            Box::new(text("!")),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('(', ')')),
        ]);

        let splice_quote_block = sequence(vec![
            Box::new(keyword("splice")),
            Box::new(optional_whitespace()),
            Box::new(keyword("quote")),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('{', '}')),
        ]);

        let splice_ident = sequence(vec![
            Box::new(keyword("splice")),
            Box::new(optional_whitespace()),
            Box::new(any_identifier()),
            // Path support? ::ident
        ]);

        let splice_pattern = alternative(vec![
            Box::new(splice_paren),
            Box::new(splice_quote_macro),
            Box::new(splice_quote_block),
            Box::new(splice_ident),
        ]);
        
        // Add splice rule
        registry.add_rule(
            splice_pattern,
            splice(true),
        );
        
        registry
    }
    
    /// Add a rule to the registry.
    pub fn add_rule<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        &mut self,
        pattern: P,
        transformer: T,
    ) {
        let rule = Rule::new(pattern, transformer);
        self.rules.push(rule);
    }
    
    /// Add a named rule with a specific priority.
    pub fn add_named_rule<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        &mut self,
        name: &str,
        priority: i32,
        pattern: P,
        transformer: T,
    ) {
        let rule = Rule::named(name.to_string(), priority, pattern, transformer);
        self.rule_map.insert(name.to_string(), self.rules.len());
        self.rules.push(rule);
    }
    
    /// Get a rule by name.
    pub fn get_rule(&self, name: &str) -> Option<&Rule> {
        self.rule_map.get(name)
            .and_then(|&index| self.rules.get(index))
    }
    
    /// Remove a rule by name.
    pub fn remove_rule(&mut self, name: &str) -> Option<Rule> {
        if let Some(&index) = self.rule_map.get(name) {
            self.rule_map.remove(name);
            Some(self.rules.remove(index))
        } else {
            None
        }
    }
    
    /// Update a rule's priority.
    pub fn set_priority(&mut self, name: &str, priority: i32) -> bool {
        if let Some(&index) = self.rule_map.get(name) {
            self.rules[index].priority = priority;
            true
        } else {
            false
        }
    }
    
    /// Get all rules sorted by priority (highest first).
    pub fn rules(&self) -> Vec<&Rule> {
        // Sort rules by priority (descending) and then by insertion order
        let mut rules: Vec<_> = self.rules.iter().collect();
        rules.sort_by(|a, b| {
            b.priority.cmp(&a.priority)
                .then(a.name.cmp(&b.name))
        });
        rules
    }
    
    /// Get the number of rules in the registry.
    pub fn len(&self) -> usize {
        self.rules.len()
    }
    
    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }
    
    /// Clear all rules from the registry.
    pub fn clear(&mut self) {
        self.rules.clear();
        self.rule_map.clear();
    }
    
    /// Transform a token using the first matching rule.
    pub fn transform_token(&self, token: Token) -> Result<String, PreprocessorError> {
        // Create a single-token array for pattern matching
        let tokens = vec![token.clone()];
        
        // Try to match rules against this token
        for rule in self.rules() {
            if let Some(end_pos) = rule.matches(&tokens, 0) {
                if end_pos == 1 { // Rule consumed the entire token
                    return rule.transform(&tokens, 0, 1);
                }
            }
        }
        
        // No rule matched, return the original token
        Ok(token.text().to_string())
    }
    
    /// Apply all rules to a sequence of tokens.
    pub fn transform_tokens(&self, tokens: &[Token]) -> Result<String, PreprocessorError> {
        let mut result = String::new();
        let mut pos = 0;
        
        while pos < tokens.len() {
            let mut matched = false;
            
            // Try to match rules starting at the current position
            for rule in self.rules() {
                if let Some(end_pos) = rule.matches(tokens, pos) {
                    // Apply the transformation
                    let transformed = rule.transform(tokens, pos, end_pos)?;
                    result.push_str(&transformed);
                    pos = end_pos;
                    matched = true;
                    break;
                }
            }
            
            if !matched {
                // No rule matched, copy the token as-is
                result.push_str(tokens[pos].text());
                pos += 1;
            }
        }
        
        Ok(result)
    }
    
    /// Find all rules that match a given pattern description.
    pub fn find_rules_by_pattern(&self, pattern_desc: &str) -> Vec<&Rule> {
        self.rules()
            .into_iter()
            .filter(|rule| rule.pattern.description().contains(pattern_desc))
            .collect()
    }
    
    /// Find all rules that use a given transformer description.
    pub fn find_rules_by_transformer(&self, transformer_desc: &str) -> Vec<&Rule> {
        self.rules()
            .into_iter()
            .filter(|rule| rule.transformer.description().contains(transformer_desc))
            .collect()
    }
    
    /// Get a summary of all rules.
    pub fn summary(&self) -> String {
        let mut summary = String::from("Rule Registry Summary:\n");
        summary.push_str("================================\n");
        
        for (i, rule) in self.rules().iter().enumerate() {
            summary.push_str(&format!("{}. {}\n", i + 1, rule.description()));
        }
        
        summary.push_str(&format!("\nTotal rules: {}\n", self.rules.len()));
        summary
    }
}

/// Configuration for the rule registry.
#[derive(Debug, Clone)]
pub struct RegistryConfig {
    /// Default priority for new rules.
    pub default_priority: i32,
    /// Whether to enable verbose logging.
    pub verbose: bool,
    /// Maximum number of rules to apply per token.
    pub max_rules_per_token: usize,
}

impl Default for RegistryConfig {
    fn default() -> Self {
        Self {
            default_priority: 0,
            verbose: false,
            max_rules_per_token: 10,
        }
    }
}

/// A builder for creating rule registries with custom configuration.
pub struct RuleRegistryBuilder {
    config: RegistryConfig,
    rules: Vec<Rule>,
}

impl RuleRegistryBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            config: RegistryConfig::default(),
            rules: Vec::new(),
        }
    }
    
    /// Set the default priority for new rules.
    pub fn with_default_priority(mut self, priority: i32) -> Self {
        self.config.default_priority = priority;
        self
    }
    
    /// Enable verbose logging.
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.config.verbose = verbose;
        self
    }
    
    /// Set the maximum number of rules per token.
    pub fn with_max_rules_per_token(mut self, max: usize) -> Self {
        self.config.max_rules_per_token = max;
        self
    }
    
    /// Add a rule to the builder.
    pub fn add_rule<P: Pattern + Send + Sync + 'static, T: Transformer + Send + Sync + 'static>(
        mut self,
        name: &str,
        priority: i32,
        pattern: P,
        transformer: T,
    ) -> Self {
        let rule = Rule::named(name.to_string(), priority, pattern, transformer);
        self.rules.push(rule);
        self
    }
    
    /// Build the rule registry.
    pub fn build(self) -> RuleRegistry {
        let mut registry = RuleRegistry::new();
        
        for rule in self.rules {
            registry.rules.push(rule);
        }
        
        registry
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::preprocessor::pattern::patterns::*;
    use crate::parser::preprocessor::transformer::transformers::*;

    #[test]
    fn test_rule_registry_basic() {
        let mut registry = RuleRegistry::new();
        
        registry.add_rule(
            keyword("test"),
            macro_call("test_macro"),
        );
        
        assert_eq!(registry.len(), 1);
        assert!(!registry.is_empty());
    }

    #[test]
    fn test_rule_priority_sorting() {
        let mut registry = RuleRegistry::new();
        
        registry.add_named_rule("low", 1, keyword("low"), macro_call("low"));
        registry.add_named_rule("high", 10, keyword("high"), macro_call("high"));
        registry.add_named_rule("medium", 5, keyword("medium"), macro_call("medium"));
        
        let rules: Vec<_> = registry.rules().iter().map(|r| r.name().unwrap()).collect();
        assert_eq!(rules, vec!["high", "medium", "low"]);
    }

    #[test]
    fn test_rule_transformation() {
        let mut registry = RuleRegistry::new();
        
        // We must match the full structure for QuoteTransformer to work correctly now
        let pattern = sequence(vec![
            Box::new(keyword("quote")),
            Box::new(optional_whitespace()),
            Box::new(balanced_brackets('{', '}')),
        ]);

        registry.add_rule(
            pattern,
            quote(false),
        );
        
        let tokens = vec![
            Token::Keyword("quote".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Text("{".to_string()),
            Token::Text(" 1 + 2 ".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let result = registry.transform_tokens(&tokens).unwrap();
        assert!(result.contains("fp_quote!({ 1 + 2 })"));
    }

    #[test]
    fn test_rule_registry_builder() {
        let registry = RuleRegistryBuilder::new()
            .with_default_priority(5)
            .add_rule("test", 10, keyword("test"), macro_call("test_macro"))
            .build();
        
        assert_eq!(registry.len(), 1);
        assert_eq!(registry.rules()[0].priority, 10);
    }

    #[test]
    fn test_rule_summary() {
        let mut registry = RuleRegistry::new();
        
        registry.add_rule(
            keyword("quote"),
            quote(false),
        );
        
        let summary = registry.summary();
        assert!(summary.contains("quote"));
        assert!(summary.contains("fp_quote"));
    }
}