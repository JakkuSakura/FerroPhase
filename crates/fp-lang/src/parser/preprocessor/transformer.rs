//! Transformers for converting matched patterns to target syntax.
//!
//! This module provides transformers that take matched patterns and convert
//! them into the appropriate macro invocations.

use crate::parser::preprocessor::token::Token;
use crate::error::PreprocessorError;
use std::fmt::Debug;

/// A transformer that converts matched patterns to target syntax.
pub trait Transformer: Send + Sync + Debug {
    /// Transform the matched tokens starting at the given position.
    fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError>;
    
    /// Get a human-readable description of this transformer.
    fn description(&self) -> String;
}

/// A transformer that converts `quote { ... }` to `fp_quote!(...)`.
#[derive(Debug)]
pub struct QuoteTransformer {
    preserve_kind: bool,
}

impl QuoteTransformer {
    /// Create a new quote transformer.
    pub fn new(preserve_kind: bool) -> Self {
        Self { preserve_kind }
    }
}

impl Transformer for QuoteTransformer {
    fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError> {
        // Tokens expected structure (based on registry pattern):
        // quote [whitespace] [identifier] [whitespace] { ... }
        
        let mut result = String::from("fp_quote!({");
        let mut pos = start;

        // Skip "quote"
        if pos < end && tokens[pos].is_keyword("quote") {
            pos += 1;
        }

        if !self.preserve_kind {
             // Skip optional whitespace
            while pos < end && matches!(tokens[pos], Token::Whitespace(_)) {
                pos += 1;
            }

            // Skip optional identifier
            if pos < end && matches!(tokens[pos], Token::Identifier(_)) {
                pos += 1;
            }

             // Skip optional whitespace again
            while pos < end && matches!(tokens[pos], Token::Whitespace(_)) {
                pos += 1;
            }
        }

        // We expect an opening brace now.
        if pos < end && tokens[pos].text() == "{" {
            pos += 1; // consume '{'
            
            // Copy content up to matching brace
            // Since the pattern matched valid balanced brackets, the last token in the range [start..end)
            // should be the closing brace '}'. Or rather, `end` points AFTER the closing brace?
            // BalancedBracketPattern matches up to and including the closing brace.
            // So tokens[end-1] should be '}'.

            let content_end = if end > pos && tokens[end - 1].text() == "}" {
                end - 1
            } else {
                end
            };

            for i in pos..content_end {
                result.push_str(tokens[i].text());
            }
        } else {
             // Fallback if structure isn't exactly as expected, just dump everything
             for i in pos..end {
                result.push_str(tokens[i].text());
            }
        }
        
        result.push_str("})");
        Ok(result)
    }
    
    fn description(&self) -> String {
        if self.preserve_kind {
            "quote { ... } -> fp_quote!({ ... }) with kind preserved".to_string()
        } else {
            "quote { ... } -> fp_quote!({ ... }) with kind ignored".to_string()
        }
    }
}

/// A transformer that converts `splice (...)` to `fp_splice!(...)`.
#[derive(Debug)]
pub struct SpliceTransformer {
    handle_nested: bool,
}

impl SpliceTransformer {
    /// Create a new splice transformer.
    pub fn new(handle_nested: bool) -> Self {
        Self { handle_nested }
    }
}

impl Transformer for SpliceTransformer {
    fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError> {
        let mut result = String::from("fp_splice!(");
        let mut pos = start;

        // Skip "splice" keyword
        if pos < end && tokens[pos].is_keyword("splice") {
            pos += 1;
        }
        
        // Skip optional whitespace
        while pos < end && matches!(tokens[pos], Token::Whitespace(_)) {
            pos += 1;
        }
        
        // Check for parenthesized form: splice (...)
        if pos < end && tokens[pos].text() == "(" {
            pos += 1; // Skip '('
             // Copy content inside parentheses, but stop before the last matching ')'
            let content_end = if end > pos && tokens[end - 1].text() == ")" {
                end - 1
            } else {
                end
            };

            for i in pos..content_end {
                 result.push_str(tokens[i].text());
            }
        } else if self.handle_nested {
             // Handle nested forms by copying everything else
             // The pattern matcher in registry guarantees we have the full construct
             // e.g. splice quote { ... } or splice fp_quote!(...)

             // If it is `splice quote { ... }`, we want to transform the inner `quote { ... }` too?
             // Or just wrap it?
             // If the inner part is `quote { ... }`, recursively applying transformers would be ideal.
             // But here we are just wrapping it in `fp_splice!(...)`.
             // If we output `fp_splice!(quote { ... })`, the compiler macro will handle it?
             // No, `quote { ... }` is source syntax. We likely want `fp_splice!(fp_quote!(...))` output.

            // Simplest approach: just copy the tokens. The subsequent passes or the macro expansion
            // should handle the inner `quote`.
            // BUT, the registry applies rules until stable.
            // So if we output `fp_splice!(quote { ... })`, the next pass will see `quote { ... }` inside?
            // Wait, `apply_until_stable` re-scans the whole output?
            // Yes. So we just need to emit the text.
            
             for i in pos..end {
                result.push_str(tokens[i].text());
            }
        } else {
             // Simple fallback
            for i in pos..end {
                result.push_str(tokens[i].text());
            }
        }
        
        result.push(')');
        Ok(result)
    }
    
    fn description(&self) -> String {
        if self.handle_nested {
            "splice (...) -> fp_splice!(...) with nested quote support".to_string()
        } else {
            "splice (...) -> fp_splice!(...)".to_string()
        }
    }
}

/// A transformer that wraps tokens in a macro call.
#[derive(Debug)]
pub struct MacroTransformer {
    macro_name: &'static str,
}

impl MacroTransformer {
    /// Create a new macro transformer.
    pub fn new(macro_name: &'static str) -> Self {
        Self { macro_name }
    }
}

impl Transformer for MacroTransformer {
    fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError> {
        // matched tokens: my_macro { ... }
        // We want: my_macro!( my_macro { ... } ) ?
        // Or just: my_macro!( ... ) ?
        //
        // The test expects: `my_macro!( my_macro { content })`
        // Which implies it wraps the *entire match* in the macro call.
        //
        // let tokens = vec![
        //     Token::Identifier("my_macro".to_string()),
        //     Token::Text(" ".to_string()),
        //     Token::Text("{".to_string()),
        //     Token::Text("content".to_string()),
        //     Token::Text("}".to_string()),
        // ];
        // result: "my_macro!( my_macro  { content })"
        
        let mut result = format!("{}!(", self.macro_name);
        
        for i in start..end {
            result.push_str(tokens[i].text());
        }
        
        result.push(')');
        Ok(result)
    }
    
    fn description(&self) -> String {
        format!("macro transformer: {}!(...)", self.macro_name)
    }
}

/// A transformer that replaces matched tokens with a fixed string.
#[derive(Debug)]
pub struct ReplacementTransformer {
    replacement: String,
}

impl ReplacementTransformer {
    /// Create a new replacement transformer.
    pub fn new(replacement: String) -> Self {
        Self { replacement }
    }
}

impl Transformer for ReplacementTransformer {
    fn transform(&self, _tokens: &[Token], _start: usize, _end: usize) -> Result<String, PreprocessorError> {
        Ok(self.replacement.clone())
    }
    
    fn description(&self) -> String {
        format!("replacement: {}", self.replacement)
    }
}

/// A composite transformer that applies multiple transformers in sequence.
#[derive(Debug)]
pub struct CompositeTransformer {
    transformers: Vec<Box<dyn Transformer>>,
}

impl CompositeTransformer {
    /// Create a new composite transformer.
    pub fn new(transformers: Vec<Box<dyn Transformer>>) -> Self {
        Self { transformers }
    }
    
    /// Add a transformer to the composite.
    pub fn add_transformer(mut self, transformer: Box<dyn Transformer>) -> Self {
        self.transformers.push(transformer);
        self
    }
}

impl Transformer for CompositeTransformer {
    fn transform(&self, tokens: &[Token], start: usize, end: usize) -> Result<String, PreprocessorError> {
        let mut result = String::new();
        
        for transformer in &self.transformers {
            let transformed = transformer.transform(tokens, start, end)?;
            result.push_str(&transformed);
        }
        
        Ok(result)
    }
    
    fn description(&self) -> String {
        format!("composite: [{}]",
            self.transformers.iter()
                .map(|t| t.description())
                .collect::<Vec<_>>()
                .join(", "))
    }
}

/// Helper functions for creating common transformers.
pub mod transformers {
    use super::*;
    
    /// Create a quote transformer.
    pub fn quote(preserve_kind: bool) -> QuoteTransformer {
        QuoteTransformer::new(preserve_kind)
    }
    
    /// Create a splice transformer.
    pub fn splice(handle_nested: bool) -> SpliceTransformer {
        SpliceTransformer::new(handle_nested)
    }
    
    /// Create a macro transformer.
    pub fn macro_call(macro_name: &'static str) -> MacroTransformer {
        MacroTransformer::new(macro_name)
    }
    
    /// Create a replacement transformer.
    pub fn replacement(replacement: String) -> ReplacementTransformer {
        ReplacementTransformer::new(replacement)
    }
    
    /// Create a composite transformer.
    pub fn composite(transformers: Vec<Box<dyn Transformer>>) -> CompositeTransformer {
        CompositeTransformer::new(transformers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::preprocessor::token::*;

    #[test]
    fn test_quote_transformer() {
        let tokens = vec![
            Token::Keyword("quote".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Text("{".to_string()),
            Token::Text("1 + 2".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let transformer = QuoteTransformer::new(false);
        let result = transformer.transform(&tokens, 0, 5).unwrap();
        
        assert_eq!(result, "fp_quote!({1 + 2})");
    }

    #[test]
    fn test_splice_transformer_parenthesized() {
        let tokens = vec![
            Token::Keyword("splice".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Text("(".to_string()),
            Token::Text("my_token".to_string()),
            Token::Text(")".to_string()),
        ];
        
        let transformer = SpliceTransformer::new(false);
        let result = transformer.transform(&tokens, 0, 5).unwrap();
        
        assert_eq!(result, "fp_splice!(my_token)");
    }

    #[test]
    fn test_splice_transformer_nested() {
        let tokens = vec![
            Token::Keyword("splice".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Keyword("quote".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Text("{".to_string()),
            Token::Text("let x = 1".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let transformer = SpliceTransformer::new(true);
        let result = transformer.transform(&tokens, 0, 7).unwrap();
        
        // The nested splice logic just copies tokens now.
        // Inner quote expansion relies on recursion in the main loop.
        assert_eq!(result, "fp_splice!(quote {let x = 1})");
    }

    #[test]
    fn test_macro_transformer() {
        let tokens = vec![
            Token::Identifier("my_macro".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Text("{".to_string()),
            Token::Text("content".to_string()),
            Token::Text("}".to_string()),
        ];
        
        let transformer = MacroTransformer::new("my_macro");
        let result = transformer.transform(&tokens, 0, 5).unwrap();
        
        assert_eq!(result, "my_macro!(my_macro {content})");
    }

    #[test]
    fn test_replacement_transformer() {
        let tokens = vec![
            Token::Keyword("old".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Identifier("syntax".to_string()),
        ];
        
        let transformer = ReplacementTransformer::new("new_syntax".to_string());
        let result = transformer.transform(&tokens, 0, 3).unwrap();
        
        assert_eq!(result, "new_syntax");
    }
}