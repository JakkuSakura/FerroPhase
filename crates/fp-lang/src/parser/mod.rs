//! FerroPhase parser powered by tree-sitter (superset of Rust)
//!
//! This module provides a thin wrapper around a tree-sitter parser configured
//! with the Rust grammar. It applies a dedicated preprocessor (parser::preprocessor)
//! to translate FerroPhase sugar (quote/splice) to macro-invocation forms the
//! Rust grammar can accept.

use eyre::Result;
use tree_sitter::{Language as TsLanguage, Parser as TsParser, Tree as TsTree};

pub mod preprocessor;

/// Tree-sitter Rust language handle
fn ts_rust_language() -> TsLanguage { tree_sitter_rust::language() }

/// Parser for the FerroPhase language (Rust superset) backed by tree-sitter.
pub struct FerroPhaseParser {
    ts: TsParser,
    pre: preprocessor::Preprocessor,
}

impl FerroPhaseParser {
    /// Create a new parser instance with the Rust grammar.
    pub fn new() -> Result<Self> {
        let mut ts = TsParser::new();
        ts.set_language(&ts_rust_language())?;
        Ok(Self { ts, pre: preprocessor::Preprocessor::default() })
    }

    /// Parse source and produce a tree-sitter parse tree.
    pub fn parse_to_cst(&mut self, source: &str) -> Result<TsTree> {
        let preprocessed = self.pre.apply_until_stable(source, 2).map_err(|e| eyre::eyre!(e))?;
        let tree = self
            .ts
            .parse(&preprocessed, None)
            .ok_or_else(|| eyre::eyre!("tree-sitter returned no tree"))?;
        Ok(tree)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_rust_like_source() {
        let mut p = FerroPhaseParser::new().unwrap();
        let t = p.parse_to_cst("fn main() { println!(\"hi\"); }").unwrap();
        assert!(t.root_node().has_error() == false);
    }

    #[test]
    fn rewrites_quote_and_splice() {
        let mut p = FerroPhaseParser::new().unwrap();
        let _ = p.parse_to_cst("fn main() { let x = splice some_token; let y = quote { 1 + 2 }; }").unwrap();
    }

    #[test]
    fn nested_quote_splice_and_control_flow() {
        let mut p = FerroPhaseParser::new().unwrap();
        let src = r#"
            fn main() {
                if true { let _ = quote { splice ( z ); }; }
                loop { let _ = quote { 1 + 2 }; break; }
                while false { let _ = splice ( quote { 3 } ); }
            }
        "#;
        let t = p.parse_to_cst(src).unwrap();
        assert!(!t.root_node().has_error());
    }
}
