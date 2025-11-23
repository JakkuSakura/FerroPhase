use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::parser::FerroPhaseParser;
use crate::parser::lower::lower_expr_from_cst;
use fp_core::ast::Node;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::intrinsics::IntrinsicNormalizer;
use fp_core::Result as CoreResult;
use fp_rust::normalization::normalize_last_to_ast;
use fp_rust::normalization::RustIntrinsicNormalizer;
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;

/// Canonical identifier for the FerroPhase source language.
pub const FERROPHASE: &str = "ferrophase";

/// Frontend that parses FerroPhase sources using the existing Rust infrastructure.
pub struct FerroFrontend {
    parser: Mutex<RustParser>,
    ferro: FerroPhaseParser,
}

impl FerroFrontend {
    pub fn new() -> Self {
        Self {
            parser: Mutex::new(RustParser::new()),
            ferro: FerroPhaseParser::new(),
        }
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }
}

impl LanguageFrontend for FerroFrontend {
    fn language(&self) -> &'static str {
        FERROPHASE
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fp", "ferro", "rs", "rust", "ferrophase"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let serializer = Arc::new(RustPrinter::new_with_rustfmt());
        let intrinsic_normalizer: Arc<dyn IntrinsicNormalizer> =
            Arc::new(RustIntrinsicNormalizer::default());

        if let Some(path) = path {
            let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
            // Avoid panicking on poisoned lock; recover from poison
            let mut parser = match self.parser.lock() {
                Ok(g) => g,
                Err(poison) => poison.into_inner(),
            };
            parser.clear_diagnostics();
            let file = parser.parse_file(&rewritten, path)?;
            let diagnostics = parser.diagnostics();
            drop(parser);

            let last = Node::file(file);
            let mut ast = last.clone();
            normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));
            let snapshot = FrontendSnapshot {
                language: self.language().to_string(),
                description: format!("Rust LAST for {}", path.display()),
                serialized: None,
            };

            return Ok(FrontendResult {
                last,
                ast,
                serializer,
                intrinsic_normalizer: Some(intrinsic_normalizer.clone()),
                snapshot: Some(snapshot),
                diagnostics,
            });
        }

        // Expression-only mode (no resolved file path). Prefer the
        // winnow CST + lowering pipeline when possible, with a
        // fallback to the legacy Rust-based parser.
        if let Ok(cst) = self.ferro.parse_to_cst(&cleaned) {
            if let Some(expr_ast) = lower_expr_from_cst(&cst) {
                let last = Node::expr(expr_ast.clone());
                let mut ast = last.clone();

                // No Rust diagnostics are available on this path; use
                // an empty diagnostic manager for now.
                let diagnostics = Arc::new(fp_core::diagnostics::DiagnosticManager::new());
                normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));

                return Ok(FrontendResult {
                    last,
                    ast,
                    serializer,
                    intrinsic_normalizer: Some(intrinsic_normalizer),
                    snapshot: None,
                    diagnostics,
                });
            }
        }

        let rewritten = self.ferro.rewrite_to_rust(&cleaned)?;
        let (expr, diagnostics) = {
            let parser = match self.parser.lock() {
                Ok(g) => g,
                Err(poison) => poison.into_inner(),
            };
            parser.clear_diagnostics();
            let expr = parser
                .try_parse_as_file(&rewritten)
                .or_else(|_| parser.try_parse_block_expression(&rewritten))
                .or_else(|_| parser.try_parse_simple_expression(&rewritten))?;
            let diagnostics = parser.diagnostics();
            (expr, diagnostics)
        };

        let last = Node::expr(expr.as_ref().clone());
        let mut ast = last.clone();
        normalize_last_to_ast(&mut ast, Some(diagnostics.as_ref()));

        Ok(FrontendResult {
            last,
            ast,
            serializer,
            intrinsic_normalizer: Some(intrinsic_normalizer),
            snapshot: None,
            diagnostics,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn language_identifier_is_ferrophase() {
        let frontend = FerroFrontend::new();
        assert_eq!(frontend.language(), FERROPHASE);
    }
}

pub mod parser;
pub mod lexer;
