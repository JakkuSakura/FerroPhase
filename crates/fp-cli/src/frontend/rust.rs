use std::path::Path;
use std::sync::{Arc, Mutex};

use fp_core::Result as CoreResult;
use fp_core::ast::{BExpr, File, Node};
use fp_rust::normalization::normalize_last_to_ast;
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;

use super::{FrontendResult, FrontendSnapshot, LanguageFrontend};

pub struct RustFrontend {
    parser: Mutex<RustParser>,
}

impl RustFrontend {
    pub fn new() -> Self {
        Self {
            parser: Mutex::new(RustParser::new()),
        }
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }

    fn parse_expression(&self, source: &str) -> CoreResult<BExpr> {
        // Try parsing as file, block expression, and finally simple expression.
        let parser = self.parser.lock().unwrap();

        if let Ok(expr) = parser.try_parse_as_file(source) {
            return Ok(expr);
        }

        if let Ok(expr) = parser.try_parse_block_expression(source) {
            return Ok(expr);
        }

        parser.try_parse_simple_expression(source)
    }

    fn parse_file(&self, source: &str, path: &Path) -> CoreResult<File> {
        let mut parser = self.parser.lock().unwrap();
        parser.parse_file(source, path)
    }
}

impl LanguageFrontend for RustFrontend {
    fn language(&self) -> &'static str {
        crate::languages::FERROPHASE
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fp", "ferro", "rs", "rust", "ferrophase"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let cleaned = self.clean_source(source);
        let serializer = Arc::new(RustPrinter::new());

        if let Some(path) = path {
            let last = Node::file(self.parse_file(&cleaned, path)?);
            let mut ast = last.clone();
            normalize_last_to_ast(&mut ast);
            let snapshot = FrontendSnapshot {
                language: self.language().to_string(),
                description: format!("Rust LAST for {}", path.display()),
                serialized: None,
            };

            return Ok(FrontendResult {
                last,
                ast,
                serializer,
                snapshot: Some(snapshot),
            });
        }

        // Fall back to expression parsing when no explicit path is provided.
        let expr = self.parse_expression(&cleaned)?;
        let last = Node::expr(expr.as_ref().clone());
        let mut ast = last.clone();
        normalize_last_to_ast(&mut ast);

        Ok(FrontendResult {
            last,
            ast,
            serializer,
            snapshot: None,
        })
    }
}
