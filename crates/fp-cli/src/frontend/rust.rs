use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{BExpr, File, Node};
use fp_rust::parser::RustParser;
use fp_rust::printer::RustPrinter;

use crate::CliError;

use super::{FrontendResult, FrontendSnapshot, LanguageFrontend};

pub struct RustFrontend {
    parser: RustParser,
}

impl RustFrontend {
    pub fn new() -> Self {
        Self {
            parser: RustParser::new(),
        }
    }

    fn clean_source(&self, source: &str) -> String {
        if source.starts_with("#!") {
            source.lines().skip(1).collect::<Vec<_>>().join("\n")
        } else {
            source.to_string()
        }
    }

    fn parse_expression(&self, source: &str) -> Result<BExpr, CliError> {
        // Try parsing as file, block expression, and finally simple expression.
        if let Ok(expr) = self
            .parser
            .try_parse_as_file(source)
            .map_err(|e| CliError::Compilation(e.to_string()))
        {
            return Ok(expr);
        }

        if let Ok(expr) = self
            .parser
            .try_parse_block_expression(source)
            .map_err(|e| CliError::Compilation(e.to_string()))
        {
            return Ok(expr);
        }

        self.parser
            .try_parse_simple_expression(source)
            .map_err(|e| CliError::Compilation(e.to_string()))
    }

    fn parse_file(&self, source: &str, path: &Path) -> Result<File, CliError> {
        let syn_file: syn::File = syn::parse_file(source).map_err(|e| {
            CliError::Compilation(format!("Failed to parse {} as file: {}", path.display(), e))
        })?;

        self.parser
            .parse_file_content(path.to_path_buf(), syn_file)
            .map_err(|e| {
                CliError::Compilation(format!("Failed to lower file {}: {}", path.display(), e))
            })
    }
}

impl LanguageFrontend for RustFrontend {
    fn language(&self) -> &'static str {
        crate::languages::FERROPHASE
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fp", "ferro", "rs", "rust", "ferrophase"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> Result<FrontendResult, CliError> {
        let cleaned = self.clean_source(source);
        let serializer = Arc::new(RustPrinter::new());

        if let Some(path) = path {
            let file = self.parse_file(&cleaned, path)?;
            let snapshot = FrontendSnapshot {
                language: self.language().to_string(),
                description: format!("Rust LAST for {}", path.display()),
                serialized: None,
            };

            return Ok(FrontendResult {
                ast: Node::File(file),
                serializer,
                snapshot: Some(snapshot),
            });
        }

        // Fall back to expression parsing when no explicit path is provided.
        let expr = self.parse_expression(&cleaned)?;

        Ok(FrontendResult {
            ast: Node::Expr(*expr),
            serializer,
            snapshot: None,
        })
    }
}
