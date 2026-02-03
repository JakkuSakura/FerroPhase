//! Go frontend backed by tree-sitter-go.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::AstSerializer;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, LanguageFrontend};

use crate::parser::GoParser;
use crate::serializer::GoSerializer;

/// Canonical identifier for the Go frontend.
pub const GOLANG: &str = "go";

/// Frontend that converts Go source code into FerroPhase AST.
pub struct GoFrontend {
    serializer: Arc<GoSerializer>,
}

impl GoFrontend {
    pub fn new() -> Self {
        Self {
            serializer: Arc::new(GoSerializer::default()),
        }
    }
}

impl Default for GoFrontend {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageFrontend for GoFrontend {
    fn language(&self) -> &'static str {
        GOLANG
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["go"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let mut parser = GoParser::new().map_err(|err| CoreError::from(err.to_string()))?;
        let node = parser
            .parse_str(source)
            .map_err(|err| CoreError::from(err.to_string()))?;

        let description = match path {
            Some(path) => format!("Go source {}", path.display()),
            None => "Go source <stdin>".to_string(),
        };

        let snapshot = fp_core::frontend::FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized: None,
        };

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer: self.serializer.clone() as Arc<dyn AstSerializer>,
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}
