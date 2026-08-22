//! `LanguageFrontend` implementation for the basic Lean 4 subset.

use std::path::Path;
use std::sync::Arc;

use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

use crate::serializer::LeanSerializer;

/// Canonical identifier for the Lean frontend.
pub const LEAN: &str = "lean";

#[derive(Debug, Default, Clone)]
pub struct LeanFrontend;

impl LeanFrontend {
    pub fn new() -> Self {
        Self
    }
}

impl LanguageFrontend for LeanFrontend {
    fn language(&self) -> &'static str {
        LEAN
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["lean"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let mut file = crate::parser::parse_file(source).map_err(CoreError::from)?;
        if let Some(path) = path {
            file.path = path.to_path_buf();
        }

        let description = match path {
            Some(path) => format!("Lean source {}", path.display()),
            None => "Lean source <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized: None,
        };

        Ok(FrontendResult {
            ast: file,
            serializer: Arc::new(LeanSerializer) as Arc<dyn fp_core::ast::AstSerializer>,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}
