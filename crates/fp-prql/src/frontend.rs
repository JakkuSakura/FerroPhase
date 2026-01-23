use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result as CoreResult;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::query::{QueryDocument, QuerySerializer};

use crate::compile::build_document;

/// Language frontend that captures PRQL pipelines and performs a best-effort
/// lowering to SQL for tooling that expects compiled statements.
#[derive(Debug, Default, Clone)]
pub struct PrqlFrontend;

impl PrqlFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_document(
        &self,
        source: &str,
        path: Option<&Path>,
        target_override: Option<crate::SqlDialect>,
    ) -> QueryDocument {
        build_document(source, path, target_override)
    }
}

impl LanguageFrontend for PrqlFrontend {
    fn language(&self) -> &'static str {
        crate::PRQL
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["prql"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let document = self.build_document(source, path, None);

        if document.is_empty() {
            diagnostics.add_diagnostic(
                Diagnostic::warning("PRQL input is empty".to_string())
                    .with_source_context("prql frontend"),
            );
        }

        let serializer: Arc<dyn AstSerializer> = Arc::new(QuerySerializer::new());
        let serialized = serializer.serialize_query(&document).ok();
        let description = match path {
            Some(path) => format!("PRQL pipeline {}", path.display()),
            None => "PRQL pipeline <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized,
        };

        let node = Node::query(document.clone());

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer,
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}
