use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result as CoreResult;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::query::{QueryDocument, QueryKind, QuerySerializer};

use crate::SqlDialect;

/// Basic SQL frontend that tokenises query statements into a query document.
#[derive(Debug, Clone)]
pub struct SqlFrontend {
    dialect: SqlDialect,
}

impl SqlFrontend {
    /// Create a frontend that assumes a generic SQL dialect.
    pub fn new() -> Self {
        Self {
            dialect: SqlDialect::Generic,
        }
    }

    /// Create a frontend that targets a concrete SQL dialect.
    pub fn with_dialect(dialect: SqlDialect) -> Self {
        Self { dialect }
    }

    fn build_document(&self, source: &str, path: Option<&Path>) -> QueryDocument {
        let mut document = QueryDocument::sql(source.to_string(), self.dialect.clone());
        if let Some(path) = path {
            if let Some(name) = path.file_name().and_then(|value| value.to_str()) {
                document = document.with_name(name.to_string());
            }
        }
        document
    }
}

impl Default for SqlFrontend {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageFrontend for SqlFrontend {
    fn language(&self) -> &'static str {
        crate::SQL
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["sql"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let mut document = self.build_document(source, path);

        if document.is_empty() {
            diagnostics.add_diagnostic(
                Diagnostic::warning("SQL input is empty".to_string())
                    .with_source_context("sql frontend"),
            );
        }

        if let QueryKind::Sql(sql) = &mut document.kind {
            if let Some(raw) = &sql.raw {
                match crate::sql_ast::parse_sql_ast(raw, sql.dialect.clone()) {
                    Ok(ast) => {
                        sql.ast = ast;
                    }
                    Err(err) => {
                        diagnostics.add_diagnostic(
                            Diagnostic::error(format!("failed to parse SQL AST: {err}"))
                                .with_source_context("sql frontend"),
                        );
                    }
                }
            }
        }

        let serializer: Arc<dyn AstSerializer> = Arc::new(QuerySerializer::new());
        let serialized = serializer.serialize_query(&document).ok();
        let description = match path {
            Some(path) => format!("SQL document {}", path.display()),
            None => "SQL document <stdin>".to_string(),
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
