//! FerroPhase frontend for PRQL pipelines.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Node};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result as CoreResult;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::query::{
    split_sql_statements, QueryDocument, QueryKind, QuerySerializer, QueryStatement,
};

pub use fp_core::query::SqlDialect;

/// Canonical language identifier for PRQL sources.
pub const PRQL: &str = "prql";

/// Language frontend that captures PRQL pipelines and performs a best-effort
/// lowering to SQL for tooling that expects compiled statements.
#[derive(Debug, Default, Clone)]
pub struct PrqlFrontend;

impl PrqlFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_document(&self, source: &str, path: Option<&Path>) -> QueryDocument {
        let mut document = QueryDocument::prql(source.to_string());

        if let Some(path) = path {
            if let Some(name) = path.file_name().and_then(|value| value.to_str()) {
                document = document.with_name(name.to_string());
            }
        }

        if let QueryKind::Prql(prql) = &mut document.kind {
            if let Some(target) = detect_target_dialect(source) {
                prql.target = Some(target.clone());
            }

            if let Some(compiled_sql) = naive_prql_to_sql(source) {
                prql.compiled = split_sql_statements(&compiled_sql)
                    .into_iter()
                    .map(QueryStatement::new)
                    .collect();
                if prql.target.is_none() {
                    prql.target = Some(SqlDialect::Generic);
                }
            }
        }

        document
    }
}

impl LanguageFrontend for PrqlFrontend {
    fn language(&self) -> &'static str {
        PRQL
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["prql"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let document = self.build_document(source, path);

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
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}

fn detect_target_dialect(source: &str) -> Option<SqlDialect> {
    for line in source.lines() {
        let trimmed = line.trim();
        if let Some(value) = trimmed.strip_prefix("target:") {
            return map_target(value.trim());
        } else if let Some(value) = trimmed.strip_prefix("sql_dialect:") {
            return map_target(value.trim());
        }
    }
    None
}

fn map_target(value: &str) -> Option<SqlDialect> {
    let lowered = value.to_ascii_lowercase();
    if lowered.contains("postgres") {
        Some(SqlDialect::Postgres)
    } else if lowered.contains("mysql") {
        Some(SqlDialect::Mysql)
    } else if lowered.contains("sqlite") {
        Some(SqlDialect::Sqlite)
    } else if lowered.contains("duckdb") {
        Some(SqlDialect::DuckDb)
    } else if lowered.contains("bigquery") {
        Some(SqlDialect::BigQuery)
    } else if lowered.contains("mssql") {
        Some(SqlDialect::Mssql)
    } else {
        None
    }
}

fn naive_prql_to_sql(source: &str) -> Option<String> {
    let mut from_table: Option<String> = None;
    let mut selects: Vec<String> = Vec::new();
    let mut filters: Vec<String> = Vec::new();
    let mut limits: Option<String> = None;

    for chunk in source.split('|') {
        let trimmed = chunk.trim();
        if trimmed.is_empty() {
            continue;
        }
        for line in trimmed.lines() {
            let entry = line.trim();
            if entry.is_empty() {
                continue;
            }
            if let Some(rest) = entry.strip_prefix("from ") {
                from_table = Some(rest.trim().to_string());
            } else if let Some(rest) = entry.strip_prefix("select") {
                if let Some(fields) = extract_group(rest) {
                    selects.extend(
                        fields
                            .into_iter()
                            .map(|field| field.trim_matches('.').to_string()),
                    );
                }
            } else if let Some(rest) = entry.strip_prefix("filter ") {
                let normalized = rest.trim().replace("==", "=");
                filters.push(normalized);
            } else if let Some(rest) = entry.strip_prefix("take ") {
                limits = Some(rest.trim().to_string());
            }
        }
    }

    let table = from_table?;
    let select_clause = if selects.is_empty() {
        "*".to_string()
    } else {
        selects.join(", ")
    };

    let mut sql = format!("SELECT {} FROM {}", select_clause, table);

    if !filters.is_empty() {
        sql.push_str(" WHERE ");
        sql.push_str(&filters.join(" AND "));
    }

    if let Some(limit) = limits {
        sql.push_str(" LIMIT ");
        sql.push_str(&limit);
    }

    Some(sql)
}

fn extract_group(rest: &str) -> Option<Vec<String>> {
    let trimmed = rest.trim();
    let start = trimmed.find(['{', '['])?;
    let end = trimmed.rfind(['}', ']'])?;
    let group = &trimmed[start + 1..end];
    let mut fields = Vec::new();
    for field in group.split(',') {
        let entry = field.trim();
        if entry.is_empty() {
            continue;
        }
        if let Some((_, value)) = entry.split_once(':') {
            fields.push(value.trim().to_string());
        } else {
            fields.push(entry.to_string());
        }
    }
    if fields.is_empty() {
        None
    } else {
        Some(fields)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::diagnostics::DiagnosticLevel;

    #[test]
    fn parses_pipeline_and_compiles_select() {
        let frontend = PrqlFrontend::new();
        let pipeline = r#"
from employees
| filter country == "US"
| select {first_name, last_name}
| take 5
"#;
        let result = frontend
            .parse(pipeline, None)
            .expect("prql frontend should parse");

        match result.ast.kind() {
            fp_core::ast::NodeKind::Query(doc) => {
                assert!(!doc.is_empty());
                let QueryKind::Prql(prql) = &doc.kind else {
                    panic!("expected prql variant");
                };
                assert_eq!(prql.compiled.len(), 1);
                let sql = &prql.compiled[0].text;
                assert!(sql.contains("SELECT"));
                assert!(sql.contains("FROM employees"));
                assert!(sql.contains("WHERE country = \"US\""));
                assert!(sql.contains("LIMIT 5"));
            }
            other => panic!("expected query node, found {other:?}"),
        }

        assert!(result
            .diagnostics
            .get_diagnostics()
            .iter()
            .all(|d| d.level != DiagnosticLevel::Error));
    }

    #[test]
    fn pipeline_detects_target_and_sets_name() {
        use std::path::Path;

        let frontend = PrqlFrontend::new();
        let pipeline = r#"
target: "postgres"
from sales
| select {total}
"#;
        let result = frontend
            .parse(pipeline, Some(Path::new("reports/query.prql")))
            .expect("prql frontend should parse target pipeline");

        let fp_core::ast::NodeKind::Query(doc) = result.ast.kind() else {
            panic!("expected query node");
        };
        assert_eq!(doc.name.as_deref(), Some("query.prql"));
        let QueryKind::Prql(prql) = &doc.kind else {
            panic!("expected prql variant");
        };
        assert_eq!(prql.target, Some(SqlDialect::Postgres));
        assert!(
            prql.compiled
                .first()
                .map(|stmt| stmt.text.contains("FROM sales"))
                .unwrap_or(false),
            "compiled statements should reference source table"
        );
    }
}
