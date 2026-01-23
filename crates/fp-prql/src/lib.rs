//! FerroPhase frontend for PRQL pipelines.

pub mod compile;
pub mod dialect;
pub mod frontend;

pub use fp_core::query::SqlDialect;
pub use compile::{PrqlCompileResult, compile_prql};
pub use frontend::PrqlFrontend;

/// Canonical language identifier for PRQL sources.
pub const PRQL: &str = "prql";

impl PrqlFrontend {
    pub fn compile(
        &self,
        source: &str,
        target_override: Option<SqlDialect>,
    ) -> fp_core::error::Result<PrqlCompileResult> {
        compile_prql(source, target_override)
    }
}

#[cfg(test)]
mod tests;

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
