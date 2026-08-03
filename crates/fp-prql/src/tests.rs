use super::*;
use fp_core::LanguageFrontend;
use fp_core::diagnostics::DiagnosticLevel;

#[test]
fn parses_pipeline_and_compiles_select() {
    let frontend = frontend::PrqlFrontend::new();
    let pipeline = r#"
from employees
| filter country == "US"
| select {first_name, last_name}
| take 5
"#;
    let result = frontend
        .parse(pipeline, None)
        .expect("prql frontend should parse");

    let snapshot = result.snapshot.as_ref().expect("snapshot");
    assert!(
        snapshot
            .serialized
            .as_ref()
            .map(|s| s.contains("SELECT"))
            .unwrap_or(false)
    );

    assert!(
        result
            .diagnostics
            .get_diagnostics()
            .iter()
            .all(|d| d.level != DiagnosticLevel::Error)
    );
}

#[test]
fn pipeline_detects_target_and_sets_name() {
    use std::path::Path;

    let frontend = frontend::PrqlFrontend::new();
    let pipeline = r#"
target: "postgres"
from sales
| select {total}
"#;
    let result = frontend
        .parse(pipeline, Some(Path::new("reports/query.prql")))
        .expect("prql frontend should parse target pipeline");

    let file_path = result.ast.path;
    assert!(file_path.to_string_lossy().contains("query.prql"));
    assert!(
        result
            .diagnostics
            .get_diagnostics()
            .iter()
            .all(|d| d.level != DiagnosticLevel::Error)
    );
}

#[test]
fn pipeline_compiles_sort_clause() {
    let frontend = frontend::PrqlFrontend::new();
    let pipeline = r#"
from ticks
| filter symbol == "AAPL"
| sort {ts, seq}
| select {value}
"#;
    let result = frontend.parse(pipeline, None).expect("parse");

    let snapshot = result.snapshot.as_ref().expect("snapshot");
    assert!(
        snapshot
            .serialized
            .as_ref()
            .map(|s| s.contains("ORDER BY"))
            .unwrap_or(false)
    );
}
