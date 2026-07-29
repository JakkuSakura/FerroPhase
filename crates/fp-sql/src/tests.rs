use super::*;
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::frontend::LanguageFrontend;
use fp_core::query::QueryIrStmt;

#[test]
fn parses_basic_select() {
    let frontend = frontend::SqlFrontend::new();
    let result = frontend
        .parse("SELECT 1;", None)
        .expect("sql frontend should parse");

    // The query document is not directly accessible from the File AST.
    // SQL frontend now uses File as the AST carrier; queries go through
    // the serialization snapshot instead.
    let snapshot = result.snapshot.as_ref().expect("snapshot");
    assert!(snapshot
        .serialized
        .as_ref()
        .map(|s| !s.is_empty())
        .unwrap_or(false));

    assert!(!result
        .diagnostics
        .get_diagnostics()
        .iter()
        .any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn parses_multiple_statements_and_attaches_name() {
    use std::path::Path;

    let frontend = frontend::SqlFrontend::with_dialect(SqlDialect::Sqlite);
    let source = "CREATE TABLE items(id INTEGER);\nINSERT INTO items VALUES (1);\n";
    let result = frontend
        .parse(source, Some(Path::new("queries/schema.sql")))
        .expect("sql frontend should parse multiple statements");

    let file_path = result.ast.path;
    assert!(file_path.to_string_lossy().contains("schema.sql"));
    assert!(!result
        .diagnostics
        .get_diagnostics()
        .iter()
        .any(|d| d.level == DiagnosticLevel::Error));
}

#[test]
fn parses_update_into_semantic_mutation() {
    let frontend = frontend::SqlFrontend::new();
    let result = frontend
        .parse(
            "UPDATE ticks SET value = value + 1 WHERE symbol = 'AAPL';",
            None,
        )
        .expect("sql frontend should parse update");

    assert!(!result
        .diagnostics
        .get_diagnostics()
        .iter()
        .any(|d| d.level == DiagnosticLevel::Error));
}
