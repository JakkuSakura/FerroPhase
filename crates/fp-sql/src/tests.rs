use super::*;
use fp_core::diagnostics::DiagnosticLevel;

#[test]
fn parses_basic_select() {
    let frontend = frontend::SqlFrontend::new();
    let result = frontend
        .parse("SELECT 1;", None)
        .expect("sql frontend should parse");

    match result.ast.kind() {
        fp_core::ast::NodeKind::Query(doc) => {
            assert!(!doc.is_empty(), "query document should not be empty");
            if let Some(sql) = doc.kind.as_sql() {
                assert_eq!(sql.statements.len(), 1);
                assert!(sql.statements[0].text.to_uppercase().contains("SELECT"));
                assert_eq!(sql.ast.len(), 1);
            } else {
                panic!("expected sql query kind");
            }
        }
        other => panic!("expected query node, found {other:?}"),
    }

    assert!(
        !result
            .diagnostics
            .get_diagnostics()
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error)
    );
}

#[test]
fn parses_multiple_statements_and_attaches_name() {
    use std::path::Path;

    let frontend = frontend::SqlFrontend::with_dialect(SqlDialect::Sqlite);
    let source = "CREATE TABLE items(id INTEGER);\nINSERT INTO items VALUES (1);\n";
    let result = frontend
        .parse(source, Some(Path::new("queries/schema.sql")))
        .expect("sql frontend should parse multiple statements");

    let fp_core::ast::NodeKind::Query(doc) = result.ast.kind() else {
        panic!("expected query node");
    };
    let sql = doc.kind.as_sql().expect("expected sql query");
    assert_eq!(doc.name.as_deref(), Some("schema.sql"));
    assert_eq!(sql.dialect, SqlDialect::Sqlite);
    assert_eq!(sql.statements.len(), 2, "expected two SQL statements");
    assert_eq!(sql.statements[0].text, "CREATE TABLE items(id INTEGER)");
    assert_eq!(sql.statements[1].text, "INSERT INTO items VALUES (1)");
    assert_eq!(sql.ast.len(), 2, "expected two SQL AST statements");
}
