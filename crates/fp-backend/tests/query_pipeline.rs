use fp_backend::transforms::{HirGenerator, MirLowering};
use fp_core::ast::NodeKind;
use fp_core::frontend::LanguageFrontend;
use fp_core::hir;
use fp_core::mir;
use fp_core::query::{QueryDocument, QueryIrStmt, QueryKind, QueryStatement, SqlDialect};
use fp_lang::FerroFrontend;

#[test]
fn sql_query_document_lowers_to_hir_and_mir_query_items() {
    let query = QueryDocument::sql("SELECT 42", SqlDialect::Generic).with_name("query.sql");
    let mut hir_generator = HirGenerator::new();
    let hir_program = hir_generator
        .transform_query_document(&query)
        .expect("hir program");
    assert_eq!(hir_program.items.len(), 1);
    let hir_query = match &hir_program.items[0].kind {
        hir::ItemKind::Query(query) => query,
        other => panic!("expected HIR query item, got {other:?}"),
    };
    assert_eq!(hir_query.document.name.as_deref(), Some("query.sql"));
    assert_eq!(hir_query.statements.len(), 1);
    assert!(matches!(
        hir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));

    let mut mir_lowering = MirLowering::new();
    let mir_program = mir_lowering.transform(hir_program).expect("mir program");
    let (diagnostics, had_errors) = mir_lowering.take_diagnostics();
    assert!(!had_errors, "{diagnostics:?}");
    assert_eq!(mir_program.items.len(), 1);
    let mir_query = match &mir_program.items[0].kind {
        mir::ItemKind::Query(query) => query,
        other => panic!("expected MIR query item, got {other:?}"),
    };
    assert_eq!(mir_query.document.name.as_deref(), Some("query.sql"));
    assert_eq!(mir_query.statements.len(), 1);
    assert!(matches!(
        mir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));
}

#[test]
fn prql_query_document_lowers_to_hir_and_mir_query_items() {
    let mut query = QueryDocument::prql("from ticks | filter symbol == \"AAPL\" | select {value}");
    if let QueryKind::Prql(prql) = &mut query.kind {
        prql.target = Some(SqlDialect::Generic);
        prql.compiled = vec![QueryStatement::new(
            "SELECT value FROM ticks WHERE symbol = \"AAPL\"",
        )];
    }
    let mut hir_generator = HirGenerator::new();
    let hir_program = hir_generator
        .transform_query_document(&query)
        .expect("hir program");
    let hir_query = match &hir_program.items[0].kind {
        hir::ItemKind::Query(query) => query,
        other => panic!("expected HIR query item, got {other:?}"),
    };
    assert_eq!(hir_query.statements.len(), 1);
    assert!(matches!(
        hir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));

    let mut mir_lowering = MirLowering::new();
    let mir_program = mir_lowering.transform(hir_program).expect("mir program");
    let (diagnostics, had_errors) = mir_lowering.take_diagnostics();
    assert!(!had_errors, "{diagnostics:?}");
    let mir_query = match &mir_program.items[0].kind {
        mir::ItemKind::Query(query) => query,
        other => panic!("expected MIR query item, got {other:?}"),
    };
    assert_eq!(mir_query.statements.len(), 1);
    assert!(matches!(
        mir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));
}

#[test]
fn fp_query_feature_lowers_in_ast_to_hir_pass() {
    let frontend = FerroFrontend::new();
    let result = frontend
        .parse(
            "from(ticks).where(symbol == \"AAPL\").select(struct { value })",
            None,
        )
        .expect("parse");
    let NodeKind::Expr(expr) = result.ast.kind() else {
        panic!("expected host AST expr before feature pass");
    };

    let mut hir_generator = HirGenerator::new();
    let hir_program = hir_generator.transform_expr(expr).expect("hir program");
    let hir_query = match &hir_program.items[0].kind {
        hir::ItemKind::Query(query) => query,
        other => panic!("expected HIR query item, got {other:?}"),
    };
    assert!(matches!(
        hir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));

    let mut mir_lowering = MirLowering::new();
    let mir_program = mir_lowering.transform(hir_program).expect("mir program");
    let (diagnostics, had_errors) = mir_lowering.take_diagnostics();
    assert!(!had_errors, "{diagnostics:?}");
    let mir_query = match &mir_program.items[0].kind {
        mir::ItemKind::Query(query) => query,
        other => panic!("expected MIR query item, got {other:?}"),
    };
    assert!(matches!(
        mir_query
            .document
            .semantic
            .as_ref()
            .and_then(|doc| doc.statements.first()),
        Some(QueryIrStmt::Query(_))
    ));
}
