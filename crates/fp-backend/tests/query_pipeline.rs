use fp_backend::transforms::{HirGenerator, LirGenerator, MirLowering};
use fp_core::ast::NodeKind;
use fp_core::frontend::LanguageFrontend;
use fp_core::hir;
use fp_core::mir;
use fp_core::query::{
    statement_to_query_ir, QueryDocument, QueryIrDocument, QueryIrStmt, SqlDialect,
};
use fp_lang::FerroFrontend;
use fp_sql::sql_ast::parse_sql_ast;

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
    assert_eq!(hir_query.ir.name.as_deref(), Some("query.sql"));
    assert_eq!(
        hir_query
            .ir
            .to_statements()
            .expect("HIR cached statements")
            .len(),
        1
    );
    assert!(matches!(
        hir_query.ir.statements.first(),
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
    assert_eq!(mir_query.ir.name.as_deref(), Some("query.sql"));
    assert_eq!(
        mir_query
            .ir
            .to_statements()
            .expect("MIR cached statements")
            .len(),
        1
    );
    assert!(matches!(
        mir_query.ir.statements.first(),
        Some(QueryIrStmt::Query(_))
    ));

    let mut lir_generator = LirGenerator::new();
    let lir_program = lir_generator.transform(mir_program).expect("lir program");
    assert_eq!(lir_program.queries.len(), 1);
    let lir_query = &lir_program.queries[0];
    assert_eq!(lir_query.ir.name.as_deref(), Some("query.sql"));
    assert!(matches!(
        lir_query.ir.statements.first(),
        Some(QueryIrStmt::Query(_))
    ));
}

#[test]
fn prql_query_document_lowers_to_hir_and_mir_query_items() {
    let mut query = QueryDocument::prql("from ticks | filter symbol == \"AAPL\" | select {value}");
    query.semantic = Some(QueryIrDocument {
        name: query.name.clone(),
        statements: parse_sql_ast(
            "SELECT value FROM ticks WHERE symbol = 'AAPL'",
            SqlDialect::Generic,
        )
        .expect("sql ast")
        .iter()
        .filter_map(statement_to_query_ir)
        .collect(),
    });
    let mut hir_generator = HirGenerator::new();
    let hir_program = hir_generator
        .transform_query_document(&query)
        .expect("hir program");
    let hir_query = match &hir_program.items[0].kind {
        hir::ItemKind::Query(query) => query,
        other => panic!("expected HIR query item, got {other:?}"),
    };
    assert_eq!(
        hir_query
            .ir
            .to_statements()
            .expect("HIR cached statements")
            .len(),
        1
    );
    assert!(matches!(
        hir_query.ir.statements.first(),
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
    assert_eq!(
        mir_query
            .ir
            .to_statements()
            .expect("MIR cached statements")
            .len(),
        1
    );
    assert!(matches!(
        mir_query.ir.statements.first(),
        Some(QueryIrStmt::Query(_))
    ));

    let mut lir_generator = LirGenerator::new();
    let lir_program = lir_generator.transform(mir_program).expect("lir program");
    assert_eq!(lir_program.queries.len(), 1);
    assert!(matches!(
        lir_program.queries[0].ir.statements.first(),
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
        hir_query.ir.statements.first(),
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
        mir_query.ir.statements.first(),
        Some(QueryIrStmt::Query(_))
    ));

    let mut lir_generator = LirGenerator::new();
    let lir_program = lir_generator.transform(mir_program).expect("lir program");
    assert_eq!(lir_program.queries.len(), 1);
    assert!(matches!(
        lir_program.queries[0].ir.statements.first(),
        Some(QueryIrStmt::Query(_))
    ));
}
