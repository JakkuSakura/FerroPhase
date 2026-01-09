use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::{
    register_threadlocal_serializer, AstSerializer, ExprKind, File, ItemKind, Node, NodeKind, Value,
};
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_rust::{printer::RustPrinter, shll_parse_items};

#[test]
fn const_eval_replaces_consts_and_records_results() -> Result<()> {
    let serializer: Arc<dyn AstSerializer> = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(serializer.clone());

    let items = shll_parse_items! {
        const WIDTH: i64 = 6;
        const HEIGHT: i64 = 7;
        const AREA: i64 = WIDTH * HEIGHT;

        mod shapes {
            pub const EDGE: i64 = 3;
            pub const PERIMETER: i64 = EDGE * 4;
        }
    };

    let mut ast = Node::file(File {
        path: PathBuf::from("const_eval.fp"),
        items,
    });

    let ctx = SharedScopedContext::new();
    let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
    let outcome = orchestrator.evaluate(&mut ast, &ctx)?;

    assert!(
        outcome.mutations_applied,
        "const eval should rewrite literal expressions"
    );
    assert!(
        !outcome.has_errors,
        "const eval should complete without diagnostics"
    );
    assert!(outcome.diagnostics.is_empty(), "no diagnostics expected");

    assert_eq!(
        Value::int(42),
        *outcome
            .evaluated_constants
            .get("AREA")
            .expect("AREA constant should be evaluated"),
    );

    assert_eq!(
        Value::int(3),
        *outcome
            .evaluated_constants
            .get("shapes::EDGE")
            .expect("qualified constant EDGE should be tracked"),
    );
    assert_eq!(
        Value::int(12),
        *outcome
            .evaluated_constants
            .get("shapes::PERIMETER")
            .expect("qualified constant PERIMETER should be tracked"),
    );

    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("expected file node after const eval"),
    };

    let area_const = match file.items[2].kind() {
        ItemKind::DefConst(def) => def,
        other => panic!("expected third item to remain a const, found {other:?}"),
    };
    match area_const.value.kind() {
        ExprKind::Value(value) => {
            let int_value = match value.as_ref() {
                Value::Int(int_value) => int_value.value,
                other => panic!("expected int value after const eval, found {other:?}"),
            };
            assert_eq!(42, int_value);
        }
        other => panic!("expected AREA value to be literal after const eval, found {other:?}"),
    }

    let shapes_module = file
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::Module(module) if module.name.as_str() == "shapes" => Some(module),
            _ => None,
        })
        .expect("shapes module should remain in AST");

    let perimeter_const = shapes_module
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::DefConst(def) if def.name.as_str() == "PERIMETER" => Some(def),
            _ => None,
        })
        .expect("PERIMETER const should still exist inside module");

    match perimeter_const.value.kind() {
        ExprKind::Value(value) => {
            let int_value = match value.as_ref() {
                Value::Int(int_value) => int_value.value,
                other => panic!("expected int value after const eval, found {other:?}"),
            };
            assert_eq!(12, int_value);
        }
        other => panic!("expected PERIMETER to be a literal after const eval, found {other:?}"),
    }

    Ok(())
}
