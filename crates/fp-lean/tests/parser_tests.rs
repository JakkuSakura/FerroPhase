use fp_core::ast::{ItemKind, Ty};
use fp_core::frontend::LanguageFrontend;
use fp_lean::LeanFrontend;

#[test]
fn parses_basic_fixture_end_to_end() {
    let source = include_str!("fixtures/basic.lean");
    let frontend = LeanFrontend::new();
    let result = frontend.parse(source, None).expect("parse basic.lean");
    assert_eq!(result.ast.items.len(), 5);

    let ItemKind::DefFunction(compute) = result.ast.items[2].kind() else {
        panic!("expected `compute` to be a DefFunction");
    };
    // two `let`s + one tail expression
    assert_eq!(compute.body.stmts.len(), 3);

    let ItemKind::DefFunction(safe_index) = result.ast.items[4].kind() else {
        panic!("expected `safe_index` to be a DefFunction");
    };
    assert!(matches!(safe_index.sig.params[0].ty, Ty::Refinement(_)));
}
