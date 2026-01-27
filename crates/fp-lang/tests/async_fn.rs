use fp_core::ast::{ExprKind, ItemKind, NodeKind};
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

#[test]
fn async_fn_body_wrapped() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("async fn foo() -> i64 { 1 }", None)
        .expect("parse");
    let item = match res.ast.kind() {
        NodeKind::Item(it) => it.clone(),
        NodeKind::File(file) => file.items.first().cloned().expect("file item"),
        other => panic!("expected Item/File, got {:?}", other),
    };
    match item.kind() {
        ItemKind::DefFunction(def) => {
            assert!(matches!(def.body.kind(), ExprKind::Async(_)));
        }
        other => panic!("expected function item, got {:?}", other),
    }
}

#[test]
fn async_trait_method_body_wrapped() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("trait T { async fn f() { 1 } }", None)
        .expect("parse");
    let item = match res.ast.kind() {
        NodeKind::Item(it) => it.clone(),
        NodeKind::File(file) => file.items.first().cloned().expect("file item"),
        other => panic!("expected Item/File, got {:?}", other),
    };
    match item.kind() {
        ItemKind::DefTrait(def) => {
            let first = def.items.first().expect("trait item");
            match first.kind() {
                ItemKind::DefFunction(func) => {
                    assert!(matches!(func.body.kind(), ExprKind::Async(_)));
                }
                other => panic!("expected function member, got {:?}", other),
            }
        }
        other => panic!("expected trait item, got {:?}", other),
    }
}
