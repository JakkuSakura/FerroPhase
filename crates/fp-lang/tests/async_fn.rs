use fp_core::ast::ItemKind;
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

#[test]
fn async_fn_body_wrapped() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("async fn foo() -> i64 { 1 }", None)
        .expect("parse");
    let item = res.ast.items.first().cloned().expect("file item");
    match item.kind() {
        ItemKind::DefFunction(def) => {
            assert!(def.is_async);
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
    let item = res.ast.items.first().cloned().expect("file item");
    match item.kind() {
        ItemKind::DefTrait(def) => {
            let first = def.items.first().expect("trait item");
            match first.kind() {
                ItemKind::DefFunction(func) => {
                    assert!(func.is_async);
                }
                other => panic!("expected function member, got {:?}", other),
            }
        }
        other => panic!("expected trait item, got {:?}", other),
    }
}
