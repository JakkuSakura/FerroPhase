use fp_core::ast::{ItemImpl, NodeKind};
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

fn parse_impl(src: &str) -> ItemImpl {
    let fe = FerroFrontend::new();
    let res = fe.parse(src, None).expect("parse");
    let item = match res.ast.kind() {
        NodeKind::Item(it) => it.clone(),
        NodeKind::File(file) => file.items.first().cloned().expect("file item"),
        other => panic!("expected Item/File, got {:?}", other),
    };
    match item.kind() {
        fp_core::ast::ItemKind::Impl(im) => im.clone(),
        other => panic!("expected impl item, got {:?}", other),
    }
}

#[test]
fn impl_generics_with_bounds_preserved() {
    let impl_block = parse_impl("impl<T: Foo + Bar> Baz<T> {}");
    assert_eq!(impl_block.generics_params.len(), 1);
    let gp = &impl_block.generics_params[0];
    assert_eq!(gp.name.as_str(), "T");
    assert!(
        !gp.bounds.bounds.is_empty(),
        "expected bounds to be preserved on impl"
    );
}

#[test]
fn impl_generics_multiple_params() {
    let impl_block = parse_impl("impl<T, U> Pair<T, U> {}");
    assert_eq!(impl_block.generics_params.len(), 2);
    assert_eq!(impl_block.generics_params[0].name.as_str(), "T");
    assert_eq!(impl_block.generics_params[1].name.as_str(), "U");
}

