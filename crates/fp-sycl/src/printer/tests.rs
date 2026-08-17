use fp_core::ast::{self, Item, ItemKind};

use super::SyclSerializer;

fn file_with_item(item: Item) -> ast::File {
    ast::File {
        path: "test.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![item],
    }
}

#[test]
fn renders_sycl_header() {
    let body = ast::ExprBlock::new();
    let function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
    let item = Item::new(ItemKind::DefFunction(function));
    let file = file_with_item(item);

    let serializer = SyclSerializer;
    let rendered = serializer.serialize_file(&file).expect("serialize");

    assert!(rendered.contains("#include <sycl/sycl.hpp>"));
    assert!(rendered.contains("FerroPhase SYCL backend"));
}

#[test]
fn renders_function_stub() {
    let body = ast::ExprBlock::new();
    let function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
    let item = Item::new(ItemKind::DefFunction(function));
    let file = file_with_item(item);

    let serializer = SyclSerializer;
    let rendered = serializer.serialize_file(&file).expect("serialize");

    assert!(rendered.contains("int main"));
    assert!(rendered.contains("return 0;"));
    assert!(rendered.contains("TODO: translate function body"));
}
