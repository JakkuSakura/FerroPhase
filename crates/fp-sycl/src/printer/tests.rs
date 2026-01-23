use fp_core::ast::{self, AstSerializer, Item, ItemKind, Node, NodeKind, Value};

use super::SyclSerializer;

#[test]
fn renders_sycl_header() {
    let body = ast::Expr::value(Value::unit()).into();
    let function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
    let item = Item::new(ItemKind::DefFunction(function));
    let node = Node::from(NodeKind::Item(item));

    let serializer = SyclSerializer;
    let rendered = serializer.serialize_node(&node).expect("serialize");

    assert!(rendered.contains("#include <sycl/sycl.hpp>"));
    assert!(rendered.contains("FerroPhase SYCL backend"));
}

#[test]
fn renders_function_stub() {
    let body = ast::Expr::value(Value::unit()).into();
    let function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
    let item = Item::new(ItemKind::DefFunction(function));
    let node = Node::from(NodeKind::Item(item));

    let serializer = SyclSerializer;
    let rendered = serializer.serialize_node(&node).expect("serialize");

    assert!(rendered.contains("int main"));
    assert!(rendered.contains("return 0;"));
    assert!(rendered.contains("TODO: translate function body"));
}
