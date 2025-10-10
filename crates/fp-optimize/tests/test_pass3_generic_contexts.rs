use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::{
    register_threadlocal_serializer, AstSerializer, File, ItemKind, Node, NodeKind,
};
use fp_core::Result;
use fp_optimize::passes::remove_generics::remove_generic_templates;
use fp_rust::{printer::RustPrinter, shll_parse_items};

fn build_file(items: Vec<fp_core::ast::Item>) -> Node {
    Node::file(File {
        path: PathBuf::from("generic_cleanup.fp"),
        items,
    })
}

fn init_serializer() -> Arc<dyn AstSerializer> {
    let serializer: Arc<dyn AstSerializer> = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(serializer.clone());
    serializer
}

#[test]
fn remove_generic_templates_drops_top_level_generics() -> Result<()> {
    let _serializer = init_serializer();
    let items = shll_parse_items! {
        fn identity<T>(value: T) -> T { value }
        fn concrete(value: i32) -> i32 { value }
    };

    let mut ast = build_file(items);
    remove_generic_templates(&mut ast)?;

    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("remove_generics should leave a file node"),
    };

    assert_eq!(
        1,
        file.items.len(),
        "generic template should be removed from top-level items",
    );

    let remaining = match file.items[0].kind() {
        ItemKind::DefFunction(func) => func,
        other => panic!("expected remaining item to be a function, found {other:?}"),
    };
    assert_eq!("concrete", remaining.name.as_str());

    Ok(())
}

#[test]
fn remove_generic_templates_cleans_nested_impls_and_modules() -> Result<()> {
    let _serializer = init_serializer();
    let items = shll_parse_items! {
        mod math {
            pub fn helper<T>(value: T) -> T { value }
            pub fn add(lhs: i64, rhs: i64) -> i64 { lhs + rhs }
        }

        struct Wrapper;

        impl Wrapper {
            fn new<T>(value: T) -> Wrapper { Wrapper }
            fn ready(value: i64) -> Wrapper { Wrapper }
        }
    };

    let mut ast = build_file(items);
    remove_generic_templates(&mut ast)?;

    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("remove_generics should leave a file node"),
    };

    let math_module = file
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::Module(module) if module.name.as_str() == "math" => Some(module),
            _ => None,
        })
        .expect("math module should be preserved");

    assert_eq!(
        1,
        math_module.items.len(),
        "generic helper function should be removed from module",
    );
    let math_fn = match math_module.items[0].kind() {
        ItemKind::DefFunction(func) => func,
        other => panic!("expected remaining module item to be a function, found {other:?}"),
    };
    assert_eq!("add", math_fn.name.as_str());

    let wrapper_impl = file
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::Impl(impl_block) => Some(impl_block),
            _ => None,
        })
        .expect("Wrapper impl should remain available");

    assert_eq!(
        1,
        wrapper_impl.items.len(),
        "generic method should be removed from impl block",
    );
    let method = match wrapper_impl.items[0].kind() {
        ItemKind::DefFunction(func) => func,
        other => panic!("expected remaining impl item to be a function, found {other:?}"),
    };
    assert_eq!("ready", method.name.as_str());

    Ok(())
}
