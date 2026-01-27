use fp_core::ast::{Abi, Item, ItemImportTree, ItemKind};
use fp_lang::ast::FerroPhaseParser;

fn parse_single_item(src: &str) -> Item {
    let parser = FerroPhaseParser::new();
    let mut items = parser.parse_items_ast(src).expect("parse succeeds");
    assert_eq!(items.len(), 1, "expected single item");
    items.pop().unwrap()
}

#[test]
fn parses_extern_crate_plain() {
    let item = parse_single_item("extern crate alloc;");
    match item.kind() {
        ItemKind::Import(import) => match &import.tree {
            ItemImportTree::Path(path) => {
                assert_eq!(path.segments.len(), 1);
                assert!(
                    matches!(path.segments[0], ItemImportTree::Ident(ref id) if id.as_str()=="alloc")
                );
            }
            other => panic!("expected import path, got {:?}", other),
        },
        other => panic!("expected import item, got {:?}", other),
    }
}

#[test]
fn parses_extern_crate_alias() {
    let item = parse_single_item("extern crate core as c;");
    match item.kind() {
        ItemKind::Import(import) => match &import.tree {
            ItemImportTree::Rename(rename) => {
                assert_eq!(rename.from.as_str(), "core");
                assert_eq!(rename.to.as_str(), "c");
            }
            other => panic!("expected rename import, got {:?}", other),
        },
        other => panic!("expected import item, got {:?}", other),
    }
}

#[test]
fn parses_extern_c_fn_decl() {
    let item = parse_single_item("extern \"C\" fn strlen(s: string) -> i64;");
    match item.kind() {
        ItemKind::DeclFunction(decl) => {
            assert_eq!(decl.name.as_str(), "strlen");
            assert_eq!(decl.sig.abi, Abi::C);
        }
        other => panic!("expected extern fn decl, got {:?}", other),
    }
}

#[test]
fn parses_extern_c_fn_def() {
    let item = parse_single_item("extern \"C\" fn add(a: i32, b: i32) -> i32 { a + b }");
    match item.kind() {
        ItemKind::DefFunction(def) => {
            assert_eq!(def.name.as_str(), "add");
            assert_eq!(def.sig.abi, Abi::C);
        }
        other => panic!("expected extern fn def, got {:?}", other),
    }
}

#[test]
fn parses_extern_c_block() {
    let parser = FerroPhaseParser::new();
    let items = parser
        .parse_items_ast("extern \"C\" { fn puts(s: string); fn puts2(s: string); }")
        .expect("parse succeeds");
    assert_eq!(items.len(), 2);
    for item in items {
        match item.kind() {
            ItemKind::DeclFunction(decl) => {
                assert_eq!(decl.sig.abi, Abi::C);
            }
            other => panic!("expected decl function, got {:?}", other),
        }
    }
}
