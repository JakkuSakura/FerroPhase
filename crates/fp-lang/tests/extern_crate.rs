use fp_core::ast::{Item, ItemImportTree, ItemKind};
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
