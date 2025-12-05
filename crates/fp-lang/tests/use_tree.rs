use fp_core::ast::{ItemImportTree, ItemKind};
use fp_lang::parser::FerroPhaseParser;

fn parse_use(src: &str) -> ItemImportTree {
    let parser = FerroPhaseParser::new();
    let mut items = parser.parse_items_ast(src).expect("parse succeeds");
    assert_eq!(items.len(), 1);
    match items.pop().unwrap().kind() {
        ItemKind::Import(import) => import.tree.clone(),
        other => panic!("expected import, got {:?}", other),
    }
}

#[test]
fn parses_nested_group_with_self_super_root_glob_and_rename() {
    let tree = parse_use("use crate::{self, super::util::{foo, bar as baz}, ::core::fmt::Debug, alloc::*};");
    let path = match tree {
        ItemImportTree::Path(p) => p,
        other => panic!("expected path import, got {:?}", other),
    };
    assert_eq!(path.segments.len(), 2, "crate::<group>");
    assert!(matches!(path.segments[0], ItemImportTree::Crate));

    let group = match &path.segments[1] {
        ItemImportTree::Group(g) => g,
        other => panic!("expected group, got {:?}", other),
    };
    assert_eq!(group.items.len(), 4);

    // self
    match &group.items[0] {
        ItemImportTree::Path(p) => assert!(matches!(p.segments.first(), Some(ItemImportTree::SelfMod))),
        other => panic!("expected self path, got {:?}", other),
    }
    // super::util::{foo, bar as baz}
    match &group.items[1] {
        ItemImportTree::Path(p) => {
            assert!(matches!(p.segments.first(), Some(ItemImportTree::SuperMod)));
            let grp = p.segments.iter().find_map(|seg| {
                if let ItemImportTree::Group(g) = seg {
                    Some(g)
                } else {
                    None
                }
            });
            let grp = grp.expect("inner group present");
            assert_eq!(grp.items.len(), 2);
            assert!(matches!(grp.items[0], ItemImportTree::Path(_)));
            match &grp.items[1] {
                ItemImportTree::Path(path) => {
                    assert!(matches!(path.segments.last(), Some(ItemImportTree::Rename(_))));
                }
                other => panic!("expected path with rename, got {:?}", other),
            }
        }
        other => panic!("expected super group path, got {:?}", other),
    }
    // ::core::fmt::Debug
    match &group.items[2] {
        ItemImportTree::Path(p) => assert!(matches!(p.segments.first(), Some(ItemImportTree::Root))),
        other => panic!("expected root path, got {:?}", other),
    }
    // alloc::*
    match &group.items[3] {
        ItemImportTree::Path(p) => assert!(matches!(p.segments.last(), Some(ItemImportTree::Glob))),
        other => panic!("expected glob path, got {:?}", other),
    }
}

#[test]
fn parses_rename_in_linear_path() {
    let tree = parse_use("use foo::bar as baz;");
    let path = match tree {
        ItemImportTree::Path(p) => p,
        other => panic!("expected path, got {:?}", other),
    };
    assert_eq!(path.segments.len(), 2);
    assert!(matches!(path.segments[0], ItemImportTree::Ident(ref id) if id.as_str()=="foo"));
    assert!(matches!(path.segments[1], ItemImportTree::Rename(_)));
}

#[test]
fn parses_glob_path() {
    let tree = parse_use("use super::util::*;");
    let path = match tree {
        ItemImportTree::Path(p) => p,
        other => panic!("expected path, got {:?}", other),
    };
    assert!(matches!(path.segments[0], ItemImportTree::SuperMod));
    assert!(matches!(path.segments[1], ItemImportTree::Ident(_)));
    assert!(matches!(path.segments[2], ItemImportTree::Glob));
}
