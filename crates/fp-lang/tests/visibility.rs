use fp_core::ast::{Item, ItemImportTree, ItemKind, Visibility};
use fp_lang::ast::FerroPhaseParser;

fn parse_single_item(src: &str) -> Item {
    let parser = FerroPhaseParser::new();
    let mut items = parser.parse_items_ast(src).expect("parse succeeds");
    assert_eq!(items.len(), 1, "expected single item");
    items.pop().unwrap()
}

#[test]
fn parses_pub_crate_fn() {
    let item = parse_single_item("pub(crate) fn f() {}");
    match item.kind() {
        ItemKind::DefFunction(def) => assert!(matches!(def.visibility, Visibility::Crate)),
        other => panic!("expected function, got {:?}", other),
    }
}

#[test]
fn parses_pub_super_struct() {
    let item = parse_single_item("pub(super) struct S {}");
    match item.kind() {
        ItemKind::DefStruct(def) => match &def.visibility {
            Visibility::Restricted(path) => {
                assert_eq!(path.segments.len(), 1);
                assert!(matches!(path.segments[0], ItemImportTree::SuperMod));
            }
            v => panic!("expected Restricted(super), got {:?}", v),
        },
        other => panic!("expected struct, got {:?}", other),
    }
}

#[test]
fn parses_pub_in_path_const() {
    let item = parse_single_item("pub(in crate::inner) const X: i32 = 1;");
    match item.kind() {
        ItemKind::DefConst(def) => match &def.visibility {
            Visibility::Restricted(path) => {
                assert_eq!(path.segments.len(), 2);
                assert!(matches!(path.segments[0], ItemImportTree::Crate));
                assert!(matches!(path.segments[1], ItemImportTree::Ident(_)));
            }
            v => panic!("expected Restricted(crate::inner), got {:?}", v),
        },
        other => panic!("expected const, got {:?}", other),
    }
}

#[test]
fn parses_pub_self_type_alias() {
    let item = parse_single_item("pub(self) type T = i32;");
    match item.kind() {
        ItemKind::DefType(def) => match &def.visibility {
            Visibility::Restricted(path) => {
                assert_eq!(path.segments.len(), 1);
                assert!(matches!(path.segments[0], ItemImportTree::SelfMod));
            }
            v => panic!("expected Restricted(self), got {:?}", v),
        },
        other => panic!("expected type alias, got {:?}", other),
    }
}

#[test]
fn incomplete_pub_in_is_error() {
    let parser = FerroPhaseParser::new();
    let res = parser.parse_items_ast("pub(in ) fn f() {}");
    assert!(res.is_err(), "incomplete pub(in …) should error");
}

#[test]
fn parses_struct_optional_field_marker() {
    let item = parse_single_item("struct User { email?: string }");
    match item.kind() {
        ItemKind::DefStruct(def) => {
            assert_eq!(def.value.fields.len(), 1);
            assert_eq!(def.value.fields[0].name.as_str(), "email");
            match &def.value.fields[0].value {
                fp_core::ast::Ty::TypeBinaryOp(bin) => {
                    assert!(matches!(bin.kind, fp_core::ast::TypeBinaryOpKind::Union));
                }
                other => panic!("expected optional union type, got {:?}", other),
            }
        }
        other => panic!("expected struct, got {:?}", other),
    }
}

#[test]
fn parses_struct_field_nested_generics_with_right_shift_token() {
    let item = parse_single_item("struct Node<T> { next: Option<Box<Node<T>>> }");
    match item.kind() {
        ItemKind::DefStruct(def) => {
            assert_eq!(def.value.fields.len(), 1);
            assert_eq!(def.value.fields[0].name.as_str(), "next");
        }
        other => panic!("expected struct, got {:?}", other),
    }
}
