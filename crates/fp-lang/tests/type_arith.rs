use fp_core::ast::*;
use fp_lang::parser::FerroPhaseParser;

fn parse_single_type_alias(src: &str) -> ItemDefType {
    let parser = FerroPhaseParser::new();
    let items = parser.parse_items_ast(src).expect("parse items");
    assert_eq!(items.len(), 1, "expected a single item");
    match items[0].kind() {
        ItemKind::DefType(def) => def.clone(),
        other => panic!("expected type def, found {:?}", other),
    }
}

#[test]
fn parses_structural_type_literal_in_alias() {
    let def = parse_single_type_alias("type A = struct { x: i64, y: i64 };");
    match def.value {
        Ty::Structural(ts) => {
            assert_eq!(ts.fields.len(), 2);
            assert_eq!(ts.fields[0].name.as_str(), "x");
            assert_eq!(ts.fields[1].name.as_str(), "y");
        }
        other => panic!("expected TypeStructural, found {:?}", other),
    }
}

#[test]
fn parses_type_binary_plus_alias() {
    let def = parse_single_type_alias("type C = A + B;");
    match def.value {
        Ty::TypeBinaryOp(_) => {
            // We don't inspect the operator here; we just ensure the
            // parser produced a symbolic type-level operator node.
        }
        other => panic!("expected Ty::TypeBinaryOp for type binary op, found {:?}", other),
    }
}
