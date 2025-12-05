use fp_core::ast::ItemKind;
use fp_lang::parser::FerroPhaseParser;

fn parse_items(src: &str) -> Vec<fp_core::ast::Item> {
    let parser = FerroPhaseParser::new();
    parser.parse_items_ast(src).expect("parse succeeds")
}

#[test]
fn parses_trait_with_bounds_and_members() {
    let items = parse_items("trait Foo: Bar + Baz { fn bar(&self); const N: i32; type Assoc; }");
    assert_eq!(items.len(), 1);
    match items[0].kind() {
        ItemKind::DefTrait(tr) => {
            // Bounds should be preserved (Bar + Baz)
            assert!(
                !tr.bounds.bounds.is_empty(),
                "expected trait bounds to be retained"
            );
            // Trait items: method + const + associated type
            assert_eq!(tr.items.len(), 3);
        }
        other => panic!("expected trait item, got {:?}", other),
    }
}

#[test]
fn parses_static_item() {
    let items = parse_items("static COUNTER: i64 = 0;");
    assert_eq!(items.len(), 1);
    match items[0].kind() {
        ItemKind::DefStatic(st) => {
            assert_eq!(st.name.as_str(), "COUNTER");
        }
        other => panic!("expected static item, got {:?}", other),
    }
}
