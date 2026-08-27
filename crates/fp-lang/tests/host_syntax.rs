use fp_core::ast::{AttrMeta, ItemKind};
use fp_lang::ast::FerroPhaseParser;

#[test]
fn parses_host_derived_struct_and_host_static_forms() {
    let source = r#"
        #[host]
        struct HostHandle { raw: usize }

        #[host]
        static HOST_HANDLE: HostHandle = HostHandle { raw: 0 };

        #[host]
        static mut HOST_STATE: HostHandle = HostHandle { raw: 1 };
    "#;
    let items = FerroPhaseParser::new()
        .parse_items_ast(source)
        .expect("host declarations should parse");

    assert_eq!(items.len(), 3);
    for item in &items {
        let attrs = match item.kind() {
            ItemKind::DefStruct(def) => &def.attrs,
            ItemKind::DefStatic(def) => &def.attrs,
            other => panic!("unexpected item: {:?}", other),
        };
        assert!(matches!(attrs.first().map(|attr| &attr.meta), Some(AttrMeta::Path(path)) if path.last().as_str() == "host"));
    }
}
