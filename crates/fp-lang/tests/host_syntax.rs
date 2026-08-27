use fp_core::ast::ItemKind;
use fp_lang::ast::FerroPhaseParser;

#[test]
fn parses_host_derived_struct_and_host_static_forms() {
    let source = r#"
        #[host]
        struct HostHandle { raw: usize }

        extern "host" static HOST_HANDLE: HostHandle;

        extern "host" static mut HOST_STATE: HostHandle;
    "#;
    let items = FerroPhaseParser::new()
        .parse_items_ast(source)
        .expect("host declarations should parse");

    assert_eq!(items.len(), 3);
    assert!(matches!(items[0].kind(), ItemKind::DefStruct(_)));
    match items[1].kind() {
        ItemKind::DeclStatic(decl) => {
            assert!(!decl.mutable);
            assert!(decl.is_host);
        }
        other => panic!("unexpected item: {:?}", other),
    }
    match items[2].kind() {
        ItemKind::DeclStatic(decl) => {
            assert!(decl.mutable);
            assert!(decl.is_host);
        }
        other => panic!("unexpected item: {:?}", other),
    }
}
