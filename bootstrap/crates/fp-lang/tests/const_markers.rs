use fp_core::ast::{AttributesExt, FunctionParam, ItemKind, NodeKind};
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;

#[test]
fn const_fn_and_const_param_flags_set() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("const fn foo(const x: i32) -> i32 { x }", None)
        .expect("parse");
    let item = match res.ast.kind() {
        NodeKind::Item(it) => it.clone(),
        NodeKind::File(file) => file.items.first().cloned().expect("file item"),
        other => panic!("expected Item/File, got {:?}", other),
    };
    let is_const = match item.kind() {
        ItemKind::DeclFunction(decl) => {
            assert_eq!(decl.sig.params.len(), 1);
            let FunctionParam { is_const, .. } = &decl.sig.params[0];
            assert!(
                *is_const,
                "const parameter marker should be preserved in AST"
            );
            decl.sig.is_const
        }
        ItemKind::DefFunction(def) => {
            assert_eq!(def.sig.params.len(), 1);
            let FunctionParam { is_const, .. } = &def.sig.params[0];
            assert!(
                *is_const,
                "const parameter marker should be preserved in AST"
            );
            def.sig.is_const
        }
        other => panic!("expected function item, got {:?}", other),
    };
    assert!(is_const, "const fn marker should be set");
}

#[test]
fn const_struct_marker_parsed() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse("const struct TypeBuilder { ty: type }", None)
        .expect("parse");
    let item = match res.ast.kind() {
        NodeKind::Item(it) => it.clone(),
        NodeKind::File(file) => file.items.first().cloned().expect("file item"),
        other => panic!("expected Item/File, got {:?}", other),
    };
    match item.kind() {
        ItemKind::DefStruct(def) => {
            assert!(
                def.attrs.find_by_name("const").is_some(),
                "const struct marker should be preserved in AST"
            );
        }
        other => panic!("expected struct item, got {:?}", other),
    }
}
