use fp_core::ast::*;
use fp_core::module::path::QualifiedPath;
use fp_typing::{AstTypeInferencer, ResolvedNameNamespace};

#[test]
fn type_inference_records_resolved_name_on_tast_expr() {
    let const_item = Item::from(ItemKind::DefConst(ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: Visibility::Public,
        name: Ident::new("VALUE"),
        ty: Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
        value: Box::new(Expr::value(Value::int(1))),
    }));
    let mut expr = Expr::name(Name::path(Path::new(
        fp_core::module::path::PathPrefix::Crate,
        vec![Ident::new("VALUE")],
    )));
    let mut node = Node::new(NodeKind::File(File {
        path: "resolved_names.fp".into(),
        attrs: Vec::new(),
        items: vec![const_item, Item::from(ItemKind::Expr(expr.clone()))],
    }));

    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "expected typing to succeed");

    let NodeKind::File(file) = node.kind else {
        panic!("expected file node");
    };
    let ItemKind::Expr(typed_expr) = file.items[1].kind() else {
        panic!("expected expr item");
    };
    expr = typed_expr.clone();
    let resolved = outcome
        .resolved_names
        .get(&expr.id())
        .expect("typing outcome should carry resolved name for expression");
    assert_eq!(resolved.path, QualifiedPath::new(vec!["VALUE".to_string()]));
    assert!(matches!(resolved.namespace, ResolvedNameNamespace::Value));
}
