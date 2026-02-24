use fp_core::ast::{
    AstSerializer, Expr, ExprBlock, ExprKind, File, Ident, Item, ItemDefConst, ItemDefFunction,
    ItemDefStruct, ItemKind, Node, NodeKind, StructuralField, Ty, TypePrimitive, Value,
};

use crate::{GoParser, GoSerializer};

#[test]
fn parse_basic_go_source() {
    let source = r#"
package main

import "fmt"

type User struct {
    Name string
    Age int
}

const Answer = 42

func main() {
    fmt.Println(Answer)
}
"#;

    let mut parser = GoParser::default();
    let node = parser.parse_str(source).expect("parse should succeed");
    let NodeKind::File(file) = node.kind() else {
        panic!("expected file node");
    };

    let has_struct = file.items.iter().any(|item| match item.kind() {
        ItemKind::DefStruct(def) => def.name.name == "User",
        _ => false,
    });
    let has_const = file.items.iter().any(|item| match item.kind() {
        ItemKind::DefConst(def) => def.name.name == "Answer",
        _ => false,
    });
    let has_func = file.items.iter().any(|item| match item.kind() {
        ItemKind::DefFunction(def) => def.name.name == "main",
        _ => false,
    });

    assert!(has_struct, "expected struct declaration");
    assert!(has_const, "expected const declaration");
    assert!(has_func, "expected function declaration");
}

#[test]
fn serialize_basic_go_ast() {
    let fields = vec![
        StructuralField::new(Ident::new("Name"), Ty::Primitive(TypePrimitive::String)),
        StructuralField::new(
            Ident::new("Age"),
            Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I64)),
        ),
    ];
    let mut user_struct = ItemDefStruct::new(Ident::new("User"), fields);
    user_struct.visibility = fp_core::ast::Visibility::Public;

    let const_item = ItemDefConst {
        attrs: Vec::new(),
        mutable: Some(false),
        ty_annotation: None,
        visibility: fp_core::ast::Visibility::Public,
        name: Ident::new("Answer"),
        ty: None,
        value: Expr::value(Value::int(42)).into(),
    };

    let body = Expr::new(ExprKind::Block(ExprBlock::new()));
    let func = ItemDefFunction::new_simple(Ident::new("main"), body.into());

    let file = File {
        path: Default::default(),
        items: vec![
            Item::new(ItemKind::DefStruct(user_struct)),
            Item::new(ItemKind::DefConst(const_item)),
            Item::new(ItemKind::DefFunction(func)),
        ],
    };

    let node = Node::from(NodeKind::File(file));
    let serializer = GoSerializer::default();
    let output = serializer
        .serialize_node(&node)
        .expect("serialize should succeed");

    assert!(output.contains("package main"));
    assert!(output.contains("type User struct"));
    assert!(output.contains("const Answer"));
    assert!(output.contains("func main"));
}
