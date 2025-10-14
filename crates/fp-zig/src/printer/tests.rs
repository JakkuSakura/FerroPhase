use fp_core::ast::{
    self, AstSerializer, Expr, ExprAssign, ExprStruct, Item, ItemKind, Node, NodeKind, Ty, Value,
};
use fp_core::ops::BinOpKind;

use crate::ZigParser;

use super::{ZigEmitter, ZigSerializer};

#[test]
fn renders_basic_function_stub() {
    let body = Expr::value(Value::unit()).into();
    let mut function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
    function.sig.ret_ty = Some(Ty::unit());

    let item = Item::new(ItemKind::DefFunction(function));
    let node = Node::from(ast::NodeKind::Item(item));

    let serializer = ZigSerializer;
    let rendered = serializer.serialize_node(&node).expect("serialize");

    assert!(rendered.contains("pub fn main"));
    assert!(rendered.contains("TODO"));
}

#[test]
fn renders_binary_expression() {
    let expr = Expr::from(ast::ExprBinOp {
        kind: BinOpKind::Add,
        lhs: Expr::ident(ast::Ident::new("a")).into(),
        rhs: Expr::ident(ast::Ident::new("b")).into(),
    });

    let emitter = ZigEmitter::new();
    let rendered = emitter.render_expr(&expr).expect("render");

    assert_eq!(rendered, "(a + b)");
}

#[test]
fn renders_struct_literal() {
    let struct_expr = Expr::from(ExprStruct::new(
        Expr::ident(ast::Ident::new("Point")).into(),
        vec![
            ast::ExprField::new(ast::Ident::new("x"), Expr::value(Value::int(1))),
            ast::ExprField::new(ast::Ident::new("y"), Expr::value(Value::int(2))),
        ],
    ));

    let emitter = ZigEmitter::new();
    let rendered = emitter.render_expr(&struct_expr).expect("render");

    assert!(
        rendered.contains("Point{ .x = 1, .y = 2 }"),
        "rendered struct literal: {rendered}"
    );
}

#[test]
fn renders_assignment_expression() {
    let assign_expr = Expr::from(ExprAssign {
        target: Expr::ident(ast::Ident::new("value")).into(),
        value: Expr::value(Value::int(42)).into(),
    });

    let emitter = ZigEmitter::new();
    let rendered = emitter.render_expr(&assign_expr).expect("render");

    assert_eq!(rendered, "value = 42");
}

#[test]
fn parses_struct_and_function() {
    let source = r#"
pub const Point = struct {
    x: i32,
    y: i32,
};

pub fn main() void {
    std.debug.print("Hello", .{});
}
"#;

    let mut parser = ZigParser::new().expect("create parser");
    let node = parser.parse_str(source).expect("parse");
    let NodeKind::File(file) = node.kind() else {
        panic!("expected file node");
    };

    assert_eq!(file.items.len(), 2, "unexpected item count");

    let struct_item = file.items[0]
        .as_struct()
        .expect("first item should be struct");
    assert_eq!(struct_item.name.name, "Point");
    assert_eq!(struct_item.value.fields.len(), 2);

    let function_item = file.items[1]
        .as_function()
        .expect("second item should be function");
    assert_eq!(function_item.name.name, "main");
    assert_eq!(function_item.sig.params.len(), 0);
    assert!(function_item.sig.ret_ty.as_ref().is_some_and(|ty| ty == &Ty::unit()));
}
