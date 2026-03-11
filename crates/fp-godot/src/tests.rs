use fp_core::ast::{
    AstSerializer, BlockStmt, EnumTypeVariant, Expr, ExprBlock, ExprField, ExprKind, ExprMatch,
    ExprMatchCase, ExprStruct, File, Ident, Item, ItemDefConst, ItemDefEnum, ItemDefFunction,
    ItemDefStruct, ItemImpl, ItemKind, Name, Node, NodeKind, Path, Pattern, PatternKind,
    PatternVariant, StmtLet, StructuralField, Ty, TypeEnum, TypePrimitive, TypeStructural, Value,
    Visibility,
};

use fp_core::module::path::PathPrefix;

use crate::GdscriptSerializer;

#[test]
fn serialize_basic_gdscript_module() {
    let fields = vec![
        StructuralField::new(Ident::new("name"), Ty::Primitive(TypePrimitive::String)),
        StructuralField::new(
            Ident::new("age"),
            Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I64)),
        ),
    ];
    let mut user_struct = ItemDefStruct::new(Ident::new("User"), fields);
    user_struct.visibility = Visibility::Public;

    let const_item = ItemDefConst {
        attrs: Vec::new(),
        mutable: Some(false),
        ty_annotation: None,
        visibility: Visibility::Public,
        name: Ident::new("ANSWER"),
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

    let serializer = GdscriptSerializer;
    let output = serializer
        .serialize_node(&node)
        .expect("serialize should succeed");

    assert!(output.contains("class User:"));
    assert!(output.contains("var name"));
    assert!(output.contains("func _init(name, age):"));
    assert!(output.contains("const ANSWER = 42"));
    assert!(output.contains("func main("));
}

#[test]
fn serialize_enum_with_impl_and_struct_variant_construction() {
    let shape_enum = ItemDefEnum {
        attrs: Vec::new(),
        visibility: Visibility::Public,
        name: Ident::new("Shape"),
        value: TypeEnum {
            name: Ident::new("Shape"),
            generics_params: Vec::new(),
            variants: vec![
                EnumTypeVariant {
                    name: Ident::new("Point"),
                    value: Ty::unit(),
                    discriminant: None,
                },
                EnumTypeVariant {
                    name: Ident::new("Rectangle"),
                    value: Ty::Structural(TypeStructural {
                        fields: vec![
                            StructuralField::new(Ident::new("w"), Ty::unit()),
                            StructuralField::new(Ident::new("h"), Ty::unit()),
                        ],
                    }),
                    discriminant: None,
                },
            ],
        },
    };

    let point_path = Name::path(Path::new(
        PathPrefix::Plain,
        vec![Ident::new("Shape"), Ident::new("Point")],
    ));
    let rectangle_path = Name::path(Path::new(
        PathPrefix::Plain,
        vec![Ident::new("Shape"), Ident::new("Rectangle")],
    ));

    let describe_match = Expr::new(ExprKind::Match(ExprMatch {
        span: fp_core::span::Span::null(),
        scrutinee: Some(Expr::ident(Ident::new("self")).into()),
        cases: vec![
            ExprMatchCase {
                span: fp_core::span::Span::null(),
                pat: Some(Box::new(Pattern::new(PatternKind::Variant(
                    PatternVariant {
                        name: Expr::new(ExprKind::Name(point_path)),
                        pattern: None,
                    },
                )))),
                cond: Expr::value(Value::bool(true)).into(),
                guard: None,
                body: Expr::value(Value::string("point".to_string())).into(),
            },
            ExprMatchCase {
                span: fp_core::span::Span::null(),
                pat: Some(Box::new(Pattern::new(PatternKind::Variant(
                    PatternVariant {
                        name: Expr::new(ExprKind::Name(rectangle_path)),
                        pattern: None,
                    },
                )))),
                cond: Expr::value(Value::bool(true)).into(),
                guard: None,
                body: Expr::value(Value::string("rect".to_string())).into(),
            },
        ],
    }));

    let describe_fn = ItemDefFunction::new_simple(Ident::new("describe"), describe_match.into());
    let impl_shape = ItemImpl::new_ident(
        Ident::new("Shape"),
        vec![Item::new(ItemKind::DefFunction(describe_fn))],
    );

    let rect_struct = Expr::new(ExprKind::Struct(ExprStruct {
        span: fp_core::span::Span::null(),
        name: Expr::new(ExprKind::Name(Name::path(Path::new(
            PathPrefix::Plain,
            vec![Ident::new("Shape"), Ident::new("Rectangle")],
        ))))
        .into(),
        fields: vec![
            ExprField::new(Ident::new("w"), Expr::value(Value::int(1))),
            ExprField::new(Ident::new("h"), Expr::value(Value::int(2))),
        ],
        update: None,
    }));

    let body = Expr::new(ExprKind::Block(ExprBlock::new_stmts(vec![BlockStmt::Let(
        StmtLet::new_simple(Ident::new("rect"), rect_struct),
    )])));
    let main_fn = ItemDefFunction::new_simple(Ident::new("main"), body.into());

    let file = File {
        path: Default::default(),
        items: vec![
            Item::new(ItemKind::DefEnum(shape_enum)),
            Item::new(ItemKind::Impl(impl_shape)),
            Item::new(ItemKind::DefFunction(main_fn)),
        ],
    };

    let node = Node::from(NodeKind::File(file));
    let serializer = GdscriptSerializer;
    let output = serializer
        .serialize_node(&node)
        .expect("serialize should succeed");

    assert!(output.contains("class Shape:"));
    assert!(output.contains("static func Point():"));
    assert!(output.contains("static func Rectangle(w, h):"));
    assert!(output.contains("func describe():"));
    assert!(output.contains("var rect = Shape.Rectangle(1, 2)"));
}
