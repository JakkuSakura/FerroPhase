use fp_core::ast::{
    BlockStmt, EnumTypeVariant, Expr, ExprBlock, ExprField, ExprKind, ExprMatch,
    ExprMatchCase, ExprStruct, File, Ident, Item, ItemDefConst, ItemDefEnum, ItemDefFunction,
    ItemDefStruct, ItemImpl, ItemKind, Name, Path, Pattern, PatternKind, PatternVariant,
    ReprOptions, StmtLet, StructuralField, Ty, TypeEnum, TypePrimitive, TypeStructural, Value,
    Visibility,
};

use fp_core::ast::path::PathPrefix;

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

    let body = ExprBlock::new();
    let func = ItemDefFunction::new_simple(Ident::new("main"), body);

    let file = File {
        path: Default::default(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![
            Item::new(ItemKind::DefStruct(user_struct)),
            Item::new(ItemKind::DefConst(const_item)),
            Item::new(ItemKind::DefFunction(func)),
        ],
    };
    let node = file;

    let serializer = GdscriptSerializer;
    let output = serializer
        .serialize_file(&node)
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
            repr: ReprOptions::default(),
            variants: vec![
                EnumTypeVariant {
                    attrs: Vec::new(),
                    name: Ident::new("Point"),
                    value: Ty::unit(),
                    discriminant: None,
                },
                EnumTypeVariant {
                    attrs: Vec::new(),
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

    let describe_fn = ItemDefFunction::new_simple(
        Ident::new("describe"),
        ExprBlock::new_expr(describe_match),
    );
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

    let body = ExprBlock::new_stmts(vec![BlockStmt::Let(StmtLet::new_simple(
        Ident::new("rect"),
        rect_struct,
    ))]);
    let main_fn = ItemDefFunction::new_simple(Ident::new("main"), body);

    let file = File {
        path: Default::default(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![
            Item::new(ItemKind::DefEnum(shape_enum)),
            Item::new(ItemKind::Impl(impl_shape)),
            Item::new(ItemKind::DefFunction(main_fn)),
        ],
    };

    let node = file;
    let serializer = GdscriptSerializer;
    let output = serializer
        .serialize_file(&node)
        .expect("serialize should succeed");

    assert!(output.contains("class Shape:"));
    assert!(output.contains("static func Point():"));
    assert!(output.contains("static func Rectangle(w, h):"));
    assert!(output.contains("func describe():"));
    assert!(output.contains("var rect = Shape.Rectangle(1, 2)"));
}

#[test]
fn block_expr_with_leading_statements_errors_instead_of_silently_discarding_them() {
    // A block used as a value, with a leading statement before its final
    // expression — GDScript has no general-purpose block expression, so
    // this must be a real error rather than silently dropping the leading
    // statement and rendering a bare `null`.
    let block_with_stmts = Expr::new(ExprKind::Block(ExprBlock::new_stmts_expr(
        vec![BlockStmt::Let(StmtLet::new_simple(
            Ident::new("unused"),
            Expr::value(Value::int(1)),
        ))],
        Expr::value(Value::int(2)),
    )));
    let body = ExprBlock::new_stmts(vec![BlockStmt::Let(StmtLet::new_simple(
        Ident::new("x"),
        block_with_stmts,
    ))]);
    let main_fn = ItemDefFunction::new_simple(Ident::new("main"), body);
    let file = File {
        path: Default::default(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::new(ItemKind::DefFunction(main_fn))],
    };

    let serializer = GdscriptSerializer;
    let result = serializer.serialize_file(&file);
    assert!(
        result.is_err(),
        "a block expression with leading statements has no honest GDScript \
         rendering — this must be a real error, not a silent `null`"
    );
}

#[test]
fn match_arm_with_unused_binding_errors_instead_of_silently_returning_null() {
    // A tuple-struct pattern binding `x`, whose arm body does not simply
    // reference `x` directly (the one shape this serializer *can* render,
    // via `scrutinee.data["0"]`) — this needs a temporary variable this
    // serializer doesn't yet introduce, so it must be a real error rather
    // than silently rendering `null`.
    let pat = Pattern::new(PatternKind::TupleStruct(fp_core::ast::PatternTupleStruct {
        name: Name::path(Path::new(PathPrefix::Plain, vec![Ident::new("Wrapper")])),
        patterns: vec![Pattern::new(PatternKind::Ident(
            fp_core::ast::PatternIdent::new(Ident::new("x")),
        ))],
    }));
    let match_expr = Expr::new(ExprKind::Match(ExprMatch {
        span: fp_core::span::Span::null(),
        scrutinee: Some(Expr::ident(Ident::new("wrapped")).into()),
        cases: vec![ExprMatchCase {
            span: fp_core::span::Span::null(),
            pat: Some(Box::new(pat)),
            cond: Expr::value(Value::bool(true)).into(),
            guard: None,
            // Deliberately not `x` itself, so the fast-path in
            // `render_match_body` doesn't apply.
            body: Expr::value(Value::bool(true)).into(),
        }],
    }));
    let body = ExprBlock::new_stmts(vec![BlockStmt::Let(StmtLet::new_simple(
        Ident::new("result"),
        match_expr,
    ))]);
    let main_fn = ItemDefFunction::new_simple(Ident::new("main"), body);
    let file = File {
        path: Default::default(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::new(ItemKind::DefFunction(main_fn))],
    };

    let serializer = GdscriptSerializer;
    let result = serializer.serialize_file(&file);
    assert!(
        result.is_err(),
        "a match arm using its bound name in anything but the direct \
         `x` shape has no honest GDScript rendering yet — this must be a \
         real error, not a silent `null`"
    );
}
