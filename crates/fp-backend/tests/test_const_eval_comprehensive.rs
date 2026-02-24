use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, BlockStmt, BlockStmtExpr, ExprBlock, ExprConstBlock, ExprField, ExprInvoke,
    ExprKind, ExprSelect, ExprSelectType, ExprStruct, File, Item, ItemDefConst, ItemDefStruct,
    ItemKind, Node, NodeKind, Pattern, PatternIdent, PatternKind, StmtLet, StructuralField, Ty,
    TypeInt, TypePrimitive, Value,
};
use fp_core::context::SharedScopedContext;
use fp_core::ops::BinOpKind;
use fp_core::span::Span;
use fp_core::Result;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;

#[test]
fn const_eval_replaces_consts_and_records_results() -> Result<()> {
    let serializer: Arc<dyn AstSerializer> = Arc::new(TestSerializer);
    fn ident(name: &str) -> fp_core::ast::Ident {
        fp_core::ast::Ident::new(name)
    }

    let width = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("WIDTH"),
            ty: Some(fp_core::ast::Ty::Primitive(
                fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I64),
            )),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(6))),
        },
    ));
    let height = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("HEIGHT"),
            ty: Some(fp_core::ast::Ty::Primitive(
                fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I64),
            )),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(7))),
        },
    ));
    let area_expr =
        fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Mul,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("WIDTH"))),
            rhs: Box::new(fp_core::ast::Expr::ident(ident("HEIGHT"))),
        }));
    let area = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("AREA"),
            ty: Some(fp_core::ast::Ty::Primitive(
                fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I64),
            )),
            value: Box::new(area_expr),
        },
    ));

    let edge = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Public,
            name: ident("EDGE"),
            ty: Some(fp_core::ast::Ty::Primitive(
                fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I64),
            )),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(3))),
        },
    ));
    let perimeter_expr =
        fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Mul,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("EDGE"))),
            rhs: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(4))),
        }));
    let perimeter = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            attrs: Vec::new(),
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Public,
            name: ident("PERIMETER"),
            ty: Some(fp_core::ast::Ty::Primitive(
                fp_core::ast::TypePrimitive::Int(fp_core::ast::TypeInt::I64),
            )),
            value: Box::new(perimeter_expr),
        },
    ));
    let shapes = fp_core::ast::Item::from(fp_core::ast::ItemKind::Module(fp_core::ast::Module {
        attrs: Vec::new(),
        name: ident("shapes"),
        items: vec![edge, perimeter],
        visibility: fp_core::ast::Visibility::Private,
        is_external: false,
    }));

    let items = vec![width, height, area, shapes];

    let mut ast = Node::file(File {
        path: PathBuf::from("const_eval.fp"),
        items,
    });

    let ctx = SharedScopedContext::new();
    fp_core::ast::register_threadlocal_serializer(serializer.clone());
    let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
    let outcome = orchestrator.evaluate(&mut ast, &ctx, None, None)?;

    assert!(
        outcome.mutations_applied,
        "const eval should rewrite literal expressions"
    );
    assert!(
        !outcome.has_errors,
        "const eval should complete without diagnostics"
    );
    assert!(outcome.diagnostics.is_empty(), "no diagnostics expected");

    assert_eq!(
        Value::int(42),
        *outcome
            .evaluated_constants
            .get("AREA")
            .expect("AREA constant should be evaluated"),
    );

    assert_eq!(
        Value::int(3),
        *outcome
            .evaluated_constants
            .get("shapes::EDGE")
            .expect("qualified constant EDGE should be tracked"),
    );
    assert_eq!(
        Value::int(12),
        *outcome
            .evaluated_constants
            .get("shapes::PERIMETER")
            .expect("qualified constant PERIMETER should be tracked"),
    );

    let file = match ast.kind() {
        NodeKind::File(file) => file,
        _ => panic!("expected file node after const eval"),
    };

    let area_const = match file.items[2].kind() {
        ItemKind::DefConst(def) => def,
        other => panic!("expected third item to remain a const, found {other:?}"),
    };
    match area_const.value.kind() {
        ExprKind::Value(value) => {
            let int_value = match value.as_ref() {
                Value::Int(int_value) => int_value.value,
                other => panic!("expected int value after const eval, found {other:?}"),
            };
            assert_eq!(42, int_value);
        }
        other => panic!("expected AREA value to be literal after const eval, found {other:?}"),
    }

    let shapes_module = file
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::Module(module) if module.name.as_str() == "shapes" => Some(module),
            _ => None,
        })
        .expect("shapes module should remain in AST");

    let perimeter_const = shapes_module
        .items
        .iter()
        .find_map(|item| match item.kind() {
            ItemKind::DefConst(def) if def.name.as_str() == "PERIMETER" => Some(def),
            _ => None,
        })
        .expect("PERIMETER const should still exist inside module");

    match perimeter_const.value.kind() {
        ExprKind::Value(value) => {
            let int_value = match value.as_ref() {
                Value::Int(int_value) => int_value.value,
                other => panic!("expected int value after const eval, found {other:?}"),
            };
            assert_eq!(12, int_value);
        }
        other => panic!("expected PERIMETER to be a literal after const eval, found {other:?}"),
    }

    Ok(())
}

#[test]
fn const_eval_materializes_late_type_for_typing() -> Result<()> {
    let serializer: Arc<dyn AstSerializer> = Arc::new(TestSerializer);
    fn ident(name: &str) -> fp_core::ast::Ident {
        fp_core::ast::Ident::new(name)
    }

    let later_struct = fp_core::ast::Item::from(ItemKind::DefStruct(ItemDefStruct::new(
        ident("Later"),
        vec![StructuralField::new(
            ident("x"),
            Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        )],
    )));

    let struct_expr = ExprKind::Struct(ExprStruct {
        span: Span::null(),
        name: Box::new(fp_core::ast::Expr::ident(ident("Later"))),
        fields: vec![ExprField::new(
            ident("x"),
            fp_core::ast::Expr::value(Value::int(1)),
        )],
        update: None,
    });

    let let_stmt = StmtLet::new(
        Pattern::from(PatternKind::Ident(PatternIdent::new(ident("p")))),
        Some(fp_core::ast::Expr::from(struct_expr)),
        None,
    );

    let select_expr = fp_core::ast::Expr::from(ExprKind::Select(ExprSelect {
        span: Span::null(),
        obj: Box::new(fp_core::ast::Expr::ident(ident("p"))),
        field: ident("x"),
        select: ExprSelectType::Field,
    }));

    let block = ExprBlock {
        span: Span::null(),
        stmts: vec![
            BlockStmt::Let(let_stmt),
            BlockStmt::Expr(BlockStmtExpr::new(select_expr).with_semicolon(false)),
        ],
    };

    let const_expr = fp_core::ast::Expr::from(ExprKind::ConstBlock(ExprConstBlock {
        span: Span::null(),
        expr: Box::new(fp_core::ast::Expr::from(ExprKind::Block(block))),
    }));

    let const_item = fp_core::ast::Item::from(ItemKind::DefConst(ItemDefConst {
        attrs: Vec::new(),
        mutable: None,
        ty_annotation: None,
        visibility: fp_core::ast::Visibility::Private,
        name: ident("VALUE"),
        ty: Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
        value: Box::new(const_expr),
    }));

    let mut ast = Node::file(File {
        path: PathBuf::from("lazy_types.fp"),
        items: vec![const_item, later_struct],
    });

    let ctx = SharedScopedContext::new();
    let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
    let outcome = orchestrator.evaluate(&mut ast, &ctx, None, None)?;

    assert!(!outcome.has_errors, "const eval should succeed");

    let NodeKind::File(file) = ast.kind() else {
        panic!("expected file node after const eval");
    };
    let value_item = file
        .items
        .iter()
        .find(|item| matches!(item.kind(), ItemKind::DefConst(def) if def.name.as_str() == "VALUE"))
        .expect("VALUE const should exist");

    let ItemKind::DefConst(def) = value_item.kind() else {
        panic!("expected VALUE to be a const");
    };
    match def.value.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::Int(int_value) => {
                assert_eq!(int_value.value, 1, "VALUE should evaluate to 1");
            }
            other => panic!("expected VALUE to be literal int after const eval, got {other:?}"),
        },
        other => panic!("expected VALUE to be literal after const eval, got {other:?}"),
    }

    Ok(())
}

#[derive(Clone)]
struct TestSerializer;

impl AstSerializer for TestSerializer {
    fn serialize_expr(&self, node: &fp_core::ast::Expr) -> fp_core::Result<String> {
        Ok(format!("{node:?}"))
    }

    fn serialize_value(&self, node: &Value) -> fp_core::Result<String> {
        Ok(format!("{node:?}"))
    }

    fn serialize_type(&self, node: &Ty) -> fp_core::Result<String> {
        Ok(format!("{node:?}"))
    }

    fn serialize_item(&self, node: &Item) -> fp_core::Result<String> {
        Ok(format!("{node:?}"))
    }

    fn serialize_invoke(&self, node: &ExprInvoke) -> fp_core::Result<String> {
        Ok(format!("{node:?}"))
    }
}
