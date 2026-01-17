use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, ExprKind, File, ItemKind, Node, NodeKind, Value,
};
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_core::query::QuerySerializer;
use fp_core::ops::BinOpKind;

#[test]
fn const_eval_replaces_consts_and_records_results() -> Result<()> {
    let serializer: Arc<dyn AstSerializer> = Arc::new(QuerySerializer::new());
    fn ident(name: &str) -> fp_core::ast::Ident {
        fp_core::ast::Ident::new(name)
    }

    let width = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("WIDTH"),
            ty: Some(fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::I64,
            ))),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(6))),
        },
    ));
    let height = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("HEIGHT"),
            ty: Some(fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::I64,
            ))),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(7))),
        },
    ));
    let area_expr = fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(
        fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Mul,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("WIDTH"))),
            rhs: Box::new(fp_core::ast::Expr::ident(ident("HEIGHT"))),
        },
    ));
    let area = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Private,
            name: ident("AREA"),
            ty: Some(fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::I64,
            ))),
            value: Box::new(area_expr),
        },
    ));

    let edge = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Public,
            name: ident("EDGE"),
            ty: Some(fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::I64,
            ))),
            value: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(3))),
        },
    ));
    let perimeter_expr = fp_core::ast::Expr::from(fp_core::ast::ExprKind::BinOp(
        fp_core::ast::ExprBinOp {
            span: fp_core::span::Span::null(),
            kind: BinOpKind::Mul,
            lhs: Box::new(fp_core::ast::Expr::ident(ident("EDGE"))),
            rhs: Box::new(fp_core::ast::Expr::value(fp_core::ast::Value::int(4))),
        },
    ));
    let perimeter = fp_core::ast::Item::from(fp_core::ast::ItemKind::DefConst(
        fp_core::ast::ItemDefConst {
            mutable: None,
            ty_annotation: None,
            visibility: fp_core::ast::Visibility::Public,
            name: ident("PERIMETER"),
            ty: Some(fp_core::ast::Ty::Primitive(fp_core::ast::TypePrimitive::Int(
                fp_core::ast::TypeInt::I64,
            ))),
            value: Box::new(perimeter_expr),
        },
    ));
    let shapes = fp_core::ast::Item::from(fp_core::ast::ItemKind::Module(
        fp_core::ast::Module {
            name: ident("shapes"),
            items: vec![edge, perimeter],
            visibility: fp_core::ast::Visibility::Private,
            is_external: false,
        },
    ));

    let items = vec![width, height, area, shapes];

    let mut ast = Node::file(File {
        path: PathBuf::from("const_eval.fp"),
        items,
    });

    let ctx = SharedScopedContext::new();
    let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
    let outcome = orchestrator.evaluate(&mut ast, &ctx)?;

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
