use fp_core::ast::*;
use fp_core::span::Span;
use fp_typing::AstTypeInferencer;

#[test]
fn loop_bottom_allows_i64_return() {
    let loop_body = Expr::block(ExprBlock::new());
    let loop_expr: Expr = ExprKind::Loop(ExprLoop {
        span: Span::null(),
        label: None,
        body: loop_body.into(),
    })
    .into();

    let func = ItemDefFunction::new_simple(Ident::new("spin"), loop_expr.into())
        .with_ret_ty(Ty::Primitive(TypePrimitive::i64()));

    let file = File {
        path: "loop_bottom.fp".into(),
        items: vec![Item::from(ItemKind::DefFunction(func))],
    };

    let mut node = Node::new(NodeKind::File(file));
    let mut typer = AstTypeInferencer::new();
    let outcome = typer.infer(&mut node).expect("infer");
    assert!(!outcome.has_errors, "loop should be bottom type");
}
