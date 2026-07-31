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

    let mut file = File {
        path: "loop_bottom.fp".into(),
        attrs: Vec::new(),
        collected_items: Vec::new(),
        items: vec![Item::from(ItemKind::DefFunction(func))],
    };

    let typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(std::rc::Rc::new(
        fp_core::workspace::WorkspaceContext::new(),
    )));
    let typer = AstTypeInferencer::new(typing_ctx.clone());
    fp_typing::block_on(typer.infer_file(&mut file)).expect("infer");
    let has_errors = typing_ctx
        .diagnostics
        .borrow()
        .iter()
        .any(|d| matches!(d.level, fp_typing::TypingDiagnosticLevel::Error));
    assert!(!has_errors, "loop should be bottom type");
}
