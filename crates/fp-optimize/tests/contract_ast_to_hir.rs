use fp_core::hir::{self, HirItemKind};
use fp_optimize::error::Result as OptimizeResult;
use fp_optimize::transformations::{HirGenerator, IrTransform};

mod support;

#[test]
fn transforms_literal_expression_into_main_function() -> OptimizeResult<()> {
    let ast_expr = support::ast::literal_expr(42);
    let mut generator = HirGenerator::new();

    let program = generator.transform(&ast_expr)?;

    assert_eq!(program.items.len(), 1);
    let item = &program.items[0];
    match &item.kind {
        HirItemKind::Function(func) => {
            assert_eq!(func.sig.name, "main");
            assert_eq!(func.sig.inputs.len(), 0);
            assert!(!func.is_const);

            let body = func.body.as_ref().expect("main should have a body");
            assert_eq!(body.params.len(), 0);
            support::assertions::assert_hir_integer(&body.value, 42);
        }
        kind => panic!("expected function item, found {:?}", kind),
    }

    Ok(())
}

#[test]
fn propagates_unimplemented_expression_error() {
    use fp_core::ast::{AstExpr, ExprTry};

    let mut generator = HirGenerator::new();
    let unsupported = AstExpr::Try(ExprTry {
        expr: Box::new(AstExpr::unit()),
    });
    let result = generator.transform(&unsupported);
    assert!(result.is_err());
}
