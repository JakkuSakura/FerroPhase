use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::intrinsics::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::Result;
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_rust::printer::RustPrinter;

fn i32_ty() -> Ty {
    Ty::Primitive(TypePrimitive::Int(TypeInt::I32))
}

#[test]
fn splice_stmt_expands_inside_const_block() -> Result<()> {
    // Build: fn demo() -> i32 { const { splice quote { return 42; }; } 0 }
    // Return token (as intrinsic return)
    let ret_expr = Expr::from(ExprKind::IntrinsicCall(IntrinsicCall::new(
        IntrinsicCallKind::Return,
        IntrinsicCallPayload::Args {
            args: vec![Expr::value(Value::int(42))],
        },
    )));

    let quoted_block = ExprBlock::new_stmts(vec![BlockStmt::Expr(BlockStmtExpr::new(
        ret_expr.clone(),
    ))]);
    let quote_expr = Expr::from(ExprKind::Quote(ExprQuote {
        block: quoted_block,
        kind: None,
    }));

    let splice_stmt = BlockStmt::Expr(
        BlockStmtExpr::new(Expr::from(ExprKind::Splice(ExprSplice {
            token: quote_expr.into(),
        })))
        .with_semicolon(true),
    );

    let const_block_expr = Expr::from(ExprKind::IntrinsicCall(IntrinsicCall::new(
        IntrinsicCallKind::ConstBlock,
        IntrinsicCallPayload::Args {
            args: vec![Expr::block(ExprBlock::new_stmts(vec![splice_stmt]))],
        },
    )));

    let mut fn_body = ExprBlock::new_stmts(vec![
        BlockStmt::Expr(BlockStmtExpr::new(const_block_expr).with_semicolon(true)),
        BlockStmt::Expr(BlockStmtExpr::new(Expr::value(Value::int(0)))),
    ]);

    let mut func = ItemDefFunction::new_simple(Ident::new("demo"), Expr::block(fn_body.clone()).into());
    func.sig.ret_ty = Some(i32_ty());

    let mut file = File {
        path: PathBuf::from("quote_splice.fp"),
        items: vec![Item::from(ItemKind::DefFunction(func))],
    };

    let mut ast = Node::file(file);

    let serializer: Arc<dyn AstSerializer> = Arc::new(RustPrinter::new());
    fp_core::ast::register_threadlocal_serializer(serializer.clone());

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        debug_assertions: false,
        diagnostics: None,
        diagnostic_context: "ast-interpreter",
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);

    // Inspect mutated AST: find the function and check a return(42) now exists in body
    let file_ref = match ast.kind() {
        NodeKind::File(f) => f,
        _ => panic!("expected file node"),
    };
    let func_ref = match &file_ref.items[0].kind() {
        ItemKind::DefFunction(f) => f,
        _ => panic!("expected function item"),
    };
    let body = func_ref.body.as_ref();
    let block = match body.kind() {
        ExprKind::Block(b) => b,
        other => panic!("expected block body, got {:?}", other),
    };

    let saw_return_42 = block.stmts.iter().any(|stmt| match stmt {
        BlockStmt::Expr(expr_stmt) => match expr_stmt.expr.kind() {
            ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Return => {
                match &call.payload {
                    IntrinsicCallPayload::Args { args } => match args.get(0) {
                        Some(arg) => match arg.kind() {
                            ExprKind::Value(v) => matches!(v.as_ref(), Value::Int(i) if i.value == 42),
                            _ => false,
                        },
                        None => false,
                    },
                    _ => false,
                }
            }
            _ => false,
        },
        _ => false,
    });

    assert!(saw_return_42, "expected spliced return 42; to be present in function body");

    Ok(())
}
