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

    let quoted_block =
        ExprBlock::new_stmts(vec![BlockStmt::Expr(BlockStmtExpr::new(ret_expr.clone()))]);
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

    let fn_body = ExprBlock::new_stmts(vec![
        BlockStmt::Expr(BlockStmtExpr::new(const_block_expr).with_semicolon(true)),
        BlockStmt::Expr(BlockStmtExpr::new(Expr::value(Value::int(0)))),
    ]);

    let mut func =
        ItemDefFunction::new_simple(Ident::new("demo"), Expr::block(fn_body.clone()).into());
    func.sig.ret_ty = Some(i32_ty());

    let file = File {
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
                            ExprKind::Value(v) => {
                                matches!(v.as_ref(), Value::Int(i) if i.value == 42)
                            }
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

    assert!(
        saw_return_42,
        "expected spliced return 42; to be present in function body"
    );

    // Also assert no quote/splice nodes remain in the function body after const-eval
    let mut has_quote_or_splice = false;
    fn visit_expr(e: &Expr, hit: &mut bool) {
        match e.kind() {
            ExprKind::Quote(_) | ExprKind::Splice(_) => {
                *hit = true;
            }
            ExprKind::Block(b) => {
                for s in &b.stmts {
                    if let BlockStmt::Expr(es) = s {
                        visit_expr(es.expr.as_ref(), hit);
                    }
                }
                if let Some(last) = b.last_expr() {
                    visit_expr(last, hit);
                }
            }
            ExprKind::If(i) => {
                visit_expr(i.cond.as_ref(), hit);
                visit_expr(i.then.as_ref(), hit);
                if let Some(e) = &i.elze {
                    visit_expr(e.as_ref(), hit);
                }
            }
            ExprKind::Loop(l) => visit_expr(l.body.as_ref(), hit),
            ExprKind::While(w) => {
                visit_expr(w.cond.as_ref(), hit);
                visit_expr(w.body.as_ref(), hit);
            }
            ExprKind::Match(m) => {
                for c in &m.cases {
                    visit_expr(c.cond.as_ref(), hit);
                    visit_expr(c.body.as_ref(), hit);
                }
            }
            ExprKind::Let(l) => visit_expr(l.expr.as_ref(), hit),
            ExprKind::Assign(a) => {
                visit_expr(a.target.as_ref(), hit);
                visit_expr(a.value.as_ref(), hit);
            }
            ExprKind::Invoke(i) => {
                for a in &i.args {
                    visit_expr(a, hit);
                }
            }
            ExprKind::Struct(s) => {
                visit_expr(s.name.as_ref(), hit);
                for f in &s.fields {
                    if let Some(v) = &f.value {
                        visit_expr(v, hit);
                    }
                }
            }
            ExprKind::Structural(s) => {
                for f in &s.fields {
                    if let Some(v) = &f.value {
                        visit_expr(v, hit);
                    }
                }
            }
            ExprKind::IntrinsicContainer(c) => match c {
                ExprIntrinsicContainer::VecElements { elements } => {
                    for e in elements {
                        visit_expr(e, hit);
                    }
                }
                ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    visit_expr(elem, hit);
                    visit_expr(len, hit);
                }
                ExprIntrinsicContainer::HashMapEntries { entries } => {
                    for e in entries {
                        visit_expr(&e.key, hit);
                        visit_expr(&e.value, hit);
                    }
                }
            },
            ExprKind::Array(a) => {
                for v in &a.values {
                    visit_expr(v, hit);
                }
            }
            ExprKind::ArrayRepeat(r) => {
                visit_expr(r.elem.as_ref(), hit);
                visit_expr(r.len.as_ref(), hit);
            }
            ExprKind::Tuple(t) => {
                for v in &t.values {
                    visit_expr(v, hit);
                }
            }
            ExprKind::BinOp(b) => {
                visit_expr(b.lhs.as_ref(), hit);
                visit_expr(b.rhs.as_ref(), hit);
            }
            ExprKind::UnOp(u) => visit_expr(u.val.as_ref(), hit),
            ExprKind::Reference(r) => visit_expr(r.referee.as_ref(), hit),
            ExprKind::Dereference(d) => visit_expr(d.referee.as_ref(), hit),
            ExprKind::Select(s) => visit_expr(s.obj.as_ref(), hit),
            ExprKind::Index(i) => {
                visit_expr(i.obj.as_ref(), hit);
                visit_expr(i.index.as_ref(), hit);
            }
            ExprKind::Cast(c) => visit_expr(c.expr.as_ref(), hit),
            ExprKind::Closure(cl) => visit_expr(cl.body.as_ref(), hit),
            ExprKind::Closured(cl) => visit_expr(cl.expr.as_ref(), hit),
            ExprKind::Try(t) => visit_expr(t.expr.as_ref(), hit),
            ExprKind::Paren(p) => visit_expr(p.expr.as_ref(), hit),
            ExprKind::FormatString(fs) => {
                for a in &fs.args {
                    visit_expr(a, hit);
                }
            }
            _ => {}
        }
    }
    for s in &block.stmts {
        if let BlockStmt::Expr(es) = s {
            visit_expr(es.expr.as_ref(), &mut has_quote_or_splice);
        }
    }
    assert!(
        !has_quote_or_splice,
        "quote/splice should not remain after const-eval in function body"
    );

    Ok(())
}
