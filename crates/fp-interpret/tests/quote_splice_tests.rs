use std::path::PathBuf;
use std::sync::Arc;

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ops::BinOpKind;
use fp_core::Result;
use fp_interpret::ast::{AstInterpreter, InterpreterMode, InterpreterOptions};
use fp_rust::printer::RustPrinter;

fn i32_ty() -> Ty {
    Ty::Primitive(TypePrimitive::Int(TypeInt::I32))
}

fn quote_item_expr(item: Item) -> Expr {
    Expr::from(ExprKind::Quote(ExprQuote {
        block: ExprBlock::new_stmts(vec![BlockStmt::Item(Box::new(item))]),
        kind: Some(QuoteFragmentKind::Item),
    }))
}

#[test]
fn quote_fn_structural_pattern_binds_name() -> Result<()> {
    let func = ItemDefFunction::new_simple(
        Ident::new("inspected"),
        Expr::value(Value::unit()).into(),
    );
    let token = Value::QuoteToken(ValueQuoteToken {
        kind: QuoteFragmentKind::Item,
        value: QuoteTokenValue::Items(vec![Item::from(ItemKind::DefFunction(func))]),
    });

    let mut quote = PatternQuote {
        fragment: QuoteFragmentKind::Item,
        item: Some(QuoteItemKind::Function),
        fields: vec![PatternStructField {
            name: Ident::new("name"),
            rename: Some(Box::new(Pattern::new(PatternKind::Ident(
                PatternIdent::new(Ident::new("fn_name")),
            )))),
        }],
        has_rest: true,
    };

    let pattern = Pattern::new(PatternKind::Quote(quote.clone()));

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        debug_assertions: false,
        diagnostics: None,
        diagnostic_context: "ast-interpreter",
        module_resolution: None,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);

    assert!(interpreter.pattern_matches(&pattern, &token));
    let bound = interpreter.lookup_value("fn_name");
    assert!(matches!(bound, Some(Value::String(s)) if s.value == "inspected"));

    quote.fields[0].rename = None;
    let pattern_no_bind = Pattern::new(PatternKind::Quote(quote));
    assert!(interpreter.pattern_matches(&pattern_no_bind, &token));

    Ok(())
}

#[test]
fn quote_items_plural_pattern_binds_list() -> Result<()> {
    let func_a = ItemDefFunction::new_simple(
        Ident::new("a"),
        Expr::value(Value::unit()).into(),
    );
    let func_b = ItemDefFunction::new_simple(
        Ident::new("b"),
        Expr::value(Value::unit()).into(),
    );
    let token = Value::QuoteToken(ValueQuoteToken {
        kind: QuoteFragmentKind::Item,
        value: QuoteTokenValue::Items(vec![
            Item::from(ItemKind::DefFunction(func_a)),
            Item::from(ItemKind::DefFunction(func_b)),
        ]),
    });

    let pattern = Pattern::new(PatternKind::QuotePlural(PatternQuotePlural {
        fragment: QuoteFragmentKind::Item,
        patterns: vec![Pattern::new(PatternKind::Ident(PatternIdent::new(
            Ident::new("items"),
        )))],
    }));

    let ctx = SharedScopedContext::new();
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        debug_assertions: false,
        diagnostics: None,
        diagnostic_context: "ast-interpreter",
        module_resolution: None,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);

    assert!(interpreter.pattern_matches(&pattern, &token));
    let bound = interpreter.lookup_value("items");
    assert!(matches!(bound, Some(Value::List(list)) if list.values.len() == 2));

    Ok(())
}

#[test]
fn splice_stmt_expands_inside_const_block() -> Result<()> {
    // Build: fn demo() -> i32 { const { splice quote { return 42; }; } 0 }
    // Return token
    let ret_expr = Expr::from(ExprKind::Return(ExprReturn {
        value: Some(Expr::value(Value::int(42)).into()),
    }));

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

    let const_block_expr = Expr::from(ExprKind::ConstBlock(ExprConstBlock {
        expr: Expr::block(ExprBlock::new_stmts(vec![splice_stmt])).into(),
    }));

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
        module_resolution: None,
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
            ExprKind::Return(ret) => match ret.value.as_ref() {
                Some(arg) => match arg.kind() {
                    ExprKind::Value(v) => matches!(v.as_ref(), Value::Int(i) if i.value == 42),
                    _ => false,
                }
                None => false,
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

#[test]
fn splice_supports_function_returning_item_list() -> Result<()> {
    let struct_a = Item::from(ItemKind::DefStruct(ItemDefStruct::new(
        Ident::new("GeneratedA"),
        vec![],
    )));
    let struct_b = Item::from(ItemKind::DefStruct(ItemDefStruct::new(
        Ident::new("GeneratedB"),
        vec![],
    )));

    let then_items = ExprKind::Array(ExprArray {
        values: vec![quote_item_expr(struct_a)],
    })
    .into();
    let else_items = ExprKind::Array(ExprArray {
        values: vec![quote_item_expr(struct_b)],
    })
    .into();

    let if_expr: Expr = ExprKind::If(ExprIf {
        cond: Box::new(Expr::value(Value::bool(true))),
        then: Box::new(then_items),
        elze: Some(Box::new(else_items)),
    })
    .into();

    let build_fn = ItemDefFunction::new_simple(Ident::new("build_items"), if_expr.into());

    let invoke = ExprKind::Invoke(ExprInvoke {
        target: ExprInvokeTarget::Function(Locator::from_ident(Ident::new("build_items"))),
        args: vec![],
    })
    .into();
    let splice_expr: Expr = ExprKind::Splice(ExprSplice {
        token: Box::new(invoke),
    })
    .into();
    let const_block = ExprKind::ConstBlock(ExprConstBlock {
        expr: Expr::block(ExprBlock::new_stmts(vec![BlockStmt::Expr(
            BlockStmtExpr::new(splice_expr).with_semicolon(true),
        )]))
        .into(),
    })
    .into();

    let file = File {
        path: PathBuf::from("quote_splice_items.fp"),
        items: vec![
            Item::from(ItemKind::DefFunction(build_fn)),
            Item::from(ItemKind::Expr(const_block)),
        ],
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
        module_resolution: None,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);

    let file_ref = match ast.kind() {
        NodeKind::File(f) => f,
        _ => panic!("expected file node"),
    };

    let has_generated_a = file_ref.items.iter().any(|item| match item.kind() {
        ItemKind::DefStruct(def) => def.name.as_str() == "GeneratedA",
        _ => false,
    });

    assert!(has_generated_a, "expected GeneratedA to be spliced");

    Ok(())
}

#[test]
fn splice_executes_expr_outside_const_block() -> Result<()> {
    let quoted_expr = Expr::from(ExprKind::Quote(ExprQuote {
        block: ExprBlock::new_stmts(vec![BlockStmt::Expr(BlockStmtExpr::new(
            Expr::from(ExprKind::BinOp(ExprBinOp {
                kind: BinOpKind::Add,
                lhs: Box::new(Expr::value(Value::int(7))),
                rhs: Box::new(Expr::value(Value::int(5))),
            })),
        ))]),
        kind: Some(QuoteFragmentKind::Expr),
    }));
    let splice_expr = Expr::from(ExprKind::Splice(ExprSplice {
        token: quoted_expr.into(),
    }));

    let let_stmt = BlockStmt::Let(StmtLet::new_simple(Ident::new("result"), splice_expr));

    let fn_body = ExprBlock::new_stmts(vec![
        let_stmt,
        BlockStmt::Expr(BlockStmtExpr::new(Expr::value(Value::int(0)))),
    ]);

    let mut func =
        ItemDefFunction::new_simple(Ident::new("demo"), Expr::block(fn_body).into());
    func.sig.ret_ty = Some(i32_ty());

    let file = File {
        path: PathBuf::from("quote_splice_exec.fp"),
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
        module_resolution: None,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);

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
    let mut saw_splice = false;
    let mut saw_binop = false;
    let mut saw_literal = false;
    for stmt in &block.stmts {
        if let BlockStmt::Let(stmt_let) = stmt {
            if let Some(init) = &stmt_let.init {
                match init.kind() {
                    ExprKind::Splice(_) => saw_splice = true,
                    ExprKind::BinOp(_) => saw_binop = true,
                    ExprKind::Value(v) => {
                        if matches!(v.as_ref(), Value::Int(i) if i.value == 12) {
                            saw_literal = true;
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    assert!(!saw_splice, "splice should be evaluated in const eval");
    assert!(
        saw_binop || saw_literal,
        "expected splice to expand to a binop or a folded literal"
    );

    Ok(())
}

#[test]
fn splice_allows_items_inside_function_bodies() -> Result<()> {
    let struct_item = Item::from(ItemKind::DefStruct(ItemDefStruct::new(
        Ident::new("Inner"),
        vec![StructuralField::new(Ident::new("value"), i32_ty())],
    )));
    let quoted_items = quote_item_expr(struct_item);
    let splice_expr = Expr::from(ExprKind::Splice(ExprSplice {
        token: quoted_items.into(),
    }));

    let fn_body = ExprBlock::new_stmts(vec![
        BlockStmt::Expr(BlockStmtExpr::new(splice_expr).with_semicolon(true)),
        BlockStmt::Expr(BlockStmtExpr::new(Expr::value(Value::int(0)))),
    ]);
    let mut func =
        ItemDefFunction::new_simple(Ident::new("demo"), Expr::block(fn_body).into());
    func.sig.ret_ty = Some(i32_ty());

    let file = File {
        path: PathBuf::from("quote_splice_items_fn.fp"),
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
        module_resolution: None,
    };
    let mut interpreter = AstInterpreter::new(&ctx, options);
    interpreter.interpret(&mut ast);

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
    let has_inner_struct = block.stmts.iter().any(|stmt| match stmt {
        BlockStmt::Item(item) => matches!(
            item.kind(),
            ItemKind::DefStruct(def) if def.name.as_str() == "Inner"
        ),
        _ => false,
    });
    assert!(has_inner_struct, "expected Inner struct spliced into body");

    Ok(())
}
