use fp_core::ast::*;
use fp_core::frontend::LanguageFrontend;
use fp_lang::ast::FerroPhaseParser;
use fp_lang::FerroFrontend;
use std::path::PathBuf;

// --- Small test helpers to streamline assertions ---

fn unwrap_node_expr<'a>(node: &'a Node) -> &'a Expr {
    match node.kind() {
        NodeKind::Expr(e) => e,
        other => panic!("expected NodeKind::Expr, found {:?}", other),
    }
}

fn unwrap_expr_quote<'a>(expr: &'a Expr) -> &'a ExprQuote {
    match expr.kind() {
        ExprKind::Quote(q) => q,
        other => panic!("expected ExprKind::Quote, found {:?}", other),
    }
}

fn unwrap_expr_splice<'a>(expr: &'a Expr) -> &'a ExprSplice {
    match expr.kind() {
        ExprKind::Splice(s) => s,
        other => panic!("expected ExprKind::Splice, found {:?}", other),
    }
}

const EXAMPLE_CONST_EVAL: &str = r#"#!/usr/bin/env fp run
//! Basic const evaluation with compile-time arithmetic and const blocks

fn main() {
    // Basic compile-time computation
    const BUFFER_SIZE: i64 = 1024 * 4;
    const MAX_CONNECTIONS: i64 = 150;
    const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
    const IS_LARGE: bool = BUFFER_SIZE > 2048;

    println!("Buffer: {}KB, factorial(5)={}, large={}",
             BUFFER_SIZE / 1024, FACTORIAL_5, IS_LARGE);

    // Struct with const defaults
    struct Config {
        buffer_size: i64,
        max_connections: i64,
    }

    const DEFAULT_CONFIG: Config = Config {
        buffer_size: BUFFER_SIZE,
        max_connections: MAX_CONNECTIONS,
    };

    println!("Config: {}KB buffer, {} connections",
             DEFAULT_CONFIG.buffer_size / 1024,
             DEFAULT_CONFIG.max_connections);

    // Const blocks: inline compile-time computation
    let runtime_multiplier = 3;
    let optimized_size = const { BUFFER_SIZE * 2 };
    let cache_strategy = const {
        if BUFFER_SIZE > 2048 {
            "large"
        } else {
            "small"
        }
    };
    let total_memory = runtime_multiplier * const { BUFFER_SIZE * MAX_CONNECTIONS };

    println!("Const blocks: size={}, strategy={}, memory={}",
             optimized_size, cache_strategy, total_memory);
}
"#;

const EXAMPLE_QUOTE_SPLICE: &str = r#"#!/usr/bin/env fp run
//! Quote and splice demonstration (parser support TBD)
//! This file documents intended surface usage; the AST-level support exists.

fn first_gt(const xs: [i32], ys: [i32]) -> i32 {
    const {
        for (i, x) in xs.iter().enumerate() {
            // Sugar: emit! { if x > ys[i] { return x; } }
            splice ( quote {
                if x > ys[i] { return x; }
            } );
        }
    }
    0
}

fn main() {
    // Intended usage once parser is wired
    let _ = first_gt([1, 2, 5], [0, 1, 3]);
}
"#;

#[test]
fn code_quote_produces_ast_quote() {
    let fe = FerroFrontend::new();
    let res = fe.parse("quote { 1 + 2 }", None).expect("parse");
    let e = unwrap_node_expr(&res.ast);
    let q = unwrap_expr_quote(e);
    assert!(
        q.kind.is_none(),
        "quote.kind should be None; inference happens later"
    );
    assert!(
        q.block.last_expr().is_some(),
        "quote block should carry trailing expr"
    );
}

#[test]
fn code_splice_produces_ast_splice() {
    let fe = FerroFrontend::new();
    let res = fe.parse("splice ( quote { 3 } )", None).expect("parse");
    let e = unwrap_node_expr(&res.ast);
    let s = unwrap_expr_splice(e);
    match s.token.kind() {
        ExprKind::Quote(q) => assert!(q.kind.is_none()),
        other => panic!("splice should contain quote token, found {:?}", other),
    }
}

#[test]
fn raw_ptr_requires_const_or_mut() {
    let fe = FerroFrontend::new();
    let ok_const = fe.parse("fn f(x: *const i32) {}", None);
    assert!(ok_const.is_ok(), "expected *const to parse");
    let ok_mut = fe.parse("fn f(x: *mut i32) {}", None);
    assert!(ok_mut.is_ok(), "expected *mut to parse");
    let ok_path = fe.parse("fn f(x: *const SockAddrIn) {}", None);
    assert!(ok_path.is_ok(), "expected *const path type to parse");
    let ok_extern = fe.parse(
        r#"extern "C" fn bind(fd: i32, addr: *const SockAddrIn, addrlen: u32) -> i32;"#,
        None,
    );
    assert!(ok_extern.is_ok(), "expected *const in extern fn to parse");
    let ok_extern_in_mod = fe.parse(
        r#"mod libc { extern "C" fn bind(fd: i32, addr: *const SockAddrIn, addrlen: u32) -> i32; }"#,
        None,
    );
    assert!(
        ok_extern_in_mod.is_ok(),
        "expected *const in extern fn inside mod to parse"
    );
    let err_bare = fe.parse("fn f(x: *i32) {}", None);
    assert!(err_bare.is_err(), "expected bare * to be rejected");
}

fn node_contains_splice_quote(node: &Node) -> bool {
    fn expr_contains(e: &Expr) -> bool {
        match e.kind() {
            ExprKind::Splice(s) => matches!(s.token.kind(), ExprKind::Quote(_)),
            ExprKind::Block(b) => {
                for stmt in &b.stmts {
                    match stmt {
                        BlockStmt::Expr(se) => {
                            if expr_contains(se.expr.as_ref()) {
                                return true;
                            }
                        }
                        BlockStmt::Item(it) => {
                            if item_contains(it.as_ref()) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
                if let Some(last) = b.last_expr() {
                    return expr_contains(last);
                }
                false
            }
            ExprKind::If(i) => {
                expr_contains(i.cond.as_ref())
                    || expr_contains(i.then.as_ref())
                    || i.elze
                        .as_ref()
                        .map(|e| expr_contains(e.as_ref()))
                        .unwrap_or(false)
            }
            ExprKind::ConstBlock(cb) => expr_contains(cb.expr.as_ref()),
            ExprKind::While(w) => expr_contains(w.cond.as_ref()) || expr_contains(w.body.as_ref()),
            ExprKind::Loop(l) => expr_contains(l.body.as_ref()),
            ExprKind::Invoke(inv) => inv.args.iter().any(expr_contains),
            ExprKind::Select(sel) => expr_contains(sel.obj.as_ref()),
            ExprKind::Reference(r) => expr_contains(r.referee.as_ref()),
            ExprKind::Dereference(d) => expr_contains(d.referee.as_ref()),
            ExprKind::Array(arr) => arr.values.iter().any(expr_contains),
            ExprKind::ArrayRepeat(rep) => {
                expr_contains(rep.elem.as_ref()) || expr_contains(rep.len.as_ref())
            }
            ExprKind::Quote(q) => expr_contains(&Expr::block(q.block.clone())),
            ExprKind::IntrinsicCall(call) => {
                call.args.iter().any(expr_contains)
                    || call.kwargs.iter().any(|kw| expr_contains(&kw.value))
            }
            _ => false,
        }
    }
    fn item_contains(item: &Item) -> bool {
        match item.kind() {
            ItemKind::DefFunction(f) => expr_contains(f.body.as_ref()),
            ItemKind::Expr(e) => expr_contains(e),
            ItemKind::Module(m) => m.items.iter().any(|it| item_contains(it)),
            _ => false,
        }
    }
    match node.kind() {
        NodeKind::Expr(e) => expr_contains(e),
        NodeKind::Item(i) => item_contains(i),
        NodeKind::File(f) => f.items.iter().any(|it| item_contains(it)),
        _ => false,
    }
}

fn expect_parse_err(src: &str) {
    let parser = FerroPhaseParser::new();
    let res = parser.parse_items_ast(src);
    assert!(res.is_err(), "expected parse error for source:\n{src}");
}

#[test]
fn code_emit_in_fn_lowers_to_splice_of_quote() {
    let fe = FerroFrontend::new();
    let code = r#"fn main() { emit! { let x = 1; } }"#;
    let res = fe.parse(code, None).expect("parse");
    assert!(
        node_contains_splice_quote(&res.ast),
        "expected to find splice(quote {{ .. }}) in AST"
    );
}

#[test]
fn quote_block_with_only_statements_has_no_trailing_expr() {
    let fe = FerroFrontend::new();
    let res = fe
        .parse(r#"quote { let x = 1; let y = 2; }"#, None)
        .expect("parse");
    let e = unwrap_node_expr(&res.ast);
    let q = unwrap_expr_quote(e);
    assert!(
        q.block.last_expr().is_none(),
        "no trailing expr in statement-only block"
    );
    assert!(q.block.stmts.len() >= 2, "expected at least two statements");
}

#[test]
fn parse_quote_splice_example_file() {
    let fe = FerroFrontend::new();
    let path = PathBuf::from("examples/20_quote_splice.fp");
    let res = fe
        .parse(EXAMPLE_QUOTE_SPLICE, Some(&path))
        .expect("parse example");
    // Must produce a file node; example includes splice/quote sugar
    match res.ast.kind() {
        NodeKind::File(_) => {}
        other => panic!("expected NodeKind::File for example, found {:?}", other),
    }
}

#[test]
fn nested_splice_inside_if_parses() {
    let fe = FerroFrontend::new();
    let code = r#"
        fn main() {
            const {
                if true {
                    splice ( quote { let z = 42; } );
                }
            }
        }
    "#;
    let path = PathBuf::from("test.fp");
    let res = fe.parse(code, Some(&path)).expect("parse");
    assert!(
        matches!(res.ast.kind(), NodeKind::File(_)),
        "expected file AST"
    );
    assert!(node_contains_splice_quote(&res.ast));
}

// -------- G3 quote/splice negative cases --------

#[test]
fn splice_without_parens_errors() {
    // `splice` parentheses are missing/unclosed.
    expect_parse_err("fn main() { splice ( quote { 1 }; }");
}

#[test]
fn mismatched_quote_delimiter_errors() {
    // `quote` requires `{ ... }`.
    expect_parse_err("fn main() { quote [ 1 + 2 ); }");
}

#[test]
fn splice_item_in_expr_position_errors() {
    // Splicing an item fragment into expression position should error.
    expect_parse_err("fn main() { let _ = splice ( quote item { struct S; } ); }");
}

#[test]
fn splice_stmt_in_item_position_errors() {
    // Invalid syntax inside a quote should cause the splice to fail.
    expect_parse_err("splice ( quote { let x = ; } );");
}

#[test]
fn emit_outside_const_errors() {
    // `emit! ( ... )` is parsed as a normal macro-style call.
    let fe = FerroFrontend::new();
    let res = fe
        .parse("fn main() { emit! ( let x = 1; ) }", None)
        .expect("parse");
    assert!(matches!(res.ast.kind(), NodeKind::File(_)));
}

#[test]
fn parse_const_eval_basics_example_file() {
    let fe = FerroFrontend::new();
    let path = PathBuf::from("examples/01_const_eval_basics.fp");
    let res = fe
        .parse(EXAMPLE_CONST_EVAL, Some(&path))
        .expect("parse example file");
    assert!(matches!(res.ast.kind(), NodeKind::File(_)));
}

#[test]
fn parser_detects_const_blocks() {
    let parser = FerroPhaseParser::new();
    let cleaned = EXAMPLE_CONST_EVAL
        .lines()
        .skip(1)
        .collect::<Vec<_>>()
        .join("\n");
    let items = parser
        .parse_items_ast(&cleaned)
        .expect("parse const eval example");
    assert!(items_contain_const_block(&items));
}

#[test]
fn cst_printer_can_reconstruct_source() {
    // CST printing is now expression-focused.
    let parser = FerroPhaseParser::new();
    let src = "a + b * 2";
    let cst = parser.parse_expr_cst(src).expect("parse expr CST");
    let printed = fp_lang::syntax::SyntaxPrinter::print(&cst);
    assert_eq!(printed, src);
}

fn items_contain_const_block(items: &[Item]) -> bool {
    fn expr_contains_const_block(expr: &Expr) -> bool {
        match expr.kind() {
            ExprKind::ConstBlock(_) => true,
            ExprKind::Block(block) => block.stmts.iter().any(|stmt| match stmt {
                BlockStmt::Expr(e) => expr_contains_const_block(&e.expr),
                BlockStmt::Let(l) => l.init.as_ref().is_some_and(expr_contains_const_block),
                _ => false,
            }),
            ExprKind::If(i) => {
                expr_contains_const_block(&i.cond)
                    || expr_contains_const_block(&i.then)
                    || i.elze
                        .as_ref()
                        .is_some_and(|e| expr_contains_const_block(e))
            }
            ExprKind::For(f) => {
                expr_contains_const_block(&f.iter) || expr_contains_const_block(&f.body)
            }
            ExprKind::While(w) => {
                expr_contains_const_block(&w.cond) || expr_contains_const_block(&w.body)
            }
            ExprKind::Loop(l) => expr_contains_const_block(&l.body),
            ExprKind::BinOp(b) => {
                expr_contains_const_block(&b.lhs) || expr_contains_const_block(&b.rhs)
            }
            ExprKind::Assign(a) => {
                expr_contains_const_block(&a.target) || expr_contains_const_block(&a.value)
            }
            ExprKind::Invoke(i) => i.args.iter().any(expr_contains_const_block),
            ExprKind::Quote(q) => {
                expr_contains_const_block(&ExprKind::Block(q.block.clone()).into())
            }
            ExprKind::Splice(s) => expr_contains_const_block(&s.token),
            _ => false,
        }
    }

    items.iter().any(|item| match item.kind() {
        ItemKind::DefFunction(def) => expr_contains_const_block(&def.body),
        _ => false,
    })
}
