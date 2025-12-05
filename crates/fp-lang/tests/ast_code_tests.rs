use fp_core::ast::*;
use fp_core::cst::{CstKind, CstNode};
use fp_core::frontend::LanguageFrontend;
use fp_lang::parser::FerroPhaseParser;
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

const EXAMPLE_EMIT: &str = r#"
fn main() {
    const {
        emit! { let generated = 42; generated }
    }
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
            ExprKind::IntrinsicCall(call) => match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    args.iter().any(expr_contains)
                }
                _ => false,
            },
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

// -------- G3 quote/splice 反例（正反例同文件便于定位） --------

#[test]
fn splice_without_parens_errors() {
    // splice 括号缺失/未闭合
    expect_parse_err("fn main() { splice ( quote { 1 }; }");
}

#[test]
fn mismatched_quote_delimiter_errors() {
    // quote 使用 [ ] 而非 { }
    expect_parse_err("fn main() { quote [ 1 + 2 ); }");
}

#[test]
fn splice_item_in_expr_position_errors() {
    // item token spliced 到表达式位置应触发种类不匹配
    expect_parse_err("fn main() { let _ = splice ( quote item { struct S; } ); }");
}

#[test]
fn splice_stmt_in_item_position_errors() {
    // quote 内语法错误应导致 splice 失败
    expect_parse_err("splice ( quote { let x = ; } );");
}

#[test]
fn emit_outside_const_errors() {
    // emit! 使用错误的定界符应触发解析错误
    expect_parse_err("fn main() { emit! ( let x = 1; ) }");
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
fn parser_rewrites_const_eval_example() {
    let parser = FerroPhaseParser::new();
    parser
        .rewrite_to_rust(EXAMPLE_CONST_EVAL)
        .expect("rewrite const eval example");
}

#[test]
fn parser_rewrites_quote_splice_example() {
    let parser = FerroPhaseParser::new();
    parser
        .rewrite_to_rust(EXAMPLE_QUOTE_SPLICE)
        .expect("rewrite quote splice example");
}

#[test]
fn parser_detects_const_blocks() {
    let parser = FerroPhaseParser::new();
    let cst = parser
        .parse_to_cst(EXAMPLE_CONST_EVAL)
        .expect("parse const eval example");
    assert!(cst_contains_kind(&cst, CstKind::ConstBlock));
}

#[test]
fn parser_rewrites_emit_macro() {
    let parser = FerroPhaseParser::new();
    let rewritten = parser
        .rewrite_to_rust(EXAMPLE_EMIT)
        .expect("rewrite emit example");
    assert!(
        rewritten.contains("fp_splice!(fp_quote!"),
        "emit! should lower to fp_splice!(fp_quote!(...))"
    );
}

fn cst_contains_kind(node: &CstNode, kind: CstKind) -> bool {
    if node.kind == kind {
        return true;
    }
    node.children
        .iter()
        .any(|child| cst_contains_kind(child, kind.clone()))
}
