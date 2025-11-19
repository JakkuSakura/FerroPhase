use fp_core::ast::*;
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;
use std::fs;
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

fn example_path(name: &str) -> PathBuf {
    // examples directory is at workspace root
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // root
    path.push("examples");
    path.push(name);
    path
}

fn read_example_file(name: &str) -> String {
    let path = example_path(name);
    fs::read_to_string(&path).expect(&format!("failed to read example {} at {}", name, path.display()))
}

#[test]
fn code_quote_produces_ast_quote() {
    let fe = FerroFrontend::new();
    let res = fe.parse("quote { 1 + 2 }", None).expect("parse");
    let e = unwrap_node_expr(&res.ast);
    let q = unwrap_expr_quote(e);
    assert!(q.kind.is_none(), "quote.kind should be None; inference happens later");
    assert!(q.block.last_expr().is_some(), "quote block should carry trailing expr");
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
                        BlockStmt::Expr(se) => if expr_contains(se.expr.as_ref()) { return true },
                        BlockStmt::Item(it) => if item_contains(it.as_ref()) { return true },
                        _ => {}
                    }
                }
                if let Some(last) = b.last_expr() { return expr_contains(last) }
                false
            }
            ExprKind::If(i) => expr_contains(i.cond.as_ref()) || expr_contains(i.then.as_ref()) || i.elze.as_ref().map(|e| expr_contains(e.as_ref())).unwrap_or(false),
            ExprKind::While(w) => expr_contains(w.cond.as_ref()) || expr_contains(w.body.as_ref()),
            ExprKind::Loop(l) => expr_contains(l.body.as_ref()),
            ExprKind::Invoke(inv) => inv.args.iter().any(expr_contains),
            ExprKind::Select(sel) => expr_contains(sel.obj.as_ref()),
            ExprKind::Reference(r) => expr_contains(r.referee.as_ref()),
            ExprKind::Dereference(d) => expr_contains(d.referee.as_ref()),
            ExprKind::Array(arr) => arr.values.iter().any(expr_contains),
            ExprKind::ArrayRepeat(rep) => expr_contains(rep.elem.as_ref()) || expr_contains(rep.len.as_ref()),
            ExprKind::Quote(q) => expr_contains(&Expr::block(q.block.clone())),
            ExprKind::IntrinsicCall(call) => match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => args.iter().any(expr_contains),
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
        .parse(
            r#"quote { let x = 1; let y = 2; }"#,
            None,
        )
        .expect("parse");
    let e = unwrap_node_expr(&res.ast);
    let q = unwrap_expr_quote(e);
    assert!(q.block.last_expr().is_none(), "no trailing expr in statement-only block");
    assert!(q.block.stmts.len() >= 2, "expected at least two statements");
}

#[test]
fn parse_quote_splice_example_file() {
    let fe = FerroFrontend::new();
    let code = read_example_file("20_quote_splice.fp");
    let path = example_path("20_quote_splice.fp");
    let res = fe.parse(&code, Some(&path)).expect("parse example");
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
    assert!(matches!(res.ast.kind(), NodeKind::File(_)), "expected file AST");
    assert!(node_contains_splice_quote(&res.ast));
}

#[test]
fn parse_const_eval_basics_example_file() {
    let fe = FerroFrontend::new();
    let code = read_example_file("01_const_eval_basics.fp");
    let path = example_path("01_const_eval_basics.fp");
    let res = fe.parse(&code, Some(&path)).expect("parse example file");
    assert!(matches!(res.ast.kind(), NodeKind::File(_)));
}
