use fp_core::ast::{Item, ItemKind};
use fp_lang::ast::FerroPhaseParser;

fn parse_single_item(src: &str) -> Item {
    let parser = FerroPhaseParser::new();
    let mut items = parser.parse_items_ast(src).expect("parse succeeds");
    assert_eq!(items.len(), 1, "expected single item");
    items.pop().unwrap()
}

fn fn_body_kinds(item: &Item) -> Vec<String> {
    match item.kind() {
        ItemKind::DefFunction(def) => def
            .body
            .stmts
            .iter()
            .map(|stmt| format!("{:?}", stmt))
            .collect(),
        other => panic!("expected function, got {:?}", other),
    }
}

/// Two `let`s chained with `&&` in an `if` condition — the baseline case
/// that already worked before this test was added.
#[test]
fn parses_two_let_chain() {
    let item = parse_single_item(
        r#"
        fn f(a: Option<i32>) {
            if let Some(x) = &a
                && let Some(y) = Some(1)
            {
                let _ = (x, y);
            }
        }
        "#,
    );
    assert!(!fn_body_kinds(&item).is_empty());
}

/// Three-plus `let`s chained with `&&`, ending in a plain boolean condition
/// referencing a chain-bound name (`z > y`) right before the block. This is
/// the regression case: `parse_let_expr` used to always parse its own
/// scrutinee with the struct-permitting expression parser even when reached
/// from a no-struct (`if`/`while` condition) context, so the innermost
/// nested `let`'s scrutinee parse could run all the way up to the trailing
/// bare identifier (`y`) and misparse `y {` as a struct literal instead of
/// stopping for the condition's own block — see
/// `crate::ast::expr::parse_let_expr_no_struct`'s doc comment.
#[test]
fn parses_three_let_chain_with_trailing_condition() {
    let item = parse_single_item(
        r#"
        fn f(a: Option<i32>) {
            if let Some(x) = &a
                && let Some(y) = Some(1)
                && let Some(z) = Some(2)
                && z > y
            {
                let _ = (x, y, z);
            }
        }
        "#,
    );
    let stmts = fn_body_kinds(&item);
    assert_eq!(stmts.len(), 1, "expected a single if-let statement");
}

/// The exact shape that originally surfaced this bug: a multi-condition
/// `&&` let-chain nested inside a `tokio::spawn(async move { .. })` block
/// inside a `loop`, ported from SakuraLens's own
/// `crates/skln-server/src/watchers.rs`.
#[test]
fn parses_nested_let_chain_inside_async_loop() {
    let item = parse_single_item(
        r#"
        fn start() {
            let refs_mtime_path: Option<i32> = None;
            tokio::spawn(async move {
                let mut last_refs_mtime = 0;
                loop {
                    if let Some(logs_path) = &refs_mtime_path
                        && let Some(meta) = Some(1)
                        && let Some(mtime) = Some(1)
                        && mtime > last_refs_mtime
                    {
                        last_refs_mtime = mtime;
                    }
                }
            });
        }
        "#,
    );
    match item.kind() {
        ItemKind::DefFunction(def) => {
            assert!(!def.body.stmts.is_empty());
        }
        other => panic!("expected function, got {:?}", other),
    }
}

/// A `let`-chain whose *first* condition is a plain expression (`x > 0`,
/// no leading `let`) still parses — regression guard that
/// `parse_if_expr`'s non-`let` branch (`parse_expr_winnow`) still handles a
/// `let` appearing later in the chain via `parse_primary`'s own
/// (struct-permitting) `parse_let_expr`, independent of the no-struct fix
/// above.
#[test]
fn parses_plain_condition_then_let() {
    let item = parse_single_item(
        r#"
        fn f(x: i32, a: Option<i32>) {
            if x > 0 && let Some(y) = a {
                let _ = y;
            }
        }
        "#,
    );
    assert!(matches!(item.kind(), ItemKind::DefFunction(_)));
}

#[test]
fn if_let_chain_produces_single_statement() {
    let item = parse_single_item(
        r#"
        fn f(a: Option<i32>) {
            if let Some(x) = &a && let Some(y) = Some(1) && y > 0 {
                let _ = (x, y);
            }
        }
        "#,
    );
    match item.kind() {
        ItemKind::DefFunction(def) => assert_eq!(def.body.stmts.len(), 1),
        other => panic!("expected function, got {:?}", other),
    }
}
