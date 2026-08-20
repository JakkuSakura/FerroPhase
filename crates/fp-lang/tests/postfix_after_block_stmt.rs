use fp_core::ast::{BlockStmt, Item, ItemKind};
use fp_lang::ast::FerroPhaseParser;

fn parse_single_item(src: &str) -> Item {
    let parser = FerroPhaseParser::new();
    let mut items = parser.parse_items_ast(src).expect("parse succeeds");
    assert_eq!(items.len(), 1, "expected single item");
    items.pop().unwrap()
}

fn fn_stmts(item: &Item) -> Vec<BlockStmt> {
    match item.kind() {
        ItemKind::DefFunction(def) => def.body.stmts.clone(),
        other => panic!("expected function, got {:?}", other),
    }
}

/// `unsafe { .. }.method()` as a block's tail expression — the exact idiom
/// from SakuraLens's `crates/skln-ffi/src/lib.rs` (`unsafe { CStr::from_ptr
/// (ptr) }.to_str().map_err(..)`) that originally surfaced this bug: a
/// statement starting with a block-like expression (`unsafe { }`, `if`,
/// `match`, ...) used to never continue past the block's closing `}`, so a
/// following `.method()` chain failed to parse. Real Rust only needs to
/// reject a bare `(`/`[` immediately after (ambiguous with a *new*
/// statement) — `.`/`?` are never ambiguous there.
#[test]
fn parses_method_chain_after_unsafe_block_tail_expr() {
    let item = parse_single_item(
        r#"
        fn f(ptr: *const i8) -> i32 {
            unsafe { g(ptr) }
                .h()
                .i()
        }
        "#,
    );
    assert_eq!(fn_stmts(&item).len(), 1);
}

/// Same idiom, but with a `?` right after the block instead of `.method()`.
#[test]
fn parses_try_after_if_block_tail_expr() {
    let item = parse_single_item(
        r#"
        fn f(c: bool) -> Result<i32, i32> {
            if c { Ok(1) } else { Err(2) }?;
            Ok(0)
        }
        "#,
    );
    assert_eq!(fn_stmts(&item).len(), 2);
}

/// Regression guard for the ambiguity this rule exists to avoid in the
/// first place: a block-like statement immediately followed by `(a, b)` (no
/// leading `.`/`?`) must still parse as *two* separate statements, not one
/// `Invoke` that misreads `(a, b)` as call arguments to the `if`.
#[test]
fn does_not_swallow_following_tuple_as_call_args() {
    let item = parse_single_item(
        r#"
        fn f(c: bool, a: i32, b: i32) {
            if c {
                g(a);
            }
            (a, b);
        }
        "#,
    );
    assert_eq!(fn_stmts(&item).len(), 2);
}
