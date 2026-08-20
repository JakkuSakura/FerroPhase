//! Regression coverage for item-position `macro_rules!` expansion.
//!
//! Before this fix, a top-level macro invocation whose transcriber is
//! itself an item (e.g. `make_adder!(add_two, 2);` expanding to `fn
//! add_two(x: i64) -> i64 { x + 2 }`) was never expanded at all —
//! `ast_to_hir`'s own `ItemKind::Macro` handling only ever warned and
//! dropped it, so `add_two` never existed as a real function, and any
//! ordinary code calling it failed with "missing HIR type for local
//! initializer" (the call's own, real, hand-written local — not a
//! synthesized one, as originally guessed). Fixed by expanding
//! item-position invocations into real `Item`s (reusing the same
//! `match_macro_rule`/`substitute_template` primitives the already-working
//! expression-position path uses) before HIR generation ever runs
//! (`fp_lang::expand_item_macros`, called from `fp-compiler`'s driver).

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::check::{CheckArgs, check_command};

async fn check_source(source: &str) -> fp_cli::Result<()> {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    fs::write(&input_file, source).unwrap();
    let args = CheckArgs {
        paths: vec![input_file],
        package: "test".to_string(),
        include: Vec::new(),
        exclude: Vec::new(),
        syntax_only: false,
    };
    check_command(args, &CliConfig::default()).await
}

#[tokio::test]
async fn item_position_macro_invocation_expands_into_a_real_callable_function() {
    let source = r#"
macro_rules! make_adder {
    ($name:ident, $value:tt) => {
        fn $name(x: i64) -> i64 {
            x + $value
        }
    };
}

make_adder!(add_two, 2);

fn main() {
    let v = add_two(5);
    println!("{}", v);
}
"#;
    check_source(source).await.expect(
        "a top-level macro_rules! invocation should expand into a real, callable item, \
         not be silently dropped",
    );
}
