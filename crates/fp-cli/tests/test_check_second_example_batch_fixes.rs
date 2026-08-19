//! Regression coverage for the second batch of `examples/*.fp`-failure
//! root-cause fixes made this session:
//!
//! 1. The item-scoped comptime probe (and `lower_program`) seeded their
//!    synthetic-`DefId` counter from `hir_program.items` (top-level items
//!    only), which can collide with a *local* `const` binding's own real
//!    `DefId` (only recorded in `def_map`) — for a single-function file,
//!    the seed lands exactly one past the enclosing function's own id, the
//!    same id a local `const` right after it gets. See `examples/
//!    01_const_eval_basics.fp`.
//! 2. `compile_warning!`/`compile_error!` had no lowering support at all
//!    outside constant-folding (`lower_intrinsic_constant` doesn't handle
//!    them, since they're diagnostics, not values) — any use as a bare
//!    statement (not bound to a variable) hit "unsupported intrinsic ...
//!    during MIR operand lowering". Added real end-to-end support via a
//!    new `lir::ComptimeOp::CompileWarning`/`CompileError`.
//! 3. `type(X)` (bare call syntax to the reserved `type` keyword) parsed to
//!    an unresolved ordinary `Call` — the old path-string-based recognition
//!    (`fp-lang`'s `resolve_lang_intrinsic`) never runs for `Invoke`
//!    expressions anymore (`needs_normalization` excludes them, by
//!    design). Recognized directly in `ast_to_hir::transform_invoke_to_hir`
//!    instead, since `type` being a reserved keyword means no real
//!    declaration could ever exist for it to resolve to (unlike ordinary
//!    identifiers, where name-based recognition risks shadowing a real,
//!    same-named user function).
//! 4. `impl Fn(..) -> ..` return-type annotations lowered through a
//!    placeholder path (`Res::Builtin(BuiltinSelfType::Function)`,
//!    discarding the param/return types) instead of a real `FnPtr` HIR
//!    type, starving the closure-hint machinery of any real signature.

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
async fn local_const_block_does_not_collide_with_synthetic_defid_counter() {
    // A single-function file, matching the exact trigger condition: with
    // only one top-level item (`main`), seeding the synthetic-DefId
    // counter from `.items.max()` lands exactly on `BUFFER_SIZE`'s own
    // real DefId, aliasing the const block's synthetic entry onto it.
    let source = r#"
fn main() {
    const BUFFER_SIZE: i64 = 1024;
    let doubled = const { BUFFER_SIZE * 2 };
    println!("{}", doubled);
}
"#;
    check_source(source)
        .await
        .expect("a local const referenced from a const block should not collide with the comptime probe's own synthetic DefId");
}

#[tokio::test]
async fn compile_warning_as_a_bare_statement_lowers_successfully() {
    let source = r#"
fn main() {
    compile_warning!("just a warning");
    println!("done");
}
"#;
    check_source(source)
        .await
        .expect("compile_warning! used as a bare statement should lower, not report 'unsupported intrinsic'");
}

#[tokio::test]
async fn bare_type_call_resolves_as_a_reflection_query() {
    // `type(Point)` (no `!`) used to parse to an unresolved ordinary call
    // to a path named "type", making every `.field`/`.method` access on it
    // report "field access ... requires a struct, found Error".
    let source = r#"
fn main() {
    struct Point {
        x: i64,
        y: i64,
    }
    let n = type(Point).fields.len();
    println!("{}", n);
}
"#;
    check_source(source)
        .await
        .expect("bare `type(X)` should resolve as a reflection query and fully typecheck");
}

#[tokio::test]
async fn impl_fn_return_type_builds_a_real_signature() {
    // Before the fix, `impl Fn(i64) -> i64` lowered to a placeholder path
    // discarding the param/return types, producing "unresolved type path
    // `fn(..)`"/"unresolved type path `_`" downstream.
    let source = r#"
fn make_adder(n: i64) -> impl Fn(i64) -> i64 {
    move |x: i64| x + n
}

fn main() {
    let add_5 = make_adder(5);
    let _ = add_5;
}
"#;
    let result = check_source(source).await;
    if let Err(err) = &result {
        let message = err.to_string();
        assert!(
            !message.contains("unresolved type path"),
            "impl Fn(..) -> .. should build a real FnPtr HIR type, not a placeholder path — got: {message}"
        );
    }
}
