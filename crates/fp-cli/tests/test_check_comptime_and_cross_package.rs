//! Regression coverage for two architectural fixes made to the compiler
//! pipeline this session:
//!
//! 1. Comptime (`const { .. }`) block resolution is item-scoped — resolving
//!    one block's value must not require the *entire* package (every other
//!    function/const) to already be fully typed. Before the fix, a package
//!    with a `const { .. }` block plus *any* other, unrelated, still-typing
//!    code failed with a generic "did not converge" error, because the
//!    mid-typecheck probe re-lowered the whole package instead of just the
//!    one block and its own dependencies.
//! 2. Cross-package `Type::method()` calls (methods defined in a
//!    dependency's `impl` block, e.g. `std`'s `String`/`Vec`) resolve
//!    outside a `const` context, for both non-generic (`String::new`) and
//!    generic (`Vec::from`) methods — previously these only ever resolved
//!    when evaluated at comptime; a real (non-const) call site had no
//!    lazy/uniform resolution path and failed with "unresolved call
//!    target".

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::check::{CheckArgs, check_command};

fn check_args(input: std::path::PathBuf) -> CheckArgs {
    CheckArgs {
        paths: vec![input],
        package: "test".to_string(),
        include: Vec::new(),
        exclude: Vec::new(),
        syntax_only: false,
    }
}

async fn check_source(source: &str) -> fp_cli::Result<()> {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    fs::write(&input_file, source).unwrap();
    check_command(check_args(input_file), &CliConfig::default()).await
}

#[tokio::test]
async fn const_block_resolves_without_requiring_the_whole_package_to_be_typed() {
    // `run_something_unrelated` is declared *after* `main` and does its own,
    // independent work — it has nothing to do with the const block's value,
    // but it's still part of the same package's typecheck pass. Before the
    // item-scoped comptime fix, resolving `main`'s `const { .. }` block
    // required this whole package (including this unrelated function's own
    // body) to already be fully typed, purely as an incidental side effect
    // of the mid-typecheck probe re-lowering everything instead of just the
    // one block and the consts it actually references.
    let source = r#"
const BUFFER_SIZE: i64 = 1024;
const SCALE: i64 = 2;

fn main() {
    let doubled = const { BUFFER_SIZE * SCALE };
    println!("{}", doubled);
    run_something_unrelated();
}

fn run_something_unrelated() -> i64 {
    let mut total = 0;
    let mut i = 0;
    while i < 5 {
        total = total + i;
        i = i + 1;
    }
    total
}
"#;
    check_source(source)
        .await
        .expect("const block referencing only same-block consts should resolve regardless of unrelated code elsewhere in the package");
}

#[tokio::test]
async fn string_new_resolves_outside_const_context() {
    // `String::new` is a non-generic method on `std`'s own `impl String`.
    // Calling it from ordinary (non-const) runtime code, in a different
    // package than the one that declares it, must resolve the same way a
    // same-package method call would.
    let source = r#"
fn main() {
    let s: String = String::new();
    println!("{}", s.len());
}
"#;
    check_source(source)
        .await
        .expect("String::new() should resolve outside a const context");
}

#[tokio::test]
async fn vec_from_call_target_resolves_outside_const_context() {
    // `Vec::from` is a *generic* method (`impl<T> Vec<T> { fn from(..) }`)
    // in a dependency package — exercises the generic-method counterpart
    // (`ensure_generic_method_def`) of the same cross-package resolution
    // fix `String::new` exercises for the non-generic case.
    //
    // This does NOT yet assert full success end-to-end: a separate,
    // already-catalogued gap (array-literal-argument type inference for a
    // generic call's own local-variable initializer — the same "missing
    // HIR type for local initializer" also seen on `examples/23_runtime_
    // collections.fp`) still fails this specific snippet downstream of
    // call-target resolution. What this test pins down is narrower and
    // already true: the call target itself resolves — the error is no
    // longer "unresolved call target `std::alloc::Vec::from`", which is
    // what cross-package generic method resolution actually fixed.
    let source = r#"
fn main() {
    let v: Vec<i64> = Vec::from([1, 2, 3]);
    println!("{}", v.len());
}
"#;
    let result = check_source(source).await;
    if let Err(err) = &result {
        let message = err.to_string();
        assert!(
            !message.contains("unresolved call target"),
            "Vec::from's call target should resolve outside a const context \
             (cross-package generic method resolution) — got: {message}"
        );
    }
}
