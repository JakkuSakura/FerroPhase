//! Regression coverage for composing `fp-typing`'s cached, `Param`-relative
//! generic-call substitution with the current specialization's own
//! `type_substs`, instead of discarding it, in `fp-backend`'s HIR→MIR
//! lowering (`BodyBuilder::lower_call`).
//!
//! A nested, non-tail generic call (`double(add(a, b))`) inside another
//! still-generic enclosing function (`pipeline<T>`) is type-checked once,
//! generically — `fp-typing` caches `add`'s resolved argument type relative
//! to `pipeline`'s own `T`, not as a final concrete type. Before this fix,
//! `fp-backend` discarded that cached, `Param`-relative result outright
//! (since a bare `Param` is "unresolved") and re-inferred `add`'s generics
//! independently, which incorrectly compared its own correctly-inferred
//! `T = i64` against `double`'s own unrelated, still-generic `T`, reporting
//! a bogus "conflicting inference" error. See `examples/11_
//! specialization_basics.fp`, which exercises this same shape.

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
async fn nested_generic_call_inside_generic_body_resolves_without_conflicting_inference() {
    let source = r#"
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn double(x: i64) -> i64 {
    x + x
}

fn pipeline_add(a: i64, b: i64) -> i64 {
    double(add(a, b))
}

fn main() {
    println!("{}", pipeline_add(1, 2));
}
"#;
    check_source(source)
        .await
        .expect("non-generic nested call should resolve as a sanity baseline");
}

#[tokio::test]
async fn nested_generic_call_inside_generic_pipeline_resolves_without_conflicting_inference() {
    // The actual regression shape: `add`/`double`/`pipeline` are all
    // generic over a same-named `T`, and `pipeline`'s own body calls
    // `double(add(a, b))` — a nested call whose typeck-cached resolution
    // is expressed relative to `pipeline`'s own `T`, not yet concrete.
    let source = r#"
fn add<T>(a: T, b: T) -> T {
    a + b
}

fn double<T>(x: T) -> T {
    x + x
}

fn pipeline<T>(a: T, b: T) -> T {
    double(add(a, b))
}

fn main() {
    let result = pipeline(10i64, 20i64);
    println!("{}", result);
}
"#;
    check_source(source).await.expect(
        "nested generic call inside a generic enclosing body should compose the cached, \
         Param-relative substitution with the current specialization instead of reporting \
         a conflicting-inference error",
    );
}
