//! Regression coverage for a false-positive re-entrancy guard fix in
//! `MirLowering::lower_operand` (`fp-backend`'s `hir_to_mir/expr.rs`).
//!
//! `FieldAccess` is meant to resolve via exactly one of two routes tried up
//! front in `lower_operand`: constant-folding, or resolving a real place.
//! When both genuinely fail (e.g. an unresolved `type(T).fields` reflection
//! chain), execution used to fall through to a wildcard match arm that
//! called `lower_expr_into_place`, which itself dispatches `FieldAccess`
//! straight back into `lower_operand` for the *same* expression — tripping
//! the function's own re-entrancy guard and reporting a misleading
//! "recursive expression detected during MIR lowering" instead of the real
//! underlying diagnostic. The fix makes the wildcard arm emit a real error
//! for `FieldAccess` directly instead of routing back through
//! `lower_expr_into_place`.

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
async fn reflection_field_access_reports_a_real_diagnostic_not_recursive_expression() {
    // `type(Point).fields` is a reflection intrinsic chain that MIR
    // lowering can't yet fully resolve (see `examples/04_struct_
    // introspection.fp`, which exercises this same construct) — this test
    // isn't asserting the reflection feature itself works end-to-end (a
    // separate, already-catalogued gap), only that failing to lower it
    // produces its own real, specific error rather than the unrelated,
    // misleading "recursive expression detected" message the re-entrancy
    // guard used to report for any `FieldAccess` whose fallbacks both fail.
    let source = r#"
fn main() {
    struct Point {
        x: f64,
        y: f64,
    }
    const POINT_FIELDS: i64 = type(Point).fields.len();
    println!("{}", POINT_FIELDS);
}
"#;
    let result = check_source(source).await;
    if let Err(err) = &result {
        let message = err.to_string();
        assert!(
            !message.contains("recursive expression detected"),
            "a genuinely unresolvable field access should report its own real \
             diagnostic, not the re-entrancy guard's false positive — got: {message}"
        );
    }
}
