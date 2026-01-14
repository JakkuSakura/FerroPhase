//! Integration tests for the FerroPhase CLI

use assert_cmd::Command;
use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, compile_command};
use predicates::prelude::*;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

#[test]
fn test_cli_help() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("FerroPhase"));
}

#[test]
fn test_cli_version() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("--version");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains(env!("CARGO_PKG_VERSION")));
}

#[test]
fn test_cli_parse_help() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("parse").arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse and display AST"));
}

#[test]
fn test_cli_completions_basic() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("completions").arg("bash");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("fp"));
}

#[test]
fn test_cli_eval_simple() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("eval").arg("--expr").arg("1 + 2");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Result: 3"));
}

#[test]
fn test_cli_compile_missing_file() {
    let output = Command::cargo_bin("fp")
        .unwrap()
        .arg("compile")
        .arg("nonexistent.fp")
        .output()
        .expect("fp should run");

    assert!(
        !output.status.success(),
        "expected failure for missing file"
    );
    let out_all = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        out_all.contains("does not exist"),
        "expected 'does not exist' in either stdout or stderr, got: {}",
        out_all
    );
}

#[test]
fn test_cli_invalid_command() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("invalid_command");

    cmd.assert().failure();
}

async fn compile_example_async(example_name: &str) {
    let temp_dir = TempDir::new().unwrap();
    let example_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples")
        .canonicalize()
        .expect("examples directory should resolve");
    let source_path = temp_dir.path().join(example_name);
    fs::copy(example_root.join(example_name), &source_path).unwrap();

    let output_path = temp_dir.path().join(example_name.replace(".fp", ".out"));

    let args = CompileArgs {
        input: vec![source_path.clone()],
        backend: "binary".to_string(),
        emitter: "native".to_string(),
        target_triple: None,
        target_cpu: None,
        target_features: None,
        target_sysroot: None,
        linker: "native".to_string(),
        target_linker: None,
        output: Some(output_path.clone()),
        package_graph: None,
        opt_level: 0,
        debug: true,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        save_intermediates: false,
        error_tolerance: false,
        max_errors: 0,
        source_language: None,
        disable_stage: Vec::new(),
    };

    if let Err(err) = compile_command(args, &CliConfig::default()).await {
        panic!("example {} failed: {:?}", example_name, err);
    }

    assert!(
        output_path.exists(),
        "{} should produce an output binary",
        example_name
    );
}

#[test]
fn test_compile_example_comptime_collections() {
    let example = "18_comptime_collections.fp".to_string();
    let handle = std::thread::Builder::new()
        .name("compile-example-18".to_string())
        .stack_size(16 * 1024 * 1024)
        .spawn(move || {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("runtime build");
            runtime.block_on(async move {
                compile_example_async(&example).await;
            });
        })
        .expect("thread spawn");
    handle.join().expect("thread join");
}
