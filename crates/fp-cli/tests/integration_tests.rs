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
fn test_cli_info() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("info");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("FerroPhase"));
}

#[test]
fn test_cli_init_basic() {
    let temp_dir = TempDir::new().unwrap();
    let project_name = "test_project";

    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("init")
        .arg(project_name)
        .arg("--output")
        .arg(temp_dir.path().join(project_name))
        .arg("--template")
        .arg("basic");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Successfully created"));

    // Check that project files were created
    assert!(
        temp_dir
            .path()
            .join(project_name)
            .join("Ferrophase.toml")
            .exists()
    );
    assert!(
        temp_dir
            .path()
            .join(project_name)
            .join("src")
            .join("main.fp")
            .exists()
    );
    assert!(
        temp_dir
            .path()
            .join(project_name)
            .join("README.md")
            .exists()
    );
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

async fn compile_example(example_name: &str) {
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
        target: "binary".to_string(),
        output: Some(output_path.clone()),
        opt_level: 0,
        debug: true,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        error_tolerance: false,
        max_errors: 0,
        save_intermediates: false,
        source_language: None,
        release: false,
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

#[tokio::test]
async fn test_compile_example_comptime_collections() {
    compile_example("18_comptime_collections.fp").await;
}
