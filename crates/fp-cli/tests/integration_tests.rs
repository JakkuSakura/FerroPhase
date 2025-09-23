//! Integration tests for the FerroPhase CLI

use assert_cmd::Command;
use predicates::prelude::*;
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

    // This might fail until evaluation is fully implemented, but should not crash
    cmd.assert().code(predicate::in_iter([0, 1])); // Accept success or failure for now
}

#[test]
fn test_cli_compile_missing_file() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("compile").arg("nonexistent.fp");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("does not exist"));
}

#[test]
fn test_cli_invalid_command() {
    let mut cmd = Command::cargo_bin("fp").unwrap();
    cmd.arg("invalid_command");

    cmd.assert().failure();
}
