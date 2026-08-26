//! Integration tests for the FerroPhase CLI

use assert_cmd::Command;
use predicates::prelude::*;

fn fp_cmd() -> Command {
    Command::new(env!("CARGO_BIN_EXE_fp"))
}

#[test]
fn test_cli_help() {
    let mut cmd = fp_cmd();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("FerroPhase"));
}

#[test]
fn test_cli_version() {
    let mut cmd = fp_cmd();
    cmd.arg("--version");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains(env!("CARGO_PKG_VERSION")));
}

#[test]
fn test_cli_completions_basic() {
    let mut cmd = fp_cmd();
    cmd.arg("completions").arg("bash");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("fp"));
}

#[test]
fn test_cli_compile_missing_file() {
    let output = fp_cmd()
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
        out_all.contains("does not exist") || out_all.contains("no package provider"),
        "expected a missing-input diagnostic in either stdout or stderr, got: {}",
        out_all
    );
}

#[test]
fn test_cli_invalid_command() {
    let mut cmd = fp_cmd();
    cmd.arg("invalid_command");

    cmd.assert().failure();
}
