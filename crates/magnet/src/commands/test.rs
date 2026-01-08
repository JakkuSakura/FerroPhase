//! Command implementation for running package tests

use crate::commands::run::resolve_package;
use crate::utils::find_furthest_manifest;
use eyre::{Context, Result, bail};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub struct TestOptions {
    pub path: PathBuf,
    pub package: Option<String>,
    pub release: bool,
    pub profile: Option<String>,
}

pub fn test(options: &TestOptions) -> Result<()> {
    let start_dir = resolve_start_dir(&options.path)?;
    let (_root, manifest) = find_furthest_manifest(&start_dir)?;
    let package = resolve_package(&start_dir, &manifest, options.package.as_deref())?;
    let cargo_path = package.root_path.join("Cargo.toml");
    if !cargo_path.exists() {
        bail!(
            "Cargo.toml not found for package {}; magnet test currently supports Rust packages only",
            package.name
        );
    }

    let mut command = Command::new("cargo");
    command.arg("test").arg("--manifest-path").arg(&cargo_path);
    if let Some(profile) = options.profile.as_ref() {
        command.arg("--profile").arg(profile);
    } else if options.release {
        command.arg("--release");
    }
    command.current_dir(&package.root_path);
    command.stdin(Stdio::inherit());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.status().with_context(|| {
        format!(
            "Failed to execute cargo test for {}",
            package.root_path.display()
        )
    })?;
    if !status.success() {
        bail!("cargo test failed with status {}", status.code().unwrap_or(-1));
    }

    Ok(())
}

fn resolve_start_dir(path: &Path) -> Result<PathBuf> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if path.is_file() {
        Ok(path.parent().unwrap_or(Path::new(".")).to_path_buf())
    } else {
        Ok(path)
    }
}
