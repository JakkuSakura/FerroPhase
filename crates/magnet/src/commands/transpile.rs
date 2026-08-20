//! Transpile — thin wrapper that first runs `magnet lock` then delegates to `fp compile --target`

use crate::commands::{LockOptions, lock};
use crate::utils::find_furthest_manifest;
use eyre::{Result, Context};
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::info;

pub struct TranspileOptions {
    pub path: PathBuf,
    pub target: String,
    pub output: Option<PathBuf>,
    pub package: Option<String>,
}

pub fn transpile(options: &TranspileOptions) -> Result<()> {
    let start_dir = if options.path.is_file() {
        options.path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        options.path.clone()
    };
    let start_dir = start_dir.canonicalize().unwrap_or(start_dir);

    let (root, _) = find_furthest_manifest(&start_dir)?;
    info!("transpile: manifest root {}", root.display());

    // Sync dependencies first
    let lock_opts = LockOptions {
        path: root.clone(),
        offline: false,
        cache_dir: None,
        fetch: true,
    };
    if let Err(e) = lock(&lock_opts) {
        info!("magnet lock skipped (deps may already be available): {e}");
    }

    let fp_bin = find_fp()?;

    let mut args: Vec<String> = vec![
        "compile".into(),
        start_dir.display().to_string(),
        "--target".into(),
        options.target.clone(),
    ];
    if let Some(out) = &options.output {
        args.push("-o".into());
        args.push(out.display().to_string());
    }
    if let Some(pkg) = &options.package {
        args.push("--package".into());
        args.push(pkg.clone());
    }

    info!("transpile: fp {}", args.join(" "));
    let status = Command::new(&fp_bin)
        .args(&args)
        .current_dir(&root)
        .status()
        .context("Failed to execute fp")?;

    if !status.success() {
        eyre::bail!("fp compile failed with status {}", status.code().unwrap_or(-1));
    }

    info!("transpile: done");
    Ok(())
}

fn find_fp() -> Result<PathBuf> {
    let cargo_target = std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".into());
    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };
    let bin = PathBuf::from(&cargo_target).join(profile).join("fp");
    if bin.exists() { return Ok(bin.canonicalize().unwrap_or(bin)); }
    eyre::bail!("Cannot find fp binary. Build: cargo build -p fp-cli")
}
