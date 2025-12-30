//! Run command implementation - delegates to magnet

use crate::{CliError, Result, cli::CliConfig};
use clap::{Args, ValueEnum};
use std::path::{Path, PathBuf};
use std::process::Stdio;
use tokio::process::Command;
use tracing::info;

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum RunMode {
    Compile,
    Interpret,
}

/// Arguments for the run command
#[derive(Debug, Clone, Args)]
pub struct RunArgs {
    /// FerroPhase file to run
    pub file: PathBuf,
    /// Execution mode (compile or interpret)
    #[arg(long, default_value = "compile")]
    pub mode: RunMode,
}

pub async fn run_command(args: RunArgs, _config: &CliConfig) -> Result<()> {
    info!("Delegating run to magnet for file '{}'", args.file.display());

    crate::commands::validate_paths_exist(&[args.file.clone()], true, "run")?;

    let magnet_bin = resolve_magnet_binary()?;
    let mut command = Command::new(&magnet_bin);
    command.arg("run").arg(&args.file);
    if matches!(args.mode, RunMode::Interpret) {
        command.arg("--mode").arg("interpret");
    }

    let status = command
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .await
        .map_err(|error| {
            CliError::MissingDependency(format!(
                "Failed to execute magnet at '{}': {}",
                magnet_bin.display(),
                error
            ))
        })?;

    if !status.success() {
        return Err(CliError::Compilation(format!(
            "magnet run failed with status {}",
            status.code().unwrap_or(-1)
        )));
    }

    Ok(())
}

fn resolve_magnet_binary() -> Result<PathBuf> {
    if let Some(path) = std::env::var_os("MAGNET_BIN") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Ok(path);
        }
    }

    if let Some(path) = locate_workspace_magnet() {
        return Ok(path);
    }

    if let Some(path) = find_in_path("magnet") {
        return Ok(path);
    }

    Err(CliError::MissingDependency(
        "magnet binary not found; set MAGNET_BIN or add it to PATH".to_string(),
    ))
}

fn locate_workspace_magnet() -> Option<PathBuf> {
    let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR")?;
    let root = Path::new(&manifest_dir).parent()?.parent()?;
    let candidates = [
        root.join("target").join("debug").join(binary_name()),
        root.join("target").join("release").join(binary_name()),
    ];

    candidates.into_iter().find(|path| path.exists())
}

fn find_in_path(binary: &str) -> Option<PathBuf> {
    let binary_name = if cfg!(windows) {
        format!("{}.exe", binary)
    } else {
        binary.to_string()
    };

    let paths = std::env::var_os("PATH")?;
    std::env::split_paths(&paths)
        .map(|path| path.join(&binary_name))
        .find(|path| path.exists())
}

fn binary_name() -> &'static str {
    if cfg!(windows) {
        "magnet.exe"
    } else {
        "magnet"
    }
}
