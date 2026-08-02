//! Code checking and validation command implementation

use crate::{Result, cli::CliConfig, compiler, utils::FileUtils};
use clap::Args;
use console::style;
use fp_core::config;
use std::path::PathBuf;

/// Arguments for the check command
#[derive(Debug, Clone, Args)]
pub struct CheckArgs {
    /// Files or directories to check
    #[arg(default_value = ".")]
    pub paths: Vec<PathBuf>,
    /// Package name used to qualify source identities
    #[arg(long = "package", required = true)]
    pub package: String,
    /// Include patterns (glob)
    #[arg(long)]
    pub include: Vec<String>,
    /// Exclude patterns (glob)
    #[arg(long)]
    pub exclude: Vec<String>,
    /// Check only syntax, skip semantic analysis
    #[arg(long)]
    pub syntax_only: bool,
}

/// Execute the check command
pub async fn check_command(args: CheckArgs, _config: &CliConfig) -> Result<()> {
    println!("{} Checking FerroPhase code...", style("🔍").cyan());

    let files = collect_check_files(&args)?;
    for file in &files {
        compiler::check_path(
            file,
            &args.package,
            args.syntax_only,
            None,
            compiler::LossyCompileOptions {
                enabled: config::lossy_mode(),
            },
        )?;
    }

    println!("{} Checked {} file(s)", style("✓").green(), files.len());
    Ok(())
}

fn collect_check_files(args: &CheckArgs) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for path in &args.paths {
        if path.is_file() {
            files.push(path.clone());
            continue;
        }

        if path.is_dir() {
            files.extend(FileUtils::find_files(path, &args.include, &args.exclude)?);
            continue;
        }

        return Err(crate::CliError::InvalidInput(format!(
            "check: path does not exist: {}",
            path.display()
        )));
    }

    if files.is_empty() {
        return Err(crate::CliError::InvalidInput(
            "check: no files matched the provided paths".to_string(),
        ));
    }

    files.sort();
    files.dedup();
    Ok(files)
}
