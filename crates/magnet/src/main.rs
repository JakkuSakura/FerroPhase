use clap::{Parser, Subcommand};
use common::{LogLevel, setup_logs};
use eyre::{Context, Result};
use std::path::PathBuf;
use tracing::debug;

mod configs;

use magnet::commands;

/// CLI entry point
fn main() -> Result<()> {
    let cli = Cli::parse();

    // Setup logging based on verbosity
    let log_level = match cli.verbose {
        0 => LogLevel::Info,
        1 => LogLevel::Debug,
        _ => LogLevel::Trace,
    };
    setup_logs(log_level)?;
    // Change working directory if specified
    if let Some(dir) = cli.working_dir {
        std::env::set_current_dir(&dir)
            .context(format!("Failed to change directory to {}", dir.display()))?;
        debug!("Changed working directory to: {}", dir.display());
    }

    // Execute the appropriate subcommand
    match cli.command {
        Some(Commands::Init { path }) => commands::init(&path),
        Some(Commands::Generate { config }) => commands::generate(&config),
        Some(Commands::Check { config }) => commands::check(&config),
        Some(Commands::Tree { config }) => commands::tree(&config),
        None => {
            println!("No command specified. Run with --help for usage information.");
            Ok(())
        }
    }
}

#[derive(Parser)]
#[command(name = "magnet")]
#[command(author, version, about = "Manage Magnet.toml project configuration files", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Sets the current working directory
    #[arg(short, long, value_name = "DIRECTORY")]
    working_dir: Option<PathBuf>,

    /// Increases the level of verbosity
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new Magnet.toml file
    Init {
        /// Path to initialize the Magnet.toml file
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Generate or update Cargo.toml files from Magnet.toml
    Generate {
        /// Path to the Magnet.toml file
        #[arg(default_value = ".")]
        config: PathBuf,
    },
    /// Check Magnet.toml for issues
    Check {
        /// Path to the Magnet.toml file
        #[arg(default_value = ".")]
        config: PathBuf,
    },
    /// Display workspace hierarchy as a tree
    Tree {
        /// Path to the Magnet.toml file
        #[arg(default_value = ".")]
        config: PathBuf,
    },
}
