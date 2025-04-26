use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use log::debug;
use std::path::PathBuf;

mod config;

use magnet::commands;

/// CLI entry point
fn main() -> Result<()> {
    let cli = Cli::parse();

    // Setup logging based on verbosity
    let log_level = match cli.verbose {
        0 => log::LevelFilter::Info,
        1 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };

    env_logger::Builder::new()
        .filter_level(log_level)
        .format_timestamp(None)
        .init();

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
        Some(Commands::List { config }) => commands::list(&config),
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
        #[arg(default_value = "Magnet.toml")]
        config: PathBuf,
    },
    /// Check Magnet.toml for issues
    Check {
        /// Path to the Magnet.toml file
        #[arg(default_value = "Magnet.toml")]
        config: PathBuf,
    },
    /// List all crates in the workspace
    List {
        /// Path to the Magnet.toml file
        #[arg(default_value = "Magnet.toml")]
        config: PathBuf,
    },
}
