//! FerroPhase CLI Binary
//!
//! The main command-line interface for FerroPhase, a meta-compilation framework
//! that enables multi-language development with advanced compile-time capabilities.
//!
//! # Usage
//!
//! ```bash
//! # Compile a FerroPhase file to Rust
//! fp compile hello.fp --target rust --output hello.rs
//!
//! # Run const evaluation with interpretation
//! fp eval --expr "1 + 2 * 3"
//!
//! # Initialize a new FerroPhase project
//! fp init my-project --template basic
//!
//! # Check and validate FerroPhase code
//! fp check src/
//!
//! # Start an interactive REPL
//! fp repl
//! ```

use clap::{Parser, Subcommand};
use console::style;
use fp_cli::{
    Result,
    cli::CliConfig,
    commands::{
        self, check::CheckArgs, compile::CompileArgs, completions::CompletionsArgs, eval::EvalArgs,
        info::InfoArgs, init::InitArgs, parse::ParseArgs, run::RunArgs, transpile::TranspileArgs,
    },
    diagnostics::setup_error_reporting,
};
use std::path::PathBuf;
use tracing::info;

#[derive(Parser)]
#[command(
    name = "fp",
    version = env!("CARGO_PKG_VERSION"),
    about = "FerroPhase: Meta-compilation framework with multi-language comptime superpowers",
    long_about = r#"
FerroPhase is a unified compilation infrastructure that extends Rust's capabilities 
while supporting multi-language interoperability and advanced compile-time computation.

EXAMPLES:
    fp run hello.fp                       # Run a FerroPhase file
    fp eval --expr "1 + 2 * 3"           # Evaluate expression
    fp compile hello.fp --target rust    # Compile to Rust
    fp init my-project                    # Create new project
    "#
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose logging (use multiple times for increased verbosity)
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Suppress non-error output
    #[arg(short, long, global = true)]
    quiet: bool,

    /// Configuration file path
    #[arg(short, long, global = true)]
    config: Option<PathBuf>,

    /// Working directory
    #[arg(short = 'C', long, global = true)]
    directory: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile FerroPhase code to various targets
    Compile(CompileArgs),

    /// Transpile FerroPhase code with const evaluation to target languages
    Transpile(TranspileArgs),

    /// Evaluate expressions using the interpreter
    Eval(EvalArgs),

    /// Parse and display AST for FerroPhase code
    Parse(ParseArgs),

    /// Run a FerroPhase file (alias for eval --file)
    Run(RunArgs),

    /// Check and validate FerroPhase code
    Check(CheckArgs),

    /// Initialize a new FerroPhase project
    Init(InitArgs),

    /// Show information about the installation
    Info(InfoArgs),

    /// Generate shell completions
    Completions(CompletionsArgs),
}

#[tokio::main]
async fn main() -> Result<()> {
    // Minimal bootstrap fast-path: when self-hosting (Stage 2), bypass Clap/Tracing and
    // invoke the pipeline deterministically from the AST snapshot.
    if std::env::var_os("FERROPHASE_BOOTSTRAP").is_some()
        && std::env::var_os("FP_BOOTSTRAP_STAGE").as_deref() == Some(std::ffi::OsStr::new("2"))
    {
        return fp_cli::bootstrap::bootstrap_compile_from_env();
    }

    // Note: in bootstrap runs we still use the standard CLI + pipeline.
    let cli = Cli::parse();

    // Set up error reporting
    setup_error_reporting()?;

    // Configure logging
    setup_logging(cli.verbose, cli.quiet)?;

    // Change working directory if specified
    if let Some(dir) = &cli.directory {
        std::env::set_current_dir(dir).map_err(|e| fp_cli::CliError::Io(e))?;
    }

    // Load configuration
    let config = CliConfig::load(cli.config.as_deref())?;

    // Execute command
    let result = match cli.command {
        Commands::Compile(args) => commands::compile_command(args, &config).await,
        Commands::Transpile(args) => commands::transpile_command(args, &config).await,
        Commands::Eval(args) => commands::eval_command(args, &config).await,
        Commands::Parse(args) => commands::parse_command(args, &config).await,
        Commands::Run(args) => commands::run_command(args, &config).await,
        Commands::Check(args) => commands::check_command(args, &config).await,
        Commands::Init(args) => commands::init_command(args, &config).await,
        Commands::Info(args) => commands::info_command(args, &config).await,
        Commands::Completions(args) => commands::completions_command(args, &config).await,
    };

    match result {
        Ok(_) => {
            if cli.verbose > 0 {
                info!("Command completed successfully");
            }
            Ok(())
        }
        Err(e) => {
            use tracing::error;
            // Emit via structured logging rather than printing directly
            error!(error = %e, "{} {}", style("Error:").red().bold(), e);
            if cli.verbose > 0 {
                error!(?e, "detailed error context");
            }
            std::process::exit(1);
        }
    }
}

fn setup_logging(verbose: u8, quiet: bool) -> Result<()> {
    use tracing_subscriber::{EnvFilter, layer::SubscriberExt, util::SubscriberInitExt};

    let filter = if quiet {
        EnvFilter::new("error")
    } else {
        match verbose {
            0 => EnvFilter::new("info"),
            1 => EnvFilter::new("debug"),
            2 => EnvFilter::new("trace"),
            // For very verbose runs, try to add a more specific directive, but never panic
            _ => {
                let base = EnvFilter::new("trace");
                match "fp=trace".parse() {
                    Ok(d) => base.add_directive(d),
                    Err(e) => {
                        tracing::warn!(
                            "Failed to parse log directive 'fp=trace': {}; falling back to 'trace'",
                            e
                        );
                        base
                    }
                }
            }
        }
    };

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(false)
                .with_timer(tracing_subscriber::fmt::time::uptime())
                .with_level(true),
        )
        .with(filter)
        .init();

    Ok(())
}
