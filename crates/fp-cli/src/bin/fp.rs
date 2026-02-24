//! FerroPhase CLI Binary
//!
//! The main command-line interface for FerroPhase, a meta-compilation framework
//! that enables multi-language development with advanced compile-time capabilities.
//!
//! # Usage
//!
//! ```bash
//! # Compile a FerroPhase file to Rust
//! fp compile hello.fp --emitter rust --output hello.rs
//!
//! # Cross-compile a binary for a target triple
//! fp compile hello.fp --emitter binary --target x86_64-unknown-linux-gnu
//!
//! # Run const evaluation with interpretation
//! fp eval --expr "1 + 2 * 3"
//!
//! # Initialize a new FerroPhase project
//! magnet init my-project --template basic
//!
//! # Check and validate FerroPhase code
//! fp check src/
//!
//! # Start an interactive REPL
//! fp repl
//! ```

use clap::{Parser, Subcommand, ValueEnum};
use fp_cli::{
    Result,
    cli::CliConfig,
    commands::{
        self, check::CheckArgs, compile::CompileArgs, completions::CompletionsArgs, eval::EvalArgs,
        interpret::InterpretArgs, parse::ParseArgs, run::RunArgs,
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
    fp compile hello.fp --emitter rust    # Compile to Rust
    magnet init my-project                # Create new project
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

    /// Set log level (overrides --verbose/--quiet)
    #[arg(long, global = true, value_enum)]
    log: Option<LogLevel>,

    /// Set log output format
    #[arg(long, global = true, value_enum, default_value = "pretty")]
    log_format: LogFormat,

    /// Configuration file path
    #[arg(short, long, global = true)]
    config: Option<PathBuf>,

    /// Working directory
    #[arg(short = 'C', long, global = true)]
    directory: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum LogLevel {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum LogFormat {
    Pretty,
    Json,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile FerroPhase code to various targets
    Compile(CompileArgs),

    /// Evaluate expressions using the interpreter
    Eval(EvalArgs),

    /// Parse and display AST for FerroPhase code
    Parse(ParseArgs),

    /// Run a FerroPhase file
    Run(RunArgs),

    /// Interpret bytecode produced by `compile --emitter bytecode`
    Interpret(InterpretArgs),

    /// Check and validate FerroPhase code
    Check(CheckArgs),

    /// Generate shell completions
    Completions(CompletionsArgs),
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Set up error reporting
    setup_error_reporting()?;

    // Configure logging
    setup_logging(cli.verbose, cli.quiet, cli.log, cli.log_format)?;

    // Change working directory if specified
    if let Some(dir) = &cli.directory {
        std::env::set_current_dir(dir).map_err(|e| fp_cli::CliError::Io(e))?;
    }

    // Load configuration
    let config = CliConfig::load(cli.config.as_deref())?;

    // Execute command
    let result = match cli.command {
        Commands::Compile(args) => commands::compile_command(args, &config).await,
        Commands::Eval(args) => commands::eval_command(args, &config).await,
        Commands::Parse(args) => commands::parse_command(args, &config).await,
        Commands::Run(args) => commands::run_command(args, &config).await,
        Commands::Interpret(args) => commands::interpret_command(args, &config).await,
        Commands::Check(args) => commands::check_command(args, &config).await,
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
            if !fp_cli::diagnostics::render_cli_error(&e) {
                // Emit via structured logging rather than printing directly
                error!("{}", e);
            }
            if cli.verbose > 0 {
                error!(?e, "detailed error context");
            }
            std::process::exit(1);
        }
    }
}

fn setup_logging(
    verbose: u8,
    quiet: bool,
    log_level: Option<LogLevel>,
    log_format: LogFormat,
) -> Result<()> {
    use tracing_subscriber::{EnvFilter, layer::SubscriberExt, util::SubscriberInitExt};

    let filter = if let Some(level) = log_level {
        EnvFilter::new(match level {
            LogLevel::Error => "error",
            LogLevel::Warn => "warn",
            LogLevel::Info => "info",
            LogLevel::Debug => "debug",
            LogLevel::Trace => "trace",
        })
    } else if quiet {
        EnvFilter::new("error")
    } else {
        match verbose {
            0 => EnvFilter::new("info"),
            1 => EnvFilter::new("debug"),
            2 => EnvFilter::new("trace"),
            _ => EnvFilter::new("trace"),
        }
    };

    let formatter = tracing_subscriber::fmt::layer()
        .with_target(false)
        .with_timer(tracing_subscriber::fmt::time::uptime())
        .with_level(true);

    match log_format {
        LogFormat::Pretty => {
            tracing_subscriber::registry()
                .with(formatter)
                .with(filter)
                .init();
        }
        LogFormat::Json => {
            tracing_subscriber::registry()
                .with(formatter.json())
                .with(filter)
                .init();
        }
    }

    Ok(())
}
