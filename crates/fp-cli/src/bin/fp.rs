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
//! # Cross-compile a binary for a target triple
//! fp compile hello.fp --target native --target-triple x86_64-unknown-linux-gnu
//!
//! # Initialize a new FerroPhase project
//! magnet init my-project --template basic
//!
//! # Start an interactive REPL
//! fp repl
//! ```

use clap::{Parser, Subcommand, ValueEnum};
use fp_cli::{
    Result,
    cli::CliConfig,
    commands::{
        self, compile::CompileArgs, completions::CompletionsArgs, inspect::InspectArgs,
        interpret::InterpretArgs,
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
    fp interpret hello.fp                 # Interpret a FerroPhase file
    fp compile hello.fp --target rust     # Compile to Rust
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

    /// Interpret bytecode produced by `compile --target bytecode`
    Interpret(InterpretArgs),

    /// Inspect binary, bytecode, and object artifacts
    Inspect(InspectArgs),

    /// Generate shell completions
    Completions(CompletionsArgs),
}

/// Compiler-driving code (`fp-typing`'s recursive-descent type checker,
/// driven via `CompilerExecutor::run` over `CompilerDriver::compile_native`) recurses once per AST
/// node through boxed `Future`s, whose generated state machines are
/// considerably larger per frame than a plain function call -- so a modestly
/// deep source expression can need more stack than a thread's default. Give
/// the runtime's worker threads the same generous stack `run_all_example_files`
/// already uses in tests, rather than let a real user's program abort with a
/// stack overflow.
fn main() -> Result<()> {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_stack_size(16 * 1024 * 1024)
        .build()
        .expect("failed to build tokio runtime");
    runtime.block_on(async_main())
}

async fn async_main() -> Result<()> {
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
        Commands::Interpret(args) => commands::interpret_command(args, &config).await,
        Commands::Inspect(args) => commands::inspect_command(args, &config).await,
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
