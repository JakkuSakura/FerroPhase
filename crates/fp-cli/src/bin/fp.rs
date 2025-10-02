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

use clap::{Args, Parser, Subcommand};
use console::style;
use fp_cli::{Result, cli::CliConfig, commands, diagnostics::setup_error_reporting};
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

#[derive(Args)]
struct CompileArgs {
    /// Input file(s) to compile
    #[arg(required = true)]
    input: Vec<PathBuf>,

    /// Output target (binary, rust, llvm, wasm, interpret)
    #[arg(short, long, default_value = "binary")]
    target: String,

    /// Output file or directory
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Optimization level (0, 1, 2, 3)
    #[arg(short = 'O', long, default_value = "2")]
    opt_level: u8,

    /// Enable debug information
    #[arg(short, long)]
    debug: bool,

    /// Treat build as release (disables debug assertions)
    #[arg(long)]
    release: bool,

    /// Additional include directories
    #[arg(short = 'I', long)]
    include: Vec<PathBuf>,

    /// Define constants for compilation
    #[arg(short = 'D', long)]
    define: Vec<String>,

    /// Execute the compiled binary using exec clib function
    #[arg(short, long)]
    exec: bool,

    /// Watch for file changes and recompile
    #[arg(short, long)]
    watch: bool,

    /// Persist intermediate representations to disk
    #[arg(long)]
    save_intermediates: bool,

    /// Enable error tolerance during compilation
    #[arg(long)]
    error_tolerance: bool,

    /// Maximum number of errors to collect when tolerance is enabled (0 = unlimited)
    #[arg(long, default_value = "50")]
    max_errors: usize,

    /// Override automatic source language detection (e.g. "typescript")
    #[arg(long = "lang", alias = "language")]
    language: Option<String>,
}

#[derive(Args)]
struct TranspileArgs {
    /// Input file(s) to transpile
    #[arg(required = true)]
    input: Vec<PathBuf>,

    /// Target language (javascript, typescript, python, go)
    #[arg(short, long, default_value = "typescript")]
    target: String,

    /// Output file or directory
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Perform const evaluation before transpilation
    #[arg(long, default_value = "true")]
    const_eval: bool,

    /// Preserve struct types and methods
    #[arg(long, default_value = "true")]
    preserve_structs: bool,

    /// Generate type definitions (for TypeScript)
    #[arg(long)]
    type_defs: bool,

    /// Pretty print output
    #[arg(long, default_value = "true")]
    pretty: bool,

    /// Include source maps
    #[arg(long)]
    source_maps: bool,

    /// Watch for file changes and re-transpile
    #[arg(short, long)]
    watch: bool,
}

#[derive(Args)]
struct EvalArgs {
    /// Expression to evaluate
    #[arg(short, long, conflicts_with = "file")]
    expr: Option<String>,

    /// File containing code to evaluate
    #[arg(short, long)]
    file: Option<PathBuf>,

    /// Print the AST representation
    #[arg(long)]
    print_ast: bool,

    /// Print optimization passes
    #[arg(long)]
    print_passes: bool,

    /// Runtime semantics to use (literal, rust)
    #[arg(long, default_value = "literal")]
    runtime: String,
}

#[derive(Args)]
struct ParseArgs {
    /// Expression to parse
    #[arg(short, long, conflicts_with = "file")]
    expr: Option<String>,

    /// File containing code to parse
    #[arg(short, long)]
    file: Option<PathBuf>,
}

#[derive(Args)]
struct RunArgs {
    /// FerroPhase file to run
    file: PathBuf,

    /// Print the AST representation
    #[arg(long)]
    print_ast: bool,

    /// Print optimization passes
    #[arg(long)]
    print_passes: bool,

    /// Runtime semantics to use (literal, rust)
    #[arg(long, default_value = "literal")]
    runtime: String,
}

#[derive(Args)]
struct CheckArgs {
    /// Files or directories to check
    #[arg(default_value = ".")]
    paths: Vec<PathBuf>,

    /// Include patterns (glob)
    #[arg(long)]
    include: Vec<String>,

    /// Exclude patterns (glob)  
    #[arg(long)]
    exclude: Vec<String>,

    /// Check only syntax, skip semantic analysis
    #[arg(long)]
    syntax_only: bool,
}

#[derive(Args)]
struct InitArgs {
    /// Project name
    project_name: String,

    /// Project template (basic, library, binary, multi-lang)
    #[arg(short, long, default_value = "basic")]
    template: String,

    /// Target directory (defaults to project name)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Initialize git repository
    #[arg(long)]
    git: bool,
}

#[derive(Args)]
struct InfoArgs {
    /// Show version information
    #[arg(long)]
    version: bool,

    /// Show build information  
    #[arg(long)]
    build: bool,

    /// Show feature flags
    #[arg(long)]
    features: bool,

    /// Show all information
    #[arg(long)]
    all: bool,
}

#[derive(Args)]
struct CompletionsArgs {
    /// Shell to generate completions for
    #[arg(value_enum)]
    shell: clap_complete::Shell,
}

#[tokio::main]
async fn main() -> Result<()> {
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
        Commands::Compile(args) => {
            let compile_args = commands::compile::CompileArgs {
                input: args.input,
                target: args.target,
                output: args.output,
                opt_level: args.opt_level,
                debug: args.debug,
                release: args.release,
                include: args.include,
                define: args.define,
                exec: args.exec,
                watch: args.watch,
                error_tolerance: args.error_tolerance,
                max_errors: args.max_errors,
                save_intermediates: args.save_intermediates,
                source_language: args.language,
                force_const_exec: false,
            };
            commands::compile_command(compile_args, &config).await
        }
        Commands::Transpile(args) => {
            let transpile_args = commands::transpile::TranspileArgs {
                input: args.input,
                target: args.target,
                output: args.output,
                const_eval: args.const_eval,
                preserve_structs: args.preserve_structs,
                type_defs: args.type_defs,
                pretty: args.pretty,
                source_maps: args.source_maps,
                watch: args.watch,
            };
            commands::transpile_command(transpile_args, &config).await
        }
        Commands::Eval(args) => {
            let eval_args = commands::eval::EvalArgs {
                expr: args.expr,
                file: args.file,
                print_ast: args.print_ast,
                print_passes: args.print_passes,
                print_result: true, // For eval command, always print result
                runtime: Some(args.runtime),
            };
            commands::eval_command(eval_args, &config).await
        }
        Commands::Parse(args) => {
            let parse_args = commands::parse::ParseArgs {
                expr: args.expr,
                file: args.file,
            };
            commands::parse_command(parse_args, &config).await
        }
        Commands::Run(args) => {
            // Use dedicated run command with simplified pipeline
            let run_args = commands::run::RunArgs {
                file: args.file,
                print_ast: args.print_ast,
                print_passes: args.print_passes,
                runtime: Some(args.runtime), // Use specified runtime
            };
            commands::run_command(run_args, &config).await
        }
        Commands::Check(args) => {
            let check_args = commands::check::CheckArgs {
                paths: args.paths,
                include: args.include,
                exclude: args.exclude,
                syntax_only: args.syntax_only,
            };
            commands::check_command(check_args, &config).await
        }
        Commands::Init(args) => {
            let init_args = commands::init::InitArgs {
                project_name: args.project_name,
                template: args.template,
                output: args.output,
                git: args.git,
            };
            commands::init_command(init_args, &config).await
        }
        Commands::Info(args) => {
            let info_args = commands::info::InfoArgs {
                version: args.version,
                build: args.build,
                features: args.features,
                all: args.all,
            };
            commands::info_command(info_args, &config).await
        }
        Commands::Completions(args) => {
            let comp_args = commands::completions::CompletionsArgs { shell: args.shell };
            commands::completions_command(comp_args, &config).await
        }
    };

    match result {
        Ok(_) => {
            if cli.verbose > 0 {
                info!("Command completed successfully");
            }
            Ok(())
        }
        Err(e) => {
            eprintln!("{} {}", style("Error:").red().bold(), e);
            if cli.verbose > 0 {
                eprintln!("{:?}", e);
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
            _ => EnvFilter::new("trace").add_directive("fp=trace".parse().unwrap()),
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
