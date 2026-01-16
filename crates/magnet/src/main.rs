use clap::{Parser, Subcommand, ValueEnum};
use eyre::{Context, Result};
use std::path::PathBuf;
use tracing::{debug, info};

// Use local utils module instead of common crate
use magnet::commands::{
    self, BenchOptions, BuildOptions, LockOptions, RunMode, RunOptions, TestOptions, UpdateOptions,
    generate::GenerateOptions,
};
use magnet::utils::{LogLevel, setup_logs};

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
        Some(Commands::Init { path, from_cargo }) => commands::init(&path, from_cargo),
        Some(Commands::Generate {
            config,
            clean,
            copy_lock,
            include_cargo_dir,
            symlink_cargo_dir,
        }) => {
            let options = GenerateOptions {
                config_path: config,
                clean,
                copy_lock,
                include_cargo_dir,
                symlink_cargo_dir,
            };
            commands::generate(&options)
        }
        Some(Commands::Check { config }) => commands::check(&config),
        Some(Commands::Tree { config }) => commands::tree(&config),
        Some(Commands::Graph { config, output }) => commands::graph(&config, output.as_deref()),
        Some(Commands::Lock {
            path,
            offline,
            cache_dir,
            fetch,
        }) => {
            let options = LockOptions {
                path,
                offline,
                cache_dir,
                fetch,
            };
            commands::lock(&options)
        }
        Some(Commands::Update {
            path,
            offline,
            cache_dir,
            fetch,
        }) => {
            let options = UpdateOptions {
                path,
                offline,
                cache_dir,
                fetch,
            };
            commands::update(&options)
        }
        Some(Commands::Export {
            package,
            clean,
            copy_lock,
            include_cargo_dir,
            symlink_cargo_dir,
            export_dir,
            crates_dir,
        }) => {
            let options = commands::export::ExportOptions {
                package_path: package,
                clean,
                copy_lock,
                include_cargo_dir,
                symlink_cargo_dir,
                export_dir,
                crates_dir,
            };
            commands::export(&options)
        }
        Some(Commands::Submodule {
            action,
            path,
            remote,
        }) => match action {
            SubmoduleAction::Init => commands::submodule_init(&path),
            SubmoduleAction::Update => commands::submodule_update(&path, remote),
            SubmoduleAction::Deinit { submodule_path } => {
                commands::submodule_deinit(&path, &submodule_path)
            }
            SubmoduleAction::List => commands::submodule_list(&path),
            SubmoduleAction::Switch { rev } => commands::submodule_switch(&path, &rev),
        },
        Some(Commands::Run {
            path,
            package,
            entry,
            mode,
            resolver,
            example,
            release,
            profile,
            build_options,
            offline,
            cache_dir,
            fetch,
        }) => {
            let options = RunOptions {
                path,
                package,
                entry,
                mode: mode.into(),
                resolver,
                example,
                release,
                profile,
                build_options,
                offline,
                cache_dir,
                fetch,
            };
            commands::run(&options)
        }
        Some(Commands::Build {
            path,
            package,
            entry,
            resolver,
            example,
            release,
            profile,
            build_options,
            offline,
            cache_dir,
            fetch,
        }) => {
            let options = BuildOptions {
                path,
                package,
                entry,
                resolver,
                example,
                release,
                profile,
                build_options,
                offline,
                cache_dir,
                fetch,
            };
            commands::build(&options)
        }
        Some(Commands::Test {
            path,
            package,
            release,
            profile,
            test,
            tests,
            example,
            examples,
            args,
        }) => {
            let options = TestOptions {
                path,
                package,
                release,
                profile,
                test,
                tests,
                example,
                examples,
                args,
            };
            commands::test(&options)
        }
        Some(Commands::Bench {
            path,
            package,
            release,
            profile,
            bench,
            benches,
            example,
            examples,
            args,
        }) => {
            let options = BenchOptions {
                path,
                package,
                release,
                profile,
                bench,
                benches,
                example,
                examples,
                args,
            };
            commands::bench(&options)
        }
        None => {
            info!("No command specified. Run with --help for usage information.");
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

        /// Initialize Magnet.toml from an existing Cargo.toml
        #[arg(long)]
        from_cargo: bool,
    },
    /// Generate or update Cargo.toml files from Magnet.toml
    Generate {
        /// Path to the Magnet.toml file
        #[arg(default_value = ".")]
        config: PathBuf,

        /// Clean the output directory before generating files
        #[arg(short = 'c', long)]
        clean: bool,

        /// Copy the Cargo.lock file if it exists
        #[arg(short = 'l', long, default_value_t = true)]
        copy_lock: bool,

        /// Include .cargo directory in the generation
        #[arg(long, default_value_t = true)]
        include_cargo_dir: bool,

        /// Create symlinks for .cargo directory instead of copying
        #[arg(long, default_value_t = true)]
        symlink_cargo_dir: bool,
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
    /// Render dependency graph as Graphviz DOT
    Graph {
        /// Path to the Magnet.toml file
        #[arg(default_value = ".")]
        config: PathBuf,
        /// Output path for the DOT file (default: target/dependencies.dot)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Resolve dependencies and generate Magnet.lock
    Lock {
        /// Path to a package/workspace directory or manifest file
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Run without network access (use cached crates)
        #[arg(long)]
        offline: bool,

        /// Override the cargo registry cache directory
        #[arg(long)]
        cache_dir: Option<PathBuf>,

        /// Skip prefetching registry dependencies
        #[arg(long = "no-fetch", default_value_t = true, action = clap::ArgAction::SetFalse)]
        fetch: bool,
    },
    /// Update Magnet.lock with the latest resolved dependencies
    Update {
        /// Path to a package/workspace directory or manifest file
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Run without network access (use cached crates)
        #[arg(long)]
        offline: bool,

        /// Override the cargo registry cache directory
        #[arg(long)]
        cache_dir: Option<PathBuf>,

        /// Skip prefetching registry dependencies
        #[arg(long = "no-fetch", default_value_t = true, action = clap::ArgAction::SetFalse)]
        fetch: bool,
    },
    /// Export local dependencies for a package/workspace
    Export {
        /// Path to the package or workspace directory
        #[arg(default_value = ".")]
        package: PathBuf,

        /// Clean the export directory before exporting
        #[arg(short = 'c', long, default_value_t = true)]
        clean: bool,

        /// Copy the Cargo.lock file if it exists
        #[arg(short = 'l', long, default_value_t = true)]
        copy_lock: bool,

        /// Include .cargo directory in the export
        #[arg(long, default_value_t = true)]
        include_cargo_dir: bool,

        /// Create symlinks for .cargo directory instead of copying
        #[arg(long, default_value_t = true)]
        symlink_cargo_dir: bool,

        /// Custom directory to export to (default: $PWD/target/export)
        #[arg(short = 'o', long)]
        export_dir: Option<PathBuf>,

        /// Subdirectory name for exported crates (default: "crates")
        #[arg(short = 'd', long, default_value = "crates")]
        crates_dir: String,
    },
    /// Run a FerroPhase package via fp-cli
    Run {
        /// Path to a package/workspace directory or entry file
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Select a package by name (preferred over cwd/auto)
        #[arg(long)]
        package: Option<String>,

        /// Override the entry file (default: src/main.fp)
        #[arg(long)]
        entry: Option<PathBuf>,

        /// Run mode (compile or interpret)
        #[arg(long, default_value = "compile")]
        mode: RunModeArg,

        /// Module resolver strategy used by fp
        #[arg(long, default_value = "ferrophase")]
        resolver: String,

        /// Run a named example under ./examples
        #[arg(long)]
        example: Option<String>,

        /// Use release profile output paths
        #[arg(long)]
        release: bool,

        /// Cargo-style profile name (overrides --release)
        #[arg(long)]
        profile: Option<String>,

        /// Build options forwarded to fp (key=value)
        #[arg(long = "build-option")]
        build_options: Vec<String>,

        /// Run without network access (use cached crates)
        #[arg(long)]
        offline: bool,

        /// Override the cargo registry cache directory
        #[arg(long)]
        cache_dir: Option<PathBuf>,

        /// Skip prefetching registry dependencies
        #[arg(long = "no-fetch", default_value_t = true, action = clap::ArgAction::SetFalse)]
        fetch: bool,
    },
    /// Build a FerroPhase package via fp-cli
    Build {
        /// Path to a package/workspace directory or entry file
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Select a package by name (preferred over cwd/auto)
        #[arg(long)]
        package: Option<String>,

        /// Override the entry file (default: src/main.fp)
        #[arg(long)]
        entry: Option<PathBuf>,

        /// Module resolver strategy used by fp
        #[arg(long, default_value = "ferrophase")]
        resolver: String,

        /// Build a named example under ./examples
        #[arg(long)]
        example: Option<String>,

        /// Use release profile output paths
        #[arg(long)]
        release: bool,

        /// Cargo-style profile name (overrides --release)
        #[arg(long)]
        profile: Option<String>,

        /// Build options forwarded to fp (key=value)
        #[arg(long = "build-option")]
        build_options: Vec<String>,

        /// Run without network access (use cached crates)
        #[arg(long)]
        offline: bool,

        /// Override the cargo registry cache directory
        #[arg(long)]
        cache_dir: Option<PathBuf>,

        /// Skip prefetching registry dependencies
        #[arg(long = "no-fetch", default_value_t = true, action = clap::ArgAction::SetFalse)]
        fetch: bool,
    },
    /// Run tests for a package
    Test {
        /// Path to a package/workspace directory or manifest
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Select a package by name
        #[arg(long)]
        package: Option<String>,

        /// Run tests in release mode
        #[arg(long)]
        release: bool,

        /// Cargo-style profile name (overrides --release)
        #[arg(long)]
        profile: Option<String>,

        /// Run only the specified test target(s)
        #[arg(long)]
        test: Vec<String>,

        /// Run all tests
        #[arg(long)]
        tests: bool,

        /// Run only the specified example target(s)
        #[arg(long)]
        example: Vec<String>,

        /// Run all examples
        #[arg(long)]
        examples: bool,

        /// Arguments passed to the test binary after '--'
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Run benchmarks for a package
    Bench {
        /// Path to a package/workspace directory or manifest
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Select a package by name
        #[arg(long)]
        package: Option<String>,

        /// Run benchmarks in release mode
        #[arg(long)]
        release: bool,

        /// Cargo-style profile name (overrides --release)
        #[arg(long)]
        profile: Option<String>,

        /// Run only the specified benchmark target(s)
        #[arg(long)]
        bench: Vec<String>,

        /// Run all benchmarks
        #[arg(long)]
        benches: bool,

        /// Run only the specified example target(s)
        #[arg(long)]
        example: Vec<String>,

        /// Run all examples
        #[arg(long)]
        examples: bool,

        /// Arguments passed to the benchmark binary after '--'
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Manage git submodules
    Submodule {
        /// Action to perform on submodules
        #[command(subcommand)]
        action: SubmoduleAction,

        /// Path to the repository root
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Fetch latest changes from remote repository (only for update)
        #[arg(short, long)]
        remote: bool,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum RunModeArg {
    Compile,
    Interpret,
}

impl From<RunModeArg> for RunMode {
    fn from(value: RunModeArg) -> Self {
        match value {
            RunModeArg::Compile => RunMode::Compile,
            RunModeArg::Interpret => RunMode::Interpret,
        }
    }
}

#[derive(Subcommand)]
enum SubmoduleAction {
    /// Initialize and update submodules
    Init,
    /// Update submodules with latest changes
    Update,
    /// Deinitialize (remove) a submodule
    Deinit {
        /// Path to the submodule to deinitialize
        submodule_path: PathBuf,
    },
    /// List all submodules
    List,
    /// Switch submodules to a specific revision
    Switch {
        /// Revision to switch to
        rev: String,
    },
}
