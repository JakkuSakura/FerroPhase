//! Information command implementation

use crate::{Result, cli::CliConfig};
use console::style;

/// Arguments for the info command
pub struct InfoArgs {
    pub version: bool,
    pub build: bool,
    pub features: bool,
    pub all: bool,
}

/// Execute the info command
pub async fn info_command(args: InfoArgs, _config: &CliConfig) -> Result<()> {
    if args.all || args.version {
        print_version_info();
    }

    if args.all || args.build {
        print_build_info();
    }

    if args.all || args.features {
        print_feature_info();
    }

    if !args.version && !args.build && !args.features && !args.all {
        // Default: show version and basic info
        print_version_info();
        print_basic_info();
    }

    Ok(())
}

fn print_version_info() {
    println!("{}", style("FerroPhase").cyan().bold());
    println!("Version: {}", env!("CARGO_PKG_VERSION"));
    println!(
        "Built: {}",
        option_env!("VERGEN_BUILD_TIMESTAMP").unwrap_or("unknown")
    );
    println!(
        "Commit: {}",
        option_env!("VERGEN_GIT_SHA").unwrap_or("unknown")
    );
    println!();
}

fn print_build_info() {
    println!("{}", style("Build Information").yellow().bold());
    println!(
        "Rust version: {}",
        option_env!("VERGEN_RUSTC_SEMVER").unwrap_or("unknown")
    );
    println!("Target: {}", std::env::consts::ARCH);
    println!(
        "Profile: {}",
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        }
    );
    println!(
        "Features: {}",
        option_env!("VERGEN_CARGO_FEATURES").unwrap_or("default")
    );
    println!();
}

fn print_feature_info() {
    println!("{}", style("Available Features").green().bold());

    let features = vec![
        (
            "Multi-language frontend",
            "Rust-with-extensions, Python, JavaScript",
        ),
        ("Const evaluation", "Advanced compile-time computation"),
        (
            "Optimization passes",
            "Specialization, inlining, const folding",
        ),
        ("Backend targets", "Rust transpilation, LLVM, WebAssembly"),
        ("Type system", "Structural typing, unions, intersections"),
        (
            "Metaprogramming",
            "Compile-time intrinsics and code generation",
        ),
    ];

    for (feature, description) in features {
        println!("  {} {}", style("✓").green(), style(feature).bold());
        println!("    {}", style(description).dim());
    }
    println!();
}

fn print_basic_info() {
    println!(
        "{}",
        style("Meta-compilation framework with multi-language comptime superpowers").dim()
    );
    println!();
    println!(
        "Documentation: {}",
        style("https://github.com/your-org/FerroPhase").cyan()
    );
    println!(
        "Issues: {}",
        style("https://github.com/your-org/FerroPhase/issues").cyan()
    );
    println!();
    println!("Use {} for help", style("fp --help").green());
    println!();
}
