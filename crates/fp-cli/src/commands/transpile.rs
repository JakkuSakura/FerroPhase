//! Simplified transpilation command using the new unified transpiler

use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, TranspilePreparationOptions},
    transpiler::*,
};
use console::style;
use fp_rust::{parse_cargo_workspace, printer::RustPrinter};
use fp_wit::{WitOptions, WorldMode};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::info;

/// Arguments for the transpile command
#[derive(Debug, Clone)]
pub struct TranspileArgs {
    pub input: Vec<PathBuf>,
    pub target: String,
    pub output: Option<PathBuf>,
    pub const_eval: bool,
    pub preserve_structs: bool,
    pub type_defs: bool,
    pub pretty: bool,
    pub source_maps: bool,
    pub watch: bool,
    pub single_world: bool,
}

/// Execute the transpile command
pub async fn transpile_command(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    info!("Starting transpilation to target: {}", args.target);

    validate_transpile_inputs(&args)?;

    if args.watch {
        transpile_with_watch(args, config).await
    } else {
        transpile_once(args, config).await
    }
}

async fn transpile_once(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    let progress = setup_progress_bar(args.input.len());

    for input_file in &args.input {
        progress.set_message(format!("Transpiling {}", input_file.display()));

        let output_file =
            determine_transpile_output_path(input_file, args.output.as_ref(), &args.target)?;
        transpile_file(input_file, &output_file, &args, config).await?;

        progress.inc(1);
    }

    progress.finish_with_message(format!(
        "{} Transpiled {} file(s) to {}",
        style("✓").green(),
        args.input.len(),
        args.target
    ));

    Ok(())
}

async fn transpile_with_watch(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    use tokio::time::{Duration, sleep};

    println!("{} Watching for changes...", style("👀").cyan());

    let mut last_modified = std::collections::HashMap::new();

    loop {
        let mut needs_retranspile = false;

        for input_file in &args.input {
            if let Ok(metadata) = std::fs::metadata(input_file) {
                if let Ok(modified) = metadata.modified() {
                    if let Some(&last_mod) = last_modified.get(input_file) {
                        if modified > last_mod {
                            needs_retranspile = true;
                        }
                    } else {
                        needs_retranspile = true;
                    }
                    last_modified.insert(input_file.clone(), modified);
                }
            }
        }

        if needs_retranspile {
            println!("{} Re-transpiling...", style("🔄").yellow());

            match transpile_once(
                TranspileArgs {
                    input: args.input.clone(),
                    target: args.target.clone(),
                    output: args.output.clone(),
                    const_eval: args.const_eval,
                    preserve_structs: args.preserve_structs,
                    type_defs: args.type_defs,
                    pretty: args.pretty,
                    source_maps: args.source_maps,
                    watch: false,
                    single_world: args.single_world,
                },
                config,
            )
            .await
            {
                Ok(_) => println!("{} Success", style("✓").green()),
                Err(e) => eprintln!("{} Failed: {}", style("✗").red(), e),
            }
        }

        sleep(Duration::from_millis(500)).await;
    }
}

async fn transpile_file(
    input: &Path,
    output: &Path,
    args: &TranspileArgs,
    _config: &CliConfig,
) -> Result<()> {
    info!(
        "Transpiling: {} -> {} ({})",
        input.display(),
        output.display(),
        args.target
    );

    // Parse source
    let mut pipeline = Pipeline::new();
    let is_cargo_manifest = input
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.eq_ignore_ascii_case("Cargo.toml"))
        .unwrap_or(false);

    let is_wit_input = input
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case("wit"))
        .unwrap_or(false);

    let (mut ast, base_path) = if is_cargo_manifest {
        let node = parse_cargo_workspace(input)?;
        pipeline.set_serializer(Arc::new(RustPrinter::new_with_rustfmt()));
        (node, input.parent().map(|dir| dir.to_path_buf()))
    } else {
        let source = std::fs::read_to_string(input).map_err(CliError::Io)?;
        let node = pipeline.parse_source_public(&source, Some(input))?;
        (node, None)
    };

    let prep_options = TranspilePreparationOptions {
        run_const_eval: args.const_eval,
        save_intermediates: false,
        base_path,
    };
    if !is_cargo_manifest && !is_wit_input {
        pipeline.prepare_for_transpile(&mut ast, &prep_options)?;
    }

    // Use simplified transpiler
    let normalized_target = args.target.to_lowercase();
    let target = match normalized_target.as_str() {
        "typescript" | "ts" => TranspileTarget::TypeScript,
        "javascript" | "js" => TranspileTarget::JavaScript,
        "csharp" | "cs" | "c#" => TranspileTarget::CSharp,
        "python" | "py" => TranspileTarget::Python,
        "zig" => TranspileTarget::Zig,
        "rust" | "rs" => TranspileTarget::Rust,
        "wit" => TranspileTarget::Wit,
        _ => {
            return Err(CliError::InvalidInput(format!(
                "Unsupported target: {}",
                args.target
            )));
        }
    };

    let mut transpiler = Transpiler::new(target, args.type_defs);
    if matches!(target, TranspileTarget::Wit) {
        let mut options = WitOptions::default();
        if args.single_world {
            options.world_mode = WorldMode::Single {
                world_name: options.root_interface.clone(),
            };
        }
        transpiler = transpiler.with_wit_options(options);
    }
    let result = transpiler.transpile(&ast)?;

    // Write output
    std::fs::write(output, result.code).map_err(|e| CliError::Io(e))?;
    if let Some(defs) = result.type_defs {
        let mut defs_path = output.to_path_buf();
        if let Some(stem) = defs_path.file_stem().and_then(|s| s.to_str()) {
            defs_path.set_file_name(format!("{}.d.ts", stem));
        } else {
            defs_path.set_extension("d.ts");
        }
        std::fs::write(defs_path, defs).map_err(|e| CliError::Io(e))?;
    }
    info!("Generated: {}", output.display());

    Ok(())
}

// Utility functions
fn validate_transpile_inputs(args: &TranspileArgs) -> Result<()> {
    for input in &args.input {
        if !input.exists() {
            return Err(CliError::InvalidInput(format!(
                "Input file does not exist: {}",
                input.display()
            )));
        }
    }
    Ok(())
}

fn determine_transpile_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: &str,
) -> Result<PathBuf> {
    if let Some(output) = output {
        Ok(output.clone())
    } else {
        let normalized = target.to_lowercase();
        let extension = match normalized.as_str() {
            "typescript" | "ts" => "ts",
            "javascript" | "js" => "js",
            "csharp" | "cs" | "c#" => "cs",
            "python" | "py" => "py",
            "zig" => "zig",
            "rust" | "rs" => "rs",
            "wit" => "wit",
            _ => {
                return Err(CliError::InvalidInput(format!(
                    "Unknown target: {}",
                    target
                )));
            }
        };
        Ok(input.with_extension(extension))
    }
}

fn setup_progress_bar(total: usize) -> indicatif::ProgressBar {
    use indicatif::{ProgressBar, ProgressStyle};
    let pb = ProgressBar::new(total as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}")
            .unwrap()
            .progress_chars("#>-"),
    );
    pb
}
