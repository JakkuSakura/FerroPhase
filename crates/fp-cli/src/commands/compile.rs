//! Compilation command implementation

use crate::{
    CliError, Result,
    cli::CliConfig,
    config::{DebugOptions, ErrorToleranceOptions, PipelineOptions, PipelineTarget, RuntimeConfig},
    pipeline::{Pipeline, PipelineInput, PipelineOutput},
};
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use tokio::{fs as async_fs, process::Command};
use tracing::{info, warn};

/// Arguments for the compile command
#[derive(Debug, Clone)]
pub struct CompileArgs {
    pub input: Vec<PathBuf>,
    pub target: String,
    pub output: Option<PathBuf>,
    pub opt_level: u8,
    pub debug: bool,
    pub include: Vec<PathBuf>,
    pub define: Vec<String>,
    pub exec: bool,
    pub watch: bool,
    /// Enable error tolerance (collect multiple errors instead of early exit)
    pub error_tolerance: bool,
    /// Maximum number of errors to collect (0 = unlimited)
    pub max_errors: usize,
    /// Persist intermediate representations to disk
    pub save_intermediates: bool,
    /// Override source language detection
    pub source_language: Option<String>,
    /// Treat build as release (disable debug assertions)
    pub release: bool,
}

/// Execute the compile command
pub async fn compile_command(args: CompileArgs, config: &CliConfig) -> Result<()> {
    info!("Starting compilation with target: {}", args.target);

    // Validate inputs
    validate_inputs(&args)?;

    if args.watch {
        compile_with_watch(args, config).await
    } else {
        compile_once(args, config).await
    }
}

async fn compile_once(args: CompileArgs, config: &CliConfig) -> Result<()> {
    let progress = setup_progress_bar(args.input.len());

    let mut compiled_files = Vec::new();

    for (_i, input_file) in args.input.iter().enumerate() {
        progress.set_message(format!("Compiling {}", input_file.display()));

        let output_file = determine_output_path(input_file, args.output.as_ref(), &args.target)?;

        // Compile single file
        if let Some(artifact_path) = compile_file(input_file, &output_file, &args, config).await? {
            compiled_files.push(artifact_path);
        }
        progress.inc(1);
    }

    progress.finish_with_message(format!(
        "{} Compiled {} file(s) successfully",
        style("✓").green(),
        args.input.len()
    ));

    // Execute if requested
    if args.exec {
        if args.target == "binary" {
            match compiled_files.as_slice() {
                [] => {
                    warn!("No compiled binaries available to execute");
                }
                [path] => {
                    exec_compiled_binary(path).await?;
                }
                _ => {
                    return Err(CliError::Compilation(
                        "--exec currently supports compiling a single binary at a time".to_string(),
                    ));
                }
            }
        } else {
            warn!("--exec is only supported for binary targets");
        }
    }

    Ok(())
}

async fn compile_with_watch(args: CompileArgs, config: &CliConfig) -> Result<()> {
    use tokio::time::{Duration, sleep};

    println!("{} Watching for changes...", style("👀").cyan());

    let mut last_modified = std::collections::HashMap::new();

    loop {
        let mut needs_recompile = false;

        for input_file in &args.input {
            if let Ok(metadata) = std::fs::metadata(input_file) {
                if let Ok(modified) = metadata.modified() {
                    if let Some(&last_mod) = last_modified.get(input_file) {
                        if modified > last_mod {
                            needs_recompile = true;
                        }
                    } else {
                        needs_recompile = true;
                    }
                    last_modified.insert(input_file.clone(), modified);
                }
            }
        }

        if needs_recompile {
            println!(
                "{} File changes detected, recompiling...",
                style("🔄").yellow()
            );

            match compile_once(
                CompileArgs {
                    input: args.input.clone(),
                    target: args.target.clone(),
                    output: args.output.clone(),
                    opt_level: args.opt_level,
                    debug: args.debug,
                    include: args.include.clone(),
                    define: args.define.clone(),
                    exec: args.exec,
                    watch: false, // Prevent recursion
                    error_tolerance: args.error_tolerance,
                    max_errors: args.max_errors,
                    save_intermediates: args.save_intermediates,
                    source_language: args.source_language.clone(),
                    release: args.release,
                },
                config,
            )
            .await
            {
                Ok(_) => println!("{} Recompilation successful", style("✓").green()),
                Err(e) => eprintln!("{} Recompilation failed: {}", style("✗").red(), e),
            }
        }

        sleep(Duration::from_millis(500)).await;
    }
}

async fn compile_file(
    input: &Path,
    output: &Path,
    args: &CompileArgs,
    _config: &CliConfig,
) -> Result<Option<PathBuf>> {
    info!("Compiling: {} -> {}", input.display(), output.display());

    // Configure pipeline for compilation with new options
    let target = match args.target.as_str() {
        "rust" => PipelineTarget::Rust,
        "llvm" => PipelineTarget::Llvm,
        "binary" => PipelineTarget::Binary,
        "bytecode" => PipelineTarget::Bytecode,
        _ => PipelineTarget::Interpret,
    };

    let execute_const_main = args.exec;

    let pipeline_options = PipelineOptions {
        target,
        runtime: RuntimeConfig {
            runtime_type: "literal".to_string(),
            options: std::collections::HashMap::new(),
        },
        source_language: args.source_language.clone(),
        optimization_level: args.opt_level,
        save_intermediates: args.save_intermediates,
        base_path: Some(output.to_path_buf()),
        debug: DebugOptions {
            print_ast: false,
            print_passes: false,
            verbose: args.debug,
        },
        error_tolerance: ErrorToleranceOptions {
            enabled: args.error_tolerance,
            max_errors: if args.max_errors == 0 {
                50
            } else {
                args.max_errors
            }, // Default cap
            show_all_errors: true,
            continue_on_error: true,
        },
        release: args.release,
        execute_main: execute_const_main,
        bootstrap_mode: false,
        emit_bootstrap_snapshot: false,
    };

    // Execute pipeline with new options
    let mut pipeline = Pipeline::new();
    let pipeline_output = pipeline
        .execute_with_options(PipelineInput::File(input.to_path_buf()), pipeline_options)
        .await?;

    if execute_const_main {
        if let Some(stdout_chunks) = pipeline.take_last_const_eval_stdout() {
            for chunk in stdout_chunks {
                print!("{}", chunk);
            }
            let _ = io::stdout().flush();
        }
    }

    // Write output to file
    let artifact = match pipeline_output {
        PipelineOutput::Code(code) => {
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent).map_err(|e| CliError::Io(e))?;
            }

            std::fs::write(output, &code).map_err(|e| CliError::Io(e))?;

            info!("Generated code: {}", output.display());
            Some(output.to_path_buf())
        }
        PipelineOutput::Binary(path) => {
            let binary_path = path;
            if binary_path != *output {
                if let Some(parent) = output.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| CliError::Io(e))?;
                }
                async_fs::copy(&binary_path, output)
                    .await
                    .map_err(|e| CliError::Io(e))?;
                if !args.save_intermediates {
                    let _ = async_fs::remove_file(&binary_path).await;
                }
            }
            info!("Generated binary: {}", output.display());
            Some(output.to_path_buf())
        }
        PipelineOutput::Value(_) => {
            // For interpret target or binary target (already compiled), we don't write to file
            info!("Operation completed");
            None
        }
        PipelineOutput::RuntimeValue(_) => {
            // For runtime interpretation, we don't write to file
            info!("Runtime interpretation completed");
            None
        }
    };

    Ok(artifact)
}

async fn exec_compiled_binary(path: &Path) -> Result<()> {
    let is_executable = path
        .extension()
        .map_or(false, |ext| ext == "out" || ext == "exe")
        || (cfg!(unix) && path.extension().is_none());

    if !is_executable {
        return Err(CliError::Compilation(format!(
            "Refusing to execute '{}': unsupported binary extension",
            path.display()
        )));
    }

    println!(
        "{} Executing compiled binary: {}",
        style("🚀").cyan(),
        path.display()
    );

    let output = Command::new(path).output().await.map_err(|e| {
        CliError::Compilation(format!("Failed to execute '{}': {}", path.display(), e))
    })?;

    if !output.stdout.is_empty() {
        print!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }

    if !output.status.success() {
        let code = output.status.code().unwrap_or(-1);
        return Err(CliError::Compilation(format!(
            "Process exited with status {}",
            code
        )));
    }

    Ok(())
}

fn validate_inputs(args: &CompileArgs) -> Result<()> {
    for input in &args.input {
        if !input.exists() {
            return Err(CliError::InvalidInput(format!(
                "Input file does not exist: {}",
                input.display()
            )));
        }

        if !input.is_file() {
            return Err(CliError::InvalidInput(format!(
                "Input path is not a file: {}",
                input.display()
            )));
        }
    }

    // Validate optimization level
    if args.opt_level > 3 {
        return Err(CliError::InvalidInput(
            "Optimization level must be 0-3".to_string(),
        ));
    }

    Ok(())
}

fn determine_output_path(input: &Path, output: Option<&PathBuf>, target: &str) -> Result<PathBuf> {
    if let Some(output) = output {
        if target == "binary" {
            let mut path = output.clone();
            let desired_ext = if cfg!(target_os = "windows") {
                "exe"
            } else {
                "out"
            };

            let needs_update = path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext != desired_ext)
                .unwrap_or(true);

            if needs_update {
                path.set_extension(desired_ext);
            }

            Ok(path)
        } else {
            Ok(output.clone())
        }
    } else {
        let extension = match target {
            "binary" => {
                // Use platform-specific executable extension
                if cfg!(target_os = "windows") {
                    "exe"
                } else {
                    "out" // Use .out extension on Unix systems for clarity
                }
            }
            "rust" => "rs",
            "llvm" => "ll",
            "wasm" => "wasm",
            _ => {
                return Err(CliError::InvalidInput(format!(
                    "Unknown target for output extension: {}",
                    target
                )));
            }
        };

        Ok(input.with_extension(extension))
    }
}

fn setup_progress_bar(total: usize) -> ProgressBar {
    let pb = ProgressBar::new(total as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}")
            .unwrap()
            .progress_chars("#>-"),
    );
    pb
}
