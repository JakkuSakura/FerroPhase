//! Compilation command implementation

use crate::{cli::CliConfig, pipeline::{Pipeline, PipelineConfig, PipelineInput, PipelineOutput}, Result, CliError};
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use std::path::{Path, PathBuf};
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
    pub run: bool,
    pub watch: bool,
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
        compile_file(input_file, &output_file, &args, config).await?;
        
        compiled_files.push(output_file);
        progress.inc(1);
    }
    
    progress.finish_with_message(format!(
        "{} Compiled {} file(s) successfully",
        style("✓").green(),
        args.input.len()
    ));
    
    // Run if requested
    if args.run {
        run_compiled_output(&compiled_files, &args.target).await?;
    }
    
    Ok(())
}

async fn compile_with_watch(args: CompileArgs, config: &CliConfig) -> Result<()> {
    use tokio::time::{sleep, Duration};
    
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
            println!("{} File changes detected, recompiling...", style("🔄").yellow());
            
            match compile_once(
                CompileArgs {
                    input: args.input.clone(),
                    target: args.target.clone(),
                    output: args.output.clone(),
                    opt_level: args.opt_level,
                    debug: args.debug,
                    include: args.include.clone(),
                    define: args.define.clone(),
                    run: args.run,
                    watch: false, // Prevent recursion
                },
                config,
            ).await {
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
) -> Result<()> {
    info!("Compiling: {} -> {}", input.display(), output.display());
    
    // Configure pipeline for compilation
    let pipeline_config = PipelineConfig {
        optimization_level: args.opt_level as u8,
        print_ast: false,
        print_passes: false,
        target: args.target.clone(),
    };
    
    // Execute pipeline
    let pipeline = Pipeline::new();
    let pipeline_output = pipeline.execute(PipelineInput::File(input.to_path_buf()), &pipeline_config).await?;
    
    // Write output to file
    match pipeline_output {
        PipelineOutput::Code(code) => {
            // Ensure output directory exists
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| CliError::Io(e))?;
            }
            
            std::fs::write(output, code)
                .map_err(|e| CliError::Io(e))?;
            
            info!("Generated code: {}", output.display());
        },
        PipelineOutput::Value(_) => {
            // For interpret target, we don't write to file
            info!("Interpretation completed");
        },
        _ => return Err(CliError::Compilation("Unexpected pipeline output".to_string())),
    }
    
    Ok(())
}

async fn run_compiled_output(files: &[PathBuf], target: &str) -> Result<()> {
    match target {
        "rust" => {
            for file in files {
                if file.extension().map_or(false, |ext| ext == "rs") {
                    println!("{} Running compiled Rust code...", style("🚀").cyan());
                    
                    // Compile with rustc and run
                    let output = tokio::process::Command::new("rustc")
                        .arg(file)
                        .arg("-o")
                        .arg(file.with_extension(""))
                        .output()
                        .await
                        .map_err(|e| CliError::Compilation(format!("Failed to run rustc: {}", e)))?;
                    
                    if !output.status.success() {
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        return Err(CliError::Compilation(format!("rustc failed: {}", stderr)));
                    }
                    
                    // Run the executable
                    let exe_path = file.with_extension("");
                    let run_output = tokio::process::Command::new(&exe_path)
                        .output()
                        .await
                        .map_err(|e| CliError::Compilation(format!("Failed to run executable: {}", e)))?;
                    
                    println!("{}", String::from_utf8_lossy(&run_output.stdout));
                    if !run_output.stderr.is_empty() {
                        eprintln!("{}", String::from_utf8_lossy(&run_output.stderr));
                    }
                }
            }
        }
        _ => {
            warn!("Running {} output not yet supported", target);
        }
    }
    
    Ok(())
}

fn validate_inputs(args: &CompileArgs) -> Result<()> {
    for input in &args.input {
        if !input.exists() {
            return Err(CliError::InvalidInput(format!("Input file does not exist: {}", input.display())));
        }
        
        if !input.is_file() {
            return Err(CliError::InvalidInput(format!("Input path is not a file: {}", input.display())));
        }
    }
    
    // Validate optimization level
    if args.opt_level > 3 {
        return Err(CliError::InvalidInput("Optimization level must be 0-3".to_string()));
    }
    
    Ok(())
}

fn determine_output_path(input: &Path, output: Option<&PathBuf>, target: &str) -> Result<PathBuf> {
    if let Some(output) = output {
        Ok(output.clone())
    } else {
        let extension = match target {
            "rust" => "rs",
            "llvm" => "ll",
            "wasm" => "wasm",
            _ => return Err(CliError::InvalidInput(format!("Unknown target for output extension: {}", target))),
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