//! Compilation command implementation

use crate::{cli::CliConfig, utils::file_utils, Result, CliError};
use console::style;
use fp_core::ast::*;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust_lang::{parser::RustParser, printer::RustPrinter};
use indicatif::{ProgressBar, ProgressStyle};
use std::path::{Path, PathBuf};
use std::sync::Arc;
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
    
    for (i, input_file) in args.input.iter().enumerate() {
        progress.set_message(format!("Compiling {}", input_file.display()));
        
        let output_file = determine_output_path(input_file, &args.output, &args.target)?;
        
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
    config: &CliConfig,
) -> Result<()> {
    // Read input file
    let source_code = std::fs::read_to_string(input)
        .map_err(|e| CliError::Io(e))?;
    
    // Parse into AST
    let parser = RustParser::new();
    let ast = parser.parse_file(&source_code)
        .map_err(|e| CliError::Compilation(format!("Parse error: {}", e)))?;
    
    // Apply optimizations if opt_level > 0
    let optimized_ast = if args.opt_level > 0 {
        apply_optimizations(ast, args, config).await?
    } else {
        ast
    };
    
    // Generate output based on target
    match args.target.as_str() {
        "rust" => compile_to_rust(optimized_ast, output).await?,
        "llvm" => compile_to_llvm(optimized_ast, output).await?,
        "wasm" => compile_to_wasm(optimized_ast, output).await?,
        "interpret" => interpret_code(optimized_ast).await?,
        _ => return Err(CliError::InvalidInput(format!("Unknown target: {}", args.target))),
    }
    
    Ok(())
}

async fn apply_optimizations(
    ast: AstFile,
    args: &CompileArgs,
    config: &CliConfig,
) -> Result<AstFile> {
    info!("Applying optimizations (level {})", args.opt_level);
    
    let printer = Arc::new(RustPrinter::new());
    let interpreter = InterpretationOrchestrator::new(printer.clone());
    
    // This is a simplified optimization pipeline
    // In a real implementation, you'd have more sophisticated passes
    
    match args.opt_level {
        1 => {
            // Basic optimizations
            info!("Running basic optimizations");
        }
        2 => {
            // Standard optimizations
            info!("Running standard optimizations");
        }
        3 => {
            // Aggressive optimizations
            info!("Running aggressive optimizations");
        }
        _ => {
            // No optimizations
        }
    }
    
    // For now, just return the original AST
    // TODO: Implement actual optimization passes
    Ok(ast)
}

async fn compile_to_rust(ast: AstFile, output: &Path) -> Result<()> {
    let printer = RustPrinter::new();
    let rust_code = printer.print_file(&ast)
        .map_err(|e| CliError::Compilation(format!("Rust generation error: {}", e)))?;
    
    // Ensure output directory exists
    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| CliError::Io(e))?;
    }
    
    std::fs::write(output, rust_code)
        .map_err(|e| CliError::Io(e))?;
    
    info!("Generated Rust code: {}", output.display());
    Ok(())
}

async fn compile_to_llvm(_ast: AstFile, _output: &Path) -> Result<()> {
    // TODO: Implement LLVM compilation
    Err(CliError::Compilation("LLVM compilation not yet implemented".to_string()))
}

async fn compile_to_wasm(_ast: AstFile, _output: &Path) -> Result<()> {
    // TODO: Implement WebAssembly compilation
    Err(CliError::Compilation("WebAssembly compilation not yet implemented".to_string()))
}

async fn interpret_code(ast: AstFile) -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    let interpreter = InterpretationOrchestrator::new(printer);
    
    // For now, just print that we would interpret
    // TODO: Implement actual interpretation
    println!("{} Code interpretation not yet fully implemented", style("ℹ").blue());
    println!("AST contains {} items", ast.items.len());
    
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