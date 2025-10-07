//! Example: Parse a C file and display the LLVM IR

use fp_clang::{ClangParser, CompileOptions, Standard};
use std::path::Path;

fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Create a simple C file for testing
    let c_code = r#"
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(5, 3);
    printf("Result: %d\n", result);
    return 0;
}
"#;

    // Write to temporary file
    let temp_dir = tempfile::tempdir()?;
    let c_file = temp_dir.path().join("example.c");
    std::fs::write(&c_file, c_code)?;

    // Create parser
    let parser = ClangParser::new()?;

    // Show clang version
    println!("Using: {}", parser.version()?);
    println!();

    // Set up compilation options
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::C11);
    options.optimization = Some("0".to_string());
    options.debug = true;

    // Parse to LLVM IR
    println!("Parsing C file: {}", c_file.display());
    let module = parser.parse_to_llvm_ir(&c_file, &options)?;

    // Display module information
    println!("\n=== LLVM Module Information ===");
    println!("Module name: {}", module.name);
    println!("Source filename: {}", module.source_file_name);
    println!(
        "Target triple: {}",
        module.target_triple.as_deref().unwrap_or("unknown")
    );
    println!("Data layout: {:?}", module.data_layout);

    println!("\n=== Functions ===");
    for func in &module.functions {
        println!(
            "  {} {} ({:?})",
            if func.is_var_arg { "variadic" } else { "fixed" },
            func.name,
            func.linkage
        );
        println!("    Return type: {:?}", func.return_type);
        println!("    Parameters: {}", func.parameters.len());
        println!("    Basic blocks: {}", func.basic_blocks.len());
    }

    println!("\n=== Global Variables ===");
    for global in &module.global_vars {
        println!("  {} ({:?})", global.name, global.linkage);
    }

    // Get the IR as text
    let ir_text = parser.compile_to_ir_text(&c_file, &options)?;
    println!("\n=== LLVM IR (first 500 chars) ===");
    println!("{}", &ir_text.chars().take(500).collect::<String>());
    if ir_text.len() > 500 {
        println!("... ({} more characters)", ir_text.len() - 500);
    }

    Ok(())
}
