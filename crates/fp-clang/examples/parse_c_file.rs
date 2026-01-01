//! Example: Parse a C file and display the LLVM IR

use fp_clang::{ClangParser, CompileOptions, Standard};

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
    println!("Module name: {}", module.module.get_name().to_str().unwrap_or(""));
    println!(
        "Target triple: {}",
        module
            .module
            .get_target_triple()
            .as_str()
            .to_string_lossy()
    );
    println!(
        "Data layout: {}",
        module
            .module
            .get_data_layout()
            .as_str()
            .to_string_lossy()
    );

    println!("\n=== Functions ===");
    for func in module.module.get_functions() {
        let name = func.get_name().to_str().unwrap_or("");
        println!(
            "  {} {} ({:?})",
            if func.get_type().is_var_arg() {
                "variadic"
            } else {
                "fixed"
            },
            name,
            func.get_linkage()
        );
        let ret = func
            .get_type()
            .get_return_type()
            .map(|ty| format!("{:?}", ty))
            .unwrap_or_else(|| "void".to_string());
        println!("    Return type: {}", ret);
        println!("    Parameters: {}", func.count_params());
        println!("    Basic blocks: {}", func.count_basic_blocks());
    }

    println!("\n=== Global Variables ===");
    for global in module.module.get_globals() {
        let name = global.get_name().to_str().unwrap_or("");
        println!("  {} ({:?})", name, global.get_linkage());
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
