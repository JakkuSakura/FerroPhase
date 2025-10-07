//! Example: Extract function declarations from C header files

use fp_clang::{ClangCodegen, CompileOptions};

fn main() -> anyhow::Result<()> {
    // Create a sample C file with function definitions
    let c_code = r#"
#include <stdarg.h>

// Math functions
int add(int a, int b) {
    return a + b;
}

int subtract(int a, int b) {
    return a - b;
}

double multiply(double a, double b) {
    return a * b;
}

// Variadic function
void log_message(const char* format, ...) {
    // Implementation would go here
}

// Struct operations
typedef struct {
    int x;
    int y;
} Point;

Point create_point(int x, int y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}
"#;

    // Write to temporary file
    let temp_dir = tempfile::tempdir()?;
    let c_file = temp_dir.path().join("mylib.c");
    std::fs::write(&c_file, c_code)?;

    // Create codegen
    let codegen = ClangCodegen::new()?;

    // Extract declarations (using the c_file instead of header since it has definitions)
    let options = CompileOptions::default();
    println!("Extracting function signatures from: {}", c_file.display());
    println!();

    let signatures = codegen.extract_declarations(&c_file, &options)?;

    println!(
        "=== Extracted {} Function Declarations ===\n",
        signatures.len()
    );

    for sig in &signatures {
        println!("{};", sig.to_declaration());
    }

    println!("\n=== Summary ===");
    println!("Total functions: {}", signatures.len());

    let variadic_count = signatures.iter().filter(|s| s.is_variadic).count();
    if variadic_count > 0 {
        println!("Variadic functions: {}", variadic_count);
    }

    Ok(())
}
