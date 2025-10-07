//! Integration tests for fp-clang

use fp_clang::{ClangParser, CompileOptions, Standard};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_parse_simple_c_file() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test.c");

    let c_code = r#"
int add(int a, int b) {
    return a + b;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module
    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "add");
    assert_eq!(module.functions[0].parameters.len(), 2);
}

#[test]
fn test_compile_with_standard() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_std.c");

    let c_code = r#"
#include <stdbool.h>

bool is_positive(int n) {
    return n > 0;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::C99);

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module with the function
    assert!(!module.functions.is_empty());
}

#[test]
fn test_compile_to_ir_text() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_text.c");

    let c_code = r#"
int multiply(int a, int b) {
    return a * b;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let ir_text = parser.compile_to_ir_text(&c_file, &options).unwrap();

    // Verify IR contains expected elements
    assert!(ir_text.contains("define"));
    assert!(ir_text.contains("multiply"));
    assert!(ir_text.contains("ret"));
}

#[test]
fn test_compile_with_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_opt.c");

    let c_code = r#"
int square(int x) {
    return x * x;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.optimization = Some("2".to_string());

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module
    assert!(!module.functions.is_empty());
    assert_eq!(module.functions[0].name, "square");
}

#[test]
fn test_variadic_function() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_variadic.c");

    let c_code = r#"
#include <stdarg.h>

int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    va_end(args);
    return total;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify variadic function
    assert_eq!(module.functions.len(), 1);
    assert!(module.functions[0].is_var_arg);
}

#[test]
fn test_cpp_compilation() {
    let temp_dir = TempDir::new().unwrap();
    let cpp_file = temp_dir.path().join("test.cpp");

    let cpp_code = r#"
class Calculator {
public:
    int add(int a, int b) {
        return a + b;
    }
};

extern "C" int calculate(int a, int b) {
    Calculator calc;
    return calc.add(a, b);
}
"#;

    fs::write(&cpp_file, cpp_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::Cxx11);

    let module = parser.parse_to_llvm_ir(&cpp_file, &options).unwrap();

    // Verify we got functions (C++ mangles names)
    assert!(!module.functions.is_empty());
}

#[test]
fn test_clang_version() {
    let parser = ClangParser::new().unwrap();
    let version = parser.version().unwrap();

    // Just verify we got some version string
    assert!(version.contains("clang") || version.contains("LLVM"));
}

#[test]
fn test_invalid_file() {
    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let result = parser.parse_to_llvm_ir(std::path::Path::new("nonexistent.c"), &options);
    assert!(result.is_err());
}

#[test]
fn test_compilation_error() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("error.c");

    // Invalid C code
    let c_code = r#"
int broken() {
    return undefined_variable;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let result = parser.parse_to_llvm_ir(&c_file, &options);
    assert!(result.is_err());
}
