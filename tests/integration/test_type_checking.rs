use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_type_mismatch_error() {
    // Test that type mismatches are caught during compilation
    let source = r#"
        fn main() -> i32 {
            let x: i32 = "hello"; // Type error: string literal to i32
            return x;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Compilation should fail
    assert!(!compilation_result.success);
    
    // Error message should mention type mismatch
    assert!(compilation_result.stderr.contains("type"));
    assert!(compilation_result.stderr.contains("mismatch") || 
            compilation_result.stderr.contains("error"));
    
    // Should have source location information
    assert!(compilation_result.stderr.contains("main"));
}

#[test]
fn test_undefined_variable_error() {
    // Test that undefined variables are caught
    let source = r#"
        fn main() -> i32 {
            return undefined_variable;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("undefined") ||
            compilation_result.stderr.contains("not found") ||
            compilation_result.stderr.contains("unresolved"));
}

#[test]
fn test_function_argument_type_checking() {
    // Test function argument type checking
    let source = r#"
        fn add_numbers(a: i32, b: i32) -> i32 {
            return a + b;
        }
        
        fn main() -> i32 {
            return add_numbers(42, "not a number"); // Type error
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("type"));
    assert!(compilation_result.stderr.contains("add_numbers") ||
            compilation_result.stderr.contains("argument"));
}

#[test]
fn test_return_type_checking() {
    // Test return type checking
    let source = r#"
        fn get_number() -> i32 {
            return "not a number"; // Type error in return
        }
        
        fn main() -> i32 {
            return get_number();
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("return") ||
            compilation_result.stderr.contains("type"));
}

#[test]
fn test_struct_field_type_checking() {
    // Test struct field type checking
    let source = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        fn main() -> i32 {
            let p = Point { x: 10, y: "not a number" }; // Type error
            return p.x;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("Point") ||
            compilation_result.stderr.contains("field") ||
            compilation_result.stderr.contains("type"));
}

#[test]
fn test_binary_operation_type_checking() {
    // Test binary operation type checking
    let source = r#"
        fn main() -> i32 {
            let result = 42 + "string"; // Type error in addition
            return result;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("type") ||
            compilation_result.stderr.contains("operator") ||
            compilation_result.stderr.contains("addition"));
}

#[test]
fn test_valid_type_inference() {
    // Test that valid type inference works
    let source = r#"
        fn main() -> i32 {
            let x = 42;        // Should infer i32
            let y = x + 8;     // Should also be i32
            return y;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Should compile successfully
    assert!(compilation_result.success, 
            "Type inference failed: {}", compilation_result.stderr);
}

#[test]
fn test_generic_type_checking() {
    // Test generic type checking (if supported)
    let source = r#"
        fn identity<T>(x: T) -> T {
            return x;
        }
        
        fn main() -> i32 {
            let result = identity(42);
            return result;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Should either compile successfully or give a clear error about generics
    if !compilation_result.success {
        // If generics aren't supported yet, error should be clear
        assert!(compilation_result.stderr.contains("generic") ||
                compilation_result.stderr.contains("not supported"));
    }
}

#[test]
fn test_recursive_type_checking() {
    // Test recursive function type checking
    let source = r#"
        fn fibonacci(n: i32) -> i32 {
            if n <= 1 {
                return n;
            } else {
                return fibonacci(n - 1) + fibonacci(n - 2);
            }
        }
        
        fn main() -> i32 {
            return fibonacci(10);
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Should compile successfully
    assert!(compilation_result.success,
            "Recursive function type checking failed: {}", compilation_result.stderr);
}

#[test]
fn test_multiple_type_errors() {
    // Test that multiple type errors are reported
    let source = r#"
        fn main() -> i32 {
            let x: i32 = "error1";     // First type error
            let y: bool = 42;          // Second type error
            return x + y;              // Third type error (if reached)
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    
    // Should report multiple errors or at least the first one clearly
    assert!(compilation_result.stderr.contains("type"));
    
    // Count error occurrences (simple heuristic)
    let error_count = compilation_result.stderr.matches("error").count();
    assert!(error_count >= 1, "Should report at least one type error");
}

#[test]
fn test_error_recovery() {
    // Test that compiler can recover from errors and continue checking
    let source = r#"
        fn function_with_error() -> i32 {
            let x: i32 = "type error";
            return x;
        }
        
        fn valid_function() -> i32 {
            return 42;
        }
        
        fn main() -> i32 {
            return valid_function();
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    assert!(!compilation_result.success);
    
    // Should report the type error but also process the rest of the file
    assert!(compilation_result.stderr.contains("function_with_error") ||
            compilation_result.stderr.contains("type"));
}

#[test]
fn test_type_checking_performance() {
    // Test type checking with a larger program
    let source = r#"
        struct Point { x: i32, y: i32 }
        struct Line { start: Point, end: Point }
        
        fn create_point(x: i32, y: i32) -> Point {
            return Point { x: x, y: y };
        }
        
        fn create_line(x1: i32, y1: i32, x2: i32, y2: i32) -> Line {
            let start = create_point(x1, y1);
            let end = create_point(x2, y2);
            return Line { start: start, end: end };
        }
        
        fn line_length_squared(line: Line) -> i32 {
            let dx = line.end.x - line.start.x;
            let dy = line.end.y - line.start.y;
            return dx * dx + dy * dy;
        }
        
        fn main() -> i32 {
            let line = create_line(0, 0, 3, 4);
            return line_length_squared(line);  // Should be 25
        }
    "#;
    
    let start_time = std::time::Instant::now();
    let compilation_result = attempt_compilation(source);
    let compile_time = start_time.elapsed();
    
    // Should compile successfully
    assert!(compilation_result.success,
            "Complex type checking failed: {}", compilation_result.stderr);
    
    // Should complete in reasonable time (less than 5 seconds for this small program)
    assert!(compile_time.as_secs() < 5,
            "Type checking took too long: {:?}", compile_time);
}

// Helper structures and functions

struct CompilationResult {
    success: bool,
    stdout: String,
    stderr: String,
}

fn attempt_compilation(source: &str) -> CompilationResult {
    let temp_dir = tempdir().unwrap();
    let source_path = temp_dir.path().join("test.fp");
    let output_path = temp_dir.path().join("test_binary");
    
    std::fs::write(&source_path, source).unwrap();
    
    let output = Command::new("cargo")
        .args(&[
            "run", "--bin", "fp", "--",
            "compile", source_path.to_str().unwrap(),
            "-o", output_path.to_str().unwrap(),
            "--llvm"
        ])
        .output()
        .unwrap();
    
    CompilationResult {
        success: output.status.success(),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    }
}