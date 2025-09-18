use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_const_arithmetic_evaluation() {
    // Test that const arithmetic is evaluated at compile time
    let source = r#"
        const MULTIPLIER: i32 = 2;
        const BASE: i32 = 21;
        const RESULT: i32 = BASE * MULTIPLIER;
        
        fn main() -> i32 {
            return RESULT;  // Should return 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
    
    // TODO: Add test to verify this was computed at compile time, not runtime
    // This could be done by checking the generated LLVM IR or binary
}

#[test]
fn test_const_boolean_evaluation() {
    // Test const boolean expressions
    let source = r#"
        const TRUE_VAL: bool = true;
        const FALSE_VAL: bool = false;
        const AND_RESULT: bool = TRUE_VAL && FALSE_VAL;
        const OR_RESULT: bool = TRUE_VAL || FALSE_VAL;
        
        fn main() -> i32 {
            if OR_RESULT && !AND_RESULT {
                return 42;
            } else {
                return 0;
            }
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_conditional_evaluation() {
    // Test const if expressions
    let source = r#"
        const CONDITION: bool = true;
        const VALUE: i32 = if CONDITION { 42 } else { 0 };
        
        fn main() -> i32 {
            return VALUE;
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_function_evaluation() {
    // Test const function calls
    let source = r#"
        const fn double(x: i32) -> i32 {
            return x * 2;
        }
        
        const fn add(a: i32, b: i32) -> i32 {
            return a + b;
        }
        
        const RESULT: i32 = add(double(10), double(11));
        
        fn main() -> i32 {
            return RESULT;  // Should be (10*2) + (11*2) = 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_recursive_evaluation() {
    // Test const recursive functions
    let source = r#"
        const fn factorial(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }
        
        const FACTORIAL_5: i32 = factorial(5);
        
        fn main() -> i32 {
            return FACTORIAL_5;  // Should be 120
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 120);
}

#[test]
fn test_const_array_evaluation() {
    // Test const array operations
    let source = r#"
        const ARRAY: [i32; 3] = [10, 20, 12];
        const SUM: i32 = ARRAY[0] + ARRAY[1] + ARRAY[2];
        
        fn main() -> i32 {
            return SUM;  // Should be 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_struct_evaluation() {
    // Test const struct operations
    let source = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        const ORIGIN: Point = Point { x: 0, y: 0 };
        const POINT: Point = Point { x: 20, y: 22 };
        const DISTANCE_SQ: i32 = (POINT.x - ORIGIN.x) * (POINT.x - ORIGIN.x) + 
                                 (POINT.y - ORIGIN.y) * (POINT.y - ORIGIN.y);
        
        fn main() -> i32 {
            return POINT.x + POINT.y;  // Should be 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_string_evaluation() {
    // Test const string operations
    let source = r#"
        const MESSAGE: &str = "Hello, World!";
        const LENGTH: usize = MESSAGE.len();
        
        fn main() -> i32 {
            return LENGTH as i32;  // "Hello, World!" has 13 characters
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 13);
}

#[test]
fn test_const_evaluation_errors() {
    // Test that const evaluation errors are caught at compile time
    let source = r#"
        const fn divide(a: i32, b: i32) -> i32 {
            return a / b;
        }
        
        const INVALID: i32 = divide(42, 0);  // Division by zero
        
        fn main() -> i32 {
            return INVALID;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Should fail at compile time
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("division by zero") ||
            compilation_result.stderr.contains("const") ||
            compilation_result.stderr.contains("evaluation"));
}

#[test]
fn test_const_evaluation_limits() {
    // Test const evaluation with reasonable limits
    let source = r#"
        const fn fibonacci(n: i32) -> i32 {
            if n <= 1 {
                return n;
            } else {
                return fibonacci(n - 1) + fibonacci(n - 2);
            }
        }
        
        const FIB_10: i32 = fibonacci(10);
        
        fn main() -> i32 {
            return FIB_10;  // Should be 55
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 55);
}

#[test]
fn test_const_evaluation_infinite_loop_detection() {
    // Test that infinite loops in const evaluation are detected
    let source = r#"
        const fn infinite_loop() -> i32 {
            return infinite_loop();
        }
        
        const NEVER_COMPUTED: i32 = infinite_loop();
        
        fn main() -> i32 {
            return NEVER_COMPUTED;
        }
    "#;
    
    let compilation_result = attempt_compilation(source);
    
    // Should fail at compile time due to infinite recursion
    assert!(!compilation_result.success);
    assert!(compilation_result.stderr.contains("infinite") ||
            compilation_result.stderr.contains("recursion") ||
            compilation_result.stderr.contains("loop") ||
            compilation_result.stderr.contains("const"));
}

#[test]
fn test_mixed_const_and_runtime() {
    // Test mixing const and runtime evaluation
    let source = r#"
        const COMPILE_TIME_VALUE: i32 = 20;
        
        fn runtime_function(x: i32) -> i32 {
            return x + 22;
        }
        
        fn main() -> i32 {
            let runtime_value = runtime_function(COMPILE_TIME_VALUE);
            return runtime_value;  // Should be 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_in_type_context() {
    // Test const evaluation in type contexts (array sizes, etc.)
    let source = r#"
        const ARRAY_SIZE: usize = 3;
        
        fn main() -> i32 {
            let array: [i32; ARRAY_SIZE] = [10, 20, 12];
            return array[0] + array[1] + array[2];  // Should be 42
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
}

#[test]
fn test_const_optimization_integration() {
    // Test that const evaluation integrates with other optimizations
    let source = r#"
        const BASE: i32 = 40;
        
        fn add_two(x: i32) -> i32 {
            return x + 2;
        }
        
        fn main() -> i32 {
            // This should be optimized to just return 42
            return add_two(BASE);
        }
    "#;
    
    let result = compile_and_run(source);
    assert_eq!(result, 42);
    
    // TODO: Verify in generated code that this was optimized
}

#[test]
fn test_const_evaluation_performance() {
    // Test const evaluation performance with complex computations
    let source = r#"
        const fn complex_computation(n: i32) -> i32 {
            let mut result = 0;
            let mut i = 0;
            while i < n {
                result = result + i * i;
                i = i + 1;
            }
            return result;
        }
        
        const RESULT: i32 = complex_computation(10);
        
        fn main() -> i32 {
            return RESULT;  // Sum of squares 0^2 + 1^2 + ... + 9^2 = 285
        }
    "#;
    
    let start_time = std::time::Instant::now();
    let result = compile_and_run(source);
    let total_time = start_time.elapsed();
    
    assert_eq!(result, 285);
    
    // Const evaluation should complete in reasonable time
    assert!(total_time.as_secs() < 10,
            "Const evaluation took too long: {:?}", total_time);
}

// Helper functions

fn compile_and_run(source: &str) -> i32 {
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
    
    if !output.status.success() {
        eprintln!("Compilation failed:");
        eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Compilation failed");
    }
    
    let execution_output = Command::new(&output_path)
        .output()
        .unwrap();
    
    execution_output.status.code().unwrap_or(-1)
}

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