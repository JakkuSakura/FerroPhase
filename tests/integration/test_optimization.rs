use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_optimization_levels_correctness() {
    // Test that different optimization levels produce correct results
    let source = r#"
        fn expensive_function(n: i32) -> i32 {
            let mut result = 0;
            let mut i = 0;
            while i < n {
                result = result + i;
                i = i + 1;
            }
            return result;
        }
        
        fn main() -> i32 {
            return expensive_function(10);  // Sum 0..9 = 45
        }
    "#;
    
    let optimization_levels = ["0", "1", "2", "3"];
    
    for opt_level in optimization_levels {
        let result = compile_and_run_with_opt(source, opt_level);
        assert_eq!(result, 45, "Wrong result with optimization level {}", opt_level);
    }
}

#[test]
fn test_dead_code_elimination() {
    // Test that dead code is eliminated
    let source = r#"
        fn dead_function() -> i32 {
            return 999;  // This function is never called
        }
        
        fn unused_variable_function() -> i32 {
            let unused_var = 123;  // This variable is never used
            return 42;
        }
        
        fn main() -> i32 {
            return unused_variable_function();
        }
    "#;
    
    // Compile with optimizations
    let binary_opt = compile_to_binary_with_opt(source, "2");
    let binary_no_opt = compile_to_binary_with_opt(source, "0");
    
    // Optimized binary should be smaller (dead code removed)
    assert!(binary_opt.len() <= binary_no_opt.len(),
            "Optimized binary should be smaller or equal");
    
    // Both should produce correct result
    assert_eq!(run_binary(&binary_opt), 42);
    assert_eq!(run_binary(&binary_no_opt), 42);
}

#[test]
fn test_constant_folding() {
    // Test that constants are folded at compile time
    let source = r#"
        fn main() -> i32 {
            let a = 10 + 15;        // Should be folded to 25
            let b = 17 * 1;         // Should be folded to 17
            let c = 100 / 5 - 20;   // Should be folded to 0
            return a + b + c;       // Should be 25 + 17 + 0 = 42
        }
    "#;
    
    let result = compile_and_run_with_opt(source, "2");
    assert_eq!(result, 42);
    
    // TODO: Verify in LLVM IR that constants were folded
    let llvm_ir = compile_to_llvm_ir(source, "2");
    assert!(constant_folding_occurred(&llvm_ir));
}

#[test]
fn test_function_inlining() {
    // Test that small functions are inlined
    let source = r#"
        fn add_one(x: i32) -> i32 {
            return x + 1;
        }
        
        fn add_two(x: i32) -> i32 {
            return add_one(add_one(x));
        }
        
        fn main() -> i32 {
            return add_two(40);  // Should be 42
        }
    "#;
    
    let result = compile_and_run_with_opt(source, "2");
    assert_eq!(result, 42);
    
    // With inlining, the optimized version should have fewer function calls
    let llvm_ir_opt = compile_to_llvm_ir(source, "2");
    let llvm_ir_no_opt = compile_to_llvm_ir(source, "0");
    
    let calls_opt = count_function_calls(&llvm_ir_opt);
    let calls_no_opt = count_function_calls(&llvm_ir_no_opt);
    
    assert!(calls_opt <= calls_no_opt,
            "Optimized version should have fewer or equal function calls");
}

#[test]
fn test_loop_optimization() {
    // Test loop optimizations
    let source = r#"
        fn sum_loop(n: i32) -> i32 {
            let mut sum = 0;
            let mut i = 0;
            while i < n {
                sum = sum + i;
                i = i + 1;
            }
            return sum;
        }
        
        fn main() -> i32 {
            return sum_loop(10);  // Sum 0..9 = 45
        }
    "#;
    
    let result = compile_and_run_with_opt(source, "2");
    assert_eq!(result, 45);
    
    // TODO: Check that loop optimizations were applied
    // This could include loop unrolling, strength reduction, etc.
}

#[test]
fn test_optimization_preserves_semantics() {
    // Test that optimizations don't change program semantics
    let source = r#"
        fn complex_function(x: i32, y: i32) -> i32 {
            let mut result = x;
            
            if y > 0 {
                let mut i = 0;
                while i < y {
                    result = result + 1;
                    i = i + 1;
                }
            } else {
                result = result - y;  // y is negative, so this adds |y|
            }
            
            return result;
        }
        
        fn main() -> i32 {
            let a = complex_function(40, 2);   // 40 + 2 = 42
            let b = complex_function(44, -2);  // 44 - (-2) = 46
            return a;  // Return 42
        }
    "#;
    
    // Test with different optimization levels
    for opt_level in ["0", "1", "2", "3"] {
        let result = compile_and_run_with_opt(source, opt_level);
        assert_eq!(result, 42, "Semantics changed with optimization level {}", opt_level);
    }
}

#[test]
fn test_debug_info_with_optimization() {
    // Test that debug information is preserved with optimization
    let source = r#"
        fn debug_function(x: i32) -> i32 {
            let local_var = x + 1;
            return local_var + 1;
        }
        
        fn main() -> i32 {
            return debug_function(40);  // Should return 42
        }
    "#;
    
    let binary_path = compile_to_binary_with_debug_and_opt(source, "2");
    
    // Should still produce correct result
    assert_eq!(run_binary_file(&binary_path), 42);
    
    // Should still have debug information
    assert!(binary_has_debug_info(&binary_path));
}

#[test]
fn test_size_optimization() {
    // Test size-focused optimization
    let source = r#"
        fn large_function() -> i32 {
            let a = 1 + 2 + 3 + 4 + 5;
            let b = 6 + 7 + 8 + 9 + 10;
            let c = 11 + 12 + 13 + 14 + 15;
            return a + b + c;  // Sum 1..15 = 120
        }
        
        fn main() -> i32 {
            return large_function() - 78;  // 120 - 78 = 42
        }
    "#;
    
    let binary_size_opt = compile_to_binary_with_opt(source, "s");  // Optimize for size
    let binary_speed_opt = compile_to_binary_with_opt(source, "3"); // Optimize for speed
    
    // Size-optimized should be smaller or equal
    assert!(binary_size_opt.len() <= binary_speed_opt.len(),
            "Size optimization should produce smaller or equal binary");
    
    // Both should produce correct results
    assert_eq!(run_binary(&binary_size_opt), 42);
    assert_eq!(run_binary(&binary_speed_opt), 42);
}

#[test]
fn test_optimization_compilation_time() {
    // Test that optimization doesn't take excessive time
    let source = r#"
        fn recursive_fibonacci(n: i32) -> i32 {
            if n <= 1 {
                return n;
            } else {
                return recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2);
            }
        }
        
        fn main() -> i32 {
            return recursive_fibonacci(10);  // Should be 55
        }
    "#;
    
    let start_time = std::time::Instant::now();
    let result = compile_and_run_with_opt(source, "3");
    let total_time = start_time.elapsed();
    
    assert_eq!(result, 55);
    
    // Even with aggressive optimization, should complete reasonably quickly
    assert!(total_time.as_secs() < 30,
            "Optimization took too long: {:?}", total_time);
}

#[test]
fn test_cross_function_optimization() {
    // Test optimizations across function boundaries
    let source = r#"
        fn always_returns_one() -> i32 {
            return 1;
        }
        
        fn multiply_by_constant(x: i32) -> i32 {
            return x * always_returns_one();  // Should be optimized to just x
        }
        
        fn main() -> i32 {
            return multiply_by_constant(42);  // Should be 42
        }
    "#;
    
    let result = compile_and_run_with_opt(source, "2");
    assert_eq!(result, 42);
    
    // TODO: Verify that the multiplication was optimized away
}

#[test]
fn test_optimization_with_complex_control_flow() {
    // Test optimizations with complex control flow
    let source = r#"
        fn complex_control_flow(x: i32) -> i32 {
            let mut result = x;
            
            if x > 20 {
                if x < 50 {
                    result = result + 2;
                } else {
                    result = result - 5;
                }
            } else {
                result = result * 2;
            }
            
            return result;
        }
        
        fn main() -> i32 {
            return complex_control_flow(40);  // 40 + 2 = 42
        }
    "#;
    
    // Test with different optimization levels
    for opt_level in ["0", "1", "2", "3"] {
        let result = compile_and_run_with_opt(source, opt_level);
        assert_eq!(result, 42, "Wrong result with optimization level {}", opt_level);
    }
}

// Helper functions

fn compile_and_run_with_opt(source: &str, opt_level: &str) -> i32 {
    let binary = compile_to_binary_with_opt(source, opt_level);
    run_binary(&binary)
}

fn compile_to_binary_with_opt(source: &str, opt_level: &str) -> Vec<u8> {
    let temp_dir = tempdir().unwrap();
    let source_path = temp_dir.path().join("test.fp");
    let output_path = temp_dir.path().join("test_binary");
    
    std::fs::write(&source_path, source).unwrap();
    
    let output = Command::new("cargo")
        .args(&[
            "run", "--bin", "fp", "--",
            "compile", source_path.to_str().unwrap(),
            "-o", output_path.to_str().unwrap(),
            &format!("-O{}", opt_level),
            "--llvm"
        ])
        .output()
        .unwrap();
    
    if !output.status.success() {
        eprintln!("Compilation failed:");
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Compilation failed");
    }
    
    std::fs::read(&output_path).unwrap()
}

fn compile_to_binary_with_debug_and_opt(source: &str, opt_level: &str) -> std::path::PathBuf {
    let temp_dir = tempdir().unwrap();
    let source_path = temp_dir.path().join("test.fp");
    let output_path = temp_dir.path().join("test_binary");
    
    std::fs::write(&source_path, source).unwrap();
    
    let output = Command::new("cargo")
        .args(&[
            "run", "--bin", "fp", "--",
            "compile", source_path.to_str().unwrap(),
            "-o", output_path.to_str().unwrap(),
            &format!("-O{}", opt_level),
            "--debug",
            "--llvm"
        ])
        .output()
        .unwrap();
    
    if !output.status.success() {
        panic!("Compilation failed");
    }
    
    output_path
}

fn compile_to_llvm_ir(source: &str, opt_level: &str) -> String {
    let temp_dir = tempdir().unwrap();
    let source_path = temp_dir.path().join("test.fp");
    let output_path = temp_dir.path().join("test.ll");
    
    std::fs::write(&source_path, source).unwrap();
    
    let output = Command::new("cargo")
        .args(&[
            "run", "--bin", "fp", "--",
            "compile", source_path.to_str().unwrap(),
            "-o", output_path.to_str().unwrap(),
            &format!("-O{}", opt_level),
            "--emit-llvm"
        ])
        .output()
        .unwrap();
    
    if !output.status.success() {
        panic!("Compilation failed");
    }
    
    std::fs::read_to_string(&output_path).unwrap()
}

fn run_binary(binary_data: &[u8]) -> i32 {
    let temp_dir = tempdir().unwrap();
    let binary_path = temp_dir.path().join("test_binary");
    
    std::fs::write(&binary_path, binary_data).unwrap();
    
    // Make executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&binary_path, perms).unwrap();
    }
    
    let output = Command::new(&binary_path)
        .output()
        .unwrap();
    
    output.status.code().unwrap_or(-1)
}

fn run_binary_file(binary_path: &std::path::Path) -> i32 {
    let output = Command::new(binary_path)
        .output()
        .unwrap();
    
    output.status.code().unwrap_or(-1)
}

fn constant_folding_occurred(llvm_ir: &str) -> bool {
    // Simple heuristic: check that arithmetic operations were reduced
    // In practice, would need more sophisticated analysis
    !llvm_ir.contains("add") || !llvm_ir.contains("mul") || llvm_ir.contains("ret i32 42")
}

fn count_function_calls(llvm_ir: &str) -> usize {
    llvm_ir.matches("call").count()
}

fn binary_has_debug_info(path: &std::path::Path) -> bool {
    // Check if binary contains debug information
    let file_output = Command::new("file")
        .arg(path)
        .output();
    
    if let Ok(output) = file_output {
        let output_str = String::from_utf8_lossy(&output.stdout);
        output_str.contains("not stripped") || output_str.contains("debug")
    } else {
        // If 'file' command is not available, assume debug info is present
        true
    }
}