//! Integration tests for the transpile command

use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

use fp_cli::commands::transpile::{transpile_command, TranspileArgs};
use fp_cli::cli::CliConfig;

#[tokio::test]
async fn test_transpile_typescript_with_structs() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.ts");
    
    // Create test input file
    let test_code = r#"
struct Point {
    x: f64,
    y: f64,
}

struct Color {
    r: u8,
    g: u8,
    b: u8,
}

fn main() {
    let origin = Point { x: 0.0, y: 0.0 };
    let red = Color { r: 255, g: 0, b: 0 };
    println!("Origin: ({}, {})", origin.x, origin.y);
}
"#;
    
    fs::write(&input_file, test_code).unwrap();
    
    // Prepare transpile arguments
    let args = TranspileArgs {
        input: vec![input_file],
        target: "typescript".to_string(),
        output: Some(output_file.clone()),
        const_eval: true,
        preserve_structs: true,
        type_defs: false,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation
    let result = transpile_command(args, &config).await;
    assert!(result.is_ok(), "Transpilation should succeed");
    
    // Verify output file exists
    assert!(output_file.exists(), "Output file should be created");
    
    // Read and verify output content
    let output_content = fs::read_to_string(&output_file).unwrap();
    
    // Check for TypeScript interfaces
    assert!(output_content.contains("interface Point"), "Should contain Point interface");
    assert!(output_content.contains("interface Color"), "Should contain Color interface");
    
    // Check for field definitions
    assert!(output_content.contains("x: number"), "Point should have x field");
    assert!(output_content.contains("y: number"), "Point should have y field");
    assert!(output_content.contains("r: number"), "Color should have r field");
    assert!(output_content.contains("g: number"), "Color should have g field");
    assert!(output_content.contains("b: number"), "Color should have b field");
    
    // Check for main function
    assert!(output_content.contains("function main()"), "Should contain main function");
    
    // Check for const values (from const evaluation)
    assert!(output_content.contains("const POINT_SIZE"), "Should contain computed const values");
}

#[tokio::test]
async fn test_transpile_javascript_with_structs() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.js");
    
    // Create test input file
    let test_code = r#"
struct Point {
    x: f64,
    y: f64,
}

fn main() {
    let origin = Point { x: 0.0, y: 0.0 };
    println!("Origin: ({}, {})", origin.x, origin.y);
}
"#;
    
    fs::write(&input_file, test_code).unwrap();
    
    // Prepare transpile arguments
    let args = TranspileArgs {
        input: vec![input_file],
        target: "javascript".to_string(),
        output: Some(output_file.clone()),
        const_eval: true,
        preserve_structs: true,
        type_defs: false,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation
    let result = transpile_command(args, &config).await;
    assert!(result.is_ok(), "Transpilation should succeed");
    
    // Verify output file exists
    assert!(output_file.exists(), "Output file should be created");
    
    // Read and verify output content
    let output_content = fs::read_to_string(&output_file).unwrap();
    
    // Check for JavaScript factory functions
    assert!(output_content.contains("function createPoint"), "Should contain Point factory function");
    
    // Check for main function
    assert!(output_content.contains("function main()"), "Should contain main function");
    
    // Check for struct instantiation
    assert!(output_content.contains("createPoint(0.0, 0.0)"), "Should use factory function for struct creation");
}

#[tokio::test]
async fn test_transpile_typescript_with_type_definitions() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.ts");
    let types_file = temp_dir.path().join("test.d.ts");
    
    // Create test input file
    let test_code = r#"
struct User {
    id: u64,
    name: String,
    active: bool,
}

fn main() {
    let user = User { id: 1, name: "Alice".to_string(), active: true };
    println!("User: {}", user.name);
}
"#;
    
    fs::write(&input_file, test_code).unwrap();
    
    // Prepare transpile arguments
    let args = TranspileArgs {
        input: vec![input_file],
        target: "typescript".to_string(),
        output: Some(output_file.clone()),
        const_eval: true,
        preserve_structs: true,
        type_defs: true,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation
    let result = transpile_command(args, &config).await;
    assert!(result.is_ok(), "Transpilation should succeed");
    
    // Verify both output files exist
    assert!(output_file.exists(), "Output file should be created");
    assert!(types_file.exists(), "Type definitions file should be created");
    
    // Read and verify type definitions content
    let types_content = fs::read_to_string(&types_file).unwrap();
    
    // Check for User interface
    assert!(types_content.contains("interface User"), "Should contain User interface");
    assert!(types_content.contains("id: number"), "User should have id field");
    assert!(types_content.contains("name: string"), "User should have name field");
    assert!(types_content.contains("active: boolean"), "User should have active field");
}

#[tokio::test]
async fn test_transpile_invalid_target() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    
    fs::write(&input_file, "fn main() {}").unwrap();
    
    let args = TranspileArgs {
        input: vec![input_file],
        target: "invalid_language".to_string(),
        output: None,
        const_eval: true,
        preserve_structs: true,
        type_defs: false,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation - should fail
    let result = transpile_command(args, &config).await;
    assert!(result.is_err(), "Should fail with invalid target language");
}

#[tokio::test]
async fn test_transpile_nonexistent_file() {
    let temp_dir = TempDir::new().unwrap();
    let nonexistent_file = temp_dir.path().join("nonexistent.fp");
    
    let args = TranspileArgs {
        input: vec![nonexistent_file],
        target: "typescript".to_string(),
        output: None,
        const_eval: true,
        preserve_structs: true,
        type_defs: false,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation - should fail
    let result = transpile_command(args, &config).await;
    assert!(result.is_err(), "Should fail with nonexistent input file");
}

#[tokio::test]
async fn test_transpile_multiple_files() {
    let temp_dir = TempDir::new().unwrap();
    let input_file1 = temp_dir.path().join("test1.fp");
    let input_file2 = temp_dir.path().join("test2.fp");
    
    // Create test input files
    fs::write(&input_file1, "struct Point { x: f64, y: f64, } fn main() {}").unwrap();
    fs::write(&input_file2, "struct Color { r: u8, g: u8, b: u8, } fn main() {}").unwrap();
    
    let args = TranspileArgs {
        input: vec![input_file1.clone(), input_file2.clone()],
        target: "typescript".to_string(),
        output: None, // Auto-generate output names
        const_eval: true,
        preserve_structs: true,
        type_defs: false,
        pretty: true,
        source_maps: false,
        watch: false,
    };
    
    let config = CliConfig::default();
    
    // Execute transpilation
    let result = transpile_command(args, &config).await;
    assert!(result.is_ok(), "Multi-file transpilation should succeed");
    
    // Verify output files exist
    let output_file1 = input_file1.with_extension("ts");
    let output_file2 = input_file2.with_extension("ts");
    
    assert!(output_file1.exists(), "First output file should be created");
    assert!(output_file2.exists(), "Second output file should be created");
}