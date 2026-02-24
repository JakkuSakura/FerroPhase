//! Integration tests for AST target emission through the compile command.

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_compile_args(input: std::path::PathBuf, output: std::path::PathBuf) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: None,
        target_cpu: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output),
        package_graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        save_intermediates: false,
        lossy: true,
        max_errors: 0,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

#[tokio::test]
async fn test_compile_target_typescript_with_structs() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.ts");

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

    let mut args = base_compile_args(input_file, output_file.clone());
    args.target = Some("typescript".to_string());

    let config = CliConfig::default();
    let result = compile_command(args, &config).await;
    assert!(result.is_ok(), "TypeScript target should succeed");

    assert!(output_file.exists(), "Output file should be created");

    let output_content = fs::read_to_string(&output_file).unwrap();
    assert!(output_content.contains("interface Point"));
    assert!(output_content.contains("interface Color"));
    assert!(output_content.contains("function main()"));
}

#[tokio::test]
async fn test_compile_target_javascript_with_structs() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.js");

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

    let mut args = base_compile_args(input_file, output_file.clone());
    args.target = Some("javascript".to_string());

    let config = CliConfig::default();
    let result = compile_command(args, &config).await;
    assert!(result.is_ok(), "JavaScript target should succeed");

    assert!(output_file.exists(), "Output file should be created");

    let output_content = fs::read_to_string(&output_file).unwrap();
    assert!(output_content.contains("function createPoint"));
    assert!(output_content.contains("function main()"));
}

#[tokio::test]
async fn test_compile_target_typescript_with_type_definitions() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.ts");
    let defs_file = temp_dir.path().join("test.d.ts");

    let test_code = r#"
struct User {
    name: String,
    age: i32,
}

fn main() {
    let user = User { name: "Alice", age: 30 };
    println!("{} {}", user.name, user.age);
}
"#;

    fs::write(&input_file, test_code).unwrap();

    let mut args = base_compile_args(input_file, output_file.clone());
    args.target = Some("typescript".to_string());
    args.type_defs = true;

    let config = CliConfig::default();
    let result = compile_command(args, &config).await;
    assert!(result.is_ok(), "TypeScript target should succeed");

    assert!(output_file.exists(), "Output file should be created");
    assert!(defs_file.exists(), "Type definitions file should be created");

    let defs_content = fs::read_to_string(&defs_file).unwrap();
    assert!(defs_content.contains("interface User"));
}

#[tokio::test]
async fn test_compile_target_invalid_target() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");

    let test_code = "fn main() { println!(\"hello\"); }";
    fs::write(&input_file, test_code).unwrap();

    let mut args = base_compile_args(input_file, temp_dir.path().join("out.invalid"));
    args.target = Some("invalid_target".to_string());

    let config = CliConfig::default();
    let result = compile_command(args, &config).await;
    assert!(result.is_err(), "Invalid target should fail");
}
