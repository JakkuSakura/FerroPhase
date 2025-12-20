//! Integration tests for the backend transpile command.

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::transpile::{transpile_command, TranspileArgs};

#[tokio::test]
async fn test_transpile_defaults_to_rust_backend() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.rs");

    let test_code = r#"
fn main() {
    println!("hello");
}
"#;
    fs::write(&input_file, test_code).unwrap();

    let args = TranspileArgs {
        input: vec![input_file],
        target: None,
        output: Some(output_file.clone()),
        opt_level: 0,
        debug: false,
        release: false,
        save_intermediates: false,
        error_tolerance: false,
        max_errors: 10,
        source_language: None,
    };

    let config = CliConfig::default();
    let result = transpile_command(args, &config).await;
    assert!(result.is_ok(), "Backend transpilation should succeed");

    assert!(output_file.exists(), "Output file should be created");
    let output_content = fs::read_to_string(&output_file).unwrap();
    assert!(
        output_content.contains("fn main"),
        "Rust output should contain a main function"
    );
}

