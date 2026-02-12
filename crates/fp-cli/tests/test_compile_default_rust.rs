//! Integration tests for default compile behavior.

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

#[tokio::test]
async fn test_compile_backend_rust() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.rs");

    let test_code = r#"
fn main() {
    println!("hello");
}
"#;
    fs::write(&input_file, test_code).unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Rust,
        target: None,
        emitter: EmitterKind::Llvm,
        target_triple: None,
        target_cpu: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
        package_graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 10,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    let config = CliConfig::default();
    let result = compile_command(args, &config).await;
    assert!(result.is_ok(), "Rust backend compilation should succeed");

    assert!(output_file.exists(), "Output file should be created");
    let output_content = fs::read_to_string(&output_file).unwrap();
    assert!(
        output_content.contains("fn main"),
        "Rust output should contain a main function"
    );
}
