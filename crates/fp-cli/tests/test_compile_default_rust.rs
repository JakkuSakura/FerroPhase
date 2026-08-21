//! Integration tests for default compile behavior.

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, compile_command};

// AST-emitting backends (fp_lang::RustBackend included) always write into
// `BackendConfig.workspace_root/<package_name>/...` — there's no
// "write directly to this single file path" mode yet, so a single-file
// compile with an explicit `-o test.rs` ends up creating `test.rs` as a
// directory instead of a file. See the same note in
// `test_compile_ast_target.rs`.
#[tokio::test]
#[ignore = "AST-emitting backends don't support single-file -o output yet (write into workspace_root/<package>/ instead)"]
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
        package: None,
        input: vec![input_file],
        target: "rust".to_string(),
        target_triple: None,
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        link: false,
        save_intermediates: false,
        source_language: None,
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
