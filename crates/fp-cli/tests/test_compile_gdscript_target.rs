//! Integration test for GDScript target emission through the compile command.

use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, compile_command};

fn base_compile_args(input: std::path::PathBuf, output: std::path::PathBuf) -> CompileArgs {
    CompileArgs {
        package: None,
        input: input,
        target: "native".to_string(),
        target_triple: None,
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output),
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
    }
}

#[tokio::test]
async fn test_compile_target_gdscript_with_struct() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.gd");

    let test_code = r#"
struct User {
    name: String,
    age: i64,
}

fn main() {
    let user = User { name: String::new(), age: 30 };
    println!("{}", user.name);
}
"#;

    fs::write(&input_file, test_code).unwrap();

    let mut args = base_compile_args(input_file, output_file.clone());
    args.target = "gdscript".to_string();

    let config = CliConfig::default();
    if let Err(err) = compile_command(args, &config).await {
        panic!("GDScript target should succeed: {err}");
    }

    assert!(output_file.exists(), "Output file should be created");
    let output_content = fs::read_to_string(&output_file).unwrap();
    assert!(output_content.contains("class User:"));
    assert!(output_content.contains("func main("));
}
