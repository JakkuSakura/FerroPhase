use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_args(input: std::path::PathBuf, output: std::path::PathBuf) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::JvmBytecode,
        target: None,
        emitter: EmitterKind::Llvm,
        target_triple: None,
        target_cpu: None,
        native_target: None,
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
        lossy: false,
        max_errors: 10,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

#[tokio::test]
async fn compile_jvm_class_emits_classfile() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("demo-app.fp");
    let output_file = temp_dir.path().join("demo-app.class");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    7
}
"#,
    )
    .unwrap();

    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert_eq!(&bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
}

#[tokio::test]
async fn compile_jvm_jar_with_save_intermediates_emits_jar_and_class() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("hello-world.fp");
    let output_file = temp_dir.path().join("hello-world.jar");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    7
}
"#,
    )
    .unwrap();

    let mut args = base_args(input_file, output_file.clone());
    args.save_intermediates = true;
    compile_command(args, &CliConfig::default()).await.unwrap();

    let jar_bytes = fs::read(&output_file).unwrap();
    assert_eq!(&jar_bytes[0..4], b"PK\x03\x04");
    assert!(jar_bytes.windows("META-INF/MANIFEST.MF".len()).any(|w| w == b"META-INF/MANIFEST.MF"));

    let class_output = temp_dir.path().join("hello-world.class");
    let class_bytes = fs::read(&class_output).unwrap();
    assert_eq!(&class_bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
}
