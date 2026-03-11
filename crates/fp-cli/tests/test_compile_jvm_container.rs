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
        emitter: EmitterKind::Native,
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
        source_language: Some("jvm-bytecode".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

#[tokio::test]
async fn compile_jvm_classfile_roundtrips() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Main.class");
    let output_file = temp_dir.path().join("Main.out.class");

    let bytes = vec![0xCA, 0xFE, 0xBA, 0xBE, 0, 0, 0, 52];
    fs::write(&input_file, &bytes).unwrap();

    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let out = fs::read(&output_file).unwrap();
    assert_eq!(out, bytes);
}
