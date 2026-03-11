use std::fs;

use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{compile_command, CompileArgs, EmitterKind};
use fp_cli::pipeline::BackendKind;

fn base_args(input: std::path::PathBuf, output: std::path::PathBuf) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("x86_64-unknown-linux-gnu".to_string()),
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

// NOTE: URCL input is now supported (see `test_compile_urcl_container.rs`).

#[tokio::test]
async fn compile_rejects_jvm_bytecode_transpile_placeholder() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Hello.class");
    let output_file = temp_dir.path().join("Hello.o");
    fs::write(&input_file, b"dummy").unwrap();

    let mut args = base_args(input_file, output_file);
    args.source_language = Some("jvm-bytecode".to_string());

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(err
        .to_string()
        .contains("JVM bytecode input currently supports only `--backend jvm-bytecode`"));
}

#[tokio::test]
async fn compile_rejects_cil_transpile_placeholder() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Hello.il");
    let output_file = temp_dir.path().join("Hello.o");
    fs::write(&input_file, b"dummy").unwrap();

    let mut args = base_args(input_file, output_file);
    args.source_language = Some("cil".to_string());

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(err
        .to_string()
        .contains("CIL input currently supports only `--backend cil` or `--backend dotnet`"));
}

