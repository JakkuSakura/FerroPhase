use std::fs;

use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
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
        graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        link: false,
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
async fn compile_transpiles_jvm_class_to_native_object() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Hello.class");
    let output_file = temp_dir.path().join("Hello.o");

    let program = fp_jvm::JvmProgram {
        class: fp_jvm::JvmClass {
            name: "Hello".to_string(),
            super_name: "java/lang/Object".to_string(),
            methods: vec![fp_jvm::JvmMethod {
                name: "add".to_string(),
                descriptor: "()I".to_string(),
                access_flags: 0x0009, // public static
                code: fp_jvm::JvmCode {
                    max_stack: 2,
                    max_locals: 0,
                    instructions: vec![
                        fp_jvm::JvmInstr::IConst(40),
                        fp_jvm::JvmInstr::IConst(2),
                        fp_jvm::JvmInstr::IAdd,
                        fp_jvm::JvmInstr::IReturn,
                    ],
                },
            }],
        },
    };
    let emitted = fp_jvm::emit_class_files(&program).unwrap();
    fs::write(&input_file, &emitted[0].bytes).unwrap();

    let mut args = base_args(input_file, output_file.clone());
    args.source_language = Some("jvm-bytecode".to_string());

    compile_command(args, &CliConfig::default()).await.unwrap();
    let bytes = fs::read(&output_file).unwrap();
    assert!(bytes.starts_with(b"\x7fELF"));
}

#[tokio::test]
async fn compile_rejects_cil_transpile_placeholder() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Hello.dll");
    let output_file = temp_dir.path().join("Hello.o");
    fs::write(&input_file, b"dummy").unwrap();

    let mut args = base_args(input_file, output_file);
    args.source_language = Some("cil".to_string());

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("binary .dll/.exe -> native transpilation is not implemented yet")
    );
}
