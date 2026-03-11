use std::fs;

use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

#[tokio::test]
async fn compile_jvm_jar_to_native_object() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("Hello.jar");
    let output_file = temp_dir.path().join("Hello.o");

    let program = fp_jvm::JvmProgram {
        class: fp_jvm::JvmClass {
            name: "Hello".to_string(),
            super_name: "java/lang/Object".to_string(),
            methods: vec![fp_jvm::JvmMethod {
                name: "add".to_string(),
                descriptor: "()I".to_string(),
                access_flags: 0x0009,
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
    let jar = fp_jvm::emit_executable_jar(&emitted, "Hello").unwrap();
    fs::write(&input_file, jar).unwrap();

    let args = CompileArgs {
        input: vec![input_file],
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
        source_language: Some("jvm-bytecode".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();
    let bytes = fs::read(&output_file).unwrap();
    assert!(bytes.starts_with(b"\x7fELF"));
}
