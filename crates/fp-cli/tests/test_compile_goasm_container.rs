use std::fs;

use object::Object as _;
use object::ObjectSection as _;
use object::{Architecture, BinaryFormat, SectionKind};
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{compile_command, CompileArgs, EmitterKind};
use fp_cli::pipeline::BackendKind;

fn base_args(input: std::path::PathBuf, output: std::path::PathBuf, emitter: EmitterKind) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter,
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
        source_language: Some("goasm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

fn minimal_goasm_program() -> &'static str {
    r#"
#include \"textflag.h\"
// fp-goasm (amd64)

TEXT ·main(SB), NOSPLIT, $0-0
main_bb0:
    MOVQ $7, R10
    MOVQ R10, AX
    RET
"#
}

#[tokio::test]
async fn compile_goasm_input_to_native_object() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.s");
    let output_file = temp_dir.path().join("main.o");
    fs::write(&input_file, minimal_goasm_program()).unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Native);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(file.sections().any(|section| section.kind() == SectionKind::Text));
}

#[tokio::test]
async fn compile_goasm_input_to_urcl_text() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.s");
    let output_file = temp_dir.path().join("main.urcl");
    fs::write(&input_file, minimal_goasm_program()).unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Urcl);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("BITS 64"));
    assert!(text.contains(".function"));
}

