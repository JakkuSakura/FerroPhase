use std::fs;

use object::Object as _;
use object::ObjectSection as _;
use object::{Architecture, BinaryFormat, SectionKind};
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
        source_language: Some("cil".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

fn minimal_cil_add_program() -> &'static str {
    r#"
.assembly extern mscorlib {}
.assembly 'FerroPhase.Container' {}
.module 'FerroPhase.Container.exe'

.class public auto ansi FerroPhaseProgram extends [mscorlib]System.Object
{
  .method public hidebysig static int32 main() cil managed
  {
    .maxstack 8
    ldc.i4 3
    ldc.i4 4
    add
    ret
  }
}
"#
}

#[tokio::test]
async fn compile_cil_text_to_native_object() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.il");
    let output_file = temp_dir.path().join("main.o");
    fs::write(&input_file, minimal_cil_add_program()).unwrap();

    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(
        file.sections()
            .any(|section| section.kind() == SectionKind::Text)
    );
}
