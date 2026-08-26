use std::fs;

use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, compile_command};

fn base_args(input: std::path::PathBuf, output: std::path::PathBuf, target: &str) -> CompileArgs {
    CompileArgs {
        package: None,
        input: input,
        target: target.to_string(),
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
        source_language: Some("cil".to_string()),
        type_defs: false,
        single_world: false,
    }
}

fn minimal_cil_program() -> &'static str {
    r#"
.assembly extern mscorlib {}
.assembly 'FerroPhase.Container' {}
.module 'FerroPhase.Container.exe'

.class public auto ansi FerroPhaseProgram extends [mscorlib]System.Object
{
  .method public hidebysig static void Main() cil managed
  {
    .entrypoint
    .maxstack 8
    ret
  }
}
"#
}

#[tokio::test]
async fn compile_cil_text_to_dotnet_assembly() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.il");
    let output_file = temp_dir.path().join("main.exe");
    fs::write(&input_file, minimal_cil_program()).unwrap();

    let args = base_args(input_file, output_file.clone(), "dotnet");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(bytes.starts_with(b"MZ"));
}
