use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_args(
    input: std::path::PathBuf,
    output: std::path::PathBuf,
    emitter: EmitterKind,
    native_target: Option<&str>,
) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter,
        target_triple: None,
        target_cpu: None,
        native_target: native_target.map(ToString::to_string),
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
async fn compile_urcl_emits_text_output() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.urcl");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    42
}
"#,
    )
    .unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Urcl, None);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("BITS 64"));
    assert!(text.contains(".function main"));
}

#[tokio::test]
async fn compile_goasm_emits_text_output() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.s");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Goasm, None);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("TEXT ·main(SB), NOSPLIT, $0-0"));
    assert!(text.contains("RET"));
}

#[tokio::test]
async fn compile_urcl_uses_default_urcl_extension() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    9
}
"#,
    )
    .unwrap();

    let args = base_args(
        input_file.clone(),
        temp_dir.path().join("ignored.out"),
        EmitterKind::Urcl,
        None,
    );
    let args = CompileArgs {
        output: None,
        ..args
    };
    compile_command(args, &CliConfig::default()).await.unwrap();

    let output_file = input_file.with_extension("urcl");
    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("BITS 64"));
}

#[tokio::test]
async fn compile_goasm_emitter_rejects_exec() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.s");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let mut args = base_args(input_file, output_file, EmitterKind::Goasm, None);
    args.exec = true;

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("--exec is not supported for text assembly emitters")
    );
}

#[tokio::test]
async fn compile_urcl_emitter_rejects_exec() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.urcl");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let mut args = base_args(input_file, output_file, EmitterKind::Urcl, None);
    args.exec = true;

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("--exec is not supported for text assembly emitters")
    );
}
