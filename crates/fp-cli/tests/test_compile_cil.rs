use std::fs;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;
use tempfile::TempDir;

#[tokio::test]
async fn test_compile_cil_emits_text_artifact() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.il");

    fs::write(
        &input_file,
        r#"
fn add(x: i64, y: i64) -> i64 {
    x + y
}

fn main() -> i64 {
    let sum = add(40, 2);
    sum
}
"#,
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Cil,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: None,
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
        graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        link: false,
        save_intermediates: false,
        lossy: true,
        max_errors: 0,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let rendered = fs::read_to_string(&output_file).unwrap();
    assert!(rendered.contains("FerroPhase .NET backend"));
    assert!(
        rendered.contains(".method public hidebysig static int64 'add'(int64, int64) cil managed")
    );
    assert!(rendered.contains("call int64 FerroPhaseProgram::'add'(int64, int64)"));
    assert!(rendered.contains(".entrypoint"));
}
