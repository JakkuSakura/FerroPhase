use std::fs;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;
use tempfile::TempDir;

fn has_ilasm() -> bool {
    std::env::var_os("PATH").is_some_and(|path| {
        std::env::split_paths(&path)
            .map(|entry| entry.join("ilasm"))
            .any(|candidate| candidate.is_file())
    })
}

fn has_mono() -> bool {
    std::env::var_os("PATH").is_some_and(|path| {
        std::env::split_paths(&path)
            .map(|entry| entry.join("mono"))
            .any(|candidate| candidate.is_file())
    })
}

#[tokio::test]
async fn test_compile_dotnet_emits_pe_artifact() {
    if !has_ilasm() {
        eprintln!("skipping .NET assembly test: ilasm not available on PATH");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.exe");

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
        backend: BackendKind::Dotnet,
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
        package_graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
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

    let bytes = fs::read(&output_file).unwrap();
    assert!(bytes.starts_with(b"MZ"));
}

#[tokio::test]
async fn test_compile_dotnet_exec_runs_assembly() {
    if !has_ilasm() || !has_mono() {
        eprintln!("skipping .NET exec test: ilasm or mono not available on PATH");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.exe");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    0
}
"#,
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Dotnet,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: None,
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file),
        package_graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: true,
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
}
