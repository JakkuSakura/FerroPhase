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
async fn compile_command_supports_dotnet_backend() {
    if !has_ilasm() || !has_mono() {
        eprintln!("skipping dotnet run test: ilasm or mono not available on PATH");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");

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
        output: None,
        graph: None,
        opt_level: 2,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: true,
        link: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 50,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();
}

#[tokio::test]
async fn compile_command_supports_dotnet_backend_with_dll_output() {
    if !has_ilasm() || !has_mono() {
        eprintln!("skipping dotnet run test: ilasm or mono not available on PATH");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("app.dll");

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
        output: Some(output_file.clone()),
        graph: None,
        opt_level: 3,
        debug: true,
        release: true,
        include: Vec::new(),
        define: Vec::new(),
        exec: true,
        link: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 50,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();
    assert!(output_file.exists());
}
