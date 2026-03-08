use std::fs;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::EmitterKind;
use fp_cli::commands::run::{RunArgs, RunMode, run_command};
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
async fn run_command_supports_dotnet_backend() {
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

    let args = RunArgs {
        file: input_file,
        mode: RunMode::Compile,
        backend: BackendKind::Dotnet,
        emitter: EmitterKind::Native,
        native_target: None,
        output: None,
        opt_level: 2,
        debug: false,
        release: false,
    };

    run_command(args, &CliConfig::default()).await.unwrap();
}

#[tokio::test]
async fn run_command_supports_dotnet_backend_with_dll_output() {
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

    let args = RunArgs {
        file: input_file,
        mode: RunMode::Compile,
        backend: BackendKind::Dotnet,
        emitter: EmitterKind::Native,
        native_target: None,
        output: Some(output_file.clone()),
        opt_level: 3,
        debug: true,
        release: true,
    };

    run_command(args, &CliConfig::default()).await.unwrap();
    assert!(output_file.exists());
}
