use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

use assert_cmd::Command;
use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;
use tempfile::TempDir;

#[tokio::test]
async fn test_compile_ebpf_emits_text_artifact() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.ebpf");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    0
}
"#,
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Ebpf,
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

    let rendered = fs::read_to_string(&output_file).unwrap();
    assert!(rendered.contains("FerroPhase eBPF backend"));
    assert!(rendered.contains(".section \"prog/main\""));
    assert!(rendered.contains("main:"));
}

#[tokio::test]
async fn test_compile_ebpf_emits_object_artifact() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.o");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    0
}
"#,
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Ebpf,
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
    assert_eq!(&bytes[..4], b"\x7FELF");
}

#[cfg(unix)]
#[test]
fn test_compile_ebpf_exec_uses_external_runtime() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.o");
    let runtime_script = temp_dir.path().join("runtime.sh");
    let marker_file = temp_dir.path().join("runtime.marker");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    0
}
"#,
    )
    .unwrap();

    fs::write(
        &runtime_script,
        format!(
            "#!/bin/sh\n[ -f \"$1\" ] || exit 7\nprintf '%s\n' \"$1\" > \"{}\"\n",
            marker_file.display()
        ),
    )
    .unwrap();
    let mut perms = fs::metadata(&runtime_script).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&runtime_script, perms).unwrap();

    Command::new(env!("CARGO_BIN_EXE_fp"))
        .arg("compile")
        .arg(input_file.as_os_str())
        .arg("--backend")
        .arg("ebpf")
        .arg("--exec")
        .arg("--output")
        .arg(output_file.as_os_str())
        .env("FP_EBPF_RUNTIME", &runtime_script)
        .assert()
        .success();

    let invoked_path = fs::read_to_string(&marker_file).unwrap();
    assert_eq!(invoked_path.trim(), output_file.display().to_string());
}

#[cfg(unix)]
#[test]
fn test_compile_ebpf_exec_passes_runtime_args() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.o");
    let runtime_script = temp_dir.path().join("runtime.sh");
    let marker_file = temp_dir.path().join("runtime.marker");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    0
}
"#,
    )
    .unwrap();

    fs::write(
        &runtime_script,
        format!(
            "#!/bin/sh\nprintf '%s\n' \"$1\" > \"{}\"\n[ -f \"$3\" ] || exit 7\n",
            marker_file.display()
        ),
    )
    .unwrap();
    let mut perms = fs::metadata(&runtime_script).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&runtime_script, perms).unwrap();

    Command::new(env!("CARGO_BIN_EXE_fp"))
        .arg("compile")
        .arg(input_file.as_os_str())
        .arg("--backend")
        .arg("ebpf")
        .arg("--exec")
        .arg("--output")
        .arg(output_file.as_os_str())
        .env("FP_EBPF_RUNTIME", &runtime_script)
        .env("FP_EBPF_RUNTIME_ARGS", "--flag before")
        .assert()
        .success();

    let first_arg = fs::read_to_string(&marker_file).unwrap();
    assert_eq!(first_arg.trim(), "--flag");
}

#[test]
fn test_compile_ebpf_exec_uses_workspace_runtime() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("test.fp");
    let output_file = temp_dir.path().join("test.o");

    fs::write(
        &input_file,
        r#"
fn main() -> i32 {
    0
}
"#,
    )
    .unwrap();

    Command::new(env!("CARGO_BIN_EXE_fp"))
        .arg("compile")
        .arg(input_file.as_os_str())
        .arg("--backend")
        .arg("ebpf")
        .arg("--exec")
        .arg("--output")
        .arg(output_file.as_os_str())
        .env(
            "FP_EBPF_RUNTIME",
            std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string()),
        )
        .env(
            "FP_EBPF_RUNTIME_ARGS",
            "run -q -p fp-ebpf --bin fp-ebpf-runtime --",
        )
        .assert()
        .success();
}
