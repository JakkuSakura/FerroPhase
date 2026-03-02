use std::fs;
use std::process::Command;

#[test]
fn compile_command_generates_script_file() {
    let bin = env!("CARGO_BIN_EXE_fp-shell");
    let directory = tempfile::tempdir().expect("tempdir should be created");
    let input = directory.path().join("deploy.fp");
    let output = directory.path().join("deploy.sh");

    fs::write(
        &input,
        r#"
const fn main() {
    std::server::shell("echo integration-test");
}
"#,
    )
    .expect("input should be written");

    let run = Command::new(bin)
        .arg("compile")
        .arg(&input)
        .arg("--output")
        .arg(&output)
        .output()
        .expect("fp-shell should execute");

    assert!(
        run.status.success(),
        "stdout:\n{}\n\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let script = fs::read_to_string(output).expect("output script should be readable");
    assert!(script.contains("echo integration-test"));
}
