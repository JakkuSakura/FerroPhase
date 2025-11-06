use fp_core::bail;
use fp_core::error::Result;
use std::io::Write;
use std::process::{Command, Stdio};

pub fn format_code(s: &str) -> Result<String> {
    let mut fmt = Command::new("rustfmt")
        .args(&["--edition", "2024"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    if let Some(mut stdin) = fmt.stdin.take() {
        stdin.write_all(s.as_bytes())?;
    } else {
        bail!("failed to open rustfmt stdin for writing")
    }
    let output = fmt.wait_with_output()?;
    if !output.status.success() {
        bail!(
            "Error when formatting: {}",
            String::from_utf8_lossy(&output.stderr)
        )
    }
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
