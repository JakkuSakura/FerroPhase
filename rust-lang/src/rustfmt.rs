use common::*;
use std::io::Write;
use std::process::{Command, Stdio};

pub fn format_code(s: &str) -> Result<String> {
    let mut fmt = Command::new("rustfmt")
        .args(&["--edition", "2021"])
        .stdin(Stdio::piped())
        .spawn()?;
    fmt.stdin.take().unwrap().write_all(s.as_bytes())?;
    let output = fmt.wait_with_output()?;
    if !output.status.success() {
        bail!(
            "Error when formatting: {}",
            String::from_utf8_lossy(&output.stderr)
        )
    }
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
