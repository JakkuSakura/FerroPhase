use fp_core::error::{Error, Result};
use std::path::Path;
use std::process::Command;

pub fn link_with_clang(obj: &Path, out: &Path, extra_args: &[String]) -> Result<()> {
    // Temporary protocol: the pipeline passes `--driver=<name>` when the user sets --linker.
    // We'll evolve this into a proper LinkOptions struct as fp-native grows.
    let mut driver: &str = "clang";
    let mut forwarded = Vec::new();
    for a in extra_args {
        if let Some(rest) = a.strip_prefix("--driver=") {
            driver = rest;
            continue;
        }
        forwarded.push(a);
    }

    let clang_available = Command::new(driver).arg("--version").output();
    if matches!(clang_available, Err(_)) {
        return Err(Error::from(format!("`{}` not found in PATH", driver)));
    }

    let mut cmd = Command::new(driver);
    cmd.arg(obj);
    cmd.arg("-o").arg(out);

    for a in forwarded {
        cmd.arg(a);
    }

    let output = cmd.output().map_err(|e| Error::from(e.to_string()))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut msg = stderr.trim().to_string();
        if msg.is_empty() {
            msg = stdout.trim().to_string();
        }
        if msg.is_empty() {
            msg = format!("{} failed without diagnostics", driver);
        }
        return Err(Error::from(format!("{driver} failed: {msg}")));
    }

    Ok(())
}
