use crate::{CliError, Result};
use std::path::{Path, PathBuf};

fn read_env_path(key: &str) -> Option<PathBuf> {
    std::env::var_os(key).map(PathBuf::from)
}

pub fn bootstrap_compile_from_env() -> Result<()> {
    // Expect environment variables set by the bootstrap script
    let snapshot = read_env_path("FP_BOOTSTRAP_SNAPSHOT")
        .ok_or_else(|| CliError::Compilation("FP_BOOTSTRAP_SNAPSHOT not set".to_string()))?;
    let output = read_env_path("FP_BOOTSTRAP_OUTPUT");
    bootstrap_compile(&snapshot, output.as_deref())
}

pub fn bootstrap_compile(snapshot: &Path, output: Option<&Path>) -> Result<()> {
    let _ = (snapshot, output);
    Err(CliError::Compilation(
        "bootstrap snapshot compilation is no longer supported by fp-cli".to_string(),
    ))
}
