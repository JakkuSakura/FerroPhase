use crate::{CliError, Result};

/// Supported output language targets for the CLI.
#[derive(Debug, Clone, Copy)]
pub enum LanguageTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Zig,
    Sycl,
    Rust,
    Wit,
}

/// Parse a transpile target from a user-provided string.
pub fn parse_language_target(s: &str) -> Result<LanguageTarget> {
    let normalized = s.to_lowercase();
    let target = match normalized.as_str() {
        "typescript" | "ts" => LanguageTarget::TypeScript,
        "javascript" | "js" => LanguageTarget::JavaScript,
        "csharp" | "cs" | "c#" => LanguageTarget::CSharp,
        "python" | "py" => LanguageTarget::Python,
        "zig" => LanguageTarget::Zig,
        "sycl" => LanguageTarget::Sycl,
        "rust" | "rs" => LanguageTarget::Rust,
        "wit" => LanguageTarget::Wit,
        _ => {
            return Err(CliError::InvalidInput(format!("Unsupported target: {}", s)));
        }
    };
    Ok(target)
}

/// File extension to use when emitting code for a target.
pub fn output_extension_for(target: LanguageTarget) -> &'static str {
    match target {
        LanguageTarget::TypeScript => "ts",
        LanguageTarget::JavaScript => "js",
        LanguageTarget::CSharp => "cs",
        LanguageTarget::Python => "py",
        LanguageTarget::Zig => "zig",
        LanguageTarget::Sycl => "cpp",
        LanguageTarget::Rust => "rs",
        LanguageTarget::Wit => "wit",
    }
}

use std::path::{Path, PathBuf};

/// Resolve the desired output path for a target, respecting explicit output.
pub fn resolve_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: &str,
) -> Result<PathBuf> {
    if let Some(out) = output.cloned() {
        Ok(out)
    } else {
        let parsed = parse_language_target(target)?;
        let ext = output_extension_for(parsed);
        Ok(input.with_extension(ext))
    }
}

// Transpilation is implemented in commands/transpile.rs
