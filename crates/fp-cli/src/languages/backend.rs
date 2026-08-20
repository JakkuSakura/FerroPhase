use crate::{CliError, Result};

/// Supported AST output targets for the CLI.
#[derive(Debug, Clone, Copy)]
pub enum BuiltinLanguageTarget {
    FerroPhase,
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Go,
    Gdscript,
    Zig,
    Sycl,
    Rust,
    Wit,
    Kotlin,
}

/// Parse an AST output target from a user-provided string.
pub fn parse_language_target(s: &str) -> Result<BuiltinLanguageTarget> {
    let normalized = s.to_lowercase();
    let target = match normalized.as_str() {
        "fp" | "ferro" | "ferrophase" => BuiltinLanguageTarget::FerroPhase,
        "typescript" | "ts" => BuiltinLanguageTarget::TypeScript,
        "javascript" | "js" => BuiltinLanguageTarget::JavaScript,
        "csharp" | "cs" | "c#" => BuiltinLanguageTarget::CSharp,
        "python" | "py" => BuiltinLanguageTarget::Python,
        "go" | "golang" => BuiltinLanguageTarget::Go,
        "gdscript" | "gd" => BuiltinLanguageTarget::Gdscript,
        "zig" => BuiltinLanguageTarget::Zig,
        "sycl" => BuiltinLanguageTarget::Sycl,
        "rust" | "rs" => BuiltinLanguageTarget::Rust,
        "wit" => BuiltinLanguageTarget::Wit,
        "kotlin" | "kt" => BuiltinLanguageTarget::Kotlin,
        _ => {
            return Err(CliError::InvalidInput(format!("Unsupported target: {}", s)));
        }
    };
    Ok(target)
}

/// File extension to use when emitting code for a target.
pub fn output_extension_for(target: BuiltinLanguageTarget) -> &'static str {
    match target {
        BuiltinLanguageTarget::FerroPhase => "fp",
        BuiltinLanguageTarget::TypeScript => "ts",
        BuiltinLanguageTarget::JavaScript => "js",
        BuiltinLanguageTarget::CSharp => "cs",
        BuiltinLanguageTarget::Python => "py",
        BuiltinLanguageTarget::Go => "go",
        BuiltinLanguageTarget::Gdscript => "gd",
        BuiltinLanguageTarget::Zig => "zig",
        BuiltinLanguageTarget::Sycl => "cpp",
        BuiltinLanguageTarget::Rust => "rs",
        BuiltinLanguageTarget::Wit => "wit",
        BuiltinLanguageTarget::Kotlin => "kt",
    }
}

/// What a given output target can express directly — see
/// `fp_core::capabilities::LanguageCapabilities`. Each target-emitting
/// crate that wants anything other than the conservative default declares
/// its own `CAPABILITIES` const (e.g. `fp_kotlin::CAPABILITIES`); this is
/// the one place that maps a requested `BuiltinLanguageTarget` to the right one.
/// Anything not listed here (including any target whose crate is a
/// disabled optional feature) gets `LanguageCapabilities::NATIVE`.
pub fn capabilities_for_target(target: BuiltinLanguageTarget) -> fp_core::capabilities::LanguageCapabilities {
    match target {
        #[cfg(feature = "lang-kotlin")]
        BuiltinLanguageTarget::Kotlin => fp_kotlin::CAPABILITIES,
        _ => fp_core::capabilities::LanguageCapabilities::NATIVE,
    }
}

use std::path::{Path, PathBuf};

/// Resolve the desired output path for a target, respecting explicit output.
pub fn resolve_ast_output_path(
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
