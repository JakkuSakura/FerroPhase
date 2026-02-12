use crate::{CliError, Result};

/// Supported AST output targets for the CLI.
#[derive(Debug, Clone, Copy)]
pub enum AstLanguageTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Go,
    Zig,
    Sycl,
    Rust,
    Wit,
}

/// Parse an AST output target from a user-provided string.
pub fn parse_ast_target(s: &str) -> Result<AstLanguageTarget> {
    let normalized = s.to_lowercase();
    let target = match normalized.as_str() {
        "typescript" | "ts" => AstLanguageTarget::TypeScript,
        "javascript" | "js" => AstLanguageTarget::JavaScript,
        "csharp" | "cs" | "c#" => AstLanguageTarget::CSharp,
        "python" | "py" => AstLanguageTarget::Python,
        "go" | "golang" => AstLanguageTarget::Go,
        "zig" => AstLanguageTarget::Zig,
        "sycl" => AstLanguageTarget::Sycl,
        "rust" | "rs" => AstLanguageTarget::Rust,
        "wit" => AstLanguageTarget::Wit,
        _ => {
            return Err(CliError::InvalidInput(format!("Unsupported target: {}", s)));
        }
    };
    Ok(target)
}

/// File extension to use when emitting code for a target.
pub fn ast_output_extension_for(target: AstLanguageTarget) -> &'static str {
    match target {
        AstLanguageTarget::TypeScript => "ts",
        AstLanguageTarget::JavaScript => "js",
        AstLanguageTarget::CSharp => "cs",
        AstLanguageTarget::Python => "py",
        AstLanguageTarget::Go => "go",
        AstLanguageTarget::Zig => "zig",
        AstLanguageTarget::Sycl => "cpp",
        AstLanguageTarget::Rust => "rs",
        AstLanguageTarget::Wit => "wit",
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
        let parsed = parse_ast_target(target)?;
        let ext = ast_output_extension_for(parsed);
        Ok(input.with_extension(ext))
    }
}
