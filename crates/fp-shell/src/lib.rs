use fp_bash::BashTarget;
use fp_core::ast::{AstTarget, AstTargetOutput};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_lang::FerroFrontend;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShellTarget {
    Bash,
}

impl ShellTarget {
    pub fn parse(raw: &str) -> Result<Self, ShellError> {
        match raw.trim().to_ascii_lowercase().as_str() {
            "bash" => Ok(Self::Bash),
            other => Err(ShellError::UnsupportedTarget(other.to_string())),
        }
    }

    pub fn extension(&self) -> &'static str {
        match self {
            Self::Bash => "sh",
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ShellError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("failed to parse source: {0}")]
    Parse(String),
    #[error("failed to emit target: {0}")]
    Emit(String),
    #[error("unsupported target: {0}")]
    UnsupportedTarget(String),
}

pub fn compile_source(
    source: &str,
    source_path: &Path,
    target: ShellTarget,
) -> Result<AstTargetOutput, ShellError> {
    let frontend = FerroFrontend::new();
    let parsed = frontend
        .parse(source, Some(source_path))
        .map_err(|err| ShellError::Parse(err.to_string()))?;

    let mut ast = parsed.ast;
    let mut const_eval = ConstEvaluationOrchestrator::new(parsed.serializer.clone());
    const_eval.set_execute_main(false);
    const_eval
        .evaluate(
            &mut ast,
            &SharedScopedContext::new(),
            parsed.macro_parser.clone(),
            parsed.intrinsic_normalizer.clone(),
        )
        .map_err(|err| ShellError::Parse(err.to_string()))?;

    match target {
        ShellTarget::Bash => BashTarget::new()
            .emit_node(&ast)
            .map_err(|err| ShellError::Emit(err.to_string())),
    }
}

pub fn compile_file(
    input: &Path,
    output: Option<&Path>,
    target: ShellTarget,
) -> Result<PathBuf, ShellError> {
    let source = fs::read_to_string(input)?;
    let generated = compile_source(&source, input, target.clone())?;

    let destination = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| input.with_extension(target.extension()));

    if let Some(parent) = destination.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&destination, generated.code)?;
    Ok(destination)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_source_generates_bash_script() {
        let source = r#"
const fn main() {
    std::server::shell("echo hello");
}
"#;

        let rendered = compile_source(source, Path::new("sample.fp"), ShellTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("#!/usr/bin/env bash"));
        assert!(rendered.code.contains("echo hello"));
    }

    #[test]
    fn compile_file_writes_default_extension() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let input = directory.path().join("deploy.fp");
        fs::write(
            &input,
            r#"
const fn main() {
    std::server::shell("echo hi");
}
"#,
        )
        .expect("input should be written");

        let output = compile_file(&input, None, ShellTarget::Bash).expect("file should compile");

        assert_eq!(output, directory.path().join("deploy.sh"));
        let content = fs::read_to_string(output).expect("output should be readable");
        assert!(content.contains("echo hi"));
    }

    #[test]
    fn host_on_requires_closure_body() {
        let source = r#"
const fn main() {
    std::host::on("web-1", {
        std::server::shell("uptime");
    });
}
"#;

        let error = compile_source(source, Path::new("sample.fp"), ShellTarget::Bash)
            .expect_err("non-closure host scope should fail");

        assert!(
            error
                .to_string()
                .contains("requires closure body syntax: std::host::on(hosts, || { ... })")
        );
    }
}
