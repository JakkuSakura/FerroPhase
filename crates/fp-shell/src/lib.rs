use fp_bash::{BashTarget, InventoryHost, ShellInventory};
use fp_core::ast::{AstTarget, AstTargetOutput};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_lang::FerroFrontend;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub inventory: Option<ShellInventory>,
}

#[derive(Debug, serde::Deserialize, Default)]
struct InventoryDocument {
    #[serde(default)]
    groups: std::collections::HashMap<String, Vec<String>>,
    #[serde(default)]
    hosts: std::collections::HashMap<String, InventoryHostDocument>,
}

#[derive(Debug, serde::Deserialize, Default)]
struct InventoryHostDocument {
    address: Option<String>,
    user: Option<String>,
    port: Option<u16>,
}

pub fn load_inventory(path: &Path) -> Result<ShellInventory, ShellError> {
    let content = fs::read_to_string(path)?;
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or_default()
        .to_ascii_lowercase();

    let parsed: InventoryDocument = if extension == "json" {
        serde_json::from_str(&content)
            .map_err(|err| ShellError::Parse(format!("invalid inventory json: {}", err)))?
    } else {
        toml::from_str(&content)
            .map_err(|err| ShellError::Parse(format!("invalid inventory toml: {}", err)))?
    };

    let hosts = parsed
        .hosts
        .into_iter()
        .map(|(name, host)| {
            (
                name,
                InventoryHost {
                    address: host.address,
                    user: host.user,
                    port: host.port,
                },
            )
        })
        .collect();

    Ok(ShellInventory {
        groups: parsed.groups,
        hosts,
    })
}

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
    compile_source_with_options(source, source_path, target, &CompileOptions::default())
}

pub fn compile_source_with_options(
    source: &str,
    source_path: &Path,
    target: ShellTarget,
    options: &CompileOptions,
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
        ShellTarget::Bash => {
            if let Some(inventory) = &options.inventory {
                BashTarget::with_inventory(inventory.clone())
                    .emit_node(&ast)
                    .map_err(|err| ShellError::Emit(err.to_string()))
            } else {
                BashTarget::new()
                    .emit_node(&ast)
                    .map_err(|err| ShellError::Emit(err.to_string()))
            }
        }
    }
}

pub fn compile_file(
    input: &Path,
    output: Option<&Path>,
    target: ShellTarget,
) -> Result<PathBuf, ShellError> {
    compile_file_with_options(input, output, target, &CompileOptions::default())
}

pub fn compile_file_with_options(
    input: &Path,
    output: Option<&Path>,
    target: ShellTarget,
    options: &CompileOptions,
) -> Result<PathBuf, ShellError> {
    let source = fs::read_to_string(input)?;
    let generated = compile_source_with_options(&source, input, target.clone(), options)?;

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

    #[test]
    fn parses_inventory_from_toml() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.toml");
        fs::write(
            &path,
            r#"
[groups]
web = ["web-1", "web-2"]

[hosts.web-1]
address = "10.0.0.10"
user = "deploy"
"#,
        )
        .expect("inventory should be written");

        let inventory = load_inventory(&path).expect("inventory should parse");
        assert_eq!(inventory.groups["web"].len(), 2);
        assert_eq!(inventory.hosts["web-1"].user.as_deref(), Some("deploy"));
    }
}
