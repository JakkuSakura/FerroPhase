mod inventory;
mod lower;

use fp_core::ast::AstTargetOutput;
use fp_core::cfg::{TargetEnv, filter_items_in_node};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_lang::FerroFrontend;
use fp_shell_core::{ScriptRenderer, ScriptTarget, ShellInventory};
use std::fs;
use std::path::{Path, PathBuf};

const BACKEND_HELPERS_SOURCE: &str = include_str!("std/shell/backend.fp");
const BACKEND_HELPERS_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/std/shell/backend.fp");
const RUNTIME_HELPERS_SOURCE: &str = include_str!("std/shell/runtime_helpers.fp");
const RUNTIME_HELPERS_PATH: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/src/std/shell/runtime_helpers.fp"
);

pub use fp_shell_core::{InventoryHost, ScriptTarget as ShellTarget, TransportKind};
pub use inventory::load_inventory;

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub inventory: Option<ShellInventory>,
}

#[derive(thiserror::Error, Debug)]
pub enum ShellError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("failed to parse source: {0}")]
    Parse(String),
    #[error("failed to lower source: {0}")]
    Lower(String),
    #[error("failed to emit target: {0}")]
    Emit(String),
    #[error("failed to parse inventory: {0}")]
    Inventory(String),
    #[error("unsupported target: {0}")]
    UnsupportedTarget(String),
}

pub fn parse_target(raw: &str) -> Result<ScriptTarget, ShellError> {
    match raw.trim().to_ascii_lowercase().as_str() {
        "bash" => Ok(ScriptTarget::Bash),
        "powershell" | "pwsh" | "ps" => Ok(ScriptTarget::PowerShell),
        other => Err(ShellError::UnsupportedTarget(other.to_string())),
    }
}

pub fn compile_source(
    source: &str,
    source_path: &Path,
    target: ScriptTarget,
) -> Result<AstTargetOutput, ShellError> {
    compile_source_with_options(source, source_path, target, &CompileOptions::default())
}

pub fn compile_source_with_options(
    source: &str,
    source_path: &Path,
    target: ScriptTarget,
    options: &CompileOptions,
) -> Result<AstTargetOutput, ShellError> {
    let frontend = FerroFrontend::new();
    let parsed = frontend
        .parse(source, Some(source_path))
        .map_err(|err| ShellError::Parse(err.to_string()))?;

    let mut ast = parsed.ast;
    let target_env = shell_target_env(target);
    filter_items_in_node(&mut ast, &target_env);
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

    let inventory = options.inventory.clone().unwrap_or_default();
    let mut program = lower_runtime_helpers(&inventory, &target_env)?;
    let user_program = lower::lower_node(&ast, &inventory).map_err(ShellError::Lower)?;
    program.externs.extend(user_program.externs);
    program.items.extend(user_program.items);

    let code = match target {
        ScriptTarget::Bash => fp_bash::BashTarget::new()
            .render(&program, &inventory)
            .map_err(|err| ShellError::Emit(err.to_string()))?,
        ScriptTarget::PowerShell => fp_powershell::PowerShellTarget::new()
            .render(&program, &inventory)
            .map_err(|err| ShellError::Emit(err.to_string()))?,
    };

    Ok(AstTargetOutput {
        code,
        side_files: Vec::new(),
    })
}

fn lower_runtime_helpers(
    inventory: &ShellInventory,
    target_env: &TargetEnv,
) -> Result<fp_shell_core::ScriptProgram, ShellError> {
    let frontend = FerroFrontend::new();
    let backend = frontend
        .parse(
            BACKEND_HELPERS_SOURCE,
            Some(Path::new(BACKEND_HELPERS_PATH)),
        )
        .map_err(|err| {
            ShellError::Lower(format!("failed to parse shell backend helpers: {}", err))
        })?;
    let mut backend_ast = backend.ast;
    filter_items_in_node(&mut backend_ast, target_env);
    let runtime = frontend
        .parse(
            RUNTIME_HELPERS_SOURCE,
            Some(Path::new(RUNTIME_HELPERS_PATH)),
        )
        .map_err(|err| {
            ShellError::Lower(format!("failed to parse shell runtime helpers: {}", err))
        })?;
    let mut runtime_ast = runtime.ast;
    filter_items_in_node(&mut runtime_ast, target_env);
    merge_helper_ast(&mut backend_ast, runtime_ast)?;
    lower::lower_node(&backend_ast, inventory).map_err(ShellError::Lower)
}

fn merge_helper_ast(
    backend_ast: &mut fp_core::ast::Node,
    runtime_ast: fp_core::ast::Node,
) -> Result<(), ShellError> {
    let fp_core::ast::NodeKind::File(backend_file) = backend_ast.kind_mut() else {
        return Err(ShellError::Lower(
            "shell backend helpers must be a file document".to_string(),
        ));
    };
    let fp_core::ast::NodeKind::File(runtime_file) = runtime_ast.kind else {
        return Err(ShellError::Lower(
            "shell runtime helpers must be a file document".to_string(),
        ));
    };
    backend_file.items.extend(runtime_file.items);
    Ok(())
}

fn shell_target_env(target: ScriptTarget) -> TargetEnv {
    let mut env = TargetEnv::host();
    env.lang = Some(
        match target {
            ScriptTarget::Bash => "bash",
            ScriptTarget::PowerShell => "pwsh",
        }
        .to_string(),
    );
    env
}

pub fn compile_file(
    input: &Path,
    output: Option<&Path>,
    target: ScriptTarget,
) -> Result<PathBuf, ShellError> {
    compile_file_with_options(input, output, target, &CompileOptions::default())
}

pub fn compile_file_with_options(
    input: &Path,
    output: Option<&Path>,
    target: ScriptTarget,
    options: &CompileOptions,
) -> Result<PathBuf, ShellError> {
    let source = fs::read_to_string(input)?;
    let generated = compile_source_with_options(&source, input, target, options)?;

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
    use std::fs;

    #[test]
    fn compile_source_generates_bash_script() {
        let source = r#"
const fn main() {
    std::server::shell("echo hello");
}
"#;

        let rendered = compile_source(source, Path::new("sample.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("#!/usr/bin/env bash"));
        assert!(rendered.code.contains("echo hello"));
    }

    #[test]
    fn compile_source_generates_powershell_script() {
        let source = r#"
const fn main() {
    std::server::shell("Write-Host hello");
}
"#;

        let rendered = compile_source(source, Path::new("sample.fp"), ScriptTarget::PowerShell)
            .expect("source should compile");

        assert!(rendered.code.contains("Set-StrictMode -Version Latest"));
        assert!(rendered.code.contains("Write-Host hello"));
    }

    #[test]
    fn load_inventory_supports_multiple_transports() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.toml");
        fs::write(
            &path,
            r#"
[hosts.app]
transport = "docker"
container = "app-container"

[hosts.api]
transport = "kubectl"
pod = "api-pod"
namespace = "prod"

[hosts.win]
transport = "winrm"
address = "10.0.0.21"
user = "Administrator"
password = "secret"
"#,
        )
        .expect("inventory should be written");

        let inventory = load_inventory(&path).expect("inventory should parse");
        assert_eq!(inventory.hosts["app"].transport, TransportKind::Docker);
        assert_eq!(inventory.hosts["api"].transport, TransportKind::Kubectl);
        assert_eq!(inventory.hosts["win"].transport, TransportKind::Winrm);
        assert_eq!(
            inventory.hosts["win"]
                .winrm
                .as_ref()
                .and_then(|entry| entry.password.as_deref()),
            Some("secret")
        );
    }

    #[test]
    fn load_inventory_supports_fp_format() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.fp");
        fs::write(
            &path,
            r#"
use std::collections::HashMap;
use std::shell::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([
            ("web", ["web-1", "web-2"]),
        ]),
        hosts: HashMap::from([
            ("web-1", Host {
                transport: "ssh",
                address: "10.0.0.11",
                user: "deploy",
            }),
            ("web-2", Host {
                transport: "ssh",
                address: "10.0.0.12",
                user: "deploy",
            }),
        ]),
    }
}
"#,
        )
        .expect("inventory should be written");

        let inventory = load_inventory(&path).expect("fp inventory should parse");
        assert_eq!(inventory.groups["web"].len(), 2);
        assert_eq!(inventory.hosts["web-1"].transport, TransportKind::Ssh);
        assert_eq!(inventory.hosts["web-2"].transport, TransportKind::Ssh);
    }

    #[test]
    fn load_inventory_supports_fp_multiple_transports() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.fp");
        fs::write(
            &path,
            r#"
use std::collections::HashMap;
use std::shell::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([("mixed", ["ssh-1", "docker-1", "k8s-1", "win-1"])]),
        hosts: HashMap::from([
            ("ssh-1", Host {
                transport: "ssh",
                address: "10.0.0.11",
                user: "deploy",
                password: "",
                port: 22,
                container: "",
                pod: "",
                namespace: "",
                context: "",
                scheme: "",
            }),
            ("docker-1", Host {
                transport: "docker",
                address: "",
                user: "root",
                password: "",
                port: 0,
                container: "app",
                pod: "",
                namespace: "",
                context: "",
                scheme: "",
            }),
            ("k8s-1", Host {
                transport: "kubectl",
                address: "",
                user: "",
                password: "",
                port: 0,
                container: "api",
                pod: "api-123",
                namespace: "prod",
                context: "prod-cluster",
                scheme: "",
            }),
            ("win-1", Host {
                transport: "winrm",
                address: "10.0.0.21",
                user: "Administrator",
                password: "secret",
                port: 5985,
                container: "",
                pod: "",
                namespace: "",
                context: "",
                scheme: "http",
            }),
        ]),
    }
}
"#,
        )
        .expect("inventory should be written");

        let inventory = load_inventory(&path).expect("fp inventory should parse");
        assert_eq!(inventory.groups["mixed"].len(), 4);
        assert_eq!(inventory.hosts["ssh-1"].transport, TransportKind::Ssh);
        assert_eq!(inventory.hosts["docker-1"].transport, TransportKind::Docker);
        assert_eq!(inventory.hosts["k8s-1"].transport, TransportKind::Kubectl);
        assert_eq!(inventory.hosts["win-1"].transport, TransportKind::Winrm);
        assert_eq!(
            inventory.hosts["docker-1"]
                .docker
                .as_ref()
                .map(|entry| entry.container.as_str()),
            Some("app")
        );
        assert_eq!(
            inventory.hosts["k8s-1"]
                .kubectl
                .as_ref()
                .map(|entry| entry.pod.as_str()),
            Some("api-123")
        );
        assert_eq!(
            inventory.hosts["win-1"]
                .winrm
                .as_ref()
                .and_then(|entry| entry.password.as_deref()),
            Some("secret")
        );
    }
}
