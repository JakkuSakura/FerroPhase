mod embedded_std;
mod inventory;
mod lower;

use fp_core::ast::{AstTargetOutput, Ident, Item, ItemKind, Module, Node, NodeKind, Visibility};
use fp_core::cfg::{TargetEnv, filter_items_in_node};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_interpret::const_eval::ConstEvaluationOrchestrator;
use fp_lang::FerroFrontend;
use fp_shell_core::{ScriptTarget, ShellInventory, validate_extern_decl};
use std::fs;
use std::path::{Path, PathBuf};

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
    let mut ast = merge_runtime_helpers(ast, &target_env)?;
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
    let lowered = lower::lower_node(&ast, &inventory).map_err(ShellError::Lower)?;
    validate_extern_decls(&lowered, target).map_err(ShellError::Lower)?;

    let code = match target {
        ScriptTarget::Bash => fp_bash::BashTarget::new()
            .render(&lowered, &inventory)
            .map_err(|err| ShellError::Emit(err.to_string()))?,
        ScriptTarget::PowerShell => fp_powershell::PowerShellTarget::new()
            .render(&lowered, &inventory)
            .map_err(|err| ShellError::Emit(err.to_string()))?,
    };

    Ok(AstTargetOutput {
        code,
        side_files: Vec::new(),
    })
}

fn merge_runtime_helpers(ast: Node, target_env: &TargetEnv) -> Result<Node, ShellError> {
    let frontend = FerroFrontend::new();
    let NodeKind::File(mut user_file) = ast.kind else {
        return Err(ShellError::Lower(
            "shell compilation requires file AST nodes".to_string(),
        ));
    };

    let mut shell_std_items = Vec::new();
    for path in [
        "shell/backend.fp",
        "shell/server.fp",
        "shell/files.fp",
        "shell/service.fp",
        "shell/facts.fp",
        "shell/capabilities.fp",
        "shell/process.fp",
    ] {
        let source = embedded_std::read(path)
            .ok_or_else(|| ShellError::Lower(format!("missing embedded shell std: {path}")))?;
        let parsed = frontend
            .parse(source, Some(Path::new(&format!("<fp-shell-std>/{path}"))))
            .map_err(|err| {
                ShellError::Lower(format!(
                    "failed to parse embedded shell std {path}: {}",
                    err
                ))
            })?;
        let mut shell_std_ast = parsed.ast;
        filter_items_in_node(&mut shell_std_ast, target_env);
        let NodeKind::File(shell_std_file) = shell_std_ast.kind else {
            return Err(ShellError::Lower(format!(
                "embedded shell std must be a file document: {path}"
            )));
        };
        shell_std_items.extend(shell_std_file.items);
    }

    if let Some(existing_std) = user_file
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::Module(module) if module.name.as_str() == "std" => Some(module),
            _ => None,
        })
    {
        existing_std.items.splice(0..0, shell_std_items);
    } else {
        user_file.items.insert(
            0,
            Item::from(ItemKind::Module(Module {
                attrs: Vec::new(),
                name: Ident::new("std"),
                items: shell_std_items,
                visibility: Visibility::Public,
                is_external: false,
            })),
        );
    }
    Ok(Node::file(user_file))
}

fn validate_extern_decls(node: &Node, target: ScriptTarget) -> Result<(), String> {
    match node.kind() {
        NodeKind::File(file) => {
            for item in &file.items {
                validate_externs_in_item(item, target)?;
            }
            Ok(())
        }
        _ => Err("shell compilation requires file AST nodes".to_string()),
    }
}

fn validate_externs_in_item(item: &Item, target: ScriptTarget) -> Result<(), String> {
    match item.kind() {
        ItemKind::DeclFunction(function) => {
            if extern_matches_target(function, target) {
                validate_extern_decl(function, target)?;
            }
            Ok(())
        }
        ItemKind::Module(module) => {
            for child in &module.items {
                validate_externs_in_item(child, target)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn extern_matches_target(function: &fp_core::ast::ItemDeclFunction, target: ScriptTarget) -> bool {
    match (&function.sig.abi, target) {
        (fp_core::ast::Abi::Named(abi), ScriptTarget::Bash) => abi == "bash",
        (fp_core::ast::Abi::Named(abi), ScriptTarget::PowerShell) => abi == "pwsh",
        _ => false,
    }
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
    fn compile_source_allows_rsync_for_winrm_host_credentials() {
        let source = r#"
const fn main() {
    std::files::rsync(
        src="./dist/",
        dest="C:/deploy/dist/",
        hosts="win-1",
        delete=true,
    );
}
"#;

        let inventory = ShellInventory {
            groups: Default::default(),
            hosts: std::collections::HashMap::from([(
                "win-1".to_string(),
                InventoryHost {
                    transport: TransportKind::Winrm,
                    fields: std::collections::HashMap::from([
                        (
                            "address".to_string(),
                            fp_shell_core::InventoryValue::String("10.0.0.21".to_string()),
                        ),
                        (
                            "user".to_string(),
                            fp_shell_core::InventoryValue::String("Administrator".to_string()),
                        ),
                        ("port".to_string(), fp_shell_core::InventoryValue::U16(2222)),
                    ]),
                },
            )]),
        };

        let rendered = compile_source_with_options(
            source,
            Path::new("sample.fp"),
            ScriptTarget::PowerShell,
            &CompileOptions {
                inventory: Some(inventory),
            },
        )
        .expect("source should compile");

        assert!(rendered.code.contains("function rsync_remote_target"));
        assert!(rendered.code.contains("Administrator"));
        assert!(rendered.code.contains("10.0.0.21"));
        assert!(rendered.code.contains("rsync -e"));
    }

    #[test]
    fn compile_source_supports_try_else_finally_and_defer_in_bash() {
        let source = r#"
const fn main() {
    try {
        defer std::server::shell("echo cleanup");
        std::server::shell("echo body");
    } catch err {
        std::server::shell(f"echo failed={err}");
    } else {
        std::server::shell("echo success");
    } finally {
        std::server::shell("echo finally");
    }
}
"#;

        let rendered = compile_source(source, Path::new("try.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("if {"));
        assert!(rendered.code.contains("echo body"));
        assert!(rendered.code.contains("echo success"));
        assert!(rendered.code.contains("echo finally"));
        assert!(rendered.code.contains("echo cleanup"));
        assert!(rendered.code.contains("failed=${"));
    }

    #[test]
    fn compile_source_supports_shell_facts_and_process_helpers() {
        let source = r#"
const fn main() {
    if std::shell::capabilities::has_rsync() {
        let cmd = std::shell::process::pipe(
            std::shell::process::raw("printf build"),
            std::shell::process::stdout_to(std::shell::process::raw("cat"), "/tmp/build.log"),
        );
        std::shell::process::run(cmd);
    }
}
"#;

        let rendered = compile_source(source, Path::new("process.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("has_command 'rsync'"));
        assert!(rendered.code.contains("local cmd=\"$(pipe"));
        assert!(rendered.code.contains("/tmp/build.log"));
    }

    #[test]
    fn compile_source_supports_with_host_context() {
        let source = r#"
const fn main() {
    with "web-1" {
        std::server::shell("echo hello");
        std::service::restart("nginx");
    }
}
"#;

        let rendered = compile_source(source, Path::new("with.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("echo hello"));
        assert!(rendered.code.contains("restart 'nginx'"));
        assert!(rendered.code.contains("web-1"));
    }

    #[test]
    fn compile_source_supports_context_params_without_host_name_convention() {
        let source = r#"
const fn deploy(command: str, context target: str) {
    std::server::shell(command, hosts=target);
}

const fn main() {
    with "web-1" {
        deploy("echo hello");
    }
}
"#;

        let rendered = compile_source(source, Path::new("context-param.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("deploy 'echo hello' 'web-1'"));
        assert!(rendered.code.contains("shell \"${command}\" \"${target}\""));
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
            inventory.hosts["win"].get_string("password"),
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
            inventory.hosts["docker-1"].get_string("container"),
            Some("app")
        );
        assert_eq!(inventory.hosts["k8s-1"].get_string("pod"), Some("api-123"));
        assert_eq!(
            inventory.hosts["win-1"].get_string("password"),
            Some("secret")
        );
    }
}
