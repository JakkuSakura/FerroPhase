use crate::ShellError;
use fp_core::ast::{Item, ItemImportPath, ItemImportTree, ItemKind, NodeKind};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::utils::to_json::ToJson;
use fp_interpret::engine::{AstInterpreter, EvalStepOutcome, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;
use fp_shell_core::{
    DockerInventory, InventoryHost, KubectlInventory, ShellInventory, SshInventory, TransportKind,
    WinRmInventory,
};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

const INVENTORY_SHELL_STD_SOURCE: &str = include_str!("std/shell/inventory.fp");
const INVENTORY_SHELL_STD_PATH: &str =
    concat!(env!("CARGO_MANIFEST_DIR"), "/src/std/shell/inventory.fp");

#[derive(Debug, serde::Deserialize, Default)]
struct InventoryDocument {
    #[serde(default)]
    groups: HashMap<String, Vec<String>>,
    #[serde(default)]
    hosts: HashMap<String, InventoryHostDocument>,
}

#[derive(Debug, serde::Deserialize, Default)]
struct InventoryHostDocument {
    transport: Option<TransportKind>,
    address: Option<String>,
    user: Option<String>,
    password: Option<String>,
    port: Option<u16>,
    container: Option<String>,
    pod: Option<String>,
    namespace: Option<String>,
    context: Option<String>,
    scheme: Option<String>,
}

pub fn load_inventory(path: &Path) -> Result<ShellInventory, ShellError> {
    let content = fs::read_to_string(path)?;
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or_default()
        .to_ascii_lowercase();

    if extension == "fp" {
        return load_inventory_from_fp(&content, path);
    }

    let parsed: InventoryDocument = if extension == "json" {
        serde_json::from_str(&content)
            .map_err(|err| ShellError::Inventory(format!("invalid inventory json: {}", err)))?
    } else {
        toml::from_str(&content)
            .map_err(|err| ShellError::Inventory(format!("invalid inventory toml: {}", err)))?
    };

    let mut hosts = HashMap::new();
    for (name, host) in parsed.hosts {
        let transport = host.transport.unwrap_or(TransportKind::Ssh);
        let entry = match transport {
            TransportKind::Local => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: None,
                winrm: None,
            },
            TransportKind::Ssh => InventoryHost {
                transport,
                ssh: Some(SshInventory {
                    address: host.address,
                    user: host.user,
                    port: host.port,
                }),
                docker: None,
                kubectl: None,
                winrm: None,
            },
            TransportKind::Docker => InventoryHost {
                transport,
                ssh: None,
                docker: Some(DockerInventory {
                    container: host.container.ok_or_else(|| {
                        ShellError::Inventory(format!("docker host '{}' requires container", name))
                    })?,
                    user: host.user,
                }),
                kubectl: None,
                winrm: None,
            },
            TransportKind::Kubectl => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: Some(KubectlInventory {
                    pod: host.pod.ok_or_else(|| {
                        ShellError::Inventory(format!("kubectl host '{}' requires pod", name))
                    })?,
                    namespace: host.namespace,
                    container: host.container,
                    context: host.context,
                }),
                winrm: None,
            },
            TransportKind::Winrm => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: None,
                winrm: Some(WinRmInventory {
                    address: host.address.ok_or_else(|| {
                        ShellError::Inventory(format!("winrm host '{}' requires address", name))
                    })?,
                    user: host.user.ok_or_else(|| {
                        ShellError::Inventory(format!("winrm host '{}' requires user", name))
                    })?,
                    password: host.password,
                    port: host.port,
                    scheme: host.scheme,
                }),
            },
        };
        hosts.insert(name, entry);
    }

    Ok(ShellInventory {
        groups: parsed.groups,
        hosts,
    })
}

fn load_inventory_from_fp(source: &str, path: &Path) -> Result<ShellInventory, ShellError> {
    let frontend = FerroFrontend::new();
    let parsed = frontend
        .parse(source, Some(path))
        .map_err(|err| ShellError::Inventory(format!("invalid inventory fp: {}", err)))?;

    let mut ast = parsed.ast;
    inject_inventory_shell_std(&frontend, &mut ast)?;
    let options = InterpreterOptions {
        mode: InterpreterMode::CompileTime,
        ..InterpreterOptions::default()
    };
    let context = SharedScopedContext::new();
    let mut interpreter = AstInterpreter::new(&context, options);
    interpreter.interpret(&mut ast);

    let NodeKind::File(file) = ast.kind_mut() else {
        return Err(ShellError::Inventory(
            "inventory fp must be a file document".to_string(),
        ));
    };

    let function = file
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::DefFunction(function) if function.name.as_str() == "inventory" => {
                Some(function)
            }
            _ => None,
        })
        .ok_or_else(|| {
            ShellError::Inventory(
                "inventory fp must define `const fn inventory() -> Inventory`".to_string(),
            )
        })?;

    if function.sig.receiver.is_some() || !function.sig.params.is_empty() {
        return Err(ShellError::Inventory(
            "inventory() must not take parameters".to_string(),
        ));
    }

    let mut body = (*function.body).clone();
    interpreter.begin_const_eval(&mut body).map_err(|err| {
        ShellError::Inventory(format!("failed to start inventory() eval: {}", err))
    })?;
    let value = loop {
        match interpreter.step_const_eval(10_000).map_err(|err| {
            ShellError::Inventory(format!("failed to evaluate inventory(): {}", err))
        })? {
            EvalStepOutcome::Yielded => continue,
            EvalStepOutcome::Complete(value) => break value,
        }
    };

    let parsed: InventoryDocument = value.to_value().map_err(|err| {
        ShellError::Inventory(format!("inventory() returned incompatible value: {}", err))
    })?;
    inventory_document_to_inventory(parsed)
}

fn inject_inventory_shell_std(
    frontend: &FerroFrontend,
    ast: &mut fp_core::ast::Node,
) -> Result<(), ShellError> {
    rewrite_inventory_shell_imports(ast);
    let std_items = parse_inventory_shell_std(frontend)?;
    let NodeKind::File(file) = ast.kind_mut() else {
        return Err(ShellError::Inventory(
            "inventory fp must be a file document".to_string(),
        ));
    };
    file.items.splice(0..0, std_items);
    Ok(())
}

fn parse_inventory_shell_std(frontend: &FerroFrontend) -> Result<Vec<Item>, ShellError> {
    let parsed = frontend
        .parse(
            INVENTORY_SHELL_STD_SOURCE,
            Some(Path::new(INVENTORY_SHELL_STD_PATH)),
        )
        .map_err(|err| ShellError::Inventory(format!("invalid inventory shell std: {}", err)))?;

    let NodeKind::File(file) = parsed.ast.kind() else {
        return Err(ShellError::Inventory(
            "inventory shell std must be a file document".to_string(),
        ));
    };
    Ok(file.items.clone())
}

fn rewrite_inventory_shell_imports(ast: &mut fp_core::ast::Node) {
    let NodeKind::File(file) = ast.kind_mut() else {
        return;
    };
    for item in &mut file.items {
        rewrite_inventory_shell_imports_in_item(item);
    }
}

fn rewrite_inventory_shell_imports_in_item(item: &mut Item) {
    match item.kind_mut() {
        ItemKind::Import(import) => rewrite_inventory_shell_import_tree(&mut import.tree),
        ItemKind::Module(module) => {
            for child in &mut module.items {
                rewrite_inventory_shell_imports_in_item(child);
            }
        }
        _ => {}
    }
}

fn rewrite_inventory_shell_import_tree(tree: &mut ItemImportTree) {
    match tree {
        ItemImportTree::Path(path) => {
            let mut segments = flatten_import_segments(path.segments.clone());
            for segment in &mut segments {
                rewrite_inventory_shell_import_tree(segment);
            }
            if starts_with_std_shell(&segments) {
                segments.drain(0..2);
            }
            *tree = match segments.len() {
                0 => ItemImportTree::Path(ItemImportPath { segments }),
                1 => segments.into_iter().next().expect("single segment"),
                _ => ItemImportTree::Path(ItemImportPath { segments }),
            };
        }
        ItemImportTree::Group(group) => {
            for item in &mut group.items {
                rewrite_inventory_shell_import_tree(item);
            }
        }
        _ => {}
    }
}

fn flatten_import_segments(segments: Vec<ItemImportTree>) -> Vec<ItemImportTree> {
    let mut flattened = Vec::new();
    for segment in segments {
        match segment {
            ItemImportTree::Path(path) => flattened.extend(flatten_import_segments(path.segments)),
            other => flattened.push(other),
        }
    }
    flattened
}

fn starts_with_std_shell(segments: &[ItemImportTree]) -> bool {
    matches!(
        segments,
        [ItemImportTree::Ident(std), ItemImportTree::Ident(shell), ..]
            if std.as_str() == "std" && shell.as_str() == "shell"
    )
}

fn inventory_document_to_inventory(
    parsed: InventoryDocument,
) -> Result<ShellInventory, ShellError> {
    let mut hosts = HashMap::new();
    for (name, host) in parsed.hosts {
        let host = normalize_host_document(host);
        let transport = host.transport.unwrap_or(TransportKind::Ssh);
        let entry = match transport {
            TransportKind::Local => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: None,
                winrm: None,
            },
            TransportKind::Ssh => InventoryHost {
                transport,
                ssh: Some(SshInventory {
                    address: host.address,
                    user: host.user,
                    port: host.port,
                }),
                docker: None,
                kubectl: None,
                winrm: None,
            },
            TransportKind::Docker => InventoryHost {
                transport,
                ssh: None,
                docker: Some(DockerInventory {
                    container: host.container.ok_or_else(|| {
                        ShellError::Inventory(format!("docker host '{}' requires container", name))
                    })?,
                    user: host.user,
                }),
                kubectl: None,
                winrm: None,
            },
            TransportKind::Kubectl => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: Some(KubectlInventory {
                    pod: host.pod.ok_or_else(|| {
                        ShellError::Inventory(format!("kubectl host '{}' requires pod", name))
                    })?,
                    namespace: host.namespace,
                    container: host.container,
                    context: host.context,
                }),
                winrm: None,
            },
            TransportKind::Winrm => InventoryHost {
                transport,
                ssh: None,
                docker: None,
                kubectl: None,
                winrm: Some(WinRmInventory {
                    address: host.address.ok_or_else(|| {
                        ShellError::Inventory(format!("winrm host '{}' requires address", name))
                    })?,
                    user: host.user.ok_or_else(|| {
                        ShellError::Inventory(format!("winrm host '{}' requires user", name))
                    })?,
                    password: host.password,
                    port: host.port,
                    scheme: host.scheme,
                }),
            },
        };
        hosts.insert(name, entry);
    }

    Ok(ShellInventory {
        groups: parsed.groups,
        hosts,
    })
}

fn normalize_host_document(mut host: InventoryHostDocument) -> InventoryHostDocument {
    fn normalize_string(value: Option<String>) -> Option<String> {
        value.and_then(|text| if text.is_empty() { None } else { Some(text) })
    }

    host.address = normalize_string(host.address);
    host.user = normalize_string(host.user);
    host.password = normalize_string(host.password);
    host.container = normalize_string(host.container);
    host.pod = normalize_string(host.pod);
    host.namespace = normalize_string(host.namespace);
    host.context = normalize_string(host.context);
    host.scheme = normalize_string(host.scheme);
    host.port = host
        .port
        .and_then(|port| if port == 0 { None } else { Some(port) });
    host
}
