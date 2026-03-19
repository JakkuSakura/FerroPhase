use crate::ShellError;
use fp_core::ast::{Item, ItemImportPath, ItemImportTree, ItemKind, NodeKind};
use fp_core::context::SharedScopedContext;
use fp_core::frontend::LanguageFrontend;
use fp_core::utils::to_json::ToJson;
use fp_interpret::engine::{AstInterpreter, EvalStepOutcome, InterpreterMode, InterpreterOptions};
use fp_lang::FerroFrontend;
use fp_shell_core::{InventoryHost, InventoryValue, ShellInventory, TransportKind};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

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
        let entry = inventory_host_from_document(&name, transport, host)?;
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
    let inventory_shell_std = crate::embedded_std::read("shell/inventory.fp")
        .ok_or_else(|| ShellError::Inventory("missing embedded inventory shell std".to_string()))?;
    let parsed = frontend
        .parse(
            inventory_shell_std,
            Some(Path::new("<fp-shell-std>/shell/inventory.fp")),
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
        let entry = inventory_host_from_document(&name, transport, host)?;
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

fn inventory_host_from_document(
    name: &str,
    transport: TransportKind,
    host: InventoryHostDocument,
) -> Result<InventoryHost, ShellError> {
    let mut fields = HashMap::new();
    match transport {
        TransportKind::Local => {}
        TransportKind::Ssh => {
            insert_optional_string(&mut fields, "address", host.address);
            insert_optional_string(&mut fields, "user", host.user);
            insert_optional_u16(&mut fields, "port", host.port);
        }
        TransportKind::Docker => {
            insert_required_string(
                &mut fields,
                "container",
                host.container,
                format!("docker host '{}' requires container", name),
            )?;
            insert_optional_string(&mut fields, "user", host.user);
        }
        TransportKind::Kubectl => {
            insert_required_string(
                &mut fields,
                "pod",
                host.pod,
                format!("kubectl host '{}' requires pod", name),
            )?;
            insert_optional_string(&mut fields, "namespace", host.namespace);
            insert_optional_string(&mut fields, "container", host.container);
            insert_optional_string(&mut fields, "context", host.context);
        }
        TransportKind::Winrm => {
            insert_required_string(
                &mut fields,
                "address",
                host.address,
                format!("winrm host '{}' requires address", name),
            )?;
            insert_required_string(
                &mut fields,
                "user",
                host.user,
                format!("winrm host '{}' requires user", name),
            )?;
            insert_optional_string(&mut fields, "password", host.password);
            insert_optional_u16(&mut fields, "port", host.port);
            insert_optional_string(&mut fields, "scheme", host.scheme);
        }
    }
    Ok(InventoryHost { transport, fields })
}

fn insert_optional_string(
    fields: &mut HashMap<String, InventoryValue>,
    key: &str,
    value: Option<String>,
) {
    if let Some(value) = value {
        fields.insert(key.to_string(), InventoryValue::String(value));
    }
}

fn insert_optional_u16(
    fields: &mut HashMap<String, InventoryValue>,
    key: &str,
    value: Option<u16>,
) {
    if let Some(value) = value {
        fields.insert(key.to_string(), InventoryValue::U16(value));
    }
}

fn insert_required_string(
    fields: &mut HashMap<String, InventoryValue>,
    key: &str,
    value: Option<String>,
    error: String,
) -> Result<(), ShellError> {
    let value = value.ok_or_else(|| ShellError::Inventory(error))?;
    fields.insert(key.to_string(), InventoryValue::String(value));
    Ok(())
}
