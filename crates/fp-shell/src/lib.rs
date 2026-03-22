mod embedded_std;
mod inventory;
mod lower;

use fp_core::ast::{
    Abi, AstTargetOutput, AttrMeta, AttributesExt, BlockStmt, Expr, ExprInvokeTarget, ExprKind,
    Ident, Item, ItemDeclFunction, ItemKind, Module, Name, Node, NodeKind, Value, Visibility,
};
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;
use std::fs;
use std::path::{Path, PathBuf};

pub use ScriptTarget as ShellTarget;
pub use inventory::load_inventory;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScriptTarget {
    Bash,
    PowerShell,
}

impl ScriptTarget {
    pub fn extension(self) -> &'static str {
        match self {
            Self::Bash => "sh",
            Self::PowerShell => "ps1",
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub inventory: Option<Node>,
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

    let ast = merge_runtime_helpers(parsed.ast, options.inventory.as_ref())?;

    let lowered = lower::lower_node(&ast).map_err(ShellError::Lower)?;
    let lowered = match lowered.kind() {
        NodeKind::File(file) => fp_backend::roundtrip_ast_file_via_hir(file)
            .map_err(|err| ShellError::Lower(err.to_string()))?,
        _ => {
            return Err(ShellError::Lower(
                "shell compilation requires file AST nodes".to_string(),
            ));
        }
    };
    validate_extern_decls(&lowered, target).map_err(ShellError::Lower)?;

    let code = match target {
        ScriptTarget::Bash => fp_bash::BashTarget::new()
            .render(&lowered, &Default::default())
            .map_err(|err| ShellError::Emit(err.to_string()))?,
        ScriptTarget::PowerShell => fp_powershell::PowerShellTarget::new()
            .render(&lowered, &Default::default())
            .map_err(|err| ShellError::Emit(err.to_string()))?,
    };

    Ok(AstTargetOutput {
        code,
        side_files: Vec::new(),
    })
}

fn merge_runtime_helpers(ast: Node, inventory: Option<&Node>) -> Result<Node, ShellError> {
    let frontend = FerroFrontend::new();
    let NodeKind::File(mut user_file) = ast.kind else {
        return Err(ShellError::Lower(
            "shell compilation requires file AST nodes".to_string(),
        ));
    };

    ensure_std_module(&mut user_file);
    merge_embedded_std_tree(&frontend, &mut user_file)?;
    if let Some(inventory) = inventory {
        merge_inventory_items(&frontend, &mut user_file, inventory)?;
    }
    Ok(Node::file(user_file))
}

fn merge_embedded_std_tree(
    frontend: &FerroFrontend,
    user_file: &mut fp_core::ast::File,
) -> Result<(), ShellError> {
    for embedded_path in embedded_std::paths() {
        let module_path = module_path_for_embedded_std(embedded_path)?;
        merge_embedded_std_items(frontend, user_file, &module_path, embedded_path)?;
    }
    Ok(())
}

fn ensure_std_module(user_file: &mut fp_core::ast::File) {
    let has_std = user_file.items.iter().any(
        |item| matches!(item.kind(), ItemKind::Module(module) if module.name.as_str() == "std"),
    );
    if !has_std {
        user_file.items.insert(
            0,
            Item::from(ItemKind::Module(Module {
                attrs: Vec::new(),
                name: Ident::new("std"),
                items: Vec::new(),
                visibility: Visibility::Public,
                is_external: false,
            })),
        );
    }
}

fn merge_embedded_std_items(
    frontend: &FerroFrontend,
    user_file: &mut fp_core::ast::File,
    module_path: &[&str],
    embedded_path: &str,
) -> Result<(), ShellError> {
    let source = embedded_std::read(embedded_path)
        .ok_or_else(|| ShellError::Lower(format!("missing embedded shell std: {embedded_path}")))?;
    let parsed = frontend
        .parse(
            source,
            Some(Path::new(&format!("<fp-shell-std>/{embedded_path}"))),
        )
        .map_err(|err| {
            ShellError::Lower(format!(
                "failed to parse embedded shell std {embedded_path}: {}",
                err
            ))
        })?;
    let NodeKind::File(file) = parsed.ast.kind else {
        return Err(ShellError::Lower(format!(
            "embedded shell std must be a file document: {embedded_path}"
        )));
    };
    let target = ensure_nested_module(user_file, module_path)?;
    target.items.extend(file.items);
    Ok(())
}

fn module_path_for_embedded_std(embedded_path: &str) -> Result<Vec<&str>, ShellError> {
    let without_suffix = embedded_path.strip_suffix(".fp").ok_or_else(|| {
        ShellError::Lower(format!(
            "embedded shell std path must end in .fp: {embedded_path}"
        ))
    })?;
    let segments = without_suffix
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        return Err(ShellError::Lower(format!(
            "embedded shell std path must contain a module name: {embedded_path}"
        )));
    }
    Ok(segments)
}

fn ensure_nested_module<'a>(
    user_file: &'a mut fp_core::ast::File,
    module_path: &[&str],
) -> Result<&'a mut Module, ShellError> {
    let std_module = user_file
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::Module(module) if module.name.as_str() == "std" => Some(module),
            _ => None,
        })
        .ok_or_else(|| ShellError::Lower("missing std module".to_string()))?;
    let mut current = std_module;
    for segment in module_path {
        let existing_index = current.items.iter().position(|item| {
            matches!(item.kind(), ItemKind::Module(module) if module.name.as_str() == *segment)
        });
        let index = match existing_index {
            Some(index) => index,
            None => {
                current.items.push(Item::from(ItemKind::Module(Module {
                    attrs: Vec::new(),
                    name: Ident::new(*segment),
                    items: Vec::new(),
                    visibility: Visibility::Public,
                    is_external: false,
                })));
                current.items.len() - 1
            }
        };
        current = match current.items[index].kind_mut() {
            ItemKind::Module(module) => module,
            _ => unreachable!(),
        };
    }
    Ok(current)
}

fn merge_inventory_items(
    frontend: &FerroFrontend,
    user_file: &mut fp_core::ast::File,
    inventory: &Node,
) -> Result<(), ShellError> {
    let NodeKind::File(inventory_file) = inventory.kind() else {
        return Err(ShellError::Inventory(
            "inventory must be a file document".to_string(),
        ));
    };
    let std_module = user_file
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::Module(module) if module.name.as_str() == "std" => Some(module),
            _ => None,
        })
        .ok_or_else(|| ShellError::Lower("missing std module after shell std merge".to_string()))?;
    let hosts_module = std_module
        .items
        .iter_mut()
        .find_map(|item| match item.kind_mut() {
            ItemKind::Module(module) if module.name.as_str() == "hosts" => Some(module),
            _ => None,
        })
        .ok_or_else(|| ShellError::Lower("missing std::hosts module".to_string()))?;
    hosts_module.items.extend(inventory_file.items.clone());
    let generated = generate_inventory_accessors(inventory)?;
    if !generated.is_empty() {
        let parsed = frontend
            .parse(
                &generated,
                Some(Path::new("<fp-shell-generated>/hosts_accessors.fp")),
            )
            .map_err(|err| {
                ShellError::Inventory(format!("invalid generated inventory accessors: {}", err))
            })?;
        let NodeKind::File(file) = parsed.ast.kind() else {
            return Err(ShellError::Inventory(
                "generated inventory accessors must be a file document".to_string(),
            ));
        };
        hosts_module.items.extend(file.items.clone());
    }
    Ok(())
}

fn generate_inventory_accessors(inventory: &Node) -> Result<String, ShellError> {
    let Some(inventory_expr) = inventory_root_expr(inventory) else {
        return Err(ShellError::Inventory(
            "inventory fp must define `const fn inventory() -> Inventory`".to_string(),
        ));
    };
    let hosts = inventory_hosts(inventory_expr);
    let groups = inventory_groups(inventory_expr);
    let mut source = String::new();
    source.push_str("pub const fn transport(host: str) -> str {\n    match host {\n");
    for host in &hosts {
        source.push_str(&format!(
            "        {:?} => {:?},\n",
            host.name, host.transport
        ));
    }
    source.push_str("        _ => \"\",\n    }\n}\n\n");
    for field in [
        "address",
        "user",
        "password",
        "port",
        "container",
        "pod",
        "namespace",
        "context",
        "scheme",
        "chroot_directory",
    ] {
        source.push_str(&format!(
            "pub const fn {field}(host: str) -> str {{\n    match host {{\n"
        ));
        for host in &hosts {
            let value = host.fields.get(field).cloned().unwrap_or_default();
            source.push_str(&format!("        {:?} => {:?},\n", host.name, value));
        }
        source.push_str("        _ => \"\",\n    }\n}\n\n");
    }
    source.push_str("pub const fn group_hosts(name: str) -> any {\n    match name {\n");
    for group in groups {
        let members = group
            .hosts
            .iter()
            .map(|host| format!("{host:?}"))
            .collect::<Vec<_>>()
            .join(", ");
        source.push_str(&format!("        {:?} => [{}],\n", group.name, members));
    }
    source.push_str("        _ => [],\n    }\n}\n");
    Ok(source)
}

#[derive(Default)]
struct InventoryHostAccessor {
    name: String,
    transport: String,
    fields: std::collections::HashMap<String, String>,
}

struct InventoryGroupAccessor {
    name: String,
    hosts: Vec<String>,
}

fn inventory_root_expr(node: &Node) -> Option<&Expr> {
    let NodeKind::File(file) = node.kind() else {
        return None;
    };
    let function = file.items.iter().find_map(|item| match item.kind() {
        ItemKind::DefFunction(function) if function.name.as_str() == "inventory" => Some(function),
        _ => None,
    })?;
    function_result_expr(&function.body)
}

fn function_result_expr(expr: &Expr) -> Option<&Expr> {
    match expr.kind() {
        ExprKind::Block(block) => block.stmts.iter().rev().find_map(|stmt| match stmt {
            BlockStmt::Expr(expr) => Some(expr.expr.as_ref()),
            _ => None,
        }),
        _ => Some(expr),
    }
}

fn inventory_hosts(expr: &Expr) -> Vec<InventoryHostAccessor> {
    let Some(hosts_expr) = struct_field_expr(expr, "hosts") else {
        return Vec::new();
    };
    let Some(entries) = hashmap_from_entries(hosts_expr) else {
        return Vec::new();
    };
    entries
        .into_iter()
        .filter_map(|(key, value)| {
            let name = string_literal_value(key)?;
            let transport = struct_field_expr(value, "transport").and_then(string_literal_value)?;
            let mut fields = std::collections::HashMap::new();
            for field in [
                "address",
                "user",
                "password",
                "port",
                "container",
                "pod",
                "namespace",
                "context",
                "scheme",
                "chroot_directory",
            ] {
                if let Some(value) =
                    struct_field_expr(value, field).and_then(stringish_literal_value)
                {
                    fields.insert(field.to_string(), value);
                }
            }
            if let Some(value) =
                struct_field_expr(value, "directory").and_then(stringish_literal_value)
            {
                fields.insert("chroot_directory".to_string(), value);
            }
            Some(InventoryHostAccessor {
                name,
                transport,
                fields,
            })
        })
        .collect()
}

fn inventory_groups(expr: &Expr) -> Vec<InventoryGroupAccessor> {
    let Some(groups_expr) = struct_field_expr(expr, "groups") else {
        return Vec::new();
    };
    let Some(entries) = hashmap_from_entries(groups_expr) else {
        return Vec::new();
    };
    entries
        .into_iter()
        .filter_map(|(key, value)| {
            Some(InventoryGroupAccessor {
                name: string_literal_value(key)?,
                hosts: string_list_literal_values(value)?,
            })
        })
        .collect()
}

fn struct_field_expr<'a>(expr: &'a Expr, field: &str) -> Option<&'a Expr> {
    let fields = match expr.kind() {
        ExprKind::Struct(expr_struct) => &expr_struct.fields,
        ExprKind::Structural(expr_structural) => &expr_structural.fields,
        _ => return None,
    };
    fields
        .iter()
        .find(|candidate| candidate.name.as_str() == field)
        .and_then(|candidate| candidate.value.as_ref())
}

fn hashmap_from_entries(expr: &Expr) -> Option<Vec<(&Expr, &Expr)>> {
    match expr.kind() {
        ExprKind::IntrinsicContainer(container) => match container {
            fp_core::ast::ExprIntrinsicContainer::HashMapEntries { entries } => Some(
                entries
                    .iter()
                    .map(|entry| (&entry.key, &entry.value))
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        },
        ExprKind::Invoke(invoke) => {
            let target = invoke_target_segments(&invoke.target)?;
            match target.as_slice() {
                [.., owner, method] if owner == "HashMap" && method == "from" => {}
                _ => return None,
            }
            let [entries_expr] = invoke.args.as_slice() else {
                return None;
            };
            tuple_like_values(entries_expr)?
                .into_iter()
                .map(|entry| match entry.kind() {
                    ExprKind::Tuple(tuple) if tuple.values.len() == 2 => {
                        Some((&tuple.values[0], &tuple.values[1]))
                    }
                    _ => None,
                })
                .collect()
        }
        _ => None,
    }
}

fn string_list_literal_values(expr: &Expr) -> Option<Vec<String>> {
    tuple_like_values(expr)?
        .into_iter()
        .map(string_literal_value)
        .collect()
}

fn tuple_like_values(expr: &Expr) -> Option<Vec<&Expr>> {
    match expr.kind() {
        ExprKind::Array(array) => Some(array.values.iter().collect()),
        ExprKind::Tuple(tuple) => Some(tuple.values.iter().collect()),
        ExprKind::IntrinsicContainer(container) => match container {
            fp_core::ast::ExprIntrinsicContainer::VecElements { elements } => {
                Some(elements.iter().collect())
            }
            _ => None,
        },
        ExprKind::Paren(paren) => tuple_like_values(&paren.expr),
        ExprKind::Struct(expr_struct) if expr_struct.fields.is_empty() => Some(Vec::new()),
        ExprKind::Structural(expr_structural) if expr_structural.fields.is_empty() => {
            Some(Vec::new())
        }
        _ => None,
    }
}

fn string_literal_value(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match &**value {
            Value::String(text) => Some(text.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn stringish_literal_value(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match &**value {
            Value::String(text) => Some(text.value.clone()),
            Value::Int(value) => Some(value.value.to_string()),
            Value::Bool(value) => Some(value.value.to_string()),
            _ => None,
        },
        _ => None,
    }
}

fn invoke_target_segments(target: &ExprInvokeTarget) -> Option<Vec<String>> {
    match target {
        ExprInvokeTarget::Function(name) => Some(name_to_segments(name)),
        _ => None,
    }
}

fn name_to_segments(name: &Name) -> Vec<String> {
    name.to_path()
        .segments
        .iter()
        .map(|ident| ident.as_str().to_string())
        .collect()
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

fn validate_extern_decl(function: &ItemDeclFunction, target: ScriptTarget) -> Result<(), String> {
    let expected_abi = match target {
        ScriptTarget::Bash => "bash",
        ScriptTarget::PowerShell => "pwsh",
    };
    let abi = match &function.sig.abi {
        Abi::Rust => {
            return Err(format!(
                "extern `{}` uses ABI `rust`, but shell target requires `{}`",
                function.name, expected_abi
            ));
        }
        Abi::Named(name) => name.as_str(),
    };
    if abi != expected_abi {
        return Err(format!(
            "extern `{}` uses ABI `{}`, but shell target requires `{}`",
            function.name, abi, expected_abi
        ));
    }
    let command = extern_command(function);
    if command.is_none() && !is_runtime_primitive(function.name.as_str()) {
        return Err(format!(
            "extern `{}` is missing #[command = \"...\"] for {} shell target",
            function.name, expected_abi
        ));
    }
    if command
        .as_deref()
        .is_some_and(|value| value.trim().is_empty())
    {
        return Err(format!(
            "extern `{}` has an empty #[command] annotation",
            function.name
        ));
    }
    Ok(())
}

fn is_runtime_primitive(name: &str) -> bool {
    matches!(
        name,
        "runtime_temp_path" | "runtime_fail" | "runtime_set_changed" | "runtime_last_changed"
    )
}

fn extern_command(function: &ItemDeclFunction) -> Option<String> {
    let AttrMeta::NameValue(meta) = function.attrs.find_by_name("command")? else {
        return None;
    };
    let ExprKind::Value(value) = meta.value.kind() else {
        return None;
    };
    let Value::String(text) = &**value else {
        return None;
    };
    Some(text.value.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn compile_source_generates_bash_script() {
        let source = r#"
const fn main() {
    std::ops::server::shell("echo hello");
}
"#;

        let rendered = compile_source(source, Path::new("sample.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("#!/usr/bin/env bash"));
        assert!(rendered.code.contains("echo hello"));
        assert!(!rendered.code.contains("shell_copy() {"));
        assert!(!rendered.code.contains("winrm_pwsh() {"));
    }

    #[test]
    fn compile_source_generates_powershell_script() {
        let source = r#"
const fn main() {
    std::ops::server::shell("Write-Host hello");
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
    std::ops::files::rsync(
        src="./dist/",
        dest="C:/deploy/dist/",
        hosts="win-1",
        delete=true,
    );
}
"#;

        let directory = tempfile::tempdir().expect("tempdir should be created");
        let inventory_path = directory.path().join("inventory.fp");
        fs::write(
            &inventory_path,
            r#"
use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("win-1", Host {
                transport: "winrm",
                address: "10.0.0.21",
                user: "Administrator",
                port: 2222,
            }),
        ]),
    }
}
"#,
        )
        .expect("inventory should be written");
        let inventory = load_inventory(&inventory_path).expect("inventory should parse");

        let rendered = compile_source_with_options(
            source,
            Path::new("sample.fp"),
            ScriptTarget::PowerShell,
            &CompileOptions {
                inventory: Some(inventory),
            },
        )
        .expect("source should compile");

        assert!(
            rendered
                .code
                .contains("__fp_std_shell_backend_rsync_remote_target_")
        );
        assert!(rendered.code.contains("Administrator"));
        assert!(rendered.code.contains("10.0.0.21"));
        assert!(rendered.code.contains("rsync -e"));
    }

    #[test]
    fn compile_source_supports_try_else_finally_and_defer_in_bash() {
        let source = r#"
const fn main() {
    try {
        defer std::ops::server::shell("echo cleanup");
        std::ops::server::shell("echo body");
    } catch err {
        std::ops::server::shell(f"echo failed={err}");
    } else {
        std::ops::server::shell("echo success");
    } finally {
        std::ops::server::shell("echo finally");
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

        assert!(
            rendered
                .code
                .contains("__fp_std_facts_has_command_ 'rsync'")
        );
        assert!(
            rendered
                .code
                .contains("local cmd=\"$(__fp_std_shell_process_process_pipe_")
        );
        assert!(rendered.code.contains("/tmp/build.log"));
    }

    #[test]
    fn compile_source_supports_with_host_context() {
        let source = r#"
const fn main() {
    with "web-1" {
        std::ops::server::shell("echo hello");
        std::ops::services::restart("nginx");
    }
}
"#;

        let rendered = compile_source(source, Path::new("with.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("echo hello"));
        assert!(rendered.code.contains("systemctl restart ${name}"));
        assert!(rendered.code.contains("web-1"));
    }

    #[test]
    fn compile_source_supports_context_params_without_host_name_convention() {
        let source = r#"
const fn deploy(command: str, context target: str) {
    std::ops::server::shell(command, hosts=target);
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
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_server_shell_ \"${command}\" \"${target}\"")
        );
    }

    #[test]
    fn compile_source_supports_package_manager_modules() {
        let source = r#"
const fn main() {
    std::ops::apt::packages("curl git", update=true);
    std::ops::apk::packages("bash", update=true, upgrade=true);
    std::ops::yum::packages("vim", clean=true);
    std::ops::dnf::packages("jq", latest=true);
    std::ops::pacman::packages("ripgrep", update=true);
    std::ops::brew::packages("wget", update=true);
    std::ops::choco::packages("notepadplusplus");
    std::ops::npm::packages("typescript", latest=true, directory="/srv/app");
    std::ops::pip::packages("pyinfra", pip="pip3", latest=true);
    std::ops::cargo::packages("ripgrep");
    std::ops::pipx::packages("ruff", latest=true);
    std::ops::gem::packages("bundler");
    std::ops::flatpak::packages("org.videolan.VLC", remote="flathub");
    std::ops::snap::packages("lxd", channel="4.0/stable", latest=true);
    std::ops::zypper::packages("vim", clean=true);
    std::ops::pkg::packages("tmux");
    std::ops::opkg::packages("vim");
    std::ops::pkgin::packages("htop", update=true);

    let apt_sources = std::facts::apt::sources();
    let apk_packages = std::facts::apk::packages();
    let yum_repos = std::facts::yum::repositories();
    let dnf_repos = std::facts::dnf::repositories();
    let pacman_packages = std::facts::pacman::packages();
    let brew_packages = std::facts::brew::packages();
    let choco_packages = std::facts::choco::packages();
    let npm_packages = std::facts::npm::packages("/srv/app");
    let pip_packages = std::facts::pip::packages("pip3");
    let cargo_packages = std::facts::cargo::packages();
    let pipx_packages = std::facts::pipx::packages();
    let gem_packages = std::facts::gem::packages();
    let flatpak_packages = std::facts::flatpak::packages();
    let snap_packages = std::facts::snap::packages();
    let zypper_repos = std::facts::zypper::repositories();
    let pkg_packages = std::facts::pkg::packages();
    let opkg_packages = std::facts::opkg::packages();
    let pkgin_packages = std::facts::pkgin::packages();

    std::ops::server::shell(apt_sources);
    std::ops::server::shell(apk_packages);
    std::ops::server::shell(yum_repos);
    std::ops::server::shell(dnf_repos);
    std::ops::server::shell(pacman_packages);
    std::ops::server::shell(brew_packages);
    std::ops::server::shell(choco_packages);
    std::ops::server::shell(npm_packages);
    std::ops::server::shell(pip_packages);
    std::ops::server::shell(cargo_packages);
    std::ops::server::shell(pipx_packages);
    std::ops::server::shell(gem_packages);
    std::ops::server::shell(flatpak_packages);
    std::ops::server::shell(snap_packages);
    std::ops::server::shell(zypper_repos);
    std::ops::server::shell(pkg_packages);
    std::ops::server::shell(opkg_packages);
    std::ops::server::shell(pkgin_packages);
}
"#;

        let rendered = compile_source(source, Path::new("packages.fp"), ScriptTarget::Bash)
            .expect("source should compile");

        assert!(rendered.code.contains("apt-get install -y ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_apt_packages_ 'curl git'")
        );
        assert!(rendered.code.contains("apk add ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_apk_packages_ 'bash'"));
        assert!(rendered.code.contains("yum install -y ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_yum_packages_ 'vim'"));
        assert!(rendered.code.contains("dnf update -y ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_dnf_packages_ 'jq'"));
        assert!(rendered.code.contains("pacman --noconfirm -S ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_pacman_packages_ 'ripgrep'")
        );
        assert!(rendered.code.contains("brew install ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_brew_packages_ 'wget'"));
        assert!(rendered.code.contains("choco install -y ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_choco_packages_ 'notepadplusplus'")
        );
        assert!(rendered.code.contains("npm update ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_npm_packages_ 'typescript'")
        );
        assert!(
            rendered
                .code
                .contains("${pip} install --upgrade ${packages}")
        );
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_pip_packages_ 'pyinfra'")
        );
        assert!(rendered.code.contains("cargo install ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_cargo_packages_ 'ripgrep'")
        );
        assert!(rendered.code.contains("pipx upgrade ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_pipx_packages_ 'ruff'"));
        assert!(rendered.code.contains("gem install ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_gem_packages_ 'bundler'")
        );
        assert!(rendered.code.contains("flatpak install --noninteractive"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_flatpak_packages_ 'org.videolan.VLC'")
        );
        assert!(
            rendered
                .code
                .contains("snap refresh ${packages} --channel=${channel}")
        );
        assert!(rendered.code.contains("__fp_std_ops_snap_packages_ 'lxd'"));
        assert!(
            rendered
                .code
                .contains("zypper --non-interactive install -y ${packages}")
        );
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_zypper_packages_ 'vim'")
        );
        assert!(rendered.code.contains("pkg install -y ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_pkg_packages_ 'tmux'"));
        assert!(rendered.code.contains("opkg install ${packages}"));
        assert!(rendered.code.contains("__fp_std_ops_opkg_packages_ 'vim'"));
        assert!(rendered.code.contains("pkgin -y install ${packages}"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_pkgin_packages_ 'htop'")
        );
        assert!(rendered.code.contains("apt/sources.list"));
        assert!(rendered.code.contains("apk list --installed"));
        assert!(rendered.code.contains("cargo install --list"));
        assert!(rendered.code.contains("pipx list --short"));
        assert!(rendered.code.contains("gem list --local"));
        assert!(rendered.code.contains("flatpak list --columns=application"));
        assert!(rendered.code.contains("snap list"));
        assert!(rendered.code.contains("/etc/zypp/repos.d/*.repo"));
        assert!(rendered.code.contains("pkg info || pkg_info || true"));
        assert!(rendered.code.contains("/bin/opkg list-installed"));
        assert!(rendered.code.contains("pkgin list"));
    }

    #[test]
    fn compile_source_supports_remaining_pyinfra_modules() {
        let source = r#"
const fn main() {
    std::ops::bsdinit::service("sshd", enabled=true);
    std::ops::crontab::crontab("echo hello", cron_name="demo", minute="0", hour="1");
    std::ops::docker::image("nginx:alpine");
    std::ops::iptables::chain("FP_TEST");
    std::ops::launchd::service("com.example.demo", running=false);
    std::ops::lxd::container("demo", present=false);
    std::ops::mysql::sql("SELECT 1");
    std::ops::mysql::user("app", password="secret", privileges="ALL");
    std::ops::mysql::database("appdb", charset="utf8mb4", collate="utf8mb4_unicode_ci");
    std::ops::openrc::service("sshd", enabled=true);
    std::ops::postgres::sql("SELECT 1");
    std::ops::postgresql::sql("SELECT 1");
    std::ops::puppet::agent(server="puppet.example.com", port="8140");
    std::ops::python::call("print(1)");
    std::ops::runit::manage("demo", managed=false);
    std::ops::selinux::boolean("httpd_can_network_connect", "on", persistent=true);
    std::ops::ssh::command("example.com", "echo hi", user="deploy", port="2222");
    std::ops::sysvinit::enable("cron", start_levels="2 3", stop_levels="0 1 6");
    std::ops::upstart::service("cron", enabled=false);
    std::ops::vzctl::start("101", force=true);
    std::ops::xbps::packages("vim", update=true);
    std::ops::zfs::filesystem("tank/data", present=false);

    let bsdinit = std::facts::bsdinit::status();
    let crons = std::facts::crontab::entries("");
    let deb_arch = std::facts::deb::arch();
    let efi = std::facts::efibootmgr::info();
    let freebsd = std::facts::freebsd::service_status("sshd", "");
    let gpg_keys = std::facts::gpg::keys("");
    let cpus = std::facts::hardware::cpus();
    let iptables = std::facts::iptables::chains("filter", "4");
    let launchd = std::facts::launchd::status();
    let lxd = std::facts::lxd::containers();
    let mysql_users = std::facts::mysql::users("", "", "", "");
    let openrc = std::facts::openrc::status("default");
    let podman = std::facts::podman::system_info();
    let postgres = std::facts::postgres::roles("", "", "", "", "");
    let postgresql = std::facts::postgresql::databases("", "", "", "", "");
    let rpm = std::facts::rpm::packages();
    let runit = std::facts::runit::status("", "/var/service");
    let selinux = std::facts::selinux::ports();
    let sysv = std::facts::sysvinit::status();
    let upstart = std::facts::upstart::status();
    let vzctl = std::facts::vzctl::containers();
    let xbps = std::facts::xbps::packages();
    let zfs = std::facts::zfs::pools();

    std::ops::server::shell(bsdinit);
    std::ops::server::shell(crons);
    std::ops::server::shell(deb_arch);
    std::ops::server::shell(efi);
    std::ops::server::shell(freebsd);
    std::ops::server::shell(gpg_keys);
    std::ops::server::shell(cpus);
    std::ops::server::shell(iptables);
    std::ops::server::shell(launchd);
    std::ops::server::shell(lxd);
    std::ops::server::shell(mysql_users);
    std::ops::server::shell(openrc);
    std::ops::server::shell(podman);
    std::ops::server::shell(postgres);
    std::ops::server::shell(postgresql);
    std::ops::server::shell(rpm);
    std::ops::server::shell(runit);
    std::ops::server::shell(selinux);
    std::ops::server::shell(sysv);
    std::ops::server::shell(upstart);
    std::ops::server::shell(vzctl);
    std::ops::server::shell(xbps);
    std::ops::server::shell(zfs);
}
"#;

        let rendered = compile_source(
            source,
            Path::new("remaining-pyinfra.fp"),
            ScriptTarget::Bash,
        )
        .expect("source should compile");

        assert!(rendered.code.contains("launchctl list"));
        assert!(rendered.code.contains("lxc list --format json --fast"));
        assert!(rendered.code.contains("mysql"));
        assert!(rendered.code.contains("psql"));
        assert!(rendered.code.contains("xbps-query -l"));
        assert!(rendered.code.contains("zpool get -H all"));
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_docker_image_ 'nginx:alpine'")
        );
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_postgres_sql_ 'SELECT 1'")
        );
        assert!(
            rendered
                .code
                .contains("__fp_std_ops_ssh_command_ 'example.com'")
        );
        assert!(rendered.code.contains("__fp_std_ops_vzctl_start_ '101'"));
    }

    #[test]
    fn load_inventory_supports_multiple_transports() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.fp");
        fs::write(
            &path,
            r#"
use std::collections::HashMap;
use std::hosts::{Host, Inventory};

const fn inventory() -> Inventory {
    Inventory {
        groups: HashMap::from([]),
        hosts: HashMap::from([
            ("app", Host {
                transport: "docker",
                container: "app-container",
            }),
            ("api", Host {
                transport: "kubectl",
                pod: "api-pod",
                namespace: "prod",
            }),
            ("win", Host {
                transport: "winrm",
                address: "10.0.0.21",
                user: "Administrator",
                password: "secret",
            }),
        ]),
    }
}
"#,
        )
        .expect("inventory should be written");

        let _inventory = load_inventory(&path).expect("inventory should parse");
    }

    #[test]
    fn load_inventory_supports_fp_format() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.fp");
        fs::write(
            &path,
            r#"
use std::collections::HashMap;
use std::hosts::{Host, Inventory};

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

        let _inventory = load_inventory(&path).expect("fp inventory should parse");
    }

    #[test]
    fn load_inventory_supports_fp_multiple_transports() {
        let directory = tempfile::tempdir().expect("tempdir should be created");
        let path = directory.path().join("inventory.fp");
        fs::write(
            &path,
            r#"
use std::collections::HashMap;
use std::hosts::{Host, Inventory};

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

        let _inventory = load_inventory(&path).expect("fp inventory should parse");
    }
}
