use fp_core::ast::{AttrMeta, AttributesExt, ExprKind, ItemDeclFunction, Value};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct ShellInventory {
    pub groups: HashMap<String, Vec<String>>,
    pub hosts: HashMap<String, InventoryHost>,
}

#[derive(Debug, Clone)]
pub struct InventoryHost {
    pub transport: TransportKind,
    pub fields: HashMap<String, InventoryValue>,
}

impl Default for InventoryHost {
    fn default() -> Self {
        Self {
            transport: TransportKind::Ssh,
            fields: HashMap::new(),
        }
    }
}

impl InventoryHost {
    pub fn get_string(&self, name: &str) -> Option<&str> {
        match self.fields.get(name) {
            Some(InventoryValue::String(value)) => Some(value.as_str()),
            _ => None,
        }
    }

    pub fn get_u16(&self, name: &str) -> Option<u16> {
        match self.fields.get(name) {
            Some(InventoryValue::U16(value)) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TransportKind {
    Local,
    Ssh,
    Docker,
    Kubectl,
    Winrm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InventoryValue {
    String(String),
    U16(u16),
}

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

fn is_runtime_primitive(name: &str) -> bool {
    matches!(
        name,
        "runtime_host_transport"
            | "runtime_host_address"
            | "runtime_host_user"
            | "runtime_host_port"
            | "runtime_host_container"
            | "runtime_host_pod"
            | "runtime_host_namespace"
            | "runtime_host_context"
            | "runtime_host_password"
            | "runtime_host_scheme"
            | "runtime_temp_path"
            | "runtime_fail"
            | "runtime_set_changed"
            | "runtime_last_changed"
    )
}

pub fn validate_extern_decl(
    function: &ItemDeclFunction,
    target: ScriptTarget,
) -> Result<(), String> {
    let expected_abi = match target {
        ScriptTarget::Bash => "bash",
        ScriptTarget::PowerShell => "pwsh",
    };
    let abi = match &function.sig.abi {
        fp_core::ast::Abi::Rust => {
            return Err(format!(
                "extern `{}` uses ABI `rust`, but shell target requires `{}`",
                function.name, expected_abi
            ));
        }
        fp_core::ast::Abi::Named(name) => name.as_str(),
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

pub fn runtime_requirements(function: &ItemDeclFunction, target: ScriptTarget) -> Vec<String> {
    if let Some(command) = extern_command(function) {
        return command
            .split_whitespace()
            .next()
            .map(|tool| vec![tool.to_string()])
            .unwrap_or_default();
    }
    match (target, function.name.as_str()) {
        (ScriptTarget::Bash, "runtime_temp_path") => vec!["mktemp".to_string()],
        _ => Vec::new(),
    }
}

pub fn extern_command(function: &ItemDeclFunction) -> Option<String> {
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
