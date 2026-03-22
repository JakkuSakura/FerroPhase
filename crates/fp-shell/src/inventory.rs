use crate::ShellError;
use fp_core::ast::{ItemKind, Node, NodeKind};
use fp_core::frontend::LanguageFrontend;
use fp_lang::FerroFrontend;
use std::fs;
use std::path::Path;

pub fn load_inventory(path: &Path) -> Result<Node, ShellError> {
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or_default()
        .to_ascii_lowercase();
    if extension != "fp" {
        return Err(ShellError::Inventory(
            "inventory must be defined in .fp".to_string(),
        ));
    }

    let content = fs::read_to_string(path)?;
    let frontend = FerroFrontend::new();
    let parsed = frontend
        .parse(&content, Some(path))
        .map_err(|err| ShellError::Inventory(format!("invalid inventory fp: {}", err)))?;
    let NodeKind::File(file) = parsed.ast.kind() else {
        return Err(ShellError::Inventory(
            "inventory fp must be a file document".to_string(),
        ));
    };
    let has_inventory = file.items.iter().any(|item| {
        matches!(
            item.kind(),
            ItemKind::DefFunction(function) if function.name.as_str() == "inventory"
        )
    });
    if !has_inventory {
        return Err(ShellError::Inventory(
            "inventory fp must define `const fn inventory() -> Inventory`".to_string(),
        ));
    }
    Ok(parsed.ast)
}
