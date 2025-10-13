use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{File, Ident, Item, ItemKind, Module, Node, NodeKind, Visibility};
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::package::provider::{ModuleProvider, PackageProvider};

use crate::package::{CargoPackageProvider, RustModuleProvider};
use crate::parser::RustParser;

/// Parse every Rust module in the workspace described by the given Cargo manifest and return a
/// synthetic FerroPhase AST file that groups items under packages.
pub fn parse_cargo_workspace(manifest_path: &Path) -> CoreResult<Node> {
    let workspace_root = manifest_path
        .parent()
        .ok_or_else(|| CoreError::from("Cargo manifest has no parent directory"))?
        .to_path_buf();

    let provider = Arc::new(CargoPackageProvider::new(workspace_root));
    provider
        .refresh()
        .map_err(|err| CoreError::from(err.to_string()))?;
    let module_provider = RustModuleProvider::new(provider.clone());

    let mut package_items: BTreeMap<String, Vec<Item>> = BTreeMap::new();
    let mut visited_paths: HashSet<PathBuf> = HashSet::new();
    let mut parser = RustParser::new();

    for package_id in provider
        .list_packages()
        .map_err(|err| CoreError::from(err.to_string()))?
    {
        for module_id in module_provider
            .modules_for_package(&package_id)
            .map_err(|err| CoreError::from(err.to_string()))?
        {
            let descriptor = module_provider
                .load_module(&module_id)
                .map_err(|err| CoreError::from(err.to_string()))?;
            let path = descriptor.source.to_path_buf();
            let canonical = path.canonicalize().unwrap_or(path.clone());

            if !visited_paths.insert(canonical.clone()) {
                continue;
            }

            let source = fs::read_to_string(&canonical).map_err(CoreError::from)?;
            let file = parser.parse_file(&source, &canonical)?;
            let ast = Node::file(file);

            let entry = package_items
                .entry(package_id.as_str().to_string())
                .or_insert_with(Vec::new);

            match ast.kind() {
                NodeKind::File(file) => entry.extend(file.items.iter().cloned()),
                NodeKind::Item(item) => entry.push(item.clone()),
                NodeKind::Expr(expr) => entry.push(Item::from(ItemKind::Expr(expr.clone()))),
            }
        }
    }

    let mut items = Vec::new();
    for (package, package_ast) in package_items {
        if package_ast.is_empty() {
            continue;
        }
        let module = Module {
            name: Ident::new(sanitise_package_ident(&package)),
            items: package_ast,
            visibility: Visibility::Public,
        };
        items.push(Item::from(module));
    }

    let file = File {
        path: manifest_path.to_path_buf(),
        items,
    };

    Ok(Node::file(file))
}

fn sanitise_package_ident(name: &str) -> String {
    let mut ident = String::with_capacity(name.len());
    for ch in name.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ident.push(ch),
            '-' => ident.push('_'),
            _ => ident.push('_'),
        }
    }
    if ident.is_empty() {
        ident.push_str("pkg");
    }
    ident
}
