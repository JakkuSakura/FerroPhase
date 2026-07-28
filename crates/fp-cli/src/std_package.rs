use std::collections::HashMap;

use fp_core::ast::Item;
use fp_core::frontend::LanguageFrontend;
use fp_core::module::path::QualifiedPath;
use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::{PackageDescriptor, PackageId};
use fp_core::vfs::VirtualPath;

/// Build a `PackageGraph` containing the embedded Ferro standard library.
/// Returns the graph and a map of pre-parsed module items (keyed by
/// qualified path) for injection into `TypingContext::std_items`.
pub fn build_ferro_std_package_graph() -> (PackageGraph, HashMap<QualifiedPath, Vec<Item>>) {
    let frontend = fp_lang::FerroFrontend::new();
    let std_root = fp_lang::embedded_std::root_dir();
    let package_id = PackageId::new("std");

    let mut descriptors: Vec<ModuleDescriptor> = Vec::new();
    let mut items_map: HashMap<QualifiedPath, Vec<Item>> = HashMap::new();

    for relative_str in fp_lang::embedded_std::module_paths() {
        let path = std_root.join(relative_str);
        let Some(source) = fp_lang::embedded_std::read(&path) else {
            continue;
        };

        let module_path = relative_path_to_module_segments(relative_str);
        if module_path.is_empty() {
            continue;
        }

        let result = match frontend.parse_file(source, &path) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("std package: failed to parse {relative_str}: {e}");
                continue;
            }
        };

        let items = extract_module_items(result.ast);
        if !items.is_empty() {
            items_map.insert(QualifiedPath::new(module_path.clone()), items);
        }

        descriptors.push(ModuleDescriptor {
            id: ModuleId::new(module_path.join("::")),
            package: package_id.clone(),
            language: ModuleLanguage::Ferro,
            module_path,
            source: VirtualPath::from_path(&path),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
    }

    let module_ids: Vec<_> = descriptors.iter().map(|d| d.id.clone()).collect();

    let package = PackageDescriptor {
        id: package_id,
        name: "std".to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&std_root.join("fp.toml")),
        root: VirtualPath::from_path(&std_root),
        metadata: Default::default(),
        modules: module_ids,
    };

    let mut graph = PackageGraph::new(vec![package]);
    for desc in descriptors {
        graph.insert_module(desc);
    }
    (graph, items_map)
}

/// Convert a relative std file path into module path segments.
fn relative_path_to_module_segments(relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec!["std".to_string()];
    let parts: Vec<&str> = relative.trim_end_matches(".fp").split('/').collect();

    if parts.len() == 1 && parts[0] == "mod" {
        return segments;
    }

    for part in parts {
        if part == "mod" {
            continue;
        }
        segments.push(part.to_string());
    }

    segments
}

/// Walk the parsed AST and collect top-level items.
fn extract_module_items(ast: fp_core::ast::Node) -> Vec<Item> {
    use fp_core::ast::NodeKind;
    match ast.kind() {
        NodeKind::File(file) => file.items.clone(),
        NodeKind::Item(item) => vec![(*item).clone()],
        _ => Vec::new(),
    }
}
