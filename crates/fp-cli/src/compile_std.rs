use std::collections::HashMap;
use std::rc::Rc;

use fp_core::ast::{File, Item};
use fp_core::frontend::LanguageFrontend;
use fp_core::module::path::QualifiedPath;
use fp_core::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::package::graph::PackageGraph;
use fp_core::package::{PackageDescriptor, PackageId};
use fp_core::vfs::VirtualPath;
use fp_core::workspace::WorkspaceContext;
use fp_typing::{AstTypeInferencer, TypingContext, default_extern_prelude};

/// Compile the embedded Ferro standard library into a `PackageCrate`
/// and add it to the `WorkspaceContext`.  Returns the populated context.
pub fn build_workspace_with_std() -> WorkspaceContext {
    let frontend = fp_lang::FerroFrontend::new();
    let std_root = fp_lang::embedded_std::root_dir();
    let package_id = PackageId::new("std");
    let mut descriptors: Vec<ModuleDescriptor> = Vec::new();
    let mut items_by_path: HashMap<QualifiedPath, Vec<Item>> = HashMap::new();

    // Parse all std files, collect module descriptors and items
    for relative_str in fp_lang::embedded_std::module_paths() {
        let path = std_root.join(relative_str);
        let Some(source) = fp_lang::embedded_std::read(&path) else { continue };

        let module_path = relative_to_module_segments(relative_str);
        if module_path.is_empty() { continue; }

        let result = match frontend.parse_file(source, &path) {
            Ok(r) => r,
            Err(_) => continue,
        };

        let items = extract_module_items(result.ast);
        let qpath = QualifiedPath::new(module_path.clone());
        if !items.is_empty() {
            items_by_path.insert(qpath, items);
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

    // Build the package graph
    let module_ids: Vec<_> = descriptors.iter().map(|d| d.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
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

    // Compile items into typing tables
    let env_ctx = Rc::new(WorkspaceContext::new());
    let typing_ctx = Rc::new(TypingContext::new(env_ctx.clone()));
    let mut inferencer = AstTypeInferencer::new(typing_ctx.clone())
        .with_extern_prelude(default_extern_prelude());

    for (path, items) in &items_by_path {
        inferencer.inject_module(path, items);
    }

    let mut std_crate = inferencer.into_package_crate("std", graph);
    // Save parsed items for on-demand LIR compilation by the CompilerDriver
    std_crate.items = items_by_path;
    // LIR bodies for std modules are compiled on-demand by the
    // CompilerDriver when needed for comptime evaluation.

    let mut workspace = WorkspaceContext::new();
    workspace.push_crate(std_crate);
    workspace
}

fn relative_to_module_segments(relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec!["std".to_string()];
    let parts: Vec<&str> = relative.trim_end_matches(".fp").split('/').collect();
    if parts.len() == 1 && parts[0] == "mod" {
        return segments;
    }
    for part in parts {
        if part == "mod" { continue; }
        segments.push(part.to_string());
    }
    segments
}

fn extract_module_items(file: File) -> Vec<Item> {
    file.items
}
