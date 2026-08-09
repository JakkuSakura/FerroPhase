use std::path::Path;
use std::sync::Arc;

use fp_core::package::provider::PackageProvider;
use fp_lang::cargo_provider::CargoWorkspaceProvider;

/// Factory: maps a source language to a PackageProvider implementation.
pub fn provider_for_language(lang: &str, root: &Path) -> Option<Arc<dyn PackageProvider>> {
    match lang {
        "ferrophase" | "rust" | "rs" | "fp" => {
            CargoWorkspaceProvider::discover(root).ok().map(|p| Arc::new(p) as Arc<dyn PackageProvider>)
        }
        _ => None,
    }
}
