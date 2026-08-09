use std::path::{Path, PathBuf};

/// Language-specific project discovery functions.
/// Each `fp-{lang}` crate provides its own implementation.
pub struct ProjectDiscovery {
    pub find_manifest: fn(&Path) -> Option<PathBuf>,
    pub list_members: fn(&Path) -> Vec<(String, PathBuf)>,
    pub list_sources: fn(&Path) -> Vec<(String, PathBuf)>,
}

/// Resolve discovery functions for a source language.
/// Mirrors the pattern in `compiler.rs::select_frontend()`.
pub fn discovery_for_language(language: &str) -> Option<&'static ProjectDiscovery> {
    match language {
        "ferrophase" | "rust" | "rs" | "fp" => Some(&RUST_DISCOVERY),
        _ => None,
    }
}

static RUST_DISCOVERY: ProjectDiscovery = ProjectDiscovery {
    find_manifest: fp_lang::project::find_manifest,
    list_members: fp_lang::project::list_members,
    list_sources: fp_lang::project::list_sources,
};
