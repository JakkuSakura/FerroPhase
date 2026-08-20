//! A runtime extension point for compile targets that live outside the
//! `fp-cli` crate (and thus outside its own Cargo workspace) — e.g.
//! `skln-fp-graph`'s `fp-graph` binary, which lives in the outer SakuraLens
//! workspace and cannot be a dependency of `fp-cli` without reversing the
//! `FerroPhase` git submodule relationship.
//!
//! Registered targets are plain `fp_core::backend::TargetBackend` impls —
//! the exact same trait every built-in target implements (see
//! `commands::compile::backend_for_target`). fp-cli has no separate
//! "external target" protocol; a registered backend is looked up by name
//! and driven through `compile_package`/`write_workspace_files` exactly
//! like a built-in one.

use std::sync::{Arc, Mutex, OnceLock};

use fp_core::backend::TargetBackend;

static REGISTRY: OnceLock<Mutex<Vec<(&'static str, Arc<dyn TargetBackend>)>>> = OnceLock::new();

fn registry() -> &'static Mutex<Vec<(&'static str, Arc<dyn TargetBackend>)>> {
    REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
}

/// Registers a backend so `--target <name>` can resolve to it. Expected to
/// be called by the embedding binary's `main()` before it calls
/// `commands::compile::compile_command`.
pub fn register_target_backend(name: &'static str, backend: Arc<dyn TargetBackend>) {
    registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push((name, backend));
}

/// Looks up a previously `register_target_backend`-ed backend by name,
/// case-insensitively.
pub fn find_registered_target_backend(name: &str) -> Option<Arc<dyn TargetBackend>> {
    let normalized = name.to_lowercase();
    registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .find(|(registered_name, _)| registered_name.eq_ignore_ascii_case(&normalized))
        .map(|(_, backend)| backend.clone())
}
