//! A runtime extension point for compile targets *and* source-language
//! providers that live outside the `fp-cli` crate (and thus outside its
//! own Cargo workspace) — e.g. `skln-fp-graph`'s `fp-graph` binary, which
//! lives in the outer SakuraLens workspace and cannot be a dependency of
//! `fp-cli` without reversing the `FerroPhase` git submodule relationship.
//!
//! Registered targets are plain `fp_core::backend::TargetBackend` impls —
//! the exact same trait every built-in target implements (see
//! `commands::compile::backend_for_target`). fp-cli has no separate
//! "external target" protocol; a registered backend is looked up by name
//! and driven through `compile_package`/`write_workspace_files` exactly
//! like a built-in one. Registered source-language providers are plain
//! `fp_core::package::provider::PackageProvider` factories, looked up the
//! same way by `package_provider_registry::provider_for_language` (see
//! `register_language_provider`).

use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use fp_core::backend::TargetBackend;
use fp_core::package::provider::PackageProvider;

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

/// A source-language provider factory — `root` is either a project
/// directory or a single standalone file (mirroring every built-in
/// per-language provider's own `::new`/`::discover`, e.g.
/// `fp_rust::RustPackageProvider`); returns `None` if `root` isn't
/// something this language's provider can handle. Boxed so a caller can
/// register a plain closure without defining a named type.
pub type LanguageProviderFactory =
    Arc<dyn Fn(&Path) -> Option<Arc<dyn PackageProvider>> + Send + Sync>;

static LANGUAGE_PROVIDER_REGISTRY: OnceLock<Mutex<Vec<(&'static str, LanguageProviderFactory)>>> =
    OnceLock::new();

/// Seeded once with every built-in language's own factory
/// (`package_provider_registry::builtin_language_providers`) — built-ins and anything an
/// embedding binary later registers live in the exact same table, looked
/// up the exact same way; there's no separate "check built-ins first"
/// step.
fn language_provider_registry() -> &'static Mutex<Vec<(&'static str, LanguageProviderFactory)>> {
    LANGUAGE_PROVIDER_REGISTRY
        .get_or_init(|| Mutex::new(super::package_provider_registry::builtin_language_providers()))
}

/// Registers a `PackageProvider` factory for `name` so `--source-language
/// <name>` (or extension-based auto-detection, once also registered in
/// `languages::SUPPORTED_LANGUAGES`) resolves to it — the source-provider
/// analogue of `register_target_backend`, for an embedding binary that
/// wants to add a language `fp-cli` itself has no crate dependency on.
/// Expected to be called from the embedding binary's `main()` before it
/// calls `commands::compile::compile_command`.
pub fn register_language_provider(
    name: &'static str,
    factory: impl Fn(&Path) -> Option<Arc<dyn PackageProvider>> + Send + Sync + 'static,
) {
    language_provider_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push((name, Arc::new(factory)));
}

/// Looks up a previously `register_language_provider`-ed factory by name,
/// case-insensitively, and invokes it with `root`.
pub fn find_registered_language_provider(name: &str, root: &Path) -> Option<Arc<dyn PackageProvider>> {
    let normalized = name.to_lowercase();
    let factory = language_provider_registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .find(|(registered_name, _)| registered_name.eq_ignore_ascii_case(&normalized))
        .map(|(_, factory)| factory.clone())?;
    factory(root)
}
