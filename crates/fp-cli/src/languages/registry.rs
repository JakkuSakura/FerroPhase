//! A runtime extension point for compile targets that live outside the
//! `fp-cli` crate (and thus outside its own Cargo workspace) — e.g.
//! `skln-fp-graph`'s `fp-graph` binary, which lives in the outer SakuraLens
//! workspace and cannot be a dependency of `fp-cli` without reversing the
//! `FerroPhase` git submodule relationship.
//!
//! `LanguageTarget` is named the same as (and modeled after) what
//! `crate::languages::backend::BuiltinLanguageTarget` used to be called,
//! because it is meant to be the *universal* shape a compile target
//! implements — not a special "external-only" side channel. Only
//! externally-registered targets implement this trait today; migrating
//! built-in targets (Kotlin, TypeScript, ...) off
//! `BuiltinLanguageTarget`'s match-arm dispatch onto this trait is future
//! work, out of scope here.
//!
//! The trait models a *collector*, not a one-shot file transform, so a
//! target can accumulate facts across every package in a workspace before
//! producing its output — symmetric with how Kotlin already collects
//! cross-package facts (mutated fields, list/string fields, ...) before
//! serializing (`commands/compile.rs`'s `compile_project`, phase 1 vs.
//! phase 2).

use std::sync::{Arc, Mutex, OnceLock};

/// The universal shape of a compile target: something that collects facts
/// from each package as fp-cli's normal package/typecheck pipeline visits
/// it, then serializes the accumulated result once the whole workspace has
/// been walked.
pub trait LanguageTarget: Send + Sync {
    /// The canonical name a user passes via `--target <name>`.
    fn name(&self) -> &'static str;

    /// Additional strings that should also resolve to this target.
    fn aliases(&self) -> &[&'static str] {
        &[]
    }

    /// File extension used when no explicit `--output` is given.
    fn output_extension(&self) -> &'static str;

    /// Called once per package as `compile_project`/`compile_project_external`'s
    /// package-discovery loop (`PackageProvider`/`ContainerRegistry`) visits
    /// it, after that package has gone through the same `typecheck_package`
    /// step every built-in target uses (subject to the same `--skip-typing`
    /// escape hatch a user may pass). Implementations accumulate internal
    /// state here — the "collect" half, run inline with discovery, not as a
    /// separate pass.
    fn visit_package(
        &self,
        package: &LanguageTargetPackage<'_>,
        ctx: &LanguageTargetContext,
    ) -> fp_core::Result<()>;

    /// Called once after every package in the workspace has been visited.
    /// Implementations serialize their accumulated state here — the
    /// "serialize" half.
    fn finish(&self) -> fp_core::Result<fp_core::ast::AstTargetOutput>;
}

/// One package's worth of (typechecked, unless `--skip-typing`) source
/// handed to `LanguageTarget::visit_package`.
///
/// A real `fp_core::package::PackageSource` spans potentially many source
/// files/modules within the package (see `PackageSource::items`'s doc
/// comment) — there is no single `file_path` per package to hand out here,
/// unlike a plain single-file `AstSerializer`. Each item still carries its
/// own fully-qualified module path (`PackageItem::path`), which a target can
/// use to recover file/module-level grouping itself if it needs it (e.g. by
/// calling `fp_core::package::split_package_into_modules`).
pub struct LanguageTargetPackage<'a> {
    pub package_id: &'a fp_core::package::PackageId,
    pub items: &'a [fp_core::package::PackageItem],
}

pub struct LanguageTargetContext {
    pub project_root: std::path::PathBuf,
}

static REGISTRY: OnceLock<Mutex<Vec<Arc<dyn LanguageTarget>>>> = OnceLock::new();

fn registry() -> &'static Mutex<Vec<Arc<dyn LanguageTarget>>> {
    REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
}

/// Registers a target so `--target <name>` (or one of its `aliases()`) can
/// resolve to it. Expected to be called by the embedding binary's `main()`
/// before it calls `commands::compile::compile_command`.
pub fn register_language_target(target: Arc<dyn LanguageTarget>) {
    registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push(target);
}

/// Looks up a previously `register_language_target`-ed target by name or
/// alias, case-insensitively (matching `parse_language_target`'s own
/// case-insensitive lookup for built-in names).
pub fn find_registered_language_target(name: &str) -> Option<Arc<dyn LanguageTarget>> {
    let normalized = name.to_lowercase();
    registry()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .find(|target| {
            target.name().eq_ignore_ascii_case(&normalized)
                || target
                    .aliases()
                    .iter()
                    .any(|alias| alias.eq_ignore_ascii_case(&normalized))
        })
        .cloned()
}
