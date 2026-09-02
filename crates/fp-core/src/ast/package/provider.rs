use std::path::Path;
use std::sync::Arc;

use crate::ast::module::ModuleId;
use crate::ast::package::{AstPackage, PackageDescriptor, PackageId, PackageMetadata};
use crate::vfs::VirtualPath;

pub type ProviderResult<T> = Result<T, ProviderError>;

#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("package not found: {0}")]
    PackageNotFound(PackageId),
    #[error("module not found: {0}")]
    ModuleNotFound(ModuleId),
    #[error("metadata error: {0}")]
    Metadata(String),
    #[error("{0}")]
    Other(String),
}

impl ProviderError {
    pub fn metadata(err: impl Into<String>) -> Self {
        Self::Metadata(err.into())
    }

    pub fn other(message: impl Into<String>) -> Self {
        Self::Other(message.into())
    }
}

pub trait PackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>>;
    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>>;
    fn refresh(&self) -> ProviderResult<()>;

    /// Load a package's modules. Discovery, parsing, and graph construction
    /// are the implementor's job. The returned `AstPackage`'s `items` and
    /// `graph` are populated; compiler-owned registries are left empty for
    /// the compiler to fill in.
    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage>;

    /// Packages this provider considers part of the *current workspace*,
    /// as opposed to packages it can merely also supply (e.g. `std`,
    /// blended in by `CompositeProvider` alongside the real project
    /// provider). Deliberately no default body: "all of `list_packages()`"
    /// is only correct for a provider that exclusively serves one
    /// project's own packages, and a provider that blends in others (like
    /// `CompositeProvider`) getting this wrong by inheriting that default
    /// would be an easy, silent mistake — every implementor states it
    /// explicitly instead.
    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>>;

    /// The `IntrinsicNormalizer` a compile of this provider's own source
    /// language should use — e.g. real Rust's `RustPackageProvider` hands
    /// back one that knows how to disambiguate a same-named `macro_rules!`
    /// collision (see `fp_rust::RustIntrinsicNormalizer`'s own doc comment
    /// for the exact real vendored-std case that motivated this). Lives
    /// here — on the already-resolved, already-per-language provider —
    /// rather than a separate registry: a compile always already has
    /// exactly one provider in hand by the time it needs a normalizer, so
    /// there is no second per-language lookup to build. Deliberately no
    /// default body, matching `workspace_packages`'s own rationale above:
    /// a provider for a macro-free language (C, native object/asm, ...)
    /// still states `NoopIntrinsicNormalizer` itself, rather than
    /// silently inheriting it and this decision going unnoticed if that
    /// default is ever revisited.
    fn intrinsic_normalizer(&self) -> Box<dyn crate::intrinsics::IntrinsicNormalizer>;

    /// Language declaration policy used by the AST resolver. Providers may
    /// override this for language-specific namespace and import semantics.
    fn declaration_rules(&self) -> crate::hir::resolve::DeclarationRules {
        crate::hir::resolve::DeclarationRules::default()
    }

    /// Language lookup policy used by the AST resolver.
    fn resolution_rules(&self) -> crate::hir::resolve::ResolutionRules {
        crate::hir::resolve::ResolutionRules::default()
    }
}

/// A `PackageProvider` that always hands back one already-built
/// `AstPackage` — for tests that construct `ast::Item`s directly with
/// no real frontend/disk parsing involved. Every real provider still
/// builds a `AstPackage` directly (e.g. `FerroPhaseProvider`'s
/// `load_embedded_package`); this just skips the "discover it from disk"
/// step, while still requiring callers to obtain their `AstPackage`
/// through the normal `PackageProvider` -> `AstProgram::begin_package`
/// path rather than hand-rolling one.
pub struct FixedPackageProvider {
    package_id: PackageId,
    descriptor: Arc<PackageDescriptor>,
    source: AstPackage,
}

impl FixedPackageProvider {
    pub fn new(descriptor: PackageDescriptor, source: AstPackage) -> Self {
        Self {
            package_id: descriptor.id.clone(),
            descriptor: Arc::new(descriptor),
            source,
        }
    }

    /// Convenience constructor for tests that don't care about manifest
    /// metadata at all — builds a minimal descriptor with an empty root
    /// path.
    pub fn for_source(package_id: PackageId, source: AstPackage) -> Self {
        let descriptor = PackageDescriptor {
            id: package_id.clone(),
            name: package_id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::new_relative(Vec::<String>::new()),
            root: VirtualPath::new_relative(Vec::<String>::new()),
            metadata: PackageMetadata::default(),
        };
        Self::new(descriptor, source)
    }
}

impl PackageProvider for FixedPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![self.package_id.clone()])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.descriptor.clone())
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        if id != &self.package_id {
            return Err(ProviderError::PackageNotFound(id.clone()));
        }
        Ok(self.source.clone())
    }

    fn intrinsic_normalizer(&self) -> Box<dyn crate::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::intrinsics::NoopIntrinsicNormalizer)
    }
}

/// A `PackageProvider` with no packages at all — for the handful of generic
/// constructors (`CompilerDriver::new`, `CompilerState::new`, standalone
/// tests) that need to build a `AstProgram` before any real provider
/// is known; a real one is attached later via a fresh `AstProgram`
/// built with it once the caller knows what it's compiling.
pub struct EmptyProvider;

impl PackageProvider for EmptyProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(Vec::new())
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(Vec::new())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        Err(ProviderError::PackageNotFound(id.clone()))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        Err(ProviderError::PackageNotFound(id.clone()))
    }

    fn intrinsic_normalizer(&self) -> Box<dyn crate::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::intrinsics::NoopIntrinsicNormalizer)
    }
}

/// Combines several already-chosen concrete `PackageProvider`s (e.g. a
/// language's std/libc provider plus the real input-package provider) into
/// one — `AstProgram` holds exactly one required provider, so any
/// caller that needs more than one source composes them here before
/// constructing the workspace. Not a language-dispatch mechanism: every
/// sub-provider is picked by the caller ahead of time, same as if only one
/// provider were being registered.
///
/// `dependencies` and `workspace` are kept distinct (rather than one flat
/// list) so `workspace_packages()` can report only the real project's own
/// packages — `dependencies` (e.g. `std`/`libc`) are reachable through
/// `list_packages()` like any other package, but aren't part of the
/// current workspace.
pub struct CompositeProvider {
    dependencies: Vec<Arc<dyn PackageProvider>>,
    workspace: Arc<dyn PackageProvider>,
}

impl CompositeProvider {
    pub fn new(
        dependencies: Vec<Arc<dyn PackageProvider>>,
        workspace: Arc<dyn PackageProvider>,
    ) -> Self {
        Self {
            dependencies,
            workspace,
        }
    }

    fn all_providers(&self) -> impl Iterator<Item = &Arc<dyn PackageProvider>> {
        self.dependencies
            .iter()
            .chain(std::iter::once(&self.workspace))
    }

    /// The sub-provider whose own `list_packages()` includes `id` — bounded
    /// by `self.all_providers().count()` (2 in every real call site today),
    /// not by workspace size, so a plain linear scan here is fine.
    fn provider_for(&self, id: &PackageId) -> Option<&Arc<dyn PackageProvider>> {
        self.all_providers().find(|provider| {
            provider
                .list_packages()
                .map(|packages| packages.iter().any(|candidate| candidate == id))
                .unwrap_or(false)
        })
    }
}

impl PackageProvider for CompositeProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(self
            .all_providers()
            .filter_map(|provider| provider.list_packages().ok())
            .flatten()
            .collect())
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.workspace.workspace_packages()
    }

    /// Delegates to the primary project provider (`self.workspace`), not
    /// the blended-in `dependencies` (e.g. std) — a workspace's own
    /// macro/intrinsic semantics follow its own source language, not
    /// whichever language its std happens to be authored in.
    fn intrinsic_normalizer(&self) -> Box<dyn crate::intrinsics::IntrinsicNormalizer> {
        self.workspace.intrinsic_normalizer()
    }

    fn declaration_rules(&self) -> crate::hir::resolve::DeclarationRules {
        self.workspace.declaration_rules()
    }

    fn resolution_rules(&self) -> crate::hir::resolve::ResolutionRules {
        self.workspace.resolution_rules()
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        self.provider_for(id)
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))?
            .load_package_metadata(id)
    }

    fn refresh(&self) -> ProviderResult<()> {
        for provider in self.all_providers() {
            provider.refresh()?;
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        self.provider_for(id)
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))?
            .load_package_source(id)
    }
}

/// Reads `root` as text and lifts it via `parse` into a target-independent
/// `crate::lir::LirBlob`, wrapping it as a one-package, one-item
/// provider (`AstPackage::single_item` + `Item::precompiled_lir`) — the
/// shared shape for any language whose input parses straight to LIR
/// (goasm, urcl, ...), with no language-specific knowledge here.
pub fn lir_from_text(
    root: &Path,
    parse: impl FnOnce(&str) -> crate::error::Result<crate::lir::LirBlob>,
) -> Option<Arc<dyn PackageProvider>> {
    let text = std::fs::read_to_string(root).ok()?;
    let lir = parse(&text).ok()?;
    let name = root
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main")
        .to_string();
    let package_id = PackageId::new(name);
    let source =
        AstPackage::single_item(package_id.clone(), crate::ast::Item::precompiled_lir(lir));
    Some(Arc::new(FixedPackageProvider::for_source(package_id, source)) as Arc<dyn PackageProvider>)
}
