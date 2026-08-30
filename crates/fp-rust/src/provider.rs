use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    AstPackage, DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata,
};
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::{AttrMeta, Attribute, Item, ItemKind, register_threadlocal_serializer};
use fp_core::cfg::{TargetEnv, item_enabled_by_cfg};
use fp_core::frontend::LanguageFrontend;
use fp_core::vfs::VirtualPath;
use fp_lang::{FerroFrontend, project};

use crate::RustFrontend;

/// `PackageProvider` for real `.rs`/Cargo-based projects (as opposed to
/// `fp_lang::magnet_provider::MagnetWorkspaceProvider`'s own `.fp`/Magnet
/// dialect).
///
/// Workspace discovery reuses `fp_lang::project` (the same Cargo/Magnet
/// manifest walking `MagnetWorkspaceProvider` uses), but parsing goes through
/// `RustFrontend` specifically — kept as its own path (rather than
/// delegating to `MagnetWorkspaceProvider` wholesale) so Rust-specific parsing
/// work has a real seam to land in without touching `.fp`-dialect behavior.
/// A member's own source root — either a real directory (walked via
/// `project::list_sources`, the ordinary Cargo-project case) or a single
/// standalone file with no enclosing project (`fp compile foo.rs` with no
/// `Cargo.toml` anywhere above it) — the degenerate one-module package
/// case, always tagged as the crate root (empty module path) regardless of
/// the file's own name, matching how `lib.rs`/`main.rs` already collapse
/// to the crate root in the directory case.
enum MemberRoot {
    Dir(PathBuf),
    File(PathBuf),
}

impl MemberRoot {
    fn manifest_path(&self) -> PathBuf {
        match self {
            MemberRoot::Dir(dir) => dir.join("Cargo.toml"),
            MemberRoot::File(path) => path.clone(),
        }
    }

    fn root_path(&self) -> &Path {
        match self {
            MemberRoot::Dir(dir) => dir,
            MemberRoot::File(path) => path,
        }
    }
}

fn rust_source_files(package_root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_rust_source_files(&package_root.join("src"), &mut files);
    files
}

fn collect_rust_source_files(directory: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(directory) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rust_source_files(&path, files);
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("rs") {
            files.push(path);
        }
    }
}

fn hash_source_bytes(hash: &mut u64, bytes: &[u8]) {
    for byte in bytes {
        *hash ^= u64::from(*byte);
        *hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    // Delimit source paths and contents so distinct sequences cannot merge.
    *hash ^= 0xff;
    *hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
}

pub struct RustPackageProvider {
    members: Vec<(String, MemberRoot)>,
    cache: RwLock<HashMap<String, (String, Vec<PackageItem>)>>,
    disk_cache: fp_core::cache::DiskCache,
}

impl RustPackageProvider {
    fn source_fingerprint(member_root: &MemberRoot) -> String {
        let mut sources = match member_root {
            MemberRoot::File(path) => vec![path.clone()],
            // This deliberately includes every Rust file in a package rather
            // than guessing its module graph. `#[path]`, generated module
            // trees, and cfg-selected modules can otherwise make a child
            // source invisible to cache invalidation. Extra invalidations are
            // cheap; serving an AST for an old child module is not.
            MemberRoot::Dir(dir) => rust_source_files(dir),
        };
        sources.sort();

        // Keep the cache key stable across processes. `DefaultHasher` is not
        // a persistence format, so use a small fixed FNV-1a accumulator over
        // both source names and bytes.
        let mut hash = 0xcbf2_9ce4_8422_2325_u64;
        for path in sources {
            hash_source_bytes(&mut hash, path.to_string_lossy().as_bytes());
            match std::fs::read(&path) {
                Ok(bytes) => hash_source_bytes(&mut hash, &bytes),
                Err(_) => hash_source_bytes(&mut hash, b"<unreadable>"),
            }
        }
        format!("{hash:016x}")
    }

    pub fn new(root: PathBuf) -> Self {
        // A standalone file (no enclosing Cargo project) is its own
        // one-member package, named after itself — the degenerate case of
        // "package", not a separate code path.
        if root.is_file() {
            let cache_root = root
                .parent()
                .unwrap_or(Path::new("."))
                .join("target/fp-cache");
            let name = root
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main")
                .to_string();
            return Self {
                members: vec![(name, MemberRoot::File(root))],
                cache: RwLock::new(HashMap::new()),
                disk_cache: fp_core::cache::DiskCache::new(cache_root),
            };
        }
        // `list_cargo_members`, not `list_members`: this provider is
        // specifically for real Rust/Cargo projects, so `Cargo.toml` is
        // authoritative here even if a stale/unrelated `Magnet.toml` also
        // exists at the same root — see that function's doc comment.
        let members = cargo_workspace_root(&root)
            .map(|workspace_root| project::list_cargo_members(&workspace_root))
            .unwrap_or_default()
            .into_iter()
            .map(|(name, dir)| {
                // Cargo identifies a package by `[package].name`, not by the
                // directory containing its manifest.  Workspace members are
                // commonly named alike, but path dependencies are allowed to
                // point at a differently named package (and can also be
                // renamed in the dependency table).
                let package_name = cargo_package_name(&dir).unwrap_or(name);
                (package_name, MemberRoot::Dir(dir))
            })
            .collect();
        Self {
            members,
            cache: RwLock::new(HashMap::new()),
            disk_cache: fp_core::cache::DiskCache::new(root.join("target/fp-cache")),
        }
    }

    pub fn discover(root: &Path) -> ProviderResult<Self> {
        Ok(Self::new(root.to_path_buf()))
    }

    fn resolve_root(&self, id: &PackageId) -> ProviderResult<&MemberRoot> {
        self.members
            .iter()
            .find(|(name, _)| name == id.as_str())
            .map(|(_, root)| root)
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }
}

impl PackageProvider for RustPackageProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(self
            .members
            .iter()
            .map(|(name, _)| PackageId::new(name))
            .collect())
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    /// Real Rust source's own frontend engine — see
    /// `RustIntrinsicNormalizer`'s doc comment for the vendored-std
    /// `uint_impl!` collision it disambiguates.
    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::normalizer::RustIntrinsicNormalizer::new())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let member_root = self.resolve_root(id)?;
        // Real module discovery (below, in `load_package_source`) walks
        // `mod` declarations from the crate root, which this method has no
        // reason to duplicate — run it once here too and let its result
        // land in `self.cache`, so `load_package_source`'s own cache check
        // picks it straight back up instead of re-walking.
        let items = self.package_items(id, member_root)?;
        let module_ids: Vec<_> = {
            use std::collections::HashSet;
            let paths: HashSet<_> = items.iter().map(|item| item.module_path.clone()).collect();
            paths
                .into_iter()
                .map(|path| ModuleId::new(&path.to_key()))
                .collect()
        };
        let mut metadata = PackageMetadata::default();
        metadata.prelude = Some(PackageId::new(STD_PACKAGE_NAME));
        metadata.dependencies.extend(implicit_rust_dependencies());
        if let MemberRoot::Dir(dir) = member_root {
            metadata
                .dependencies
                .extend(workspace_path_dependencies(dir, &self.members));
            metadata.dependencies.extend(registry_api_dependencies(dir));
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&member_root.manifest_path()),
            root: VirtualPath::from_path(member_root.root_path()),
            metadata,
            modules: module_ids,
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        if let Ok(mut c) = self.cache.write() {
            c.clear();
        }
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        let member_root = self.resolve_root(id)?;
        let items = self.package_items(id, member_root)?;
        let metadata = self.load_package_metadata(id)?.metadata.clone();
        Ok(package_source_from_items(id, &items, metadata))
    }
}

impl RustPackageProvider {
    /// Real module discovery: start at this member's crate-root file
    /// (`src/lib.rs`/`src/main.rs` by convention, or the file itself for a
    /// standalone single-file member) and recursively resolve every `mod
    /// name;` it transitively declares to its backing file — a `#[path]`
    /// override first, else the `name.rs`/`name/mod.rs` convention —
    /// instead of independently guessing each on-disk file's module path
    /// from its own filesystem location (`project::list_sources` has no
    /// `mod`-graph awareness at all). Cached per package id, shared by
    /// `load_package_metadata` and `load_package_source` so whichever
    /// runs first does the real work.
    fn package_items(
        &self,
        id: &PackageId,
        member_root: &MemberRoot,
    ) -> ProviderResult<Vec<PackageItem>> {
        let fingerprint = Self::source_fingerprint(member_root);
        let cache_key = format!("rust/package-source/{id}/{fingerprint}");
        if let Ok(c) = self.cache.read() {
            if let Some((cached_fingerprint, items)) = c.get(id.as_str())
                && cached_fingerprint == &fingerprint
            {
                return Ok(items.clone());
            }
        }
        if let Ok(Some(bytes)) = self.disk_cache.get(&cache_key) {
            if let Ok(cached) = serde_json::from_slice::<Vec<(Vec<String>, Item)>>(&bytes) {
                let items = cached
                    .into_iter()
                    .map(|(module_path, item)| PackageItem {
                        module_path: QualifiedPath::new(module_path),
                        item,
                    })
                    .collect::<Vec<_>>();
                if let Ok(mut c) = self.cache.write() {
                    c.insert(id.as_str().to_string(), (fingerprint, items.clone()));
                }
                return Ok(items);
            }
        }

        let (root_file, base_dir) = match member_root {
            MemberRoot::Dir(dir) => {
                let src = dir.join("src");
                let lib_rs = src.join("lib.rs");
                let root_file = if lib_rs.is_file() {
                    lib_rs
                } else {
                    src.join("main.rs")
                };
                (root_file, src)
            }
            MemberRoot::File(path) => (
                path.clone(),
                path.parent().unwrap_or(Path::new(".")).to_path_buf(),
            ),
        };

        let env = TargetEnv::host();
        let mut parse = |path: &Path, source: &str| -> ProviderResult<Vec<Item>> {
            let frontend = RustFrontend::new();
            let result = frontend
                .parse_file(source, path)
                .map_err(|e| ProviderError::other(format!("parse {}: {}", path.display(), e)))?;
            // The typed-HIR pipeline (Display/Debug-formatting AST nodes for
            // diagnostics, etc.) panics without a thread-local serializer
            // registered — `parse_file_with_context`'s single-file path
            // already does this; this provider-based path didn't.
            register_threadlocal_serializer(result.serializer.clone());
            Ok(result.ast.items)
        };
        let read = |path: &Path| -> Option<String> { std::fs::read_to_string(path).ok() };

        let mut items = Vec::new();
        if let Some(source) = read(&root_file) {
            let root_items = parse(&root_file, &source)?;
            let file_dir = root_file.parent().unwrap_or(&base_dir);
            let children_base_dir = children_base_dir_for(&root_file);
            discover_items(
                &read,
                &mut parse,
                &env,
                file_dir,
                &children_base_dir,
                &QualifiedPath::new(Vec::new()),
                &root_items,
                None,
                &mut items,
            )?;
        }

        if let Ok(mut c) = self.cache.write() {
            c.insert(id.as_str().to_string(), (fingerprint, items.clone()));
        }
        let serializable = items
            .iter()
            .map(|item| (item.module_path.segments.clone(), item.item.clone()))
            .collect::<Vec<_>>();
        if let Ok(bytes) = serde_json::to_vec(&serializable) {
            let _ = self.disk_cache.put(&cache_key, &bytes);
        }
        Ok(items)
    }
}

/// Reads `dir/Cargo.toml`'s `[dependencies]` table and resolves every
/// `path = "..."` entry to the sibling workspace member it points at —
/// FerroPhase's analogue of rustc's `--extern` crate-metadata wiring: a
/// package can only see another package's real, typed definitions (struct
/// fields, etc.) once it's a recorded dependency here, which is what lets
/// `CompilerDriver::compile_package`'s existing dependency loop recurse
/// into it and register it in the depending package's own workspace
/// (`AstProgram::crates()`/`hir_definitions()`). Non-path
/// dependencies (crates.io/registry deps) are skipped — there's no
/// provider for arbitrary external crates, so recording an unresolvable
/// `DependencyDescriptor` would just make `compile_package`'s dependency
/// loop error out.
fn workspace_path_dependencies(
    package_dir: &Path,
    members: &[(String, MemberRoot)],
) -> Vec<DependencyDescriptor> {
    let manifest_path = package_dir.join("Cargo.toml");
    let Ok(content) = std::fs::read_to_string(&manifest_path) else {
        return Vec::new();
    };
    let Ok(manifest) = content.parse::<toml::Value>() else {
        return Vec::new();
    };
    let canonical_members: Vec<(String, PathBuf)> = members
        .iter()
        .map(|(name, root)| {
            let path = root.root_path();
            (
                name.clone(),
                std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf()),
            )
        })
        .collect();

    let mut result = Vec::new();
    for (table_name, kind) in [
        ("dependencies", DependencyKind::Normal),
        ("dev-dependencies", DependencyKind::Development),
        ("build-dependencies", DependencyKind::Build),
    ] {
        let Some(dependencies) = manifest.get(table_name).and_then(|v| v.as_table()) else {
            continue;
        };
        result.extend(dependencies.iter().filter_map(|(dep_name, spec)| {
            let relative_path = spec.get("path")?.as_str()?;
            let absolute_path = package_dir.join(relative_path);
            let canonical_path = std::fs::canonicalize(&absolute_path).unwrap_or(absolute_path);
            let (member_name, _) = canonical_members
                .iter()
                .find(|(_, member_path)| *member_path == canonical_path)?;
            Some(DependencyDescriptor {
                package: dep_name.clone(),
                resolved_package_id: Some(PackageId::new(member_name)),
                constraint: None,
                kind: kind.clone(),
                features: Vec::new(),
                optional: spec
                    .get("optional")
                    .and_then(toml::Value::as_bool)
                    .unwrap_or(false),
                target: Default::default(),
            })
        }));
    }
    result
}

/// Registry dependencies which have a declared portability surface. They are
/// loaded as ordinary typed packages so calls resolve by definition identity
/// before a target backend materializes them.
fn external_api_dependency(name: &str, kind: DependencyKind) -> Option<DependencyDescriptor> {
    matches!(name, "serde_json" | "toml" | "tokio" | "winnow").then(|| DependencyDescriptor {
        package: name.to_owned(),
        resolved_package_id: Some(PackageId::new(name)),
        constraint: None,
        kind,
        features: Vec::new(),
        optional: false,
        target: Default::default(),
    })
}

fn registry_api_dependencies(package_dir: &Path) -> Vec<DependencyDescriptor> {
    let Ok(content) = std::fs::read_to_string(package_dir.join("Cargo.toml")) else {
        return Vec::new();
    };
    let Ok(manifest) = content.parse::<toml::Value>() else {
        return Vec::new();
    };
    [
        ("dependencies", DependencyKind::Normal),
        ("dev-dependencies", DependencyKind::Development),
        ("build-dependencies", DependencyKind::Build),
    ]
    .into_iter()
    .flat_map(|(table_name, kind)| {
        manifest
            .get(table_name)
            .and_then(toml::Value::as_table)
            .into_iter()
            .flat_map(move |dependencies| {
                let kind = kind.clone();
                dependencies
                    .keys()
                    .filter_map(move |name| external_api_dependency(name, kind.clone()))
            })
    })
    .collect()
}

fn cargo_package_name(package_dir: &Path) -> Option<String> {
    let content = std::fs::read_to_string(package_dir.join("Cargo.toml")).ok()?;
    let manifest = content.parse::<toml::Value>().ok()?;
    manifest
        .get("package")
        .and_then(|package| package.get("name"))
        .and_then(toml::Value::as_str)
        .map(str::to_string)
}

/// Find the Cargo workspace containing the package selected by `root`.
///
/// `project::find_manifest` intentionally returns the nearest manifest. That
/// is the package manifest when the provider is created for a workspace
/// member, but the provider needs the enclosing workspace manifest to expose
/// sibling packages as dependencies. Only an ancestor with an explicit
/// `[workspace].members` entry resolving back to the selected package is
/// accepted, so an unrelated parent Cargo project cannot capture a standalone
/// package by accident.
fn cargo_workspace_root(root: &Path) -> Option<PathBuf> {
    let package_root = nearest_cargo_root(root)?;
    let package_root = std::fs::canonicalize(&package_root).unwrap_or(package_root);
    if cargo_manifest_has_workspace(&package_root) {
        return Some(package_root);
    }
    let mut candidate = package_root.as_path();

    loop {
        if candidate.join("Cargo.toml").is_file()
            && cargo_manifest_has_workspace(candidate)
            && project::list_cargo_members(candidate)
                .into_iter()
                .any(|(_, member)| same_path(&member, &package_root))
        {
            return Some(candidate.to_path_buf());
        }

        candidate = candidate.parent()?;
    }
}

fn nearest_cargo_root(root: &Path) -> Option<PathBuf> {
    let mut current = if root.is_dir() {
        root.to_path_buf()
    } else {
        root.parent()?.to_path_buf()
    };

    loop {
        if current.join("Cargo.toml").is_file() {
            return Some(current);
        }
        current = current.parent()?.to_path_buf();
    }
}

fn cargo_manifest_has_workspace(root: &Path) -> bool {
    std::fs::read_to_string(root.join("Cargo.toml"))
        .ok()
        .and_then(|content| content.parse::<toml::Value>().ok())
        .and_then(|manifest| manifest.get("workspace").cloned())
        .is_some()
}

fn same_path(left: &Path, right: &Path) -> bool {
    let left = std::fs::canonicalize(left).unwrap_or_else(|_| left.to_path_buf());
    let right = std::fs::canonicalize(right).unwrap_or_else(|_| right.to_path_buf());
    left == right
}

/// Computes the flat, file-derived `PackageItem` path tag for a source file
/// relative to a package's source root (e.g. `"config.rs"` → `["config"]`).
/// Exported so callers outside this module (e.g. a single-file compile that
/// wants to match a real package's own tagging) can compute the same tag.
pub fn rs_relative_to_module_path(rel: &str) -> QualifiedPath {
    // The crate root file (`lib.rs`/`main.rs`, never nested in a
    // subdirectory) defines crate-root-level items directly, not a `lib::`/
    // `main::` submodule — tag it with an empty path so
    // `AstToHirLowerer::transform_package`'s per-item `with_module_scope` (which
    // pushes one scope level per path segment) doesn't wrongly nest them.
    if rel.is_empty() || (!rel.contains('/') && (rel == "lib.rs" || rel == "main.rs")) {
        return QualifiedPath::new(Vec::new());
    }
    let stem = rel.trim_end_matches(".rs").trim_end_matches(".fp");
    let mut parts: Vec<String> = stem.split('/').map(|s| s.to_string()).collect();
    // Pre-2018-edition module file convention: `foo/mod.rs` *is* module
    // `foo` itself, not a `mod` submodule nested inside it — drop the
    // trailing "mod" segment `mod.rs` would otherwise contribute.
    if parts.len() > 1 && parts.last().map(String::as_str) == Some("mod") {
        parts.pop();
    }
    QualifiedPath::new(parts)
}

fn package_source_from_items(
    id: &PackageId,
    items: &[PackageItem],
    metadata: PackageMetadata,
) -> AstPackage {
    use std::collections::HashSet;
    let paths: HashSet<_> = items.iter().map(|item| item.module_path.clone()).collect();
    let descriptors: Vec<ModuleDescriptor> = paths
        .into_iter()
        .map(|path| ModuleDescriptor {
            id: ModuleId::new(&path.to_key()),
            package: id.clone(),
            language: ModuleLanguage::Rust,
            module_path: path.segments.clone(),
            source: VirtualPath::from_path(Path::new(".")),
            exports: Vec::new(),
            requires_features: Vec::new(),
        })
        .collect();
    let module_ids: Vec<_> = descriptors.iter().map(|d| d.id.clone()).collect();
    let package = PackageDescriptor {
        id: id.clone(),
        name: id.as_str().to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(Path::new("Cargo.toml")),
        root: VirtualPath::from_path(Path::new(".")),
        metadata,
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for desc in descriptors {
        graph.insert_module(desc);
    }
    let mut source = AstPackage::new(id.clone(), id.as_str(), graph);
    source.items = items.to_vec();
    source
}

const CORE_PACKAGE_NAME: &str = "core";
const ALLOC_PACKAGE_NAME: &str = "alloc";
const STD_PACKAGE_NAME: &str = "std";
const TEST_PACKAGE_NAME: &str = "test";
const LIBC_PACKAGE_NAME: &str = "libc";

/// Dependency rustc injects into an ordinary Rust crate's extern prelude.
/// `alloc` and `core` are reached through `std`'s dependency graph; they are
/// not direct dependencies of the consumer crate.
fn implicit_rust_dependencies() -> Vec<DependencyDescriptor> {
    [STD_PACKAGE_NAME]
        .into_iter()
        .map(|package| DependencyDescriptor {
            package: package.to_string(),
            resolved_package_id: Some(PackageId::new(package)),
            constraint: None,
            kind: DependencyKind::Normal,
            features: Vec::new(),
            optional: false,
            target: Default::default(),
        })
        .collect()
}

/// Real Rust sysroots vendor `core`/`alloc`/`std` as three independent
/// crates — `alloc` depends on `core`, `std` depends on `core`+`alloc`
/// (+`libc` here, standing in for `std`'s real platform `sys` bindings) —
/// not one `std` package containing them as sub-modules. Wrapping them
/// under a shared outer package used to produce a doubled `std::std::`
/// crate root for the `std` facade crate itself, and gave every bare
/// `core`/`alloc` absolute path real Rust source uses (`core::option::
/// Option`, ...) a *different* qualified key than the one their actual
/// definitions were stored under.
///
/// `PackageProvider` for the `core`/`alloc`/`std`/`test`/`libc` package IDs
/// (`RustPackageProvider`'s counterpart to `fp_lang::provider::
/// FerroPhaseProvider`). `libc` is delegated straight to `fp-lang`'s
/// embedded copy — there's nothing Rust-specific about C ABI
/// declarations, no need to duplicate them.
///
/// Real std source is far more complex than anything `FerroFrontend` has been
/// validated against (heavy `unsafe`, `#[lang = "..."]` items, `cfg`-gated
/// platform code, const generics, specialization, inline asm, ...). Files
/// that fail to parse are skipped with a warning rather than failing the
/// whole package load, so whatever subset *does* parse is still usable.
pub struct RustStdProvider;

/// Minimal typed declarations for registry crates whose calls are supported
/// by target runtimes. This is metadata, not an emulation of the crates:
/// their bodies are never lowered and the intrinsic identity carries the
/// semantic contract to a backend.
pub struct RustExternalApiProvider;

const EXTERNAL_API_SOURCES: &[(&str, &str)] = &[
    (
        "serde_json",
        r#"
            pub struct Error;
            pub struct Value;
            #[intrinsic = "serde_json_from_str"]
            pub fn from_str<T>(input: &str) -> Result<T, Error> { unreachable!() }
            #[intrinsic = "serde_json_to_string"]
            pub fn to_string<T>(value: &T) -> Result<String, Error> { unreachable!() }
        "#,
    ),
    (
        "toml",
        r#"
            pub mod de { pub struct Error; }
            #[intrinsic = "toml_from_str"]
            pub fn from_str<T>(input: &str) -> Result<T, de::Error> { unreachable!() }
        "#,
    ),
    (
        "tokio",
        r#"
            pub mod net {
                pub struct TcpStream;
                impl TcpStream {
                    #[intrinsic = "tokio_tcp_connect"]
                    pub async fn connect<A>(address: A) -> Result<TcpStream, std::io::Error> { unreachable!() }
                    #[intrinsic = "tokio_tcp_write_all"]
                    pub async fn write_all(&mut self, bytes: &[u8]) -> Result<(), std::io::Error> { unreachable!() }
                }
            }
            pub mod time {
                #[intrinsic = "sleep"]
                pub async fn sleep(duration: std::time::Duration) { unreachable!() }
            }
        "#,
    ),
    (
        "winnow",
        r#"
            pub struct ContextError;
            pub type ModalResult<T> = Result<T, ContextError>;
            pub struct ParserValue<T>;

            #[op(func = "winnow_alt")]
            pub fn alt<T>(parsers: T) -> ParserValue<T> { unreachable!() }

            #[op(func = "winnow_take_while")]
            pub fn take_while<R, F>(range: R, predicate: F) -> ParserValue<String> { unreachable!() }

            pub trait Parser<I, O, E> {
                #[op(method = "winnow_parse_next")]
                fn parse_next(&mut self, input: &mut I) -> O { unreachable!() }
                #[op(method = "winnow_map")]
                fn map<F, R>(self, transform: F) -> ParserValue<R> { unreachable!() }
                #[op(method = "winnow_verify")]
                fn verify<F>(self, predicate: F) -> Self { unreachable!() }
            }
        "#,
    ),
];

impl RustExternalApiProvider {
    fn source_for(id: &PackageId) -> ProviderResult<&'static str> {
        EXTERNAL_API_SOURCES
            .iter()
            .find_map(|(name, source)| (*name == id.as_str()).then_some(*source))
            .ok_or_else(|| ProviderError::PackageNotFound(id.clone()))
    }

    fn package_source(id: &PackageId) -> ProviderResult<AstPackage> {
        let source = Self::source_for(id)?;
        let path = PathBuf::from(format!("<rust-external-api>/{}.rs", id.as_str()));
        let parsed = RustFrontend::new()
            .parse_file(source, &path)
            .map_err(|error| {
                ProviderError::other(format!("failed to parse {} API: {error}", id))
            })?;
        let mut items = Vec::new();
        flatten_items(
            &QualifiedPath::new(vec![id.as_str().to_owned()]),
            &parsed.ast.items,
            &mut items,
        );
        Ok(package_source_from_items(
            id,
            &items,
            PackageMetadata::default(),
        ))
    }
}

impl PackageProvider for RustExternalApiProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(EXTERNAL_API_SOURCES
            .iter()
            .map(|(name, _)| PackageId::new(*name))
            .collect())
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(Vec::new())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        Self::source_for(id)?;
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_owned(),
            version: None,
            manifest_path: VirtualPath::from_path(Path::new("<rust-external-api>/Cargo.toml")),
            root: VirtualPath::from_path(Path::new("<rust-external-api>")),
            metadata: PackageMetadata::default(),
            modules: Vec::new(),
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        Self::package_source(id)
    }

    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::normalizer::RustIntrinsicNormalizer::new())
    }
}

impl RustStdProvider {
    fn dependencies_of(crate_name: &str) -> Vec<&'static str> {
        match crate_name {
            CORE_PACKAGE_NAME => vec![],
            ALLOC_PACKAGE_NAME => vec![CORE_PACKAGE_NAME],
            STD_PACKAGE_NAME => vec![CORE_PACKAGE_NAME, ALLOC_PACKAGE_NAME, LIBC_PACKAGE_NAME],
            // Mirrors library/test/Cargo.toml from rust-src. `getopts` is a
            // build-only external dependency and is intentionally not
            // advertised because this provider cannot load registry crates.
            TEST_PACKAGE_NAME => vec![STD_PACKAGE_NAME, CORE_PACKAGE_NAME, LIBC_PACKAGE_NAME],
            _ => vec![],
        }
    }
}

impl PackageProvider for RustStdProvider {
    fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
        Ok(vec![
            PackageId::new(CORE_PACKAGE_NAME),
            PackageId::new(ALLOC_PACKAGE_NAME),
            PackageId::new(STD_PACKAGE_NAME),
            PackageId::new(TEST_PACKAGE_NAME),
            PackageId::new(LIBC_PACKAGE_NAME),
        ])
    }

    fn workspace_packages(&self) -> ProviderResult<Vec<PackageId>> {
        self.list_packages()
    }

    // Only ever blended in as a `CompositeProvider` *dependency* (std/libc),
    // never the primary `workspace` provider — `CompositeProvider::
    // intrinsic_normalizer` always defers to `self.workspace`'s own choice
    // instead, so this one is never actually consulted.
    fn intrinsic_normalizer(&self) -> Box<dyn fp_core::intrinsics::IntrinsicNormalizer> {
        Box::new(crate::normalizer::RustIntrinsicNormalizer::new())
    }

    fn load_package_metadata(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
        let root = match id.as_str() {
            CORE_PACKAGE_NAME | ALLOC_PACKAGE_NAME | STD_PACKAGE_NAME | TEST_PACKAGE_NAME => {
                crate::embedded_std::root_dir().join(id.as_str())
            }
            LIBC_PACKAGE_NAME => fp_lang::embedded_libc::root_dir(),
            _ => return Err(ProviderError::PackageNotFound(id.clone())),
        };
        let mut metadata = PackageMetadata::default();
        metadata.prelude = match id.as_str() {
            CORE_PACKAGE_NAME | STD_PACKAGE_NAME => Some(id.clone()),
            ALLOC_PACKAGE_NAME => Some(PackageId::new(CORE_PACKAGE_NAME)),
            TEST_PACKAGE_NAME => Some(PackageId::new(STD_PACKAGE_NAME)),
            LIBC_PACKAGE_NAME => None,
            _ => None,
        };
        for dependency in Self::dependencies_of(id.as_str()) {
            metadata.dependencies.push(DependencyDescriptor {
                package: dependency.to_string(),
                resolved_package_id: Some(PackageId::new(dependency)),
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: Default::default(),
            });
        }
        Ok(Arc::new(PackageDescriptor {
            id: id.clone(),
            name: id.as_str().to_string(),
            version: None,
            manifest_path: VirtualPath::from_path(&root.join("Cargo.toml")),
            root: VirtualPath::from_path(&root),
            metadata,
            modules: Vec::new(),
        }))
    }

    fn refresh(&self) -> ProviderResult<()> {
        Ok(())
    }

    fn load_package_source(&self, id: &PackageId) -> ProviderResult<AstPackage> {
        match id.as_str() {
            CORE_PACKAGE_NAME => load_real_std_subcrate(CORE_PACKAGE_NAME),
            ALLOC_PACKAGE_NAME => load_real_std_subcrate(ALLOC_PACKAGE_NAME),
            STD_PACKAGE_NAME => load_real_std_subcrate(STD_PACKAGE_NAME),
            TEST_PACKAGE_NAME => load_real_std_subcrate(TEST_PACKAGE_NAME),
            LIBC_PACKAGE_NAME => load_embedded_fp_package(
                LIBC_PACKAGE_NAME,
                fp_lang::embedded_libc::root_dir(),
                fp_lang::embedded_libc::module_paths(),
                fp_lang::embedded_libc::read,
            ),
            _ => Err(ProviderError::PackageNotFound(id.clone())),
        }
    }
}

/// Collapses `..`/`.` components in `path` without touching the
/// filesystem — needed before handing a `#[path = "../unix/sync"]`-style
/// redirect (real vendored std uses several of these) to `embedded_std::
/// read`, which rejects any raw `ParentDir` component outright.
fn normalize_path_components(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                out.pop();
            }
            std::path::Component::CurDir => {}
            other => out.push(other.as_os_str()),
        }
    }
    out
}

fn string_literal(expr: &fp_core::ast::Expr) -> Option<String> {
    match expr.kind() {
        fp_core::ast::ExprKind::Value(value) => match value.as_ref() {
            fp_core::ast::Value::String(s) => Some(s.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// Finds this `mod` item's own `#[path = "..."]` redirect, if any — a bare
/// `path = "..."` always applies; a `#[cfg_attr(cond, path = "...")]`
/// applies only when `cond` holds (real vendored std's `core::io::error`'s
/// `mod repr;` picks between two backing files this way, by pointer
/// width). Uses the exact same predicate evaluation
/// (`fp_core::cfg::cfg_meta_enabled`/`TargetEnv`) already used everywhere
/// else `#[cfg(..)]` is evaluated in this pipeline — no separate cfg
/// engine needed.
fn mod_path_attr(attrs: &[Attribute], env: &TargetEnv) -> Option<String> {
    for attr in attrs {
        match &attr.meta {
            AttrMeta::NameValue(nv) if nv.name.last().as_str() == "path" => {
                if let Some(value) = string_literal(&nv.value) {
                    return Some(value);
                }
            }
            AttrMeta::List(list) if list.name.last().as_str() == "cfg_attr" => {
                let [cond, rest @ ..] = list.items.as_slice() else {
                    continue;
                };
                if !fp_core::cfg::cfg_meta_enabled(cond, env) {
                    continue;
                }
                for item in rest {
                    if let AttrMeta::NameValue(nv) = item {
                        if nv.name.last().as_str() == "path" {
                            if let Some(value) = string_literal(&nv.value) {
                                return Some(value);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Resolves one child `mod name;`'s backing file and the directory *its
/// own* nested external `mod`s resolve against — mirrors real rustc: a
/// `#[path]` redirect's children resolve next to the redirected file
/// itself (or, for a directory-shaped redirect like `path = "../unix/
/// sync"`, inside that directory directly), never in a `name/`
/// subdirectory of the *declaring* module the way the ordinary
/// convention works. Returns `None` when nothing backs this `mod` at all
/// (an intentionally-empty inline `mod name {}`, or a `#[path]` pointing
/// outside the vendored corpus entirely — e.g. real vendored std's own
/// `stdarch`/`portable-simd`/`backtrace` sibling-crate redirects) — the
/// caller treats that identically to a real empty module, no error.
/// A `#[path]` value is always relative to the *declaring file's own*
/// directory (`file_dir`) — never to `children_base_dir` (the directory
/// this module's *un*-redirected, plain `mod name;` children resolve in,
/// which for a non-`mod.rs` file is a subdirectory *named after that
/// file*, one level down from `file_dir`). Conflating the two resolves a
/// redirect like `core::io::error`'s `#[path = "error/repr_bitpacked.rs"]`
/// (relative to `core/io/`, the directory containing `error.rs` itself)
/// as if it were relative to `core/io/error/` (the directory `error.rs`'s
/// *ordinary* children live in) instead — double-nesting the path and
/// always failing to find the file.
fn resolve_external_mod(
    read: &dyn Fn(&Path) -> Option<String>,
    file_dir: &Path,
    children_base_dir: &Path,
    mod_name: &str,
    attrs: &[Attribute],
    env: &TargetEnv,
) -> Option<(PathBuf, String)> {
    if let Some(redirect) = mod_path_attr(attrs, env) {
        let target = normalize_path_components(&file_dir.join(redirect));
        if let Some(source) = read(&target) {
            return Some((target, source));
        }
        // A directory-shaped redirect (real vendored std's own
        // `std::sys::pal::teeos`'s `#[path = "../unix/sync"]`) — its own
        // content is `target/mod.rs`, exactly like any other directory
        // module.
        let mod_rs = target.join("mod.rs");
        if let Some(source) = read(&mod_rs) {
            return Some((mod_rs, source));
        }
        return None;
    }
    let name_rs = children_base_dir.join(format!("{mod_name}.rs"));
    if let Some(source) = read(&name_rs) {
        return Some((name_rs, source));
    }
    let mod_rs = children_base_dir.join(mod_name).join("mod.rs");
    if let Some(source) = read(&mod_rs) {
        return Some((mod_rs, source));
    }
    None
}

/// The directory a file's *own* plain, unredirected `mod name;` children
/// resolve in: the same directory the file itself lives in when the file
/// is an "index" file (`mod.rs`, or a crate root `lib.rs`/`main.rs`), else
/// a subdirectory named after the file's own stem — real rustc's file
/// convention, applied uniformly regardless of *how* this file itself was
/// reached (ordinary convention or a `#[path]` redirect).
fn children_base_dir_for(file: &Path) -> PathBuf {
    let dir = file.parent().unwrap_or(Path::new(""));
    match file.file_name().and_then(|n| n.to_str()) {
        Some("mod.rs") | Some("lib.rs") | Some("main.rs") => dir.to_path_buf(),
        _ => match file.file_stem().and_then(|s| s.to_str()) {
            Some(stem) => dir.join(stem),
            None => dir.to_path_buf(),
        },
    }
}

/// Recursively resolves a module's item list — walking into every inline
/// `mod name { .. }` body directly, and resolving every external `mod
/// name;` (no body of its own in the parsed AST at all) to its backing
/// file via `resolve_external_mod`, parsing that file (through `parse`,
/// which owns whatever caching the caller wants) and recursing into it.
/// This is the one piece of logic real rustc-style module discovery
/// needs that a flat, path-derived filesystem scan structurally cannot
/// have — everything else (which package a file belongs to, its
/// `PackageItem` tagging) falls out of walking this exactly once.
#[allow(clippy::too_many_arguments)]
fn discover_items(
    read: &dyn Fn(&Path) -> Option<String>,
    parse: &mut dyn FnMut(&Path, &str) -> ProviderResult<Vec<Item>>,
    env: &TargetEnv,
    file_dir: &Path,
    children_base_dir: &Path,
    module_path: &QualifiedPath,
    items: &[Item],
    descriptor_ctx: Option<(&PackageId, ModuleLanguage, &mut Vec<ModuleDescriptor>)>,
    items_out: &mut Vec<PackageItem>,
) -> ProviderResult<()> {
    let mut descriptor_ctx = descriptor_ctx;
    for item in items {
        if !item_enabled_by_cfg(item, env) {
            continue;
        }
        // `cfg_select! { pred => { #[path = ".."] mod repr; } .. }` hides a
        // `mod` declaration (and its `#[path]` redirect) inside an
        // item-position macro invocation this walk otherwise never looks
        // inside — unlike `expand_item_macros_in_item`'s later, ast_to_hir-
        // side expansion of the same macro, this walk is the only place
        // that ever resolves a `mod name;`'s *file*, so a `mod` revealed
        // only after that later expansion runs would find no body at all
        // (see `core::io::io_slice`'s own `mod repr;`, hidden this way).
        // Expand right here, before the `ItemKind::Module` check below, so
        // any `mod` items it reveals still get their files resolved.
        if let ItemKind::Macro(item_macro) = item.kind() {
            if item_macro.declared_name.is_none() {
                if let Some(expanded) =
                    fp_lang::expand_item_macro_invocation(&item_macro.invocation, &HashMap::new())
                {
                    discover_items(
                        read,
                        parse,
                        env,
                        file_dir,
                        children_base_dir,
                        module_path,
                        &expanded,
                        descriptor_ctx
                            .as_mut()
                            .map(|(id, lang, descs)| (*id, lang.clone(), &mut **descs)),
                        items_out,
                    )?;
                    continue;
                }
            }
        }
        let ItemKind::Module(module) = item.kind() else {
            items_out.push(PackageItem {
                module_path: module_path.clone(),
                item: item.clone(),
            });
            continue;
        };
        let child_path = module_path.with_segment(module.name.as_str().to_owned());
        if !module.items.is_empty() {
            // An inline body isn't a new file at all — a `#[path]` on one
            // of *its own* nested mods still resolves against `file_dir`
            // unchanged, only its plain, unredirected children move one
            // level deeper by name.
            let child_base_dir = children_base_dir.join(module.name.as_str());
            discover_items(
                read,
                parse,
                env,
                file_dir,
                &child_base_dir,
                &child_path,
                &module.items,
                descriptor_ctx
                    .as_mut()
                    .map(|(id, lang, descs)| (*id, lang.clone(), &mut **descs)),
                items_out,
            )?;
            continue;
        }
        let Some((target_file, source)) = resolve_external_mod(
            read,
            file_dir,
            children_base_dir,
            module.name.as_str(),
            &module.attrs,
            env,
        ) else {
            continue;
        };
        let file_items = parse(&target_file, &source)?;
        if let Some((package_id, language, descriptors)) = descriptor_ctx.as_mut() {
            descriptors.push(ModuleDescriptor {
                id: ModuleId::new(&child_path.to_key()),
                package: (*package_id).clone(),
                language: (*language).clone(),
                module_path: child_path.segments.clone(),
                source: VirtualPath::from_path(&target_file),
                exports: Vec::new(),
                requires_features: Vec::new(),
            });
        }
        let child_file_dir = target_file.parent().unwrap_or(file_dir).to_path_buf();
        let child_base_dir = children_base_dir_for(&target_file);
        discover_items(
            read,
            parse,
            env,
            &child_file_dir,
            &child_base_dir,
            &child_path,
            &file_items,
            descriptor_ctx
                .as_mut()
                .map(|(id, lang, descs)| (*id, lang.clone(), &mut **descs)),
            items_out,
        )?;
    }
    Ok(())
}

fn flatten_items(path: &QualifiedPath, items: &[Item], output: &mut Vec<PackageItem>) {
    for item in items {
        if is_cfg_test(item_attrs(item)) {
            // Rust-test-only code (`#[cfg(test)] mod tests { .. }` or a
            // standalone `#[cfg(test)] fn`) was never meant to exist in a
            // transpiled target — skip it (and, for a module, everything
            // nested inside it) entirely rather than trying to transpile
            // test-harness code (`std::process::Command`, `std::fs`,
            // tempdirs, ...) that has no equivalent here.
            continue;
        }
        if let ItemKind::Module(module) = item.kind() {
            flatten_items(
                &path.with_segment(module.name.as_str().to_owned()),
                &module.items,
                output,
            );
        } else {
            output.push(PackageItem {
                module_path: path.clone(),
                item: item.clone(),
            });
        }
    }
}

/// Attributes for the item kinds that can plausibly carry `#[cfg(test)]`.
fn item_attrs(item: &Item) -> &[Attribute] {
    match item.kind() {
        ItemKind::Module(m) => &m.attrs,
        ItemKind::DefFunction(f) => &f.attrs,
        ItemKind::DefStruct(s) => &s.attrs,
        ItemKind::DefEnum(e) => &e.attrs,
        ItemKind::DefConst(c) => &c.attrs,
        ItemKind::Impl(i) => &i.attrs,
        _ => &[],
    }
}

/// True if `attrs` contains `#[cfg(test)]`.
fn is_cfg_test(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        let AttrMeta::List(list) = &attr.meta else {
            return false;
        };
        list.name.last().as_str() == "cfg"
            && list.items.iter().any(|item| match item {
                AttrMeta::Path(p) => p.last().as_str() == "test",
                _ => false,
            })
    })
}

/// Bump this whenever the Rust AST representation or parser semantics change.
/// Parsed standard-library source is semantic compiler input, so a cache entry
/// created by an older parser must never be reused by a newer compiler.
const STD_PARSE_CACHE_SCHEMA: u8 = 1;

/// Parse every embedded real-std `.rs` file, skipping (with a warning) any
/// that `RustFrontend` can't handle yet, rather than failing the whole load.
///
/// Uses a disk cache whose identity includes the exact embedded source and a
/// parser schema version. A pre-parsed AST bundled with the executable cannot
/// safely be reused because it has no source or parser provenance.
fn load_real_std_subcrate(crate_name: &'static str) -> ProviderResult<AstPackage> {
    let package_id = PackageId::new(crate_name);
    let root = crate::embedded_std::root_dir();
    let mut descriptors = Vec::new();
    let mut items = Vec::new();
    let mut parsed = 0usize;
    let mut cache_hits = 0usize;
    let mut disk_misses = 0usize;
    let mut decode_failures = 0usize;
    let mut encode_failures = 0usize;
    let mut write_failures = 0usize;
    let mut skipped = 0usize;
    let disk_cache = fp_core::cache::DiskCache::new(
        std::env::var_os("FP_CACHE_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("target/fp-cache")),
    );

    // Real rustc's sysroot vendors `core`/`alloc`/`std` as independent
    // crates (each its own crate root, `std`/`alloc` depending on `core`)
    // rather than one `std` package containing them as sub-modules — this
    // provider mirrors that instead of wrapping every sub-crate under an
    // outer `std::` prefix (which produced a doubled `std::std::` crate
    // root for the `std` facade crate itself, and made the bare
    // `core`/`alloc` absolute paths real Rust code actually uses resolve
    // to a different qualified key than where their definitions actually
    // live).
    //
    // Discovery itself now follows real rustc semantics: start at the
    // crate root file and recursively resolve every `mod name;` it
    // (transitively) declares to its backing file — a `#[path]` override
    // first, else the `name.rs`/`name/mod.rs` convention — rather than
    // independently guessing every embedded file's module path from its
    // own filesystem location. This is what lets a `#[path]`-redirected
    // module (`core::io::error`'s `mod repr;`, `core::lib.rs`'s `mod
    // legacy_int_modules;`) end up reachable under the name the rest of
    // the crate actually references it by, instead of only under its own
    // unrelated file path. It also makes the old `tests.rs`/`test.rs`
    // filename-convention skip unnecessary: a `#[cfg(test)] mod tests;`
    // is now simply never visited by `item_enabled_by_cfg`'s ordinary
    // cfg-filtering, the same way any other disabled item is skipped.
    let env = TargetEnv::host();
    let root_file = root.join(crate_name).join("lib.rs");
    let base_dir = root.join(crate_name);

    let mut parse = |path: &Path, source: &str| -> ProviderResult<Vec<Item>> {
        let relative = path
            .strip_prefix(&root)
            .ok()
            .and_then(|p| p.to_str())
            .unwrap_or_default()
            .to_string();
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        source.hash(&mut hasher);
        let source_hash = hasher.finish();
        let disk_key = format!(
            "rust/std-source/v{STD_PARSE_CACHE_SCHEMA}/{crate_name}/{relative}/{source_hash:016x}"
        );
        // A fresh frontend per file, not one shared across the whole
        // walk — each `.rs` file is its own independent translation
        // unit, and a parser is free to accumulate internal
        // recovery/nesting state across `parse_file` calls since nothing
        // about its public API promises isolation between them. A syntax
        // error in one file (there are, unfortunately, real ones among
        // these — see `FP_STD_PARSE_VERBOSE`) must never leave that
        // state dirty enough to spuriously fail the *next* file's
        // otherwise-valid parse.
        let frontend = RustFrontend::new();
        match disk_cache.get(&disk_key) {
            Ok(Some(bytes)) => match serde_json::from_slice::<Vec<Item>>(&bytes) {
                Ok(cached) => {
                    frontend.register_file_only(source, path);
                    cache_hits += 1;
                    return Ok(cached);
                }
                Err(_) => decode_failures += 1,
            },
            Ok(None) => disk_misses += 1,
            Err(_) => disk_misses += 1,
        }
        match frontend.parse_file(source, path) {
            Ok(result) => {
                register_threadlocal_serializer(result.serializer.clone());
                parsed += 1;
                let items = result.ast.items;
                match serde_json::to_vec(&items) {
                    Ok(bytes) => {
                        if disk_cache.put(&disk_key, &bytes).is_err() {
                            write_failures += 1;
                        }
                    }
                    Err(_) => encode_failures += 1,
                }
                Ok(items)
            }
            Err(err) => {
                skipped += 1;
                // Verbose per-file diagnostics are opt-in (`358 skipped`
                // on every run would otherwise be noisy) — but the
                // failure itself is silent by default beyond that
                // aggregate count, which makes a *specific* regression
                // (e.g. one file losing a syntax construct it used to
                // support) invisible until something downstream that
                // depended on it breaks. Set `FP_STD_PARSE_VERBOSE=1` to
                // see exactly which file and why. Real vendored std is
                // far too irregular to hard-fail the whole crate on one
                // unparseable file — skip it (empty item list) and keep
                // going, exactly as before.
                if std::env::var("FP_STD_PARSE_VERBOSE").is_ok() {
                    eprintln!("fp-rust: failed to parse {}: {err}", path.display());
                }
                Ok(Vec::new())
            }
        }
    };
    let read =
        |path: &Path| -> Option<String> { crate::embedded_std::read(path).map(|s| s.to_string()) };

    // Real rustc absolute paths this corpus actually uses (`core::option::
    // Option`, `std::result::Result`, ...) name each sub-crate as an
    // explicit leading segment — the crate root's *own* qualified path is
    // therefore `[crate_name]` (e.g. `["core"]`), not empty; only a plain
    // on-disk Cargo project's own crate root (a single, self-contained
    // package with no shared cross-crate namespace) collapses to `[]` (see
    // `rs_relative_to_module_path`, used there instead).
    let root_module_path = QualifiedPath::new(vec![crate_name.to_string()]);
    if let Some(source) = read(&root_file) {
        let root_items = parse(&root_file, &source)?;
        descriptors.push(ModuleDescriptor {
            id: ModuleId::new(&root_module_path.to_key()),
            package: package_id.clone(),
            language: ModuleLanguage::Rust,
            module_path: root_module_path.segments.clone(),
            source: VirtualPath::from_path(&root_file),
            exports: Vec::new(),
            requires_features: Vec::new(),
        });
        discover_items(
            &read,
            &mut parse,
            &env,
            &base_dir,
            &base_dir,
            &root_module_path,
            &root_items,
            Some((&package_id, ModuleLanguage::Rust, &mut descriptors)),
            &mut items,
        )?;
    }
    eprintln!(
        "fp-rust: real {crate_name} parse result — {parsed} file(s) parsed, {cache_hits} from cache, {disk_misses} disk misses, {decode_failures} decode failures, {encode_failures} encode failures, {write_failures} write failures, {skipped} skipped (parse errors)"
    );

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let mut metadata = PackageMetadata::default();
    metadata.prelude = match crate_name {
        CORE_PACKAGE_NAME | STD_PACKAGE_NAME => Some(PackageId::new(crate_name)),
        ALLOC_PACKAGE_NAME => Some(PackageId::new(CORE_PACKAGE_NAME)),
        TEST_PACKAGE_NAME => Some(PackageId::new(STD_PACKAGE_NAME)),
        LIBC_PACKAGE_NAME => None,
        _ => None,
    };
    for dependency in RustStdProvider::dependencies_of(crate_name) {
        metadata.dependencies.push(DependencyDescriptor {
            package: dependency.to_string(),
            resolved_package_id: Some(PackageId::new(dependency)),
            constraint: None,
            kind: DependencyKind::Normal,
            features: Vec::new(),
            optional: false,
            target: Default::default(),
        });
    }
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: crate_name.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join(crate_name).join("Cargo.toml")),
        root: VirtualPath::from_path(&root.join(crate_name)),
        metadata,
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for descriptor in descriptors {
        graph.insert_module(descriptor);
    }
    let mut krate = AstPackage::new(package_id, crate_name, graph);
    krate.items = items;
    Ok(krate)
}

/// Same shape as `fp_lang::provider`'s private `load_embedded_package`, kept
/// as its own copy here since that one isn't exported — used for delegating
/// to `fp-lang`'s embedded `libc` `.fp` source.
fn load_embedded_fp_package(
    package_name: &str,
    root: PathBuf,
    module_paths: &'static [&'static str],
    read: fn(&std::path::Path) -> Option<&'static str>,
) -> ProviderResult<AstPackage> {
    let frontend = FerroFrontend::new();
    let package_id = PackageId::new(package_name);
    let mut descriptors = Vec::new();
    let mut items = Vec::new();

    for relative_str in module_paths {
        let path = root.join(relative_str);
        let Some(source) = read(&path) else {
            continue;
        };
        let module_path = fp_relative_to_module_segments(package_name, relative_str);
        if module_path.is_empty() {
            continue;
        }
        let result = frontend
            .parse_file(source, &path)
            .map_err(|e| ProviderError::other(format!("failed to parse {relative_str}: {e}")))?;
        register_threadlocal_serializer(result.serializer.clone());
        flatten_items(
            &QualifiedPath::new(module_path.clone()),
            &result.ast.items,
            &mut items,
        );
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

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: package_name.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join("fp.toml")),
        root: VirtualPath::from_path(&root),
        metadata: Default::default(),
        modules: module_ids,
    };
    let mut graph = PackageGraph::new(vec![package]);
    for descriptor in descriptors {
        graph.insert_module(descriptor);
    }
    let mut krate = AstPackage::new(PackageId::new(package_name), package_name, graph);
    krate.items = items;
    Ok(krate)
}

fn fp_relative_to_module_segments(package_name: &str, relative: &str) -> Vec<String> {
    let mut segments: Vec<String> = vec![package_name.to_string()];
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

#[cfg(test)]
mod provider_tests {
    use super::*;
    use fp_core::intrinsics::extract_op_attr;

    fn op_method_tags(relative_path: &str) -> Vec<(String, String)> {
        let path = crate::embedded_std::root_dir().join(relative_path);
        let source = crate::embedded_std::read(&path).expect("vendored std source");
        let parsed = RustFrontend::new()
            .parse_file(source, &path)
            .expect("parse vendored std source");

        parsed
            .ast
            .items
            .iter()
            .filter_map(|item| {
                let ItemKind::Impl(impl_block) = item.kind() else {
                    return None;
                };
                let class = extract_op_attr(&impl_block.attrs, "class")?;
                Some(impl_block.items.iter().filter_map(move |member| {
                    let ItemKind::DefFunction(function) = member.kind() else {
                        return None;
                    };
                    extract_op_attr(&function.attrs, "method").map(|method| (class.clone(), method))
                }))
            })
            .flatten()
            .collect()
    }

    fn repository_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(Path::parent)
            .and_then(Path::parent)
            .expect("fp-rust is nested below the repository root")
            .to_path_buf()
    }

    #[test]
    fn rust_sysroot_packages_report_direct_dependencies() {
        let provider = RustStdProvider;
        let expected = [
            (CORE_PACKAGE_NAME, &[][..]),
            (ALLOC_PACKAGE_NAME, &[CORE_PACKAGE_NAME][..]),
            (
                STD_PACKAGE_NAME,
                &[CORE_PACKAGE_NAME, ALLOC_PACKAGE_NAME, LIBC_PACKAGE_NAME][..],
            ),
            (
                TEST_PACKAGE_NAME,
                &[STD_PACKAGE_NAME, CORE_PACKAGE_NAME, LIBC_PACKAGE_NAME][..],
            ),
        ];

        let packages = provider.list_packages().unwrap();
        for (name, dependencies) in expected {
            assert!(packages.iter().any(|id| id.as_str() == name));
            let metadata = provider
                .load_package_metadata(&PackageId::new(name))
                .unwrap();
            let actual: Vec<_> = metadata
                .metadata
                .dependencies
                .iter()
                .map(|dependency| dependency.resolved_package_id.as_ref().unwrap().as_str())
                .collect();
            assert_eq!(actual, dependencies);
        }
    }

    #[test]
    fn vendored_std_exposes_requested_portable_operation_metadata() {
        let expected = [
            ("core/str/mod.rs", "str", "char_indices"),
            ("core/str/mod.rs", "str", "split_at"),
            ("core/str/mod.rs", "str", "strip_prefix"),
            ("core/slice/mod.rs", "slice", "split_at"),
            ("core/slice/mod.rs", "slice", "strip_prefix"),
            ("core/bool.rs", "bool", "then_some"),
            ("core/char/methods.rs", "char", "is_ascii_hexdigit"),
            ("core/range.rs", "RangeInclusive", "contains"),
        ];

        for (path, class, method) in expected {
            assert!(
                op_method_tags(path)
                    .iter()
                    .any(|(actual_class, actual_method)| {
                        actual_class == class && actual_method == method
                    }),
                "missing #[op(class = {class:?})] #[op(method = {method:?})] in {path}",
            );
        }
    }

    #[test]
    fn rust_sysroot_source_starts_at_external_crate_root() {
        let provider = RustStdProvider;
        for crate_name in [CORE_PACKAGE_NAME, ALLOC_PACKAGE_NAME, STD_PACKAGE_NAME] {
            let source = provider
                .load_package_source(&PackageId::new(crate_name))
                .unwrap();
            assert!(
                source
                    .module_paths
                    .iter()
                    .all(|path| path.segments.first().map(String::as_str) == Some(crate_name)),
                "{crate_name} module paths must start at the external crate root"
            );
            assert!(
                source
                    .module_paths
                    .iter()
                    .any(|path| path.segments == [crate_name.to_string()]),
                "{crate_name} must publish its crate-root module"
            );
            assert!(
                source.items.iter().all(|item| {
                    item.module_path.segments.first().map(String::as_str) == Some(crate_name)
                }),
                "{crate_name} items must start at the external crate root"
            );
            let package = source
                .graph
                .packages()
                .find(|package| package.id.as_str() == crate_name)
                .expect("sysroot source graph package");
            let dependencies: Vec<_> = package
                .metadata
                .dependencies
                .iter()
                .filter_map(|dependency| dependency.resolved_package_id.as_ref())
                .map(PackageId::as_str)
                .collect();
            assert_eq!(dependencies, RustStdProvider::dependencies_of(crate_name));
        }
    }

    #[test]
    fn workspace_path_dependency_uses_cargo_package_id_and_root() {
        let root = repository_root();
        let provider = RustPackageProvider::new(root.clone());

        let package_ids = provider.list_packages().unwrap();
        assert!(package_ids.iter().any(|id| id.as_str() == "skln-git"));
        assert!(package_ids.iter().any(|id| id.as_str() == "skln-core"));

        let git = provider
            .load_package_metadata(&PackageId::new("skln-git"))
            .unwrap();
        let dependency = git
            .metadata
            .dependencies
            .iter()
            .find(|dependency| dependency.package == "skln-core")
            .expect("skln-git should expose its path dependency");
        assert_eq!(
            dependency.resolved_package_id,
            Some(PackageId::new("skln-core"))
        );
        assert_eq!(dependency.kind, DependencyKind::Normal);

        let core = provider
            .load_package_metadata(&PackageId::new("skln-core"))
            .unwrap();
        assert_eq!(
            core.root.to_path_buf(),
            root.join("crates/skln-core").canonicalize().unwrap()
        );
        assert_eq!(
            core.manifest_path.to_path_buf(),
            root.join("crates/skln-core/Cargo.toml")
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn member_directory_discovers_sibling_workspace_packages() {
        let workspace = repository_root();
        let member = workspace.join("crates/skln-git");
        let provider = RustPackageProvider::new(member);
        let package_ids = provider.list_packages().unwrap();

        assert!(package_ids.iter().any(|id| id.as_str() == "skln-git"));
        assert!(package_ids.iter().any(|id| id.as_str() == "skln-core"));
    }

    #[test]
    fn workspace_path_dependency_uses_declared_package_name() {
        let root = std::env::temp_dir().join(format!(
            "fp-rust-provider-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let consumer = root.join("consumer");
        let renamed_dir = root.join("renamed-dir");
        std::fs::create_dir_all(consumer.join("src")).unwrap();
        std::fs::create_dir_all(renamed_dir.join("src")).unwrap();
        std::fs::write(
            root.join("Cargo.toml"),
            "[workspace]\nmembers = [\"consumer\", \"renamed-dir\"]\n",
        )
        .unwrap();
        std::fs::write(
            consumer.join("Cargo.toml"),
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n[dependencies]\nrenamed = { package = \"actual-core\", path = \"../renamed-dir\" }\n",
        )
        .unwrap();
        std::fs::write(
            renamed_dir.join("Cargo.toml"),
            "[package]\nname = \"actual-core\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let provider = RustPackageProvider::new(root.clone());
        let package_ids = provider.list_packages().unwrap();
        assert!(package_ids.iter().any(|id| id.as_str() == "actual-core"));
        assert!(!package_ids.iter().any(|id| id.as_str() == "renamed-dir"));

        let consumer_metadata = provider
            .load_package_metadata(&PackageId::new("consumer"))
            .unwrap();
        let dependency = consumer_metadata
            .metadata
            .dependencies
            .iter()
            .find(|dependency| dependency.package == "renamed")
            .expect("the dependency alias should be retained");
        assert_eq!(
            dependency.resolved_package_id,
            Some(PackageId::new("actual-core"))
        );

        let actual_metadata = provider
            .load_package_metadata(&PackageId::new("actual-core"))
            .unwrap();
        assert_eq!(
            actual_metadata.root.to_path_buf().canonicalize().unwrap(),
            renamed_dir.canonicalize().unwrap()
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn package_source_cache_survives_a_fresh_provider() {
        let root = std::env::temp_dir().join(format!(
            "fp-rust-disk-cache-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let source = root.join("sample.rs");
        std::fs::write(&source, "pub struct CachedType;").unwrap();

        let package_id = PackageId::new("sample");
        let first = RustPackageProvider::new(source.clone())
            .load_package_source(&package_id)
            .unwrap();
        assert_eq!(first.items.len(), 1);
        let serializable = first
            .items
            .iter()
            .map(|item| (item.module_path.segments.clone(), item.item.clone()))
            .collect::<Vec<_>>();
        serde_json::to_vec(&serializable).unwrap();
        assert!(root.join("target/fp-cache").is_dir());

        let second = RustPackageProvider::new(source.clone())
            .load_package_source(&package_id)
            .unwrap();
        assert_eq!(second.items.len(), 1);
        assert_eq!(second.items[0].module_path, first.items[0].module_path);

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn package_source_cache_invalidates_when_a_child_module_changes() {
        let root = std::env::temp_dir().join(format!(
            "fp-rust-module-cache-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let source_dir = root.join("src");
        std::fs::create_dir_all(&source_dir).unwrap();
        std::fs::write(
            root.join("Cargo.toml"),
            "[workspace]\nmembers = [\".\"]\n\n[package]\nname = \"sample\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(source_dir.join("lib.rs"), "mod child;").unwrap();
        let child = source_dir.join("child.rs");
        std::fs::write(&child, "pub struct Before;").unwrap();

        let package_id = PackageId::new("sample");
        let first = RustPackageProvider::new(root.clone())
            .load_package_source(&package_id)
            .unwrap();
        assert!(first.items.iter().any(|item| {
            matches!(item.item.as_struct(), Some(definition) if definition.name.as_str() == "Before")
        }));

        std::fs::write(&child, "pub struct After;").unwrap();
        let second = RustPackageProvider::new(root.clone())
            .load_package_source(&package_id)
            .unwrap();
        assert!(second.items.iter().any(|item| {
            matches!(item.item.as_struct(), Some(definition) if definition.name.as_str() == "After")
        }));
        assert!(!second.items.iter().any(|item| {
            matches!(item.item.as_struct(), Some(definition) if definition.name.as_str() == "Before")
        }));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn external_api_provider_declares_supported_registry_crates() {
        let provider = RustExternalApiProvider;
        for package in ["serde_json", "toml", "tokio"] {
            let source = provider
                .load_package_source(&PackageId::new(package))
                .expect("supported registry API package");
            assert!(
                !source.items.is_empty(),
                "{package} must expose typed portability declarations"
            );
        }
    }
}

#[cfg(test)]
mod real_std_parse_coverage {
    use super::*;

    /// Canary, not a strict gate: catches a wholesale regression (e.g. a
    /// frontend change that suddenly can't parse anything) without blocking
    /// gradual improvement to `RustFrontend`'s grammar coverage as it grows
    /// to handle more of real std's `unsafe`/`cfg`/attribute-heavy surface.
    #[test]
    fn measures_real_std_parse_coverage() {
        let frontend = RustFrontend::new();
        let root = crate::embedded_std::root_dir();
        let mut parsed = 0usize;
        let mut skipped = 0usize;
        for relative_str in crate::embedded_std::module_paths() {
            let path = root.join(relative_str);
            let Some(source) = crate::embedded_std::read(&path) else {
                continue;
            };
            match frontend.parse_file(source, &path) {
                Ok(_) => parsed += 1,
                Err(_) => skipped += 1,
            }
        }
        let total = parsed + skipped;
        let pct = parsed as f64 / total as f64 * 100.0;
        eprintln!("fp-rust: real std parse coverage — {parsed}/{total} files ({pct:.1}%)");
        assert!(
            parsed > total / 2,
            "real std parse coverage dropped below 50% ({parsed}/{total} files)"
        );
    }
}
