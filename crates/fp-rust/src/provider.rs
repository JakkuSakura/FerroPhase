use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::{AttrMeta, Attribute, Item, ItemKind, register_threadlocal_serializer};
use fp_core::cfg::{TargetEnv, item_enabled_by_cfg};
use fp_core::frontend::LanguageFrontend;
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::module::{ModuleDescriptor, ModuleId, ModuleLanguage};
use fp_core::ast::package::graph::PackageGraph;
use fp_core::ast::package::provider::{PackageProvider, ProviderError, ProviderResult};
use fp_core::ast::package::{
    DependencyDescriptor, DependencyKind, PackageDescriptor, PackageId, PackageItem,
    PackageMetadata, AstPackage,
};
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

pub struct RustPackageProvider {
    members: Vec<(String, MemberRoot)>,
    cache: RwLock<HashMap<String, Vec<PackageItem>>>,
}

impl RustPackageProvider {
    pub fn new(root: PathBuf) -> Self {
        // A standalone file (no enclosing Cargo project) is its own
        // one-member package, named after itself — the degenerate case of
        // "package", not a separate code path.
        if root.is_file() {
            let name = root
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main")
                .to_string();
            return Self {
                members: vec![(name, MemberRoot::File(root))],
                cache: RwLock::new(HashMap::new()),
            };
        }
        // `list_cargo_members`, not `list_members`: this provider is
        // specifically for real Rust/Cargo projects, so `Cargo.toml` is
        // authoritative here even if a stale/unrelated `Magnet.toml` also
        // exists at the same root — see that function's doc comment.
        let members = project::find_manifest(&root)
            .map(|manifest_root| project::list_cargo_members(&manifest_root))
            .unwrap_or_default()
            .into_iter()
            .map(|(name, dir)| (name, MemberRoot::Dir(dir)))
            .collect();
        Self {
            members,
            cache: RwLock::new(HashMap::new()),
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
        metadata.dependencies.push(DependencyDescriptor {
            package: "std".to_string(),
            resolved_package_id: Some(PackageId::new("std")),
            constraint: None,
            kind: DependencyKind::Normal,
            features: Vec::new(),
            optional: false,
            target: Default::default(),
        });
        if let MemberRoot::Dir(dir) = member_root {
            metadata
                .dependencies
                .extend(workspace_path_dependencies(dir, &self.members));
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
        Ok(package_source_from_items(id, &items))
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
        if let Ok(c) = self.cache.read() {
            if let Some(items) = c.get(id.as_str()) {
                return Ok(items.clone());
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
            c.insert(id.as_str().to_string(), items.clone());
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
    let Some(dependencies) = manifest.get("dependencies").and_then(|v| v.as_table()) else {
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

    dependencies
        .iter()
        .filter_map(|(dep_name, spec)| {
            let relative_path = spec.get("path")?.as_str()?;
            let absolute_path = package_dir.join(relative_path);
            let canonical_path =
                std::fs::canonicalize(&absolute_path).unwrap_or(absolute_path);
            let (member_name, _) = canonical_members
                .iter()
                .find(|(_, member_path)| *member_path == canonical_path)?;
            Some(DependencyDescriptor {
                package: dep_name.clone(),
                resolved_package_id: Some(PackageId::new(member_name)),
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: Default::default(),
            })
        })
        .collect()
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

fn package_source_from_items(id: &PackageId, items: &[PackageItem]) -> AstPackage {
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
        metadata: Default::default(),
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
const LIBC_PACKAGE_NAME: &str = "libc";

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
/// `PackageProvider` for the `core`/`alloc`/`std`/`libc` package IDs
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

impl RustStdProvider {
    fn dependencies_of(crate_name: &str) -> Vec<&'static str> {
        match crate_name {
            CORE_PACKAGE_NAME => vec![],
            ALLOC_PACKAGE_NAME => vec![CORE_PACKAGE_NAME],
            STD_PACKAGE_NAME => vec![CORE_PACKAGE_NAME, ALLOC_PACKAGE_NAME, LIBC_PACKAGE_NAME],
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
            CORE_PACKAGE_NAME | ALLOC_PACKAGE_NAME | STD_PACKAGE_NAME => {
                crate::embedded_std::root_dir().join(id.as_str())
            }
            LIBC_PACKAGE_NAME => fp_lang::embedded_libc::root_dir(),
            _ => return Err(ProviderError::PackageNotFound(id.clone())),
        };
        let mut metadata = PackageMetadata::default();
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

/// Pre-parsed cache of real std source, bundled into the binary by
/// `build.rs` from `std_cache.bin` (empty if none has been generated yet
/// — see `package_std_parse_cache` in `build.rs`). Keyed by the same
/// `relative_str` `load_real_std_package` already iterates by
/// (`crate::embedded_std::module_paths()`), value is that file's raw
/// parsed `Vec<Item>` (pre-`flatten_items`) — a direct substitute for
/// `frontend.parse_file(...).ast.items`, letting the winnow tokenizer/
/// parser be skipped entirely for every file the cache already covers.
static STD_CACHE_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/std_cache.bin"));

/// Deserialized fresh on each call rather than memoized behind a `static`
/// — `Item` embeds `ExprClosured`'s `SharedScopedContext` (an `Rc`-based
/// type), so `HashMap<String, Vec<Item>>` isn't `Sync` and can't live in a
/// `static`/`OnceLock`. `load_real_std_package` is only ever called once
/// per package load in practice, so the extra deserialize is negligible
/// next to the winnow parsing it replaces.
fn cached_std_items() -> HashMap<String, Vec<Item>> {
    if STD_CACHE_BYTES.is_empty() {
        return HashMap::new();
    }
    match bincode::deserialize(STD_CACHE_BYTES) {
        Ok(cache) => cache,
        Err(err) => {
            eprintln!("fp-rust: failed to deserialize bundled std parse cache: {err}");
            HashMap::new()
        }
    }
}

/// Parse every embedded real-std `.rs` file, skipping (with a warning) any
/// that `RustFrontend` can't handle yet, rather than failing the whole load.
///
/// Consults `cached_std_items()` first for each file — a cache hit still
/// registers the file in the source map (`RustFrontend::register_file_only`)
/// so its cached spans' `FileId`s stay in sync with this run's, but skips
/// the actual tokenize/parse. Setting `FP_STD_CACHE_DUMP=<path>` bypasses
/// the cache entirely (always parses fresh from source, so the dump is
/// never built from a possibly-stale cache) and writes the resulting
/// per-file item map to that path afterward, for `build.rs` to bundle into
/// the next build.
fn load_real_std_subcrate(crate_name: &'static str) -> ProviderResult<AstPackage> {
    let package_id = PackageId::new(crate_name);
    let root = crate::embedded_std::root_dir();
    let mut descriptors = Vec::new();
    let mut items = Vec::new();
    let mut parsed = 0usize;
    let mut cache_hits = 0usize;
    let mut skipped = 0usize;

    let dump_path = std::env::var("FP_STD_CACHE_DUMP").ok();
    let cache = if dump_path.is_some() {
        HashMap::new()
    } else {
        cached_std_items()
    };
    let mut fresh_cache: HashMap<String, Vec<Item>> = HashMap::new();

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
        if let Some(cached) = cache.get(&relative) {
            frontend.register_file_only(source, path);
            cache_hits += 1;
            return Ok(cached.clone());
        }
        match frontend.parse_file(source, path) {
            Ok(result) => {
                register_threadlocal_serializer(result.serializer.clone());
                parsed += 1;
                if dump_path.is_some() {
                    fresh_cache.insert(relative, result.ast.items.clone());
                }
                Ok(result.ast.items)
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
    let read = |path: &Path| -> Option<String> {
        crate::embedded_std::read(path).map(|s| s.to_string())
    };

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
        "fp-rust: real {crate_name} parse result — {parsed} file(s) parsed, {cache_hits} from cache, {skipped} skipped (parse errors)"
    );

    if let Some(dump_path) = dump_path {
        match bincode::serialize(&fresh_cache) {
            Ok(bytes) => match std::fs::write(&dump_path, &bytes) {
                Ok(()) => eprintln!(
                    "fp-rust: wrote {crate_name} parse cache ({} file(s)) to {dump_path}",
                    fresh_cache.len()
                ),
                Err(err) => eprintln!("fp-rust: failed to write {crate_name} cache to {dump_path}: {err}"),
            },
            Err(err) => eprintln!("fp-rust: failed to serialize {crate_name} cache: {err}"),
        }
    }

    let module_ids = descriptors.iter().map(|desc| desc.id.clone()).collect();
    let package = PackageDescriptor {
        id: package_id.clone(),
        name: crate_name.to_string(),
        version: None,
        manifest_path: VirtualPath::from_path(&root.join(crate_name).join("Cargo.toml")),
        root: VirtualPath::from_path(&root.join(crate_name)),
        metadata: Default::default(),
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
