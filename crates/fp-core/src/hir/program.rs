use super::*;

/// The whole compiled result — every package involved, keyed by
/// `PackageId`. `HirGenerator` owns one of these and works package-by-package
/// against it (see `docs/Resolution.md`); resolution across an
/// already-compiled dependency package is a lookup into this same
/// structure, not a separate clone-and-merge pass.
///
/// Packages are `Rc`, not owned — building a `HirProgram` (e.g. a
/// `AstProgram` snapshotting its already-compiled dependency
/// packages, each already an `Rc<HirPackage>`, for a consumer like
/// `MirLowering` to dispatch cross-package `DefId` lookups against) is
/// then just a handful of `Rc` clones, never a deep clone of every
/// dependency's own items/def_map/def_paths.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct HirProgram {
    pub packages: HashMap<PackageId, std::rc::Rc<HirPackage>>,
    /// Direct name -> `DefId` lookup across every package, for well-known
    /// cross-package lookups by bare name (e.g. `fp_typing`'s well-known
    /// standard-library collection types) — maintained incrementally by
    /// `add_package`, never rescanned per query. First package added to
    /// declare a given name wins (add the current package last, after its
    /// dependencies, for "current package's own name shadows a
    /// dependency's" priority).
    struct_defs_by_name: HashMap<String, DefId>,
}

impl HirProgram {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
            struct_defs_by_name: HashMap::new(),
        }
    }

    pub fn package(&self, id: PackageId) -> Option<&HirPackage> {
        self.packages.get(&id).map(|package| package.as_ref())
    }

    /// Inserts `package`, merging its own `struct_defs_by_name` into this
    /// `HirProgram`'s direct lookup index in the same step — the
    /// incremental counterpart to re-deriving that index by scanning every
    /// package's items on every query.
    pub fn add_package(&mut self, package: std::rc::Rc<HirPackage>) {
        for (name, def_id) in &package.struct_defs_by_name {
            self.struct_defs_by_name.entry(name.clone()).or_insert(*def_id);
        }
        self.packages.insert(package.id, package);
    }

    /// O(1) direct lookup — no package iteration — for a struct declared
    /// under `name` in any package this `HirProgram` knows about.
    pub fn struct_def_id(&self, name: &str) -> Option<DefId> {
        self.struct_defs_by_name.get(name).copied()
    }

    /// Every item across every package this `HirProgram` knows about — for
    /// callers that genuinely need the full set (e.g. a one-time reverse
    /// index build), not a single `DefId` lookup.
    pub fn all_items(&self) -> impl Iterator<Item = &Item> {
        self.packages.values().flat_map(|package| package.items.iter())
    }

    /// A definition's fully-qualified path, wherever its owning package
    /// lives — routes to that package's own `def_paths` via the `DefId`'s
    /// own `package_id`, so a caller never has to know or track which
    /// package a `DefId` came from before asking this question.
    pub fn def_path(&self, def_id: DefId) -> Option<&DefPath> {
        self.package(def_id.package_id)?.def_paths.get(&def_id)
    }

    /// A transparent type alias's expansion target — see
    /// `HirPackage::type_alias_targets`'s doc comment for why this table
    /// exists at all.
    pub fn type_alias_target(&self, def_id: DefId) -> Option<&TypeExpr> {
        self.package(def_id.package_id)?
            .type_alias_targets
            .get(&def_id)
    }

    pub fn item(&self, def_id: DefId) -> Option<&Item> {
        self.package(def_id.package_id)?.def_map.get(&def_id)
    }

    pub fn op_def(&self, def_id: DefId) -> Option<&crate::intrinsics::PortableOp> {
        self.package(def_id.package_id)?.op_defs.get(&def_id)
    }

    pub fn intrinsic_def(&self, def_id: DefId) -> Option<&CallKind> {
        self.package(def_id.package_id)?.intrinsic_defs.get(&def_id)
    }

    pub fn is_placeholder_def(&self, def_id: DefId) -> bool {
        self.package(def_id.package_id)
            .is_some_and(|package| package.placeholder_defs.contains(&def_id))
    }

    /// Every `impl` item (from any package) whose self-type resolves to
    /// `did` — an impl for a type can live in a different package than the
    /// type itself, so this unions every package's own
    /// `HirPackage::impls_by_self_did` rather than only looking in `did`'s
    /// own package. Each per-package lookup is still O(1); only the number
    /// of packages that actually declare a matching impl costs anything.
    pub fn impls_for_adt(&self, did: DefId) -> impl Iterator<Item = &Item> {
        self.packages.values().flat_map(move |package| {
            package
                .impls_by_self_did
                .get(&did)
                .into_iter()
                .flatten()
                .filter_map(move |impl_def_id| package.def_map.get(impl_def_id))
        })
    }

    /// Every `impl` item across every package — the fallback for a
    /// method-call/UFCS-call whose receiver type isn't a resolved ADT
    /// (so there's no `did` to key `impls_for_adt` by).
    pub fn all_impls(&self) -> impl Iterator<Item = &Item> {
        self.all_items()
            .filter(|item| matches!(item.kind, ItemKind::Impl(_)))
    }

    /// Resolves `path` (in namespace `ns`) starting from `from_module` in
    /// package `from`, falling through to another already-compiled
    /// package's own module tree when `path`'s root names a different
    /// package (mirrors how a real cross-crate path resolves — the target
    /// package's own tree, not the caller's).
    /// Resolves `name` (in namespace `ns`) as seen from `from_module` in
    /// package `from` — takes a plain module path, not a `ModuleId`:
    /// `ModuleId` is `ModuleTree`'s own internal node handle, never meant
    /// to leak past this API to a caller (fp-typing, hir_to_ast, ...) that
    /// has no reason to know the tree exists at all, only that it can ask
    /// the program a question about a path.
    pub fn resolve(
        &self,
        from: PackageId,
        from_module: &crate::ast::path::QualifiedPath,
        ns: resolve::Namespace,
        name: &str,
    ) -> Option<&Res> {
        let package = self.package(from)?;
        let module = package.module_tree.module_id(from_module)?;
        package.module_tree.lookup_res(module, ns, name)
    }
}
