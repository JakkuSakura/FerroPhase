use super::*;
use std::cell::RefCell;
use std::rc::Rc;

/// One compiled package's HIR content — items, definitions, and (as of the
/// `ModuleTree` migration) its own module/name-resolution tree. Several of
/// these live inside a `HirProgram`, which owns the whole multi-package
/// compiled result (see `HirProgram`'s own doc comment).
#[derive(Debug, Clone, PartialEq)]
pub struct HirPackage {
    pub id: PackageId,
    pub module_tree: resolve::ModuleTree,
    pub items: Vec<Item>,
    pub def_map: HashMap<DefId, Item>,
    pub next_hir_id: u32,
    /// Fully-qualified path for a definition's `DefId`, recorded once at
    /// first registration (module segments + the definition's own bare
    /// name as the last segment). Analogous to rustc's `DefPathTable`:
    /// item `name` fields are always bare, local identifiers, and a
    /// qualified path — when one is needed for lookup/diagnostics — is
    /// computed by consulting this table rather than stored redundantly
    /// on the item itself. A missing entry means the definition has no
    /// meaningful module qualification (e.g. impl methods, addressed by
    /// (type, method) pair instead, or synthetic items).
    pub def_paths: HashMap<DefId, DefPath>,
    /// `DefId`s of items whose HIR form is a structural stand-in, not a
    /// real lowering of the original source construct — currently, trait
    /// declarations (HIR has no first-class trait item; `ast_to_hir`
    /// fabricates a placeholder `Const` just so the definition has some
    /// HIR shape to type-check as a value/type reference). Consumers that
    /// reconstruct AST from HIR (`HirToAstLifter`) must skip these rather
    /// than lift the placeholder itself, so backends that work from the
    /// original source item (e.g. fp-kotlin modeling a trait as a real
    /// Kotlin interface) see it unmodified instead of overwritten.
    pub placeholder_defs: HashSet<DefId>,
    /// A definition's portable op, when its source declaration was tagged
    /// `#[op(func = "...")]` (free function) or `#[op(method = "...")]`
    /// (inside a `class`-tagged `impl` block) — populated once, by
    /// `ast_to_hir` reading the item's own attrs at the point it assigns
    /// that item's real `DefId`. Consulted post-typecheck directly by
    /// `HirToAstLifter`, keyed by the *resolved*
    /// identity of a call's callee (`hir::Res::Def`) or a method call's
    /// resolution (`TypeckResults::method_resolutions`) — never by
    /// re-deriving and string/path-comparing a call site's own syntax,
    /// which is both redundant (the compiler already resolved this) and
    /// where the earlier, retired `compile_mode_std_path`/path-based
    /// registry design went wrong.
    pub op_defs: HashMap<DefId, crate::intrinsics::PortableOp>,
    /// A free function's compiler intrinsic, when its source declaration was
    /// tagged `#[intrinsic = "..."]` — populated the same way and at the
    /// same site as `op_defs`, and consulted the same way: keyed by the
    /// *resolved* identity of a call's callee (`hir::Res::Def`), never by
    /// re-deriving and name/path-comparing a call site's own syntax. A
    /// bare-name call with no real declaration to resolve to (nothing in
    /// scope, not even this intrinsic's own) simply never reaches this map —
    /// it stays an ordinary (and, for a genuinely undefined name, erroring)
    /// call, same as any other unresolved identifier.
    pub intrinsic_defs: HashMap<DefId, CallKind>,
    /// A transparent type alias's expansion (`type Foo = Bar;`, where
    /// `Bar` isn't itself a fresh struct/enum/structural literal this
    /// alias declaration introduces) — HIR has no first-class "type
    /// alias" item (mirroring the `placeholder_defs` doc comment above:
    /// there's no dedicated item shape for this either), so the alias's
    /// own `DefId` still resolves (via `global_type_defs`/`def_map`
    /// registration at `ast_to_hir` time) but has no entry in `def_map`
    /// itself — `path_ty` consults this table instead, recursively
    /// checking the aliased type expression in the alias's own place.
    /// Without this, `type __darwin_useconds_t = __uint32_t;`-style
    /// aliases (extremely common in real Rust — most of libc's typedefs,
    /// and many of std's own `pub type Result<T> = ...`-style aliases)
    /// could never resolve at all: nothing else in the pipeline gives a
    /// non-materializing alias any HIR item to look up.
    pub type_alias_targets: HashMap<DefId, TypeExpr>,
    /// Struct `DefId`s in `items`, keyed by name — built once by
    /// `index_derived_lookups` alongside this package's other derived
    /// tables, so cross-package HIR struct lookups
    /// (`AstProgram::find_hir_struct_def_id`) are an O(1) hash lookup
    /// per package instead of a linear scan over every item every time.
    pub struct_defs_by_name: HashMap<String, DefId>,
    /// For every method `ImplItem` in `items`, its own `DefId` mapped to
    /// the `DefId` of the enclosing `impl` item — built incrementally by
    /// `add_item`/`index_derived_lookups`, so cross-package HIR method
    /// lookups (`AstProgram::find_hir_impl_method`) are an O(1) hash
    /// lookup per package instead of a linear scan over every impl block
    /// and its members every time. Keyed to the impl's own `DefId` (looked
    /// up via `def_map`), not its position in `items` — a `usize` index
    /// would silently go stale the moment an item is ever
    /// inserted/removed/reordered.
    pub impl_method_item_index: HashMap<DefId, DefId>,
    /// Every `impl` item in `items` whose self-type resolves to a nominal
    /// `Res::Def(did)`, keyed by that `did` -> the `DefId`s of every
    /// matching impl item (not their positions — see
    /// `impl_method_item_index`'s doc comment for why) — built
    /// incrementally by `add_item`/`index_derived_lookups`.
    /// `HirProgram::impls_for_adt` unions this across every package (an
    /// impl for type T can live in a different package than T itself), so a
    /// method-call/UFCS-call expression's fast-reject candidate search
    /// (`fp_typing`'s `method_output`) is a per-package O(1) hash lookup
    /// instead of scanning every impl in the whole workspace.
    pub impls_by_self_did: HashMap<DefId, Vec<DefId>>,
    /// An enum variant's own `DefId` -> its enclosing enum item's own
    /// `DefId` (not its position in `items` — see
    /// `impl_method_item_index`'s doc comment for why) — maintained
    /// incrementally by `add_item`, same rationale as
    /// `impl_method_item_index`: `HirProgram`/callers key by a variant's
    /// own `DefId` (e.g. `enum_variant_by_def_id`) but the enum item
    /// itself, not the bare variant, is what's actually stored in `items`.
    pub enum_variant_item_index: HashMap<DefId, DefId>,
    /// Any impl member (method *or* assoc const) or enum variant's own
    /// `DefId` -> its enclosing top-level item's own `DefId` — the general
    /// counterpart of `impl_method_item_index`/`enum_variant_item_index`
    /// above (which only cover methods and variants respectively) for
    /// callers that need to resolve *any* non-top-level member back to the
    /// `def_map` entry it's nested under, e.g. to type-check an `impl`'s
    /// assoc const as its own item. Maintained the same incremental way,
    /// by `add_item`/`index_derived_lookups`.
    pub member_to_owning_item: HashMap<DefId, DefId>,
    /// Fully-qualified name -> HIR `Res` lookup entries exported by this
    /// package (moved from the old `CompiledPackage::hir_exports`) —
    /// populated incrementally by `ast_to_hir` as it registers each
    /// exported definition. `HirProgram` merges these across every loaded
    /// package for cross-package bare-name resolution (`AstProgram`'s old
    /// `find_export`/`find_export_by_suffix`, now `HirProgram`'s).
    pub hir_exports: HashMap<String, Res>,
    /// Memoized `check_type_expr(&impl_item.self_ty)` results, keyed by the
    /// impl's own `self_ty` `TypeExpr`'s `HirId` (stable per declared impl,
    /// independent of any particular call site — an impl's self-type
    /// declaration is checked once against its own generics, never against
    /// a specific receiver, so the result is call-site-independent and safe
    /// to share). Populated by `fp_typing`, not `ast_to_hir`/`add_item` —
    /// unlike the derived indices above, this fills in lazily as typing
    /// actually visits each impl's self-type, not eagerly at HIR
    /// construction, so it's a `RefCell`, not maintained incrementally.
    checked_impl_self_ty_cache: RefCell<HashMap<HirId, Ty>>,
    /// Memoized `resolve_trait_def` results, keyed by the trait's own
    /// `DefId`. A trait definition (its full `items: Vec<TraitItem>` —
    /// every default method, potentially large for a trait like
    /// `Iterator`) never changes once loaded, so cloning it out of
    /// `def_map` is safe to do at most once per trait, not once per
    /// method-call/UFCS-call expression that falls through to a trait's
    /// default-method resolution. `Rc`, not owned, since the whole point is
    /// for repeat callers to share one clone instead of each paying for
    /// their own. Lazily filled by `fp_typing`, same reasoning as
    /// `checked_impl_self_ty_cache`.
    resolved_trait_defs: RefCell<HashMap<DefId, Rc<Trait>>>,
    /// Refinement-type hints for function parameters/return types,
    /// persisted across items — a per-item `HirTypeChecker`'s own transient
    /// hint bookkeeping only lives for the duration of whichever item's
    /// check happens to populate it, but this needs to be discharged
    /// against every later call site of an already-checked function too.
    /// Keyed by the function's own `output` `TypeExpr`'s `HirId` (stable
    /// per declaration) plus a `ParamSlot` discriminating which
    /// parameter/the output the hint belongs to, so every later call site —
    /// even from a different item's checker instance — can still discharge
    /// against it. Lazily filled by `fp_typing`, same reasoning as
    /// `checked_impl_self_ty_cache`.
    refinement_hints: RefCell<HashMap<(HirId, ParamSlot), RefinementHint>>,
    /// Field shapes for a `type X = const { .. };` whose RHS resolves via
    /// `Res::Local(hir_id)` rather than a real `def_map` item — keyed by
    /// that same definition's `DefId`, which `fp_typing::field_ty` recovers
    /// from the `Ty`'s own `AdtDef.did` (constructed with identical
    /// `package_id`/`index`) whenever `AdtFlags::IS_COMPTIME_LOCAL` is set,
    /// instead of consulting `def_map`. Lazily filled by `fp_typing`, same
    /// reasoning as `checked_impl_self_ty_cache`.
    local_struct_fields: RefCell<HashMap<DefId, Vec<(Symbol, Ty)>>>,
    /// Semantic information produced by HIR type checking for this package
    /// — HIR itself remains a source-shaped tree; inferred types and
    /// resolutions are recorded here, keyed by HIR node, rather than
    /// mutating the tree in place. Written by many concurrent per-item
    /// tasks while `fp_typing::TypingShared` is checking this exact
    /// `HirPackage` (`TypingShared` holds the same `Rc` this struct is
    /// wrapped in, so every write lands directly here, not into a
    /// separate side table copied out at the end of typecheck).
    expr_types: RefCell<HashMap<HirId, Ty>>,
    type_expr_types: RefCell<HashMap<HirId, Ty>>,
    pat_types: RefCell<HashMap<HirId, Ty>>,
    /// A `MethodCall` expr's own `hir_id` -> the concrete method `DefId`
    /// it resolved to (never the receiver's/type's `DefId`).
    method_resolutions: RefCell<HashMap<HirId, DefId>>,
    generic_call_args: RefCell<HashMap<HirId, GenericCallResolution>>,
    generic_method_args: RefCell<HashMap<HirId, GenericCallResolution>>,
    const_types: RefCell<HashMap<DefId, Ty>>,
    const_values: RefCell<HashMap<DefId, Value>>,
    /// Comptime-evaluated values of `const { ... }` blocks, keyed by the
    /// block's own `DefId` (minted during AST-to-HIR lowering, see
    /// `ExprConstBlock::def_id`) — the same identity kind named consts use,
    /// via `const_values`.
    const_block_values: RefCell<HashMap<DefId, Value>>,
    /// A `const { .. }` block's own not-yet-published HIR body, recorded
    /// under its own `DefId` the moment the type checker builds a
    /// `ComptimeRequest` for it — since the request itself carries only
    /// `package_id`/`def_id` (see `fp_typing::ComptimeRequest`'s doc
    /// comment), this is how the driver's comptime resolver recovers the
    /// exact block to lower, the same shared-package lookup every other
    /// typed result already goes through.
    pending_comptime_blocks: RefCell<HashMap<DefId, Block>>,
    /// This package's typing diagnostics (warnings and recovered, non-fatal
    /// mismatches — see `fp_typing::TypingShared::record_error`'s doc
    /// comment for the full split with hard item-check aborts). Lives here
    /// directly (not copied out of a scratch `TypingShared` at the end of
    /// typecheck) since `TypingShared` writes straight through to this same
    /// `Rc<HirPackage>` — so a diagnostic survives as long as the package
    /// itself does, with nothing to keep in sync.
    pub diagnostics: crate::diagnostics::DiagnosticManager,
}

impl HirPackage {
    pub fn new() -> Self {
        Self {
            id: PackageId::default(),
            module_tree: resolve::ModuleTree::new(),
            items: Vec::new(),
            def_map: HashMap::new(),
            next_hir_id: 0,
            def_paths: HashMap::new(),
            placeholder_defs: HashSet::new(),
            op_defs: HashMap::new(),
            intrinsic_defs: HashMap::new(),
            type_alias_targets: HashMap::new(),
            struct_defs_by_name: HashMap::new(),
            impl_method_item_index: HashMap::new(),
            impls_by_self_did: HashMap::new(),
            enum_variant_item_index: HashMap::new(),
            member_to_owning_item: HashMap::new(),
            hir_exports: HashMap::new(),
            checked_impl_self_ty_cache: RefCell::new(HashMap::new()),
            resolved_trait_defs: RefCell::new(HashMap::new()),
            refinement_hints: RefCell::new(HashMap::new()),
            local_struct_fields: RefCell::new(HashMap::new()),
            expr_types: RefCell::new(HashMap::new()),
            type_expr_types: RefCell::new(HashMap::new()),
            pat_types: RefCell::new(HashMap::new()),
            method_resolutions: RefCell::new(HashMap::new()),
            generic_call_args: RefCell::new(HashMap::new()),
            generic_method_args: RefCell::new(HashMap::new()),
            const_types: RefCell::new(HashMap::new()),
            const_values: RefCell::new(HashMap::new()),
            const_block_values: RefCell::new(HashMap::new()),
            pending_comptime_blocks: RefCell::new(HashMap::new()),
            diagnostics: crate::diagnostics::DiagnosticManager::new(),
        }
    }

    pub fn with_id(id: PackageId) -> Self {
        Self {
            id,
            ..Self::new()
        }
    }

    pub fn next_id(&mut self, package_id: PackageId) -> HirId {
        let id = self.next_hir_id;
        self.next_hir_id += 1;
        HirId::new(package_id, id)
    }

    /// Registers `item`'s derived-index entries (`struct_defs_by_name`,
    /// `impl_method_item_index`, `impls_by_self_did`,
    /// `enum_variant_item_index`, `member_to_owning_item`), keyed by
    /// `item`'s own stable `DefId` —
    /// the incremental counterpart to a clear-and-rebuild pass: call this
    /// once per item as it's added instead of re-scanning every item in
    /// `items` from scratch whenever the package's HIR changes.
    fn index_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Struct(def) => {
                self.struct_defs_by_name
                    .insert(def.name.as_str().to_string(), item.def_id);
            }
            ItemKind::Enum(def) => {
                for variant in &def.variants {
                    self.enum_variant_item_index.insert(variant.def_id, item.def_id);
                    self.member_to_owning_item.insert(variant.def_id, item.def_id);
                }
            }
            ItemKind::Impl(impl_item) => {
                for impl_member in &impl_item.items {
                    if matches!(impl_member.kind, ImplItemKind::Method(_)) {
                        self.impl_method_item_index
                            .insert(impl_member.def_id, item.def_id);
                    }
                    self.member_to_owning_item.insert(impl_member.def_id, item.def_id);
                }
                let resolved_did = match &impl_item.self_ty.kind {
                    TypeExprKind::Path(path) => match path.res {
                        Some(Res::Def(did)) => Some(did),
                        _ => None,
                    },
                    _ => None,
                };
                match resolved_did {
                    Some(did) => {
                        self.impls_by_self_did.entry(did).or_default().push(item.def_id);
                    }
                    None => {
                        // Not a resolved nominal path (a generic param, a
                        // primitive/tuple/slice extension impl, or an
                        // unresolved path) — `impls_by_self_did` can't key
                        // this impl, so `HirProgram::impls_for_adt`'s fast
                        // path will never surface it; only the `all_impls`
                        // full-scan fallback will. Common in real code
                        // (`impl Add for i32`, `impl<T> Deref for Vec<T>`),
                        // so this is expected, not necessarily a bug — but
                        // flagged so a genuinely-should-have-resolved path
                        // that silently didn't doesn't go unnoticed.
                        crate::diagnostics::report_warning(format!(
                            "impl at {:?} has a self-type that isn't a resolved nominal path; \
                             it won't be found by impls_by_self_did's fast-reject index",
                            item.hir_id
                        ));
                    }
                }
            }
            // No derived-index entry applies to these — a free function,
            // const, trait, query, or item-position expr is never looked
            // up by name/impl-target/variant the way struct/enum/impl
            // items are.
            ItemKind::Function(_)
            | ItemKind::Const(_)
            | ItemKind::Trait(_)
            | ItemKind::Query(_)
            | ItemKind::Expr(_) => {}
        }
    }

    /// Appends `item` to `items` and incrementally maintains every derived
    /// index in the same step — the preferred way to add an item to an
    /// already-published-or-publishing package; no separate rebuild pass
    /// needed afterward.
    pub fn add_item(&mut self, item: Item) {
        self.def_map.insert(item.def_id, item.clone());
        self.index_item(&item);
        self.items.push(item);
    }

    /// Rebuilds every derived index from `items` as they stand right now —
    /// for the one bulk-construction path (`ast_to_hir::HirGenerator`)
    /// that still builds a whole `items: Vec<Item>` up front rather than
    /// through `add_item` one at a time. New code should prefer `add_item`.
    pub fn index_derived_lookups(&mut self) {
        self.struct_defs_by_name.clear();
        self.impl_method_item_index.clear();
        self.impls_by_self_did.clear();
        self.enum_variant_item_index.clear();
        self.member_to_owning_item.clear();
        for index in 0..self.items.len() {
            let item = self.items[index].clone();
            self.index_item(&item);
        }
    }

    pub fn span(&self) -> Span {
        Span::union(self.items.iter().map(Item::span))
    }

    /// The enclosing top-level item's own `DefId` for a member `def_id`
    /// (an impl method/assoc-const, or an enum variant) that isn't itself
    /// a `def_map` key — see `member_to_owning_item`'s doc comment.
    pub fn member_owner(&self, def_id: DefId) -> Option<DefId> {
        self.member_to_owning_item.get(&def_id).copied()
    }

    /// See `checked_impl_self_ty_cache`'s doc comment.
    pub fn checked_impl_self_ty(&self, hir_id: HirId) -> Option<Ty> {
        self.checked_impl_self_ty_cache.borrow().get(&hir_id).cloned()
    }

    pub fn cache_checked_impl_self_ty(&self, hir_id: HirId, ty: Ty) {
        self.checked_impl_self_ty_cache.borrow_mut().insert(hir_id, ty);
    }

    /// See `resolved_trait_defs`'s doc comment.
    pub fn resolved_trait_def(&self, def_id: DefId) -> Option<Rc<Trait>> {
        self.resolved_trait_defs.borrow().get(&def_id).cloned()
    }

    pub fn cache_resolved_trait_def(&self, def_id: DefId, trait_def: Rc<Trait>) {
        self.resolved_trait_defs.borrow_mut().insert(def_id, trait_def);
    }

    /// See `refinement_hints`'s doc comment.
    pub fn refinement_hint(&self, hir_id: HirId, slot: ParamSlot) -> Option<RefinementHint> {
        self.refinement_hints.borrow().get(&(hir_id, slot)).cloned()
    }

    pub fn insert_refinement_hint(&self, hir_id: HirId, slot: ParamSlot, hint: RefinementHint) {
        self.refinement_hints.borrow_mut().insert((hir_id, slot), hint);
    }

    /// See `local_struct_fields`'s doc comment.
    pub fn local_struct_fields(&self, def_id: DefId) -> Option<Vec<(Symbol, Ty)>> {
        self.local_struct_fields.borrow().get(&def_id).cloned()
    }

    pub fn insert_local_struct_fields(&self, def_id: DefId, fields: Vec<(Symbol, Ty)>) {
        self.local_struct_fields.borrow_mut().insert(def_id, fields);
    }

    // --- Typed results (formerly `PackageTypes`) --------------------------
    //
    // Single-entry get/insert pairs for the common "does this one node have
    // a recorded type/resolution yet" query typing itself makes constantly,
    // plus a whole-map snapshot getter for the few bulk consumers (backend
    // lowering, tests) that need every entry at once.

    pub fn expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.expr_types.borrow().get(&hir_id).cloned()
    }

    pub fn record_expr_type(&self, hir_id: HirId, ty: Ty) {
        self.expr_types.borrow_mut().insert(hir_id, ty);
    }

    pub fn expr_types(&self) -> HashMap<HirId, Ty> {
        self.expr_types.borrow().clone()
    }

    pub fn type_expr_type(&self, hir_id: HirId) -> Option<Ty> {
        self.type_expr_types.borrow().get(&hir_id).cloned()
    }

    pub fn record_type_expr_type(&self, hir_id: HirId, ty: Ty) {
        self.type_expr_types.borrow_mut().insert(hir_id, ty);
    }

    pub fn type_expr_types(&self) -> HashMap<HirId, Ty> {
        self.type_expr_types.borrow().clone()
    }

    pub fn pat_type(&self, hir_id: HirId) -> Option<Ty> {
        self.pat_types.borrow().get(&hir_id).cloned()
    }

    pub fn record_pat_type(&self, hir_id: HirId, ty: Ty) {
        self.pat_types.borrow_mut().insert(hir_id, ty);
    }

    pub fn pat_types(&self) -> HashMap<HirId, Ty> {
        self.pat_types.borrow().clone()
    }

    /// A `MethodCall` expr's own `hir_id` -> the concrete method `DefId` it
    /// resolved to.
    pub fn method_resolution(&self, hir_id: HirId) -> Option<DefId> {
        self.method_resolutions.borrow().get(&hir_id).copied()
    }

    pub fn record_method_resolution(&self, hir_id: HirId, def_id: DefId) {
        self.method_resolutions.borrow_mut().insert(hir_id, def_id);
    }

    pub fn method_resolutions(&self) -> HashMap<HirId, DefId> {
        self.method_resolutions.borrow().clone()
    }

    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.generic_call_args.borrow().get(&hir_id).cloned()
    }

    pub fn record_generic_call_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        self.generic_call_args.borrow_mut().insert(hir_id, resolution);
    }

    pub fn generic_call_args(&self) -> HashMap<HirId, GenericCallResolution> {
        self.generic_call_args.borrow().clone()
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.generic_method_args.borrow().get(&hir_id).cloned()
    }

    pub fn record_generic_method_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        self.generic_method_args.borrow_mut().insert(hir_id, resolution);
    }

    pub fn generic_method_args(&self) -> HashMap<HirId, GenericCallResolution> {
        self.generic_method_args.borrow().clone()
    }

    pub fn const_type(&self, def_id: DefId) -> Option<Ty> {
        self.const_types.borrow().get(&def_id).cloned()
    }

    pub fn record_const_type(&self, def_id: DefId, ty: Ty) {
        self.const_types.borrow_mut().insert(def_id, ty);
    }

    pub fn const_types(&self) -> HashMap<DefId, Ty> {
        self.const_types.borrow().clone()
    }

    pub fn const_value(&self, def_id: DefId) -> Option<Value> {
        self.const_values.borrow().get(&def_id).cloned()
    }

    pub fn record_const_value(&self, def_id: DefId, value: Value) {
        self.const_values.borrow_mut().insert(def_id, value);
    }

    pub fn const_values(&self) -> HashMap<DefId, Value> {
        self.const_values.borrow().clone()
    }

    /// Comptime-evaluated value of a `const { ... }` block, keyed by its
    /// own `DefId` — see `const_block_values`'s doc comment.
    pub fn const_block_value(&self, def_id: DefId) -> Option<Value> {
        self.const_block_values.borrow().get(&def_id).cloned()
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        self.const_block_values.borrow_mut().insert(def_id, value);
    }

    pub fn const_block_values(&self) -> HashMap<DefId, Value> {
        self.const_block_values.borrow().clone()
    }

    /// See `pending_comptime_blocks`'s doc comment.
    pub fn record_pending_comptime_block(&self, def_id: DefId, block: Block) {
        self.pending_comptime_blocks.borrow_mut().insert(def_id, block);
    }

    pub fn pending_comptime_block(&self, def_id: DefId) -> Option<Block> {
        self.pending_comptime_blocks.borrow().get(&def_id).cloned()
    }
}

impl Default for HirPackage {
    fn default() -> Self {
        Self::new()
    }
}
