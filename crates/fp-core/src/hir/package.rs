use super::*;
use std::cell::RefCell;
use std::rc::Rc;

/// The fixed set of primitive scalar type names an impl's self-type can
/// name directly (`impl u8 { .. }`), mirrored from the identical name set
/// `fp_typing::hir_typeck::primitive_path_ty`/`primitive_ty` already use to
/// go the other way (name -> checked `Ty`) — kept in sync manually since
/// `fp-core` can't depend on `fp-typing` to share one table.
const PRIMITIVE_SELF_TYPE_NAMES: &[&str] = &[
    "bool", "char", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128",
    "usize", "f16", "f32", "f64", "f128", "str",
];

/// The fast-reject bucket key for an impl's own self-type, mirroring
/// rustc's `SimplifiedType` (`rustc_middle::ty::fast_reject`) exactly —
/// including treating a nominal ADT self-type as just one more bucket
/// variant, not a separate mechanism from every other concrete shape.
/// `impls_by_bucket` is the single index every method/associated-item
/// candidate search goes through; there is deliberately no second,
/// ADT-only index alongside it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplBucketKey {
    /// A resolved nominal struct/enum self-type (`impl Vec<T> { .. }`),
    /// keyed by that type's own `DefId`.
    Adt(DefId),
    /// Any other concrete, classifiable self-type shape (`impl u8 { .. }`,
    /// `impl<T> Trait for [T] { .. }`, ...) — see `classify_impl_shape`.
    /// Reuses `BuiltinSelfType::bucket_key()`'s own string convention for
    /// the non-primitive shapes, and the bare primitive name for
    /// primitives, rather than inventing a second key encoding.
    Shape(String),
}

/// One compiled package's HIR content — items, definitions, and (as of the
/// `ModuleTree` migration) its own module/name-resolution tree. Several of
/// these live inside a `HirProgram`, which owns the whole multi-package
/// compiled result (see `HirProgram`'s own doc comment).
#[derive(Debug, Clone, PartialEq)]
pub struct HirPackage {
    pub id: PackageId,
    /// The package whose reserved prelude bindings are imported while this
    /// package is resolved. Copied from provider metadata at the AST-to-HIR
    /// boundary so later lowering never infers a prelude from package names
    /// or dependency contents.
    pub prelude: Option<PackageId>,
    /// Resolved direct dependencies visible from this crate's extern prelude.
    /// This is the HIR equivalent of rustc's crate metadata edge: name
    /// resolution must consult only crates reachable through this list, not
    /// every package loaded in the compilation session.
    pub dependencies: Vec<PackageId>,
    pub items: Vec<Item>,
    pub def_map: HashMap<DefId, Item>,
    /// High-water mark for `next_def_id` — per-package, not a driver-wide
    /// counter: `DefId` is already `{package_id, index}`, so two packages
    /// minting indices from independently-reset counters can never
    /// collide, and this field's own `def_map` (once construction
    /// finishes) is the single source of truth a caller would otherwise
    /// have to separately track and pass back in. Starts at 1 — index 0 is
    /// reserved for the package-root `OwnerId` (see `OwnerId::root`), so a
    /// real item can never mint a `DefId` that collides with it.
    pub next_def_id: u32,
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
    /// real lowering of the original source construct — for example, a
    /// trait declaration may also have a placeholder `Const` entry so the
    /// definition has a HIR shape to type-check as a value/type reference,
    /// while the real trait members live in `ItemKind::Trait`. Consumers
    /// that reconstruct AST from HIR (`HirToAstLifter`) must skip these
    /// rather than lift the placeholder itself, so backends that work from
    /// the original source item (e.g. fp-kotlin modeling a trait as a real
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
    /// itself — `path_ty` consults the already-resolved result recorded for
    /// this target expression's `HirId`.
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
    /// Every impl in `items` whose self-type is *not* a resolved nominal
    /// `Res::Def(did)` path (so `impls_by_self_did` can't key it) but
    /// still resolves to a concrete, classifiable shape — `impl Trait for
    /// u8`, `impl<T> Trait for [T]`, `impl<T> Trait for (T, T)`, etc. —
    /// keyed by a stable shape-bucket string (reusing `BuiltinSelfType::
    /// bucket_key()`'s own convention for the non-primitive shapes, and
    /// the bare primitive name for primitives — see `classify_impl_shape`).
    /// Together with `blanket_impls`, this is what makes every method/
    /// associated-item candidate search a bounded, indexed lookup instead
    /// of a scan over every impl in the workspace: see
    /// `HirProgram::impls_for_shape`'s doc comment.
    pub impls_by_shape: HashMap<String, Vec<DefId>>,
    /// Impls whose self-type is literally one of the impl's *own* generic
    /// type parameters (`impl<T> Trait for T`, `impl<T: ?Sized> Borrow<T>
    /// for T`) — a true blanket impl, which by construction must be
    /// checked against every receiver shape (there's nothing to bucket it
    /// under). Kept as its own small list rather than folded into
    /// `impls_by_shape` under some catch-all key, since every shape-keyed
    /// lookup must union this list in, not just one shape's bucket.
    pub blanket_impls: Vec<DefId>,
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
    /// exported definition. Exact cross-package resolution is performed by
    /// `HirProgram`'s namespace-aware module-tree index; this map remains a
    /// metadata compatibility surface for package serialization.
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
    /// Memoized `fp_typing::function_signature` results, keyed by the
    /// function's own `output` type's `HirId` (stable per declared
    /// function, independent of any particular call site — a function's
    /// signature is checked once against its own generics, never against a
    /// specific call site, so the result is call-site-independent and safe
    /// to share). Lazily filled by `fp_typing`, same reasoning as
    /// `checked_impl_self_ty_cache`.
    function_signature_cache: RefCell<HashMap<HirId, Ty>>,
    /// Memoized `fp_typing::impl_assoc_types` results, keyed the same way
    /// as `checked_impl_self_ty_cache` (the impl's own `self_ty` `HirId` —
    /// there's no separate stable id on `hir::Impl` itself, and this one
    /// is already unique per declared impl). `assoc_type_for_self`'s own
    /// candidate search calls this for every candidate whose self-type
    /// shape merely *might* match, not just the one that's ultimately
    /// used — an impl whose own associated-type binding happens to be a
    /// genuinely broken/unresolvable one (a real bug in that impl, not in
    /// whatever unrelated item's own type-check first reaches it as a
    /// candidate) would otherwise have its diagnostic re-recorded once
    /// per candidate-search call site across the whole workspace instead
    /// of once, the same O(workspace) blowup class already fixed for
    /// `checked_impl_self_ty`/`function_signature` above.
    impl_assoc_types_cache: RefCell<HashMap<HirId, HashMap<Symbol, Ty>>>,
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
    /// Memoized `fp_typing::assoc_type_for_self` results, keyed by
    /// `(target type's debug repr, assoc name)`. Without this, a single
    /// unqualified `T::AssocName` projection (e.g. `usize::Output`, from a
    /// `<usize as Add>::Output` UFCS path that `parse_qualified_path_type`
    /// flattens, dropping the `as Trait` disambiguator) pays a full
    /// O(impls in workspace) scan *every time it's referenced* — tens of
    /// thousands of times over for a macro-generated `add_impl! { usize u8
    /// u16 .. }`-style block in the vendored std, the same O(workspace)
    /// blowup class `checked_impl_self_ty_cache`/`function_signature_cache`
    /// already guard against. Lazily filled by `fp_typing`, same reasoning
    /// as `checked_impl_self_ty_cache`.
    assoc_type_for_self_cache: RefCell<HashMap<(String, Symbol), Option<Ty>>>,
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
    /// Raw refinement annotations encountered by `fp_typing::check_type_expr`
    /// (which is synchronous), keyed by the `TypeExpr`'s own `hir_id` —
    /// staging for `refinement_hints` above, not a replacement for it: a
    /// caller that still has the same `TypeExpr` in hand right after
    /// `check_type_expr` returns (e.g. the `Let` arm) takes it straight out
    /// by that raw `hir_id`, while `function_signature` instead re-keys it
    /// by `(function_hir_id, ParamSlot)` into `refinement_hints` so a
    /// *different* checker instance, at a later call site, can still
    /// discharge it. Globally unique per `HirId` (never per-item-scoped),
    /// so sharing this on the package is safe even with multiple items'
    /// checks running concurrently — no two items' `TypeExpr`s ever share a
    /// `hir_id`.
    raw_refinement_hints: RefCell<HashMap<HirId, RefinementHint>>,
    /// The resolved set of literal strings a string-literal/union-of-literal/
    /// template-literal type `TypeExpr` expands to, keyed by that node's own
    /// `hir_id`. Purely an internal accelerator for `fp_typing::check_type_expr`
    /// so a containing `Template`/union/intrinsic-string-op arm can look up
    /// what an already-checked child resolved to without re-walking the raw
    /// `TypeExpr` tree — never consulted past typecheck (every arm still
    /// erases to the same `str`-shaped `Ty` regardless of this side table).
    literal_type_hints: RefCell<HashMap<HirId, Vec<String>>>,
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
    reflection_field_intrinsics: RefCell<HashMap<HirId, crate::intrinsics::IntrinsicKind>>,
    reflection_field_intrinsics_by_span:
        RefCell<HashMap<crate::span::Span, crate::intrinsics::IntrinsicKind>>,
    generic_call_args: RefCell<HashMap<HirId, GenericCallResolution>>,
    generic_method_args: RefCell<HashMap<HirId, GenericCallResolution>>,
    const_types: RefCell<HashMap<DefId, Ty>>,
    const_values: RefCell<HashMap<DefId, Value>>,
    /// Comptime-evaluated values of `const { ... }` blocks, keyed by the
    /// block's own `DefId` (minted during AST-to-HIR lowering, see
    /// `ExprConstBlock::def_id`) — the same identity kind named consts use,
    /// via `const_values`.
    anonymous_consts: RefCell<HashMap<DefId, Block>>,
    /// A `const { .. }` block's own HIR body, recorded under its own
    /// `DefId` once, unconditionally, at AST-to-HIR lowering time (the
    /// moment `ExprConstBlock`/`TypeExprKind::ConstBlock` mint that
    /// `DefId` — see `AstToHirLowerer::transform_const_block_to_hir` and the
    /// type-position `ConstBlock` lowering site). Exists because
    /// `fp_typing::ComptimeRequest` carries only `package_id`/`def_id`
    /// (never the block itself, see that type's doc comment), so this is
    /// how the driver's comptime resolver recovers the exact block to
    /// lower from just that `DefId` — the same shared-package lookup
    /// every other typed result already goes through, alongside
    /// `type_alias_targets` (another "extra HIR shape with no real
    /// `def_map` entry" index).
    /// This package's typing diagnostics (warnings and recovered, non-fatal
    /// mismatches — see `fp_typing::TypingShared::record_error`'s doc
    /// comment for the full split with hard item-check aborts). Lives here
    /// directly (not copied out of a scratch `TypingShared` at the end of
    /// typecheck) since `TypingShared` writes straight through to this same
    /// `Rc<HirPackage>` — so a diagnostic survives as long as the package
    /// itself does, with nothing to keep in sync.
    pub diagnostics: crate::diagnostics::DiagnosticManager,
}

/// Result of classifying an impl's self-type for `impls_by_shape`/
/// `blanket_impls` indexing — see `classify_impl_shape`.
enum ImplShapeClass {
    Nominal(DefId),
    Shape(String),
    Blanket,
    Unclassified,
}

/// Structurally classifies `impl_item`'s own self-type (no type-checking
/// needed — this only looks at the HIR shape, specifically `Res` as
/// already recorded by `ast_to_hir`'s `canonical_type_path`/self-type
/// lowering) into a shape-bucket key, a blanket impl over one of
/// `impl_item`'s own generic params, or `Unclassified` if it's neither a
/// recognized concrete shape nor a resolved nominal ADT path (already
/// handled separately by the caller via `impls_by_self_did`) nor a
/// blanket impl. Every legitimate impl self-type in real Rust falls into
/// one of the first two cases.
///
/// Non-primitive shapes (`&T`, `[T]`, `[T; N]`, `(A, B)`, `fn(..)`, `!`)
/// lower to a `Path` tagged with `Res::Builtin(BuiltinSelfType)` — reuse
/// its own `bucket_key()` directly rather than re-deriving an equivalent
/// key here. A primitive scalar (`impl u8 { .. }`) has no such tag (its
/// self-type `Path` is simply unresolved, its first segment the primitive
/// name); match `PRIMITIVE_SELF_TYPE_NAMES` directly for that case.
fn classify_impl_shape(
    impl_item: &Impl,
    type_alias_targets: &HashMap<DefId, TypeExpr>,
) -> ImplShapeClass {
    // A primitive self-type written where the parser recognizes it as a
    // literal type expression (`ast::Value::Type(Ty::Primitive(_))`, e.g.
    // real std's `impl Add for i64`) lowers straight to
    // `TypeExprKind::Primitive` via `transform_type_to_hir`'s early
    // `Value::Type` shortcut — bypassing the `Path`/`Res::Builtin`
    // machinery entirely, so it never reached `PRIMITIVE_SELF_TYPE_NAMES`
    // below at all. Without this arm every trait impl on a primitive
    // (`Add`/`Sub`/`PartialOrd`/...) silently fails to index, and neither
    // its methods nor its associated types (`<i64 as Add>::Output`) are
    // ever discoverable by `method_declared_signature_at`/
    // `assoc_type_for_self`.
    if let TypeExprKind::Primitive(prim) = &impl_item.self_ty.kind {
        if let Some(name) = primitive_shape_name(prim) {
            return ImplShapeClass::Shape(name);
        }
        return ImplShapeClass::Unclassified;
    }
    // A compound self-type (`impl Trait for &T`/`[T]`/`(A, B)`/`fn(..)
    // -> R`/...) lowers straight to its own native `TypeExprKind` variant
    // via `transform_impl`'s `transform_type_to_hir` call — it never goes
    // through `ast_expr_to_hir_path`'s `Res::Builtin`-tagged `Path`
    // machinery at all (that's only reached for a self-type built via
    // *that* function, e.g. `predeclare_items`'s canonical-method-path
    // computation, a separate concern from this item's own stored
    // `self_ty`). Without these arms every impl on one of these shapes
    // (extremely common throughout real vendored std — `impl<T> PartialEq
    // for &T`, `impl<T> Iterator for &mut [T]`, every `impl<Args> FnMut
    // <Args> for fn(Args) -> R`, ...) was silently unindexed. Reuses the
    // exact same bucket keys `Res::Builtin`'s own `bucket_key()` already
    // established, so a self-type reaching either route still lands in
    // the same bucket.
    match &impl_item.self_ty.kind {
        TypeExprKind::Ref(_) => return ImplShapeClass::Shape("&".to_string()),
        TypeExprKind::Ptr { mutable, .. } => {
            return ImplShapeClass::Shape(if *mutable { "*mut" } else { "*const" }.to_string());
        }
        TypeExprKind::Slice(_) => return ImplShapeClass::Shape("[]".to_string()),
        TypeExprKind::Array(_, _) => return ImplShapeClass::Shape("[;N]".to_string()),
        TypeExprKind::Tuple(elems) if !elems.is_empty() => {
            return ImplShapeClass::Shape("(,)".to_string());
        }
        TypeExprKind::Tuple(_) => return ImplShapeClass::Shape("()".to_string()),
        TypeExprKind::FnPtr(_) => return ImplShapeClass::Shape("fn(..)".to_string()),
        TypeExprKind::Never => return ImplShapeClass::Shape("!".to_string()),
        _ => {}
    }
    classify_type_shape(
        &impl_item.self_ty,
        &impl_item.generics,
        type_alias_targets,
        &mut HashSet::new(),
    )
}

/// Classify the outer type exactly once, following transparent aliases by
/// identity.  An alias is not a dispatch type in rustc: its simplified type
/// is the simplified type of its target.  Keeping this lookup identity-based
/// also handles aliases imported from another module/package without making
/// their spelling part of dispatch.
fn classify_type_shape(
    ty: &TypeExpr,
    generics: &Generics,
    aliases: &HashMap<DefId, TypeExpr>,
    active_aliases: &mut HashSet<DefId>,
) -> ImplShapeClass {
    match &ty.kind {
        TypeExprKind::Primitive(prim) => primitive_shape_name(prim)
            .map(ImplShapeClass::Shape)
            .unwrap_or(ImplShapeClass::Unclassified),
        TypeExprKind::Ref(_) => ImplShapeClass::Shape("&".to_string()),
        TypeExprKind::Ptr { mutable, .. } => {
            ImplShapeClass::Shape(if *mutable { "*mut" } else { "*const" }.to_string())
        }
        TypeExprKind::Slice(_) => ImplShapeClass::Shape("[]".to_string()),
        TypeExprKind::Array(_, _) => ImplShapeClass::Shape("[;N]".to_string()),
        TypeExprKind::Tuple(elems) => ImplShapeClass::Shape(if elems.is_empty() {
            "()".to_string()
        } else {
            "(,)".to_string()
        }),
        TypeExprKind::FnPtr(_) => ImplShapeClass::Shape("fn(..)".to_string()),
        TypeExprKind::Never => ImplShapeClass::Shape("!".to_string()),
        TypeExprKind::Path(path) => match &path.res {
            Some(Res::Builtin(builtin)) => ImplShapeClass::Shape(builtin.bucket_key().to_string()),
            Some(Res::Def(did)) if generics.params.iter().any(|param| param.def_id == *did) => {
                ImplShapeClass::Blanket
            }
            Some(Res::Def(did)) => {
                if let Some(target) = aliases.get(did) {
                    if !active_aliases.insert(did.clone()) {
                        return ImplShapeClass::Unclassified;
                    }
                    let result = classify_type_shape(target, generics, aliases, active_aliases);
                    active_aliases.remove(did);
                    result
                } else {
                    ImplShapeClass::Nominal(did.clone())
                }
            }
            _ if path.segments.len() == 1
                && PRIMITIVE_SELF_TYPE_NAMES.contains(&path.segments[0].name.as_str()) =>
            {
                ImplShapeClass::Shape(path.segments[0].name.as_str().to_string())
            }
            _ => ImplShapeClass::Unclassified,
        },
        _ => ImplShapeClass::Unclassified,
    }
}

/// `ast::TypePrimitive` -> the same canonical scalar name
/// `PRIMITIVE_SELF_TYPE_NAMES` lists, mirroring `TypeInt`/`DecimalType`'s
/// own `Display` impls (already exactly "i64"/"u8"/"f64"/...). `List` has
/// no primitive self-type name of its own (never a real impl self-type
/// shape) and returns `None`.
fn primitive_shape_name(prim: &TypePrimitive) -> Option<String> {
    match prim {
        TypePrimitive::Int(int) => Some(int.to_string()),
        TypePrimitive::Decimal(decimal) => Some(decimal.to_string()),
        TypePrimitive::Bool => Some("bool".to_string()),
        TypePrimitive::Char => Some("char".to_string()),
        TypePrimitive::String => Some("str".to_string()),
        TypePrimitive::List => None,
    }
}

impl HirPackage {
    /// `id` is a required parameter, not filled in after the fact — a
    /// caller that builds a fresh `HirPackage` and forgets to copy its real
    /// id back in (as `AstToHirLowerer::transform_package`/
    /// `transform_module_inner` both once did) previously got a
    /// silently-plausible `PackageId::default()` instead of a compile
    /// error, which let two different packages collide under the same key
    /// in `HirProgram::add_package` with no diagnostic at all.
    pub fn new(id: PackageId) -> Self {
        Self {
            id,
            prelude: None,
            dependencies: Vec::new(),
            items: Vec::new(),
            def_map: HashMap::new(),
            next_def_id: 1,
            def_paths: HashMap::new(),
            placeholder_defs: HashSet::new(),
            op_defs: HashMap::new(),
            intrinsic_defs: HashMap::new(),
            type_alias_targets: HashMap::new(),
            struct_defs_by_name: HashMap::new(),
            impl_method_item_index: HashMap::new(),
            impls_by_self_did: HashMap::new(),
            impls_by_shape: HashMap::new(),
            blanket_impls: Vec::new(),
            enum_variant_item_index: HashMap::new(),
            member_to_owning_item: HashMap::new(),
            hir_exports: HashMap::new(),
            checked_impl_self_ty_cache: RefCell::new(HashMap::new()),
            impl_assoc_types_cache: RefCell::new(HashMap::new()),
            function_signature_cache: RefCell::new(HashMap::new()),
            resolved_trait_defs: RefCell::new(HashMap::new()),
            assoc_type_for_self_cache: RefCell::new(HashMap::new()),
            refinement_hints: RefCell::new(HashMap::new()),
            raw_refinement_hints: RefCell::new(HashMap::new()),
            literal_type_hints: RefCell::new(HashMap::new()),
            local_struct_fields: RefCell::new(HashMap::new()),
            expr_types: RefCell::new(HashMap::new()),
            type_expr_types: RefCell::new(HashMap::new()),
            pat_types: RefCell::new(HashMap::new()),
            method_resolutions: RefCell::new(HashMap::new()),
            reflection_field_intrinsics: RefCell::new(HashMap::new()),
            reflection_field_intrinsics_by_span: RefCell::new(HashMap::new()),
            generic_call_args: RefCell::new(HashMap::new()),
            generic_method_args: RefCell::new(HashMap::new()),
            const_types: RefCell::new(HashMap::new()),
            const_values: RefCell::new(HashMap::new()),
            anonymous_consts: RefCell::new(HashMap::new()),
            diagnostics: crate::diagnostics::DiagnosticManager::new(),
        }
    }

    pub fn next_def_id(&mut self) -> DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;
        DefId::new(self.id.clone(), id)
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
                    .insert(def.name.as_str().to_string(), item.def_id.clone());
            }
            ItemKind::Enum(def) => {
                for variant in &def.variants {
                    self.enum_variant_item_index
                        .insert(variant.def_id.clone(), item.def_id.clone());
                    self.member_to_owning_item
                        .insert(variant.def_id.clone(), item.def_id.clone());
                }
            }
            ItemKind::Impl(impl_item) => {
                for impl_member in &impl_item.items {
                    if matches!(impl_member.kind, ImplItemKind::Method(_)) {
                        self.impl_method_item_index
                            .insert(impl_member.def_id.clone(), item.def_id.clone());
                    }
                    self.member_to_owning_item
                        .insert(impl_member.def_id.clone(), item.def_id.clone());
                }
                // Nominal identity is established by name resolution before
                // indexing, exactly as in rustc's `DefId`-keyed
                // `SimplifiedType` table.  Do not reconstruct an identity
                // from path spelling here: a re-export or a same-named item
                // in another package can otherwise produce an index key that
                // differs from the `Res::Def` consumed by type checking.
                match classify_impl_shape(impl_item, &self.type_alias_targets) {
                    ImplShapeClass::Nominal(did) => {
                        self.impls_by_self_did
                            .entry(did)
                            .or_default()
                            .push(item.def_id.clone());
                    }
                    ImplShapeClass::Shape(shape) => {
                        self.impls_by_shape
                            .entry(shape)
                            .or_default()
                            .push(item.def_id.clone());
                    }
                    ImplShapeClass::Blanket => {
                        self.blanket_impls.push(item.def_id.clone());
                    }
                    // Rustc's fast-reject table silently leaves impls whose
                    // self type has no simplified key out of the fast path.
                    // This is an indexing limitation, not a source error;
                    // reporting it as a hard diagnostic creates a cascade
                    // before the consumer's actual obligations are checked.
                    ImplShapeClass::Unclassified => {}
                }
            }
            // No derived-index entry applies to these — a free function,
            // const, trait, query, or item-position expr is never looked
            // up by name/impl-target/variant the way struct/enum/impl
            // items are.
            ItemKind::Function(_)
            | ItemKind::Const(_)
            | ItemKind::TypeAlias(_)
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
        self.def_map.insert(item.def_id.clone(), item.clone());
        self.index_item(&item);
        self.items.push(item);
    }

    /// Rebuilds every derived index from `items` as they stand right now —
    /// for the one bulk-construction path (`ast_to_hir::AstToHirLowerer`)
    /// that still builds a whole `items: Vec<Item>` up front rather than
    /// through `add_item` one at a time. New code should prefer `add_item`.
    pub fn index_derived_lookups(&mut self) {
        // Bulk lowerers append top-level items directly and may not update
        // the point-lookup map on every path. Rebuild it from the item list
        // before deriving the secondary indexes.
        for item in &self.items {
            self.def_map
                .entry(item.def_id.clone())
                .or_insert_with(|| item.clone());
        }
        self.struct_defs_by_name.clear();
        self.impl_method_item_index.clear();
        self.impls_by_self_did.clear();
        self.impls_by_shape.clear();
        self.blanket_impls.clear();
        self.enum_variant_item_index.clear();
        self.member_to_owning_item.clear();
        let items: Vec<Item> = self.def_map.values().cloned().collect();
        for item in items {
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
        self.member_to_owning_item.get(&def_id).cloned()
    }

    /// Point lookup into `def_map` — same-package counterpart of
    /// `HirProgram::item`, for a caller that has already routed to this
    /// specific package (e.g. by `def_id.package_id`) and has no reason to
    /// reach into `def_map` directly.
    pub fn item(&self, def_id: &DefId) -> Option<&Item> {
        self.def_map.get(def_id)
    }

    /// Point lookup into `def_paths` — same-package counterpart of
    /// `HirProgram::def_path`.
    pub fn def_path(&self, def_id: &DefId) -> Option<&DefPath> {
        self.def_paths.get(def_id)
    }

    /// Every definition this package knows about, including nested/local
    /// ones only ever recorded in `def_map` (never in `items`, which only
    /// lists top-level items) — distinct from `HirProgram::all_items`,
    /// which iterates `items` only.
    pub fn all_defs(&self) -> impl Iterator<Item = &Item> {
        self.def_map.values()
    }

    /// See `checked_impl_self_ty_cache`'s doc comment.
    pub fn checked_impl_self_ty(&self, hir_id: HirId) -> Option<Ty> {
        self.checked_impl_self_ty_cache
            .borrow()
            .get(&hir_id)
            .cloned()
    }

    pub fn cache_checked_impl_self_ty(&self, hir_id: HirId, ty: Ty) {
        self.checked_impl_self_ty_cache
            .borrow_mut()
            .insert(hir_id, ty);
    }

    /// See `impl_assoc_types_cache`'s doc comment.
    pub fn impl_assoc_types(&self, hir_id: HirId) -> Option<HashMap<Symbol, Ty>> {
        self.impl_assoc_types_cache.borrow().get(&hir_id).cloned()
    }

    pub fn cache_impl_assoc_types(&self, hir_id: HirId, types: HashMap<Symbol, Ty>) {
        self.impl_assoc_types_cache
            .borrow_mut()
            .insert(hir_id, types);
    }

    /// See `function_signature_cache`'s doc comment.
    pub fn function_signature(&self, hir_id: HirId) -> Option<Ty> {
        self.function_signature_cache.borrow().get(&hir_id).cloned()
    }

    pub fn cache_function_signature(&self, hir_id: HirId, ty: Ty) {
        self.function_signature_cache
            .borrow_mut()
            .insert(hir_id, ty);
    }

    /// See `resolved_trait_defs`'s doc comment.
    pub fn resolved_trait_def(&self, def_id: DefId) -> Option<Rc<Trait>> {
        self.resolved_trait_defs.borrow().get(&def_id).cloned()
    }

    pub fn cache_resolved_trait_def(&self, def_id: DefId, trait_def: Rc<Trait>) {
        self.resolved_trait_defs
            .borrow_mut()
            .insert(def_id, trait_def);
    }

    /// See `assoc_type_for_self_cache`'s doc comment. The outer `Option`
    /// distinguishes "not cached yet" from the inner `Option`, "cached and
    /// confirmed unresolvable" (a real, storable answer — not every
    /// `T::AssocName` projection resolves, and re-scanning every impl again
    /// for a projection already confirmed absent would defeat the point of
    /// caching at all).
    pub fn assoc_type_for_self(&self, key: &(String, Symbol)) -> Option<Option<Ty>> {
        self.assoc_type_for_self_cache.borrow().get(key).cloned()
    }

    pub fn cache_assoc_type_for_self(&self, key: (String, Symbol), result: Option<Ty>) {
        self.assoc_type_for_self_cache
            .borrow_mut()
            .insert(key, result);
    }

    /// See `refinement_hints`'s doc comment.
    pub fn refinement_hint(&self, hir_id: HirId, slot: ParamSlot) -> Option<RefinementHint> {
        self.refinement_hints.borrow().get(&(hir_id, slot)).cloned()
    }

    pub fn insert_refinement_hint(&self, hir_id: HirId, slot: ParamSlot, hint: RefinementHint) {
        self.refinement_hints
            .borrow_mut()
            .insert((hir_id, slot), hint);
    }

    /// See `raw_refinement_hints`'s doc comment. Take, not peek — a raw hint
    /// is staging for exactly one later consumer (the `Let` arm, or
    /// `function_signature` re-keying it into `refinement_hints`), never
    /// read twice.
    pub fn take_raw_refinement_hint(&self, hir_id: HirId) -> Option<RefinementHint> {
        self.raw_refinement_hints.borrow_mut().remove(&hir_id)
    }

    pub fn insert_raw_refinement_hint(&self, hir_id: HirId, hint: RefinementHint) {
        self.raw_refinement_hints.borrow_mut().insert(hir_id, hint);
    }

    /// See `literal_type_hints`'s doc comment.
    pub fn literal_type_hint(&self, hir_id: HirId) -> Option<Vec<String>> {
        self.literal_type_hints.borrow().get(&hir_id).cloned()
    }

    pub fn insert_literal_type_hint(&self, hir_id: HirId, literals: Vec<String>) {
        self.literal_type_hints
            .borrow_mut()
            .insert(hir_id, literals);
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

    /// The target node whose resolved type is the expansion of a transparent
    /// alias. The lookup key is independent of the target's syntax.
    pub fn type_alias_target_hir_id(&self, def_id: &DefId) -> Option<HirId> {
        self.type_alias_targets
            .get(def_id)
            .map(|target| target.hir_id.clone())
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
        self.method_resolutions.borrow().get(&hir_id).cloned()
    }

    pub fn record_method_resolution(&self, hir_id: HirId, def_id: DefId) {
        self.method_resolutions.borrow_mut().insert(hir_id, def_id);
    }

    pub fn method_resolutions(&self) -> HashMap<HirId, DefId> {
        self.method_resolutions.borrow().clone()
    }

    pub fn reflection_field_intrinsic(
        &self,
        hir_id: HirId,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.reflection_field_intrinsics
            .borrow()
            .get(&hir_id)
            .copied()
    }

    pub fn reflection_field_intrinsic_at_span(
        &self,
        span: crate::span::Span,
    ) -> Option<crate::intrinsics::IntrinsicKind> {
        self.reflection_field_intrinsics_by_span
            .borrow()
            .get(&span)
            .copied()
    }

    pub fn record_reflection_field_intrinsic(
        &self,
        hir_id: HirId,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        self.reflection_field_intrinsics
            .borrow_mut()
            .insert(hir_id, intrinsic);
    }

    pub fn record_reflection_field_intrinsic_at_span(
        &self,
        span: crate::span::Span,
        intrinsic: crate::intrinsics::IntrinsicKind,
    ) {
        self.reflection_field_intrinsics_by_span
            .borrow_mut()
            .insert(span, intrinsic);
    }

    pub fn generic_call_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.generic_call_args.borrow().get(&hir_id).cloned()
    }

    pub fn record_generic_call_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        self.generic_call_args
            .borrow_mut()
            .insert(hir_id, resolution);
    }

    pub fn generic_call_args(&self) -> HashMap<HirId, GenericCallResolution> {
        self.generic_call_args.borrow().clone()
    }

    pub fn generic_method_arg(&self, hir_id: HirId) -> Option<GenericCallResolution> {
        self.generic_method_args.borrow().get(&hir_id).cloned()
    }

    pub fn record_generic_method_arg(&self, hir_id: HirId, resolution: GenericCallResolution) {
        self.generic_method_args
            .borrow_mut()
            .insert(hir_id, resolution);
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
        self.const_value(def_id)
    }

    pub fn record_const_block_value(&self, def_id: DefId, value: Value) {
        self.record_const_value(def_id, value);
    }

    pub fn const_block_values(&self) -> HashMap<DefId, Value> {
        self.const_values()
    }

    /// Register an anonymous const body under its ordinary compiler DefId.
    pub fn add_anonymous_const(&mut self, def_id: DefId, block: Block) {
        self.anonymous_consts.get_mut().insert(def_id, block);
    }

    pub fn anonymous_const(&self, def_id: DefId) -> Option<Block> {
        self.anonymous_consts.borrow().get(&def_id).cloned()
    }

    pub fn anonymous_consts(&self) -> HashMap<DefId, Block> {
        self.anonymous_consts.borrow().clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn path_type(res: Res, name: &str) -> TypeExpr {
        TypeExpr::new(
            HirId::new(OwnerId::root(PackageId::new("test")), 1),
            TypeExprKind::Path(Path {
                segments: vec![PathSegment {
                    name: name.into(),
                    args: None,
                }],
                res: Some(res),
            }),
            Span::null(),
        )
    }

    fn impl_for(self_ty: TypeExpr, generics: Generics) -> Impl {
        Impl {
            generics,
            trait_ty: None,
            self_ty,
            items: Vec::new(),
        }
    }

    #[test]
    fn impl_index_classifies_nominal_alias_by_target_identity() {
        let target = DefId::new(PackageId::new("core"), 7);
        let alias = DefId::new(PackageId::new("std"), 9);
        let mut aliases = HashMap::new();
        aliases.insert(alias.clone(), path_type(Res::Def(target.clone()), "Target"));

        let class = classify_impl_shape(
            &impl_for(path_type(Res::Def(alias), "Alias"), Generics::default()),
            &aliases,
        );
        assert!(matches!(class, ImplShapeClass::Nominal(did) if did == target));
    }

    #[test]
    fn impl_index_keeps_only_own_generic_parameter_as_blanket() {
        let param = DefId::new(PackageId::new("crate"), 3);
        let generics = Generics {
            params: vec![GenericParam {
                hir_id: HirId::new(OwnerId::root(PackageId::new("test")), 2),
                def_id: param.clone(),
                name: "T".into(),
                kind: GenericParamKind::Type { default: None },
                bounds: Vec::new(),
                explicit_bindings: Vec::new(),
                projection_bounds: Vec::new(),
            }],
            where_clause: None,
        };
        let class = classify_impl_shape(
            &impl_for(path_type(Res::Def(param), "T"), generics),
            &HashMap::new(),
        );
        assert!(matches!(class, ImplShapeClass::Blanket));
    }
}
