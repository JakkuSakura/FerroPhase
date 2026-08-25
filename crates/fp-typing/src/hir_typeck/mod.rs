use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::{Error, Result};
use fp_core::executor::{ExecutorHandle, TaskHandle};
use fp_core::hir;
use fp_core::hir::ty::{self, AdtDef, AdtFlags, GenericArg, ReprFlags, ReprOptions, Ty, TyKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;

use crate::context::{ComptimeRequest, ComptimeResolver, ITEM_CHECK_FAILURE_CODE};
use fp_core::hir::GenericCallResolution;
use std::rc::{Rc, Weak};

mod generic_types;
mod intrinsics;
mod literals;
mod type_shapes;
use type_shapes::*;

/// Builds the unified `program: Rc<hir::HirProgram>` a package's typecheck
/// runs against, by cloning `dependency_program` (a cheap `Rc`-map clone,
/// not a deep one — see `hir::HirProgram`'s own `packages` field) and adding
/// `current_package` (still in progress, not yet published) into it last,
/// via `HirProgram::add_package`.
pub fn build_typing_program(
    current_package: hir::HirPackage,
    dependency_program: Option<Rc<hir::HirProgram>>,
) -> Rc<hir::HirProgram> {
    let mut program = dependency_program.as_deref().cloned().unwrap_or_default();
    program.add_package(Rc::new(current_package));
    Rc::new(program)
}

/// One `HirTypeChecker` instance plays one of two roles, distinguished by
/// `root`:
///
/// - The **root** instance (`root: None`, built by `new`) holds the
///   package-level state one whole package's typecheck pass shares:
///   `program`/`current_package`/`comptime_resolver`/`executor`. Every
///   item's task (see `spawn_item_task`) is spawned against the same
///   `Rc<RefCell<HirTypeChecker>>` root handle, so writes made through
///   `root.borrow().package()` (routing to the one `Rc<HirPackage>` entry
///   inside `program`) are visible to every other item's checker.
/// - A per-item ("child") instance (`root: Some(weak)`, built by `for_item`,
///   or by one of the `with_*` helpers below while recursing into a nested
///   scope) carries its own copy of `program`/`current_package`/
///   `comptime_resolver`/`executor` (cheap clones) plus this item's own
///   recursion state (`locals`/`generic_scope`/`self_type`/`assoc_types`/
///   `expected_expr_type`). `root` is only ever consulted to recover the
///   shared `Rc<RefCell<HirTypeChecker>>` handle itself (needed to spawn
///   *further* item/comptime tasks against the same root — see
///   `root_handle`), never for the recursion-state fields.
///
/// Nested scopes (entering a block, a generics scope, an impl candidate
/// being tried, an expected-type hint) are represented by constructing a
/// new, independent child `HirTypeChecker` (see the `with_*` methods) with
/// just the relevant field overridden, and recursing using that child —
/// not by pushing onto a shared stack. Once the child is dropped, the
/// parent (never mutated) is exactly what it was before; there is no
/// explicit "pop" anywhere.
///
/// `root` is a `Weak`, not a strong `Rc`, back-reference: the root already
/// outlives every item instance spawned against it (kept alive by whoever
/// is driving the whole package's typecheck pass — see `spawn_item_task`'s
/// own `checker: &Rc<RefCell<Self>>` parameter), so a child instance
/// holding a strong reference back to its own root would just be redundant
/// refcounting, not a real ownership need.
#[derive(Clone)]
pub struct HirTypeChecker {
    /// The whole compiled workspace's HIR, as of when this package's type
    /// checking started: every already-published dependency package, plus
    /// this package's own (still in-progress, not yet published) HIR,
    /// inserted under its own `PackageId` (see `build_typing_program`). Both
    /// `HirId` and `DefId` already carry their owning `PackageId` (see
    /// `hir::DefId`'s own doc comment), so any lookup by id — same-package
    /// or cross-package alike — routes through this one `HirProgram` via
    /// `HirProgram::item`/`def_path`/etc.
    program: Rc<hir::HirProgram>,
    /// Which entry in `program` is the package actually being checked —
    /// needed for iterating just this package's own items (the initial
    /// per-item spawn loop) and for snapshotting the package's own,
    /// not-yet-published HIR into a `ComptimeRequest.current`.
    current_package: hir::PackageId,
    /// Answers requests made by HIR while checking compile-time constants —
    /// see `ComptimeResolver`'s doc comment. `None` when no resolver was
    /// supplied at construction; calling `request_comptime` in that case is
    /// a caller bug (there is no compile-time value to hand back).
    comptime_resolver: Option<ComptimeResolver>,
    executor: ExecutorHandle,
    /// See the struct's own doc comment.
    root: Option<Weak<RefCell<HirTypeChecker>>>,
    /// Every binding currently visible, innermost-shadows-outermost. A
    /// nested block's child starts as a clone of the parent's map
    /// (identical visibility) and inserts its own bindings into that clone
    /// — equivalent to pushing a new scope frame, since `HashMap::insert`
    /// already shadows by name, and the clone means those insertions never
    /// reach the parent.
    locals: HashMap<hir::Symbol, Ty>,
    /// Every generic param currently in scope (impl generics plus any
    /// enclosing method/fn generics, merged). `DefId`s are globally
    /// unique, so a flat merged map is equivalent to searching a stack of
    /// nested generic scopes.
    generic_scope: HashMap<hir::DefId, Ty>,
    /// Each in-scope generic parameter's own trait bounds, keyed by name
    /// (not `DefId` — `path_ty`'s `T::AssocName` fallback only ever has
    /// `T`'s bare name from the unresolved path's own first segment, never
    /// a resolved `DefId`, since the path as a whole failed to resolve in
    /// the first place). Merged the same way `generic_scope` is: a child
    /// entering a new generics scope (`with_generics`) clones this map and
    /// inserts its own parameters' bounds into the clone. Lets a still-
    /// generic `F::Output`/`I::Item`-style projection resolve from the
    /// bound that actually declares it (`F: FnOnce() -> R`, `I: Iterator
    /// <Item = T>`) instead of only ever resolving `T::AssocName` once `T`
    /// is a concrete type.
    generic_param_bounds: HashMap<hir::Symbol, Vec<hir::TypeExpr>>,
    /// Each in-scope generic parameter's own explicit associated-type
    /// bindings (`I: Iterator<Item = U>` binds `Item` to `U`), keyed and
    /// merged identically to `generic_param_bounds` — see
    /// `hir::GenericParam::explicit_bindings`'s own doc comment.
    generic_param_bindings: HashMap<hir::Symbol, Vec<(hir::Symbol, hir::TypeExpr)>>,
    /// `Self`'s type for the impl candidate currently being resolved
    /// against, if any. A child overrides this to try one candidate; once
    /// it's dropped, the parent's own value (never touched) is live again.
    self_type: Option<Ty>,
    /// The current impl candidate's own `type Target = Y;` bindings,
    /// paired with `self_type` the same way — lets `path_ty` resolve
    /// `Self::Target` for code lexically inside that impl. Deliberately
    /// scoped to the impl's own bindings only (no trait-default fallback,
    /// no cross-impl projection resolution for code outside the impl) —
    /// see `impl_assoc_types`.
    assoc_types: Option<HashMap<hir::Symbol, Ty>>,
    /// The ambient expected-type hint for the expression currently being
    /// checked, if any.
    expected_expr_type: Option<Ty>,
    /// Cycle guard for `assoc_type_for_self`'s impl search: checking a
    /// candidate impl's self-type/associated-type bindings can itself
    /// trigger another `T::AssocName` projection lookup (e.g. two
    /// primitive-int impls whose `Output`s reference each other through a
    /// shared helper type) — without this, that recurses through
    /// `assoc_type_for_self` indefinitely and overflows the stack instead
    /// of failing the (rare, genuinely cyclic) lookup gracefully. Plain
    /// state (not a scoped stack like `self_type`/`assoc_types`), so it
    /// carries over unchanged through every `with_*` child the same way
    /// `#[derive(Clone)]` already carries every other field.
    resolving_assoc_projections: Vec<(String, hir::Symbol)>,
    /// Fully-qualified path of the item currently being checked, if
    /// known — purely diagnostic (see `typecheck_item`'s own doc comment
    /// on why: an otherwise file/line-less error like `path_ty`'s
    /// "unresolved type path" needs *some* lead back to its real source,
    /// or it's nearly untraceable once the same message recurs across a
    /// large real corpus).
    current_item_path: Option<String>,
}

impl HirTypeChecker {
    /// Builds the unified `program: Rc<hir::HirProgram>` (via
    /// `build_typing_program`) and wraps the whole package-level state in
    /// one `Rc<RefCell<_>>` root handle, spawned against by every item's
    /// task (see `spawn_item_task`).
    pub fn new(
        current_package: hir::HirPackage,
        dependency_program: Option<Rc<hir::HirProgram>>,
        comptime_resolver: Option<ComptimeResolver>,
        executor: ExecutorHandle,
    ) -> Rc<RefCell<Self>> {
        let package_id = current_package.id.clone();
        let program = build_typing_program(current_package, dependency_program);
        Rc::new(RefCell::new(Self {
            program,
            current_package: package_id,
            comptime_resolver,
            executor,
            root: None,
            locals: HashMap::new(),
            generic_scope: HashMap::new(),
            generic_param_bounds: HashMap::new(),
            generic_param_bindings: HashMap::new(),
            self_type: None,
            assoc_types: None,
            expected_expr_type: None,
            resolving_assoc_projections: Vec::new(),
            current_item_path: None,
        }))
    }

    /// Fresh, item-local recursion state for checking exactly one item,
    /// cloned off `root`'s package-level state — see the struct's own doc
    /// comment for why `program`/`current_package`/`comptime_resolver`/
    /// `executor` are copied in directly rather than reached through
    /// `root` on every access.
    fn for_item(root: &Rc<RefCell<Self>>) -> Self {
        let shared = root.borrow();
        Self {
            program: shared.program.clone(),
            current_package: shared.current_package.clone(),
            comptime_resolver: shared.comptime_resolver.clone(),
            executor: shared.executor.clone(),
            root: Some(Rc::downgrade(root)),
            locals: HashMap::new(),
            generic_scope: HashMap::new(),
            generic_param_bounds: HashMap::new(),
            generic_param_bindings: HashMap::new(),
            self_type: None,
            assoc_types: None,
            expected_expr_type: None,
            resolving_assoc_projections: Vec::new(),
            current_item_path: None,
        }
    }

    /// Recovers the shared `Rc<RefCell<HirTypeChecker>>` root handle from a
    /// child instance's weak back-reference — needed wherever item-level
    /// code spawns *further* item/comptime tasks against the same root
    /// (`spawn_item_task`/`spawn_comptime_task` both take that handle, not
    /// `&self`). Panics if called on the root instance itself (`root` is
    /// `None` there — the root already *is* what callers elsewhere hold
    /// their own `Rc<RefCell<_>>` to) or if the root was somehow dropped
    /// while an item check spawned against it was still running (should
    /// never happen: the root is kept alive by whoever is driving the
    /// whole package's typecheck pass for at least as long as any item
    /// task spawned against it).
    fn root_handle(&self) -> Rc<RefCell<Self>> {
        self.root
            .as_ref()
            .expect("root_handle called on the root HirTypeChecker instance itself")
            .upgrade()
            .expect("root HirTypeChecker dropped while an item check was still using it")
    }

    /// A child for entering a new block-local scope — see `locals`'s own
    /// doc comment: the child's own later insertions shadow the parent's
    /// (cloned-in) bindings without ever reaching back into the parent.
    fn with_fresh_block_scope(&self) -> Self {
        self.clone()
    }

    /// A child with `generics.params` merged into a clone of the current
    /// `generic_scope` — the same set `push_generics` used to push as a
    /// new stack frame, just materialized as a new `Self` instead.
    fn with_generics(&self, generics: &hir::Generics) -> Self {
        let mut child = self.clone();
        for (index, parameter) in generics.params.iter().enumerate() {
            if matches!(parameter.kind, hir::GenericParamKind::Type { .. }) {
                child.generic_scope.insert(
                    parameter.def_id.clone(),
                    Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: index as u32,
                            name: parameter.name.clone(),
                        }),
                    },
                );
                if !parameter.bounds.is_empty() {
                    child
                        .generic_param_bounds
                        .insert(parameter.name.clone(), parameter.bounds.clone());
                }
                if !parameter.explicit_bindings.is_empty() {
                    child
                        .generic_param_bindings
                        .insert(parameter.name.clone(), parameter.explicit_bindings.clone());
                }
            }
        }
        child
    }

    /// A child trying `ty` as `Self` for one impl candidate.
    fn with_self_type(&self, ty: Ty) -> Self {
        let mut child = self.clone();
        child.self_type = Some(ty);
        child
    }

    /// A child with `assoc` as the current impl candidate's own associated
    /// types — see `assoc_types`'s own doc comment.
    fn with_assoc_types(&self, assoc: HashMap<hir::Symbol, Ty>) -> Self {
        let mut child = self.clone();
        child.assoc_types = Some(assoc);
        child
    }

    /// A child checking one expression under an ambient expected-type hint.
    fn with_expected_expr_type(&self, ty: Ty) -> Self {
        let mut child = self.clone();
        child.expected_expr_type = Some(ty);
        child
    }

    fn generic_ty(&self, def_id: hir::DefId) -> Option<Ty> {
        self.generic_scope.get(&def_id).cloned()
    }

    /// The package actually being checked — an `Rc` clone (a pointer bump,
    /// never a deep copy of the package's own data; `HirProgram::package`
    /// itself only hands out a plain `&HirPackage`, not the `Rc` needed
    /// here, so this reads the underlying `packages` map directly).
    pub fn package(&self) -> Rc<hir::HirPackage> {
        self.program
            .packages
            .get(&self.current_package)
            .cloned()
            .expect("current_package is always inserted into program at construction")
    }

    /// The whole workspace `HirProgram`, for cross-package lookups that
    /// `package()`'s single-package view can't answer.
    fn program_rc(&self) -> Rc<hir::HirProgram> {
        self.program.clone()
    }

    /// The whole workspace `HirProgram` this package is being checked
    /// against (every already-published dependency, plus this package's
    /// own still-in-progress HIR) — handed to `CompilerState` for the
    /// duration of this package's typecheck so a mid-typecheck
    /// `ComptimeRequest` can resolve its own package by id.
    pub fn program_handle(&self) -> Rc<hir::HirProgram> {
        self.program_rc()
    }

    /// The `PackageId` `ComptimeRequest`s built while checking this package
    /// name themselves under — the driver's comptime resolver looks the
    /// still-in-progress package back up by this id (see
    /// `CompilerState::in_progress_hir_program`), since the request itself
    /// no longer carries the package's own `Rc` directly.
    fn current_package(&self) -> hir::PackageId {
        self.current_package.clone()
    }

    /// See `current_package`; kept as a distinctly-named `pub` accessor
    /// since external crates (`fp-compiler`) shouldn't need to know that
    /// the field itself is also called `current_package`.
    pub fn current_package_id(&self) -> hir::PackageId {
        self.current_package.clone()
    }

    /// Request a compile-time value — awaits `ComptimeResolver` directly, so
    /// the caller (an item's typecheck task) just suspends naturally until
    /// the answer is ready, with no manual queue-draining/polling by
    /// driver-level code required.
    async fn request_comptime(
        &self,
        request: ComptimeRequest,
    ) -> fp_core::Result<fp_core::ast::Value> {
        let resolver = self
            .comptime_resolver
            .clone()
            .expect("HirTypeChecker::request_comptime called without a comptime_resolver");
        let def_id = request.def_id.clone();
        let value = resolver(request).await?;
        // Recorded here, once, for every caller — not each call site's own
        // responsibility (see `spawn_comptime_task`, which used to do this
        // itself right after awaiting this same method).
        self.program.record_const_block_value(def_id, value.clone());
        Ok(value)
    }

    /// `tcx.sess.has_errors()`-style query: true once any item's check has
    /// hard-aborted (tagged `ITEM_CHECK_FAILURE_CODE`, see `HirPackage::
    /// diagnostics`'s doc comment) — the only category that leaves a real
    /// typed-results gap, and thus the only category safe to gate later
    /// stages on.
    pub fn has_typing_errors(&self) -> bool {
        self.package()
            .diagnostics
            .get_diagnostics()
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some(ITEM_CHECK_FAILURE_CODE))
    }

    /// Records a typing diagnostic instead of hard-aborting the whole
    /// package's typecheck over one error — mirrors `ast_to_hir`'s
    /// `error_placeholder_expr_kind` precedent (`fp-backend/src/
    /// transforms/ast_to_hir/exprs.rs`). `self.package().diagnostics`
    /// is the real sink; `typecheck_package` inspects it after the whole
    /// pass finishes to decide overall pass/fail, so this doesn't silently
    /// let a genuinely broken package look fully typed.
    fn record_error(&self, message: impl Into<String>) {
        self.package()
            .diagnostics
            .add_diagnostic(crate::types::typing_diagnostic(message, None));
    }

    /// `record_error` plus a `Ty::error()` placeholder, for `Result<Ty>`
    /// call sites that need *some* type to keep going with.
    fn error_ty(&self, message: impl Into<String>) -> Ty {
        self.record_error(message);
        Ty::error()
    }

    /// `record_error`, but with a real span attached (`TypingDiagnostic::
    /// error_with_span`) — use whenever the caller already has the
    /// offending expression's span in scope, so the diagnostic is
    /// locatable instead of a bare, file/line-less message.
    fn record_error_with_span(&self, message: impl Into<String>, span: fp_core::span::Span) {
        self.package()
            .diagnostics
            .add_diagnostic(crate::types::typing_diagnostic(message, Some(span)));
    }

    /// `error_ty`, but with a real span attached — see `record_error_with_span`.
    fn error_ty_with_span(&self, message: impl Into<String>, span: fp_core::span::Span) -> Ty {
        self.record_error_with_span(message, span);
        Ty::error()
    }

    /// Like `record_error`, but specifically for `typecheck_item`'s own
    /// catch — an item's `check_item` returned a hard `Err` and its check
    /// aborted outright, leaving a real gap in the package's typed results
    /// for whatever that item didn't finish recording (unlike the
    /// pervasive, deliberately non-fatal `record_error`/`error_ty` calls
    /// sprinkled through `check_expr`/`check_block`, e.g. `require_same`'s
    /// isolated mismatches, which recover and leave no gap). Tagged with
    /// `ITEM_CHECK_FAILURE_CODE` so `has_typing_errors` can gate later
    /// stages on real gaps without also tripping on every recovered,
    /// harmless mismatch also sitting in the same manager.
    fn record_item_check_failure(&self, message: impl Into<String>) {
        let diagnostic = crate::types::typing_diagnostic(message, None)
            .with_code(crate::context::ITEM_CHECK_FAILURE_CODE);
        self.package().diagnostics.add_diagnostic(diagnostic);
    }
}

impl HirTypeChecker {
    /// Read the final `hir::HirPackage` out — only meaningful once every
    /// per-item task (see `spawn_item_task`) has settled (i.e. its returned
    /// future resolved). Every typed result (expr/pat types, resolutions,
    /// diagnostics, ...) is already embedded directly on this same package
    /// (every item instance writes straight through to it — see
    /// `HirPackage`'s own typed-results fields), so the `Rc` handle itself
    /// (an `Rc` clone, not a deep copy — see `package`) *is* the whole
    /// snapshot; there's no separate side table to read back alongside it,
    /// and no need to ever copy the package's own data out of it.
    pub fn finish(&self) -> Rc<hir::HirPackage> {
        self.package()
    }

    /// Entrypoint for type-checking a single item by `DefId`, directly —
    /// unlike `spawn_item_task`, this does *not* go through
    /// `ExecutorHandle::get_or_spawn`'s dedup-by-key task registry; it just
    /// builds a per-item instance (`for_item`) and runs `check_item`
    /// inline, for callers driving a single item's check themselves rather
    /// than through the shared per-package task pool (e.g. tooling that
    /// wants one item's diagnostics/`Result` directly, without another
    /// concurrent awaiter of the same `def_id` silently sharing this run).
    /// `def_id` must already name a top-level item — `program.def_map`
    /// only has entries for those (an `impl`'s own methods/assoc-consts
    /// aren't separate `def_map` keys; see `expr_path_ty`'s manual
    /// `member_owner` scan for that case), so a member `DefId` here just
    /// misses and no-ops, same as `spawn_item_task`.
    pub async fn typecheck_item(checker: &Rc<RefCell<Self>>, def_id: hir::DefId) -> Result<()> {
        let Some(item) = checker.borrow().package().def_map.get(&def_id).cloned() else {
            return Ok(());
        };
        let mut item_checker = Self::for_item(checker);
        item_checker.check_item(&item).await
    }

    /// Get-or-spawn the task that type-checks `def_id`, keyed so any number
    /// of dependents (another item's task, or the initial per-package
    /// spawn loop) share the same in-flight/completed attempt instead of
    /// re-checking it. Errors from checking the item itself are recorded
    /// (via `record_item_check_failure`, against this item specifically)
    /// rather than propagated — one item's failure never stops any other
    /// item's task from completing, which is what "a package almost never
    /// fails as a whole" means in practice. `def_id` must already name a
    /// top-level item — see `typecheck_item`'s doc comment.
    pub fn spawn_item_task(checker: &Rc<RefCell<Self>>, def_id: hir::DefId) -> TaskHandle<()> {
        let key = format!("typecheck:{def_id:?}");
        let checker = checker.clone();
        let executor = checker.borrow().executor.clone();
        executor.get_or_spawn(key, move || {
            Box::pin(async move {
                let Some(item) = checker.borrow().package().def_map.get(&def_id).cloned() else {
                    return;
                };
                let mut item_checker = Self::for_item(&checker);
                if let Err(error) = item_checker.check_item(&item).await {
                    item_checker.record_item_check_failure(format!("{error}"));
                }
            }) as Pin<Box<dyn Future<Output = ()>>>
        })
    }

    /// Resolves one comptime unit (a const block, keyed by its own `DefId`
    /// — see `hir::ExprConstBlock::def_id`) by spawning (or reusing, via
    /// `get_or_spawn`) a task on the shared executor — mirrors
    /// `spawn_item_task`'s dedup pattern, so two typer tasks reaching the
    /// same const-block concurrently share one interpretation run instead
    /// of each independently awaiting `request_comptime`. The resolved
    /// value is recorded into the package's own `const_block_values` by
    /// `request_comptime` itself, not here — every awaiter and every later
    /// `DefId` lookup against the package's own table always agree.
    pub fn spawn_comptime_task(
        checker: &Rc<RefCell<Self>>,
        def_id: hir::DefId,
        request: crate::ComptimeRequest,
    ) -> TaskHandle<Option<hir::Value>> {
        let cache_key = format!("comptime:{def_id:?}");
        let checker = checker.clone();
        let executor = checker.borrow().executor.clone();
        executor.get_or_spawn(cache_key, move || {
            Box::pin(async move { checker.borrow().request_comptime(request).await.ok() })
                as Pin<Box<dyn Future<Output = Option<hir::Value>>>>
        })
    }
}

impl HirTypeChecker {
    /// Type-checks and collects an impl block's own `type Target = Y;`
    /// associated-type bindings, ready to combine with `self_type` — see
    /// `with_self_type`/`with_assoc_types` and that field's own doc
    /// comment for the deliberate scope
    /// limit (impl's own bindings only, no trait-default consultation, no
    /// cross-impl resolution).
    async fn impl_assoc_types(
        &mut self,
        impl_items: &[hir::ImplItem],
        cache_key: hir::HirId,
    ) -> Result<HashMap<hir::Symbol, Ty>> {
        if let Some(cached) = self.package().impl_assoc_types(cache_key.clone()) {
            return Ok(cached);
        }
        let mut out = HashMap::new();
        for item in impl_items {
            if let hir::ImplItemKind::AssocType(assoc) = &item.kind {
                let ty = self.check_type_expr(&assoc.ty).await?;
                out.insert(assoc.name.clone(), ty);
            }
        }
        self.package()
            .cache_impl_assoc_types(cache_key, out.clone());
        Ok(out)
    }

    fn check_item<'a>(&'a mut self, item: &'a hir::Item) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            match &item.kind {
                hir::ItemKind::Function(function) => {
                    self.check_function(function).await?;
                }
                hir::ItemKind::Const(constant) => {
                    let declared_ty = self.check_type_expr(&constant.ty).await?;
                    let mut scope = self.with_expected_expr_type(declared_ty.clone());
                    let body_ty = scope.check_body(&constant.body).await?;
                    let package = self.package();
                    package.record_type_expr_type(constant.ty.hir_id.clone(), body_ty.clone());
                    package.record_const_type(item.def_id.clone(), body_ty);
                }
                hir::ItemKind::Impl(impl_item) => {
                    let mut scope = self.with_generics(&impl_item.generics);
                    let self_ty = scope.checked_impl_self_ty(&impl_item.self_ty).await?;
                    let mut scope = scope.with_self_type(self_ty);
                    // `impl_item.trait_ty`, when present, names the trait
                    // being implemented — a reference to a trait
                    // definition, not a value type. `path_ty`/
                    // `check_type_expr` only knows how to build a concrete
                    // type from a struct/enum def_id (there is no
                    // `hir::ItemKind::Trait`; `DefTrait` items lower to a
                    // placeholder `hir::ItemKind::Const`, see
                    // `ast_to_hir/mod.rs`'s `ItemKind::DefTrait` arm), so
                    // running it through the same ADT-only type-checking
                    // path as `self_ty` above always fails with
                    // "definition `{def_id}` is not a type" — this
                    // position was never meant to type-check as a
                    // concrete type in the first place.
                    let assoc_types = scope
                        .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                        .await?;
                    let mut scope = scope.with_assoc_types(assoc_types);
                    for item in &impl_item.items {
                        match &item.kind {
                            hir::ImplItemKind::Method(function) => {
                                scope.check_function(function).await?
                            }
                            hir::ImplItemKind::AssocConst(constant) => {
                                scope.check_type_expr(&constant.ty).await?;
                                scope.check_body(&constant.body).await?;
                            }
                            hir::ImplItemKind::AssocType(_) => {
                                // Already type-checked into `assoc_types` above.
                            }
                        }
                    }
                }
                hir::ItemKind::Struct(def) => {
                    let mut scope = self.with_generics(&def.generics);
                    for field in &def.fields {
                        scope.check_type_expr(&field.ty).await?;
                    }
                }
                hir::ItemKind::Enum(def) => {
                    let mut scope = self.with_generics(&def.generics);
                    for variant in &def.variants {
                        if let Some(payload) = &variant.payload {
                            scope.check_type_expr(payload).await?;
                        }
                        if let Some(discriminant) = &variant.discriminant {
                            scope.check_expr(discriminant).await?;
                        }
                    }
                }
                hir::ItemKind::Trait(_) => {
                    // Trait definitions exist here purely as a fallback
                    // signature/default-body source for `method_output`'s
                    // trait-default-method resolution (see there) — the
                    // vendored `core`/`alloc`/`std` source this almost
                    // always originates from is already known-correct real
                    // rustc code, and re-verifying a default method's body
                    // here would require checking it against an abstract,
                    // uninstantiated `Self`/`Self::AssocType`, which this
                    // scope-based checker has no general mechanism for.
                    // Concrete `impl Trait for X` blocks are still fully
                    // checked as normal, above.
                }
                hir::ItemKind::Query(_) => {}
                hir::ItemKind::Expr(expr) => {
                    self.check_expr(expr).await?;
                }
            }
            Ok(())
        })
    }

    fn check_function<'a>(
        &'a mut self,
        function: &'a hir::Function,
    ) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            let mut scope = self.with_generics(&function.sig.generics);
            scope
                .check_signature(&function.sig)
                .await
                .map_err(|error| {
                    Error::from(format!(
                        "in function `{}` signature: {error}",
                        function.sig.name
                    ))
                })?;
            if let Some(body) = &function.body {
                scope
                    .check_function_body(&function.sig.inputs, &function.sig.output, body)
                    .await
                    .map_err(|error| {
                        Error::from(format!("in function `{}` body: {error}", function.sig.name))
                    })?;
            }
            Ok(())
        })
    }

    /// The trait-bound list for a still-generic type parameter named
    /// `name`, if one is currently in scope — see `generic_param_bounds`'s
    /// doc comment for why this is name-keyed rather than `DefId`-keyed.
    fn generic_param_bounds(&self, name: &hir::Symbol) -> Option<&[hir::TypeExpr]> {
        self.generic_param_bounds.get(name).map(Vec::as_slice)
    }

    /// The explicit associated-type bindings for a still-generic type
    /// parameter named `name`, if any are currently in scope — see
    /// `generic_param_bindings`'s own doc comment.
    fn generic_param_bindings(
        &self,
        name: &hir::Symbol,
    ) -> Option<&[(hir::Symbol, hir::TypeExpr)]> {
        self.generic_param_bindings.get(name).map(Vec::as_slice)
    }

    async fn check_signature(&mut self, signature: &hir::FunctionSig) -> Result<()> {
        for input in &signature.inputs {
            self.check_type_expr(&input.ty).await?;
        }
        self.check_type_expr(&signature.output).await?;
        Ok(())
    }

    async fn check_body(&mut self, body: &hir::Body) -> Result<Ty> {
        let mut scope = self.with_fresh_block_scope();
        for param in &body.params {
            let ty = scope.check_type_expr(&param.ty).await?;
            scope.bind_pattern(&param.pat, ty).await?;
        }
        scope.check_expr(&body.value).await
    }

    async fn check_function_body(
        &mut self,
        params: &[hir::Param],
        output: &hir::TypeExpr,
        block: &hir::Block,
    ) -> Result<()> {
        let mut scope = self.with_fresh_block_scope();
        for param in params {
            let ty = scope.check_type_expr(&param.ty).await?;
            scope.bind_pattern(&param.pat, ty).await?;
        }
        // Same expected-type hint `ConstBlock`/`Assign` already provide:
        // without it, a trailing zero-arg generic call like
        // `Option::none()` (no argument to infer `T` from) can't resolve
        // its type parameter even though the function's own declared
        // return type unambiguously determines it. Scoped to just the
        // block's own trailing expression (see
        // `check_block_with_expected_tail`) — not the whole body — so it
        // doesn't leak into unrelated statements earlier in the function.
        let output_ty = scope.check_type_expr(output).await?;
        let refinement_hint = scope
            .program_rc()
            .take_raw_refinement_hint(output.hir_id.clone());
        scope
            .check_block_with_expected_tail(block, Some(output_ty))
            .await?;
        if let Some(hint) = &refinement_hint {
            if let Some(tail) = block.expr.as_ref() {
                scope.discharge_refinement(hint, tail)?;
            }
        }
        Ok(())
    }

    fn check_expr<'a>(&'a mut self, expr: &'a hir::Expr) -> crate::BoxFuture<'a, Result<Ty>> {
        Box::pin(async move {
            let ty = match &expr.kind {
                hir::ExprKind::Literal(lit) => {
                    // An untyped integer/float literal takes its expected
                    // type when the ambient hint names a concrete numeric
                    // type — the same override the `Call` arm's own
                    // argument-checking loop already applies for a call
                    // argument specifically, generalized to every other
                    // position a literal can appear in (assignments,
                    // binary operands, array/struct/tuple elements, ...).
                    // `literal_ty`'s own fixed `i64`/`f64` default is only
                    // ever right by coincidence outside of call arguments;
                    // real Rust infers an untyped integer/float literal's
                    // type from its surrounding context exactly this way.
                    let default = self.literal_ty(lit);
                    match (lit, self.expected_expr_type.as_ref()) {
                        (
                            hir::Lit::Integer(_),
                            Some(Ty {
                                kind: kind @ (TyKind::Int(_) | TyKind::Uint(_)),
                            }),
                        ) => Ty { kind: kind.clone() },
                        (
                            hir::Lit::Float(_),
                            Some(Ty {
                                kind: kind @ TyKind::Float(_),
                            }),
                        ) => Ty { kind: kind.clone() },
                        _ => default,
                    }
                }
                hir::ExprKind::Path(path) => self.expr_path_ty(path).await?,
                hir::ExprKind::Binary(op, lhs, rhs) => {
                    let lhs_literal =
                        matches!(lhs.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                    let rhs_literal =
                        matches!(rhs.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                    let lhs = self.check_expr(lhs).await?;
                    let rhs = self.check_expr(rhs).await?;
                    let integer_literal = (lhs_literal
                        && matches!(rhs.kind, TyKind::Int(_) | TyKind::Uint(_)))
                        || (rhs_literal && matches!(lhs.kind, TyKind::Int(_) | TyKind::Uint(_)));
                    if !integer_literal {
                        match op {
                            hir::BinOp::And | hir::BinOp::Or => {
                                self.require_same_at(&lhs, &Ty::bool(), expr.span)?;
                                self.require_same_at(&rhs, &Ty::bool(), expr.span)?;
                            }
                            hir::BinOp::Eq
                            | hir::BinOp::Ne
                            | hir::BinOp::Lt
                            | hir::BinOp::Le
                            | hir::BinOp::Gt
                            | hir::BinOp::Ge => {
                                // `unify_call_types`, not `require_same`: a
                                // comparison's two sides should coerce the
                                // same way a call argument would (e.g. a
                                // `&str` value compared against a bare `str`
                                // literal like `value == ""`).
                                let mut substitutions = HashMap::new();
                                self.unify_call_types(&lhs, &rhs, &mut substitutions)?;
                            }
                            _ => {
                                let mut substitutions = HashMap::new();
                                self.unify_call_types(&lhs, &rhs, &mut substitutions)?;
                            }
                        }
                    }
                    match op {
                        hir::BinOp::Eq
                        | hir::BinOp::Ne
                        | hir::BinOp::Lt
                        | hir::BinOp::Le
                        | hir::BinOp::Gt
                        | hir::BinOp::Ge
                        | hir::BinOp::And
                        | hir::BinOp::Or => Ty::bool(),
                        // An untyped integer literal takes the *other*
                        // side's real type, not its own bare `i64`
                        // default — `integer_literal` above deliberately
                        // skips unification for this exact pairing (a
                        // literal already trivially "fits" any integer
                        // type, so there's nothing to unify), but the
                        // arithmetic result still needs to be the
                        // concrete side's type, matching real Rust's own
                        // literal-takes-context-type inference. Blindly
                        // returning `lhs` here previously reported the
                        // whole expression as `i64` whenever the literal
                        // happened to be on the left (`1 + some_u8`), so
                        // every caller expecting the real operand's type
                        // (`u8`, `usize`, ...) saw a spurious mismatch.
                        _ if lhs_literal && !rhs_literal => rhs,
                        _ => lhs,
                    }
                }
                hir::ExprKind::Unary(op, value) => {
                    let value_ty = self.check_expr(value).await?;
                    match op {
                        hir::UnOp::Not => {
                            self.require_same_at(&value_ty, &Ty::bool(), expr.span)?;
                            Ty::bool()
                        }
                        hir::UnOp::Deref => match value_ty.kind {
                            TyKind::Ref(_, inner, _)
                            | TyKind::RawPtr(ty::TypeAndMut { ty: inner, .. }) => *inner,
                            _ => self.error_ty("cannot dereference a non-pointer value"),
                        },
                        hir::UnOp::Neg | hir::UnOp::Box => value_ty,
                    }
                }
                hir::ExprKind::Reference(reference) => {
                    let mut referent = self.check_expr(&reference.expr).await?;
                    // Re-borrow, don't stack references: `&expr` where
                    // `expr` is already a `&T`/`&mut T` (e.g. `&self.field`
                    // when `field`'s own declared type is `&str`) produces
                    // `&T`, not `&&T` — the same collapsing a real `&`
                    // operator does when applied to an existing reference.
                    // Collapse every existing layer, not just one, in case
                    // `expr`'s own type is already multiply-referenced.
                    while let TyKind::Ref(_, inner, _) = referent.kind {
                        referent = *inner;
                    }
                    Ty {
                        kind: TyKind::Ref(
                            ty::Region::ReErased,
                            Box::new(referent),
                            reference.mutable,
                        ),
                    }
                }
                hir::ExprKind::Call(callee, args) => {
                    let mut callee_ty = self.check_expr(callee).await?;
                    // A direct call through a still-generic parameter
                    // itself (`f(x)` where `f: F` and `F: FnMut(T) -> R`,
                    // `Fn(...)`, or `FnOnce(...)`) — by far the most common
                    // call shape in generic functional/iterator code
                    // (`fold`, `map`, `filter`, `partition`'s own inner
                    // `extend` helper, ...). `check_expr` on a local bound
                    // to a generic parameter only ever returns the bare
                    // `TyKind::Param`, never expanded to the callable
                    // signature its own `Fn`-sugar bound declares — this
                    // was the single largest remaining unresolved-call
                    // bucket in the full corpus (10,000+ occurrences),
                    // dwarfing every other case fixed this session,
                    // because it fires on every generic higher-order
                    // function that calls its own closure parameter.
                    // Resolve it the same way `assoc_type_from_generic_
                    // param_bounds` already resolves `F::Output` from the
                    // identical bound — just for the callee's own
                    // signature instead of one associated type.
                    if let TyKind::Param(param) = &callee_ty.kind {
                        if let Some(fn_ptr) = self
                            .generic_param_bounds(&param.name)
                            .map(<[_]>::to_vec)
                            .and_then(|bounds| {
                                bounds.iter().find_map(|bound| match &bound.kind {
                                    hir::TypeExprKind::FnPtr(fn_ptr) => Some(fn_ptr.clone()),
                                    _ => None,
                                })
                            })
                        {
                            let mut inputs = Vec::with_capacity(fn_ptr.inputs.len());
                            for input in &fn_ptr.inputs {
                                inputs.push(Box::new(self.check_type_expr(input).await?));
                            }
                            let output = Box::new(self.check_type_expr(&fn_ptr.output).await?);
                            callee_ty = Ty {
                                kind: TyKind::FnPtr(ty::PolyFnSig {
                                    binder: ty::Binder {
                                        value: ty::FnSig {
                                            inputs,
                                            output,
                                            c_variadic: false,
                                            unsafety: ty::Unsafety::Normal,
                                            abi: ty::Abi::Rust,
                                        },
                                        bound_vars: Vec::new(),
                                    },
                                }),
                            };
                        }
                    }
                    let expected_inputs = match &callee_ty.kind {
                        TyKind::FnPtr(signature) => Some(signature.binder.value.inputs.clone()),
                        _ => None,
                    };
                    // Refinement hints for this callee's own parameters,
                    // if any were recorded when its signature was
                    // resolved (possibly by an entirely different item's
                    // check, possibly long ago — see `hir::HirPackage::
                    // refinement_hints`'s doc comment). Only resolvable
                    // for a directly-named callee with a real `DefId`;
                    // a call through a function pointer/closure value
                    // simply has no hints to discharge, same as today.
                    let callee_refinement_cache_key = if let hir::ExprKind::Path(path) =
                        &callee.kind
                    {
                        path.res.as_ref().and_then(|res| match res {
                            hir::Res::Def(def_id) => match self.program_rc().item(def_id.clone()) {
                                Some(item) => match &item.kind {
                                    hir::ItemKind::Function(function) => {
                                        Some(function.sig.output.hir_id.clone())
                                    }
                                    _ => None,
                                },
                                None => None,
                            },
                            _ => None,
                        })
                    } else {
                        None
                    };
                    let mut arg_types = Vec::with_capacity(args.len());
                    for (index, arg) in args.iter().enumerate() {
                        // Scope the expected-type hint to *this parameter's*
                        // declared type — same rationale as the struct
                        // literal field case above, e.g. a call inside a
                        // `ConstBlock`/`Assign`/function-tail context must
                        // not leak that outer hint into its own arguments.
                        let param_hint: Option<Ty> = expected_inputs
                            .as_ref()
                            .and_then(|inputs| inputs.get(index))
                            .map(|ty| (**ty).clone());
                        let actual = if let Some(hint) = &param_hint {
                            self.with_expected_expr_type(hint.clone())
                                .check_expr(&arg.value)
                                .await
                        } else {
                            self.check_expr(&arg.value).await
                        };
                        let actual = actual?;
                        let actual = match expected_inputs
                            .as_ref()
                            .and_then(|inputs| inputs.get(index))
                        {
                            Some(expected)
                                if matches!(
                                    arg.value.kind,
                                    hir::ExprKind::Literal(hir::Lit::Integer(_))
                                ) && matches!(
                                    expected.kind,
                                    TyKind::Int(_) | TyKind::Uint(_)
                                ) =>
                            {
                                // Integer literals can take the type of their direct parameter.
                                (**expected).clone()
                            }
                            _ => actual,
                        };
                        if let Some(ref cache_key) = callee_refinement_cache_key {
                            let hint = self
                                .program
                                .refinement_hint(cache_key.clone(), hir::ParamSlot::Input(index));
                            if let Some(hint) = &hint {
                                self.discharge_refinement(hint, &arg.value)?;
                            }
                        }
                        arg_types.push(actual);
                    }
                    // Trait-qualified UFCS syntax (`Add::add(lhs, rhs)`,
                    // `Ord::max(a, b)`) — generated en masse by std's
                    // `forward_ref_binop!`/`forward_ref_unop!` macros
                    // (`internal_macros.rs`) and used directly by default
                    // trait methods like `Ord::max`/`min`. `expr_path_ty`
                    // already resolves the base segment to the trait's own
                    // `DefId` (see `ast_to_hir`'s type-relative path
                    // resolution), but which *impl* of that trait applies
                    // depends on the call's own arguments, which aren't
                    // known until here — the first call argument (`lhs`/
                    // `self`) is always the receiver for this call shape,
                    // so resolve the method the same way `.method()` call
                    // syntax already does, keyed off its concrete type.
                    if matches!(callee_ty.kind, TyKind::Error(_)) {
                        if let hir::ExprKind::Path(path) = &callee.kind {
                            // `path.segments.len() >= 2`, not `== 2`: the
                            // trait name may itself be qualified
                            // (`core::fmt::Display::fmt`/`fmt::Display::
                            // fmt`, not just bare `Display::fmt`) —
                            // `expr_path_ty`'s own recovery for this
                            // shape already resolves `path.res`/this
                            // deferred `Ty::Error` off the *second-to-
                            // last* segment regardless of how many
                            // segments precede it, so this call-site
                            // re-dispatch needs to key off the *last*
                            // segment as the method name the same way,
                            // not assume the trait is always exactly
                            // `segments[0]`.
                            if path.segments.len() >= 2 {
                                if let Some(receiver_ty) = arg_types.first().cloned() {
                                    let method_name = path.segments.last().unwrap().name.clone();
                                    // The receiver may itself still be a
                                    // generic type parameter (`fn max<T:
                                    // Ord>(a: T, b: T) -> T { Ord::max(a,
                                    // b) }` — the real body of `Ord::max`'s
                                    // own default implementation elsewhere
                                    // in std) — there is no concrete impl
                                    // to search in that case, so resolve
                                    // against the parameter's own bounds
                                    // instead, exactly like `T::method(..)`
                                    // does when the base segment itself
                                    // names the parameter.
                                    let sig = if let TyKind::Param(param) = &receiver_ty.kind {
                                        self.generic_param_bound_method_signature(
                                            &param.name,
                                            &method_name,
                                        )
                                        .await?
                                    } else {
                                        self.method_declared_signature_at(
                                            &receiver_ty,
                                            &method_name,
                                        )
                                        .await?
                                    };
                                    if let Some(sig) = sig {
                                        tracing::debug!(
                                            method = %method_name,
                                            ?receiver_ty,
                                            "trait-qualified UFCS call resolved"
                                        );
                                        callee_ty = sig;
                                    }
                                }
                            }
                        }
                    }
                    // Trait-qualified UFCS calls whose receiver is the
                    // trait's own *output* type, not any argument
                    // (`Default::default()`, `FromIterator::from_iter
                    // (iter)`) — the same call shape as above, but the
                    // real `Self` comes from the ambient expected type at
                    // the call site (`let x: Foo = Default::default();`),
                    // exactly like a bare struct/enum literal's own field
                    // types resolve from the same `expected_expr_types`
                    // stack elsewhere in this function. Only tried once
                    // the argument-typed attempt above has already come up
                    // empty, since a real receiver argument always takes
                    // priority when one exists.
                    if matches!(callee_ty.kind, TyKind::Error(_)) {
                        if let hir::ExprKind::Path(path) = &callee.kind {
                            if path.segments.len() >= 2 {
                                // The ambient expected type is itself only
                                // a real receiver to search with when it's
                                // an actual type — if it's `TyKind::Error`
                                // (the typechecker's own "already failed
                                // here" placeholder, not any real Rust
                                // type), there is nothing to search, so
                                // don't hand it to a type-directed lookup
                                // as if it named something.
                                let expected = self
                                    .expected_expr_type
                                    .clone()
                                    .filter(|ty| !matches!(ty.kind, TyKind::Error(_)));
                                if let Some(receiver_ty) = expected {
                                    let method_name = path.segments.last().unwrap().name.clone();
                                    let sig = if let TyKind::Param(param) = &receiver_ty.kind {
                                        self.generic_param_bound_method_signature(
                                            &param.name,
                                            &method_name,
                                        )
                                        .await?
                                    } else {
                                        self.method_declared_signature_at(
                                            &receiver_ty,
                                            &method_name,
                                        )
                                        .await?
                                    };
                                    if let Some(sig) = sig {
                                        tracing::debug!(
                                            method = %method_name,
                                            ?receiver_ty,
                                            "expected-type-qualified UFCS call resolved"
                                        );
                                        callee_ty = sig;
                                    }
                                }
                            }
                        }
                    }
                    // A tuple struct's own name (or `Self`, once resolved
                    // to that struct's `Adt` type) called like a function
                    // is real Rust's tuple-struct constructor call
                    // (`Wrapper(x)`, `Self(value)` from within its own
                    // inherent impl — real vendored std's own idiom
                    // throughout for newtype wrappers). `instantiate_call`
                    // only knows genuine callable (`FnPtr`) types, so this
                    // always reached the generic "not a function" error
                    // below otherwise. The constructor's result is simply
                    // the struct type itself; real field-arity/type
                    // checking against the struct's own declared fields
                    // isn't attempted here (no field list is in scope at
                    // this call site) — same "tolerate what's broken,
                    // keep what isn't" policy as elsewhere, rather than
                    // erroring the whole enclosing expression over it.
                    if let TyKind::Adt(adt, _) = &callee_ty.kind {
                        if adt.flags.contains(AdtFlags::IS_STRUCT) {
                            return Ok(callee_ty);
                        }
                    }
                    let Some((mut substitutions, _)) =
                        self.instantiate_call(&callee_ty, &arg_types)?
                    else {
                        tracing::debug!(
                            callee_kind = ?callee_ty.kind,
                            callee_expr = ?callee.kind,
                            "call target did not resolve to a function type"
                        );
                        return Ok(self.error_ty(format!(
                            "called expression is not a function (callee type: {:?}, callee expr: {:?})",
                            callee_ty.kind, callee.kind
                        )));
                    };
                    if substitutions.is_empty() {
                        if let Some(expected) = self.expected_expr_type.as_ref() {
                            if let TyKind::FnPtr(signature) = &callee_ty.kind {
                                // Only worth consulting the ambient
                                // expected-type hint when the callee is
                                // actually still generic here (a zero-arg
                                // constructor like `Vec::new()`, which has
                                // no argument to infer `T` from) — a fully
                                // concrete, non-generic callee's result type
                                // is already fully determined by its own
                                // signature and must never be reconciled
                                // against an outer hint that may belong to
                                // an unrelated enclosing expression (e.g.
                                // this call is just the receiver of a
                                // further method call, not the tail
                                // position the hint was pushed for).
                                if ty_contains_param(&signature.binder.value.output) {
                                    self.unify_call_types(
                                        &signature.binder.value.output,
                                        expected,
                                        &mut substitutions,
                                    )?;
                                }
                            }
                        }
                    }
                    let output = match &callee_ty.kind {
                        TyKind::FnPtr(signature) => self
                            .substitute_param_map(&signature.binder.value.output, &substitutions),
                        _ => unreachable!(),
                    };
                    if let hir::ExprKind::Path(path) = &callee.kind {
                        if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                            let args = self
                                .generic_call_args(def_id.clone(), &substitutions)?
                                .or_else(|| self.callable_output_args(&callee_ty, &substitutions));
                            if let Some(args) = args {
                                self.package().record_generic_call_arg(
                                    expr.hir_id.clone(),
                                    GenericCallResolution {
                                        def_id: def_id.clone(),
                                        args,
                                    },
                                );
                            }
                        }
                    }
                    output
                }
                hir::ExprKind::MethodCall(receiver, method, args) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    // A per-parameter expected-type hint (mirroring `Call`'s
                    // existing one, above) needs the callee's *declared*
                    // signature before any argument is checked — `Self`'s
                    // position substitutes cleanly from `receiver_ty` alone,
                    // independent of the other arguments, so this doesn't
                    // need to wait for them the way the final, full
                    // `instantiate_call` inside `method_output` below does.
                    // Load-bearing for a closure argument to a generic
                    // method (`Option::map_or`'s `f: fn(T) -> U`): without a
                    // real `T` hint here, the closure's own parameter is
                    // unusable when its body gets checked.
                    let declared_inputs =
                        match self.method_declared_signature(&receiver_ty, method).await {
                            Ok(Some(Ty {
                                kind: TyKind::FnPtr(sig),
                            })) => Some(sig.binder.value.inputs),
                            _ => None,
                        };
                    let mut arg_types = vec![receiver_ty.clone()];
                    for (index, arg) in args.iter().enumerate() {
                        // `inputs[0]` is `Self`; `args[index]` lines up with
                        // `inputs[index + 1]`.
                        let param_hint = declared_inputs
                            .as_ref()
                            .and_then(|inputs| inputs.get(index + 1))
                            .map(|ty| (**ty).clone());
                        let actual = if let Some(hint) = &param_hint {
                            self.with_expected_expr_type(hint.clone())
                                .check_expr(&arg.value)
                                .await
                        } else {
                            self.check_expr(&arg.value).await
                        };
                        arg_types.push(actual?);
                    }
                    let formatter_append = method.as_str() == "append"
                        && matches!(&receiver_ty.kind, TyKind::Ref(_, inner, _)
                            if matches!(&inner.kind, TyKind::Adt(adt, _)
                                if self
                                    .program_rc()
                                    .def_path(adt.did.clone())
                                    .is_some_and(|path| path.segments.last().is_some_and(|name| name.as_str() == "Formatter"))));
                    if formatter_append {
                        return Ok(self.unit_ty());
                    }
                    // Method resolution has no natural "error" `DefId` to
                    // substitute (unlike `Ty::error()`), so the whole
                    // `Result` from `method_output` (and anything it calls
                    // via `?` internally, like `method_generic_args`) is
                    // caught right here instead of inside those functions —
                    // one catch point covers all of them.
                    match self.method_output(&receiver_ty, method, &arg_types).await {
                        Ok((method_def_id, generic_args, output)) => {
                            let package = self.package();
                            package.record_method_resolution(
                                expr.hir_id.clone(),
                                method_def_id.clone(),
                            );
                            if let Some(args) = generic_args {
                                package.record_generic_method_arg(
                                    expr.hir_id.clone(),
                                    GenericCallResolution {
                                        def_id: method_def_id,
                                        args,
                                    },
                                );
                            }
                            output
                        }
                        Err(error) => self.error_ty_with_span(error.to_string(), expr.span),
                    }
                }
                hir::ExprKind::FieldAccess(receiver, field) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    self.field_ty(&receiver_ty, field).await?
                }
                hir::ExprKind::Index(receiver, index) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    let index_ty = self.check_expr(index).await?;
                    let receiver_ty = match &receiver_ty.kind {
                        TyKind::Ref(_, inner, _) => inner.as_ref(),
                        _ => &receiver_ty,
                    };
                    match &receiver_ty.kind {
                        TyKind::Array(inner, _) | TyKind::Slice(inner) => {
                            self.require_same_at(
                                &index_ty,
                                &Ty::uint(ty::UintTy::Usize),
                                expr.span,
                            )?;
                            (**inner).clone()
                        }
                        // `HashMap<K, V>` is a real struct (see
                        // `collection_constructor_signature`), not
                        // `Array`/`Slice`, so `[]` on it needs its own case
                        // here rather than falling out of the generic shape
                        // check above. `Vec<T>` used to need the identical
                        // treatment, but it's a real struct with a real
                        // `Index<usize>` impl now (`crates/fp-lang/src/std/alloc/mod.fp`),
                        // so it goes through the general method-dispatch
                        // fallback below like any other type would.
                        TyKind::Adt(adt, args)
                            if Some(adt.did.clone())
                                == self.well_known_struct_def_id("HashMap") =>
                        {
                            let (Some(GenericArg::Type(key_ty)), Some(GenericArg::Type(value_ty))) =
                                (args.first(), args.get(1))
                            else {
                                return Ok(self.error_ty(
                                    "HashMap index requires key and value type arguments",
                                ));
                            };
                            self.require_same_at(&index_ty, key_ty, expr.span)?;
                            value_ty.clone()
                        }
                        // Any other nominal (struct) type — dispatch `x[i]`
                        // through ordinary method resolution (the same
                        // `method_output` path `MethodCall` uses) by
                        // looking for an `index` method, the way `Vec<T>`'s
                        // `Index<usize>` impl provides one. This is what
                        // lets a type support indexing simply by
                        // implementing `fn index(&self, idx: ...) ->
                        // Output` — mirroring Rust's `Index` trait in
                        // spirit, using this language's existing
                        // structural (name + signature based, no vtables)
                        // method dispatch rather than inventing new
                        // trait-object machinery. Scoped to `Adt` only
                        // (not a catch-all `_`): every `impl` block targets
                        // a nominal type, so a primitive/error/other kind
                        // can never have an `index` method to find, and
                        // trying anyway would burn a full-program method
                        // scan and risk a confusing second diagnostic on
                        // top of a plain type error (e.g. indexing an
                        // `i32`).
                        TyKind::Adt(_, _) => {
                            let arg_types = vec![receiver_ty.clone(), index_ty.clone()];
                            match self
                                .method_output(receiver_ty, &hir::Symbol::from("index"), &arg_types)
                                .await
                            {
                                Ok((method_def_id, generic_args, output)) => {
                                    let package = self.package();
                                    package.record_method_resolution(expr.hir_id.clone(), method_def_id.clone());
                                    if let Some(args) = generic_args {
                                        package.record_generic_method_arg(
                                            expr.hir_id.clone(),
                                            GenericCallResolution {
                                                def_id: method_def_id,
                                                args,
                                            },
                                        );
                                    }
                                    output
                                }
                                Err(_) => self.error_ty(
                                    "indexing requires an array or slice, or a type implementing Index",
                                ),
                            }
                        }
                        _ => self.error_ty("indexing requires an array or slice"),
                    }
                }
                hir::ExprKind::Cast(value, target) => {
                    self.check_expr(value).await?;
                    self.check_type_expr(target).await?
                }
                hir::ExprKind::Struct(path, fields) => {
                    let ty = match self.enum_variant_ty(path).await? {
                        Some(ty) => ty,
                        None => self.path_ty(path).await?,
                    };
                    let payload_ty = self.enum_struct_payload_type(path, &ty).await?;
                    // When the literal's path has no explicit `<T>` args,
                    // `path_ty`/`enum_variant_ty` leave the ADT's generic
                    // args as raw, unsubstituted `TyKind::Param`s (see
                    // `path_ty`'s no-args fallback). `field_ty` substitutes
                    // using those same args, so it likewise returns a raw
                    // `Param` for a generic field — unify each field's
                    // declared (possibly still-generic) type against its
                    // actual value type the same way call arguments already
                    // are (`unify_call_types`/`instantiate_call`), instead
                    // of comparing them with strict equality, then apply
                    // the resulting substitutions back onto the ADT type.
                    let mut substitutions = HashMap::new();
                    for field in fields {
                        let field_ty = if let Some(payload) = payload_ty.as_ref() {
                            self.field_ty(payload, &field.name).await?
                        } else {
                            self.field_ty(&ty, &field.name).await?
                        };
                        // Scope the expected-type hint to *this field's*
                        // declared type, not whatever hint the enclosing
                        // struct literal itself was checked under — e.g.
                        // `BinaryHeap { values: Vec::new(), .. }` inside a
                        // function returning `BinaryHeap<T>` must not leak
                        // that outer `BinaryHeap<T>` hint into `values`'
                        // own zero-arg `Vec::new()` call, which needs (and
                        // has) its own field type, `Vec<T>`, to infer from.
                        let value_ty = self
                            .with_expected_expr_type(field_ty.clone())
                            .check_expr(&field.expr)
                            .await;
                        let value_ty = value_ty?;
                        self.unify_call_types(&field_ty, &value_ty, &mut substitutions)?;
                    }
                    self.substitute_param_map(&ty, &substitutions)
                }
                hir::ExprKind::If(condition, then_expr, else_expr) => {
                    let condition = self.check_expr(condition).await?;
                    self.require_same_at(&condition, &Ty::bool(), expr.span)?;
                    let then_ty = self.check_expr(then_expr).await?;
                    let mut result_ty = then_ty;
                    if let Some(else_expr) = else_expr {
                        let else_ty = self.check_expr(else_expr).await?;
                        result_ty = self.unify_branch_types(&result_ty, &else_ty)?;
                    }
                    match else_expr.as_ref() {
                        Some(_) => result_ty,
                        None => self.unit_ty(),
                    }
                }
                hir::ExprKind::Match(scrutinee, arms) => {
                    let scrutinee_ty = self.check_expr(scrutinee).await?;
                    if arms.is_empty() {
                        return Ok(self.error_ty("match expression requires at least one arm"));
                    }
                    let mut result: Option<Ty> = None;
                    for arm in arms {
                        let arm_ty = self.check_match_arm(arm, &scrutinee_ty).await?;
                        if let Some(result_ty) = &result {
                            result = Some(self.unify_branch_types(result_ty, &arm_ty)?);
                        } else {
                            result = Some(arm_ty);
                        }
                    }
                    result.unwrap_or_else(|| {
                        self.error_ty("match expression requires at least one arm")
                    })
                }
                hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                    self.check_block(block).await?
                }
                hir::ExprKind::ConstBlock(const_block) => {
                    let body_ty = self.check_expr(&const_block.body).await?;
                    // Recorded unconditionally, before any generic-param
                    // skip below — `register_const_block_comptime_entry`
                    // (`hir_to_mir`'s own body-walk, reached whenever this
                    // item's typecheck succeeds overall even if *this*
                    // const block's own comptime evaluation was skipped or
                    // failed) panics with "const block has no checked type
                    // in TypeckResults" if this entry is missing — it has
                    // no other way to know this expression's type, and a
                    // missing entry used to be unreachable only because
                    // every such item's typecheck previously hard-failed
                    // via the request's own `?` before HIR-to-MIR ever ran.
                    self.package()
                        .record_expr_type(expr.hir_id.clone(), body_ty.clone());
                    // A `const { .. }` block whose value still depends on
                    // an uninstantiated generic parameter of the enclosing
                    // (still-generic) function/impl can't be evaluated yet
                    // — real Rust defers exactly this case to
                    // monomorphization, evaluating it once per concrete
                    // instantiation. This compiler has no per-instantiation
                    // comptime re-evaluation machinery yet, so requesting
                    // evaluation here would reach a genuinely unresolved
                    // `TyKind::Param` downstream and crash instead of
                    // failing gracefully. Skip the request — leaving no
                    // comptime value recorded routes through `hir_to_mir`'s
                    // own existing fallback ("no comptime value available":
                    // lower the block's body as ordinary code instead).
                    if !ty_contains_param(&body_ty) {
                        // Deliberately non-fatal: a failure here (most
                        // commonly the body references a generic/const-
                        // generic parameter this compiler can't evaluate
                        // before monomorphization — the same class of
                        // "can't be evaluated yet" case `ty_contains_param`
                        // catches above, just from the *value* instead of
                        // the return type) is safe to swallow for an
                        // *expression*-position block: its value feeds
                        // ordinary runtime code, not a type-level decision,
                        // so `hir_to_mir`'s own existing "no comptime value
                        // available — lower the body as ordinary code"
                        // fallback (reached via `register_const_block_
                        // comptime_entry`, now hardened to fail gracefully
                        // instead of panicking on a missing checked type —
                        // see that function's own doc comment) handles it
                        // correctly. This was tried once before and
                        // reverted because that fallback still panicked on
                        // a *different*, sibling const block inside the
                        // same enclosing item; now fixed at the source, so
                        // this is safe to re-enable.
                        let def_id = const_block.def_id.clone();
                        let request = crate::ComptimeRequest {
                            package_id: self.current_package(),
                            def_id: def_id.clone(),
                        };
                        HirTypeChecker::spawn_comptime_task(&self.root_handle(), def_id, request)
                            .await;
                    }
                    body_ty
                }
                hir::ExprKind::While(condition, block) => {
                    let condition_ty = self.check_expr(condition).await?;
                    self.require_same_at(&condition_ty, &Ty::bool(), expr.span)?;
                    self.check_block(block).await?
                }
                hir::ExprKind::For(pat, iter, body) => {
                    // Only ever constructed for a target whose
                    // `LanguageCapabilities::first_class_for_loops` is set
                    // (see `ast_to_hir::exprs::transform_for_to_hir`) — the
                    // loop pattern's own scope must enclose the body block,
                    // so it's pushed/popped here rather than delegated to
                    // `check_block`, which only scopes the body itself.
                    let iter_ty = self.check_expr(iter).await?;
                    let elem_ty = self.for_loop_element_ty(&iter_ty).await;
                    let mut scope = self.with_fresh_block_scope();
                    scope.bind_pattern(pat, elem_ty).await?;
                    scope.check_block(body).await?;
                    self.unit_ty()
                }
                hir::ExprKind::Array(values) => {
                    if values.is_empty() {
                        // No element to infer from — fall back to the
                        // ambient expected-type hint (a `let` binding's
                        // own type annotation, a function's declared
                        // return type, ...), the same hint threaded
                        // through everywhere else in this checker
                        // (`with_expected_expr_type`). Only an array/slice
                        // expectation is a legitimate answer here; any
                        // other expected type just means this `[]` isn't
                        // actually in array position yet (mismatch is
                        // reported elsewhere), so still fall through to
                        // the error in that case.
                        if let Some(expected) = &self.expected_expr_type {
                            // `&[T]`/`&[T; N]` (a reference to the
                            // array/slice, not the bare shape itself) is
                            // by far the more common expectation in
                            // practice — an owned `[]` almost never
                            // appears where a bare `Array`/`Slice` value
                            // type is expected, but constantly appears
                            // where a function parameter expects a
                            // reference to one.
                            let unwrapped = match &expected.kind {
                                TyKind::Ref(_, inner, _) => inner.as_ref(),
                                _ => expected,
                            };
                            if matches!(&unwrapped.kind, TyKind::Array(_, _) | TyKind::Slice(_)) {
                                return Ok(unwrapped.clone());
                            }
                        }
                        return Ok(self.error_ty("empty array has no inferable element type"));
                    }
                    let mut value_types = Vec::with_capacity(values.len());
                    for value in values {
                        value_types.push(self.check_expr(value).await?);
                    }
                    let element = values
                        .iter()
                        .zip(&value_types)
                        .find_map(|(value, value_ty)| {
                            (!matches!(value.kind, hir::ExprKind::Literal(hir::Lit::Integer(_))))
                                .then(|| value_ty.clone())
                        })
                        .unwrap_or_else(|| value_types[0].clone());
                    for (value, value_ty) in values.iter().zip(value_types) {
                        let integer_literal =
                            matches!(value.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                        let integer_element =
                            matches!(element.kind, TyKind::Int(_) | TyKind::Uint(_));
                        if !(integer_literal && integer_element) {
                            self.require_same_at(&element, &value_ty, expr.span)?;
                        }
                    }
                    Ty {
                        kind: TyKind::Array(
                            Box::new(element),
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: values.len() as u128,
                                    size: 8,
                                },
                            ))),
                        ),
                    }
                }
                hir::ExprKind::ArrayRepeat { elem, len } => {
                    let element = self.check_expr(elem).await?;
                    self.check_expr(len).await?;
                    let length = match &len.kind {
                        hir::ExprKind::Literal(hir::Lit::Integer(value)) if *value >= 0 => {
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: *value as u128,
                                    size: 8,
                                },
                            )))
                        }
                        _ => ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id.local_id())),
                    };
                    Ty {
                        kind: TyKind::Array(Box::new(element), length),
                    }
                }
                hir::ExprKind::Tuple(values) => {
                    let mut element_types = Vec::with_capacity(values.len());
                    for value in values {
                        element_types.push(Box::new(self.check_expr(value).await?));
                    }
                    Ty {
                        kind: TyKind::Tuple(element_types),
                    }
                }
                hir::ExprKind::Assign(lhs, rhs) => {
                    let lhs = self.check_expr(lhs).await?;
                    // Give the RHS the same expected-type hint `ConstBlock`
                    // already provides its body: a zero-arg generic call
                    // like `Vec::new()` has no argument types to infer `T`
                    // from, so without this it leaves `T` unresolved and
                    // `require_same` below fails a plain reassignment like
                    // `self.keys = Vec::new();` even though the field's own
                    // declared type unambiguously determines `T`.
                    let rhs = self
                        .with_expected_expr_type(lhs.clone())
                        .check_expr(rhs)
                        .await;
                    let rhs = rhs?;
                    // `unify_call_types`, not `require_same`: an assignment
                    // target's type should accept a value the same way a
                    // call parameter of that type would (e.g. `self.field =
                    // some_fn_returning_str();` into a `&str` field — the
                    // same `Ref`-peeling coercion call arguments already
                    // get), not require exact structural equality.
                    let mut substitutions = HashMap::new();
                    self.unify_call_types(&lhs, &rhs, &mut substitutions)?;
                    lhs
                }
                hir::ExprKind::Return(value) | hir::ExprKind::Break(value) => {
                    match value.as_ref() {
                        Some(value) => self.check_expr(value).await?,
                        None => self.unit_ty(),
                    }
                }
                hir::ExprKind::Continue => Ty::never(),
                hir::ExprKind::Let(pattern, target, value) => {
                    let ty = self.check_type_expr(target).await?;
                    let hint = self
                        .program_rc()
                        .take_raw_refinement_hint(target.hir_id.clone());
                    if let Some(value) = value {
                        let value_ty = self.check_expr(value).await?;
                        self.require_same_at(&ty, &value_ty, expr.span)?;
                        if let Some(hint) = &hint {
                            self.discharge_refinement(hint, value)?;
                        }
                    }
                    self.bind_pattern(pattern, ty.clone()).await?;
                    ty
                }
                hir::ExprKind::Try(value) => {
                    let input_ty = self.check_expr(&value.expr).await?;
                    let result_ty = input_ty.clone();
                    for catch in &value.catches {
                        if let Some(pattern) = &catch.pat {
                            self.bind_pattern(
                                pattern,
                                Ty {
                                    kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
                                },
                            )
                            .await?;
                        }
                        let catch_ty = self.check_expr(&catch.body).await?;
                        self.require_same_at(&result_ty, &catch_ty, expr.span)?;
                    }
                    if let Some(elze) = &value.elze {
                        let elze_ty = self.check_expr(elze).await?;
                        self.require_same_at(&result_ty, &elze_ty, expr.span)?;
                    }
                    if let Some(finally) = &value.finally {
                        self.check_expr(finally).await?;
                    }
                    result_ty
                }
                hir::ExprKind::With(context, body) => {
                    self.check_expr(context).await?;
                    self.check_expr(body).await?
                }
                hir::ExprKind::Slice(slice) => {
                    let base_ty = self.check_expr(&slice.base).await?;
                    if let Some(start) = &slice.start {
                        self.check_expr(start).await?;
                    }
                    if let Some(end) = &slice.end {
                        self.check_expr(end).await?;
                    }
                    match base_ty.kind {
                        TyKind::Array(inner, _) => Ty {
                            kind: TyKind::Slice(inner),
                        },
                        TyKind::Slice(inner) => Ty {
                            kind: TyKind::Slice(inner),
                        },
                        _ => self.error_ty("slicing requires an array or slice"),
                    }
                }
                hir::ExprKind::Closure(closure) => {
                    // The load-bearing case: an unannotated closure
                    // (`|s| ..`, the overwhelming majority) has no useful
                    // type of its own — its parameters are resolved from
                    // whatever `Fn`-shaped hint the call site pushed onto
                    // `expected_expr_types` (see `MethodCall`'s
                    // `method_declared_signature`-derived hint, and
                    // `Call`'s existing per-parameter hint above), mirroring
                    // rustc's own closure-signature deduction. An explicit
                    // parameter annotation still wins over the hint when
                    // present. With no hint and no annotation, the
                    // parameter gets an honest `Infer` placeholder rather
                    // than silently resolving to something unusable later.
                    let hint = self.expected_expr_type.as_ref().cloned();
                    let hint_sig = match &hint {
                        Some(Ty {
                            kind: TyKind::FnPtr(sig),
                        }) => Some(sig.binder.value.clone()),
                        _ => None,
                    };
                    let mut scope = self.with_fresh_block_scope();
                    let mut param_types = Vec::with_capacity(closure.params.len());
                    for (index, param) in closure.params.iter().enumerate() {
                        let declared = if matches!(param.ty.kind, hir::TypeExprKind::Infer) {
                            None
                        } else {
                            Some(scope.check_type_expr(&param.ty).await?)
                        };
                        let param_ty = declared
                            .or_else(|| {
                                hint_sig
                                    .as_ref()
                                    .and_then(|sig| sig.inputs.get(index))
                                    .map(|ty| (**ty).clone())
                            })
                            .unwrap_or_else(|| Ty {
                                kind: TyKind::Infer(ty::InferTy::FreshTy(param.hir_id.local_id())),
                            });
                        scope.bind_pattern(&param.pat, param_ty.clone()).await?;
                        param_types.push(param_ty);
                    }
                    let body_ty = scope.check_expr(&closure.body).await?;
                    Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: ty::FnSig {
                                    inputs: param_types.into_iter().map(Box::new).collect(),
                                    output: Box::new(body_ty),
                                    c_variadic: false,
                                    unsafety: ty::Unsafety::Normal,
                                    abi: ty::Abi::Rust,
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    }
                }
                hir::ExprKind::Query(_) => self.error_ty("query typing is not implemented"),
                hir::ExprKind::IntrinsicCall(call) => self.check_intrinsic(call).await?,
                hir::ExprKind::FormatString(format) => {
                    for part in &format.parts {
                        if let hir::FormatTemplatePart::Placeholder(placeholder) = part {
                            let _ = placeholder;
                        }
                    }
                    Ty {
                        kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
                    }
                }
            };
            self.package()
                .record_expr_type(expr.hir_id.clone(), ty.clone());
            Ok(ty)
        })
    }

    async fn check_block(&mut self, block: &hir::Block) -> Result<Ty> {
        self.check_block_with_expected_tail(block, None).await
    }

    /// Like `check_block`, but with an optional expected-type hint for the
    /// block's own trailing expression only (e.g. a function's declared
    /// return type) — pushed onto `expected_expr_types` right before
    /// checking the tail and popped immediately after, so it never leaks
    /// into unrelated statements earlier in the same block (a zero-arg
    /// generic call like `Vec::new()` mid-block has nothing to do with what
    /// the block as a whole eventually evaluates to).
    async fn check_block_with_expected_tail(
        &mut self,
        block: &hir::Block,
        expected_tail: Option<Ty>,
    ) -> Result<Ty> {
        let mut scope = self.with_fresh_block_scope();
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    let ty = match (&local.ty, &local.init) {
                        (Some(annotation), Some(init)) => {
                            let mut ty = scope.check_type_expr(annotation).await?;
                            let refinement_hint = scope
                                .program_rc()
                                .take_raw_refinement_hint(annotation.hir_id.clone());
                            let init_ty = scope.check_expr(init).await?;
                            if let Some(hint) = &refinement_hint {
                                scope.discharge_refinement(hint, init)?;
                            }

                            // `[T; _]`: resolve this binding's own declared-
                            // type hole from its own initializer — ordinary
                            // hole-inference for the annotation itself
                            // (mirroring Rust's stable `let x: [i32; _] =
                            // [1,2,3];`), not a call-argument-style
                            // coercion, so it belongs here regardless of
                            // anything else. Every other combination
                            // (including a genuinely mismatched slice/array
                            // annotation) is left to the ordinary, now-
                            // strict equality check below — an array
                            // literal's type is always `[T; N]`, so e.g.
                            // `let x: [i32] = [1,2,3,4];` is simply an
                            // ordinary `[i32]` vs `[i32; 4]` type mismatch,
                            // not a case needing special detection here.
                            if let TyKind::Array(elem, ty::ConstKind::Infer(_)) = &ty.kind {
                                let literal_len = match &init.kind {
                                    hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. } => {
                                        match &init_ty.kind {
                                            TyKind::Array(_, len) => const_kind_to_u64(len),
                                            _ => None,
                                        }
                                    }
                                    _ => None,
                                };
                                if let Some(literal_len) = literal_len {
                                    ty = Ty {
                                        kind: TyKind::Array(
                                            elem.clone(),
                                            u64_to_const_kind(literal_len),
                                        ),
                                    };
                                }
                            }

                            let resolved_init = if matches!(
                                init.kind,
                                hir::ExprKind::Literal(hir::Lit::Integer(_))
                            ) && matches!(
                                ty.kind,
                                TyKind::Int(_) | TyKind::Uint(_)
                            ) {
                                ty.clone()
                            } else if matches!(
                                init.kind,
                                hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. }
                            ) && matches!(ty.kind, TyKind::Array(_, _))
                            {
                                // Trust the annotation's element type over
                                // the literal's own inferred one (matches
                                // the integer-literal shortcut above —
                                // element types are otherwise flexible/
                                // untyped literals) *only* when the
                                // lengths genuinely agree; a real,
                                // concretely-known mismatch must surface
                                // via `init_ty` instead, so the strict
                                // check below actually catches it rather
                                // than tautologically comparing `ty`
                                // against a clone of itself.
                                let lengths_match = match (&ty.kind, &init_ty.kind) {
                                    (TyKind::Array(_, ty_len), TyKind::Array(_, init_len)) => {
                                        match (
                                            const_kind_to_u64(ty_len),
                                            const_kind_to_u64(init_len),
                                        ) {
                                            (Some(a), Some(b)) => a == b,
                                            _ => true,
                                        }
                                    }
                                    _ => true,
                                };
                                if lengths_match {
                                    ty.clone()
                                } else {
                                    init_ty.clone()
                                }
                            } else {
                                let mut substitutions = HashMap::new();
                                scope.unify_call_types(&init_ty, &ty, &mut substitutions)?;
                                scope.substitute_param_map(&init_ty, &substitutions)
                            };
                            if !Self::ty_matches_with_infer_holes(&ty, &resolved_init) {
                                return Err(Error::from(format!(
                                    "type mismatch: expected `{ty}`, found `{resolved_init}`"
                                )));
                            }
                            scope
                                .package()
                                .record_expr_type(init.hir_id.clone(), resolved_init.clone());
                            resolved_init
                        }
                        (Some(annotation), None) => scope.check_type_expr(annotation).await?,
                        (None, Some(init)) => scope.check_expr(init).await?,
                        (None, None) => scope.error_ty("local binding needs a type or initializer"),
                    };
                    scope.bind_pattern(&local.pat, ty).await?;
                }
                hir::StmtKind::Item(item) => scope.check_item(item).await?,
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    scope.check_expr(expr).await?;
                }
            }
        }
        let ty = match block.expr.as_ref() {
            Some(expr) => {
                if let Some(expected) = expected_tail {
                    let result = scope
                        .with_expected_expr_type(expected)
                        .check_expr(expr)
                        .await;
                    result?
                } else {
                    scope.check_expr(expr).await?
                }
            }
            None => scope.unit_ty(),
        };
        Ok(ty)
    }

    async fn check_match_arm(&mut self, arm: &hir::MatchArm, scrutinee_ty: &Ty) -> Result<Ty> {
        let mut scope = self.with_fresh_block_scope();
        scope.bind_pattern(&arm.pat, scrutinee_ty.clone()).await?;
        if let Some(guard) = &arm.guard {
            let guard_ty = scope.check_expr(guard).await?;
            scope.require_same_at(&guard_ty, &Ty::bool(), guard.span)?;
        }
        scope.check_expr(&arm.body).await
    }

    fn callable_output_args(
        &self,
        callable: &Ty,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Option<Vec<Ty>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return None;
        };
        let output = self.substitute_param_map(&signature.binder.value.output, substitutions);
        let TyKind::Adt(_, args) = output.kind else {
            return None;
        };
        let args = args
            .into_iter()
            .filter_map(|arg| match arg {
                GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .collect::<Vec<_>>();
        (!args.is_empty()).then_some(args)
    }

    fn check_type_expr<'a>(
        &'a mut self,
        expr: &'a hir::TypeExpr,
    ) -> crate::BoxFuture<'a, Result<Ty>> {
        Box::pin(async move {
            let ty = match &expr.kind {
                hir::TypeExprKind::Primitive(primitive) => primitive_ty(*primitive),
                hir::TypeExprKind::Path(path) => self.path_ty(path).await?,
                hir::TypeExprKind::Tuple(items) => {
                    let mut checked = Vec::with_capacity(items.len());
                    for item in items {
                        checked.push(Box::new(self.check_type_expr(item).await?));
                    }
                    Ty {
                        kind: TyKind::Tuple(checked),
                    }
                }
                hir::TypeExprKind::Slice(item) => Ty {
                    kind: TyKind::Slice(Box::new(self.check_type_expr(item).await?)),
                },
                hir::TypeExprKind::Ptr(item) => Ty {
                    kind: TyKind::RawPtr(ty::TypeAndMut {
                        ty: Box::new(self.check_type_expr(item).await?),
                        mutbl: ty::Mutability::Not,
                    }),
                },
                hir::TypeExprKind::Ref(item) => Ty {
                    kind: TyKind::Ref(
                        ty::Region::ReErased,
                        Box::new(self.check_type_expr(item).await?),
                        ty::Mutability::Not,
                    ),
                },
                hir::TypeExprKind::FnPtr(function) => {
                    let mut inputs = Vec::with_capacity(function.inputs.len());
                    for input in &function.inputs {
                        inputs.push(Box::new(self.check_type_expr(input).await?));
                    }
                    let output = Box::new(self.check_type_expr(&function.output).await?);
                    Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: ty::FnSig {
                                    inputs,
                                    output,
                                    c_variadic: false,
                                    unsafety: ty::Unsafety::Normal,
                                    abi: ty::Abi::Rust,
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    }
                }
                hir::TypeExprKind::Never => Ty::never(),
                hir::TypeExprKind::Array(item, length) => Ty {
                    kind: TyKind::Array(
                        Box::new(self.check_type_expr(item).await?),
                        match length.as_deref() {
                            Some(hir::Expr {
                                kind: hir::ExprKind::Literal(hir::Lit::Integer(value)),
                                ..
                            }) if *value >= 0 => ty::ConstKind::Value(ty::ConstValue::Scalar(
                                ty::Scalar::Int(ty::ScalarInt {
                                    data: *value as u128,
                                    size: 8,
                                }),
                            )),
                            _ => {
                                ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id.local_id()))
                            }
                        },
                    ),
                },
                hir::TypeExprKind::Infer => Ty {
                    kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id.local_id())),
                },
                hir::TypeExprKind::ConstBlock(def_id, body) => {
                    // Requested immediately, in place — no deferral to the
                    // end of the item's check (there is no more
                    // `pending_type_const_blocks` staging list): the shared
                    // `HirTypeChecker`/`get_or_spawn` dedup already makes
                    // this cheap if another item reaches the same block
                    // first.
                    let def_id = def_id.clone();
                    let hir_id = expr.hir_id.clone();
                    let body_ty = self.check_expr(body).await?;
                    // A `const { .. }` block whose value still depends on
                    // an uninstantiated generic parameter of the enclosing
                    // (still-generic) function/impl can't be evaluated yet
                    // — real Rust defers exactly this case to
                    // monomorphization, evaluating it once per concrete
                    // instantiation. This compiler has no per-instantiation
                    // comptime re-evaluation machinery yet, so requesting
                    // evaluation here would reach a genuinely unresolved
                    // `TyKind::Param` downstream and crash instead of
                    // failing gracefully. Skip the request — leaving no
                    // comptime value recorded routes through `hir_to_mir`'s
                    // own existing fallback ("no comptime value available":
                    // lower the block's body as ordinary code instead),
                    // correct for a value that isn't const-critical outside
                    // a monomorphized instantiation.
                    if !ty_contains_param(&body_ty) {
                        let request = crate::ComptimeRequest {
                            package_id: self.current_package(),
                            def_id: def_id.clone(),
                        };
                        HirTypeChecker::spawn_comptime_task(&self.root_handle(), def_id, request)
                            .await
                            .ok_or_else(|| Error::from("comptime evaluation failed"))?;
                    }
                    // Replace the `Infer` placeholder just below with the
                    // body's actual checked type, now that it's known —
                    // matches expression-position const-blocks, whose own
                    // type is likewise the checked type of their body.
                    self.package()
                        .record_type_expr_type(hir_id, body_ty.clone());
                    body_ty
                }
                hir::TypeExprKind::Error => self.error_ty("invalid type expression"),
                hir::TypeExprKind::Structural(_) => {
                    self.error_ty("structural types are not supported by HIR typing")
                }
                hir::TypeExprKind::TypeBinaryOp(op)
                    if matches!(op.kind, fp_core::ast::TypeBinaryOpKind::Union) =>
                {
                    let lhs_ty = self.check_type_expr(&op.lhs).await?;
                    let rhs_ty = self.check_type_expr(&op.rhs).await?;
                    let lhs_literals = self.program_rc().literal_type_hint(op.lhs.hir_id.clone());
                    let rhs_literals = self.program_rc().literal_type_hint(op.rhs.hir_id.clone());
                    match (lhs_literals, rhs_literals) {
                        (Some(mut lhs), Some(rhs)) => {
                            for value in rhs {
                                if !lhs.contains(&value) {
                                    lhs.push(value);
                                }
                            }
                            self.program_rc()
                                .insert_literal_type_hint(expr.hir_id.clone(), lhs);
                            str_ty()
                        }
                        _ => {
                            let _ = (lhs_ty, rhs_ty);
                            self.error_ty(
                                "type expressions cannot be combined with a type operator",
                            )
                        }
                    }
                }
                // `+` (`TypeBinaryOpKind::Add`) is also how fp-lang parses
                // a multi-bound trait-object/`impl` type
                // (`dyn Error + Send + Sync`, `impl Iterator<Item = T> +
                // '_`) — the same token this compiler's struct-composition
                // `+` uses (see `struct_fields_from_type`'s own `Add` arm),
                // just in a context with no structural fields to merge at
                // all. This compiler has no multi-trait `dyn`/`impl`
                // representation to check against regardless, so — same
                // "tolerate what's broken, keep what isn't" policy as
                // elsewhere — approximate it as just its first bound
                // rather than hard-erroring the whole enclosing type
                // (dropping the extra bounds is lossy but harmless: they
                // only ever narrow what's already a supertype-shaped
                // reference here, never change its own concrete shape).
                hir::TypeExprKind::TypeBinaryOp(op) => self.check_type_expr(&op.lhs).await?,
                hir::TypeExprKind::LiteralString(value) => {
                    self.program_rc()
                        .insert_literal_type_hint(expr.hir_id.clone(), vec![value.clone()]);
                    str_ty()
                }
                hir::TypeExprKind::Type => Ty { kind: TyKind::Type },
                hir::TypeExprKind::Any => Ty { kind: TyKind::Any },
                hir::TypeExprKind::Refinement {
                    base,
                    binder,
                    predicate,
                } => {
                    let base_ty = self.check_type_expr(base).await?;
                    self.program_rc().insert_raw_refinement_hint(
                        expr.hir_id.clone(),
                        hir::RefinementHint {
                            binder: binder.clone(),
                            predicate: (**predicate).clone(),
                            base: base_ty.clone(),
                        },
                    );
                    base_ty
                }
            };
            self.package()
                .record_type_expr_type(expr.hir_id.clone(), ty.clone());
            Ok(ty)
        })
    }

    async fn path_ty(&mut self, path: &hir::Path) -> Result<Ty> {
        if path.segments.len() == 1 {
            let name = path.segments[0].name.as_str();
            if let Some(primitive) = primitive_path_ty(name) {
                return Ok(primitive);
            }
        }
        // `ast_to_hir` tags a non-nominal self-type shape (`&T`, `[T]`,
        // `fn(..) -> ..`, ...) with `Res::Builtin` instead of a `DefId`
        // (mirrors rustc's `SimplifiedType` fast-reject bucketing — see
        // `BuiltinSelfType`'s own doc comment) — reachable here whenever
        // one of these gets approximated down to a bare path (e.g. the
        // first bound of a multi-bound `dyn Fn(..) -> X + Send` type).
        // The real element/signature type behind the tag was already
        // discarded by that point, so this can only ever produce the
        // same arity-blind approximation `BuiltinSelfType` itself already
        // commits to, not a precise reconstruction.
        match &path.res {
            Some(hir::Res::Builtin(hir::BuiltinSelfType::Function)) => {
                return Ok(Ty {
                    kind: TyKind::FnPtr(ty::PolyFnSig {
                        binder: ty::Binder {
                            value: ty::FnSig {
                                inputs: Vec::new(),
                                output: Box::new(Ty { kind: TyKind::Tuple(Vec::new()) }),
                                c_variadic: false,
                                unsafety: ty::Unsafety::Normal,
                                abi: ty::Abi::Rust,
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                });
            }
            Some(hir::Res::Builtin(hir::BuiltinSelfType::Unit | hir::BuiltinSelfType::Never | hir::BuiltinSelfType::Tuple)) => {
                return Ok(Ty { kind: TyKind::Tuple(Vec::new()) });
            }
            Some(hir::Res::Builtin(kind)) => {
                return Ok(self.error_ty(format!(
                    "type path resolved to an unsupported builtin shape: {kind:?}"
                )));
            }
            _ => {}
        }
        if let Some(hir::Res::Def(ref def_id)) = path.res {
            // A local `type X = const { .. };` (`ast_to_hir`'s
            // `comptime_type_alias_rhs` lowering) binds `X` to the const
            // block's own `DefId` (scope-local only, not a real `def_map`
            // item — see that lowering site's doc comment). Its shape is
            // whatever the tagged expression comptime-evaluated to, already
            // resolved by the time any later statement's `path_ty` call
            // runs (the expression-position `ConstBlock` arm checks it
            // eagerly, in-sequence). A real item's `Res::Def` never has a
            // `const_block_values` entry, so this only fires for the
            // comptime-local case; everything else falls through to the
            // ordinary `def_map`-based resolution below.
            if let Some(value) = self.package().const_block_value(def_id.clone()) {
                return Ok(match value {
                    fp_core::ast::Value::Type(fp_core::ast::Ty::Struct(struct_ty)) => {
                        let fields: Vec<(hir::Symbol, Ty)> = struct_ty
                            .fields
                            .iter()
                            .map(|field| {
                                let field_ty = ast_value_ty_to_hir_ty(&field.value)
                                    .unwrap_or_else(|| self.error_ty(format!(
                                        "field `{}`'s comptime-constructed type is not supported here",
                                        field.name.name
                                    )));
                                (hir::Symbol::new(field.name.name.clone()), field_ty)
                            })
                            .collect();
                        self.program_rc()
                            .insert_local_struct_fields(def_id.clone(), fields.clone());
                        let variant = ty::VariantDef {
                            def_id: def_id.clone(),
                            ctor_def_id: None,
                            ident: hir::Symbol::new(struct_ty.name.name.clone()),
                            discr: ty::VariantDiscr::Relative(0),
                            fields: fields
                                .iter()
                                .map(|(name, _)| ty::FieldDef {
                                    did: def_id.clone(),
                                    ident: name.clone(),
                                    vis: ty::TyVisibility::Public,
                                })
                                .collect(),
                            ctor_kind: ty::CtorKind::Fn,
                            is_recovered: false,
                        };
                        Ty {
                            kind: TyKind::Adt(
                                AdtDef {
                                    did: def_id.clone(),
                                    variants: vec![variant],
                                    flags: AdtFlags::IS_STRUCT | AdtFlags::IS_COMPTIME_LOCAL,
                                    repr: ReprOptions {
                                        int: None,
                                        align: None,
                                        pack: None,
                                        flags: ReprFlags::empty(),
                                        field_shuffle_seed: 0,
                                    },
                                },
                                Vec::new(),
                            ),
                        }
                    }
                    _ => self.error_ty(format!("local `{def_id:?}` is not a type")),
                });
            }
        }
        if matches!(path.res, Some(hir::Res::SelfTy)) {
            let Some(self_type) = self.self_type.clone() else {
                tracing::debug!(?path, "Self is not available in this type context");
                return Ok(self.error_ty(format!(
                    "Self is not available in this type context (path: {:?})",
                    path
                )));
            };
            // `Self::Target` (an associated-type path rooted at `Self`,
            // e.g. inside `impl Deref for X { fn deref(&self) -> &Self::
            // Target { .. } }`) — resolved from the enclosing impl's own
            // `type Target = Y;` binding (`assoc_types`, set alongside
            // `self_type`). Deliberately doesn't consult a trait default
            // or resolve `Self::X` for code outside the impl — see
            // `impl_assoc_types`'s doc comment. Kept as a fallback here in
            // case this function is ever reached for a genuinely type-like
            // use, though the method lookup above should cover the
            // overwhelming majority of real value-path cases.
            if let Some(assoc_segment) = path.segments.get(1) {
                let scope = self.assoc_types.as_ref();
                if let Some(ty) = scope.and_then(|scope| scope.get(&assoc_segment.name)) {
                    return Ok(ty.clone());
                }
                // `Self` is still a generic parameter here (a blanket
                // impl's own `Self = I`, or a trait default method's
                // implicit `Self: Trait`) — there is no concrete impl to
                // have populated `assoc_types` from, so fall back to the
                // same trait-bound-derived resolution `T::AssocType`
                // already uses for an explicit generic parameter.
                if let TyKind::Param(param) = &self_type.kind {
                    if let Some(ty) = self
                        .assoc_type_from_generic_param_bounds(&param.name, &assoc_segment.name)
                        .await?
                    {
                        return Ok(ty);
                    }
                }
                tracing::debug!(
                    assoc = %assoc_segment.name,
                    ?self_type,
                    "associated type not defined in this impl"
                );
                return Ok(self.error_ty(format!(
                    "associated type `Self::{}` is not defined in this impl (self type: {:?})",
                    assoc_segment.name, self_type
                )));
            }
            return Ok(self_type);
        }
        let Some(def_id) = (match path.res {
            Some(hir::Res::Def(ref def_id)) => Some(def_id),
            _ => None,
        }) else {
            // Treat `void` as unit type (C compatibility) — a genuine,
            // narrow, intentional modeling choice: C's `void` has no Rust
            // equivalent type at all, unlike every other unresolved path
            // below, which is a real resolution failure that must surface
            // rather than be silently laundered into `()`.
            if path.segments.len() == 1 && path.segments[0].name.as_str() == "void" {
                return Ok(hir::Ty {
                    kind: hir::ty::TyKind::Tuple(vec![]),
                });
            }
            // `create_null_type` (`ast_to_hir::mod.rs`) deliberately builds
            // this exact shape — an unresolved `Res::None` path literally
            // named `null` — for the never type (`!`)/`ast::Ty::Nothing`,
            // matching how `hir_to_mir::lower_path_type`'s own "null" case
            // already treats it (a raw byte pointer, `*const i8`). Not a
            // laundering fallback: it's the same internal synthetic-type
            // convention already used downstream, just missing here too.
            if path.segments.len() == 1 && path.segments[0].name.as_str() == "null" {
                return Ok(Ty {
                    kind: TyKind::RawPtr(ty::TypeAndMut {
                        ty: Box::new(Ty::int(ty::IntTy::I8)),
                        mutbl: ty::Mutability::Not,
                    }),
                });
            }
            // `<usize as Add>::Output`-style UFCS paths lower to a flat
            // `usize::Output` (`parse_qualified_path_type` in fp-lang
            // intentionally drops the `as Trait` disambiguator — its own
            // doc comment explains why), which `ast_to_hir` never
            // resolves (`usize` is a primitive, not a module/item, so no
            // `Res` exists for it) — real rustc resolves an unqualified
            // `T::AssocName` the same way, by searching `T`'s own trait
            // impls for the one that declares `AssocName`. Reuse the
            // same impl-matching machinery `method_output_at` already
            // uses for `.method()` calls, just for an associated type
            // instead of a method.
            //
            // `X::Residual::TryType` (the `?`-operator/`try_trait_v2`
            // desugaring's own idiom throughout real vendored std) is a
            // 3-segment *chained* projection: `X::Residual` is itself an
            // associated-type projection (on `Try`), and `::TryType` then
            // projects again on *that* result (via `Residual<O>`) — this
            // whole UFCS branch only ever resolves a single flat
            // `Base::AssocName` hop (segments[0]/segments.last()), so a
            // chain like this always fell straight through to "unresolved
            // type path" (the middle segment silently dropped). Handled
            // as a hardcoded special case rather than a general chained-
            // projection resolver: `Residual<O>::TryType` is *defined* by
            // real Rust's own trait law to always round-trip back to the
            // original `Try`-implementing type itself (see `Residual`'s
            // own doc comment: "`<Result<Infallible, E> as Residual<T>>::
            // TryType = Result<T, E>`" — i.e. `X::Residual::TryType ==
            // X` for any `X`, unconditionally), so this is a safe,
            // exact answer, not an approximation.
            if let [base, mid, tail] = path.segments.as_slice() {
                if mid.name.as_str() == "Residual" && tail.name.as_str() == "TryType" {
                    // `path.res` is already known `None` here (that's why
                    // this whole branch is reached at all), so `base`
                    // itself is, in every real case this chain actually
                    // appears in real vendored std, a still-generic type
                    // parameter with no separate `Res` of its own to
                    // check — approximate it the same nominal-placeholder
                    // way `assoc_type_from_generic_param_bounds` already
                    // does for an unresolved bound projection elsewhere:
                    // the exact identity doesn't matter for unification
                    // purposes here, only that every reference to `base`
                    // by this same name checks to the same placeholder.
                    return Ok(Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: u32::MAX,
                            name: base.name.clone(),
                        }),
                    });
                }
            }
            if path.segments.len() >= 2 {
                let base_ty = match primitive_path_ty(path.segments[0].name.as_str()) {
                    Some(ty) => Some(ty),
                    // The same flattening also drops the `as Trait` from
                    // e.g. `<Wrapping<u8> as Add>::Output` — unlike a
                    // primitive, the base here is a real struct, still
                    // named by its own first segment, just with the
                    // struct's own generic args carried on that segment
                    // (see `parse_qualified_path_type`'s `Name::
                    // ParameterPath` arm) instead of on the trait.
                    None => {
                        let args = match &path.segments[0].args {
                            Some(generic_args) => {
                                let mut checked = Vec::with_capacity(generic_args.args.len());
                                for arg in &generic_args.args {
                                    let arg = match arg {
                                        hir::GenericArg::Type(ty) => {
                                            GenericArg::Type(self.check_type_expr(ty).await?)
                                        }
                                        hir::GenericArg::Const(_) => {
                                            GenericArg::Type(self.error_ty(
                                                "const generic arguments are not supported",
                                            ))
                                        }
                                    };
                                    checked.push(arg);
                                }
                                checked
                            }
                            None => Vec::new(),
                        };
                        self.well_known_struct_ty(path.segments[0].name.as_str(), args)
                    }
                };
                if let Some(base_ty) = base_ty {
                    let assoc_name = path.segments.last().unwrap().name.clone();
                    if let Some(ty) = self.assoc_type_for_self(&base_ty, &assoc_name).await? {
                        return Ok(ty);
                    }
                }
                // `<bool as Not>::Output` flattens the same way to `bool::
                // Output`, but `bool` also happens to name a *real* module
                // at the crate root (`core::bool`, a handful of small
                // bool-related helpers) — `ast_to_hir`'s absolute-crate-
                // root-module resolution (used generally so a bare
                // top-level module name resolves without an explicit
                // `use`) doesn't know this position means the primitive,
                // not the module, so it further expands the already-
                // flattened path to `core::bool::Output` before this ever
                // reaches type-checking. Retry treating the *middle*
                // segment as the primitive base in exactly that 3-segment
                // shape, rather than teaching `ast_to_hir` to special-case
                // one primitive's own unlucky name collision.
                if let [_first, middle, last] = path.segments.as_slice() {
                    if let Some(base_ty) = primitive_path_ty(middle.name.as_str()) {
                        if let Some(ty) = self.assoc_type_for_self(&base_ty, &last.name).await? {
                            return Ok(ty);
                        }
                    }
                }
                // `T::AssocName` where `T` still names an in-scope generic
                // parameter (real closures' `F::Output` for `F: FnOnce()
                // -> R`, or `I::Item` for `I: Iterator<Item = U>`) — no
                // concrete `base_ty` exists yet to search impls for, but
                // the parameter's own bound already declares the
                // projection directly.
                if let Some(ty) = self
                    .assoc_type_from_generic_param_bounds(
                        &path.segments[0].name,
                        &path.segments.last().unwrap().name,
                    )
                    .await?
                {
                    return Ok(ty);
                }
            }
            return Ok(self.error_ty(format!(
                "unresolved type path `{}`{}",
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::"),
                self.current_item_path
                    .as_deref()
                    .map(|item| format!(" (in `{item}`)"))
                    .unwrap_or_default()
            )));
        };
        if let Some(generic) = self.generic_ty(def_id.clone()) {
            return Ok(generic);
        }
        // A transparent type alias (`type __darwin_useconds_t =
        // __uint32_t;`) — HIR has no first-class item for one (see
        // `hir::HirPackage::type_alias_targets`'s doc comment), so its
        // `DefId` has no `def_map` entry to look up; expand it in place by
        // checking its already-lowered target type expression instead.
        if let Some(target) = self.program_rc().type_alias_target(def_id.clone()).cloned() {
            return self.check_type_expr(&target).await;
        }
        let Some(item) = self.program_rc().item(def_id.clone()).cloned() else {
            // No `hir::Item`/`def_path` at all for this `DefId` — real
            // vendored std's dominant case is a trait's own *associated
            // type declaration* (`trait Iterator { type Item; }`)
            // referenced directly by its declaring `DefId` (e.g. from
            // within the trait's own default method bodies/signatures,
            // where there's no concrete impl yet to substitute the
            // projection through) — HIR has no first-class item for an
            // associated-type declaration itself, only for the *trait*
            // that owns it. Approximate the same way a bare trait
            // reference does just above: a nominal `Param` placeholder
            // keyed by the name actually referenced, so every use of it
            // unifies consistently instead of hard-erroring.
            let name = path
                .segments
                .last()
                .map(|segment| segment.name.clone())
                .unwrap_or_else(|| hir::Symbol::new("_"));
            return Ok(Ty {
                kind: TyKind::Param(ty::ParamTy {
                    index: u32::MAX,
                    name,
                }),
            });
        };
        let (flags, variants) = match &item.kind {
            hir::ItemKind::Struct(_) => (AdtFlags::IS_STRUCT, Vec::new()),
            hir::ItemKind::Enum(def) => (
                AdtFlags::IS_ENUM,
                def.variants
                    .iter()
                    .enumerate()
                    .map(|(index, variant)| ty::VariantDef {
                        def_id: variant.def_id.clone(),
                        ctor_def_id: Some(variant.def_id.clone()),
                        ident: variant.name.clone(),
                        discr: ty::VariantDiscr::Relative(index as u32),
                        fields: Vec::new(),
                        ctor_kind: ty::CtorKind::Fn,
                        is_recovered: false,
                    })
                    .collect(),
            ),
            // A bare reference to a *trait's* own `DefId` in type position
            // (`&Error`/`&(dyn Error + 'static)` flattened down to just
            // `Error` somewhere along the way, or a trait bound resolved
            // straight to its `Res::Def` rather than going through the
            // `dyn`/`impl Trait` multi-bound-approximation machinery this
            // checker already has elsewhere) — HIR has no real trait-
            // object type to build here, so approximate the same way
            // `assoc_type_from_generic_param_bounds`/the `X::Residual::
            // TryType` special case elsewhere in this file already do:
            // a nominal placeholder keyed by the trait's own name, so
            // every reference to it unifies consistently rather than
            // hard-erroring "not a type" (which cascades into unrelated
            // `HIR type mismatch`/`method not found` noise wherever a
            // std trait like `Error` is referenced this way — the
            // dominant real-corpus case).
            hir::ItemKind::Trait(_) => {
                let name = path
                    .segments
                    .last()
                    .map(|segment| segment.name.clone())
                    .unwrap_or_else(|| hir::Symbol::new("dyn"));
                return Ok(Ty {
                    kind: TyKind::Param(ty::ParamTy {
                        index: u32::MAX,
                        name,
                    }),
                });
            }
            _ => return Ok(self.error_ty(format!("definition `{def_id}` is not a type"))),
        };
        let args = match path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
        {
            Some(args) => {
                let mut checked = Vec::with_capacity(args.args.len());
                for arg in &args.args {
                    let arg = match arg {
                        hir::GenericArg::Type(ty) => {
                            GenericArg::Type(self.check_type_expr(ty).await?)
                        }
                        hir::GenericArg::Const(_) => GenericArg::Type(
                            self.error_ty("const generic arguments are not supported"),
                        ),
                    };
                    checked.push(arg);
                }
                // A source reference may omit trailing generic params that
                // have a declared default (`Vec<T>`, never spelling out
                // `Vec<T, A = Global>`'s allocator) — without padding
                // these back in, this `Adt`'s arg count (1) permanently
                // disagrees with every impl's own arg count (2, since an
                // impl block's generics are always written out in full),
                // so `generic_args_compatible`/`unify_call_types` can
                // never match this receiver against any of that type's
                // impls at all (see `method_output`'s `matches_receiver`).
                // A param with no declared default falls back to a fresh
                // type parameter, exactly like the fully-omitted (`None`)
                // case below — still less specific than a real default,
                // but at least the right *count*, so matching by base
                // ADT + arity-correct unification still works.
                let declared_params = match &item.kind {
                    hir::ItemKind::Struct(def) => Some(&def.generics.params),
                    hir::ItemKind::Enum(def) => Some(&def.generics.params),
                    _ => None,
                };
                if let Some(declared_params) = declared_params {
                    for (index, parameter) in declared_params.iter().enumerate().skip(checked.len())
                    {
                        let default_ty = match &parameter.kind {
                            hir::GenericParamKind::Type {
                                default: Some(default),
                            } => Some(self.check_type_expr(default).await?),
                            _ => None,
                        };
                        checked.push(GenericArg::Type(default_ty.unwrap_or_else(|| Ty {
                            kind: TyKind::Param(ty::ParamTy {
                                index: index as u32,
                                name: parameter.name.clone(),
                            }),
                        })));
                    }
                }
                checked
            }
            None => match &item.kind {
                hir::ItemKind::Struct(def) => def
                    .generics
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| {
                        GenericArg::Type(Ty {
                            kind: TyKind::Param(ty::ParamTy {
                                index: index as u32,
                                name: parameter.name.clone(),
                            }),
                        })
                    })
                    .collect(),
                hir::ItemKind::Enum(def) => def
                    .generics
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| {
                        GenericArg::Type(Ty {
                            kind: TyKind::Param(ty::ParamTy {
                                index: index as u32,
                                name: parameter.name.clone(),
                            }),
                        })
                    })
                    .collect(),
                _ => Vec::new(),
            },
        };
        Ok(Ty {
            kind: TyKind::Adt(
                AdtDef {
                    did: def_id.clone(),
                    variants,
                    flags,
                    repr: ReprOptions {
                        int: None,
                        align: None,
                        pack: None,
                        flags: ReprFlags::empty(),
                        field_shuffle_seed: 0,
                    },
                },
                args,
            ),
        })
    }

    /// `vec![...]`/collection-literal macros desugar to calls like
    /// `Vec::from([...])`/`HashMap::from([...])` that have no real backing
    /// function — MIR lowering recognizes them purely by their trailing
    /// path segments (see the `"Vec"`/`"List"`/`"HashMap"` name checks in
    /// `hir_to_mir/expr.rs`, e.g. around its `lower_call`/path-type-
    /// resolution code). Because nothing ever defines them, HIR's resolver
    /// never gives their path a `Res::Def`, so plain path resolution always
    /// reports "unresolved value path". This synthesizes a real, unifiable
    /// function signature for exactly that fixed set of names, mirroring
    /// MIR lowering's list — keep the two in sync.
    fn collection_constructor_signature(&mut self, path: &hir::Path) -> Option<Result<Ty>> {
        let mut names = path.segments.iter().rev().map(|seg| seg.name.as_str());
        let last = names.next()?;
        let second_last = names.next();
        if last != "from" {
            return None;
        }
        let make_sig = |inputs: Vec<Ty>, output: Ty| Ty {
            kind: TyKind::FnPtr(ty::PolyFnSig {
                binder: ty::Binder {
                    value: ty::FnSig {
                        inputs: inputs.into_iter().map(Box::new).collect(),
                        output: Box::new(output),
                        c_variadic: false,
                        unsafety: ty::Unsafety::Normal,
                        abi: ty::Abi::Rust,
                    },
                    bound_vars: Vec::new(),
                },
            }),
        };
        match second_last {
            Some("HashMap") => {
                let k = Ty {
                    kind: TyKind::Param(ty::ParamTy {
                        index: 0,
                        name: hir::Symbol::new("K"),
                    }),
                };
                let v = Ty {
                    kind: TyKind::Param(ty::ParamTy {
                        index: 1,
                        name: hir::Symbol::new("V"),
                    }),
                };
                let entry = match self.well_known_struct_ty(
                    "HashMapEntry",
                    vec![GenericArg::Type(k.clone()), GenericArg::Type(v.clone())],
                ) {
                    Some(ty) => ty,
                    None => {
                        return Some(Ok(
                            self.error_ty("`HashMapEntry` struct definition was not found")
                        ));
                    }
                };
                let output = match self
                    .well_known_struct_ty("HashMap", vec![GenericArg::Type(k), GenericArg::Type(v)])
                {
                    Some(ty) => ty,
                    None => {
                        return Some(Ok(
                            self.error_ty("`HashMap` struct definition was not found")
                        ));
                    }
                };
                let input = Ty {
                    kind: TyKind::Slice(Box::new(entry)),
                };
                Some(Ok(make_sig(vec![input], output)))
            }
            _ => None,
        }
    }

    /// Element type a `hir::ExprKind::For`'s loop pattern binds to, given
    /// the already-checked type of its iterator expression. Handles the
    /// shapes an un-desugared `for` loop's `iter` can actually resolve to:
    /// a real `Array`/`Slice`, `Vec<T>` (a real struct with a generic
    /// argument, not `TyKind::Array`/`Slice` — see the `Index` arm above
    /// for why `Vec` needs its own case here rather than falling out of
    /// the array/slice shapes), or any other type with a real `Iterator`
    /// impl reachable by its own declared `type Item = ..;` (looked up the
    /// same way any other associated-type projection is, via
    /// `assoc_type_for_self` — real vendored std's own iterator adapters
    /// declare `Item` directly on themselves, so this needs no actual
    /// `next()`-calling iteration protocol modeled, just the same impl
    /// search every other associated-type/method lookup already does).
    /// Anything else records a diagnostic and yields an error type rather
    /// than hard-failing the whole item.
    async fn for_loop_element_ty(&mut self, iter_ty: &Ty) -> Ty {
        let iter_ty = match &iter_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => iter_ty,
        }
        .clone();
        match &iter_ty.kind {
            TyKind::Array(elem, _) | TyKind::Slice(elem) => (**elem).clone(),
            TyKind::Adt(adt, args)
                if Some(adt.did.clone()) == self.well_known_struct_def_id("Vec") =>
            {
                match args.first() {
                    Some(GenericArg::Type(elem)) => elem.clone(),
                    _ => self.error_ty("`Vec` for-loop iterator missing its element type argument"),
                }
            }
            _ => {
                if let Ok(Some(item_ty)) = self
                    .assoc_type_for_self(&iter_ty, &hir::Symbol::new("Item"))
                    .await
                {
                    return item_ty;
                }
                self.for_loop_element_ty_fallback_error(&iter_ty)
            }
        }
    }

    fn for_loop_element_ty_fallback_error(&self, iter_ty: &Ty) -> Ty {
        self.error_ty(format!(
            "`for` loop iterator must be Vec/array/slice-shaped, found `{iter_ty}`"
        ))
    }

    /// Finds a real struct definition by name, searching this package first
    /// and then loaded dependency packages — used only for well-known
    /// standard-library collection types that a synthesized function
    /// signature (see `collection_constructor_signature`) needs to name as
    /// its output type, since normal path resolution never runs for them.
    /// O(1) per package via `hir::HirPackage::struct_defs_by_name` (built once
    /// per package, not scanned per lookup).
    fn well_known_struct_def_id(&self, name: &str) -> Option<hir::DefId> {
        self.program_rc().struct_def_id(name)
    }

    fn well_known_struct_ty(&self, name: &str, args: Vec<GenericArg>) -> Option<Ty> {
        let did = self.well_known_struct_def_id(name)?;
        Some(Ty {
            kind: TyKind::Adt(
                AdtDef {
                    did,
                    variants: Vec::new(),
                    flags: AdtFlags::IS_STRUCT,
                    repr: ReprOptions {
                        int: None,
                        align: None,
                        pack: None,
                        flags: ReprFlags::empty(),
                        field_shuffle_seed: 0,
                    },
                },
                args,
            ),
        })
    }

    async fn expr_path_ty(&mut self, path: &hir::Path) -> Result<Ty> {
        tracing::debug!(?path, "expr_path_ty");
        // A multi-segment `Self::` value path is an associated
        // function/method called via `Self::name(..)` (e.g. `Self::new()`,
        // or a default trait method calling a sibling via
        // `Self::helper(self, ..)`) — resolve it the same way `.method()`
        // call syntax already does, via `method_declared_signature_at`.
        if matches!(path.res, Some(hir::Res::SelfTy)) {
            if let Some(self_ty) = self.self_type.clone() {
                match path.segments.len() {
                    // A bare `Self` value path — the callee of a
                    // tuple-struct-style constructor call (`Self(x)`, real
                    // std's own idiom throughout, e.g. wrapper types built
                    // as `Self(value)` inside their own inherent impl) or
                    // any other position that names `Self` as a plain
                    // value rather than a member of it. `Self` unqualified
                    // simply *is* the enclosing impl's own type — no
                    // method/const lookup needed, unlike the `Self::name`
                    // case below.
                    1 => return Ok(self_ty),
                    2 => {
                        let method_segment = &path.segments[1];
                        let found = self
                            .method_declared_signature_at(&self_ty, &method_segment.name)
                            .await?;
                        tracing::debug!(
                            method = %method_segment.name,
                            ?self_ty,
                            found = found.is_some(),
                            "Self::method lookup"
                        );
                        if let Some(sig) = found {
                            return Ok(sig);
                        }
                        return Ok(self.error_ty(format!(
                            "method `{}` not found on `Self` (self type: {:?})",
                            method_segment.name, self_ty
                        )));
                    }
                    _ => {}
                }
            }
        }
        // A primitive named directly (`u8::MAX`, `u8::from_str_radix(..)`)
        // — the same type-relative shape as `Map::new`/`Add::add`, but the
        // base has no `DefId` at all (see `ast_to_hir`'s `Res::Builtin
        // (BuiltinSelfType::Primitive(..))` fallback). The receiver type
        // is simply the primitive itself; resolve the tail the same way
        // `Self::`/struct bases already do.
        if let Some(hir::Res::Builtin(hir::BuiltinSelfType::Primitive(name))) = &path.res {
            if let (Some(receiver_ty), Some(tail)) = (primitive_path_ty(name), path.segments.get(1))
            {
                if path.segments.len() == 2 {
                    if let Some(sig) = self
                        .method_declared_signature_at(&receiver_ty, &tail.name)
                        .await?
                    {
                        return Ok(sig);
                    }
                    return Ok(self.error_ty(format!(
                        "no item named `{}` found on primitive `{}`",
                        tail.name, name
                    )));
                }
            }
            // A bare primitive name used standalone as a value (e.g. a
            // macro's own type parameter substituted directly into a
            // generic-argument-as-value position, like real vendored
            // std's `can_not_overflow::<$int_ty>(..)` pattern once
            // `$int_ty` is a concrete primitive) — no method tail to
            // resolve, so there's no callee/const to look up; its value
            // is the type itself, the same meta-type every other bare
            // type-as-value expression already checks to (see
            // `transform_value_to_hir`'s `Value::Type` arm, lowered to
            // the identical `IntrinsicCall(PrimitiveType)` this mirrors).
            if path.segments.len() == 1 {
                return Ok(Ty { kind: TyKind::Type });
            }
        }
        if let Some(hir::Res::Local(ref local)) = path.res {
            if let Some(ty) = self.program_rc().pat_type(local.clone()) {
                return Ok(ty);
            }
            if let Some(name) = path.segments.last().map(|segment| &segment.name) {
                if let Some(ty) = self.locals.get(name) {
                    return Ok(ty.clone());
                }
            }
            // HIR locals are resolved by the lowering resolver. Their
            // bindings may be outside this pass's lexical reconstruction
            // (for example generated closure parameters), so preserve the
            // resolved value path and let MIR handle its value semantics.
            let _ = local;
            return Ok(self.error_ty(format!("local `{local}` has no inferred type")));
        }
        // A fully- or partially-qualified trait-method value path
        // (`core::fmt::Display::fmt`/`fmt::Debug::fmt`, real vendored
        // std's own idiom for calling a trait method explicitly rather
        // than through dot-call syntax) sometimes reaches here with
        // `path.res` still `None` — `ast_to_hir`'s own multi-segment
        // resolution only ever matches a *literal* absolute key or a
        // bare (1-segment) import-relative name, neither of which covers
        // an arbitrary-length qualified prefix ending in a trait name.
        // Recover by looking the second-to-last segment (the presumed
        // trait/type name, since the last segment is the method) up by
        // its own bare name across every package's exports — the same
        // "defining module unknown, search by suffix" fallback
        // `ast_to_hir`'s own `resolve_ambiguous_type_export_by_name`
        // already uses for the analogous type-position case.
        let recovered_def_id = if !matches!(path.res, Some(hir::Res::Def(_)))
            && path.segments.len() >= 2
        {
            let trait_name = &path.segments[path.segments.len() - 2].name;
            // `find_export_by_name` only ever sees already-published
            // dependency packages' `hir_exports` (a table nothing in
            // `ast_to_hir` actually populates for the package currently
            // being lowered/checked) — a same-package qualified
            // trait-method path like `core::fmt::Display::fmt`, checked
            // while compiling `core` itself, needs to be found in this
            // package's own module tree instead.
            // Neither the workspace-wide export table
            // (`find_export_by_name`, which only ever sees already-published
            // dependency packages) nor this package's own `module_tree`
            // (rebuilt from scratch per source file by
            // `reset_file_context`, so only the last-lowered file's
            // bindings survive to typecheck time) can answer an
            // intra-package lookup by bare name. `def_paths` is the one
            // package-scoped table that is populated once per `DefId` and
            // never cleared — recorded across every file it's a
            // `DefId -> DefPath` map, so scan it for an entry whose last
            // segment is this trait name.
            let found = self.program_rc().find_export_by_name(trait_name.as_str());
            let found = found.or_else(|| {
                let package = self.package();
                let program = self.program_rc();
                // `HashMap` iteration order isn't stable across runs (its
                // default hasher is seeded per-process), so picking the
                // first match by iteration order would make which
                // same-named item gets recovered here nondeterministic —
                // pick the lowest `DefId` index instead (first declared)
                // for a reproducible result regardless of hash seed. Also
                // filter to items that are actually traits *before*
                // picking the minimum: some names are reused by an
                // unrelated non-trait item too (e.g. `core::mem::
                // type_info::Pointer`, a plain struct, alongside the real
                // `core::fmt::Pointer` trait this recovery is meant to
                // find for `Pointer::fmt`) — picking the lowest index
                // without this filter could land on the wrong one and
                // reject it for not being a trait, even though a real
                // trait with this name does exist.
                package
                    .def_paths
                    .iter()
                    .filter(|(def_id, def_path)| {
                        def_path
                            .segments
                            .last()
                            .is_some_and(|seg| seg.as_str() == trait_name.as_str())
                            && program
                                .item((*def_id).clone())
                                .is_some_and(|item| matches!(&item.kind, hir::ItemKind::Trait(_)))
                    })
                    .min_by_key(|(def_id, _)| def_id.index)
                    .map(|(def_id, _)| hir::Res::Def(def_id.clone()))
            });
            match found {
                Some(hir::Res::Def(def_id))
                    if self
                        .program_rc()
                        .item(def_id.clone())
                        .is_some_and(|item| matches!(&item.kind, hir::ItemKind::Trait(_))) =>
                {
                    Some(def_id)
                }
                _ => None,
            }
        } else {
            None
        };
        let def_id = match (&path.res, &recovered_def_id) {
            (Some(hir::Res::Def(def_id)), _) => def_id.clone(),
            (_, Some(def_id)) => def_id.clone(),
            _ => {
                if let Some(sig) = self.collection_constructor_signature(path) {
                    return sig;
                }
                // A value path can refer to a definition supplied by a
                // loaded crate. It is resolved by HIR lowering, while
                // this pass only needs a semantic value type for
                // subsequent expression checks.
                return Ok(self.error_ty(format!(
                    "unresolved value path `{}`",
                    path.segments
                        .iter()
                        .map(|segment| segment.name.as_str())
                        .collect::<Vec<_>>()
                        .join("::")
                )));
            }
        };
        let def_id = &def_id;
        // Type-relative value path (`Map::new(..)`, `T::default()`) —
        // `ast_to_hir` resolves only the *base* segment (mirroring rustc's
        // `QPath::TypeRelative`: the resolver settles the type, typeck
        // settles the method), so `def_id` here names the struct/enum/
        // trait/generic-param the path starts from, not the method itself.
        if path.segments.len() > 1 {
            let tail_method = &path.segments.last().unwrap().name;
            // A generic type parameter base (`T::default()` where
            // `T: Default`) — there is no impl to search (`T` is still
            // abstract), so resolve the method against the parameter's own
            // trait bounds instead, the same way `assoc_type_from_generic_
            // param_bounds` resolves `T::AssocType` for the analogous
            // associated-type case.
            if let Some(sig) = self
                .generic_param_bound_method_signature(&path.segments[0].name, tail_method)
                .await?
            {
                return Ok(sig);
            }
            // A struct/enum base (`Map::new(iter, f)`) — the receiver type
            // is the struct/enum itself (still generic if it declares type
            // parameters), known immediately with no argument context
            // needed, unlike the trait-qualified case below.
            let is_struct_or_enum = self
                .program_rc()
                .item(def_id.clone())
                .map(|item| {
                    matches!(
                        &item.kind,
                        hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_)
                    )
                })
                .unwrap_or(false);
            if is_struct_or_enum {
                let receiver_ty = self.path_ty(path).await?;
                if let Some(sig) = self
                    .method_declared_signature_at(&receiver_ty, tail_method)
                    .await?
                {
                    return Ok(sig);
                }
            }
            // A trait base (`Add::add(a, b)`) — which impl to search
            // depends on the concrete receiver, which isn't known until
            // the call's own arguments are checked, so this function
            // genuinely cannot resolve it yet. Return a bare (undiagnosed)
            // error type rather than falling into the ordinary per-
            // `ItemKind` handling below, whose catch-all calls `error_ty`
            // and so *records a diagnostic immediately* — that would fire
            // even when the `Call` arm's argument-typed fallback goes on
            // to resolve this same path successfully right afterward. A
            // real "unresolved" diagnostic belongs to whichever call site
            // actually exhausts every resolution attempt, not to this
            // intermediate, expected-to-be-incomplete step.
            if let Some(item) = self.program_rc().item(def_id.clone()) {
                if matches!(&item.kind, hir::ItemKind::Trait(_)) {
                    return Ok(Ty::error());
                }
            }
        }
        let Some(item) = self.program_rc().item(def_id.clone()).cloned() else {
            // `program` is an owned `Rc` clone (cheap — it's the same
            // `Rc<HirProgram>` `self.program` already is), not a borrow
            // of `self` — so `item`/`impl_item` below can stay borrowed
            // from it across the `self.with_generics(..)` call afterward
            // (which needs `&mut self`) with no conflict, and this whole
            // fallback no longer needs to clone the owning impl's
            // `generics`/`self_ty`/`items`/`function` just to escape a
            // borrow of `self` that was never actually necessary.
            let program = self.program_rc();
            let owner_id = program
                .package(&self.current_package())
                .and_then(|package| package.member_owner(def_id.clone()));
            let found = owner_id
                .and_then(|owner_id| program.item(owner_id))
                .and_then(|item| {
                    let hir::ItemKind::Impl(impl_item) = &item.kind else {
                        return None;
                    };
                    let impl_member = impl_item
                        .items
                        .iter()
                        .find(|member| member.def_id == *def_id)?;
                    Some((impl_item, &impl_member.kind))
                });
            if let Some((impl_item, hir::ImplItemKind::Method(function))) = found {
                let mut scope = self.with_generics(&impl_item.generics);
                let self_ty = scope.check_type_expr(&impl_item.self_ty).await?;
                let mut scope = scope.with_self_type(self_ty);
                let assoc_types = scope
                    .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                    .await?;
                let mut scope = scope.with_assoc_types(assoc_types);
                return scope.function_signature(function).await;
            }
            // An impl's own associated const (`impl char { pub const MIN:
            // char = '\0'; }`) — its *declared* type annotation is all a
            // value-position reference elsewhere needs (exactly like a
            // top-level `const`'s `ItemKind::Const` arm below, which also
            // never needs the const's initializer body/value to know its
            // type). Deliberately does NOT route through `ensure_item_
            // checked`/`spawn_item_task` the way a top-level const does:
            // those are keyed per *top-level* item, so awaiting one would
            // mean awaiting this whole impl block's task — every other
            // method in it too — before this one const's type is even
            // available. For an impl with many methods (`impl char`) that
            // reference each other across a mutually-recursive type
            // (`char` <-> `core::wtf8::CodePoint`, say), that
            // whole-impl-granularity wait is exactly what the executor's
            // "genuine dependency cycle" stall detector was catching —
            // not a real cycle in the *types* (a const's declared type
            // never needs another item's body), only in this
            // coarser-than-necessary task granularity.
            if let Some((impl_item, hir::ImplItemKind::AssocConst(constant))) = found {
                let mut scope = self.with_generics(&impl_item.generics);
                let self_ty = scope.check_type_expr(&impl_item.self_ty).await?;
                let mut scope = scope.with_self_type(self_ty);
                return scope.check_type_expr(&constant.ty).await;
            }
            // Any package's own `impl_method_item_index` (built once per
            // package, see `hir::HirPackage::index_derived_lookups`) gives the
            // enclosing impl's item index directly — same-package or
            // cross-package alike, since `def_id` already carries the
            // owning `package_id` and `self.program_rc()` now holds
            // every package uniformly. Matches both `Method` and
            // `AssocConst` members the same way the same-package `found`
            // lookup just above does — see its own doc comment for why
            // an associated const only ever needs its declared type, never
            // a whole-impl wait.
            let cross_package_member = self
                .program_rc()
                .package(&def_id.package_id)
                .and_then(|package| {
                    let impl_def_id = package.impl_method_item_index.get(&def_id)?;
                    package.def_map.get(impl_def_id)
                })
                .and_then(|item| {
                    let hir::ItemKind::Impl(impl_item) = &item.kind else {
                        return None;
                    };
                    impl_item.items.iter().find_map(|member| {
                        if member.def_id != *def_id {
                            return None;
                        }
                        Some((
                            impl_item.generics.clone(),
                            impl_item.self_ty.clone(),
                            impl_item.items.clone(),
                            member.kind.clone(),
                        ))
                    })
                });
            if let Some((generics, self_ty, impl_items, member_kind)) = cross_package_member {
                match member_kind {
                    hir::ImplItemKind::Method(function) => {
                        let self_ty_hir_id = self_ty.hir_id.clone();
                        let mut scope = self.with_generics(&generics);
                        let self_ty = scope.check_type_expr(&self_ty).await?;
                        let mut scope = scope.with_self_type(self_ty);
                        let assoc_types =
                            scope.impl_assoc_types(&impl_items, self_ty_hir_id).await?;
                        let mut scope = scope.with_assoc_types(assoc_types);
                        return scope.function_signature(&function).await;
                    }
                    hir::ImplItemKind::AssocConst(constant) => {
                        let mut scope = self.with_generics(&generics);
                        let self_ty = scope.check_type_expr(&self_ty).await?;
                        let mut scope = scope.with_self_type(self_ty);
                        return scope.check_type_expr(&constant.ty).await;
                    }
                    _ => {}
                }
            }
            let matched_enum_item = self
                .program_rc()
                .member_owner(def_id.clone())
                .and_then(|owner_id| self.program_rc().item(owner_id).cloned())
                .filter(|item| matches!(&item.kind, hir::ItemKind::Enum(_)));
            if let Some(enum_item) = matched_enum_item {
                let hir::ItemKind::Enum(enum_def) = &enum_item.kind else {
                    unreachable!("matched_enum_item only holds ItemKind::Enum items")
                };
                let variant = enum_def
                    .variants
                    .iter()
                    .find(|variant| variant.def_id == *def_id)
                    .expect("matched_enum_item's enum_def contains this variant");
                let enum_ty = self.enum_item_ty(&enum_item, path).await?;
                if let Some(payload) = &variant.payload {
                    let mut scope = self.with_generics(&enum_def.generics);
                    let payload_result = scope.check_type_expr(payload).await;
                    let payload_ty = payload_result?;
                    let inputs = match payload_ty.kind {
                        TyKind::Tuple(fields) => fields,
                        _ => vec![Box::new(payload_ty)],
                    };
                    return Ok(Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: ty::FnSig {
                                    inputs,
                                    output: Box::new(enum_ty),
                                    c_variadic: false,
                                    unsafety: ty::Unsafety::Normal,
                                    abi: ty::Abi::Rust,
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    });
                }
                return Ok(enum_ty);
            }
            // Same root cause as `path_ty`'s identical fallback: a trait's
            // own associated-item declaration (an associated const, most
            // often) referenced directly by its declaring `DefId` with no
            // concrete impl context to substitute through — HIR has no
            // first-class item for the declaration itself. Approximate
            // with a nominal placeholder instead of hard-erroring.
            let name = path
                .segments
                .last()
                .map(|segment| segment.name.clone())
                .unwrap_or_else(|| hir::Symbol::new("_"));
            return Ok(Ty {
                kind: TyKind::Param(ty::ParamTy {
                    index: u32::MAX,
                    name,
                }),
            });
        };
        match &item.kind {
            // A tuple struct's name used in value position (`Wrapping(x)`,
            // `TryFromIntError(())`) is its own constructor function, not
            // a bare type — its fields have no names of their own
            // (positional `0`, `1`, ... assigned by lowering), so build
            // the constructor's `FnPtr` (fields as inputs, the struct
            // itself as output) instead of falling through to `path_ty`'s
            // bare-type answer, which left every such call with an `Adt`
            // callee and no signature to check the call against at all.
            hir::ItemKind::Struct(def)
                if !def.fields.is_empty()
                    && def
                        .fields
                        .iter()
                        .enumerate()
                        .all(|(index, field)| field.name.as_str() == index.to_string()) =>
            {
                let output = self.path_ty(path).await?;
                let fields = def.fields.clone();
                let mut inputs = Vec::with_capacity(fields.len());
                for field in &fields {
                    inputs.push(Box::new(self.check_type_expr(&field.ty).await?));
                }
                Ok(Ty {
                    kind: TyKind::FnPtr(ty::PolyFnSig {
                        binder: ty::Binder {
                            value: ty::FnSig {
                                inputs,
                                output: Box::new(output),
                                c_variadic: false,
                                unsafety: ty::Unsafety::Normal,
                                abi: ty::Abi::Rust,
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                })
            }
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => self.path_ty(path).await,
            hir::ItemKind::Const(constant)
                if matches!(
                    constant.body.value.kind,
                    hir::ExprKind::Literal(hir::Lit::Integer(_))
                ) =>
            {
                self.check_type_expr(&constant.ty).await
            }
            hir::ItemKind::Const(constant) if !self.package().def_map.contains_key(&def_id) => {
                // A `DefId`'s own `package_id` field is not a reliable
                // "which package is this really in" signal on its own —
                // e.g. `hir::DefId::local(..)` always uses a generic
                // placeholder `package_id`, regardless of which real
                // package its item was actually registered under (see
                // several `hir_typeck` unit tests below, which construct
                // same-package items this way). Check real membership in
                // *this* package's own `def_map` instead: `item` above was
                // found via `program_rc().item(..)`, the merged cross-
                // package view (a dependency's const genuinely is found
                // there too) — but `ensure_item_checked`/`typecheck_item`
                // (below, the same-package arm) only ever spawn a task
                // against *this* package's own `program`/`results`, which
                // a foreign `def_id` never populates: awaiting it would
                // just hang or silently no-op, and `const_types` never
                // gets an entry, surfacing as "constant type was not
                // recorded" downstream. Check it fresh instead — the same
                // fallback `expr_path_ty`'s cross-package branch above
                // uses for a foreign impl method's signature.
                let declared_ty = self.check_type_expr(&constant.ty).await?;
                self.with_expected_expr_type(declared_ty)
                    .check_body(&constant.body)
                    .await
            }
            hir::ItemKind::Const(_) => {
                // A same-package `const` may be declared later in
                // `program.items` than the item currently being checked —
                // `const_types` is written by that other const's own task
                // (see `typecheck_item`), so ensure it's run (on demand,
                // shared with any other awaiter via `get_or_spawn`) before
                // reading it, rather than assuming textual order already
                // reached it. A genuine cycle (`const A` needs `const B`
                // needs `const A`) surfaces as a stalled executor, not a
                // wrong answer — this `.await` just suspends like any
                // other.
                if self.package().const_type(def_id.clone()).is_none() {
                    HirTypeChecker::spawn_item_task(&self.root_handle(), def_id.clone()).await;
                }
                Ok(self
                    .package()
                    .const_type(def_id.clone())
                    .unwrap_or_else(|| self.error_ty("constant type was not recorded")))
            }
            hir::ItemKind::Function(function) => self.function_signature(function).await,
            _ => Ok(self.error_ty("resolved path is not a value")),
        }
    }

    async fn enum_item_ty(&mut self, item: &hir::Item, path: &hir::Path) -> Result<Ty> {
        let hir::ItemKind::Enum(enum_def) = &item.kind else {
            return Ok(self.error_ty("enum path does not resolve to an enum"));
        };
        let variants = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| ty::VariantDef {
                def_id: variant.def_id.clone(),
                ctor_def_id: Some(variant.def_id.clone()),
                ident: variant.name.clone(),
                discr: ty::VariantDiscr::Relative(index as u32),
                fields: Vec::new(),
                ctor_kind: ty::CtorKind::Fn,
                is_recovered: false,
            })
            .collect();
        let explicit_args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref());
        let args = if let Some(args) = explicit_args {
            let mut checked = Vec::with_capacity(args.args.len());
            for arg in &args.args {
                let arg = match arg {
                    hir::GenericArg::Type(ty) => GenericArg::Type(self.check_type_expr(ty).await?),
                    hir::GenericArg::Const(_) => {
                        GenericArg::Type(self.error_ty("const generic arguments are not supported"))
                    }
                };
                checked.push(arg);
            }
            Some(checked)
        } else {
            None
        };
        let args = match args {
            Some(args) => args,
            None => enum_def
                .generics
                .params
                .iter()
                .enumerate()
                .map(|(index, parameter)| {
                    GenericArg::Type(Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: index as u32,
                            name: parameter.name.clone(),
                        }),
                    })
                })
                .collect(),
        };
        Ok(Ty {
            kind: TyKind::Adt(
                AdtDef {
                    did: item.def_id.clone(),
                    variants,
                    flags: AdtFlags::IS_ENUM,
                    repr: ReprOptions {
                        int: None,
                        align: None,
                        pack: None,
                        flags: ReprFlags::empty(),
                        field_shuffle_seed: 0,
                    },
                },
                args,
            ),
        })
    }

    async fn function_signature(&mut self, function: &hir::Function) -> Result<Ty> {
        // Keyed on the function's own `output` HirId — unique per declared
        // function, and never revisited via a different generic
        // substitution here (this checks the function's *own* declared
        // generics, not a call site's), so it's safe to cache across every
        // call site that asks for this same function's signature.
        let cache_key = function.sig.output.hir_id.clone();
        if let Some(cached) = self.program_rc().function_signature(cache_key.clone()) {
            return Ok(cached);
        }
        // A genuine `Err` from checking this function's own declared
        // inputs/output (as opposed to `check_type_expr`'s far more
        // common "record a diagnostic, return an error-placeholder `Ty`"
        // recovery) must still populate the cache — this function is
        // reached from candidate search over every impl in the workspace
        // (`method_output_at`/`assoc_type_for_self`'s trait-default-
        // method fallback), so an uncached hard failure here means this
        // exact same real, once-legitimate error gets independently
        // re-triggered (and re-recorded as a diagnostic) once per
        // candidate-search caller across the whole corpus instead of
        // once — the same O(workspace) blowup class already fixed for
        // `checked_impl_self_ty`/`impl_assoc_types` above, just reached
        // through the genuinely-`Err` path instead of the record-and-
        // recover one.
        let signature = match self
            .function_signature_uncached(function, cache_key.clone())
            .await
        {
            Ok(signature) => signature,
            Err(error) => {
                self.program_rc()
                    .cache_function_signature(cache_key, Ty::error());
                return Err(error);
            }
        };
        self.program_rc()
            .cache_function_signature(cache_key, signature.clone());
        Ok(signature)
    }

    async fn function_signature_uncached(
        &mut self,
        function: &hir::Function,
        cache_key: hir::HirId,
    ) -> Result<Ty> {
        let mut scope = self.with_generics(&function.sig.generics);
        let mut inputs = Vec::with_capacity(function.sig.inputs.len());
        for (index, input) in function.sig.inputs.iter().enumerate() {
            let ty = scope.check_type_expr(&input.ty).await?;
            if let Some(hint) = scope
                .program_rc()
                .take_raw_refinement_hint(input.ty.hir_id.clone())
            {
                scope.program_rc().insert_refinement_hint(
                    cache_key.clone(),
                    hir::ParamSlot::Input(index),
                    hint,
                );
            }
            inputs.push(Box::new(ty));
        }
        let output = Box::new(scope.check_type_expr(&function.sig.output).await?);
        if let Some(hint) = scope
            .program_rc()
            .take_raw_refinement_hint(function.sig.output.hir_id.clone())
        {
            scope.program_rc().insert_refinement_hint(
                cache_key.clone(),
                hir::ParamSlot::Output,
                hint,
            );
        }
        drop(scope);
        Ok(Ty {
            kind: TyKind::FnPtr(ty::PolyFnSig {
                binder: ty::Binder {
                    value: ty::FnSig {
                        inputs,
                        output,
                        c_variadic: false,
                        unsafety: ty::Unsafety::Normal,
                        abi: ty::Abi::Rust,
                    },
                    bound_vars: Vec::new(),
                },
            }),
        })
    }

    /// `check_type_expr(self_ty)` for an impl's own self-type declaration,
    /// memoized by its `HirId` (see `hir::HirPackage::checked_impl_self_ty_cache`'s
    /// doc comment) — same caching shape as `function_signature`, just for
    /// the self-type check `method_output_at`/`method_declared_signature_at`
    /// both do before ever looking at a call site's actual receiver.
    async fn checked_impl_self_ty(&mut self, self_ty: &hir::TypeExpr) -> Result<Ty> {
        let cache_key = self_ty.hir_id.clone();
        if let Some(cached) = self.program_rc().checked_impl_self_ty(cache_key.clone()) {
            return Ok(cached);
        }
        let checked = self.check_type_expr(self_ty).await?;
        self.program_rc()
            .cache_checked_impl_self_ty(cache_key, checked.clone());
        Ok(checked)
    }

    fn substitute_param_map_fn_sig(
        &self,
        sig: &ty::FnSig,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> ty::FnSig {
        ty::FnSig {
            inputs: sig
                .inputs
                .iter()
                .map(|input| Box::new(self.substitute_param_map(input, substitutions)))
                .collect(),
            output: Box::new(self.substitute_param_map(&sig.output, substitutions)),
            c_variadic: sig.c_variadic,
            unsafety: sig.unsafety,
            abi: sig.abi.clone(),
        }
    }

    /// Real rustc method resolution builds an "autoderef chain" — the
    /// receiver, then `*receiver`, `**receiver`, ... via the `Deref` trait
    /// — and looks for the method at each step, stopping at the first
    /// match. `&`/`&mut` referencing is already peeled at every call site
    /// via the `Ref`-stripping in `method_output_at`; this loop handles
    /// the *trait* (`Vec<T>` -> `[T]`, `Box<T>`/`Rc<T>`/`Arc<T>` -> `T`,
    /// ...) via `deref_target`. Bounded the same conservative way rustc
    /// bounds its own autoderef chain (a fixed small limit, not truly
    /// unbounded) — a real `Deref` chain this deep would be pathological.
    /// Resolves an unqualified `T::AssocName` associated-type projection
    /// (see `path_ty`'s call site) by searching every impl block for one
    /// whose self-type structurally matches `target_ty` and that declares
    /// an associated type named `assoc_name` — the same impl-candidate
    /// walk `method_output_at` performs for `.method()` calls, minus the
    /// method-signature matching (there is no `self`/argument list to
    /// check for an associated *type*).
    /// Resolves `T::AssocName` when `T` is still a bare, uninstantiated
    /// generic parameter (see `path_ty`'s call site) by consulting `T`'s
    /// own trait bounds directly, rather than searching for an impl on a
    /// concrete self-type (there isn't one yet). Two bound shapes:
    ///
    /// - `Fn`/`FnOnce`/`FnMut(..) -> R` sugar's own `Output` — fp-lang's
    ///   parser already folds this straight into a `TypeExprKind::FnPtr`,
    ///   discarding the trait name (see `GenericParam::bounds`'s own doc
    ///   comment), so this is read directly off the bound's own `output`.
    /// - An ordinary named-trait bound (`F: Fn<A>`, real `core::ops::
    ///   function`'s own `impl<A: Tuple, F: ?Sized> Fn<A> for &F where F:
    ///   Fn<A> { fn call(&self, args: A) -> F::Output { .. } }`) whose
    ///   trait declares (but doesn't itself bind) `AssocName` — this
    ///   checker has no per-instantiation projection tracking (it would
    ///   need to know the *caller's* concrete `F` to know `F::Output`'s
    ///   real value), so the best available answer is a fresh opaque
    ///   placeholder type: enough to stop this from being a hard
    ///   "unresolved type path" error and let type inference continue
    ///   past it, matching how an under-constrained projection is treated
    ///   everywhere else in this checker (`ty::ParamTy`-shaped, unified
    ///   against whatever the caller substitutes at each concrete use).
    ///
    /// An explicit associated-type binding in an ordinary trait bound
    /// (`I: Iterator<Item = U>`) is a separate, still-open gap — the
    /// binding survives parsing as an `Ident = Type` generic arg, but
    /// `transform_type_to_hir`'s general `ast::Ty::Expr` case has no
    /// `ExprKind::Assign` arm to carry it into HIR in the first place, so
    /// there's nothing yet for this function to read it back out of.
    async fn assoc_type_from_generic_param_bounds(
        &mut self,
        param_name: &hir::Symbol,
        assoc_name: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        if let Some(bound_ty) = self
            .generic_param_bindings(param_name)
            .and_then(|bindings| bindings.iter().find(|(name, _)| name == assoc_name))
            .map(|(_, ty)| ty.clone())
        {
            return Ok(Some(self.check_type_expr(&bound_ty).await?));
        }
        let Some(bounds) = self.generic_param_bounds(param_name).map(<[_]>::to_vec) else {
            return Ok(None);
        };
        if assoc_name.as_str() == "Output" {
            if let Some(fn_ptr) = bounds.iter().find_map(|bound| match &bound.kind {
                hir::TypeExprKind::FnPtr(fn_ptr) => Some(fn_ptr.clone()),
                _ => None,
            }) {
                return Ok(Some(self.check_type_expr(&fn_ptr.output).await?));
            }
        }
        for bound in &bounds {
            let hir::TypeExprKind::Path(path) = &bound.kind else {
                continue;
            };
            let Some(hir::Res::Def(trait_def_id)) = &path.res else {
                continue;
            };
            let Some(item) = self.package().def_map.get(trait_def_id).cloned() else {
                continue;
            };
            let hir::ItemKind::Trait(trait_def) = &item.kind else {
                continue;
            };
            let mut seen = HashSet::new();
            if self.trait_declares_assoc_type(trait_def, assoc_name, &mut seen) {
                return Ok(Some(Ty {
                    kind: TyKind::Param(ty::ParamTy {
                        index: u32::MAX,
                        name: assoc_name.clone(),
                    }),
                }));
            }
        }
        Ok(None)
    }

    /// Resolves `T::method_name(..)` where `T` is a still-generic type
    /// parameter currently in scope (`T::default()` with `T: Default`,
    /// e.g.) — the analogous case to `assoc_type_from_generic_param_bounds`
    /// but for a *method* rather than an associated type. There is no impl
    /// to search here (`T` is abstract, not a concrete receiver), so this
    /// walks the parameter's own trait bounds and returns the bound
    /// trait's declared signature directly, with `Self` resolving to the
    /// parameter's own `TyKind::Param` — deliberately does not require a
    /// method body the way `method_declared_signature_at`'s trait-default
    /// fallback does, since the call is only checked against the trait's
    /// abstract signature (real dispatch happens at monomorphization,
    /// which this checker doesn't perform).
    async fn generic_param_bound_method_signature(
        &mut self,
        param_name: &hir::Symbol,
        method_name: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        let Some(bounds) = self.generic_param_bounds(param_name).map(<[_]>::to_vec) else {
            return Ok(None);
        };
        let param_ty = Ty {
            kind: TyKind::Param(ty::ParamTy {
                index: u32::MAX,
                name: param_name.clone(),
            }),
        };
        for bound in &bounds {
            let hir::TypeExprKind::Path(path) = &bound.kind else {
                continue;
            };
            let Some(hir::Res::Def(trait_def_id)) = &path.res else {
                continue;
            };
            let Some(item) = self.program_rc().item(trait_def_id.clone()).cloned() else {
                continue;
            };
            let hir::ItemKind::Trait(trait_def) = &item.kind else {
                continue;
            };
            for trait_item in &trait_def.items {
                let hir::TraitItemKind::Method(function) = &trait_item.kind else {
                    continue;
                };
                if trait_item.name == *method_name {
                    let mut scope = self.with_self_type(param_ty.clone());
                    return Ok(Some(scope.function_signature(function).await?));
                }
            }
        }
        Ok(None)
    }

    /// Whether `trait_def` (or one of its supertraits, transitively —
    /// real `core::ops::function`'s own `Fn<Args>: FnMut<Args>: FnOnce
    /// <Args>` chain, where only `FnOnce` actually declares `type
    /// Output;`) declares an associated type named `assoc_name`. `seen`
    /// guards against a cyclic supertrait chain (never valid Rust, but
    /// this checker shouldn't hang on one anyway).
    fn trait_declares_assoc_type(
        &self,
        trait_def: &hir::Trait,
        assoc_name: &hir::Symbol,
        seen: &mut HashSet<hir::DefId>,
    ) -> bool {
        let declares_directly = trait_def.items.iter().any(|trait_item| {
            trait_item.name == *assoc_name
                && matches!(trait_item.kind, hir::TraitItemKind::AssocType(_))
        });
        if declares_directly {
            return true;
        }
        trait_def.supertraits.iter().any(|supertrait| {
            let Some(hir::Res::Def(supertrait_def_id)) = &supertrait.res else {
                return false;
            };
            if !seen.insert(supertrait_def_id.clone()) {
                return false;
            }
            let Some(item) = self.package().def_map.get(supertrait_def_id).cloned() else {
                return false;
            };
            let hir::ItemKind::Trait(supertrait_def) = &item.kind else {
                return false;
            };
            self.trait_declares_assoc_type(supertrait_def, assoc_name, seen)
        })
    }

    /// Ported onto the `HirPackage`-backed cache / `with_generics`-child
    /// architecture (this fix predates that rewrite; the original used the
    /// retired `TypingShared::assoc_type_for_self_cache` and a sync closure
    /// to guarantee `resolving_assoc_projections.pop()` ran on every return
    /// path — replaced here with a labeled block, since an async closure
    /// capturing `&mut self` across `.await` points isn't viable).
    async fn assoc_type_for_self(
        &mut self,
        target_ty: &Ty,
        assoc_name: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        let key = (format!("{:?}", target_ty.kind), assoc_name.clone());
        if let Some(cached) = self.package().assoc_type_for_self(&key) {
            return Ok(cached);
        }
        if self.resolving_assoc_projections.contains(&key) {
            return Ok(None);
        }
        self.resolving_assoc_projections.push(key.clone());
        // A still-generic target (`T::AssocName` where `T` is abstract)
        // has no impl to search — resolve straight from `T`'s own trait
        // bounds instead, the same way `path_ty` already does via
        // `assoc_type_from_generic_param_bounds` for the identical
        // underlying case reached through a type-relative path instead of
        // this function's own (`Self::AssocName`/UFCS-derived) callers.
        if let TyKind::Param(param) = &target_ty.kind {
            let result = self
                .assoc_type_from_generic_param_bounds(&param.name, assoc_name)
                .await;
            self.resolving_assoc_projections.pop();
            return result;
        }
        // An ADT target can only ever match an impl whose self-type also
        // resolves to `TyKind::Adt` with the same `did` — same fast-reject
        // reasoning as `method_output_at`'s own `impls_for_adt`/shape-
        // bucketed split.
        let receiver_def = match &target_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did.clone()),
            _ => None,
        };
        let program = self.program_rc();
        let candidates: Box<dyn Iterator<Item = &hir::Item> + '_> = match receiver_def {
            Some(def_id) => Box::new(program.impls_for_adt(def_id.clone())),
            None => Box::new(shape_and_blanket_candidates(&program, &target_ty.kind)),
        };
        let result: Result<Option<Ty>> = 'search: {
            for item in candidates {
                let hir::ItemKind::Impl(impl_item) = &item.kind else {
                    continue;
                };
                let mut scope = self.with_generics(&impl_item.generics);
                // A candidate whose *own* self-type fails to check at all
                // is not this projection's answer — skip it and keep
                // searching, the same "a rejected candidate is not a real
                // error" principle `unify_call_types_probe` already
                // applies to the compatibility check just below. Without
                // this, a `?` here would let one broken/irrelevant impl
                // elsewhere in the workspace (e.g. one whose own `type
                // Item = S::Item;` binding fails to resolve for unrelated
                // reasons) hard-fail *every other* unrelated item's own
                // typecheck the first time this search happens to reach
                // it, misattributing that impl's real problem to whatever
                // completely unrelated item triggered this search first.
                let Ok(checked_self_ty) = scope.checked_impl_self_ty(&impl_item.self_ty).await
                else {
                    continue;
                };
                let self_ty = match &checked_self_ty.kind {
                    TyKind::Ref(_, inner, _) => inner.as_ref(),
                    _ => &checked_self_ty,
                };
                let mut substitutions = HashMap::new();
                let matches = Self::ty_shapes_compatible(&self_ty.kind, &target_ty.kind)
                    && scope
                        .unify_call_types_probe(self_ty, target_ty, &mut substitutions)
                        .is_ok();
                if !matches {
                    continue;
                }
                // Same reasoning as `checked_impl_self_ty` above — a
                // matching candidate whose own associated-type bindings
                // fail to check isn't necessarily *this* projection's
                // fault; move on rather than hard-failing the whole
                // search (and misattributing the failure to whatever
                // unrelated item happened to trigger it).
                let Ok(assoc_types) = scope
                    .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                    .await
                else {
                    continue;
                };
                if let Some(ty) = assoc_types.get(assoc_name) {
                    break 'search Ok(Some(scope.substitute_param_map(ty, &substitutions)));
                }
            }
            Ok(None)
        };
        // Popped unconditionally (the labeled block above only ever
        // *breaks out* to here, on every path — found, not found, or
        // errored — never returns past it), matching the original
        // closure's guarantee.
        self.resolving_assoc_projections.pop();
        if let Ok(resolved) = &result {
            self.package()
                .cache_assoc_type_for_self(key, resolved.clone());
        }
        result
    }

    async fn method_output(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
        actuals: &[Ty],
    ) -> Result<(hir::DefId, Option<Vec<Ty>>, Ty)> {
        let mut current = receiver_ty.clone();
        for _ in 0..8 {
            if let Some(result) = self.method_output_at(&current, method, actuals).await? {
                return Ok(result);
            }
            match self.deref_target(&current).await {
                Some(target) => current = target,
                None => break,
            }
        }
        Err(Error::from(format!("method `{method}` was not found")))
    }

    async fn method_output_at(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
        actuals: &[Ty],
    ) -> Result<Option<(hir::DefId, Option<Vec<Ty>>, Ty)>> {
        let receiver_ty = match &receiver_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver_ty,
        };
        // A still-generic receiver (`self: T`, e.g. a default trait
        // method's own body calling `self.other_method()`) has no impl to
        // search — by far the most common non-ADT receiver shape in a
        // large real corpus (default-method bodies over generic `Self`),
        // so this is checked first, before ever touching the impl
        // candidate lists below. Resolved straight from `T`'s own trait
        // bounds, the same way `T::method(..)`'s type-relative-path
        // resolution already does via `generic_param_bound_method_
        // signature` — duplicated here (rather than reusing that helper
        // directly) because this call site additionally needs the
        // resolved method's own `DefId` and its instantiated generic
        // args/output against `actuals`, not just the bare signature.
        if let TyKind::Param(param) = &receiver_ty.kind {
            let Some(bounds) = self.generic_param_bounds(&param.name).map(<[_]>::to_vec) else {
                return Ok(None);
            };
            for bound in &bounds {
                let hir::TypeExprKind::Path(path) = &bound.kind else {
                    continue;
                };
                let Some(hir::Res::Def(trait_def_id)) = &path.res else {
                    continue;
                };
                let Some(item) = self.program_rc().item(trait_def_id.clone()).cloned() else {
                    continue;
                };
                let hir::ItemKind::Trait(trait_def) = &item.kind else {
                    continue;
                };
                for trait_item in &trait_def.items {
                    let hir::TraitItemKind::Method(function) = &trait_item.kind else {
                        continue;
                    };
                    if trait_item.name != *method {
                        continue;
                    }
                    let mut scope = self.with_self_type(receiver_ty.clone());
                    let signature = scope.function_signature(function).await?;
                    let Some((substitutions, result)) =
                        scope.instantiate_call(&signature, actuals)?
                    else {
                        return Err(Error::from("method arguments do not match its signature"));
                    };
                    let args = scope.method_generic_args(
                        &hir::Generics::default(),
                        &function.sig.generics,
                        &substitutions,
                    )?;
                    return Ok(Some((trait_item.def_id.clone(), args, result)));
                }
            }
            return Ok(None);
        }
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did.clone()),
            _ => None,
        };
        // An ADT receiver can only ever match an impl whose self-type also
        // resolves to `TyKind::Adt` with the same `did` (see the
        // `matches_receiver` match below) — go straight to that bucket via
        // `hir::HirProgram::impls_for_adt` instead of fully type-checking
        // every impl's self-type in the workspace. A concrete non-ADT
        // receiver (primitive/tuple/slice/etc.) uses the shape-bucketed
        // counterpart instead — either way, a bounded, indexed lookup,
        // never a scan over every impl in the workspace. `program` is
        // cloned out first so the borrow doesn't outlive the `&mut self`
        // calls below.
        let program = self.program_rc();
        let candidates: Box<dyn Iterator<Item = &hir::Item> + '_> = match &receiver_def {
            // A blanket impl (`impl<I: Iterator> IntoIterator for I { .. }`
            // — real vendored std's own idiom bridging `Iterator` into
            // `IntoIterator` for every concrete iterator adaptor struct)
            // applies to a concrete ADT receiver exactly as much as a
            // non-ADT one; excluding it here (as this branch previously
            // did unconditionally) meant a concrete Iterator-implementing
            // struct's `.into_iter()` — along with any other method only
            // ever provided through a blanket impl — could never resolve
            // at all.
            Some(def_id) => {
                Box::new(program.impls_for_adt(def_id.clone()).chain(program.blanket_impls()))
            }
            None => Box::new(shape_and_blanket_candidates(&program, &receiver_ty.kind)),
        };
        for item in candidates {
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            let mut scope = self.with_generics(&impl_item.generics);
            let checked_self_ty = scope.checked_impl_self_ty(&impl_item.self_ty).await?;
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let matches_receiver = match (receiver_def.clone(), &receiver_ty.kind, &self_ty.kind) {
                (
                    Some(receiver_def),
                    TyKind::Adt(_, receiver_args),
                    TyKind::Adt(impl_receiver, impl_args),
                ) => {
                    // Match the base ADT *and* its generic arguments — an
                    // `impl Vec<&str> { .. }`/`impl Vec<String> { .. }`
                    // specialization must only match the receiver it was
                    // actually written for, not every `Vec<T>` regardless
                    // of `T` (a still-generic impl like `impl<T> Vec<T>`
                    // always matches, since its own args are bare `Param`s
                    // with nothing concrete to mismatch against).
                    impl_receiver.did == receiver_def
                        && scope.generic_args_compatible(impl_args, receiver_args)
                }
                // A blanket impl's self-type is its own generic parameter
                // (`impl<I: Iterator> IntoIterator for I` checks to
                // `TyKind::Param`, never `TyKind::Adt`) — it structurally
                // matches any concrete ADT receiver, the same permissive
                // "no further shape to mismatch against" treatment the
                // `Vec<T>`-style still-generic-impl case above already
                // gets. `I`'s own trait bound (`Iterator`) isn't verified
                // here, matching how a blanket impl already isn't bound-
                // checked in the non-ADT branch below either.
                (Some(_), TyKind::Adt(_, _), TyKind::Param(_)) => true,
                (None, TyKind::Adt(_, _), _) => false,
                // General unification instead of strict structural
                // equality — a generic non-ADT impl (`impl<T> [T] { .. }`,
                // matched against a concrete `[(String, PathBuf, String)]`
                // receiver) needs its own `Param`s substituted the same
                // way a call argument would, not an exact-shape match
                // (which a still-generic impl could never satisfy).
                (None, _, _) => {
                    Self::ty_shapes_compatible(&self_ty.kind, &receiver_ty.kind)
                        && scope
                            .unify_call_types_probe(self_ty, receiver_ty, &mut HashMap::new())
                            .is_ok()
                }
                (Some(_), _, _) => false,
            };
            if !matches_receiver {
                continue;
            }
            let mut scope = scope.with_self_type(checked_self_ty);
            let assoc_types = scope
                .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                .await?;
            let mut scope = scope.with_assoc_types(assoc_types);
            let impl_generics = impl_item.generics.clone();
            for impl_item in &impl_item.items {
                let hir::ImplItemKind::Method(function) = &impl_item.kind else {
                    continue;
                };
                if impl_item.name == *method {
                    let signature = scope.function_signature(function).await?;
                    let Some((substitutions, result)) =
                        scope.instantiate_call(&signature, actuals)?
                    else {
                        return Err(Error::from("method arguments do not match its signature"));
                    };
                    let args = scope.method_generic_args(
                        &impl_generics,
                        &function.sig.generics,
                        &substitutions,
                    )?;
                    return Ok(Some((impl_item.def_id.clone(), args, result)));
                }
            }
            // Not redeclared in this impl's own items — if this is a
            // trait impl, the trait itself may provide a default-bodied
            // method by this name (`Iterator::map`/`filter_map`/etc. are
            // never redeclared per adaptor struct; only `next` typically
            // is). `scope.self_type`/`scope.assoc_types` are still the
            // ones set for *this* impl candidate above, so a default
            // method's `Self`/`Self::Item`-shaped signature substitutes
            // through exactly the same generic-instantiation machinery as
            // an inherent method's, with no separate mechanism needed.
            if let Some(trait_ty) = &impl_item.trait_ty {
                if let Some(trait_def) = scope.resolve_trait_def(trait_ty) {
                    for trait_item in &trait_def.items {
                        let hir::TraitItemKind::Method(function) = &trait_item.kind else {
                            continue;
                        };
                        // An abstract (no-body) trait method can never be
                        // a fallback signature source — if the impl
                        // doesn't redeclare it, that's a genuine "method
                        // not found" case, not something to paper over.
                        if trait_item.name == *method && function.body.is_some() {
                            let signature = scope.function_signature(function).await?;
                            let Some((substitutions, result)) =
                                scope.instantiate_call(&signature, actuals)?
                            else {
                                return Err(Error::from(
                                    "method arguments do not match its signature",
                                ));
                            };
                            let args = scope.method_generic_args(
                                &impl_generics,
                                &function.sig.generics,
                                &substitutions,
                            )?;
                            return Ok(Some((trait_item.def_id.clone(), args, result)));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    /// Resolves a trait impl's `trait_ty` (`impl Trait for X`'s `Trait`
    /// path) to its real `hir::Trait` definition — `None` if unresolved
    /// (an unknown/erroring path) or if the resolved item isn't actually a
    /// trait (shouldn't happen for a well-formed `trait_ty`, but this is a
    /// read path, not a validator — fail open rather than panic).
    fn resolve_trait_def(&self, trait_ty: &hir::TypeExpr) -> Option<Rc<hir::Trait>> {
        let hir::TypeExprKind::Path(path) = &trait_ty.kind else {
            return None;
        };
        let hir::Res::Def(def_id) = path.res.clone()? else {
            return None;
        };
        if let Some(cached) = self.program_rc().resolved_trait_def(def_id.clone()) {
            return Some(cached);
        }
        let program = self.program_rc();
        let item = program.item(def_id.clone())?;
        let hir::ItemKind::Trait(tr) = &item.kind else {
            return None;
        };
        let tr = Rc::new(tr.clone());
        self.program_rc()
            .cache_resolved_trait_def(def_id, tr.clone());
        Some(tr)
    }

    /// The real `Deref` *trait*'s effect on method resolution (distinct
    /// from the `&`/`&mut` reference-peeling every caller already does
    /// inline) — `Vec<T>` has no `iter`/`push`/etc. of its own; it only
    /// has these because `impl Deref for Vec<T> { type Target = [T]; }`
    /// lets `[T]`'s own inherent methods be called directly on a `Vec<T>`
    /// receiver. Returns that impl's `Target`, substituted with this
    /// receiver's own concrete generic arguments — `None` if no such impl
    /// exists (a non-ADT receiver, or an ADT with no `Deref` impl), which
    /// is exactly where real dereferencing stops too.
    async fn deref_target(&mut self, receiver_ty: &Ty) -> Option<Ty> {
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => receiver.did.clone(),
            _ => return None,
        };
        let program = self.program_rc();
        let candidates: Vec<hir::Item> = program.impls_for_adt(receiver_def).cloned().collect();
        for item in &candidates {
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            let Some(trait_ty) = &impl_item.trait_ty else {
                continue;
            };
            let hir::TypeExprKind::Path(path) = &trait_ty.kind else {
                continue;
            };
            if path.segments.last().map(|seg| seg.name.as_str()) != Some("Deref") {
                continue;
            }
            let mut scope = self.with_generics(&impl_item.generics);
            let Ok(checked_self_ty) = scope.checked_impl_self_ty(&impl_item.self_ty).await else {
                continue;
            };
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let mut substitutions = HashMap::new();
            if scope
                .unify_call_types_probe(self_ty, receiver_ty, &mut substitutions)
                .is_err()
            {
                continue;
            }
            let Ok(assoc_types) = scope
                .impl_assoc_types(&impl_item.items, impl_item.self_ty.hir_id.clone())
                .await
            else {
                continue;
            };
            let Some(target) = assoc_types.get(&hir::Symbol::new("Target")) else {
                continue;
            };
            return Some(scope.substitute_param_map(target, &substitutions));
        }
        None
    }

    fn method_generic_args(
        &self,
        impl_generics: &hir::Generics,
        method_generics: &hir::Generics,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Result<Option<Vec<Ty>>> {
        if impl_generics.params.is_empty() && method_generics.params.is_empty() {
            return Ok(None);
        }
        // This only ever fills in the *explicit* generic-argument list
        // recorded for codegen's benefit (`TypeckResults::generic_method_
        // args`) — the call expression's own resulting type was already
        // fully computed by `instantiate_call` before either caller ever
        // reaches this function. A generic parameter that only appears in
        // a position `instantiate_call`'s own unification never touched
        // (e.g. a method-level generic used solely in a `where` bound, or
        // meant to be inferred from an expected return type this checker
        // doesn't propagate here) is a real, common case, not a type
        // error — so failing to fill in the *list* must not fail the
        // *call*: fall back to `Ok(None)` ("no explicit args recorded")
        // instead of aborting the whole method resolution via `Err`, which
        // previously discarded an already-correct `result` type purely
        // because this auxiliary bookkeeping came up short.
        let mut args = Vec::new();
        for (index, parameter) in impl_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            // A hit that's itself still `Param` (transitively, after
            // `resolve_param_transitively` walks any chain) is not a real
            // resolution — e.g. bound to an enclosing scope's own,
            // not-yet-closed-over generic — and must fail the same way a
            // lookup miss does, not be silently returned as if it were a
            // concrete type (see `resolve_param_transitively`'s doc
            // comment).
            match self.resolve_param_transitively(&param, substitutions) {
                Some(argument) if !matches!(argument.kind, TyKind::Param(_)) => {
                    args.push(argument.clone());
                }
                _ => return Ok(None),
            }
        }
        for (index, parameter) in method_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            match self.resolve_param_transitively(&param, substitutions) {
                Some(argument) if !matches!(argument.kind, TyKind::Param(_)) => {
                    args.push(argument.clone());
                }
                _ => return Ok(None),
            }
        }
        Ok(Some(args))
    }

    fn bind_pattern<'a>(
        &'a mut self,
        pattern: &'a hir::Pat,
        ty: Ty,
    ) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            self.package()
                .record_pat_type(pattern.hir_id.clone(), ty.clone());
            // Match ergonomics: an ADT-shaped pattern (`Value::Null`, `Point {
            // x, y }`, a tuple) matches against a `&Value`/`&(A, B)` scrutinee
            // the same way it matches a bare one — e.g. matching on `self`
            // inside a `&self` method. Only peel the reference for these
            // structural shapes; `Binding`/`Wild`/`Lit` keep the original type
            // (a bound name under a `&T` scrutinee must still bind as `&T`).
            let adt_ty = match &pattern.kind {
                hir::PatKind::Tuple(_)
                | hir::PatKind::Struct(_, _, _)
                | hir::PatKind::TupleStruct(_, _)
                | hir::PatKind::Variant(_) => match &ty.kind {
                    TyKind::Ref(_, inner, _) => (**inner).clone(),
                    _ => ty.clone(),
                },
                _ => ty.clone(),
            };
            match &pattern.kind {
                hir::PatKind::Binding { name, .. } => {
                    self.locals.insert(name.clone(), ty);
                }
                hir::PatKind::Wild => {}
                hir::PatKind::Lit(lit) => {
                    let integer_literal = matches!(lit, hir::Lit::Integer(_));
                    let integer_ty = matches!(ty.kind, TyKind::Int(_) | TyKind::Uint(_));
                    if !(integer_literal && integer_ty) {
                        self.require_same(&ty, &self.literal_ty(lit))?;
                    }
                }
                hir::PatKind::Tuple(patterns) => {
                    let TyKind::Tuple(fields) = adt_ty.kind else {
                        self.record_error("tuple pattern requires a tuple scrutinee");
                        return Ok(());
                    };
                    if patterns.len() != fields.len() {
                        self.record_error("tuple pattern arity does not match scrutinee");
                        return Ok(());
                    }
                    for (pattern, field) in patterns.iter().zip(fields) {
                        self.bind_pattern(pattern, *field).await?;
                    }
                }
                hir::PatKind::Struct(path, fields, _) => {
                    if self.enum_variant_ty(path).await?.is_some() {
                        let (_, payloads) = self.variant_payload_types(path, &adt_ty).await?;
                        let [payload] = payloads.as_slice() else {
                            self.record_error(
                                "struct enum pattern requires exactly one payload type",
                            );
                            return Ok(());
                        };
                        for field in fields {
                            let field_ty = self.field_ty(payload, &field.name).await?;
                            self.bind_pattern(&field.pat, field_ty).await?;
                        }
                    } else {
                        let struct_ty = if path.segments.is_empty() {
                            adt_ty.clone()
                        } else {
                            self.path_ty(path).await?
                        };
                        self.require_same_adt(&adt_ty, &struct_ty, "struct pattern")?;
                        for field in fields {
                            let field_ty = self.field_ty(&struct_ty, &field.name).await?;
                            self.bind_pattern(&field.pat, field_ty).await?;
                        }
                    }
                }
                hir::PatKind::TupleStruct(path, patterns) => {
                    let (_, payloads) = self.variant_payload_types(path, &adt_ty).await?;
                    if patterns.len() != payloads.len() {
                        self.record_error("tuple struct pattern arity does not match variant");
                        return Ok(());
                    }
                    for (pattern, payload) in patterns.iter().zip(payloads) {
                        self.bind_pattern(pattern, payload).await?;
                    }
                }
                hir::PatKind::Variant(path) => {
                    let (_, payloads) = self.variant_payload_types(path, &adt_ty).await?;
                    if !payloads.is_empty() {
                        self.record_error("payload variant requires a tuple or struct pattern");
                    }
                }
            }
            Ok(())
        })
    }

    async fn field_ty(&mut self, receiver: &Ty, field: &hir::Symbol) -> Result<Ty> {
        let receiver = match &receiver.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver,
        };
        // A plain (non-`Adt`) tuple type has no struct fields, but its own
        // numeric field-access syntax (`.0`, `.1`, ...) reaches this same
        // `field_ty` call — HIR apparently has no separate "tuple index"
        // expression kind, just an ordinary `FieldAccess` whose `field`
        // happens to be a digit-string `Symbol`. Index into the tuple's
        // own element types directly rather than requiring a struct.
        if let TyKind::Tuple(elements) = &receiver.kind {
            if let Ok(index) = field.as_str().parse::<usize>() {
                if let Some(element) = elements.get(index) {
                    return Ok((**element).clone());
                }
            }
        }
        let TyKind::Adt(adt, args) = &receiver.kind else {
            return Ok(self.error_ty(format!(
                "field access `{field}` requires a struct, found {:?}",
                receiver.kind
            )));
        };
        if adt.flags.contains(AdtFlags::IS_COMPTIME_LOCAL) {
            // `path_ty`'s comptime-local-type-alias arm set this bit
            // itself, on this exact `Ty`, when it built it from a
            // comptime-evaluated local type alias — the field shapes it
            // recorded then are the only source of truth here, `def_map`
            // was never involved in producing this `Ty` at all.
            let Some(fields) = self.program_rc().local_struct_fields(adt.did.clone()) else {
                return Ok(self.error_ty("comptime-constructed struct's field shape was not found"));
            };
            let Some((_, field_ty)) = fields.iter().find(|(name, _)| name == field) else {
                return Ok(self.error_ty(format!("field `{field}` was not found")));
            };
            return Ok(field_ty.clone());
        }
        let Some(item) = self.program_rc().item(adt.did.clone()).cloned() else {
            return Ok(self.error_ty("struct definition was not found"));
        };
        let hir::ItemKind::Struct(def) = item.kind else {
            return Ok(self.error_ty("field access requires a struct"));
        };
        let Some(field_def) = def.fields.iter().find(|candidate| candidate.name == *field) else {
            return Ok(self.error_ty(format!("field `{field}` was not found")));
        };
        let mut scope = self.with_generics(&def.generics);
        let result = scope.check_type_expr(&field_def.ty).await;
        let ty = result?;
        let substituted = scope.substitute_params(ty, args);
        drop(scope);
        Ok(substituted)
    }

    async fn variant_payload_types(
        &mut self,
        path: &hir::Path,
        scrutinee: &Ty,
    ) -> Result<(Ty, Vec<Ty>)> {
        let Some(hir::Res::Def(ref variant_id)) = path.res else {
            return Ok((self.error_ty("variant pattern is unresolved"), Vec::new()));
        };
        if let Some((item, variant)) = self.enum_variant_by_def_id(variant_id.clone()) {
            let hir::ItemKind::Enum(def) = &item.kind else {
                unreachable!("enum_variant_by_def_id only returns enum variants");
            };
            // The variant path carries the constructor identity, not the
            // instantiated enum arguments. The scrutinee is the authoritative
            // enum type for generic variants such as `Option<T>::Some`.
            let enum_ty = scrutinee.clone();
            let matches_enum = matches!(
                &scrutinee.kind,
                TyKind::Adt(adt, _) if adt.did == item.def_id
            );
            if !matches_enum {
                let scrutinee_def = match &scrutinee.kind {
                    TyKind::Adt(adt, _) => format!("{}", adt.did),
                    _ => format!("<non-adt {:?}>", scrutinee.kind),
                };
                return Ok((
                    self.error_ty(format!(
                        "variant pattern does not match scrutinee type (variant={}, owner={}, scrutinee={})",
                        variant_id, item.def_id, scrutinee_def
                    )),
                    Vec::new(),
                ));
            }
            let scrutinee_args = match &scrutinee.kind {
                TyKind::Adt(_, args) => args,
                _ => unreachable!("variant pattern ADT was checked above"),
            };
            let Some(payload) = &variant.payload else {
                return Ok((enum_ty, Vec::new()));
            };
            let mut scope = self.with_generics(&def.generics);
            let payload_result = scope.check_type_expr(payload).await;
            let payload = payload_result?;
            let payload = scope.substitute_params(payload, scrutinee_args);
            drop(scope);
            let payloads = match payload.kind {
                TyKind::Tuple(fields) => fields.into_iter().map(|field| *field).collect(),
                _ => vec![payload],
            };
            return Ok((enum_ty, payloads));
        }
        Ok((
            self.error_ty("variant definition was not found"),
            Vec::new(),
        ))
    }

    async fn enum_struct_payload_type(
        &mut self,
        path: &hir::Path,
        scrutinee: &Ty,
    ) -> Result<Option<Ty>> {
        if self.enum_variant_ty(path).await?.is_none() {
            return Ok(None);
        }
        let (_, payloads) = self.variant_payload_types(path, scrutinee).await?;
        let Some(payload) = payloads.into_iter().next() else {
            return Ok(None);
        };
        let TyKind::Adt(adt, _) = &payload.kind else {
            return Ok(None);
        };
        if matches!(
            self.program_rc()
                .item(adt.did.clone())
                .map(|item| &item.kind),
            Some(hir::ItemKind::Struct(_))
        ) {
            Ok(Some(payload))
        } else {
            Ok(None)
        }
    }

    async fn enum_variant_ty(&mut self, path: &hir::Path) -> Result<Option<Ty>> {
        let Some(hir::Res::Def(ref variant_id)) = path.res else {
            return Ok(None);
        };
        let Some((item, _)) = self.enum_variant_by_def_id(variant_id.clone()) else {
            return Ok(None);
        };
        Ok(Some(self.enum_item_ty(&item, path).await?))
    }

    fn enum_variant_by_def_id(
        &self,
        variant_id: hir::DefId,
    ) -> Option<(hir::Item, hir::EnumVariant)> {
        // `enum_variant_item_index` is a direct `variant_id -> owning enum
        // item's DefId` lookup (maintained incrementally, see
        // `hir::HirPackage::add_item`), so this never scans package items to
        // find the owning enum.
        self.program
            .package(&variant_id.package_id)
            .and_then(|package| {
                let enum_def_id = package.enum_variant_item_index.get(&variant_id)?;
                package.def_map.get(enum_def_id)
            })
            .and_then(|item| {
                let hir::ItemKind::Enum(def) = &item.kind else {
                    return None;
                };
                def.variants
                    .iter()
                    .find(|variant| variant.def_id == variant_id)
                    .cloned()
                    .map(|variant| (item.clone(), variant))
            })
    }

    fn substitute_params(&self, ty: Ty, args: &[GenericArg]) -> Ty {
        match ty.kind {
            TyKind::Param(param) => match args.get(param.index as usize) {
                Some(GenericArg::Type(ty)) => ty.clone(),
                Some(GenericArg::Const(_) | GenericArg::Lifetime(_)) | None => Ty {
                    kind: TyKind::Param(param),
                },
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .into_iter()
                        .map(|field| Box::new(self.substitute_params(*field, args)))
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(Box::new(self.substitute_params(*inner, args)), length),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_params(*inner, args))),
            },
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region,
                    Box::new(self.substitute_params(*inner, args)),
                    mutable,
                ),
            },
            TyKind::RawPtr(mutability) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_params(*mutability.ty, args)),
                    mutbl: mutability.mutbl,
                }),
            },
            kind => Ty { kind },
        }
    }

    /* moved to intrinsics.rs
    async fn check_intrinsic(&mut self, call: &hir::IntrinsicCallExpr) -> Result<Ty> {
        use fp_core::intrinsics::IntrinsicKind;

        // `IntrinsicCallExpr.kind` is a `CallKind`, which also covers
        // `#[op(...)]`-tagged calls that AST-level recognition folded into
        // an `IntrinsicCall` (see `fp-core/src/intrinsics/calls.rs`'s
        // `CallKind::Op`/`CallKind::intrinsic_kind`). Most `OpKind` variants
        // are just the portable name for a genuine low-level intrinsic
        // (`OpKind::Println` == `IntrinsicKind::Println`, etc.) and
        // type-check exactly the same way, so resolve down to the
        // `IntrinsicKind` they share and run the ordinary rules below.
        // The few genuinely high-level ops with no intrinsic equivalent
        // (`Option`/`Result`/`Vec` constructors, `collect`, `find`, ...)
        // fall to `check_high_level_op` instead.
        // `CallKind::Op` was retired, so `intrinsic_kind()` is always `Some`
        // now — every `IntrinsicCallExpr` is a genuine low-level intrinsic.
        let kind = call
            .kind
            .intrinsic_kind()
            .expect("CallKind is always Intrinsic now that CallKind::Op is retired");
        // `sizeof!`/`field_count!`/`method_count!`'s single argument names
        // a *type* (a struct, or — inside a generic function/impl body —
        // the function's own type parameter, e.g. `sizeof!(T)` in
        // `Vec<T>::push`), not a value, even though it's parsed with
        // expression syntax. Their own result type is always `u64`
        // regardless of what the argument resolves to (matching the match
        // arm this used to fall into below), so — unlike every other
        // intrinsic here — checking the argument as an ordinary value
        // expression is both unnecessary and actively wrong: a bare type
        // parameter name has no value-namespace binding to resolve
        // against (`register_type_generic` in
        // `crates/fp-backend/src/transforms/ast_to_hir/items.rs` only
        // registers it in the type namespace) and would otherwise fail
        // with "unresolved value path".
        if matches!(
            kind,
            IntrinsicKind::SizeOf | IntrinsicKind::FieldCount | IntrinsicKind::MethodCount
        ) {
            return Ok(Ty::uint(ty::UintTy::U64));
        }
        let mut arg_types = Vec::with_capacity(call.callargs.len());
        for arg in &call.callargs {
            arg_types.push(self.check_expr(&arg.value).await?);
        }
        Ok(match kind {
            IntrinsicKind::Print | IntrinsicKind::Println => Ty {
                kind: TyKind::Tuple(Vec::new()),
            },
            // `panic!` diverges — type it `!` so it unifies with whatever
            // the surrounding context (a match arm, an `if`/`else` branch,
            // a function's declared return type) actually expects, exactly
            // like a genuinely unreachable expression already does.
            IntrinsicKind::Panic => Ty::never(),
            IntrinsicKind::Format => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::Len => Ty::uint(ty::UintTy::Usize),
            IntrinsicKind::Slice => match arg_types.first() {
                None => self.error_ty("slice intrinsic requires a base expression"),
                Some(base) => match &base.kind {
                    TyKind::Array(inner, _) | TyKind::Slice(inner) => Ty {
                        kind: TyKind::Slice(inner.clone()),
                    },
                    _ => self.error_ty("slice intrinsic base must be an array or slice"),
                },
            },
            IntrinsicKind::DebugAssertions
            | IntrinsicKind::FsExists
            | IntrinsicKind::FsIsDir
            | IntrinsicKind::FsIsFile
            | IntrinsicKind::EnvVarExists
            | IntrinsicKind::HasField
            | IntrinsicKind::HasMethod => Ty::bool(),
            IntrinsicKind::Input
            | IntrinsicKind::FsReadToString
            | IntrinsicKind::FsReadDir
            | IntrinsicKind::FsWalkDir
            | IntrinsicKind::FsGlob
            | IntrinsicKind::EnvCurrentDir
            | IntrinsicKind::EnvTempDir
            | IntrinsicKind::EnvHomeDir
            | IntrinsicKind::EnvVar
            | IntrinsicKind::PathJoin
            | IntrinsicKind::PathParent
            | IntrinsicKind::PathFileName
            | IntrinsicKind::PathExtension
            | IntrinsicKind::PathStem
            | IntrinsicKind::PathNormalize
            | IntrinsicKind::IoReadStdinToString
            | IntrinsicKind::YamlToJson
            | IntrinsicKind::JsonParse
            | IntrinsicKind::ProcMacroTokenStreamToString => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::PathIsAbsolute => Ty::bool(),
            IntrinsicKind::TimeNow => Ty::float(ty::FloatTy::F64),
            IntrinsicKind::CatchUnwind => Ty::bool(),
            IntrinsicKind::CatchUnwindResult => match arg_types.first().cloned() {
                None => self.error_ty("catch_unwind_result requires a callable argument"),
                Some(value) => Ty {
                    kind: TyKind::Tuple(vec![Box::new(Ty::bool()), Box::new(value)]),
                },
            },
            IntrinsicKind::Spawn | IntrinsicKind::Select => match arg_types.first() {
                Some(value) => value.clone(),
                None => self.error_ty(format!("{:?} intrinsic requires an argument", kind)),
            },
            IntrinsicKind::Join => {
                if arg_types.len() == 1 {
                    arg_types[0].clone()
                } else if arg_types.is_empty() {
                    self.error_ty("join intrinsic requires an argument")
                } else {
                    Ty {
                        kind: TyKind::Tuple(arg_types.into_iter().map(Box::new).collect()),
                    }
                }
            }
            IntrinsicKind::FieldNameAt
            | IntrinsicKind::TypeName
            | IntrinsicKind::ProcMacroTokenStreamFromStr => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::VecType => {
                self.error_ty("type-valued intrinsic has no HIR type representation")
            }
            // `type(x)`/`.field_type(name)` are reflection *queries* — they
            // report on a real, already-known type, so (unlike
            // `create_struct`/`addfield`/`build_type` below) their result
            // has a real, ordinary struct shape: `std::meta::TypeDescriptor`/
            // `FieldTypeDescriptor`, the same real structs any other value
            // of those types would be.
            IntrinsicKind::TypeOf => self
                .well_known_struct_ty("TypeDescriptor", Vec::new())
                .unwrap_or_else(|| self.error_ty("std::meta::TypeDescriptor is not declared")),
            IntrinsicKind::FieldType => self
                .well_known_struct_ty("FieldTypeDescriptor", Vec::new())
                .unwrap_or_else(|| self.error_ty("std::meta::FieldTypeDescriptor is not declared")),
            // `create_struct`/`addfield`/`build_type` construct or mutate a
            // type value (backing `TypeBuilder.ty: type`), consumed only by
            // the comptime interpreter (`Value::Type`) via dedicated
            // `ComptimeOp`s — same opaque-handle kind as the `type`
            // keyword's own surface annotation (`TypeExprKind::Type`).
            IntrinsicKind::CreateStruct
            | IntrinsicKind::AddField
            | IntrinsicKind::BuildType
            | IntrinsicKind::PrimitiveType => Ty { kind: TyKind::Type },
            IntrinsicKind::FsWriteString
            | IntrinsicKind::FsAppendString
            | IntrinsicKind::FsCreateDirAll
            | IntrinsicKind::FsRemoveFile
            | IntrinsicKind::FsRemoveDirAll
            | IntrinsicKind::IoWriteStdout
            | IntrinsicKind::IoWriteStderr
            | IntrinsicKind::TestCommandMockReset
            | IntrinsicKind::TestCommandMockPush
            | IntrinsicKind::TestCommandMockApply
            | IntrinsicKind::Sleep
            | IntrinsicKind::Yield
            | IntrinsicKind::CompileWarning => self.unit_ty(),
            IntrinsicKind::CompileError => {
                self.error_ty("compile_error intrinsic requested an error")
            }
            _ => self.error_ty(format!("intrinsic `{:?}` has no HIR type rule", kind)),
        })
    }

    }
    */

    /// A type mismatch is recorded as a diagnostic, not a hard error — this
    /// is called from dozens of sites across `check_expr`/`check_block`/etc
    /// via `?`, so making it non-fatal is what lets typechecking continue
    /// past an isolated mismatch anywhere in a package instead of the whole
    /// package's typecheck aborting on the first one found (previously the
    /// single highest-leverage source of "one hard error, every time").
    fn require_same(&self, lhs: &Ty, rhs: &Ty) -> Result<()> {
        if lhs == rhs || matches!(lhs.kind, TyKind::Never) || matches!(rhs.kind, TyKind::Never) {
            Ok(())
        } else {
            self.record_error(format!("HIR type mismatch: {lhs} and {rhs}"));
            Ok(())
        }
    }

    /// `require_same`, but with a real span attached — use at any call site
    /// that already has the offending expression's span in scope (e.g.
    /// `check_expr`, which always holds `expr: &hir::Expr`) so the
    /// diagnostic is locatable instead of a bare "HIR type mismatch: X and
    /// Y" with no file/line. Call sites with no span reachable without
    /// deeper plumbing (`unify_call_types`/`generic_args_compatible`, which
    /// are `Ty`-only, and `check_pat`, whose `hir::Pat` carries no span)
    /// still use the spanless `require_same` above.
    fn require_same_at(&self, lhs: &Ty, rhs: &Ty, span: fp_core::span::Span) -> Result<()> {
        if lhs == rhs || matches!(lhs.kind, TyKind::Never) || matches!(rhs.kind, TyKind::Never) {
            Ok(())
        } else {
            self.record_error_with_span(format!("HIR type mismatch: {lhs} and {rhs}"), span);
            Ok(())
        }
    }

    /// Discharge a refinement type's predicate against the value actually
    /// flowing into that position — `decide` (exact evaluation) first, then
    /// `omega` (linear-arithmetic decision procedure) for symbolic values.
    /// Like `require_same`, a failure is recorded as a diagnostic rather
    /// than a hard `Err`, so one bad refinement doesn't abort the whole
    /// item's check. See `crate::refinement` for the algorithm.
    /// Unlike `require_same` (which only records a soft diagnostic and lets
    /// the check continue), a refinement violation returns a hard `Err` —
    /// matching the sibling `let`-binding strict-equality check right above
    /// this call site (`ty_matches_with_infer_holes`), which is also a hard
    /// `Err`, not a recorded diagnostic. Some invocation paths (e.g. a
    /// single-file `--target interpret` compile) never inspect the soft
    /// diagnostics list at all, so a refinement violation that only used
    /// `record_error` would silently compile and run anyway — exactly as
    /// wrong as letting an ordinary type mismatch through.
    fn discharge_refinement(
        &self,
        hint: &crate::refinement::RefinementHint,
        value_expr: &hir::Expr,
    ) -> Result<()> {
        let hypotheses = crate::refinement::implicit_hypotheses(&hint.base, &hint.binder);
        match crate::refinement::discharge(&hint.binder, &hint.predicate, value_expr, &hypotheses) {
            crate::refinement::RefinementOutcome::ProvenTrue => Ok(()),
            crate::refinement::RefinementOutcome::ProvenFalse => Err(Error::from(format!(
                "refinement predicate violated at compile time: value does not satisfy `{} : {} // ...`",
                hint.binder, hint.base
            ))),
            crate::refinement::RefinementOutcome::Undecidable => Err(Error::from(
                "refinement predicate outside supported linear-arithmetic fragment \
                 (only comparisons, `+ - * /`, `&&`, literals, and variable references \
                 are supported)"
                    .to_string(),
            )),
        }
    }

    fn require_same_adt(&self, actual: &Ty, expected: &Ty, context: &str) -> Result<()> {
        let (TyKind::Adt(actual_def, actual_args), TyKind::Adt(expected_def, expected_args)) =
            (&actual.kind, &expected.kind)
        else {
            self.record_error(format!("{context} requires an ADT scrutinee"));
            return Ok(());
        };
        if actual_def.did != expected_def.did || actual_args.len() != expected_args.len() {
            self.record_error(format!("{context} does not match scrutinee type"));
            return Ok(());
        }
        let mut substitutions = HashMap::new();
        for (actual, expected) in actual_args.iter().zip(expected_args) {
            match (actual, expected) {
                (GenericArg::Type(actual), GenericArg::Type(expected)) => {
                    self.unify_call_types(expected, actual, &mut substitutions)?;
                }
                (actual, expected) if actual == expected => {}
                _ => {
                    self.record_error(format!("{context} does not match scrutinee type"));
                    return Ok(());
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
