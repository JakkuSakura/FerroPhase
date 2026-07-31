use fp_core::config;
use std::collections::{HashMap, HashSet};

pub mod runtime_types;
pub mod typing;
pub mod typing_context;
pub use typing_context::TypingContext;
pub use runtime_types::{materialize_type_with_hooks, type_from_value, TypeMaterializeHooks};
pub use typing::types::{
    ExprId, GenericMonorph, ResolvedName,
    ResolvedNameNamespace, ResolvedNameTable, TypingDiagnostic, TypingDiagnosticLevel,
    TypingOutcome,
};

use fp_core::ast::*;
use fp_core::ast::{AttributesExt, Ident, Name};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::{Error, Result};
use fp_core::module::path::{parse_path, resolve_item_path, ParsedPath, PathPrefix, QualifiedPath};
use fp_core::package::PackageCrate;
use fp_core::span::Span;
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;
// intrinsic and op kinds handled in submodules
fn typing_error(msg: impl Into<String>) -> Error {
    Error::from(msg.into())
}

/// A boxed, pinned future borrowing for lifetime `'a` -- the standard shape
/// for a recursive/mutually-recursive async fn's return type, since Rust
/// can't give a directly- or mutually-recursive `async fn` a statically
/// sized state machine (the recursive call's future would need to contain
/// itself). Used throughout the typing SCC (`typing::unify`,
/// `typing::infer_expr`, `typing::infer_stmt`, and this crate root) at
/// exactly the functions that call themselves -- everything else in that
/// call graph is a plain `async fn` with no heap allocation of its own.
pub(crate) type BoxFuture<'a, T> = std::pin::Pin<Box<dyn std::future::Future<Output = T> + 'a>>;

/// Minimal single-poll driver for tests and other standalone callers that
/// just want a typing/comptime result synchronously and know up front that
/// nothing in this call will genuinely suspend (no unloaded package, no
/// pending comptime value) -- real suspend/resume across an actual
/// `Poll::Pending` is the driver's job (`fp-compiler`'s `Executor`, which
/// owns the waker registries in `TypingContext` that make resuming
/// meaningful). This just polls once with a waker that panics if used,
/// since a `Waker` registered here would never be woken by anything.
pub fn block_on<F: std::future::Future>(fut: F) -> F::Output {
    use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

    fn no_wake(_: *const ()) {}
    fn clone_noop_waker(_: *const ()) -> RawWaker {
        RawWaker::new(std::ptr::null(), &VTABLE)
    }
    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone_noop_waker, no_wake, no_wake, |_| {});

    let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) };
    let mut cx = Context::from_waker(&waker);
    let mut fut = std::pin::pin!(fut);
    match fut.as_mut().poll(&mut cx) {
        Poll::Ready(value) => value,
        Poll::Pending => panic!(
            "fp_typing::block_on: future returned Poll::Pending -- this helper only supports \
             futures that resolve on the very first poll (tests / synchronous callers with no \
             real package or comptime suspension); drive genuinely suspending futures through \
             fp-compiler's Executor instead"
        ),
    }
}

pub(crate) type TypeVarId = usize;

fn attrs_has_name(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| match &attr.meta {
        AttrMeta::Path(path) => path.last().as_str() == name,
        AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
        AttrMeta::List(list) => list.name.last().as_str() == name,
    })
}

fn attrs_has_feature(attrs: &[Attribute], feature: &str) -> bool {
    for attr in attrs {
        let AttrMeta::List(AttrMetaList { name, items }) = &attr.meta else {
            continue;
        };
        if name.last().as_str() != "feature" {
            continue;
        }
        for item in items {
            if let AttrMeta::Path(path) = item {
                if path.last().as_str() == feature {
                    return true;
                }
            }
        }
    }
    false
}

fn detect_lossy_mode() -> bool {
    config::lossy_mode()
}

pub fn default_extern_prelude() -> HashSet<String> {
    ["std", "core", "alloc"]
        .into_iter()
        .map(|name| name.to_string())
        .collect()
}

fn make_std_task_future_ty(inner: Ty) -> Ty {
    let future_seg = ParameterPathSegment::new(Ident::new("Future"), vec![inner]);
    let path = ParameterPath::new(
        PathPrefix::Plain,
        vec![
            ParameterPathSegment::new(Ident::new("std"), vec![]),
            ParameterPathSegment::new(Ident::new("task"), vec![]),
            future_seg,
        ],
    );
    Ty::locator(Name::ParameterPath(path))
}

fn make_std_result_ty(ok: Ty, err: Ty) -> Ty {
    let result_seg = ParameterPathSegment::new(Ident::new("Result"), vec![ok, err]);
    let path = ParameterPath::new(
        PathPrefix::Plain,
        vec![
            ParameterPathSegment::new(Ident::new("std"), vec![]),
            ParameterPathSegment::new(Ident::new("result"), vec![]),
            result_seg,
        ],
    );
    Ty::locator(Name::ParameterPath(path))
}

fn std_error_ty() -> Ty {
    let path = Path::plain(vec![
        Ident::new("std"),
        Ident::new("error"),
        Ident::new("Error"),
    ]);
    Ty::locator(Name::Path(path))
}

fn std_result_inner_types(ty: &Ty) -> Option<(Ty, Ty)> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    if path.segments.len() == 1 {
        let result_seg = &path.segments[0];
        if result_seg.ident.as_str() != "Result" || result_seg.args.len() != 2 {
            return None;
        }
        return Some((result_seg.args[0].clone(), result_seg.args[1].clone()));
    }
    if path.segments.len() < 3 {
        return None;
    }
    let n = path.segments.len();
    let (prefix_a, prefix_b, result_seg) = (
        path.segments[n - 3].ident.as_str(),
        path.segments[n - 2].ident.as_str(),
        &path.segments[n - 1],
    );
    let is_std_result =
        prefix_a == "std" && prefix_b == "result" && result_seg.ident.as_str() == "Result";
    let is_fs_result =
        prefix_a == "std" && prefix_b == "fs" && result_seg.ident.as_str() == "Result";
    if !is_std_result && !is_fs_result {
        return None;
    }
    if result_seg.args.len() != 2 {
        return None;
    }
    Some((result_seg.args[0].clone(), result_seg.args[1].clone()))
}

fn is_std_result_ty(ty: &Ty) -> bool {
    std_result_inner_types(ty).is_some()
}

fn is_std_task_future_ty(ty: &Ty) -> bool {
    let Ty::Expr(expr) = ty else {
        return false;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return false;
    };
    if path.segments.len() < 3 {
        return false;
    }
    let n = path.segments.len();
    path.segments[n - 3].ident.as_str() == "std"
        && path.segments[n - 2].ident.as_str() == "task"
        && path.segments[n - 1].ident.as_str() == "Future"
        && path.segments[n - 1].args.len() == 1
}

fn std_task_future_inner_ty(ty: &Ty) -> Option<Ty> {
    let Ty::Expr(expr) = ty else {
        return None;
    };
    let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
        return None;
    };
    if path.segments.len() < 3 {
        return None;
    }
    let n = path.segments.len();
    if path.segments[n - 3].ident.as_str() != "std"
        || path.segments[n - 2].ident.as_str() != "task"
        || path.segments[n - 1].ident.as_str() != "Future"
        || path.segments[n - 1].args.len() != 1
    {
        return None;
    }
    Some(path.segments[n - 1].args[0].clone())
}

fn is_future_like_ty(ty: &Ty) -> bool {
    is_std_task_future_ty(ty)
        || matches!(ty, Ty::Struct(struct_ty) if struct_ty.name.as_str() == "Future")
}

pub trait TypeResolutionHook {
    fn resolve_symbol(&mut self, name: &str) -> bool;
    /// Try to resolve `expr` as a compile-time value right now, via scoped
    /// lowering and evaluation of just this expression. On success the
    /// implementation stores the value under `key` (and any other lookup
    /// keys the caller checks, e.g. `typing_ctx.expr_resolutions` for a
    /// `__fp_expr_<id>`-shaped key) and returns `true`, so the caller
    /// re-checks its resolved-value lookup. Returns `false` if genuinely
    /// blocked (e.g. a nested unresolved dependency) — the caller falls back
    /// to registering deferred scheduler work.
    fn request_comptime(&mut self, key: &str, expr: &Expr) -> bool;
}

use crate::typing::unify::{TypeVar, TypeVarKind};

#[derive(Clone, Debug)]
struct ImplContext {
    struct_name: QualifiedPath,
    self_ty: Ty,
}

#[derive(Clone)]
enum EnvEntry {
    Mono(TypeVarId),
    Poly(Ty),
}

struct PatternBinding {
    name: String,
    var: TypeVarId,
}

struct PatternInfo {
    var: TypeVarId,
    bindings: Vec<PatternBinding>,
}

impl PatternInfo {
    fn new(var: TypeVarId) -> Self {
        Self {
            var,
            bindings: Vec::new(),
        }
    }

    fn with_binding(mut self, name: String, var: TypeVarId) -> Self {
        self.bindings.push(PatternBinding { name, var });
        self
    }

    #[allow(dead_code)]
    fn extend_bindings(&mut self, other: PatternInfo) {
        self.bindings.extend(other.bindings);
    }
}

// Typing diagnostics/outcome are defined in typing/types.rs and re-exported above.

struct FunctionTypeInfo {
    params: Vec<TypeVarId>,
    ret: TypeVarId,
}

struct LoopContext {
    result_var: TypeVarId,
    saw_break: bool,
}

impl LoopContext {
    fn new(result_var: TypeVarId) -> Self {
        Self {
            result_var,
            saw_break: false,
        }
    }
}

#[derive(Clone)]
struct ContextBinding {
    ty: Ty,
    expr: Expr,
}

#[derive(Clone, Copy)]
enum ExceptionReturnPolicy {
    Disabled,
    ExplicitResult,
    AutoResult,
}

struct ExceptionContext {
    policy: ExceptionReturnPolicy,
}

/// Holds an `Rc<RefCell<Inner>>` clone; its `Drop` pops `exception_stack`
/// when the guarded scope ends. No `unsafe`/raw pointer needed (unlike the
/// pre-concurrency version this replaces) since `AstTypeInferencer` is now a
/// cheap `Clone`-able handle over shared interior-mutable state.
struct ExceptionContextGuard {
    inner: Rc<RefCell<Inner>>,
}

impl Drop for ExceptionContextGuard {
    fn drop(&mut self) {
        self.inner.borrow_mut().exception_stack.pop();
    }
}

/// The mutually-recursive SCC's own per-pass state — every field here is
/// reached only through `AstTypeInferencer::inner.borrow()`/`borrow_mut()`,
/// scoped to short synchronous stretches that never span an `.await` (the
/// same discipline already used for `TypingContext`'s `RefCell` fields
/// elsewhere in this crate). This split exists so that multiple concurrent
/// item-resolution tasks (see `typing_context::TypingContext::tasks`) can
/// each hold their own cheap `Rc::clone` of the same underlying state,
/// instead of requiring one exclusive `&mut AstTypeInferencer` per task.
///
/// No lifetime parameter (unlike an earlier iteration of this split): the
/// only field that ever needed one (`ctx: Option<&'ctx SharedScopedContext>`)
/// was dead -- nothing in the crate ever read it, it was a holdover from
/// before `TypingContext` existed -- and removing it, along with the now
///-unnecessary `+ 'ctx` bound on `resolution_hook`, lets `AstTypeInferencer`
/// be plain `Clone` + effectively `'static`, which `TypingContext::tasks`'
/// `Executor::spawn` (bound `+ 'static`) requires of anything it spawns.
struct Inner {
    type_vars: Vec<TypeVar>,
    env: Vec<HashMap<String, EnvEntry>>,
    generic_scopes: Vec<HashSet<String>>,
    enum_variants: HashMap<QualifiedPath, Vec<QualifiedPath>>,
    trait_method_sigs: HashMap<String, HashMap<String, FunctionSignature>>,
    extern_function_signatures: HashMap<QualifiedPath, FunctionSignature>,
    impl_traits: HashMap<QualifiedPath, HashSet<String>>,
    generic_trait_bounds: HashMap<TypeVarId, Vec<String>>,
    impl_stack: Vec<Option<ImplContext>>,
    module_path: QualifiedPath,
    module_defs: HashSet<QualifiedPath>,
    module_scope_depths: Vec<usize>,
    root_modules: HashSet<String>,
    extern_prelude: HashSet<String>,
    module_aliases: Vec<HashMap<String, QualifiedPath>>,
    symbol_aliases: Vec<HashMap<String, QualifiedPath>>,
    unimplemented_symbols: HashSet<QualifiedPath>,
    current_level: usize,
    diagnostics: Vec<TypingDiagnostic>,
    has_errors: bool,
    literal_ints: HashSet<TypeVarId>,
    loop_stack: Vec<LoopContext>,
    lossy_mode: bool,
    hashmap_args: HashMap<TypeVarId, (TypeVarId, TypeVarId)>,
    context_env: Vec<Vec<ContextBinding>>,
    exception_mode: bool,
    exception_stack: Vec<ExceptionContext>,
    current_span: Option<Span>,
    resolution_hook: Option<Box<dyn TypeResolutionHook>>,
    resolved_names: ResolvedNameTable,
    generic_type_vars: HashMap<TypeVarId, String>,
    /// Generic invocations with resolved concrete types ready for monomorphization.
    pending_generics: Vec<GenericMonorph>,
    /// Structs (and their `impl` blocks) resolved from a workspace crate
    /// rather than the local one — e.g. `std::meta::TypeBuilder` via
    /// `TypeBuilder::new(...)`. Reported out so the driver can predeclare
    /// the owning crate's impl items alongside the file's own when lowering
    /// to HIR, since MIR lowering's call-target resolution only ever sees
    /// impls declared in the same HIR program.
    cross_crate_struct_refs: HashSet<QualifiedPath>,
}

/// A cheap, `Clone`-able handle (an `Rc`-wrapped state, not a full copy).
/// `typing_ctx`/`own_crate` are already `Rc`(`<RefCell<_>>`)-based and read
/// constantly throughout the SCC, so they stay direct fields (no double
/// indirection through `inner`) — only the ~30 fields that were plain owned
/// collections before this conversion moved into `Inner`.
#[derive(Clone)]
pub struct AstTypeInferencer {
    /// Shared mutable state with the driver: resolved consts, types,
    /// module resolution, expression resolution, diagnostics, the
    /// package-waker registry that makes `await_package` genuinely suspend,
    /// and the shared task executor concurrent item-resolution runs on.
    typing_ctx: std::rc::Rc<crate::typing_context::TypingContext>,
    /// This crate's own registry of definitions — struct_defs, enum_defs,
    /// function_sigs, trait_defs — shared (via `Rc<RefCell<_>>`) with the
    /// root `WorkspaceContext`, which already holds every other crate the
    /// same way. Reads/writes go through `own_struct_defs[_mut]` etc. below;
    /// there is deliberately no separate local copy of this data — the
    /// "current crate" is just one more entry in the same root registry
    /// that cross-crate lookups (`env_ctx.find_struct`/`find_function_sig`)
    /// already search.
    own_crate: Rc<RefCell<PackageCrate>>,
    inner: Rc<RefCell<Inner>>,
}

impl AstTypeInferencer {
    // --- Reference pattern for the `&mut self` -> `&self` conversion ---
    // (established here; applied mechanically to the rest of the SCC below
    // and in `typing/{infer_expr,infer_stmt,unify,solver}.rs`):
    //
    // 1. `&mut self` becomes `&self` everywhere in this `impl` block (and
    //    the sibling `impl` blocks in the other 4 files).
    // 2. A read of a former plain field `self.foo` becomes
    //    `self.inner.borrow().foo` (often followed immediately by
    //    `.clone()`); a write `self.foo = x` / `self.foo.insert(..)` becomes
    //    a scoped `self.inner.borrow_mut()` — bind it to a `let mut inner =
    //    self.inner.borrow_mut();` only when several fields are touched in
    //    the same synchronous stretch, otherwise inline
    //    `self.inner.borrow_mut().foo = x;`.
    // 3. `self.typing_ctx`/`self.own_crate` (and thus `own_struct_defs()`
    //    etc.) are UNCHANGED — they stayed direct fields on the outer
    //    handle, not inside `Inner`, precisely so these extremely common
    //    call sites don't need to change at all.
    // 4. **Never** hold a `Ref`/`RefMut` guard (nor a `let inner = ...
    //    .borrow_mut();` binding) across an `.await` point. Extract
    //    `.clone()`d values out of the borrow first if they're needed after
    //    an `.await`; re-borrow fresh afterward if a write is needed then.
    // 5. Boxed self-recursive functions (`fn foo<'a>(&'a mut self, ...) ->
    //    BoxFuture<'a, T>`) drop the `'a` bound on `self` entirely — `self`
    //    is `Clone`, so `let this = self.clone();` before `Box::pin(async
    //    move { ... use `this` ... })` gives the async block its own owned
    //    handle. The `'a` lifetime, if still needed, now only bounds
    //    whatever *other* borrowed arguments (e.g. `items: &'a [Item]`) the
    //    function takes — never `self`.
    async fn register_qualified_symbol(&self, path: &QualifiedPath) -> TypeVarId {
        let key = path.to_key();
        if let Some(var) = self.lookup_env_var(&key).await {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    /// Boxed: recurses into itself for nested modules (see `BoxFuture`'s doc
    /// comment). `self` is cloned into the async block (see the reference
    /// pattern above) rather than borrowed, so only `items`/`prefix` bound
    /// the `'a` lifetime now.
    fn register_qualified_items<'a>(
        &self,
        items: &'a [Item],
        prefix: &'a QualifiedPath,
    ) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
        for item in items {
            match item.kind() {
                ItemKind::Module(module) => {
                    let next = prefix.with_segment(module.name.as_str().to_string());
                    let is_root = {
                        let mut inner = this.inner.borrow_mut();
                        inner.module_defs.insert(next.clone());
                        prefix.is_empty()
                    };
                    if is_root {
                        this.inner.borrow_mut().root_modules.insert(module.name.as_str().to_string());
                    }
                    this.register_qualified_items(&module.items, &next).await;
                }
                ItemKind::DefFunction(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.own_function_sigs_mut()
                        .insert(name.clone(), def.sig.clone());
                    let var = this.register_qualified_symbol(&name).await;
                    let saved = std::mem::replace(&mut this.inner.borrow_mut().module_path, prefix.clone());
                    this.prebind_function_signature(def, var).await;
                    this.inner.borrow_mut().module_path = saved;
                }
                ItemKind::DeclFunction(decl) => {
                    let name = prefix.with_segment(decl.name.as_str().to_string());
                    this.own_function_sigs_mut()
                        .insert(name.clone(), decl.sig.clone());
                    if decl.sig.abi.is_c() {
                        this.inner.borrow_mut().extern_function_signatures
                            .insert(name.clone(), decl.sig.clone());
                    }
                    let var = this.register_qualified_symbol(&name).await;
                    let saved = std::mem::replace(&mut this.inner.borrow_mut().module_path, prefix.clone());
                    this.prebind_decl_function_signature(decl, var).await;
                    this.inner.borrow_mut().module_path = saved;
                }
                ItemKind::DefConst(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefStatic(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefStruct(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.own_struct_defs_mut().insert(name.clone(), def.value.clone());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefStructural(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefEnum(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.own_enum_defs_mut().insert(name.clone(), def.value.clone());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefType(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::OpaqueType(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::DefTrait(def) => {
                    let name = prefix.with_segment(def.name.as_str().to_string());
                    this.own_trait_defs_mut().insert(name.clone());
                    this.register_qualified_symbol(&name).await;
                }
                ItemKind::Impl(impl_block) => {
                    if let Some(self_name) = impl_self_ty_name(&impl_block.self_ty) {
                        let struct_path = prefix.with_segment(self_name);
                        for child in &impl_block.items {
                            if let ItemKind::DefFunction(func) = child.kind() {
                                // Store on the struct for method lookup
                                if let Some(s) = this.own_struct_defs_mut().get_mut(&struct_path) {
                                    s.method_sigs.push((func.name.as_str().to_string(), func.sig.clone()));
                                }
                                // Also store as a function sig for ::call syntax
                                let fn_path = struct_path.with_segment(func.name.as_str().to_string());
                                this.own_function_sigs_mut().insert(fn_path.clone(), func.sig.clone());
                                this.register_qualified_symbol(&fn_path).await;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        })
    }

    /// Populate `module_defs` and `root_modules` from all known
    /// crates in the workspace so that import resolution can see
    /// module paths like `std::meta`. Also seeds `root_modules` from
    /// *registered* (not-yet-loaded) packages, so `use std::...`-style
    /// paths resolve to the right qualified path even before `std` is
    /// actually loaded — loading itself happens on demand (see
    /// `lookup_struct`/`lookup_function_signature_with_path`).
    pub fn seed_workspace_graph(&self) {
        let mut inner = self.inner.borrow_mut();
        for krate in self.typing_ctx.env_ctx.crates().values() {
            for path in &krate.borrow().module_paths {
                inner.module_defs.insert(path.clone());
                if let Some(head) = path.segments.first() {
                    inner.root_modules.insert(head.clone());
                }
            }
        }
        for name in self.typing_ctx.env_ctx.registered_names() {
            inner.root_modules.insert(name.to_string());
        }
    }

    /// Register pre-parsed items from an external module into the
    /// typer's lookup tables. Used when compiling dependency crates
    /// (e.g. std) whose items need to be available for name resolution.
    pub async fn inject_module(&self, path: &QualifiedPath, items: &[Item]) {
        {
            let mut inner = self.inner.borrow_mut();
            inner.module_defs.insert(path.clone());
            if path.segments.len() == 1 {
                inner.root_modules.insert(path.segments[0].clone());
            }
        }
        self.register_qualified_items(items, path).await;
    }

    /// Borrowed access to this crate's own registry — the "current crate"
    /// is just one more entry in the same root every other crate lives in;
    /// there's no separate local-vs-workspace branch anywhere. Each
    /// accessor's `Ref`/`RefMut` guard is a short-lived temporary (dropped
    /// at the end of the statement that uses it), matching how the plain
    /// `HashMap` fields these replace were always used.
    fn own_struct_defs(&self) -> Ref<'_, HashMap<QualifiedPath, TypeStruct>> {
        Ref::map(self.own_crate.borrow(), |k| &k.struct_defs)
    }
    fn own_struct_defs_mut(&self) -> RefMut<'_, HashMap<QualifiedPath, TypeStruct>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.struct_defs)
    }
    fn own_enum_defs(&self) -> Ref<'_, HashMap<QualifiedPath, TypeEnum>> {
        Ref::map(self.own_crate.borrow(), |k| &k.enum_defs)
    }
    fn own_enum_defs_mut(&self) -> RefMut<'_, HashMap<QualifiedPath, TypeEnum>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.enum_defs)
    }
    fn own_function_sigs(&self) -> Ref<'_, HashMap<QualifiedPath, FunctionSignature>> {
        Ref::map(self.own_crate.borrow(), |k| &k.function_sigs)
    }
    fn own_function_sigs_mut(&self) -> RefMut<'_, HashMap<QualifiedPath, FunctionSignature>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.function_sigs)
    }
    fn own_trait_defs(&self) -> Ref<'_, HashSet<QualifiedPath>> {
        Ref::map(self.own_crate.borrow(), |k| &k.trait_defs)
    }
    fn own_trait_defs_mut(&self) -> RefMut<'_, HashSet<QualifiedPath>> {
        RefMut::map(self.own_crate.borrow_mut(), |k| &mut k.trait_defs)
    }

    /// Use a specific shared crate entry (already registered in the root
    /// `WorkspaceContext`, e.g. via `env_ctx.begin_crate`) instead of the
    /// default standalone one `new()` creates — used when typing a
    /// freshly-loaded package (`CompilerDriver::load_package`), so its
    /// registry ends up in the same place every lookup already searches.
    pub fn with_own_crate(self, krate: Rc<RefCell<PackageCrate>>) -> Self {
        Self {
            own_crate: krate,
            ..self
        }
    }

    pub fn new(typing_ctx: std::rc::Rc<crate::typing_context::TypingContext>) -> Self {
        let inner = Inner {
            type_vars: Vec::new(),
            env: vec![HashMap::new()],
            generic_scopes: vec![HashSet::new()],
            enum_variants: HashMap::new(),
            trait_method_sigs: HashMap::new(),
            extern_function_signatures: HashMap::new(),
            impl_traits: HashMap::new(),
            generic_trait_bounds: HashMap::new(),
            impl_stack: Vec::new(),
            module_path: QualifiedPath::new(Vec::new()),
            module_defs: HashSet::new(),
            module_scope_depths: vec![0],
            root_modules: HashSet::new(),
            extern_prelude: default_extern_prelude(),
            module_aliases: vec![HashMap::new()],
            symbol_aliases: vec![HashMap::new()],
            current_level: 0,
            diagnostics: Vec::new(),
            has_errors: false,
            literal_ints: HashSet::new(),
            loop_stack: Vec::new(),
            lossy_mode: detect_lossy_mode(),
            hashmap_args: HashMap::new(),
            context_env: vec![Vec::new()],
            exception_mode: false,
            exception_stack: Vec::new(),
            current_span: None,
            resolution_hook: None,
            unimplemented_symbols: HashSet::new(),
            resolved_names: HashMap::new(),
            generic_type_vars: HashMap::new(),
            pending_generics: Vec::new(),
            cross_crate_struct_refs: HashSet::new(),
        };
        let inferencer = Self {
            typing_ctx,
            own_crate: Rc::new(RefCell::new(PackageCrate::default())),
            inner: Rc::new(RefCell::new(inner)),
        };
        inferencer.insert_default_prelude_aliases();
        inferencer
    }

    pub fn with_extern_prelude<I, S>(self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.set_extern_prelude(names);
        self
    }

    pub fn set_extern_prelude<I, S>(&self, names: I)
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut inner = self.inner.borrow_mut();
        inner.extern_prelude.clear();
        for name in names {
            inner.extern_prelude.insert(name.into());
        }
    }

    fn insert_default_prelude_aliases(&self) {
        if !self.inner.borrow().extern_prelude.contains("std") {
            return;
        }
        self.insert_prelude_symbol_alias("Result", &["std", "result", "Result"]);
        self.insert_prelude_symbol_alias("Option", &["std", "option", "Option"]);
        self.insert_prelude_symbol_alias("Ok", &["std", "result", "Ok"]);
        self.insert_prelude_symbol_alias("Err", &["std", "result", "Err"]);
        self.insert_prelude_symbol_alias("Some", &["std", "option", "Option", "Some"]);
        self.insert_prelude_symbol_alias("None", &["std", "option", "Option", "None"]);
    }

    fn insert_prelude_symbol_alias(&self, alias: &str, segments: &[&str]) {
        if let Some(scope) = self.inner.borrow_mut().symbol_aliases.first_mut() {
            let path = QualifiedPath::new(segments.iter().map(|seg| (*seg).to_string()).collect());
            scope.insert(alias.to_string(), path);
        }
    }

    pub fn set_resolution_hook(&self, hook: Box<dyn TypeResolutionHook>) {
        self.inner.borrow_mut().resolution_hook = Some(hook);
    }

    fn exception_policy_for_ret(&self, ret_ty: Option<&Ty>) -> ExceptionReturnPolicy {
        if !self.inner.borrow().exception_mode {
            return ExceptionReturnPolicy::Disabled;
        }
        match ret_ty {
            Some(ty) if is_std_result_ty(ty) => ExceptionReturnPolicy::ExplicitResult,
            Some(_) => ExceptionReturnPolicy::Disabled,
            None => ExceptionReturnPolicy::AutoResult,
        }
    }

    fn current_exception_policy(&self) -> ExceptionReturnPolicy {
        self.inner.borrow().exception_stack
            .last()
            .map(|ctx| ctx.policy)
            .unwrap_or(ExceptionReturnPolicy::Disabled)
    }

    fn push_exception_context(
        &self,
        policy: ExceptionReturnPolicy,
    ) -> ExceptionContextGuard {
        self.inner.borrow_mut().exception_stack.push(ExceptionContext { policy });
        ExceptionContextGuard {
            inner: self.inner.clone(),
        }
    }

    fn record_hashmap_args(
        &self,
        map_var: TypeVarId,
        key_var: TypeVarId,
        value_var: TypeVarId,
    ) {
        self.inner.borrow_mut().hashmap_args.insert(map_var, (key_var, value_var));
    }

    async fn lookup_hashmap_args(&self, map_var: TypeVarId) -> Option<(TypeVarId, TypeVarId)> {
        let mut current = map_var;
        loop {
            if let Some(args) = self.inner.borrow().hashmap_args.get(&current).copied() {
                return Some(args);
            }
            // Extracted into an owned local *before* matching: the match's
            // `Bound` arm awaits, and a `self.inner.borrow()` used directly
            // as the match scrutinee would otherwise have its guard's scope
            // extended across that `.await` (Rust extends a match
            // scrutinee's temporaries over the whole match).
            let kind = self.inner.borrow().type_vars.get(current).map(|var| var.kind.clone());
            match kind {
                Some(TypeVarKind::Link(next)) => current = next,
                Some(TypeVarKind::Bound(ty)) => {
                    if let Some(inner) = self.reference_inner_from_ty(&ty).await {
                        current = inner;
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }
    }

    pub async fn infer_file(&self, file: &mut File) -> Result<TypingOutcome> {
        self.infer_module_inner(
            &QualifiedPath::new(Vec::new()),
            &mut file.items,
            &file.attrs,
            &file.collected_items,
        )
        .await
    }

    /// Type-check a module's items directly, without an `ast::File` wrapper —
    /// used for on-demand compilation of workspace-crate modules (e.g.
    /// `std::meta`), where the driver already has `(QualifiedPath, Vec<Item>)`
    /// in hand and shouldn't need to synthesize a fake `File` just to satisfy
    /// this entrypoint.
    pub async fn infer_module(
        &self,
        module_path: &QualifiedPath,
        items: &mut Vec<Item>,
    ) -> Result<TypingOutcome> {
        self.infer_module_inner(module_path, items, &[], &[]).await
    }

    async fn infer_module_inner(
        &self,
        module_path: &QualifiedPath,
        items: &mut Vec<Item>,
        attrs: &[Attribute],
        collected_items: &[Item],
    ) -> Result<TypingOutcome> {
        let previous_exception = self.inner.borrow().exception_mode;
        self.inner.borrow_mut().exception_mode = attrs_has_feature(attrs, "exception");
        let saved_module_path = self.inner.borrow().module_path.clone();
        self.inner.borrow_mut().module_path = module_path.clone();
        self.register_qualified_items(items, module_path).await;
        self.predeclare_scope_items(collected_items).await;
        for item in items.iter_mut() {
            let result = self.infer_item_inner(item).await;
            result.map_err(|err| self.error_with_span(err, self.span_option(item.span())))?;
        }
        self.inner.borrow_mut().exception_mode = previous_exception;
        self.inner.borrow_mut().module_path = saved_module_path;
        Ok(self.finish().await)
    }

    pub async fn infer_item(&self, item: &mut Item) -> Result<TypingOutcome> {
        self.predeclare_item(item).await;
        match self.infer_item_inner(item).await {
            Ok(()) => {
                let ty = item.ty().cloned().unwrap_or_else(|| Ty::Unit(TypeUnit));
                item.set_ty(ty);
            }
            Err(err) => return Err(self.error_with_span(err, self.span_option(item.span()))),
        }
        Ok(self.finish().await)
    }

    pub async fn infer_expr(&self, expr: &mut Expr) -> Result<TypingOutcome> {
        self.predeclare_expr_scope(expr).await;
        let resolved = match self.infer_expr_inner(expr).await {
            Ok(var) => self.resolve_to_ty(var).await,
            Err(err) => Err(err),
        };
        match resolved {
            Ok(ty) => expr.set_ty(ty),
            Err(err) => return Err(self.error_with_span(err, self.span_option(expr.span()))),
        }
        Ok(self.finish().await)
    }

    /// Initialize the typer with declarations from a file without doing full inference.
    pub async fn initialize_from_file(&self, file: &File) {
        self.register_qualified_items(&file.items, &QualifiedPath::new(Vec::new())).await;
        self.predeclare_scope_items(&file.collected_items).await;
    }

    /// Initialize the typer with an expression scope without doing full inference.
    pub async fn initialize_from_expr(&self, expr: &Expr) {
        self.predeclare_expr_scope(expr).await;
    }

    /// Initialize import aliases without running full inference.
    pub async fn initialize_imports_from_file(&self, file: &File) {
        self.register_import_aliases_for_items(&file.items).await;
    }

    /// Initialize import aliases from a single item.
    pub async fn initialize_imports_from_item(&self, item: &Item) {
        self.register_import_aliases_for_item(item).await;
    }

    /// Boxed: mutually recursive with `register_import_aliases_for_item` for
    /// nested modules/impls/traits (see `BoxFuture`'s doc comment). `self` is
    /// cloned into the async block (see the reference pattern established at
    /// `register_qualified_items`) rather than borrowed, so only `items`
    /// bounds the `'a` lifetime now.
    fn register_import_aliases_for_items<'a>(&self, items: &'a [Item]) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
            for item in items {
                this.register_import_aliases_for_item(item).await;
            }
        })
    }

    async fn register_import_aliases_for_item(&self, item: &Item) {
        match item.kind() {
            ItemKind::Import(import) => self.register_import_aliases(import).await,
            ItemKind::Module(module) => {
                self.register_import_aliases_for_items(&module.items).await
            }
            ItemKind::Impl(impl_block) => {
                self.register_import_aliases_for_items(&impl_block.items).await;
            }
            ItemKind::DefTrait(def) => {
                self.register_import_aliases_for_items(&def.items).await;
            }
            _ => {}
        }
    }

    /// Initialize the typer with a single item for incremental typing.
    pub async fn initialize_from_item(&self, item: &Item) {
        self.predeclare_item(item).await;
    }

    /// Boxed: `predeclare_item` predeclares a nested module/impl's own scope
    /// by calling back into `predeclare_scope_items`, so the two are
    /// mutually recursive -- this is the half of the cycle that needs the
    /// heap indirection (see `BoxFuture`'s doc comment). `self` is cloned
    /// into the async block rather than borrowed, so only `items` bounds
    /// the `'a` lifetime now.
    fn predeclare_scope_items<'a>(&self, items: &'a [Item]) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
            for item in items {
                this.predeclare_item(item).await;
            }
        })
    }

    async fn predeclare_expr_scope(&self, expr: &Expr) {
        match expr.kind() {
            ExprKind::Block(block) => self.predeclare_scope_items(&block.collected_items).await,
            ExprKind::Quote(quote) => self.predeclare_scope_items(&quote.collected_items).await,
            ExprKind::ConstBlock(block) => {
                self.predeclare_scope_items(&block.collected_items).await
            }
            ExprKind::Item(item) => self.predeclare_item(item.as_ref()).await,
            _ => {}
        }
    }

    /// A pure name/dependency scan over `expr` -- collects bare `Name`/`Path`
    /// references that name a const/type-alias this pass hasn't resolved yet
    /// (checked against `resolved_consts`/`resolved_types`). Used by
    /// `await_comptime` to force every dependency *before* attempting
    /// resolution, so that attempt only ever needs to run once -- no retry
    /// loop. Mirrors the shape of `resolve_comptime_now`'s own
    /// `inline_resolved_names` walk in `fp-compiler`, but only collects
    /// candidates rather than substituting them.
    fn comptime_dependency_names(&self, expr: &Expr) -> Vec<String> {
        let mut names = Vec::new();
        self.collect_comptime_dependency_names(expr, &mut names);
        names
    }

    fn collect_comptime_dependency_names(&self, expr: &Expr, out: &mut Vec<String>) {
        let already_resolved = |name: &str| {
            self.typing_ctx.resolved_consts.borrow().contains_key(name)
                || self.typing_ctx.resolved_types.borrow().contains_key(name)
        };
        if let ExprKind::Name(locator) = expr.kind() {
            let name = locator.to_string();
            if !already_resolved(&name) {
                out.push(name);
            }
        }
        match expr.kind() {
            ExprKind::Struct(s) => {
                for field in &s.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.collect_comptime_dependency_names(value, out);
                    }
                }
            }
            ExprKind::Tuple(t) => {
                for value in &t.values {
                    self.collect_comptime_dependency_names(value, out);
                }
            }
            ExprKind::Array(a) => {
                for value in &a.values {
                    self.collect_comptime_dependency_names(value, out);
                }
            }
            ExprKind::BinOp(b) => {
                self.collect_comptime_dependency_names(&b.lhs, out);
                self.collect_comptime_dependency_names(&b.rhs, out);
            }
            ExprKind::UnOp(u) => self.collect_comptime_dependency_names(&u.val, out),
            ExprKind::Cast(c) => self.collect_comptime_dependency_names(&c.expr, out),
            ExprKind::Invoke(invoke) => {
                for arg in &invoke.args {
                    self.collect_comptime_dependency_names(arg, out);
                }
            }
            ExprKind::If(if_expr) => {
                self.collect_comptime_dependency_names(&if_expr.cond, out);
                self.collect_comptime_dependency_names(&if_expr.then, out);
                if let Some(elze) = if_expr.elze.as_ref() {
                    self.collect_comptime_dependency_names(elze, out);
                }
            }
            ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    match stmt {
                        BlockStmt::Expr(e) => self.collect_comptime_dependency_names(&e.expr, out),
                        BlockStmt::Let(s) => {
                            if let Some(init) = s.init.as_ref() {
                                self.collect_comptime_dependency_names(init, out);
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    /// Ensures `key`'s compile-time value is resolved, forcing whatever
    /// other const/type-alias items `expr` depends on first (each of which
    /// is its own independently-spawned task -- see
    /// `TypingContext::tasks`/`predeclare_item`), and returns the resolved
    /// `Value` directly -- callers do `let value = self.await_comptime(&key,
    /// expr).await?;`, not a separate map lookup after the fact. Genuinely
    /// suspends via a real `Waker` (not a retry loop) when the value isn't
    /// ready yet; resumes precisely when the resolving task writes it and
    /// calls `TypingContext::wake_comptime`.
    async fn await_comptime(&self, key: &str, expr: &Expr) -> Result<Value> {
        for name in self.comptime_dependency_names(expr) {
            self.force(&name).await?;
        }
        if let Some(value) = self.typing_ctx.resolved_consts.borrow().get(key).cloned() {
            return Ok(value);
        }
        let resolved = self
            .inner
            .borrow_mut()
            .resolution_hook
            .as_mut()
            .map(|hook| hook.request_comptime(key, expr))
            .unwrap_or(false);
        if resolved {
            if let Some(value) = self.typing_ctx.resolved_consts.borrow().get(key).cloned() {
                return Ok(value);
            }
        }
        // Genuinely not resolvable synchronously (no hook, or the hook
        // itself failed for a reason other than a missing dependency we
        // could force) -- suspend for real. Some other task's write to
        // `resolved_consts` (via `TypingContext::wake_comptime`) is what
        // would ever make this ready; if nothing ever does, this compile
        // unit's driver-level drive loop reports it as a genuine deadlock.
        let typing_ctx = self.typing_ctx.clone();
        let key_owned = key.to_string();
        std::future::poll_fn(move |cx| {
            if typing_ctx.resolved_consts.borrow().contains_key(&key_owned) {
                return std::task::Poll::Ready(());
            }
            typing_ctx
                .comptime_wakers
                .borrow_mut()
                .entry(key_owned.clone())
                .or_default()
                .push(cx.waker().clone());
            std::task::Poll::Pending
        })
        .await;
        self.typing_ctx
            .resolved_consts
            .borrow()
            .get(key)
            .cloned()
            .ok_or_else(|| typing_error(format!("could not resolve comptime value for `{key}`")))
    }

    /// Same shape as `await_comptime`, but for a `type Foo = const { ... }`
    /// alias's resolved struct shape rather than a plain value.
    async fn await_struct_alias(&self, name: &str) -> Result<TypeStruct> {
        if let Some(s) = self.typing_ctx.resolved_types.borrow().get(name).cloned() {
            return Ok(s);
        }
        self.force(name).await?;
        if let Some(s) = self.typing_ctx.resolved_types.borrow().get(name).cloned() {
            return Ok(s);
        }
        let typing_ctx = self.typing_ctx.clone();
        let name_owned = name.to_string();
        std::future::poll_fn(move |cx| {
            if typing_ctx.resolved_types.borrow().contains_key(&name_owned) {
                return std::task::Poll::Ready(());
            }
            typing_ctx
                .comptime_wakers
                .borrow_mut()
                .entry(name_owned.clone())
                .or_default()
                .push(cx.waker().clone());
            std::task::Poll::Pending
        })
        .await;
        self.typing_ctx
            .resolved_types
            .borrow()
            .get(name)
            .cloned()
            .ok_or_else(|| typing_error(format!("`{name}` did not resolve to a struct type")))
    }

    /// Ensure `name`'s value/struct-shape is resolved, waiting on its
    /// independently-spawned task (see `predeclare_item`) if one was ever
    /// spawned for it. Not spawning a task for `name` at all (it isn't a
    /// known const/type-alias in this compile unit) just means there's
    /// nothing to wait for here -- the caller's own subsequent lookup will
    /// report the real "not found" error.
    ///
    /// This waits on `resolved_consts`/`resolved_types` directly (the same
    /// `comptime_wakers` channel `await_comptime`'s own tail uses) rather
    /// than reaching into the executor's task-completion state -- the
    /// *task* finishing and the *value* landing happen together (the task
    /// body's own `await_comptime` call is what writes the value and wakes
    /// this), so there's no need for a separate "await this task" primitive.
    async fn force(&self, name: &str) -> Result<()> {
        if self.typing_ctx.resolved_consts.borrow().contains_key(name)
            || self.typing_ctx.resolved_types.borrow().contains_key(name)
        {
            return Ok(());
        }
        if !self.typing_ctx.tasks.contains(name) {
            return Ok(());
        }
        let typing_ctx = self.typing_ctx.clone();
        let name_owned = name.to_string();
        std::future::poll_fn(move |cx| {
            if typing_ctx.resolved_consts.borrow().contains_key(&name_owned)
                || typing_ctx.resolved_types.borrow().contains_key(&name_owned)
            {
                return std::task::Poll::Ready(());
            }
            typing_ctx
                .comptime_wakers
                .borrow_mut()
                .entry(name_owned.clone())
                .or_default()
                .push(cx.waker().clone());
            std::task::Poll::Pending
        })
        .await;
        Ok(())
    }

    async fn finish(&self) -> TypingOutcome {
        let (outcome, diags) = {
            let mut inner = self.inner.borrow_mut();
            let outcome = TypingOutcome {
                resolved_names: std::mem::take(&mut inner.resolved_names),
                pending_generics: std::mem::take(&mut inner.pending_generics),
                cross_crate_struct_refs: std::mem::take(&mut inner.cross_crate_struct_refs)
                    .into_iter()
                    .collect(),
            };
            let diags = std::mem::take(&mut inner.diagnostics);
            (outcome, diags)
        };
        self.typing_ctx.diagnostics.borrow_mut().extend(diags);
        outcome
    }

    fn expr_id(&self, expr: &Expr) -> ExprId {
        expr.id()
    }

    fn record_resolved_name(&self, expr_id: ExprId, resolved_name: ResolvedName) {
        self.inner.borrow_mut().resolved_names.insert(expr_id, resolved_name);
    }

    fn validate_struct_recursion(&self, name: &str, fields: &[StructuralField]) {
        let mut visiting = HashSet::new();
        for field in fields {
            let mut path = vec![field.name.as_str().to_string()];
            if let Some((path_str, chain)) = self.contains_illegal_struct_recursion(
                &field.value,
                name,
                false,
                &mut visiting,
                &mut path,
            ) {
                let location = if path_str.is_empty() {
                    "field".to_string()
                } else {
                    format!("field {}", path_str)
                };
                let chain = if chain.is_empty() {
                    String::new()
                } else {
                    let mut cycle = chain.clone();
                    if let Some(first) = chain.first() {
                        cycle.push(first.clone());
                    }
                    format!(" (cycle: {})", cycle.join(" -> "))
                };
                self.emit_error_with_span(
                    self.span_option(field.span()),
                    format!(
                        "recursive struct {} {} must use heap indirection (Box/Arc/Rc/Weak/Vec/&/Box<dyn ...>){}",
                        name, location, chain
                    ),
                );
            }
        }
    }

    fn contains_illegal_struct_recursion(
        &self,
        ty: &Ty,
        target: &str,
        heap_wrapped: bool,
        visiting: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> Option<(String, Vec<String>)> {
        if heap_wrapped {
            return None;
        }

        if let Some(inner) = self.heap_inner_ty(ty) {
            return self.contains_illegal_struct_recursion(inner, target, true, visiting, path);
        }

        match ty {
            Ty::Struct(struct_ty) => {
                let name = struct_ty.name.as_str();
                if name == target {
                    return Some((path.join("."), vec![target.to_string()]));
                }
                if !visiting.insert(name.to_string()) {
                    return None;
                }
                let result = struct_ty.fields.iter().find_map(|field| {
                    path.push(field.name.as_str().to_string());
                    let found = self.contains_illegal_struct_recursion(
                        &field.value,
                        target,
                        false,
                        visiting,
                        path,
                    );
                    path.pop();
                    found
                });
                visiting.remove(name);
                result
            }
            Ty::Structural(structural) => structural.fields.iter().find_map(|field| {
                path.push(field.name.as_str().to_string());
                let found = self.contains_illegal_struct_recursion(
                    &field.value,
                    target,
                    false,
                    visiting,
                    path,
                );
                path.pop();
                found
            }),
            Ty::Tuple(tuple) => tuple.types.iter().find_map(|elem| {
                path.push("tuple".to_string());
                let found =
                    self.contains_illegal_struct_recursion(elem, target, false, visiting, path);
                path.pop();
                found
            }),
            Ty::Vec(vec) => {
                self.contains_illegal_struct_recursion(&vec.ty, target, false, visiting, path)
            }
            Ty::Array(array) => {
                self.contains_illegal_struct_recursion(&array.elem, target, false, visiting, path)
            }
            Ty::Slice(slice) => {
                self.contains_illegal_struct_recursion(&slice.elem, target, false, visiting, path)
            }
            Ty::Reference(reference) => {
                self.contains_illegal_struct_recursion(&reference.ty, target, false, visiting, path)
            }
            Ty::Function(function) => {
                for param in &function.params {
                    if let Some(found) =
                        self.contains_illegal_struct_recursion(param, target, false, visiting, path)
                    {
                        return Some(found);
                    }
                }
                function.ret_ty.as_ref().and_then(|ret| {
                    self.contains_illegal_struct_recursion(ret, target, false, visiting, path)
                })
            }
            Ty::TypeBinaryOp(op) => self
                .contains_illegal_struct_recursion(&op.lhs, target, false, visiting, path)
                .or_else(|| {
                    self.contains_illegal_struct_recursion(&op.rhs, target, false, visiting, path)
                }),
            Ty::Expr(expr) => {
                let ExprKind::Name(locator) = expr.kind() else {
                    return None;
                };
                if let Some(inner) = self.heap_inner_ty(ty) {
                    return self
                        .contains_illegal_struct_recursion(inner, target, true, visiting, path);
                }
                let Some(name) = self.locator_tail_name(locator) else {
                    return None;
                };
                if name == target {
                    return Some((path.join("."), vec![target.to_string()]));
                }
                let direct = QualifiedPath::new(vec![name.clone()]);
                let def = if let Some(def) = self.own_struct_defs().get(&direct) {
                    def.clone()
                } else if let Some(def) = self.typing_ctx.env_ctx.find_struct(&direct) {
                    def
                } else {
                    let mut match_def = None;
                    for (key, def) in self.own_struct_defs().iter() {
                        if key.tail() == Some(name.as_str()) {
                            if match_def.is_some() {
                                return None;
                            }
                            match_def = Some(def.clone());
                        }
                    }
                    // Also check workspace
                    if match_def.is_none() {
                        for krate in self.typing_ctx.env_ctx.crates().values() {
                            for (key, def) in &krate.borrow().struct_defs {
                                if key.tail() == Some(name.as_str()) {
                                    if match_def.is_some() {
                                        return None;
                                    }
                                    match_def = Some(def.clone());
                                }
                            }
                        }
                    }
                    let Some(def) = match_def else {
                        return None;
                    };
                    def
                };
                if !visiting.insert(name.clone()) {
                    return None;
                }
                let result = def.fields.iter().find_map(|field| {
                    path.push(field.name.as_str().to_string());
                    let found = self.contains_illegal_struct_recursion(
                        &field.value,
                        target,
                        false,
                        visiting,
                        path,
                    );
                    path.pop();
                    found.map(|(path_str, mut chain)| {
                        chain.insert(0, name.clone());
                        (path_str, chain)
                    })
                });
                visiting.remove(&name);
                result
            }
            _ => None,
        }
    }

    fn heap_inner_ty<'a>(&self, ty: &'a Ty) -> Option<&'a Ty> {
        match ty {
            Ty::Reference(reference) => Some(&reference.ty),
            Ty::Vec(vec) => Some(&vec.ty),
            Ty::Expr(expr) => {
                let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
                    return None;
                };
                let segment = path.segments.last()?;
                if segment.args.len() != 1 {
                    return None;
                }
                match segment.ident.as_str() {
                    "Box" | "Arc" | "Rc" | "Weak" | "Vec" => Some(&segment.args[0]),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn struct_name_variants(&self, name: &str) -> Vec<QualifiedPath> {
        let parsed = parse_path(name).ok();
        let (segments, is_unqualified) = match parsed {
            Some(parsed) => {
                let is_unqualified =
                    parsed.prefix == PathPrefix::Plain && parsed.segments.len() == 1;
                (parsed.segments, is_unqualified)
            }
            None => (vec![name.to_string()], true),
        };
        let name_path = QualifiedPath::new(segments);
        self.struct_name_variants_for_path(&name_path, is_unqualified)
    }

    fn struct_name_variants_for_path(
        &self,
        name_path: &QualifiedPath,
        is_unqualified: bool,
    ) -> Vec<QualifiedPath> {
        // A single shared borrow for this whole (synchronous, no re-entrant
        // `self` calls) function -- simpler than repeating `self.inner
        // .borrow()` at each of the several `module_path`/`root_modules`
        // reads below.
        let inner = self.inner.borrow();
        let mut names = Vec::new();
        let mut seen = HashSet::new();
        let push = |value: QualifiedPath,
                    names: &mut Vec<QualifiedPath>,
                    seen: &mut HashSet<QualifiedPath>| {
            if seen.insert(value.clone()) {
                names.push(value);
            }
        };

        if !inner.module_path.is_empty() && is_unqualified {
            if let Some(head) = name_path.head() {
                push(
                    inner.module_path.with_segment(head.to_string()),
                    &mut names,
                    &mut seen,
                );
                if let Some(module_head) = inner.module_path.head() {
                    if inner.root_modules.contains(module_head) {
                        if inner.module_path.segments.len() > 1 {
                            let mut segments = Vec::with_capacity(inner.module_path.segments.len());
                            segments.extend(inner.module_path.segments.iter().skip(1).cloned());
                            segments.push(head.to_string());
                            push(QualifiedPath::new(segments), &mut names, &mut seen);
                        }
                    } else {
                        for root in &inner.root_modules {
                            let mut segments =
                                Vec::with_capacity(inner.module_path.segments.len() + 2);
                            segments.push(root.to_string());
                            segments.extend(inner.module_path.segments.iter().cloned());
                            segments.push(head.to_string());
                            push(QualifiedPath::new(segments), &mut names, &mut seen);
                        }
                    }
                }
            }
        }
        push(name_path.clone(), &mut names, &mut seen);

        if name_path.segments.len() > 1 {
            if let Some(tail) = name_path.tail() {
                push(
                    QualifiedPath::new(vec![tail.to_string()]),
                    &mut names,
                    &mut seen,
                );
            }
        }

        names
    }

    async fn lookup_struct_def_by_name(&self, name: &str) -> Option<(QualifiedPath, TypeStruct)> {
        if name == "TypeBuilder" && std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            let keys = self
                .own_struct_defs()
                .keys()
                .filter(|key| key.tail() == Some("TypeBuilder"))
                .cloned()
                .collect::<Vec<_>>();
            eprintln!(
                "debug TypeBuilder: module_path={:?} keys={:?}",
                self.inner.borrow().module_path, keys
            );
        }
        let parsed = parse_path(name).ok();
        let segments = parsed
            .map(|parsed| parsed.segments)
            .unwrap_or_else(|| vec![name.to_string()]);
        let name_path = QualifiedPath::new(segments.clone());

        if let Some(def) = self.own_struct_defs().get(&name_path).cloned() {
            return Some((name_path, def));
        }
        if let Some(def) = self.typing_ctx.env_ctx.find_struct(&name_path) {
            return Some((name_path, def));
        }
        if let Some(stripped) = Self::strip_std_prefix(&name_path) {
            if let Some(def) = self.own_struct_defs().get(&stripped).cloned() {
                return Some((stripped, def));
            }
        }
        if !self.inner.borrow().module_path.is_empty() && segments.len() == 1 {
            let qualified = self.inner.borrow().module_path.with_segment(segments[0].clone());
            if let Some(def) = self.own_struct_defs().get(&qualified).cloned() {
                return Some((qualified, def));
            }
        }
        if segments.len() > 1 {
            return None;
        }
        let mut match_key = None;
        for key in self.own_struct_defs().keys() {
            if key.tail() == Some(name) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self.own_struct_defs().get(&key).cloned().map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in self.own_struct_defs().iter() {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self.own_struct_defs().get(&key).cloned().map(|def| (key, def));
        }
        // Also check the workspace — a bare/ambiguous-locally name may still
        // resolve unambiguously to a single cross-crate struct.
        let mut match_key = None;
        for krate in self.typing_ctx.env_ctx.crates().values() {
            for key in krate.borrow().struct_defs.keys() {
                if key.tail() == Some(name) {
                    if match_key.is_some() {
                        return None;
                    }
                    match_key = Some(key.clone());
                }
            }
        }
        if let Some(key) = match_key {
            if let Some(def) = self.typing_ctx.env_ctx.find_struct(&key) {
                return Some((key, def));
            }
        }
        for candidate in self.struct_name_variants(name) {
            if let Some(var) = self.lookup_env_var(&candidate.to_key()).await {
                if let Ok(ty) = self.resolve_to_ty(var).await {
                    if let Ty::Struct(def) = ty {
                        return Some((candidate, def));
                    }
                }
            }
        }
        None
    }

    fn lookup_enum_def_by_name(&self, name: &str) -> Option<(QualifiedPath, TypeEnum)> {
        let parsed = parse_path(name).ok();
        let segments = parsed
            .map(|parsed| parsed.segments)
            .unwrap_or_else(|| vec![name.to_string()]);
        let name_path = QualifiedPath::new(segments.clone());

        if let Some(def) = self.own_enum_defs().get(&name_path).cloned() {
            return Some((name_path, def));
        }
        if !self.inner.borrow().module_path.is_empty() && segments.len() == 1 {
            let qualified = self.inner.borrow().module_path.with_segment(segments[0].clone());
            if let Some(def) = self.own_enum_defs().get(&qualified).cloned() {
                return Some((qualified, def));
            }
        }
        if segments.len() > 1 {
            return None;
        }
        let mut match_key = None;
        for key in self.own_enum_defs().keys() {
            if key.tail() == Some(name) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self.own_enum_defs().get(&key).cloned().map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in self.own_enum_defs().iter() {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        match_key.and_then(|key| self.own_enum_defs().get(&key).cloned().map(|def| (key, def)))
    }

    fn record_function_signature(&self, name: &Ident, sig: &FunctionSignature) {
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self.inner.borrow().module_path.with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.own_function_sigs_mut().insert(candidate, sig.clone());
        }
    }

    fn record_extern_function_signature(&self, name: &Ident, sig: &FunctionSignature) {
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self.inner.borrow().module_path.with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.inner.borrow_mut().extern_function_signatures
                .insert(candidate, sig.clone());
        }
    }

    fn record_unimplemented_symbol(&self, name: &Ident, attrs: &[Attribute]) {
        if !attrs_has_name(attrs, "unimplemented") {
            return;
        }
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self.inner.borrow().module_path.with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.inner.borrow_mut().unimplemented_symbols.insert(candidate);
        }
    }

    fn is_unimplemented_name(&self, name: &QualifiedPath) -> bool {
        self.inner.borrow().unimplemented_symbols.contains(name)
    }

    fn env_contains(&self, key: &str) -> bool {
        self.inner.borrow().env.iter().rev().any(|scope| scope.contains_key(key))
    }

    fn scope_contains_non_module(&self, name: &str) -> bool {
        let inner = self.inner.borrow();
        let module_depth = *inner.module_scope_depths.last().unwrap_or(&0);
        inner
            .env
            .iter()
            .enumerate()
            .rev()
            .any(|(idx, scope)| idx > module_depth && scope.contains_key(name))
    }

    fn item_exists_path(&self, path: &QualifiedPath) -> bool {
        let key = path.to_key();
        self.own_struct_defs().contains_key(path)
            || self.own_enum_defs().contains_key(path)
            || self.own_function_sigs().contains_key(path)
            || self.inner.borrow().extern_function_signatures.contains_key(path)
            || self.own_trait_defs().contains(path)
            || self.inner.borrow().unimplemented_symbols.contains(path)
            || self.env_contains(&key)
            || self.typing_ctx.env_ctx.find_struct(path).is_some()
            || self.typing_ctx.env_ctx.find_function_sig(path).is_some()
    }

    fn resolve_locator_key(&self, locator: &Name) -> Option<QualifiedPath> {
        if let Some(qualified) = self.resolve_alias_locator(locator) {
            return Some(qualified);
        }
        let parsed = self.resolution_parsed_path(locator)?;
        let found = {
            let inner = self.inner.borrow();
            resolve_item_path(
                &parsed,
                &inner.module_path,
                &inner.root_modules,
                &inner.extern_prelude,
                &inner.module_defs,
                |candidate| self.item_exists_path(candidate),
                |name| self.scope_contains_non_module(name),
            )
        };
        if let Some(qualified) = found {
            return Some(qualified);
        }
        // Fallback: try the raw segments as a fully-qualified path
        let raw = QualifiedPath::new(parsed.segments);
        if self.item_exists_path(&raw) {
            return Some(raw);
        }
        None
    }

    fn resolve_segments_key(
        &self,
        prefix: PathPrefix,
        segments: &[String],
    ) -> Option<QualifiedPath> {
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        if matches!(prefix, PathPrefix::Plain) && !segments.is_empty() {
            if let Some(symbol_path) = self.lookup_symbol_alias(&segments[0]) {
                return Some(symbol_path.join(&segments[1..]));
            }
            if let Some(module_path) = self.lookup_module_alias(&segments[0]) {
                return Some(module_path.join(&segments[1..]));
            }
        }
        let parsed = ParsedPath {
            prefix,
            segments: segments.to_vec(),
        };
        let qualified = {
            let inner = self.inner.borrow();
            resolve_item_path(
                &parsed,
                &inner.module_path,
                &inner.root_modules,
                &inner.extern_prelude,
                &inner.module_defs,
                |candidate| self.item_exists_path(candidate),
                |name| self.scope_contains_non_module(name),
            )
        }?;
        Some(qualified)
    }

    fn check_unimplemented_locator(&self, locator: &Name) -> bool {
        if let Some(ident) = locator.as_ident() {
            if !self.inner.borrow().module_path.is_empty() {
                let candidate = self.inner.borrow().module_path.with_segment(ident.as_str().to_string());
                if self.is_unimplemented_name(&candidate) {
                    if !self.is_same_crate_path(&candidate) {
                        self.emit_warning(format!(
                            "use of unimplemented item: {}",
                            candidate.to_key()
                        ));
                    }
                    return false;
                }
            }
        }
        let Some(candidate) = self.resolve_locator_key(locator) else {
            return false;
        };
        if self.is_unimplemented_name(&candidate) {
            if !self.is_same_crate_path(&candidate) {
                self.emit_warning(format!("use of unimplemented item: {}", candidate.to_key()));
            }
            return false;
        }
        false
    }

    fn is_same_crate_path(&self, candidate: &QualifiedPath) -> bool {
        let Some(current_root) = self.inner.borrow().module_path.head().map(|s| s.to_string()) else {
            return false;
        };
        candidate.head() == Some(current_root.as_str())
    }

    fn lookup_function_signature(&self, locator: &Name) -> Option<FunctionSignature> {
        let candidate = self
            .resolve_locator_key(locator)
            .or_else(|| self.fallback_locator_key(locator))?;
        if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&candidate) {
            return Some(sig.clone());
        }
        if let Some(sig) = self.own_function_sigs().get(&candidate) {
            return Some(sig.clone());
        }
        self.lookup_stripped_function_signature(&candidate)
            .or_else(|| self.lookup_prefixed_function_signature(&candidate))
            .or_else(|| {
                self.locator_tail_name(locator)
                    .and_then(|name| self.lookup_function_signature_by_name(&name))
            })
    }

    /// Suspends once (via `await_package`) if the first attempt fails and
    /// the locator's head names a registered-but-unloaded package, then
    /// retries the whole lookup -- mirrors `lookup_struct`'s suspend/retry
    /// shape for the function-signature case.
    async fn lookup_function_signature_with_path(
        &self,
        locator: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        if let Some(found) = self.lookup_function_signature_with_path_once(locator) {
            return Some(found);
        }
        let candidate = self
            .resolve_locator_key(locator)
            .or_else(|| self.fallback_locator_key(locator))?;
        let head = candidate.head()?;
        if !self.typing_ctx.env_ctx.is_registered(head) {
            return None;
        }
        self.await_package(head).await;
        self.lookup_function_signature_with_path_once(locator)
    }

    fn lookup_function_signature_with_path_once(
        &self,
        locator: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let candidate = self
            .resolve_locator_key(locator)
            .or_else(|| self.fallback_locator_key(locator))?;
        if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&candidate) {
            return Some((candidate, sig.clone()));
        }
        if let Some(sig) = self.own_function_sigs().get(&candidate) {
            return Some((candidate, sig.clone()));
        }
        if let Some(sig) = self.typing_ctx.env_ctx.find_function_sig(&candidate) {
            // Resolved via a workspace crate, not the local one — if this is
            // a `Struct::method`-shaped path, the struct's owning crate's
            // `impl` block needs to be visible to HIR/MIR lowering too (see
            // `cross_crate_struct_refs`'s doc comment).
            if candidate.segments.len() >= 2 {
                if let Some(struct_path) = candidate.parent_n(1) {
                    self.inner.borrow_mut().cross_crate_struct_refs.insert(struct_path);
                }
            }
            return Some((candidate, sig.clone()));
        }
        if let Some(stripped) = Self::strip_std_prefix(&candidate) {
            if let Some(sig) = self.own_function_sigs().get(&stripped) {
                return Some((stripped, sig.clone()));
            }
        }
        if let Some((path, sig)) = self.lookup_prefixed_signature_with_path(&candidate, false) {
            return Some((path, sig));
        }
        if let Some(found) = self
            .locator_tail_name(locator)
            .and_then(|name| self.lookup_function_signature_by_name_with_path(&name))
        {
            return Some(found);
        }
        None
    }

    fn lookup_extern_function_signature_with_path(
        &self,
        locator: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let candidate = self
            .resolve_locator_key(locator)
            .or_else(|| self.fallback_locator_key(locator))?;
        if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&candidate) {
            return Some((candidate, sig.clone()));
        }
        if let Some(stripped) = Self::strip_std_prefix(&candidate) {
            if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&stripped) {
                return Some((stripped, sig.clone()));
            }
        }
        self.lookup_prefixed_signature_with_path(&candidate, true)
    }

    fn lookup_stripped_function_signature(
        &self,
        candidate: &QualifiedPath,
    ) -> Option<FunctionSignature> {
        let stripped = Self::strip_std_prefix(candidate)?;
        self.own_function_sigs().get(&stripped).cloned()
    }

    fn strip_std_prefix(candidate: &QualifiedPath) -> Option<QualifiedPath> {
        let first = candidate.segments.first()?;
        if (first == "std" || first == "core" || first == "alloc") && candidate.segments.len() > 1 {
            Some(QualifiedPath::new(
                candidate.segments.iter().skip(1).cloned().collect(),
            ))
        } else {
            None
        }
    }

    fn lookup_prefixed_function_signature(
        &self,
        candidate: &QualifiedPath,
    ) -> Option<FunctionSignature> {
        self.lookup_prefixed_signature(candidate, false)
    }

    fn fallback_locator_key(&self, locator: &Name) -> Option<QualifiedPath> {
        let (prefix, segments) = match locator {
            Name::Path(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.as_str().to_string())
                    .collect::<Vec<_>>(),
            ),
            Name::ParameterPath(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.ident.as_str().to_string())
                    .collect::<Vec<_>>(),
            ),
            _ => return None,
        };
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        match prefix {
            PathPrefix::Plain | PathPrefix::Root | PathPrefix::Crate => {
                Some(QualifiedPath::new(segments))
            }
            _ => None,
        }
    }

    fn lookup_function_signature_by_name(&self, name: &str) -> Option<FunctionSignature> {
        let mut found: Option<FunctionSignature> = None;
        for (key, sig) in self.own_function_sigs().iter() {
            if key.tail() == Some(name) {
                if found.is_some() {
                    return None;
                }
                found = Some(sig.clone());
            }
        }
        found
    }

    fn lookup_function_signature_by_name_with_path(
        &self,
        name: &str,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let mut found: Option<(QualifiedPath, FunctionSignature)> = None;
        for (key, sig) in self.own_function_sigs().iter() {
            if key.tail() == Some(name) {
                if found.is_some() {
                    return None;
                }
                found = Some((key.clone(), sig.clone()));
            }
        }
        found
    }

    fn lookup_prefixed_signature(
        &self,
        candidate: &QualifiedPath,
        extern_only: bool,
    ) -> Option<FunctionSignature> {
        let first = candidate.segments.first()?;
        if first == "std" || first == "core" || first == "alloc" {
            return None;
        }
        for prefix in ["std", "core", "alloc"] {
            if !self.inner.borrow().root_modules.contains(prefix) {
                continue;
            }
            let base = QualifiedPath::new(vec![prefix.to_string()]);
            let qualified = base.join(&candidate.segments);
            if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&qualified) {
                return Some(sig.clone());
            }
            if !extern_only {
                if let Some(sig) = self.own_function_sigs().get(&qualified) {
                    return Some(sig.clone());
                }
            }
        }
        None
    }

    fn lookup_prefixed_signature_with_path(
        &self,
        candidate: &QualifiedPath,
        extern_only: bool,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let first = candidate.segments.first()?;
        if first == "std" || first == "core" || first == "alloc" {
            return None;
        }
        for prefix in ["std", "core", "alloc"] {
            if !self.inner.borrow().root_modules.contains(prefix) {
                continue;
            }
            let base = QualifiedPath::new(vec![prefix.to_string()]);
            let qualified = base.join(&candidate.segments);
            if let Some(sig) = self.inner.borrow().extern_function_signatures.get(&qualified) {
                return Some((qualified, sig.clone()));
            }
            if !extern_only {
                if let Some(sig) = self.own_function_sigs().get(&qualified) {
                    return Some((qualified, sig.clone()));
                }
            }
        }
        None
    }

    async fn resolve_impl_context(&self, self_ty: &Expr) -> Option<ImplContext> {
        if let ExprKind::Value(value) = self_ty.kind() {
            if let Value::Type(ty) = value.as_ref() {
                let name = format!("<impl:{}>", ty);
                return Some(ImplContext {
                    struct_name: QualifiedPath::new(vec![name]),
                    self_ty: ty.clone(),
                });
            }
        }
        let resolved_name = match self_ty.kind() {
            ExprKind::Name(locator) => self.resolve_locator_key(locator),
            _ => None,
        };
        let name = resolved_name
            .or_else(|| self.struct_name_from_expr(self_ty))
            .unwrap_or_else(|| QualifiedPath::new(Vec::new()));

        if name.is_empty() {
            self.emit_error("impl self type must resolve to a struct or enum");
            return None;
        }

        if let Some(def) = self.own_struct_defs().get(&name).cloned() {
            return Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Struct(def),
            });
        }
        if let Some(def) = self.own_enum_defs().get(&name).cloned() {
            return Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Enum(def),
            });
        }

        if let Some((resolved, def)) = self.lookup_struct_def_by_name(&name.to_key()).await {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Struct(def),
            });
        }
        if let Some((resolved, def)) = self.lookup_enum_def_by_name(&name.to_key()) {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Enum(def),
            });
        }
        if let Some(ty) = self.resolve_impl_self_from_env(&name).await {
            return Some(ty);
        }

        for candidate in self.struct_name_variants_for_path(&name, name.segments.len() == 1) {
            if let Some(def) = self.own_struct_defs().get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Struct(def),
                });
            }
        }
        for candidate in self.struct_name_variants_for_path(&name, name.segments.len() == 1) {
            if let Some(def) = self.own_enum_defs().get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Enum(def),
                });
            }
        }

        {
            let placeholder_name = name.tail().unwrap_or("Unknown").to_string();
            let placeholder = TypeStruct {
                name: Ident::new(placeholder_name),
                generics_params: Vec::new(),
                repr: ReprOptions::default(),
                method_sigs: Vec::new(),
                fields: Vec::new(),
            };
            self.emit_warning(format!(
                "impl target {} is not a known struct or enum",
                name.to_key()
            ));
            Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Struct(placeholder),
            })
        }
    }

    async fn resolve_impl_self_from_env(&self, name: &QualifiedPath) -> Option<ImplContext> {
        let mut candidates = Vec::new();
        candidates.push(name.clone());
        if !self.inner.borrow().module_path.is_empty() && name.segments.len() == 1 {
            candidates.push(self.inner.borrow().module_path.with_segment(name.segments[0].clone()));
        }
        for candidate in candidates {
            let key = candidate.to_key();
            if let Some(var) = self.lookup_env_var(&key).await {
                if let Ok(ty) = self.resolve_to_ty(var).await {
                    match ty {
                        Ty::Struct(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Struct(def),
                            })
                        }
                        Ty::Enum(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Enum(def),
                            })
                        }
                        _ => {}
                    }
                }
            }
        }
        None
    }

    fn ty_for_receiver(&self, ctx: &ImplContext, receiver: &FunctionParamReceiver) -> Ty {
        match receiver {
            FunctionParamReceiver::Implicit
            | FunctionParamReceiver::Value
            | FunctionParamReceiver::MutValue => ctx.self_ty.clone(),
            FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                }
                .into(),
            ),
            FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                }
                .into(),
            ),
        }
    }

    fn register_method_stub(&self, ctx: &ImplContext, func: &ItemDefFunction) {
        for candidate in self
            .struct_name_variants_for_path(&ctx.struct_name, ctx.struct_name.segments.len() == 1)
        {
            if let Some(s) = self.own_struct_defs_mut().get_mut(&candidate) {
                s.method_sigs.push((func.name.as_str().to_string(), func.sig.clone()));
            }
        }
    }

    fn peel_reference(mut ty: Ty) -> Ty {
        loop {
            match ty {
                Ty::Reference(reference) => {
                    ty = (*reference.ty).clone();
                }
                other => return other,
            }
        }
    }

    async fn predeclare_item(&self, item: &Item) {
        if std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            match item.kind() {
                ItemKind::DefStruct(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefStruct module_path={:?}",
                        self.inner.borrow().module_path
                    );
                }
                ItemKind::DefConst(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefConst module_path={:?}",
                        self.inner.borrow().module_path
                    );
                }
                ItemKind::DefType(def) if def.name.as_str().contains("TypeBuilder") => {
                }
                ItemKind::DefStructural(def) if def.name.as_str().contains("TypeBuilder") => {
                }
                _ => {}
            }
        }
        match item.kind() {
            ItemKind::Macro(mac) => {
                self.predeclare_macro_item(mac);
            }
            ItemKind::DefStruct(def) => {
                self.insert_struct_def(&def.name, def.value.clone());
                let var = self.symbol_var(&def.name).await;
                let ty = Ty::Struct(def.value.clone());
                if let Ok(struct_var) = self.type_from_ast_ty(&ty).await {
                    let _ = self.unify(var, struct_var).await;
                }
            }
            ItemKind::DefStructural(def) => {
                let struct_ty = TypeStruct {
                    name: def.name.clone(),
                    generics_params: Vec::new(),
                    repr: ReprOptions::default(),
                    method_sigs: Vec::new(),
                    fields: def.value.fields.clone(),
                };
                self.insert_struct_def(&def.name, struct_ty);
                self.register_symbol(&def.name);
            }
            ItemKind::DefType(def) => {
                self.register_symbol(&def.name);
                // Type the value (const block or direct expression). Struct types
                // are resolved via comptime eval and seeded into struct_defs on retry.
                let _ = self.type_from_ast_ty(&def.value).await;
                let is_const_block = matches!(&def.value, Ty::ConstBlock(_));

                if !is_const_block {
                    self.record_unimplemented_symbol(&def.name, &def.attrs);
                } else if let Ty::ConstBlock(block) = &def.value {
                    // Spawn this alias's struct-shape resolution as its own
                    // independent task (see `TypingContext::tasks`), keyed
                    // by the alias's own name so `force`/`await_struct_alias`
                    // can find and await it -- mirrors the `DefConst` arm
                    // above. Only computes/caches the resolved struct shape
                    // via a throwaway clone of the const-block's inner expr;
                    // the item's own symbol-table bookkeeping stays owned by
                    // the sequential loop's later "already resolved" fast
                    // path (the full-inference `DefType` arm below).
                    let name = def.name.as_str().to_string();
                    if !self.typing_ctx.resolved_types.borrow().contains_key(&name)
                        && !self.typing_ctx.tasks.contains(&name)
                    {
                        let this = self.clone();
                        let mut expr_clone = (*block.expr).clone();
                        let expr_id = self.expr_id(&block.expr);
                        let key = format!("__fp_expr_{expr_id}");
                        self.typing_ctx.tasks.spawn(name, async move {
                            let _ = this.infer_expr_inner(&mut expr_clone).await;
                            this.await_comptime(&key, &expr_clone).await.map(|_| ())
                        });
                    }
                }

                // Look up the struct by its qualified name, in case a prior
                // pass already resolved it — lets sibling items in this same
                // scope reference it before this item's own full-inference
                // turn comes up.
                let path = if self.inner.borrow().module_path.is_empty() {
                    QualifiedPath::new(vec![def.name.as_str().to_string()])
                } else {
                    self.inner.borrow().module_path.with_segment(def.name.as_str().to_string())
                };
                let struct_def = self.own_struct_defs().get(&path).cloned().or_else(|| {
                    self.typing_ctx.resolved_types.borrow().get(def.name.as_str()).cloned()
                        .map(|s| {
                            self.own_struct_defs_mut().insert(path.clone(), s.clone());
                            s
                        })
                });
                if let Some(struct_def) = struct_def {
                    let var = self.symbol_var(&def.name).await;
                    self.bind(var, Ty::Struct(struct_def));
                }
            }
            ItemKind::DefEnum(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let enum_name = self
                    .qualified_name(def.name.as_str())
                    .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                self.own_enum_defs_mut().insert(enum_name.clone(), def.value.clone());
                self.register_symbol(&def.name);

                let mut variant_keys = Vec::new();
                for variant in &def.value.variants {
                    let qualified = enum_name.with_segment(variant.name.as_str().to_string());
                    variant_keys.push(qualified.clone());
                    let key = qualified.to_key();
                    if self.lookup_env_var(&key).await.is_none() {
                        let var = self.fresh_type_var();
                        self.insert_env(key, EnvEntry::Mono(var));
                    }
                }
                self.inner.borrow_mut().enum_variants.insert(enum_name, variant_keys);
            }
            ItemKind::DefTrait(def) => {
                let trait_name = self
                    .qualified_name(def.name.as_str())
                    .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                self.own_trait_defs_mut().insert(trait_name);
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                self.register_symbol(&def.name);
                // `entry`/`or_default()` returns a `&mut` borrowed from the
                // `RefMut` guard, so the guard is bound to a named local
                // (`inner`) rather than left as an inline temporary --
                // a bare temporary's scope wouldn't survive past this
                // statement, but `entry` needs to keep being written to for
                // the rest of this arm. Safe to hold across the loop below
                // since nothing in it re-borrows `self.inner` or awaits.
                let mut inner = self.inner.borrow_mut();
                let entry = inner
                    .trait_method_sigs
                    .entry(def.name.as_str().to_string())
                    .or_default();
                for member in &def.items {
                    match member.kind() {
                        ItemKind::DeclFunction(decl) => {
                            if let Some(name) = decl.sig.name.as_ref() {
                                entry.insert(name.as_str().to_string(), decl.sig.clone());
                            }
                        }
                        ItemKind::DefFunction(func) => {
                            entry.insert(func.name.as_str().to_string(), func.sig.clone());
                        }
                        _ => {}
                    }
                }
                drop(inner);
            }
            ItemKind::DefConst(def) => {
                self.register_symbol(&def.name);
                // Spawn this const's comptime-value resolution as its own
                // independent task (see `TypingContext::tasks`) -- so
                // `force`/`await_comptime` can await it directly regardless
                // of where in the item list it sits, instead of the
                // sequential item loop's own order determining whether a
                // forward reference resolves. This only computes and caches
                // the VALUE (via a throwaway clone of the expr, same
                // fast-path-tolerant typing + hook call the real `DefConst`
                // arm below does) -- it deliberately does *not* run the
                // item's own symbol-table bookkeeping (`symbol_var`/
                // `generalize_symbol`), which the sequential loop still owns
                // exactly once, later, via its normal "already resolved --
                // bind from cache" fast path.
                let name = def.name.as_str().to_string();
                if !self.typing_ctx.resolved_consts.borrow().contains_key(&name)
                    && !self.typing_ctx.tasks.contains(&name)
                {
                    let this = self.clone();
                    let mut expr_clone = (*def.value).clone();
                    let key = name.clone();
                    self.typing_ctx.tasks.spawn(name, async move {
                        let _ = this.infer_expr_inner(&mut expr_clone).await;
                        this.await_comptime(&key, &expr_clone).await.map(|_| ())
                    });
                }
            }
            ItemKind::DefStatic(def) => {
                self.register_symbol(&def.name);
                if let Ty::Struct(ty) = &def.ty {
                    self.insert_struct_def(&ty.name, ty.clone());
                }
            }
            ItemKind::DefFunction(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let in_impl = self.inner.borrow().impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&def.name, &def.sig);
                }
                // Extracted to an owned local *before* the `if let`: this
                // chain's final `else` branch awaits, and using the
                // `self.inner.borrow()` call directly as the `if let`
                // scrutinee would otherwise have the guard's scope extended
                // across that `.await` (Rust extends an `if let`
                // scrutinee's temporaries over the whole `if`/`else if`/
                // `else` chain).
                let impl_ctx_opt = self.inner.borrow().impl_stack.last().cloned().flatten();
                let fn_var = if let Some(ctx) = impl_ctx_opt {
                    let key = ctx.struct_name.with_segment(def.name.as_str().to_string());
                    let key_str = key.to_key();
                    if let Some(var) = self.lookup_env_var(&key_str).await {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key_str, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    if !in_impl {
                        self.register_symbol(&def.name);
                    }
                    self.symbol_var(&def.name).await
                };
                if def.sig.generics_params.is_empty() {
                    self.prebind_function_signature(def, fn_var).await;
                } else {
                    let fn_key = self.inner.borrow().impl_stack.last().cloned().flatten().map(|ctx| {
                        ctx.struct_name
                            .with_segment(def.name.as_str().to_string())
                            .to_key()
                    });
                    self.enter_scope();
                    for param in &def.sig.generics_params {
                        let var = self.register_generic_param(param.name.as_str());
                        let bounds = Self::extract_trait_bounds(&param.bounds);
                        if !bounds.is_empty() {
                            self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                        }
                    }
                    let mut ok = true;
                    let mut param_vars = Vec::new();
                    for param in &def.sig.params {
                        match self.type_from_ast_ty(&param.ty).await {
                            Ok(var) => param_vars.push(var),
                            Err(_) => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    let ret_var = if ok {
                        if let Some(ret_ty) = def.sig.ret_ty.as_ref() {
                            self.type_from_ast_ty(ret_ty).await.ok()
                        } else {
                            Some(self.unit_type_var())
                        }
                    } else {
                        None
                    };
                    if ok {
                        if let Some(ret_var) = ret_var {
                            self.bind_function_term(fn_var, param_vars, ret_var);
                        } else {
                            ok = false;
                        }
                    }
                    self.exit_scope();
                    if ok {
                        if let Ok(scheme) = self.generalize(fn_var).await {
                            if let Some(key) = fn_key.as_ref() {
                                self.replace_env_entry(key.as_str(), EnvEntry::Poly(scheme));
                            } else {
                                self.replace_env_entry(def.name.as_str(), EnvEntry::Poly(scheme));
                            }
                        }
                    }
                }
            }
            ItemKind::DeclFunction(decl) => {
                let in_impl = self.inner.borrow().impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&decl.name, &decl.sig);
                    if decl.sig.abi.is_c() {
                        self.record_extern_function_signature(&decl.name, &decl.sig);
                    }
                    self.register_symbol(&decl.name);
                }
                // See the `DefFunction` arm above for why this is extracted
                // before the `if let` chain (its `else` branch awaits).
                let impl_ctx_opt = self.inner.borrow().impl_stack.last().cloned().flatten();
                let fn_var = if let Some(ctx) = impl_ctx_opt {
                    let key = ctx.struct_name.with_segment(decl.name.as_str().to_string());
                    let key_str = key.to_key();
                    if let Some(var) = self.lookup_env_var(&key_str).await {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key_str, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    self.symbol_var(&decl.name).await
                };
                if decl.sig.generics_params.is_empty() && decl.sig.receiver.is_none() {
                    self.prebind_decl_function_signature(decl, fn_var).await;
                }
            }
            ItemKind::Module(module) => {
                self.record_module_def(module.name.as_str());
                self.push_module_path(module.name.as_str());
                self.enter_scope();
                // Read `env.len()` before taking the `module_scope_depths`
                // write borrow -- both are `Inner` fields, and a `RefMut`
                // for the push's receiver held simultaneously with a `Ref`
                // for its argument would panic at runtime (`RefCell`
                // borrows aren't partitioned per-field).
                let env_len = self.inner.borrow().env.len();
                self.inner.borrow_mut().module_scope_depths.push(env_len.saturating_sub(1));
                self.predeclare_scope_items(&module.collected_items).await;
                self.exit_scope();
                self.inner.borrow_mut().module_scope_depths.pop();
                self.pop_module_path();
                let prefix = if self.inner.borrow().module_path.is_empty() {
                    QualifiedPath::new(vec![module.name.as_str().to_string()])
                } else {
                    self.inner.borrow().module_path
                        .with_segment(module.name.as_str().to_string())
                };
                self.register_qualified_items(&module.items, &prefix).await;
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty).await;
                self.inner.borrow_mut().impl_stack.push(ctx.clone());
                if let Some(ref ctx) = ctx {
                    for child in &impl_block.items {
                        if let ItemKind::DefFunction(func) = child.kind() {
                            self.register_method_stub(ctx, func);
                            for candidate in self.struct_name_variants_for_path(
                                &ctx.struct_name,
                                ctx.struct_name.segments.len() == 1,
                            ) {
                                let key = candidate.with_segment(func.name.as_str().to_string());
                                let key_str = key.to_key();
                                if self.lookup_env_var(&key_str).await.is_none() {
                                    let var = self.fresh_type_var();
                                    self.insert_env(key_str, EnvEntry::Mono(var));
                                    self.prebind_function_signature(func, var).await;
                                }
                            }
                        }
                    }
                }

                self.enter_scope();
                self.predeclare_scope_items(&impl_block.collected_items).await;
                self.exit_scope();
                self.inner.borrow_mut().impl_stack.pop();
            }
            _ => {}
        }
    }

    fn predeclare_macro_item(&self, mac: &ItemMacro) {
        let macro_name = mac
            .invocation
            .path
            .segments
            .last()
            .map(|ident| ident.as_str());
        let Some(macro_name) = macro_name else {
            return;
        };
        let tokens = tokenize_macro_tokens(&mac.invocation.tokens);
        match macro_name {
            "common_struct" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "struct") {
                    self.register_placeholder_struct(&name);
                }
            }
            "common_enum" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "enum") {
                    self.register_placeholder_enum(&name);
                }
            }
            "plain_value" => {
                if let Some(name) = find_first_type_ident(&tokens) {
                    self.register_placeholder_struct(&name);
                }
            }
            _ => {}
        }
    }

    fn register_placeholder_struct(&self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| QualifiedPath::new(vec![name.to_string()]));
        if self.own_struct_defs().contains_key(&key) {
            return;
        }
        let ty = TypeStruct {
            name: Ident::new(name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            method_sigs: Vec::new(),
            fields: Vec::new(),
        };
        self.own_struct_defs_mut().insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    fn register_placeholder_enum(&self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| QualifiedPath::new(vec![name.to_string()]));
        if self.own_enum_defs().contains_key(&key) {
            return;
        }
        let ty = TypeEnum {
            name: Ident::new(name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            variants: Vec::new(),
        };
        self.own_enum_defs_mut().insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    async fn register_import_aliases(&self, import: &ItemImport) {
        let entries = match self.expand_import_tree(&import.tree, Vec::new()) {
            Ok(entries) => entries,
            Err(err) => {
                self.emit_error(format!("failed to expand import tree: {}", err));
                return;
            }
        };

        for (path_segments, alias) in entries {
            if path_segments.is_empty() {
                continue;
            }
            let mut qualified = QualifiedPath::new(path_segments);
            let parsed = ParsedPath {
                prefix: PathPrefix::Plain,
                segments: qualified.segments.clone(),
            };
            let resolved = {
                let inner = self.inner.borrow();
                resolve_item_path(
                    &parsed,
                    &inner.module_path,
                    &inner.root_modules,
                    &inner.extern_prelude,
                    &inner.module_defs,
                    |candidate| self.item_exists_path(candidate),
                    |name| self.scope_contains_non_module(name),
                )
            };
            if let Some(resolved) = resolved {
                qualified = resolved;
            }
            let mut key = qualified.to_key();
            if self.lookup_env_var(&key).await.is_none()
                && !self.inner.borrow().module_defs.contains(&qualified)
            {
                if let Some(first) = qualified.segments.first() {
                    if (first == "std" || first == "core" || first == "alloc")
                        && qualified.segments.len() > 1
                    {
                        let stripped = QualifiedPath::new(
                            qualified.segments.iter().skip(1).cloned().collect(),
                        );
                        let stripped_key = stripped.to_key();
                        if self.lookup_env_var(&stripped_key).await.is_some()
                            || self.inner.borrow().module_defs.contains(&stripped)
                            || self.item_exists_path(&stripped)
                        {
                            qualified = stripped;
                            key = stripped_key;
                        }
                    }
                }
            }
            if self.lookup_env_var(&key).await.is_some() {
                self.insert_symbol_alias(&alias, qualified);
                continue;
            }
            if self.inner.borrow().module_defs.contains(&qualified) {
                self.insert_module_alias(&alias, qualified);
                continue;
            }
            if self.item_exists_path(&qualified) {
                self.insert_symbol_alias(&alias, qualified);
                continue;
            }
            if self.inner.borrow().lossy_mode {
                let var = self.fresh_type_var();
                self.bind_error(var);
                self.insert_env(key.clone(), EnvEntry::Mono(var));
                self.insert_symbol_alias(&alias, qualified);
            } else {
                let var = self.fresh_type_var();
                self.bind_error(var);
                self.insert_env(key.clone(), EnvEntry::Mono(var));
                self.insert_symbol_alias(&alias, qualified);
            }
        }
    }

    fn insert_module_alias(&self, alias: &str, path: QualifiedPath) {
        if let Some(scope) = self.inner.borrow_mut().module_aliases.last_mut() {
            scope.insert(alias.to_string(), path);
        }
    }

    fn insert_symbol_alias(&self, alias: &str, qualified: QualifiedPath) {
        if let Some(scope) = self.inner.borrow_mut().symbol_aliases.last_mut() {
            scope.insert(alias.to_string(), qualified);
        }
    }

    fn expand_import_tree(
        &self,
        tree: &ItemImportTree,
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        match tree {
            ItemImportTree::Path(path) => self.expand_import_segments(&path.segments, base),
            ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                Ok(results)
            }
            ItemImportTree::Root => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::SelfMod => {
                self.expand_import_segments(&[], self.inner.borrow().module_path.segments.clone())
            }
            ItemImportTree::SuperMod => {
                self.expand_import_segments(&[], self.parent_module_path().segments)
            }
            ItemImportTree::Crate => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
            _ => self.expand_import_segments(std::slice::from_ref(tree), base),
        }
    }

    fn expand_import_segments(
        &self,
        segments: &[ItemImportTree],
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        if segments.is_empty() {
            return Ok(Vec::new());
        }

        let first = &segments[0];
        let rest = &segments[1..];
        match first {
            ItemImportTree::Ident(ident) => {
                let name = ident.name.as_str();
                let mut new_base = base;
                match name {
                    "self" => new_base = self.inner.borrow().module_path.segments.clone(),
                    "super" => new_base = self.parent_module_path().segments,
                    "crate" => new_base = Vec::new(),
                    _ => new_base.push(ident.name.clone()),
                }

                if rest.is_empty() && !matches!(name, "self" | "super" | "crate") {
                    Ok(vec![(new_base.clone(), ident.name.clone())])
                } else if rest.is_empty() {
                    Ok(Vec::new())
                } else {
                    self.expand_import_segments(rest, new_base)
                }
            }
            ItemImportTree::Rename(rename) => {
                if !rest.is_empty() {
                    return Err(typing_error("rename segments must be terminal"));
                }
                let mut new_base = base;
                new_base.push(rename.from.name.clone());
                Ok(vec![(new_base, rename.to.name.clone())])
            }
            ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                if rest.is_empty() {
                    Ok(results)
                } else {
                    let mut final_results = Vec::new();
                    for (path_segments, alias) in results {
                        let mut more = self.expand_import_segments(rest, path_segments.clone())?;
                        if more.is_empty() {
                            final_results.push((path_segments, alias));
                        } else {
                            final_results.append(&mut more);
                        }
                    }
                    Ok(final_results)
                }
            }
            ItemImportTree::Path(path) => {
                let nested = self.expand_import_segments(&path.segments, base.clone())?;
                if rest.is_empty() {
                    Ok(nested)
                } else {
                    let mut results = Vec::new();
                    for (segments_acc, alias) in nested {
                        let mut more = self.expand_import_segments(rest, segments_acc.clone())?;
                        if more.is_empty() {
                            results.push((segments_acc, alias));
                        } else {
                            results.append(&mut more);
                        }
                    }
                    Ok(results)
                }
            }
            ItemImportTree::Root => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::SelfMod => {
                self.expand_import_segments(rest, self.inner.borrow().module_path.segments.clone())
            }
            ItemImportTree::SuperMod => {
                self.expand_import_segments(rest, self.parent_module_path().segments)
            }
            ItemImportTree::Crate => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
        }
    }

    async fn prebind_function_signature(&self, func: &ItemDefFunction, fn_var: TypeVarId) {
        if matches!(func.body.kind(), ExprKind::Async(_))
            || func
                .sig
                .ret_ty
                .as_ref()
                .map(is_std_task_future_ty)
                .unwrap_or(false)
        {
            return;
        }

        if !func.sig.generics_params.is_empty() || func.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        // Extracted to an owned local first: the guard clause below awaits,
        // and matching directly on `self.inner.borrow()...` would extend
        // the borrow guard's scope across that `.await`.
        let root_kind = self.inner.borrow().type_vars[root].kind.clone();
        if matches!(
            root_kind,
            TypeVarKind::Bound(ty) if self.function_term_from_ty(&ty).await.is_some()
        ) {
            return;
        }

        let module_path = self.inner.borrow().module_path.clone();
        let mut param_vars = Vec::new();
        for param in &func.sig.params {
            match self.type_from_ast_ty_in_module(&param.ty, &module_path).await {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &func.sig.ret_ty {
            match self.type_from_ast_ty_in_module(ret_ty, &module_path).await {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, Ty::Unit(TypeUnit));
            unit
        };

        self.bind_function_term(fn_var, param_vars, ret_var);
    }

    async fn prebind_decl_function_signature(&self, decl: &ItemDeclFunction, fn_var: TypeVarId) {
        if !decl.sig.generics_params.is_empty() || decl.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        // See `prebind_function_signature` above: extracted to an owned
        // local before matching, since the guard clause awaits.
        let root_kind = self.inner.borrow().type_vars[root].kind.clone();
        if matches!(
            root_kind,
            TypeVarKind::Bound(ty) if self.function_term_from_ty(&ty).await.is_some()
        ) {
            return;
        }

        let module_path = self.inner.borrow().module_path.clone();
        let mut param_vars = Vec::new();
        for param in &decl.sig.params {
            match self.type_from_ast_ty_in_module(&param.ty, &module_path).await {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &decl.sig.ret_ty {
            match self.type_from_ast_ty_in_module(ret_ty, &module_path).await {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, Ty::Unit(TypeUnit));
            unit
        };

        self.bind_function_term(fn_var, param_vars, ret_var);
    }

    fn infer_item_inner<'a>(&self, item: &'a mut Item) -> BoxFuture<'a, Result<()>> {
        let this = self.clone();
        Box::pin(async move {
            let span = item.span();
            let previous = this.inner.borrow().current_span;
            let active = this.span_or_previous(span, previous);
            this.inner.borrow_mut().current_span = active;
            let result = this.infer_item_inner_body(item).await;
            this.inner.borrow_mut().current_span = previous;
            result.map_err(|err| this.error_with_span(err, active))
        })
    }

    /// Split out of `infer_item_inner` so the span save/restore around it
    /// (which must run even on error) doesn't itself need to live inside a
    /// plain (sync) closure -- a sync closure can't contain `.await`, so
    /// this replaces the old IIFE-closure trick.
    fn infer_item_inner_body<'a>(&self, item: &'a mut Item) -> BoxFuture<'a, Result<()>> {
        let this = self.clone();
        Box::pin(async move {
            let ty = match item.kind_mut() {
                ItemKind::DefStruct(def) => {
                    this.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    this.insert_struct_def(&def.name, def.value.clone());
                    let ty = Ty::Struct(def.value.clone());
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder).await?;
                    ty
                }
                ItemKind::DefStructural(def) => {
                    this.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    let struct_ty = TypeStruct {
                        name: def.name.clone(),
                        generics_params: Vec::new(),
                        repr: ReprOptions::default(),
                        method_sigs: Vec::new(),
                        fields: def.value.fields.clone(),
                    };
                    this.insert_struct_def(&def.name, struct_ty.clone());
                    let ty = Ty::Struct(struct_ty);
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder).await?;
                    ty
                }
                ItemKind::DefType(def) => {
                    // Resolve the RHS to a concrete type; if it is structural, materialize it as a
                    // named struct so that later term-level syntax like `Foo { ... }` can type-check.
                    let placeholder = this.symbol_var(&def.name).await;

                    // Fast path: a const-block type alias already resolved to
                    // a concrete struct by comptime evaluation in a prior
                    // pass — use it directly instead of re-deriving it via
                    // structural inference alone, which can't determine the
                    // shape of a conditionally-built type (e.g. a builder
                    // chain inside `if`). Mirrors `DefConst`'s
                    // `resolved_consts` fast path (see below in this match).
                    let cached = if matches!(&def.value, Ty::ConstBlock(_)) {
                        this.typing_ctx
                            .resolved_types
                            .borrow()
                            .get(def.name.as_str())
                            .cloned()
                    } else {
                        None
                    };

                    let normalized = if let Some(struct_def) = cached {
                        this.insert_struct_def(&def.name, struct_def.clone());
                        Ty::Struct(struct_def)
                    } else if let Ty::ConstBlock(ref mut block) = def.value {
                        // Type the block body first (structural inference
                        // alone — it doesn't need the comptime result), then
                        // try to resolve its compile-time value now: the hook
                        // needs a concretely-typed expression to lower.
                        // Structural inference tolerates unresolved names by
                        // binding them to an error type rather than
                        // hard-failing, so its result is discarded here.
                        let _ = this.infer_expr_inner(block.expr.as_mut()).await;

                        let expr_id = this.expr_id(&block.expr);
                        let key = format!("__fp_expr_{expr_id}");
                        let _value = this.await_comptime(&key, &block.expr).await?;

                        let resolved_struct = this
                            .typing_ctx
                            .resolved_types
                            .borrow()
                            .get(def.name.as_str())
                            .cloned();
                        match resolved_struct {
                            Some(struct_def) => {
                                this.insert_struct_def(&def.name, struct_def.clone());
                                Ty::Struct(struct_def)
                            }
                            None => {
                                // The hook resolved *something* but it
                                // wasn't a struct under this name — a
                                // real error, not a silent placeholder.
                                this.emit_error(format!(
                                    "`type {} = const {{ ... }}` did not resolve to a struct type",
                                    def.name
                                ));
                                Ty::Unknown(TypeUnknown)
                            }
                        }
                    } else {
                        let value_var = this.type_from_ast_ty(&def.value).await?;
                        let resolved = this.resolve_to_ty(value_var).await?;
                        this.normalize_deftype_value(&def.name, resolved).await
                    };

                    let var = this.type_from_ast_ty(&normalized).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder).await?;
                    normalized
                }
                ItemKind::DefEnum(def) => {
                    this.enter_scope();
                    if !def.value.generics_params.is_empty() {
                        for param in &def.value.generics_params {
                            let var = this.register_generic_param(param.name.as_str());
                            let bounds = Self::extract_trait_bounds(&param.bounds);
                            if !bounds.is_empty() {
                                this.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                            }
                        }
                    }

                    this.insert_enum_def(&def.name, def.value.clone());
                    let ty = Ty::Enum(def.value.clone());
                    let placeholder = this.symbol_var(&def.name).await;
                    let var = this.type_from_ast_ty(&ty).await?;
                    this.unify(placeholder, var).await?;
                    this.generalize_symbol(def.name.as_str(), placeholder).await?;

                    let enum_name = this
                        .qualified_name(def.name.as_str())
                        .unwrap_or_else(|| QualifiedPath::new(vec![def.name.as_str().to_string()]));
                    // Extracted to an owned local before the `if let`: its
                    // body awaits repeatedly, and matching directly on
                    // `this.inner.borrow()...` would extend the guard's
                    // scope across those `.await`s.
                    let variant_keys_opt = this.inner.borrow().enum_variants.get(&enum_name).cloned();
                    if let Some(variant_keys) = variant_keys_opt {
                        let enum_var = placeholder;
                        for (variant, qualified) in
                            def.value.variants.iter().zip(variant_keys.into_iter())
                        {
                            if let Some(variant_var) = this.lookup_env_var(&qualified.to_key()).await {
                                let variant_type_var = if matches!(variant.value, Ty::Unit(_)) {
                                    enum_var
                                } else if let Ty::Tuple(tuple) = &variant.value {
                                    let mut param_vars = Vec::new();
                                    for elem in &tuple.types {
                                        param_vars.push(this.type_from_ast_ty(elem).await?);
                                    }
                                    let fn_var = this.fresh_type_var();
                                    this.bind_function_term(fn_var, param_vars, enum_var);
                                    fn_var
                                } else {
                                    let payload_var = this.type_from_ast_ty(&variant.value).await?;
                                    let fn_var = this.fresh_type_var();
                                    this.bind_function_term(fn_var, vec![payload_var], enum_var);
                                    fn_var
                                };
                                let _ = this.unify(variant_var, variant_type_var).await;
                                let _ = this.generalize_symbol(&qualified.to_key(), variant_var).await;
                            }
                        }
                    }

                    this.exit_scope();

                    ty
                }
                ItemKind::DefConst(def) => {
                    let name = def.name.as_str().to_string();
                    let resolved = this.typing_ctx.resolved_consts.borrow().get(&name).cloned();
                    if let Some(resolved) = resolved {
                        // Already evaluated in a prior pass — bind the
                        // symbol directly and skip comptime re-request. Keep
                        // its declared type: `Value::List` cannot retain an
                        // array's length on its own.
                        let placeholder = this.symbol_var(&def.name).await;
                        let ty = def
                            .ty
                            .clone()
                            .or_else(|| def.ty_annotation.clone())
                            .unwrap_or_else(|| crate::runtime_types::type_from_value(&resolved));
                        let ty_var = this.type_from_ast_ty(&ty).await?;
                        this.unify(placeholder, ty_var).await?;
                        def.ty_annotation = Some(ty.clone());
                        def.ty.get_or_insert(ty.clone());
                        this.generalize_symbol(def.name.as_str(), placeholder).await?;
                        ty
                    } else {
                        let placeholder = this.symbol_var(&def.name).await;
                        if let Some(annot) = def.ty.as_ref() {
                            def.value.set_ty(annot.clone());
                        }
                        // Type the value first (structural inference alone —
                        // it doesn't need the comptime result), *then* try to
                        // resolve its compile-time value: the hook needs a
                        // concretely-typed expression to lower.
                        let expr_var = {
                            let mut value = def.value.as_mut();
                            this.infer_expr_inner(&mut value).await?
                        };

                        if let Some(annot) = &def.ty {
                            let annot_var = this.type_from_ast_ty(annot).await?;
                            this.unify(expr_var, annot_var).await?;
                        }

                        this.unify(placeholder, expr_var).await?;
                        let ty = this.resolve_to_ty(expr_var).await?;
                        def.ty_annotation = Some(ty.clone());
                        def.ty.get_or_insert(ty.clone());
                        this.generalize_symbol(def.name.as_str(), placeholder).await?;

                        // If the value is itself a `const { ... }` block, it
                        // may already have resolved via its own hook call
                        // (recorded in `expr_resolutions`, keyed by that
                        // block's own expr id) earlier in this same pass —
                        // in which case there's nothing left to request, only
                        // to copy over under this item's name.
                        let already_resolved_inner = this
                            .typing_ctx
                            .expr_resolutions
                            .borrow()
                            .resolved_value(this.expr_id(&def.value))
                            .cloned();
                        if let Some(value) = already_resolved_inner {
                            this.typing_ctx
                                .resolved_consts
                                .borrow_mut()
                                .insert(name.clone(), value);
                            this.typing_ctx.wake_comptime(&name);
                        } else {
                            let _value = this.await_comptime(&name, &def.value).await?;
                        }
                        ty
                    }
                }
                ItemKind::DefStatic(def) => {
                    let placeholder = this.symbol_var(&def.name).await;
                    let expr_var = {
                        let mut value = def.value.as_mut();
                        this.infer_expr_inner(&mut value).await?
                    };
                    let ty_var = this.type_from_ast_ty(&def.ty).await?;
                    this.unify(expr_var, ty_var).await?;
                    this.unify(placeholder, expr_var).await?;
                    let ty = this.resolve_to_ty(expr_var).await?;
                    def.ty_annotation = Some(ty.clone());
                    this.generalize_symbol(def.name.as_str(), placeholder).await?;
                    ty
                }
                ItemKind::DefFunction(func) => this.infer_function(func).await?,
                ItemKind::DeclConst(decl) => {
                    // An external const declaration has no body to evaluate
                    // here at all -- nothing to await.
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclStatic(decl) => {
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclType(decl) => {
                    let ty = Ty::TypeBounds(decl.bounds.clone());
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclFunction(decl) => {
                    this.validate_extern_c_signature(&decl.sig);
                    let ty = this.ty_from_function_signature(&decl.sig)?;
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::Module(module) => {
                    this.push_module_path(module.name.as_str());
                    this.enter_scope();
                    // Read `env.len()` before taking the
                    // `module_scope_depths` write borrow -- see the same
                    // pattern in `predeclare_item`'s `Module` arm.
                    let env_len = this.inner.borrow().env.len();
                    this.inner.borrow_mut().module_scope_depths.push(env_len.saturating_sub(1));
                    this.predeclare_scope_items(&module.collected_items).await;
                    for child in &mut module.items {
                        this.infer_item_inner(child).await?;
                    }
                    this.exit_scope();
                    this.inner.borrow_mut().module_scope_depths.pop();
                    this.pop_module_path();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Import(import) => {
                    this.register_import_aliases(import).await;
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Macro(_) => {
                    if this.inner.borrow().lossy_mode {
                        Ty::Unit(TypeUnit)
                    } else {
                        this.emit_error("macro items are not yet supported");
                        Ty::Unknown(TypeUnknown)
                    }
                }
                ItemKind::DefTrait(trait_def) => {
                    let trait_name = trait_def.name.as_str().to_string();
                    this.enter_scope();
                    this.predeclare_scope_items(&trait_def.collected_items).await;

                    // Provide `Self` inside trait methods as a generic parameter
                    // bounded by the trait itself.
                    let self_var = this.register_generic_param("Self");
                    this.inner.borrow_mut().generic_trait_bounds
                        .insert(self_var, vec![trait_name.clone()]);

                    for member in &mut trait_def.items {
                        match member.kind_mut() {
                            ItemKind::DeclFunction(decl) => {
                                let ty = this.ty_from_function_signature(&decl.sig)?;
                                decl.ty_annotation = Some(ty);
                            }
                            ItemKind::DefFunction(func) => {
                                this.infer_trait_method(func).await?;
                            }
                            _ => {}
                        }
                    }

                    this.exit_scope();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Impl(impl_block) => {
                    let ctx = this.resolve_impl_context(&impl_block.self_ty).await;

                    if let (Some(ctx), Some(trait_ty)) =
                        (ctx.as_ref(), impl_block.trait_ty.as_ref())
                    {
                        let trait_name = trait_ty.to_string();
                        this.inner.borrow_mut().impl_traits
                            .entry(ctx.struct_name.clone())
                            .or_default()
                            .insert(trait_name.clone());

                        // No `.await` anywhere in this `if let`'s body, so a
                        // borrow taken directly as its scrutinee (and
                        // extended by Rust across the whole body) is fine.
                        if let Some(methods) = this.inner.borrow().trait_method_sigs.get(&trait_name).cloned() {
                            for (method_name, sig) in methods {
                                if sig.receiver.is_none() {
                                    continue;
                                }
                                // Ensure default trait methods are callable as inherent methods
                                // on this concrete receiver type.
                                for candidate in this.struct_name_variants_for_path(
                                    &ctx.struct_name,
                                    ctx.struct_name.segments.len() == 1,
                                ) {
                                    if let Some(s) = this.own_struct_defs_mut().get_mut(&candidate) {
                                        if s.method_sigs.iter().any(|(n, _)| n == &method_name) {
                                            continue;
                                        }
                                        s.method_sigs.push((method_name.clone(), sig.clone()));
                                    }
                                }
                            }
                        }
                    }

                    this.inner.borrow_mut().impl_stack.push(ctx.clone());
                    this.enter_scope();
                    this.predeclare_scope_items(&impl_block.collected_items).await;
                    for child in &mut impl_block.items {
                        this.infer_item_inner(child).await?;
                    }
                    this.exit_scope();
                    this.inner.borrow_mut().impl_stack.pop();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Expr(expr) => {
                    if let ExprKind::Splice(splice) = expr.kind_mut() {
                        let token_var = this.infer_expr_inner(splice.token.as_mut()).await?;
                        let token_ty = this.resolve_to_ty(token_var).await?;
                        if !this.is_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    this.emit_error(format!(
                                        "splice in item position requires item token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => this.emit_error("splice expects a quote token expression"),
                            }
                        }
                        Ty::Unit(TypeUnit)
                    } else if let ExprKind::SplicePending(pending) = expr.kind_mut() {
                        let token_var = this.infer_expr_inner(pending.token.as_mut()).await?;
                        let token_ty = this.resolve_to_ty(token_var).await?;
                        if !this.is_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    this.emit_error(format!(
                                        "splice in item position requires item token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => this.emit_error("splice expects a quote token expression"),
                            }
                        }
                        Ty::Unit(TypeUnit)
                    } else {
                        let var = this.infer_expr_inner(expr).await?;
                        this.resolve_to_ty(var).await?
                    }
                }
                _ => {
                    this.emit_error("type inference for item not implemented");
                    Ty::Unknown(TypeUnknown)
                }
            };

            item.set_ty(ty);
            Ok(())
        })
    }

    async fn infer_function(&self, func: &mut ItemDefFunction) -> Result<Ty> {
        self.validate_extern_c_signature(&func.sig);
        let is_lang_item = func.attrs.find_by_name("lang").is_some();
        let impl_ctx = self.inner.borrow().impl_stack.last().cloned().flatten();
        let fn_key = impl_ctx
            .as_ref()
            .map(|ctx| ctx.struct_name.with_segment(func.name.as_str().to_string()));
        let fn_var = if let Some(key) = fn_key.as_ref() {
            let key_str = key.to_key();
            if let Some(var) = self.lookup_env_var(&key_str).await {
                var
            } else {
                let var = self.fresh_type_var();
                self.insert_env(key_str, EnvEntry::Mono(var));
                var
            }
        } else {
            self.symbol_var(&func.name).await
        };
        let param_count = func.sig.params.len();
        let body_is_async_expr = matches!(func.body.kind(), ExprKind::Async(_));
        let is_async_fn = body_is_async_expr
            || func
                .sig
                .ret_ty
                .as_ref()
                .map(is_std_task_future_ty)
                .unwrap_or(false);
        let existing_signature = if is_async_fn {
            None
        } else {
            let root = self.find(fn_var);
            // Extracted to an owned local first: the `Bound` arm awaits,
            // and matching directly on `self.inner.borrow()...` would
            // extend the guard's scope across that `.await`.
            let root_kind = self.inner.borrow().type_vars[root].kind.clone();
            match root_kind {
                TypeVarKind::Bound(ty) => {
                    if let Some(func_term) = self.function_term_from_ty(&ty).await {
                        if func_term.params.len() == param_count {
                            Some(func_term)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        };

        let exception_policy = self.exception_policy_for_ret(func.sig.ret_ty.as_ref());
        let _exception_guard = self.push_exception_context(exception_policy);

        self.enter_scope();

        let _receiver_ty: Option<Ty> = None;
        if let Some(receiver) = func.sig.receiver.as_ref() {
            if let Some(ctx) = impl_ctx.as_ref() {
                let receiver_type = self.ty_for_receiver(ctx, receiver);
                let self_var = self.fresh_type_var();
                let expected = self.type_from_ast_ty(&receiver_type).await?;
                self.unify(self_var, expected).await?;
                self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
            } else {
                self.emit_error(format!(
                    "method {} defined without an impl context",
                    func.name
                ));
            }
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for (idx, param) in func.sig.params.iter_mut().enumerate() {
            let var = existing_signature
                .as_ref()
                .and_then(|sig| sig.params.get(idx).cloned())
                .unwrap_or_else(|| self.fresh_type_var());
            let annot_var = self.type_from_ast_ty(&param.ty).await?;
            self.unify(var, annot_var).await?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var).await?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = if is_lang_item {
            if let Some(ret) = &func.sig.ret_ty {
                self.type_from_ast_ty(ret).await?
            } else {
                self.fresh_type_var()
            }
        } else if let Some(kind) = func.sig.quote_kind {
            let body_block = func.body.as_ref().clone().into_block();
            let mut quote_expr = Expr::from(ExprKind::Quote(ExprQuote {
                span: Span::null(),
                collected_items: body_block.collected_items.clone(),
                block: body_block,
                kind: Some(kind),
            }));
            self.infer_expr_inner(&mut quote_expr).await?
        } else {
            let mut body = func.body.as_mut();
            self.infer_expr_inner(&mut body).await?
        };

        let ret_var = if matches!(exception_policy, ExceptionReturnPolicy::AutoResult) {
            let body_ty = self.resolve_to_ty(body_var).await?;
            let inner_ty = if is_async_fn {
                std_task_future_inner_ty(&body_ty).unwrap_or(body_ty)
            } else {
                body_ty
            };
            let result_ty = make_std_result_ty(inner_ty, std_error_ty());
            let final_ret_ty = if is_async_fn {
                make_std_task_future_ty(result_ty)
            } else {
                result_ty
            };
            let result_var = self.type_from_ast_ty(&final_ret_ty).await?;
            if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
                self.unify(existing, result_var).await?;
                existing
            } else {
                result_var
            }
        } else if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
            if !is_async_fn || body_is_async_expr {
                self.unify(existing, body_var).await?;
            }
            if let Some(ret) = &func.sig.ret_ty {
                if is_async_fn {
                    let future_ty = if is_std_task_future_ty(ret) {
                        ret.clone()
                    } else {
                        make_std_task_future_ty(ret.clone())
                    };
                    let future_var = self.type_from_ast_ty(&future_ty).await?;
                    self.unify(existing, future_var).await?;

                    if !body_is_async_expr {
                        let body_ty = self.resolve_to_ty(body_var).await?;
                        if is_future_like_ty(&body_ty) {
                            self.unify(body_var, future_var).await?;
                        } else if let Some(inner_ty) = std_task_future_inner_ty(&future_ty) {
                            let inner_var = self.type_from_ast_ty(&inner_ty).await?;
                            self.unify(body_var, inner_var).await?;
                        }
                    }
                } else {
                    let annot_var = self.type_from_ast_ty(ret).await?;
                    self.unify(existing, annot_var).await?;
                }
            }
            existing
        } else if let Some(ret) = &func.sig.ret_ty {
            if is_async_fn {
                let future_ty = if is_std_task_future_ty(ret) {
                    ret.clone()
                } else {
                    make_std_task_future_ty(ret.clone())
                };
                let future_var = self.type_from_ast_ty(&future_ty).await?;
                if body_is_async_expr {
                    self.unify(body_var, future_var).await?;
                } else {
                    let body_ty = self.resolve_to_ty(body_var).await?;
                    if is_future_like_ty(&body_ty) {
                        self.unify(body_var, future_var).await?;
                    } else if let Some(inner_ty) = std_task_future_inner_ty(&future_ty) {
                        let inner_var = self.type_from_ast_ty(&inner_ty).await?;
                        self.unify(body_var, inner_var).await?;
                    }
                }
                future_var
            } else {
                let annot_var = self.type_from_ast_ty(ret).await?;
                self.unify(body_var, annot_var).await?;
                annot_var
            }
        } else {
            body_var
        };

        let ret_ty = self.resolve_to_ty(ret_var.clone()).await?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        self.exit_scope();

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var).await?);
        }

        self.bind_function_term(fn_var, param_vars.clone(), ret_var);

        let scheme = self.generalize(fn_var).await?;
        let scheme_env = scheme.clone();
        if let Some(key) = fn_key.as_ref() {
            let key_str = key.to_key();
            self.replace_env_entry(&key_str, EnvEntry::Poly(scheme_env));
        } else {
            self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme_env));
        }

        if let Some(ctx) = impl_ctx.as_ref() {
            for candidate in self.struct_name_variants_for_path(
                &ctx.struct_name,
                ctx.struct_name.segments.len() == 1,
            ) {
                if let Some(s) = self.own_struct_defs_mut().get_mut(&candidate) {
                    if !s.method_sigs.iter().any(|(n, _)| n == func.name.as_str()) {
                        s.method_sigs.push((func.name.as_str().to_string(), func.sig.clone()));
                    }
                }
            }
        }

        let func_ty = TypeFunction {
            params: param_tys.clone(),
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty.clone())),
        };

        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    async fn infer_trait_method(&self, func: &mut ItemDefFunction) -> Result<Ty> {
        let fn_var = self.symbol_var(&func.name).await;

        let exception_policy = self.exception_policy_for_ret(func.sig.ret_ty.as_ref());
        let _exception_guard = self.push_exception_context(exception_policy);

        self.enter_scope();

        if let Some(receiver) = func.sig.receiver.as_ref() {
            let self_ty = Ty::locator(Name::ident("Self"));
            let receiver_type = match receiver {
                FunctionParamReceiver::Implicit
                | FunctionParamReceiver::Value
                | FunctionParamReceiver::MutValue => self_ty,
                FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                    TypeReference {
                        ty: Box::new(self_ty),
                        mutability: Some(false),
                        lifetime: None,
                    }
                    .into(),
                ),
                FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => {
                    Ty::Reference(
                        TypeReference {
                            ty: Box::new(self_ty),
                            mutability: Some(true),
                            lifetime: None,
                        }
                        .into(),
                    )
                }
            };

            let self_var = self.fresh_type_var();
            let expected = self.type_from_ast_ty(&receiver_type).await?;
            self.unify(self_var, expected).await?;
            self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for param in func.sig.params.iter_mut() {
            let var = self.fresh_type_var();
            let annot_var = self.type_from_ast_ty(&param.ty).await?;
            self.unify(var, annot_var).await?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var).await?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = {
            let mut body = func.body.as_mut();
            self.infer_expr_inner(&mut body).await?
        };

        let ret_var = if matches!(exception_policy, ExceptionReturnPolicy::AutoResult) {
            let body_ty = self.resolve_to_ty(body_var).await?;
            let result_ty = make_std_result_ty(body_ty, std_error_ty());
            self.type_from_ast_ty(&result_ty).await?
        } else if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret).await?;
            self.unify(body_var, annot_var).await?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        self.bind_function_term(fn_var, param_vars.clone(), ret_var);

        let scheme = self.generalize(fn_var).await?;
        self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme));

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var).await?);
        }
        let ret_ty = self.resolve_to_ty(ret_var).await?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        let func_ty = TypeFunction {
            params: param_tys,
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        };
        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    // infer_expr moved to typing::infer_expr

    // infer_block moved to typing::infer_stmt

    // infer_if moved to typing::infer_stmt

    // infer_binop moved to typing::infer_expr

    // infer_unop moved to typing::infer_expr

    // infer_loop moved to typing::infer_stmt

    // infer_while moved to typing::infer_stmt

    // moved: infer_reference, infer_dereference, infer_index, infer_range, infer_splat, infer_splat_dict

    // moved: infer_intrinsic

    // moved: infer_closure

    // infer_match moved to typing::infer_stmt

    // moved: infer_invoke

    async fn apply_pattern_generalization(&self, info: &PatternInfo) -> Result<()> {
        for binding in &info.bindings {
            let scheme = self.generalize(binding.var).await?;
            self.replace_env_entry(&binding.name, EnvEntry::Poly(scheme));
        }
        Ok(())
    }

    // generalize moved to typing/unify.rs

    // build_scheme_type moved to typing/unify.rs

    // scheme_from_term moved to typing/unify.rs

    // instantiate_scheme moved to typing/unify.rs

    // instantiate_scheme_type moved to typing/unify.rs

    async fn scheme_from_method_signature(&self, sig: &FunctionSignature) -> Result<Ty> {
        let fn_var = self.fresh_type_var();
        let mut param_vars = Vec::new();
        for param in &sig.params {
            param_vars.push(self.type_from_ast_ty(&param.ty).await?);
        }
        let ret_var = if let Some(ret) = sig.ret_ty.as_ref() {
            self.type_from_ast_ty(ret).await?
        } else {
            self.unit_type_var()
        };
        self.bind_function_term(fn_var, param_vars, ret_var);
        self.generalize(fn_var).await
    }

    pub(crate) fn extract_trait_bounds(bounds: &TypeBounds) -> Vec<String> {
        bounds
            .bounds
            .iter()
            .filter_map(|expr| match expr.kind() {
                ExprKind::Name(locator) => Some(locator.to_string()),
                ExprKind::Value(value) => match value.as_ref() {
                    Value::Type(Ty::Expr(inner)) => match inner.kind() {
                        ExprKind::Name(locator) => Some(locator.to_string()),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }

    fn register_generic_param(&self, name: &str) -> TypeVarId {
        let var = self.fresh_type_var();
        self.insert_env(name.to_string(), EnvEntry::Mono(var));
        self.inner.borrow_mut().generic_type_vars.insert(var, name.to_string());
        if let Some(scope) = self.inner.borrow_mut().generic_scopes.last_mut() {
            scope.insert(name.to_string());
        }
        var
    }

    // unused: generic_name_in_scope (removed)

    fn insert_env(&self, name: String, entry: EnvEntry) {
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.insert(name, entry);
        }
    }

    fn replace_env_entry(&self, name: &str, entry: EnvEntry) {
        for scope in self.inner.borrow_mut().env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), entry);
                return;
            }
        }
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.insert(name.to_string(), entry);
        }
    }

    fn enter_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.current_level += 1;
        inner.env.push(HashMap::new());
        inner.generic_scopes.push(HashSet::new());
        inner.module_aliases.push(HashMap::new());
        inner.symbol_aliases.push(HashMap::new());
        inner.context_env.push(Vec::new());
    }

    fn exit_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.pop();
        inner.generic_scopes.pop();
        inner.module_aliases.pop();
        inner.symbol_aliases.pop();
        inner.context_env.pop();
        if inner.current_level > 0 {
            inner.current_level -= 1;
        }
    }

    fn push_module_path(&self, name: &str) {
        self.inner.borrow_mut().module_path.push(name.to_string());
    }

    fn pop_module_path(&self) {
        let _ = self.inner.borrow_mut().module_path.pop();
    }

    fn record_module_def(&self, name: &str) {
        let mut inner = self.inner.borrow_mut();
        let path = inner.module_path.with_segment(name.to_string());
        inner.module_defs.insert(path);
        if inner.module_path.is_empty() {
            inner.root_modules.insert(name.to_string());
        }
    }

    fn qualified_name(&self, name: &str) -> Option<QualifiedPath> {
        if self.inner.borrow().module_path.is_empty() {
            None
        } else {
            Some(self.inner.borrow().module_path.with_segment(name.to_string()))
        }
    }

    fn insert_struct_def(&self, name: &Ident, def: TypeStruct) {
        let key = if self.inner.borrow().module_path.is_empty() {
            QualifiedPath::new(vec![name.as_str().to_string()])
        } else {
            self.inner.borrow().module_path.with_segment(name.as_str().to_string())
        };
        self.own_struct_defs_mut().insert(key, def);
    }

    fn insert_enum_def(&self, name: &Ident, def: TypeEnum) {
        let key = if self.inner.borrow().module_path.is_empty() {
            QualifiedPath::new(vec![name.as_str().to_string()])
        } else {
            self.inner.borrow().module_path.with_segment(name.as_str().to_string())
        };
        self.own_enum_defs_mut().insert(key, def);
    }

    /// Normalize a `DefType` RHS's resolved type into what gets bound under
    /// `name`: a structural type is materialized as a named struct, and a
    /// struct from `TypeBuilder::from(SourceType)` has `SourceType`'s fields
    /// merged in. Shared between the const-block and plain-alias paths in
    /// the `ItemKind::DefType` arm of `infer_item_inner`.
    async fn normalize_deftype_value(&self, name: &Ident, resolved: Ty) -> Ty {
        match resolved {
            Ty::Structural(structural) => {
                let struct_ty = TypeStruct {
                    name: name.clone(),
                    generics_params: Vec::new(),
                    repr: ReprOptions::default(),
                    method_sigs: Vec::new(),
                    fields: structural.fields.clone(),
                };
                self.insert_struct_def(name, struct_ty.clone());
                Ty::Struct(struct_ty)
            }
            Ty::Struct(struct_ty) => {
                let mut merged_ty = struct_ty;
                // Merge fields from source struct for TypeBuilder::from(Type)
                if merged_ty.name != *name {
                    let source_name =
                        QualifiedPath::new(vec![merged_ty.name.as_str().to_string()]);
                    let source_def = self.own_struct_defs().get(&source_name).cloned();
                    match source_def {
                        Some(source_def) => {
                            let mut merged = source_def.fields.clone();
                            for f in &merged_ty.fields {
                                if !merged.iter().any(|m| m.name == f.name) {
                                    merged.push(f.clone());
                                }
                            }
                            merged_ty.fields = merged;
                            merged_ty.name = name.clone();
                        }
                        None => {
                            // Don't silently continue with only the new
                            // fields — that would materialize an incomplete
                            // struct for callers to build against. Tell
                            // apart "source's package hasn't loaded yet"
                            // (suspend, then retry the lookup once loaded)
                            // from "source genuinely doesn't exist" (real
                            // error).
                            let registered = source_name
                                .head()
                                .is_some_and(|head| self.typing_ctx.env_ctx.is_registered(head));
                            if registered {
                                self.await_package(source_name.head().unwrap()).await;
                                let found = self.own_struct_defs().get(&source_name).cloned();
                                if let Some(source_def) = found {
                                    let mut merged = source_def.fields.clone();
                                    for f in &merged_ty.fields {
                                        if !merged.iter().any(|m| m.name == f.name) {
                                            merged.push(f.clone());
                                        }
                                    }
                                    merged_ty.fields = merged;
                                    merged_ty.name = name.clone();
                                    self.insert_struct_def(name, merged_ty.clone());
                                    return Ty::Struct(merged_ty);
                                }
                            }
                            self.emit_error(format!(
                                "unknown source type `{}` for type alias `{}`",
                                merged_ty.name.as_str(),
                                name.as_str()
                            ));
                            return Ty::Unknown(TypeUnknown);
                        }
                    }
                }
                self.insert_struct_def(name, merged_ty.clone());
                Ty::Struct(merged_ty)
            }
            Ty::Enum(enum_ty) => {
                self.insert_enum_def(name, enum_ty.clone());
                Ty::Enum(enum_ty)
            }
            other => other,
        }
    }

    fn parent_module_path(&self) -> QualifiedPath {
        self.inner.borrow().module_path
            .parent_n(1)
            .unwrap_or_else(|| QualifiedPath::new(Vec::new()))
    }

    // fresh_type_var moved to typing/unify.rs

    // unit_type_var moved to typing/unify.rs

    // nothing_type_var moved to typing/unify.rs

    // bind moved to typing/unify.rs

    // find moved to typing/unify.rs

    // unify moved to typing/unify.rs

    // occurs_in_term moved to typing/unify.rs

    // occurs_in moved to typing/unify.rs

    // unify_terms moved to typing/unify.rs

    // resolve_to_ty moved to typing/unify.rs

    // term_to_ty moved to typing/unify.rs

    // type_from_ast_ty moved to typing/unify.rs

    async fn lookup_associated_function(&self, locator: &Name) -> Result<Option<TypeVarId>> {
        if let Name::Path(path) = locator {
            if path.segments.len() >= 2 {
                if let Some(method_segment) = path.segments.last() {
                    let method_name = method_segment.as_str();
                    let struct_segments = path
                        .segments
                        .iter()
                        .take(path.segments.len() - 1)
                        .map(|seg| seg.as_str().to_string())
                        .collect::<Vec<_>>();
                        if let Some(struct_name) =
                            self.resolve_segments_key(path.prefix, &struct_segments)
                        {
                            for candidate in self.struct_name_variants_for_path(
                            &struct_name,
                            struct_name.segments.len() == 1,
                        ) {
                            let qualified = candidate.with_segment(method_name.to_string());
                            if let Some(var) = self.lookup_env_var(&qualified.to_key()).await {
                                return Ok(Some(var));
                            }
                            // Only borrow/clone the struct def long enough to
                            // find the one matching method signature — the
                            // local case doesn't need to clone the whole
                            // `TypeStruct` at all (cross-crate lookups
                            // already clone internally via `find_struct`,
                            // since crates now live behind a `RefCell` for
                            // on-demand loading).
                            let local = self.own_struct_defs().get(&candidate).map(|s| {
                                (
                                    false,
                                    s.method_sigs
                                        .iter()
                                        .find(|(n, _)| n == method_name)
                                        .map(|(_, sig)| sig.clone()),
                                )
                            });
                            let (is_cross_crate, found_sig) = if let Some(result) = local {
                                result
                            } else if let Some(s) =
                                self.typing_ctx.env_ctx.find_struct(&candidate)
                            {
                                (
                                    true,
                                    s.method_sigs
                                        .into_iter()
                                        .find(|(n, _)| n == method_name)
                                        .map(|(_, sig)| sig),
                                )
                            } else {
                                let registered = candidate
                                    .head()
                                    .is_some_and(|head| self.typing_ctx.env_ctx.is_registered(head));
                                if registered {
                                    self.await_package(candidate.head().unwrap()).await;
                                    match self.typing_ctx.env_ctx.find_struct(&candidate) {
                                        Some(s) => (
                                            true,
                                            s.method_sigs
                                                .into_iter()
                                                .find(|(n, _)| n == method_name)
                                                .map(|(_, sig)| sig),
                                        ),
                                        None => (false, None),
                                    }
                                } else {
                                    (false, None)
                                }
                            };
                            if is_cross_crate || self.own_struct_defs().contains_key(&candidate) {
                                if is_cross_crate {
                                    self.inner.borrow_mut().cross_crate_struct_refs.insert(candidate.clone());
                                }
                                if let Some(sig) = found_sig {
                                    if !sig.generics_params.is_empty() {
                                        let scheme = self.scheme_from_method_signature(&sig).await.ok();
                                        if let Some(scheme) = scheme {
                                            return Ok(Some(self.instantiate_scheme(&scheme).await));
                                        }
                                    }
                                    if let Some(var) = self.lookup_env_var(method_name).await {
                                        return Ok(Some(var));
                                    }
                                    let fn_ty = self.ty_from_function_signature(&sig)?;
                                    let fn_var = self.type_from_ast_ty(&fn_ty).await?;
                                    return Ok(Some(fn_var));
                                }
                            }
                        }

                        // Enum tuple variant constructors: `Enum::Variant(...)`.
                        let enum_def = self.own_enum_defs().get(&struct_name).cloned();
                        if let Some(enum_def) = enum_def {
                            if let Some(variant) = enum_def
                                .variants
                                .iter()
                                .find(|v| v.name.as_str() == method_name)
                            {
                                self.enter_scope();
                                if !enum_def.generics_params.is_empty() {
                                    for param in &enum_def.generics_params {
                                        let var = self.register_generic_param(param.name.as_str());
                                        let bounds = Self::extract_trait_bounds(&param.bounds);
                                        if !bounds.is_empty() {
                                            self.inner.borrow_mut().generic_trait_bounds.insert(var, bounds);
                                        }
                                    }
                                }
                                let mut params = Vec::new();
                                match &variant.value {
                                    Ty::Unit(_) => {}
                                    Ty::Tuple(tuple_ty) => params.extend(tuple_ty.types.clone()),
                                    other => params.push(other.clone()),
                                }

                                let func_ty = Ty::Function(TypeFunction {
                                    params,
                                    generics_params: Vec::new(),
                                    ret_ty: Some(Box::new(Ty::Enum(enum_def.clone()))),
                                });
                                let func_var = self.type_from_ast_ty(&func_ty).await?;
                                self.exit_scope();
                                return Ok(Some(func_var));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    async fn lookup_locator(&self, locator: &Name) -> Result<TypeVarId> {
        self.lookup_locator_with_resolution(locator).await
            .map(|(var, _)| var)
    }

    async fn lookup_locator_with_resolution(
        &self,
        locator: &Name,
    ) -> Result<(TypeVarId, Option<ResolvedName>)> {
        if self.check_unimplemented_locator(locator) {
            return Ok((self.error_type_var(), None));
        }
        if let Name::Path(path) = locator {
            if path.segments.len() >= 2 {
                let variant_name = path.segments.last().map(|seg| seg.as_str());
                let enum_segments = path
                    .segments
                    .iter()
                    .take(path.segments.len() - 1)
                    .map(|seg| seg.as_str().to_string())
                    .collect::<Vec<_>>();
                if let (Some(variant_name), Some(enum_key)) = (
                    variant_name,
                    self.resolve_segments_key(path.prefix, &enum_segments),
                ) {
                    let enum_def = self.own_enum_defs().get(&enum_key).cloned();
                    if let Some(enum_def) = enum_def {
                        if enum_def
                            .variants
                            .iter()
                            .any(|v| v.name.as_str() == variant_name)
                        {
                            let var = self.fresh_type_var();
                            self.bind(var, Ty::Enum(enum_def));
                            let qualified = enum_key.with_segment(variant_name.to_string());
                            return Ok((
                                var,
                                Some(ResolvedName {
                                    namespace: ResolvedNameNamespace::Value,
                                    path: qualified,
                                }),
                            ));
                        }
                    }
                }
            }
        }
        if let Some(ident) = locator.as_ident() {
            let name = ident.as_str();
            if let Some(var) = self.lookup_env_var(name).await {
                return Ok((var, None));
            }
            if !self.inner.borrow().module_path.is_empty() {
                let qualified = self.inner.borrow().module_path.with_segment(name.to_string());
                if let Some(var) = self.lookup_env_var(&qualified.to_key()).await {
                    return Ok((
                        var,
                        Some(ResolvedName {
                            namespace: ResolvedNameNamespace::Value,
                            path: qualified,
                        }),
                    ));
                }
            }
        }
        let key = match self.resolve_locator_key(locator) {
            Some(key) => key,
            None => {
                // In value position, names like i64, bool, str, type
                // refer to types — bind them as type-level values.
                if let Some(ident) = locator.as_ident() {
                    let name = ident.as_str();
                    if name == "type" {
                        let var = self.fresh_type_var();
                        self.bind(var, Ty::Type(TypeType::new(Span::null())));
                        return Ok((var, Some(ResolvedName {
                            namespace: ResolvedNameNamespace::Type,
                            path: QualifiedPath::new(vec![name.to_string()]),
                        })));
                    }
                    if let Some(prim) = crate::typing::unify::primitive_from_name(name) {
                        let var = self.fresh_type_var();
                        self.bind(var, Ty::Type(TypeType {
                            span: Span::null(),
                            inner: Some(Box::new(Ty::Primitive(prim))),
                        }));
                        return Ok((var, Some(ResolvedName {
                            namespace: ResolvedNameNamespace::Type,
                            path: QualifiedPath::new(vec![name.to_string()]),
                        })));
                    }
                }
                self.emit_error(format!("unresolved symbol: {}", locator));
                return Ok((self.error_type_var(), None));
            }
        };
        if self.own_struct_defs().contains_key(&key) || self.own_enum_defs().contains_key(&key) {
            let var = self.fresh_type_var();
            self.bind(var, Ty::Type(TypeType::new(Span::null())));
            return Ok((
                var,
                Some(ResolvedName {
                    namespace: ResolvedNameNamespace::Type,
                    path: key,
                }),
            ));
        }
        if let Some(var) = self.lookup_env_var(&key.to_key()).await {
            return Ok((
                var,
                Some(ResolvedName {
                    namespace: ResolvedNameNamespace::Value,
                    path: key,
                }),
            ));
        }
        // Fallback: workspace crates may have this function registered
        let workspace_sig = self.typing_ctx.env_ctx.find_function_sig(&key);
        if let Some(sig) = workspace_sig {
            let fn_ty = self.ty_from_function_signature(&sig)?;
            let var = self.type_from_ast_ty(&fn_ty).await?;
            return Ok((var, Some(ResolvedName {
                namespace: ResolvedNameNamespace::Value,
                path: key,
            })));
        }
        self.emit_error(format!("unresolved symbol: {}", key.to_key()));
        Ok((self.error_type_var(), None))
    }

    fn resolve_alias_locator(&self, locator: &Name) -> Option<QualifiedPath> {
        match locator {
            Name::Ident(ident) => self.lookup_symbol_alias(ident.as_str()),
            Name::Path(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(module_path.join(&extra));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(symbol_path.join(&extra));
                    }
                }
                None
            }
            Name::ParameterPath(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.ident.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.ident.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(module_path.join(&extra));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.ident.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.ident.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(symbol_path.join(&extra));
                    }
                }
                None
            }
        }
    }

    fn lookup_symbol_alias(&self, name: &str) -> Option<QualifiedPath> {
        for scope in self.inner.borrow().symbol_aliases.iter().rev() {
            if let Some(target) = scope.get(name) {
                return Some(target.clone());
            }
        }
        None
    }

    fn lookup_module_alias(&self, name: &str) -> Option<QualifiedPath> {
        for scope in self.inner.borrow().module_aliases.iter().rev() {
            if let Some(path) = scope.get(name) {
                return Some(path.clone());
            }
        }
        None
    }

    async fn lookup_env_var(&self, name: &str) -> Option<TypeVarId> {
        if let Some(var) = self.lookup_env_var_direct(name).await {
            return Some(var);
        }
        let should_retry = self
            .inner
            .borrow_mut()
            .resolution_hook
            .as_mut()
            .map(|hook| hook.resolve_symbol(name))
            .unwrap_or(false);
        if should_retry {
            return self.lookup_env_var_direct(name).await;
        }
        None
    }

    async fn lookup_env_var_direct(&self, name: &str) -> Option<TypeVarId> {
        // The scope scan itself is confined to a single `Ref` borrow that
        // ends before returning -- iterating `self.env` directly as the
        // `for` loop's iterable would otherwise keep that borrow alive for
        // the whole loop (a `for` loop's iterable temporary lives for the
        // entire loop), including across the `Poly` branch's `.await` below.
        let found = {
            let inner = self.inner.borrow();
            inner.env.iter().rev().find_map(|scope| scope.get(name).cloned())
        };
        match found {
            Some(EnvEntry::Mono(var)) => Some(var),
            Some(EnvEntry::Poly(scheme)) => Some(self.instantiate_scheme(&scheme).await),
            None => None,
        }
    }

    async fn symbol_var(&self, name: &Ident) -> TypeVarId {
        let key = name.as_str().to_string();
        if let Some(var) = self.lookup_env_var(&key).await {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    fn register_symbol(&self, name: &Ident) {
        let key = name.as_str().to_string();
        let var = self.fresh_type_var();
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.entry(key).or_insert(EnvEntry::Mono(var));
        }
    }

    fn emit_error(&self, message: impl Into<String>) {
        let span = self.inner.borrow().current_span;
        self.emit_error_with_span(span, message);
    }

    fn emit_error_with_span(&self, span: Option<Span>, message: impl Into<String>) {
        let message = message.into();
        let mut inner = self.inner.borrow_mut();
        if inner.lossy_mode {
            if let Some(span) = span {
                inner.diagnostics
                    .push(TypingDiagnostic::warning_with_span(message, span));
            } else {
                inner.diagnostics.push(TypingDiagnostic::warning(message));
            }
        } else {
            inner.has_errors = true;
            if let Some(span) = span {
                inner.diagnostics
                    .push(TypingDiagnostic::error_with_span(message, span));
            } else {
                inner.diagnostics.push(TypingDiagnostic::error(message));
            }
        }
    }

    fn span_option(&self, span: Span) -> Option<Span> {
        if span.is_null() {
            None
        } else {
            Some(span)
        }
    }

    fn span_or_previous(&self, span: Span, previous: Option<Span>) -> Option<Span> {
        if span.is_null() {
            previous
        } else {
            Some(span)
        }
    }

    fn error_with_span(&self, err: Error, span: Option<Span>) -> Error {
        let Some(span) = span else {
            return err;
        };
        if let Error::Diagnostic(ref diagnostic) = err {
            if diagnostic.span.is_some() {
                return err;
            }
        }
        Error::diagnostic(Diagnostic::error(err.to_string()).with_span(span))
    }

    fn error_with_current_span(&self, message: impl Into<String>) -> Error {
        let message = message.into();
        if let Some(span) = self.inner.borrow().current_span {
            Error::diagnostic(Diagnostic::error(message).with_span(span))
        } else {
            Error::from(message)
        }
    }

    #[allow(dead_code)]
    fn emit_warning(&self, message: impl Into<String>) {
        self.inner.borrow_mut().diagnostics.push(TypingDiagnostic::warning(message));
    }

     fn error_type_var(&self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind_error(var);
        var
    }

    // unused: primitive_from_name (removed)

    fn expect_reference<'a>(
        &self,
        var: TypeVarId,
        context: &'a str,
    ) -> BoxFuture<'a, Result<TypeVarId>> {
        let this = self.clone();
        Box::pin(async move {
            let root = this.find(var);
            // Extracted to an owned local first: the `Bound(Reference)` and
            // `Link` arms below await, and matching directly on
            // `this.inner.borrow()...` would extend the guard's scope
            // across those `.await`s.
            let root_kind = this.inner.borrow().type_vars[root].kind.clone();
            match root_kind {
                TypeVarKind::Unbound { .. } => {
                    let inner = this.fresh_type_var();
                    this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Reference(TypeReference {
                        ty: Box::new(Ty::infer_var(inner)),
                        mutability: None,
                        lifetime: None,
                    }));
                    Ok(inner)
                }
                TypeVarKind::Bound(Ty::Reference(reference)) => match reference.ty.as_ref() {
                    Ty::InferVar(infer) => Ok(infer.id),
                    other => this.type_from_ast_ty(other).await,
                },
                TypeVarKind::Link(next) => this.expect_reference(next, context).await,
                _other => {
                    this.emit_error(format!(
                        "expected reference value for {} (hint: add `&`/`&mut` or change the annotation to a non-reference type)",
                        context
                    ));
                    let placeholder = this.error_type_var();
                    this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Reference(TypeReference {
                        ty: Box::new(Ty::infer_var(placeholder)),
                        mutability: None,
                        lifetime: None,
                    }));
                    Ok(placeholder)
                }
            }
        })
    }

    async fn generalize_symbol(&self, name: &str, var: TypeVarId) -> Result<()> {
        let scheme = self.generalize(var).await?;
        self.replace_env_entry(name, EnvEntry::Poly(scheme));
        Ok(())
    }

    // ensure_numeric moved to typing::solver

    // ensure_bool moved to typing::solver

    // ensure_integer moved to typing::solver

    // ensure_function moved to typing::solver

    fn ty_from_function_signature(&self, sig: &FunctionSignature) -> Result<Ty> {
        self.validate_extern_c_signature(sig);
        let mut params = Vec::new();
        for param in &sig.params {
            params.push(param.ty.clone());
        }
        let ret_ty = sig.ret_ty.clone().unwrap_or_else(|| Ty::Unit(TypeUnit));
        Ok(Ty::Function(TypeFunction {
            params,
            generics_params: sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        }))
    }

    fn validate_extern_c_signature(&self, sig: &FunctionSignature) {
        if !sig.abi.is_c() {
            return;
        }
        for param in &sig.params {
            if self.is_disallowed_c_string_type(&param.ty) {
                self.emit_error(format!(
                    "extern \"C\" functions must use &CStr for string parameters: {}",
                    param.name
                ));
            }
        }
        if let Some(ret_ty) = &sig.ret_ty {
            if self.is_disallowed_c_string_type(ret_ty) {
                self.emit_error("extern \"C\" functions must use &CStr for string return types");
            }
        }
    }

    fn is_disallowed_c_string_type(&self, ty: &Ty) -> bool {
        if self.is_cstr_reference(ty) {
            return false;
        }
        if self.is_string_like_type(ty) {
            return true;
        }
        if let Ty::Reference(reference) = ty {
            if self.is_string_like_type(reference.ty.as_ref()) {
                return true;
            }
        }
        false
    }

    fn is_cstr_reference(&self, ty: &Ty) -> bool {
        let Ty::Reference(reference) = ty else {
            return false;
        };
        self.type_name(reference.ty.as_ref()) == Some("CStr")
    }

    fn is_string_like_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Primitive(TypePrimitive::String) => true,
            _ => matches!(
                self.type_name(ty),
                Some("str") | Some("String") | Some("string")
            ),
        }
    }

    fn type_name<'a>(&self, ty: &'a Ty) -> Option<&'a str> {
        match ty {
            Ty::Struct(struct_ty) => Some(struct_ty.name.as_str()),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(locator) => match locator {
                    Name::Ident(ident) => Some(ident.as_str()),
                    Name::Path(path) => path.segments.last().map(|seg| seg.as_str()),
                    Name::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str()),
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn struct_name_from_expr(&self, expr: &Expr) -> Option<QualifiedPath> {
        match expr.kind() {
            ExprKind::Name(locator) => {
                let name = match locator {
                    Name::ParameterPath(path) => path
                        .segments
                        .last()
                        .map(|seg| seg.ident.as_str().to_string())?,
                    Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string())?,
                    Name::Ident(ident) => ident.as_str().to_string(),
                };
                if name == "Self" {
                    self.inner.borrow()
                        .impl_stack
                        .last()
                        .and_then(|ctx| ctx.as_ref())
                        .map(|ctx| ctx.struct_name.clone())
                } else {
                    Some(QualifiedPath::new(vec![name]))
                }
            }
            ExprKind::Value(value) => match &**value {
                Value::Type(Ty::Struct(struct_ty)) => Some(QualifiedPath::new(vec![struct_ty
                    .name
                    .as_str()
                    .to_string()])),
                Value::Type(Ty::Enum(enum_ty)) => {
                    Some(QualifiedPath::new(vec![enum_ty.name.as_str().to_string()]))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

fn tokenize_macro_tokens(tokens: &str) -> Vec<&str> {
    tokens.split_whitespace().collect()
}

fn is_ident_token(token: &str) -> bool {
    let mut chars = token.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn find_ident_after_keyword(tokens: &[&str], keyword: &str) -> Option<String> {
    let mut iter = tokens.iter().peekable();
    while let Some(token) = iter.next() {
        if *token == keyword {
            for next in iter.by_ref() {
                if is_ident_token(next) {
                    return Some(next.to_string());
                }
            }
            break;
        }
    }
    None
}

fn find_first_type_ident(tokens: &[&str]) -> Option<String> {
    for token in tokens {
        if is_ident_token(token) {
            if token.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                return Some((*token).to_string());
            }
        }
    }
    None
}

/// Infer the fragment kind for an unkinded quote based on its block shape.
/// - Single trailing expression and no statements => Expr
/// - All items at top level => Item
/// - Otherwise => Stmt
// moved to typing::infer_expr::infer_quote_kind

impl AstTypeInferencer {
    pub async fn infer_expression(&self, expr: &mut Expr) -> Result<()> {
        let var = self.infer_expr_inner(expr).await?;
        let ty = self.resolve_to_ty(var).await?;
        expr.set_ty(ty);
        Ok(())
    }

    pub fn push_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.push(HashMap::new());
        inner.generic_scopes.push(HashSet::new());
        inner.context_env.push(Vec::new());
        inner.current_level += 1;
    }

    pub fn pop_scope(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.env.pop();
        inner.generic_scopes.pop();
        inner.context_env.pop();
        if inner.current_level > 0 {
            inner.current_level -= 1;
        }
    }

    pub async fn bind_variable(&self, name: &str, ty: Ty) {
        let type_var = match self.type_from_ast_ty(&ty).await {
            Ok(var) => var,
            Err(_) => self.fresh_type_var(),
        };
        if let Some(current_env) = self.inner.borrow_mut().env.last_mut() {
            current_env.insert(name.to_string(), EnvEntry::Mono(type_var));
        }
    }

    fn push_context_binding(&self, ty: Ty, expr: Expr) {
        if let Some(scope) = self.inner.borrow_mut().context_env.last_mut() {
            scope.push(ContextBinding { ty, expr });
        }
    }

    fn resolve_context_argument(&self, param: &FunctionParam) -> Option<Expr> {
        if !param.is_context {
            return None;
        }

        self.inner.borrow()
            .context_env
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .find(|binding| binding.ty == param.ty)
            .map(|binding| binding.expr.clone())
    }
}

/// Pre-walk: collect quote-valued DefConst items and replace splice(NAME)
/// expressions with extracted items. Recurses into modules and impl blocks.
/// Called by the scheduler before typing.


impl AstTypeInferencer {
    fn locator_tail_name(&self, locator: &Name) -> Option<String> {
        match locator {
            Name::Ident(ident) => Some(ident.as_str().to_string()),
            Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string()),
            Name::ParameterPath(path) => path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().to_string()),
        }
    }

    fn resolution_parsed_path(&self, locator: &Name) -> Option<ParsedPath> {
        let (prefix, segments) = match locator {
            Name::Ident(ident) => (PathPrefix::Plain, vec![ident.as_str().to_string()]),
            Name::Path(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.as_str().to_string())
                    .collect(),
            ),
            Name::ParameterPath(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.ident.as_str().to_string())
                    .collect(),
            ),
        };
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        Some(ParsedPath { prefix, segments })
    }
}

impl AstTypeInferencer {
}


pub fn impl_self_ty_name(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Name(name) => Some(name.to_string()),
        _ => None,
    }
}

#[cfg(test)]
mod deftype_normalize_tests {
    use super::*;
    use fp_core::package::provider::{PackageProvider, ProviderResult};
    use fp_core::package::{PackageDescriptor, PackageId};
    use fp_core::workspace::WorkspaceContext;
    use std::future::Future;
    use std::rc::Rc;
    use std::sync::Arc;

    struct NoopProvider;
    impl PackageProvider for NoopProvider {
        fn list_packages(&self) -> ProviderResult<Vec<PackageId>> {
            Ok(Vec::new())
        }
        fn load_package(&self, id: &PackageId) -> ProviderResult<Arc<PackageDescriptor>> {
            Err(fp_core::package::provider::ProviderError::other(format!(
                "not needed for this test: {id}"
            )))
        }
        fn refresh(&self) -> ProviderResult<()> {
            Ok(())
        }
    }

    fn new_inferencer(register_pending_package: Option<&str>) -> AstTypeInferencer {
        let mut workspace = WorkspaceContext::new();
        if let Some(name) = register_pending_package {
            workspace.register_provider(name, Arc::new(NoopProvider));
        }
        let typing_ctx = Rc::new(TypingContext::new(Rc::new(workspace)));
        AstTypeInferencer::new(typing_ctx)
    }

    fn source_struct_ty(source_name: &str) -> Ty {
        Ty::Struct(TypeStruct {
            name: Ident::new(source_name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            method_sigs: Vec::new(),
            fields: vec![StructuralField::new(
                Ident::new("extra"),
                Ty::Primitive(TypePrimitive::Bool),
            )],
        })
    }

    /// A registered-but-unloaded package's source type must genuinely
    /// suspend (`Poll::Pending`) rather than degrade to `Ty::Unknown` and
    /// let the caller retry the whole module later -- that degrade-and-retry
    /// behavior is exactly what real suspension (Phase 3b) replaces. Nothing
    /// in this test ever loads "SourcePkg", so polling more than once would
    /// hang forever; a single manual poll is enough to prove it suspends
    /// instead of silently returning a placeholder.
    #[test]
    fn suspends_when_source_type_belongs_to_an_unloaded_package() {
        let typer = new_inferencer(Some("SourcePkg"));
        let alias = Ident::new("Alias");
        let mut fut = std::pin::pin!(typer.normalize_deftype_value(&alias, source_struct_ty("SourcePkg")));
        let waker = std::task::Waker::noop();
        let mut cx = std::task::Context::from_waker(waker);
        match fut.as_mut().poll(&mut cx) {
            std::task::Poll::Pending => {}
            std::task::Poll::Ready(_) => {
                panic!("expected suspension while the source package is unloaded, got Ready")
            }
        }
    }

    #[test]
    fn errors_when_source_type_is_genuinely_unknown() {
        let typer = new_inferencer(None);
        let resolved = crate::block_on(typer.normalize_deftype_value(
            &Ident::new("Alias"),
            source_struct_ty("NoSuchSource"),
        ));

        assert!(matches!(resolved, Ty::Unknown(_)));
        assert!(
            typer
                .inner
                .borrow()
                .diagnostics
                .iter()
                .any(|d| matches!(d.level, TypingDiagnosticLevel::Error)),
            "a genuinely unknown source type must emit a real error, not silently continue"
        );
    }
}
