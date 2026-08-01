use std::collections::{HashMap, HashSet};

mod api;
pub mod context;
mod diagnostics;
pub mod infer_expr;
pub mod infer_stmt;
mod items;
mod lifecycle;
mod lookup;
mod registration;
pub mod runtime;
pub mod solver;
pub(crate) mod support;
pub mod types;
pub mod unify;
pub use context::TypingContext;
pub use runtime::{materialize_type_with_hooks, type_from_value, TypeMaterializeHooks};
pub(crate) use support::std_result_inner_types;
pub use support::{block_on, default_extern_prelude, impl_self_ty_name};
pub use types::{
    ExprId, GenericMonorph, ResolvedName, ResolvedNameNamespace, ResolvedNameTable,
    TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome,
};

use crate::support::{
    attrs_has_feature, attrs_has_name, detect_lossy_mode, find_first_type_ident,
    find_ident_after_keyword, is_future_like_ty, is_std_result_ty, is_std_task_future_ty,
    make_std_result_ty, make_std_task_future_ty, std_error_ty, std_task_future_inner_ty,
    tokenize_macro_tokens,
};
use fp_core::hir::*;
use fp_core::error::Error;
use fp_core::module::path::QualifiedPath;
use fp_core::package::PackageCrate;
use fp_core::span::Span;
use std::cell::RefCell;
use std::rc::Rc;
use fp_core::ast::{FunctionSignature, StructuralField};

// intrinsic and op kinds handled in submodules
pub(crate) fn typing_error(msg: impl Into<String>) -> Error {
    Error::from(msg.into())
}

/// A boxed, pinned future borrowing for lifetime `'a` -- the standard shape
/// for a recursive/mutually-recursive async fn's return type, since Rust
/// can't give a directly- or mutually-recursive `async fn` a statically
/// sized state machine (the recursive call's future would need to contain
/// itself). Used throughout the typing SCC (`unify`, `infer_expr`,
/// `infer_stmt`, and this crate root) at
/// exactly the functions that call themselves -- everything else in that
/// call graph is a plain `async fn` with no heap allocation of its own.
pub(crate) type BoxFuture<'a, T> = std::pin::Pin<Box<dyn std::future::Future<Output = T> + 'a>>;

pub(crate) type TypeVarId = usize;

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

use crate::unify::{TypeVar, TypeVarKind};
mod state;
pub(crate) use state::{
    ContextBinding, EnvEntry, FunctionTypeInfo, ImplContext, LoopContext, PatternBinding,
    PatternInfo,
};
#[derive(Clone, Copy)]
pub(crate) enum ExceptionReturnPolicy {
    Disabled,
    ExplicitResult,
    AutoResult,
}
struct ExceptionContext {
    policy: ExceptionReturnPolicy,
}
struct ExceptionContextGuard {
    pub(crate) inner: Rc<RefCell<Inner>>,
}
impl Drop for ExceptionContextGuard {
    fn drop(&mut self) {
        self.inner.borrow_mut().exception_stack.pop();
    }
}

/// The mutually-recursive SCC's own per-pass state — every field here is
/// reached only through `HirTypeInferencer::inner.borrow()`/`borrow_mut()`,
/// scoped to short synchronous stretches that never span an `.await` (the
/// same discipline already used for `TypingContext`'s `RefCell` fields
/// elsewhere in this crate). This split exists so that multiple concurrent
/// item-resolution tasks (see `HirTypeInferencer::tasks`) can
/// each hold their own cheap `Rc::clone` of the same underlying state,
/// instead of requiring one exclusive `&mut HirTypeInferencer` per task.
///
/// No lifetime parameter (unlike an earlier iteration of this split): the
/// only field that ever needed one (`ctx: Option<&'ctx SharedScopedContext>`)
/// was dead -- nothing in the crate ever read it, it was a holdover from
/// before `TypingContext` existed -- and removing it, along with the now
///-unnecessary `+ 'ctx` bound on `resolution_hook`, lets `HirTypeInferencer`
/// be plain `Clone` + effectively `'static`, which `HirTypeInferencer::tasks`'
/// `Executor::spawn` (bound `+ 'static`) requires of anything it spawns.
struct Inner {
    pub(crate) type_vars: Vec<TypeVar>,
    pub(crate) env: Vec<HashMap<String, EnvEntry>>,
    pub(crate) generic_scopes: Vec<HashSet<String>>,
    pub(crate) enum_variants: HashMap<QualifiedPath, Vec<QualifiedPath>>,
    pub(crate) trait_method_sigs: HashMap<String, HashMap<String, FunctionSignature>>,
    pub(crate) extern_function_signatures: HashMap<QualifiedPath, FunctionSignature>,
    pub(crate) impl_traits: HashMap<QualifiedPath, HashSet<String>>,
    pub(crate) generic_trait_bounds: HashMap<TypeVarId, Vec<String>>,
    pub(crate) impl_stack: Vec<Option<ImplContext>>,
    pub(crate) module_path: QualifiedPath,
    pub(crate) module_defs: HashSet<QualifiedPath>,
    pub(crate) module_scope_depths: Vec<usize>,
    pub(crate) root_modules: HashSet<String>,
    pub(crate) extern_prelude: HashSet<String>,
    pub(crate) module_aliases: Vec<HashMap<String, QualifiedPath>>,
    pub(crate) symbol_aliases: Vec<HashMap<String, QualifiedPath>>,
    pub(crate) unimplemented_symbols: HashSet<QualifiedPath>,
    pub(crate) current_level: usize,
    pub(crate) diagnostics: Vec<TypingDiagnostic>,
    pub(crate) has_errors: bool,
    pub(crate) literal_ints: HashSet<TypeVarId>,
    pub(crate) loop_stack: Vec<LoopContext>,
    pub(crate) lossy_mode: bool,
    pub(crate) hashmap_args: HashMap<TypeVarId, (TypeVarId, TypeVarId)>,
    pub(crate) context_env: Vec<Vec<ContextBinding>>,
    pub(crate) exception_mode: bool,
    pub(crate) exception_stack: Vec<ExceptionContext>,
    pub(crate) current_span: Option<Span>,
    pub(crate) resolution_hook: Option<Box<dyn TypeResolutionHook>>,
    pub(crate) resolved_names: ResolvedNameTable,
    /// Maps a registered generic parameter's own type var back to its name
    /// -- lets `resolve_to_ty` resolve a still-abstract generic parameter
    /// (one nothing has bound to a concrete type, by design, since it's
    /// generic) to a plain name reference (`Ty::ident`) instead of erroring
    /// as an unresolved type variable. A name reference (not `Ty::GenericVar`,
    /// which is index-based and belongs to this crate's separate
    /// let-polymorphism scheme machinery in `build_generalized_ty`/
    /// `instantiate_poly_ty`) matches how a generic parameter's *declared*
    /// type annotations (`fn f<T>(x: T) -> T`) are already represented --
    /// `T` lowers as a plain identifier expression -- and how downstream
    /// HIR/MIR generic-instantiation inference (`infer_generic_from_type_expr`
    /// in fp-backend) matches a generic parameter by name. Consulted by
    /// `resolve_to_ty`; entries are propagated to the surviving root when
    /// two `Unbound` vars merge (see `unify`'s `merge_generic_identity_into`
    /// call) so the identity isn't lost regardless of which side survives.
    pub(crate) generic_type_vars: HashMap<TypeVarId, String>,
    /// Structs (and their `impl` blocks) resolved from a workspace crate
    /// rather than the local one — e.g. `std::meta::TypeBuilder` via
    /// `TypeBuilder::new(...)`. Reported out so the driver can predeclare
    /// the owning crate's impl items alongside the file's own when lowering
    /// to HIR, since MIR lowering's call-target resolution only ever sees
    /// impls declared in the same HIR program.
    pub(crate) cross_crate_struct_refs: HashSet<QualifiedPath>,
}

/// A cheap, `Clone`-able handle (an `Rc`-wrapped state, not a full copy).
/// `typing_ctx`/`own_crate` are already `Rc`(`<RefCell<_>>`)-based and read
/// constantly throughout the SCC, so they stay direct fields (no double
/// indirection through `inner`) — only the ~30 fields that were plain owned
/// collections before this conversion moved into `Inner`.
#[derive(Clone)]
pub struct HirTypeInferencer {
    /// Shared mutable state with the driver: resolved consts, types,
    /// module resolution, expression resolution, diagnostics, and the
    /// package-waker registry that makes `await_package` genuinely suspend.
    pub(crate) typing_ctx: std::rc::Rc<crate::context::TypingContext>,
    /// The driver's shared task pool (see `CompilerState::tasks`) --
    /// concurrent item-resolution (one task per const/type-alias item,
    /// spawned during `predeclare_item`) and generic-monomorphization-ready
    /// signals (`infer_generic_function_call_body`) both spawn into this.
    /// Deliberately not a field of `typing_ctx`/`TypingContext`: scheduling
    /// is the driver's concern, not typing data -- this is just a cheap
    /// `Rc` handle to the same pool the driver itself owns and drives
    /// (`CompilerDriver::run_pool_to_idle`). Defaults to a fresh, empty,
    /// never-driven pool (see `new`) so standalone/test callers that never
    /// touch generics or nested comptime tasks don't need to know this
    /// field exists; real driver call sites plug in the shared one via
    /// `with_tasks`.
    pub(crate) tasks: std::rc::Rc<fp_core::executor::Executor<fp_core::error::Result<()>>>,
    /// The discovering compile unit's own `AstId`, as a plain string (this
    /// crate can't name `fp-compiler`'s `AstId` type) -- stamped onto every
    /// `GenericMonorph` this typer pushes (see that struct's doc comment)
    /// so `CompilerDriver::handle_resolved_task`, which runs with no
    /// compile-unit-specific context of its own, still knows which stored
    /// `File` to search for the specialized function's `ItemId`. Defaults
    /// to empty for standalone/test callers that never push a
    /// `GenericMonorph`; real driver call sites set it via `with_ast_key`.
    pub(crate) ast_key: String,
    /// This crate's own registry of definitions — struct_defs, enum_defs,
    /// function_sigs, trait_defs — shared (via `Rc<RefCell<_>>`) with the
    /// root `WorkspaceContext`, which already holds every other crate the
    /// same way. Reads/writes go through `own_struct_defs[_mut]` etc. below;
    /// there is deliberately no separate local copy of this data — the
    /// "current crate" is just one more entry in the same root registry
    /// that cross-crate lookups (`env_ctx.find_struct`/`find_function_sig`)
    /// already search.
    pub(crate) own_crate: Rc<RefCell<PackageCrate>>,
    pub(crate) inner: Rc<RefCell<Inner>>,
}

impl HirTypeInferencer {
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
    pub fn with_own_crate(self, krate: Rc<RefCell<PackageCrate>>) -> Self {
        Self {
            own_crate: krate,
            ..self
        }
    }

    /// Use the driver's real, shared task pool instead of the fresh, empty
    /// one `new()` creates — so tasks this typer spawns (const/type-alias
    /// resolution, generic-monomorphization-ready signals) and tasks it
    /// awaits (`await_comptime`/`force`'s `.contains` checks) are visible to
    /// `CompilerDriver::run_pool_to_idle`, not stranded in a pool nothing
    /// ever drives.
    pub fn with_tasks(
        self,
        tasks: Rc<fp_core::executor::Executor<fp_core::error::Result<()>>>,
    ) -> Self {
        Self { tasks, ..self }
    }

    /// Stamp this compile unit's own `AstId` (as a plain string) onto every
    /// `GenericMonorph` this typer pushes -- see `HirTypeInferencer::ast_key`'s
    /// doc comment for why the driver needs it back.
    pub fn with_ast_key(self, ast_key: impl Into<String>) -> Self {
        Self {
            ast_key: ast_key.into(),
            ..self
        }
    }

    pub fn new(typing_ctx: std::rc::Rc<crate::context::TypingContext>) -> Self {
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
            cross_crate_struct_refs: HashSet::new(),
        };
        let inferencer = Self {
            typing_ctx,
            own_crate: Rc::new(RefCell::new(PackageCrate::default())),
            inner: Rc::new(RefCell::new(inner)),
            tasks: Rc::new(fp_core::executor::Executor::new()),
            ast_key: String::new(),
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
        self.inner
            .borrow()
            .exception_stack
            .last()
            .map(|ctx| ctx.policy)
            .unwrap_or(ExceptionReturnPolicy::Disabled)
    }

    fn push_exception_context(&self, policy: ExceptionReturnPolicy) -> ExceptionContextGuard {
        self.inner
            .borrow_mut()
            .exception_stack
            .push(ExceptionContext { policy });
        ExceptionContextGuard {
            inner: self.inner.clone(),
        }
    }

    fn record_hashmap_args(&self, map_var: TypeVarId, key_var: TypeVarId, value_var: TypeVarId) {
        self.inner
            .borrow_mut()
            .hashmap_args
            .insert(map_var, (key_var, value_var));
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
            let kind = self
                .inner
                .borrow()
                .type_vars
                .get(current)
                .map(|var| var.kind.clone());
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
                let ExprKind::Name(name) = expr.kind() else {
                    return None;
                };
                if let Some(inner) = self.heap_inner_ty(ty) {
                    return self
                        .contains_illegal_struct_recursion(inner, target, true, visiting, path);
                }
                let Some(name) = self.name_tail(name) else {
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
}

#[cfg(test)]
mod deftype_normalize_tests {
    use super::*;
    use fp_core::hir::Ident;
    use fp_core::package::provider::{PackageProvider, ProviderResult};
    use fp_core::package::{PackageDescriptor, PackageId};
    use fp_core::workspace::WorkspaceContext;
    use std::future::Future;
    use std::rc::Rc;
    use std::sync::Arc;
    use fp_core::ast::{StructuralField, TypePrimitive, TypeStruct};

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

    fn new_inferencer(register_pending_package: Option<&str>) -> HirTypeInferencer {
        let mut workspace = WorkspaceContext::new();
        if let Some(name) = register_pending_package {
            workspace.register_provider(name, Arc::new(NoopProvider));
        }
        let typing_ctx = Rc::new(TypingContext::new(Rc::new(workspace)));
        HirTypeInferencer::new(typing_ctx)
    }

    fn source_struct_ty(source_name: &str) -> Ty {
        Ty::Struct(TypeStruct {
            name: Ident::new(source_name),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
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
        let mut fut =
            std::pin::pin!(typer.normalize_deftype_value(&alias, source_struct_ty("SourcePkg")));
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
        let resolved = crate::block_on(
            typer.normalize_deftype_value(&Ident::new("Alias"), source_struct_ty("NoSuchSource")),
        );

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
