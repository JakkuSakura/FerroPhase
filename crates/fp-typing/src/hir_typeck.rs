use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::{Error, Result};
use fp_core::executor::{ExecutorHandle, TaskHandle};
use fp_core::hir;
use fp_core::hir::ty::{self, AdtDef, AdtFlags, GenericArg, ReprFlags, ReprOptions, Ty, TyKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;

use crate::TypingContext;
use crate::types::{GenericCallResolution, TypeckResults};
use std::rc::Rc;

/// State shared by every per-item type-checking task spawned for one
/// package's HIR (see `typecheck_item`) — as opposed to `HirTypeChecker`'s
/// own fields (`locals`, `generic_scopes`, ...), which are scoped to the
/// single item currently being checked and must NOT be shared, since
/// multiple items' checks can be concurrently in-flight (one suspended
/// awaiting another's task) on the same `ExecutorHandle`.
pub struct TypingShared {
    program: Rc<hir::Program>,
    /// Every item task writes its own contribution here as it finishes;
    /// another item's task awaiting this one (see `expr_path_ty`'s
    /// same-package `Const` lookup) reads back through the same cell —
    /// this is what makes same-package forward references resolve
    /// regardless of `program.items`' textual order.
    results: RefCell<TypeckResults>,
    typing_context: Option<Rc<TypingContext>>,
    executor: ExecutorHandle,
    /// Lazily-built, memoized snapshot of every item `method_output` scans
    /// to find a method's owning `impl` — this program's own items plus
    /// every other loaded package's (`typing_context`'s `env_ctx::
    /// hir_definitions()`). Neither source changes once type checking of
    /// this program has begun, but `method_output` runs once per method
    /// call *expression*, and both sources were previously being cloned
    /// (whole HIR programs, for every other package) from scratch on every
    /// single call — for a large program (e.g. the vendored std library)
    /// this made every method call pay an O(workspace size) cost. Built
    /// once (shared across every item's task, since it's expensive,
    /// read-only-once-built, and doesn't depend on which item is currently
    /// being checked), reused as a cheap `Rc` clone for the rest of the
    /// check.
    impl_lookup_items: RefCell<Option<Rc<Vec<hir::Item>>>>,
    /// Lazily-built, memoized fast-reject index over `impl_lookup_items`:
    /// for every `impl` item whose self-type is a resolved nominal path
    /// (`TypeExprKind::Path` with `Res::Def(def_id)` — a struct/enum's own
    /// real `DefId`, already resolved during HIR lowering, before this
    /// pass ever runs), maps `def_id` to that impl's index in
    /// `impl_lookup_items`. `method_output` used to fully type-check every
    /// impl's self-type (`check_type_expr`, plus a `generic_args_compatible`
    /// call) before even checking whether it could possibly match the
    /// receiver — once per method-call/index expression, over every impl
    /// in the whole workspace. For an ADT receiver (the overwhelming common
    /// case), only impls bucketed under that exact `DefId` can ever match
    /// (see `method_output`'s own `matches_receiver` match arms: an ADT
    /// receiver only ever matches an impl whose self-type also resolves to
    /// `TyKind::Adt` with the same `did`), so this index lets that lookup
    /// skip straight to the real candidates.
    impl_items_by_receiver_def: RefCell<Option<Rc<HashMap<hir::DefId, Vec<usize>>>>>,
}

impl TypingShared {
    fn new(program: hir::Program, typing_context: Option<Rc<TypingContext>>) -> Rc<Self> {
        let executor = typing_context
            .as_ref()
            .map(|context| context.executor.clone())
            .unwrap_or_else(|| fp_core::executor::CompilerExecutor::new().handle());
        Rc::new(Self {
            program: Rc::new(program),
            results: RefCell::new(TypeckResults::default()),
            typing_context,
            executor,
            impl_lookup_items: RefCell::new(None),
            impl_items_by_receiver_def: RefCell::new(None),
        })
    }
}

/// Type checks resolved HIR and records semantic types outside the side
/// table (`TypingShared::results`, shared across every item's task). This
/// is deliberately a side-table pass: HIR nodes remain source-shaped and
/// MIR lowering can consume the results without an AST round trip.
///
/// One `HirTypeChecker` instance checks exactly one item — its fields below
/// are the item-local recursion state (locals in scope, generic
/// substitutions, the impl `Self` type, ...), fresh per item and never
/// shared with another item's concurrently in-flight check. Cross-item
/// state lives on `TypingShared` instead (`self.shared`).
pub struct HirTypeChecker {
    shared: Rc<TypingShared>,
    locals: Vec<HashMap<hir::Symbol, Ty>>,
    generic_scopes: Vec<HashMap<hir::DefId, Ty>>,
    self_types: Vec<Ty>,
    /// The current impl block's own associated-type bindings (`type
    /// Target = Y;`), pushed/popped in lockstep with `self_types` — lets
    /// `path_ty` resolve `Self::Target` for code lexically inside that
    /// impl. Deliberately scoped to the impl's own bindings only (no
    /// trait-default fallback, no cross-impl projection resolution for
    /// code outside the impl) — see `impl_assoc_types`.
    assoc_types: Vec<HashMap<hir::Symbol, Ty>>,
    expected_expr_types: Vec<Ty>,
    /// Type-position `const { ... }` blocks encountered while checking
    /// types (which is synchronous). Resolved via comptime once this
    /// item's own check finishes; see `resolve_pending_type_const_blocks`.
    pending_type_const_blocks: Vec<(hir::HirId, hir::Expr)>,
}

struct GenericScope<'a> {
    checker: &'a mut HirTypeChecker,
}

impl Deref for GenericScope<'_> {
    type Target = HirTypeChecker;

    fn deref(&self) -> &Self::Target {
        self.checker
    }
}

impl DerefMut for GenericScope<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.checker
    }
}

impl HirTypeChecker {
    /// Records a typing diagnostic instead of hard-aborting the whole
    /// package's typecheck over one error — mirrors `ast_to_hir`'s
    /// `error_placeholder_expr_kind` precedent (`fp-backend/src/
    /// transforms/ast_to_hir/exprs.rs`). `self.shared.typing_context` (already
    /// carried by every `HirTypeChecker`) is the real sink
    /// (`TypingContext.diagnostics`, previously never populated by
    /// anything); `typecheck_package` inspects it after the whole pass
    /// finishes to decide overall pass/fail, so this doesn't silently
    /// let a genuinely broken package look fully typed.
    fn record_error(&self, message: impl Into<String>) {
        if let Some(context) = &self.shared.typing_context {
            context
                .diagnostics
                .borrow_mut()
                .push(crate::types::TypingDiagnostic::error(message));
        }
    }

    /// `record_error` plus a `Ty::error()` placeholder, for `Result<Ty>`
    /// call sites that need *some* type to keep going with.
    fn error_ty(&self, message: impl Into<String>) -> Ty {
        self.record_error(message);
        Ty::error()
    }
}

impl Drop for GenericScope<'_> {
    fn drop(&mut self) {
        self.checker.generic_scopes.pop();
    }
}

/// Given a `def_id` that might name an `impl`'s method/assoc-const rather
/// than a top-level item, resolve it to the enclosing top-level item's own
/// `def_id` — `program.def_map` only has entries for top-level items (see
/// `expr_path_ty`'s pre-existing manual scan for the same reason), so an
/// impl member's `def_id` needs this extra step before it can be used as a
/// `spawn_item_task`/`typecheck_item` key. Returns `def_id` unchanged if
/// it's already a top-level item (the common case, checked first so this
/// stays O(1) except for actual impl members).
fn resolve_top_level_def_id(program: &hir::Program, def_id: hir::DefId) -> hir::DefId {
    if program.def_map.contains_key(&def_id) {
        return def_id;
    }
    for item in &program.items {
        if let hir::ItemKind::Impl(impl_item) = &item.kind {
            if impl_item.items.iter().any(|member| member.def_id == def_id) {
                return item.def_id;
            }
        }
    }
    def_id
}

/// Spawn one type-checking task per top-level item in `program`, sharing
/// results/caches across them via `TypingShared` — this is what makes
/// same-package forward references (a `const` or comptime block referring
/// to another item later in `program.items`) resolve regardless of textual
/// order: each item's task, on demand, awaits whatever other item it needs
/// via `get_or_spawn`, rather than assuming the linear walk already reached
/// it. Read the final result back out via `finish_package_typecheck` once
/// the returned future resolves.
pub fn spawn_package_typecheck(
    program: hir::Program,
    typing_context: Option<Rc<TypingContext>>,
) -> (Rc<TypingShared>, crate::BoxFuture<'static, Result<()>>) {
    let shared = TypingShared::new(program, typing_context);
    let items = shared.program.items.clone();
    let handles: Vec<_> = items
        .iter()
        .map(|item| spawn_item_task(&shared, item.def_id))
        .collect();
    let joined: crate::BoxFuture<'static, Result<()>> = Box::pin(async move {
        for handle in handles {
            handle.await;
        }
        Ok(())
    });
    (shared, joined)
}

/// Read the final `(hir::Program, TypeckResults)` out of `shared` — only
/// meaningful once every task `spawn_package_typecheck` spawned has
/// settled (i.e. its returned future resolved).
pub fn finish_package_typecheck(shared: &Rc<TypingShared>) -> (hir::Program, TypeckResults) {
    (
        shared.program.as_ref().clone(),
        shared.results.borrow().clone(),
    )
}

/// Get-or-spawn the task that type-checks `def_id` (resolved to its
/// enclosing top-level item first — see `resolve_top_level_def_id`),
/// keyed so any number of dependents (another item's task, or the initial
/// per-package spawn loop) share the same in-flight/completed attempt
/// instead of re-checking it.
fn spawn_item_task(shared: &Rc<TypingShared>, def_id: hir::DefId) -> TaskHandle<()> {
    let def_id = resolve_top_level_def_id(&shared.program, def_id);
    let key = format!("typecheck:{def_id:?}");
    let shared = shared.clone();
    shared
        .executor
        .clone()
        .get_or_spawn(key, move || {
            Box::pin(typecheck_item(shared, def_id)) as Pin<Box<dyn Future<Output = ()>>>
        })
}

/// Ensure `def_id` (a same-package item referenced by whatever item is
/// currently being checked) has been type-checked, on demand — awaiting
/// this shares the same underlying task as every other caller asking for
/// `def_id` (see `spawn_item_task`). A genuine cycle (this item
/// transitively depending on the item that's awaiting it) surfaces as a
/// stalled `ExecutorHandle` (`has_parked_tasks`), not as a hang or a wrong
/// answer — the driver's polling loop is what turns that into a
/// diagnostic; from this function's point of view it's just a suspend.
pub(crate) async fn ensure_item_checked(shared: &Rc<TypingShared>, def_id: hir::DefId) {
    spawn_item_task(shared, def_id).await
}

/// Type-check exactly one top-level item. Errors are recorded (via
/// `record_error`, against this item specifically) rather than propagated
/// — one item's failure never stops any other item's task from
/// completing, which is what "a package almost never fails as a whole"
/// means in practice.
pub async fn typecheck_item(shared: Rc<TypingShared>, def_id: hir::DefId) {
    let Some(item) = shared.program.def_map.get(&def_id).cloned() else {
        return;
    };
    let mut checker = HirTypeChecker {
        shared,
        locals: vec![HashMap::new()],
        generic_scopes: Vec::new(),
        self_types: Vec::new(),
        assoc_types: Vec::new(),
        expected_expr_types: Vec::new(),
        pending_type_const_blocks: Vec::new(),
    };
    if let Err(error) = checker.check_item(&item).await {
        checker.record_error(format!("{error}"));
        return;
    }
    if let Err(error) = checker.resolve_pending_type_const_blocks().await {
        checker.record_error(format!("{error}"));
    }
}

impl HirTypeChecker {
    /// Resolve `const { ... }` blocks encountered in type position
    /// (`check_type_expr` is synchronous, so it defers these rather than
    /// awaiting inline) — scoped to the single item just checked, unlike
    /// the old whole-program deferral this replaces.
    async fn resolve_pending_type_const_blocks(&mut self) -> Result<()> {
        let pending = std::mem::take(&mut self.pending_type_const_blocks);
        for (hir_id, body) in pending {
            let body_ty = self.check_expr(&body).await?;
            let Some(context) = self.shared.typing_context.clone() else {
                continue;
            };
            let value = context
                .request_comptime(crate::ComptimeRequest {
                    program: self.shared.program.as_ref().clone(),
                    typeck_results: self.shared.results.borrow().clone(),
                    block: hir::Block {
                        hir_id,
                        stmts: Vec::new(),
                        expr: Some(Box::new(body)),
                    },
                    expression_id: hir_id,
                    expected_ty: hir::TypeExpr {
                        hir_id,
                        kind: hir::TypeExprKind::Infer,
                        span: fp_core::span::Span::null(),
                    },
                })
                .await?;
            let mut results = self.shared.results.borrow_mut();
            results.const_block_values.insert(hir_id, value);
            // Replace the `Infer` placeholder `check_type_expr` recorded for
            // this node with the body's actual checked type, now that it's
            // known — matches expression-position const-blocks, whose own
            // type is likewise the checked type of their body.
            results.record_type_expr_type(hir_id, body_ty);
        }
        Ok(())
    }

    /// Type-checks and collects an impl block's own `type Target = Y;`
    /// associated-type bindings, ready to push onto `assoc_types` alongside
    /// `self_types` — see that field's doc comment for the deliberate scope
    /// limit (impl's own bindings only, no trait-default consultation, no
    /// cross-impl resolution).
    fn impl_assoc_types(
        &mut self,
        impl_items: &[hir::ImplItem],
    ) -> Result<HashMap<hir::Symbol, Ty>> {
        let mut out = HashMap::new();
        for item in impl_items {
            if let hir::ImplItemKind::AssocType(assoc) = &item.kind {
                let ty = self.check_type_expr(&assoc.ty)?;
                out.insert(assoc.name.clone(), ty);
            }
        }
        Ok(out)
    }

    fn check_item<'a>(&'a mut self, item: &'a hir::Item) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            match &item.kind {
                hir::ItemKind::Function(function) => {
                    self.check_function(function).await?;
                }
                hir::ItemKind::Const(constant) => {
                    let declared_ty = self.check_type_expr(&constant.ty)?;
                    self.expected_expr_types.push(declared_ty.clone());
                    let body_result = self.check_body(&constant.body).await;
                    self.expected_expr_types.pop();
                    let body_ty = body_result?;
                    self.shared.results.borrow_mut()
                        .type_expr_types
                        .insert(constant.ty.hir_id, body_ty.clone());
                    self.shared.results.borrow_mut().const_types.insert(item.def_id, body_ty);
                }
                hir::ItemKind::Impl(impl_item) => {
                    let mut scope = self.generic_scope(&impl_item.generics);
                    let self_ty = scope.check_type_expr(&impl_item.self_ty)?;
                    scope.self_types.push(self_ty);
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
                    let assoc_types = scope.impl_assoc_types(&impl_item.items)?;
                    scope.assoc_types.push(assoc_types);
                    for item in &impl_item.items {
                        match &item.kind {
                            hir::ImplItemKind::Method(function) => {
                                scope.check_function(function).await?
                            }
                            hir::ImplItemKind::AssocConst(constant) => {
                                scope.check_type_expr(&constant.ty)?;
                                scope.check_body(&constant.body).await?;
                            }
                            hir::ImplItemKind::AssocType(_) => {
                                // Already type-checked into `assoc_types` above.
                            }
                        }
                    }
                    scope.assoc_types.pop();
                    scope.self_types.pop();
                }
                hir::ItemKind::Struct(def) => {
                    let mut scope = self.generic_scope(&def.generics);
                    for field in &def.fields {
                        scope.check_type_expr(&field.ty)?;
                    }
                }
                hir::ItemKind::Enum(def) => {
                    let mut scope = self.generic_scope(&def.generics);
                    for variant in &def.variants {
                        if let Some(payload) = &variant.payload {
                            scope.check_type_expr(payload)?;
                        }
                        if let Some(discriminant) = &variant.discriminant {
                            scope.check_expr(discriminant).await?;
                        }
                    }
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
            self.push_generics(&function.sig.generics);
            self.check_signature(&function.sig).map_err(|error| {
                Error::from(format!(
                    "in function `{}` signature: {error}",
                    function.sig.name
                ))
            })?;
            if let Some(body) = &function.body {
                self.check_function_body(&function.sig.inputs, &function.sig.output, body)
                    .await
                    .map_err(|error| {
                        Error::from(format!("in function `{}` body: {error}", function.sig.name))
                    })?;
            }
            self.generic_scopes.pop();
            Ok(())
        })
    }

    fn push_generics(&mut self, generics: &hir::Generics) {
        let mut scope = HashMap::new();
        for (index, parameter) in generics.params.iter().enumerate() {
            if matches!(parameter.kind, hir::GenericParamKind::Type { .. }) {
                scope.insert(
                    parameter.def_id,
                    Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: index as u32,
                            name: parameter.name.clone(),
                        }),
                    },
                );
            }
        }
        self.generic_scopes.push(scope);
    }

    fn generic_scope(&mut self, generics: &hir::Generics) -> GenericScope<'_> {
        self.push_generics(generics);
        GenericScope { checker: self }
    }

    fn generic_ty(&self, def_id: hir::DefId) -> Option<Ty> {
        self.generic_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&def_id).cloned())
    }

    fn check_signature(&mut self, signature: &hir::FunctionSig) -> Result<()> {
        for input in &signature.inputs {
            self.check_type_expr(&input.ty)?;
        }
        self.check_type_expr(&signature.output)?;
        Ok(())
    }

    async fn check_body(&mut self, body: &hir::Body) -> Result<Ty> {
        self.locals.push(HashMap::new());
        for param in &body.params {
            let ty = self.check_type_expr(&param.ty)?;
            self.bind_pattern(&param.pat, ty)?;
        }
        let value_ty = self.check_expr(&body.value).await?;
        self.locals.pop();
        Ok(value_ty)
    }

    async fn check_function_body(
        &mut self,
        params: &[hir::Param],
        output: &hir::TypeExpr,
        block: &hir::Block,
    ) -> Result<()> {
        self.locals.push(HashMap::new());
        for param in params {
            let ty = self.check_type_expr(&param.ty)?;
            self.bind_pattern(&param.pat, ty)?;
        }
        // Same expected-type hint `ConstBlock`/`Assign` already provide:
        // without it, a trailing zero-arg generic call like
        // `Option::none()` (no argument to infer `T` from) can't resolve
        // its type parameter even though the function's own declared
        // return type unambiguously determines it. Scoped to just the
        // block's own trailing expression (see
        // `check_block_with_expected_tail`) — not the whole body — so it
        // doesn't leak into unrelated statements earlier in the function.
        let output_ty = self.check_type_expr(output)?;
        self.check_block_with_expected_tail(block, Some(output_ty))
            .await?;
        self.locals.pop();
        Ok(())
    }

    fn check_expr<'a>(&'a mut self, expr: &'a hir::Expr) -> crate::BoxFuture<'a, Result<Ty>> {
        Box::pin(async move {
            let ty = match &expr.kind {
                hir::ExprKind::Literal(lit) => self.literal_ty(lit),
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
                                self.require_same(&lhs, &Ty::bool())?;
                                self.require_same(&rhs, &Ty::bool())?;
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
                        _ => lhs,
                    }
                }
                hir::ExprKind::Unary(op, value) => {
                    let value_ty = self.check_expr(value).await?;
                    match op {
                        hir::UnOp::Not => {
                            self.require_same(&value_ty, &Ty::bool())?;
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
                        kind: TyKind::Ref(ty::Region::ReErased, Box::new(referent), reference.mutable),
                    }
                }
                hir::ExprKind::Call(callee, args) => {
                    let callee_ty = self.check_expr(callee).await?;
                    let expected_inputs = match &callee_ty.kind {
                        TyKind::FnPtr(signature) => Some(signature.binder.value.inputs.clone()),
                        _ => None,
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
                        if let Some(hint) = &param_hint {
                            self.expected_expr_types.push(hint.clone());
                        }
                        let actual = self.check_expr(&arg.value).await;
                        if param_hint.is_some() {
                            self.expected_expr_types.pop();
                        }
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
                        arg_types.push(actual);
                    }
                    let Some((mut substitutions, _)) =
                        self.instantiate_call(&callee_ty, &arg_types)?
                    else {
                        return Ok(self.error_ty("called expression is not a function"));
                    };
                    if substitutions.is_empty() {
                        if let Some(expected) = self.expected_expr_types.last() {
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
                                .generic_call_args(*def_id, &substitutions)?
                                .or_else(|| self.callable_output_args(&callee_ty, &substitutions));
                            if let Some(args) = args {
                                self.shared.results.borrow_mut().generic_call_args.insert(
                                    expr.hir_id,
                                    GenericCallResolution {
                                        def_id: *def_id,
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
                    let declared_inputs = match self.method_declared_signature(&receiver_ty, method)
                    {
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
                        if let Some(hint) = &param_hint {
                            self.expected_expr_types.push(hint.clone());
                        }
                        let actual = self.check_expr(&arg.value).await;
                        if param_hint.is_some() {
                            self.expected_expr_types.pop();
                        }
                        arg_types.push(actual?);
                    }
                    // Method resolution has no natural "error" `DefId` to
                    // substitute (unlike `Ty::error()`), so the whole
                    // `Result` from `method_output` (and anything it calls
                    // via `?` internally, like `method_generic_args`) is
                    // caught right here instead of inside those functions —
                    // one catch point covers all of them.
                    match self.method_output(&receiver_ty, method, &arg_types) {
                        Ok((method_def_id, generic_args, output)) => {
                            self.shared.results.borrow_mut()
                                .method_resolutions
                                .insert(expr.hir_id, method_def_id);
                            if let Some(args) = generic_args {
                                self.shared.results.borrow_mut().generic_method_args.insert(
                                    expr.hir_id,
                                    GenericCallResolution {
                                        def_id: method_def_id,
                                        args,
                                    },
                                );
                            }
                            output
                        }
                        Err(error) => self.error_ty(error.to_string()),
                    }
                }
                hir::ExprKind::FieldAccess(receiver, field) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    self.field_ty(&receiver_ty, field)?
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
                            self.require_same(&index_ty, &Ty::int(ty::IntTy::I64))?;
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
                            if Some(adt.did) == self.well_known_struct_def_id("HashMap") =>
                        {
                            let (Some(GenericArg::Type(key_ty)), Some(GenericArg::Type(value_ty))) =
                                (args.first(), args.get(1))
                            else {
                                return Ok(self.error_ty(
                                    "HashMap index requires key and value type arguments",
                                ));
                            };
                            self.require_same(&index_ty, key_ty)?;
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
                            match self.method_output(
                                receiver_ty,
                                &hir::Symbol::from("index"),
                                &arg_types,
                            ) {
                                Ok((method_def_id, generic_args, output)) => {
                                    self.shared.results.borrow_mut()
                                        .method_resolutions
                                        .insert(expr.hir_id, method_def_id);
                                    if let Some(args) = generic_args {
                                        self.shared.results.borrow_mut().generic_method_args.insert(
                                            expr.hir_id,
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
                    self.check_type_expr(target)?
                }
                hir::ExprKind::Struct(path, fields) => {
                    let ty = match self.enum_variant_ty(path)? {
                        Some(ty) => ty,
                        None => self.path_ty(path)?,
                    };
                    let payload_ty = self.enum_struct_payload_type(path, &ty)?;
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
                            self.field_ty(payload, &field.name)?
                        } else {
                            self.field_ty(&ty, &field.name)?
                        };
                        // Scope the expected-type hint to *this field's*
                        // declared type, not whatever hint the enclosing
                        // struct literal itself was checked under — e.g.
                        // `BinaryHeap { values: Vec::new(), .. }` inside a
                        // function returning `BinaryHeap<T>` must not leak
                        // that outer `BinaryHeap<T>` hint into `values`'
                        // own zero-arg `Vec::new()` call, which needs (and
                        // has) its own field type, `Vec<T>`, to infer from.
                        self.expected_expr_types.push(field_ty.clone());
                        let value_ty = self.check_expr(&field.expr).await;
                        self.expected_expr_types.pop();
                        let value_ty = value_ty?;
                        self.unify_call_types(&field_ty, &value_ty, &mut substitutions)?;
                    }
                    self.substitute_param_map(&ty, &substitutions)
                }
                hir::ExprKind::If(condition, then_expr, else_expr) => {
                    let condition = self.check_expr(condition).await?;
                    self.require_same(&condition, &Ty::bool())?;
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
                    let declared_ty = self.check_type_expr(&const_block.ty)?;
                    self.expected_expr_types.push(declared_ty.clone());
                    let body_result = self.check_expr(&const_block.body).await;
                    self.expected_expr_types.pop();
                    let body_ty = body_result?;
                    if let Some(context) = self.shared.typing_context.clone() {
                        let value = context
                            .request_comptime(crate::ComptimeRequest {
                                program: self.shared.program.as_ref().clone(),
                                typeck_results: self.shared.results.borrow().clone(),
                                block: hir::Block {
                                    hir_id: expr.hir_id,
                                    stmts: Vec::new(),
                                    expr: Some(const_block.body.clone()),
                                },
                                expression_id: expr.hir_id,
                                expected_ty: (*const_block.ty).clone(),
                            })
                            .await?;
                        self.shared.results.borrow_mut().const_block_values.insert(expr.hir_id, value);
                    }
                    body_ty
                }
                hir::ExprKind::While(condition, block) => {
                    let condition_ty = self.check_expr(condition).await?;
                    self.require_same(&condition_ty, &Ty::bool())?;
                    self.check_block(block).await?
                }
                hir::ExprKind::Array(values) => {
                    if values.is_empty() {
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
                            self.require_same(&element, &value_ty)?;
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
                        _ => ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id)),
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
                    self.expected_expr_types.push(lhs.clone());
                    let rhs = self.check_expr(rhs).await;
                    self.expected_expr_types.pop();
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
                    let ty = self.check_type_expr(target)?;
                    if let Some(value) = value {
                        let value_ty = self.check_expr(value).await?;
                        self.require_same(&ty, &value_ty)?;
                    }
                    self.bind_pattern(pattern, ty.clone())?;
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
                            )?;
                        }
                        let catch_ty = self.check_expr(&catch.body).await?;
                        self.require_same(&result_ty, &catch_ty)?;
                    }
                    if let Some(elze) = &value.elze {
                        let elze_ty = self.check_expr(elze).await?;
                        self.require_same(&result_ty, &elze_ty)?;
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
                    let hint = self.expected_expr_types.last().cloned();
                    let hint_sig = match &hint {
                        Some(Ty {
                            kind: TyKind::FnPtr(sig),
                        }) => Some(sig.binder.value.clone()),
                        _ => None,
                    };
                    self.locals.push(HashMap::new());
                    let mut param_types = Vec::with_capacity(closure.params.len());
                    for (index, param) in closure.params.iter().enumerate() {
                        let declared = if matches!(param.ty.kind, hir::TypeExprKind::Infer) {
                            None
                        } else {
                            Some(self.check_type_expr(&param.ty)?)
                        };
                        let param_ty = declared
                            .or_else(|| {
                                hint_sig
                                    .as_ref()
                                    .and_then(|sig| sig.inputs.get(index))
                                    .map(|ty| (**ty).clone())
                            })
                            .unwrap_or_else(|| Ty {
                                kind: TyKind::Infer(ty::InferTy::FreshTy(param.hir_id)),
                            });
                        self.bind_pattern(&param.pat, param_ty.clone())?;
                        param_types.push(param_ty);
                    }
                    let body_ty = self.check_expr(&closure.body).await?;
                    self.locals.pop();
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
            self.shared.results.borrow_mut().record_expr_type(expr.hir_id, ty.clone());
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
        self.locals.push(HashMap::new());
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    let ty = match (&local.ty, &local.init) {
                        (Some(annotation), Some(init)) => {
                            let ty = self.check_type_expr(annotation)?;
                            let init_ty = self.check_expr(init).await?;
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
                                ty.clone()
                            } else {
                                let mut substitutions = HashMap::new();
                                self.unify_call_types(&init_ty, &ty, &mut substitutions)?;
                                self.substitute_param_map(&init_ty, &substitutions)
                            };
                            self.require_same(&ty, &resolved_init)?;
                            self.shared.results.borrow_mut().record_expr_type(init.hir_id, resolved_init);
                            ty
                        }
                        (Some(annotation), None) => self.check_type_expr(annotation)?,
                        (None, Some(init)) => self.check_expr(init).await?,
                        (None, None) => {
                            self.error_ty("local binding needs a type or initializer")
                        }
                    };
                    self.bind_pattern(&local.pat, ty)?;
                }
                hir::StmtKind::Item(item) => self.check_item(item).await?,
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    self.check_expr(expr).await?;
                }
            }
        }
        let ty = match block.expr.as_ref() {
            Some(expr) => {
                if let Some(expected) = expected_tail {
                    self.expected_expr_types.push(expected);
                    let result = self.check_expr(expr).await;
                    self.expected_expr_types.pop();
                    result?
                } else {
                    self.check_expr(expr).await?
                }
            }
            None => self.unit_ty(),
        };
        self.locals.pop();
        Ok(ty)
    }

    async fn check_match_arm(&mut self, arm: &hir::MatchArm, scrutinee_ty: &Ty) -> Result<Ty> {
        self.locals.push(HashMap::new());
        self.bind_pattern(&arm.pat, scrutinee_ty.clone())?;
        if let Some(guard) = &arm.guard {
            let guard_ty = self.check_expr(guard).await?;
            self.require_same(&guard_ty, &Ty::bool())?;
        }
        let result = self.check_expr(&arm.body).await;
        self.locals.pop();
        result
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

    fn check_type_expr(&mut self, expr: &hir::TypeExpr) -> Result<Ty> {
        let ty = match &expr.kind {
            hir::TypeExprKind::Primitive(primitive) => primitive_ty(*primitive),
            hir::TypeExprKind::Path(path) => self.path_ty(path)?,
            hir::TypeExprKind::Tuple(items) => Ty {
                kind: TyKind::Tuple(
                    items
                        .iter()
                        .map(|item| self.check_type_expr(item).map(Box::new))
                        .collect::<Result<_>>()?,
                ),
            },
            hir::TypeExprKind::Slice(item) => Ty {
                kind: TyKind::Slice(Box::new(self.check_type_expr(item)?)),
            },
            hir::TypeExprKind::Ptr(item) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.check_type_expr(item)?),
                    mutbl: ty::Mutability::Not,
                }),
            },
            hir::TypeExprKind::Ref(item) => Ty {
                kind: TyKind::Ref(
                    ty::Region::ReErased,
                    Box::new(self.check_type_expr(item)?),
                    ty::Mutability::Not,
                ),
            },
            hir::TypeExprKind::FnPtr(function) => Ty {
                kind: TyKind::FnPtr(ty::PolyFnSig {
                    binder: ty::Binder {
                        value: ty::FnSig {
                            inputs: function
                                .inputs
                                .iter()
                                .map(|input| self.check_type_expr(input).map(Box::new))
                                .collect::<Result<_>>()?,
                            output: Box::new(self.check_type_expr(&function.output)?),
                            c_variadic: false,
                            unsafety: ty::Unsafety::Normal,
                            abi: ty::Abi::Rust,
                        },
                        bound_vars: Vec::new(),
                    },
                }),
            },
            hir::TypeExprKind::Never => Ty::never(),
            hir::TypeExprKind::Array(item, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.check_type_expr(item)?),
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
                        _ => ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id)),
                    },
                ),
            },
            hir::TypeExprKind::Infer => Ty {
                kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id)),
            },
            hir::TypeExprKind::ConstBlock(body) => {
                self.pending_type_const_blocks
                    .push((expr.hir_id, (**body).clone()));
                Ty {
                    kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id)),
                }
            }
            hir::TypeExprKind::Error => self.error_ty("invalid type expression"),
            hir::TypeExprKind::Structural(_) => {
                self.error_ty("structural types are not supported by HIR typing")
            }
            hir::TypeExprKind::TypeBinaryOp(_) => {
                self.error_ty("type expressions cannot be combined with a type operator")
            }
        };
        self.shared.results.borrow_mut().record_type_expr_type(expr.hir_id, ty.clone());
        Ok(ty)
    }

    fn path_ty(&mut self, path: &hir::Path) -> Result<Ty> {
        if let Some(name) = path.segments.last().map(|segment| segment.name.as_str()) {
            if let Some(primitive) = primitive_path_ty(name) {
                return Ok(primitive);
            }
        }
        if let Some(hir::Res::Local(local)) = path.res {
            return Ok(self.error_ty(format!("local `{local}` is not a type")));
        }
        if matches!(path.res, Some(hir::Res::SelfTy)) {
            if self.self_types.is_empty() {
                return Ok(self.error_ty("Self is not available in this type context"));
            }
            // `Self::Target` (an associated-type path rooted at `Self`,
            // e.g. inside `impl Deref for X { fn deref(&self) -> &Self::
            // Target { .. } }`) — resolved from the enclosing impl's own
            // `type Target = Y;` binding (`assoc_types`, pushed alongside
            // `self_types`). Deliberately doesn't consult a trait default
            // or resolve `Self::X` for code outside the impl — see
            // `impl_assoc_types`'s doc comment.
            if let Some(assoc_segment) = path.segments.get(1) {
                let scope = self.assoc_types.last();
                if let Some(ty) = scope.and_then(|scope| scope.get(&assoc_segment.name)) {
                    return Ok(ty.clone());
                }
                return Ok(self.error_ty(format!(
                    "associated type `Self::{}` is not defined in this impl",
                    assoc_segment.name
                )));
            }
            return Ok(self.self_types.last().cloned().unwrap());
        }
        let Some(def_id) = (match path.res {
            Some(hir::Res::Def(def_id)) => Some(def_id),
            _ => None,
        }) else {
            // Treat `void` as unit type (C compatibility)
            if path.segments.len() == 1 && path.segments[0].name.as_str() == "void" {
                return Ok(hir::Ty { kind: hir::ty::TyKind::Tuple(vec![]) });
            }
            // Fallback: treat single-segment unresolved types as unit type
            // (handles C FFI types like fenv_t, etc.)
            if path.segments.len() == 1 && path.res.is_none() {
                return Ok(hir::Ty { kind: hir::ty::TyKind::Tuple(vec![]) });
            }
            return Ok(self.error_ty(format!(
                "unresolved type path `{}`",
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::")
            )));
        };
        if let Some(generic) = self.generic_ty(def_id) {
            return Ok(generic);
        }
        let Some(item) = self.shared.program.def_map.get(&def_id).cloned() else {
            return Ok(self.error_ty(format!("type definition `{def_id}` was not found")));
        };
        let (flags, variants) = match &item.kind {
            hir::ItemKind::Struct(_) => (AdtFlags::IS_STRUCT, Vec::new()),
            hir::ItemKind::Enum(def) => (
                AdtFlags::IS_ENUM,
                def.variants
                    .iter()
                    .enumerate()
                    .map(|(index, variant)| ty::VariantDef {
                        def_id: variant.def_id,
                        ctor_def_id: Some(variant.def_id),
                        ident: variant.name.clone(),
                        discr: ty::VariantDiscr::Relative(index as u32),
                        fields: Vec::new(),
                        ctor_kind: ty::CtorKind::Fn,
                        is_recovered: false,
                    })
                    .collect(),
            ),
            _ => return Ok(self.error_ty(format!("definition `{def_id}` is not a type"))),
        };
        let args = match path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
        {
            Some(args) => args
                .args
                .iter()
                .map(|arg| match arg {
                    hir::GenericArg::Type(ty) => self.check_type_expr(ty).map(GenericArg::Type),
                    hir::GenericArg::Const(_) => Ok(GenericArg::Type(
                        self.error_ty("const generic arguments are not supported"),
                    )),
                })
                .collect::<Result<Vec<_>>>()?,
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
                    did: def_id,
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
            Some("Vec") | Some("List") => {
                let t = Ty {
                    kind: TyKind::Param(ty::ParamTy {
                        index: 0,
                        name: hir::Symbol::new("T"),
                    }),
                };
                let output = match self.well_known_struct_ty("Vec", vec![GenericArg::Type(t.clone())]) {
                    Some(ty) => ty,
                    None => return Some(Ok(self.error_ty("`Vec` struct definition was not found"))),
                };
                let input = Ty {
                    kind: TyKind::Slice(Box::new(t)),
                };
                Some(Ok(make_sig(vec![input], output)))
            }
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
                        return Some(Ok(self.error_ty("`HashMapEntry` struct definition was not found")))
                    }
                };
                let output = match self.well_known_struct_ty("HashMap", vec![GenericArg::Type(k), GenericArg::Type(v)]) {
                    Some(ty) => ty,
                    None => {
                        return Some(Ok(self.error_ty("`HashMap` struct definition was not found")))
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

    /// Finds a real struct definition by name, searching this package first
    /// and then loaded dependency packages — used only for well-known
    /// standard-library collection types that a synthesized function
    /// signature (see `collection_constructor_signature`) needs to name as
    /// its output type, since normal path resolution never runs for them.
    fn well_known_struct_def_id(&self, name: &str) -> Option<hir::DefId> {
        let find_in = |items: &[hir::Item]| {
            items.iter().find_map(|item| match &item.kind {
                hir::ItemKind::Struct(def) if def.name.as_str() == name => Some(item.def_id),
                _ => None,
            })
        };
        if let Some(def_id) = find_in(&self.shared.program.items) {
            return Some(def_id);
        }
        if let Some(context) = &self.shared.typing_context {
            // Borrows each dependency package just long enough to scan its
            // HIR items in place, instead of `hir_definitions()`'s full
            // clone of every package's whole HIR `Program`.
            return context.env_ctx.find_hir_struct_def_id(name);
        }
        None
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
        if let Some(hir::Res::Local(local)) = path.res {
            if let Some(name) = path.segments.last().map(|segment| &segment.name) {
                for scope in self.locals.iter().rev() {
                    if let Some(ty) = scope.get(name) {
                        return Ok(ty.clone());
                    }
                }
            }
            // HIR locals are resolved by the lowering resolver. Their
            // bindings may be outside this pass's lexical reconstruction
            // (for example generated closure parameters), so preserve the
            // resolved value path and let MIR handle its value semantics.
            let _ = local;
            return Ok(self.error_ty(format!("local `{local}` has no inferred type")));
        }
        let Some(hir::Res::Def(def_id)) = path.res else {
            if let Some(sig) = self.collection_constructor_signature(path) {
                return sig;
            }
            // A value path can refer to a definition supplied by a loaded
            // crate. It is resolved by HIR lowering, while this pass only
            // needs a semantic value type for subsequent expression checks.
            return Ok(self.error_ty(format!(
                "unresolved value path `{}`",
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::")
            )));
        };
        let Some(item) = self.shared.program.def_map.get(&def_id).cloned() else {
            let associated = self.shared.program.items.iter().find_map(|item| {
                let hir::ItemKind::Impl(impl_item) = &item.kind else {
                    return None;
                };
                impl_item.items.iter().find_map(|impl_member| {
                    if impl_member.def_id != def_id {
                        return None;
                    }
                    let hir::ImplItemKind::Method(function) = &impl_member.kind else {
                        return None;
                    };
                    Some((
                        impl_item.generics.clone(),
                        impl_item.self_ty.clone(),
                        impl_item.items.clone(),
                        function.clone(),
                    ))
                })
            });
            if let Some((generics, self_ty, impl_items, function)) = associated {
                let mut scope = self.generic_scope(&generics);
                let self_ty = scope.check_type_expr(&self_ty)?;
                scope.self_types.push(self_ty);
                let assoc_types = scope.impl_assoc_types(&impl_items)?;
                scope.assoc_types.push(assoc_types);
                let result = scope.function_signature(&function);
                scope.assoc_types.pop();
                scope.self_types.pop();
                return result;
            }
            if let Some(context) = &self.shared.typing_context {
                // Borrows each dependency package just long enough to scan
                // its HIR items in place, instead of `hir_definitions()`'s
                // full clone of every package's whole HIR `Program`.
                if let Some((generics, self_ty, impl_items, function)) =
                    context.env_ctx.find_hir_impl_method(def_id)
                {
                    let mut scope = self.generic_scope(&generics);
                    let self_ty = scope.check_type_expr(&self_ty)?;
                    scope.self_types.push(self_ty);
                    let assoc_types = scope.impl_assoc_types(&impl_items)?;
                    scope.assoc_types.push(assoc_types);
                    let result = scope.function_signature(&function);
                    scope.assoc_types.pop();
                    scope.self_types.pop();
                    return result;
                }
            }
            let matched_enum_item = self.shared.program.items.iter().find_map(|item| {
                let hir::ItemKind::Enum(enum_def) = &item.kind else {
                    return None;
                };
                enum_def
                    .variants
                    .iter()
                    .any(|variant| variant.def_id == def_id)
                    .then(|| item.clone())
            });
            if let Some(enum_item) = matched_enum_item {
                let hir::ItemKind::Enum(enum_def) = &enum_item.kind else {
                    unreachable!("matched_enum_item only holds ItemKind::Enum items")
                };
                let variant = enum_def
                    .variants
                    .iter()
                    .find(|variant| variant.def_id == def_id)
                    .expect("matched_enum_item's enum_def contains this variant");
                let enum_ty = self.enum_item_ty(&enum_item, path)?;
                if let Some(payload) = &variant.payload {
                    let mut scope = self.generic_scope(&enum_def.generics);
                    let payload_result = scope.check_type_expr(payload);
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
            return Ok(self.error_ty(format!("value definition `{def_id}` was not found")));
        };
        match &item.kind {
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => self.path_ty(path),
            hir::ItemKind::Const(constant)
                if matches!(
                    constant.body.value.kind,
                    hir::ExprKind::Literal(hir::Lit::Integer(_))
                ) =>
            {
                self.check_type_expr(&constant.ty)
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
                if self
                    .shared
                    .results
                    .borrow()
                    .const_types
                    .get(&def_id)
                    .is_none()
                {
                    ensure_item_checked(&self.shared, def_id).await;
                }
                Ok(self
                    .shared
                    .results
                    .borrow()
                    .const_types
                    .get(&def_id)
                    .cloned()
                    .unwrap_or_else(|| self.error_ty("constant type was not recorded")))
            }
            hir::ItemKind::Function(function) => self.function_signature(function),
            _ => Ok(self.error_ty("resolved path is not a value")),
        }
    }

    fn enum_item_ty(&mut self, item: &hir::Item, path: &hir::Path) -> Result<Ty> {
        let hir::ItemKind::Enum(enum_def) = &item.kind else {
            return Ok(self.error_ty("enum path does not resolve to an enum"));
        };
        let variants = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| ty::VariantDef {
                def_id: variant.def_id,
                ctor_def_id: Some(variant.def_id),
                ident: variant.name.clone(),
                discr: ty::VariantDiscr::Relative(index as u32),
                fields: Vec::new(),
                ctor_kind: ty::CtorKind::Fn,
                is_recovered: false,
            })
            .collect();
        let args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| {
                args.args
                    .iter()
                    .map(|arg| match arg {
                        hir::GenericArg::Type(ty) => self.check_type_expr(ty).map(GenericArg::Type),
                        hir::GenericArg::Const(_) => Ok(GenericArg::Type(
                            self.error_ty("const generic arguments are not supported"),
                        )),
                    })
                    .collect::<Result<Vec<_>>>()
            })
            .transpose()?;
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
                    did: item.def_id,
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

    fn function_signature(&mut self, function: &hir::Function) -> Result<Ty> {
        let mut scope = self.generic_scope(&function.sig.generics);
        let inputs = function
            .sig
            .inputs
            .iter()
            .map(|input| scope.check_type_expr(&input.ty).map(Box::new))
            .collect::<Result<Vec<_>>>()?;
        let output = Box::new(scope.check_type_expr(&function.sig.output)?);
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

    fn instantiate_call(
        &self,
        callable: &Ty,
        actuals: &[Ty],
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return Ok(None);
        };
        if signature.binder.value.inputs.len() != actuals.len() {
            // `None` already means "not callable with these args" to every
            // caller (see the `TyKind::FnPtr` mismatch case just above) —
            // an arity mismatch is the same kind of "doesn't match", not a
            // hard error, so report it the same way instead of aborting.
            self.record_error("call argument count does not match function signature");
            return Ok(None);
        }
        let mut substitutions: HashMap<ty::ParamTy, Ty> = HashMap::new();
        for (expected, actual) in signature.binder.value.inputs.iter().zip(actuals) {
            self.unify_call_types(expected, actual, &mut substitutions)?;
        }
        let output = self.substitute_param_map(&signature.binder.value.output, &substitutions);
        Ok(Some((substitutions, output)))
    }

    fn generic_call_args(
        &self,
        def_id: hir::DefId,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Result<Option<Vec<Ty>>> {
        let Some(item) = self.shared.program.def_map.get(&def_id) else {
            return Ok(None);
        };
        let hir::ItemKind::Function(function) = &item.kind else {
            return Ok(None);
        };
        if function.sig.generics.params.is_empty() {
            return Ok(None);
        }
        let mut args = Vec::with_capacity(function.sig.generics.params.len());
        for (index, parameter) in function.sig.generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                self.record_error(format!(
                    "could not infer generic parameter `{}` for `{def_id}`",
                    parameter.name
                ));
                return Ok(None);
            };
            args.push(argument.clone());
        }
        Ok(Some(args))
    }

    fn unify_call_types(
        &self,
        expected: &Ty,
        actual: &Ty,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
    ) -> Result<()> {
        match (&expected.kind, &actual.kind) {
            (TyKind::Param(param), _) => {
                if let Some(previous) = substitutions.get(param) {
                    self.require_same(previous, actual)?;
                } else {
                    substitutions.insert(param.clone(), actual.clone());
                }
                Ok(())
            }
            (_, TyKind::Param(param)) => {
                if let Some(previous) = substitutions.get(param) {
                    self.require_same(previous, expected)?;
                } else {
                    substitutions.insert(param.clone(), expected.clone());
                }
                Ok(())
            }
            (TyKind::Ref(_, expected, _), TyKind::Ref(_, actual, _)) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::Ref(_, expected, _), _) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            // Symmetric to the rule above: a bare-expected/`Ref`-actual pair
            // (e.g. a `str`-returning call's result reconciled against a
            // `&str` expected-type hint) derefs the actual side the same
            // way. Safe as a general rule — if the underlying shapes still
            // don't match after peeling, the recursive call's own catch-all
            // still reports a genuine mismatch.
            (_, TyKind::Ref(_, actual, _)) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::FnPtr(expected), TyKind::FnPtr(actual))
                if expected.binder.value.inputs.len() == actual.binder.value.inputs.len() =>
            {
                for (expected, actual) in expected
                    .binder
                    .value
                    .inputs
                    .iter()
                    .zip(&actual.binder.value.inputs)
                {
                    self.unify_call_types(expected, actual, substitutions)?;
                }
                self.unify_call_types(
                    &expected.binder.value.output,
                    &actual.binder.value.output,
                    substitutions,
                )
            }
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => {
                expected
                    .iter()
                    .zip(actual)
                    .try_for_each(|(expected, actual)| {
                        self.unify_call_types(expected, actual, substitutions)
                    })
            }
            (TyKind::Array(expected, _), TyKind::Array(actual, _))
            | (TyKind::Slice(expected), TyKind::Slice(actual))
            | (TyKind::Array(expected, _), TyKind::Slice(actual))
            | (TyKind::Slice(expected), TyKind::Array(actual, _)) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    if let (GenericArg::Type(expected), GenericArg::Type(actual)) =
                        (expected, actual)
                    {
                        self.unify_call_types(expected, actual, substitutions)?;
                    }
                }
                Ok(())
            }
            // C-string/FFI decay: a `&str`/string-literal argument may be
            // passed where a raw pointer (`*const char`/`*mut char`, an
            // `extern "C"` parameter) is expected — the same implicit
            // decay C itself performs for string literals and `&str`'s
            // byte-slice representation already matches a C string's byte
            // layout at the FFI boundary.
            (TyKind::RawPtr(_), TyKind::Slice(_)) => Ok(()),
            (TyKind::RawPtr(_), TyKind::Ref(_, inner, _))
                if matches!(inner.kind, TyKind::Slice(_)) =>
            {
                Ok(())
            }
            // `void*`/any-object-pointer decay, same as C: a raw pointer of
            // one pointee type may be passed where a raw pointer of another
            // is expected (e.g. `*mut u8` into `memcpy`'s `*mut void`
            // parameter) — this compiler has no real `void`/opaque-pointer
            // distinction, just an ordinary `RawPtr(())`.
            (TyKind::RawPtr(_), TyKind::RawPtr(_)) => Ok(()),
            _ => self.require_same(expected, actual),
        }
    }

    /// Whether an impl block's generic arguments (e.g. `Vec`'s `[&str]` in
    /// `impl Vec<&str> { .. }`) are compatible with a call site's actual
    /// receiver arguments (e.g. `Vec`'s `[str]` for a `Vec<str>` value) —
    /// used by `method_output` to pick the right specialization instead of
    /// letting any `impl SomeGeneric<T> { .. }` match every instantiation.
    /// A still-generic impl argument (containing an uninstantiated
    /// `Param`, e.g. `impl<T> Vec<T>`) always matches, since it has nothing
    /// concrete to conflict with; a concrete one must coerce the same way
    /// a call argument would (the same `Ref`-peeling rules as everywhere
    /// else), not require exact structural equality.
    fn generic_args_compatible(&self, impl_args: &[GenericArg], receiver_args: &[GenericArg]) -> bool {
        if impl_args.len() != receiver_args.len() {
            return false;
        }
        impl_args.iter().zip(receiver_args).all(|(impl_arg, receiver_arg)| {
            match (impl_arg, receiver_arg) {
                (GenericArg::Type(impl_ty), GenericArg::Type(receiver_ty)) => {
                    if ty_contains_param(impl_ty) {
                        return true;
                    }
                    let mut substitutions = HashMap::new();
                    self.unify_call_types(impl_ty, receiver_ty, &mut substitutions)
                        .is_ok()
                }
                _ => impl_arg == receiver_arg,
            }
        })
    }

    /// Unify two branches of the same control-flow join (`if`/`else`,
    /// `match` arms) that are expected to produce the same type. Unlike
    /// `require_same`, this tolerates one side still carrying an
    /// uninstantiated `TyKind::Param` (e.g. `Option::None`'s type, which has
    /// no argument of its own to infer `T` from) against the other side's
    /// fully concrete type (e.g. `Option::Some(x)`'s `Option<i64>`) —
    /// exactly the same substitution `unify_call_types` already performs for
    /// call arguments, just tried in both directions since either branch
    /// could be the concrete one.
    fn unify_branch_types(&self, a: &Ty, b: &Ty) -> Result<Ty> {
        if a == b {
            return Ok(a.clone());
        }
        let mut substitutions = HashMap::new();
        if self.unify_call_types(a, b, &mut substitutions).is_ok() {
            return Ok(self.substitute_param_map(a, &substitutions));
        }
        let mut substitutions = HashMap::new();
        if self.unify_call_types(b, a, &mut substitutions).is_ok() {
            return Ok(self.substitute_param_map(b, &substitutions));
        }
        self.require_same(a, b)?;
        Ok(a.clone())
    }

    fn substitute_param_map(&self, ty: &Ty, substitutions: &HashMap<ty::ParamTy, Ty>) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => match substitutions.get(param) {
                Some(ty) => ty.clone(),
                None => ty.clone(),
            },
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region.clone(),
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    *mutable,
                ),
            },
            TyKind::RawPtr(value) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_param_map(&value.ty, substitutions)),
                    mutbl: value.mutbl,
                }),
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .iter()
                        .map(|field| Box::new(self.substitute_param_map(field, substitutions)))
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    length.clone(),
                ),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_param_map(inner, substitutions))),
            },
            TyKind::Adt(def, args) => Ty {
                kind: TyKind::Adt(
                    def.clone(),
                    args.iter()
                        .map(|arg| match arg {
                            GenericArg::Type(ty) => {
                                GenericArg::Type(self.substitute_param_map(ty, substitutions))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                ),
            },
            _ => ty.clone(),
        }
    }

    /// Finds the impl method `method_output` would eventually resolve for
    /// `receiver_ty`, returning just its *declared* signature (`Self`
    /// substituted from the already-known receiver type; the method's own
    /// generics, e.g. `map_or`'s `U`, left as unresolved `Param`s) —
    /// without needing any argument types. `Self`'s position always
    /// substitutes cleanly from `receiver_ty` alone, entirely independent
    /// of the call's other arguments, so this doesn't need to wait for
    /// them the way `instantiate_call`'s full unification does.
    ///
    /// This exists so `MethodCall` can seed a real expected-type hint for
    /// each argument *before* checking it (mirroring `Call`'s existing
    /// per-parameter hint) — the load-bearing case being a closure
    /// argument to a generic method like `Option::map_or(self, default: U,
    /// f: fn(T) -> U) -> U`: without this, `T` is unknown when the closure
    /// literal is checked, so its parameter silently gets an unusable
    /// placeholder type. Mirrors the matching loop in `method_output`
    /// (kept in sync deliberately, not factored into one shared loop,
    /// since the two stop at different points and diverging their control
    /// flow — `Result` vs best-effort `None` — reads more clearly split).
    fn method_declared_signature(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
    ) -> Result<Option<Ty>> {
        let receiver_ty = match &receiver_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver_ty,
        };
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did),
            _ => None,
        };
        let impl_items = self.impl_lookup_items();
        // See `method_output`'s matching copy of this fast-reject index
        // lookup for why this is safe: an ADT receiver can only ever match
        // an impl whose self-type also resolves to `TyKind::Adt` with the
        // same `did`.
        let candidate_indices: Vec<usize> = match receiver_def {
            Some(def_id) => self
                .impl_items_by_receiver_def()
                .get(&def_id)
                .cloned()
                .unwrap_or_default(),
            None => (0..impl_items.len()).collect(),
        };
        for &item_index in &candidate_indices {
            let item = &impl_items[item_index];
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            let mut scope = self.generic_scope(&impl_item.generics);
            let checked_self_ty = scope.check_type_expr(&impl_item.self_ty)?;
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let matches_receiver = match (receiver_def, &receiver_ty.kind, &self_ty.kind) {
                (Some(receiver_def), TyKind::Adt(_, receiver_args), TyKind::Adt(impl_receiver, impl_args)) => {
                    impl_receiver.did == receiver_def
                        && scope.generic_args_compatible(impl_args, receiver_args)
                }
                (None, TyKind::Adt(_, _), _) => false,
                (None, _, _) => self_ty == receiver_ty,
                (Some(_), _, _) => false,
            };
            if !matches_receiver {
                continue;
            }
            for impl_item in &impl_item.items {
                let hir::ImplItemKind::Method(function) = &impl_item.kind else {
                    continue;
                };
                if impl_item.name == *method {
                    let signature = scope.function_signature(function)?;
                    let TyKind::FnPtr(sig) = &signature.kind else {
                        return Ok(None);
                    };
                    let Some(self_input) = sig.binder.value.inputs.first() else {
                        return Ok(None);
                    };
                    // `Self`'s position, substituted from the *actual*
                    // receiver — everything else in the signature stays
                    // in terms of the method's own generics for now.
                    let mut substitutions = HashMap::new();
                    if scope.unify_call_types(self_input, receiver_ty, &mut substitutions).is_err() {
                        return Ok(None);
                    }
                    let substituted = scope.substitute_param_map_fn_sig(&sig.binder.value, &substitutions);
                    return Ok(Some(Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: substituted,
                                bound_vars: sig.binder.bound_vars.clone(),
                            },
                        }),
                    }));
                }
            }
        }
        Ok(None)
    }

    /// Substitutes a partial `ParamTy -> Ty` map through every input/output
    /// position of a function signature — the same substitution
    /// `substitute_param_map` already does for a single `Ty`, applied
    /// across a whole `FnSig` (used by `method_declared_signature`, which
    /// only has `Self`'s own substitution available yet, not a full
    /// `instantiate_call` result).
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

    fn method_output(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
        actuals: &[Ty],
    ) -> Result<(hir::DefId, Option<Vec<Ty>>, Ty)> {
        let receiver_ty = match &receiver_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver_ty,
        };
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did),
            _ => None,
        };
        let impl_items = self.impl_lookup_items();
        // An ADT receiver can only ever match an impl whose self-type also
        // resolves to `TyKind::Adt` with the same `did` (see the
        // `matches_receiver` match below) — go straight to that bucket via
        // the fast-reject index instead of fully type-checking every
        // impl's self-type in the workspace. A non-ADT receiver (rare:
        // extension impls on primitives/tuples/etc.) falls back to
        // checking every impl, exactly as before.
        let candidate_indices: Vec<usize> = match receiver_def {
            Some(def_id) => self
                .impl_items_by_receiver_def()
                .get(&def_id)
                .cloned()
                .unwrap_or_default(),
            None => (0..impl_items.len()).collect(),
        };
        for &item_index in &candidate_indices {
            let item = &impl_items[item_index];
            let hir::ItemKind::Impl(impl_item) = &item.kind else {
                continue;
            };
            let mut scope = self.generic_scope(&impl_item.generics);
            let checked_self_ty = scope.check_type_expr(&impl_item.self_ty)?;
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let matches_receiver = match (receiver_def, &receiver_ty.kind, &self_ty.kind) {
                (Some(receiver_def), TyKind::Adt(_, receiver_args), TyKind::Adt(impl_receiver, impl_args)) => {
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
                (None, TyKind::Adt(_, _), _) => false,
                (None, _, _) => self_ty == receiver_ty,
                (Some(_), _, _) => false,
            };
            if !matches_receiver {
                continue;
            }
            scope.self_types.push(checked_self_ty);
            let assoc_types = scope.impl_assoc_types(&impl_item.items)?;
            scope.assoc_types.push(assoc_types);
            let impl_generics = impl_item.generics.clone();
            for impl_item in &impl_item.items {
                let hir::ImplItemKind::Method(function) = &impl_item.kind else {
                    continue;
                };
                if impl_item.name == *method {
                    let signature = scope.function_signature(function)?;
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
                    scope.assoc_types.pop();
                    scope.self_types.pop();
                    return Ok((impl_item.def_id, args, result));
                }
            }
            scope.assoc_types.pop();
            scope.self_types.pop();
        }
        Err(Error::from(format!("method `{method}` was not found")))
    }

    /// Lazily builds and memoizes `TypingShared::impl_lookup_items` (see its
    /// doc comment) on first use — shared across every item's task, since
    /// it's expensive and doesn't depend on which item is being checked —
    /// then returns a cheap `Rc` clone on every later call.
    fn impl_lookup_items(&self) -> Rc<Vec<hir::Item>> {
        if self.shared.impl_lookup_items.borrow().is_none() {
            let mut items = self.shared.program.items.clone();
            if let Some(context) = &self.shared.typing_context {
                items.extend(
                    context
                        .env_ctx
                        .hir_definitions()
                        .into_iter()
                        .flat_map(|(_, program, _)| program.items),
                );
            }
            *self.shared.impl_lookup_items.borrow_mut() = Some(Rc::new(items));
        }
        self.shared
            .impl_lookup_items
            .borrow()
            .clone()
            .expect("just populated above")
    }

    /// Lazily builds and memoizes `TypingShared::impl_items_by_receiver_def`
    /// (see its doc comment) on first use, then returns a cheap `Rc` clone
    /// on every later call.
    fn impl_items_by_receiver_def(&self) -> Rc<HashMap<hir::DefId, Vec<usize>>> {
        if self.shared.impl_items_by_receiver_def.borrow().is_none() {
            let items = self.impl_lookup_items();
            let mut index: HashMap<hir::DefId, Vec<usize>> = HashMap::new();
            for (item_index, item) in items.iter().enumerate() {
                let hir::ItemKind::Impl(impl_item) = &item.kind else {
                    continue;
                };
                if let hir::TypeExprKind::Path(path) = &impl_item.self_ty.kind {
                    if let Some(hir::Res::Def(def_id)) = path.res {
                        index.entry(def_id).or_default().push(item_index);
                    }
                }
            }
            *self.shared.impl_items_by_receiver_def.borrow_mut() = Some(Rc::new(index));
        }
        self.shared
            .impl_items_by_receiver_def
            .borrow()
            .clone()
            .expect("just populated above")
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
        let mut args = Vec::new();
        for (index, parameter) in impl_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                return Err(Error::from(format!(
                    "could not infer generic parameter `{}` in impl method",
                    parameter.name
                )));
            };
            args.push(argument.clone());
        }
        for (index, parameter) in method_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                return Err(Error::from(format!(
                    "could not infer generic parameter `{}` in method",
                    parameter.name
                )));
            };
            args.push(argument.clone());
        }
        Ok(Some(args))
    }

    fn bind_pattern(&mut self, pattern: &hir::Pat, ty: Ty) -> Result<()> {
        self.shared.results.borrow_mut().record_pat_type(pattern.hir_id, ty.clone());
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
                let Some(scope) = self.locals.last_mut() else {
                    self.record_error("no local scope is active");
                    return Ok(());
                };
                scope.insert(name.clone(), ty);
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
                    self.bind_pattern(pattern, *field)?;
                }
            }
            hir::PatKind::Struct(path, fields, _) => {
                if self.enum_variant_ty(path)?.is_some() {
                    let (_, payloads) = self.variant_payload_types(path, &adt_ty)?;
                    let [payload] = payloads.as_slice() else {
                        self.record_error("struct enum pattern requires exactly one payload type");
                        return Ok(());
                    };
                    for field in fields {
                        let field_ty = self.field_ty(payload, &field.name)?;
                        self.bind_pattern(&field.pat, field_ty)?;
                    }
                } else {
                    let struct_ty = if path.segments.is_empty() {
                        adt_ty.clone()
                    } else {
                        self.path_ty(path)?
                    };
                    self.require_same_adt(&adt_ty, &struct_ty, "struct pattern")?;
                    for field in fields {
                        let field_ty = self.field_ty(&struct_ty, &field.name)?;
                        self.bind_pattern(&field.pat, field_ty)?;
                    }
                }
            }
            hir::PatKind::TupleStruct(path, patterns) => {
                let (_, payloads) = self.variant_payload_types(path, &adt_ty)?;
                if patterns.len() != payloads.len() {
                    self.record_error("tuple struct pattern arity does not match variant");
                    return Ok(());
                }
                for (pattern, payload) in patterns.iter().zip(payloads) {
                    self.bind_pattern(pattern, payload)?;
                }
            }
            hir::PatKind::Variant(path) => {
                let (_, payloads) = self.variant_payload_types(path, &adt_ty)?;
                if !payloads.is_empty() {
                    self.record_error("payload variant requires a tuple or struct pattern");
                }
            }
        }
        Ok(())
    }

    fn field_ty(&mut self, receiver: &Ty, field: &hir::Symbol) -> Result<Ty> {
        let receiver = match &receiver.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver,
        };
        let TyKind::Adt(adt, args) = &receiver.kind else {
            return Ok(self.error_ty(format!(
                "field access `{field}` requires a struct, found {:?}",
                receiver.kind
            )));
        };
        let Some(item) = self.shared.program.def_map.get(&adt.did).cloned() else {
            return Ok(self.error_ty("struct definition was not found"));
        };
        let hir::ItemKind::Struct(def) = item.kind else {
            return Ok(self.error_ty("field access requires a struct"));
        };
        let Some(field_def) = def.fields.iter().find(|candidate| candidate.name == *field) else {
            return Ok(self.error_ty(format!("field `{field}` was not found")));
        };
        let mut scope = self.generic_scope(&def.generics);
        let result = scope.check_type_expr(&field_def.ty);
        let ty = result?;
        let substituted = scope.substitute_params(ty, args);
        drop(scope);
        Ok(substituted)
    }

    fn variant_payload_types(&mut self, path: &hir::Path, scrutinee: &Ty) -> Result<(Ty, Vec<Ty>)> {
        let Some(hir::Res::Def(variant_id)) = path.res else {
            return Ok((self.error_ty("variant pattern is unresolved"), Vec::new()));
        };
        if let Some((item, variant)) = self.enum_variant_by_def_id(variant_id) {
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
            let mut scope = self.generic_scope(&def.generics);
            let payload_result = scope.check_type_expr(payload);
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

    fn enum_struct_payload_type(&mut self, path: &hir::Path, scrutinee: &Ty) -> Result<Option<Ty>> {
        if self.enum_variant_ty(path)?.is_none() {
            return Ok(None);
        }
        let (_, payloads) = self.variant_payload_types(path, scrutinee)?;
        let Some(payload) = payloads.into_iter().next() else {
            return Ok(None);
        };
        let TyKind::Adt(adt, _) = &payload.kind else {
            return Ok(None);
        };
        if matches!(
            self.shared.program.def_map.get(&adt.did).map(|item| &item.kind),
            Some(hir::ItemKind::Struct(_))
        ) {
            Ok(Some(payload))
        } else {
            Ok(None)
        }
    }

    fn enum_variant_ty(&mut self, path: &hir::Path) -> Result<Option<Ty>> {
        let Some(hir::Res::Def(variant_id)) = path.res else {
            return Ok(None);
        };
        let Some((item, _)) = self.enum_variant_by_def_id(variant_id) else {
            return Ok(None);
        };
        Ok(Some(self.enum_item_ty(&item, path)?))
    }

    fn enum_variant_by_def_id(
        &self,
        variant_id: hir::DefId,
    ) -> Option<(hir::Item, hir::EnumVariant)> {
        // `program.items` only ever holds *this* package's own items — a
        // dependency's enums (e.g. `std`'s `Option`/`Result`, or any other
        // package's own enum) are copied only into `program.def_map` by
        // `seed_workspace_definitions` (deliberately not duplicated into
        // `items`, see its own doc comment), so a variant scan restricted
        // to `items` alone can never match a foreign enum's variant
        // `DefId` — exactly the same distinction `field_ty`'s struct
        // lookup (`program.def_map.get(&adt.did)`) already accounts for.
        self.shared.program
            .items
            .iter()
            .chain(self.shared.program.def_map.values())
            .find_map(|item| {
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

    async fn check_intrinsic(&mut self, call: &hir::IntrinsicCallExpr) -> Result<Ty> {
        use fp_core::intrinsics::IntrinsicKind;
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
            call.kind,
            IntrinsicKind::SizeOf | IntrinsicKind::FieldCount | IntrinsicKind::MethodCount
        ) {
            return Ok(Ty::uint(ty::UintTy::U64));
        }
        let mut arg_types = Vec::with_capacity(call.callargs.len());
        for arg in &call.callargs {
            arg_types.push(self.check_expr(&arg.value).await?);
        }
        Ok(match call.kind {
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
            IntrinsicKind::Len => Ty::uint(ty::UintTy::U64),
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
                None => self.error_ty(format!("{:?} intrinsic requires an argument", call.kind)),
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
            IntrinsicKind::FieldType | IntrinsicKind::VecType => {
                self.error_ty("type-valued intrinsic has no HIR type representation")
            }
            IntrinsicKind::TypeOf
            | IntrinsicKind::CreateStruct
            | IntrinsicKind::AddField
            | IntrinsicKind::BuildType => {
                self.error_ty("type-valued intrinsic typing is not implemented")
            }
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
            _ => self.error_ty(format!("intrinsic `{:?}` has no HIR type rule", call.kind)),
        })
    }

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

    fn unit_ty(&self) -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    fn literal_ty(&self, literal: &hir::Lit) -> Ty {
        match literal {
            hir::Lit::Bool(_) => Ty::bool(),
            hir::Lit::Char(_) => Ty::char(),
            hir::Lit::Integer(_) => Ty::int(ty::IntTy::I64),
            hir::Lit::Float(_) => Ty::float(ty::FloatTy::F64),
            hir::Lit::Str(_) => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            hir::Lit::Null => Ty::never(),
            // `b"..."` — a real Rust byte-string literal's type, `&[u8; N]`.
            hir::Lit::Bytes(bytes) => Ty {
                kind: TyKind::Ref(
                    ty::Region::ReErased,
                    Box::new(Ty {
                        kind: TyKind::Array(
                            Box::new(Ty::uint(ty::UintTy::U8)),
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: bytes.len() as u128,
                                    size: 8,
                                },
                            ))),
                        ),
                    }),
                    ty::Mutability::Not,
                ),
            },
            // `c"..."` — typed as `&std::ffi::CStr`, a real (empty) struct
            // that already resolves fine through ordinary ADT lookup; look
            // it up the same way `collection_constructor_signature` finds
            // `Vec`/`HashMap` (no HIR path to resolve against here, since
            // this literal has no backing definition of its own).
            hir::Lit::CStr(_) => self
                .well_known_struct_ty("CStr", Vec::new())
                .map(|ty| Ty {
                    kind: TyKind::Ref(ty::Region::ReErased, Box::new(ty), ty::Mutability::Not),
                })
                .unwrap_or_else(Ty::never),
        }
    }
}

/// Whether `ty` still has an uninstantiated generic parameter anywhere in
/// its structure — used to decide whether a call's result is worth
/// reconciling against an ambient expected-type hint (only meaningful for
/// still-generic results; a fully concrete type needs no such help).
fn ty_contains_param(ty: &Ty) -> bool {
    match &ty.kind {
        TyKind::Param(_) => true,
        TyKind::Ref(_, inner, _) => ty_contains_param(inner),
        TyKind::RawPtr(value) => ty_contains_param(&value.ty),
        TyKind::Slice(inner) => ty_contains_param(inner),
        TyKind::Array(inner, _) => ty_contains_param(inner),
        TyKind::Tuple(tys) => tys.iter().any(|ty| ty_contains_param(ty)),
        TyKind::Adt(_, args) => args.iter().any(|arg| match arg {
            GenericArg::Type(ty) => ty_contains_param(ty),
            _ => false,
        }),
        TyKind::FnPtr(signature) => {
            signature
                .binder
                .value
                .inputs
                .iter()
                .any(|ty| ty_contains_param(ty))
                || ty_contains_param(&signature.binder.value.output)
        }
        _ => false,
    }
}

fn primitive_path_ty(name: &str) -> Option<Ty> {
    Some(match name {
        "bool" => Ty::bool(),
        "char" => Ty::char(),
        "i8" => Ty::int(ty::IntTy::I8),
        "i16" => Ty::int(ty::IntTy::I16),
        "i32" => Ty::int(ty::IntTy::I32),
        "i64" => Ty::int(ty::IntTy::I64),
        "i128" => Ty::int(ty::IntTy::I128),
        "isize" => Ty::int(ty::IntTy::Isize),
        "u8" => Ty::uint(ty::UintTy::U8),
        "u16" => Ty::uint(ty::UintTy::U16),
        "u32" => Ty::uint(ty::UintTy::U32),
        "u64" => Ty::uint(ty::UintTy::U64),
        "u128" => Ty::uint(ty::UintTy::U128),
        "usize" => Ty::uint(ty::UintTy::Usize),
        "f32" => Ty::float(ty::FloatTy::F32),
        "f64" => Ty::float(ty::FloatTy::F64),
        "str" => Ty {
            kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
        },
        _ => return None,
    })
}

fn primitive_ty(primitive: TypePrimitive) -> Ty {
    match primitive {
        TypePrimitive::Bool => Ty::bool(),
        TypePrimitive::Char => Ty::char(),
        TypePrimitive::Int(int) => match int {
            TypeInt::I8 => Ty::int(ty::IntTy::I8),
            TypeInt::I16 => Ty::int(ty::IntTy::I16),
            TypeInt::I32 => Ty::int(ty::IntTy::I32),
            TypeInt::I64 => Ty::int(ty::IntTy::I64),
            TypeInt::I128 => Ty::int(ty::IntTy::I128),
            TypeInt::U8 => Ty::uint(ty::UintTy::U8),
            TypeInt::U16 => Ty::uint(ty::UintTy::U16),
            TypeInt::U32 => Ty::uint(ty::UintTy::U32),
            TypeInt::U64 => Ty::uint(ty::UintTy::U64),
            TypeInt::U128 => Ty::uint(ty::UintTy::U128),
            TypeInt::BigInt => Ty::int(ty::IntTy::I128),
        },
        TypePrimitive::Decimal(decimal) => Ty::float(match decimal {
            DecimalType::F32 => ty::FloatTy::F32,
            _ => ty::FloatTy::F64,
        }),
        TypePrimitive::String => Ty {
            kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
        },
        TypePrimitive::List => Ty {
            kind: TyKind::Slice(Box::new(Ty::never())),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test-only stand-in for the old `HirTypeChecker::new(program).check()`
    /// single-future entry point — drives `spawn_package_typecheck`'s
    /// per-item tasks to completion on a standalone executor (no driver,
    /// no comptime requests expected in these tests).
    fn typecheck_program_sync(program: hir::Program) -> Result<(hir::Program, TypeckResults)> {
        let (shared, mut future) = spawn_package_typecheck(program, None);
        let waker = std::task::Waker::noop();
        let mut cx = std::task::Context::from_waker(waker);
        loop {
            match future.as_mut().poll(&mut cx) {
                std::task::Poll::Ready(result) => {
                    result?;
                    return Ok(finish_package_typecheck(&shared));
                }
                std::task::Poll::Pending => {
                    if shared.executor.tick().is_none() {
                        panic!("typecheck_program_sync: executor stalled unexpectedly");
                    }
                }
            }
        }
    }

    #[test]
    fn records_literal_type_by_hir_id() {
        let expr = hir::Expr {
            hir_id: 7,
            kind: hir::ExprKind::Literal(hir::Lit::Integer(4)),
            span: fp_core::span::Span::null(),
        };
        let mut program = hir::Program::new();
        let item = hir::Item {
            hir_id: 1,
            def_id: hir::DefId::local(1),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Expr(expr),
            span: fp_core::span::Span::null(),
        };
        program.items.push(item.clone());
        // Real HIR lowering always populates `def_map` before typing begins
        // (see `ast_to_hir::transform_package`'s last step) — per-item tasks
        // look items up by `DefId` through it (needed so a cross-reference
        // to an item spawned only by `def_id`, not handed the `Item`
        // directly, can still find it), so a hand-built test program needs
        // to mirror that.
        program.def_map.insert(item.def_id, item);

        let (_, results) = typecheck_program_sync(program).expect("HIR type check");
        assert_eq!(results.expr_types.get(&7), Some(&Ty::int(ty::IntTy::I64)));
    }

    #[test]
    fn records_binding_pattern_type() {
        let pattern = hir::Pat {
            hir_id: 8,
            kind: hir::PatKind::Binding {
                name: "value".into(),
                mutable: false,
            },
        };
        let expr = hir::Expr {
            hir_id: 9,
            kind: hir::ExprKind::Let(
                pattern,
                Box::new(hir::TypeExpr {
                    hir_id: 10,
                    kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
                    span: fp_core::span::Span::null(),
                }),
                None,
            ),
            span: fp_core::span::Span::null(),
        };
        let mut program = hir::Program::new();
        let item = hir::Item {
            hir_id: 1,
            def_id: hir::DefId::local(1),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Expr(expr),
            span: fp_core::span::Span::null(),
        };
        program.items.push(item.clone());
        program.def_map.insert(item.def_id, item);

        let (_, results) = typecheck_program_sync(program).expect("HIR type check");
        assert_eq!(results.pat_types.get(&8), Some(&Ty::int(ty::IntTy::I64)));
    }
}
