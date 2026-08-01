use crate::*;
use fp_core::hir::*;
use fp_core::error::Result;
use fp_core::module::path::*;

impl HirTypeInferencer {
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

    pub(crate) async fn infer_module_inner(
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
        self.register_qualified_items(&file.items, &QualifiedPath::new(Vec::new()))
            .await;
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
    pub(crate) fn register_import_aliases_for_items<'a>(
        &self,
        items: &'a [Item],
    ) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
            for item in items {
                this.register_import_aliases_for_item(item).await;
            }
        })
    }

    pub(crate) async fn register_import_aliases_for_item(&self, item: &Item) {
        match item.kind() {
            ItemKind::Import(import) => self.register_import_aliases(import).await,
            ItemKind::Module(module) => self.register_import_aliases_for_items(&module.items).await,
            ItemKind::Impl(impl_block) => {
                self.register_import_aliases_for_items(&impl_block.items)
                    .await;
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
    pub(crate) fn predeclare_scope_items<'a>(&self, items: &'a [Item]) -> BoxFuture<'a, ()> {
        let this = self.clone();
        Box::pin(async move {
            for item in items {
                this.predeclare_item(item).await;
            }
        })
    }

    pub(crate) async fn predeclare_expr_scope(&self, expr: &Expr) {
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
    pub(crate) fn comptime_dependency_names(&self, expr: &Expr) -> Vec<String> {
        let mut names = Vec::new();
        self.collect_comptime_dependency_names(expr, &mut names);
        names
    }

    pub(crate) fn collect_comptime_dependency_names(&self, expr: &Expr, out: &mut Vec<String>) {
        let already_resolved = |name: &str| {
            self.typing_ctx.resolved_consts.borrow().contains_key(name)
                || self.typing_ctx.resolved_types.borrow().contains_key(name)
        };
        if let ExprKind::Name(name) = expr.kind() {
            let name = name.to_string();
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

    /// Tries to resolve `key`'s compile-time value right now: forces
    /// whatever other const/type-alias items `expr` depends on first (each
    /// of which is its own independently-spawned task -- see
    /// `HirTypeInferencer::tasks`/`predeclare_item`), then checks the cache and
    /// tries the resolution hook once. Never suspends and never fails on a
    /// mere "couldn't resolve it this way" outcome -- returns `Ok(None)` so
    /// the caller decides what a non-resolution means for it. Shared by
    /// `await_comptime` (which turns `None` into a genuine wait-or-error)
    /// and `best_effort_resolve_comptime` (which tolerates `None` outright).
    pub(crate) async fn try_resolve_comptime_now(
        &self,
        key: &str,
        expr: &Expr,
    ) -> Result<Option<Value>> {
        for name in self.comptime_dependency_names(expr) {
            self.force(&name).await?;
        }
        if let Some(value) = self.typing_ctx.resolved_consts.borrow().get(key).cloned() {
            return Ok(Some(value));
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
                return Ok(Some(value));
            }
        }
        Ok(None)
    }

    /// Ensures `key`'s compile-time value is resolved, returning the
    /// resolved `Value` directly -- callers do `let value =
    /// self.await_comptime(&key, expr).await?;`, not a separate map lookup
    /// after the fact. Use this only when the caller genuinely cannot
    /// proceed without the concrete value (e.g. `ExprKind::ConstBlock`,
    /// whose own type depends on it, or a concurrently-spawned sibling
    /// task's own resolution attempt). A `DefConst`/`DefType` item's own
    /// best-effort attempt to opportunistically populate `resolved_consts`
    /// for *other* code's later benefit -- where the item's own type is
    /// already fully known regardless of whether this resolves -- should
    /// use `best_effort_resolve_comptime` instead; treating every hook
    /// failure as "genuinely blocked, wait" is wrong when nothing else in
    /// this compile unit will ever produce the value (e.g. no concurrent
    /// task was ever spawned for `key`, because nothing actually depends on
    /// having it typed).
    ///
    /// Only genuinely suspends (via a real `Waker`, resumed precisely when
    /// the resolving task writes the value and calls
    /// `TypingContext::wake_comptime`) when a concurrently-spawned sibling
    /// task for this exact `key` is still in flight and might resolve it --
    /// mirrors `force`'s own dependency-side gating. Otherwise there is
    /// nothing left that could ever produce this value, so it fails fast
    /// instead of hanging forever.
    pub(crate) async fn await_comptime(&self, key: &str, expr: &Expr) -> Result<Value> {
        if let Some(value) = self.try_resolve_comptime_now(key, expr).await? {
            return Ok(value);
        }
        if !self.tasks.contains(key) {
            return Err(typing_error(format!(
                "could not resolve comptime value for `{key}`"
            )));
        }
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

    /// Best-effort counterpart to `await_comptime` for a `DefConst`/
    /// `DefType` item's own inline attempt to fold its own value: tries
    /// once (cache, then hook), and silently tolerates not resolving --
    /// mirrors the pre-async design's "note it as still-unresolved and keep
    /// going" behavior for values that later compiler stages (LIR-level
    /// constant folding, or plain runtime evaluation) can pick up on their
    /// own. The item's own type was already determined before this call, so
    /// there is nothing to block on here.
    pub(crate) async fn best_effort_resolve_comptime(&self, key: &str, expr: &Expr) {
        let _ = self.try_resolve_comptime_now(key, expr).await;
    }

    /// Same shape as `await_comptime`, but for a `type Foo = const { ... }`
    /// alias's resolved struct shape rather than a plain value.
    pub(crate) async fn await_struct_alias(&self, name: &str) -> Result<TypeStruct> {
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
    pub(crate) async fn force(&self, name: &str) -> Result<()> {
        if self.typing_ctx.resolved_consts.borrow().contains_key(name)
            || self.typing_ctx.resolved_types.borrow().contains_key(name)
        {
            return Ok(());
        }
        if !self.tasks.contains(name) {
            return Ok(());
        }
        let typing_ctx = self.typing_ctx.clone();
        let name_owned = name.to_string();
        std::future::poll_fn(move |cx| {
            if typing_ctx
                .resolved_consts
                .borrow()
                .contains_key(&name_owned)
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

    pub(crate) async fn finish(&self) -> TypingOutcome {
        let (outcome, diags) = {
            let mut inner = self.inner.borrow_mut();
            let outcome = TypingOutcome {
                resolved_names: std::mem::take(&mut inner.resolved_names),
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

    pub(crate) fn expr_id(&self, expr: &Expr) -> crate::ExprId {
        expr.id()
    }

    pub(crate) fn record_resolved_name(&self, expr_id: crate::ExprId, resolved_name: ResolvedName) {
        self.inner
            .borrow_mut()
            .resolved_names
            .insert(expr_id, resolved_name);
    }
}
