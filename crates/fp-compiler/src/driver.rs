use fp_backend::transformations::{HirGenerator, HirLoweringConfig, LirGenerator, MirLowering};
use fp_core::ast::{
    BlockStmt, Expr, ExprInvokeTarget, ExprKind, File, Item, ItemDefEnum, ItemDefStruct, ItemKind,
    Name, Ty, TypeStruct, TypeType, Value, Visibility,
};
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::module::path::QualifiedPath;
use fp_core::package::{PackageId, PackageSource};
use fp_core::span::Span;
use fp_interpret::LirInterpreter;
use fp_lang::FerroIntrinsicNormalizer;
use fp_typing::{ComptimeRequest, HirTypeChecker, TypeckResults, TypingContext};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;

use crate::{
    AstId, BytecodeId, CompilerDriverError, CompilerState, ConstValueId, ExecutorHandle,
    FullyQualifiedPath, HirId, LirId, MirId, RuntimeValueId,
};

pub struct CompilerDriver {
    pub state: CompilerState,
    interpreter: LirInterpreter,
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::package::CompiledPackage>>>,
    next_hir_def_id: u32,
}

struct CompileUnitCoreResult {
    hir_id: HirId,
    mir_id: MirId,
    lir_id: LirId,
}

struct TypingUnit {
    module_path: QualifiedPath,
    package_id: hir::PackageId,
    source: File,
    lowering_config: HirLoweringConfig,
    external_definitions: Vec<(QualifiedPath, hir::Program, HashMap<String, hir::Res>)>,
    def_id_start: u32,
}

impl CompilerDriver {
    /// Build (but do not spawn or drive) the HIR typing task for one source
    /// module. The future is owned by the driver's task pool, while the unit
    /// keeps the source document and module identity together.
    fn typing_future(
        &self,
        unit: TypingUnit,
    ) -> fp_typing::BoxFuture<
        'static,
        fp_core::error::Result<(hir::Program, TypeckResults, HashMap<String, hir::Res>, u32)>,
    > {
        let typing_context = self.state.typing_ctx.clone();
        Box::pin(async move {
            let mut generator = HirGenerator::new()
                .with_intrinsic_normalizer(FerroIntrinsicNormalizer::new(
                    fp_core::intrinsics::IntrinsicNormalizationMode::Compile,
                ))
                .with_package_id(unit.package_id)
                .with_def_id_start(unit.def_id_start)
                .with_lowering_config(unit.lowering_config)
                .with_external_definitions(unit.external_definitions);
            let result = match generator
                .transform_module_async(
                    &unit.module_path,
                    &unit.source.items,
                    typing_context.clone(),
                )
                .await
            {
                Ok(program) => {
                    let exports = generator.exported_symbols();
                    let next_def_id = generator.next_def_id_value();
                    HirTypeChecker::new(program)
                        .with_context(typing_context)
                        .check()
                        .await
                        .map(|(program, results)| (program, results, exports, next_def_id))
                }
                Err(error) => Err(error),
            };
            result
        })
    }

    pub fn new(data_layout: fp_core::lir::LirDataLayout, tasks: ExecutorHandle) -> Self {
        Self::with_workspace(
            data_layout,
            tasks,
            Rc::new(fp_core::workspace::WorkspaceContext::new()),
        )
    }

    pub fn with_workspace(
        data_layout: fp_core::lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<fp_core::workspace::WorkspaceContext>,
    ) -> Self {
        let mut state = CompilerState::new(data_layout.clone(), tasks);
        state.typing_ctx = Rc::new(TypingContext::new(data_layout, workspace));
        Self {
            state,
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            state,
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
        }
    }

    pub async fn compile_native(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        self.compile_unit_compile_native(ast_id, path).await
    }

    pub async fn compile_bytecode(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        self.compile_unit_compile_bytecode(ast_id, path).await
    }

    pub fn compile_native_sync(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_native(ast_id, path))
    }

    pub fn compile_bytecode_sync(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_unit_compile_bytecode(ast_id, path))
    }

    /// Focus subsequent module work on an already compiled package. The
    /// package itself and its imported dependencies remain shared through
    /// `Rc`; only the lookup context becomes package-local.
    pub fn focus_package(&mut self, package_id: PackageId) -> Result<(), CompilerDriverError> {
        let parent_context = self.state.typing_ctx.clone();
        let package_workspace = parent_context.env_ctx.for_package(package_id.clone());
        for (dependency_id, package) in parent_context.env_ctx.crates().iter() {
            if dependency_id != &package_id {
                package_workspace.import_package(dependency_id.clone(), package.clone());
            }
        }
        if let Some(std_package) = package_workspace.compiled_package(&PackageId::new("std")) {
            package_workspace.install_prelude(std_package);
        }
        let source = parent_context
            .env_ctx
            .compiled_package(&package_id)
            .map(|package| {
                let package = package.borrow();
                PackageSource {
                    package_id: package_id.clone(),
                    name: package.name.clone(),
                    graph: package.graph.clone(),
                    module_paths: package.module_paths.clone(),
                    items: package.items.clone(),
                }
            })
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        package_workspace.begin_package(package_id, source);
        self.state.typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
        ));
        Ok(())
    }

    pub fn execute_runtime(
        &mut self,
        lir_id: &LirId,
    ) -> Result<fp_core::ast::Value, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter.inject_globals(&resolved);
        let entrypoint = self.state.runtime_entrypoint(lir_id)?;
        let value = self.interpreter.run_entrypoint(&lir, entrypoint)?;
        let value_id = RuntimeValueId::new(format!("runtime_value:{}", lir_id.as_str()));
        self.state.insert_runtime_value(value_id, value.clone());
        Ok(value)
    }

    /// Compile a package after recursively compiling its declared
    /// dependencies. Dependency resolution and version selection happen in
    /// the provider; the driver only consumes the concrete package IDs it is
    /// given by metadata.
    pub async fn compile_package(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        let parent_context = self.state.typing_ctx.clone();
        if let Some(package) = self.compiled_packages.get(package_id).cloned() {
            parent_context
                .env_ctx
                .import_package(package_id.clone(), package.clone());
            return Ok(package);
        }
        if let Some(package) = parent_context.env_ctx.compiled_package(package_id) {
            return Ok(package);
        }
        if !self.building_packages.insert(package_id.clone()) {
            return Err(CompilerDriverError::UnresolvablePackage(format!(
                "dependency cycle involving {package_id}"
            )));
        }

        let std_package = if matches!(package_id.as_str(), "std" | "libc") {
            None
        } else {
            match Box::pin(self.compile_package(&PackageId::new("std"))).await {
                Ok(package) => Some(package),
                Err(error) => {
                    self.building_packages.remove(package_id);
                    return Err(error);
                }
            }
        };

        let package_workspace = parent_context.env_ctx.for_package(package_id.clone());
        if let Some(std_package) = std_package {
            package_workspace.import_package(PackageId::new("std"), std_package);
            if let Some(std_package) = package_workspace.compiled_package(&PackageId::new("std")) {
                package_workspace.install_prelude(std_package);
            }
        }
        self.state.typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
        ));

        let result: Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> =
            async {
                let provider = self
                    .state
                    .typing_ctx
                    .env_ctx
                    .provider_for(package_id)
                    .ok_or_else(|| {
                        CompilerDriverError::UnresolvablePackage(package_id.to_string())
                    })?;
                let metadata = provider
                    .load_package_metadata(package_id)
                    .map_err(|error| {
                        CompilerDriverError::UnresolvablePackage(format!("{package_id}: {error}"))
                    })?;

                for dependency in &metadata.metadata.dependencies {
                    let dependency_id = PackageId::new(dependency.package.clone());
                    let dependency_package = Box::pin(self.compile_package(&dependency_id)).await?;
                    if dependency_id.as_str() == "std" {
                        self.state
                            .typing_ctx
                            .env_ctx
                            .install_prelude(dependency_package);
                    }
                }

                let source = provider.load_package_source(package_id).map_err(|error| {
                    CompilerDriverError::UnresolvablePackage(format!("{package_id}: {error}"))
                })?;
                let package = self
                    .state
                    .typing_ctx
                    .env_ctx
                    .begin_package(package_id.clone(), source);
                let items = package.borrow().items.clone();
                let units = self.compile_items_to_lir_units(&items, true).await?;
                package.borrow_mut().lir_units = units;
                Ok(package)
            }
            .await;

        self.building_packages.remove(package_id);
        self.state.typing_ctx = parent_context.clone();
        let package = result?;
        self.compiled_packages
            .insert(package_id.clone(), package.clone());
        parent_context
            .env_ctx
            .import_package(package_id.clone(), package.clone());
        Ok(package)
    }

    /// The real single-source-file entry point: fetches the stored `File`
    /// and delegates to `compile_module_core` with its items — the shared
    /// core itself never touches `ast::File`.
    async fn compile_unit_core(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<CompileUnitCoreResult, CompilerDriverError> {
        let ast = self.state.ast(ast_id)?.clone();
        let result = self
            .compile_module_core(path.path().clone(), ast.items, ast_id, path)
            .await?;
        Ok(result)
    }

    /// Runs the typer → HIR → MIR → LIR pipeline for a module's items
    /// directly — no `ast::File` involved. Used by `compile_unit_core` (the
    /// one real user-source-file case, which extracts `.items` from its
    /// stored `File` before calling in) and, more importantly, by
    /// `compile_items_to_lir_units`/generic monomorphization, which already
    /// have `(QualifiedPath, Vec<Item>)` in hand and no real file at all.
    async fn compile_module_core(
        &mut self,
        module_path: QualifiedPath,
        items: Vec<Item>,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<CompileUnitCoreResult, CompilerDriverError> {
        let key = format!("module:{}", ast_id.as_str());
        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .package_id_for_module(&module_path)
            .unwrap_or_default();
        let source = File {
            path: std::path::PathBuf::from(path.to_key()),
            attrs: Vec::new(),
            collected_items: Vec::new(),
            items,
        };
        if self.state.tasks.contains(&key) {
            return Err(CompilerDriverError::UnsupportedWork(format!(
                "typing task already exists: {key}"
            )));
        }
        let typing_handle = self.state.tasks.spawn(
            key,
            self.typing_future(TypingUnit {
                module_path,
                package_id,
                source,
                lowering_config: HirLoweringConfig,
                external_definitions: self.state.typing_ctx.env_ctx.hir_definitions(),
                def_id_start: self.next_hir_def_id,
            }),
        );
        self.run_pool_to_idle().await?;
        let (hir_program, typeck_results, hir_exports, next_def_id) = typing_handle.await?;
        self.next_hir_def_id = self.next_hir_def_id.max(next_def_id);
        if let Some(package_id) = self.state.typing_ctx.env_ctx.current_package().cloned() {
            if let Some(package) = self.state.typing_ctx.env_ctx.compiled_package(&package_id) {
                package.borrow_mut().hir_exports.extend(hir_exports);
            }
        }
        let entrypoint = hir_program.items.iter().find_map(|item| match &item.kind {
            hir::ItemKind::Function(function) if function.sig.name.as_str() == "main" => {
                Some(item.def_id)
            }
            _ => None,
        });

        // `run_pool_to_idle` only returns once every package/comptime need
        // the typer touched has actually been satisfied in place — nothing
        // pending to check for here, just lower for real.
        let hir_id = HirId::new(format!("hir:{}", path.to_key()));
        self.state.insert_hir(hir_id.clone(), hir_program);
        self.state.insert_hir_typeck(hir_id.clone(), typeck_results);
        let mir_id = self.lower_to_mir(&hir_id, path)?;
        let lir_id = self.lower_to_lir(&mir_id, path)?;
        if let Some(entrypoint) = entrypoint {
            self.state
                .insert_runtime_entrypoint(lir_id.clone(), entrypoint);
        }

        Ok(CompileUnitCoreResult {
            hir_id,
            mir_id,
            lir_id,
        })
    }

    /// Ticks the shared task pool (`CompilerState::tasks`) until nothing is
    /// left ready or parked, loading whatever package is blocking progress
    /// on demand when nothing is ready to poll. Has no idea what any task
    /// computes -- not "which module," not "which compile unit," not
    /// "typing" at all. Every suspendable unit of driver work is just a task
    /// spawned into this one pool: per-const/per-type-alias comptime
    /// resolution (spawned in `predeclare_item`), the per-compile-unit
    /// module-typing task (`compile_module_core`), and
    /// generic-monomorphization-ready signals (`infer_generic_function_call_body`).
    ///
    /// A task suspends and resumes exactly where it needs a compiler-owned
    /// comptime value. Package dependencies are loaded before the module
    /// task is scheduled.
    ///
    /// Safe to run all the way to full idle (not just until one specific
    /// task resolves): nothing in this pool ever suspends waiting on a task
    /// that hasn't already been spawned (`await_comptime`/`force` fail fast
    /// instead of registering a wait for a name nothing has registered yet),
    /// so this always terminates -- either every task resolves, or a
    /// genuine deadlock is reported.
    ///
    /// Every key that resolves (not just the caller's own, if it has one in
    /// mind) is handed to `handle_resolved_task`, which is a no-op unless
    /// that key is a pending generic-specialization signal. Package loading
    /// is deliberately absent from this loop: a parked task indicates a
    /// compiler task deadlock or an invalid package build order.
    async fn run_pool_to_idle(&mut self) -> Result<(), CompilerDriverError> {
        loop {
            let requests = self.state.typing_ctx.take_comptime_requests();
            if !requests.is_empty() {
                for pending in requests {
                    let value = self.answer_hir_comptime(pending.request()).await?;
                    pending.complete(Ok(value));
                }
                continue;
            }
            if let Some(key) = self.state.tasks.tick() {
                self.handle_resolved_task(&key).await?;
                continue;
            }
            if self.state.typing_ctx.has_comptime_requests() {
                continue;
            }
            if self.state.tasks.is_idle() {
                return Ok(());
            }
            return Err(CompilerDriverError::UnresolvableComptime(
                "no ready task and no comptime request".to_string(),
            ));
        }
    }

    /// Called by `run_pool_to_idle` for *every* key that resolves in the
    /// shared task pool. A no-op unless `key` is a pending
    /// generic-specialization signal (`TypingContext::ready_generics`,
    /// written by `infer_generic_function_call_body` the moment a call's
    /// concrete type arguments resolve, alongside the trivial always-ready
    /// task spawned under the same key -- see that field's doc comment for
    /// why: there's no dependency reason a compile unit's own typing ever
    /// waits on this, it already has everything it needs to type the call
    /// site itself via ordinary unification against the generic signature's
    /// own declared return type; the task exists purely to surface "this is
    /// ready" through the same resolve-and-dispatch loop everything else
    /// flows through).
    ///
    /// When it is one: dedups via `generic_cannon_key`/`generic_instantiations`
    /// (unchanged), finds the original definition by its stable `ItemId` in
    /// *the discovering compile unit's own pre-typing stored AST*
    /// (`self.state.ast(&AstId::new(monomorph.ast_key))`; safe because
    /// `Item` derives
    /// `Clone` including its `id`, so the pre-typing clone typing was
    /// working from carries the identical `ItemId`), substitutes the
    /// concrete types into a clone, and compiles the specialization directly
    /// through the shared compiler task pool.
    fn handle_resolved_task<'a>(
        &'a mut self,
        key: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<(), CompilerDriverError>> + 'a>> {
        Box::pin(async move {
            let Some(monomorph) = self
                .state
                .typing_ctx
                .ready_generics
                .borrow_mut()
                .remove(key)
            else {
                return Ok(());
            };
            let cannon_key = Self::generic_cannon_key(
                &FullyQualifiedPath::new(monomorph.function_path.clone()),
                &monomorph.concrete_types,
            );
            if self.state.generic_instantiations.contains(&cannon_key) {
                return Ok(());
            }
            self.state.generic_instantiations.insert(cannon_key.clone());

            let ast_id = AstId::new(monomorph.ast_key.clone());
            let original = self.state.ast(&ast_id)?;
            let mut func_item = Self::find_item_by_id(&original.items, monomorph.item_id)
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "generic function not found: {}",
                        monomorph.function_path.to_key()
                    ))
                })?;

            if let ItemKind::DefFunction(def) = func_item.kind_mut() {
                for param in &mut def.sig.params {
                    Self::substitute_in_ty(
                        &mut param.ty,
                        &monomorph.generic_params,
                        &monomorph.concrete_types,
                    );
                }
                if let Some(ret_ty) = &mut def.sig.ret_ty {
                    Self::substitute_in_ty(
                        ret_ty,
                        &monomorph.generic_params,
                        &monomorph.concrete_types,
                    );
                }
                def.sig.generics_params.clear();
                Self::substitute_in_block(
                    &mut def.body,
                    &monomorph.generic_params,
                    &monomorph.concrete_types,
                );
            }

            let specialized_path =
                FullyQualifiedPath::new(monomorph.function_path.with_segment(cannon_key.clone()));
            let specialized_ast_id = AstId::new(format!("ast:{}", specialized_path.to_key()));
            let file = File {
                path: std::path::PathBuf::new(),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: vec![func_item],
            };
            self.state.insert_ast(specialized_ast_id.clone(), file);

            self.compile_unit_compile_native(&specialized_ast_id, &specialized_path)
                .await?;
            Ok(())
        })
    }

    /// Evaluate a comptime request using the HIR unit supplied by typing.
    /// The driver never looks up an AST expression or reconstructs source
    /// from a string key here: the request is the complete evaluation input.
    fn find_hir_item(program: &hir::Program, target: hir::DefId) -> Option<&hir::Item> {
        fn in_block(block: &hir::Block, target: hir::DefId) -> Option<&hir::Item> {
            for stmt in &block.stmts {
                if let hir::StmtKind::Item(item) = &stmt.kind {
                    if item.def_id == target {
                        return Some(item);
                    }
                    if let Some(found) = in_item(item, target) {
                        return Some(found);
                    }
                }
            }
            block.expr.as_deref().and_then(|expr| in_expr(expr, target))
        }

        fn in_expr(expr: &hir::Expr, target: hir::DefId) -> Option<&hir::Item> {
            match &expr.kind {
                hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => in_block(block, target),
                hir::ExprKind::While(condition, block) => {
                    in_expr(condition, target).or_else(|| in_block(block, target))
                }
                hir::ExprKind::If(condition, then_expr, else_expr) => in_expr(condition, target)
                    .or_else(|| in_expr(then_expr, target))
                    .or_else(|| else_expr.as_deref().and_then(|expr| in_expr(expr, target))),
                hir::ExprKind::Binary(_, left, right)
                | hir::ExprKind::Assign(left, right)
                | hir::ExprKind::Index(left, right) => {
                    in_expr(left, target).or_else(|| in_expr(right, target))
                }
                hir::ExprKind::Unary(_, value)
                | hir::ExprKind::Reference(hir::ExprReference { expr: value, .. })
                | hir::ExprKind::Return(Some(value))
                | hir::ExprKind::Break(Some(value))
                | hir::ExprKind::Cast(value, _) => in_expr(value, target),
                hir::ExprKind::Call(callee, args) | hir::ExprKind::MethodCall(callee, _, args) => {
                    in_expr(callee, target)
                        .or_else(|| args.iter().find_map(|arg| in_expr(&arg.value, target)))
                }
                hir::ExprKind::FieldAccess(value, _) => in_expr(value, target),
                hir::ExprKind::Slice(slice) => in_expr(&slice.base, target)
                    .or_else(|| {
                        slice
                            .start
                            .as_deref()
                            .and_then(|expr| in_expr(expr, target))
                    })
                    .or_else(|| slice.end.as_deref().and_then(|expr| in_expr(expr, target))),
                hir::ExprKind::Struct(_, fields) => {
                    fields.iter().find_map(|field| in_expr(&field.expr, target))
                }
                hir::ExprKind::Match(scrutinee, arms) => in_expr(scrutinee, target).or_else(|| {
                    arms.iter().find_map(|arm| {
                        arm.guard
                            .as_ref()
                            .and_then(|guard| in_expr(guard, target))
                            .or_else(|| in_expr(&arm.body, target))
                    })
                }),
                hir::ExprKind::Try(value) => in_expr(&value.expr, target)
                    .or_else(|| {
                        value
                            .catches
                            .iter()
                            .find_map(|catch| in_expr(&catch.body, target))
                    })
                    .or_else(|| value.elze.as_deref().and_then(|expr| in_expr(expr, target)))
                    .or_else(|| {
                        value
                            .finally
                            .as_deref()
                            .and_then(|expr| in_expr(expr, target))
                    }),
                hir::ExprKind::With(context, body) => {
                    in_expr(context, target).or_else(|| in_expr(body, target))
                }
                hir::ExprKind::Array(values) => {
                    values.iter().find_map(|value| in_expr(value, target))
                }
                hir::ExprKind::ArrayRepeat { elem, len } => {
                    in_expr(elem, target).or_else(|| in_expr(len, target))
                }
                hir::ExprKind::IntrinsicCall(call) => call
                    .callargs
                    .iter()
                    .find_map(|arg| in_expr(&arg.value, target)),
                hir::ExprKind::Let(_, _, value) => {
                    value.as_deref().and_then(|value| in_expr(value, target))
                }
                hir::ExprKind::Literal(_)
                | hir::ExprKind::Path(_)
                | hir::ExprKind::Query(_)
                | hir::ExprKind::Continue
                | hir::ExprKind::FormatString(_)
                | hir::ExprKind::Return(None)
                | hir::ExprKind::Break(None) => None,
            }
        }

        fn in_item(item: &hir::Item, target: hir::DefId) -> Option<&hir::Item> {
            match &item.kind {
                hir::ItemKind::Function(function) => function
                    .body
                    .as_ref()
                    .and_then(|body| in_block(body, target)),
                hir::ItemKind::Const(constant) => in_expr(&constant.body.value, target),
                hir::ItemKind::Impl(impl_item) => impl_item.items.iter().find_map(|item| {
                    if item.def_id == target {
                        None
                    } else {
                        match &item.kind {
                            hir::ImplItemKind::Method(function) => function
                                .body
                                .as_ref()
                                .and_then(|body| in_block(body, target)),
                            hir::ImplItemKind::AssocConst(constant) => {
                                in_expr(&constant.body.value, target)
                            }
                        }
                    }
                }),
                _ => None,
            }
        }

        program.items.iter().find_map(|item| {
            (item.def_id == target)
                .then_some(item)
                .or_else(|| in_item(item, target))
        })
    }

    async fn answer_hir_comptime(
        &mut self,
        request: &ComptimeRequest,
    ) -> Result<Value, CompilerDriverError> {
        let target_item =
            Self::find_hir_item(&request.program, request.target).ok_or_else(|| {
                CompilerDriverError::UnresolvableComptime(format!(
                    "HIR comptime target {} is not a const",
                    request.target
                ))
            })?;
        let target_const = match &target_item.kind {
            hir::ItemKind::Const(constant) => constant,
            _ => {
                return Err(CompilerDriverError::UnresolvableComptime(format!(
                    "HIR comptime target {} is not a const",
                    request.target
                )));
            }
        };

        // A comptime request is emitted while the enclosing function is still
        // being type-checked. Lower a zero-argument probe for only the
        // requested const; lowering the whole function would require types
        // for later runtime locals that the checker has not reached yet.
        let probe_name = format!(
            "__fp_comptime_probe_{}_{}",
            request.target.package_id.0, request.target.index
        );
        let probe = hir::Item {
            hir_id: target_item.hir_id,
            def_id: request.target.saturating_add(1),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Function(hir::Function {
                sig: hir::FunctionSig {
                    name: hir::Symbol::new(probe_name.clone()),
                    inputs: Vec::new(),
                    output: target_const.ty.clone(),
                    generics: hir::Generics::default(),
                    abi: hir::Abi::Rust,
                },
                body: Some(hir::Block {
                    hir_id: target_const.body.hir_id,
                    stmts: Vec::new(),
                    expr: Some(Box::new(target_const.body.value.clone())),
                }),
                is_const: true,
                is_extern: false,
                attrs: Vec::new(),
            }),
            span: target_item.span,
        };
        let mut comptime_program = request.program.clone();
        let mut probe_items = vec![probe.clone()];
        probe_items.extend(
            request
                .program
                .def_map
                .values()
                .filter(|item| {
                    matches!(
                        item.kind,
                        hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) | hir::ItemKind::Impl(_)
                    )
                })
                .cloned(),
        );
        comptime_program.items = probe_items;
        comptime_program.def_map.insert(probe.def_id, probe);

        let mut lowering = MirLowering::new()
            .with_typeck_results(&request.typeck_results)
            .map_err(CompilerDriverError::Core)?;
        let mir = lowering
            .transform(comptime_program)
            .map_err(CompilerDriverError::Core)?;
        let mut lir_generator = LirGenerator::new(self.state.typing_ctx.data_layout.clone());
        let lir = lir_generator
            .transform(mir)
            .map_err(CompilerDriverError::Core)?;

        let package_id = request.target.package_id;
        let unit = fp_core::lir::LirCompileUnit {
            package_id,
            module_path: QualifiedPath::new(Vec::new()),
            program: lir,
        };
        self.interpreter = LirInterpreter::new();
        self.interpreter
            .inject_globals(&self.collect_resolved_const_values());
        let mut value = self
            .interpreter
            .run_function_named(&[unit], package_id, &probe_name)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        if let Some(resolved) = Self::resolve_comptime_value(&mut self.interpreter, &value) {
            value = resolved;
        }
        Ok(value)
    }

    /// Try to resolve `expr` as a compile-time value right now, scoped to
    /// just this expression — no synthetic file/item, no re-typing. `expr`
    /// arrives already fully typed (the caller in fp-typing only invokes
    /// this after its own `infer_expr_inner` pass, which stamps a concrete
    /// `Ty` on every node). Before lowering, any reference to an
    /// already-resolved const (by name) is inlined as a literal value —
    /// MIR lowering only resolves free identifiers against items actually
    /// declared in the same program, and this probe deliberately declares
    /// none, so a bare cross-reference would otherwise be unresolvable.
    /// The inlined expression then goes through `HirGenerator::transform_expr`
    /// (lowers one expression into a minimal HIR program with a
    /// synthesized `main`), non-lossy `MirLowering`, and `LirGenerator`,
    /// then runs through the interpreter. On success the value is stored
    /// under `key` (and, if `key` is a `__fp_expr_<id>` key, also into
    /// `expr_resolutions` so the originating `ConstBlock` expression sees
    /// it). This is the compiler's comptime probe implementation.
    ///
    /// Returns `false` on any lowering/evaluation failure — e.g. the
    /// expression references another dependency this scoped lowering can't
    /// see (a sibling function, an as-yet-unresolved const) — leaving the
    /// caller to resolve it through the compiler task pool.
    ///
    /// Takes `&TypingContext` rather than `&mut self`/`&self` — everything
    /// this needs (resolved consts/types, the workspace's compiled crates)
    /// is reachable through it, and `ComptimeHook` only holds an
    /// `Rc<TypingContext>`, not a `&CompilerDriver`.
    fn resolve_comptime_now(typing_ctx: &TypingContext, key: &str, expr: &Expr) -> Option<Value> {
        let mut probe_expr = expr.clone();
        let resolved_names = Self::resolved_const_values_snapshot(typing_ctx);
        Self::inline_resolved_names(&mut probe_expr, &resolved_names);

        let resolved = (|| -> Result<Value, CompilerDriverError> {
            let hir_program = HirGenerator::new().transform_expr(&probe_expr)?;
            let mir_program = MirLowering::new().transform(hir_program)?;
            let lir_program =
                LirGenerator::new(typing_ctx.data_layout.clone()).transform(mir_program)?;

            let mut units = vec![fp_core::lir::LirCompileUnit {
                package_id: fp_core::hir::PackageId(0),
                module_path: QualifiedPath::new(Vec::new()),
                program: lir_program,
            }];
            // Struct construction (and any other intrinsic-backed runtime
            // support) may depend on workspace crates' compiled LIR, exactly
            // as `evaluate_comptime_lir` includes for the whole-file path —
            // a bare single-expression unit alone doesn't carry that support.
            for krate in typing_ctx.env_ctx.crates().values() {
                units.extend(krate.borrow().lir_units.iter().cloned());
            }
            let mut interpreter = LirInterpreter::new();
            interpreter.inject_globals(&Self::resolved_const_values_snapshot(typing_ctx));
            let mut value =
                interpreter.run_function_named(&units, fp_core::hir::PackageId(0), "main")?;
            // Only int/uint results that are *actually* comptime struct
            // construction (per the expression's own type) are raw object
            // handles needing resolution — treating every plain integer as a
            // possible handle risks coincidentally resolving to an unrelated
            // object in this probe's otherwise-mostly-empty object table.
            let is_struct_construction =
                matches!(expr.ty(), Some(Ty::Type(_)) | Some(Ty::Struct(_)));
            if is_struct_construction {
                if let Some(resolved) = Self::resolve_comptime_value(&mut interpreter, &value) {
                    value = resolved;
                }
            }
            Ok(value)
        })();

        let Ok(value) = resolved else {
            return None;
        };

        if let Some(struct_ty) = Self::extract_struct_type(&value) {
            let name = struct_ty.name.as_str().to_string();
            typing_ctx
                .resolved_types
                .borrow_mut()
                .insert(name.clone(), struct_ty);
            typing_ctx.wake_comptime(&name);
        }

        // Store under `typing_ctx.resolved_consts` (checked by name for
        // `DefConst`) and, if this is a `ConstBlock`'s key, also into
        // `expr_resolutions` (checked by expr id) — the two lookups the
        // fp-typing call sites use to recognize an already-resolved value.
        typing_ctx
            .resolved_consts
            .borrow_mut()
            .insert(key.to_string(), value.clone());
        typing_ctx.wake_comptime(key);
        if let Some(expr_id) = Self::expr_id_from_const_key(key) {
            typing_ctx
                .expr_resolutions
                .borrow_mut()
                .insert_value(expr_id, value.clone());
        }
        Some(value)
    }

    /// Replace any reference to an already-resolved const (matched by bare
    /// name against `resolved`) with its literal value. Used by
    /// `resolve_comptime_now` to make a probed expression self-contained
    /// before lowering, since the probe declares no sibling items for
    /// cross-references to resolve against. Not exhaustive over every
    /// `ExprKind` — only the shapes const initializers actually use.
    fn inline_resolved_names(expr: &mut Expr, resolved: &HashMap<String, Value>) {
        if let ExprKind::Name(name) = expr.kind() {
            if let Some(name) = Self::simple_name_key(name) {
                if let Some(value) = resolved.get(&name) {
                    let ty = expr.ty().cloned();
                    let mut literal = Expr::value(value.clone());
                    if let Some(ty) = ty {
                        literal.set_ty(ty);
                    }
                    *expr = literal;
                    return;
                }
            }
        }
        match expr.kind_mut() {
            ExprKind::Struct(s) => {
                for field in &mut s.fields {
                    if let Some(value) = field.value.as_mut() {
                        Self::inline_resolved_names(value, resolved);
                    }
                }
            }
            ExprKind::Tuple(t) => {
                for value in &mut t.values {
                    Self::inline_resolved_names(value, resolved);
                }
            }
            ExprKind::Array(a) => {
                for value in &mut a.values {
                    Self::inline_resolved_names(value, resolved);
                }
            }
            ExprKind::BinOp(b) => {
                Self::inline_resolved_names(b.lhs.as_mut(), resolved);
                Self::inline_resolved_names(b.rhs.as_mut(), resolved);
            }
            ExprKind::UnOp(u) => Self::inline_resolved_names(u.val.as_mut(), resolved),
            ExprKind::Cast(c) => Self::inline_resolved_names(c.expr.as_mut(), resolved),
            ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    Self::inline_resolved_names(arg, resolved);
                }
            }
            ExprKind::If(if_expr) => {
                Self::inline_resolved_names(if_expr.cond.as_mut(), resolved);
                Self::inline_resolved_names(if_expr.then.as_mut(), resolved);
                if let Some(elze) = if_expr.elze.as_mut() {
                    Self::inline_resolved_names(elze, resolved);
                }
            }
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    match stmt {
                        BlockStmt::Expr(e) => Self::inline_resolved_names(&mut e.expr, resolved),
                        BlockStmt::Let(s) => {
                            if let Some(init) = s.init.as_mut() {
                                Self::inline_resolved_names(init, resolved);
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn simple_name_key(name: &Name) -> Option<String> {
        match name {
            Name::Ident(ident) => Some(ident.as_str().to_string()),
            Name::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string()),
            Name::ParameterPath(_) => None,
        }
    }

    /// Collect nominal struct/enum declarations referenced by `expr`'s type
    /// (and the types of its sub-expressions), synthesizing an
    /// `ItemKind::DefStruct`/`DefEnum` for each so `resolve_comptime_now`'s
    /// isolated probe can predeclare them — `MirLowering`'s struct/enum
    /// registration pass only scans a program's own items, so a struct type
    /// that exists only in the AST's type annotations (never as a sibling
    /// item in the probe) would otherwise be invisible to it. `seen` dedupes
    /// by name across the whole walk. Not exhaustive over every `ExprKind` —
    /// only the shapes const initializers actually use (mirrors
    /// `inline_resolved_names`'s scope).
    fn collect_referenced_struct_enum_items(
        expr: &Expr,
        out: &mut Vec<Item>,
        seen: &mut HashSet<String>,
    ) {
        if let Some(ty) = expr.ty() {
            Self::collect_struct_enum_from_ty(ty, out, seen);
        }
        match expr.kind() {
            ExprKind::Struct(s) => {
                for field in &s.fields {
                    if let Some(value) = field.value.as_ref() {
                        Self::collect_referenced_struct_enum_items(value, out, seen);
                    }
                }
            }
            ExprKind::Tuple(t) => {
                for value in &t.values {
                    Self::collect_referenced_struct_enum_items(value, out, seen);
                }
            }
            ExprKind::Array(a) => {
                for value in &a.values {
                    Self::collect_referenced_struct_enum_items(value, out, seen);
                }
            }
            ExprKind::BinOp(b) => {
                Self::collect_referenced_struct_enum_items(&b.lhs, out, seen);
                Self::collect_referenced_struct_enum_items(&b.rhs, out, seen);
            }
            ExprKind::UnOp(u) => Self::collect_referenced_struct_enum_items(&u.val, out, seen),
            ExprKind::Cast(c) => Self::collect_referenced_struct_enum_items(&c.expr, out, seen),
            ExprKind::Invoke(invoke) => {
                // The receiver of a chained method call (`.with_field(...)`)
                // lives in `target`, not `args` — without walking into it,
                // a receiver type several calls deep (e.g. the `TypeBuilder`
                // in `TypeBuilder::new(...).with_field(...).build()`) is
                // never discovered, so its impl block never makes it into
                // `extra_items` and method-call lowering falls back to an
                // opaque stub that doesn't thread values between calls.
                match &invoke.target {
                    ExprInvokeTarget::Method(select) => {
                        Self::collect_referenced_struct_enum_items(&select.obj, out, seen);
                    }
                    ExprInvokeTarget::Expr(target_expr) => {
                        Self::collect_referenced_struct_enum_items(target_expr, out, seen);
                    }
                    _ => {}
                }
                for arg in &invoke.args {
                    Self::collect_referenced_struct_enum_items(arg, out, seen);
                }
            }
            ExprKind::If(if_expr) => {
                Self::collect_referenced_struct_enum_items(&if_expr.cond, out, seen);
                Self::collect_referenced_struct_enum_items(&if_expr.then, out, seen);
                if let Some(elze) = if_expr.elze.as_ref() {
                    Self::collect_referenced_struct_enum_items(elze, out, seen);
                }
            }
            ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    match stmt {
                        BlockStmt::Expr(e) => {
                            Self::collect_referenced_struct_enum_items(&e.expr, out, seen)
                        }
                        BlockStmt::Let(s) => {
                            if let Some(init) = s.init.as_ref() {
                                Self::collect_referenced_struct_enum_items(init, out, seen);
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_struct_enum_from_ty(ty: &Ty, out: &mut Vec<Item>, seen: &mut HashSet<String>) {
        match ty {
            Ty::Struct(struct_ty) => {
                if seen.insert(struct_ty.name.as_str().to_string()) {
                    for field in &struct_ty.fields {
                        Self::collect_struct_enum_from_ty(&field.value, out, seen);
                    }
                    out.push(Item::new(ItemKind::DefStruct(ItemDefStruct {
                        attrs: Vec::new(),
                        visibility: Visibility::Public,
                        name: struct_ty.name.clone(),
                        value: struct_ty.clone(),
                    })));
                }
            }
            Ty::Enum(enum_ty) => {
                if seen.insert(enum_ty.name.as_str().to_string()) {
                    for variant in &enum_ty.variants {
                        Self::collect_struct_enum_from_ty(&variant.value, out, seen);
                    }
                    out.push(Item::new(ItemKind::DefEnum(ItemDefEnum {
                        attrs: Vec::new(),
                        visibility: Visibility::Public,
                        name: enum_ty.name.clone(),
                        value: enum_ty.clone(),
                    })));
                }
            }
            Ty::Reference(r) => Self::collect_struct_enum_from_ty(&r.ty, out, seen),
            Ty::Slice(s) => Self::collect_struct_enum_from_ty(&s.elem, out, seen),
            Ty::Array(a) => Self::collect_struct_enum_from_ty(&a.elem, out, seen),
            Ty::Vec(v) => Self::collect_struct_enum_from_ty(&v.ty, out, seen),
            Ty::Tuple(t) => {
                for elem in &t.types {
                    Self::collect_struct_enum_from_ty(elem, out, seen);
                }
            }
            _ => {}
        }
    }

    /// Find each referenced struct/enum's inherent `impl` block across every
    /// loaded workspace crate and append it to `out`. Without this, HIR
    /// generation for the probe only ever sees the bare struct/enum shape
    /// (from `collect_referenced_struct_enum_items`), never its methods —
    /// so a chained method call like `TypeBuilder::new(...).with_field(...)`
    /// can't resolve `with_field` to the real function and falls back to a
    /// synthetic "opaque" stub that doesn't thread values between calls.
    ///
    /// Takes a bare `&TypingContext` rather than `&self` — usable from
    /// `resolve_comptime_now`, which only holds `Rc<TypingContext>`, not a
    /// `&CompilerDriver` (see `ComptimeHook`'s doc comment for why).
    fn impl_items_for_types(
        typing_ctx: &TypingContext,
        names: &HashSet<String>,
        out: &mut Vec<Item>,
    ) {
        if names.is_empty() {
            return;
        }
        for krate in typing_ctx.env_ctx.crates().values() {
            for items in krate.borrow().items.values() {
                for item in items {
                    let ItemKind::Impl(impl_block) = item.kind() else {
                        continue;
                    };
                    if impl_block.trait_ty.is_some() {
                        continue;
                    }
                    if let Some(name) = fp_typing::impl_self_ty_name(&impl_block.self_ty) {
                        if names.contains(&name) {
                            out.push(item.clone());
                        }
                    }
                }
            }
        }
    }

    async fn compile_unit_compile_native(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let core = self.compile_unit_core(ast_id, path).await?;
        self.evaluate_comptime_lir(&core.lir_id, path).await?;
        Ok(())
    }

    async fn compile_unit_compile_bytecode(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let core = self.compile_unit_core(ast_id, path).await?;
        self.generate_bytecode(&core.mir_id, path)?;
        Ok(())
    }

    /// Compile-time values are resolved in place through the compiler task
    /// pool; this entry point remains for callers that need an explicit value.
    pub async fn answer_comptime(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<Value, CompilerDriverError> {
        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
        let core = self.compile_unit_core(ast_id, path).await?;
        self.evaluate_comptime_lir(&core.lir_id, path).await?;
        self.state.const_value(&value_id).cloned()
    }

    /// Canonical identity for a generic instantiation, matching the doc's
    /// `FullyQualifiedPath#{...}` convention (docs/Compiler.md, "Compile-Time
    /// Needs And Requests"). Uses `Debug` rather than `Display` for the
    /// concrete types because `Ty`'s `Display` impl depends on a thread-local
    /// serializer that may not be registered, which would make the key
    /// non-deterministic; `Debug` is derived and stable for a given `Ty`
    /// shape. This key participates in `generic_instantiations` dedup, so it
    /// must not change across calls for the same concrete types.
    fn generic_cannon_key(path: &FullyQualifiedPath, concrete_types: &[Ty]) -> String {
        let types_str: Vec<String> = concrete_types
            .iter()
            .map(|ty| format!("{:?}", ty))
            .collect();
        format!("{}#<{}>", path.to_key(), types_str.join(", "))
    }

    /// Finds the `Item` with this exact stable identity (see
    /// `fp_core::ast::ItemId`'s doc comment) within `items`, recursing into
    /// `Module`/`Impl` bodies. Unlike a path-based search, this is a plain
    /// equality check -- it doesn't need `items`' nesting to match any
    /// particular qualification convention, because `ItemId` isn't derived
    /// from one: it's assigned once, at the node's construction, by
    /// `Item::new`/`Item::with_ty`.
    fn find_item_by_id(items: &[Item], target: fp_core::ast::ItemId) -> Option<Item> {
        for item in items {
            if item.id() == target {
                return Some(item.clone());
            }
            let found = match item.kind() {
                ItemKind::Module(module) => Self::find_item_by_id(&module.items, target),
                ItemKind::Impl(impl_block) => Self::find_item_by_id(&impl_block.items, target),
                _ => None,
            };
            if found.is_some() {
                return found;
            }
        }
        None
    }

    /// `param_names`/`concrete_types` are parallel (same index means the
    /// same generic parameter). A user-written `fn f<T>(x: T) -> T`'s
    /// declared parameter/return types are parsed as a bare name reference
    /// (`Ty::Expr(Expr::ident("T"))`, matched below by name) -- `Ty::GenericVar`
    /// is a *different* representation, produced only by this crate's own
    /// let-polymorphism scheme machinery (`build_generalized_ty` in
    /// fp-typing), not by parsing. Both are handled here since either can
    /// appear (a scheme-generalized type may itself reference a
    /// user-written generic parameter's declared type elsewhere).
    fn substitute_in_ty(ty: &mut Ty, param_names: &[String], concrete_types: &[Ty]) {
        match ty {
            Ty::GenericVar(gv) => {
                let idx = gv.index as usize;
                if idx < concrete_types.len() {
                    *ty = concrete_types[idx].clone();
                }
            }
            Ty::Expr(expr) => {
                if let ExprKind::Name(name) = expr.kind() {
                    if let Some(name) = Self::simple_name_key(name) {
                        if let Some(idx) = param_names.iter().position(|p| *p == name) {
                            if let Some(concrete) = concrete_types.get(idx) {
                                *ty = concrete.clone();
                            }
                        }
                    }
                }
            }
            Ty::Function(f) => {
                for param in &mut f.params {
                    Self::substitute_in_ty(param, param_names, concrete_types);
                }
                if let Some(ret_ty) = &mut f.ret_ty {
                    Self::substitute_in_ty(ret_ty, param_names, concrete_types);
                }
            }
            Ty::Reference(r) => {
                Self::substitute_in_ty(&mut *r.ty, param_names, concrete_types);
            }
            Ty::Slice(s) => {
                Self::substitute_in_ty(&mut *s.elem, param_names, concrete_types);
            }
            Ty::Array(a) => {
                Self::substitute_in_ty(&mut *a.elem, param_names, concrete_types);
            }
            Ty::Vec(v) => {
                Self::substitute_in_ty(&mut *v.ty, param_names, concrete_types);
            }
            Ty::Tuple(t) => {
                for ty in &mut t.types {
                    Self::substitute_in_ty(ty, param_names, concrete_types);
                }
            }
            _ => {}
        }
    }

    fn substitute_in_body(expr: &mut Expr, param_names: &[String], concrete_types: &[Ty]) {
        if let Some(ty) = expr.ty_mut() {
            Self::substitute_in_ty(ty, param_names, concrete_types);
        }
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    match stmt {
                        BlockStmt::Expr(e) => {
                            Self::substitute_in_body(&mut e.expr, param_names, concrete_types)
                        }
                        BlockStmt::Let(s) => {
                            if let Some(init) = s.init.as_mut() {
                                Self::substitute_in_body(init, param_names, concrete_types);
                            }
                        }
                        BlockStmt::Defer(d) => {
                            Self::substitute_in_body(d.expr.as_mut(), param_names, concrete_types);
                        }
                        _ => {}
                    }
                }
            }
            ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    Self::substitute_in_body(arg, param_names, concrete_types);
                }
            }
            ExprKind::BinOp(bin) => {
                Self::substitute_in_body(bin.lhs.as_mut(), param_names, concrete_types);
                Self::substitute_in_body(bin.rhs.as_mut(), param_names, concrete_types);
            }
            ExprKind::UnOp(u) => {
                Self::substitute_in_body(u.val.as_mut(), param_names, concrete_types);
            }
            ExprKind::If(if_expr) => {
                Self::substitute_in_body(if_expr.then.as_mut(), param_names, concrete_types);
                if let Some(elze) = if_expr.elze.as_mut() {
                    Self::substitute_in_body(elze, param_names, concrete_types);
                }
            }
            ExprKind::Return(ret) => {
                if let Some(val) = ret.value.as_mut() {
                    Self::substitute_in_body(val, param_names, concrete_types);
                }
            }
            ExprKind::Match(m) => {
                for case in &mut m.cases {
                    Self::substitute_in_body(case.body.as_mut(), param_names, concrete_types);
                }
            }
            ExprKind::Loop(l) => {
                Self::substitute_in_body(l.body.as_mut(), param_names, concrete_types);
            }
            ExprKind::While(w) => {
                Self::substitute_in_body(w.body.as_mut(), param_names, concrete_types);
            }
            ExprKind::For(f) => {
                Self::substitute_in_body(f.body.as_mut(), param_names, concrete_types);
            }
            ExprKind::Assign(a) => {
                Self::substitute_in_body(a.value.as_mut(), param_names, concrete_types);
            }
            ExprKind::Cast(c) => {
                Self::substitute_in_body(c.expr.as_mut(), param_names, concrete_types);
            }
            ExprKind::Struct(s) => {
                for field in &mut s.fields {
                    if let Some(val) = field.value.as_mut() {
                        Self::substitute_in_body(val, param_names, concrete_types);
                    }
                }
            }
            ExprKind::Tuple(t) => {
                for v in &mut t.values {
                    Self::substitute_in_body(v, param_names, concrete_types);
                }
            }
            ExprKind::Array(a) => {
                for v in &mut a.values {
                    Self::substitute_in_body(v, param_names, concrete_types);
                }
            }
            ExprKind::With(w) => {
                Self::substitute_in_body(w.context.as_mut(), param_names, concrete_types);
                Self::substitute_in_body(w.body.as_mut(), param_names, concrete_types);
            }
            ExprKind::Let(l) => {
                Self::substitute_in_body(l.expr.as_mut(), param_names, concrete_types);
            }
            ExprKind::ConstBlock(cb) => {
                Self::substitute_in_body(cb.expr.as_mut(), param_names, concrete_types);
            }
            ExprKind::SplicePending(_) | ExprKind::Splice(_) => {
                // Splice resolution is not yet wired into the compiler task
                // pool; these nodes are left as-is for a future staging step.
            }
            _ => {}
        }
    }

    fn substitute_in_block(
        block: &mut fp_core::ast::ExprBlock,
        param_names: &[String],
        concrete_types: &[Ty],
    ) {
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr) => {
                    Self::substitute_in_body(expr.expr.as_mut(), param_names, concrete_types)
                }
                BlockStmt::Let(stmt) => {
                    if let Some(init) = stmt.init.as_mut() {
                        Self::substitute_in_body(init, param_names, concrete_types);
                    }
                    if let Some(diverge) = stmt.diverge.as_mut() {
                        Self::substitute_in_body(diverge, param_names, concrete_types);
                    }
                }
                BlockStmt::Defer(stmt) => {
                    Self::substitute_in_body(stmt.expr.as_mut(), param_names, concrete_types)
                }
                BlockStmt::Item(_) | BlockStmt::Noop | BlockStmt::Any(_) => {}
            }
        }
    }

    fn generate_bytecode(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let program = fp_bytecode::lower_program(&mir)?;
        let bytecode_id = BytecodeId::new(format!("bytecode:{}", path.to_key()));
        self.state.insert_bytecode(bytecode_id, program);
        Ok(())
    }

    /// Compile all items in a workspace crate through the full pipeline
    /// (typer → HIR → MIR → LIR) and return the merged LirProgram.
    /// Used for on-demand compilation when a crate's lir_program is None.
    async fn compile_items_to_lir_units(
        &mut self,
        items_map: &HashMap<QualifiedPath, Vec<Item>>,
        strict: bool,
    ) -> Result<Vec<fp_core::lir::LirCompileUnit>, CompilerDriverError> {
        let mut units = Vec::new();
        let mut pending: Vec<_> = items_map.keys().cloned().collect();
        pending.sort_by_key(|path| path.to_key());
        while !pending.is_empty() {
            let mut next_pending = Vec::new();
            let mut first_error = None;
            let mut progressed = false;
            for path in pending {
                let items = &items_map[&path];
                if items.is_empty() {
                    continue;
                }

                let ast_id = AstId::new(format!("on_demand:{}", path.to_key()));
                // Stored purely so `compile_unit_core`'s thin `File`-based
                // wrapper can find it if anything else looks this AST id up
                // later — not something any generator sees directly.
                self.state.insert_ast(
                    ast_id.clone(),
                    File {
                        path: std::path::PathBuf::from(path.to_key()),
                        items: items.clone(),
                        collected_items: Vec::new(),
                        attrs: Vec::new(),
                    },
                );

                let fqp = FullyQualifiedPath::new(path.clone());
                let core = match self
                    .compile_module_core(path.clone(), items.clone(), &ast_id, &fqp)
                    .await
                {
                    Ok(core) => {
                        progressed = true;
                        core
                    }
                    Err(error) if !strict => {
                        eprintln!(
                            "WARNING: compile_module_core failed for {}: {error}",
                            path.to_key()
                        );
                        continue;
                    }
                    Err(error) => {
                        first_error.get_or_insert((path.to_key(), error));
                        next_pending.push(path);
                        continue;
                    }
                };
                if let Some(package_id) = self.state.typing_ctx.env_ctx.current_package().cloned() {
                    if let Some(package) =
                        self.state.typing_ctx.env_ctx.compiled_package(&package_id)
                    {
                        let hir = self.state.hir(&core.hir_id)?.clone();
                        package.borrow_mut().hir_modules.insert(path.clone(), hir);
                    }
                }
                let lir = self.state.lir(&core.lir_id)?.clone();
                units.push(fp_core::lir::LirCompileUnit {
                    package_id: self
                        .state
                        .typing_ctx
                        .env_ctx
                        .package_id_for_module(&path)
                        .unwrap_or_default(),
                    module_path: path.clone(),
                    program: lir,
                });
            }
            if !strict || next_pending.is_empty() {
                break;
            }
            if !progressed {
                let (path, error) = first_error.expect("failed module compilation had an error");
                return Err(CompilerDriverError::UnsupportedWork(format!(
                    "module {path}: {error}"
                )));
            }
            pending = next_pending;
        }
        Ok(units)
    }

    async fn evaluate_comptime_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
    ) -> Result<usize, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        // Only `comptime_entries` is needed after `lir` itself is moved into
        // `all_units` below — clone just that (much smaller than the whole
        // program) rather than cloning the entire `LirProgram` a second time.
        let comptime_entries = lir.comptime_entries.clone();

        // Collect all LirCompileUnits: user's module + workspace crates
        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .package_id_for_module(path.path())
            .unwrap_or_default();
        let mut all_units: Vec<fp_core::lir::LirCompileUnit> = Vec::new();
        all_units.push(fp_core::lir::LirCompileUnit {
            package_id,
            module_path: path.path().clone(),
            program: lir,
        });
        // Workspace crates with pre-compiled LIR
        for krate in self.state.typing_ctx.env_ctx.crates().values() {
            for unit in &krate.borrow().lir_units {
                all_units.push(unit.clone());
            }
        }

        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));

        if comptime_entries.is_empty() {
            self.state
                .insert_const_value(value_id.clone(), Value::unit());
            return Ok(0);
        }

        let mut count = 0usize;
        let mut last = Value::unit();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter.inject_globals(&resolved);
        let mut workspace_lir_loaded = false;
        for entry in &comptime_entries {
            let mut result = self.interpreter.run_function_named(
                &all_units,
                package_id,
                entry.function.as_str(),
            );
            if result.is_err() && !workspace_lir_loaded {
                // Most comptime entries are self-contained. Materialize
                // provider packages only when execution reaches an external
                // function, avoiding an eager pass over every std module.
                let crates_to_compile: Vec<_> = self
                    .state
                    .typing_ctx
                    .env_ctx
                    .crates()
                    .values()
                    .map(|crate_state| crate_state.borrow())
                    .filter(|crate_state| {
                        crate_state.lir_units.is_empty() && !crate_state.items.is_empty()
                    })
                    .map(|crate_state| crate_state.items.clone())
                    .collect();
                for items_map in &crates_to_compile {
                    match self.compile_items_to_lir_units(items_map, false).await {
                        Ok(compiled_units) => all_units.extend(compiled_units),
                        Err(error) => return Err(error),
                    }
                }
                workspace_lir_loaded = true;
                result = self.interpreter.run_function_named(
                    &all_units,
                    package_id,
                    entry.function.as_str(),
                );
            }
            let mut value = match result {
                Ok(value) => value,
                Err(_) => continue,
            };
            // If the returned value is a raw handle (u64 from comptime
            // struct construction), resolve it from the interpreter's objects.
            if let Some(resolved) = Self::resolve_comptime_value(&mut self.interpreter, &value) {
                value = resolved;
            }
            // Store struct types from ALL entries, not just the last one.
            // Each const-block type alias produces its own struct.
            let entry_struct = Self::extract_struct_type(&value);
            if let Some(ref struct_ty) = entry_struct {
                let name = struct_ty.name.as_str().to_string();
                self.state
                    .typing_ctx
                    .resolved_types
                    .borrow_mut()
                    .insert(name.clone(), struct_ty.clone());
                self.state.typing_ctx.wake_comptime(&name);
            }
            let constant = self
                .value_to_mir_constant(&value, &entry.ty)
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "unsupported comptime result for {}",
                        entry.key
                    ))
                })?;
            self.state
                .insert_resolved_const_value(entry.key.clone(), constant);
            self.state
                .insert_typing_const(entry.key.clone(), value.clone());
            if let Some(expr_id) = Self::expr_id_from_const_key(&entry.key) {
                self.state
                    .insert_expr_resolution_value(expr_id, value.clone());
            }
            last = value;
            count += 1;
        }

        // Store the final value for const evaluation purposes

        self.state.insert_const_value(value_id.clone(), last);
        Ok(count)
    }

    fn extract_struct_type(value: &Value) -> Option<TypeStruct> {
        match value {
            Value::Type(Ty::Struct(s)) => Some(s.clone()),
            Value::Type(Ty::Type(TypeType {
                inner: Some(inner), ..
            })) => match inner.as_ref() {
                Ty::Struct(s) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Resolve a raw handle (u64) from the interpreter's objects table.
    /// During comptime evaluation, struct construction produces handles that
    /// need to be looked up to get the actual struct type.
    fn resolve_comptime_value(interp: &mut LirInterpreter, value: &Value) -> Option<Value> {
        let handle = match value {
            Value::Int(v) => v.value as u64,
            Value::UInt(v) => v.value,
            _ => return None,
        };
        interp.resolve_object(handle)
    }

    fn lower_to_mir(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<MirId, CompilerDriverError> {
        self.lower_to_mir_lossy(hir_id, path, false)
    }

    /// Lower HIR to MIR, optionally tolerating diagnostic errors.
    /// Used for comptime LIR generation where unresolved types are
    /// expected to resolve after evaluation.
    fn lower_to_mir_lossy(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
        allow_errors: bool,
    ) -> Result<MirId, CompilerDriverError> {
        let hir = self.state.hir(hir_id)?.clone();
        let typeck_results = self.state.hir_typeck(hir_id)?.clone();
        let mut lowering = MirLowering::new().with_typeck_results(&typeck_results)?;
        lowering.set_lossy(self.state.lossy() || allow_errors);
        for (key, value) in self.state.resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let mir = lowering.transform(hir);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
        let mir = if allow_errors {
            match mir {
                Ok(program) => program,
                Err(err) => return Err(err.into()),
            }
        } else {
            match (mir, had_errors, self.state.lossy()) {
                (Ok(program), false, _) => program,
                (Ok(_), true, true) => fp_core::mir::Program::new(),
                (Err(_), _, true) => fp_core::mir::Program::new(),
                (Ok(_), true, false) => {
                    let message = diagnostics
                        .iter()
                        .find(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
                        .map(|diagnostic| diagnostic.message.clone())
                        .unwrap_or_else(|| "HIR→MIR lowering reported errors".to_string());
                    return Err(CompilerDriverError::UnsupportedWork(message));
                }
                (Err(err), _, false) => return Err(err.into()),
            }
        };
        let mir_id = MirId::new(format!("mir:{}", path.to_key()));
        self.state.insert_mir(mir_id.clone(), mir);
        Ok(mir_id)
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
    ) -> Result<LirId, CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .package_id_for_module(path.path())
            .unwrap_or_default();
        let mut lowering = LirGenerator::new(self.state.typing_ctx.data_layout.clone())
            .with_package_id(package_id);
        let lir = lowering.transform(mir)?;
        let lir_id = LirId::new(format!("lir:{}", path.to_key()));
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(lir_id)
    }

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        Self::resolved_const_values_snapshot(&self.state.typing_ctx)
    }

    /// Same as `collect_resolved_const_values`, but against a bare
    /// `Rc<TypingContext>` — usable from `resolve_comptime_now`, which no
    /// longer holds a `&CompilerDriver`.
    fn resolved_const_values_snapshot(typing_ctx: &TypingContext) -> HashMap<String, Value> {
        typing_ctx.resolved_consts.borrow().clone()
    }

    fn value_to_mir_constant(&self, value: &Value, ty: &mir::Ty) -> Option<mir::Constant> {
        let literal = match value {
            Value::Bool(value) => mir::ConstantKind::Bool(value.value),
            Value::Int(value) => mir::ConstantKind::Int(value.value),
            Value::UInt(value) => mir::ConstantKind::UInt(value.value),
            Value::Decimal(value) => mir::ConstantKind::Float(value.value),
            Value::String(value) => mir::ConstantKind::Str(value.value.clone()),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                mir::ConstantKind::Str(s)
            }
            Value::Null(_) => mir::ConstantKind::Null,
            _ => mir::ConstantKind::Val(self.value_to_const_value(value, ty)?),
        };
        Some(mir::Constant {
            span: Span::null(),
            ty: ty.clone(),
            user_ty: None,
            literal,
        })
    }

    fn value_to_const_value(&self, value: &Value, ty: &mir::Ty) -> Option<mir::ConstValue> {
        match value {
            Value::Unit(_) => Some(mir::ConstValue::Unit),
            Value::Bool(value) => Some(mir::ConstValue::Bool(value.value)),
            Value::Int(value) => Some(match ty.kind {
                TyKind::Uint(UintTy::Usize)
                | TyKind::Uint(UintTy::U8)
                | TyKind::Uint(UintTy::U16)
                | TyKind::Uint(UintTy::U32)
                | TyKind::Uint(UintTy::U64)
                | TyKind::Uint(UintTy::U128) => mir::ConstValue::UInt(value.value as u64),
                _ => mir::ConstValue::Int(value.value),
            }),
            Value::UInt(value) => Some(match ty.kind {
                TyKind::Int(IntTy::Isize)
                | TyKind::Int(IntTy::I8)
                | TyKind::Int(IntTy::I16)
                | TyKind::Int(IntTy::I32)
                | TyKind::Int(IntTy::I64)
                | TyKind::Int(IntTy::I128) => mir::ConstValue::Int(value.value as i64),
                _ => mir::ConstValue::UInt(value.value),
            }),
            Value::Decimal(value) => Some(match ty.kind {
                TyKind::Float(FloatTy::F32) | TyKind::Float(FloatTy::F64) => {
                    mir::ConstValue::Float(value.value)
                }
                _ => return None,
            }),
            Value::String(value) => Some(mir::ConstValue::Str(value.value.clone())),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                Some(mir::ConstValue::Str(s))
            }
            Value::Null(_) => Some(mir::ConstValue::Null),
            Value::Tuple(tuple) => {
                let TyKind::Tuple(fields) = &ty.kind else {
                    return None;
                };
                if tuple.values.len() != fields.len() {
                    return None;
                }
                let values = tuple
                    .values
                    .iter()
                    .zip(fields.iter())
                    .map(|(value, field_ty)| self.value_to_const_value(value, field_ty))
                    .collect::<Option<Vec<_>>>()?;
                Some(mir::ConstValue::Tuple(values))
            }
            Value::List(list) => match &ty.kind {
                TyKind::Array(elem_ty, _) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Array(values))
                }
                TyKind::Slice(elem_ty) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::List {
                        elements: values,
                        elem_ty: elem_ty.as_ref().clone(),
                    })
                }
                _ => None,
            },
            Value::Struct(value_struct) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if value_struct.structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = adt_def.variants.first()?;
                    if value_struct.structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .map(|field| self.value_to_untyped_const_value(&field.value))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => return None,
            },
            Value::Structural(structural) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = adt_def.variants.first()?;
                    if structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .map(|field| self.value_to_untyped_const_value(&field.value))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn value_to_untyped_const_value(&self, value: &Value) -> Option<mir::ConstValue> {
        match value {
            Value::Unit(_) => Some(mir::ConstValue::Unit),
            Value::Bool(value) => Some(mir::ConstValue::Bool(value.value)),
            Value::Int(value) => Some(mir::ConstValue::Int(value.value)),
            Value::UInt(value) => Some(mir::ConstValue::UInt(value.value)),
            Value::Decimal(value) => Some(mir::ConstValue::Float(value.value)),
            Value::String(value) => Some(mir::ConstValue::Str(value.value.clone())),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                Some(mir::ConstValue::Str(s))
            }
            Value::Null(_) => Some(mir::ConstValue::Null),
            Value::Tuple(tuple) => Some(mir::ConstValue::Tuple(
                tuple
                    .values
                    .iter()
                    .map(|value| self.value_to_untyped_const_value(value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::List(list) => Some(mir::ConstValue::Array(
                list.values
                    .iter()
                    .map(|value| self.value_to_untyped_const_value(value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::Struct(value_struct) => Some(mir::ConstValue::Struct(
                value_struct
                    .structural
                    .fields
                    .iter()
                    .map(|field| self.value_to_untyped_const_value(&field.value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Value::Structural(structural) => Some(mir::ConstValue::Struct(
                structural
                    .fields
                    .iter()
                    .map(|field| self.value_to_untyped_const_value(&field.value))
                    .collect::<Option<Vec<_>>>()?,
            )),
            _ => None,
        }
    }

    fn expr_id_from_const_key(key: &str) -> Option<u64> {
        let name = key.rsplit(':').next()?;
        let suffix = name.strip_prefix("__fp_expr_")?;
        suffix.parse().ok()
    }
}

#[cfg(test)]
mod comptime_source_tests {
    use super::*;
    use crate::{AstId, CompilerExecutor, FullyQualifiedPath};
    use fp_core::frontend::LanguageFrontend;

    fn path() -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(vec!["test".to_string(), "main".to_string()])
    }

    fn test_data_layout() -> fp_core::lir::LirDataLayout {
        fp_core::lir::LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid test data layout")
    }

    #[test]
    fn parses_fp_source_and_runs_const_item_through_driver() {
        let source = r#"
const ANSWER: i64 = 42;

fn main() {
    let result = const { ANSWER * 2 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("test.fp"))
            .expect("parse .fp source");
        let ast_node = result.ast;

        let executor = CompilerExecutor::new();
        let mut driver = CompilerDriver::new(test_data_layout(), executor.handle());
        let ast_id = AstId::new("ast:test::main");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver
            .compile_native_sync(&ast_id, &path())
            .expect("compile unit");
    }

    #[test]
    fn parses_simple_const_and_evaluates_through_full_pipeline() {
        let source = "const ANSWER: i64 = 42;\n";
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("const.fp"))
            .expect("parse const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let ast_id = AstId::new("ast:test::const");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver
            .compile_native_sync(&ast_id, &path())
            .expect("compile unit");

        assert_eq!(
            driver.state.const_value_len(),
            1,
            "const should produce one compile-time value"
        );
    }

    #[test]
    fn multiple_const_items_produce_separate_comptime_needs() {
        let source = r#"
const WIDTH: i64 = 640;
const HEIGHT: i64 = 480;
const AREA: i64 = WIDTH * HEIGHT;
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("multi.fp"))
            .expect("parse multi-const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let ast_id = AstId::new("ast:test::multi");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver
            .compile_native_sync(&ast_id, &path())
            .expect("compile unit");
        assert_eq!(driver.state.const_value_len(), 1);
    }

    #[test]
    fn const_block_in_function_triggers_comptime_path() {
        let source = r#"
fn calculate() {
    let size = const { 1024 * 8 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("block.fp"))
            .expect("parse const-block source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let ast_id = AstId::new("ast:test::block");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver
            .compile_native_sync(&ast_id, &path())
            .expect("compile unit");
    }

    enum ExampleResult {
        Completed { lowered: usize, executed: usize },
    }

    /// Regression test for the generic-monomorphization identity fix: a
    /// generic function that's actually *called* (not just declared --
    /// `compile_unit_native_submits_enqueue_for_generic` above never calls
    /// `id`, so it never exercised `enqueue_generic`'s function lookup at
    /// all) must specialize successfully end to end through the real
    /// driver, rather than failing with "generic function not found" (the
    /// old path-based lookup treated the compile unit's own synthetic
    /// identity prefix as if it were real `mod` nesting in the file).
    #[test]
    fn called_generic_function_compiles_end_to_end() {
        let source = r#"
fn identity<T>(a: T) -> T {
    a
}

fn main() {
    let r = identity(10);
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("generic_call.fp"))
            .expect("parse generic-call source");
        let ast_node = result.ast;

        let mut workspace = fp_core::workspace::WorkspaceContext::new();
        workspace.register_provider(std::sync::Arc::new(fp_lang::provider::FerroPhaseProvider));
        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        driver.state.typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(
            test_data_layout(),
            std::rc::Rc::new(workspace),
        ));
        let ast_id = AstId::new("ast:test::generic_call");
        driver.state.insert_ast(ast_id.clone(), ast_node);
        let path =
            FullyQualifiedPath::from_segments(vec!["test".to_string(), "generic_call".to_string()]);
        driver
            .compile_native_sync(&ast_id, &path)
            .expect("driver should not error");

        assert!(
            driver
                .state
                .hir(&HirId::new("hir:test::generic_call"))
                .is_ok()
        );
        assert!(
            driver
                .state
                .mir(&MirId::new("mir:test::generic_call"))
                .is_ok()
        );
    }

    /// Regression test for `CompiledPackage::method_sigs`: inherent methods
    /// (`impl SelfType { .. }`) now resolve through one shared, name-keyed
    /// registry regardless of whether `SelfType` is a struct or an enum --
    /// previously `TypeEnum` had no method storage at all (`TypeStruct` did),
    /// so an enum's own `impl` block was silently ignored and specific
    /// methods (`is_some`/`is_none`/`is_ok`/`is_err`/`unwrap` on
    /// `Option`/`Result`) were hardcoded by literal type name in the typer
    /// instead. This exercises a local enum *and* a local struct calling
    /// their own inherent methods, proving both now go through the exact
    /// same dispatch path (`lookup_struct_method`).
    ///
    /// Doesn't use `std::option::Option`/`std::result::Result` directly:
    /// confirmed (via direct debug instrumentation on `lookup_struct_method`)
    /// that `is_some`/`is_none`/`is_ok`/`is_err` already resolve correctly
    /// against `std`'s real `impl<T> Option<T>`/`impl<T,E> Result<T,E>`
    /// blocks through this same mechanism -- but merely *referencing*
    /// `Option::Some(..)`/`Result::Ok(..)` at all (even with no method call)
    /// separately triggers a pre-existing, unrelated failure somewhere in
    /// the comptime-probing pipeline when `std::option`/`std::result` get
    /// pulled in for on-demand LIR compilation, which is out of scope for
    /// this fix (method dispatch, not comptime probing).
    #[test]
    fn struct_and_enum_inherent_methods_share_one_method_registry() {
        let source = r#"
enum Status {
    Ready,
    Done,
}

impl Status {
    fn is_ready(&self) -> bool {
        true
    }
}

struct Counter {
    value: i64,
}

impl Counter {
    fn get(&self) -> i64 {
        self.value
    }
}

fn main() {
    let s = Status::Ready;
    let ready = s.is_ready();
    let c = Counter { value: 5 };
    let v = c.get();
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("struct_enum_methods.fp"))
            .expect("parse struct/enum-methods source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let ast_id = AstId::new("ast:test::struct_enum_methods");
        driver.state.insert_ast(ast_id.clone(), ast_node);
        let path = FullyQualifiedPath::from_segments(vec![
            "test".to_string(),
            "struct_enum_methods".to_string(),
        ]);
        driver
            .compile_native_sync(&ast_id, &path)
            .expect("driver should not error");
    }

    fn compile_example_file(
        name: &str,
        workspace: std::rc::Rc<fp_core::workspace::WorkspaceContext>,
    ) -> Result<ExampleResult, String> {
        let abs = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../examples")
            .join(name);
        let source = std::fs::read_to_string(&abs).map_err(|e| format!("read: {e}"))?;

        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(&source, &abs)
            .map_err(|e| format!("parse: {e}"))?;
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        driver.state.typing_ctx =
            std::rc::Rc::new(fp_typing::TypingContext::new(test_data_layout(), workspace));
        let label = name.trim_end_matches(".fp");
        let ast_id = AstId::new(format!("ast:example::{label}"));
        driver.state.insert_ast(ast_id.clone(), ast_node);

        let path = FullyQualifiedPath::from_segments(vec!["example".into(), label.to_string()]);
        driver
            .compile_native_sync(&ast_id, &path)
            .map_err(|e| format!("compile: {e}"))?;

        Ok(ExampleResult::Completed {
            lowered: 1,
            executed: driver.state.const_value_len(),
        })
    }

    #[test]
    fn run_all_example_files() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .name("example-runner".into())
            .spawn(run_all_example_files_impl)
            .unwrap()
            .join()
            .unwrap();
    }

    fn run_all_example_files_impl() {
        let examples_dir =
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples");
        let mut entries: Vec<_> = std::fs::read_dir(&examples_dir)
            .expect("read examples dir")
            .filter_map(|e| match e {
                Ok(entry) => Some(entry),
                Err(err) => {
                    eprintln!("[fp-compiler] error reading examples dir entry: {err}");
                    None
                }
            })
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "fp"))
            .map(|e| e.file_name().to_string_lossy().to_string())
            .collect();
        entries.sort();

        let mut workspace = fp_core::workspace::WorkspaceContext::new();
        workspace.register_provider(std::sync::Arc::new(fp_lang::provider::FerroPhaseProvider));
        let workspace = std::rc::Rc::new(workspace);

        let mut completed = 0;
        let mut errors = 0;

        for name in &entries {
            print!("  {name:.<50} ");
            match compile_example_file(name, workspace.clone()) {
                Ok(ExampleResult::Completed { lowered, executed }) => {
                    completed += 1;
                    println!("OK  (lowered={lowered}, executed={executed})");
                }
                Err(e) => {
                    errors += 1;
                    println!("ERROR: {e}");
                }
            }
        }

        println!(
            "\n  Examples: {completed} completed, {errors} errors ({} total)",
            entries.len()
        );
    }

    /// `TypeBuilder::new(...).with_field(...)` — a chained instance method
    /// call whose receiver's struct is only discoverable via `.ty()`
    /// annotations several calls deep. Before this fix,
    /// `collect_referenced_struct_enum_items` only walked `invoke.args`, so
    /// a receiver buried in `invoke.target` (an `ExprInvokeTarget::Method`)
    /// was never found — the referenced struct's impl block never made it
    /// into the comptime probe, and method-call lowering fell back to a
    /// synthetic stub with no real body.
    #[test]
    fn collect_referenced_struct_enum_items_walks_method_call_receiver() {
        use fp_core::ast::{ExprSelect, ExprSelectType, Ident, ReprOptions, TypeStruct};

        let mut receiver = Expr::ident(Ident::new("builder"));
        receiver.set_ty(Ty::Struct(TypeStruct {
            name: Ident::new("Foo"),
            generics_params: Vec::new(),
            repr: ReprOptions::default(),
            fields: Vec::new(),
        }));

        let call = Expr::from(fp_core::ast::ExprKind::Invoke(fp_core::ast::ExprInvoke {
            span: Span::default(),
            target: ExprInvokeTarget::Method(ExprSelect {
                span: Span::default(),
                obj: Box::new(receiver),
                field: Ident::new("with_field"),
                select: ExprSelectType::Method,
            }),
            args: Vec::new(),
            kwargs: Vec::new(),
        }));

        let mut extra_items = Vec::new();
        let mut seen = HashSet::new();
        CompilerDriver::collect_referenced_struct_enum_items(&call, &mut extra_items, &mut seen);

        assert!(
            seen.contains("Foo"),
            "expected the method call's receiver type to be discovered, got {seen:?}"
        );
        assert!(
            extra_items.iter().any(
                |item| matches!(item.kind(), ItemKind::DefStruct(d) if d.name.as_str() == "Foo")
            ),
            "expected a DefStruct item for the discovered receiver type"
        );
    }

    /// Once the receiver type is discovered, `impl_items_for_types` must
    /// pull in its *inherent* impl block from wherever it's actually defined
    /// (a loaded workspace crate) — that's what lets the real `with_field`
    /// method resolve instead of falling back to an opaque stub with no
    /// body.
    #[test]
    fn collect_impl_items_for_types_finds_inherent_impl_in_loaded_crate() {
        use fp_core::ast::{Ident, ItemImpl};

        let driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let krate = driver.state.typing_ctx.env_ctx.begin_package(
            fp_core::package::PackageId::new("somepkg"),
            fp_core::package::PackageSource::new(
                fp_core::package::PackageId::new("somepkg"),
                "somepkg",
                fp_core::package::graph::PackageGraph::new(vec![]),
            ),
        );
        let impl_item = Item::new(ItemKind::Impl(ItemImpl {
            attrs: Vec::new(),
            is_negative: false,
            trait_ty: None,
            self_ty: Expr::ident(Ident::new("Foo")),
            generics_params: Vec::new(),
            collected_items: Vec::new(),
            items: Vec::new(),
        }));
        krate.borrow_mut().items.insert(
            QualifiedPath::new(vec!["somepkg".to_string()]),
            vec![impl_item],
        );

        let mut names = HashSet::new();
        names.insert("Foo".to_string());
        let mut extra_items = Vec::new();
        CompilerDriver::impl_items_for_types(&driver.state.typing_ctx, &names, &mut extra_items);

        assert_eq!(
            extra_items.len(),
            1,
            "expected exactly the one matching impl block"
        );
        assert!(matches!(extra_items[0].kind(), ItemKind::Impl(_)));
    }

    #[test]
    fn duplicate_package_requests_load_std_exactly_once() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicUsize, Ordering};

        struct CountingStdProvider {
            calls: Arc<AtomicUsize>,
        }

        impl fp_core::package::provider::PackageProvider for CountingStdProvider {
            fn list_packages(
                &self,
            ) -> fp_core::package::provider::ProviderResult<Vec<fp_core::package::PackageId>>
            {
                Ok(vec![fp_core::package::PackageId::new("std")])
            }

            fn load_package_metadata(
                &self,
                id: &fp_core::package::PackageId,
            ) -> fp_core::package::provider::ProviderResult<Arc<fp_core::package::PackageDescriptor>>
            {
                Ok(Arc::new(fp_core::package::PackageDescriptor {
                    id: id.clone(),
                    name: id.0.clone(),
                    version: None,
                    manifest_path: fp_core::vfs::VirtualPath::from_path(std::path::Path::new(
                        "std/fp.toml",
                    )),
                    root: fp_core::vfs::VirtualPath::from_path(std::path::Path::new("std")),
                    metadata: Default::default(),
                    modules: Vec::new(),
                }))
            }

            fn refresh(&self) -> fp_core::package::provider::ProviderResult<()> {
                Ok(())
            }

            fn load_package_source(
                &self,
                id: &fp_core::package::PackageId,
            ) -> fp_core::package::provider::ProviderResult<fp_core::package::PackageSource>
            {
                self.calls.fetch_add(1, Ordering::SeqCst);
                Ok(fp_core::package::PackageSource::new(
                    id.clone(),
                    id.0.clone(),
                    fp_core::package::graph::PackageGraph::new(vec![]),
                ))
            }
        }

        let calls = Arc::new(AtomicUsize::new(0));
        let mut workspace = fp_core::workspace::WorkspaceContext::new();
        workspace.register_provider(Arc::new(CountingStdProvider {
            calls: calls.clone(),
        }));
        let executor = CompilerExecutor::new();
        let mut driver = CompilerDriver::new(test_data_layout(), executor.handle());
        driver.state.typing_ctx = std::rc::Rc::new(fp_typing::TypingContext::new(
            test_data_layout(),
            std::rc::Rc::new(workspace),
        ));

        let std = fp_core::package::PackageId::new("std");
        let first = executor
            .run(driver.compile_package(&std))
            .expect("first load");
        let second = executor
            .run(driver.compile_package(&std))
            .expect("second load (already loaded)");

        assert_eq!(first.borrow().name, "std");
        assert!(std::rc::Rc::ptr_eq(&first, &second));
        assert_eq!(
            calls.load(Ordering::SeqCst),
            1,
            "provider should only be invoked once across both requests"
        );
        assert_eq!(
            driver.state.typing_ctx.env_ctx.crates().len(),
            1,
            "workspace should hold exactly one std crate, not one per request"
        );
    }

    fn compile_inline_source(source: &str, expected_const_values: usize) {
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("inline.fp"))
            .unwrap_or_else(|e| panic!("parse inline: {e}"));
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new(test_data_layout(), CompilerExecutor::new().handle());
        let ast_id = AstId::new("ast:example::inline");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        let path = FullyQualifiedPath::from_segments(vec!["example".into(), "inline".into()]);
        driver
            .compile_native_sync(&ast_id, &path)
            .expect("compile unit");

        assert_eq!(
            driver.state.const_value_len(),
            expected_const_values,
            "expected {expected_const_values} const values, got {}",
            driver.state.const_value_len()
        );
    }

    #[test]
    fn comptime_const_with_arithmetic() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 1024 * 4;
const MAX_CONNECTIONS: i64 = 150;
const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
const IS_LARGE: bool = BUFFER_SIZE > 2048;
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_with_struct_defaults() {
        compile_inline_source(
            r#"
struct Config {
    buffer_size: i64,
    max_connections: i64,
}

const BUFFER_SIZE: i64 = 4096;
const MAX_CONNECTIONS: i64 = 150;
const DEFAULT_CONFIG: Config = Config {
    buffer_size: BUFFER_SIZE,
    max_connections: MAX_CONNECTIONS,
};
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_block_with_conditional() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 4096;
const OPTIMIZED_SIZE: i64 = const { BUFFER_SIZE * 2 };
const CACHE_STRATEGY: &str = const {
    if BUFFER_SIZE > 2048 {
        "large"
    } else {
        "small"
    }
};
"#,
            1,
        );
    }
}
