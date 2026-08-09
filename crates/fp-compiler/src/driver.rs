use fp_backend::transformations::{HirGenerator, HirLoweringConfig, LirGenerator, MirLowering};
use fp_core::ast::{
    BlockStmt, Expr, ExprKind, Item, ItemKind, Name, Ty, TypeStruct, TypeType, Value,
};
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::module::path::QualifiedPath;
use fp_core::package::PackageId;
use fp_core::span::Span;
use fp_interpret::LirInterpreter;
use fp_lang::FerroIntrinsicNormalizer;
use fp_typing::{ComptimeRequest, HirTypeChecker, TypingContext};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;

use crate::{
    CompilerDriverError, CompilerState, ConstValueId, ExecutorHandle, FullyQualifiedPath, HirId,
    LirId, MirId, RuntimeValueId,
};

pub struct CompilerDriver {
    pub state: CompilerState,
    interpreter: LirInterpreter,
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::package::CompiledPackage>>>,
    next_hir_def_id: u32,
    pub pipeline: PipelineMode,
}

/// Controls how far the compiler pipeline runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineMode {
    /// Full native compilation: AST → HIR → MIR → LIR
    Full,
    /// Stop after parsing: resolve modules, parse sources, return AST items
    Parse,
}

impl CompilerDriver {
    fn module_state_key(&self, path: &QualifiedPath) -> String {
        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .map(|package_id| package_id.as_str().to_string());
        match package_id {
            Some(package_id) => format!("{package_id}:{}", path.to_key()),
            None => path.to_key(),
        }
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
            pipeline: PipelineMode::Full,
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            state,
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
            pipeline: PipelineMode::Full,
        }
    }

    pub async fn compile_native(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        self.compile_package(package_id).await
    }

    pub async fn compile_bytecode(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        let package = self.compile_package(package_id).await?;
        let mir = package.borrow().mir_program.clone().ok_or_else(|| {
            CompilerDriverError::InternalCompilerError(format!(
                "package {package_id} has no MIR program"
            ))
        })?;
        fp_bytecode::lower_program(&mir).map_err(CompilerDriverError::from)
    }

    pub fn compile_native_sync(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_native(package_id))
    }

    pub fn compile_bytecode_sync(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        let executor = self.state.tasks.clone();
        executor.run(self.compile_bytecode(package_id))
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
        let package = parent_context
            .env_ctx
            .compiled_package(&package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        package_workspace.import_package(package_id, package);
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
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let entrypoint = self.state.runtime_entrypoint(lir_id)?;
        let value = self.interpreter.run_entrypoint(&lir, entrypoint)?;
        let value_id = RuntimeValueId::new(format!("runtime_value:{}", lir_id.as_str()));
        self.state.insert_runtime_value(value_id, value.clone());
        Ok(value)
    }

    pub fn select_entrypoint(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<LirId, CompilerDriverError> {
        let package = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let function = package
            .borrow()
            .hir_program
            .as_ref()
            .and_then(|program| {
                program.items.iter().find_map(|item| match &item.kind {
                    hir::ItemKind::Function(function)
                        if function.sig.name.as_str() == function_name =>
                    {
                        Some(item.def_id)
                    }
                    _ => None,
                })
            })
            .ok_or_else(|| {
                CompilerDriverError::Interpreter(format!(
                    "package `{package_id}` module `{}` has no `{function_name}` entrypoint",
                    module_path.to_key()
                ))
            })?;
        let lir_id = Self::package_module_lir_id(package_id, module_path);
        let lir = package.borrow().lir_workspace.to_program();
        self.state.insert_lir(lir_id.clone(), lir);
        self.state.lir(&lir_id)?;
        self.state
            .insert_runtime_entrypoint(lir_id.clone(), function);
        Ok(lir_id)
    }

    pub async fn compile_package_module_native(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<(), CompilerDriverError> {
        let lir_id = self.select_entrypoint(package_id, module_path, function_name)?;
        self.evaluate_comptime_lir(&lir_id, &FullyQualifiedPath::new(module_path.clone()))
            .await?;
        Ok(())
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
                if metadata.id != *package_id {
                    return Err(CompilerDriverError::UnresolvablePackage(format!(
                        "provider returned metadata for {}, requested {package_id}",
                        metadata.id
                    )));
                }

                for dependency in &metadata.metadata.dependencies {
                    let dependency_id =
                        dependency.resolved_package_id.clone().ok_or_else(|| {
                            CompilerDriverError::UnresolvablePackage(format!(
                            "dependency `{}` of package `{package_id}` has no selected package ID",
                            dependency.package
                        ))
                        })?;
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
                if source.package_id != *package_id {
                    return Err(CompilerDriverError::UnresolvablePackage(format!(
                        "provider returned source for {}, requested {package_id}",
                        source.package_id
                    )));
                }
                let package = self.state.typing_ctx.env_ctx.begin_package(
                    package_id.clone(),
                    source,
                    self.state.typing_ctx.data_layout.clone(),
                );
                if self.pipeline == PipelineMode::Full {
                    let initial_units = self.compile_items_to_lir_units(&package).await?;
                    Self::publish_lir_units(&package, package_id, &initial_units)?;

                    for unit in &initial_units {
                        if unit.program.comptime_entries.is_empty() {
                            continue;
                        }
                        let lir_id = Self::package_module_lir_id(package_id, &unit.module_path);
                        self.state.insert_lir(lir_id.clone(), unit.program.clone());
                        self.evaluate_comptime_lir(
                            &lir_id,
                            &FullyQualifiedPath::new(unit.module_path.clone()),
                        )
                        .await?;
                    }

                    let units = self.compile_items_to_lir_units(&package).await?;
                    Self::publish_lir_units(&package, package_id, &units)?;
                }
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

    fn publish_lir_units(
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
        package_id: &PackageId,
        units: &[fp_core::lir::LirCompileUnit],
    ) -> Result<(), CompilerDriverError> {
        let layout = package.borrow().lir_workspace.data_layout.clone();
        let mut workspace = fp_core::lir::LirWorkspace::new(layout);
        for unit in units {
            workspace
                .add_program(
                    package_id.clone(),
                    unit.module_path.clone(),
                    unit.program.clone(),
                )
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }
        let mut package = package.borrow_mut();
        package.lir_units = units.to_vec();
        package.lir_workspace = workspace;
        Ok(())
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
                    let value = self.evaluate_comptime_block(pending.request()).await?;
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
    /// package's source items; safe because
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

            let package_id = self
                .state
                .typing_ctx
                .env_ctx
                .current_package()
                .cloned()
                .ok_or_else(|| {
                    CompilerDriverError::UnresolvablePackage(
                        "generic specialization requires a focused package".to_string(),
                    )
                })?;
            let package = self
                .state
                .typing_ctx
                .env_ctx
                .compiled_package(&package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
            let mut func_item = package
                .borrow()
                .items
                .iter()
                .find_map(|item| (item.item.id() == monomorph.item_id).then(|| item.item.clone()))
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

            let item = fp_core::package::PackageItem {
                path: monomorph.function_path.clone(),
                item: func_item,
            };
            package.borrow_mut().items.push(item);
            self.compile_package(&package_id).await?;
            Ok(())
        })
    }

    async fn evaluate_comptime_block(
        &mut self,
        request: &ComptimeRequest,
    ) -> Result<Value, CompilerDriverError> {
        // The block is already typed in its original lexical environment.
        // A callable is introduced only because MIR/LIR execution requires an
        // entrypoint; semantic lookup remains rooted in the original program.
        let function_name = format!("__fp_comptime_block_{}", request.expression_id);
        let function_def_id = request
            .program
            .items
            .iter()
            .map(|item| item.def_id)
            .max()
            .unwrap_or(hir::DefId::local(0))
            .saturating_add(1);
        let function = hir::Item {
            hir_id: request.block.hir_id,
            def_id: function_def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Function(hir::Function {
                sig: hir::FunctionSig {
                    name: hir::Symbol::new(function_name.clone()),
                    inputs: Vec::new(),
                    output: request.expected_ty.clone(),
                    generics: hir::Generics::default(),
                    abi: hir::Abi::Rust,
                },
                body: Some(request.block.clone()),
                is_const: true,
                is_extern: false,
                attrs: Vec::new(),
            }),
            span: request
                .block
                .expr
                .as_ref()
                .map(|expr| expr.span)
                .unwrap_or_else(Span::null),
        };
        let mut comptime_program = request.program.clone();
        comptime_program
            .def_map
            .insert(function.def_id, function.clone());
        comptime_program.items.push(function);

        let runtime_package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "comptime evaluation requires a focused package workspace".to_string(),
                )
            })?;
        let mut lowering = MirLowering::new()
            .with_typeck_results(&request.typeck_results)
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "comptime HIR-to-MIR setup failed: {error}"
                ))
            })?;
        lowering.register_external_definitions(&comptime_program);
        let mir = lowering.transform(comptime_program).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "comptime HIR-to-MIR lowering failed: {error}"
            ))
        })?;
        let mut lir_generator = LirGenerator::new(self.state.typing_ctx.data_layout.clone())
            .with_package_id(runtime_package_id.clone());
        let lir = lir_generator.transform(mir).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "comptime MIR-to-LIR lowering failed: {error}"
            ))
        })?;

        let mut execution_workspace =
            fp_core::lir::LirWorkspace::new(self.state.typing_ctx.data_layout.clone());
        for package in self.state.typing_ctx.env_ctx.crates().values() {
            execution_workspace
                .add_workspace(&package.borrow().lir_workspace)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }
        execution_workspace
            .add_program(
                runtime_package_id.clone(),
                QualifiedPath::new(Vec::new()),
                lir,
            )
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        self.interpreter = LirInterpreter::new();
        self.interpreter
            .inject_globals(&self.collect_resolved_const_values())
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let value = self
            .interpreter
            .run_function_named_in_workspace(
                &execution_workspace,
                &runtime_package_id,
                &fp_core::lir::Name::new(function_name),
            )
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        Ok(value)
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
                    if let Some(name) = Self::name_last_segment(name) {
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

    fn name_last_segment(name: &Name) -> Option<String> {
        match name {
            Name::Ident(ident) => Some(ident.as_str().to_string()),
            Name::Path(path) => path
                .segments
                .last()
                .map(|segment| segment.as_str().to_string()),
            Name::ParameterPath(_) => None,
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

    async fn compile_items_to_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
    ) -> Result<Vec<fp_core::lir::LirCompileUnit>, CompilerDriverError> {
        let hir_package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .and_then(|package_id| {
                self.state
                    .typing_ctx
                    .env_ctx
                    .compiled_package(package_id)
                    .map(|package| package.borrow().package_id)
            })
            .unwrap_or_default();
        let mut generator = HirGenerator::new()
            .with_intrinsic_normalizer(FerroIntrinsicNormalizer::new(
                fp_core::intrinsics::IntrinsicNormalizationMode::Compile,
            ))
            .with_package_id(hir_package_id)
            .with_def_id_start(self.next_hir_def_id)
            .with_lowering_config(HirLoweringConfig)
            .with_external_definitions(self.state.typing_ctx.env_ctx.hir_definitions())
            .with_external_modules(self.state.typing_ctx.env_ctx.module_paths());
        let package_source = package.borrow().clone();
        let hir_program = generator.transform_package(&package_source)?;
        self.next_hir_def_id = self.next_hir_def_id.max(generator.next_def_id_value());
        let package_exports = generator.exported_symbols();
        if let Some(package_id) = self.state.typing_ctx.env_ctx.current_package().cloned() {
            if let Some(package) = self.state.typing_ctx.env_ctx.compiled_package(&package_id) {
                package.borrow_mut().hir_exports.extend(package_exports);
            }
        }
        let (hir_program, typeck_results) = HirTypeChecker::new(hir_program)
            .with_context(self.state.typing_ctx.clone())
            .check()
            .await
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package HIR type checking failed: {error}"
                ))
            })?;
        let current_package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "package compilation requires a focused package workspace".to_string(),
                )
            })?;
        let package_path = QualifiedPath::new(Vec::new());
        let hir_id = HirId::new(format!("hir:{}", self.module_state_key(&package_path)));
        self.state.insert_hir(hir_id.clone(), hir_program);
        self.state.insert_hir_typeck(hir_id.clone(), typeck_results);
        if let Some(package) = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package.borrow_mut().hir_program = Some(self.state.hir(&hir_id)?.clone());
        }
        let fqp = FullyQualifiedPath::new(package_path.clone());
        let mir_id = self.lower_to_mir(&hir_id, &fqp).await?;
        if let Some(package) = self
            .state
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package.borrow_mut().mir_program = Some(self.state.mir(&mir_id)?.clone());
        }
        let lir_id = self.lower_to_lir(&mir_id, &fqp, &current_package_id)?;
        let lir = self.state.lir(&lir_id)?.clone();
        Ok(vec![fp_core::lir::LirCompileUnit {
            package_id: hir_package_id,
            module_path: package_path,
            program: lir,
        }])
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

        let package_id = self
            .state
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "comptime evaluation requires a focused package workspace".to_string(),
                )
            })?;
        let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
        if comptime_entries.is_empty() {
            self.state
                .insert_const_value(value_id.clone(), Value::unit());
            return Ok(0);
        }
        let mut execution_workspace = fp_core::lir::LirWorkspace::new(lir.data_layout.clone());
        for (_dependency_id, package) in self.state.typing_ctx.env_ctx.crates().iter() {
            execution_workspace
                .add_workspace(&package.borrow().lir_workspace)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }
        if execution_workspace
            .find_function(package_id.clone(), &comptime_entries[0].function)
            .is_none()
        {
            execution_workspace
                .add_program(package_id.clone(), path.path().clone(), lir)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        }

        let mut count = 0usize;
        let mut last = Value::unit();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        for entry in &comptime_entries {
            let result = self.interpreter.run_function_named_in_workspace(
                &execution_workspace,
                &package_id,
                &entry.function,
            );
            let mut value =
                result.map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            let entry_lir_ty = execution_workspace
                .find_function(package_id.clone(), &entry.function)
                .map(|function| function.signature.return_type.clone())
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "missing LIR result type for comptime entry {}",
                        entry.function
                    ))
                })?;
            value = self
                .interpreter
                .read_typed_const_value(value, &entry_lir_ty)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
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

    async fn lower_to_mir(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<MirId, CompilerDriverError> {
        // HIR has already passed type checking at this boundary. Lowering is
        // therefore strict: a failure is an internal compiler error, never a
        // recoverable source diagnostic.
        let mut hir = self.state.hir(hir_id)?.clone();
        for (_, external, _) in self.state.typing_ctx.env_ctx.hir_definitions() {
            for item in external.items {
                if matches!(item.kind, hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_)) {
                    hir.def_map.insert(item.def_id, item.clone());
                    hir.items.push(item);
                }
            }
        }
        let typeck_results = self.state.hir_typeck(hir_id)?.clone();
        let mut lowering = MirLowering::new()
            .with_typeck_results(&typeck_results)
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR setup failed for {}: {error}",
                    path.to_key()
                ))
            })?;
        lowering.register_external_definitions(&hir);
        for (key, value) in self.state.resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let result = lowering.transform_async(hir).await;
        let (diagnostics, had_errors) = lowering.take_diagnostics();
        let mir = result.map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering failed: {error}"
            ))
        })?;
        if had_errors {
            let details = diagnostics
                .iter()
                .map(|diagnostic| diagnostic.message.as_str())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering reported diagnostics: {details}"
            )));
        }
        let mir_id = MirId::new(format!("mir:{}", self.module_state_key(path.path())));
        self.state.insert_mir(mir_id.clone(), mir);
        Ok(mir_id)
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
        package_id: &PackageId,
    ) -> Result<LirId, CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let mut lowering = LirGenerator::new(self.state.typing_ctx.data_layout.clone())
            .with_package_id(package_id.clone())
            .with_module_path(path.path().to_key());
        let lir = lowering.transform(mir).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "MIR-to-LIR lowering failed for {}: {error}",
                path.to_key()
            ))
        })?;
        let lir_id = Self::package_module_lir_id(package_id, path.path());
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(lir_id)
    }

    fn package_module_lir_id(package_id: &PackageId, path: &QualifiedPath) -> LirId {
        LirId::new(format!("lir:{}:{}", package_id.as_str(), path.to_key()))
    }

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        Self::resolved_const_values_snapshot(&self.state.typing_ctx)
    }

    /// Same as `collect_resolved_const_values`, but against a bare
    /// `Rc<TypingContext>` for compiler hooks that do not hold a driver.
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
