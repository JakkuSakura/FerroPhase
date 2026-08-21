use fp_backend::transformations::{HirGenerator, HirLoweringConfig, LirGenerator, MirLowering};
use fp_core::ast::{Expr, ExprKind, Item, ItemKind, Ty, TypeStruct, TypeType, Value};
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::ast::path::QualifiedPath;
use fp_core::package::{DependencyDescriptor, DependencyKind, PackageId};
use fp_core::span::Span;
use fp_interpret::LirInterpreter;
use fp_lang::FerroIntrinsicNormalizer;
use fp_typing::TypingContext;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::task::{Context, Poll, Waker};
use std::rc::Rc;

use crate::{
    CompilerDriverError, CompilerState, ConstValueId, ExecutorHandle, FullyQualifiedPath, HirId,
    LirId, MirId, RuntimeValueId,
};

pub struct CompilerDriver {
    /// `Rc<RefCell<_>>`, not owned: a spawned comptime-resolution task (see
    /// `TypingContext`'s injected `resolve_comptime` callback,
    /// `type_check_program`) needs to reach the same HIR/MIR/LIR/typing
    /// state independently of whatever `&mut self`-holding future is
    /// already driving `compile_package`/`compile_native` at the time —
    /// that future's `&mut self` borrow lasts for its entire lifetime (how
    /// `async fn` desugars), so a `'static` task closure cannot also borrow
    /// `self` directly. Sharing just `state` this way (not `interpreter`,
    /// `building_packages`, etc. — those aren't needed by anything spawned
    /// as a task) keeps the rest of `CompilerDriver` an ordinary `&mut
    /// self`-based type.
    pub state: Rc<RefCell<CompilerState>>,
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
    Native,
    /// Stop after parsing: resolve modules, parse sources, return AST items
    NominalTranspile,
    /// HIR typing + lift back to AST: AST → HIR → typing → AST
    TypecheckedTranspile,
}

impl CompilerDriver {
    fn module_state_key(&self, path: &QualifiedPath) -> String {
        Self::module_state_key_for(&self.state, path)
    }

    /// Same as `module_state_key`, but against a bare `Rc<RefCell<CompilerState>>` —
    /// for the spawned comptime-resolution task, which doesn't hold a `CompilerDriver`.
    fn module_state_key_for(state: &Rc<RefCell<CompilerState>>, path: &QualifiedPath) -> String {
        let package_id = state
            .borrow()
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
            Rc::new(fp_core::workspace::WorkspaceContext::new(std::sync::Arc::new(
                fp_core::package::provider::EmptyProvider,
            ))),
        )
    }

    pub fn with_workspace(
        data_layout: fp_core::lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<fp_core::workspace::WorkspaceContext>,
    ) -> Self {
        let mut state = CompilerState::new(data_layout.clone(), tasks.clone());
        state.typing_ctx = Rc::new(TypingContext::new(data_layout, workspace, tasks));
        Self {
            state: Rc::new(RefCell::new(state)),
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
            pipeline: PipelineMode::Native,
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            state: Rc::new(RefCell::new(state)),
            interpreter: LirInterpreter::new(),
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            next_hir_def_id: 0,
            pipeline: PipelineMode::Native,
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
        let executor = self.state.borrow().tasks.clone();
        executor.run(self.compile_native(package_id))
    }

    pub fn compile_bytecode_sync(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        let executor = self.state.borrow().tasks.clone();
        executor.run(self.compile_bytecode(package_id))
    }

    /// Focus subsequent module work on an already compiled package. The
    /// package itself and its imported dependencies remain shared through
    /// `Rc`; only the lookup context becomes package-local.
    pub fn focus_package(&mut self, package_id: PackageId) -> Result<(), CompilerDriverError> {
        let parent_context = self.state.borrow().typing_ctx.clone();
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
        self.state.borrow_mut().typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
            parent_context.executor.clone(),
        ));
        Ok(())
    }

    pub fn execute_runtime(
        &mut self,
        lir_id: &LirId,
    ) -> Result<fp_core::ast::Value, CompilerDriverError> {
        let lir = self.state.borrow().lir(lir_id)?.clone();
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let entrypoint = self.state.borrow().runtime_entrypoint(lir_id)?;
        let value = self.interpreter.run_entrypoint(&lir, entrypoint)?;
        let value_id = RuntimeValueId::new(format!("runtime_value:{}", lir_id.as_str()));
        self.state.borrow_mut().insert_runtime_value(value_id, value.clone());
        Ok(value)
    }

    /// Resolves the `DefId` of the function named `function_name` anywhere
    /// in `package_id`'s published HIR — package-based, not module-based
    /// (see `fp_core::package::resolve_entrypoint_def_id`'s doc comment).
    /// `module_path` isn't used for this resolution itself; it's taken here
    /// only because every caller already has one on hand for the sibling
    /// LIR-id-keying/comptime-path purposes `select_entrypoint`/
    /// `compile_package_module_native` need it for.
    pub fn resolve_entrypoint_def_id(
        &self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<hir::DefId, CompilerDriverError> {
        let _ = module_path;
        let package = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let package = package.borrow();
        fp_core::package::resolve_entrypoint_def_id(package_id, &package, function_name)
            .map_err(|error| CompilerDriverError::Interpreter(error.to_string()))
    }

    /// Renames the LIR function identified by `def_id` to `bare_name` in
    /// place. The process entry point is located downstream (native/asm
    /// emission) by its final, bare symbol name — a linkage requirement,
    /// not a display convention. Normal mangling gives a module-nested
    /// `main` a qualified name (e.g. `module__main`), but the OS/runtime
    /// always calls the bare `main`, so the resolved entrypoint needs
    /// renaming back to the name it was looked up by, regardless of its
    /// module qualification.
    pub fn rename_lir_function(
        lir: &mut fp_core::lir::LirProgram,
        def_id: hir::DefId,
        bare_name: &str,
    ) {
        fp_core::package::rename_lir_function(lir, def_id, bare_name)
    }

    pub fn select_entrypoint(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<LirId, CompilerDriverError> {
        let package = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let function = self.resolve_entrypoint_def_id(package_id, module_path, function_name)?;
        let lir_id = Self::package_module_lir_id(package_id, module_path);
        let mut lir = package.borrow().lir_workspace.to_program();
        Self::rename_lir_function(&mut lir, function, function_name);
        self.state.borrow_mut().insert_lir(lir_id.clone(), lir);
        self.state.borrow().lir(&lir_id)?;
        self.state.borrow_mut()
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
        let parent_context = self.state.borrow().typing_ctx.clone();
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

        let package_workspace = parent_context.env_ctx.for_package(package_id.clone());
        self.state.borrow_mut().typing_ctx = Rc::new(TypingContext::new(
            parent_context.data_layout.clone(),
            Rc::new(package_workspace),
            parent_context.executor.clone(),
        ));

        let result: Result<Rc<RefCell<fp_core::package::CompiledPackage>>, CompilerDriverError> =
            async {
                let provider = self
                    .state.borrow()
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

                self.compile_dependencies(package_id, &metadata.metadata.dependencies)
                    .await?;

                let source = provider.load_package_source(package_id).map_err(|error| {
                    CompilerDriverError::UnresolvablePackage(format!("{package_id}: {error}"))
                })?;
                if source.package_id != *package_id {
                    return Err(CompilerDriverError::UnresolvablePackage(format!(
                        "provider returned source for {}, requested {package_id}",
                        source.package_id
                    )));
                }
                let package = self.state.borrow().typing_ctx.env_ctx.begin_package(
                    package_id.clone(),
                    source,
                    self.state.borrow().typing_ctx.data_layout.clone(),
                );
                // `TypecheckedTranspile` needs HIR generation + typing too (it lifts the
                // typed HIR back to AST inside `compile_items_to_lir_units`) — it now also
                // attempts MIR/LIR lowering there, best-effort, purely so any comptime
                // entries (e.g. `const { .. }` blocks) can be validated below through the
                // real interpreter; unlike Native, a comptime failure here is reported,
                // not propagated, since the Kotlin backend never consumes the resolved
                // value (only the block's type, already known independent of this).
                if matches!(self.pipeline, PipelineMode::Native | PipelineMode::TypecheckedTranspile) {
                    let mut units = self
                        .compile_items_to_lir_units(&package)
                        .await?;
                    if !units.is_empty() {
                        Self::publish_lir_units(&package, package_id, &units)?;

                        let lir = package.borrow().lir_workspace.to_program();
                        if !lir.comptime_entries.is_empty() {
                            let module_path = QualifiedPath::new(Vec::new());
                            let lir_id = Self::package_module_lir_id(package_id, &module_path);
                            self.state.borrow_mut().insert_lir(lir_id.clone(), lir);
                            let fqp = FullyQualifiedPath::new(module_path);
                            if self.pipeline == PipelineMode::Native {
                                let block_values = self.evaluate_comptime_lir(&lir_id, &fqp).await?;
                                if !block_values.is_empty() {
                                    self.apply_resolved_comptime_block_values(&block_values)?;
                                }
                                units = self.relower_cached_lir_units(&package).await?;
                                Self::publish_lir_units(&package, package_id, &units)?;
                            } else if let Err(error) =
                                self.evaluate_comptime_lir(&lir_id, &fqp).await
                            {
                                fp_core::diagnostics::report_warning_with_context(
                                    "const-eval".to_string(),
                                    format!("comptime validation failed: {error}"),
                                );
                            }
                        }
                    }
                    let _ = units;
                }
                Ok(package)
            }
            .await;

        self.building_packages.remove(package_id);
        self.state.borrow_mut().typing_ctx = parent_context.clone();
        let package = result?;
        self.compiled_packages
            .insert(package_id.clone(), package.clone());
        parent_context
            .env_ctx
            .import_package(package_id.clone(), package.clone());
        Ok(package)
    }

    /// Compile each of `package_id`'s declared dependencies, in order,
    /// installing `std`'s package as the prelude source once it's compiled.
    /// Extracted out of `compile_package` so `compile_workspace` can drive
    /// the same recursive, cached, cycle-safe walk over a synthetic
    /// dependency list built from a workspace's member packages.
    async fn compile_dependencies(
        &mut self,
        package_id: &PackageId,
        dependencies: &[DependencyDescriptor],
    ) -> Result<(), CompilerDriverError> {
        for dependency in dependencies {
            let dependency_id = dependency.resolved_package_id.clone().ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(format!(
                    "dependency `{}` of package `{package_id}` has no selected package ID",
                    dependency.package
                ))
            })?;
            let dependency_package = Box::pin(self.compile_package(&dependency_id)).await?;
            if dependency_id.as_str() == "std" {
                self.state
                    .borrow()
                    .typing_ctx
                    .env_ctx
                    .install_prelude(dependency_package);
            }
        }
        Ok(())
    }

    /// Compile every member of a workspace by walking them through the same
    /// recursive, cached, cycle-safe dependency machinery `compile_package`
    /// already uses for a package's own declared dependencies — `root_id` is
    /// a caller-supplied bookkeeping identity only, never resolved through a
    /// `PackageProvider`. An inter-member dependency (e.g. member B
    /// path-depends on sibling A) is compiled exactly once regardless of
    /// which member's turn surfaces it first, since both go through
    /// `compile_package`'s own `compiled_packages` cache. Callers read back
    /// each member's result via `WorkspaceContext::package_source` rather
    /// than from this call's return value.
    pub async fn compile_workspace(
        &mut self,
        root_id: &PackageId,
        members: &[PackageId],
    ) -> Result<(), CompilerDriverError> {
        let dependencies: Vec<DependencyDescriptor> = members
            .iter()
            .map(|id| DependencyDescriptor {
                package: id.as_str().to_string(),
                resolved_package_id: Some(id.clone()),
                constraint: None,
                kind: DependencyKind::Normal,
                features: Vec::new(),
                optional: false,
                target: Default::default(),
            })
            .collect();
        self.compile_dependencies(root_id, &dependencies).await
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

    async fn compile_items_to_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
    ) -> Result<Vec<fp_core::lir::LirCompileUnit>, CompilerDriverError> {
        let hir_package_id = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .current_package()
            .and_then(|package_id| {
                self.state.borrow()
                    .typing_ctx
                    .env_ctx
                    .compiled_package(package_id)
                    .map(|package| package.borrow().package_id)
            })
            .unwrap_or_default();
        let mut package_source = package.borrow().clone();
        let macro_rules_defs =
            fp_lang::collect_macro_rules_defs(package_source.items.iter().map(|item| &item.item));
        // Item-position macro invocations (e.g. `make_adder!(add_two, 2);`)
        // must expand into real items *before* `HirGenerator` ever sees
        // them — matching rustc's own model, where macro-expanded tokens
        // re-enter the exact same pipeline as hand-written code rather than
        // a separate, lesser one. Without this, such an invocation is
        // silently dropped by `ast_to_hir`'s own item loop, and whatever it
        // would have defined never exists.
        package_source.items = fp_lang::expand_item_macros(package_source.items, &macro_rules_defs);
        let mut generator = HirGenerator::new()
            .with_intrinsic_normalizer(
                FerroIntrinsicNormalizer::new(fp_core::intrinsics::IntrinsicNormalizationMode::Compile)
                    .with_macro_rules_defs(macro_rules_defs),
            )
            .with_package_id(hir_package_id)
            .with_def_id_start(self.next_hir_def_id)
            .with_lowering_config(HirLoweringConfig {
                // Per-target capabilities (see `fp_core::capabilities::
                // LanguageCapabilities`), set by `fp-cli` before compiling
                // via `CompilerState::set_capabilities` — defaults to
                // `NATIVE` (nothing first-class) for any caller that never
                // sets it, matching this field's prior behavior exactly.
                capabilities: self.state.borrow().capabilities(),
            })
            .with_workspace(self.state.borrow().typing_ctx.env_ctx.clone());
        let hir_program = generator.transform_package(&package_source)?;
        self.next_hir_def_id = self.next_hir_def_id.max(generator.next_def_id_value());
        let package_exports = generator.exported_symbols();
        let type_alias_exports = generator.exported_type_aliases();
        if let Some(package_id) = self.state.borrow().typing_ctx.env_ctx.current_package().cloned() {
            if let Some(package) = self.state.borrow().typing_ctx.env_ctx.compiled_package(&package_id) {
                package.borrow_mut().hir_exports.extend(package_exports);
                package
                    .borrow_mut()
                    .type_alias_exports
                    .extend(type_alias_exports);
            }
        }
        let (mut hir_program, typeck_results) = self
            .type_check_program(hir_program)
            .await
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package HIR type checking failed: {error}"
                ))
            })?;
        // Formalized post-typecheck `#[op(...)]` promotion (see
        // `hir_normalization` for the full rationale): only
        // `TypecheckedTranspile` (Kotlin/Shell AST-lift) promotes
        // pure-`Op`-only calls to `IntrinsicCall(CallKind::Op(..))` in
        // place. `Native` leaves them as ordinary calls to their real stub
        // bodies (see `hir_materialization` for why that's correct here).
        let promote_op_only = matches!(self.pipeline, PipelineMode::TypecheckedTranspile);
        fp_backend::transforms::hir_normalization::normalize_program(
            &mut hir_program,
            Some(&typeck_results),
            promote_op_only,
        );
        let current_package_id = self
            .state.borrow()
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
        self.state.borrow_mut().insert_hir(hir_id.clone(), hir_program);
        self.state.borrow_mut().insert_hir_typeck(hir_id.clone(), typeck_results);
        if let Some(package) = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package
                .borrow_mut()
                .set_hir_program(self.state.borrow().hir(&hir_id)?.clone());
        }

        // TypecheckedTranspile: lift typed HIR back to AST — this is what
        // the Kotlin backend actually reads, and doesn't depend on
        // anything below succeeding.
        if self.pipeline == PipelineMode::TypecheckedTranspile {
            // Scoped narrowly (dropped before the `lower_to_mir`/`lower_to_lir`
            // calls below, which need their own `self.state` borrows) —
            // `lift_items_by_path`/`referenced_paths_by_path` return owned
            // data, so nothing here needs to outlive this block.
            let (lifted_items_by_path, referenced_paths_by_path) = {
                let state = self.state.borrow();
                let hir = state.hir(&hir_id)?;
                // `typeck_results` was moved into `self.state` above
                // (`insert_hir_typeck`) — fetch it back by the same `hir_id`
                // to attach real resolved types onto the lifted AST's
                // `Expr.ty()` slots.
                let typeck = state.hir_typeck(&hir_id).ok();
                // Keyed by qualified name so the merge below can splice typed
                // content back onto the original source items by identity
                // even when the two lists don't match 1:1 (synthetic items
                // with no source counterpart, `use` items with no HIR
                // counterpart, or an individual item that fails to lift
                // without poisoning the rest of the package).
                let lifter = fp_backend::transforms::HirToAstLifter::new(
                    hir,
                    typeck,
                    Some(state.typing_ctx.env_ctx.as_ref()),
                );
                // `lift_items_by_path` treats an `impl` block as an opaque
                // placeholder — merge in each impl *method*'s own lifted
                // body too (keyed by its own qualified path, disjoint from
                // any top-level item's), or typed/normalized impl method
                // bodies never get spliced back in at all.
                let mut lifted_items_by_path = lifter.lift_items_by_path();
                lifted_items_by_path.extend(lifter.lift_impl_methods_by_path());
                // Publish this lift's `ExprId -> resolved Ty` side-table so
                // downstream backend serializers / AST materializer passes
                // (Kotlin's Vec/String/enum receiver checks, the intrinsic
                // materializer's HashMap detection, ...) can look resolved
                // types up via `fp_core::ast::resolved_expr_type` — see
                // `HirToAstLifter::publish_resolved_expr_types`'s doc comment.
                lifter.publish_resolved_expr_types();
                (lifted_items_by_path, lifter.referenced_paths_by_path())
            };
            if let Some(pkg) = self
                .state.borrow()
                .typing_ctx
                .env_ctx
                .compiled_package(&current_package_id)
            {
                let mut pkg = pkg.borrow_mut();
                // Splice typed/normalized content back onto the original
                // untyped source items by qualified-path identity — the
                // single, canonical reconciliation point (this used to live
                // in `fp-cli`'s `typecheck_package`, re-deriving the same
                // keys `HirToAstLifter` already computes here; two
                // independent implementations of "this item's qualified
                // path" is exactly what let them silently disagree).
                for pkg_item in &mut pkg.items {
                    if let ItemKind::Impl(imp) = pkg_item.item.kind_mut() {
                        // Trait impls aren't in `lifted_items_by_path` at
                        // all (`lift_impl_methods_by_path` skips them —
                        // see its doc comment) — only attempt this for
                        // inherent impls, matching what was actually lifted.
                        if imp.trait_ty.is_some() {
                            continue;
                        }
                        let mut base_path = pkg_item.module_path.segments.clone();
                        let Some(self_ty_name) = impl_self_type_name(&imp.self_ty) else {
                            continue;
                        };
                        base_path.push(self_ty_name.to_string());
                        for method_item in imp.items.iter_mut() {
                            let ItemKind::DefFunction(function) = method_item.kind() else {
                                continue;
                            };
                            let mut path = base_path.clone();
                            path.push(function.name.name.clone());
                            let key = fp_core::hir::DefPath::new(
                                path.into_iter().map(fp_core::hir::Symbol::new).collect(),
                            );
                            if let Some(typed) = lifted_items_by_path.get(&key) {
                                *method_item = typed.clone();
                            }
                        }
                        continue;
                    }
                    let Some(name) = item_own_name(&pkg_item.item) else {
                        // No natural qualified name (`Import`, a bare
                        // `Module` declaration, ...) — never a key in
                        // `lifted_items_by_path`, so it keeps its original
                        // source form.
                        continue;
                    };
                    let mut path = pkg_item.module_path.segments.clone();
                    path.push(name.to_string());
                    let key = fp_core::hir::DefPath::new(
                        path.into_iter().map(fp_core::hir::Symbol::new).collect(),
                    );
                    if let Some(typed) = lifted_items_by_path.get(&key) {
                        pkg_item.item = typed.clone();
                    }
                    // Else: this item's typed form wasn't produced — e.g. a
                    // per-item lift failure — keep the original, untyped
                    // item rather than failing typed-splice for the whole
                    // package.
                }
                pkg.referenced_paths_by_path = Some(referenced_paths_by_path);
            }
            // Best-effort HIR->MIR->LIR lowering, purely so any
            // `const { .. }` blocks (see `register_const_block_comptime_entry`)
            // get validated later through the real interpreter
            // (`evaluate_comptime_lir`) instead of a hand-rolled one. MIR
            // lowering was built for the Native pipeline and may not yet
            // cover every construct real vendored std/workspace code
            // exercises — a failure here is reported and this package
            // simply produces no LIR units (comptime validation is
            // skipped for it), matching this pipeline's prior behavior;
            // the lifted AST above is already complete regardless.
            let fqp = FullyQualifiedPath::new(package_path.clone());
            return match self.lower_to_mir(&hir_id, &fqp).await {
                Ok((mir_id, struct_layouts, full_layouts, adt_defs, opaque_payload_sizes, resolved_const_values)) => {
                    if let Some(package) = self
                        .state.borrow()
                        .typing_ctx
                        .env_ctx
                        .compiled_package(&current_package_id)
                    {
                        package.borrow_mut().mir_program = Some(self.state.borrow().mir(&mir_id)?.clone());
                        package
                            .borrow_mut()
                            .mir_struct_fields
                            .extend(struct_layouts.clone());
                        package.borrow_mut().mir_adt_defs.extend(adt_defs.clone());
                        package
                            .borrow_mut()
                            .mir_resolved_const_values
                            .extend(resolved_const_values);
                    }
                    match self.lower_to_lir(
                        &mir_id,
                        &fqp,
                        &current_package_id,
                        &full_layouts,
                        &opaque_payload_sizes,
                    ) {
                        Ok(lir_id) => {
                            let lir = self.state.borrow().lir(&lir_id)?.clone();
                            Ok(vec![fp_core::lir::LirCompileUnit {
                                package_id: hir_package_id,
                                module_path: package_path,
                                program: lir,
                            }])
                        }
                        Err(error) => {
                            fp_core::diagnostics::report_warning_with_context(
                                "const-eval".to_string(),
                                format!(
                                    "MIR->LIR lowering failed, skipping comptime validation for this package: {error}"
                                ),
                            );
                            Ok(Vec::new())
                        }
                    }
                }
                Err(error) => {
                    fp_core::diagnostics::report_warning_with_context(
                        "const-eval".to_string(),
                        format!(
                            "HIR->MIR lowering failed, skipping comptime validation for this package: {error}"
                        ),
                    );
                    Ok(Vec::new())
                }
            };
        }

        let fqp = FullyQualifiedPath::new(package_path.clone());
        let (mir_id, struct_layouts, full_layouts, adt_defs, opaque_payload_sizes, resolved_const_values) =
            self.lower_to_mir(&hir_id, &fqp).await?;
        if let Some(package) = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&current_package_id)
        {
            package.borrow_mut().mir_program = Some(self.state.borrow().mir(&mir_id)?.clone());
            package
                .borrow_mut()
                .mir_struct_fields
                .extend(struct_layouts);
            package.borrow_mut().mir_adt_defs.extend(adt_defs.clone());
            package
                .borrow_mut()
                .mir_resolved_const_values
                .extend(resolved_const_values);
        }
        // `lower_to_lir` falls back to the workspace's other packages'
        // `mir_adt_defs` lazily on a miss (see `LirGenerator::
        // lookup_adt_def`) — no need to pre-flatten every dependency's
        // ADT defs into a combined map here.
        let lir_id = self.lower_to_lir(
            &mir_id,
            &fqp,
            &current_package_id,
            &full_layouts,
            &opaque_payload_sizes,
        )?;

        let lir = self.state.borrow().lir(&lir_id)?.clone();
        Ok(vec![fp_core::lir::LirCompileUnit {
            package_id: hir_package_id,
            module_path: package_path,
            program: lir,
        }])
    }

    /// Writes `evaluate_comptime_lir`'s real, interpreter-computed values
    /// back into this package's stored `TypeckResults::const_block_values`
    /// (keyed by each block's own `HirId`, via `const_block_hir_id`),
    /// before `relower_cached_lir_units` re-lowers HIR->MIR a second time.
    /// That second lowering pass already has a fast path
    /// (`typeck_const_block_values.get(&expr.hir_id)`,
    /// `hir_to_mir/expr.rs`'s `ConstBlock` arm) that embeds a real constant
    /// when a value is present there and otherwise falls back to lowering
    /// the block as ordinary runtime code — this is what turns that
    /// fallback into a real compile-time constant, without needing typing
    /// itself to ever suspend on the value (neither `ConstBlock` arm in
    /// `hir_typeck.rs` needs it to determine its own type).
    fn apply_resolved_comptime_block_values(
        &mut self,
        block_values: &HashMap<hir::HirId, Value>,
    ) -> Result<(), CompilerDriverError> {
        let module_path = QualifiedPath::new(Vec::new());
        let hir_id = HirId::new(format!("hir:{}", self.module_state_key(&module_path)));
        let mut typeck_results = self.state.borrow().hir_typeck(&hir_id)?.clone();
        typeck_results
            .const_block_values
            .extend(block_values.iter().map(|(id, value)| (*id, value.clone())));
        self.state.borrow_mut().insert_hir_typeck(hir_id, typeck_results);
        Ok(())
    }

    async fn relower_cached_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::package::CompiledPackage>>,
    ) -> Result<Vec<fp_core::lir::LirCompileUnit>, CompilerDriverError> {
        let package_id = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "package re-lowering requires a focused package workspace".to_string(),
                )
            })?;
        let hir_package_id = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&package_id)
            .map(|package| package.borrow().package_id)
            .unwrap_or_default();
        let module_path = QualifiedPath::new(Vec::new());
        let hir_id = HirId::new(format!("hir:{}", self.module_state_key(&module_path)));
        let fqp = FullyQualifiedPath::new(module_path.clone());
        let (mir_id, struct_layouts, full_layouts, adt_defs, opaque_payload_sizes, resolved_const_values) =
            self.lower_to_mir(&hir_id, &fqp).await?;
        {
            let mut package = package.borrow_mut();
            package.mir_program = Some(self.state.borrow().mir(&mir_id)?.clone());
            package.mir_struct_fields.extend(struct_layouts);
            package.mir_adt_defs.extend(adt_defs.clone());
            package.mir_resolved_const_values.extend(resolved_const_values);
        }
        // See the identical comment in `compile_items_to_lir_units` —
        // `lower_to_lir` falls back to the workspace's other packages
        // lazily, no pre-flatten needed.
        let lir_id = self.lower_to_lir(
            &mir_id,
            &fqp,
            &package_id,
            &full_layouts,
            &opaque_payload_sizes,
        )?;
        Ok(vec![fp_core::lir::LirCompileUnit {
            package_id: hir_package_id,
            module_path,
            program: self.state.borrow().lir(&lir_id)?.clone(),
        }])
    }

    /// Type-checks `program`. Each `const { .. }` block's `ComptimeRequest`
    /// (`hir_typeck.rs`'s two `ConstBlock` arms, both genuinely `.await` it —
    /// suspending there is what lets the executor make progress on other,
    /// independent items in the meantime, exactly like any other suspend
    /// point) is answered with a *real* value computed by the interpreter,
    /// never a placeholder — and answered immediately, not deferred until
    /// the executor stalls: a `ComptimeRequest` carries its own
    /// self-sufficient snapshot (`request.program`/`request.typeck_results`,
    /// captured right after its own body finished type-checking), so
    /// nothing is gained by waiting. Every request drained this round is
    /// resolved right here via `resolve_one_comptime_request` (item-scoped —
    /// see that method's doc comment) and completed on the spot, `Ok` or
    /// `Err`; a request's own failure only fails the specific item awaiting
    /// it (per-item isolation, matching `typecheck_item`), not the whole
    /// package. If a tick makes no executor progress *and* there were no
    /// requests to resolve, that's a genuine stall — a real dependency
    /// cycle among same-package items with no comptime request involved at
    /// all — and the whole type-check fails.
    async fn type_check_program(
        &mut self,
        program: hir::Program,
    ) -> fp_core::Result<(hir::Program, fp_typing::TypeckResults)> {
        let context = self.state.borrow().typing_ctx.clone();
        let (shared, mut future) =
            fp_typing::spawn_package_typecheck(program, Some(context.clone()));
        loop {
            let poll = {
                let waker = Waker::noop();
                let mut cx = Context::from_waker(waker);
                future.as_mut().poll(&mut cx)
            };
            match poll {
                Poll::Ready(result) => {
                    result?;
                    // rustc-style `tcx.sess.has_errors()` gate: a per-item
                    // task that hit a real typecheck error still resolves
                    // the joined future `Ok(())` (see `typecheck_item`'s
                    // deliberate per-item isolation in `fp-typing`), so
                    // without this check the resulting, incomplete
                    // `TypeckResults` would be handed straight to
                    // HIR->MIR lowering — whose own, unrelated failure
                    // (triggered by the exact gap this item's aborted
                    // check left behind) would then mask the real,
                    // specific diagnostic recorded here.
                    if context.has_typing_errors() {
                        // Every diagnostic accumulated so far — not just the
                        // hard item-check aborts folded into the returned
                        // `Err` below — goes to stderr, so a recovered/
                        // non-fatal mismatch elsewhere in the same package
                        // (which never aborts an item's check, so never
                        // makes it into the combined `Err` message) is
                        // still visible when diagnosing a failure.
                        Self::emit_typing_diagnostics_to_stderr(&context);
                        let combined = context
                            .item_check_failures
                            .borrow()
                            .iter()
                            .map(|diagnostic| diagnostic.as_core_diagnostic().to_string())
                            .collect::<Vec<_>>()
                            .join("\n");
                        // A real error anywhere is a hard failure: this
                        // package's `TypeckResults` would otherwise be
                        // handed straight to HIR->MIR lowering with the
                        // failed item's types missing.
                        return Err(fp_core::error::Error::diagnostic(
                            fp_core::diagnostics::Diagnostic::error(combined),
                        ));
                    }
                    return Ok(fp_typing::finish_package_typecheck(&shared));
                }
                Poll::Pending => {
                    // Drive whichever per-item typecheck tasks are ready —
                    // this is what lets one item's task (e.g. a same-package
                    // `const` referencing another declared later in
                    // `program.items`) await another item's task and make
                    // real progress, instead of depending on textual order.
                    let mut progressed = false;
                    while context.executor.tick().is_some() {
                        progressed = true;
                    }
                    let requests = self.state.borrow().typing_ctx.take_comptime_requests();
                    for request in requests {
                        let result = self
                            .resolve_one_comptime_request(request.request())
                            .await
                            .map_err(|error| fp_core::error::Error::from(error.to_string()));
                        request.complete(result);
                        progressed = true;
                    }
                    if progressed {
                        continue;
                    }
                    if context.executor.has_parked_tasks() {
                        // A stall has no combined-`Err` diagnostic message
                        // of its own (unlike the `has_typing_errors` branch
                        // above) — whatever real, specific diagnostics did
                        // get recorded before the stall are the only lead on
                        // *why* the cycle happened, so they must reach
                        // stderr here or they're lost entirely.
                        Self::emit_typing_diagnostics_to_stderr(&context);
                        let keys = context.executor.parked_task_keys();
                        eprintln!("fp-compiler: {} task(s) still parked at stall:", keys.len());
                        let key_to_path: std::collections::HashMap<String, String> = shared
                            .program()
                            .def_paths
                            .iter()
                            .map(|(def_id, path)| (format!("typecheck:{def_id:?}"), path.to_string()))
                            .collect();
                        for key in &keys {
                            let resolved = key_to_path.get(key).cloned().or_else(|| {
                                Self::find_item_label_by_key(shared.program(), key)
                            });
                            eprintln!(
                                "  {key} -> {}",
                                resolved.as_deref().unwrap_or("? (not found)")
                            );
                        }
                        return Err(fp_core::error::Error::from(
                            "HIR type checking stalled: a genuine dependency cycle among \
                             same-package items prevented further progress",
                        ));
                    }
                    Self::emit_typing_diagnostics_to_stderr(&context);
                    return Err(fp_core::error::Error::from(
                        "HIR type checking suspended without a comptime request",
                    ));
                }
            }
        }
    }

    /// Prints every diagnostic accumulated on `context` so far to stderr,
    /// one per line — both the hard item-check aborts (`item_check_failures`)
    /// and every other recovered/non-fatal mismatch recorded along the way
    /// (`diagnostics`), since either category can be the real lead on why a
    /// package's typecheck ultimately failed or stalled, and previously
    /// neither was visible outside the one combined message folded into a
    /// success-path `Err` (never emitted at all on the stall path).
    fn emit_typing_diagnostics_to_stderr(context: &fp_typing::TypingContext) {
        let diagnostics = context.diagnostics.borrow();
        if diagnostics.is_empty() {
            return;
        }
        eprintln!(
            "fp-compiler: {} typing diagnostic(s) recorded before failure:",
            diagnostics.len()
        );
        for diagnostic in diagnostics.iter() {
            eprintln!("  {}", diagnostic.as_core_diagnostic());
        }
    }

    /// Best-effort fallback for a stalled task's key that `def_paths` has no
    /// entry for — the common case is a function-local item statement,
    /// which deliberately never gets a module-qualified registration (see
    /// `suppress_global_registration_depth` in `ast_to_hir`) and so has no
    /// `def_paths` entry either. Walks every item (recursing one level into
    /// function/method bodies, where a local item statement actually
    /// lives) looking for a matching `DefId`, and reports it by its own
    /// `HirId`/kind — not a real qualified path (there isn't one), just
    /// enough to identify which declaration is stuck.
    fn find_item_label_by_key(program: &hir::Program, key: &str) -> Option<String> {
        fn scan_block(block: &hir::Block, target: &str, found: &mut Option<String>) {
            if found.is_some() {
                return;
            }
            for stmt in &block.stmts {
                if let hir::StmtKind::Item(item) = &stmt.kind {
                    if format!("typecheck:{:?}", item.def_id) == target {
                        *found = Some(format!("<local item, kind {:?}>", item.kind));
                        return;
                    }
                    scan_item(item, target, found);
                }
            }
        }
        fn scan_item(item: &hir::Item, target: &str, found: &mut Option<String>) {
            if found.is_some() {
                return;
            }
            match &item.kind {
                hir::ItemKind::Function(function) => {
                    if let Some(body) = &function.body {
                        scan_block(body, target, found);
                    }
                }
                hir::ItemKind::Impl(impl_item) => {
                    for member in &impl_item.items {
                        if format!("typecheck:{:?}", member.def_id) == target {
                            *found = Some(format!("<impl member, kind {:?}>", member.kind));
                            return;
                        }
                        if let hir::ImplItemKind::Method(function) = &member.kind {
                            if let Some(body) = &function.body {
                                scan_block(body, target, found);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        let mut found = None;
        for item in &program.items {
            if format!("typecheck:{:?}", item.def_id) == key {
                return Some(format!("<top-level item, kind {:?}>", item.kind));
            }
            scan_item(item, key, &mut found);
            if found.is_some() {
                break;
            }
        }
        found
    }

    /// Resolves exactly one pending comptime request, using only its own
    /// self-sufficient snapshot — `request.program`/`request.typeck_results`,
    /// captured at the moment its own body finished type-checking (see
    /// `fp_typing::ComptimeRequest`'s doc comment: everything that body can
    /// legally reference is, by construction, already fully typed there).
    /// Never re-fetches a fresher whole-package snapshot and never lowers
    /// any item's body other than the one synthetic function built from
    /// this request's own `block`/`expected_ty`
    /// (`MirLowering::transform_comptime_request`, via
    /// `lower_to_mir_for_comptime_request_with`) — called immediately for
    /// every request as soon as it's dequeued (`type_check_program`'s
    /// `Poll::Pending` arm), not deferred until the executor stalls. A
    /// genuine failure here (not "try again later" — there is no "later"
    /// for this call, its inputs are already final) only fails the one
    /// item awaiting this specific request, via `request.complete(Err(..))`
    /// — the same per-item isolation `typecheck_item` already relies on.
    async fn resolve_one_comptime_request(
        &mut self,
        request: &fp_typing::ComptimeRequest,
    ) -> Result<Value, CompilerDriverError> {
        let package_id = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .current_package()
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(
                    "comptime evaluation requires a focused package workspace".to_string(),
                )
            })?;
        let module_path = QualifiedPath::new(vec!["__comptime_probe__".to_string()]);
        let hir_id = HirId::new(format!("hir:{}", self.module_state_key(&module_path)));
        let fqp = FullyQualifiedPath::new(module_path);
        self.state.borrow_mut().insert_hir(hir_id.clone(), request.program.clone());
        self.state.borrow_mut()
            .insert_hir_typeck(hir_id.clone(), request.typeck_results.clone());
        let (mir_id, struct_layouts, full_layouts, adt_defs, opaque_payload_sizes, resolved_const_values) =
            Self::lower_to_mir_for_comptime_request_with(&self.state, &hir_id, &fqp, request)
                .await?;
        // Merge onto the current package the same way `compile_package`'s
        // own `lower_to_mir` callers already do — otherwise a struct/enum
        // first introduced by *this* probe (e.g. resolving a `const fn`
        // like `Vec::new()` mid-typecheck) never reaches `mir_struct_fields`/
        // `mir_adt_defs`, and `value_to_const_value`'s Adt lookup misses
        // for exactly the case that matters.
        if let Some(package) = self
            .state.borrow()
            .typing_ctx
            .env_ctx
            .compiled_package(&package_id)
        {
            package.borrow_mut().mir_struct_fields.extend(struct_layouts);
            package.borrow_mut().mir_adt_defs.extend(adt_defs);
            package
                .borrow_mut()
                .mir_resolved_const_values
                .extend(resolved_const_values);
        }
        let lir_id = self.lower_to_lir(
            &mir_id,
            &fqp,
            &package_id,
            &full_layouts,
            &opaque_payload_sizes,
        )?;
        let values = self.evaluate_comptime_lir(&lir_id, &fqp).await?;
        values.get(&request.expression_id).cloned().ok_or_else(|| {
            CompilerDriverError::UnresolvableComptime(format!(
                "expression {} did not produce a comptime value",
                request.expression_id
            ))
        })
    }

    /// Interprets every comptime entry in `lir_id`'s `LirProgram` for real
    /// and returns each const block's resolved value keyed by its own
    /// `HirId` (`LirComptimeEntry::const_block_hir_id`, threaded through
    /// structurally from `register_const_block_comptime_entry` via
    /// `mir::ExecutableConst`). Two callers: `resolve_comptime_values_now`
    /// (mid-typing-pass, on a scratch HIR/MIR/LIR slot, to answer pending
    /// `ComptimeRequest`s for real — see `type_check_program`) and
    /// `compile_package`'s post-typing pass (to embed real constants via
    /// `apply_resolved_comptime_block_values` + `relower_cached_lir_units`,
    /// for whichever entries the mid-pass attempts hadn't already resolved).
    async fn evaluate_comptime_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
    ) -> Result<HashMap<hir::HirId, Value>, CompilerDriverError> {
        Self::evaluate_comptime_lir_with(&self.state, lir_id, path)
    }

    /// Same as `evaluate_comptime_lir`, but against a bare
    /// `Rc<RefCell<CompilerState>>` and its own fresh, local
    /// `LirInterpreter` (not `CompilerDriver.interpreter` — every call site
    /// already resets that field unconditionally before use, so it never
    /// carried state across calls anyway) — see `lower_to_mir_with`.
    fn evaluate_comptime_lir_with(
        state: &Rc<RefCell<CompilerState>>,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
    ) -> Result<HashMap<hir::HirId, Value>, CompilerDriverError> {
        let lir = state.borrow().lir(lir_id)?.clone();
        // Only `comptime_entries` is needed after `lir` itself is moved into
        // `all_units` below — clone just that (much smaller than the whole
        // program) rather than cloning the entire `LirProgram` a second time.
        let comptime_entries = lir.comptime_entries.clone();

        let package_id = state
            .borrow()
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
            // No comptime entry needed the real interpreter, but that
            // doesn't mean nothing was computed for this package — a
            // directly-foldable top-level const (e.g. `const X = 1 + 2 *
            // 3;`, no `let` needed — see `MirLowering::lower_const`'s
            // constant-folding fast path) resolves its value without ever
            // becoming a comptime entry. Surface each one the same way an
            // interpreted entry's value already is (`insert_typing_const`,
            // populating `resolved_consts` so a caller looking up a const
            // by name — e.g. `eval_script` — finds it) instead of
            // unconditionally substituting a unit placeholder that looks
            // exactly like a real "this evaluates to nothing" result.
            let folded = state
                .borrow()
                .typing_ctx
                .env_ctx
                .compiled_package(&package_id)
                .map(|package| package.borrow().mir_resolved_const_values.clone())
                .unwrap_or_default();
            let mut last_value = None;
            for (key, constant) in &folded {
                if let Some(value) = Self::mir_constant_to_value(constant) {
                    state.borrow_mut().insert_typing_const(key.clone(), value.clone());
                    last_value = Some(value);
                }
            }
            state
                .borrow_mut()
                .insert_const_value(value_id.clone(), last_value.unwrap_or_else(Value::unit));
            return Ok(HashMap::new());
        }
        // Query each package's own `lir_workspace` directly instead of
        // cloning every artifact from every dependency into one throwaway
        // combined workspace first — `run_function_named_in_workspace`
        // now accepts a chain and checks each in turn.
        let dependency_packages: Vec<_> = state
            .borrow()
            .typing_ctx
            .env_ctx
            .crates()
            .values()
            .cloned()
            .collect();
        let dependency_borrows: Vec<_> = dependency_packages.iter().map(|p| p.borrow()).collect();
        let mut workspaces: Vec<&fp_core::lir::LirWorkspace> = dependency_borrows
            .iter()
            .map(|package| &package.lir_workspace)
            .collect();
        // This package's own just-compiled `lir` may not be stored in any
        // package's `lir_workspace` yet — fall back to a small
        // single-program workspace holding just it, same as before.
        let mut temp_workspace = None;
        if !workspaces.iter().any(|ws| {
            ws.find_function(package_id.clone(), &comptime_entries[0].function)
                .is_some()
        }) {
            let mut ws = fp_core::lir::LirWorkspace::new(lir.data_layout.clone());
            ws.add_program(package_id.clone(), path.path().clone(), lir)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            temp_workspace = Some(ws);
        }
        if let Some(ws) = temp_workspace.as_ref() {
            workspaces.insert(0, ws);
        }

        let mut count = 0usize;
        let mut last = Value::unit();
        let mut block_values: HashMap<hir::HirId, Value> = HashMap::new();
        let mut interpreter = LirInterpreter::new();
        let resolved = Self::resolved_const_values_snapshot(&state.borrow().typing_ctx);
        interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        for entry in &comptime_entries {
            let result = interpreter.run_function_named_in_workspace(
                &workspaces,
                &package_id,
                &entry.function,
            );
            let mut value =
                result.map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            let entry_lir_ty = workspaces
                .iter()
                .find_map(|ws| ws.find_function(package_id.clone(), &entry.function))
                .map(|function| function.signature.return_type.clone())
                .ok_or_else(|| {
                    CompilerDriverError::UnsupportedWork(format!(
                        "missing LIR result type for comptime entry {}",
                        entry.function
                    ))
                })?;
            value = interpreter
                .read_typed_const_value(value, &entry_lir_ty)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            // Store struct types from ALL entries, not just the last one.
            // Each const-block type alias produces its own struct.
            let entry_struct = Self::extract_struct_type(&value);
            if let Some(ref struct_ty) = entry_struct {
                let name = struct_ty.name.as_str().to_string();
                state
                    .borrow()
                    .typing_ctx
                    .resolved_types
                    .borrow_mut()
                    .insert(name.clone(), struct_ty.clone());
                state.borrow().typing_ctx.wake_comptime(&name);
            }
            let constant = Self::value_to_mir_constant(&value, &entry.ty, &dependency_packages).ok_or_else(|| {
                CompilerDriverError::UnsupportedWork(format!(
                    "unsupported comptime result for {}",
                    entry.key
                ))
            })?;
            state
                .borrow_mut()
                .insert_resolved_const_value(entry.key.clone(), constant);
            state
                .borrow_mut()
                .insert_typing_const(entry.key.clone(), value.clone());
            if let Some(hir_id) = entry.const_block_hir_id {
                block_values.insert(hir_id, value.clone());
            }
            let mut newly_resolved = HashMap::new();
            newly_resolved.insert(entry.key.clone(), value.clone());
            interpreter
                .inject_globals(&newly_resolved)
                .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
            last = value;
            count += 1;
        }

        // Store the final value for const evaluation purposes
        let _ = count;
        state.borrow_mut().insert_const_value(value_id.clone(), last);
        Ok(block_values)
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
    ) -> Result<
        (
            MirId,
            HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>>,
            HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
            HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef>,
            HashMap<String, u64>,
            HashMap<String, fp_core::mir::Constant>,
        ),
        CompilerDriverError,
    > {
        Self::lower_to_mir_with(&self.state, hir_id, path).await
    }

    /// Same as `lower_to_mir`, but against a bare `Rc<RefCell<CompilerState>>` —
    /// this is what lets the comptime-resolution task (spawned 'static,
    /// independent of whatever `&mut CompilerDriver` chain is also in
    /// progress — see `CompilerDriver::state`'s doc comment) reuse the
    /// exact same lowering logic instead of duplicating it.
    async fn lower_to_mir_with(
        state: &Rc<RefCell<CompilerState>>,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<
        (
            MirId,
            HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>>,
            HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
            HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef>,
            HashMap<String, u64>,
            HashMap<String, fp_core::mir::Constant>,
        ),
        CompilerDriverError,
    > {
        // HIR has already passed type checking at this boundary. Lowering is
        // therefore strict: a failure is an internal compiler error, never a
        // recoverable source diagnostic.
        let mut hir = state.borrow().hir(hir_id)?.clone();
        let typeck_results = state.borrow().hir_typeck(hir_id)?.clone();
        let mut lowering = MirLowering::new()
            .with_typeck_results(&typeck_results)
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR setup failed for {}: {error}",
                    path.to_key()
                ))
            })?;
        for (key, value) in state.borrow().resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let result = lowering.transform_async(hir).await;
        let mir = match result {
            Ok(mir) => mir,
            Err(error) => {
                let (diagnostics, _) = lowering.take_diagnostics();
                let details = diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.message.as_str())
                    .collect::<Vec<_>>()
                    .join("; ");
                return Err(CompilerDriverError::InternalCompilerError(if details.is_empty() {
                    format!("HIR-to-MIR lowering failed: {error}")
                } else {
                    format!("HIR-to-MIR lowering failed: {error}; diagnostics: {details}")
                }));
            }
        };
        // Run *before* the diagnostics check below — `walk_program_types_for_layouts`
        // can itself report errors (e.g. an unregistered ADT layout), and
        // those must not be silently dropped when `lowering` goes out of
        // scope at the end of this function.
        lowering.walk_program_types_for_layouts(&mir);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
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
        let adt_defs: HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef> =
            lowering.take_adt_defs();
        let struct_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> =
            lowering.all_adt_field_tys().into_iter().collect();
        let mut full_layouts: HashMap<
            (fp_core::mir::DefId, Vec<fp_core::mir::Ty>),
            Vec<fp_core::mir::Ty>,
        > = lowering
            .struct_layout_map()
                .iter()
                .map(|(key, layout)| ((key.def_id, key.args.clone()), layout.field_tys.clone()))
                .collect();
        // Enums share the same `(DefId, args)`-keyed channel as structs —
        // `mir_to_lir`'s `lir_type_from_ty` reconstructs an enum's runtime
        // shape as `{tag, ...payload slots}`, exactly mirroring
        // `EnumLayout::tag_ty`/`payload_tys` here (a mismatched/union slot
        // is an opaque placeholder at this point; sized separately via
        // `opaque_payload_sizes` below, since it has no fields of its own).
        for (key, layout) in lowering.enum_layout_map() {
            let mut fields = Vec::with_capacity(1 + layout.payload_tys.len());
            fields.push(layout.tag_ty.clone());
            fields.extend(layout.payload_tys.iter().cloned());
            full_layouts.insert((key.def_id, key.args.clone()), fields);
        }
        let opaque_payload_sizes = lowering.opaque_payload_sizes().clone();
        let resolved_const_values = lowering.take_resolved_const_values();
        let mir_id = MirId::new(format!("mir:{}", Self::module_state_key_for(state, path.path())));
        state.borrow_mut().insert_mir(mir_id.clone(), mir);
        Ok((
            mir_id,
            struct_layouts,
            full_layouts,
            adt_defs,
            opaque_payload_sizes,
            resolved_const_values,
        ))
    }

    /// Same shape as `lower_to_mir_with`, but calls
    /// `MirLowering::transform_comptime_request_async` instead of
    /// `.transform_async` — item-scoped to exactly the one pending
    /// `request` (see that method's own doc comment), never the whole
    /// package. Everything downstream (diagnostics, layout extraction) is
    /// identical and reused verbatim, since it only reads from `lowering`'s
    /// accumulated state and the returned `mir::Program`, never from
    /// `hir.items` directly.
    async fn lower_to_mir_for_comptime_request_with(
        state: &Rc<RefCell<CompilerState>>,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
        request: &fp_typing::ComptimeRequest,
    ) -> Result<
        (
            MirId,
            HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>>,
            HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
            HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef>,
            HashMap<String, u64>,
            HashMap<String, fp_core::mir::Constant>,
        ),
        CompilerDriverError,
    > {
        let hir = state.borrow().hir(hir_id)?.clone();
        let typeck_results = state.borrow().hir_typeck(hir_id)?.clone();
        let mut lowering = MirLowering::new()
            .with_typeck_results(&typeck_results)
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR setup failed for {}: {error}",
                    path.to_key()
                ))
            })?;
        for (key, value) in state.borrow().resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let result = lowering.transform_comptime_request_async(hir, request).await;
        let mir = match result {
            Ok(mir) => mir,
            Err(error) => {
                let (diagnostics, _) = lowering.take_diagnostics();
                let details = diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.message.as_str())
                    .collect::<Vec<_>>()
                    .join("; ");
                return Err(CompilerDriverError::InternalCompilerError(if details.is_empty() {
                    format!("HIR-to-MIR lowering failed: {error}")
                } else {
                    format!("HIR-to-MIR lowering failed: {error}; diagnostics: {details}")
                }));
            }
        };
        lowering.walk_program_types_for_layouts(&mir);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
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
        let adt_defs: HashMap<fp_core::hir::DefId, fp_core::mir::ty::AdtDef> =
            lowering.take_adt_defs();
        let struct_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> =
            lowering.all_adt_field_tys().into_iter().collect();
        let mut full_layouts: HashMap<
            (fp_core::mir::DefId, Vec<fp_core::mir::Ty>),
            Vec<fp_core::mir::Ty>,
        > = lowering
            .struct_layout_map()
            .iter()
            .map(|(key, layout)| ((key.def_id, key.args.clone()), layout.field_tys.clone()))
            .collect();
        for (key, layout) in lowering.enum_layout_map() {
            let mut fields = Vec::with_capacity(1 + layout.payload_tys.len());
            fields.push(layout.tag_ty.clone());
            fields.extend(layout.payload_tys.iter().cloned());
            full_layouts.insert((key.def_id, key.args.clone()), fields);
        }
        let opaque_payload_sizes = lowering.opaque_payload_sizes().clone();
        let resolved_const_values = lowering.take_resolved_const_values();
        let mir_id = MirId::new(format!("mir:{}", Self::module_state_key_for(state, path.path())));
        state.borrow_mut().insert_mir(mir_id.clone(), mir);
        Ok((
            mir_id,
            struct_layouts,
            full_layouts,
            adt_defs,
            opaque_payload_sizes,
            resolved_const_values,
        ))
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
        package_id: &PackageId,
        full_layouts: &HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
        opaque_payload_sizes: &HashMap<String, u64>,
    ) -> Result<LirId, CompilerDriverError> {
        Self::lower_to_lir_with(
            &self.state,
            mir_id,
            path,
            package_id,
            full_layouts,
            opaque_payload_sizes,
        )
    }

    /// Same as `lower_to_lir`, but against a bare `Rc<RefCell<CompilerState>>` —
    /// see `lower_to_mir_with`.
    fn lower_to_lir_with(
        state: &Rc<RefCell<CompilerState>>,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
        package_id: &PackageId,
        full_layouts: &HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
        opaque_payload_sizes: &HashMap<String, u64>,
    ) -> Result<LirId, CompilerDriverError> {
        let mir = state.borrow().mir(mir_id)?.clone();
        // Dependency packages (this package's own entry is included too,
        // and by this point already carries this call's freshly-extended
        // `mir_struct_fields`/`mir_adt_defs` — see the callers) are handed
        // to `LirGenerator` as a cheap `Rc` snapshot, queried lazily on a
        // miss by `lookup_adt_def`/`lookup_mir_layout`, instead of eagerly
        // flattening every package's struct-field layouts into one map
        // here on every call. Collected once (with package ids) and reused
        // below for `predeclare_dependency_function_signatures` too, rather
        // than taking a second separate `env_ctx.crates()` borrow/iteration
        // for the same data a few lines later.
        let dependency_packages: Vec<_> = state
            .borrow()
            .typing_ctx
            .env_ctx
            .crates()
            .iter()
            .map(|(id, package)| (id.clone(), package.clone()))
            .collect();
        let mut lowering = LirGenerator::new(state.borrow().typing_ctx.data_layout.clone())
            .with_package_id(package_id.clone())
            .with_module_path(path.path().to_key())
            .with_full_layouts(full_layouts.clone())
            .with_opaque_payload_sizes(opaque_payload_sizes.clone())
            .with_dependency_packages(
                dependency_packages
                    .iter()
                    .map(|(_, package)| package.clone())
                    .collect(),
            );
        // Thread dependency packages' compiled function signatures into
        // this generator too, mirroring the `mir_struct_fields` merge
        // above — otherwise a cross-package call (e.g. `json::parse`)
        // fails during MIR-to-LIR with "missing MIR function definition",
        // since `function_def_map` was previously only ever populated from
        // this package's own MIR.
        for (dep_id, dep_package) in &dependency_packages {
            if let Some(dep_mir) = dep_package.borrow().mir_program.as_ref() {
                lowering.predeclare_dependency_function_signatures(dep_mir, dep_id.clone());
            }
        }
        let lir = lowering.transform(mir).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "MIR-to-LIR lowering failed for {}: {error}",
                path.to_key()
            ))
        })?;
        let lir_id = Self::package_module_lir_id(package_id, path.path());
        state.borrow_mut().insert_lir(lir_id.clone(), lir);
        Ok(lir_id)
    }

    fn package_module_lir_id(package_id: &PackageId, path: &QualifiedPath) -> LirId {
        LirId::new(format!("lir:{}:{}", package_id.as_str(), path.to_key()))
    }

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        Self::resolved_const_values_snapshot(&self.state.borrow().typing_ctx)
    }

    /// Same as `collect_resolved_const_values`, but against a bare
    /// `Rc<TypingContext>` for compiler hooks that do not hold a driver.
    fn resolved_const_values_snapshot(typing_ctx: &TypingContext) -> HashMap<String, Value> {
        typing_ctx.resolved_consts.borrow().clone()
    }

    /// The reverse of `value_to_mir_constant` — needed because a
    /// directly-foldable top-level const (e.g. `const X = 1 + 2 * 3;`, no
    /// `let` needed) never becomes a comptime entry requiring the
    /// interpreter (see `lower_const`'s constant-folding fast path in
    /// hir_to_mir), so its value never reaches `resolved_consts` the way
    /// an interpreted one does unless something converts its already-
    /// computed `mir::Constant` back into a `Value` — see
    /// `resolve_folded_const_values`, the sole caller.
    fn mir_constant_to_value(constant: &mir::Constant) -> Option<Value> {
        match &constant.literal {
            mir::ConstantKind::Bool(v) => Some(Value::bool(*v)),
            mir::ConstantKind::Int(v) => Some(Value::int(*v)),
            mir::ConstantKind::UInt(v) => Some(Value::uint(*v)),
            mir::ConstantKind::Float(v) => Some(Value::decimal(*v)),
            mir::ConstantKind::Str(v) => Some(Value::string(v.clone())),
            mir::ConstantKind::Null => Some(Value::null()),
            mir::ConstantKind::Val(value) => Self::mir_const_value_to_value(value),
            // A function reference, token stream, or global-path constant
            // has no meaningful runtime `Value` representation outside
            // actual execution — an honest "can't convert this" rather
            // than a placeholder.
            mir::ConstantKind::Ty(_)
            | mir::ConstantKind::Fn(_)
            | mir::ConstantKind::FnDef(_, _)
            | mir::ConstantKind::Global(_)
            | mir::ConstantKind::TokenStream { .. }
            | mir::ConstantKind::Undef => None,
        }
    }

    fn mir_const_value_to_value(value: &mir::ConstValue) -> Option<Value> {
        match value {
            mir::ConstValue::Unit => Some(Value::unit()),
            mir::ConstValue::Bool(v) => Some(Value::bool(*v)),
            mir::ConstValue::Int(v) => Some(Value::int(*v)),
            mir::ConstValue::UInt(v) => Some(Value::uint(*v)),
            mir::ConstValue::Float(v) => Some(Value::decimal(*v)),
            mir::ConstValue::Str(v) => Some(Value::string(v.clone())),
            mir::ConstValue::Null => Some(Value::null()),
            // The comptime interpreter represents every positional
            // aggregate (tuple *or* struct) as `Value::Tuple` — see
            // `value_to_const_value`'s own doc comment on the same
            // asymmetry in the forward direction.
            mir::ConstValue::Tuple(values) | mir::ConstValue::Struct(values) => {
                let values = values
                    .iter()
                    .map(Self::mir_const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(Value::Tuple(fp_core::ast::ValueTuple::new(values)))
            }
            mir::ConstValue::Array(values) => {
                let values = values
                    .iter()
                    .map(Self::mir_const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(Value::List(fp_core::ast::ValueList::new(values)))
            }
            mir::ConstValue::List { elements, .. } => {
                let values = elements
                    .iter()
                    .map(Self::mir_const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(Value::List(fp_core::ast::ValueList::new(values)))
            }
            // No `Value::Map` constructor exists to convert into (see
            // `all_adt_field_tys`'s neighbors) — an honest "can't convert
            // this" rather than a placeholder.
            mir::ConstValue::Map { .. } => None,
            // A function reference has no meaningful runtime `Value`
            // representation outside actual execution.
            mir::ConstValue::Fn(_) => None,
        }
    }

    /// The authoritative source for an Adt's real field list — populated by
    /// `fp-backend`'s `take_adt_defs()` specifically so a downstream
    /// consumer with no live `MirLowering` (like this one) can look it up.
    /// Never use `Ty::Adt(adt_def, _).variants` directly: it's deliberately
    /// left empty by several real construction paths (`adt_shell_ty`, the
    /// general Adt case in `lower_hir_ty`) that only ever needed to convey
    /// type *identity*, not full field layout.
    fn lookup_real_adt_def(
        packages: &[Rc<RefCell<fp_core::package::CompiledPackage>>],
        def_id: hir::DefId,
    ) -> Option<mir::ty::AdtDef> {
        packages
            .iter()
            .find_map(|p| p.borrow().mir_adt_defs.get(&def_id).cloned())
    }

    fn value_to_mir_constant(
        value: &Value,
        ty: &mir::Ty,
        packages: &[Rc<RefCell<fp_core::package::CompiledPackage>>],
    ) -> Option<mir::Constant> {
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
            _ => mir::ConstantKind::Val(Self::value_to_const_value(value, ty, packages)?),
        };
        Some(mir::Constant {
            span: Span::null(),
            ty: ty.clone(),
            user_ty: None,
            literal,
        })
    }

    fn value_to_const_value(
        value: &Value,
        ty: &mir::Ty,
        packages: &[Rc<RefCell<fp_core::package::CompiledPackage>>],
    ) -> Option<mir::ConstValue> {
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
            // A raw pointer's comptime value is just its address — e.g.
            // `Vec::new()`'s `ptr: *mut T` field, always null before any
            // allocation happens. There's no dedicated pointer/address
            // `ConstValue` variant, so mirror `Value::Null`'s treatment
            // for a null address (the only case that can arise from a
            // `const`/`const fn` evaluation — a real heap/stack address
            // from a genuinely *runtime* allocation has no meaningful
            // representation as a compile-time constant at all) and
            // otherwise surface the address as a plain integer.
            Value::Pointer(pointer) => Some(if pointer.value == 0 {
                mir::ConstValue::Null
            } else {
                mir::ConstValue::UInt(pointer.value as u64)
            }),
            // `fp-interpret` stores every register-resident aggregate as a
            // plain `Value::Tuple` regardless of its nominal type (structs
            // included — see `default_value_for_type`/`load_value_at`), so
            // a struct/enum-typed comptime result (e.g. `Vec::new()`'s
            // `Vec<T>{ptr,len,capacity}`) arrives here as `Value::Tuple`
            // even though `ty.kind` is `TyKind::Adt`, not `TyKind::Tuple`.
            // Mirror the `Value::Struct`/`TyKind::Adt` arm below rather
            // than rejecting it.
            Value::Tuple(tuple) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if tuple.values.len() != fields.len() {
                        return None;
                    }
                    let values = tuple
                        .values
                        .iter()
                        .zip(fields.iter())
                        .map(|(value, field_ty)| Self::value_to_const_value(value, field_ty, packages))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Tuple(values))
                }
                // Never derive field info from `adt_def.variants` directly
                // (see `lookup_real_adt_def`'s doc comment) — look up the
                // real, registered `AdtDef` instead, and convert each field
                // against its own declared `Ty` rather than blindly
                // guessing (the previous untyped conversion always
                // produced a signed `Int` even for an unsigned field).
                TyKind::Adt(adt_def, _substs) => {
                    let variant = Self::lookup_real_adt_def(packages, adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if tuple.values.len() != variant.fields.len() {
                        return None;
                    }
                    let values = tuple
                        .values
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(value, field_def)| {
                            Self::value_to_const_value(value, &field_def.ty, packages)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            Value::List(list) => match &ty.kind {
                TyKind::Array(elem_ty, _) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| Self::value_to_const_value(value, elem_ty, packages))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Array(values))
                }
                TyKind::Slice(elem_ty) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| Self::value_to_const_value(value, elem_ty, packages))
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
                        .map(|(field, field_ty)| {
                            Self::value_to_const_value(&field.value, field_ty, packages)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = Self::lookup_real_adt_def(packages, adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if value_struct.structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(field, field_def)| {
                            Self::value_to_const_value(&field.value, &field_def.ty, packages)
                        })
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
                        .map(|(field, field_ty)| {
                            Self::value_to_const_value(&field.value, field_ty, packages)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = Self::lookup_real_adt_def(packages, adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(field, field_def)| {
                            Self::value_to_const_value(&field.value, &field_def.ty, packages)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            _ => None,
        }
    }

}

/// The name an `ast::Item` is registered under during AST→HIR lowering
/// (`HirGenerator::predeclare_items`'s `register_type_def`/
/// `register_value_def` calls) — `None` for item kinds with no name of
/// their own (`Impl`, `Import`, bare `Expr`, ...), which therefore can
/// never be looked up in a qualified-path-keyed map.
fn item_own_name(item: &Item) -> Option<&str> {
    match item.kind() {
        ItemKind::Module(module) => Some(module.name.name.as_str()),
        ItemKind::DefStruct(def) => Some(def.name.name.as_str()),
        ItemKind::DefStructural(def) => Some(def.name.name.as_str()),
        ItemKind::DefEnum(def) => Some(def.name.name.as_str()),
        ItemKind::DefType(def) => Some(def.name.name.as_str()),
        ItemKind::OpaqueType(def) => Some(def.name.name.as_str()),
        ItemKind::DefConst(def) => Some(def.name.name.as_str()),
        ItemKind::DefStatic(def) => Some(def.name.name.as_str()),
        ItemKind::DefFunction(def) => Some(def.name.name.as_str()),
        ItemKind::DefTrait(def) => Some(def.name.name.as_str()),
        ItemKind::DeclType(def) => Some(def.name.name.as_str()),
        ItemKind::DeclConst(def) => Some(def.name.name.as_str()),
        ItemKind::DeclStatic(def) => Some(def.name.name.as_str()),
        ItemKind::DeclFunction(def) => Some(def.name.name.as_str()),
        ItemKind::Macro(item_macro) => item_macro.declared_name.as_ref().map(|ident| ident.name.as_str()),
        ItemKind::Impl(_)
        | ItemKind::Import(_)
        | ItemKind::Expr(_)
        | ItemKind::ConstBlock(_)
        | ItemKind::PrecompiledAsm(_) => None,
    }
}

/// An `impl` block's own qualified name is its self-type's name (mirrors
/// `ast_to_hir::self_type_first_segment_name`, which resolves the same
/// question at HIR-lowering time) — used to reconstruct the same
/// `DefPath` shape `HirToAstLifter::lift_impl_methods_by_path` records
/// each method's typed body under, from the untyped source alone. Only
/// handles the common bare-ident/simple-path shape (`impl Foo { .. }`/
/// `impl foo::Bar { .. }`) — a self-type written as something
/// structurally different (`&T`, `[T]`, a generic instantiation, ...)
/// returns `None`, and that impl's methods simply keep their original,
/// untyped source form, the same fallback every other unmatched case
/// already gets.
fn impl_self_type_name(self_ty: &Expr) -> Option<&str> {
    let ExprKind::Name(name) = self_ty.kind() else {
        return None;
    };
    match name {
        fp_core::ast::Name::Ident(ident) => Some(ident.name.as_str()),
        fp_core::ast::Name::Path(path) => path.segments.last().map(|seg| seg.name.as_str()),
        fp_core::ast::Name::ParameterPath(param_path) => {
            param_path.segments.last().map(|seg| seg.ident.name.as_str())
        }
    }
}
