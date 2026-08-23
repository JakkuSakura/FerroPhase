use fp_backend::transformations::{
    AstToHirLowerer, HirLoweringConfig, MirToLirLowerer, LirToMir, HirToMirLowerer, MirToHir,
};
use fp_core::ast::{Expr, ExprKind, Item, ItemKind, Value};
use fp_core::hir;
use fp_core::mir;
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::package::{DependencyDescriptor, DependencyKind, PackageId};
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel};
use fp_interpret::LirInterpreter;
use fp_lang::FerroIntrinsicNormalizer;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{CompilerDriverError, CompilerState, ConstValueId, ExecutorHandle};

/// Real Rust source over an entire vendored `std`/`core`/`alloc` package
/// legitimately produces tens of thousands of "skipping unresolvable
/// type"-style *warnings* from `HirToMirLowerer` (one per HIR node whose type
/// couldn't be lowered) — joining every one of those into a single
/// `InternalCompilerError` message (as this used to) built a single
/// multi-hundred-megabyte string per failed package, which is what real
/// compilers never do (diagnostics are reported through their own channel,
/// not embedded whole into a top-level error's `Display`). Only genuine
/// `Error`-level diagnostics belong in the error text, and even those are
/// capped — a caller wants "what broke", not a full warning transcript.
fn diagnostics_summary(diagnostics: &[Diagnostic]) -> String {
    const MAX_SHOWN: usize = 10;
    let errors: Vec<&str> = diagnostics
        .iter()
        .filter(|d| d.level == DiagnosticLevel::Error)
        .map(|d| d.message.as_str())
        .collect();
    if errors.is_empty() {
        return String::new();
    }
    let shown = errors.iter().take(MAX_SHOWN).cloned().collect::<Vec<_>>().join("; ");
    if errors.len() > MAX_SHOWN {
        format!("{shown}; ... and {} more error(s)", errors.len() - MAX_SHOWN)
    } else {
        shown
    }
}

pub struct CompilerDriver {
    /// `Rc<RefCell<_>>`, not owned: a spawned comptime-resolution task (see
    /// `CompilerState::comptime_resolver`, `type_check_program`) needs to
    /// reach the same HIR/MIR/LIR state independently of whatever `&mut
    /// self`-holding future is already driving `compile_package`/
    /// `compile_native` at the time — that future's `&mut self` borrow
    /// lasts for its entire lifetime (how `async fn` desugars), so a
    /// `'static` task closure cannot also borrow `self` directly. Sharing
    /// just `state` this way (not `interpreter`, `building_packages`, etc.
    /// — those aren't needed by anything spawned as a task) keeps the rest
    /// of `CompilerDriver` an ordinary `&mut self`-based type.
    pub state: Rc<RefCell<CompilerState>>,
    interpreter: LirInterpreter,
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::ast::package::AstPackage>>>,
    next_hir_def_id: u32,
    pub pipeline: PipelineMode,
}

/// Controls how far the compiler pipeline runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineMode {
    /// Full native compilation: AST → HIR → MIR → LIR
    Native,
    /// HIR typing + lift back to AST: AST → HIR → typing → AST
    Transpile,
}

impl CompilerDriver {
    pub fn new(data_layout: fp_core::lir::LirDataLayout, tasks: ExecutorHandle) -> Self {
        Self::with_workspace(
            data_layout,
            tasks,
            Rc::new(fp_core::ast::program::AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
        )
    }

    pub fn with_workspace(
        data_layout: fp_core::lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<fp_core::ast::program::AstProgram>,
    ) -> Self {
        let state = CompilerState::with_workspace(data_layout, tasks, workspace);
        Self::from_state_rc(Rc::new(RefCell::new(state)))
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self::from_state_rc(Rc::new(RefCell::new(state)))
    }

    /// Shared tail of `with_workspace`/`with_state` — wires up
    /// `CompilerState::comptime_resolver` (see `make_comptime_resolver`)
    /// against this driver's own `state`, which can only happen once
    /// `state` is behind the same `Rc<RefCell<_>>` every other
    /// comptime-resolution call site closes over. The resolver itself is
    /// stateless (it resolves purely off a request's own `def_id`), so one
    /// built here is reused for every package this driver ever compiles —
    /// no per-package reconstruction needed.
    fn from_state_rc(state: Rc<RefCell<CompilerState>>) -> Self {
        let resolver = Self::make_comptime_resolver(&state);
        state.borrow_mut().comptime_resolver = Some(resolver);
        Self {
            state,
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
    ) -> Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> {
        self.compile_package(package_id).await
    }

    pub async fn compile_bytecode(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        self.compile_package(package_id).await?;
        let mir = self.state.borrow().mir_module(package_id);
        if mir.items.is_empty() {
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "package {package_id} has no MIR program"
            )));
        }
        fp_bytecode::lower_program(&mir).map_err(CompilerDriverError::from)
    }

    pub fn execute_runtime(
        &mut self,
        lir_path: &fp_core::lir::LirPath,
    ) -> Result<fp_core::ast::Value, CompilerDriverError> {
        let entrypoint = self.state.borrow().runtime_entrypoint(lir_path)?;
        // `LirPath` already carries the real package id directly (unlike
        // the old string-keyed `LirId`, which needed parsing back out of
        // `"lir:{package_id}:{path}"`) — recovering it is what lets calls
        // to any function other than the entrypoint itself resolve; see
        // `run_entrypoint_with_package`'s doc comment.
        let lir = self
            .state
            .borrow()
            .runtime_blob(&lir_path.package_id, lir_path, entrypoint)?;
        self.interpreter = LirInterpreter::new();
        let resolved = self.collect_resolved_const_values();
        self.interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let value = self.interpreter.run_entrypoint_with_package(
            &lir,
            entrypoint,
            lir_path.package_id.clone(),
        )?;
        Ok(value)
    }

    /// Resolves the `DefId` of the function named `function_name` anywhere
    /// in `package_id`'s published HIR — package-based, not module-based
    /// (see `fp_core::ast::package::resolve_entrypoint_def_id`'s doc comment).
    /// `module_path` isn't used for this resolution itself; it's taken here
    /// only because every caller already has one on hand for the sibling
    /// LIR-lookup/comptime-path purposes `select_entrypoint`/
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
            .workspace
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let hir_package_id = package.borrow().hir_package_id;
        let hir_package = self.state.borrow().hir(hir_package_id)?;
        fp_core::ast::package::resolve_entrypoint_def_id(package_id, &hir_package, function_name)
            .map_err(|error| CompilerDriverError::Interpreter(error.to_string()))
    }

    /// The process entry point is located downstream (native/asm emission)
    /// by its final, bare symbol name — a linkage requirement, not a
    /// display convention. Normal mangling gives a module-nested `main` a
    /// qualified name (e.g. `module__main`), but the OS/runtime always
    /// calls the bare `main`, so the resolved entrypoint's own
    /// `LirCodeUnit` needs renaming back to the name it was looked up by,
    /// regardless of its module qualification. Operates on the one
    /// function directly (not a whole `LirBlob`) — see
    /// `CompilerState::insert_runtime_program`'s doc comment for why only
    /// this one function needs its own storage.
    fn rename_lir_function_unit(mut unit: fp_core::lir::LirCodeUnit, bare_name: &str) -> fp_core::lir::LirCodeUnit {
        if let fp_core::lir::LirCodeUnitKind::Function(function) = &mut unit.kind {
            function.name = fp_core::lir::Name::new(bare_name.to_string());
        }
        unit
    }

    pub fn select_entrypoint(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<fp_core::lir::LirPath, CompilerDriverError> {
        let function = self.resolve_entrypoint_def_id(package_id, module_path, function_name)?;
        let lir_path = fp_core::lir::LirPath::new(package_id.clone(), module_path.clone());
        let lir_function = self
            .state
            .borrow()
            .lir_blob(package_id)
            .functions
            .into_iter()
            .find(|candidate| candidate.def_id == Some(function))
            .ok_or_else(|| {
                CompilerDriverError::Interpreter(format!(
                    "entrypoint {function} was not emitted"
                ))
            })?;
        let unit = fp_core::lir::LirCodeUnit {
            package_id: package_id.clone(),
            module_path: module_path.clone(),
            kind: fp_core::lir::LirCodeUnitKind::Function(lir_function),
        };
        let unit = Self::rename_lir_function_unit(unit, function_name);
        self.state.borrow_mut().insert_runtime_program(lir_path.clone(), unit);
        self.state.borrow_mut()
            .insert_runtime_entrypoint(lir_path.clone(), function);
        Ok(lir_path)
    }

    pub async fn compile_package_module_native(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        function_name: &str,
    ) -> Result<(), CompilerDriverError> {
        let lir_path = self.select_entrypoint(package_id, module_path, function_name)?;
        let block_values = self.evaluate_comptime_lir(package_id, module_path).await?;
        // Mirrors `compile_package`'s dependency-loading branch (see its
        // identical three-call sequence): `evaluate_comptime_lir` computes
        // each `const { .. }` block's real value into `block_values`, but
        // that alone never reaches the executable blob — the block's
        // owning item (e.g. a `const X = const { .. };`) was already
        // lowered to an `ExecutableConst`/`LirComptimeEntry` *before* its
        // value was known, not a `LirGlobal`. Re-lowering HIR->MIR->LIR now
        // that `apply_resolved_comptime_block_values` has recorded the
        // answer lets `lower_const_expr` constant-fold the const item for
        // real this time, materializing the missing global. Without this,
        // the const item's global is simply absent, and running it fails
        // at runtime with "missing global" — never a compile-time error,
        // since nothing upstream of execution ever notices the gap.
        if !block_values.is_empty() {
            let package = self
                .state
                .borrow()
                .workspace
                .compiled_package(package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
            let hir_package_id = package.borrow().hir_package_id;
            self.apply_resolved_comptime_block_values(hir_package_id, &block_values)?;
            self.relower_cached_lir_units(package_id, hir_package_id).await?;
        }
        let _ = lir_path;
        Ok(())
    }

    /// Compile a package after recursively compiling its declared
    /// dependencies. Dependency resolution and version selection happen in
    /// the provider; the driver only consumes the concrete package IDs it is
    /// given by metadata.
    pub async fn compile_package(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> {
        let parent_workspace = self.state.borrow().workspace.clone();
        if let Some(package) = self.compiled_packages.get(package_id).cloned() {
            parent_workspace.import_package(package_id.clone(), package.clone());
            return Ok(package);
        }
        if let Some(package) = parent_workspace.compiled_package(package_id) {
            return Ok(package);
        }
        if !self.building_packages.insert(package_id.clone()) {
            return Err(CompilerDriverError::UnresolvablePackage(format!(
                "dependency cycle involving {package_id}"
            )));
        }

        let package_workspace = parent_workspace.for_package(package_id.clone());
        {
            let mut state = self.state.borrow_mut();
            state.workspace = Rc::new(package_workspace);
        }

        let result: Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> =
            async {
                let provider = self
                    .state.borrow()
                    .workspace
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
                // A `PrecompiledLir` item (see `fp_core::ast::ItemKind`'s doc
                // comment) already *is* LIR — install it into
                // `state.lir_program` directly via `publish_precompiled_lir`
                // instead of running it through `AstToHirLowerer` (which has no
                // arm for an already-compiled item and would just record a
                // spurious "unimplemented" diagnostic for it).
                let precompiled_lir_units: Vec<fp_core::lir::LirCompileUnit> = source
                    .items
                    .iter()
                    .filter_map(|pkg_item| match pkg_item.item.kind() {
                        fp_core::ast::ItemKind::PrecompiledLir(lir) => {
                            Some(fp_core::lir::LirCompileUnit {
                                package_id: fp_core::hir::PackageId::default(),
                                module_path: pkg_item.module_path.clone(),
                                blob: lir.clone(),
                            })
                        }
                        _ => None,
                    })
                    .collect();
                let package = self.state.borrow().workspace.begin_package(
                    package_id.clone(),
                    source,
                    self.state.borrow().data_layout.clone(),
                );
                if !precompiled_lir_units.is_empty() {
                    Self::publish_precompiled_lir(&self.state, package_id, &precompiled_lir_units)?;
                } else if matches!(self.pipeline, PipelineMode::Native | PipelineMode::Transpile) {
                    // `Transpile` needs HIR generation + typing too (it lifts the
                    // typed HIR back to AST inside `compile_items_to_lir_units`) — it now also
                    // attempts MIR/LIR lowering there so any comptime entries (e.g. `const
                    // { .. }` blocks) get resolved and relowered the same way `Native` does,
                    // below — every target needs the resolved value, not just the block's
                    // type, so both pipeline modes handle a comptime failure identically.
                    self.compile_items_to_lir_units(&package).await?;
                    let comptime_entries = self.state.borrow().lir_blob(package_id).comptime_entries;
                    if !comptime_entries.is_empty() {
                        let hir_package_id = package.borrow().hir_package_id;
                        // Both pipeline modes need the resolved value
                        // relowered back into a real `LirGlobal` (see this
                        // arm's own doc comment above) — a failed comptime
                        // block is a genuine compile error in either mode,
                        // not something to downgrade to a warning in one
                        // and not the other.
                        let block_values = self.evaluate_comptime_lir(package_id, &QualifiedPath::new(Vec::new())).await?;
                        if !block_values.is_empty() {
                            self.apply_resolved_comptime_block_values(hir_package_id, &block_values)?;
                            self.relower_cached_lir_units(package_id, hir_package_id).await?;
                        }
                    }
                }
                Ok(package)
            }
            .await;

        self.building_packages.remove(package_id);
        {
            let mut state = self.state.borrow_mut();
            state.workspace = parent_workspace.clone();
        }
        let package = result?;
        self.compiled_packages
            .insert(package_id.clone(), package.clone());
        parent_workspace.import_package(package_id.clone(), package.clone());
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
                    .workspace
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
    /// each member's result via `AstProgram::package_source` rather
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

    /// Installs a pre-baked `LirBlob` (from a `PrecompiledLir` item) into
    /// `state.lir_program` directly, bypassing the whole HIR->MIR->LIR
    /// pipeline that every other package goes through — there's nothing to
    /// lower, the blob already *is* the package's LIR.
    fn publish_precompiled_lir(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        units: &[fp_core::lir::LirCompileUnit],
    ) -> Result<(), CompilerDriverError> {
        for unit in units {
            state
                .borrow_mut()
                .insert_lir_blob(package_id, unit.module_path.clone(), unit.blob.clone())?;
        }
        Ok(())
    }

    /// Runs a whole package's HIR generation + typing, then per-`DefId`
    /// HIR->MIR->LIR lowering (`lower_package_to_lir_with`) — every step one
    /// `DefId` in, one unit out, stored straight into the shared
    /// `CompilerState` as it goes (see this module's own data-flow doc
    /// comment for the full picture). Never predeclares/lowers the whole
    /// package as one flat blob.
    async fn compile_items_to_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::ast::package::AstPackage>>,
    ) -> Result<(), CompilerDriverError> {
        let hir_package_id = package.borrow().hir_package_id;
        let current_package_id = package.borrow().package_id.clone();
        let mut package_source = package.borrow().clone();
        let macro_rules_defs =
            fp_lang::collect_macro_rules_defs(package_source.items.iter().map(|item| &item.item));
        // Item-position macro invocations (e.g. `make_adder!(add_two, 2);`)
        // must expand into real items *before* `AstToHirLowerer` ever sees
        // them — matching rustc's own model, where macro-expanded tokens
        // re-enter the exact same pipeline as hand-written code rather than
        // a separate, lesser one. Without this, such an invocation is
        // silently dropped by `ast_to_hir`'s own item loop, and whatever it
        // would have defined never exists.
        package_source.items = fp_lang::expand_item_macros(package_source.items, &macro_rules_defs);
        let mut generator = AstToHirLowerer::new(hir_package_id)
            .with_intrinsic_normalizer(
                FerroIntrinsicNormalizer::new(fp_core::intrinsics::IntrinsicNormalizationMode::Compile)
                    .with_macro_rules_defs(macro_rules_defs),
            )
            .with_def_id_start(self.next_hir_def_id)
            .with_lowering_config(HirLoweringConfig {
                // The active `TargetBackend`'s own capabilities (see
                // `fp_core::backend::TargetBackend::capabilities`), set by
                // `fp-cli` before compiling via
                // `CompilerState::set_backend_capabilities` — defaults to
                // `NATIVE` (nothing first-class) for any caller that never
                // sets it, matching this field's prior behavior exactly.
                capabilities: self.state.borrow().backend_capabilities(),
            })
            .with_workspace(self.state.borrow().workspace.clone());
        let hir_program = generator.transform_package(&package_source)?;
        self.next_hir_def_id = self.next_hir_def_id.max(generator.next_def_id_value());
        let package_exports = generator.exported_symbols();
        let type_alias_exports = generator.exported_type_aliases();
        package.borrow_mut().type_alias_exports.extend(type_alias_exports);
        let mut hir_program_typed = self.type_check_program(hir_program).await.map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "package HIR type checking failed: {error}"
            ))
        })?;
        // Formalized post-typecheck `#[op(...)]` promotion (see
        // `hir_normalization` for the full rationale): only
        // `Transpile` (Kotlin/Shell AST-lift) promotes
        // pure-`Op`-only calls to `IntrinsicCall(CallKind::Op(..))` in
        // place. `Native` leaves them as ordinary calls to their real stub
        // bodies (see `hir_materialization` for why that's correct here).
        let promote_op_only = matches!(self.pipeline, PipelineMode::Transpile);
        fp_backend::transforms::hir_normalization::normalize_program(&mut hir_program_typed, promote_op_only);
        hir_program_typed.hir_exports.extend(package_exports);
        self.state.borrow_mut().insert_hir(hir_program_typed);

        // Transpile: lift typed HIR back to AST — this is what the Kotlin
        // backend actually reads, and doesn't depend on anything below
        // succeeding.
        if self.pipeline == PipelineMode::Transpile {
            let package_path = QualifiedPath::new(Vec::new());
            // Scoped narrowly — `lift_items_by_path`/`referenced_paths_by_path`
            // return owned data, so nothing here needs to outlive this block.
            let (lifted_items_by_path, referenced_paths_by_path) = {
                let state = self.state.borrow();
                let hir = state.hir(hir_package_id)?;
                let lifter = fp_backend::transforms::HirToAstLifter::new(
                    &hir,
                    Some(state.hir_program()),
                );
                // `lift_items_by_path` treats an `impl` block as an opaque
                // placeholder — merge in each impl *method*'s own lifted
                // body too (keyed by its own qualified path, disjoint from
                // any top-level item's), or typed/normalized impl method
                // bodies never get spliced back in at all.
                let mut lifted_items_by_path = lifter.lift_items_by_path();
                lifted_items_by_path.extend(lifter.lift_impl_methods_by_path());
                lifter.publish_resolved_expr_types();
                (lifted_items_by_path, lifter.referenced_paths_by_path())
            };
            if let Some(pkg) = self
                .state.borrow()
                .workspace
                .compiled_package(&current_package_id)
            {
                let mut pkg = pkg.borrow_mut();
                // Splice typed/normalized content back onto the original
                // untyped source items by qualified-path identity — the
                // single, canonical reconciliation point.
                for pkg_item in &mut pkg.items {
                    if let ItemKind::Impl(imp) = pkg_item.item.kind_mut() {
                        // Trait impls aren't in `lifted_items_by_path` at
                        // all (`lift_impl_methods_by_path` skips them) —
                        // only attempt this for inherent impls, matching
                        // what was actually lifted.
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
                }
                pkg.referenced_paths = referenced_paths_by_path
                    .into_iter()
                    .map(|(key, values)| {
                        let key: Vec<String> = key.segments.iter().map(|s| s.as_str().to_string()).collect();
                        let values: Vec<Vec<String>> = values
                            .into_iter()
                            .map(|path| path.segments.iter().map(|s| s.as_str().to_string()).collect())
                            .collect();
                        (key, values)
                    })
                    .collect();
            }
            // Best-effort HIR->MIR->LIR lowering, purely so any
            // `const { .. }` blocks get validated later through the real
            // interpreter (`evaluate_comptime_lir`) instead of a hand-rolled
            // one. MIR lowering was built for the Native pipeline and may
            // not yet cover every construct real vendored std/workspace
            // code exercises — a failure here is reported and this package
            // simply produces no LIR (comptime validation is skipped for
            // it), matching this pipeline's prior behavior; the lifted AST
            // above is already complete regardless.
            if let Err(error) =
                Self::lower_package_to_lir_with(&self.state, &current_package_id, hir_package_id, package_path)
                    .await
            {
                fp_core::diagnostics::report_warning_with_context(
                    "const-eval".to_string(),
                    format!(
                        "HIR->MIR/LIR lowering failed, skipping comptime validation for this package: {error}"
                    ),
                );
            }
            return Ok(());
        }

        let module_path = QualifiedPath::new(Vec::new());
        Self::lower_package_to_lir_with(&self.state, &current_package_id, hir_package_id, module_path).await
    }

    /// Writes `evaluate_comptime_lir`'s real, interpreter-computed values
    /// back onto this package's own `HirPackage::const_block_values`
    /// (keyed by each block's own `DefId`), before `relower_cached_lir_units`
    /// re-lowers HIR->MIR a second time. That second lowering pass already
    /// has a fast path (`typeck_const_block_value`, `hir_to_mir/expr.rs`'s
    /// `ConstBlock` arm) that embeds a real constant
    /// when a value is present there and otherwise falls back to lowering
    /// the block as ordinary runtime code — this is what turns that
    /// fallback into a real compile-time constant, without needing typing
    /// itself to ever suspend on the value.
    fn apply_resolved_comptime_block_values(
        &mut self,
        hir_package_id: hir::PackageId,
        block_values: &HashMap<hir::DefId, Value>,
    ) -> Result<(), CompilerDriverError> {
        let package = self.state.borrow().hir(hir_package_id)?;
        for (def_id, value) in block_values {
            package.record_const_block_value(*def_id, value.clone());
        }
        self.state.borrow_mut().insert_hir(package);
        Ok(())
    }

    /// Re-runs `lower_package_to_lir_with` for `package_id` now that
    /// `apply_resolved_comptime_block_values` has recorded a real value for
    /// each `const { .. }` block — every `DefId` is re-lowered against the
    /// now-resolved typeck results, replacing its previous unit in place
    /// (`insert_mir_unit`) and its previous LIR artifacts
    /// (`lower_package_to_lir_with` resets the package's LIR table first, so
    /// a repeat lowering never collides with the stale one it's replacing).
    async fn relower_cached_lir_units(
        &mut self,
        package_id: &PackageId,
        hir_package_id: hir::PackageId,
    ) -> Result<(), CompilerDriverError> {
        let module_path = QualifiedPath::new(Vec::new());
        Self::lower_package_to_lir_with(&self.state, package_id, hir_package_id, module_path).await
    }

    /// Type-checks `program`. Each `const { .. }` block's `ComptimeRequest`
    /// (`hir_typeck.rs`'s two `ConstBlock` arms, both genuinely `.await` it)
    /// is answered with a *real* value computed by the interpreter, never a
    /// placeholder, via `CompilerState::comptime_resolver`
    /// (`make_comptime_resolver`) — so an item's task suspends on it and
    /// resumes naturally, exactly like any other `.await`, with no
    /// driver-side polling loop involved. A request's own failure only
    /// fails the specific item awaiting it (per-item isolation, matching
    /// `typecheck_item`), not the whole package. Same-package items awaiting
    /// each other (a `const` referencing another item declared later in
    /// `program.items`) resolve regardless of textual order via
    /// `fp_typing::HirTypeChecker::spawn_item_task`; a genuine dependency cycle among them
    /// (impossible to make progress on) surfaces as the ambient
    /// `CompilerExecutor::run` driving this whole call stalling, not as a
    /// return from here.
    async fn type_check_program(
        &mut self,
        program: hir::HirPackage,
    ) -> fp_core::Result<hir::HirPackage> {
        let comptime_resolver = self.state.borrow().comptime_resolver.clone();
        let dependency_program = Rc::new(self.state.borrow().hir_program().clone());
        let executor = self.state.borrow().tasks.clone();
        let checker =
            fp_typing::HirTypeChecker::new(program, Some(dependency_program), comptime_resolver, executor);
        // Published for the duration of this package's typecheck so a
        // mid-typecheck `ComptimeRequest` (which only names its own
        // `package_id`/`def_id`, see `fp_typing::ComptimeRequest`'s doc
        // comment) can resolve this still-unpublished package back by id —
        // cleared below regardless of how this function returns.
        self.state
            .borrow_mut()
            .set_in_progress_hir_program(Some(checker.borrow().program_handle()));
        let item_ids: Vec<_> = checker
            .borrow()
            .package()
            .items
            .iter()
            .map(|item| item.def_id)
            .collect();
        let handles: Vec<_> = item_ids
            .into_iter()
            .map(|def_id| fp_typing::HirTypeChecker::spawn_item_task(&checker, def_id))
            .collect();
        for handle in handles {
            handle.await;
        }
        self.state.borrow_mut().set_in_progress_hir_program(None);
        // rustc-style `tcx.sess.has_errors()` gate: a per-item task that hit
        // a real typecheck error still resolves its own `TaskHandle`
        // successfully (see `typecheck_item`'s deliberate per-item isolation
        // in `fp-typing`), so without this check the resulting, incomplete
        // typed results would be handed straight to HIR->MIR lowering —
        // whose own, unrelated failure (triggered by the exact gap this
        // item's aborted check left behind) would then mask the real,
        // specific diagnostic recorded here.
        if checker.borrow().has_typing_errors() {
            let package = checker.borrow().finish();
            Self::emit_typing_diagnostics_to_stderr(&package);
            let combined = package
                .diagnostics
                .get_diagnostics()
                .iter()
                .filter(|diagnostic| {
                    diagnostic.code.as_deref() == Some(fp_typing::context::ITEM_CHECK_FAILURE_CODE)
                })
                .map(|diagnostic| diagnostic.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            return Err(fp_core::error::Error::diagnostic(
                fp_core::diagnostics::Diagnostic::error(combined),
            ));
        }
        let package = checker.borrow().finish();
        // `checker` is the last other strong owner of this `Rc<HirPackage>`
        // (via its own `program.packages` map) — dropping it here, before
        // unwrapping, lets the caller take real ownership without ever
        // deep-copying the package's own data.
        drop(checker);
        Ok(Rc::try_unwrap(package).unwrap_or_else(|_| {
            unreachable!("no other strong reference to this package's HirPackage should outlive its own typecheck pass")
        }))
    }

    /// Prints every diagnostic accumulated on `package` so far to stderr, one
    /// per line — both the hard item-check aborts and every other
    /// recovered/non-fatal mismatch recorded along the way (all in the one
    /// unified `diagnostics` manager), since either category can be the
    /// real lead on why a package's typecheck ultimately failed or stalled.
    fn emit_typing_diagnostics_to_stderr(package: &hir::HirPackage) {
        let diagnostics = package.diagnostics.get_diagnostics();
        if diagnostics.is_empty() {
            return;
        }
        eprintln!(
            "fp-compiler: {} typing diagnostic(s) recorded before failure:",
            diagnostics.len()
        );
        for diagnostic in &diagnostics {
            eprintln!("  {}", diagnostic);
        }
    }

    /// Builds the `fp_typing::ComptimeResolver` a package's typecheck
    /// awaits directly from `request_comptime` — this is what lets an
    /// item's typecheck task suspend on a real `const { .. }` block and
    /// resume naturally once it's answered, with no driver-side polling
    /// loop pumping a request queue. Only needs `state` (not a full
    /// `CompilerDriver`, so it can be captured by a `'static` closure and
    /// handed to `fp-typing` independent of any particular
    /// `&mut CompilerDriver` call).
    fn make_comptime_resolver(state: &Rc<RefCell<CompilerState>>) -> fp_typing::ComptimeResolver {
        let state = state.clone();
        Rc::new(move |request: fp_typing::ComptimeRequest| {
            let state = state.clone();
            Box::pin(async move {
                Self::resolve_comptime_request_with(&state, request)
                    .await
                    .map_err(|error| fp_core::error::Error::from(error.to_string()))
            })
        })
    }

    /// Resolves exactly one comptime request. This is *not* a separate,
    /// isolated pipeline — it's the exact same per-`DefId` compile+store+
    /// execute steps `lower_package_to_lir_with` uses for a whole package,
    /// run for one specific `DefId` (the synthetic function built from this
    /// request's own `block`/`expected_ty`), touching the same shared
    /// `CompilerState` — so the result is cached/reused like any other
    /// compiled item, not recomputed the next time something references it.
    /// Only carries its own `package_id`/`def_id` (see
    /// `fp_typing::ComptimeRequest`'s doc comment) — everything else (the
    /// still-in-progress package's own HIR, its typed results, the pending
    /// `const { .. }` block itself) is looked up here, from `state`, by
    /// that id, never lowers any item's body other than this one synthetic
    /// function (`HirToMirLowerer::transform_comptime_request`). A genuine
    /// failure here only fails the one item awaiting this specific
    /// request, via the `Err` it returns to its `.await` point — the same
    /// per-item isolation `typecheck_item` already relies on.
    async fn resolve_comptime_request_with(
        state: &Rc<RefCell<CompilerState>>,
        request: fp_typing::ComptimeRequest,
    ) -> Result<Value, CompilerDriverError> {
        // The request already names its own package via `def_id` — no need
        // for the workspace to have any package "focused" first.
        let package_id = state
            .borrow()
            .workspace
            .compiled_package_for_def(request.def_id)
            .map(|package| package.borrow().package_id.clone())
            .ok_or_else(|| {
                CompilerDriverError::UnresolvablePackage(format!(
                    "no compiled package owns comptime request {:?}",
                    request.def_id
                ))
            })?;

        let hir_program = state
            .borrow()
            .in_progress_hir_program()
            .ok_or_else(|| {
                CompilerDriverError::MissingHir(format!(
                    "no in-progress HIR program for comptime request {:?}",
                    request.def_id
                ))
            })?;
        let current_package = hir_program
            .packages
            .get(&request.package_id)
            .cloned()
            .ok_or_else(|| CompilerDriverError::MissingHir(format!("{:?}", request.package_id)))?;

        let mut lowering = HirToMirLowerer::new();
        for (key, value) in state.borrow().resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        let mir_module = match lowering.transform_comptime_request(hir_program, current_package, request.def_id) {
            Ok(module) => module,
            Err(error) => {
                let (diagnostics, _) = lowering.take_diagnostics();
                let details = diagnostics_summary(&diagnostics);
                return Err(CompilerDriverError::InternalCompilerError(if details.is_empty() {
                    format!("HIR-to-MIR lowering failed: {error}")
                } else {
                    format!("HIR-to-MIR lowering failed: {error}; diagnostics: {details}")
                }));
            }
        };
        lowering.walk_program_types_for_layouts(&mir_module);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
        if had_errors {
            let details = diagnostics_summary(&diagnostics);
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering reported diagnostics: {details}"
            )));
        }
        let adt_defs = lowering.take_adt_defs();
        let struct_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> =
            lowering.all_adt_field_tys().into_iter().collect();
        let full_layouts = Self::collect_full_layouts(&lowering);
        let opaque_payload_sizes = lowering.opaque_payload_sizes().clone();
        let resolved_const_values = lowering.take_resolved_const_values();
        let resolved_const_defs = lowering.take_resolved_const_defs();
        state.borrow_mut().extend_mir_package(
            &package_id,
            struct_layouts,
            adt_defs,
            resolved_const_values,
            resolved_const_defs,
        );

        // Store this request's synthetic unit keyed by its own `DefId`,
        // into the shared `CompilerState` — a normal, cached compile, not
        // an isolated in-memory-only side channel.
        let unit = mir::MirCodeUnit {
            items: mir_module.items,
            bodies: mir_module.bodies,
        };
        state.borrow_mut().insert_mir_unit(&package_id, request.def_id, unit.clone());

        let module_path = QualifiedPath::new(vec!["__comptime_probe__".to_string()]);
        let mut lir_gen = Self::new_lir_generator(state, &package_id, &module_path, full_layouts, opaque_payload_sizes);
        for item in unit.items.iter().cloned() {
            let blob = lir_gen.transform_item(item, &unit.bodies).map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "MIR-to-LIR lowering failed for comptime request: {error}"
                ))
            })?;
            state
                .borrow_mut()
                .insert_lir_blob(&package_id, module_path.clone(), blob)?;
        }

        let values = Self::evaluate_comptime_lir_with(state, &package_id, &module_path)?;
        values.get(&request.def_id).cloned().ok_or_else(|| {
            CompilerDriverError::UnresolvableComptime(format!(
                "expression {} did not produce a comptime value",
                request.def_id
            ))
        })
    }

    /// Per-package HIR->MIR->LIR lowering, one `DefId` in, one unit out for
    /// every step — the shared core of `compile_items_to_lir_units` and
    /// `relower_cached_lir_units`. Every produced unit is stored directly
    /// into `state` as it's produced (`insert_mir_unit`/`insert_lir_blob`),
    /// so this never assembles a whole flattened program of its own to hand
    /// back to a caller; `state.mir_module`/`state.lir_blob` recover the
    /// flattened view on demand for anything that still needs it (the
    /// interpreter, `fp-bytecode`, ...).
    async fn lower_package_to_lir_with(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        hir_package_id: hir::PackageId,
        module_path: QualifiedPath,
    ) -> Result<(), CompilerDriverError> {
        // HIR has already passed type checking at this boundary. Lowering is
        // therefore strict: a failure is an internal compiler error, never a
        // recoverable source diagnostic.
        let hir = state.borrow().hir(hir_package_id)?;
        let mut lowering = HirToMirLowerer::new();
        for (key, value) in state.borrow().resolved_const_values() {
            lowering.seed_resolved_const(key.to_string(), value.clone());
        }
        // Seed once, then lower every top-level item on demand by its own
        // `DefId` — the same `ensure_item_lowered` primitive a single
        // comptime request's item-scoped lowering already goes through,
        // rather than one eager whole-package `.transform()` sweep. A
        // full-package compile still needs every item lowered (no
        // `main`-rooted reachability pruning here), so this loop still
        // walks `hir.items`; what's different is that each item's own unit
        // is stored into `state` the moment it's produced, not assembled
        // into one flat module first.
        let item_def_ids: Vec<_> = hir.items.iter().map(|item| item.def_id).collect();
        lowering.seed_package(hir.clone());
        for def_id in item_def_ids {
            if let Err(error) = lowering.ensure_item_lowered(def_id) {
                let (diagnostics, _) = lowering.take_diagnostics();
                let details = diagnostics_summary(&diagnostics);
                return Err(CompilerDriverError::InternalCompilerError(if details.is_empty() {
                    format!("HIR-to-MIR lowering failed: {error}")
                } else {
                    format!("HIR-to-MIR lowering failed: {error}; diagnostics: {details}")
                }));
            }
            let unit = lowering.take_unit(def_id);
            state.borrow_mut().insert_mir_unit(package_id, def_id, unit);
        }
        // Anything left over (e.g. synthetic runtime stub functions —
        // `append_runtime_stubs` — pulled in by an item's body but not
        // themselves any top-level `DefId`) is stored under one reserved
        // sentinel `DefId` local to this package's HIR numbering space, so
        // it's still folded into `state.mir_module(package_id)`'s flattened
        // view and still gets its own LIR unit lowered below.
        let leftover = lowering.take_program();
        if !leftover.items.is_empty() || !leftover.bodies.is_empty() {
            let sentinel = hir::DefId::new(hir_package_id, u32::MAX);
            state.borrow_mut().insert_mir_unit(
                package_id,
                sentinel,
                mir::MirCodeUnit {
                    items: leftover.items,
                    bodies: leftover.bodies,
                },
            );
        }
        // Run *before* the diagnostics check below — `walk_program_types_for_layouts`
        // can itself report errors (e.g. an unregistered ADT layout), and
        // those must not be silently dropped when `lowering` goes out of
        // scope at the end of this function.
        let mir_module = state.borrow().mir_module(package_id);
        lowering.walk_program_types_for_layouts(&mir_module);
        let (diagnostics, had_errors) = lowering.take_diagnostics();
        if had_errors {
            let details = diagnostics_summary(&diagnostics);
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering reported diagnostics: {details}"
            )));
        }
        let adt_defs = lowering.take_adt_defs();
        let struct_layouts: HashMap<fp_core::mir::DefId, Vec<fp_core::mir::Ty>> =
            lowering.all_adt_field_tys().into_iter().collect();
        let full_layouts = Self::collect_full_layouts(&lowering);
        let opaque_payload_sizes = lowering.opaque_payload_sizes().clone();
        let resolved_const_values = lowering.take_resolved_const_values();
        let resolved_const_defs = lowering.take_resolved_const_defs();
        state.borrow_mut().extend_mir_package(
            package_id,
            struct_layouts,
            adt_defs,
            resolved_const_values,
            resolved_const_defs,
        );

        // --- MIR -> LIR: per-`DefId`, lazy signature resolution, no
        // whole-program predeclare sweep (see `MirToLirLowerer::
        // with_signature_resolver`'s own doc comment) ---
        state.borrow_mut().reset_lir_package(package_id);
        let def_ids: Vec<_> = state
            .borrow()
            .mir_program()
            .package(package_id)
            .map(|package| package.units.keys().copied().collect())
            .unwrap_or_default();
        let mut lir_gen = Self::new_lir_generator(state, package_id, &module_path, full_layouts, opaque_payload_sizes);
        for def_id in def_ids {
            let Some(unit) = state.borrow().mir_unit(package_id, def_id).cloned() else {
                continue;
            };
            for item in unit.items.iter().cloned() {
                let blob = lir_gen.transform_item(item, &unit.bodies).map_err(|error| {
                    CompilerDriverError::InternalCompilerError(format!(
                        "MIR-to-LIR lowering failed for {}: {error}",
                        module_path.to_key()
                    ))
                })?;
                state
                    .borrow_mut()
                    .insert_lir_blob(package_id, module_path.clone(), blob)?;
            }
        }
        Ok(())
    }

    /// Folds `struct_layout_map`/`enum_layout_map` into the one combined
    /// `(DefId, args) -> field types` view `MirToLirLowerer` expects — enums
    /// share the same channel as structs (`mir_to_lir`'s `lir_type_from_ty`
    /// reconstructs an enum's runtime shape as `{tag, ...payload slots}`,
    /// exactly mirroring `EnumLayout::tag_ty`/`payload_tys` here).
    fn collect_full_layouts(
        lowering: &HirToMirLowerer,
    ) -> HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>> {
        let mut full_layouts: HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>> =
            lowering
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
        full_layouts
    }

    /// Builds a `MirToLirLowerer` wired with a lazy, per-`DefId` signature
    /// resolver (see `MirToLirLowerer::with_signature_resolver`'s own doc
    /// comment) instead of a whole-program predeclare sweep: a callee's
    /// signature is looked up first in `package_id`'s own
    /// `mir::MirPackage::sigs` (a forward reference within the same
    /// package), then in every other loaded package's (a cross-package
    /// call) — exactly the same two-tier lookup `predeclare_dependency_
    /// function_signatures` used to do eagerly, just resolved lazily and
    /// cached on first reference.
    fn new_lir_generator(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        module_path: &QualifiedPath,
        full_layouts: HashMap<(fp_core::mir::DefId, Vec<fp_core::mir::Ty>), Vec<fp_core::mir::Ty>>,
        opaque_payload_sizes: HashMap<String, u64>,
    ) -> MirToLirLowerer {
        let dependency_packages: Vec<mir::MirPackage> =
            state.borrow().mir_program().packages.values().cloned().collect();
        let resolver_state = state.clone();
        let resolver_package_id = package_id.clone();
        let resolve_signature: Box<
            dyn Fn(mir::DefId) -> Option<(mir::Function, Option<PackageId>)>,
        > = Box::new(move |def_id: mir::DefId| {
            let state = resolver_state.borrow();
            let mir_program = state.mir_program();
            if let Some(func) = mir_program
                .package(&resolver_package_id)
                .and_then(|package| package.sigs.get(&def_id))
            {
                return Some((func.clone(), None));
            }
            mir_program.packages.iter().find_map(|(dep_id, dep_package)| {
                if dep_id == &resolver_package_id {
                    return None;
                }
                dep_package
                    .sigs
                    .get(&def_id)
                    .map(|func| (func.clone(), Some(dep_id.clone())))
            })
        });
        MirToLirLowerer::new(state.borrow().data_layout.clone())
            .with_package_id(package_id.clone())
            .with_module_path(module_path.to_key())
            .with_full_layouts(full_layouts)
            .with_opaque_payload_sizes(opaque_payload_sizes)
            .with_dependency_packages(dependency_packages)
            .with_signature_resolver(resolve_signature)
    }

    /// Interprets every comptime entry in `package_id`'s own LIR for real
    /// and returns each const block's resolved value keyed by its own
    /// `DefId` (`LirComptimeEntry::def_id`, threaded through structurally
    /// from `register_const_block_comptime_entry`/`lower_const` via
    /// `mir::ExecutableConst`). Two callers: `resolve_comptime_request_with`
    /// (mid-typing-pass, to answer pending `ComptimeRequest`s for real) and
    /// `compile_package`'s post-typing pass (to embed real constants via
    /// `apply_resolved_comptime_block_values` + `relower_cached_lir_units`,
    /// for whichever entries the mid-pass attempts hadn't already resolved).
    async fn evaluate_comptime_lir(
        &mut self,
        package_id: &PackageId,
        module_path: &QualifiedPath,
    ) -> Result<HashMap<hir::DefId, Value>, CompilerDriverError> {
        Self::evaluate_comptime_lir_with(&self.state, package_id, module_path)
    }

    /// Same as `evaluate_comptime_lir`, but against a bare
    /// `Rc<RefCell<CompilerState>>` and its own fresh, local
    /// `LirInterpreter` (not `CompilerDriver.interpreter` — every call site
    /// already resets that field unconditionally before use, so it never
    /// carried state across calls anyway). `module_path` isn't used to find
    /// anything (`package_id`'s own already-lowered LIR, read via
    /// `state.lir_blob`, is) — only to key the resulting `ConstValueId` the
    /// same way `insert_const_value`'s caller (e.g. `fp-cli`'s
    /// `eval_script`/`execute_ast`) looks it back up by.
    fn evaluate_comptime_lir_with(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        module_path: &QualifiedPath,
    ) -> Result<HashMap<hir::DefId, Value>, CompilerDriverError> {
        let lir = state.borrow().lir_blob(package_id);
        let comptime_entries = lir.comptime_entries.clone();

        let value_id = ConstValueId::new(format!("const_value:{}", module_path.to_key()));
        if comptime_entries.is_empty() {
            // No comptime entry needed the real interpreter, but that
            // doesn't mean nothing was computed for this package — a
            // directly-foldable top-level const (e.g. `const X = 1 + 2 *
            // 3;`, no `let` needed — see `HirToMirLowerer::lower_const`'s
            // constant-folding fast path) resolves its value without ever
            // becoming a comptime entry. Surface the last one the same way
            // an interpreted entry's value already is (`insert_const_value`,
            // keyed by this package's own id) instead of unconditionally
            // substituting a unit placeholder that looks exactly like a
            // real "this evaluates to nothing" result.
            let mut last_value = None;
            if let Some(mir_package) = state.borrow().mir_program().package(package_id) {
                for key in mir_package.resolved_const_values.keys() {
                    let Some(value) = mir_package
                        .resolved_const(key)
                        .and_then(MirToHir::constant_to_value)
                    else {
                        continue;
                    };
                    last_value = Some(value);
                }
            }
            state
                .borrow_mut()
                .insert_const_value(value_id.clone(), last_value.unwrap_or_else(Value::unit));
            return Ok(HashMap::new());
        }
        // Query each package's own LIR directly instead of cloning every
        // artifact from every dependency into one throwaway combined
        // workspace first — `run_function_named_in_workspace` accepts a
        // chain and checks each in turn.
        let dependency_packages: Vec<mir::MirPackage> =
            state.borrow().mir_program().packages.values().cloned().collect();
        let lir_to_mir = LirToMir::new(dependency_packages);
        let workspaces_owned: Vec<fp_core::lir::LirUnitTable> = state
            .borrow()
            .lir_program()
            .packages
            .values()
            .map(|package| package.own_artifacts.clone())
            .collect();
        let workspaces: Vec<&fp_core::lir::LirUnitTable> = workspaces_owned.iter().collect();

        let mut count = 0usize;
        let mut last = Value::unit();
        let mut block_values: HashMap<hir::DefId, Value> = HashMap::new();
        let mut interpreter = LirInterpreter::new();
        let resolved = Self::resolved_const_values_snapshot(state);
        interpreter
            .inject_globals(&resolved)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        for entry in &comptime_entries {
            let result = interpreter.run_function_named_in_workspace(
                &workspaces,
                package_id,
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
            let constant = lir_to_mir.value_to_mir_constant(&value, &entry.ty).ok_or_else(|| {
                CompilerDriverError::UnsupportedWork(format!(
                    "unsupported comptime result for {}",
                    entry.key
                ))
            })?;
            state
                .borrow_mut()
                .insert_resolved_const_value(entry.key.clone(), constant);
            if entry.const_block_hir_id.is_some() {
                block_values.insert(entry.def_id, value.clone());
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

    fn collect_resolved_const_values(&self) -> HashMap<String, Value> {
        Self::resolved_const_values_snapshot(&self.state)
    }

    /// Same as `collect_resolved_const_values`, but against a bare
    /// `Rc<RefCell<CompilerState>>` for compiler hooks that do not hold a
    /// driver. Derives each name's `Value` from `CompilerState`'s own
    /// MIR-level, string-keyed `resolved_const_values` (folded/interpreted
    /// constants recorded during MIR lowering/comptime evaluation) rather
    /// than a separate typing-owned cache.
    fn resolved_const_values_snapshot(state: &Rc<RefCell<CompilerState>>) -> HashMap<String, Value> {
        state
            .borrow()
            .resolved_const_values()
            .filter_map(|(key, constant)| {
                MirToHir::constant_to_value(constant).map(|value| (key.to_string(), value))
            })
            .collect()
    }

}

/// The name an `ast::Item` is registered under during AST→HIR lowering
/// (`AstToHirLowerer::predeclare_items`'s `register_type_def`/
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
        | ItemKind::PrecompiledAsm(_)
        | ItemKind::PrecompiledLir(_)
        | ItemKind::PrecompiledArtifact(_) => None,
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
