use fp_backend::transformations::{
    AstToHirLowerer, HirLoweringConfig, HirToMirLowerer, MirToLirLowerer,
};
use fp_core::ast::package::{DependencyDescriptor, DependencyKind, PackageId};
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::{Expr, ExprKind, Item, ItemKind, Value};
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel};
use fp_core::hir;
use fp_core::mir;
use fp_interpret::LirInterpreter;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{CompilerDriverError, CompilerState, ExecutorHandle};

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
    let shown = errors
        .iter()
        .take(MAX_SHOWN)
        .cloned()
        .collect::<Vec<_>>()
        .join("; ");
    if errors.len() > MAX_SHOWN {
        format!(
            "{shown}; ... and {} more error(s)",
            errors.len() - MAX_SHOWN
        )
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
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::ast::package::AstPackage>>>,
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
            building_packages: HashSet::new(),
            compiled_packages: HashMap::new(),
            pipeline: PipelineMode::Native,
        }
    }

    pub async fn compile_native(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> {
        self.compile_package(package_id).await
    }

    /// Lowers one already typechecked workspace member through the native
    /// MIR/LIR path. Workspace compilation initially runs in transpile mode
    /// so vendored std packages are not forced to lower every public API;
    /// native backends then request this only for the package they emit.
    pub async fn lower_package_native_lir(
        &mut self,
        package_id: &PackageId,
    ) -> Result<(), CompilerDriverError> {
        let package = self
            .state
            .borrow()
            .workspace
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let previous_pipeline = self.pipeline;
        self.pipeline = PipelineMode::Native;
        let result = self.compile_items_to_lir_units(&package).await;
        self.pipeline = previous_pipeline;
        result
    }

    pub async fn compile_bytecode(
        &mut self,
        package_id: &PackageId,
    ) -> Result<fp_bytecode::BytecodeProgram, CompilerDriverError> {
        self.state.borrow_mut().set_bytecode_comptime(true);
        let package = self.compile_package(package_id).await?;
        // Bytecode packages do not enter the native/transpile lowering branch
        // in `compile_package`; establish their MIR roots before collecting
        // executable const entries for stackcode evaluation.
        self.compile_items_to_lir_units(&package).await?;
        self.evaluate_package_comptime_constants(package_id).await?;
        // Executable constants are first lowered as comptime entry points.
        // Once their values have been recorded, rerun the package lowering
        // so references become ordinary MIR constants before bytecode sees
        // them. This is the same two-phase model used by native compilation.
        self.compile_items_to_lir_units(&package).await?;
        let state = self.state.borrow();
        let mut mir = mir::MirCodeUnit::new();
        if let Some(package) = state.mir_program().package(package_id) {
            let package = package.borrow();
            mir.items.extend(package.items().cloned());
            mir.bodies
                .extend(package.bodies().map(|(id, body)| (*id, body.clone())));
        }
        if mir.items.is_empty() {
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "package {package_id} has no MIR program after root lowering"
            )));
        }
        fp_bytecode::lower_program(&mir).map_err(CompilerDriverError::from)
    }

    pub async fn evaluate_package_comptime_constants(
        &mut self,
        package_id: &PackageId,
    ) -> Result<(), CompilerDriverError> {
        let (mir, entries) = {
            let state = self.state.borrow();
            let package_rc = state
                .mir_program()
                .package(package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
            let package = package_rc.borrow();
            let mut mir = mir::MirCodeUnit::new();
            mir.items.extend(package.items().cloned());
            mir.bodies
                .extend(package.bodies().map(|(id, body)| (*id, body.clone())));
            let entries = package
                .executable_consts
                .iter()
                .map(|(def_id, (function_name, _))| {
                    (def_id.clone(), function_name.as_str().to_string())
                })
                .collect::<Vec<_>>();
            (mir, entries)
        };
        let bytecode =
            if self.state.borrow().bytecode_comptime() {
                Some(fp_bytecode::lower_program(&mir).map_err(|error| {
                    CompilerDriverError::InternalCompilerError(error.to_string())
                })?)
            } else {
                None
            };
        let mut resolved_entries = Vec::with_capacity(entries.len());
        for (def_id, function_name) in entries {
            let value = if let Some(bytecode) = bytecode.clone() {
                fp_stackcode::interpret_const(bytecode, &function_name)
                    .map_err(|error| CompilerDriverError::Interpreter(error.to_string()))?
            } else {
                Self::evaluate_comptime_lir(&self.state, &def_id)?
            };
            let hir_package_id = self
                .state
                .borrow()
                .workspace
                .compiled_package(package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?
                .borrow()
                .hir_package_id
                .clone();
            let hir_package = self.state.borrow().hir_package_rc(hir_package_id)?;
            hir_package.record_const_value(def_id.clone(), value.clone());
            hir_package.record_const_block_value(def_id.clone(), value);
            resolved_entries.push(def_id);
        }
        // The first lowering represents executable constants as entry points so
        // they can be evaluated. Re-lower the package after recording their
        // values to publish ordinary LIR globals for native code generation.
        {
            let mut state = self.state.borrow_mut();
            let mir_package = state.mir_package_rc(package_id);
            mir_package
                .borrow_mut()
                .executable_consts
                .retain(|def_id, _| !resolved_entries.contains(def_id));
            state.clear_lir_package(package_id);
        }
        let package = self
            .state
            .borrow()
            .workspace
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        self.compile_items_to_lir_units(&package).await?;
        Ok(())
    }

    /// Runs `def_id`'s own function through the shared `LirInterpreter`
    /// against the whole session's already-lowered LIR (`lir_program_rc`)
    /// — no separate "selected entrypoint" blob/rename step (that's
    /// `select_entrypoint`'s own, different concern: giving a *process*
    /// entrypoint its required bare OS symbol name) — deferred:
    /// `run_entrypoint` finds `def_id`'s function directly, under
    /// whatever name it already has.
    pub fn execute_runtime(
        &mut self,
        package_id: &PackageId,
        def_id: &hir::DefId,
    ) -> Result<fp_core::ast::Value, CompilerDriverError> {
        let mut state = self.state.borrow_mut();
        let program = state.lir_program_rc();
        let interpreter = state.interpreter_mut();
        *interpreter = LirInterpreter::new();
        interpreter
            .load_program(program)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))?;
        let value = interpreter.run_entrypoint(package_id, def_id)?;
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
            .state
            .borrow()
            .workspace
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let hir_package_id = package.borrow().hir_package_id.clone();
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
    fn rename_lir_function_unit(
        mut unit: fp_core::lir::LirCodeUnit,
        bare_name: &str,
    ) -> fp_core::lir::LirCodeUnit {
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
            .find(|candidate| candidate.def_id == Some(function.clone()))
            .ok_or_else(|| {
                CompilerDriverError::Interpreter(format!("entrypoint {function} was not emitted"))
            })?;
        let unit = fp_core::lir::LirCodeUnit {
            package_id: package_id.clone(),
            module_path: module_path.clone(),
            kind: fp_core::lir::LirCodeUnitKind::Function(lir_function),
        };
        let unit = Self::rename_lir_function_unit(unit, function_name);
        self.state
            .borrow_mut()
            .insert_runtime_program(lir_path.clone(), unit);
        self.state
            .borrow_mut()
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
        let entrypoint_def_id =
            self.resolve_entrypoint_def_id(package_id, module_path, function_name)?;
        let value = Self::evaluate_comptime_lir(&self.state, &entrypoint_def_id)?;
        // Mirrors `compile_package`'s dependency-loading branch (see its
        // identical three-call sequence): `evaluate_comptime_lir` computes
        // the entrypoint's real value, but that alone never reaches the
        // executable blob — the block's owning item (e.g. a `const X =
        // const { .. };`) was already lowered to an `ExecutableConst`
        // *before* its value was known, not a `LirGlobal`. Recording the
        // answer directly onto the HIR package's own (interior-mutable)
        // `const_block_values` here — via the same `Rc<HirPackage>`
        // already published in `hir_program`, not a clone of it — is what
        // lets a later relowering fold the const item for real, materializing
        // the missing global. Without this, the const item's global is
        // simply absent, and running it fails at runtime with "missing
        // global" — never a compile-time error, since nothing upstream of
        // execution ever notices the gap.
        let package = self
            .state
            .borrow()
            .workspace
            .compiled_package(package_id)
            .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
        let hir_package_id = package.borrow().hir_package_id.clone();
        self.state
            .borrow()
            .hir_package_rc(hir_package_id)?
            .record_const_block_value(entrypoint_def_id, value);
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
                    .state
                    .borrow()
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
                let precompiled_lir_blobs: Vec<fp_core::lir::LirBlob> = source
                    .items
                    .iter()
                    .filter_map(|pkg_item| match pkg_item.item.kind() {
                        fp_core::ast::ItemKind::PrecompiledLir(lir) => Some(lir.clone()),
                        _ => None,
                    })
                    .collect();
                let package = self.state.borrow().workspace.begin_package(
                    package_id.clone(),
                    source,
                    self.state.borrow().data_layout.clone(),
                );
                if !precompiled_lir_blobs.is_empty() {
                    Self::publish_precompiled_lir(&self.state, package_id, &precompiled_lir_blobs)?;
                } else if matches!(
                    self.pipeline,
                    PipelineMode::Native | PipelineMode::Transpile
                ) {
                    // `Transpile` needs HIR generation + typing too (it lifts the
                    // typed HIR back to AST inside `compile_items_to_lir_units`) — it now also
                    // attempts MIR/LIR lowering there so any comptime entries (e.g. `const
                    // { .. }` blocks) get resolved and relowered the same way `Native` does,
                    // below — every target needs the resolved value, not just the block's
                    // type, so both pipeline modes handle a comptime failure identically.
                    self.compile_items_to_lir_units(&package).await?;
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
        blobs: &[fp_core::lir::LirBlob],
    ) -> Result<(), CompilerDriverError> {
        for blob in blobs {
            state
                .borrow_mut()
                .insert_lir_blob_for_package(package_id, blob.clone());
        }
        Ok(())
    }

    /// Runs a whole package's HIR generation + typing, then per-`DefId`
    /// HIR->MIR->LIR lowering (`lower_package_to_mir`/`lower_package_to_lir_with`)
    /// — every step one `DefId` in, one unit out, stored straight into the shared
    /// `CompilerState` as it goes (see this module's own data-flow doc
    /// comment for the full picture). Never predeclares/lowers the whole
    /// package as one flat blob.
    async fn compile_items_to_lir_units(
        &mut self,
        package: &Rc<RefCell<fp_core::ast::package::AstPackage>>,
    ) -> Result<(), CompilerDriverError> {
        let hir_package_id = package.borrow().hir_package_id.clone();
        let current_package_id = package.borrow().package_id.clone();
        let package_source = package.borrow().clone();
        // Re-lowering after comptime evaluation rebuilds HIR from the same
        // source. Preserve values recorded on the previous package so the
        // new MIR pass can replace executable entries with static data.
        let prior_const_values = self
            .state
            .borrow()
            .hir_program()
            .package(&hir_package_id)
            .map(|hir_package| {
                (
                    hir_package.const_values(),
                    hir_package.const_block_values(),
                )
            });
        // Item-position macro invocations (e.g. `make_adder!(add_two, 2);`,
        // real vendored std's `int_impl!`/`uint_impl!`, ...) are expanded by
        // `AstToHirLowerer`'s own single-pass item walker
        // (`ast_to_hir::expand_item_macros`), driven by the provider's own
        // `intrinsic_normalizer()` — see `PackageProvider::
        // intrinsic_normalizer`'s doc comment for why that choice lives on
        // the already-resolved, already-per-language provider rather than a
        // separate frontend dependency of this crate.
        let normalizer = self
            .state
            .borrow()
            .workspace
            .provider()
            .intrinsic_normalizer();
        let mut generator =
            AstToHirLowerer::new(self.state.borrow().hir_program_rc(), hir_package_id.clone())
                .with_intrinsic_normalizer(normalizer)
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
        let package_exports = generator.exported_symbols();
        let type_alias_exports = generator.exported_type_aliases();
        package
            .borrow_mut()
            .type_alias_exports
            .extend(type_alias_exports);
        self.type_check_program(hir_program, package_exports)
            .await
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package HIR type checking failed: {error}"
                ))
            })?;
        if let Some((const_values, const_block_values)) = prior_const_values {
            let hir_package = self.state.borrow().hir_package_rc(hir_package_id.clone())?;
            for (def_id, value) in const_values {
                hir_package.record_const_value(def_id, value);
            }
            for (def_id, value) in const_block_values {
                hir_package.record_const_block_value(def_id, value);
            }
        }

        // Transpile: lift typed HIR back to AST — this is what the Kotlin
        // backend actually reads, and doesn't depend on anything below
        // succeeding.
        if self.pipeline == PipelineMode::Transpile {
            return self
                .transpile_lift_and_validate(current_package_id, hir_package_id)
                .await;
        }

        let state = self.state.clone();
        let hir_program = state.borrow().hir_program_rc();
        let mir_package = state.borrow_mut().mir_package_rc(&current_package_id);
        let mut lowering = HirToMirLowerer::new(hir_program, hir_package_id.clone(), mir_package);
        lowering.register_package_items();
        let current_package = lowering.current_package_handle();
        // Only the package's own public surface (plus `main`, conventionally
        // private — see `resolve_entrypoint_def_id`'s doc comment) needs an
        // explicit root here: every already-established lazy path
        // (`ensure_function_lowered`/`ensure_method_lowered`/
        // `ensure_const_info`/`try_lazily_register_adt`/
        // `try_lazily_register_method`, all triggered from within
        // `lower_package_to_mir` itself as a lowered body actually
        // references them) pulls in whatever a root item transitively
        // needs. A private item unreached from any root is genuinely dead
        // code and no longer gets a MIR unit at all.
        for item in &current_package.items {
            if item.def_id.package_id != hir_package_id {
                continue;
            }
            if !Self::is_lowering_root(item) {
                continue;
            }
            Self::lower_package_to_mir(
                &state,
                &current_package_id,
                &mut lowering,
                item.def_id.clone(),
            )
            .await?;
        }
        let runtime_support = state
            .borrow_mut()
            .mir_package_rc(&current_package_id)
            .borrow()
            .runtime_support
            .clone();
        if !runtime_support.items.is_empty() || !runtime_support.bodies.is_empty() {
            lowering.walk_program_types_for_layouts(&runtime_support);
        }
        let diagnostics = lowering.take_diagnostics();
        if diagnostics.has_errors() {
            let details = diagnostics_summary(&diagnostics.get_diagnostics());
            return Err(CompilerDriverError::InternalCompilerError(format!(
                "HIR-to-MIR lowering reported diagnostics: {details}"
            )));
        }
        lowering.sync_layout_exports();

        // --- MIR -> LIR: per-`DefId`, lazy signature resolution, no
        // whole-program predeclare sweep — `MirToLirLowerer` reads
        // `full_layouts`/`opaque_payload_sizes`/signatures straight off
        // `mir_program_rc()` (just extended above); every blob this lowers
        // gets pushed alongside any earlier one, never reset first (see
        // `lir::LirPackage`'s own doc comment) ---
        let def_ids: Vec<_> = state
            .borrow()
            .mir_program()
            .package(&current_package_id)
            .map(|package| package.borrow().units.keys().cloned().collect())
            .unwrap_or_default();
        let mut lir_gen = {
            let borrowed = state.borrow();
            MirToLirLowerer::new(
                borrowed.data_layout.clone(),
                borrowed.mir_program_rc(),
                borrowed.lir_program_rc(),
            )
            .with_package_id(current_package_id.clone())
        };
        lir_gen.prepare_package(&current_package_id);
        for def_id in def_ids {
            Self::lower_package_to_lir_with(&state, &current_package_id, &mut lir_gen, def_id)
                .await?;
        }
        Self::lower_runtime_support_to_lir(&state, &current_package_id, &mut lir_gen).await?;
        Ok(())
    }

    /// `compile_items_to_lir_units`'s `PipelineMode::Transpile` path: lifts
    /// the just-typechecked HIR back to AST (what the Kotlin backend
    /// actually reads), splices it onto the original untyped source items
    /// by qualified-path identity, then does a best-effort HIR->MIR->LIR
    /// lowering purely so any `const { .. }` blocks get validated later
    /// through the real interpreter (`evaluate_comptime_lir`) instead of a
    /// hand-rolled one. MIR lowering was built for the Native pipeline and
    /// may not yet cover every construct real vendored std/workspace code
    /// exercises — a failure here is reported and this package simply
    /// produces no LIR (comptime validation is skipped for it), matching
    /// this pipeline's prior behavior; the lifted AST above is already
    /// complete regardless.
    async fn transpile_lift_and_validate(
        &mut self,
        current_package_id: PackageId,
        hir_package_id: hir::PackageId,
    ) -> Result<(), CompilerDriverError> {
        // Scoped narrowly — `lift_items_by_path`/`referenced_paths_by_path`
        // return owned data, so nothing here needs to outlive this block.
        let (lifted_items_by_path, referenced_paths_by_path) = {
            let state = self.state.borrow();
            let hir = state.hir(hir_package_id.clone())?;
            let lifter =
                fp_backend::transforms::HirToAstLifter::new(&hir, Some(state.hir_program()));
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
            .state
            .borrow()
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
                    let key: Vec<String> = key
                        .segments
                        .iter()
                        .map(|s| s.as_str().to_string())
                        .collect();
                    let values: Vec<Vec<String>> = values
                        .into_iter()
                        .map(|path| {
                            path.segments
                                .iter()
                                .map(|s| s.as_str().to_string())
                                .collect()
                        })
                        .collect();
                    (key, values)
                })
                .collect();
        }
        let state = self.state.clone();
        if let Err(error) = (async {
            let hir_program = state.borrow().hir_program_rc();
            let mir_package = state.borrow_mut().mir_package_rc(&current_package_id);
            let mut lowering =
                HirToMirLowerer::new(hir_program, hir_package_id.clone(), mir_package);
            lowering.register_package_items();
            let current_package = lowering.current_package_handle();
            // Same public-surface-plus-`main`-only roots as the Native
            // pipeline's own loop (`is_lowering_root`'s doc comment) — the
            // same lazy on-demand paths pull in whatever a root actually
            // references, transitively.
            for item in &current_package.items {
                if !Self::is_lowering_root(item) {
                    continue;
                }
                Self::lower_package_to_mir(
                    &state,
                    &current_package_id,
                    &mut lowering,
                    item.def_id.clone(),
                )
                .await?;
            }
            let runtime_support = state
                .borrow_mut()
                .mir_package_rc(&current_package_id)
                .borrow()
                .runtime_support
                .clone();
            if !runtime_support.items.is_empty() || !runtime_support.bodies.is_empty() {
                lowering.walk_program_types_for_layouts(&runtime_support);
            }
            let diagnostics = lowering.take_diagnostics();
            if diagnostics.has_errors() {
                let details = diagnostics_summary(&diagnostics.get_diagnostics());
                return Err(CompilerDriverError::InternalCompilerError(format!(
                    "HIR-to-MIR lowering reported diagnostics: {details}"
                )));
            }
            // Every struct/enum/method/const `lowering` computed already
            // landed directly in the session's own package as it was
            // produced (shared `Rc<RefCell<MirPackage>>` — see
            // `HirToMirLowerer::mir_package`'s own doc comment); only
            // `full_layouts`/`opaque_payload_sizes` are a folded view
            // computed after the fact, written in place on that same
            // shared package by `sync_layout_exports` itself.
            lowering.sync_layout_exports();

            // --- MIR -> LIR: per-`DefId`, lazy signature resolution, no
            // whole-program predeclare sweep — `MirToLirLowerer` reads
            // `full_layouts`/`opaque_payload_sizes`/signatures straight
            // off `mir_program_rc()` (just extended above); every blob
            // this lowers gets pushed alongside any earlier one for the
            // same package, never reset first (see `lir::LirPackage`'s
            // own doc comment) ---
            let def_ids: Vec<_> = state
                .borrow()
                .mir_program()
                .package(&current_package_id)
                .map(|package| package.borrow().units.keys().cloned().collect())
                .unwrap_or_default();
            let mut lir_gen = {
                let borrowed = state.borrow();
                MirToLirLowerer::new(
                    borrowed.data_layout.clone(),
                    borrowed.mir_program_rc(),
                    borrowed.lir_program_rc(),
                )
                .with_package_id(current_package_id.clone())
            };
            for def_id in def_ids {
                Self::lower_package_to_lir_with(&state, &current_package_id, &mut lir_gen, def_id)
                    .await?;
            }
            Self::lower_runtime_support_to_lir(&state, &current_package_id, &mut lir_gen).await?;
            Ok(())
        })
        .await
        {
            fp_core::diagnostics::DiagnosticManager::report_warning_with_context(
                "const-eval".to_string(),
                format!(
                    "HIR->MIR/LIR lowering failed, skipping comptime validation for this package: {error}"
                ),
            );
        }
        Ok(())
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
        package_exports: std::collections::HashMap<String, hir::Res>,
    ) -> fp_core::Result<()> {
        let comptime_resolver = self.state.borrow().comptime_resolver.clone();
        let dependency_program = self.state.borrow().hir_program_rc();
        let executor = self.state.borrow().tasks.clone();
        let checker = fp_typing::HirTypeChecker::new(
            program,
            Some(dependency_program),
            comptime_resolver,
            executor,
        );
        let item_ids: Vec<_> = checker
            .borrow()
            .package()
            .items
            .iter()
            .map(|item| item.def_id.clone())
            .collect();
        let handles: Vec<_> = item_ids
            .into_iter()
            .map(|def_id| fp_typing::HirTypeChecker::spawn_item_task(&checker, def_id))
            .collect();
        for handle in handles {
            handle.await;
        }
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
        // unwrapping, lets us take real ownership without ever deep-copying
        // the package's own data.
        drop(checker);
        let mut package = Rc::try_unwrap(package).unwrap_or_else(|_| {
            unreachable!("no other strong reference to this package's HirPackage should outlive its own typecheck pass")
        });
        package.hir_exports.extend(package_exports);
        self.state.borrow_mut().insert_hir(package);
        Ok(())
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
    /// execute steps `compile_items_to_lir_units` uses for a whole package,
    /// run for one specific `DefId` (the block's
    /// own, real one — see `HirToMirLowerer::ensure_const_block_lowered`),
    /// touching the same shared `CompilerState` — so the result is
    /// cached/reused like any other compiled item, not recomputed the next
    /// time something references it. `HirToMirLowerer` has no separate
    /// comptime-request entry point of its own: `ensure_item_lowered`
    /// (via `lower_package_to_mir`) already falls back to lowering a
    /// const-block `DefId` when it isn't a top-level item, so a comptime
    /// request is lowered exactly the same way any other item is. A
    /// genuine failure here only fails the one item awaiting this
    /// specific request, via the `Err` it returns to its `.await` point —
    /// the same per-item isolation `typecheck_item` already relies on.
    async fn resolve_comptime_request_with(
        state: &Rc<RefCell<CompilerState>>,
        request: fp_typing::ComptimeRequest,
    ) -> Result<Value, CompilerDriverError> {
        // The request's own `def_id` already carries its owning package's
        // id — no need to look the compiled package up in the workspace
        // just to recover an id it already has.
        let package_id = PackageId::new(request.def_id.package_id.as_str());
        let hir_program = state.borrow().hir_program_rc();
        let mir_package = state.borrow_mut().mir_package_rc(&package_id);
        let mut lowering =
            HirToMirLowerer::new(hir_program, request.package_id.clone(), mir_package);
        lowering.register_package_items();
        Self::lower_package_to_mir(state, &package_id, &mut lowering, request.def_id.clone())
            .await?;

        lowering.sync_layout_exports();

        let mut lir_gen = {
            let borrowed = state.borrow();
            MirToLirLowerer::new(
                borrowed.data_layout.clone(),
                borrowed.mir_program_rc(),
                borrowed.lir_program_rc(),
            )
            .with_package_id(package_id.clone())
        };
        Self::lower_package_to_lir_with(state, &package_id, &mut lir_gen, request.def_id.clone())
            .await?;

        if state.borrow().bytecode_comptime() {
            Self::evaluate_comptime_bytecode(state, &request.def_id)
        } else {
            Self::evaluate_comptime_lir(state, &request.def_id)
        }
    }

    fn evaluate_comptime_bytecode(
        state: &Rc<RefCell<CompilerState>>,
        def_id: &hir::DefId,
    ) -> Result<Value, CompilerDriverError> {
        let package_id = PackageId::new(def_id.package_id.as_str());
        let (mir, function_name) = {
            let state_ref = state.borrow();
            let package = state_ref
                .mir_program()
                .package(&package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
            let package_ref = package.borrow();
            let function_name = package_ref
                .executable_consts
                .get(def_id)
                .map(|(name, _)| name.as_str().to_string())
                .ok_or_else(|| {
                    CompilerDriverError::InternalCompilerError(format!(
                        "missing executable const for {def_id}"
                    ))
                })?;
            let mut mir = mir::MirCodeUnit::new();
            mir.items.extend(package_ref.items().cloned());
            mir.bodies
                .extend(package_ref.bodies().map(|(id, body)| (*id, body.clone())));
            (mir, function_name)
        };
        let bytecode = fp_bytecode::lower_program(&mir)
            .map_err(|error| CompilerDriverError::InternalCompilerError(error.to_string()))?;
        fp_stackcode::interpret_const(bytecode, &function_name)
            .map_err(|error| CompilerDriverError::Interpreter(error.to_string()))
    }

    /// Whether `item` needs an explicit HIR->MIR lowering root — the
    /// package's own public surface, plus `main` (conventionally declared
    /// without `pub` — see `resolve_entrypoint_def_id`'s doc comment, so a
    /// bare `Visibility::Public` check alone would miss it). Every other
    /// item, if actually used, is pulled in transitively by whichever root
    /// (or root's transitive dependency) references it.
    fn is_lowering_root(item: &hir::Item) -> bool {
        item.visibility == hir::Visibility::Public
            || matches!(
                &item.kind,
                hir::ItemKind::Function(function) if function.sig.name.as_str() == "main"
            )
            || matches!(&item.kind, hir::ItemKind::Impl(_))
    }

    /// One `DefId`'s own HIR->MIR lowering — call once per top-level `DefId`
    /// in `lowering`'s own package (`register_package_items` must already
    /// have run on `lowering`, once, before the first call). Stores the
    /// produced unit directly into `state` (`insert_mir_unit`) and walks its
    /// types for layouts immediately, so this never assembles a whole
    /// flattened program of its own to hand back to a caller.
    async fn lower_package_to_mir(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        lowering: &mut HirToMirLowerer,
        def_id: hir::DefId,
    ) -> Result<(), CompilerDriverError> {
        if let Err(error) = lowering.ensure_item_lowered(def_id.clone()) {
            let diagnostics = lowering.take_diagnostics();
            let details = diagnostics_summary(&diagnostics.get_diagnostics());
            return Err(CompilerDriverError::InternalCompilerError(
                if details.is_empty() {
                    format!("HIR-to-MIR lowering failed: {error}")
                } else {
                    format!("HIR-to-MIR lowering failed: {error}; diagnostics: {details}")
                },
            ));
        }
        let unit = lowering.take_unit();
        // Run *before* the diagnostics check below — `walk_program_types_for_layouts`
        // can itself report errors (e.g. an unregistered ADT layout), and
        // those must not be silently dropped when `lowering` goes out of
        // scope at the end of this function.
        lowering.walk_program_types_for_layouts(&unit);
        state.borrow_mut().insert_mir_unit(package_id, def_id, unit);
        Ok(())
    }

    /// One `DefId`'s own MIR->LIR lowering — call once per `DefId` already
    /// stored under `package_id`, sharing the same `lir_gen` across a whole
    /// package's own loop (see `compile_items_to_lir_units`) so its lazily
    /// resolved signatures stay cached across calls. Every produced blob is
    /// stored directly into `state` as it's produced
    /// (`insert_lir_blob_for_package`).
    async fn lower_package_to_lir_with(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        lir_gen: &mut MirToLirLowerer,
        def_id: hir::DefId,
    ) -> Result<(), CompilerDriverError> {
        let blobs = lir_gen.transform_unit(def_id.clone()).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "MIR-to-LIR lowering failed for {def_id}: {error}"
            ))
        })?;
        for blob in blobs {
            state
                .borrow_mut()
                .insert_lir_blob_for_package(package_id, blob);
        }
        Ok(())
    }

    /// `runtime_support`'s counterpart to `lower_package_to_lir_with` — it
    /// has no owning `DefId` at all (see `MirPackage::runtime_support`'s
    /// own doc comment), so it's lowered once via `transform_items` (which
    /// takes an owned `MirCodeUnit` directly, no `DefId` lookup) rather
    /// than through `transform_unit`'s per-`DefId` path.
    async fn lower_runtime_support_to_lir(
        state: &Rc<RefCell<CompilerState>>,
        package_id: &PackageId,
        lir_gen: &mut MirToLirLowerer,
    ) -> Result<(), CompilerDriverError> {
        let runtime_support = state
            .borrow()
            .mir_program()
            .package(package_id)
            .map(|package| package.borrow().runtime_support.clone())
            .unwrap_or_default();
        if runtime_support.items.is_empty() {
            return Ok(());
        }
        let blobs = lir_gen.transform_items(runtime_support).map_err(|error| {
            CompilerDriverError::InternalCompilerError(format!(
                "MIR-to-LIR lowering failed for runtime-support stubs: {error}"
            ))
        })?;
        for blob in blobs {
            state
                .borrow_mut()
                .insert_lir_blob_for_package(package_id, blob);
        }
        Ok(())
    }

    /// Runs `def_id`'s own comptime function through the shared
    /// `LirInterpreter` (`CompilerState::interpreter_mut`) for real and
    /// returns its resolved value directly — a bare `Rc<RefCell<
    /// CompilerState>>`, not `&mut self`, since this is reached both from
    /// `compile_package_module_native` and from
    /// `resolve_comptime_request_with`'s free-standing, mid-typing-pass
    /// context.
    fn evaluate_comptime_lir(
        state: &Rc<RefCell<CompilerState>>,
        def_id: &hir::DefId,
    ) -> Result<Value, CompilerDriverError> {
        // `def_id` already carries its own owning package's id — the same
        // identity `lower_executable_const`/`LirProgram::
        // find_function_by_def_id` use to name/find this exact entry's
        // LIR function, so no separate `package_id` parameter is needed.
        let package_id = PackageId::new(def_id.package_id.as_str());
        let mut state_mut = state.borrow_mut();
        // The whole session's `LirProgram`, not just `package_id`'s own
        // blob — a comptime function can call into a dependency package,
        // and `LirInterpreter::run_entrypoint`'s own lookups
        // (`LirProgram::find_function`/`find_function_any_package`) only
        // ever see what `load_program` handed it. `lir_program_rc` clones
        // the shared `Rc`, not the program itself.
        let program = state_mut.lir_program_rc();
        let return_ty = program
            .find_function_by_def_id(def_id)
            .map(|function| function.signature.return_type.clone());
        let interpreter = state_mut.interpreter_mut();
        interpreter.load_program(program)?;
        let mut value = interpreter.run_entrypoint(&package_id, def_id)?;
        if let Some(ty) = return_ty {
            value = interpreter.read_typed_const_value(value, &ty)?;
        }
        Ok(value)
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
        ItemKind::Macro(item_macro) => item_macro
            .declared_name
            .as_ref()
            .map(|ident| ident.name.as_str()),
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
        fp_core::ast::Name::ParameterPath(param_path) => param_path
            .segments
            .last()
            .map(|seg| seg.ident.name.as_str()),
    }
}
