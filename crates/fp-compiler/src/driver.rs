use fp_backend::transformations::{
    AstToHirLowerer, HirLoweringConfig, HirToMirLowerer, MirToLirLowerer,
};
use fp_core::ast::package::{DependencyDescriptor, PackageId};
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
    /// `compile_package` at the time — that future's `&mut self` borrow
    /// lasts for its entire lifetime (how `async fn` desugars), so a
    /// `'static` task closure cannot also borrow `self` directly. Sharing
    /// just `state` this way (not `interpreter`, `building_packages`, etc.
    /// — those aren't needed by anything spawned as a task) keeps the rest
    /// of `CompilerDriver` an ordinary `&mut self`-based type.
    pub state: Rc<RefCell<CompilerState>>,
    building_packages: HashSet<PackageId>,
    compiled_packages: HashMap<PackageId, Rc<RefCell<fp_core::ast::package::AstPackage>>>,
    /// The compiled `std` package is the source of Rust's implicit prelude.
    /// Keep it on the driver because package-scoped `AstProgram`s deliberately
    /// have independent prelude slots and are recreated during recursion.
    prelude_package: Option<Rc<RefCell<fp_core::ast::package::AstPackage>>>,
    /// Packages that completed the pipeline required of a compilation root.
    /// A transpile dependency is deliberately absent until a later workspace
    /// walk promotes it to a root.
    completed_roots: HashSet<PackageId>,
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
            prelude_package: None,
            completed_roots: HashSet::new(),
            pipeline: PipelineMode::Native,
        }
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
        let entries = {
            let state = self.state.borrow();
            let package_rc = state
                .mir_program()
                .package(package_id)
                .ok_or_else(|| CompilerDriverError::UnresolvablePackage(package_id.to_string()))?;
            let package = package_rc.borrow();
            package
                .executable_consts
                .iter()
                .map(|(def_id, (function_name, _))| {
                    (def_id.clone(), function_name.as_str().to_string())
                })
                .collect::<Vec<_>>()
        };
        let mut resolved_entries = Vec::with_capacity(entries.len());
        for (def_id, _function_name) in entries {
            // Lower the requested entry itself before interpreting it. The
            // package root pass registers executable-const metadata, but the
            // synthetic const-block function is intentionally demand-driven.
            // Reuse the same shared HIR/MIR/LIR path used by type checking;
            // this keeps one DefId and one cache instead of interpreting a
            // stale package snapshot.
            let value = Self::resolve_comptime_request_with(
                &self.state,
                fp_typing::ComptimeRequest {
                    package_id: def_id.package_id.clone(),
                    def_id: def_id.clone(),
                },
            )
            .await?;
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
            hir_package
                .borrow()
                .record_const_value(def_id.clone(), value.clone());
            hir_package
                .borrow()
                .record_const_block_value(def_id.clone(), value);
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
    /// only because callers also use it to name the runtime LIR unit selected
    /// by `select_entrypoint`.
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

    /// Compile a package after recursively compiling its declared
    /// dependencies. Dependency resolution and version selection happen in
    /// the provider; the driver only consumes the concrete package IDs it is
    /// given by metadata.
    pub async fn compile_package(
        &mut self,
        package_id: &PackageId,
    ) -> Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> {
        self.compile_package_with_scope(package_id, true).await
    }

    /// Compiles a package with an explicit root/dependency scope. In transpile
    /// mode dependencies need their HIR definitions and exports available to
    /// resolve the root package, but they are not compilation roots: checking
    /// and backend lowering every dependency would make the transpiler process
    /// the entire sysroot. The same rule applies to native roots: dependencies
    /// publish typed HIR for identity-based resolution while only requested
    /// workspace members enter MIR/LIR lowering.
    async fn compile_package_with_scope(
        &mut self,
        package_id: &PackageId,
        is_root: bool,
    ) -> Result<Rc<RefCell<fp_core::ast::package::AstPackage>>, CompilerDriverError> {
        let parent_workspace = self.state.borrow().workspace.clone();
        if let Some(package) = self.compiled_packages.get(package_id).cloned() {
            parent_workspace.import_package(package_id.clone(), package.clone());
            if !is_root {
                self.ensure_hir_for_resolution(&package)?;
            }
            if is_root && !self.completed_roots.contains(package_id) {
                self.ensure_hir_for_compilation(&package)?;
                self.compile_items_to_lir_units(&package).await?;
                self.completed_roots.insert(package_id.clone());
            }
            return Ok(package);
        }
        if let Some(package) = parent_workspace.compiled_package(package_id) {
            if !is_root {
                self.ensure_hir_for_resolution(&package)?;
            }
            if is_root && !self.completed_roots.contains(package_id) {
                self.ensure_hir_for_compilation(&package)?;
                self.compile_items_to_lir_units(&package).await?;
                self.completed_roots.insert(package_id.clone());
            }
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
                    if !is_root {
                        self.ensure_hir_for_resolution(&package)?;
                    }
                } else if !is_root {
                    self.ensure_hir_for_resolution(&package)?;
                } else if matches!(
                    self.pipeline,
                    PipelineMode::Native | PipelineMode::Transpile
                ) {
                    self.ensure_hir_for_compilation(&package)?;
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
        if is_root {
            self.completed_roots.insert(package_id.clone());
        }
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
            let dependency_package =
                Box::pin(self.compile_package_with_scope(&dependency_id, false)).await?;
            if dependency_id.as_str() == "std" {
                self.prelude_package = Some(dependency_package.clone());
                self.state
                    .borrow()
                    .workspace
                    .install_prelude(dependency_package);
            }
        }
        // `std` may have been discovered below an intermediate dependency.
        // That dependency's package-scoped workspace is discarded when its
        // recursive compilation returns, so install the retained prelude in
        // the workspace that belongs to this package as well.
        if let Some(prelude) = self.prelude_package.clone() {
            self.state.borrow().workspace.install_prelude(prelude);
        }
        Ok(())
    }

    /// Compile every workspace member as a root through the same recursive,
    /// cached, cycle-safe dependency machinery `compile_package` already uses
    /// for a package's declared dependencies. `root_id` is retained as a
    /// caller-supplied bookkeeping identity and is never resolved through a
    /// `PackageProvider`. Dependencies discovered while compiling a member
    /// follow the transpile dependency policy, while every listed member is
    /// promoted to a root and therefore fully checked and handed to the
    /// backend. Callers read each result via `AstProgram::package_source`.
    pub async fn compile_workspace(
        &mut self,
        _root_id: &PackageId,
        members: &[PackageId],
    ) -> Result<(), CompilerDriverError> {
        for member in members {
            self.compile_package_with_scope(member, true).await?;
        }
        Ok(())
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

    /// Lowers a package into HIR and publishes its exported definitions. This
    /// is the dependency path for transpilation: name and type resolution can
    /// inspect the package's real HIR, while the package itself is not made a
    /// type-checking or backend root.
    fn lower_package_hir(
        &mut self,
        package_source: &fp_core::ast::package::AstPackage,
        hir_package_id: hir::PackageId,
        resolution_only: bool,
    ) -> Result<
        (
            hir::HirPackage,
            std::collections::HashMap<String, hir::Res>,
            std::collections::HashMap<String, fp_core::ast::package::TypeAliasExport>,
        ),
        CompilerDriverError,
    > {
        let normalizer = self
            .state
            .borrow()
            .workspace
            .provider()
            .intrinsic_normalizer();
        let mut generator =
            AstToHirLowerer::new(self.state.borrow().hir_program_rc(), hir_package_id)
                .with_intrinsic_normalizer(normalizer)
                .with_lowering_config(HirLoweringConfig {
                    capabilities: self.state.borrow().backend_capabilities(),
                    resolution_only,
                })
                .with_workspace(self.state.borrow().workspace.clone());
        let hir_package = generator.transform_package(package_source)?;
        Ok((
            hir_package,
            generator.exported_symbols(),
            generator.exported_type_aliases(),
        ))
    }

    fn lower_package_hir_for_resolution(
        &mut self,
        package: &Rc<RefCell<fp_core::ast::package::AstPackage>>,
    ) -> Result<(), CompilerDriverError> {
        let hir_package_id = package.borrow().hir_package_id.clone();
        let package_source = package.borrow().clone();
        let (mut hir_package, exports, type_aliases) =
            self.lower_package_hir(&package_source, hir_package_id, true)?;
        hir_package.hir_exports.extend(exports);
        package.borrow_mut().type_alias_exports.extend(type_aliases);
        self.state.borrow_mut().insert_hir(hir_package);
        Ok(())
    }

    /// Ensure a transpile dependency has a published HIR package before a
    /// consumer is lowered. A package can already be present in the AST
    /// workspace when it was compiled through another scope, or it can have
    /// taken the precompiled-LIR branch above; neither fact implies that its
    /// public HIR exports are in the session-wide program. This is the
    /// dependency equivalent of rustc's crate metadata loading: publish once,
    /// then let every later lowerer resolve through the same program.
    fn ensure_hir_for_resolution(
        &mut self,
        package: &Rc<RefCell<fp_core::ast::package::AstPackage>>,
    ) -> Result<(), CompilerDriverError> {
        let hir_package_id = package.borrow().hir_package_id.clone();
        let existing = self
            .state
            .borrow()
            .hir_program()
            .package(&hir_package_id)
            .map(|hir_package| {
                let hir_package = hir_package.borrow();
                (
                    hir_package.module_tree.all_paths().count(),
                    hir_package.hir_exports.len(),
                )
            });
        if existing.is_some() {
            return Ok(());
        }
        self.lower_package_hir_for_resolution(package)
    }

    fn ensure_hir_for_compilation(
        &mut self,
        package: &Rc<RefCell<fp_core::ast::package::AstPackage>>,
    ) -> Result<(), CompilerDriverError> {
        let hir_package_id = package.borrow().hir_package_id.clone();
        if self
            .state
            .borrow()
            .hir_program()
            .package(&hir_package_id)
            .is_some()
        {
            return Ok(());
        }
        let package_source = package.borrow().clone();
        let (mut hir_package, exports, type_aliases) =
            self.lower_package_hir(&package_source, hir_package_id, false)?;
        hir_package.hir_exports.extend(exports);
        package.borrow_mut().type_alias_exports.extend(type_aliases);
        self.state.borrow_mut().insert_hir(hir_package);
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
        // Re-lowering after comptime evaluation rebuilds HIR from the same
        // source. Preserve values recorded on the previous package so the
        // new MIR pass can replace executable entries with static data.
        let prior_const_values = self
            .state
            .borrow()
            .hir_program()
            .package(&hir_package_id)
            .map(|hir_package| {
                let hir_package = hir_package.borrow();
                (hir_package.const_values(), hir_package.const_block_values())
            });
        let _hir_package = self
            .state
            .borrow()
            .hir_program()
            .package(&hir_package_id)
            .ok_or_else(|| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package {hir_package_id} has no established HIR package"
                ))
            })?;
        self.type_check_program(hir_package_id.clone())
            .await
            .map_err(|error| {
                CompilerDriverError::InternalCompilerError(format!(
                    "package HIR type checking failed: {error}"
                ))
            })?;
        if let Some((const_values, const_block_values)) = prior_const_values {
            let hir_package = self.state.borrow().hir_package_rc(hir_package_id.clone())?;
            for (def_id, value) in const_values {
                hir_package.borrow().record_const_value(def_id, value);
            }
            for (def_id, value) in const_block_values {
                hir_package.borrow().record_const_block_value(def_id, value);
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
        let root_def_ids = current_package
            .borrow()
            .items
            .iter()
            .filter(|item| Self::is_lowering_root(item))
            .map(|item| item.def_id.clone())
            .collect::<Vec<_>>();
        for def_id in root_def_ids {
            Self::lower_package_to_mir(&state, &current_package_id, &mut lowering, def_id).await?;
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
    /// the validated HIR back to the typed AST view consumed by source
    /// backends, then returns. Transpilation has no MIR/LIR phase; native
    /// lowering and comptime execution belong exclusively to `Native`.
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
            let shared_program = state.hir_program();
            let program = shared_program.borrow();
            let lifter = fp_backend::transforms::HirToAstLifter::new(&hir, &program);
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
                    let mut typed = typed.clone();
                    preserve_source_declaration_metadata(&pkg_item.item, &mut typed);
                    pkg_item.item = typed;
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
        return Ok(());
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
        package_id: hir::PackageId,
    ) -> fp_core::Result<()> {
        let comptime_resolver = self.state.borrow().comptime_resolver.clone();
        let hir_program = self.state.borrow().hir_program_rc();
        let executor = self.state.borrow().tasks.clone();
        let package = hir_program.package_rc(&package_id).ok_or_else(|| {
            fp_core::error::Error::from(format!(
                "typechecking package `{package_id}` without published HIR"
            ))
        })?;
        let checker =
            fp_typing::HirTypeChecker::new(package, hir_program, comptime_resolver, executor);
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
            let package = package.borrow();
            Self::emit_typing_diagnostics_to_stderr(&package);
            let combined = package
                .diagnostics
                .get_diagnostics()
                .iter()
                .filter(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
                .map(|diagnostic| diagnostic.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            if self.pipeline != PipelineMode::Transpile {
                return Err(fp_core::error::Error::diagnostic(
                    fp_core::diagnostics::Diagnostic::error(combined),
                ));
            }
        }
        // The checker has been operating on the package handle already
        // installed by `compile_items_to_lir_units`. Keep that one shared
        // package as the authoritative HIR; publishing the checker handle
        // again would obscure the ownership/lifetime boundary and risks
        // replacing state that later comptime queries have recorded.
        let _package = checker.borrow().finish();
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

        let foreign_methods = {
            let state_ref = state.borrow();
            state_ref
                .mir_program()
                .packages
                .values()
                .flat_map(|package| package.borrow().method_hir_defs.keys().cloned().collect::<Vec<_>>())
                .filter(|def_id| def_id.package_id != package_id)
                .collect::<Vec<_>>()
        };
        for method_def_id in foreign_methods {
            let owner_package_id = method_def_id.package_id.clone();
            let owner_hir = state.borrow().hir_program_rc();
            let owner_mir = state.borrow_mut().mir_package_rc(&owner_package_id);
            let mut owner_lowering = HirToMirLowerer::new(
                owner_hir,
                owner_package_id.clone(),
                owner_mir,
            );
            owner_lowering.register_package_items();
            owner_lowering
                .ensure_method_lowered(method_def_id.clone())
                .map_err(|error| {
                    CompilerDriverError::InternalCompilerError(format!(
                        "MIR lowering failed for foreign method {method_def_id}: {error}"
                    ))
                })?;
            let unit = owner_lowering.take_unit();
            state
                .borrow_mut()
                .insert_mir_unit(&owner_package_id, method_def_id.clone(), unit);
            owner_lowering.sync_layout_exports();
        }

        // A comptime body may call a method whose HIR/MIR owner is a
        // dependency package (for example `std::meta::TypeBuilder`). The
        // shared interpreter resolves that call by package and DefId, so
        // publish the dependency units that were materialized by the same
        // HIR-to-MIR request before evaluating the entry.
        let dependency_units = {
            let state_ref = state.borrow();
            state_ref
                .mir_program()
                .packages
                .iter()
                .flat_map(|(id, package)| {
                    package
                        .borrow()
                        .units
                        .keys()
                        .cloned()
                        .filter(|def_id| def_id != &request.def_id)
                        .map(|def_id| (id.clone(), def_id))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        };
        for (dependency_id, def_id) in dependency_units {
            let mut dependency_lir = {
                let state_ref = state.borrow();
                MirToLirLowerer::new(
                    state_ref.data_layout.clone(),
                    state_ref.mir_program_rc(),
                    state_ref.lir_program_rc(),
                )
                .with_package_id(dependency_id.clone())
            };
            Self::lower_package_to_lir_with(state, &dependency_id, &mut dependency_lir, def_id)
                .await?;
        }

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
    /// runtime entrypoint selection and from
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

/// HIR owns checked declaration shape and bodies, while some Rust source
/// attributes are backend-facing metadata with no HIR representation (for
/// example `#[derive(thiserror::Error)]`). Preserve that metadata when the
/// typed splice replaces a source declaration. This is structural, never
/// keyed on declaration names or target-language behavior.
fn preserve_source_declaration_metadata(source: &Item, typed: &mut Item) {
    match (source.kind(), typed.kind_mut()) {
        (ItemKind::DefStruct(source), ItemKind::DefStruct(typed)) => {
            typed.attrs = source.attrs.clone();
        }
        (ItemKind::DefStructural(source), ItemKind::DefStructural(typed)) => {
            typed.attrs = source.attrs.clone();
        }
        (ItemKind::DefEnum(source), ItemKind::DefEnum(typed)) => {
            typed.attrs = source.attrs.clone();
            for (source_variant, typed_variant) in source
                .value
                .variants
                .iter()
                .zip(typed.value.variants.iter_mut())
            {
                typed_variant.attrs = source_variant.attrs.clone();
            }
        }
        (ItemKind::DefType(source), ItemKind::DefType(typed)) => {
            typed.attrs = source.attrs.clone();
        }
        (ItemKind::DefConst(source), ItemKind::DefConst(typed)) => {
            typed.attrs = source.attrs.clone();
        }
        (ItemKind::DefStatic(source), ItemKind::DefStatic(typed)) => {
            typed.attrs = source.attrs.clone();
        }
        _ => {}
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
