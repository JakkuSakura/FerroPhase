use std::collections::BTreeMap;
use std::rc::Rc;

use fp_core::{
    ast::Value,
    ast::package::PackageId,
    ast::program::AstProgram,
    executor::ExecutorHandle,
    hir, lir, mir,
};
use fp_typing::ComptimeResolver;

use crate::error::CompilerDriverError;
use crate::ConstValueId;

pub struct CompilerState {
    /// Every package's own HIR published so far this session — mirrors
    /// `mir_program`/`lir_program` below; keyed internally by `hir::PackageId`
    /// (see `HirProgram`'s own shape), not the compiler's surface
    /// `ast::package::PackageId`, since a `hir::Package`'s identity is
    /// always the former. Typed results (expr/pat types, resolutions,
    /// diagnostics, ...) live directly on each published `HirPackage` —
    /// there's no separate typed-results table to keep in sync.
    hir_program: hir::HirProgram,
    /// The whole workspace `HirProgram` a package's `TypingShared` is
    /// checking against, published only for the duration of that package's
    /// typecheck (see `fp_typing::TypingShared::program_handle`) — this is
    /// what lets a mid-typecheck `ComptimeRequest` (which only carries
    /// `package_id`/`def_id`, not its own `Rc<HirPackage>`) resolve its own
    /// still-unpublished package back by id.
    in_progress_hir_program: Option<Rc<hir::HirProgram>>,
    /// Every package's MIR content produced so far this session, one
    /// `mir::MirCodeUnit` per top-level `DefId` (see `mir::MirPackage`'s
    /// own doc comment) — replaces the old `BTreeMap<MirId, MirModule>`,
    /// which stored one whole flattened program per artifact-id and had no
    /// way to update just the one item a resolved comptime value actually
    /// affects.
    mir_program: mir::MirProgram,
    /// Mirrors `mir_program` for LIR — one `lir::LirProgram`, a collection
    /// of `LirPackage`s, each already a collection of `LirCodeUnit`s (via
    /// its own `own_artifacts: LirUnitTable`, keyed by `Name`).
    lir_program: lir::LirProgram,
    /// The one renamed entrypoint function `select_entrypoint` builds per
    /// selection (the resolved entrypoint, renamed to its bare linkage
    /// name) — just that one `LirCodeUnit`, not a whole duplicated
    /// `LirBlob`: every other function it might call already lives in
    /// `lir_program`'s own per-package storage, so only the rename itself
    /// needs its own slot. `runtime_blob` reassembles the full executable
    /// blob on demand by substituting this into the package's own flattened
    /// LIR. Cached by `lir::LirPath` since it's one specific
    /// package+entrypoint selection, not per-`DefId` lowering output, so it
    /// doesn't fit the `lir_program` hierarchy above.
    runtime_programs: std::collections::HashMap<lir::LirPath, lir::LirCodeUnit>,
    runtime_entrypoints: std::collections::HashMap<lir::LirPath, hir::DefId>,
    const_values: BTreeMap<ConstValueId, Value>,
    /// This package's comptime resolver, wired up by the driver
    /// (`make_comptime_resolver`) before `TypingShared` exists — a
    /// package's HIR isn't generated yet at that point, so there's nowhere
    /// else to stash it until `type_check_program` builds the real
    /// `TypingShared` and hands the resolver straight through.
    pub comptime_resolver: Option<ComptimeResolver>,
    /// The compiled-package registry for this compilation session.
    pub workspace: Rc<AstProgram>,
    /// Target ABI data shared by typing-triggered comptime blocks and
    /// normal MIR-to-LIR lowering for this compilation session — same
    /// rationale as `workspace` above.
    pub data_layout: lir::LirDataLayout,
    /// What the active `TargetBackend` can express directly — see
    /// `fp_core::capabilities::LanguageCapabilities`. Defaults to `NATIVE`
    /// (nothing first-class); `fp-cli` reads the real value off its
    /// already-constructed backend (`TargetBackend::capabilities`) and
    /// sets it here before compiling (`set_backend_capabilities`).
    backend_capabilities: fp_core::capabilities::LanguageCapabilities,
    /// The one shared task pool every suspendable unit of driver work runs
    /// through: per-compile-unit HIR typing tasks and compiler-owned
    /// comptime work. Lives here since scheduling ("what task runs next")
    /// is the driver's concern, not typing's. `Rc`, not `Rc<RefCell<_>>`:
    /// `CompilerExecutor` is already internally
    /// interior-mutable (its own methods take `&self`, specifically so a
    /// task can reentrantly `spawn`/`contains`-check it from within its own
    /// poll).
    pub(crate) tasks: ExecutorHandle,
}

impl CompilerState {
    pub fn new(data_layout: lir::LirDataLayout, tasks: ExecutorHandle) -> Self {
        Self::with_workspace(
            data_layout,
            tasks,
            Rc::new(AstProgram::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
        )
    }

    pub fn with_workspace(
        data_layout: lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<AstProgram>,
    ) -> Self {
        Self {
            hir_program: hir::HirProgram::new(),
            in_progress_hir_program: None,
            mir_program: mir::MirProgram::new(),
            lir_program: lir::LirProgram::new(),
            runtime_programs: std::collections::HashMap::new(),
            runtime_entrypoints: std::collections::HashMap::new(),
            const_values: BTreeMap::new(),
            comptime_resolver: None,
            workspace,
            data_layout,
            backend_capabilities: fp_core::capabilities::LanguageCapabilities::NATIVE,
            tasks,
        }
    }

    /// Publishes `package` under its own `id` — `HirProgram::add_package`
    /// already keys by that, so no separate id parameter is needed.
    pub fn insert_hir(&mut self, package: hir::HirPackage) {
        self.hir_program.add_package(std::rc::Rc::new(package));
    }

    /// Publishes `program` (every already-published dependency, plus the
    /// package currently being type-checked) for the duration of that
    /// package's typecheck — see `in_progress_hir_program`'s doc comment.
    pub fn set_in_progress_hir_program(&mut self, program: Option<Rc<hir::HirProgram>>) {
        self.in_progress_hir_program = program;
    }

    /// The whole workspace `HirProgram` a package's typecheck is currently
    /// in progress against, if any — the mid-typecheck counterpart of
    /// `hir_program`'s already-published packages, for resolving a
    /// `ComptimeRequest` whose own package hasn't finished typechecking
    /// (and so isn't in `hir_program` yet).
    pub fn in_progress_hir_program(&self) -> Option<Rc<hir::HirProgram>> {
        self.in_progress_hir_program.clone()
    }

    /// Records `def_id`'s own lowered content — the only way `mir_program`
    /// is ever written, so re-lowering one item after a comptime value
    /// resolves is always this exact call with a fresh unit (see
    /// `mir::MirCodeUnit`'s doc comment), never a partial in-place edit.
    pub fn insert_mir_unit(
        &mut self,
        package_id: &PackageId,
        def_id: hir::DefId,
        unit: mir::MirCodeUnit,
    ) {
        self.mir_program.package_mut(package_id).insert_unit(def_id, unit);
    }

    pub fn mir_unit(&self, package_id: &PackageId, def_id: hir::DefId) -> Option<&mir::MirCodeUnit> {
        self.mir_program.package(package_id)?.unit(def_id)
    }

    /// Read-only view of every package's MIR compiled so far this session —
    /// used by `MirToLirLowerer`'s lazy signature resolver (see
    /// `CompilerDriver::new_lir_generator`) to look a callee's signature up
    /// by `DefId`, in this package first and then every other loaded
    /// package, without requiring a whole-program predeclare sweep.
    pub fn mir_program(&self) -> &mir::MirProgram {
        &self.mir_program
    }

    /// Folds `struct_fields`/`adt_defs` produced while lowering
    /// `package_id`'s HIR into its `mir::MirPackage` — the per-package
    /// tables `MirToLirLowerer` reads alongside the package's lowered
    /// units.
    pub fn extend_mir_package(
        &mut self,
        package_id: &PackageId,
        struct_fields: impl IntoIterator<Item = (mir::DefId, Vec<mir::Ty>)>,
        adt_defs: impl IntoIterator<Item = (hir::DefId, mir::ty::AdtDef)>,
    ) {
        let package = self.mir_program.package_mut(package_id);
        package.extend_struct_fields(struct_fields);
        package.extend_adt_defs(adt_defs);
    }

    /// Resets `package_id`'s LIR artifacts back to empty — used when
    /// re-lowering a whole package after a comptime value resolves
    /// (`CompilerDriver::relower_cached_lir_units`), since
    /// `LirUnitTable::add_program` errors on a duplicate artifact name
    /// rather than silently overwriting the previous lowering.
    pub fn reset_lir_package(&mut self, package_id: &PackageId) {
        self.lir_program
            .packages
            .insert(package_id.clone(), lir::LirPackage::new(self.data_layout.clone()));
    }

    /// Every `MirCodeUnit` this package has produced so far, folded into
    /// one flat `mir::MirModule` — the view `MirToLirLowerer`/the interpreter
    /// still need. Empty (not an error) if the package has no units yet.
    pub fn mir_module(&self, package_id: &PackageId) -> mir::MirModule {
        self.mir_program
            .package(package_id)
            .map(|package| package.flatten())
            .unwrap_or_default()
    }

    /// Records one LIR artifact into `package_id`'s own unit table — the
    /// only way `lir_program` is ever written, mirroring `insert_mir_unit`.
    pub fn insert_lir_unit(
        &mut self,
        package_id: &PackageId,
        unit: lir::LirCodeUnit,
    ) -> Result<(), CompilerDriverError> {
        self.lir_program
            .package_mut(package_id, &self.data_layout)
            .own_artifacts
            .add_artifact(unit)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))
    }

    /// Splits a whole flat `lir::LirBlob` (e.g. `MirToLirLowerer::transform`'s
    /// output) back into its individual artifacts and records each one via
    /// `insert_lir_unit` — `LirUnitTable::add_program` already does exactly
    /// this splitting, so this just routes to it instead of duplicating
    /// the per-`LirCodeUnitKind` match here.
    pub fn insert_lir_blob(
        &mut self,
        package_id: &PackageId,
        module_path: fp_core::ast::path::QualifiedPath,
        blob: lir::LirBlob,
    ) -> Result<(), CompilerDriverError> {
        self.lir_program
            .package_mut(package_id, &self.data_layout)
            .own_artifacts
            .add_program(package_id.clone(), module_path, blob)
            .map_err(|error| CompilerDriverError::Core(error.to_string().into()))
    }

    /// Every artifact `package_id` has produced so far, folded into one
    /// flat `lir::LirBlob` — mirrors `mir_module`. Empty (not an error) if
    /// the package has no artifacts yet.
    pub fn lir_blob(&self, package_id: &PackageId) -> lir::LirBlob {
        self.lir_program
            .package(package_id)
            .map(|package| package.own_artifacts.to_blob())
            .unwrap_or_else(|| lir::LirBlob::new(self.data_layout.clone()))
    }

    /// The whole session's `lir::LirProgram` — used by callers (`fp-cli`'s
    /// `run_compile_pipeline`) that need to merge a package's LIR together
    /// with every dependency's (`LirProgram::merged_blob_for_package`)
    /// before handing it to a `TargetBackend`.
    pub fn lir_program(&self) -> &lir::LirProgram {
        &self.lir_program
    }

    /// The whole session's `hir::HirProgram` — used by callers
    /// (`HirToAstLifter`, `AstToHirLowerer::with_hir_program`) that need
    /// cross-package HIR lookups (`find_export`, `find_hir_impl_method`,
    /// `find_hir_enum_for_variant`, ...), now that `AstProgram` no longer
    /// carries HIR content itself.
    pub fn hir_program(&self) -> &hir::HirProgram {
        &self.hir_program
    }

    /// Records the one renamed entrypoint function `select_entrypoint`
    /// produced — a single `LirCodeUnit`, not a whole duplicated `LirBlob`:
    /// every other function the entrypoint might call already lives in
    /// this package's own `lir_program` entry, so only the one thing that
    /// actually changed (the rename) needs its own storage.
    pub fn insert_runtime_program(&mut self, lir_path: lir::LirPath, unit: lir::LirCodeUnit) {
        self.runtime_programs.insert(lir_path, unit);
    }

    /// Assembles the full, executable `LirBlob` for a selected entrypoint:
    /// `package_id`'s own flattened LIR (`lir_blob`), with `def_id`'s
    /// original (mangled-named) function swapped out for the one renamed
    /// `LirCodeUnit` `select_entrypoint` recorded under `lir_path`.
    pub fn runtime_blob(
        &self,
        package_id: &PackageId,
        lir_path: &lir::LirPath,
        def_id: hir::DefId,
    ) -> Result<lir::LirBlob, CompilerDriverError> {
        let unit = self.runtime_programs.get(lir_path).ok_or_else(|| {
            CompilerDriverError::MissingLir(format!("{lir_path:?}"))
        })?;
        let lir::LirCodeUnitKind::Function(renamed) = &unit.kind else {
            return Err(CompilerDriverError::Interpreter(
                "runtime entrypoint unit is not a function".to_string(),
            ));
        };
        let mut blob = self.lir_blob(package_id);
        blob.functions.retain(|function| function.def_id != Some(def_id.clone()));
        blob.functions.push(renamed.clone());
        Ok(blob)
    }

    pub fn insert_runtime_entrypoint(&mut self, lir_path: lir::LirPath, def_id: hir::DefId) {
        self.runtime_entrypoints.insert(lir_path, def_id);
    }

    pub fn runtime_entrypoint(&self, lir_path: &lir::LirPath) -> Result<hir::DefId, CompilerDriverError> {
        self.runtime_entrypoints
            .get(lir_path)
            .cloned()
            .ok_or_else(|| {
                CompilerDriverError::Interpreter("program has no explicit entrypoint".to_string())
            })
    }

    pub fn insert_const_value(&mut self, value_id: ConstValueId, value: Value) {
        self.const_values.insert(value_id, value);
    }

    pub fn set_backend_capabilities(&mut self, capabilities: fp_core::capabilities::LanguageCapabilities) {
        self.backend_capabilities = capabilities;
    }

    pub fn backend_capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        self.backend_capabilities
    }

    pub fn hir(&self, package_id: hir::PackageId) -> Result<hir::HirPackage, CompilerDriverError> {
        self.hir_program
            .package(&package_id)
            .cloned()
            .ok_or_else(|| CompilerDriverError::MissingHir(format!("{package_id:?}")))
    }

    /// Every package's own HIR compiled so far — used by `drain_driver` to
    /// report typing diagnostics, which live directly on each package
    /// (see `hir::HirPackage::diagnostics`'s doc comment), not on the
    /// driver's scratch, per-package `TypingShared`.
    pub fn all_packages(&self) -> impl Iterator<Item = &Rc<hir::HirPackage>> {
        self.hir_program.packages.values()
    }

    pub fn const_value(&self, value_id: &ConstValueId) -> Result<&Value, CompilerDriverError> {
        self.const_values
            .get(value_id)
            .ok_or_else(|| CompilerDriverError::MissingConstValue(value_id.clone()))
    }

    pub fn const_value_len(&self) -> usize {
        self.const_values.len()
    }

}
