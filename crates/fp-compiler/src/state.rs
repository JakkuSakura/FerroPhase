use std::collections::BTreeMap;
use std::rc::Rc;

use fp_core::{
    ast::Value,
    ast::package::PackageId,
    ast::program::AstProgram,
    executor::ExecutorHandle,
    hir, lir, mir,
};
use fp_typing::TypingContext;
use fp_core::hir::PackageTypes;

use crate::error::CompilerDriverError;
use crate::ConstValueId;

pub struct CompilerState {
    /// Every package's own HIR published so far this session — mirrors
    /// `mir_program`/`lir_program` below; keyed internally by `hir::PackageId`
    /// (see `HirProgram`/`ProgramTypes`'s own shape), not the compiler's
    /// surface `ast::package::PackageId`, since a `hir::Package`'s identity
    /// is always the former.
    hir_program: hir::HirProgram,
    hir_program_types: hir::ProgramTypes,
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
    /// Renamed, ready-to-execute `LirBlob`s built by `select_entrypoint`
    /// (the compiled package's own artifacts, with the resolved entrypoint
    /// function renamed to its bare linkage name) — a distinct concern from
    /// `lir_program`: this is one specific executable variant cached by a
    /// `lir::LirPath`, not per-package/per-`DefId` lowering output, so it
    /// doesn't fit the `lir_program` hierarchy above.
    runtime_programs: std::collections::HashMap<lir::LirPath, lir::LirBlob>,
    runtime_entrypoints: std::collections::HashMap<lir::LirPath, hir::DefId>,
    const_values: BTreeMap<ConstValueId, Value>,
    /// MIR-level const values for HIR→MIR lowering seed.
    resolved_const_values: BTreeMap<String, mir::Constant>,
    pub typing_ctx: std::rc::Rc<TypingContext>,
    /// The compiled-package registry for this compilation session — moved
    /// here from `TypingContext` (which held it as a bare pass-through
    /// field with no forwarding methods of its own) since this is squarely
    /// driver-owned state, not typing-owned.
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
    /// comptime work. Lives here, not on
    /// `TypingContext`, because scheduling ("what task runs next") is the
    /// driver's concern, not typing's — `TypingContext` only holds typing
    /// data. `Rc`, not `Rc<RefCell<_>>`: `CompilerExecutor` is already internally
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
            hir_program_types: hir::ProgramTypes::new(),
            mir_program: mir::MirProgram::new(),
            lir_program: lir::LirProgram::new(),
            runtime_programs: std::collections::HashMap::new(),
            runtime_entrypoints: std::collections::HashMap::new(),
            const_values: BTreeMap::new(),
            resolved_const_values: BTreeMap::new(),
            typing_ctx: std::rc::Rc::new(TypingContext::new()),
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

    pub fn insert_hir_typeck(&mut self, hir_package_id: hir::PackageId, results: PackageTypes) {
        self.hir_program_types.insert_package(hir_package_id, results);
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

    /// Every `MirCodeUnit` this package has produced so far, folded into
    /// one flat `mir::MirModule` — the view `LirGenerator`/the interpreter
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

    /// Splits a whole flat `lir::LirBlob` (e.g. `LirGenerator::transform`'s
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

    pub fn insert_runtime_program(&mut self, lir_path: lir::LirPath, program: lir::LirBlob) {
        self.runtime_programs.insert(lir_path, program);
    }

    pub fn runtime_program(&self, lir_path: &lir::LirPath) -> Result<&lir::LirBlob, CompilerDriverError> {
        self.runtime_programs
            .get(lir_path)
            .ok_or_else(|| CompilerDriverError::MissingLir(format!("{lir_path:?}")))
    }

    pub fn insert_runtime_entrypoint(&mut self, lir_path: lir::LirPath, def_id: hir::DefId) {
        self.runtime_entrypoints.insert(lir_path, def_id);
    }

    pub fn runtime_entrypoint(&self, lir_path: &lir::LirPath) -> Result<hir::DefId, CompilerDriverError> {
        self.runtime_entrypoints
            .get(lir_path)
            .copied()
            .ok_or_else(|| {
                CompilerDriverError::Interpreter("program has no explicit entrypoint".to_string())
            })
    }

    pub fn insert_const_value(&mut self, value_id: ConstValueId, value: Value) {
        self.const_values.insert(value_id, value);
    }

    pub fn insert_resolved_const_value(&mut self, key: impl Into<String>, value: mir::Constant) {
        self.resolved_const_values.insert(key.into(), value);
    }

    /// Record a comptime-resolved named const's value into its own
    /// package's `PackageTypes::const_values`, keyed by the const item's
    /// own stable `DefId` — replaces the old string-name-keyed
    /// `TypingContext.resolved_consts` broadcast.
    pub fn insert_resolved_const(&mut self, package_hir_id: hir::PackageId, def_id: hir::DefId, value: Value) {
        self.hir_program_types
            .package_or_default(package_hir_id)
            .borrow_mut()
            .const_values
            .insert(def_id, value);
    }

    pub fn set_backend_capabilities(&mut self, capabilities: fp_core::capabilities::LanguageCapabilities) {
        self.backend_capabilities = capabilities;
    }

    pub fn backend_capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        self.backend_capabilities
    }

    pub fn hir(&self, package_id: hir::PackageId) -> Result<std::rc::Rc<hir::HirPackage>, CompilerDriverError> {
        self.hir_program
            .package(package_id)
            .cloned()
            .ok_or_else(|| CompilerDriverError::MissingHir(format!("{package_id:?}")))
    }

    pub fn hir_typeck(&self, package_id: hir::PackageId) -> Result<PackageTypes, CompilerDriverError> {
        self.hir_program_types
            .package(package_id)
            .map(|types| types.borrow().clone())
            .ok_or_else(|| CompilerDriverError::MissingHir(format!("{package_id:?}")))
    }

    /// Every package's own typed results compiled so far — used by
    /// `drain_driver` to report typing diagnostics, which live on each
    /// package's own durable `PackageTypes` (see its `diagnostics` field's
    /// doc comment), not on the driver's scratch, per-package-swapped
    /// `TypingContext`.
    pub fn all_package_types(&self) -> impl Iterator<Item = std::rc::Rc<std::cell::RefCell<PackageTypes>>> + '_ {
        self.hir_program_types.packages.values().cloned()
    }

    pub fn const_value(&self, value_id: &ConstValueId) -> Result<&Value, CompilerDriverError> {
        self.const_values
            .get(value_id)
            .ok_or_else(|| CompilerDriverError::MissingConstValue(value_id.clone()))
    }

    pub fn resolved_const_values(&self) -> impl Iterator<Item = (&str, &mir::Constant)> {
        self.resolved_const_values
            .iter()
            .map(|(key, value)| (key.as_str(), value))
    }

    pub fn const_value_len(&self) -> usize {
        self.const_values.len()
    }

}
