use std::collections::BTreeMap;
use std::rc::Rc;

use fp_core::{
    ast::Value,
    ast::workspace::WorkspaceContext,
    executor::ExecutorHandle,
    hir, lir, mir,
};
use fp_typing::TypingContext;
use fp_core::hir::PackageTypes;

use crate::error::CompilerDriverError;
use crate::{BytecodeId, ConstValueId, HirId, LirId, MirId, RuntimeValueId};

pub struct CompilerState {
    hir: BTreeMap<HirId, hir::Package>,
    hir_typeck: BTreeMap<HirId, PackageTypes>,
    mir: BTreeMap<MirId, mir::Program>,
    lir: BTreeMap<LirId, lir::LirProgram>,
    runtime_entrypoints: BTreeMap<LirId, hir::DefId>,
    const_values: BTreeMap<ConstValueId, Value>,
    /// MIR-level const values for HIR→MIR lowering seed.
    resolved_const_values: BTreeMap<String, mir::Constant>,
    pub typing_ctx: std::rc::Rc<TypingContext>,
    /// The compiled-package registry for this compilation session — moved
    /// here from `TypingContext` (which held it as a bare pass-through
    /// field with no forwarding methods of its own) since this is squarely
    /// driver-owned state, not typing-owned.
    pub workspace: Rc<WorkspaceContext>,
    /// Target ABI data shared by typing-triggered comptime blocks and
    /// normal MIR-to-LIR lowering for this compilation session — same
    /// rationale as `workspace` above.
    pub data_layout: lir::LirDataLayout,
    runtime_values: BTreeMap<RuntimeValueId, Value>,
    /// What the requested output target can express directly — see
    /// `fp_core::capabilities::LanguageCapabilities`. Defaults to
    /// `NATIVE` (nothing first-class); `fp-cli` sets the real value per
    /// target language before compiling (`set_capabilities`).
    capabilities: fp_core::capabilities::LanguageCapabilities,
    bytecode: BTreeMap<BytecodeId, fp_bytecode::BytecodeProgram>,
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
            Rc::new(WorkspaceContext::new(std::sync::Arc::new(
                fp_core::ast::package::provider::EmptyProvider,
            ))),
        )
    }

    pub fn with_workspace(
        data_layout: lir::LirDataLayout,
        tasks: ExecutorHandle,
        workspace: Rc<WorkspaceContext>,
    ) -> Self {
        Self {
            hir: BTreeMap::new(),
            hir_typeck: BTreeMap::new(),
            mir: BTreeMap::new(),
            lir: BTreeMap::new(),
            runtime_entrypoints: BTreeMap::new(),
            const_values: BTreeMap::new(),
            resolved_const_values: BTreeMap::new(),
            typing_ctx: std::rc::Rc::new(TypingContext::new()),
            workspace,
            data_layout,
            runtime_values: BTreeMap::new(),
            capabilities: fp_core::capabilities::LanguageCapabilities::NATIVE,
            bytecode: BTreeMap::new(),
            tasks,
        }
    }

    pub fn insert_hir(&mut self, hir_id: HirId, hir: hir::Package) {
        self.hir.insert(hir_id, hir);
    }

    pub fn insert_hir_typeck(&mut self, hir_id: HirId, results: PackageTypes) {
        self.hir_typeck.insert(hir_id, results);
    }

    pub fn insert_mir(&mut self, mir_id: MirId, mir: mir::Program) {
        self.mir.insert(mir_id, mir);
    }

    pub fn insert_lir(&mut self, lir_id: LirId, lir: lir::LirProgram) {
        self.lir.insert(lir_id, lir);
    }

    pub fn insert_runtime_entrypoint(&mut self, lir_id: LirId, def_id: hir::DefId) {
        self.runtime_entrypoints.insert(lir_id, def_id);
    }

    pub fn runtime_entrypoint(&self, lir_id: &LirId) -> Result<hir::DefId, CompilerDriverError> {
        self.runtime_entrypoints
            .get(lir_id)
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
    pub fn insert_resolved_const(&mut self, package_hir_id: HirId, def_id: hir::DefId, value: Value) {
        self.hir_typeck
            .entry(package_hir_id)
            .or_default()
            .const_values
            .insert(def_id, value);
    }

    pub fn insert_runtime_value(&mut self, value_id: RuntimeValueId, value: Value) {
        self.runtime_values.insert(value_id, value);
    }

    pub fn set_capabilities(&mut self, capabilities: fp_core::capabilities::LanguageCapabilities) {
        self.capabilities = capabilities;
    }

    pub fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        self.capabilities
    }

    pub fn hir(&self, hir_id: &HirId) -> Result<&hir::Package, CompilerDriverError> {
        self.hir
            .get(hir_id)
            .ok_or_else(|| CompilerDriverError::MissingHir(hir_id.clone()))
    }

    pub fn hir_typeck(&self, hir_id: &HirId) -> Result<&PackageTypes, CompilerDriverError> {
        self.hir_typeck
            .get(hir_id)
            .ok_or_else(|| CompilerDriverError::MissingHir(hir_id.clone()))
    }

    /// Every package's own typed results compiled so far — used by
    /// `drain_driver` to report typing diagnostics, which live on each
    /// package's own durable `PackageTypes` (see its `diagnostics` field's
    /// doc comment), not on the driver's scratch, per-package-swapped
    /// `TypingContext`.
    pub fn all_package_types(&self) -> impl Iterator<Item = &PackageTypes> {
        self.hir_typeck.values()
    }

    pub fn mir(&self, mir_id: &MirId) -> Result<&mir::Program, CompilerDriverError> {
        self.mir
            .get(mir_id)
            .ok_or_else(|| CompilerDriverError::MissingMir(mir_id.clone()))
    }

    pub fn lir(&self, lir_id: &LirId) -> Result<&lir::LirProgram, CompilerDriverError> {
        self.lir
            .get(lir_id)
            .ok_or_else(|| CompilerDriverError::MissingLir(lir_id.clone()))
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

    pub fn runtime_value(&self, value_id: &RuntimeValueId) -> Result<&Value, CompilerDriverError> {
        self.runtime_values
            .get(value_id)
            .ok_or_else(|| CompilerDriverError::MissingRuntimeValue(value_id.clone()))
    }

    pub fn hir_len(&self) -> usize {
        self.hir.len()
    }

    pub fn mir_len(&self) -> usize {
        self.mir.len()
    }

    pub fn lir_len(&self) -> usize {
        self.lir.len()
    }

    pub fn const_value_len(&self) -> usize {
        self.const_values.len()
    }

    pub fn runtime_value_len(&self) -> usize {
        self.runtime_values.len()
    }

    pub fn insert_bytecode(&mut self, id: BytecodeId, program: fp_bytecode::BytecodeProgram) {
        self.bytecode.insert(id, program);
    }

    pub fn bytecode_program(
        &self,
        id: &BytecodeId,
    ) -> Result<&fp_bytecode::BytecodeProgram, CompilerDriverError> {
        self.bytecode
            .get(id)
            .ok_or_else(|| CompilerDriverError::MissingBytecode(id.clone()))
    }
}
