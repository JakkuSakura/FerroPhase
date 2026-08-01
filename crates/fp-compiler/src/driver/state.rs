use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;

use fp_core::{
    ast::{Expr, ExprId, File, Item, Value},
    hir, lir, mir,
    module::path::QualifiedPath,
    module::resolution::ModuleResolutionContext,
};
use fp_typing::TypingContext;

use crate::driver::CompilerDriverError;
use crate::module_resolution::CompilerModuleResolver;
use crate::scheduler::{AstId, BytecodeId, ConstValueId, HirId, LirId, MirId, RuntimeValueId, TypedAstId};

pub struct CompilerState {
    ast: BTreeMap<AstId, File>,
    typed_ast: BTreeMap<TypedAstId, File>,
    hir: BTreeMap<HirId, hir::Program>,
    mir: BTreeMap<MirId, mir::Program>,
    lir: BTreeMap<LirId, lir::LirProgram>,
    const_values: BTreeMap<ConstValueId, Value>,
    /// MIR-level const values for HIR→MIR lowering seed.
    resolved_const_values: BTreeMap<String, mir::Constant>,
    pub typing_ctx: std::rc::Rc<TypingContext>,
    runtime_values: BTreeMap<RuntimeValueId, Value>,
    lossy: bool,
    module_resolver: Option<Arc<dyn CompilerModuleResolver>>,
    module_resolutions: BTreeMap<AstId, ModuleResolutionContext>,
    pub(crate) generic_instantiations: HashSet<String>,
    bytecode: BTreeMap<BytecodeId, fp_bytecode::BytecodeProgram>,
    /// Memoized `(origin module path, matched items)` per cross-crate struct
    /// path (e.g. `std::meta::TypeBuilder`), populated by
    /// `CompilerDriver::collect_cross_crate_items`. Avoids re-scanning a
    /// workspace crate's `PackageCrate::items` and re-deriving the same
    /// group every time a different compile unit references the same
    /// cross-crate struct — mirrors `generic_instantiations`' existing dedup
    /// role for generic monomorphization.
    pub(crate) cross_crate_items_cache: HashMap<QualifiedPath, (QualifiedPath, Vec<Item>)>,
    /// The one shared task pool every suspendable unit of driver work runs
    /// through: per-const/per-type-alias comptime resolution (spawned by
    /// `fp-typing`'s `predeclare_item` via `HirTypeInferencer::tasks`, a
    /// clone of this same handle), the per-compile-unit module-typing task,
    /// and generic-monomorphization-ready signals (see
    /// `CompilerDriver::run_pool_to_idle`). Lives here, not on
    /// `TypingContext`, because scheduling ("what task runs next") is the
    /// driver's concern, not typing's — `TypingContext` only holds typing
    /// data. `Rc`, not `Rc<RefCell<_>>`: `Executor` is already internally
    /// interior-mutable (its own methods take `&self`, specifically so a
    /// task can reentrantly `spawn`/`contains`-check it from within its own
    /// poll).
    pub tasks: std::rc::Rc<fp_core::executor::Executor<fp_core::error::Result<()>>>,
}

impl CompilerState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_ast(&mut self, ast_id: AstId, ast: File) {
        self.ast.insert(ast_id, ast);
    }

    pub fn insert_typed_ast(&mut self, typed_ast_id: TypedAstId, ast: File) {
        self.typed_ast.insert(typed_ast_id, ast);
    }

    pub fn insert_hir(&mut self, hir_id: HirId, hir: hir::Program) {
        self.hir.insert(hir_id, hir);
    }

    pub fn insert_mir(&mut self, mir_id: MirId, mir: mir::Program) {
        self.mir.insert(mir_id, mir);
    }

    pub fn insert_lir(&mut self, lir_id: LirId, lir: lir::LirProgram) {
        self.lir.insert(lir_id, lir);
    }

    pub fn insert_const_value(&mut self, value_id: ConstValueId, value: Value) {
        self.const_values.insert(value_id, value);
    }

    pub fn insert_resolved_const_value(&mut self, key: impl Into<String>, value: mir::Constant) {
        self.resolved_const_values.insert(key.into(), value);
    }

    /// Write a typed comptime value into the shared typing context so the
    /// typer can see it on the next pass.
    pub fn insert_typing_const(&mut self, key: impl Into<String>, value: Value) {
        let key = key.into();
        self.typing_ctx.resolved_consts.borrow_mut().insert(key.clone(), value);
        self.typing_ctx.wake_comptime(&key);
    }

    pub fn insert_expr_resolution_source(&mut self, expr_id: ExprId, expr: Expr) {
        self.typing_ctx.expr_resolutions.borrow_mut().insert_source(expr_id, expr);
    }

    pub fn insert_expr_resolution_value(&mut self, expr_id: ExprId, value: Value) {
        self.typing_ctx.expr_resolutions.borrow_mut().insert_value(expr_id, value);
    }

    pub fn insert_runtime_value(&mut self, value_id: RuntimeValueId, value: Value) {
        self.runtime_values.insert(value_id, value);
    }

    pub fn set_module_resolver(&mut self, resolver: Arc<dyn CompilerModuleResolver>) {
        self.module_resolver = Some(resolver);
    }

    pub fn prepare_module_resolution(
        &mut self,
        ast_id: AstId,
        input: &Path,
    ) -> Result<(), CompilerDriverError> {
        let Some(resolver) = self.module_resolver.as_ref() else {
            return Ok(());
        };
        let context = resolver.resolve_context(input)?;
        self.module_resolutions.insert(ast_id, context);
        Ok(())
    }

    pub fn set_lossy(&mut self, lossy: bool) {
        self.lossy = lossy;
    }

    pub fn ast(&self, ast_id: &AstId) -> Result<&File, CompilerDriverError> {
        self.ast
            .get(ast_id)
            .ok_or_else(|| CompilerDriverError::MissingAst(ast_id.clone()))
    }

    pub fn typed_ast(&self, typed_ast_id: &TypedAstId) -> Result<&File, CompilerDriverError> {
        self.typed_ast
            .get(typed_ast_id)
            .ok_or_else(|| CompilerDriverError::MissingTypedAst(typed_ast_id.clone()))
    }

    pub fn hir(&self, hir_id: &HirId) -> Result<&hir::Program, CompilerDriverError> {
        self.hir
            .get(hir_id)
            .ok_or_else(|| CompilerDriverError::MissingHir(hir_id.clone()))
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

    pub fn lossy(&self) -> bool {
        self.lossy
    }

    pub fn module_resolution(&self, ast_id: &AstId) -> Option<&ModuleResolutionContext> {
        self.module_resolutions.get(ast_id)
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

    pub fn bytecode_program(&self, id: &BytecodeId) -> Result<&fp_bytecode::BytecodeProgram, CompilerDriverError> {
        self.bytecode
            .get(id)
            .ok_or_else(|| CompilerDriverError::MissingBytecode(id.clone()))
    }
}

impl Default for CompilerState {
    fn default() -> Self {
        Self {
            ast: BTreeMap::new(),
            typed_ast: BTreeMap::new(),
            hir: BTreeMap::new(),
            mir: BTreeMap::new(),
            lir: BTreeMap::new(),
            const_values: BTreeMap::new(),
            resolved_const_values: BTreeMap::new(),
            typing_ctx: std::rc::Rc::new(TypingContext::new(
                std::rc::Rc::new(fp_core::workspace::WorkspaceContext::new()))),
            runtime_values: BTreeMap::new(),
            lossy: false,
            module_resolver: None,
            module_resolutions: BTreeMap::new(),
            generic_instantiations: HashSet::new(),
            bytecode: BTreeMap::new(),
            cross_crate_items_cache: HashMap::new(),
            tasks: std::rc::Rc::new(fp_core::executor::Executor::new()),
        }
    }
}
