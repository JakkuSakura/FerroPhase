use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;

use fp_core::{ast::Node, ast::Value, hir, lir, mir, module::resolution::ModuleResolutionContext};
use fp_typing::TypingDiagnostic;

use crate::driver::CompilerDriverError;
use crate::module_resolution::CompilerModuleResolver;
use crate::scheduler::{AstId, ConstValueId, HirId, LirId, MirId, RuntimeValueId, TypedAstId};

pub struct CompilerState {
    ast: BTreeMap<AstId, Node>,
    typed_ast: BTreeMap<TypedAstId, Node>,
    hir: BTreeMap<HirId, hir::Program>,
    mir: BTreeMap<MirId, mir::Program>,
    lir: BTreeMap<LirId, lir::LirProgram>,
    const_values: BTreeMap<ConstValueId, Value>,
    runtime_values: BTreeMap<RuntimeValueId, Value>,
    typing_diagnostics: Vec<TypingDiagnostic>,
    module_resolver: Option<Arc<dyn CompilerModuleResolver>>,
    module_resolutions: BTreeMap<AstId, ModuleResolutionContext>,
    /// Per-AST count of unresolved comptime needs. Decremented on each resolve.
    pub(crate) comptime_pending: HashMap<AstId, usize>,
    /// ASTs whose initial comptime needs have been counted (to avoid double-counting on retype).
    pub(crate) comptime_seeded: HashSet<AstId>,
    /// Maps HirId to the TypedAstId that was lowered to produce it.
    pub(crate) hir_to_typed_ast: BTreeMap<HirId, TypedAstId>,
    /// Maps MirId to the TypedAstId that was lowered to produce it.
    pub(crate) mir_to_typed_ast: BTreeMap<MirId, TypedAstId>,
    /// Maps LirId to the TypedAstId that was lowered to produce it.
    pub(crate) lir_to_typed_ast: BTreeMap<LirId, TypedAstId>,
}

impl CompilerState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_ast(&mut self, ast_id: AstId, ast: Node) {
        self.ast.insert(ast_id, ast);
    }

    pub fn insert_typed_ast(&mut self, typed_ast_id: TypedAstId, ast: Node) {
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

    pub fn extend_typing_diagnostics(
        &mut self,
        diagnostics: impl IntoIterator<Item = TypingDiagnostic>,
    ) {
        self.typing_diagnostics.extend(diagnostics);
    }

    pub fn ast(&self, ast_id: &AstId) -> Result<&Node, CompilerDriverError> {
        self.ast
            .get(ast_id)
            .ok_or_else(|| CompilerDriverError::MissingAst(ast_id.clone()))
    }

    pub fn typed_ast(&self, typed_ast_id: &TypedAstId) -> Result<&Node, CompilerDriverError> {
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

    pub fn runtime_value(&self, value_id: &RuntimeValueId) -> Result<&Value, CompilerDriverError> {
        self.runtime_values
            .get(value_id)
            .ok_or_else(|| CompilerDriverError::MissingRuntimeValue(value_id.clone()))
    }

    pub fn typing_diagnostics(&self) -> &[TypingDiagnostic] {
        &self.typing_diagnostics
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
            runtime_values: BTreeMap::new(),
            typing_diagnostics: Vec::new(),
            module_resolver: None,
            module_resolutions: BTreeMap::new(),
            comptime_pending: HashMap::new(),
            comptime_seeded: HashSet::new(),
            hir_to_typed_ast: BTreeMap::new(),
            mir_to_typed_ast: BTreeMap::new(),
            lir_to_typed_ast: BTreeMap::new(),
        }
    }
}
