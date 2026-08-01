use crate::TypeVarId;
use fp_core::hir::*;
use fp_core::module::path::QualifiedPath;

#[derive(Clone, Debug)]
pub(crate) struct ImplContext {
    pub(crate) struct_name: QualifiedPath,
    pub(crate) self_ty: Ty,
    pub(crate) impl_generics_params: Vec<GenericParam>,
}

#[derive(Clone)]
pub(crate) enum EnvEntry {
    Mono(TypeVarId),
    Poly(Ty),
}

pub(crate) struct PatternBinding {
    pub(crate) name: String,
    pub(crate) var: TypeVarId,
}

pub(crate) struct PatternInfo {
    pub(crate) var: TypeVarId,
    pub(crate) bindings: Vec<PatternBinding>,
}

impl PatternInfo {
    pub(crate) fn new(var: TypeVarId) -> Self {
        Self {
            var,
            bindings: Vec::new(),
        }
    }

    pub(crate) fn with_binding(mut self, name: String, var: TypeVarId) -> Self {
        self.bindings.push(PatternBinding { name, var });
        self
    }
}

pub(crate) struct FunctionTypeInfo {
    pub(crate) params: Vec<TypeVarId>,
    pub(crate) ret: TypeVarId,
}

pub(crate) struct LoopContext {
    pub(crate) result_var: TypeVarId,
    pub(crate) saw_break: bool,
}

impl LoopContext {
    pub(crate) fn new(result_var: TypeVarId) -> Self {
        Self {
            result_var,
            saw_break: false,
        }
    }
}

#[derive(Clone)]
pub(crate) struct ContextBinding {
    pub(crate) ty: Ty,
    pub(crate) expr: Expr,
}
