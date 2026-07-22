use crate::typing::unify::TypeVarKind;
use crate::{typing_error, AstTypeInferencer, TypeVarId};
use fp_core::ast::{Ty, TypeFunction, TypeInt, TypePrimitive};
use fp_core::error::Result;

fn primitive_ty(ty: &Ty) -> Option<TypePrimitive> {
    match ty {
        Ty::Primitive(primitive) => Some(*primitive),
        _ => None,
    }
}

fn is_any_ty(ty: &Ty) -> bool {
    matches!(ty, Ty::Any(_))
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn ensure_numeric(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                self.emit_error(format!("expected numeric value for {}", context));
                Err(typing_error("expected numeric type, found error"))
            }
            TypeVarKind::Unbound { .. } | TypeVarKind::Bound(_) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_numeric(next, context),
        }
    }

    pub(crate) fn ensure_bool(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        if self.lossy_mode {
            return Ok(());
        }
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind = TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Bool));
                Ok(())
            }
            TypeVarKind::Bound(Ty::ErrorType(_)) => Ok(()),
            TypeVarKind::Bound(ty) if primitive_ty(&ty) == Some(TypePrimitive::Bool) => Ok(()),
            TypeVarKind::Bound(ty) if is_any_ty(&ty) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_bool(next, context),
            other => {
                tracing::debug!("ensure_bool failure: context={} type={:?}", context, other);
                self.emit_error(format!("expected boolean for {}", context));
                Err(typing_error(format!("expected bool, found {:?}", other)))
            }
        }
    }

    pub(crate) fn ensure_integer(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(ty) if is_any_ty(&ty) => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(ty) if matches!(primitive_ty(&ty), Some(TypePrimitive::Int(_))) => {
                Ok(())
            }
            TypeVarKind::Link(next) => self.ensure_integer(next, context),
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            other => {
                self.emit_error(format!("expected integer value for {}", context));
                Err(typing_error(format!("expected integer, found {:?}", other)))
            }
        }
    }

    pub(crate) fn ensure_function(
        &mut self,
        var: TypeVarId,
        arity: usize,
    ) -> Result<super::super::FunctionTypeInfo> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                let params: Vec<_> = (0..arity).map(|_| self.fresh_type_var()).collect();
                let ret = self.fresh_type_var();
                self.bind_function_term(root, params.clone(), ret);
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(ty) if is_any_ty(&ty) => {
                let params: Vec<_> = (0..arity).map(|_| self.fresh_type_var()).collect();
                let ret = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                    params: params.iter().copied().map(Ty::infer_var).collect(),
                    generics_params: Vec::new(),
                    ret_ty: Some(Box::new(Ty::infer_var(ret))),
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(Ty::Function(func)) => {
                if func.params.len() != arity {
                    self.emit_error(format!(
                        "function arity mismatch: expected {}, found {}",
                        arity,
                        func.params.len()
                    ));
                    let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                    let ret = self.error_type_var();
                    self.type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    }));
                    return Ok(super::super::FunctionTypeInfo { params, ret });
                }
                let mut params = Vec::with_capacity(func.params.len());
                for param in &func.params {
                    let Ty::InferVar(infer) = param else {
                        let inferred = self.type_from_ast_ty(param)?;
                        params.push(inferred);
                        continue;
                    };
                    params.push(infer.id);
                }
                let ret = match func.ret_ty.as_deref() {
                    Some(Ty::InferVar(infer)) => infer.id,
                    Some(other) => self.type_from_ast_ty(other)?,
                    None => self.unit_type_var(),
                };
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Link(next) => self.ensure_function(next, arity),
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                let ret = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                    params: params.iter().copied().map(Ty::infer_var).collect(),
                    generics_params: Vec::new(),
                    ret_ty: Some(Box::new(Ty::infer_var(ret))),
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            other => {
                self.emit_error(format!("expected function, found {:?}", other));
                let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                let ret = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                    params: params.iter().copied().map(Ty::infer_var).collect(),
                    generics_params: Vec::new(),
                    ret_ty: Some(Box::new(Ty::infer_var(ret))),
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
        }
    }
}
