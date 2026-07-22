use crate::typing::unify::{TypeTerm, TypeVarKind};
use crate::{typing_error, AstTypeInferencer, TypeVarId};
use fp_core::ast::{Ty, TypeFunction, TypeInt, TypePrimitive};
use fp_core::error::Result;

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn ensure_numeric(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } | TypeVarKind::Bound(TypeTerm::Concrete(_)) => Ok(()),
            TypeVarKind::Bound(term)
                if matches!(
                    term.primitive_ty(),
                    Some(TypePrimitive::Int(_)) | Some(TypePrimitive::Decimal(_))
                ) =>
            {
                Ok(())
            }
            TypeVarKind::Link(next) => self.ensure_numeric(next, context),
            other => {
                self.emit_error(format!("expected numeric value for {}", context));
                Err(typing_error(format!(
                    "expected numeric type, found {:?}",
                    other
                )))
            }
        }
    }

    pub(crate) fn ensure_bool(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        if self.lossy_mode {
            return Ok(());
        }
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Bool));
                Ok(())
            }
            TypeVarKind::Bound(term) if term.primitive_ty() == Some(TypePrimitive::Bool) => Ok(()),
            TypeVarKind::Bound(term) if term.is_any() || term.is_error() => Ok(()),
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
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(term) if term.is_any() || term.is_error() => {
                self.type_vars[root].kind =
                    TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(term) if matches!(term.primitive_ty(), Some(TypePrimitive::Int(_))) => Ok(()),
            TypeVarKind::Link(next) => self.ensure_integer(next, context),
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
            TypeVarKind::Bound(term) if term.is_any() || term.is_error() => {
                let params: Vec<_> = (0..arity).map(|_| self.fresh_type_var()).collect();
                let ret = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Concrete(Ty::Function(
                    TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    },
                )));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(TypeTerm::Concrete(Ty::Function(func))) => {
                if func.params.len() != arity {
                    self.emit_error(format!(
                        "function arity mismatch: expected {}, found {}",
                        arity,
                        func.params.len()
                    ));
                    let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                    let ret = self.error_type_var();
                    self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Concrete(
                        Ty::Function(TypeFunction {
                            params: params.iter().copied().map(Ty::infer_var).collect(),
                            generics_params: Vec::new(),
                            ret_ty: Some(Box::new(Ty::infer_var(ret))),
                        }),
                    ));
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
            other => {
                self.emit_error(format!("expected function, found {:?}", other));
                let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                let ret = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Concrete(Ty::Function(
                    TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    },
                )));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
        }
    }
}
