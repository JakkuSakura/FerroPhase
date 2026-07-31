use crate::typing::unify::TypeVarKind;
use crate::{typing_error, AstTypeInferencer, BoxFuture, TypeVarId};
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

impl AstTypeInferencer {
    pub(crate) fn ensure_numeric(&self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        let kind = self.inner.borrow().type_vars[root].kind.clone();
        match kind {
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                self.emit_error(format!("expected numeric value for {}", context));
                Err(typing_error("expected numeric type, found error"))
            }
            TypeVarKind::Unbound { .. } => Ok(()),
            TypeVarKind::Bound(ref ty) => match primitive_ty(ty) {
                Some(TypePrimitive::Int(_)) | Some(TypePrimitive::Decimal(_)) => Ok(()),
                _ => {
                    self.emit_error(format!(
                        "expected numeric value for {context}, found {ty}"
                    ));
                    Err(typing_error(format!(
                        "expected numeric type, found {ty}"
                    )))
                }
            },
            TypeVarKind::Link(next) => self.ensure_numeric(next, context),
        }
    }

    pub(crate) fn ensure_bool(&self, var: TypeVarId, context: &str) -> Result<()> {
        if self.inner.borrow().lossy_mode {
            return Ok(());
        }
        let root = self.find(var);
        let kind = self.inner.borrow().type_vars[root].kind.clone();
        match kind {
            TypeVarKind::Unbound { .. } => {
                self.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Bool));
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

    pub(crate) fn ensure_integer(&self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        let kind = self.inner.borrow().type_vars[root].kind.clone();
        match kind {
            TypeVarKind::Unbound { .. } => {
                self.inner.borrow_mut().type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(ty) if is_any_ty(&ty) => {
                self.inner.borrow_mut().type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            TypeVarKind::Bound(ty) if matches!(primitive_ty(&ty), Some(TypePrimitive::Int(_))) => {
                Ok(())
            }
            TypeVarKind::Link(next) => self.ensure_integer(next, context),
            TypeVarKind::Bound(Ty::ErrorType(_)) => {
                self.inner.borrow_mut().type_vars[root].kind =
                    TypeVarKind::Bound(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                Ok(())
            }
            other => {
                self.emit_error(format!("expected integer value for {}", context));
                Err(typing_error(format!("expected integer, found {:?}", other)))
            }
        }
    }

    /// Async because the `Bound(Ty::Function(func))` arm resolves each
    /// non-`InferVar` param/return annotation via `type_from_ast_ty`, which
    /// is part of the mutually-recursive typing SCC (it can itself suspend on
    /// an unloaded package via `lookup_struct`).
    pub(crate) fn ensure_function(
        &self,
        var: TypeVarId,
        arity: usize,
    ) -> BoxFuture<'static, Result<super::super::FunctionTypeInfo>> {
        let this = self.clone();
        Box::pin(async move {
            let root = this.find(var);
            let kind = this.inner.borrow().type_vars[root].kind.clone();
            match kind {
                TypeVarKind::Unbound { .. } => {
                    let params: Vec<_> = (0..arity).map(|_| this.fresh_type_var()).collect();
                    let ret = this.fresh_type_var();
                    this.bind_function_term(root, params.clone(), ret);
                    Ok(super::super::FunctionTypeInfo { params, ret })
                }
                TypeVarKind::Bound(ty) if is_any_ty(&ty) => {
                    let params: Vec<_> = (0..arity).map(|_| this.fresh_type_var()).collect();
                    let ret = this.fresh_type_var();
                    this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    }));
                    Ok(super::super::FunctionTypeInfo { params, ret })
                }
                TypeVarKind::Bound(Ty::Function(func)) => {
                    if func.params.len() != arity {
                        this.emit_error(format!(
                            "function arity mismatch: expected {}, found {}",
                            arity,
                            func.params.len()
                        ));
                        let params: Vec<_> = (0..arity).map(|_| this.error_type_var()).collect();
                        let ret = this.error_type_var();
                        this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                            params: params.iter().copied().map(Ty::infer_var).collect(),
                            generics_params: Vec::new(),
                            ret_ty: Some(Box::new(Ty::infer_var(ret))),
                        }));
                        return Ok(super::super::FunctionTypeInfo { params, ret });
                    }
                    let mut params = Vec::with_capacity(func.params.len());
                    for param in &func.params {
                        let Ty::InferVar(infer) = param else {
                            let inferred = this.type_from_ast_ty(param).await?;
                            params.push(inferred);
                            continue;
                        };
                        params.push(infer.id);
                    }
                    let ret = match func.ret_ty.as_deref() {
                        Some(Ty::InferVar(infer)) => infer.id,
                        Some(other) => this.type_from_ast_ty(other).await?,
                        None => this.unit_type_var(),
                    };
                    Ok(super::super::FunctionTypeInfo { params, ret })
                }
                TypeVarKind::Link(next) => this.ensure_function(next, arity).await,
                TypeVarKind::Bound(Ty::ErrorType(_)) => {
                    let params: Vec<_> = (0..arity).map(|_| this.error_type_var()).collect();
                    let ret = this.error_type_var();
                    this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    }));
                    Ok(super::super::FunctionTypeInfo { params, ret })
                }
                other => {
                    this.emit_error(format!("expected function, found {:?}", other));
                    let params: Vec<_> = (0..arity).map(|_| this.error_type_var()).collect();
                    let ret = this.error_type_var();
                    this.inner.borrow_mut().type_vars[root].kind = TypeVarKind::Bound(Ty::Function(TypeFunction {
                        params: params.iter().copied().map(Ty::infer_var).collect(),
                        generics_params: Vec::new(),
                        ret_ty: Some(Box::new(Ty::infer_var(ret))),
                    }));
                    Ok(super::super::FunctionTypeInfo { params, ret })
                }
            }
        })
    }
}
