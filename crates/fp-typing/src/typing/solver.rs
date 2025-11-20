use crate::typing::unify::{FunctionTerm, TypeTerm, TypeVarKind};
use crate::{typing_error, AstTypeInferencer, TypeVarId};
use fp_core::ast::{TypeInt, TypePrimitive};
use fp_core::error::Result;

impl<'ctx> AstTypeInferencer<'ctx> {
    pub(crate) fn ensure_numeric(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. }
            | TypeVarKind::Bound(TypeTerm::Any)
            | TypeVarKind::Bound(TypeTerm::Unknown)
            | TypeVarKind::Bound(TypeTerm::Custom(_)) => Ok(()),
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(_)))
            | TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Decimal(_))) => Ok(()),
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
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Bool)) => Ok(()),
            TypeVarKind::Bound(TypeTerm::Any) | TypeVarKind::Bound(TypeTerm::Unknown) => Ok(()),
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
            TypeVarKind::Bound(TypeTerm::Primitive(TypePrimitive::Int(_))) => Ok(()),
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
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                    params: params.clone(),
                    ret,
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(TypeTerm::Any) => {
                let params: Vec<_> = (0..arity).map(|_| self.fresh_type_var()).collect();
                let ret = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                    params: params.clone(),
                    ret,
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
            TypeVarKind::Bound(TypeTerm::Function(func)) => {
                if func.params.len() != arity {
                    self.emit_error(format!(
                        "function arity mismatch: expected {}, found {}",
                        arity,
                        func.params.len()
                    ));
                    let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                    let ret = self.error_type_var();
                    self.type_vars[root].kind =
                        TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                            params: params.clone(),
                            ret,
                        }));
                    return Ok(super::super::FunctionTypeInfo { params, ret });
                }
                Ok(super::super::FunctionTypeInfo {
                    params: func.params,
                    ret: func.ret,
                })
            }
            TypeVarKind::Link(next) => self.ensure_function(next, arity),
            other => {
                self.emit_error(format!("expected function, found {:?}", other));
                let params: Vec<_> = (0..arity).map(|_| self.error_type_var()).collect();
                let ret = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Function(FunctionTerm {
                    params: params.clone(),
                    ret,
                }));
                Ok(super::super::FunctionTypeInfo { params, ret })
            }
        }
    }
}
