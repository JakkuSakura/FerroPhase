use crate::context::ExecutionContext;
use crate::tree::*;
use crate::value::*;
use crate::Serializer;
use common::*;
use std::rc::Rc;

pub struct TypeSystem {
    serializer: Rc<dyn Serializer>,
}

impl TypeSystem {
    pub fn type_check_value(&self, lit: &Value, ty: &TypeValue) -> Result<()> {
        match lit {
            Value::Int(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::I64)),
                    "Expected i64, got {:?}",
                    lit
                )
            }
            Value::Bool(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Bool)),
                    "Expected bool, got {:?}",
                    lit
                )
            }
            Value::Decimal(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::F64)),
                    "Expected f64, got {:?}",
                    lit
                )
            }
            Value::Char(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Char)),
                    "Expected char, got {:?}",
                    lit
                )
            }
            Value::String(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::String)),
                    "Expected string, got {:?}",
                    lit
                )
            }
            Value::List(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::List)),
                    "Expected list, got {:?}",
                    lit
                )
            }
            Value::Unit(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Unit)),
                    "Expected unit, got {:?}",
                    lit
                )
            }
            Value::Type(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Type)),
                    "Expected type, got {:?}",
                    lit
                )
            }
            Value::Struct(_) => {}
            Value::Function(_) => {}
            Value::Tuple(_) => {}
            Value::Expr(_) => {}
        }
        Ok(())
    }
    pub fn type_check_expr_against_value(
        &self,
        expr: &Expr,
        type_value: &TypeValue,
        ctx: &ExecutionContext,
    ) -> Result<()> {
        match expr {
            Expr::Ident(n) => {
                let expr = ctx
                    .get_expr(n)
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return self.type_check_expr_against_value(&expr, type_value, ctx);
            }
            Expr::Path(_) => {}
            Expr::Value(v) => return self.type_check_value(v, type_value),
            _ => {}
        }
        Ok(())
    }
    pub fn evaluate_type_value_op(&self, op: &TypeOp, ctx: &ExecutionContext) -> Result<TypeValue> {
        match op {
            TypeOp::Add(a) => {
                let lhs = self.evaluate_type_value(&a.lhs, ctx)?;
                let rhs = self.evaluate_type_value(&a.rhs, ctx)?;
                match (lhs, rhs) {
                    (TypeValue::RequireTraits(mut l), TypeValue::RequireTraits(r)) => {
                        l.traits.extend(r.traits);
                        return Ok(TypeValue::RequireTraits(l));
                    }
                    _ => {}
                }
                bail!("Could not evaluate type value op {:?}", op)
            }
            TypeOp::Sub(a) => {
                let lhs = self.evaluate_type_value(&a.lhs, ctx)?;
                let rhs = self.evaluate_type_value(&a.rhs, ctx)?;
                match (lhs, rhs) {
                    (TypeValue::RequireTraits(mut l), TypeValue::RequireTraits(r)) => {
                        for r in r.traits {
                            l.traits.retain(|x| x.name != r.name);
                        }
                        return Ok(TypeValue::RequireTraits(l));
                    }
                    _ => {}
                }
                bail!("Could not evaluate type value op {:?}", op)
            }
        }
    }
    pub fn evaluate_type_value(&self, ty: &TypeExpr, ctx: &ExecutionContext) -> Result<TypeValue> {
        match ty {
            TypeExpr::Primitive(p) => Ok(TypeValue::Primitive(p.clone())),
            TypeExpr::NamedStruct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.ty, ctx)?;
                        Ok::<_, Error>(FieldTypeValue {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                Ok(TypeValue::NamedStruct(NamedStructType {
                    name: n.name.clone(),
                    fields,
                }))
            }
            TypeExpr::UnnamedStruct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.ty, ctx)?;
                        Ok::<_, Error>(FieldTypeValue {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                Ok(TypeValue::UnnamedStruct(UnnamedStructType { fields }))
            }
            TypeExpr::FuncType(f) => {
                let params = f
                    .params
                    .iter()
                    .map(|x| self.evaluate_type_value(x, ctx))
                    .try_collect()?;
                let ret = self.evaluate_type_value(&f.ret, ctx)?;
                Ok(TypeValue::FuncType(FuncTypeValue {
                    params,
                    ret: Box::new(ret),
                }))
            }
            TypeExpr::Ident(i) => {
                let ty = ctx
                    .get_type(i)
                    .with_context(|| format!("Could not find type {:?}", i))?;
                Ok(ty)
            }
            TypeExpr::Op(o) => self.evaluate_type_value_op(o, ctx),
            _ => bail!("Could not evaluate type value {:?}", ty),
        }
    }
    pub fn type_check_expr(
        &self,
        expr: &Expr,
        ty: &TypeExpr,
        ctx: &ExecutionContext,
    ) -> Result<()> {
        let tv = self.evaluate_type_value(ty, ctx)?;
        self.type_check_expr_against_value(expr, &tv, ctx)
    }

    pub fn infer_type_call(
        &self,
        callee: &Expr,
        params: &[Expr],
        ctx: &ExecutionContext,
    ) -> Result<TypeValue> {
        match callee {
            Expr::Ident(ident) => match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_type_expr(params.first().context("No param")?, ctx)
                }
                "print" => return Ok(TypeValue::unit()),
                _ => {}
            },
            _ => {}
        }

        let callee = self.infer_type_expr(callee, ctx)?;
        match callee {
            TypeValue::FuncType(f) => return Ok(*f.ret),
            _ => {}
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_type_ident(&self, ident: &Ident, ctx: &ExecutionContext) -> Result<TypeValue> {
        match ident.as_str() {
            ">" | ">=" | "<" | "<=" | "==" | "!=" => {
                return Ok(TypeValue::FuncType(FuncTypeValue {
                    params: vec![],
                    ret: Box::new(TypeValue::bool()),
                }));
            }
            "print" => {
                return Ok(TypeValue::FuncType(FuncTypeValue {
                    params: vec![],
                    ret: Box::new(TypeValue::unit()),
                }))
            }
            _ => {}
        }
        let expr = ctx
            .get_expr(ident)
            .with_context(|| format!("Could not find {:?} in context", ident))?;
        self.infer_type_expr(&expr, ctx)
    }
    pub fn infer_type_expr(&self, expr: &Expr, ctx: &ExecutionContext) -> Result<TypeValue> {
        match expr {
            Expr::Ident(n) => {
                let ty = ctx
                    .get_type(n)
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return Ok(ty);
            }
            Expr::Path(_) => {}
            Expr::Value(l) => match l {
                Value::Int(_) => return Ok(TypeValue::Primitive(PrimitiveType::I64)),
                Value::Decimal(_) => return Ok(TypeValue::Primitive(PrimitiveType::F64)),
                Value::Unit(_) => return Ok(TypeValue::Primitive(PrimitiveType::Unit)),
                Value::Bool(_) => return Ok(TypeValue::Primitive(PrimitiveType::Bool)),
                Value::String(_) => return Ok(TypeValue::Primitive(PrimitiveType::String)),
                Value::Type(_) => return Ok(TypeValue::Primitive(PrimitiveType::Type)),
                Value::Char(_) => return Ok(TypeValue::Primitive(PrimitiveType::Char)),
                Value::List(_) => return Ok(TypeValue::Primitive(PrimitiveType::List)),
                _ => {}
            },
            _ => {}
        }

        bail!(
            "Could not infer type of {}",
            self.serializer.serialize_expr(expr)?
        )
    }
}
