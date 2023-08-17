use crate::context::ScopedContext;
use crate::tree::*;
use crate::value::*;
use crate::Serializer;
use common::*;
use std::rc::Rc;

pub struct TypeSystem {
    serializer: Rc<dyn Serializer>,
}

impl TypeSystem {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self { serializer }
    }
    pub fn type_check_value(&self, lit: &Value, ty: &TypeValue) -> Result<()> {
        match lit {
            Value::Int(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Int(_))),
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
                    matches!(ty, TypeValue::Primitive(PrimitiveType::Decimal(_))),
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
                    matches!(ty, TypeValue::Unit(_)),
                    "Expected unit, got {:?}",
                    lit
                )
            }
            Value::Type(_) => {
                ensure!(
                    matches!(ty, TypeValue::Type(_)),
                    "Expected type, got {:?}",
                    lit
                )
            }
            Value::Struct(_) => {}
            Value::Function(_) => {}
            Value::Tuple(_) => {}
            Value::Expr(_) => {}
            Value::Any(_) => {}
            Value::Null(_) => {}
            Value::Undefined(_) => {}
            Value::BinOpKind(_) => {}
        }
        Ok(())
    }
    pub fn type_check_expr_against_value(
        &self,
        expr: &Expr,
        type_value: &TypeValue,
        ctx: &ScopedContext,
    ) -> Result<()> {
        match expr {
            Expr::Pat(n) => {
                let expr = ctx
                    .get_expr(n)
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return self.type_check_expr_against_value(&expr, type_value, ctx);
            }

            Expr::Value(v) => return self.type_check_value(v, type_value),
            _ => {}
        }
        Ok(())
    }
    pub fn evaluate_type_value_op(&self, op: &TypeBinOp, ctx: &ScopedContext) -> Result<TypeValue> {
        match op {
            TypeBinOp::Add(a) => {
                let lhs = self.evaluate_type_expr(&a.lhs, ctx)?;
                let rhs = self.evaluate_type_expr(&a.rhs, ctx)?;
                match (lhs, rhs) {
                    (TypeValue::ImplTraits(mut l), TypeValue::ImplTraits(r)) => {
                        l.bounds.bounds.extend(r.bounds.bounds);
                        return Ok(TypeValue::ImplTraits(l));
                    }
                    _ => {}
                }
                bail!("Could not evaluate type value op {:?}", op)
            }
            TypeBinOp::Sub(a) => {
                let lhs = self.evaluate_type_expr(&a.lhs, ctx)?;
                let rhs = self.evaluate_type_expr(&a.rhs, ctx)?;
                match (lhs, rhs) {
                    // (TypeValue::ImplTraits(mut l), TypeValue::ImplTraits(r)) => {
                    //     for r in r.bounds.bounds {
                    //         l.bounds.bounds.retain(|x| x.name != r.name);
                    //     }
                    //     return Ok(TypeValue::ImplTraits(l));
                    // }
                    _ => {}
                }
                bail!("Could not evaluate type value op {:?}", op)
            }
        }
    }
    pub fn evaluate_type_value(&self, ty: &TypeValue, ctx: &ScopedContext) -> Result<TypeValue> {
        match ty {
            TypeValue::Expr(expr) => self.evaluate_type_expr(expr, ctx),
            TypeValue::NamedStruct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
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
            TypeValue::UnnamedStruct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
                        Ok::<_, Error>(FieldTypeValue {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                Ok(TypeValue::UnnamedStruct(UnnamedStructType { fields }))
            }
            TypeValue::Function(f) => {
                let sub = ctx.child();
                for g in &f.generics_params {
                    let constrain = self.evaluate_type_bounds(&g.bounds, &sub)?;
                    sub.insert_type(g.name.clone(), constrain);
                }
                let params = f
                    .params
                    .iter()
                    .map(|x| self.evaluate_type_value(x, &sub))
                    .try_collect()?;
                let ret = self.evaluate_type_value(&f.ret, &sub)?;
                Ok(TypeValue::Function(FunctionType {
                    params,
                    generics_params: f.generics_params.clone(),
                    ret: Box::new(ret),
                }))
            }
            TypeValue::TypeBounds(b) => self.evaluate_type_bounds(b, ctx),
            TypeValue::ImplTraits(t) => self.evaluate_impl_traits(t, ctx),
            _ => Ok(ty.clone()),
        }
    }
    pub fn evaluate_impl_traits(
        &self,
        traits: &ImplTraits,
        ctx: &ScopedContext,
    ) -> Result<TypeValue> {
        let traits = self.evaluate_type_bounds(&traits.bounds, ctx)?;
        match traits {
            TypeValue::TypeBounds(bounds) => Ok(TypeValue::ImplTraits(ImplTraits { bounds })),
            _ => Ok(traits),
        }
    }
    pub fn evaluate_type_expr(&self, ty: &TypeExpr, ctx: &ScopedContext) -> Result<TypeValue> {
        match ty {
            TypeExpr::Value(v) => self.evaluate_type_value(v, ctx),
            TypeExpr::Ident(i) => {
                match i.as_str() {
                    "Add" => return Ok(TypeValue::impl_trait("Add".into())),
                    _ => {}
                }
                let ty = ctx
                    .get_type(i)
                    .with_context(|| format!("Could not find type {:?}", i))?;
                Ok(ty)
            }
            TypeExpr::BinOp(o) => self.evaluate_type_value_op(o, ctx),
            _ => bail!("Could not evaluate type value {:?}", ty),
        }
    }
    pub fn evaluate_type_bounds(
        &self,
        bounds: &TypeBounds,
        ctx: &ScopedContext,
    ) -> Result<TypeValue> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.evaluate_type_expr(x, ctx))
            .try_collect()?;
        if bounds.is_empty() {
            return Ok(TypeValue::any());
        }
        if bounds.len() == 1 {
            return Ok(bounds.first().unwrap().clone());
        }

        bail!("failed to evaluate type bounds: {:?}", bounds)
        // Ok(TypeValue::TypeBounds(TypeBounds { bounds }))
    }

    pub fn type_check_expr(&self, expr: &Expr, ty: &TypeExpr, ctx: &ScopedContext) -> Result<()> {
        let tv = self.evaluate_type_expr(ty, ctx)?;
        self.type_check_expr_against_value(expr, &tv, ctx)
    }

    pub fn infer_type_call(
        &self,
        callee: &Expr,
        params: &[Expr],
        ctx: &ScopedContext,
    ) -> Result<TypeValue> {
        match callee {
            Expr::Pat(Pat::Ident(ident)) => match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_expr(params.first().context("No param")?, ctx)
                }
                "print" => return Ok(TypeValue::unit()),
                _ => {}
            },
            _ => {}
        }

        let callee = self.infer_expr(callee, ctx)?;
        match callee {
            TypeValue::Function(f) => return Ok(*f.ret),
            _ => {}
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_ident(&self, ident: &Ident, ctx: &ScopedContext) -> Result<TypeValue> {
        match ident.as_str() {
            ">" | ">=" | "<" | "<=" | "==" | "!=" => {
                return Ok(TypeValue::Function(FunctionType {
                    generics_params: vec![GenericParam {
                        name: Ident::new("T"),
                        bounds: TypeBounds::new(TypeExpr::value(TypeValue::any())),
                    }],
                    params: vec![
                        TypeValue::expr(TypeExpr::Ident("T".into())),
                        TypeValue::expr(TypeExpr::Ident("T".into())),
                    ],
                    ret: Box::new(TypeValue::bool()),
                }));
            }
            "print" => {
                return Ok(TypeValue::Function(FunctionType {
                    params: vec![],
                    generics_params: vec![],
                    ret: Box::new(TypeValue::unit()),
                }))
            }
            _ => {}
        }
        let expr = ctx
            .get_expr(ident)
            .with_context(|| format!("Could not find {:?} in context", ident))?;
        self.infer_expr(&expr, ctx)
    }
    pub fn infer_expr(&self, expr: &Expr, ctx: &ScopedContext) -> Result<TypeValue> {
        match expr {
            Expr::Pat(n) => {
                let ty = ctx
                    .get_type(n)
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return Ok(ty);
            }
            Expr::Value(l) => match l {
                Value::Int(_) => return Ok(TypeValue::Primitive(PrimitiveType::Int(IntType::I64))),
                Value::Decimal(_) => {
                    return Ok(TypeValue::Primitive(PrimitiveType::Decimal(
                        DecimalType::F64,
                    )))
                }
                Value::Unit(_) => return Ok(TypeValue::unit()),
                Value::Bool(_) => return Ok(TypeValue::Primitive(PrimitiveType::Bool)),
                Value::String(_) => return Ok(TypeValue::Primitive(PrimitiveType::String)),
                Value::Type(_) => return Ok(TypeValue::Type(TypeType)),
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

    pub fn infer_function(
        &self,
        func: &FunctionValue,
        _ctx: &ScopedContext,
    ) -> Result<FunctionType> {
        let mut params = vec![];
        for p in &func.params {
            params.push(p.ty.clone());
        }
        let ret = func.ret.clone();
        Ok(FunctionType {
            params,
            generics_params: func.generics_params.clone(),
            ret: Box::new(ret),
        })
    }
}
