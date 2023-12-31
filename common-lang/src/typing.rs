use crate::ast::*;
use crate::context::ArcScopedContext;
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
                    matches!(ty, TypeValue::Primitive(TypePrimitive::Int(_))),
                    "Expected i64, got {:?}",
                    lit
                )
            }
            Value::Bool(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(TypePrimitive::Bool)),
                    "Expected bool, got {:?}",
                    lit
                )
            }
            Value::Decimal(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(TypePrimitive::Decimal(_))),
                    "Expected f64, got {:?}",
                    lit
                )
            }
            Value::Char(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(TypePrimitive::Char)),
                    "Expected char, got {:?}",
                    lit
                )
            }
            Value::String(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(TypePrimitive::String)),
                    "Expected string, got {:?}",
                    lit
                )
            }
            Value::List(_) => {
                ensure!(
                    matches!(ty, TypeValue::Primitive(TypePrimitive::List)),
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
            _ => {}
        }
        Ok(())
    }
    pub fn type_check_expr_against_value(
        &self,
        expr: &Expr,
        type_value: &TypeValue,
        ctx: &ArcScopedContext,
    ) -> Result<()> {
        match expr {
            Expr::Locator(n) => {
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
    pub fn evaluate_type_value_op(
        &self,
        op: &TypeBinOp,
        ctx: &ArcScopedContext,
    ) -> Result<TypeValue> {
        match op {
            TypeBinOp::Add { left, right } => {
                let lhs = self.evaluate_type_expr(&left, ctx)?;
                let rhs = self.evaluate_type_expr(&right, ctx)?;
                match (lhs, rhs) {
                    (TypeValue::ImplTraits(mut l), TypeValue::ImplTraits(r)) => {
                        l.bounds.bounds.extend(r.bounds.bounds);
                        return Ok(TypeValue::ImplTraits(l));
                    }
                    _ => {}
                }
                bail!("Could not evaluate type value op {:?}", op)
            }
            TypeBinOp::Sub { left, right } => {
                let lhs = self.evaluate_type_expr(&left, ctx)?;
                let rhs = self.evaluate_type_expr(&right, ctx)?;
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
    pub fn evaluate_type_value(&self, ty: &TypeValue, ctx: &ArcScopedContext) -> Result<TypeValue> {
        match ty {
            TypeValue::Expr(expr) => self.evaluate_type_expr(expr, ctx),
            TypeValue::Struct(n) => {
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
                Ok(TypeValue::Struct(TypeStruct {
                    name: n.name.clone(),
                    fields,
                }))
            }
            TypeValue::Structural(n) => {
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
                Ok(TypeValue::Structural(TypeStructural { fields }))
            }
            TypeValue::Function(f) => {
                let sub = ctx.child(Ident::new("__func__"), Visibility::Private, false);
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
                Ok(TypeValue::Function(
                    TypeFunction {
                        params,
                        generics_params: f.generics_params.clone(),
                        ret,
                    }
                    .into(),
                ))
            }
            TypeValue::TypeBounds(b) => self.evaluate_type_bounds(b, ctx),
            TypeValue::ImplTraits(t) => self.evaluate_impl_traits(t, ctx),
            _ => Ok(ty.clone()),
        }
    }
    pub fn evaluate_impl_traits(
        &self,
        traits: &ImplTraits,
        ctx: &ArcScopedContext,
    ) -> Result<TypeValue> {
        let traits = self.evaluate_type_bounds(&traits.bounds, ctx)?;
        match traits {
            TypeValue::TypeBounds(bounds) => Ok(TypeValue::ImplTraits(ImplTraits { bounds })),
            _ => Ok(traits),
        }
    }
    pub fn evaluate_type_expr(&self, ty: &TypeExpr, ctx: &ArcScopedContext) -> Result<TypeValue> {
        match ty {
            TypeExpr::Value(v) => self.evaluate_type_value(v, ctx),
            TypeExpr::Locator(i) => {
                match i.to_string().as_str() {
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
        ctx: &ArcScopedContext,
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

    pub fn type_check_expr(
        &self,
        expr: &Expr,
        ty: &TypeExpr,
        ctx: &ArcScopedContext,
    ) -> Result<()> {
        let tv = self.evaluate_type_expr(ty, ctx)?;
        self.type_check_expr_against_value(expr, &tv, ctx)
    }

    pub fn infer_type_call(
        &self,
        callee: &Expr,
        params: &[Expr],
        ctx: &ArcScopedContext,
    ) -> Result<TypeValue> {
        match callee {
            Expr::Locator(Locator::Ident(ident)) => match ident.as_str() {
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
            TypeValue::Function(f) => return Ok(f.ret),
            _ => {}
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_ident(&self, ident: &Ident, ctx: &ArcScopedContext) -> Result<TypeValue> {
        match ident.as_str() {
            ">" | ">=" | "<" | "<=" | "==" | "!=" => {
                return Ok(TypeValue::Function(
                    TypeFunction {
                        generics_params: vec![GenericParam {
                            name: Ident::new("T"),
                            bounds: TypeBounds::new(TypeExpr::value(TypeValue::any())),
                        }],
                        params: vec![TypeValue::ident("T".into()), TypeValue::ident("T".into())],
                        ret: TypeValue::bool(),
                    }
                    .into(),
                ));
            }
            "print" => {
                return Ok(TypeValue::Function(
                    TypeFunction {
                        params: vec![],
                        generics_params: vec![],
                        ret: TypeValue::unit(),
                    }
                    .into(),
                ))
            }
            _ => {}
        }
        let expr = ctx
            .get_expr(ident)
            .with_context(|| format!("Could not find {:?} in context", ident))?;
        self.infer_expr(&expr, ctx)
    }
    pub fn infer_expr(&self, expr: &Expr, ctx: &ArcScopedContext) -> Result<TypeValue> {
        match expr {
            Expr::Locator(n) => {
                let ty = ctx
                    .get_type(n)
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return Ok(ty);
            }
            Expr::Value(l) => match &**l {
                Value::Int(_) => return Ok(TypeValue::Primitive(TypePrimitive::Int(IntType::I64))),
                Value::Decimal(_) => {
                    return Ok(TypeValue::Primitive(TypePrimitive::Decimal(
                        DecimalType::F64,
                    )))
                }
                Value::Unit(_) => return Ok(TypeValue::unit()),
                Value::Bool(_) => return Ok(TypeValue::Primitive(TypePrimitive::Bool)),
                Value::String(_) => return Ok(TypeValue::Primitive(TypePrimitive::String)),
                Value::Type(_) => return Ok(TypeValue::Type(TypeType {})),
                Value::Char(_) => return Ok(TypeValue::Primitive(TypePrimitive::Char)),
                Value::List(_) => return Ok(TypeValue::Primitive(TypePrimitive::List)),
                _ => {}
            },
            Expr::Invoke(invoke) => match &invoke.func {
                Expr::Value(value) => match &**value {
                    Value::BinOpKind(kind) if kind.is_bool() => {
                        return Ok(TypeValue::Primitive(TypePrimitive::Bool))
                    }
                    Value::BinOpKind(_) => {
                        return Ok(self.infer_expr(invoke.args.first().context("No param")?, ctx)?)
                    }
                    _ => {}
                },

                _ => {}
            },
            _ => {}
        }

        bail!(
            "Could not infer type of {}: {:?}",
            self.serializer.serialize_expr(expr)?,
            expr
        )
    }

    pub fn infer_function(
        &self,
        func: &ValueFunction,
        _ctx: &ArcScopedContext,
    ) -> Result<TypeFunction> {
        let mut params = vec![];
        for p in &func.params {
            params.push(p.ty.clone());
        }
        let ret = func.ret.clone();
        Ok(TypeFunction {
            params,
            generics_params: func.generics_params.clone(),
            ret: ret,
        })
    }
}
