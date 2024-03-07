use crate::ast::Visibility;
use crate::context::SharedScopedContext;
use crate::expr::Expr;
use crate::pass::InterpreterPass;
use crate::utils::conv::TryConv;
use crate::value::{
    DecimalType, FieldTypeValue, GenericParam, ImplTraits, IntType, Type, TypeBounds, TypeFunction,
    TypePrimitive, TypeStruct, TypeStructural, TypeType, Value, ValueFunction,
};
use common::{bail, ensure, ContextCompat, Error, Result};
use itertools::Itertools;

impl InterpreterPass {
    pub fn type_check_value(&self, lit: &Value, ty: &Type) -> Result<()> {
        match lit {
            Value::Int(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::Int(_))),
                    "Expected i64, got {:?}",
                    lit
                )
            }
            Value::Bool(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::Bool)),
                    "Expected bool, got {:?}",
                    lit
                )
            }
            Value::Decimal(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::Decimal(_))),
                    "Expected f64, got {:?}",
                    lit
                )
            }
            Value::Char(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::Char)),
                    "Expected char, got {:?}",
                    lit
                )
            }
            Value::String(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::String)),
                    "Expected string, got {:?}",
                    lit
                )
            }
            Value::List(_) => {
                ensure!(
                    matches!(ty, Type::Primitive(TypePrimitive::List)),
                    "Expected list, got {:?}",
                    lit
                )
            }
            Value::Unit(_) => {
                ensure!(matches!(ty, Type::Unit(_)), "Expected unit, got {:?}", lit)
            }
            Value::Type(_) => {
                ensure!(matches!(ty, Type::Type(_)), "Expected type, got {:?}", lit)
            }
            _ => {}
        }
        Ok(())
    }
    pub fn type_check_expr_against_value(
        &self,
        expr: &Expr,
        type_value: &Type,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        match expr {
            Expr::Locator(n) => {
                let expr = ctx
                    .get_expr(n.to_path())
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return self.type_check_expr_against_value(&expr, type_value, ctx);
            }

            Expr::Value(v) => return self.type_check_value(v, type_value),
            _ => {}
        }
        Ok(())
    }

    pub fn evaluate_type_value(&self, ty: &Type, ctx: &SharedScopedContext) -> Result<Type> {
        match ty {
            Type::Expr(expr) => {
                let value = self.interpret_expr(expr, ctx)?;
                let ty = value.try_conv()?;
                return Ok(ty);
            }
            Type::Struct(n) => {
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
                return Ok(Type::Struct(TypeStruct {
                    name: n.name.clone(),
                    fields,
                }));
            }
            Type::Structural(n) => {
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
                return Ok(Type::Structural(TypeStructural { fields }));
            }
            Type::Function(f) => {
                let sub = ctx.child(
                    crate::id::Ident::new("__func__"),
                    Visibility::Private,
                    false,
                );
                for g in &f.generics_params {
                    let constrain = self.evaluate_type_bounds(&g.bounds, &sub)?;
                    sub.insert_value_with_ctx(g.name.clone(), constrain.into());
                }
                let params = f
                    .params
                    .iter()
                    .map(|x| self.evaluate_type_value(x, &sub))
                    .try_collect()?;
                let ret = self.evaluate_type_value(&f.ret, &sub)?;
                return Ok(Type::Function(
                    TypeFunction {
                        params,
                        generics_params: f.generics_params.clone(),
                        ret,
                    }
                    .into(),
                ));
            }
            Type::TypeBounds(b) => return self.evaluate_type_bounds(b, ctx),
            Type::ImplTraits(t) => return self.evaluate_impl_traits(t, ctx),
            _ => Ok(ty.clone()),
        }
    }
    pub fn evaluate_impl_traits(
        &self,
        traits: &ImplTraits,
        ctx: &SharedScopedContext,
    ) -> Result<Type> {
        let traits = self.evaluate_type_bounds(&traits.bounds, ctx)?;
        match traits {
            Type::TypeBounds(bounds) => Ok(Type::ImplTraits(ImplTraits { bounds })),
            _ => Ok(traits),
        }
    }

    pub fn evaluate_type_bounds(
        &self,
        bounds: &TypeBounds,
        ctx: &SharedScopedContext,
    ) -> Result<Type> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        if bounds.is_empty() {
            return Ok(Type::any());
        }
        if bounds.len() == 1 {
            return bounds.first().unwrap().clone().try_conv();
        }

        bail!("failed to evaluate type bounds: {:?}", bounds)
        // Ok(TypeValue::TypeBounds(TypeBounds { bounds }))
    }

    pub fn type_check_expr(&self, expr: &Expr, ty: &Expr, ctx: &SharedScopedContext) -> Result<()> {
        let tv = self.interpret_expr(ty, ctx)?.try_conv()?;

        self.type_check_expr_against_value(expr, &tv, ctx)
    }

    pub fn infer_type_call(
        &self,
        callee: &Expr,
        params: &[Expr],
        ctx: &SharedScopedContext,
    ) -> Result<Type> {
        match callee {
            Expr::Locator(crate::id::Locator::Ident(ident)) => match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_expr(params.first().context("No param")?, ctx)
                }
                "print" => return Ok(Type::unit()),
                _ => {}
            },
            _ => {}
        }

        let callee = self.infer_expr(callee, ctx)?;
        match callee {
            Type::Function(f) => return Ok(f.ret),
            _ => {}
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_ident(&self, ident: &crate::id::Ident, ctx: &SharedScopedContext) -> Result<Type> {
        match ident.as_str() {
            ">" | ">=" | "<" | "<=" | "==" | "!=" => {
                return Ok(Type::Function(
                    TypeFunction {
                        generics_params: vec![GenericParam {
                            name: crate::id::Ident::new("T"),
                            bounds: TypeBounds::new(Expr::value(Type::any().into())),
                        }],
                        params: vec![Type::ident("T".into()), Type::ident("T".into())],
                        ret: Type::bool(),
                    }
                    .into(),
                ));
            }
            "print" => {
                return Ok(Type::Function(
                    TypeFunction {
                        params: vec![],
                        generics_params: vec![],
                        ret: Type::unit(),
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
    pub fn infer_expr(&self, expr: &Expr, ctx: &SharedScopedContext) -> Result<Type> {
        match expr {
            Expr::Locator(n) => {
                let ty = ctx
                    .get_type(n.to_path())
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return Ok(ty);
            }
            Expr::Value(l) => match &**l {
                Value::Int(_) => return Ok(Type::Primitive(TypePrimitive::Int(IntType::I64))),
                Value::Decimal(_) => {
                    return Ok(Type::Primitive(TypePrimitive::Decimal(DecimalType::F64)))
                }
                Value::Unit(_) => return Ok(Type::unit()),
                Value::Bool(_) => return Ok(Type::Primitive(TypePrimitive::Bool)),
                Value::String(_) => return Ok(Type::Primitive(TypePrimitive::String)),
                Value::Type(_) => return Ok(Type::Type(TypeType {})),
                Value::Char(_) => return Ok(Type::Primitive(TypePrimitive::Char)),
                Value::List(_) => return Ok(Type::Primitive(TypePrimitive::List)),
                _ => {}
            },
            Expr::Invoke(invoke) => match invoke.func.get() {
                Expr::Value(value) => match *value {
                    Value::BinOpKind(kind) if kind.is_bool() => {
                        return Ok(Type::Primitive(TypePrimitive::Bool))
                    }
                    Value::BinOpKind(_) => {
                        return Ok(
                            self.infer_expr(&invoke.args.first().context("No param")?.get(), ctx)?
                        )
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
        _ctx: &SharedScopedContext,
    ) -> Result<TypeFunction> {
        let mut params = vec![];
        for p in &func.params {
            params.push(p.ty.clone());
        }
        let ret = func.ret.clone();
        Ok(TypeFunction {
            params,
            generics_params: func.generics_params.clone(),
            ret,
        })
    }
}
