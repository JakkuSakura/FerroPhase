use common::{bail, ensure, ContextCompat, Error, Result};
use itertools::Itertools;

use lang_core::ast::{AstExpr, Visibility};
use lang_core::ast::{
    AstType, DecimalType, ExprInvokeTarget, ImplTraits, StructuralField, TypeBounds, TypeFunction,
    TypeInt, TypePrimitive, TypeStruct, TypeStructural, TypeType, Value, ValueFunction,
};
use lang_core::context::SharedScopedContext;
use lang_core::ctx::{Context, TypeSystem};
use lang_core::id::{Ident, Locator};
use lang_core::utils::conv::TryConv;

use crate::pass::{FoldOptimizer, InterpreterPass};

impl InterpreterPass {
    pub fn type_check_value(&self, lit: &Value, ty: &AstType) -> Result<()> {
        match lit {
            Value::Int(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::Int(_))),
                    "Expected i64, got {:?}",
                    lit
                )
            }
            Value::Bool(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::Bool)),
                    "Expected bool, got {:?}",
                    lit
                )
            }
            Value::Decimal(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::Decimal(_))),
                    "Expected f64, got {:?}",
                    lit
                )
            }
            Value::Char(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::Char)),
                    "Expected char, got {:?}",
                    lit
                )
            }
            Value::String(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::String)),
                    "Expected string, got {:?}",
                    lit
                )
            }
            Value::List(_) => {
                ensure!(
                    matches!(ty, AstType::Primitive(TypePrimitive::List)),
                    "Expected list, got {:?}",
                    lit
                )
            }
            Value::Unit(_) => {
                ensure!(
                    matches!(ty, AstType::Unit(_)),
                    "Expected unit, got {:?}",
                    lit
                )
            }
            Value::Type(_) => {
                ensure!(
                    matches!(ty, AstType::Type(_)),
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
        expr: &AstExpr,
        type_value: &AstType,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        match expr {
            AstExpr::Locator(n) => {
                let expr = ctx
                    .get_expr(n.to_path())
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                return self.type_check_expr_against_value(&expr, type_value, ctx);
            }

            AstExpr::Value(v) => return self.type_check_value(v, type_value),
            _ => {}
        }
        Ok(())
    }

    pub fn evaluate_type_value(&self, ty: &AstType, ctx: &SharedScopedContext) -> Result<AstType> {
        match ty {
            AstType::Expr(expr) => {
                let value = self.interpret_expr(expr, ctx)?;
                let ty = value.try_conv()?;
                return Ok(ty);
            }
            AstType::Struct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
                        Ok::<_, Error>(StructuralField {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                return Ok(AstType::Struct(TypeStruct {
                    name: n.name.clone(),
                    fields,
                }));
            }
            AstType::Structural(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
                        Ok::<_, Error>(StructuralField {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                return Ok(AstType::Structural(TypeStructural { fields }));
            }
            AstType::Function(f) => {
                let sub = ctx.child(Ident::new("__func__"), Visibility::Private, false);
                for g in &f.generics_params {
                    let constrain = self.evaluate_type_bounds(&g.bounds, &sub)?;
                    sub.insert_value_with_ctx(g.name.clone(), constrain.into());
                }
                let params = f
                    .params
                    .iter()
                    .map(|x| self.evaluate_type_value(x, &sub))
                    .try_collect()?;
                let ret_ty = self.evaluate_type_value(&f.ret_ty, &sub)?;
                return Ok(AstType::Function(
                    TypeFunction {
                        params,
                        generics_params: f.generics_params.clone(),
                        ret_ty: ret_ty.into(),
                    }
                    .into(),
                ));
            }
            AstType::TypeBounds(b) => return self.evaluate_type_bounds(b, ctx),
            AstType::ImplTraits(t) => return self.evaluate_impl_traits(t, ctx),
            _ => Ok(ty.clone()),
        }
    }
    pub fn evaluate_impl_traits(
        &self,
        traits: &ImplTraits,
        ctx: &SharedScopedContext,
    ) -> Result<AstType> {
        let traits = self.evaluate_type_bounds(&traits.bounds, ctx)?;
        match traits {
            AstType::TypeBounds(bounds) => Ok(AstType::ImplTraits(ImplTraits { bounds })),
            _ => Ok(traits),
        }
    }

    pub fn evaluate_type_bounds(
        &self,
        bounds: &TypeBounds,
        ctx: &SharedScopedContext,
    ) -> Result<AstType> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        if bounds.is_empty() {
            return Ok(AstType::any());
        }
        if bounds.len() == 1 {
            return bounds.first().unwrap().clone().try_conv();
        }

        bail!("failed to evaluate type bounds: {:?}", bounds)
        // Ok(TypeValue::TypeBounds(TypeBounds { bounds }))
    }

    pub fn type_check_expr(
        &self,
        expr: &AstExpr,
        ty: &AstExpr,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        let tv = self.interpret_expr(ty, ctx)?.try_conv()?;

        self.type_check_expr_against_value(expr, &tv, ctx)
    }

    pub fn infer_type_call(
        &self,
        callee: &AstExpr,
        params: &[AstExpr],
        ctx: &SharedScopedContext,
    ) -> Result<AstType> {
        match callee {
            AstExpr::Locator(Locator::Ident(ident)) => match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_expr(params.first().context("No param")?, ctx)
                }
                "print" => return Ok(AstType::unit()),
                _ => {}
            },
            _ => {}
        }

        let callee = self.infer_expr(callee, ctx)?;
        match callee {
            AstType::Function(f) => return Ok(*f.ret_ty),
            _ => {}
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_ident(&self, ident: &Ident, ctx: &SharedScopedContext) -> Result<AstType> {
        match ident.as_str() {
            "print" => {
                return Ok(AstType::Function(
                    TypeFunction {
                        params: vec![],
                        generics_params: vec![],
                        ret_ty: AstType::unit().into(),
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
    pub fn infer_locator(&self, locator: &Locator, ctx: &SharedScopedContext) -> Result<AstType> {
        if let Some(ty) = ctx.get_type(locator.to_path()) {
            return Ok(ty);
        }
        match locator {
            Locator::Ident(ident) => self.infer_ident(ident, ctx),
            _ => bail!("Could not infer locator {:?}", locator),
        }
    }
    pub fn infer_expr_invoke_target(
        &self,
        target: &ExprInvokeTarget,
        ctx: &SharedScopedContext,
    ) -> Result<AstType> {
        match target {
            ExprInvokeTarget::Function(ident) => self.infer_locator(ident, ctx),

            _ => bail!("Could not infer invoke target {:?}", target),
        }
    }
    pub fn infer_expr(&self, expr: &AstExpr, ctx: &SharedScopedContext) -> Result<AstType> {
        let ret = match expr {
            AstExpr::Locator(n) => self.infer_locator(n, ctx)?,
            AstExpr::Value(l) => match l.as_ref() {
                Value::Int(_) => AstType::Primitive(TypePrimitive::Int(TypeInt::I64)),
                Value::Decimal(_) => AstType::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                Value::Unit(_) => AstType::unit(),
                Value::Bool(_) => AstType::Primitive(TypePrimitive::Bool),
                Value::String(_) => AstType::Primitive(TypePrimitive::String),
                Value::Type(_) => AstType::Type(TypeType {}),
                Value::Char(_) => AstType::Primitive(TypePrimitive::Char),
                Value::List(_) => AstType::Primitive(TypePrimitive::List),
                _ => bail!("Could not infer type of {:?}", l),
            },
            AstExpr::Invoke(invoke) => {
                let function = self.infer_expr_invoke_target(&invoke.target, ctx)?;
                match function {
                    AstType::Function(f) => *f.ret_ty,
                    _ => bail!("Expected function, got {:?}", function),
                }
            }
            AstExpr::BinOp(op) => {
                if op.kind.is_ret_bool() {
                    return Ok(AstType::Primitive(TypePrimitive::Bool));
                }
                let lhs = self.infer_expr(&op.lhs, ctx)?;
                let rhs = self.infer_expr(&op.rhs, ctx)?;
                ensure!(
                    lhs == rhs,
                    "Expected same types, got {:?} and {:?}",
                    lhs,
                    rhs
                );
                lhs
            }
            _ => bail!("Could not infer type of {:?}", expr),
        };
        Ok(ret)
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
        let ret_ty = func.ret_ty.clone();
        Ok(TypeFunction {
            params,
            generics_params: func.generics_params.clone(),
            ret_ty: ret_ty.into(),
        })
    }
}
impl TypeSystem for InterpreterPass {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &AstExpr) -> Result<AstType> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));

        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            AstExpr::Value(v) => match v.into() {
                Value::Type(t) => return Ok(t),
                v => bail!("Expected type, got {:?}", v),
            },
            _ => bail!("Expected type, got {:?}", expr),
        }
    }
    fn get_ty_from_value(&self, ctx: &Context, value: &Value) -> Result<AstType> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));

        let value = fold.optimize_expr(AstExpr::Value(value.clone().into()), &ctx.values)?;

        match value {
            AstExpr::Value(v) => match v.into() {
                Value::Type(t) => return Ok(t),
                v => bail!("Expected type, got {:?}", v),
            },
            _ => bail!("Expected type, got {:?}", value),
        }
    }
}
