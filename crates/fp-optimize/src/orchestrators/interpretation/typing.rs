use fp_core::error::Result;
use itertools::Itertools;

use fp_core::ast::{
    DecimalType, ExprInvokeTarget, ImplTraits, StructuralField, Ty, TypeBounds, TypeFunction,
    TypeInt, TypePrimitive, TypeStruct, TypeStructural, TypeType, Value, ValueFunction,
};
use fp_core::ast::{Expr, Visibility};
use fp_core::context::SharedScopedContext;
use fp_core::ctx::{Context, TypeSystem};
use fp_core::id::{Ident, Locator};
use fp_core::utils::conv::TryConv;

use crate::error::optimization_error;
use crate::opt_bail;
use crate::opt_ensure;
use crate::orchestrators::InterpretationOrchestrator;
use crate::utils::FoldOptimizer;

impl InterpretationOrchestrator {
    pub fn type_check_value(&self, lit: &Value, ty: &Ty) -> Result<()> {
        match lit {
            Value::Int(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::Int(_))),
                    format!("Expected i64, got {:?}", lit)
                )
            }
            Value::Bool(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::Bool)),
                    format!("Expected bool, got {:?}", lit)
                )
            }
            Value::Decimal(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::Decimal(_))),
                    format!("Expected f64, got {:?}", lit)
                )
            }
            Value::Char(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::Char)),
                    format!("Expected char, got {:?}", lit)
                )
            }
            Value::String(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::String)),
                    format!("Expected string, got {:?}", lit)
                )
            }
            Value::List(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Primitive(TypePrimitive::List)),
                    format!("Expected list, got {:?}", lit)
                )
            }
            Value::Unit(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Unit(_)),
                    format!("Expected unit, got {:?}", lit)
                )
            }
            Value::Type(_) => {
                opt_ensure!(
                    matches!(ty, Ty::Type(_)),
                    format!("Expected type, got {:?}", lit)
                )
            }
            _ => {}
        }
        Ok(())
    }
    pub fn type_check_expr_against_value(
        &self,
        expr: &Expr,
        type_value: &Ty,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        match expr {
            Expr::Locator(n) => {
                let expr = ctx.get_expr(n.to_path()).ok_or_else(|| {
                    optimization_error(format!("Could not find {:?} in context", n))
                })?;
                return self.type_check_expr_against_value(&expr, type_value, ctx);
            }

            Expr::Value(v) => return self.type_check_value(v, type_value),
            _ => {}
        }
        Ok(())
    }

    pub fn evaluate_type_value(&self, ty: &Ty, ctx: &SharedScopedContext) -> Result<Ty> {
        match ty {
            Ty::Expr(expr) => {
                let value = self.interpret_expr(expr, ctx)?;
                return Ok(value.try_conv()?);
            }
            Ty::Struct(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
                        Ok::<_, fp_core::error::Error>(StructuralField {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                return Ok(Ty::Struct(TypeStruct {
                    name: n.name.clone(),
                    fields,
                }));
            }
            Ty::Structural(n) => {
                let fields = n
                    .fields
                    .iter()
                    .map(|x| {
                        let value = self.evaluate_type_value(&x.value, ctx)?;
                        Ok::<_, fp_core::error::Error>(StructuralField {
                            name: x.name.clone(),
                            value,
                        })
                    })
                    .try_collect()?;
                return Ok(Ty::Structural(TypeStructural { fields }));
            }
            Ty::Function(f) => {
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

                let ret_ty = match &f.ret_ty {
                    Some(t) => Some(self.evaluate_type_value(t, &sub)?.into()),
                    None => None,
                };
                return Ok(Ty::Function(
                    TypeFunction {
                        params,
                        generics_params: f.generics_params.clone(),
                        ret_ty,
                    }
                    .into(),
                ));
            }
            Ty::TypeBounds(b) => return self.evaluate_type_bounds(b, ctx),
            Ty::ImplTraits(t) => return self.evaluate_impl_traits(t, ctx),
            _ => Ok(ty.clone()),
        }
    }
    pub fn evaluate_impl_traits(
        &self,
        traits: &ImplTraits,
        ctx: &SharedScopedContext,
    ) -> Result<Ty> {
        let traits = self.evaluate_type_bounds(&traits.bounds, ctx)?;
        match traits {
            Ty::TypeBounds(bounds) => Ok(Ty::ImplTraits(ImplTraits { bounds })),
            _ => Ok(traits),
        }
    }

    pub fn evaluate_type_bounds(
        &self,
        bounds: &TypeBounds,
        ctx: &SharedScopedContext,
    ) -> Result<Ty> {
        let bounds: Vec<_> = bounds
            .bounds
            .iter()
            .map(|x| self.interpret_expr(x, ctx))
            .try_collect()?;
        if bounds.is_empty() {
            return Ok(Ty::any());
        }
        if bounds.len() == 1 {
            return Ok(bounds.first().unwrap().clone().try_conv()?);
        }

        opt_bail!(format!("failed to evaluate type bounds: {:?}", bounds))
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
    ) -> Result<Ty> {
        match callee {
            Expr::Locator(Locator::Ident(ident)) => match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_expr(
                        params
                            .first()
                            .ok_or_else(|| optimization_error("No param"))?,
                        ctx,
                    )
                }
                "print" => return Ok(Ty::unit()),
                "println!" => return Ok(Ty::unit()),
                _ => {}
            },
            _ => {}
        }

        let callee = self.infer_expr(callee, ctx)?;
        match callee {
            Ty::Function(f) => {
                return match f.ret_ty {
                    Some(t) => Ok(*t),
                    None => Ok(Ty::unit()),
                }
            }
            _ => {}
        }

        opt_bail!(format!("Could not infer type call {:?}", callee))
    }
    pub fn infer_ident(&self, ident: &Ident, ctx: &SharedScopedContext) -> Result<Ty> {
        match ident.as_str() {
            "print" | "println!" => {
                return Ok(Ty::Function(
                    TypeFunction {
                        params: vec![],
                        generics_params: vec![],
                        ret_ty: None,
                    }
                    .into(),
                ))
            }
            _ => {}
        }
        let expr = ctx
            .get_expr(ident)
            .ok_or_else(|| optimization_error(format!("Could not find {:?} in context", ident)))?;
        self.infer_expr(&expr, ctx)
    }
    pub fn infer_locator(&self, locator: &Locator, ctx: &SharedScopedContext) -> Result<Ty> {
        if let Some(ty) = ctx.get_type(locator.to_path()) {
            return Ok(ty);
        }
        match locator {
            Locator::Ident(ident) => self.infer_ident(ident, ctx),
            _ => opt_bail!(format!("Could not infer locator {:?}", locator)),
        }
    }
    pub fn infer_expr_invoke_target(
        &self,
        target: &ExprInvokeTarget,
        ctx: &SharedScopedContext,
    ) -> Result<Ty> {
        match target {
            ExprInvokeTarget::Function(ident) => self.infer_locator(ident, ctx),

            _ => opt_bail!(format!("Could not infer invoke target {:?}", target)),
        }
    }
    pub fn infer_expr(&self, expr: &Expr, ctx: &SharedScopedContext) -> Result<Ty> {
        let ret = match expr {
            Expr::Locator(n) => self.infer_locator(n, ctx)?,
            Expr::Value(l) => match l.as_ref() {
                Value::Int(_) => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
                Value::Decimal(_) => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                Value::Unit(_) => Ty::unit(),
                Value::Bool(_) => Ty::Primitive(TypePrimitive::Bool),
                Value::String(_) => Ty::Primitive(TypePrimitive::String),
                Value::Type(_) => Ty::Type(TypeType {}),
                Value::Char(_) => Ty::Primitive(TypePrimitive::Char),
                Value::List(_) => Ty::Primitive(TypePrimitive::List),
                _ => opt_bail!(format!("Could not infer type of {:?}", l)),
            },
            Expr::Invoke(invoke) => {
                let function = self.infer_expr_invoke_target(&invoke.target, ctx)?;
                match function {
                    Ty::Function(TypeFunction { ret_ty: None, .. }) => Ty::unit(),
                    Ty::Function(TypeFunction {
                        ret_ty: Some(t), ..
                    }) => *t,

                    _ => opt_bail!(format!("Expected function, got {:?}", function)),
                }
            }
            Expr::BinOp(op) => {
                if op.kind.is_ret_bool() {
                    return Ok(Ty::Primitive(TypePrimitive::Bool));
                }
                let lhs = self.infer_expr(&op.lhs, ctx)?;
                let rhs = self.infer_expr(&op.rhs, ctx)?;
                opt_ensure!(
                    lhs == rhs,
                    format!("Expected same types, got {:?} and {:?}", lhs, rhs)
                );
                lhs
            }
            _ => opt_bail!(format!("Could not infer type of {:?}", expr)),
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
            ret_ty: ret_ty.map(|x| x.into()),
        })
    }
}
impl TypeSystem for InterpretationOrchestrator {
    fn get_ty_from_expr(&self, ctx: &Context, expr: &Expr) -> Result<Ty> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));

        let expr = fold.optimize_expr(expr.clone(), &ctx.values)?;
        match expr {
            Expr::Value(v) => match v.into() {
                Value::Type(t) => return Ok(t),
                v => opt_bail!(format!("Expected type, got {:?}", v)),
            },
            _ => opt_bail!(format!("Expected type, got {:?}", expr)),
        }
    }
    fn get_ty_from_value(&self, ctx: &Context, value: &Value) -> Result<Ty> {
        let fold = FoldOptimizer::new(self.serializer.clone(), Box::new(self.clone()));

        let value = fold.optimize_expr(Expr::Value(value.clone().into()), &ctx.values)?;

        match value {
            Expr::Value(v) => match v.into() {
                Value::Type(t) => return Ok(t),
                v => opt_bail!(format!("Expected type, got {:?}", v)),
            },
            _ => opt_bail!(format!("Expected type, got {:?}", value)),
        }
    }
}
