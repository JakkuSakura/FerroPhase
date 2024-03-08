use crate::ast::{DefFunction, Import, Item, Visibility};
use crate::context::SharedScopedContext;
use crate::expr::*;
use crate::id::{Ident, Locator};
use crate::pass::{InterpreterPass, OptimizePass};
use crate::pat::{Pattern, PatternIdent};
use crate::value::*;
use crate::*;
use common::*;
use itertools::{zip_eq, Itertools};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

pub struct SpecializePass {
    spec_id: AtomicUsize,
    serializer: Arc<dyn Serializer>,
    interpreter: InterpreterPass,
}
impl SpecializePass {
    pub fn new(serializer: Arc<dyn Serializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
            interpreter: InterpreterPass::new(serializer.clone()),
            serializer,
        }
    }

    pub fn specialize_import(&self, import: Import, _ctx: &SharedScopedContext) -> Result<Import> {
        Ok(import)
    }

    pub fn specialize_invoke_details(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        let mut args: Vec<AExpr> = vec![];
        for arg in invoke.args.iter() {
            let x = match arg.get() {
                Expr::Locator(v) => ctx
                    .get_expr(v.to_path())
                    .with_context(|| format!("Couldn't find {:?} in context", v))?,
                x => x,
            };
            args.push(x.into())
        }

        debug!(
            "Specializing Invoke {} with {} [{}]",
            func,
            self.serializer.serialize_args_arena(&args)?,
            ctx.list_values()
                .into_iter()
                .map(|x| x.to_string())
                .join(", ")
        );
        let name = func.name.as_ref().map(|x| x.name.as_str()).unwrap_or("fun");
        let mut new_params: Vec<FunctionParam> = vec![];
        let mut new_args: Vec<Expr> = vec![];
        for (param, arg) in zip_eq(func.params.iter(), args.iter()) {
            match self.interpreter.interpret_expr(&arg.get(), ctx) {
                Err(err) => {
                    warn!("Cannot evaluate arg {} {:?}: {:?}", param.name, arg, err);
                    new_args.push(arg.get());
                    new_params.push(param.clone());
                }
                Ok(_) => {}
            }
        }
        if !new_params.is_empty() && new_params.len() == func.params.len() {
            warn!(
                "Couldn't specialize Invoke {} with {}",
                self.serializer.serialize_function(&func)?,
                self.serializer.serialize_args_arena(&args)?,
            );
            ctx.print_values()?;
            return Ok(invoke.into());
        }
        let mut bindings = vec![];
        for name in ctx.list_values() {
            let value = ctx.get_value(&name).unwrap();

            if matches!(value, Value::Function(_)) {
                warn!("Skipping function {}", name);
                continue;
            }
            let name = name.last().clone();

            let binding = Statement::Let(StatementLet {
                pat: Pattern::Ident(PatternIdent {
                    ident: name,
                    mutability: Some(false),
                }),
                value: Expr::value(value),
            });
            bindings.push(binding);
        }

        let new_body = Expr::block(Block::prepend(bindings, func.body.clone()));
        let new_name = Ident::new(format!(
            "{}_{}",
            name,
            self.spec_id.fetch_add(1, Ordering::Relaxed)
        ));

        let mut ret = func.ret.clone();
        match &ret {
            Type::Expr(expr) => match &**expr {
                Expr::Locator(Locator::Ident(ident))
                    if func
                        .generics_params
                        .iter()
                        .find(|x| &x.name == ident)
                        .is_some() =>
                {
                    ret = self.interpreter.infer_expr(&new_body, &ctx)?.into();
                }
                _ => {}
            },
            _ => {}
        }
        let sig = FunctionSignature {
            name: Some(new_name.clone()),
            params: new_params.clone(),
            generics_params: vec![],
            ret: ret.clone(),
        };
        let new_func = ValueFunction {
            sig,
            body: new_body.into(),
        };
        debug!(
            "Specialized function {} with {} => {}",
            name,
            self.serializer.serialize_args_arena(&args)?,
            self.serializer.serialize_function(&new_func)?
        );

        // ctx.root()
        //     .insert_specialized(new_name.clone().into(), new_func);
        // return Ok(Invoke {
        //     func: Expr::ident(new_name).into(),
        //     args: Default::default(),
        // });
        if invoke.args.is_empty() {
            Ok(new_func.body)
        } else {
            Ok(Block::new(vec![
                Item::DefFunction(DefFunction {
                    name: new_name.clone(),
                    ty: None,
                    value: new_func,
                    visibility: Visibility::Private,
                })
                .into(),
                Statement::Expr(
                    Invoke {
                        func: Expr::ident(new_name).into(),
                        args: Default::default(),
                    }
                    .into(),
                ),
            ])
            .into())
        }
    }

    pub fn specialize_invoke_func(
        &self,
        invoke: Invoke,
        func: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match invoke.func.get() {
            Expr::Locator(Locator::Ident(ident)) if ident.as_str() == "print" => {
                return Ok(invoke.into());
            }
            _ => {}
        }

        self.specialize_invoke_details(invoke, func, ctx)
    }
}
impl OptimizePass for SpecializePass {
    fn name(&self) -> &str {
        "specialize"
    }

    fn try_evaluate_expr(&self, pat: &Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        match ctx.try_get_value_from_expr(pat) {
            Some(value) => Ok(Expr::value(value)),
            None => Ok(pat.clone()),
        }
    }
    fn optimize_expr(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<Expr> {
        match ctx.try_get_value_from_expr(&expr) {
            Some(value) => Ok(Expr::value(value)),
            None => Ok(expr),
        }
    }

    fn evaluate_invoke(&self, _invoke: Invoke, _ctx: &SharedScopedContext) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: Invoke,
        func: &Value,
        ctx: &SharedScopedContext,
    ) -> Result<Expr> {
        match func {
            Value::Function(func) => self
                .specialize_invoke_func(invoke, func, ctx)
                .map(|x| x.into()),

            _ => {
                if let Ok(v) = self.interpreter.optimize_invoke(invoke.clone(), func, ctx) {
                    return Ok(v);
                }
                Ok(invoke.into())
            }
        }
    }
    fn evaluate_condition(&self, expr: Expr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        self.interpreter.evaluate_condition(expr, ctx)
    }
}
