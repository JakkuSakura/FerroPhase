use crate::orchestrators::InterpretationOrchestrator;
use crate::utils::OptimizePass;
// Replace common::* with specific imports
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::id::{Ident, Locator};
use itertools::{zip_eq, Itertools};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tracing::{debug, warn};

// Import our error helpers
use crate::error::optimization_error;

pub struct SpecializePass {
    spec_id: AtomicUsize,
    serializer: Arc<dyn AstSerializer>,
    // TODO: use Context instead of InterpretationOrchestrator
    interpreter: InterpretationOrchestrator,
}
impl SpecializePass {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
            interpreter: InterpretationOrchestrator::new(serializer.clone()),
            serializer,
        }
    }

    pub fn specialize_import(
        &self,
        import: ItemImport,
        _ctx: &SharedScopedContext,
    ) -> Result<ItemImport> {
        Ok(import)
    }

    pub fn specialize_invoke_details(
        &self,
        invoke: ExprInvoke,
        func: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<AstExpr> {
        let mut args: Vec<BExpr> = vec![];
        for arg in invoke.args.iter() {
            let x = match arg.get() {
                AstExpr::Locator(v) => ctx.get_expr(v.to_path()).ok_or_else(|| {
                    optimization_error(format!("Couldn't find {:?} in context", v))
                })?,
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
        let mut new_args: Vec<AstExpr> = vec![];
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
                self.serializer.serialize_value_function(&func)?,
                self.serializer.serialize_args_arena(&args)?,
            );
            ctx.print_values()?;
            return Ok(invoke.into());
        }
        let mut bindings = vec![];
        for name in ctx.list_values() {
            let value = ctx.get_value(&name).unwrap();

            if matches!(value, AstValue::Function(_)) {
                warn!("Skipping function {}", name);
                continue;
            }
            let name = name.last().clone();

            let binding = BlockStmt::Let(StmtLet::new_simple(name, AstExpr::value(value).into()));
            bindings.push(binding);
        }

        let new_body = AstExpr::block(ExprBlock::new_stmts_expr(bindings, func.body.clone()));
        let new_name = Ident::new(format!(
            "{}_{}",
            name,
            self.spec_id.fetch_add(1, Ordering::Relaxed)
        ));

        let mut ret = func.ret_ty.clone();
        match &ret {
            Some(AstType::Expr(expr)) => match &**expr {
                AstExpr::Locator(Locator::Ident(ident))
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
            receiver: None,
            params: new_params.clone(),
            generics_params: vec![],
            ret_ty: ret.clone(),
        };
        let new_func = ValueFunction {
            sig,
            body: new_body.into(),
        };
        debug!(
            "Specialized function {} with {} => {}",
            name,
            self.serializer.serialize_args_arena(&args)?,
            self.serializer.serialize_value_function(&new_func)?
        );

        // ctx.root()
        //     .insert_specialized(new_name.clone().into(), new_func);
        // return Ok(Invoke {
        //     func: Expr::ident(new_name).into(),
        //     args: Default::default(),
        // });
        if invoke.args.is_empty() {
            return Ok(new_func.body.into());
        }
        let block = ExprBlock::new_stmts_expr(
            vec![BlockStmt::Item(
                AstItem::DefFunction(ItemDefFunction {
                    attrs: vec![],
                    name: new_name.clone(),
                    ty: None,
                    sig: new_func.sig,
                    body: new_func.body,
                    visibility: Visibility::Private,
                })
                .into(),
            )],
            AstExpr::Invoke(ExprInvoke {
                target: ExprInvokeTarget::Function(new_name.into()),
                args: Default::default(),
            }),
        );
        Ok(AstExpr::Block(block))
    }

    pub fn specialize_invoke_func(
        &self,
        invoke: ExprInvoke,
        func: &ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<AstExpr> {
        match &invoke.target {
            ExprInvokeTarget::Function(Locator::Ident(ident)) if ident.as_str() == "print" => {
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

    fn try_evaluate_expr(&self, pat: &AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        match ctx.try_get_value_from_expr(pat) {
            Some(value) => Ok(AstExpr::value(value)),
            None => Ok(pat.clone()),
        }
    }
    fn optimize_expr(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        match ctx.try_get_value_from_expr(&expr) {
            Some(value) => Ok(AstExpr::value(value)),
            None => Ok(expr),
        }
    }

    fn evaluate_invoke(
        &self,
        _invoke: ExprInvoke,
        _ctx: &SharedScopedContext,
    ) -> Result<ControlFlow> {
        Ok(ControlFlow::Into)
    }
    fn optimize_invoke(
        &self,
        invoke: ExprInvoke,
        func: &AstValue,
        ctx: &SharedScopedContext,
    ) -> Result<AstExpr> {
        match func {
            AstValue::Function(func) => self
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
    fn evaluate_condition(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<ControlFlow> {
        self.interpreter.evaluate_condition(expr, ctx)
    }
}
