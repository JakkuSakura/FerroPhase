use crate::pass::{InlinePass, OptimizePass, SpecializePass};
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::context::SharedScopedContext;
use lang_core::id::Ident;
use lang_core::*;
use std::mem::take;
use std::sync::Arc;

pub fn load_optimizers(serializer: Arc<dyn Serializer>) -> Vec<FoldOptimizer> {
    let optimizers: Vec<Box<dyn OptimizePass>> = vec![
        Box::new(SpecializePass::new(serializer.clone())),
        Box::new(InlinePass::new(serializer.clone())),
    ];

    optimizers
        .into_iter()
        .map(|x| FoldOptimizer::new(serializer.clone(), x))
        .collect()
}

pub struct FoldOptimizer {
    serializer: Arc<dyn Serializer>,
    pub(crate) pass: Box<dyn OptimizePass>,
}
impl FoldOptimizer {
    pub fn new(serializer: Arc<dyn Serializer>, pass: Box<dyn OptimizePass>) -> Self {
        Self { serializer, pass }
    }

    pub fn optimize_invoke(
        &self,
        mut invoke: ExprInvoke,
        ctx: &SharedScopedContext,
    ) -> Result<AstExpr> {
        let mut func;
        let mut closure_context = None;
        match &invoke.target {
            ExprInvokeTarget::Function(id) => {
                func = ctx
                    .get_expr_with_ctx(id.to_path())
                    .with_context(|| format!("Couldn't find {}", id))?;
            }
            ExprInvokeTarget::Method(_) => {
                todo!()
            }
            ExprInvokeTarget::Type(_) => {
                todo!()
            }
            ExprInvokeTarget::BinOp(_) => {
                todo!()
            }
            ExprInvokeTarget::Closure(v) => {
                func = AstExpr::value(v.clone().into());
            }
            ExprInvokeTarget::Expr(expr) => {
                func = self.optimize_expr(expr.get(), ctx)?;
            }
        }

        if let AstExpr::Closured(f) = &func {
            closure_context = Some(f.ctx.clone());
            func = f.expr.get();
        }

        let args = take(&mut invoke.args);
        for arg in args {
            let expr = self.optimize_expr(arg.into(), ctx)?;
            let arg = self.pass.try_evaluate_expr(&expr, ctx)?;
            invoke.args.push(arg.into());
        }

        let control = self.pass.evaluate_invoke(invoke.clone(), ctx)?;
        match control {
            ControlFlow::Into => {
                // FIXME: optimize out this clone
                match func.clone() {
                    AstExpr::Value(value) => match value.into() {
                        Value::Function(mut f) => {
                            // TODO: when calling function, use context of its own, instead of use current context

                            let sub_ctx = closure_context
                                .map(|x| x.child("__invoke__".into(), Visibility::Private, false))
                                .unwrap_or_else(|| SharedScopedContext::new());
                            for (i, arg) in invoke.args.clone().into_iter().enumerate() {
                                let param = f.params.get(i).with_context(|| {
                                    format!("Couldn't find {} parameter of {:?}", i, f)
                                })?;

                                sub_ctx.insert_expr(param.name.clone(), arg.into());
                            }
                            debug!("Doing {} for {} invoking 1", self.pass.name(), invoke);
                            f.body = self.optimize_expr(f.body.into(), &sub_ctx)?.into();

                            debug!("Doing {} for {} invoking 2", self.pass.name(), invoke);

                            let ret = self.pass.optimize_invoke(
                                invoke.clone(),
                                &Value::Function(f),
                                &sub_ctx,
                            )?;

                            debug!(
                                "Done {} for {} invoking => {}",
                                self.pass.name(),
                                invoke,
                                ret
                            );

                            Ok(ret)
                        }
                        value => {
                            let ret = self.pass.optimize_invoke(invoke, &value, ctx)?;
                            Ok(ret)
                        }
                    },
                    _ => {
                        warn!(
                            "Couldn't optimize {} due to {} not in context",
                            invoke, func
                        );
                        ctx.print_values()?;
                        Ok(AstExpr::Invoke(invoke.into()))
                    }
                }
            }
            ControlFlow::Continue => {
                return Ok(AstExpr::Invoke(invoke.into()));
            }
            _ => bail!("Cannot handle control flow {:?}", control),
        }
    }

    pub fn optimize_expr(&self, mut expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let serialized = self.serializer.serialize_expr(&expr)?;
        debug!("Doing {} for {}", self.pass.name(), serialized);

        expr = match expr {
            AstExpr::Locator(val) => {
                info!("Looking for {}", val);
                ctx.get_expr_with_ctx(val.to_path())
                    .with_context(|| format!("Couldn't find {}", val))?
            }
            AstExpr::Block(x) => self.optimize_block(x, ctx)?,
            AstExpr::Match(x) => self.optimize_match(x, ctx)?,
            AstExpr::If(x) => self.optimize_if(x, ctx)?,
            AstExpr::Invoke(x) => self.optimize_invoke(x, ctx)?,
            _ => self.pass.optimize_expr(expr, ctx)?,
        };

        debug!("Done {} for {} => {}", self.pass.name(), serialized, expr);

        Ok(expr)
    }

    pub fn optimize_import(&self, import: Import, _ctx: &SharedScopedContext) -> Result<Import> {
        Ok(import)
    }

    pub fn optimize_module(
        &self,
        mut module: Module,
        ctx: &SharedScopedContext,
        with_submodule: bool,
    ) -> Result<Module> {
        let sub = if with_submodule {
            ctx.child(module.name.clone(), module.visibility, false)
        } else {
            ctx.clone()
        };
        module
            .items
            .iter()
            .try_for_each(|x| self.prescan_item(x, ctx))?;

        module.items = module
            .items
            .into_iter()
            .map(|x| self.optimize_item(x, &sub))
            .try_collect()?;
        module.items.retain(|x| !x.is_unit());

        let module = self.pass.optimize_module(module, ctx)?;
        Ok(module)
    }
    fn prescan_item(&self, item: &AstItem, ctx: &SharedScopedContext) -> Result<()> {
        match item {
            AstItem::DefFunction(x) => self.prescan_def_function(x, ctx),
            AstItem::Module(x) => self.prescan_module(x, ctx),
            _ => Ok(()),
        }
    }
    fn prescan_module(&self, module: &Module, ctx: &SharedScopedContext) -> Result<()> {
        let module = module.clone();
        for item in module.items {
            self.prescan_item(&item, ctx)?;
        }
        Ok(())
    }
    pub fn optimize_item(&self, mut item: AstItem, ctx: &SharedScopedContext) -> Result<AstItem> {
        let serialized = self.serializer.serialize_item(&item)?;
        debug!("Doing {} for {}", self.pass.name(), serialized);

        item = match item {
            // Item::DefFunction(x) => self.optimize_def_function(x, ctx).map(Item::DefFunction)?,
            AstItem::Import(x) => self.optimize_import(x, ctx).map(AstItem::Import)?,
            AstItem::Module(x) => self.optimize_module(x, ctx, true).map(AstItem::Module)?,
            AstItem::Expr(x) => {
                let expr = self.optimize_expr(x, ctx)?;
                AstItem::Expr(expr)
            }
            _ => item,
        };

        item = self.pass.optimize_item(item, ctx)?;

        let serialized2 = self.serializer.serialize_item(&item)?;
        // if serialized != serialized2 {
        debug!(
            "Done {} for {} => {}",
            self.pass.name(),
            serialized,
            serialized2
        );
        // }
        Ok(item)
    }

    pub fn optimize_let(
        &self,
        let_: StatementLet,
        ctx: &SharedScopedContext,
    ) -> Result<StatementLet> {
        let value = self.optimize_expr(let_.value, ctx)?;
        ctx.insert_expr(
            let_.pat.as_ident().context("Only supports ident")?.clone(),
            value.clone(),
        );

        Ok(StatementLet {
            pat: let_.pat.clone(),
            value,
        })
    }
    pub fn optimize_stmt(&self, stmt: Statement, ctx: &SharedScopedContext) -> Result<Statement> {
        match stmt {
            Statement::Expr(x) => {
                let expr = self.optimize_expr(x, ctx)?;
                Ok(Statement::Expr(expr))
            }
            Statement::Item(x) => self
                .optimize_item(*x, ctx)
                .map(Box::new)
                .map(Statement::Item),
            Statement::Any(_) => Ok(stmt),
            Statement::Let(x) => self.optimize_let(x, ctx).map(Statement::Let),
            #[allow(unreachable_patterns)]
            _ => bail!("Could not optimize {:?}", stmt),
        }
    }

    fn prescan_stmt(&self, stmt: &Statement, ctx: &SharedScopedContext) -> Result<()> {
        match stmt {
            Statement::Item(x) => self.prescan_item(&**x, ctx),

            _ => Ok(()),
        }
    }
    pub fn optimize_block(&self, mut b: ExprBlock, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);
        b.stmts
            .iter()
            .try_for_each(|x| self.prescan_stmt(x, &ctx))?;

        let mut stmts: Vec<_> = b
            .stmts
            .into_iter()
            .map(|x| self.optimize_stmt(x, &ctx))
            .try_collect()?;
        stmts.retain(|x| !x.is_unit());
        // FIXME: is it correct to remove unit statements with side effects?
        // stmts.retain(|x| {
        //     !matches!(
        //         x,
        //         Statement::SideEffect(SideEffect {
        //             expr: Expr::Value(Value::Unit(_))
        //         })
        //     )
        // });
        b.stmts = stmts;
        Ok(AstExpr::block(b))
    }
    pub fn optimize_match(&self, b: ExprMatch, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let mut cases = vec![];
        for case in b.cases {
            let cond: BExpr = self.optimize_expr(case.cond.into(), ctx)?.into();
            let do_continue = self.pass.evaluate_condition(cond.get(), ctx)?;
            match do_continue {
                ControlFlow::Continue => continue,
                ControlFlow::Break(_) => break,
                ControlFlow::Return(_) => break,
                ControlFlow::Into => {
                    let body: BExpr = self.optimize_expr(case.body.into(), ctx)?.into();
                    cases.push(ExprMatchCase { cond, body });
                }
                ControlFlow::IntoAndBreak(_) => {
                    let body: BExpr = self.optimize_expr(case.body.into(), ctx)?.into();
                    cases.push(ExprMatchCase { cond, body });
                    break;
                }
            }
        }

        Ok(AstExpr::Match(ExprMatch { cases }))
    }
    pub fn optimize_if(&self, if_: ExprIf, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let mut cases = vec![ExprMatchCase {
            cond: if_.cond,
            body: if_.then,
        }];
        if let Some(elze) = if_.elze {
            cases.push(ExprMatchCase {
                cond: AstExpr::Value(Value::Bool(ValueBool { value: true }).into()).into(),
                body: elze,
            });
        }
        let match_ = ExprMatch { cases };

        let match_ = self.optimize_match(match_, ctx)?;
        if let AstExpr::Match(match_) = match_ {
            if match_.cases.len() == 0 {
                return Ok(AstExpr::Value(Value::Unit(ValueUnit).into()));
            }

            if match_.cases.len() >= 1 {
                let mut iter = match_.cases.into_iter();
                let first = iter.next().unwrap();
                let second = iter.next();
                return Ok(ExprIf {
                    cond: first.cond,
                    then: first.body,
                    elze: second.map(|x| x.body),
                }
                .into());
            }
            unreachable!()
        }
        Ok(match_)
    }
    pub fn optimize_func(
        &self,
        func: ValueFunction,
        ctx: &SharedScopedContext,
    ) -> Result<ValueFunction> {
        let body = self.optimize_expr(func.body.into(), ctx)?;
        Ok(ValueFunction {
            body: body.into(),
            ..func
        })
    }

    pub fn optimize_def_function(
        &self,
        mut def: DefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<DefFunction> {
        def.value = self.optimize_func(def.value, ctx)?;

        Ok(def)
    }
    pub fn prescan_def_function(&self, def: &DefFunction, ctx: &SharedScopedContext) -> Result<()> {
        let def = def.clone();
        match def.name.as_str() {
            _ => {
                debug!(
                    "Registering function {}",
                    self.serializer.serialize_function(&def.value)?,
                );

                ctx.insert_value_with_ctx(def.name.clone(), Value::Function(def.value.clone()));
                Ok(())
            }
        }
    }
    pub fn optimize_file(&self, mut file: AstFile, ctx: &SharedScopedContext) -> Result<AstFile> {
        file.module = self.optimize_module(file.module, ctx, false)?;
        Ok(file)
    }
    pub fn optimize_tree(&self, node: AstTree, ctx: &SharedScopedContext) -> Result<AstTree> {
        match node {
            AstTree::Item(item) => {
                let item = self.optimize_item(item, ctx)?;
                Ok(AstTree::Item(item))
            }
            AstTree::Expr(expr) => {
                let expr = self.optimize_expr(expr, ctx)?;
                Ok(AstTree::Expr(expr))
            }
            AstTree::File(file) => {
                let file = self.optimize_file(file, ctx)?;
                Ok(AstTree::File(file))
            }
        }
    }
}
