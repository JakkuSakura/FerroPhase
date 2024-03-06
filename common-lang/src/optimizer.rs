use crate::ast::{DefFunction, File, Import, Item, Module, Tree, Visibility};
use crate::context::ArcScopedContext;
use crate::expr::*;
use crate::id::Ident;
use crate::passes::*;
use crate::value::*;
use crate::*;
use common::*;
use std::mem::take;
use std::rc::Rc;

pub fn load_optimizers(serializer: Rc<dyn Serializer>) -> Vec<FoldOptimizer> {
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
    serializer: Rc<dyn Serializer>,
    pub(crate) pass: Box<dyn OptimizePass>,
}
impl FoldOptimizer {
    pub fn new(serializer: Rc<dyn Serializer>, pass: Box<dyn OptimizePass>) -> Self {
        Self { serializer, pass }
    }

    pub fn optimize_invoke(&self, mut invoke: Invoke, ctx: &ArcScopedContext) -> Result<Expr> {
        let func = self.optimize_expr(invoke.func.into(), ctx)?;
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
                    Expr::Value(value) => match *value {
                        Value::Function(mut f) => {
                            // TODO: when calling function, use context of its own, instead of use current context

                            let sub_ctx = ctx.child(
                                f.name.clone().unwrap_or(Ident::new("__func__")),
                                Visibility::Private,
                                false,
                            );
                            for (i, arg) in invoke.args.clone().into_iter().enumerate() {
                                let param = f.params.get(i).with_context(|| {
                                    format!("Couldn't find {} parameter of {:?}", i, f)
                                })?;

                                sub_ctx.insert_expr(param.name.clone(), arg.into());
                            }
                            debug!("Doing {} for {} invoking 1", self.pass.name(), invoke);
                            f.body = self.optimize_expr(f.body, &sub_ctx)?;

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

                            return Ok(ret);
                        }
                        value => {
                            let ret = self.pass.optimize_invoke(invoke, &value, ctx)?;
                            return Ok(ret);
                        }
                    },
                    _ => {
                        warn!(
                            "Couldn't optimize {} due to {} not in context",
                            invoke, func
                        );
                        ctx.print_values()?;
                    }
                }
            }
            ControlFlow::Continue => {}
            _ => bail!("Cannot handle control flow {:?}", control),
        }

        let invoke = Expr::Invoke(invoke.into());
        Ok(invoke)
    }

    pub fn optimize_expr(&self, mut expr: Expr, ctx: &ArcScopedContext) -> Result<Expr> {
        let serialized = self.serializer.serialize_expr(&expr)?;
        debug!("Doing {} for {}", self.pass.name(), serialized);

        expr = match expr {
            Expr::Locator(val) => {
                info!("Looking for {}", val);
                ctx.get_expr(&val)
                    .with_context(|| format!("Couldn't find {}", val))?
            }
            Expr::Block(x) => self.optimize_block(x, ctx)?,
            Expr::Match(x) => self.optimize_match(x, ctx)?,
            Expr::If(x) => self.optimize_if(x, ctx)?,
            Expr::Invoke(x) => self.optimize_invoke(x, ctx)?,
            _ => self.pass.optimize_expr(expr, ctx)?,
        };

        debug!("Done {} for {} => {}", self.pass.name(), serialized, expr);

        Ok(expr)
    }

    pub fn optimize_import(&self, import: Import, _ctx: &ArcScopedContext) -> Result<Import> {
        Ok(import)
    }

    pub fn optimize_module(
        &self,
        mut module: Module,
        ctx: &ArcScopedContext,
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
    fn prescan_item(&self, item: &Item, ctx: &ArcScopedContext) -> Result<()> {
        match item {
            Item::DefFunction(x) => self.prescan_def_function(x, ctx),
            Item::Module(x) => self.prescan_module(x, ctx),
            _ => Ok(()),
        }
    }
    fn prescan_module(&self, module: &Module, ctx: &ArcScopedContext) -> Result<()> {
        let module = module.clone();
        for item in module.items {
            self.prescan_item(&item, ctx)?;
        }
        Ok(())
    }
    pub fn optimize_item(&self, mut item: Item, ctx: &ArcScopedContext) -> Result<Item> {
        let serialized = self.serializer.serialize_item(&item)?;
        debug!("Doing {} for {}", self.pass.name(), serialized);

        item = match item {
            // Item::DefFunction(x) => self.optimize_def_function(x, ctx).map(Item::DefFunction)?,
            Item::Import(x) => self.optimize_import(x, ctx).map(Item::Import)?,
            Item::Module(x) => self.optimize_module(x, ctx, true).map(Item::Module)?,
            Item::Expr(x) => {
                let expr = self.optimize_expr(x, ctx)?;
                Item::Expr(expr)
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

    pub fn optimize_let(&self, let_: StatementLet, ctx: &ArcScopedContext) -> Result<StatementLet> {
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
    pub fn optimize_stmt(&self, stmt: Statement, ctx: &ArcScopedContext) -> Result<Statement> {
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
            Statement::SideEffect(x) => {
                let expr = self.optimize_expr(x.expr, ctx)?;
                Ok(Statement::SideEffect(SideEffect { expr }))
            }
            _ => bail!("Could not optimize {:?}", stmt),
        }
    }

    fn prescan_stmt(&self, stmt: &Statement, ctx: &ArcScopedContext) -> Result<()> {
        match stmt {
            Statement::Item(x) => self.prescan_item(&**x, ctx),

            _ => Ok(()),
        }
    }
    pub fn optimize_block(&self, mut b: Block, ctx: &ArcScopedContext) -> Result<Expr> {
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
        Ok(Expr::block(b))
    }
    pub fn optimize_match(&self, b: Match, ctx: &ArcScopedContext) -> Result<Expr> {
        let mut cases = vec![];
        for case in b.cases {
            let cond = self.optimize_expr(case.cond, ctx)?;
            let do_continue = self.pass.evaluate_condition(cond.clone(), ctx)?;
            match do_continue {
                ControlFlow::Continue => continue,
                ControlFlow::Break(_) => break,
                ControlFlow::Return(_) => break,
                ControlFlow::Into => {
                    let body = self.optimize_expr(case.body, ctx)?;
                    cases.push(MatchCase { cond, body });
                }
                ControlFlow::IntoAndBreak(_) => {
                    let body = self.optimize_expr(case.body, ctx)?;
                    cases.push(MatchCase { cond, body });
                    break;
                }
            }
        }
        if cases.len() == 1 {
            return Ok(cases.into_iter().next().unwrap().body);
        }
        Ok(Expr::Match(Match { cases }))
    }
    pub fn optimize_if(&self, if_: If, ctx: &ArcScopedContext) -> Result<Expr> {
        let match_ = Match { cases: if_.cases };
        let expr = self.optimize_match(match_, ctx)?;
        if let Expr::Match(match_) = expr {
            return Ok(Expr::If(If {
                cases: match_.cases,
            }));
        }
        Ok(expr)
    }
    pub fn optimize_func(
        &self,
        func: ValueFunction,
        ctx: &ArcScopedContext,
    ) -> Result<ValueFunction> {
        let body = self.optimize_expr(func.body, ctx)?;
        Ok(ValueFunction {
            body: body.into(),
            ..func
        })
    }

    pub fn optimize_def_function(
        &self,
        mut def: DefFunction,
        ctx: &ArcScopedContext,
    ) -> Result<DefFunction> {
        def.value = self.optimize_func(def.value, ctx)?;

        Ok(def)
    }
    pub fn prescan_def_function(&self, def: &DefFunction, ctx: &ArcScopedContext) -> Result<()> {
        let def = def.clone();
        match def.name.as_str() {
            _ => {
                debug!(
                    "Registering function {}",
                    self.serializer.serialize_function(&def.value)?,
                );

                ctx.insert_function(def.name.clone(), def.value.clone());
                Ok(())
            }
        }
    }
    pub fn optimize_file(&self, mut file: File, ctx: &ArcScopedContext) -> Result<File> {
        file.module = self.optimize_module(file.module, ctx, false)?;
        Ok(file)
    }
    pub fn optimize_tree(&self, node: Tree, ctx: &ArcScopedContext) -> Result<Tree> {
        match node {
            Tree::Item(item) => {
                let item = self.optimize_item(item, ctx)?;
                Ok(Tree::Item(item))
            }
            Tree::Expr(expr) => {
                let expr = self.optimize_expr(expr, ctx)?;
                Ok(Tree::Expr(expr))
            }
            Tree::File(file) => {
                let file = self.optimize_file(file, ctx)?;
                Ok(Tree::File(file))
            }
        }
    }
}
