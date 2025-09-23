use crate::passes::{InlinePass, SpecializePass};
use crate::utils::OptimizePass;
// Replace common::* with specific imports
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use fp_core::id::Ident;
use itertools::Itertools;
use std::mem::take;
use std::sync::Arc;
use tracing::{debug, info, warn};

// Import our custom error helpers
use crate::error::optimization_error;
use crate::opt_bail;

pub fn load_optimizers(serializer: Arc<dyn AstSerializer>) -> Vec<FoldOptimizer> {
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
    serializer: Arc<dyn AstSerializer>,
    pub(crate) pass: Box<dyn OptimizePass>,
}
impl FoldOptimizer {
    pub fn new(serializer: Arc<dyn AstSerializer>, pass: Box<dyn OptimizePass>) -> Self {
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
                    .ok_or_else(|| optimization_error(format!("Couldn't find {}", id)))?;
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
                        AstValue::Function(mut f) => {
                            // TODO: when calling function, use context of its own, instead of use current context

                            let sub_ctx = closure_context
                                .map(|x| x.child("__invoke__".into(), Visibility::Private, false))
                                .unwrap_or_else(|| SharedScopedContext::new());
                            for (i, arg) in invoke.args.clone().into_iter().enumerate() {
                                let param = f.params.get(i).ok_or_else(|| {
                                    optimization_error(format!(
                                        "Couldn't find {} parameter of {:?}",
                                        i, f
                                    ))
                                })?;

                                sub_ctx.insert_expr(param.name.clone(), arg.into());
                            }
                            debug!("Doing {} for {} invoking 1", self.pass.name(), invoke);
                            f.body = self.optimize_expr(f.body.into(), &sub_ctx)?.into();

                            debug!("Doing {} for {} invoking 2", self.pass.name(), invoke);

                            let ret = self.pass.optimize_invoke(
                                invoke.clone(),
                                &AstValue::Function(f),
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
            ControlFlow::Continue => Ok(AstExpr::Invoke(invoke.into())),
            _ => opt_bail!(format!("Cannot handle control flow {:?}", control)),
        }
    }

    pub fn optimize_expr(&self, mut expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let serialized = self.serializer.serialize_expr(&expr)?;
        debug!("Doing {} for {}", self.pass.name(), serialized);

        expr = match expr {
            AstExpr::Locator(val) => {
                info!("Looking for {}", val);
                ctx.get_expr_with_ctx(val.to_path())
                    .ok_or_else(|| optimization_error(format!("Couldn't find {}", val)))?
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

    pub fn optimize_import(
        &self,
        import: ItemImport,
        _ctx: &SharedScopedContext,
    ) -> Result<ItemImport> {
        Ok(import)
    }

    pub fn optimize_module(
        &self,
        mut module: AstModule,
        ctx: &SharedScopedContext,
        with_submodule: bool,
    ) -> Result<AstModule> {
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
    fn prescan_module(&self, module: &AstModule, ctx: &SharedScopedContext) -> Result<()> {
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

    pub fn optimize_let(&self, let_: StmtLet, ctx: &SharedScopedContext) -> Result<StmtLet> {
        if let Some(init) = &let_.init {
            let init = self.optimize_expr(init.clone(), ctx)?;
            let value = self.pass.try_evaluate_expr(&init, ctx)?;
            let ident = let_
                .pat
                .as_ident()
                .ok_or_else(|| optimization_error("Only supports ident"))?
                .clone();

            ctx.insert_expr(ident, value.clone());

            Ok(StmtLet::new(let_.pat.clone(), value.into(), None))
        } else {
            Ok(StmtLet::new(let_.pat.clone(), None, None))
        }
    }
    pub fn optimize_stmt(&self, stmt: BlockStmt, ctx: &SharedScopedContext) -> Result<BlockStmt> {
        match stmt {
            BlockStmt::Expr(x) => {
                let expr = self.optimize_expr(*x.expr, ctx)?;
                Ok(BlockStmt::Expr(BlockStmtExpr {
                    expr: expr.into(),
                    semicolon: x.semicolon,
                }))
            }
            BlockStmt::Item(x) => self
                .optimize_item(*x, ctx)
                .map(Box::new)
                .map(BlockStmt::Item),
            BlockStmt::Any(_) => Ok(stmt),
            BlockStmt::Let(x) => self.optimize_let(x, ctx).map(BlockStmt::Let),
            #[allow(unreachable_patterns)]
            _ => opt_bail!(format!("Could not optimize {:?}", stmt)),
        }
    }

    fn prescan_stmt(&self, stmt: &BlockStmt, ctx: &SharedScopedContext) -> Result<()> {
        match stmt {
            BlockStmt::Item(x) => self.prescan_item(&**x, ctx),

            _ => Ok(()),
        }
    }
    pub fn optimize_block(&self, mut b: ExprBlock, ctx: &SharedScopedContext) -> Result<AstExpr> {
        let ctx = ctx.child(Ident::new("__block__"), Visibility::Private, true);
        b.stmts
            .iter()
            .try_for_each(|x| self.prescan_stmt(x, &ctx))?;

        let stmts: Vec<_> = b
            .stmts
            .into_iter()
            .map(|x| self.optimize_stmt(x, &ctx))
            .try_collect()?;
        // stmts.retain(|x| !x.is_unit());

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
                cond: AstExpr::Value(AstValue::Bool(ValueBool { value: true }).into()).into(),
                body: elze,
            });
        }
        let match_ = ExprMatch { cases };

        let match_ = self.optimize_match(match_, ctx)?;
        if let AstExpr::Match(match_) = match_ {
            if match_.cases.len() == 0 {
                return Ok(AstExpr::Value(AstValue::Unit(ValueUnit).into()));
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
        mut def: ItemDefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<ItemDefFunction> {
        let value = self.optimize_func(def._to_value(), ctx)?;
        def.body = value.body;
        def.sig = value.sig;

        Ok(def)
    }
    pub fn prescan_def_function(
        &self,
        def: &ItemDefFunction,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        match def.name.as_str() {
            _ => {
                debug!(
                    "Registering function {}",
                    self.serializer.serialize_def_function(def)?,
                );

                ctx.insert_value_with_ctx(def.name.clone(), AstValue::Function(def._to_value()));
                Ok(())
            }
        }
    }
    pub fn optimize_file(&self, mut file: AstFile, ctx: &SharedScopedContext) -> Result<AstFile> {
        file.items = self
            .optimize_module(
                AstModule {
                    name: "__file__".into(),
                    items: file.items,
                    visibility: Visibility::Public,
                },
                ctx,
                false,
            )?
            .items;
        Ok(file)
    }
    pub fn optimize_tree(&self, node: AstNode, ctx: &SharedScopedContext) -> Result<AstNode> {
        match node {
            AstNode::Item(item) => {
                let item = self.optimize_item(item, ctx)?;
                Ok(AstNode::Item(item))
            }
            AstNode::Expr(expr) => {
                let expr = self.optimize_expr(expr, ctx)?;
                Ok(AstNode::Expr(expr))
            }
            AstNode::File(file) => {
                let file = self.optimize_file(file, ctx)?;
                Ok(AstNode::File(file))
            }
        }
    }
}
