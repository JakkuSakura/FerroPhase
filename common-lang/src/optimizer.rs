use crate::context::ExecutionContext;
use crate::passes::{InlinePass, OptimizePass, SpecializePass};
use crate::tree::*;
use crate::value::*;
use crate::*;
use common::*;
pub fn load_optimizer(serializer: Rc<dyn Serializer>) -> FoldOptimizer {
    let mut opt = FoldOptimizer::new(serializer.clone());
    opt.add_pass(SpecializePass::new(serializer.clone()));
    opt.add_pass(InlinePass::new(serializer.clone()));
    opt
}

pub struct FoldOptimizer {
    serializer: Rc<dyn Serializer>,
    passes: Vec<Box<dyn OptimizePass>>,
}
impl FoldOptimizer {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            serializer,
            passes: vec![],
        }
    }
    pub fn add_pass(&mut self, pass: impl OptimizePass + 'static) {
        self.passes.push(Box::new(pass));
    }

    pub fn optimize_invoke(&self, invoke: Invoke, ctx: &ExecutionContext) -> Result<Expr> {
        let func = self.optimize_expr(*invoke.func, ctx)?;
        let args: Vec<_> = invoke
            .args
            .into_iter()
            .map(|x| self.optimize_expr(x, ctx))
            .try_collect()?;
        let invoke = Invoke {
            func: func.into(),
            args,
        };
        Ok(Expr::Invoke(invoke))
    }
    pub fn optimize_expr(&self, mut expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        for pass in &self.passes {
            let serialized = self.serializer.serialize_expr(&expr)?;
            // debug!("Doing pass {} for {}", pass.name(), serialized);
            expr = pass.optimize_expr_pre(expr, ctx)?;

            expr = match expr {
                Expr::Ident(_) => expr,
                Expr::Path(_) => expr,
                Expr::Value(_) => expr,
                Expr::Block(x) => self.optimize_block(x, ctx).map(Expr::Block)?,
                Expr::Cond(x) => self.optimize_cond(x, ctx)?,
                Expr::Invoke(x) => self.optimize_invoke(x, ctx)?,
                Expr::BinOpKind(_) => expr,
                Expr::Any(x) => Expr::Any(x.clone()),
                _ => bail!("Could not optimize {:?}", expr),
            };
            expr = pass.optimize_expr_post(expr, ctx)?;
            let serialized2 = self.serializer.serialize_expr(&expr)?;
            if serialized != serialized2 {
                debug!(
                    "Done pass {} for {} => {}",
                    pass.name(),
                    serialized,
                    serialized2
                );
            }
        }
        Ok(expr)
    }

    pub fn optimize_import(&self, import: Import, _ctx: &ExecutionContext) -> Result<Import> {
        Ok(import)
    }

    pub fn optimize_module(&self, m: Module, ctx: &ExecutionContext) -> Result<Module> {
        m.items.iter().try_for_each(|x| self.prescan_item(x, ctx))?;
        let items: Vec<_> = m
            .items
            .into_iter()
            .map(|x| self.optimize_item(x, ctx))
            .try_collect()?;
        let items: Vec<_> = items.into_iter().flatten().collect();

        Ok(Module {
            name: m.name.clone(),
            items,
        })
    }
    fn prescan_item(&self, item: &Item, ctx: &ExecutionContext) -> Result<()> {
        match item {
            Item::Def(x) => self.prescan_def(x, ctx),
            _ => Ok(()),
        }
    }
    pub fn optimize_item(&self, mut item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        for pass in &self.passes {
            let serialized = self.serializer.serialize_item(&item)?;
            // debug!("Doing pass {} for {}", pass.name(), serialized);
            item = match pass.optimize_item_pre(item, ctx)? {
                Some(new_item) => new_item,
                None => {
                    return Ok(None);
                }
            };
            item = match item {
                Item::Def(x) => self.optimize_def(x, ctx).map(Item::Def)?,
                Item::Import(x) => self.optimize_import(x, ctx).map(Item::Import)?,
                Item::Module(x) => self.optimize_module(x, ctx).map(Item::Module)?,
                _ => item,
            };
            item = match pass.optimize_item_post(item, ctx)? {
                Some(new_item) => new_item,
                None => {
                    return Ok(None);
                }
            };
            let serialized2 = self.serializer.serialize_item(&item)?;
            if serialized != serialized2 {
                debug!(
                    "Done pass {} for {} => {}",
                    pass.name(),
                    serialized,
                    serialized2
                );
            }
        }
        Ok(Some(item))
    }

    pub fn optimize_let(&self, let_: Let, ctx: &ExecutionContext) -> Result<Let> {
        let value = self.optimize_expr(let_.value, ctx)?;
        ctx.insert_expr(let_.name.clone().into(), value.clone());

        Ok(Let {
            name: let_.name.clone(),
            value,
            ty: let_.ty,
        })
    }
    pub fn optimize_stmt(
        &self,
        stmt: Statement,
        ctx: &ExecutionContext,
    ) -> Result<Option<Statement>> {
        match stmt {
            Statement::Expr(x) => {
                let expr = self.optimize_expr(x, ctx)?;
                Ok(Some(Statement::Expr(expr)))
            }
            Statement::Item(x) => {
                if let Some(x) = self.optimize_item(*x, ctx)? {
                    Ok(Some(Statement::Item(x.into())))
                } else {
                    Ok(None)
                }
            }
            Statement::Any(x) => Ok(Some(Statement::Any(x.clone()))),
            Statement::Let(x) => self.optimize_let(x, ctx).map(Statement::Let).map(Some),
            Statement::StmtExpr(x) => {
                let expr = self.optimize_expr(x.expr, ctx)?;

                if matches!(&expr, Expr::Value(Value::Unit(_))) {
                    Ok(None)
                } else {
                    Ok(Some(Statement::stmt_expr(expr)))
                }
            }
        }
    }

    fn prescan_stmt(&self, stmt: &Statement, ctx: &ExecutionContext) -> Result<()> {
        match stmt {
            Statement::Item(x) => self.prescan_item(&**x, ctx),
            _ => Ok(()),
        }
    }
    pub fn optimize_block(&self, b: Block, ctx: &ExecutionContext) -> Result<Block> {
        let ctx = ctx.child();
        b.stmts
            .iter()
            .try_for_each(|x| self.prescan_stmt(x, &ctx))?;
        let items: Vec<_> = b
            .stmts
            .into_iter()
            .map(|x| self.optimize_stmt(x, &ctx))
            .try_collect()?;
        let stmts: Vec<_> = items.into_iter().flatten().collect();
        Ok(Block { stmts })
    }
    pub fn optimize_cond(&self, b: Cond, ctx: &ExecutionContext) -> Result<Expr> {
        let cases: Vec<_> = b
            .cases
            .into_iter()
            .map(|x| {
                let cond = self.optimize_expr(x.cond, ctx)?;
                let body = self.optimize_expr(x.body, ctx)?;
                Ok::<_, Error>(CondCase { cond, body })
            })
            .try_collect()?;
        Ok(Expr::Cond(Cond {
            cases,
            if_style: b.if_style,
        }))
    }
    pub fn optimize_func(
        &self,
        func: FunctionValue,
        ctx: &ExecutionContext,
    ) -> Result<FunctionValue> {
        let body = self.optimize_expr(*func.body, ctx)?;
        Ok(FunctionValue {
            body: body.into(),
            ..func
        })
    }

    pub fn optimize_def(&self, mut def: Define, ctx: &ExecutionContext) -> Result<Define> {
        def.value = match def.value {
            DefValue::Function(func) => self.optimize_func(func, ctx).map(DefValue::Function)?,
            _ => def.value,
        };
        Ok(def)
    }
    pub fn prescan_def(&self, def: &Define, ctx: &ExecutionContext) -> Result<()> {
        let def = def.clone();
        match def.value.clone() {
            DefValue::Function(f) => match def.name.as_str() {
                _ => {
                    debug!(
                        "Registering function {} => {}",
                        def.name,
                        self.serializer.serialize_expr(&f.body)?
                    );

                    ctx.insert_func_decl(def.name.clone(), f.clone());
                    Ok(())
                }
            },
            _ => Ok(()),
        }
    }
    pub fn optimize_tree(&self, node: Tree, ctx: &ExecutionContext) -> Result<Option<Tree>> {
        match node {
            Tree::Item(item) => {
                let item = self.optimize_item(item, ctx)?;
                Ok(item.map(Tree::Item))
            }
        }
    }
}
