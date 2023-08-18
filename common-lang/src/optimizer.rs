use crate::ast::*;
use crate::context::ScopedContext;
use crate::passes::*;
use crate::value::*;
use crate::*;
use common::*;

pub fn load_optimizer(serializer: Rc<dyn Serializer>) -> FoldOptimizer {
    let passes: MultiplePass = vec![
        Box::new(SpecializePass::new(serializer.clone())),
        Box::new(InlinePass::new(serializer.clone())),
    ];

    FoldOptimizer::new(serializer.clone(), passes)
}

pub struct FoldOptimizer {
    serializer: Rc<dyn Serializer>,
    pass: Box<dyn OptimizePass>,
}
impl FoldOptimizer {
    pub fn new(serializer: Rc<dyn Serializer>, pass: impl OptimizePass + 'static) -> Self {
        Self {
            serializer,
            pass: Box::new(pass),
        }
    }

    pub fn optimize_invoke(&self, mut invoke: Invoke, ctx: &ScopedContext) -> Result<Expr> {
        let func = self.optimize_expr(*invoke.func.clone(), ctx)?;

        invoke.args = invoke
            .args
            .clone()
            .into_iter()
            .map(|x| self.optimize_expr(x, ctx))
            .try_collect()?;

        if let Some(looked_up) = ctx.try_get_value_from_expr(&func) {
            match looked_up {
                Value::Function(f) => {
                    let sub_ctx = ctx.child();
                    for (i, arg) in invoke.args.iter().cloned().enumerate() {
                        let param = f
                            .params
                            .get(i)
                            .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;

                        sub_ctx.insert_expr(param.name.clone(), arg);
                    }

                    let ret = self.optimize_invoking(invoke, f, &sub_ctx)?;

                    return Ok(ret);
                }

                _ => {
                    warn!(
                        "Failed to optimize further {}",
                        self.serializer.serialize_expr(&func)?
                    )
                }
            }
        }

        let invoke = Expr::Invoke(invoke);
        info!(
            "Couldn't optimize {}",
            self.serializer.serialize_expr(&invoke)?
        );
        Ok(invoke)
    }

    pub fn optimize_invoking(
        &self,
        mut invoke: Invoke,
        mut func: FunctionValue,
        ctx: &ScopedContext,
    ) -> Result<Expr> {
        let serialized = self.serializer.serialize_invoke(&invoke)?;
        debug!("Doing passes {} for {}", self.pass.name(), serialized);

        invoke = self.pass.optimize_invoke_pre(invoke.clone(), &func, ctx)?;

        let func_body_serialized = self.serializer.serialize_expr(&func.body)?;
        debug!("Doing passes for {}", func_body_serialized);
        func.body = self.optimize_expr(*func.body, ctx)?.into();
        let func_body_serialized2 = self.serializer.serialize_expr(&func.body)?;
        // if func_body_serialized != func_body_serialized2 {
        debug!(
            "Done passes for {} => {}",
            func_body_serialized, func_body_serialized2
        );
        // }
        invoke = self.pass.optimize_invoke_post(invoke.clone(), &func, ctx)?;

        let serialized2 = self.serializer.serialize_invoke(&invoke)?;
        // if serialized != serialized2 {
        debug!("Done passes for {} => {}", serialized, serialized2);
        // }
        Ok(Expr::Invoke(invoke))
    }
    pub fn optimize_expr(&self, mut expr: Expr, ctx: &ScopedContext) -> Result<Expr> {
        let serialized = self.serializer.serialize_expr(&expr)?;
        debug!("Doing passes for {}", serialized);

        expr = self.pass.optimize_expr_pre(expr, ctx)?;

        expr = match expr {
            Expr::Pat(_) => expr,
            Expr::Value(_) => expr,
            Expr::Block(x) => self.optimize_block(x, ctx)?,
            Expr::Cond(x) => self.optimize_cond(x, ctx)?,
            Expr::Invoke(x) => self.optimize_invoke(x, ctx)?,
            Expr::Any(x) => Expr::Any(x.clone()),
            _ => bail!("Could not optimize {:?}", expr),
        };
        expr = self.pass.optimize_expr_post(expr, ctx)?;

        let serialized2 = self.serializer.serialize_expr(&expr)?;
        // if serialized != serialized2 {
        debug!("Done passes for {} => {}", serialized, serialized2);
        // }
        Ok(expr)
    }

    pub fn optimize_import(&self, import: Import, _ctx: &ScopedContext) -> Result<Import> {
        Ok(import)
    }

    pub fn optimize_module(&self, m: Module, ctx: &ScopedContext) -> Result<Module> {
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
    fn prescan_item(&self, item: &Item, ctx: &ScopedContext) -> Result<()> {
        match item {
            Item::Def(x) => self.prescan_def(x, ctx),
            _ => Ok(()),
        }
    }
    pub fn optimize_item(&self, mut item: Item, ctx: &ScopedContext) -> Result<Option<Item>> {
        let serialized = self.serializer.serialize_item(&item)?;
        debug!("Doing passes for {}", serialized);

        item = match self.pass.optimize_item_pre(item, ctx)? {
            Some(new_item) => new_item,
            None => {
                return Ok(None);
            }
        };

        item = match item {
            Item::Def(x) => self.optimize_def(x, ctx).map(Item::Def)?,
            Item::Import(x) => self.optimize_import(x, ctx).map(Item::Import)?,
            Item::Module(x) => self.optimize_module(x, ctx).map(Item::Module)?,
            Item::Expr(x) => {
                let expr = self.optimize_expr(x, ctx)?;
                Item::Expr(expr)
            }
            _ => item,
        };

        item = match self.pass.optimize_item_post(item, ctx)? {
            Some(new_item) => new_item,
            None => {
                return Ok(None);
            }
        };

        let serialized2 = self.serializer.serialize_item(&item)?;
        // if serialized != serialized2 {
        debug!("Done passes for {} => {}", serialized, serialized2);
        // }
        Ok(Some(item))
    }

    pub fn optimize_let(&self, let_: Let, ctx: &ScopedContext) -> Result<Let> {
        let value = self.optimize_expr(let_.value, ctx)?;
        ctx.insert_expr(let_.name.clone(), value.clone());

        Ok(Let {
            name: let_.name.clone(),
            value,
            ty: let_.ty,
        })
    }
    pub fn optimize_stmt(&self, stmt: Statement, ctx: &ScopedContext) -> Result<Option<Statement>> {
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
            Statement::SideEffect(x) => {
                let expr = self.optimize_expr(x.expr, ctx)?;

                if matches!(&expr, Expr::Value(Value::Unit(_))) {
                    Ok(None)
                } else {
                    Ok(Some(Statement::stmt_expr(expr)))
                }
            }
        }
    }

    fn prescan_stmt(&self, stmt: &Statement, ctx: &ScopedContext) -> Result<()> {
        match stmt {
            Statement::Item(x) => self.prescan_item(&**x, ctx),
            _ => Ok(()),
        }
    }
    pub fn optimize_block(&self, mut b: Block, ctx: &ScopedContext) -> Result<Expr> {
        let ctx = ctx.child();
        b.stmts
            .iter()
            .try_for_each(|x| self.prescan_stmt(x, &ctx))?;
        let stmts: Vec<_> = b
            .stmts
            .into_iter()
            .map(|x| self.optimize_stmt(x, &ctx))
            .try_collect()?;
        b.stmts = stmts.into_iter().flatten().collect();

        Ok(Expr::block(b))
    }
    pub fn optimize_cond(&self, b: Cond, ctx: &ScopedContext) -> Result<Expr> {
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
    pub fn optimize_func(&self, func: FunctionValue, ctx: &ScopedContext) -> Result<FunctionValue> {
        let body = self.optimize_expr(*func.body, ctx)?;
        Ok(FunctionValue {
            body: body.into(),
            ..func
        })
    }

    pub fn optimize_def(&self, mut def: Define, ctx: &ScopedContext) -> Result<Define> {
        def.value = match def.value {
            DefValue::Function(func) => self.optimize_func(func, ctx).map(DefValue::Function)?,
            _ => def.value,
        };
        Ok(def)
    }
    pub fn prescan_def(&self, def: &Define, ctx: &ScopedContext) -> Result<()> {
        let def = def.clone();
        match def.value.clone() {
            DefValue::Function(f) => match def.name.as_str() {
                _ => {
                    debug!(
                        "Registering function {}",
                        self.serializer.serialize_function(&f)?,
                    );

                    ctx.insert_function(def.name.clone(), f.clone());
                    Ok(())
                }
            },
            _ => Ok(()),
        }
    }
    pub fn optimize_tree(&self, node: Tree, ctx: &ScopedContext) -> Result<Option<Tree>> {
        match node {
            Tree::Item(item) => {
                let item = self.optimize_item(item, ctx)?;
                Ok(item.map(Tree::Item))
            }
            Tree::Expr(expr) => {
                let expr = self.optimize_expr(expr, ctx)?;
                Ok(Some(Tree::Expr(expr)))
            }
            Tree::File(file) => {
                let module = self.optimize_module(file.module, ctx)?;
                Ok(Some(Tree::File(File {
                    path: file.path,
                    module,
                })))
            }
        }
    }
}
