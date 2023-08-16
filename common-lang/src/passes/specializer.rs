use crate::context::ExecutionContext;
use crate::interpreter::Interpreter;
use crate::ops::BinOpKind;
use crate::passes::OptimizePass;
use crate::tree::*;
use crate::type_system::TypeSystem;
use crate::value::*;
use crate::*;
use common::*;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct SpecializePass {
    spec_id: AtomicUsize,
    serializer: Rc<dyn Serializer>,
    interpreter: Interpreter,
    type_system: TypeSystem,
}
impl SpecializePass {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
            interpreter: Interpreter::new(serializer.clone()),
            type_system: TypeSystem::new(serializer.clone()),
            serializer,
        }
    }
    pub fn get_expr_by_ident(&self, ident: Ident, ctx: &ExecutionContext) -> Result<Expr> {
        return match ident.as_str() {
            "+" | "-" | "*" | "<" | ">" | "<=" | ">=" | "==" | "!=" => Ok(Expr::Ident(ident)),
            "print" => Ok(Expr::Ident(ident)),
            _ => ctx
                .get_expr(&ident)
                .with_context(|| format!("Could not find {:?} in context", ident)),
        };
    }
    pub fn lookup_func_decl(
        &self,
        expr: &Expr,
        ctx: &ExecutionContext,
    ) -> Result<Option<FunctionValue>> {
        // debug!(
        //     "Lookup for func decl: {}",
        //     self.serializer.serialize_expr(expr)?
        // );

        Ok(match expr {
            Expr::Ident(n) => ctx.get_func_decl(n),
            Expr::Path(n) => ctx.get_func_decl(n),
            _ => None,
        })
    }

    pub fn specialize_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
        match expr {
            Expr::Ident(ref n) => match n.as_str() {
                "print" => Ok(expr),
                _ => {
                    let value = ctx
                        .get_expr(n)
                        .with_context(|| format!("Could not find {:?} in context", n))?;
                    // trace!(
                    //     "Look up {} => {}",
                    //     n,
                    //     self.serializer.serialize_expr(&value)?
                    // );
                    Ok(value)
                }
            },
            Expr::Path(n) => {
                let value = ctx
                    .get_expr(n.clone())
                    .with_context(|| format!("Could not find {:?} in context", n))?;
                debug!(
                    "Look up {} => {}",
                    n,
                    self.serializer.serialize_expr(&value)?
                );
                Ok(value)
            }
            Expr::Block(x) => self.specialize_block(x, ctx).map(Expr::Block),
            Expr::Invoke(x) => self.specialize_invoke(x, ctx),
            Expr::Cond(x) => self.specialize_cond(x, ctx),
            _ => Ok(expr),
        }
    }

    pub fn specialize_import(&self, import: Import, _ctx: &ExecutionContext) -> Result<Import> {
        Ok(import)
    }

    pub fn specialize_invoke_details(
        &self,
        func: FunctionValue,
        params: Vec<Expr>,
        ctx: &ExecutionContext,
    ) -> Result<Expr> {
        let name = func.name.as_ref().map(|x| x.name.as_str()).unwrap_or("fun");
        let args: Vec<_> = params
            .into_iter()
            .map(|x| self.specialize_expr(x, ctx))
            .try_collect()?;
        // debug!(
        //     "Specializing Invoke {} with {}",
        //     name,
        //     self.serializer.serialize_args(&args)?
        // );

        let sub = ctx.child();
        for (i, arg) in args.iter().cloned().enumerate() {
            let param = func
                .params
                .get(i)
                .with_context(|| format!("Couldn't find {} parameter of {}", i, name))?;
            // TODO: type check here
            sub.insert_expr(param.name.clone().into(), arg);
        }
        match name {
            "print" => {
                return Ok(Expr::Invoke(Invoke {
                    fun: Expr::Ident(func.name.clone().unwrap()).into(),
                    args,
                }))
            }
            _ => {}
        }

        let new_body = self.specialize_expr(*func.body, &sub)?;
        let new_name = Ident::new(format!(
            "{}_{}",
            name,
            self.spec_id.fetch_add(1, Ordering::Relaxed)
        ));
        trace!(
            "Specialized function {} with {} => {} {}",
            name,
            self.serializer.serialize_args(&args)?,
            new_name,
            self.serializer.serialize_expr(&new_body)?
        );
        let mut ret = func.ret.clone();
        match &ret {
            TypeValue::Expr(expr) => match &**expr {
                TypeExpr::Ident(ident)
                    if func
                        .generics_params
                        .iter()
                        .find(|x| &x.name == ident)
                        .is_some() =>
                {
                    ret = self.type_system.infer_expr(&new_body, &sub)?.into();
                }
                _ => {}
            },
            _ => {}
        }

        ctx.root().insert_specialized(
            new_name.clone().into(),
            FunctionValue {
                name: Some(new_name.clone()),
                params: Default::default(),
                generics_params: vec![],
                ret,
                body: new_body.into(),
            },
        );
        return Ok(Expr::Invoke(Invoke {
            fun: Expr::Ident(new_name).into(),
            args: Default::default(),
        }));
    }
    pub fn specialize_invoke_bin_op(
        &self,
        kind: BinOpKind,
        args: Vec<Expr>,
        ctx: &ExecutionContext,
    ) -> Result<Expr> {
        let args: Vec<_> = args
            .into_iter()
            .map(|x| self.specialize_expr(x, ctx))
            .try_collect()?;
        debug!(
            "Specializing Invoke {:?} with {}",
            kind,
            self.serializer.serialize_args(&args)?
        );

        let result_invoke = Invoke {
            fun: Expr::BinOpKind(kind.clone()).into(),
            args: args.clone(),
        };
        match self.interpreter.interpret_invoke(&result_invoke, ctx) {
            Ok(val) => Ok(Expr::value(val)),
            Err(err) => {
                warn!(
                    "Failed to interpret {:?} {}: {:?}",
                    kind,
                    self.serializer.serialize_args(&args)?,
                    err
                );
                Ok(Expr::Invoke(result_invoke))
            }
        }
    }
    pub fn specialize_invoke(&self, node: Invoke, ctx: &ExecutionContext) -> Result<Expr> {
        if let Some(fun) = self.lookup_func_decl(&node.fun, ctx)? {
            self.specialize_invoke_details(fun, node.args, ctx)
        } else {
            let fun = self.specialize_expr(*node.fun, ctx)?;

            match fun {
                Expr::Value(Value::Function(f)) => {
                    self.specialize_invoke_details(f, node.args, ctx)
                }
                Expr::BinOpKind(op) => self.specialize_invoke_bin_op(op, node.args, ctx),
                _ => bail!("Failed to specialize {:?}", fun),
            }
        }
    }
    pub fn specialize_module(&self, mut module: Module, ctx: &ExecutionContext) -> Result<Module> {
        debug!(
            "Specializing module {}",
            self.serializer.serialize_module(&module)?
        );

        for specialized_name in ctx.list_specialized().into_iter().sorted() {
            let func = ctx.get_func_decl(specialized_name).unwrap();
            let define = Define {
                name: func.name.clone().expect("No specialized name"),
                kind: DefKind::Function,
                ty: Some(TypeValue::Function(
                    self.type_system.infer_function(&func, ctx)?,
                )),
                value: DefValue::Function(func),
                visibility: Visibility::Public,
            };
            module.items.push(Item::Def(define));
        }

        Ok(module)
    }
    pub fn specialize_item(&self, item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        match item {
            Item::Def(x) => {
                return if let Some(x) = self.specialize_def(x, ctx)? {
                    Ok(Some(Item::Def(x)))
                } else {
                    Ok(None)
                }
            }
            Item::Import(x) => self.specialize_import(x, ctx).map(Item::Import),
            Item::Module(x) => self.specialize_module(x, ctx).map(Item::Module),
            Item::Any(x) => Ok(Item::Any(x.clone())),
            Item::Impl(x) => Ok(Item::Impl(x.clone())),
        }
        .map(Some)
    }

    pub fn specialize_let(&self, let_: Let, ctx: &ExecutionContext) -> Result<Let> {
        let value = self.specialize_expr(let_.value, ctx)?;
        ctx.insert_expr(let_.name.clone().into(), value.clone());
        let ty = self.type_system.infer_expr(&value, ctx)?;
        Ok(Let {
            name: let_.name.clone(),
            value,
            ty: Some(ty),
        })
    }
    pub fn specialize_stmt_inner(
        &self,
        stmt: Statement,
        ctx: &ExecutionContext,
    ) -> Result<Option<Statement>> {
        match stmt {
            Statement::Expr(x) => {
                let expr = self.specialize_expr(x, ctx)?;
                Ok(Some(Statement::Expr(expr)))
            }
            Statement::Item(x) => {
                if let Some(x) = self.specialize_item(*x, ctx)? {
                    Ok(Some(Statement::Item(x.into())))
                } else {
                    Ok(None)
                }
            }
            Statement::Any(x) => Ok(Some(Statement::Any(x.clone()))),
            Statement::Let(x) => self.specialize_let(x, ctx).map(Statement::Let).map(Some),
            Statement::StmtExpr(x) => {
                let expr = self.specialize_expr(x.expr, ctx)?;

                if matches!(&expr, Expr::Value(Value::Unit(_))) {
                    Ok(None)
                } else {
                    Ok(Some(Statement::stmt_expr(expr)))
                }
            }
        }
    }
    pub fn specialize_stmt(
        &self,
        stmt: Statement,
        ctx: &ExecutionContext,
    ) -> Result<Option<Statement>> {
        let stmt_serialized = self.serializer.serialize_stmt(&stmt)?;
        // debug!("Specializing stmt {}", stmt_serialized);
        let specialized = self.specialize_stmt_inner(stmt, ctx);
        match specialized {
            Ok(Some(specialized)) => {
                debug!(
                    "Specialized {} => {}",
                    stmt_serialized,
                    self.serializer.serialize_stmt(&specialized)?
                );
                Ok(Some(specialized))
            }
            Ok(None) => {
                debug!("Specialized {} => None", stmt_serialized);
                Ok(None)
            }
            Err(err) => {
                warn!("Failed to specialize {}: {}", stmt_serialized, err);
                Err(err)
            }
        }
    }
    pub fn specialize_block(&self, b: Block, ctx: &ExecutionContext) -> Result<Block> {
        let ctx = ctx.child();
        let items: Vec<_> = b
            .stmts
            .into_iter()
            .map(|x| self.specialize_stmt(x, &ctx))
            .try_collect()?;
        let stmts: Vec<_> = items.into_iter().flatten().collect();
        Ok(Block { stmts })
    }
    pub fn specialize_cond(&self, b: Cond, ctx: &ExecutionContext) -> Result<Expr> {
        for case in &b.cases {
            let interpreted =
                Interpreter::new(self.serializer.clone()).interpret_expr(&case.cond, ctx)?;
            match interpreted {
                Value::Bool(b) => {
                    if b.value {
                        return self.specialize_expr(case.body.clone(), ctx);
                    } else {
                        continue;
                    }
                }
                _ => break,
            }
        }
        Ok(Expr::Cond(Cond {
            cases: b
                .cases
                .into_iter()
                .map(|case| {
                    Ok::<_, Error>(CondCase {
                        cond: self.specialize_expr(case.cond, ctx)?,
                        body: self.specialize_expr(case.body, ctx)?,
                    })
                })
                .try_collect()?,
            if_style: b.if_style,
        }))
    }

    pub fn specialize_def(
        &self,
        mut def: Define,
        ctx: &ExecutionContext,
    ) -> Result<Option<Define>> {
        match def.value {
            DefValue::Function(f) if f.params.is_empty() && f.generics_params.is_empty() => {
                debug!(
                    "Specializing function {} => {}",
                    def.name,
                    self.serializer.serialize_expr(&f.body)?
                );
                let body = self.specialize_expr(*f.body, ctx)?;
                def.value = DefValue::Function(FunctionValue {
                    body: body.into(),
                    name: f.name,
                    params: f.params,
                    generics_params: f.generics_params,
                    ret: f.ret,
                });

                Ok(Some(def))
            }
            DefValue::Function(_) => match def.name.as_str() {
                "print" => Ok(Some(def)),
                _ => Ok(None),
            },

            _ => Ok(Some(def)),
        }
    }
}
impl OptimizePass for SpecializePass {
    fn name(&self) -> &str {
        "specialize"
    }

    fn optimize_item_post(&self, item: Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        self.specialize_item(item, ctx)
    }
    // fn optimize_expr(&self, expr: Expr, ctx: &ExecutionContext) -> Result<Expr> {
    //     self.specialize_expr(expr, ctx)
    // }
}
