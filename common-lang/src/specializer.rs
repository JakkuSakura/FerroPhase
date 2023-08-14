use crate::context::ExecutionContext;
use crate::interpreter::Interpreter;
use crate::tree::*;
use crate::type_system::TypeSystem;
use crate::value::*;
use crate::*;
use common::*;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Specializer {
    spec_id: AtomicUsize,
    serializer: Rc<dyn Serializer>,
    type_system: TypeSystem,
}
impl Specializer {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
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
        debug!(
            "Lookup for func decl: {}",
            self.serializer.serialize_expr(expr)?
        );

        Ok(match expr {
            Expr::Ident(n) => ctx.get_func_decl(n),
            Expr::Path(n) => ctx.get_func_decl(n),
            _ => None,
        })
    }
    pub fn specialize_expr_inner(&self, expr: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        match expr {
            Expr::Ident(n) => match n.as_str() {
                "+" | "-" | "*" | "<" | ">" | "<=" | ">=" | "==" | "!=" => Ok(expr.clone()),
                "print" => Ok(expr.clone()),
                _ => {
                    let value = ctx
                        .get_expr(n)
                        .with_context(|| format!("Could not find {:?} in context", n))?;
                    debug!(
                        "Look up {} => {}",
                        n,
                        self.serializer.serialize_expr(&value)?
                    );
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
            Expr::Value(_) => Ok(expr.clone()),
            Expr::Block(x) => self.specialize_block(x, ctx).map(Expr::Block),
            Expr::Cond(x) => self.specialize_cond(x, ctx),
            Expr::Invoke(x) => self.specialize_invoke(x, ctx),
            Expr::BinOpKind(_) => Ok(expr.clone()),
            Expr::Any(x) => Ok(Expr::Any(x.clone())),
            _ => bail!("Could not specialize {:?}", expr),
        }
    }
    pub fn specialize_expr(&self, expr: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Specializing {}", self.serializer.serialize_expr(expr)?);
        let specialized = self.specialize_expr_inner(expr, ctx);
        match specialized {
            Ok(specialized) => {
                debug!(
                    "Specialized {} => {}",
                    self.serializer.serialize_expr(expr)?,
                    self.serializer.serialize_expr(&specialized)?
                );
                Ok(specialized)
            }
            Err(err) => {
                warn!(
                    "Failed to specialize {}: {}",
                    self.serializer.serialize_expr(expr)?,
                    err
                );
                Err(err)
            }
        }
    }
    pub fn specialize_import(&self, import: &Import, _ctx: &ExecutionContext) -> Result<Import> {
        Ok(import.clone())
    }
    pub fn specialize_invoke_details(
        &self,
        func: &FunctionValue,
        params: &[Expr],
        ctx: &ExecutionContext,
    ) -> Result<Expr> {
        let name = func.name.as_ref().map(|x| x.name.as_str()).unwrap_or("fun");
        let args: Vec<_> = params
            .iter()
            .map(|x| self.specialize_expr(x, ctx))
            .try_collect()?;
        debug!(
            "Specializing Invoke {} with {}",
            name,
            self.serializer.serialize_exprs(&args)?
        );

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
        let new_body = self.specialize_expr(&func.body, &sub)?;
        let new_name = Ident::new(format!(
            "{}_{}",
            name,
            self.spec_id.fetch_add(1, Ordering::Relaxed)
        ));
        debug!(
            "Specialized function {} with {} => {} {}",
            name,
            self.serializer.serialize_exprs(&args)?,
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
    pub fn specialize_invoke(&self, node: &Invoke, ctx: &ExecutionContext) -> Result<Expr> {
        if let Some(fun) = self.lookup_func_decl(&node.fun, ctx)? {
            self.specialize_invoke_details(&fun, &node.args, ctx)
        } else {
            let fun = self.specialize_expr(&node.fun, ctx)?;

            match fun {
                Expr::Value(Value::Function(f)) => {
                    self.specialize_invoke_details(&f, &node.args, ctx)
                }
                Expr::BinOpKind(op) => {
                    let args: Vec<_> = node
                        .args
                        .iter()
                        .map(|x| self.specialize_expr(x, ctx))
                        .try_collect()?;
                    debug!(
                        "Invoking {:?} with {}",
                        op,
                        self.serializer.serialize_exprs(&args)?
                    );

                    let result_invoke = Invoke {
                        fun: Expr::BinOpKind(op).into(),
                        args,
                    };
                    if let Ok(val) = Interpreter::new(self.serializer.clone())
                        .interpret_invoke(&result_invoke, ctx)
                    {
                        Ok(Expr::value(val))
                    } else {
                        Ok(Expr::Invoke(result_invoke))
                    }
                }
                _ => bail!("Failed to specialize {:?} {:?}", node, fun),
            }
        }
    }
    pub fn specialize_module(&self, m: &Module, ctx: &ExecutionContext) -> Result<Module> {
        debug!(
            "Specializing module {}",
            self.serializer.serialize_module(m)?
        );

        let items: Vec<_> = m
            .items
            .iter()
            .map(|x| self.specialize_item(x, ctx))
            .try_collect()?;
        let mut items: Vec<_> = items.into_iter().flatten().collect();
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
            items.push(Item::Def(define));
        }

        Ok(Module {
            name: m.name.clone(),
            items,
        })
    }
    pub fn specialize_item_inner(
        &self,
        item: &Item,
        ctx: &ExecutionContext,
    ) -> Result<Option<Item>> {
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
            Item::Expr(x) => self.specialize_expr(x, ctx).map(Item::Expr),
            Item::Impl(x) => Ok(Item::Impl(x.clone())),
            Item::Stmt(x) => self.specialize_expr(x, ctx).map(Item::Stmt),
        }
        .map(Some)
    }
    pub fn specialize_item(&self, item: &Item, ctx: &ExecutionContext) -> Result<Option<Item>> {
        debug!(
            "Specializing item {}",
            self.serializer.serialize_item(item)?
        );
        let specialized = self.specialize_item_inner(item, ctx);
        match specialized {
            Ok(Some(specialized)) => {
                debug!(
                    "Specialized {} => {}",
                    self.serializer.serialize_item(item)?,
                    self.serializer.serialize_item(&specialized)?
                );
                Ok(Some(specialized))
            }
            Ok(None) => {
                debug!(
                    "Specialized {} => None",
                    self.serializer.serialize_item(item)?
                );
                Ok(None)
            }
            Err(err) => {
                warn!(
                    "Failed to specialize {}: {}",
                    self.serializer.serialize_item(item)?,
                    err
                );
                Err(err)
            }
        }
    }
    pub fn specialize_block(&self, b: &Block, ctx: &ExecutionContext) -> Result<Block> {
        let items: Vec<_> = b
            .stmts
            .iter()
            .map(|x| self.specialize_item(x, ctx))
            .try_collect()?;
        let items: Vec<_> = items.into_iter().flatten().collect();
        Ok(Block { stmts: items })
    }
    pub fn specialize_cond(&self, b: &Cond, ctx: &ExecutionContext) -> Result<Expr> {
        for case in &b.cases {
            let interpreted =
                Interpreter::new(self.serializer.clone()).interpret_expr(&case.cond, ctx)?;
            match interpreted {
                Value::Bool(b) => {
                    if b.value {
                        return self.specialize_expr(&case.body, ctx);
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
                .iter()
                .map(|case| {
                    Ok::<_, Error>(CondCase {
                        cond: self.specialize_expr(&case.cond, ctx)?,
                        body: self.specialize_expr(&case.body, ctx)?,
                    })
                })
                .try_collect()?,
            if_style: b.if_style,
        }))
    }

    pub fn specialize_def(&self, def: &Define, ctx: &ExecutionContext) -> Result<Option<Define>> {
        let def = def.clone();
        match def.value.clone() {
            DefValue::Function(f) => match def.name.as_str() {
                "main" => {
                    debug!(
                        "Specializing main function {} => {}",
                        def.name,
                        self.serializer.serialize_expr(&f.body)?
                    );
                    let define = Define {
                        value: DefValue::Function(FunctionValue {
                            body: self.specialize_expr(&f.body, ctx)?.into(),
                            ..f
                        }),
                        ..def
                    };
                    Ok(Some(define))
                }

                _ => {
                    debug!(
                        "Registering function {} => {}",
                        def.name,
                        self.serializer.serialize_expr(&f.body)?
                    );

                    ctx.insert_func_decl(def.name.clone(), f.clone());
                    match def.name.as_str() {
                        "print" => Ok(Some(def)),
                        _ => Ok(None),
                    }
                }
            },
            _ => Ok(Some(def.clone())),
        }
    }
    pub fn specialize_tree(&self, node: &Tree, ctx: &ExecutionContext) -> Result<Option<Tree>> {
        match node {
            Tree::Item(item) => {
                let item = self.specialize_item(item, ctx)?;
                Ok(item.map(Tree::Item))
            }
        }
    }
}
