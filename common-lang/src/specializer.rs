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
                        .with_context(|| format!("Could not find {:?} in context", n));
                    debug!("Look up {} {:?}", n, value);
                    value
                }
            },
            Expr::Path(n) => {
                let value = ctx
                    .get_expr(n.clone())
                    .with_context(|| format!("Could not find {:?} in context", n));
                debug!("Look up {} {:?}", n, value);
                value
            }
            Expr::Value(_) => Ok(expr.clone()),
            Expr::Block(x) => self.specialize_block(x, ctx).map(Expr::Block),
            Expr::Cond(x) => self.specialize_cond(x, ctx),
            Expr::Invoke(x) => self.specialize_invoke(x, ctx),
            Expr::BinOpKind(_) => Ok(expr.clone()),
            Expr::Any(x) => Ok(Expr::Any(x.clone())),
            Expr::Stmt(x) => self.specialize_expr(x, ctx).map(|_| Expr::unit()),
            _ => bail!("Could not specialize {:?}", expr),
        }
    }
    pub fn specialize_expr(&self, expr: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Specializing {}", self.serializer.serialize_expr(expr)?);
        let specialized = self.specialize_expr_inner(expr, ctx)?;
        debug!(
            "Specialized {} => {}",
            self.serializer.serialize_expr(expr)?,
            self.serializer.serialize_expr(&specialized)?
        );
        Ok(specialized)
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
        let new_body = self.specialize_block(&func.body, &sub)?;
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
            self.serializer.serialize_block(&new_body)?
        );
        let ret = func.ret.clone();
        // TODO: resolve generics
        ctx.root().insert_specialized(
            new_name.clone().into(),
            FunctionValue {
                name: Some(new_name.clone()),
                params: Default::default(),
                generics_params: vec![],
                ret,
                body: new_body,
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
                    let func_decl = FunctionValue {
                        name: None,
                        params: f.params,
                        generics_params: vec![],
                        ret: f.ret,
                        body: f.body,
                    };
                    self.specialize_invoke_details(&func_decl, &node.args, ctx)
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
                        Ok(val)
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

        let mut items: Vec<_> = m
            .items
            .iter()
            .map(|x| self.specialize_item(x, ctx))
            .try_collect()?;
        for specialized_name in ctx.list_specialized().into_iter().sorted() {
            let func = ctx.get_func_decl(specialized_name).unwrap();
            let define = Define {
                name: func.name.clone().expect("No specialized name"),
                kind: DefKind::Function,
                ty: Some(TypeValue::Function(
                    self.type_system.infer_type_function(&func, ctx)?,
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
    pub fn specialize_item(&self, item: &Item, ctx: &ExecutionContext) -> Result<Item> {
        match item {
            Item::Import(x) => self.specialize_import(x, ctx).map(Item::Import),
            Item::Def(x) => self.specialize_def(x, ctx).map(Item::Def),
            Item::Module(x) => self.specialize_module(x, ctx).map(Item::Module),
            Item::Any(x) => Ok(Item::Any(x.clone())),
            Item::Expr(x) => self.specialize_expr(x, ctx).map(Item::Expr),
            Item::Impl(x) => Ok(Item::Impl(x.clone())),
        }
    }
    pub fn specialize_block(&self, b: &Block, ctx: &ExecutionContext) -> Result<Block> {
        Ok(Block {
            stmts: b
                .stmts
                .iter()
                .map(|x| self.specialize_item(x, ctx))
                .try_collect()?,
            last_value: b.last_value,
        })
    }
    pub fn specialize_cond(&self, b: &Cond, ctx: &ExecutionContext) -> Result<Expr> {
        for case in &b.cases {
            let interpreted =
                Interpreter::new(self.serializer.clone()).interpret_expr(&case.cond, ctx)?;
            match interpreted {
                Expr::Value(Value::Bool(b)) => {
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

    pub fn specialize_def(&self, def: &Define, ctx: &ExecutionContext) -> Result<Define> {
        match def.value.clone() {
            DefValue::Function(f) => match def.name.as_str() {
                "main" => Ok(Define {
                    name: def.name.clone(),
                    kind: DefKind::Function,
                    ty: None,
                    value: DefValue::Function(FunctionValue {
                        name: f.name,
                        params: f.params,
                        generics_params: vec![],
                        ret: f.ret,
                        body: self.specialize_block(&f.body, ctx)?,
                    }),
                    visibility: def.visibility,
                }),
                _ => {
                    debug!("Registering function {}", def.name);

                    ctx.insert_func_decl(def.name.clone(), f.clone());
                    Ok(def.clone())
                }
            },
            _ => Ok(def.clone()),
        }
    }
    pub fn specialize_tree(&self, node: &Tree, ctx: &ExecutionContext) -> Result<Tree> {
        match node {
            Tree::Item(item) => self.specialize_item(item, ctx).map(Tree::Item),
        }
    }
}
