use crate::context::ExecutionContext;
use crate::interpreter::Interpreter;
use crate::tree::*;
use crate::value::Value;
use crate::*;
use common::*;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Specializer {
    spec_id: AtomicUsize,
    serializer: Rc<dyn Serializer>,
}
impl Specializer {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self {
            spec_id: AtomicUsize::default(),
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
    pub fn specialize_expr(&self, expr: &Expr, ctx: &ExecutionContext) -> Result<Expr> {
        debug!("Specializing {}", self.serializer.serialize_expr(expr)?);

        match expr {
            Expr::Ident(n) => match n.as_str() {
                "+" | "-" | "*" | "<" | ">" | "<=" | ">=" | "==" | "!=" => Ok(expr.clone()),
                "print" => Ok(expr.clone()),
                _ => ctx
                    .get_expr(n)
                    .with_context(|| format!("Could not find {:?} in context", n.name)),
            },
            Expr::Path(_) => Ok(expr.clone()),
            Expr::Value(_) => Ok(expr.clone()),
            Expr::Block(x) => self.specialize_block(x, ctx).map(Expr::Block),
            Expr::Cond(x) => self.specialize_cond(x, ctx),
            Expr::Invoke(x) => self.specialize_invoke(x, ctx),
            _ => bail!("Could not specialize {:?}", expr),
        }
    }
    pub fn specialize_import(&self, import: &Import, _ctx: &ExecutionContext) -> Result<Import> {
        Ok(import.clone())
    }
    pub fn specialize_invoke(&self, node: &InvokeExpr, ctx: &ExecutionContext) -> Result<Expr> {
        let fun = self.specialize_expr(&node.fun, ctx)?;
        let name = self.serializer.serialize_expr(&fun)?;
        let args: Vec<_> = node
            .args
            .iter()
            .map(|x| self.specialize_expr(x, ctx))
            .try_collect()?;
        match fun {
            Expr::Value(Value::Function(f)) => {
                let args_ = self.serializer.serialize_exprs(&args)?;
                debug!("Invoking {} with {}", name, args_);
                let sub = ctx.child();
                for (i, arg) in args.iter().cloned().enumerate() {
                    let param = f
                        .params
                        .get(i)
                        .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                    // TODO: type check here
                    sub.insert_expr(param.name.clone(), arg);
                }
                let new_body = self.specialize_block(&f.body, &sub)?;
                let bd = self.serializer.serialize_block(&new_body)?;
                let args_ = self.serializer.serialize_exprs(&args)?;
                debug!("Specialied {} with {} => {}", name, args_, bd);
                let new_name = Ident::new(format!(
                    "{}_{}",
                    name,
                    self.spec_id.fetch_add(1, Ordering::Relaxed)
                ));
                let ret = f.ret.clone();
                // if ret.as_ast::<Ident>() == Some(&Ident::new("T")) {
                //     ret = new_body
                //         .stmts
                //         .last()
                //         .map(|x| self.infer_type(x, ctx))
                //         .unwrap_or(Ok(UnitValue.into()))?;
                // }
                ctx.root().insert_specialized(
                    new_name.clone(),
                    FuncDecl {
                        name: new_name.clone(),
                        params: Default::default(),
                        generics_params: vec![],
                        ret: TypeExpr::Value(ret),
                        body: new_body,
                    },
                );
                return Ok(Expr::Invoke(InvokeExpr {
                    fun: Expr::Ident(new_name).into(),
                    args: Default::default(),
                }));
            }
            _ => {}
        }

        bail!("Failed to specialize {:?}", node)
    }
    pub fn specialize_module(&self, m: &Module, ctx: &ExecutionContext) -> Result<Module> {
        let mut items: Vec<_> = m
            .items
            .iter()
            .map(|x| self.specialize_item(x, ctx))
            .try_collect()?;
        let specialized: Vec<_> = ctx
            .list_specialized()
            .into_iter()
            .sorted()
            .map(|name| {
                ctx.get_func_decl(&name)
                    .map(|def| {
                        Define {
                            name: def.name.clone(),
                            kind: DefKind::Function,
                            ty: None, // TODO: infer type
                            value: DefValue::Function(def),
                            visibility: Visibility::Public,
                        }
                    })
                    .unwrap()
            })
            .map(Item::Def)
            .collect();
        items.extend(specialized);
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
    pub fn specialize_def(&self, d: &Define, ctx: &ExecutionContext) -> Result<Define> {
        let fun = d.value.clone();
        match fun {
            DefValue::Function(f) => match f.name.as_str() {
                "main" => {
                    return Ok(Define {
                        name: d.name.clone(),
                        kind: DefKind::Function,
                        ty: None,
                        value: DefValue::Function(FuncDecl {
                            name: f.name,
                            params: f.params,
                            generics_params: vec![],
                            ret: f.ret,
                            body: self.specialize_block(&f.body, ctx)?,
                        }),
                        visibility: d.visibility,
                    });
                }
                _ => return Ok(d.clone()),
            },
            _ => return Ok(d.clone()),
        }
    }
}
