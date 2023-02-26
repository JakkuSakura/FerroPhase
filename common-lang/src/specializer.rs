use crate::ast::*;
use crate::interpreter::{Interpreter, InterpreterContext};
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

    pub fn specialize_expr(&self, expr: Expr, ctx: &InterpreterContext) -> Result<Expr> {
        let expr = uplift_common_ast(&expr);
        if let Some(n) = expr.as_ast::<Uplifted>() {
            return self.specialize_expr(n.uplifted.clone(), ctx);
        }
        debug!("Specializing {}", self.serializer.serialize(&expr)?);
        macro specialize($f: ident, $t: ty) {
            if expr.is_ast::<$t>() {
                return self.$f(expr.into_ast().unwrap(), ctx).map(|x| x.into());
            }
        }
        specialize!(specialize_block, Block);
        specialize!(specialize_module, Module);

        if expr.is_literal() {
            return Ok(expr);
        }
        if expr.is_raw() {
            return Ok(expr);
        }
        specialize!(specialize_def, Def);
        specialize!(specialize_call, Call);

        if let Some(n) = expr.as_ast::<Ident>() {
            return match n.as_str() {
                "+" | "-" | "*" | "<" | ">" | "<=" | ">=" | "==" | "!=" => Ok(expr),
                "print" => Ok(expr),
                _ => ctx
                    .get(n)
                    .with_context(|| format!("Could not find {:?} in context", n.name)),
            };
        }
        specialize!(specialize_cond, Cond);

        bail!("Could not specialize {:?}", expr)
        // Ok(expr)
    }
    pub fn infer_type_call(
        &self,
        callee: &Expr,
        params: &[Expr],
        ctx: &InterpreterContext,
    ) -> Result<Expr> {
        let mut inner: Option<Expr> = None;
        if let Some(ident) = callee.as_ast::<Ident>() {
            match ident.as_str() {
                "+" | "-" | "*" => {
                    return self.infer_type(params.first().context("No param")?, ctx)
                }
                ">" | ">=" | "<" | "<=" | "==" | "!=" => {
                    return Ok(Types::bool().into());
                }
                "print" => return Ok(Unit.into()),
                _ => inner = ctx.get(ident),
            }
        };
        let inner = inner.with_context(|| format!("Could not find {:?} in context", callee))?;
        if let Some(fun) = inner.as_ast::<FuncDecl>() {
            // TODO: make sure fun.ret is a solid type
            return Ok(fun.ret.clone());
        }

        bail!("Could not infer type call {:?}", callee)
    }
    pub fn infer_type(&self, expr: &Expr, ctx: &InterpreterContext) -> Result<Expr> {
        if let Some(call) = expr.as_ast::<Call>() {
            return self.infer_type_call(&call.fun, &call.args.args, ctx);
        }
        if let Some(_) = expr.as_ast::<LiteralInt>() {
            return Ok(Types::i64().into());
        }
        if let Some(_) = expr.as_ast::<LiteralDecimal>() {
            return Ok(Types::f64().into());
        }
        bail!(
            "Could not infer type of {}",
            self.serializer.serialize(expr)?
        )
    }
    pub fn specialize_call(&self, node: Call, ctx: &InterpreterContext) -> Result<Expr> {
        let mut fun = self.specialize_expr(node.fun.clone(), ctx)?;
        if let Some(g) = fun.as_ast::<Generics>() {
            fun = g.value.clone();
        }
        let args = PosArgs {
            args: node
                .args
                .args
                .iter()
                .cloned()
                .map(|x| self.specialize_expr(x, ctx))
                .try_collect()?,
        };
        if let Some(f) = fun.as_ast::<FuncDecl>() {
            let name = f.name.as_ref().map(|x| x.as_str()).unwrap_or("<fun>");
            let args_ = self.serializer.serialize(&args.clone().into())?;
            debug!("Invoking {} with {}", name, args_);
            let sub = ctx.child();
            for (i, arg) in args.args.iter().cloned().enumerate() {
                let param = f
                    .params
                    .params
                    .get(i)
                    .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                // TODO: type check here
                sub.insert(param.name.clone(), arg);
            }
            let new_body =
                self.specialize_block(f.body.clone().context("Funtion body is empty")?, &sub)?;
            let bd = self.serializer.serialize(&new_body.clone().into())?;
            let args_ = self.serializer.serialize(&args.clone().into())?;
            debug!("Specialied {} with {} => {}", name, args_, bd);
            let new_name = Ident::new(format!(
                "{}_{}",
                name,
                self.spec_id.fetch_add(1, Ordering::Relaxed)
            ));
            let mut ret = f.ret.clone();
            if ret.as_ast::<Ident>() == Some(&Ident::new("T")) {
                ret = new_body
                    .stmts
                    .last()
                    .map(|x| self.infer_type(x, ctx))
                    .unwrap_or(Ok(Unit.into()))?;
            }
            ctx.root().insert_specialized(
                new_name.clone(),
                FuncDecl {
                    name: Some(new_name.clone()),
                    params: Params::default(),
                    ret,
                    body: Some(new_body),
                }
                .into(),
            );
            return Ok(Call {
                fun: new_name.into(),
                args: PosArgs::default(),
            }
            .into());
        }
        if let Some(id) = fun.as_ast::<Ident>() {
            return Ok(Call {
                fun: id.clone().into(),
                args,
            }
            .into());
        }
        bail!("Failed to specialize {:?}", node)
    }
    pub fn specialize_module(&self, m: Module, ctx: &InterpreterContext) -> Result<Module> {
        let mut stmts: Vec<_> = m
            .stmts
            .into_iter()
            .map(|x| self.specialize_expr(x, ctx))
            .try_collect()?;
        let specialized: Vec<_> = ctx
            .list_specialized()
            .into_iter()
            .map(|name| {
                ctx.get(&name)
                    .map(|x| {
                        Def {
                            name: x.as_ast::<FuncDecl>().unwrap().name.clone().unwrap(),
                            ty: None,
                            value: x,
                            visibility: Visibility::Public,
                        }
                        .into()
                    })
                    .context("impossible")
            })
            .try_collect()?;
        stmts.extend(specialized);
        Ok(Module {
            name: m.name,
            stmts: stmts
                .into_iter()
                .filter(|x| x.as_ast::<Unit>().is_none())
                .collect(),
        })
    }
    pub fn specialize_block(&self, b: Block, ctx: &InterpreterContext) -> Result<Block> {
        Ok(Block {
            stmts: b
                .stmts
                .into_iter()
                .map(|x| self.specialize_expr(x, ctx))
                .try_collect()?,
            last_value: b.last_value,
        })
    }
    pub fn specialize_cond(&self, b: Cond, ctx: &InterpreterContext) -> Result<Expr> {
        for case in &b.cases {
            let interpreted =
                Interpreter::new(self.serializer.clone()).interprete_expr(&case.cond, ctx)?;
            let ret = interpreted.as_ast::<LiteralBool>().map(|x| x.value);
            match ret {
                Some(true) => {
                    return self.specialize_expr(case.body.clone(), ctx);
                }
                Some(false) => {
                    continue;
                }
                None => break,
            }
        }
        Ok(Cond {
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
        }
        .into())
    }
    pub fn specialize_def(&self, d: Def, ctx: &InterpreterContext) -> Result<Expr> {
        let fun;
        if let Some(g) = d.value.as_ast::<Generics>() {
            fun = g.value.clone();
        } else {
            fun = d.value.clone();
        }

        if let Some(f) = fun.as_ast::<FuncDecl>().cloned() {
            match f.name.as_ref().map(|x| x.as_str()).unwrap_or("") {
                "main" => {
                    return Ok(Def {
                        name: d.name,
                        ty: None,
                        value: FuncDecl {
                            name: f.name,
                            params: f.params,
                            ret: f.ret,
                            body: Some(self.specialize_block(f.body.context("empty main")?, ctx)?),
                        }
                        .into(),
                        visibility: d.visibility,
                    }
                    .into());
                }

                "print" => {
                    ctx.insert(d.name.clone(), d.value.clone());
                    return Ok(d.into());
                }
                _ => ctx.insert(d.name.clone(), d.value.clone()),
            }
        }
        Ok(Unit.into())
    }
}
