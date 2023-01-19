use crate::{Ast, Block, Call, Expr, Fun, Ident, LiteralInt, Module, Serializer, Unit};
use common::*;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::mem::replace;
use std::rc::Rc;
pub struct InterpreterContextInner {
    parent: Option<InterpreterContext>,
    values: HashMap<Ident, Expr>,
    buffer: Vec<String>,
}

#[derive(Clone)]
pub struct InterpreterContext {
    inner: Rc<RefCell<InterpreterContextInner>>,
}
impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner {
                parent: None,
                values: Default::default(),
                buffer: vec![],
            })),
        }
    }
    pub fn child(&self) -> InterpreterContext {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner {
                parent: Some(self.clone()),
                values: Default::default(),
                buffer: vec![],
            })),
        }
    }
    pub fn insert(&self, key: Ident, value: Expr) {
        self.inner.borrow_mut().values.insert(key, value);
    }
    pub fn get(&self, key: &Ident) -> Option<Expr> {
        let inner = self.inner.borrow();
        inner
            .values
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get(key))
    }
    pub fn root(&self) -> InterpreterContext {
        self.inner
            .borrow()
            .parent
            .as_ref()
            .map(|x| x.root())
            .unwrap_or_else(|| self.clone())
    }
    pub fn print_str(&self, s: String) {
        self.inner.borrow_mut().buffer.push(s);
    }
    pub fn take_outputs(&self) -> Vec<String> {
        replace(&mut self.inner.borrow_mut().buffer, vec![])
    }
}
struct BuiltinFn {
    name: String,
    f: Box<dyn Fn(&[Expr], &InterpreterContext) -> Result<Expr>>,
}
impl BuiltinFn {
    pub fn new(
        name: String,
        f: impl Fn(&[Expr], &InterpreterContext) -> Result<Expr> + 'static,
    ) -> Self {
        Self {
            name,
            f: Box::new(f),
        }
    }
    pub fn call(&self, args: &[Expr], ctx: &InterpreterContext) -> Result<Expr> {
        (self.f)(args, ctx)
    }
}
impl Debug for BuiltinFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFn")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}
impl Ast for BuiltinFn {}
pub struct Interpreter {
    serializer: Rc<dyn Serializer>,
}
impl Interpreter {
    pub fn new(serializer: Rc<dyn Serializer>) -> Self {
        Self { serializer }
    }
    pub fn interprete_module(&self, node: &Module, ctx: &InterpreterContext) -> Result<Expr> {
        let _: Vec<_> = node
            .stmts
            .iter()
            .filter(|x| x.as_ast::<Fun>().is_some())
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        let result: Vec<_> = node
            .stmts
            .iter()
            .filter(|x| x.as_ast::<Fun>().is_none())
            .map(|x| self.interprete(x, ctx))
            .try_collect::<Expr, Vec<_>, _>()?
            .into_iter()
            .filter(|x| x.as_ast::<Unit>().is_none())
            .collect();
        Ok(result.into_iter().next().unwrap_or(Unit.into()))
    }
    pub fn interprete_decl_fun(&self, node: &Fun, ctx: &InterpreterContext) -> Result<Expr> {
        ctx.insert(node.name.clone().unwrap(), node.clone().into());
        Ok(Unit.into())
    }
    pub fn interprete_apply(&self, node: &Call, ctx: &InterpreterContext) -> Result<Expr> {
        let fun = self.interprete(&node.fun, ctx)?;
        let args: Vec<_> = node
            .args
            .args
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        if let Some(f) = fun.as_ast::<Fun>() {
            let name = f.name.as_ref().map(|x| x.name.as_str()).unwrap_or("<fun>");
            debug!("Invoking {} with {:?}", name, args);
            let sub = ctx.child();
            for (i, arg) in args.iter().cloned().enumerate() {
                let param = f
                    .params
                    .params
                    .get(i)
                    .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                // TODO: type check here
                sub.insert(param.name.clone(), arg);
            }
            let ret =
                self.interprete_block(f.body.as_ref().context("Funtion body is empty")?, &sub);
            debug!("Invoked {} with {:?} => {:?}", name, args, ret);
            return ret;
        }
        if let Some(f) = fun.as_ast::<BuiltinFn>() {
            debug!("Invoking {} with {:?}", f.name, args);
            let ret = f.call(&args, ctx);
            debug!("Invoked {} with {:?} => {:?}", f.name, args, ret);
            return ret;
        }

        bail!("Failed to interprete {:?}", node)
    }

    pub fn interprete_block(&self, node: &Block, ctx: &InterpreterContext) -> Result<Expr> {
        let ret: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        if node.last_value && !ret.is_empty() {
            Ok(ret.last().cloned().unwrap())
        } else {
            Ok(Unit.into())
        }
    }
    pub fn interprete_print(
        se: &dyn Serializer,
        args: &[Expr],
        ctx: &InterpreterContext,
    ) -> Result<Expr> {
        let formatted: Vec<_> = args.into_iter().map(|x| se.serialize(x)).try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(Unit.into())
    }
    pub fn interprete(&self, node: &Expr, ctx: &InterpreterContext) -> Result<Expr> {
        debug!("Interpreting {:?}", node);
        if let Some(n) = node.as_ast() {
            return self.interprete_module(n, ctx);
        }
        if let Some(n) = node.as_ast::<Fun>() {
            if n.name == Some(Ident::new("main")) {
                return self.interprete_block(
                    n.body.as_ref().context("main() has no implementation")?,
                    ctx,
                );
            } else {
                return self.interprete_decl_fun(n, ctx);
            }
        }
        if let Some(n) = node.as_ast() {
            return self.interprete_apply(n, ctx);
        }
        if let Some(n) = node.as_ast::<Ident>() {
            fn operate_on_i64(name: &str, op: impl Fn(&[i64]) -> i64 + 'static) -> BuiltinFn {
                BuiltinFn::new(name.to_string(), move |args, _ctx| {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            x.as_ast::<LiteralInt>()
                                .map(|x| x.value)
                                .context("Only LiteralInt")
                        })
                        .try_collect()?;
                    Ok(LiteralInt::new(op(&args)).into())
                })
            }
            return match n.name.as_str() {
                "+" => Ok(operate_on_i64("+", |x| x.into_iter().sum()).into()),
                "-" => Ok(operate_on_i64("-", |x| {
                    x.into_iter()
                        .enumerate()
                        .map(|(i, &x)| if i > 0 { -x } else { x })
                        .sum()
                })
                .into()),
                "*" => Ok(operate_on_i64("*", |x| x.into_iter().fold(1, |a, b| a * b)).into()),
                "print" => {
                    let se = Rc::clone(&self.serializer);
                    Ok(BuiltinFn::new("print".to_string(), move |args, ctx| {
                        Self::interprete_print(&*se, args, ctx)
                    })
                    .into())
                }
                _ => ctx
                    .get(n)
                    .ok_or_else(|| eyre!("could not find {:?} in context", n.name)),
            };
        }
        if let Some(n) = node.as_ast::<LiteralInt>() {
            return Ok(n.clone().into());
        }

        bail!("Failed to interprete {:?}", node)
    }
}
