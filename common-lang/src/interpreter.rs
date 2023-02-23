use crate::*;
use common::*;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::mem::replace;
use std::rc::Rc;
#[derive(Default)]
pub struct InterpreterContextInner {
    parent: Option<InterpreterContext>,
    values: HashMap<Ident, Expr>,
    is_specialized: HashMap<Ident, bool>,
    buffer: Vec<String>,
}

#[derive(Clone)]
pub struct InterpreterContext {
    inner: Rc<RefCell<InterpreterContextInner>>,
}
impl InterpreterContext {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner::default())),
        }
    }
    pub fn child(&self) -> InterpreterContext {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner {
                parent: Some(self.clone()),
                ..Default::default()
            })),
        }
    }
    pub fn insert(&self, key: Ident, value: Expr) {
        self.inner.borrow_mut().values.insert(key, value);
    }

    pub fn insert_specialized(&self, key: Ident, value: Expr) {
        self.inner.borrow_mut().values.insert(key.clone(), value);
        self.inner.borrow_mut().is_specialized.insert(key, true);
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
    pub fn list_specialized(&self) -> Vec<Ident> {
        self.inner
            .borrow()
            .is_specialized
            .iter()
            .filter(|x| *x.1)
            .map(|x| x.0.clone())
            .collect()
    }
}
#[derive(Clone)]
struct BuiltinFn {
    name: String,
    f: Rc<dyn Fn(&[Expr], &InterpreterContext) -> Result<Expr>>,
}
impl BuiltinFn {
    pub fn new(
        name: String,
        f: impl Fn(&[Expr], &InterpreterContext) -> Result<Expr> + 'static,
    ) -> Self {
        Self {
            name,
            f: Rc::new(f),
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
        node.stmts
            .iter()
            .filter(|x| x.as_ast::<Def>().is_some())
            .try_for_each(|x| {
                if let Some(n) = x.as_ast::<Def>() {
                    if let Some(n) = n.value.as_ast::<FuncDecl>() {
                        return self.register_decl_fun(n, ctx).map(|_| ());
                    }
                }

                return Ok(());
            })?;
        let result: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect::<Expr, Vec<_>, _>()?
            .into_iter()
            .filter(|x| x.as_ast::<Unit>().is_none())
            .collect();
        Ok(result.into_iter().next().unwrap_or(Unit.into()))
    }
    pub fn register_decl_fun(&self, node: &FuncDecl, ctx: &InterpreterContext) -> Result<Expr> {
        ctx.insert(node.name.clone().unwrap(), node.clone().into());
        Ok(Unit.into())
    }
    pub fn interprete_call(&self, node: &Call, ctx: &InterpreterContext) -> Result<Expr> {
        let fun = self.interprete(&node.fun, ctx)?;

        let args: Vec<_> = node
            .args
            .args
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        if let Some(f) = fun.as_ast::<FuncDecl>() {
            let name = f.name.as_ref().map(|x| x.name.as_str()).unwrap_or("<fun>");
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

            debug!("Invoking {} with {:?}", name, args);
            let ret =
                self.interprete_block(f.body.as_ref().context("Funtion body is empty")?, &sub)?;
            debug!("Invoked {} with {:?} => {:?}", name, args, ret);
            return Ok(ret);
        }
        if let Some(f) = fun.as_ast::<BuiltinFn>() {
            debug!("Invoking {} with {:?}", f.name, args);
            let ret = f.call(&args, ctx)?;
            debug!("Invoked {} with {:?} => {:?}", f.name, args, ret);
            return Ok(ret);
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
    pub fn interprete_cond(&self, node: &Cond, ctx: &InterpreterContext) -> Result<Expr> {
        for case in &node.cases {
            let interpreted = self.interprete(&case.cond, ctx)?;
            let ret = interpreted.as_ast::<LiteralBool>().map(|x| x.value);
            match ret {
                Some(true) => {
                    return self.interprete(&case.body, ctx);
                }
                Some(false) => {
                    continue;
                }
                None => {
                    bail!("Failed to interprete {:?} => {:?}", case.cond, interpreted)
                }
            }
        }
        Ok(Unit.into())
    }
    pub fn interprete_print(
        se: &dyn Serializer,
        args: &[Expr],
        ctx: &InterpreterContext,
    ) -> Result<Expr> {
        let formatted: Vec<_> = args.into_iter().map(|x| se.serialize(&**x)).try_collect()?;
        ctx.root().print_str(formatted.join(" "));
        Ok(Unit.into())
    }
    pub fn interprete_ident(&self, ident: &Ident, ctx: &InterpreterContext) -> Result<Expr> {
        fn operate_on_literals(
            name: &str,
            op_i64: impl Fn(&[i64]) -> i64 + 'static,
            op_f64: impl Fn(&[f64]) -> f64 + 'static,
        ) -> BuiltinFn {
            BuiltinFn::new(name.to_string(), move |args, _ctx| {
                if args
                    .first()
                    .map(|x| x.as_ast::<LiteralInt>())
                    .flatten()
                    .is_some()
                {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            x.as_ast::<LiteralInt>()
                                .map(|x| x.value)
                                .context("Only LiteralInt")
                        })
                        .try_collect()?;
                    return Ok(LiteralInt::new(op_i64(&args)).into());
                }
                if args
                    .first()
                    .map(|x| x.as_ast::<LiteralDecimal>())
                    .flatten()
                    .is_some()
                {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            x.as_ast::<LiteralDecimal>()
                                .map(|x| x.value)
                                .context("Only LiteralInt")
                        })
                        .try_collect()?;
                    return Ok(LiteralDecimal::new(op_f64(&args)).into());
                }
                bail!("Does not support argument type {:?}", args)
            })
        }
        fn binary_comparison_on_literals(
            name: &str,
            op_i64: impl Fn(i64, i64) -> bool + 'static,
            op_f64: impl Fn(f64, f64) -> bool + 'static,
        ) -> BuiltinFn {
            BuiltinFn::new(name.to_string(), move |args, _ctx| {
                if args.len() != 2 {
                    bail!("Argument expected 2, got: {:?}", args)
                }
                if args
                    .first()
                    .map(|x| x.as_ast::<LiteralInt>())
                    .flatten()
                    .is_some()
                {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            x.as_ast::<LiteralInt>()
                                .map(|x| x.value)
                                .context("Only LiteralInt")
                        })
                        .try_collect()?;
                    return Ok(LiteralBool::new(op_i64(args[0], args[1])).into());
                }
                if args
                    .first()
                    .map(|x| x.as_ast::<LiteralDecimal>())
                    .flatten()
                    .is_some()
                {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| {
                            x.as_ast::<LiteralDecimal>()
                                .map(|x| x.value)
                                .context("Only LiteralInt")
                        })
                        .try_collect()?;
                    return Ok(LiteralBool::new(op_f64(args[0], args[1])).into());
                }
                bail!("Does not support argument type {:?}", args)
            })
        }
        return match ident.as_str() {
            "+" => Ok(
                operate_on_literals("+", |x| x.into_iter().sum(), |x| x.into_iter().sum()).into(),
            ),
            "-" => Ok(operate_on_literals(
                "-",
                |x| {
                    x.into_iter()
                        .enumerate()
                        .map(|(i, &x)| if i > 0 { -x } else { x })
                        .sum()
                },
                |x| {
                    x.into_iter()
                        .enumerate()
                        .map(|(i, &x)| if i > 0 { -x } else { x })
                        .sum()
                },
            )
            .into()),
            "*" => Ok(operate_on_literals(
                "*",
                |x| x.into_iter().fold(1, |a, b| a * b),
                |x| x.into_iter().fold(1.0, |a, b| a * b),
            )
            .into()),
            "print" => {
                let se = Rc::clone(&self.serializer);
                Ok(BuiltinFn::new("print".to_string(), move |args, ctx| {
                    Self::interprete_print(&*se, args, ctx)
                })
                .into())
            }
            ">" => Ok(binary_comparison_on_literals(">", |x, y| x > y, |x, y| x > y).into()),
            ">=" => Ok(binary_comparison_on_literals(">=", |x, y| x >= y, |x, y| x >= y).into()),
            "<" => Ok(binary_comparison_on_literals("<", |x, y| x < y, |x, y| x < y).into()),
            "<=" => Ok(binary_comparison_on_literals("<=", |x, y| x <= y, |x, y| x <= y).into()),
            "==" => Ok(binary_comparison_on_literals("==", |x, y| x == y, |x, y| x == y).into()),
            "!=" => Ok(binary_comparison_on_literals("!=", |x, y| x != y, |x, y| x != y).into()),

            _ => ctx
                .get(ident)
                .with_context(|| format!("could not find {:?} in context", ident.name)),
        };
    }
    pub fn interprete_def(&self, n: &Def, ctx: &InterpreterContext) -> Result<Expr> {
        let mut decl = &n.value;
        if let Some(g) = decl.as_ast::<Generics>() {
            decl = &g.value;
        }
        if let Some(n) = decl.as_ast::<FuncDecl>() {
            if n.name == Some(Ident::new("main")) {
                return self.interprete_block(
                    n.body.as_ref().context("main() has no implementation")?,
                    ctx,
                );
            } else {
                return self.register_decl_fun(n, ctx);
            }
        }
        bail!("Could not process {:?}", n)
    }
    pub fn interprete(&self, node: &Expr, ctx: &InterpreterContext) -> Result<Expr> {
        debug!("Interpreting {}", self.serializer.serialize(&**node)?);
        if let Some(n) = node.as_ast() {
            return self.interprete_module(n, ctx);
        }
        if let Some(n) = node.as_ast() {
            return self.interprete_block(n, ctx);
        }
        if let Some(n) = node.as_ast::<Def>() {
            return self.interprete_def(n, ctx);
        }
        if let Some(n) = node.as_ast() {
            return self.interprete_call(n, ctx);
        }
        if let Some(n) = node.as_ast::<Ident>() {
            return self.interprete_ident(n, ctx);
        }
        if let Some(e) = node.as_ast() {
            return self.interprete_cond(e, ctx);
        }
        if node.is_literal() {
            return Ok(node.clone());
        }
        if node.is_raw() {
            return Ok(Unit.into());
        }
        bail!("Failed to interprete {:?}", node)
    }
}
