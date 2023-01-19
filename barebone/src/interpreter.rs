use crate::{Apply, Ast, AstNode, Block, Fun, Ident, LiteralInt, Module, Unit};
use common::*;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
pub struct InterpreterContextInner {
    parent: Option<InterpreterContext>,
    values: HashMap<Ident, AstNode>,
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
            })),
        }
    }
    pub fn child(&self) -> InterpreterContext {
        Self {
            inner: Rc::new(RefCell::new(InterpreterContextInner {
                parent: Some(self.clone()),
                values: Default::default(),
            })),
        }
    }
    pub fn insert(&self, key: Ident, value: AstNode) {
        self.inner.borrow_mut().values.insert(key, value);
    }
    pub fn get(&self, key: &Ident) -> Option<AstNode> {
        let inner = self.inner.borrow();
        inner
            .values
            .get(key)
            .cloned()
            .or_else(|| inner.parent.as_ref()?.get(key))
    }
}
struct BuiltinFn {
    f: Box<dyn Fn(&[AstNode]) -> Result<AstNode>>,
}
impl BuiltinFn {
    pub fn new(f: impl Fn(&[AstNode]) -> Result<AstNode> + 'static) -> Self {
        Self { f: Box::new(f) }
    }
}
impl Debug for BuiltinFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("BuiltinFn")
    }
}
impl Ast for BuiltinFn {}
pub struct Interpreter {}
impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }
    pub fn interprete_module(&self, node: &Module, ctx: &InterpreterContext) -> Result<AstNode> {
        let _: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        Ok(Unit.into())
    }
    pub fn interprete_decl_fun(&self, node: &Fun, ctx: &InterpreterContext) -> Result<AstNode> {
        ctx.insert(node.name.clone().unwrap(), node.clone().into());
        Ok(Unit.into())
    }
    pub fn interprete_apply(&self, node: &Apply, ctx: &InterpreterContext) -> Result<AstNode> {
        let fun = self.interprete(&node.fun, ctx)?;
        if let Some(f) = fun.as_ast::<Fun>() {
            let sub = ctx.child();
            for (i, arg) in node.args.args.iter().enumerate() {
                let param = f
                    .params
                    .params
                    .get(i)
                    .with_context(|| format!("Couldn't find {} parameter of {:?}", i, f))?;
                let arg = self.interprete(arg, ctx)?;
                // TODO: type check here
                sub.insert(param.name.clone(), arg);
            }
            return self.interprete_block(f.body.as_ref().context("Funtion body is empty")?, &sub);
        }
        if let Some(f) = fun.as_ast::<BuiltinFn>() {
            let args: Vec<_> = node
                .args
                .args
                .iter()
                .map(|x| self.interprete(x, ctx))
                .try_collect()?;
            return (f.f)(&args);
        }

        bail!("Failed to interprete {:?}", node)
    }

    pub fn interprete_block(&self, node: &Block, ctx: &InterpreterContext) -> Result<AstNode> {
        let _: Vec<_> = node
            .stmts
            .iter()
            .map(|x| self.interprete(x, ctx))
            .try_collect()?;
        Ok(Unit.into())
    }
    pub fn interprete(&self, node: &AstNode, ctx: &InterpreterContext) -> Result<AstNode> {
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
            return match n.name.as_str() {
                "+" => Ok(BuiltinFn::new(|args| {
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| x.as_ast::<LiteralInt>().context("Only LiteralInt"))
                        .try_collect()?;
                    Ok(LiteralInt::new(args.into_iter().map(|x| x.value).sum()).into())
                })
                .into()),
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
