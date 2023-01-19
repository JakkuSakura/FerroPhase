pub mod interpreter;

use common::*;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use std::sync::Arc;

pub trait AnyAst: Ast {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Ast + Any + 'static> AnyAst for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl dyn AnyAst {
    pub fn as_ast<T: Ast + 'static>(&self) -> Option<&T> {
        self.as_any().downcast_ref::<T>()
    }
}

pub trait Ast: Debug {}
impl<T: Ast> Ast for Arc<T> {}

#[derive(Clone)]
pub struct AstNode {
    inner: Rc<dyn AnyAst>,
}

impl AstNode {
    pub fn new(a: impl Ast + 'static) -> Self {
        Self { inner: Rc::new(a) }
    }
}
impl Deref for AstNode {
    type Target = dyn AnyAst;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl Debug for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
impl<T: Ast + 'static> From<T> for AstNode {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
pub trait Serializer {
    fn serialize(&self, node: &AstNode) -> Result<String>;
}

pub trait Deserializer {
    fn deserialize(&self, code: &str) -> Result<AstNode>;
}

#[derive(Debug, Clone)]
pub struct Module {
    pub stmts: Vec<AstNode>,
}
impl Ast for Module {}
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<AstNode>,
    pub last_value: bool,
}
impl Ast for Block {}
#[derive(Debug, Clone)]
pub struct Unit;
impl Ast for Unit {}
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Ident {
    pub name: String,
}
impl Ident {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}
impl Ast for Ident {}

#[derive(Debug, Clone)]
pub struct LiteralInt {
    pub value: i64,
}
impl LiteralInt {
    pub fn new(i: i64) -> Self {
        Self { value: i }
    }
}
impl Ast for LiteralInt {}
#[derive(Debug, Clone)]
pub struct LiteralBool {
    pub value: bool,
}
impl Ast for LiteralBool {}

#[derive(Debug, Clone)]
pub struct LiteralDecimal {
    pub value: f64,
}
impl Ast for LiteralDecimal {}
#[derive(Debug, Clone)]
pub struct LiteralChar {
    pub value: char,
}
impl Ast for LiteralChar {}
#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: char,
}
impl Ast for LiteralString {}

#[derive(Debug, Clone)]
pub struct LiteralList {
    pub value: Vec<AstNode>,
}
impl Ast for LiteralList {}

#[derive(Debug, Clone)]
pub struct LiteralUnknown {}
impl Ast for LiteralUnknown {}

#[derive(Debug, Clone)]
pub struct PosArgs {
    pub args: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub struct KwArgs {
    pub args: Vec<(String, AstNode)>,
}
#[derive(Debug, Clone)]
pub struct Apply {
    pub fun: AstNode,
    pub args: PosArgs,
}
impl Ast for Apply {}
#[derive(Debug, Clone)]
pub struct Def {
    pub name: Ident,
    pub ty: Option<AstNode>,
    pub value: AstNode,
}
impl Ast for Def {}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: AstNode,
}
#[derive(Debug, Clone)]
pub struct Params {
    pub params: Vec<Param>,
}
#[derive(Debug, Clone)]
pub struct Fun {
    pub name: Option<Ident>,
    pub params: Params,
    pub ret: AstNode,
    pub body: Option<Block>,
}
impl Ast for Fun {}

#[derive(Debug, Clone)]
pub struct Assign {
    pub target: AstNode,
    pub value: AstNode,
}
impl Ast for Assign {}

#[derive(Debug, Clone)]
pub struct When {
    pub cond: AstNode,
    pub body: Block,
}
impl Ast for When {}

#[derive(Debug, Clone)]
pub struct Case {
    pub cases: Vec<When>,
}

#[derive(Debug, Clone)]
pub struct ForEach {
    pub variable: Ident,
    pub iterable: AstNode,
    pub body: Block,
}
impl Ast for ForEach {}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: AstNode,
    pub body: Block,
}
impl Ast for While {}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: AstNode,
}

#[derive(Debug, Clone)]
pub struct Fields {
    pub fields: Vec<Field>,
}
#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Fields,
}
impl Ast for Struct {}

#[derive(Debug, Clone)]
pub struct BuildStruct {
    pub name: AstNode, // either Ident or Struct
    pub field: KwArgs,
}
impl Ast for BuildStruct {}

#[derive(Debug, Clone)]
pub struct Select {
    pub obj: AstNode,
    pub field: Ident,
}
impl Ast for Select {}

#[cfg(test)]
mod tests {
    use crate::{AstNode, Module};

    #[test]
    fn test_ast_node() {
        let n = AstNode::new(Module { stmts: vec![] });
        n.as_ast::<Module>().unwrap();
    }
}
