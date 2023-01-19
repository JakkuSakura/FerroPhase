pub mod interpreter;
pub mod specializer;

use common::*;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use std::sync::Arc;

pub trait AnyAst: Ast {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Ast + Any> AnyAst for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl dyn AnyAst {
    pub fn as_ast<T: Ast + 'static>(&self) -> Option<&T> {
        self.as_any().downcast_ref::<T>()
    }
}

pub trait Ast: Debug {
    fn is_literal(&self) -> bool {
        false
    }
    fn is_raw(&self) -> bool {
        false
    }
}
impl<T: Ast> Ast for Arc<T> {}

#[derive(Clone)]
pub struct Expr {
    ty: &'static str,
    inner: Rc<dyn AnyAst>,
}

impl Expr {
    pub fn new<T: Ast + 'static>(e: T) -> Self {
        Self {
            ty: std::any::type_name::<T>(),
            inner: Rc::new(e),
        }
    }
    pub fn get_type(&self) -> &str {
        self.ty
    }
}
impl Deref for Expr {
    type Target = dyn AnyAst;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
impl<T: Ast + 'static> From<T> for Expr {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
pub trait Serializer {
    fn serialize(&self, node: &Expr) -> Result<String>;
}

pub trait Deserializer {
    fn deserialize(&self, code: &str) -> Result<Expr>;
}

#[derive(Debug, Clone)]
pub struct Module {
    pub stmts: Vec<Expr>,
}
impl Ast for Module {}
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Expr>,
    pub last_value: bool,
}
impl Ast for Block {}
#[derive(Debug, Clone)]
pub struct Unit;
impl Ast for Unit {
    fn is_literal(&self) -> bool {
        true
    }
}
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
impl Ast for LiteralInt {
    fn is_literal(&self) -> bool {
        true
    }
}
#[derive(Debug, Clone)]
pub struct LiteralBool {
    pub value: bool,
}
impl Ast for LiteralBool {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct LiteralDecimal {
    pub value: f64,
}
impl LiteralDecimal {
    pub fn new(v: f64) -> Self {
        Self { value: v }
    }
}
impl Ast for LiteralDecimal {
    fn is_literal(&self) -> bool {
        true
    }
}
#[derive(Debug, Clone)]
pub struct LiteralChar {
    pub value: char,
}
impl Ast for LiteralChar {
    fn is_literal(&self) -> bool {
        true
    }
}
#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: char,
}
impl Ast for LiteralString {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct LiteralList {
    pub value: Vec<Expr>,
}
impl Ast for LiteralList {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct LiteralUnknown {}
impl Ast for LiteralUnknown {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Default, Debug, Clone)]
pub struct PosArgs {
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct KwArgs {
    pub args: Vec<(String, Expr)>,
}
#[derive(Debug, Clone)]
pub struct Call {
    pub fun: Expr,
    pub args: PosArgs,
}
impl Ast for Call {}
#[derive(Debug, Clone)]
pub struct Def {
    pub name: Ident,
    pub ty: Option<Expr>,
    pub value: Expr,
}
impl Ast for Def {}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: Expr,
}
#[derive(Default, Debug, Clone)]
pub struct Params {
    pub params: Vec<Param>,
}
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: Option<Ident>,
    pub params: Params,
    pub ret: Expr,
    pub body: Option<Block>,
}
impl Ast for FuncDecl {}
#[derive(Debug, Clone)]
pub struct Generics {
    pub params: Params,
    // TODOL restrains
    pub value: Expr,
}
impl Ast for Generics {}

#[derive(Debug, Clone)]
pub struct Assign {
    pub target: Expr,
    pub value: Expr,
}
impl Ast for Assign {}

#[derive(Debug, Clone)]
pub struct When {
    pub cond: Expr,
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
    pub iterable: Expr,
    pub body: Block,
}
impl Ast for ForEach {}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Block,
}
impl Ast for While {}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: Expr,
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
    pub name: Expr, // either Ident or Struct
    pub field: KwArgs,
}
impl Ast for BuildStruct {}

#[derive(Debug, Clone)]
pub struct Select {
    pub obj: Expr,
    pub field: Ident,
}
impl Ast for Select {}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: Vec<Expr>,
    pub ret: Expr,
}
impl Ast for FuncType {}

#[cfg(test)]
mod tests {
    use crate::{Expr, Module};

    #[test]
    fn test_ast_node() {
        let n = Expr::new(Module { stmts: vec![] });
        n.as_ast::<Module>().unwrap();
    }
}
