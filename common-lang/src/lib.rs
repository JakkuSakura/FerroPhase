#![feature(decl_macro)]
extern crate core;

pub mod interpreter;
pub mod specializer;

use common::*;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use std::sync::Arc;

pub trait AnyAst: Ast {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
    fn into_any_rc(self: Rc<Self>) -> Rc<dyn Any>;
    fn clone_any_rc(&self) -> Rc<dyn AnyAst>;
}

impl<T: Ast + Clone + Any> AnyAst for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
    fn into_any_rc(self: Rc<Self>) -> Rc<dyn Any> {
        self
    }
    fn clone_any_rc(&self) -> Rc<dyn AnyAst> {
        Rc::new(self.clone())
    }
}

impl dyn AnyAst {
    pub fn is_ast<T: Ast + 'static>(&self) -> bool {
        self.as_any().is::<T>()
    }

    pub fn as_ast<T: Ast + 'static>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }
    pub fn as_ast_mut<T: Ast + 'static>(&mut self) -> Option<&mut T> {
        self.as_any_mut().downcast_mut()
    }
    pub fn into_ast<T: Ast + 'static>(self: Box<Self>) -> Option<T> {
        self.into_any().downcast().ok().map(|x| *x)
    }
    pub fn into_ast_rc<T: Ast + Clone + 'static>(self: Rc<Self>) -> Option<T> {
        self.into_any_rc()
            .downcast()
            .ok()
            .map(|x| match Rc::try_unwrap(x) {
                Ok(x) => x,
                Err(x) => T::clone(&x),
            })
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
    pub fn new<T: Ast + Clone + 'static>(e: T) -> Self {
        Self {
            ty: std::any::type_name::<T>(),
            inner: Rc::new(e),
        }
    }
    pub fn get_type(&self) -> &str {
        self.ty
    }
    pub fn into_ast<T: Ast + Clone + 'static>(self) -> Option<T> {
        self.inner.into_ast_rc()
    }
    pub fn make_ast_mut<T: Ast + Clone + 'static>(&mut self) -> Option<&mut T> {
        if Rc::weak_count(&self.inner) == 0 && Rc::strong_count(&self.inner) == 1 {
            Rc::get_mut(&mut self.inner).unwrap().as_ast_mut::<T>()
        } else {
            let inner = self.inner.clone_any_rc();
            self.inner = inner;
            Rc::get_mut(&mut self.inner).unwrap().as_ast_mut::<T>()
        }
    }
}
impl Deref for Expr {
    type Target = dyn AnyAst;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}
// impl DerefMut for Expr {
//     type Target = dyn AnyAst;
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         Rc::make_mut(&mut self.inner)
//     }
// }

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
impl<T: Ast + Clone + 'static> From<T> for Expr {
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
    pub name: Ident,
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
    pub fn as_str(&self) -> &str {
        self.name.as_str()
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
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct LiteralBool {
    pub value: bool,
}
impl LiteralBool {
    pub fn new(i: bool) -> Self {
        Self { value: i }
    }
}

impl Ast for LiteralBool {
    fn is_literal(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
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
#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}
#[derive(Debug, Clone)]
pub struct Def {
    pub name: Ident,
    pub ty: Option<Expr>,
    pub value: Expr,
    pub visibility: Visibility,
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
pub struct CondCase {
    pub cond: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub cases: Vec<CondCase>,
    pub if_style: bool,
}
impl Ast for Cond {}

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
pub enum SelectType {
    Unknown,
    Field,
    Method,
    Function,
}
#[derive(Debug, Clone)]
pub struct Select {
    pub obj: Expr,
    pub field: Ident,
    pub select: SelectType,
}
impl Ast for Select {}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: Vec<Expr>,
    pub ret: Expr,
}
impl Ast for FuncType {}

pub struct Types {}
impl Types {
    pub fn func(params: Vec<Expr>, ret: Expr) -> FuncType {
        FuncType { params, ret }
    }
    pub fn i64() -> Ident {
        Ident::new("i64")
    }
    pub fn f64() -> Ident {
        Ident::new("f64")
    }
    pub fn bool() -> Ident {
        Ident::new("bool")
    }
}
