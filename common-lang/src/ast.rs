use common::{impl_downcast, Downcast};
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;

pub trait Ast: Downcast + Debug {
    fn is_literal(&self) -> bool {
        false
    }
    fn is_raw(&self) -> bool {
        false
    }
}

impl_downcast!(Ast);

#[derive(Clone)]
pub struct Expr {
    inner: Rc<dyn Ast>,
}

impl Expr {
    pub fn new<T: Ast>(e: T) -> Self {
        Self { inner: Rc::new(e) }
    }

    pub fn is_ast<T: Ast>(&self) -> bool {
        self.inner.is::<T>()
    }

    pub fn as_ast<T: Ast>(&self) -> Option<&T> {
        self.inner.downcast_ref::<T>()
    }

    pub fn make_ast_mut<T: Ast + Clone + 'static>(&mut self) -> Option<&mut T> {
        if Rc::weak_count(&self.inner) == 0 && Rc::strong_count(&self.inner) == 1 {
            Rc::get_mut(&mut self.inner).unwrap().downcast_mut::<T>()
        } else {
            let inner = Rc::new(self.inner.as_any().downcast_ref::<T>()?.clone());
            self.inner = inner;
            Rc::get_mut(&mut self.inner).unwrap().downcast_mut::<T>()
        }
    }
}

impl Deref for Expr {
    type Target = dyn Ast;
    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: Ast + Clone> From<T> for Expr {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl AsRef<dyn Ast> for Expr {
    fn as_ref(&self) -> &dyn Ast {
        &*self.inner
    }
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

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
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

impl Ast for PosArgs {}

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

#[derive(Debug, Clone)]
pub struct Reference {
    pub referee: Expr,
    pub mutable: Option<bool>,
}

impl Ast for Reference {}

#[derive(Debug, Clone)]
pub struct Uplifted {
    pub uplifted: Expr,
    pub raw: Expr,
}
impl Ast for Uplifted {
    fn is_raw(&self) -> bool {
        self.raw.is_raw()
    }
    fn is_literal(&self) -> bool {
        self.raw.is_literal()
    }
}
#[derive(Debug, Clone)]
pub enum Types {
    Function(FuncType),
    I64,
    F64,
    Bool,
}

impl Types {
    pub fn func(params: Vec<Expr>, ret: Expr) -> FuncType {
        FuncType { params, ret }
    }
    pub fn i64() -> Types {
        Types::I64
    }
    pub fn f64() -> Types {
        Types::F64
    }
    pub fn bool() -> Types {
        Types::Bool
    }
}
impl Ast for Types {}

pub fn uplift_common_ast(expr: &Expr) -> Expr {
    if let Some(expr) = expr.as_ast::<Types>() {
        let uplifted: Expr = match expr {
            Types::I64 => Ident::new("i64").into(),
            Types::F64 => Ident::new("f64").into(),
            Types::Bool => Ident::new("bool").into(),
            Types::Function(f) => f.clone().into(),
        };
        return Uplifted {
            uplifted: uplifted,
            raw: expr.clone().into(),
        }
        .into();
    }
    expr.clone()
}
