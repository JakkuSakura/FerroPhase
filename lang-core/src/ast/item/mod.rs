use crate::ast::attr::AstAttribute;
use crate::ast::*;
use crate::id::{Ident, Locator, Path};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};
use std::hash::Hash;

pub type BItem = Box<AstItem>;

common_enum! {
    /// Item is an syntax tree node that "declares" a thing without returning a value
    pub enum AstItem {
        Module(Module),
        DefStruct(DefStruct),
        DefEnum(DefEnum),
        DefType(DefType),
        DefConst(DefConst),
        DefFunction(DefFunction),
        DefTrait(DefTrait),
        DeclType(DeclType),
        DeclConst(DeclConst),
        DeclFunction(DeclFunction),
        Import(Import),
        Impl(Impl),
        Expr(AstExpr),
        Any(AnyBox),
    }
}

impl AstItem {
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(AnyBox::new(any))
    }
    pub fn as_expr(&self) -> Option<&AstExpr> {
        match self {
            Self::Expr(expr) => Some(expr),
            _ => None,
        }
    }
    pub fn unit() -> Self {
        Self::Expr(AstExpr::Value(Value::unit().into()))
    }
    pub fn is_unit(&self) -> bool {
        if let Some(expr) = self.as_expr() {
            if let AstExpr::Value(value) = expr {
                if let Value::Unit(_) = &**value {
                    return true;
                }
            }
        }
        false
    }
    pub fn as_function(&self) -> Option<&DefFunction> {
        match self {
            Self::DefFunction(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_struct(&self) -> Option<&DefStruct> {
        match self {
            Self::DefStruct(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_enum(&self) -> Option<&DefEnum> {
        match self {
            Self::DefEnum(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_type(&self) -> Option<&DefType> {
        match self {
            Self::DefType(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_const(&self) -> Option<&DefConst> {
        match self {
            Self::DefConst(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_trait(&self) -> Option<&DefTrait> {
        match self {
            Self::DefTrait(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_module(&self) -> Option<&Module> {
        match self {
            Self::Module(module) => Some(module),
            _ => None,
        }
    }
    pub fn as_import(&self) -> Option<&Import> {
        match self {
            Self::Import(import) => Some(import),
            _ => None,
        }
    }
    pub fn as_impl(&self) -> Option<&Impl> {
        match self {
            Self::Impl(impl_) => Some(impl_),
            _ => None,
        }
    }
    pub fn get_ident(&self) -> Option<&Ident> {
        match self {
            Self::DefFunction(define) => Some(&define.name),
            Self::DefStruct(define) => Some(&define.name),
            Self::DefEnum(define) => Some(&define.name),
            Self::DefType(define) => Some(&define.name),
            Self::DefConst(define) => Some(&define.name),
            Self::DeclType(declare) => Some(&declare.name),
            Self::DeclConst(declare) => Some(&declare.name),
            Self::DeclFunction(declare) => Some(&declare.name),
            Self::Module(module) => Some(&module.name),
            _ => None,
        }
    }
}

pub type ItemChunk = Vec<AstItem>;
pub trait ItemChunkExt {
    fn find_item(&self, name: &str) -> Option<&AstItem>;
    fn list_impl_trait(&self, trait_ty: &str) -> Vec<&Impl>;
    fn list_impl_type(&self, self_ty: &str) -> Vec<&Impl>;
}
impl ItemChunkExt for ItemChunk {
    fn find_item(&self, name: &str) -> Option<&AstItem> {
        self.iter().find(|item| {
            if let Some(ident) = item.get_ident() {
                ident.as_str() == name
            } else {
                false
            }
        })
    }
    fn list_impl_trait(&self, trait_ty: &str) -> Vec<&Impl> {
        self.iter()
            .filter_map(|item| match item {
                AstItem::Impl(impl_) => {
                    if let Some(trait_ty_) = &impl_.trait_ty {
                        if trait_ty_.to_string() == trait_ty {
                            Some(impl_)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }
    fn list_impl_type(&self, self_ty: &str) -> Vec<&Impl> {
        self.iter()
            .filter_map(|item| match item {
                AstItem::Impl(impl_) if impl_.trait_ty.is_none() => {
                    if let AstExpr::Locator(Locator::Ident(ident)) = &impl_.self_ty {
                        if ident.as_str() == self_ty {
                            Some(impl_)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }
}
common_struct! {
    pub struct Module {
        pub name: Ident,
        pub items: ItemChunk,
        pub visibility: Visibility,
    }
}

common_enum! {
    /// Visibility is a label to an item
    /// The exact semantic is dependent on the language and context
    #[derive(Copy)]
    pub enum Visibility {
        Public,
        Private,
        Inherited,
    }
}

common_struct! {
    pub struct DefStruct {
        pub name: Ident,
        pub value: TypeStruct,
        pub visibility: Visibility,
    }
}

common_struct! {
    pub struct DefEnum {
        pub name: Ident,
        pub value: TypeEnum,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct DefType {
        pub name: Ident,
        pub value: AstType,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct DefConst {
        pub name: Ident,
        pub ty: Option<AstType>,
        pub value: Value,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct DefFunction {
        pub attrs: Vec<AstAttribute>,
        pub name: Ident,
        pub ty: Option<TypeFunction>,
        pub sig: FunctionSignature,
        pub body: BExpr,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct DefTrait {
        pub name: Ident,
        pub bounds: TypeBounds,
        pub items: ItemChunk,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct Import {
        pub visibility: Visibility,
        pub path: Path,
    }
}

common_struct! {
    pub struct Impl {
        pub trait_ty: Option<Locator>,
        pub self_ty: AstExpr,
        pub items: ItemChunk,
    }
}

common_struct! {
    pub struct DeclConst {
        pub name: Ident,
        pub ty: AstType,
    }
}
common_struct! {
    pub struct DeclType {
        pub name: Ident,
        pub bounds: TypeBounds,
    }
}
common_struct! {
    pub struct DeclFunction {
        pub name: Ident,
        pub sig: FunctionSignature,
    }
}
