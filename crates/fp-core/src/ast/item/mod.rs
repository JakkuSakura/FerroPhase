use std::fmt::Formatter;
use std::hash::Hash;

use crate::ast::{TySlot, *};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::{common_enum, common_struct};

mod decl;
mod def;
mod import;

pub use decl::*;
pub use def::*;
pub use import::*;

pub type BItem = Box<Item>;

common_enum! {
    /// Item is syntax node that "declares" a thing without returning a value
    ///
    /// It usually happens at compile time
    ///
    /// For run timm declarations, like in Python, `class Foo: pass`, it is not an item,
    /// rather an Expr
    pub enum ItemKind {
        Module(Module),
        DefStruct(ItemDefStruct),
        DefStructural(ItemDefStructural),
        DefEnum(ItemDefEnum),
        DefType(ItemDefType),
        DefConst(ItemDefConst),
        DefStatic(ItemDefStatic),
        DefFunction(ItemDefFunction),
        DefTrait(ItemDefTrait),
        DeclType(ItemDeclType),
        DeclConst(ItemDeclConst),
        DeclStatic(ItemDeclStatic),
        DeclFunction(ItemDeclFunction),
        Import(ItemImport),
        Impl(ItemImpl),
        Macro(ItemMacro),
        /// not for direct construction, but for interpretation and optimization
        Expr(Expr),
        Any(AnyBox),
    }
}

common_struct! {
    pub struct Item {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty: TySlot,
        #[serde(flatten)]
        pub kind: ItemKind,
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(serializer) = try_get_threadlocal_serializer() {
            match serializer.serialize_item(self) {
                Ok(text) => write!(f, "{}", text),
                Err(_) => write!(f, "<item serialization error>"),
            }
        } else {
            // Fallback: do not panic if no serializer is registered.
            // Use Debug for kind since ItemKind may not implement Display/ToString.
            write!(f, "<item> {:?}", self.kind())
        }
    }
}

impl Item {
    pub fn new(kind: ItemKind) -> Self {
        Self { ty: None, kind }
    }

    pub fn with_ty(kind: ItemKind, ty: TySlot) -> Self {
        Self { ty, kind }
    }

    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    pub fn ty_mut(&mut self) -> &mut TySlot {
        &mut self.ty
    }

    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = Some(ty);
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut ItemKind {
        &mut self.kind
    }

    pub fn with_ty_slot(mut self, ty: TySlot) -> Self {
        self.ty = ty;
        self
    }

    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::from(ItemKind::Any(AnyBox::new(any)))
    }
    pub fn as_expr(&self) -> Option<&Expr> {
        match self.kind() {
            ItemKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }
    pub fn unit() -> Self {
        Self::from(ItemKind::Expr(Expr::value(Value::unit())))
    }
    pub fn is_unit(&self) -> bool {
        self.as_expr().map_or(false, Expr::is_unit)
    }
    pub fn as_function(&self) -> Option<&ItemDefFunction> {
        match self.kind() {
            ItemKind::DefFunction(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_struct(&self) -> Option<&ItemDefStruct> {
        match self.kind() {
            ItemKind::DefStruct(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_enum(&self) -> Option<&ItemDefEnum> {
        match self.kind() {
            ItemKind::DefEnum(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_type(&self) -> Option<&ItemDefType> {
        match self.kind() {
            ItemKind::DefType(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_const(&self) -> Option<&ItemDefConst> {
        match self.kind() {
            ItemKind::DefConst(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_trait(&self) -> Option<&ItemDefTrait> {
        match self.kind() {
            ItemKind::DefTrait(define) => Some(define),
            _ => None,
        }
    }
    pub fn as_module(&self) -> Option<&Module> {
        match self.kind() {
            ItemKind::Module(module) => Some(module),
            _ => None,
        }
    }
    pub fn as_import(&self) -> Option<&ItemImport> {
        match self.kind() {
            ItemKind::Import(import) => Some(import),
            _ => None,
        }
    }
    pub fn as_impl(&self) -> Option<&ItemImpl> {
        match self.kind() {
            ItemKind::Impl(impl_) => Some(impl_),
            _ => None,
        }
    }
    pub fn get_ident(&self) -> Option<&Ident> {
        match self.kind() {
            ItemKind::DefFunction(define) => Some(&define.name),
            ItemKind::DefStruct(define) => Some(&define.name),
            ItemKind::DefEnum(define) => Some(&define.name),
            ItemKind::DefType(define) => Some(&define.name),
            ItemKind::DefConst(define) => Some(&define.name),
            ItemKind::DeclType(declare) => Some(&declare.name),
            ItemKind::DeclConst(declare) => Some(&declare.name),
            ItemKind::DeclFunction(declare) => Some(&declare.name),
            ItemKind::Module(module) => Some(&module.name),
            _ => None,
        }
    }
}

impl<T> From<T> for Item
where
    ItemKind: From<T>,
{
    fn from(value: T) -> Self {
        Item::new(ItemKind::from(value))
    }
}

pub type ItemChunk = Vec<Item>;
pub trait ItemChunkExt {
    fn find_item(&self, name: &str) -> Option<&Item>;
    fn list_impl_trait(&self, trait_ty: &str) -> Vec<&ItemImpl>;
    fn list_impl_type(&self, self_ty: &str) -> Vec<&ItemImpl>;
}
impl ItemChunkExt for ItemChunk {
    fn find_item(&self, name: &str) -> Option<&Item> {
        self.iter().find(|item| {
            if let Some(ident) = item.get_ident() {
                ident.as_str() == name
            } else {
                false
            }
        })
    }
    fn list_impl_trait(&self, trait_ty: &str) -> Vec<&ItemImpl> {
        self.iter()
            .filter_map(|item| match item.kind() {
                ItemKind::Impl(impl_) => {
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
    fn list_impl_type(&self, self_ty: &str) -> Vec<&ItemImpl> {
        self.iter()
            .filter_map(|item| match item.kind() {
                ItemKind::Impl(impl_) if impl_.trait_ty.is_none() => {
                    if let ExprKind::Locator(Locator::Ident(ident)) = impl_.self_ty.kind() {
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
        #[serde(default)]
        pub is_external: bool,
    }
}

common_enum! {
    /// Visibility is a label to an item
    /// The exact semantic is dependent on the language and context
    pub enum Visibility {
        Public,
        Crate,
        Restricted(ItemImportPath),
        Private,
        Inherited,
    }
}

common_struct! {
    pub struct ItemImpl {
        pub trait_ty: Option<Locator>,
        pub self_ty: Expr,
        #[serde(default)]
        pub generics_params: Vec<GenericParam>,
        pub items: ItemChunk,
    }
}

impl ItemImpl {
    pub fn new_ident(self_ty: Ident, items: ItemChunk) -> Self {
        Self {
            trait_ty: None,
            self_ty: Expr::ident(self_ty).into(),
            generics_params: Vec::new(),
            items,
        }
    }
    pub fn new(trait_ty: Option<Locator>, self_ty: Expr, items: ItemChunk) -> Self {
        Self {
            trait_ty,
            self_ty,
            generics_params: Vec::new(),
            items,
        }
    }
}
