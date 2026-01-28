use std::fmt::Formatter;
use std::hash::Hash;

use crate::ast::{TySlot, *};
use crate::span::Span;
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::common_struct;

mod decl;
mod def;
mod import;

pub use decl::*;
pub use def::*;
pub use import::*;

pub type BItem = Box<Item>;

/// Item is syntax node that "declares" a thing without returning a value
///
/// It usually happens at compile time
///
/// For run timm declarations, like in Python, `class Foo: pass`, it is not an item,
/// rather an Expr
#[derive(Debug, Clone, PartialEq, Hash)]
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

impl From<Module> for ItemKind {
    fn from(value: Module) -> Self {
        ItemKind::Module(value)
    }
}

impl From<ItemDefStruct> for ItemKind {
    fn from(value: ItemDefStruct) -> Self {
        ItemKind::DefStruct(value)
    }
}

impl From<ItemDefStructural> for ItemKind {
    fn from(value: ItemDefStructural) -> Self {
        ItemKind::DefStructural(value)
    }
}

impl From<ItemDefEnum> for ItemKind {
    fn from(value: ItemDefEnum) -> Self {
        ItemKind::DefEnum(value)
    }
}

impl From<ItemDefType> for ItemKind {
    fn from(value: ItemDefType) -> Self {
        ItemKind::DefType(value)
    }
}

impl From<ItemDefConst> for ItemKind {
    fn from(value: ItemDefConst) -> Self {
        ItemKind::DefConst(value)
    }
}

impl From<ItemDefStatic> for ItemKind {
    fn from(value: ItemDefStatic) -> Self {
        ItemKind::DefStatic(value)
    }
}

impl From<ItemDefFunction> for ItemKind {
    fn from(value: ItemDefFunction) -> Self {
        ItemKind::DefFunction(value)
    }
}

impl From<ItemDefTrait> for ItemKind {
    fn from(value: ItemDefTrait) -> Self {
        ItemKind::DefTrait(value)
    }
}

impl From<ItemDeclType> for ItemKind {
    fn from(value: ItemDeclType) -> Self {
        ItemKind::DeclType(value)
    }
}

impl From<ItemDeclConst> for ItemKind {
    fn from(value: ItemDeclConst) -> Self {
        ItemKind::DeclConst(value)
    }
}

impl From<ItemDeclStatic> for ItemKind {
    fn from(value: ItemDeclStatic) -> Self {
        ItemKind::DeclStatic(value)
    }
}

impl From<ItemDeclFunction> for ItemKind {
    fn from(value: ItemDeclFunction) -> Self {
        ItemKind::DeclFunction(value)
    }
}

impl From<ItemImport> for ItemKind {
    fn from(value: ItemImport) -> Self {
        ItemKind::Import(value)
    }
}

impl From<ItemImpl> for ItemKind {
    fn from(value: ItemImpl) -> Self {
        ItemKind::Impl(value)
    }
}

impl From<ItemMacro> for ItemKind {
    fn from(value: ItemMacro) -> Self {
        ItemKind::Macro(value)
    }
}

impl From<Expr> for ItemKind {
    fn from(value: Expr) -> Self {
        ItemKind::Expr(value)
    }
}

impl From<AnyBox> for ItemKind {
    fn from(value: AnyBox) -> Self {
        ItemKind::Any(value)
    }
}

common_struct! {
    pub struct Item {
        pub ty: TySlot,
        pub span: Option<Span>,
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
        Self {
            ty: None,
            span: None,
            kind,
        }
    }

    pub fn with_ty(kind: ItemKind, ty: TySlot) -> Self {
        Self {
            ty,
            span: None,
            kind,
        }
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

    pub fn span(&self) -> Span {
        self.span.unwrap_or_else(|| self.kind.span())
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
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

impl ItemKind {
    pub fn span(&self) -> Span {
        match self {
            ItemKind::Module(module) => module.span(),
            ItemKind::DefStruct(def) => def.span(),
            ItemKind::DefStructural(def) => def.span(),
            ItemKind::DefEnum(def) => def.span(),
            ItemKind::DefType(def) => def.span(),
            ItemKind::DefConst(def) => def.span(),
            ItemKind::DefStatic(def) => def.span(),
            ItemKind::DefFunction(def) => def.span(),
            ItemKind::DefTrait(def) => def.span(),
            ItemKind::DeclType(decl) => decl.span(),
            ItemKind::DeclConst(decl) => decl.span(),
            ItemKind::DeclStatic(decl) => decl.span(),
            ItemKind::DeclFunction(decl) => decl.span(),
            ItemKind::Import(import) => import.span(),
            ItemKind::Impl(impl_block) => impl_block.span(),
            ItemKind::Macro(mac) => mac.span(),
            ItemKind::Expr(expr) => expr.span(),
            ItemKind::Any(_) => Span::null(),
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
        pub attrs: Vec<Attribute>,
        pub name: Ident,
        pub items: ItemChunk,
        pub visibility: Visibility,
        pub is_external: bool,
    }
}
impl Module {
    pub fn span(&self) -> Span {
        Span::union(self.items.iter().map(Item::span))
    }
}

/// Visibility is a label to an item
/// The exact semantic is dependent on the language and context
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Crate,
    Restricted(ItemImportPath),
    Private,
    Inherited,
}

impl From<ItemImportPath> for Visibility {
    fn from(value: ItemImportPath) -> Self {
        Visibility::Restricted(value)
    }
}

common_struct! {
    pub struct ItemImpl {
        pub attrs: Vec<Attribute>,
        pub trait_ty: Option<Locator>,
        pub self_ty: Expr,
        pub generics_params: Vec<GenericParam>,
        pub items: ItemChunk,
    }
}

impl ItemImpl {
    pub fn new_ident(self_ty: Ident, items: ItemChunk) -> Self {
        Self {
            attrs: Vec::new(),
            trait_ty: None,
            self_ty: Expr::ident(self_ty).into(),
            generics_params: Vec::new(),
            items,
        }
    }
    pub fn new(trait_ty: Option<Locator>, self_ty: Expr, items: ItemChunk) -> Self {
        Self {
            attrs: Vec::new(),
            trait_ty,
            self_ty,
            generics_params: Vec::new(),
            items,
        }
    }

    pub fn span(&self) -> Span {
        Span::union(
            [
                self.trait_ty.as_ref().map(Locator::span),
                Some(self.self_ty.span()),
                Some(Span::union(self.items.iter().map(Item::span))),
            ]
            .into_iter()
            .flatten(),
        )
    }
}
