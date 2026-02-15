use crate::ast::{FunctionSignature, Ident, Ty, TySlot, TypeBounds};
use crate::common_struct;
use crate::span::Span;

common_struct! {
    pub struct ItemDeclConst {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub ty: Ty,
    }
}
impl ItemDeclConst {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.ty_annotation.as_ref().map(Ty::span),
                Some(self.ty.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}

common_struct! {
    pub struct ItemDeclStatic {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub ty: Ty,
    }
}
impl ItemDeclStatic {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.ty_annotation.as_ref().map(Ty::span),
                Some(self.ty.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}
common_struct! {
    pub struct ItemDeclType {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub bounds: TypeBounds,
    }
}
impl ItemDeclType {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.ty_annotation.as_ref().map(Ty::span),
                Some(self.bounds.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}
common_struct! {
    pub struct ItemDeclFunction {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub sig: FunctionSignature,
    }
}
impl ItemDeclFunction {
    pub fn span(&self) -> Span {
        Span::union(
            [
                self.ty_annotation.as_ref().map(Ty::span),
                Some(self.sig.span()),
            ]
            .into_iter()
            .flatten(),
        )
    }
}
