use crate::ast::{FunctionSignature, Ty, TySlot, TypeBounds};
use crate::common_struct;
use crate::id::Ident;

common_struct! {
    pub struct ItemDeclConst {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub ty: Ty,
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
common_struct! {
    pub struct ItemDeclType {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub name: Ident,
        pub bounds: TypeBounds,
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
