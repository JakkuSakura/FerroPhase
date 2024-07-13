use crate::ast::{AstType, FunctionSignature, TypeBounds};
use crate::common_struct;
use crate::id::Ident;

common_struct! {
    pub struct ItemDeclConst {
        pub name: Ident,
        pub ty: AstType,
    }
}

common_struct! {
    pub struct ItemDeclStatic {
        pub name: Ident,
        pub ty: AstType,
    }
}
common_struct! {
    pub struct ItemDeclType {
        pub name: Ident,
        pub bounds: TypeBounds,
    }
}
common_struct! {
    pub struct ItemDeclFunction {
        pub name: Ident,
        pub sig: FunctionSignature,
    }
}
