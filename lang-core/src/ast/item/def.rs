use crate::ast::{
    AstAttribute, AstType, BExpr, FunctionSignature, ItemChunk, TypeBounds, TypeEnum, TypeFunction,
    TypeStruct, TypeStructural, ValueFunction, Visibility,
};
use crate::common_struct;
use crate::id::Ident;

common_struct! {
    pub struct ItemDefStruct {
        pub name: Ident,
        pub value: TypeStruct,
        pub visibility: Visibility,
    }
}

common_struct! {
    pub struct ItemDefStructural {
        pub name: Ident,
        pub value: TypeStructural,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct ItemDefEnum {
        pub name: Ident,
        pub value: TypeEnum,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct ItemDefType {
        pub name: Ident,
        pub value: AstType,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct ItemDefConst {
        pub name: Ident,
        pub ty: Option<AstType>,
        pub value: BExpr,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct ItemDefStatic {
        pub name: Ident,
        pub ty: AstType,
        pub value: BExpr,
        pub visibility: Visibility,
    }
}
common_struct! {
    pub struct ItemDefFunction {
        pub attrs: Vec<AstAttribute>,
        pub name: Ident,
        pub ty: Option<TypeFunction>,
        pub sig: FunctionSignature,
        pub body: BExpr,
        pub visibility: Visibility,
    }
}
impl ItemDefFunction {
    pub fn new_simple(name: Ident, body: BExpr) -> Self {
        let mut sig = FunctionSignature::unit();
        sig.name = Some(name.clone());
        Self {
            attrs: Vec::new(),
            name,
            ty: None,
            sig,
            body,
            visibility: Visibility::Public,
        }
    }
    pub fn _to_value(&self) -> ValueFunction {
        ValueFunction {
            sig: self.sig.clone(),
            body: self.body.clone(),
        }
    }
}
common_struct! {
    pub struct ItemDefTrait {
        pub name: Ident,
        pub bounds: TypeBounds,
        pub items: ItemChunk,
        pub visibility: Visibility,
    }
}
