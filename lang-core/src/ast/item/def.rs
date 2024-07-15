use crate::ast::{
    AstAttribute, AstType, BExpr, FunctionParam, FunctionParamReceiver, FunctionSignature,
    ItemChunk, StructuralField, TypeBounds, TypeEnum, TypeFunction, TypeStruct, TypeStructural,
    ValueFunction, Visibility,
};
use crate::common_struct;
use crate::id::Ident;

common_struct! {
    pub struct ItemDefStruct {
        pub visibility: Visibility,
        pub name: Ident,
        pub value: TypeStruct,
    }
}
impl ItemDefStruct {
    pub fn new(name: Ident, fields: Vec<StructuralField>) -> Self {
        Self {
            visibility: Visibility::Public,
            value: TypeStruct {
                name: name.clone(),
                fields,
            },
            name,
        }
    }
}

common_struct! {
    pub struct ItemDefStructural {
        pub visibility: Visibility,
        pub name: Ident,
        pub value: TypeStructural,
    }
}
common_struct! {
    pub struct ItemDefEnum {
        pub visibility: Visibility,
        pub name: Ident,
        pub value: TypeEnum,
    }
}
common_struct! {
    pub struct ItemDefType {
        pub visibility: Visibility,
        pub name: Ident,
        pub value: AstType,
    }
}
common_struct! {
    pub struct ItemDefConst {
        pub visibility: Visibility,
        pub name: Ident,
        pub ty: Option<AstType>,
        pub value: BExpr,
    }
}
common_struct! {
    pub struct ItemDefStatic {
        pub visibility: Visibility,
        pub name: Ident,
        pub ty: AstType,
        pub value: BExpr,
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
    pub fn with_receiver(mut self, receiver: FunctionParamReceiver) -> Self {
        self.sig.receiver = Some(receiver);
        self
    }
    pub fn with_params(mut self, params: Vec<(Ident, AstType)>) -> Self {
        self.sig.params = params
            .into_iter()
            .map(|(name, ty)| FunctionParam::new(name, ty))
            .collect();
        self
    }
    pub fn with_ret_ty(mut self, ret_ty: AstType) -> Self {
        self.sig.ret_ty = ret_ty;
        self
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
