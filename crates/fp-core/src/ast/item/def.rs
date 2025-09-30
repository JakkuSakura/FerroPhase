use crate::ast::{
    Attribute, BExpr, FunctionParam, FunctionParamReceiver, FunctionSignature, ItemChunk,
    StructuralField, Ty, TySlot, TypeBounds, TypeEnum, TypeFunction, TypeStruct, TypeStructural,
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
        pub value: Ty,
    }
}
common_struct! {
    pub struct ItemDefConst {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub visibility: Visibility,
        pub name: Ident,
        pub ty: Option<Ty>,
        pub value: BExpr,
    }
}
impl ItemDefConst {
    pub fn ty_annotation(&self) -> Option<&Ty> {
        self.ty_annotation.as_ref()
    }

    pub fn ty_annotation_mut(&mut self) -> &mut TySlot {
        &mut self.ty_annotation
    }

    pub fn set_ty_annotation(&mut self, ty: Ty) {
        self.ty_annotation = Some(ty);
    }
}
common_struct! {
    pub struct ItemDefStatic {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub visibility: Visibility,
        pub name: Ident,
        pub ty: Ty,
        pub value: BExpr,
    }
}
impl ItemDefStatic {
    pub fn ty_annotation(&self) -> Option<&Ty> {
        self.ty_annotation.as_ref()
    }

    pub fn ty_annotation_mut(&mut self) -> &mut TySlot {
        &mut self.ty_annotation
    }

    pub fn set_ty_annotation(&mut self, ty: Ty) {
        self.ty_annotation = Some(ty);
    }
}
common_struct! {
    pub struct ItemDefFunction {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty_annotation: TySlot,
        pub attrs: Vec<Attribute>,
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
            ty_annotation: None,
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
    pub fn with_params(mut self, params: Vec<(Ident, Ty)>) -> Self {
        self.sig.params = params
            .into_iter()
            .map(|(name, ty)| FunctionParam::new(name, ty))
            .collect();
        self
    }
    pub fn with_ret_ty(mut self, ret_ty: Ty) -> Self {
        self.sig.ret_ty = Some(ret_ty);
        self
    }
    pub fn _to_value(&self) -> ValueFunction {
        ValueFunction {
            sig: self.sig.clone(),
            body: self.body.clone(),
        }
    }

    pub fn ty_annotation(&self) -> Option<&Ty> {
        self.ty_annotation.as_ref()
    }

    pub fn ty_annotation_mut(&mut self) -> &mut TySlot {
        &mut self.ty_annotation
    }

    pub fn set_ty_annotation(&mut self, ty: Ty) {
        self.ty_annotation = Some(ty);
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
