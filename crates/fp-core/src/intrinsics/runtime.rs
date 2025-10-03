use crate::ast::{
    ExprIntrinsicCall, File, FunctionParam, FunctionSignature, Item, ItemDeclFunction, ItemKind,
    Ty, TypeFunction,
};
use crate::error::Result;
use crate::id::Ident;

/// Strategy interface for backends to supply runtime intrinsic materialisations.
pub trait RuntimeIntrinsicStrategy {
    fn prepare_file(&self, _file: &mut File) {}
    fn rewrite_intrinsic(
        &self,
        _call: &ExprIntrinsicCall,
        _expr_ty: &crate::ast::TySlot,
    ) -> Result<Option<crate::ast::Expr>> {
        Ok(None)
    }
}

/// Specification for generating a function declaration node.
#[derive(Clone, Debug)]
pub struct ParamSpec {
    pub name: Ident,
    pub ty: Ty,
    pub as_tuple: bool,
    pub as_dict: bool,
}

impl ParamSpec {
    pub fn new(name: &str, ty: Ty) -> Self {
        Self {
            name: Ident::new(name),
            ty,
            as_tuple: false,
            as_dict: false,
        }
    }

    pub fn string(name: &str) -> Self {
        Self::new(name, Ty::Primitive(crate::ast::TypePrimitive::String))
    }

    pub fn any(name: &str) -> Self {
        Self::new(name, Ty::Any(crate::ast::TypeAny))
    }

    pub fn any_tuple(name: &str) -> Self {
        let mut spec = Self::any(name);
        spec.as_tuple = true;
        spec
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: Ident,
    pub params: Vec<ParamSpec>,
    pub ret_ty: Ty,
}

impl FunctionDecl {
    pub fn new(name: &str, params: Vec<ParamSpec>, ret_ty: Ty) -> Self {
        Self {
            name: Ident::new(name),
            params,
            ret_ty,
        }
    }

    pub fn into_decl(self) -> ItemDeclFunction {
        let params: Vec<FunctionParam> = self
            .params
            .into_iter()
            .map(|spec| {
                let mut param = FunctionParam::new(spec.name, spec.ty.clone());
                param.ty_annotation = Some(spec.ty);
                param.as_tuple = spec.as_tuple;
                param.as_dict = spec.as_dict;
                param
            })
            .collect();

        let ty_annotation = Ty::Function(TypeFunction {
            params: params.iter().map(|p| p.ty.clone()).collect(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(self.ret_ty.clone())),
        });

        let sig = FunctionSignature {
            name: Some(self.name.clone()),
            receiver: None,
            params,
            generics_params: Vec::new(),
            ret_ty: Some(self.ret_ty),
        };

        ItemDeclFunction {
            ty_annotation: Some(ty_annotation),
            name: self.name,
            sig,
        }
    }
}

/// Insert a function declaration if one with the same name does not already exist.
pub fn ensure_function_decl(file: &mut File, decl: FunctionDecl) {
    let name = decl.name.as_str();
    let exists = file.items.iter().any(|item| match item.kind() {
        ItemKind::DeclFunction(existing) if existing.name.as_str() == name => true,
        ItemKind::DefFunction(existing) if existing.name.as_str() == name => true,
        _ => false,
    });

    if exists {
        return;
    }

    file.items.insert(0, Item::from(decl.into_decl()));
}
