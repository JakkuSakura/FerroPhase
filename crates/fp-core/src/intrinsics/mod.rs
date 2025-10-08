//! Shared intrinsic and standard-library descriptors.
//!
//! Front-ends normalise language-specific helpers into symbolic intrinsics while
//! back-ends materialise those symbols into concrete runtime calls. The goal of
//! this module is to host the shared vocabulary so every consumer speaks the same
//! language before we introduce backend-specific resolvers.

use crate::ast::{
    ExprIntrinsicCall, File, FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction,
    ItemKind, Ty, TySlot, TypeFunction,
};
use crate::error::Result;

/// Strategy interface for language-specific intrinsic normalisation.
pub trait IntrinsicNormalizer {
    fn normalize(&self, _node: &mut crate::ast::Node) -> Result<()> {
        Ok(())
    }
}

/// Strategy interface for backend-specific intrinsic materialisation.
pub trait IntrinsicMaterializer {
    fn prepare_file(&self, _file: &mut File) {}

    fn rewrite_intrinsic(
        &self,
        _call: &ExprIntrinsicCall,
        _expr_ty: &TySlot,
    ) -> Result<Option<crate::ast::Expr>> {
        Ok(None)
    }
}

/// Lightweight parameter description used to build function declarations when materialising
/// runtime helpers.
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

fn build_function_decl_item(name: &str, params: Vec<ParamSpec>, ret_ty: Ty) -> ItemDeclFunction {
    let name_ident = Ident::new(name);
    let params: Vec<FunctionParam> = params
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
        ret_ty: Some(Box::new(ret_ty.clone())),
    });

    let sig = FunctionSignature {
        name: Some(name_ident.clone()),
        receiver: None,
        params,
        generics_params: Vec::new(),
        ret_ty: Some(ret_ty),
    };

    ItemDeclFunction {
        ty_annotation: Some(ty_annotation),
        name: name_ident,
        sig,
    }
}

/// Insert a function declaration if one with the same name does not already exist.
pub fn ensure_function_decl(file: &mut File, name: &str, params: Vec<ParamSpec>, ret_ty: Ty) {
    let exists = file.items.iter().any(|item| match item.kind() {
        ItemKind::DeclFunction(existing) if existing.name.as_str() == name => true,
        ItemKind::DefFunction(existing) if existing.name.as_str() == name => true,
        _ => false,
    });

    if exists {
        return;
    }

    let decl = build_function_decl_item(name, params, ret_ty);
    file.items.insert(0, Item::from(decl));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StdIntrinsic {
    // I/O
    IoPrintln,
    IoPrint,
    IoEprint,
    IoEprintln,

    // Memory allocation
    AllocAlloc,
    AllocDealloc,
    AllocRealloc,

    // Math - f64
    F64Sin,
    F64Cos,
    F64Tan,
    F64Sqrt,
    F64Pow,
    F64Log,
    F64Exp,

    // Math - f32
    F32Sin,
    F32Cos,
    F32Tan,
    F32Sqrt,
    F32Pow,
    F32Log,
    F32Exp,

    // String operations
    StrLen,
    StrCmp,

    // Process control
    ProcessExit,
    ProcessAbort,
}

pub mod calls;

pub use calls::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};
