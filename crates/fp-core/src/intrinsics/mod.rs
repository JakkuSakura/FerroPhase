//! Shared intrinsic and standard-library descriptors.
//!
//! Front-ends normalise language-specific helpers into symbolic intrinsics while
//! back-ends materialise those symbols into concrete runtime calls. The goal of
//! this module is to host the shared vocabulary so every consumer speaks the same
//! language before we introduce backend-specific resolvers.

use crate::ast::{
    Expr, ExprIntrinsicCall, ExprIntrinsicContainer, ExprMacro, ExprStruct, ExprStructural, File,
    FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemKind, Ty, TySlot,
    TypeFunction,
};
use crate::error::Result;

/// Strategy interface for language-specific intrinsic normalisation.
pub trait IntrinsicNormalizer {
    fn normalize(&self, _node: &mut crate::ast::Node) -> Result<()> {
        Ok(())
    }

    fn normalize_call(&self, _call: &ExprIntrinsicCall) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn normalize_container(&self, _container: &ExprIntrinsicContainer) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn normalize_struct(&self, _struct_expr: &ExprStruct) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn normalize_structural(&self, _struct_expr: &ExprStructural) -> Result<Option<Expr>> {
        Ok(None)
    }

    /// Language-specific macro lowering hook. When provided by a frontend, the
    /// shared intrinsic normalizer will delegate `ExprKind::Macro` to this
    /// implementation. Return `Ok(Some(expr))` to replace the macro with `expr`.
    fn normalize_macro(&self, _macro_expr: &ExprMacro) -> Result<Option<Expr>> {
        Ok(None)
    }
}

/// Strategy interface for backend-specific intrinsic materialisation.
pub trait IntrinsicMaterializer {
    fn prepare_file(&self, _file: &mut File) {}

    fn materialize_call(
        &self,
        _call: &mut ExprIntrinsicCall,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_struct(
        &self,
        _struct_expr: &mut ExprStruct,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_structural(
        &self,
        _struct_expr: &mut ExprStructural,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }

    fn materialize_container(
        &self,
        _container: &mut ExprIntrinsicContainer,
        _expr_ty: &TySlot,
    ) -> Result<Option<Expr>> {
        Ok(None)
    }
}

fn build_function_decl_item(
    name: &str,
    mut params: Vec<FunctionParam>,
    ret_ty: Ty,
) -> ItemDeclFunction {
    let name_ident = Ident::new(name);
    for param in params.iter_mut() {
        if param.ty_annotation.is_none() {
            param.ty_annotation = Some(param.ty.clone());
        }
    }

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
        is_const: false,
        ret_ty: Some(ret_ty),
    };

    ItemDeclFunction {
        ty_annotation: Some(ty_annotation),
        name: name_ident,
        sig,
    }
}

/// Insert a function declaration if one with the same name does not already exist.
pub fn make_function_decl(name: &str, params: Vec<FunctionParam>, ret_ty: Ty) -> ItemDeclFunction {
    build_function_decl_item(name, params, ret_ty)
}

pub fn ensure_function_decl(file: &mut File, decl: ItemDeclFunction) {
    let name = decl.name.clone();
    let exists = file.items.iter().any(|item| match item.kind() {
        ItemKind::DeclFunction(existing) if existing.name == name => true,
        ItemKind::DefFunction(existing) if existing.name == name => true,
        _ => false,
    });

    if exists {
        return;
    }

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
