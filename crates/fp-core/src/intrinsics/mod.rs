//! Shared intrinsic and standard-library descriptors.
//!
//! Front-ends normalise language-specific helpers into symbolic intrinsics while
//! back-ends materialise those symbols into concrete runtime calls. The goal of
//! this module is to host the shared vocabulary so every consumer speaks the same
//! language before we introduce backend-specific resolvers.

use crate::ast::{
    Expr, ExprIntrinsicCall, ExprIntrinsicContainer, ExprStruct, ExprStructural, File,
    FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemKind, Ty, TySlot,
    TypeFunction,
};
use crate::error::Result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NormalizeOutcome<T> {
    /// The strategy chose not to handle this node; the shared framework should
    /// continue normalizing it (including descending into children).
    Ignored(T),
    /// The strategy normalized this node and produced a replacement.
    Normalized(T),
}

impl<T> NormalizeOutcome<T> {
    pub fn into_inner(self) -> T {
        match self {
            NormalizeOutcome::Ignored(value) | NormalizeOutcome::Normalized(value) => value,
        }
    }

    pub fn is_normalized(&self) -> bool {
        matches!(self, NormalizeOutcome::Normalized(_))
    }
}

/// Default strategy that never performs language-specific normalization.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoopIntrinsicNormalizer;

/// Strategy interface for language-specific intrinsic normalisation.
pub trait IntrinsicNormalizer {
    fn normalize(&self, _node: &mut crate::ast::Node) -> Result<()> {
        Ok(())
    }

    /// Strategy hook for intrinsic call expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::IntrinsicCall`.
    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for intrinsic container expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::IntrinsicContainer`.
    fn normalize_container(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for struct literal expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Struct`.
    fn normalize_struct(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Strategy hook for structural literal expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Structural`.
    fn normalize_structural(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }

    /// Language-specific macro lowering hook. When provided by a frontend, the
    /// shared intrinsic normalizer will delegate `ExprKind::Macro` to this
    /// implementation. Return `NormalizeOutcome::Normalized(expr)` to replace
    /// the macro with `expr`.
    /// Strategy hook for macro expressions.
    ///
    /// The framework guarantees `expr.kind()` is `ExprKind::Macro`.
    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        Ok(NormalizeOutcome::Ignored(expr))
    }
}

impl IntrinsicNormalizer for NoopIntrinsicNormalizer {}

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
