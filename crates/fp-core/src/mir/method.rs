//! Method/function-specialization bookkeeping `HirToMirLowerer`
//! (`fp-backend`) computes while lowering one package — moved here (rather
//! than kept as `fp-backend`-local types) so it can live durably on
//! `MirPackage` instead of being recomputed, cached only per lowering
//! instance, and thrown away at the end of each compile. Pure data: no
//! behavior lives on these types except `ConstInfo::typed_value`, a trivial
//! field-copying getter.

use super::ty::{SubstsRef, Ty};
use super::FunctionSig;
use crate::hir;
use crate::span::Span;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct MethodLoweringInfo {
    pub def_id: Option<hir::DefId>,
    pub substs: SubstsRef,
    pub sig: FunctionSig,
    pub fn_name: String,
    pub fn_ty: Ty,
    pub struct_def: Option<hir::DefId>,
}

#[derive(Clone)]
pub struct MethodDefinition {
    pub def_id: hir::DefId,
    pub function: hir::Function,
    pub impl_generics: hir::Generics,
    pub self_ty: hir::TypeExpr,
    pub self_def: Option<hir::DefId>,
    pub method_name: String,
    pub assoc_types: HashMap<String, hir::TypeExpr>,
}

#[derive(Clone)]
pub struct MethodHirRef {
    pub function: hir::Function,
    pub span: Span,
    pub method_context: Option<MethodContext>,
}

#[derive(Clone, Debug)]
pub struct FunctionSpecializationInfo {
    pub def_id: hir::DefId,
    pub substs: SubstsRef,
    pub name: String,
    pub sig: FunctionSig,
    pub fn_ty: Ty,
}

#[derive(Clone, Debug)]
pub struct MethodContext {
    pub def_id: Option<hir::DefId>,
    pub path: Vec<hir::PathSegment>,
    pub mir_self_ty: Ty,
    /// This impl's own `type Name = ...;` bindings (e.g. `impl<T> Index<usize>
    /// for Vec<T> { type Output = T; ... }` → `{"Output": T's TypeExpr}`),
    /// so a method signature's `Self::Output` resolves to the *bound*
    /// type (here, the impl's own `T`) rather than collapsing to `Self`
    /// itself.
    pub assoc_types: HashMap<String, hir::TypeExpr>,
}

/// Reverse index from a method's own `impl_item.def_id` to its owning
/// `Impl` item (and that item's index within it) — `Rc`, not owned, since
/// it's built by iterating an impl's own `items` once per *method*; a naive
/// owned clone here would re-clone the same enclosing `Item` (every
/// method's own body included) once per method in that impl.
pub type MethodOwnerIndex = HashMap<hir::DefId, (Rc<hir::Item>, usize)>;

#[derive(Clone)]
pub struct ConstInfo {
    pub ty: Ty,
    pub value: super::Constant,
}

impl ConstInfo {
    pub fn typed_value(&self) -> super::Constant {
        let mut value = self.value.clone();
        value.ty = self.ty.clone();
        value
    }
}
