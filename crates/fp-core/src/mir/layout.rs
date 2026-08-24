//! Struct/enum shape and layout data `HirToMirLowerer` (`fp-backend`)
//! computes while lowering one package — moved here (rather than kept as
//! `fp-backend`-local types) so it can live durably on `MirPackage` instead
//! of being recomputed, cached only per lowering instance, and thrown away
//! at the end of each compile. Pure data: no behavior lives on these types.

use super::ty::Ty;
use crate::hir;

#[derive(Clone, Debug)]
pub struct StructFieldDef {
    pub name: String,
    pub ty: hir::TypeExpr,
}

#[derive(Clone, Debug)]
pub struct StructDefinition {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<StructFieldDef>,
    pub field_index: std::collections::HashMap<String, usize>,
}

#[derive(Clone, Debug)]
pub struct StructLayout {
    pub ty: Ty,
    pub field_tys: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructuralLayoutKey {
    pub fields: Vec<(String, Ty)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructLayoutKey {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
}

#[derive(Clone, Debug)]
pub struct EnumVariantDef {
    pub def_id: hir::DefId,
    pub name: String,
    pub discriminant: i64,
    pub payload: Option<hir::TypeExpr>,
}

#[derive(Clone, Debug)]
pub struct EnumDefinition {
    pub def_id: hir::DefId,
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<EnumVariantDef>,
}

#[derive(Clone, Debug)]
pub struct EnumVariantInfo {
    pub def_id: hir::DefId,
    pub enum_def: hir::DefId,
    pub discriminant: i64,
    pub payload_def: Option<hir::DefId>,
}

#[derive(Clone, Debug)]
pub struct EnumLayout {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
    pub tag_ty: Ty,
    pub payload_tys: Vec<Ty>,
    pub enum_ty: Ty,
    pub variant_payloads: std::collections::HashMap<hir::DefId, Vec<Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumLayoutKey {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
}
