use super::*;

impl MirGenerator {
    pub(super) fn is_unit_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Tuple(ref elems) if elems.is_empty())
    }

    pub(super) fn is_never_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Never)
    }

    pub(super) fn transform_type(&self, ty: &hir_types::Ty) -> Ty {
        Ty {
            kind: match &ty.kind {
                hir_types::TyKind::Bool => TyKind::Bool,
                hir_types::TyKind::Char => TyKind::Char,
                hir_types::TyKind::Int(int_ty) => TyKind::Int(convert_int_ty(*int_ty)),
                hir_types::TyKind::Uint(uint_ty) => TyKind::Uint(convert_uint_ty(*uint_ty)),
                hir_types::TyKind::Float(float_ty) => TyKind::Float(convert_float_ty(*float_ty)),
                hir_types::TyKind::Never => TyKind::Never,
                hir_types::TyKind::Tuple(elements) => TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.transform_type(elem)))
                        .collect(),
                ),
                hir_types::TyKind::Array(inner, _) => TyKind::Array(
                    Box::new(self.transform_type(inner)),
                    mir_types::ConstKind::Value(mir_types::ConstValue::ZeroSized),
                ),
                hir_types::TyKind::Slice(inner) => {
                    TyKind::Slice(Box::new(self.transform_type(inner)))
                }
                hir_types::TyKind::Ref(_, inner, mutbl) => TyKind::Ref(
                    mir_types::Region::ReStatic,
                    Box::new(self.transform_type(inner)),
                    match mutbl {
                        hir_types::Mutability::Mut => mir_types::Mutability::Mut,
                        hir_types::Mutability::Not => mir_types::Mutability::Not,
                    },
                ),
                hir_types::TyKind::RawPtr(pointee) => TyKind::RawPtr(mir_types::TypeAndMut {
                    ty: Box::new(self.transform_type(&pointee.ty)),
                    mutbl: match pointee.mutbl {
                        hir_types::Mutability::Mut => mir_types::Mutability::Mut,
                        hir_types::Mutability::Not => mir_types::Mutability::Not,
                    },
                }),
                hir_types::TyKind::FnDef(def_id, _) => TyKind::FnDef(*def_id, Vec::new()),
                hir_types::TyKind::FnPtr(_) => default_fn_ptr(),
                hir_types::TyKind::Param(_) => TyKind::Int(mir_types::IntTy::I32),
                hir_types::TyKind::Opaque(def_id, _) => TyKind::Opaque(*def_id, Vec::new()),
                hir_types::TyKind::Adt(_, _) => TyKind::Int(mir_types::IntTy::I64),
                _ => TyKind::Int(mir_types::IntTy::I32),
            },
        }
    }
}

fn convert_int_ty(int_ty: hir_types::IntTy) -> mir_types::IntTy {
    match int_ty {
        hir_types::IntTy::Isize => mir_types::IntTy::Isize,
        hir_types::IntTy::I8 => mir_types::IntTy::I8,
        hir_types::IntTy::I16 => mir_types::IntTy::I16,
        hir_types::IntTy::I32 => mir_types::IntTy::I32,
        hir_types::IntTy::I64 => mir_types::IntTy::I64,
        hir_types::IntTy::I128 => mir_types::IntTy::I128,
    }
}

fn convert_uint_ty(uint_ty: hir_types::UintTy) -> mir_types::UintTy {
    match uint_ty {
        hir_types::UintTy::Usize => mir_types::UintTy::Usize,
        hir_types::UintTy::U8 => mir_types::UintTy::U8,
        hir_types::UintTy::U16 => mir_types::UintTy::U16,
        hir_types::UintTy::U32 => mir_types::UintTy::U32,
        hir_types::UintTy::U64 => mir_types::UintTy::U64,
        hir_types::UintTy::U128 => mir_types::UintTy::U128,
    }
}

fn convert_float_ty(float_ty: hir_types::FloatTy) -> mir_types::FloatTy {
    match float_ty {
        hir_types::FloatTy::F32 => mir_types::FloatTy::F32,
        hir_types::FloatTy::F64 => mir_types::FloatTy::F64,
    }
}

fn default_fn_ptr() -> TyKind {
    TyKind::RawPtr(mir_types::TypeAndMut {
        ty: Box::new(Ty {
            kind: TyKind::Int(mir_types::IntTy::I8),
        }),
        mutbl: mir_types::Mutability::Not,
    })
}
