use super::instr::MirToLirLowerer;
use fp_core::mir::ty::{ConstKind, ConstValue, Scalar};
use fp_core::{lir, mir};

pub(super) fn is_zero_sized(ty: &mir::Ty) -> bool {
    matches!(ty.kind, mir::ty::TyKind::Tuple(ref elements) if elements.is_empty())
        || matches!(ty.kind, mir::ty::TyKind::Never)
}

pub(super) fn slice_ref_element_ty(ty: &mir::Ty) -> Option<&mir::Ty> {
    match &ty.kind {
        mir::ty::TyKind::Slice(elem_ty) => Some(elem_ty),
        mir::ty::TyKind::Ref(_, inner, _) => match &inner.kind {
            mir::ty::TyKind::Slice(elem_ty) => Some(elem_ty),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn instantiate_ty(ty: &mir::Ty, substs: &[mir::ty::GenericArg]) -> mir::Ty {
    let kind = match &ty.kind {
        mir::ty::TyKind::Param(param) => {
            return match substs.get(param.index as usize) {
                Some(mir::ty::GenericArg::Type(concrete)) => concrete.clone(),
                _ => ty.clone(),
            };
        }
        mir::ty::TyKind::RawPtr(mir::ty::TypeAndMut { ty: inner, mutbl }) => {
            mir::ty::TyKind::RawPtr(mir::ty::TypeAndMut {
                ty: Box::new(instantiate_ty(inner, substs)),
                mutbl: *mutbl,
            })
        }
        mir::ty::TyKind::Ref(region, inner, mutbl) => mir::ty::TyKind::Ref(
            region.clone(),
            Box::new(instantiate_ty(inner, substs)),
            *mutbl,
        ),
        mir::ty::TyKind::Slice(inner) => {
            mir::ty::TyKind::Slice(Box::new(instantiate_ty(inner, substs)))
        }
        mir::ty::TyKind::Array(inner, len) => {
            mir::ty::TyKind::Array(Box::new(instantiate_ty(inner, substs)), len.clone())
        }
        mir::ty::TyKind::Tuple(elements) => mir::ty::TyKind::Tuple(
            elements
                .iter()
                .map(|elem| Box::new(instantiate_ty(elem, substs)))
                .collect(),
        ),
        mir::ty::TyKind::Adt(adt, inner_substs) => mir::ty::TyKind::Adt(
            adt.clone(),
            inner_substs
                .iter()
                .map(|arg| match arg {
                    mir::ty::GenericArg::Type(inner) => {
                        mir::ty::GenericArg::Type(instantiate_ty(inner, substs))
                    }
                    other => other.clone(),
                })
                .collect(),
        ),
        other => other.clone(),
    };
    mir::Ty { kind }
}

impl MirToLirLowerer {
    pub(super) fn array_length_from_const(&self, len: &ConstKind) -> u64 {
        match len {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => int.data as u64,
            other => {
                tracing::warn!(
                    "MIR→LIR: array length {:?} not evaluated; defaulting to 0",
                    other
                );
                0
            }
        }
    }

    pub(super) fn zero_value_for_lir_type(&self, ty: &lir::LirType) -> Option<lir::LirValue> {
        match ty {
            lir::LirType::I1
            | lir::LirType::I8
            | lir::LirType::I16
            | lir::LirType::I32
            | lir::LirType::I64
            | lir::LirType::I128 => self
                .integer_constant(ty, 0)
                .ok()
                .map(lir::LirValue::constant),
            lir::LirType::F32 | lir::LirType::F64 => self
                .float_constant(ty, 0.0)
                .ok()
                .map(lir::LirValue::constant),
            lir::LirType::Ptr(_) => {
                Some(lir::LirValue::constant(lir::LirConstant::null(ty.clone())))
            }
            _ => None,
        }
    }

    pub(super) fn zero_constant_for_lir_type(&self, ty: &lir::LirType) -> Option<lir::LirConstant> {
        match ty {
            lir::LirType::I1
            | lir::LirType::I8
            | lir::LirType::I16
            | lir::LirType::I32
            | lir::LirType::I64
            | lir::LirType::I128 => self.integer_constant(ty, 0).ok(),
            lir::LirType::F32 | lir::LirType::F64 => self.float_constant(ty, 0.0).ok(),
            lir::LirType::Ptr(_) => Some(lir::LirConstant::null(ty.clone())),
            _ => None,
        }
    }

    pub(super) fn type_of_operand(&self, operand: &mir::Operand) -> Option<lir::LirType> {
        match operand {
            mir::Operand::Move(place) | mir::Operand::Copy(place) => self
                .lookup_place_type(place)
                .map(|ty| self.lir_type_from_ty(&ty)),
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Bool(_) => Some(lir::LirType::I1),
                mir::ConstantKind::Int(_) | mir::ConstantKind::UInt(_) => Some(lir::LirType::I64),
                mir::ConstantKind::Float(_) => Some(lir::LirType::F64),
                mir::ConstantKind::Fn(_) | mir::ConstantKind::Global(_) => {
                    Some(self.lir_type_from_ty(&constant.ty))
                }
                mir::ConstantKind::Null => Some(lir::LirType::Ptr(Box::new(lir::LirType::I8))),
                _ => None,
            },
        }
    }

    pub(super) fn is_integral_type(&self, ty: &lir::LirType) -> bool {
        matches!(
            ty,
            lir::LirType::I1
                | lir::LirType::I8
                | lir::LirType::I16
                | lir::LirType::I32
                | lir::LirType::I64
                | lir::LirType::I128
        )
    }

    pub(super) fn is_float_type(&self, ty: &lir::LirType) -> bool {
        matches!(ty, lir::LirType::F32 | lir::LirType::F64)
    }

    pub(super) fn type_bit_width(&self, ty: &lir::LirType) -> Option<u32> {
        match ty {
            lir::LirType::I1 => Some(1),
            lir::LirType::I8 => Some(8),
            lir::LirType::I16 => Some(16),
            lir::LirType::I32 => Some(32),
            lir::LirType::I64 => Some(64),
            lir::LirType::I128 => Some(128),
            lir::LirType::F32 => Some(32),
            lir::LirType::F64 => Some(64),
            _ => None,
        }
    }
}
