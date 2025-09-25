use super::*;

impl MirGenerator {
    pub(super) fn is_unit_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Tuple(ref elems) if elems.is_empty())
    }

    pub(super) fn is_never_type(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Never)
    }

    pub(super) fn transform_type(&self, ty: &fp_core::types::Ty) -> fp_core::types::Ty {
        // For now, just clone the THIR type since MIR can use the same type system
        // In a full implementation, this might transform types to MIR-specific representations
        ty.clone()
    }
}
