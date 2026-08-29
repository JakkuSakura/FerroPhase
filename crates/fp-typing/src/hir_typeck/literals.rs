use super::*;

impl HirTypeChecker {
    pub(super) fn unit_ty(&self) -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    pub(super) fn literal_ty(&self, literal: &hir::Lit) -> Ty {
        match literal {
            hir::Lit::Bool(_) => Ty::bool(),
            hir::Lit::Char(_) => Ty::char(),
            hir::Lit::Integer(_) => Ty::int(ty::IntTy::I64),
            hir::Lit::Float(_) => Ty::float(ty::FloatTy::F64),
            hir::Lit::Str(_) => str_ty(),
            hir::Lit::Null => Ty::never(),
            hir::Lit::Bytes(bytes) => Ty {
                kind: TyKind::Ref(
                    ty::Region::ReErased,
                    Box::new(Ty {
                        kind: TyKind::Array(
                            Box::new(Ty::uint(ty::UintTy::U8)),
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: bytes.len() as u128,
                                    size: 8,
                                },
                            ))),
                        ),
                    }),
                    ty::Mutability::Not,
                ),
            },
            hir::Lit::CStr(_) => self
                .well_known_struct_ty("CStr", Vec::new())
                .map(|ty| Ty {
                    kind: TyKind::Ref(ty::Region::ReErased, Box::new(ty), ty::Mutability::Not),
                })
                .unwrap_or_else(Ty::never),
        }
    }
}
