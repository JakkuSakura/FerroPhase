use super::*;

pub(super) fn const_kind_to_u64(kind: &ty::ConstKind) -> Option<u64> {
    match kind {
        ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(scalar))) => {
            Some(scalar.data as u64)
        }
        _ => None,
    }
}

/// Inverse of `const_kind_to_u64` — builds a resolved array-length
/// `ConstKind` from a real count, matching the exact construction the
/// `Array`/`ArrayRepeat` expression-checking arms above already use.
pub(super) fn u64_to_const_kind(value: u64) -> ty::ConstKind {
    ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(ty::ScalarInt {
        data: value as u128,
        size: 8,
    })))
}

/// Whether `ty` still has an uninstantiated generic parameter anywhere in
/// its structure — used to decide whether a call's result is worth
/// reconciling against an ambient expected-type hint (only meaningful for
/// still-generic results; a fully concrete type needs no such help).
pub(super) fn ty_contains_param(ty: &Ty) -> bool {
    match &ty.kind {
        TyKind::Param(_) => true,
        TyKind::Ref(_, inner, _) => ty_contains_param(inner),
        TyKind::RawPtr(value) => ty_contains_param(&value.ty),
        TyKind::Slice(inner) => ty_contains_param(inner),
        TyKind::Array(inner, _) => ty_contains_param(inner),
        TyKind::Tuple(tys) => tys.iter().any(|ty| ty_contains_param(ty)),
        TyKind::Adt(_, args) => args.iter().any(|arg| match arg {
            GenericArg::Type(ty) => ty_contains_param(ty),
            _ => false,
        }),
        TyKind::FnPtr(signature) => {
            signature
                .binder
                .value
                .inputs
                .iter()
                .any(|ty| ty_contains_param(ty))
                || ty_contains_param(&signature.binder.value.output)
        }
        _ => false,
    }
}

pub(super) fn ty_contains_error(ty: &Ty) -> bool {
    match &ty.kind {
        TyKind::Error(_) => true,
        TyKind::Ref(_, inner, _) => ty_contains_error(inner),
        TyKind::RawPtr(value) => ty_contains_error(&value.ty),
        TyKind::Slice(inner) | TyKind::Array(inner, _) => ty_contains_error(inner),
        TyKind::Tuple(tys) => tys.iter().any(|ty| ty_contains_error(ty)),
        TyKind::Adt(_, args) => args.iter().any(|arg| match arg {
            GenericArg::Type(ty) => ty_contains_error(ty),
            GenericArg::Const(_) | GenericArg::Lifetime(_) => false,
        }),
        TyKind::FnPtr(signature) => {
            signature.binder.value.inputs.iter().any(|ty| ty_contains_error(ty))
                || ty_contains_error(&signature.binder.value.output)
        }
        _ => false,
    }
}

/// The same erased shape a plain `str`/string literal already resolves to
/// (`literal_ty`'s `Lit::Str` arm) — used by literal/union string type
/// resolution so every one of those erases identically.
pub(super) fn str_ty() -> Ty {
    Ty {
        kind: TyKind::Slice(Box::new(Ty::uint(ty::UintTy::U8))),
    }
}

/// The `hir::HirPackage::impls_by_shape` bucket key(s) a *checked* receiver
/// `TyKind` corresponds to — the lookup-side counterpart to
/// `hir::package::classify_impl_shape`'s index-side classification of an
/// impl's own (unchecked) self-type `TypeExprKind`. Both sides must agree
/// on the same key for a given shape, or a real impl silently becomes
/// unreachable from method/associated-item candidate search.
///
/// Returns more than one key only for the one representational collision
/// in this compiler's checked-`Ty` system: `str` and `[u8]`-ish slices
/// both check to the identical `TyKind::Slice(Box::new(Ty::uint(U8)))`
/// shape (see `primitive_ty`'s own `TypePrimitive::String` arm) — there is
/// no way to tell them apart once a value has reached this checked `Ty`
/// form, so both the `"[]"` and `"str"` buckets are checked rather than
/// risking one of them going silently unreachable.
///
/// Returns `None` for receiver kinds this compiler has no concrete-impl
/// dispatch shape for at all (closures, generators, trait objects, `dyn`
/// existentials, ...) — those still get checked against `blanket_impls`
/// by every caller, which is the only class of impl that could apply to
/// them; there is deliberately no broader fallback here (see
/// `method_output_at`'s own doc comment for why a receiver landing here
/// with `None` is expected, not a bug — unlike an *impl* whose self-type
/// can't be classified at index time, which is a bug and is flagged via
/// `HirPackage::diagnostics` at that point instead).
pub(super) fn ty_shape_keys(kind: &TyKind) -> Option<Vec<&'static str>> {
    Some(match kind {
        TyKind::Bool => vec!["bool"],
        TyKind::Char => vec!["char"],
        TyKind::Int(int) => vec![match int {
            ty::IntTy::I8 => "i8",
            ty::IntTy::I16 => "i16",
            ty::IntTy::I32 => "i32",
            ty::IntTy::I64 => "i64",
            ty::IntTy::I128 => "i128",
            ty::IntTy::Isize => "isize",
        }],
        TyKind::Uint(uint) => vec![match uint {
            ty::UintTy::U8 => "u8",
            ty::UintTy::U16 => "u16",
            ty::UintTy::U32 => "u32",
            ty::UintTy::U64 => "u64",
            ty::UintTy::U128 => "u128",
            ty::UintTy::Usize => "usize",
        }],
        TyKind::Float(float) => vec![match float {
            ty::FloatTy::F16 => "f16",
            ty::FloatTy::F32 => "f32",
            ty::FloatTy::F64 => "f64",
            ty::FloatTy::F128 => "f128",
        }],
        TyKind::Slice(_) => vec!["[]", "str"],
        TyKind::Array(_, _) => vec!["[;N]"],
        TyKind::Tuple(elements) if elements.is_empty() => vec!["()"],
        TyKind::Tuple(_) => vec!["(,)"],
        TyKind::Ref(_, _, ty::Mutability::Not) => vec!["&"],
        TyKind::Ref(_, _, ty::Mutability::Mut) => vec!["&mut"],
        TyKind::RawPtr(pointee) => vec![match pointee.mutbl {
            ty::Mutability::Not => "*const",
            ty::Mutability::Mut => "*mut",
        }],
        TyKind::FnPtr(_) => vec!["fn(..)"],
        TyKind::Never => vec!["!"],
        _ => return None,
    })
}

/// The bounded, indexed candidate list for a receiver that isn't a
/// resolved ADT (`hir::HirProgram::impls_for_adt`'s domain) — every impl
/// whose self-type shares `receiver_kind`'s own shape (`ty_shape_keys`),
/// plus every blanket impl (`impl<T> Trait for T`, which must be checked
/// regardless of shape). Deliberately never falls back to scanning every
/// impl in the workspace: a receiver kind `ty_shape_keys` has no bucket
/// for (closures, generators, `dyn` trait objects, ...) just gets the
/// blanket-impl list alone, which is the only class of impl that could
/// possibly apply to it anyway.
pub(super) fn shape_and_blanket_candidates<'a>(
    program: &'a hir::HirProgram,
    receiver_kind: &TyKind,
) -> impl Iterator<Item = &'a hir::Item> {
    ty_shape_keys(receiver_kind)
        .into_iter()
        .flatten()
        .flat_map(move |key| program.impls_for_shape(key))
        .chain(program.blanket_impls())
}

pub(super) fn primitive_path_ty(name: &str) -> Option<Ty> {
    Some(match name {
        "bool" => Ty::bool(),
        "char" => Ty::char(),
        "i8" => Ty::int(ty::IntTy::I8),
        "i16" => Ty::int(ty::IntTy::I16),
        "i32" => Ty::int(ty::IntTy::I32),
        "i64" => Ty::int(ty::IntTy::I64),
        "i128" => Ty::int(ty::IntTy::I128),
        "isize" => Ty::int(ty::IntTy::Isize),
        "u8" => Ty::uint(ty::UintTy::U8),
        "u16" => Ty::uint(ty::UintTy::U16),
        "u32" => Ty::uint(ty::UintTy::U32),
        "u64" => Ty::uint(ty::UintTy::U64),
        "u128" => Ty::uint(ty::UintTy::U128),
        "usize" => Ty::uint(ty::UintTy::Usize),
        "f16" => Ty::float(ty::FloatTy::F16),
        "f32" => Ty::float(ty::FloatTy::F32),
        "f64" => Ty::float(ty::FloatTy::F64),
        "f128" => Ty::float(ty::FloatTy::F128),
        "str" => Ty {
            kind: TyKind::Slice(Box::new(Ty::uint(ty::UintTy::U8))),
        },
        _ => return None,
    })
}

/// Converts the subset of `ast::Ty` that `TypeBuilder`'s intrinsics
/// (`ComptimeOp::CreateStruct`/`AddField`, `fp-interpret`) can actually
/// produce for a field's type — primitives and references to them — into
/// the checked `hir::ty::Ty` shape `field_ty` needs. Anything else (a
/// nested/generic comptime-constructed field type) is out of scope, per
/// this feature's stated scope, and returns `None` rather than guessing.
pub(super) fn ast_value_ty_to_hir_ty(ty: &fp_core::ast::Ty) -> Option<Ty> {
    match ty {
        fp_core::ast::Ty::Primitive(primitive) => Some(primitive_ty(*primitive)),
        fp_core::ast::Ty::Reference(reference) => {
            ast_value_ty_to_hir_ty(&reference.ty).map(|inner| Ty {
                kind: TyKind::Ref(ty::Region::ReStatic, Box::new(inner), ty::Mutability::Not),
            })
        }
        _ => None,
    }
}

pub(super) fn primitive_ty(primitive: TypePrimitive) -> Ty {
    match primitive {
        TypePrimitive::Bool => Ty::bool(),
        TypePrimitive::Char => Ty::char(),
        TypePrimitive::Int(int) => match int {
            TypeInt::I8 => Ty::int(ty::IntTy::I8),
            TypeInt::I16 => Ty::int(ty::IntTy::I16),
            TypeInt::I32 => Ty::int(ty::IntTy::I32),
            TypeInt::I64 => Ty::int(ty::IntTy::I64),
            TypeInt::I128 => Ty::int(ty::IntTy::I128),
            TypeInt::U8 => Ty::uint(ty::UintTy::U8),
            TypeInt::U16 => Ty::uint(ty::UintTy::U16),
            TypeInt::U32 => Ty::uint(ty::UintTy::U32),
            TypeInt::U64 => Ty::uint(ty::UintTy::U64),
            TypeInt::U128 => Ty::uint(ty::UintTy::U128),
            TypeInt::BigInt => Ty::int(ty::IntTy::I128),
        },
        TypePrimitive::Decimal(decimal) => Ty::float(match decimal {
            DecimalType::F32 => ty::FloatTy::F32,
            _ => ty::FloatTy::F64,
        }),
        TypePrimitive::String => Ty {
            kind: TyKind::Slice(Box::new(Ty::uint(ty::UintTy::U8))),
        },
        TypePrimitive::List => Ty {
            kind: TyKind::Slice(Box::new(Ty::never())),
        },
    }
}
