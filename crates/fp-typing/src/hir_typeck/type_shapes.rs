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
        TyKind::Array(inner, length) => ty_contains_param(inner) || const_contains_param(length),
        TyKind::Tuple(tys) => tys.iter().any(|ty| ty_contains_param(ty)),
        TyKind::Adt(_, args) => args.iter().any(|arg| match arg {
            GenericArg::Type(ty) => ty_contains_param(ty),
            GenericArg::Const(constant) => const_contains_param(constant),
            GenericArg::Lifetime(_) => false,
        }),
        TyKind::FnDef(_, args)
        | TyKind::Closure(_, args)
        | TyKind::Generator(_, args, _)
        | TyKind::Opaque(_, args) => args.iter().any(generic_arg_contains_param),
        TyKind::GeneratorWitness(tys) => tys.iter().any(|ty| ty_contains_param(ty)),
        TyKind::Projection(projection) => projection.substs.iter().any(generic_arg_contains_param),
        TyKind::Dynamic(predicates, _) => predicates.iter().any(|predicate| match predicate {
            ty::ExistentialPredicate::Trait(trait_ref) => {
                trait_ref.substs.iter().any(generic_arg_contains_param)
            }
            ty::ExistentialPredicate::Projection(projection) => {
                projection.substs.iter().any(generic_arg_contains_param)
                    || match &projection.term {
                        ty::Term::Ty(ty) => ty_contains_param(ty),
                        ty::Term::Const(_) => false,
                    }
            }
            ty::ExistentialPredicate::AutoTrait(_) => false,
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

fn generic_arg_contains_param(arg: &GenericArg) -> bool {
    match arg {
        GenericArg::Type(ty) => ty_contains_param(ty),
        GenericArg::Const(_) | GenericArg::Lifetime(_) => false,
    }
}

fn const_contains_param(constant: &ty::ConstKind) -> bool {
    match constant {
        ty::ConstKind::Param(_) => true,
        ty::ConstKind::Unevaluated(value) => value.substs.iter().any(generic_arg_contains_param),
        _ => false,
    }
}

pub(super) fn ty_contains_error(ty: &Ty) -> bool {
    match &ty.kind {
        TyKind::Error(_) => true,
        TyKind::Ref(_, inner, _) => ty_contains_error(inner),
        TyKind::RawPtr(value) => ty_contains_error(&value.ty),
        TyKind::Slice(inner) => ty_contains_error(inner),
        TyKind::Array(inner, length) => ty_contains_error(inner) || const_contains_error(length),
        TyKind::Tuple(tys) => tys.iter().any(|ty| ty_contains_error(ty)),
        TyKind::Adt(_, args) => args.iter().any(|arg| match arg {
            GenericArg::Type(ty) => ty_contains_error(ty),
            GenericArg::Const(constant) => const_contains_error(constant),
            GenericArg::Lifetime(_) => false,
        }),
        TyKind::FnDef(_, args)
        | TyKind::Closure(_, args)
        | TyKind::Generator(_, args, _)
        | TyKind::Opaque(_, args) => args.iter().any(generic_arg_contains_error),
        TyKind::GeneratorWitness(tys) => tys.iter().any(|ty| ty_contains_error(ty)),
        TyKind::Projection(projection) => projection.substs.iter().any(generic_arg_contains_error),
        TyKind::Dynamic(predicates, _) => predicates.iter().any(|predicate| match predicate {
            ty::ExistentialPredicate::Trait(trait_ref) => {
                trait_ref.substs.iter().any(generic_arg_contains_error)
            }
            ty::ExistentialPredicate::Projection(projection) => {
                projection.substs.iter().any(generic_arg_contains_error)
                    || match &projection.term {
                        ty::Term::Ty(ty) => ty_contains_error(ty),
                        ty::Term::Const(constant) => {
                            matches!(constant, ty::ConstKind::Error(_))
                        }
                    }
            }
            ty::ExistentialPredicate::AutoTrait(_) => false,
        }),
        TyKind::FnPtr(signature) => {
            signature
                .binder
                .value
                .inputs
                .iter()
                .any(|ty| ty_contains_error(ty))
                || ty_contains_error(&signature.binder.value.output)
        }
        _ => false,
    }
}

fn generic_arg_contains_error(arg: &GenericArg) -> bool {
    match arg {
        GenericArg::Type(ty) => ty_contains_error(ty),
        GenericArg::Const(constant) => matches!(constant, ty::ConstKind::Error(_)),
        GenericArg::Lifetime(_) => false,
    }
}

fn const_contains_error(constant: &ty::ConstKind) -> bool {
    match constant {
        ty::ConstKind::Error(_) => true,
        ty::ConstKind::Unevaluated(value) => value.substs.iter().any(generic_arg_contains_error),
        _ => false,
    }
}

/// The primitive `str` type shared by string literals and literal-string
/// annotations. It remains distinct from `[u8]` despite their similar
/// runtime representation, so inherent method lookup sees the same method
/// sets as rustc.
pub(super) fn str_ty() -> Ty {
    Ty { kind: TyKind::Str }
}

/// The `hir::HirPackage::impls_by_shape` bucket key(s) a *checked* receiver
/// `TyKind` corresponds to — the lookup-side counterpart to
/// `hir::package::classify_impl_shape`'s index-side classification of an
/// impl's own (unchecked) self-type `TypeExprKind`. Both sides must agree
/// on the same key for a given shape, or a real impl silently becomes
/// unreachable from method/associated-item candidate search.
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
        TyKind::Str => vec!["str"],
        TyKind::Slice(_) => vec!["[]"],
        // An array reaches slice methods through the explicit built-in
        // array-to-slice adjustment in the method lookup chain. Keep the
        // index faithful to the receiver's actual simplified type so an
        // array impl is considered before the adjusted slice receiver.
        TyKind::Array(_, _) => vec!["[;N]"],
        TyKind::Tuple(elements) if elements.is_empty() => vec!["()"],
        TyKind::Tuple(_) => vec!["(,)"],
        // HIR's `TypeExprKind::Ref` intentionally stores only the referent;
        // mutability is represented on the checked `Ty`, not on the impl
        // header shape.  The index therefore has one reference bucket, as
        // rustc's simplified type does before receiver mutability is checked
        // during candidate matching.  Querying an `&mut` bucket here would
        // make every reference impl unreachable because no such bucket can
        // be produced by the HIR indexer.
        TyKind::Ref(_, _, _) => vec!["&"],
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
) -> Box<dyn Iterator<Item = hir::Item> + 'a> {
    if matches!(receiver_kind, TyKind::Infer(_)) {
        // An inference variable has no rustc `SimplifiedType` yet. Keep the
        // obligation open; selecting candidates by scanning every impl would
        // be a fallback index with no sound fast-reject key and could commit
        // to a method before inference has established the receiver type.
        return Box::new(program.blanket_impls());
    }
    Box::new(
        ty_shape_keys(receiver_kind)
            .into_iter()
            .flatten()
            .flat_map(move |key| program.impls_for_shape(key))
            .chain(program.blanket_impls()),
    )
}

/// Returns the complete indexed method candidate set for a checked receiver.
///
/// Rustc's method lookup starts with the receiver's simplified-type bucket and
/// then adds blanket impls.  Keeping that operation in one helper is
/// important because the signature-probing pass and the final call-resolution
/// pass must see the same candidate set; otherwise a method can be found only
/// after its arguments have already been checked.
pub(super) fn method_candidates<'a>(
    program: &'a hir::HirProgram,
    receiver_kind: &TyKind,
) -> Box<dyn Iterator<Item = hir::Item> + 'a> {
    match receiver_kind {
        TyKind::Adt(receiver, _) => Box::new(
            program
                .impls_for_adt(receiver.did.clone())
                .chain(program.blanket_impls()),
        ),
        _ => shape_and_blanket_candidates(program, receiver_kind),
    }
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
        "str" => str_ty(),
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
        // Surface parsing may preserve a primitive annotation as an
        // identifier expression (`i64`, `str`) rather than as
        // `Ty::Primitive`. TypeBuilder receives exactly those AST type values
        // when a source struct is cloned and extended, so resolve the
        // primitive spelling here before declaring the generated field type
        // invalid.
        fp_core::ast::Ty::Expr(expr) => match expr.kind() {
            fp_core::ast::ExprKind::Name(fp_core::ast::Name::Ident(ident)) => {
                primitive_path_ty(ident.name.as_str())
            }
            fp_core::ast::ExprKind::Name(fp_core::ast::Name::Path(path)) => path
                .segments
                .last()
                .and_then(|segment| primitive_path_ty(segment.ident.as_str())),
            _ => None,
        },
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
        TypePrimitive::String => str_ty(),
        TypePrimitive::List => Ty {
            kind: TyKind::Slice(Box::new(Ty::never())),
        },
    }
}
