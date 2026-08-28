use super::instr::MirToLirLowerer;
use fp_core::mir::ty::{FloatTy, IntTy, Ty, TyKind, TypeAndMut, UintTy};
use fp_core::{lir, mir};

impl MirToLirLowerer {
    pub(super) fn lir_type_from_ty(&self, ty: &Ty) -> lir::LirType {
        match &ty.kind {
            TyKind::Bool => lir::LirType::I1,
            TyKind::Char => lir::LirType::I32,
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => lir::LirType::I8,
                IntTy::I16 => lir::LirType::I16,
                IntTy::I32 => lir::LirType::I32,
                IntTy::I64 => lir::LirType::I64,
                IntTy::I128 => lir::LirType::I128,
                IntTy::Isize => lir::LirType::I64,
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => lir::LirType::I8,
                UintTy::U16 => lir::LirType::I16,
                UintTy::U32 => lir::LirType::I32,
                UintTy::U64 => lir::LirType::I64,
                UintTy::U128 => lir::LirType::I128,
                UintTy::Usize => lir::LirType::I64,
            },
            TyKind::Float(float_ty) => match float_ty {
                // `LirType`/downstream backends (LLVM, JVM, bytecode) only
                // model f32/f64 storage; f16/f128 lower lossily to their
                // nearest supported width rather than failing codegen.
                // Full native f16/f128 codegen is out of scope here — see
                // HIR-level `hir::ty::FloatTy`, which keeps the precise
                // width through typechecking.
                FloatTy::F16 => lir::LirType::F32,
                FloatTy::F32 => lir::LirType::F32,
                FloatTy::F64 => lir::LirType::F64,
                FloatTy::F128 => lir::LirType::F64,
            },
            TyKind::Tuple(elements) if elements.is_empty() => lir::LirType::Void,
            TyKind::Tuple(elements) => lir::LirType::Struct {
                fields: elements
                    .iter()
                    .map(|elem| self.lir_type_from_ty(elem))
                    .collect(),
                packed: false,
                name: None,
            },
            TyKind::Array(element_ty, len) => lir::LirType::Array(
                Box::new(self.lir_type_from_ty(element_ty)),
                self.array_length_from_const(len),
            ),
            TyKind::Slice(element_ty) => {
                let elem_lir = self.lir_type_from_ty(element_ty);
                self.slice_lir_type(&elem_lir)
            }
            TyKind::Ref(_, inner, _) => {
                if let Some(elem_ty) = Self::slice_ref_element_ty(inner) {
                    let elem_lir = self.lir_type_from_ty(elem_ty);
                    self.slice_lir_type(&elem_lir)
                } else if matches!(&inner.kind, TyKind::Tuple(fields)
                    if fields.is_empty()
                        || (fields.len() == 1
                            && matches!(fields[0].kind, TyKind::RawPtr(_))))
                {
                    // Some std wrapper declarations are structurally
                    // normalized before MIR lowering, so their one-pointer
                    // representation arrives as a tuple rather than an
                    // ADT. Preserve the same thin-pointer ABI here.
                    lir::LirType::Ptr(Box::new(lir::LirType::I8))
                } else if let TyKind::Adt(adt, substs) = &inner.kind {
                    // A reference to a struct that's really just an opaque/
                    // extern-style pointer wrapper — either genuinely empty
                    // (zero fields), or a single-field newtype whose one
                    // field is itself a pointer (e.g. `&std::ffi::CStr`,
                    // `pub struct CStr { ptr: *const char }`) — is not a
                    // real, independently-sized value to point *at*, unlike
                    // Rust's own `&CStr` (an unsized type: a thin pointer
                    // directly at the C string's bytes, not a pointer to a
                    // struct that itself holds a pointer). Lowering it the
                    // normal way gives `Ptr(Struct{fields:[Ptr(I8)]})` (or
                    // `Ptr(Struct{fields:[]})` for the empty case), a
                    // pointer to a wrapper — but the actual value flowing
                    // through this reference (e.g. a `c"..."` literal's own
                    // constant, materialized as a bare `Ptr(I8)`) is the
                    // pointer itself, not a pointer to a boxed pointer.
                    // Treat both shapes as a bare pointer, matching how
                    // this backend already represents other raw/opaque
                    // pointers.
                    let cached_opaque_wrapper = self
                        .struct_layouts
                        .borrow()
                        .get(&(adt.did.clone(), Self::adt_substs_types(substs)))
                        .map(|fields| {
                            fields.is_empty()
                                || (fields.len() == 1
                                    && matches!(fields[0], Some(lir::LirType::Ptr(_))))
                        })
                        .unwrap_or(false);
                    // Reference conversion can run before the per-ADT
                    // layout cache has been populated, and a cached layout
                    // may still have lost the source-level wrapper shape.
                    // Consult the typed definition as well so opaque
                    // pointer wrappers such as `CStr` retain their
                    // thin-pointer ABI in every lowering phase.
                    let definition_is_opaque_wrapper =
                        adt.variants.first().is_some_and(|variant| {
                            variant.fields.is_empty()
                                || (variant.fields.len() == 1
                                    && matches!(variant.fields[0].ty.kind, TyKind::RawPtr(_)))
                        });
                    let is_opaque_wrapper = cached_opaque_wrapper || definition_is_opaque_wrapper;
                    if is_opaque_wrapper {
                        lir::LirType::Ptr(Box::new(lir::LirType::I8))
                    } else {
                        lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner)))
                    }
                } else {
                    lir::LirType::Ptr(Box::new(self.lir_type_from_ty(inner)))
                }
            }
            TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                // A raw pointer's pointee is ABI-irrelevant. Generic null
                // pointer constants can retain the source-level `T` after
                // monomorphization, so do not require that phantom type to
                // have a concrete LIR representation.
                let pointee = if self.contains_unresolved_param(inner) {
                    lir::LirType::I8
                } else {
                    self.lir_type_from_ty(inner)
                };
                lir::LirType::Ptr(Box::new(pointee))
            }
            // An opaque enum-payload-slot placeholder (`HirToMirLowerer::
            // opaque_ty`, minted for a slot where variants disagree on the
            // payload type) has a synthetic `DefId` matching nothing in
            // `struct_layouts`/`full_layouts`/`adt_defs` — it was never a
            // real struct/enum, just a byte count for
            // whichever variant's payload is actually stored there at
            // runtime. Recognized by its single synthetic variant's ident,
            // the same name `opaque_payload_sizes` is keyed by.
            TyKind::Adt(adt, _)
                if adt.variants.first().is_some_and(|variant| {
                    self.lookup_opaque_payload_size(variant.ident.as_str())
                        .is_some()
                }) =>
            {
                let size = self
                    .lookup_opaque_payload_size(adt.variants[0].ident.as_str())
                    .expect("checked by this arm's own guard");
                lir::LirType::Array(Box::new(lir::LirType::I8), size)
            }
            TyKind::Adt(adt, substs)
                if self
                    .struct_layouts
                    .borrow()
                    .contains_key(&(adt.did.clone(), Self::adt_substs_types(substs))) =>
            {
                let key = (adt.did.clone(), Self::adt_substs_types(substs));
                let fields = self.struct_layouts.borrow().get(&key).unwrap().clone();
                lir::LirType::Struct {
                    fields: fields
                        .iter()
                        .map(|field| {
                            field.clone().unwrap_or_else(|| {
                                panic!(
                                    "MIR-to-LIR ICE: missing layout for field of ADT {}",
                                    adt.did
                                )
                            })
                        })
                        .collect(),
                    packed: false,
                    name: None,
                }
            }
            TyKind::Adt(adt, substs) => {
                let key = (adt.did.clone(), Self::adt_substs_types(substs));
                // `full_layouts` is an exact-instantiation cache (keyed by
                // `(DefId, substs)`, like `struct_layouts` above) — when
                // this exact instantiation has already been computed
                // elsewhere, reuse it directly.
                if let Some(field_tys) = self.lookup_full_layout(&key) {
                    // Mirror the cache-miss guard below: a cached entry can
                    // only be poisoned this way if it was produced by a
                    // no-context fallback that deliberately manufactures
                    // placeholders (e.g. a layout-for-display helper with
                    // no real instantiation to substitute) rather than a
                    // genuine instantiation. Reusing it here would
                    // otherwise recurse into `lir_type_from_ty` on the
                    // unresolved field and panic several frames deeper
                    // with only the bare field `Ty` to go on — fail right
                    // here instead, attributing it to the exact ADT/substs
                    // this cache entry came from.
                    if field_tys.iter().any(|ty| {
                        matches!(
                            ty.kind,
                            TyKind::Infer(_) | TyKind::Error(_) | TyKind::Param(_)
                        )
                    }) {
                        panic!(
                            "MIR-to-LIR ICE: cached layout for {} (substs {:?}) contains an unresolved field type: {:?}",
                            adt.did, substs, field_tys
                        );
                    }
                    let fields: Vec<Option<lir::LirType>> = field_tys
                        .iter()
                        .map(|ty| Some(self.lir_type_from_ty(ty)))
                        .collect();
                    let struct_fields: Vec<lir::LirType> =
                        fields.iter().map(|f| f.clone().unwrap()).collect();
                    self.struct_layouts.borrow_mut().insert(key, fields);
                    return lir::LirType::Struct {
                        fields: struct_fields,
                        packed: false,
                        name: None,
                    };
                }
                // Otherwise, compute it — the same way rustc's own
                // `layout_of` always does (`tcx.type_of(field.did)
                // .instantiate(tcx, args)`), instead of reusing a
                // *different* instantiation's already-substituted fields.
                // `lookup_adt_def` returns the struct's real, registered
                // declaration (`finalize_adt_definitions` populates
                // `AdtDef.variants[0].fields[i].ty` with the *generic*,
                // unsubstituted field types — the same for every
                // instantiation, unlike `struct_layouts`/`full_layouts`),
                // so substituting its `Param`s with this call's own
                // `substs` via `instantiate_ty` gives the correct fields
                // for *this* instantiation specifically, computed on
                // demand and cached for reuse. There is deliberately no
                // further fallback beyond this: a `DefId` `lookup_adt_def`
                // has never even heard of is a genuine "this type is
                // unknown" error, not something to guess an answer for.
                if substs.iter().any(|arg| {
                    matches!(
                        arg,
                        mir::ty::GenericArg::Type(ty) if matches!(ty.kind, TyKind::Infer(_))
                    )
                }) {
                    panic!(
                        "MIR-to-LIR ICE: unresolved ADT substitution for {}: {:?}",
                        adt.did, ty
                    );
                }
                if let Some(populated) = self.lookup_adt_def(&adt.did) {
                    if let Some(variant) = populated.variants.first() {
                        let fields: Vec<Option<lir::LirType>> = variant
                            .fields
                            .iter()
                            .map(|f| {
                                Some(self.lir_type_from_ty(&Self::instantiate_ty(&f.ty, substs)))
                            })
                            .collect();
                        let struct_fields: Vec<lir::LirType> =
                            fields.iter().map(|f| f.clone().unwrap()).collect();
                        self.struct_layouts.borrow_mut().insert(key, fields);
                        return lir::LirType::Struct {
                            fields: struct_fields,
                            packed: false,
                            name: None,
                        };
                    }
                }
                panic!(
                    "MIR-to-LIR ICE: unknown ADT {} — never registered by any compiled package",
                    adt.did
                )
            }
            TyKind::FnDef(def_id, substs) => panic!(
                "MIR-to-LIR ICE: function definition {} with substitutions {:?} used as a data type",
                def_id, substs
            ),
            // An immutable handle into the comptime interpreter's own type
            // pool — not a plain integer, so it can't be a scalar int/float
            // destination (the generic "runtime value conversion" coercion
            // path has no rule for boxing one, and shouldn't need one:
            // every real operation on a `type` value is a dedicated
            // `ComptimeOp` LIR instruction, never ordinary arithmetic).
            // `Ptr(Void)` is exactly the shape `fp-interpret`'s own
            // `Value::Type` storage already expects (its `encode_storage_word`
            // auto-boxes into the object table whenever the destination is
            // `Ptr(_)` or an aggregate), so this is what makes a `type`-typed
            // struct field/local/return value round-trip correctly.
            TyKind::Type => lir::LirType::Ptr(Box::new(lir::LirType::Void)),
            // `any` — a fixed, concrete, fully type-erased value. Same
            // storage strategy as `TyKind::Type`: always boxed/pointer-sized,
            // never a scalar destination — see `TyKind::Any`'s own doc
            // comment for why this must be handled here rather than falling
            // into the `Infer`/`Param`/... "unresolved" panic arm below.
            TyKind::Any => lir::LirType::Ptr(Box::new(lir::LirType::Void)),
            TyKind::Dynamic(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Opaque(_, _)
            | TyKind::Param(_)
            | TyKind::Bound(_, _)
            | TyKind::Placeholder(_)
            | TyKind::Infer(_)
            | TyKind::Error(_) => {
                panic!("MIR-to-LIR ICE: unsupported unresolved type in typed MIR: {ty:?}")
            }
            TyKind::Never => lir::LirType::Void,
            TyKind::FnPtr(poly_fn_sig) => {
                let fn_sig = &poly_fn_sig.binder.value;
                lir::LirType::Ptr(Box::new(lir::LirType::Function {
                    return_type: Box::new(self.lir_type_from_ty(&fn_sig.output)),
                    param_types: fn_sig
                        .inputs
                        .iter()
                        .map(|ty| self.lir_type_from_ty(ty))
                        .collect(),
                    is_variadic: fn_sig.c_variadic,
                }))
            }
        }
    }
}
