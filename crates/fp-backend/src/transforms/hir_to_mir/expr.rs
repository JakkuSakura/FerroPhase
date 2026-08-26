// HIR→MIR lowering implementation (moved from mod.rs)
// This file currently contains the full original implementation and will be
// gradually split into stmt/control_flow/types/borrow submodules.

// BEGIN ORIGINAL CONTENT
use fp_core::ast::{
    DecimalType, TypeBinaryOpKind, TypeInt, TypePrimitive, Value, ValueList, ValueMap, ValueTuple,
};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::hir::place::{
    HirAssignTargetBase, HirAssignTargetProjection, project_hir_assign_target,
};

pub(crate) use super::call_args::values as call_arg_values;
use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir::ty::{
    AdtDef, AdtFlags, ConstKind, ConstValue, CtorKind, ErrorGuaranteed, FloatTy, GenericArg, IntTy,
    Mutability, ReprFlags, ReprOptions, Scalar, ScalarInt, Ty, TyKind, TypeAndMut, UintTy,
    VariantDef, VariantDiscr,
};
use fp_core::mir::{
    self, ConstInfo, EnumDefinition, EnumLayout, EnumLayoutKey, EnumVariantDef, EnumVariantInfo,
    FunctionSpecializationInfo, MethodContext, MethodDefinition, MethodHirRef, MethodLoweringInfo,
    StructDefinition, StructFieldDef, StructLayout, StructLayoutKey, StructuralLayoutKey, Symbol,
};
use fp_core::ops::format_value_with_spec;
use fp_core::span::Span;
use std::collections::{HashMap, HashSet, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};

use super::body::BodyBuilder;

const DIAGNOSTIC_CONTEXT: &str = "hir→mir";

fn lower_hir_ty(ty: &hir::ty::Ty) -> Result<Ty> {
    pub(super) fn lower_const(value: &hir::ty::ConstKind) -> Result<mir::ty::ConstKind> {
        Ok(match value {
            hir::ty::ConstKind::Infer(hir::ty::InferConst::Fresh(id)) => {
                mir::ty::ConstKind::Infer(mir::ty::InferConst::Fresh(*id))
            }
            hir::ty::ConstKind::Infer(hir::ty::InferConst::Var(_)) => {
                return Err(fp_core::error::Error::from(
                    "unsupported HIR const inference variable in MIR type bridge",
                ));
            }
            hir::ty::ConstKind::Value(value) => mir::ty::ConstKind::Value(match value {
                hir::ty::ConstValue::Scalar(scalar) => mir::ty::ConstValue::Scalar(match scalar {
                    hir::ty::Scalar::Int(value) => mir::ty::Scalar::Int(mir::ty::ScalarInt {
                        data: value.data,
                        size: value.size,
                    }),
                    hir::ty::Scalar::Ptr(pointer) => mir::ty::Scalar::Ptr(mir::ty::Pointer {
                        alloc_id: mir::ty::AllocId(pointer.alloc_id.0),
                        offset: mir::ty::Size {
                            bytes: pointer.offset.bytes,
                        },
                    }),
                }),
                hir::ty::ConstValue::ZeroSized => mir::ty::ConstValue::ZeroSized,
                hir::ty::ConstValue::Slice { data, start, end } => mir::ty::ConstValue::Slice {
                    data: data.clone(),
                    start: *start,
                    end: *end,
                },
                hir::ty::ConstValue::ByRef { alloc, offset } => mir::ty::ConstValue::ByRef {
                    alloc: mir::ty::AllocId(alloc.0),
                    offset: mir::ty::Size {
                        bytes: offset.bytes,
                    },
                },
            }),
            hir::ty::ConstKind::Error(error) => {
                mir::ty::ConstKind::Error(mir::ty::ErrorGuaranteed { index: error.index })
            }
            hir::ty::ConstKind::Param(_)
            | hir::ty::ConstKind::Bound(_, _)
            | hir::ty::ConstKind::Placeholder(_)
            | hir::ty::ConstKind::Unevaluated(_) => {
                return Err(fp_core::error::Error::from(
                    "unsupported HIR const kind in MIR type bridge",
                ));
            }
        })
    }

    pub(super) fn lower_arg(arg: &hir::ty::GenericArg) -> Result<mir::ty::GenericArg> {
        Ok(match arg {
            hir::ty::GenericArg::Type(ty) => mir::ty::GenericArg::Type(lower_hir_ty(ty)?),
            hir::ty::GenericArg::Const(value) => mir::ty::GenericArg::Const(lower_const(value)?),
            hir::ty::GenericArg::Lifetime(_) => {
                mir::ty::GenericArg::Lifetime(mir::ty::Region::ReErased)
            }
        })
    }

    let kind = match &ty.kind {
        hir::ty::TyKind::Bool => TyKind::Bool,
        hir::ty::TyKind::Char => TyKind::Char,
        hir::ty::TyKind::Int(value) => TyKind::Int(match value {
            hir::ty::IntTy::Isize => IntTy::Isize,
            hir::ty::IntTy::I8 => IntTy::I8,
            hir::ty::IntTy::I16 => IntTy::I16,
            hir::ty::IntTy::I32 => IntTy::I32,
            hir::ty::IntTy::I64 => IntTy::I64,
            hir::ty::IntTy::I128 => IntTy::I128,
        }),
        hir::ty::TyKind::Uint(value) => TyKind::Uint(match value {
            hir::ty::UintTy::Usize => UintTy::Usize,
            hir::ty::UintTy::U8 => UintTy::U8,
            hir::ty::UintTy::U16 => UintTy::U16,
            hir::ty::UintTy::U32 => UintTy::U32,
            hir::ty::UintTy::U64 => UintTy::U64,
            hir::ty::UintTy::U128 => UintTy::U128,
        }),
        hir::ty::TyKind::Float(value) => TyKind::Float(match value {
            hir::ty::FloatTy::F16 => FloatTy::F16,
            hir::ty::FloatTy::F32 => FloatTy::F32,
            hir::ty::FloatTy::F64 => FloatTy::F64,
            hir::ty::FloatTy::F128 => FloatTy::F128,
        }),
        hir::ty::TyKind::Adt(def, args) => TyKind::Adt(
            AdtDef {
                did: def.did.clone(),
                variants: def
                    .variants
                    .iter()
                    .map(|variant| VariantDef {
                        def_id: variant.def_id.clone(),
                        ctor_def_id: variant.ctor_def_id.clone(),
                        ident: variant.ident.clone().into(),
                        discr: match variant.discr {
                            hir::ty::VariantDiscr::Relative(value) => VariantDiscr::Relative(value),
                            hir::ty::VariantDiscr::Explicit(ref value) => {
                                VariantDiscr::Explicit(value.clone())
                            }
                        },
                        fields: Vec::new(),
                        ctor_kind: match variant.ctor_kind {
                            hir::ty::CtorKind::Fn => CtorKind::Fn,
                            hir::ty::CtorKind::Const => CtorKind::Const,
                            hir::ty::CtorKind::Fictive => CtorKind::Fictive,
                        },
                        is_recovered: variant.is_recovered,
                    })
                    .collect(),
                flags: AdtFlags::from_bits_retain(def.flags.bits()),
                repr: ReprOptions {
                    int: def.repr.int.map(|value| match value {
                        hir::ty::IntegerType::Pointer(value) => {
                            mir::ty::IntegerType::Pointer(value)
                        }
                        hir::ty::IntegerType::Fixed(value, signed) => mir::ty::IntegerType::Fixed(
                            match value {
                                hir::ty::Integer::I8 => mir::ty::Integer::I8,
                                hir::ty::Integer::I16 => mir::ty::Integer::I16,
                                hir::ty::Integer::I32 => mir::ty::Integer::I32,
                                hir::ty::Integer::I64 => mir::ty::Integer::I64,
                                hir::ty::Integer::I128 => mir::ty::Integer::I128,
                            },
                            signed,
                        ),
                    }),
                    align: def
                        .repr
                        .align
                        .map(|value| mir::ty::Align { pow2: value.pow2 }),
                    pack: def
                        .repr
                        .pack
                        .map(|value| mir::ty::Align { pow2: value.pow2 }),
                    flags: mir::ty::ReprFlags::from_bits_retain(def.repr.flags.bits()),
                    field_shuffle_seed: def.repr.field_shuffle_seed,
                },
            },
            args.iter().map(lower_arg).collect::<Result<Vec<_>>>()?,
        ),
        hir::ty::TyKind::Array(inner, length) => {
            TyKind::Array(Box::new(lower_hir_ty(inner)?), lower_const(length)?)
        }
        hir::ty::TyKind::Slice(inner) => TyKind::Slice(Box::new(lower_hir_ty(inner)?)),
        hir::ty::TyKind::RawPtr(value) => TyKind::RawPtr(TypeAndMut {
            ty: Box::new(lower_hir_ty(&value.ty)?),
            mutbl: match value.mutbl {
                hir::ty::Mutability::Mut => Mutability::Mut,
                hir::ty::Mutability::Not => Mutability::Not,
            },
        }),
        hir::ty::TyKind::Ref(_, inner, mutbl) => TyKind::Ref(
            mir::ty::Region::ReErased,
            Box::new(lower_hir_ty(inner)?),
            match mutbl {
                hir::ty::Mutability::Mut => Mutability::Mut,
                hir::ty::Mutability::Not => Mutability::Not,
            },
        ),
        hir::ty::TyKind::FnPtr(signature) => TyKind::FnPtr(mir::ty::PolyFnSig {
            binder: mir::ty::Binder {
                value: mir::ty::FnSig {
                    inputs: signature
                        .binder
                        .value
                        .inputs
                        .iter()
                        .map(|ty| lower_hir_ty(ty).map(Box::new))
                        .collect::<Result<Vec<_>>>()?,
                    output: Box::new(lower_hir_ty(&signature.binder.value.output)?),
                    c_variadic: signature.binder.value.c_variadic,
                    unsafety: match signature.binder.value.unsafety {
                        hir::ty::Unsafety::Unsafe => mir::ty::Unsafety::Unsafe,
                        hir::ty::Unsafety::Normal => mir::ty::Unsafety::Normal,
                    },
                    abi: mir::ty::Abi::Rust,
                },
                bound_vars: Vec::new(),
            },
        }),
        hir::ty::TyKind::FnDef(def, args) => TyKind::FnDef(
            def.clone(),
            args.iter().map(lower_arg).collect::<Result<Vec<_>>>()?,
        ),
        hir::ty::TyKind::Opaque(def, args) => TyKind::Opaque(
            def.clone(),
            args.iter().map(lower_arg).collect::<Result<Vec<_>>>()?,
        ),
        hir::ty::TyKind::Never => TyKind::Never,
        hir::ty::TyKind::Tuple(items) => TyKind::Tuple(
            items
                .iter()
                .map(|item| lower_hir_ty(item).map(Box::new))
                .collect::<Result<Vec<_>>>()?,
        ),
        hir::ty::TyKind::Param(param) => TyKind::Param(mir::ty::ParamTy {
            index: param.index,
            name: param.name.clone().into(),
        }),
        hir::ty::TyKind::Infer(hir::ty::InferTy::FreshTy(id)) => {
            TyKind::Infer(mir::ty::InferTy::FreshTy(*id))
        }
        hir::ty::TyKind::Infer(_) => {
            return Err(fp_core::error::Error::from(
                "unsupported HIR inference variable in MIR type bridge",
            ));
        }
        hir::ty::TyKind::Error(_) => {
            return Err(fp_core::error::Error::from(
                "cannot lower an HIR error type into MIR",
            ));
        }
        hir::ty::TyKind::Type => TyKind::Type,
        hir::ty::TyKind::Any => TyKind::Any,
        _ => {
            return Err(fp_core::error::Error::from(
                "unsupported HIR type in MIR type bridge",
            ));
        }
    };
    Ok(Ty { kind })
}

/// Minimal HIR → MIR lowering pass.
///
/// This currently produces skeletal MIR that is sufficient to feed the
/// downstream MIR→LIR/LLVM pipeline. Unsupported constructs surface diagnostics
/// so callers can decide whether to abort or continue.
fn assoc_types_from_impl_items(items: &[hir::ImplItem]) -> HashMap<String, hir::TypeExpr> {
    items
        .iter()
        .filter_map(|item| match &item.kind {
            hir::ImplItemKind::AssocType(assoc) => {
                Some((assoc.name.as_str().to_string(), assoc.ty.clone()))
            }
            _ => None,
        })
        .collect()
}

#[derive(Clone)]
pub(crate) struct StructFieldInfo {
    pub(super) name: String,
    pub(super) ty: Ty,
}

#[derive(Clone)]
pub(crate) enum ConstContainerArgs {
    List { elem_ty: Ty },
    Map { key_ty: Ty, value_ty: Ty },
}

pub struct HirToMirLowerer {
    pub(super) diagnostics: DiagnosticManager,
    /// Every struct/enum definition, layout, method table, specialization
    /// cache, const value, and ADT def this instance computes while lowering
    /// the current package — the *exact same* shared handle
    /// `CompilerState`/`MirToLirLowerer` read (`CompilerState::mir_package_rc`),
    /// not a private local copy: every insert here lands directly in the
    /// session's real package as it happens, so a struct/enum layout
    /// computed once is visible immediately (no separate "merge when
    /// lowering finishes" step, and nothing computed here is ever lost or
    /// redundantly recomputed by a later re-lowering of the same package).
    pub(crate) mir_package: std::rc::Rc<std::cell::RefCell<mir::MirPackage>>,
    struct_layouts_in_progress: HashSet<StructLayoutKey>,
    enum_layouts_in_progress: HashSet<EnumLayoutKey>,
    pub(crate) extra_items: Vec<mir::Item>,
    pub(crate) extra_bodies: Vec<(mir::BodyId, mir::Body)>,
    /// Marks a function/method `DefId` whose body has already been lowered
    /// and pushed into `extra_items`/`extra_bodies` (or, for the normal
    /// whole-package pipeline, `mir_program.items`/`.bodies` directly) —
    /// see `ensure_function_lowered`/`ensure_method_lowered`. Neither
    /// `function_sigs` nor `method_lookup_by_def` can serve as this guard:
    /// both are also populated by signature-only registration (the
    /// call-site lazy fallback, the impl signature pre-pass) with no body
    /// ever lowered.
    pub(super) lowered_items: HashSet<hir::DefId>,
    /// Snapshot of the whole-workspace `hir::HirPackage.def_map`/`def_paths`
    /// (local items + every dependency's, via `seed_workspace_definitions`),
    /// taken once at the top of `lower_program`/`transform`. Lets
    /// `compute_adt_layout` look up and lazily register a foreign
    /// struct/enum on demand (O(1) point lookup) instead of every
    /// dependency's ADTs being eagerly duplicated into `program.items`
    /// whether anything here references them or not.
    /// Every *already-published* dependency package's own HIR — `Rc`, not
    /// owned, and never rebuilt/re-scanned here: `transform_comptime_request`
    /// shares the exact same `Rc` its `ComptimeRequest::program` already
    /// is (itself `AstProgram::hir_program()`, incrementally
    /// maintained — see its own doc comment), and `transform`/`lower_program`
    /// (the ordinary whole-package path) fetches it once, the same way.
    /// The current package is always one of this map's own packages —
    /// `new` inserts a fresh empty one if the `package_id` isn't already
    /// present, and `transform` re-inserts the freshly-typechecked package
    /// under the same id — so every lookup method (`hir_item`,
    /// `hir_def_path`, `hir_all_items`) reads straight off this map with no
    /// separate "current package first" fallback.
    pub(super) hir_program: std::rc::Rc<hir::HirProgram>,
    /// The id of the package this instance is currently lowering — its HIR
    /// lives in `hir_program.packages` under this id (`new`/`transform`
    /// insert it there), so all HIR access routes through `hir_program`
    /// with no separate `current_package` handle.
    pub(super) current_package_id: mir::package::PackageId,
    /// Qualified path of whichever item's body/signature is currently
    /// being lowered — set by `ensure_function_lowered`/
    /// `ensure_method_lowered`/`lower_const` right before they start, and
    /// appended to `emit_error`'s "unresolved ... path" diagnostics so a
    /// misattributed root-cause investigation (like the one that found the
    /// `super::super::` import bug, via `fp-typing`'s matching
    /// `current_item_path`) doesn't have to run the whole corpus and
    /// guess. Not exhaustive — lazily-triggered lowering from an unusual
    /// call site may leave this stale or `None` — but covers the ordinary
    /// top-level item-lowering loop in `lower_program`.
    pub(super) current_item_path: Option<String>,
}

impl HirToMirLowerer {
    pub(super) fn generic_function_def(&self, def_id: &hir::DefId) -> Option<hir::Function> {
        self.mir_package
            .borrow()
            .generic_function_defs
            .get(def_id)
            .cloned()
    }

    pub(super) fn enum_variant_def(&self, def_id: &hir::DefId) -> Option<EnumVariantInfo> {
        self.mir_package.borrow().enum_variants.get(def_id).cloned()
    }

    pub(super) fn struct_def(&self, def_id: &hir::DefId) -> Option<StructDefinition> {
        self.mir_package.borrow().struct_defs.get(def_id).cloned()
    }

    pub(super) fn has_enum_def(&self, def_id: &hir::DefId) -> bool {
        self.mir_package.borrow().enum_defs.contains_key(def_id)
    }

    pub(super) fn enum_def(&self, def_id: &hir::DefId) -> Option<EnumDefinition> {
        self.mir_package.borrow().enum_defs.get(def_id).cloned()
    }
    /// `hir_program`/`package_id` are required, not filled in later via a
    /// `with_*` builder or left as an empty/default placeholder — every
    /// real caller already has both on hand at construction time (the
    /// workspace-wide `HirProgram` snapshot it's lowering against, and the
    /// specific package this instance lowers). `current_package_id` is
    /// derived from them immediately; if the package isn't already in
    /// `hir_program` it is inserted as a fresh empty package, so the
    /// current package is always a member of `hir_program` and the lookup
    /// methods can query `hir_program` alone.
    pub fn new(
        mut hir_program: std::rc::Rc<hir::HirProgram>,
        package_id: hir::PackageId,
        mir_package: std::rc::Rc<std::cell::RefCell<mir::MirPackage>>,
    ) -> Self {
        if !hir_program.packages.contains_key(&package_id) {
            let fresh = std::rc::Rc::new(hir::HirPackage::new(package_id.clone()));
            std::rc::Rc::make_mut(&mut hir_program).add_package(fresh);
        }
        Self {
            diagnostics: DiagnosticManager::new(),
            mir_package,
            struct_layouts_in_progress: HashSet::new(),
            enum_layouts_in_progress: HashSet::new(),
            extra_items: Vec::new(),
            extra_bodies: Vec::new(),
            lowered_items: HashSet::new(),
            hir_program,
            current_package_id: package_id,
            current_item_path: None,
        }
    }

    /// Same idea as `typeck_expr_type`, for a `MethodCall` expr's own
    /// resolved callee `DefId` — read straight off `hir_program`, no
    /// lowering step needed.
    pub(crate) fn typeck_method_resolution(&self, hir_id: hir::HirId) -> Option<hir::DefId> {
        self.hir_program.method_resolution(hir_id)
    }

    pub(crate) fn typeck_method_intrinsic(
        &self,
        hir_id: hir::HirId,
    ) -> Option<fp_core::intrinsics::IntrinsicKind> {
        let def_id = self.typeck_method_resolution(hir_id)?;
        match self.hir_program.intrinsic_def(def_id)? {
            fp_core::intrinsics::CallKind::Len => Some(fp_core::intrinsics::IntrinsicKind::Len),
            _ => None,
        }
    }

    pub(crate) fn typeck_reflection_field_intrinsic(
        &self,
        hir_id: hir::HirId,
    ) -> Option<fp_core::intrinsics::IntrinsicKind> {
        self.hir_program.reflection_field_intrinsic(hir_id)
    }

    pub(crate) fn typeck_reflection_field_intrinsic_expr(
        &self,
        expr: &hir::Expr,
    ) -> Option<fp_core::intrinsics::IntrinsicKind> {
        self.typeck_reflection_field_intrinsic(expr.hir_id.clone())
            .or_else(|| {
                self.hir_program
                    .reflection_field_intrinsic_at_span(expr.hir_id.package_id().clone(), expr.span)
            })
    }

    /// Same idea as `typeck_expr_type`, for a `const { .. }` block's
    /// already-resolved comptime value.
    pub(crate) fn typeck_const_block_value(&self, def_id: hir::DefId) -> Option<Value> {
        self.hir_program.const_block_value(def_id)
    }

    /// Point `DefId` lookup straight off `hir_program` — the current package
    /// is always one of `hir_program`'s own packages (see `new`/`transform`,
    /// which both insert it), so there is no separate "current package
    /// first" fallback anymore. Replaces every old direct
    /// `self.hir_def_map.get(def_id)` read.
    pub(crate) fn hir_item(&self, def_id: hir::DefId) -> Option<&hir::Item> {
        self.hir_program.item(def_id)
    }

    /// Same dispatch as `hir_item`, for `def_paths` — used by
    /// `def_path_str`, which every `register_struct`/`register_enum` call
    /// now goes through instead of being handed a whole `def_paths` map.
    pub(super) fn hir_def_path(&self, def_id: hir::DefId) -> Option<&hir::DefPath> {
        self.hir_program.def_path(def_id)
    }

    /// Every item `hir_program` knows about (which always includes
    /// the current package itself) — replaces every old
    /// `self.hir_def_map.values()`/`.iter()` full scan (used to build a
    /// one-time reverse index; never a per-lookup cost).
    pub(crate) fn hir_all_items(&self) -> impl Iterator<Item = &hir::Item> {
        self.hir_program
            .packages
            .values()
            .flat_map(|package| package.all_defs())
    }

    pub fn transform(&mut self, hir_program: hir::HirPackage) -> Result<mir::MirCodeUnit> {
        let hir_program = std::rc::Rc::new(hir_program);
        self.current_package_id = hir_program.id.clone();
        std::rc::Rc::make_mut(&mut self.hir_program).add_package(hir_program.clone());
        let program = self.lower_program(&hir_program)?;
        if self.diagnostics.has_errors() {
            return Err(fp_core::error::Error::from(
                "internal compiler error: HIR-to-MIR lowering reported an error",
            ));
        }
        Ok(program)
    }

    /// Lower HIR through the compiler's asynchronous boundary.
    ///
    /// Generic instance requests are resolved while producing MIR and are
    /// cached by their typed `(DefId, SubstsRef)` identity. Keeping this boundary
    /// async lets the compiler driver own executor progress without making
    /// every recursive expression operation an artificial future.
    pub async fn transform_async(
        &mut self,
        hir_program: hir::HirPackage,
    ) -> Result<mir::MirCodeUnit> {
        self.transform(hir_program)
    }

    pub fn compute_adt_layout(&mut self, def_id: hir::DefId, substs: &[Ty], span: Span) {
        if !self.mir_package.borrow().struct_defs.contains_key(&def_id)
            && !self.mir_package.borrow().enum_defs.contains_key(&def_id)
        {
            self.try_lazily_register_adt(def_id.clone(), span);
        }
        // `def_id` is either a struct or an enum, never both — calling both
        // layout functions regardless of which one it actually is makes the
        // non-matching call spuriously report "definition not registered"
        // for a perfectly valid, correctly-registered type.
        if self.mir_package.borrow().struct_defs.contains_key(&def_id) {
            let _ = self.struct_layout_for_instance(def_id, substs, span);
        } else if self.mir_package.borrow().enum_defs.contains_key(&def_id) {
            let _ = self.enum_layout_for_instance(def_id, substs, span);
        }
    }

    /// Eagerly registers every struct/enum reachable in `hir_def_map` —
    /// every previously-compiled workspace package's own items, via
    /// `seed_workspace_definitions` — before any layout is ever computed.
    /// Mirrors rustc's own model: `AdtDef` collection for every reachable
    /// item happens upfront and unconditionally, with layout resolution
    /// staying a separate, later, lazy/memoized query — there is no
    /// "register on whichever lookup happens to miss first" step in
    /// rustc's pipeline, and that ordering-dependence is exactly what let
    /// `finalize_adt_definitions`'s eager local-layout pass reach a
    /// not-yet-registered dependency type (e.g. `std::alloc::Vec`, needed
    /// by `std::json::Value`'s own layout) before this pass existed.
    ///
    /// `register_enum` evaluates explicit variant discriminants and can
    /// `emit_error` on failure. Registering *every* reachable dependency
    /// enum — not just what this package actually references — risks
    /// surfacing a spurious diagnostic for some unrelated, unused,
    /// possibly-broken enum elsewhere in `std`. Matching rustc's own
    /// split (bare `AdtDef` collection is unconditional and cannot fail;
    /// discriminant evaluation is a separate, still-lazy query fired only
    /// once a type is genuinely used), diagnostics raised purely by this
    /// eager pre-pass are discarded rather than surfaced — a real failure
    /// still surfaces normally the moment something in the current
    /// compile actually uses the offending enum, via the ordinary
    /// `finalize_adt_definitions`/on-demand layout paths below.
    pub(super) fn register_all_dependency_adts(&mut self) {
        let diagnostics_before = self.diagnostics.snapshot();
        // Only `Struct`/`Enum` items are ever registered below — cloning
        // every item regardless of kind (as this used to) paid for a
        // full deep-clone of every dependency function/impl body in the
        // workspace (by far the largest items) merely to inspect its
        // `ItemKind` tag and then discard it. `register_struct`/
        // `register_enum` themselves already early-return once a
        // `def_id` is registered, so on a `HirToMirLowerer` instance that
        // does span multiple registration passes, repeating this scan is
        // cheap; on a *fresh* instance (as `transform_comptime_request`
        // creates once per comptime request — see `HirToMirLowerer::new()`'s
        // callers) every dependency struct/enum is still cloned once per
        // request, since there is no cross-request cache today. Fully
        // eliminating that repetition needs a cache that outlives a
        // single `HirToMirLowerer` instance (e.g. on `CompilerState`), which
        // is out of scope for this pass. `register_struct`/`register_enum`
        // no longer need a `def_paths` map handed to them (they dispatch
        // through `hir_def_path` themselves), so there's no borrow-vs-
        // `&mut self` conflict left to work around here at all.
        let items: Vec<hir::Item> = self
            .hir_all_items()
            .filter(|item| {
                matches!(
                    &item.kind,
                    hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_)
                )
            })
            .cloned()
            .collect();
        for item in &items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id.clone(), def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id.clone(), def, item.span);
                }
                _ => {}
            }
        }
        self.diagnostics.truncate(diagnostics_before);
    }

    /// Defensive fallback for a struct/enum somehow missed by
    /// `register_all_dependency_adts`'s eager sweep — reached only when a
    /// `def_id` isn't already registered locally.
    pub(crate) fn try_lazily_register_adt(&mut self, def_id: hir::DefId, span: Span) {
        let Some(item) = self.hir_item(def_id.clone()).cloned() else {
            return;
        };
        match &item.kind {
            hir::ItemKind::Struct(strukt) => {
                self.register_struct(def_id, strukt, span);
            }
            hir::ItemKind::Enum(enm) => {
                self.register_enum(def_id, enm, span);
            }
            _ => {}
        }
    }

    pub(super) fn compute_ty_layout(&mut self, ty: &Ty, span: Span) {
        match &ty.kind {
            TyKind::Adt(adt, substs) => {
                for a in substs {
                    if let mir::ty::GenericArg::Type(t) = a {
                        self.compute_ty_layout(t, span);
                    }
                }
                let types: Vec<Ty> = substs
                    .iter()
                    .filter_map(|a| match a {
                        mir::ty::GenericArg::Type(t) => Some(t.clone()),
                        _ => None,
                    })
                    .collect();
                self.compute_adt_layout(adt.did.clone(), &types, span);
            }
            TyKind::Tuple(elements) => {
                for elem in elements {
                    self.compute_ty_layout(elem, span);
                }
            }
            TyKind::Array(elem, _) | TyKind::Slice(elem) => {
                self.compute_ty_layout(elem, span);
            }
            TyKind::Ref(_, inner, _) | TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                self.compute_ty_layout(inner, span);
            }
            _ => {}
        }
    }

    pub(super) fn compute_body_locals(&mut self, program: &mir::MirCodeUnit, body_id: mir::BodyId) {
        if let Some(body) = program.bodies.get(&body_id) {
            for local in &body.locals {
                self.compute_ty_layout(&local.ty, Span::null());
            }
            for block in &body.basic_blocks {
                for stmt in &block.statements {
                    self.compute_stmt_layouts(body, stmt);
                }
                if let Some(term) = &block.terminator {
                    self.compute_terminator_layouts(body, term);
                }
            }
        }
    }

    pub(super) fn compute_stmt_layouts(&mut self, body: &mir::Body, stmt: &mir::Statement) {
        match &stmt.kind {
            mir::StatementKind::Assign(place, rv) => {
                self.compute_place_layouts(body, place);
                self.compute_rvalue_layouts(rv);
            }
            mir::StatementKind::IntrinsicCall { args, .. } => {
                for arg in args {
                    self.compute_operand_layouts(body, arg);
                }
            }
            mir::StatementKind::SetDiscriminant { place, .. }
            | mir::StatementKind::Retag(_, place)
            | mir::StatementKind::AscribeUserType(place, _, _) => {
                self.compute_place_layouts(body, place);
            }
            _ => {}
        }
    }

    pub(super) fn compute_terminator_layouts(&mut self, body: &mir::Body, term: &mir::Terminator) {
        match &term.kind {
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } => {
                self.compute_operand_layouts(body, func);
                for arg in args {
                    self.compute_operand_layouts(body, arg);
                }
                if let Some((place, _)) = destination {
                    self.compute_place_layouts(body, place);
                }
            }
            mir::TerminatorKind::SwitchInt { discr, .. }
            | mir::TerminatorKind::Assert { cond: discr, .. } => {
                self.compute_operand_layouts(body, discr);
            }
            mir::TerminatorKind::Drop { place, .. } => {
                self.compute_place_layouts(body, place);
            }
            mir::TerminatorKind::DropAndReplace { place, value, .. } => {
                self.compute_place_layouts(body, place);
                self.compute_operand_layouts(body, value);
            }
            mir::TerminatorKind::Yield {
                value, resume_arg, ..
            } => {
                self.compute_operand_layouts(body, value);
                self.compute_place_layouts(body, resume_arg);
            }
            _ => {}
        }
    }

    pub(super) fn compute_place_layouts(&mut self, body: &mir::Body, place: &mir::Place) {
        let Some(mut ty) = body.locals.get(place.local as usize).map(|l| l.ty.clone()) else {
            return;
        };
        self.compute_ty_layout(&ty, Span::null());
        for proj in &place.projection {
            match proj {
                mir::PlaceElem::Field(_, field_ty) => {
                    self.compute_ty_layout(field_ty, Span::null());
                    ty = field_ty.clone();
                }
                mir::PlaceElem::Deref => match &ty.kind {
                    TyKind::Ref(_, inner, _) | TyKind::RawPtr(TypeAndMut { ty: inner, .. }) => {
                        let inner = inner.clone();
                        self.compute_ty_layout(&inner, Span::null());
                        ty = *inner;
                    }
                    _ => return,
                },
                _ => {}
            }
        }
    }

    pub(super) fn compute_operand_layouts(&mut self, body: &mir::Body, op: &mir::Operand) {
        match op {
            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                self.compute_place_layouts(body, place);
            }
            mir::Operand::Constant(c) => {
                self.compute_ty_layout(&c.ty, Span::null());
            }
        }
    }

    pub(super) fn compute_rvalue_layouts(&mut self, rv: &mir::Rvalue) {
        match rv {
            mir::Rvalue::Cast(_, _, ty) => {
                self.compute_ty_layout(ty, Span::null());
            }
            mir::Rvalue::Aggregate(agg, _) => {
                if let mir::AggregateKind::Adt(adt, _, substs, _) = agg {
                    let substs_types: Vec<Ty> = substs
                        .iter()
                        .filter_map(|a| match a {
                            mir::ty::GenericArg::Type(t) => Some(t.clone()),
                            _ => None,
                        })
                        .collect();
                    self.compute_adt_layout(adt.did.clone(), &substs_types, Span::null());
                }
            }
            _ => {}
        }
    }

    pub fn walk_program_types_for_layouts(&mut self, program: &mir::MirCodeUnit) {
        for item in &program.items {
            match &item.kind {
                mir::ItemKind::Function(func) => {
                    for ty in &func.sig.inputs {
                        self.compute_ty_layout(ty, Span::null());
                    }
                    self.compute_ty_layout(&func.sig.output, Span::null());
                    self.compute_body_locals(program, func.body_id);
                }
                mir::ItemKind::ExecutableConst(ec) => {
                    self.compute_ty_layout(&ec.ty, Span::null());
                    self.compute_body_locals(program, ec.body_id);
                }
                mir::ItemKind::Static(s) => {
                    self.compute_ty_layout(&s.ty, Span::null());
                }
                _ => {}
            }
        }
    }

    /// Folds `struct_layouts`/`enum_layouts` into the one combined
    /// `(DefId, args) -> field Tys` shape `mir_to_lir` actually looks
    /// layouts up by, and mirrors `opaque_ty_sizes` into its export-facing
    /// field name — both written straight onto the shared `mir_package`
    /// (the exact same `Rc<RefCell<MirPackage>>` `CompilerState`/
    /// `MirToLirLowerer` already read), so there's no separate owned copy
    /// for a caller to re-fetch the package and reassign afterward.
    pub fn sync_layout_exports(&self) {
        let mut full_layouts = HashMap::new();
        for (key, layout) in self.mir_package.borrow().struct_layouts.iter() {
            full_layouts.insert(
                (key.def_id.clone(), key.args.clone()),
                layout.field_tys.clone(),
            );
        }
        for (key, layout) in self.mir_package.borrow().enum_layouts.iter() {
            let mut fields = Vec::with_capacity(1 + layout.payload_tys.len());
            fields.push(layout.tag_ty.clone());
            fields.extend(layout.payload_tys.iter().cloned());
            full_layouts.insert((key.def_id.clone(), key.args.clone()), fields);
        }
        let opaque_payload_sizes = self.mir_package.borrow().opaque_ty_sizes.clone();
        let mut mir_package = self.mir_package.borrow_mut();
        mir_package.full_layouts = full_layouts;
        mir_package.opaque_payload_sizes = opaque_payload_sizes;
    }

    /// `fp_typing`'s checked type for `hir_id`, read straight off
    /// `hir_program` (which owns the `HirPackage` it was written onto —
    /// no separate copy-and-lower-everything-up-front pass here anymore,
    /// see this type's own doc comment for why that used to exist) and
    /// lowered to a MIR `Ty` on demand. Deliberately `&self`, not `&mut
    /// self`: an unresolvable type here is silently treated exactly like a
    /// never-recorded entry (both already flow into the same `Option<_>`
    /// every caller already handles) rather than reported via
    /// `emit_warning`, so this works uniformly from any call site —
    /// including ones that only hold `&self` or reach `HirToMirLowerer` through
    /// an immutably-borrowed field — without a parallel `&mut self` variant.
    pub(crate) fn typeck_expr_type(&self, hir_id: hir::HirId) -> Option<Ty> {
        let ty = self.hir_program.expr_type(hir_id)?;
        lower_hir_ty(&ty).ok()
    }

    /// Same as `typeck_expr_type`, for a type-position `TypeExpr`'s own
    /// checked type instead of a value expr's.
    pub(crate) fn typeck_type_expr_type(&self, hir_id: hir::HirId) -> Option<Ty> {
        let ty = self.hir_program.type_expr_type(hir_id)?;
        lower_hir_ty(&ty).ok()
    }

    /// Same idea, for a resolved generic call/method call's own concrete
    /// type arguments — if any one argument fails to lower, the whole
    /// resolution is skipped (a partial arg list would be nonsensical).
    pub(crate) fn typeck_generic_call_arg(&self, hir_id: hir::HirId) -> Option<Vec<Ty>> {
        let resolution = self.hir_program.generic_call_arg(hir_id)?;
        resolution
            .args
            .iter()
            .map(lower_hir_ty)
            .collect::<Result<Vec<_>>>()
            .ok()
    }

    pub(super) fn typeck_generic_method_arg(&self, hir_id: hir::HirId) -> Option<Vec<Ty>> {
        let resolution = self.hir_program.generic_method_arg(hir_id)?;
        resolution
            .args
            .iter()
            .map(lower_hir_ty)
            .collect::<Result<Vec<_>>>()
            .ok()
    }

    /// Convert a comptime-evaluated `Value` (from `const { ... }` block
    /// resolution) into an MIR constant. No declared type is available at
    /// either call site, so this infers a reasonable one from the value's
    /// own scalar shape, then defers the actual `Constant` construction to
    /// `LirToMir::value_to_mir_constant` (the shared LIR->MIR lift, see
    /// `fp_backend::transforms::lir_to_mir`) — an empty package list is
    /// correct here since only scalar shapes ever reach this path (no Adt
    /// lookup can trigger).
    pub(super) fn const_block_value_to_mir_constant(
        &self,
        value: &Value,
        span: Span,
    ) -> Option<mir::Constant> {
        let ty = match value {
            Value::Int(_) => Ty {
                kind: TyKind::Int(IntTy::I64),
            },
            Value::UInt(_) => Ty {
                kind: TyKind::Uint(UintTy::U64),
            },
            Value::Bool(_) => Ty { kind: TyKind::Bool },
            Value::Decimal(_) => Ty {
                kind: TyKind::Float(FloatTy::F64),
            },
            Value::String(_) => Ty {
                kind: TyKind::Slice(Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                })),
            },
            Value::Null(_) => Ty {
                kind: TyKind::Tuple(Vec::new()),
            },
            _ => return None,
        };
        let mut constant = crate::transforms::lir_to_mir::LirToMir::new(Vec::new())
            .value_to_mir_constant(value, &ty)?;
        constant.span = span;
        Some(constant)
    }

    pub(super) fn const_key(&self, name: &str, span: Span) -> String {
        let file = fp_core::source_map::source_map()
            .file(span.file)
            .map(|file| file.path.display().to_string())
            .unwrap_or_else(|| format!("file#{}", span.file));
        format!("{file}:{}:{}:{name}", span.lo, span.hi)
    }

    pub(super) fn synthetic_const_function_name(&self, name: &hir::Symbol, key: &str) -> String {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let hash = hasher.finish();
        format!("__fp_comptime_const_{}_{}", name.as_str(), hash)
    }

    pub(super) fn lower_program(&mut self, program: &hir::HirPackage) -> Result<mir::MirCodeUnit> {
        // `current_package_id` is already set (and `program` inserted into
        // `hir_program`) by `transform` (the only caller) before this runs.
        let mut mir_program = mir::MirCodeUnit::new();
        // Same "seed from `.items` alone can collide with a local const's
        // own real DefId" fix as `transform_comptime_request` — see that
        // function's own comment for the full rationale.
        self.mir_package.borrow_mut().set_next_synthetic_hir_def_id(
            program
                .def_map
                .keys()
                .cloned()
                .max()
                .unwrap_or(hir::DefId::local(0))
                .saturating_add(1),
        );

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id.clone(), def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id.clone(), def, item.span);
                }
                _ => {}
            }
        }
        self.register_all_dependency_adts();
        self.finalize_adt_definitions(program);
        // Signature-only pre-pass (see `register_impl_signatures`'s own doc
        // comment) so non-generic method/associated-function calls resolve
        // regardless of which module declares the caller vs. the callee —
        // lookup success must not depend on `program.items` order.
        for item in &program.items {
            if let hir::ItemKind::Impl(impl_block) = &item.kind {
                self.register_impl_signatures(impl_block);
            }
        }
        // Lower every item unconditionally. This function builds MIR for one
        // package's own HIR in isolation (a dependency package's MIR is
        // never re-filtered by a downstream package), so a `main`-rooted
        // reachability pass here would silently drop library items with no
        // caller inside their own package — e.g. `std::json::parse`, never
        // called from any `std`-level `main`/const, but very much part of
        // `std`'s public surface that `examples` needs to link against.
        let items: Vec<&hir::Item> = program.items.iter().collect();

        for item in &items {
            match &item.kind {
                hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => {}
                hir::ItemKind::Const(const_item) => {
                    let ty = self.lower_type_expr(&const_item.ty);
                    if Self::is_unit_ty(&ty) {
                        // Unit consts don't need a static allocation; keep them as inline constants.
                        self.ensure_item_lowered(item.def_id.clone())?;
                        continue;
                    }
                    let mir_item = self.lower_const(item.def_id.clone(), const_item)?;
                    mir_program.items.push(mir_item);
                }
                hir::ItemKind::Function(function) => {
                    if !function.sig.generics.params.is_empty() {
                        self.register_generic_function(item.def_id.clone(), function);
                    } else {
                        // Idempotent — a prior call site may have already
                        // lowered this on demand (`ensure_function_lowered`'s
                        // `resolve_callee_path` fallback); this proactive
                        // call is what guarantees full-package coverage for
                        // items with no local caller (see comment above).
                        self.ensure_function_lowered(item.def_id.clone())?;
                    }
                }
                hir::ItemKind::Impl(impl_block) => {
                    // Non-generic methods, on demand (idempotent, same
                    // reasoning as the `Function` arm above); generic
                    // methods were already registered — raw HIR only — by
                    // `register_impl_signatures`'s pre-pass, and lower per
                    // call site via `ensure_method_specialization`.
                    for impl_item in &impl_block.items {
                        if let hir::ImplItemKind::Method(_) = &impl_item.kind {
                            self.ensure_method_lowered(impl_item.def_id.clone())?;
                        }
                    }
                }
                hir::ItemKind::Query(query) => {
                    mir_program.items.push(self.lower_query(item, query));
                }
                // Trait definitions are only a fallback signature source
                // for HIR typechecking's method resolution — never lowered
                // to MIR/emitted to any backend directly (a concrete `impl
                // Trait for X` is what actually gets lowered, via the
                // `Impl` arm above).
                hir::ItemKind::Trait(_) => {}
                hir::ItemKind::Expr(_) => {}
            }
        }

        self.flush_extra_items(&mut mir_program);

        Ok(mir_program)
    }

    pub(super) fn lower_query(&mut self, item: &hir::Item, query: &hir::Query) -> mir::Item {
        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Query(mir::Query {
                origin: query.origin.clone(),
                ir: query.ir.clone(),
                span: item.span,
            }),
        };
        mir_item
    }

    pub(super) fn flush_extra_items(&mut self, program: &mut mir::MirCodeUnit) {
        for item in self.extra_items.drain(..) {
            program.items.push(item);
        }
        for (body_id, body) in self.extra_bodies.drain(..) {
            program.bodies.insert(body_id, body);
        }
    }

    /// Cheap `Rc` clone of the package this instance is lowering — looked
    /// up straight off the shared `hir_program` under `current_package_id`,
    /// so a driver-level loop that needs to iterate `.items` while also
    /// calling `&mut self` methods (`ensure_item_lowered`) can hold this
    /// handle instead of re-fetching/cloning a whole `HirPackage` out of
    /// `CompilerState` separately.
    pub fn current_package_handle(&self) -> std::rc::Rc<hir::HirPackage> {
        self.hir_program
            .package_rc(&self.current_package_id)
            .expect("current package is always a member of hir_program")
    }

    /// Registers every top-level struct/enum and impl signature in the
    /// current package (already in `hir_program`) — call once before any
    /// `ensure_item_lowered` call, then `ensure_item_lowered` for each
    /// top-level `DefId` that needs lowering (in any order — this is
    /// exactly what makes it usable from a driver-level loop instead of
    /// requiring one eager whole-package sweep).
    ///
    /// Struct/enum layouts and method signatures still need this eager
    /// pass: `finalize_adt_definitions`'s layout computation and
    /// `register_impl_signatures`'s signature table both assume every
    /// nominal type in reach (including cross-package dependencies, via
    /// `register_all_dependency_adts`) is already registered by the time
    /// they run — see `register_all_dependency_adts`'s doc comment for the
    /// ordering bug that guarantee exists to prevent. Consts have no such
    /// coupling (each one lowers independently), so they need no eager
    /// pass at all: `ensure_const_info`, used by every `const_values` read
    /// site, lazily triggers `ensure_item_lowered` on a cache miss.
    pub fn register_package_items(&mut self) {
        let current_package = self.current_package_handle();
        // Same "seed from `.items` alone can collide with a local const's
        // own real DefId" fix as `lower_program`/`transform_comptime_request`.
        self.mir_package.borrow_mut().set_next_synthetic_hir_def_id(
            current_package
                .def_map
                .keys()
                .cloned()
                .max()
                .unwrap_or(hir::DefId::local(0))
                .saturating_add(1),
        );
        for item in &current_package.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id.clone(), def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id.clone(), def, item.span);
                }
                _ => {}
            }
        }
        self.register_all_dependency_adts();
        self.finalize_adt_definitions(&current_package);
        for item in &current_package.items {
            if let hir::ItemKind::Impl(impl_block) = &item.kind {
                self.register_impl_signatures(impl_block);
            }
        }
        // Consts are the one part of this sweep that's genuinely lazy:
        // `ensure_const_info` (used by every `const_values` read site)
        // triggers `ensure_item_lowered` on a cache miss itself, so no
        // eager per-const pass is needed here at all.
    }

    /// On-demand, per-`DefId` counterpart to `lower_program`'s per-item
    /// loop — call once per top-level `DefId` after `register_package_items`.
    /// Idempotent (safe to call more than once for the same `def_id`,
    /// mirroring `ensure_function_lowered`/`ensure_method_lowered`, which
    /// this dispatches to for `Function`/`Impl` items). `Struct`/`Enum` are
    /// already fully registered by `register_package_items`; `Trait`/`Expr` are never
    /// lowered — both are no-ops here.
    pub fn ensure_item_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
        if self.lowered_items.contains(&def_id) {
            return Ok(());
        }
        let Some(item) = self.hir_item(def_id.clone()).cloned() else {
            // Not a top-level item at all — check whether it's a
            // `const { .. }` block's own synthetic `DefId` instead (see
            // `ensure_const_block_lowered`'s doc comment). Nothing else
            // is addressable by `DefId` here, so a miss on both is a
            // silent no-op, same as before.
            return self.ensure_const_block_lowered(def_id);
        };
        match &item.kind {
            hir::ItemKind::Struct(_)
            | hir::ItemKind::Enum(_)
            | hir::ItemKind::Trait(_)
            | hir::ItemKind::Expr(_) => {}
            hir::ItemKind::Const(const_item) => {
                self.lowered_items.insert(def_id.clone());
                let ty = self.lower_type_expr(&const_item.ty);
                if Self::is_unit_ty(&ty) {
                    // Unit consts don't need a static allocation; keep them
                    // as inline constants — already registered by
                    // `register_package_items`.
                } else {
                    let mir_item = self.lower_const(def_id, const_item)?;
                    self.extra_items.push(mir_item);
                }
            }
            hir::ItemKind::Function(function) => {
                if !function.sig.generics.params.is_empty() {
                    self.lowered_items.insert(def_id.clone());
                    self.register_generic_function(def_id, function);
                } else {
                    self.ensure_function_lowered(def_id)?;
                }
            }
            hir::ItemKind::Impl(impl_block) => {
                self.lowered_items.insert(def_id);
                for impl_item in &impl_block.items {
                    if let hir::ImplItemKind::Method(_) = &impl_item.kind {
                        self.ensure_method_lowered(impl_item.def_id.clone())?;
                    }
                }
            }
            hir::ItemKind::Query(query) => {
                self.lowered_items.insert(def_id);
                let mir_item = self.lower_query(&item, query);
                self.extra_items.push(mir_item);
            }
        }
        Ok(())
    }

    /// `ensure_item_lowered`'s counterpart for a `const { .. }` block's own
    /// `DefId` — a block is never in the current package's `items`/`def_map`
    /// (`record_const_block_def` is its own side table, not `def_map`; see
    /// `hir::HirPackage::const_block_defs`'s doc comment), so it can't be
    /// dispatched on there directly, but lowering it is otherwise identical
    /// to a top-level `const`'s own non-foldable path (`lower_const`'s
    /// `lower_executable_const` call): build a synthetic zero-arg function
    /// from the block's body and register it as a pending-comptime global
    /// under this exact `def_id` — the same identity `fp_typing::
    /// ComptimeRequest::def_id`/`LirProgram::find_function_by_def_id`
    /// already use, so a driver-level comptime request resolves through
    /// this one call, the same as any other item, with no separate
    /// setup/entry point of its own.
    pub(super) fn ensure_const_block_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
        if self.lowered_items.contains(&def_id) {
            return Ok(());
        }
        let Some(block) = self
            .hir_program
            .package(&def_id.package_id)
            .ok_or_else(|| crate::error::optimization_error("missing HIR package"))?
            .const_block_def(def_id.clone())
        else {
            return Ok(());
        };
        self.lowered_items.insert(def_id.clone());
        let Some(body) = block.expr.as_ref() else {
            return Ok(());
        };
        let Some(ty) = self.typeck_expr_type(block.hir_id.clone()) else {
            return Ok(());
        };
        let name = hir::Symbol::new(format!(
            "__const_block_{}_{}",
            def_id.package_id.0, def_id.index
        ));
        let konst = hir::Const {
            name: name.clone(),
            ty: hir::TypeExpr {
                hir_id: block.hir_id.clone(),
                kind: hir::TypeExprKind::Infer,
                span: body.span,
            },
            body: hir::Body {
                hir_id: block.hir_id.clone(),
                params: Vec::new(),
                value: (**body).clone(),
            },
        };
        let key = self.const_key(name.as_str(), body.span);
        let mir_item = self.lower_executable_const(def_id, &konst, ty, key, Some(block.hir_id))?;
        self.extra_items.push(mir_item);
        Ok(())
    }

    /// Drains everything pushed to `extra_items`/`extra_bodies` since the
    /// last `take_unit` call into one `MirCodeUnit` — call right after
    /// `ensure_item_lowered(def_id)` returns, before the next
    /// `ensure_item_lowered` call for a different `DefId`, so the drained
    /// content is exactly what that one call produced (usually its own item
    /// plus one body, occasionally more when it pulled in something it
    /// directly references).
    pub fn take_unit(&mut self) -> mir::MirCodeUnit {
        mir::MirCodeUnit {
            items: std::mem::take(&mut self.extra_items),
            bodies: self.extra_bodies.drain(..).collect(),
        }
    }

    /// On-demand counterpart to `lower_function`, for a non-generic free
    /// function: lowers `def_id`'s body at most once (guarded by
    /// `lowered_items`), pushing the result into `extra_items`/
    /// `extra_bodies`. Callable both proactively (`lower_program`'s main
    /// loop, ensuring full package coverage) and reactively (a call site
    /// whose callee hasn't been lowered yet — `resolve_callee_path`'s
    /// `hir_def_map` fallback), mirroring the same lazy pattern
    /// `ensure_item_lowered`/`try_lazily_register_adt`/
    /// `ensure_function_specialization` already use for consts/ADTs/
    /// generics. A miss (unknown `def_id`, or a non-`Function` item) is not
    /// an error here — the caller's own resolution path already reports a
    /// real diagnostic when nothing usable comes back.
    pub(super) fn fill_missing_substs_from_expected_return(
        &self,
        return_ty: &hir::TypeExpr,
        expected_return: &Ty,
        generics: &[String],
        substs: &mut HashMap<String, Ty>,
    ) {
        let return_ty = self.unwrap_expr_type_expr(return_ty);
        let expected_return = self.unwrap_expr_actual_ty(expected_return);
        if let Some(inner_return_ty) = self.expr_inner_type_expr(return_ty) {
            if let Some(inner_expected) = self.expr_inner_actual_ty(expected_return) {
                self.fill_missing_substs_from_expected_return(
                    inner_return_ty,
                    inner_expected,
                    generics,
                    substs,
                );
                return;
            }
            self.fill_missing_substs_from_expected_return(
                inner_return_ty,
                expected_return,
                generics,
                substs,
            );
            return;
        }
        if let Some(inner_expected) = self.expr_inner_actual_ty(expected_return) {
            self.fill_missing_substs_from_expected_return(
                return_ty,
                inner_expected,
                generics,
                substs,
            );
            return;
        }
        let hir::TypeExprKind::Path(path) = &return_ty.kind else {
            return;
        };
        let expected_return = match &expected_return.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
            _ => expected_return,
        };
        if self.is_result_path(path) {
            if let Some(actual_args) = self.explicit_args_from_expected_result_ty(expected_return) {
                if actual_args.len() == generics.len() {
                    for (name, actual_arg) in generics.iter().zip(actual_args) {
                        if substs.contains_key(name) {
                            continue;
                        }
                        if matches!(actual_arg.kind, TyKind::Infer(_)) {
                            continue;
                        }
                        substs.insert(name.to_string(), actual_arg);
                    }
                    return;
                }
            }
        }
        let actual_substs = match &expected_return.kind {
            TyKind::Adt(_, substs) | TyKind::Opaque(_, substs) => substs,
            _ => return,
        };

        let actual_type_args = actual_substs
            .iter()
            .filter_map(|arg| match arg {
                mir::ty::GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .collect::<Vec<_>>();
        let path_args = path
            .segments
            .iter()
            .rev()
            .find_map(|segment| segment.args.as_ref());
        if path_args.is_none() {
            if actual_type_args.len() == generics.len() {
                for (name, actual_arg) in generics.iter().zip(actual_type_args) {
                    if substs.contains_key(name) {
                        continue;
                    }
                    if matches!(actual_arg.kind, TyKind::Infer(_)) {
                        continue;
                    }
                    substs.insert(name.to_string(), actual_arg.clone());
                }
            }
            return;
        }
        let Some(path_args) = path_args else {
            return;
        };
        let path_type_args = path_args
            .args
            .iter()
            .filter_map(|arg| match arg {
                hir::GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .collect::<Vec<_>>();
        if path_type_args.is_empty() {
            if actual_type_args.len() == generics.len() {
                for (name, actual_arg) in generics.iter().zip(actual_type_args) {
                    if substs.contains_key(name) {
                        continue;
                    }
                    if matches!(actual_arg.kind, TyKind::Infer(_)) {
                        continue;
                    }
                    substs.insert(name.to_string(), actual_arg.clone());
                }
            }
            return;
        }
        if path_type_args.len() != actual_type_args.len() {
            return;
        }

        for (type_arg, actual_arg) in path_type_args.into_iter().zip(actual_type_args) {
            let hir::TypeExprKind::Path(type_path) = &type_arg.kind else {
                continue;
            };
            if type_path.segments.len() != 1 || type_path.segments[0].args.is_some() {
                continue;
            }
            let name = type_path.segments[0].name.as_str();
            if !generics.iter().any(|generic| generic == name) || substs.contains_key(name) {
                continue;
            }
            if matches!(actual_arg.kind, TyKind::Infer(_)) {
                continue;
            }
            substs.insert(name.to_string(), actual_arg.clone());
        }
    }

    pub(super) fn build_substs_from_explicit_args(
        &mut self,
        generics: &[String],
        explicit_args: &[Ty],
        span: Span,
    ) -> Result<HashMap<String, Ty>> {
        if explicit_args.len() != generics.len() {
            self.emit_error(
                span,
                format!(
                    "expected {} generic arguments, got {}",
                    generics.len(),
                    explicit_args.len()
                ),
            );
            return Err(crate::error::optimization_error(
                "generic argument count mismatch",
            ));
        }

        let mut substs = HashMap::new();
        for (name, ty) in generics.iter().zip(explicit_args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }
        Ok(substs)
    }

    pub(crate) fn is_result_path(&self, path: &hir::Path) -> bool {
        path.segments
            .last()
            .map(|segment| segment.name.as_str() == "Result")
            .unwrap_or(false)
    }

    pub(super) fn explicit_args_from_expected_result_ty(
        &self,
        expected_ty: &Ty,
    ) -> Option<Vec<Ty>> {
        let expected_ty = self.unwrap_expr_actual_ty(expected_ty);
        let expected_ty = match &expected_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
            _ => expected_ty,
        };
        let (adt, substs) = match &expected_ty.kind {
            TyKind::Adt(adt, substs) => (&adt.did, substs),
            TyKind::Opaque(def_id, substs) => (def_id, substs),
            _ => {
                let layout = self.enum_layout_for_ty(expected_ty)?;
                let is_result = self
                    .mir_package
                    .borrow()
                    .enum_defs
                    .get(&layout.def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false);
                if !is_result {
                    return None;
                }
                let mut args = Vec::new();
                for ty in &layout.args {
                    args.push(ty.clone());
                }
                if args.is_empty() {
                    return None;
                }
                return Some(args);
            }
        };
        let is_result = self
            .mir_package
            .borrow()
            .enum_defs
            .get(adt)
            .map(|def| def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result"))
            .or_else(|| {
                self.mir_package
                    .borrow()
                    .struct_defs
                    .get(adt)
                    .cloned()
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
            })
            .unwrap_or(false);
        if !is_result {
            if let Some(layout) = self.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .mir_package
                    .borrow()
                    .enum_defs
                    .get(&layout.def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false);
                if !is_result_layout {
                    return None;
                }
                let mut args = Vec::new();
                for ty in &layout.args {
                    let ty = self.unwrap_expr_actual_ty(ty);
                    args.push(ty.clone());
                }
                if args.is_empty() {
                    return None;
                }
                return Some(args);
            }
            return None;
        }
        let mut args = Vec::new();
        for arg in substs {
            let mir::ty::GenericArg::Type(ty) = arg else {
                continue;
            };
            let ty = self.unwrap_expr_actual_ty(ty);
            args.push(ty.clone());
        }
        if args.len() < 2 {
            if let Some(layout) = self.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .mir_package
                    .borrow()
                    .enum_defs
                    .get(&layout.def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false);
                if is_result_layout {
                    let layout_args = layout
                        .args
                        .iter()
                        .map(|ty| self.unwrap_expr_actual_ty(ty).clone())
                        .collect::<Vec<_>>();
                    for (idx, layout_ty) in layout_args.iter().enumerate() {
                        if args.len() <= idx {
                            args.push(layout_ty.clone());
                            continue;
                        }
                        if matches!(args[idx].kind, TyKind::Infer(_) | TyKind::Error(_))
                            && !matches!(layout_ty.kind, TyKind::Infer(_) | TyKind::Error(_))
                        {
                            args[idx] = layout_ty.clone();
                        }
                    }
                    if args.len() < 2 {
                        if let Some(def) = self
                            .mir_package
                            .borrow()
                            .enum_defs
                            .get(&layout.def_id)
                            .cloned()
                        {
                            let mut ok_payload = None;
                            let mut err_payload = None;
                            for variant in &def.variants {
                                if variant.name.as_str() == "Ok"
                                    || variant.name.as_str().ends_with("::Ok")
                                {
                                    if let Some(payloads) =
                                        layout.variant_payloads.get(&variant.def_id)
                                    {
                                        if payloads.len() == 1 {
                                            ok_payload = Some(payloads[0].clone());
                                        }
                                    }
                                    continue;
                                }
                                if variant.name.as_str() == "Err"
                                    || variant.name.as_str().ends_with("::Err")
                                {
                                    if let Some(payloads) =
                                        layout.variant_payloads.get(&variant.def_id)
                                    {
                                        if payloads.len() == 1 {
                                            err_payload = Some(payloads[0].clone());
                                        }
                                    }
                                }
                            }
                            if args.is_empty() {
                                if let Some(ok) = ok_payload {
                                    args.push(ok);
                                }
                                if let Some(err) = err_payload {
                                    args.push(err);
                                }
                            } else if args.len() == 1 {
                                if let Some(err) = err_payload {
                                    args.push(err);
                                }
                            }
                        }
                    }
                }
            }
        }
        if args.is_empty() {
            if let Some(layout) = self.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .mir_package
                    .borrow()
                    .enum_defs
                    .get(&layout.def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false);
                if is_result_layout {
                    for ty in &layout.args {
                        let ty = self.unwrap_expr_actual_ty(ty);
                        args.push(ty.clone());
                    }
                }
            }
        }
        if args.is_empty() {
            return None;
        }
        Some(args)
    }

    pub(crate) fn expr_inner_type_expr<'a>(
        &self,
        ty_expr: &'a hir::TypeExpr,
    ) -> Option<&'a hir::TypeExpr> {
        let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
            return None;
        };
        let segment = path.segments.last()?;
        if segment.name.as_str() != "Expr" {
            return None;
        }
        let args = segment.args.as_ref()?;
        let mut type_args = args.args.iter().filter_map(|arg| match arg {
            hir::GenericArg::Type(ty) => Some(ty.as_ref()),
            _ => None,
        });
        let inner = type_args.next()?;
        if type_args.next().is_some() {
            return None;
        }
        Some(inner)
    }

    pub(super) fn expr_inner_actual_ty<'a>(&self, actual_ty: &'a Ty) -> Option<&'a Ty> {
        let (def_id, substs) = match &actual_ty.kind {
            TyKind::Adt(adt, substs) => (adt.did.clone(), substs),
            TyKind::Opaque(def_id, substs) => (def_id.clone(), substs),
            _ => return None,
        };
        let is_expr = self
            .mir_package
            .borrow()
            .struct_defs
            .get(&def_id)
            .map(|def| def.name.as_str() == "Expr" || def.name.as_str().ends_with("::Expr"))
            .unwrap_or(false)
            || self
                .mir_package
                .borrow()
                .enum_defs
                .get(&def_id)
                .map(|def| def.name.as_str() == "Expr" || def.name.as_str().ends_with("::Expr"))
                .unwrap_or(false)
            || self
                .display_type_name(actual_ty)
                .map(|name| name == "Expr" || name.ends_with("::Expr"))
                .unwrap_or(false);
        if !is_expr {
            return None;
        }
        let mut type_args = substs.iter().filter_map(|arg| match arg {
            mir::ty::GenericArg::Type(ty) => Some(ty),
            _ => None,
        });
        let inner = type_args.next()?;
        if type_args.next().is_some() {
            return None;
        }
        Some(inner)
    }

    pub(super) fn unwrap_expr_type_expr<'a>(
        &self,
        mut ty_expr: &'a hir::TypeExpr,
    ) -> &'a hir::TypeExpr {
        while let Some(inner) = self.expr_inner_type_expr(ty_expr) {
            ty_expr = inner;
        }
        ty_expr
    }

    pub(crate) fn unwrap_expr_actual_ty<'a>(&self, mut actual_ty: &'a Ty) -> &'a Ty {
        while let Some(inner) = self.expr_inner_actual_ty(actual_ty) {
            actual_ty = inner;
        }
        actual_ty
    }

    pub(super) fn infer_generic_from_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
        actual_ty: &Ty,
        generics: &[String],
        substs: &mut HashMap<String, Ty>,
        span: Span,
    ) -> Result<()> {
        if matches!(actual_ty.kind, TyKind::Error(_) | TyKind::Infer(_)) {
            return Ok(());
        }
        if let Some(inner_actual) = self.expr_inner_actual_ty(actual_ty) {
            if let Some(inner_ty_expr) = self.expr_inner_type_expr(ty_expr) {
                return self.infer_generic_from_type_expr(
                    inner_ty_expr,
                    inner_actual,
                    generics,
                    substs,
                    span,
                );
            }
            return self.infer_generic_from_type_expr(
                ty_expr,
                inner_actual,
                generics,
                substs,
                span,
            );
        }
        if let Some(inner_ty_expr) = self.expr_inner_type_expr(ty_expr) {
            return self.infer_generic_from_type_expr(
                inner_ty_expr,
                actual_ty,
                generics,
                substs,
                span,
            );
        }
        // Keep inference conservative: only bind direct generic parameters.
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => {
                let variant_enum_def = path.res.as_ref().and_then(|res| {
                    if let hir::Res::Def(def_id) = res {
                        self.mir_package
                            .borrow()
                            .enum_variants
                            .get(def_id)
                            .map(|variant| variant.enum_def.clone())
                    } else {
                        None
                    }
                });
                if let Some((actual_def_id, actual_substs, actual_is_opaque)) = match &actual_ty
                    .kind
                {
                    TyKind::Adt(adt, substs) => Some((Some(adt.did.clone()), substs, false)),
                    TyKind::Opaque(def_id, substs) => Some((Some(def_id.clone()), substs, true)),
                    _ => None,
                } {
                    let mut matches_def = false;
                    if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                        if let Some(ref actual_def_id) = actual_def_id {
                            matches_def = *def_id == *actual_def_id
                                || variant_enum_def == Some(actual_def_id.clone());
                        }
                        if !matches_def {
                            if let Some(name) = path.segments.last().map(|seg| seg.name.as_str()) {
                                if let Some(actual_def_id) = actual_def_id {
                                    matches_def = self
                                        .mir_package
                                        .borrow()
                                        .enum_defs
                                        .get(&actual_def_id)
                                        .map(|def| {
                                            def.name.as_str() == name
                                                || def
                                                    .name
                                                    .as_str()
                                                    .ends_with(&format!("::{}", name))
                                        })
                                        .unwrap_or(false)
                                        || self
                                            .mir_package
                                            .borrow()
                                            .struct_defs
                                            .get(&actual_def_id)
                                            .map(|def| {
                                                def.name.as_str() == name
                                                    || def
                                                        .name
                                                        .as_str()
                                                        .ends_with(&format!("::{}", name))
                                            })
                                            .unwrap_or(false);
                                }
                            }
                        }
                    } else if let Some(name) = path.segments.last().map(|seg| seg.name.as_str()) {
                        if let Some(actual_def_id) = actual_def_id {
                            matches_def = self
                                .mir_package
                                .borrow()
                                .enum_defs
                                .get(&actual_def_id)
                                .map(|def| {
                                    def.name.as_str() == name
                                        || def.name.as_str().ends_with(&format!("::{}", name))
                                })
                                .unwrap_or(false)
                                || self
                                    .mir_package
                                    .borrow()
                                    .struct_defs
                                    .get(&actual_def_id)
                                    .map(|def| {
                                        def.name.as_str() == name
                                            || def.name.as_str().ends_with(&format!("::{}", name))
                                    })
                                    .unwrap_or(false);
                        }
                    }

                    if matches_def {
                        if let Some(path_args) =
                            path.segments.iter().rev().find_map(|seg| seg.args.as_ref())
                        {
                            let path_type_args = path_args
                                .args
                                .iter()
                                .filter_map(|arg| match arg {
                                    hir::GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            let actual_type_args = actual_substs
                                .iter()
                                .filter_map(|arg| match arg {
                                    mir::ty::GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            if !path_type_args.is_empty()
                                && path_type_args.len() == actual_type_args.len()
                            {
                                for (type_arg, actual_arg) in
                                    path_type_args.into_iter().zip(actual_type_args)
                                {
                                    self.infer_generic_from_type_expr(
                                        type_arg, actual_arg, generics, substs, span,
                                    )?;
                                }
                                return Ok(());
                            }
                        }
                    } else if actual_is_opaque {
                        if let Some(path_args) =
                            path.segments.iter().rev().find_map(|seg| seg.args.as_ref())
                        {
                            let path_type_args = path_args
                                .args
                                .iter()
                                .filter_map(|arg| match arg {
                                    hir::GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            let actual_type_args = actual_substs
                                .iter()
                                .filter_map(|arg| match arg {
                                    mir::ty::GenericArg::Type(ty) => Some(ty),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            if !path_type_args.is_empty()
                                && path_type_args.len() == actual_type_args.len()
                            {
                                for (type_arg, actual_arg) in
                                    path_type_args.into_iter().zip(actual_type_args)
                                {
                                    self.infer_generic_from_type_expr(
                                        type_arg, actual_arg, generics, substs, span,
                                    )?;
                                }
                                return Ok(());
                            }
                        }
                    }
                }
                if let Some(path_args) =
                    path.segments.iter().rev().find_map(|seg| seg.args.as_ref())
                {
                    let def_id = path.res.as_ref().and_then(|res| match res {
                        hir::Res::Def(def_id) => Some(def_id.clone()),
                        _ => None,
                    });
                    if let Some(def_id) = def_id {
                        // Prefer the exact reverse-index lookup over the
                        // fuzzy scan (`enum_layout_for_ty`, which treats
                        // `TyKind::Infer` on either side as a wildcard and
                        // can therefore return an unrelated or
                        // not-yet-fully-specialized layout when multiple
                        // instantiations of the same enum are registered)
                        // — see `enum_layout_for_ty_exact`'s doc comment.
                        let layout = self
                            .enum_layout_for_ty_exact(actual_ty)
                            .or_else(|| self.enum_layout_for_ty(actual_ty));
                        if let Some(layout) = layout {
                            let enum_def_id =
                                variant_enum_def.clone().unwrap_or_else(|| def_id.clone());
                            if layout.def_id == enum_def_id {
                                let layout_args = layout.args.clone();
                                let path_type_args = path_args
                                    .args
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        hir::GenericArg::Type(ty) => Some(ty),
                                        _ => None,
                                    })
                                    .collect::<Vec<_>>();
                                if !path_type_args.is_empty()
                                    && path_type_args.len() == layout_args.len()
                                {
                                    for (type_arg, actual_arg) in
                                        path_type_args.into_iter().zip(layout_args.iter())
                                    {
                                        self.infer_generic_from_type_expr(
                                            type_arg, actual_arg, generics, substs, span,
                                        )?;
                                    }
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
                let name = path
                    .segments
                    .last()
                    .map(|seg| seg.name.as_str())
                    .unwrap_or("");
                let is_generic = path.segments.iter().all(|seg| seg.args.is_none())
                    && generics.iter().any(|g| g == name);
                if is_generic {
                    let actual_is_opaque = self.is_opaque_ty(actual_ty);
                    if let Some(existing) = substs.get(name) {
                        if existing != actual_ty {
                            if matches!(existing.kind, TyKind::Error(_) | TyKind::Infer(_)) {
                                substs.insert(name.to_string(), actual_ty.clone());
                                return Ok(());
                            }
                            let existing_is_opaque = self.is_opaque_ty(existing);
                            if existing_is_opaque && !actual_is_opaque {
                                substs.insert(name.to_string(), actual_ty.clone());
                                return Ok(());
                            }
                            if actual_is_opaque {
                                if !existing_is_opaque {
                                    substs.insert(name.to_string(), actual_ty.clone());
                                }
                                return Ok(());
                            }
                            self.emit_error(
                                span,
                                format!(
                                    "conflicting inference for `{}`: {:?} vs {:?}",
                                    name, existing, actual_ty
                                ),
                            );
                            return Err(crate::error::optimization_error(
                                "conflicting generic inference",
                            ));
                        }
                    } else {
                        substs.insert(name.to_string(), actual_ty.clone());
                    }
                    return Ok(());
                }

                let path_args = path.segments.last().and_then(|seg| seg.args.as_ref());
                if let Some(path_args) = path_args {
                    if let Some(adt_substs) = match &actual_ty.kind {
                        TyKind::Adt(_, substs) | TyKind::Opaque(_, substs) => Some(substs),
                        _ => None,
                    } {
                        for (arg, subst) in path_args.args.iter().zip(adt_substs.iter()) {
                            let mir::ty::GenericArg::Type(actual_arg_ty) = subst else {
                                continue;
                            };
                            if let hir::GenericArg::Type(type_arg) = arg {
                                self.infer_generic_from_type_expr(
                                    type_arg.as_ref(),
                                    actual_arg_ty,
                                    generics,
                                    substs,
                                    span,
                                )?;
                            }
                        }
                    }
                }

                if let (Some(path_args), Some(hir::Res::Def(def_id))) =
                    (path_args, path.res.as_ref())
                {
                    let enum_def_id = if self.mir_package.borrow().enum_defs.contains_key(def_id) {
                        Some(def_id.clone())
                    } else {
                        variant_enum_def
                    };
                    if let Some(enum_def_id) = enum_def_id {
                        let mut candidates: Vec<EnumLayout> = self
                            .mir_package
                            .borrow()
                            .enum_layouts
                            .values()
                            .filter(|layout| layout.def_id == enum_def_id)
                            .cloned()
                            .collect();
                        if !candidates.is_empty() {
                            let exact: Vec<EnumLayout> = candidates
                                .iter()
                                .cloned()
                                .filter(|layout| layout.enum_ty == *actual_ty)
                                .collect();
                            if !exact.is_empty() {
                                candidates = exact;
                            }
                        }
                        if !candidates.is_empty() {
                            let mut scored: Vec<(EnumLayout, usize, usize, String)> = candidates
                                .into_iter()
                                .map(|layout| {
                                    let mut mismatch: usize = 0;
                                    let mut actual_iter = layout.args.iter();
                                    for arg in &path_args.args {
                                        let hir::GenericArg::Type(type_arg) = arg else {
                                            continue;
                                        };
                                        let Some(actual_arg_ty) = actual_iter.next() else {
                                            break;
                                        };
                                        if let hir::TypeExprKind::Path(type_path) = &type_arg.kind {
                                            if type_path.segments.len() == 1
                                                && type_path.segments[0].args.is_none()
                                            {
                                                let name = type_path.segments[0].name.as_str();
                                                if let Some(existing) = substs.get(name) {
                                                    if existing != actual_arg_ty
                                                        && !matches!(
                                                            existing.kind,
                                                            TyKind::Error(_) | TyKind::Infer(_)
                                                        )
                                                    {
                                                        mismatch = mismatch.saturating_add(1);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    let unresolved = layout
                                        .args
                                        .iter()
                                        .filter(|ty| self.has_unresolved_ty(ty))
                                        .count();
                                    let repr = format!("{:?}", layout.args);
                                    (layout, mismatch, unresolved, repr)
                                })
                                .collect();
                            scored.sort_by(|a, b| (a.1, a.2, &a.3).cmp(&(b.1, b.2, &b.3)));
                            let layout_args = scored[0].0.args.clone();
                            let mut actual_iter = layout_args.iter();
                            for arg in &path_args.args {
                                let hir::GenericArg::Type(type_arg) = arg else {
                                    continue;
                                };
                                let Some(actual_arg_ty) = actual_iter.next() else {
                                    break;
                                };
                                self.infer_generic_from_type_expr(
                                    type_arg.as_ref(),
                                    actual_arg_ty,
                                    generics,
                                    substs,
                                    span,
                                )?;
                            }
                        }
                    }
                }

                if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                    if let Some(struct_def) = self.struct_def(def_id) {
                        if let TyKind::Tuple(elements) = &actual_ty.kind {
                            for (field, actual_field_ty) in
                                struct_def.fields.iter().zip(elements.iter())
                            {
                                self.infer_generic_from_type_expr(
                                    &field.ty,
                                    actual_field_ty.as_ref(),
                                    generics,
                                    substs,
                                    span,
                                )?;
                            }
                        }
                    }
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                if let TyKind::Ref(_, actual_inner, _) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                } else {
                    // HIR does not preserve explicit references, so allow inference from the
                    // underlying value type when a ref is expected.
                    self.infer_generic_from_type_expr(inner, actual_ty, generics, substs, span)?;
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                if let TyKind::RawPtr(actual_inner) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.ty.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::Tuple(items) => {
                if let TyKind::Tuple(actual_items) = &actual_ty.kind {
                    for (item, actual_item) in items.iter().zip(actual_items.iter()) {
                        self.infer_generic_from_type_expr(
                            item,
                            actual_item.as_ref(),
                            generics,
                            substs,
                            span,
                        )?;
                    }
                }
            }
            hir::TypeExprKind::Array(inner, _) => {
                if let TyKind::Array(actual_inner, _) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::Slice(inner) => {
                if let TyKind::Slice(actual_inner) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                match &actual_ty.kind {
                    TyKind::FnPtr(poly_sig) => {
                        let sig = &poly_sig.binder.value;
                        if fn_ptr.inputs.len() != sig.inputs.len() {
                            return Ok(());
                        }
                        for (expected, actual) in fn_ptr.inputs.iter().zip(sig.inputs.iter()) {
                            self.infer_generic_from_type_expr(
                                expected,
                                actual.as_ref(),
                                generics,
                                substs,
                                span,
                            )?;
                        }
                        self.infer_generic_from_type_expr(
                            &fn_ptr.output,
                            sig.output.as_ref(),
                            generics,
                            substs,
                            span,
                        )?;
                        return Ok(());
                    }
                    TyKind::FnDef(def_id, _) => {
                        let sig = match self.mir_package.borrow().function_sigs.get(def_id).cloned()
                        {
                            Some(sig) => sig,
                            None => return Ok(()),
                        };
                        if fn_ptr.inputs.len() != sig.inputs.len() {
                            return Ok(());
                        }
                        for (expected, actual) in fn_ptr.inputs.iter().zip(sig.inputs.iter()) {
                            self.infer_generic_from_type_expr(
                                expected, actual, generics, substs, span,
                            )?;
                        }
                        self.infer_generic_from_type_expr(
                            &fn_ptr.output,
                            &sig.output,
                            generics,
                            substs,
                            span,
                        )?;
                        return Ok(());
                    }
                    _ => {}
                };
            }
            _ => {}
        }

        Ok(())
    }

    pub(super) fn def_path_str(&self, def_id: hir::DefId, bare_name: &str) -> String {
        self.hir_def_path(def_id)
            .map(|path| path.to_string())
            .unwrap_or_else(|| bare_name.to_string())
    }

    /// The part of a (possibly `::`-qualified) definition name after its
    /// final `::`, or the whole name if unqualified — see
    /// `struct_defs_by_tail_name`'s doc comment for why this is a safe
    /// pre-filter key for `struct_def_from_ty`'s suffix-match fallback.
    pub(crate) fn name_tail(name: &str) -> &str {
        name.rsplit("::").next().unwrap_or(name)
    }

    pub(crate) fn register_struct(
        &mut self,
        def_id: hir::DefId,
        strukt: &hir::Struct,
        _span: Span,
    ) {
        if self.mir_package.borrow().struct_defs.contains_key(&def_id) {
            return;
        }

        let mut fields = Vec::new();
        let mut field_index = HashMap::new();

        for (idx, field) in strukt.fields.iter().enumerate() {
            fields.push(StructFieldDef {
                name: String::from(field.name.clone()),
                ty: field.ty.clone(),
            });
            field_index.insert(String::from(field.name.clone()), idx);
        }

        let generics = strukt
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();

        let name = self.def_path_str(def_id.clone(), strukt.name.as_str());
        self.mir_package
            .borrow_mut()
            .struct_defs_by_tail_name
            .entry(Self::name_tail(&name).to_string())
            .or_default()
            .push(def_id.clone());
        self.mir_package.borrow_mut().struct_defs.insert(
            def_id,
            StructDefinition {
                name,
                generics,
                fields,
                field_index,
            },
        );
    }

    pub(crate) fn register_enum(&mut self, def_id: hir::DefId, enm: &hir::Enum, _span: Span) {
        if self.mir_package.borrow().enum_defs.contains_key(&def_id) {
            return;
        }

        let generics = enm
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();
        let enum_qualified_name = self.def_path_str(def_id.clone(), enm.name.as_str());

        let mut variants = Vec::new();
        let mut next_value: i64 = 0;
        for variant in &enm.variants {
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(def_id.clone());
                    }
                }
                None
            });
            let value = if let Some(expr) = &variant.discriminant {
                match self.eval_int_expr(expr) {
                    Some(val) => {
                        next_value = val.saturating_add(1);
                        val
                    }
                    None => {
                        self.emit_error(
                            expr.span,
                            format!(
                                "unable to evaluate discriminant for enum variant `{}`",
                                variant.name
                            ),
                        );
                        let val = next_value;
                        next_value = next_value.saturating_add(1);
                        val
                    }
                }
            } else {
                let val = next_value;
                next_value = next_value.saturating_add(1);
                val
            };

            variants.push(EnumVariantDef {
                def_id: variant.def_id.clone(),
                name: variant.name.as_str().to_string(),
                discriminant: value,
                payload: variant.payload.clone(),
            });

            self.mir_package.borrow_mut().enum_variants.insert(
                variant.def_id.clone(),
                EnumVariantInfo {
                    def_id: variant.def_id.clone(),
                    enum_def: def_id.clone(),
                    discriminant: value,
                    payload_def,
                },
            );

            let qualified_name = format!("{}::{}", enum_qualified_name, variant.name.as_str());
            self.mir_package
                .borrow_mut()
                .enum_variant_names
                .insert(qualified_name.clone(), variant.def_id.clone());
            self.mir_package
                .borrow_mut()
                .enum_variant_names
                .entry(variant.name.as_str().to_string())
                .or_insert(variant.def_id.clone());
        }

        self.mir_package
            .borrow_mut()
            .enum_defs_by_name
            .entry(enum_qualified_name.clone())
            .or_insert(def_id.clone());
        self.mir_package.borrow_mut().enum_defs.insert(
            def_id.clone(),
            EnumDefinition {
                def_id,
                name: enum_qualified_name,
                generics,
                variants,
            },
        );
    }

    // Resolve field types and layouts only after every canonical ADT identity
    // has been registered; dependency definitions arrive in hash-map order.
    pub(super) fn finalize_adt_definitions(&mut self, program: &hir::HirPackage) {
        for item in &program.items {
            self.current_item_path = self
                .hir_def_path(item.def_id.clone())
                .map(|path| path.join("::"));
            match &item.kind {
                hir::ItemKind::Struct(strukt) => {
                    let mir_fields = strukt
                        .fields
                        .iter()
                        .map(|field| mir::ty::FieldDef {
                            did: item.def_id.clone(),
                            ident: mir::Symbol::from(field.name.as_str()),
                            vis: mir::ty::Visibility::Public,
                            ty: self.lower_type_expr(&field.ty),
                        })
                        .collect();
                    let mir_variant = mir::ty::VariantDef {
                        def_id: item.def_id.clone(),
                        ctor_def_id: None,
                        ident: mir::Symbol::from(strukt.name.as_str()),
                        discr: mir::ty::VariantDiscr::Relative(0),
                        fields: mir_fields,
                        ctor_kind: mir::ty::CtorKind::Fn,
                        is_recovered: false,
                    };
                    self.mir_package.borrow_mut().adt_defs.insert(
                        item.def_id.clone(),
                        mir::ty::AdtDef {
                            did: item.def_id.clone(),
                            variants: vec![mir_variant],
                            flags: mir::ty::AdtFlags::from_bits_retain(0),
                            repr: mir::ty::ReprOptions {
                                int: None,
                                align: None,
                                pack: None,
                                flags: mir::ty::ReprFlags::empty(),
                                field_shuffle_seed: 0,
                            },
                        },
                    );
                    if strukt.generics.params.is_empty() {
                        let _ =
                            self.struct_layout_for_instance(item.def_id.clone(), &[], item.span);
                    }
                }
                hir::ItemKind::Enum(enm) if enm.generics.params.is_empty() => {
                    let _ = self.enum_layout_for_instance(item.def_id.clone(), &[], item.span);
                    // Register a real, nominal `AdtDef` for this enum too
                    // (structs already get one above) — this is what lets
                    // `mir_package.adt_defs` export enums to mir_to_lir at all.
                    // Reuses `adt_shell_ty`'s construction (real variant
                    // idents/discriminants, empty per-variant `fields` —
                    // payload types are supplied separately via the
                    // exported `EnumLayout` data, not via `VariantDef
                    // ::fields`, to avoid computing that shape twice).
                    if let Some(Ty {
                        kind: TyKind::Adt(adt, _),
                    }) = self.adt_shell_ty(item.def_id.clone(), &[])
                    {
                        self.mir_package
                            .borrow_mut()
                            .adt_defs
                            .insert(item.def_id.clone(), adt);
                    }
                }
                _ => {}
            }
        }
        self.current_item_path = None;
    }

    pub(crate) fn struct_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<StructLayout> {
        let key = StructLayoutKey {
            def_id: def_id.clone(),
            args: args.to_vec(),
        };

        if let Some(layout) = self.mir_package.borrow().struct_layouts.get(&key).cloned() {
            return Some(layout.clone());
        }

        let Some(struct_def) = self.struct_def(&def_id) else {
            self.emit_error(span, "struct definition not registered");
            return None;
        };

        if self.struct_layouts_in_progress.contains(&key) {
            self.emit_error(
                span,
                format!("recursive type `{}` has infinite size", struct_def.name),
            );
            let opaque = self.opaque_ty(&struct_def.name);
            return Some(StructLayout {
                ty: opaque,
                field_tys: Vec::new(),
            });
        }

        if struct_def.generics.len() != args.len() {
            self.emit_error(
                span,
                format!(
                    "struct `{}` expects {} generic arguments, got {}",
                    struct_def.name,
                    struct_def.generics.len(),
                    args.len()
                ),
            );
            return None;
        }

        let mut substs = HashMap::new();
        for (name, ty) in struct_def.generics.iter().zip(args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }

        self.struct_layouts_in_progress.insert(key.clone());

        let mut field_tys = Vec::with_capacity(struct_def.fields.len());
        for field in &struct_def.fields {
            field_tys.push(self.lower_type_expr_with_substs(&field.ty, &substs));
        }

        let struct_ty = Ty {
            kind: TyKind::Tuple(field_tys.iter().cloned().map(Box::new).collect()),
        };

        let layout = StructLayout {
            ty: struct_ty.clone(),
            field_tys,
        };

        self.mir_package
            .borrow_mut()
            .struct_layouts
            .insert(key.clone(), layout.clone());
        self.mir_package
            .borrow_mut()
            .struct_layouts_by_ty
            .insert(struct_ty, key.clone());
        self.struct_layouts_in_progress.remove(&key);

        let field_tys = layout.field_tys.clone();
        for field_ty in &field_tys {
            if let TyKind::Adt(adt, substs) = &field_ty.kind {
                let is_struct = self.mir_package.borrow().struct_defs.contains_key(&adt.did);
                let is_enum =
                    !is_struct && self.mir_package.borrow().enum_defs.contains_key(&adt.did);
                if !is_struct && !is_enum {
                    continue;
                }
                let types: Vec<Ty> = substs
                    .iter()
                    .filter_map(|a| match a {
                        mir::ty::GenericArg::Type(t) => Some(t.clone()),
                        _ => None,
                    })
                    .collect();
                // `adt.did` is either a struct or an enum, never both —
                // calling the non-matching layout function regardless would
                // spuriously report "definition not registered".
                if is_struct {
                    let _ = self.struct_layout_for_instance(adt.did.clone(), &types, span);
                } else {
                    let _ = self.enum_layout_for_instance(adt.did.clone(), &types, span);
                }
            }
        }

        Some(layout)
    }

    pub(crate) fn struct_layout_for_ty(&self, ty: &Ty) -> Option<StructLayout> {
        let key = self
            .mir_package
            .borrow()
            .struct_layouts_by_ty
            .get(ty)
            .cloned()?;
        self.mir_package.borrow().struct_layouts.get(&key).cloned()
    }

    /// Exact-match counterpart to `enum_layout_for_ty`'s fuzzy scan — an
    /// O(1) lookup from a flattened-tuple enum shape back to its concrete
    /// `EnumLayout` (which carries the original generic args in
    /// `EnumLayout.args`). Prefer this everywhere a concrete instantiation
    /// is expected; fall back to the fuzzy scan only when this misses.
    pub(crate) fn enum_layout_for_ty_exact(&self, ty: &Ty) -> Option<EnumLayout> {
        let key = self
            .mir_package
            .borrow()
            .enum_layouts_by_ty
            .get(ty)
            .cloned()?;
        self.mir_package.borrow().enum_layouts.get(&key).cloned()
    }

    pub(super) fn enum_payload_types(
        &mut self,
        payload: &Option<hir::TypeExpr>,
        substs: &HashMap<String, Ty>,
    ) -> Vec<Ty> {
        let Some(payload) = payload else {
            return Vec::new();
        };
        // A genuine multi-field tuple-variant declaration (`Bar(A, B)`) is
        // represented at the HIR level as a single `TypeExprKind::Tuple`
        // payload — one element per declared field — so unpacking it into
        // one payload slot per element (below) is correct. But a
        // single-field variant (`Ok(T)`) is represented as `T` directly,
        // *not* wrapped in a one-element tuple, so it must always produce
        // exactly one payload slot regardless of what `T` substitutes to —
        // deciding arity from the *substituted* type instead (as
        // `enum_payload_types_from_ty` alone would) breaks the moment `T`
        // resolves to `()` (itself a tuple type, indistinguishable from a
        // zero-field variant): `Result<(), E>::Ok(())` would wrongly
        // compute 0 payload values instead of 1. Check the shape of the
        // *declaration* first, before substitution, to keep genuine arity
        // and "this one field's type happens to be a tuple" distinct.
        if let hir::TypeExprKind::Tuple(elements) = &payload.kind {
            return elements
                .iter()
                .map(|element| self.lower_type_expr_with_substs(element, substs))
                .collect();
        }
        let payload_ty = self.lower_type_expr_with_substs(payload, substs);
        vec![payload_ty]
    }

    pub(super) fn enum_payload_types_from_ty(&self, ty: &Ty) -> Vec<Ty> {
        match &ty.kind {
            TyKind::Tuple(fields) => fields.iter().map(|f| (**f).clone()).collect(),
            _ if Self::is_unit_ty(ty) => Vec::new(),
            _ => vec![ty.clone()],
        }
    }

    pub(crate) fn enum_variant_payloads_for_args(
        &mut self,
        variant: &EnumVariantInfo,
        args: &[Ty],
        span: Span,
    ) -> Option<Vec<Ty>> {
        // Only `generics` (a short `Vec<String>`) and the one matched
        // variant's own `payload` are actually needed below — clone just
        // those instead of the whole `EnumDefinition` (every variant of
        // the enum, needed or not).
        let enum_def = self
            .mir_package
            .borrow()
            .enum_defs
            .get(&variant.enum_def)
            .cloned()?;
        if enum_def.generics.len() != args.len() {
            let name = enum_def.name.clone();
            let generics_len = enum_def.generics.len();
            self.emit_error(
                span,
                format!(
                    "enum `{}` expects {} generic arguments, got {}",
                    name,
                    generics_len,
                    args.len()
                ),
            );
            return None;
        }
        let generics = enum_def.generics.clone();
        let payload = enum_def
            .variants
            .iter()
            .find(|def| def.def_id == variant.def_id)?
            .payload
            .clone();

        let mut substs = HashMap::new();
        for (name, ty) in generics.iter().zip(args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }
        Some(self.enum_payload_types(&payload, &substs))
    }

    pub(crate) fn enum_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<EnumLayout> {
        let key = EnumLayoutKey {
            def_id: def_id.clone(),
            args: args.to_vec(),
        };

        if let Some(layout) = self.mir_package.borrow().enum_layouts.get(&key).cloned() {
            return Some(layout.clone());
        }

        let Some(enum_def) = self.mir_package.borrow().enum_defs.get(&def_id).cloned() else {
            self.emit_error(span, "enum definition not registered");
            return None;
        };

        if self.enum_layouts_in_progress.contains(&key) {
            self.emit_error(
                span,
                format!("recursive type `{}` has infinite size", enum_def.name),
            );
            let opaque = self.opaque_ty(&enum_def.name);
            return Some(EnumLayout {
                def_id,
                args: args.to_vec(),
                tag_ty: Ty {
                    kind: TyKind::Int(IntTy::Isize),
                },
                payload_tys: Vec::new(),
                enum_ty: opaque,
                variant_payloads: HashMap::new(),
            });
        }

        if enum_def.generics.len() != args.len() {
            self.emit_error(
                span,
                format!(
                    "enum `{}` expects {} generic arguments, got {}",
                    enum_def.name,
                    enum_def.generics.len(),
                    args.len()
                ),
            );
            return None;
        }

        let mut substs = HashMap::new();
        for (name, ty) in enum_def.generics.iter().zip(args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }

        self.enum_layouts_in_progress.insert(key.clone());

        let tag_ty = Ty {
            kind: TyKind::Int(IntTy::Isize),
        };
        let mut payload_layout: Vec<Ty> = Vec::new();
        // Parallel to `payload_layout` — the largest size seen so far for
        // each slot, across every variant that uses it. Needed once a slot
        // opaques out (heterogeneous per-variant types): the opaque
        // placeholder has no fields of its own to size, but real runtime
        // storage for that slot must still fit whichever variant is
        // actually active, so its size is `max` over all contributors, not
        // any single one of them (see `opaque_ty_sizes`).
        let mut payload_slot_sizes: Vec<u64> = Vec::new();
        let mut variant_payloads = HashMap::new();
        let mut has_payload = false;
        let is_union_enum = enum_def.name.starts_with("__union_");

        for variant in &enum_def.variants {
            let payload_tys = if is_union_enum {
                if let Some(payload) = variant.payload.as_ref() {
                    let payload_ty = self.lower_type_expr_with_substs(payload, &substs);
                    if let TyKind::Adt(adt, _) = &payload_ty.kind {
                        // JUSTIFY: layout may be uncomputable for forward-referenced
                        // types; computed lazily when needed later.
                        if self
                            .struct_layout_for_instance(adt.did.clone(), &[], span)
                            .is_none()
                        {
                            self.emit_warning(
                                span,
                                "struct layout computation returned None for variant payload",
                            );
                        }
                    }
                    if let Some(layout) = self.struct_layout_for_ty(&payload_ty) {
                        layout.field_tys.clone()
                    } else {
                        self.enum_payload_types_from_ty(&payload_ty)
                    }
                } else {
                    Vec::new()
                }
            } else {
                self.enum_payload_types(&variant.payload, &substs)
            };
            if !payload_tys.is_empty() {
                has_payload = true;
            }
            for (idx, ty) in payload_tys.iter().enumerate() {
                let ty_size = self.size_of_ty(ty, span).unwrap_or(0);
                let slot_ty = if let Some(existing) = payload_layout.get_mut(idx) {
                    if existing != ty {
                        // Opaque out *this* mismatched shared slot only —
                        // this must not also flip `is_union_enum` (that
                        // flag exists to identify genuine synthetic
                        // `__union_`-prefixed enums, which flatten a
                        // struct payload's own fields into the shared
                        // slots; it's name-derived and set once above).
                        // Any real multi-variant enum with heterogeneous
                        // per-variant payload types (e.g. `Value` with
                        // `Bool(bool)`/`Number(Number)`/`Array(Vec<Value>)`
                        // /etc.) hits a slot-0 mismatch on its very second
                        // variant — previously that mid-loop mutation made
                        // every subsequent variant wrongly take the
                        // struct-field-flattening branch below (turning
                        // `Object(Vec<Field>)`'s one `Vec<Field>` payload
                        // into three separate payload slots for `Vec`'s
                        // own `ptr`/`len`/`capacity` fields).
                        let opaque_name = format!("{}::payload{}", enum_def.name, idx);
                        *existing = self.opaque_ty(&opaque_name);
                    }
                    if let Some(slot_size) = payload_slot_sizes.get_mut(idx) {
                        *slot_size = (*slot_size).max(ty_size);
                    }
                    None
                } else if is_union_enum {
                    // Unrelated to the mismatch case above: a synthetic
                    // `__union_` slot is *always* opaque from its first
                    // use (pre-existing behavior) since its real type
                    // varies per variant by construction, not by accident.
                    let opaque_name = format!("{}::payload{}", enum_def.name, idx);
                    Some(self.opaque_ty(&opaque_name))
                } else {
                    Some(ty.clone())
                };
                if let Some(slot_ty) = slot_ty {
                    payload_layout.push(slot_ty);
                    payload_slot_sizes.push(ty_size);
                }
                // Whatever the reason a slot ended up opaque (mismatch
                // above, or always-opaque union slot), its real storage
                // must fit whichever variant is actually active, so record
                // the size unconditionally from the *current* type's shape
                // rather than re-deriving "why" it's opaque here.
                if self.is_opaque_ty(&payload_layout[idx]) {
                    let opaque_name = format!("{}::payload{}", enum_def.name, idx);
                    let size = payload_slot_sizes[idx];
                    self.mir_package
                        .borrow_mut()
                        .opaque_ty_sizes
                        .insert(opaque_name, size);
                }
            }
            variant_payloads.insert(variant.def_id.clone(), payload_tys);
        }

        let enum_ty = if has_payload {
            let mut fields = Vec::with_capacity(1 + payload_layout.len());
            fields.push(Box::new(tag_ty.clone()));
            fields.extend(payload_layout.iter().cloned().map(Box::new));
            Ty {
                kind: TyKind::Tuple(fields),
            }
        } else {
            tag_ty.clone()
        };

        let layout = EnumLayout {
            def_id,
            args: args.to_vec(),
            tag_ty: tag_ty.clone(),
            payload_tys: payload_layout.clone(),
            enum_ty: enum_ty.clone(),
            variant_payloads,
        };

        self.mir_package
            .borrow_mut()
            .enum_layouts
            .insert(key.clone(), layout.clone());
        self.mir_package
            .borrow_mut()
            .enum_layouts_by_ty
            .insert(enum_ty.clone(), key.clone());
        self.enum_layouts_in_progress.remove(&key);

        let payload_tys = layout.payload_tys.clone();
        for field_ty in &payload_tys {
            if let TyKind::Adt(adt, substs) = &field_ty.kind {
                let is_struct = self.mir_package.borrow().struct_defs.contains_key(&adt.did);
                let is_enum =
                    !is_struct && self.mir_package.borrow().enum_defs.contains_key(&adt.did);
                if !is_struct && !is_enum {
                    continue;
                }
                let types: Vec<Ty> = substs
                    .iter()
                    .filter_map(|a| match a {
                        mir::ty::GenericArg::Type(t) => Some(t.clone()),
                        _ => None,
                    })
                    .collect();
                // Same as above: `adt.did` is either a struct or an enum,
                // never both — only call the layout function for the kind
                // it actually is.
                if is_struct {
                    let _ = self.struct_layout_for_instance(adt.did.clone(), &types, span);
                } else {
                    let _ = self.enum_layout_for_instance(adt.did.clone(), &types, span);
                }
            }
        }

        if !has_payload {
            for variant in &enum_def.variants {
                if self
                    .mir_package
                    .borrow()
                    .const_values
                    .contains_key(&variant.def_id)
                {
                    continue;
                }
                let constant = mir::Constant {
                    span,
                    ty: enum_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(variant.discriminant),
                };
                self.mir_package.borrow_mut().const_values.insert(
                    variant.def_id.clone(),
                    ConstInfo {
                        ty: enum_ty.clone(),
                        value: constant,
                    },
                );
            }
        }

        Some(layout)
    }

    pub(crate) fn lower_generic_args(
        &mut self,
        args: Option<&hir::GenericArgs>,
        span: Span,
    ) -> Vec<Ty> {
        let Some(args) = args else {
            return Vec::new();
        };
        let mut lowered = Vec::new();
        for arg in &args.args {
            match arg {
                hir::GenericArg::Type(ty) => lowered.push(self.lower_type_expr(ty)),
                hir::GenericArg::Const(_) => {
                    self.emit_warning(span, "const generics are ignored during MIR lowering");
                }
            }
        }
        lowered
    }

    pub(super) fn lower_type_expr_with_substs(
        &mut self,
        ty_expr: &hir::TypeExpr,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        match &ty_expr.kind {
            hir::TypeExprKind::Primitive(primitive) => {
                self.lower_primitive_type(primitive, ty_expr.span)
            }
            hir::TypeExprKind::Structural(structural) => {
                self.lower_structural_type_expr(structural, ty_expr.span)
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => {
                self.lower_type_binary_op_expr(type_op, ty_expr.span)
            }
            hir::TypeExprKind::Tuple(elements) => Ty {
                kind: TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.lower_type_expr_with_substs(elem, substs)))
                        .collect(),
                ),
            },
            hir::TypeExprKind::Array(elem, len_expr) => {
                let elem_ty = self.lower_type_expr_with_substs(elem, substs);
                let len = len_expr
                    .as_ref()
                    .and_then(|expr| self.eval_type_length(expr))
                    .unwrap_or(0);
                Ty {
                    kind: TyKind::Array(
                        Box::new(elem_ty),
                        ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                            data: len as u128,
                            size: 8,
                        }))),
                    ),
                }
            }
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr_with_substs(elem, substs);
                Ty {
                    kind: TyKind::Slice(Box::new(elem_ty)),
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                if self.is_string_slice_ref(inner) {
                    return self.string_slice_ty();
                }
                let inner_ty = self.lower_type_expr_with_substs(inner, substs);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                let inner_ty = self.lower_type_expr_with_substs(inner, substs);
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(inner_ty),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            hir::TypeExprKind::Path(path) => {
                if let Some(first) = path.segments.first() {
                    if path.segments.len() == 1 && first.args.is_none() {
                        if let Some(subst) = substs.get(first.name.as_str()) {
                            return subst.clone();
                        }
                    }
                }

                if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                    if self.mir_package.borrow().enum_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| {
                                args.args
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        hir::GenericArg::Type(ty) => {
                                            Some(self.lower_generic_type_arg(ty, substs))
                                        }
                                        hir::GenericArg::Const(_) => {
                                            self.emit_warning(
                                                ty_expr.span,
                                                "const generics are ignored during MIR lowering",
                                            );
                                            None
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        if let Some(layout) =
                            self.enum_layout_for_instance(def_id.clone(), &args, ty_expr.span)
                        {
                            return self.nominal_enum_ty(&layout);
                        }
                        return self.error_ty();
                    }
                    if self.mir_package.borrow().struct_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| {
                                args.args
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        hir::GenericArg::Type(ty) => {
                                            Some(self.lower_generic_type_arg(ty, substs))
                                        }
                                        hir::GenericArg::Const(_) => {
                                            self.emit_warning(
                                                ty_expr.span,
                                                "const generics are ignored during MIR lowering",
                                            );
                                            None
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        if let Some(layout) =
                            self.struct_layout_for_instance(def_id.clone(), &args, ty_expr.span)
                        {
                            return layout.ty.clone();
                        }
                        return self.error_ty();
                    }
                }

                self.lower_path_type(path, ty_expr.span)
            }
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                let inputs = fn_ptr
                    .inputs
                    .iter()
                    .map(|ty| Box::new(self.lower_type_expr_with_substs(ty, substs)))
                    .collect();
                let output = Box::new(self.lower_type_expr_with_substs(&fn_ptr.output, substs));
                Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs,
                                output,
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::C { unwind: false },
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                }
            }
            hir::TypeExprKind::Never => Ty {
                kind: TyKind::Never,
            },
            hir::TypeExprKind::Infer => self.error_ty(),
            hir::TypeExprKind::Error => self.error_ty(),
            hir::TypeExprKind::ConstBlock(_, _) => self
                .typeck_type_expr_type(ty_expr.hir_id.clone())
                .unwrap_or_else(|| self.error_ty()),
            hir::TypeExprKind::Type => Ty { kind: TyKind::Type },
            hir::TypeExprKind::Any => Ty { kind: TyKind::Any },
            hir::TypeExprKind::Refinement { base, .. } => {
                self.lower_type_expr_with_substs(base, substs)
            }
            hir::TypeExprKind::LiteralString(_) => self.string_slice_ty(),
        }
    }

    pub(crate) fn raw_string_ptr_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    pub(crate) fn string_slice_ty(&self) -> Ty {
        Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        }
    }

    pub(crate) fn is_string_slice_ref(&self, inner: &hir::TypeExpr) -> bool {
        match &inner.kind {
            hir::TypeExprKind::Primitive(TypePrimitive::String) => true,
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str() == "str")
                .unwrap_or(false),
            _ => false,
        }
    }

    pub(super) fn eval_int_expr(&mut self, expr: &hir::Expr) -> Option<i64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value),
            hir::ExprKind::Unary(hir::UnOp::Neg, inner) => self.eval_int_expr(inner).map(|v| -v),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let left = self.eval_int_expr(lhs)?;
                let right = self.eval_int_expr(rhs)?;
                match op {
                    hir::BinOp::Add => Some(left.saturating_add(right)),
                    hir::BinOp::Sub => Some(left.saturating_sub(right)),
                    hir::BinOp::Mul => Some(left.saturating_mul(right)),
                    hir::BinOp::Div => Some(if right != 0 {
                        left / right
                    } else {
                        return None;
                    }),
                    hir::BinOp::Rem => Some(if right != 0 {
                        left % right
                    } else {
                        return None;
                    }),
                    // Enum-discriminant expressions routinely use bit
                    // ops rather than plain arithmetic (real
                    // `core::mem::alignment`'s own `AlignmentEnum`, whose
                    // every variant's discriminant is `1 << N`).
                    hir::BinOp::Shl => Some(left.wrapping_shl(right as u32)),
                    hir::BinOp::Shr => Some(left.wrapping_shr(right as u32)),
                    hir::BinOp::BitOr => Some(left | right),
                    hir::BinOp::BitAnd => Some(left & right),
                    hir::BinOp::BitXor => Some(left ^ right),
                    _ => None,
                }
            }
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(info) = self.ensure_const_info(def_id.clone()) {
                        match &info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value),
                            mir::ConstantKind::UInt(value) => Some(*value as i64),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            hir::ExprKind::Cast(inner, _) => self.eval_int_expr(inner),
            _ => None,
        }
    }

    /// The const-value counterpart to `ensure_method_info`/
    /// `compute_adt_layout`'s "check the cache; lazily register on a miss;
    /// proceed" shape. Every `const_values` read should go through this
    /// rather than reading the map directly, so a const referenced by an
    /// item lowered before it in `program.items` order resolves exactly
    /// the same as one referenced after it, with no eager pre-pass needed.
    pub fn ensure_const_info(&mut self, def_id: hir::DefId) -> Option<ConstInfo> {
        if let Some(info) = self.mir_package.borrow().const_values.get(&def_id).cloned() {
            return Some(info);
        }
        let _ = self.ensure_item_lowered(def_id.clone());
        self.mir_package.borrow().const_values.get(&def_id).cloned()
    }

    pub(super) fn struct_name_from_type(&self, ty: &hir::TypeExpr) -> Option<String> {
        match &ty.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| String::from(seg.name.clone())),
            hir::TypeExprKind::Ref(inner) | hir::TypeExprKind::Ptr(inner) => {
                self.struct_name_from_type(inner)
            }
            _ => None,
        }
    }

    /// `HashMap`'s `from`/`len`/`get_unchecked` get a bespoke intrinsic
    /// lowering elsewhere (see `lower_call`'s `HashMap::from` handling) and
    /// must not also be registered as ordinary methods — shared by both
    /// the signature-registration path (`register_impl_signature_for_item`)
    /// and the real body-lowering path (`lower_impl`) so the two can never
    /// diverge on which methods this applies to.
    pub(super) fn is_hashmap_intrinsic_method(
        struct_name: Option<&str>,
        method_name: &str,
    ) -> bool {
        let is_hashmap_impl = struct_name
            .map(|name| name.ends_with("HashMap"))
            .unwrap_or(false);
        let is_hashmap_method = matches!(method_name, "from" | "len" | "get_unchecked")
            || method_name.ends_with("::from")
            || method_name.ends_with("::len")
            || method_name.ends_with("::get_unchecked");
        is_hashmap_impl && is_hashmap_method
    }

    /// Signature-only pre-pass over every impl in the package, run before
    /// any bodies are lowered (see `lower_program`) — so a call to a
    /// method/associated function resolves regardless of which module
    /// declares the caller vs. the callee. Without this, `lower_impl`'s
    /// per-method registration (only inserted *after* a body successfully
    /// lowers, or — for generic methods — only when `lower_impl` itself
    /// reaches that impl item) makes lookup success depend on
    /// `program.items` order, so a forward reference across modules (e.g.
    /// `std::alloc`'s `Vec::join` calling `std::string::String::new`, where
    /// `alloc` is declared before `string` in `std/mod.fp`) fails with
    /// "unresolved call target" purely because of declaration order,
    /// aborting the rest of the pass. Non-generic methods get a fully
    /// lowered signature registered now (`register_method_lowering_info`);
    /// generic methods (e.g. `impl<T> Vec<T>`) can't — their signature
    /// needs concrete substs only known at a call site — so they instead
    /// get their raw, unspecialized HIR registered
    /// (`register_generic_method_definition`), which specialization can
    /// find and lower on demand regardless of order. Mirrors `lower_impl`'s
    /// own skip conditions (HashMap special-case) verbatim so this
    /// pre-pass never registers something the main pass would skip.
    pub(super) fn register_impl_signatures(&mut self, impl_block: &hir::Impl) {
        let struct_name = self.struct_name_from_type(&impl_block.self_ty);
        let method_context = self.make_method_context(
            &impl_block.self_ty,
            &assoc_types_from_impl_items(&impl_block.items),
        );
        for impl_item in &impl_block.items {
            self.register_impl_signature_for_item(
                struct_name.as_deref(),
                method_context.as_ref(),
                impl_block,
                impl_item,
            );
        }
    }

    /// Registers exactly one impl item's signature — extracted so
    /// `register_impl_signatures`'s eager, whole-impl pre-pass and
    /// `try_lazily_register_method`'s on-demand, single-method lookup
    /// share one implementation and can never drift apart, the same way
    /// `try_lazily_register_adt` reuses `register_struct`/`register_enum`
    /// rather than re-deriving their logic.
    pub(super) fn register_impl_signature_for_item(
        &mut self,
        struct_name: Option<&str>,
        method_context: Option<&MethodContext>,
        impl_block: &hir::Impl,
        impl_item: &hir::ImplItem,
    ) {
        let hir::ImplItemKind::Method(function) = &impl_item.kind else {
            return;
        };
        let method_name = function.sig.name.as_str();
        if Self::is_hashmap_intrinsic_method(struct_name, method_name) {
            return;
        }
        let impl_is_generic = !impl_block.generics.params.is_empty();
        if impl_is_generic || !function.sig.generics.params.is_empty() {
            self.register_generic_method_definition(
                struct_name,
                method_context,
                impl_item,
                function,
                impl_block,
            );
            return;
        }
        let Some(struct_name) = struct_name else {
            return;
        };
        let method_span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or_else(Span::null);
        self.mir_package.borrow_mut().method_hir_defs.insert(
            impl_item.def_id.clone(),
            MethodHirRef {
                function: function.clone(),
                span: method_span,
                method_context: method_context.cloned(),
            },
        );
        let sig = self.lower_function_sig(&function.sig, method_context);
        self.register_method_lowering_info(struct_name, method_context, impl_item, function, sig);
    }

    /// On-demand registration for a method defined in a *different*
    /// package (std/libc/etc.) — reached only when `def_id` isn't already
    /// registered locally. Unlike structs/enums/consts/functions, an
    /// individual method's own `DefId` is never a key in `def_map` (only
    /// the *owning* `Impl` item's own top-level `DefId` is), so this
    /// resolves the owning impl via `HirProgram::member_owner` (the
    /// `member_to_owning_item` reverse index maintained during HIR
    /// building) and then finds the member within it by `DefId`.
    pub(super) fn try_lazily_register_method(&mut self, def_id: hir::DefId) {
        let Some(owning_def_id) = self.hir_program.member_owner(def_id.clone()) else {
            return;
        };
        let Some(owning_item) = self.hir_program.item(owning_def_id).cloned() else {
            return;
        };
        let hir::ItemKind::Impl(impl_block) = &owning_item.kind else {
            return;
        };
        let Some(impl_item) = impl_block
            .items
            .iter()
            .find(|item| item.def_id == def_id)
            .cloned()
        else {
            return;
        };
        let struct_name = self.struct_name_from_type(&impl_block.self_ty);
        let method_context = self.make_method_context(
            &impl_block.self_ty,
            &assoc_types_from_impl_items(&impl_block.items),
        );
        self.register_impl_signature_for_item(
            struct_name.as_deref(),
            method_context.as_ref(),
            impl_block,
            &impl_item,
        );
    }

    /// Uniform method-signature lookup: check the cache, lazily register
    /// on a miss, then check again — the exact "check the cache; lazily
    /// register on a miss; proceed" shape `compute_adt_layout` already
    /// uses for ADTs. Every method-resolution call site should go through
    /// this rather than reading `method_lookup_by_def` directly, so a
    /// method defined in this package or any dependency's resolves the
    /// same way, with no caller-visible distinction between the two.
    pub(crate) fn ensure_method_info(&mut self, def_id: hir::DefId) -> Option<MethodLoweringInfo> {
        if let Some(info) = self
            .mir_package
            .borrow()
            .method_lookup_by_def
            .get(&def_id)
            .cloned()
        {
            return Some(info.clone());
        }
        self.try_lazily_register_method(def_id.clone());
        self.mir_package
            .borrow()
            .method_lookup_by_def
            .get(&def_id)
            .cloned()
    }

    /// Generic counterpart to `ensure_method_info` — same "check the
    /// cache; lazily register on a miss; proceed" shape, but reading
    /// `method_defs_by_def` (a generic method's raw, unspecialized HIR;
    /// its real signature needs concrete substs only known at the call
    /// site — see `register_generic_method_definition`) instead of
    /// `method_lookup_by_def`. `try_lazily_register_method` itself
    /// already dispatches to whichever of the two a given method needs.
    pub(crate) fn ensure_generic_method_def(
        &mut self,
        def_id: hir::DefId,
    ) -> Option<MethodDefinition> {
        if let Some(def) = self
            .mir_package
            .borrow()
            .method_defs_by_def
            .get(&def_id)
            .cloned()
        {
            return Some(def.clone());
        }
        self.try_lazily_register_method(def_id.clone());
        self.mir_package
            .borrow()
            .method_defs_by_def
            .get(&def_id)
            .cloned()
    }

    /// Shared by `register_impl_signatures` (order-independent pre-pass)
    /// and `lower_impl` (real per-item pass) so the two paths can't drift
    /// apart — the generic-method counterpart to
    /// `register_method_lowering_info`. Registers a generic method's raw,
    /// unspecialized HIR so call-site specialization
    /// (`ensure_method_specialization`) can find it regardless of
    /// `program.items` order; the body itself is only ever lowered later,
    /// once specialized for a concrete call site's substs. `entry(...)
    /// .or_insert` rather than an unconditional overwrite since this can
    /// now run twice for the same method (once from the pre-pass, once
    /// from `lower_impl` reaching the same item) — both calls would
    /// produce an identical `MethodDefinition` from the same immutable
    /// HIR, so it doesn't matter which one wins, only that neither panics
    /// or does redundant work.
    pub(super) fn register_generic_method_definition(
        &mut self,
        struct_name: Option<&str>,
        method_context: Option<&MethodContext>,
        impl_item: &hir::ImplItem,
        function: &hir::Function,
        impl_block: &hir::Impl,
    ) {
        let qualified_name = match struct_name {
            Some(name) => format!("{}::{}", name, function.sig.name),
            None => function.sig.name.as_str().to_string(),
        };
        let def = MethodDefinition {
            def_id: impl_item.def_id.clone(),
            function: function.clone(),
            impl_generics: impl_block.generics.clone(),
            self_ty: impl_block.self_ty.clone(),
            self_def: method_context.and_then(|ctx| ctx.def_id.clone()),
            method_name: qualified_name.clone(),
            assoc_types: assoc_types_from_impl_items(&impl_block.items),
        };
        if let Some(ref self_def) = def.self_def {
            self.mir_package
                .borrow_mut()
                .method_defs_by_self_and_name
                .entry((self_def.clone(), def.function.sig.name.as_str().to_string()))
                .or_insert(impl_item.def_id.clone());
        }
        self.mir_package
            .borrow_mut()
            .method_defs_by_def
            .entry(impl_item.def_id.clone())
            .or_insert_with(|| def.clone());
        self.mir_package
            .borrow_mut()
            .method_defs
            .entry(qualified_name)
            .or_insert(def);
    }

    /// Disambiguating suffix for a method's qualified/mangled name, when
    /// its impl targets a concrete instantiation of a generic struct —
    /// e.g. `impl Vec<&str> { fn join }` vs `impl Vec<String> { fn join }`
    /// are two already-concrete methods (this is only reached for
    /// non-generic impls; a truly generic `impl<T> Vec<T>` is
    /// disambiguated further downstream instead, per call-site
    /// substitution). Every qualified-name computation for a method
    /// (`register_method_lowering_info`, `lower_method`) derives its
    /// struct-path prefix purely from the struct's own module path
    /// segments, dropping any generic arguments the impl specializes —
    /// so two impls of this shape would otherwise both compute the
    /// identical name ("std::alloc::Vec::join"), colliding once both
    /// reach the same LIR workspace ("duplicate LIR artifact
    /// `Vec__join`"). Hashes the fully resolved Self type (not just its
    /// raw HIR argument syntax) so aliases/paths that resolve to the same
    /// concrete type still collide the way they should.
    pub(super) fn method_self_type_spec_suffix(
        &self,
        method_context: Option<&MethodContext>,
    ) -> Option<String> {
        match method_context.map(|ctx| &ctx.mir_self_ty.kind) {
            Some(TyKind::Adt(_, substs)) if !substs.is_empty() => {
                let mut hasher = DefaultHasher::new();
                method_context.unwrap().mir_self_ty.hash(&mut hasher);
                Some(format!("_spec_{:x}", hasher.finish()))
            }
            _ => None,
        }
    }

    /// Shared by `register_impl_signatures` (signature-only pre-pass) and
    /// `lower_impl` (real lowering) so the two paths can never drift apart.
    pub(super) fn register_method_lowering_info(
        &mut self,
        struct_name: &str,
        method_context: Option<&MethodContext>,
        impl_item: &hir::ImplItem,
        function: &hir::Function,
        sig: mir::FunctionSig,
    ) {
        let struct_prefix = method_context
            .and_then(|ctx| {
                if ctx.path.is_empty() {
                    None
                } else {
                    Some(
                        ctx.path
                            .iter()
                            .map(|seg| seg.name.as_str())
                            .collect::<Vec<_>>()
                            .join("::"),
                    )
                }
            })
            .unwrap_or_else(|| struct_name.to_string());
        let fn_name = format!(
            "{}::{}{}",
            struct_prefix,
            function.sig.name.as_str(),
            self.method_self_type_spec_suffix(method_context)
                .unwrap_or_default()
        );
        let fn_ty = self.function_pointer_ty(&sig);
        let struct_def = method_context.and_then(|ctx| ctx.def_id.clone());
        let method_name = function.sig.name.as_str().to_string();
        let impl_item_name = impl_item.name.as_str().to_string();
        let info = MethodLoweringInfo {
            def_id: Some(impl_item.def_id.clone()),
            substs: Vec::new(),
            sig,
            fn_name: fn_name.clone(),
            fn_ty,
            struct_def,
        };

        self.mir_package
            .borrow_mut()
            .method_lookup_by_def
            .insert(impl_item.def_id.clone(), info.clone());
        self.mir_package
            .borrow_mut()
            .method_lookup
            .insert(fn_name, info.clone());
        self.mir_package
            .borrow_mut()
            .method_lookup
            .insert(format!("{}::{}", struct_name, method_name), info.clone());
        self.mir_package
            .borrow_mut()
            .method_lookup
            .insert(format!("{}::{}", struct_name, impl_item_name), info.clone());
        self.mir_package
            .borrow_mut()
            .method_name_output_consensus
            .entry(method_name.clone())
            .and_modify(|existing| {
                if existing.as_ref() != Some(&info.sig.output) {
                    *existing = None;
                }
            })
            .or_insert_with(|| Some(info.sig.output.clone()));
        self.mir_package
            .borrow_mut()
            .struct_methods
            .entry(struct_name.to_string())
            .or_default()
            .insert(method_name, info);
    }

    pub(crate) fn lower_impl(
        &mut self,
        item: &hir::Item,
        impl_block: &hir::Impl,
        output: Option<&mut mir::MirCodeUnit>,
    ) -> Result<()> {
        let mut output = output;
        let mut emit_function =
            |this: &mut Self, mir_item: mir::Item, body_id: mir::BodyId, body: mir::Body| {
                if let Some(program_ref) = output.as_mut() {
                    let program: &mut mir::MirCodeUnit = &mut **program_ref;
                    program.items.push(mir_item);
                    program.bodies.insert(body_id, body);
                } else {
                    this.extra_items.push(mir_item);
                    this.extra_bodies.push((body_id, body));
                }
            };

        let struct_name = self.struct_name_from_type(&impl_block.self_ty);

        let method_context = self.make_method_context(
            &impl_block.self_ty,
            &assoc_types_from_impl_items(&impl_block.items),
        );
        let impl_is_generic = !impl_block.generics.params.is_empty();

        for impl_item in &impl_block.items {
            match &impl_item.kind {
                hir::ImplItemKind::Method(function) => {
                    let method_name = function.sig.name.as_str();
                    if Self::is_hashmap_intrinsic_method(struct_name.as_deref(), method_name) {
                        continue;
                    }
                    if impl_is_generic || !function.sig.generics.params.is_empty() {
                        self.register_generic_method_definition(
                            struct_name.as_deref(),
                            method_context.as_ref(),
                            impl_item,
                            function,
                            impl_block,
                        );
                        continue;
                    }

                    let (mir_item, body_id, body, sig) = self.lower_method(
                        impl_item.def_id.clone(),
                        function,
                        item.span,
                        method_context.as_ref(),
                    )?;
                    emit_function(self, mir_item, body_id, body);

                    if let Some(struct_name) = struct_name.as_deref() {
                        self.register_method_lowering_info(
                            struct_name,
                            method_context.as_ref(),
                            impl_item,
                            function,
                            sig,
                        );
                    }
                }
                hir::ImplItemKind::AssocConst(_const_item) => {
                    // TODO: lower associated constants when needed
                }
                hir::ImplItemKind::AssocType(_assoc_type) => {
                    // Type-level only — nothing to lower into MIR (no
                    // runtime representation), mirrors AssocConst above.
                }
            }
        }

        Ok(())
    }

    pub(super) fn lower_method(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        parent_span: Span,
        method_context: Option<&MethodContext>,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body, mir::FunctionSig)> {
        let body_id = mir::BodyId::new(self.mir_package.borrow_mut().fresh_body_id());

        let sig = self.lower_function_sig(&function.sig, method_context);
        let span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(parent_span);
        let mir_body = BodyBuilder::new(
            self,
            function,
            &sig,
            span,
            method_context.cloned(),
            HashMap::new(),
        )
        .lower()?;

        let method_name = function.sig.name.as_str();
        let qualified_name = match method_context {
            Some(ctx) if !ctx.path.is_empty() => {
                let path = ctx
                    .path
                    .iter()
                    .map(|seg| seg.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("{}::{}", path, method_name)
            }
            _ => method_name.to_string(),
        };
        // See `method_self_type_spec_suffix`'s doc comment — disambiguates
        // e.g. `impl Vec<&str> { fn join }` from `impl Vec<String> { fn join }`.
        let qualified_name = match self.method_self_type_spec_suffix(method_context) {
            Some(suffix) => qualified_name + &suffix,
            None => qualified_name,
        };

        let mir_function = mir::Function {
            name: mir::Symbol::new(qualified_name),
            def_id: Some(def_id),
            substs: Vec::new(),
            sig: sig.clone(),
            body_id,
            abi: self.map_abi(&function.sig.abi),
            is_extern: false,
            attrs: Vec::new(),
        };

        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Function(mir_function),
        };

        Ok((mir_item, body_id, mir_body, sig))
    }

    pub(crate) fn make_method_context(
        &mut self,
        self_ty: &hir::TypeExpr,
        assoc_types: &HashMap<String, hir::TypeExpr>,
    ) -> Option<MethodContext> {
        if let hir::TypeExprKind::Path(path) = &self_ty.kind {
            let def_id = match &path.res {
                Some(hir::Res::Def(def_id)) => Some(def_id.clone()),
                _ => None,
            };
            let mir_self_ty = self.lower_type_expr(self_ty);
            Some(MethodContext {
                def_id,
                path: path.segments.clone(),
                mir_self_ty,
                assoc_types: assoc_types.clone(),
            })
        } else {
            None
        }
    }

    pub(crate) fn struct_field(
        &mut self,
        def_id: hir::DefId,
        struct_ty: &Ty,
        name: &str,
        span: Span,
    ) -> Option<(usize, StructFieldInfo)> {
        let def = self
            .mir_package
            .borrow()
            .struct_defs
            .get(&def_id)
            .cloned()?;
        let idx = *def.field_index.get(name)?;
        let layout = self
            .struct_layout_for_ty(struct_ty)
            .or_else(|| match &struct_ty.kind {
                TyKind::Adt(_, args) => {
                    let type_args =
                        args.iter()
                            .filter_map(|arg| match arg {
                                mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                                mir::ty::GenericArg::Lifetime(_)
                                | mir::ty::GenericArg::Const(_) => None,
                            })
                            .collect::<Vec<_>>();
                    self.struct_layout_for_instance(def_id, &type_args, span)
                }
                _ if self.is_opaque_ty(struct_ty) => {
                    self.struct_layout_for_instance(def_id, &[], span)
                }
                _ => None,
            })?;
        let ty = layout.field_tys.get(idx)?.clone();
        Some((
            idx,
            StructFieldInfo {
                name: name.to_string(),
                ty,
            },
        ))
    }

    pub(crate) fn function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
        Ty {
            kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                binder: mir::ty::Binder {
                    value: mir::ty::FnSig {
                        inputs: sig.inputs.iter().map(|ty| Box::new(ty.clone())).collect(),
                        output: Box::new(sig.output.clone()),
                        c_variadic: false,
                        unsafety: mir::ty::Unsafety::Normal,
                        abi: mir::ty::Abi::Rust,
                    },
                    bound_vars: Vec::new(),
                },
            }),
        }
    }

    pub(crate) fn c_function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
        Ty {
            kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                binder: mir::ty::Binder {
                    value: mir::ty::FnSig {
                        inputs: sig.inputs.iter().map(|ty| Box::new(ty.clone())).collect(),
                        output: Box::new(sig.output.clone()),
                        c_variadic: false,
                        unsafety: mir::ty::Unsafety::Normal,
                        abi: mir::ty::Abi::C { unwind: false },
                    },
                    bound_vars: Vec::new(),
                },
            }),
        }
    }

    pub(crate) fn make_local_decl(&mut self, ty: &Ty, span: Span) -> mir::LocalDecl {
        mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty: ty.clone(),
            user_ty: None,
            source_info: span,
        }
    }

    pub(crate) fn const_len_from_constant(&self, constant: &mir::Constant) -> Option<u64> {
        match &constant.literal {
            mir::ConstantKind::Str(value) => Some(value.len() as u64),
            mir::ConstantKind::Val(mir::ConstValue::List { elements, .. }) => {
                Some(elements.len() as u64)
            }
            mir::ConstantKind::Val(mir::ConstValue::Array(elements)) => Some(elements.len() as u64),
            mir::ConstantKind::Val(mir::ConstValue::Map { entries, .. }) => {
                Some(entries.len() as u64)
            }
            mir::ConstantKind::Val(mir::ConstValue::Tuple(fields)) => Some(fields.len() as u64),
            _ => None,
        }
    }

    pub(crate) fn const_index_value(
        &mut self,
        span: Span,
        constant: &mir::Constant,
        index: &hir::Expr,
    ) -> Option<(mir::Constant, Ty)> {
        let key = self.lower_const_value(index, None)?;
        match &constant.literal {
            mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }) => {
                let idx = match key {
                    mir::ConstValue::Int(value) if value >= 0 => value as usize,
                    mir::ConstValue::UInt(value) => value as usize,
                    _ => {
                        self.emit_error(span, "list index must be a non-negative integer");
                        return None;
                    }
                };
                let value = elements.get(idx)?;
                let constant = self.const_value_to_constant(span, value, elem_ty);
                Some((constant, elem_ty.clone()))
            }
            mir::ConstantKind::Val(mir::ConstValue::Array(elements)) => {
                let idx = match key {
                    mir::ConstValue::Int(value) if value >= 0 => value as usize,
                    mir::ConstValue::UInt(value) => value as usize,
                    _ => {
                        self.emit_error(span, "array index must be a non-negative integer");
                        return None;
                    }
                };
                let TyKind::Array(elem_ty, _) = &constant.ty.kind else {
                    return None;
                };
                let value = elements.get(idx)?;
                let constant = self.const_value_to_constant(span, value, elem_ty);
                Some((constant, (*elem_ty.clone()).clone()))
            }
            mir::ConstantKind::Val(mir::ConstValue::Map {
                entries,
                key_ty: _,
                value_ty,
            }) => {
                let (_, value) = entries
                    .iter()
                    .find(|(entry_key, _)| self.const_value_matches(entry_key, &key))?;
                let constant = self.const_value_to_constant(span, value, value_ty);
                Some((constant, value_ty.clone()))
            }
            _ => None,
        }
    }

    pub(super) fn const_value_matches(&self, lhs: &mir::ConstValue, rhs: &mir::ConstValue) -> bool {
        match (lhs, rhs) {
            (mir::ConstValue::Int(a), mir::ConstValue::Int(b)) => a == b,
            (mir::ConstValue::UInt(a), mir::ConstValue::UInt(b)) => a == b,
            (mir::ConstValue::Int(a), mir::ConstValue::UInt(b)) => *a >= 0 && *a as u64 == *b,
            (mir::ConstValue::UInt(a), mir::ConstValue::Int(b)) => *b >= 0 && *a == *b as u64,
            (mir::ConstValue::Bool(a), mir::ConstValue::Bool(b)) => a == b,
            (mir::ConstValue::Str(a), mir::ConstValue::Str(b)) => a == b,
            (mir::ConstValue::Null, mir::ConstValue::Null) => true,
            (mir::ConstValue::Fn(a), mir::ConstValue::Fn(b)) => a == b,
            _ => lhs == rhs,
        }
    }

    pub(super) fn const_value_to_constant(
        &self,
        span: Span,
        value: &mir::ConstValue,
        ty: &Ty,
    ) -> mir::Constant {
        let literal = match value {
            mir::ConstValue::Bool(value) => mir::ConstantKind::Bool(*value),
            mir::ConstValue::Int(value) => mir::ConstantKind::Int(*value),
            mir::ConstValue::UInt(value) => mir::ConstantKind::UInt(*value),
            mir::ConstValue::Float(value) => mir::ConstantKind::Float(*value),
            mir::ConstValue::Str(value) => mir::ConstantKind::Str(value.clone()),
            mir::ConstValue::Null => mir::ConstantKind::Null,
            mir::ConstValue::Fn(name) => mir::ConstantKind::Fn(name.clone()),
            _ => mir::ConstantKind::Val(value.clone()),
        };
        mir::Constant {
            span,
            ty: ty.clone(),
            user_ty: None,
            literal,
        }
    }

    pub(super) fn lower_literal(&self, lit: &hir::Lit) -> mir::ConstantKind {
        match lit {
            hir::Lit::Bool(value) => mir::ConstantKind::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstantKind::Int(*value),
            hir::Lit::Float(value) => mir::ConstantKind::Float(*value),
            hir::Lit::Str(value) => mir::ConstantKind::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstantKind::Int(*value as i64),
            hir::Lit::Null => mir::ConstantKind::Null,
            hir::Lit::Bytes(bytes) | hir::Lit::CStr(bytes) => {
                mir::ConstantKind::Str(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    pub(crate) fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        let mut message = message.into();
        if let Some(item) = &self.current_item_path {
            message.push_str(&format!(" (in `{item}`)"));
        }
        let diagnostic = Diagnostic::error(message)
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.add_diagnostic(diagnostic);
    }

    pub(crate) fn emit_warning(&mut self, span: Span, message: impl Into<String>) {
        let diagnostic = Diagnostic::warning(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.add_diagnostic(diagnostic);
    }

    pub(crate) fn unit_ty() -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    pub(crate) fn type_ty() -> Ty {
        Ty { kind: TyKind::Type }
    }

    pub(crate) fn is_unit_ty(ty: &Ty) -> bool {
        matches!(&ty.kind, TyKind::Tuple(elements) if elements.is_empty())
    }

    pub(super) fn pointer_sized_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Uint(UintTy::U8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    pub(super) fn sanitize_placeholder_ty(&self, ty: &Ty) -> Ty {
        match &ty.kind {
            TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::RawPtr(_)
            | TyKind::Ref(_, _, _)
            | TyKind::FnPtr(_) => ty.clone(),
            _ => self.pointer_sized_ty(),
        }
    }

    pub(crate) fn sanitize_function_sig(&self, sig: &mir::FunctionSig) -> mir::FunctionSig {
        let inputs = sig
            .inputs
            .iter()
            .map(|ty| self.sanitize_placeholder_ty(ty))
            .collect();
        let output = if Self::is_unit_ty(&sig.output) {
            sig.output.clone()
        } else {
            self.sanitize_placeholder_ty(&sig.output)
        };
        mir::FunctionSig { inputs, output }
    }

    pub(super) fn opaque_ty(&mut self, name: &str) -> Ty {
        if let Some(existing) = self.mir_package.borrow().opaque_types.get(name).cloned() {
            return existing.clone();
        }
        let adt_def_id = self.mir_package.borrow_mut().fresh_synthetic_def_id();
        let variant_def_id = self.mir_package.borrow_mut().fresh_synthetic_def_id();

        let symbol = Symbol::new(name);
        let variant = VariantDef {
            def_id: variant_def_id,
            ctor_def_id: None,
            ident: symbol.clone(),
            discr: VariantDiscr::Relative(0),
            fields: Vec::new(),
            ctor_kind: CtorKind::Const,
            is_recovered: false,
        };

        let adt = AdtDef {
            did: adt_def_id,
            variants: vec![variant],
            flags: AdtFlags::IS_STRUCT,
            repr: ReprOptions {
                int: None,
                align: None,
                pack: None,
                flags: ReprFlags::empty(),
                field_shuffle_seed: 0,
            },
        };

        let ty = Ty {
            kind: TyKind::Adt(adt, Vec::new()),
        };
        self.mir_package
            .borrow_mut()
            .opaque_types
            .insert(name.to_string(), ty.clone());
        ty
    }

    /// Builds a lazy `TyKind::Adt` reference to a *real* (already
    /// registered) struct/enum def, without computing its layout. Unlike
    /// `opaque_ty`, this carries the type's actual `def_id`, so a later
    /// `struct_layout_for_instance`/`enum_layout_for_instance(adt.did, ..)`
    /// call can still resolve it on demand — it's an identity reference,
    /// not a dead end.
    pub(super) fn adt_shell_ty(&mut self, def_id: hir::DefId, args: &[Ty]) -> Option<Ty> {
        let generic_args: Vec<mir::ty::GenericArg> = args
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect();
        if let Some(enum_def) = self.mir_package.borrow().enum_defs.get(&def_id).cloned() {
            let variants = enum_def
                .variants
                .iter()
                .enumerate()
                .map(|(idx, variant)| VariantDef {
                    def_id: variant.def_id.clone(),
                    // Match `fp-typing::hir_typeck`'s own nominal-enum
                    // construction (`check_type_expr`/`path_ty`) exactly —
                    // `ctor_def_id`/`ctor_kind` here, not just `did`/
                    // `variants[].ident` — so a function signature's
                    // typeck-derived `Ty` and this on-demand shell compare
                    // structurally equal for the same enum instantiation.
                    ctor_def_id: Some(variant.def_id.clone()),
                    ident: Symbol::new(&variant.name),
                    discr: VariantDiscr::Relative(idx as u32),
                    fields: Vec::new(),
                    ctor_kind: CtorKind::Fn,
                    is_recovered: false,
                })
                .collect();
            let adt = AdtDef {
                did: def_id,
                variants,
                flags: AdtFlags::IS_ENUM,
                repr: ReprOptions {
                    int: None,
                    align: None,
                    pack: None,
                    flags: ReprFlags::empty(),
                    field_shuffle_seed: 0,
                },
            };
            return Some(Ty {
                kind: TyKind::Adt(adt, generic_args),
            });
        }
        if self.mir_package.borrow().struct_defs.contains_key(&def_id) {
            // Structs carry an *empty* `variants` list (only enums populate
            // it) — see `display_type_name`'s doc comment. A dummy variant
            // here would shadow the `struct_defs` name lookup with an empty
            // ident, breaking name resolution for this shell.
            let adt = AdtDef {
                did: def_id,
                variants: Vec::new(),
                flags: AdtFlags::IS_STRUCT,
                repr: ReprOptions {
                    int: None,
                    align: None,
                    pack: None,
                    flags: ReprFlags::empty(),
                    field_shuffle_seed: 0,
                },
            };
            return Some(Ty {
                kind: TyKind::Adt(adt, generic_args),
            });
        }
        None
    }

    /// `HirToMirLowerer`-level byte-size computation, used while computing an
    /// enum's own layout (`enum_layout_for_instance`, which runs before any
    /// `BodyBuilder`/`type_substs` context exists — `BodyBuilder::
    /// compute_ty_size` isn't reachable here). Mirrors that function's
    /// logic minus the generic-`Param`-via-`type_substs` fallback (payload
    /// types reaching this point are already substituted).
    pub(super) fn size_of_ty(&mut self, ty: &Ty, span: Span) -> Option<u64> {
        match &ty.kind {
            TyKind::Bool => Some(1),
            TyKind::Char => Some(4),
            TyKind::Int(int_ty) => Some(match int_ty {
                IntTy::I8 => 1,
                IntTy::I16 => 2,
                IntTy::I32 => 4,
                IntTy::I64 => 8,
                IntTy::I128 => 16,
                IntTy::Isize => 8,
            }),
            TyKind::Uint(uint_ty) => Some(match uint_ty {
                UintTy::U8 => 1,
                UintTy::U16 => 2,
                UintTy::U32 => 4,
                UintTy::U64 => 8,
                UintTy::U128 => 16,
                UintTy::Usize => 8,
            }),
            TyKind::Float(float_ty) => Some(match float_ty {
                FloatTy::F16 => 2,
                FloatTy::F32 => 4,
                FloatTy::F64 => 8,
                FloatTy::F128 => 16,
            }),
            TyKind::Tuple(elements) => {
                let mut total = 0u64;
                for elem in elements {
                    total = total.saturating_add(self.size_of_ty(elem, span)?);
                }
                Some(total)
            }
            TyKind::Array(elem_ty, len) => {
                let len = match len {
                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => int.data as u64,
                    _ => return None,
                };
                Some(self.size_of_ty(elem_ty, span)?.saturating_mul(len))
            }
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) | TyKind::FnPtr(_) | TyKind::FnDef(_, _) => {
                Some(8)
            }
            TyKind::Never => Some(0),
            TyKind::Slice(_) => Some(16),
            // Pointer-sized handle — see `TyKind::Type`'s own doc comment
            // and `lir_type_from_ty`'s matching `Ptr(Void)` lowering.
            TyKind::Type => Some(8),
            // Same storage strategy as `TyKind::Type` — see `TyKind::Any`'s
            // own doc comment.
            TyKind::Any => Some(8),
            TyKind::Adt(adt, substs) => {
                if let Some(size) = self.display_type_name(ty).and_then(|name| {
                    self.mir_package
                        .borrow()
                        .opaque_ty_sizes
                        .get(&name)
                        .copied()
                }) {
                    return Some(size);
                }
                let args: Vec<Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(inner) => Some(inner.clone()),
                        _ => None,
                    })
                    .collect();
                if self.mir_package.borrow().struct_defs.contains_key(&adt.did) {
                    let layout = self.struct_layout_for_ty(ty).or_else(|| {
                        self.struct_layout_for_instance(adt.did.clone(), &args, span)
                    })?;
                    let mut total = 0u64;
                    for field in &layout.field_tys {
                        total = total.saturating_add(self.size_of_ty(field, span)?);
                    }
                    return Some(total);
                }
                if self.mir_package.borrow().enum_defs.contains_key(&adt.did) {
                    let layout = self.enum_layout_for_instance(adt.did.clone(), &args, span)?;
                    let mut total = self.size_of_ty(&layout.tag_ty, span)?;
                    for payload in &layout.payload_tys {
                        total = total.saturating_add(self.size_of_ty(payload, span)?);
                    }
                    return Some(total);
                }
                None
            }
            _ => None,
        }
    }

    /// The nominal `Ty` a constructed/typed enum value should carry —
    /// `adt_shell_ty` for `layout`'s own `(def_id, args)`, falling back to
    /// the flattened `layout.enum_ty` only if the enum somehow isn't
    /// registered (shouldn't happen for any layout that was itself
    /// successfully computed, since that requires the same registration).
    pub(crate) fn nominal_enum_ty(&mut self, layout: &EnumLayout) -> Ty {
        self.adt_shell_ty(layout.def_id.clone(), &layout.args)
            .unwrap_or_else(|| layout.enum_ty.clone())
    }

    /// If `ty` is a registered struct's flattened `Tuple` representation
    /// (looked up via `struct_layouts_by_ty`), returns its nominal
    /// `TyKind::Adt` form instead — otherwise returns `ty` unchanged.
    /// Structs stay flattened everywhere by default in this refactor (only
    /// enums were made nominal), but a few call sites need to recognize a
    /// struct as indexable/method-dispatchable (`real_indexable_struct_def
    /// _id`, which only matches `TyKind::Adt`) from a `Ty` that only has
    /// the flattened shape available — e.g. a match-bound enum payload
    /// local (`bind_match_pattern`), whose declared type comes from
    /// `EnumLayout::variant_payloads` rather than a type annotation that
    /// would otherwise resolve nominally via `struct_def_from_ty`.
    pub(crate) fn nominalize_struct_ty(&mut self, ty: Ty) -> Ty {
        let Some(key) = self
            .mir_package
            .borrow()
            .struct_layouts_by_ty
            .get(&ty)
            .cloned()
        else {
            return ty;
        };
        self.adt_shell_ty(key.def_id, &key.args).unwrap_or(ty)
    }

    /// Resolves a type used as a *generic argument* (e.g. `Field` in
    /// `Vec<Field>`) lazily: if it names a registered struct/enum, this
    /// returns a cheap identity reference (`adt_shell_ty`) instead of
    /// eagerly computing that type's full layout via
    /// `lower_type_expr_with_substs`'s `Path` arm. A generic argument's own
    /// byte layout is irrelevant to building the outer type's substitution
    /// map, and forcing it here is what causes self-referential types (e.g.
    /// `std::json::Value`, which reaches itself through `Vec<Field>` where
    /// `Field` directly embeds `Value`) to re-enter their own in-progress
    /// layout computation.
    pub(super) fn lower_generic_type_arg(
        &mut self,
        ty_expr: &hir::TypeExpr,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
            if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                if self.mir_package.borrow().struct_defs.contains_key(def_id)
                    || self.mir_package.borrow().enum_defs.contains_key(def_id)
                {
                    let nested_args: Vec<Ty> = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| {
                            args.args
                                .iter()
                                .filter_map(|arg| match arg {
                                    hir::GenericArg::Type(ty) => {
                                        Some(self.lower_generic_type_arg(ty, substs))
                                    }
                                    hir::GenericArg::Const(_) => None,
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    if let Some(shell) = self.adt_shell_ty(def_id.clone(), &nested_args) {
                        return shell;
                    }
                }
            }
        }
        self.lower_type_expr_with_substs(ty_expr, substs)
    }

    pub(crate) fn display_type_name(&self, ty: &Ty) -> Option<String> {
        match &ty.kind {
            // `AdtDef`s built for a *struct* (e.g. `path_ty` in
            // `fp-typing/src/hir_typeck.rs`) always carry an empty
            // `variants` list — only enums populate it — so a struct name
            // (like "Vec"/"HashMap", needed by `is_list_container`/
            // `is_map_container`) has to come from `struct_defs` instead.
            TyKind::Adt(adt, _) => adt
                .variants
                .first()
                .map(|variant| variant.ident.as_str().to_string())
                .or_else(|| {
                    self.mir_package
                        .borrow()
                        .struct_defs
                        .get(&adt.did)
                        .cloned()
                        .map(|def| def.name.clone())
                }),
            TyKind::Ref(_, inner, _) => self.display_type_name(inner),
            TyKind::RawPtr(type_and_mut) => self.display_type_name(&type_and_mut.ty),
            _ => None,
        }
    }

    pub(crate) fn is_opaque_ty(&self, ty: &Ty) -> bool {
        self.display_type_name(ty)
            .map(|name| self.mir_package.borrow().opaque_types.contains_key(&name))
            .unwrap_or(false)
    }

    pub(crate) fn has_unresolved_ty(&self, ty: &Ty) -> bool {
        if self.is_opaque_ty(ty) {
            return true;
        }
        match &ty.kind {
            TyKind::Infer(_)
            | TyKind::Error(_)
            | TyKind::Param(_)
            | TyKind::Placeholder(_)
            | TyKind::Bound(_, _)
            | TyKind::Opaque(_, _)
            | TyKind::Projection(_)
            | TyKind::Dynamic(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Closure(_, _)
            | TyKind::Type => true,
            TyKind::Ref(_, inner, _) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.has_unresolved_ty(type_and_mut.ty.as_ref()),
            TyKind::Slice(inner) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::Array(inner, _) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::Tuple(elements) => elements
                .iter()
                .any(|elem| self.has_unresolved_ty(elem.as_ref())),
            TyKind::FnPtr(poly_sig) => {
                let sig = &poly_sig.binder.value;
                sig.inputs
                    .iter()
                    .any(|input| self.has_unresolved_ty(input.as_ref()))
                    || self.has_unresolved_ty(sig.output.as_ref())
            }
            TyKind::Adt(_, substs) => substs.iter().any(|arg| {
                matches!(arg, mir::ty::GenericArg::Type(inner) if self.has_unresolved_ty(inner))
            }),
            TyKind::Bool
            | TyKind::Char
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::FnDef(_, _)
            | TyKind::Any
            | TyKind::Never => false,
        }
    }

    /// Replaces bare generic-param references (`TyKind::Param`) inside an
    /// already-lowered `Ty` with their concrete substitution, recursing
    /// through the same structural positions `has_unresolved_ty` checks.
    /// Used to repair a typeck-cached type recovered from a generic body's
    /// *abstract* (unspecialized) type-check pass once a concrete
    /// `type_substs` mapping is available for this call's monomorphization.
    pub(crate) fn substitute_ty(&self, ty: &Ty, substs: &HashMap<String, Ty>) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => substs
                .get(param.name.as_str())
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            TyKind::Ref(region, inner, mutbl) => Ty {
                kind: TyKind::Ref(
                    region.clone(),
                    Box::new(self.substitute_ty(inner, substs)),
                    *mutbl,
                ),
            },
            TyKind::RawPtr(type_and_mut) => Ty {
                kind: TyKind::RawPtr(TypeAndMut {
                    ty: Box::new(self.substitute_ty(&type_and_mut.ty, substs)),
                    mutbl: type_and_mut.mutbl,
                }),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_ty(inner, substs))),
            },
            TyKind::Array(inner, len) => Ty {
                kind: TyKind::Array(Box::new(self.substitute_ty(inner, substs)), len.clone()),
            },
            TyKind::Tuple(elements) => Ty {
                kind: TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.substitute_ty(elem, substs)))
                        .collect(),
                ),
            },
            TyKind::Adt(adt, args) => Ty {
                kind: TyKind::Adt(
                    adt.clone(),
                    args.iter()
                        .map(|arg| match arg {
                            mir::ty::GenericArg::Type(inner) => {
                                mir::ty::GenericArg::Type(self.substitute_ty(inner, substs))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                ),
            },
            _ => ty.clone(),
        }
    }

    /// Reads this struct-literal expression's generic type args from
    /// `fp-typing`'s own already-resolved type-check result
    /// (`typeck_expr_type`) instead of re-deriving them independently in
    /// `fp-backend` — HIR→MIR
    /// lowering must only consume typeck's answers, never recompute generic
    /// substitutions itself (mirrors rustc's `rustc_mir_build`, which never
    /// re-infers what `rustc_hir_typeck` already resolved). When the cached
    /// type is itself expressed relative to an *enclosing* still-generic
    /// item's own params (legitimate, since generic items are type-checked
    /// once, generically, never per call-site), `type_substs` — the current
    /// specialization's own concrete substitution map — is composed in via
    /// `substitute_ty` before the result is trusted. Returns `None` (letting
    /// the caller hard-error the same way an explicit turbofish mismatch
    /// would) whenever there's no cached entry, the cached type isn't this
    /// same struct, or composing `type_substs` still leaves it unresolved.
    pub(crate) fn adt_ty_args_from_typeck_cache(
        &mut self,
        hir_id: hir::HirId,
        def_id: hir::DefId,
        type_substs: &HashMap<String, Ty>,
    ) -> Option<Vec<Ty>> {
        let cached = self.typeck_expr_type(hir_id)?;
        let TyKind::Adt(adt, args) = &cached.kind else {
            return None;
        };
        if adt.did != def_id {
            return None;
        }
        let mut resolved = Vec::with_capacity(args.len());
        for arg in args {
            let mir::ty::GenericArg::Type(ty) = arg else {
                return None;
            };
            let ty = if type_substs.is_empty() {
                ty.clone()
            } else {
                self.substitute_ty(ty, type_substs)
            };
            if self.has_unresolved_ty(&ty) {
                return None;
            }
            resolved.push(ty);
        }
        Some(resolved)
    }

    pub(crate) fn error_ty(&mut self) -> Ty {
        let error = ErrorGuaranteed {
            index: self.mir_package.borrow_mut().fresh_error_id(),
        };
        Ty {
            kind: TyKind::Error(error),
        }
    }

    pub(crate) fn error_constant(&mut self, span: Span) -> mir::Constant {
        self.emit_error(span, "unable to lower expression to a constant");
        mir::Constant {
            span,
            ty: self.error_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Bool(false),
        }
    }

    pub(crate) fn enum_layout_for_def(
        &mut self,
        def_id: hir::DefId,
        span: Span,
    ) -> Option<EnumLayout> {
        let Some(definition) = self.mir_package.borrow().enum_defs.get(&def_id).cloned() else {
            return None;
        };
        if !definition.generics.is_empty() {
            let inferred: Vec<Ty> = definition
                .generics
                .iter()
                .enumerate()
                .map(|(idx, _)| Ty {
                    kind: TyKind::Infer(mir::ty::InferTy::FreshTy(idx as u32)),
                })
                .collect();
            return self.enum_layout_for_instance(def_id, &inferred, span);
        }
        self.enum_layout_for_instance(def_id, &[], span)
    }

    pub(crate) fn enum_layout_for_ty(&self, ty: &Ty) -> Option<EnumLayout> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_ty(inner),
            TyKind::RawPtr(type_and_mut) => self.enum_layout_for_ty(&type_and_mut.ty),
            TyKind::Adt(adt, substs)
                if self.mir_package.borrow().enum_defs.contains_key(&adt.did) =>
            {
                let args: Vec<Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(t) => Some(t.clone()),
                        _ => None,
                    })
                    .collect();
                let key = EnumLayoutKey {
                    def_id: adt.did.clone(),
                    args,
                };
                self.mir_package.borrow().enum_layouts.get(&key).cloned()
            }
            // A tuple-shaped `ty` (a generic enum's already-flattened
            // `(discriminant, ...payload)` layout shape) can't be
            // destructured into an `EnumLayoutKey` directly the way the
            // `Adt` arm above does, but `enum_layouts_by_ty` already
            // indexes every layout by this exact flattened shape (see
            // `enum_layout_for_ty_exact`'s doc comment) — try that O(1)
            // lookup before falling back to the linear
            // `enum_layouts.values()` scan, which is only actually needed
            // for a `ty` containing `Infer` wildcard positions (the one
            // case `enum_layout_ty_matches` can match that an exact
            // `HashMap` key lookup can't).
            _ => self
                .enum_layout_for_ty_exact(ty)
                .or_else(|| self.enum_layout_for_ty_fuzzy(ty)),
        }
    }

    /// Fallback for a `ty` containing `Infer` wildcard positions, where
    /// more than one cached `EnumLayout` can structurally match the same
    /// flattened shape — e.g. `std::json::Value`'s own concrete layout and
    /// `std::option::Option<T>`'s generic *template* layout (built with
    /// `Infer` filler args for a context-free `Some(..)`/`None`
    /// construction) both flatten to the same `Tuple[I64, X]` shape.
    /// Picking arbitrarily (a bare `.find()` over `enum_layouts.values()`,
    /// whose iteration order is randomized per-process by `HashMap`) is a
    /// real non-determinism bug — there is no "guess which cached layout
    /// looks right" step anywhere in rustc's own layout-query model.
    /// Deterministically prefer a genuinely concrete instantiation (no
    /// unresolved arg) over a still-generic template, then break any
    /// remaining tie by `def_id` — never by hash order.
    pub(super) fn enum_layout_for_ty_fuzzy(&self, ty: &Ty) -> Option<EnumLayout> {
        self.mir_package
            .borrow()
            .enum_layouts
            .values()
            .filter(|layout| Self::enum_layout_ty_matches(&layout.enum_ty, ty))
            .min_by_key(|layout| {
                let is_template = layout.args.iter().any(|arg| self.has_unresolved_ty(arg));
                (is_template, layout.def_id.clone())
            })
            .cloned()
    }

    pub(super) fn enum_layout_ty_matches(layout_ty: &Ty, requested_ty: &Ty) -> bool {
        match (&layout_ty.kind, &requested_ty.kind) {
            (TyKind::Infer(_), _) | (_, TyKind::Infer(_)) => true,
            (TyKind::Tuple(layout), TyKind::Tuple(requested)) => {
                layout.len() == requested.len()
                    && layout
                        .iter()
                        .zip(requested)
                        .all(|(layout, requested)| Self::enum_layout_ty_matches(layout, requested))
            }
            _ => layout_ty == requested_ty,
        }
    }

    pub(super) fn enum_layout_for_concrete_ty(
        &mut self,
        ty: &Ty,
        span: Span,
    ) -> Option<EnumLayout> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_concrete_ty(inner, span),
            TyKind::RawPtr(type_and_mut) => {
                self.enum_layout_for_concrete_ty(&type_and_mut.ty, span)
            }
            TyKind::Adt(adt, substs) => {
                if !self.mir_package.borrow().enum_defs.contains_key(&adt.did) {
                    return None;
                }
                let mut args = Vec::new();
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        args.push(inner.clone());
                    }
                }
                self.enum_layout_for_instance(adt.did.clone(), &args, span)
            }
            _ => None,
        }
    }

    pub fn take_diagnostics(&mut self) -> DiagnosticManager {
        std::mem::replace(&mut self.diagnostics, DiagnosticManager::new())
    }
}
