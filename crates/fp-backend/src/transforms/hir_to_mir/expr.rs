// HIR→MIR lowering implementation (moved from mod.rs)
// This file currently contains the full original implementation and will be
// gradually split into stmt/control_flow/types/borrow submodules.

// BEGIN ORIGINAL CONTENT
use fp_core::ast::{
    DecimalType, TypeBinaryOpKind, TypeInt, TypePrimitive, Value, ValueList, ValueMap, ValueTuple,
};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::hir::place::{
    HirAssignTargetBase, HirAssignTargetProjection, project_hir_assign_target,
};

fn call_arg_values(args: &[hir::CallArg]) -> Vec<&hir::Expr> {
    args.iter().map(|arg| &arg.value).collect()
}
use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir::ty::{
    AdtDef, AdtFlags, ConstKind, ConstValue, CtorKind, ErrorGuaranteed, FloatTy, GenericArg, IntTy,
    Mutability, ReprFlags, ReprOptions, Scalar, ScalarInt, Ty, TyKind, TypeAndMut, UintTy,
    VariantDef, VariantDiscr,
};
use fp_core::mir::{self, Symbol};
use fp_core::ops::format_value_with_spec;
use fp_core::span::Span;
use fp_typing::TypeckResults;
use std::collections::{HashMap, HashSet, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};

const DIAGNOSTIC_CONTEXT: &str = "hir→mir";

fn lower_hir_ty(ty: &hir::ty::Ty) -> Result<Ty> {
    fn lower_const(value: &hir::ty::ConstKind) -> Result<mir::ty::ConstKind> {
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

    fn lower_arg(arg: &hir::ty::GenericArg) -> Result<mir::ty::GenericArg> {
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
                did: def.did,
                variants: def
                    .variants
                    .iter()
                    .map(|variant| VariantDef {
                        def_id: variant.def_id,
                        ctor_def_id: variant.ctor_def_id,
                        ident: variant.ident.clone().into(),
                        discr: match variant.discr {
                            hir::ty::VariantDiscr::Relative(value) => VariantDiscr::Relative(value),
                            hir::ty::VariantDiscr::Explicit(value) => VariantDiscr::Explicit(value),
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
            *def,
            args.iter().map(lower_arg).collect::<Result<Vec<_>>>()?,
        ),
        hir::ty::TyKind::Opaque(def, args) => TyKind::Opaque(
            *def,
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
#[derive(Clone, Debug)]
struct MethodLoweringInfo {
    def_id: Option<hir::DefId>,
    substs: mir::ty::SubstsRef,
    sig: mir::FunctionSig,
    fn_name: String,
    fn_ty: Ty,
    struct_def: Option<hir::DefId>,
}

/// Raw HIR for a generic method (`impl<T> Vec<T> { .. }`), cached here by
/// `register_generic_method_definition` and specialized on demand per call
/// site by `ensure_method_specialization*`, which build a concrete
/// `MethodLoweringInfo`/MIR body for the call's own substs and cache the
/// result in `method_specializations` keyed by `(DefId, SubstsRef)`.
#[derive(Clone)]
struct MethodDefinition {
    def_id: hir::DefId,
    function: hir::Function,
    impl_generics: hir::Generics,
    self_ty: hir::TypeExpr,
    self_def: Option<hir::DefId>,
    method_name: String,
    assoc_types: HashMap<String, hir::TypeExpr>,
}

/// Raw HIR needed to lower a *non-generic* method's body on demand
/// (`ensure_method_lowered`), keyed by the method's own `impl_item.def_id`.
/// Populated only by `register_impl_signatures`'s signature-only pre-pass —
/// the counterpart to `MethodDefinition`/`register_generic_method_definition`,
/// which does the same job for generic methods (those get lowered per-call
/// via specialization instead, keyed by substs, not by this map). The
/// pre-pass itself needs no typed bodies, so this is safe to populate
/// unconditionally for a whole, possibly mid-typecheck package.
#[derive(Clone)]
struct MethodHirRef {
    function: hir::Function,
    span: Span,
    method_context: Option<MethodContext>,
}

#[derive(Clone, Debug)]
struct FunctionSpecializationInfo {
    def_id: hir::DefId,
    substs: mir::ty::SubstsRef,
    name: String,
    sig: mir::FunctionSig,
    fn_ty: Ty,
}

#[derive(Clone, Debug)]
pub struct EnumLayout {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
    pub tag_ty: Ty,
    pub payload_tys: Vec<Ty>,
    pub enum_ty: Ty,
    pub variant_payloads: HashMap<hir::DefId, Vec<Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumLayoutKey {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
}

#[derive(Clone, Debug)]
struct EnumDefinition {
    def_id: hir::DefId,
    name: String,
    generics: Vec<String>,
    variants: Vec<EnumVariantDef>,
}

#[derive(Clone, Debug)]
struct EnumVariantDef {
    def_id: hir::DefId,
    name: String,
    discriminant: i64,
    payload: Option<hir::TypeExpr>,
}

#[derive(Clone, Debug)]
struct EnumVariantInfo {
    def_id: hir::DefId,
    enum_def: hir::DefId,
    discriminant: i64,
    payload_def: Option<hir::DefId>,
}

#[derive(Clone, Debug)]
struct MethodContext {
    def_id: Option<hir::DefId>,
    path: Vec<hir::PathSegment>,
    mir_self_ty: Ty,
    /// This impl's own `type Name = ...;` bindings (e.g. `impl<T> Index<usize>
    /// for Vec<T> { type Output = T; ... }` → `{"Output": T's TypeExpr}`),
    /// so a method signature's `Self::Output` resolves to the *bound*
    /// type (here, the impl's own `T`) rather than collapsing to `Self`
    /// itself — see `lower_type_expr_with_context`/`_and_substs`.
    assoc_types: HashMap<String, hir::TypeExpr>,
}

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

#[derive(Clone, Debug)]
struct StructDefinition {
    name: String,
    generics: Vec<String>,
    fields: Vec<StructFieldDef>,
    field_index: HashMap<String, usize>,
}

#[derive(Clone, Debug)]
struct StructFieldDef {
    name: String,
    ty: hir::TypeExpr,
}

#[derive(Clone, Debug)]
pub struct StructLayout {
    pub ty: Ty,
    pub field_tys: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StructuralLayoutKey {
    fields: Vec<(String, Ty)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructLayoutKey {
    pub def_id: hir::DefId,
    pub args: Vec<Ty>,
}

#[derive(Clone)]
struct StructFieldInfo {
    name: String,
    ty: Ty,
}

#[derive(Clone)]
struct ConstInfo {
    ty: Ty,
    value: mir::Constant,
}

impl ConstInfo {
    fn typed_value(&self) -> mir::Constant {
        let mut value = self.value.clone();
        value.ty = self.ty.clone();
        value
    }
}

#[derive(Clone)]
enum ConstContainerArgs {
    List { elem_ty: Ty },
    Map { key_ty: Ty, value_ty: Ty },
}

pub struct MirLowering {
    next_mir_id: mir::MirId,
    next_body_id: u32,
    next_error_id: u32,
    next_synthetic_def_id: mir::ty::DefId,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    struct_defs: HashMap<hir::DefId, StructDefinition>,
    /// Reverse index from a struct's name's tail segment (the part after
    /// its final `::`, or the whole name if unqualified) to every
    /// registered `DefId` sharing that tail — lets `struct_def_from_ty`'s
    /// name-based fallback narrow straight to the (usually one-element)
    /// candidate set sharing a receiver's short name, instead of scanning
    /// every struct definition in the program with a `format!` allocation
    /// per iteration. Provably safe as a pre-filter: `struct_def_from_ty`'s
    /// own match condition (`def.name == name || def.name.ends_with("::"
    /// + name)`) can only hold when `def.name`'s tail segment equals
    /// `name`'s tail segment, so this never excludes a true match.
    struct_defs_by_tail_name: HashMap<String, Vec<hir::DefId>>,
    struct_layouts: HashMap<StructLayoutKey, StructLayout>,
    struct_layouts_by_ty: HashMap<Ty, StructLayoutKey>,
    struct_layouts_in_progress: HashSet<StructLayoutKey>,
    structural_defs: HashMap<StructuralLayoutKey, hir::DefId>,
    enum_defs: HashMap<hir::DefId, EnumDefinition>,
    /// Reverse index from an enum's exact name to its `DefId` — lets
    /// `lower_let_expr`'s unresolved-type-annotation fallback (a bare
    /// enum name with no `Res::Def` yet, e.g. before full path resolution)
    /// look it up in O(1) instead of scanning every registered enum
    /// definition. Built alongside `enum_defs`'s two insertion sites.
    enum_defs_by_name: HashMap<String, hir::DefId>,
    enum_layouts: HashMap<EnumLayoutKey, EnumLayout>,
    /// Exact reverse index from a flattened-tuple enum representation back
    /// to the `EnumLayoutKey` that produced it — mirrors
    /// `struct_layouts_by_ty`, which structs already had. Without this,
    /// recovering an enum's concrete generic args from a flattened value
    /// (e.g. during generic specialization) fell back to a fuzzy,
    /// `Infer`-as-wildcard linear scan (`enum_layout_for_ty`) that could
    /// silently fail to bind a type parameter.
    enum_layouts_by_ty: HashMap<Ty, EnumLayoutKey>,
    enum_layouts_in_progress: HashSet<EnumLayoutKey>,
    enum_variants: HashMap<hir::DefId, EnumVariantInfo>,
    enum_variant_names: HashMap<String, hir::DefId>,
    const_values: HashMap<hir::DefId, ConstInfo>,
    executable_consts: HashMap<hir::DefId, (mir::Symbol, Ty)>,
    resolved_const_values: HashMap<String, mir::Constant>,
    function_sigs: HashMap<hir::DefId, mir::FunctionSig>,
    generic_function_defs: HashMap<hir::DefId, hir::Function>,
    runtime_functions: HashMap<String, mir::FunctionSig>,
    struct_methods: HashMap<String, HashMap<String, MethodLoweringInfo>>,
    /// For each bare method name, `Some(ty)` if every method registered
    /// under that name (across every struct) so far agrees on the same
    /// declared output type, or `None` once any two disagree — maintained
    /// incrementally as each method is registered
    /// (`register_method_lowering_info`, the single insertion point for
    /// `struct_methods`) instead of being recomputed by scanning every
    /// struct's whole method table on every `MethodCall` whose receiver
    /// type isn't otherwise known. Registration fully completes (via the
    /// signature-only pre-pass) before any body is lowered, so this is
    /// already in its final state by the time any `MethodCall` reads it —
    /// equivalent to a lazy per-call scan over the complete map, just
    /// computed once instead of on every such call.
    method_name_output_consensus: HashMap<String, Option<Ty>>,
    method_lookup_by_def: HashMap<hir::DefId, MethodLoweringInfo>,
    method_lookup: HashMap<String, MethodLoweringInfo>,
    method_defs: HashMap<String, MethodDefinition>,
    method_defs_by_def: HashMap<hir::DefId, MethodDefinition>,
    /// Raw HIR for every non-generic method, keyed by `impl_item.def_id` —
    /// see `MethodHirRef`'s own doc comment.
    method_hir_defs: HashMap<hir::DefId, MethodHirRef>,
    /// Reverse index from a method's own `impl_item.def_id` to its owning
    /// `Impl` item (and that item's index within it) — built once, lazily,
    /// on first need by `try_lazily_register_method`. Unlike structs,
    /// enums, consts, and top-level functions, an individual method's own
    /// `DefId` is never a key in `hir_def_map` (only the *owning* `Impl`
    /// item's own top-level `DefId` is), so resolving one requires this
    /// one-time scan first — mirrors `hir_def_map` itself being a
    /// one-time whole-workspace snapshot.
    method_owner_index: Option<HashMap<hir::DefId, (hir::Item, usize)>>,
    /// Reverse index from `(self_def, method_name)` to the method's key in
    /// `method_defs_by_def` — lets `real_indexable_struct_def_id`/
    /// `call_real_method_into_place` find a struct's `index`/`index_set`
    /// method in O(1) instead of each doing its own
    /// `method_defs_by_def.values().find(...)` linear scan over every
    /// method in the whole program (the two used to scan twice combined,
    /// once per `x[i]` in the program).
    method_defs_by_self_and_name: HashMap<(hir::DefId, String), hir::DefId>,
    method_specializations: HashMap<(hir::DefId, mir::ty::SubstsRef), MethodLoweringInfo>,
    function_specializations: HashMap<(hir::DefId, mir::ty::SubstsRef), FunctionSpecializationInfo>,
    /// Pre-substitution memo, keyed directly on a call site's own raw
    /// arguments rather than the `substs` derived from them — lets repeat
    /// calls to the same generic function/method with the same concrete
    /// argument types skip the whole substitution-inference walk
    /// (`build_substs_from_args` and its `infer_generic_from_type_expr`
    /// fallback chain) instead of only benefiting from the cache *after*
    /// paying for that walk. Safe because struct/enum registration
    /// finishes in a pre-pass (`finalize_adt_definitions`) before any
    /// function body is lowered, so the walk's inputs-to-output mapping is
    /// pure for the remainder of the lowering pass. `Span` is deliberately
    /// excluded — two distinct call sites with identical types must share
    /// a cache entry.
    function_specialization_call_cache:
        HashMap<(hir::DefId, Vec<Ty>, Vec<Ty>, Option<Ty>), FunctionSpecializationInfo>,
    method_specialization_call_cache:
        HashMap<(hir::DefId, Vec<Ty>, Vec<Ty>, Option<Ty>), MethodLoweringInfo>,
    extra_items: Vec<mir::Item>,
    extra_bodies: Vec<(mir::BodyId, mir::Body)>,
    /// Marks a function/method `DefId` whose body has already been lowered
    /// and pushed into `extra_items`/`extra_bodies` (or, for the normal
    /// whole-package pipeline, `mir_program.items`/`.bodies` directly) —
    /// see `ensure_function_lowered`/`ensure_method_lowered`. Neither
    /// `function_sigs` nor `method_lookup_by_def` can serve as this guard:
    /// both are also populated by signature-only registration (the
    /// call-site lazy fallback, the impl signature pre-pass) with no body
    /// ever lowered.
    lowered_items: HashSet<hir::DefId>,
    opaque_types: HashMap<String, Ty>,
    /// Byte size for an opaque type minted for a *mismatched enum payload
    /// slot* (`enum_layout_for_instance`'s per-slot merge loop) — the
    /// largest size among the concrete per-variant types that share that
    /// slot, since the slot's real runtime storage must fit whichever
    /// variant is actually active (a tagged union, not a merged/uniform
    /// field). Not populated for other opaque types (e.g. plain
    /// `impl Trait` existentials), which have no such "size" concept.
    opaque_ty_sizes: HashMap<String, u64>,
    synthetic_runtime_functions: HashSet<String>,
    next_synthetic_hir_def_id: hir::DefId,
    typeck_type_exprs: HashMap<hir::HirId, Ty>,
    typeck_exprs: HashMap<hir::HirId, Ty>,
    /// Comptime-evaluated `const { ... }` block values, keyed by the
    /// block expression's own `HirId` — populated from
    /// `TypeckResults::const_block_values`. Looked up directly when
    /// lowering `hir::ExprKind::ConstBlock`/`TypeExprKind::ConstBlock`;
    /// no synthetic item, no string key.
    typeck_const_block_values: HashMap<hir::HirId, Value>,
    typeck_method_resolutions: HashMap<hir::HirId, hir::DefId>,
    typeck_generic_call_args: HashMap<hir::HirId, Vec<Ty>>,
    typeck_generic_method_args: HashMap<hir::HirId, Vec<Ty>>,
    adt_defs: HashMap<hir::DefId, mir::ty::AdtDef>,
    /// Snapshot of the whole-workspace `hir::Package.def_map`/`def_paths`
    /// (local items + every dependency's, via `seed_workspace_definitions`),
    /// taken once at the top of `lower_program`/`transform`. Lets
    /// `compute_adt_layout` look up and lazily register a foreign
    /// struct/enum on demand (O(1) point lookup) instead of every
    /// dependency's ADTs being eagerly duplicated into `program.items`
    /// whether anything here references them or not.
    hir_def_map: HashMap<hir::DefId, hir::Item>,
    hir_def_paths: HashMap<hir::DefId, hir::DefPath>,
    /// Package this `MirLowering` instance is itself compiling — the
    /// package every id in `program.items` shares, captured once at the
    /// top of `lower_program`/`transform_comptime_request`. `hir_def_map`
    /// spans the whole workspace (this package's own items plus every
    /// dependency's, via `seed_workspace_definitions`), but only *this*
    /// package's own structs/enums/consts/impls get registered by the
    /// registration passes below — a dependency's own items are only ever
    /// safe to reference by signature (the dependency compiles its own
    /// body separately; see `ensure_function_lowered`/
    /// `ensure_method_lowered`), never to lower a body from here.
    current_package_id: Option<hir::PackageId>,
}

impl MirLowering {
    fn default_runtime_signatures() -> HashMap<String, mir::FunctionSig> {
        let mut map = HashMap::new();
        map.insert(
            "printf".to_string(),
            mir::FunctionSig {
                inputs: Vec::new(),
                output: Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
            },
        );
        map.insert(
            "fp_panic".to_string(),
            mir::FunctionSig {
                inputs: vec![Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                }],
                output: Ty {
                    kind: TyKind::Tuple(Vec::new()),
                },
            },
        );
        map
    }

    pub fn new() -> Self {
        Self {
            next_mir_id: 0,
            next_body_id: 0,
            next_error_id: 0,
            next_synthetic_def_id: mir::ty::DefId::local(1),
            diagnostics: Vec::new(),
            has_errors: false,
            struct_defs: HashMap::new(),
            struct_defs_by_tail_name: HashMap::new(),
            struct_layouts: HashMap::new(),
            struct_layouts_by_ty: HashMap::new(),
            struct_layouts_in_progress: HashSet::new(),
            structural_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_defs_by_name: HashMap::new(),
            enum_layouts: HashMap::new(),
            enum_layouts_by_ty: HashMap::new(),
            enum_layouts_in_progress: HashSet::new(),
            enum_variants: HashMap::new(),
            enum_variant_names: HashMap::new(),
            const_values: HashMap::new(),
            executable_consts: HashMap::new(),
            resolved_const_values: HashMap::new(),
            function_sigs: HashMap::new(),
            generic_function_defs: HashMap::new(),
            runtime_functions: Self::default_runtime_signatures(),
            struct_methods: HashMap::new(),
            method_name_output_consensus: HashMap::new(),
            method_lookup_by_def: HashMap::new(),
            method_lookup: HashMap::new(),
            method_defs: HashMap::new(),
            method_defs_by_def: HashMap::new(),
            method_hir_defs: HashMap::new(),
            method_owner_index: None,
            method_defs_by_self_and_name: HashMap::new(),
            method_specializations: HashMap::new(),
            function_specializations: HashMap::new(),
            function_specialization_call_cache: HashMap::new(),
            method_specialization_call_cache: HashMap::new(),
            extra_items: Vec::new(),
            extra_bodies: Vec::new(),
            lowered_items: HashSet::new(),
            opaque_types: HashMap::new(),
            opaque_ty_sizes: HashMap::new(),
            synthetic_runtime_functions: HashSet::new(),
            next_synthetic_hir_def_id: hir::DefId::local(1),
            typeck_type_exprs: HashMap::new(),
            typeck_exprs: HashMap::new(),
            typeck_const_block_values: HashMap::new(),
            typeck_method_resolutions: HashMap::new(),
            typeck_generic_call_args: HashMap::new(),
            typeck_generic_method_args: HashMap::new(),
            adt_defs: HashMap::new(),
            hir_def_map: HashMap::new(),
            hir_def_paths: HashMap::new(),
            current_package_id: None,
        }
    }

    pub fn transform(&mut self, hir_program: hir::Package) -> Result<mir::Program> {
        let program = self.lower_program(&hir_program)?;
        if self.has_errors {
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
    pub async fn transform_async(&mut self, hir_program: hir::Package) -> Result<mir::Program> {
        self.transform(hir_program)
    }

    /// Item-scoped lowering for exactly one pending `const { .. }` block,
    /// answering a `fp_typing::ComptimeRequest` directly from its own
    /// self-sufficient snapshot (see `ComptimeRequest`'s doc comment and
    /// `CompilerDriver::resolve_one_comptime_request`). Unlike `transform`
    /// (used for the real, whole-package compile, where every item's body
    /// genuinely needs lowering), this never lowers any function, method,
    /// or const body other than the one synthetic function built directly
    /// from `request.block`/`request.expected_ty` — no walk of `main`'s or
    /// any other item's own body at all. Whatever that one body actually
    /// references (other consts, plain functions, generic methods)
    /// resolves correctly and on demand via `register_const_value`/
    /// `ensure_function_lowered`/`ensure_method_lowered`/
    /// `ensure_function_specialization`'s existing lazy mechanisms.
    pub fn transform_comptime_request(
        &mut self,
        hir_program: &hir::Package,
        request: &fp_typing::ComptimeRequest,
    ) -> Result<mir::Program> {
        self.hir_def_map = hir_program.def_map.clone();
        self.hir_def_paths = hir_program.def_paths.clone();
        self.current_package_id = hir_program.items.first().map(|item| item.def_id.package_id);
        // `hir_program.items` only lists *top-level* items — a local
        // `const` binding inside a function body (or any other nested
        // item) gets a real `DefId` from the same monotonic counter but is
        // only recorded in `def_map`, never in `.items`. Seeding from
        // `.items` alone can collide with such a DefId (confirmed via
        // lldb: for a single-function file, `.items.max()` lands exactly
        // on the enclosing function's own id, one below a local const's).
        // `def_map` also carries every dependency package's own merged
        // definitions (`seed_workspace_definitions`), so restrict the max
        // to this request's own package — a synthetic id must stay within
        // the current package's id space, the same invariant
        // `saturating_add` below (which only bumps `index`, keeping
        // `package_id`) already assumes.
        self.next_synthetic_hir_def_id = hir_program
            .def_map
            .keys()
            .filter(|def_id| Some(def_id.package_id) == self.current_package_id)
            .copied()
            .max()
            .unwrap_or(hir::DefId::local(0))
            .saturating_add(1);

        // Whole-program-safe, order-independent registration only — none
        // of this requires any item's body to be typed (mirrors
        // `lower_program`'s matching passes verbatim; deliberately stops
        // short of that function's const-registration and per-item body
        // loop, which is exactly the whole-package dependency this entry
        // point exists to avoid).
        for item in &hir_program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(&hir_program.def_paths, item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(&hir_program.def_paths, item.def_id, def, item.span);
                }
                _ => {}
            }
        }
        self.register_all_dependency_adts();
        self.finalize_adt_definitions(hir_program);
        // Deliberately *not* an eager `register_impl_signatures` sweep
        // over every impl in the package (as this used to be) — that
        // cloned every method's `hir::Function` body, for every impl,
        // regardless of whether this one synthetic comptime body actually
        // calls it, and repeated that on every single comptime request.
        // `ensure_method_info`/`ensure_generic_method_def` already lazily
        // call this same per-item registration (`try_lazily_register_method`
        // -> `register_impl_signature_for_item`, the identical function
        // the eager sweep called) on demand, off the same `hir_def_map` —
        // exactly the "whole-program-safe... none of this requires any
        // item's body to be typed" registration this function's own doc
        // comment already scopes itself to, just reached lazily instead
        // of unconditionally for methods this request never references.

        let body = request.block.expr.as_ref().ok_or_else(|| {
            fp_core::error::Error::from(
                "internal compiler error: comptime request block has no body expression",
            )
        })?;
        self.register_const_block_comptime_entry_direct(
            request.expression_id,
            &request.expected_ty,
            body,
            body.span,
        );

        let mut mir_program = mir::Program::new();
        self.flush_extra_items(&mut mir_program);
        self.append_runtime_stubs(&mut mir_program);

        if self.has_errors {
            return Err(fp_core::error::Error::from(
                "internal compiler error: HIR-to-MIR lowering reported an error",
            ));
        }
        Ok(mir_program)
    }

    pub fn compute_adt_layout(&mut self, def_id: hir::DefId, substs: &[Ty], span: Span) {
        if !self.struct_defs.contains_key(&def_id) && !self.enum_defs.contains_key(&def_id) {
            self.try_lazily_register_adt(def_id, span);
        }
        // `def_id` is either a struct or an enum, never both — calling both
        // layout functions regardless of which one it actually is makes the
        // non-matching call spuriously report "definition not registered"
        // for a perfectly valid, correctly-registered type.
        if self.struct_defs.contains_key(&def_id) {
            let _ = self.struct_layout_for_instance(def_id, substs, span);
        } else if self.enum_defs.contains_key(&def_id) {
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
    fn register_all_dependency_adts(&mut self) {
        let diagnostics_before = self.diagnostics.len();
        let had_errors_before = self.has_errors;
        // `mem::take` instead of `.clone()` — `register_struct`/
        // `register_enum` only ever read `def_paths` (never touch
        // `self.hir_def_paths`), so there's no need to pay for cloning
        // the whole workspace-wide def-path table just to satisfy the
        // borrow checker; put it back once done.
        let def_paths = std::mem::take(&mut self.hir_def_paths);
        // Only `Struct`/`Enum` items are ever registered below — cloning
        // every item regardless of kind (as this used to) paid for a
        // full deep-clone of every dependency function/impl body in the
        // workspace (by far the largest items) merely to inspect its
        // `ItemKind` tag and then discard it. `register_struct`/
        // `register_enum` themselves already early-return once a
        // `def_id` is registered, so on a `MirLowering` instance that
        // does span multiple registration passes, repeating this scan is
        // cheap; on a *fresh* instance (as `transform_comptime_request`
        // creates once per comptime request — see `MirLowering::new()`'s
        // callers) every dependency struct/enum is still cloned once per
        // request, since there is no cross-request cache today. Fully
        // eliminating that repetition needs a cache that outlives a
        // single `MirLowering` instance (e.g. on `CompilerState`), which
        // is out of scope for this pass.
        let items: Vec<hir::Item> = self
            .hir_def_map
            .values()
            .filter(|item| matches!(&item.kind, hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_)))
            .cloned()
            .collect();
        for item in &items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(&def_paths, item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(&def_paths, item.def_id, def, item.span);
                }
                _ => {}
            }
        }
        self.hir_def_paths = def_paths;
        self.diagnostics.truncate(diagnostics_before);
        self.has_errors = had_errors_before;
    }

    /// Defensive fallback for a struct/enum somehow missed by
    /// `register_all_dependency_adts`'s eager sweep of `hir_def_map` —
    /// reached only when a `def_id` isn't already registered locally.
    /// `mem::take`s `hir_def_paths` for the duration of the call to hand
    /// `register_struct`/`register_enum` an owned reference without a
    /// `&self`/`&mut self` borrow conflict.
    fn try_lazily_register_adt(&mut self, def_id: hir::DefId, span: Span) {
        let Some(item) = self.hir_def_map.get(&def_id).cloned() else {
            return;
        };
        let def_paths = std::mem::take(&mut self.hir_def_paths);
        match &item.kind {
            hir::ItemKind::Struct(strukt) => {
                self.register_struct(&def_paths, def_id, strukt, span);
            }
            hir::ItemKind::Enum(enm) => {
                self.register_enum(&def_paths, def_id, enm, span);
            }
            _ => {}
        }
        self.hir_def_paths = def_paths;
    }

    fn compute_ty_layout(&mut self, ty: &Ty, span: Span) {
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
                self.compute_adt_layout(adt.did, &types, span);
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

    fn compute_body_locals(&mut self, program: &mir::Program, body_id: mir::BodyId) {
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

    fn compute_stmt_layouts(&mut self, body: &mir::Body, stmt: &mir::Statement) {
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

    fn compute_terminator_layouts(&mut self, body: &mir::Body, term: &mir::Terminator) {
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

    fn compute_place_layouts(&mut self, body: &mir::Body, place: &mir::Place) {
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

    fn compute_operand_layouts(&mut self, body: &mir::Body, op: &mir::Operand) {
        match op {
            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                self.compute_place_layouts(body, place);
            }
            mir::Operand::Constant(c) => {
                self.compute_ty_layout(&c.ty, Span::null());
            }
        }
    }

    fn compute_rvalue_layouts(&mut self, rv: &mir::Rvalue) {
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
                    self.compute_adt_layout(adt.did, &substs_types, Span::null());
                }
            }
            _ => {}
        }
    }

    pub fn walk_program_types_for_layouts(&mut self, program: &mir::Program) {
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

    pub fn struct_layout_map(&self) -> &HashMap<StructLayoutKey, StructLayout> {
        &self.struct_layouts
    }

    pub fn enum_layout_map(&self) -> &HashMap<EnumLayoutKey, EnumLayout> {
        &self.enum_layouts
    }

    /// Byte size for every opaque type minted for a mismatched/union enum
    /// payload slot (see `opaque_ty_sizes`'s own doc comment) — exported so
    /// `mir_to_lir` can size that slot's runtime storage (a raw byte
    /// buffer big enough for whichever variant is actually active)
    /// without needing to resolve the opaque placeholder's (nonexistent)
    /// field structure.
    pub fn opaque_payload_sizes(&self) -> &HashMap<String, u64> {
        &self.opaque_ty_sizes
    }

    pub fn take_adt_defs(&mut self) -> HashMap<hir::DefId, mir::ty::AdtDef> {
        std::mem::take(&mut self.adt_defs)
    }

    /// Every top-level const resolved by direct folding this pass (see
    /// `lower_const`'s fast path) — a directly-foldable const never
    /// becomes a comptime entry requiring the real interpreter, so its
    /// value would otherwise never reach `driver.rs`'s `resolved_consts`
    /// the way an interpreted const's value already does.
    pub fn take_resolved_const_values(&mut self) -> HashMap<String, mir::Constant> {
        std::mem::take(&mut self.resolved_const_values)
    }

    /// Struct field types only — enums are exported separately via
    /// `enum_layout_map` (keyed by `(DefId, args)`, since two different
    /// instantiations of a generic enum need different field lists, unlike
    /// this bare-`DefId`-keyed map).
    pub fn all_adt_field_tys(&self) -> HashMap<hir::DefId, Vec<Ty>> {
        let mut map = HashMap::new();
        for (key, layout) in &self.struct_layouts {
            map.insert(key.def_id, layout.field_tys.clone());
        }
        map
    }

    pub fn seed_resolved_const(&mut self, key: impl Into<String>, value: mir::Constant) {
        self.resolved_const_values.insert(key.into(), value);
    }

    pub fn with_typeck_results(mut self, results: &TypeckResults) -> Result<Self> {
        self.typeck_type_exprs = self.lower_ty_map(results.type_expr_types.iter());
        self.typeck_exprs = self.lower_ty_map(results.expr_types.iter());
        self.typeck_const_block_values = results.const_block_values.clone();
        self.typeck_method_resolutions = results.method_resolutions.clone();
        self.typeck_generic_call_args = self.lower_generic_args_map(results.generic_call_args.iter());
        self.typeck_generic_method_args =
            self.lower_generic_args_map(results.generic_method_args.iter());
        Ok(self)
    }

    /// A single unresolvable type recorded elsewhere in this package's
    /// typeck results (e.g. an unrelated item that failed to typecheck)
    /// must not block MIR lowering for every *other*, independently-correct
    /// item — skip just that one hir_id's entry (recording a diagnostic)
    /// instead of aborting the whole map. Every real consumer of these maps
    /// already looks entries up via `.get(&hir_id) -> Option<_>`, so a
    /// missing entry here is the same sparse-map shape they already
    /// tolerate — not a new kind of gap.
    fn lower_ty_map<'a>(
        &mut self,
        entries: impl Iterator<Item = (&'a hir::HirId, &'a hir::ty::Ty)>,
    ) -> HashMap<hir::HirId, Ty> {
        entries
            .filter_map(|(id, ty)| match lower_hir_ty(ty) {
                Ok(lowered) => Some((*id, lowered)),
                Err(error) => {
                    self.emit_warning(
                        Span::default(),
                        format!("skipping unresolvable type for HIR node {id:?}: {error}"),
                    );
                    None
                }
            })
            .collect()
    }

    /// Same reasoning as `lower_ty_map`, for the generic-args resolution
    /// maps: if any one argument in a resolution fails to lower, that
    /// resolution as a whole is skipped (a partial arg list would be
    /// nonsensical), but other hir_ids' resolutions are unaffected.
    fn lower_generic_args_map<'a>(
        &mut self,
        entries: impl Iterator<Item = (&'a hir::HirId, &'a fp_typing::types::GenericCallResolution)>,
    ) -> HashMap<hir::HirId, Vec<Ty>> {
        entries
            .filter_map(|(id, resolution)| {
                match resolution
                    .args
                    .iter()
                    .map(lower_hir_ty)
                    .collect::<Result<Vec<_>>>()
                {
                    Ok(args) => Some((*id, args)),
                    Err(error) => {
                        self.emit_warning(
                            Span::default(),
                            format!("skipping unresolvable generic args for HIR node {id:?}: {error}"),
                        );
                        None
                    }
                }
            })
            .collect()
    }

    /// Convert a comptime-evaluated `Value` (from `const { ... }` block
    /// resolution) into an MIR constant. Mirrors the scalar cases the
    /// driver's own `simple_value_to_mir_constant` handles for named
    /// consts; kept as a separate, small copy here since `fp-backend`
    /// cannot depend on `fp-compiler`.
    fn const_block_value_to_mir_constant(&self, value: &Value, span: Span) -> Option<mir::Constant> {
        let (ty, literal) = match value {
            Value::Int(value) => (
                Ty {
                    kind: TyKind::Int(IntTy::I64),
                },
                mir::ConstantKind::Int(value.value),
            ),
            Value::UInt(value) => (
                Ty {
                    kind: TyKind::Uint(UintTy::U64),
                },
                mir::ConstantKind::UInt(value.value),
            ),
            Value::Bool(value) => (Ty { kind: TyKind::Bool }, mir::ConstantKind::Bool(value.value)),
            Value::Decimal(value) => (
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
                mir::ConstantKind::Float(value.value),
            ),
            Value::String(value) => (
                Ty {
                    kind: TyKind::Slice(Box::new(Ty {
                        kind: TyKind::Int(IntTy::I8),
                    })),
                },
                mir::ConstantKind::Str(value.value.clone()),
            ),
            Value::Null(_) => (
                Ty {
                    kind: TyKind::Tuple(Vec::new()),
                },
                mir::ConstantKind::Null,
            ),
            _ => return None,
        };
        Some(mir::Constant {
            span,
            ty,
            user_ty: None,
            literal,
        })
    }

    fn const_key(&self, name: &str, span: Span) -> String {
        let file = fp_core::source_map::source_map()
            .file(span.file)
            .map(|file| file.path.display().to_string())
            .unwrap_or_else(|| format!("file#{}", span.file));
        format!("{file}:{}:{}:{name}", span.lo, span.hi)
    }

    fn synthetic_const_function_name(&self, name: &hir::Symbol, key: &str) -> String {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let hash = hasher.finish();
        format!("__fp_comptime_const_{}_{}", name.as_str(), hash)
    }

    fn lower_program(&mut self, program: &hir::Package) -> Result<mir::Program> {
        // Snapshot for `compute_adt_layout`'s lazy foreign-struct/enum
        // lookup — see the fields' doc comment. One clone per package
        // compile, not per lookup.
        self.hir_def_map = program.def_map.clone();
        self.hir_def_paths = program.def_paths.clone();
        self.current_package_id = program.items.first().map(|item| item.def_id.package_id);
        let mut mir_program = mir::Program::new();
        // Same "seed from `.items` alone can collide with a local const's
        // own real DefId" fix as `transform_comptime_request` — see that
        // function's own comment for the full rationale.
        self.next_synthetic_hir_def_id = program
            .def_map
            .keys()
            .filter(|def_id| Some(def_id.package_id) == self.current_package_id)
            .copied()
            .max()
            .unwrap_or(hir::DefId::local(0))
            .saturating_add(1);

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(&program.def_paths, item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(&program.def_paths, item.def_id, def, item.span);
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
            if let hir::ItemKind::Const(const_item) = &item.kind {
                self.register_const_value(item.def_id, const_item);
            }
        }

        for item in &items {
            match &item.kind {
                hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => {}
                hir::ItemKind::Const(const_item) => {
                    let ty = self.lower_type_expr(&const_item.ty);
                    if Self::is_unit_ty(&ty) {
                        // Unit consts don't need a static allocation; keep them as inline constants.
                        self.register_const_value(item.def_id, const_item);
                        continue;
                    }
                    let mir_item = self.lower_const(item.def_id, const_item)?;
                    mir_program.items.push(mir_item);
                }
                hir::ItemKind::Function(function) => {
                    if !function.sig.generics.params.is_empty() {
                        self.register_generic_function(item.def_id, function);
                    } else {
                        // Idempotent — a prior call site may have already
                        // lowered this on demand (`ensure_function_lowered`'s
                        // `resolve_callee_path` fallback); this proactive
                        // call is what guarantees full-package coverage for
                        // items with no local caller (see comment above).
                        self.ensure_function_lowered(item.def_id)?;
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
                            self.ensure_method_lowered(impl_item.def_id)?;
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
        self.append_runtime_stubs(&mut mir_program);

        Ok(mir_program)
    }

    fn lower_query(&mut self, item: &hir::Item, query: &hir::Query) -> mir::Item {
        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Query(mir::Query {
                origin: query.origin.clone(),
                ir: query.ir.clone(),
                span: item.span,
            }),
        };
        self.next_mir_id += 1;
        mir_item
    }

    fn append_runtime_stubs(&mut self, program: &mut mir::Program) {
        let span = Span::new(0, 0, 0);
        for name in self.synthetic_runtime_functions.clone() {
            // `printf` is only ever called through the dedicated
            // `IntrinsicCall{Print}` MIR path, never as a normal call
            // operand — it never reaches `function_value`'s lookup, so it
            // genuinely needs no MIR-level representation at all.
            if self.is_extern_runtime_function(&name) {
                continue;
            }
            let exists = program.items.iter().any(|item| match &item.kind {
                mir::ItemKind::Function(func) => func.name.as_str() == name,
                _ => false,
            });
            if exists {
                continue;
            }

            let Some(sig) = self.runtime_functions.get(&name).cloned() else {
                continue;
            };

            let body = self.stub_body(&sig, span);
            let body_id = mir::BodyId::new(self.next_body_id);
            self.next_body_id += 1;
            program.bodies.insert(body_id, body);

            // `fp_panic` *is* called as a normal `ConstantKind::Fn` operand
            // (from `assert!`/`unwrap`/etc. lowering), so MIR→LIR's
            // `function_value` needs a registered signature for it — but
            // its actual implementation is hand-synthesized directly in
            // each native backend (see `program_uses_fp_panic`/
            // `needs_panic_stub`), not lowered from a MIR body. Declare it
            // `extern` (a signature-only, body-less LIR declaration) rather
            // than giving it the generic `Unreachable`-terminated stub body
            // every other synthetic runtime function gets — that stub body
            // would make every panic/assert trap instead of actually
            // reporting a message, and would fight the backend's own
            // definition for the same symbol.
            let is_fp_panic = name == "fp_panic";
            let mir_function = mir::Function {
                name: mir::Symbol::new(name.clone()),
                def_id: None,
                substs: Vec::new(),
                sig: sig.clone(),
                body_id,
                abi: if is_fp_panic {
                    mir::ty::Abi::C { unwind: false }
                } else {
                    mir::ty::Abi::Rust
                },
                is_extern: is_fp_panic,
                attrs: Vec::new(),
            };

            program.items.push(mir::Item {
                mir_id: self.next_mir_id,
                kind: mir::ItemKind::Function(mir_function),
            });
            self.next_mir_id += 1;
        }
    }

    fn is_extern_runtime_function(&self, name: &str) -> bool {
        matches!(name, "printf")
    }

    fn flush_extra_items(&mut self, program: &mut mir::Program) {
        for item in self.extra_items.drain(..) {
            program.items.push(item);
        }
        for (body_id, body) in self.extra_bodies.drain(..) {
            program.bodies.insert(body_id, body);
        }
    }

    /// On-demand counterpart to `lower_function`, for a non-generic free
    /// function: lowers `def_id`'s body at most once (guarded by
    /// `lowered_items`), pushing the result into `extra_items`/
    /// `extra_bodies`. Callable both proactively (`lower_program`'s main
    /// loop, ensuring full package coverage) and reactively (a call site
    /// whose callee hasn't been lowered yet — `resolve_callee_path`'s
    /// `hir_def_map` fallback), mirroring the same lazy pattern
    /// `register_const_value`/`try_lazily_register_adt`/
    /// `ensure_function_specialization` already use for consts/ADTs/
    /// generics. A miss (unknown `def_id`, or a non-`Function` item) is not
    /// an error here — the caller's own resolution path already reports a
    /// real diagnostic when nothing usable comes back.
    fn ensure_function_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
        if self.lowered_items.contains(&def_id) {
            return Ok(());
        }
        // `def_id` isn't necessarily a *function* — `resolve_callee_path`
        // calls this unconditionally for any `Res::Def`, including a
        // method's `impl_item.def_id` (never present in `hir_def_map`;
        // `ensure_method_lowered` owns that case instead). Only claim
        // `lowered_items` once we've confirmed this really is a function —
        // marking it here on a miss would permanently block
        // `ensure_method_lowered` from ever getting a real chance at the
        // same `def_id` afterwards.
        let Some(item) = self.hir_def_map.get(&def_id).cloned() else {
            return Ok(());
        };
        let hir::ItemKind::Function(function) = &item.kind else {
            return Ok(());
        };
        self.lowered_items.insert(def_id);
        if let Some(current) = self.current_package_id {
            if def_id.package_id != current {
                // A dependency package's own function — that package
                // compiles its own body separately, in its own
                // `MirLowering` instance (own struct/enum/const
                // registrations); lowering it here would build it against
                // *this* package's registrations instead, silently
                // producing a wrong/incomplete body the moment it
                // references anything this package never registered. Only
                // this call site's own operand needs a signature — the
                // real body is supplied later, correctly, by
                // `predeclare_dependency_function_signatures` reading that
                // package's own compiled MIR.
                let sig = self.lower_function_sig(&function.sig, None);
                self.function_sigs.insert(def_id, sig);
                return Ok(());
            }
        }
        if !function.sig.generics.params.is_empty() {
            // Generic: raw HIR registration only, lowered per call site via
            // `ensure_function_specialization` — this function doesn't own
            // that path.
            self.register_generic_function(def_id, function);
            return Ok(());
        }
        let (mir_item, body_id, body) = self.lower_function(&item, function)?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));
        Ok(())
    }

    /// On-demand counterpart to `lower_impl`'s per-method loop, for a single
    /// non-generic method: lowers `def_id`'s body at most once (guarded by
    /// `lowered_items`) from the raw HIR `register_impl_signatures`'s
    /// signature-only pre-pass already stashed in `method_hir_defs`,
    /// pushing the result into `extra_items`/`extra_bodies`. See
    /// `ensure_function_lowered`'s doc comment — same pattern, same two
    /// caller shapes. A miss (unknown `def_id`, e.g. a generic method —
    /// those are lowered per call site via `ensure_method_specialization`
    /// instead) is not an error here, for the same reason.
    fn ensure_method_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
        if self.lowered_items.contains(&def_id) {
            return Ok(());
        }
        // Same reasoning as `ensure_function_lowered`: only claim
        // `lowered_items` once we've confirmed `def_id` really is a
        // non-generic method — `resolve_callee_path`'s `Res::Def` branch
        // tries `ensure_function_lowered` on every def_id first (a plain
        // function, by far the common case), which correctly leaves
        // `lowered_items` untouched on its own miss so this function still
        // gets a real chance afterwards.
        let Some(method_ref) = self.method_hir_defs.get(&def_id).cloned() else {
            return Ok(());
        };
        self.lowered_items.insert(def_id);
        if let Some(current) = self.current_package_id {
            if def_id.package_id != current {
                // Same reasoning as `ensure_function_lowered`'s
                // cross-package guard — this method's own package compiles
                // its body separately.
                let sig = self.lower_function_sig(
                    &method_ref.function.sig,
                    method_ref.method_context.as_ref(),
                );
                self.function_sigs.insert(def_id, sig);
                return Ok(());
            }
        }
        let (mir_item, body_id, body, _sig) = self.lower_method(
            def_id,
            &method_ref.function,
            method_ref.span,
            method_ref.method_context.as_ref(),
        )?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));
        Ok(())
    }

    fn lower_function(
        &mut self,
        item: &hir::Item,
        function: &hir::Function,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let sig = self.lower_function_sig(&function.sig, None);
        self.function_sigs.insert(item.def_id, sig.clone());
        let span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(item.span);
        let mir_body = if function.body.is_none() {
            self.stub_body(&sig, span)
        } else {
            self.lower_body(item, function, &sig, None)?
        };

        let mir_function = mir::Function {
            name: mir::Symbol::new(Self::def_path_str(
                &self.hir_def_paths,
                item.def_id,
                function.sig.name.as_str(),
            )),
            def_id: Some(item.def_id),
            substs: Vec::new(),
            sig,
            body_id,
            abi: self.map_abi(&function.sig.abi),
            is_extern: function.is_extern,
            attrs: function.attrs.clone(),
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body))
    }

    fn stub_body(&mut self, sig: &mir::FunctionSig, span: Span) -> mir::Body {
        let mut locals = Vec::new();
        locals.push(self.make_local_decl(&sig.output, span));
        for input in &sig.inputs {
            locals.push(self.make_local_decl(input, span));
        }

        let block = mir::BasicBlockData::new(Some(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        }));

        mir::Body::new(vec![block], locals, sig.inputs.len(), span)
    }

    fn catch_unwind_default_constant_for_ty(&self, ty: &Ty) -> Result<mir::ConstantKind> {
        match &ty.kind {
            TyKind::Bool => Ok(mir::ConstantKind::Bool(false)),
            TyKind::Int(_) => Ok(mir::ConstantKind::Int(0)),
            TyKind::Uint(_) => Ok(mir::ConstantKind::UInt(0)),
            TyKind::Float(_) => Ok(mir::ConstantKind::Float(0.0)),
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => Ok(mir::ConstantKind::UInt(0)),
            _ => Err(fp_core::error::Error::from(format!(
                "catch_unwind_result cannot synthesize unwind value for type `{ty}`"
            ))),
        }
    }

    fn register_generic_function(&mut self, def_id: hir::DefId, function: &hir::Function) {
        if self.generic_function_defs.contains_key(&def_id) {
            return;
        }
        let sig = self.lower_function_sig(&function.sig, None);
        self.function_sigs.insert(def_id, sig);
        self.generic_function_defs.insert(def_id, function.clone());
    }

    fn lower_function_with_substs(
        &mut self,
        item_def_id: hir::DefId,
        item_span: Span,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        substs: HashMap<String, Ty>,
        name_override: &str,
        function_substs: mir::ty::SubstsRef,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(item_span);

        let mir_body = BodyBuilder::new(self, function, sig, span, None, substs).lower()?;

        let mir_function = mir::Function {
            name: mir::Symbol::new(name_override),
            def_id: Some(item_def_id),
            substs: function_substs,
            sig: sig.clone(),
            body_id,
            abi: self.map_abi(&function.sig.abi),
            is_extern: false,
            attrs: Vec::new(),
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body))
    }

    fn ensure_function_specialization(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<FunctionSpecializationInfo> {
        let pre_key = (
            def_id,
            explicit_args.to_vec(),
            arg_types.to_vec(),
            expected_return.cloned(),
        );
        if let Some(info) = self.function_specialization_call_cache.get(&pre_key) {
            return Ok(info.clone());
        }
        let info = self.ensure_function_specialization_uncached(
            def_id,
            function,
            explicit_args,
            arg_types,
            expected_return,
            span,
        )?;
        self.function_specialization_call_cache
            .insert(pre_key, info.clone());
        Ok(info)
    }

    fn ensure_function_specialization_uncached(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<FunctionSpecializationInfo> {
        let generics = function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();
        let is_result_ctor = function.sig.name.as_str() == "Ok"
            || function.sig.name.as_str() == "Err"
            || function.sig.name.as_str().ends_with("::Ok")
            || function.sig.name.as_str().ends_with("::Err");
        let mut fallback_expected_return = None;
        let mut expected_return_for_infer = expected_return;
        if is_result_ctor {
            let needs_fallback = expected_return_for_infer
                .map(|ty| self.has_unresolved_ty(ty))
                .unwrap_or(true);
            if needs_fallback {
                let fallback = self.lower_type_expr(&function.sig.output);
                fallback_expected_return = Some(fallback);
                expected_return_for_infer = fallback_expected_return.as_ref();
            }
            let needs_sig_fallback = expected_return_for_infer
                .and_then(|ty| self.explicit_args_from_expected_result_ty(ty))
                .is_none();
            if needs_sig_fallback {
                let fallback = self.lower_type_expr(&function.sig.output);
                fallback_expected_return = Some(fallback);
                expected_return_for_infer = fallback_expected_return.as_ref();
            }
        }

        let mut explicit_args = explicit_args.to_vec();
        if is_result_ctor && explicit_args.is_empty() {
            let fallback_ty = expected_return_for_infer.or(fallback_expected_return.as_ref());
            if let Some(fallback_ty) = fallback_ty {
                if let Some(mut fallback_args) =
                    self.explicit_args_from_expected_result_ty(fallback_ty)
                {
                    if fallback_args.len() == generics.len() {
                        let is_unresolved =
                            |ty: &Ty| matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_));
                        if let Some(arg_ty) = arg_types.get(0) {
                            let arg_ty = self.unwrap_expr_actual_ty(arg_ty);
                            if !is_unresolved(arg_ty) {
                                match function.sig.name.as_str() {
                                    "Ok" => fallback_args[0] = arg_ty.clone(),
                                    "Err" if fallback_args.len() > 1 => {
                                        fallback_args[1] = arg_ty.clone();
                                    }
                                    _ => {}
                                }
                            }
                        }
                        for (idx, name) in generics.iter().enumerate() {
                            if let Some(arg) = fallback_args.get_mut(idx) {
                                if !is_unresolved(arg) {
                                    continue;
                                }
                                match name.as_str() {
                                    "T" => *arg = Self::unit_ty(),
                                    "E" => *arg = self.error_ty(),
                                    _ => {}
                                }
                            }
                        }
                        if fallback_args
                            .iter()
                            .any(|ty| !matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)))
                        {
                            return self.ensure_function_specialization_from_explicit_args(
                                def_id,
                                function,
                                &fallback_args,
                                span,
                            );
                        }
                    }
                }
            }
            if explicit_args.is_empty() && !generics.is_empty() {
                let mut inferred = vec![
                    Ty {
                        kind: TyKind::Infer(mir::ty::InferTy::FreshTy(0)),
                    };
                    generics.len()
                ];
                if let Some(arg_ty) = arg_types.get(0) {
                    let arg_ty = self.unwrap_expr_actual_ty(arg_ty);
                    if !matches!(arg_ty.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                        match function.sig.name.as_str() {
                            "Ok" => inferred[0] = arg_ty.clone(),
                            "Err" if inferred.len() > 1 => inferred[1] = arg_ty.clone(),
                            _ => {}
                        }
                    }
                }
                for (idx, name) in generics.iter().enumerate() {
                    if !matches!(inferred[idx].kind, TyKind::Infer(_) | TyKind::Error(_)) {
                        continue;
                    }
                    match name.as_str() {
                        "T" => inferred[idx] = Self::unit_ty(),
                        "E" => inferred[idx] = self.error_ty(),
                        _ => {}
                    }
                }
                if inferred
                    .iter()
                    .any(|ty| !matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)))
                {
                    explicit_args = inferred;
                }
            }
        }
        if is_result_ctor {
            let fallback_ty = expected_return_for_infer.or(fallback_expected_return.as_ref());
            if let Some(fallback_ty) = fallback_ty {
                if let Some(fallback_args) = self.explicit_args_from_expected_result_ty(fallback_ty)
                {
                    if fallback_args.len() == generics.len()
                        && explicit_args.len() == generics.len()
                    {
                        for (idx, explicit_arg) in explicit_args.iter_mut().enumerate() {
                            if !matches!(explicit_arg.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                                continue;
                            }
                            let Some(fallback_arg) = fallback_args.get(idx) else {
                                continue;
                            };
                            if matches!(fallback_arg.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                                continue;
                            }
                            *explicit_arg = fallback_arg.clone();
                        }
                    }
                }
            }
        }
        if is_result_ctor && explicit_args.len() == generics.len() {
            for (idx, name) in generics.iter().enumerate() {
                if let Some(explicit_arg) = explicit_args.get_mut(idx) {
                    if !matches!(explicit_arg.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                        continue;
                    }
                    match name.as_str() {
                        "T" => *explicit_arg = Self::unit_ty(),
                        "E" => *explicit_arg = self.error_ty(),
                        _ => {}
                    }
                }
            }
        }

        let substs = self.build_substs_from_args(
            &generics,
            None,
            None,
            &function.sig.inputs,
            Some(&function.sig.output),
            &explicit_args,
            arg_types,
            expected_return_for_infer,
            span,
        )?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let function_substs = args_in_order
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect::<mir::ty::SubstsRef>();
        let key = (def_id, function_substs.clone());

        if let Some(info) = self.function_specializations.get(&key) {
            return Ok(info.clone());
        }

        let sig = self.lower_function_sig_with_substs(&function.sig, None, &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}_{}", function.sig.name.as_str(), suffix, def_id);
        let fn_ty = self.function_pointer_ty(&sig);

        let item_span = self
            .hir_def_map
            .get(&def_id)
            .map(|item| item.span)
            .ok_or_else(|| crate::error::optimization_error("missing function item"))?;
        let (mir_item, body_id, body) = self.lower_function_with_substs(
            def_id,
            item_span,
            function,
            &sig,
            substs,
            &name,
            function_substs.clone(),
        )?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));

        let info = FunctionSpecializationInfo {
            def_id,
            substs: function_substs,
            name: name.clone(),
            sig: sig.clone(),
            fn_ty: fn_ty.clone(),
        };
        self.function_specializations.insert(key, info.clone());
        Ok(info)
    }

    fn ensure_function_specialization_from_explicit_args(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        explicit_args: &[Ty],
        span: Span,
    ) -> Result<FunctionSpecializationInfo> {
        let generics = function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();
        let substs = self.build_substs_from_explicit_args(&generics, explicit_args, span)?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let function_substs = args_in_order
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect::<mir::ty::SubstsRef>();
        let key = (def_id, function_substs.clone());

        if let Some(info) = self.function_specializations.get(&key) {
            return Ok(info.clone());
        }

        let sig = self.lower_function_sig_with_substs(&function.sig, None, &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}_{}", function.sig.name.as_str(), suffix, def_id);
        let fn_ty = self.function_pointer_ty(&sig);

        let item_span = self
            .hir_def_map
            .get(&def_id)
            .map(|item| item.span)
            .ok_or_else(|| crate::error::optimization_error("missing function item"))?;
        let (mir_item, body_id, body) = self.lower_function_with_substs(
            def_id,
            item_span,
            function,
            &sig,
            substs,
            &name,
            function_substs.clone(),
        )?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));

        let info = FunctionSpecializationInfo {
            def_id,
            substs: function_substs,
            name: name.clone(),
            sig: sig.clone(),
            fn_ty: fn_ty.clone(),
        };
        self.function_specializations.insert(key, info.clone());
        Ok(info)
    }

    fn ensure_method_specialization(
        &mut self,
        def: &MethodDefinition,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<MethodLoweringInfo> {
        let pre_key = (
            def.def_id,
            explicit_args.to_vec(),
            arg_types.to_vec(),
            expected_return.cloned(),
        );
        if let Some(info) = self.method_specialization_call_cache.get(&pre_key) {
            return Ok(info.clone());
        }
        let info = self.ensure_method_specialization_uncached(
            def,
            explicit_args,
            arg_types,
            expected_return,
            span,
        )?;
        self.method_specialization_call_cache
            .insert(pre_key, info.clone());
        Ok(info)
    }

    fn ensure_method_specialization_uncached(
        &mut self,
        def: &MethodDefinition,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<MethodLoweringInfo> {
        let impl_generics = def
            .impl_generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let method_generics = def
            .function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let generics = impl_generics.chain(method_generics).collect::<Vec<_>>();

        let is_result_ctor = def.method_name == "Ok"
            || def.method_name == "Err"
            || def.method_name.ends_with("::Ok")
            || def.method_name.ends_with("::Err");
        let mut fallback_expected_return = None;
        let mut expected_return_for_infer = expected_return;
        if is_result_ctor {
            let needs_fallback = expected_return_for_infer
                .map(|ty| self.has_unresolved_ty(ty))
                .unwrap_or(true);
            if needs_fallback {
                let fallback = self.lower_type_expr(&def.function.sig.output);
                fallback_expected_return = Some(fallback);
                expected_return_for_infer = fallback_expected_return.as_ref();
            }
        }
        if expected_return_for_infer.is_none() && is_result_ctor {
            let fallback = self.lower_type_expr(&def.function.sig.output);
            fallback_expected_return = Some(fallback);
            expected_return_for_infer = fallback_expected_return.as_ref();
        }
        if is_result_ctor {
            let needs_sig_fallback = expected_return_for_infer
                .and_then(|ty| self.explicit_args_from_expected_result_ty(ty))
                .is_none();
            if needs_sig_fallback {
                let fallback = self.lower_type_expr(&def.function.sig.output);
                fallback_expected_return = Some(fallback);
                expected_return_for_infer = fallback_expected_return.as_ref();
            }
        }
        let has_receiver = def
            .function
            .sig
            .inputs
            .first()
            .and_then(|param| match &param.pat.kind {
                hir::PatKind::Binding { name, .. } => Some(name.as_str() == "self"),
                _ => None,
            })
            .unwrap_or(false);
        let mut self_arg_ty = if has_receiver {
            arg_types.first()
        } else {
            expected_return_for_infer
        };
        if !has_receiver {
            if let Some(candidate) = self_arg_ty {
                if let Some(inner) = self.expr_inner_actual_ty(candidate) {
                    self_arg_ty = Some(inner);
                }
            }
        }
        let mut explicit_args = explicit_args.to_vec();
        if is_result_ctor && explicit_args.is_empty() {
            let fallback_ty = expected_return_for_infer.or(fallback_expected_return.as_ref());
            if let Some(fallback_ty) = fallback_ty {
                if let Some(fallback_args) = self.explicit_args_from_expected_result_ty(fallback_ty)
                {
                    if fallback_args.len() == generics.len()
                        && fallback_args
                            .iter()
                            .any(|ty| !matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)))
                    {
                        return self.ensure_method_specialization_from_explicit_args(
                            def,
                            &fallback_args,
                            span,
                        );
                    }
                }
            }
        }
        if is_result_ctor {
            let fallback_ty = expected_return_for_infer.or(fallback_expected_return.as_ref());
            if let Some(fallback_ty) = fallback_ty {
                if let Some(fallback_args) = self.explicit_args_from_expected_result_ty(fallback_ty)
                {
                    if fallback_args.len() == generics.len() {
                        if explicit_args.is_empty() {
                            explicit_args = fallback_args;
                        } else if explicit_args.len() == generics.len() {
                            for (idx, explicit_arg) in explicit_args.iter_mut().enumerate() {
                                if !matches!(explicit_arg.kind, TyKind::Infer(_) | TyKind::Error(_))
                                {
                                    continue;
                                }
                                let Some(fallback_arg) = fallback_args.get(idx) else {
                                    continue;
                                };
                                if matches!(fallback_arg.kind, TyKind::Infer(_) | TyKind::Error(_))
                                {
                                    continue;
                                }
                                *explicit_arg = fallback_arg.clone();
                            }
                        }
                    }
                }
            }
        }
        let substs = self.build_substs_from_args(
            &generics,
            Some(&def.self_ty),
            self_arg_ty,
            &def.function.sig.inputs,
            Some(&def.function.sig.output),
            &explicit_args,
            arg_types,
            expected_return_for_infer,
            span,
        )?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let method_substs = args_in_order
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect::<mir::ty::SubstsRef>();
        self.finish_method_specialization(def, substs, &args_in_order, method_substs, span, true)
    }

    fn ensure_method_specialization_from_explicit_args(
        &mut self,
        def: &MethodDefinition,
        explicit_args: &[Ty],
        span: Span,
    ) -> Result<MethodLoweringInfo> {
        let impl_generics = def
            .impl_generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let method_generics = def
            .function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let generics = impl_generics.chain(method_generics).collect::<Vec<_>>();

        let substs = self.build_substs_from_explicit_args(&generics, explicit_args, span)?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let method_substs = args_in_order
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect::<mir::ty::SubstsRef>();
        self.finish_method_specialization(def, substs, &args_in_order, method_substs, span, false)
    }

    /// Shared tail of `ensure_method_specialization_uncached`/
    /// `_from_explicit_args`: once a concrete `substs` map is known —
    /// however it was derived, from call-site argument/return-type
    /// inference or from fully-explicit turbofish args — building and
    /// caching the specialized `MethodLoweringInfo`/MIR body is identical.
    /// `carries_def_id` is the one real difference between the two
    /// callers (the explicit-args path's resulting `MethodLoweringInfo`
    /// omits `def_id`, matching its own prior behavior) — kept as a
    /// parameter rather than unified further since it's the only place
    /// that distinction matters.
    fn finish_method_specialization(
        &mut self,
        def: &MethodDefinition,
        substs: HashMap<String, Ty>,
        args_in_order: &[Ty],
        method_substs: mir::ty::SubstsRef,
        span: Span,
        carries_def_id: bool,
    ) -> Result<MethodLoweringInfo> {
        let key = (def.def_id, method_substs.clone());

        if let Some(info) = self.method_specializations.get(&key) {
            return Ok(info.clone());
        }

        let mut method_context = if let hir::TypeExprKind::Path(path) = &def.self_ty.kind {
            let mir_self_ty = self.lower_type_expr_with_substs(&def.self_ty, &substs);
            Some(MethodContext {
                def_id: def.self_def,
                path: path.segments.clone(),
                mir_self_ty,
                assoc_types: def.assoc_types.clone(),
            })
        } else {
            None
        };

        let sig = self.lower_function_sig_with_substs(
            &def.function.sig,
            method_context.as_ref(),
            &substs,
        );
        let suffix = self.specialization_suffix(args_in_order);
        let name = format!("{}__{}", def.method_name, suffix);
        let fn_ty = self.function_pointer_ty(&sig);

        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let span = def
            .function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(span);
        let mir_body = BodyBuilder::new(
            self,
            &def.function,
            &sig,
            span,
            method_context.take(),
            substs,
        )
        .lower()?;

        let mir_function = mir::Function {
            name: mir::Symbol::new(name.clone()),
            def_id: Some(def.def_id),
            substs: method_substs.clone(),
            sig: sig.clone(),
            body_id,
            abi: self.map_abi(&def.function.sig.abi),
            is_extern: false,
            attrs: Vec::new(),
        };
        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, mir_body));

        let info = MethodLoweringInfo {
            def_id: if carries_def_id { Some(def.def_id) } else { None },
            substs: method_substs,
            sig,
            fn_name: name.clone(),
            fn_ty,
            struct_def: def.self_def,
        };
        self.method_specializations.insert(key, info.clone());
        Ok(info)
    }

    fn lower_function_sig(
        &mut self,
        sig: &hir::FunctionSig,
        method_context: Option<&MethodContext>,
    ) -> mir::FunctionSig {
        mir::FunctionSig {
            inputs: sig
                .inputs
                .iter()
                .map(|param| {
                    self.lower_type_expr_with_context_for_abi(&param.ty, method_context, &sig.abi)
                })
                .collect(),
            output: self.lower_type_expr_with_context_for_abi(
                &sig.output,
                method_context,
                &sig.abi,
            ),
        }
    }

    fn lower_function_sig_with_substs(
        &mut self,
        sig: &hir::FunctionSig,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
    ) -> mir::FunctionSig {
        mir::FunctionSig {
            inputs: sig
                .inputs
                .iter()
                .map(|param| {
                    self.lower_type_expr_with_context_and_substs_for_abi(
                        &param.ty,
                        method_context,
                        substs,
                        &sig.abi,
                    )
                })
                .collect(),
            output: self.lower_type_expr_with_context_and_substs_for_abi(
                &sig.output,
                method_context,
                substs,
                &sig.abi,
            ),
        }
    }

    fn lower_type_expr_with_context_for_abi(
        &mut self,
        ty: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
        abi: &hir::Abi,
    ) -> Ty {
        if matches!(abi, hir::Abi::C { .. } | hir::Abi::System { .. }) {
            match &ty.kind {
                hir::TypeExprKind::Ref(inner) => {
                    let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                    return Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(inner_ty),
                            mutbl: Mutability::Not,
                        }),
                    };
                }
                hir::TypeExprKind::Ptr(inner) => {
                    let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                    return Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(inner_ty),
                            mutbl: Mutability::Mut,
                        }),
                    };
                }
                _ => {}
            }
        }
        self.lower_type_expr_with_context(ty, method_context)
    }

    fn lower_type_expr_with_context_and_substs_for_abi(
        &mut self,
        ty: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
        abi: &hir::Abi,
    ) -> Ty {
        if matches!(abi, hir::Abi::C { .. } | hir::Abi::System { .. }) {
            match &ty.kind {
                hir::TypeExprKind::Ref(inner) => {
                    let inner_ty =
                        self.lower_type_expr_with_context_and_substs(inner, method_context, substs);
                    return Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(inner_ty),
                            mutbl: Mutability::Not,
                        }),
                    };
                }
                hir::TypeExprKind::Ptr(inner) => {
                    let inner_ty =
                        self.lower_type_expr_with_context_and_substs(inner, method_context, substs);
                    return Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(inner_ty),
                            mutbl: Mutability::Mut,
                        }),
                    };
                }
                _ => {}
            }
        }
        self.lower_type_expr_with_context_and_substs(ty, method_context, substs)
    }

    fn map_abi(&self, abi: &hir::Abi) -> mir::ty::Abi {
        match abi {
            hir::Abi::Rust => mir::ty::Abi::Rust,
            hir::Abi::C { unwind } => mir::ty::Abi::C { unwind: *unwind },
            hir::Abi::Named(_) => mir::ty::Abi::Rust,
            hir::Abi::System { unwind } => mir::ty::Abi::System { unwind: *unwind },
            _ => mir::ty::Abi::Rust,
        }
    }

    fn specialization_suffix(&self, args: &[Ty]) -> String {
        let mut hasher = DefaultHasher::new();
        for ty in args {
            ty.hash(&mut hasher);
        }
        format!("mono_{:x}", hasher.finish())
    }

    fn build_substs_from_args(
        &mut self,
        generics: &[String],
        self_ty: Option<&hir::TypeExpr>,
        self_arg_ty: Option<&Ty>,
        params: &[hir::Param],
        return_ty: Option<&hir::TypeExpr>,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<HashMap<String, Ty>> {
        if params.len() != arg_types.len() {
            self.emit_error(
                span,
                format!(
                    "generic call argument count mismatch: expected {}, got {}",
                    params.len(),
                    arg_types.len()
                ),
            );
            return Err(crate::error::optimization_error(
                "generic call argument count mismatch",
            ));
        }
        if !explicit_args.is_empty() && explicit_args.len() != generics.len() {
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
            if matches!(ty.kind, TyKind::Infer(_)) {
                continue;
            }
            substs.insert(name.clone(), ty);
        }

        let has_explicit_substitutions = explicit_args.len() == generics.len();
        let return_ty = return_ty.map(|ty| self.unwrap_expr_type_expr(ty));
        let expected_return = expected_return.map(|ty| self.unwrap_expr_actual_ty(ty));
        if !has_explicit_substitutions {
            if let (Some(self_ty), Some(self_arg_ty)) = (self_ty, self_arg_ty) {
                self.infer_generic_from_type_expr(
                    self_ty,
                    self_arg_ty,
                    generics,
                    &mut substs,
                    span,
                )?;
            }

            for (param, actual_ty) in params.iter().zip(arg_types.iter()) {
                self.infer_generic_from_type_expr(
                    &param.ty,
                    actual_ty,
                    generics,
                    &mut substs,
                    span,
                )?;
            }
            if let (Some(return_ty), Some(expected_return)) = (return_ty, expected_return) {
                self.infer_generic_from_type_expr(
                    return_ty,
                    expected_return,
                    generics,
                    &mut substs,
                    span,
                )?;
            }
        }
        if substs.len() != generics.len() {
            if let (Some(return_ty), Some(expected_return)) = (return_ty, expected_return) {
                self.fill_missing_substs_from_expected_return(
                    return_ty,
                    expected_return,
                    generics,
                    &mut substs,
                );
            }
        }
        if substs.len() != generics.len() {
            if let Some(expected_return) = expected_return {
                let expected_return = match &expected_return.kind {
                    TyKind::Ref(_, inner, _) => inner.as_ref(),
                    TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
                    _ => expected_return,
                };
                let mut actual_type_args = match &expected_return.kind {
                    TyKind::Adt(_, substs) | TyKind::Opaque(_, substs) => substs
                        .iter()
                        .filter_map(|arg| match arg {
                            mir::ty::GenericArg::Type(ty) => Some(self.unwrap_expr_actual_ty(ty)),
                            _ => None,
                        })
                        .collect::<Vec<_>>(),
                    _ => Vec::new(),
                };
                if actual_type_args.is_empty() {
                    let layout = self
                        .enum_layout_for_ty_exact(expected_return)
                        .or_else(|| self.enum_layout_for_ty(expected_return));
                    if let Some(layout) = layout {
                        actual_type_args = layout
                            .args
                            .iter()
                            .map(|ty| self.unwrap_expr_actual_ty(ty))
                            .collect::<Vec<_>>();
                    }
                }
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
            }
        }
        if substs.len() != generics.len() {
            if let Some(self_arg_ty) = self_arg_ty {
                if let Some(actual_args) = self.explicit_args_from_expected_result_ty(self_arg_ty) {
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
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(expected_return) = expected_return {
                let expected_return = self.unwrap_expr_actual_ty(expected_return);
                let expected_return = match &expected_return.kind {
                    TyKind::Ref(_, inner, _) => inner.as_ref(),
                    TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
                    _ => expected_return,
                };
                let layout = self
                    .enum_layout_for_ty_exact(expected_return)
                    .or_else(|| self.enum_layout_for_ty(expected_return));
                if let Some(layout) = layout {
                    let is_result_layout = self
                        .enum_defs
                        .get(&layout.def_id)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result_layout && generics.len() >= 2 {
                        if let Some(def) = self.enum_defs.get(&layout.def_id) {
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
                            if let Some(name) = generics.get(0) {
                                if !substs.contains_key(name) {
                                    if let Some(ok) = ok_payload.as_ref() {
                                        if !matches!(ok.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                                            substs.insert(name.to_string(), ok.clone());
                                        }
                                    }
                                }
                            }
                            if let Some(name) = generics.get(1) {
                                if !substs.contains_key(name) {
                                    if let Some(err) = err_payload.as_ref() {
                                        if !matches!(err.kind, TyKind::Infer(_) | TyKind::Error(_))
                                        {
                                            substs.insert(name.to_string(), err.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                if let hir::TypeExprKind::Path(path) = &return_ty.kind {
                    if self.is_result_path(path) {
                        let fallback = self.lower_type_expr(return_ty);
                        // JUSTIFY: best-effort inference from Result path;
                        // a separate fallback below uses explicit_args_from_expected_result_ty.
                        if let Err(e) = self.infer_generic_from_type_expr(
                            return_ty,
                            &fallback,
                            generics,
                            &mut substs,
                            span,
                        ) {
                            self.emit_warning(span, format!("generic type inference error: {e}"));
                        }
                        let fallback = self.lower_type_expr(return_ty);
                        if let Some(fallback_args) =
                            self.explicit_args_from_expected_result_ty(&fallback)
                        {
                            if fallback_args.len() == generics.len() {
                                for (name, fallback_arg) in
                                    generics.iter().zip(fallback_args.into_iter())
                                {
                                    if substs.contains_key(name) {
                                        continue;
                                    }
                                    if matches!(fallback_arg.kind, TyKind::Infer(_)) {
                                        continue;
                                    }
                                    substs.insert(name.to_string(), fallback_arg);
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                if let hir::TypeExprKind::Path(path) = &return_ty.kind {
                    if path
                        .segments
                        .last()
                        .map(|seg| seg.name.as_str() == "Self")
                        .unwrap_or(false)
                    {
                        let mut fallback_ty =
                            expected_return.map(|ty| self.unwrap_expr_actual_ty(ty).clone());
                        if fallback_ty.is_none() {
                            fallback_ty = Some(self.lower_type_expr(return_ty));
                        }
                        if let Some(fallback_ty) = fallback_ty.as_ref() {
                            if let Some(fallback_args) =
                                self.explicit_args_from_expected_result_ty(fallback_ty)
                            {
                                if fallback_args.len() == generics.len() {
                                    for (name, fallback_arg) in
                                        generics.iter().zip(fallback_args.into_iter())
                                    {
                                        if substs.contains_key(name) {
                                            continue;
                                        }
                                        if matches!(fallback_arg.kind, TyKind::Infer(_)) {
                                            continue;
                                        }
                                        substs.insert(name.to_string(), fallback_arg);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(self_arg_ty) = self_arg_ty {
                let layout = self
                    .enum_layout_for_ty_exact(self_arg_ty)
                    .or_else(|| self.enum_layout_for_ty(self_arg_ty));
                if let Some(layout) = layout {
                    let is_result_layout = self
                        .enum_defs
                        .get(&layout.def_id)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result_layout {
                        if let Some(return_ty) = return_ty {
                            let fallback = self.lower_type_expr(return_ty);
                            if let Some(fallback_args) =
                                self.explicit_args_from_expected_result_ty(&fallback)
                            {
                                if fallback_args.len() == generics.len() {
                                    for (name, fallback_arg) in
                                        generics.iter().zip(fallback_args.into_iter())
                                    {
                                        if substs.contains_key(name) {
                                            continue;
                                        }
                                        if matches!(fallback_arg.kind, TyKind::Infer(_)) {
                                            continue;
                                        }
                                        substs.insert(name.to_string(), fallback_arg);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                let mut output_ty = return_ty;
                while let Some(inner) = self.expr_inner_type_expr(output_ty) {
                    output_ty = inner;
                }
                if let hir::TypeExprKind::Path(path) = &output_ty.kind {
                    if self.is_result_path(path) {
                        if let Some(args) = path.segments.last().and_then(|seg| seg.args.as_ref()) {
                            let mut output_args = Vec::new();
                            for arg in &args.args {
                                let hir::GenericArg::Type(type_arg) = arg else {
                                    continue;
                                };
                                output_args.push(self.lower_type_expr(type_arg));
                            }
                            if output_args.len() == generics.len() {
                                for (name, output_arg) in
                                    generics.iter().zip(output_args.into_iter())
                                {
                                    if substs.contains_key(name) {
                                        continue;
                                    }
                                    if matches!(output_arg.kind, TyKind::Infer(_)) {
                                        if substs.is_empty() {
                                            continue;
                                        }
                                    }
                                    substs.insert(name.to_string(), output_arg);
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                if let hir::TypeExprKind::Path(path) = &return_ty.kind {
                    if self.is_result_path(path) {
                        if let Some(args) = path.segments.last().and_then(|seg| seg.args.as_ref()) {
                            let mut output_args = Vec::new();
                            for arg in &args.args {
                                let hir::GenericArg::Type(type_arg) = arg else {
                                    continue;
                                };
                                output_args.push(self.lower_type_expr(type_arg));
                            }
                            if output_args.len() == generics.len() {
                                for (name, output_arg) in
                                    generics.iter().zip(output_args.into_iter())
                                {
                                    if substs.contains_key(name) {
                                        continue;
                                    }
                                    substs.insert(name.to_string(), output_arg);
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                let mut output_ty = return_ty;
                while let Some(inner) = self.expr_inner_type_expr(output_ty) {
                    output_ty = inner;
                }
                if let hir::TypeExprKind::Path(path) = &output_ty.kind {
                    if self.is_result_path(path) {
                        let fallback = self.lower_type_expr(return_ty);
                        if let Some(fallback_args) =
                            self.explicit_args_from_expected_result_ty(&fallback)
                        {
                            if fallback_args.len() == generics.len() {
                                for (name, fallback_arg) in
                                    generics.iter().zip(fallback_args.into_iter())
                                {
                                    if substs.contains_key(name) {
                                        continue;
                                    }
                                    if matches!(fallback_arg.kind, TyKind::Infer(_)) {
                                        continue;
                                    }
                                    substs.insert(name.to_string(), fallback_arg);
                                }
                            }
                        }
                    }
                }
            }
        }
        if substs.len() != generics.len() {
            if let Some(return_ty) = return_ty {
                let fallback = self.lower_type_expr(return_ty);
                if let Some(fallback_args) = self.explicit_args_from_expected_result_ty(&fallback) {
                    if fallback_args.len() >= generics.len() {
                        for (idx, name) in generics.iter().enumerate() {
                            if substs.contains_key(name) {
                                continue;
                            }
                            let Some(fallback_arg) = fallback_args.get(idx) else {
                                continue;
                            };
                            if matches!(fallback_arg.kind, TyKind::Infer(_)) {
                                continue;
                            }
                            substs.insert(name.to_string(), fallback_arg.clone());
                        }
                    }
                }
            }
        }
        for name in generics {
            if substs.contains_key(name) {
                continue;
            }
            if name.as_str() == "T" {
                substs.insert(name.to_string(), Self::unit_ty());
            } else if name.as_str() == "E" {
                substs.insert(name.to_string(), self.error_ty());
            }
        }
        if substs.len() != generics.len() {
            let missing = generics
                .iter()
                .filter(|name| !substs.contains_key(*name))
                .collect::<Vec<_>>();
            if missing.len() == 1 && missing[0].as_str() == "E" {
                substs.insert("E".to_string(), self.error_ty());
            }
        }

        for name in generics {
            if !substs.contains_key(name) {
                match name.as_str() {
                    "T" => {
                        substs.insert(name.to_string(), Self::unit_ty());
                        continue;
                    }
                    "E" => {
                        substs.insert(name.to_string(), self.error_ty());
                        continue;
                    }
                    _ => {}
                }
                self.emit_error(
                    span,
                    format!(
                        "unable to infer generic parameter `{}`; add explicit type arguments",
                        name
                    ),
                );
                return Err(crate::error::optimization_error(
                    "generic parameter inference failed",
                ));
            }
        }

        Ok(substs)
    }

    fn fill_missing_substs_from_expected_return(
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

    fn build_substs_from_explicit_args(
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

    fn is_result_path(&self, path: &hir::Path) -> bool {
        path.segments
            .last()
            .map(|segment| segment.name.as_str() == "Result")
            .unwrap_or(false)
    }

    fn explicit_args_from_expected_result_ty(&self, expected_ty: &Ty) -> Option<Vec<Ty>> {
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
            .enum_defs
            .get(adt)
            .map(|def| def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result"))
            .or_else(|| {
                self.struct_defs.get(adt).map(|def| {
                    def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                })
            })
            .unwrap_or(false);
        if !is_result {
            if let Some(layout) = self.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
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
                        if let Some(def) = self.enum_defs.get(&layout.def_id) {
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

    fn expr_inner_type_expr<'a>(&self, ty_expr: &'a hir::TypeExpr) -> Option<&'a hir::TypeExpr> {
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

    fn expr_inner_actual_ty<'a>(&self, actual_ty: &'a Ty) -> Option<&'a Ty> {
        let (def_id, substs) = match &actual_ty.kind {
            TyKind::Adt(adt, substs) => (adt.did, substs),
            TyKind::Opaque(def_id, substs) => (*def_id, substs),
            _ => return None,
        };
        let is_expr = self
            .struct_defs
            .get(&def_id)
            .map(|def| def.name.as_str() == "Expr" || def.name.as_str().ends_with("::Expr"))
            .unwrap_or(false)
            || self
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

    fn unwrap_expr_type_expr<'a>(&self, mut ty_expr: &'a hir::TypeExpr) -> &'a hir::TypeExpr {
        while let Some(inner) = self.expr_inner_type_expr(ty_expr) {
            ty_expr = inner;
        }
        ty_expr
    }

    fn unwrap_expr_actual_ty<'a>(&self, mut actual_ty: &'a Ty) -> &'a Ty {
        while let Some(inner) = self.expr_inner_actual_ty(actual_ty) {
            actual_ty = inner;
        }
        actual_ty
    }

    fn infer_generic_from_type_expr(
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
                        self.enum_variants
                            .get(def_id)
                            .map(|variant| variant.enum_def)
                    } else {
                        None
                    }
                });
                if let Some((actual_def_id, actual_substs, actual_is_opaque)) =
                    match &actual_ty.kind {
                        TyKind::Adt(adt, substs) => Some((Some(adt.did), substs, false)),
                        TyKind::Opaque(def_id, substs) => Some((Some(*def_id), substs, true)),
                        _ => None,
                    }
                {
                    let mut matches_def = false;
                    if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                        if let Some(actual_def_id) = actual_def_id {
                            matches_def =
                                *def_id == actual_def_id || variant_enum_def == Some(actual_def_id);
                        }
                        if !matches_def {
                            if let Some(name) = path.segments.last().map(|seg| seg.name.as_str()) {
                                if let Some(actual_def_id) = actual_def_id {
                                    matches_def = self
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
                                .enum_defs
                                .get(&actual_def_id)
                                .map(|def| {
                                    def.name.as_str() == name
                                        || def.name.as_str().ends_with(&format!("::{}", name))
                                })
                                .unwrap_or(false)
                                || self
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
                            hir::Res::Def(def_id) => Some(*def_id),
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
                            let enum_def_id = variant_enum_def.unwrap_or(def_id);
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
                    let enum_def_id = if self.enum_defs.contains_key(def_id) {
                        Some(*def_id)
                    } else {
                        variant_enum_def
                    };
                    if let Some(enum_def_id) = enum_def_id {
                        let mut candidates: Vec<&EnumLayout> = self
                            .enum_layouts
                            .values()
                            .filter(|layout| layout.def_id == enum_def_id)
                            .collect();
                        if !candidates.is_empty() {
                            let exact: Vec<&EnumLayout> = candidates
                                .iter()
                                .copied()
                                .filter(|layout| layout.enum_ty == *actual_ty)
                                .collect();
                            if !exact.is_empty() {
                                candidates = exact;
                            }
                        }
                        if !candidates.is_empty() {
                            let mut scored: Vec<(&EnumLayout, usize, usize, String)> = candidates
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
                            let layout = scored[0].0;
                            let layout_args = layout.args.clone();
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
                    if let Some(struct_def) = self.struct_defs.get(def_id).cloned() {
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
                        let sig = match self.function_sigs.get(def_id).cloned() {
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

    fn lower_type_expr_with_context_and_substs(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    // `Self::AssocName` (e.g. `Index::index`'s `-> Self::
                    // Output`) is a *projection* through this impl's own
                    // `type AssocName = ...;` binding, not `Self` itself —
                    // resolve it via that binding (substituted with this
                    // specialization's own `substs`, e.g. `T` -> `BenchCase`)
                    // before falling back to treating a bare `Self` as the
                    // whole receiver type.
                    if path.segments.len() > 1 {
                        if let Some(assoc_name) = path.segments.get(1) {
                            if let Some(assoc_ty) =
                                ctx.assoc_types.get(assoc_name.name.as_str())
                            {
                                return self.lower_type_expr_with_substs(assoc_ty, substs);
                            }
                        }
                    } else {
                        return ctx.mir_self_ty.clone();
                    }
                }
            }
        }
        if substs.is_empty() {
            return self.lower_type_expr_with_context(ty_expr, method_context);
        }
        self.lower_type_expr_with_substs(ty_expr, substs)
    }

    fn lower_type_expr_with_context(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    if path.segments.len() > 1 {
                        if let Some(assoc_name) = path.segments.get(1) {
                            if let Some(assoc_ty) =
                                ctx.assoc_types.get(assoc_name.name.as_str()).cloned()
                            {
                                return self.lower_type_expr_with_context(
                                    &assoc_ty,
                                    method_context,
                                );
                            }
                        }
                        return self.error_ty();
                    }
                    return ctx.mir_self_ty.clone();
                }
            }
        }

        match &ty_expr.kind {
            hir::TypeExprKind::Ref(inner) => {
                if self.is_string_slice_ref(inner) {
                    return self.string_slice_ty();
                }
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(inner_ty),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            _ => self.lower_type_expr(ty_expr),
        }
    }

    fn lower_body(
        &mut self,
        item: &hir::Item,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        method_context: Option<MethodContext>,
    ) -> Result<mir::Body> {
        let span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(item.span);

        BodyBuilder::new(self, function, sig, span, method_context, HashMap::new()).lower()
    }

    fn lower_const(
        &mut self,
        def_id: hir::DefId,
        konst: &hir::Const,
    ) -> Result<mir::Item> {
        let declared_ty = self.lower_type_expr(&konst.ty);
        let ty = match declared_ty.clone() {
            Ty {
                kind: TyKind::Adt(adt, args),
            } => {
                let type_args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                        mir::ty::GenericArg::Lifetime(_) | mir::ty::GenericArg::Const(_) => None,
                    })
                    .collect::<Vec<_>>();
                self.struct_layout_for_instance(adt.did, &type_args, konst.ty.span)
                    .map(|layout| layout.ty)
                    .unwrap_or(declared_ty)
            }
            ty => ty,
        };
        let key = self.const_key(konst.name.as_str(), konst.body.value.span);
        let container_args = self.container_args_from_type_expr(&konst.ty);
        let Some(init_constant) = self.lower_const_expr(
            &konst.body.value,
            Some(&ty),
            container_args.as_ref(),
        ) else {
            return self.lower_executable_const(def_id, konst, ty, key, None);
        };
        let init = mir::Operand::Constant(init_constant.clone());

        self.const_values.insert(
            def_id,
            ConstInfo {
                ty: ty.clone(),
                value: init_constant.clone(),
            },
        );
        // A const resolved this way (directly foldable — no `let`, no
        // side effects requiring the real interpreter) never becomes a
        // comptime entry, so nothing else would ever surface its value to
        // `driver.rs`'s `resolved_consts` the way an interpreted const's
        // value already does — see `all_resolved_const_values`, the
        // exporter this feeds.
        self.resolved_const_values.insert(key, init_constant);

        let mir_static = mir::Static {
            name: konst.name.clone().into(),
            ty,
            init,
            mutability: mir::Mutability::Not,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Static(mir_static),
        };
        self.next_mir_id += 1;

        Ok(mir_item)
    }

    fn lower_executable_const(
        &mut self,
        def_id: hir::DefId,
        konst: &hir::Const,
        ty: Ty,
        key: String,
        const_block_hir_id: Option<hir::HirId>,
    ) -> Result<mir::Item> {
        self.executable_consts
            .insert(def_id, (mir::Symbol::from(&konst.name), ty.clone()));
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let fn_name = self.synthetic_const_function_name(&konst.name, &key);
        let synthetic_item = hir::Item {
            hir_id: konst.body.hir_id,
            def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Function(hir::Function {
                sig: hir::FunctionSig {
                    name: hir::Symbol::new(fn_name.clone()),
                    inputs: Vec::new(),
                    output: konst.ty.clone(),
                    generics: hir::Generics {
                        params: Vec::new(),
                        where_clause: None,
                    },
                    abi: hir::Abi::Rust,
                },
                body: Some(hir::Block {
                    hir_id: konst.body.hir_id,
                    stmts: Vec::new(),
                    expr: Some(Box::new(konst.body.value.clone())),
                }),
                is_const: true,
                is_extern: false,
                is_async: false,
                attrs: Vec::new(),
            }),
            span: konst.body.value.span,
        };
        let hir::ItemKind::Function(function) = &synthetic_item.kind else {
            unreachable!();
        };

        let sig = mir::FunctionSig {
            inputs: Vec::new(),
            output: ty.clone(),
        };
        let body = self.lower_body(&synthetic_item, function, &sig, None)?;
        self.extra_bodies.push((body_id, body));

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::ExecutableConst(mir::ExecutableConst {
                name: mir::Symbol::from(&konst.name),
                function_name: mir::Symbol::new(fn_name),
                ty,
                body_id,
                key,
                span: konst.body.value.span,
                const_block_hir_id,
            }),
        };
        self.next_mir_id += 1;
        Ok(mir_item)
    }

    /// Registers an expression-position `const { .. }` block as an extra
    /// `ExecutableConst` MIR item (mirroring `lower_executable_const`, but
    /// for an ad hoc block rather than a named top-level `const` item),
    /// pushed onto `extra_items` so it becomes a real `lir.comptime_entries`
    /// entry once this package's own MIR/LIR is built. This is a
    /// best-effort side channel purely for real (interpreter-backed)
    /// comptime validation — the block's own operand lowering already
    /// falls back to ordinary runtime code when no comptime value is
    /// available (see `const_block_value_to_mir_constant`'s callers), so a
    /// failure here is reported, not fatal.
    fn register_const_block_comptime_entry(
        &mut self,
        expr_hir_id: hir::HirId,
        const_block: &hir::ExprConstBlock,
        span: Span,
    ) {
        self.register_const_block_comptime_entry_direct(
            expr_hir_id,
            &const_block.ty,
            &const_block.body,
            span,
        );
    }

    /// Shared by `register_const_block_comptime_entry` (found incidentally
    /// while walking a body that contains a `const { .. }` block) and
    /// `transform_comptime_request` (fed directly from a
    /// `fp_typing::ComptimeRequest`'s own `expected_ty`/`block.expr`, with
    /// no body walk at all) — both just need `ty`/`body` unboxed, not an
    /// `hir::ExprConstBlock` specifically.
    fn register_const_block_comptime_entry_direct(
        &mut self,
        expr_hir_id: hir::HirId,
        ty: &hir::TypeExpr,
        body: &hir::Expr,
        span: Span,
    ) {
        let lowered_ty = self.lower_type_expr(ty);
        let name = hir::Symbol::new(format!(
            "__const_block_{}_{}",
            expr_hir_id.package_id.0, expr_hir_id.index
        ));
        let konst = hir::Const {
            name: name.clone(),
            ty: ty.clone(),
            body: hir::Body {
                hir_id: expr_hir_id,
                params: Vec::new(),
                value: body.clone(),
            },
        };
        let key = self.const_key(name.as_str(), span);
        let def_id = self.next_synthetic_def_id();
        match self.lower_executable_const(def_id, &konst, lowered_ty, key, Some(expr_hir_id)) {
            Ok(mir_item) => self.extra_items.push(mir_item),
            Err(error) => self.emit_warning(
                span,
                format!("const block not lowerable for comptime validation: {error}"),
            ),
        }
    }

    fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        if let hir::TypeExprKind::Ref(inner) = &ty_expr.kind {
            if self.is_string_slice_ref(inner) {
                return self.string_slice_ty();
            }
        }
        if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
            if path.segments.last().is_some_and(|segment| {
                matches!(
                    segment.name.as_str(),
                    "bool"
                        | "char"
                        | "str"
                        | "i8"
                        | "i16"
                        | "i32"
                        | "i64"
                        | "i128"
                        | "isize"
                        | "u8"
                        | "u16"
                        | "u32"
                        | "u64"
                        | "u128"
                        | "usize"
                        | "f16"
                        | "f32"
                        | "f64"
                        | "f128"
                )
            }) {
                return self.lower_path_type(path, ty_expr.span);
            }
        }
        if let Some(ty) = self.typeck_type_exprs.get(&ty_expr.hir_id) {
            return ty.clone();
        }
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
                        .map(|elem| Box::new(self.lower_type_expr(elem)))
                        .collect(),
                ),
            },
            hir::TypeExprKind::Array(elem, len_expr) => {
                let elem_ty = self.lower_type_expr(elem);
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
                let elem_ty = self.lower_type_expr(elem);
                Ty {
                    kind: TyKind::Slice(Box::new(elem_ty)),
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                let inner_ty = self.lower_type_expr(inner);
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(inner_ty),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                if self.is_string_slice_ref(inner) {
                    return self.string_slice_ty();
                }
                let inner_ty = self.lower_type_expr(inner);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Path(path) => self.lower_path_type(path, ty_expr.span),
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                let inputs = fn_ptr
                    .inputs
                    .iter()
                    .map(|ty| Box::new(self.lower_type_expr(ty)))
                    .collect();
                let output = Box::new(self.lower_type_expr(&fn_ptr.output));
                Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs,
                                output,
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::Rust,
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
            // The typeck-resolved type for this node is looked up via
            // `typeck_type_exprs` above (populated from the type checker's
            // `resolve_pending_type_const_blocks`); reaching here means that
            // lookup missed, so fall back the same way `Infer` does.
            hir::TypeExprKind::ConstBlock(_) => self.error_ty(),
            hir::TypeExprKind::Type => Ty { kind: TyKind::Type },
            hir::TypeExprKind::Any => Ty { kind: TyKind::Any },
            // Erases to `base`'s `TyKind` directly — there is deliberately
            // no corresponding `TyKind::Refinement` (see the doc comment on
            // `hir::TypeExprKind::Refinement`).
            hir::TypeExprKind::Refinement { base, .. } => self.lower_type_expr(base),
        }
    }

    fn eval_type_length(&self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    self.const_values
                        .get(def_id)
                        .and_then(|info| match &info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value as u64),
                            mir::ConstantKind::UInt(value) => Some(*value),
                            _ => None,
                        })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn next_synthetic_def_id(&mut self) -> hir::DefId {
        let id = self.next_synthetic_hir_def_id;
        self.next_synthetic_hir_def_id = self.next_synthetic_hir_def_id.saturating_add(1);
        id
    }

    fn lower_structural_type_expr(&mut self, structural: &hir::TypeStructural, span: Span) -> Ty {
        let mut entries_ty: Option<&hir::TypeExpr> = None;
        if structural.fields.len() == 1 {
            if let Some(field) = structural.fields.first() {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                }
            }
        } else {
            for field in &structural.fields {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                    break;
                }
            }
        }

        if let Some(entries_ty) = entries_ty {
            let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
            match &entries_ty.kind {
                hir::TypeExprKind::Path(path) => {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Vec" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = Some(inner.as_ref());
                                    }
                                }
                            }
                        }
                    }
                }
                hir::TypeExprKind::Slice(inner) => {
                    entry_ty_expr = Some(inner.as_ref());
                }
                _ => {}
            }

            if let Some(mut entry_ty_expr) = entry_ty_expr {
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Expr" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = inner.as_ref();
                                    }
                                }
                            }
                        }
                    }
                }

                let mut key_ty_expr = None;
                let mut value_ty_expr = None;
                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        if let Some(tail) = path.segments.last() {
                            if tail.name.as_str() == "HashMapEntry" {
                                if let Some(args) = &tail.args {
                                    if args.args.len() == 2 {
                                        if let (
                                            hir::GenericArg::Type(key),
                                            hir::GenericArg::Type(value),
                                        ) = (&args.args[0], &args.args[1])
                                        {
                                            key_ty_expr = Some(key.as_ref());
                                            value_ty_expr = Some(value.as_ref());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            key_ty_expr = Some(fields[0].as_ref());
                            value_ty_expr = Some(fields[1].as_ref());
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }

                if let (Some(key_ty_expr), Some(value_ty_expr)) = (key_ty_expr, value_ty_expr) {
                    let key_ty = self.lower_type_expr(key_ty_expr);
                    let value_ty = self.lower_type_expr(value_ty_expr);
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Tuple(vec![Box::new(key_ty), Box::new(value_ty)]),
                        })),
                    };
                }
            }
        }

        let mut fields = Vec::with_capacity(structural.fields.len());
        for field in &structural.fields {
            fields.push(StructFieldDef {
                name: field.name.as_str().to_string(),
                ty: (*field.ty).clone(),
            });
        }

        let key_fields = fields
            .iter()
            .map(|field| (field.name.clone(), self.lower_type_expr(&field.ty)))
            .collect::<Vec<_>>();
        let key = StructuralLayoutKey { fields: key_fields };

        let def_id = if let Some(def_id) = self.structural_defs.get(&key).copied() {
            def_id
        } else {
            let def_id = self.next_synthetic_def_id();
            let mut field_index = HashMap::new();
            for (idx, field) in fields.iter().enumerate() {
                if field_index.insert(field.name.clone(), idx).is_some() {
                    self.emit_error(span, format!("duplicate structural field `{}`", field.name));
                }
            }

            let name = format!("__structural_{}", def_id);
            self.struct_defs_by_tail_name
                .entry(Self::name_tail(&name).to_string())
                .or_default()
                .push(def_id);
            self.struct_defs.insert(
                def_id,
                StructDefinition {
                    name,
                    generics: Vec::new(),
                    fields: fields.clone(),
                    field_index,
                },
            );
            self.structural_defs.insert(key, def_id);
            def_id
        };

        self.struct_layout_for_instance(def_id, &[], span)
            .map(|layout| layout.ty)
            .unwrap_or_else(|| self.error_ty())
    }

    fn lower_type_binary_op_expr(&mut self, type_op: &hir::TypeBinaryOp, span: Span) -> Ty {
        match type_op.kind {
            TypeBinaryOpKind::Union => self.lower_union_type_expr(&type_op.lhs, &type_op.rhs, span),
            TypeBinaryOpKind::Add | TypeBinaryOpKind::Intersect | TypeBinaryOpKind::Subtract => {
                let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span);
                let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span);
                let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                    self.emit_error(
                        span,
                        "type arithmetic requires structural or named struct operands",
                    );
                    return self.error_ty();
                };

                let combined = match type_op.kind {
                    TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Intersect => self.intersect_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Subtract => self.subtract_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Union => unreachable!("union handled above"),
                };
                let fields = combined
                    .into_iter()
                    .map(|field| hir::TypeStructuralField {
                        name: hir::Symbol::new(field.name),
                        ty: Box::new(field.ty),
                    })
                    .collect::<Vec<_>>();
                self.lower_structural_type_expr(&hir::TypeStructural { fields }, span)
            }
        }
    }

    fn structural_fields_for_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
        span: Span,
    ) -> Option<Vec<StructFieldDef>> {
        match &ty_expr.kind {
            hir::TypeExprKind::Structural(structural) => Some(
                structural
                    .fields
                    .iter()
                    .map(|field| StructFieldDef {
                        name: field.name.as_str().to_string(),
                        ty: (*field.ty).clone(),
                    })
                    .collect(),
            ),
            hir::TypeExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(def) = self.struct_defs.get(def_id) {
                        return Some(def.fields.clone());
                    }
                }
                self.emit_error(
                    span,
                    "type arithmetic requires struct operands with known definitions",
                );
                None
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => match type_op.kind {
                TypeBinaryOpKind::Add
                | TypeBinaryOpKind::Intersect
                | TypeBinaryOpKind::Subtract => {
                    let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span)?;
                    let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span)?;
                    Some(match type_op.kind {
                        TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                        TypeBinaryOpKind::Intersect => {
                            self.intersect_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Subtract => {
                            self.subtract_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Union => unreachable!("union handled separately"),
                    })
                }
                TypeBinaryOpKind::Union => None,
            },
            _ => None,
        }
    }

    fn merge_structural_fields(
        &mut self,
        span: Span,
        mut lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        for rhs_field in rhs {
            if let Some(existing) = lhs.iter().find(|field| field.name == rhs_field.name) {
                if !self.type_exprs_equivalent(&existing.ty, &rhs_field.ty) {
                    self.emit_error(
                        span,
                        format!(
                            "conflicting field types for `{}` in structural merge",
                            rhs_field.name
                        ),
                    );
                }
                continue;
            }
            lhs.push(rhs_field);
        }
        lhs
    }

    fn intersect_structural_fields(
        &mut self,
        span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter_map(|field| {
                rhs.iter()
                    .find(|rhs_field| rhs_field.name == field.name)
                    .map(|rhs_field| {
                        if !self.type_exprs_equivalent(&rhs_field.ty, &field.ty) {
                            self.emit_error(
                                span,
                                format!(
                                    "conflicting field types for `{}` in structural intersect",
                                    field.name
                                ),
                            );
                        }
                        field.clone()
                    })
            })
            .collect()
    }

    fn subtract_structural_fields(
        &mut self,
        _span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter(|field| !rhs.iter().any(|rhs_field| rhs_field.name == field.name))
            .collect()
    }

    fn type_exprs_equivalent(&self, lhs: &hir::TypeExpr, rhs: &hir::TypeExpr) -> bool {
        match (&lhs.kind, &rhs.kind) {
            (hir::TypeExprKind::Primitive(a), hir::TypeExprKind::Primitive(b)) => a == b,
            (hir::TypeExprKind::Path(a), hir::TypeExprKind::Path(b)) => {
                if a.segments.len() != b.segments.len() {
                    return false;
                }
                for (a_seg, b_seg) in a.segments.iter().zip(b.segments.iter()) {
                    if a_seg.name != b_seg.name {
                        return false;
                    }
                    match (&a_seg.args, &b_seg.args) {
                        (None, None) => {}
                        (Some(a_args), Some(b_args)) => {
                            if a_args.args.len() != b_args.args.len() {
                                return false;
                            }
                            for (a_arg, b_arg) in a_args.args.iter().zip(b_args.args.iter()) {
                                match (a_arg, b_arg) {
                                    (hir::GenericArg::Type(a_ty), hir::GenericArg::Type(b_ty)) => {
                                        if !self.type_exprs_equivalent(a_ty, b_ty) {
                                            return false;
                                        }
                                    }
                                    (hir::GenericArg::Const(_), hir::GenericArg::Const(_)) => {}
                                    _ => return false,
                                }
                            }
                        }
                        _ => return false,
                    }
                }
                true
            }
            (hir::TypeExprKind::Structural(a), hir::TypeExprKind::Structural(b)) => {
                if a.fields.len() != b.fields.len() {
                    return false;
                }
                for (a_field, b_field) in a.fields.iter().zip(b.fields.iter()) {
                    if a_field.name != b_field.name {
                        return false;
                    }
                    if !self.type_exprs_equivalent(&a_field.ty, &b_field.ty) {
                        return false;
                    }
                }
                true
            }
            (hir::TypeExprKind::TypeBinaryOp(a), hir::TypeExprKind::TypeBinaryOp(b)) => {
                a.kind == b.kind
                    && self.type_exprs_equivalent(&a.lhs, &b.lhs)
                    && self.type_exprs_equivalent(&a.rhs, &b.rhs)
            }
            (hir::TypeExprKind::Tuple(a), hir::TypeExprKind::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
            }
            (hir::TypeExprKind::Array(a_elem, _), hir::TypeExprKind::Array(b_elem, _)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Slice(a_elem), hir::TypeExprKind::Slice(b_elem)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Ptr(a), hir::TypeExprKind::Ptr(b)) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::Ref(a), hir::TypeExprKind::Ref(b)) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::FnPtr(a), hir::TypeExprKind::FnPtr(b)) => {
                if a.inputs.len() != b.inputs.len() {
                    return false;
                }
                if !a
                    .inputs
                    .iter()
                    .zip(b.inputs.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
                {
                    return false;
                }
                self.type_exprs_equivalent(&a.output, &b.output)
            }
            (hir::TypeExprKind::Never, hir::TypeExprKind::Never) => true,
            (hir::TypeExprKind::Infer, hir::TypeExprKind::Infer) => true,
            (hir::TypeExprKind::Error, hir::TypeExprKind::Error) => true,
            _ => false,
        }
    }

    fn lower_union_type_expr(
        &mut self,
        lhs: &hir::TypeExpr,
        rhs: &hir::TypeExpr,
        span: Span,
    ) -> Ty {
        let def_id = self.next_synthetic_def_id();
        let enum_name = format!("__union_{}", def_id);

        let lhs_name = self.union_variant_name(lhs, "Left");
        let mut rhs_name = self.union_variant_name(rhs, "Right");
        if lhs_name == rhs_name {
            rhs_name = format!("{}_rhs", rhs_name);
        }

        let lhs_payload = match lhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(lhs) => None,
            _ => Some(lhs.clone()),
        };
        let rhs_payload = match rhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(rhs) => None,
            _ => Some(rhs.clone()),
        };

        let variants = vec![
            EnumVariantDef {
                def_id: self.next_synthetic_def_id(),
                name: lhs_name,
                discriminant: 0,
                payload: lhs_payload,
            },
            EnumVariantDef {
                def_id: self.next_synthetic_def_id(),
                name: rhs_name,
                discriminant: 1,
                payload: rhs_payload,
            },
        ];

        self.register_synthetic_enum(def_id, enum_name, variants, span);

        match self.enum_layout_for_instance(def_id, &[], span) {
            Some(layout) => self.nominal_enum_ty(&layout),
            None => self.error_ty(),
        }
    }

    fn union_variant_name(&self, ty_expr: &hir::TypeExpr, fallback: &str) -> String {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str().to_string())
                .filter(|name| !name.is_empty())
                .unwrap_or_else(|| fallback.to_string()),
            hir::TypeExprKind::Structural(structural) => {
                let mut matches = self
                    .struct_defs
                    .values()
                    .filter(|def| def.fields.len() == structural.fields.len())
                    .filter(|def| {
                        def.fields.iter().zip(structural.fields.iter()).all(
                            |(def_field, struct_field)| {
                                def_field.name == struct_field.name.as_str()
                                    && self.type_exprs_equivalent(&def_field.ty, &struct_field.ty)
                            },
                        )
                    })
                    .map(|def| def.name.clone())
                    .collect::<Vec<_>>();
                if let Some(name) = matches
                    .iter()
                    .find(|name| !name.starts_with("__structural_"))
                {
                    return name.clone();
                }
                matches.pop().unwrap_or_else(|| fallback.to_string())
            }
            _ => fallback.to_string(),
        }
    }

    fn is_null_type_expr(&self, ty_expr: &hir::TypeExpr) -> bool {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str() == "null")
                .unwrap_or(false),
            _ => false,
        }
    }

    fn register_synthetic_enum(
        &mut self,
        def_id: hir::DefId,
        name: String,
        variants: Vec<EnumVariantDef>,
        span: Span,
    ) {
        if self.enum_defs.contains_key(&def_id) {
            return;
        }

        for variant in &variants {
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(*def_id);
                    }
                }
                None
            });
            self.enum_variants.insert(
                variant.def_id,
                EnumVariantInfo {
                    def_id: variant.def_id,
                    enum_def: def_id,
                    discriminant: variant.discriminant,
                    payload_def,
                },
            );

            let qualified_name = format!("{}::{}", name, variant.name);
            self.enum_variant_names
                .insert(qualified_name.clone(), variant.def_id);
            self.enum_variant_names
                .entry(variant.name.clone())
                .or_insert(variant.def_id);
        }

        self.enum_defs_by_name
            .entry(name.clone())
            .or_insert(def_id);
        self.enum_defs.insert(
            def_id,
            EnumDefinition {
                def_id,
                name,
                generics: Vec::new(),
                variants,
            },
        );

        // JUSTIFY: layout may be uncomputable for forward-referenced types
        // during registration; computed lazily when needed later.
        if self.enum_layout_for_instance(def_id, &[], span).is_none() {
            self.emit_warning(
                span,
                "enum layout computation returned None during registration",
            );
        }
    }

    fn lower_primitive_type(&mut self, primitive: &TypePrimitive, span: Span) -> Ty {
        match primitive {
            TypePrimitive::Bool => Ty { kind: TyKind::Bool },
            TypePrimitive::Char => Ty { kind: TyKind::Char },
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => Ty {
                    kind: TyKind::Int(IntTy::I8),
                },
                TypeInt::I16 => Ty {
                    kind: TyKind::Int(IntTy::I16),
                },
                TypeInt::I32 => Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
                TypeInt::I64 => Ty {
                    kind: TyKind::Int(IntTy::I64),
                },
                TypeInt::I128 => Ty {
                    kind: TyKind::Int(IntTy::I128),
                },
                TypeInt::U8 => Ty {
                    kind: TyKind::Uint(UintTy::U8),
                },
                TypeInt::U16 => Ty {
                    kind: TyKind::Uint(UintTy::U16),
                },
                TypeInt::U32 => Ty {
                    kind: TyKind::Uint(UintTy::U32),
                },
                TypeInt::U64 => Ty {
                    kind: TyKind::Uint(UintTy::U64),
                },
                TypeInt::U128 => Ty {
                    kind: TyKind::Uint(UintTy::U128),
                },
                TypeInt::BigInt => {
                    self.emit_error(span, "big integers are not yet supported in MIR");
                    self.error_ty()
                }
            },
            TypePrimitive::Decimal(decimal) => match decimal {
                DecimalType::F32 => Ty {
                    kind: TyKind::Float(FloatTy::F32),
                },
                DecimalType::F64 => Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
                DecimalType::BigDecimal | DecimalType::Decimal { .. } => {
                    self.emit_warning(span, "lowering arbitrary precision decimal to f64 in MIR");
                    Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    }
                }
            },
            TypePrimitive::String => self.string_slice_ty(),
            TypePrimitive::List => {
                self.emit_warning(
                    span,
                    "treating list primitive as opaque type during MIR lowering",
                );
                self.opaque_ty("list")
            }
        }
    }

    fn resolve_path_def_id(&self, path: &hir::Path) -> Option<hir::DefId> {
        match path.res {
            Some(hir::Res::Def(def_id)) => Some(def_id),
            _ => None,
        }
    }

    fn lower_path_type(&mut self, path: &hir::Path, span: Span) -> Ty {
        if let Some(def_id) = self.resolve_path_def_id(path) {
            if self.struct_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.struct_layout_for_instance(def_id, &args, span) {
                    return layout.ty.clone();
                }
                return self.error_ty();
            }
            if self.enum_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.enum_layout_for_instance(def_id, &args, span) {
                    return self.nominal_enum_ty(&layout);
                }
                return self.error_ty();
            }
            if let Some(sig) = self.function_sigs.get(&def_id) {
                return Ty {
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
                };
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            if name == "Vec" || name == "List" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(elem_ty) = args.first().cloned() {
                    return Ty {
                        kind: TyKind::Slice(Box::new(elem_ty)),
                    };
                }
                self.emit_error(span, "Vec/List requires a single type argument");
                return self.error_ty();
            }
            if name == "HashMap" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if args.len() == 2 {
                    let entry_ty = Ty {
                        kind: TyKind::Tuple(vec![
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ]),
                    };
                    return Ty {
                        kind: TyKind::Slice(Box::new(entry_ty)),
                    };
                }
                self.emit_error(span, "HashMap requires two type arguments");
                return self.error_ty();
            }
        }

        if let Some(res) = &path.res {
            if let hir::Res::Def(def_id) = res {
                if self.struct_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) = self.struct_layout_for_instance(*def_id, &args, span) {
                        return layout.ty.clone();
                    }
                    return self.error_ty();
                }
                if self.enum_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) = self.enum_layout_for_instance(*def_id, &args, span) {
                        return self.nominal_enum_ty(&layout);
                    }
                    return self.error_ty();
                }
                if let Some(sig) = self.function_sigs.get(def_id) {
                    // Treat function types as function pointers when referenced as types
                    return Ty {
                        kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                            binder: mir::ty::Binder {
                                value: mir::ty::FnSig {
                                    inputs: sig
                                        .inputs
                                        .iter()
                                        .map(|ty| Box::new(ty.clone()))
                                        .collect(),
                                    output: Box::new(sig.output.clone()),
                                    c_variadic: false,
                                    unsafety: mir::ty::Unsafety::Normal,
                                    abi: mir::ty::Abi::C { unwind: false },
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    };
                }
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.clone();
            match name.as_str() {
                "i8" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I8),
                    };
                }
                "i16" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I16),
                    };
                }
                "i32" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I32),
                    };
                }
                "i64" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I64),
                    };
                }
                "i128" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I128),
                    };
                }
                "usize" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                }
                "isize" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::Isize),
                    };
                }
                "u8" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U8),
                    };
                }
                "u16" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U16),
                    };
                }
                "u32" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U32),
                    };
                }
                "u64" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    };
                }
                "u128" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U128),
                    };
                }
                "bool" => return Ty { kind: TyKind::Bool },
                "char" => return Ty { kind: TyKind::Char },
                "f16" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F16),
                    };
                }
                "f32" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F32),
                    };
                }
                "f64" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    };
                }
                "f128" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F128),
                    };
                }
                "str" => {
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    };
                }
                "null" => {
                    return self.raw_string_ptr_ty();
                }
                _ => {}
            }
        }

        let display = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.emit_error(span, format!("unresolved type path `{display}`"));
        self.error_ty()
    }

    /// Qualified display name for a definition, sourced from
    /// `hir::Package::def_paths` (the item's `name` field is always bare —
    /// see that table's doc comment). Falls back to the bare name itself
    /// when no path is recorded (e.g. synthetic items). Takes the
    /// `def_paths` table directly (not the whole `&hir::Package`) so
    /// `register_struct`/`register_enum` can be called from a context that
    /// only has `def_paths` on hand (`compute_adt_layout`'s lazy foreign-type
    /// lookup, which runs after the original `hir::Package` is out of scope).
    fn def_path_str(
        def_paths: &HashMap<hir::DefId, hir::DefPath>,
        def_id: hir::DefId,
        bare_name: &str,
    ) -> String {
        def_paths
            .get(&def_id)
            .map(|path| path.to_string())
            .unwrap_or_else(|| bare_name.to_string())
    }

    /// The part of a (possibly `::`-qualified) definition name after its
    /// final `::`, or the whole name if unqualified — see
    /// `struct_defs_by_tail_name`'s doc comment for why this is a safe
    /// pre-filter key for `struct_def_from_ty`'s suffix-match fallback.
    fn name_tail(name: &str) -> &str {
        name.rsplit("::").next().unwrap_or(name)
    }

    fn register_struct(
        &mut self,
        def_paths: &HashMap<hir::DefId, hir::DefPath>,
        def_id: hir::DefId,
        strukt: &hir::Struct,
        _span: Span,
    ) {
        if self.struct_defs.contains_key(&def_id) {
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

        let name = Self::def_path_str(def_paths, def_id, strukt.name.as_str());
        self.struct_defs_by_tail_name
            .entry(Self::name_tail(&name).to_string())
            .or_default()
            .push(def_id);
        self.struct_defs.insert(
            def_id,
            StructDefinition {
                name,
                generics,
                fields,
                field_index,
            },
        );
                }

    fn register_enum(
        &mut self,
        def_paths: &HashMap<hir::DefId, hir::DefPath>,
        def_id: hir::DefId,
        enm: &hir::Enum,
        _span: Span,
    ) {
        if self.enum_defs.contains_key(&def_id) {
            return;
        }

        let generics = enm
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();
        let enum_qualified_name = Self::def_path_str(def_paths, def_id, enm.name.as_str());

        let mut variants = Vec::new();
        let mut next_value: i64 = 0;
        for variant in &enm.variants {
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(*def_id);
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
                def_id: variant.def_id,
                name: variant.name.as_str().to_string(),
                discriminant: value,
                payload: variant.payload.clone(),
            });

            self.enum_variants.insert(
                variant.def_id,
                EnumVariantInfo {
                    def_id: variant.def_id,
                    enum_def: def_id,
                    discriminant: value,
                    payload_def,
                },
            );

            let qualified_name = format!("{}::{}", enum_qualified_name, variant.name.as_str());
            self.enum_variant_names
                .insert(qualified_name.clone(), variant.def_id);
            self.enum_variant_names
                .entry(variant.name.as_str().to_string())
                .or_insert(variant.def_id);
        }

        self.enum_defs_by_name
            .entry(enum_qualified_name.clone())
            .or_insert(def_id);
        self.enum_defs.insert(
            def_id,
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
    fn finalize_adt_definitions(&mut self, program: &hir::Package) {
        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(strukt) => {
                    let mir_fields = strukt
                        .fields
                        .iter()
                        .map(|field| mir::ty::FieldDef {
                            did: hir::DefId::new(item.def_id.package_id, field.hir_id.index),
                            ident: mir::Symbol::from(field.name.as_str()),
                            vis: mir::ty::Visibility::Public,
                            ty: self.lower_type_expr(&field.ty),
                        })
                        .collect();
                    let mir_variant = mir::ty::VariantDef {
                        def_id: item.def_id,
                        ctor_def_id: None,
                        ident: mir::Symbol::from(strukt.name.as_str()),
                        discr: mir::ty::VariantDiscr::Relative(0),
                        fields: mir_fields,
                        ctor_kind: mir::ty::CtorKind::Fn,
                        is_recovered: false,
                    };
                    self.adt_defs.insert(
                        item.def_id,
                        mir::ty::AdtDef {
                            did: item.def_id,
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
                        let _ = self.struct_layout_for_instance(item.def_id, &[], item.span);
                    }
                }
                hir::ItemKind::Enum(enm) if enm.generics.params.is_empty() => {
                    let _ = self.enum_layout_for_instance(item.def_id, &[], item.span);
                    // Register a real, nominal `AdtDef` for this enum too
                    // (structs already get one above) — this is what lets
                    // `take_adt_defs()` export enums to mir_to_lir at all.
                    // Reuses `adt_shell_ty`'s construction (real variant
                    // idents/discriminants, empty per-variant `fields` —
                    // payload types are supplied separately via the
                    // exported `EnumLayout` data, not via `VariantDef
                    // ::fields`, to avoid computing that shape twice).
                    if let Some(Ty {
                        kind: TyKind::Adt(adt, _),
                    }) = self.adt_shell_ty(item.def_id, &[])
                    {
                        self.adt_defs.insert(item.def_id, adt);
                    }
                }
                _ => {}
            }
        }
    }

    fn struct_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<StructLayout> {
        let key = StructLayoutKey {
            def_id,
            args: args.to_vec(),
        };

        if let Some(layout) = self.struct_layouts.get(&key) {
            return Some(layout.clone());
        }

        let Some(struct_def) = self.struct_defs.get(&def_id).cloned() else {
            self.emit_error(span, "struct definition not registered");
            return None;
        };

        if self.struct_layouts_in_progress.contains(&key) {
            self.emit_error(
                span,
                format!(
                    "recursive type `{}` has infinite size",
                    struct_def.name
                ),
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

        self.struct_layouts.insert(key.clone(), layout.clone());
        self.struct_layouts_by_ty.insert(struct_ty, key.clone());
        self.struct_layouts_in_progress.remove(&key);

        let field_tys = layout.field_tys.clone();
        for field_ty in &field_tys {
            if let TyKind::Adt(adt, substs) = &field_ty.kind {
                let is_struct = self.struct_defs.contains_key(&adt.did);
                let is_enum = !is_struct && self.enum_defs.contains_key(&adt.did);
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
                    let _ = self.struct_layout_for_instance(adt.did, &types, span);
                } else {
                    let _ = self.enum_layout_for_instance(adt.did, &types, span);
                }
            }
        }

        Some(layout)
    }

    fn struct_layout_for_ty(&self, ty: &Ty) -> Option<StructLayout> {
        let key = self.struct_layouts_by_ty.get(ty)?;
        self.struct_layouts.get(key).cloned()
    }

    /// Exact-match counterpart to `enum_layout_for_ty`'s fuzzy scan — an
    /// O(1) lookup from a flattened-tuple enum shape back to its concrete
    /// `EnumLayout` (which carries the original generic args in
    /// `EnumLayout.args`). Prefer this everywhere a concrete instantiation
    /// is expected; fall back to the fuzzy scan only when this misses.
    fn enum_layout_for_ty_exact(&self, ty: &Ty) -> Option<&EnumLayout> {
        let key = self.enum_layouts_by_ty.get(ty)?;
        self.enum_layouts.get(key)
    }

    fn enum_payload_types(
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

    fn enum_payload_types_from_ty(&self, ty: &Ty) -> Vec<Ty> {
        match &ty.kind {
            TyKind::Tuple(fields) => fields.iter().map(|f| (**f).clone()).collect(),
            _ if Self::is_unit_ty(ty) => Vec::new(),
            _ => vec![ty.clone()],
        }
    }

    fn enum_variant_payloads_for_args(
        &mut self,
        variant: &EnumVariantInfo,
        args: &[Ty],
        span: Span,
    ) -> Option<Vec<Ty>> {
        // Only `generics` (a short `Vec<String>`) and the one matched
        // variant's own `payload` are actually needed below — clone just
        // those instead of the whole `EnumDefinition` (every variant of
        // the enum, needed or not).
        let enum_def = self.enum_defs.get(&variant.enum_def)?;
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

    fn enum_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<EnumLayout> {
        let key = EnumLayoutKey {
            def_id,
            args: args.to_vec(),
        };

        if let Some(layout) = self.enum_layouts.get(&key) {
            return Some(layout.clone());
        }

        let Some(enum_def) = self.enum_defs.get(&def_id).cloned() else {
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
                            .struct_layout_for_instance(adt.did, &[], span)
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
                    self.opaque_ty_sizes.insert(opaque_name, size);
                }
            }
            variant_payloads.insert(variant.def_id, payload_tys);
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

        self.enum_layouts.insert(key.clone(), layout.clone());
        self.enum_layouts_by_ty.insert(enum_ty.clone(), key.clone());
        self.enum_layouts_in_progress.remove(&key);

        let payload_tys = layout.payload_tys.clone();
        for field_ty in &payload_tys {
            if let TyKind::Adt(adt, substs) = &field_ty.kind {
                let is_struct = self.struct_defs.contains_key(&adt.did);
                let is_enum = !is_struct && self.enum_defs.contains_key(&adt.did);
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
                    let _ = self.struct_layout_for_instance(adt.did, &types, span);
                } else {
                    let _ = self.enum_layout_for_instance(adt.did, &types, span);
                }
            }
        }

        if !has_payload {
            for variant in &enum_def.variants {
                if self.const_values.contains_key(&variant.def_id) {
                    continue;
                }
                let constant = mir::Constant {
                    span,
                    ty: enum_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(variant.discriminant),
                };
                self.const_values.insert(
                    variant.def_id,
                    ConstInfo {
                        ty: enum_ty.clone(),
                        value: constant,
                    },
                );
            }
        }

        Some(layout)
    }

    fn lower_generic_args(&mut self, args: Option<&hir::GenericArgs>, span: Span) -> Vec<Ty> {
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

    fn lower_type_expr_with_substs(
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
                    if self.enum_defs.contains_key(def_id) {
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
                            self.enum_layout_for_instance(*def_id, &args, ty_expr.span)
                        {
                            return self.nominal_enum_ty(&layout);
                        }
                        return self.error_ty();
                    }
                    if self.struct_defs.contains_key(def_id) {
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
                            self.struct_layout_for_instance(*def_id, &args, ty_expr.span)
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
            hir::TypeExprKind::ConstBlock(_) => self
                .typeck_type_exprs
                .get(&ty_expr.hir_id)
                .cloned()
                .unwrap_or_else(|| self.error_ty()),
            hir::TypeExprKind::Type => Ty { kind: TyKind::Type },
            hir::TypeExprKind::Any => Ty { kind: TyKind::Any },
            hir::TypeExprKind::Refinement { base, .. } => {
                self.lower_type_expr_with_substs(base, substs)
            }
        }
    }

    fn raw_string_ptr_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    fn string_slice_ty(&self) -> Ty {
        Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        }
    }

    fn is_string_slice_ref(&self, inner: &hir::TypeExpr) -> bool {
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

    fn eval_int_expr(&mut self, expr: &hir::Expr) -> Option<i64> {
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
                    _ => None,
                }
            }
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(info) = self.const_values.get(def_id) {
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

    fn register_const_value(
        &mut self,
        def_id: hir::DefId,
        konst: &hir::Const,
    ) {
        if self.const_values.contains_key(&def_id) {
            return;
        }
        let ty = self.lower_type_expr(&konst.ty);
        let key = self.const_key(konst.name.as_str(), konst.body.value.span);
        if let Some(constant) = self.resolved_const_values.get(&key).cloned() {
            self.const_values.insert(
                def_id,
                ConstInfo {
                    ty,
                    value: constant,
                },
            );
            return;
        }
        let container_args = self.container_args_from_type_expr(&konst.ty);
        if let Some(constant) = self.lower_const_expr(
            &konst.body.value,
            Some(&ty),
            container_args.as_ref(),
        ) {
            self.const_values.insert(
                def_id,
                ConstInfo {
                    ty,
                    value: constant,
                },
            );
        }
    }

    fn struct_name_from_type(&self, ty: &hir::TypeExpr) -> Option<String> {
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
    fn is_hashmap_intrinsic_method(struct_name: Option<&str>, method_name: &str) -> bool {
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
    fn register_impl_signatures(&mut self, impl_block: &hir::Impl) {
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
    fn register_impl_signature_for_item(
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
        self.method_hir_defs.insert(
            impl_item.def_id,
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
    /// registered locally. Mirrors `try_lazily_register_adt` exactly, one
    /// level down: unlike structs/enums/consts/functions, an individual
    /// method's own `DefId` is never a key in `hir_def_map` (only the
    /// *owning* `Impl` item's own top-level `DefId` is), so this first
    /// needs a one-time, cached reverse index (`method_owner_index`) —
    /// built by scanning every `Impl` item `hir_def_map` knows about
    /// (this package's own, plus every workspace dependency's) exactly
    /// once — before it can do its own point lookup.
    fn try_lazily_register_method(&mut self, def_id: hir::DefId) {
        if self.method_owner_index.is_none() {
            let mut index = HashMap::new();
            for item in self.hir_def_map.values() {
                if let hir::ItemKind::Impl(impl_block) = &item.kind {
                    for (item_idx, impl_item) in impl_block.items.iter().enumerate() {
                        if matches!(impl_item.kind, hir::ImplItemKind::Method(_)) {
                            index.insert(impl_item.def_id, (item.clone(), item_idx));
                        }
                    }
                }
            }
            self.method_owner_index = Some(index);
        }
        let Some((owning_item, item_idx)) = self
            .method_owner_index
            .as_ref()
            .and_then(|index| index.get(&def_id))
            .cloned()
        else {
            return;
        };
        let hir::ItemKind::Impl(impl_block) = owning_item.kind else {
            return;
        };
        let impl_item = impl_block.items[item_idx].clone();
        let struct_name = self.struct_name_from_type(&impl_block.self_ty);
        let method_context = self.make_method_context(
            &impl_block.self_ty,
            &assoc_types_from_impl_items(&impl_block.items),
        );
        self.register_impl_signature_for_item(
            struct_name.as_deref(),
            method_context.as_ref(),
            &impl_block,
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
    fn ensure_method_info(&mut self, def_id: hir::DefId) -> Option<MethodLoweringInfo> {
        if let Some(info) = self.method_lookup_by_def.get(&def_id) {
            return Some(info.clone());
        }
        self.try_lazily_register_method(def_id);
        self.method_lookup_by_def.get(&def_id).cloned()
    }

    /// Generic counterpart to `ensure_method_info` — same "check the
    /// cache; lazily register on a miss; proceed" shape, but reading
    /// `method_defs_by_def` (a generic method's raw, unspecialized HIR;
    /// its real signature needs concrete substs only known at the call
    /// site — see `register_generic_method_definition`) instead of
    /// `method_lookup_by_def`. `try_lazily_register_method` itself
    /// already dispatches to whichever of the two a given method needs.
    fn ensure_generic_method_def(&mut self, def_id: hir::DefId) -> Option<MethodDefinition> {
        if let Some(def) = self.method_defs_by_def.get(&def_id) {
            return Some(def.clone());
        }
        self.try_lazily_register_method(def_id);
        self.method_defs_by_def.get(&def_id).cloned()
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
    fn register_generic_method_definition(
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
            def_id: impl_item.def_id,
            function: function.clone(),
            impl_generics: impl_block.generics.clone(),
            self_ty: impl_block.self_ty.clone(),
            self_def: method_context.and_then(|ctx| ctx.def_id),
            method_name: qualified_name.clone(),
            assoc_types: assoc_types_from_impl_items(&impl_block.items),
        };
        if let Some(self_def) = def.self_def {
            self.method_defs_by_self_and_name
                .entry((self_def, def.function.sig.name.as_str().to_string()))
                .or_insert(impl_item.def_id);
        }
        self.method_defs_by_def
            .entry(impl_item.def_id)
            .or_insert_with(|| def.clone());
        self.method_defs.entry(qualified_name).or_insert(def);
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
    fn method_self_type_spec_suffix(&self, method_context: Option<&MethodContext>) -> Option<String> {
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
    fn register_method_lowering_info(
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
        let struct_def = method_context.and_then(|ctx| ctx.def_id);
        let method_name = function.sig.name.as_str().to_string();
        let impl_item_name = impl_item.name.as_str().to_string();
        let info = MethodLoweringInfo {
            def_id: Some(impl_item.def_id),
            substs: Vec::new(),
            sig,
            fn_name: fn_name.clone(),
            fn_ty,
            struct_def,
        };

        self.method_lookup_by_def
            .insert(impl_item.def_id, info.clone());
        self.method_lookup.insert(fn_name, info.clone());
        self.method_lookup
            .insert(format!("{}::{}", struct_name, method_name), info.clone());
        self.method_lookup
            .insert(format!("{}::{}", struct_name, impl_item_name), info.clone());
        self.method_name_output_consensus
            .entry(method_name.clone())
            .and_modify(|existing| {
                if existing.as_ref() != Some(&info.sig.output) {
                    *existing = None;
                }
            })
            .or_insert_with(|| Some(info.sig.output.clone()));
        self.struct_methods
            .entry(struct_name.to_string())
            .or_default()
            .insert(method_name, info);
    }

    fn lower_impl(
        &mut self,
        item: &hir::Item,
        impl_block: &hir::Impl,
        output: Option<&mut mir::Program>,
    ) -> Result<()> {
        let mut output = output;
        let mut emit_function =
            |this: &mut Self, mir_item: mir::Item, body_id: mir::BodyId, body: mir::Body| {
                if let Some(program_ref) = output.as_mut() {
                    let program: &mut mir::Program = &mut **program_ref;
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
                        impl_item.def_id,
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

    fn lower_method(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        parent_span: Span,
        method_context: Option<&MethodContext>,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body, mir::FunctionSig)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

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
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body, sig))
    }

    fn make_method_context(
        &mut self,
        self_ty: &hir::TypeExpr,
        assoc_types: &HashMap<String, hir::TypeExpr>,
    ) -> Option<MethodContext> {
        if let hir::TypeExprKind::Path(path) = &self_ty.kind {
            let def_id = match &path.res {
                Some(hir::Res::Def(def_id)) => Some(*def_id),
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

    fn struct_field(
        &mut self,
        def_id: hir::DefId,
        struct_ty: &Ty,
        name: &str,
        span: Span,
    ) -> Option<(usize, StructFieldInfo)> {
        let def = self.struct_defs.get(&def_id)?;
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

    fn function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
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

    fn c_function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
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

    fn make_local_decl(&mut self, ty: &Ty, span: Span) -> mir::LocalDecl {
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

    fn lower_const_expr(
        &mut self,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
        container_args: Option<&ConstContainerArgs>,
    ) -> Option<mir::Constant> {
        let constant_ty = expected_ty
            .cloned()
            .or_else(|| self.typeck_exprs.get(&expr.hir_id).cloned());
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(mir::Constant {
                span: expr.span,
                ty: constant_ty.clone()?,
                user_ty: None,
                literal: self.lower_literal(lit),
            }),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_expr(inner, expected_ty, container_args);
                }
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Unit),
                })
            }
            hir::ExprKind::Array(elements) => {
                if let Some(container_args) = container_args {
                    return self.lower_container_const(
                        expr.span,
                        elements,
                        container_args,
                    );
                }
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(
                        element,
                        Some(elem_ty.as_ref()),
                    )?);
                }
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered)),
                })
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                if let Some(container_args) = container_args {
                    return self.lower_container_repeat_const(
                        expr.span,
                        elem,
                        len,
                        container_args,
                    );
                }
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered)),
                })
            }
            hir::ExprKind::Struct(_, _) => {
                let value = self.lower_const_value(expr, expected_ty)?;
                let ty = match constant_ty.clone()? {
                    Ty {
                        kind: TyKind::Adt(adt, args),
                    } => {
                        let type_args = args
                            .iter()
                            .filter_map(|arg| match arg {
                                mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                                mir::ty::GenericArg::Lifetime(_)
                                | mir::ty::GenericArg::Const(_) => None,
                            })
                            .collect::<Vec<_>>();
                        self.struct_layout_for_instance(adt.did, &type_args, expr.span)
                            .map(|layout| layout.ty)
                            .unwrap_or(Ty {
                                kind: TyKind::Adt(adt, args),
                            })
                    }
                    ty => ty,
                };
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(value),
                })
            }
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };
                if let Some(const_info) = self.const_values.get(def_id) {
                    return Some(const_info.typed_value());
                }
                let const_item = self.hir_def_map.get(def_id).and_then(|item| {
                    match &item.kind {
                        hir::ItemKind::Const(konst) => Some(konst.clone()),
                        _ => None,
                    }
                });
                if let Some(konst) = const_item {
                    self.register_const_value(*def_id, &konst);
                    if let Some(const_info) = self.const_values.get(def_id) {
                        return Some(const_info.typed_value());
                    }
                }
                let item = self.hir_def_map.get(def_id)?;
                let hir::ItemKind::Function(_function) = &item.kind else {
                    return None;
                };
                let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) = expected_ty.map(|ty| &ty.kind)?
                else {
                    return None;
                };
                let fn_ty = expected_ty.cloned()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: fn_ty,
                    user_ty: None,
                    literal: mir::ConstantKind::FnDef(*def_id, Vec::new()),
                })
            }
            hir::ExprKind::Slice(slice) => {
                let value = self.lower_const_string_slice(slice)?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: constant_ty.clone()?,
                    user_ty: None,
                    literal: mir::ConstantKind::Str(value),
                })
            }
            hir::ExprKind::Index(base, index) => self
                .lower_const_expr(base, None, container_args)
                .and_then(|constant| self.const_index_value(expr.span, &constant, index))
                .map(|(constant, _)| constant),
            hir::ExprKind::FieldAccess(base, field) => {
                self.lower_const_field_access(base, field.as_str(), expr.span)
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let branch = match self.lower_const_value(cond, None)? {
                    mir::ConstValue::Bool(value) => {
                        if value {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::Int(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::UInt(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    _ => return None,
                };
                self.lower_const_expr(branch, expected_ty, container_args)
            }
            hir::ExprKind::MethodCall(receiver, method_name, args) => {
                let ty = constant_ty.clone()?;
                let value = self.lower_const_method_value(
                    receiver,
                    method_name.as_str(),
                    args,
                    expr.span,
                )?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: self.const_value_to_constant(expr.span, &value, &ty).literal,
                })
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let kind = if let (Some(left), Some(right)) = (
                    self.lower_const_expr(lhs, expected_ty, container_args),
                    self.lower_const_expr(rhs, expected_ty, container_args),
                ) {
                    Self::lower_binary_op_const(op, &left, &right)
                } else {
                    let left = self.lower_const_value(lhs, expected_ty)?;
                    let right = self.lower_const_value(rhs, expected_ty)?;
                    Self::lower_binary_op_const_values(op, &left, &right)
                }?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: constant_ty.clone()?,
                    user_ty: None,
                    literal: kind,
                })
            }
            _ => None,
        }
    }

    fn lower_binary_op_const(
        op: &hir::BinOp,
        left: &mir::Constant,
        right: &mir::Constant,
    ) -> Option<mir::ConstantKind> {
        match (&left.literal, &right.literal) {
            (mir::ConstantKind::Int(l), mir::ConstantKind::Int(r)) => match op {
                hir::BinOp::Add => Some(mir::ConstantKind::Int(l + r)),
                hir::BinOp::Sub => Some(mir::ConstantKind::Int(l - r)),
                hir::BinOp::Mul => Some(mir::ConstantKind::Int(l * r)),
                hir::BinOp::Div => Some(mir::ConstantKind::Int(l / r)),
                hir::BinOp::Gt => Some(mir::ConstantKind::Bool(l > r)),
                hir::BinOp::Lt => Some(mir::ConstantKind::Bool(l < r)),
                hir::BinOp::Ge => Some(mir::ConstantKind::Bool(l >= r)),
                hir::BinOp::Le => Some(mir::ConstantKind::Bool(l <= r)),
                hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
                hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
                _ => None,
            },
            (mir::ConstantKind::UInt(l), mir::ConstantKind::UInt(r)) => match op {
                hir::BinOp::Add => Some(mir::ConstantKind::UInt(l + r)),
                hir::BinOp::Sub => Some(mir::ConstantKind::UInt(l - r)),
                hir::BinOp::Mul => Some(mir::ConstantKind::UInt(l * r)),
                hir::BinOp::Div => Some(mir::ConstantKind::UInt(l / r)),
                hir::BinOp::Gt => Some(mir::ConstantKind::Bool(l > r)),
                hir::BinOp::Lt => Some(mir::ConstantKind::Bool(l < r)),
                _ => None,
            },
            (mir::ConstantKind::Str(l), mir::ConstantKind::Str(r)) => match op {
                hir::BinOp::Add => Some(mir::ConstantKind::Str(format!("{l}{r}"))),
                hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
                hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
                _ => None,
            },
            _ => None,
        }
    }

    fn lower_binary_op_const_values(
        op: &hir::BinOp,
        left: &mir::ConstValue,
        right: &mir::ConstValue,
    ) -> Option<mir::ConstantKind> {
        match (left, right) {
            (mir::ConstValue::Str(l), mir::ConstValue::Str(r)) => match op {
                hir::BinOp::Add => Some(mir::ConstantKind::Str(format!("{l}{r}"))),
                hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
                hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
                _ => None,
            },
            _ => None,
        }
    }

    fn lower_const_value(
        &mut self,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::ConstValue> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(self.const_value_from_lit(lit)),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_value(inner, expected_ty);
                }
                Some(mir::ConstValue::Unit)
            }
            hir::ExprKind::Array(elements) => {
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(
                        element,
                        Some(elem_ty.as_ref()),
                    )?);
                }
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::Struct(path, fields) => {
                let def_id = self.resolve_path_def_id(path)?;
                let struct_def = self.struct_defs.get(&def_id)?.clone();
                let mut args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                if args.is_empty() && !struct_def.generics.is_empty() {
                    // No explicit turbofish — read `fp-typing`'s own
                    // already-resolved generic args for this literal
                    // (`typeck_exprs`) rather than re-deriving them here.
                    // Top-level `const` items aren't themselves generic, so
                    // there's no live specialization context to compose in
                    // (empty substs map); if the cache has no entry, fall
                    // through to today's behavior unchanged, letting
                    // `struct_layout_for_instance`'s own arity check
                    // surface the real diagnostic.
                    if let Some(cached) =
                        self.adt_ty_args_from_typeck_cache(expr.hir_id, def_id, &HashMap::new())
                    {
                        args = cached;
                    }
                }
                let layout = self.struct_layout_for_instance(def_id, &args, expr.span);
                let layout = match layout {
                    Some(l) => l,
                    None => return None,
                };
                let mut field_map: HashMap<String, &hir::Expr> = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.as_str().to_string(), &field.expr);
                }
                let mut lowered = Vec::with_capacity(struct_def.fields.len());
                for (idx, field_def) in struct_def.fields.iter().enumerate() {
                    let Some(field_expr) = field_map.get(&field_def.name) else {
                        self.emit_error(
                            expr.span,
                            format!("missing field `{}` in const struct literal", field_def.name),
                        );
                        return None;
                    };
                    let field_ty = layout.field_tys.get(idx)?;
                    lowered.push(self.lower_const_value(field_expr, Some(field_ty))?);
                }
                Some(mir::ConstValue::Struct(lowered))
            }
            hir::ExprKind::Slice(slice) => Some(mir::ConstValue::Str(
                self.lower_const_string_slice(slice)?,
            )),
            hir::ExprKind::Index(base, index) => self
                .lower_const_expr(base, None, None)
                .and_then(|constant| self.const_index_value(expr.span, &constant, index))
                .and_then(|(constant, _)| self.const_value_from_constant(&constant)),
            hir::ExprKind::FieldAccess(base, field) => self
                .lower_const_field_access(base, field.as_str(), expr.span)
                .and_then(|constant| self.const_value_from_constant(&constant)),
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let branch = match self.lower_const_value(cond, None)? {
                    mir::ConstValue::Bool(value) => {
                        if value {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::Int(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::UInt(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    _ => return None,
                };
                self.lower_const_value(branch, expected_ty)
            }
            hir::ExprKind::MethodCall(receiver, method_name, args) => self
                .lower_const_method_value(receiver, method_name.as_str(), args, expr.span),
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };

                // Check const_values first — function-local consts are
                // registered here by lower_const but may not be in
                // program.def_map.
                if let Some(const_info) = self.const_values.get(&def_id) {
                    return match &const_info.value.literal {
                        mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
                        mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
                        mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
                        mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
                        mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
                        mir::ConstantKind::Val(v) => Some(v.clone()),
                        _ => None,
                    };
                }

                let item = self.hir_def_map.get(def_id)?;
                match &item.kind {
                    hir::ItemKind::Function(function) => {
                        let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) =
                            expected_ty.map(|ty| &ty.kind)?
                        else {
                            return None;
                        };
                        Some(mir::ConstValue::Fn(mir::Symbol::new(
                            function.sig.name.as_str(),
                        )))
                    }
                    hir::ItemKind::Const(_) => {
                        let const_info = self.const_values.get(&def_id)?;
                        match &const_info.value.literal {
                            mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
                            mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
                            mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
                            mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
                            mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
                            mir::ConstantKind::Val(v) => Some(v.clone()),
                            _ => None,
                        }
                    }
                    _ => return None,
                }
            }
            _ => None,
        }
    }

    fn lower_const_string_slice(
        &mut self,
        slice: &hir::SliceExpr,
    ) -> Option<String> {
        let base = self.const_string_from_expr(slice.base.as_ref())?;
        let start = match slice.start.as_ref() {
            Some(start) => self.const_index_u64(start.as_ref())? as usize,
            None => 0,
        };
        let mut end = match slice.end.as_ref() {
            Some(end) => self.const_index_u64(end.as_ref())? as usize,
            None => base.len(),
        };
        if slice.inclusive {
            end = end.checked_add(1)?;
        }
        if start > end || end > base.len() {
            return None;
        }
        base.get(start..end).map(str::to_string)
    }

    fn lower_const_method_value(
        &mut self,
        receiver: &hir::Expr,
        method_name: &str,
        args: &[hir::CallArg],
        _span: Span,
    ) -> Option<mir::ConstValue> {
        let matches_name =
            |name: &str| method_name == name || method_name.ends_with(&format!("::{name}"));
        let receiver_value = self.lower_const_value(receiver, None)?;

        if matches_name("len") && args.is_empty() {
            return match &receiver_value {
                mir::ConstValue::Str(text) => Some(mir::ConstValue::UInt(text.len() as u64)),
                mir::ConstValue::List { elements, .. } => {
                    Some(mir::ConstValue::UInt(elements.len() as u64))
                }
                mir::ConstValue::Array(elements) => {
                    Some(mir::ConstValue::UInt(elements.len() as u64))
                }
                mir::ConstValue::Tuple(fields) => Some(mir::ConstValue::UInt(fields.len() as u64)),
                _ => None,
            };
        }

        let receiver_text = match &receiver_value {
            mir::ConstValue::Str(text) => Some(text.clone()),
            _ => None,
        };
        let needle = match args.first() {
            Some(arg) => self.const_string_from_expr(&arg.value)?,
            None => return None,
        };
        if matches_name("starts_with") && args.len() == 1 {
            let receiver_text = receiver_text?;
            return Some(mir::ConstValue::Bool(receiver_text.starts_with(&needle)));
        }
        if matches_name("ends_with") && args.len() == 1 {
            let receiver_text = receiver_text?;
            return Some(mir::ConstValue::Bool(receiver_text.ends_with(&needle)));
        }
        if matches_name("contains") && args.len() == 1 {
            if let Some(receiver_text) = receiver_text {
                return Some(mir::ConstValue::Bool(receiver_text.contains(&needle)));
            }
            if let Some(items) = Self::const_string_items(&receiver_value) {
                return Some(mir::ConstValue::Bool(
                    items.iter().any(|item| item == &needle),
                ));
            }
        }
        None
    }

    fn lower_const_field_access(
        &mut self,
        base: &hir::Expr,
        field: &str,
        span: Span,
    ) -> Option<mir::Constant> {
        if let Some(constant) = self.lower_const_expr(base, None, None) {
            if let Some(field_value) =
                self.lower_const_struct_field_from_constant(&constant, field, span)
            {
                return Some(field_value);
            }
        }

        let hir::ExprKind::IntrinsicCall(call) = &base.kind else {
            return None;
        };
        if call.kind.intrinsic_kind() != Some(IntrinsicKind::TypeOf) || call.callargs.len() != 1 {
            return None;
        }
        let type_arg = &call.callargs[0].value;

        let hir::ExprKind::Path(path) = &type_arg.kind else {
            return None;
        };
        let struct_def_id = if let Some(hir::Res::Def(def_id)) = &path.res {
            *def_id
        } else {
            let name = path.segments.last()?.name.as_str();
            let mut matches = self
                .struct_defs
                .iter()
                .filter_map(|(def_id, info)| (info.name == name).then_some(*def_id))
                .collect::<Vec<_>>();
            if matches.len() != 1 {
                return None;
            }
            matches.pop()?
        };
        let struct_info = self.struct_defs.get(&struct_def_id)?;
        match field {
            "fields" => {
                let names = struct_info
                    .fields
                    .iter()
                    .map(|field| field.name.clone())
                    .collect::<Vec<_>>();
                Some(self.string_list_constant(span, names))
            }
            "methods" => {
                let method_names = self
                    .struct_methods
                    .get(&struct_info.name)
                    .map(|methods| methods.keys().cloned().collect::<Vec<_>>())
                    .unwrap_or_default();
                Some(self.string_list_constant(span, method_names))
            }
            _ => None,
        }
    }

    fn lower_const_struct_field_from_constant(
        &mut self,
        constant: &mir::Constant,
        field: &str,
        span: Span,
    ) -> Option<mir::Constant> {
        let (values, ty) = match &constant.literal {
            mir::ConstantKind::Val(mir::ConstValue::Struct(values)) => (values, &constant.ty),
            _ => return None,
        };

        match &ty.kind {
            // `adt_def.variants` is deliberately empty for several real
            // construction paths (`adt_shell_ty`, the general Adt case in
            // `lower_hir_ty`) — those only ever needed to convey type
            // *identity*, not full field layout. `struct_field` is the
            // authoritative, substitution-aware lookup (via `struct_defs`/
            // `struct_layout_for_ty`/`struct_layout_for_instance`) already
            // used elsewhere in this file for exactly this; never derive
            // field info from `adt_def.variants` directly.
            TyKind::Adt(adt_def, _) => {
                let (field_index, field_info) = self.struct_field(adt_def.did, ty, field, span)?;
                let field_value = values.get(field_index)?;
                Some(self.const_value_to_constant(span, field_value, &field_info.ty))
            }
            TyKind::Tuple(field_tys) => {
                if let Some(key) = self.struct_layouts_by_ty.get(ty) {
                    let field_index = self
                        .struct_defs
                        .get(&key.def_id)?
                        .field_index
                        .get(field)
                        .copied()?;
                    let layout = self.struct_layouts.get(key)?;
                    let field_ty = layout.field_tys.get(field_index)?;
                    let field_value = values.get(field_index)?;
                    return Some(self.const_value_to_constant(span, field_value, field_ty));
                }
                let field_index = field.parse::<usize>().ok()?;
                let field_ty = field_tys.get(field_index)?.as_ref();
                let field_value = values.get(field_index)?;
                Some(self.const_value_to_constant(span, field_value, field_ty))
            }
            _ => None,
        }
    }

    fn string_list_constant(&self, span: Span, items: Vec<String>) -> mir::Constant {
        let elem_ty = self.string_slice_ty();
        let ty = Ty {
            kind: TyKind::Slice(Box::new(elem_ty.clone())),
        };
        let elements = items.into_iter().map(mir::ConstValue::Str).collect();
        mir::Constant {
            span,
            ty: ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }),
        }
    }

    fn const_value_from_constant(&self, constant: &mir::Constant) -> Option<mir::ConstValue> {
        match &constant.literal {
            mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
            mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
            mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
            mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
            mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
            mir::ConstantKind::Val(v) => Some(v.clone()),
            _ => None,
        }
    }

    fn const_string_items(value: &mir::ConstValue) -> Option<Vec<String>> {
        let items = match value {
            mir::ConstValue::List { elements, .. } | mir::ConstValue::Array(elements) => elements,
            mir::ConstValue::Tuple(fields) => fields,
            _ => return None,
        };
        let mut names = Vec::with_capacity(items.len());
        for item in items {
            let mir::ConstValue::Str(name) = item else {
                return None;
            };
            names.push(name.clone());
        }
        Some(names)
    }

    fn const_string_from_expr(
        &mut self,
        expr: &hir::Expr,
    ) -> Option<String> {
        match self.lower_const_value(expr, None)? {
            mir::ConstValue::Str(value) => Some(value),
            _ => None,
        }
    }

    fn const_index_u64(&mut self, expr: &hir::Expr) -> Option<u64> {
        match self.lower_const_value(expr, None)? {
            mir::ConstValue::UInt(value) => Some(value),
            mir::ConstValue::Int(value) if value >= 0 => Some(value as u64),
            _ => None,
        }
    }

    fn const_value_from_lit(&self, lit: &hir::Lit) -> mir::ConstValue {
        match lit {
            hir::Lit::Bool(value) => mir::ConstValue::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstValue::Int(*value),
            hir::Lit::Float(value) => mir::ConstValue::Float(*value),
            hir::Lit::Str(value) => mir::ConstValue::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstValue::Int(*value as i64),
            hir::Lit::Null => mir::ConstValue::Null,
            // MIR constants have no raw-byte-buffer representation yet
            // (only UTF-8 `Str`) — every current use of `b"..."`/`c"..."`
            // in this codebase is plain ASCII, so this is lossy only for
            // non-UTF-8 byte content, which nothing currently needs.
            hir::Lit::Bytes(bytes) | hir::Lit::CStr(bytes) => {
                mir::ConstValue::Str(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    fn lower_container_const(
        &mut self,
        span: Span,
        elements: &[hir::Expr],
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(element, Some(elem_ty))?);
                }
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::List {
                        elements: lowered,
                        elem_ty: elem_ty.clone(),
                    }),
                })
            }
            ConstContainerArgs::Map { key_ty, value_ty } => {
                let mut entries = Vec::with_capacity(elements.len());
                for element in elements {
                    let (key_expr, value_expr) = match &element.kind {
                        hir::ExprKind::Array(pair) if pair.len() == 2 => (&pair[0], &pair[1]),
                        _ => {
                            self.emit_error(
                                span,
                                "HashMap literal expects entries as [key, value]",
                            );
                            return None;
                        }
                    };
                    let key = self.lower_const_value(key_expr, Some(key_ty))?;
                    let value = self.lower_const_value(value_expr, Some(value_ty))?;
                    entries.push((key, value));
                }
                let entry_ty = Ty {
                    kind: TyKind::Tuple(vec![Box::new(key_ty.clone()), Box::new(value_ty.clone())]),
                };
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(entry_ty)),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Map {
                        entries,
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                    }),
                })
            }
        }
    }

    fn lower_container_repeat_const(
        &mut self,
        span: Span,
        elem: &hir::Expr,
        len: &hir::Expr,
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let repeat_len = self.eval_type_length(len)?;
                let value = self.lower_const_value(elem, Some(elem_ty))?;
                let mut elements = Vec::with_capacity(repeat_len as usize);
                elements.resize(repeat_len as usize, value);
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::List {
                        elements,
                        elem_ty: elem_ty.clone(),
                    }),
                })
            }
            ConstContainerArgs::Map { .. } => None,
        }
    }

    fn container_args_from_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
    ) -> Option<ConstContainerArgs> {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => {
                let tail = path.segments.last()?;
                let args = tail.args.as_ref()?;
                match tail.name.as_str() {
                    "Vec" if args.args.len() == 1 => {
                        let hir::GenericArg::Type(elem) = &args.args[0] else {
                            return None;
                        };
                        let elem_ty = self.lower_type_expr(elem.as_ref());
                        Some(ConstContainerArgs::List { elem_ty })
                    }
                    "HashMap" if args.args.len() == 2 => {
                        let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                            (&args.args[0], &args.args[1])
                        else {
                            return None;
                        };
                        let key_ty = self.lower_type_expr(key.as_ref());
                        let value_ty = self.lower_type_expr(value.as_ref());
                        Some(ConstContainerArgs::Map { key_ty, value_ty })
                    }
                    _ => None,
                }
            }
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr(elem.as_ref());
                Some(ConstContainerArgs::List { elem_ty })
            }
            hir::TypeExprKind::Structural(structural) => {
                let mut entries_ty: Option<&hir::TypeExpr> = None;
                for field in &structural.fields {
                    if field.name.as_str() == "entries" {
                        entries_ty = Some(field.ty.as_ref());
                        break;
                    }
                }
                let Some(entries_ty) = entries_ty else {
                    return None;
                };
                let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
                match &entries_ty.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments.last()?;
                        if tail.name.as_str() == "Vec" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 1 {
                                if let hir::GenericArg::Type(inner) = &args.args[0] {
                                    entry_ty_expr = Some(inner.as_ref());
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Slice(inner) => {
                        entry_ty_expr = Some(inner.as_ref());
                    }
                    _ => {}
                }

                let Some(mut entry_ty_expr) = entry_ty_expr else {
                    return None;
                };
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    let tail = path.segments.last()?;
                    if tail.name.as_str() == "Expr" {
                        let args = tail.args.as_ref()?;
                        if args.args.len() == 1 {
                            if let hir::GenericArg::Type(inner) = &args.args[0] {
                                entry_ty_expr = inner.as_ref();
                            }
                        }
                    }
                }

                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments.last()?;
                        if tail.name.as_str() == "HashMapEntry" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 2 {
                                if let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                                    (&args.args[0], &args.args[1])
                                {
                                    let key_ty = self.lower_type_expr(key.as_ref());
                                    let value_ty = self.lower_type_expr(value.as_ref());
                                    return Some(ConstContainerArgs::Map { key_ty, value_ty });
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            let key_ty = self.lower_type_expr(fields[0].as_ref());
                            let value_ty = self.lower_type_expr(fields[1].as_ref());
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        let mut key_ty_expr = None;
                        let mut value_ty_expr = None;
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                        if let (Some(key_ty_expr), Some(value_ty_expr)) =
                            (key_ty_expr, value_ty_expr)
                        {
                            let key_ty = self.lower_type_expr(key_ty_expr);
                            let value_ty = self.lower_type_expr(value_ty_expr);
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    _ => {}
                }

                None
            }
            hir::TypeExprKind::Ref(inner) => self.container_args_from_type_expr(inner.as_ref()),
            _ => None,
        }
    }

    fn const_len_from_constant(&self, constant: &mir::Constant) -> Option<u64> {
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

    fn const_index_value(
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

    fn const_value_matches(&self, lhs: &mir::ConstValue, rhs: &mir::ConstValue) -> bool {
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

    fn const_value_to_constant(
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

    fn lower_literal(&self, lit: &hir::Lit) -> mir::ConstantKind {
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

    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        self.has_errors = true;
        let diagnostic = Diagnostic::error(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn emit_warning(&mut self, span: Span, message: impl Into<String>) {
        let diagnostic = Diagnostic::warning(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn unit_ty() -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    fn type_ty() -> Ty {
        Ty { kind: TyKind::Type }
    }

    fn is_unit_ty(ty: &Ty) -> bool {
        matches!(&ty.kind, TyKind::Tuple(elements) if elements.is_empty())
    }

    fn pointer_sized_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Uint(UintTy::U8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    fn sanitize_placeholder_ty(&self, ty: &Ty) -> Ty {
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

    fn sanitize_function_sig(&self, sig: &mir::FunctionSig) -> mir::FunctionSig {
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

    fn update_placeholder_signature(
        &mut self,
        name: &str,
        existing_sig: &mir::FunctionSig,
        arg_types: &[Ty],
        destination_ty: Option<&Ty>,
    ) -> mir::FunctionSig {
        let mut inputs: Vec<Ty> = if arg_types.is_empty() {
            existing_sig
                .inputs
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect()
        } else {
            arg_types
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect()
        };

        let mut output = if let Some(expected) = destination_ty {
            self.sanitize_placeholder_ty(expected)
        } else if Self::is_unit_ty(&existing_sig.output) {
            existing_sig.output.clone()
        } else {
            self.sanitize_placeholder_ty(&existing_sig.output)
        };

        if inputs.is_empty() && arg_types.is_empty() && !existing_sig.inputs.is_empty() {
            inputs = existing_sig
                .inputs
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect();
        }

        if destination_ty.is_none() && Self::is_unit_ty(&existing_sig.output) {
            output = existing_sig.output.clone();
        }

        let new_sig = mir::FunctionSig { inputs, output };

        let needs_update = self
            .runtime_functions
            .get(name)
            .map(|current| current != &new_sig)
            .unwrap_or(true);

        if needs_update {
            self.runtime_functions
                .insert(name.to_string(), new_sig.clone());
            self.synthetic_runtime_functions.insert(name.to_string());
            new_sig
        } else {
            existing_sig.clone()
        }
    }

    fn opaque_ty(&mut self, name: &str) -> Ty {
        if let Some(existing) = self.opaque_types.get(name) {
            return existing.clone();
        }
        let adt_def_id = self.next_synthetic_def_id;
        self.next_synthetic_def_id = self.next_synthetic_def_id.saturating_add(1);
        let variant_def_id = self.next_synthetic_def_id;
        self.next_synthetic_def_id = self.next_synthetic_def_id.saturating_add(1);

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
        self.opaque_types.insert(name.to_string(), ty.clone());
        ty
    }

    /// Builds a lazy `TyKind::Adt` reference to a *real* (already
    /// registered) struct/enum def, without computing its layout. Unlike
    /// `opaque_ty`, this carries the type's actual `def_id`, so a later
    /// `struct_layout_for_instance`/`enum_layout_for_instance(adt.did, ..)`
    /// call can still resolve it on demand — it's an identity reference,
    /// not a dead end.
    fn adt_shell_ty(&mut self, def_id: hir::DefId, args: &[Ty]) -> Option<Ty> {
        let generic_args: Vec<mir::ty::GenericArg> = args
            .iter()
            .cloned()
            .map(mir::ty::GenericArg::Type)
            .collect();
        if let Some(enum_def) = self.enum_defs.get(&def_id).cloned() {
            let variants = enum_def
                .variants
                .iter()
                .enumerate()
                .map(|(idx, variant)| VariantDef {
                    def_id: variant.def_id,
                    // Match `fp-typing::hir_typeck`'s own nominal-enum
                    // construction (`check_type_expr`/`path_ty`) exactly —
                    // `ctor_def_id`/`ctor_kind` here, not just `did`/
                    // `variants[].ident` — so a function signature's
                    // typeck-derived `Ty` and this on-demand shell compare
                    // structurally equal for the same enum instantiation.
                    ctor_def_id: Some(variant.def_id),
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
        if self.struct_defs.contains_key(&def_id) {
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

    /// `MirLowering`-level byte-size computation, used while computing an
    /// enum's own layout (`enum_layout_for_instance`, which runs before any
    /// `BodyBuilder`/`type_substs` context exists — `BodyBuilder::
    /// compute_ty_size` isn't reachable here). Mirrors that function's
    /// logic minus the generic-`Param`-via-`type_substs` fallback (payload
    /// types reaching this point are already substituted).
    fn size_of_ty(&mut self, ty: &Ty, span: Span) -> Option<u64> {
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
                if let Some(size) = self
                    .display_type_name(ty)
                    .and_then(|name| self.opaque_ty_sizes.get(&name).copied())
                {
                    return Some(size);
                }
                let args: Vec<Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(inner) => Some(inner.clone()),
                        _ => None,
                    })
                    .collect();
                if self.struct_defs.contains_key(&adt.did) {
                    let layout = self
                        .struct_layout_for_ty(ty)
                        .or_else(|| self.struct_layout_for_instance(adt.did, &args, span))?;
                    let mut total = 0u64;
                    for field in &layout.field_tys {
                        total = total.saturating_add(self.size_of_ty(field, span)?);
                    }
                    return Some(total);
                }
                if self.enum_defs.contains_key(&adt.did) {
                    let layout = self.enum_layout_for_instance(adt.did, &args, span)?;
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
    fn nominal_enum_ty(&mut self, layout: &EnumLayout) -> Ty {
        self.adt_shell_ty(layout.def_id, &layout.args)
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
    fn nominalize_struct_ty(&mut self, ty: Ty) -> Ty {
        let Some(key) = self.struct_layouts_by_ty.get(&ty).cloned() else {
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
    fn lower_generic_type_arg(
        &mut self,
        ty_expr: &hir::TypeExpr,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
            if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                if self.struct_defs.contains_key(def_id) || self.enum_defs.contains_key(def_id) {
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
                    if let Some(shell) = self.adt_shell_ty(*def_id, &nested_args) {
                        return shell;
                    }
                }
            }
        }
        self.lower_type_expr_with_substs(ty_expr, substs)
    }

    fn display_type_name(&self, ty: &Ty) -> Option<String> {
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
                .or_else(|| self.struct_defs.get(&adt.did).map(|def| def.name.clone())),
            TyKind::Ref(_, inner, _) => self.display_type_name(inner),
            TyKind::RawPtr(type_and_mut) => self.display_type_name(&type_and_mut.ty),
            _ => None,
        }
    }

    fn is_opaque_ty(&self, ty: &Ty) -> bool {
        self.display_type_name(ty)
            .map(|name| self.opaque_types.contains_key(&name))
            .unwrap_or(false)
    }

    fn has_unresolved_ty(&self, ty: &Ty) -> bool {
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
    fn substitute_ty(&self, ty: &Ty, substs: &HashMap<String, Ty>) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => substs.get(param.name.as_str()).cloned().unwrap_or_else(|| ty.clone()),
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
    /// `fp-typing`'s own already-resolved type-check result (`typeck_exprs`)
    /// instead of re-deriving them independently in `fp-backend` — HIR→MIR
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
    fn adt_ty_args_from_typeck_cache(
        &self,
        hir_id: hir::HirId,
        def_id: hir::DefId,
        type_substs: &HashMap<String, Ty>,
    ) -> Option<Vec<Ty>> {
        let cached = self.typeck_exprs.get(&hir_id)?;
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

    fn ensure_runtime_stub(&mut self, name: &str, sig: &mir::FunctionSig) {
        let sanitized = self.sanitize_function_sig(sig);
        self.runtime_functions.insert(name.to_string(), sanitized);
        self.synthetic_runtime_functions.insert(name.to_string());
    }

    fn placeholder_function_sig(&mut self, name: &str) -> mir::FunctionSig {
        let entry = self
            .runtime_functions
            .entry(name.to_string())
            .or_insert_with(|| mir::FunctionSig {
                inputs: Vec::new(),
                output: Self::unit_ty(),
            })
            .clone();
        self.synthetic_runtime_functions.insert(name.to_string());
        entry
    }

    fn error_ty(&mut self) -> Ty {
        let error = ErrorGuaranteed {
            index: self.next_error_id,
        };
        self.next_error_id += 1;
        Ty {
            kind: TyKind::Error(error),
        }
    }

    fn error_constant(&mut self, span: Span) -> mir::Constant {
        self.emit_error(span, "unable to lower expression to a constant");
        mir::Constant {
            span,
            ty: self.error_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Bool(false),
        }
    }

    fn enum_layout_for_def(&mut self, def_id: hir::DefId, span: Span) -> Option<EnumLayout> {
        let Some(definition) = self.enum_defs.get(&def_id) else {
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

    fn enum_layout_for_ty(&self, ty: &Ty) -> Option<&EnumLayout> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_ty(inner),
            TyKind::RawPtr(type_and_mut) => self.enum_layout_for_ty(&type_and_mut.ty),
            TyKind::Adt(adt, substs) if self.enum_defs.contains_key(&adt.did) => {
                let args: Vec<Ty> = substs
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(t) => Some(t.clone()),
                        _ => None,
                    })
                    .collect();
                let key = EnumLayoutKey {
                    def_id: adt.did,
                    args,
                };
                self.enum_layouts.get(&key)
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
    fn enum_layout_for_ty_fuzzy(&self, ty: &Ty) -> Option<&EnumLayout> {
        self.enum_layouts
            .values()
            .filter(|layout| Self::enum_layout_ty_matches(&layout.enum_ty, ty))
            .min_by_key(|layout| {
                let is_template = layout.args.iter().any(|arg| self.has_unresolved_ty(arg));
                (is_template, layout.def_id)
            })
    }

    fn enum_layout_ty_matches(layout_ty: &Ty, requested_ty: &Ty) -> bool {
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

    fn enum_layout_for_concrete_ty(&mut self, ty: &Ty, span: Span) -> Option<EnumLayout> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_concrete_ty(inner, span),
            TyKind::RawPtr(type_and_mut) => {
                self.enum_layout_for_concrete_ty(&type_and_mut.ty, span)
            }
            TyKind::Adt(adt, substs) => {
                if !self.enum_defs.contains_key(&adt.did) {
                    return None;
                }
                let mut args = Vec::new();
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        args.push(inner.clone());
                    }
                }
                self.enum_layout_for_instance(adt.did, &args, span)
            }
            _ => None,
        }
    }

    pub fn take_diagnostics(&mut self) -> (Vec<Diagnostic>, bool) {
        let diagnostics = std::mem::take(&mut self.diagnostics);
        let has_errors = std::mem::replace(&mut self.has_errors, false);
        (diagnostics, has_errors)
    }
}

impl Default for MirLowering {
    fn default() -> Self {
        Self::new()
    }
}

/// One undone mutation to `local_map`/`fallback_locals`, recorded by
/// `bind_match_binding` while `BodyBuilder::match_binding_undo_log` is
/// active. Carries the key's *previous* value (`None` if the key was
/// absent) so restoring can distinguish "put the old mapping back" from
/// "the key didn't exist before this arm" — a plain `.remove(key)` would
/// wrongly erase an outer-scope binding of the same name that the arm's
/// pattern shadowed.
enum MatchBindingUndo {
    LocalMap(hir::HirId, Option<mir::LocalId>),
    Fallback(String, Option<mir::LocalId>),
}

struct BodyBuilder<'a> {
    lowering: &'a mut MirLowering,
    function: &'a hir::Function,
    sig: &'a mir::FunctionSig,
    locals: Vec<mir::LocalDecl>,
    local_map: HashMap<hir::HirId, mir::LocalId>,
    fallback_locals: HashMap<String, mir::LocalId>,
    /// When `Some`, `bind_match_binding` records the pre-insert value (if
    /// any) of every `local_map`/`fallback_locals` key it touches here,
    /// instead of `lower_match_expr` cloning both whole maps before every
    /// arm and restoring the clones afterward — turns an O(arms ×
    /// bindings-so-far) clone-and-restore into an O(bindings-in-this-arm)
    /// undo log. Only ever active for the duration of a single
    /// `bind_match_pattern` call.
    match_binding_undo_log: Option<Vec<MatchBindingUndo>>,
    local_structs: HashMap<mir::LocalId, hir::DefId>,
    container_locals: HashMap<mir::LocalId, mir::ContainerKind>,
    const_items: HashMap<hir::DefId, hir::Const>,
    blocks: Vec<mir::BasicBlockData>,
    current_block: mir::BasicBlockId,
    span: Span,
    method_context: Option<MethodContext>,
    type_substs: HashMap<String, Ty>,
    loop_stack: Vec<LoopContext>,
    defer_scopes: Vec<DeferScope>,
    current_unwind_target: Option<mir::BasicBlockId>,
    null_locals: HashSet<mir::LocalId>,
    active_exprs: HashSet<hir::HirId>,
    control_flow_emitted: bool,
}

struct PlaceInfo {
    place: mir::Place,
    ty: Ty,
    struct_def: Option<hir::DefId>,
}

struct OperandInfo {
    operand: mir::Operand,
    ty: Ty,
}

struct StructRef {
    def_id: hir::DefId,
    args: Vec<Ty>,
}

impl OperandInfo {
    fn constant(span: Span, ty: Ty, literal: mir::ConstantKind) -> Self {
        Self {
            operand: mir::Operand::Constant(mir::Constant {
                span,
                ty: ty.clone(),
                user_ty: None,
                literal,
            }),
            ty,
        }
    }
}

#[derive(Clone)]
struct LoopDestination {
    place: mir::Place,
    ty: Ty,
}

#[derive(Clone)]
struct LoopContext {
    break_block: mir::BasicBlockId,
    continue_block: mir::BasicBlockId,
    break_destination: Option<LoopDestination>,
    break_value_allowed: bool,
    defer_scope_depth: usize,
}

struct DeferScope {
    deferred: Vec<hir::Expr>,
}

struct ExprRecursionGuard {
    set: *mut HashSet<hir::HirId>,
    id: hir::HirId,
}

impl ExprRecursionGuard {
    fn new(set: &mut HashSet<hir::HirId>, id: hir::HirId) -> Self {
        Self {
            set: set as *mut HashSet<hir::HirId>,
            id,
        }
    }
}

impl Drop for ExprRecursionGuard {
    fn drop(&mut self) {
        unsafe {
            (*self.set).remove(&self.id);
        }
    }
}

impl<'a> BodyBuilder<'a> {
    fn emit_c_call(
        &mut self,
        name: &str,
        sig: mir::FunctionSig,
        args: Vec<mir::Operand>,
        destination: mir::Place,
        span: Span,
    ) -> Result<()> {
        let continue_block = self.new_block();

        let fn_ty = self.lowering.c_function_pointer_ty(&sig);
        let func_operand = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::from(name.to_string())),
        });

        self.blocks[self.current_block as usize].terminator = Some(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args,
                destination: Some((destination.clone(), continue_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: false,
                fn_span: span,
            },
        });

        self.current_block = continue_block;
        Ok(())
    }

    fn lower_path_inner_str(&mut self, path_expr: &hir::Expr) -> Result<mir::Place> {
        // std::path::Path { inner: str }
        let path_place = if let Some(place_info) = self.lower_place(path_expr)? {
            place_info.place
        } else {
            let lowered = self.lower_operand(path_expr, None)?;
            match lowered.operand {
                mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                other => {
                    let local_id = self.allocate_temp(lowered.ty.clone(), path_expr.span);
                    let temp_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: path_expr.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Use(other),
                        ),
                    });
                    temp_place
                }
            }
        };

        let str_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };

        Ok(mir::Place {
            local: path_place.local,
            projection: path_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Deref, mir::PlaceElem::Field(0, str_ty)])
                .collect(),
        })
    }

    fn lower_slice_ptr_place(&self, slice_place: mir::Place) -> mir::Place {
        let elem_ty = self.lowering.raw_string_ptr_ty();
        mir::Place {
            local: slice_place.local,
            projection: slice_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Field(0, elem_ty)])
                .collect(),
        }
    }

    fn lower_slice_len_place(&self, slice_place: mir::Place) -> mir::Place {
        let len_ty = Ty {
            kind: TyKind::Int(IntTy::I64),
        };
        mir::Place {
            local: slice_place.local,
            projection: slice_place
                .projection
                .into_iter()
                .chain([mir::PlaceElem::Field(1, len_ty)])
                .collect(),
        }
    }

    fn lower_env_var_exists_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "env::exists intrinsic expects one name argument");
        }

        let name_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };
        let name_info = args
            .get(0)
            .map(|arg| self.lower_operand(&arg.value, Some(&name_ty)))
            .transpose()?;

        let name_place = if let Some(info) = &name_info {
            if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &info.operand {
                place.clone()
            } else {
                let local_id = self.allocate_temp(name_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Use(info.operand.clone()),
                    ),
                });
                local_place
            }
        } else {
            let local_id = self.allocate_temp(name_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        };

        let name_ptr_place = self.lower_slice_ptr_place(name_place);
        let name_ptr_op = mir::Operand::copy(name_ptr_place);

        let getenv_ret_ty = self.lowering.raw_string_ptr_ty();
        let getenv_local = self.allocate_temp(getenv_ret_ty.clone(), expr.span);
        let getenv_place = mir::Place::from_local(getenv_local);

        self.emit_c_call(
            "getenv",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: getenv_ret_ty.clone(),
            },
            vec![name_ptr_op],
            getenv_place.clone(),
            expr.span,
        )?;

        let is_null_local = self.allocate_temp(Ty { kind: TyKind::Bool }, expr.span);
        let is_null_place = mir::Place::from_local(is_null_local);
        let null_const = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: getenv_ret_ty,
            user_ty: None,
            literal: mir::ConstantKind::Null,
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                is_null_place.clone(),
                mir::Rvalue::BinaryOp(mir::BinOp::Eq, mir::Operand::copy(getenv_place), null_const),
            ),
        });

        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::UnaryOp(mir::UnOp::Not, mir::Operand::copy(is_null_place)),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    fn lower_env_var_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "env::var intrinsic expects one name argument");
        }

        let str_ty = Ty {
            kind: TyKind::Slice(Box::new(Ty {
                kind: TyKind::Int(IntTy::I8),
            })),
        };
        let name_info = args
            .get(0)
            .map(|arg| self.lower_operand(&arg.value, Some(&str_ty)))
            .transpose()?;

        let name_place = if let Some(info) = &name_info {
            if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &info.operand {
                place.clone()
            } else {
                let local_id = self.allocate_temp(str_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Use(info.operand.clone()),
                    ),
                });
                local_place
            }
        } else {
            let local_id = self.allocate_temp(str_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        };

        let name_ptr_place = self.lower_slice_ptr_place(name_place);

        let getenv_ret_ty = self.lowering.raw_string_ptr_ty();
        let getenv_local = self.allocate_temp(getenv_ret_ty.clone(), expr.span);
        let getenv_place = mir::Place::from_local(getenv_local);
        self.emit_c_call(
            "getenv",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: getenv_ret_ty.clone(),
            },
            vec![mir::Operand::copy(name_ptr_place)],
            getenv_place.clone(),
            expr.span,
        )?;

        let strlen_ret_ty = Ty {
            kind: TyKind::Uint(UintTy::Usize),
        };
        let strlen_local = self.allocate_temp(strlen_ret_ty.clone(), expr.span);
        let strlen_place = mir::Place::from_local(strlen_local);
        self.emit_c_call(
            "strlen",
            mir::FunctionSig {
                inputs: vec![getenv_ret_ty.clone()],
                output: strlen_ret_ty.clone(),
            },
            vec![mir::Operand::copy(getenv_place.clone())],
            strlen_place.clone(),
            expr.span,
        )?;

        // Build `str` slice in `place`: { ptr, len }
        let ptr_field_place = self.lower_slice_ptr_place(place.clone());
        let len_field_place = self.lower_slice_len_place(place.clone());
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field_place,
                mir::Rvalue::Use(mir::Operand::copy(getenv_place)),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field_place,
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(strlen_place),
                    Ty {
                        kind: TyKind::Int(IntTy::I64),
                    },
                ),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    fn lower_fs_exists_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering
                .emit_error(expr.span, "fs::exists intrinsic expects one path argument");
        }

        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;

        let path_ptr = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let ret_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let access_local = self.allocate_temp(ret_ty.clone(), expr.span);
        let access_place = mir::Place::from_local(access_local);
        let f_ok = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: ret_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Int(0),
        });
        self.emit_c_call(
            "access",
            mir::FunctionSig {
                inputs: vec![self.lowering.raw_string_ptr_ty(), ret_ty.clone()],
                output: ret_ty.clone(),
            },
            vec![mir::Operand::copy(path_ptr), f_ok],
            access_place.clone(),
            expr.span,
        )?;

        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::BinaryOp(
                    mir::BinOp::Eq,
                    mir::Operand::copy(access_place),
                    mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: ret_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Int(0),
                    }),
                ),
            ),
        });

        let _ = expected_ty;
        Ok(())
    }

    fn lower_fs_remove_file_as_statement(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "fs::remove_file intrinsic expects one path argument",
            );
        }
        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;
        let path_ptr = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let ret_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let local_id = self.allocate_temp(ret_ty.clone(), expr.span);
        let temp_place = mir::Place::from_local(local_id);
        self.emit_c_call(
            "remove",
            mir::FunctionSig {
                inputs: vec![self.lowering.raw_string_ptr_ty()],
                output: ret_ty,
            },
            vec![mir::Operand::copy(path_ptr)],
            temp_place,
            expr.span,
        )
    }

    fn lower_fs_read_to_string_into_place(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let args = &call.callargs;
        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "fs_read_to_string intrinsic expects one path argument",
            );
        }

        let path_inner = args
            .get(0)
            .map(|arg| self.lower_path_inner_str(&arg.value))
            .transpose()?;
        let path_ptr_place = path_inner
            .map(|p| self.lower_slice_ptr_place(p))
            .unwrap_or_else(|| {
                let local = self.allocate_temp(self.lowering.raw_string_ptr_ty(), expr.span);
                let place = mir::Place::from_local(local);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Null,
                        })),
                    ),
                });
                place
            });

        let file_ty = self.lowering.raw_string_ptr_ty();
        let file_local = self.allocate_temp(file_ty.clone(), expr.span);
        let file_place = mir::Place::from_local(file_local);
        // mode = "rb"
        let mode_const = mir::Operand::Constant(mir::Constant {
            span: expr.span,
            ty: self.lowering.raw_string_ptr_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Str("rb".to_string()),
        });

        self.emit_c_call(
            "fopen",
            mir::FunctionSig {
                inputs: vec![
                    self.lowering.raw_string_ptr_ty(),
                    self.lowering.raw_string_ptr_ty(),
                ],
                output: file_ty.clone(),
            },
            vec![mir::Operand::copy(path_ptr_place), mode_const],
            file_place.clone(),
            expr.span,
        )?;

        // If fopen failed, return empty string slice.
        let ok_block = self.new_block();
        let fail_block = self.new_block();
        let join_block = self.new_block();

        let is_null_local = self.allocate_temp(Ty { kind: TyKind::Bool }, expr.span);
        let is_null_place = mir::Place::from_local(is_null_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                is_null_place.clone(),
                mir::Rvalue::BinaryOp(
                    mir::BinOp::Eq,
                    mir::Operand::copy(file_place.clone()),
                    mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: file_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Null,
                    }),
                ),
            ),
        });

        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: mir::Operand::copy(is_null_place),
                switch_ty: Ty { kind: TyKind::Bool },
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![fail_block],
                    otherwise: ok_block,
                },
            },
        });

        // fail: set place to empty slice
        self.current_block = fail_block;
        let ptr_field = self.lower_slice_ptr_place(place.clone());
        let len_field = self.lower_slice_len_place(place.clone());
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field,
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: self.lowering.raw_string_ptr_ty(),
                    user_ty: None,
                    literal: mir::ConstantKind::Str("".to_string()),
                })),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field,
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty {
                        kind: TyKind::Int(IntTy::I64),
                    },
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        // ok: read file size via fseek/ftell, malloc, fread, fclose
        self.current_block = ok_block;
        let int_ty = Ty {
            kind: TyKind::Int(IntTy::I32),
        };
        let long_ty = Ty {
            kind: TyKind::Int(IntTy::I64),
        };
        let size_ty = Ty {
            kind: TyKind::Uint(UintTy::Usize),
        };

        let seek_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fseek",
            mir::FunctionSig {
                inputs: vec![file_ty.clone(), long_ty.clone(), int_ty.clone()],
                output: int_ty.clone(),
            },
            vec![
                mir::Operand::copy(file_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: long_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                }),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: int_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(2), // SEEK_END
                }),
            ],
            mir::Place::from_local(seek_ret_local),
            expr.span,
        )?;

        let len_local = self.allocate_temp(long_ty.clone(), expr.span);
        let len_place = mir::Place::from_local(len_local);
        self.emit_c_call(
            "ftell",
            mir::FunctionSig {
                inputs: vec![file_ty.clone()],
                output: long_ty.clone(),
            },
            vec![mir::Operand::copy(file_place.clone())],
            len_place.clone(),
            expr.span,
        )?;

        let rewind_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fseek",
            mir::FunctionSig {
                inputs: vec![file_ty.clone(), long_ty.clone(), int_ty.clone()],
                output: int_ty.clone(),
            },
            vec![
                mir::Operand::copy(file_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: long_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0),
                }),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: int_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Int(0), // SEEK_SET
                }),
            ],
            mir::Place::from_local(rewind_ret_local),
            expr.span,
        )?;

        let malloc_ret_ty = self.lowering.raw_string_ptr_ty();
        let buf_local = self.allocate_temp(malloc_ret_ty.clone(), expr.span);
        let buf_place = mir::Place::from_local(buf_local);
        let size_cast_local = self.allocate_temp(size_ty.clone(), expr.span);
        let size_cast_place = mir::Place::from_local(size_cast_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                size_cast_place.clone(),
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(len_place.clone()),
                    size_ty.clone(),
                ),
            ),
        });
        self.emit_c_call(
            "malloc",
            mir::FunctionSig {
                inputs: vec![size_ty.clone()],
                output: malloc_ret_ty.clone(),
            },
            vec![mir::Operand::copy(size_cast_place.clone())],
            buf_place.clone(),
            expr.span,
        )?;

        let fread_ret_local = self.allocate_temp(size_ty.clone(), expr.span);
        let fread_len_cast_local = self.allocate_temp(size_ty.clone(), expr.span);
        let fread_len_cast_place = mir::Place::from_local(fread_len_cast_local);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                fread_len_cast_place.clone(),
                mir::Rvalue::Cast(
                    mir::CastKind::Misc,
                    mir::Operand::copy(len_place.clone()),
                    size_ty.clone(),
                ),
            ),
        });
        self.emit_c_call(
            "fread",
            mir::FunctionSig {
                inputs: vec![
                    malloc_ret_ty.clone(),
                    size_ty.clone(),
                    size_ty.clone(),
                    file_ty.clone(),
                ],
                output: size_ty.clone(),
            },
            vec![
                mir::Operand::copy(buf_place.clone()),
                mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: size_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::UInt(1),
                }),
                mir::Operand::copy(fread_len_cast_place),
                mir::Operand::copy(file_place.clone()),
            ],
            mir::Place::from_local(fread_ret_local),
            expr.span,
        )?;

        let fclose_ret_local = self.allocate_temp(int_ty.clone(), expr.span);
        self.emit_c_call(
            "fclose",
            mir::FunctionSig {
                inputs: vec![file_ty.clone()],
                output: int_ty,
            },
            vec![mir::Operand::copy(file_place)],
            mir::Place::from_local(fclose_ret_local),
            expr.span,
        )?;

        // write slice fields
        let ptr_field_place = self.lower_slice_ptr_place(place.clone());
        let len_field_place = self.lower_slice_len_place(place);
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                ptr_field_place,
                mir::Rvalue::Use(mir::Operand::copy(buf_place)),
            ),
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                len_field_place,
                mir::Rvalue::Use(mir::Operand::copy(len_place)),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        let _ = expected_ty;
        Ok(())
    }

    fn new(
        lowering: &'a mut MirLowering,
        function: &'a hir::Function,
        sig: &'a mir::FunctionSig,
        span: Span,
        method_context: Option<MethodContext>,
        type_substs: HashMap<String, Ty>,
    ) -> Self {
        let mut locals = Vec::new();
        locals.push(lowering.make_local_decl(&sig.output, span));

        let mut builder = Self {
            lowering,
            function,
            sig,
            locals,
            local_map: HashMap::new(),
            fallback_locals: HashMap::new(),
            match_binding_undo_log: None,
            local_structs: HashMap::new(),
            container_locals: HashMap::new(),
            const_items: HashMap::new(),
            blocks: vec![mir::BasicBlockData::new(None)],
            current_block: 0,
            span,
            method_context,
            type_substs,
            loop_stack: Vec::new(),
            defer_scopes: Vec::new(),
            current_unwind_target: None,
            null_locals: HashSet::new(),
            active_exprs: HashSet::new(),
            control_flow_emitted: false,
        };

        let body_params = builder
            .function
            .body
            .as_ref()
            .map(|_| builder.function.sig.inputs.as_slice())
            .unwrap_or(&[]);

        for (idx, ty) in builder.sig.inputs.iter().enumerate() {
            let mut decl = builder.lowering.make_local_decl(ty, builder.span);
            decl.mutability = mir::Mutability::Not;
            let local_id = builder.push_local(decl);

            if let Some(param) = body_params.get(idx) {
                builder.bind_pattern(&param.pat, local_id, Some(ty));
            }
        }

        builder
    }

    fn push_local(&mut self, decl: mir::LocalDecl) -> mir::LocalId {
        let local_id = self.locals.len() as mir::LocalId;
        self.locals.push(decl);
        local_id
    }

    fn is_null_literal_expr(expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Literal(hir::Lit::Null))
    }

    fn update_null_tracking(&mut self, place: mir::Place, ty: Option<&Ty>, expr: &hir::Expr) {
        if !place.projection.is_empty() {
            return;
        }
        if let Some(ty) = ty {
            if !matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                self.null_locals.remove(&place.local);
                return;
            }
        }
        if Self::is_null_literal_expr(expr) {
            self.null_locals.insert(place.local);
        } else {
            self.null_locals.remove(&place.local);
        }
    }

    fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        if let Some(ctx) = self.method_context.as_ref() {
            if Self::type_expr_mentions_self(ty_expr) {
                return self.lowering.lower_type_expr_with_context_and_substs(
                    ty_expr,
                    Some(ctx),
                    &self.type_substs,
                );
            }
        }
        if Self::is_builtin_type_path(ty_expr) {
            return self.lowering.lower_type_expr(ty_expr);
        }
        if let hir::TypeExprKind::Ref(inner) = &ty_expr.kind {
            if self.lowering.is_string_slice_ref(inner) {
                return self.lowering.string_slice_ty();
            }
        }
        if let Some(ty) = self.lowering.typeck_type_exprs.get(&ty_expr.hir_id) {
            // The type checker type-checks a generic method body once,
            // abstractly, before any monomorphization exists as a concept —
            // its cached type for a bare generic-param reference inside
            // that body (e.g. `*mut T` in `Vec<T>::index`) is genuinely
            // `T`, unsubstituted. Trusting it here, while lowering *this*
            // call's specialized body (`type_substs` populated, e.g.
            // `T -> &str`), would leak that unresolved placeholder straight
            // into typed MIR. Only use the cached type when it doesn't
            // still contain something `type_substs` would otherwise
            // resolve.
            let trust_cache = !matches!(ty.kind, TyKind::Error(_))
                && (self.type_substs.is_empty() || !self.lowering.has_unresolved_ty(ty));
            if trust_cache {
                return ty.clone();
            }
        }
        // NOTE(jakku): This is the key hook for generic lowering. When
        // type_substs is populated, we substitute generic params so MIR
        // sees concrete types. Otherwise we fall back to the existing
        // lowering (which treats unknown generics as opaque).
        if self.type_substs.is_empty() {
            return self.lowering.lower_type_expr(ty_expr);
        }
        self.lowering
            .lower_type_expr_with_substs(ty_expr, &self.type_substs)
    }

    fn is_builtin_type_path(ty_expr: &hir::TypeExpr) -> bool {
        let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
            return false;
        };
        let Some(segment) = path.segments.last() else {
            return false;
        };
        matches!(
            segment.name.as_str(),
            "bool"
                | "char"
                | "str"
                | "i8"
                | "i16"
                | "i32"
                | "i64"
                | "i128"
                | "isize"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "u128"
                | "usize"
                | "f16"
                | "f32"
                | "f64"
                | "f128"
        )
    }

    fn type_expr_mentions_self(ty_expr: &hir::TypeExpr) -> bool {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .first()
                .map(|segment| segment.name.as_str() == "Self")
                .unwrap_or(false),
            hir::TypeExprKind::Tuple(items) => {
                items.iter().any(|item| Self::type_expr_mentions_self(item))
            }
            hir::TypeExprKind::Array(item, _) | hir::TypeExprKind::Slice(item) => {
                Self::type_expr_mentions_self(item)
            }
            hir::TypeExprKind::Ptr(item) | hir::TypeExprKind::Ref(item) => {
                Self::type_expr_mentions_self(item)
            }
            hir::TypeExprKind::FnPtr(function) => {
                function
                    .inputs
                    .iter()
                    .any(|item| Self::type_expr_mentions_self(item))
                    || Self::type_expr_mentions_self(&function.output)
            }
            _ => false,
        }
    }

    fn bind_pattern(&mut self, pat: &hir::Pat, local: mir::LocalId, ty: Option<&Ty>) {
        match &pat.kind {
            hir::PatKind::Binding { name, mutable } => {
                self.local_map.insert(pat.hir_id, local);
                self.fallback_locals
                    .insert(name.as_str().to_string(), local);
                if let Some(decl) = self.locals.get_mut(local as usize) {
                    if *mutable {
                        decl.mutability = mir::Mutability::Mut;
                    }
                    let mut struct_def = ty.and_then(|ty| self.struct_def_from_ty(ty));
                    if let Some(ctx) = &self.method_context {
                        if let Some(def_id) = ctx.def_id {
                            let name_matches_self = name.as_str() == "self";
                            let ty_matches_self = ty
                                .map(|ty| self.ty_matches(ty, &ctx.mir_self_ty))
                                .unwrap_or(false);
                            if name_matches_self || ty_matches_self {
                                struct_def = Some(def_id);
                            }
                        }
                    }
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(local, def_id);
                    }
                }
            }
            hir::PatKind::Wild => {
                self.local_map.insert(pat.hir_id, local);
            }
            _ => {
                self.local_map.insert(pat.hir_id, local);
                let place = mir::Place::from_local(local);
                let scrutinee_ty = ty.cloned().unwrap_or_else(|| {
                    self.locals
                        .get(local as usize)
                        .map(|decl| decl.ty.clone())
                        .unwrap_or(Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        })
                });
                self.bind_match_pattern(pat, &place, &scrutinee_ty, self.span);
            }
        }
    }

    fn struct_def_from_ty(&mut self, ty: &Ty) -> Option<hir::DefId> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.struct_def_from_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.struct_def_from_ty(type_and_mut.ty.as_ref()),
            // `path_ty` (`fp-typing/src/hir_typeck.rs`) builds a struct's
            // `AdtDef` with an empty `variants` list (only enums populate
            // it), so an unannotated local bound to a function-call result
            // carries that empty-variants `Adt` straight through — check
            // `struct_defs` (keyed by the real `DefId`) directly first,
            // rather than only via the name-based fallback below, which
            // needs `display_type_name` to already know the name.
            //
            // A struct declared in a dependency package (e.g. `std`) may
            // not be registered yet if nothing has forced its layout to be
            // computed so far — mirror `compute_adt_layout`'s lazy foreign-
            // struct/enum registration here too, rather than falling
            // through to the name-based fallback below (which needs
            // `struct_defs_by_tail_name` to already have an entry, i.e. the
            // very registration we're trying to trigger).
            TyKind::Adt(adt, _) => {
                if !self.lowering.struct_defs.contains_key(&adt.did) {
                    self.lowering.try_lazily_register_adt(adt.did, Span::null());
                }
                if self.lowering.struct_defs.contains_key(&adt.did) {
                    Some(adt.did)
                } else {
                    Self::struct_def_from_ty_by_name(self.lowering, ty)
                }
            }
            _ => Self::struct_def_from_ty_by_name(self.lowering, ty),
        }
    }

    fn struct_def_from_ty_by_name(lowering: &MirLowering, ty: &Ty) -> Option<hir::DefId> {
        lowering
            .struct_layouts_by_ty
            .get(ty)
            .map(|key| key.def_id)
            .or_else(|| {
                let name = lowering.display_type_name(ty)?;
                // `struct_defs_by_tail_name` narrows straight to the
                // (usually one-element) candidate set sharing `name`'s
                // tail segment — provably safe, since the match
                // condition below can only hold when `def.name`'s tail
                // segment equals `name`'s tail segment (see that
                // field's doc comment) — instead of scanning every
                // struct definition in the program with a `format!`
                // allocation per iteration.
                let candidates = lowering
                    .struct_defs_by_tail_name
                    .get(MirLowering::name_tail(&name))?;
                let matches: Vec<hir::DefId> = candidates
                    .iter()
                    .filter_map(|def_id| {
                        let def = lowering.struct_defs.get(def_id)?;
                        if def.name == name || def.name.ends_with(&format!("::{}", name)) {
                            Some(*def_id)
                        } else {
                            None
                        }
                    })
                    .collect();
                if matches.len() == 1 {
                    matches.into_iter().next()
                } else {
                    None
                }
            })
    }

    fn boxed_inner_ty(&self, ty: &Ty) -> Option<Ty> {
        let TyKind::Adt(adt, substs) = &ty.kind else {
            return None;
        };

        let is_box = adt
            .variants
            .first()
            .map(|variant| variant.ident.as_str())
            .map(|name| name == "Box" || name.ends_with("::Box"))
            .unwrap_or(false);
        if !is_box {
            return None;
        }

        let first = substs.first()?;
        let mir::ty::GenericArg::Type(inner) = first else {
            return None;
        };
        Some(inner.clone())
    }

    fn enum_def_from_ty(&self, ty: &Ty) -> Option<hir::DefId> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_def_from_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.enum_def_from_ty(type_and_mut.ty.as_ref()),
            // Mirrors `struct_def_from_ty`'s `Adt`-shell check: a lazily
            // resolved generic argument (`adt_shell_ty`) carries the real
            // `DefId` directly, so it's checked before falling back to the
            // by-value `enum_layouts` scan below.
            TyKind::Adt(adt, _) if self.lowering.enum_defs.contains_key(&adt.did) => {
                Some(adt.did)
            }
            _ => self
                .lowering
                .enum_layouts
                .iter()
                .find_map(|(key, layout)| (layout.enum_ty == *ty).then_some(key.def_id)),
        }
    }

    fn enum_layout_for_ty(&mut self, ty: &Ty, span: Span) -> Option<EnumLayout> {
        if let Some(layout) = self.lowering.enum_layout_for_ty_exact(ty) {
            return Some(layout.clone());
        }
        self.lowering
            .enum_layout_for_concrete_ty(ty, span)
            .or_else(|| self.lowering.enum_layout_for_ty(ty).cloned())
    }

    fn enum_layout_for_variant(
        &mut self,
        variant: &EnumVariantInfo,
        ty_hint: Option<&Ty>,
        span: Span,
    ) -> Option<EnumLayout> {
        let Some(ty_hint) = ty_hint else {
            return None;
        };
        self.enum_layout_for_variant_ty(variant, ty_hint, span)
    }

    fn enum_layout_for_variant_ty(
        &mut self,
        variant: &EnumVariantInfo,
        ty_hint: &Ty,
        span: Span,
    ) -> Option<EnumLayout> {
        match &ty_hint.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_variant_ty(variant, inner, span),
            TyKind::RawPtr(type_and_mut) => {
                self.enum_layout_for_variant_ty(variant, &type_and_mut.ty, span)
            }
            TyKind::Adt(adt, substs) => {
                if adt.did != variant.enum_def {
                    return None;
                }
                let mut args = Vec::new();
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        args.push(inner.clone());
                    }
                }
                let mut layout = self
                    .lowering
                    .enum_layout_for_instance(adt.did, &args, span)?;
                if !layout.variant_payloads.contains_key(&variant.def_id) {
                    if let Some(payloads) = self
                        .lowering
                        .enum_variant_payloads_for_args(variant, &args, span)
                    {
                        layout.variant_payloads.insert(variant.def_id, payloads);
                    }
                }
                Some(layout)
            }
            TyKind::Opaque(def_id, substs) => {
                if *def_id != variant.enum_def {
                    return None;
                }
                let mut args = Vec::new();
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        args.push(inner.clone());
                    }
                }
                let mut layout = self
                    .lowering
                    .enum_layout_for_instance(*def_id, &args, span)?;
                if !layout.variant_payloads.contains_key(&variant.def_id) {
                    if let Some(payloads) = self
                        .lowering
                        .enum_variant_payloads_for_args(variant, &args, span)
                    {
                        layout.variant_payloads.insert(variant.def_id, payloads);
                    }
                }
                Some(layout)
            }
            _ => None,
        }
    }

    fn infer_enum_args_from_expected_ty(
        &self,
        enum_def: hir::DefId,
        expected_ty: &Ty,
    ) -> Option<Vec<Ty>> {
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty);
        match &expected_ty.kind {
            TyKind::Ref(_, inner, _) => self.infer_enum_args_from_expected_ty(enum_def, inner),
            TyKind::RawPtr(type_and_mut) => {
                self.infer_enum_args_from_expected_ty(enum_def, &type_and_mut.ty)
            }
            TyKind::Adt(adt, substs) => {
                if adt.did == enum_def {
                    let mut args = Vec::new();
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            args.push(inner.clone());
                        }
                    }
                    return if args.is_empty() { None } else { Some(args) };
                }
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        if let Some(args) = self.infer_enum_args_from_expected_ty(enum_def, inner) {
                            return Some(args);
                        }
                    }
                }
                None
            }
            TyKind::Opaque(def_id, substs) => {
                if *def_id == enum_def {
                    let mut args = Vec::new();
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            args.push(inner.clone());
                        }
                    }
                    return if args.is_empty() { None } else { Some(args) };
                }
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        if let Some(args) = self.infer_enum_args_from_expected_ty(enum_def, inner) {
                            return Some(args);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// When the current function body is a specific generic
    /// specialization (`self.type_substs` non-empty, e.g. lowering
    /// `unwrap_or::<i64>`'s body), prefer computing this variant's payload
    /// types fresh from that specialization's own substitution map over
    /// anything a `layout`/`enum_layouts` lookup might return. Those
    /// lookups key on the scrutinee's *type shape* (see
    /// `enum_layout_ty_matches`'s wildcard `TyKind::Infer` matching), which
    /// can accidentally match a stale, differently- or not-yet-substituted
    /// layout cached from an earlier, generic (unspecialized) pass over
    /// the same enum+variant — `type_substs`, in contrast, is always the
    /// authoritative substitution for *this* specific specialization.
    /// Returns `None` when `type_substs` is empty or doesn't cover this
    /// variant's enum (e.g. a genuinely non-generic enum, or a generic one
    /// matched outside any specialized method body), letting the caller
    /// fall back to the layout-based derivation as before.
    fn payload_types_from_type_substs(&mut self, variant: &EnumVariantInfo, span: Span) -> Option<Vec<Ty>> {
        if self.type_substs.is_empty() {
            return None;
        }
        let generics = self.lowering.enum_defs.get(&variant.enum_def)?.generics.clone();
        if generics.is_empty() {
            return None;
        }
        let mut args = Vec::with_capacity(generics.len());
        for name in &generics {
            args.push(self.type_substs.get(name)?.clone());
        }
        self.lowering
            .enum_variant_payloads_for_args(variant, &args, span)
    }

    fn variant_payloads_from_layout_or_ty(
        &mut self,
        layout: &EnumLayout,
        variant: &EnumVariantInfo,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Vec<Ty> {
        if let Some(payloads) = self.payload_types_from_type_substs(variant, span) {
            return payloads;
        }
        if let Some(payloads) = layout.variant_payloads.get(&variant.def_id) {
            return payloads.clone();
        }
        if layout.def_id == variant.enum_def {
            if let Some(payloads) =
                self.lowering
                    .enum_variant_payloads_for_args(variant, &layout.args, span)
            {
                return payloads;
            }
        }

        let mut ty = scrutinee_ty;
        if let TyKind::Ref(_, inner, _) = &ty.kind {
            ty = inner.as_ref();
        }
        if let TyKind::RawPtr(type_and_mut) = &ty.kind {
            ty = type_and_mut.ty.as_ref();
        }
        if layout.def_id != variant.enum_def {
            let matching_layout = self
                .lowering
                .enum_layouts
                .iter()
                .find(|(key, layout)| {
                    key.def_id == variant.enum_def
                        && self.ty_matches_with_opaque(&layout.enum_ty, ty)
                })
                .map(|(_, layout)| layout.clone());
            if let Some(matching_layout) = matching_layout {
                if let Some(payloads) = matching_layout.variant_payloads.get(&variant.def_id) {
                    return payloads.clone();
                }
                if let Some(payloads) = self.lowering.enum_variant_payloads_for_args(
                    variant,
                    &matching_layout.args,
                    span,
                ) {
                    return payloads;
                }
            }
        }
        if let TyKind::Adt(adt, substs) = &ty.kind {
            if adt.did == variant.enum_def {
                let mut args = Vec::new();
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        args.push(inner.clone());
                    }
                }
                if let Some(payloads) = self
                    .lowering
                    .enum_variant_payloads_for_args(variant, &args, span)
                {
                    return payloads;
                }
            }
        }
        if let TyKind::Tuple(fields) = &ty.kind {
            if fields.len() >= 1 {
                return fields
                    .iter()
                    .skip(1)
                    .map(|field| (**field).clone())
                    .collect();
            }
        }

        self.lowering.emit_error(
            span,
            format!(
                "enum variant payload layout not registered (variant={:?}, enum_def={:?}, layout_def={:?}, scrutinee_ty={:?})",
                variant.def_id,
                variant.enum_def,
                layout.def_id,
                scrutinee_ty.kind,
            ),
        );
        Vec::new()
    }

    fn ty_matches(&self, lhs: &Ty, rhs: &Ty) -> bool {
        fn strip_refs<'a>(ty: &'a Ty) -> &'a Ty {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => strip_refs(inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => strip_refs(type_and_mut.ty.as_ref()),
                _ => ty,
            }
        }

        strip_refs(lhs) == strip_refs(rhs)
    }

    fn ty_matches_with_opaque(&self, lhs: &Ty, rhs: &Ty) -> bool {
        fn strip_refs<'a>(ty: &'a Ty) -> &'a Ty {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => strip_refs(inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => strip_refs(type_and_mut.ty.as_ref()),
                _ => ty,
            }
        }

        fn matches<F>(lhs: &Ty, rhs: &Ty, is_opaque: &F) -> bool
        where
            F: Fn(&Ty) -> bool,
        {
            let lhs = strip_refs(lhs);
            let rhs = strip_refs(rhs);

            if is_opaque(lhs) || is_opaque(rhs) {
                return true;
            }

            match (&lhs.kind, &rhs.kind) {
                (TyKind::Tuple(left), TyKind::Tuple(right)) => {
                    if left.len() != right.len() {
                        return false;
                    }
                    left.iter()
                        .zip(right.iter())
                        .all(|(l, r)| matches(l, r, is_opaque))
                }
                (TyKind::Array(left, left_len), TyKind::Array(right, right_len)) => {
                    left_len == right_len && matches(left, right, is_opaque)
                }
                (TyKind::Slice(left), TyKind::Slice(right)) => matches(left, right, is_opaque),
                _ => lhs == rhs,
            }
        }

        matches(lhs, rhs, &|ty| self.lowering.is_opaque_ty(ty))
    }

    fn lower(mut self) -> Result<mir::Body> {
        let has_body = self.function.body.is_some();
        if let Some(body) = &self.function.body {
            self.lower_block(body)?;
        }

        // A body-less declaration (its real body was a compiler-intrinsic
        // marker, dropped by `function_body_is_compiler_intrinsic_marker`)
        // never assigns its return local from anything real, so there's
        // nothing to validate here.
        //
        // Separately: `any` (e.g. `spawn(fut: any) -> any`) has no real
        // unification support — every occurrence gets its own, never-unified
        // fresh inference variable, even when semantically meant to be "the
        // same type" (a wrapper's own declared return type vs. the type its
        // body's tail call to the real intrinsic actually produces). Neither
        // side of such a comparison carries verifiable type information, so
        // an `Infer` on either side is a "don't know, don't fail" case, not
        // a genuine mismatch — this mirrors `contains_unresolved_param`'s
        // identical tolerance for `Infer` in MIR-to-LIR.
        let expected_return_ty = self.sig.output.clone();
        let actual_return_ty = &self.locals[0].ty;
        let either_unresolved = matches!(actual_return_ty.kind, TyKind::Infer(_))
            || matches!(expected_return_ty.kind, TyKind::Infer(_));
        if has_body && !either_unresolved && *actual_return_ty != expected_return_ty {
            return Err(fp_core::error::Error::from(format!(
                "function body lowered to `{}` but expected return type `{}`",
                actual_return_ty, expected_return_ty
            )));
        }

        self.ensure_terminated();
        Ok(mir::Body::new(
            self.blocks,
            self.locals,
            self.sig.inputs.len(),
            self.span,
        ))
    }

    fn ensure_terminated(&mut self) {
        if let Some(block) = self.blocks.last_mut() {
            if block.terminator.is_none() {
                block.terminator = Some(mir::Terminator {
                    source_info: self.span,
                    kind: mir::TerminatorKind::Return,
                });
            }
        }
    }

    fn lower_block(&mut self, block: &hir::Block) -> Result<()> {
        self.lower_block_impl(block, true)
    }

    fn lower_block_as_statement(&mut self, block: &hir::Block) -> Result<()> {
        self.lower_block_impl(block, false)
    }

    fn lower_block_impl(&mut self, block: &hir::Block, is_tail: bool) -> Result<()> {
        let scope_depth = self.defer_scopes.len();
        self.defer_scopes.push(DeferScope {
            deferred: Vec::new(),
        });

        let mut tail_expr = block.expr.as_deref();
        let mut stmt_slice = block.stmts.as_slice();
        if tail_expr.is_none() {
            if let Some(last) = block.stmts.last() {
                if let hir::StmtKind::Expr(expr) = &last.kind {
                    tail_expr = Some(expr);
                    stmt_slice = &block.stmts[..block.stmts.len().saturating_sub(1)];
                }
            }
        }

        for stmt in stmt_slice {
            self.lower_stmt(stmt)?;
            if self.control_flow_emitted {
                break;
            }
        }

        if !self.control_flow_emitted {
            if let Some(expr) = tail_expr {
                if is_tail {
                    if let hir::ExprKind::Block(inner) = &expr.kind {
                        self.lower_block(inner)?;
                    } else {
                        self.lower_tail_expr(expr)?;
                    }
                } else {
                    self.lower_expr_as_statement(expr)?;
                }
            }
        }

        if self.defer_scopes.len() > scope_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
        }

        Ok(())
    }

    fn run_popped_deferred(&mut self, scope: DeferScope) -> Result<()> {
        for deferred in scope.deferred.into_iter().rev() {
            self.control_flow_emitted = false;
            self.lower_expr_as_statement(&deferred)?;
            if self.control_flow_emitted {
                break;
            }
        }
        Ok(())
    }

    fn unwind_defer_scopes_to(&mut self, target_depth: usize) -> Result<()> {
        while self.defer_scopes.len() > target_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
            if self.control_flow_emitted {
                return Ok(());
            }
        }
        Ok(())
    }

    fn with_unwind_target<T>(
        &mut self,
        unwind_target: Option<mir::BasicBlockId>,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let saved = self.current_unwind_target;
        self.current_unwind_target = unwind_target;
        let result = f(self);
        self.current_unwind_target = saved;
        result
    }

    fn lower_try_expr(
        &mut self,
        expr: &hir::Expr,
        expr_try: &hir::TryExpr,
        destination: Option<(mir::Place, Ty)>,
        as_statement: bool,
    ) -> Result<()> {
        let outer_scope_depth = self.defer_scopes.len();
        if let Some(finally_expr) = expr_try.finally.as_ref() {
            self.defer_scopes.push(DeferScope {
                deferred: vec![finally_expr.as_ref().clone()],
            });
        }

        let join_block = self.new_block();
        let panic_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(panic_block as usize) {
            block.is_cleanup = true;
        }

        self.control_flow_emitted = false;
        self.with_unwind_target(Some(panic_block), |this| match &destination {
            Some((place, ty)) if !as_statement && expr_try.elze.is_none() => {
                this.lower_expr_into_place(&expr_try.expr, place.clone(), ty)
            }
            _ => this.lower_expr_as_statement(&expr_try.expr),
        })?;

        if !self.control_flow_emitted {
            if let Some(elze) = expr_try.elze.as_ref() {
                self.control_flow_emitted = false;
                match &destination {
                    Some((place, ty)) if !as_statement => {
                        self.lower_expr_into_place(elze, place.clone(), ty)?;
                    }
                    _ => self.lower_expr_as_statement(elze)?,
                }
            }
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: expr.span,
                    kind: mir::TerminatorKind::Goto { target: join_block },
                });
            }
        }

        let outer_unwind = self.current_unwind_target;
        let mut next_catch_block = panic_block;
        for (idx, catch) in expr_try.catches.iter().enumerate() {
            self.current_block = next_catch_block;
            let fallback_block = if idx + 1 < expr_try.catches.len() {
                let block = self.new_block();
                if let Some(data) = self.blocks.get_mut(block as usize) {
                    data.is_cleanup = true;
                }
                Some(block)
            } else {
                None
            };

            if let Some(pat) = &catch.pat {
                let panic_value_local =
                    self.allocate_temp(self.lowering.raw_string_ptr_ty(), catch.body.span);
                self.push_statement(mir::Statement {
                    source_info: catch.body.span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(panic_value_local),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span: catch.body.span,
                            ty: self.lowering.raw_string_ptr_ty(),
                            user_ty: None,
                            literal: mir::ConstantKind::Str(
                                "<panic payload unavailable>".to_string(),
                            ),
                        })),
                    ),
                });
                self.bind_pattern(
                    pat,
                    panic_value_local,
                    Some(&self.lowering.raw_string_ptr_ty()),
                );
            }

            self.control_flow_emitted = false;
            self.with_unwind_target(fallback_block, |this| match &destination {
                Some((place, ty)) if !as_statement => {
                    this.lower_expr_into_place(&catch.body, place.clone(), ty)
                }
                _ => this.lower_expr_as_statement(&catch.body),
            })?;
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: catch.body.span,
                    kind: mir::TerminatorKind::Goto { target: join_block },
                });
            }

            if let Some(block) = fallback_block {
                next_catch_block = block;
            }
        }

        self.current_block = next_catch_block;
        if expr_try.catches.is_empty()
            || self.blocks[self.current_block as usize]
                .terminator
                .is_none()
        {
            self.with_unwind_target(outer_unwind, |this| this.lower_panic(expr.span, &[]))?;
        }

        self.current_block = join_block;
        self.control_flow_emitted = false;
        if self.defer_scopes.len() > outer_scope_depth {
            let scope = self.defer_scopes.pop().unwrap();
            self.run_popped_deferred(scope)?;
        }

        Ok(())
    }

    fn lower_let_expr(
        &mut self,
        pat: &hir::Pat,
        ty: &hir::TypeExpr,
        init: &Option<Box<hir::Expr>>,
        span: Span,
    ) -> Result<()> {
        let init_span = init.as_ref().map(|expr| expr.span).unwrap_or(span);
        let ty_is_infer = matches!(ty.kind, hir::TypeExprKind::Infer | hir::TypeExprKind::Error);
        let declared_ty = if ty_is_infer {
            None
        } else {
            Some(self.lower_type_expr(ty))
        };
        let mut storage_ty = declared_ty.clone();
        let annotated_enum_def = if ty_is_infer {
            None
        } else if let hir::TypeExprKind::Path(path) = &ty.kind {
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self.lowering.enum_defs.contains_key(def_id) {
                    Some(*def_id)
                } else {
                    None
                }
            } else {
                if let Some(seg) = path.segments.last() {
                    let name = seg.name.as_str();
                    self.lowering.enum_defs_by_name.get(name).copied()
                } else {
                    None
                }
            }
        } else {
            None
        };
        if !ty_is_infer {
            if let hir::TypeExprKind::Path(path) = &ty.kind {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if self.lowering.enum_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| self.lowering.lower_generic_args(Some(args), init_span))
                            .unwrap_or_default();
                        let layout = if args.is_empty() {
                            self.lowering.enum_layout_for_def(*def_id, init_span)
                        } else {
                            self.lowering
                                .enum_layout_for_instance(*def_id, &args, init_span)
                        };
                        if let Some(layout) = layout {
                            storage_ty = Some(self.lowering.nominal_enum_ty(&layout));
                        }
                    }
                }
            }
        }

        let implicit_ty = init
            .as_deref()
            .map(|expr| self.implicit_local_init_ty(expr))
            .transpose()?;
        let local_ty = storage_ty
            .as_ref()
            .or(implicit_ty.as_ref())
            .ok_or_else(|| fp_core::error::Error::from("local declaration has no type"))?;
        let mut decl = self.lowering.make_local_decl(local_ty, init_span);
        decl.local_info = mir::LocalInfo::User(());

        if let hir::PatKind::Binding { mutable, .. } = &pat.kind {
            if *mutable {
                decl.mutability = mir::Mutability::Mut;
            }
        }

        let local_id = self.push_local(decl);
        self.bind_pattern(pat, local_id, Some(local_ty));

        if let Some(init_expr) = init {
            self.update_null_tracking(
                mir::Place::from_local(local_id),
                declared_ty.as_ref(),
                init_expr,
            );
            self.lower_assignment(
                local_id,
                declared_ty.as_ref(),
                annotated_enum_def,
                init_expr,
            )?;
        }

        Ok(())
    }

    fn lower_loop_expr(
        &mut self,
        span: Span,
        block: &hir::Block,
        destination: Option<LoopDestination>,
        break_value_allowed: bool,
    ) -> Result<()> {
        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_header = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: header_block,
            },
        };
        self.set_current_terminator(goto_header);

        self.current_block = header_block;
        let goto_body = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: body_block },
        };
        self.set_current_terminator(goto_body);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: header_block,
            break_destination: context_destination,
            break_value_allowed,
            defer_scope_depth: self.defer_scopes.len(),
        });

        self.current_block = body_block;
        self.lower_block_as_statement(block)?;

        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: header_block,
                },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        Ok(())
    }

    fn lower_while_expr(
        &mut self,
        span: Span,
        cond: &hir::Expr,
        block: &hir::Block,
        destination: Option<LoopDestination>,
    ) -> Result<()> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_cond = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: cond_block },
        };
        self.set_current_terminator(goto_cond);

        self.current_block = cond_block;
        let bool_ty = Ty { kind: TyKind::Bool };
        let cond_operand = self.lower_condition_operand(cond)?;
        let switch = mir::Terminator {
            source_info: cond.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: cond_operand,
                switch_ty: bool_ty.clone(),
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![body_block],
                    otherwise: exit_block,
                },
            },
        };
        self.set_current_terminator(switch);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: cond_block,
            break_destination: context_destination,
            break_value_allowed: false,
            defer_scope_depth: self.defer_scopes.len(),
        });

        self.current_block = body_block;
        self.lower_block(block)?;
        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto { target: cond_block },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        if let Some(dest) = destination.as_ref() {
            let assign_unit = mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    dest.place.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            };
            self.push_statement(assign_unit);
            if dest.place.projection.is_empty() {
                self.locals[dest.place.local as usize].ty = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
            }
        }

        Ok(())
    }

    fn lower_break(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`break` used outside of a loop");
                return Ok(());
            }
        };
        let break_value = if let Some(value_expr) = value {
            let expected =
                context
                    .break_destination
                    .as_ref()
                    .and_then(|dest| match &dest.ty.kind {
                        TyKind::Tuple(elements) if elements.is_empty() => None,
                        TyKind::Error(_) => None,
                        _ => Some(&dest.ty),
                    });
            let (temp_place, temp_ty) = if let Some(expected_ty) = expected {
                let temp_local = self.allocate_temp(expected_ty.clone(), value_expr.span);
                let temp_place = mir::Place::from_local(temp_local);
                self.lower_expr_into_place(value_expr, temp_place.clone(), expected_ty)?;
                (temp_place, expected_ty.clone())
            } else {
                let operand = self.lower_operand(value_expr, None)?;
                let temp_local = self.allocate_temp(operand.ty.clone(), value_expr.span);
                let temp_place = mir::Place::from_local(temp_local);
                self.push_statement(mir::Statement {
                    source_info: value_expr.span,
                    kind: mir::StatementKind::Assign(
                        temp_place.clone(),
                        mir::Rvalue::Use(operand.operand),
                    ),
                });
                (temp_place, operand.ty)
            };
            Some((temp_place, temp_ty))
        } else {
            None
        };
        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(context.defer_scope_depth)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        if let Some((value_place, value_ty)) = break_value {
            if !context.break_value_allowed {
                self.lowering.emit_error(
                    span,
                    "`break` with a value is only supported inside `loop` expressions",
                );
            } else if let Some(dest) = context.break_destination.as_ref() {
                let statement = mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        dest.place.clone(),
                        mir::Rvalue::Use(mir::Operand::Copy(value_place)),
                    ),
                };
                self.push_statement(statement);
                if dest.place.projection.is_empty() {
                    self.locals[dest.place.local as usize].ty = value_ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(&value_ty) {
                        self.local_structs.insert(dest.place.local, struct_def);
                    }
                }
            } else {
                self.lowering.emit_error(
                    span,
                    "`break` with a value requires the surrounding loop to produce a value",
                );
            }
        } else if context.break_value_allowed {
            if let Some(dest) = context.break_destination.as_ref() {
                match &dest.ty.kind {
                    TyKind::Tuple(elements) if elements.is_empty() => {}
                    TyKind::Never => {}
                    _ => {
                        self.lowering.emit_error(
                            span,
                            "`break` without a value in a value-producing loop is not supported",
                        );
                    }
                }
            }
        }

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.break_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    fn lower_continue(&mut self, span: Span) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`continue` used outside of a loop");
                return Ok(());
            }
        };
        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(context.defer_scope_depth)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.continue_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    fn lower_return(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let return_place = mir::Place::from_local(0);
        let return_value = if let Some(value_expr) = value {
            let temp_local = self.allocate_temp(return_ty.clone(), value_expr.span);
            let temp_place = mir::Place::from_local(temp_local);
            self.lower_expr_into_place(value_expr, temp_place.clone(), &return_ty)?;
            Some(temp_place)
        } else {
            None
        };

        self.control_flow_emitted = false;
        self.unwind_defer_scopes_to(0)?;
        if self.control_flow_emitted {
            return Ok(());
        }

        if let Some(value_place) = return_value {
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    return_place.clone(),
                    mir::Rvalue::Use(mir::Operand::Copy(value_place)),
                ),
            });
        } else {
            if !matches!(return_ty.kind, TyKind::Tuple(ref elems) if elems.is_empty()) {
                self.lowering
                    .emit_error(span, "`return` without a value requires unit return type");
            }
        }

        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Return,
        };
        self.set_current_terminator(terminator);
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> Result<()> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => self.lower_local(local),
            hir::StmtKind::Item(item) => self.lower_inner_item(item),
            hir::StmtKind::Semi(expr) | hir::StmtKind::Expr(expr) => {
                self.lower_expr_statement(expr)
            }
        }
    }

    fn lower_tail_expr(&mut self, expr: &hir::Expr) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let place = mir::Place::from_local(0);
        if MirLowering::is_unit_ty(&return_ty) {
            self.lower_expr_as_statement(expr)?;
            self.push_statement(mir::Statement {
                source_info: expr.span,
                kind: mir::StatementKind::Assign(
                    place,
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            });
            Ok(())
        } else {
            self.lower_expr_into_place(expr, place, &return_ty)
        }
    }

    fn lower_match_expr(
        &mut self,
        span: Span,
        scrutinee: &hir::Expr,
        arms: &[hir::MatchArm],
        destination: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let scrutinee_info = self.lower_operand(scrutinee, None)?;
        let scrutinee_local = self.allocate_temp(scrutinee_info.ty.clone(), scrutinee.span);
        let scrutinee_place = mir::Place::from_local(scrutinee_local);
        self.push_statement(mir::Statement {
            source_info: scrutinee.span,
            kind: mir::StatementKind::Assign(
                scrutinee_place.clone(),
                mir::Rvalue::Use(scrutinee_info.operand),
            ),
        });

        let continue_block = self.new_block();
        let mut next_block = self.current_block;
        let mut fallthrough_block = None;

        for (idx, arm) in arms.iter().enumerate() {
            let body_block = self.new_block();
            let is_last = idx == arms.len() - 1;
            let mut next_arm_block = self.new_block();
            let always_matches = self.pattern_always_matches(&arm.pat);
            if is_last && always_matches {
                next_arm_block = continue_block;
            } else if is_last {
                fallthrough_block = Some(next_arm_block);
            }

            self.current_block = next_block;
            if always_matches {
                self.set_current_terminator(mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::Goto { target: body_block },
                });
            } else {
                let cond_operand = self.lower_match_condition(
                    &arm.pat,
                    &scrutinee_place,
                    &scrutinee_info.ty,
                    span,
                )?;
                let switch = mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![body_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(switch);
            }

            self.current_block = body_block;
            self.match_binding_undo_log = Some(Vec::new());
            self.bind_match_pattern(&arm.pat, &scrutinee_place, &scrutinee_info.ty, span);
            let undo_log = self.match_binding_undo_log.take().unwrap_or_default();

            if let Some(guard) = &arm.guard {
                let guard_operand = self.lower_condition_operand(guard)?;
                let guard_block = self.new_block();
                let guard_switch = mir::Terminator {
                    source_info: guard.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: guard_operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![guard_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(guard_switch);
                self.current_block = guard_block;
            }

            self.lower_expr_into_place(&arm.body, destination.clone(), expected_ty)?;
            self.set_current_terminator(mir::Terminator {
                source_info: arm.body.span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
            for entry in undo_log.into_iter().rev() {
                match entry {
                    MatchBindingUndo::LocalMap(hir_id, Some(prev)) => {
                        self.local_map.insert(hir_id, prev);
                    }
                    MatchBindingUndo::LocalMap(hir_id, None) => {
                        self.local_map.remove(&hir_id);
                    }
                    MatchBindingUndo::Fallback(name, Some(prev)) => {
                        self.fallback_locals.insert(name, prev);
                    }
                    MatchBindingUndo::Fallback(name, None) => {
                        self.fallback_locals.remove(&name);
                    }
                }
            }

            next_block = next_arm_block;
        }

        if let Some(fallthrough) = fallthrough_block {
            self.current_block = fallthrough;
            self.lowering
                .emit_warning(span, "match arms did not cover all cases");
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    destination.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            });
            self.set_current_terminator(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = continue_block;
        Ok(())
    }

    fn pattern_always_matches(&self, pat: &hir::Pat) -> bool {
        match &pat.kind {
            hir::PatKind::Wild | hir::PatKind::Binding { .. } => true,
            hir::PatKind::Tuple(items) => {
                items.iter().all(|item| self.pattern_always_matches(item))
            }
            hir::PatKind::Struct(_, fields, _) => fields
                .iter()
                .all(|field| self.pattern_always_matches(&field.pat)),
            _ => false,
        }
    }

    fn lower_match_condition(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Result<mir::Operand> {
        if let hir::PatKind::Tuple(items) = &pat.kind {
            let mut tuple_place = scrutinee_place.clone();
            let mut tuple_ty = scrutinee_ty.clone();
            if matches!(tuple_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                tuple_place.projection.push(mir::PlaceElem::Deref);
                tuple_ty = match &tuple_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => tuple_ty,
                };
            }

            let TyKind::Tuple(elem_tys) = &tuple_ty.kind else {
                self.lowering.emit_warning(
                    span,
                    "tuple pattern match requires tuple scrutinee; treating as non-matching",
                );
                return Ok(self.constant_bool_operand(false, span).operand);
            };

            if items.len() != elem_tys.len() {
                self.lowering.emit_warning(
                    span,
                    "tuple pattern length mismatch; treating as non-matching",
                );
                return Ok(self.constant_bool_operand(false, span).operand);
            }

            let mut combined: Option<mir::Operand> = None;
            for (index, item) in items.iter().enumerate() {
                match &item.kind {
                    hir::PatKind::Lit(lit) => {
                        let (literal, ty) = self.lower_literal(lit, None);
                        let mut field_place = tuple_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(index, (*elem_tys[index]).clone()));
                        let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                        let eq_place = mir::Place::from_local(eq_temp);
                        self.push_statement(mir::Statement {
                            source_info: span,
                            kind: mir::StatementKind::Assign(
                                eq_place.clone(),
                                mir::Rvalue::BinaryOp(
                                    mir::BinOp::Eq,
                                    mir::Operand::Copy(field_place),
                                    mir::Operand::Constant(mir::Constant {
                                        span,
                                        ty,
                                        user_ty: None,
                                        literal,
                                    }),
                                ),
                            ),
                        });
                        let eq_operand = mir::Operand::Copy(eq_place);
                        combined = Some(match combined {
                            None => eq_operand,
                            Some(existing) => {
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            existing,
                                            eq_operand,
                                        ),
                                    ),
                                });
                                mir::Operand::Copy(and_place)
                            }
                        });
                    }
                    hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                    _ => {
                        self.lowering.emit_warning(
                            span,
                            "tuple pattern element not supported; treating as non-matching",
                        );
                        return Ok(self.constant_bool_operand(false, span).operand);
                    }
                }
            }

            return Ok(combined.unwrap_or_else(|| self.constant_bool_operand(true, span).operand));
        }
        if let hir::PatKind::Struct(path, fields, _) = &pat.kind {
            if let Some(variant) = self.enum_variant_info_from_path(path) {
                let layout = self
                    .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                    .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
                if let Some(layout) = layout {
                    let mut base_place = scrutinee_place.clone();
                    if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                        base_place.projection.push(mir::PlaceElem::Deref);
                    }

                    let mut tag_place = base_place.clone();
                    tag_place
                        .projection
                        .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                    let tag_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                    let tag_place_out = mir::Place::from_local(tag_temp);
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            tag_place_out.clone(),
                            mir::Rvalue::BinaryOp(
                                mir::BinOp::Eq,
                                mir::Operand::Copy(tag_place),
                                mir::Operand::Constant(mir::Constant {
                                    span,
                                    ty: layout.tag_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Int(variant.discriminant),
                                }),
                            ),
                        ),
                    });
                    let mut combined = mir::Operand::Copy(tag_place_out);

                    let payload_tys = self.variant_payloads_from_layout_or_ty(
                        &layout,
                        &variant,
                        scrutinee_ty,
                        span,
                    );
                    for (idx, field) in fields.iter().enumerate() {
                        if idx >= payload_tys.len() {
                            break;
                        }
                        match &field.pat.kind {
                            hir::PatKind::Lit(lit) => {
                                let (literal, ty) = self.lower_literal(lit, None);
                                let field_ty = payload_tys[idx].clone();
                                let mut field_place = base_place.clone();
                                field_place
                                    .projection
                                    .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                                let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let eq_place = mir::Place::from_local(eq_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        eq_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::Eq,
                                            mir::Operand::Copy(field_place),
                                            mir::Operand::Constant(mir::Constant {
                                                span,
                                                ty,
                                                user_ty: None,
                                                literal,
                                            }),
                                        ),
                                    ),
                                });
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            combined,
                                            mir::Operand::Copy(eq_place),
                                        ),
                                    ),
                                });
                                combined = mir::Operand::Copy(and_place);
                            }
                            hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                            _ => {
                                self.lowering.emit_warning(
                                    span,
                                    "enum struct pattern field not supported; ignoring",
                                );
                            }
                        }
                    }

                    return Ok(combined);
                }
            }

            let mut base_place = scrutinee_place.clone();
            let mut base_ty = scrutinee_ty.clone();
            if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                base_place.projection.push(mir::PlaceElem::Deref);
                base_ty = match &base_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => base_ty,
                };
            }
            if let Some(struct_def) = self.struct_def_from_ty(&base_ty) {
                let mut combined: Option<mir::Operand> = None;
                for field in fields {
                    match &field.pat.kind {
                        hir::PatKind::Lit(lit) => {
                            let Some((field_index, field_info)) = self.lowering.struct_field(
                                struct_def,
                                &base_ty,
                                field.name.as_str(),
                                span,
                            ) else {
                                self.lowering.emit_warning(
                                    span,
                                    format!(
                                        "struct pattern field `{}` not found; treating as non-matching",
                                        field.name
                                    ),
                                );
                                return Ok(self.constant_bool_operand(false, span).operand);
                            };
                            let (literal, ty) = self.lower_literal(lit, None);
                            let mut field_place = base_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                            let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                            let eq_place = mir::Place::from_local(eq_temp);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    eq_place.clone(),
                                    mir::Rvalue::BinaryOp(
                                        mir::BinOp::Eq,
                                        mir::Operand::Copy(field_place),
                                        mir::Operand::Constant(mir::Constant {
                                            span,
                                            ty,
                                            user_ty: None,
                                            literal,
                                        }),
                                    ),
                                ),
                            });
                            let eq_operand = mir::Operand::Copy(eq_place);
                            combined = Some(match combined {
                                None => eq_operand,
                                Some(existing) => {
                                    let and_temp =
                                        self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                    let and_place = mir::Place::from_local(and_temp);
                                    self.push_statement(mir::Statement {
                                        source_info: span,
                                        kind: mir::StatementKind::Assign(
                                            and_place.clone(),
                                            mir::Rvalue::BinaryOp(
                                                mir::BinOp::And,
                                                existing,
                                                eq_operand,
                                            ),
                                        ),
                                    });
                                    mir::Operand::Copy(and_place)
                                }
                            });
                        }
                        hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                        _ => {
                            self.lowering.emit_warning(
                                span,
                                "struct pattern field not supported; treating as non-matching",
                            );
                            return Ok(self.constant_bool_operand(false, span).operand);
                        }
                    }
                }

                return Ok(
                    combined.unwrap_or_else(|| self.constant_bool_operand(true, span).operand)
                );
            }
        }

        if let hir::PatKind::TupleStruct(path, parts) = &pat.kind {
            if let Some(variant) = self.enum_variant_info_from_path(path) {
                let layout = self
                    .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                    .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
                if let Some(layout) = layout {
                    let mut base_place = scrutinee_place.clone();
                    if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                        base_place.projection.push(mir::PlaceElem::Deref);
                    }

                    let mut tag_place = base_place.clone();
                    tag_place
                        .projection
                        .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                    let tag_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                    let tag_place_out = mir::Place::from_local(tag_temp);
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            tag_place_out.clone(),
                            mir::Rvalue::BinaryOp(
                                mir::BinOp::Eq,
                                mir::Operand::Copy(tag_place),
                                mir::Operand::Constant(mir::Constant {
                                    span,
                                    ty: layout.tag_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Int(variant.discriminant),
                                }),
                            ),
                        ),
                    });
                    let mut combined = mir::Operand::Copy(tag_place_out);

                    let payload_tys = self.variant_payloads_from_layout_or_ty(
                        &layout,
                        &variant,
                        scrutinee_ty,
                        span,
                    );
                    for (idx, part) in parts.iter().enumerate() {
                        if idx >= payload_tys.len() {
                            break;
                        }
                        match &part.kind {
                            hir::PatKind::Lit(lit) => {
                                let (literal, ty) = self.lower_literal(lit, None);
                                let field_ty = payload_tys[idx].clone();
                                let mut field_place = base_place.clone();
                                field_place
                                    .projection
                                    .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                                let eq_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let eq_place = mir::Place::from_local(eq_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        eq_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::Eq,
                                            mir::Operand::Copy(field_place),
                                            mir::Operand::Constant(mir::Constant {
                                                span,
                                                ty,
                                                user_ty: None,
                                                literal,
                                            }),
                                        ),
                                    ),
                                });
                                let and_temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
                                let and_place = mir::Place::from_local(and_temp);
                                self.push_statement(mir::Statement {
                                    source_info: span,
                                    kind: mir::StatementKind::Assign(
                                        and_place.clone(),
                                        mir::Rvalue::BinaryOp(
                                            mir::BinOp::And,
                                            combined,
                                            mir::Operand::Copy(eq_place),
                                        ),
                                    ),
                                });
                                combined = mir::Operand::Copy(and_place);
                            }
                            hir::PatKind::Wild | hir::PatKind::Binding { .. } => {}
                            _ => {
                                self.lowering.emit_warning(
                                    span,
                                    "tuple-struct pattern element not supported; ignoring",
                                );
                            }
                        }
                    }

                    return Ok(combined);
                }
            }
        }

        let literal = match &pat.kind {
            hir::PatKind::Lit(lit) => {
                let (literal, ty) = self.lower_literal(lit, None);
                mir::Operand::Constant(mir::Constant {
                    span,
                    ty,
                    user_ty: None,
                    literal,
                })
            }
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => {
                if let Some(variant) = self.enum_variant_info_from_path(path) {
                    let tag_ty = self
                        .enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                        .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span))
                        .ok_or_else(|| {
                            crate::error::optimization_error(
                                "enum pattern has no concrete MIR layout",
                            )
                        })?
                        .tag_ty;
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: tag_ty,
                        user_ty: None,
                        literal: mir::ConstantKind::Int(variant.discriminant),
                    })
                } else {
                    let expr = hir::Expr {
                        hir_id: pat.hir_id,
                        kind: hir::ExprKind::Path(path.clone()),
                        span,
                    };
                    let operand = self.lower_operand(&expr, None)?;
                    operand.operand
                }
            }
            _ => {
                self.lowering.emit_warning(
                    span,
                    "unsupported pattern in match condition; treating as non-matching",
                );
                self.constant_bool_operand(false, span).operand
            }
        };

        let scrutinee_operand = if matches!(
            pat.kind,
            hir::PatKind::Variant(_)
                | hir::PatKind::Struct(_, _, _)
                | hir::PatKind::TupleStruct(_, _)
        ) {
            let layout = match &pat.kind {
                hir::PatKind::Variant(path)
                | hir::PatKind::Struct(path, _, _)
                | hir::PatKind::TupleStruct(path, _) => self
                    .enum_variant_info_from_path(path)
                    .and_then(|variant| {
                        self.enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                            .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span))
                    })
                    .or_else(|| self.enum_layout_for_ty(scrutinee_ty, span)),
                _ => self.enum_layout_for_ty(scrutinee_ty, span),
            };
            if let Some(layout) = layout {
                let mut tag_place = scrutinee_place.clone();
                if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                    tag_place.projection.push(mir::PlaceElem::Deref);
                }
                tag_place
                    .projection
                    .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                mir::Operand::Copy(tag_place)
            } else {
                mir::Operand::Copy(scrutinee_place.clone())
            }
        } else {
            mir::Operand::Copy(scrutinee_place.clone())
        };

        let temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
        let place = mir::Place::from_local(temp);
        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::BinaryOp(mir::BinOp::Eq, scrutinee_operand, literal),
            ),
        };
        self.push_statement(assign);
        Ok(mir::Operand::Copy(place))
    }

    fn bind_match_pattern(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        if let hir::PatKind::Tuple(parts) = &pat.kind {
            let mut base_place = scrutinee_place.clone();
            let mut base_ty = scrutinee_ty.clone();
            if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                base_place.projection.push(mir::PlaceElem::Deref);
                base_ty = match &base_ty.kind {
                    TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                    TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                    _ => base_ty,
                };
            }
            if let TyKind::Tuple(elem_tys) = &base_ty.kind {
                if parts.len() == elem_tys.len() {
                    for (idx, part) in parts.iter().enumerate() {
                        let field_ty = (*elem_tys[idx]).clone();
                        let mut field_place = base_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(idx, field_ty.clone()));
                        self.bind_match_pattern(part, &field_place, &field_ty, span);
                    }
                    return;
                }
            }
        }
        if let hir::PatKind::Struct(path, fields, _) = &pat.kind {
            if self.enum_variant_info_from_path(path).is_none() {
                let mut base_place = scrutinee_place.clone();
                let mut base_ty = scrutinee_ty.clone();
                if matches!(base_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                    base_place.projection.push(mir::PlaceElem::Deref);
                    base_ty = match &base_ty.kind {
                        TyKind::Ref(_, inner, _) => (*inner.as_ref()).clone(),
                        TyKind::RawPtr(type_and_mut) => (*type_and_mut.ty).clone(),
                        _ => base_ty,
                    };
                }
                if let Some(def_id) = self.struct_def_from_ty(&base_ty) {
                    for field in fields {
                        let Some((field_index, field_info)) =
                            self.lowering
                                .struct_field(def_id, &base_ty, field.name.as_str(), span)
                        else {
                            continue;
                        };
                        let mut field_place = base_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                        self.bind_match_pattern(&field.pat, &field_place, &field_info.ty, span);
                    }
                    return;
                }
            }
        }
        let layout = match &pat.kind {
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => self
                .enum_variant_info_from_path(path)
                .and_then(|variant| {
                    self.enum_layout_for_variant(&variant, Some(scrutinee_ty), span)
                        .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span))
                })
                .or_else(|| self.enum_layout_for_ty(scrutinee_ty, span)),
            _ => self.enum_layout_for_ty(scrutinee_ty, span),
        };
        if let Some(layout) = layout {
            let mut scrutinee_place = scrutinee_place.clone();
            if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                scrutinee_place.projection.push(mir::PlaceElem::Deref);
            }
            match &pat.kind {
                hir::PatKind::Variant(path) => {
                    if self.enum_variant_info_from_path(path).is_some() {
                        return;
                    }
                }
                hir::PatKind::TupleStruct(path, parts) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = self.variant_payloads_from_layout_or_ty(
                            &layout,
                            &variant,
                            scrutinee_ty,
                            span,
                        );
                        for (idx, part) in parts.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                            // The projection above keeps the flattened
                            // struct shape (matching actual tuple-based
                            // storage), but the *bound local*'s declared
                            // type should be nominal when this payload is a
                            // registered struct — otherwise callers like
                            // `real_indexable_struct_def_id` can't
                            // recognize e.g. a `Vec<Field>` match-bound
                            // payload as indexable.
                            let bound_ty = self.lowering.nominalize_struct_ty(field_ty);
                            self.bind_match_pattern(part, &field_place, &bound_ty, span);
                        }
                        return;
                    }
                }
                hir::PatKind::Struct(path, fields, _) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = self.variant_payloads_from_layout_or_ty(
                            &layout,
                            &variant,
                            scrutinee_ty,
                            span,
                        );
                        for (idx, field) in fields.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
                            let bound_ty = self.lowering.nominalize_struct_ty(field_ty);
                            self.bind_match_pattern(&field.pat, &field_place, &bound_ty, span);
                        }
                        return;
                    }
                }
                _ => {}
            }
        } else if let TyKind::Tuple(fields) = &scrutinee_ty.kind {
            // A generic enum's payload is sometimes represented, by this
            // point, as a plain `(discriminant, ...payload)` tuple rather
            // than a `TyKind::Adt` the layout lookup above can recognize
            // (e.g. inside a monomorphized generic method body, where the
            // scrutinee's registered local type is already the flattened
            // tuple form) — `enum_layout_for_variant_ty`/`enum_layout_for_ty`
            // only match `Ref`/`RawPtr`/`Adt`/`Opaque`, so `layout` above is
            // `None` even though the pattern genuinely is an enum-variant
            // destructure. Falling through to the generic tuple-pattern
            // case below would incorrectly bind each part to the *whole*
            // enum value/type instead of projecting into its payload
            // field — extract payload types directly from the tuple shape
            // instead (field 0 is always the discriminant; this mirrors
            // `variant_payloads_from_layout_or_ty`'s own `TyKind::Tuple`
            // fallback for exactly this situation).
            match &pat.kind {
                hir::PatKind::TupleStruct(path, parts)
                    if self.enum_variant_info_from_path(path).is_some() =>
                {
                    let variant = self.enum_variant_info_from_path(path).expect("checked above");
                    let substituted_payloads = self.payload_types_from_type_substs(&variant, span);
                    for (idx, part) in parts.iter().enumerate() {
                        let field_idx = idx + 1;
                        let field_ty = match substituted_payloads.as_ref() {
                            Some(payloads) if idx < payloads.len() => payloads[idx].clone(),
                            _ if field_idx < fields.len() => (*fields[field_idx]).clone(),
                            _ => break,
                        };
                        let mut field_place = scrutinee_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_idx, field_ty.clone()));
                        self.bind_match_pattern(part, &field_place, &field_ty, span);
                    }
                    return;
                }
                hir::PatKind::Struct(path, pat_fields, _)
                    if self.enum_variant_info_from_path(path).is_some() =>
                {
                    let variant = self.enum_variant_info_from_path(path).expect("checked above");
                    let substituted_payloads = self.payload_types_from_type_substs(&variant, span);
                    for (idx, field) in pat_fields.iter().enumerate() {
                        let field_idx = idx + 1;
                        let field_ty = match substituted_payloads.as_ref() {
                            Some(payloads) if idx < payloads.len() => payloads[idx].clone(),
                            _ if field_idx < fields.len() => (*fields[field_idx]).clone(),
                            _ => break,
                        };
                        let mut field_place = scrutinee_place.clone();
                        field_place
                            .projection
                            .push(mir::PlaceElem::Field(field_idx, field_ty.clone()));
                        self.bind_match_pattern(&field.pat, &field_place, &field_ty, span);
                    }
                    return;
                }
                _ => {}
            }
        }
        match &pat.kind {
            hir::PatKind::Binding { name, .. } => {
                self.bind_match_binding(name, pat, scrutinee_place, scrutinee_ty, span);
            }
            hir::PatKind::Tuple(parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::TupleStruct(_, parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::Struct(_, fields, _) => {
                for field in fields {
                    self.bind_match_pattern(&field.pat, scrutinee_place, scrutinee_ty, span);
                }
            }
            _ => {}
        }
    }

    fn bind_match_binding(
        &mut self,
        name: &hir::Symbol,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        let mut decl = self.lowering.make_local_decl(scrutinee_ty, span);
        decl.mutability = mir::Mutability::Not;
        let local_id = self.push_local(decl);
        let place = mir::Place::from_local(local_id);
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::Use(mir::Operand::Copy(scrutinee_place.clone())),
            ),
        });
        if let Some(log) = self.match_binding_undo_log.as_mut() {
            log.push(MatchBindingUndo::LocalMap(
                pat.hir_id,
                self.local_map.get(&pat.hir_id).copied(),
            ));
            log.push(MatchBindingUndo::Fallback(
                name.as_str().to_string(),
                self.fallback_locals.get(name.as_str()).copied(),
            ));
        }
        self.local_map.insert(pat.hir_id, local_id);
        self.fallback_locals
            .insert(name.as_str().to_string(), local_id);
        if let Some(def_id) = self.struct_def_from_ty(scrutinee_ty) {
            self.local_structs.insert(local_id, def_id);
        }
    }

    fn lower_local(&mut self, local: &hir::Local) -> Result<()> {
        let init_span = local
            .init
            .as_ref()
            .map(|expr| expr.span)
            .unwrap_or(self.span);

        let mut declared_ty = local
            .ty
            .as_ref()
            .filter(|ty_expr| {
                !matches!(
                    ty_expr.kind,
                    hir::TypeExprKind::Infer | hir::TypeExprKind::Error
                )
            })
            .map(|ty_expr| self.lower_type_expr(ty_expr));
        let annotated_enum_def = local.ty.as_ref().and_then(|ty_expr| {
            let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
                return None;
            };
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self.lowering.enum_defs.contains_key(def_id) {
                    return Some(*def_id);
                }
            }
            let name = path.segments.last()?.name.as_str();
            self.lowering
                .enum_defs
                .values()
                .find(|enm| enm.name == name)
                .map(|enm| enm.def_id)
        });
        if let Some(ty_expr) = local.ty.as_ref() {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if self.lowering.enum_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| self.lowering.lower_generic_args(Some(args), init_span))
                            .unwrap_or_default();
                        let layout = if args.is_empty() {
                            self.lowering.enum_layout_for_def(*def_id, init_span)
                        } else {
                            self.lowering
                                .enum_layout_for_instance(*def_id, &args, init_span)
                        };
                        if let Some(layout) = layout {
                            declared_ty = Some(self.lowering.nominal_enum_ty(&layout));
                        }
                    }
                }
            }
        }

        let implicit_ty = if declared_ty.is_none() {
            local
                .init
                .as_ref()
                .map(|expr| self.implicit_local_init_ty(expr))
                .transpose()?
        } else {
            None
        };
        let local_ty = declared_ty
            .as_ref()
            .or(implicit_ty.as_ref())
            .ok_or_else(|| fp_core::error::Error::from("local declaration has no type"))?;
        let mut decl = self.lowering.make_local_decl(local_ty, init_span);
        decl.local_info = mir::LocalInfo::User(());

        if let hir::PatKind::Binding { mutable, .. } = &local.pat.kind {
            if *mutable {
                decl.mutability = mir::Mutability::Mut;
            }
        }

        let local_id = self.push_local(decl);
        self.bind_pattern(&local.pat, local_id, Some(local_ty));

        if let Some(init_expr) = &local.init {
            self.update_null_tracking(
                mir::Place::from_local(local_id),
                declared_ty.as_ref(),
                init_expr,
            );
            self.lower_assignment(
                local_id,
                declared_ty.as_ref(),
                annotated_enum_def,
                init_expr,
            )?;
        }

        Ok(())
    }

    fn implicit_local_init_ty(&self, expr: &hir::Expr) -> Result<Ty> {
        let ty = self
            .lowering
            .typeck_exprs
            .get(&expr.hir_id)
            .cloned()
            .ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "missing HIR type for local initializer {}",
                    expr.hir_id
                ))
            })?;
        // Same concern as `lower_type_expr`'s typeck-cache check: the type
        // checker's cached type for this initializer expression comes from
        // type-checking the generic body once, abstractly — inside a
        // monomorphized specialization (`type_substs` populated), a bare
        // generic-param reference in that cached type (e.g. `*mut T`) is
        // unresolved and must be substituted, not returned as-is.
        if !self.type_substs.is_empty() && self.lowering.has_unresolved_ty(&ty) {
            return Ok(self.lowering.substitute_ty(&ty, &self.type_substs));
        }
        Ok(ty)
    }

    fn lower_inner_item(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Struct(def) => {
                self.lowering.register_struct(
                    &self.lowering.hir_def_paths.clone(),
                    item.def_id,
                    def,
                    item.span,
                );
            }
            hir::ItemKind::Enum(enm) => {
                self.lowering.register_enum(
                    &self.lowering.hir_def_paths.clone(),
                    item.def_id,
                    enm,
                    item.span,
                );
            }
            hir::ItemKind::Const(konst) => {
                self.lowering.register_const_value(item.def_id, konst);
                self.const_items.insert(item.def_id, konst.clone());
                // Emit a Static/ExecutableConst MIR item for every
                // non-unit const so cross-references between consts
                // work correctly in the interpreter and native codegen.
                let ty = self.lowering.lower_type_expr(&konst.ty);
                if !MirLowering::is_unit_ty(&ty) {
                    let mir_item = self.lowering.lower_const(item.def_id, konst)?;
                    self.lowering.extra_items.push(mir_item);
                }
            }
            hir::ItemKind::Impl(impl_block) => {
                self.lowering.lower_impl(item, impl_block, None)?;
            }
            hir::ItemKind::Function(function) => {
                let (mir_item, body_id, body) = self.lowering.lower_function(item, function)?;
                self.lowering.extra_items.push(mir_item);
                self.lowering.extra_bodies.push((body_id, body));
            }
            hir::ItemKind::Query(_) => {}
            hir::ItemKind::Trait(_) => {}
            hir::ItemKind::Expr(expr) => {
                self.lower_expr_statement(expr)?;
            }
        }
        Ok(())
    }

    fn lower_expr_statement(&mut self, expr: &hir::Expr) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Let(pat, ty, init) => {
                self.lower_let_expr(pat, ty, init, expr.span)?;
            }
            hir::ExprKind::Block(block) => {
                self.lower_block_as_statement(block)?;
            }
            hir::ExprKind::Assign(place_expr, value_expr) => {
                // `x[i] = v` where `x`'s type has a real `index_set`
                // method (e.g. `Vec<T>`'s `Index`-trait-style impl) isn't a
                // real addressable MIR place at all — the struct's memory
                // is behind a raw pointer field, reachable only through a
                // method call, not a field/array projection — so it can't
                // go through `lower_place` (see its `Index` projection
                // case bailing out for exactly this reason). Detect it
                // here, before `lower_place` gets a chance to report
                // "assignment target is not addressable", and dispatch to
                // a real method call instead. `typeck_exprs` gives the
                // receiver's type without lowering it (no side effects
                // from lowering something we might not use).
                if let hir::ExprKind::Index(receiver, index) = &place_expr.kind {
                    let receiver_ty = self.lowering.typeck_exprs.get(&receiver.hir_id).cloned();
                    if let Some(struct_def_id) = receiver_ty
                        .as_ref()
                        .and_then(|ty| self.real_indexable_struct_def_id(ty))
                    {
                        let unit_ty = Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        };
                        let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.call_real_method_into_place(
                            struct_def_id,
                            "index_set",
                            receiver,
                            &[index, value_expr],
                            place,
                            Some(&unit_ty),
                            expr.span,
                        )?;
                        return Ok(());
                    }
                }
                let place_info = match self.lower_place(place_expr)? {
                    Some(info) => info,
                    None => {
                        self.lowering
                            .emit_error(place_expr.span, "assignment target is not addressable");
                        return Ok(());
                    }
                };

                self.update_null_tracking(
                    place_info.place.clone(),
                    Some(&place_info.ty),
                    value_expr,
                );
                let expected_ty = place_info.ty.clone();
                self.lower_expr_into_place(value_expr, place_info.place, &expected_ty)?;
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, None)?;
            }
            hir::ExprKind::Loop(block) => {
                let temp_unit = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
                let temp_local = self.allocate_temp(temp_unit.clone(), expr.span);
                let destination = LoopDestination {
                    place: mir::Place::from_local(temp_local),
                    ty: temp_unit,
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                self.lower_if_statement(expr.span, cond, then_expr, else_expr)?;
            }
            hir::ExprKind::While(cond, block) => {
                self.lower_while_expr(expr.span, cond, block, None)?;
            }
            hir::ExprKind::Try(expr_try) => {
                self.lower_try_expr(expr, expr_try, None, true)?;
            }
            hir::ExprKind::Break(value) => {
                self.lower_break(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Return(value) => {
                self.lower_return(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Continue => {
                self.lower_continue(expr.span)?;
            }
            _ => {
                // Evaluate then drop result
                let _ = self.lower_operand(expr, None)?;
            }
        }
        Ok(())
    }

    fn lower_expr_as_statement(&mut self, expr: &hir::Expr) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Block(block) => self.lower_block_as_statement(block),
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                self.lower_if_statement(expr.span, cond, then_expr, else_expr)
            }
            _ => self.lower_expr_statement(expr),
        }
    }

    fn lower_if_statement(
        &mut self,
        span: Span,
        cond: &hir::Expr,
        then_expr: &hir::Expr,
        else_expr: &Option<Box<hir::Expr>>,
    ) -> Result<()> {
        let bool_ty = Ty { kind: TyKind::Bool };
        let cond_operand = self.lower_condition_operand(cond)?;

        let then_block = self.new_block();
        let else_block = self.new_block();
        let continue_block = self.new_block();

        let switch = mir::Terminator {
            source_info: cond.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: cond_operand,
                switch_ty: bool_ty,
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![then_block],
                    otherwise: else_block,
                },
            },
        };
        self.set_current_terminator(switch);

        self.current_block = then_block;
        self.control_flow_emitted = false;
        self.lower_expr_as_statement(then_expr)?;
        if !self.control_flow_emitted
            && self.blocks[self.current_block as usize]
                .terminator
                .is_none()
        {
            self.set_current_terminator(mir::Terminator {
                source_info: then_expr.span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = else_block;
        if let Some(else_expr) = else_expr {
            self.control_flow_emitted = false;
            self.lower_expr_as_statement(else_expr)?;
            if !self.control_flow_emitted
                && self.blocks[self.current_block as usize]
                    .terminator
                    .is_none()
            {
                self.set_current_terminator(mir::Terminator {
                    source_info: else_expr.span,
                    kind: mir::TerminatorKind::Goto {
                        target: continue_block,
                    },
                });
            }
        } else {
            self.control_flow_emitted = false;
            self.set_current_terminator(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = continue_block;
        self.control_flow_emitted = false;
        Ok(())
    }

    fn lower_assignment(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        annotated_enum_def: Option<hir::DefId>,
        expr: &hir::Expr,
    ) -> Result<()> {
        // Coerce enum payloads into their tagged layout when assigning from a place.
        let place_info = self.lower_place(expr)?;
        if let Some(place_info) = place_info {
            if let Some(enum_def) = annotated_enum_def {
                if let Some(layout) = self.lowering.enum_layout_for_def(enum_def, expr.span) {
                    if let Some((variant, layout)) = self.enum_variant_for_payload(
                        &layout.enum_ty,
                        &place_info.ty,
                        place_info.struct_def,
                    ) {
                        self.assign_enum_variant_from_place(
                            mir::Place::from_local(local_id),
                            &variant,
                            &layout,
                            Some(&layout.enum_ty),
                            place_info.place,
                            expr.span,
                        )?;
                        self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                        return Ok(());
                    }
                }
            }
            if let Some(expected_ty) = annotated_ty {
                if let Some((variant, layout)) = self.enum_variant_for_payload(
                    expected_ty,
                    &place_info.ty,
                    place_info.struct_def,
                ) {
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        place_info.place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
            }
        }
        if let Some(expected_ty) = annotated_ty {
            if self.enum_layout_for_ty(expected_ty, expr.span).is_some()
                && matches!(
                    expr.kind,
                    hir::ExprKind::Literal(_)
                        | hir::ExprKind::Index(_, _)
                        | hir::ExprKind::Cast(_, _)
                )
            {
                let value = self.lower_operand(expr, None)?;
                let payload_def = self.struct_def_from_ty(&value.ty);
                if let Some((variant, layout)) =
                    self.enum_variant_for_payload(expected_ty, &value.ty, payload_def)
                {
                    let payload_local = self.allocate_temp(value.ty.clone(), expr.span);
                    let payload_place = mir::Place::from_local(payload_local);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            payload_place.clone(),
                            mir::Rvalue::Use(value.operand),
                        ),
                    });
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        payload_place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
            }
        }
        if let hir::ExprKind::Struct(path, fields) = &expr.kind {
            self.lower_struct_literal(local_id, annotated_ty, expr.hir_id, path, fields, expr.span)
        } else if let hir::ExprKind::Call(callee, args) = &expr.kind {
            let place = mir::Place::from_local(local_id);
            let ty = annotated_ty
                .cloned()
                .unwrap_or_else(|| self.locals[local_id as usize].ty.clone());
            if let Some(info) = self.lower_call(expr, callee, args, Some((place, ty.clone())))? {
                self.locals[local_id as usize].ty = info.ty.clone();
                if let Some(def_id) = info.struct_def {
                    self.local_structs.insert(local_id, def_id);
                }
            }
            Ok(())
        } else {
            let expected_ty = annotated_ty
                .cloned()
                .or_else(|| Some(self.locals[local_id as usize].ty.clone()));
            if let (
                Some(expected_ty),
                hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. },
            ) = (expected_ty.as_ref(), &expr.kind)
            {
                if self.is_list_container(expected_ty) || self.is_map_container(expected_ty) {
                    let place = mir::Place::from_local(local_id);
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                    return Ok(());
                }
            }
            let expected_ty = annotated_ty
                .cloned()
                .or_else(|| Some(self.locals[local_id as usize].ty.clone()));
            let value = self.lower_operand(expr, expected_ty.as_ref())?;
            let statement = mir::Statement {
                source_info: expr.span,
                kind: mir::StatementKind::Assign(
                    mir::Place::from_local(local_id),
                    mir::Rvalue::Use(value.operand),
                ),
            };
            self.push_statement(statement);
            let struct_def = expected_ty
                .as_ref()
                .and_then(|ty| self.struct_def_from_ty(ty))
                .or_else(|| self.struct_def_from_ty(&value.ty));
            if let Some(def_id) = struct_def {
                self.local_structs.insert(local_id, def_id);
            }
            // Prefer the destination's already-known `expected_ty` (always
            // populated above, either from the explicit annotation or the
            // local's own prior declared type) over `value.ty` — see the
            // identical reasoning in `lower_expr_into_place`'s
            // `Literal|Path|Index|FieldAccess|ConstBlock` group: a
            // comptime-frozen constant can lose its ADT identity on the
            // way to a `mir::Constant`, and `value.ty` would then wrongly
            // clobber a local whose real, declared type is already known.
            self.locals[local_id as usize].ty =
                expected_ty.unwrap_or_else(|| value.ty.clone());
            Ok(())
        }
    }

    fn resolve_self_path(&self, path: &mut hir::Path) {
        if let Some(context) = &self.method_context {
            if let Some(first) = path.segments.first() {
                if first.name.as_str() == "Self" {
                    let mut new_segments = context.path.clone();
                    new_segments.extend(path.segments.iter().skip(1).cloned());
                    path.segments = new_segments;
                    if let Some(def_id) = context.def_id {
                        path.res = Some(hir::Res::Def(def_id));
                    }
                }
            }
        }
    }

    fn enum_variant_info_from_path(&self, path: &hir::Path) -> Option<EnumVariantInfo> {
        if let Some(hir::Res::Def(def_id)) = &path.res {
            if let Some(info) = self.lowering.enum_variants.get(def_id) {
                return Some(info.clone());
            }
            if self.lowering.generic_function_defs.contains_key(def_id) {
                return None;
            }
        }
        if matches!(path.res, Some(hir::Res::Local(_)) | Some(hir::Res::SelfTy)) {
            return None;
        }

        let name = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.lowering
            .enum_variant_names
            .get(&name)
            .or_else(|| {
                path.segments
                    .last()
                    .and_then(|seg| self.lowering.enum_variant_names.get(seg.name.as_str()))
            })
            .and_then(|def_id| self.lowering.enum_variants.get(def_id))
            .cloned()
    }

    fn enum_variant_info_from_expected(
        &self,
        path: &hir::Path,
        expected_ty: Option<&Ty>,
    ) -> Option<EnumVariantInfo> {
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty?);

        let name = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        let def_id = self
            .lowering
            .enum_variant_names
            .get(&name)
            .copied()
            .or_else(|| {
                path.segments
                    .last()
                    .and_then(|seg| self.lowering.enum_variant_names.get(seg.name.as_str()))
                    .copied()
            });

        fn expected_contains_enum(enum_def: hir::DefId, expected_ty: &Ty) -> bool {
            match &expected_ty.kind {
                TyKind::Ref(_, inner, _) => expected_contains_enum(enum_def, inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => {
                    expected_contains_enum(enum_def, type_and_mut.ty.as_ref())
                }
                TyKind::Adt(adt, substs) => {
                    if adt.did == enum_def {
                        return true;
                    }
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            if expected_contains_enum(enum_def, inner) {
                                return true;
                            }
                        }
                    }
                    false
                }
                TyKind::Opaque(def_id, substs) => {
                    if *def_id == enum_def {
                        return true;
                    }
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            if expected_contains_enum(enum_def, inner) {
                                return true;
                            }
                        }
                    }
                    false
                }
                _ => false,
            }
        }

        if let Some(def_id) = def_id {
            if let Some(info) = self.lowering.enum_variants.get(&def_id).cloned() {
                if expected_contains_enum(info.enum_def, expected_ty) {
                    return Some(info);
                }
            }
        }
        let tail = path.segments.last()?.name.as_str();

        self.enum_variant_from_expected_ty_by_name(expected_ty, tail)
    }

    fn enum_variant_from_enum_def(
        &self,
        enum_def: hir::DefId,
        variant_name: &str,
    ) -> Option<EnumVariantInfo> {
        let def = self.lowering.enum_defs.get(&enum_def)?;
        let variant = def
            .variants
            .iter()
            .find(|variant| variant.name == variant_name)?;
        self.lowering.enum_variants.get(&variant.def_id).cloned()
    }

    fn enum_variant_from_expected_ty_by_name(
        &self,
        expected_ty: &Ty,
        variant_name: &str,
    ) -> Option<EnumVariantInfo> {
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty);
        match &expected_ty.kind {
            TyKind::Ref(_, inner, _) => {
                self.enum_variant_from_expected_ty_by_name(inner.as_ref(), variant_name)
            }
            TyKind::RawPtr(type_and_mut) => {
                self.enum_variant_from_expected_ty_by_name(type_and_mut.ty.as_ref(), variant_name)
            }
            TyKind::Adt(adt, substs) => {
                if let Some(info) = self.enum_variant_from_enum_def(adt.did, variant_name) {
                    return Some(info);
                }
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        if let Some(info) =
                            self.enum_variant_from_expected_ty_by_name(inner, variant_name)
                        {
                            return Some(info);
                        }
                    }
                }
                None
            }
            TyKind::Opaque(def_id, substs) => {
                if let Some(info) = self.enum_variant_from_enum_def(*def_id, variant_name) {
                    return Some(info);
                }
                for arg in substs {
                    if let mir::ty::GenericArg::Type(inner) = arg {
                        if let Some(info) =
                            self.enum_variant_from_expected_ty_by_name(inner, variant_name)
                        {
                            return Some(info);
                        }
                    }
                }
                None
            }
            _ => self
                .enum_def_from_ty(expected_ty)
                .and_then(|enum_def| self.enum_variant_from_enum_def(enum_def, variant_name)),
        }
    }

    fn result_variant_from_expected(
        &self,
        expected_ty: &Ty,
        variant_name: &str,
    ) -> Option<EnumVariantInfo> {
        if variant_name != "Ok" && variant_name != "Err" {
            return None;
        }
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty);

        fn find_result_def(lowering: &MirLowering, ty: &Ty) -> Option<hir::DefId> {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => find_result_def(lowering, inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => find_result_def(lowering, type_and_mut.ty.as_ref()),
                TyKind::Adt(adt, substs) => {
                    let is_result = lowering
                        .enum_defs
                        .get(&adt.did)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result {
                        return Some(adt.did);
                    }
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            if let Some(found) = find_result_def(lowering, inner) {
                                return Some(found);
                            }
                        }
                    }
                    None
                }
                TyKind::Opaque(def_id, substs) => {
                    let is_result = lowering
                        .enum_defs
                        .get(def_id)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result {
                        return Some(*def_id);
                    }
                    for arg in substs {
                        if let mir::ty::GenericArg::Type(inner) = arg {
                            if let Some(found) = find_result_def(lowering, inner) {
                                return Some(found);
                            }
                        }
                    }
                    None
                }
                _ => lowering.enum_layout_for_ty(ty).and_then(|layout| {
                    lowering.enum_defs.get(&layout.def_id).and_then(|def| {
                        let is_result = def.name.as_str() == "Result"
                            || def.name.as_str().ends_with("::Result");
                        is_result.then_some(layout.def_id)
                    })
                }),
            }
        }

        let result_def = find_result_def(&self.lowering, expected_ty)?;
        self.enum_variant_from_enum_def(result_def, variant_name)
    }

    fn explicit_args_from_expected_result_ty(&self, expected_ty: &Ty) -> Option<Vec<Ty>> {
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty);
        let expected_ty = match &expected_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
            _ => expected_ty,
        };
        let (adt, substs) = match &expected_ty.kind {
            TyKind::Adt(adt, substs) => (&adt.did, substs),
            TyKind::Opaque(def_id, substs) => (def_id, substs),
            _ => {
                let layout = self.lowering.enum_layout_for_ty(expected_ty)?;
                let is_result = self
                    .lowering
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
            .lowering
            .enum_defs
            .get(adt)
            .map(|def| def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result"))
            .or_else(|| {
                self.lowering.struct_defs.get(adt).map(|def| {
                    def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                })
            })
            .unwrap_or(false);
        if !is_result {
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .lowering
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
                    let ty = self.lowering.unwrap_expr_actual_ty(ty);
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
            let ty = self.lowering.unwrap_expr_actual_ty(ty);
            args.push(ty.clone());
        }
        if args.len() < 2 {
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .lowering
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
                        .map(|ty| self.lowering.unwrap_expr_actual_ty(ty).clone())
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
                        if let Some(def) = self.lowering.enum_defs.get(&layout.def_id) {
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
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .lowering
                    .enum_defs
                    .get(&layout.def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false);
                if is_result_layout {
                    for ty in &layout.args {
                        let ty = self.lowering.unwrap_expr_actual_ty(ty);
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

    fn enum_variant_for_payload(
        &mut self,
        expected_ty: &Ty,
        payload_ty: &Ty,
        payload_def: Option<hir::DefId>,
    ) -> Option<(EnumVariantInfo, EnumLayout)> {
        let layout = self.enum_layout_for_ty(expected_ty, self.span)?;
        let enum_def = self.enum_def_from_ty(expected_ty);
        for (def_id, payloads) in &layout.variant_payloads {
            let matches = if payloads.is_empty() {
                MirLowering::is_unit_ty(payload_ty)
            } else if payloads.len() == 1 {
                payloads[0] == *payload_ty
            } else {
                let tuple_ty = Ty {
                    kind: TyKind::Tuple(payloads.iter().cloned().map(Box::new).collect()),
                };
                if tuple_ty == *payload_ty {
                    true
                } else if let Some(layout) = self.lowering.struct_layout_for_ty(payload_ty) {
                    layout.field_tys == *payloads
                } else {
                    false
                }
            };

            if matches {
                if let Some(info) = self.lowering.enum_variants.get(def_id) {
                    return Some((info.clone(), layout));
                }
            }
        }
        let payload_struct_def = payload_def.or_else(|| self.struct_def_from_ty(payload_ty));
        if let (Some(enum_def), Some(payload_struct_def)) = (enum_def, payload_struct_def) {
            if let Some(info) = self.lowering.enum_variants.values().find(|info| {
                info.enum_def == enum_def && info.payload_def == Some(payload_struct_def)
            }) {
                return Some((info.clone(), layout));
            }
        }
        None
    }

    fn assign_enum_variant(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        scrutinee_ty: Option<&Ty>,
        args: &[hir::CallArg],
        span: Span,
    ) -> Result<()> {
        let payload_tys = self.enum_variant_payloads_for_layout(
            layout,
            variant,
            scrutinee_ty.unwrap_or(&layout.enum_ty),
            span,
        );

        if args.len() != payload_tys.len() {
            return Err(fp_core::error::Error::from(format!(
                "enum variant expected {} payload values, got {}",
                payload_tys.len(),
                args.len()
            )));
        }

        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            ty: layout.tag_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Int(variant.discriminant),
        }));

        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
            if let Some(expected_ty) = payload_tys.get(idx) {
                let arg = args.get(idx).ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "enum variant payload {idx} is missing after arity validation"
                    ))
                })?;
                let operand = self.lower_operand(&arg.value, Some(expected_ty))?;
                if self.lowering.is_opaque_ty(slot_ty) && slot_ty != expected_ty {
                    // This slot is shared with sibling variants whose own
                    // payload types disagree (`enum_layout_for_instance`
                    // already opaqued it out to a byte blob sized to fit
                    // all of them) — this variant's own value is narrower
                    // than that shared slot. Write it through a
                    // `PlaceElem::Field` projection at this variant's own
                    // (narrower) type, exactly mirroring how *reading* a
                    // payload back out of an opaque slot already works
                    // (`apply_field_projection` in mir_to_lir resolves a
                    // field projection via an address + pointer-cast to
                    // the caller-chosen type, independent of the slot's
                    // own declared type) — never construct the aggregate
                    // from a bare, mismatched-width operand directly.
                    let opaque_local = self.allocate_temp(slot_ty.clone(), span);
                    let mut opaque_field_place = mir::Place::from_local(opaque_local);
                    opaque_field_place
                        .projection
                        .push(mir::PlaceElem::Field(0, expected_ty.clone()));
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            opaque_field_place,
                            mir::Rvalue::Use(operand.operand),
                        ),
                    });
                    operands.push(mir::Operand::Copy(mir::Place::from_local(opaque_local)));
                } else {
                    operands.push(operand.operand);
                }
            } else {
                operands.push(mir::Operand::Constant(mir::Constant {
                    span,
                    ty: slot_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Undef,
                }));
            }
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        });
        Ok(())
    }

    fn enum_variant_payloads_for_layout(
        &mut self,
        layout: &EnumLayout,
        variant: &EnumVariantInfo,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Vec<Ty> {
        self.variant_payloads_from_layout_or_ty(layout, variant, scrutinee_ty, span)
    }

    fn assign_enum_variant_from_place(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        scrutinee_ty: Option<&Ty>,
        payload_place: mir::Place,
        span: Span,
    ) -> Result<()> {
        let payload_tys = self.enum_variant_payloads_for_layout(
            layout,
            variant,
            scrutinee_ty.unwrap_or(&layout.enum_ty),
            span,
        );

        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            ty: layout.tag_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Int(variant.discriminant),
        }));

        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
            if let Some(payload_ty) = payload_tys.get(idx) {
                let mut field_place = payload_place.clone();
                field_place
                    .projection
                    .push(mir::PlaceElem::Field(idx, payload_ty.clone()));
                operands.push(mir::Operand::Copy(field_place));
            } else if payload_tys.len() == 1 {
                let source_ty = self
                    .locals
                    .get(payload_place.local as usize)
                    .map(|local| local.ty.clone())
                    .ok_or_else(|| {
                        fp_core::error::Error::from(
                            "enum struct payload source local is unavailable",
                        )
                    })?;
                let source_layout = self.lowering.struct_layout_for_ty(&source_ty).or_else(|| {
                    if let TyKind::Adt(adt, substs) = &source_ty.kind {
                        let args = substs
                            .iter()
                            .filter_map(|arg| match arg {
                                mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        self.lowering
                            .struct_layout_for_instance(adt.did, &args, span)
                    } else {
                        None
                    }
                }).ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "enum struct payload source layout is unavailable for {:?}",
                        source_ty.kind
                    ))
                })?;
                let field_ty = source_layout.field_tys.get(idx).cloned().ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "enum struct payload field {idx} is unavailable"
                    ))
                })?;
                let mut field_place = payload_place.clone();
                field_place
                    .projection
                    .push(mir::PlaceElem::Field(idx, field_ty));
                operands.push(mir::Operand::Copy(field_place));
            } else {
                return Err(fp_core::error::Error::from(format!(
                    "enum variant payload slot {idx} is missing in source place during MIR lowering (slot_ty={slot_ty})"
                )));
            }
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        });
        Ok(())
    }

    fn lower_enum_variant_value(
        &mut self,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        expected_ty: Option<&Ty>,
        args: &[hir::CallArg],
        span: Span,
    ) -> Result<OperandInfo> {
        let nominal_ty = self.lowering.nominal_enum_ty(layout);
        let local_id = self.allocate_temp(nominal_ty.clone(), span);
        let place = mir::Place::from_local(local_id);
        self.assign_enum_variant(place.clone(), variant, layout, expected_ty, args, span)?;
        Ok(OperandInfo {
            operand: mir::Operand::copy(place),
            ty: nominal_ty,
        })
    }

    fn lower_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        expr_hir_id: hir::HirId,
        path: &hir::Path,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<()> {
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);
        let mut generic_args = resolved_path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| self.lowering.lower_generic_args(Some(args), span))
            .unwrap_or_default();
        let def_id = self.lowering.resolve_path_def_id(&resolved_path);

        if let (Some(expected_ty), Some(variant)) = (
            annotated_ty,
            self.enum_variant_info_from_path(&resolved_path),
        ) {
            let context_layout = self.enum_layout_for_variant(&variant, Some(expected_ty), span);
            if context_layout.is_none()
                && !self.lowering.has_unresolved_ty(expected_ty)
                && self.enum_def_from_ty(expected_ty) == Some(variant.enum_def)
            {
                // `expected_ty` is concrete and already names this exact
                // enum, yet the context-aware attempt still failed (e.g. a
                // substitution/arity mismatch somewhere upstream) —
                // falling through to `enum_layout_for_def`'s no-context
                // placeholder path here would silently substitute
                // `Infer`-tainted layout data for a case that had a real,
                // concrete instantiation available. That fallback is only
                // legitimate for genuinely context-free situations (see
                // its own doc comment) — surface a real diagnostic
                // instead, matching the sibling `emit_error` a few lines
                // below for the analogous "still unresolved after every
                // attempt" case.
                self.lowering.emit_error(
                    span,
                    "unable to resolve enum layout for variant construction despite a concrete declared type",
                );
                return Ok(());
            }
            if let Some(layout) =
                context_layout.or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span))
            {
                if self.enum_def_from_ty(expected_ty) == Some(layout.def_id) {
                    self.assign_enum_variant_from_struct_fields(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        fields,
                        span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
            }
        }

        if let Some(def_id) = def_id {
            if let Some(info) = self.lowering.struct_defs.get(&def_id).cloned() {
                if generic_args.is_empty() && !info.generics.is_empty() {
                    // No explicit turbofish — read `fp-typing`'s own
                    // already-resolved generic args for this literal
                    // (`typeck_exprs`), composed with this specialization's
                    // own `type_substs` when the cached result is still
                    // `Param`-relative to an enclosing generic item. Do not
                    // fall back to independently re-deriving the args here
                    // (by name-matching against `type_substs` or
                    // re-unifying against field literals) — that inference
                    // belongs in `fp-typing`, not in MIR lowering. A cache
                    // miss falls through to `struct_layout_for_instance`'s
                    // own arity check below, which reports a real
                    // diagnostic instead of guessing.
                    if let Some(cached) = self.lowering.adt_ty_args_from_typeck_cache(
                        expr_hir_id,
                        def_id,
                        &self.type_substs,
                    ) {
                        generic_args = cached;
                    }
                }
                if let Some(layout) =
                    self.lowering
                        .struct_layout_for_instance(def_id, &generic_args, span)
                {
                    return self.lower_registered_struct_literal(
                        local_id,
                        annotated_ty,
                        &info,
                        &layout,
                        fields,
                        span,
                        def_id,
                    );
                }
            }

            if let Some(variant) = self.lowering.enum_variants.get(&def_id).cloned() {
                let layout = annotated_ty
                    .and_then(|ty| self.enum_layout_for_ty(ty, span))
                    .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
                if let Some(layout) = layout {
                    self.assign_enum_variant_from_struct_fields(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        annotated_ty,
                        fields,
                        span,
                    )?;
                    self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                    return Ok(());
                }
                self.lowering.emit_error(
                    span,
                    "unable to resolve enum layout for struct-like variant",
                );
                return Ok(());
            }

            if let Some(const_info) = self.lowering.const_values.get(&def_id).cloned() {
                if !fields.is_empty() {
                    self.lowering.emit_warning(
                        span,
                        "struct literal for enum variant payload ignored; using discriminant",
                    );
                }
                let statement = mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(local_id),
                        mir::Rvalue::Use(mir::Operand::Constant(const_info.typed_value())),
                    ),
                };
                self.push_statement(statement);
                self.locals[local_id as usize].ty = const_info.ty.clone();
                return Ok(());
            }
        }

        if let Some(variant) = self.enum_variant_info_from_path(&resolved_path) {
            let layout = annotated_ty
                .and_then(|ty| self.enum_layout_for_variant(&variant, Some(ty), span))
                .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
            if let Some(layout) = layout {
                self.assign_enum_variant_from_struct_fields(
                    mir::Place::from_local(local_id),
                    &variant,
                    &layout,
                    annotated_ty,
                    fields,
                    span,
                )?;

                self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                return Ok(());
            }
            self.lowering.emit_error(
                span,
                "unable to resolve enum layout for struct-like variant",
            );
            return Ok(());
        }

        if let Some(expected_ty) = annotated_ty {
            if let Some(def_id) = self.struct_def_from_ty(expected_ty) {
                if let Some(info) = self.lowering.struct_defs.get(&def_id).cloned() {
                    if let Some(layout) = self
                        .lowering
                        .struct_layout_for_ty(expected_ty)
                        .or_else(|| self.lowering.struct_layout_for_instance(def_id, &[], span))
                    {
                        return self.lower_registered_struct_literal(
                            local_id,
                            annotated_ty,
                            &info,
                            &layout,
                            fields,
                            span,
                            def_id,
                        );
                    }
                }
            }
        }

        self.lowering.emit_warning(
            span,
            "struct literal without registered definition; using tuple aggregate",
        );
        self.lower_unknown_struct_literal(local_id, annotated_ty, fields, span)
    }

    fn assign_enum_variant_from_struct_fields(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        scrutinee_ty: Option<&Ty>,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<()> {
        let payload_tys = self.enum_variant_payloads_for_layout(
            layout,
            variant,
            scrutinee_ty.unwrap_or(&layout.enum_ty),
            span,
        );
        if payload_tys.is_empty() && fields.is_empty() {
            return self.assign_enum_variant(
                place,
                variant,
                layout,
                scrutinee_ty,
                &[],
                span,
            );
        }
        if payload_tys.len() != 1 && payload_tys.len() != fields.len() {
            return Err(fp_core::error::Error::from(
                format!(
                    "struct-like enum payload shape does not match its ABI layout (payloads={}, fields={}, slots={})",
                    payload_tys.len(), fields.len(), layout.payload_tys.len()
                ),
            ));
        }
        if payload_tys.len() == 1 && fields.len() != layout.payload_tys.len() {
            let payload_ty = payload_tys[0].clone();
            // Prefer the struct DefId already recorded on the variant (from
            // its original HIR payload type) over re-deriving it from the
            // lowered payload Ty — single-field structs are flattened to
            // their inner field's type for ABI purposes (e.g. `Adt(Some)`
            // with one `i32` field lowers to plain `Int(I32)`), so
            // `struct_def_from_ty` can no longer find a struct definition
            // to match against once that optimization has applied.
            let payload_def = variant
                .payload_def
                .or_else(|| self.struct_def_from_ty(&payload_ty))
                .ok_or_else(|| {
                    fp_core::error::Error::from("struct-like enum payload definition is unavailable")
                })?;
            let payload_info = self
                .lowering
                .struct_defs
                .get(&payload_def)
                .cloned()
                .ok_or_else(|| fp_core::error::Error::from("struct-like enum payload fields are unavailable"))?;
            // Same flattening concern as `payload_def` above: look the
            // layout up by the original struct's DefId first, since
            // `payload_ty` may no longer be the struct's own Adt type.
            let payload_layout = self
                .lowering
                .struct_layout_for_ty(&payload_ty)
                .or_else(|| self.lowering.struct_layout_for_instance(payload_def, &[], span))
                .ok_or_else(|| fp_core::error::Error::from("struct-like enum payload layout is unavailable"))?;
            // `lower_registered_struct_literal`'s own missing-field check
            // only fires for its generic (non-enum) struct-literal path — it
            // can't tell this is an enum payload once `payload_ty` has been
            // flattened to a non-Adt type, so it would otherwise report a
            // plain "missing field in struct literal" diagnostic (and only
            // as a diagnostic, not a hard error) instead of failing lowering
            // outright. This is already known to be an enum variant's
            // struct-like payload here, so check field completeness
            // directly and fail hard with the caller-facing message.
            let provided_fields: std::collections::HashSet<&str> =
                fields.iter().map(|field| field.name.as_str()).collect();
            for field_def in &payload_info.fields {
                if !provided_fields.contains(field_def.name.as_str()) {
                    return Err(fp_core::error::Error::from(format!(
                        "missing field `{}` in enum variant struct literal",
                        field_def.name
                    )));
                }
            }
            let payload_local = self.allocate_temp(payload_ty.clone(), span);
            self.lower_registered_struct_literal(
                payload_local,
                Some(&payload_ty),
                &payload_info,
                &payload_layout,
                fields,
                span,
                payload_def,
            )?;
            let mut operands = vec![mir::Operand::Constant(mir::Constant {
                span,
                ty: layout.tag_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Int(variant.discriminant),
            })];
            let slot_ty = layout.payload_tys.first();
            if let Some(slot_ty) = slot_ty {
                if self.lowering.is_opaque_ty(slot_ty) && *slot_ty != payload_ty {
                    // Same reasoning as `assign_enum_variant`: this
                    // struct-like variant's own payload shape is narrower
                    // than the slot shared with sibling variants whose
                    // payload types disagree — write it through a field
                    // projection at its own type rather than constructing
                    // the aggregate from a mismatched-width operand.
                    let opaque_local = self.allocate_temp(slot_ty.clone(), span);
                    let mut opaque_field_place = mir::Place::from_local(opaque_local);
                    opaque_field_place
                        .projection
                        .push(mir::PlaceElem::Field(0, payload_ty.clone()));
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            opaque_field_place,
                            mir::Rvalue::Use(mir::Operand::Copy(mir::Place::from_local(
                                payload_local,
                            ))),
                        ),
                    });
                    operands.push(mir::Operand::Copy(mir::Place::from_local(opaque_local)));
                } else {
                    operands.push(mir::Operand::Copy(mir::Place::from_local(payload_local)));
                }
            } else {
                operands.push(mir::Operand::Copy(mir::Place::from_local(payload_local)));
            }
            for slot_ty in layout.payload_tys.iter().skip(1) {
                operands.push(mir::Operand::Constant(mir::Constant {
                    span,
                    ty: slot_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Undef,
                }));
            }
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    place,
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                ),
            });
            return Ok(());
        }
        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            ty: layout.tag_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Int(variant.discriminant),
        }));
        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
            let field = fields.get(idx).ok_or_else(|| {
                fp_core::error::Error::from(format!("missing enum payload field {idx}"))
            })?;
            operands.push(self.lower_operand(&field.expr, Some(slot_ty))?.operand);
        }
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        });
        Ok(())
    }

    fn lower_registered_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        struct_def: &StructDefinition,
        layout: &StructLayout,
        fields: &[hir::StructExprField],
        span: Span,
        def_id: hir::DefId,
    ) -> Result<()> {
        let mut operands = Vec::with_capacity(struct_def.fields.len());
        let mut field_map: HashMap<String, &hir::StructExprField> = HashMap::new();
        for field in fields {
            field_map.insert(String::from(field.name.clone()), field);
        }

        let mut struct_fields = Vec::with_capacity(struct_def.fields.len());
        for (idx, field) in struct_def.fields.iter().enumerate() {
            let Some(field_ty) = layout.field_tys.get(idx) else {
                self.lowering.emit_error(
                    span,
                    format!("struct layout missing field type for `{}`", field.name),
                );
                return Ok(());
            };
            struct_fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty: field_ty.clone(),
            });
        }

        if let (Some(expected_ty), Some(struct_info)) =
            (annotated_ty, self.lowering.struct_defs.get(&def_id).cloned())
        {
            let enum_layout = match &expected_ty.kind {
                TyKind::Adt(adt, substs) if self.lowering.enum_defs.contains_key(&adt.did) => {
                    let args: Vec<Ty> = substs
                        .iter()
                        .filter_map(|arg| match arg {
                            mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                            _ => None,
                        })
                        .collect();
                    self.lowering
                        .enum_layout_for_instance(adt.did, &args, span)
                        .map(|layout| (adt.did, layout))
                }
                _ => None,
            };
            if let Some((enum_def_id, layout)) = enum_layout {
                if let Some(enum_def) = self.lowering.enum_defs.get(&enum_def_id) {
                    if let Some(variant_def) = enum_def
                        .variants
                        .iter()
                        .find(|variant| variant.name == struct_info.name)
                    {
                        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
                        operands.push(mir::Operand::Constant(mir::Constant {
                            span,
                            ty: layout.tag_ty.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Int(variant_def.discriminant),
                        }));

                        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
                            if let Some(field_info) = struct_fields.get(idx) {
                                let expr = match field_map.get(&field_info.name) {
                                    Some(field) => &field.expr,
                                    None => {
                                        return Err(fp_core::error::Error::from(format!(
                                            "missing field `{}` in enum variant struct literal",
                                            field_info.name
                                        )));
                                    }
                                };
                                let operand = self.lower_operand(expr, Some(slot_ty))?;
                                operands.push(operand.operand);
                            } else {
                                return Err(fp_core::error::Error::from(format!(
                                    "enum variant payload slot {idx} has no corresponding field in struct literal layout (slot_ty={slot_ty})"
                                )));
                            }
                        }

                        self.push_statement(mir::Statement {
                            source_info: span,
                            kind: mir::StatementKind::Assign(
                                mir::Place::from_local(local_id),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                            ),
                        });
                        self.locals[local_id as usize].ty = self.lowering.nominal_enum_ty(&layout);
                        return Ok(());
                    }
                }
            }
        }

        for field_info in struct_fields.iter() {
            let expr = match field_map.get(&field_info.name) {
                Some(field) => &field.expr,
                None => {
                    self.lowering.emit_error(
                        span,
                        format!("missing field `{}` in struct literal", field_info.name),
                    );
                    return Ok(());
                }
            };
            let operand = self.lower_operand(expr, Some(&field_info.ty))?;
            operands.push(operand.operand);
        }

        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                mir::Place::from_local(local_id),
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        };
        self.push_statement(assign);
        self.local_structs.insert(local_id, def_id);

        if let Some(ty) = annotated_ty {
            self.locals[local_id as usize].ty = ty.clone();
        } else {
            self.locals[local_id as usize].ty = layout.ty.clone();
        }

        Ok(())
    }

    fn lower_unknown_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<()> {
        let mut operands = Vec::with_capacity(fields.len());
        let mut tuple_types: Vec<Box<Ty>> = Vec::with_capacity(fields.len());

        for field in fields {
            let operand = self.lower_operand(&field.expr, None)?;
            tuple_types.push(Box::new(operand.ty.clone()));
            operands.push(operand.operand);
        }

        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                mir::Place::from_local(local_id),
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        };
        self.push_statement(assign);

        if let Some(ty) = annotated_ty {
            self.locals[local_id as usize].ty = ty.clone();
        } else {
            self.locals[local_id as usize].ty = Ty {
                kind: TyKind::Tuple(tuple_types),
            };
        }

        Ok(())
    }

    fn infer_explicit_args_from_expected_return(
        &mut self,
        function: &hir::Function,
        expected_return: Option<&Ty>,
    ) -> Option<Vec<Ty>> {
        if function.sig.generics.params.is_empty() {
            return None;
        }
        let expected_return = expected_return?;
        let expected_return = self.lowering.unwrap_expr_actual_ty(expected_return);
        let expected_return = match &expected_return.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            TyKind::RawPtr(type_and_mut) => type_and_mut.ty.as_ref(),
            _ => expected_return,
        };
        let mut expected_type_args = match &expected_return.kind {
            TyKind::Adt(_, substs) | TyKind::Opaque(_, substs) => substs
                .iter()
                .filter_map(|arg| match arg {
                    mir::ty::GenericArg::Type(ty) => {
                        Some(self.lowering.unwrap_expr_actual_ty(ty).clone())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        if expected_type_args.is_empty() {
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_return) {
                expected_type_args = layout
                    .args
                    .iter()
                    .map(|ty| self.lowering.unwrap_expr_actual_ty(ty).clone())
                    .collect::<Vec<_>>();
            }
        }
        let mut output_ty = &function.sig.output;
        while let Some(inner) = self.lowering.expr_inner_type_expr(output_ty) {
            output_ty = inner;
        }
        if let hir::TypeExprKind::Path(path) = &output_ty.kind {
            let (expected_def_id, substs) = match &expected_return.kind {
                TyKind::Adt(adt, substs) => (Some(adt.did), substs),
                TyKind::Opaque(_, substs) => (None, substs),
                _ => return None,
            };
            if let (Some(hir::Res::Def(def_id)), Some(expected_def_id)) =
                (path.res.as_ref(), expected_def_id)
            {
                if *def_id != expected_def_id {
                    let matches_name = path
                        .segments
                        .last()
                        .map(|seg| seg.name.as_str())
                        .map(|name| {
                            self.lowering
                                .enum_defs
                                .get(&expected_def_id)
                                .map(|def| {
                                    def.name.as_str() == name
                                        || def.name.as_str().ends_with(&format!("::{}", name))
                                })
                                .unwrap_or(false)
                                || self
                                    .lowering
                                    .struct_defs
                                    .get(&expected_def_id)
                                    .map(|def| {
                                        def.name.as_str() == name
                                            || def.name.as_str().ends_with(&format!("::{}", name))
                                    })
                                    .unwrap_or(false)
                        })
                        .unwrap_or(false);
                    if !matches_name {
                        return None;
                    }
                }
            }

            let path_args = path.segments.last().and_then(|seg| seg.args.as_ref());
            if path_args.map(|args| args.args.is_empty()).unwrap_or(true) {
                if expected_type_args.len() != function.sig.generics.params.len() {
                    return None;
                }
                let mut inferred = Vec::with_capacity(expected_type_args.len());
                for actual_ty in expected_type_args {
                    if matches!(actual_ty.kind, TyKind::Infer(_)) {
                        return None;
                    }
                    inferred.push(actual_ty.clone());
                }
                return Some(inferred);
            }
            let path_args = path_args?;

            let mut inferred = Vec::new();
            let mut actual_iter = substs.iter().filter_map(|arg| match arg {
                mir::ty::GenericArg::Type(ty) => Some(self.lowering.unwrap_expr_actual_ty(ty)),
                _ => None,
            });
            for arg in &path_args.args {
                let hir::GenericArg::Type(type_arg) = arg else {
                    continue;
                };
                let Some(actual_ty) = actual_iter.next() else {
                    return None;
                };
                let mut type_arg = type_arg.as_ref();
                while let Some(inner) = self.lowering.expr_inner_type_expr(type_arg) {
                    type_arg = inner;
                }
                let hir::TypeExprKind::Path(type_path) = &type_arg.kind else {
                    return None;
                };
                if type_path.segments.len() != 1 || type_path.segments[0].args.is_some() {
                    return None;
                }
                let name = type_path.segments[0].name.as_str();
                if !function
                    .sig
                    .generics
                    .params
                    .iter()
                    .any(|param| param.name.as_str() == name)
                {
                    return None;
                }
                if matches!(actual_ty.kind, TyKind::Infer(_)) {
                    return None;
                }
                inferred.push(actual_ty.clone());
            }

            if inferred.len() != function.sig.generics.params.len() {
                if expected_type_args.len() != function.sig.generics.params.len() {
                    return None;
                }
                let mut fallback = Vec::with_capacity(expected_type_args.len());
                for actual_ty in expected_type_args {
                    if matches!(actual_ty.kind, TyKind::Error(_) | TyKind::Infer(_)) {
                        return None;
                    }
                    fallback.push(actual_ty.clone());
                }
                return Some(fallback);
            }

            return Some(inferred);
        }

        let is_result_constructor = function.sig.name.as_str() == "Ok"
            || function.sig.name.as_str() == "Err"
            || function.sig.name.as_str().ends_with("::Ok")
            || function.sig.name.as_str().ends_with("::Err");
        if is_result_constructor {
            let is_result_ty = match &expected_return.kind {
                TyKind::Adt(adt, _) => self
                    .lowering
                    .enum_defs
                    .get(&adt.did)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false),
                TyKind::Opaque(def_id, _) => self
                    .lowering
                    .enum_defs
                    .get(def_id)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false),
                _ => false,
            };
            if is_result_ty && expected_type_args.len() == function.sig.generics.params.len() {
                let mut inferred = Vec::with_capacity(expected_type_args.len());
                for actual_ty in &expected_type_args {
                    if matches!(actual_ty.kind, TyKind::Error(_) | TyKind::Infer(_)) {
                        return None;
                    }
                    inferred.push(actual_ty.clone());
                }
                return Some(inferred);
            }
        }

        if expected_type_args.len() != function.sig.generics.params.len() {
            return None;
        }
        let mut inferred = Vec::with_capacity(expected_type_args.len());
        for actual_ty in expected_type_args {
            if matches!(actual_ty.kind, TyKind::Infer(_)) {
                return None;
            }
            inferred.push(actual_ty.clone());
        }
        Some(inferred)
    }

    fn lower_call(
        &mut self,
        expr: &hir::Expr,
        callee: &hir::Expr,
        args: &[hir::CallArg],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<Option<PlaceInfo>> {
        let mut reordered_args = None;
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some(param_names) = self.param_names_for_callee(path) {
                let ordered = self.reorder_named_call_args(args, &param_names, expr.span)?;
                reordered_args = Some(ordered);
            }
        }
        let args = reordered_args.as_deref().unwrap_or(args);
        let arg_values = call_arg_values(args);
        if let hir::ExprKind::Path(path) = &callee.kind {
            let segments = &path.segments;
            if segments.len() >= 2
                && segments[segments.len() - 2].name.as_str() == "HashMap"
                && segments[segments.len() - 1].name.as_str() == "from"
            {
                if let Some((place, expected_ty)) = destination {
                    if arg_values.len() != 1 {
                        self.lowering.emit_error(
                            expr.span,
                            "HashMap::from expects a single entries argument",
                        );
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: expected_ty,
                            struct_def: None,
                        }));
                    }

                    let hir::ExprKind::Array(elements) = &arg_values[0].kind else {
                        self.lowering.emit_error(
                            expr.span,
                            "HashMap::from expects an array literal of entries",
                        );
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: expected_ty,
                            struct_def: None,
                        }));
                    };

                    let mut entries = Vec::with_capacity(elements.len());
                    let mut key_ty: Option<Ty> = None;
                    let mut value_ty: Option<Ty> = None;

                    for element in elements {
                        if let hir::ExprKind::Struct(path, fields) = &element.kind {
                            let tail = path.segments.last().map(|seg| seg.name.as_str());
                            if tail == Some("HashMapEntry") {
                                let mut key_expr = None;
                                let mut value_expr = None;
                                for field in fields {
                                    match field.name.as_str() {
                                        "key" => key_expr = Some(&field.expr),
                                        "value" => value_expr = Some(&field.expr),
                                        _ => {}
                                    }
                                }
                                if let (Some(key_expr), Some(value_expr)) = (key_expr, value_expr) {
                                    let key_operand = self.lower_operand(key_expr, None)?;
                                    let value_operand = self.lower_operand(value_expr, None)?;
                                    if key_ty.is_none() {
                                        key_ty = Some(key_operand.ty.clone());
                                    }
                                    if value_ty.is_none() {
                                        value_ty = Some(value_operand.ty.clone());
                                    }
                                    entries.push((key_operand.operand, value_operand.operand));
                                    continue;
                                }
                            }
                        }
                        self.lowering.emit_error(
                            element.span,
                            "HashMap::from expects entries as HashMapEntry { key, value }",
                        );
                    }

                    let key_ty = key_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let value_ty = value_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::Map {
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                        len: entries.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerMapLiteral {
                                kind: kind.clone(),
                                entries,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        if (place.local as usize) < self.locals.len() {
                            self.locals[place.local as usize].ty = expected_ty.clone();
                        }
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(Some(PlaceInfo {
                        place,
                        ty: expected_ty,
                        struct_def: None,
                    }));
                }
            }
            if segments.last().map(|seg| seg.name.as_str()) == Some("raw_parts_to_str") {
                // `std::ffi::raw_parts_to_str(ptr, len)` — the one genuinely
                // backend-level primitive `CStr::as_str_unchecked` needs:
                // assembling a `&str`/`str` fat pointer from an already
                // runtime-computed `(ptr, len)` pair. Everything else about
                // `CStr` (fields, `from_ptr`, `as_ptr`, the `strlen` call
                // itself) is ordinary, real `.fp` code.
                if let Some((place, expected_ty)) = destination {
                    if arg_values.len() != 2 {
                        self.lowering.emit_error(
                            expr.span,
                            "raw_parts_to_str expects (ptr, len) arguments",
                        );
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: expected_ty,
                            struct_def: None,
                        }));
                    }
                    let ptr_operand = self.lower_operand(arg_values[0], None)?;
                    let len_ty = Ty {
                        kind: TyKind::Int(IntTy::I64),
                    };
                    let len_operand = self.lower_operand(arg_values[1], Some(&len_ty))?;
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::StrFromRawParts {
                                ptr: ptr_operand.operand,
                                len: len_operand.operand,
                            },
                        ),
                    });
                    return Ok(Some(PlaceInfo {
                        place,
                        ty: expected_ty,
                        struct_def: None,
                    }));
                }
            }
        }
        if let hir::ExprKind::Path(path) = &callee.kind {
            let tail = path.segments.last().map(|seg| seg.name.as_str());
            if tail == Some("get_unchecked") || tail == Some("::get_unchecked") {
                let (place, expected_ty) = match destination.as_ref() {
                    Some((place, expected_ty)) => (place.clone(), expected_ty.clone()),
                    None => {
                        self.lowering
                            .emit_error(expr.span, "HashMap::get_unchecked requires a destination");
                        return Ok(None);
                    }
                };
                if args.len() != 2 {
                    self.lowering.emit_error(
                        expr.span,
                        "HashMap::get_unchecked expects a container and key",
                    );
                    return Ok(Some(PlaceInfo {
                        place,
                        ty: expected_ty,
                        struct_def: None,
                    }));
                }

                if let hir::ExprKind::Path(path) = &arg_values[0].kind {
                    let mut resolved_path = path.clone();
                    self.resolve_self_path(&mut resolved_path);
                    let mut const_info = None;
                    let mut const_body_len = None;
                    if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                        if let Some(info) = self.lowering.const_values.get(def_id) {
                            const_info = Some(info.clone());
                        } else if let Some(konst) =
                            self.lowering.hir_def_map.get(def_id).and_then(|item| {
                                match &item.kind {
                                    hir::ItemKind::Const(konst) => Some(konst.clone()),
                                    _ => None,
                                }
                            })
                        {
                            if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                const_body_len = Some(elements.len() as u64);
                            }
                            self.lowering.register_const_value(*def_id, &konst);
                            if let Some(info) = self.lowering.const_values.get(def_id) {
                                const_info = Some(info.clone());
                            }
                        }
                    } else if resolved_path.segments.len() == 1 {
                        let name = resolved_path.segments[0].name.as_str();
                        let matching_const = self.lowering.hir_def_map.iter().find_map(
                            |(def_id, item)| match &item.kind {
                                hir::ItemKind::Const(konst) if konst.name.as_str() == name => {
                                    Some((*def_id, konst.clone()))
                                }
                                _ => None,
                            },
                        );
                        if let Some((def_id, konst)) = matching_const {
                            if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                const_body_len = Some(elements.len() as u64);
                            }
                            self.lowering.register_const_value(def_id, &konst);
                            if let Some(info) = self.lowering.const_values.get(&def_id) {
                                const_info = Some(info.clone());
                            }
                        }
                    }

                    if let Some(const_info) = const_info {
                        if let mir::ConstantKind::Val(value) = &const_info.value.literal {
                            if let Some((constant, ty)) = self.lowering.const_index_value(
                                expr.span,
                                &const_info.typed_value(),
                                &arg_values[1],
                            ) {
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::Use(mir::Operand::Constant(constant)),
                                    ),
                                });
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = ty.clone();
                                }
                                return Ok(Some(PlaceInfo {
                                    place,
                                    ty,
                                    struct_def: None,
                                }));
                            }
                            let mut map_len: Option<u64> = None;
                            let mut map_key_ty: Option<Ty> = None;
                            let mut map_value_ty: Option<Ty> = None;
                            match value {
                                mir::ConstValue::Map {
                                    entries,
                                    key_ty,
                                    value_ty,
                                } => {
                                    map_len = Some(entries.len() as u64);
                                    map_key_ty = Some(key_ty.clone());
                                    map_value_ty = Some(value_ty.clone());
                                }
                                mir::ConstValue::List { elements, elem_ty } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(elements.len() as u64);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                                mir::ConstValue::Array(elements) => {
                                    if let TyKind::Array(elem_ty, _) = &const_info.ty.kind {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_len = Some(elements.len() as u64);
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                            if map_len.is_none() {
                                map_len = const_body_len;
                            }

                            if let (Some(key_ty), Some(value_ty), Some(len)) =
                                (map_key_ty, map_value_ty, map_len)
                            {
                                if len != 0 {
                                    let key_operand =
                                        self.lower_operand(arg_values[1], Some(&key_ty))?;
                                    self.push_statement(mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place.clone(),
                                            mir::Rvalue::ContainerGet {
                                                kind: mir::ContainerKind::Map {
                                                    key_ty: key_ty.clone(),
                                                    value_ty: value_ty.clone(),
                                                    len,
                                                },
                                                container: mir::Operand::Constant(
                                                    const_info.typed_value(),
                                                ),
                                                key: key_operand.operand,
                                            },
                                        ),
                                    });
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = value_ty.clone();
                                    }
                                    return Ok(Some(PlaceInfo {
                                        place,
                                        ty: value_ty,
                                        struct_def: None,
                                    }));
                                }
                            }
                        }
                    }
                }

                let container_info = self.lower_operand(arg_values[0], None)?;
                let mut map_len: Option<u64> = None;
                let mut map_key_ty: Option<Ty> = None;
                let mut map_value_ty: Option<Ty> = None;

                if let mir::Operand::Constant(constant) = &container_info.operand {
                    if let mir::ConstantKind::Val(value) = &constant.literal {
                        match value {
                            mir::ConstValue::Map {
                                entries,
                                key_ty,
                                value_ty,
                            } => {
                                map_len = Some(entries.len() as u64);
                                map_key_ty = Some(key_ty.clone());
                                map_value_ty = Some(value_ty.clone());
                            }
                            mir::ConstValue::List { elements, elem_ty } => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_len = Some(elements.len() as u64);
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                    }
                                }
                            }
                            mir::ConstValue::Array(elements) => {
                                if let TyKind::Array(elem_ty, _) = &container_info.ty.kind {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(elements.len() as u64);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }

                if map_len.is_none() {
                    if let Some(local_id) = self.local_id_from_expr(arg_values[0]) {
                        if let Some(container_kind) = self.container_locals.get(&local_id).cloned()
                        {
                            match container_kind {
                                mir::ContainerKind::Map {
                                    key_ty,
                                    value_ty,
                                    len,
                                } => {
                                    map_len = Some(len);
                                    map_key_ty = Some(key_ty);
                                    map_value_ty = Some(value_ty);
                                }
                                mir::ContainerKind::List { elem_ty, len } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(len);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if map_len.is_none() {
                    if let mir::Operand::Copy(place) = &container_info.operand {
                        if let Some(container_kind) =
                            self.container_locals.get(&place.local).cloned()
                        {
                            match container_kind {
                                mir::ContainerKind::Map {
                                    key_ty,
                                    value_ty,
                                    len,
                                } => {
                                    map_len = Some(len);
                                    map_key_ty = Some(key_ty);
                                    map_value_ty = Some(value_ty);
                                }
                                mir::ContainerKind::List { elem_ty, len } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(len);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if map_len.is_none() {
                    let container_ty = match &container_info.ty.kind {
                        TyKind::Ref(_, inner, _) => inner.as_ref(),
                        _ => &container_info.ty,
                    };
                    match &container_ty.kind {
                        TyKind::Array(elem_ty, len) => {
                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                if fields.len() == 2 {
                                    map_key_ty = Some((*fields[0].clone()).clone());
                                    map_value_ty = Some((*fields[1].clone()).clone());
                                    map_len = self.const_kind_to_u64(expr.span, len);
                                }
                            }
                        }
                        TyKind::Slice(elem_ty) => {
                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                if fields.len() == 2 {
                                    map_key_ty = Some((*fields[0].clone()).clone());
                                    map_value_ty = Some((*fields[1].clone()).clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }

                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                    let len = map_len.unwrap_or(0);
                    if len != 0 {
                        let key_operand = self.lower_operand(arg_values[1], Some(&key_ty))?;
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::ContainerGet {
                                    kind: mir::ContainerKind::Map {
                                        key_ty: key_ty.clone(),
                                        value_ty: value_ty.clone(),
                                        len,
                                    },
                                    container: container_info.operand,
                                    key: key_operand.operand,
                                },
                            ),
                        });
                        if (place.local as usize) < self.locals.len() {
                            self.locals[place.local as usize].ty = value_ty.clone();
                        }
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: value_ty,
                            struct_def: None,
                        }));
                    }
                }
            }
        }
        if let hir::ExprKind::Path(path) = &callee.kind {
            let expected_ty = destination.as_ref().map(|(_, ty)| ty);
            let tail = path.segments.last().map(|seg| seg.name.as_str());
            let variant = self
                .enum_variant_info_from_path(path)
                .or_else(|| self.enum_variant_info_from_expected(path, expected_ty))
                .or_else(|| {
                    tail.and_then(|name| {
                        expected_ty.and_then(|ty| self.result_variant_from_expected(ty, name))
                    })
                });
            if let Some(variant) = variant {
                let explicit_enum_args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                let mut layout = destination.as_ref().and_then(|(_, ty)| {
                    self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                });
                if layout.is_none() {
                    if !explicit_enum_args.is_empty() {
                        layout = self.lowering.enum_layout_for_instance(
                            variant.enum_def,
                            &explicit_enum_args,
                            expr.span,
                        );
                    } else if let Some((_, expected_ty)) = destination.as_ref() {
                        if let Some(inferred_args) =
                            self.infer_enum_args_from_expected_ty(variant.enum_def, expected_ty)
                        {
                            layout = self.lowering.enum_layout_for_instance(
                                variant.enum_def,
                                &inferred_args,
                                expr.span,
                            );
                        }
                    }
                    if layout.is_none() {
                        if let Some((_, expected_ty)) = destination.as_ref() {
                            if let Some(layout_from_ty) =
                                self.enum_layout_for_ty(expected_ty, expr.span)
                            {
                                if layout_from_ty.def_id == variant.enum_def {
                                    layout = Some(layout_from_ty);
                                }
                            }
                        }
                    }
                    if layout.is_none() {
                        layout = self
                            .lowering
                            .enum_layout_for_def(variant.enum_def, expr.span);
                    }
                }

                if let Some(layout) = layout {
                    let nominal_ty = self.lowering.nominal_enum_ty(&layout);
                    let place = destination
                        .as_ref()
                        .map(|(place, _)| place.clone())
                        .unwrap_or_else(|| {
                            let local_id = self.allocate_temp(nominal_ty.clone(), expr.span);
                            mir::Place::from_local(local_id)
                        });
                    let expected_ty = destination.as_ref().map(|(_, ty)| ty);
                    self.assign_enum_variant(
                        place.clone(),
                        &variant,
                        &layout,
                        expected_ty,
                        args,
                        expr.span,
                    )?;
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = nominal_ty.clone();
                    }
                    if destination.is_some() {
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: nominal_ty,
                            struct_def: None,
                        }));
                    }
                    return Ok(None);
                }

                if !args.is_empty() {
                    self.lowering
                        .emit_error(expr.span, "enum variant does not accept payload values");
                }
                if let Some(const_info) = self.lowering.const_values.get(&variant.def_id).cloned() {
                    if let Some((place, _)) = destination {
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(mir::Operand::Constant(const_info.typed_value())),
                            ),
                        });
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: const_info.ty.clone(),
                            struct_def: None,
                        }));
                    }
                    return Ok(None);
                }
            }
        }
        let mut generic_def_id = None;
        let mut generic_method_def: Option<MethodDefinition> = None;
        let mut explicit_args: Vec<Ty> = Vec::new();
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some(args) = path
                .segments
                .iter()
                .find_map(|segment| segment.args.as_ref())
            {
                explicit_args = self.lowering.lower_generic_args(Some(args), expr.span);
            }
            if explicit_args.is_empty() {
                if let Some(args) = self.lowering.typeck_generic_call_args.get(&expr.hir_id) {
                    // The type checker's own cached inference for this call
                    // can itself still be `Param`-relative rather than
                    // fully concrete: when the call is nested inside
                    // another still-generic enclosing item's own body
                    // (e.g. `add(a, b)` inside `pipeline<T>`), typeck
                    // resolves it once, generically, relative to that
                    // enclosing item's own params — not per
                    // monomorphization. Composing the current
                    // specialization's own concrete bindings
                    // (`self.type_substs`) in first (a no-op when empty)
                    // lets a legitimately `Param`-relative cached result
                    // resolve to something concrete, the same way it can
                    // also stay an unbound placeholder (e.g. a generic-impl
                    // associated function called with no receiver/args to
                    // unify against, like `Vec::<T>::new()`) that composing
                    // can't fix — only trust the result once composing
                    // leaves nothing still unresolved, so the
                    // destination-type fallback inference below still runs
                    // for the latter case.
                    let composed: Vec<Ty> = if self.type_substs.is_empty() {
                        args.clone()
                    } else {
                        args.iter()
                            .map(|ty| self.lowering.substitute_ty(ty, &self.type_substs))
                            .collect()
                    };
                    if !composed.iter().any(|ty| self.lowering.has_unresolved_ty(ty)) {
                        explicit_args = composed;
                    }
                }
            }
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self.lowering.generic_function_defs.contains_key(def_id) {
                    generic_def_id = Some(*def_id);
                }
            }
            if let Some(hir::Res::Def(def_id)) = &path.res {
                // `ensure_generic_method_def` is the uniform lookup —
                // resolves a generic method (`Vec::from`, etc.) in this
                // package or any dependency's the same way, lazily
                // registering it on a miss instead of requiring it to
                // already be known (see `resolve_callee_path`'s matching
                // comment for the non-generic case).
                generic_method_def = self.lowering.ensure_generic_method_def(*def_id);
            }
        }

        let (mut func_operand, mut sig, mut callee_name) = if let Some(def_id) = generic_def_id {
            let function = self
                .lowering
                .generic_function_defs
                .get(&def_id)
                .cloned()
                .ok_or_else(|| crate::error::optimization_error("missing generic function def"))?;
            let sig = self.lowering.lower_function_sig(&function.sig, None);
            let fn_ty = self.lowering.function_pointer_ty(&sig);
            let name = function.sig.name.as_str().to_string();
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                ty: fn_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Fn(Symbol::new(name.clone())),
            });
            (operand, sig, Some(name))
        } else if let Some(def) = generic_method_def.as_ref() {
            let method_ctx = self.lowering.make_method_context(&def.self_ty, &def.assoc_types);
            let sig = self
                .lowering
                .lower_function_sig(&def.function.sig, method_ctx.as_ref());
            let fn_ty = self.lowering.function_pointer_ty(&sig);
            let name = def.method_name.clone();
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                ty: fn_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Fn(Symbol::new(name.clone())),
            });
            (operand, sig, Some(name))
        } else {
            self.resolve_callee(callee)?
        };
        let mut associated_struct = match &callee.kind {
            hir::ExprKind::Path(path) => path
                .res
                .as_ref()
                .and_then(|res| match res {
                    hir::Res::Def(def_id) => self.lowering.method_lookup_by_def.get(def_id),
                    _ => None,
                })
                .and_then(|info| info.struct_def),
            _ => None,
        };
        let callee_tail = if let hir::ExprKind::Path(path) = &callee.kind {
            path.segments.last().map(|seg| seg.name.as_str())
        } else {
            None
        };
        let mut callee_abi = None;
        let mut callee_is_extern = false;
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some((abi, is_extern)) = self.callee_abi_from_path(path) {
                callee_abi = Some(abi);
                callee_is_extern = is_extern;
            }
        }
        if callee_abi.is_none() {
            if let Some(name) = callee_name.as_ref() {
                for item in self.lowering.hir_def_map.values() {
                    if let hir::ItemKind::Function(func) = &item.kind {
                        if func.sig.name.as_str() == name {
                            callee_abi = Some(func.sig.abi.clone());
                            callee_is_extern = func.is_extern;
                            break;
                        }
                    }
                }
            }
        }

        let mut lowered_args = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = sig.inputs.get(idx);
            let abi_is_c = callee_is_extern
                || matches!(
                    callee_abi,
                    Some(hir::Abi::C { .. } | hir::Abi::System { .. })
                );
            if abi_is_c {
                if let hir::ExprKind::Cast(inner, ty_expr) = &arg.value.kind {
                    let cast_ty = self.lower_type_expr(ty_expr);
                    if matches!(cast_ty.kind, TyKind::RawPtr(_)) {
                        let operand = self.lower_operand(inner, None)?;
                        let temp_local = self.allocate_temp(cast_ty.clone(), arg.value.span);
                        let temp_place = mir::Place::from_local(temp_local);
                        self.push_statement(mir::Statement {
                            source_info: arg.value.span,
                            kind: mir::StatementKind::Assign(
                                temp_place.clone(),
                                mir::Rvalue::Cast(
                                    mir::CastKind::Misc,
                                    operand.operand,
                                    cast_ty.clone(),
                                ),
                            ),
                        });
                        lowered_args.push(mir::Operand::copy(temp_place));
                        arg_types.push(cast_ty);
                        continue;
                    }
                }
            }
            if abi_is_c {
                if let Some(expected_ty) = expected_ty {
                    if let TyKind::RawPtr(type_and_mut) = &expected_ty.kind {
                        let direct_operand = self.lower_operand(&arg.value, Some(expected_ty))?;
                        if matches!(direct_operand.ty.kind, TyKind::RawPtr(_)) {
                            lowered_args.push(direct_operand.operand);
                            arg_types.push(expected_ty.clone());
                            continue;
                        }
                        let borrow_expr =
                            if let hir::ExprKind::Reference(reference) = &arg.value.kind {
                                reference.expr.as_ref()
                            } else {
                                &arg.value
                            };
                        let mut place = if let Some(place) = self.lower_place(borrow_expr)? {
                            place
                        } else {
                            self.materialize_expr_place(borrow_expr)?
                        };
                        if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                            place.place.projection.push(mir::PlaceElem::Deref);
                            place.ty = inner_ty.as_ref().clone();
                            place.struct_def = self.struct_def_from_ty(&place.ty);
                        }
                        let addr_mutability = match type_and_mut.mutbl {
                            mir::ty::Mutability::Mut => mir::Mutability::Mut,
                            mir::ty::Mutability::Not => mir::Mutability::Not,
                        };
                        let temp_local = self.allocate_temp(expected_ty.clone(), arg.value.span);
                        let temp_place = mir::Place::from_local(temp_local);
                        self.push_statement(mir::Statement {
                            source_info: arg.value.span,
                            kind: mir::StatementKind::Assign(
                                temp_place.clone(),
                                mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                            ),
                        });
                        lowered_args.push(mir::Operand::copy(temp_place));
                        arg_types.push(expected_ty.clone());
                        continue;
                    }
                    if let TyKind::Ref(_region, inner, mutability) = &expected_ty.kind {
                        let borrow_expr =
                            if let hir::ExprKind::Reference(reference) = &arg.value.kind {
                                reference.expr.as_ref()
                            } else {
                                &arg.value
                            };
                        let mut place = if let Some(place) = self.lower_place(borrow_expr)? {
                            place
                        } else {
                            self.materialize_expr_place(borrow_expr)?
                        };
                        if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                            place.place.projection.push(mir::PlaceElem::Deref);
                            place.ty = inner_ty.as_ref().clone();
                            place.struct_def = self.struct_def_from_ty(&place.ty);
                        }
                        let resolved_inner = if self.lowering.has_unresolved_ty(inner.as_ref())
                            && !self.lowering.has_unresolved_ty(&place.ty)
                        {
                            place.ty.clone()
                        } else {
                            inner.as_ref().clone()
                        };
                        let ptr_ty = Ty {
                            kind: TyKind::RawPtr(TypeAndMut {
                                ty: Box::new(resolved_inner),
                                mutbl: match mutability {
                                    Mutability::Mut => mir::ty::Mutability::Mut,
                                    Mutability::Not => mir::ty::Mutability::Not,
                                },
                            }),
                        };
                        let addr_mutability = match mutability {
                            Mutability::Mut => mir::Mutability::Mut,
                            Mutability::Not => mir::Mutability::Not,
                        };
                        let temp_local = self.allocate_temp(ptr_ty.clone(), arg.value.span);
                        let temp_place = mir::Place::from_local(temp_local);
                        self.push_statement(mir::Statement {
                            source_info: arg.value.span,
                            kind: mir::StatementKind::Assign(
                                temp_place.clone(),
                                mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                            ),
                        });
                        lowered_args.push(mir::Operand::copy(temp_place));
                        arg_types.push(ptr_ty);
                        continue;
                    }
                }
            }
            if let hir::ExprKind::Reference(reference) = &arg.value.kind {
                if let Some(expected_ty) = expected_ty {
                    if let TyKind::RawPtr(type_and_mut) = &expected_ty.kind {
                        let mut place = if let Some(place) = self.lower_place(&reference.expr)? {
                            place
                        } else {
                            self.materialize_expr_place(&reference.expr)?
                        };
                        if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                            place.place.projection.push(mir::PlaceElem::Deref);
                            place.ty = inner_ty.as_ref().clone();
                            place.struct_def = self.struct_def_from_ty(&place.ty);
                        }
                        let addr_mutability = match type_and_mut.mutbl {
                            mir::ty::Mutability::Mut => mir::Mutability::Mut,
                            mir::ty::Mutability::Not => mir::Mutability::Not,
                        };
                        let temp_local = self.allocate_temp(expected_ty.clone(), arg.value.span);
                        let temp_place = mir::Place::from_local(temp_local);
                        self.push_statement(mir::Statement {
                            source_info: arg.value.span,
                            kind: mir::StatementKind::Assign(
                                temp_place.clone(),
                                mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                            ),
                        });
                        lowered_args.push(mir::Operand::copy(temp_place));
                        arg_types.push(expected_ty.clone());
                        continue;
                    }
                    if abi_is_c {
                        if let TyKind::Ref(_region, _inner, mutability) = &expected_ty.kind {
                            let mut place =
                                if let Some(place) = self.lower_place(&reference.expr)? {
                                    place
                                } else {
                                    self.materialize_expr_place(&reference.expr)?
                                };
                            if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                                place.place.projection.push(mir::PlaceElem::Deref);
                                place.ty = inner_ty.as_ref().clone();
                                place.struct_def = self.struct_def_from_ty(&place.ty);
                            }
                            let addr_mutability = match mutability {
                                Mutability::Mut => mir::Mutability::Mut,
                                Mutability::Not => mir::Mutability::Not,
                            };
                            let ptr_ty = Ty {
                                kind: TyKind::RawPtr(TypeAndMut {
                                    ty: Box::new(place.ty.clone()),
                                    mutbl: match mutability {
                                        Mutability::Mut => mir::ty::Mutability::Mut,
                                        Mutability::Not => mir::ty::Mutability::Not,
                                    },
                                }),
                            };
                            let temp_local = self.allocate_temp(ptr_ty.clone(), arg.value.span);
                            let temp_place = mir::Place::from_local(temp_local);
                            self.push_statement(mir::Statement {
                                source_info: arg.value.span,
                                kind: mir::StatementKind::Assign(
                                    temp_place.clone(),
                                    mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                                ),
                            });
                            lowered_args.push(mir::Operand::copy(temp_place));
                            arg_types.push(ptr_ty);
                            continue;
                        }
                    }
                }
                if abi_is_c
                    && expected_ty
                        .map(|ty| self.lowering.has_unresolved_ty(ty))
                        .unwrap_or(true)
                {
                    let mut place = if let Some(place) = self.lower_place(&reference.expr)? {
                        place
                    } else {
                        self.materialize_expr_place(&reference.expr)?
                    };
                    if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                        place.place.projection.push(mir::PlaceElem::Deref);
                        place.ty = inner_ty.as_ref().clone();
                        place.struct_def = self.struct_def_from_ty(&place.ty);
                    }
                    let addr_mutability = match reference.mutable {
                        hir::ty::Mutability::Mut => mir::Mutability::Mut,
                        hir::ty::Mutability::Not => mir::Mutability::Not,
                    };
                    let ptr_ty = Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(place.ty.clone()),
                            mutbl: match reference.mutable {
                                hir::ty::Mutability::Mut => mir::ty::Mutability::Mut,
                                hir::ty::Mutability::Not => mir::ty::Mutability::Not,
                            },
                        }),
                    };
                    let temp_local = self.allocate_temp(ptr_ty.clone(), arg.value.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    self.push_statement(mir::Statement {
                        source_info: arg.value.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                        ),
                    });
                    lowered_args.push(mir::Operand::copy(temp_place));
                    arg_types.push(ptr_ty);
                    continue;
                }
                let operand = self.lower_reference_operand(reference, arg.value.span)?;
                let inferred_ty = if let Some(expected_ty) = expected_ty {
                    if self.lowering.has_unresolved_ty(expected_ty) {
                        operand.ty.clone()
                    } else {
                        expected_ty.clone()
                    }
                } else {
                    operand.ty.clone()
                };
                lowered_args.push(operand.operand);
                arg_types.push(inferred_ty);
                continue;
            }
            let mut operand = self.lower_operand(&arg.value, expected_ty)?;
            if let Some(expected_ty) = expected_ty {
                if let TyKind::Ref(region, inner, mutability) = &expected_ty.kind {
                    let borrow_expr = if let hir::ExprKind::Reference(reference) = &arg.value.kind {
                        reference.expr.as_ref()
                    } else {
                        &arg.value
                    };
                    let mut place = if let Some(place) = self.lower_place(borrow_expr)? {
                        place
                    } else {
                        self.materialize_expr_place(borrow_expr)?
                    };
                    if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                        place.place.projection.push(mir::PlaceElem::Deref);
                        place.ty = inner_ty.as_ref().clone();
                        place.struct_def = self.struct_def_from_ty(&place.ty);
                    }
                    let resolved_inner = if self.lowering.has_unresolved_ty(inner.as_ref())
                        && !self.lowering.has_unresolved_ty(&place.ty)
                    {
                        place.ty.clone()
                    } else {
                        inner.as_ref().clone()
                    };
                    let ref_ty = if resolved_inner == *inner.as_ref() {
                        expected_ty.clone()
                    } else {
                        Ty {
                            kind: TyKind::Ref(
                                region.clone(),
                                Box::new(resolved_inner),
                                *mutability,
                            ),
                        }
                    };
                    let borrow_kind = match mutability {
                        Mutability::Mut => mir::BorrowKind::Mut {
                            allow_two_phase_borrow: false,
                        },
                        Mutability::Not => mir::BorrowKind::Shared,
                    };
                    let temp_local = self.allocate_temp(ref_ty.clone(), arg.value.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    self.push_statement(mir::Statement {
                        source_info: arg.value.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Ref((), borrow_kind, place.place.clone()),
                        ),
                    });
                    operand = OperandInfo {
                        operand: mir::Operand::copy(temp_place),
                        ty: ref_ty,
                    };
                } else if let TyKind::RawPtr(type_and_mut) = &expected_ty.kind {
                    let borrow_expr = if let hir::ExprKind::Reference(reference) = &arg.value.kind {
                        reference.expr.as_ref()
                    } else {
                        &arg.value
                    };
                    let mut place = if let Some(place) = self.lower_place(borrow_expr)? {
                        place
                    } else {
                        self.materialize_expr_place(borrow_expr)?
                    };
                    if let TyKind::Ref(_, inner_ty, _) = &place.ty.kind {
                        place.place.projection.push(mir::PlaceElem::Deref);
                        place.ty = inner_ty.as_ref().clone();
                        place.struct_def = self.struct_def_from_ty(&place.ty);
                    }
                    let addr_mutability = match type_and_mut.mutbl {
                        mir::ty::Mutability::Mut => mir::Mutability::Mut,
                        mir::ty::Mutability::Not => mir::Mutability::Not,
                    };
                    let temp_local = self.allocate_temp(expected_ty.clone(), arg.value.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    self.push_statement(mir::Statement {
                        source_info: arg.value.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::AddressOf(addr_mutability, place.place.clone()),
                        ),
                    });
                    operand = OperandInfo {
                        operand: mir::Operand::copy(temp_place),
                        ty: expected_ty.clone(),
                    };
                }
            }
            let inferred_ty = if let Some(expected_ty) = expected_ty {
                if let TyKind::Ref(_region, _inner, mutability) = &expected_ty.kind {
                    let local_id = match &arg.value.kind {
                        hir::ExprKind::Path(path) => {
                            if let Some(hir::Res::Local(hir_id)) = &path.res {
                                self.local_map.get(hir_id).copied()
                            } else {
                                path.segments
                                    .first()
                                    .filter(|_| path.segments.len() == 1)
                                    .and_then(|seg| {
                                        self.fallback_locals.get(seg.name.as_str()).copied()
                                    })
                            }
                        }
                        _ => None,
                    };
                    if let Some(local_id) = local_id {
                        if let Some(local_decl) = self.locals.get(local_id as usize) {
                            let inferred = local_decl.ty.clone();
                            if matches!(mutability, Mutability::Mut) {
                                self.lowering.emit_warning(
                                    arg.value.span,
                                    "mutable reference taken from non-mutable local in call",
                                );
                            }
                            arg_types.push(inferred.clone());
                            lowered_args.push(operand.operand);
                            continue;
                        }
                    }
                }
                if self.lowering.has_unresolved_ty(expected_ty) {
                    operand.ty.clone()
                } else {
                    expected_ty.clone()
                }
            } else {
                operand.ty.clone()
            };
            lowered_args.push(operand.operand);
            arg_types.push(inferred_ty);
        }

        if let Some(def_id) = generic_def_id {
            if let Some(function) = self.lowering.generic_function_defs.get(&def_id).cloned() {
                let is_result_ctor = matches!(callee_tail, Some("Ok" | "Err"));
                if explicit_args.is_empty() {
                    if let Some(inferred) = self.infer_explicit_args_from_expected_return(
                        &function,
                        destination.as_ref().map(|(_, ty)| ty),
                    ) {
                        explicit_args = inferred;
                    }
                }
                let is_unresolved =
                    |ty: &Ty| matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_));
                let needs_result_ctor_infer = is_result_ctor
                    && (explicit_args.is_empty()
                        || explicit_args.iter().any(|ty| is_unresolved(ty)));
                if needs_result_ctor_infer {
                    let expected_for_infer = destination.as_ref().map(|(_, ty)| ty);
                    let mut inferred_args = if explicit_args.is_empty() {
                        expected_for_infer.and_then(|expected_ty| {
                            self.explicit_args_from_expected_result_ty(expected_ty)
                        })
                    } else {
                        Some(explicit_args.clone())
                    };
                    if inferred_args.is_none() {
                        let needs_fallback = match expected_for_infer {
                            Some(expected_ty) => self.lowering.has_unresolved_ty(expected_ty),
                            None => true,
                        };
                        if needs_fallback {
                            let fallback = self.lower_type_expr(&self.function.sig.output);
                            let fallback_args =
                                self.explicit_args_from_expected_result_ty(&fallback);
                            let fallback_usable = fallback_args
                                .as_ref()
                                .map(|args| args.iter().any(|ty| !is_unresolved(ty)))
                                .unwrap_or(false);
                            if fallback_usable || !self.lowering.has_unresolved_ty(&fallback) {
                                inferred_args = fallback_args;
                            }
                        }
                    }
                    if inferred_args.is_none() {
                        let fallback = self.lower_type_expr(&self.function.sig.output);
                        let fallback_args = self.explicit_args_from_expected_result_ty(&fallback);
                        let fallback_usable = fallback_args
                            .as_ref()
                            .map(|args| args.iter().any(|ty| !is_unresolved(ty)))
                            .unwrap_or(false);
                        if fallback_usable || !self.lowering.has_unresolved_ty(&fallback) {
                            inferred_args = fallback_args;
                        }
                    }
                    if inferred_args.is_none() {
                        if let hir::TypeExprKind::Path(path) = &self.function.sig.output.kind {
                            if self.lowering.is_result_path(path) {
                                if let Some(args) =
                                    path.segments.last().and_then(|seg| seg.args.as_ref())
                                {
                                    let mut output_args = Vec::new();
                                    for arg in &args.args {
                                        let hir::GenericArg::Type(type_arg) = arg else {
                                            continue;
                                        };
                                        output_args.push(self.lower_type_expr(type_arg));
                                    }
                                    if output_args.len() == function.sig.generics.params.len() {
                                        inferred_args = Some(output_args);
                                    }
                                }
                            }
                        }
                    }
                    if let Some(mut inferred) = inferred_args {
                        if inferred.len() == function.sig.generics.params.len() {
                            if inferred.iter().any(|ty| is_unresolved(ty)) {
                                let fallback = self.lower_type_expr(&self.function.sig.output);
                                if !self.lowering.has_unresolved_ty(&fallback) {
                                    if let Some(fallback_args) =
                                        self.explicit_args_from_expected_result_ty(&fallback)
                                    {
                                        for (idx, inferred_ty) in inferred.iter_mut().enumerate() {
                                            if !is_unresolved(inferred_ty) {
                                                continue;
                                            }
                                            let Some(fallback_ty) = fallback_args.get(idx) else {
                                                continue;
                                            };
                                            if is_unresolved(fallback_ty) {
                                                continue;
                                            }
                                            *inferred_ty = fallback_ty.clone();
                                        }
                                    }
                                }
                            }
                            if let Some(arg_ty) = arg_types.get(0) {
                                let arg_ty = self.lowering.unwrap_expr_actual_ty(arg_ty);
                                let usable_arg = !is_unresolved(arg_ty);
                                if usable_arg {
                                    match callee_tail {
                                        Some("Ok") => inferred[0] = arg_ty.clone(),
                                        Some("Err") => inferred[1] = arg_ty.clone(),
                                        _ => {}
                                    }
                                }
                            }
                            if inferred.iter().all(|ty| !is_unresolved(ty)) {
                                explicit_args = inferred;
                            } else {
                                explicit_args = inferred;
                            }
                        }
                    }
                    let needs_local_fill = explicit_args.len()
                        == function.sig.generics.params.len()
                        && explicit_args.iter().any(|ty| is_unresolved(ty));
                    if explicit_args.is_empty() || needs_local_fill {
                        if let Some(local_return) = self.locals.get(0).map(|local| &local.ty) {
                            if let Some(local_args) =
                                self.explicit_args_from_expected_result_ty(local_return)
                            {
                                if local_args.len() == function.sig.generics.params.len() {
                                    if explicit_args.is_empty() {
                                        explicit_args = local_args;
                                    } else {
                                        for (idx, local_ty) in local_args.into_iter().enumerate() {
                                            if let Some(explicit_ty) = explicit_args.get_mut(idx) {
                                                if is_unresolved(explicit_ty)
                                                    && !is_unresolved(&local_ty)
                                                {
                                                    *explicit_ty = local_ty;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if explicit_args.is_empty() {
                        let mut output_ty = &self.function.sig.output;
                        while let Some(inner) = self.lowering.expr_inner_type_expr(output_ty) {
                            output_ty = inner;
                        }
                        if let hir::TypeExprKind::Path(path) = &output_ty.kind {
                            if self.lowering.is_result_path(path) {
                                if let Some(args) =
                                    path.segments.last().and_then(|seg| seg.args.as_ref())
                                {
                                    let mut output_args = Vec::new();
                                    for arg in &args.args {
                                        let hir::GenericArg::Type(type_arg) = arg else {
                                            continue;
                                        };
                                        output_args.push(self.lower_type_expr(type_arg));
                                    }
                                    if output_args.len() == function.sig.generics.params.len()
                                        && output_args.iter().all(|ty| !is_unresolved(ty))
                                    {
                                        explicit_args = output_args;
                                    } else if output_args.len() >= 2 {
                                        let mut stitched = Vec::new();
                                        if let Some(arg_ty) = arg_types.get(0) {
                                            let arg_ty =
                                                self.lowering.unwrap_expr_actual_ty(arg_ty);
                                            if matches!(
                                                arg_ty.kind,
                                                TyKind::Infer(_) | TyKind::Error(_)
                                            ) {
                                                stitched.push(output_args[0].clone());
                                            } else {
                                                stitched.push(arg_ty.clone());
                                            }
                                        } else {
                                            stitched.push(output_args[0].clone());
                                        }
                                        stitched.push(output_args[1].clone());
                                        if stitched.len() == function.sig.generics.params.len()
                                            && stitched.iter().all(|ty| !is_unresolved(ty))
                                        {
                                            explicit_args = stitched;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if is_result_ctor
                    && explicit_args.len() == function.sig.generics.params.len()
                    && explicit_args
                        .iter()
                        .any(|ty| matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)))
                {
                    let fallback = self.lower_type_expr(&self.function.sig.output);
                    if let Some(fallback_args) =
                        self.explicit_args_from_expected_result_ty(&fallback)
                    {
                        for (idx, fallback_arg) in fallback_args.into_iter().enumerate() {
                            let Some(explicit_ty) = explicit_args.get_mut(idx) else {
                                continue;
                            };
                            if matches!(explicit_ty.kind, TyKind::Infer(_) | TyKind::Error(_))
                                && !matches!(fallback_arg.kind, TyKind::Infer(_) | TyKind::Error(_))
                            {
                                *explicit_ty = fallback_arg;
                            }
                        }
                    }
                }
                if is_result_ctor && explicit_args.len() == function.sig.generics.params.len() {
                    let is_unresolved =
                        |ty: &Ty| matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_));
                    if explicit_args.iter().any(|ty| is_unresolved(ty)) {
                        if let Some(arg_ty) = arg_types.get(0) {
                            let arg_ty = self.lowering.unwrap_expr_actual_ty(arg_ty);
                            if !is_unresolved(arg_ty) {
                                match callee_tail {
                                    Some("Ok") => explicit_args[0] = arg_ty.clone(),
                                    Some("Err") if explicit_args.len() > 1 => {
                                        explicit_args[1] = arg_ty.clone();
                                    }
                                    _ => {}
                                }
                            }
                        }
                        if explicit_args.len() >= 1
                            && is_unresolved(&explicit_args[0])
                            && matches!(callee_tail, Some("Err"))
                        {
                            explicit_args[0] = MirLowering::unit_ty();
                        }
                        if explicit_args.len() >= 2 && is_unresolved(&explicit_args[1]) {
                            explicit_args[1] = self.lowering.error_ty();
                        }
                    }
                }
                let mut fallback_expected_return: Option<Ty> = None;
                let mut expected_return_for_specialization: Option<Ty> =
                    match destination.as_ref().map(|(_, ty)| ty) {
                        Some(expected_ty) => {
                            let mut needs_fallback = self.lowering.has_unresolved_ty(expected_ty);
                            if is_result_ctor {
                                if let Some(args) =
                                    self.explicit_args_from_expected_result_ty(expected_ty)
                                {
                                    let is_unresolved = |ty: &Ty| {
                                        matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_))
                                    };
                                    let generics_len = function.sig.generics.params.len();
                                    if args.len() == generics_len
                                        && args.iter().all(|ty| !is_unresolved(ty))
                                    {
                                        needs_fallback = false;
                                    }
                                }
                            } else if !needs_fallback {
                                if let Some(args) =
                                    self.explicit_args_from_expected_result_ty(expected_ty)
                                {
                                    needs_fallback = args.iter().any(|ty| {
                                        matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_))
                                    });
                                }
                            }
                            // `self.function` is *this* body's own function (see
                            // `BodyBuilder::function`) -- using its return type
                            // as a stand-in for the callee's expected return
                            // only makes sense when this call is itself in tail
                            // position (its result becomes this function's own
                            // return value), which is exactly the `is_result_ctor`
                            // case this fallback was written for (`Ok(x)`/`Err(x)`
                            // constructed as the tail expression of a function
                            // that itself returns `Result<T, E>`). Applying it to
                            // an arbitrary generic call (e.g. a plain
                            // `let r = identity(10);`) substitutes a completely
                            // unrelated function's return type -- observed
                            // hard-failing generic calls with "conflicting
                            // generic inference" (`T` inferred as the argument's
                            // real type from the call site, vs `T` clobbered by
                            // this function's own unrelated return type).
                            if needs_fallback && is_result_ctor {
                                let fallback = self.lower_type_expr(&self.function.sig.output);
                                if !self.lowering.has_unresolved_ty(&fallback) {
                                    fallback_expected_return = Some(fallback.clone());
                                    Some(fallback)
                                } else {
                                    Some(expected_ty.clone())
                                }
                            } else {
                                Some(expected_ty.clone())
                            }
                        }
                        None => {
                            if is_result_ctor {
                                let fallback = self.lower_type_expr(&self.function.sig.output);
                                fallback_expected_return = Some(fallback.clone());
                                Some(fallback)
                            } else {
                                None
                            }
                        }
                    };
                if is_result_ctor {
                    let sig_expected = self.lower_type_expr(&self.function.sig.output);
                    if let Some(args) = self.explicit_args_from_expected_result_ty(&sig_expected) {
                        if args.len() == function.sig.generics.params.len() {
                            fallback_expected_return = Some(sig_expected.clone());
                            expected_return_for_specialization = Some(sig_expected);
                        }
                    }
                    let is_unresolved =
                        |ty: &Ty| matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_));
                    let needs_sig_fallback = explicit_args.is_empty()
                        || explicit_args.iter().any(|ty| is_unresolved(ty));
                    if needs_sig_fallback {
                        if fallback_expected_return.is_none() {
                            let fallback = self.lower_type_expr(&self.function.sig.output);
                            let fallback_args =
                                self.explicit_args_from_expected_result_ty(&fallback);
                            let fallback_usable = fallback_args
                                .as_ref()
                                .map(|args| args.iter().any(|ty| !is_unresolved(ty)))
                                .unwrap_or(false);
                            if fallback_usable || !self.lowering.has_unresolved_ty(&fallback) {
                                fallback_expected_return = Some(fallback.clone());
                            }
                        }
                        if let Some(fallback) = fallback_expected_return.as_ref() {
                            expected_return_for_specialization = Some(fallback.clone());
                        }
                    }
                }
                if is_result_ctor {
                    let needs_forced = expected_return_for_specialization
                        .as_ref()
                        .map(|ty| self.lowering.has_unresolved_ty(ty))
                        .unwrap_or(true);
                    if needs_forced {
                        if fallback_expected_return.is_none() {
                            let fallback = self.lower_type_expr(&self.function.sig.output);
                            fallback_expected_return = Some(fallback.clone());
                        }
                        if let Some(fallback) = fallback_expected_return.as_ref() {
                            expected_return_for_specialization = Some(fallback.clone());
                        }
                    }
                }
                let info = self.lowering.ensure_function_specialization(
                                        def_id,
                    &function,
                    &explicit_args,
                    &arg_types,
                    expected_return_for_specialization.as_ref(),
                    expr.span,
                )?;
                func_operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: info.fn_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::FnDef(info.def_id, info.substs.clone()),
                });
                sig = info.sig.clone();
                callee_name = Some(info.name.clone());

                for (idx, arg) in args.iter().enumerate() {
                    let Some(expected_ty) = sig.inputs.get(idx) else {
                        continue;
                    };
                    if !matches!(expected_ty.kind, TyKind::FnPtr(_)) {
                        continue;
                    }
                    let operand = self.lower_operand(&arg.value, Some(expected_ty))?;
                    arg_types[idx] = operand.ty.clone();
                    lowered_args[idx] = operand.operand;
                }
            }
        }

        if let Some(def) = generic_method_def {
            let info = self.lowering.ensure_method_specialization(
                                &def,
                &explicit_args,
                &arg_types,
                destination.as_ref().map(|(_, ty)| ty),
                expr.span,
            )?;
            func_operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                ty: info.fn_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::FnDef(
                    info.def_id.ok_or_else(|| {
                        fp_core::error::Error::from("specialized method has no definition identity")
                    })?,
                    info.substs.clone(),
                ),
            });
            sig = info.sig.clone();
            callee_name = Some(info.fn_name.clone());
            associated_struct = info.struct_def;
        }

        if let Some(name) = callee_name.as_ref() {
            if self.lowering.synthetic_runtime_functions.contains(name) {
                let destination_ty = destination.as_ref().map(|(_, ty)| ty);
                let updated_sig = self.lowering.update_placeholder_signature(
                    name,
                    &sig,
                    &arg_types,
                    destination_ty,
                );

                if updated_sig != sig {
                    let symbol = Symbol::new(name.clone());
                    let literal = match &func_operand {
                        mir::Operand::Constant(constant) => match &constant.literal {
                            mir::ConstantKind::Global(_) => {
                                mir::ConstantKind::Global(mir::Path::from_symbol(symbol.clone()))
                            }
                            _ => mir::ConstantKind::Fn(symbol.clone()),
                        },
                        _ => mir::ConstantKind::Fn(symbol.clone()),
                    };
                    let function_ty = match &literal {
                        mir::ConstantKind::Global(_) => {
                            self.lowering.c_function_pointer_ty(&updated_sig)
                        }
                        _ => self.lowering.function_pointer_ty(&updated_sig),
                    };
                    func_operand = mir::Operand::Constant(mir::Constant {
                        span: callee.span,
                        ty: function_ty,
                        user_ty: None,
                        literal,
                    });
                    sig = updated_sig;

                    for (idx, expected_input) in sig.inputs.iter().enumerate() {
                        if let Some(original_ty) = arg_types.get(idx) {
                            if MirLowering::is_unit_ty(original_ty)
                                && matches!(
                                    expected_input.kind,
                                    TyKind::Ref(_, _, _) | TyKind::RawPtr(_)
                                )
                            {
                                lowered_args[idx] = mir::Operand::Constant(mir::Constant {
                                    span: callee.span,
                                    ty: expected_input.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::UInt(0),
                                });
                            }
                        }
                    }

                    for (idx, operand) in lowered_args.iter_mut().enumerate() {
                        if let Some(expected_input) = sig.inputs.get(idx) {
                            match operand {
                                mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty =
                                            expected_input.clone();
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        for (idx, operand) in lowered_args.iter_mut().enumerate() {
            let Some(expected_ty) = sig.inputs.get(idx) else {
                continue;
            };
            if self.enum_layout_for_ty(expected_ty, expr.span).is_none() {
                continue;
            }

            let place = match operand {
                mir::Operand::Copy(place) | mir::Operand::Move(place) => place.clone(),
                _ => continue,
            };

            let local_ty = self
                .locals
                .get(place.local as usize)
                .map(|local| local.ty.clone())
                .unwrap_or_else(|| expected_ty.clone());
            let struct_def = self.local_structs.get(&place.local).copied();

            if let Some((variant, layout)) =
                self.enum_variant_for_payload(expected_ty, &local_ty, struct_def)
            {
                let nominal_ty = self.lowering.nominal_enum_ty(&layout);
                let local_id = self.allocate_temp(nominal_ty.clone(), expr.span);
                let enum_place = mir::Place::from_local(local_id);
                self.assign_enum_variant_from_place(
                    enum_place.clone(),
                    &variant,
                    &layout,
                    Some(expected_ty),
                    place,
                    expr.span,
                )?;
                *operand = mir::Operand::Move(enum_place);
                if let Some(arg_type) = arg_types.get_mut(idx) {
                    *arg_type = nominal_ty;
                }
            }
        }

        let continue_block = self.new_block();

        let (mir_destination, place_info) = match destination {
            Some((place, _ty)) => {
                let result_ty = sig.output.clone();
                let struct_def = associated_struct.or_else(|| self.struct_def_from_ty(&result_ty));
                // Only the call's *own* result type is `result_ty` — if
                // `place` is a projection (e.g. `self.inner = f()`, a field
                // of a larger local), `place.local` names the *base* local
                // (`self`), not the destination value itself. Overwriting
                // `locals[place.local].ty`/`local_structs` here would
                // silently replace that base local's own declared type with
                // the call's unrelated result type.
                if place.projection.is_empty() && (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(place.local, def_id);
                    }
                }
                let info = PlaceInfo {
                    place: place.clone(),
                    ty: result_ty,
                    struct_def,
                };
                (Some((place, continue_block)), Some(info))
            }
            None => {
                let ty = sig.output.clone();
                let temp = self.allocate_temp(ty, expr.span);
                let place = mir::Place::from_local(temp);
                (Some((place, continue_block)), None)
            }
        };

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args: lowered_args,
                destination: mir_destination.clone(),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: expr.span,
            },
        };

        self.blocks[self.current_block as usize].terminator = Some(terminator);
        self.current_block = continue_block;

        if place_info.is_none() {
            if let Some((place, _)) = mir_destination {
                let result_ty = sig.output.clone();
                if place.projection.is_empty() && (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                    let struct_def = associated_struct.or_else(|| self.struct_def_from_ty(&result_ty));
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(place.local, def_id);
                    }
                }
            }
        }

        Ok(place_info)
    }

    fn param_names_for_callee(&self, path: &hir::Path) -> Option<Vec<hir::Symbol>> {
        match &path.res {
            Some(hir::Res::Def(def_id)) => self.param_names_for_def_id(*def_id).or_else(|| {
                self.lowering
                    .method_defs_by_def
                    .get(def_id)
                    .and_then(|def| self.param_names_from_params(&def.function.sig.inputs))
            }),
            _ => None,
        }
    }

    fn param_names_for_def_id(&self, def_id: hir::DefId) -> Option<Vec<hir::Symbol>> {
        let item = self.lowering.hir_def_map.get(&def_id)?;
        match &item.kind {
            hir::ItemKind::Function(function) => self.param_names_from_params(&function.sig.inputs),
            _ => None,
        }
    }

    fn param_names_from_params(&self, params: &[hir::Param]) -> Option<Vec<hir::Symbol>> {
        let mut names = Vec::with_capacity(params.len());
        for param in params {
            match &param.pat.kind {
                hir::PatKind::Binding { name, .. } => names.push(name.clone()),
                _ => return None,
            }
        }
        Some(names)
    }

    /// Returns the callee's `(abi, is_extern)` together, both read off the
    /// same `hir::ItemKind::Function` — so a caller that resolves via the
    /// fast `Res::Def` path (the common case: an ordinary already-resolved
    /// function reference) gets both pieces of information from one O(1)
    /// lookup, instead of needing a *second* full linear scan over every
    /// item in the program just to learn `is_extern` (see `lower_call`,
    /// which used to always pay that second scan regardless of whether
    /// this fast path already succeeded).
    fn callee_abi_from_path(&self, path: &hir::Path) -> Option<(hir::Abi, bool)> {
        if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
            if let Some(item) = self.lowering.hir_def_map.get(def_id) {
                if let hir::ItemKind::Function(func) = &item.kind {
                    return Some((func.sig.abi.clone(), func.is_extern));
                }
            }
        }
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);
        let qualified = resolved_path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        if qualified.is_empty() {
            return None;
        }
        for item in self.lowering.hir_def_map.values() {
            if let hir::ItemKind::Function(func) = &item.kind {
                if func.sig.name.as_str() == qualified {
                    return Some((func.sig.abi.clone(), func.is_extern));
                }
            }
        }
        let tail = resolved_path.segments.last().map(|seg| seg.name.as_str());
        if let Some(tail) = tail {
            let mut candidate: Option<(hir::Abi, bool)> = None;
            for item in self.lowering.hir_def_map.values() {
                if let hir::ItemKind::Function(func) = &item.kind {
                    let name = func.sig.name.as_str();
                    let matches_tail = name == tail || name.ends_with(&format!("::{}", tail));
                    if matches_tail {
                        if candidate.is_some() {
                            return None;
                        }
                        candidate = Some((func.sig.abi.clone(), func.is_extern));
                    }
                }
            }
            if candidate.is_some() {
                return candidate;
            }
        }
        None
    }

    fn reorder_named_call_args(
        &mut self,
        args: &[hir::CallArg],
        param_names: &[hir::Symbol],
        span: Span,
    ) -> Result<Vec<hir::CallArg>> {
        if args.len() != param_names.len() {
            return Ok(args.to_vec());
        }

        let mut has_named = false;
        for (index, arg) in args.iter().enumerate() {
            let expected = format!("arg{}", index);
            if arg.name.as_str() != expected {
                has_named = true;
                break;
            }
        }

        if !has_named {
            return Ok(args.to_vec());
        }

        let mut index_map = HashMap::new();
        for (index, name) in param_names.iter().enumerate() {
            index_map.insert(name.as_str().to_string(), index);
        }

        let mut reordered: Vec<Option<hir::CallArg>> = vec![None; param_names.len()];
        for (index, arg) in args.iter().enumerate() {
            let mut target = None;
            let expected = format!("arg{}", index);
            if arg.name.as_str() == expected {
                target = Some(index);
            } else if let Some(mapped) = index_map.get(arg.name.as_str()) {
                target = Some(*mapped);
            }

            let Some(slot) = target else {
                self.lowering.emit_error(
                    span,
                    format!("unknown named argument `{}` in call", arg.name),
                );
                return Ok(args.to_vec());
            };

            if slot >= reordered.len() || reordered[slot].is_some() {
                self.lowering.emit_error(
                    span,
                    format!("duplicate or out-of-range argument `{}`", arg.name),
                );
                return Ok(args.to_vec());
            }
            reordered[slot] = Some(arg.clone());
        }

        let mut flattened = Vec::with_capacity(reordered.len());
        for arg in reordered {
            let Some(value) = arg else {
                self.lowering
                    .emit_error(span, "missing named argument in call");
                return Ok(args.to_vec());
            };
            flattened.push(value);
        }

        Ok(flattened)
    }

    fn resolve_callee(
        &mut self,
        callee: &hir::Expr,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        match &callee.kind {
            hir::ExprKind::Path(path) => self.resolve_callee_path(callee, path),
            hir::ExprKind::FieldAccess(_, _) => {
                let operand = self.lower_operand(callee, None)?;
                if let TyKind::FnPtr(poly_fn_sig) = &operand.ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    return Ok((operand.operand, sig, None));
                }
                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "call target must be a function pointer, found {:?}",
                        operand.ty.kind
                    ),
                );
                Ok((
                    mir::Operand::Constant(self.lowering.error_constant(callee.span)),
                    mir::FunctionSig {
                        inputs: Vec::new(),
                        output: Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        },
                    },
                    None,
                ))
            }
            _ => {
                let operand = self.lower_operand(callee, None)?;
                if let TyKind::FnPtr(poly_fn_sig) = &operand.ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    return Ok((operand.operand, sig, None));
                }
                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "call target must be a function pointer, found {:?}",
                        operand.ty.kind
                    ),
                );
                Ok((
                    mir::Operand::Constant(self.lowering.error_constant(callee.span)),
                    mir::FunctionSig {
                        inputs: Vec::new(),
                        output: Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        },
                    },
                    None,
                ))
            }
        }
    }

    fn resolve_callee_path(
        &mut self,
        callee: &hir::Expr,
        path: &hir::Path,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);

        // Handle local variables (e.g., function parameters) as indirect calls
        if let Some(hir::Res::Local(hir_id)) = &resolved_path.res {
            if let Some(local_id) = self.local_map.get(hir_id) {
                let local_id = *local_id;
                let ty = self.locals[local_id as usize].ty.clone();

                // Extract function signature from function pointer type
                if let TyKind::FnPtr(poly_fn_sig) = &ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    let place = mir::Place::from_local(local_id);
                    let operand = mir::Operand::copy(place);
                    return Ok((operand, sig, None));
                }

                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "local variable is not a function pointer, has type: {:?}",
                        ty
                    ),
                );
            } else {
                self.lowering
                    .emit_error(callee.span, "local variable not found in local_map");
            }
        }

        if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
            if self.lowering.function_sigs.get(def_id).is_none() {
                // Not yet lowered/registered — e.g. a same-package function
                // this MIR-lowering pass hasn't proactively reached yet
                // (out-of-order cross-module reference), or one deliberately
                // never visited at all (the comptime-probe's item-scoped
                // entry point, `transform_comptime_request`, never walks an
                // unrelated item's body). Lower it on demand now, mirroring
                // `register_const_value`/`try_lazily_register_adt`'s
                // existing lazy pattern for the same reason.
                self.lowering.ensure_function_lowered(*def_id)?;
            }
            if let Some(sig) = self.lowering.function_sigs.get(def_id).cloned() {
                let name = self
                    .lowering
                    .hir_def_map
                    .get(def_id)
                    .and_then(|item| match &item.kind {
                        hir::ItemKind::Function(func) => Some(func.sig.name.clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| hir::Symbol::new(format!("fn#{}", def_id)));
                let ty = self.lowering.function_pointer_ty(&sig);
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::FnDef(*def_id, Vec::new()),
                });
                return Ok((operand, sig, Some(String::from(name))));
            }
        }

        if resolved_path.segments.len() >= 2 {
            let method_name = resolved_path
                .segments
                .last()
                .expect("segments len checked")
                .name
                .clone();
            let struct_name = resolved_path
                .segments
                .get(resolved_path.segments.len() - 2)
                .expect("segments len checked")
                .name
                .clone();
            if let Some(info) = self
                .lowering
                .struct_methods
                .get(&String::from(struct_name.clone()))
                .and_then(|methods| methods.get(&String::from(method_name.clone())))
            {
                let literal = match info.def_id {
                    Some(def_id) => mir::ConstantKind::FnDef(def_id, Vec::new()),
                    None => mir::ConstantKind::Fn(mir::Symbol::new(info.fn_name.clone())),
                };
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: info.fn_ty.clone(),
                    user_ty: None,
                    literal,
                });
                let qualified_name = format!("{}::{}", struct_name, method_name);
                return Ok((operand, info.sig.clone(), Some(qualified_name)));
            }
        }

        // Built lazily here, not at function entry — every fast path above
        // (local-variable indirect call, resolved `Def`, and the
        // struct-method `>= 2` segments case) returns before ever needing
        // this joined name.
        let name = resolved_path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");

        if let Some(sig) = self.lowering.runtime_functions.get(&name).cloned() {
            let ty = self.lowering.c_function_pointer_ty(&sig);
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                ty: ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Global(mir::Path::new(
                    resolved_path
                        .segments
                        .iter()
                        .map(|segment| mir::Symbol::new(segment.name.clone()))
                        .collect(),
                )),
            });
            return Ok((operand, sig, Some(name)));
        }

        if let Some(hir::Res::Def(def_id)) = resolved_path.res.as_ref() {
            // `ensure_method_info` is the uniform lookup — it resolves a
            // method in this package or any dependency's the same way
            // (mirrors `compute_adt_layout`'s existing "check the cache,
            // lazily register on a miss" shape for ADTs). A hit only
            // proves the *signature* is known — the body itself may not
            // be lowered yet if this pass never proactively reached this
            // method's own `impl` item (e.g. the comptime-probe's
            // item-scoped entry point, which deliberately never walks
            // unrelated items, or a dependency's own method, whose body
            // belongs to that package's own separate compile). Ensure it
            // now, on demand, before referencing it — a no-op for a
            // cross-package method, which `ensure_method_lowered`'s
            // existing `current_package_id` guard already handles.
            if let Some(info) = self.lowering.ensure_method_info(*def_id) {
                self.lowering.ensure_method_lowered(*def_id)?;
                let literal = match info.def_id {
                    Some(def_id) => mir::ConstantKind::FnDef(def_id, Vec::new()),
                    None => mir::ConstantKind::Fn(mir::Symbol::new(info.fn_name.clone())),
                };
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: info.fn_ty.clone(),
                    user_ty: None,
                    literal,
                });
                return Ok((operand, info.sig.clone(), Some(info.fn_name.clone())));
            }
        }

        self.lowering.emit_error(
            callee.span,
            format!("unresolved call target `{}` during MIR lowering", name),
        );
        let sig = self.lowering.placeholder_function_sig(&name);
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let operand = mir::Operand::Constant(mir::Constant {
            span: callee.span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(Symbol::new(name.clone())),
        });
        Ok((operand, sig, Some(name)))
    }

    fn lower_operand(&mut self, expr: &hir::Expr, expected: Option<&Ty>) -> Result<OperandInfo> {
        let inferred_expected = if expected.is_none() {
            self.lowering.typeck_exprs.get(&expr.hir_id).cloned()
        } else {
            None
        };
        let expected = expected.or(inferred_expected.as_ref());
        if self.active_exprs.contains(&expr.hir_id) {
            let message = "recursive expression detected during MIR lowering";
            self.lowering.emit_error(expr.span, message);
            return Err(fp_core::error::Error::from(message));
        }
        self.active_exprs.insert(expr.hir_id);
        let _guard = ExprRecursionGuard::new(&mut self.active_exprs, expr.hir_id);
        if matches!(
            expr.kind,
            hir::ExprKind::FieldAccess(_, _) | hir::ExprKind::MethodCall(_, _, _)
        ) {
            if let Some(constant) =
                self.lowering
                    .lower_const_expr(expr, expected, None)
            {
                let ty = expected
                    .cloned()
                    .or_else(|| self.constant_ty_from_constant(&constant))
                    .unwrap_or_else(|| self.lowering.error_ty());
                return Ok(OperandInfo {
                    operand: mir::Operand::Constant(constant),
                    ty,
                });
            }
        }
        if let Some(place) = self.lower_place(expr)? {
            if let Some(expected_ty) = expected {
                if let Some((variant, layout)) =
                    self.enum_variant_for_payload(expected_ty, &place.ty, place.struct_def)
                {
                    let nominal_ty = self.lowering.nominal_enum_ty(&layout);
                    let local_id = self.allocate_temp(nominal_ty.clone(), expr.span);
                    let enum_place = mir::Place::from_local(local_id);
                    self.assign_enum_variant_from_place(
                        enum_place.clone(),
                        &variant,
                        &layout,
                        Some(expected_ty),
                        place.place.clone(),
                        expr.span,
                    )?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(enum_place),
                        ty: nominal_ty,
                    });
                }
            }
            if let Some(expected_ty) = expected {
                if let TyKind::Ref(region, inner, mutability) = &expected_ty.kind {
                    if matches!(place.ty.kind, TyKind::Ref(_, _, _)) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(place.place.clone()),
                            ty: place.ty,
                        });
                    }

                    let resolved_inner = if self.lowering.has_unresolved_ty(inner.as_ref())
                        && !self.lowering.has_unresolved_ty(&place.ty)
                    {
                        place.ty.clone()
                    } else {
                        inner.as_ref().clone()
                    };
                    let ref_ty = if resolved_inner == *inner.as_ref() {
                        expected_ty.clone()
                    } else {
                        Ty {
                            kind: TyKind::Ref(
                                region.clone(),
                                Box::new(resolved_inner),
                                *mutability,
                            ),
                        }
                    };
                    let borrow_kind = match mutability {
                        Mutability::Mut => mir::BorrowKind::Mut {
                            allow_two_phase_borrow: false,
                        },
                        Mutability::Not => mir::BorrowKind::Shared,
                    };
                    let temp_local = self.allocate_temp(ref_ty.clone(), expr.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    let assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Ref((), borrow_kind, place.place.clone()),
                        ),
                    };
                    self.push_statement(assign);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(temp_place),
                        ty: ref_ty,
                    });
                }
            }
            return Ok(OperandInfo {
                operand: mir::Operand::copy(place.place.clone()),
                ty: place.ty,
            });
        }

        match &expr.kind {
            hir::ExprKind::Reference(reference) => {
                self.lower_reference_operand(reference, expr.span)
            }
            hir::ExprKind::Query(query) => {
                let query_ty = expected.cloned().ok_or_else(|| {
                    fp_core::error::Error::from("query expression requires an expected result type")
                })?;
                let local_id = self.allocate_temp(query_ty.clone(), expr.span);
                let place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Query(mir::Query {
                            origin: query.origin.clone(),
                            ir: query.ir.clone(),
                            span: query.span,
                        }),
                    ),
                });
                Ok(OperandInfo {
                    operand: mir::Operand::copy(place),
                    ty: query_ty,
                })
            }
            hir::ExprKind::Let(pat, ty, init) => {
                self.lower_let_expr(pat, ty, init, expr.span)?;
                let unit_ty = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
                Ok(OperandInfo::constant(
                    expr.span,
                    unit_ty.clone(),
                    mir::ConstantKind::Val(mir::ConstValue::Unit),
                ))
            }
            hir::ExprKind::Literal(lit) => {
                let (literal, ty) = self.lower_literal(lit, expected);
                Ok(OperandInfo {
                    operand: mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: ty.clone(),
                        user_ty: None,
                        literal,
                    }),
                    ty,
                })
            }
            hir::ExprKind::Path(path) => {
                let mut resolved_path = path.clone();
                self.resolve_self_path(&mut resolved_path);
                let explicit_args = resolved_path
                    .segments
                    .iter()
                    .find_map(|segment| segment.args.as_ref())
                    .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                let has_explicit_args = !explicit_args.is_empty();
                let expected_sig = expected.and_then(|ty| {
                    if let TyKind::FnPtr(poly_fn_sig) = &ty.kind {
                        let sig = &poly_fn_sig.binder.value;
                        Some(mir::FunctionSig {
                            inputs: sig.inputs.iter().map(|t| (**t).clone()).collect(),
                            output: (*sig.output).clone(),
                        })
                    } else {
                        None
                    }
                });
                if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                    if has_explicit_args {
                        if let Some(function) =
                            self.lowering.generic_function_defs.get(def_id).cloned()
                        {
                            let info = self
                                .lowering
                                .ensure_function_specialization_from_explicit_args(
                                                                        *def_id,
                                    &function,
                                    &explicit_args,
                                    expr.span,
                                )?;
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::FnDef(
                                        info.def_id,
                                        info.substs.clone(),
                                    ),
                                }),
                                ty: info.fn_ty,
                            });
                        }
                    }
                    if let Some(expected_sig) = expected_sig.as_ref() {
                        if let Some(function) =
                            self.lowering.generic_function_defs.get(def_id).cloned()
                        {
                            let expected_has_opaque = expected_sig
                                .inputs
                                .iter()
                                .any(|ty| self.lowering.is_opaque_ty(ty))
                                || self.lowering.is_opaque_ty(&expected_sig.output);
                            if expected_has_opaque {
                                let fn_ty = self.lowering.function_pointer_ty(expected_sig);
                                return Ok(OperandInfo {
                                    operand: mir::Operand::Constant(mir::Constant {
                                        span: expr.span,
                                        ty: fn_ty.clone(),
                                        user_ty: None,
                                        literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                            function.sig.name.as_str().to_string(),
                                        )),
                                    }),
                                    ty: fn_ty,
                                });
                            }
                            let info = self.lowering.ensure_function_specialization(
                                                                *def_id,
                                &function,
                                &[],
                                &expected_sig.inputs,
                                Some(&expected_sig.output),
                                expr.span,
                            )?;
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                        info.name.clone(),
                                    )),
                                }),
                                ty: info.fn_ty,
                            });
                        }
                    }
                    if let Some(const_info) = self.lowering.const_values.get(def_id).cloned() {
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(const_info.typed_value()),
                            ty: const_info.ty,
                        });
                    }
                    if let Some((name, ty)) = self.lowering.executable_consts.get(def_id) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: ty.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Global(mir::Path::from_symbol(
                                    name.clone(),
                                )),
                            }),
                            ty: ty.clone(),
                        });
                    }
                    let const_def_item = self.lowering.hir_def_map.get(def_id).and_then(|item| {
                        match &item.kind {
                            hir::ItemKind::Const(konst) => Some(konst.clone()),
                            _ => None,
                        }
                    });
                    if let Some(konst) = const_def_item {
                        self.lowering.register_const_value(*def_id, &konst);
                        if let Some(const_info) = self.lowering.const_values.get(def_id) {
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(const_info.typed_value()),
                                ty: const_info.ty.clone(),
                            });
                        }
                        let ty = self.lower_type_expr(&konst.ty);
                        let local_id = self.allocate_temp(ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.lower_expr_into_place(&konst.body.value, place.clone(), &ty)?;
                        if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                            self.local_structs.insert(local_id, struct_def);
                        }
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(place),
                            ty,
                        });
                    } else if let Some(konst) = self.const_items.get(def_id).cloned() {
                        let ty = self.lower_type_expr(&konst.ty);
                        let local_id = self.allocate_temp(ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.lower_expr_into_place(&konst.body.value, place.clone(), &ty)?;
                        if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                            self.local_structs.insert(local_id, struct_def);
                        }
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(place),
                            ty,
                        });
                    }
                    if let Some(variant) = self.lowering.enum_variants.get(def_id).cloned() {
                        let mut layout = expected.and_then(|ty| {
                            self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                        });
                        if layout.is_none() {
                            let args = resolved_path
                                .segments
                                .last()
                                .and_then(|segment| segment.args.as_ref())
                                .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                                .unwrap_or_default();
                            if !args.is_empty() {
                                layout = self.lowering.enum_layout_for_instance(
                                    variant.enum_def,
                                    &args,
                                    expr.span,
                                );
                            } else if let Some(expected_ty) = expected {
                                // `expected_ty` is often already the flattened
                                // tuple representation (not `Adt(enum, args)`),
                                // which `enum_layout_for_variant` above can't
                                // match on directly — try the exact-index
                                // lookup (via `enum_layout_for_ty`) before
                                // giving up and minting a fresh generic
                                // template below.
                                if let Some(layout_from_ty) =
                                    self.enum_layout_for_ty(expected_ty, expr.span)
                                {
                                    if layout_from_ty.def_id == variant.enum_def {
                                        layout = Some(layout_from_ty);
                                    }
                                }
                            }
                            if layout.is_none() {
                                layout = self
                                    .lowering
                                    .enum_layout_for_def(variant.enum_def, expr.span);
                            }
                        }
                        if let Some(layout) = layout {
                            return self.lower_enum_variant_value(
                                &variant,
                                &layout,
                                expected,
                                &[],
                                expr.span,
                            );
                        }
                        self.lowering.emit_error(
                            expr.span,
                            "unable to resolve enum layout for variant value",
                        );
                    }
                    let referenced_fn_sig = self.lowering.hir_def_map.get(def_id).and_then(|item| {
                        match &item.kind {
                            hir::ItemKind::Function(func) => Some(func.sig.clone()),
                            _ => None,
                        }
                    });
                    if let Some(fn_sig) = referenced_fn_sig {
                        // Function reference - create a function pointer constant
                        let sig = self.lowering.lower_function_sig(&fn_sig, None);
                        let fn_ty = self.lowering.function_pointer_ty(&sig);
                        let fn_name = fn_sig.name.clone();
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: fn_ty.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Fn(mir::Symbol::from(fn_name)),
                            }),
                            ty: fn_ty,
                        });
                    }
                }

                if resolved_path.res.is_none() {
                    if let Some(variant) = self.enum_variant_info_from_path(&resolved_path) {
                        let mut layout = expected.and_then(|ty| {
                            self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                        });
                        if layout.is_none() {
                            let args = resolved_path
                                .segments
                                .last()
                                .and_then(|segment| segment.args.as_ref())
                                .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                                .unwrap_or_default();
                            if !args.is_empty() {
                                layout = self.lowering.enum_layout_for_instance(
                                    variant.enum_def,
                                    &args,
                                    expr.span,
                                );
                            } else if let Some(expected_ty) = expected {
                                // `expected_ty` is often already the flattened
                                // tuple representation (not `Adt(enum, args)`),
                                // which `enum_layout_for_variant` above can't
                                // match on directly — try the exact-index
                                // lookup (via `enum_layout_for_ty`) before
                                // giving up and minting a fresh generic
                                // template below.
                                if let Some(layout_from_ty) =
                                    self.enum_layout_for_ty(expected_ty, expr.span)
                                {
                                    if layout_from_ty.def_id == variant.enum_def {
                                        layout = Some(layout_from_ty);
                                    }
                                }
                            }
                            if layout.is_none() {
                                layout = self
                                    .lowering
                                    .enum_layout_for_def(variant.enum_def, expr.span);
                            }
                        }
                        if let Some(layout) = layout {
                            return self.lower_enum_variant_value(
                                &variant,
                                &layout,
                                expected,
                                &[],
                                expr.span,
                            );
                        }
                        self.lowering.emit_error(
                            expr.span,
                            "unable to resolve enum layout for variant value",
                        );
                    }
                }

                if has_explicit_args {
                    let method_def = match resolved_path.res.as_ref() {
                        Some(hir::Res::Def(def_id)) => {
                            self.lowering.method_defs_by_def.get(def_id).cloned()
                        }
                        _ => None,
                    };
                    if let Some(def) = method_def {
                        let info = self
                            .lowering
                            .ensure_method_specialization_from_explicit_args(
                                                                &def,
                                &explicit_args,
                                expr.span,
                            )?;
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: info.fn_ty.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::FnDef(
                                    info.def_id.ok_or_else(|| {
                                        fp_core::error::Error::from(
                                            "specialized method has no definition identity",
                                        )
                                    })?,
                                    info.substs.clone(),
                                ),
                            }),
                            ty: info.fn_ty,
                        });
                    }
                }

                let name = resolved_path
                    .segments
                    .iter()
                    .map(|seg| seg.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::");
                // Type names used as values (i64, bool, str, etc.) —
                // return an opaque placeholder constant.
                if is_known_type_name(&name) {
                    let ty = self.lowering.error_ty();
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: ty.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Val(mir::ConstValue::Unit),
                        }),
                        ty,
                    });
                }
                Err(fp_core::error::Error::from(format!(
                    "unresolved value path during MIR lowering: `{name}`"
                )))
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = self.lower_type_expr(ty_expr);
                if let hir::ExprKind::Literal(hir::Lit::Integer(value)) = &inner.kind {
                    if matches!(target_ty.kind, TyKind::Int(_) | TyKind::Uint(_)) {
                        let (literal, ty) =
                            self.lower_literal(&hir::Lit::Integer(*value), Some(&target_ty));
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: ty.clone(),
                                user_ty: None,
                                literal,
                            }),
                            ty,
                        });
                    }
                }
                let local_id = self.allocate_temp(target_ty.clone(), expr.span);
                let place_local = mir::Place::from_local(local_id);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place_local.clone(),
                        mir::Rvalue::Cast(mir::CastKind::Misc, operand.operand, target_ty.clone()),
                    ),
                };
                self.push_statement(statement);
                Ok(OperandInfo {
                    operand: mir::Operand::copy(place_local),
                    ty: target_ty,
                })
            }
            hir::ExprKind::Slice(slice) => self.lower_slice_operand(slice, expr.span, expected),
            hir::ExprKind::Index(base, index) => {
                let mut resolved_const_base = None;
                if let hir::ExprKind::Path(path) = &base.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        if let Some(const_info) = self.lowering.const_values.get(def_id).cloned() {
                            if let Some((constant, ty)) = self.lowering.const_index_value(
                                expr.span,
                                &const_info.typed_value(),
                                index,
                            ) {
                                return Ok(OperandInfo {
                                    operand: mir::Operand::Constant(constant),
                                    ty,
                                });
                            }
                            resolved_const_base = Some(OperandInfo {
                                operand: mir::Operand::Constant(const_info.typed_value()),
                                ty: const_info.ty,
                            });
                        }
                        if let Some(konst) = self.const_items.get(def_id).cloned() {
                            let ty = self.lowering.lower_type_expr(&konst.ty);
                            if let Some(constant) = self.lowering.lower_const_expr(
                                &konst.body.value,
                                Some(&ty),
                                None,
                            ) {
                                if let Some((constant, ty)) = self.lowering.const_index_value(
                                    expr.span,
                                    &constant,
                                    index,
                                ) {
                                    return Ok(OperandInfo {
                                        operand: mir::Operand::Constant(constant),
                                        ty,
                                    });
                                }
                                resolved_const_base = Some(OperandInfo {
                                    operand: mir::Operand::Constant(constant),
                                    ty,
                                });
                            }
                        }
                    }
                }
                let base_info = match resolved_const_base {
                    Some(const_info) => const_info,
                    None => self.lower_operand(base, None)?,
                };
                if let Some(struct_def_id) = self.real_indexable_struct_def_id(&base_info.ty) {
                    let element_ty = expected.cloned().unwrap_or_else(|| self.lowering.error_ty());
                    let local_id = self.allocate_temp(element_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let result_ty = self.call_real_method_into_place(
                        struct_def_id,
                        "index",
                        base,
                        &[index],
                        local_place.clone(),
                        Some(&element_ty),
                        expr.span,
                    )?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: result_ty,
                    });
                }
                if self.is_list_container(&base_info.ty) {
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let index_operand = self.lower_operand(index, Some(&index_ty))?;
                    let element_ty = expected
                        .cloned()
                        .or_else(|| self.expect_array_element_ty(&base_info.ty))
                        .unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::List {
                        elem_ty: element_ty.clone(),
                        len: self
                            .local_id_from_expr(base)
                            .and_then(|id| self.container_locals.get(&id))
                            .and_then(|kind| match kind {
                                mir::ContainerKind::List { len, .. } => Some(*len),
                                _ => None,
                            })
                            .unwrap_or(0),
                    };
                    let local_id = self.allocate_temp(element_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::ContainerGet {
                                kind,
                                container: base_info.operand,
                                key: index_operand.operand,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: element_ty,
                    });
                }
                if self.is_map_container(&base_info.ty) {
                    let index_operand = self.lower_operand(index, None)?;
                    let mut value_ty = expected
                        .cloned()
                        .unwrap_or_else(|| self.lowering.error_ty());
                    let mut kind = mir::ContainerKind::Map {
                        key_ty: index_operand.ty.clone(),
                        value_ty: value_ty.clone(),
                        len: 0,
                    };
                    if let Some(local_id) = self.local_id_from_expr(base) {
                        if let Some(container_kind) = self.container_locals.get(&local_id) {
                            if let mir::ContainerKind::Map {
                                key_ty,
                                value_ty: entry_value_ty,
                                len,
                            } = container_kind
                            {
                                kind = mir::ContainerKind::Map {
                                    key_ty: key_ty.clone(),
                                    value_ty: entry_value_ty.clone(),
                                    len: *len,
                                };
                                value_ty = entry_value_ty.clone();
                            }
                        }
                    }
                    if let mir::ContainerKind::Map {
                        key_ty,
                        value_ty: entry_value_ty,
                        len,
                    } = &mut kind
                    {
                        if *len == 0 {
                            if let mir::Operand::Constant(constant) = &base_info.operand {
                                if let mir::ConstantKind::Val(value) = &constant.literal {
                                    match value {
                                        mir::ConstValue::Map {
                                            entries,
                                            key_ty: map_key_ty,
                                            value_ty: map_value_ty,
                                        } => {
                                            *len = entries.len() as u64;
                                            *key_ty = map_key_ty.clone();
                                            *entry_value_ty = map_value_ty.clone();
                                            value_ty = map_value_ty.clone();
                                        }
                                        mir::ConstValue::List { elements, elem_ty } => {
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    *len = elements.len() as u64;
                                                    *key_ty = (*fields[0].clone()).clone();
                                                    *entry_value_ty = (*fields[1].clone()).clone();
                                                    value_ty = (*fields[1].clone()).clone();
                                                }
                                            }
                                        }
                                        mir::ConstValue::Array(elements) => {
                                            if let TyKind::Array(elem_ty, _) = &base_info.ty.kind {
                                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                    if fields.len() == 2 {
                                                        *len = elements.len() as u64;
                                                        *key_ty = (*fields[0].clone()).clone();
                                                        *entry_value_ty =
                                                            (*fields[1].clone()).clone();
                                                        value_ty = (*fields[1].clone()).clone();
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    if matches!(kind, mir::ContainerKind::Map { len: 0, .. }) {
                        self.lowering.emit_error(
                            expr.span,
                            "map indexing requires a literal HashMap for now",
                        );
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            ),
                            ty: value_ty,
                        });
                    }
                    let local_id = self.allocate_temp(value_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::ContainerGet {
                                kind,
                                container: base_info.operand,
                                key: index_operand.operand,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: value_ty,
                    });
                }
                let index_ty = Ty {
                    kind: TyKind::Uint(UintTy::Usize),
                };
                let index_operand = self.lower_operand(index, Some(&index_ty))?;
                let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                let index_place = mir::Place::from_local(index_local);
                self.push_statement(mir::Statement {
                    source_info: index.span,
                    kind: mir::StatementKind::Assign(
                        index_place.clone(),
                        mir::Rvalue::Use(index_operand.operand),
                    ),
                });

                let (mut place, mut base_ty) = match base_info.operand {
                    mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                        (place, base_info.ty.clone())
                    }
                    other => {
                        let local_id = self.allocate_temp(base_info.ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(other),
                            ),
                        });
                        (place, base_info.ty.clone())
                    }
                };

                loop {
                    match &base_ty.kind {
                        TyKind::Ref(_, inner, _) => {
                            place.projection.push(mir::PlaceElem::Deref);
                            base_ty = inner.as_ref().clone();
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            place.projection.push(mir::PlaceElem::Deref);
                            base_ty = type_and_mut.ty.as_ref().clone();
                        }
                        _ => break,
                    }
                }

                let element_ty = match &base_ty.kind {
                    TyKind::Array(elem, _) => *elem.clone(),
                    TyKind::Slice(elem) => *elem.clone(),
                    _ => {
                        self.lowering.emit_error(
                            expr.span,
                            format!(
                                "index access requires array, slice, or supported container; found {:?}",
                                base_ty.kind
                            ),
                        );
                        let ty = expected
                            .cloned()
                            .unwrap_or_else(|| self.lowering.error_ty());
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            ),
                            ty,
                        });
                    }
                };

                place.projection.push(mir::PlaceElem::Index(index_local));
                return Ok(OperandInfo {
                    operand: mir::Operand::copy(place),
                    ty: element_ty,
                });
            }
            hir::ExprKind::IntrinsicCall(call) => {
                // Portable `#[op(...)]` calls with no low-level intrinsic
                // equivalent (`CallKind::Op` variants that don't map via
                // `intrinsic_kind()`) haven't been normalized/materialized
                // away by the time MIR lowering runs -- fail loudly here
                // rather than silently mis-lowering them, matching the
                // "unsupported intrinsic" fallback already used below for
                // intrinsics this function doesn't otherwise handle.
                let Some(kind) = call.kind.intrinsic_kind() else {
                    self.lowering.emit_error(
                        expr.span,
                        format!(
                            "portable op {:?} reached MIR operand lowering, which only handles genuine intrinsics",
                            call.kind
                        ),
                    );
                    let unit_ty = self.lowering.error_ty();
                    let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: unit_ty,
                    });
                };
                if matches!(kind, IntrinsicKind::Print | IntrinsicKind::Println) {
                    self.emit_printf_call(call, expr.span)?;
                    let unit_ty = MirLowering::unit_ty();
                    let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: unit_ty,
                    });
                }
                if kind == IntrinsicKind::Format {
                    let (format, args) = self.prepare_format_call(call, expr.span)?;
                    let string_ty = Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(Ty {
                                kind: TyKind::Int(IntTy::I8),
                            }),
                            mutbl: Mutability::Not,
                        }),
                    };
                    let local_id = self.allocate_temp(string_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicKind::Format,
                                format,
                                args,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: string_ty,
                    });
                }
                if kind == IntrinsicKind::Panic {
                    self.emit_panic_intrinsic(call, expr.span)?;
                    let unit_ty = MirLowering::unit_ty();
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: unit_ty.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Val(mir::ConstValue::Unit),
                        }),
                        ty: unit_ty,
                    });
                }
                if kind == IntrinsicKind::CatchUnwind {
                    return self.lower_catch_unwind(expr, call, None);
                }
                if kind == IntrinsicKind::CatchUnwindResult {
                    return self.lower_catch_unwind_result(expr, call, None);
                }
                if kind == IntrinsicKind::TimeNow {
                    let args = &call.callargs;
                    if !args.is_empty() {
                        self.lowering
                            .emit_error(expr.span, "time::now intrinsic expects no arguments");
                    }
                    let now_ty = Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    };
                    let local_id = self.allocate_temp(now_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicKind::TimeNow,
                                format: String::new(),
                                args: Vec::new(),
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: now_ty,
                    });
                }
                if kind == IntrinsicKind::FsReadToString {
                    let ty = expected.cloned().unwrap_or_else(|| Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    });
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.lower_fs_read_to_string_into_place(expr, call, local_place.clone(), &ty)?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                if kind == IntrinsicKind::FsExists {
                    let ty = Ty { kind: TyKind::Bool };
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.lower_fs_exists_into_place(expr, call, local_place.clone(), &ty)?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                if kind == IntrinsicKind::FsRemoveFile {
                    self.lower_fs_remove_file_as_statement(expr, call)?;
                    let unit_ty = MirLowering::unit_ty();
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: unit_ty.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Val(mir::ConstValue::Unit),
                        }),
                        ty: unit_ty,
                    });
                }
                if kind == IntrinsicKind::EnvVarExists {
                    let ty = Ty { kind: TyKind::Bool };
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.lower_env_var_exists_into_place(expr, call, local_place.clone(), &ty)?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                if kind == IntrinsicKind::EnvVar {
                    let ty = expected.cloned().unwrap_or_else(|| Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    });
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.lower_env_var_into_place(expr, call, local_place.clone(), &ty)?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                if matches!(
                    kind,
                    IntrinsicKind::FsWriteString
                        | IntrinsicKind::FsAppendString
                        | IntrinsicKind::FsIsDir
                        | IntrinsicKind::FsIsFile
                ) {
                    self.lowering.emit_error(
                        expr.span,
                        format!("{:?} is not implemented for compiled backends", kind),
                    );
                    let ty = expected
                        .cloned()
                        .unwrap_or_else(|| self.lowering.error_ty());
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(self.lowering.error_constant(expr.span)),
                        ty,
                    });
                }
                if kind == IntrinsicKind::Slice {
                    let args = &call.callargs;
                    if args.len() != 3 {
                        self.lowering.emit_error(
                            expr.span,
                            "slice intrinsic expects base, start, and end arguments",
                        );
                    }
                    let base = args.get(0).map(|arg| &arg.value);
                    let start = args.get(1).map(|arg| &arg.value);
                    let end = args.get(2).map(|arg| &arg.value);
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let base_operand = base
                        .map(|expr| self.lower_operand(expr, None))
                        .transpose()?;
                    let start_operand = start
                        .map(|expr| self.lower_operand(expr, Some(&index_ty)))
                        .transpose()?;
                    let end_operand = end
                        .map(|expr| self.lower_operand(expr, Some(&index_ty)))
                        .transpose()?;
                    let slice_ty = expected.cloned().unwrap_or_else(|| Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    });
                    let local_id = self.allocate_temp(slice_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let mut args = Vec::new();
                    if let Some(base) = base_operand {
                        args.push(base.operand);
                    }
                    if let Some(start) = start_operand {
                        args.push(start.operand);
                    }
                    if let Some(end) = end_operand {
                        args.push(end.operand);
                    }
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicKind::Slice,
                                format: String::new(),
                                args,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: slice_ty.clone(),
                    });
                }
                if kind == IntrinsicKind::Len {
                    let args = &call.callargs;
                    let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

                    let Some(arg) = arg_values.first() else {
                        self.lowering
                            .emit_error(expr.span, "len intrinsic expects one argument");
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                },
                                user_ty: None,
                                literal: mir::ConstantKind::UInt(0),
                            }),
                            ty: Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            },
                        });
                    };

                    if let Some(local_id) = self.local_id_from_expr(arg) {
                        if let Some(kind) = self.container_locals.get(&local_id).cloned() {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            let local_id_out = self.allocate_temp(len_ty.clone(), expr.span);
                            let local_place = mir::Place::from_local(local_id_out);
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::ContainerLen {
                                        kind,
                                        container: mir::Operand::copy(mir::Place::from_local(
                                            local_id,
                                        )),
                                    },
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(OperandInfo {
                                operand: mir::Operand::copy(local_place),
                                ty: len_ty,
                            });
                        }
                    }

                    let place = if let Some(place_info) = self.lower_place(arg)? {
                        place_info.place
                    } else {
                        let arg_ty = expected.cloned().unwrap_or_else(|| Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        });
                        let local_id = self.allocate_temp(arg_ty.clone(), arg.span);
                        let temp_place = mir::Place::from_local(local_id);
                        self.lower_expr_into_place(arg, temp_place.clone(), &arg_ty)?;
                        temp_place
                    };

                    let len_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let local_id = self.allocate_temp(len_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Len(place),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: len_ty,
                    });
                }
                if matches!(
                    kind,
                    IntrinsicKind::Spawn | IntrinsicKind::Join | IntrinsicKind::Select
                ) {
                    let mut lowered_args = Vec::with_capacity(call.callargs.len());
                    for arg in &call.callargs {
                        lowered_args.push(self.lower_operand(&arg.value, None)?);
                    }

                    match kind {
                        IntrinsicKind::Spawn | IntrinsicKind::Select => {
                            if lowered_args.is_empty() {
                                self.lowering.emit_error(
                                    expr.span,
                                    format!(
                                        "{:?} intrinsic expects at least one argument",
                                        kind
                                    ),
                                );
                                let unit_ty = MirLowering::unit_ty();
                                let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                                let local_place = mir::Place::from_local(local_id);
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        local_place.clone(),
                                        mir::Rvalue::Aggregate(
                                            mir::AggregateKind::Tuple,
                                            Vec::new(),
                                        ),
                                    ),
                                });
                                return Ok(OperandInfo {
                                    operand: mir::Operand::copy(local_place),
                                    ty: unit_ty,
                                });
                            }

                            let mut lowered_args = lowered_args.into_iter();
                            let first = lowered_args
                                .next()
                                .expect("checked non-empty intrinsic args");
                            return Ok(first);
                        }
                        IntrinsicKind::Join => {
                            if lowered_args.is_empty() {
                                self.lowering
                                    .emit_error(expr.span, "join intrinsic expects arguments");
                                let unit_ty = MirLowering::unit_ty();
                                let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                                let local_place = mir::Place::from_local(local_id);
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        local_place.clone(),
                                        mir::Rvalue::Aggregate(
                                            mir::AggregateKind::Tuple,
                                            Vec::new(),
                                        ),
                                    ),
                                });
                                return Ok(OperandInfo {
                                    operand: mir::Operand::copy(local_place),
                                    ty: unit_ty,
                                });
                            }

                            if lowered_args.len() == 1 {
                                return Ok(lowered_args
                                    .into_iter()
                                    .next()
                                    .expect("single intrinsic arg"));
                            }

                            let tuple_tys = lowered_args
                                .iter()
                                .map(|arg| Box::new(arg.ty.clone()))
                                .collect::<Vec<_>>();
                            let tuple_ty = Ty {
                                kind: TyKind::Tuple(tuple_tys),
                            };
                            let local_id = self.allocate_temp(tuple_ty.clone(), expr.span);
                            let local_place = mir::Place::from_local(local_id);
                            let operands = lowered_args
                                .into_iter()
                                .map(|arg| arg.operand)
                                .collect::<Vec<_>>();
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                                ),
                            });
                            return Ok(OperandInfo {
                                operand: mir::Operand::copy(local_place),
                                ty: tuple_ty,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                // Comptime struct-building intrinsics — lowered as
                // mir::Rvalue::IntrinsicCall so MIR→LIR can convert
                // them to ComptimeOp instructions.
                if matches!(
                    kind,
                    IntrinsicKind::CreateStruct
                        | IntrinsicKind::AddField
                        | IntrinsicKind::BuildType
                ) {
                    let lowered_args: Vec<OperandInfo> = call
                        .callargs
                        .iter()
                        .map(|arg| self.lower_operand(&arg.value, None))
                        .collect::<Result<Vec<_>>>()?;
                    let operands: Vec<mir::Operand> =
                        lowered_args.iter().map(|a| a.operand.clone()).collect();
                    let ty = MirLowering::type_ty();
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: kind,
                                format: String::new(),
                                args: operands,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                // `compile_warning!`/`compile_error!` — evaluated for real
                // by the LIR interpreter (`lir::ComptimeOp::CompileWarning`/
                // `CompileError`) when this comptime block's MIR/LIR is
                // actually executed, exactly like the comptime
                // struct-building intrinsics just above. Neither ever
                // folds to a constant (they're diagnostics, not values),
                // so this must run before `lower_intrinsic_constant` below,
                // which would otherwise just report them as unsupported.
                if matches!(
                    kind,
                    IntrinsicKind::CompileWarning | IntrinsicKind::CompileError
                ) {
                    let lowered_args: Vec<OperandInfo> = call
                        .callargs
                        .iter()
                        .map(|arg| self.lower_operand(&arg.value, None))
                        .collect::<Result<Vec<_>>>()?;
                    let operands: Vec<mir::Operand> =
                        lowered_args.iter().map(|a| a.operand.clone()).collect();
                    let ty = MirLowering::unit_ty();
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: kind,
                                format: String::new(),
                                args: operands,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty,
                    });
                }
                if let Some((literal, ty)) = self.lower_intrinsic_constant(call, expr.span) {
                    let operand = mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: ty.clone(),
                        user_ty: None,
                        literal,
                    });
                    return Ok(OperandInfo { operand, ty });
                }

                self.lowering.emit_error(
                    expr.span,
                    format!(
                        "unsupported intrinsic {:?} during MIR operand lowering",
                        kind
                    ),
                );
                let unit_ty = self.lowering.error_ty();
                let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                self.push_statement(mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                });
                Ok(OperandInfo {
                    operand: mir::Operand::copy(local_place),
                    ty: unit_ty,
                })
            }
            hir::ExprKind::ConstBlock(const_block) => {
                // Real evaluation of this block (if any) happens later,
                // once this package's own MIR/LIR exists — register it as
                // a comptime entry now so `evaluate_comptime_lir` can run
                // it for real through the actual interpreter, resolving
                // arbitrary code (method calls, etc.), not a hand-rolled
                // subset-of-Rust evaluator.
                self.lowering.register_const_block_comptime_entry(
                    expr.hir_id,
                    const_block,
                    expr.span,
                );
                // The value was resolved eagerly during type checking (see
                // `HirTypeChecker::check_expr`'s `ConstBlock` arm) and handed
                // here keyed by this expression's own `hir_id` — no
                // synthetic item, no string key.
                if let Some(value) = self.lowering.typeck_const_block_values.get(&expr.hir_id) {
                    if let Some(constant) = self
                        .lowering
                        .const_block_value_to_mir_constant(&value.clone(), expr.span)
                    {
                        let ty = expected
                            .cloned()
                            .or_else(|| self.constant_ty_from_constant(&constant))
                            .unwrap_or_else(|| self.lowering.error_ty());
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(constant),
                            ty,
                        });
                    }
                }
                // No comptime value available (e.g. this HIR was built
                // directly rather than through typeck) — best effort:
                // lower the body as ordinary code.
                self.lower_operand(&const_block.body, expected)
            }
            // Unlike `MethodCall` (which `lower_expr_into_place` handles
            // for real, via its own dedicated `Call` lowering, further
            // down in that function's own `match`), `FieldAccess` gets no
            // such treatment there — its only arm (grouped with
            // `Literal`/`Path`/`Index`/`ConstBlock`) just calls straight
            // back into `lower_operand` for this same `expr`. Reaching
            // this wildcard arm with a `FieldAccess` means the const-fold/
            // `lower_place` attempts above (~15455-15473) — the only two
            // legitimate ways to resolve one — already failed for real;
            // routing through `lower_expr_into_place` here would just
            // re-enter `lower_operand` for a `hir_id` still on the call
            // stack (still in `active_exprs`), tripping the re-entrancy
            // guard's "recursive expression detected" — a false positive
            // that masks the actual failure. Emit that failure directly.
            hir::ExprKind::FieldAccess(_, _) => {
                let message = "unable to lower field access to an operand: neither a \
                     constant value nor a real place could be computed for it";
                self.lowering.emit_error(expr.span, message);
                Err(fp_core::error::Error::from(message))
            }
            _ => {
                // Fallback: evaluate into temporary local
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::Tuple(Vec::new()),
                });
                let local_id = self.allocate_temp(ty.clone(), expr.span);
                self.lower_expr_into_place(expr, mir::Place::from_local(local_id), &ty)?;
                let actual_ty = self.locals[local_id as usize].ty.clone();
                Ok(OperandInfo {
                    operand: mir::Operand::copy(mir::Place::from_local(local_id)),
                    ty: actual_ty,
                })
            }
        }
    }

    fn lower_slice_operand(
        &mut self,
        slice: &hir::SliceExpr,
        span: Span,
        expected: Option<&Ty>,
    ) -> Result<OperandInfo> {
        let base_place = if let Some(place) = self.lower_place(slice.base.as_ref())? {
            place
        } else {
            self.materialize_expr_place(slice.base.as_ref())?
        };
        let base_operand = OperandInfo {
            operand: mir::Operand::copy(base_place.place.clone()),
            ty: base_place.ty.clone(),
        };

        let index_ty = Ty {
            kind: TyKind::Uint(UintTy::Usize),
        };
        let start_operand = match slice.start.as_ref() {
            Some(start) => self.lower_operand(start.as_ref(), Some(&index_ty))?,
            None => OperandInfo::constant(span, index_ty.clone(), mir::ConstantKind::UInt(0)),
        };

        let mut end_operand = match slice.end.as_ref() {
            Some(end) => self.lower_operand(end.as_ref(), Some(&index_ty))?,
            None => {
                let mut len_place = base_place.place.clone();
                let mut len_ty = base_place.ty.clone();
                loop {
                    match &len_ty.kind {
                        TyKind::Ref(_, inner, _) => {
                            len_place.projection.push(mir::PlaceElem::Deref);
                            len_ty = inner.as_ref().clone();
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            len_place.projection.push(mir::PlaceElem::Deref);
                            len_ty = type_and_mut.ty.as_ref().clone();
                        }
                        _ => break,
                    }
                }

                if !matches!(len_ty.kind, TyKind::Array(_, _) | TyKind::Slice(_)) {
                    self.lowering.emit_error(
                        span,
                        "omitted slice end requires an array or slice base type",
                    );
                    OperandInfo::constant(span, index_ty.clone(), mir::ConstantKind::UInt(0))
                } else {
                    // `.len()`'s result type is `usize` (see `IntrinsicKind::Len`'s
                    // typing) and `index_ty` (above) is also `usize` — no
                    // cast needed between them anymore, unlike when `Len`
                    // used to type as `u64`.
                    let len_local = self.allocate_temp(index_ty.clone(), span);
                    let len_local_place = mir::Place::from_local(len_local);
                    self.push_statement(mir::Statement {
                        source_info: span,
                        kind: mir::StatementKind::Assign(
                            len_local_place.clone(),
                            mir::Rvalue::Len(len_place),
                        ),
                    });
                    OperandInfo {
                        operand: mir::Operand::copy(len_local_place),
                        ty: index_ty.clone(),
                    }
                }
            }
        };

        let inclusive = if slice.inclusive && slice.end.is_none() {
            self.lowering.emit_error(
                span,
                "inclusive slice syntax requires an explicit end bound",
            );
            false
        } else {
            slice.inclusive
        };

        if inclusive {
            let one = OperandInfo::constant(span, index_ty.clone(), mir::ConstantKind::UInt(1));
            let temp_local = self.allocate_temp(index_ty.clone(), span);
            let temp_place = mir::Place::from_local(temp_local);
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    temp_place.clone(),
                    mir::Rvalue::BinaryOp(mir::BinOp::Add, end_operand.operand, one.operand),
                ),
            });
            end_operand = OperandInfo {
                operand: mir::Operand::copy(temp_place),
                ty: index_ty.clone(),
            };
        }

        let slice_ty = expected
            .cloned()
            .filter(|ty| matches!(ty.kind, TyKind::Slice(_)))
            .or_else(|| {
                let mut ty = base_place.ty.clone();
                loop {
                    match &ty.kind {
                        TyKind::Ref(_, inner, _) => ty = inner.as_ref().clone(),
                        TyKind::RawPtr(type_and_mut) => ty = type_and_mut.ty.as_ref().clone(),
                        _ => break,
                    }
                }
                match &ty.kind {
                    TyKind::Array(elem, _) => Some(Ty {
                        kind: TyKind::Slice(elem.clone()),
                    }),
                    TyKind::Slice(elem) => Some(Ty {
                        kind: TyKind::Slice(elem.clone()),
                    }),
                    _ => None,
                }
            })
            .unwrap_or_else(|| Ty {
                kind: TyKind::Slice(Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                })),
            });

        let local_id = self.allocate_temp(slice_ty.clone(), span);
        let local_place = mir::Place::from_local(local_id);
        let statement = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                local_place.clone(),
                mir::Rvalue::IntrinsicCall {
                    kind: IntrinsicKind::Slice,
                    format: String::new(),
                    args: vec![
                        base_operand.operand,
                        start_operand.operand,
                        end_operand.operand,
                    ],
                },
            ),
        };
        self.push_statement(statement);
        Ok(OperandInfo {
            operand: mir::Operand::copy(local_place),
            ty: slice_ty,
        })
    }

    fn lower_reference_operand(
        &mut self,
        reference: &hir::ExprReference,
        span: Span,
    ) -> Result<OperandInfo> {
        let place = if let Some(place) = self.lower_place(&reference.expr)? {
            place
        } else {
            self.materialize_expr_place(&reference.expr)?
        };
        let ty_mutability = match reference.mutable {
            hir::ty::Mutability::Mut => mir::ty::Mutability::Mut,
            hir::ty::Mutability::Not => mir::ty::Mutability::Not,
        };
        let ref_ty = Ty {
            kind: TyKind::Ref(
                mir::ty::Region::ReErased,
                Box::new(place.ty.clone()),
                ty_mutability,
            ),
        };
        let borrow_kind = match ty_mutability {
            mir::ty::Mutability::Mut => mir::BorrowKind::Mut {
                allow_two_phase_borrow: false,
            },
            mir::ty::Mutability::Not => mir::BorrowKind::Shared,
        };
        let temp_local = self.allocate_temp(ref_ty.clone(), span);
        let temp_place = mir::Place::from_local(temp_local);
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                temp_place.clone(),
                mir::Rvalue::Ref((), borrow_kind, place.place.clone()),
            ),
        });
        Ok(OperandInfo {
            operand: mir::Operand::copy(temp_place),
            ty: ref_ty,
        })
    }

    fn constant_bool_operand(&self, value: bool, span: Span) -> OperandInfo {
        OperandInfo::constant(
            span,
            Ty { kind: TyKind::Bool },
            mir::ConstantKind::Bool(value),
        )
    }

    fn constant_ty_from_constant(&self, constant: &mir::Constant) -> Option<Ty> {
        Some(constant.ty.clone())
    }

    fn lower_condition_operand(&mut self, expr: &hir::Expr) -> Result<mir::Operand> {
        let bool_ty = Ty { kind: TyKind::Bool };
        let local_id = self.allocate_temp(bool_ty, expr.span);
        let place = mir::Place::from_local(local_id);
        self.lower_expr_into_place(expr, place.clone(), &Ty { kind: TyKind::Bool })?;
        Ok(mir::Operand::copy(place))
    }

    fn allocate_temp(&mut self, ty: Ty, span: Span) -> mir::LocalId {
        let mut decl = self.lowering.make_local_decl(&ty, span);
        decl.mutability = mir::Mutability::Mut;
        self.push_local(decl)
    }

    fn set_current_terminator(&mut self, terminator: mir::Terminator) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.terminator = Some(terminator);
        }
    }

    fn lower_literal(&mut self, lit: &hir::Lit, expected: Option<&Ty>) -> (mir::ConstantKind, Ty) {
        match lit {
            hir::Lit::Bool(value) => (mir::ConstantKind::Bool(*value), Ty { kind: TyKind::Bool }),
            hir::Lit::Integer(value) => {
                if let Some(expected_ty) = expected {
                    match &expected_ty.kind {
                        TyKind::Uint(_) => {
                            (mir::ConstantKind::UInt(*value as u64), expected_ty.clone())
                        }
                        TyKind::Int(_) => (mir::ConstantKind::Int(*value), expected_ty.clone()),
                        _ => (
                            mir::ConstantKind::Int(*value),
                            Ty {
                                kind: TyKind::Int(IntTy::I64),
                            },
                        ),
                    }
                } else {
                    (
                        mir::ConstantKind::Int(*value),
                        Ty {
                            kind: TyKind::Int(IntTy::I64),
                        },
                    )
                }
            }
            hir::Lit::Float(value) => (
                mir::ConstantKind::Float(*value),
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
            ),
            hir::Lit::Str(value) => (
                mir::ConstantKind::Str(value.clone()),
                self.lowering.string_slice_ty(),
            ),
            hir::Lit::Char(value) => (
                mir::ConstantKind::Int(*value as i64),
                Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
            ),
            hir::Lit::Null => {
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                });
                (mir::ConstantKind::Null, ty)
            }
            // `expected` should always be populated in practice (a
            // `b"..."`/`c"..."` literal only ever appears where a
            // `&[u8; N]`/`&CStr`-typed context already exists), matching
            // what HIR-typeck already resolved (`literal_ty` in
            // `fp-typing/src/hir_typeck.rs`) — the fallback here is a
            // best-effort default for the rare case it isn't.
            hir::Lit::Bytes(bytes) => {
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(Ty {
                            kind: TyKind::Array(
                                Box::new(Ty {
                                    kind: TyKind::Uint(UintTy::U8),
                                }),
                                ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                                    data: bytes.len() as u128,
                                    size: 8,
                                }))),
                            ),
                        }),
                        Mutability::Not,
                    ),
                });
                (
                    mir::ConstantKind::Str(String::from_utf8_lossy(bytes).into_owned()),
                    ty,
                )
            }
            hir::Lit::CStr(bytes) => {
                let ty = expected.cloned().unwrap_or_else(|| self.lowering.string_slice_ty());
                (
                    mir::ConstantKind::Str(String::from_utf8_lossy(bytes).into_owned()),
                    ty,
                )
            }
        }
    }

    fn lower_intrinsic_constant(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(mir::ConstantKind, Ty)> {
        let args = &call.callargs;
        if call
            .callargs
            .first()
            .is_some_and(|arg| matches!(arg.value.kind, hir::ExprKind::FormatString(_)))
        {
            self.lowering.emit_warning(
                span,
                "treating formatted intrinsic payload as opaque during MIR lowering",
            );
            return None;
        }
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        // Portable ops with no intrinsic equivalent have no constant-folding
        // rule here either -- same "not handled" outcome as `_ => None` below.
        let Some(kind) = call.kind.intrinsic_kind() else {
            return None;
        };

        match kind {
            IntrinsicKind::SizeOf => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! intrinsic expects one argument");
                        return None;
                    }
                };

                // `sizeof!(T)` where `T` is the enclosing function/method's
                // own generic type parameter (e.g. `impl<T> Vec<T> { fn
                // push(&mut self, value: T) { ... sizeof!(T) ... } }`) — `T`
                // has no struct definition for `resolve_struct_ref` to find,
                // but by the time this specialized body is lowered,
                // `self.type_substs` (the same per-specialization map
                // `payload_types_from_type_substs` reads for enum payloads)
                // already holds the concrete substitution for it. AST→HIR
                // still lowers an unresolved bare identifier like `T` to a
                // usable `hir::Path` (segment name preserved, `res: None`),
                // so check `type_substs` by name before falling through to
                // the struct-only path below.
                if let hir::ExprKind::Path(path) = &target_expr.kind {
                    if let [segment] = path.segments.as_slice() {
                        if let Some(resolved_ty) =
                            self.type_substs.get(segment.name.as_str()).cloned()
                        {
                            let size = match self.compute_ty_size(span, &resolved_ty) {
                                Some(value) => value,
                                None => return None,
                            };
                            return Some((
                                mir::ConstantKind::UInt(size),
                                Ty {
                                    kind: TyKind::Uint(UintTy::U64),
                                },
                            ));
                        }
                    }
                }

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! only supports struct types at the moment");
                        return None;
                    }
                };

                let size = match self.compute_struct_size(span, &struct_ref) {
                    Some(value) => value,
                    None => return None,
                };

                Some((
                    mir::ConstantKind::UInt(size),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicKind::FieldCount => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! only supports struct types");
                        return None;
                    }
                };

                let field_count = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.fields.len() as u64,
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::UInt(field_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicKind::HasField => {
                if args.len() != 2 {
                    self.lowering
                        .emit_error(span, "hasfield! intrinsic expects a type and field name");
                    return None;
                }

                let struct_ref = match self.resolve_struct_ref(arg_values[0]) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "hasfield! only supports struct types");
                        return None;
                    }
                };

                let field_name = match self.expect_string_literal(arg_values[1], span) {
                    Some(name) => name,
                    None => return None,
                };

                let has_field = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.field_index.contains_key(&field_name),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::Bool(has_field),
                    Ty { kind: TyKind::Bool },
                ))
            }
            IntrinsicKind::MethodCount => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! only supports struct types");
                        return None;
                    }
                };

                let struct_name = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.name.clone(),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                let method_count = self
                    .lowering
                    .struct_methods
                    .get(&struct_name)
                    .map(|methods| methods.len() as u64)
                    .unwrap_or(0);

                Some((
                    mir::ConstantKind::UInt(method_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            _ => None,
        }
    }

    fn emit_printf_call(&mut self, call: &hir::IntrinsicCallExpr, span: Span) -> Result<()> {
        let Some((template, positional_slots, named_args, name_map)) =
            self.format_call_parts(call, span)
        else {
            return Ok(());
        };

        let mut prepared_positional = Vec::with_capacity(positional_slots.len());
        for slot in positional_slots {
            if let Some(arg) = slot {
                let lowered = if let Some(formatted) =
                    self.try_format_const_expr_for_printf(&arg.value, span)
                {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
                prepared_positional.push(Some(self.prepare_printf_arg(lowered, span)?));
            } else {
                prepared_positional.push(None);
            }
        }

        let mut prepared_named = Vec::with_capacity(named_args.len());
        for arg in named_args {
            let lowered =
                if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
            prepared_named.push(self.prepare_printf_arg(lowered, span)?);
        }

        let mut format = String::new();
        let mut implicit_index = 0usize;
        let mut ordered_operands = Vec::new();

        for part in &template.parts {
            match part {
                hir::FormatTemplatePart::Literal(text) => format.push_str(text.as_str()),
                hir::FormatTemplatePart::Placeholder(placeholder) => {
                    let (prepared, missing_message) = match &placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => {
                            let current = implicit_index;
                            implicit_index += 1;
                            (
                                prepared_positional.get(current).cloned().flatten(),
                                format!(
                                    "format placeholder references missing argument at index {}",
                                    current
                                ),
                            )
                        }
                        hir::FormatArgRef::Positional(index) => (
                            prepared_positional.get(*index).cloned().flatten(),
                            format!(
                                "format placeholder references missing argument at index {}",
                                index
                            ),
                        ),
                        hir::FormatArgRef::Named(name) => (
                            name_map
                                .get(name)
                                .and_then(|index| prepared_named.get(*index).cloned()),
                            format!("format placeholder references missing argument `{name}`"),
                        ),
                    };

                    let Some((operand, _ty, spec)) = prepared else {
                        self.lowering.emit_error(span, missing_message);
                        return Ok(());
                    };
                    ordered_operands.push(operand);

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
                        }
                    } else {
                        format.push_str(&spec);
                    }
                }
            }
        }

        let printf_kind = call
            .kind
            .intrinsic_kind()
            .filter(|k| matches!(k, IntrinsicKind::Print | IntrinsicKind::Println))
            .unwrap_or(IntrinsicKind::Print);
        if printf_kind == IntrinsicKind::Println {
            format.push('\n');
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::IntrinsicCall {
                kind: printf_kind,
                format,
                args: ordered_operands,
            },
        });
        Ok(())
    }

    fn prepare_format_call(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Result<(String, Vec<mir::Operand>)> {
        let Some((template, positional_slots, named_args, name_map)) =
            self.format_call_parts(call, span)
        else {
            return Ok((String::new(), Vec::new()));
        };

        let mut prepared_positional = Vec::with_capacity(positional_slots.len());
        for slot in positional_slots {
            if let Some(arg) = slot {
                let lowered = if let Some(formatted) =
                    self.try_format_const_expr_for_printf(&arg.value, span)
                {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
                prepared_positional.push(Some(self.prepare_printf_arg(lowered, span)?));
            } else {
                prepared_positional.push(None);
            }
        }

        let mut prepared_named = Vec::with_capacity(named_args.len());
        for arg in named_args {
            let lowered =
                if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                    formatted
                } else {
                    self.lower_operand(&arg.value, None)?
                };
            prepared_named.push(self.prepare_printf_arg(lowered, span)?);
        }

        let mut format = String::new();
        let mut implicit_index = 0usize;
        let mut ordered_operands = Vec::new();

        for part in &template.parts {
            match part {
                hir::FormatTemplatePart::Literal(text) => format.push_str(text.as_str()),
                hir::FormatTemplatePart::Placeholder(placeholder) => {
                    let (prepared, missing_message) = match &placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => {
                            let current = implicit_index;
                            implicit_index += 1;
                            (
                                prepared_positional.get(current).cloned().flatten(),
                                format!(
                                    "format placeholder references missing argument at index {}",
                                    current
                                ),
                            )
                        }
                        hir::FormatArgRef::Positional(index) => (
                            prepared_positional.get(*index).cloned().flatten(),
                            format!(
                                "format placeholder references missing argument at index {}",
                                index
                            ),
                        ),
                        hir::FormatArgRef::Named(name) => (
                            name_map
                                .get(name)
                                .and_then(|index| prepared_named.get(*index).cloned()),
                            format!("format placeholder references missing argument `{name}`"),
                        ),
                    };

                    let Some((operand, _ty, spec)) = prepared else {
                        self.lowering.emit_error(span, missing_message);
                        return Ok((String::new(), Vec::new()));
                    };
                    ordered_operands.push(operand);

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
                        }
                    } else {
                        format.push_str(&spec);
                    }
                }
            }
        }

        Ok((format, ordered_operands))
    }

    fn format_call_parts(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(
        hir::FormatString,
        Vec<Option<hir::CallArg>>,
        Vec<hir::CallArg>,
        HashMap<String, usize>,
    )> {
        let Some(first) = call.callargs.first() else {
            self.lowering
                .emit_error(span, "format intrinsic requires a template argument");
            return None;
        };

        let hir::ExprKind::FormatString(template) = &first.value.kind else {
            self.lowering
                .emit_error(span, "format intrinsic requires a template argument");
            return None;
        };

        let mut positional_slots: Vec<Option<hir::CallArg>> = Vec::new();
        let mut named_args = Vec::new();
        for arg in &call.callargs[1..] {
            let name = arg.name.as_str();
            if let Some(index) = name.strip_prefix("arg") {
                if index.chars().all(|ch| ch.is_ascii_digit()) {
                    let idx = index.parse::<usize>().unwrap_or(0);
                    if idx == 0 {
                        named_args.push(arg.clone());
                        continue;
                    }
                    let idx = idx - 1;
                    if positional_slots.len() <= idx {
                        positional_slots.resize(idx + 1, None);
                    }
                    if positional_slots[idx].is_some() {
                        self.lowering.emit_error(
                            span,
                            format!("format argument index {idx} is provided more than once"),
                        );
                        return None;
                    }
                    positional_slots[idx] = Some(arg.clone());
                    continue;
                }
            }
            named_args.push(arg.clone());
        }

        let mut name_map = HashMap::new();
        for (offset, arg) in named_args.iter().enumerate() {
            let index = offset;
            let name = arg.name.as_str().to_string();
            if name_map.insert(name.clone(), index).is_some() {
                self.lowering.emit_error(
                    span,
                    format!("format argument '{name}' is provided more than once"),
                );
                return None;
            }
        }

        Some((template.clone(), positional_slots, named_args, name_map))
    }

    fn emit_panic_intrinsic(&mut self, call: &hir::IntrinsicCallExpr, span: Span) -> Result<()> {
        let message = if call.callargs.is_empty() {
            "panic! macro triggered".to_string()
        } else if call.callargs.len() == 1 {
            match &call.callargs[0].value.kind {
                hir::ExprKind::Literal(hir::Lit::Str(text)) => text.clone(),
                hir::ExprKind::FormatString(template) => {
                    let has_placeholders = template
                        .parts
                        .iter()
                        .any(|part| matches!(part, hir::FormatTemplatePart::Placeholder(_)));
                    if has_placeholders {
                        let format_call = hir::IntrinsicCallExpr {
                            kind: fp_core::intrinsics::CallKind::Intrinsic(IntrinsicKind::Format),
                            callargs: call.callargs.clone(),
                        };
                        let (format, args) = match self.prepare_format_call(&format_call, span) {
                            Ok(value) => value,
                            Err(_) => (String::new(), Vec::new()),
                        };
                        if format.is_empty() && args.is_empty() {
                            self.lowering.emit_error(
                                span,
                                "panic format payload is not supported in compiled backends",
                            );
                            "<panic message unavailable>".to_string()
                        } else {
                            let string_ty = self.lowering.raw_string_ptr_ty();
                            let local_id = self.allocate_temp(string_ty.clone(), span);
                            let local_place = mir::Place::from_local(local_id);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::IntrinsicCall {
                                        kind: IntrinsicKind::Format,
                                        format,
                                        args,
                                    },
                                ),
                            });
                            self.locals[local_id as usize].ty = string_ty.clone();
                            let sig = mir::FunctionSig {
                                inputs: vec![string_ty.clone()],
                                output: MirLowering::unit_ty(),
                            };
                            self.lowering.ensure_runtime_stub("fp_panic", &sig);
                            let fn_ty = self.lowering.function_pointer_ty(&sig);
                            let func = mir::Operand::Constant(mir::Constant {
                                span,
                                ty: fn_ty.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                    "fp_panic".to_string(),
                                )),
                            });
                            let args = vec![mir::Operand::Copy(local_place)];

                            let result_local = self.allocate_temp(MirLowering::unit_ty(), span);
                            let after_block = self.new_block();
                            let terminator = mir::Terminator {
                                source_info: span,
                                kind: mir::TerminatorKind::Call {
                                    func,
                                    args,
                                    destination: Some((
                                        mir::Place::from_local(result_local),
                                        after_block,
                                    )),
                                    cleanup: self.current_unwind_target,
                                    from_hir_call: true,
                                    fn_span: span,
                                },
                            };
                            self.blocks[self.current_block as usize].terminator = Some(terminator);

                            self.current_block = after_block;
                            self.set_current_terminator(mir::Terminator {
                                source_info: span,
                                kind: mir::TerminatorKind::Unreachable,
                            });
                            self.current_block = self.new_block();
                            return Ok(());
                        }
                    } else {
                        template
                            .parts
                            .iter()
                            .map(|part| match part {
                                hir::FormatTemplatePart::Literal(text) => text.as_str(),
                                hir::FormatTemplatePart::Placeholder(_) => "",
                            })
                            .collect::<Vec<_>>()
                            .join("")
                    }
                }
                _ => {
                    // A non-literal panic argument (e.g. `Option::expect`'s
                    // forwarded `message: &str` parameter — `panic!(message)`
                    // in `crates/fp-lang/src/std/option/mod.fp`) is a
                    // legitimate, valid program: forwarding a caller-supplied
                    // message is normal. `fp_panic`'s runtime call
                    // convention already takes a *runtime* string pointer
                    // (see the `FormatString`-with-placeholders branch
                    // above), not a compile-time constant, so there's no
                    // runtime-side reason to require a literal here either —
                    // lower the argument as a normal operand and call
                    // `fp_panic` with it directly.
                    let string_ty = self.lowering.raw_string_ptr_ty();
                    let mut message_operand =
                        self.lower_operand(&call.callargs[0].value, Some(&string_ty))?;
                    // `expected` above is only a hint — if the argument's
                    // real type is still a `&str`/slice (a fat pointer:
                    // data ptr + length), not yet the bare byte pointer
                    // `fp_panic`'s C-ABI signature requires, extract just
                    // its data-pointer field (mirrors how other C-ABI call
                    // sites in this file convert a slice argument via
                    // `lower_slice_ptr_place`).
                    if message_operand.ty != string_ty {
                        if let mir::Operand::Copy(place) | mir::Operand::Move(place) =
                            &message_operand.operand
                        {
                            let ptr_place = self.lower_slice_ptr_place(place.clone());
                            message_operand = OperandInfo {
                                operand: mir::Operand::Copy(ptr_place),
                                ty: string_ty.clone(),
                            };
                        }
                    }
                    let sig = mir::FunctionSig {
                        inputs: vec![string_ty.clone()],
                        output: MirLowering::unit_ty(),
                    };
                    self.lowering.ensure_runtime_stub("fp_panic", &sig);
                    let fn_ty = self.lowering.function_pointer_ty(&sig);
                    let func = mir::Operand::Constant(mir::Constant {
                        span,
                        ty: fn_ty.clone(),
                        user_ty: None,
                        literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
                    });
                    let args = vec![message_operand.operand];

                    let result_local = self.allocate_temp(MirLowering::unit_ty(), span);
                    let after_block = self.new_block();
                    let terminator = mir::Terminator {
                        source_info: span,
                        kind: mir::TerminatorKind::Call {
                            func,
                            args,
                            destination: Some((mir::Place::from_local(result_local), after_block)),
                            cleanup: self.current_unwind_target,
                            from_hir_call: true,
                            fn_span: span,
                        },
                    };
                    self.blocks[self.current_block as usize].terminator = Some(terminator);

                    self.current_block = after_block;
                    self.set_current_terminator(mir::Terminator {
                        source_info: span,
                        kind: mir::TerminatorKind::Unreachable,
                    });
                    self.current_block = self.new_block();
                    return Ok(());
                }
            }
        } else {
            self.lowering
                .emit_error(span, "panic expects zero or one argument");
            "<panic message unavailable>".to_string()
        };

        let sig = mir::FunctionSig {
            inputs: vec![self.lowering.raw_string_ptr_ty()],
            output: MirLowering::unit_ty(),
        };
        self.lowering.ensure_runtime_stub("fp_panic", &sig);
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let func = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
        });
        let args = vec![mir::Operand::Constant(mir::Constant {
            span,
            ty: self.lowering.raw_string_ptr_ty(),
            user_ty: None,
            literal: mir::ConstantKind::Str(message),
        })];

        let result_local = self.allocate_temp(MirLowering::unit_ty(), span);
        let after_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func,
                args,
                destination: Some((mir::Place::from_local(result_local), after_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = after_block;
        self.set_current_terminator(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        });
        self.current_block = self.new_block();
        Ok(())
    }

    fn lower_panic(&mut self, span: Span, args: &[hir::CallArg]) -> Result<()> {
        let string_ty = self.lowering.raw_string_ptr_ty();
        // Non-literal messages (e.g. a forwarded `&str` parameter) are a
        // legitimate, valid program — see the identical fallback in
        // `emit_panic_intrinsic` for the full reasoning. Lower the
        // argument as a normal operand instead of requiring a literal.
        let message_operand = match args.first() {
            Some(arg) => match &arg.value.kind {
                hir::ExprKind::Literal(hir::Lit::Str(message)) => mir::Operand::Constant(mir::Constant {
                    span,
                    ty: string_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Str(message.clone()),
                }),
                _ => self.lower_operand(&arg.value, Some(&string_ty))?.operand,
            },
            None => mir::Operand::Constant(mir::Constant {
                span,
                ty: string_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::Str("panic".to_string()),
            }),
        };

        let sig = mir::FunctionSig {
            inputs: vec![string_ty.clone()],
            output: MirLowering::unit_ty(),
        };
        self.lowering.ensure_runtime_stub("fp_panic", &sig);
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let func = mir::Operand::Constant(mir::Constant {
            span,
            ty: fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string())),
        });
        let args = vec![message_operand];

        let result_local = self.allocate_temp(MirLowering::unit_ty(), span);
        let after_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func,
                args,
                destination: Some((mir::Place::from_local(result_local), after_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = after_block;
        self.set_current_terminator(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        });
        self.current_block = self.new_block();
        self.control_flow_emitted = true;
        Ok(())
    }

    fn lower_catch_unwind(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        destination: Option<mir::Place>,
    ) -> Result<OperandInfo> {
        let args = &call.callargs;
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind expects exactly one callable argument",
            );
            return Ok(self.constant_bool_operand(false, expr.span));
        }

        let callee = arg_values[0];
        let mut call_args: Vec<mir::Operand> = Vec::new();
        let (func, sig, _name) = if let hir::ExprKind::Struct(path, _) = &callee.kind {
            let struct_name = path.segments.last().map(|seg| seg.name.as_str());
            let closure_suffix = struct_name.and_then(|name| name.strip_prefix("__Closure"));
            if let Some(suffix) = closure_suffix {
                let env = self.lower_operand(callee, None)?;
                let call_name = format!("__closure{}_call", suffix);
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new(call_name),
                        args: None,
                    }],
                    res: None,
                };
                let call_expr = hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Path(path),
                    span: expr.span,
                };
                call_args.push(env.operand);
                self.resolve_callee(&call_expr)?
            } else {
                self.resolve_callee(callee)?
            }
        } else {
            self.resolve_callee(callee)?
        };
        if call_args.is_empty() {
            if !sig.inputs.is_empty() {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind only supports zero-argument callables",
                );
            }
        } else if sig.inputs.len() != call_args.len() {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind closure must not take user arguments",
            );
        }
        if !MirLowering::is_unit_ty(&sig.output) {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind only supports callables that return unit",
            );
        }

        let result_ty = Ty { kind: TyKind::Bool };
        let result_place = destination.unwrap_or_else(|| {
            let local_id = self.allocate_temp(result_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        });
        if (result_place.local as usize) < self.locals.len() {
            self.locals[result_place.local as usize].ty = result_ty.clone();
        }

        let call_result_local = self.allocate_temp(sig.output.clone(), expr.span);
        let call_result_place = mir::Place::from_local(call_result_local);

        let ok_block = self.new_block();
        let unwind_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(unwind_block as usize) {
            block.is_cleanup = true;
        }
        let join_block = self.new_block();

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func,
                args: call_args,
                destination: Some((call_result_place, ok_block)),
                cleanup: Some(unwind_block),
                from_hir_call: true,
                fn_span: expr.span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = ok_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty { kind: TyKind::Bool },
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(true),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = unwind_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: Ty { kind: TyKind::Bool },
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(false),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        Ok(OperandInfo {
            operand: mir::Operand::copy(result_place),
            ty: result_ty,
        })
    }

    fn lower_catch_unwind_result(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        destination: Option<mir::Place>,
    ) -> Result<OperandInfo> {
        let args = &call.callargs;
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind_result expects exactly one callable argument",
            );
            return Ok(self.constant_bool_operand(false, expr.span));
        }

        let callee = arg_values[0];
        let mut call_args: Vec<mir::Operand> = Vec::new();
        let (func, sig, _name) = if let hir::ExprKind::Struct(path, _) = &callee.kind {
            let struct_name = path.segments.last().map(|seg| seg.name.as_str());
            let closure_suffix = struct_name.and_then(|name| name.strip_prefix("__Closure"));
            if let Some(suffix) = closure_suffix {
                let env = self.lower_operand(callee, None)?;
                let call_name = format!("__closure{}_call", suffix);
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new(call_name),
                        args: None,
                    }],
                    res: None,
                };
                let call_expr = hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Path(path),
                    span: expr.span,
                };
                call_args.push(env.operand);
                self.resolve_callee(&call_expr)?
            } else {
                self.resolve_callee(callee)?
            }
        } else {
            self.resolve_callee(callee)?
        };
        match (call_args.is_empty(), sig.inputs.len(), call_args.len()) {
            (true, 0, _) => {}
            (true, _, _) => {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind_result only supports zero-argument callables",
                );
            }
            (false, expected, actual) if expected != actual => {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind_result closure must not take user arguments",
                );
            }
            (false, _, _) => {}
        }

        let result_ty = Ty {
            kind: TyKind::Tuple(vec![
                Box::new(Ty { kind: TyKind::Bool }),
                Box::new(sig.output.clone()),
            ]),
        };
        let result_place = destination.unwrap_or_else(|| {
            let local_id = self.allocate_temp(result_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        });
        if (result_place.local as usize) < self.locals.len() {
            self.locals[result_place.local as usize].ty = result_ty.clone();
        }

        let call_result_local = self.allocate_temp(sig.output.clone(), expr.span);
        let call_result_place = mir::Place::from_local(call_result_local);

        let ok_block = self.new_block();
        let unwind_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(unwind_block as usize) {
            block.is_cleanup = true;
        }
        let join_block = self.new_block();

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func,
                args: call_args,
                destination: Some((call_result_place.clone(), ok_block)),
                cleanup: Some(unwind_block),
                from_hir_call: true,
                fn_span: expr.span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = ok_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Aggregate(
                    mir::AggregateKind::Tuple,
                    vec![
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: Ty { kind: TyKind::Bool },
                            user_ty: None,
                            literal: mir::ConstantKind::Bool(true),
                        }),
                        mir::Operand::Copy(call_result_place),
                    ],
                ),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = unwind_block;
        let unwind_default = self
            .lowering
            .catch_unwind_default_constant_for_ty(&sig.output)?;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Aggregate(
                    mir::AggregateKind::Tuple,
                    vec![
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: sig.output.clone(),
                            user_ty: None,
                            literal: mir::ConstantKind::Bool(false),
                        }),
                        mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            ty: sig.output.clone(),
                            user_ty: None,
                            literal: unwind_default,
                        }),
                    ],
                ),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        Ok(OperandInfo {
            operand: mir::Operand::copy(result_place),
            ty: result_ty,
        })
    }

    fn prepare_printf_arg(
        &mut self,
        arg: OperandInfo,
        span: Span,
    ) -> Result<(mir::Operand, Ty, String)> {
        let (operand, ty) = (arg.operand, arg.ty);
        if let mir::Operand::Constant(constant) = &operand {
            if matches!(constant.literal, mir::ConstantKind::Null) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: self.lowering.raw_string_ptr_ty(),
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &operand {
            if place.projection.is_empty() && self.null_locals.contains(&place.local) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        ty: self.lowering.raw_string_ptr_ty(),
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        match &ty.kind {
            TyKind::Bool => Ok((operand, ty.clone(), "%d".to_string())),
            TyKind::Char => Ok((operand, ty.clone(), "%c".to_string())),
            TyKind::Int(int_ty) => Ok((
                operand,
                ty.clone(),
                match int_ty {
                    IntTy::I8 => "%hhd",
                    IntTy::I16 => "%hd",
                    IntTy::I32 => "%d",
                    IntTy::I64 => "%lld",
                    IntTy::I128 => "%lld",
                    IntTy::Isize => "%lld",
                }
                .to_string(),
            )),
            TyKind::Uint(uint_ty) => Ok((
                operand,
                ty.clone(),
                match uint_ty {
                    UintTy::U8 => "%hhu",
                    UintTy::U16 => "%hu",
                    UintTy::U32 => "%u",
                    UintTy::U64 => "%llu",
                    UintTy::U128 => "%llu",
                    UintTy::Usize => "%llu",
                }
                .to_string(),
            )),
            TyKind::Float(_) => Ok((operand, ty.clone(), "%f".to_string())),
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    Ok((operand, ty.clone(), "%s".to_string()))
                } else {
                    let spec = self.printf_spec_for_ty(&ty, span)?;
                    Ok((operand, ty.clone(), spec))
                }
            }
            TyKind::Slice(elem) => {
                if self.is_c_string_ptr(elem.as_ref()) {
                    let ptr_ty = self.lowering.raw_string_ptr_ty();
                    let ptr_operand = match operand {
                        mir::Operand::Constant(constant)
                            if matches!(constant.literal, mir::ConstantKind::Str(_)) =>
                        {
                            mir::Operand::Constant(mir::Constant {
                                span: constant.span,
                                ty: ptr_ty.clone(),
                                user_ty: constant.user_ty,
                                literal: constant.literal,
                            })
                        }
                        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                            let mut ptr_place = place;
                            ptr_place
                                .projection
                                .push(mir::PlaceElem::Field(0, ptr_ty.clone()));
                            mir::Operand::Copy(ptr_place)
                        }
                        operand => {
                            let local = self.allocate_temp(ty.clone(), span);
                            let place = mir::Place::from_local(local);
                            self.push_statement(mir::Statement {
                                source_info: span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::Use(operand),
                                ),
                            });
                            let mut ptr_place = place;
                            ptr_place
                                .projection
                                .push(mir::PlaceElem::Field(0, ptr_ty.clone()));
                            mir::Operand::Copy(ptr_place)
                        }
                    };
                    Ok((ptr_operand, ptr_ty, "%s".to_string()))
                } else {
                    self.lowering
                        .emit_warning(span, "printf using %p for non-string slice argument");
                    Ok((operand, ty.clone(), "%p".to_string()))
                }
            }
            TyKind::Tuple(elements) if elements.is_empty() => Ok((
                mir::Operand::Constant(mir::Constant {
                    span,
                    ty: self.lowering.raw_string_ptr_ty(),
                    user_ty: None,
                    literal: mir::ConstantKind::Str("()".to_string()),
                }),
                self.lowering.raw_string_ptr_ty(),
                "%s".to_string(),
            )),
            TyKind::Tuple(_) | TyKind::Array(_, _) | TyKind::Adt(_, _) => {
                if let Some((string_operand, string_ty)) =
                    self.format_const_operand_for_printf(&operand, span)
                {
                    return Ok((string_operand, string_ty, "%s".to_string()));
                }
                self.lowering.emit_warning(
                    span,
                    "printf lowering tuple/array/struct argument as opaque pointer",
                );
                Ok((operand, ty.clone(), "%p".to_string()))
            }
            TyKind::Ref(_, inner, _) => {
                if let TyKind::RawPtr(type_and_mut) = &inner.kind {
                    if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if let TyKind::Slice(elem) = &inner.kind {
                    if self.is_c_string_ptr(elem.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if self.is_c_string_ptr(inner.as_ref()) {
                    return Ok((operand, ty.clone(), "%s".to_string()));
                }
                let spec = self.printf_spec_for_ty(&ty, span)?;
                Ok((operand, ty.clone(), spec))
            }
            _ => {
                if let Some((string_operand, string_ty)) =
                    self.format_const_operand_for_printf(&operand, span)
                {
                    return Ok((string_operand, string_ty, "%s".to_string()));
                }
                if self.lowering.is_opaque_ty(&ty) {
                    return Ok((operand, ty.clone(), "%p".to_string()));
                }
                let ty_name = self
                    .lowering
                    .display_type_name(&ty)
                    .unwrap_or_else(|| format!("{:?}", ty.kind));
                self.lowering.emit_warning(
                    span,
                    format!(
                        "printf argument type is not supported: {}; using %p",
                        ty_name
                    ),
                );
                Ok((operand, ty.clone(), "%p".to_string()))
            }
        }
    }

    fn format_const_operand_for_printf(
        &mut self,
        operand: &mir::Operand,
        span: Span,
    ) -> Option<(mir::Operand, Ty)> {
        let mir::Operand::Constant(constant) = operand else {
            return None;
        };
        let mir::ConstantKind::Val(value) = &constant.literal else {
            return None;
        };
        let ast_value = self.const_value_to_ast_value(value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        let constant = mir::Constant {
            span,
            ty: ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Str(formatted),
        };
        Some((mir::Operand::Constant(constant), ty))
    }

    fn try_format_const_expr_for_printf(
        &mut self,
        expr: &hir::Expr,
        span: Span,
    ) -> Option<OperandInfo> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        let Some(hir::Res::Def(def_id)) = &path.res else {
            return None;
        };
        let const_info = self.lowering.const_values.get(def_id)?;
        let mir::ConstantKind::Val(value) = &const_info.value.literal else {
            return None;
        };
        let value = value.clone();
        if !matches!(
            value,
            mir::ConstValue::Array(_)
                | mir::ConstValue::List { .. }
                | mir::ConstValue::Map { .. }
                | mir::ConstValue::Tuple(_)
                | mir::ConstValue::Struct(_)
        ) {
            return None;
        }
        let ast_value = self.const_value_to_ast_value(&value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        Some(OperandInfo::constant(
            span,
            ty,
            mir::ConstantKind::Str(formatted),
        ))
    }

    fn const_value_to_ast_value(&mut self, value: &mir::ConstValue) -> Option<Value> {
        match value {
            mir::ConstValue::Unit => Some(Value::unit()),
            mir::ConstValue::Bool(value) => Some(Value::bool(*value)),
            mir::ConstValue::Int(value) => Some(Value::int(*value)),
            mir::ConstValue::UInt(value) => Some(Value::int(*value as i64)),
            mir::ConstValue::Float(value) => Some(Value::decimal(*value)),
            mir::ConstValue::Str(value) => Some(Value::string(value.clone())),
            mir::ConstValue::Null => Some(Value::null()),
            mir::ConstValue::Fn(_) => None,
            mir::ConstValue::Tuple(values) | mir::ConstValue::Struct(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::Tuple(ValueTuple::new(elements)))
            }
            mir::ConstValue::Array(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(elements)))
            }
            mir::ConstValue::List { elements, .. } => {
                let mut items = Vec::with_capacity(elements.len());
                for element in elements {
                    items.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(items)))
            }
            mir::ConstValue::Map { entries, .. } => {
                let mut items = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let key_value = self.const_value_to_ast_value(key)?;
                    let value_value = self.const_value_to_ast_value(value)?;
                    items.push((key_value, value_value));
                }
                // `entries` is already a valid runtime map's contents (from
                // `mir::ConstValue::Map`), so keys are already guaranteed
                // unique — skip `from_pairs`'s per-key duplicate scan.
                Some(Value::Map(ValueMap::from_unique_pairs(items)))
            }
        }
    }

    fn printf_spec_for_ty(&mut self, ty: &Ty, span: Span) -> Result<String> {
        let spec = match &ty.kind {
            TyKind::Bool => "%d",
            TyKind::Char => "%c",
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => "%hhd",
                IntTy::I16 => "%hd",
                IntTy::I32 => "%d",
                IntTy::I64 => "%lld",
                IntTy::I128 => "%lld",
                IntTy::Isize => "%lld",
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => "%hhu",
                UintTy::U16 => "%hu",
                UintTy::U32 => "%u",
                UintTy::U64 => "%llu",
                UintTy::U128 => "%llu",
                UintTy::Usize => "%llu",
            },
            TyKind::Float(_) => "%f",
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    "%s"
                } else {
                    self.lowering
                        .emit_warning(span, "printf using %p for non-string raw pointer argument");
                    "%p"
                }
            }
            TyKind::Ref(_, _, _) => {
                self.lowering
                    .emit_warning(span, "printf using %p for non-string reference argument");
                "%p"
            }
            _ => {
                if self.lowering.is_opaque_ty(ty) {
                    "%p"
                } else {
                    self.lowering
                        .emit_warning(span, "printf argument type is not supported; using %p");
                    "%p"
                }
            }
        };
        Ok(spec.to_string())
    }

    fn is_c_string_ptr(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Int(IntTy::I8) | TyKind::Uint(UintTy::U8))
    }

    fn resolve_struct_ref(&mut self, expr: &hir::Expr) -> Option<StructRef> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };

        let args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
            .unwrap_or_default();

        if let Some(hir::Res::Def(def_id)) = &path.res {
            return Some(StructRef {
                def_id: *def_id,
                args,
            });
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            let mut matches = self
                .lowering
                .struct_defs
                .iter()
                .filter_map(|(def_id, info)| (info.name == name).then_some(*def_id))
                .collect::<Vec<_>>();
            if matches.len() == 1 {
                return Some(StructRef {
                    def_id: matches.pop()?,
                    args,
                });
            }
        }

        None
    }

    fn compute_struct_size(&mut self, span: Span, struct_ref: &StructRef) -> Option<u64> {
        let layout = match self.lowering.struct_layout_for_instance(
            struct_ref.def_id,
            &struct_ref.args,
            span,
        ) {
            Some(layout) => layout,
            None => return None,
        };

        let mut total = 0u64;
        for field_ty in layout.field_tys {
            let field_size = match self.compute_ty_size(span, &field_ty) {
                Some(size) => size,
                None => return None,
            };
            total = total.saturating_add(field_size);
        }
        Some(total)
    }

    fn compute_ty_size(&mut self, span: Span, ty: &Ty) -> Option<u64> {
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
                    let size = match self.compute_ty_size(span, elem) {
                        Some(value) => value,
                        None => return None,
                    };
                    total = total.saturating_add(size);
                }
                Some(total)
            }
            TyKind::Array(elem_ty, len) => {
                let len = match self.const_kind_to_u64(span, len) {
                    Some(value) => value,
                    None => return None,
                };
                let elem_size = match self.compute_ty_size(span, elem_ty) {
                    Some(value) => value,
                    None => return None,
                };
                Some(elem_size.saturating_mul(len))
            }
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) | TyKind::FnPtr(_) | TyKind::FnDef(_, _) => {
                Some(8)
            }
            TyKind::Never => Some(0),
            TyKind::Error(_) => None,
            TyKind::Slice(_) => {
                // Slices are fat pointers (data + length).
                Some(16)
            }
            TyKind::Adt(_, _)
            | TyKind::Dynamic(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Opaque(_, _)
            | TyKind::Param(_)
            | TyKind::Placeholder(_)
            | TyKind::Bound(_, _)
            | TyKind::Infer(_)
            | TyKind::Type
            | TyKind::Any => {
                if let TyKind::Adt(adt, substs) = &ty.kind {
                    // A payload slot opaqued out by `enum_layout_for_
                    // instance` (heterogeneous per-variant types sharing a
                    // slot) has no fields to size structurally — its size
                    // was already computed there as the max over every
                    // contributing variant's own type at that slot.
                    if let Some(size) = self
                        .lowering
                        .display_type_name(ty)
                        .and_then(|name| self.lowering.opaque_ty_sizes.get(&name).copied())
                    {
                        return Some(size);
                    }
                    let args: Vec<Ty> = substs
                        .iter()
                        .filter_map(|arg| match arg {
                            mir::ty::GenericArg::Type(inner) => Some(inner.clone()),
                            _ => None,
                        })
                        .collect();
                    // `struct_layout_for_ty` is a cache-only reverse lookup
                    // (`&self`, can't trigger computation) — if nothing has
                    // needed this struct's layout yet (e.g. `sizeof!(T)` is
                    // the *first* thing to ask for `String`'s size while
                    // specializing `Vec<String>::push`), it simply misses.
                    // Fall back to `struct_layout_for_instance`, which
                    // computes and caches the layout on demand from the
                    // struct's own `DefId` + concrete generic args, exactly
                    // as a struct-literal use of this same type would.
                    if self.lowering.struct_defs.contains_key(&adt.did) {
                        let layout = self
                            .lowering
                            .struct_layout_for_ty(ty)
                            .or_else(|| self.lowering.struct_layout_for_instance(adt.did, &args, span));
                        if let Some(layout) = layout {
                            let mut total = 0u64;
                            for field in &layout.field_tys {
                                let size = match self.compute_ty_size(span, field) {
                                    Some(value) => value,
                                    None => return None,
                                };
                                total = total.saturating_add(size);
                            }
                            return Some(total);
                        }
                    }
                    // Enums are nominal (`TyKind::Adt`) now too, but their
                    // actual byte layout is still the flattened
                    // `tag + payload...` shape computed by
                    // `enum_layout_for_instance` — mirror that shape's own
                    // size (tag plus every payload slot) rather than trying
                    // `struct_layout_for_instance` against an enum `DefId`.
                    if self.lowering.enum_defs.contains_key(&adt.did) {
                        if let Some(layout) =
                            self.lowering.enum_layout_for_instance(adt.did, &args, span)
                        {
                            let mut total = self.compute_ty_size(span, &layout.tag_ty)?;
                            for payload in &layout.payload_tys {
                                let size = self.compute_ty_size(span, payload)?;
                                total = total.saturating_add(size);
                            }
                            return Some(total);
                        }
                    }
                }
                // `sizeof!(T)` called on a function/method's own generic type
                // parameter (e.g. inside `impl<T> Vec<T> { fn push(&mut self,
                // value: T) { ... sizeof!(T) ... } }`) — `T` isn't a concrete
                // type in general, but `self.type_substs` (populated per
                // specialization by the same mechanism
                // `payload_types_from_type_substs` already reads for enum
                // payloads) holds the concrete substitution for *this*
                // specialization. Resolve and recurse before giving up.
                if let TyKind::Param(param) = &ty.kind {
                    if let Some(resolved) = self.type_substs.get(param.name.as_str()).cloned() {
                        // Guard against a self-referential/unresolved
                        // substitution (`type_substs["T"]` itself being
                        // `Param("T")`, e.g. when specialization couldn't
                        // infer a concrete type and left an identity
                        // placeholder) — recursing on that would loop
                        // forever instead of erroring.
                        let made_progress = !matches!(
                            &resolved.kind,
                            TyKind::Param(resolved_param) if resolved_param.name == param.name
                        );
                        if made_progress {
                            return self.compute_ty_size(span, &resolved);
                        }
                    }
                }
                self.lowering.emit_error(
                    span,
                    format!("size_of for type `{:?}` is not supported", ty.kind),
                );
                None
            }
        }
    }

    fn const_kind_to_u64(&mut self, span: Span, konst: &ConstKind) -> Option<u64> {
        match konst {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => Some(int.data as u64),
            ConstKind::Value(ConstValue::Scalar(Scalar::Ptr(_))) => {
                self.lowering.emit_warning(
                    span,
                    "array length uses a pointer value; treating length as zero",
                );
                Some(0)
            }
            ConstKind::Value(ConstValue::ZeroSized) => Some(0),
            _ => {
                self.lowering
                    .emit_error(span, "array length is not a compile-time integer constant");
                None
            }
        }
    }

    fn expect_string_literal(&mut self, expr: &hir::Expr, span: Span) -> Option<String> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Str(value)) => Some(value.clone()),
            _ => {
                self.lowering
                    .emit_error(span, "intrinsic argument must be a string literal");
                None
            }
        }
    }

    fn lower_place_path_base(
        &mut self,
        _expr: &hir::Expr,
        path: &hir::Path,
    ) -> Result<Option<PlaceInfo>> {
        let fallback_local = path
            .segments
            .first()
            .filter(|_| path.segments.len() == 1)
            .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied());
        match &path.res {
            Some(hir::Res::Local(hir_id)) => {
                if let Some(local_id) = self.local_map.get(hir_id) {
                    let local_id = *local_id;
                    let ty = self.locals[local_id as usize].ty.clone();
                    let mut struct_def = self.local_structs.get(&local_id).copied();
                    if struct_def.is_none() {
                        if let Some(derived) = self.struct_def_from_ty(&ty) {
                            self.local_structs.insert(local_id, derived);
                            struct_def = Some(derived);
                        }
                    }
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
                if let Some(local_id) = fallback_local {
                    let ty = self.locals[local_id as usize].ty.clone();
                    let struct_def = self.struct_def_from_ty(&ty);
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
            }
            // Constants are operands, never assignable places. Let
            // `lower_operand` resolve them to their constant or global form.
            Some(hir::Res::Def(_)) => {}
            _ => {
                if let Some(local_id) = fallback_local {
                    let ty = self.locals[local_id as usize].ty.clone();
                    let struct_def = self.struct_def_from_ty(&ty);
                    return Ok(Some(PlaceInfo {
                        place: mir::Place::from_local(local_id),
                        ty,
                        struct_def,
                    }));
                }
            }
        }
        Ok(None)
    }

    fn lower_place_expr_base(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        match &expr.kind {
            hir::ExprKind::Unary(hir::UnOp::Deref, inner) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    self.lowering
                        .emit_error(expr.span, "dereference target is not a place expression");
                    return Ok(None);
                };
                let mut base_ty = place_info.ty.clone();
                loop {
                    match &base_ty.kind {
                        TyKind::Ref(_, inner_ty, _) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = inner_ty.as_ref().clone();
                            break;
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = type_and_mut.ty.as_ref().clone();
                            break;
                        }
                        _ if self.boxed_inner_ty(&base_ty).is_some() => {
                            base_ty = self
                                .boxed_inner_ty(&base_ty)
                                .expect("checked boxed inner type above");
                            break;
                        }
                        _ => break,
                    }
                }
                place_info.ty = base_ty;
                place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                Ok(Some(place_info))
            }
            hir::ExprKind::Cast(inner, ty) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    return Ok(None);
                };
                let cast_ty = self.lower_type_expr(ty);
                let place_ok = match (&place_info.ty.kind, &cast_ty.kind) {
                    (TyKind::Ref(_, _, _), TyKind::Ref(_, _, _)) => true,
                    (TyKind::RawPtr(_), TyKind::RawPtr(_)) => true,
                    (TyKind::FnDef(_, _), TyKind::FnPtr(_)) => true,
                    (TyKind::FnPtr(_), TyKind::FnPtr(_)) => true,
                    _ => false,
                };
                if !place_ok {
                    return Ok(None);
                }
                place_info.ty = cast_ty.clone();
                place_info.struct_def = self.struct_def_from_ty(&cast_ty);
                Ok(Some(place_info))
            }
            _ => Ok(None),
        }
    }

    fn lower_place_from_projected(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        let Some(projected) = project_hir_assign_target(expr) else {
            return Ok(None);
        };

        let mut place_info = match projected.base {
            HirAssignTargetBase::Name(path) => {
                let Some(place) = self.lower_place_path_base(expr, &path)? else {
                    return Ok(None);
                };
                place
            }
            HirAssignTargetBase::Expr(base_expr) => {
                let Some(place) = self.lower_place_expr_base(base_expr.as_ref())? else {
                    return Ok(None);
                };
                place
            }
        };

        for projection in projected.projections {
            match projection {
                HirAssignTargetProjection::Deref => {
                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner_ty, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner_ty.as_ref().clone();
                                break;
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                                break;
                            }
                            _ if self.boxed_inner_ty(&base_ty).is_some() => {
                                base_ty = self
                                    .boxed_inner_ty(&base_ty)
                                    .expect("checked boxed inner type above");
                                break;
                            }
                            _ => return Ok(None),
                        }
                    }
                    place_info.ty = base_ty;
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Field(field) => {
                    let mut base_ty = place_info.ty.clone();
                    let mut struct_def = place_info.struct_def;
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    if struct_def.is_none() {
                        struct_def = self.struct_def_from_ty(&base_ty);
                    }
                    let struct_def = match struct_def {
                        Some(def_id) => def_id,
                        None => {
                            self.lowering
                                .emit_error(expr.span, "field access on non-struct value");
                            return Ok(None);
                        }
                    };
                    let (field_index, field_info) = match self.lowering.struct_field(
                        struct_def,
                        &base_ty,
                        field.as_str(),
                        expr.span,
                    ) {
                        Some(data) => data,
                        None => {
                            self.lowering
                                .emit_error(expr.span, format!("unknown field `{}`", field));
                            return Ok(None);
                        }
                    };
                    place_info
                        .place
                        .projection
                        .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
                    place_info.ty = field_info.ty.clone();
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Index(index) => {
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let index_operand = self.lower_operand(index.as_ref(), Some(&index_ty))?;
                    let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                    let index_place = mir::Place::from_local(index_local);
                    let assign = mir::Statement {
                        source_info: index.span,
                        kind: mir::StatementKind::Assign(
                            index_place.clone(),
                            mir::Rvalue::Use(index_operand.operand),
                        ),
                    };
                    self.push_statement(assign);

                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    if self.real_indexable_struct_def_id(&base_ty).is_some()
                        || self.is_list_container(&base_ty)
                        || self.is_map_container(&base_ty)
                    {
                        return Ok(None);
                    }
                    let element_ty = match &base_ty.kind {
                        TyKind::Array(elem, _) => *elem.clone(),
                        TyKind::Slice(elem) => *elem.clone(),
                        _ => {
                            self.lowering
                                .emit_error(expr.span, "index access requires array or slice type");
                            return Ok(None);
                        }
                    };
                    place_info
                        .place
                        .projection
                        .push(mir::PlaceElem::Index(index_local));
                    place_info.ty = element_ty;
                    place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                }
                HirAssignTargetProjection::Slice(slice) => {
                    let mut base_ty = place_info.ty.clone();
                    loop {
                        match &base_ty.kind {
                            TyKind::Ref(_, inner, _) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = inner.as_ref().clone();
                            }
                            TyKind::RawPtr(type_and_mut) => {
                                place_info.place.projection.push(mir::PlaceElem::Deref);
                                base_ty = type_and_mut.ty.as_ref().clone();
                            }
                            _ => break,
                        }
                    }
                    let element_ty = match &base_ty.kind {
                        TyKind::Array(elem, _) => *elem.clone(),
                        TyKind::Slice(elem) => *elem.clone(),
                        _ => {
                            self.lowering
                                .emit_error(expr.span, "slice access requires array or slice type");
                            return Ok(None);
                        }
                    };
                    let Some(from) = slice
                        .start
                        .as_ref()
                        .map_or(Some(0), |start| self.evaluate_array_length(start.as_ref()))
                    else {
                        return Ok(None);
                    };
                    let Some(mut to) = (match slice.end.as_ref() {
                        Some(end) => self.evaluate_array_length(end.as_ref()),
                        None => match &base_ty.kind {
                            TyKind::Array(_, len) => self.const_kind_to_u64(expr.span, len),
                            _ => None,
                        },
                    }) else {
                        return Ok(None);
                    };
                    if slice.inclusive {
                        to = to.saturating_add(1);
                    }
                    if to < from {
                        self.lowering
                            .emit_error(expr.span, "slice end is before slice start");
                        return Ok(None);
                    }
                    place_info.place.projection.push(mir::PlaceElem::Subslice {
                        from,
                        to,
                        from_end: false,
                    });
                    place_info.ty = Ty {
                        kind: TyKind::Slice(Box::new(element_ty)),
                    };
                    place_info.struct_def = None;
                }
            }
        }

        Ok(Some(place_info))
    }

    fn lower_place(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        self.lower_place_from_projected(expr)
    }

    fn materialize_expr_place(&mut self, expr: &hir::Expr) -> Result<PlaceInfo> {
        let value = self.lower_operand(expr, None)?;
        let local_id = self.allocate_temp(value.ty.clone(), expr.span);
        let place = mir::Place::from_local(local_id);
        let container_kind = match &value.operand {
            mir::Operand::Constant(constant) => match &constant.literal {
                mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }) => {
                    Some(mir::ContainerKind::List {
                        elem_ty: elem_ty.clone(),
                        len: elements.len() as u64,
                    })
                }
                mir::ConstantKind::Val(mir::ConstValue::Map {
                    entries,
                    key_ty,
                    value_ty,
                }) => Some(mir::ContainerKind::Map {
                    key_ty: key_ty.clone(),
                    value_ty: value_ty.clone(),
                    len: entries.len() as u64,
                }),
                _ => None,
            },
            _ => None,
        };
        let statement = mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(place.clone(), mir::Rvalue::Use(value.operand)),
        };
        self.push_statement(statement);
        self.locals[local_id as usize].ty = value.ty.clone();
        let struct_def = self.struct_def_from_ty(&value.ty);
        if let Some(def_id) = struct_def {
            self.local_structs.insert(local_id, def_id);
        }
        if let Some(kind) = container_kind {
            self.container_locals.insert(local_id, kind);
        }
        Ok(PlaceInfo {
            place,
            ty: value.ty.clone(),
            struct_def,
        })
    }

    fn lower_expr_into_place(
        &mut self,
        expr: &hir::Expr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Let(pat, ty, init) => {
                self.lower_let_expr(pat, ty, init, expr.span)?;
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place,
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::Query(query) => {
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Query(mir::Query {
                            origin: query.origin.clone(),
                            ir: query.ir.clone(),
                            span: query.span,
                        }),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = expected_ty.clone();
                }
            }
            hir::ExprKind::Literal(_)
            | hir::ExprKind::Path(_)
            | hir::ExprKind::Index(_, _)
            | hir::ExprKind::FieldAccess(_, _)
            | hir::ExprKind::ConstBlock(_) => {
                let assignment_place = place.clone();
                let value = self.lower_operand(expr, Some(expected_ty))?;
                let container_kind = match &value.operand {
                    mir::Operand::Constant(constant) => match &constant.literal {
                        mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }) => {
                            Some(mir::ContainerKind::List {
                                elem_ty: elem_ty.clone(),
                                len: elements.len() as u64,
                            })
                        }
                        mir::ConstantKind::Val(mir::ConstValue::Map {
                            entries,
                            key_ty,
                            value_ty,
                        }) => Some(mir::ContainerKind::Map {
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                            len: entries.len() as u64,
                        }),
                        _ => None,
                    },
                    _ => None,
                };
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        assignment_place.clone(),
                        mir::Rvalue::Use(value.operand),
                    ),
                };
                self.push_statement(statement);
                if assignment_place.projection.is_empty() {
                    // Prefer the destination's already-known `expected_ty`
                    // (the declared/annotated type this value is being
                    // assigned into) over `value.ty` (the operand's own,
                    // independently-derived type) — a comptime-frozen
                    // constant can lose its ADT identity on the way to a
                    // `mir::Constant` (a struct value degrading to a bare
                    // field tuple, since `mir::ConstValue`/`LirType` are
                    // purely structural and don't carry it through), and
                    // `value.ty` would then wrongly clobber a local whose
                    // real, declared type (`Vec<BenchCase>`, say) is
                    // already known and correct.
                    self.locals[assignment_place.local as usize].ty = expected_ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(expected_ty) {
                        self.local_structs
                            .insert(assignment_place.local, struct_def);
                    }
                    if let Some(kind) = container_kind {
                        self.container_locals.insert(assignment_place.local, kind);
                    }
                }
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = self.lower_type_expr(ty_expr);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Cast(mir::CastKind::Misc, operand.operand, target_ty.clone()),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = target_ty;
                }
            }
            hir::ExprKind::Loop(block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::While(cond, block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_while_expr(expr.span, cond, block, Some(destination))?;
            }
            hir::ExprKind::Try(expr_try) => {
                self.lower_try_expr(
                    expr,
                    expr_try,
                    Some((place.clone(), expected_ty.clone())),
                    false,
                )?;
            }
            hir::ExprKind::Break(value) => {
                self.lower_break(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Return(value) => {
                self.lower_return(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Continue => {
                self.lower_continue(expr.span)?;
            }
            hir::ExprKind::Struct(path, fields) => {
                let local_id = place.local;
                self.lower_struct_literal(
                    local_id,
                    Some(expected_ty),
                    expr.hir_id,
                    path,
                    fields,
                    expr.span,
                )?;
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let left = self.lower_operand(lhs, None)?;
                let right = self.lower_operand(rhs, None)?;

                if MirLowering::is_unit_ty(&left.ty) || MirLowering::is_unit_ty(&right.ty) {
                    return Err(fp_core::error::Error::from(format!(
                        "binary operation `{op:?}` received unit operand(s): lhs=`{}`, rhs=`{}`",
                        left.ty, right.ty
                    )));
                }

                let mir_op = Self::convert_bin_op(op);
                let result_ty = Self::binary_result_ty(op, &left.ty);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::BinaryOp(mir_op, left.operand, right.operand),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = result_ty;
                }
            }
            hir::ExprKind::Unary(op, operand_expr) => match op {
                hir::UnOp::Neg | hir::UnOp::Not => {
                    let operand = self.lower_operand(operand_expr, None)?;
                    let mir_op = match Self::convert_un_op(op) {
                        Some(op) => op,
                        None => unreachable!("Neg/Not must convert to MIR op"),
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::UnaryOp(mir_op, operand.operand),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = operand.ty.clone();
                    }
                }
                hir::UnOp::Deref => {
                    let place_info = match self.lower_place(expr)? {
                        Some(info) => info,
                        None => {
                            self.lowering.emit_error(
                                expr.span,
                                "dereference expressions must resolve to a place",
                            );
                            return Ok(());
                        }
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Copy(place_info.place.clone())),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                    }
                }
                hir::UnOp::Box => {
                    let operand = self.lower_operand(operand_expr, None)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(operand.operand),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                    }
                }
            },
            hir::ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    self.lower_stmt(stmt)?;
                }

                if let Some(expr) = &block.expr {
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                } else {
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let bool_ty = Ty { kind: TyKind::Bool };
                let cond_operand = self.lower_condition_operand(cond)?;

                let then_block = self.new_block();
                let else_block = self.new_block();
                let continue_block = self.new_block();

                let switch = mir::Terminator {
                    source_info: cond.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand,
                        switch_ty: bool_ty,
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![then_block],
                            otherwise: else_block,
                        },
                    },
                };
                self.set_current_terminator(switch);

                // Then branch
                self.current_block = then_block;
                self.control_flow_emitted = false;
                self.lower_expr_into_place(then_expr, place.clone(), expected_ty)?;
                if !self.control_flow_emitted
                    && self.blocks[self.current_block as usize]
                        .terminator
                        .is_none()
                {
                    let then_goto = mir::Terminator {
                        source_info: then_expr.span,
                        kind: mir::TerminatorKind::Goto {
                            target: continue_block,
                        },
                    };
                    self.set_current_terminator(then_goto);
                }

                // Else branch (if present)
                self.current_block = else_block;
                if let Some(else_expr) = else_expr {
                    self.control_flow_emitted = false;
                    self.lower_expr_into_place(else_expr, place, expected_ty)?;
                    if !self.control_flow_emitted
                        && self.blocks[self.current_block as usize]
                            .terminator
                            .is_none()
                    {
                        let else_goto = mir::Terminator {
                            source_info: else_expr.span,
                            kind: mir::TerminatorKind::Goto {
                                target: continue_block,
                            },
                        };
                        self.set_current_terminator(else_goto);
                    }
                } else {
                    self.control_flow_emitted = false;
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                    if self.blocks[self.current_block as usize]
                        .terminator
                        .is_none()
                    {
                        let else_goto = mir::Terminator {
                            source_info: expr.span,
                            kind: mir::TerminatorKind::Goto {
                                target: continue_block,
                            },
                        };
                        self.set_current_terminator(else_goto);
                    }
                }

                self.current_block = continue_block;
                self.control_flow_emitted = false;
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.lower_match_expr(expr.span, scrutinee, arms, place, expected_ty)?;
            }
            hir::ExprKind::IntrinsicCall(call) => match call.kind.intrinsic_kind() {
                Some(kind) => match kind {
                IntrinsicKind::Print | IntrinsicKind::Println => {
                    self.emit_printf_call(call, expr.span)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = MirLowering::unit_ty();
                    }
                    return Ok(());
                }
                IntrinsicKind::Format => {
                    let (format, args) = self.prepare_format_call(call, expr.span)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicKind::Format,
                                format,
                                args,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicKind::Panic => {
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                    self.emit_panic_intrinsic(call, expr.span)?;
                    return Ok(());
                }
                IntrinsicKind::CatchUnwind => {
                    self.lower_catch_unwind(expr, call, Some(place.clone()))?;
                    return Ok(());
                }
                IntrinsicKind::CatchUnwindResult => {
                    self.lower_catch_unwind_result(expr, call, Some(place.clone()))?;
                    return Ok(());
                }
                IntrinsicKind::TimeNow => {
                    let args = &call.callargs;
                    if !args.is_empty() {
                        self.lowering
                            .emit_error(expr.span, "time::now intrinsic expects no arguments");
                    }
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicKind::TimeNow,
                                format: String::new(),
                                args: Vec::new(),
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicKind::FsReadToString => {
                    self.lower_fs_read_to_string_into_place(
                        expr,
                        call,
                        place.clone(),
                        expected_ty,
                    )?;
                    return Ok(());
                }
                IntrinsicKind::FsWriteString
                | IntrinsicKind::FsAppendString
                | IntrinsicKind::FsIsDir
                | IntrinsicKind::FsIsFile => {
                    self.lowering.emit_error(
                        expr.span,
                        format!("{:?} is not implemented for compiled backends", kind),
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            )),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicKind::FsExists => {
                    self.lower_fs_exists_into_place(expr, call, place.clone(), expected_ty)?;
                    return Ok(());
                }
                IntrinsicKind::FsRemoveFile => {
                    self.lower_fs_remove_file_as_statement(expr, call)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicKind::EnvVarExists => {
                    self.lower_env_var_exists_into_place(expr, call, place.clone(), expected_ty)?;
                    return Ok(());
                }
                IntrinsicKind::EnvVar => {
                    self.lower_env_var_into_place(expr, call, place.clone(), expected_ty)?;
                    return Ok(());
                }
                IntrinsicKind::Spawn | IntrinsicKind::Select => {
                    if let Some(first) = call.callargs.first() {
                        self.lower_expr_into_place(&first.value, place.clone(), expected_ty)?;
                    } else {
                        self.lowering.emit_error(
                            expr.span,
                            format!("{:?} intrinsic expects at least one argument", kind),
                        );
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(statement);
                    }
                    return Ok(());
                }
                IntrinsicKind::Join => {
                    let args = &call.callargs;
                    if args.is_empty() {
                        self.lowering
                            .emit_error(expr.span, "join intrinsic expects arguments");
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }

                    if args.len() == 1 {
                        self.lower_expr_into_place(&args[0].value, place.clone(), expected_ty)?;
                        return Ok(());
                    }

                    let mut operands = Vec::with_capacity(args.len());
                    for arg in args {
                        let value = self.lower_operand(&arg.value, None)?;
                        operands.push(value.operand);
                    }
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicKind::ProcMacroTokenStreamFromStr
                | IntrinsicKind::ProcMacroTokenStreamToString => {
                    let mut operands = Vec::with_capacity(call.callargs.len());
                    for arg in &call.callargs {
                        let value = self.lower_operand(&arg.value, None)?;
                        operands.push(value.operand);
                    }
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: kind,
                                format: String::new(),
                                args: operands,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                _ => {
                    if let Some((literal, ty)) = self.lower_intrinsic_constant(call, expr.span) {
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty,
                                    user_ty: None,
                                    literal,
                                })),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }

                    self.lowering.emit_warning(
                        expr.span,
                        format!(
                            "intrinsic {:?} is not yet supported for MIR assignment",
                            call.kind
                        ),
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
                },
                None => {
                    // Portable op with no intrinsic equivalent and no
                    // constant-folding rule -- same "not yet supported"
                    // fallback the wildcard arm above uses for intrinsics
                    // this function doesn't otherwise handle.
                    self.lowering.emit_warning(
                        expr.span,
                        format!(
                            "portable op {:?} is not yet supported for MIR assignment",
                            call.kind
                        ),
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
            },
            hir::ExprKind::MethodCall(receiver, method_name, args) => {
                if let Some(constant) =
                    self.lowering
                        .lower_const_expr(expr, Some(expected_ty), None)
                {
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Constant(constant)),
                        ),
                    });
                    return Ok(());
                }
                let mut resolved_info: Option<(MethodLoweringInfo, Option<PlaceInfo>)> = None;
                let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

                if let Some(def_id) = self.lowering.typeck_method_resolutions.get(&expr.hir_id).copied() {
                    // `ensure_method_info` is the uniform lookup, same
                    // shape as `compute_adt_layout` — see `resolve_callee_path`'s
                    // matching comment.
                    if let Some(info) = self.lowering.ensure_method_info(def_id) {
                        resolved_info = Some((info, None));
                        // Signature presence doesn't imply the body's been
                        // lowered yet — ensure it now.
                        self.lowering.ensure_method_lowered(def_id)?;
                    }
                }

                // `str::len`/`str::as_ptr` (`crates/fp-lang/src/std/string/
                // mod.fp`'s `impl str { .. }`) are `compile_error!("compiler
                // intrinsic")`-marked stubs — `str` has no `.fp`-visible
                // fields to read a real body from, so `function_body_is_
                // compiler_intrinsic_marker` drops their HIR body to `None`
                // (`ast_to_hir/items.rs:294-296`). They're still registered
                // into `method_lookup_by_def` like any other non-generic
                // impl method, so without this check `resolved_info` above
                // would already be `Some`, and the plain-call path below
                // would emit a `Call` to that empty-bodied function (which
                // has no lowered statements — `BodyBuilder::lower` skips
                // lowering entirely when `function.body` is `None` — so its
                // return "value" would be whatever's left in its
                // uninitialized return-place local, not the real length or
                // pointer). Intercept by name here — before `resolved_info`
                // is consumed — and compute the real value directly from
                // the receiver's own slice-shaped place, reusing the same
                // `lower_slice_len_place`/`lower_slice_ptr_place` helpers
                // the hand-written env/fs intrinsics already use.
                let str_intrinsic_name = |name: &str| -> Option<bool> {
                    if name == "len" || name.ends_with("::len") {
                        Some(true)
                    } else if name == "as_ptr" || name.ends_with("::as_ptr") {
                        Some(false)
                    } else {
                        None
                    }
                };
                if let Some(is_len) = str_intrinsic_name(method_name.as_str()) {
                    if let Some(receiver_place) = self.lower_place(receiver)? {
                        let mut base_ty = receiver_place.ty.clone();
                        let mut base_place = receiver_place.place.clone();
                        loop {
                            match &base_ty.kind {
                                TyKind::Ref(_, inner, _) => {
                                    base_place.projection.push(mir::PlaceElem::Deref);
                                    base_ty = inner.as_ref().clone();
                                }
                                TyKind::RawPtr(type_and_mut) => {
                                    base_place.projection.push(mir::PlaceElem::Deref);
                                    base_ty = type_and_mut.ty.as_ref().clone();
                                }
                                _ => break,
                            }
                        }
                        if let TyKind::Slice(_) = &base_ty.kind {
                            let (field_place, declared_ty) = if is_len {
                                (
                                    self.lower_slice_len_place(base_place),
                                    Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    },
                                )
                            } else {
                                (
                                    self.lower_slice_ptr_place(base_place),
                                    self.lowering.raw_string_ptr_ty(),
                                )
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = declared_ty;
                            }
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Use(mir::Operand::copy(field_place)),
                                ),
                            });
                            return Ok(());
                        }
                    }
                }

                if (method_name.as_str() == "get_unchecked"
                    || method_name.as_str().ends_with("::get_unchecked"))
                    && args.len() == 1
                {
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        let mut resolved_path = path.clone();
                        self.resolve_self_path(&mut resolved_path);
                        let mut const_info = None;
                        let mut const_body_len = None;
                        if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                            if let Some(info) = self.lowering.const_values.get(def_id) {
                                const_info = Some(info.clone());
                            } else if let Some(konst) =
                                self.lowering.hir_def_map.get(def_id).and_then(|item| {
                                    match &item.kind {
                                        hir::ItemKind::Const(konst) => Some(konst.clone()),
                                        _ => None,
                                    }
                                })
                            {
                                if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                    const_body_len = Some(elements.len() as u64);
                                }
                                self.lowering.register_const_value(*def_id, &konst);
                                if let Some(info) = self.lowering.const_values.get(def_id) {
                                    const_info = Some(info.clone());
                                }
                            }
                        } else if resolved_path.segments.len() == 1 {
                            let name = resolved_path.segments[0].name.as_str();
                            let matching_const = self.lowering.hir_def_map.iter().find_map(
                                |(def_id, item)| match &item.kind {
                                    hir::ItemKind::Const(konst) if konst.name.as_str() == name => {
                                        Some((*def_id, konst.clone()))
                                    }
                                    _ => None,
                                },
                            );
                            if let Some((def_id, konst)) = matching_const {
                                if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                    const_body_len = Some(elements.len() as u64);
                                }
                                self.lowering.register_const_value(def_id, &konst);
                                if let Some(info) = self.lowering.const_values.get(&def_id) {
                                    const_info = Some(info.clone());
                                }
                            }
                        }

                        if let Some(const_info) = const_info {
                            if let mir::ConstantKind::Val(value) = &const_info.value.literal {
                                if let Some((constant, ty)) = self.lowering.const_index_value(
                                    expr.span,
                                    &const_info.typed_value(),
                                    &args[0].value,
                                ) {
                                    self.push_statement(mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Constant(constant)),
                                        ),
                                    });
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = ty.clone();
                                    }
                                    return Ok(());
                                }
                                let mut map_len: Option<u64> = None;
                                let mut map_key_ty: Option<Ty> = None;
                                let mut map_value_ty: Option<Ty> = None;
                                match value {
                                    mir::ConstValue::Map {
                                        entries,
                                        key_ty,
                                        value_ty,
                                    } => {
                                        map_len = Some(entries.len() as u64);
                                        map_key_ty = Some(key_ty.clone());
                                        map_value_ty = Some(value_ty.clone());
                                    }
                                    mir::ConstValue::List { elements, elem_ty } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_len = Some(elements.len() as u64);
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                            }
                                        }
                                    }
                                    mir::ConstValue::Array(elements) => {
                                        if let TyKind::Array(elem_ty, _) = &const_info.ty.kind {
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_len = Some(elements.len() as u64);
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                                if map_len.is_none() {
                                    map_len = const_body_len;
                                }

                                if map_key_ty.is_none() {
                                    let key_operand = self.lower_operand(&args[0].value, None)?;
                                    map_key_ty = Some(key_operand.ty);
                                }
                                if map_value_ty.is_none() {
                                    map_value_ty = Some(expected_ty.clone());
                                }

                                if let (Some(key_ty), Some(value_ty), Some(len)) =
                                    (map_key_ty, map_value_ty, map_len)
                                {
                                    if len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len,
                                                    },
                                                    container: mir::Operand::Constant(
                                                        const_info.typed_value(),
                                                    ),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }

                    if let Ok(receiver_info) = self.lower_operand(receiver, None) {
                        if let mir::Operand::Constant(constant) = &receiver_info.operand {
                            if let mir::ConstantKind::Val(mir::ConstValue::Map {
                                entries,
                                key_ty,
                                value_ty,
                            }) = &constant.literal
                            {
                                let key_operand =
                                    self.lower_operand(arg_values[0], Some(key_ty))?;
                                let kind = mir::ContainerKind::Map {
                                    key_ty: key_ty.clone(),
                                    value_ty: value_ty.clone(),
                                    len: entries.len() as u64,
                                };
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind,
                                            container: receiver_info.operand.clone(),
                                            key: key_operand.operand,
                                        },
                                    ),
                                });

                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                        if let Some(local_id) = self.local_id_from_expr(receiver) {
                            if let Some(container_kind) =
                                self.container_locals.get(&local_id).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        let local_place = mir::Place::from_local(local_id);
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: mir::Operand::copy(local_place),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                        if let mir::Operand::Copy(place) = &receiver_info.operand {
                            if let Some(container_kind) =
                                self.container_locals.get(&place.local).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: receiver_info.operand.clone(),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }

                        let mut map_len: Option<u64> = None;
                        let mut map_key_ty: Option<Ty> = None;
                        let mut map_value_ty: Option<Ty> = None;
                        let receiver_ty = match &receiver_info.ty.kind {
                            TyKind::Ref(_, inner, _) => inner.as_ref(),
                            _ => &receiver_info.ty,
                        };
                        match &receiver_ty.kind {
                            TyKind::Array(elem_ty, len) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                        map_len = self.const_kind_to_u64(expr.span, len);
                                    }
                                }
                            }
                            TyKind::Slice(elem_ty) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                    }
                                }
                            }
                            _ => {}
                        }

                        if map_len.is_none() {
                            if let mir::Operand::Constant(constant) = &receiver_info.operand {
                                if let mir::ConstantKind::Val(value) = &constant.literal {
                                    match value {
                                        mir::ConstValue::Map {
                                            entries,
                                            key_ty,
                                            value_ty,
                                        } => {
                                            map_len = Some(entries.len() as u64);
                                            map_key_ty = Some(key_ty.clone());
                                            map_value_ty = Some(value_ty.clone());
                                        }
                                        mir::ConstValue::List { elements, elem_ty } => {
                                            map_len = Some(elements.len() as u64);
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }

                        if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                            let len = map_len.unwrap_or(0);
                            if len != 0 {
                                let key_operand =
                                    self.lower_operand(&args[0].value, Some(&key_ty))?;
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind: mir::ContainerKind::Map {
                                                key_ty: key_ty.clone(),
                                                value_ty: value_ty.clone(),
                                                len,
                                            },
                                            container: receiver_info.operand,
                                            key: key_operand.operand,
                                        },
                                    ),
                                });
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                    }
                }

                if let Some((info, _cached_place)) = resolved_info {
                    let receiver_expected = info.sig.inputs.get(0);
                    let receiver_operand = self.lower_operand(receiver, receiver_expected)?;

                    let mut lowered_args = Vec::with_capacity(args.len() + 1);
                    lowered_args.push(receiver_operand.operand);
                    for (idx, arg) in args.iter().enumerate() {
                        let expected = info.sig.inputs.get(idx + 1);
                        let operand = self.lower_operand(&arg.value, expected)?;
                        lowered_args.push(operand.operand);
                    }

                    let literal = match info.def_id {
                        Some(def_id) => mir::ConstantKind::FnDef(def_id, Vec::new()),
                        None => mir::ConstantKind::Fn(mir::Symbol::new(info.fn_name.clone())),
                    };
                    let func_operand = mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        ty: info.fn_ty.clone(),
                        user_ty: None,
                        literal,
                    });

                    let continue_block = self.new_block();
                    let destination = Some((place.clone(), continue_block));
                    let terminator = mir::Terminator {
                        source_info: expr.span,
                        kind: mir::TerminatorKind::Call {
                            func: func_operand,
                            args: lowered_args,
                            destination: destination.clone(),
                            cleanup: self.current_unwind_target,
                            from_hir_call: true,
                            fn_span: expr.span,
                        },
                    };

                    self.blocks[self.current_block as usize].terminator = Some(terminator);
                    self.current_block = continue_block;

                    let result_ty = info.sig.output.clone();
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = result_ty.clone();
                    }
                    if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                        self.local_structs.insert(place.local, struct_def);
                    }

                    return Ok(());
                }

                if let Ok(Some(place_info)) = self.lower_place(receiver) {
                    if let Some(def_id) = place_info
                        .struct_def
                        .or_else(|| self.struct_def_from_ty(&place_info.ty))
                    {
                        if let Some(_struct_entry) = self.lowering.struct_defs.get(&def_id) {
                            let method_def = self
                                .lowering
                                .typeck_method_resolutions
                                .get(&expr.hir_id)
                                .and_then(|def_id| self.lowering.method_defs_by_def.get(def_id))
                                .cloned();
                            if let Some(def) = method_def {
                                let method_ctx = self.lowering.make_method_context(&def.self_ty, &def.assoc_types);
                                let tentative_sig = self
                                    .lowering
                                    .lower_function_sig(&def.function.sig, method_ctx.as_ref());
                                let receiver_expected = tentative_sig.inputs.get(0);
                                let receiver_operand =
                                    self.lower_operand(receiver, receiver_expected)?;

                                let mut call_args = args.to_vec();
                                if let Some(mut param_names) =
                                    self.param_names_from_params(&def.function.sig.inputs)
                                {
                                    if !param_names.is_empty() {
                                        param_names.remove(0);
                                    }
                                    call_args = self.reorder_named_call_args(
                                        args,
                                        &param_names,
                                        expr.span,
                                    )?;
                                }

                                let mut lowered_args = Vec::with_capacity(call_args.len() + 1);
                                let mut arg_types = Vec::with_capacity(call_args.len() + 1);
                                arg_types.push(receiver_operand.ty.clone());
                                lowered_args.push(receiver_operand.operand);
                                for (idx, arg) in call_args.iter().enumerate() {
                                    let expected = tentative_sig.inputs.get(idx + 1);
                                    let operand = self.lower_operand(&arg.value, expected)?;
                                    arg_types.push(operand.ty.clone());
                                    lowered_args.push(operand.operand);
                                }

                                let generic_args = self
                                    .lowering
                                    .typeck_generic_method_args
                                    .get(&expr.hir_id)
                                    .cloned()
                                    .ok_or_else(|| {
                                        crate::error::optimization_error(
                                            "missing HIR generic method substitutions",
                                        )
                                    })?;
                                let info = self.lowering.ensure_method_specialization(
                                                                        &def,
                                    &generic_args,
                                    &arg_types,
                                    Some(&place_info.ty),
                                    expr.span,
                                )?;

                                let func_operand = mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                        info.fn_name.clone(),
                                    )),
                                });

                                let continue_block = self.new_block();
                                let destination = Some((place.clone(), continue_block));
                                let terminator = mir::Terminator {
                                    source_info: expr.span,
                                    kind: mir::TerminatorKind::Call {
                                        func: func_operand,
                                        args: lowered_args,
                                        destination: destination.clone(),
                                        cleanup: self.current_unwind_target,
                                        from_hir_call: true,
                                        fn_span: expr.span,
                                    },
                                };

                                self.blocks[self.current_block as usize].terminator =
                                    Some(terminator);
                                self.current_block = continue_block;

                                let result_ty = info.sig.output.clone();
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = result_ty.clone();
                                }
                                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                                    self.local_structs.insert(place.local, struct_def);
                                }

                                return Ok(());
                            }
                        }
                    } else if let Some(enum_def) = self.enum_def_from_ty(&place_info.ty) {
                        if let Some(_enum_entry) = self.lowering.enum_defs.get(&enum_def) {
                            let method_def = self
                                .lowering
                                .typeck_method_resolutions
                                .get(&expr.hir_id)
                                .and_then(|def_id| self.lowering.method_defs_by_def.get(def_id))
                                .cloned();
                            if let Some(def) = method_def {
                                let method_ctx = self.lowering.make_method_context(&def.self_ty, &def.assoc_types);
                                let tentative_sig = self
                                    .lowering
                                    .lower_function_sig(&def.function.sig, method_ctx.as_ref());
                                let receiver_expected = tentative_sig.inputs.get(0);
                                let receiver_operand =
                                    self.lower_operand(receiver, receiver_expected)?;

                                let mut call_args = args.to_vec();
                                if let Some(mut param_names) =
                                    self.param_names_from_params(&def.function.sig.inputs)
                                {
                                    if !param_names.is_empty() {
                                        param_names.remove(0);
                                    }
                                    call_args = self.reorder_named_call_args(
                                        args,
                                        &param_names,
                                        expr.span,
                                    )?;
                                }

                                let mut lowered_args = Vec::with_capacity(call_args.len() + 1);
                                let mut arg_types = Vec::with_capacity(call_args.len() + 1);
                                arg_types.push(receiver_operand.ty.clone());
                                lowered_args.push(receiver_operand.operand);
                                for (idx, arg) in call_args.iter().enumerate() {
                                    let expected = tentative_sig.inputs.get(idx + 1);
                                    let operand = self.lower_operand(&arg.value, expected)?;
                                    arg_types.push(operand.ty.clone());
                                    lowered_args.push(operand.operand);
                                }

                                let generic_args = self
                                    .lowering
                                    .typeck_generic_method_args
                                    .get(&expr.hir_id)
                                    .cloned()
                                    .ok_or_else(|| {
                                        crate::error::optimization_error(
                                            "missing HIR generic method substitutions",
                                        )
                                    })?;
                                let info = self.lowering.ensure_method_specialization(
                                                                        &def,
                                    &generic_args,
                                    &arg_types,
                                    Some(&place_info.ty),
                                    expr.span,
                                )?;

                                let func_operand = mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: info.fn_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(mir::Symbol::new(
                                        info.fn_name.clone(),
                                    )),
                                });

                                let continue_block = self.new_block();
                                let destination = Some((place.clone(), continue_block));
                                let terminator = mir::Terminator {
                                    source_info: expr.span,
                                    kind: mir::TerminatorKind::Call {
                                        func: func_operand,
                                        args: lowered_args,
                                        destination: destination.clone(),
                                        cleanup: self.current_unwind_target,
                                        from_hir_call: true,
                                        fn_span: expr.span,
                                    },
                                };

                                self.blocks[self.current_block as usize].terminator =
                                    Some(terminator);
                                self.current_block = continue_block;

                                let result_ty = info.sig.output.clone();
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = result_ty.clone();
                                }
                                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                                    self.local_structs.insert(place.local, struct_def);
                                }

                                return Ok(());
                            }
                        }
                    }
                }

                if method_name.as_str() == "push" && args.len() == 1 {
                    if let Some(receiver_place) = self.lower_place(receiver)? {
                        if self.is_list_container(&receiver_place.ty) {
                            let elem_ty = self
                                .expect_array_element_ty(&receiver_place.ty)
                                .unwrap_or_else(|| self.lowering.error_ty());
                            let value_info = self.lower_operand(&args[0].value, Some(&elem_ty))?;
                            let kind = mir::ContainerKind::List {
                                elem_ty: elem_ty.clone(),
                                len: 0,
                            };
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    receiver_place.place.clone(),
                                    mir::Rvalue::ContainerPush {
                                        kind,
                                        container: mir::Operand::copy(
                                            receiver_place.place.clone(),
                                        ),
                                        value: value_info.operand,
                                    },
                                ),
                            });
                            // `push` returns unit; still initialize the call
                            // expression's own (unused) destination place.
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                                ),
                            });
                            return Ok(());
                        }
                    }
                }

                if method_name.as_str() == "len" && args.is_empty() {
                    if let Some(constant) =
                        self.lowering
                            .lower_const_expr(receiver, None, None)
                    {
                        if let Some(len) = self.lowering.const_len_from_constant(&constant) {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = len_ty.clone();
                            }
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                        span: expr.span,
                                        ty: len_ty.clone(),
                                        user_ty: None,
                                        literal: mir::ConstantKind::UInt(len),
                                    })),
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }
                    }
                    if let Some(local_id) = self.local_id_from_expr(receiver) {
                        if let Some(kind) = self.container_locals.get(&local_id).cloned() {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = len_ty.clone();
                            }
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::ContainerLen {
                                        kind,
                                        container: mir::Operand::copy(mir::Place::from_local(
                                            local_id,
                                        )),
                                    },
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }
                        if let Some(local) = self.locals.get(local_id as usize) {
                            if self.is_list_container(&local.ty) {
                                let elem_ty = self
                                    .expect_array_element_ty(&local.ty)
                                    .unwrap_or_else(|| self.lowering.error_ty());
                                let len = self
                                    .container_locals
                                    .get(&local_id)
                                    .and_then(|kind| match kind {
                                        mir::ContainerKind::List { len, .. } => Some(*len),
                                        _ => None,
                                    })
                                    .unwrap_or(0);
                                let kind = mir::ContainerKind::List {
                                    elem_ty: elem_ty.clone(),
                                    len,
                                };
                                let len_ty = Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                };
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = len_ty.clone();
                                }
                                let statement = mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place,
                                        mir::Rvalue::ContainerLen {
                                            kind,
                                            container: mir::Operand::copy(mir::Place::from_local(
                                                local_id,
                                            )),
                                        },
                                    ),
                                };
                                self.push_statement(statement);
                                return Ok(());
                            }
                        }
                        let array_len = self.locals.get(local_id as usize).and_then(|local| {
                            if let TyKind::Array(_, len) = &local.ty.kind {
                                Some(len.clone())
                            } else {
                                None
                            }
                        });
                        if let Some(len) = array_len {
                            if let Some(len) = self.const_kind_to_u64(expr.span, &len) {
                                let len_ty = Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                };
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = len_ty.clone();
                                }
                                let statement = mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place,
                                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                            span: expr.span,
                                            ty: len_ty.clone(),
                                            user_ty: None,
                                            literal: mir::ConstantKind::UInt(len),
                                        })),
                                    ),
                                };
                                self.push_statement(statement);
                                return Ok(());
                            }
                        }
                    }
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        if let Some(hir::Res::Def(def_id)) = &path.res {
                            if let Some(const_info) = self.lowering.const_values.get(def_id) {
                                if let Some(len) =
                                    self.lowering.const_len_from_constant(&const_info.value)
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(len),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = &const_info.ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                            if let Some(konst) = self.const_items.get(def_id).cloned() {
                                let ty = self.lower_type_expr(&konst.ty);
                                if let Some(constant) = self.lowering.lower_const_expr(
                                    &konst.body.value,
                                    Some(&ty),
                                    None,
                                ) {
                                    if let Some(len) =
                                        self.lowering.const_len_from_constant(&constant)
                                    {
                                        let len_ty = Ty {
                                            kind: TyKind::Uint(UintTy::Usize),
                                        };
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = len_ty.clone();
                                        }
                                        let statement = mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place,
                                                mir::Rvalue::Use(mir::Operand::Constant(
                                                    mir::Constant {
                                                        span: expr.span,
                                                        ty: len_ty.clone(),
                                                        user_ty: None,
                                                        literal: mir::ConstantKind::UInt(len),
                                                    },
                                                )),
                                            ),
                                        };
                                        self.push_statement(statement);
                                        return Ok(());
                                    }
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    ty: len_ty.clone(),
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                        }
                    }
                    self.lowering.emit_error(
                        expr.span,
                        "len() method is only supported on constant arrays during lowering",
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Use(mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            )),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }

                let receiver_operand = self.lower_operand(receiver, None)?;
                let mut lowered_args = Vec::with_capacity(args.len() + 1);
                let mut input_tys = Vec::with_capacity(args.len() + 1);
                lowered_args.push(receiver_operand.operand.clone());
                input_tys.push(receiver_operand.ty.clone());
                for arg in args {
                    let lowered = self.lower_operand(&arg.value, None)?;
                    input_tys.push(lowered.ty.clone());
                    lowered_args.push(lowered.operand);
                }

                let mut result_ty = expected_ty.clone();
                // `method_name_output_consensus` is maintained incrementally
                // at registration time (see its doc comment) instead of
                // rescanning every struct's whole method table here.
                if let Some(Some(output)) = self
                    .lowering
                    .method_name_output_consensus
                    .get(method_name.as_str())
                {
                    result_ty = output.clone();
                }
                let type_name = self
                    .lowering
                    .display_type_name(&receiver_operand.ty)
                    .unwrap_or_else(|| "opaque".to_string());
                let fn_name = format!("{}::{}", type_name, method_name);
                let sig = mir::FunctionSig {
                    inputs: input_tys,
                    output: result_ty.clone(),
                };
                self.lowering.ensure_runtime_stub(&fn_name, &sig);

                let sanitized_sig = self
                    .lowering
                    .runtime_functions
                    .get(&fn_name)
                    .cloned()
                    .unwrap_or_else(|| self.lowering.sanitize_function_sig(&sig));
                let arg_types = sig.inputs.clone();

                for (idx, expected_input) in sanitized_sig.inputs.iter().enumerate() {
                    if let Some(original_ty) = arg_types.get(idx) {
                        if MirLowering::is_unit_ty(original_ty)
                            && matches!(
                                expected_input.kind,
                                TyKind::Ref(_, _, _) | TyKind::RawPtr(_)
                            )
                        {
                            lowered_args[idx] = mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                ty: expected_input.clone(),
                                user_ty: None,
                                literal: mir::ConstantKind::Null,
                            });
                        }
                    }

                    if let Some(operand) = lowered_args.get_mut(idx) {
                        match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                                if (place.local as usize) < self.locals.len() {
                                    let existing = self.locals[place.local as usize].ty.clone();
                                    if MirLowering::is_unit_ty(&existing)
                                        || matches!(
                                            existing.kind,
                                            TyKind::Infer(_) | TyKind::Error(_)
                                        )
                                    {
                                        self.locals[place.local as usize].ty =
                                            expected_input.clone();
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }

                let func_operand = mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    ty: self.lowering.function_pointer_ty(&sanitized_sig),
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(Symbol::new(fn_name.clone())),
                });

                let continue_block = self.new_block();
                let destination = Some((place.clone(), continue_block));
                self.blocks[self.current_block as usize].terminator = Some(mir::Terminator {
                    source_info: expr.span,
                    kind: mir::TerminatorKind::Call {
                        func: func_operand,
                        args: lowered_args,
                        destination: destination.clone(),
                        cleanup: self.current_unwind_target,
                        from_hir_call: true,
                        fn_span: expr.span,
                    },
                });

                self.current_block = continue_block;
                if (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                }
                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                    self.local_structs.insert(place.local, struct_def);
                }

                return Ok(());
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, Some((place, expected_ty.clone())))?;
            }
            hir::ExprKind::Array(elements) => {
                if self.is_map_container(expected_ty) {
                    let mut entries = Vec::with_capacity(elements.len());
                    let mut key_ty: Option<Ty> = None;
                    let mut value_ty: Option<Ty> = None;

                    for element in elements {
                        let hir::ExprKind::Array(entry) = &element.kind else {
                            self.lowering
                                .emit_error(element.span, "HashMap literal expects array entries");
                            continue;
                        };
                        if entry.len() != 2 {
                            self.lowering.emit_error(
                                element.span,
                                "HashMap literal expects array entries of length 2",
                            );
                            continue;
                        }
                        let key_operand = self.lower_operand(&entry[0], None)?;
                        let value_operand = self.lower_operand(&entry[1], None)?;
                        if key_ty.is_none() {
                            key_ty = Some(key_operand.ty.clone());
                        }
                        if value_ty.is_none() {
                            value_ty = Some(value_operand.ty.clone());
                        }
                        entries.push((key_operand.operand, value_operand.operand));
                    }

                    let key_ty = key_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let value_ty = value_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::Map {
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                        len: entries.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerMapLiteral {
                                kind: kind.clone(),
                                entries,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                if self.is_list_container(expected_ty) {
                    // Derive the element type from the annotated destination
                    // type up front (mirroring the fixed-size-array fallback
                    // below), instead of letting every element default to
                    // whatever type its own literal infers to regardless of
                    // what `expected_ty`'s element type actually is.
                    let declared_elem_ty = self.expect_array_element_ty(expected_ty);
                    let mut operands = Vec::with_capacity(elements.len());
                    let mut elem_ty: Option<Ty> = declared_elem_ty.clone();
                    for element in elements {
                        let lowered = self.lower_operand(element, declared_elem_ty.as_ref())?;
                        if elem_ty.is_none() {
                            elem_ty = Some(lowered.ty.clone());
                        }
                        operands.push(lowered.operand);
                    }

                    let elem_ty = elem_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::List {
                        elem_ty: elem_ty.clone(),
                        len: operands.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerLiteral {
                                kind: kind.clone(),
                                elements: operands,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                let mut element_ty = self.expect_array_element_ty(expected_ty);
                let mut operands = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());
                let mut heterogeneous = false;
                if let Some(elem_ty) = element_ty.clone() {
                    for element in elements {
                        let lowered = self.lower_operand(element, Some(&elem_ty))?;
                        if lowered.ty != elem_ty {
                            heterogeneous = true;
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                } else {
                    for element in elements {
                        let lowered = self.lower_operand(element, None)?;
                        if element_ty.is_none() {
                            element_ty = Some(lowered.ty.clone());
                        } else if let Some(existing) = element_ty.as_ref() {
                            if &lowered.ty != existing {
                                heterogeneous = true;
                            }
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                }

                let expected_is_array = matches!(&expected_ty.kind, TyKind::Array(_, _))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _) if matches!(inner.kind, TyKind::Array(_, _))
                    );
                if heterogeneous && expected_is_array {
                    self.lowering
                        .emit_error(expr.span, "array literal elements have mismatched types");
                }

                let element_ty = element_ty.unwrap_or_else(|| {
                    self.lowering
                        .emit_error(expr.span, "array expression expected array type");
                    self.lowering.error_ty()
                });

                let expected_is_slice = matches!(&expected_ty.kind, TyKind::Slice(_))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _)
                            if matches!(inner.kind, TyKind::Slice(_))
                    );
                if (expected_is_slice || matches!(expected_ty.kind, TyKind::Error(_)))
                    && place.projection.is_empty()
                {
                    let array_ty = Ty {
                        kind: TyKind::Array(
                            Box::new(element_ty.clone()),
                            ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                                data: elements.len() as u128,
                                size: 8,
                            }))),
                        ),
                    };
                    if let Some(local) = self.locals.get_mut(place.local as usize) {
                        local.ty = array_ty;
                    }
                }

                let aggregate_kind = if heterogeneous && !expected_is_array {
                    if place.projection.is_empty() {
                        let tuple_ty = Ty {
                            kind: TyKind::Tuple(element_types.into_iter().map(Box::new).collect()),
                        };
                        if let Some(local) = self.locals.get_mut(place.local as usize) {
                            local.ty = tuple_ty;
                        }
                    }
                    mir::AggregateKind::Tuple
                } else {
                    mir::AggregateKind::Array(element_ty.clone())
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(aggregate_kind, operands),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let element_ty = self
                    .expect_array_element_ty(expected_ty)
                    .unwrap_or_else(|| {
                        self.lowering
                            .emit_error(expr.span, "array repeat expression expected array type");
                        self.lowering.error_ty()
                    });

                let lowered_elem = self.lower_operand(elem, Some(&element_ty))?;
                let repeat_len = match self.evaluate_array_length(len) {
                    Some(len) => len,
                    None => {
                        self.lowering
                            .emit_error(len.span, "array repeat length must be a constant integer");
                        0
                    }
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Repeat(lowered_elem.operand, repeat_len),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::Tuple(elements) => {
                let mut operands = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());
                for element in elements {
                    let lowered = self.lower_operand(element, None)?;
                    element_types.push(lowered.ty.clone());
                    operands.push(lowered.operand);
                }
                if place.projection.is_empty() {
                    let tuple_ty = Ty {
                        kind: TyKind::Tuple(element_types.into_iter().map(Box::new).collect()),
                    };
                    if let Some(local) = self.locals.get_mut(place.local as usize) {
                        local.ty = tuple_ty;
                    }
                }
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                    ),
                };
                self.push_statement(statement);
            }
            _ => {
                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating expression {:?} as unit during MIR assignment",
                        expr.kind
                    ),
                );
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                };
                self.push_statement(statement);
            }
        }

        Ok(())
    }

    fn convert_bin_op(op: &hir::BinOp) -> mir::BinOp {
        match op {
            hir::BinOp::Add => mir::BinOp::Add,
            hir::BinOp::Sub => mir::BinOp::Sub,
            hir::BinOp::Mul => mir::BinOp::Mul,
            hir::BinOp::Div => mir::BinOp::Div,
            hir::BinOp::Rem => mir::BinOp::Rem,
            hir::BinOp::And => mir::BinOp::And,
            hir::BinOp::Or => mir::BinOp::Or,
            hir::BinOp::BitXor => mir::BinOp::BitXor,
            hir::BinOp::BitAnd => mir::BinOp::BitAnd,
            hir::BinOp::BitOr => mir::BinOp::BitOr,
            hir::BinOp::Shl => mir::BinOp::Shl,
            hir::BinOp::Shr => mir::BinOp::Shr,
            hir::BinOp::Eq => mir::BinOp::Eq,
            hir::BinOp::Ne => mir::BinOp::Ne,
            hir::BinOp::Lt => mir::BinOp::Lt,
            hir::BinOp::Le => mir::BinOp::Le,
            hir::BinOp::Gt => mir::BinOp::Gt,
            hir::BinOp::Ge => mir::BinOp::Ge,
        }
    }

    fn convert_un_op(op: &hir::UnOp) -> Option<mir::UnOp> {
        match op {
            hir::UnOp::Not => Some(mir::UnOp::Not),
            hir::UnOp::Neg => Some(mir::UnOp::Neg),
            hir::UnOp::Deref | hir::UnOp::Box => None,
        }
    }

    fn binary_result_ty(op: &hir::BinOp, lhs_ty: &Ty) -> Ty {
        match op {
            hir::BinOp::Add
            | hir::BinOp::Sub
            | hir::BinOp::Mul
            | hir::BinOp::Div
            | hir::BinOp::Rem
            | hir::BinOp::BitXor
            | hir::BinOp::BitAnd
            | hir::BinOp::BitOr
            | hir::BinOp::Shl
            | hir::BinOp::Shr => lhs_ty.clone(),
            hir::BinOp::And
            | hir::BinOp::Or
            | hir::BinOp::Eq
            | hir::BinOp::Ne
            | hir::BinOp::Lt
            | hir::BinOp::Le
            | hir::BinOp::Gt
            | hir::BinOp::Ge => Ty { kind: TyKind::Bool },
        }
    }

    fn expect_array_element_ty(&self, ty: &Ty) -> Option<Ty> {
        match &ty.kind {
            TyKind::Array(elem, _) => Some(*elem.clone()),
            TyKind::Slice(elem) => Some(*elem.clone()),
            TyKind::Ref(_, elem, _) => match &elem.kind {
                TyKind::Array(inner, _) => Some(*inner.clone()),
                TyKind::Slice(inner) => Some(*inner.clone()),
                _ => None,
            },
            TyKind::Adt(_, args) if self.is_list_container(ty) => args.iter().find_map(|arg| {
                if let GenericArg::Type(element) = arg {
                    Some(element.clone())
                } else {
                    None
                }
            }),
            _ => None,
        }
    }

    fn container_type_name(&self, ty: &Ty) -> Option<String> {
        self.lowering.display_type_name(ty)
    }

    fn is_list_container(&self, ty: &Ty) -> bool {
        if matches!(ty.kind, TyKind::Slice(_)) {
            return true;
        }
        self.container_type_name(ty)
            .map(|name| matches!(name.as_str(), "Vec" | "List" | "list"))
            .unwrap_or(false)
    }

    fn is_map_container(&self, ty: &Ty) -> bool {
        self.container_type_name(ty)
            .map(|name| name == "HashMap")
            .unwrap_or(false)
    }

    /// Whether `ty` is a nominal struct with a real, registered `index`
    /// method — i.e. it has a genuine `Index`-trait-style impl (like
    /// `Vec<T>`'s in `crates/fp-lang/src/std/alloc/mod.fp`) to dispatch
    /// `x[i]` through, as opposed to a genuine slice/array (no struct to
    /// dispatch a method on at all) or an ADT with no such method (e.g.
    /// `HashMap`, which keeps its own hardcoded `is_map_container` path
    /// unchanged — out of scope here). Deliberately checks for the method
    /// itself, not any struct name: this is what makes indexing dispatch
    /// work generically for *any* type that implements `Index`, not just
    /// `Vec` specifically.
    fn real_indexable_struct_def_id(&self, ty: &Ty) -> Option<hir::DefId> {
        // `self[idx]` inside a `&self`/`&mut self` method (e.g. `Vec<&str>
        // ::join`) has a receiver of type `&Vec<T>`, not `Vec<T>` directly
        // — peel the reference the same way callers elsewhere in this file
        // do (e.g. `hir_typeck`'s own `Index` type-checking) before
        // checking for the underlying ADT.
        let mut ty = ty;
        if let TyKind::Ref(_, inner, _) = &ty.kind {
            ty = inner.as_ref();
        }
        let TyKind::Adt(adt, _) = &ty.kind else {
            return None;
        };
        let has_index_method = self
            .lowering
            .method_defs_by_self_and_name
            .contains_key(&(adt.did, "index".to_string()));
        has_index_method.then_some(adt.did)
    }

    /// Look up a real method named `method_name` on the struct identified
    /// by `struct_def_id` (matched structurally via `MethodDefinition
    /// ::self_def`, not by re-deriving a qualified-name string — the
    /// struct's own `DefId` is already unambiguous), lower `receiver` and
    /// each of `extra_args` against the method's own declared parameter
    /// types (so `receiver` becomes a proper `&Self`/`&mut Self` reference
    /// rather than a bare value copy, matching how every other real
    /// method-call receiver in this file is lowered — see the
    /// `tentative_sig`/`receiver_expected` pattern in the `MethodCall`
    /// lowering above), specialize via `ensure_method_specialization`, and
    /// emit a `Call` terminator writing the result into `place`. Used to
    /// dispatch `x[i]`/`x[i] = v` through a real `index`/`index_set`
    /// method exactly as `receiver.index(i)`/`receiver.index_set(i, v)`
    /// would — indexing has no receiver-expression HIR shape of its own to
    /// fall into the ordinary `MethodCall` lowering paths, but it can
    /// still reuse the same specialization/`Call`-terminator machinery.
    fn call_real_method_into_place(
        &mut self,
        struct_def_id: hir::DefId,
        method_name: &str,
        receiver: &hir::Expr,
        extra_args: &[&hir::Expr],
        place: mir::Place,
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<Ty> {
        let def = self
            .lowering
            .method_defs_by_self_and_name
            .get(&(struct_def_id, method_name.to_string()))
            .and_then(|def_id| self.lowering.method_defs_by_def.get(def_id))
            .cloned()
            .ok_or_else(|| {
                crate::error::optimization_error(format!(
                    "no method `{}` found on struct {:?}",
                    method_name, struct_def_id
                ))
            })?;
        let method_ctx = self.lowering.make_method_context(&def.self_ty, &def.assoc_types);
        let tentative_sig = self
            .lowering
            .lower_function_sig(&def.function.sig, method_ctx.as_ref());
        let receiver_operand = self.lower_operand(receiver, tentative_sig.inputs.get(0))?;
        let mut lowered_args = Vec::with_capacity(extra_args.len() + 1);
        let mut arg_types = Vec::with_capacity(extra_args.len() + 1);
        arg_types.push(receiver_operand.ty.clone());
        lowered_args.push(receiver_operand.operand);
        for (idx, arg) in extra_args.iter().enumerate() {
            let operand = self.lower_operand(arg, tentative_sig.inputs.get(idx + 1))?;
            arg_types.push(operand.ty.clone());
            lowered_args.push(operand.operand);
        }
        let info = self.lowering.ensure_method_specialization(
                        &def,
            &[],
            &arg_types,
            expected_return,
            span,
        )?;
        let func_operand = mir::Operand::Constant(mir::Constant {
            span,
            ty: info.fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new(info.fn_name.clone())),
        });
        let continue_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args: lowered_args,
                destination: Some((place.clone(), continue_block)),
                cleanup: self.current_unwind_target,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);
        self.current_block = continue_block;
        if (place.local as usize) < self.locals.len() {
            self.locals[place.local as usize].ty = info.sig.output.clone();
        }
        Ok(info.sig.output.clone())
    }

    fn local_id_from_expr(&self, expr: &hir::Expr) -> Option<mir::LocalId> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        if let Some(hir::Res::Local(hir_id)) = &path.res {
            return self.local_map.get(hir_id).copied();
        }
        path.segments
            .first()
            .filter(|_| path.segments.len() == 1)
            .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied())
    }

    fn evaluate_array_length(&self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = path.res {
                    if let Some(const_info) = self.lowering.const_values.get(&def_id) {
                        match &const_info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value as u64),
                            mir::ConstantKind::UInt(value) => Some(*value),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn new_block(&mut self) -> mir::BasicBlockId {
        let id = self.blocks.len() as mir::BasicBlockId;
        self.blocks.push(mir::BasicBlockData::new(None));
        id
    }

    fn push_statement(&mut self, statement: mir::Statement) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.statements.push(statement);
        }
    }
}

fn is_known_type_name(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f16"
            | "f32"
            | "f64"
            | "f128"
            | "bool"
            | "char"
            | "str"
            | "string"
            | "type"
            | "__fp_type"
            | "__fp_escaped"
    )
}
