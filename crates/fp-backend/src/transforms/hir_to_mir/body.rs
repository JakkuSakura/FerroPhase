// BodyBuilder: per-body MIR lowering (moved out of expr.rs).
// BodyBuilder drives statement/expression lowering for a single function body,
// delegating type/ADT/method queries back to `HirToMirLowerer` via `self.lowering`.

use fp_core::ast::{Value, ValueList, ValueMap, ValueTuple};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::hir::place::{
    HirAssignTargetBase, HirAssignTargetProjection, project_hir_assign_target,
};
use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir::ty::{
    AdtDef, ConstKind, ConstValue, FloatTy, GenericArg, IntTy, Mutability, Scalar, ScalarInt, Ty,
    TyKind, TypeAndMut, UintTy,
};
use fp_core::mir::{
    self, EnumLayout, EnumVariantInfo, MethodContext, MethodDefinition, MethodLoweringInfo,
    StructDefinition, StructLayout, Symbol,
};
use fp_core::ops::format_value_with_spec;
use fp_core::span::Span;
use std::collections::{HashMap, HashSet};

use super::expr::{HirToMirLowerer, StructFieldInfo, call_arg_values};
use super::guards::ExprRecursionGuard;
use super::type_names::is_known_type_name;

/// One undone mutation to `local_map`/`fallback_locals`, recorded by
/// `bind_match_binding` while `BodyBuilder::match_binding_undo_log` is
/// active. Carries the key's *previous* value (`None` if the key was
/// absent) so restoring can distinguish "put the old mapping back" from
/// "the key didn't exist before this arm" — a plain `.remove(key)` would
/// wrongly erase an outer-scope binding of the same name that the arm's
/// pattern shadowed.
pub(super) enum MatchBindingUndo {
    LocalMap(hir::HirId, Option<mir::LocalId>),
    Fallback(String, Option<mir::LocalId>),
}

pub(crate) struct BodyBuilder<'a> {
    pub(super) lowering: &'a mut HirToMirLowerer,
    pub(super) function: &'a hir::Function,
    pub(super) sig: &'a mir::FunctionSig,
    pub(super) locals: Vec<mir::LocalDecl>,
    pub(super) local_map: HashMap<hir::HirId, mir::LocalId>,
    pub(super) fallback_locals: HashMap<String, mir::LocalId>,
    /// When `Some`, `bind_match_binding` records the pre-insert value (if
    /// any) of every `local_map`/`fallback_locals` key it touches here,
    /// instead of `lower_match_expr` cloning both whole maps before every
    /// arm and restoring the clones afterward — turns an O(arms ×
    /// bindings-so-far) clone-and-restore into an O(bindings-in-this-arm)
    /// undo log. Only ever active for the duration of a single
    /// `bind_match_pattern` call.
    pub(super) match_binding_undo_log: Option<Vec<MatchBindingUndo>>,
    pub(super) local_structs: HashMap<mir::LocalId, hir::DefId>,
    pub(super) container_locals: HashMap<mir::LocalId, mir::ContainerKind>,
    pub(super) const_items: HashMap<hir::DefId, hir::Const>,
    pub(super) blocks: Vec<mir::BasicBlockData>,
    pub(super) current_block: mir::BasicBlockId,
    pub(super) span: Span,
    pub(super) method_context: Option<MethodContext>,
    pub(super) type_substs: HashMap<String, Ty>,
    pub(super) loop_stack: Vec<LoopContext>,
    pub(super) defer_scopes: Vec<DeferScope>,
    pub(super) current_unwind_target: Option<mir::BasicBlockId>,
    pub(super) null_locals: HashSet<mir::LocalId>,
    pub(super) active_exprs: HashSet<hir::HirId>,
    pub(super) control_flow_emitted: bool,
}

pub(super) struct PlaceInfo {
    pub(super) place: mir::Place,
    pub(super) ty: Ty,
    pub(super) struct_def: Option<hir::DefId>,
}

pub(super) struct OperandInfo {
    pub(super) operand: mir::Operand,
    pub(super) ty: Ty,
}

pub(super) struct StructRef {
    pub(super) def_id: hir::DefId,
    pub(super) args: Vec<Ty>,
}

impl OperandInfo {
    pub(super) fn constant(span: Span, ty: Ty, literal: mir::ConstantKind) -> Self {
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
pub(super) struct LoopDestination {
    pub(super) place: mir::Place,
    pub(super) ty: Ty,
}

#[derive(Clone)]
pub(super) struct LoopContext {
    pub(super) break_block: mir::BasicBlockId,
    pub(super) continue_block: mir::BasicBlockId,
    pub(super) break_destination: Option<LoopDestination>,
    pub(super) break_value_allowed: bool,
    pub(super) defer_scope_depth: usize,
}

pub(super) struct DeferScope {
    pub(super) deferred: Vec<hir::Expr>,
}

impl<'a> BodyBuilder<'a> {
    pub(crate) fn new(
        lowering: &'a mut HirToMirLowerer,
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

    pub(super) fn push_local(&mut self, decl: mir::LocalDecl) -> mir::LocalId {
        let local_id = self.locals.len() as mir::LocalId;
        self.locals.push(decl);
        local_id
    }

    pub(super) fn is_null_literal_expr(expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Literal(hir::Lit::Null))
    }

    pub(super) fn update_null_tracking(
        &mut self,
        place: mir::Place,
        ty: Option<&Ty>,
        expr: &hir::Expr,
    ) {
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

    pub(super) fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
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
        if let Some(ty) = self.lowering.typeck_type_expr_type(ty_expr.hir_id.clone()) {
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
                && (self.type_substs.is_empty() || !self.lowering.has_unresolved_ty(&ty));
            if trust_cache {
                return ty;
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

    pub(super) fn is_builtin_type_path(ty_expr: &hir::TypeExpr) -> bool {
        let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
            return false;
        };
        let Some(segment) = path.segments().last() else {
            return false;
        };
        matches!(
            segment.ident.as_str(),
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

    pub(super) fn type_expr_mentions_self(ty_expr: &hir::TypeExpr) -> bool {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments()
                .first()
                .map(|segment| segment.ident.as_str() == "Self")
                .unwrap_or(false),
            hir::TypeExprKind::Tuple(items) => {
                items.iter().any(|item| Self::type_expr_mentions_self(item))
            }
            hir::TypeExprKind::Array(item, _) | hir::TypeExprKind::Slice(item) => {
                Self::type_expr_mentions_self(item)
            }
            hir::TypeExprKind::Ptr { inner: item, .. } | hir::TypeExprKind::Ref(item) => {
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

    pub(super) fn bind_pattern(&mut self, pat: &hir::Pat, local: mir::LocalId, ty: Option<&Ty>) {
        match &pat.kind {
            hir::PatKind::Binding { name, mutable } => {
                self.local_map.insert(pat.hir_id.clone(), local);
                self.fallback_locals
                    .insert(name.as_str().to_string(), local);
                if let Some(decl) = self.locals.get_mut(local as usize) {
                    if *mutable {
                        decl.mutability = mir::Mutability::Mut;
                    }
                    let mut struct_def = ty.and_then(|ty| self.struct_def_from_ty(ty));
                    if let Some(ctx) = &self.method_context {
                        if let Some(ref def_id) = ctx.def_id {
                            let name_matches_self = name.as_str() == "self";
                            let ty_matches_self = ty
                                .map(|ty| self.ty_matches(ty, &ctx.mir_self_ty))
                                .unwrap_or(false);
                            if name_matches_self || ty_matches_self {
                                struct_def = Some(def_id.clone());
                            }
                        }
                    }
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(local, def_id);
                    }
                }
            }
            hir::PatKind::Wild => {
                self.local_map.insert(pat.hir_id.clone(), local);
            }
            _ => {
                self.local_map.insert(pat.hir_id.clone(), local);
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

    pub(super) fn struct_def_from_ty(&mut self, ty: &Ty) -> Option<hir::DefId> {
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
                if !self
                    .lowering
                    .mir_package
                    .borrow()
                    .struct_defs
                    .contains_key(&adt.did)
                {
                    self.lowering
                        .try_lazily_register_adt(adt.did.clone(), Span::null());
                }
                if self
                    .lowering
                    .mir_package
                    .borrow()
                    .struct_defs
                    .contains_key(&adt.did)
                {
                    Some(adt.did.clone())
                } else {
                    Self::struct_def_from_ty_by_name(self.lowering, ty)
                }
            }
            _ => Self::struct_def_from_ty_by_name(self.lowering, ty),
        }
    }

    pub(super) fn struct_def_from_ty_by_name(
        lowering: &HirToMirLowerer,
        ty: &Ty,
    ) -> Option<hir::DefId> {
        lowering
            .mir_package
            .borrow()
            .struct_layouts_by_ty
            .get(ty)
            .map(|key| key.def_id.clone())
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
                    .mir_package
                    .borrow()
                    .struct_defs_by_tail_name
                    .get(HirToMirLowerer::name_tail(&name))
                    .cloned()?;
                let matches: Vec<hir::DefId> = candidates
                    .iter()
                    .filter_map(|def_id| {
                        let def = lowering
                            .mir_package
                            .borrow()
                            .struct_defs
                            .get(def_id)
                            .cloned()?;
                        if def.name == name || def.name.ends_with(&format!("::{}", name)) {
                            Some(def_id.clone())
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

    pub(super) fn boxed_inner_ty(&self, ty: &Ty) -> Option<Ty> {
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

    pub(super) fn enum_def_from_ty(&self, ty: &Ty) -> Option<hir::DefId> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_def_from_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.enum_def_from_ty(type_and_mut.ty.as_ref()),
            // Mirrors `struct_def_from_ty`'s `Adt`-shell check: a lazily
            // resolved generic argument (`adt_shell_ty`) carries the real
            // `DefId` directly, so it's checked before falling back to the
            // by-value `enum_layouts` scan below.
            TyKind::Adt(adt, _)
                if self
                    .lowering
                    .mir_package
                    .borrow()
                    .enum_defs
                    .contains_key(&adt.did) =>
            {
                Some(adt.did.clone())
            }
            _ => self
                .lowering
                .mir_package
                .borrow()
                .enum_layouts
                .iter()
                .find_map(|(key, layout)| (layout.enum_ty == *ty).then_some(key.def_id.clone())),
        }
    }

    pub(super) fn enum_layout_for_ty(&mut self, ty: &Ty, span: Span) -> Option<EnumLayout> {
        if let Some(layout) = self.lowering.enum_layout_for_ty_exact(ty) {
            return Some(layout.clone());
        }
        self.lowering
            .enum_layout_for_concrete_ty(ty, span)
            .or_else(|| self.lowering.enum_layout_for_ty(ty))
    }

    pub(super) fn enum_layout_for_variant(
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

    pub(super) fn enum_layout_for_variant_ty(
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
                let mut layout =
                    self.lowering
                        .enum_layout_for_instance(adt.did.clone(), &args, span)?;
                if !layout.variant_payloads.contains_key(&variant.def_id) {
                    if let Some(payloads) = self
                        .lowering
                        .enum_variant_payloads_for_args(variant, &args, span)
                    {
                        layout
                            .variant_payloads
                            .insert(variant.def_id.clone(), payloads);
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
                let mut layout =
                    self.lowering
                        .enum_layout_for_instance(def_id.clone(), &args, span)?;
                if !layout.variant_payloads.contains_key(&variant.def_id) {
                    if let Some(payloads) = self
                        .lowering
                        .enum_variant_payloads_for_args(variant, &args, span)
                    {
                        layout
                            .variant_payloads
                            .insert(variant.def_id.clone(), payloads);
                    }
                }
                Some(layout)
            }
            _ => None,
        }
    }

    pub(super) fn infer_enum_args_from_expected_ty(
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
                        if let Some(args) =
                            self.infer_enum_args_from_expected_ty(enum_def.clone(), inner)
                        {
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
                        if let Some(args) =
                            self.infer_enum_args_from_expected_ty(enum_def.clone(), inner)
                        {
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
    pub(super) fn payload_types_from_type_substs(
        &mut self,
        variant: &EnumVariantInfo,
        span: Span,
    ) -> Option<Vec<Ty>> {
        if self.type_substs.is_empty() {
            return None;
        }
        let generics = self
            .lowering
            .mir_package
            .borrow()
            .enum_defs
            .get(&variant.enum_def)
            .cloned()?
            .generics
            .clone();
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

    pub(super) fn variant_payloads_from_layout_or_ty(
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
                .mir_package
                .borrow()
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

    pub(super) fn ty_matches(&self, lhs: &Ty, rhs: &Ty) -> bool {
        fn strip_refs<'a>(ty: &'a Ty) -> &'a Ty {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => strip_refs(inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => strip_refs(type_and_mut.ty.as_ref()),
                _ => ty,
            }
        }

        strip_refs(lhs) == strip_refs(rhs)
    }

    pub(super) fn ty_matches_with_opaque(&self, lhs: &Ty, rhs: &Ty) -> bool {
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

    pub(crate) fn lower(mut self) -> Result<mir::Body> {
        let has_body = self.function.body.is_some();
        if let Some(body) = &self.function.body {
            self.collect_local_consts(body);
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

    fn collect_local_consts(&mut self, block: &hir::Block) {
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Item(item) => {
                    if let hir::ItemKind::Const(konst) = &item.kind {
                        self.const_items.insert(item.def_id.clone(), konst.clone());
                    }
                }
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    self.collect_local_consts_from_expr(expr);
                }
                hir::StmtKind::Local(local) => {
                    if let Some(init) = &local.init {
                        self.collect_local_consts_from_expr(init);
                    }
                }
            }
        }
        if let Some(expr) = &block.expr {
            self.collect_local_consts_from_expr(expr);
        }
    }

    fn collect_local_consts_from_expr(&mut self, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.collect_local_consts(block)
            }
            hir::ExprKind::While(_, block) | hir::ExprKind::For(_, _, block) => {
                self.collect_local_consts(block)
            }
            hir::ExprKind::If(_, then_block, else_expr) => {
                self.collect_local_consts_from_expr(then_block);
                if let Some(else_expr) = else_expr {
                    self.collect_local_consts_from_expr(else_expr);
                }
            }
            _ => {}
        }
    }

    pub(super) fn ensure_terminated(&mut self) {
        if let Some(block) = self.blocks.last_mut() {
            if block.terminator.is_none() {
                block.terminator = Some(mir::Terminator {
                    source_info: self.span,
                    kind: mir::TerminatorKind::Return,
                });
            }
        }
    }

    pub(super) fn pattern_always_matches(&self, pat: &hir::Pat) -> bool {
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

    pub(super) fn resolve_self_path(&self, path: &mut hir::Path) {
        if let Some(context) = &self.method_context {
            if let Some(first) = path.segments().first() {
                if first.ident.as_str() == "Self" {
                    let mut new_segments = context.path.clone();
                    new_segments.extend(path.segments().iter().skip(1).cloned());
                    path.segments = new_segments;
                    if let Some(ref def_id) = context.def_id {
                        path.res = hir::Res::Def(def_id.clone());
                    }
                }
            }
        }
    }

    pub(super) fn lower_call(
        &mut self,
        expr: &hir::Expr,
        callee: &hir::Expr,
        args: &[hir::CallArg],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<Option<PlaceInfo>> {
        let mut reordered_args = None;
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some(param_names) = path
                .path()
                .and_then(|path| self.param_names_for_callee(path))
            {
                let ordered = self.reorder_named_call_args(args, &param_names, expr.span)?;
                reordered_args = Some(ordered);
            }
        }
        let args = reordered_args.as_deref().unwrap_or(args);
        let arg_values = call_arg_values(args);
        // A type-qualified call such as `TypeBuilder::new("Config")` has a
        // callee path resolved to the base type, not to `new` itself. The
        // type checker records the selected associated item's DefId on the
        // call/callee HIR nodes; use that identity before inspecting the
        // syntactic path so intrinsic wrappers lower exactly like methods.
        let resolved_call_def = self
            .lowering
            .typeck_method_resolution(expr.hir_id.clone())
            .or_else(|| {
                self.lowering
                    .typeck_method_resolution(callee.hir_id.clone())
            });
        if let Some(def_id) = resolved_call_def {
            if let Some(kind) = self.lowering.hir_program.intrinsic_def(def_id) {
                if matches!(kind, IntrinsicKind::Print | IntrinsicKind::Println) {
                    self.lower_resolved_print_call(expr, kind, args, destination.clone())?;
                    return Ok(None);
                }
                if self.lower_resolved_intrinsic_call(
                    expr,
                    kind,
                    &arg_values,
                    destination.clone(),
                )? {
                    return Ok(None);
                }
            }
        }
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let hir::Res::Def(def_id) = &path.res_ref() {
                if let Some(kind) = self.lowering.hir_program.intrinsic_def(def_id.clone()) {
                    if matches!(kind, IntrinsicKind::Print | IntrinsicKind::Println) {
                        self.lower_resolved_print_call(expr, kind, args, destination.clone())?;
                        return Ok(None);
                    }
                    if self.lower_resolved_intrinsic_call(
                        expr,
                        kind,
                        &arg_values,
                        destination.clone(),
                    )? {
                        return Ok(None);
                    }
                }
            }
        }
        if let hir::ExprKind::Path(path) = &callee.kind {
            let segments = &path.segments();
            if segments.len() >= 2
                && segments[segments.len() - 2].ident.as_str() == "HashMap"
                && segments[segments.len() - 1].ident.as_str() == "from"
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
                            let tail = path.segments().last().map(|seg| seg.ident.as_str());
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
            if segments.last().map(|seg| seg.ident.as_str()) == Some("raw_parts_to_str") {
                // `std::ffi::raw_parts_to_str(ptr, len)` — the one genuinely
                // backend-level primitive `CStr::as_str_unchecked` needs:
                // assembling a `&str`/`str` fat pointer from an already
                // runtime-computed `(ptr, len)` pair. Everything else about
                // `CStr` (fields, `from_ptr`, `as_ptr`, the `strlen` call
                // itself) is ordinary, real `.fp` code.
                if let Some((place, expected_ty)) = destination {
                    if arg_values.len() != 2 {
                        self.lowering
                            .emit_error(expr.span, "raw_parts_to_str expects (ptr, len) arguments");
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
            let tail = path.segments().last().map(|seg| seg.ident.as_str());
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
                    if let hir::QPath::Resolved(_, inner) = &mut resolved_path {
                        self.resolve_self_path(inner);
                    }
                    let mut const_info = None;
                    let mut const_body_len = None;
                    if let hir::Res::Def(def_id) = &resolved_path.res_ref() {
                        if let Some(info) = self.lowering.ensure_const_info(def_id.clone()) {
                            const_info = Some(info.clone());
                        } else if let Some(konst) =
                            self.lowering.hir_item(def_id.clone()).and_then(|item| {
                                match &item.kind {
                                    hir::ItemKind::Const(konst) => Some(konst.clone()),
                                    _ => None,
                                }
                            })
                        {
                            if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                const_body_len = Some(elements.len() as u64);
                            }
                            self.lowering.ensure_item_lowered(def_id.clone())?;
                            if let Some(info) = self.lowering.ensure_const_info(def_id.clone()) {
                                const_info = Some(info.clone());
                            }
                        }
                    } else if resolved_path.segments().len() == 1 {
                        let name = resolved_path.segments()[0].ident.as_str();
                        let matching_const =
                            self.lowering
                                .hir_all_items()
                                .find_map(|item| match &item.kind {
                                    hir::ItemKind::Const(konst) if konst.name.as_str() == name => {
                                        Some((item.def_id.clone(), konst.clone()))
                                    }
                                    _ => None,
                                });
                        if let Some((def_id, konst)) = matching_const {
                            if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                const_body_len = Some(elements.len() as u64);
                            }
                            self.lowering.ensure_item_lowered(def_id.clone())?;
                            if let Some(info) = self.lowering.ensure_const_info(def_id.clone()) {
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
                                mir::ConstValue::Array(elements) => {
                                    if let Some(elem_ty) =
                                        self.expect_array_element_ty(&const_info.ty)
                                    {
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
                            mir::ConstValue::Array(elements) => {
                                if let Some(elem_ty) =
                                    self.expect_array_element_ty(&container_info.ty)
                                {
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
            let tail = path.segments().last().map(|seg| seg.ident.as_str());
            let variant = path
                .path()
                .and_then(|path| self.enum_variant_info_from_path(path))
                .or_else(|| {
                    path.path()
                        .and_then(|path| self.enum_variant_info_from_expected(path, expected_ty))
                })
                .or_else(|| {
                    tail.and_then(|name| {
                        expected_ty.and_then(|ty| self.result_variant_from_expected(ty, name))
                    })
                });
            if let Some(variant) = variant {
                let explicit_enum_args = path
                    .path()
                    .and_then(|path| {
                        path.segments
                            .iter()
                            .find_map(|segment| segment.explicit_args())
                    })
                    .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                let mut layout = destination.as_ref().and_then(|(_, ty)| {
                    self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                });
                if layout.is_none() {
                    if !explicit_enum_args.is_empty() {
                        layout = self.lowering.enum_layout_for_instance(
                            variant.enum_def.clone(),
                            &explicit_enum_args,
                            expr.span,
                        );
                    } else if let Some((_, expected_ty)) = destination.as_ref() {
                        if let Some(inferred_args) = self
                            .infer_enum_args_from_expected_ty(variant.enum_def.clone(), expected_ty)
                        {
                            layout = self.lowering.enum_layout_for_instance(
                                variant.enum_def.clone(),
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
                            .enum_layout_for_def(variant.enum_def.clone(), expr.span);
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
                if let Some(const_info) = self.lowering.ensure_const_info(variant.def_id.clone()) {
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
                .segments()
                .last()
                .and_then(|segment| segment.explicit_args())
            {
                explicit_args = self.lowering.lower_generic_args(Some(args), expr.span);
            }
            if explicit_args.is_empty() {
                if let Some(args) = self.lowering.typeck_generic_call_arg(expr.hir_id.clone()) {
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
                    if !composed
                        .iter()
                        .any(|ty| self.lowering.has_unresolved_ty(ty))
                    {
                        explicit_args = composed;
                    }
                }
            }
            if let hir::Res::Def(def_id) = &path.res_ref() {
                if self
                    .lowering
                    .mir_package
                    .borrow()
                    .generic_function_defs
                    .contains_key(def_id)
                {
                    generic_def_id = Some(def_id.clone());
                }
            }
            let resolved_method_def_id = self
                .lowering
                .typeck_method_resolution(expr.hir_id.clone())
                .or_else(|| {
                    self.lowering
                        .typeck_method_resolution(callee.hir_id.clone())
                });
            if let Some(def_id) = resolved_method_def_id.as_ref() {
                generic_method_def = self.lowering.ensure_generic_method_def(def_id.clone());
            } else if let hir::Res::Def(def_id) = &path.res_ref() {
                // `ensure_generic_method_def` is the uniform lookup —
                // resolves a generic method (`Vec::from`, etc.) in this
                // package or any dependency's the same way, lazily
                // registering it on a miss instead of requiring it to
                // already be known (see `resolve_callee_path`'s matching
                // comment for the non-generic case).
                generic_method_def = self.lowering.ensure_generic_method_def(def_id.clone());
            }
        }

        let (mut func_operand, mut sig, callee_name) = if let Some(ref def_id) = generic_def_id {
            let function = self
                .lowering
                .mir_package
                .borrow()
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
                literal: mir::ConstantKind::FnDef(
                    def_id.clone(),
                    explicit_args
                        .iter()
                        .cloned()
                        .map(GenericArg::Type)
                        .collect(),
                ),
            });
            (operand, sig, Some(name))
        } else if let Some(def) = generic_method_def.as_ref() {
            let method_ctx = self
                .lowering
                .make_method_context(&def.self_ty, &def.assoc_types);
            let sig = self
                .lowering
                .lower_function_sig(&def.function.sig, method_ctx.as_ref());
            let fn_ty = self.lowering.function_pointer_ty(&sig);
            let name = def.method_name.clone();
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                ty: fn_ty.clone(),
                user_ty: None,
                literal: mir::ConstantKind::FnDef(
                    def.def_id.clone(),
                    explicit_args
                        .iter()
                        .cloned()
                        .map(GenericArg::Type)
                        .collect(),
                ),
            });
            (operand, sig, Some(name))
        } else {
            self.resolve_callee(expr.hir_id.clone(), callee)?
        };
        let mut associated_struct = match &callee.kind {
            hir::ExprKind::Path(path) => self
                .lowering
                .typeck_method_resolution(expr.hir_id.clone())
                .or_else(|| {
                    self.lowering
                        .typeck_method_resolution(callee.hir_id.clone())
                })
                .or_else(|| match path.res_ref().as_ref() {
                    Some(hir::Res::Def(def_id)) => Some(def_id.clone()),
                    _ => None,
                })
                .and_then(|def_id| self.lowering.ensure_method_info(def_id))
                .and_then(|info| info.struct_def),
            _ => None,
        };
        let callee_tail = if let hir::ExprKind::Path(path) = &callee.kind {
            path.segments().last().map(|seg| seg.ident.as_str())
        } else {
            None
        };
        let mut callee_abi = None;
        let mut callee_is_extern = false;
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some((abi, is_extern)) =
                path.path().and_then(|path| self.callee_abi_from_path(path))
            {
                callee_abi = Some(abi);
                callee_is_extern = is_extern;
            }
        }
        if callee_abi.is_none() {
            if let Some(name) = callee_name.as_ref() {
                for item in self.lowering.hir_all_items() {
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
                            if let hir::Res::Local(hir_id) = &path.res_ref() {
                                self.local_map.get(hir_id).copied()
                            } else {
                                path.segments()
                                    .first()
                                    .filter(|_| path.segments().len() == 1)
                                    .and_then(|seg| {
                                        self.fallback_locals.get(seg.ident.as_str()).copied()
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
            if let Some(function) = self.lowering.generic_function_def(&def_id) {
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
                            if path
                                .path()
                                .is_some_and(|path| self.lowering.is_result_path(path))
                            {
                                if let Some(args) = path.path().and_then(|path| {
                                    path.segments
                                        .last()
                                        .and_then(|segment| segment.explicit_args())
                                }) {
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
                            if path
                                .path()
                                .is_some_and(|path| self.lowering.is_result_path(path))
                            {
                                if let Some(args) = path.path().and_then(|path| {
                                    path.segments
                                        .last()
                                        .and_then(|segment| segment.explicit_args())
                                }) {
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
                            explicit_args[0] = HirToMirLowerer::unit_ty();
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
            associated_struct = info.struct_def;
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
            let struct_def = self.local_structs.get(&place.local).cloned();

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
                let struct_def = associated_struct
                    .clone()
                    .or_else(|| self.struct_def_from_ty(&result_ty));
                // Only the call's *own* result type is `result_ty` — if
                // `place` is a projection (e.g. `self.inner = f()`, a field
                // of a larger local), `place.local` names the *base* local
                // (`self`), not the destination value itself. Overwriting
                // `locals[place.local].ty`/`local_structs` here would
                // silently replace that base local's own declared type with
                // the call's unrelated result type.
                if place.projection.is_empty() && (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                    if let Some(ref def_id) = struct_def {
                        self.local_structs.insert(place.local, def_id.clone());
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
                    let struct_def =
                        associated_struct.or_else(|| self.struct_def_from_ty(&result_ty));
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(place.local, def_id);
                    }
                }
            }
        }

        Ok(place_info)
    }

    /// Lowers type-building declarations tagged as intrinsics. The resolved
    /// declaration chooses this path; wrapper names and module paths play no
    /// part in the decision.
    pub(super) fn lower_resolved_intrinsic_call(
        &mut self,
        expr: &hir::Expr,
        kind: IntrinsicKind,
        args: &[&hir::Expr],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<bool> {
        if !matches!(
            kind,
            IntrinsicKind::CreateStruct
                | IntrinsicKind::AddField
                | IntrinsicKind::CloneStruct
                | IntrinsicKind::BuildType
                | IntrinsicKind::PrimitiveType
        ) {
            return Ok(false);
        }

        let mut operands = Vec::with_capacity(args.len());
        for arg in args {
            if kind == IntrinsicKind::CloneStruct {
                if let hir::ExprKind::Path(path) = &arg.kind {
                    if let hir::Res::Def(def_id) = &path.res_ref() {
                        if let Some(Value::Type(value)) =
                            self.lowering.typeck_const_block_value(def_id.clone())
                        {
                            let ty = HirToMirLowerer::type_ty();
                            let local = self.allocate_temp(ty.clone(), arg.span);
                            let place = mir::Place::from_local(local);
                            self.push_statement(mir::Statement {
                                source_info: arg.span,
                                kind: mir::StatementKind::Assign(
                                    place.clone(),
                                    mir::Rvalue::TypeValue(value),
                                ),
                            });
                            operands.push(mir::Operand::copy(place));
                            continue;
                        }
                    }
                }
            }
            operands.push(self.lower_operand(arg, None)?.operand);
        }
        let (place, ty) = destination.unwrap_or_else(|| {
            let ty = self
                .lowering
                .typeck_expr_type(expr.hir_id.clone())
                .unwrap_or_else(HirToMirLowerer::type_ty);
            let local = self.allocate_temp(ty.clone(), expr.span);
            (mir::Place::from_local(local), ty)
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::IntrinsicCall {
                    kind,
                    format: String::new(),
                    args: operands,
                },
            ),
        });
        if (place.local as usize) < self.locals.len() {
            self.locals[place.local as usize].ty = ty;
        }
        Ok(true)
    }

    fn lower_resolved_print_call(
        &mut self,
        expr: &hir::Expr,
        kind: IntrinsicKind,
        args: &[hir::CallArg],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<()> {
        self.emit_printf_call(
            &hir::IntrinsicCallExpr {
                kind,
                callargs: args.to_vec(),
            },
            expr.span,
        )?;
        let (place, _) = destination.unwrap_or_else(|| {
            let local = self.allocate_temp(HirToMirLowerer::unit_ty(), expr.span);
            (mir::Place::from_local(local), HirToMirLowerer::unit_ty())
        });
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
            ),
        });
        if place.projection.is_empty() && (place.local as usize) < self.locals.len() {
            self.locals[place.local as usize].ty = HirToMirLowerer::unit_ty();
        }
        Ok(())
    }

    pub(super) fn param_names_for_callee(&mut self, path: &hir::Path) -> Option<Vec<hir::Symbol>> {
        match &path.res_ref() {
            hir::Res::Def(def_id) => self.param_names_for_def_id(def_id.clone()).or_else(|| {
                self.lowering
                    .ensure_generic_method_def(def_id.clone())
                    .and_then(|def| self.param_names_from_params(&def.function.sig.inputs))
            }),
            _ => None,
        }
    }

    pub(super) fn param_names_for_def_id(&self, def_id: hir::DefId) -> Option<Vec<hir::Symbol>> {
        let item = self.lowering.hir_item(def_id)?;
        match &item.kind {
            hir::ItemKind::Function(function) => self.param_names_from_params(&function.sig.inputs),
            _ => None,
        }
    }

    pub(super) fn param_names_from_params(
        &self,
        params: &[hir::Param],
    ) -> Option<Vec<hir::Symbol>> {
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
    pub(super) fn callee_abi_from_path(&self, path: &hir::Path) -> Option<(hir::Abi, bool)> {
        if let hir::Res::Def(def_id) = &path.res_ref() {
            if let Some(item) = self.lowering.hir_item(def_id.clone()) {
                if let hir::ItemKind::Function(func) = &item.kind {
                    return Some((func.sig.abi.clone(), func.is_extern));
                }
            }
        }
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);
        let qualified = resolved_path
            .segments()
            .iter()
            .map(|seg| seg.ident.as_str())
            .collect::<Vec<_>>()
            .join("::");
        if qualified.is_empty() {
            return None;
        }
        for item in self.lowering.hir_all_items() {
            if let hir::ItemKind::Function(func) = &item.kind {
                if func.sig.name.as_str() == qualified {
                    return Some((func.sig.abi.clone(), func.is_extern));
                }
            }
        }
        let tail = resolved_path.segments().last().map(|seg| seg.ident.as_str());
        if let Some(tail) = tail {
            let mut candidate: Option<(hir::Abi, bool)> = None;
            for item in self.lowering.hir_all_items() {
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

    pub(super) fn reorder_named_call_args(
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

    pub(super) fn resolve_callee(
        &mut self,
        call_hir_id: hir::HirId,
        callee: &hir::Expr,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        match &callee.kind {
            hir::ExprKind::Path(path) => path.path().map_or_else(
                || Err(fp_core::Error::from("type-relative callee unsupported")),
                |path| self.resolve_callee_path(call_hir_id, callee, path),
            ),
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

    pub(super) fn resolve_callee_path(
        &mut self,
        call_hir_id: hir::HirId,
        callee: &hir::Expr,
        path: &hir::Path,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);

        // Handle local variables (e.g., function parameters) as indirect calls
        if let hir::Res::Local(hir_id) = &resolved_path.res_ref() {
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

        // Type checking resolves type-relative associated functions (for
        // example `TypeBuilder::new`) to the concrete impl member.  Consume
        // that identity directly; MIR must not rediscover the member through
        // a `struct_methods["Type"]["method"]` name lookup.
        if let Some(method_def_id) = self
            .lowering
            .typeck_method_resolution(call_hir_id.clone())
            .or_else(|| {
                self.lowering
                    .typeck_method_resolution(callee.hir_id.clone())
            })
        {
            if let Some(info) = self.lowering.ensure_method_info(method_def_id.clone()) {
                self.lowering.ensure_method_lowered(method_def_id.clone())?;
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: info.fn_ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::FnDef(method_def_id, Vec::new()),
                });
                return Ok((operand, info.sig.clone(), Some(info.fn_name.clone())));
            }
            self.lowering.emit_error(
                callee.span,
                format!("resolved associated method `{method_def_id}` has no MIR signature"),
            );
        }

        if let hir::Res::Def(def_id) = &resolved_path.res_ref() {
            if let Some(info) = self.lowering.ensure_method_info(def_id.clone()) {
                self.lowering.ensure_method_lowered(def_id.clone())?;
                let literal = info
                    .def_id
                    .clone()
                    .map(|method_def_id| mir::ConstantKind::FnDef(method_def_id, Vec::new()))
                    .ok_or_else(|| {
                        crate::error::optimization_error(format!(
                            "method definition `{def_id}` has no resolved identity"
                        ))
                    })?;
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    ty: info.fn_ty.clone(),
                    user_ty: None,
                    literal,
                });
                return Ok((operand, info.sig.clone(), Some(info.fn_name.clone())));
            }
            if self
                .lowering
                .mir_package
                .borrow()
                .function_sigs
                .get(def_id)
                .cloned()
                .is_none()
            {
                // Not yet lowered/registered — e.g. a same-package function
                // this MIR-lowering pass hasn't proactively reached yet
                // (out-of-order cross-module reference), or one deliberately
                // never visited at all (the comptime-probe's item-scoped
                // entry point, `transform_comptime_request`, never walks an
                // unrelated item's body). Lower it on demand now, mirroring
                // `ensure_item_lowered`/`try_lazily_register_adt`'s
                // existing lazy pattern for the same reason.
                self.lowering.ensure_function_lowered(def_id.clone())?;
            }
            if let Some(sig) = self
                .lowering
                .mir_package
                .borrow()
                .function_sigs
                .get(def_id)
                .cloned()
            {
                let name = self
                    .lowering
                    .hir_item(def_id.clone())
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
                    literal: mir::ConstantKind::FnDef(def_id.clone(), Vec::new()),
                });
                return Ok((operand, sig, Some(String::from(name))));
            }
        }

        let path = resolved_path
            .segments()
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>()
            .join("::");
        Err(crate::error::optimization_error(format!(
            "unresolved call target `{path}` with resolution {:?}: no HIR-resolved definition",
            resolved_path.res_ref()
        )))
    }

    pub(super) fn lower_operand(
        &mut self,
        expr: &hir::Expr,
        expected: Option<&Ty>,
    ) -> Result<OperandInfo> {
        let inferred_expected = if expected.is_none() {
            self.lowering.typeck_expr_type(expr.hir_id.clone())
        } else {
            None
        };
        let expected = expected.or(inferred_expected.as_ref());
        if self.active_exprs.contains(&expr.hir_id) {
            let message = "recursive expression detected during MIR lowering";
            self.lowering.emit_error(expr.span, message);
            return Err(fp_core::error::Error::from(message));
        }
        self.active_exprs.insert(expr.hir_id.clone());
        let _guard = ExprRecursionGuard::new(&mut self.active_exprs, expr.hir_id.clone());
        if matches!(expr.kind, hir::ExprKind::FieldAccess(_, _)) {
            // A reflection field (for example `type(T).name`) computes its
            // own concrete type. It must not inherit the enclosing
            // expression's expectation: `println(...)` itself has unit type.
            if let Some(constant) = self.lowering.lower_const_expr(expr, None, None) {
                let ty = self
                    .constant_ty_from_constant(&constant)
                    .unwrap_or_else(|| self.lowering.error_ty());
                return Ok(OperandInfo {
                    operand: mir::Operand::Constant(constant),
                    ty,
                });
            }

            if let Some(place) = self.lower_place(expr)? {
                return Ok(OperandInfo {
                    operand: mir::Operand::copy(place.place),
                    ty: place.ty,
                });
            }

            let hir::ExprKind::FieldAccess(base, field) = &expr.kind else {
                unreachable!();
            };
            // A field access is still a value expression when its receiver
            // is a const or another computed expression. Materialize that
            // receiver once, then use the ordinary typed place projection.
            let mut base = self.materialize_expr_place(base)?;
            let mut base_ty = base.ty.clone();
            while let TyKind::Ref(_, inner, _) | TyKind::RawPtr(TypeAndMut { ty: inner, .. }) =
                &base_ty.kind
            {
                base.place.projection.push(mir::PlaceElem::Deref);
                base_ty = inner.as_ref().clone();
            }
            let Some(struct_def) = base
                .struct_def
                .clone()
                .or_else(|| self.struct_def_from_ty(&base_ty))
            else {
                let message = format!("field access `{field}` has a non-struct receiver");
                self.lowering.emit_error(expr.span, &message);
                return Err(fp_core::error::Error::from(message));
            };
            let Some((field_index, field_info)) =
                self.lowering
                    .struct_field(struct_def, &base_ty, field.as_str(), expr.span)
            else {
                let message = format!("unknown field `{field}` on field-access receiver");
                self.lowering.emit_error(expr.span, &message);
                return Err(fp_core::error::Error::from(message));
            };
            base.place
                .projection
                .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));
            return Ok(OperandInfo {
                operand: mir::Operand::copy(base.place),
                ty: field_info.ty,
            });
        }
        // A method call is a value expression, even when its receiver is a
        // place. Do not let `lower_place` project through the receiver and
        // discard the resolved method call (notably in chained comptime
        // calls such as `TypeBuilder::new(..).build()`). Materializing it
        // routes it through `lower_expr_into_place`, which dispatches by
        // typeck's recorded method DefId.
        if matches!(expr.kind, hir::ExprKind::MethodCall(_, _, _, _)) {
            let ty = expected
                .cloned()
                .or_else(|| self.lowering.typeck_expr_type(expr.hir_id.clone()))
                .ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "missing HIR type for method call {}",
                        expr.hir_id
                    ))
                })?;
            let local_id = self.allocate_temp(ty.clone(), expr.span);
            let place = mir::Place::from_local(local_id);
            self.lower_expr_into_place(expr, place.clone(), &ty)?;
            return Ok(OperandInfo {
                operand: mir::Operand::copy(place),
                ty,
            });
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
                if let hir::QPath::Resolved(_, inner) = &mut resolved_path {
                    self.resolve_self_path(inner);
                }
                let explicit_args = resolved_path
                    .segments()
                    .last()
                    .and_then(|segment| segment.explicit_args())
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
                if let hir::Res::Def(def_id) = &resolved_path.res_ref() {
                    // A type alias can be named in value position only for
                    // a comptime intrinsic argument. Its target's completed
                    // const-block result is the authoritative type handle;
                    // do not lower the alias again or recover it by name.
                    let direct_type_value = self.lowering.typeck_const_block_value(def_id.clone());
                    if let Some(Value::Type(value)) = direct_type_value {
                        let ty = HirToMirLowerer::type_ty();
                        let local_id = self.allocate_temp(ty.clone(), expr.span);
                        let local_place = mir::Place::from_local(local_id);
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                local_place.clone(),
                                mir::Rvalue::TypeValue(value),
                            ),
                        });
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(local_place),
                            ty,
                        });
                    }
                    if resolved_path.segments().len() == 1
                        && let Some(Value::Type(value)) =
                            self.lowering.type_value_for_def(def_id.clone())
                    {
                        let ty = HirToMirLowerer::type_ty();
                        let local_id = self.allocate_temp(ty.clone(), expr.span);
                        let local_place = mir::Place::from_local(local_id);
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                local_place.clone(),
                                mir::Rvalue::TypeValue(value),
                            ),
                        });
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(local_place),
                            ty,
                        });
                    }
                    if has_explicit_args {
                        if let Some(function) = self.lowering.generic_function_def(def_id) {
                            let info = self
                                .lowering
                                .ensure_function_specialization_from_explicit_args(
                                    def_id.clone(),
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
                        if let Some(function) = self.lowering.generic_function_def(def_id) {
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
                                        literal: mir::ConstantKind::FnDef(
                                            def_id.clone(),
                                            Vec::new(),
                                        ),
                                    }),
                                    ty: fn_ty,
                                });
                            }
                            let info = self.lowering.ensure_function_specialization(
                                def_id.clone(),
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
                                    literal: mir::ConstantKind::FnDef(
                                        info.def_id.clone(),
                                        info.substs.clone(),
                                    ),
                                }),
                                ty: info.fn_ty,
                            });
                        }
                    }
                    if let Some(konst) = self.const_items.get(def_id).cloned() {
                        let ty = self.lower_type_expr(&konst.ty);
                        if let Some(constant) =
                            self.lowering
                                .lower_const_expr(&konst.body.value, Some(&ty), None)
                        {
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(constant),
                                ty,
                            });
                        }
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
                    if let Some(const_info) = self.lowering.ensure_const_info(def_id.clone()) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(const_info.typed_value()),
                            ty: const_info.ty,
                        });
                    }
                    if let Some((name, ty)) = self
                        .lowering
                        .mir_package
                        .borrow()
                        .executable_consts
                        .get(def_id)
                        .cloned()
                    {
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
                    let const_def_item =
                        self.lowering
                            .hir_item(def_id.clone())
                            .and_then(|item| match &item.kind {
                                hir::ItemKind::Const(konst) => Some(konst.clone()),
                                _ => None,
                            });
                    if let Some(konst) = const_def_item {
                        let const_ty = self.lower_type_expr(&konst.ty);
                        if let Some(constant) =
                            self.lowering
                                .lower_const_expr(&konst.body.value, Some(&const_ty), None)
                        {
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(constant),
                                ty: const_ty,
                            });
                        }
                        self.lowering.ensure_item_lowered(def_id.clone())?;
                        if let Some(const_info) = self.lowering.ensure_const_info(def_id.clone()) {
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(const_info.typed_value()),
                                ty: const_info.ty.clone(),
                            });
                        }
                        // `ensure_item_lowered`'s non-foldable fallback
                        // (a call-shaped initializer) registers this
                        // const's real global via `executable_consts`,
                        // not `const_values` — check that too before
                        // giving up and inlining the body as ordinary
                        // code, which would silently bypass the real
                        // global this const's own top-level declaration
                        // needs to exist as.
                        if let Some((name, ty)) = self
                            .lowering
                            .mir_package
                            .borrow()
                            .executable_consts
                            .get(def_id)
                            .cloned()
                        {
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
                    if let Some(variant) = self.lowering.enum_variant_def(def_id) {
                        let mut layout = expected.and_then(|ty| {
                            self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                        });
                        if layout.is_none() {
                            let args = resolved_path
                                .path()
                                .and_then(|path| {
                                    path.segments
                                        .iter()
                                        .find_map(|segment| segment.explicit_args())
                                })
                                .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                                .unwrap_or_default();
                            if !args.is_empty() {
                                layout = self.lowering.enum_layout_for_instance(
                                    variant.enum_def.clone(),
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
                                    .enum_layout_for_def(variant.enum_def.clone(), expr.span);
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
                    let referenced_fn_sig =
                        self.lowering
                            .hir_item(def_id.clone())
                            .and_then(|item| match &item.kind {
                                hir::ItemKind::Function(func) => Some(func.sig.clone()),
                                _ => None,
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
                                literal: mir::ConstantKind::FnDef(def_id.clone(), Vec::new()),
                            }),
                            ty: fn_ty,
                        });
                    }
                }

                if resolved_path.res_ref().is_none() {
                    if let Some(variant) = resolved_path
                        .path()
                        .and_then(|path| self.enum_variant_info_from_path(path))
                    {
                        let mut layout = expected.and_then(|ty| {
                            self.enum_layout_for_variant(&variant, Some(ty), expr.span)
                        });
                        if layout.is_none() {
                            let args = resolved_path
                                .path()
                                .and_then(|path| {
                                    path.segments
                                        .iter()
                                        .find_map(|segment| segment.explicit_args())
                                })
                                .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                                .unwrap_or_default();
                            if !args.is_empty() {
                                layout = self.lowering.enum_layout_for_instance(
                                    variant.enum_def.clone(),
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
                                    .enum_layout_for_def(variant.enum_def.clone(), expr.span);
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
                    let method_def = self
                        .lowering
                        .typeck_method_resolution(expr.hir_id.clone())
                        .or_else(|| match resolved_path.res_ref().as_ref() {
                            Some(hir::Res::Def(def_id)) => Some(def_id.clone()),
                            _ => None,
                        })
                        .and_then(|def_id| self.lowering.ensure_generic_method_def(def_id));
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
                    .segments()
                    .iter()
                    .map(|seg| seg.ident.as_str())
                    .collect::<Vec<_>>()
                    .join("::");
                // Type names used as values (i64, bool, str, etc.) —
                // materialize the corresponding comptime type handle. This
                // is the value-side counterpart of the `type` syntax, used
                // by concrete std wrappers such as `TypeBuilder::with_field`.
                if is_known_type_name(&name) {
                    let ty = HirToMirLowerer::type_ty();
                    let local_id = self.allocate_temp(ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let name_ty = self.lowering.string_slice_ty();
                    return Ok(OperandInfo {
                        operand: {
                            self.push_statement(mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::IntrinsicCall {
                                        kind: IntrinsicKind::PrimitiveType,
                                        format: String::new(),
                                        args: vec![mir::Operand::Constant(mir::Constant {
                                            span: expr.span,
                                            ty: name_ty,
                                            user_ty: None,
                                            literal: mir::ConstantKind::Str(name),
                                        })],
                                    },
                                ),
                            });
                            mir::Operand::copy(local_place)
                        },
                        ty,
                    });
                }
                Err(fp_core::error::Error::from(format!(
                    "unresolved value path during MIR lowering: `{name}` (resolution: {:?})",
                    resolved_path.res_ref()
                )))
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = if matches!(ty_expr.kind, hir::TypeExprKind::Infer) {
                    self.lowering
                        .typeck_expr_type(expr.hir_id.clone())
                        .unwrap_or_else(|| self.lowering.error_ty())
                } else {
                    self.lower_type_expr(ty_expr)
                };
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
                    if let hir::Res::Def(def_id) = &path.res_ref() {
                        if let Some(const_info) = self.lowering.ensure_const_info(def_id.clone()) {
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
                            if let Some(constant) =
                                self.lowering
                                    .lower_const_expr(&konst.body.value, Some(&ty), None)
                            {
                                if let Some((constant, ty)) =
                                    self.lowering.const_index_value(expr.span, &constant, index)
                                {
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
                /*
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
                */
                /*
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
                                        mir::ConstValue::Array(elements) => {
                                            if let Some(elem_ty) =
                                                self.expect_array_element_ty(&base_info.ty)
                                            {
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
                */
                let mut structural_ty = base_info.ty.clone();
                while let TyKind::Ref(_, inner, _) = &structural_ty.kind {
                    structural_ty = inner.as_ref().clone();
                }
                if matches!(structural_ty.kind, TyKind::Array(_, _) | TyKind::Slice(_)) {
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let index_operand = self.lower_operand(index, Some(&index_ty))?;
                    let mut place_info = match self.lower_place(base)? {
                        Some(place) => place,
                        None => self.materialize_expr_place(base)?,
                    };
                    let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                    self.push_statement(mir::Statement {
                        source_info: index.span,
                        kind: mir::StatementKind::Assign(
                            mir::Place::from_local(index_local),
                            mir::Rvalue::Use(index_operand.operand),
                        ),
                    });
                    place_info
                        .place
                        .projection
                        .push(mir::PlaceElem::Index(index_local));
                    place_info.ty = self
                        .expect_array_element_ty(&structural_ty)
                        .unwrap_or_else(|| self.lowering.error_ty());
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(place_info.place),
                        ty: place_info.ty,
                    });
                }
                let Some(method_def_id) =
                    self.lowering.typeck_method_resolution(expr.hir_id.clone())
                else {
                    self.lowering.emit_error(
                        expr.span,
                        "index expression has no resolved std Index implementation",
                    );
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(self.lowering.error_constant(expr.span)),
                        ty: expected
                            .cloned()
                            .unwrap_or_else(|| self.lowering.error_ty()),
                    });
                };
                let element_ty = expected
                    .cloned()
                    .or_else(|| self.lowering.typeck_expr_type(expr.hir_id.clone()))
                    .unwrap_or_else(|| self.lowering.error_ty());
                let local_id = self.allocate_temp(element_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                let generic_args = self
                    .lowering
                    .typeck_generic_method_arg(expr.hir_id.clone())
                    .unwrap_or_default();
                let result_ty = self.call_method_def_into_place(
                    method_def_id,
                    generic_args,
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
            hir::ExprKind::IntrinsicCall(call) => {
                // Portable `#[op(...)]` calls with no low-level intrinsic
                // equivalent (`CallKind::Op` variants that don't map via
                // `intrinsic_kind()`) haven't been normalized/materialized
                // away by the time MIR lowering runs -- fail loudly here
                // rather than silently mis-lowering them, matching the
                // "unsupported intrinsic" fallback already used below for
                // intrinsics this function doesn't otherwise handle.
                let kind = call.kind;
                if matches!(kind, IntrinsicKind::Print | IntrinsicKind::Println) {
                    self.emit_printf_call(call, expr.span)?;
                    let unit_ty = HirToMirLowerer::unit_ty();
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
                    let unit_ty = HirToMirLowerer::unit_ty();
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
                    let unit_ty = HirToMirLowerer::unit_ty();
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

                    if let Some(constant) = self.lowering.lower_const_expr(arg, None, None) {
                        if let Some(len) = self.lowering.const_len_from_constant(&constant) {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    ty: len_ty.clone(),
                                    user_ty: None,
                                    literal: mir::ConstantKind::UInt(len),
                                }),
                                ty: len_ty,
                            });
                        }
                    }

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
                                    format!("{:?} intrinsic expects at least one argument", kind),
                                );
                                let unit_ty = HirToMirLowerer::unit_ty();
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
                                let unit_ty = HirToMirLowerer::unit_ty();
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
                        | IntrinsicKind::CloneStruct
                        | IntrinsicKind::BuildType
                        | IntrinsicKind::PrimitiveType
                ) {
                    let lowered_args: Vec<OperandInfo> = call
                        .callargs
                        .iter()
                        .map(|arg| self.lower_operand(&arg.value, None))
                        .collect::<Result<Vec<_>>>()?;
                    let operands: Vec<mir::Operand> =
                        lowered_args.iter().map(|a| a.operand.clone()).collect();
                    let ty = HirToMirLowerer::type_ty();
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
                    let ty = HirToMirLowerer::unit_ty();
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
                // once this package's own MIR/LIR exists — lower it as a
                // pending-comptime global now (the same on-demand path
                // any other item goes through) so `evaluate_comptime_lir`
                // can run it for real through the actual interpreter,
                // resolving arbitrary code (method calls, etc.), not a
                // hand-rolled subset-of-Rust evaluator. Best-effort: a
                // failure here just falls back to the ordinary runtime
                // lowering below, same as before.
                if let Err(error) = self
                    .lowering
                    .ensure_const_block_lowered(const_block.def_id.clone())
                {
                    self.lowering.emit_warning(
                        expr.span,
                        format!(
                            "const block {:?} failed to lower for comptime validation: {error}",
                            const_block.def_id
                        ),
                    );
                }
                // The value was resolved eagerly during type checking (see
                // `HirTypeChecker::check_expr`'s `ConstBlock` arm) and handed
                // here keyed by this block's own `def_id` — no synthetic
                // item, no string key.
                if let Some(value) = self
                    .lowering
                    .typeck_const_block_value(const_block.def_id.clone())
                {
                    if let Some(constant) = self
                        .lowering
                        .const_block_value_to_mir_constant(&value, expr.span)
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

    pub(super) fn lower_slice_operand(
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

    pub(super) fn lower_reference_operand(
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

    pub(super) fn constant_bool_operand(&self, value: bool, span: Span) -> OperandInfo {
        OperandInfo::constant(
            span,
            Ty { kind: TyKind::Bool },
            mir::ConstantKind::Bool(value),
        )
    }

    pub(super) fn constant_ty_from_constant(&self, constant: &mir::Constant) -> Option<Ty> {
        Some(constant.ty.clone())
    }

    pub(super) fn lower_condition_operand(&mut self, expr: &hir::Expr) -> Result<mir::Operand> {
        let bool_ty = Ty { kind: TyKind::Bool };
        let local_id = self.allocate_temp(bool_ty, expr.span);
        let place = mir::Place::from_local(local_id);
        self.lower_expr_into_place(expr, place.clone(), &Ty { kind: TyKind::Bool })?;
        Ok(mir::Operand::copy(place))
    }

    pub(super) fn allocate_temp(&mut self, ty: Ty, span: Span) -> mir::LocalId {
        let mut decl = self.lowering.make_local_decl(&ty, span);
        decl.mutability = mir::Mutability::Mut;
        self.push_local(decl)
    }

    pub(super) fn set_current_terminator(&mut self, terminator: mir::Terminator) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.terminator = Some(terminator);
        }
    }

    pub(super) fn convert_bin_op(op: &hir::BinOp) -> mir::BinOp {
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

    pub(super) fn convert_un_op(op: &hir::UnOp) -> Option<mir::UnOp> {
        match op {
            hir::UnOp::Not => Some(mir::UnOp::Not),
            hir::UnOp::Neg => Some(mir::UnOp::Neg),
            hir::UnOp::Deref | hir::UnOp::Box => None,
        }
    }

    pub(super) fn binary_result_ty(op: &hir::BinOp, lhs_ty: &Ty) -> Ty {
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

    pub(super) fn expect_array_element_ty(&self, ty: &Ty) -> Option<Ty> {
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

    pub(super) fn container_type_name(&self, ty: &Ty) -> Option<String> {
        let mut ty = ty;
        while let TyKind::Ref(_, inner, _) = &ty.kind {
            ty = inner.as_ref();
        }
        if let TyKind::Adt(adt, _) = &ty.kind {
            if let Some(definition) = self.lowering.struct_def(&adt.did) {
                return Some(definition.name);
            }
        }
        self.lowering.display_type_name(ty)
    }

    fn container_name_tail(name: &str) -> &str {
        name.rsplit("::")
            .next()
            .unwrap_or(name)
            .split('<')
            .next()
            .unwrap_or(name)
    }

    pub(super) fn is_list_container(&self, ty: &Ty) -> bool {
        if matches!(ty.kind, TyKind::Slice(_)) {
            return true;
        }
        self.container_type_name(ty)
            .map(|name| matches!(Self::container_name_tail(&name), "Vec" | "List" | "list"))
            .unwrap_or(false)
    }

    pub(super) fn is_map_container(&self, ty: &Ty) -> bool {
        self.container_type_name(ty)
            .map(|name| Self::container_name_tail(&name) == "HashMap")
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
    pub(super) fn real_indexable_struct_def_id(&self, ty: &Ty) -> Option<hir::DefId> {
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
            .mir_package
            .borrow()
            .method_defs_by_self_and_name
            .contains_key(&(adt.did.clone(), "index".to_string()));
        has_index_method.then_some(adt.did.clone())
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
    pub(super) fn call_real_method_into_place(
        &mut self,
        struct_def_id: hir::DefId,
        method_name: &str,
        receiver: &hir::Expr,
        extra_args: &[&hir::Expr],
        place: mir::Place,
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<Ty> {
        let method_def_id = self
            .lowering
            .mir_package
            .borrow()
            .method_defs_by_self_and_name
            .get(&(struct_def_id.clone(), method_name.to_string()))
            .cloned()
            .ok_or_else(|| {
                crate::error::optimization_error(format!(
                    "no method `{}` found on struct {:?}",
                    method_name, struct_def_id
                ))
            })?;
        self.call_method_def_into_place(
            method_def_id,
            Vec::new(),
            receiver,
            extra_args,
            place,
            expected_return,
            span,
        )
    }

    pub(super) fn call_method_def_into_place(
        &mut self,
        method_def_id: hir::DefId,
        generic_args: Vec<Ty>,
        receiver: &hir::Expr,
        extra_args: &[&hir::Expr],
        place: mir::Place,
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<Ty> {
        let def = self
            .lowering
            .ensure_generic_method_def(method_def_id.clone())
            .ok_or_else(|| {
                crate::error::optimization_error(format!(
                    "no method definition for {method_def_id:?}"
                ))
            })?;
        let method_ctx = self
            .lowering
            .make_method_context(&def.self_ty, &def.assoc_types);
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
            &generic_args,
            &arg_types,
            expected_return,
            span,
        )?;
        let func_operand = mir::Operand::Constant(mir::Constant {
            span,
            ty: info.fn_ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::FnDef(def.def_id.clone(), info.substs.clone()),
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

    pub(super) fn local_id_from_expr(&self, expr: &hir::Expr) -> Option<mir::LocalId> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        if let hir::Res::Local(hir_id) = &path.res_ref() {
            return self.local_map.get(hir_id).copied();
        }
        path.segments()
            .first()
            .filter(|_| path.segments().len() == 1)
            .and_then(|seg| self.fallback_locals.get(seg.ident.as_str()).copied())
    }

    pub(super) fn evaluate_array_length(&mut self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Cast(inner, _) => self.evaluate_array_length(inner),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => block
                .expr
                .as_deref()
                .and_then(|inner| self.evaluate_array_length(inner)),
            hir::ExprKind::Path(path) => {
                if let hir::Res::Def(def_id) = path.res_ref() {
                    if let Some(const_info) = self.lowering.ensure_const_info(def_id.clone()) {
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

    pub(super) fn new_block(&mut self) -> mir::BasicBlockId {
        let id = self.blocks.len() as mir::BasicBlockId;
        self.blocks.push(mir::BasicBlockData::new(None));
        id
    }

    pub(super) fn push_statement(&mut self, statement: mir::Statement) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.statements.push(statement);
        }
    }
}
