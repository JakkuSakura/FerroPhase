use super::body::BodyBuilder;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{GenericArg, Ty, TyKind};
use fp_core::mir::{EnumLayout, EnumVariantInfo, MethodDefinition, StructDefinition, StructLayout};
use fp_core::span::Span;
use std::collections::HashMap;

impl<'a> BodyBuilder<'a> {
    pub(super) fn enum_variant_info_from_qpath(&self, path: &hir::QPath) -> Option<EnumVariantInfo> {
        match path {
            hir::QPath::Resolved(_, path) => self.enum_variant_info_from_path(path),
            hir::QPath::TypeRelative(_, segment) => {
                if let hir::Res::Def(def_id) = &segment.res {
                    if let Some(info) = self
                        .lowering
                        .mir_package
                        .borrow()
                        .enum_variants
                        .get(def_id)
                        .cloned()
                    {
                        return Some(info);
                    }
                }
                self.lowering
                    .mir_package
                    .borrow()
                    .enum_variant_names
                    .get(segment.ident.as_str())
                    .cloned()
                    .and_then(|def_id| self.lowering.enum_variant_def(&def_id))
            }
        }
    }

    pub(super) fn enum_variant_info_from_path(&self, path: &hir::Path) -> Option<EnumVariantInfo> {
        if let hir::Res::Def(def_id) = &path.res_ref() {
            if let Some(info) = self
                .lowering
                .mir_package
                .borrow()
                .enum_variants
                .get(def_id)
                .cloned()
            {
                return Some(info.clone());
            }
            if self
                .lowering
                .mir_package
                .borrow()
                .generic_function_defs
                .contains_key(def_id)
            {
                return None;
            }
        }
        if matches!(path.res_ref(), hir::Res::Local(_) | hir::Res::SelfTy) {
            return None;
        }

        let name = path
            .segments()
            .iter()
            .map(|seg| seg.ident.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.lowering
            .mir_package
            .borrow()
            .enum_variant_names
            .get(&name)
            .cloned()
            .or_else(|| {
                path.segments().last().and_then(|seg| {
                    self.lowering
                        .mir_package
                        .borrow()
                        .enum_variant_names
                        .get(seg.ident.as_str())
                        .cloned()
                })
            })
            .and_then(|def_id| self.lowering.enum_variant_def(&def_id))
    }

    pub(super) fn enum_variant_info_from_expected(
        &self,
        path: &hir::Path,
        expected_ty: Option<&Ty>,
    ) -> Option<EnumVariantInfo> {
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty?);

        let name = path
            .segments()
            .iter()
            .map(|seg| seg.ident.as_str())
            .collect::<Vec<_>>()
            .join("::");
        let def_id = self
            .lowering
            .mir_package
            .borrow()
            .enum_variant_names
            .get(&name)
            .cloned()
            .or_else(|| {
                path.segments().last().and_then(|seg| {
                    self.lowering
                        .mir_package
                        .borrow()
                        .enum_variant_names
                        .get(seg.ident.as_str())
                        .cloned()
                })
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
                            if expected_contains_enum(enum_def.clone(), inner) {
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
                            if expected_contains_enum(enum_def.clone(), inner) {
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
            if let Some(info) = self
                .lowering
                .mir_package
                .borrow()
                .enum_variants
                .get(&def_id)
                .cloned()
            {
                if expected_contains_enum(info.enum_def.clone(), expected_ty) {
                    return Some(info);
                }
            }
        }
        let tail = path.segments().last()?.ident.as_str();

        self.enum_variant_from_expected_ty_by_name(expected_ty, tail)
    }

    pub(super) fn enum_variant_from_enum_def(
        &self,
        enum_def: hir::DefId,
        variant_name: &str,
    ) -> Option<EnumVariantInfo> {
        let def = self
            .lowering
            .mir_package
            .borrow()
            .enum_defs
            .get(&enum_def)
            .cloned()?;
        let variant = def
            .variants
            .iter()
            .find(|variant| variant.name == variant_name)?;
        self.lowering
            .mir_package
            .borrow()
            .enum_variants
            .get(&variant.def_id)
            .cloned()
    }

    pub(super) fn enum_variant_from_expected_ty_by_name(
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
                if let Some(info) = self.enum_variant_from_enum_def(adt.did.clone(), variant_name) {
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
                if let Some(info) = self.enum_variant_from_enum_def(def_id.clone(), variant_name) {
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

    pub(super) fn result_variant_from_expected(
        &self,
        expected_ty: &Ty,
        variant_name: &str,
    ) -> Option<EnumVariantInfo> {
        if variant_name != "Ok" && variant_name != "Err" {
            return None;
        }
        let expected_ty = self.lowering.unwrap_expr_actual_ty(expected_ty);

        fn find_result_def(lowering: &HirToMirLowerer, ty: &Ty) -> Option<hir::DefId> {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => find_result_def(lowering, inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => find_result_def(lowering, type_and_mut.ty.as_ref()),
                TyKind::Adt(adt, substs) => {
                    let is_result = lowering
                        .mir_package
                        .borrow()
                        .enum_defs
                        .get(&adt.did)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result {
                        return Some(adt.did.clone());
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
                        .mir_package
                        .borrow()
                        .enum_defs
                        .get(def_id)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result {
                        return Some(def_id.clone());
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
                    lowering
                        .mir_package
                        .borrow()
                        .enum_defs
                        .get(&layout.def_id)
                        .cloned()
                        .and_then(|def| {
                            let is_result = def.name.as_str() == "Result"
                                || def.name.as_str().ends_with("::Result");
                            is_result.then_some(layout.def_id.clone())
                        })
                }),
            }
        }

        let result_def = find_result_def(&self.lowering, expected_ty)?;
        self.enum_variant_from_enum_def(result_def, variant_name)
    }

    pub(super) fn explicit_args_from_expected_result_ty(
        &self,
        expected_ty: &Ty,
    ) -> Option<Vec<Ty>> {
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
            .lowering
            .mir_package
            .borrow()
            .enum_defs
            .get(adt)
            .map(|def| def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result"))
            .or_else(|| {
                self.lowering
                    .mir_package
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
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .lowering
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
                        if let Some(def) = self
                            .lowering
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
            if let Some(layout) = self.lowering.enum_layout_for_ty(expected_ty) {
                let is_result_layout = self
                    .lowering
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

    pub(super) fn enum_variant_for_payload(
        &mut self,
        expected_ty: &Ty,
        payload_ty: &Ty,
        payload_def: Option<hir::DefId>,
    ) -> Option<(EnumVariantInfo, EnumLayout)> {
        let layout = self.enum_layout_for_ty(expected_ty, self.span)?;
        let enum_def = self.enum_def_from_ty(expected_ty);
        for (def_id, payloads) in &layout.variant_payloads {
            let matches = if payloads.is_empty() {
                HirToMirLowerer::is_unit_ty(payload_ty)
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
                if let Some(info) = self
                    .lowering
                    .mir_package
                    .borrow()
                    .enum_variants
                    .get(def_id)
                    .cloned()
                {
                    return Some((info.clone(), layout));
                }
            }
        }
        let payload_struct_def = payload_def.or_else(|| self.struct_def_from_ty(payload_ty));
        if let (Some(enum_def), Some(payload_struct_def)) = (enum_def, payload_struct_def) {
            if let Some(info) = self
                .lowering
                .mir_package
                .borrow()
                .enum_variants
                .values()
                .find(|info| {
                    info.enum_def == enum_def
                        && info.payload_def == Some(payload_struct_def.clone())
                })
            {
                return Some((info.clone(), layout));
            }
        }
        None
    }

    pub(super) fn assign_enum_variant(
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

        let rvalue = if layout.payload_tys.is_empty() {
            // A payload-free enum is represented by its tag scalar.  Do not
            // wrap the tag in a one-element tuple: that creates a value whose
            // MIR shape disagrees with `layout.enum_ty` and loses explicit
            // discriminants at later ABI lowering stages.
            mir::Rvalue::Use(operands.into_iter().next().expect("enum tag operand"))
        } else {
            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands)
        };
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(place, rvalue),
        });
        Ok(())
    }

    pub(super) fn enum_variant_payloads_for_layout(
        &mut self,
        layout: &EnumLayout,
        variant: &EnumVariantInfo,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Vec<Ty> {
        self.variant_payloads_from_layout_or_ty(layout, variant, scrutinee_ty, span)
    }

    pub(super) fn assign_enum_variant_from_place(
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
                let source_layout = self
                    .lowering
                    .struct_layout_for_ty(&source_ty)
                    .or_else(|| {
                        if let TyKind::Adt(adt, substs) = &source_ty.kind {
                            let args = substs
                                .iter()
                                .filter_map(|arg| match arg {
                                    mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            self.lowering
                                .struct_layout_for_instance(adt.did.clone(), &args, span)
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| {
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

    pub(super) fn lower_enum_variant_value(
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

    pub(super) fn lower_struct_literal(
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
            .segments()
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
                && self.enum_def_from_ty(expected_ty) == Some(variant.enum_def.clone())
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
            if let Some(layout) = context_layout.or_else(|| {
                self.lowering
                    .enum_layout_for_def(variant.enum_def.clone(), span)
            }) {
                if self.enum_def_from_ty(expected_ty) == Some(layout.def_id.clone()) {
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
            if let Some(info) = self.lowering.struct_def(&def_id) {
                if generic_args.is_empty() && !info.generics.is_empty() {
                    // No explicit turbofish — read `fp-typing`'s own
                    // already-resolved generic args for this literal
                    // (`typeck_expr_type`), composed with this specialization's
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
                        def_id.clone(),
                        &self.type_substs,
                    ) {
                        generic_args = cached;
                    }
                }
                if let Some(layout) =
                    self.lowering
                        .struct_layout_for_instance(def_id.clone(), &generic_args, span)
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

            if let Some(variant) = self.lowering.enum_variant_def(&def_id) {
                let layout = annotated_ty
                    .and_then(|ty| self.enum_layout_for_ty(ty, span))
                    .or_else(|| {
                        self.lowering
                            .enum_layout_for_def(variant.enum_def.clone(), span)
                    });
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

            if let Some(const_info) = self.lowering.ensure_const_info(def_id.clone()) {
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
                .or_else(|| {
                    self.lowering
                        .enum_layout_for_def(variant.enum_def.clone(), span)
                });
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
                if let Some(info) = self.lowering.struct_def(&def_id) {
                    if let Some(layout) =
                        self.lowering.struct_layout_for_ty(expected_ty).or_else(|| {
                            self.lowering
                                .struct_layout_for_instance(def_id.clone(), &[], span)
                        })
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

    pub(super) fn assign_enum_variant_from_struct_fields(
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
            return self.assign_enum_variant(place, variant, layout, scrutinee_ty, &[], span);
        }
        if payload_tys.len() != 1 && payload_tys.len() != fields.len() {
            return Err(fp_core::error::Error::from(format!(
                "struct-like enum payload shape does not match its ABI layout (payloads={}, fields={}, slots={})",
                payload_tys.len(),
                fields.len(),
                layout.payload_tys.len()
            )));
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
                .clone()
                .or_else(|| self.struct_def_from_ty(&payload_ty))
                .ok_or_else(|| {
                    fp_core::error::Error::from(
                        "struct-like enum payload definition is unavailable",
                    )
                })?;
            let payload_info = self
                .lowering
                .mir_package
                .borrow()
                .struct_defs
                .get(&payload_def)
                .cloned()
                .ok_or_else(|| {
                    fp_core::error::Error::from("struct-like enum payload fields are unavailable")
                })?;
            // Same flattening concern as `payload_def` above: look the
            // layout up by the original struct's DefId first, since
            // `payload_ty` may no longer be the struct's own Adt type.
            let payload_layout = self
                .lowering
                .struct_layout_for_ty(&payload_ty)
                .or_else(|| {
                    self.lowering
                        .struct_layout_for_instance(payload_def.clone(), &[], span)
                })
                .ok_or_else(|| {
                    fp_core::error::Error::from("struct-like enum payload layout is unavailable")
                })?;
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

    pub(super) fn lower_registered_struct_literal(
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
            (annotated_ty, self.lowering.struct_def(&def_id))
        {
            let enum_layout = match &expected_ty.kind {
                TyKind::Adt(adt, substs) if self.lowering.has_enum_def(&adt.did) => {
                    let args: Vec<Ty> = substs
                        .iter()
                        .filter_map(|arg| match arg {
                            mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                            _ => None,
                        })
                        .collect();
                    self.lowering
                        .enum_layout_for_instance(adt.did.clone(), &args, span)
                        .map(|layout| (adt.did.clone(), layout))
                }
                _ => None,
            };
            if let Some((enum_def_id, layout)) = enum_layout {
                if let Some(enum_def) = self.lowering.enum_def(&enum_def_id) {
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

    pub(super) fn lower_unknown_struct_literal(
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

    pub(super) fn infer_explicit_args_from_expected_return(
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
                TyKind::Adt(adt, substs) => (Some(adt.did.clone()), substs),
                TyKind::Opaque(_, substs) => (None, substs),
                _ => return None,
            };
            if let (Some(hir::Res::Def(def_id)), Some(expected_def_id)) =
                (path.res_ref().as_ref(), expected_def_id)
            {
                if def_id != &expected_def_id {
                    let matches_name = path
                        .segments()
                        .last()
                        .map(|seg| seg.ident.as_str())
                        .map(|name| {
                            self.lowering
                                .mir_package
                                .borrow()
                                .enum_defs
                                .get(&expected_def_id)
                                .map(|def| {
                                    def.name.as_str() == name
                                        || def.name.as_str().ends_with(&format!("::{}", name))
                                })
                                .unwrap_or(false)
                                || self
                                    .lowering
                                    .mir_package
                                    .borrow()
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

            let path_args = path.path().and_then(|path| {
                path.segments
                    .iter()
                    .find_map(|segment| segment.args.as_ref())
            });
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
                if type_path.segments().len() != 1 || type_path.segments()[0].args.is_some() {
                    return None;
                }
                let name = type_path.segments()[0].ident.as_str();
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
                    .mir_package
                    .borrow()
                    .enum_defs
                    .get(&adt.did)
                    .map(|def| {
                        def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                    })
                    .unwrap_or(false),
                TyKind::Opaque(def_id, _) => self
                    .lowering
                    .mir_package
                    .borrow()
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
}
