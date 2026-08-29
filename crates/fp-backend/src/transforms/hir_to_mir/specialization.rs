use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir::ty::{Abi, GenericArg, Ty, TyKind};
use fp_core::mir::{
    self, FunctionSpecializationInfo, MethodContext, MethodDefinition, MethodLoweringInfo,
};
use fp_core::span::Span;
use std::collections::HashMap;

impl HirToMirLowerer {
    pub(crate) fn ensure_function_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
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
        let Some(item) = self.hir_item(def_id.clone()).cloned() else {
            return Ok(());
        };
        let hir::ItemKind::Function(function) = &item.kind else {
            return Ok(());
        };
        self.lowered_items.insert(def_id.clone());
        if def_id.package_id != self.current_package_id {
            // A dependency package's own function — that package
            // compiles its own body separately, in its own
            // `HirToMirLowerer` instance (own struct/enum/const
            // registrations); lowering it here would build it against
            // *this* package's registrations instead, silently
            // producing a wrong/incomplete body the moment it
            // references anything this package never registered. Only
            // this call site's own operand needs a signature — the
            // real body is supplied later, correctly, by
            // `predeclare_dependency_function_signatures` reading that
            // package's own compiled MIR.
            let sig = self.lower_function_sig(&function.sig, None);
            self.mir_package
                .borrow_mut()
                .function_sigs
                .insert(def_id, sig);
            return Ok(());
        }
        if !function.sig.generics.params.is_empty() {
            // Generic: raw HIR registration only, lowered per call site via
            // `ensure_function_specialization` — this function doesn't own
            // that path.
            self.register_generic_function(def_id, function);
            return Ok(());
        }
        let previous_item_path = self.current_item_path.take();
        self.current_item_path = self.hir_def_path(def_id).map(|path| path.join("::"));
        let result = self
            .lower_function(&item, function)
            .map_err(|error| format!("while lowering `{}`: {error}", function.sig.name));
        self.current_item_path = previous_item_path;
        let (mir_item, body_id, body) = result?;
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
    /// Lowers one resolved method body. The compiler driver uses this for a
    /// concrete foreign method reached by a comptime entry, always with a
    /// lowerer rooted in the method's owning package.
    pub fn ensure_method_lowered(&mut self, def_id: hir::DefId) -> Result<()> {
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
        let Some(method_ref) = self
            .mir_package
            .borrow()
            .method_hir_defs
            .get(&def_id)
            .cloned()
        else {
            return Ok(());
        };
        self.lowered_items.insert(def_id.clone());
        if def_id.package_id != self.current_package_id {
            // Same reasoning as `ensure_function_lowered`'s
            // cross-package guard — this method's own package compiles
            // its body separately.
            let sig = self
                .lower_function_sig(&method_ref.function.sig, method_ref.method_context.as_ref());
            self.mir_package
                .borrow_mut()
                .function_sigs
                .insert(def_id, sig);
            return Ok(());
        }
        let previous_item_path = self.current_item_path.take();
        self.current_item_path = self
            .hir_def_path(def_id.clone())
            .map(|path| path.join("::"));
        let result = self
            .lower_method(
                def_id,
                &method_ref.function,
                method_ref.span,
                method_ref.method_context.as_ref(),
            )
            .map_err(|error| {
                format!(
                    "while lowering method `{}`: {error}",
                    method_ref.function.sig.name
                )
            });
        self.current_item_path = previous_item_path;
        let (mir_item, body_id, body, _sig) = result?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));
        Ok(())
    }

    pub(crate) fn lower_function(
        &mut self,
        item: &hir::Item,
        function: &hir::Function,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.mir_package.borrow_mut().fresh_body_id());

        let sig = self.lower_function_sig(&function.sig, None);
        self.mir_package
            .borrow_mut()
            .function_sigs
            .insert(item.def_id.clone(), sig.clone());
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
            name: mir::Symbol::new(
                self.def_path_str(item.def_id.clone(), function.sig.name.as_str()),
            ),
            def_id: Some(item.def_id.clone()),
            substs: Vec::new(),
            sig,
            body_id,
            abi: self.map_abi(&function.sig.abi),
            is_extern: function.is_extern,
            attrs: function.attrs.clone(),
        };

        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Function(mir_function),
        };

        Ok((mir_item, body_id, mir_body))
    }

    pub(super) fn stub_body(&mut self, sig: &mir::FunctionSig, span: Span) -> mir::Body {
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

    pub(super) fn catch_unwind_default_constant_for_ty(
        &self,
        ty: &Ty,
    ) -> Result<mir::ConstantKind> {
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

    pub(super) fn register_generic_function(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
    ) {
        if self
            .mir_package
            .borrow()
            .generic_function_defs
            .contains_key(&def_id)
        {
            return;
        }
        let sig = self.lower_function_sig(&function.sig, None);
        self.mir_package
            .borrow_mut()
            .function_sigs
            .insert(def_id.clone(), sig);
        self.mir_package
            .borrow_mut()
            .generic_function_defs
            .insert(def_id, function.clone());
    }

    pub(super) fn lower_function_with_substs(
        &mut self,
        item_def_id: hir::DefId,
        item_span: Span,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        substs: HashMap<String, Ty>,
        name_override: &str,
        function_substs: mir::ty::SubstsRef,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.mir_package.borrow_mut().fresh_body_id());

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
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Function(mir_function),
        };

        Ok((mir_item, body_id, mir_body))
    }

    pub(crate) fn ensure_function_specialization(
        &mut self,
        def_id: hir::DefId,
        function: &hir::Function,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<FunctionSpecializationInfo> {
        let pre_key = (
            def_id.clone(),
            explicit_args.to_vec(),
            arg_types.to_vec(),
            expected_return.cloned(),
        );
        if let Some(info) = self
            .mir_package
            .borrow()
            .function_specialization_call_cache
            .get(&pre_key)
            .cloned()
        {
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
        self.mir_package
            .borrow_mut()
            .function_specialization_call_cache
            .insert(pre_key, info.clone());
        Ok(info)
    }

    pub(super) fn ensure_function_specialization_uncached(
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
        let key = (def_id.clone(), function_substs.clone());

        if let Some(info) = self
            .mir_package
            .borrow()
            .function_specializations
            .get(&key)
            .cloned()
        {
            return Ok(info.clone());
        }

        let sig = self.lower_function_sig_with_substs(&function.sig, None, &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}_{}", function.sig.name.as_str(), suffix, def_id);
        let fn_ty = self.function_pointer_ty(&sig);

        let item_span = self
            .hir_item(def_id.clone())
            .map(|item| item.span)
            .ok_or_else(|| crate::error::optimization_error("missing function item"))?;
        let (mir_item, body_id, body) = self.lower_function_with_substs(
            def_id.clone(),
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
        self.mir_package
            .borrow_mut()
            .function_specializations
            .insert(key, info.clone());
        Ok(info)
    }

    pub(super) fn ensure_function_specialization_from_explicit_args(
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
        let key = (def_id.clone(), function_substs.clone());

        if let Some(info) = self
            .mir_package
            .borrow()
            .function_specializations
            .get(&key)
            .cloned()
        {
            return Ok(info.clone());
        }

        let sig = self.lower_function_sig_with_substs(&function.sig, None, &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}_{}", function.sig.name.as_str(), suffix, def_id);
        let fn_ty = self.function_pointer_ty(&sig);

        let item_span = self
            .hir_item(def_id.clone())
            .map(|item| item.span)
            .ok_or_else(|| crate::error::optimization_error("missing function item"))?;
        let (mir_item, body_id, body) = self.lower_function_with_substs(
            def_id.clone(),
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
        self.mir_package
            .borrow_mut()
            .function_specializations
            .insert(key, info.clone());
        Ok(info)
    }

    pub(crate) fn ensure_method_specialization(
        &mut self,
        def: &MethodDefinition,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        expected_return: Option<&Ty>,
        span: Span,
    ) -> Result<MethodLoweringInfo> {
        let pre_key = (
            def.def_id.clone(),
            explicit_args.to_vec(),
            arg_types.to_vec(),
            expected_return.cloned(),
        );
        if let Some(info) = self
            .mir_package
            .borrow()
            .method_specialization_call_cache
            .get(&pre_key)
            .cloned()
        {
            return Ok(info.clone());
        }
        let info = self.ensure_method_specialization_uncached(
            def,
            explicit_args,
            arg_types,
            expected_return,
            span,
        )?;
        self.mir_package
            .borrow_mut()
            .method_specialization_call_cache
            .insert(pre_key, info.clone());
        Ok(info)
    }

    pub(super) fn ensure_method_specialization_uncached(
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

    pub(super) fn ensure_method_specialization_from_explicit_args(
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
    pub(super) fn finish_method_specialization(
        &mut self,
        def: &MethodDefinition,
        substs: HashMap<String, Ty>,
        args_in_order: &[Ty],
        method_substs: mir::ty::SubstsRef,
        span: Span,
        carries_def_id: bool,
    ) -> Result<MethodLoweringInfo> {
        let key = (def.def_id.clone(), method_substs.clone());

        if let Some(info) = self
            .mir_package
            .borrow()
            .method_specializations
            .get(&key)
            .cloned()
        {
            return Ok(info.clone());
        }

        let mut method_context = if let hir::TypeExprKind::Path(path) = &def.self_ty.kind {
            let mir_self_ty = self.lower_type_expr_with_substs(&def.self_ty, &substs);
            Some(MethodContext {
                def_id: def.self_def.clone(),
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

        let body_id = mir::BodyId::new(self.mir_package.borrow_mut().fresh_body_id());

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
            def_id: Some(def.def_id.clone()),
            substs: method_substs.clone(),
            sig: sig.clone(),
            body_id,
            abi: self.map_abi(&def.function.sig.abi),
            is_extern: false,
            attrs: Vec::new(),
        };
        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Function(mir_function),
        };

        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, mir_body));

        let info = MethodLoweringInfo {
            def_id: if carries_def_id {
                Some(def.def_id.clone())
            } else {
                None
            },
            substs: method_substs,
            sig,
            fn_name: name.clone(),
            fn_ty,
            struct_def: def.self_def.clone(),
        };
        self.mir_package
            .borrow_mut()
            .method_specializations
            .insert(key, info.clone());
        Ok(info)
    }

    pub(crate) fn lower_function_sig(
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

    pub(super) fn lower_function_sig_with_substs(
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

    pub(super) fn lower_type_expr_with_context_for_abi(
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
                hir::TypeExprKind::Ptr { inner: inner, .. } => {
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

    pub(super) fn lower_type_expr_with_context_and_substs_for_abi(
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
                hir::TypeExprKind::Ptr { inner: inner, .. } => {
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

    pub(super) fn map_abi(&self, abi: &hir::Abi) -> mir::ty::Abi {
        match abi {
            hir::Abi::Rust => mir::ty::Abi::Rust,
            hir::Abi::C { unwind } => mir::ty::Abi::C { unwind: *unwind },
            hir::Abi::Named(_) => mir::ty::Abi::Rust,
            hir::Abi::System { unwind } => mir::ty::Abi::System { unwind: *unwind },
            _ => mir::ty::Abi::Rust,
        }
    }

    pub(super) fn specialization_suffix(&self, args: &[Ty]) -> String {
        let mut hasher = DefaultHasher::new();
        for ty in args {
            ty.hash(&mut hasher);
        }
        format!("mono_{:x}", hasher.finish())
    }

    pub(super) fn build_substs_from_args(
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
                            mir::ty::GenericArg::Type(ty) => {
                                Some(self.unwrap_expr_actual_ty(ty).clone())
                            }
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
                            .map(|ty| self.unwrap_expr_actual_ty(ty).clone())
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
                        .mir_package
                        .borrow()
                        .enum_defs
                        .get(&layout.def_id)
                        .map(|def| {
                            def.name.as_str() == "Result" || def.name.as_str().ends_with("::Result")
                        })
                        .unwrap_or(false);
                    if is_result_layout && generics.len() >= 2 {
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
                        .mir_package
                        .borrow()
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
}
