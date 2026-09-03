use super::*;

impl AstToHirLowerer {
    pub(super) fn create_main_function(
        &mut self,
        body: hir::Block,
        output: hir::TypeExpr,
    ) -> Result<hir::Function> {
        let sig = hir::FunctionSig {
            name: hir::Symbol::new("main"),
            inputs: Vec::new(),
            output,
            generics: hir::Generics::default(),
            abi: hir::Abi::Rust,
        };

        Ok(hir::Function::new(sig, Some(body), false, false))
    }

    pub fn transform_function(
        &mut self,
        func: &ast::ItemDefFunction,
        self_ty: Option<hir::TypeExpr>,
    ) -> Result<hir::Function> {
        self.transform_function_with_body(func, self_ty, true)
    }

    pub(super) fn transform_function_with_body(
        &mut self,
        func: &ast::ItemDefFunction,
        self_ty: Option<hir::TypeExpr>,
        lower_body: bool,
    ) -> Result<hir::Function> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let generics = self.transform_generics(&func.sig.generics_params)?;

            let mut params = self.transform_params(&func.sig.params)?;
            if let Some(receiver) = &func.sig.receiver {
                let receiver_ty = self_ty.clone().unwrap_or_else(|| self.create_unit_type());
                let self_param = self.make_self_param(receiver, receiver_ty)?;
                self.register_pattern_bindings(&self_param.pat);
                params.insert(0, self_param);
            }
            let mut output = if let Some(ret_ty) = &func.sig.ret_ty {
                self.transform_type_to_hir(ret_ty)?
            } else {
                self.create_unit_type()
            };
            if let hir::TypeExprKind::FnPtr(_) | hir::TypeExprKind::Infer = output.kind {
                if let Some(last_expr) = func.body.last_expr() {
                    if let ast::ExprKind::Struct(struct_expr) = last_expr.kind() {
                        if let Ok(path) = self.ast_expr_to_hir_path(
                            struct_expr.name.as_ref(),
                            PathResolutionScope::Type,
                        ) {
                            if path
                                .segments
                                .last()
                                .map(|seg| seg.name.as_str().starts_with("__Closure"))
                                .unwrap_or(false)
                            {
                                output = hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(path),
                                    Span::new(self.current_file, 0, 0),
                                );
                            }
                        }
                    }
                }
            }

            let sig = hir::FunctionSig {
                name: hir::Symbol::new(func.name.name.clone()),
                inputs: params.clone(),
                output: output.clone(),
                generics,
                abi: self.map_abi(&func.sig.abi),
            };

            if lower_body {
                let body = self.transform_block_node_to_hir(&func.body)?;

                let mut function = hir::Function::new(sig, Some(body), func.sig.is_const, false);
                function.is_async = func.is_async;
                function.attrs = func.attrs.clone();
                Ok(function)
            } else {
                let mut function = hir::Function::new(sig, None, func.sig.is_const, false);
                function.is_async = func.is_async;
                function.attrs = func.attrs.clone();
                Ok(function)
            }
        })();

        self.pop_value_scope();
        self.pop_type_scope();

        result
    }

    pub fn transform_decl_function_sig(
        &mut self,
        func: &ast::ItemDeclFunction,
        self_ty: Option<hir::TypeExpr>,
    ) -> Result<hir::Function> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let generics = self.transform_generics(&func.sig.generics_params)?;

            let mut params = self.transform_params(&func.sig.params)?;
            if let Some(receiver) = &func.sig.receiver {
                let receiver_ty = self_ty.clone().unwrap_or_else(|| self.create_unit_type());
                let self_param = self.make_self_param(receiver, receiver_ty)?;
                self.register_pattern_bindings(&self_param.pat);
                params.insert(0, self_param);
            }
            let output = if let Some(ret_ty) = &func.sig.ret_ty {
                self.transform_type_to_hir(ret_ty)?
            } else {
                self.create_unit_type()
            };

            let sig = hir::FunctionSig {
                name: hir::Symbol::new(func.name.name.clone()),
                inputs: params,
                output,
                generics,
                abi: self.map_abi(&func.sig.abi),
            };

            let mut function = hir::Function::new(sig, None, func.sig.is_const, true);
            function.attrs = func.attrs.clone();
            Ok(function)
        })();

        self.pop_value_scope();
        self.pop_type_scope();

        result
    }

    pub(super) fn transform_params(
        &mut self,
        params: &[ast::FunctionParam],
    ) -> Result<Vec<hir::Param>> {
        params
            .iter()
            .map(|param| {
                let ty = self.transform_type_to_hir(&param.ty)?;
                let pat = hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Binding {
                        name: param.name.clone().into(),
                        mutable: false,
                    },
                };

                let hir_param = hir::Param {
                    hir_id: self.next_id(),
                    pat,
                    ty,
                    is_context: param.is_context,
                    as_tuple: param.as_tuple,
                    as_dict: param.as_dict,
                    default: param
                        .default
                        .as_ref()
                        .map(|value| self.transform_expr_to_hir(&ast::Expr::value(value.clone())))
                        .transpose()?,
                };

                self.register_pattern_bindings(&hir_param.pat);

                Ok(hir_param)
            })
            .collect()
    }

    pub(super) fn transform_generics(
        &mut self,
        params: &[ast::GenericParam],
    ) -> Result<hir::Generics> {
        let mut hir_params = Vec::new();
        for (index, param) in params.iter().enumerate() {
            let namespace = match param.kind {
                ast::GenericParamKind::Type => fp_core::hir::resolve::Namespace::Type,
                ast::GenericParamKind::Const { .. } => fp_core::hir::resolve::Namespace::Value,
            };
            let hir_id = self.next_id();
            let def_id = self
                .current_owner
                .as_ref()
                .and_then(|owner| {
                    self.impl_generic_param_ids
                        .get(&(owner.clone(), index))
                        .cloned()
                })
                .or_else(|| {
                    self.current_owner.as_ref().map(|owner| {
                        self.package_mut()
                            .member_def_id(owner, param.name.name.clone(), namespace)
                    })
                })
                .unwrap_or_else(|| self.next_def_id());
            match &param.kind {
                ast::GenericParamKind::Type => {
                    self.register_type_generic(&param.name.name, def_id.clone());
                }
                ast::GenericParamKind::Const { .. } => {
                    self.register_value_generic(&param.name.name, def_id.clone());
                }
            }
            // A generic parameter's own trait bounds (`F: FnOnce() -> R`,
            // `I: Iterator<Item = T>`, ...) so `path_ty` can resolve a
            // still-generic `F::Output`/`I::Item`-style associated-type
            // projection from the bound that actually declares it,
            // instead of only ever resolving `T::AssocName` once `T` is a
            // concrete type. `parse_type_bounds` (fp-lang) already folds
            // `Fn`/`FnOnce`/`FnMut(..) -> R` sugar into a bare `Ty::
            // Function`, wrapped as `Expr::Value(Value::Type(..))` by
            // `type_to_expr` — lower that case through the ordinary type
            // path (preserving the return type as `FnPtr`'s `output`);
            // every other bound is a real trait-bound expression, lowered
            // to a `Path` the same way any other one is. A bound that
            // fails to resolve here (e.g. one naming a trait this checker
            // hasn't seen yet) is simply dropped from the list — the
            // parameter is still usable structurally, it just won't help
            // resolve that one associated-type projection.
            let bounds = param
                .bounds
                .bounds
                .iter()
                .filter_map(|bound| {
                    if let ast::ExprKind::Value(value) = bound.kind() {
                        if let ast::Value::Type(ty) = &**value {
                            return self.transform_type_to_hir(ty).ok();
                        }
                    }
                    let path = self
                        .ast_expr_to_hir_path(bound, PathResolutionScope::Type)
                        .ok()?;
                    Some(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Path(path),
                        bound.span(),
                    ))
                })
                .collect();
            // Explicit associated-type bindings on one of this
            // parameter's own trait bounds (`I: Iterator<Item = U>`) —
            // extracted straight from the original AST bound expression
            // (a path-based Name, per-segment `args: Vec<Ty>`,
            // fp-lang's own `parse_type_arg` already turning `Item = U`
            // into a `Ty::Expr(Assign { target: Item, value: U })` entry
            // there) rather than re-derived from the just-lowered `Path`
            // above, whose ordinary `GenericArgs` has nowhere to carry a
            // binding as such. See `GenericParam::explicit_bindings`'s
            // own doc comment for why this needs its own field.
            let explicit_bindings = param
                .bounds
                .bounds
                .iter()
                .flat_map(|bound| {
                    let ast::ExprKind::Name(fp_core::ast::Name {
                        path: parameter_path,
                        ..
                    }) = bound.kind()
                    else {
                        return Vec::new();
                    };
                    let Some(last_segment) = parameter_path.segments.last() else {
                        return Vec::new();
                    };
                    last_segment
                        .args
                        .iter()
                        .filter_map(|arg| {
                            let ast::Ty::Expr(arg_expr) = arg else {
                                return None;
                            };
                            let ast::ExprKind::Assign(assign) = arg_expr.kind() else {
                                return None;
                            };
                            let ast::ExprKind::Name(fp_core::ast::Name {
                                path: binding_path, ..
                            }) = assign.target.kind()
                            else {
                                return None;
                            };
                            let Some(binding_name) = binding_path.clone().try_into_ident() else {
                                return None;
                            };
                            let value_path = self
                                .ast_expr_to_hir_path(&assign.value, PathResolutionScope::Type)
                                .ok()?;
                            Some((
                                binding_name.name.clone().into(),
                                hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(value_path),
                                    assign.value.span(),
                                ),
                            ))
                        })
                        .collect()
                })
                .collect();
            let projection_bounds = param
                .projection_bounds
                .iter()
                .map(|(projection, bounds)| {
                    let bounds = bounds
                        .bounds
                        .iter()
                        .filter_map(|bound| {
                            let path = self
                                .ast_expr_to_hir_path(bound, PathResolutionScope::Type)
                                .ok()?;
                            Some(hir::TypeExpr::new(
                                self.next_id(),
                                hir::TypeExprKind::Path(path),
                                bound.span(),
                            ))
                        })
                        .collect();
                    (projection.name.clone().into(), bounds)
                })
                .collect();
            let kind = match &param.kind {
                ast::GenericParamKind::Type => hir::GenericParamKind::Type { default: None },
                ast::GenericParamKind::Const { ty } => hir::GenericParamKind::Const {
                    ty: Box::new(self.transform_type_to_hir(ty)?),
                },
            };
            hir_params.push(hir::GenericParam {
                hir_id,
                def_id: def_id.clone(),
                name: param.name.clone().into(),
                kind,
                bounds,
                explicit_bindings,
                projection_bounds,
            });
        }

        Ok(hir::Generics {
            params: hir_params,
            where_clause: None,
        })
    }

    pub(super) fn wrap_ref_type(&mut self, ty: hir::TypeExpr) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Ref(Box::new(ty)),
            Span::new(self.current_file, 0, 0),
        )
    }

    pub(super) fn make_self_param(
        &mut self,
        receiver: &ast::FunctionParamReceiver,
        self_ty: hir::TypeExpr,
    ) -> Result<hir::Param> {
        let ty = match receiver {
            ast::FunctionParamReceiver::Typed(ty) => self.transform_type_to_hir(ty)?,
            ast::FunctionParamReceiver::Ref
            | ast::FunctionParamReceiver::RefStatic
            | ast::FunctionParamReceiver::RefMut
            | ast::FunctionParamReceiver::RefMutStatic => self.wrap_ref_type(self_ty),
            _ => self_ty,
        };

        Ok(hir::Param {
            hir_id: self.next_id(),
            pat: hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding {
                    name: hir::Symbol::new("self"),
                    mutable: false,
                },
            },
            ty,
            is_context: false,
            as_tuple: false,
            as_dict: false,
            default: None,
        })
    }

    fn is_unimplemented_type_expr(&self, ty: &hir::TypeExpr) -> bool {
        let hir::TypeExprKind::Path(path) = &ty.kind else {
            return false;
        };
        let hir::Res::Def(ref def_id) = path.res else {
            return false;
        };
        self.unimplemented_type_def_ids.contains(&def_id)
    }

    pub(super) fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::Impl> {
        let saved_impl_self_ty = self.current_impl_self_ty.clone();
        self.push_type_scope();
        // `Self` in type position (`-> Self`, `&Self`) resolves through the
        // type scope above. `Self { x, y }` struct-literal construction in
        // method bodies resolves through the *value* namespace instead
        // (mirroring how real struct names are registered in both
        // namespaces, `register_type_def` + `register_value_def`) — needs
        // its own registration here or it stays unresolved.
        self.push_value_scope();
        let result = (|| {
            // `Self` is a lexical binding of every impl, in both namespaces:
            // type references use it directly, while `Self { ... }` and
            // associated-value paths use the value namespace. Register it
            // before lowering the self type and any method signatures.
            for namespace in [
                fp_core::hir::resolve::Namespace::Type,
                fp_core::hir::resolve::Namespace::Value,
            ] {
                let _ = self.local_resolver.declare(
                    "Self",
                    fp_core::hir::resolve::Binding::Import {
                        target: hir::Res::SelfTy,
                        namespace,
                        span: Span::null(),
                    },
                );
            }
            // Register impl generics in the current type scope.
            let generics = self.transform_generics(&impl_block.generics_params)?;
            let self_ty_ast = ast::Ty::expr(impl_block.self_ty.clone());
            let lowered_self_ty = self.transform_type_to_hir(&self_ty_ast)?;
            let self_ty = lowered_self_ty;
            self.current_impl_self_ty = Some(self_ty.clone());
            let impl_key = self.impl_self_key(&self_ty).ok();
            let trait_ty = if let Some(trait_name) = &impl_block.trait_ty {
                let trait_expr = ast::Expr::new(ast::ExprKind::Name(trait_name.clone()));
                let trait_path =
                    self.ast_expr_to_hir_path(&trait_expr, PathResolutionScope::Trait)?;
                Some(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(trait_path),
                    Span::new(self.current_file, 0, 0),
                ))
            } else {
                None
            };
            let stub_methods = attrs_has_name(&impl_block.attrs, "unimplemented")
                || self.is_unimplemented_type_expr(&self_ty);

            let mut items = Vec::new();
            let mut method_names = HashSet::new();
            for item in &impl_block.items {
                if should_drop_const_type_item(item) {
                    continue;
                }
                match item.kind() {
                    ast::ItemKind::DefFunction(func) => {
                        let mut method = self.transform_function_with_body(
                            func,
                            Some(self_ty.clone()),
                            !stub_methods && !attrs_has_name(&func.attrs, "unimplemented"),
                        )?;
                        // See `function_body_is_compiler_intrinsic_marker`'s
                        // doc comment: a real, hand-written method's body
                        // must survive; only a bare `compile_error!(...)`
                        // marker body gets dropped back to a stub.
                        if function_body_is_compiler_intrinsic_marker(&method) {
                            method.body = None;
                        }
                        let method_def_id = impl_key
                            .as_ref()
                            .and_then(|key| {
                                self.impl_items
                                    .get(&(key.clone(), func.name.name.clone().into()))
                                    .cloned()
                            })
                            .unwrap_or_else(|| self.next_def_id());
                        if let Some(key) = impl_key.clone() {
                            self.impl_items.insert(
                                (key, func.name.name.clone().into()),
                                method_def_id.clone(),
                            );
                        }
                        if let Some(tag) = fp_core::lang::extract_intrinsic_item(&func.attrs) {
                            if let Some(kind) =
                                fp_core::intrinsics::lang_intrinsic_for_lang_item(&tag)
                                    .and_then(fp_core::intrinsics::lang_intrinsic_call_kind)
                            {
                                self.package_mut()
                                    .intrinsic_defs
                                    .insert(method_def_id.clone(), kind);
                            }
                        }
                        method_names.insert(method.sig.name.as_str().to_string());
                        items.push(hir::ImplItem {
                            def_id: method_def_id,
                            hir_id: self.next_id(),
                            name: func.name.name.clone().into(),
                            kind: hir::ImplItemKind::Method(method),
                        });
                    }
                    ast::ItemKind::DefConst(const_item) => {
                        let assoc_const = self.transform_const_def(const_item)?;
                        // Must reuse the same `DefId` `predeclare_items`
                        // already allocated and registered this const's
                        // value-path under (`next_def_id`, same
                        // as the `DefFunction` arm above does for
                        // methods) — a fresh `next_def_id()` here mints an
                        // unrelated number, so any reference resolved
                        // during predeclare (e.g. `char::MIN` elsewhere)
                        // would point at a `DefId` this `ImplItem` never
                        // actually carries.
                        let const_def_id = impl_key
                            .as_ref()
                            .and_then(|key| {
                                self.impl_items
                                    .get(&(key.clone(), const_item.name.name.clone().into()))
                                    .cloned()
                            })
                            .unwrap_or_else(|| self.next_def_id());
                        if let Some(key) = impl_key.clone() {
                            self.impl_items.insert(
                                (key, const_item.name.name.clone().into()),
                                const_def_id.clone(),
                            );
                        }
                        items.push(hir::ImplItem {
                            def_id: const_def_id,
                            hir_id: self.next_id(),
                            name: const_item.name.clone().into(),
                            kind: hir::ImplItemKind::AssocConst(assoc_const),
                        });
                    }
                    // `type Target = Y;` — an impl's own binding for one of
                    // its trait's associated types. Lets `Self::Target`
                    // resolve during typecheck (`HirTypeChecker::
                    // impl_assoc_types`) instead of being silently dropped
                    // like before (this arm used to fall into the `_ => {}`
                    // catch-all).
                    ast::ItemKind::DefType(type_item) => {
                        let ty = self.transform_type_to_hir(&type_item.value)?;
                        items.push(hir::ImplItem {
                            def_id: self.member_def_id(
                                type_item.name.clone(),
                                fp_core::hir::resolve::Namespace::Type,
                            ),
                            hir_id: self.next_id(),
                            name: type_item.name.clone().into(),
                            kind: hir::ImplItemKind::AssocType(hir::AssocType {
                                name: type_item.name.clone().into(),
                                ty,
                            }),
                        });
                    }
                    _ => {}
                }
            }

            if let Some(trait_name) = &impl_block.trait_ty {
                let trait_generic_args: &[ast::Ty] = match trait_name {
                    ast::Name { path, .. } => path
                        .segments
                        .last()
                        .map(|seg| seg.args.as_slice())
                        .unwrap_or_default(),
                };
                let trait_name = match trait_name {
                    ast::Name { path, .. } => path
                        .segments
                        .last()
                        .map(|seg| seg.ident.name.clone())
                        .unwrap_or_default(),
                };
                if let Some(trait_def) = self.trait_defs.get(&trait_name).cloned() {
                    let trait_items = trait_def.items.clone();
                    // A default-bodied trait method (`PartialEq::ne`,
                    // `PartialOrd::lt`/`le`/`gt`/`ge`, ...) synthesized
                    // below is lowered fresh, right here, inside *this*
                    // impl's own scope — which has `Self` bound but not
                    // the trait's own generic parameters (`PartialEq<Rhs
                    // = Self>`'s `Rhs`), since that scope only existed
                    // during `transform_trait`'s own (long-finished)
                    // processing of the trait declaration itself. Left
                    // unbound, every occurrence of `Rhs` inside a
                    // synthesized method's copied-verbatim signature
                    // (`fn ne(&self, other: &Rhs) -> bool`) fails to
                    // resolve — the single largest unresolved-type-path
                    // bucket across vendored core/alloc/std, since almost
                    // no `PartialEq`/`PartialOrd` impl ever redeclares
                    // `ne`/`lt`/etc. Bind each trait generic parameter to
                    // this impl's own explicit argument for it
                    // (`impl PartialEq<Foo> for X`'s `Foo`), positionally,
                    // falling back to `Self` for anything left
                    // unspecified — real rustc's own `Rhs = Self` default,
                    // which `ast::GenericParam` has nowhere to carry
                    // through from the parser (see `parse_optional_generic_params`,
                    // which parses and discards a default value's tokens).
                    // These are imports deliberately: the copied method is
                    // an instantiated impl method, not a new generic
                    // declaration. A `Binding::Generic` here would leave
                    // the trait parameter abstract in the impl signature
                    // and lose the concrete substitution represented by the
                    // impl's trait arguments.
                    self.push_type_scope();
                    for (index, param) in trait_def.generics_params.iter().enumerate() {
                        let target = match trait_generic_args.get(index) {
                            Some(ast::Ty::Expr(expr))
                                if matches!(expr.kind(), ast::ExprKind::Name(_)) =>
                            {
                                self.ast_expr_to_hir_path(expr, PathResolutionScope::Type)
                                    .map(|path| path.res)?
                            }
                            // Trait type arguments are parsed as type
                            // expressions.  Keep the default `Self` target
                            // for an omitted argument, matching Rust's
                            // defaulted trait parameter semantics.
                            _ => hir::Res::SelfTy,
                        };
                        self.local_resolver.declare(
                            param.name.name.clone(),
                            fp_core::hir::resolve::Binding::Import {
                                target,
                                namespace: fp_core::hir::resolve::Namespace::Type,
                                span: Span::null(),
                            },
                        );
                    }
                    // Synthesize default trait methods into the impl if they
                    // are missing — resolving names as if still in the
                    // trait's own declaring module, not this impl's:
                    // `Iterator::try_fold`'s default body returns
                    // `ControlFlow`, `Any::type_id`'s returns `TypeId`, ...
                    // via that file's own `use` imports, which are
                    // persisted keyed by the *importing* module's path
                    // (`record_type_symbol`), so resolving them from the
                    // impl's own (generally different) module finds
                    // nothing — the same shape of bug the trait-generic
                    // bindings above fix for a trait's own type
                    // parameters, just for its ordinary imports instead.
                    let saved_module_path = self.module_path.clone();
                    if let Some(trait_module) = self.trait_def_modules.get(&trait_name) {
                        self.module_path = trait_module.clone();
                    }
                    let synthesis_result = (|| -> Result<()> {
                        for trait_item in &trait_items {
                            let ast::ItemKind::DefFunction(func) = trait_item.kind() else {
                                continue;
                            };
                            if should_drop_const_type_item(trait_item) {
                                continue;
                            }
                            if method_names.contains(&func.name.name) {
                                continue;
                            }
                            let method = self.transform_function_with_body(
                                func,
                                Some(self_ty.clone()),
                                !stub_methods && !attrs_has_name(&func.attrs, "unimplemented"),
                            )?;
                            method_names.insert(method.sig.name.as_str().to_string());
                            let trait_method_def_id = trait_items.iter().find_map(|item| {
                                let ast::ItemKind::DefFunction(trait_func) = item.kind() else {
                                    return None;
                                };
                                (trait_func.name.name == func.name.name).then(|| self.next_def_id())
                            });
                            if let Some(trait_method_def_id) = trait_method_def_id {}
                            items.push(hir::ImplItem {
                                def_id: self.member_def_id(
                                    method.sig.name.clone(),
                                    fp_core::hir::resolve::Namespace::Value,
                                ),
                                hir_id: self.next_id(),
                                name: method.sig.name.clone(),
                                kind: hir::ImplItemKind::Method(method),
                            });
                        }
                        Ok(())
                    })();
                    self.module_path = saved_module_path;
                    synthesis_result?;
                    self.pop_type_scope();
                }
            }

            Ok(hir::Impl {
                generics,
                trait_ty,
                self_ty,
                items,
            })
        })();

        self.pop_value_scope();
        self.pop_type_scope();
        self.current_impl_self_ty = saved_impl_self_ty;

        result
    }

    /// Lowers a trait definition into a real `hir::Trait` — the shared
    /// declaration every concrete `impl Trait for X` is checked/resolved
    /// against (see `HirTypeChecker::method_output`'s trait-default-method
    /// fallback, which searches an already-resolved trait's `items` when a
    /// concrete impl doesn't redeclare a requested method itself). `Self`
    /// resolves to the same `hir::Res::SelfTy` lexical binding
    /// `transform_impl` registers for a real impl's own `self_ty` — here
    /// there's no concrete self-type to substitute (the trait definition
    /// itself is never instantiated on its own), so default-method bodies
    /// referencing `Self`/`Self::AssocType` stay abstract; that's fine,
    /// since these bodies are never type-checked on their own (see
    /// `HirTypeChecker::check_item`'s `ItemKind::Trait` arm) — only their
    /// signatures are ever read, after substitution against a real impl's
    /// concrete `Self`.
    pub(super) fn transform_trait(&mut self, def_trait: &ast::ItemDefTrait) -> Result<hir::Trait> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let generics = self.transform_generics(&def_trait.generics_params)?;
            let self_ty = hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Path(hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new("Self"),
                        args: None,
                    }],
                    res: hir::Res::SelfTy,
                }),
                Span::new(self.current_file, 0, 0),
            );

            let mut items = Vec::new();
            for item in &def_trait.items {
                match item.kind() {
                    ast::ItemKind::DefFunction(func) => {
                        // A default-provided method (has a real body) —
                        // the fallback signature source `method_output`
                        // reads when a concrete impl doesn't redeclare it.
                        let function =
                            self.transform_function_with_body(func, Some(self_ty.clone()), true)?;
                        let method_def_id = self.member_def_id(
                            func.name.name.clone(),
                            fp_core::hir::resolve::Namespace::Value,
                        );
                        items.push(hir::TraitItem {
                            def_id: method_def_id,
                            hir_id: self.next_id(),
                            name: func.name.name.clone().into(),
                            kind: hir::TraitItemKind::Method(function),
                        });
                    }
                    ast::ItemKind::DeclFunction(func_decl) => {
                        // An abstract method (no body) — every concrete
                        // impl must supply its own; never used as a
                        // fallback signature source.
                        let function =
                            self.transform_decl_function_sig(func_decl, Some(self_ty.clone()))?;
                        let method_def_id = self.member_def_id(
                            func_decl.name.name.clone(),
                            fp_core::hir::resolve::Namespace::Value,
                        );
                        items.push(hir::TraitItem {
                            def_id: method_def_id,
                            hir_id: self.next_id(),
                            name: func_decl.name.name.clone().into(),
                            kind: hir::TraitItemKind::Method(function),
                        });
                    }
                    ast::ItemKind::DeclType(decl_type) => {
                        let bounds = decl_type
                            .bounds
                            .bounds
                            .iter()
                            .filter_map(|bound| {
                                let path = self
                                    .ast_expr_to_hir_path(bound, PathResolutionScope::Type)
                                    .ok()?;
                                Some(hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(path),
                                    bound.span(),
                                ))
                            })
                            .collect();
                        items.push(hir::TraitItem {
                            def_id: self.member_def_id(
                                decl_type.name.name.clone(),
                                fp_core::hir::resolve::Namespace::Type,
                            ),
                            hir_id: self.next_id(),
                            name: decl_type.name.name.clone().into(),
                            kind: hir::TraitItemKind::AssocType(hir::TraitAssocType {
                                name: decl_type.name.name.clone().into(),
                                bounds,
                            }),
                        });
                    }
                    ast::ItemKind::DefConst(const_item) => {
                        let konst = self.transform_const_def(const_item)?;
                        items.push(hir::TraitItem {
                            def_id: self.member_def_id(
                                const_item.name.name.clone(),
                                fp_core::hir::resolve::Namespace::Value,
                            ),
                            hir_id: self.next_id(),
                            name: const_item.name.name.clone().into(),
                            kind: hir::TraitItemKind::AssocConst(hir::TraitAssocConst {
                                name: const_item.name.name.clone().into(),
                                ty: konst.ty,
                                body: Some(konst.body),
                            }),
                        });
                    }
                    ast::ItemKind::DeclConst(const_item) => {
                        let ty = self.transform_type_to_hir(&const_item.ty)?;
                        items.push(hir::TraitItem {
                            def_id: self.member_def_id(
                                const_item.name.name.clone(),
                                fp_core::hir::resolve::Namespace::Value,
                            ),
                            hir_id: self.next_id(),
                            name: const_item.name.name.clone().into(),
                            kind: hir::TraitItemKind::AssocConst(hir::TraitAssocConst {
                                name: const_item.name.name.clone().into(),
                                ty,
                                body: None,
                            }),
                        });
                    }
                    _ => {}
                }
            }

            // Supertrait bounds (`trait Fn<Args>: FnMut<Args>`) — see
            // `hir::Trait::supertraits`'s own doc comment for why a
            // still-generic `F::Output` projection needs this chain.
            // Dropped (not an error) if a bound doesn't resolve to a real
            // path, same tolerant treatment as everywhere else a bound
            // this checker doesn't act on further is simply skipped.
            let supertraits = def_trait
                .bounds
                .bounds
                .iter()
                .filter_map(|bound| {
                    self.ast_expr_to_hir_path(bound, PathResolutionScope::Type)
                        .ok()
                })
                .collect();

            Ok(hir::Trait {
                generics,
                items,
                supertraits,
            })
        })();

        self.pop_value_scope();
        self.pop_type_scope();

        result
    }
}
