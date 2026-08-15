use super::*;

impl HirGenerator {
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
            let generics = self.transform_generics(&func.sig.generics_params);

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
                function.attrs = func.attrs.clone();
                Ok(function)
            } else {
                let mut function = hir::Function::new(sig, None, func.sig.is_const, false);
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
            let generics = self.transform_generics(&func.sig.generics_params);

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

    pub(super) fn transform_generics(&mut self, params: &[ast::GenericParam]) -> hir::Generics {
        let mut hir_params = Vec::new();
        for param in params {
            let hir_id = self.next_id();
            let def_id = self.next_def_id();
            hir_params.push(hir::GenericParam {
                hir_id,
                def_id,
                name: param.name.clone().into(),
                kind: hir::GenericParamKind::Type { default: None },
            });
            self.register_type_generic(&param.name.name, def_id);
        }

        hir::Generics {
            params: hir_params,
            where_clause: None,
        }
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
            default: None,
        })
    }

    fn is_unimplemented_type_expr(&self, ty: &hir::TypeExpr) -> bool {
        let hir::TypeExprKind::Path(path) = &ty.kind else {
            return false;
        };
        let Some(hir::Res::Def(def_id)) = path.res else {
            return false;
        };
        self.unimplemented_type_def_ids.contains(&def_id)
    }

    pub(super) fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::Impl> {
        self.push_type_scope();
        self.current_type_scope()
            .insert("Self".to_string(), hir::Res::SelfTy);
        // `Self` in type position (`-> Self`, `&Self`) resolves through the
        // type scope above. `Self { x, y }` struct-literal construction in
        // method bodies resolves through the *value* namespace instead
        // (mirroring how real struct names are registered in both
        // namespaces, `register_type_def` + `register_value_def`) — needs
        // its own registration here or it stays unresolved.
        self.push_value_scope();
        self.current_value_scope()
            .insert("Self".to_string(), hir::Res::SelfTy);
        let result = (|| {
            // Register impl generics in the current type scope.
            let generics = self.transform_generics(&impl_block.generics_params);
            let self_ty_ast = ast::Ty::expr(impl_block.self_ty.clone());
            let self_ty = self.transform_type_to_hir(&self_ty_ast)?;
            let trait_ty = if let Some(trait_name) = &impl_block.trait_ty {
                Some(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(
                        self.name_to_hir_path_with_scope(trait_name, PathResolutionScope::Type)?,
                    ),
                    Span::new(self.current_file, 0, 0),
                ))
            } else {
                None
            };
            let stub_methods = attrs_has_name(&impl_block.attrs, "unimplemented")
                || self.is_unimplemented_type_expr(&self_ty);
            let impl_op_class = fp_core::intrinsics::extract_op_attr(&impl_block.attrs, "class");

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
                        let method_def_id = self.def_id_for_item(item);
                        if let Some(tag) = fp_core::intrinsics::extract_op_attr(&func.attrs, "method") {
                            let op = impl_op_class
                                .as_deref()
                                .and_then(|class| fp_core::intrinsics::OpKind::from_class_and_member(class, &tag));
                            if let Some(op) = op {
                                self.op_defs.insert(method_def_id, op);
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
                        items.push(hir::ImplItem {
                            def_id: self.next_def_id(),
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
                            def_id: self.next_def_id(),
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
                let trait_name = match trait_name {
                    ast::Name::Ident(ident) => ident.name.clone(),
                    ast::Name::Path(path) => path
                        .segments
                        .last()
                        .map(|seg| seg.name.clone())
                        .unwrap_or_default(),
                    ast::Name::ParameterPath(path) => path
                        .segments
                        .last()
                        .map(|seg| seg.ident.name.clone())
                        .unwrap_or_default(),
                };
                if let Some(trait_def) = self.trait_defs.get(&trait_name) {
                    let trait_items = trait_def.items.clone();
                    // Synthesize default trait methods into the impl if they are missing.
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
                        items.push(hir::ImplItem {
                            def_id: self.next_def_id(),
                            hir_id: self.next_id(),
                            name: method.sig.name.clone(),
                            kind: hir::ImplItemKind::Method(method),
                        });
                    }
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

        result
    }
}
