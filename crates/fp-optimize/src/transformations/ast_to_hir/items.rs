use super::*;

impl HirGenerator {
    pub(super) fn create_main_function(&mut self, body_expr: hir::Expr) -> Result<hir::Function> {
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value: body_expr,
        };

        let sig = hir::FunctionSig {
            name: hir::Symbol::new("main"),
            inputs: Vec::new(),
            output: hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Tuple(Vec::new()),
                body.value.span,
            ),
            generics: hir::Generics::default(),
        };

        Ok(hir::Function::new(sig, Some(body), false))
    }

    pub fn transform_function(
        &mut self,
        func: &ast::ItemDefFunction,
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
                name: hir::Symbol::new(self.qualify_name(&func.name.name)),
                inputs: params.clone(),
                output: output.clone(),
                generics,
            };

            let body_expr = self.transform_expr_to_hir(&func.body)?;
            let body = hir::Body {
                hir_id: self.next_id(),
                params,
                value: body_expr,
            };

            Ok(hir::Function::new(sig, Some(body), false))
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
            hir_params.push(hir::GenericParam {
                hir_id,
                name: param.name.clone().into(),
                kind: hir::GenericParamKind::Type { default: None },
            });
            self.register_type_generic(&param.name.name, hir_id);
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
        })
    }

    pub(super) fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::Impl> {
        self.push_type_scope();
        self.current_type_scope()
            .insert("Self".to_string(), hir::Res::SelfTy);
        let result = (|| {
            // Register impl generics in the current type scope.
            let generics = self.transform_generics(&impl_block.generics_params);
            let self_ty_ast = ast::Ty::expr(impl_block.self_ty.clone());
            let self_ty = self.transform_type_to_hir(&self_ty_ast)?;
            let trait_ty = if let Some(trait_locator) = &impl_block.trait_ty {
                Some(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(self.locator_to_hir_path_with_scope(
                        trait_locator,
                        PathResolutionScope::Type,
                    )?),
                    Span::new(self.current_file, 0, 0),
                ))
            } else {
                None
            };

            let mut items = Vec::new();
            for item in &impl_block.items {
                match item.kind() {
                    ast::ItemKind::DefFunction(func) => {
                        let method = self.transform_function(func, Some(self_ty.clone()))?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: method.sig.name.clone(),
                            kind: hir::ImplItemKind::Method(method),
                        });
                    }
                    ast::ItemKind::DefConst(const_item) => {
                        let assoc_const = self.transform_const_def(const_item)?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: const_item.name.clone().into(),
                            kind: hir::ImplItemKind::AssocConst(assoc_const),
                        });
                    }
                    _ => {}
                }
            }

            Ok(hir::Impl {
                generics,
                trait_ty,
                self_ty,
                items,
            })
        })();

        self.pop_type_scope();

        result
    }
}
