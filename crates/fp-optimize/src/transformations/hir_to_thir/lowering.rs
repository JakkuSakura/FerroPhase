use super::*;
use fp_core::intrinsics::IntrinsicCallKind;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(super) struct FunctionNaming {
    name: String,
    path: Vec<String>,
    def_id: Option<hir_types::DefId>,
}

impl FunctionNaming {
    fn new(name: String, path: Vec<String>, def_id: Option<hir_types::DefId>) -> Self {
        Self { name, path, def_id }
    }
}

fn describe_type_expr(ty: &hir::TypeExpr) -> String {
    use hir::TypeExprKind;

    match &ty.kind {
        TypeExprKind::Path(path) => {
            let parts: Vec<String> = path.segments.iter().map(|seg| seg.name.clone()).collect();
            if parts.is_empty() {
                "anonymous".to_string()
            } else {
                parts.join("::")
            }
        }
        TypeExprKind::Primitive(prim) => format!("{prim:?}"),
        TypeExprKind::Tuple(_) => "tuple".to_string(),
        TypeExprKind::Array(elem, _) => format!("array_of_{}", describe_type_expr(elem)),
        TypeExprKind::Ptr(inner) => format!("ptr_to_{}", describe_type_expr(inner)),
        TypeExprKind::Ref(inner) => format!("ref_to_{}", describe_type_expr(inner)),
        TypeExprKind::Never => "never".to_string(),
        TypeExprKind::Infer => "infer".to_string(),
    }
}

impl ThirGenerator {
    pub(super) fn transform_item(&mut self, hir_item: hir::Item) -> Result<thir::Item> {
        let thir_id = self.next_id();
        let def_id = hir_item.def_id as hir_types::DefId;
        let span = hir_item.span;

        let (kind, ty) = match hir_item.kind {
            hir::ItemKind::Function(func) => {
                let fn_name = func.sig.name.clone();
                let naming = FunctionNaming::new(fn_name.clone(), vec![fn_name], Some(def_id));
                let thir_func = self.transform_function(func, naming)?;
                let func_ty = self.get_function_type(def_id, &thir_func)?;
                (thir::ItemKind::Function(thir_func), func_ty)
            }
            hir::ItemKind::Struct(struct_def) => {
                let struct_ty = self
                    .type_context
                    .make_struct_ty_by_id(def_id, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type());
                let thir_struct = self.transform_struct(struct_def)?;
                (thir::ItemKind::Struct(thir_struct), struct_ty)
            }
            hir::ItemKind::Const(const_def) => {
                // Record initializer for inlining and transform const
                self.const_init_map
                    .insert(def_id, const_def.body.value.clone());
                let thir_const = self.transform_const(Some(def_id), const_def)?;
                let const_ty = thir_const.ty.clone();
                (thir::ItemKind::Const(thir_const), const_ty)
            }
            hir::ItemKind::Impl(impl_block) => {
                let thir_impl = self.transform_impl(impl_block)?;
                let self_ty = thir_impl.self_ty.clone();
                (thir::ItemKind::Impl(thir_impl), self_ty)
            }
        };

        Ok(thir::Item {
            thir_id,
            kind,
            ty,
            span,
        })
    }

    /// Transform HIR function to THIR
    pub(super) fn transform_function(
        &mut self,
        hir_func: hir::Function,
        naming: FunctionNaming,
    ) -> Result<thir::Function> {
        if let Some(ref body) = hir_func.body {
            let mut inferencer = TypeInferencer::new(self, self.current_self_ty.clone());
            inferencer.infer_function(body)?;
        }

        let inputs = hir_func
            .sig
            .inputs
            .iter()
            .map(|param| self.hir_ty_to_ty(&param.ty))
            .collect::<Result<Vec<_>>>()?;

        let output = self.hir_ty_to_ty(&hir_func.sig.output)?;

        let sig = thir::FunctionSig {
            inputs,
            output,
            c_variadic: false,
        };

        let body_id = if let Some(hir_body) = hir_func.body {
            let body_id = self.next_body_id();
            let thir_body = self.transform_body(hir_body)?;
            self.body_map.insert(body_id, thir_body);
            Some(body_id)
        } else {
            None
        };

        Ok(thir::Function {
            name: naming.name,
            path: naming.path,
            def_id: naming.def_id,
            sig,
            body_id,
            is_const: hir_func.is_const,
        })
    }

    pub(super) fn transform_impl(&mut self, hir_impl: hir::Impl) -> Result<thir::Impl> {
        let self_scope_name = describe_type_expr(&hir_impl.self_ty);
        let self_ty = self.hir_ty_to_ty(&hir_impl.self_ty)?;
        let saved_self_ty = self.current_self_ty.clone();
        self.current_self_ty = Some(self_ty.clone());

        let mut items = Vec::new();
        for item in hir_impl.items {
            match item.kind {
                hir::ImplItemKind::Method(method) => {
                    let method_name = method.sig.name.clone();
                    let path = vec![self_scope_name.clone(), method_name.clone()];
                    let naming = FunctionNaming::new(method_name, path, None);
                    let thir_method = self.transform_function(method, naming)?;
                    items.push(thir::ImplItem {
                        thir_id: self.next_id(),
                        kind: thir::ImplItemKind::Method(thir_method),
                    });
                }
                hir::ImplItemKind::AssocConst(const_item) => {
                    let thir_const = self.transform_const(None, const_item)?;
                    items.push(thir::ImplItem {
                        thir_id: self.next_id(),
                        kind: thir::ImplItemKind::AssocConst(thir_const),
                    });
                }
            }
        }

        self.current_self_ty = saved_self_ty;

        Ok(thir::Impl {
            self_ty,
            items,
            trait_ref: hir_impl.trait_ty.map(|_| ()),
        })
    }

    /// Transform HIR struct to THIR
    pub(super) fn transform_struct(&mut self, hir_struct: hir::Struct) -> Result<thir::Struct> {
        let fields = hir_struct
            .fields
            .into_iter()
            .map(|field| self.transform_struct_field(field))
            .collect::<Result<Vec<_>>>()?;

        Ok(thir::Struct {
            fields,
            variant_data: thir::VariantData::Struct(Vec::new(), false),
        })
    }

    /// Transform HIR struct field to THIR
    pub(super) fn transform_struct_field(
        &mut self,
        hir_field: hir::StructField,
    ) -> Result<thir::StructField> {
        let thir_id = self.next_id();
        let ty = self.hir_ty_to_ty(&hir_field.ty)?;
        let vis = self.transform_visibility(hir_field.vis);

        Ok(thir::StructField { thir_id, ty, vis })
    }

    /// Transform HIR const to THIR
    pub(super) fn transform_const(
        &mut self,
        def_id: Option<hir_types::DefId>,
        hir_const: hir::Const,
    ) -> Result<thir::Const> {
        let ty = self.hir_ty_to_ty(&hir_const.ty)?;
        let body_id = self.next_body_id();
        let thir_body = self.transform_body(hir_const.body)?;
        self.body_map.insert(body_id, thir_body);
        if let Some(id) = def_id {
            self.const_symbols.insert(id, body_id);
        }

        Ok(thir::Const {
            ty,
            body_id,
            def_id,
        })
    }

    /// Transform HIR body to THIR
    pub(super) fn transform_body(&mut self, hir_body: hir::Body) -> Result<thir::Body> {
        let hir::Body {
            hir_id: _body_hir_id,
            params: hir_params,
            value: hir_value,
        } = hir_body;

        let saved_scopes = mem::take(&mut self.local_scopes);
        let saved_locals = mem::take(&mut self.current_locals);
        let saved_next_local_id = self.next_local_id;

        self.local_scopes.push(HashMap::new());
        self.local_const_inits.push(HashMap::new());
        self.local_const_inits.push(HashMap::new());
        self.current_locals = Vec::new();
        self.next_local_id = 0;

        let params = hir_params
            .into_iter()
            .map(|param| {
                let hir_ty = self.hir_ty_to_ty(&param.ty)?;
                let pat = match param.pat.kind {
                    hir::PatKind::Binding { ref name, mutable } => {
                        Some(self.create_binding_pattern(name.clone(), hir_ty.clone(), mutable))
                    }
                    _ => None,
                };
                Ok(thir::Param { ty: hir_ty, pat })
            })
            .collect::<Result<Vec<_>>>()?;

        let value = self.transform_expr(hir_value)?;

        let locals = mem::take(&mut self.current_locals);

        // Restore generator state for surrounding bodies
        self.local_scopes = saved_scopes;
        self.current_locals = saved_locals;
        self.next_local_id = saved_next_local_id;

        Ok(thir::Body {
            params,
            value,
            locals,
        })
    }

    /// Transform HIR expression to THIR with type checking
    pub(super) fn transform_expr(&mut self, hir_expr: hir::Expr) -> Result<thir::Expr> {
        let thir_id = self.next_id();
        let expected_ty = self.lookup_expr_ty(hir_expr.hir_id).cloned();

        let (kind, ty) = match hir_expr.kind {
            hir::ExprKind::Literal(lit) => {
                let (thir_lit, ty) = self.transform_literal(lit, expected_ty.as_ref())?;
                (thir::ExprKind::Literal(thir_lit), ty)
            }
            hir::ExprKind::Path(path) => {
                let base_name = path
                    .segments
                    .last()
                    .map(|seg| seg.name.clone())
                    .unwrap_or_default();

                if let Some((local_id, local_ty)) = self.lookup_local_binding(&base_name) {
                    (thir::ExprKind::VarRef { id: local_id }, local_ty)
                } else {
                    let ty = self.infer_path_type(&path)?;
                    let (def_id_opt, qualified_name, _, _substs) = self.path_to_type_info(&path)?;
                    let resolved_def_id = def_id_opt
                        .or_else(|| self.type_context.lookup_value_def_id(&qualified_name));
                    let display_name = qualified_name.clone();

                    if let Some(def_id) = resolved_def_id {
                        if let Some(init_expr) = self.const_init_map.get(&def_id) {
                            let inlined = self.transform_expr(init_expr.clone())?;
                            (inlined.kind, inlined.ty)
                        } else {
                            (
                                thir::ExprKind::Path(thir::ItemRef {
                                    name: display_name,
                                    def_id: Some(def_id),
                                }),
                                ty,
                            )
                        }
                    } else if let Some(init_expr) = self.lookup_local_const_init(&base_name) {
                        let inlined = self.transform_expr(init_expr)?;
                        (inlined.kind, inlined.ty)
                    } else {
                        (
                            thir::ExprKind::Path(thir::ItemRef {
                                name: display_name,
                                def_id: None,
                            }),
                            ty,
                        )
                    }
                }
            }
            hir::ExprKind::Binary(op, left, right) => {
                let left_thir = self.transform_expr(*left)?;
                let right_thir = self.transform_expr(*right)?;
                let op_thir = self.transform_binary_op(op);

                // Constant folding for simple integer ops when both sides are literals
                match (&left_thir.kind, &right_thir.kind, &op_thir) {
                    (
                        thir::ExprKind::Literal(thir::Lit::Int(a, _)),
                        thir::ExprKind::Literal(thir::Lit::Int(b, _)),
                        thir::BinOp::Add | thir::BinOp::Sub | thir::BinOp::Mul | thir::BinOp::Div,
                    ) => {
                        let result_hir_ty = expected_ty
                            .clone()
                            .unwrap_or_else(|| self.create_i32_type());
                        let thir_int_ty = match &result_hir_ty.kind {
                            hir_types::TyKind::Int(int_ty) => match int_ty {
                                hir_types::IntTy::Isize => thir::IntTy::Isize,
                                hir_types::IntTy::I8 => thir::IntTy::I8,
                                hir_types::IntTy::I16 => thir::IntTy::I16,
                                hir_types::IntTy::I32 => thir::IntTy::I32,
                                hir_types::IntTy::I64 => thir::IntTy::I64,
                                hir_types::IntTy::I128 => thir::IntTy::I128,
                            },
                            _ => thir::IntTy::I64,
                        };
                        let val = match op_thir {
                            thir::BinOp::Add => a + b,
                            thir::BinOp::Sub => a - b,
                            thir::BinOp::Mul => a * b,
                            thir::BinOp::Div => {
                                if *b != 0 {
                                    a / b
                                } else {
                                    *a
                                }
                            }
                            _ => unreachable!(),
                        };
                        (
                            thir::ExprKind::Literal(thir::Lit::Int(val, thir_int_ty)),
                            result_hir_ty,
                        )
                    }
                    _ => {
                        let result_ty = expected_ty
                            .clone()
                            .unwrap_or_else(|| self.create_i32_type());
                        (
                            thir::ExprKind::Binary(
                                op_thir,
                                Box::new(left_thir),
                                Box::new(right_thir),
                            ),
                            result_ty,
                        )
                    }
                }
            }
            hir::ExprKind::Call(func, args) => {
                let func_thir = self.transform_expr(*func)?;
                let args_thir: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;

                let return_ty = self.infer_call_return_type(&func_thir, &args_thir)?;

                (
                    thir::ExprKind::Call {
                        fun: Box::new(func_thir),
                        args: args_thir,
                        from_hir_call: true,
                    },
                    return_ty,
                )
            }
            hir::ExprKind::Return(expr) => {
                let return_ty = self.create_never_type();
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                (
                    thir::ExprKind::Return {
                        value: expr_thir.map(Box::new),
                    },
                    return_ty,
                )
            }
            hir::ExprKind::Block(block) => {
                let block_thir = self.transform_block(block)?;
                let block_ty = self.infer_block_type(&block_thir)?;
                (thir::ExprKind::Block(block_thir), block_ty)
            }
            hir::ExprKind::IntrinsicCall(call) => {
                use fp_core::intrinsics::IntrinsicCallPayload;

                let payload = match &call.payload {
                    IntrinsicCallPayload::Format { template } => IntrinsicCallPayload::Format {
                        template: self.transform_format_string(template.clone())?,
                    },
                    IntrinsicCallPayload::Args { args } => IntrinsicCallPayload::Args {
                        args: args
                            .iter()
                            .map(|arg| self.transform_expr(arg.clone()))
                            .collect::<Result<Vec<_>>>()?,
                    },
                };

                let ty = match call.kind {
                    IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                        self.create_unit_type()
                    }
                    IntrinsicCallKind::Len => self.create_usize_type(),
                };

                (
                    thir::ExprKind::IntrinsicCall(thir::ThirIntrinsicCall {
                        kind: call.kind,
                        payload,
                    }),
                    ty,
                )
            }
            hir::ExprKind::Unary(op, expr) => {
                let expr_thir = self.transform_expr(*expr)?;
                let op_thir = self.transform_unary_op(op)?;
                let result_ty = self.check_unary_op_type(&expr_thir.ty, &op_thir)?;
                (
                    thir::ExprKind::Unary(op_thir, Box::new(expr_thir)),
                    result_ty,
                )
            }
            hir::ExprKind::MethodCall(receiver, method_name, args) => {
                // Convert method call to function call for simplicity
                let receiver_thir = self.transform_expr(*receiver)?;
                let args_thir: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                let return_ty =
                    self.infer_method_call_return_type(&receiver_thir, &method_name, &args_thir)?;

                // Create a function call with method name
                let func_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::Path(thir::ItemRef {
                        name: method_name.clone(),
                        def_id: self.type_context.lookup_value_def_id(&method_name),
                    }),
                    ty: self.create_unit_type(), // Simplified
                    span: Span::new(0, 0, 0),
                };

                let mut all_args = vec![receiver_thir];
                all_args.extend(args_thir);

                (
                    thir::ExprKind::Call {
                        fun: Box::new(func_expr),
                        args: all_args,
                        from_hir_call: true,
                    },
                    return_ty,
                )
            }
            hir::ExprKind::FieldAccess(expr, field_name) => {
                // If base is a const struct, inline the specific field initializer
                if let hir::ExprKind::Path(ref p) = expr.kind {
                    let (base_def_id_opt, qualified_name, base_name, _) =
                        self.path_to_type_info(p)?;
                    let init_expr = base_def_id_opt
                        .or_else(|| self.type_context.lookup_value_def_id(&qualified_name))
                        .or_else(|| self.type_context.lookup_value_def_id(&base_name))
                        .and_then(|id| self.const_init_map.get(&id));

                    if let Some(init) = init_expr {
                        if let hir::ExprKind::Struct(_path, fields) = &init.kind {
                            if let Some(field) =
                                fields.iter().find(|f| f.name.to_string() == field_name)
                            {
                                let thir_expr = self.transform_expr(field.expr.clone())?;
                                return Ok(thir_expr);
                            }
                        }
                    } else if let Some(init) = self.lookup_local_const_init(&base_name) {
                        if let hir::ExprKind::Struct(_path, fields) = &init.kind {
                            if let Some(field) =
                                fields.iter().find(|f| f.name.to_string() == field_name)
                            {
                                let thir_expr = self.transform_expr(field.expr.clone())?;
                                return Ok(thir_expr);
                            }
                        }
                    }
                }
                // Fallback: regular field selection
                let hir_base = *expr;
                let expr_thir = self.transform_expr(hir_base)?;
                if let Some((idx, field_ty)) = self
                    .type_context
                    .lookup_field_info(&expr_thir.ty, &field_name)
                {
                    (
                        thir::ExprKind::Field {
                            base: Box::new(expr_thir),
                            field_idx: idx,
                        },
                        field_ty,
                    )
                } else {
                    (
                        thir::ExprKind::Field {
                            base: Box::new(expr_thir),
                            field_idx: 0,
                        },
                        hir_types::Ty::int(hir_types::IntTy::I32),
                    )
                }
            }
            hir::ExprKind::Struct(path, _fields) => {
                let (def_id_opt, qualified_name, base_name, substs) =
                    self.path_to_type_info(&path)?;
                let struct_ty = def_id_opt
                    .or_else(|| self.type_context.lookup_struct_def_id(&qualified_name))
                    .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
                    .and_then(|id| self.type_context.make_struct_ty_by_id(id, substs))
                    .unwrap_or_else(|| self.create_unit_type());
                (
                    thir::ExprKind::Literal(thir::Lit::Int(0, thir::IntTy::I32)),
                    struct_ty,
                )
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let cond_thir = self.transform_expr(*cond)?;
                let then_thir = self.transform_expr(*then_expr)?;
                let else_thir = else_expr.map(|e| self.transform_expr(*e)).transpose()?;
                let result_ty = if let Some(ref else_expr) = else_thir {
                    self.unify_types(&then_thir.ty, &else_expr.ty)?
                } else {
                    self.create_unit_type()
                };
                (
                    thir::ExprKind::If {
                        cond: Box::new(cond_thir),
                        then: Box::new(then_thir),
                        else_opt: else_thir.map(Box::new),
                    },
                    result_ty,
                )
            }
            hir::ExprKind::Let(pat, ty_expr, init) => {
                let mut init_thir = init.map(|e| self.transform_expr(*e)).transpose()?;
                let explicit_ty_opt = if matches!(&ty_expr.kind, hir::TypeExprKind::Infer) {
                    None
                } else {
                    Some(self.hir_ty_to_ty(&ty_expr)?)
                };
                let binding_ty = if let Some(explicit_ty) = explicit_ty_opt {
                    explicit_ty
                } else if let Some(expr) = init_thir.as_ref() {
                    expr.ty.clone()
                } else {
                    self.create_unit_type()
                };

                if let Some(init_expr) = init_thir.as_mut() {
                    init_expr.ty = binding_ty.clone();
                }

                let pattern = match pat.kind {
                    hir::PatKind::Binding { name, mutable } => {
                        self.create_binding_pattern(name.clone(), binding_ty.clone(), mutable)
                    }
                    _ => self.create_wildcard_pattern(),
                };

                let expr_value = init_thir.map(Box::new).unwrap_or_else(|| {
                    Box::new(thir::Expr {
                        thir_id: self.next_id(),
                        kind: thir::ExprKind::Literal(thir::Lit::Int(0, thir::IntTy::I32)),
                        ty: self.create_i32_type(),
                        span: Span::new(0, 0, 0),
                    })
                });

                (
                    thir::ExprKind::Let {
                        expr: expr_value,
                        pat: pattern,
                    },
                    self.create_unit_type(),
                )
            }
            hir::ExprKind::Assign(lhs, rhs) => {
                let lhs_thir = self.transform_expr(*lhs)?;
                let mut rhs_thir = self.transform_expr(*rhs)?;
                let unit_ty = self.create_unit_type();

                rhs_thir.ty = lhs_thir.ty.clone();

                (
                    thir::ExprKind::Assign {
                        lhs: Box::new(lhs_thir),
                        rhs: Box::new(rhs_thir),
                    },
                    unit_ty,
                )
            }
            hir::ExprKind::Break(expr) => {
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                let never_ty = self.create_never_type();
                (
                    thir::ExprKind::Break {
                        value: expr_thir.map(Box::new),
                    },
                    never_ty,
                )
            }
            hir::ExprKind::Continue => {
                let never_ty = self.create_never_type();
                (thir::ExprKind::Continue, never_ty)
            }
            hir::ExprKind::Loop(block) => {
                let block_thir = self.transform_block(block)?;
                let never_ty = self.create_never_type();
                // Convert block to expression
                let block_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::Block(block_thir),
                    ty: never_ty.clone(),
                    span: Span::new(0, 0, 0),
                };
                (
                    thir::ExprKind::Loop {
                        body: Box::new(block_expr),
                    },
                    never_ty,
                )
            }
            hir::ExprKind::While(cond, block) => {
                // Convert while loop to infinite loop with conditional break
                let cond_thir = self.transform_expr(*cond)?;
                let block_thir = self.transform_block(block)?;
                let unit_ty = self.create_unit_type();
                let cond_span = cond_thir.span;

                let block_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::Block(block_thir),
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                let break_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::Break { value: None },
                    ty: self.create_never_type(),
                    span: cond_span,
                };

                let if_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::If {
                        cond: Box::new(cond_thir),
                        then: Box::new(block_expr.clone()),
                        else_opt: Some(Box::new(break_expr)),
                    },
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                let loop_body_block = thir::Block {
                    targeted_by_break: true,
                    region: 0,
                    span: Span::new(0, 0, 0),
                    stmts: vec![thir::Stmt {
                        kind: thir::StmtKind::Expr(if_expr),
                    }],
                    expr: None,
                    safety_mode: thir::BlockSafetyMode::Safe,
                };

                let loop_body_expr = thir::Expr {
                    thir_id: self.next_id(),
                    kind: thir::ExprKind::Block(loop_body_block),
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                (
                    thir::ExprKind::Loop {
                        body: Box::new(loop_body_expr),
                    },
                    unit_ty,
                )
            }
        };

        Ok(thir::Expr {
            thir_id,
            kind,
            ty,
            span: hir_expr.span,
        })
    }

    /// Transform HIR block to THIR
    pub(super) fn transform_block(&mut self, hir_block: hir::Block) -> Result<thir::Block> {
        self.local_scopes.push(HashMap::new());

        let stmts = match hir_block
            .stmts
            .into_iter()
            .map(|stmt| self.transform_stmt(stmt))
            .collect::<Result<Vec<_>>>()
        {
            Ok(stmts) => stmts,
            Err(err) => {
                self.local_scopes.pop();
                self.local_const_inits.pop();
                return Err(err);
            }
        };

        let expr = hir_block
            .expr
            .map(|e| self.transform_expr(*e))
            .transpose()?;

        self.local_scopes.pop();
        self.local_const_inits.pop();

        Ok(thir::Block {
            targeted_by_break: false,
            region: 0,                // Simplified scope handling
            span: Span::new(0, 0, 0), // Will be updated with proper span
            stmts,
            expr: expr.map(Box::new),
            safety_mode: thir::BlockSafetyMode::Safe,
        })
    }

    /// Transform HIR statement to THIR
    pub(super) fn transform_stmt(&mut self, hir_stmt: hir::Stmt) -> Result<thir::Stmt> {
        let kind = match hir_stmt.kind {
            hir::StmtKind::Expr(expr) => {
                let thir_expr = self.transform_expr(expr)?;
                thir::StmtKind::Expr(thir_expr)
            }
            hir::StmtKind::Semi(expr) => {
                let mut thir_expr = self.transform_expr(expr)?;
                thir_expr.ty = self.create_unit_type();
                thir::StmtKind::Expr(thir_expr)
            }
            hir::StmtKind::Local(local) => {
                let mut initializer = match &local.init {
                    Some(init) => Some(self.transform_expr(init.clone())?),
                    None => None,
                };

                let binding_ty = self
                    .lookup_pattern_ty(local.pat.hir_id)
                    .cloned()
                    .or_else(|| initializer.as_ref().map(|expr| expr.ty.clone()))
                    .or_else(|| {
                        local
                            .ty
                            .as_ref()
                            .and_then(|ty_expr| self.hir_ty_to_ty(ty_expr).ok())
                    })
                    .unwrap_or_else(|| self.create_unit_type());

                if let Some(init_expr) = initializer.as_mut() {
                    init_expr.ty = binding_ty.clone();
                }

                let pattern = match &local.pat.kind {
                    hir::PatKind::Binding { name, mutable } => {
                        let pat =
                            self.create_binding_pattern(name.clone(), binding_ty.clone(), *mutable);
                        match &pat.kind {
                            thir::PatKind::Binding { .. } => {}
                            other => {
                                let _ = other;
                            }
                        }
                        pat
                    }
                    _ => self.create_wildcard_pattern(),
                };

                thir::StmtKind::Let {
                    remainder_scope: 0,
                    init_scope: 0,
                    pattern,
                    initializer,
                    lint_level: 0,
                }
            }
            hir::StmtKind::Item(item) => match item.kind {
                hir::ItemKind::Const(const_def) => self.transform_const_stmt(const_def)?,
                hir::ItemKind::Struct(_) => thir::StmtKind::Expr(self.create_unit_expr()),
                _ => {
                    return Err(crate::error::optimization_error(
                        "Unsupported HIR statement during THIR lowering",
                    ));
                }
            },
        };

        Ok(thir::Stmt { kind })
    }

    fn transform_const_stmt(&mut self, const_def: hir::Const) -> Result<thir::StmtKind> {
        if !const_def.body.params.is_empty() {
            return Err(crate::error::optimization_error(
                "Const items with parameters are not supported in block scope",
            ));
        }

        let binding_ty = self.hir_ty_to_ty(&const_def.ty)?;
        let hir::Body {
            hir_id: _,
            params: _,
            value,
        } = const_def.body;

        if let Some(scope) = self.local_const_inits.last_mut() {
            scope.insert(const_def.name.clone(), value.clone());
        }

        let mut initializer = self.transform_expr(value)?;
        initializer.ty = binding_ty.clone();

        let pattern =
            self.create_binding_pattern(const_def.name.clone(), binding_ty.clone(), false);

        Ok(thir::StmtKind::Let {
            remainder_scope: 0,
            init_scope: 0,
            pattern,
            initializer: Some(initializer),
            lint_level: 0,
        })
    }

    /// Transform HIR literal to THIR with type inference
    pub(super) fn transform_literal(
        &mut self,
        hir_lit: hir::Lit,
        expected_ty: Option<&hir_types::Ty>,
    ) -> Result<(thir::Lit, hir_types::Ty)> {
        use hir_types::TyKind;
        let (thir_lit, ty) = match hir_lit {
            hir::Lit::Bool(b) => (thir::Lit::Bool(b), hir_types::Ty::bool()),
            hir::Lit::Integer(i) => {
                let inferred = expected_ty.and_then(|ty| match &ty.kind {
                    TyKind::Int(int_ty) => Some(*int_ty),
                    _ => None,
                });
                let int_ty = inferred.unwrap_or(hir_types::IntTy::I64);
                let thir_ty = match int_ty {
                    hir_types::IntTy::Isize => thir::IntTy::Isize,
                    hir_types::IntTy::I8 => thir::IntTy::I8,
                    hir_types::IntTy::I16 => thir::IntTy::I16,
                    hir_types::IntTy::I32 => thir::IntTy::I32,
                    hir_types::IntTy::I64 => thir::IntTy::I64,
                    hir_types::IntTy::I128 => thir::IntTy::I128,
                };
                (
                    thir::Lit::Int(i as i128, thir_ty),
                    hir_types::Ty::int(int_ty),
                )
            }
            hir::Lit::Float(f) => {
                let inferred = expected_ty.and_then(|ty| match &ty.kind {
                    TyKind::Float(float_ty) => Some(*float_ty),
                    _ => None,
                });
                let float_ty = inferred.unwrap_or(hir_types::FloatTy::F64);
                let thir_ty = match float_ty {
                    hir_types::FloatTy::F32 => thir::FloatTy::F32,
                    hir_types::FloatTy::F64 => thir::FloatTy::F64,
                };
                (thir::Lit::Float(f, thir_ty), hir_types::Ty::float(float_ty))
            }
            hir::Lit::Str(s) => (thir::Lit::Str(s), self.create_string_type()),
            hir::Lit::Char(c) => (thir::Lit::Char(c), hir_types::Ty::char()),
        };
        Ok((thir_lit, ty))
    }

    /// Transform HIR binary operator to THIR
    pub(super) fn transform_binary_op(&self, hir_op: hir::BinOp) -> thir::BinOp {
        match hir_op {
            hir::BinOp::Add => thir::BinOp::Add,
            hir::BinOp::Sub => thir::BinOp::Sub,
            hir::BinOp::Mul => thir::BinOp::Mul,
            hir::BinOp::Div => thir::BinOp::Div,
            hir::BinOp::Rem => thir::BinOp::Rem,
            hir::BinOp::BitXor => thir::BinOp::BitXor,
            hir::BinOp::BitAnd => thir::BinOp::BitAnd,
            hir::BinOp::BitOr => thir::BinOp::BitOr,
            hir::BinOp::Shl => thir::BinOp::Shl,
            hir::BinOp::Shr => thir::BinOp::Shr,
            hir::BinOp::Eq => thir::BinOp::Eq,
            hir::BinOp::Ne => thir::BinOp::Ne,
            hir::BinOp::Lt => thir::BinOp::Lt,
            hir::BinOp::Le => thir::BinOp::Le,
            hir::BinOp::Gt => thir::BinOp::Gt,
            hir::BinOp::Ge => thir::BinOp::Ge,
            _ => thir::BinOp::Add, // Fallback
        }
    }

    /// Convert HIR type to hir_types::Ty
    pub(super) fn hir_ty_to_ty(&mut self, hir_ty: &hir::TypeExpr) -> Result<hir_types::Ty> {
        match &hir_ty.kind {
            hir::TypeExprKind::Primitive(prim) => Ok(self.primitive_ty_to_ty(prim)),
            hir::TypeExprKind::Path(path) => {
                if matches!(path.res, Some(hir::Res::SelfTy)) {
                    if let Some(self_ty) = &self.current_self_ty {
                        return Ok(self_ty.clone());
                    }
                }
                let (def_id_opt, qualified_name, base_name, substs) =
                    self.path_to_type_info(path)?;

                if let Some(primitive) = self.make_primitive_ty(&base_name) {
                    return Ok(primitive);
                }

                if let Some(def_id) = def_id_opt {
                    if let Some(ty) = self
                        .type_context
                        .make_struct_ty_by_id(def_id, substs.clone())
                    {
                        return Ok(ty);
                    }

                    if let Some(const_ty) = self.type_context.lookup_const_type(def_id) {
                        return Ok(const_ty.clone());
                    }

                    if self
                        .type_context
                        .lookup_function_signature(def_id)
                        .is_some()
                    {
                        return Ok(hir_types::Ty {
                            kind: hir_types::TyKind::FnDef(def_id, substs),
                        });
                    }
                }

                if let Some(struct_id) = self
                    .type_context
                    .lookup_struct_def_id(&qualified_name)
                    .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
                {
                    if let Some(ty) = self
                        .type_context
                        .make_struct_ty_by_id(struct_id, substs.clone())
                    {
                        return Ok(ty);
                    }
                }

                let lookup_name = qualified_name.clone();
                let stub_id = self.type_context.ensure_struct_stub(&lookup_name);
                Ok(self
                    .type_context
                    .make_struct_ty_by_id(stub_id, substs)
                    .unwrap_or_else(|| self.create_unit_type()))
            }
            hir::TypeExprKind::Tuple(elements) => {
                let tys = elements
                    .iter()
                    .map(|ty| Ok(Box::new(self.hir_ty_to_ty(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir_types::Ty {
                    kind: hir_types::TyKind::Tuple(tys),
                })
            }
            hir::TypeExprKind::Ref(inner) => {
                let inner_ty = self.hir_ty_to_ty(inner)?;
                Ok(hir_types::Ty {
                    kind: hir_types::TyKind::Ref(
                        hir_types::Region::ReStatic,
                        Box::new(inner_ty),
                        hir_types::Mutability::Not,
                    ),
                })
            }
            hir::TypeExprKind::Array(inner, _) => {
                let elem_ty = self.hir_ty_to_ty(inner)?;
                Ok(hir_types::Ty {
                    kind: hir_types::TyKind::Array(
                        Box::new(elem_ty),
                        hir_types::ConstKind::Value(hir_types::ConstValue::ZeroSized),
                    ),
                })
            }
            hir::TypeExprKind::Ptr(inner) => {
                let pointee = self.hir_ty_to_ty(inner)?;
                Ok(hir_types::Ty {
                    kind: hir_types::TyKind::RawPtr(hir_types::TypeAndMut {
                        ty: Box::new(pointee),
                        mutbl: hir_types::Mutability::Not,
                    }),
                })
            }
            hir::TypeExprKind::Never => Ok(hir_types::Ty::never()),
            hir::TypeExprKind::Infer => Ok(hir_types::Ty {
                kind: hir_types::TyKind::Infer(hir_types::InferTy::FreshTy(0)),
            }),
        }
    }

    pub(super) fn primitive_ty_to_ty(&mut self, prim: &TypePrimitive) -> hir_types::Ty {
        match prim {
            TypePrimitive::Bool => hir_types::Ty::bool(),
            TypePrimitive::Char => hir_types::Ty::char(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => hir_types::Ty::int(hir_types::IntTy::I8),
                TypeInt::I16 => hir_types::Ty::int(hir_types::IntTy::I16),
                TypeInt::I32 => hir_types::Ty::int(hir_types::IntTy::I32),
                TypeInt::I64 => hir_types::Ty::int(hir_types::IntTy::I64),
                TypeInt::U8 => hir_types::Ty::uint(hir_types::UintTy::U8),
                TypeInt::U16 => hir_types::Ty::uint(hir_types::UintTy::U16),
                TypeInt::U32 => hir_types::Ty::uint(hir_types::UintTy::U32),
                TypeInt::U64 => hir_types::Ty::uint(hir_types::UintTy::U64),
                TypeInt::BigInt => hir_types::Ty::int(hir_types::IntTy::I128),
            },
            TypePrimitive::Decimal(dec_ty) => match dec_ty {
                DecimalType::F32 => hir_types::Ty::float(hir_types::FloatTy::F32),
                DecimalType::F64 => hir_types::Ty::float(hir_types::FloatTy::F64),
                DecimalType::BigDecimal | DecimalType::Decimal { .. } => {
                    hir_types::Ty::float(hir_types::FloatTy::F64)
                }
            },
            TypePrimitive::String => {
                let stub = self.type_context.ensure_struct_stub("String");
                self.type_context
                    .make_struct_ty_by_id(stub, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type())
            }
            TypePrimitive::List => {
                let stub = self.type_context.ensure_struct_stub("List");
                self.type_context
                    .make_struct_ty_by_id(stub, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type())
            }
        }
    }

    /// Infer type from HIR path
    pub(super) fn infer_path_type(&mut self, path: &hir::Path) -> Result<hir_types::Ty> {
        let (def_id_opt, qualified_name, base_name, substs) = self.path_to_type_info(path)?;

        if let Some(def_id) = def_id_opt {
            if let Some(const_ty) = self.type_context.lookup_const_type(def_id) {
                return Ok(const_ty.clone());
            }
            if let Some(struct_ty) = self
                .type_context
                .make_struct_ty_by_id(def_id, substs.clone())
            {
                return Ok(struct_ty);
            }
            if self
                .type_context
                .lookup_function_signature(def_id)
                .is_some()
            {
                return Ok(hir_types::Ty {
                    kind: hir_types::TyKind::FnDef(def_id, substs),
                });
            }
        }

        if let Some(primitive) = self.make_primitive_ty(&base_name) {
            return Ok(primitive);
        }

        if let Some(struct_id) = self
            .type_context
            .lookup_struct_def_id(&qualified_name)
            .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
        {
            if let Some(struct_ty) = self
                .type_context
                .make_struct_ty_by_id(struct_id, substs.clone())
            {
                return Ok(struct_ty);
            }
        }

        Ok(hir_types::Ty::int(hir_types::IntTy::I32))
    }

    /// Infer return type of function call
    pub(super) fn infer_call_return_type(
        &self,
        func: &thir::Expr,
        _args: &[thir::Expr],
    ) -> Result<hir_types::Ty> {
        if let thir::ExprKind::Path(item_ref) = &func.kind {
            if let Some(def_id) = item_ref.def_id {
                if let Some(sig) = self.type_context.lookup_function_signature(def_id) {
                    return Ok((*sig.output).clone());
                }
            } else if let Some(def_id) = self.type_context.lookup_value_def_id(&item_ref.name) {
                if let Some(sig) = self.type_context.lookup_function_signature(def_id) {
                    return Ok((*sig.output).clone());
                }
            }
        }
        Ok(self.create_unit_type())
    }

    /// Infer type of block expression
    pub(super) fn infer_block_type(&self, block: &thir::Block) -> Result<hir_types::Ty> {
        if let Some(expr) = &block.expr {
            Ok(expr.ty.clone())
        } else {
            Ok(self.create_unit_type())
        }
    }

    /// Get function type
    pub(super) fn get_function_type(
        &self,
        def_id: hir_types::DefId,
        _func: &thir::Function,
    ) -> Result<hir_types::Ty> {
        Ok(hir_types::Ty {
            kind: hir_types::TyKind::FnDef(def_id, Vec::new()),
        })
    }

    /// Transform visibility
    pub(super) fn transform_visibility(&self, hir_vis: hir::Visibility) -> thir::Visibility {
        match hir_vis {
            hir::Visibility::Public => thir::Visibility::Public,
            hir::Visibility::Private => thir::Visibility::Inherited,
        }
    }

    // Helper methods for creating common types
    pub(super) fn create_unit_type(&self) -> hir_types::Ty {
        hir_types::Ty {
            kind: hir_types::TyKind::Tuple(Vec::new()),
        }
    }

    pub(super) fn create_never_type(&self) -> hir_types::Ty {
        hir_types::Ty::never()
    }

    pub(super) fn create_i32_type(&self) -> hir_types::Ty {
        hir_types::Ty::int(hir_types::IntTy::I32)
    }

    pub(super) fn create_usize_type(&self) -> hir_types::Ty {
        hir_types::Ty::uint(hir_types::UintTy::Usize)
    }

    pub(super) fn create_string_type(&self) -> hir_types::Ty {
        // Simplified string type representation
        hir_types::Ty {
            kind: hir_types::TyKind::RawPtr(hir_types::TypeAndMut {
                ty: Box::new(hir_types::Ty::int(hir_types::IntTy::I8)),
                mutbl: hir_types::Mutability::Not,
            }),
        }
    }

    fn transform_format_string(&mut self, format: hir::FormatString) -> Result<thir::FormatString> {
        if !format.kwargs.is_empty() {
            return Err(crate::error::optimization_error(
                "Named arguments for println! are not yet supported during lowering",
            ));
        }

        let parts = format
            .parts
            .into_iter()
            .map(|part| self.transform_format_part(part))
            .collect::<Result<Vec<_>>>()?;

        let args = format
            .args
            .into_iter()
            .map(|arg| self.transform_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(thir::FormatString {
            parts,
            args,
            kwargs: Vec::new(),
        })
    }

    fn transform_format_part(
        &self,
        part: hir::FormatTemplatePart,
    ) -> Result<thir::FormatTemplatePart> {
        Ok(match part {
            hir::FormatTemplatePart::Literal(text) => thir::FormatTemplatePart::Literal(text),
            hir::FormatTemplatePart::Placeholder(placeholder) => {
                thir::FormatTemplatePart::Placeholder(thir::FormatPlaceholder {
                    arg_ref: match placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => thir::FormatArgRef::Implicit,
                        hir::FormatArgRef::Positional(idx) => thir::FormatArgRef::Positional(idx),
                        hir::FormatArgRef::Named(name) => {
                            return Err(crate::error::optimization_error(format!(
                                "Named argument '{}' is not yet supported during lowering",
                                name
                            )));
                        }
                    },
                    format_spec: placeholder.format_spec,
                })
            }
        })
    }

    pub(super) fn create_wildcard_pattern(&mut self) -> thir::Pat {
        thir::Pat {
            thir_id: self.next_id(),
            kind: thir::PatKind::Wild,
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        }
    }

    /// Transform unary operator
    pub(super) fn transform_unary_op(&self, op: hir::UnOp) -> Result<thir::UnOp> {
        let lowered = match op {
            hir::UnOp::Not => thir::UnOp::Not,
            hir::UnOp::Neg => thir::UnOp::Neg,
            hir::UnOp::Deref => {
                return Err(crate::error::optimization_error(
                    "Unary deref is not supported during HIR→THIR lowering",
                ))
            }
        };

        Ok(lowered)
    }

    /// Check type for unary operations
    pub(super) fn check_unary_op_type(
        &self,
        operand_ty: &hir_types::Ty,
        _op: &thir::UnOp,
    ) -> Result<hir_types::Ty> {
        // Simplified type checking - return the operand type
        Ok(operand_ty.clone())
    }

    /// Infer method call return type
    pub(super) fn infer_method_call_return_type(
        &self,
        receiver: &thir::Expr,
        method_name: &str,
        _args: &[thir::Expr],
    ) -> Result<hir_types::Ty> {
        if let Some(sig) = self
            .type_context
            .lookup_method_signature(&receiver.ty, method_name)
        {
            return Ok((*sig.output).clone());
        }
        Err(crate::error::optimization_error(format!(
            "Method '{}' not found for receiver type {:?}",
            method_name, receiver.ty
        )))
    }

    /// Unify two types
    pub(super) fn unify_types(
        &self,
        ty1: &hir_types::Ty,
        ty2: &hir_types::Ty,
    ) -> Result<hir_types::Ty> {
        // Simplified type unification - return the first type
        if ty1 == ty2 {
            Ok(ty1.clone())
        } else {
            Err(crate::error::optimization_error(format!(
                "Type mismatch during unification: {:?} vs {:?}",
                ty1, ty2
            )))
        }
    }

    fn create_binding_pattern(
        &mut self,
        name: String,
        ty: hir_types::Ty,
        mutable: bool,
    ) -> thir::Pat {
        let local_id = self.allocate_local(&name, ty.clone());
        thir::Pat {
            thir_id: self.next_id(),
            kind: thir::PatKind::Binding {
                mutability: if mutable {
                    thir::Mutability::Mut
                } else {
                    thir::Mutability::Not
                },
                name,
                mode: thir::BindingMode::ByValue,
                var: local_id,
                ty: ty.clone(),
            },
            ty,
            span: Span::new(0, 0, 0),
        }
    }

    fn create_unit_expr(&mut self) -> thir::Expr {
        thir::Expr {
            thir_id: self.next_id(),
            kind: thir::ExprKind::Block(thir::Block {
                targeted_by_break: false,
                region: 0,
                span: Span::new(0, 0, 0),
                stmts: Vec::new(),
                expr: None,
                safety_mode: thir::BlockSafetyMode::Safe,
            }),
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        }
    }

    fn allocate_local(&mut self, name: &str, ty: hir_types::Ty) -> thir::LocalId {
        let local_id = self.next_local_id;
        self.next_local_id += 1;

        self.current_locals.push(thir::LocalDecl {
            ty: ty.clone(),
            source_info: Span::new(0, 0, 0),
            internal: false,
        });

        if self.local_scopes.is_empty() {
            self.local_scopes.push(HashMap::new());
        }

        if let Some(scope) = self.local_scopes.last_mut() {
            scope.insert(name.to_string(), (local_id, ty));
        }

        local_id
    }

    fn lookup_local_binding(&self, name: &str) -> Option<(thir::LocalId, hir_types::Ty)> {
        for scope in self.local_scopes.iter().rev() {
            if let Some((local_id, ty)) = scope.get(name) {
                return Some((*local_id, ty.clone()));
            }
        }
        None
    }
}
