use super::*;

impl ThirGenerator {
    pub(super) fn transform_item(&mut self, hir_item: hir::Item) -> Result<thir::Item> {
        let thir_id = self.next_id();
        let def_id = hir_item.def_id as types::DefId;
        let span = hir_item.span;

        let (kind, ty) = match hir_item.kind {
            hir::ItemKind::Function(func) => {
                let thir_func = self.transform_function(func)?;
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
    pub(super) fn transform_function(&mut self, hir_func: hir::Function) -> Result<thir::Function> {
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
            sig,
            body_id,
            is_const: hir_func.is_const,
        })
    }

    pub(super) fn transform_impl(&mut self, hir_impl: hir::Impl) -> Result<thir::Impl> {
        let self_ty = self.hir_ty_to_ty(&hir_impl.self_ty)?;

        let mut items = Vec::new();
        for item in hir_impl.items {
            match item.kind {
                hir::ImplItemKind::Method(method) => {
                    let thir_method = self.transform_function(method)?;
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
        def_id: Option<types::DefId>,
        hir_const: hir::Const,
    ) -> Result<thir::Const> {
        let ty = self.hir_ty_to_ty(&hir_const.ty)?;
        let body_id = self.next_body_id();
        let thir_body = self.transform_body(hir_const.body)?;
        self.body_map.insert(body_id, thir_body);
        if let Some(id) = def_id {
            self.const_symbols.insert(id, body_id);
        }

        Ok(thir::Const { ty, body_id })
    }

    /// Transform HIR body to THIR
    pub(super) fn transform_body(&mut self, hir_body: hir::Body) -> Result<thir::Body> {
        let saved_bindings = mem::take(&mut self.binding_inits);
        let value = self.transform_expr(hir_body.value)?;

        let params = hir_body
            .params
            .into_iter()
            .map(|param| {
                let ty = self.hir_ty_to_ty(&param.ty)?;
                Ok(thir::Param {
                    ty,
                    pat: None, // Simplified for now
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let body = thir::Body {
            params,
            value,
            locals: Vec::new(), // Will be populated during MIR generation
        };

        self.binding_inits = saved_bindings;

        Ok(body)
    }

    /// Transform HIR expression to THIR with type checking
    pub(super) fn transform_expr(&mut self, hir_expr: hir::Expr) -> Result<thir::Expr> {
        let thir_id = self.next_id();

        let (kind, ty) = match hir_expr.kind {
            hir::ExprKind::Literal(lit) => {
                let (thir_lit, ty) = self.transform_literal(lit)?;
                (thir::ExprKind::Literal(thir_lit), ty)
            }
            hir::ExprKind::Path(path) => {
                let ty = self.infer_path_type(&path)?;
                let (def_id_opt, qualified_name, base_name, _substs) =
                    self.path_to_type_info(&path)?;
                let resolved_def_id = def_id_opt
                    .or_else(|| self.type_context.lookup_value_def_id(&qualified_name))
                    .or_else(|| self.type_context.lookup_value_def_id(&base_name));
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
                } else if let Some(init_expr) = self.binding_inits.get(&base_name) {
                    let inlined = self.transform_expr(init_expr.clone())?;
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
                            thir::ExprKind::Literal(thir::Lit::Int(val, thir::IntTy::I32)),
                            self.create_i32_type(),
                        )
                    }
                    _ => {
                        // Type checking: ensure operands are compatible
                        let result_ty =
                            self.check_binary_op_types(&left_thir.ty, &right_thir.ty, &op_thir)?;
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
            hir::ExprKind::StdIoPrintln(println) => {
                let func_expr = self.make_std_io_println_path_expr();

                let mut args = Vec::new();
                args.push(self.make_string_literal_expr(println.template.clone()));
                for arg in println.args {
                    args.push(self.transform_expr(arg)?);
                }

                (
                    thir::ExprKind::Call {
                        fun: Box::new(func_expr),
                        args,
                        from_hir_call: true,
                    },
                    self.create_unit_type(),
                )
            }
            hir::ExprKind::Unary(op, expr) => {
                let expr_thir = self.transform_expr(*expr)?;
                let op_thir = self.transform_unary_op(op);
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
                        .and_then(|id| self.const_init_map.get(&id))
                        .or_else(|| self.binding_inits.get(&base_name));

                    if let Some(init) = init_expr {
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
                        types::Ty::int(types::IntTy::I32),
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
            hir::ExprKind::Let(pat, _ty, init) => {
                let init_thir = init.map(|e| self.transform_expr(*e)).transpose()?;
                let unit_ty = self.create_unit_type();
                if let Some(init_expr) = init_thir {
                    (
                        thir::ExprKind::Let {
                            expr: Box::new(init_expr),
                            pat: self.transform_pattern(pat)?,
                        },
                        unit_ty,
                    )
                } else {
                    // No initializer, return unit literal
                    (
                        thir::ExprKind::Literal(thir::Lit::Int(0, thir::IntTy::I32)),
                        unit_ty,
                    )
                }
            }
            hir::ExprKind::Assign(lhs, rhs) => {
                let lhs_thir = self.transform_expr(*lhs)?;
                let rhs_thir = self.transform_expr(*rhs)?;
                let unit_ty = self.create_unit_type();
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
        let stmts = hir_block
            .stmts
            .into_iter()
            .map(|stmt| self.transform_stmt(stmt))
            .collect::<Result<Vec<_>>>()?;

        let expr = hir_block
            .expr
            .map(|e| self.transform_expr(*e))
            .transpose()?;

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
                let thir_expr = self.transform_expr(expr)?;
                thir::StmtKind::Expr(thir_expr)
            }
            hir::StmtKind::Local(local) => {
                // Record initializer for inlining when encountering Path(name)
                if let hir::PatKind::Binding(name) = &local.pat.kind {
                    if let Some(init) = &local.init {
                        self.binding_inits.insert(name.to_string(), init.clone());
                    }
                }
                // Lower to THIR Let with transformed initializer, so evaluation order is preserved
                let init_expr = match &local.init {
                    Some(init) => Some(self.transform_expr(init.clone())?),
                    None => None,
                };
                thir::StmtKind::Let {
                    remainder_scope: 0,
                    init_scope: 0,
                    pattern: self.create_wildcard_pattern(),
                    initializer: init_expr,
                    lint_level: 0,
                }
            }
            _ => thir::StmtKind::Expr(thir::Expr {
                thir_id: self.next_id(),
                kind: thir::ExprKind::Literal(thir::Lit::Int(0, thir::IntTy::I32)),
                ty: self.create_i32_type(),
                span: Span::new(0, 0, 0),
            }),
        };

        Ok(thir::Stmt { kind })
    }

    /// Transform HIR literal to THIR with type inference
    pub(super) fn transform_literal(
        &mut self,
        hir_lit: hir::Lit,
    ) -> Result<(thir::Lit, types::Ty)> {
        let (thir_lit, ty) = match hir_lit {
            hir::Lit::Bool(b) => (thir::Lit::Bool(b), types::Ty::bool()),
            hir::Lit::Integer(i) => (
                thir::Lit::Int(i as i128, thir::IntTy::I32),
                types::Ty::int(types::IntTy::I32),
            ),
            hir::Lit::Float(f) => (
                thir::Lit::Float(f, thir::FloatTy::F64),
                types::Ty::float(types::FloatTy::F64),
            ),
            hir::Lit::Str(s) => (thir::Lit::Str(s), self.create_string_type()),
            hir::Lit::Char(c) => (thir::Lit::Char(c), types::Ty::char()),
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

    /// Convert HIR type to types::Ty
    pub(super) fn hir_ty_to_ty(&mut self, hir_ty: &hir::Ty) -> Result<types::Ty> {
        match &hir_ty.kind {
            hir::TyKind::Primitive(prim) => Ok(self.primitive_ty_to_ty(prim)),
            hir::TyKind::Path(path) => {
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
                        return Ok(types::Ty::new(types::TyKind::FnDef(def_id, substs)));
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
            hir::TyKind::Tuple(elements) => {
                let tys = elements
                    .iter()
                    .map(|ty| Ok(Box::new(self.hir_ty_to_ty(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(types::Ty::new(types::TyKind::Tuple(tys)))
            }
            hir::TyKind::Ref(inner) => {
                let inner_ty = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::Ref(
                    types::Region::ReStatic,
                    Box::new(inner_ty),
                    types::Mutability::Not,
                )))
            }
            hir::TyKind::Array(inner, _) => {
                let elem_ty = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::Array(
                    Box::new(elem_ty),
                    types::ConstKind::Value(types::ConstValue::ZeroSized),
                )))
            }
            hir::TyKind::Ptr(inner) => {
                let pointee = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::RawPtr(types::TypeAndMut {
                    ty: Box::new(pointee),
                    mutbl: types::Mutability::Not,
                })))
            }
            hir::TyKind::Never => Ok(types::Ty::never()),
            hir::TyKind::Infer => Ok(types::Ty::new(types::TyKind::Infer(
                types::InferTy::FreshTy(0),
            ))),
        }
    }

    pub(super) fn primitive_ty_to_ty(&mut self, prim: &TypePrimitive) -> types::Ty {
        match prim {
            TypePrimitive::Bool => types::Ty::bool(),
            TypePrimitive::Char => types::Ty::char(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => types::Ty::int(types::IntTy::I8),
                TypeInt::I16 => types::Ty::int(types::IntTy::I16),
                TypeInt::I32 => types::Ty::int(types::IntTy::I32),
                TypeInt::I64 => types::Ty::int(types::IntTy::I64),
                TypeInt::U8 => types::Ty::uint(types::UintTy::U8),
                TypeInt::U16 => types::Ty::uint(types::UintTy::U16),
                TypeInt::U32 => types::Ty::uint(types::UintTy::U32),
                TypeInt::U64 => types::Ty::uint(types::UintTy::U64),
                TypeInt::BigInt => types::Ty::int(types::IntTy::I128),
            },
            TypePrimitive::Decimal(dec_ty) => match dec_ty {
                DecimalType::F32 => types::Ty::float(types::FloatTy::F32),
                DecimalType::F64 => types::Ty::float(types::FloatTy::F64),
                DecimalType::BigDecimal | DecimalType::Decimal { .. } => {
                    types::Ty::float(types::FloatTy::F64)
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

    /// Type checking for binary operations
    pub(super) fn check_binary_op_types(
        &self,
        left_ty: &types::Ty,
        right_ty: &types::Ty,
        _op: &thir::BinOp,
    ) -> Result<types::Ty> {
        // Simplified type checking - assume same types for operands
        if left_ty == right_ty {
            Ok(left_ty.clone())
        } else {
            // Try to find a common type
            Ok(left_ty.clone())
        }
    }

    /// Infer type from HIR path
    pub(super) fn infer_path_type(&mut self, path: &hir::Path) -> Result<types::Ty> {
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
                return Ok(types::Ty::new(types::TyKind::FnDef(def_id, substs)));
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

        Ok(types::Ty::int(types::IntTy::I32))
    }

    /// Infer return type of function call
    pub(super) fn infer_call_return_type(
        &self,
        func: &thir::Expr,
        _args: &[thir::Expr],
    ) -> Result<types::Ty> {
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
    pub(super) fn infer_block_type(&self, block: &thir::Block) -> Result<types::Ty> {
        if let Some(expr) = &block.expr {
            Ok(expr.ty.clone())
        } else {
            Ok(self.create_unit_type())
        }
    }

    /// Get function type
    pub(super) fn get_function_type(
        &self,
        def_id: types::DefId,
        _func: &thir::Function,
    ) -> Result<types::Ty> {
        Ok(types::Ty::new(types::TyKind::FnDef(def_id, Vec::new())))
    }

    /// Transform visibility
    pub(super) fn transform_visibility(&self, hir_vis: hir::Visibility) -> thir::Visibility {
        match hir_vis {
            hir::Visibility::Public => thir::Visibility::Public,
            hir::Visibility::Private => thir::Visibility::Inherited,
        }
    }

    // Helper methods for creating common types
    pub(super) fn create_unit_type(&self) -> types::Ty {
        types::Ty::new(types::TyKind::Tuple(Vec::new()))
    }

    pub(super) fn create_never_type(&self) -> types::Ty {
        types::Ty::never()
    }

    pub(super) fn create_i32_type(&self) -> types::Ty {
        types::Ty::int(types::IntTy::I32)
    }

    pub(super) fn create_string_type(&self) -> types::Ty {
        // Simplified string type representation
        types::Ty::new(types::TyKind::RawPtr(types::TypeAndMut {
            ty: Box::new(types::Ty::int(types::IntTy::I8)),
            mutbl: types::Mutability::Not,
        }))
    }

    fn make_string_literal_expr(&mut self, template: String) -> thir::Expr {
        thir::Expr {
            thir_id: self.next_id(),
            kind: thir::ExprKind::Literal(thir::Lit::Str(template)),
            ty: self.create_string_type(),
            span: Span::new(0, 0, 0),
        }
    }

    fn make_std_io_println_path_expr(&mut self) -> thir::Expr {
        let name = "std::io::println".to_string();
        let def_id = self.type_context.lookup_value_def_id(&name);
        thir::Expr {
            thir_id: self.next_id(),
            kind: thir::ExprKind::Path(thir::ItemRef { name, def_id }),
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        }
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
    pub(super) fn transform_unary_op(&self, op: hir::UnOp) -> thir::UnOp {
        match op {
            hir::UnOp::Not => thir::UnOp::Not,
            hir::UnOp::Neg => thir::UnOp::Neg,
            hir::UnOp::Deref => thir::UnOp::Not, // Deref not available, use Not as fallback
        }
    }

    /// Check type for unary operations
    pub(super) fn check_unary_op_type(
        &self,
        operand_ty: &types::Ty,
        _op: &thir::UnOp,
    ) -> Result<types::Ty> {
        // Simplified type checking - return the operand type
        Ok(operand_ty.clone())
    }

    /// Infer method call return type
    pub(super) fn infer_method_call_return_type(
        &self,
        receiver: &thir::Expr,
        method_name: &str,
        _args: &[thir::Expr],
    ) -> Result<types::Ty> {
        if let Some(sig) = self
            .type_context
            .lookup_method_signature(&receiver.ty, method_name)
        {
            return Ok((*sig.output).clone());
        }
        Ok(self.create_unit_type())
    }

    /// Unify two types
    pub(super) fn unify_types(&self, ty1: &types::Ty, ty2: &types::Ty) -> Result<types::Ty> {
        // Simplified type unification - return the first type
        if ty1 == ty2 {
            Ok(ty1.clone())
        } else {
            Ok(ty1.clone())
        }
    }

    /// Transform pattern
    pub(super) fn transform_pattern(&mut self, _pat: hir::Pat) -> Result<thir::Pat> {
        Ok(thir::Pat {
            thir_id: self.next_id(),
            kind: thir::PatKind::Wild, // Simplified
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        })
    }
}
