use super::*;

impl HirGenerator {
    /// Transform an AST expression to HIR expression
    pub(super) fn transform_expr_to_hir(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        use ast::Expr;

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        let kind = match ast_expr {
            Expr::Value(value) => self.transform_value_to_hir(value)?,
            Expr::Locator(locator) => hir::ExprKind::Path(
                self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
            ),
            Expr::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            Expr::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            Expr::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            Expr::Select(select) => self.transform_select_to_hir(select)?,
            Expr::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            Expr::Block(block) => self.transform_block_to_hir(block)?,
            Expr::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            Expr::Loop(loop_expr) => self.transform_loop_to_hir(loop_expr)?,
            Expr::While(while_expr) => self.transform_while_to_hir(while_expr)?,
            Expr::Assign(assign) => self.transform_assign_to_hir(assign)?,
            Expr::Paren(paren) => self.transform_paren_to_hir(paren)?,
            Expr::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            Expr::Any(_) => {
                // Handle macro expressions and other "any" expressions
                // For now, return a placeholder boolean literal
                hir::ExprKind::Literal(hir::Lit::Bool(false))
            }
            Expr::FormatString(format_str) => self.transform_format_string_to_hir(format_str)?,
            Expr::StdIoPrintln(println) => self.transform_std_io_println_to_hir(println)?,
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                )));
            }
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    /// Create a main function wrapper for the HIR expression
    pub(super) fn create_main_function(&mut self, body_expr: hir::Expr) -> Result<hir::Function> {
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value: body_expr,
        };

        let sig = hir::FunctionSig {
            name: "main".to_string(),
            inputs: Vec::new(),
            output: hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Tuple(Vec::new()), // Unit type ()
                body.value.span,
            ),
            generics: hir::Generics::default(),
        };

        Ok(hir::Function::new(sig, Some(body), false))
    }

    /// Generate next HIR ID
    pub(super) fn next_id(&mut self) -> hir::HirId {
        let id = self.next_hir_id;
        self.next_hir_id += 1;
        id
    }

    /// Generate next definition ID
    pub(super) fn next_def_id(&mut self) -> hir::DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;
        id
    }

    /// Transform a function definition
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
                name: self.qualify_name(&func.name.name),
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
                    kind: hir::PatKind::Binding(param.name.name.clone()),
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
                name: param.name.name.clone(),
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
                kind: hir::PatKind::Binding("self".to_string()),
            },
            ty,
        })
    }

    pub(super) fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::Impl> {
        self.push_type_scope();
        self.current_type_scope()
            .insert("Self".to_string(), hir::Res::SelfTy);
        let result = (|| {
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
                match item {
                    ast::Item::DefFunction(func) => {
                        let method = self.transform_function(func, Some(self_ty.clone()))?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: method.sig.name.clone(),
                            kind: hir::ImplItemKind::Method(method),
                        });
                    }
                    ast::Item::DefConst(const_item) => {
                        let assoc_const = self.transform_const_def(const_item)?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: const_item.name.name.clone(),
                            kind: hir::ImplItemKind::AssocConst(assoc_const),
                        });
                    }
                    _ => {
                        // Skip unsupported impl items for now
                    }
                }
            }

            Ok(hir::Impl {
                generics: hir::Generics::default(),
                trait_ty,
                self_ty,
                items,
            })
        })();

        self.pop_type_scope();

        result
    }

    /// Transform AST value to HIR expression kind
    pub(super) fn transform_value_to_hir(&mut self, value: &ast::BValue) -> Result<hir::ExprKind> {
        use ast::Value;

        match value.as_ref() {
            Value::Int(i) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(i.value))),
            Value::Bool(b) => Ok(hir::ExprKind::Literal(hir::Lit::Bool(b.value))),
            Value::String(s) => Ok(hir::ExprKind::Literal(hir::Lit::Str(s.value.clone()))),
            Value::Decimal(d) => Ok(hir::ExprKind::Literal(hir::Lit::Float(d.value))),
            Value::Expr(expr) => self.transform_expr_to_hir(expr).map(|e| e.kind),
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented AST value type for HIR transformation: {:?}",
                std::mem::discriminant(value.as_ref())
            ))),
        }
    }

    /// Transform binary operation to HIR
    pub(super) fn transform_binop_to_hir(
        &mut self,
        binop: &ast::ExprBinOp,
    ) -> Result<hir::ExprKind> {
        let left = Box::new(self.transform_expr_to_hir(&binop.lhs)?);
        let right = Box::new(self.transform_expr_to_hir(&binop.rhs)?);
        let op = self.convert_binop_kind(&binop.kind);

        Ok(hir::ExprKind::Binary(op, left, right))
    }

    /// Transform unary operation to HIR
    pub(super) fn transform_unop_to_hir(&mut self, unop: &ast::ExprUnOp) -> Result<hir::ExprKind> {
        let operand = Box::new(self.transform_expr_to_hir(&unop.val)?);
        let op = self.convert_unop_kind(&unop.op);

        Ok(hir::ExprKind::Unary(op, operand))
    }

    /// Transform function call/invoke to HIR
    pub(super) fn transform_invoke_to_hir(
        &mut self,
        invoke: &ast::ExprInvoke,
    ) -> Result<hir::ExprKind> {
        match &invoke.target {
            ast::ExprInvokeTarget::Method(select) => {
                let receiver = self.transform_expr_to_hir(&select.obj)?;
                let mut args = Vec::new();
                for arg in &invoke.args {
                    args.push(self.transform_expr_to_hir(arg)?);
                }
                Ok(hir::ExprKind::MethodCall(
                    Box::new(receiver),
                    select.field.name.clone(),
                    args,
                ))
            }
            ast::ExprInvokeTarget::Function(locator) => {
                if let Some(std_println) = ast::ExprStdIoPrintln::from_invoke(invoke) {
                    return self.transform_std_io_println_to_hir(&std_println);
                }

                let func_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(
                        self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
                    ),
                    span: self.create_span(1),
                };
                let args = self.transform_call_args(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let func_expr = self.transform_expr_to_hir(expr)?;
                let args = self.transform_call_args(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented invoke target type for HIR transformation: {:?}",
                invoke.target
            ))),
        }
    }

    /// Transform field selection to HIR
    pub(super) fn transform_select_to_hir(
        &mut self,
        select: &ast::ExprSelect,
    ) -> Result<hir::ExprKind> {
        let expr = Box::new(self.transform_expr_to_hir(&select.obj)?);
        let field = select.field.name.clone();

        Ok(hir::ExprKind::FieldAccess(expr, field))
    }

    /// Transform struct construction to HIR
    pub(super) fn transform_struct_to_hir(
        &mut self,
        struct_expr: &ast::ExprStruct,
    ) -> Result<hir::ExprKind> {
        let path =
            self.ast_expr_to_hir_path(struct_expr.name.as_ref(), PathResolutionScope::Type)?;

        let fields = struct_expr
            .fields
            .iter()
            .map(|field| {
                let expr = if let Some(value) = field.value.as_ref() {
                    self.transform_expr_to_hir(value)?
                } else {
                    // Shorthand - reference local with same name
                    let res = self.resolve_value_symbol(&field.name.name);
                    hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Path(hir::Path {
                            segments: vec![hir::PathSegment {
                                name: field.name.name.clone(),
                                args: None,
                            }],
                            res,
                        }),
                        span: self.create_span(1),
                    }
                };

                Ok(hir::StructExprField {
                    hir_id: self.next_id(),
                    name: field.name.name.clone(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::ExprKind::Struct(path, fields))
    }

    /// Transform block expression to HIR
    pub(super) fn transform_block_to_hir(
        &mut self,
        block: &ast::ExprBlock,
    ) -> Result<hir::ExprKind> {
        self.push_value_scope();
        let result = (|| {
            let stmts = block
                .stmts
                .iter()
                .map(|stmt| self.transform_block_stmt_to_hir(stmt))
                .collect::<Result<Vec<_>>>()?;

            // For the final expression, check if the last statement is an expression without semicolon
            let expr = if let Some(last_expr) = block.last_expr() {
                Some(Box::new(self.transform_expr_to_hir(last_expr)?))
            } else {
                None
            };

            Ok(hir::ExprKind::Block(hir::Block {
                hir_id: self.next_id(),
                stmts,
                expr,
            }))
        })();
        self.pop_value_scope();
        result
    }

    /// Transform if expression to HIR
    pub(super) fn transform_if_to_hir(&mut self, if_expr: &ast::ExprIf) -> Result<hir::ExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&if_expr.cond)?);
        let then_branch = Box::new(self.transform_expr_to_hir(&if_expr.then)?);
        let else_branch = if let Some(else_expr) = if_expr.elze.as_ref() {
            Some(Box::new(self.transform_expr_to_hir(else_expr)?))
        } else {
            None
        };

        Ok(hir::ExprKind::If(cond, then_branch, else_branch))
    }

    /// Transform loop to HIR
    pub(super) fn transform_loop_to_hir(
        &mut self,
        loop_expr: &ast::ExprLoop,
    ) -> Result<hir::ExprKind> {
        let body_expr = self.transform_expr_to_hir(&loop_expr.body.get())?;
        let body_block = if let hir::ExprKind::Block(block) = body_expr.kind {
            block
        } else {
            // If the body is not a block, wrap it in one
            hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: Some(Box::new(body_expr)),
            }
        };

        Ok(hir::ExprKind::Loop(body_block))
    }

    /// Transform while loop to HIR
    pub(super) fn transform_while_to_hir(
        &mut self,
        while_expr: &ast::ExprWhile,
    ) -> Result<hir::ExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&while_expr.cond.get())?);
        let body_expr = self.transform_expr_to_hir(&while_expr.body.get())?;
        let body_block = if let hir::ExprKind::Block(block) = body_expr.kind {
            block
        } else {
            // If the body is not a block, wrap it in one
            hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: Some(Box::new(body_expr)),
            }
        };

        Ok(hir::ExprKind::While(cond, body_block))
    }

    /// Transform assignment to HIR
    pub(super) fn transform_assign_to_hir(
        &mut self,
        assign: &ast::ExprAssign,
    ) -> Result<hir::ExprKind> {
        let lhs = Box::new(self.transform_expr_to_hir(&assign.target)?);
        let rhs = Box::new(self.transform_expr_to_hir(&assign.value)?);

        Ok(hir::ExprKind::Assign(lhs, rhs))
    }

    /// Transform block statement to HIR (using actual AST types)
    pub(super) fn transform_block_stmt_to_hir(
        &mut self,
        stmt: &ast::BlockStmt,
    ) -> Result<hir::Stmt> {
        let kind = match stmt {
            ast::BlockStmt::Expr(expr_stmt) => {
                hir::StmtKind::Expr(self.transform_expr_to_hir(&expr_stmt.expr)?)
            }
            ast::BlockStmt::Let(let_stmt) => {
                let pat = self.transform_pattern(&let_stmt.pat)?;
                let init = let_stmt
                    .init
                    .as_ref()
                    .map(|v| self.transform_expr_to_hir(v))
                    .transpose()?;

                let local = hir::Local {
                    hir_id: self.next_id(),
                    pat,
                    ty: None, // Type inference will fill this in later
                    init,
                };

                self.register_pattern_bindings(&local.pat);

                hir::StmtKind::Local(local)
            }
            ast::BlockStmt::Item(item) => {
                // Transform items (struct definitions, const declarations, etc.)
                self.transform_item_to_hir_stmt(item)?
            }
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented block statement type for HIR transformation: {:?}",
                    stmt
                )));
            }
        };

        Ok(hir::Stmt {
            hir_id: self.next_id(),
            kind,
        })
    }

    /// Convert AST binary operator to HIR
    pub(super) fn convert_binop_kind(&self, op: &BinOpKind) -> hir::BinOp {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => hir::BinOp::Add,
            BinOpKind::Sub => hir::BinOp::Sub,
            BinOpKind::Mul => hir::BinOp::Mul,
            BinOpKind::Div => hir::BinOp::Div,
            BinOpKind::Mod => hir::BinOp::Rem,
            BinOpKind::Eq => hir::BinOp::Eq,
            BinOpKind::Ne => hir::BinOp::Ne,
            BinOpKind::Lt => hir::BinOp::Lt,
            BinOpKind::Le => hir::BinOp::Le,
            BinOpKind::Gt => hir::BinOp::Gt,
            BinOpKind::Ge => hir::BinOp::Ge,
            BinOpKind::And => hir::BinOp::And,
            BinOpKind::Or => hir::BinOp::Or,
            BinOpKind::BitOr => hir::BinOp::BitOr,
            BinOpKind::BitAnd => hir::BinOp::BitAnd,
            BinOpKind::BitXor => hir::BinOp::BitXor,
        }
    }

    /// Convert AST unary operator to HIR
    pub(super) fn convert_unop_kind(&self, op: &UnOpKind) -> hir::UnOp {
        match op {
            UnOpKind::Neg => hir::UnOp::Neg,
            UnOpKind::Not => hir::UnOp::Not,
            UnOpKind::Deref => hir::UnOp::Deref,
            UnOpKind::Any(_) => {
                // For Any variants, default to Neg as a fallback
                // This handles custom unary operators that don't have direct HIR equivalents
                hir::UnOp::Neg
            }
        }
    }

    /// Transform parentheses expression to HIR (just unwrap the inner expression)
    pub(super) fn transform_paren_to_hir(
        &mut self,
        paren: &ast::ExprParen,
    ) -> Result<hir::ExprKind> {
        // Parentheses don't change semantics, just unwrap the inner expression
        let inner_expr = self.transform_expr_to_hir(&paren.expr)?;
        Ok(inner_expr.kind)
    }

    /// Transform format string to HIR - keep it as FormatString for later const evaluation
    pub(super) fn transform_format_string_to_hir(
        &mut self,
        format_str: &ast::ExprFormatString,
    ) -> Result<hir::ExprKind> {
        self.transform_std_io_println_to_hir(&ast::ExprStdIoPrintln {
            format: format_str.clone(),
            newline: true,
        })
    }

    pub(super) fn transform_std_io_println_to_hir(
        &mut self,
        println: &ast::ExprStdIoPrintln,
    ) -> Result<hir::ExprKind> {
        tracing::debug!(
            "Lowering std::io::println with {} template parts and {} captured args",
            println.format.parts.len(),
            println.format.args.len()
        );

        if !println.format.kwargs.is_empty() {
            return Err(crate::error::optimization_error(
                "Named arguments for println! are not yet supported",
            ));
        }

        let mut parts = Vec::new();
        for part in &println.format.parts {
            let converted = match part {
                ast::FormatTemplatePart::Literal(text) => {
                    hir::FormatTemplatePart::Literal(text.clone())
                }
                ast::FormatTemplatePart::Placeholder(ph) => {
                    let arg_ref = match &ph.arg_ref {
                        ast::FormatArgRef::Implicit => hir::FormatArgRef::Implicit,
                        ast::FormatArgRef::Positional(idx) => hir::FormatArgRef::Positional(*idx),
                        ast::FormatArgRef::Named(name) => hir::FormatArgRef::Named(name.clone()),
                    };

                    hir::FormatTemplatePart::Placeholder(hir::FormatPlaceholder {
                        arg_ref,
                        format_spec: ph.format_spec.clone(),
                    })
                }
            };
            parts.push(converted);
        }

        let mut args = Vec::new();
        for arg in &println.format.args {
            args.push(self.transform_expr_to_hir(arg)?);
        }

        let mut kwargs = Vec::new();
        for kwarg in &println.format.kwargs {
            kwargs.push(hir::FormatKwArg {
                name: kwarg.name.clone(),
                value: self.transform_expr_to_hir(&kwarg.value)?,
            });
        }

        Ok(hir::ExprKind::StdIoPrintln(hir::StdIoPrintln {
            format: hir::FormatString {
                parts,
                args,
                kwargs,
            },
            newline: println.newline,
        }))
    }

    pub(super) fn transform_call_args(&mut self, args: &[ast::Expr]) -> Result<Vec<hir::Expr>> {
        args.iter()
            .map(|arg| self.transform_expr_to_hir(arg))
            .collect()
    }

    pub(super) fn locator_to_hir_path_with_scope(
        &mut self,
        locator: &Locator,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        let segments = match locator {
            Locator::Ident(ident) => vec![self.make_path_segment(&ident.name, None)],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|seg| self.make_path_segment(&seg.name, None))
                .collect(),
            Locator::ParameterPath(param_path) => {
                let mut segs = Vec::new();
                for seg in &param_path.segments {
                    let args = if seg.args.is_empty() {
                        None
                    } else {
                        Some(self.convert_generic_args(&seg.args)?)
                    };
                    segs.push(self.make_path_segment(&seg.ident.name, args));
                }
                segs
            }
        };

        let mut resolved = segments.last().and_then(|segment| match scope {
            PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
            PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
        });

        if resolved.is_none() {
            let canonical = self.canonicalize_segments(&segments);
            resolved = self.lookup_global_res(&canonical, scope);
        }

        Ok(hir::Path {
            segments,
            res: resolved,
        })
    }

    pub(super) fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        match expr {
            ast::Expr::Locator(locator) => self.locator_to_hir_path_with_scope(locator, scope),
            _ => Err(crate::error::optimization_error(format!(
                "Unsupported path expression: {:?}",
                expr
            ))),
        }
    }

    pub(super) fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    pub(super) fn canonicalize_segments(&self, segments: &[hir::PathSegment]) -> Vec<String> {
        let mut result: Vec<String> = Vec::new();
        for segment in segments {
            match segment.name.as_str() {
                "crate" => {
                    result.clear();
                }
                "self" => {
                    result = self.module_path.clone();
                }
                "super" => {
                    if result.is_empty() {
                        let mut parent = self.module_path.clone();
                        if !parent.is_empty() {
                            parent.pop();
                        }
                        result = parent;
                    } else {
                        result.pop();
                    }
                }
                name => result.push(name.to_string()),
            }
        }
        result
    }

    pub(super) fn lookup_global_res(
        &self,
        segments: &[String],
        scope: PathResolutionScope,
    ) -> Option<hir::Res> {
        if segments.is_empty() {
            return None;
        }
        let key = segments.join("::");
        match scope {
            PathResolutionScope::Value => self.lookup_symbol(&key, &self.global_value_defs),
            PathResolutionScope::Type => self.lookup_symbol(&key, &self.global_type_defs),
        }
    }

    pub(super) fn make_path_segment(
        &self,
        name: &str,
        args: Option<hir::GenericArgs>,
    ) -> hir::PathSegment {
        hir::PathSegment {
            name: name.to_string(),
            args,
        }
    }

    pub(super) fn primitive_type_to_hir(&mut self, prim: ast::TypePrimitive) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Primitive(prim),
            Span::new(self.current_file, 0, 0),
        )
    }

    pub(super) fn transform_pattern(&mut self, pat: &Pattern) -> Result<hir::Pat> {
        match pat {
            Pattern::Ident(ident) => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding(ident.ident.name.clone()),
            }),
            Pattern::Wildcard(_) => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Wild,
            }),
            Pattern::Type(pattern_type) => self.transform_pattern(&pattern_type.pat),
            _ => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding("_".to_string()),
            }),
        }
    }

    pub(super) fn register_pattern_bindings(&mut self, pat: &hir::Pat) {
        match &pat.kind {
            hir::PatKind::Binding(name) => {
                self.register_value_local(name, pat.hir_id);
            }
            hir::PatKind::Struct(_, fields) => {
                for field in fields {
                    self.register_pattern_bindings(&field.pat);
                }
            }
            hir::PatKind::Tuple(elements) => {
                for element in elements {
                    self.register_pattern_bindings(element);
                }
            }
            _ => {}
        }
    }

    /// Transform let expression to HIR
    pub(super) fn transform_let_to_hir(
        &mut self,
        let_expr: &ast::ExprLet,
    ) -> Result<hir::ExprKind> {
        let pat = self.transform_pattern(&let_expr.pat)?;
        self.register_pattern_bindings(&pat);
        let init = self.transform_expr_to_hir(&let_expr.expr)?;
        let ty = self.create_unit_type();

        Ok(hir::ExprKind::Let(pat, Box::new(ty), Some(Box::new(init))))
    }
}
