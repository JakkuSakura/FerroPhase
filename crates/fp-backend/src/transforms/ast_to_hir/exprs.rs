use super::*;
use fp_core::intrinsics::IntrinsicCallKind;

struct EnumerateLoopSpec {
    base_segments: Vec<ast::Ident>,
    index_ident: ast::Ident,
    value_ident: ast::Ident,
}

struct IterLoopSpec {
    base_segments: Vec<ast::Ident>,
    value_ident: ast::Ident,
}

impl HirGenerator {
    /// Transform an AST expression to HIR expression
    pub(super) fn transform_expr_to_hir(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        use ast::ExprKind;

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        let kind = match ast_expr.kind() {
            ExprKind::Value(value) => self.transform_value_to_hir(value)?,
            ExprKind::Locator(locator) => hir::ExprKind::Path(
                self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
            ),
            ExprKind::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            ExprKind::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            ExprKind::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            ExprKind::Select(select) => self.transform_select_to_hir(select)?,
            ExprKind::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            ExprKind::Block(block) => self.transform_block_to_hir(block)?,
            ExprKind::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            ExprKind::Match(match_expr) => self.transform_match_to_hir(match_expr)?,
            ExprKind::Loop(loop_expr) => self.transform_loop_to_hir(loop_expr)?,
            ExprKind::While(while_expr) => self.transform_while_to_hir(while_expr)?,
            ExprKind::Assign(assign) => self.transform_assign_to_hir(assign)?,
            ExprKind::Paren(paren) => self.transform_paren_to_hir(paren)?,
            ExprKind::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            ExprKind::Array(array_expr) => self.transform_array_to_hir(array_expr)?,
            ExprKind::ArrayRepeat(array_repeat) => {
                self.transform_array_repeat_to_hir(array_repeat)?
            }
            ExprKind::Range(_range) => {
                if self.error_tolerance {
                    self.add_warning(Diagnostic::warning(
                        "range expressions are only supported in for loops and slicing; treating as empty array".to_string(),
                    ));
                    hir::ExprKind::Array(Vec::new())
                } else {
                    return Err(crate::error::optimization_error(
                        "range expressions are only supported in for loops and slicing",
                    ));
                }
            }
            ExprKind::Index(index_expr) => {
                if let ast::ExprKind::Range(range) = index_expr.index.kind() {
                    if range.step.is_some() {
                        self.add_warning(Diagnostic::warning(
                            "range steps are not supported in slicing; ignoring step".to_string(),
                        ));
                    }
                    let base_expr = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let start_expr = match range.start.as_ref() {
                        Some(expr) => self.transform_expr_to_hir(expr.as_ref())?,
                        None => hir::Expr {
                            hir_id: self.next_id(),
                            kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
                            span: self.create_span(1),
                        },
                    };
                    let mut end_expr = match range.end.as_ref() {
                        Some(expr) => self.transform_expr_to_hir(expr.as_ref())?,
                        None => {
                            let len_call = hir::IntrinsicCallExpr {
                                kind: IntrinsicCallKind::Len,
                                callargs: vec![hir::CallArg {
                                    name: hir::Symbol::new("base"),
                                    value: base_expr.clone(),
                                }],
                            };
                            hir::Expr {
                                hir_id: self.next_id(),
                                kind: hir::ExprKind::IntrinsicCall(len_call),
                                span: self.create_span(1),
                            }
                        }
                    };
                    if matches!(range.limit, ast::ExprRangeLimit::Inclusive) {
                        end_expr = hir::Expr {
                            hir_id: self.next_id(),
                            kind: hir::ExprKind::Binary(
                                hir::BinOp::Add,
                                Box::new(end_expr),
                                Box::new(hir::Expr {
                                    hir_id: self.next_id(),
                                    kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
                                    span: self.create_span(1),
                                }),
                            ),
                            span: self.create_span(1),
                        };
                    }
                    let call = hir::IntrinsicCallExpr {
                        kind: IntrinsicCallKind::Slice,
                        callargs: vec![
                            hir::CallArg {
                                name: hir::Symbol::new("base"),
                                value: base_expr,
                            },
                            hir::CallArg {
                                name: hir::Symbol::new("start"),
                                value: start_expr,
                            },
                            hir::CallArg {
                                name: hir::Symbol::new("end"),
                                value: end_expr,
                            },
                        ],
                    };
                    hir::ExprKind::IntrinsicCall(call)
                } else {
                    let base = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let index = self.transform_expr_to_hir(index_expr.index.as_ref())?;
                    hir::ExprKind::Index(Box::new(base), Box::new(index))
                }
            }
            ExprKind::Quote(_quote) => {
                return Err(crate::error::optimization_error(
                    "quote expressions should be removed by const-eval",
                ));
            }
            ExprKind::Splice(_splice) => {
                if self.error_tolerance {
                    let block = hir::Block {
                        hir_id: self.next_id(),
                        stmts: Vec::new(),
                        expr: None,
                    };
                    hir::ExprKind::Block(block)
                } else {
                    return Err(crate::error::optimization_error(
                        "splice expressions should be removed by const-eval",
                    ));
                }
            }
            ExprKind::Try(expr_try) => {
                let inner_expr = self.transform_expr_to_hir(expr_try.expr.as_ref())?;
                if self.error_tolerance {
                    return Ok(hir::Expr {
                        hir_id,
                        kind: inner_expr.kind,
                        span,
                    });
                }
                return Err(crate::error::optimization_error(
                    "`?` operator lowering not implemented",
                ));
            }
            ExprKind::Await(expr_await) => {
                let inner_expr = self.transform_expr_to_hir(expr_await.base.as_ref())?;
                if self.error_tolerance {
                    return Ok(hir::Expr {
                        hir_id,
                        kind: inner_expr.kind,
                        span,
                    });
                }
                return Err(crate::error::optimization_error(
                    "`await` lowering not implemented",
                ));
            }
            ExprKind::Async(async_expr) => {
                let inner_expr = self.transform_expr_to_hir(async_expr.expr.as_ref())?;
                if self.error_tolerance {
                    return Ok(hir::Expr {
                        hir_id,
                        kind: inner_expr.kind,
                        span,
                    });
                }
                return Err(crate::error::optimization_error(
                    "`async` lowering not implemented",
                ));
            }
            ExprKind::For(for_expr) => {
                let kind = self.transform_for_to_hir(for_expr)?;
                return Ok(hir::Expr { hir_id, kind, span });
            }
            ExprKind::Closure(_closure) => {
                if self.error_tolerance {
                    self.add_warning(
                        Diagnostic::warning(
                            "closure lowering is not supported; substituting unit".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT),
                    );
                    let block = hir::Block {
                        hir_id: self.next_id(),
                        stmts: Vec::new(),
                        expr: None,
                    };
                    return Ok(hir::Expr {
                        hir_id,
                        kind: hir::ExprKind::Block(block),
                        span,
                    });
                } else {
                    return Err(crate::error::optimization_error(
                        "closure lowering not implemented",
                    ));
                }
            }
            ExprKind::Cast(cast_expr) => {
                let operand = self.transform_expr_to_hir(cast_expr.expr.as_ref())?;
                let ty = self.transform_type_to_hir(&cast_expr.ty)?;
                hir::ExprKind::Cast(Box::new(operand), Box::new(ty))
            }
            ExprKind::Any(any) => {
                if let Some(expr) = any.downcast_ref::<ast::Expr>() {
                    let lowered = self.transform_expr_to_hir(expr)?;
                    lowered.kind
                } else if let Some(value) = any.downcast_ref::<ast::Value>() {
                    let boxed: ast::BValue = Box::new(value.clone());
                    self.transform_value_to_hir(&boxed)?
                } else {
                    if self.error_tolerance {
                        self.add_warning(
                            Diagnostic::warning(
                                "unsupported dynamic expression payload for `Any` node; substituting unit"
                                    .to_string(),
                            )
                            .with_source_context(DIAGNOSTIC_CONTEXT),
                        );
                        let block = hir::Block {
                            hir_id: self.next_id(),
                            stmts: Vec::new(),
                            expr: None,
                        };
                        hir::ExprKind::Block(block)
                    } else {
                        return Err(crate::error::optimization_error(
                            "unsupported dynamic expression payload for `Any` node",
                        ));
                    }
                }
            }
            ExprKind::Macro(mac) => {
                // Hard error: macros must be lowered during normalization.
                return Err(crate::error::optimization_error(format!(
                    "macro `{}` was not lowered during normalization",
                    mac.invocation.path
                )));
            }
            ExprKind::FormatString(format_str) => {
                self.transform_format_string_to_hir(format_str)?
            }
            ExprKind::Return(ret) => {
                let value = ret
                    .value
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Return(value)
            }
            ExprKind::Break(brk) => {
                let value = brk
                    .value
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Break(value)
            }
            ExprKind::Continue(_) => hir::ExprKind::Continue,
            ExprKind::ConstBlock(_const_block) => {
                return Err(crate::error::optimization_error(
                    "const block must be evaluated before AST→HIR lowering",
                ));
            }
            ExprKind::IntrinsicContainer(container) => {
                self.transform_intrinsic_container_to_hir(container)?
            }
            ExprKind::IntrinsicCall(call) => self.transform_intrinsic_call_to_hir(call)?,
            ExprKind::Reference(reference) => {
                return self.transform_expr_to_hir(reference.referee.as_ref());
            }
            ExprKind::Dereference(deref) => {
                let inner = self.transform_expr_to_hir(deref.referee.as_ref())?;
                hir::ExprKind::Unary(hir::UnOp::Deref, Box::new(inner))
            }
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                )));
            }
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    // create_main_function moved to items.rs

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

    // transform_function moved to items.rs

    // transform_params moved to items.rs

    // transform_generics moved to items.rs

    // wrap_ref_type moved to items.rs

    // make_self_param moved to items.rs

    // transform_impl moved to items.rs

    /// Transform AST value to HIR expression kind
    pub(super) fn transform_value_to_hir(&mut self, value: &ast::BValue) -> Result<hir::ExprKind> {
        use ast::Value;

        match value.as_ref() {
            Value::Int(i) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(i.value))),
            Value::Bool(b) => Ok(hir::ExprKind::Literal(hir::Lit::Bool(b.value))),
            Value::String(s) => Ok(hir::ExprKind::Literal(hir::Lit::Str(s.value.clone()))),
            Value::Decimal(d) => Ok(hir::ExprKind::Literal(hir::Lit::Float(d.value))),
            Value::Char(ch) => Ok(hir::ExprKind::Literal(hir::Lit::Char(ch.value))),
            Value::Unit(_) => {
                let block_id = self.next_id();
                Ok(hir::ExprKind::Block(hir::Block {
                    hir_id: block_id,
                    stmts: Vec::new(),
                    expr: None,
                }))
            }
            Value::Null(_) | Value::None(_) => Ok(hir::ExprKind::Literal(hir::Lit::Null)),
            Value::Struct(struct_val) => {
                let struct_name = struct_val.ty.name.name.as_str();
                let mut segments = Vec::new();
                segments.push(self.make_path_segment(struct_name, None));
                let res = self.resolve_type_symbol(struct_name);

                let path = hir::Path { segments, res };

                let mut fields = Vec::with_capacity(struct_val.structural.fields.len());
                for field in &struct_val.structural.fields {
                    let field_expr_kind =
                        self.transform_value_to_hir(&Box::new(field.value.clone()))?;
                    let field_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: field_expr_kind,
                        span: self.create_span(1),
                    };

                    fields.push(hir::StructExprField {
                        hir_id: self.next_id(),
                        name: field.name.clone().into(),
                        expr: field_expr,
                    });
                }

                Ok(hir::ExprKind::Struct(path, fields))
            }
            Value::Structural(structural) => {
                let def = self.materialize_structural_value_def(structural)?;
                let path = self.path_for_structural_def(&def);

                let mut fields = Vec::with_capacity(structural.fields.len());
                for field in &structural.fields {
                    let field_expr_kind =
                        self.transform_value_to_hir(&Box::new(field.value.clone()))?;
                    let field_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: field_expr_kind,
                        span: self.create_span(1),
                    };

                    fields.push(hir::StructExprField {
                        hir_id: self.next_id(),
                        name: field.name.clone().into(),
                        expr: field_expr,
                    });
                }

                Ok(hir::ExprKind::Struct(path, fields))
            }
            Value::List(list) => {
                let mut elements = Vec::with_capacity(list.values.len());
                for value in &list.values {
                    let expr_kind = self.transform_value_to_hir(&Box::new(value.clone()))?;
                    elements.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: expr_kind,
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(elements))
            }
            Value::Map(map) => {
                let mut entries = Vec::with_capacity(map.entries.len());
                for entry in &map.entries {
                    let key_kind = self.transform_value_to_hir(&Box::new(entry.key.clone()))?;
                    let value_kind = self.transform_value_to_hir(&Box::new(entry.value.clone()))?;
                    let key_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: key_kind,
                        span: self.create_span(1),
                    };
                    let value_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: value_kind,
                        span: self.create_span(1),
                    };
                    let entry = hir::ExprKind::Array(vec![key_expr, value_expr]);
                    entries.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: entry,
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(entries))
            }
            Value::Expr(expr) => self.transform_expr_to_hir(expr).map(|e| e.kind),
            Value::Function(func) => {
                let name = func.sig.name.clone().ok_or_else(|| {
                    crate::error::optimization_error(
                        "function value must have a name for HIR lowering",
                    )
                })?;
                let locator = Locator::Ident(name);
                let path =
                    self.locator_to_hir_path_with_scope(&locator, PathResolutionScope::Value)?;
                Ok(hir::ExprKind::Path(path))
            }
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented AST value type for HIR transformation: {:?}",
                std::mem::discriminant(value.as_ref())
            ))),
        }
    }

    fn transform_array_to_hir(&mut self, array: &ast::ExprArray) -> Result<hir::ExprKind> {
        let mut elements = Vec::with_capacity(array.values.len());
        for value in &array.values {
            elements.push(self.transform_expr_to_hir(value)?);
        }
        Ok(hir::ExprKind::Array(elements))
    }

    fn transform_array_repeat_to_hir(
        &mut self,
        repeat: &ast::ExprArrayRepeat,
    ) -> Result<hir::ExprKind> {
        let elem = Box::new(self.transform_expr_to_hir(repeat.elem.as_ref())?);
        let len = Box::new(self.transform_expr_to_hir(repeat.len.as_ref())?);
        Ok(hir::ExprKind::ArrayRepeat { elem, len })
    }

    fn transform_intrinsic_container_to_hir(
        &mut self,
        container: &ast::ExprIntrinsicContainer,
    ) -> Result<hir::ExprKind> {
        match container {
            ast::ExprIntrinsicContainer::VecElements { elements } => {
                let mut items = Vec::with_capacity(elements.len());
                for element in elements {
                    items.push(self.transform_expr_to_hir(element)?);
                }
                Ok(hir::ExprKind::Array(items))
            }
            ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                let elem = Box::new(self.transform_expr_to_hir(elem.as_ref())?);
                let len = Box::new(self.transform_expr_to_hir(len.as_ref())?);
                Ok(hir::ExprKind::ArrayRepeat { elem, len })
            }
            ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                let mut items = Vec::with_capacity(entries.len());
                for entry in entries {
                    let key = self.transform_expr_to_hir(&entry.key)?;
                    let value = self.transform_expr_to_hir(&entry.value)?;
                    items.push(hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Array(vec![key, value]),
                        span: self.create_span(1),
                    });
                }
                Ok(hir::ExprKind::Array(items))
            }
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
        let op = self.convert_unop_kind(&unop.op)?;

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
                let args = self.transform_call_args_strict(&invoke.args)?;
                Ok(hir::ExprKind::MethodCall(
                    Box::new(receiver),
                    select.field.clone().into(),
                    args,
                ))
            }
            ast::ExprInvokeTarget::Function(locator) => {
                if let Some(intrinsic_call) = ast::intrinsic_call_from_invoke(invoke) {
                    return self.transform_intrinsic_call_to_hir(&intrinsic_call);
                }

                let func_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(
                        self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
                    ),
                    span: self.create_span(1),
                };
                let args = self.transform_call_args_bound(&invoke.args, Some(&func_expr))?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let func_expr = self.transform_expr_to_hir(expr)?;
                let args = self.transform_call_args_strict(&invoke.args)?;
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
        let field = select.field.clone().into();

        Ok(hir::ExprKind::FieldAccess(expr, field))
    }

    /// Transform struct construction to HIR
    pub(super) fn transform_struct_to_hir(
        &mut self,
        struct_expr: &ast::ExprStruct,
    ) -> Result<hir::ExprKind> {
        let path =
            self.ast_expr_to_hir_path(struct_expr.name.as_ref(), PathResolutionScope::Type)?;

        let mut explicit_names = std::collections::HashSet::new();
        let fields = struct_expr
            .fields
            .iter()
            .map(|field| {
                let expr = if let Some(value) = field.value.as_ref() {
                    self.transform_expr_to_hir(value)?
                } else {
                    // Shorthand - reference local with same name.
                    let res = self.resolve_value_symbol(&field.name.name);
                    hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Path(hir::Path {
                            segments: vec![hir::PathSegment {
                                name: field.name.clone().into(),
                                args: None,
                            }],
                            res,
                        }),
                        span: self.create_span(1),
                    }
                };

                explicit_names.insert(field.name.name.clone());
                Ok(hir::StructExprField {
                    hir_id: self.next_id(),
                    name: field.name.clone().into(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let Some(update_expr) = struct_expr.update.as_ref() else {
            return Ok(hir::ExprKind::Struct(path, fields));
        };

        // Lower `Foo { ..base, field: value }` into a block that binds `base`
        // once and then fills missing fields from it, so later MIR lowering
        // only sees a plain struct literal.
        let struct_fields = match path.res {
            Some(hir::Res::Def(def_id)) => self
                .struct_field_defs
                .get(&def_id)
                .cloned()
                .ok_or_else(|| {
                    crate::error::optimization_error(
                        "struct update requires a known struct field layout",
                    )
                })?,
            _ => {
                let segments = path
                    .segments
                    .iter()
                    .map(|seg| seg.name.as_str().to_string())
                    .collect::<Vec<_>>();
                let alias = self.lookup_type_alias(&segments).cloned().ok_or_else(|| {
                    crate::error::optimization_error(
                        "struct update requires a resolved struct definition",
                    )
                })?;
                self.struct_fields_from_type(&alias, Span::new(self.current_file, 0, 0))?
            }
        };

        let base_expr = self.transform_expr_to_hir(update_expr.as_ref())?;
        let base_name = format!("__struct_update_{}", self.next_id());
        let base_symbol = hir::Symbol::new(base_name.clone());
        let base_pat_id = self.next_id();
        let base_pat = hir::Pat {
            hir_id: base_pat_id,
            kind: hir::PatKind::Binding {
                name: base_symbol.clone(),
                mutable: false,
            },
        };
        let local = hir::Local {
            hir_id: self.next_id(),
            pat: base_pat,
            ty: None,
            init: Some(base_expr),
        };
        let local_stmt = hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(local),
        };

        let base_path = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: base_symbol,
                    args: None,
                }],
                res: Some(hir::Res::Local(base_pat_id)),
            }),
            span: self.create_span(1),
        };

        let mut merged_fields = fields;
        for field in struct_fields {
            if explicit_names.contains(field.name.name.as_str()) {
                continue;
            }
            let access = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::FieldAccess(
                    Box::new(base_path.clone()),
                    hir::Symbol::new(field.name.name.clone()),
                ),
                span: self.create_span(1),
            };
            merged_fields.push(hir::StructExprField {
                hir_id: self.next_id(),
                name: hir::Symbol::new(field.name.name.clone()),
                expr: access,
            });
        }

        let struct_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Struct(path, merged_fields),
            span: self.create_span(1),
        };
        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts: vec![local_stmt],
            expr: Some(Box::new(struct_expr)),
        }))
    }

    /// Transform block expression to HIR
    pub(super) fn transform_block_to_hir(
        &mut self,
        block: &ast::ExprBlock,
    ) -> Result<hir::ExprKind> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let last_expr_index = block
                .last_expr()
                .and_then(|_| block.stmts.len().checked_sub(1));
            let stmts = block
                .stmts
                .iter()
                .enumerate()
                .filter_map(|(idx, stmt)| {
                    if Some(idx) == last_expr_index {
                        return None;
                    }
                    Some(self.transform_block_stmt_to_hir(stmt))
                })
                .collect::<Result<Vec<_>>>()?;

            // Preserve the value of the final expression without duplicating it as a statement.
            let expr = last_expr_index
                .and_then(|idx| block.stmts.get(idx))
                .and_then(|stmt| match stmt {
                    ast::BlockStmt::Expr(expr) if expr.has_value() => {
                        Some(self.transform_expr_to_hir(expr.expr.as_ref()))
                    }
                    _ => None,
                })
                .transpose()?
                .map(Box::new);

            Ok(hir::ExprKind::Block(hir::Block {
                hir_id: self.next_id(),
                stmts,
                expr,
            }))
        })();
        self.pop_value_scope();
        self.pop_type_scope();
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

    pub(super) fn transform_match_to_hir(
        &mut self,
        match_expr: &ast::ExprMatch,
    ) -> Result<hir::ExprKind> {
        let scrutinee = match_expr
            .scrutinee
            .as_ref()
            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
            .transpose()?;

        let scrutinee = match scrutinee {
            Some(expr) => expr,
            None => {
                return Err(crate::error::optimization_error(
                    "match expressions without scrutinee are not supported",
                ));
            }
        };

        let mut arms = Vec::with_capacity(match_expr.cases.len());
        for case in &match_expr.cases {
            let pat = if let Some(pat) = case.pat.as_ref() {
                self.transform_pattern(pat.as_ref())?
            } else {
                hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Wild,
                }
            };
            self.register_pattern_bindings(&pat);

            let guard = case
                .guard
                .as_ref()
                .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                .transpose()?;
            let body = self.transform_expr_to_hir(case.body.as_ref())?;

            arms.push(hir::MatchArm {
                hir_id: self.next_id(),
                pat,
                guard,
                body,
            });
        }

        Ok(hir::ExprKind::Match(Box::new(scrutinee), arms))
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

    pub(super) fn transform_for_to_hir(
        &mut self,
        for_expr: &ast::ExprFor,
    ) -> Result<hir::ExprKind> {
        let mut stmts = Vec::new();

        if !matches!(for_expr.iter.kind(), ast::ExprKind::Range(_)) {
            if let Some(enum_spec) = self.extract_enumerate_loop_spec(for_expr)? {
                return self.lower_enumerate_for_loop(for_expr, enum_spec);
            }
            if let Some(iter_spec) = self.extract_iter_loop_spec(for_expr)? {
                return self.lower_iter_for_loop(for_expr, iter_spec);
            }
            return Err(crate::error::optimization_error(
                "`for` loop lowering only supports range iterators, iter(), and enumerate()",
            ));
        }

        let (mut pat, _ty, _) = self.transform_pattern_with_metadata(&for_expr.pat)?;
        let (loop_name, loop_res) = match &mut pat.kind {
            hir::PatKind::Binding { name, .. } => (name.clone(), Some(hir::Res::Local(pat.hir_id))),
            _ => {
                return Err(crate::error::optimization_error(
                    "`for` loop pattern must be a simple binding",
                ));
            }
        };
        if let hir::PatKind::Binding { mutable, .. } = &mut pat.kind {
            *mutable = true;
        }

        let (start_expr, end_expr, step_expr, inclusive) = match for_expr.iter.kind() {
            ast::ExprKind::Range(range) => {
                let start = range
                    .start
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let end = range
                    .end
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let step = range
                    .step
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?;
                let inclusive = matches!(range.limit, ast::ExprRangeLimit::Inclusive);
                (start, end, step, inclusive)
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "`for` loop lowering currently only supports range iterators",
                ));
            }
        };

        let init_expr = start_expr.unwrap_or_else(|| hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
            span: Span::new(self.current_file, 0, 0),
        });

        let local = hir::Local {
            hir_id: self.next_id(),
            pat: pat.clone(),
            ty: None,
            init: Some(init_expr),
        };
        self.register_pattern_bindings(&local.pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(local),
        });

        let loop_var = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: loop_name.clone(),
                    args: None,
                }],
                res: loop_res,
            }),
            span: Span::new(self.current_file, 0, 0),
        };

        let end_expr = end_expr.ok_or_else(|| {
            crate::error::optimization_error("`for` loop range missing end expression")
        })?;

        let cmp_op = if inclusive {
            hir::BinOp::Le
        } else {
            hir::BinOp::Lt
        };
        let cond_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Binary(cmp_op, Box::new(loop_var.clone()), Box::new(end_expr)),
            span: Span::new(self.current_file, 0, 0),
        };

        let step_expr = step_expr.unwrap_or_else(|| hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
            span: Span::new(self.current_file, 0, 0),
        });
        let increment = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Assign(
                Box::new(loop_var.clone()),
                Box::new(hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Binary(
                        hir::BinOp::Add,
                        Box::new(loop_var.clone()),
                        Box::new(step_expr),
                    ),
                    span: Span::new(self.current_file, 0, 0),
                }),
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let body_expr = self.transform_expr_to_hir(for_expr.body.as_ref())?;
        let mut body_stmts = Vec::new();
        if let hir::ExprKind::Block(block) = &body_expr.kind {
            body_stmts.extend(block.stmts.clone());
            if let Some(expr) = &block.expr {
                body_stmts.push(hir::Stmt {
                    hir_id: self.next_id(),
                    kind: hir::StmtKind::Semi(*expr.clone()),
                });
            }
        } else {
            body_stmts.push(hir::Stmt {
                hir_id: self.next_id(),
                kind: hir::StmtKind::Semi(body_expr),
            });
        }

        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Semi(increment),
        });

        let while_block = hir::Block {
            hir_id: self.next_id(),
            stmts: body_stmts,
            expr: None,
        };

        let while_expr = hir::ExprKind::While(Box::new(cond_expr), while_block);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Expr(hir::Expr {
                hir_id: self.next_id(),
                kind: while_expr,
                span: Span::new(self.current_file, 0, 0),
            }),
        });

        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts,
            expr: None,
        }))
    }

    fn extract_enumerate_loop_spec(
        &mut self,
        for_expr: &ast::ExprFor,
    ) -> Result<Option<EnumerateLoopSpec>> {
        let ast::ExprKind::Invoke(invoke) = for_expr.iter.kind() else {
            return Ok(None);
        };
        if !invoke.args.is_empty() {
            return Ok(None);
        }
        let segments = match &invoke.target {
            ast::ExprInvokeTarget::Function(locator) => match locator {
                ast::Locator::Path(path) => path.segments.clone(),
                ast::Locator::Ident(ident) => vec![ident.clone()],
                ast::Locator::ParameterPath(path) => {
                    path.segments.iter().map(|seg| seg.ident.clone()).collect()
                }
            },
            ast::ExprInvokeTarget::Method(select) => {
                let Some(mut base) = self.path_segments_from_expr(&select.obj) else {
                    return Ok(None);
                };
                base.push(select.field.clone());
                base
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let Some(segments) = self.path_segments_from_expr(expr) else {
                    return Ok(None);
                };
                segments
            }
            _ => return Ok(None),
        };
        if segments.len() < 3 {
            return Ok(None);
        }
        let last = segments.last().map(|seg| seg.as_str());
        let penultimate = segments.get(segments.len() - 2).map(|seg| seg.as_str());
        if last != Some("enumerate") || penultimate != Some("iter") {
            return Ok(None);
        }

        let base_segments = segments[..segments.len() - 2].to_vec();
        if base_segments.is_empty() {
            return Err(crate::error::optimization_error(
                "enumerate() base path is empty",
            ));
        }

        let tuple = match for_expr.pat.kind() {
            ast::PatternKind::Tuple(tuple) => tuple,
            _ => {
                return Err(crate::error::optimization_error(
                    "enumerate() loop pattern must be a tuple of bindings",
                ));
            }
        };
        if tuple.patterns.len() != 2 {
            return Err(crate::error::optimization_error(
                "enumerate() loop pattern must bind (index, value)",
            ));
        }

        let index_ident = match tuple.patterns.get(0).and_then(|pat| pat.as_ident()) {
            Some(ident) => ident.clone(),
            None => {
                return Err(crate::error::optimization_error(
                    "enumerate() loop index must be a simple binding",
                ));
            }
        };
        let value_ident = match tuple.patterns.get(1).and_then(|pat| pat.as_ident()) {
            Some(ident) => ident.clone(),
            None => {
                return Err(crate::error::optimization_error(
                    "enumerate() loop value must be a simple binding",
                ));
            }
        };

        Ok(Some(EnumerateLoopSpec {
            base_segments,
            index_ident,
            value_ident,
        }))
    }

    fn extract_iter_loop_spec(&mut self, for_expr: &ast::ExprFor) -> Result<Option<IterLoopSpec>> {
        let ast::ExprKind::Invoke(invoke) = for_expr.iter.kind() else {
            return Ok(None);
        };
        if !invoke.args.is_empty() {
            return Ok(None);
        }
        let segments = match &invoke.target {
            ast::ExprInvokeTarget::Function(locator) => match locator {
                ast::Locator::Path(path) => path.segments.clone(),
                ast::Locator::Ident(ident) => vec![ident.clone()],
                ast::Locator::ParameterPath(path) => {
                    path.segments.iter().map(|seg| seg.ident.clone()).collect()
                }
            },
            ast::ExprInvokeTarget::Method(select) => {
                let Some(mut base) = self.path_segments_from_expr(&select.obj) else {
                    return Ok(None);
                };
                base.push(select.field.clone());
                base
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let Some(segments) = self.path_segments_from_expr(expr) else {
                    return Ok(None);
                };
                segments
            }
            _ => return Ok(None),
        };
        if segments.len() < 2 {
            return Ok(None);
        }
        let last = segments.last().map(|seg| seg.as_str());
        if last != Some("iter") {
            return Ok(None);
        }

        let base_segments = segments[..segments.len() - 1].to_vec();
        if base_segments.is_empty() {
            return Err(crate::error::optimization_error(
                "iter() base path is empty",
            ));
        }

        let value_ident = match for_expr.pat.as_ident() {
            Some(ident) => ident.clone(),
            None => {
                return Err(crate::error::optimization_error(
                    "iter() loop pattern must be a simple binding",
                ));
            }
        };

        Ok(Some(IterLoopSpec {
            base_segments,
            value_ident,
        }))
    }

    fn lower_enumerate_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        spec: EnumerateLoopSpec,
    ) -> Result<hir::ExprKind> {
        use fp_core::intrinsics::IntrinsicCallKind;

        let mut stmts = Vec::new();

        let base_path = ast::Path::new(spec.base_segments.clone());
        let base_locator = ast::Locator::path(base_path);
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.locator_to_hir_path_with_scope(&base_locator, PathResolutionScope::Value)?,
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let idx_hir_id = self.next_id();
        let idx_name = hir::Symbol::new(format!("__fp_idx{}", idx_hir_id));
        let idx_pat = hir::Pat {
            hir_id: idx_hir_id,
            kind: hir::PatKind::Binding {
                name: idx_name.clone(),
                mutable: true,
            },
        };
        let idx_init = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
            span: Span::new(self.current_file, 0, 0),
        };
        let idx_local = hir::Local {
            hir_id: self.next_id(),
            pat: idx_pat.clone(),
            ty: None,
            init: Some(idx_init),
        };
        self.register_pattern_bindings(&idx_pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(idx_local),
        });

        let idx_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: idx_name.clone(),
                    args: None,
                }],
                res: Some(hir::Res::Local(idx_pat.hir_id)),
            }),
            span: Span::new(self.current_file, 0, 0),
        };

        let len_expr = if let Some(len) = self.lookup_const_list_length(&spec.base_segments) {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Literal(hir::Lit::Integer(len as i64)),
                span: Span::new(self.current_file, 0, 0),
            }
        } else {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: IntrinsicCallKind::Len,
                    callargs: vec![hir::CallArg {
                        name: hir::Symbol::new("arg0"),
                        value: base_expr.clone(),
                    }],
                }),
                span: Span::new(self.current_file, 0, 0),
            }
        };

        let cond_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Binary(
                hir::BinOp::Lt,
                Box::new(idx_expr.clone()),
                Box::new(len_expr),
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let index_pat = hir::Pat {
            hir_id: self.next_id(),
            kind: hir::PatKind::Binding {
                name: hir::Symbol::new(spec.index_ident.name.clone()),
                mutable: false,
            },
        };
        let index_local = hir::Local {
            hir_id: self.next_id(),
            pat: index_pat.clone(),
            ty: None,
            init: Some(idx_expr.clone()),
        };
        self.register_pattern_bindings(&index_pat);

        let value_pat = hir::Pat {
            hir_id: self.next_id(),
            kind: hir::PatKind::Binding {
                name: hir::Symbol::new(spec.value_ident.name.clone()),
                mutable: false,
            },
        };
        let value_init = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Index(Box::new(base_expr.clone()), Box::new(idx_expr.clone())),
            span: Span::new(self.current_file, 0, 0),
        };
        let value_local = hir::Local {
            hir_id: self.next_id(),
            pat: value_pat.clone(),
            ty: None,
            init: Some(value_init),
        };
        self.register_pattern_bindings(&value_pat);

        let mut body_stmts = Vec::new();
        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(index_local),
        });
        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(value_local),
        });

        let body_expr = self.transform_expr_to_hir(for_expr.body.as_ref())?;
        if let hir::ExprKind::Block(block) = &body_expr.kind {
            body_stmts.extend(block.stmts.clone());
            if let Some(expr) = &block.expr {
                body_stmts.push(hir::Stmt {
                    hir_id: self.next_id(),
                    kind: hir::StmtKind::Semi(*expr.clone()),
                });
            }
        } else {
            body_stmts.push(hir::Stmt {
                hir_id: self.next_id(),
                kind: hir::StmtKind::Semi(body_expr),
            });
        }

        let increment = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Assign(
                Box::new(idx_expr.clone()),
                Box::new(hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Binary(
                        hir::BinOp::Add,
                        Box::new(idx_expr.clone()),
                        Box::new(hir::Expr {
                            hir_id: self.next_id(),
                            kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
                            span: Span::new(self.current_file, 0, 0),
                        }),
                    ),
                    span: Span::new(self.current_file, 0, 0),
                }),
            ),
            span: Span::new(self.current_file, 0, 0),
        };
        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Semi(increment),
        });

        let while_block = hir::Block {
            hir_id: self.next_id(),
            stmts: body_stmts,
            expr: None,
        };
        let while_expr = hir::ExprKind::While(Box::new(cond_expr), while_block);

        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Expr(hir::Expr {
                hir_id: self.next_id(),
                kind: while_expr,
                span: Span::new(self.current_file, 0, 0),
            }),
        });

        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts,
            expr: None,
        }))
    }

    fn lower_iter_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        spec: IterLoopSpec,
    ) -> Result<hir::ExprKind> {
        use fp_core::intrinsics::IntrinsicCallKind;

        let mut stmts = Vec::new();

        let base_path = ast::Path::new(spec.base_segments.clone());
        let base_locator = ast::Locator::path(base_path);
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.locator_to_hir_path_with_scope(&base_locator, PathResolutionScope::Value)?,
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let idx_hir_id = self.next_id();
        let idx_name = hir::Symbol::new(format!("__fp_idx{}", idx_hir_id));
        let idx_pat = hir::Pat {
            hir_id: idx_hir_id,
            kind: hir::PatKind::Binding {
                name: idx_name.clone(),
                mutable: true,
            },
        };
        let idx_init = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
            span: Span::new(self.current_file, 0, 0),
        };
        let idx_local = hir::Local {
            hir_id: self.next_id(),
            pat: idx_pat.clone(),
            ty: None,
            init: Some(idx_init),
        };
        self.register_pattern_bindings(&idx_pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(idx_local),
        });

        let idx_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: idx_name.clone(),
                    args: None,
                }],
                res: Some(hir::Res::Local(idx_pat.hir_id)),
            }),
            span: Span::new(self.current_file, 0, 0),
        };

        let len_expr = if let Some(len) = self.lookup_const_list_length(&spec.base_segments) {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Literal(hir::Lit::Integer(len as i64)),
                span: Span::new(self.current_file, 0, 0),
            }
        } else {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: IntrinsicCallKind::Len,
                    callargs: vec![hir::CallArg {
                        name: hir::Symbol::new("arg0"),
                        value: base_expr.clone(),
                    }],
                }),
                span: Span::new(self.current_file, 0, 0),
            }
        };

        let cond_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Binary(
                hir::BinOp::Lt,
                Box::new(idx_expr.clone()),
                Box::new(len_expr),
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let value_pat = hir::Pat {
            hir_id: self.next_id(),
            kind: hir::PatKind::Binding {
                name: hir::Symbol::new(spec.value_ident.name.clone()),
                mutable: false,
            },
        };
        let value_init = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Index(Box::new(base_expr.clone()), Box::new(idx_expr.clone())),
            span: Span::new(self.current_file, 0, 0),
        };
        let value_local = hir::Local {
            hir_id: self.next_id(),
            pat: value_pat.clone(),
            ty: None,
            init: Some(value_init),
        };
        self.register_pattern_bindings(&value_pat);

        let mut body_stmts = Vec::new();
        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(value_local),
        });

        let body_expr = self.transform_expr_to_hir(for_expr.body.as_ref())?;
        if let hir::ExprKind::Block(block) = &body_expr.kind {
            body_stmts.extend(block.stmts.clone());
            if let Some(expr) = &block.expr {
                body_stmts.push(hir::Stmt {
                    hir_id: self.next_id(),
                    kind: hir::StmtKind::Semi(*expr.clone()),
                });
            }
        } else {
            body_stmts.push(hir::Stmt {
                hir_id: self.next_id(),
                kind: hir::StmtKind::Semi(body_expr),
            });
        }

        let increment = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Assign(
                Box::new(idx_expr.clone()),
                Box::new(hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Binary(
                        hir::BinOp::Add,
                        Box::new(idx_expr.clone()),
                        Box::new(hir::Expr {
                            hir_id: self.next_id(),
                            kind: hir::ExprKind::Literal(hir::Lit::Integer(1)),
                            span: Span::new(self.current_file, 0, 0),
                        }),
                    ),
                    span: Span::new(self.current_file, 0, 0),
                }),
            ),
            span: Span::new(self.current_file, 0, 0),
        };
        body_stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Semi(increment),
        });

        let while_block = hir::Block {
            hir_id: self.next_id(),
            stmts: body_stmts,
            expr: None,
        };
        let while_expr = hir::ExprKind::While(Box::new(cond_expr), while_block);

        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Expr(hir::Expr {
                hir_id: self.next_id(),
                kind: while_expr,
                span: Span::new(self.current_file, 0, 0),
            }),
        });

        Ok(hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts,
            expr: None,
        }))
    }

    fn path_segments_from_expr(&self, expr: &ast::Expr) -> Option<Vec<ast::Ident>> {
        match expr.kind() {
            ast::ExprKind::Locator(locator) => match locator {
                ast::Locator::Path(path) => Some(path.segments.clone()),
                ast::Locator::Ident(ident) => Some(vec![ident.clone()]),
                ast::Locator::ParameterPath(path) => {
                    Some(path.segments.iter().map(|seg| seg.ident.clone()).collect())
                }
            },
            ast::ExprKind::Invoke(invoke) => {
                // Permit no-arg method chains like `xs.iter().enumerate()` to be treated as a path.
                // This is used by enumerate() lowering to recover the base path segments.
                if !invoke.args.is_empty() {
                    return None;
                }
                match &invoke.target {
                    ast::ExprInvokeTarget::Function(locator) => match locator {
                        ast::Locator::Path(path) => Some(path.segments.clone()),
                        ast::Locator::Ident(ident) => Some(vec![ident.clone()]),
                        ast::Locator::ParameterPath(path) => {
                            Some(path.segments.iter().map(|seg| seg.ident.clone()).collect())
                        }
                    },
                    ast::ExprInvokeTarget::Method(select) => {
                        let mut base = self.path_segments_from_expr(&select.obj)?;
                        base.push(select.field.clone());
                        Some(base)
                    }
                    ast::ExprInvokeTarget::Expr(expr) => self.path_segments_from_expr(expr),
                    _ => None,
                }
            }
            ast::ExprKind::Select(select) => {
                let mut base = self.path_segments_from_expr(&select.obj)?;
                base.push(select.field.clone());
                Some(base)
            }
            _ => None,
        }
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
                let expr = self.transform_expr_to_hir(&expr_stmt.expr)?;
                if expr_stmt.has_value() {
                    hir::StmtKind::Expr(expr)
                } else {
                    hir::StmtKind::Semi(expr)
                }
            }
            ast::BlockStmt::Let(let_stmt) => {
                let (pat, explicit_ty, _) = self.transform_pattern_with_metadata(&let_stmt.pat)?;
                let init = let_stmt
                    .init
                    .as_ref()
                    .map(|v| self.transform_expr_to_hir(v))
                    .transpose()?;

                let local = hir::Local {
                    hir_id: self.next_id(),
                    pat,
                    ty: explicit_ty,
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
            BinOpKind::Shl => hir::BinOp::Shl,
            BinOpKind::Shr => hir::BinOp::Shr,
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
    pub(super) fn convert_unop_kind(&self, op: &UnOpKind) -> Result<hir::UnOp> {
        match op {
            UnOpKind::Neg => Ok(hir::UnOp::Neg),
            UnOpKind::Not => Ok(hir::UnOp::Not),
            UnOpKind::Deref => Ok(hir::UnOp::Deref),
            UnOpKind::Any(kind) => Err(crate::error::optimization_error(format!(
                "Unsupported unary operator variant encountered during AST→HIR lowering: {:?}",
                kind
            ))),
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
        format_str: &ast::ExprStringTemplate,
    ) -> Result<hir::ExprKind> {
        let parts = format_str
            .parts
            .iter()
            .map(|part| match part {
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
            })
            .collect();

        Ok(hir::ExprKind::FormatString(hir::FormatString { parts }))
    }

    pub(super) fn transform_intrinsic_call_to_hir(
        &mut self,
        call: &ast::ExprIntrinsicCall,
    ) -> Result<hir::ExprKind> {
        let mut callargs = Vec::with_capacity(call.args.len() + call.kwargs.len());
        for (index, arg) in call.args.iter().enumerate() {
            callargs.push(hir::CallArg {
                name: hir::Symbol::new(format!("arg{}", index)),
                value: self.transform_expr_to_hir(arg)?,
            });
        }
        for kwarg in &call.kwargs {
            callargs.push(hir::CallArg {
                name: kwarg.name.clone().into(),
                value: self.transform_expr_to_hir(&kwarg.value)?,
            });
        }

        Ok(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
            kind: call.kind,
            callargs,
        }))
    }

    pub(super) fn transform_call_args_bound(
        &mut self,
        args: &[ast::Expr],
        callee: Option<&hir::Expr>,
    ) -> Result<Vec<hir::CallArg>> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.transform_expr_to_hir(arg)?);
        }
        let Some(param_names) = callee
            .and_then(|expr| match &expr.kind {
                hir::ExprKind::Path(path) => path.res.as_ref(),
                _ => None,
            })
            .and_then(|res| match res {
                hir::Res::Def(def_id) => Some(*def_id),
                _ => None,
            })
            .and_then(|def_id| self.program_def_params(def_id))
        else {
            return Ok(values
                .into_iter()
                .enumerate()
                .map(|(index, value)| hir::CallArg {
                    name: hir::Symbol::new(format!("arg{}", index)),
                    value,
                })
                .collect());
        };

        if values.len() != param_names.len() {
            return Err(crate::error::optimization_error(
                "call arguments do not match function parameter count",
            ));
        }

        Ok(values
            .into_iter()
            .enumerate()
            .map(|(index, value)| hir::CallArg {
                name: param_names[index].clone(),
                value,
            })
            .collect())
    }

    pub(super) fn transform_call_args_strict(
        &mut self,
        args: &[ast::Expr],
    ) -> Result<Vec<hir::CallArg>> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.transform_expr_to_hir(arg)?);
        }
        Ok(values
            .into_iter()
            .enumerate()
            .map(|(index, value)| hir::CallArg {
                name: hir::Symbol::new(format!("arg{}", index)),
                value,
            })
            .collect())
    }

    pub(super) fn program_def_params(&self, def_id: hir::DefId) -> Option<Vec<hir::Symbol>> {
        let Some(item) = self.program_def_map.get(&def_id) else {
            return None;
        };
        match &item.kind {
            hir::ItemKind::Function(function) => Some(
                function
                    .sig
                    .inputs
                    .iter()
                    .filter_map(|param| match &param.pat.kind {
                        hir::PatKind::Binding { name, .. } => Some(name.clone()),
                        _ => None,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    // locator_to_hir_path_with_scope moved to helpers.rs

    // ast_expr_to_hir_path moved to helpers.rs

    // convert_generic_args moved to helpers.rs

    // canonicalize_segments moved to helpers.rs

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

    // make_path_segment moved to helpers.rs

    pub(super) fn primitive_type_to_hir(&mut self, prim: ast::TypePrimitive) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Primitive(prim),
            Span::new(self.current_file, 0, 0),
        )
    }

    // transform_pattern_with_metadata moved to patterns.rs

    // transform_pattern moved to patterns.rs

    // register_pattern_bindings moved to patterns.rs

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

impl HirGenerator {
    fn struct_fields_from_type(
        &mut self,
        ty: &ast::Ty,
        span: Span,
    ) -> Result<Vec<ast::StructuralField>> {
        match ty {
            ast::Ty::Structural(structural) => Ok(structural.fields.clone()),
            ast::Ty::Struct(struct_ty) => Ok(struct_ty.fields.clone()),
            ast::Ty::TypeBinaryOp(op) => {
                let lhs = self.struct_fields_from_type(&op.lhs, span)?;
                let rhs = self.struct_fields_from_type(&op.rhs, span)?;
                match op.kind {
                    ast::TypeBinaryOpKind::Add => self.merge_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Intersect => self.intersect_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Subtract => self.subtract_struct_fields(lhs, rhs),
                    ast::TypeBinaryOpKind::Union => Err(crate::error::optimization_error(
                        "struct update does not support union type operands",
                    )),
                }
            }
            ast::Ty::Expr(expr) => {
                if let ast::ExprKind::Locator(locator) = expr.kind() {
                    let path = locator.to_path();
                    let segments = path
                        .segments
                        .iter()
                        .map(|seg| seg.name.clone())
                        .collect::<Vec<_>>();
                    if let Some(alias) = self.lookup_type_alias(&segments).cloned() {
                        return self.struct_fields_from_type(&alias, span);
                    }
                }
                Err(crate::error::optimization_error(
                    "struct update requires a resolved struct definition",
                ))
            }
            _ => Err(crate::error::optimization_error(
                "struct update requires a resolved struct definition",
            )),
        }
    }

    fn merge_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let mut result = Vec::new();
        let mut seen = HashMap::new();
        for field in lhs {
            seen.insert(field.name.name.clone(), field.value.clone());
            result.push(field);
        }
        for field in rhs {
            if let Some(existing) = seen.get(&field.name.name) {
                if existing != &field.value {
                    return Err(crate::error::optimization_error(format!(
                        "conflicting field types for `{}` in structural merge",
                        field.name.name
                    )));
                }
                continue;
            }
            seen.insert(field.name.name.clone(), field.value.clone());
            result.push(field);
        }
        Ok(result)
    }

    fn intersect_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let mut rhs_map = HashMap::new();
        for field in rhs {
            rhs_map.insert(field.name.name.clone(), field.value);
        }
        let mut result = Vec::new();
        for field in lhs {
            if let Some(rhs_ty) = rhs_map.get(&field.name.name) {
                if rhs_ty != &field.value {
                    return Err(crate::error::optimization_error(format!(
                        "conflicting field types for `{}` in structural intersect",
                        field.name.name
                    )));
                }
                result.push(field);
            }
        }
        Ok(result)
    }

    fn subtract_struct_fields(
        &mut self,
        lhs: Vec<ast::StructuralField>,
        rhs: Vec<ast::StructuralField>,
    ) -> Result<Vec<ast::StructuralField>> {
        let rhs_names = rhs
            .into_iter()
            .map(|field| field.name.name)
            .collect::<HashSet<_>>();
        Ok(lhs
            .into_iter()
            .filter(|field| !rhs_names.contains(&field.name.name))
            .collect())
    }
}
