use super::*;
use fp_core::intrinsics::{CallKind, OpKind};
use fp_core::module::path::PathPrefix;
use fp_core::query::lower_fp_expr_to_query;

struct EnumerateLoopSpec {
    base_prefix: PathPrefix,
    base_segments: Vec<ast::Ident>,
    index_ident: ast::Ident,
    value_ident: ast::Ident,
}

struct IterLoopSpec {
    base_prefix: PathPrefix,
    base_segments: Vec<ast::Ident>,
    value_ident: ast::Ident,
}

impl HirGenerator {
    fn borrowed_string_from_bytes(bytes: &ast::ValueBytes) -> Option<String> {
        let raw = bytes.value.as_ref();
        let trimmed = raw.strip_suffix(&[0])?;
        std::str::from_utf8(trimmed).ok().map(str::to_string)
    }

    /// Lowers an AST `Value::Bytes` expression, produced either by a real
    /// `b"..."`/`c"..."` literal (`ast/expr.rs::parse_string`, which
    /// attaches a `&[u8; N]`/`&std::ffi::CStr` `ty_slot` to disambiguate
    /// the two) or by some other, older producer of a bare `Value::Bytes`
    /// with no such type hint (the Python frontend, `fp-interpret`'s
    /// raw-memory intrinsics) — preserved via the same UTF-8-plus-
    /// trailing-NUL fallback this used to always take.
    fn transform_bytes_value_to_hir(bytes: &ast::ValueBytes, ty: Option<&ast::Ty>) -> hir::ExprKind {
        let raw: Vec<u8> = bytes.value.as_ref().to_vec();
        if let Some(ast::Ty::Reference(reference)) = ty {
            return if matches!(reference.ty.as_ref(), ast::Ty::Array(_)) {
                hir::ExprKind::Literal(hir::Lit::Bytes(raw))
            } else {
                hir::ExprKind::Literal(hir::Lit::CStr(raw))
            };
        }
        if let Some(text) = Self::borrowed_string_from_bytes(bytes) {
            hir::ExprKind::Literal(hir::Lit::Str(text))
        } else {
            hir::ExprKind::Literal(hir::Lit::Bytes(raw))
        }
    }

    /// Records a diagnostic for an AST construct that can't be lowered to
    /// HIR (an unhandled shape, an unnormalized macro, etc.) and returns an
    /// empty-block placeholder in its place — lets HIR generation for the
    /// rest of the package continue past isolated gaps instead of
    /// aborting entirely on the first one (which previously forced a
    /// whole-package fallback to the untyped pipeline over a single
    /// unsupported construct anywhere in it). Mirrors the pre-existing
    /// closure-lowering-not-implemented precedent.
    fn error_placeholder_expr_kind(&mut self, message: String, error_span: Span) -> hir::ExprKind {
        self.add_error(
            Diagnostic::error(message)
                .with_source_context(DIAGNOSTIC_CONTEXT)
                .with_span(error_span),
        );
        hir::ExprKind::Block(hir::Block {
            hir_id: self.next_id(),
            stmts: Vec::new(),
            expr: None,
        })
    }

    /// Transform an AST expression to HIR expression
    pub(super) fn transform_expr_to_hir(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        let Some(normalizer) = self.intrinsic_normalizer.as_ref() else {
            return self.transform_expr_to_hir_inner(ast_expr);
        };

        let needs_normalization = matches!(
            ast_expr.kind(),
            ast::ExprKind::Macro(_)
                | ast::ExprKind::IntrinsicCall(_)
                | ast::ExprKind::IntrinsicContainer(_)
                | ast::ExprKind::Struct(_)
                | ast::ExprKind::Structural(_)
                | ast::ExprKind::Invoke(_)
        );
        if !needs_normalization {
            return self.transform_expr_to_hir_inner(ast_expr);
        }

        let mut normalized = normalizer.normalize_expr(ast_expr.clone())?.into_inner();
        if matches!(ast_expr.kind(), ast::ExprKind::Macro(_))
            && matches!(normalized.kind(), ast::ExprKind::IntrinsicCall(_))
        {
            normalized = normalizer.normalize_expr(normalized)?.into_inner();
        }
        self.transform_expr_to_hir_inner(&normalized)
    }

    fn transform_expr_to_hir_inner(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        use ast::ExprKind;

        let expr_span = ast_expr.span();

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        if let Some(document) = lower_fp_expr_to_query(ast_expr, None) {
            let ir = self.resolve_query_ir(&document)?;
            return Ok(hir::Expr {
                hir_id,
                kind: hir::ExprKind::Query(hir::Query {
                    origin: super::query_origin(&document),
                    ir,
                    span: expr_span,
                }),
                span,
            });
        }

        let kind = match ast_expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Bytes(bytes) => {
                    Self::transform_bytes_value_to_hir(bytes, ast_expr.ty())
                }
                _ => self.transform_value_to_hir(value)?,
            },
            ExprKind::Id(expr_id) => self.error_placeholder_expr_kind(
                format!("unresolved expression id {expr_id} during AST→HIR lowering"),
                expr_span,
            ),
            ExprKind::Name(_) => hir::ExprKind::Path(
                self.ast_expr_to_hir_path(ast_expr, PathResolutionScope::Value)?,
            ),
            ExprKind::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            ExprKind::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            ExprKind::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            ExprKind::Select(select) => self.transform_select_to_hir(select)?,
            ExprKind::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            ExprKind::Block(block) => {
                hir::ExprKind::Block(self.transform_block_node_to_hir(block)?)
            }
            ExprKind::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            ExprKind::Match(match_expr) => self.transform_match_to_hir(match_expr)?,
            ExprKind::Loop(loop_expr) => self.transform_loop_to_hir(loop_expr)?,
            ExprKind::While(while_expr) => self.transform_while_to_hir(while_expr)?,
            ExprKind::With(expr_with) => {
                let context = self.transform_expr_to_hir(expr_with.context.as_ref())?;
                let body = self.transform_expr_to_hir(expr_with.body.as_ref())?;
                hir::ExprKind::With(Box::new(context), Box::new(body))
            }
            ExprKind::Assign(assign) => self.transform_assign_to_hir(assign)?,
            ExprKind::Paren(paren) => self.transform_paren_to_hir(paren)?,
            ExprKind::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            ExprKind::Array(array_expr) => self.transform_array_to_hir(array_expr)?,
            ExprKind::ArrayRepeat(array_repeat) => {
                self.transform_array_repeat_to_hir(array_repeat)?
            }
            ExprKind::Tuple(tuple_expr) => {
                let values = tuple_expr
                    .values
                    .iter()
                    .map(|value| self.transform_expr_to_hir(value))
                    .collect::<Result<Vec<_>>>()?;
                hir::ExprKind::Tuple(values)
            }
            ExprKind::Range(_range) => {
                self.add_error(
                    Diagnostic::warning(
                        "range expressions are only supported in for loops and slicing; treating as empty array"
                            .to_string(),
                    )
                    .with_span(expr_span),
                );
                hir::ExprKind::Array(Vec::new())
            }
            ExprKind::Index(index_expr) => {
                if let ast::ExprKind::Range(range) = index_expr.index.kind() {
                    if range.step.is_some() {
                        self.add_error(
                            Diagnostic::warning(
                                "range steps are not supported in slicing; ignoring step"
                                    .to_string(),
                            )
                            .with_span(expr_span),
                        );
                    }
                    let base_expr = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let start_expr = range
                        .start
                        .as_ref()
                        .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                        .transpose()?
                        .map(Box::new);
                    let end_expr = range
                        .end
                        .as_ref()
                        .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                        .transpose()?
                        .map(Box::new);
                    let inclusive = matches!(range.limit, ast::ExprRangeLimit::Inclusive);
                    hir::ExprKind::Slice(hir::SliceExpr {
                        hir_id: self.next_id(),
                        base: Box::new(base_expr),
                        start: start_expr,
                        end: end_expr,
                        inclusive,
                    })
                } else {
                    let base = self.transform_expr_to_hir(index_expr.obj.as_ref())?;
                    let index = self.transform_expr_to_hir(index_expr.index.as_ref())?;
                    hir::ExprKind::Index(Box::new(base), Box::new(index))
                }
            }
            ExprKind::Quote(_quote) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::Splice(_splice) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::SplicePending(_pending) => {
                let block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                hir::ExprKind::Block(block)
            }
            ExprKind::Try(expr_try) => {
                let body = Box::new(self.transform_expr_to_hir(expr_try.expr.as_ref())?);
                let mut catches = Vec::with_capacity(expr_try.catches.len());
                for catch in &expr_try.catches {
                    let pat = catch
                        .pat
                        .as_ref()
                        .map(|pat| self.transform_pattern(pat.as_ref()))
                        .transpose()?;
                    catches.push(hir::TryCatch {
                        hir_id: self.next_id(),
                        pat,
                        body: self.transform_expr_to_hir(catch.body.as_ref())?,
                    });
                }
                let elze = expr_try
                    .elze
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                let finally = expr_try
                    .finally
                    .as_ref()
                    .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                    .transpose()?
                    .map(Box::new);
                hir::ExprKind::Try(hir::TryExpr {
                    expr: body,
                    catches,
                    elze,
                    finally,
                })
            }
            ExprKind::Await(expr_await) => {
                let inner_expr = self.transform_expr_to_hir(expr_await.base.as_ref())?;
                return Ok(hir::Expr {
                    hir_id,
                    kind: inner_expr.kind,
                    span,
                });
            }
            ExprKind::Async(async_expr) => {
                let inner_expr = self.transform_expr_to_hir(async_expr.expr.as_ref())?;
                return Ok(hir::Expr {
                    hir_id,
                    kind: inner_expr.kind,
                    span,
                });
            }
            ExprKind::For(for_expr) => {
                let kind = self.transform_for_to_hir(for_expr)?;
                return Ok(hir::Expr { hir_id, kind, span });
            }
            ExprKind::Closure(_closure) => {
                self.add_error(
                    Diagnostic::error("closure lowering not implemented".to_string())
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(expr_span),
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
                    self.error_placeholder_expr_kind(
                        "unsupported dynamic expression payload for `Any` node".to_string(),
                        expr_span,
                    )
                }
            }
            ExprKind::Macro(mac) => self.error_placeholder_expr_kind(
                format!(
                    "macro `{}` was not lowered during normalization",
                    mac.invocation.path
                ),
                expr_span,
            ),
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
            ExprKind::ConstBlock(const_block) => {
                self.transform_const_block_to_hir(ast_expr, const_block)?
            }
            ExprKind::IntrinsicContainer(container) => {
                self.transform_intrinsic_container_to_hir(container)?
            }
            ExprKind::IntrinsicCall(call) => self.transform_intrinsic_call_to_hir(call)?,
            ExprKind::Reference(reference) => {
                let inner = self.transform_expr_to_hir(reference.referee.as_ref())?;
                let mutable = match reference.mutable {
                    Some(true) => hir::ty::Mutability::Mut,
                    _ => hir::ty::Mutability::Not,
                };
                hir::ExprKind::Reference(hir::ExprReference {
                    hir_id: self.next_id(),
                    mutable,
                    expr: Box::new(inner),
                })
            }
            ExprKind::Dereference(deref) => {
                let inner = self.transform_expr_to_hir(deref.referee.as_ref())?;
                hir::ExprKind::Unary(hir::UnOp::Deref, Box::new(inner))
            }
            _ => self.error_placeholder_expr_kind(
                format!(
                    "unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                ),
                expr_span,
            ),
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    fn transform_const_block_to_hir(
        &mut self,
        ast_expr: &ast::Expr,
        const_block: &ast::ExprConstBlock,
    ) -> Result<hir::ExprKind> {
        let body = Box::new(self.transform_expr_to_hir(const_block.expr.as_ref())?);
        let ty = ast_expr
            .ty()
            .map(|ty| self.transform_type_to_hir(ty))
            .transpose()?
            .unwrap_or_else(|| self.create_unit_type());
        Ok(hir::ExprKind::ConstBlock(hir::ExprConstBlock {
            ty: Box::new(ty),
            body,
        }))
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
        hir::DefId::new(self.package_id, id)
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
            Value::UInt(u) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(u.value as i64))),
            // `hir::Lit::Integer` is `i64`-only — no arbitrary-precision HIR
            // literal exists. Best-effort narrow (saturating on overflow,
            // matching `fp-kotlin`'s own `Value::BigInt` rendering, which
            // already accepts the same imprecision for values this large).
            Value::BigInt(b) => {
                let narrowed = b.value.to_string().parse::<i64>().unwrap_or(i64::MAX);
                Ok(hir::ExprKind::Literal(hir::Lit::Integer(narrowed)))
            }
            Value::Bool(b) => Ok(hir::ExprKind::Literal(hir::Lit::Bool(b.value))),
            Value::String(s) => Ok(hir::ExprKind::Literal(hir::Lit::Str(s.value.clone()))),
            Value::Bytes(bytes) => {
                if let Some(text) = Self::borrowed_string_from_bytes(bytes) {
                    Ok(hir::ExprKind::Literal(hir::Lit::Str(text)))
                } else {
                    Ok(self.error_placeholder_expr_kind(
                        "byte values are not supported in AST→HIR expression lowering"
                            .to_string(),
                        value.span(),
                    ))
                }
            }
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
            // Deferred const-block types (Escaped) — placeholder until retry
            Value::Escaped(_) => {
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new("__fp_escaped"),
                        args: None,
                    }],
                    res: None,
                };
                Ok(hir::ExprKind::Path(path))
            }
            // Type-level values (Ty::Type, used by type(Config) etc.)
            Value::Type(_) => {
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new("__fp_type"),
                        args: None,
                    }],
                    res: None,
                };
                Ok(hir::ExprKind::Path(path))
            }
            Value::Function(func) => {
                let name = func.sig.name.clone().unwrap_or_else(|| {
                    self.add_error(
                        Diagnostic::error(
                            "function value must have a name for HIR lowering".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(value.span()),
                    );
                    ast::Ident::new("__fp_error".to_string())
                });
                let name = Name::Ident(name);
                let path = self.name_to_hir_path_with_scope(&name, PathResolutionScope::Value)?;
                Ok(hir::ExprKind::Path(path))
            }
            _ => Ok(self.error_placeholder_expr_kind(
                format!(
                    "unimplemented AST value type for HIR transformation: {:?}",
                    std::mem::discriminant(value.as_ref())
                ),
                value.span(),
            )),
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
        let op = self.convert_unop_kind(&unop.op, unop.span())?;

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
            ast::ExprInvokeTarget::Function(name) => {
                if let Some(ident) = name.as_ident() {
                    if ident.as_str() == "import" {
                        return Ok(self.error_placeholder_expr_kind(
                            "dynamic import is only supported in interpret mode".to_string(),
                            invoke.span(),
                        ));
                    }
                }
                if self.intrinsic_normalizer.is_none() {
                    if let Some(intrinsic_call) = ast::intrinsic_call_from_invoke(invoke) {
                        return self.transform_intrinsic_call_to_hir(&intrinsic_call);
                    }
                }

                let func_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(
                        self.name_to_hir_path_with_scope(name, PathResolutionScope::Value)?,
                    ),
                    span: self.create_span(1),
                };
                let args =
                    self.transform_call_args_bound(&invoke.args, &invoke.kwargs, Some(&func_expr))?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let func_expr = self.transform_expr_to_hir(expr)?;
                let args = self.transform_call_args_strict(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }

            _ => Ok(self.error_placeholder_expr_kind(
                format!(
                    "unimplemented invoke target type for HIR transformation: {:?}",
                    invoke.target
                ),
                invoke.span(),
            )),
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
            self.ast_expr_to_hir_path(struct_expr.name.as_ref(), PathResolutionScope::Value)?;
        let struct_span = struct_expr.span();

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
            Some(hir::Res::Def(def_id)) => {
                if let Some(fields) = self.struct_field_defs.get(&def_id).cloned() {
                    fields
                } else {
                    self.add_error(
                        Diagnostic::error(
                            "struct update requires a known struct field layout".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(struct_span),
                    );
                    return Ok(hir::ExprKind::Struct(path, fields));
                }
            }
            _ => {
                let segments = path
                    .segments
                    .iter()
                    .map(|seg| seg.name.as_str().to_string())
                    .collect::<Vec<_>>();
                let Some(alias) = self.lookup_type_alias(&segments) else {
                    self.add_error(
                        Diagnostic::error(
                            "struct update requires a resolved struct definition".to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(struct_span),
                    );
                    return Ok(hir::ExprKind::Struct(path, fields));
                };
                self.struct_fields_from_type(&alias, struct_span)?
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

    /// Transform a block node to HIR without wrapping it in an expression.
    pub(super) fn transform_block_node_to_hir(
        &mut self,
        block: &ast::ExprBlock,
    ) -> Result<hir::Block> {
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

            Ok(hir::Block {
                hir_id: self.next_id(),
                stmts,
                expr,
            })
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

        let scrutinee = scrutinee.ok_or_else(|| {
            fp_core::error::Error::from("match expressions without scrutinee are not supported")
        })?;

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
            if for_expr.pat.as_ident().is_some() {
                // Last resort: an arbitrary expression that isn't a range,
                // `.iter()`, or `.enumerate()` call but is nonetheless
                // Vec/slice-typed (e.g. `type(source).fields`, a
                // reflection intrinsic result) — index over it directly
                // rather than hard-erroring just because it lacks an
                // explicit `.iter()` suffix.
                return self.lower_bare_iter_for_loop(for_expr);
            }
            return Ok(self.error_placeholder_expr_kind(
                "`for` loop lowering only supports range iterators, iter(), and enumerate()"
                    .to_string(),
                for_expr.span(),
            ));
        }

        let (mut pat, _ty, _) = self.transform_pattern_with_metadata(&for_expr.pat)?;
        let (loop_name, loop_res) = match &mut pat.kind {
            hir::PatKind::Binding { name, .. } => (name.clone(), Some(hir::Res::Local(pat.hir_id))),
            _ => {
                return Ok(self.error_placeholder_expr_kind(
                    "`for` loop pattern must be a simple binding".to_string(),
                    for_expr.span(),
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
                return Ok(self.error_placeholder_expr_kind(
                    "`for` loop lowering currently only supports range iterators".to_string(),
                    for_expr.span(),
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

        let end_expr = match end_expr {
            Some(expr) => expr,
            None => {
                self.add_error(
                    Diagnostic::error("`for` loop range missing end expression".to_string())
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(for_expr.span()),
                );
                hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Integer(0)),
                    span: Span::new(self.current_file, 0, 0),
                }
            }
        };

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
        let (segments, base_prefix) = match &invoke.target {
            ast::ExprInvokeTarget::Function(name) => match name {
                ast::Name::Path(path) => (path.segments.clone(), path.prefix),
                ast::Name::Ident(ident) => (vec![ident.clone()], PathPrefix::Plain),
                ast::Name::ParameterPath(path) => (
                    path.segments.iter().map(|seg| seg.ident.clone()).collect(),
                    path.prefix,
                ),
            },
            ast::ExprInvokeTarget::Method(select) => {
                let Some(mut base) = self.path_segments_from_expr(&select.obj) else {
                    return Ok(None);
                };
                base.push(select.field.clone());
                (base, PathPrefix::Plain)
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let Some(segments) = self.path_segments_from_expr(expr) else {
                    return Ok(None);
                };
                (segments, PathPrefix::Plain)
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
            self.add_error(
                Diagnostic::error("enumerate() base path is empty".to_string())
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
            );
            return Ok(None);
        }

        let tuple = match for_expr.pat.kind() {
            ast::PatternKind::Tuple(tuple) => tuple,
            _ => {
                self.add_error(
                    Diagnostic::error(
                        "enumerate() loop pattern must be a tuple of bindings".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
                );
                return Ok(None);
            }
        };
        if tuple.patterns.len() != 2 {
            self.add_error(
                Diagnostic::error("enumerate() loop pattern must bind (index, value)".to_string())
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
            );
            return Ok(None);
        }

        let index_ident = match tuple.patterns.get(0).and_then(|pat| pat.as_ident()) {
            Some(ident) => ident.clone(),
            None => {
                self.add_error(
                    Diagnostic::error(
                        "enumerate() loop index must be a simple binding".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
                );
                return Ok(None);
            }
        };
        let value_ident = match tuple.patterns.get(1).and_then(|pat| pat.as_ident()) {
            Some(ident) => ident.clone(),
            None => {
                self.add_error(
                    Diagnostic::error(
                        "enumerate() loop value must be a simple binding".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
                );
                return Ok(None);
            }
        };

        Ok(Some(EnumerateLoopSpec {
            base_prefix,
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
        let (segments, base_prefix) = match &invoke.target {
            ast::ExprInvokeTarget::Function(name) => match name {
                ast::Name::Path(path) => (path.segments.clone(), path.prefix),
                ast::Name::Ident(ident) => (vec![ident.clone()], PathPrefix::Plain),
                ast::Name::ParameterPath(path) => (
                    path.segments.iter().map(|seg| seg.ident.clone()).collect(),
                    path.prefix,
                ),
            },
            ast::ExprInvokeTarget::Method(select) => {
                let Some(mut base) = self.path_segments_from_expr(&select.obj) else {
                    return Ok(None);
                };
                base.push(select.field.clone());
                (base, PathPrefix::Plain)
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let Some(segments) = self.path_segments_from_expr(expr) else {
                    return Ok(None);
                };
                (segments, PathPrefix::Plain)
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
            self.add_error(
                Diagnostic::error("iter() base path is empty".to_string())
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(for_expr.span()),
            );
            return Ok(None);
        }

        let value_ident = match for_expr.pat.as_ident() {
            Some(ident) => ident.clone(),
            None => {
                self.add_error(
                    Diagnostic::error("iter() loop pattern must be a simple binding".to_string())
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(for_expr.span()),
                );
                return Ok(None);
            }
        };

        Ok(Some(IterLoopSpec {
            base_prefix,
            base_segments,
            value_ident,
        }))
    }

    fn lower_enumerate_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        spec: EnumerateLoopSpec,
    ) -> Result<hir::ExprKind> {
        use fp_core::intrinsics::IntrinsicKind;

        let mut stmts = Vec::new();

        let base_path = ast::Path::new(spec.base_prefix, spec.base_segments.clone());
        let base_name = ast::Name::path(base_path);
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.name_to_hir_path_with_scope(&base_name, PathResolutionScope::Value)?,
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
            // `IntrinsicKind::Len` returns `u64`, but the synthesized loop
            // index (`idx_expr`, initialized from an untyped integer
            // literal) defaults to `i64` — and needs to stay `i64` since
            // it's also used to index `base_expr` below, which requires an
            // `i64` index. Cast the length to `i64` here rather than
            // changing the index's type, to avoid the mismatch without
            // disturbing indexing.
            let len_call = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: IntrinsicKind::Len,
                    callargs: vec![hir::CallArg {
                        name: hir::Symbol::new("arg0"),
                        value: base_expr.clone(),
                    }],
                }),
                span: Span::new(self.current_file, 0, 0),
            };
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Cast(
                    Box::new(len_call),
                    Box::new(self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))),
                ),
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
        use fp_core::intrinsics::IntrinsicKind;

        let base_path = ast::Path::new(spec.base_prefix, spec.base_segments.clone());
        let base_name = ast::Name::path(base_path);
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.name_to_hir_path_with_scope(&base_name, PathResolutionScope::Value)?,
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let len_expr = if let Some(len) = self.lookup_const_list_length(&spec.base_segments) {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Literal(hir::Lit::Integer(len as i64)),
                span: Span::new(self.current_file, 0, 0),
            }
        } else {
            // See the matching comment in `lower_enumerate_for_loop`:
            // `Len` returns `u64`, but the loop index defaults to (and
            // must stay) `i64` to satisfy indexing, so cast here instead.
            let len_call = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: IntrinsicKind::Len,
                    callargs: vec![hir::CallArg {
                        name: hir::Symbol::new("arg0"),
                        value: base_expr.clone(),
                    }],
                }),
                span: Span::new(self.current_file, 0, 0),
            };
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Cast(
                    Box::new(len_call),
                    Box::new(self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))),
                ),
                span: Span::new(self.current_file, 0, 0),
            }
        };

        self.build_indexed_for_loop(for_expr, Vec::new(), base_expr, len_expr, &spec.value_ident)
    }

    /// Desugars `for field in <expr>` when `<expr>` isn't a `.iter()`/
    /// `.enumerate()` call or a range — the last-resort shape for an
    /// arbitrary expression that already evaluates to a Vec/slice-typed
    /// value (e.g. `type(source).fields`, a reflection intrinsic result).
    /// Unlike `lower_iter_for_loop`, which cheaply re-derives its base
    /// expression from path segments, `<expr>` here may not be a
    /// side-effect-free path, so it's lowered and evaluated exactly once
    /// into a synthetic local, which the index/length machinery then reads
    /// from repeatedly.
    fn lower_bare_iter_for_loop(&mut self, for_expr: &ast::ExprFor) -> Result<hir::ExprKind> {
        use fp_core::intrinsics::IntrinsicKind;

        let value_ident = match for_expr.pat.as_ident() {
            Some(ident) => ident.clone(),
            None => {
                return Ok(self.error_placeholder_expr_kind(
                    "`for` loop pattern must be a simple binding".to_string(),
                    for_expr.span(),
                ));
            }
        };

        let base_lowered = self.transform_expr_to_hir(&for_expr.iter)?;
        let base_hir_id = self.next_id();
        let base_name = hir::Symbol::new(format!("__fp_iter_base{}", base_hir_id));
        let base_pat = hir::Pat {
            hir_id: base_hir_id,
            kind: hir::PatKind::Binding {
                name: base_name.clone(),
                mutable: false,
            },
        };
        let base_local = hir::Local {
            hir_id: self.next_id(),
            pat: base_pat.clone(),
            ty: None,
            init: Some(base_lowered),
        };
        self.register_pattern_bindings(&base_pat);
        let base_local_stmt = hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(base_local),
        };
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: base_name,
                    args: None,
                }],
                res: Some(hir::Res::Local(base_pat.hir_id)),
            }),
            span: Span::new(self.current_file, 0, 0),
        };

        // See the matching comment in `lower_enumerate_for_loop`: `Len`
        // returns `u64`, but the loop index defaults to (and must stay)
        // `i64` to satisfy indexing, so cast here instead.
        let len_call = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                kind: IntrinsicKind::Len,
                callargs: vec![hir::CallArg {
                    name: hir::Symbol::new("arg0"),
                    value: base_expr.clone(),
                }],
            }),
            span: Span::new(self.current_file, 0, 0),
        };
        let len_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Cast(
                Box::new(len_call),
                Box::new(self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))),
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        self.build_indexed_for_loop(
            for_expr,
            vec![base_local_stmt],
            base_expr,
            len_expr,
            &value_ident,
        )
    }

    /// Shared index-based desugaring for both `lower_iter_for_loop` and
    /// `lower_bare_iter_for_loop`: `let mut idx = 0; while idx < <len_expr>
    /// { let <value_ident> = <base_expr>[idx]; <body>; idx += 1; }`,
    /// prefixed by whatever setup statements the caller already needs
    /// (e.g. binding `base_expr` to a local).
    fn build_indexed_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        mut stmts: Vec<hir::Stmt>,
        base_expr: hir::Expr,
        len_expr: hir::Expr,
        value_ident: &ast::Ident,
    ) -> Result<hir::ExprKind> {
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
                name: hir::Symbol::new(value_ident.name.clone()),
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
            ast::ExprKind::Name(name) => match name {
                ast::Name::Path(path) => Some(path.segments.clone()),
                ast::Name::Ident(ident) => Some(vec![ident.clone()]),
                ast::Name::ParameterPath(path) => {
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
                    ast::ExprInvokeTarget::Function(name) => match name {
                        ast::Name::Path(path) => Some(path.segments.clone()),
                        ast::Name::Ident(ident) => Some(vec![ident.clone()]),
                        ast::Name::ParameterPath(path) => {
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
                if let ast::ExprKind::Let(let_expr) = expr_stmt.expr.kind() {
                    let pat = self.transform_pattern(&let_expr.pat)?;
                    self.register_pattern_bindings(&pat);
                    let init = self.transform_expr_to_hir(&let_expr.expr)?;
                    let local = hir::Local {
                        hir_id: self.next_id(),
                        pat,
                        ty: None,
                        init: Some(init),
                    };
                    hir::StmtKind::Local(local)
                } else {
                    let expr = self.transform_expr_to_hir(&expr_stmt.expr)?;
                    if expr_stmt.has_value() {
                        hir::StmtKind::Expr(expr)
                    } else {
                        hir::StmtKind::Semi(expr)
                    }
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
                let stmt_span = self.create_span(1);
                let placeholder = self.error_placeholder_expr_kind(
                    format!(
                        "unimplemented block statement type for HIR transformation: {:?}",
                        stmt
                    ),
                    stmt_span,
                );
                hir::StmtKind::Semi(hir::Expr {
                    hir_id: self.next_id(),
                    kind: placeholder,
                    span: stmt_span,
                })
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
    pub(super) fn convert_unop_kind(&mut self, op: &UnOpKind, span: Span) -> Result<hir::UnOp> {
        match op {
            UnOpKind::Neg => Ok(hir::UnOp::Neg),
            UnOpKind::Not => Ok(hir::UnOp::Not),
            UnOpKind::Deref => Ok(hir::UnOp::Deref),
            UnOpKind::Any(kind) => {
                if kind.as_str() == "box" {
                    return Ok(hir::UnOp::Box);
                }
                self.add_error(
                    Diagnostic::error(format!(
                        "Unsupported unary operator variant encountered during AST→HIR lowering: {:?}",
                        kind
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(hir::UnOp::Not)
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

        if matches!(
            call.kind,
            CallKind::Op(OpKind::Print)
                | CallKind::Op(OpKind::Println)
                | CallKind::Op(OpKind::Format)
        ) {
            let mut existing = callargs
                .iter()
                .map(|arg| arg.name.as_str().to_string())
                .collect::<std::collections::HashSet<_>>();

            let captured_names = callargs
                .first()
                .and_then(|first| match &first.value.kind {
                    hir::ExprKind::FormatString(template) => Some(
                        template
                            .parts
                            .iter()
                            .filter_map(|part| match part {
                                hir::FormatTemplatePart::Placeholder(placeholder) => {
                                    match &placeholder.arg_ref {
                                        hir::FormatArgRef::Named(name) => {
                                            Some(name.as_str().to_string())
                                        }
                                        _ => None,
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>(),
                    ),
                    _ => None,
                })
                .unwrap_or_default();

            for name in captured_names {
                if existing.contains(&name) {
                    continue;
                }

                let path = self.name_to_hir_path_with_scope(
                    &Name::Ident(ast::Ident::new(name.as_str())),
                    PathResolutionScope::Value,
                )?;
                let value = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(path),
                    span: self.create_span(1),
                };

                callargs.push(hir::CallArg {
                    name: hir::Symbol::new(name.clone()),
                    value,
                });
                existing.insert(name);
            }
        }

        let kind = call
            .kind
            .intrinsic_kind()
            .ok_or_else(|| fp_core::error::Error::from("high-level op reached the compiler HIR"))?;
        Ok(hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
            kind,
            callargs,
        }))
    }

    pub(super) fn transform_call_args_bound(
        &mut self,
        args: &[ast::Expr],
        kwargs: &[ast::ExprKwArg],
        callee: Option<&hir::Expr>,
    ) -> Result<Vec<hir::CallArg>> {
        let mut values = Vec::with_capacity(args.len() + kwargs.len());
        for arg in args {
            values.push((
                hir::Symbol::new(format!("arg{}", values.len())),
                self.transform_expr_to_hir(arg)?,
            ));
        }
        for kwarg in kwargs {
            values.push((
                kwarg.name.clone().into(),
                self.transform_expr_to_hir(&kwarg.value)?,
            ));
        }
        let Some((param_names, is_variadic)) = callee
            .and_then(|expr| match &expr.kind {
                hir::ExprKind::Path(path) => path.res.as_ref(),
                _ => None,
            })
            .and_then(|res| match res {
                hir::Res::Def(def_id) => Some(*def_id),
                _ => None,
            })
            .and_then(|def_id| self.program_def_param_info(def_id))
        else {
            return Ok(values
                .into_iter()
                .map(|(name, value)| hir::CallArg { name, value })
                .collect());
        };

        if values.len() != param_names.len() {
            if is_variadic {
                let required = param_names.len().saturating_sub(1);
                if values.len() >= required {
                    return Ok(values
                        .into_iter()
                        .map(|(name, value)| hir::CallArg { name, value })
                        .collect());
                }
            }
            let span = args
                .first()
                .map(|arg| arg.span())
                .unwrap_or_else(Span::null);
            self.add_error(
                Diagnostic::error(
                    "call arguments do not match function parameter count".to_string(),
                )
                .with_source_context(DIAGNOSTIC_CONTEXT)
                .with_span(span),
            );
            return Ok(values
                .into_iter()
                .map(|(name, value)| hir::CallArg { name, value })
                .collect());
        }

        let mut ordered = vec![None; param_names.len()];
        for (index, (name, value)) in values.into_iter().enumerate() {
            let target = name
                .as_str()
                .strip_prefix("arg")
                .and_then(|index| index.parse::<usize>().ok())
                .filter(|target| *target == index)
                .or_else(|| {
                    param_names
                        .iter()
                        .position(|param| param.as_str() == name.as_str())
                });

            let Some(target) = target else {
                self.add_error(
                    Diagnostic::error(format!("unknown named argument `{name}` in call"))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(
                            args.first()
                                .map(|arg| arg.span())
                                .unwrap_or_else(Span::null),
                        ),
                );
                return Ok(Vec::new());
            };
            if ordered[target].is_some() {
                self.add_error(
                    Diagnostic::error(format!("duplicate argument `{name}` in call"))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(
                            args.first()
                                .map(|arg| arg.span())
                                .unwrap_or_else(Span::null),
                        ),
                );
                return Ok(Vec::new());
            }
            ordered[target] = Some(value);
        }

        Ok(ordered
            .into_iter()
            .enumerate()
            .map(|(index, value)| hir::CallArg {
                name: param_names[index].clone(),
                value: value.expect("argument count was checked against the function signature"),
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

    pub(super) fn program_def_param_info(
        &self,
        def_id: hir::DefId,
    ) -> Option<(Vec<hir::Symbol>, bool)> {
        let Some(item) = self.program_def_map.get(&def_id) else {
            return None;
        };
        match &item.kind {
            hir::ItemKind::Function(function) => {
                let mut names = Vec::with_capacity(function.sig.inputs.len());
                for param in &function.sig.inputs {
                    match &param.pat.kind {
                        hir::PatKind::Binding { name, .. } => names.push(name.clone()),
                        _ => return None,
                    }
                }
                let is_variadic = function
                    .sig
                    .inputs
                    .last()
                    .map(|param| matches!(param.ty.kind, hir::TypeExprKind::Infer))
                    .unwrap_or(false);
                Some((names, is_variadic))
            }
            _ => None,
        }
    }

    // name_to_hir_path_with_scope moved to helpers.rs

    // ast_expr_to_hir_path moved to helpers.rs

    // convert_generic_args moved to helpers.rs

    // canonicalize_segments moved to helpers.rs

    pub(super) fn lookup_global_res(
        &self,
        path: &fp_core::module::path::QualifiedPath,
        scope: PathResolutionScope,
    ) -> Option<hir::Res> {
        if path.segments.is_empty() {
            return None;
        }
        let key = path.to_key();
        let local = match scope {
            PathResolutionScope::Value => self.lookup_symbol(&key, &self.global_value_defs),
            PathResolutionScope::Type => self.lookup_symbol(&key, &self.global_type_defs),
        };
        // A cross-package export (e.g. `libc::macos::getenv`) is looked up
        // lazily against the workspace on a local-lookup miss, instead of
        // being eagerly copied into `global_value_defs`/`global_type_defs`
        // up front (see `seed_workspace_definitions`).
        local.or_else(|| self.workspace.as_ref()?.find_export(&key))
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
                    ast::TypeBinaryOpKind::Union => {
                        self.add_error(
                            Diagnostic::error(
                                "struct update does not support union type operands".to_string(),
                            )
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(span),
                        );
                        Ok(Vec::new())
                    }
                }
            }
            ast::Ty::Expr(expr) => {
                if let ast::ExprKind::Name(name) = expr.kind() {
                    let path = name.to_path();
                    let segments = path
                        .segments
                        .iter()
                        .map(|seg| seg.name.clone())
                        .collect::<Vec<_>>();
                    if let Some(alias) = self.lookup_type_alias(&segments) {
                        return self.struct_fields_from_type(&alias, span);
                    }
                }
                self.add_error(
                    Diagnostic::error(
                        "struct update requires a resolved struct definition".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(Vec::new())
            }
            _ => {
                self.add_error(
                    Diagnostic::error(
                        "struct update requires a resolved struct definition".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(span),
                );
                Ok(Vec::new())
            }
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
                    self.add_error(
                        Diagnostic::error(format!(
                            "conflicting field types for `{}` in structural merge",
                            field.name.name
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(Span::union([field.value.span(), existing.span()])),
                    );
                    continue;
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
                    self.add_error(
                        Diagnostic::error(format!(
                            "conflicting field types for `{}` in structural intersect",
                            field.name.name
                        ))
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(Span::union([field.value.span(), rhs_ty.span()])),
                    );
                    continue;
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
