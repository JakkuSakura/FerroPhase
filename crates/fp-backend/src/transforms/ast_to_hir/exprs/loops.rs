use super::*;

pub(super) struct EnumerateLoopSpec {
    base_prefix: PathPrefix,
    base_segments: Vec<ast::Ident>,
    index_ident: ast::Ident,
    value_ident: ast::Ident,
}

pub(super) struct IterLoopSpec {
    base_prefix: PathPrefix,
    base_segments: Vec<ast::Ident>,
    value_pat: ast::Pattern,
}

impl AstToHirLowerer {
    pub(super) fn extract_enumerate_loop_spec(
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
                ast::Name { path, .. } => (
                    path.segments.iter().map(|s| s.ident.clone()).collect(),
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

    pub(super) fn extract_iter_loop_spec(
        &mut self,
        for_expr: &ast::ExprFor,
    ) -> Result<Option<IterLoopSpec>> {
        let ast::ExprKind::Invoke(invoke) = for_expr.iter.kind() else {
            return Ok(None);
        };
        if !invoke.args.is_empty() {
            return Ok(None);
        }
        let (segments, base_prefix) = match &invoke.target {
            ast::ExprInvokeTarget::Function(name) => match name {
                ast::Name { path, .. } => (
                    path.segments.iter().map(|s| s.ident.clone()).collect(),
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

        Ok(Some(IterLoopSpec {
            base_prefix,
            base_segments,
            value_pat: (*for_expr.pat).clone(),
        }))
    }

    pub(super) fn lower_enumerate_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        spec: EnumerateLoopSpec,
    ) -> Result<hir::ExprKind> {
        let mut stmts = Vec::new();

        let base_path = ast::Path::new(
            spec.base_prefix,
            spec.base_segments.iter().cloned().map(Into::into).collect(),
        );
        let base_name = ast::Name::path(base_path);
        let base_ast = ast::Expr::new(ast::ExprKind::Name(base_name));
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.ast_expr_to_hir_path(
                    &base_ast,
                    PathResolutionScope::Value,
                    ParamMode::Optional,
                )?,
            ),
            span: Span::new(self.current_file, 0, 0),
        };

        let idx_hir_id = self.next_id();
        let idx_name = hir::Symbol::new(format!("__fp_idx{}", idx_hir_id.local_id.0));
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
            ty: Some(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            init: Some(idx_init),
        };
        self.register_pattern_bindings(&idx_pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(idx_local),
        });

        let idx_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                segments: vec![hir::PathSegment {
                    ident: idx_name.clone(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Local(idx_pat.hir_id.clone()),
                }],
                res: hir::Res::Local(idx_pat.hir_id),
            })),
            span: Span::new(self.current_file, 0, 0),
        };

        let len_expr = if let Some(len) = self.lookup_const_list_length(&spec.base_segments) {
            hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Literal(hir::Lit::Integer(len as i64)),
                span: Span::new(self.current_file, 0, 0),
            }
        } else {
            let len_call = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::MethodCall(
                    Box::new(base_expr.clone()),
                    hir::Symbol::new("len"),
                    None,
                    Vec::new(),
                ),
                span: Span::new(self.current_file, 0, 0),
            };
            hir::Expr {
                hir_id: self.next_id(),
                kind: len_call.kind,
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

    pub(super) fn lower_iter_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        spec: IterLoopSpec,
    ) -> Result<hir::ExprKind> {
        let base_path = ast::Path::new(
            spec.base_prefix,
            spec.base_segments.iter().cloned().map(Into::into).collect(),
        );
        let base_name = ast::Name::path(base_path);
        let base_ast = ast::Expr::new(ast::ExprKind::Name(base_name));
        let base_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(
                self.ast_expr_to_hir_path(
                    &base_ast,
                    PathResolutionScope::Value,
                    ParamMode::Optional,
                )?,
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
            let len_call = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::MethodCall(
                    Box::new(base_expr.clone()),
                    hir::Symbol::new("len"),
                    None,
                    Vec::new(),
                ),
                span: Span::new(self.current_file, 0, 0),
            };
            hir::Expr {
                hir_id: self.next_id(),
                kind: len_call.kind,
                span: Span::new(self.current_file, 0, 0),
            }
        };

        self.build_indexed_for_loop(for_expr, Vec::new(), base_expr, len_expr, &spec.value_pat)
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
    pub(super) fn lower_bare_iter_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
    ) -> Result<hir::ExprKind> {
        let value_pat = (*for_expr.pat).clone();

        let base_lowered = self.transform_expr_to_hir(&for_expr.iter)?;
        let base_hir_id = self.next_id();
        let base_name = hir::Symbol::new(format!("__fp_iter_base{}", base_hir_id.local_id.0));
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
            kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                segments: vec![hir::PathSegment {
                    ident: base_name,
                    args: None,
                    infer_args: true,
                    res: hir::Res::Local(base_pat.hir_id.clone()),
                }],
                res: hir::Res::Local(base_pat.hir_id),
            })),
            span: Span::new(self.current_file, 0, 0),
        };

        let len_call = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::MethodCall(
                Box::new(base_expr.clone()),
                hir::Symbol::new("len"),
                None,
                Vec::new(),
            ),
            span: Span::new(self.current_file, 0, 0),
        };
        let len_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: len_call.kind,
            span: Span::new(self.current_file, 0, 0),
        };

        self.build_indexed_for_loop(
            for_expr,
            vec![base_local_stmt],
            base_expr,
            len_expr,
            &value_pat,
        )
    }

    /// Shared index-based desugaring for both `lower_iter_for_loop` and
    /// `lower_bare_iter_for_loop`: `let mut idx = 0; while idx < <len_expr>
    /// { let <value_ident> = <base_expr>[idx]; <body>; idx += 1; }`,
    /// prefixed by whatever setup statements the caller already needs
    /// (e.g. binding `base_expr` to a local).
    pub(super) fn build_indexed_for_loop(
        &mut self,
        for_expr: &ast::ExprFor,
        mut stmts: Vec<hir::Stmt>,
        base_expr: hir::Expr,
        len_expr: hir::Expr,
        for_pat: &ast::Pattern,
    ) -> Result<hir::ExprKind> {
        let idx_hir_id = self.next_id();
        let idx_name = hir::Symbol::new(format!("__fp_idx{}", idx_hir_id.local_id.0));
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
            ty: Some(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            init: Some(idx_init),
        };
        self.register_pattern_bindings(&idx_pat);
        stmts.push(hir::Stmt {
            hir_id: self.next_id(),
            kind: hir::StmtKind::Local(idx_local),
        });

        let idx_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::QPath::resolved(hir::Path {
                segments: vec![hir::PathSegment {
                    ident: idx_name.clone(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Local(idx_pat.hir_id.clone()),
                }],
                res: hir::Res::Local(idx_pat.hir_id),
            })),
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

        // The loop pattern isn't always a plain identifier (e.g. `for (name,
        // _n) in list.iter()` destructures the indexed element directly) —
        // `transform_pattern_with_metadata` already lowers any pattern
        // shape (tuple, struct, wildcard, ...) the same way a `let`
        // binding's own pattern would be, so reuse it here instead of only
        // handling a bare identifier and silently producing an empty,
        // no-op loop body for anything else.
        let (value_pat, _ty, _) = self.transform_pattern_with_metadata(for_pat)?;
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
}
