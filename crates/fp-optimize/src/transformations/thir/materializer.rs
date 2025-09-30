use fp_core::hir::typed::ty::{IntTy, Mutability, Ty, TyKind, TypeAndMut, UintTy};
use fp_core::hir::typed::{self as thir, Block, ExprKind, Stmt, StmtKind};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload, IntrinsicMaterializer};
use fp_core::span::Span;

pub struct BackendThirMaterializer {
    materializer: &'static dyn IntrinsicMaterializer,
    next_thir_id: thir::ThirId,
}

impl BackendThirMaterializer {
    pub fn new(materializer: &'static dyn IntrinsicMaterializer) -> Self {
        Self {
            materializer,
            next_thir_id: 0,
        }
    }

    pub fn materialize(&mut self, mut program: thir::Program) -> thir::Program {
        self.next_thir_id = program.next_thir_id;

        for body in program.bodies.values_mut() {
            self.materialize_expr(&mut body.value);
        }

        program.next_thir_id = self.next_thir_id;
        program
    }

    fn next_id(&mut self) -> thir::ThirId {
        let id = self.next_thir_id;
        self.next_thir_id += 1;
        id
    }

    fn materialize_expr(&mut self, expr: &mut thir::Expr) {
        match &mut expr.kind {
            ExprKind::IntrinsicCall(call) => {
                let cloned = call.clone();
                let thir_id = expr.thir_id;
                let span = expr.span;
                *expr = self.materialize_std_call(thir_id, span, cloned);
            }
            ExprKind::Call { fun, args, .. } => {
                self.materialize_expr(fun);
                for arg in args {
                    self.materialize_expr(arg);
                }
            }
            ExprKind::Unary(_, value) => self.materialize_expr(value),
            ExprKind::Binary(_, lhs, rhs) => {
                self.materialize_expr(lhs);
                self.materialize_expr(rhs);
            }
            ExprKind::LogicalOp { lhs, rhs, .. } => {
                self.materialize_expr(lhs);
                self.materialize_expr(rhs);
            }
            ExprKind::Cast(value, _) => self.materialize_expr(value),
            ExprKind::Deref(value) => self.materialize_expr(value),
            ExprKind::Index(base, index) => {
                self.materialize_expr(base);
                self.materialize_expr(index);
            }
            ExprKind::Field { base, .. } => self.materialize_expr(base),
            ExprKind::Borrow { arg, .. } => self.materialize_expr(arg),
            ExprKind::AddressOf { arg, .. } => self.materialize_expr(arg),
            ExprKind::Return { value } | ExprKind::Break { value } => {
                if let Some(expr) = value.as_mut() {
                    self.materialize_expr(expr);
                }
            }
            ExprKind::Block(block) => self.materialize_block(block),
            ExprKind::Assign { lhs, rhs } | ExprKind::AssignOp { lhs, rhs, .. } => {
                self.materialize_expr(lhs);
                self.materialize_expr(rhs);
            }
            ExprKind::Scope { value, .. } => self.materialize_expr(value),
            ExprKind::If {
                cond,
                then,
                else_opt,
            } => {
                self.materialize_expr(cond);
                self.materialize_expr(then);
                if let Some(else_expr) = else_opt.as_mut() {
                    self.materialize_expr(else_expr);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.materialize_expr(scrutinee);
                for arm in arms {
                    if let Some(guard) = arm.guard.as_mut() {
                        self.materialize_expr(&mut guard.cond);
                    }
                    self.materialize_expr(&mut arm.body);
                }
            }
            ExprKind::Loop { body } => self.materialize_expr(body),
            ExprKind::Let { expr, .. } => self.materialize_expr(expr),
            ExprKind::Use(inner) => self.materialize_expr(inner),
            _ => {}
        }
    }

    fn materialize_block(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.materialize_stmt(stmt);
        }
        if let Some(expr) = block.expr.as_mut() {
            self.materialize_expr(expr);
        }
    }

    fn materialize_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Expr(expr) => self.materialize_expr(expr),
            StmtKind::Let { initializer, .. } => {
                if let Some(init) = initializer.as_mut() {
                    self.materialize_expr(init);
                }
            }
        }
    }

    fn materialize_std_call(
        &mut self,
        thir_id: thir::ThirId,
        span: Span,
        call: thir::ThirIntrinsicCall,
    ) -> thir::Expr {
        let thir::ThirIntrinsicCall { kind, payload } = call;

        match kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                let mut template = match payload {
                    IntrinsicCallPayload::Format { template } => template,
                    IntrinsicCallPayload::Args { .. } => {
                        unreachable!("print intrinsics must carry format payload after lowering")
                    }
                };

                // Materialize all arguments in the template
                for arg in &mut template.args {
                    self.materialize_expr(arg);
                }

                // Delegate to backend-specific materializer
                if let Some(materialized) = self.materializer.prepare_print(kind, &template) {
                    // Backend provides printf-style transformation
                    let mut call_args = Vec::with_capacity(template.args.len() + 1);
                    call_args
                        .push(self.make_string_literal_expr(span, materialized.format_literal));
                    call_args.extend(template.args.into_iter());

                    let fun =
                        Box::new(self.make_path_expr(span, &materialized.printf_function_name));

                    thir::Expr {
                        thir_id,
                        kind: ExprKind::Call {
                            fun,
                            args: call_args,
                            from_hir_call: true,
                        },
                        ty: self.create_unit_type(),
                        span,
                    }
                } else {
                    // Backend doesn't handle this at THIR level, keep as intrinsic call
                    thir::Expr {
                        thir_id,
                        kind: ExprKind::IntrinsicCall(thir::ThirIntrinsicCall {
                            kind,
                            payload: IntrinsicCallPayload::Format { template },
                        }),
                        ty: self.create_unit_type(),
                        span,
                    }
                }
            }
            IntrinsicCallKind::Len => {
                let args = match payload {
                    IntrinsicCallPayload::Args { args } => args
                        .into_iter()
                        .map(|mut arg| {
                            self.materialize_expr(&mut arg);
                            arg
                        })
                        .collect::<Vec<_>>(),
                    IntrinsicCallPayload::Format { .. } => {
                        unreachable!("len intrinsic should not carry format payload")
                    }
                };

                let fun = Box::new(self.make_std_builtin_path_expr(span, "std::builtins::len"));

                thir::Expr {
                    thir_id,
                    kind: ExprKind::Call {
                        fun,
                        args,
                        from_hir_call: true,
                    },
                    ty: self.create_usize_type(),
                    span,
                }
            }
            IntrinsicCallKind::ConstBlock => {
                let args = match payload {
                    IntrinsicCallPayload::Args { mut args } => {
                        for arg in &mut args {
                            self.materialize_expr(arg);
                        }
                        args
                    }
                    _ => unreachable!("const block intrinsic expects args payload"),
                };

                let result_ty = args
                    .first()
                    .map(|expr| expr.ty.clone())
                    .unwrap_or_else(|| self.create_unit_type());

                thir::Expr {
                    thir_id,
                    kind: ExprKind::IntrinsicCall(thir::ThirIntrinsicCall {
                        kind,
                        payload: IntrinsicCallPayload::Args { args },
                    }),
                    ty: result_ty,
                    span,
                }
            }
            IntrinsicCallKind::DebugAssertions => thir::Expr {
                thir_id,
                kind: ExprKind::IntrinsicCall(thir::ThirIntrinsicCall { kind, payload }),
                ty: self.create_bool_type(),
                span,
            },
            IntrinsicCallKind::Input => {
                let args = match payload {
                    IntrinsicCallPayload::Args { mut args } => {
                        for arg in &mut args {
                            self.materialize_expr(arg);
                        }
                        args
                    }
                    _ => Vec::new(),
                };

                thir::Expr {
                    thir_id,
                    kind: ExprKind::IntrinsicCall(thir::ThirIntrinsicCall {
                        kind,
                        payload: IntrinsicCallPayload::Args { args },
                    }),
                    ty: self.create_string_type(),
                    span,
                }
            }
            IntrinsicCallKind::Break | IntrinsicCallKind::Continue | IntrinsicCallKind::Return => {
                // These should never appear as intrinsic calls in THIR
                // They are converted to proper ExprKind variants in AST→HIR
                unreachable!(
                    "unexpected control flow intrinsic {:?} in THIR materializer",
                    kind
                );
            }
        }
    }

    fn make_path_expr(&mut self, span: Span, name: &str) -> thir::Expr {
        // Determine the type based on the function name
        let ty = if name == "printf" {
            self.create_printf_function_type()
        } else {
            self.create_unit_type()
        };

        thir::Expr {
            thir_id: self.next_id(),
            kind: ExprKind::Path(thir::ItemRef {
                name: name.to_string(),
                def_id: None,
            }),
            ty,
            span,
        }
    }

    fn make_std_builtin_path_expr(&mut self, span: Span, name: &str) -> thir::Expr {
        thir::Expr {
            thir_id: self.next_id(),
            kind: ExprKind::Path(thir::ItemRef {
                name: name.to_string(),
                def_id: None,
            }),
            ty: self.create_unit_type(),
            span,
        }
    }

    fn make_string_literal_expr(&mut self, span: Span, value: String) -> thir::Expr {
        thir::Expr {
            thir_id: self.next_id(),
            kind: ExprKind::Literal(thir::Lit::Str(value)),
            ty: self.create_string_type(),
            span,
        }
    }

    fn create_unit_type(&self) -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    fn create_string_type(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    fn create_bool_type(&self) -> Ty {
        Ty { kind: TyKind::Bool }
    }

    fn create_usize_type(&self) -> Ty {
        Ty {
            kind: TyKind::Uint(UintTy::Usize),
        }
    }

    fn create_printf_function_type(&self) -> Ty {
        use fp_core::hir::ty::{Abi, Binder, FnSig, PolyFnSig, Unsafety};

        // printf signature: fn(format: *const i8, ...) -> i32
        // inputs: [*const i8] (first parameter, rest are variadic)
        // output: i32
        // c_variadic: true
        let fn_sig = FnSig {
            inputs: vec![Box::new(self.create_string_type())],
            output: Box::new(Ty {
                kind: TyKind::Int(IntTy::I32),
            }),
            c_variadic: true,
            unsafety: Unsafety::Normal,
            abi: Abi::C { unwind: false },
        };

        Ty {
            kind: TyKind::FnPtr(PolyFnSig {
                binder: Binder {
                    value: fn_sig,
                    bound_vars: Vec::new(),
                },
            }),
        }
    }
}
