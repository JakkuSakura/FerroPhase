use super::format::build_printf_format;
use fp_core::hir::typed::ty::{IntTy, Mutability, Ty, TyKind, TypeAndMut, UintTy};
use fp_core::hir::typed::{self as thir, Block, ExprKind, Stmt, StmtKind};
use fp_core::intrinsics::{BackendFlavor, IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::span::Span;

pub struct BackendThirMaterializer {
    backend: BackendFlavor,
    next_thir_id: thir::ThirId,
}

impl BackendThirMaterializer {
    pub fn new(backend: BackendFlavor) -> Self {
        Self {
            backend,
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
                let newline = matches!(kind, IntrinsicCallKind::Println);
                let mut template = match payload {
                    IntrinsicCallPayload::Format { template } => template,
                    IntrinsicCallPayload::Args { .. } => {
                        unreachable!("print intrinsics must carry format payload after lowering")
                    }
                };

                for arg in &mut template.args {
                    self.materialize_expr(arg);
                }

                match self.backend {
                    BackendFlavor::Llvm => {
                        let args_clone = template.args.clone();
                        let format_literal = build_printf_format(&template, &args_clone, newline);

                        let mut call_args = Vec::with_capacity(args_clone.len() + 1);
                        call_args.push(self.make_string_literal_expr(span, format_literal));
                        call_args.extend(args_clone.into_iter());

                        let fun = Box::new(self.make_std_io_println_path_expr(span, newline));

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
                    }
                    BackendFlavor::Interpreter
                    | BackendFlavor::TranspileRust
                    | BackendFlavor::TranspileJavascript => thir::Expr {
                        thir_id,
                        kind: ExprKind::IntrinsicCall(thir::ThirIntrinsicCall {
                            kind,
                            payload: IntrinsicCallPayload::Format { template },
                        }),
                        ty: self.create_unit_type(),
                        span,
                    },
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
        }
    }

    fn make_std_io_println_path_expr(&mut self, span: Span, newline: bool) -> thir::Expr {
        let name = if newline {
            "std::io::println"
        } else {
            "std::io::print"
        }
        .to_string();
        thir::Expr {
            thir_id: self.next_id(),
            kind: ExprKind::Path(thir::ItemRef { name, def_id: None }),
            ty: self.create_unit_type(),
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

    fn create_usize_type(&self) -> Ty {
        Ty {
            kind: TyKind::Uint(UintTy::Usize),
        }
    }
}
