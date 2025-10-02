use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir;
use fp_core::mir::ty::{ErrorGuaranteed, FloatTy, IntTy, Ty, TyKind};
use fp_core::span::Span;

use super::IrTransform;

const DIAGNOSTIC_CONTEXT: &str = "hir→mir";

/// Minimal HIR → MIR lowering pass.
///
/// This currently produces skeletal MIR that is sufficient to feed the
/// downstream MIR→LIR/LLVM pipeline. Unsupported constructs surface diagnostics
/// so callers can decide whether to abort or continue.
pub struct MirLowering {
    next_mir_id: mir::MirId,
    next_body_id: u32,
    next_error_id: u32,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
}

impl MirLowering {
    pub fn new() -> Self {
        Self {
            next_mir_id: 0,
            next_body_id: 0,
            next_error_id: 0,
            diagnostics: Vec::new(),
            has_errors: false,
        }
    }

    fn lower_program(&mut self, program: &hir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Function(function) => {
                    let (mir_item, body_id, body) = self.lower_function(item, function)?;
                    mir_program.items.push(mir_item);
                    mir_program.bodies.insert(body_id, body);
                }
                hir::ItemKind::Const(const_item) => {
                    let mir_item = self.lower_const(const_item)?;
                    mir_program.items.push(mir_item);
                }
                hir::ItemKind::Struct(_) | hir::ItemKind::Impl(_) => {
                    self.emit_error(item.span, "structs and impls are not yet lowered to MIR");
                }
            }
        }

        Ok(mir_program)
    }

    fn lower_function(
        &mut self,
        item: &hir::Item,
        function: &hir::Function,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let sig = self.lower_function_sig(&function.sig);
        let mir_body = self.lower_body(function, &sig, item.span)?;

        let mir_function = mir::Function {
            name: function.sig.name.clone(),
            path: Vec::new(),
            def_id: Some(item.def_id),
            sig,
            body_id,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body))
    }

    fn lower_function_sig(&mut self, sig: &hir::FunctionSig) -> mir::FunctionSig {
        mir::FunctionSig {
            inputs: sig
                .inputs
                .iter()
                .map(|param| self.lower_type_expr(&param.ty))
                .collect(),
            output: self.lower_type_expr(&sig.output),
        }
    }

    fn lower_body(
        &mut self,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        span: Span,
    ) -> Result<mir::Body> {
        let span = function
            .body
            .as_ref()
            .map(|body| body.value.span)
            .unwrap_or(span);

        let mut locals = Vec::new();
        locals.push(self.make_local_decl(&sig.output, span));
        for ty in &sig.inputs {
            locals.push(self.make_local_decl(ty, span));
        }

        let mut block = mir::BasicBlockData::new(None);

        if let Some(body) = &function.body {
            if let Some(constant) = self.lower_literal_expr(&body.value) {
                block.statements.push(mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(0),
                        mir::Rvalue::Use(mir::Operand::Constant(constant)),
                    ),
                });
            } else {
                self.emit_error(
                    span,
                    "only literal function bodies are lowered to MIR for now",
                );
            }
        }

        block.terminator = Some(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Return,
        });

        let body = mir::Body::new(vec![block], locals, sig.inputs.len(), span);
        Ok(body)
    }

    fn lower_const(&mut self, konst: &hir::Const) -> Result<mir::Item> {
        let ty = self.lower_type_expr(&konst.ty);
        let init_constant = self
            .lower_literal_expr(&konst.body.value)
            .unwrap_or_else(|| self.error_constant(konst.body.value.span));
        let init = mir::Operand::Constant(init_constant);

        let mir_static = mir::Static {
            ty,
            init,
            mutability: mir::Mutability::Not,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Static(mir_static),
        };
        self.next_mir_id += 1;

        Ok(mir_item)
    }

    fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        match &ty_expr.kind {
            hir::TypeExprKind::Primitive(primitive) => {
                self.lower_primitive_type(primitive, ty_expr.span)
            }
            hir::TypeExprKind::Tuple(elements) => Ty {
                kind: TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.lower_type_expr(elem)))
                        .collect(),
                ),
            },
            _ => {
                self.emit_error(ty_expr.span, "type lowering not yet supported");
                self.error_ty()
            }
        }
    }

    fn lower_primitive_type(&mut self, primitive: &TypePrimitive, span: Span) -> Ty {
        match primitive {
            TypePrimitive::Bool => Ty { kind: TyKind::Bool },
            TypePrimitive::Char => Ty { kind: TyKind::Char },
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => Ty {
                    kind: TyKind::Int(IntTy::I8),
                },
                TypeInt::I16 => Ty {
                    kind: TyKind::Int(IntTy::I16),
                },
                TypeInt::I32 => Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
                TypeInt::I64 => Ty {
                    kind: TyKind::Int(IntTy::I64),
                },
                TypeInt::BigInt | TypeInt::U64 | TypeInt::U32 | TypeInt::U16 | TypeInt::U8 => {
                    self.emit_error(span, "unsigned/big integers are not yet supported in MIR");
                    self.error_ty()
                }
            },
            TypePrimitive::Decimal(decimal) => match decimal {
                DecimalType::F32 => Ty {
                    kind: TyKind::Float(FloatTy::F32),
                },
                DecimalType::F64 => Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
                DecimalType::BigDecimal | DecimalType::Decimal { .. } => {
                    self.emit_error(
                        span,
                        "arbitrary precision decimals are not supported in MIR",
                    );
                    self.error_ty()
                }
            },
            TypePrimitive::String | TypePrimitive::List => {
                self.emit_error(span, "string/list primitives are not yet supported in MIR");
                self.error_ty()
            }
        }
    }

    fn make_local_decl(&mut self, ty: &Ty, span: Span) -> mir::LocalDecl {
        mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty: ty.clone(),
            user_ty: None,
            source_info: span,
        }
    }

    fn lower_literal_expr(&mut self, expr: &hir::Expr) -> Option<mir::Constant> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(mir::Constant {
                span: expr.span,
                user_ty: None,
                literal: self.lower_literal(lit),
            }),
            _ => {
                self.emit_error(expr.span, "only literal expressions are supported in MIR");
                None
            }
        }
    }

    fn lower_literal(&self, lit: &hir::Lit) -> mir::ConstantKind {
        match lit {
            hir::Lit::Bool(value) => mir::ConstantKind::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstantKind::Int(*value),
            hir::Lit::Float(value) => mir::ConstantKind::Float(*value),
            hir::Lit::Str(value) => mir::ConstantKind::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstantKind::Int(*value as i64),
        }
    }

    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        self.has_errors = true;
        let diagnostic = Diagnostic::error(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn error_ty(&mut self) -> Ty {
        let error = ErrorGuaranteed {
            index: self.next_error_id,
        };
        self.next_error_id += 1;
        Ty {
            kind: TyKind::Error(error),
        }
    }

    fn error_constant(&mut self, span: Span) -> mir::Constant {
        self.emit_error(span, "unable to lower expression to a constant");
        mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Bool(false),
        }
    }

    pub fn take_diagnostics(&mut self) -> (Vec<Diagnostic>, bool) {
        let diagnostics = std::mem::take(&mut self.diagnostics);
        let has_errors = std::mem::replace(&mut self.has_errors, false);
        (diagnostics, has_errors)
    }
}

impl Default for MirLowering {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> IrTransform<&'a hir::Program, mir::Program> for MirLowering {
    fn transform(&mut self, source: &'a hir::Program) -> Result<mir::Program> {
        self.lower_program(source)
    }
}
