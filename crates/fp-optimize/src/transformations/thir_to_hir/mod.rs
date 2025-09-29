use std::collections::HashMap;

use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::Result;
use fp_core::hir::{self, typed as thir};
use fp_core::span::Span;

use crate::error::optimization_error;
use crate::transformations::IrTransform;

/// Reconstructs a lightweight HIR programme from an already-evaluated THIR snapshot.
///
/// The detyper is intentionally conservative: it recreates the structural shape of the
/// programme, synthesising types where necessary and reporting unsupported constructs
/// rather than guessing. This keeps downstream re-lowering predictable while giving
/// future work a dedicated place to expand coverage.
pub struct ThirDetyper {
    next_hir_id: hir::HirId,
}

impl ThirDetyper {
    pub fn new() -> Self {
        Self { next_hir_id: 0 }
    }

    fn fresh_hir_id(&mut self) -> hir::HirId {
        let id = self.next_hir_id;
        self.next_hir_id += 1;
        id
    }

    fn infer_type(&mut self) -> hir::TypeExpr {
        hir::TypeExpr {
            hir_id: self.fresh_hir_id(),
            kind: hir::TypeExprKind::Infer,
            span: Span::new(0, 0, 0),
        }
    }

    fn detype_type_expr(&mut self, ty: &thir::Ty) -> hir::TypeExpr {
        let kind = match &ty.kind {
            thir::ty::TyKind::Bool => hir::TypeExprKind::Primitive(TypePrimitive::Bool),
            thir::ty::TyKind::Char => hir::TypeExprKind::Primitive(TypePrimitive::Char),
            thir::ty::TyKind::Int(int_ty) => {
                hir::TypeExprKind::Primitive(TypePrimitive::Int(Self::map_int_ty(*int_ty)))
            }
            thir::ty::TyKind::Uint(uint_ty) => {
                hir::TypeExprKind::Primitive(TypePrimitive::Int(Self::map_uint_ty(*uint_ty)))
            }
            thir::ty::TyKind::Float(float_ty) => {
                hir::TypeExprKind::Primitive(TypePrimitive::Decimal(Self::map_float_ty(*float_ty)))
            }
            thir::ty::TyKind::Never => hir::TypeExprKind::Never,
            thir::ty::TyKind::Tuple(elements) => {
                let exprs = elements
                    .iter()
                    .map(|elem| Box::new(self.detype_type_expr(elem)))
                    .collect();
                hir::TypeExprKind::Tuple(exprs)
            }
            thir::ty::TyKind::Array(elem_ty, _) => {
                hir::TypeExprKind::Array(Box::new(self.detype_type_expr(elem_ty)), None)
            }
            thir::ty::TyKind::Ref(_, inner, _) => {
                hir::TypeExprKind::Ref(Box::new(self.detype_type_expr(inner)))
            }
            thir::ty::TyKind::RawPtr(type_and_mut) => {
                hir::TypeExprKind::Ptr(Box::new(self.detype_type_expr(&type_and_mut.ty)))
            }
            thir::ty::TyKind::Adt(adt_def, _) => {
                let name = adt_def
                    .variants
                    .first()
                    .map(|variant| variant.ident.clone())
                    .unwrap_or_else(|| "detyped_struct".to_string());
                hir::TypeExprKind::Path(hir::Path {
                    segments: vec![hir::PathSegment { name, args: None }],
                    res: None,
                })
            }
            _ => hir::TypeExprKind::Infer,
        };

        hir::TypeExpr {
            hir_id: self.fresh_hir_id(),
            kind,
            span: Span::new(0, 0, 0),
        }
    }

    fn map_int_ty(int_ty: thir::ty::IntTy) -> TypeInt {
        match int_ty {
            thir::ty::IntTy::I8 => TypeInt::I8,
            thir::ty::IntTy::I16 => TypeInt::I16,
            thir::ty::IntTy::I32 => TypeInt::I32,
            thir::ty::IntTy::I64 => TypeInt::I64,
            thir::ty::IntTy::I128 => TypeInt::BigInt,
            thir::ty::IntTy::Isize => TypeInt::I64,
        }
    }

    fn map_uint_ty(uint_ty: thir::ty::UintTy) -> TypeInt {
        match uint_ty {
            thir::ty::UintTy::U8 => TypeInt::U8,
            thir::ty::UintTy::U16 => TypeInt::U16,
            thir::ty::UintTy::U32 => TypeInt::U32,
            thir::ty::UintTy::U64 => TypeInt::U64,
            thir::ty::UintTy::U128 => TypeInt::BigInt,
            thir::ty::UintTy::Usize => TypeInt::U64,
        }
    }

    fn map_float_ty(float_ty: thir::ty::FloatTy) -> DecimalType {
        match float_ty {
            thir::ty::FloatTy::F32 => DecimalType::F32,
            thir::ty::FloatTy::F64 => DecimalType::F64,
        }
    }

    pub fn transform_program(&mut self, program: thir::Program) -> Result<hir::Program> {
        let mut out = hir::Program::new();
        let bodies = program.bodies;

        for item in program.items {
            let hir_item = self.detype_item(item, &bodies)?;
            out.def_map.insert(hir_item.def_id, hir_item.clone());
            out.items.push(hir_item);
        }

        out.next_hir_id = self.next_hir_id;
        Ok(out)
    }

    fn detype_item(
        &mut self,
        item: thir::Item,
        bodies: &HashMap<thir::BodyId, thir::Body>,
    ) -> Result<hir::Item> {
        let hir_id = self.fresh_hir_id();
        let def_id = item.thir_id as hir::DefId;
        let visibility = hir::Visibility::Public;

        let kind = match item.kind {
            thir::ItemKind::Function(func) => {
                hir::ItemKind::Function(self.detype_function(func, bodies)?)
            }
            thir::ItemKind::Const(constant) => {
                hir::ItemKind::Const(self.detype_const(constant, bodies)?)
            }
            thir::ItemKind::Struct(struct_item) => {
                hir::ItemKind::Struct(self.detype_struct(struct_item)?)
            }
            thir::ItemKind::Impl(impl_block) => {
                hir::ItemKind::Impl(self.detype_impl(impl_block, bodies)?)
            }
        };

        Ok(hir::Item {
            hir_id,
            def_id,
            visibility,
            kind,
            span: item.span,
        })
    }

    fn detype_function(
        &mut self,
        func: thir::Function,
        bodies: &HashMap<thir::BodyId, thir::Body>,
    ) -> Result<hir::Function> {
        let params = func
            .sig
            .inputs
            .iter()
            .enumerate()
            .map(|(idx, _)| hir::Param {
                hir_id: self.fresh_hir_id(),
                pat: self.synthetic_binding(&format!("arg{}", idx), false),
                ty: self.infer_type(),
            })
            .collect::<Vec<_>>();

        let body = match func.body_id {
            Some(body_id) => {
                let thir_body = bodies
                    .get(&body_id)
                    .ok_or_else(|| optimization_error("Missing THIR body during detyping"))?;
                Some(self.detype_body(thir_body)?)
            }
            None => None,
        };

        let sig = hir::FunctionSig {
            name: func.name,
            inputs: params.clone(),
            output: self.infer_type(),
            generics: hir::Generics::default(),
        };

        Ok(hir::Function {
            sig,
            body,
            is_const: func.is_const,
        })
    }

    fn detype_const(
        &mut self,
        constant: thir::Const,
        bodies: &HashMap<thir::BodyId, thir::Body>,
    ) -> Result<hir::Const> {
        let body = bodies
            .get(&constant.body_id)
            .ok_or_else(|| optimization_error("Missing THIR body for const during detyping"))?;
        Ok(hir::Const {
            name: format!("const_{}", constant.body_id.0),
            ty: self.infer_type(),
            body: self.detype_body(body)?,
        })
    }

    fn detype_struct(&mut self, item: thir::Struct) -> Result<hir::Struct> {
        let fields = item
            .fields
            .into_iter()
            .enumerate()
            .map(|(idx, field)| hir::StructField {
                hir_id: self.fresh_hir_id(),
                name: format!("field{}", idx),
                ty: self.infer_type(),
                vis: self.map_visibility(&field.vis),
            })
            .collect();

        Ok(hir::Struct {
            name: "detyped_struct".to_string(),
            fields,
            generics: hir::Generics::default(),
        })
    }

    fn detype_impl(
        &mut self,
        impl_block: thir::Impl,
        bodies: &HashMap<thir::BodyId, thir::Body>,
    ) -> Result<hir::Impl> {
        let items = impl_block
            .items
            .into_iter()
            .map(|item| -> Result<hir::ImplItem> {
                let hir_id = self.fresh_hir_id();
                let name = format!("impl_item_{}", item.thir_id);
                let kind = match item.kind {
                    thir::ImplItemKind::Method(func) => {
                        hir::ImplItemKind::Method(self.detype_function(func, bodies)?)
                    }
                    thir::ImplItemKind::AssocConst(constant) => {
                        hir::ImplItemKind::AssocConst(self.detype_const(constant, bodies)?)
                    }
                };
                Ok(hir::ImplItem { hir_id, name, kind })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Impl {
            generics: hir::Generics::default(),
            trait_ty: None,
            self_ty: self.infer_type(),
            items,
        })
    }

    fn detype_body(&mut self, body: &thir::Body) -> Result<hir::Body> {
        let mut ctx = BodyContext::new(body, self);

        let params = body
            .params
            .iter()
            .enumerate()
            .map(|(idx, param)| {
                let pat = match &param.pat {
                    Some(pat) => self.detype_pattern(pat, &mut ctx)?,
                    None => self.synthetic_binding(&format!("arg{}", idx), false),
                };
                Ok(hir::Param {
                    hir_id: self.fresh_hir_id(),
                    pat,
                    ty: self.infer_type(),
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let value = self.detype_expr(&body.value, &mut ctx)?;

        Ok(hir::Body {
            hir_id: self.fresh_hir_id(),
            params,
            value,
        })
    }

    fn detype_expr(&mut self, expr: &thir::Expr, ctx: &mut BodyContext) -> Result<hir::Expr> {
        let hir_id = self.fresh_hir_id();
        let span = expr.span;

        let kind = match &expr.kind {
            thir::ExprKind::Literal(lit) => hir::ExprKind::Literal(self.detype_literal(lit)?),
            thir::ExprKind::Local(local_id) | thir::ExprKind::VarRef { id: local_id } => {
                hir::ExprKind::Path(ctx.path_for_local(*local_id, self))
            }
            thir::ExprKind::Path(item_ref) => {
                hir::ExprKind::Path(self.path_from_item_ref(item_ref))
            }
            thir::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = Box::new(self.detype_expr(lhs, ctx)?);
                let rhs = Box::new(self.detype_expr(rhs, ctx)?);
                hir::ExprKind::Binary(self.map_bin_op(op), lhs, rhs)
            }
            thir::ExprKind::LogicalOp { op, lhs, rhs } => {
                let lhs = Box::new(self.detype_expr(lhs, ctx)?);
                let rhs = Box::new(self.detype_expr(rhs, ctx)?);
                let bin_op = match op {
                    thir::LogicalOp::And => hir::BinOp::And,
                    thir::LogicalOp::Or => hir::BinOp::Or,
                };
                hir::ExprKind::Binary(bin_op, lhs, rhs)
            }
            thir::ExprKind::Unary(op, inner) => {
                let inner = Box::new(self.detype_expr(inner, ctx)?);
                hir::ExprKind::Unary(self.map_un_op(op), inner)
            }
            thir::ExprKind::Call { fun, args, .. } => {
                let callee = Box::new(self.detype_expr(fun, ctx)?);
                let args = args
                    .iter()
                    .map(|arg| self.detype_expr(arg, ctx))
                    .collect::<Result<Vec<_>>>()?;
                hir::ExprKind::Call(callee, args)
            }
            thir::ExprKind::IntrinsicCall(call) => {
                use fp_core::intrinsics::IntrinsicCallPayload;

                let payload = match &call.payload {
                    IntrinsicCallPayload::Format { template } => IntrinsicCallPayload::Format {
                        template: self.detype_format_string(template, ctx)?,
                    },
                    IntrinsicCallPayload::Args { args } => IntrinsicCallPayload::Args {
                        args: args
                            .iter()
                            .map(|expr| self.detype_expr(expr, ctx))
                            .collect::<Result<Vec<_>>>()?,
                    },
                };

                hir::ExprKind::IntrinsicCall(hir::IntrinsicCallExpr {
                    kind: call.kind,
                    payload,
                })
            }
            thir::ExprKind::Block(block) => hir::ExprKind::Block(self.detype_block(block, ctx)?),
            thir::ExprKind::If {
                cond,
                then,
                else_opt,
            } => {
                let cond = Box::new(self.detype_expr(cond, ctx)?);
                let then = Box::new(self.detype_expr(then, ctx)?);
                let else_expr = else_opt
                    .as_ref()
                    .map(|expr| self.detype_expr(expr, ctx))
                    .transpose()?;
                hir::ExprKind::If(cond, then, else_expr.map(Box::new))
            }
            thir::ExprKind::Assign { lhs, rhs } => {
                let lhs_expr = Box::new(self.detype_expr(lhs, ctx)?);
                let rhs_expr = Box::new(self.detype_expr(rhs, ctx)?);
                hir::ExprKind::Assign(lhs_expr, rhs_expr)
            }
            thir::ExprKind::AssignOp { op, lhs, rhs } => {
                let lhs_expr = self.detype_expr(lhs, ctx)?;
                let rhs_expr = self.detype_expr(rhs, ctx)?;
                let binary = hir::Expr {
                    hir_id: self.fresh_hir_id(),
                    kind: hir::ExprKind::Binary(
                        self.map_bin_op(op),
                        Box::new(lhs_expr.clone()),
                        Box::new(rhs_expr.clone()),
                    ),
                    span,
                };
                hir::ExprKind::Assign(Box::new(lhs_expr), Box::new(binary))
            }
            thir::ExprKind::Return { value } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.detype_expr(expr, ctx))
                    .transpose()?;
                hir::ExprKind::Return(value.map(Box::new))
            }
            thir::ExprKind::Break { value } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.detype_expr(expr, ctx))
                    .transpose()?;
                hir::ExprKind::Break(value.map(Box::new))
            }
            thir::ExprKind::Continue => hir::ExprKind::Continue,
            thir::ExprKind::Loop { body } => {
                let inner = self.detype_expr(body, ctx)?;
                let block = match inner.kind {
                    hir::ExprKind::Block(block) => block,
                    other => hir::Block {
                        hir_id: self.fresh_hir_id(),
                        stmts: vec![hir::Stmt {
                            hir_id: self.fresh_hir_id(),
                            kind: hir::StmtKind::Expr(hir::Expr {
                                hir_id: self.fresh_hir_id(),
                                kind: other,
                                span,
                            }),
                        }],
                        expr: None,
                    },
                };
                hir::ExprKind::Loop(block)
            }
            thir::ExprKind::Let { expr: value, pat } => {
                let expr = Box::new(self.detype_expr(value, ctx)?);
                let pat = self.detype_pattern(pat, ctx)?;
                hir::ExprKind::Let(pat, Box::new(self.infer_type()), Some(expr))
            }
            thir::ExprKind::Scope { value, .. }
            | thir::ExprKind::Use(value)
            | thir::ExprKind::Borrow { arg: value, .. }
            | thir::ExprKind::AddressOf { arg: value, .. }
            | thir::ExprKind::Cast(value, _)
            | thir::ExprKind::Deref(value) => {
                return self.detype_expr(value, ctx);
            }
            thir::ExprKind::Field { base, field_idx } => {
                let base_expr = Box::new(self.detype_expr(base, ctx)?);
                let field_name = format!("field{}", field_idx);
                hir::ExprKind::FieldAccess(base_expr, field_name)
            }
            thir::ExprKind::Match { .. }
            | thir::ExprKind::Index(..)
            | thir::ExprKind::UpvarRef { .. } => {
                return Err(optimization_error(
                    "Detyping encountered unsupported THIR expression variant",
                ));
            }
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    fn detype_format_string(
        &mut self,
        format: &thir::FormatString,
        ctx: &mut BodyContext,
    ) -> Result<hir::FormatString> {
        let parts = format
            .parts
            .iter()
            .map(|part| match part {
                thir::FormatTemplatePart::Literal(text) => {
                    Ok(hir::FormatTemplatePart::Literal(text.clone()))
                }
                thir::FormatTemplatePart::Placeholder(placeholder) => {
                    let arg_ref = match &placeholder.arg_ref {
                        thir::FormatArgRef::Implicit => hir::FormatArgRef::Implicit,
                        thir::FormatArgRef::Positional(idx) => hir::FormatArgRef::Positional(*idx),
                        thir::FormatArgRef::Named(name) => hir::FormatArgRef::Named(name.clone()),
                    };

                    Ok(hir::FormatTemplatePart::Placeholder(
                        hir::FormatPlaceholder {
                            arg_ref,
                            format_spec: placeholder.format_spec.clone(),
                        },
                    ))
                }
            })
            .collect::<Result<Vec<_>>>()?;

        let args = format
            .args
            .iter()
            .map(|expr| self.detype_expr(expr, ctx))
            .collect::<Result<Vec<_>>>()?;

        let kwargs = format
            .kwargs
            .iter()
            .map(|kw| {
                Ok(hir::FormatKwArg {
                    name: kw.name.clone(),
                    value: self.detype_expr(&kw.value, ctx)?,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::FormatString {
            parts,
            args,
            kwargs,
        })
    }

    fn detype_block(&mut self, block: &thir::Block, ctx: &mut BodyContext) -> Result<hir::Block> {
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.detype_stmt(stmt, ctx))
            .collect::<Result<Vec<_>>>()?;
        let expr = block
            .expr
            .as_ref()
            .map(|expr| self.detype_expr(expr, ctx))
            .transpose()?;
        Ok(hir::Block {
            hir_id: self.fresh_hir_id(),
            stmts,
            expr: expr.map(Box::new),
        })
    }

    fn detype_stmt(&mut self, stmt: &thir::Stmt, ctx: &mut BodyContext) -> Result<hir::Stmt> {
        let hir_id = self.fresh_hir_id();
        let kind = match &stmt.kind {
            thir::StmtKind::Expr(expr) => hir::StmtKind::Expr(self.detype_expr(expr, ctx)?),
            thir::StmtKind::Let {
                pattern,
                initializer,
                ..
            } => {
                let annotated_ty = self.detype_type_expr(&pattern.ty);
                let pat = self.detype_pattern(pattern, ctx)?;
                let init = initializer
                    .as_ref()
                    .map(|expr| self.detype_expr(expr, ctx))
                    .transpose()?;
                hir::StmtKind::Local(hir::Local {
                    hir_id: self.fresh_hir_id(),
                    pat,
                    ty: Some(annotated_ty),
                    init,
                })
            }
        };
        Ok(hir::Stmt { hir_id, kind })
    }

    fn detype_pattern(&mut self, pat: &thir::Pat, ctx: &mut BodyContext) -> Result<hir::Pat> {
        let (hir_id, kind) = match &pat.kind {
            thir::PatKind::Wild => (self.fresh_hir_id(), hir::PatKind::Wild),
            thir::PatKind::Binding {
                name,
                var,
                mutability,
                ..
            } => {
                let hir_id = ctx.register_local(*var, name.clone(), self);
                let kind = hir::PatKind::Binding {
                    name: name.clone(),
                    mutable: matches!(mutability, thir::Mutability::Mut),
                };
                (hir_id, kind)
            }
            thir::PatKind::Constant { value } => {
                // THIR currently exposes constant patterns without literal payloads.
                // Fall back to a wildcard so the detyper stays permissive.
                let _ = value;
                (self.fresh_hir_id(), hir::PatKind::Wild)
            }
            _ => (self.fresh_hir_id(), hir::PatKind::Wild),
        };
        Ok(hir::Pat { hir_id, kind })
    }

    fn detype_literal(&self, lit: &thir::Lit) -> Result<hir::Lit> {
        Ok(match lit {
            thir::Lit::Bool(value) => hir::Lit::Bool(*value),
            thir::Lit::Int(value, _) => hir::Lit::Integer(*value as i64),
            thir::Lit::Uint(value, _) => hir::Lit::Integer(*value as i64),
            thir::Lit::Float(value, _) => hir::Lit::Float(*value),
            thir::Lit::Char(value) => hir::Lit::Char(*value),
            thir::Lit::Str(value) => hir::Lit::Str(value.clone()),
            _ => {
                return Err(optimization_error(
                    "Detyping encountered unsupported literal variant",
                ));
            }
        })
    }

    fn path_from_item_ref(&self, item_ref: &thir::ItemRef) -> hir::Path {
        let segments = item_ref
            .name
            .split("::")
            .map(|segment| hir::PathSegment {
                name: segment.to_string(),
                args: None,
            })
            .collect();

        hir::Path {
            segments,
            res: item_ref.def_id.map(|id| hir::Res::Def(id as hir::DefId)),
        }
    }

    fn synthetic_binding(&mut self, name: &str, mutable: bool) -> hir::Pat {
        hir::Pat {
            hir_id: self.fresh_hir_id(),
            kind: hir::PatKind::Binding {
                name: name.to_string(),
                mutable,
            },
        }
    }

    fn map_bin_op(&self, op: &thir::BinOp) -> hir::BinOp {
        match op {
            thir::BinOp::Add => hir::BinOp::Add,
            thir::BinOp::Sub => hir::BinOp::Sub,
            thir::BinOp::Mul => hir::BinOp::Mul,
            thir::BinOp::Div => hir::BinOp::Div,
            thir::BinOp::Rem => hir::BinOp::Rem,
            thir::BinOp::BitXor => hir::BinOp::BitXor,
            thir::BinOp::BitAnd => hir::BinOp::BitAnd,
            thir::BinOp::BitOr => hir::BinOp::BitOr,
            thir::BinOp::Shl => hir::BinOp::Shl,
            thir::BinOp::Shr => hir::BinOp::Shr,
            thir::BinOp::Eq => hir::BinOp::Eq,
            thir::BinOp::Lt => hir::BinOp::Lt,
            thir::BinOp::Le => hir::BinOp::Le,
            thir::BinOp::Ne => hir::BinOp::Ne,
            thir::BinOp::Ge => hir::BinOp::Ge,
            thir::BinOp::Gt => hir::BinOp::Gt,
        }
    }

    fn map_un_op(&self, op: &thir::UnOp) -> hir::UnOp {
        match op {
            thir::UnOp::Not => hir::UnOp::Not,
            thir::UnOp::Neg => hir::UnOp::Neg,
        }
    }

    fn map_visibility(&self, vis: &thir::Visibility) -> hir::Visibility {
        match vis {
            thir::Visibility::Public => hir::Visibility::Public,
            _ => hir::Visibility::Private,
        }
    }
}

impl IrTransform<thir::Program, hir::Program> for ThirDetyper {
    fn transform(&mut self, source: thir::Program) -> Result<hir::Program> {
        self.transform_program(source)
    }
}

struct LocalInfo {
    name: String,
    hir_id: hir::HirId,
}

struct BodyContext {
    locals: HashMap<thir::LocalId, LocalInfo>,
}

impl BodyContext {
    fn new(body: &thir::Body, detyper: &mut ThirDetyper) -> Self {
        let mut locals = HashMap::new();
        for (idx, decl) in body.locals.iter().enumerate() {
            let name = if decl.internal {
                format!("_tmp{}", idx)
            } else {
                format!("local{}", idx)
            };
            locals.insert(
                idx as thir::LocalId,
                LocalInfo {
                    name,
                    hir_id: detyper.fresh_hir_id(),
                },
            );
        }
        Self { locals }
    }

    fn register_local(
        &mut self,
        local_id: thir::LocalId,
        name: String,
        detyper: &mut ThirDetyper,
    ) -> hir::HirId {
        let entry = self.locals.entry(local_id).or_insert_with(|| LocalInfo {
            name: name.clone(),
            hir_id: detyper.fresh_hir_id(),
        });
        entry.name = name;
        entry.hir_id
    }

    fn ensure_local(&mut self, local_id: thir::LocalId, detyper: &mut ThirDetyper) -> &LocalInfo {
        self.locals.entry(local_id).or_insert_with(|| LocalInfo {
            name: format!("local{}", local_id),
            hir_id: detyper.fresh_hir_id(),
        })
    }

    fn path_for_local(&mut self, local_id: thir::LocalId, detyper: &mut ThirDetyper) -> hir::Path {
        let info = self.ensure_local(local_id, detyper);
        hir::Path {
            segments: vec![hir::PathSegment {
                name: info.name.clone(),
                args: None,
            }],
            res: Some(hir::Res::Local(info.hir_id)),
        }
    }
}
