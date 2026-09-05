use super::expr::ConstContainerArgs;
use super::*;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::intrinsics::IntrinsicKind;
use fp_core::mir;
use fp_core::mir::ty::{ConstKind, ConstValue, IntTy, Ty, TyKind, UintTy};
use fp_core::span::Span;
use std::collections::HashMap;

impl HirToMirLowerer {
    pub(crate) fn lower_const_expr(
        &mut self,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
        container_args: Option<&ConstContainerArgs>,
    ) -> Option<mir::Constant> {
        let constant_ty = expected_ty
            .cloned()
            .or_else(|| self.typeck_expr_type(expr.hir_id.clone()));
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(mir::Constant {
                span: expr.span,
                ty: constant_ty.clone()?,
                user_ty: None,
                literal: self.lower_literal(lit),
            }),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_expr(inner, expected_ty, container_args);
                }
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Unit),
                })
            }
            hir::ExprKind::Array(elements) => {
                if let Some(container_args) = container_args {
                    return self.lower_container_const(expr.span, elements, container_args);
                }
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(element, Some(elem_ty.as_ref()))?);
                }
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered)),
                })
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                if let Some(container_args) = container_args {
                    return self.lower_container_repeat_const(expr.span, elem, len, container_args);
                }
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                let ty = constant_ty.clone()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered)),
                })
            }
            hir::ExprKind::Struct(_, _) => {
                let value = self.lower_const_value(expr, expected_ty)?;
                let ty = match constant_ty.clone()? {
                    Ty {
                        kind: TyKind::Adt(adt, args),
                    } => {
                        let type_args = args
                            .iter()
                            .filter_map(|arg| match arg {
                                mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                                mir::ty::GenericArg::Lifetime(_)
                                | mir::ty::GenericArg::Const(_) => None,
                            })
                            .collect::<Vec<_>>();
                        self.struct_layout_for_instance(adt.did.clone(), &type_args, expr.span)
                            .map(|layout| layout.ty)
                            .unwrap_or(Ty {
                                kind: TyKind::Adt(adt, args),
                            })
                    }
                    ty => ty,
                };
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(value),
                })
            }
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res_ref().as_ref()? else {
                    return None;
                };
                if let Some(const_info) = self.ensure_const_info(def_id.clone()) {
                    return Some(const_info.typed_value());
                }
                if let Some(hir::Item {
                    kind: hir::ItemKind::Const(constant),
                    ..
                }) = self.hir_item(def_id.clone())
                {
                    let ty = self.lower_type_expr(&constant.ty);
                    return self.lower_const_expr(&constant.body.value, Some(&ty), container_args);
                }
                let item = self.hir_item(def_id.clone())?;
                let hir::ItemKind::Function(_function) = &item.kind else {
                    return None;
                };
                let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) = expected_ty.map(|ty| &ty.kind)?
                else {
                    return None;
                };
                let fn_ty = expected_ty.cloned()?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: fn_ty,
                    user_ty: None,
                    literal: mir::ConstantKind::FnDef(def_id.clone(), Vec::new()),
                })
            }
            hir::ExprKind::Slice(slice) => {
                let value = self.lower_const_string_slice(slice)?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: constant_ty.clone()?,
                    user_ty: None,
                    literal: mir::ConstantKind::Str(value),
                })
            }
            hir::ExprKind::Index(base, index) => self
                .lower_const_expr(base, None, container_args)
                .and_then(|constant| self.const_index_value(expr.span, &constant, index))
                .map(|(constant, _)| constant),
            hir::ExprKind::FieldAccess(base, field) => {
                self.lower_const_field_access(base, field.as_str(), expr.span, constant_ty.as_ref())
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let branch = match self.lower_const_value(cond, None)? {
                    mir::ConstValue::Bool(value) => {
                        if value {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::Int(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::UInt(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    _ => return None,
                };
                self.lower_const_expr(branch, expected_ty, container_args)
            }
            hir::ExprKind::MethodCall(receiver, method_name, _, args) => {
                let ty = constant_ty.clone()?;
                let value =
                    self.lower_const_method_value(receiver, method_name.as_str(), args, expr.span)?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: self.const_value_to_constant(expr.span, &value, &ty).literal,
                })
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let kind = if let (Some(left), Some(right)) = (
                    self.lower_const_expr(lhs, expected_ty, container_args),
                    self.lower_const_expr(rhs, expected_ty, container_args),
                ) {
                    lower_binary_op_const(op, &left, &right)
                } else {
                    let left = self.lower_const_value(lhs, expected_ty)?;
                    let right = self.lower_const_value(rhs, expected_ty)?;
                    lower_binary_op_const_values(op, &left, &right)
                }?;
                Some(mir::Constant {
                    span: expr.span,
                    ty: constant_ty.clone()?,
                    user_ty: None,
                    literal: kind,
                })
            }
            _ => None,
        }
    }
}

pub(super) fn lower_binary_op_const(
    op: &hir::BinOp,
    left: &mir::Constant,
    right: &mir::Constant,
) -> Option<mir::ConstantKind> {
    match (&left.literal, &right.literal) {
        (mir::ConstantKind::Int(l), mir::ConstantKind::Int(r)) => match op {
            hir::BinOp::Add => Some(mir::ConstantKind::Int(l + r)),
            hir::BinOp::Sub => Some(mir::ConstantKind::Int(l - r)),
            hir::BinOp::Mul => Some(mir::ConstantKind::Int(l * r)),
            hir::BinOp::Div => Some(mir::ConstantKind::Int(l / r)),
            hir::BinOp::Gt => Some(mir::ConstantKind::Bool(l > r)),
            hir::BinOp::Lt => Some(mir::ConstantKind::Bool(l < r)),
            hir::BinOp::Ge => Some(mir::ConstantKind::Bool(l >= r)),
            hir::BinOp::Le => Some(mir::ConstantKind::Bool(l <= r)),
            hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
            hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
            _ => None,
        },
        (mir::ConstantKind::UInt(l), mir::ConstantKind::UInt(r)) => match op {
            hir::BinOp::Add => Some(mir::ConstantKind::UInt(l + r)),
            hir::BinOp::Sub => Some(mir::ConstantKind::UInt(l - r)),
            hir::BinOp::Mul => Some(mir::ConstantKind::UInt(l * r)),
            hir::BinOp::Div => Some(mir::ConstantKind::UInt(l / r)),
            hir::BinOp::Gt => Some(mir::ConstantKind::Bool(l > r)),
            hir::BinOp::Lt => Some(mir::ConstantKind::Bool(l < r)),
            _ => None,
        },
        (mir::ConstantKind::Str(l), mir::ConstantKind::Str(r)) => match op {
            hir::BinOp::Add => Some(mir::ConstantKind::Str(format!("{l}{r}"))),
            hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
            hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn lower_binary_op_const_values(
    op: &hir::BinOp,
    left: &mir::ConstValue,
    right: &mir::ConstValue,
) -> Option<mir::ConstantKind> {
    match (left, right) {
        (mir::ConstValue::Str(l), mir::ConstValue::Str(r)) => match op {
            hir::BinOp::Add => Some(mir::ConstantKind::Str(format!("{l}{r}"))),
            hir::BinOp::Eq => Some(mir::ConstantKind::Bool(l == r)),
            hir::BinOp::Ne => Some(mir::ConstantKind::Bool(l != r)),
            _ => None,
        },
        _ => None,
    }
}

impl HirToMirLowerer {
    pub(super) fn lower_const_value(
        &mut self,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::ConstValue> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(self.const_value_from_lit(lit)),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_value(inner, expected_ty);
                }
                Some(mir::ConstValue::Unit)
            }
            hir::ExprKind::Array(elements) => {
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(element, Some(elem_ty.as_ref()))?);
                }
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::Struct(path, fields) => {
                let def_id = self.resolve_path_def_id(path.path()?)?;
                let struct_def = self
                    .mir_package
                    .borrow()
                    .struct_defs
                    .get(&def_id)
                    .cloned()?
                    .clone();
                let mut args = path
                    .path()
                    .and_then(|path| {
                        path.segments
                            .iter()
                            .find_map(|segment| segment.args.as_ref())
                    })
                    .map(|args| self.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                if args.is_empty() && !struct_def.generics.is_empty() {
                    // No explicit turbofish — read `fp-typing`'s own
                    // already-resolved generic args for this literal
                    // (`typeck_expr_type`) rather than re-deriving them here.
                    // Top-level `const` items aren't themselves generic, so
                    // there's no live specialization context to compose in
                    // (empty substs map); if the cache has no entry, fall
                    // through to today's behavior unchanged, letting
                    // `struct_layout_for_instance`'s own arity check
                    // surface the real diagnostic.
                    if let Some(cached) = self.adt_ty_args_from_typeck_cache(
                        expr.hir_id.clone(),
                        def_id.clone(),
                        &HashMap::new(),
                    ) {
                        args = cached;
                    }
                }
                let layout = self.struct_layout_for_instance(def_id, &args, expr.span);
                let layout = match layout {
                    Some(l) => l,
                    None => return None,
                };
                let mut field_map: HashMap<String, &hir::Expr> = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.as_str().to_string(), &field.expr);
                }
                let mut lowered = Vec::with_capacity(struct_def.fields.len());
                for (idx, field_def) in struct_def.fields.iter().enumerate() {
                    let Some(field_expr) = field_map.get(&field_def.name) else {
                        self.emit_error(
                            expr.span,
                            format!("missing field `{}` in const struct literal", field_def.name),
                        );
                        return None;
                    };
                    let field_ty = layout.field_tys.get(idx)?;
                    lowered.push(self.lower_const_value(field_expr, Some(field_ty))?);
                }
                Some(mir::ConstValue::Struct(lowered))
            }
            hir::ExprKind::Slice(slice) => {
                Some(mir::ConstValue::Str(self.lower_const_string_slice(slice)?))
            }
            hir::ExprKind::Index(base, index) => self
                .lower_const_expr(base, None, None)
                .and_then(|constant| self.const_index_value(expr.span, &constant, index))
                .and_then(|(constant, _)| self.const_value_from_constant(&constant)),
            hir::ExprKind::FieldAccess(base, field) => self
                .lower_const_field_access(base, field.as_str(), expr.span, None)
                .and_then(|constant| self.const_value_from_constant(&constant)),
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let branch = match self.lower_const_value(cond, None)? {
                    mir::ConstValue::Bool(value) => {
                        if value {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::Int(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    mir::ConstValue::UInt(value) => {
                        if value != 0 {
                            then_expr.as_ref()
                        } else {
                            else_expr.as_deref()?
                        }
                    }
                    _ => return None,
                };
                self.lower_const_value(branch, expected_ty)
            }
            hir::ExprKind::MethodCall(receiver, method_name, _, args) => {
                self.lower_const_method_value(receiver, method_name.as_str(), args, expr.span)
            }
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res_ref().as_ref()? else {
                    return None;
                };

                // Check const_values first — function-local consts are
                // registered here by lower_const but may not be in
                // program.def_map.
                if let Some(const_info) = self.ensure_const_info(def_id.clone()) {
                    return match &const_info.value.literal {
                        mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
                        mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
                        mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
                        mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
                        mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
                        mir::ConstantKind::Val(v) => Some(v.clone()),
                        _ => None,
                    };
                }

                if let Some(hir::Item {
                    kind: hir::ItemKind::Const(constant),
                    ..
                }) = self.hir_item(def_id.clone())
                {
                    let ty = self.lower_type_expr(&constant.ty);
                    return self.lower_const_value(&constant.body.value, Some(&ty));
                }

                let item = self.hir_item(def_id.clone())?;
                match &item.kind {
                    hir::ItemKind::Function(_function) => {
                        let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) =
                            expected_ty.map(|ty| &ty.kind)?
                        else {
                            return None;
                        };
                        Some(mir::ConstValue::FnDef(def_id.clone(), Vec::new()))
                    }
                    hir::ItemKind::Const(_) => {
                        let const_info = self.ensure_const_info(def_id.clone())?;
                        match &const_info.value.literal {
                            mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
                            mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
                            mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
                            mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
                            mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
                            mir::ConstantKind::Val(v) => Some(v.clone()),
                            _ => None,
                        }
                    }
                    _ => return None,
                }
            }
            _ => None,
        }
    }

    pub(super) fn lower_const_string_slice(&mut self, slice: &hir::SliceExpr) -> Option<String> {
        let base = self.const_string_from_expr(slice.base.as_ref())?;
        let start = match slice.start.as_ref() {
            Some(start) => self.const_index_u64(start.as_ref())? as usize,
            None => 0,
        };
        let mut end = match slice.end.as_ref() {
            Some(end) => self.const_index_u64(end.as_ref())? as usize,
            None => base.len(),
        };
        if slice.inclusive {
            end = end.checked_add(1)?;
        }
        if start > end || end > base.len() {
            return None;
        }
        base.get(start..end).map(str::to_string)
    }

    pub(super) fn lower_const_method_value(
        &mut self,
        receiver: &hir::Expr,
        method_name: &str,
        args: &[hir::CallArg],
        _span: Span,
    ) -> Option<mir::ConstValue> {
        let matches_name =
            |name: &str| method_name == name || method_name.ends_with(&format!("::{name}"));
        let receiver_value = self.lower_const_value(receiver, None)?;

        if matches_name("len") && args.is_empty() {
            return match &receiver_value {
                mir::ConstValue::Str(text) => Some(mir::ConstValue::UInt(text.len() as u64)),
                mir::ConstValue::Array(elements) => {
                    Some(mir::ConstValue::UInt(elements.len() as u64))
                }
                mir::ConstValue::Tuple(fields) => Some(mir::ConstValue::UInt(fields.len() as u64)),
                _ => None,
            };
        }

        let receiver_text = match &receiver_value {
            mir::ConstValue::Str(text) => Some(text.clone()),
            _ => None,
        };
        let needle = match args.first() {
            Some(arg) => self.const_string_from_expr(&arg.value)?,
            None => return None,
        };
        if matches_name("starts_with") && args.len() == 1 {
            let receiver_text = receiver_text?;
            return Some(mir::ConstValue::Bool(receiver_text.starts_with(&needle)));
        }
        if matches_name("ends_with") && args.len() == 1 {
            let receiver_text = receiver_text?;
            return Some(mir::ConstValue::Bool(receiver_text.ends_with(&needle)));
        }
        if matches_name("contains") && args.len() == 1 {
            if let Some(receiver_text) = receiver_text {
                return Some(mir::ConstValue::Bool(receiver_text.contains(&needle)));
            }
            if let Some(items) = Self::const_string_items(&receiver_value) {
                return Some(mir::ConstValue::Bool(
                    items.iter().any(|item| item == &needle),
                ));
            }
        }
        None
    }

    pub(super) fn lower_const_field_access(
        &mut self,
        base: &hir::Expr,
        field: &str,
        span: Span,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::Constant> {
        if let Some(constant) = self.lower_const_expr(base, None, None) {
            if let Some(field_value) =
                self.lower_const_struct_field_from_constant(&constant, field, span)
            {
                return Some(field_value);
            }
        }

        // `type(T).field_type("field").name` is reflection, not a runtime
        // method call. Resolve the nominal type and its declared field here
        // so the `TypeOf` marker never reaches MIR operand lowering.
        if field == "name" {
            if let hir::ExprKind::MethodCall(receiver, method, _, args) = &base.kind {
                if method.as_str() == "field_type" && args.len() == 1 {
                    if let hir::ExprKind::IntrinsicCall(call) = &receiver.kind {
                        if call.kind == IntrinsicKind::TypeOf && call.callargs.len() == 1 {
                            let hir::ExprKind::Path(type_path) = &call.callargs[0].value.kind
                            else {
                                return None;
                            };
                            let hir::Res::Def(def_id) = type_path.res_ref().as_ref()? else {
                                return None;
                            };
                            let field_name = self.const_string_from_expr(&args[0].value)?;
                            let struct_def_id = def_id.clone();
                            self.try_lazily_register_adt(struct_def_id.clone(), span);
                            let info = self
                                .mir_package
                                .borrow()
                                .struct_defs
                                .get(&struct_def_id)?
                                .clone();
                            let index = info.field_index.get(&field_name).copied()?;
                            let field_ty = self.lower_type_expr(&info.fields.get(index)?.ty);
                            let name =
                                self.display_type_name(&field_ty).or_else(|| {
                                    match field_ty.kind {
                                        TyKind::Bool => Some("bool".to_string()),
                                        TyKind::Char => Some("char".to_string()),
                                        TyKind::Int(_) => Some("i64".to_string()),
                                        TyKind::Uint(_) => Some("u64".to_string()),
                                        TyKind::Float(_) => Some("f64".to_string()),
                                        TyKind::Slice(_) => Some("str".to_string()),
                                        _ => None,
                                    }
                                })?;
                            return Some(mir::Constant {
                                span,
                                ty: self.raw_string_ptr_ty(),
                                user_ty: None,
                                literal: mir::ConstantKind::Str(name),
                            });
                        }
                    }
                }
            }
        }

        let hir::ExprKind::IntrinsicCall(call) = &base.kind else {
            return None;
        };
        if call.kind != IntrinsicKind::TypeOf || call.callargs.len() != 1 {
            return None;
        }
        let type_arg = &call.callargs[0].value;

        let hir::ExprKind::Path(path) = &type_arg.kind else {
            return None;
        };
        let hir::Res::Def(def_id) = path.res_ref().as_ref()? else {
            return None;
        };
        let reflected_name = path
            .segments()
            .last()
            .map(|segment| format!("struct {}", segment.ident));
        let struct_def_id = def_id.clone();
        self.try_lazily_register_adt(struct_def_id.clone(), span);
        let struct_info = self
            .mir_package
            .borrow()
            .struct_defs
            .get(&struct_def_id)
            .cloned()?;
        match field {
            "fields" => {
                let names = struct_info
                    .fields
                    .iter()
                    .map(|field| field.name.clone())
                    .collect::<Vec<_>>();
                self.reflection_fields_constant(span, &struct_info, names, expected_ty)
            }
            "methods" => {
                let method_names = self
                    .mir_package
                    .borrow()
                    .struct_methods
                    .get(&struct_info.name)
                    .map(|methods| methods.keys().cloned().collect::<Vec<_>>())
                    .unwrap_or_default();
                self.reflection_string_list_constant(span, method_names, expected_ty)
            }
            "size" => {
                let layout = self.struct_layout_for_instance(struct_def_id, &[], span)?;
                let size = self.size_of_ty(&layout.ty, span)?;
                Some(mir::Constant {
                    span,
                    ty: Ty {
                        kind: TyKind::Uint(fp_core::mir::ty::UintTy::Usize),
                    },
                    user_ty: None,
                    literal: mir::ConstantKind::UInt(size),
                })
            }
            "name" => Some(mir::Constant {
                span,
                ty: self.raw_string_ptr_ty(),
                user_ty: None,
                literal: mir::ConstantKind::Str(reflected_name.unwrap_or(struct_info.name)),
            }),
            _ => None,
        }
    }

    pub(super) fn lower_const_struct_field_from_constant(
        &mut self,
        constant: &mir::Constant,
        field: &str,
        span: Span,
    ) -> Option<mir::Constant> {
        let (values, ty) = match &constant.literal {
            mir::ConstantKind::Val(mir::ConstValue::Struct(values)) => (values, &constant.ty),
            _ => return None,
        };

        match &ty.kind {
            // `adt_def.variants` is deliberately empty for several real
            // construction paths (`adt_shell_ty`, the general Adt case in
            // `lower_hir_ty`) — those only ever needed to convey type
            // *identity*, not full field layout. `struct_field` is the
            // authoritative, substitution-aware lookup (via `struct_defs`/
            // `struct_layout_for_ty`/`struct_layout_for_instance`) already
            // used elsewhere in this file for exactly this; never derive
            // field info from `adt_def.variants` directly.
            TyKind::Adt(adt_def, _) => {
                let (field_index, field_info) =
                    self.struct_field(adt_def.did.clone(), ty, field, span)?;
                let field_value = values.get(field_index)?;
                Some(self.const_value_to_constant(span, field_value, &field_info.ty))
            }
            TyKind::Tuple(field_tys) => {
                if let Some(key) = self
                    .mir_package
                    .borrow()
                    .struct_layouts_by_ty
                    .get(ty)
                    .cloned()
                {
                    let field_index = self
                        .mir_package
                        .borrow()
                        .struct_defs
                        .get(&key.def_id)?
                        .field_index
                        .get(field)
                        .copied()?;
                    let layout = self
                        .mir_package
                        .borrow()
                        .struct_layouts
                        .get(&key)
                        .cloned()?;
                    let field_ty = layout.field_tys.get(field_index)?;
                    let field_value = values.get(field_index)?;
                    return Some(self.const_value_to_constant(span, field_value, field_ty));
                }
                let field_index = field.parse::<usize>().ok()?;
                let field_ty = field_tys.get(field_index)?.as_ref();
                let field_value = values.get(field_index)?;
                Some(self.const_value_to_constant(span, field_value, field_ty))
            }
            _ => None,
        }
    }

    pub(super) fn string_list_constant(&self, span: Span, items: Vec<String>) -> mir::Constant {
        let elem_ty = self.string_slice_ty();
        let ty = Ty {
            kind: TyKind::Slice(Box::new(elem_ty.clone())),
        };
        let elements = items.into_iter().map(mir::ConstValue::Str).collect();
        mir::Constant {
            span,
            ty: ty.clone(),
            user_ty: None,
            literal: mir::ConstantKind::Val(mir::ConstValue::Array(elements)),
        }
    }

    fn list_constant_types(&mut self, target_ty: &Ty) -> Option<(Ty, Ty)> {
        match &target_ty.kind {
            TyKind::Slice(elem_ty) => Some((elem_ty.as_ref().clone(), target_ty.clone())),
            TyKind::Array(elem_ty, _) => Some((elem_ty.as_ref().clone(), target_ty.clone())),
            TyKind::Adt(_, substs) => substs.iter().find_map(|arg| match arg {
                mir::ty::GenericArg::Type(elem_ty) => Some((elem_ty.clone(), target_ty.clone())),
                _ => None,
            }),
            TyKind::Tuple(_) => {
                let key = self
                    .mir_package
                    .borrow()
                    .struct_layouts_by_ty
                    .get(target_ty)
                    .cloned()?;
                key.args
                    .first()
                    .cloned()
                    .map(|elem_ty| (elem_ty, target_ty.clone()))
            }
            _ => None,
        }
    }

    fn reflection_string_list_constant(
        &mut self,
        span: Span,
        items: Vec<String>,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::Constant> {
        let Some(target_ty) = expected_ty else {
            return Some(self.string_list_constant(span, items));
        };
        let Some((_elem_ty, target_ty)) = self.list_constant_types(target_ty) else {
            self.emit_error(span, "reflection list has no concrete collection type");
            return None;
        };
        let elements = items.into_iter().map(mir::ConstValue::Str).collect();
        let literal = mir::ConstValue::Array(elements);
        Some(mir::Constant {
            span,
            ty: target_ty,
            user_ty: None,
            literal: mir::ConstantKind::Val(literal),
        })
    }

    fn reflection_fields_constant(
        &mut self,
        span: Span,
        struct_info: &mir::StructDefinition,
        names: Vec<String>,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::Constant> {
        let Some(target_ty) = expected_ty else {
            return Some(self.string_list_constant(span, names));
        };
        let Some((field_ty, target_ty)) = self.list_constant_types(target_ty) else {
            self.emit_error(
                span,
                "reflection field list has no concrete collection type",
            );
            return None;
        };
        let field_ty = self.nominalize_struct_ty(field_ty);
        match &field_ty.kind {
            TyKind::Adt(_, _) => {}
            _ => {
                self.emit_error(span, "reflection field list has no StructField type");
                return None;
            }
        }
        let mut elements = Vec::with_capacity(struct_info.fields.len());
        for (name, field) in names.into_iter().zip(&struct_info.fields) {
            let field_value_ty = self.lower_type_expr(&field.ty);
            let type_name =
                self.display_type_name(&field_value_ty)
                    .or_else(|| match field_value_ty.kind {
                        TyKind::Bool => Some("bool".to_string()),
                        TyKind::Char => Some("char".to_string()),
                        TyKind::Int(_) => Some("i64".to_string()),
                        TyKind::Uint(_) => Some("u64".to_string()),
                        TyKind::Float(_) => Some("f64".to_string()),
                        TyKind::Slice(_) => Some("str".to_string()),
                        _ => None,
                    })?;
            elements.push(mir::ConstValue::Struct(vec![
                mir::ConstValue::Str(name),
                mir::ConstValue::Struct(vec![mir::ConstValue::Str(type_name)]),
            ]));
        }
        let literal = mir::ConstValue::Array(elements);
        Some(mir::Constant {
            span,
            ty: target_ty,
            user_ty: None,
            literal: mir::ConstantKind::Val(literal),
        })
    }

    pub(super) fn const_value_from_constant(
        &self,
        constant: &mir::Constant,
    ) -> Option<mir::ConstValue> {
        match &constant.literal {
            mir::ConstantKind::Int(v) => Some(mir::ConstValue::Int(*v)),
            mir::ConstantKind::UInt(v) => Some(mir::ConstValue::UInt(*v)),
            mir::ConstantKind::Bool(v) => Some(mir::ConstValue::Bool(*v)),
            mir::ConstantKind::Float(v) => Some(mir::ConstValue::Float(*v)),
            mir::ConstantKind::Str(v) => Some(mir::ConstValue::Str(v.clone())),
            mir::ConstantKind::Val(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub(super) fn const_string_items(value: &mir::ConstValue) -> Option<Vec<String>> {
        let items = match value {
            mir::ConstValue::Array(elements) => elements,
            mir::ConstValue::Tuple(fields) => fields,
            _ => return None,
        };
        let mut names = Vec::with_capacity(items.len());
        for item in items {
            let mir::ConstValue::Str(name) = item else {
                return None;
            };
            names.push(name.clone());
        }
        Some(names)
    }

    pub(super) fn const_string_from_expr(&mut self, expr: &hir::Expr) -> Option<String> {
        match self.lower_const_value(expr, None)? {
            mir::ConstValue::Str(value) => Some(value),
            _ => None,
        }
    }

    pub(super) fn const_index_u64(&mut self, expr: &hir::Expr) -> Option<u64> {
        match self.lower_const_value(expr, None)? {
            mir::ConstValue::UInt(value) => Some(value),
            mir::ConstValue::Int(value) if value >= 0 => Some(value as u64),
            _ => None,
        }
    }

    pub(super) fn const_value_from_lit(&self, lit: &hir::Lit) -> mir::ConstValue {
        match lit {
            hir::Lit::Bool(value) => mir::ConstValue::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstValue::Int(*value),
            hir::Lit::Float(value) => mir::ConstValue::Float(*value),
            hir::Lit::Str(value) => mir::ConstValue::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstValue::Int(*value as i64),
            hir::Lit::Null => mir::ConstValue::Null,
            // MIR constants have no raw-byte-buffer representation yet
            // (only UTF-8 `Str`) — every current use of `b"..."`/`c"..."`
            // in this codebase is plain ASCII, so this is lossy only for
            // non-UTF-8 byte content, which nothing currently needs.
            hir::Lit::Bytes(bytes) | hir::Lit::CStr(bytes) => {
                mir::ConstValue::Str(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    pub(super) fn lower_container_const(
        &mut self,
        span: Span,
        elements: &[hir::Expr],
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(element, Some(elem_ty))?);
                }
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered)),
                })
            }
            ConstContainerArgs::Map { key_ty, value_ty } => {
                let mut entries = Vec::with_capacity(elements.len());
                for element in elements {
                    let (key_expr, value_expr) = match &element.kind {
                        hir::ExprKind::Array(pair) if pair.len() == 2 => (&pair[0], &pair[1]),
                        _ => {
                            self.emit_error(
                                span,
                                "HashMap literal expects entries as [key, value]",
                            );
                            return None;
                        }
                    };
                    let key = self.lower_const_value(key_expr, Some(key_ty))?;
                    let value = self.lower_const_value(value_expr, Some(value_ty))?;
                    entries.push((key, value));
                }
                let entry_ty = Ty {
                    kind: TyKind::Tuple(vec![Box::new(key_ty.clone()), Box::new(value_ty.clone())]),
                };
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(entry_ty)),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Map {
                        entries,
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                    }),
                })
            }
        }
    }

    pub(super) fn lower_container_repeat_const(
        &mut self,
        span: Span,
        elem: &hir::Expr,
        len: &hir::Expr,
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let repeat_len = self.eval_type_length(len)?;
                let value = self.lower_const_value(elem, Some(elem_ty))?;
                let mut elements = Vec::with_capacity(repeat_len as usize);
                elements.resize(repeat_len as usize, value);
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    ty: ty.clone(),
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(elements)),
                })
            }
            ConstContainerArgs::Map { .. } => None,
        }
    }

    pub(super) fn container_args_from_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
    ) -> Option<ConstContainerArgs> {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => {
                let tail = path.segments().last()?;
                let args = tail.args.as_ref()?;
                match tail.ident.as_str() {
                    "Vec" if args.args.len() == 1 => {
                        let hir::GenericArg::Type(elem) = &args.args[0] else {
                            return None;
                        };
                        let elem_ty = self.lower_type_expr(elem.as_ref());
                        Some(ConstContainerArgs::List { elem_ty })
                    }
                    "HashMap" if args.args.len() == 2 => {
                        let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                            (&args.args[0], &args.args[1])
                        else {
                            return None;
                        };
                        let key_ty = self.lower_type_expr(key.as_ref());
                        let value_ty = self.lower_type_expr(value.as_ref());
                        Some(ConstContainerArgs::Map { key_ty, value_ty })
                    }
                    _ => None,
                }
            }
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr(elem.as_ref());
                Some(ConstContainerArgs::List { elem_ty })
            }
            hir::TypeExprKind::Structural(structural) => {
                let mut entries_ty: Option<&hir::TypeExpr> = None;
                for field in &structural.fields {
                    if field.name.as_str() == "entries" {
                        entries_ty = Some(field.ty.as_ref());
                        break;
                    }
                }
                let Some(entries_ty) = entries_ty else {
                    return None;
                };
                let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
                match &entries_ty.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments().last()?;
                        if tail.ident.as_str() == "Vec" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 1 {
                                if let hir::GenericArg::Type(inner) = &args.args[0] {
                                    entry_ty_expr = Some(inner.as_ref());
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Slice(inner) => {
                        entry_ty_expr = Some(inner.as_ref());
                    }
                    _ => {}
                }

                let Some(mut entry_ty_expr) = entry_ty_expr else {
                    return None;
                };
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    let tail = path.segments().last()?;
                    if tail.ident.as_str() == "Expr" {
                        let args = tail.args.as_ref()?;
                        if args.args.len() == 1 {
                            if let hir::GenericArg::Type(inner) = &args.args[0] {
                                entry_ty_expr = inner.as_ref();
                            }
                        }
                    }
                }

                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments().last()?;
                        if tail.ident.as_str() == "HashMapEntry" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 2 {
                                if let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                                    (&args.args[0], &args.args[1])
                                {
                                    let key_ty = self.lower_type_expr(key.as_ref());
                                    let value_ty = self.lower_type_expr(value.as_ref());
                                    return Some(ConstContainerArgs::Map { key_ty, value_ty });
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            let key_ty = self.lower_type_expr(fields[0].as_ref());
                            let value_ty = self.lower_type_expr(fields[1].as_ref());
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        let mut key_ty_expr = None;
                        let mut value_ty_expr = None;
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                        if let (Some(key_ty_expr), Some(value_ty_expr)) =
                            (key_ty_expr, value_ty_expr)
                        {
                            let key_ty = self.lower_type_expr(key_ty_expr);
                            let value_ty = self.lower_type_expr(value_ty_expr);
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    _ => {}
                }

                None
            }
            hir::TypeExprKind::Ref(inner) => self.container_args_from_type_expr(inner.as_ref()),
            _ => None,
        }
    }
}
