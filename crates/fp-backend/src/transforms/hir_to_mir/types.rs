use super::*;
use fp_core::ast::{DecimalType, TypeBinaryOpKind, TypeInt, TypePrimitive};
use fp_core::error::Result;
use fp_core::hir;
use fp_core::mir::ty::{
    ConstKind, ConstValue, FloatTy, IntTy, Mutability, Scalar, ScalarInt, Ty, TyKind, TypeAndMut,
    UintTy,
};
use fp_core::mir::{
    self, ConstInfo, EnumDefinition, EnumVariantDef, EnumVariantInfo, MethodContext,
    StructDefinition, StructFieldDef, StructuralLayoutKey,
};
use fp_core::span::Span;
use std::collections::HashMap;

impl HirToMirLowerer {
    pub(super) fn lower_type_expr_with_context_and_substs(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    // `Self::AssocName` (e.g. `Index::index`'s `-> Self::
                    // Output`) is a *projection* through this impl's own
                    // `type AssocName = ...;` binding, not `Self` itself —
                    // resolve it via that binding (substituted with this
                    // specialization's own `substs`, e.g. `T` -> `BenchCase`)
                    // before falling back to treating a bare `Self` as the
                    // whole receiver type.
                    if path.segments.len() > 1 {
                        if let Some(assoc_name) = path.segments.get(1) {
                            if let Some(assoc_ty) = ctx.assoc_types.get(assoc_name.name.as_str()) {
                                return self.lower_type_expr_with_substs(assoc_ty, substs);
                            }
                        }
                    } else {
                        return ctx.mir_self_ty.clone();
                    }
                }
            }
        }
        if substs.is_empty() {
            return self.lower_type_expr_with_context(ty_expr, method_context);
        }
        self.lower_type_expr_with_substs(ty_expr, substs)
    }

    pub(super) fn lower_type_expr_with_context(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    if path.segments.len() > 1 {
                        if let Some(assoc_name) = path.segments.get(1) {
                            if let Some(assoc_ty) =
                                ctx.assoc_types.get(assoc_name.name.as_str()).cloned()
                            {
                                return self
                                    .lower_type_expr_with_context(&assoc_ty, method_context);
                            }
                        }
                        return self.error_ty();
                    }
                    return ctx.mir_self_ty.clone();
                }
            }
        }

        match &ty_expr.kind {
            hir::TypeExprKind::Ref(inner) => {
                if self.is_string_slice_ref(inner) {
                    return self.string_slice_ty();
                }
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Ptr { inner: inner, .. } => {
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(inner_ty),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            _ => self.lower_type_expr(ty_expr),
        }
    }

    pub(super) fn lower_body(
        &mut self,
        item: &hir::Item,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        method_context: Option<MethodContext>,
    ) -> Result<mir::Body> {
        let span = function
            .body
            .as_ref()
            .map(|body| body.span())
            .unwrap_or(item.span);

        BodyBuilder::new(self, function, sig, span, method_context, HashMap::new()).lower()
    }

    pub(super) fn lower_const(
        &mut self,
        def_id: hir::DefId,
        konst: &hir::Const,
    ) -> Result<mir::Item> {
        let declared_ty = self.lower_type_expr(&konst.ty);
        let ty = match declared_ty.clone() {
            Ty {
                kind: TyKind::Adt(adt, args),
            } => {
                let type_args = args
                    .iter()
                    .filter_map(|arg| match arg {
                        mir::ty::GenericArg::Type(ty) => Some(ty.clone()),
                        mir::ty::GenericArg::Lifetime(_) | mir::ty::GenericArg::Const(_) => None,
                    })
                    .collect::<Vec<_>>();
                self.struct_layout_for_instance(adt.did, &type_args, konst.ty.span)
                    .map(|layout| layout.ty)
                    .unwrap_or(declared_ty)
            }
            ty => ty,
        };
        let container_args = self.container_args_from_type_expr(&konst.ty);
        let folded = self
            .lower_const_expr(&konst.body.value, Some(&ty), container_args.as_ref())
            .or_else(|| {
                // On a relower pass (after `CompilerDriver::evaluate_
                // comptime_lir` has run this const's own `ExecutableConst`
                // comptime entry through the real interpreter and
                // recorded its answer via `record_const_block_value`),
                // this is now foldable after all — without this check,
                // `lower_const` would keep producing the same
                // `ExecutableConst` placeholder forever, and this const
                // would never actually become a real global.
                let value = self
                    .hir_program
                    .const_value(def_id.clone())
                    .or_else(|| self.hir_program.const_block_value(def_id.clone()))?;
                self.typed_const_value_to_mir_constant(&value, &ty, konst.body.value.span)
            });
        let Some(init_constant) = folded else {
            // Not the same `key` as the foldable path below — this const
            // isn't folding inline, it becomes a real `Global` reference
            // elsewhere in the program until a relower pass replaces it,
            // and a `Global` operand must be addressed by exactly the
            // same string the interpreter later publishes its resolved
            // value under. Source-span/surface-name strings (`const_key`)
            // aren't a stable identity for that; `def_id` already is —
            // see `DefId::comptime_const_symbol`'s own doc comment. Every
            // reference site (`lower_operand`'s `executable_consts.
            // get(def_id)` branches) derives the exact same string fresh
            // from this same `def_id`, so there's only ever one name.
            //
            // `Some(..)` for the last argument (not `None`) is what makes
            // `CompilerDriver::evaluate_comptime_lir_with` feed this
            // entry's interpreted result back via `record_const_block_
            // value` at all (it gates on `entry.const_block_hir_id.
            // is_some()`, `driver.rs:1240`) — without it, this const's
            // `ExecutableConst` placeholder never gets a chance to become
            // a real folded global on the relower pass above.
            return self.lower_executable_const(
                def_id.clone(),
                konst,
                ty,
                def_id.comptime_const_symbol(),
                Some(konst.body.hir_id.clone()),
            );
        };
        let init = mir::Operand::Constant(init_constant.clone());

        self.mir_package.borrow_mut().const_values.insert(
            def_id.clone(),
            ConstInfo {
                ty: ty.clone(),
                value: init_constant.clone(),
            },
        );
        let mir_static = mir::Static {
            name: konst.name.clone().into(),
            ty,
            init,
            mutability: mir::Mutability::Not,
        };

        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::Static(mir_static),
        };

        Ok(mir_item)
    }

    pub(super) fn lower_executable_const(
        &mut self,
        def_id: hir::DefId,
        konst: &hir::Const,
        ty: Ty,
        key: String,
        const_block_hir_id: Option<hir::HirId>,
    ) -> Result<mir::Item> {
        // Not `konst.name` (its bare surface name) — a `Global` operand
        // referencing this const elsewhere must be addressed by exactly
        // the same string the interpreter later publishes its resolved
        // value under (see `lower_const`'s own comment on this same
        // point). `DefId::comptime_const_symbol` is that one shared
        // identity, called fresh from `def_id` at every site that needs
        // to name this same entity.
        self.mir_package.borrow_mut().executable_consts.insert(
            def_id.clone(),
            (mir::Symbol::new(def_id.comptime_const_symbol()), ty.clone()),
        );
        let body_id = mir::BodyId::new(self.mir_package.borrow_mut().fresh_body_id());

        let fn_name = self.synthetic_const_function_name(&konst.name, &key);
        let synthetic_item = hir::Item {
            hir_id: konst.body.hir_id.clone(),
            def_id: def_id.clone(),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Function(hir::Function {
                sig: hir::FunctionSig {
                    name: hir::Symbol::new(fn_name.clone()),
                    inputs: Vec::new(),
                    output: konst.ty.clone(),
                    generics: hir::Generics {
                        params: Vec::new(),
                        where_clause: None,
                    },
                    abi: hir::Abi::Rust,
                },
                body: Some(hir::Block {
                    hir_id: konst.body.hir_id.clone(),
                    stmts: Vec::new(),
                    expr: Some(Box::new(konst.body.value.clone())),
                }),
                is_const: true,
                is_extern: false,
                is_async: false,
                attrs: Vec::new(),
            }),
            span: konst.body.value.span,
        };
        let hir::ItemKind::Function(function) = &synthetic_item.kind else {
            unreachable!();
        };

        let sig = mir::FunctionSig {
            inputs: Vec::new(),
            output: ty.clone(),
        };
        let body = self.lower_body(&synthetic_item, function, &sig, None)?;
        self.extra_bodies.push((body_id, body));

        let mir_item = mir::Item {
            mir_id: self.mir_package.borrow_mut().fresh_mir_id(),
            kind: mir::ItemKind::ExecutableConst(mir::ExecutableConst {
                name: mir::Symbol::from(&konst.name),
                function_name: mir::Symbol::new(fn_name),
                ty,
                body_id,
                key,
                span: konst.body.value.span,
                const_block_hir_id,
                def_id,
            }),
        };
        Ok(mir_item)
    }

    pub(super) fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        if let hir::TypeExprKind::Ref(inner) = &ty_expr.kind {
            if self.is_string_slice_ref(inner) {
                return self.string_slice_ty();
            }
        }
        if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
            if path.segments.last().is_some_and(|segment| {
                matches!(
                    segment.name.as_str(),
                    "bool"
                        | "char"
                        | "str"
                        | "i8"
                        | "i16"
                        | "i32"
                        | "i64"
                        | "i128"
                        | "isize"
                        | "u8"
                        | "u16"
                        | "u32"
                        | "u64"
                        | "u128"
                        | "usize"
                        | "f16"
                        | "f32"
                        | "f64"
                        | "f128"
                )
            }) {
                return self.lower_path_type(path, ty_expr.span);
            }
        }
        if let Some(ty) = self.typeck_type_expr_type(ty_expr.hir_id.clone()) {
            return ty;
        }
        match &ty_expr.kind {
            hir::TypeExprKind::Primitive(primitive) => {
                self.lower_primitive_type(primitive, ty_expr.span)
            }
            hir::TypeExprKind::Structural(structural) => {
                self.lower_structural_type_expr(structural, ty_expr.span)
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => {
                self.lower_type_binary_op_expr(type_op, ty_expr.span)
            }
            hir::TypeExprKind::Tuple(elements) => Ty {
                kind: TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.lower_type_expr(elem)))
                        .collect(),
                ),
            },
            hir::TypeExprKind::Array(elem, len_expr) => {
                let elem_ty = self.lower_type_expr(elem);
                let len = len_expr
                    .as_ref()
                    .and_then(|expr| self.eval_type_length(expr))
                    .unwrap_or(0);
                Ty {
                    kind: TyKind::Array(
                        Box::new(elem_ty),
                        ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                            data: len as u128,
                            size: 8,
                        }))),
                    ),
                }
            }
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr(elem);
                Ty {
                    kind: TyKind::Slice(Box::new(elem_ty)),
                }
            }
            hir::TypeExprKind::Ptr { inner: inner, .. } => {
                let inner_ty = self.lower_type_expr(inner);
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(inner_ty),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                if self.is_string_slice_ref(inner) {
                    return self.string_slice_ty();
                }
                let inner_ty = self.lower_type_expr(inner);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Path(path) => self.lower_path_type(path, ty_expr.span),
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                let inputs = fn_ptr
                    .inputs
                    .iter()
                    .map(|ty| Box::new(self.lower_type_expr(ty)))
                    .collect();
                let output = Box::new(self.lower_type_expr(&fn_ptr.output));
                Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs,
                                output,
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::Rust,
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                }
            }
            hir::TypeExprKind::Dynamic(_) => self
                .typeck_type_expr_type(ty_expr.hir_id.clone())
                .unwrap_or_else(|| self.error_ty()),
            hir::TypeExprKind::Never => Ty {
                kind: TyKind::Never,
            },
            hir::TypeExprKind::Infer => self.error_ty(),
            hir::TypeExprKind::Error => self.error_ty(),
            // The typeck-resolved type for this node is looked up via
            // `typeck_type_expr_type` above (populated from the type
            // checker's `resolve_pending_type_const_blocks`); reaching here
            // means that lookup missed, so fall back the same way `Infer`
            // does.
            hir::TypeExprKind::ConstBlock(_, _) => self.error_ty(),
            hir::TypeExprKind::Type => Ty { kind: TyKind::Type },
            hir::TypeExprKind::Any => Ty { kind: TyKind::Any },
            // Erases to `base`'s `TyKind` directly — there is deliberately
            // no corresponding `TyKind::Refinement` (see the doc comment on
            // `hir::TypeExprKind::Refinement`).
            hir::TypeExprKind::Refinement { base, .. } => self.lower_type_expr(base),
            // Erases to plain `str` — the typeck-resolved lookup above
            // should always hit (populated by `fp_typing::check_type_expr`'s
            // `LiteralString` arm); this is the same fallback shape as a
            // normal `str`.
            hir::TypeExprKind::LiteralString(_) => self.string_slice_ty(),
        }
    }

    pub(super) fn eval_type_length(&self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    self.mir_package
                        .borrow()
                        .const_values
                        .get(def_id)
                        .and_then(|info| match &info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value as u64),
                            mir::ConstantKind::UInt(value) => Some(*value),
                            _ => None,
                        })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn lower_structural_type_expr(
        &mut self,
        structural: &hir::TypeStructural,
        span: Span,
    ) -> Ty {
        let mut entries_ty: Option<&hir::TypeExpr> = None;
        if structural.fields.len() == 1 {
            if let Some(field) = structural.fields.first() {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                }
            }
        } else {
            for field in &structural.fields {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                    break;
                }
            }
        }

        if let Some(entries_ty) = entries_ty {
            let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
            match &entries_ty.kind {
                hir::TypeExprKind::Path(path) => {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Vec" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = Some(inner.as_ref());
                                    }
                                }
                            }
                        }
                    }
                }
                hir::TypeExprKind::Slice(inner) => {
                    entry_ty_expr = Some(inner.as_ref());
                }
                _ => {}
            }

            if let Some(mut entry_ty_expr) = entry_ty_expr {
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Expr" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = inner.as_ref();
                                    }
                                }
                            }
                        }
                    }
                }

                let mut key_ty_expr = None;
                let mut value_ty_expr = None;
                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        if let Some(tail) = path.segments.last() {
                            if tail.name.as_str() == "HashMapEntry" {
                                if let Some(args) = &tail.args {
                                    if args.args.len() == 2 {
                                        if let (
                                            hir::GenericArg::Type(key),
                                            hir::GenericArg::Type(value),
                                        ) = (&args.args[0], &args.args[1])
                                        {
                                            key_ty_expr = Some(key.as_ref());
                                            value_ty_expr = Some(value.as_ref());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            key_ty_expr = Some(fields[0].as_ref());
                            value_ty_expr = Some(fields[1].as_ref());
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }

                if let (Some(key_ty_expr), Some(value_ty_expr)) = (key_ty_expr, value_ty_expr) {
                    let key_ty = self.lower_type_expr(key_ty_expr);
                    let value_ty = self.lower_type_expr(value_ty_expr);
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Tuple(vec![Box::new(key_ty), Box::new(value_ty)]),
                        })),
                    };
                }
            }
        }

        let mut fields = Vec::with_capacity(structural.fields.len());
        for field in &structural.fields {
            fields.push(StructFieldDef {
                name: field.name.as_str().to_string(),
                ty: (*field.ty).clone(),
            });
        }

        let key_fields = fields
            .iter()
            .map(|field| (field.name.clone(), self.lower_type_expr(&field.ty)))
            .collect::<Vec<_>>();
        let key = StructuralLayoutKey { fields: key_fields };

        let def_id = if let Some(def_id) =
            self.mir_package.borrow().structural_defs.get(&key).cloned()
        {
            def_id
        } else {
            let def_id = self.mir_package.borrow_mut().fresh_synthetic_hir_def_id();
            let mut field_index = HashMap::new();
            for (idx, field) in fields.iter().enumerate() {
                if field_index.insert(field.name.clone(), idx).is_some() {
                    self.emit_error(span, format!("duplicate structural field `{}`", field.name));
                }
            }

            let name = format!("__structural_{}", def_id);
            self.mir_package
                .borrow_mut()
                .struct_defs_by_tail_name
                .entry(Self::name_tail(&name).to_string())
                .or_default()
                .push(def_id.clone());
            self.mir_package.borrow_mut().struct_defs.insert(
                def_id.clone(),
                StructDefinition {
                    name,
                    generics: Vec::new(),
                    fields: fields.clone(),
                    field_index,
                },
            );
            self.mir_package
                .borrow_mut()
                .structural_defs
                .insert(key, def_id.clone());
            def_id
        };

        self.struct_layout_for_instance(def_id, &[], span)
            .map(|layout| layout.ty)
            .unwrap_or_else(|| self.error_ty())
    }

    pub(super) fn lower_type_binary_op_expr(
        &mut self,
        type_op: &hir::TypeBinaryOp,
        span: Span,
    ) -> Ty {
        match type_op.kind {
            TypeBinaryOpKind::Union => self.lower_union_type_expr(&type_op.lhs, &type_op.rhs, span),
            TypeBinaryOpKind::Add | TypeBinaryOpKind::Intersect | TypeBinaryOpKind::Subtract => {
                let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span);
                let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span);
                let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                    self.emit_error(
                        span,
                        "type arithmetic requires structural or named struct operands",
                    );
                    return self.error_ty();
                };

                let combined = match type_op.kind {
                    TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Intersect => self.intersect_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Subtract => self.subtract_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Union => unreachable!("union handled above"),
                };
                let fields = combined
                    .into_iter()
                    .map(|field| hir::TypeStructuralField {
                        name: hir::Symbol::new(field.name),
                        ty: Box::new(field.ty),
                    })
                    .collect::<Vec<_>>();
                self.lower_structural_type_expr(&hir::TypeStructural { fields }, span)
            }
        }
    }

    pub(super) fn structural_fields_for_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
        span: Span,
    ) -> Option<Vec<StructFieldDef>> {
        match &ty_expr.kind {
            hir::TypeExprKind::Structural(structural) => Some(
                structural
                    .fields
                    .iter()
                    .map(|field| StructFieldDef {
                        name: field.name.as_str().to_string(),
                        ty: (*field.ty).clone(),
                    })
                    .collect(),
            ),
            hir::TypeExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(def) = self.mir_package.borrow().struct_defs.get(def_id).cloned() {
                        return Some(def.fields.clone());
                    }
                }
                self.emit_error(
                    span,
                    "type arithmetic requires struct operands with known definitions",
                );
                None
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => match type_op.kind {
                TypeBinaryOpKind::Add
                | TypeBinaryOpKind::Intersect
                | TypeBinaryOpKind::Subtract => {
                    let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span)?;
                    let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span)?;
                    Some(match type_op.kind {
                        TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                        TypeBinaryOpKind::Intersect => {
                            self.intersect_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Subtract => {
                            self.subtract_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Union => unreachable!("union handled separately"),
                    })
                }
                TypeBinaryOpKind::Union => None,
            },
            _ => None,
        }
    }

    pub(super) fn merge_structural_fields(
        &mut self,
        span: Span,
        mut lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        for rhs_field in rhs {
            if let Some(existing) = lhs.iter().find(|field| field.name == rhs_field.name) {
                if !self.type_exprs_equivalent(&existing.ty, &rhs_field.ty) {
                    self.emit_error(
                        span,
                        format!(
                            "conflicting field types for `{}` in structural merge",
                            rhs_field.name
                        ),
                    );
                }
                continue;
            }
            lhs.push(rhs_field);
        }
        lhs
    }

    pub(super) fn intersect_structural_fields(
        &mut self,
        span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter_map(|field| {
                rhs.iter()
                    .find(|rhs_field| rhs_field.name == field.name)
                    .map(|rhs_field| {
                        if !self.type_exprs_equivalent(&rhs_field.ty, &field.ty) {
                            self.emit_error(
                                span,
                                format!(
                                    "conflicting field types for `{}` in structural intersect",
                                    field.name
                                ),
                            );
                        }
                        field.clone()
                    })
            })
            .collect()
    }

    pub(super) fn subtract_structural_fields(
        &mut self,
        _span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter(|field| !rhs.iter().any(|rhs_field| rhs_field.name == field.name))
            .collect()
    }

    pub(super) fn type_exprs_equivalent(&self, lhs: &hir::TypeExpr, rhs: &hir::TypeExpr) -> bool {
        match (&lhs.kind, &rhs.kind) {
            (hir::TypeExprKind::Primitive(a), hir::TypeExprKind::Primitive(b)) => a == b,
            (hir::TypeExprKind::Path(a), hir::TypeExprKind::Path(b)) => {
                if a.segments.len() != b.segments.len() {
                    return false;
                }
                for (a_seg, b_seg) in a.segments.iter().zip(b.segments.iter()) {
                    if a_seg.name != b_seg.name {
                        return false;
                    }
                    match (&a_seg.args, &b_seg.args) {
                        (None, None) => {}
                        (Some(a_args), Some(b_args)) => {
                            if a_args.args.len() != b_args.args.len() {
                                return false;
                            }
                            for (a_arg, b_arg) in a_args.args.iter().zip(b_args.args.iter()) {
                                match (a_arg, b_arg) {
                                    (hir::GenericArg::Type(a_ty), hir::GenericArg::Type(b_ty)) => {
                                        if !self.type_exprs_equivalent(a_ty, b_ty) {
                                            return false;
                                        }
                                    }
                                    (hir::GenericArg::Const(_), hir::GenericArg::Const(_)) => {}
                                    _ => return false,
                                }
                            }
                        }
                        _ => return false,
                    }
                }
                true
            }
            (hir::TypeExprKind::Structural(a), hir::TypeExprKind::Structural(b)) => {
                if a.fields.len() != b.fields.len() {
                    return false;
                }
                for (a_field, b_field) in a.fields.iter().zip(b.fields.iter()) {
                    if a_field.name != b_field.name {
                        return false;
                    }
                    if !self.type_exprs_equivalent(&a_field.ty, &b_field.ty) {
                        return false;
                    }
                }
                true
            }
            (hir::TypeExprKind::TypeBinaryOp(a), hir::TypeExprKind::TypeBinaryOp(b)) => {
                a.kind == b.kind
                    && self.type_exprs_equivalent(&a.lhs, &b.lhs)
                    && self.type_exprs_equivalent(&a.rhs, &b.rhs)
            }
            (hir::TypeExprKind::Tuple(a), hir::TypeExprKind::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
            }
            (hir::TypeExprKind::Array(a_elem, _), hir::TypeExprKind::Array(b_elem, _)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Slice(a_elem), hir::TypeExprKind::Slice(b_elem)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Ptr { inner: a, .. }, hir::TypeExprKind::Ptr { inner: b, .. }) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::Ref(a), hir::TypeExprKind::Ref(b)) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::FnPtr(a), hir::TypeExprKind::FnPtr(b)) => {
                if a.inputs.len() != b.inputs.len() {
                    return false;
                }
                if !a
                    .inputs
                    .iter()
                    .zip(b.inputs.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
                {
                    return false;
                }
                self.type_exprs_equivalent(&a.output, &b.output)
            }
            (hir::TypeExprKind::Never, hir::TypeExprKind::Never) => true,
            (hir::TypeExprKind::Infer, hir::TypeExprKind::Infer) => true,
            (hir::TypeExprKind::Error, hir::TypeExprKind::Error) => true,
            _ => false,
        }
    }

    pub(super) fn lower_union_type_expr(
        &mut self,
        lhs: &hir::TypeExpr,
        rhs: &hir::TypeExpr,
        span: Span,
    ) -> Ty {
        let def_id = self.mir_package.borrow_mut().fresh_synthetic_hir_def_id();
        let enum_name = format!("__union_{}", def_id);

        let lhs_name = self.union_variant_name(lhs, "Left");
        let mut rhs_name = self.union_variant_name(rhs, "Right");
        if lhs_name == rhs_name {
            rhs_name = format!("{}_rhs", rhs_name);
        }

        let lhs_payload = match lhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(lhs) => None,
            _ => Some(lhs.clone()),
        };
        let rhs_payload = match rhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(rhs) => None,
            _ => Some(rhs.clone()),
        };

        let variants = vec![
            EnumVariantDef {
                def_id: self.mir_package.borrow_mut().fresh_synthetic_hir_def_id(),
                name: lhs_name,
                discriminant: 0,
                payload: lhs_payload,
            },
            EnumVariantDef {
                def_id: self.mir_package.borrow_mut().fresh_synthetic_hir_def_id(),
                name: rhs_name,
                discriminant: 1,
                payload: rhs_payload,
            },
        ];

        self.register_synthetic_enum(def_id.clone(), enum_name, variants, span);

        match self.enum_layout_for_instance(def_id, &[], span) {
            Some(layout) => self.nominal_enum_ty(&layout),
            None => self.error_ty(),
        }
    }

    pub(super) fn union_variant_name(&self, ty_expr: &hir::TypeExpr, fallback: &str) -> String {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str().to_string())
                .filter(|name| !name.is_empty())
                .unwrap_or_else(|| fallback.to_string()),
            hir::TypeExprKind::Structural(structural) => {
                let mut matches = self
                    .mir_package
                    .borrow()
                    .struct_defs
                    .values()
                    .filter(|def| def.fields.len() == structural.fields.len())
                    .filter(|def| {
                        def.fields.iter().zip(structural.fields.iter()).all(
                            |(def_field, struct_field)| {
                                def_field.name == struct_field.name.as_str()
                                    && self.type_exprs_equivalent(&def_field.ty, &struct_field.ty)
                            },
                        )
                    })
                    .map(|def| def.name.clone())
                    .collect::<Vec<_>>();
                if let Some(name) = matches
                    .iter()
                    .find(|name| !name.starts_with("__structural_"))
                {
                    return name.clone();
                }
                matches.pop().unwrap_or_else(|| fallback.to_string())
            }
            _ => fallback.to_string(),
        }
    }

    pub(super) fn is_null_type_expr(&self, ty_expr: &hir::TypeExpr) -> bool {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str() == "null")
                .unwrap_or(false),
            _ => false,
        }
    }

    pub(super) fn register_synthetic_enum(
        &mut self,
        def_id: hir::DefId,
        name: String,
        variants: Vec<EnumVariantDef>,
        span: Span,
    ) {
        if self.mir_package.borrow().enum_defs.contains_key(&def_id) {
            return;
        }

        for variant in &variants {
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(def_id.clone());
                    }
                }
                None
            });
            self.mir_package.borrow_mut().enum_variants.insert(
                variant.def_id.clone(),
                EnumVariantInfo {
                    def_id: variant.def_id.clone(),
                    enum_def: def_id.clone(),
                    discriminant: variant.discriminant,
                    payload_def,
                },
            );

            let qualified_name = format!("{}::{}", name, variant.name);
            self.mir_package
                .borrow_mut()
                .enum_variant_names
                .insert(qualified_name.clone(), variant.def_id.clone());
            self.mir_package
                .borrow_mut()
                .enum_variant_names
                .entry(variant.name.clone())
                .or_insert(variant.def_id.clone());
        }

        self.mir_package
            .borrow_mut()
            .enum_defs_by_name
            .entry(name.clone())
            .or_insert(def_id.clone());
        self.mir_package.borrow_mut().enum_defs.insert(
            def_id.clone(),
            EnumDefinition {
                def_id: def_id.clone(),
                name,
                generics: Vec::new(),
                variants,
            },
        );

        // JUSTIFY: layout may be uncomputable for forward-referenced types
        // during registration; computed lazily when needed later.
        if self.enum_layout_for_instance(def_id, &[], span).is_none() {
            self.emit_warning(
                span,
                "enum layout computation returned None during registration",
            );
        }
    }

    pub(super) fn lower_primitive_type(&mut self, primitive: &TypePrimitive, span: Span) -> Ty {
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
                TypeInt::I128 => Ty {
                    kind: TyKind::Int(IntTy::I128),
                },
                TypeInt::U8 => Ty {
                    kind: TyKind::Uint(UintTy::U8),
                },
                TypeInt::U16 => Ty {
                    kind: TyKind::Uint(UintTy::U16),
                },
                TypeInt::U32 => Ty {
                    kind: TyKind::Uint(UintTy::U32),
                },
                TypeInt::U64 => Ty {
                    kind: TyKind::Uint(UintTy::U64),
                },
                TypeInt::U128 => Ty {
                    kind: TyKind::Uint(UintTy::U128),
                },
                TypeInt::BigInt => {
                    self.emit_error(span, "big integers are not yet supported in MIR");
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
                    self.emit_warning(span, "lowering arbitrary precision decimal to f64 in MIR");
                    Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    }
                }
            },
            TypePrimitive::String => self.string_slice_ty(),
            TypePrimitive::List => {
                self.emit_warning(
                    span,
                    "treating list primitive as opaque type during MIR lowering",
                );
                self.opaque_ty("list")
            }
        }
    }

    pub(super) fn resolve_path_def_id(&self, path: &hir::Path) -> Option<hir::DefId> {
        match path.res {
            Some(hir::Res::Def(ref def_id)) => Some(def_id.clone()),
            _ => None,
        }
    }

    pub(super) fn lower_path_type(&mut self, path: &hir::Path, span: Span) -> Ty {
        if let Some(def_id) = self.resolve_path_def_id(path) {
            if self.mir_package.borrow().struct_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.struct_layout_for_instance(def_id, &args, span) {
                    return layout.ty.clone();
                }
                return self.error_ty();
            }
            if self.mir_package.borrow().enum_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.enum_layout_for_instance(def_id, &args, span) {
                    return self.nominal_enum_ty(&layout);
                }
                return self.error_ty();
            }
            if let Some(sig) = self
                .mir_package
                .borrow()
                .function_sigs
                .get(&def_id)
                .cloned()
            {
                return Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs: sig.inputs.iter().map(|ty| Box::new(ty.clone())).collect(),
                                output: Box::new(sig.output.clone()),
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::C { unwind: false },
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                };
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            if name == "Vec" || name == "List" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(elem_ty) = args.first().cloned() {
                    return Ty {
                        kind: TyKind::Slice(Box::new(elem_ty)),
                    };
                }
                self.emit_error(span, "Vec/List requires a single type argument");
                return self.error_ty();
            }
            if name == "HashMap" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if args.len() == 2 {
                    let entry_ty = Ty {
                        kind: TyKind::Tuple(vec![
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ]),
                    };
                    return Ty {
                        kind: TyKind::Slice(Box::new(entry_ty)),
                    };
                }
                self.emit_error(span, "HashMap requires two type arguments");
                return self.error_ty();
            }
        }

        if let Some(res) = &path.res {
            if let hir::Res::Def(def_id) = res {
                if self.mir_package.borrow().struct_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) =
                        self.struct_layout_for_instance(def_id.clone(), &args, span)
                    {
                        return layout.ty.clone();
                    }
                    return self.error_ty();
                }
                if self.mir_package.borrow().enum_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) = self.enum_layout_for_instance(def_id.clone(), &args, span)
                    {
                        return self.nominal_enum_ty(&layout);
                    }
                    return self.error_ty();
                }
                if let Some(sig) = self.mir_package.borrow().function_sigs.get(def_id).cloned() {
                    // Treat function types as function pointers when referenced as types
                    return Ty {
                        kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                            binder: mir::ty::Binder {
                                value: mir::ty::FnSig {
                                    inputs: sig
                                        .inputs
                                        .iter()
                                        .map(|ty| Box::new(ty.clone()))
                                        .collect(),
                                    output: Box::new(sig.output.clone()),
                                    c_variadic: false,
                                    unsafety: mir::ty::Unsafety::Normal,
                                    abi: mir::ty::Abi::C { unwind: false },
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    };
                }
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.clone();
            match name.as_str() {
                "i8" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I8),
                    };
                }
                "i16" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I16),
                    };
                }
                "i32" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I32),
                    };
                }
                "i64" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I64),
                    };
                }
                "i128" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I128),
                    };
                }
                "usize" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                }
                "isize" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::Isize),
                    };
                }
                "u8" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U8),
                    };
                }
                "u16" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U16),
                    };
                }
                "u32" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U32),
                    };
                }
                "u64" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    };
                }
                "u128" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U128),
                    };
                }
                "bool" => return Ty { kind: TyKind::Bool },
                "char" => return Ty { kind: TyKind::Char },
                "f16" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F16),
                    };
                }
                "f32" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F32),
                    };
                }
                "f64" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    };
                }
                "f128" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F128),
                    };
                }
                "str" => {
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    };
                }
                "null" => {
                    return self.raw_string_ptr_ty();
                }
                _ => {}
            }
        }

        let display = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.emit_error(span, format!("unresolved type path `{display}`"));
        self.error_ty()
    }
}
