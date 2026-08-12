use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::{Error, Result};
use fp_core::hir;
use fp_core::hir::ty::{self, AdtDef, AdtFlags, GenericArg, ReprFlags, ReprOptions, Ty, TyKind};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use crate::TypingContext;
use crate::types::{GenericCallResolution, TypeckResults};
use std::rc::Rc;

/// Type checks resolved HIR and records semantic types outside the source tree.
/// This is deliberately a side-table pass: HIR nodes remain source-shaped and
/// MIR lowering can consume the results without an AST round trip.
pub struct HirTypeChecker {
    program: hir::Program,
    results: TypeckResults,
    locals: Vec<HashMap<hir::Symbol, Ty>>,
    generic_scopes: Vec<HashMap<hir::DefId, Ty>>,
    self_types: Vec<Ty>,
    typing_context: Option<Rc<TypingContext>>,
    expected_expr_types: Vec<Ty>,
    /// Type-position `const { ... }` blocks encountered while checking
    /// types (which is synchronous). Resolved via comptime once the main
    /// item walk finishes; see `resolve_pending_type_const_blocks`.
    pending_type_const_blocks: Vec<(hir::HirId, hir::Expr)>,
}

struct GenericScope<'a> {
    checker: &'a mut HirTypeChecker,
}

impl Deref for GenericScope<'_> {
    type Target = HirTypeChecker;

    fn deref(&self) -> &Self::Target {
        self.checker
    }
}

impl DerefMut for GenericScope<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.checker
    }
}

impl Drop for GenericScope<'_> {
    fn drop(&mut self) {
        self.checker.generic_scopes.pop();
    }
}

impl HirTypeChecker {
    pub fn new(program: hir::Program) -> Self {
        Self {
            program,
            results: TypeckResults::default(),
            locals: vec![HashMap::new()],
            generic_scopes: Vec::new(),
            self_types: Vec::new(),
            typing_context: None,
            expected_expr_types: Vec::new(),
            pending_type_const_blocks: Vec::new(),
        }
    }

    pub fn with_context(mut self, context: Rc<TypingContext>) -> Self {
        self.typing_context = Some(context);
        self
    }

    pub fn check(self) -> crate::BoxFuture<'static, Result<(hir::Program, TypeckResults)>> {
        Box::pin(async move { self.check_async().await })
    }

    async fn check_async(mut self) -> Result<(hir::Program, TypeckResults)> {
        let items = self.program.items.clone();
        for item in &items {
            self.check_item(item).await?;
        }
        self.resolve_pending_type_const_blocks().await?;
        Ok((self.program, self.results))
    }

    /// Resolve `const { ... }` blocks encountered in type position
    /// (`check_type_expr` is synchronous, so it defers these rather than
    /// awaiting inline). Structural, not name-based: anything queued here
    /// got there solely by being a `TypeExprKind::ConstBlock` node.
    async fn resolve_pending_type_const_blocks(&mut self) -> Result<()> {
        let pending = std::mem::take(&mut self.pending_type_const_blocks);
        for (hir_id, body) in pending {
            let body_ty = self.check_expr(&body).await?;
            let Some(context) = self.typing_context.clone() else {
                continue;
            };
            let value = context
                .request_comptime(crate::ComptimeRequest {
                    program: self.program.clone(),
                    typeck_results: self.results.clone(),
                    block: hir::Block {
                        hir_id,
                        stmts: Vec::new(),
                        expr: Some(Box::new(body)),
                    },
                    expression_id: hir_id,
                    expected_ty: hir::TypeExpr {
                        hir_id,
                        kind: hir::TypeExprKind::Infer,
                        span: fp_core::span::Span::null(),
                    },
                })
                .await?;
            self.results.const_block_values.insert(hir_id, value);
            // Replace the `Infer` placeholder `check_type_expr` recorded for
            // this node with the body's actual checked type, now that it's
            // known — matches expression-position const-blocks, whose own
            // type is likewise the checked type of their body.
            self.results.record_type_expr_type(hir_id, body_ty);
        }
        Ok(())
    }

    fn check_item<'a>(&'a mut self, item: &'a hir::Item) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            match &item.kind {
                hir::ItemKind::Function(function) => {
                    self.check_function(function).await?;
                }
                hir::ItemKind::Const(constant) => {
                    let declared_ty = self.check_type_expr(&constant.ty)?;
                    self.expected_expr_types.push(declared_ty.clone());
                    let body_result = self.check_body(&constant.body).await;
                    self.expected_expr_types.pop();
                    let body_ty = body_result?;
                    self.results
                        .type_expr_types
                        .insert(constant.ty.hir_id, body_ty.clone());
                    self.results.const_types.insert(item.def_id, body_ty);
                }
                hir::ItemKind::Impl(impl_item) => {
                    let mut scope = self.generic_scope(&impl_item.generics);
                    let self_ty = scope.check_type_expr(&impl_item.self_ty)?;
                    scope.self_types.push(self_ty);
                    if let Some(trait_ty) = &impl_item.trait_ty {
                        scope.check_type_expr(trait_ty)?;
                    }
                    for item in &impl_item.items {
                        match &item.kind {
                            hir::ImplItemKind::Method(function) => {
                                scope.check_function(function).await?
                            }
                            hir::ImplItemKind::AssocConst(constant) => {
                                scope.check_type_expr(&constant.ty)?;
                                scope.check_body(&constant.body).await?;
                            }
                        }
                    }
                    scope.self_types.pop();
                }
                hir::ItemKind::Struct(def) => {
                    let mut scope = self.generic_scope(&def.generics);
                    for field in &def.fields {
                        scope.check_type_expr(&field.ty)?;
                    }
                }
                hir::ItemKind::Enum(def) => {
                    let mut scope = self.generic_scope(&def.generics);
                    for variant in &def.variants {
                        if let Some(payload) = &variant.payload {
                            scope.check_type_expr(payload)?;
                        }
                        if let Some(discriminant) = &variant.discriminant {
                            scope.check_expr(discriminant).await?;
                        }
                    }
                }
                hir::ItemKind::Query(_) => {}
                hir::ItemKind::Expr(expr) => {
                    self.check_expr(expr).await?;
                }
            }
            Ok(())
        })
    }

    fn check_function<'a>(
        &'a mut self,
        function: &'a hir::Function,
    ) -> crate::BoxFuture<'a, Result<()>> {
        Box::pin(async move {
            self.push_generics(&function.sig.generics);
            self.check_signature(&function.sig).map_err(|error| {
                Error::from(format!(
                    "in function `{}` signature: {error}",
                    function.sig.name
                ))
            })?;
            if let Some(body) = &function.body {
                self.check_function_body(&function.sig.inputs, body)
                    .await
                    .map_err(|error| {
                        Error::from(format!("in function `{}` body: {error}", function.sig.name))
                    })?;
            }
            self.generic_scopes.pop();
            Ok(())
        })
    }

    fn push_generics(&mut self, generics: &hir::Generics) {
        let mut scope = HashMap::new();
        for (index, parameter) in generics.params.iter().enumerate() {
            if matches!(parameter.kind, hir::GenericParamKind::Type { .. }) {
                scope.insert(
                    parameter.def_id,
                    Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: index as u32,
                            name: parameter.name.clone(),
                        }),
                    },
                );
            }
        }
        self.generic_scopes.push(scope);
    }

    fn generic_scope(&mut self, generics: &hir::Generics) -> GenericScope<'_> {
        self.push_generics(generics);
        GenericScope { checker: self }
    }

    fn generic_ty(&self, def_id: hir::DefId) -> Option<Ty> {
        self.generic_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&def_id).cloned())
    }

    fn check_signature(&mut self, signature: &hir::FunctionSig) -> Result<()> {
        for input in &signature.inputs {
            self.check_type_expr(&input.ty)?;
        }
        self.check_type_expr(&signature.output)?;
        Ok(())
    }

    async fn check_body(&mut self, body: &hir::Body) -> Result<Ty> {
        self.locals.push(HashMap::new());
        for param in &body.params {
            let ty = self.check_type_expr(&param.ty)?;
            self.bind_pattern(&param.pat, ty)?;
        }
        let value_ty = self.check_expr(&body.value).await?;
        self.locals.pop();
        Ok(value_ty)
    }

    async fn check_function_body(
        &mut self,
        params: &[hir::Param],
        block: &hir::Block,
    ) -> Result<()> {
        self.locals.push(HashMap::new());
        for param in params {
            let ty = self.check_type_expr(&param.ty)?;
            self.bind_pattern(&param.pat, ty)?;
        }
        self.check_block(block).await?;
        self.locals.pop();
        Ok(())
    }

    fn check_expr<'a>(&'a mut self, expr: &'a hir::Expr) -> crate::BoxFuture<'a, Result<Ty>> {
        Box::pin(async move {
            let ty = match &expr.kind {
                hir::ExprKind::Literal(lit) => self.literal_ty(lit),
                hir::ExprKind::Path(path) => self.expr_path_ty(path)?,
                hir::ExprKind::Binary(op, lhs, rhs) => {
                    let lhs_literal =
                        matches!(lhs.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                    let rhs_literal =
                        matches!(rhs.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                    let lhs = self.check_expr(lhs).await?;
                    let rhs = self.check_expr(rhs).await?;
                    let integer_literal = (lhs_literal
                        && matches!(rhs.kind, TyKind::Int(_) | TyKind::Uint(_)))
                        || (rhs_literal && matches!(lhs.kind, TyKind::Int(_) | TyKind::Uint(_)));
                    if !integer_literal {
                        match op {
                            hir::BinOp::And | hir::BinOp::Or => {
                                self.require_same(&lhs, &Ty::bool())?;
                                self.require_same(&rhs, &Ty::bool())?;
                            }
                            hir::BinOp::Eq
                            | hir::BinOp::Ne
                            | hir::BinOp::Lt
                            | hir::BinOp::Le
                            | hir::BinOp::Gt
                            | hir::BinOp::Ge => {
                                self.require_same(&lhs, &rhs)?;
                            }
                            _ => {
                                self.require_same(&lhs, &rhs)?;
                            }
                        }
                    }
                    match op {
                        hir::BinOp::Eq
                        | hir::BinOp::Ne
                        | hir::BinOp::Lt
                        | hir::BinOp::Le
                        | hir::BinOp::Gt
                        | hir::BinOp::Ge
                        | hir::BinOp::And
                        | hir::BinOp::Or => Ty::bool(),
                        _ => lhs,
                    }
                }
                hir::ExprKind::Unary(op, value) => {
                    let value_ty = self.check_expr(value).await?;
                    match op {
                        hir::UnOp::Not => {
                            self.require_same(&value_ty, &Ty::bool())?;
                            Ty::bool()
                        }
                        hir::UnOp::Deref => match value_ty.kind {
                            TyKind::Ref(_, inner, _)
                            | TyKind::RawPtr(ty::TypeAndMut { ty: inner, .. }) => *inner,
                            _ => return Err(Error::from("cannot dereference a non-pointer value")),
                        },
                        hir::UnOp::Neg | hir::UnOp::Box => value_ty,
                    }
                }
                hir::ExprKind::Reference(reference) => Ty {
                    kind: TyKind::Ref(
                        ty::Region::ReErased,
                        Box::new(self.check_expr(&reference.expr).await?),
                        reference.mutable,
                    ),
                },
                hir::ExprKind::Call(callee, args) => {
                    let callee_ty = self.check_expr(callee).await?;
                    let expected_inputs = match &callee_ty.kind {
                        TyKind::FnPtr(signature) => Some(signature.binder.value.inputs.clone()),
                        _ => None,
                    };
                    let mut arg_types = Vec::with_capacity(args.len());
                    for (index, arg) in args.iter().enumerate() {
                        let actual = self.check_expr(&arg.value).await?;
                        let actual = match expected_inputs
                            .as_ref()
                            .and_then(|inputs| inputs.get(index))
                        {
                            Some(expected)
                                if matches!(
                                    arg.value.kind,
                                    hir::ExprKind::Literal(hir::Lit::Integer(_))
                                ) && matches!(
                                    expected.kind,
                                    TyKind::Int(_) | TyKind::Uint(_)
                                ) =>
                            {
                                // Integer literals can take the type of their direct parameter.
                                (**expected).clone()
                            }
                            _ => actual,
                        };
                        arg_types.push(actual);
                    }
                    let Some((mut substitutions, _)) =
                        self.instantiate_call(&callee_ty, &arg_types)?
                    else {
                        return Err(Error::from("called expression is not a function"));
                    };
                    if substitutions.is_empty() {
                        if let Some(expected) = self.expected_expr_types.last() {
                            if let TyKind::FnPtr(signature) = &callee_ty.kind {
                                self.unify_call_types(
                                    &signature.binder.value.output,
                                    expected,
                                    &mut substitutions,
                                )?;
                            }
                        }
                    }
                    let output = match &callee_ty.kind {
                        TyKind::FnPtr(signature) => self
                            .substitute_param_map(&signature.binder.value.output, &substitutions),
                        _ => unreachable!(),
                    };
                    if let hir::ExprKind::Path(path) = &callee.kind {
                        if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                            let args = self
                                .generic_call_args(*def_id, &substitutions)?
                                .or_else(|| self.callable_output_args(&callee_ty, &substitutions));
                            if let Some(args) = args {
                                self.results.generic_call_args.insert(
                                    expr.hir_id,
                                    GenericCallResolution {
                                        def_id: *def_id,
                                        args,
                                    },
                                );
                            }
                        }
                    }
                    output
                }
                hir::ExprKind::MethodCall(receiver, method, args) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    let mut arg_types = vec![receiver_ty.clone()];
                    for arg in args {
                        arg_types.push(self.check_expr(&arg.value).await?);
                    }
                    let (method_def_id, generic_args, output) =
                        self.method_output(&receiver_ty, method, &arg_types)?;
                    self.results
                        .method_resolutions
                        .insert(expr.hir_id, method_def_id);
                    if let Some(args) = generic_args {
                        self.results.generic_method_args.insert(
                            expr.hir_id,
                            GenericCallResolution {
                                def_id: method_def_id,
                                args,
                            },
                        );
                    }
                    output
                }
                hir::ExprKind::FieldAccess(receiver, field) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    self.field_ty(&receiver_ty, field)?
                }
                hir::ExprKind::Index(receiver, index) => {
                    let receiver_ty = self.check_expr(receiver).await?;
                    let index_ty = self.check_expr(index).await?;
                    self.require_same(&index_ty, &Ty::int(ty::IntTy::I64))?;
                    let receiver_ty = match &receiver_ty.kind {
                        TyKind::Ref(_, inner, _) => inner.as_ref(),
                        _ => &receiver_ty,
                    };
                    match &receiver_ty.kind {
                        TyKind::Array(inner, _) | TyKind::Slice(inner) => (**inner).clone(),
                        _ => return Err(Error::from("indexing requires an array or slice")),
                    }
                }
                hir::ExprKind::Cast(value, target) => {
                    self.check_expr(value).await?;
                    self.check_type_expr(target)?
                }
                hir::ExprKind::Struct(path, fields) => {
                    let ty = match self.enum_variant_ty(path)? {
                        Some(ty) => ty,
                        None => self.path_ty(path)?,
                    };
                    let payload_ty = self.enum_struct_payload_type(path, &ty)?;
                    for field in fields {
                        let value_ty = self.check_expr(&field.expr).await?;
                        let field_ty = if let Some(payload) = payload_ty.as_ref() {
                            self.field_ty(payload, &field.name)?
                        } else {
                            self.field_ty(&ty, &field.name)?
                        };
                        self.require_same(&value_ty, &field_ty)?;
                    }
                    ty
                }
                hir::ExprKind::If(condition, then_expr, else_expr) => {
                    let condition = self.check_expr(condition).await?;
                    self.require_same(&condition, &Ty::bool())?;
                    let then_ty = self.check_expr(then_expr).await?;
                    if let Some(else_expr) = else_expr {
                        let else_ty = self.check_expr(else_expr).await?;
                        self.require_same(&then_ty, &else_ty)?;
                    }
                    match else_expr.as_ref() {
                        Some(_) => then_ty,
                        None => self.unit_ty(),
                    }
                }
                hir::ExprKind::Match(scrutinee, arms) => {
                    let scrutinee_ty = self.check_expr(scrutinee).await?;
                    if arms.is_empty() {
                        return Err(Error::from("match expression requires at least one arm"));
                    }
                    let mut result = None;
                    for arm in arms {
                        let arm_ty = self.check_match_arm(arm, &scrutinee_ty).await?;
                        if let Some(result_ty) = &result {
                            self.require_same(result_ty, &arm_ty)?;
                        } else {
                            result = Some(arm_ty);
                        }
                    }
                    result
                        .ok_or_else(|| Error::from("match expression requires at least one arm"))?
                }
                hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                    self.check_block(block).await?
                }
                hir::ExprKind::ConstBlock(const_block) => {
                    let declared_ty = self.check_type_expr(&const_block.ty)?;
                    self.expected_expr_types.push(declared_ty.clone());
                    let body_result = self.check_expr(&const_block.body).await;
                    self.expected_expr_types.pop();
                    let body_ty = body_result?;
                    if let Some(context) = self.typing_context.clone() {
                        let value = context
                            .request_comptime(crate::ComptimeRequest {
                                program: self.program.clone(),
                                typeck_results: self.results.clone(),
                                block: hir::Block {
                                    hir_id: expr.hir_id,
                                    stmts: Vec::new(),
                                    expr: Some(const_block.body.clone()),
                                },
                                expression_id: expr.hir_id,
                                expected_ty: (*const_block.ty).clone(),
                            })
                            .await?;
                        self.results.const_block_values.insert(expr.hir_id, value);
                    }
                    body_ty
                }
                hir::ExprKind::While(condition, block) => {
                    let condition_ty = self.check_expr(condition).await?;
                    self.require_same(&condition_ty, &Ty::bool())?;
                    self.check_block(block).await?
                }
                hir::ExprKind::Array(values) => {
                    if values.is_empty() {
                        return Err(Error::from("empty array has no inferable element type"));
                    }
                    let mut value_types = Vec::with_capacity(values.len());
                    for value in values {
                        value_types.push(self.check_expr(value).await?);
                    }
                    let element = values
                        .iter()
                        .zip(&value_types)
                        .find_map(|(value, value_ty)| {
                            (!matches!(value.kind, hir::ExprKind::Literal(hir::Lit::Integer(_))))
                                .then(|| value_ty.clone())
                        })
                        .unwrap_or_else(|| value_types[0].clone());
                    for (value, value_ty) in values.iter().zip(value_types) {
                        let integer_literal =
                            matches!(value.kind, hir::ExprKind::Literal(hir::Lit::Integer(_)));
                        let integer_element =
                            matches!(element.kind, TyKind::Int(_) | TyKind::Uint(_));
                        if !(integer_literal && integer_element) {
                            self.require_same(&element, &value_ty)?;
                        }
                    }
                    Ty {
                        kind: TyKind::Array(
                            Box::new(element),
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: values.len() as u128,
                                    size: 8,
                                },
                            ))),
                        ),
                    }
                }
                hir::ExprKind::ArrayRepeat { elem, len } => {
                    let element = self.check_expr(elem).await?;
                    self.check_expr(len).await?;
                    let length = match &len.kind {
                        hir::ExprKind::Literal(hir::Lit::Integer(value)) if *value >= 0 => {
                            ty::ConstKind::Value(ty::ConstValue::Scalar(ty::Scalar::Int(
                                ty::ScalarInt {
                                    data: *value as u128,
                                    size: 8,
                                },
                            )))
                        }
                        _ => ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id)),
                    };
                    Ty {
                        kind: TyKind::Array(Box::new(element), length),
                    }
                }
                hir::ExprKind::Tuple(values) => {
                    let mut element_types = Vec::with_capacity(values.len());
                    for value in values {
                        element_types.push(Box::new(self.check_expr(value).await?));
                    }
                    Ty {
                        kind: TyKind::Tuple(element_types),
                    }
                }
                hir::ExprKind::Assign(lhs, rhs) => {
                    let lhs = self.check_expr(lhs).await?;
                    let rhs = self.check_expr(rhs).await?;
                    self.require_same(&lhs, &rhs)?;
                    lhs
                }
                hir::ExprKind::Return(value) | hir::ExprKind::Break(value) => {
                    match value.as_ref() {
                        Some(value) => self.check_expr(value).await?,
                        None => self.unit_ty(),
                    }
                }
                hir::ExprKind::Continue => Ty::never(),
                hir::ExprKind::Let(pattern, target, value) => {
                    let ty = self.check_type_expr(target)?;
                    if let Some(value) = value {
                        let value_ty = self.check_expr(value).await?;
                        self.require_same(&ty, &value_ty)?;
                    }
                    self.bind_pattern(pattern, ty.clone())?;
                    ty
                }
                hir::ExprKind::Try(value) => {
                    let input_ty = self.check_expr(&value.expr).await?;
                    let result_ty = input_ty.clone();
                    for catch in &value.catches {
                        if let Some(pattern) = &catch.pat {
                            self.bind_pattern(
                                pattern,
                                Ty {
                                    kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
                                },
                            )?;
                        }
                        let catch_ty = self.check_expr(&catch.body).await?;
                        self.require_same(&result_ty, &catch_ty)?;
                    }
                    if let Some(elze) = &value.elze {
                        let elze_ty = self.check_expr(elze).await?;
                        self.require_same(&result_ty, &elze_ty)?;
                    }
                    if let Some(finally) = &value.finally {
                        self.check_expr(finally).await?;
                    }
                    result_ty
                }
                hir::ExprKind::With(context, body) => {
                    self.check_expr(context).await?;
                    self.check_expr(body).await?
                }
                hir::ExprKind::Slice(slice) => {
                    let base_ty = self.check_expr(&slice.base).await?;
                    if let Some(start) = &slice.start {
                        self.check_expr(start).await?;
                    }
                    if let Some(end) = &slice.end {
                        self.check_expr(end).await?;
                    }
                    match base_ty.kind {
                        TyKind::Array(inner, _) => Ty {
                            kind: TyKind::Slice(inner),
                        },
                        TyKind::Slice(inner) => Ty {
                            kind: TyKind::Slice(inner),
                        },
                        _ => return Err(Error::from("slicing requires an array or slice")),
                    }
                }
                hir::ExprKind::Query(_) => {
                    return Err(Error::from("query typing is not implemented"));
                }
                hir::ExprKind::IntrinsicCall(call) => self.check_intrinsic(call).await?,
                hir::ExprKind::FormatString(format) => {
                    for part in &format.parts {
                        if let hir::FormatTemplatePart::Placeholder(placeholder) = part {
                            let _ = placeholder;
                        }
                    }
                    Ty {
                        kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
                    }
                }
            };
            self.results.record_expr_type(expr.hir_id, ty.clone());
            Ok(ty)
        })
    }

    async fn check_block(&mut self, block: &hir::Block) -> Result<Ty> {
        self.locals.push(HashMap::new());
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    let ty = match (&local.ty, &local.init) {
                        (Some(annotation), Some(init)) => {
                            let ty = self.check_type_expr(annotation)?;
                            let init_ty = self.check_expr(init).await?;
                            let resolved_init = if matches!(
                                init.kind,
                                hir::ExprKind::Literal(hir::Lit::Integer(_))
                            ) && matches!(
                                ty.kind,
                                TyKind::Int(_) | TyKind::Uint(_)
                            ) {
                                ty.clone()
                            } else if matches!(
                                init.kind,
                                hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. }
                            ) && matches!(ty.kind, TyKind::Array(_, _))
                            {
                                ty.clone()
                            } else {
                                let mut substitutions = HashMap::new();
                                self.unify_call_types(&init_ty, &ty, &mut substitutions)?;
                                self.substitute_param_map(&init_ty, &substitutions)
                            };
                            self.require_same(&ty, &resolved_init)?;
                            self.results.record_expr_type(init.hir_id, resolved_init);
                            ty
                        }
                        (Some(annotation), None) => self.check_type_expr(annotation)?,
                        (None, Some(init)) => self.check_expr(init).await?,
                        (None, None) => {
                            return Err(Error::from("local binding needs a type or initializer"));
                        }
                    };
                    self.bind_pattern(&local.pat, ty)?;
                }
                hir::StmtKind::Item(item) => self.check_item(item).await?,
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                    self.check_expr(expr).await?;
                }
            }
        }
        let ty = match block.expr.as_ref() {
            Some(expr) => self.check_expr(expr).await?,
            None => self.unit_ty(),
        };
        self.locals.pop();
        Ok(ty)
    }

    async fn check_match_arm(&mut self, arm: &hir::MatchArm, scrutinee_ty: &Ty) -> Result<Ty> {
        self.locals.push(HashMap::new());
        self.bind_pattern(&arm.pat, scrutinee_ty.clone())?;
        if let Some(guard) = &arm.guard {
            let guard_ty = self.check_expr(guard).await?;
            self.require_same(&guard_ty, &Ty::bool())?;
        }
        let result = self.check_expr(&arm.body).await;
        self.locals.pop();
        result
    }

    fn callable_output_args(
        &self,
        callable: &Ty,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Option<Vec<Ty>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return None;
        };
        let output = self.substitute_param_map(&signature.binder.value.output, substitutions);
        let TyKind::Adt(_, args) = output.kind else {
            return None;
        };
        let args = args
            .into_iter()
            .filter_map(|arg| match arg {
                GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .collect::<Vec<_>>();
        (!args.is_empty()).then_some(args)
    }

    fn check_type_expr(&mut self, expr: &hir::TypeExpr) -> Result<Ty> {
        let ty = match &expr.kind {
            hir::TypeExprKind::Primitive(primitive) => primitive_ty(*primitive),
            hir::TypeExprKind::Path(path) => self.path_ty(path)?,
            hir::TypeExprKind::Tuple(items) => Ty {
                kind: TyKind::Tuple(
                    items
                        .iter()
                        .map(|item| self.check_type_expr(item).map(Box::new))
                        .collect::<Result<_>>()?,
                ),
            },
            hir::TypeExprKind::Slice(item) => Ty {
                kind: TyKind::Slice(Box::new(self.check_type_expr(item)?)),
            },
            hir::TypeExprKind::Ptr(item) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.check_type_expr(item)?),
                    mutbl: ty::Mutability::Not,
                }),
            },
            hir::TypeExprKind::Ref(item) => Ty {
                kind: TyKind::Ref(
                    ty::Region::ReErased,
                    Box::new(self.check_type_expr(item)?),
                    ty::Mutability::Not,
                ),
            },
            hir::TypeExprKind::FnPtr(function) => Ty {
                kind: TyKind::FnPtr(ty::PolyFnSig {
                    binder: ty::Binder {
                        value: ty::FnSig {
                            inputs: function
                                .inputs
                                .iter()
                                .map(|input| self.check_type_expr(input).map(Box::new))
                                .collect::<Result<_>>()?,
                            output: Box::new(self.check_type_expr(&function.output)?),
                            c_variadic: false,
                            unsafety: ty::Unsafety::Normal,
                            abi: ty::Abi::Rust,
                        },
                        bound_vars: Vec::new(),
                    },
                }),
            },
            hir::TypeExprKind::Never => Ty::never(),
            hir::TypeExprKind::Array(item, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.check_type_expr(item)?),
                    match length.as_deref() {
                        Some(hir::Expr {
                            kind: hir::ExprKind::Literal(hir::Lit::Integer(value)),
                            ..
                        }) if *value >= 0 => ty::ConstKind::Value(ty::ConstValue::Scalar(
                            ty::Scalar::Int(ty::ScalarInt {
                                data: *value as u128,
                                size: 8,
                            }),
                        )),
                        _ => ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id)),
                    },
                ),
            },
            hir::TypeExprKind::Infer => Ty {
                kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id)),
            },
            hir::TypeExprKind::ConstBlock(body) => {
                self.pending_type_const_blocks
                    .push((expr.hir_id, (**body).clone()));
                Ty {
                    kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id)),
                }
            }
            hir::TypeExprKind::Error => {
                return Err(Error::from("invalid type expression"));
            }
            hir::TypeExprKind::Structural(_) => {
                return Err(Error::from(
                    "structural types are not supported by HIR typing",
                ));
            }
            hir::TypeExprKind::TypeBinaryOp(_) => {
                return Err(Error::from(
                    "type expressions cannot be combined with a type operator",
                ));
            }
        };
        self.results.record_type_expr_type(expr.hir_id, ty.clone());
        Ok(ty)
    }

    fn path_ty(&mut self, path: &hir::Path) -> Result<Ty> {
        if let Some(name) = path.segments.last().map(|segment| segment.name.as_str()) {
            if let Some(primitive) = primitive_path_ty(name) {
                return Ok(primitive);
            }
        }
        if let Some(hir::Res::Local(local)) = path.res {
            return Err(Error::from(format!("local `{local}` is not a type")));
        }
        if matches!(path.res, Some(hir::Res::SelfTy)) {
            let Some(self_ty) = self.self_types.last().cloned() else {
                return Err(Error::from("Self is not available in this type context"));
            };
            return Ok(self_ty);
        }
        let Some(def_id) = (match path.res {
            Some(hir::Res::Def(def_id)) => Some(def_id),
            _ => None,
        }) else {
            return Err(Error::from(format!(
                "unresolved type path `{}`",
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::")
            )));
        };
        if let Some(generic) = self.generic_ty(def_id) {
            return Ok(generic);
        }
        let Some(item) = self.program.def_map.get(&def_id).cloned() else {
            return Err(Error::from(format!(
                "type definition `{def_id}` was not found"
            )));
        };
        let (flags, variants) = match &item.kind {
            hir::ItemKind::Struct(_) => (AdtFlags::IS_STRUCT, Vec::new()),
            hir::ItemKind::Enum(def) => (
                AdtFlags::IS_ENUM,
                def.variants
                    .iter()
                    .enumerate()
                    .map(|(index, variant)| ty::VariantDef {
                        def_id: variant.def_id,
                        ctor_def_id: Some(variant.def_id),
                        ident: variant.name.clone(),
                        discr: ty::VariantDiscr::Relative(index as u32),
                        fields: Vec::new(),
                        ctor_kind: ty::CtorKind::Fn,
                        is_recovered: false,
                    })
                    .collect(),
            ),
            _ => return Err(Error::from(format!("definition `{def_id}` is not a type"))),
        };
        let args = match path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
        {
            Some(args) => args
                .args
                .iter()
                .map(|arg| match arg {
                    hir::GenericArg::Type(ty) => self.check_type_expr(ty).map(GenericArg::Type),
                    hir::GenericArg::Const(_) => {
                        Err(Error::from("const generic arguments are not supported"))
                    }
                })
                .collect::<Result<Vec<_>>>()?,
            None => match &item.kind {
                hir::ItemKind::Struct(def) => def
                    .generics
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| {
                        GenericArg::Type(Ty {
                            kind: TyKind::Param(ty::ParamTy {
                                index: index as u32,
                                name: parameter.name.clone(),
                            }),
                        })
                    })
                    .collect(),
                hir::ItemKind::Enum(def) => def
                    .generics
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| {
                        GenericArg::Type(Ty {
                            kind: TyKind::Param(ty::ParamTy {
                                index: index as u32,
                                name: parameter.name.clone(),
                            }),
                        })
                    })
                    .collect(),
                _ => Vec::new(),
            },
        };
        Ok(Ty {
            kind: TyKind::Adt(
                AdtDef {
                    did: def_id,
                    variants,
                    flags,
                    repr: ReprOptions {
                        int: None,
                        align: None,
                        pack: None,
                        flags: ReprFlags::empty(),
                        field_shuffle_seed: 0,
                    },
                },
                args,
            ),
        })
    }

    fn expr_path_ty(&mut self, path: &hir::Path) -> Result<Ty> {
        if let Some(hir::Res::Local(local)) = path.res {
            if let Some(name) = path.segments.last().map(|segment| &segment.name) {
                for scope in self.locals.iter().rev() {
                    if let Some(ty) = scope.get(name) {
                        return Ok(ty.clone());
                    }
                }
            }
            // HIR locals are resolved by the lowering resolver. Their
            // bindings may be outside this pass's lexical reconstruction
            // (for example generated closure parameters), so preserve the
            // resolved value path and let MIR handle its value semantics.
            let _ = local;
            return Err(Error::from(format!("local `{local}` has no inferred type")));
        }
        let Some(hir::Res::Def(def_id)) = path.res else {
            // A value path can refer to a definition supplied by a loaded
            // crate. It is resolved by HIR lowering, while this pass only
            // needs a semantic value type for subsequent expression checks.
            return Err(Error::from(format!(
                "unresolved value path `{}`",
                path.segments
                    .iter()
                    .map(|segment| segment.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::")
            )));
        };
        let Some(item) = self.program.def_map.get(&def_id).cloned() else {
            let associated = self.program.items.iter().find_map(|item| {
                let hir::ItemKind::Impl(impl_item) = &item.kind else {
                    return None;
                };
                impl_item.items.iter().find_map(|impl_member| {
                    if impl_member.def_id != def_id {
                        return None;
                    }
                    let hir::ImplItemKind::Method(function) = &impl_member.kind else {
                        return None;
                    };
                    Some((
                        impl_item.generics.clone(),
                        impl_item.self_ty.clone(),
                        function.clone(),
                    ))
                })
            });
            if let Some((generics, self_ty, function)) = associated {
                let mut scope = self.generic_scope(&generics);
                let self_ty = scope.check_type_expr(&self_ty)?;
                scope.self_types.push(self_ty);
                let result = scope.function_signature(&function);
                scope.self_types.pop();
                return result;
            }
            if let Some(context) = &self.typing_context {
                for (_, external, _) in context.env_ctx.hir_definitions() {
                    let associated = external.items.iter().find_map(|item| {
                        let hir::ItemKind::Impl(impl_item) = &item.kind else {
                            return None;
                        };
                        impl_item.items.iter().find_map(|impl_member| {
                            if impl_member.def_id != def_id {
                                return None;
                            }
                            let hir::ImplItemKind::Method(function) = &impl_member.kind else {
                                return None;
                            };
                            Some((
                                impl_item.generics.clone(),
                                impl_item.self_ty.clone(),
                                function.clone(),
                            ))
                        })
                    });
                    if let Some((generics, self_ty, function)) = associated {
                        let mut scope = self.generic_scope(&generics);
                        let self_ty = scope.check_type_expr(&self_ty)?;
                        scope.self_types.push(self_ty);
                        let result = scope.function_signature(&function);
                        scope.self_types.pop();
                        return result;
                    }
                }
            }
            for enum_item in self.program.items.clone() {
                let hir::ItemKind::Enum(enum_def) = &enum_item.kind else {
                    continue;
                };
                let Some(variant) = enum_def
                    .variants
                    .iter()
                    .find(|variant| variant.def_id == def_id)
                else {
                    continue;
                };
                let enum_ty = self.enum_item_ty(&enum_item, path)?;
                if let Some(payload) = &variant.payload {
                    let mut scope = self.generic_scope(&enum_def.generics);
                    let payload_result = scope.check_type_expr(payload);
                    let payload_ty = payload_result?;
                    let inputs = match payload_ty.kind {
                        TyKind::Tuple(fields) => fields,
                        _ => vec![Box::new(payload_ty)],
                    };
                    return Ok(Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: ty::FnSig {
                                    inputs,
                                    output: Box::new(enum_ty),
                                    c_variadic: false,
                                    unsafety: ty::Unsafety::Normal,
                                    abi: ty::Abi::Rust,
                                },
                                bound_vars: Vec::new(),
                            },
                        }),
                    });
                }
                return Ok(enum_ty);
            }
            return Err(Error::from(format!(
                "value definition `{def_id}` was not found"
            )));
        };
        match &item.kind {
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => self.path_ty(path),
            hir::ItemKind::Const(constant)
                if matches!(
                    constant.body.value.kind,
                    hir::ExprKind::Literal(hir::Lit::Integer(_))
                ) =>
            {
                self.check_type_expr(&constant.ty)
            }
            hir::ItemKind::Const(_) => self
                .results
                .const_types
                .get(&def_id)
                .cloned()
                .ok_or_else(|| Error::from("constant type was not recorded")),
            hir::ItemKind::Function(function) => self.function_signature(function),
            _ => Err(Error::from("resolved path is not a value")),
        }
    }

    fn enum_item_ty(&mut self, item: &hir::Item, path: &hir::Path) -> Result<Ty> {
        let hir::ItemKind::Enum(enum_def) = &item.kind else {
            return Err(Error::from("enum path does not resolve to an enum"));
        };
        let variants = enum_def
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| ty::VariantDef {
                def_id: variant.def_id,
                ctor_def_id: Some(variant.def_id),
                ident: variant.name.clone(),
                discr: ty::VariantDiscr::Relative(index as u32),
                fields: Vec::new(),
                ctor_kind: ty::CtorKind::Fn,
                is_recovered: false,
            })
            .collect();
        let args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| {
                args.args
                    .iter()
                    .map(|arg| match arg {
                        hir::GenericArg::Type(ty) => self.check_type_expr(ty).map(GenericArg::Type),
                        hir::GenericArg::Const(_) => {
                            Err(Error::from("const generic arguments are not supported"))
                        }
                    })
                    .collect::<Result<Vec<_>>>()
            })
            .transpose()?;
        let args = match args {
            Some(args) => args,
            None => enum_def
                .generics
                .params
                .iter()
                .enumerate()
                .map(|(index, parameter)| {
                    GenericArg::Type(Ty {
                        kind: TyKind::Param(ty::ParamTy {
                            index: index as u32,
                            name: parameter.name.clone(),
                        }),
                    })
                })
                .collect(),
        };
        Ok(Ty {
            kind: TyKind::Adt(
                AdtDef {
                    did: item.def_id,
                    variants,
                    flags: AdtFlags::IS_ENUM,
                    repr: ReprOptions {
                        int: None,
                        align: None,
                        pack: None,
                        flags: ReprFlags::empty(),
                        field_shuffle_seed: 0,
                    },
                },
                args,
            ),
        })
    }

    fn function_signature(&mut self, function: &hir::Function) -> Result<Ty> {
        let mut scope = self.generic_scope(&function.sig.generics);
        let inputs = function
            .sig
            .inputs
            .iter()
            .map(|input| scope.check_type_expr(&input.ty).map(Box::new))
            .collect::<Result<Vec<_>>>()?;
        let output = Box::new(scope.check_type_expr(&function.sig.output)?);
        Ok(Ty {
            kind: TyKind::FnPtr(ty::PolyFnSig {
                binder: ty::Binder {
                    value: ty::FnSig {
                        inputs,
                        output,
                        c_variadic: false,
                        unsafety: ty::Unsafety::Normal,
                        abi: ty::Abi::Rust,
                    },
                    bound_vars: Vec::new(),
                },
            }),
        })
    }

    fn instantiate_call(
        &self,
        callable: &Ty,
        actuals: &[Ty],
    ) -> Result<Option<(HashMap<ty::ParamTy, Ty>, Ty)>> {
        let TyKind::FnPtr(signature) = &callable.kind else {
            return Ok(None);
        };
        if signature.binder.value.inputs.len() != actuals.len() {
            return Err(Error::from(
                "call argument count does not match function signature",
            ));
        }
        let mut substitutions: HashMap<ty::ParamTy, Ty> = HashMap::new();
        for (expected, actual) in signature.binder.value.inputs.iter().zip(actuals) {
            self.unify_call_types(expected, actual, &mut substitutions)?;
        }
        let output = self.substitute_param_map(&signature.binder.value.output, &substitutions);
        Ok(Some((substitutions, output)))
    }

    fn generic_call_args(
        &self,
        def_id: hir::DefId,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Result<Option<Vec<Ty>>> {
        let Some(item) = self.program.def_map.get(&def_id) else {
            return Ok(None);
        };
        let hir::ItemKind::Function(function) = &item.kind else {
            return Ok(None);
        };
        if function.sig.generics.params.is_empty() {
            return Ok(None);
        }
        let mut args = Vec::with_capacity(function.sig.generics.params.len());
        for (index, parameter) in function.sig.generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                return Err(Error::from(format!(
                    "could not infer generic parameter `{}` for `{def_id}`",
                    parameter.name
                )));
            };
            args.push(argument.clone());
        }
        Ok(Some(args))
    }

    fn unify_call_types(
        &self,
        expected: &Ty,
        actual: &Ty,
        substitutions: &mut HashMap<ty::ParamTy, Ty>,
    ) -> Result<()> {
        match (&expected.kind, &actual.kind) {
            (TyKind::Param(param), _) => {
                if let Some(previous) = substitutions.get(param) {
                    self.require_same(previous, actual)?;
                } else {
                    substitutions.insert(param.clone(), actual.clone());
                }
                Ok(())
            }
            (_, TyKind::Param(param)) => {
                if let Some(previous) = substitutions.get(param) {
                    self.require_same(previous, expected)?;
                } else {
                    substitutions.insert(param.clone(), expected.clone());
                }
                Ok(())
            }
            (TyKind::Ref(_, expected, _), TyKind::Ref(_, actual, _)) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::Ref(_, expected, _), _) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::FnPtr(expected), TyKind::FnPtr(actual))
                if expected.binder.value.inputs.len() == actual.binder.value.inputs.len() =>
            {
                for (expected, actual) in expected
                    .binder
                    .value
                    .inputs
                    .iter()
                    .zip(&actual.binder.value.inputs)
                {
                    self.unify_call_types(expected, actual, substitutions)?;
                }
                self.unify_call_types(
                    &expected.binder.value.output,
                    &actual.binder.value.output,
                    substitutions,
                )
            }
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => {
                expected
                    .iter()
                    .zip(actual)
                    .try_for_each(|(expected, actual)| {
                        self.unify_call_types(expected, actual, substitutions)
                    })
            }
            (TyKind::Array(expected, _), TyKind::Array(actual, _))
            | (TyKind::Slice(expected), TyKind::Slice(actual)) => {
                self.unify_call_types(expected, actual, substitutions)
            }
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    if let (GenericArg::Type(expected), GenericArg::Type(actual)) =
                        (expected, actual)
                    {
                        self.unify_call_types(expected, actual, substitutions)?;
                    }
                }
                Ok(())
            }
            _ => self.require_same(expected, actual),
        }
    }

    fn substitute_param_map(&self, ty: &Ty, substitutions: &HashMap<ty::ParamTy, Ty>) -> Ty {
        match &ty.kind {
            TyKind::Param(param) => match substitutions.get(param) {
                Some(ty) => ty.clone(),
                None => ty.clone(),
            },
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region.clone(),
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    *mutable,
                ),
            },
            TyKind::RawPtr(value) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_param_map(&value.ty, substitutions)),
                    mutbl: value.mutbl,
                }),
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .iter()
                        .map(|field| Box::new(self.substitute_param_map(field, substitutions)))
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(
                    Box::new(self.substitute_param_map(inner, substitutions)),
                    length.clone(),
                ),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_param_map(inner, substitutions))),
            },
            TyKind::Adt(def, args) => Ty {
                kind: TyKind::Adt(
                    def.clone(),
                    args.iter()
                        .map(|arg| match arg {
                            GenericArg::Type(ty) => {
                                GenericArg::Type(self.substitute_param_map(ty, substitutions))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                ),
            },
            _ => ty.clone(),
        }
    }

    fn method_output(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
        actuals: &[Ty],
    ) -> Result<(hir::DefId, Option<Vec<Ty>>, Ty)> {
        let receiver_ty = match &receiver_ty.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver_ty,
        };
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => Some(receiver.did),
            _ => None,
        };
        let mut impl_items = self.program.items.clone();
        if let Some(context) = &self.typing_context {
            impl_items.extend(
                context
                    .env_ctx
                    .hir_definitions()
                    .into_iter()
                    .flat_map(|(_, program, _)| program.items),
            );
        }
        for item in impl_items {
            let hir::ItemKind::Impl(impl_item) = item.kind else {
                continue;
            };
            let mut scope = self.generic_scope(&impl_item.generics);
            let checked_self_ty = scope.check_type_expr(&impl_item.self_ty)?;
            let self_ty = match &checked_self_ty.kind {
                TyKind::Ref(_, inner, _) => inner.as_ref(),
                _ => &checked_self_ty,
            };
            let matches_receiver = match (receiver_def, &receiver_ty.kind, &self_ty.kind) {
                (Some(receiver_def), _, TyKind::Adt(impl_receiver, _)) => {
                    impl_receiver.did == receiver_def
                }
                (None, TyKind::Adt(_, _), _) => false,
                (None, _, _) => self_ty == receiver_ty,
                (Some(_), _, _) => false,
            };
            if !matches_receiver {
                continue;
            }
            scope.self_types.push(checked_self_ty);
            let impl_generics = impl_item.generics.clone();
            for impl_item in impl_item.items {
                let hir::ImplItemKind::Method(function) = impl_item.kind else {
                    continue;
                };
                if impl_item.name == *method {
                    let signature = scope.function_signature(&function)?;
                    let Some((substitutions, result)) =
                        scope.instantiate_call(&signature, actuals)?
                    else {
                        return Err(Error::from("method arguments do not match its signature"));
                    };
                    let args = scope.method_generic_args(
                        &impl_generics,
                        &function.sig.generics,
                        &substitutions,
                    )?;
                    scope.self_types.pop();
                    return Ok((impl_item.def_id, args, result));
                }
            }
            scope.self_types.pop();
        }
        Err(Error::from(format!("method `{method}` was not found")))
    }

    fn method_generic_args(
        &self,
        impl_generics: &hir::Generics,
        method_generics: &hir::Generics,
        substitutions: &HashMap<ty::ParamTy, Ty>,
    ) -> Result<Option<Vec<Ty>>> {
        if impl_generics.params.is_empty() && method_generics.params.is_empty() {
            return Ok(None);
        }
        let mut args = Vec::new();
        for (index, parameter) in impl_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                return Err(Error::from(format!(
                    "could not infer generic parameter `{}` in impl method",
                    parameter.name
                )));
            };
            args.push(argument.clone());
        }
        for (index, parameter) in method_generics.params.iter().enumerate() {
            let param = ty::ParamTy {
                index: index as u32,
                name: parameter.name.clone(),
            };
            let Some(argument) = substitutions.get(&param) else {
                return Err(Error::from(format!(
                    "could not infer generic parameter `{}` in method",
                    parameter.name
                )));
            };
            args.push(argument.clone());
        }
        Ok(Some(args))
    }

    fn bind_pattern(&mut self, pattern: &hir::Pat, ty: Ty) -> Result<()> {
        self.results.record_pat_type(pattern.hir_id, ty.clone());
        match &pattern.kind {
            hir::PatKind::Binding { name, .. } => {
                let Some(scope) = self.locals.last_mut() else {
                    return Err(Error::from("no local scope is active"));
                };
                scope.insert(name.clone(), ty);
            }
            hir::PatKind::Wild => {}
            hir::PatKind::Lit(lit) => {
                let integer_literal = matches!(lit, hir::Lit::Integer(_));
                let integer_ty = matches!(ty.kind, TyKind::Int(_) | TyKind::Uint(_));
                if !(integer_literal && integer_ty) {
                    self.require_same(&ty, &self.literal_ty(lit))?;
                }
            }
            hir::PatKind::Tuple(patterns) => {
                let TyKind::Tuple(fields) = ty.kind else {
                    return Err(Error::from("tuple pattern requires a tuple scrutinee"));
                };
                if patterns.len() != fields.len() {
                    return Err(Error::from("tuple pattern arity does not match scrutinee"));
                }
                for (pattern, field) in patterns.iter().zip(fields) {
                    self.bind_pattern(pattern, *field)?;
                }
            }
            hir::PatKind::Struct(path, fields, _) => {
                if self.enum_variant_ty(path)?.is_some() {
                    let (_, payloads) = self.variant_payload_types(path, &ty)?;
                    let [payload] = payloads.as_slice() else {
                        return Err(Error::from(
                            "struct enum pattern requires exactly one payload type",
                        ));
                    };
                    for field in fields {
                        let field_ty = self.field_ty(payload, &field.name)?;
                        self.bind_pattern(&field.pat, field_ty)?;
                    }
                } else {
                    let struct_ty = if path.segments.is_empty() {
                        ty.clone()
                    } else {
                        self.path_ty(path)?
                    };
                    self.require_same_adt(&ty, &struct_ty, "struct pattern")?;
                    for field in fields {
                        let field_ty = self.field_ty(&struct_ty, &field.name)?;
                        self.bind_pattern(&field.pat, field_ty)?;
                    }
                }
            }
            hir::PatKind::TupleStruct(path, patterns) => {
                let (_, payloads) = self.variant_payload_types(path, &ty)?;
                if patterns.len() != payloads.len() {
                    return Err(Error::from(
                        "tuple struct pattern arity does not match variant",
                    ));
                }
                for (pattern, payload) in patterns.iter().zip(payloads) {
                    self.bind_pattern(pattern, payload)?;
                }
            }
            hir::PatKind::Variant(path) => {
                let (_, payloads) = self.variant_payload_types(path, &ty)?;
                if !payloads.is_empty() {
                    return Err(Error::from(
                        "payload variant requires a tuple or struct pattern",
                    ));
                }
            }
        }
        Ok(())
    }

    fn field_ty(&mut self, receiver: &Ty, field: &hir::Symbol) -> Result<Ty> {
        let receiver = match &receiver.kind {
            TyKind::Ref(_, inner, _) => inner.as_ref(),
            _ => receiver,
        };
        let TyKind::Adt(adt, args) = &receiver.kind else {
            return Err(Error::from(format!(
                "field access `{field}` requires a struct, found {:?}",
                receiver.kind
            )));
        };
        let Some(item) = self.program.def_map.get(&adt.did).cloned() else {
            return Err(Error::from("struct definition was not found"));
        };
        let hir::ItemKind::Struct(def) = item.kind else {
            return Err(Error::from("field access requires a struct"));
        };
        let Some(field_def) = def.fields.iter().find(|candidate| candidate.name == *field) else {
            return Err(Error::from(format!("field `{field}` was not found")));
        };
        let mut scope = self.generic_scope(&def.generics);
        let result = scope.check_type_expr(&field_def.ty);
        let ty = result?;
        let substituted = scope.substitute_params(ty, args);
        drop(scope);
        Ok(substituted)
    }

    fn variant_payload_types(&mut self, path: &hir::Path, scrutinee: &Ty) -> Result<(Ty, Vec<Ty>)> {
        let Some(hir::Res::Def(variant_id)) = path.res else {
            return Err(Error::from("variant pattern is unresolved"));
        };
        if let Some((item, variant)) = self.enum_variant_by_def_id(variant_id) {
            let hir::ItemKind::Enum(def) = &item.kind else {
                unreachable!("enum_variant_by_def_id only returns enum variants");
            };
            // The variant path carries the constructor identity, not the
            // instantiated enum arguments. The scrutinee is the authoritative
            // enum type for generic variants such as `Option<T>::Some`.
            let enum_ty = scrutinee.clone();
            let matches_enum = matches!(
                &scrutinee.kind,
                TyKind::Adt(adt, _) if adt.did == item.def_id
            );
            if !matches_enum {
                let scrutinee_def = match &scrutinee.kind {
                    TyKind::Adt(adt, _) => format!("{}", adt.did),
                    _ => format!("<non-adt {:?}>", scrutinee.kind),
                };
                return Err(Error::from(format!(
                    "variant pattern does not match scrutinee type (variant={}, owner={}, scrutinee={})",
                    variant_id, item.def_id, scrutinee_def
                )));
            }
            let scrutinee_args = match &scrutinee.kind {
                TyKind::Adt(_, args) => args,
                _ => unreachable!("variant pattern ADT was checked above"),
            };
            let Some(payload) = &variant.payload else {
                return Ok((enum_ty, Vec::new()));
            };
            let mut scope = self.generic_scope(&def.generics);
            let payload_result = scope.check_type_expr(payload);
            let payload = payload_result?;
            let payload = scope.substitute_params(payload, scrutinee_args);
            drop(scope);
            let payloads = match payload.kind {
                TyKind::Tuple(fields) => fields.into_iter().map(|field| *field).collect(),
                _ => vec![payload],
            };
            return Ok((enum_ty, payloads));
        }
        Err(Error::from("variant definition was not found"))
    }

    fn enum_struct_payload_type(&mut self, path: &hir::Path, scrutinee: &Ty) -> Result<Option<Ty>> {
        if self.enum_variant_ty(path)?.is_none() {
            return Ok(None);
        }
        let (_, payloads) = self.variant_payload_types(path, scrutinee)?;
        let Some(payload) = payloads.into_iter().next() else {
            return Ok(None);
        };
        let TyKind::Adt(adt, _) = &payload.kind else {
            return Ok(None);
        };
        if matches!(
            self.program.def_map.get(&adt.did).map(|item| &item.kind),
            Some(hir::ItemKind::Struct(_))
        ) {
            Ok(Some(payload))
        } else {
            Ok(None)
        }
    }

    fn enum_variant_ty(&mut self, path: &hir::Path) -> Result<Option<Ty>> {
        let Some(hir::Res::Def(variant_id)) = path.res else {
            return Ok(None);
        };
        let Some((item, _)) = self.enum_variant_by_def_id(variant_id) else {
            return Ok(None);
        };
        Ok(Some(self.enum_item_ty(&item, path)?))
    }

    fn enum_variant_by_def_id(
        &self,
        variant_id: hir::DefId,
    ) -> Option<(hir::Item, hir::EnumVariant)> {
        self.program.items.iter().find_map(|item| {
            let hir::ItemKind::Enum(def) = &item.kind else {
                return None;
            };
            def.variants
                .iter()
                .find(|variant| variant.def_id == variant_id)
                .cloned()
                .map(|variant| (item.clone(), variant))
        })
    }

    fn substitute_params(&self, ty: Ty, args: &[GenericArg]) -> Ty {
        match ty.kind {
            TyKind::Param(param) => match args.get(param.index as usize) {
                Some(GenericArg::Type(ty)) => ty.clone(),
                Some(GenericArg::Const(_) | GenericArg::Lifetime(_)) | None => Ty {
                    kind: TyKind::Param(param),
                },
            },
            TyKind::Tuple(fields) => Ty {
                kind: TyKind::Tuple(
                    fields
                        .into_iter()
                        .map(|field| Box::new(self.substitute_params(*field, args)))
                        .collect(),
                ),
            },
            TyKind::Array(inner, length) => Ty {
                kind: TyKind::Array(Box::new(self.substitute_params(*inner, args)), length),
            },
            TyKind::Slice(inner) => Ty {
                kind: TyKind::Slice(Box::new(self.substitute_params(*inner, args))),
            },
            TyKind::Ref(region, inner, mutable) => Ty {
                kind: TyKind::Ref(
                    region,
                    Box::new(self.substitute_params(*inner, args)),
                    mutable,
                ),
            },
            TyKind::RawPtr(mutability) => Ty {
                kind: TyKind::RawPtr(ty::TypeAndMut {
                    ty: Box::new(self.substitute_params(*mutability.ty, args)),
                    mutbl: mutability.mutbl,
                }),
            },
            kind => Ty { kind },
        }
    }

    async fn check_intrinsic(&mut self, call: &hir::IntrinsicCallExpr) -> Result<Ty> {
        let mut arg_types = Vec::with_capacity(call.callargs.len());
        for arg in &call.callargs {
            arg_types.push(self.check_expr(&arg.value).await?);
        }
        use fp_core::intrinsics::IntrinsicKind;
        Ok(match call.kind {
            IntrinsicKind::Print | IntrinsicKind::Println | IntrinsicKind::Panic => Ty {
                kind: TyKind::Tuple(Vec::new()),
            },
            IntrinsicKind::Format => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::Len => Ty::uint(ty::UintTy::U64),
            IntrinsicKind::Slice => {
                let Some(base) = arg_types.first() else {
                    return Err(Error::from("slice intrinsic requires a base expression"));
                };
                match &base.kind {
                    TyKind::Array(inner, _) | TyKind::Slice(inner) => Ty {
                        kind: TyKind::Slice(inner.clone()),
                    },
                    _ => {
                        return Err(Error::from(
                            "slice intrinsic base must be an array or slice",
                        ));
                    }
                }
            }
            IntrinsicKind::DebugAssertions
            | IntrinsicKind::FsExists
            | IntrinsicKind::FsIsDir
            | IntrinsicKind::FsIsFile
            | IntrinsicKind::EnvVarExists
            | IntrinsicKind::HasField
            | IntrinsicKind::HasMethod => Ty::bool(),
            IntrinsicKind::Input
            | IntrinsicKind::FsReadToString
            | IntrinsicKind::FsReadDir
            | IntrinsicKind::FsWalkDir
            | IntrinsicKind::FsGlob
            | IntrinsicKind::EnvCurrentDir
            | IntrinsicKind::EnvTempDir
            | IntrinsicKind::EnvHomeDir
            | IntrinsicKind::EnvVar
            | IntrinsicKind::PathJoin
            | IntrinsicKind::PathParent
            | IntrinsicKind::PathFileName
            | IntrinsicKind::PathExtension
            | IntrinsicKind::PathStem
            | IntrinsicKind::PathNormalize
            | IntrinsicKind::IoReadStdinToString
            | IntrinsicKind::YamlToJson
            | IntrinsicKind::JsonParse
            | IntrinsicKind::ProcMacroTokenStreamToString => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::PathIsAbsolute => Ty::bool(),
            IntrinsicKind::TimeNow => Ty::float(ty::FloatTy::F64),
            IntrinsicKind::CatchUnwind => Ty::bool(),
            IntrinsicKind::CatchUnwindResult => {
                let Some(value) = arg_types.first().cloned() else {
                    return Err(Error::from(
                        "catch_unwind_result requires a callable argument",
                    ));
                };
                Ty {
                    kind: TyKind::Tuple(vec![Box::new(Ty::bool()), Box::new(value)]),
                }
            }
            IntrinsicKind::Spawn | IntrinsicKind::Select => {
                let Some(value) = arg_types.first() else {
                    return Err(Error::from(format!(
                        "{:?} intrinsic requires an argument",
                        call.kind
                    )));
                };
                value.clone()
            }
            IntrinsicKind::Join => {
                if arg_types.len() == 1 {
                    arg_types[0].clone()
                } else if arg_types.is_empty() {
                    return Err(Error::from("join intrinsic requires an argument"));
                } else {
                    Ty {
                        kind: TyKind::Tuple(arg_types.into_iter().map(Box::new).collect()),
                    }
                }
            }
            IntrinsicKind::SizeOf | IntrinsicKind::FieldCount | IntrinsicKind::MethodCount => {
                Ty::uint(ty::UintTy::U64)
            }
            IntrinsicKind::FieldNameAt
            | IntrinsicKind::TypeName
            | IntrinsicKind::ProcMacroTokenStreamFromStr => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            IntrinsicKind::FieldType | IntrinsicKind::VecType => {
                return Err(Error::from(
                    "type-valued intrinsic has no HIR type representation",
                ));
            }
            IntrinsicKind::TypeOf
            | IntrinsicKind::CreateStruct
            | IntrinsicKind::AddField
            | IntrinsicKind::BuildType => {
                return Err(Error::from(
                    "type-valued intrinsic typing is not implemented",
                ));
            }
            IntrinsicKind::FsWriteString
            | IntrinsicKind::FsAppendString
            | IntrinsicKind::FsCreateDirAll
            | IntrinsicKind::FsRemoveFile
            | IntrinsicKind::FsRemoveDirAll
            | IntrinsicKind::IoWriteStdout
            | IntrinsicKind::IoWriteStderr
            | IntrinsicKind::TestCommandMockReset
            | IntrinsicKind::TestCommandMockPush
            | IntrinsicKind::TestCommandMockApply
            | IntrinsicKind::Sleep
            | IntrinsicKind::Yield
            | IntrinsicKind::CompileWarning => self.unit_ty(),
            IntrinsicKind::CompileError => {
                return Err(Error::from("compile_error intrinsic requested an error"));
            }
            _ => {
                return Err(Error::from(format!(
                    "intrinsic `{:?}` has no HIR type rule",
                    call.kind
                )));
            }
        })
    }

    fn require_same(&self, lhs: &Ty, rhs: &Ty) -> Result<()> {
        if lhs == rhs || matches!(lhs.kind, TyKind::Never) || matches!(rhs.kind, TyKind::Never) {
            Ok(())
        } else {
            Err(Error::from(format!("HIR type mismatch: {lhs} and {rhs}")))
        }
    }

    fn require_same_adt(&self, actual: &Ty, expected: &Ty, context: &str) -> Result<()> {
        let (TyKind::Adt(actual_def, actual_args), TyKind::Adt(expected_def, expected_args)) =
            (&actual.kind, &expected.kind)
        else {
            return Err(Error::from(format!("{context} requires an ADT scrutinee")));
        };
        if actual_def.did != expected_def.did || actual_args.len() != expected_args.len() {
            return Err(Error::from(format!(
                "{context} does not match scrutinee type"
            )));
        }
        let mut substitutions = HashMap::new();
        for (actual, expected) in actual_args.iter().zip(expected_args) {
            match (actual, expected) {
                (GenericArg::Type(actual), GenericArg::Type(expected)) => {
                    self.unify_call_types(expected, actual, &mut substitutions)?;
                }
                (actual, expected) if actual == expected => {}
                _ => {
                    return Err(Error::from(format!(
                        "{context} does not match scrutinee type"
                    )));
                }
            }
        }
        Ok(())
    }

    fn unit_ty(&self) -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    fn literal_ty(&self, literal: &hir::Lit) -> Ty {
        match literal {
            hir::Lit::Bool(_) => Ty::bool(),
            hir::Lit::Char(_) => Ty::char(),
            hir::Lit::Integer(_) => Ty::int(ty::IntTy::I64),
            hir::Lit::Float(_) => Ty::float(ty::FloatTy::F64),
            hir::Lit::Str(_) => Ty {
                kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
            },
            hir::Lit::Null => Ty::never(),
        }
    }
}

fn primitive_path_ty(name: &str) -> Option<Ty> {
    Some(match name {
        "bool" => Ty::bool(),
        "char" => Ty::char(),
        "i8" => Ty::int(ty::IntTy::I8),
        "i16" => Ty::int(ty::IntTy::I16),
        "i32" => Ty::int(ty::IntTy::I32),
        "i64" => Ty::int(ty::IntTy::I64),
        "i128" => Ty::int(ty::IntTy::I128),
        "isize" => Ty::int(ty::IntTy::Isize),
        "u8" => Ty::uint(ty::UintTy::U8),
        "u16" => Ty::uint(ty::UintTy::U16),
        "u32" => Ty::uint(ty::UintTy::U32),
        "u64" => Ty::uint(ty::UintTy::U64),
        "u128" => Ty::uint(ty::UintTy::U128),
        "usize" => Ty::uint(ty::UintTy::Usize),
        "f32" => Ty::float(ty::FloatTy::F32),
        "f64" => Ty::float(ty::FloatTy::F64),
        "str" => Ty {
            kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
        },
        _ => return None,
    })
}

fn primitive_ty(primitive: TypePrimitive) -> Ty {
    match primitive {
        TypePrimitive::Bool => Ty::bool(),
        TypePrimitive::Char => Ty::char(),
        TypePrimitive::Int(int) => match int {
            TypeInt::I8 => Ty::int(ty::IntTy::I8),
            TypeInt::I16 => Ty::int(ty::IntTy::I16),
            TypeInt::I32 => Ty::int(ty::IntTy::I32),
            TypeInt::I64 => Ty::int(ty::IntTy::I64),
            TypeInt::I128 => Ty::int(ty::IntTy::I128),
            TypeInt::U8 => Ty::uint(ty::UintTy::U8),
            TypeInt::U16 => Ty::uint(ty::UintTy::U16),
            TypeInt::U32 => Ty::uint(ty::UintTy::U32),
            TypeInt::U64 => Ty::uint(ty::UintTy::U64),
            TypeInt::U128 => Ty::uint(ty::UintTy::U128),
            TypeInt::BigInt => Ty::int(ty::IntTy::I128),
        },
        TypePrimitive::Decimal(decimal) => Ty::float(match decimal {
            DecimalType::F32 => ty::FloatTy::F32,
            _ => ty::FloatTy::F64,
        }),
        TypePrimitive::String => Ty {
            kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))),
        },
        TypePrimitive::List => Ty {
            kind: TyKind::Slice(Box::new(Ty::never())),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_literal_type_by_hir_id() {
        let expr = hir::Expr {
            hir_id: 7,
            kind: hir::ExprKind::Literal(hir::Lit::Integer(4)),
            span: fp_core::span::Span::null(),
        };
        let mut program = hir::Program::new();
        program.items.push(hir::Item {
            hir_id: 1,
            def_id: hir::DefId::local(1),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Expr(expr),
            span: fp_core::span::Span::null(),
        });

        let (_, results) =
            crate::block_on(HirTypeChecker::new(program).check()).expect("HIR type check");
        assert_eq!(results.expr_types.get(&7), Some(&Ty::int(ty::IntTy::I64)));
    }

    #[test]
    fn records_binding_pattern_type() {
        let pattern = hir::Pat {
            hir_id: 8,
            kind: hir::PatKind::Binding {
                name: "value".into(),
                mutable: false,
            },
        };
        let expr = hir::Expr {
            hir_id: 9,
            kind: hir::ExprKind::Let(
                pattern,
                Box::new(hir::TypeExpr {
                    hir_id: 10,
                    kind: hir::TypeExprKind::Primitive(TypePrimitive::Int(TypeInt::I64)),
                    span: fp_core::span::Span::null(),
                }),
                None,
            ),
            span: fp_core::span::Span::null(),
        };
        let mut program = hir::Program::new();
        program.items.push(hir::Item {
            hir_id: 1,
            def_id: hir::DefId::local(1),
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Expr(expr),
            span: fp_core::span::Span::null(),
        });

        let (_, results) =
            crate::block_on(HirTypeChecker::new(program).check()).expect("HIR type check");
        assert_eq!(results.pat_types.get(&8), Some(&Ty::int(ty::IntTy::I64)));
    }
}
