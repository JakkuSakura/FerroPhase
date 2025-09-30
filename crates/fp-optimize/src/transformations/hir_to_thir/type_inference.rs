use super::*;
use std::collections::HashMap;

pub(super) struct TypeInferencer<'a> {
    generator: &'a mut ThirGenerator,
    type_vars: Vec<TypeVar>,
    expr_types: HashMap<hir::HirId, TypeVarId>,
    pattern_types: HashMap<hir::HirId, TypeVarId>,
    binding_vars: HashMap<hir::HirId, TypeVarId>,
    env: HashMap<hir::HirId, TypeScheme>,
    scope_stack: Vec<Vec<hir::HirId>>,
    current_level: usize,
    current_self_ty: Option<hir_types::Ty>,
}

type TypeVarId = usize;

#[derive(Clone)]
struct TypeVar {
    kind: TypeVarKind,
}

#[derive(Clone)]
enum TypeVarKind {
    Unbound { level: usize },
    Link(TypeVarId),
    Bound(TypeTerm),
}

#[derive(Clone, Debug)]
enum TypeTerm {
    Bool,
    Int(Option<hir_types::IntTy>),
    Uint(Option<hir_types::UintTy>),
    Float(Option<hir_types::FloatTy>),
    String,
    Unit,
    Never,
    Custom(hir_types::Ty),
    Tuple(Vec<TypeVarId>),
    Function(Vec<TypeVarId>, TypeVarId),
}

#[derive(Clone)]
struct TypeScheme {
    vars: usize,
    body: SchemeType,
}

#[derive(Clone)]
enum SchemeType {
    Var(u32),
    Bool,
    Int(Option<hir_types::IntTy>),
    Uint(Option<hir_types::UintTy>),
    Float(Option<hir_types::FloatTy>),
    String,
    Unit,
    Never,
    Custom(hir_types::Ty),
    Tuple(Vec<SchemeType>),
    Function(Vec<SchemeType>, Box<SchemeType>),
}

impl<'a> TypeInferencer<'a> {
    pub fn new(generator: &'a mut ThirGenerator, current_self_ty: Option<hir_types::Ty>) -> Self {
        Self {
            generator,
            type_vars: Vec::new(),
            expr_types: HashMap::new(),
            pattern_types: HashMap::new(),
            binding_vars: HashMap::new(),
            env: HashMap::new(),
            scope_stack: Vec::new(),
            current_level: 0,
            current_self_ty,
        }
    }

    pub fn infer_function(&mut self, body: &hir::Body) -> Result<()> {
        self.scope_stack.push(Vec::new());
        self.current_level += 1;

        for param in &body.params {
            let pattern_ty = self.infer_pattern(&param.pat)?;
            let annotated = self.generator.hir_ty_to_ty(&param.ty)?;
            let annotated_var = self.type_from_hir_ty(&annotated)?;
            self.unify(pattern_ty, annotated_var)?;
            // Introduce bindings for their side effects on the type environment
            let _ = self.introduce_pattern_bindings(&param.pat)?;
        }

        // Infer body expression type for side effects on the type environment
        let _ = self.infer_expr(&body.value)?;

        self.generalize_record_results()?;

        if let Some(ids) = self.scope_stack.pop() {
            for id in ids {
                self.env.remove(&id);
            }
        }
        self.current_level -= 1;
        Ok(())
    }

    fn infer_block(&mut self, block: &hir::Block) -> Result<TypeVarId> {
        self.scope_stack.push(Vec::new());
        self.current_level += 1;

        for stmt in &block.stmts {
            self.infer_stmt(stmt)?;
        }

        let result = if let Some(expr) = &block.expr {
            self.infer_expr(expr)?
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, TypeTerm::Unit)?;
            unit
        };

        if let Some(ids) = self.scope_stack.pop() {
            for id in ids {
                self.env.remove(&id);
            }
        }
        self.current_level -= 1;
        Ok(result)
    }

    fn infer_stmt(&mut self, stmt: &hir::Stmt) -> Result<()> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                let init_var = if let Some(init) = &local.init {
                    Some(self.infer_expr(init)?)
                } else {
                    None
                };

                let pat_var = self.infer_pattern(&local.pat)?;
                let has_annotation = local.ty.is_some();
                if let Some(ty_expr) = &local.ty {
                    let annotated = self.generator.hir_ty_to_ty(ty_expr)?;
                    let annotated_var = self.type_from_hir_ty(&annotated)?;
                    self.unify(pat_var, annotated_var)?;
                }
                if let Some(init_var) = init_var {
                    if !has_annotation {
                        self.unify(pat_var, init_var)?;
                    }
                }

                // Introduce bindings for their side effects on the type environment
                let _ = self.introduce_pattern_bindings(&local.pat)?;
            }
            hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                // Infer expression type for side effects on the type environment
                let _ = self.infer_expr(expr)?;
            }
            hir::StmtKind::Item(_) => {}
        }
        Ok(())
    }

    fn infer_expr(&mut self, expr: &hir::Expr) -> Result<TypeVarId> {
        use hir::ExprKind;
        let ty = match &expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit)?,
            ExprKind::Path(path) => self.infer_path(expr.hir_id, path)?,
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_ty = self.infer_expr(lhs)?;
                let rhs_ty = self.infer_expr(rhs)?;
                match op {
                    hir::BinOp::Add
                    | hir::BinOp::Sub
                    | hir::BinOp::Mul
                    | hir::BinOp::Div
                    | hir::BinOp::Rem => {
                        self.unify(lhs_ty, rhs_ty)?;
                        self.ensure_numeric(lhs_ty, "arithmetic operation")?;
                        lhs_ty
                    }
                    hir::BinOp::BitAnd
                    | hir::BinOp::BitOr
                    | hir::BinOp::BitXor
                    | hir::BinOp::Shl
                    | hir::BinOp::Shr => {
                        self.unify(lhs_ty, rhs_ty)?;
                        self.ensure_integer(lhs_ty, "bitwise operation")?;
                        lhs_ty
                    }
                    hir::BinOp::Eq
                    | hir::BinOp::Ne
                    | hir::BinOp::Lt
                    | hir::BinOp::Le
                    | hir::BinOp::Gt
                    | hir::BinOp::Ge => {
                        self.unify(lhs_ty, rhs_ty)?;
                        let bool_ty = self.fresh_type_var();
                        self.bind(bool_ty, TypeTerm::Bool)?;
                        bool_ty
                    }
                    hir::BinOp::And | hir::BinOp::Or => {
                        let bool_ty = self.fresh_type_var();
                        self.bind(bool_ty, TypeTerm::Bool)?;
                        self.unify(lhs_ty, bool_ty)?;
                        self.unify(rhs_ty, bool_ty)?;
                        bool_ty
                    }
                }
            }
            ExprKind::Unary(op, value) => {
                let value_ty = self.infer_expr(value)?;
                match op {
                    hir::UnOp::Neg => {
                        let numeric = self.fresh_type_var();
                        self.bind(numeric, TypeTerm::Int(None))?;
                        self.unify(value_ty, numeric)?;
                        value_ty
                    }
                    hir::UnOp::Not => {
                        let bool_ty = self.fresh_type_var();
                        self.bind(bool_ty, TypeTerm::Bool)?;
                        self.unify(value_ty, bool_ty)?;
                        bool_ty
                    }
                    hir::UnOp::Deref => {
                        let resolved = self.resolve_to_hir(value_ty)?;
                        match &resolved.kind {
                            hir_types::TyKind::Ref(_, inner, _) => {
                                let expected_ref = self.type_from_hir_ty(&resolved)?;
                                self.unify(value_ty, expected_ref)?;
                                self.type_from_hir_ty(inner.as_ref())?
                            }
                            hir_types::TyKind::RawPtr(ptr) => {
                                let expected_ptr = self.type_from_hir_ty(&resolved)?;
                                self.unify(value_ty, expected_ptr)?;
                                self.type_from_hir_ty(ptr.ty.as_ref())?
                            }
                            _ => {
                                return Err(crate::error::optimization_error(format!(
                                    "Cannot dereference value of type {:?}",
                                    resolved
                                )));
                            }
                        }
                    }
                }
            }
            ExprKind::Assign(lhs, rhs) => {
                let lhs_ty = self.infer_expr(lhs)?;
                let rhs_ty = self.infer_expr(rhs)?;
                self.unify(lhs_ty, rhs_ty)?;
                let unit = self.fresh_type_var();
                self.bind(unit, TypeTerm::Unit)?;
                unit
            }
            ExprKind::Call(callee, args) => {
                let _callee_ty = self.infer_expr(callee)?;
                if let hir::ExprKind::Path(path) = &callee.kind {
                    if let Some(hir::Res::Def(def_id)) = path.res {
                        let function_sig = self
                            .generator
                            .type_context
                            .lookup_function_signature(def_id)
                            .cloned();

                        if let Some(sig) = function_sig {
                            for (arg_expr, expected_ty) in args.iter().zip(sig.inputs.iter()) {
                                let arg_ty = self.infer_expr(arg_expr)?;
                                let expected_var = self.type_from_hir_ty(expected_ty.as_ref())?;
                                self.unify(arg_ty, expected_var)?;
                            }
                            return self.type_from_hir_ty(sig.output.as_ref());
                        }
                    }

                    if let Some(method_sig) = self.lookup_associated_method_signature(path)? {
                        for (arg_expr, expected_ty) in args.iter().zip(method_sig.inputs.iter()) {
                            let arg_ty = self.infer_expr(arg_expr)?;
                            let expected = self.type_from_hir_ty(expected_ty.as_ref())?;
                            self.unify(arg_ty, expected)?;
                        }
                        return self.type_from_hir_ty(method_sig.output.as_ref());
                    }
                }
                self.infer_call_args(args)?
            }
            ExprKind::MethodCall(receiver, method, args) => {
                let receiver_ty = self.infer_expr(receiver)?;
                let resolved_receiver_ty = self.resolve_to_hir(receiver_ty)?;
                if let Some(sig) = self
                    .generator
                    .type_context
                    .lookup_method_signature(&resolved_receiver_ty, method)
                    .cloned()
                {
                    if let Some(self_param) = sig.inputs.first() {
                        let expected_self_ty = self_param.as_ref();
                        match (&expected_self_ty.kind, &resolved_receiver_ty.kind) {
                            (hir_types::TyKind::Ref(_, _, _), hir_types::TyKind::Ref(_, _, _))
                            | (hir_types::TyKind::RawPtr(_), hir_types::TyKind::RawPtr(_)) => {
                                let expected_self = self.type_from_hir_ty(expected_self_ty)?;
                                self.unify(receiver_ty, expected_self)?;
                            }
                            (hir_types::TyKind::Ref(_, inner, _), _) => {
                                let inner_var = self.type_from_hir_ty(inner.as_ref())?;
                                self.unify(receiver_ty, inner_var)?;
                            }
                            (hir_types::TyKind::RawPtr(type_and_mut), _) => {
                                let inner_var = self.type_from_hir_ty(type_and_mut.ty.as_ref())?;
                                self.unify(receiver_ty, inner_var)?;
                            }
                            _ => {
                                let expected_self = self.type_from_hir_ty(expected_self_ty)?;
                                self.unify(receiver_ty, expected_self)?;
                            }
                        }
                    }
                    for (arg_expr, expected_ty) in args.iter().zip(sig.inputs.iter().skip(1)) {
                        let arg_ty = self.infer_expr(arg_expr)?;
                        let resolved_arg_ty = self.resolve_to_hir(arg_ty)?;
                        let expected_hir_ty = expected_ty.as_ref();
                        match (&expected_hir_ty.kind, &resolved_arg_ty.kind) {
                            (hir_types::TyKind::Ref(_, _, _), hir_types::TyKind::Ref(_, _, _))
                            | (hir_types::TyKind::RawPtr(_), hir_types::TyKind::RawPtr(_)) => {
                                let expected = self.type_from_hir_ty(expected_hir_ty)?;
                                self.unify(arg_ty, expected)?;
                            }
                            (hir_types::TyKind::Ref(_, inner, _), _) => {
                                let expected = self.type_from_hir_ty(inner.as_ref())?;
                                self.unify(arg_ty, expected)?;
                            }
                            (hir_types::TyKind::RawPtr(type_and_mut), _) => {
                                let expected = self.type_from_hir_ty(type_and_mut.ty.as_ref())?;
                                self.unify(arg_ty, expected)?;
                            }
                            _ => {
                                let expected = self.type_from_hir_ty(expected_hir_ty)?;
                                self.unify(arg_ty, expected)?;
                            }
                        }
                    }
                    self.type_from_hir_ty(sig.output.as_ref())?
                } else {
                    return Err(crate::error::optimization_error(format!(
                        "Unknown method `{}` for type {:?}",
                        method, &resolved_receiver_ty
                    )));
                }
            }
            ExprKind::FieldAccess(owner, field_name) => {
                let owner_ty_var = self.infer_expr(owner)?;
                let resolved_owner_ty = self.resolve_to_hir(owner_ty_var)?;
                if let Some((_idx, field_ty)) = self
                    .generator
                    .type_context
                    .lookup_field_info(&resolved_owner_ty, field_name)
                {
                    let expected_owner = self.type_from_hir_ty(&resolved_owner_ty)?;
                    self.unify(owner_ty_var, expected_owner)?;
                    self.type_from_hir_ty(&field_ty)?
                } else {
                    // Fall back to a fresh type variable when field metadata is unavailable.
                    // Detyped HIR may omit structural layouts, so we remain permissive here.
                    let fallback = self.fresh_type_var();
                    fallback
                }
            }
            ExprKind::Struct(path, fields) => {
                let struct_ty = if matches!(path.res, Some(hir::Res::SelfTy)) {
                    self.current_self_ty.clone().ok_or_else(|| {
                        crate::error::optimization_error(
                            "`Self` used outside of an impl context".to_string(),
                        )
                    })?
                } else {
                    self.generator.infer_path_type(path)?
                };

                let struct_var = self.type_from_hir_ty(&struct_ty)?;
                for field in fields {
                    let value_ty = self.infer_expr(&field.expr)?;
                    if let Some((_idx, expected_ty)) = self
                        .generator
                        .type_context
                        .lookup_field_info(&struct_ty, field.name.as_str())
                    {
                        let expected = self.type_from_hir_ty(&expected_ty)?;
                        self.unify(value_ty, expected)?;
                    } else {
                        return Err(crate::error::optimization_error(format!(
                            "Unknown field `{}` on type {:?}",
                            field.name.as_str(),
                            &struct_ty
                        )));
                    }
                }
                struct_var
            }
            ExprKind::Block(block) => self.infer_block(block)?,
            ExprKind::IntrinsicCall(call) => {
                use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

                match call.kind {
                    IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                        if let IntrinsicCallPayload::Format { template } = &call.payload {
                            for arg in &template.args {
                                // Infer arg types for side effects on the type environment
                                let _ = self.infer_expr(arg)?;
                            }
                            for kw in &template.kwargs {
                                // Infer kwarg types for side effects on the type environment
                                let _ = self.infer_expr(&kw.value)?;
                            }
                        }
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit)?;
                        unit
                    }
                    IntrinsicCallKind::Len => {
                        if let IntrinsicCallPayload::Args { args } = &call.payload {
                            if let Some(arg) = args.first() {
                                // Infer arg type for side effects on the type environment
                                let _ = self.infer_expr(arg)?;
                            }
                        }
                        let int = self.fresh_type_var();
                        self.bind(int, TypeTerm::Uint(Some(hir_types::UintTy::Usize)))?;
                        int
                    }
                    IntrinsicCallKind::ConstBlock => {
                        if let IntrinsicCallPayload::Args { args } = &call.payload {
                            if let Some(body) = args.first() {
                                return self.infer_expr(body);
                            }
                        }
                        let unit = self.fresh_type_var();
                        self.bind(unit, TypeTerm::Unit)?;
                        unit
                    }
                    IntrinsicCallKind::DebugAssertions => {
                        let bool_ty = self.fresh_type_var();
                        self.bind(bool_ty, TypeTerm::Bool)?;
                        bool_ty
                    }
                    IntrinsicCallKind::Input => {
                        if let IntrinsicCallPayload::Args { args } = &call.payload {
                            for arg in args {
                                // Infer arg types for side effects on the type environment
                                let _ = self.infer_expr(arg)?;
                            }
                        }
                        let string_ty = self.fresh_type_var();
                        self.bind(string_ty, TypeTerm::String)?;
                        string_ty
                    }
                    IntrinsicCallKind::Break | IntrinsicCallKind::Continue | IntrinsicCallKind::Return => {
                        // These should never appear as intrinsic calls in HIR
                        // They are converted to proper ExprKind variants in AST→HIR
                        return Err(crate::error::optimization_error(format!(
                            "unexpected control flow intrinsic {:?} in type inference",
                            call.kind
                        )));
                    }
                }
            }
            ExprKind::If(cond, then_expr, else_expr) => {
                let cond_ty = self.infer_expr(cond)?;
                let bool_ty = self.fresh_type_var();
                self.bind(bool_ty, TypeTerm::Bool)?;
                self.unify(cond_ty, bool_ty)?;
                let then_ty = self.infer_expr(then_expr)?;
                if let Some(else_expr) = else_expr {
                    let else_ty = self.infer_expr(else_expr)?;
                    self.unify(then_ty, else_ty)?;
                    then_ty
                } else {
                    then_ty
                }
            }
            ExprKind::Return(value) => {
                if let Some(expr) = value {
                    // Infer return value type for side effects on the type environment
                    let _ = self.infer_expr(expr)?;
                }
                let never = self.fresh_type_var();
                self.bind(never, TypeTerm::Never)?;
                never
            }
            ExprKind::Break(value) => {
                if let Some(expr) = value {
                    // Infer break value type for side effects on the type environment
                    let _ = self.infer_expr(expr)?;
                }
                let never = self.fresh_type_var();
                self.bind(never, TypeTerm::Never)?;
                never
            }
            ExprKind::Continue => {
                let never = self.fresh_type_var();
                self.bind(never, TypeTerm::Never)?;
                never
            }
            ExprKind::Let(pat, _ty, init) => {
                let init_ty = if let Some(expr) = init {
                    self.infer_expr(expr)?
                } else {
                    let unit = self.fresh_type_var();
                    self.bind(unit, TypeTerm::Unit)?;
                    unit
                };
                let pat_ty = self.infer_pattern(pat)?;
                self.unify(pat_ty, init_ty)?;
                let inserted = self.introduce_pattern_bindings(pat)?;
                let bool_ty = self.fresh_type_var();
                self.bind(bool_ty, TypeTerm::Bool)?;
                for id in inserted {
                    self.env.remove(&id);
                    if let Some(scope) = self.scope_stack.last_mut() {
                        scope.retain(|entry| *entry != id);
                    }
                }
                bool_ty
            }
            _ => {
                let unit = self.fresh_type_var();
                self.bind(unit, TypeTerm::Unit)?;
                unit
            }
        };

        self.expr_types.insert(expr.hir_id, ty);
        Ok(ty)
    }

    fn infer_literal(&mut self, lit: &hir::Lit) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match lit {
            hir::Lit::Bool(_value) => self.bind(var, TypeTerm::Bool)?,
            hir::Lit::Integer(_) => self.bind(var, TypeTerm::Int(None))?,
            hir::Lit::Float(_) => self.bind(var, TypeTerm::Float(None))?,
            hir::Lit::Str(_) => self.bind(var, TypeTerm::String)?,
            hir::Lit::Char(_) => self.bind(var, TypeTerm::Custom(hir_types::Ty::char()))?,
        }
        Ok(var)
    }

    fn infer_path(&mut self, hir_id: hir::HirId, path: &hir::Path) -> Result<TypeVarId> {
        if let Some(hir::Res::Local(local_id)) = path.res {
            if let Some(scheme) = self.env.get(&local_id).cloned() {
                return self.instantiate(&scheme);
            }
        }

        if matches!(path.res, Some(hir::Res::SelfTy)) {
            let self_ty = self.current_self_ty.clone().ok_or_else(|| {
                crate::error::optimization_error(
                    "`Self` used outside of an impl context".to_string(),
                )
            })?;
            let var = self.type_from_hir_ty(&self_ty)?;
            self.expr_types.insert(hir_id, var);
            return Ok(var);
        }

        if let Some(hir::Res::Def(def_id)) = path.res {
            let const_ty = self
                .generator
                .type_context
                .lookup_const_type(def_id)
                .cloned();
            if let Some(const_ty) = const_ty {
                return self.type_from_hir_ty(&const_ty);
            }

            let function_sig = self
                .generator
                .type_context
                .lookup_function_signature(def_id)
                .cloned();
            if let Some(sig) = function_sig {
                let ret_var = self.type_from_hir_ty(sig.output.as_ref())?;
                return Ok(ret_var);
            }
        }

        let ty = self.generator.infer_path_type(&path)?;
        let var = self.type_from_hir_ty(&ty)?;
        self.expr_types.insert(hir_id, var);
        Ok(var)
    }

    fn infer_call_args(&mut self, args: &[hir::Expr]) -> Result<TypeVarId> {
        for arg in args {
            // Infer arg types for side effects on the type environment
            let _ = self.infer_expr(arg)?;
        }
        let unit = self.fresh_type_var();
        self.bind(unit, TypeTerm::Unit)?;
        Ok(unit)
    }

    fn lookup_associated_method_signature(
        &mut self,
        path: &hir::Path,
    ) -> Result<Option<hir_types::FnSig>> {
        if path.segments.len() < 2 {
            return Ok(None);
        }

        let method_segment = path
            .segments
            .last()
            .expect("path has at least two segments");
        let owner_name = path.segments[..path.segments.len() - 1]
            .iter()
            .map(|seg| seg.name.clone())
            .collect::<Vec<_>>()
            .join("::");

        if owner_name.is_empty() {
            return Ok(None);
        }

        if let Some(def_id) = self
            .generator
            .type_context
            .lookup_struct_def_id(&owner_name)
        {
            if let Some(owner_ty) = self
                .generator
                .type_context
                .make_struct_ty_by_id(def_id, Vec::new())
            {
                if let Some(sig) = self
                    .generator
                    .type_context
                    .lookup_method_signature(&owner_ty, &method_segment.name)
                    .cloned()
                {
                    return Ok(Some(sig));
                }
            }
        }

        Ok(None)
    }

    fn infer_pattern(&mut self, pat: &hir::Pat) -> Result<TypeVarId> {
        use hir::PatKind;
        let var = match &pat.kind {
            PatKind::Binding { .. } => {
                let var = self.fresh_type_var();
                self.binding_vars.insert(pat.hir_id, var);
                var
            }
            PatKind::Wild => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Unit)?;
                var
            }
            PatKind::Struct(_, fields) => {
                let struct_var = self.fresh_type_var();
                for field in fields {
                    let field_var = self.infer_pattern(&field.pat)?;
                    self.unify(struct_var, field_var)?;
                }
                struct_var
            }
            PatKind::Tuple(elements) => {
                let tuple_var = self.fresh_type_var();
                for elem in elements {
                    let elem_var = self.infer_pattern(elem)?;
                    self.unify(tuple_var, elem_var)?;
                }
                tuple_var
            }
            PatKind::Lit(lit) => {
                let var = self.infer_literal(lit)?;
                var
            }
        };
        self.pattern_types.insert(pat.hir_id, var);
        Ok(var)
    }

    fn introduce_pattern_bindings(&mut self, pat: &hir::Pat) -> Result<Vec<hir::HirId>> {
        let mut bindings = Vec::new();
        self.collect_pattern_bindings(pat, &mut bindings);
        for binding_id in &bindings {
            if let Some(var) = self.binding_vars.get(&binding_id).copied() {
                let scheme = self.generalize(var)?;
                self.env.insert(*binding_id, scheme);
                if let Some(scope) = self.scope_stack.last_mut() {
                    scope.push(*binding_id);
                }
            }
        }
        Ok(bindings)
    }

    fn collect_pattern_bindings(&self, pat: &hir::Pat, acc: &mut Vec<hir::HirId>) {
        match &pat.kind {
            hir::PatKind::Binding { .. } => acc.push(pat.hir_id),
            hir::PatKind::Struct(_, fields) => {
                for field in fields {
                    self.collect_pattern_bindings(&field.pat, acc);
                }
            }
            hir::PatKind::Tuple(elements) => {
                for element in elements {
                    self.collect_pattern_bindings(element, acc);
                }
            }
            _ => {}
        }
    }

    fn fresh_type_var(&mut self) -> TypeVarId {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar {
            kind: TypeVarKind::Unbound {
                level: self.current_level,
            },
        });
        id
    }

    fn find(&mut self, var: TypeVarId) -> TypeVarId {
        match self.type_vars[var].kind.clone() {
            TypeVarKind::Link(target) => {
                let root = self.find(target);
                self.type_vars[var].kind = TypeVarKind::Link(root);
                root
            }
            _ => var,
        }
    }

    fn bind(&mut self, var: TypeVarId, term: TypeTerm) -> Result<()> {
        let root = self.find(var);
        if self.occurs_check(root, &term)? {
            return Err(crate::error::optimization_error(
                "Occurs check failed during type inference",
            ));
        }
        self.type_vars[root].kind = TypeVarKind::Bound(term);
        Ok(())
    }

    fn occurs_check(&mut self, var: TypeVarId, term: &TypeTerm) -> Result<bool> {
        let mut stack = Vec::new();
        stack.push(term.clone());
        while let Some(t) = stack.pop() {
            match t {
                TypeTerm::Tuple(elems) => {
                    for elem in elems {
                        let root = self.find(elem);
                        if root == var {
                            return Ok(true);
                        }
                        match self.type_vars[root].kind.clone() {
                            TypeVarKind::Bound(term) => stack.push(term),
                            _ => {}
                        }
                    }
                }
                TypeTerm::Function(args, ret) => {
                    for arg in args {
                        let root = self.find(arg);
                        if root == var {
                            return Ok(true);
                        }
                        match self.type_vars[root].kind.clone() {
                            TypeVarKind::Bound(term) => stack.push(term),
                            _ => {}
                        }
                    }
                    let ret_root = self.find(ret);
                    if ret_root == var {
                        return Ok(true);
                    }
                    if let TypeVarKind::Bound(term) = self.type_vars[ret_root].kind.clone() {
                        stack.push(term);
                    }
                }
                _ => {}
            }
        }
        Ok(false)
    }

    fn unify(&mut self, left: TypeVarId, right: TypeVarId) -> Result<()> {
        let left_root = self.find(left);
        let right_root = self.find(right);
        if left_root == right_root {
            return Ok(());
        }

        let left_kind = self.type_vars[left_root].kind.clone();
        let right_kind = self.type_vars[right_root].kind.clone();

        match (left_kind, right_kind) {
            (TypeVarKind::Unbound { level: l_level }, TypeVarKind::Unbound { level: r_level }) => {
                if l_level <= r_level {
                    self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                } else {
                    self.type_vars[left_root].kind = TypeVarKind::Link(right_root);
                }
                Ok(())
            }
            (TypeVarKind::Unbound { .. }, TypeVarKind::Bound(term)) => {
                self.bind(left_root, term)?;
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (TypeVarKind::Bound(term), TypeVarKind::Unbound { .. }) => {
                self.bind(right_root, term)?;
                self.type_vars[left_root].kind = TypeVarKind::Link(right_root);
                Ok(())
            }
            (TypeVarKind::Bound(left_term), TypeVarKind::Bound(right_term)) => {
                self.unify_terms(left_root, left_term, right_root, right_term)
            }
            (TypeVarKind::Link(_), _) | (_, TypeVarKind::Link(_)) => unreachable!(),
        }
    }

    fn unify_terms(
        &mut self,
        left_root: TypeVarId,
        left_term: TypeTerm,
        right_root: TypeVarId,
        right_term: TypeTerm,
    ) -> Result<()> {
        use TypeTerm::*;
        match (left_term, right_term) {
            (Bool, Bool) | (String, String) | (Unit, Unit) | (Never, Never) => {
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Int(a), Int(b)) => {
                let merged = Self::merge_int_kinds(a, b)?;
                self.type_vars[left_root].kind = TypeVarKind::Bound(Int(merged));
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Uint(a), Uint(b)) => {
                let merged = Self::merge_uint_kinds(a, b)?;
                self.type_vars[left_root].kind = TypeVarKind::Bound(Uint(merged));
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Int(None), Uint(kind)) => {
                let merged = Self::merge_uint_kinds(None, kind)?;
                self.type_vars[left_root].kind = TypeVarKind::Bound(Uint(merged));
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Uint(kind), Int(None)) => {
                let merged = Self::merge_uint_kinds(kind, None)?;
                self.type_vars[left_root].kind = TypeVarKind::Bound(Uint(merged));
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Float(a), Float(b)) => {
                let merged = Self::merge_float_kinds(a, b)?;
                self.type_vars[left_root].kind = TypeVarKind::Bound(Float(merged));
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Custom(a), Custom(b)) if a == b => {
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Tuple(a_elems), Tuple(b_elems)) if a_elems.len() == b_elems.len() => {
                for (a, b) in a_elems.into_iter().zip(b_elems.into_iter()) {
                    self.unify(a, b)?;
                }
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (Function(a_args, a_ret), Function(b_args, b_ret)) if a_args.len() == b_args.len() => {
                for (a, b) in a_args.into_iter().zip(b_args.into_iter()) {
                    self.unify(a, b)?;
                }
                self.unify(a_ret, b_ret)?;
                self.type_vars[right_root].kind = TypeVarKind::Link(left_root);
                Ok(())
            }
            (lhs, rhs) => Err(crate::error::optimization_error(format!(
                "Type mismatch during unification: {:?} vs {:?}",
                lhs, rhs
            ))),
        }
    }

    fn merge_int_kinds(
        a: Option<hir_types::IntTy>,
        b: Option<hir_types::IntTy>,
    ) -> Result<Option<hir_types::IntTy>> {
        match (a, b) {
            (Some(x), Some(y)) if x == y => Ok(Some(x)),
            (Some(x), None) | (None, Some(x)) => Ok(Some(x)),
            (None, None) => Ok(None),
            (Some(x), Some(y)) => Err(crate::error::optimization_error(format!(
                "Conflicting integer types: {:?} vs {:?}",
                x, y
            ))),
        }
    }

    fn merge_uint_kinds(
        a: Option<hir_types::UintTy>,
        b: Option<hir_types::UintTy>,
    ) -> Result<Option<hir_types::UintTy>> {
        match (a, b) {
            (Some(x), Some(y)) if x == y => Ok(Some(x)),
            (Some(x), None) | (None, Some(x)) => Ok(Some(x)),
            (None, None) => Ok(None),
            (Some(x), Some(y)) => Err(crate::error::optimization_error(format!(
                "Conflicting unsigned integer types: {:?} vs {:?}",
                x, y
            ))),
        }
    }

    fn merge_float_kinds(
        a: Option<hir_types::FloatTy>,
        b: Option<hir_types::FloatTy>,
    ) -> Result<Option<hir_types::FloatTy>> {
        match (a, b) {
            (Some(x), Some(y)) if x == y => Ok(Some(x)),
            (Some(x), None) | (None, Some(x)) => Ok(Some(x)),
            (None, None) => Ok(None),
            (Some(x), Some(y)) => Err(crate::error::optimization_error(format!(
                "Conflicting float types: {:?} vs {:?}",
                x, y
            ))),
        }
    }

    fn type_from_hir_ty(&mut self, ty: &hir_types::Ty) -> Result<TypeVarId> {
        use hir_types::TyKind;
        let var = self.fresh_type_var();
        let term = match &ty.kind {
            TyKind::Bool => TypeTerm::Bool,
            TyKind::Char => TypeTerm::Custom(hir_types::Ty::char()),
            TyKind::Int(int_ty) => TypeTerm::Int(Some(*int_ty)),
            TyKind::Uint(uint_ty) => TypeTerm::Uint(Some(*uint_ty)),
            TyKind::Float(float_ty) => TypeTerm::Float(Some(*float_ty)),
            TyKind::Tuple(elements) => {
                let mut element_vars = Vec::new();
                for elem in elements {
                    let elem_ty = self.type_from_hir_ty(elem)?;
                    element_vars.push(elem_ty);
                }
                TypeTerm::Tuple(element_vars)
            }
            TyKind::Never => TypeTerm::Never,
            _ => TypeTerm::Custom(ty.clone()),
        };
        self.bind(var, term)?;
        Ok(var)
    }

    fn instantiate(&mut self, scheme: &TypeScheme) -> Result<TypeVarId> {
        let mut map = Vec::with_capacity(scheme.vars);
        for _ in 0..scheme.vars {
            map.push(self.fresh_type_var());
        }
        self.instantiate_scheme_type(&scheme.body, &map)
    }

    fn instantiate_scheme_type(&mut self, ty: &SchemeType, map: &[TypeVarId]) -> Result<TypeVarId> {
        use SchemeType::*;
        Ok(match ty {
            Var(idx) => map[*idx as usize],
            Bool => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Bool)?;
                var
            }
            Int(kind) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Int(*kind))?;
                var
            }
            Uint(kind) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Uint(*kind))?;
                var
            }
            Float(kind) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Float(*kind))?;
                var
            }
            String => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::String)?;
                var
            }
            Unit => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Unit)?;
                var
            }
            Never => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Never)?;
                var
            }
            Custom(inner) => {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Custom(inner.clone()))?;
                var
            }
            Tuple(elements) => {
                let mut vars = Vec::new();
                for elem in elements {
                    vars.push(self.instantiate_scheme_type(elem, map)?);
                }
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Tuple(vars))?;
                var
            }
            Function(args, ret) => {
                let mut arg_vars = Vec::new();
                for arg in args {
                    arg_vars.push(self.instantiate_scheme_type(arg, map)?);
                }
                let ret_var = self.instantiate_scheme_type(ret, map)?;
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Function(arg_vars, ret_var))?;
                var
            }
        })
    }

    fn generalize(&mut self, var: TypeVarId) -> Result<TypeScheme> {
        let mut mapping = HashMap::new();
        let mut next_index = 0u32;
        let body = self.build_scheme_type(var, &mut mapping, &mut next_index)?;
        Ok(TypeScheme {
            vars: next_index as usize,
            body,
        })
    }

    fn build_scheme_type(
        &mut self,
        var: TypeVarId,
        mapping: &mut HashMap<TypeVarId, u32>,
        next_index: &mut u32,
    ) -> Result<SchemeType> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { level } => {
                if level > self.current_level {
                    if let Some(idx) = mapping.get(&root) {
                        Ok(SchemeType::Var(*idx))
                    } else {
                        let idx = *next_index;
                        mapping.insert(root, idx);
                        *next_index += 1;
                        Ok(SchemeType::Var(idx))
                    }
                } else {
                    // Default unconstrained variables to i64
                    Ok(SchemeType::Int(Some(hir_types::IntTy::I64)))
                }
            }
            TypeVarKind::Bound(term) => match term {
                TypeTerm::Bool => Ok(SchemeType::Bool),
                TypeTerm::Int(kind) => Ok(SchemeType::Int(kind)),
                TypeTerm::Uint(kind) => Ok(SchemeType::Uint(kind)),
                TypeTerm::Float(kind) => Ok(SchemeType::Float(kind)),
                TypeTerm::String => Ok(SchemeType::String),
                TypeTerm::Unit => Ok(SchemeType::Unit),
                TypeTerm::Never => Ok(SchemeType::Never),
                TypeTerm::Custom(inner) => Ok(SchemeType::Custom(inner)),
                TypeTerm::Tuple(elements) => {
                    let mut converted = Vec::new();
                    for elem in elements {
                        converted.push(self.build_scheme_type(elem, mapping, next_index)?);
                    }
                    Ok(SchemeType::Tuple(converted))
                }
                TypeTerm::Function(args, ret) => {
                    let mut converted_args = Vec::new();
                    for arg in args {
                        converted_args.push(self.build_scheme_type(arg, mapping, next_index)?);
                    }
                    let ret_ty = self.build_scheme_type(ret, mapping, next_index)?;
                    Ok(SchemeType::Function(converted_args, Box::new(ret_ty)))
                }
            },
            TypeVarKind::Link(_) => unreachable!(),
        }
    }

    fn generalize_record_results(&mut self) -> Result<()> {
        let mut expr_results = HashMap::new();
        let expr_ids: Vec<_> = self.expr_types.keys().copied().collect();
        for hir_id in expr_ids {
            let var = self.expr_types[&hir_id];
            let ty = self.resolve_to_hir(var)?;
            expr_results.insert(hir_id, ty);
        }

        let mut pat_results = HashMap::new();
        let pat_ids: Vec<_> = self.pattern_types.keys().copied().collect();
        for hir_id in pat_ids {
            let var = self.pattern_types[&hir_id];
            let ty = self.resolve_to_hir(var)?;
            pat_results.insert(hir_id, ty);
        }

        for (hir_id, ty) in expr_results {
            self.generator.inferred_expr_types.insert(hir_id, ty);
        }
        for (hir_id, ty) in pat_results {
            self.generator.inferred_pattern_types.insert(hir_id, ty);
        }

        Ok(())
    }

    fn ensure_numeric(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let ty = self.resolve_to_hir(var)?;
        match ty.kind {
            hir_types::TyKind::Int(_)
            | hir_types::TyKind::Uint(_)
            | hir_types::TyKind::Float(_) => Ok(()),
            _ => Err(crate::error::optimization_error(format!(
                "{} requires numeric operands, found {:?}",
                context, ty
            ))),
        }
    }

    fn ensure_integer(&mut self, var: TypeVarId, context: &str) -> Result<()> {
        let ty = self.resolve_to_hir(var)?;
        match ty.kind {
            hir_types::TyKind::Int(_) | hir_types::TyKind::Uint(_) => Ok(()),
            _ => Err(crate::error::optimization_error(format!(
                "{} requires integer operands, found {:?}",
                context, ty
            ))),
        }
    }

    fn resolve_to_hir(&mut self, var: TypeVarId) -> Result<hir_types::Ty> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => Ok(hir_types::Ty::int(hir_types::IntTy::I64)),
            TypeVarKind::Bound(term) => self.term_to_hir(term),
            TypeVarKind::Link(_) => unreachable!(),
        }
    }

    fn term_to_hir(&mut self, term: TypeTerm) -> Result<hir_types::Ty> {
        use TypeTerm::*;
        Ok(match term {
            Bool => hir_types::Ty::bool(),
            Int(kind) => hir_types::Ty::int(kind.unwrap_or(hir_types::IntTy::I64)),
            Uint(kind) => hir_types::Ty::uint(kind.unwrap_or(hir_types::UintTy::U32)),
            Float(kind) => hir_types::Ty::float(kind.unwrap_or(hir_types::FloatTy::F64)),
            String => self.generator.create_string_type(),
            Unit => self.generator.create_unit_type(),
            Never => self.generator.create_never_type(),
            Custom(inner) => inner,
            Tuple(elements) => {
                let mut tys = Vec::new();
                for elem in elements {
                    tys.push(Box::new(self.resolve_to_hir(elem)?));
                }
                hir_types::Ty {
                    kind: hir_types::TyKind::Tuple(tys),
                }
            }
            Function(args, ret) => {
                let _ = (args, ret);
                self.generator.create_unit_type()
            }
        })
    }
}
