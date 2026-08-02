use fp_core::error::{Error, Result};
use fp_core::hir;
use fp_core::hir::ty::{self, AdtDef, AdtFlags, GenericArg, ReprFlags, ReprOptions, Ty, TyKind};
use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use std::collections::HashMap;

use crate::types::{GenericCallResolution, TypeckResults};

/// Type checks resolved HIR and records semantic types outside the source tree.
/// This is deliberately a side-table pass: HIR nodes remain source-shaped and
/// MIR lowering can consume the results without an AST round trip.
pub struct HirTypeChecker {
    program: hir::Program,
    results: TypeckResults,
    locals: Vec<HashMap<hir::Symbol, Ty>>,
    generic_scopes: Vec<HashMap<hir::DefId, Ty>>,
    self_types: Vec<Ty>,
}

impl HirTypeChecker {
    pub fn new(program: hir::Program) -> Self {
        Self {
            program,
            results: TypeckResults::default(),
            locals: vec![HashMap::new()],
            generic_scopes: Vec::new(),
            self_types: Vec::new(),
        }
    }

    pub fn check(mut self) -> Result<(hir::Program, TypeckResults)> {
        let items = self.program.items.clone();
        for item in &items {
            self.check_item(item)?;
        }
        Ok((self.program, self.results))
    }

    fn check_item(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Function(function) => {
                self.check_function(function)?;
            }
            hir::ItemKind::Const(constant) => {
                self.check_type_expr(&constant.ty)?;
                self.check_body(&constant.body)?;
            }
            hir::ItemKind::Impl(impl_item) => {
                self.push_generics(&impl_item.generics);
                let self_ty = self.check_type_expr(&impl_item.self_ty)?;
                self.self_types.push(self_ty);
                if let Some(trait_ty) = &impl_item.trait_ty {
                    self.check_type_expr(trait_ty)?;
                }
                for item in &impl_item.items {
                    match &item.kind {
                        hir::ImplItemKind::Method(function) => self.check_function(function)?,
                        hir::ImplItemKind::AssocConst(constant) => {
                            self.check_type_expr(&constant.ty)?;
                            self.check_body(&constant.body)?;
                        }
                    }
                }
                self.self_types.pop();
                self.pop_generics();
            }
            hir::ItemKind::Struct(def) => {
                self.push_generics(&def.generics);
                for field in &def.fields {
                    self.check_type_expr(&field.ty)?;
                }
                self.pop_generics();
            }
            hir::ItemKind::Enum(def) => {
                self.push_generics(&def.generics);
                for variant in &def.variants {
                    if let Some(payload) = &variant.payload {
                        self.check_type_expr(payload)?;
                    }
                    if let Some(discriminant) = &variant.discriminant {
                        self.check_expr(discriminant)?;
                    }
                }
                self.pop_generics();
            }
            hir::ItemKind::Query(_) => {}
            hir::ItemKind::Expr(expr) => {
                self.check_expr(expr)?;
            }
        }
        Ok(())
    }

    fn check_function(&mut self, function: &hir::Function) -> Result<()> {
        self.push_generics(&function.sig.generics);
        self.check_signature(&function.sig)?;
        if let Some(body) = &function.body {
            self.check_body(body)?;
        }
        self.pop_generics();
        Ok(())
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

    fn pop_generics(&mut self) {
        self.generic_scopes.pop();
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

    fn check_body(&mut self, body: &hir::Body) -> Result<()> {
        self.locals.push(HashMap::new());
        for param in &body.params {
            let ty = self.check_type_expr(&param.ty)?;
            self.bind_pattern(&param.pat, ty)?;
        }
        self.check_expr(&body.value)?;
        self.locals.pop();
        Ok(())
    }

    fn check_expr(&mut self, expr: &hir::Expr) -> Result<Ty> {
        let ty = match &expr.kind {
            hir::ExprKind::Literal(lit) => self.literal_ty(lit),
            hir::ExprKind::Path(path) => self.expr_path_ty(path)?,
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
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
                let value_ty = self.check_expr(value)?;
                if matches!(op, hir::UnOp::Not) {
                    self.require_same(&value_ty, &Ty::bool())?;
                }
                value_ty
            }
            hir::ExprKind::Reference(reference) => Ty {
                kind: TyKind::Ref(
                    ty::Region::ReErased,
                    Box::new(self.check_expr(&reference.expr)?),
                    reference.mutable,
                ),
            },
            hir::ExprKind::Call(callee, args) => {
                let callee_ty = self.check_expr(callee)?;
                let arg_types = args
                    .iter()
                    .map(|arg| self.check_expr(&arg.value))
                    .collect::<Result<Vec<_>>>()?;
                let Some((substitutions, output)) = self.instantiate_call(&callee_ty, &arg_types)? else {
                    return Err(Error::from("called expression is not a function"));
                };
                if let hir::ExprKind::Path(path) = &callee.kind {
                    if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                        if let Some(args) = self.generic_call_args(*def_id, &substitutions)? {
                            self.results.generic_call_args.insert(
                                expr.hir_id,
                                GenericCallResolution { def_id: *def_id, args },
                            );
                        }
                    }
                }
                output
            }
            hir::ExprKind::MethodCall(receiver, method, args) => {
                let receiver_ty = self.check_expr(receiver)?;
                let mut arg_types = vec![receiver_ty.clone()];
                arg_types.extend(
                    args.iter()
                        .map(|arg| self.check_expr(&arg.value))
                        .collect::<Result<Vec<_>>>()?,
                );
                let (method_def_id, output) = self.method_output(&receiver_ty, method, &arg_types)?;
                self.results
                    .method_resolutions
                    .insert(expr.hir_id, method_def_id);
                output
            }
            hir::ExprKind::FieldAccess(receiver, field) => {
                let receiver_ty = self.check_expr(receiver)?;
                self.field_ty(&receiver_ty, field)?
            }
            hir::ExprKind::Index(receiver, index) => {
                let receiver_ty = self.check_expr(receiver)?;
                let index_ty = self.check_expr(index)?;
                self.require_same(&index_ty, &Ty::int(ty::IntTy::I64))?;
                match receiver_ty.kind {
                    TyKind::Array(inner, _) | TyKind::Slice(inner) => *inner,
                    _ => return Err(Error::from("indexing requires an array or slice")),
                }
            }
            hir::ExprKind::Cast(value, target) => {
                self.check_expr(value)?;
                self.check_type_expr(target)?
            }
            hir::ExprKind::Struct(path, fields) => {
                let ty = self.path_ty(path)?;
                for field in fields {
                    let value_ty = self.check_expr(&field.expr)?;
                    let field_ty = self.field_ty(&ty, &field.name)?;
                    self.require_same(&value_ty, &field_ty)?;
                }
                ty
            }
            hir::ExprKind::If(condition, then_expr, else_expr) => {
                let condition = self.check_expr(condition)?;
                self.require_same(&condition, &Ty::bool())?;
                let then_ty = self.check_expr(then_expr)?;
                if let Some(else_expr) = else_expr {
                    let else_ty = self.check_expr(else_expr)?;
                    self.require_same(&then_ty, &else_ty)?;
                }
                match else_expr.as_ref() {
                    Some(_) => then_ty,
                    None => self.unit_ty(),
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                let scrutinee_ty = self.check_expr(scrutinee)?;
                if arms.is_empty() {
                    return Err(Error::from("match expression requires at least one arm"));
                }
                let mut result = None;
                for arm in arms {
                    let arm_ty = self.check_match_arm(arm, &scrutinee_ty)?;
                    if let Some(result_ty) = &result {
                        self.require_same(result_ty, &arm_ty)?;
                    } else {
                        result = Some(arm_ty);
                    }
                }
                result.ok_or_else(|| Error::from("match expression requires at least one arm"))?
            }
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.check_block(block)?
            }
            hir::ExprKind::While(condition, block) => {
                let condition_ty = self.check_expr(condition)?;
                self.require_same(&condition_ty, &Ty::bool())?;
                self.check_block(block)?
            }
            hir::ExprKind::Array(values) => {
                let Some(first) = values.first() else {
                    return Err(Error::from("empty array has no inferable element type"));
                };
                let element = self.check_expr(first)?;
                for value in values.iter().skip(1) {
                    let value_ty = self.check_expr(value)?;
                    self.require_same(&element, &value_ty)?;
                }
                Ty { kind: TyKind::Array(Box::new(element), ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id))) }
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let element = self.check_expr(elem)?;
                self.check_expr(len)?;
                Ty { kind: TyKind::Array(Box::new(element), ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id))) }
            }
            hir::ExprKind::Assign(lhs, rhs) => {
                let lhs = self.check_expr(lhs)?;
                let rhs = self.check_expr(rhs)?;
                self.require_same(&lhs, &rhs)?;
                lhs
            }
            hir::ExprKind::Return(value) | hir::ExprKind::Break(value) => match value.as_ref() {
                Some(value) => self.check_expr(value)?,
                None => self.unit_ty(),
            },
            hir::ExprKind::Continue => Ty::never(),
            hir::ExprKind::Let(pattern, target, value) => {
                let ty = self.check_type_expr(target)?;
                if let Some(value) = value {
                    let value_ty = self.check_expr(value)?;
                    self.require_same(&ty, &value_ty)?;
                }
                self.bind_pattern(pattern, ty.clone())?;
                ty
            }
            hir::ExprKind::Try(value) => {
                let input_ty = self.check_expr(&value.expr)?;
                let result_ty = input_ty.clone();
                for catch in &value.catches {
                    if let Some(pattern) = &catch.pat {
                        self.bind_pattern(pattern, Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) })?;
                    }
                    let catch_ty = self.check_expr(&catch.body)?;
                    self.require_same(&result_ty, &catch_ty)?;
                }
                if let Some(elze) = &value.elze {
                    let elze_ty = self.check_expr(elze)?;
                    self.require_same(&result_ty, &elze_ty)?;
                }
                if let Some(finally) = &value.finally {
                    self.check_expr(finally)?;
                }
                result_ty
            }
            hir::ExprKind::With(context, body) => {
                self.check_expr(context)?;
                self.check_expr(body)?
            }
            hir::ExprKind::Slice(slice) => {
                let base_ty = self.check_expr(&slice.base)?;
                if let Some(start) = &slice.start { self.check_expr(start)?; }
                if let Some(end) = &slice.end { self.check_expr(end)?; }
                match base_ty.kind {
                    TyKind::Array(inner, _) => Ty { kind: TyKind::Slice(inner) },
                    TyKind::Slice(inner) => Ty { kind: TyKind::Slice(inner) },
                    _ => return Err(Error::from("slicing requires an array or slice")),
                }
            }
            hir::ExprKind::Query(_) => return Err(Error::from("query typing is not implemented")),
            hir::ExprKind::IntrinsicCall(call) => self.check_intrinsic(call)?,
            hir::ExprKind::FormatString(format) => {
                for part in &format.parts {
                    if let hir::FormatTemplatePart::Placeholder(placeholder) = part {
                        let _ = placeholder;
                    }
                }
                Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) }
            }
        };
        self.results.record_expr_type(expr.hir_id, ty.clone());
        Ok(ty)
    }

    fn check_block(&mut self, block: &hir::Block) -> Result<Ty> {
        self.locals.push(HashMap::new());
        for stmt in &block.stmts {
            match &stmt.kind {
                hir::StmtKind::Local(local) => {
                    let ty = match (&local.ty, &local.init) {
                        (Some(annotation), Some(init)) => {
                            let ty = self.check_type_expr(annotation)?;
                            let init_ty = self.check_expr(init)?;
                            self.require_same(&ty, &init_ty)?;
                            ty
                        }
                        (Some(annotation), None) => self.check_type_expr(annotation)?,
                        (None, Some(init)) => self.check_expr(init)?,
                        (None, None) => return Err(Error::from("local binding needs a type or initializer")),
                    };
                    self.bind_pattern(&local.pat, ty)?;
                }
                hir::StmtKind::Item(item) => self.check_item(item)?,
                hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => { self.check_expr(expr)?; }
            }
        }
        let ty = match block.expr.as_ref() {
            Some(expr) => self.check_expr(expr)?,
            None => self.unit_ty(),
        };
        self.locals.pop();
        Ok(ty)
    }

    fn check_match_arm(&mut self, arm: &hir::MatchArm, scrutinee_ty: &Ty) -> Result<Ty> {
        self.locals.push(HashMap::new());
        let result = (|| {
            self.bind_pattern(&arm.pat, scrutinee_ty.clone())?;
            if let Some(guard) = &arm.guard {
                let guard_ty = self.check_expr(guard)?;
                self.require_same(&guard_ty, &Ty::bool())?;
            }
            self.check_expr(&arm.body)
        })();
        self.locals.pop();
        result
    }

    fn check_type_expr(&mut self, expr: &hir::TypeExpr) -> Result<Ty> {
        let ty = match &expr.kind {
            hir::TypeExprKind::Primitive(primitive) => primitive_ty(*primitive),
            hir::TypeExprKind::Path(path) => self.path_ty(path)?,
            hir::TypeExprKind::Tuple(items) => Ty { kind: TyKind::Tuple(items.iter().map(|item| self.check_type_expr(item).map(Box::new)).collect::<Result<_>>()?) },
            hir::TypeExprKind::Slice(item) => Ty { kind: TyKind::Slice(Box::new(self.check_type_expr(item)?)) },
            hir::TypeExprKind::Ptr(item) => Ty { kind: TyKind::RawPtr(ty::TypeAndMut { ty: Box::new(self.check_type_expr(item)?), mutbl: ty::Mutability::Not }) },
            hir::TypeExprKind::Ref(item) => Ty { kind: TyKind::Ref(ty::Region::ReErased, Box::new(self.check_type_expr(item)?), ty::Mutability::Not) },
            hir::TypeExprKind::FnPtr(function) => Ty { kind: TyKind::FnPtr(ty::PolyFnSig { binder: ty::Binder { value: ty::FnSig { inputs: function.inputs.iter().map(|input| self.check_type_expr(input).map(Box::new)).collect::<Result<_>>()?, output: Box::new(self.check_type_expr(&function.output)?), c_variadic: false, unsafety: ty::Unsafety::Normal, abi: ty::Abi::Rust }, bound_vars: Vec::new() } }) },
            hir::TypeExprKind::Never => Ty::never(),
            hir::TypeExprKind::Array(item, _) => Ty { kind: TyKind::Array(Box::new(self.check_type_expr(item)?), ty::ConstKind::Infer(ty::InferConst::Fresh(expr.hir_id))) },
            hir::TypeExprKind::Infer => Ty { kind: TyKind::Infer(ty::InferTy::FreshTy(expr.hir_id)) },
            hir::TypeExprKind::Error => {
                return Err(Error::from("invalid type expression"));
            }
            hir::TypeExprKind::Structural(_) => {
                return Err(Error::from("structural types are not supported by HIR typing"));
            }
            hir::TypeExprKind::TypeBinaryOp(_) => {
                return Err(Error::from("type expressions cannot be combined with a type operator"));
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
        let Some(hir::Res::Def(def_id)) = path.res else {
            return Err(Error::from("unresolved type path"));
        };
        if let Some(generic) = self.generic_ty(def_id) {
            return Ok(generic);
        }
        let Some(item) = self.program.def_map.get(&def_id).cloned() else {
            return Err(Error::from(format!("type definition `{def_id}` was not found")));
        };
        let (flags, variants) = match &item.kind {
            hir::ItemKind::Struct(_) => (AdtFlags::IS_STRUCT, Vec::new()),
            hir::ItemKind::Enum(def) => (AdtFlags::IS_ENUM, def.variants.iter().enumerate().map(|(index, variant)| ty::VariantDef { def_id: variant.def_id, ctor_def_id: Some(variant.def_id), ident: variant.name.clone(), discr: ty::VariantDiscr::Relative(index as u32), fields: Vec::new(), ctor_kind: ty::CtorKind::Fn, is_recovered: false }).collect()),
            _ => {
                return Err(Error::from(format!("definition `{def_id}` is not a type")))
            }
        };
        let args = match path.segments.iter().find_map(|segment| segment.args.as_ref()) {
            Some(args) => args
                .args
                .iter()
                .map(|arg| match arg {
                    hir::GenericArg::Type(ty) => self.check_type_expr(ty).map(GenericArg::Type),
                    hir::GenericArg::Const(_) => Err(Error::from("const generic arguments are not supported")),
                })
                .collect::<Result<Vec<_>>>()?,
            None => Vec::new(),
        };
        Ok(Ty { kind: TyKind::Adt(AdtDef { did: def_id, variants, flags, repr: ReprOptions { int: None, align: None, pack: None, flags: ReprFlags::empty(), field_shuffle_seed: 0 } }, args) })
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
            return Err(Error::from("unresolved value path"));
        };
        let Some(item) = self.program.def_map.get(&def_id).cloned() else {
            for enum_item in self.program.items.clone() {
                let hir::ItemKind::Enum(enum_def) = &enum_item.kind else {
                    continue;
                };
                let Some(variant) = enum_def.variants.iter().find(|variant| variant.def_id == def_id) else {
                    continue;
                };
                let enum_ty = self.enum_item_ty(&enum_item, path)?;
                if let Some(payload) = &variant.payload {
                    let payload_ty = self.check_type_expr(payload)?;
                    return Ok(Ty {
                        kind: TyKind::FnPtr(ty::PolyFnSig {
                            binder: ty::Binder {
                                value: ty::FnSig {
                                    inputs: vec![Box::new(payload_ty)],
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
            return Err(Error::from(format!("value definition `{def_id}` was not found")));
        };
        match &item.kind {
            hir::ItemKind::Struct(_) | hir::ItemKind::Enum(_) => self.path_ty(path),
            hir::ItemKind::Const(constant) => self.check_type_expr(&constant.ty),
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
                        hir::GenericArg::Const(_) => Err(Error::from("const generic arguments are not supported")),
                    })
                    .collect::<Result<Vec<_>>>()
            })
            .transpose()?;
        let args = match args {
            Some(args) => args,
            None => Vec::new(),
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
        self.push_generics(&function.sig.generics);
        let inputs = function
            .sig
            .inputs
            .iter()
            .map(|input| self.check_type_expr(&input.ty).map(Box::new))
            .collect::<Result<Vec<_>>>()?;
        let output = Box::new(self.check_type_expr(&function.sig.output)?);
        self.pop_generics();
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
        let TyKind::FnPtr(signature) = &callable.kind else { return Ok(None) };
        if signature.binder.value.inputs.len() != actuals.len() {
            return Err(Error::from("call argument count does not match function signature"));
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

    fn unify_call_types(&self, expected: &Ty, actual: &Ty, substitutions: &mut HashMap<ty::ParamTy, Ty>) -> Result<()> {
        match (&expected.kind, &actual.kind) {
            (TyKind::Param(param), _) => {
                if let Some(previous) = substitutions.get(param) {
                    self.require_same(previous, actual)?;
                } else {
                    substitutions.insert(param.clone(), actual.clone());
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
            (TyKind::Tuple(expected), TyKind::Tuple(actual)) if expected.len() == actual.len() => expected
                .iter()
                .zip(actual)
                .try_for_each(|(expected, actual)| self.unify_call_types(expected, actual, substitutions)),
            (TyKind::Array(expected, _), TyKind::Array(actual, _))
            | (TyKind::Slice(expected), TyKind::Slice(actual)) => self.unify_call_types(expected, actual, substitutions),
            (TyKind::Adt(expected, expected_args), TyKind::Adt(actual, actual_args))
                if expected.did == actual.did && expected_args.len() == actual_args.len() =>
            {
                for (expected, actual) in expected_args.iter().zip(actual_args) {
                    if let (GenericArg::Type(expected), GenericArg::Type(actual)) = (expected, actual) {
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
            TyKind::Ref(region, inner, mutable) => Ty { kind: TyKind::Ref(region.clone(), Box::new(self.substitute_param_map(inner, substitutions)), *mutable) },
            TyKind::RawPtr(value) => Ty { kind: TyKind::RawPtr(ty::TypeAndMut { ty: Box::new(self.substitute_param_map(&value.ty, substitutions)), mutbl: value.mutbl }) },
            TyKind::Tuple(fields) => Ty { kind: TyKind::Tuple(fields.iter().map(|field| Box::new(self.substitute_param_map(field, substitutions))).collect()) },
            TyKind::Array(inner, length) => Ty { kind: TyKind::Array(Box::new(self.substitute_param_map(inner, substitutions)), length.clone()) },
            TyKind::Slice(inner) => Ty { kind: TyKind::Slice(Box::new(self.substitute_param_map(inner, substitutions))) },
            TyKind::Adt(def, args) => Ty { kind: TyKind::Adt(def.clone(), args.iter().map(|arg| match arg {
                GenericArg::Type(ty) => GenericArg::Type(self.substitute_param_map(ty, substitutions)),
                other => other.clone(),
            }).collect()) },
            _ => ty.clone(),
        }
    }

    fn method_output(
        &mut self,
        receiver_ty: &Ty,
        method: &hir::Symbol,
        actuals: &[Ty],
    ) -> Result<(hir::DefId, Ty)> {
        let receiver_def = match &receiver_ty.kind {
            TyKind::Adt(receiver, _) => receiver.did,
            TyKind::Ref(_, inner, _) => match &inner.kind {
                TyKind::Adt(receiver, _) => receiver.did,
                _ => return Err(Error::from("method receiver is not a nominal type")),
            },
            _ => return Err(Error::from("method receiver is not a nominal type")),
        };
        for item in self.program.items.clone() {
            let hir::ItemKind::Impl(impl_item) = item.kind else {
                continue;
            };
            let hir::TypeExprKind::Path(path) = &impl_item.self_ty.kind else {
                continue;
            };
            if !matches!(path.res, Some(hir::Res::Def(def_id)) if def_id == receiver_def) {
                continue;
            }
            self.push_generics(&impl_item.generics);
            for impl_item in impl_item.items {
                let hir::ImplItemKind::Method(function) = impl_item.kind else {
                    continue;
                };
                if function.sig.name == *method {
                    let signature = self.function_signature(&function)?;
                    let Some((_, result)) = self.instantiate_call(&signature, actuals)? else {
                        self.pop_generics();
                        return Err(Error::from("method arguments do not match its signature"));
                    };
                    self.pop_generics();
                    return Ok((impl_item.def_id, result));
                }
            }
            self.pop_generics();
        }
        Err(Error::from(format!("method `{method}` was not found")))
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
            hir::PatKind::Lit(lit) => self.require_same(&ty, &self.literal_ty(lit))?,
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
                let struct_ty = if path.segments.is_empty() { ty.clone() } else { self.path_ty(path)? };
                self.require_same_adt(&ty, &struct_ty, "struct pattern")?;
                for field in fields {
                    let field_ty = self.field_ty(&struct_ty, &field.name)?;
                    self.bind_pattern(&field.pat, field_ty)?;
                }
            }
            hir::PatKind::TupleStruct(path, patterns) => {
                let (enum_ty, payloads) = self.variant_payload_types(path)?;
                self.require_same_adt(&ty, &enum_ty, "tuple struct pattern")?;
                if patterns.len() != payloads.len() {
                    return Err(Error::from("tuple struct pattern arity does not match variant"));
                }
                for (pattern, payload) in patterns.iter().zip(payloads) {
                    self.bind_pattern(pattern, payload)?;
                }
            }
            hir::PatKind::Variant(path) => {
                let (enum_ty, payloads) = self.variant_payload_types(path)?;
                self.require_same_adt(&ty, &enum_ty, "variant pattern")?;
                if !payloads.is_empty() {
                    return Err(Error::from("payload variant requires a tuple or struct pattern"));
                }
            }
        }
        Ok(())
    }

    fn field_ty(&mut self, receiver: &Ty, field: &hir::Symbol) -> Result<Ty> {
        let TyKind::Adt(adt, args) = &receiver.kind else {
            return Err(Error::from("field access requires a struct"));
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
        let ty = self.check_type_expr(&field_def.ty)?;
        Ok(self.substitute_params(ty, args))
    }

    fn variant_payload_types(&mut self, path: &hir::Path) -> Result<(Ty, Vec<Ty>)> {
        let Some(hir::Res::Def(variant_id)) = path.res else {
            return Err(Error::from("variant pattern is unresolved"));
        };
        for item in self.program.items.clone() {
            let hir::ItemKind::Enum(def) = &item.kind else { continue };
            let Some(variant) = def.variants.iter().find(|variant| variant.def_id == variant_id) else { continue };
            let enum_ty = self.enum_item_ty(&item, path)?;
            let args = match &enum_ty.kind { TyKind::Adt(_, args) => args.clone(), _ => Vec::new() };
            let Some(payload) = &variant.payload else { return Ok((enum_ty, Vec::new())) };
            let payload = self.check_type_expr(payload)?;
            let payload = self.substitute_params(payload, &args);
            let payloads = match payload.kind {
                TyKind::Tuple(fields) => fields.into_iter().map(|field| *field).collect(),
                _ => vec![payload],
            };
            return Ok((enum_ty, payloads));
        }
        Err(Error::from("variant definition was not found"))
    }

    fn substitute_params(&self, ty: Ty, args: &[GenericArg]) -> Ty {
        match ty.kind {
            TyKind::Param(param) => match args.get(param.index as usize) {
                Some(GenericArg::Type(ty)) => ty.clone(),
                Some(GenericArg::Const(_) | GenericArg::Lifetime(_)) | None => {
                    Ty { kind: TyKind::Param(param) }
                }
            },
            TyKind::Tuple(fields) => Ty { kind: TyKind::Tuple(fields.into_iter().map(|field| Box::new(self.substitute_params(*field, args))).collect()) },
            TyKind::Array(inner, length) => Ty { kind: TyKind::Array(Box::new(self.substitute_params(*inner, args)), length) },
            TyKind::Slice(inner) => Ty { kind: TyKind::Slice(Box::new(self.substitute_params(*inner, args))) },
            TyKind::Ref(region, inner, mutable) => Ty { kind: TyKind::Ref(region, Box::new(self.substitute_params(*inner, args)), mutable) },
            TyKind::RawPtr(mutability) => Ty { kind: TyKind::RawPtr(ty::TypeAndMut { ty: Box::new(self.substitute_params(*mutability.ty, args)), mutbl: mutability.mutbl }) },
            kind => Ty { kind },
        }
    }

    fn check_intrinsic(&mut self, call: &hir::IntrinsicCallExpr) -> Result<Ty> {
        let arg_types = call
            .callargs
            .iter()
            .map(|arg| self.check_expr(&arg.value))
            .collect::<Result<Vec<_>>>()?;
        use fp_core::intrinsics::IntrinsicCallKind;
        Ok(match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println | IntrinsicCallKind::Panic => Ty { kind: TyKind::Tuple(Vec::new()) },
            IntrinsicCallKind::Format => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) },
            IntrinsicCallKind::Len => Ty::uint(ty::UintTy::U64),
            IntrinsicCallKind::Slice => {
                let Some(base) = arg_types.first() else {
                    return Err(Error::from("slice intrinsic requires a base expression"));
                };
                match &base.kind {
                    TyKind::Array(inner, _) | TyKind::Slice(inner) => Ty { kind: TyKind::Slice(inner.clone()) },
                    _ => return Err(Error::from("slice intrinsic base must be an array or slice")),
                }
            }
            IntrinsicCallKind::DebugAssertions
            | IntrinsicCallKind::FsExists
            | IntrinsicCallKind::FsIsDir
            | IntrinsicCallKind::FsIsFile
            | IntrinsicCallKind::EnvVarExists
            | IntrinsicCallKind::HasField
            | IntrinsicCallKind::HasMethod => Ty::bool(),
            IntrinsicCallKind::Input
            | IntrinsicCallKind::FsReadToString
            | IntrinsicCallKind::FsReadDir
            | IntrinsicCallKind::FsWalkDir
            | IntrinsicCallKind::FsGlob
            | IntrinsicCallKind::EnvCurrentDir
            | IntrinsicCallKind::EnvTempDir
            | IntrinsicCallKind::EnvHomeDir
            | IntrinsicCallKind::EnvVar
            | IntrinsicCallKind::PathJoin
            | IntrinsicCallKind::PathParent
            | IntrinsicCallKind::PathFileName
            | IntrinsicCallKind::PathExtension
            | IntrinsicCallKind::PathStem
            | IntrinsicCallKind::PathNormalize
            | IntrinsicCallKind::IoReadStdinToString
            | IntrinsicCallKind::YamlToJson
            | IntrinsicCallKind::JsonParse
            | IntrinsicCallKind::ProcMacroTokenStreamToString => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) },
            IntrinsicCallKind::PathIsAbsolute => Ty::bool(),
            IntrinsicCallKind::TimeNow => Ty::float(ty::FloatTy::F64),
            IntrinsicCallKind::CatchUnwind => Ty::bool(),
            IntrinsicCallKind::CatchUnwindResult => {
                let Some(value) = arg_types.first().cloned() else {
                    return Err(Error::from("catch_unwind_result requires a callable argument"));
                };
                Ty { kind: TyKind::Tuple(vec![Box::new(Ty::bool()), Box::new(value)]) }
            }
            IntrinsicCallKind::Spawn | IntrinsicCallKind::Select => {
                let Some(value) = arg_types.first() else {
                    return Err(Error::from(format!("{:?} intrinsic requires an argument", call.kind)));
                };
                value.clone()
            }
            IntrinsicCallKind::Join => {
                if arg_types.len() == 1 {
                    arg_types[0].clone()
                } else if arg_types.is_empty() {
                    return Err(Error::from("join intrinsic requires an argument"));
                } else {
                    Ty { kind: TyKind::Tuple(arg_types.into_iter().map(Box::new).collect()) }
                }
            }
            IntrinsicCallKind::SizeOf | IntrinsicCallKind::FieldCount | IntrinsicCallKind::MethodCount => Ty::uint(ty::UintTy::U64),
            IntrinsicCallKind::FieldNameAt | IntrinsicCallKind::TypeName | IntrinsicCallKind::ProcMacroTokenStreamFromStr => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) },
            IntrinsicCallKind::FieldType | IntrinsicCallKind::VecType => return Err(Error::from("type-valued intrinsic has no HIR type representation")),
            IntrinsicCallKind::TypeOf | IntrinsicCallKind::CreateStruct | IntrinsicCallKind::AddField | IntrinsicCallKind::BuildType => {
                return Err(Error::from("type-valued intrinsic typing is not implemented"));
            }
            IntrinsicCallKind::FsWriteString
            | IntrinsicCallKind::FsAppendString
            | IntrinsicCallKind::FsCreateDirAll
            | IntrinsicCallKind::FsRemoveFile
            | IntrinsicCallKind::FsRemoveDirAll
            | IntrinsicCallKind::IoWriteStdout
            | IntrinsicCallKind::IoWriteStderr
            | IntrinsicCallKind::TestCommandMockReset
            | IntrinsicCallKind::TestCommandMockPush
            | IntrinsicCallKind::TestCommandMockApply
            | IntrinsicCallKind::Sleep
            | IntrinsicCallKind::Yield
            | IntrinsicCallKind::CompileWarning => self.unit_ty(),
            IntrinsicCallKind::CompileError => return Err(Error::from("compile_error intrinsic requested an error")),
            _ => return Err(Error::from(format!("intrinsic `{:?}` has no HIR type rule", call.kind))),
        })
    }

    fn require_same(&self, lhs: &Ty, rhs: &Ty) -> Result<()> {
        if lhs == rhs
            || matches!(lhs.kind, TyKind::Never)
            || matches!(rhs.kind, TyKind::Never)
        {
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
        if actual_def.did != expected_def.did || actual_args != expected_args {
            return Err(Error::from(format!("{context} does not match scrutinee type")));
        }
        Ok(())
    }

    fn unit_ty(&self) -> Ty {
        Ty { kind: TyKind::Tuple(Vec::new()) }
    }

    fn literal_ty(&self, literal: &hir::Lit) -> Ty {
        match literal { hir::Lit::Bool(_) => Ty::bool(), hir::Lit::Char(_) => Ty::char(), hir::Lit::Integer(_) => Ty::int(ty::IntTy::I64), hir::Lit::Float(_) => Ty::float(ty::FloatTy::F64), hir::Lit::Str(_) => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) }, hir::Lit::Null => Ty::never() }
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
        "str" | "string" => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) },
        _ => return None,
    })
}

fn primitive_ty(primitive: TypePrimitive) -> Ty {
    match primitive {
        TypePrimitive::Bool => Ty::bool(),
        TypePrimitive::Char => Ty::char(),
        TypePrimitive::Int(int) => match int { TypeInt::I8 => Ty::int(ty::IntTy::I8), TypeInt::I16 => Ty::int(ty::IntTy::I16), TypeInt::I32 => Ty::int(ty::IntTy::I32), TypeInt::I64 => Ty::int(ty::IntTy::I64), TypeInt::I128 => Ty::int(ty::IntTy::I128), TypeInt::U8 => Ty::uint(ty::UintTy::U8), TypeInt::U16 => Ty::uint(ty::UintTy::U16), TypeInt::U32 => Ty::uint(ty::UintTy::U32), TypeInt::U64 => Ty::uint(ty::UintTy::U64), TypeInt::U128 => Ty::uint(ty::UintTy::U128), TypeInt::BigInt => Ty::int(ty::IntTy::I128) },
        TypePrimitive::Decimal(decimal) => Ty::float(match decimal { DecimalType::F32 => ty::FloatTy::F32, _ => ty::FloatTy::F64 }),
        TypePrimitive::String => Ty { kind: TyKind::Slice(Box::new(Ty::int(ty::IntTy::I8))) },
        TypePrimitive::List => Ty { kind: TyKind::Slice(Box::new(Ty::never())) },
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

        let (_, results) = HirTypeChecker::new(program).check().expect("HIR type check");
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

        let (_, results) = HirTypeChecker::new(program).check().expect("HIR type check");
        assert_eq!(results.pat_types.get(&8), Some(&Ty::int(ty::IntTy::I64)));
    }
}
