use super::*;

#[derive(Clone)]
pub(super) struct ClosureInfo {
    env_struct_ident: ast::Ident,
    env_struct_ty: ast::Ty,
    call_fn_ident: ast::Ident,
}

#[derive(Clone)]
pub(super) struct Capture {
    name: ast::Ident,
    ty: ast::Ty,
}

pub(super) struct ClosureLowering {
    counter: usize,
    symbol_prefix: String,
    function_infos: HashMap<String, ClosureInfo>,
    struct_infos: HashMap<String, ClosureInfo>,
    variable_infos: HashMap<String, ClosureInfo>,
    pub(super) generated_items: Vec<ast::Item>,
    pub(super) diagnostics: Vec<Diagnostic>,
    /// Struct name -> (field name, declared field type), collected once up
    /// front over the whole package — used only to derive a closure
    /// argument's real parameter type at its call site (see
    /// `closure_param_ty_for_invoke`), never mutated afterward.
    pub(super) struct_field_types: HashMap<String, Vec<(String, ast::Ty)>>,
    /// The enclosing top-level function's own parameter name -> declared
    /// type, while rewriting its body (see `rewrite_usage`) — the other
    /// half of the same closure-argument-type derivation. Does not cover
    /// `impl` method receivers/params or `let`-bound locals; a receiver
    /// expression built from those simply doesn't resolve here, same as
    /// any other unhandled shape.
    current_param_types: HashMap<String, ast::Ty>,
}
// TODO: move to new file
impl ClosureLowering {
    pub(super) fn new(symbol_prefix: String) -> Self {
        Self {
            counter: 0,
            symbol_prefix,
            function_infos: HashMap::new(),
            struct_infos: HashMap::new(),
            variable_infos: HashMap::new(),
            generated_items: Vec::new(),
            diagnostics: Vec::new(),
            struct_field_types: HashMap::new(),
            current_param_types: HashMap::new(),
        }
    }

    pub(super) fn reserve_generated_names(&mut self, items: &[ast::Item]) {
        for item in items {
            self.reserve_generated_names_in_item(item);
        }
    }

    fn reserve_generated_names_in_item(&mut self, item: &ast::Item) {
        match item.kind() {
            ast::ItemKind::Module(module) => {
                for item in &module.items {
                    self.reserve_generated_names_in_item(item);
                }
            }
            ast::ItemKind::DefFunction(function) => {
                self.reserve_generated_names_in_block(&function.body)
            }
            ast::ItemKind::DefStruct(definition) => {
                self.reserve_generated_name(definition.name.as_str())
            }
            ast::ItemKind::DefConst(definition) => {
                self.reserve_generated_names_in_expr(definition.value.as_ref())
            }
            ast::ItemKind::DefStatic(definition) => {
                if !attrs_has_name(&definition.attrs, "host") {
                    self.reserve_generated_names_in_expr(definition.value.as_ref())
                }
            }
            ast::ItemKind::Expr(expression) => self.reserve_generated_names_in_expr(expression),
            _ => {}
        }
    }

    fn reserve_generated_names_in_block(&mut self, block: &ast::ExprBlock) {
        for statement in &block.stmts {
            match statement {
                ast::BlockStmt::Expr(statement) => {
                    self.reserve_generated_names_in_expr(statement.expr.as_ref())
                }
                ast::BlockStmt::Let(statement) => {
                    if let Some(init) = &statement.init {
                        self.reserve_generated_names_in_expr(init);
                    }
                }
                ast::BlockStmt::Defer(statement) => {
                    self.reserve_generated_names_in_expr(statement.expr.as_ref())
                }
                ast::BlockStmt::Item(item) => self.reserve_generated_names_in_item(item),
                ast::BlockStmt::Noop => {}
            }
        }
    }

    fn reserve_generated_names_in_expr(&mut self, expr: &ast::Expr) {
        match expr.kind() {
            ast::ExprKind::Struct(struct_expr) => {
                if let Some(name) = extract_ident(struct_expr.name.as_ref()) {
                    self.reserve_generated_name(name.as_str());
                }
                for field in &struct_expr.fields {
                    if let Some(value) = &field.value {
                        self.reserve_generated_names_in_expr(value);
                    }
                }
            }
            ast::ExprKind::Value(value) => {
                if let ast::Value::Struct(structure) = value.as_ref() {
                    self.reserve_generated_name(structure.ty.name.as_str());
                }
            }
            ast::ExprKind::Block(block) => self.reserve_generated_names_in_block(block),
            ast::ExprKind::Invoke(invoke) => {
                if let ast::ExprInvokeTarget::Expr(target) = &invoke.target {
                    self.reserve_generated_names_in_expr(target);
                }
                for argument in &invoke.args {
                    self.reserve_generated_names_in_expr(argument);
                }
            }
            ast::ExprKind::Let(let_expr) => self.reserve_generated_names_in_expr(&let_expr.expr),
            ast::ExprKind::Closure(closure) => self.reserve_generated_names_in_expr(&closure.body),
            ast::ExprKind::BinOp(binop) => {
                self.reserve_generated_names_in_expr(&binop.lhs);
                self.reserve_generated_names_in_expr(&binop.rhs);
            }
            ast::ExprKind::Paren(paren) => self.reserve_generated_names_in_expr(&paren.expr),
            _ => {}
        }
    }

    fn reserve_generated_name(&mut self, name: &str) {
        let prefix = format!("__Closure{}_", self.symbol_prefix);
        let Some(index) = name
            .strip_prefix(&prefix)
            .and_then(|suffix| suffix.parse::<usize>().ok())
        else {
            return;
        };
        self.counter = self.counter.max(index.saturating_add(1));
    }

    /// One-time pre-pass collecting every struct's declared field types,
    /// so `closure_param_ty_for_invoke` can resolve a field-access chain
    /// (`node.stats`) back to its real type without a full type checker.
    pub(super) fn collect_struct_field_types(&mut self, items: &[ast::Item]) {
        for item in items {
            match item.kind() {
                ast::ItemKind::Module(module) => self.collect_struct_field_types(&module.items),
                ast::ItemKind::DefStruct(def) => {
                    let fields = def
                        .value
                        .fields
                        .iter()
                        .map(|field| (field.name.as_str().to_string(), field.value.clone()))
                        .collect();
                    self.struct_field_types
                        .insert(def.name.as_str().to_string(), fields);
                }
                _ => {}
            }
        }
    }

    /// Best-effort, deliberately narrow structural type lookup for a
    /// receiver expression — not a general type checker, just enough to
    /// resolve the two shapes real call sites need: a tracked function
    /// parameter's own declared type, and field access through a known
    /// struct definition. Returns `None` for anything else.
    fn infer_static_expr_ty(&self, expr: &ast::Expr) -> Option<ast::Ty> {
        match expr.kind() {
            ast::ExprKind::Name(name) => self
                .current_param_types
                .get(name.as_ident()?.as_str())
                .cloned(),
            ast::ExprKind::FieldAccess(select) => {
                let base_ty = self.infer_static_expr_ty(&select.obj)?;
                let struct_name = Self::struct_name_of(&base_ty)?;
                self.struct_field_types
                    .get(&struct_name)?
                    .iter()
                    .find(|(name, _)| name == select.field.as_str())
                    .map(|(_, ty)| ty.clone())
            }
            _ => None,
        }
    }

    /// The struct name a type ultimately names, stripping reference
    /// wrappers and unwrapping the `Ty::Expr(Name(..))` shape a bare
    /// (non-generic) struct reference parses as.
    fn struct_name_of(ty: &ast::Ty) -> Option<String> {
        match ty {
            ast::Ty::Reference(r) => Self::struct_name_of(&r.ty),
            ast::Ty::Struct(s) => Some(s.name.as_str().to_string()),
            ast::Ty::Expr(expr) => match expr.kind() {
                ast::ExprKind::Name(name) => name.as_ident().map(|i| i.as_str().to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    /// The `index`-th generic type argument of a parameterized type
    /// reference (`Option<T>`'s `T` is index 0, `Result<T, E>`'s `E` is
    /// index 1) — generic types parse as `Ty::Expr` wrapping a
    /// path-based Name whose segment carries the type args
    /// directly (see `fp-lang/src/ast/types.rs`'s `parse_simple_type`).
    fn generic_type_arg_at(ty: &ast::Ty, index: usize) -> Option<ast::Ty> {
        let ast::Ty::Expr(expr) = ty else {
            return None;
        };
        let ast::ExprKind::Name(ast::Name { path, .. }) = expr.kind() else {
            return None;
        };
        path.segments.last()?.args.get(index).cloned()
    }

    /// Resolves a call receiver's static type, peeling through at most one
    /// trailing `.as_ref()`/`.as_mut()` — common right before a
    /// closure-taking method (`opt.as_ref().map_or(..)`) — and reporting
    /// whether the generic argument later extracted from it should be
    /// reference-wrapped to match (`.as_ref()` turns `Option<T>` access
    /// into effectively `Option<&T>` for the closure's purposes).
    fn receiver_ty_for_closure_arg(&self, expr: &ast::Expr) -> (Option<ast::Ty>, bool) {
        if let ast::ExprKind::Invoke(invoke) = expr.kind() {
            if let ast::ExprInvokeTarget::Method(sel) = &invoke.target {
                if invoke.args.is_empty() && matches!(sel.field.name.as_str(), "as_ref" | "as_mut")
                {
                    return (self.infer_static_expr_ty(&sel.obj), true);
                }
            }
        }
        (self.infer_static_expr_ty(expr), false)
    }

    /// Derives the real parameter type for a closure passed to one of the
    /// handful of `Option`/`Result` methods whose Kotlin codegen needs a
    /// literal closure (see `fp-kotlin`'s `map_or`/`map_err` special
    /// cases) — `None` if the receiver's type isn't structurally
    /// resolvable, or the method isn't one of these.
    /// Returns `(param_ty, ret_ty)` for the closure argument of a
    /// `map_or`/`map`/`map_err`/`and_then` call — the closure's own return
    /// type also needs to be a real type, not `Unknown`: leaving it
    /// `Unknown` reproduces the exact same "silently resolves to a null
    /// placeholder" failure mode this whole derivation exists to avoid,
    /// just one step later (at the synthetic `__closureN_call` function's
    /// own return position instead of its parameter). The full body
    /// wouldn't need type inference to get this right in general, but
    /// `map_or`'s `default` argument is frequently a literal with an
    /// obvious static type, which covers the common case cheaply.
    fn closure_param_ty_for_invoke(
        &self,
        invoke: &ast::ExprInvoke,
    ) -> (Option<ast::Ty>, Option<ast::Ty>) {
        let ast::ExprInvokeTarget::Method(sel) = &invoke.target else {
            return (None, None);
        };
        let arg_index = match sel.field.name.as_str() {
            "map_or" | "map" | "and_then" => 0,
            "map_err" => 1,
            _ => return (None, None),
        };
        if sel.field.name.as_str() == "map_err" {
            // Kotlin's Result exposes failures as Throwable. The source
            // error parameter can be unconstrained and therefore lift as
            // Any, but a Kotlin mapError callback must never receive Any?.
            return (
                Some(ast::Ty::path(ast::Path::plain(vec![ast::Ident::new(
                    "Throwable",
                )]))),
                None,
            );
        }
        let (receiver_ty, by_ref) = self.receiver_ty_for_closure_arg(&sel.obj);
        let Some(inner) = receiver_ty.and_then(|ty| Self::generic_type_arg_at(&ty, arg_index))
        else {
            return (None, None);
        };
        let param_ty = if by_ref {
            ast::Ty::Reference(
                ast::TypeReference {
                    ty: Box::new(inner),
                    mutability: None,
                    lifetime: None,
                }
                .into(),
            )
        } else {
            inner
        };
        let ret_ty = if sel.field.name.as_str() == "map_or" {
            invoke.args.first().and_then(Self::literal_expr_ty)
        } else {
            None
        };
        (Some(param_ty), ret_ty)
    }

    /// The static type of an integer/float/bool/string literal expression
    /// — used only as a best-effort return-type hint (see
    /// `closure_param_ty_for_invoke`), not a general literal-type table.
    fn literal_expr_ty(expr: &ast::Expr) -> Option<ast::Ty> {
        let ast::ExprKind::Value(value) = expr.kind() else {
            return None;
        };
        Some(match value.as_ref() {
            ast::Value::Int(_) => ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I64)),
            ast::Value::Decimal(_) => {
                ast::Ty::Primitive(ast::TypePrimitive::Decimal(ast::DecimalType::F64))
            }
            ast::Value::Bool(_) => ast::Ty::Primitive(ast::TypePrimitive::Bool),
            ast::Value::String(_) => ast::Ty::Primitive(ast::TypePrimitive::String),
            _ => return None,
        })
    }

    fn add_error(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    fn block_stmt_expr(expr: ast::Expr, has_value: bool) -> ast::BlockStmt {
        ast::BlockStmt::Expr(ast::BlockStmtExpr::new(expr).with_semicolon(!has_value))
    }

    fn desugar_block_defer(&mut self, block: &mut ast::ExprBlock) -> bool {
        let defer_index = block
            .stmts
            .iter()
            .position(|stmt| matches!(stmt, ast::BlockStmt::Defer(_)));
        let Some(index) = defer_index else {
            return false;
        };
        let ast::BlockStmt::Defer(stmt_defer) = block.stmts.remove(index) else {
            return false;
        };
        let suffix = block.stmts.split_off(index);
        let has_value = match suffix.last() {
            Some(ast::BlockStmt::Expr(expr_stmt)) => expr_stmt.has_value(),
            _ => false,
        };
        let wrapped = ast::Expr::new(ast::ExprKind::Try(ast::ExprTry {
            span: stmt_defer.span(),
            expr: Box::new(ast::Expr::new(ast::ExprKind::Block(
                ast::ExprBlock::new_stmts(suffix),
            ))),
            catches: Vec::new(),
            elze: None,
            finally: Some(stmt_defer.expr),
        }));
        block.stmts.push(Self::block_stmt_expr(wrapped, has_value));
        true
    }

    pub(super) fn find_and_transform_functions(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => {
                    self.find_and_transform_functions(&mut module.items)?;
                }
                ast::ItemKind::DefFunction(func) => {
                    let previous = std::mem::replace(
                        &mut self.current_param_types,
                        func.sig
                            .params
                            .iter()
                            .map(|param| (param.name.as_str().to_string(), param.ty.clone()))
                            .collect(),
                    );
                    let info = self.transform_function(func)?;
                    self.current_param_types = previous;
                    if let Some(info) = info {
                        self.function_infos
                            .insert(func.name.as_str().to_string(), info.clone());
                        self.struct_infos
                            .insert(info.env_struct_ident.as_str().to_string(), info);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn transform_function(
        &mut self,
        func: &mut ast::ItemDefFunction,
    ) -> Result<Option<ClosureInfo>> {
        if let Some(last_expr) = func.body.last_expr_mut()
            && let Some(info) = self.transform_closure_expr(last_expr)?
        {
            let env_ret_ty = info.env_struct_ty.clone();

            if let Some(ty_fn) = func.ty.as_mut() {
                ty_fn.ret_ty = Some(Box::new(env_ret_ty.clone()));
            }

            if func.ty.is_none() {
                func.ty = Some(ast::TypeFunction {
                    params: func
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    generics_params: func.sig.generics_params.clone(),
                    ret_ty: Some(Box::new(env_ret_ty.clone())),
                });
            }

            if func.ty_annotation.is_some() || func.ty.is_some() {
                func.ty_annotation = func
                    .ty
                    .as_ref()
                    .map(|ty_fn| ast::Ty::Function(ty_fn.clone()));
            }

            if let Some(ret_slot) = func.sig.ret_ty.as_mut() {
                *ret_slot = env_ret_ty.clone();
            } else {
                func.sig.ret_ty = Some(env_ret_ty.clone());
            }

            return Ok(Some(info));
        }

        Ok(None)
    }

    /// `transform_closure_expr` only decomposes a closure literal that
    /// already carries a `Ty::Function` type — true for a function's own
    /// tail expression (its declared return-type annotation is copied
    /// onto the tail by an earlier pass), but never true for a closure
    /// passed as a call *argument*: this pre-pass runs before typecheck,
    /// so the callee's parameter type isn't resolved yet, and previously
    /// nothing else ever gave the closure a type here either. Left
    /// unaddressed, such a closure falls through every other lowering
    /// path all the way to `transform_expr_to_hir_inner`'s
    /// `ExprKind::Closure` arm, which has no implementation and silently
    /// discards it (an empty HIR block, plus an error diagnostic nothing
    /// currently surfaces).
    ///
    /// The real parameter/return types aren't needed to decompose the
    /// closure correctly — only its *arity* is, and that's already known
    /// from the closure literal itself, with no inference required.
    /// `transform_closure_expr` already tolerates missing per-parameter
    /// and return types gracefully (falling back to `Any`/`Unknown`), so
    /// synthesizing a same-arity placeholder `Ty::Function` here is
    /// sufficient to let it decompose the closure like any other.
    fn ensure_closure_has_function_ty(
        expr: &mut ast::Expr,
        param_ty: Option<&ast::Ty>,
        ret_ty: Option<&ast::Ty>,
    ) {
        let _ = (expr, param_ty, ret_ty);
    }

    fn transform_closure_expr(&mut self, expr: &mut ast::Expr) -> Result<Option<ClosureInfo>> {
        Self::ensure_closure_has_function_ty(expr, None, None);
        let ast::ExprKind::Closure(closure_ref) = expr.kind() else {
            return Ok(None);
        };
        let fn_ty = ast::TypeFunction {
            params: closure_ref
                .params
                .iter()
                .map(|_| ast::Ty::Any(ast::TypeAny))
                .collect(),
            generics_params: Vec::new(),
            ret_ty: closure_ref.ret_ty.clone(),
        };

        let ast::ExprKind::Closure(closure) = expr.kind_mut() else {
            return Ok(None);
        };

        let mut param_names = Vec::new();
        let mut param_set = HashSet::new();
        for param in &closure.params {
            let ident = match param.kind() {
                ast::PatternKind::Ident(ident) => ident,
                ast::PatternKind::Type(typed) => match typed.pat.kind() {
                    ast::PatternKind::Ident(ident) => ident,
                    _ => {
                        self.add_error(
                            Diagnostic::error(
                                "only simple identifier parameters are supported in closures"
                                    .to_string(),
                            )
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(param.span()),
                        );
                        return Ok(None);
                    }
                },
                _ => {
                    self.add_error(
                        Diagnostic::error(
                            "only simple identifier parameters are supported in closures"
                                .to_string(),
                        )
                        .with_source_context(DIAGNOSTIC_CONTEXT)
                        .with_span(param.span()),
                    );
                    return Ok(None);
                }
            };
            {
                let name = ident.ident.name.as_str().to_string();
                param_set.insert(name.clone());
                param_names.push(name);
            }
        }

        let mut captures = self.collect_captures(closure.body.as_ref(), &param_set)?;
        for capture in &mut captures {
            if let Some(param_ty) = self.current_param_types.get(capture.name.as_str()) {
                capture.ty = param_ty.clone();
            }
        }

        let closure_id = self.counter;
        let struct_ident =
            ast::Ident::new(format!("__Closure{}_{}", self.symbol_prefix, closure_id));
        let call_ident = ast::Ident::new(format!(
            "__closure{}_{}_call",
            self.symbol_prefix, closure_id
        ));
        self.counter += 1;

        let mut struct_fields: Vec<ast::StructuralField> = captures
            .iter()
            .map(|capture| ast::StructuralField::new(capture.name.clone(), capture.ty.clone()))
            .collect();
        if struct_fields.is_empty() {
            struct_fields.push(ast::StructuralField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I8)),
            ));
        }
        let struct_decl = ast::TypeStruct {
            name: struct_ident.clone(),
            generics_params: Vec::new(),
            repr: ast::ReprOptions::default(),
            fields: struct_fields,
        };
        let env_struct_ty = ast::Ty::Struct(struct_decl.clone());

        let struct_item = ast::Item::new(ast::ItemKind::DefStruct(ast::ItemDefStruct {
            attrs: Vec::new(),
            visibility: ast::Visibility::Private,
            name: struct_ident.clone(),
            value: struct_decl.clone(),
        }));
        let env_param_ident = ast::Ident::new("__env");
        let mut fn_params = Vec::new();
        let mut fn_param_tys = Vec::new();
        fn_params.push(ast::FunctionParam::new(
            env_param_ident.clone(),
            env_struct_ty.clone(),
        ));
        fn_param_tys.push(env_struct_ty.clone());
        for (idx, name) in param_names.iter().enumerate() {
            let ty = fn_ty
                .params
                .get(idx)
                .cloned()
                .unwrap_or_else(|| ast::Ty::Any(ast::TypeAny));
            fn_params.push(ast::FunctionParam::new(
                ast::Ident::new(name.clone()),
                ty.clone(),
            ));
            fn_param_tys.push(ty);
        }

        let mut rewritten_body = (*closure.body).clone();
        let inferred_ret_ty = fn_ty.ret_ty.as_ref().and_then(|ty| {
            if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                None
            } else {
                Some(ty.as_ref().clone())
            }
        });
        let fallback_ret_ty = fn_ty.ret_ty.as_ref().and_then(|ty| {
            if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                None
            } else {
                Some(ty.as_ref().clone())
            }
        });
        let call_ret_ty = inferred_ret_ty
            .clone()
            .or(fallback_ret_ty)
            .unwrap_or_else(|| ast::Ty::Unknown(ast::TypeUnknown));

        self.rewrite_captured_usage(&mut rewritten_body, &captures, &env_param_ident);

        let mut fn_item_ast = ast::ItemDefFunction::new_simple(
            call_ident.clone(),
            ast::ExprBlock::new_expr(rewritten_body),
        );
        fn_item_ast.visibility = ast::Visibility::Private;
        fn_item_ast.sig.params = fn_params;
        fn_item_ast.sig.ret_ty = Some(call_ret_ty.clone());
        fn_item_ast.ty = Some(ast::TypeFunction {
            params: fn_param_tys.clone(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(call_ret_ty.clone())),
        });
        fn_item_ast.ty_annotation = fn_item_ast.ty.clone().map(|ty_fn| ast::Ty::Function(ty_fn));

        let fn_item = ast::Item::new(ast::ItemKind::DefFunction(fn_item_ast));

        self.generated_items.push(struct_item);
        self.generated_items.push(fn_item);

        let mut fields = Vec::new();
        for capture in &captures {
            let value_expr = ast::Expr::ident(capture.name.clone());
            fields.push(ast::ExprField::new(capture.name.clone(), value_expr));
        }
        if fields.is_empty() {
            let value_expr = ast::Expr::value(ast::Value::int(0));
            fields.push(ast::ExprField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                value_expr,
            ));
        }

        let struct_name_expr = ast::Expr::ident(struct_ident.clone());

        let mut struct_expr = ast::Expr::new(ast::ExprKind::Struct(ast::ExprStruct {
            span: fp_core::span::Span::null(),
            name: struct_name_expr.into(),
            fields,
            update: None,
        }));

        *expr = struct_expr;

        let info = ClosureInfo {
            env_struct_ident: struct_ident,
            env_struct_ty,
            call_fn_ident: call_ident,
        };

        Ok(Some(info))
    }

    pub(super) fn rewrite_usage(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
                ast::ItemKind::DefFunction(func) => {
                    let previous = std::mem::replace(
                        &mut self.current_param_types,
                        func.sig
                            .params
                            .iter()
                            .map(|param| (param.name.as_str().to_string(), param.ty.clone()))
                            .collect(),
                    );
                    self.rewrite_in_block(&mut func.body)?;
                    self.current_param_types = previous;
                }
                ast::ItemKind::DefConst(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ast::ItemKind::DefStatic(def) if !attrs_has_name(&def.attrs, "host") => {
                    self.rewrite_in_expr(def.value.as_mut())?
                }
                ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
                _ => {}
            }
        }
        Ok(())
    }
    // FIXME: rewrite things is sus, you should be finishing this during a pas
    pub(super) fn rewrite_in_expr(&mut self, expr: &mut ast::Expr) -> Result<()> {
        if expand_intrinsic_collection(expr) {
            return self.rewrite_in_expr(expr);
        }

        // Normalization wraps expressions recovered from a scoped context in
        // `Closured`. The wrapper is compile-time bookkeeping, while native
        // closure lowering must see the enclosed lambda itself.
        if let ast::ExprKind::Closured(closured) = expr.kind_mut() {
            let inner = (*closured.expr).clone();
            *expr = inner;
            return self.rewrite_in_expr(expr);
        }

        if let Some(info) = self.transform_closure_expr(expr)? {
            self.struct_infos
                .insert(info.env_struct_ident.as_str().to_string(), info);
            return self.rewrite_in_expr(expr);
        }

        match expr.kind_mut() {
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                while self.desugar_block_defer(block) {
                    self.rewrite_in_expr(expr)?;
                    return Ok(());
                }
                if let Some(last) = block.last_expr_mut() {
                    self.rewrite_in_expr(last)?;
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.rewrite_in_expr(expr_if.cond.as_mut())?;
                self.rewrite_in_expr(expr_if.then.as_mut())?;
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_in_expr(elze)?;
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.rewrite_in_expr(expr_loop.body.as_mut())?,
            ast::ExprKind::While(expr_while) => {
                self.rewrite_in_expr(expr_while.cond.as_mut())?;
                self.rewrite_in_expr(expr_while.body.as_mut())?;
            }
            ast::ExprKind::With(expr_with) => {
                self.rewrite_in_expr(expr_with.context.as_mut())?;
                self.rewrite_in_expr(expr_with.body.as_mut())?;
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.rewrite_in_expr(const_block.expr.as_mut())?;
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_in_expr(case.cond.as_mut())?;
                    self.rewrite_in_expr(case.body.as_mut())?;
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.rewrite_in_expr(expr_for.iter.as_mut())?;
                self.rewrite_in_expr(expr_for.body.as_mut())?;
            }
            ast::ExprKind::Let(expr_let) => {
                Self::ensure_closure_has_function_ty(expr_let.expr.as_mut(), None, None);
                self.rewrite_in_expr(expr_let.expr.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(expr_let.expr.as_ref()) {
                    let mut names = Vec::new();
                    collect_pattern_idents(expr_let.pat.as_ref(), &mut names);
                    for name in names {
                        self.variable_infos.insert(name, info.clone());
                    }
                    expr_let.pat = Box::new(ast::Pattern::from(ast::PatternKind::Type(
                        ast::PatternType::new((*expr_let.pat).clone(), info.env_struct_ty),
                    )));
                }
            }
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                if let Some(last) = q.block.clone().last_expr_mut() {
                    let mut last_clone = last.clone();
                    self.rewrite_in_expr(&mut last_clone)?;
                }
            }
            ast::ExprKind::Splice(s) => {
                self.rewrite_in_expr(s.token.as_mut())?;
            }
            ast::ExprKind::Invoke(invoke) => {
                // A closure literal passed as a call argument (as opposed
                // to a function's own tail expression, whose declared
                // return-type annotation an earlier pass already copies
                // onto it) never carries a `Ty::Function` type at this
                // pre-typecheck stage — give it one so
                // `transform_closure_expr` (called from `rewrite_in_expr`
                // below) can still decompose it instead of silently
                // discarding it later. Prefer the real, structurally
                // derived parameter type when this call is one
                // `closure_param_ty_for_invoke` covers; computed once per
                // invoke (not per arg, since it depends on the whole call,
                // not any individual argument).
                let (closure_param_ty, closure_ret_ty) = self.closure_param_ty_for_invoke(invoke);
                for arg in &mut invoke.args {
                    // Scoped to exactly this position (not applied to
                    // every closure `rewrite_in_expr` visits) since
                    // closures still nested inside an unexpanded macro's
                    // argument tokens must not be touched here.
                    Self::ensure_closure_has_function_ty(
                        arg,
                        closure_param_ty.as_ref(),
                        closure_ret_ty.as_ref(),
                    );
                    self.rewrite_in_expr(arg)?;
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.rewrite_in_expr(target.as_mut())?;
                        if let Some(info) = self.closure_info_from_expr(target.as_ref()) {
                            let call_name = ast::Name::ident(info.call_fn_ident.clone());
                            let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                            new_args.push(*target.clone());
                            new_args.extend(invoke.args.iter().cloned());
                            invoke.target = ast::ExprInvokeTarget::Function(call_name);
                            invoke.args = new_args;
                        }
                    }
                    ast::ExprInvokeTarget::Function(name) => {
                        if let Some(ident) = name.as_ident() {
                            let info = self
                                .variable_infos
                                .get(ident.as_str())
                                .cloned()
                                .or_else(|| self.struct_infos.get(ident.as_str()).cloned());
                            if let Some(info) = info {
                                let env_expr = ast::Expr::new(ast::ExprKind::Name(name.clone()));
                                let call_name = ast::Name::ident(info.call_fn_ident.clone());
                                let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                                new_args.push(env_expr);
                                new_args.extend(invoke.args.iter().cloned());
                                invoke.target = ast::ExprInvokeTarget::Function(call_name);
                                invoke.args = new_args;
                            }
                        }
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.rewrite_in_expr(await_expr.base.as_mut())?;
            }
            ast::ExprKind::Async(async_expr) => {
                self.rewrite_in_expr(async_expr.expr.as_mut())?;
            }
            ast::ExprKind::Assign(assign) => {
                self.rewrite_in_expr(assign.target.as_mut())?;
                self.rewrite_in_expr(assign.value.as_mut())?;
            }
            ast::ExprKind::FieldAccess(select) => self.rewrite_in_expr(select.obj.as_mut())?,
            ast::ExprKind::Struct(struct_expr) => {
                self.rewrite_in_expr(struct_expr.name.as_mut())?;
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_in_expr(array_repeat.elem.as_mut())?;
                self.rewrite_in_expr(array_repeat.len.as_mut())?;
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Reference(reference) => {
                self.rewrite_in_expr(reference.referee.as_mut())?;
            }
            ast::ExprKind::Dereference(deref) => {
                self.rewrite_in_expr(deref.referee.as_mut())?;
            }
            ast::ExprKind::Cast(cast) => self.rewrite_in_expr(cast.expr.as_mut())?,
            ast::ExprKind::Index(index) => {
                self.rewrite_in_expr(index.obj.as_mut())?;
                self.rewrite_in_expr(index.index.as_mut())?;
            }
            ast::ExprKind::BinOp(binop) => {
                self.rewrite_in_expr(binop.lhs.as_mut())?;
                self.rewrite_in_expr(binop.rhs.as_mut())?;
            }
            ast::ExprKind::UnOp(unop) => self.rewrite_in_expr(unop.val.as_mut())?,
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.rewrite_in_expr(start.as_mut())?;
                }
                if let Some(end) = range.end.as_mut() {
                    self.rewrite_in_expr(end.as_mut())?;
                }
                if let Some(step) = range.step.as_mut() {
                    self.rewrite_in_expr(step.as_mut())?;
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => {
                self.rewrite_in_expr(expr_try.expr.as_mut())?;
                for catch in &mut expr_try.catches {
                    self.rewrite_in_expr(catch.body.as_mut())?;
                }
                if let Some(elze) = expr_try.elze.as_mut() {
                    self.rewrite_in_expr(elze.as_mut())?;
                }
                if let Some(finally) = expr_try.finally.as_mut() {
                    self.rewrite_in_expr(finally.as_mut())?;
                }
            }
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.rewrite_in_expr(expr.as_mut())?,
                ast::Value::Function(func) => self.rewrite_in_expr(func.body.as_mut())?,
                _ => {}
            },
            ast::ExprKind::Splat(splat) => self.rewrite_in_expr(splat.iter.as_mut())?,
            ast::ExprKind::SplatDict(dict) => self.rewrite_in_expr(dict.dict.as_mut())?,
            ast::ExprKind::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.rewrite_in_expr(arg)?;
                }
                for kwarg in &mut call.kwargs {
                    self.rewrite_in_expr(&mut kwarg.value)?;
                }
            }
            ast::ExprKind::Paren(paren) => self.rewrite_in_expr(paren.expr.as_mut())?,
            ast::ExprKind::IntrinsicContainer(_) => {
                unreachable!("intrinsic collections should have been expanded")
            }
            ast::ExprKind::Name(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::Closure(_) => {}
        }
        Ok(())
    }

    fn rewrite_in_block(&mut self, block: &mut ast::ExprBlock) -> Result<()> {
        for stmt in &mut block.stmts {
            self.rewrite_in_stmt(stmt)?;
        }
        while self.desugar_block_defer(block) {
            for stmt in &mut block.stmts {
                self.rewrite_in_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn rewrite_in_stmt(&mut self, stmt: &mut ast::BlockStmt) -> Result<()> {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.rewrite_in_expr(expr_stmt.expr.as_mut())?,
            ast::BlockStmt::Defer(stmt_defer) => self.rewrite_in_expr(stmt_defer.expr.as_mut())?,
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    // Local closures do not have a call-site parameter
                    // from which to infer a signature. Preserve explicit
                    // parameter annotations so they can be lowered into a
                    // callable environment just like closure arguments.
                    Self::ensure_closure_has_function_ty(init, None, None);
                    self.rewrite_in_expr(init)?;
                    if let Some(info) = self.closure_info_from_expr(init) {
                        let mut names = Vec::new();
                        collect_pattern_idents(&stmt_let.pat, &mut names);
                        for name in names {
                            self.variable_infos.insert(name, info.clone());
                        }
                    }
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.rewrite_in_expr(diverge)?;
                }
            }
            ast::BlockStmt::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::BlockStmt::Noop => {}
        }
        Ok(())
    }

    fn rewrite_in_item(&mut self, item: &mut ast::Item) -> Result<()> {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
            ast::ItemKind::DefConst(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = Some(info.env_struct_ty.clone());
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefStatic(def) => {
                if attrs_has_name(&def.attrs, "host") {
                    return Ok(());
                }
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = info.env_struct_ty.clone();
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefFunction(func) => self.rewrite_in_block(&mut func.body)?,
            ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
            _ => {}
        }
        Ok(())
    }

    fn closure_info_from_expr(&self, expr: &ast::Expr) -> Option<ClosureInfo> {
        match expr.kind() {
            ast::ExprKind::Struct(struct_expr) => extract_ident(struct_expr.name.as_ref())
                .and_then(|ident| self.struct_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Invoke(invoke) => {
                if let ast::ExprInvokeTarget::Function(name) = &invoke.target {
                    name.as_ident()
                        .and_then(|ident| self.function_infos.get(ident.as_str()).cloned())
                } else {
                    None
                }
            }
            ast::ExprKind::Name(name) => name
                .as_ident()
                .and_then(|ident| self.variable_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Paren(paren) => self.closure_info_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn collect_captures(&self, expr: &ast::Expr, params: &HashSet<String>) -> Result<Vec<Capture>> {
        let mut collector = CaptureCollector::new(params.clone());
        collector.visit(expr);
        Ok(collector.into_captures())
    }

    fn rewrite_captured_usage(
        &self,
        expr: &mut ast::Expr,
        captures: &[Capture],
        env_ident: &ast::Ident,
    ) {
        let mut replacer = CaptureReplacer::new(captures, env_ident.clone());
        replacer.visit(expr);
    }
}

pub(super) struct CaptureCollector {
    scope: Vec<HashSet<String>>,
    captures: Vec<(String, ast::Ty)>,
    seen: HashSet<String>,
}

impl CaptureCollector {
    pub(super) fn new(params: HashSet<String>) -> Self {
        Self {
            scope: vec![params],
            captures: Vec::new(),
            seen: HashSet::new(),
        }
    }

    fn visit(&mut self, expr: &ast::Expr) {
        match expr.kind() {
            ast::ExprKind::Quote(q) => {
                self.scope.push(HashSet::new());
                for stmt in &q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_ref());
            }
            ast::ExprKind::Closure(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::IntrinsicContainer(collection) => {
                let expanded = collection.clone().into_const_expr();
                self.visit(&expanded);
            }
            ast::ExprKind::Block(block) => {
                self.scope.push(HashSet::new());
                for stmt in &block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Let(expr_let) => {
                self.visit(expr_let.expr.as_ref());
                let mut names = Vec::new();
                collect_pattern_idents(&expr_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                match &invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => self.visit(target.as_ref()),
                    ast::ExprInvokeTarget::Method(select) => self.visit(select.obj.as_ref()),
                    _ => {}
                }
                for arg in &invoke.args {
                    self.visit(arg);
                }
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_ref());
                self.visit(assign.value.as_ref());
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_ref());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_ref());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_ref());
                self.visit(binop.rhs.as_ref());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_ref()),
            ast::ExprKind::FieldAccess(select) => self.visit(select.obj.as_ref()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_ref());
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_ref());
                self.visit(array_repeat.len.as_ref());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_ref()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_ref()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_ref()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_ref());
                self.visit(index.index.as_ref());
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_ref());
                self.visit(expr_if.then.as_ref());
                if let Some(elze) = expr_if.elze.as_ref() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_ref()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_ref());
                self.visit(expr_while.body.as_ref());
            }
            ast::ExprKind::With(expr_with) => {
                self.visit(expr_with.context.as_ref());
                self.visit(expr_with.body.as_ref());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_ref());
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_ref());
                self.visit(expr_for.body.as_ref());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &expr_match.cases {
                    self.visit(case.cond.as_ref());
                    self.visit(case.body.as_ref());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_ref() {
                    self.visit(start.as_ref());
                }
                if let Some(end) = range.end.as_ref() {
                    self.visit(end.as_ref());
                }
                if let Some(step) = range.step.as_ref() {
                    self.visit(step.as_ref());
                }
            }
            ast::ExprKind::Try(expr_try) => {
                self.visit(expr_try.expr.as_ref());
                for catch in &expr_try.catches {
                    self.visit(catch.body.as_ref());
                }
                if let Some(elze) = expr_try.elze.as_ref() {
                    self.visit(elze.as_ref());
                }
                if let Some(finally) = expr_try.finally.as_ref() {
                    self.visit(finally.as_ref());
                }
            }
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Expr(expr) => self.visit(expr.as_ref()),
                ast::Value::Function(func) => self.visit(func.body.as_ref()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_ref()),
            ast::ExprKind::Name(name) => {
                if let Some(ident) = name.as_ident() {
                    let name = ident.as_str();
                    if !self.is_in_scope(name) && !self.seen.contains(name) {
                        let ty = ast::Ty::Any(ast::TypeAny);
                        self.seen.insert(name.to_string());
                        self.captures.push((name.to_string(), ty));
                    }
                }
            }
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_ref()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_ref()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_ref()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &call.args {
                    self.visit(arg);
                }
                for kwarg in &call.kwargs {
                    self.visit(&kwarg.value);
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_ref()),
            ast::BlockStmt::Defer(stmt_defer) => self.visit(stmt_defer.expr.as_ref()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_ref() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_ref() {
                    self.visit(diverge);
                }
                let mut names = Vec::new();
                collect_pattern_idents(&stmt_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_ref()),
            ast::BlockStmt::Noop => {}
        }
    }

    fn visit_block(&mut self, block: &ast::ExprBlock) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_item(&mut self, item: &ast::Item) {
        match item.kind() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_ref()),
            ast::ItemKind::DefStatic(def) if !attrs_has_name(&def.attrs, "host") => {
                self.visit(def.value.as_ref())
            }
            ast::ItemKind::DefFunction(func) => self.visit_block(&func.body),
            ast::ItemKind::Module(module) => {
                for item in &module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }

    fn is_in_scope(&self, name: &str) -> bool {
        self.scope.iter().rev().any(|scope| scope.contains(name))
    }

    fn into_captures(self) -> Vec<Capture> {
        self.captures
            .into_iter()
            .map(|(name, ty)| Capture {
                name: ast::Ident::new(name),
                ty,
            })
            .collect()
    }
}

fn collect_pattern_idents(pat: &ast::Pattern, out: &mut Vec<String>) {
    match pat.kind() {
        ast::PatternKind::Ident(ident) => out.push(ident.ident.name.as_str().to_string()),
        ast::PatternKind::Bind(bind) => {
            out.push(bind.ident.ident.name.as_str().to_string());
            collect_pattern_idents(&bind.pattern, out);
        }
        ast::PatternKind::Tuple(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        ast::PatternKind::Struct(pat_struct) => {
            for field in &pat_struct.fields {
                if let Some(rename) = field.rename.as_ref() {
                    collect_pattern_idents(rename.as_ref(), out);
                } else {
                    out.push(field.name.as_str().to_string());
                }
            }
        }
        ast::PatternKind::TupleStruct(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        _ => {}
    }
}

pub(super) struct CaptureReplacer {
    captures: HashMap<String, ast::Ty>,
    env_ident: ast::Ident,
}

impl CaptureReplacer {
    pub(super) fn new(captures: &[Capture], env_ident: ast::Ident) -> Self {
        let mut capture_map = HashMap::new();
        for capture in captures {
            capture_map.insert(capture.name.as_str().to_string(), capture.ty.clone());
        }
        Self {
            captures: capture_map,
            env_ident,
        }
    }

    fn visit(&mut self, expr: &mut ast::Expr) {
        match expr.kind_mut() {
            ast::ExprKind::Name(name) => {
                if let Some(ident) = name.as_ident() {
                    if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                        let mut expr_struct =
                            ast::Expr::new(ast::ExprKind::FieldAccess(ast::ExprFieldAccess {
                                span: fp_core::span::Span::null(),
                                obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                field: ident.clone(),
                                generic_args: Vec::new(),
                            }));
                        *expr = expr_struct;
                    }
                }
            }
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_mut());
                self.visit(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_mut()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_mut());
                self.visit(expr_while.body.as_mut());
            }
            ast::ExprKind::With(expr_with) => {
                self.visit(expr_with.context.as_mut());
                self.visit(expr_with.body.as_mut());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_mut());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.visit(case.cond.as_mut());
                    self.visit(case.body.as_mut());
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_mut());
                self.visit(expr_for.body.as_mut());
            }
            ast::ExprKind::Let(expr_let) => self.visit(expr_let.expr.as_mut()),
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.visit(arg);
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.visit(target.as_mut());
                    }
                    ast::ExprInvokeTarget::Function(name) => {
                        if let Some(ident) = name.as_ident() {
                            if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                                let expr_struct = ast::Expr::new(ast::ExprKind::FieldAccess(
                                    ast::ExprFieldAccess {
                                        span: fp_core::span::Span::null(),
                                        obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                        field: ident.clone(),
                                        generic_args: Vec::new(),
                                    },
                                ));
                                invoke.target = ast::ExprInvokeTarget::Expr(expr_struct.into());
                            }
                        }
                    }
                    ast::ExprInvokeTarget::Method(select) => {
                        self.visit(select.obj.as_mut());
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_mut());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_mut());
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_mut());
                self.visit(assign.value.as_mut());
            }
            ast::ExprKind::FieldAccess(select) => self.visit(select.obj.as_mut()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_mut());
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_mut());
                self.visit(array_repeat.len.as_mut());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_mut()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_mut()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_mut()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_mut());
                self.visit(index.index.as_mut());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_mut());
                self.visit(binop.rhs.as_mut());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_mut()),
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.visit(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.visit(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.visit(step.as_mut());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => {
                self.visit(expr_try.expr.as_mut());
                for catch in &mut expr_try.catches {
                    self.visit(catch.body.as_mut());
                }
                if let Some(elze) = expr_try.elze.as_mut() {
                    self.visit(elze.as_mut());
                }
                if let Some(finally) = expr_try.finally.as_mut() {
                    self.visit(finally.as_mut());
                }
            }
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.visit(expr.as_mut()),
                ast::Value::Function(func) => self.visit(func.body.as_mut()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_mut()),
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_mut()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_mut()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_mut()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.visit(arg);
                }
                for kwarg in &mut call.kwargs {
                    self.visit(&mut kwarg.value);
                }
            }
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_mut());
            }
            ast::ExprKind::IntrinsicContainer(container) => {
                let mut new_expr = container.take_into_const_expr();
                self.visit(&mut new_expr);
                *expr = new_expr;
            }
            ast::ExprKind::Closure(_) | ast::ExprKind::Closured(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_mut()),
            ast::BlockStmt::Defer(stmt_defer) => self.visit(stmt_defer.expr.as_mut()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.visit(diverge);
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_mut()),
            ast::BlockStmt::Noop => {}
        }
    }

    fn visit_block(&mut self, block: &mut ast::ExprBlock) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_item(&mut self, item: &mut ast::Item) {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_mut()),
            ast::ItemKind::DefStatic(def) if !attrs_has_name(&def.attrs, "host") => {
                self.visit(def.value.as_mut())
            }
            ast::ItemKind::DefFunction(func) => self.visit_block(&mut func.body),
            ast::ItemKind::Module(module) => {
                for item in &mut module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }
}

fn extract_ident(expr: &ast::Expr) -> Option<&ast::Ident> {
    if let ast::ExprKind::Name(name) = expr.kind() {
        name.as_ident()
    } else {
        None
    }
}

/// Strips `#[doc = "..."]`/`///` attributes from every item (recursing
/// into modules and impl blocks) — HIR carries no doc-comment concept, so
/// backends that lower through it never see these; only callers that skip
/// HIR-based typechecking and hand items to a renderer more directly
/// (`fp-shell`'s roundtrip) need to strip them explicitly first.
pub(crate) fn strip_doc_attrs_in_items(items: &mut [ast::Item]) {
    for item in items {
        strip_doc_attrs_in_item(item);
    }
}

fn strip_doc_attrs_in_item(item: &mut ast::Item) {
    if let Some(attrs) = item_attrs_mut(item) {
        attrs.retain(|attr| !is_doc_attr(attr));
    }

    match item.kind_mut() {
        ItemKind::Module(module) => strip_doc_attrs_in_items(&mut module.items),
        ItemKind::Impl(impl_block) => strip_doc_attrs_in_items(&mut impl_block.items),
        _ => {}
    }
}

fn item_attrs_mut(item: &mut ast::Item) -> Option<&mut Vec<ast::Attribute>> {
    match item.kind_mut() {
        ItemKind::Module(module) => Some(&mut module.attrs),
        ItemKind::DefStruct(def) => Some(&mut def.attrs),
        ItemKind::DefStructural(def) => Some(&mut def.attrs),
        ItemKind::DefEnum(def) => Some(&mut def.attrs),
        ItemKind::DefType(def) => Some(&mut def.attrs),
        ItemKind::DefConst(def) => Some(&mut def.attrs),
        ItemKind::DefStatic(def) => Some(&mut def.attrs),
        ItemKind::DefFunction(def) => Some(&mut def.attrs),
        ItemKind::DefTrait(def) => Some(&mut def.attrs),
        ItemKind::Import(import) => Some(&mut import.attrs),
        ItemKind::Impl(impl_block) => Some(&mut impl_block.attrs),
        _ => None,
    }
}

pub(super) fn attrs_has_name(attrs: &[ast::Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| attr_has_name(attr, name))
}

/// True if `function`'s lowered body is nothing but a bare
/// `compile_error!(...)` call — the established convention (throughout
/// `crates/fp-lang/src/std/**/*.fp`) for a function whose real
/// implementation the compiler synthesizes elsewhere, with the `.fp`-level
/// body existing only to satisfy the type checker's signature
/// requirements. See the `ItemKind::DefFunction` caller for why this can't
/// just be type-checked/lowered normally.
pub(super) fn function_body_is_compiler_intrinsic_marker(function: &hir::Function) -> bool {
    let Some(body) = &function.body else {
        return false;
    };
    // Marker bodies are allowed any number of leading `let _ = param;`
    // statements (silencing "unused parameter" for params only meaningful
    // to the real, compiler-synthesized implementation) before the bare
    // `compile_error!(...)` marker call itself.
    let all_leading_stmts_are_discards = body.stmts.iter().all(|stmt| {
        matches!(
            &stmt.kind,
            hir::StmtKind::Local(local) if matches!(local.pat.kind, hir::PatKind::Wild)
        )
    });
    if !all_leading_stmts_are_discards {
        return false;
    }
    matches!(
        body.expr.as_deref().map(|expr| &expr.kind),
        Some(hir::ExprKind::IntrinsicCall(call))
            if call.kind == fp_core::intrinsics::IntrinsicKind::CompileError
    )
}

fn attr_has_name(attr: &ast::Attribute, name: &str) -> bool {
    match &attr.meta {
        ast::AttrMeta::Path(path) => path.last().as_str() == name,
        ast::AttrMeta::List(list) => list.name.last().as_str() == name,
        ast::AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
    }
}

fn is_doc_attr(attr: &ast::Attribute) -> bool {
    match &attr.meta {
        ast::AttrMeta::Path(path) => path.last().as_str() == "doc",
        ast::AttrMeta::List(list) => list.name.last().as_str() == "doc",
        ast::AttrMeta::NameValue(nv) => nv.name.last().as_str() == "doc",
    }
}
