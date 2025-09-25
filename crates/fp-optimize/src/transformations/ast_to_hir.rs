use fp_core::error::Result;
use fp_core::id::Locator;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::pat::Pattern;
use fp_core::span::{FileId, Span};
use fp_core::{ast, hir};
use std::collections::HashMap;
use std::path::Path;

use super::IrTransform;

/// Generator for transforming AST to HIR (High-level IR)
pub struct HirGenerator {
    next_hir_id: hir::HirId,
    next_def_id: hir::DefId,
    current_file: FileId,
    current_position: u32,
    type_scopes: Vec<HashMap<String, hir::Res>>,
    value_scopes: Vec<HashMap<String, hir::Res>>,
    module_path: Vec<String>,
    global_value_defs: HashMap<String, hir::Res>,
    global_type_defs: HashMap<String, hir::Res>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PathResolutionScope {
    Value,
    Type,
}

impl HirGenerator {
    /// Create a new HIR generator
    pub fn new() -> Self {
        Self {
            next_hir_id: 0,
            next_def_id: 0,
            current_file: 0, // Default file ID
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: Vec::new(),
            global_value_defs: HashMap::new(),
            global_type_defs: HashMap::new(),
        }
    }

    /// Create a new HIR generator with file context
    pub fn with_file<P: AsRef<Path>>(file_path: P) -> Self {
        // Generate a simple hash-based file ID from the path
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        file_path.as_ref().hash(&mut hasher);
        let file_id = hasher.finish();

        Self {
            next_hir_id: 0,
            next_def_id: 0,
            current_file: file_id,
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: Vec::new(),
            global_value_defs: HashMap::new(),
            global_type_defs: HashMap::new(),
        }
    }

    fn reset_file_context<P: AsRef<Path>>(&mut self, file_path: P) {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        file_path.as_ref().hash(&mut hasher);
        self.current_file = hasher.finish();
        self.current_position = 0;
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path.clear();
        self.global_value_defs.clear();
        self.global_type_defs.clear();
    }

    fn current_type_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.type_scopes
            .last_mut()
            .expect("at least one type scope must exist")
    }

    fn current_value_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.value_scopes
            .last_mut()
            .expect("at least one value scope must exist")
    }

    fn register_type_generic(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_type_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    fn register_value_def(&mut self, name: &str, def_id: hir::DefId) {
        self.current_value_scope()
            .insert(name.to_string(), hir::Res::Def(def_id));
        let qualified = self.qualify_name(name);
        self.global_value_defs
            .insert(qualified, hir::Res::Def(def_id));
    }

    fn register_value_local(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_value_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    fn register_type_def(&mut self, name: &str, def_id: hir::DefId) {
        self.current_type_scope()
            .insert(name.to_string(), hir::Res::Def(def_id));
        let qualified = self.qualify_name(name);
        self.global_type_defs
            .insert(qualified, hir::Res::Def(def_id));
    }

    fn push_module_scope(&mut self, name: &str) {
        self.module_path.push(name.to_string());
        self.push_type_scope();
        self.push_value_scope();
    }

    fn pop_module_scope(&mut self) {
        self.pop_value_scope();
        self.pop_type_scope();
        self.module_path.pop();
    }

    fn qualify_name(&self, name: &str) -> String {
        if self.module_path.is_empty() {
            name.to_string()
        } else {
            let mut qualified = self.module_path.join("::");
            qualified.push_str("::");
            qualified.push_str(name);
            qualified
        }
    }

    fn resolve_type_symbol(&self, name: &str) -> Option<hir::Res> {
        self.type_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.global_type_defs.get(name).cloned())
    }

    fn resolve_value_symbol(&self, name: &str) -> Option<hir::Res> {
        self.value_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.global_value_defs.get(name).cloned())
    }

    fn push_value_scope(&mut self) {
        self.value_scopes.push(HashMap::new());
    }

    fn pop_value_scope(&mut self) {
        self.value_scopes.pop();
        if self.value_scopes.is_empty() {
            self.value_scopes.push(HashMap::new());
        }
    }

    fn push_type_scope(&mut self) {
        self.type_scopes.push(HashMap::new());
    }

    fn pop_type_scope(&mut self) {
        self.type_scopes.pop();
        if self.type_scopes.is_empty() {
            self.type_scopes.push(HashMap::new());
        }
    }

    /// Create a span for the current position
    fn create_span(&mut self, length: u32) -> Span {
        let span = Span::new(
            self.current_file,
            self.current_position,
            self.current_position + length,
        );
        self.current_position += length;
        span
    }

    /// Transform an AST expression tree to HIR
    pub fn transform_expr(&mut self, ast_expr: &ast::Expr) -> Result<hir::Program> {
        let mut hir_program = hir::Program::new();

        // Transform the root expression into a main function
        let main_body = self.transform_expr_to_hir(ast_expr)?;
        let main_fn = self.create_main_function(main_body)?;

        // Add main function to program
        let main_item = hir::Item {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            kind: hir::ItemKind::Function(main_fn),
            span: self.create_span(4), // Span for "main" function
        };

        hir_program.items.push(main_item);

        Ok(hir_program)
    }

    /// Transform a parsed AST file into HIR
    pub fn transform_file(&mut self, file: &ast::File) -> Result<hir::Program> {
        self.reset_file_context(&file.path);
        let mut program = hir::Program::new();

        for item in &file.items {
            self.append_item(&mut program, item)?;
        }

        Ok(program)
    }

    fn append_item(&mut self, program: &mut hir::Program, item: &ast::Item) -> Result<()> {
        match item {
            ast::Item::Module(module) => {
                self.push_module_scope(&module.name.name);
                for child in &module.items {
                    self.append_item(program, child)?;
                }
                self.pop_module_scope();
            }
            _ => {
                let boxed = Box::new(item.clone());
                let hir_item = self.transform_item_to_hir(&boxed)?;
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                program.items.push(hir_item);
            }
        }
        Ok(())
    }

    /// Transform an AST expression to HIR expression
    fn transform_expr_to_hir(&mut self, ast_expr: &ast::Expr) -> Result<hir::Expr> {
        use ast::Expr;

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        let kind = match ast_expr {
            Expr::Value(value) => self.transform_value_to_hir(value)?,
            Expr::Locator(locator) => hir::ExprKind::Path(
                self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
            ),
            Expr::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            Expr::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            Expr::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            Expr::Select(select) => self.transform_select_to_hir(select)?,
            Expr::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            Expr::Block(block) => self.transform_block_to_hir(block)?,
            Expr::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            Expr::Assign(assign) => self.transform_assign_to_hir(assign)?,
            Expr::Paren(paren) => self.transform_paren_to_hir(paren)?,
            Expr::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            Expr::Any(_) => {
                // Handle macro expressions and other "any" expressions
                // For now, return a placeholder boolean literal
                hir::ExprKind::Literal(hir::Lit::Bool(false))
            }
            Expr::FormatString(format_str) => self.transform_format_string_to_hir(format_str)?,
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                )));
            }
        };

        Ok(hir::Expr { hir_id, kind, span })
    }

    /// Create a main function wrapper for the HIR expression
    fn create_main_function(&mut self, body_expr: hir::Expr) -> Result<hir::Function> {
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value: body_expr,
        };

        let sig = hir::FunctionSig {
            name: "main".to_string(),
            inputs: Vec::new(),
            output: hir::Ty::new(
                self.next_id(),
                hir::TyKind::Tuple(Vec::new()), // Unit type ()
                body.value.span,
            ),
            generics: hir::Generics::default(),
        };

        Ok(hir::Function::new(sig, Some(body), false))
    }

    /// Generate next HIR ID
    fn next_id(&mut self) -> hir::HirId {
        let id = self.next_hir_id;
        self.next_hir_id += 1;
        id
    }

    /// Generate next definition ID
    fn next_def_id(&mut self) -> hir::DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;
        id
    }

    /// Transform a function definition
    pub fn transform_function(
        &mut self,
        func: &ast::ItemDefFunction,
        self_ty: Option<hir::Ty>,
    ) -> Result<hir::Function> {
        self.push_type_scope();
        self.push_value_scope();
        let result = (|| {
            let generics = self.transform_generics(&func.sig.generics_params);

            let mut params = self.transform_params(&func.sig.params)?;
            if let Some(receiver) = &func.sig.receiver {
                let receiver_ty = self_ty.clone().unwrap_or_else(|| self.create_unit_type());
                let self_param = self.make_self_param(receiver, receiver_ty)?;
                self.register_pattern_bindings(&self_param.pat);
                params.insert(0, self_param);
            }
            let output = if let Some(ret_ty) = &func.sig.ret_ty {
                self.transform_type_to_hir(ret_ty)?
            } else {
                self.create_unit_type()
            };

            let sig = hir::FunctionSig {
                name: self.qualify_name(&func.name.name),
                inputs: params.clone(),
                output: output.clone(),
                generics,
            };

            let body_expr = self.transform_expr_to_hir(&func.body)?;
            let body = hir::Body {
                hir_id: self.next_id(),
                params,
                value: body_expr,
            };

            Ok(hir::Function::new(sig, Some(body), false))
        })();

        self.pop_value_scope();
        self.pop_type_scope();

        result
    }

    fn transform_params(&mut self, params: &[ast::FunctionParam]) -> Result<Vec<hir::Param>> {
        params
            .iter()
            .map(|param| {
                let ty = self.transform_type_to_hir(&param.ty)?;
                let pat = hir::Pat {
                    hir_id: self.next_id(),
                    kind: hir::PatKind::Binding(param.name.name.clone()),
                };

                let hir_param = hir::Param {
                    hir_id: self.next_id(),
                    pat,
                    ty,
                };

                self.register_pattern_bindings(&hir_param.pat);

                Ok(hir_param)
            })
            .collect()
    }

    fn transform_generics(&mut self, params: &[ast::GenericParam]) -> hir::Generics {
        let mut hir_params = Vec::new();
        for param in params {
            let hir_id = self.next_id();
            hir_params.push(hir::GenericParam {
                hir_id,
                name: param.name.name.clone(),
                kind: hir::GenericParamKind::Type { default: None },
            });
            self.register_type_generic(&param.name.name, hir_id);
        }

        hir::Generics {
            params: hir_params,
            where_clause: None,
        }
    }

    fn wrap_ref_type(&mut self, ty: hir::Ty) -> hir::Ty {
        hir::Ty::new(
            self.next_id(),
            hir::TyKind::Ref(Box::new(ty)),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn make_self_param(
        &mut self,
        receiver: &ast::FunctionParamReceiver,
        self_ty: hir::Ty,
    ) -> Result<hir::Param> {
        let ty = match receiver {
            ast::FunctionParamReceiver::Ref
            | ast::FunctionParamReceiver::RefStatic
            | ast::FunctionParamReceiver::RefMut
            | ast::FunctionParamReceiver::RefMutStatic => self.wrap_ref_type(self_ty),
            _ => self_ty,
        };

        Ok(hir::Param {
            hir_id: self.next_id(),
            pat: hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding("self".to_string()),
            },
            ty,
        })
    }

    fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::Impl> {
        self.push_type_scope();
        self.current_type_scope()
            .insert("Self".to_string(), hir::Res::SelfTy);
        let result = (|| {
            let self_ty_ast = ast::Ty::expr(impl_block.self_ty.clone());
            let self_ty = self.transform_type_to_hir(&self_ty_ast)?;
            let trait_ty = if let Some(trait_locator) = &impl_block.trait_ty {
                Some(hir::Ty::new(
                    self.next_id(),
                    hir::TyKind::Path(self.locator_to_hir_path_with_scope(
                        trait_locator,
                        PathResolutionScope::Type,
                    )?),
                    Span::new(self.current_file, 0, 0),
                ))
            } else {
                None
            };

            let mut items = Vec::new();
            for item in &impl_block.items {
                match item {
                    ast::Item::DefFunction(func) => {
                        let method = self.transform_function(func, Some(self_ty.clone()))?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: method.sig.name.clone(),
                            kind: hir::ImplItemKind::Method(method),
                        });
                    }
                    ast::Item::DefConst(const_item) => {
                        let assoc_const = self.transform_const_def(const_item)?;
                        items.push(hir::ImplItem {
                            hir_id: self.next_id(),
                            name: const_item.name.name.clone(),
                            kind: hir::ImplItemKind::AssocConst(assoc_const),
                        });
                    }
                    _ => {
                        // Skip unsupported impl items for now
                    }
                }
            }

            Ok(hir::Impl {
                generics: hir::Generics::default(),
                trait_ty,
                self_ty,
                items,
            })
        })();

        self.pop_type_scope();

        result
    }

    /// Transform AST value to HIR expression kind
    fn transform_value_to_hir(&mut self, value: &ast::BValue) -> Result<hir::ExprKind> {
        use ast::Value;

        match value.as_ref() {
            Value::Int(i) => Ok(hir::ExprKind::Literal(hir::Lit::Integer(i.value))),
            Value::Bool(b) => Ok(hir::ExprKind::Literal(hir::Lit::Bool(b.value))),
            Value::String(s) => Ok(hir::ExprKind::Literal(hir::Lit::Str(s.value.clone()))),
            Value::Decimal(d) => Ok(hir::ExprKind::Literal(hir::Lit::Float(d.value))),
            Value::Expr(expr) => self.transform_expr_to_hir(expr).map(|e| e.kind),
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented AST value type for HIR transformation: {:?}",
                std::mem::discriminant(value.as_ref())
            ))),
        }
    }

    /// Transform binary operation to HIR
    fn transform_binop_to_hir(&mut self, binop: &ast::ExprBinOp) -> Result<hir::ExprKind> {
        let left = Box::new(self.transform_expr_to_hir(&binop.lhs)?);
        let right = Box::new(self.transform_expr_to_hir(&binop.rhs)?);
        let op = self.convert_binop_kind(&binop.kind);

        Ok(hir::ExprKind::Binary(op, left, right))
    }

    /// Transform unary operation to HIR
    fn transform_unop_to_hir(&mut self, unop: &ast::ExprUnOp) -> Result<hir::ExprKind> {
        let operand = Box::new(self.transform_expr_to_hir(&unop.val)?);
        let op = self.convert_unop_kind(&unop.op);

        Ok(hir::ExprKind::Unary(op, operand))
    }

    /// Transform function call/invoke to HIR
    fn transform_invoke_to_hir(&mut self, invoke: &ast::ExprInvoke) -> Result<hir::ExprKind> {
        match &invoke.target {
            ast::ExprInvokeTarget::Method(select) => {
                let receiver = self.transform_expr_to_hir(&select.obj)?;
                let mut args = Vec::new();
                for arg in &invoke.args {
                    args.push(self.transform_expr_to_hir(arg)?);
                }
                Ok(hir::ExprKind::MethodCall(
                    Box::new(receiver),
                    select.field.name.clone(),
                    args,
                ))
            }
            ast::ExprInvokeTarget::Function(locator) => {
                // Handle println! formatted calls specially to preserve template
                if locator.to_string() == "println" {
                    if let Some(ast::Expr::FormatString(fmt)) = invoke.args.get(0) {
                        return self.transform_format_like_call(fmt, &invoke.args[1..]);
                    }
                }

                let func_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Path(
                        self.locator_to_hir_path_with_scope(locator, PathResolutionScope::Value)?,
                    ),
                    span: self.create_span(1),
                };
                let args = self.transform_call_args(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            ast::ExprInvokeTarget::Expr(expr) => {
                let func_expr = self.transform_expr_to_hir(expr)?;
                let args = self.transform_call_args(&invoke.args)?;
                Ok(hir::ExprKind::Call(Box::new(func_expr), args))
            }
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented invoke target type for HIR transformation: {:?}",
                invoke.target
            ))),
        }
    }

    /// Transform field selection to HIR
    fn transform_select_to_hir(&mut self, select: &ast::ExprSelect) -> Result<hir::ExprKind> {
        let expr = Box::new(self.transform_expr_to_hir(&select.obj)?);
        let field = select.field.name.clone();

        Ok(hir::ExprKind::FieldAccess(expr, field))
    }

    /// Transform struct construction to HIR
    fn transform_struct_to_hir(&mut self, struct_expr: &ast::ExprStruct) -> Result<hir::ExprKind> {
        let path =
            self.ast_expr_to_hir_path(struct_expr.name.as_ref(), PathResolutionScope::Type)?;

        let fields = struct_expr
            .fields
            .iter()
            .map(|field| {
                let expr = if let Some(value) = field.value.as_ref() {
                    self.transform_expr_to_hir(value)?
                } else {
                    // Shorthand - reference local with same name
                    let res = self.resolve_value_symbol(&field.name.name);
                    hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Path(hir::Path {
                            segments: vec![hir::PathSegment {
                                name: field.name.name.clone(),
                                args: None,
                            }],
                            res,
                        }),
                        span: self.create_span(1),
                    }
                };

                Ok(hir::StructExprField {
                    hir_id: self.next_id(),
                    name: field.name.name.clone(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::ExprKind::Struct(path, fields))
    }

    /// Transform block expression to HIR
    fn transform_block_to_hir(&mut self, block: &ast::ExprBlock) -> Result<hir::ExprKind> {
        self.push_value_scope();
        let result = (|| {
            let stmts = block
                .stmts
                .iter()
                .map(|stmt| self.transform_block_stmt_to_hir(stmt))
                .collect::<Result<Vec<_>>>()?;

            // For the final expression, check if the last statement is an expression without semicolon
            let expr = if let Some(last_expr) = block.last_expr() {
                Some(Box::new(self.transform_expr_to_hir(last_expr)?))
            } else {
                None
            };

            Ok(hir::ExprKind::Block(hir::Block {
                hir_id: self.next_id(),
                stmts,
                expr,
            }))
        })();
        self.pop_value_scope();
        result
    }

    /// Transform if expression to HIR
    fn transform_if_to_hir(&mut self, if_expr: &ast::ExprIf) -> Result<hir::ExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&if_expr.cond)?);
        let then_branch = Box::new(self.transform_expr_to_hir(&if_expr.then)?);
        let else_branch = if let Some(else_expr) = if_expr.elze.as_ref() {
            Some(Box::new(self.transform_expr_to_hir(else_expr)?))
        } else {
            None
        };

        Ok(hir::ExprKind::If(cond, then_branch, else_branch))
    }

    /// Transform assignment to HIR
    fn transform_assign_to_hir(&mut self, assign: &ast::ExprAssign) -> Result<hir::ExprKind> {
        let lhs = Box::new(self.transform_expr_to_hir(&assign.target)?);
        let rhs = Box::new(self.transform_expr_to_hir(&assign.value)?);

        Ok(hir::ExprKind::Assign(lhs, rhs))
    }

    /// Transform block statement to HIR (using actual AST types)
    fn transform_block_stmt_to_hir(&mut self, stmt: &ast::BlockStmt) -> Result<hir::Stmt> {
        let kind = match stmt {
            ast::BlockStmt::Expr(expr_stmt) => {
                hir::StmtKind::Expr(self.transform_expr_to_hir(&expr_stmt.expr)?)
            }
            ast::BlockStmt::Let(let_stmt) => {
                let pat = self.transform_pattern(&let_stmt.pat)?;
                let init = let_stmt
                    .init
                    .as_ref()
                    .map(|v| self.transform_expr_to_hir(v))
                    .transpose()?;

                let local = hir::Local {
                    hir_id: self.next_id(),
                    pat,
                    ty: None, // Type inference will fill this in later
                    init,
                };

                self.register_pattern_bindings(&local.pat);

                hir::StmtKind::Local(local)
            }
            ast::BlockStmt::Item(item) => {
                // Transform items (struct definitions, const declarations, etc.)
                self.transform_item_to_hir_stmt(item)?
            }
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented block statement type for HIR transformation: {:?}",
                    stmt
                )));
            }
        };

        Ok(hir::Stmt {
            hir_id: self.next_id(),
            kind,
        })
    }

    /// Transform an AST item into a HIR statement
    fn transform_item_to_hir_stmt(&mut self, item: &ast::BItem) -> Result<hir::StmtKind> {
        let hir_item = self.transform_item_to_hir(item)?;
        Ok(hir::StmtKind::Item(hir_item))
    }

    /// Transform an AST item into a HIR item
    fn transform_item_to_hir(&mut self, item: &ast::BItem) -> Result<hir::Item> {
        let hir_id = self.next_id();
        let def_id = self.next_def_id();
        let span = self.create_span(1);

        let kind = match item.as_ref() {
            ast::Item::DefConst(const_def) => {
                self.register_value_def(&const_def.name.name, def_id);
                let hir_const = self.transform_const_def(const_def)?;
                hir::ItemKind::Const(hir_const)
            }
            ast::Item::DefStruct(struct_def) => {
                self.register_type_def(&struct_def.name.name, def_id);
                let name = self.qualify_name(&struct_def.name.name);
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: field.name.name.clone(),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let generics = hir::Generics {
                    params: vec![],
                    where_clause: None,
                };

                hir::ItemKind::Struct(hir::Struct {
                    name,
                    fields,
                    generics,
                })
            }
            ast::Item::DefFunction(func_def) => {
                self.register_value_def(&func_def.name.name, def_id);
                let function = self.transform_function(func_def, None)?;
                hir::ItemKind::Function(function)
            }
            ast::Item::Impl(impl_block) => {
                let hir_impl = self.transform_impl(impl_block)?;
                hir::ItemKind::Impl(hir_impl)
            }
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST item type for HIR transformation: {:?}",
                    item
                )));
            }
        };

        Ok(hir::Item {
            hir_id,
            def_id,
            kind,
            span,
        })
    }

    fn transform_const_def(&mut self, const_def: &ast::ItemDefConst) -> Result<hir::Const> {
        let ty = if let Some(ty) = &const_def.ty {
            self.transform_type_to_hir(ty)?
        } else {
            self.create_unit_type()
        };

        let value = self.transform_expr_to_hir(&const_def.value)?;
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value,
        };

        Ok(hir::Const {
            name: self.qualify_name(&const_def.name.name),
            ty,
            body,
        })
    }

    /// Transform an AST type into a HIR type
    fn transform_type_to_hir(&mut self, ty: &ast::Ty) -> Result<hir::Ty> {
        match ty {
            ast::Ty::Primitive(prim) => Ok(self.primitive_type_to_hir(*prim)),
            ast::Ty::Struct(struct_ty) => Ok(hir::Ty::new(
                self.next_id(),
                hir::TyKind::Path(hir::Path {
                    segments: vec![self.make_path_segment(&struct_ty.name.name, None)],
                    res: None,
                }),
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Reference(reference) => {
                let inner = self.transform_type_to_hir(&reference.ty)?;
                Ok(hir::Ty::new(
                    self.next_id(),
                    hir::TyKind::Ref(Box::new(inner)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Tuple(tuple) => {
                let elements = tuple
                    .types
                    .iter()
                    .map(|ty| Ok(Box::new(self.transform_type_to_hir(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir::Ty::new(
                    self.next_id(),
                    hir::TyKind::Tuple(elements),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Vec(vec_ty) => {
                let args = self.convert_generic_args(&[*vec_ty.ty.clone()])?;
                Ok(hir::Ty::new(
                    self.next_id(),
                    hir::TyKind::Path(hir::Path {
                        segments: vec![self.make_path_segment("Vec", Some(args))],
                        res: None,
                    }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Expr(expr) => {
                let path = self.ast_expr_to_hir_path(expr, PathResolutionScope::Type)?;
                Ok(hir::Ty::new(
                    self.next_id(),
                    hir::TyKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            _ => {
                // Fallback to unit type for unsupported types
                Ok(self.create_unit_type())
            }
        }
    }

    /// Convert AST binary operator to HIR
    fn convert_binop_kind(&self, op: &BinOpKind) -> hir::BinOp {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => hir::BinOp::Add,
            BinOpKind::Sub => hir::BinOp::Sub,
            BinOpKind::Mul => hir::BinOp::Mul,
            BinOpKind::Div => hir::BinOp::Div,
            BinOpKind::Mod => hir::BinOp::Rem,
            BinOpKind::Eq => hir::BinOp::Eq,
            BinOpKind::Ne => hir::BinOp::Ne,
            BinOpKind::Lt => hir::BinOp::Lt,
            BinOpKind::Le => hir::BinOp::Le,
            BinOpKind::Gt => hir::BinOp::Gt,
            BinOpKind::Ge => hir::BinOp::Ge,
            BinOpKind::And => hir::BinOp::And,
            BinOpKind::Or => hir::BinOp::Or,
            BinOpKind::BitOr => hir::BinOp::BitOr,
            BinOpKind::BitAnd => hir::BinOp::BitAnd,
            BinOpKind::BitXor => hir::BinOp::BitXor,
        }
    }

    /// Convert AST unary operator to HIR
    fn convert_unop_kind(&self, op: &UnOpKind) -> hir::UnOp {
        match op {
            UnOpKind::Neg => hir::UnOp::Neg,
            UnOpKind::Not => hir::UnOp::Not,
            UnOpKind::Deref => hir::UnOp::Deref,
            UnOpKind::Any(_) => {
                // For Any variants, default to Neg as a fallback
                // This handles custom unary operators that don't have direct HIR equivalents
                hir::UnOp::Neg
            }
        }
    }

    /// Transform parentheses expression to HIR (just unwrap the inner expression)
    fn transform_paren_to_hir(&mut self, paren: &ast::ExprParen) -> Result<hir::ExprKind> {
        // Parentheses don't change semantics, just unwrap the inner expression
        let inner_expr = self.transform_expr_to_hir(&paren.expr)?;
        Ok(inner_expr.kind)
    }

    /// Transform format string to HIR - keep it as FormatString for later const evaluation
    fn transform_format_string_to_hir(
        &mut self,
        format_str: &ast::ExprFormatString,
    ) -> Result<hir::ExprKind> {
        self.transform_format_like_call(format_str, &[])
    }

    fn transform_format_like_call(
        &mut self,
        format_str: &ast::ExprFormatString,
        trailing_args: &[ast::Expr],
    ) -> Result<hir::ExprKind> {
        tracing::debug!(
            "Preserving structured format string with {} parts and {} captured args",
            format_str.parts.len(),
            format_str.args.len()
        );

        let mut template = String::new();
        for part in &format_str.parts {
            match part {
                ast::FormatTemplatePart::Literal(text) => template.push_str(text),
                ast::FormatTemplatePart::Placeholder(_) => template.push_str("{}"),
            }
        }

        let mut call_args = Vec::new();
        call_args.push(hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Str(template)),
            span: self.create_span(1),
        });

        for arg in &format_str.args {
            call_args.push(self.transform_expr_to_hir(arg)?);
        }
        for arg in trailing_args {
            call_args.push(self.transform_expr_to_hir(arg)?);
        }

        let func_expr = hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: "println".to_string(),
                    args: None,
                }],
                res: None,
            }),
            span: self.create_span(1),
        };

        Ok(hir::ExprKind::Call(Box::new(func_expr), call_args))
    }

    fn transform_call_args(&mut self, args: &[ast::Expr]) -> Result<Vec<hir::Expr>> {
        args.iter()
            .map(|arg| self.transform_expr_to_hir(arg))
            .collect()
    }

    fn locator_to_hir_path_with_scope(
        &mut self,
        locator: &Locator,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        let segments = match locator {
            Locator::Ident(ident) => vec![self.make_path_segment(&ident.name, None)],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|seg| self.make_path_segment(&seg.name, None))
                .collect(),
            Locator::ParameterPath(param_path) => {
                let mut segs = Vec::new();
                for seg in &param_path.segments {
                    let args = if seg.args.is_empty() {
                        None
                    } else {
                        Some(self.convert_generic_args(&seg.args)?)
                    };
                    segs.push(self.make_path_segment(&seg.ident.name, args));
                }
                segs
            }
        };

        let resolved = segments.last().and_then(|segment| match scope {
            PathResolutionScope::Value => self.resolve_value_symbol(&segment.name),
            PathResolutionScope::Type => self.resolve_type_symbol(&segment.name),
        });

        let resolved = if resolved.is_none() {
            let qualified: String = segments
                .iter()
                .map(|seg| seg.name.as_str())
                .collect::<Vec<_>>()
                .join("::");
            match scope {
                PathResolutionScope::Value => {
                    self.global_value_defs.get(&qualified).cloned().or(resolved)
                }
                PathResolutionScope::Type => {
                    self.global_type_defs.get(&qualified).cloned().or(resolved)
                }
            }
        } else {
            resolved
        };

        Ok(hir::Path {
            segments,
            res: resolved,
        })
    }

    fn ast_expr_to_hir_path(
        &mut self,
        expr: &ast::Expr,
        scope: PathResolutionScope,
    ) -> Result<hir::Path> {
        match expr {
            ast::Expr::Locator(locator) => self.locator_to_hir_path_with_scope(locator, scope),
            _ => Err(crate::error::optimization_error(format!(
                "Unsupported path expression: {:?}",
                expr
            ))),
        }
    }

    fn convert_generic_args(&mut self, args: &[ast::Ty]) -> Result<hir::GenericArgs> {
        let mut hir_args = Vec::new();
        for arg in args {
            let ty = self.transform_type_to_hir(arg)?;
            hir_args.push(hir::GenericArg::Type(Box::new(ty)));
        }

        Ok(hir::GenericArgs { args: hir_args })
    }

    fn make_path_segment(&self, name: &str, args: Option<hir::GenericArgs>) -> hir::PathSegment {
        hir::PathSegment {
            name: name.to_string(),
            args,
        }
    }

    fn primitive_type_to_hir(&mut self, prim: ast::TypePrimitive) -> hir::Ty {
        hir::Ty::new(
            self.next_id(),
            hir::TyKind::Primitive(prim),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn transform_pattern(&mut self, pat: &Pattern) -> Result<hir::Pat> {
        match pat {
            Pattern::Ident(ident) => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding(ident.ident.name.clone()),
            }),
            Pattern::Wildcard(_) => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Wild,
            }),
            Pattern::Type(pattern_type) => self.transform_pattern(&pattern_type.pat),
            _ => Ok(hir::Pat {
                hir_id: self.next_id(),
                kind: hir::PatKind::Binding("_".to_string()),
            }),
        }
    }

    fn register_pattern_bindings(&mut self, pat: &hir::Pat) {
        match &pat.kind {
            hir::PatKind::Binding(name) => {
                self.register_value_local(name, pat.hir_id);
            }
            hir::PatKind::Struct(_, fields) => {
                for field in fields {
                    self.register_pattern_bindings(&field.pat);
                }
            }
            hir::PatKind::Tuple(elements) => {
                for element in elements {
                    self.register_pattern_bindings(element);
                }
            }
            _ => {}
        }
    }

    /// Transform let expression to HIR
    fn transform_let_to_hir(&mut self, let_expr: &ast::ExprLet) -> Result<hir::ExprKind> {
        let pat = self.transform_pattern(&let_expr.pat)?;
        self.register_pattern_bindings(&pat);
        let init = self.transform_expr_to_hir(&let_expr.expr)?;
        let ty = self.create_unit_type();

        Ok(hir::ExprKind::Let(pat, Box::new(ty), Some(Box::new(init))))
    }

    /// Create a simple HIR literal expression
    pub fn create_simple_literal(&mut self, value: i64) -> hir::Expr {
        hir::Expr::new(
            self.next_id(),
            hir::ExprKind::Literal(hir::Lit::Integer(value)),
            Span::new(0, 0, 0),
        )
    }

    /// Create a simple HIR type
    pub fn create_simple_type(&mut self, type_name: &str) -> hir::Ty {
        hir::Ty::new(
            self.next_id(),
            hir::TyKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: type_name.to_string(),
                    args: None,
                }],
                res: None,
            }),
            Span::new(0, 0, 0),
        )
    }

    fn create_unit_type(&mut self) -> hir::Ty {
        hir::Ty::new(
            self.next_id(),
            hir::TyKind::Tuple(Vec::new()),
            Span::new(self.current_file, 0, 0),
        )
    }
}

impl<'a> IrTransform<&'a ast::Expr, hir::Program> for HirGenerator {
    fn transform(&mut self, source: &'a ast::Expr) -> Result<hir::Program> {
        self.transform_expr(source)
    }
}

impl<'a> IrTransform<&'a ast::File, hir::Program> for HirGenerator {
    fn transform(&mut self, source: &'a ast::File) -> Result<hir::Program> {
        self.transform_file(source)
    }
}

impl Default for HirGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::register_threadlocal_serializer;
    use fp_rust::printer::RustPrinter;
    use fp_rust::shll_parse_items;
    use std::collections::HashMap;
    use std::sync::Arc;

    #[test]
    fn test_hir_generator_creation() {
        let generator = HirGenerator::new();
        assert_eq!(generator.next_hir_id, 0);
        assert_eq!(generator.next_def_id, 0);
    }

    #[test]
    fn test_simple_literal_creation() -> Result<()> {
        let mut generator = HirGenerator::new();
        let expr = generator.create_simple_literal(42);

        match expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => {
                assert_eq!(value, 42);
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "Expected integer literal".to_string(),
                ));
            }
        }
        Ok(())
    }

    #[test]
    fn test_simple_type_creation() -> Result<()> {
        let mut generator = HirGenerator::new();
        let ty = generator.create_simple_type("i32");

        match ty.kind {
            hir::TyKind::Path(path) => {
                assert_eq!(path.segments[0].name, "i32");
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "Expected path type".to_string(),
                ));
            }
        }
        Ok(())
    }

    #[test]
    fn transform_file_with_function_and_struct() -> Result<()> {
        let printer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(printer.clone());

        let items = shll_parse_items! {
            struct Point {
                x: i64,
                y: i64,
            }

            fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        };

        let ast_file = ast::File {
            path: "test.fp".into(),
            items,
        };

        let mut generator = HirGenerator::new();
        let program = generator.transform_file(&ast_file)?;

        assert_eq!(program.items.len(), 2);
        let names: Vec<_> = program
            .items
            .iter()
            .map(|item| match &item.kind {
                hir::ItemKind::Struct(def) => def.name.clone(),
                hir::ItemKind::Function(func) => func.sig.name.clone(),
                _ => String::new(),
            })
            .collect();

        assert!(names.contains(&"Point".to_string()));
        assert!(names.contains(&"add".to_string()));

        Ok(())
    }

    #[test]
    fn transform_generic_function_and_method() -> Result<()> {
        let printer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(printer.clone());

        let items = shll_parse_items! {
            struct Container {
                value: i64,
            }

            impl Container {
                fn get(&self) -> i64 {
                    self.value
                }
            }

            fn identity<T>(x: T) -> T {
                x
            }
        };

        let ast_file = ast::File {
            path: "generics.fp".into(),
            items,
        };

        let mut generator = HirGenerator::new();
        let program = generator.transform_file(&ast_file)?;

        let identity = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Function(func) if func.sig.name == "identity" => Some(func),
                _ => None,
            })
            .expect("identity function present");
        assert_eq!(identity.sig.generics.params.len(), 1);
        if let hir::TyKind::Path(path) = &identity.sig.output.kind {
            assert!(
                matches!(path.res, Some(hir::Res::Local(_))),
                "generic return type should resolve to local generic param"
            );
        } else {
            panic!("expected path return type for identity function");
        }
        let param_ty = &identity.sig.inputs[0].ty;
        if let hir::TyKind::Path(path) = &param_ty.kind {
            assert!(
                matches!(path.res, Some(hir::Res::Local(_))),
                "generic parameter type should resolve to local generic param"
            );
        } else {
            panic!("expected path param type for identity function parameter");
        }

        let impl_item = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Impl(impl_block) => Some(impl_block),
                _ => None,
            })
            .expect("impl block present");
        assert!(impl_item.trait_ty.is_none());

        let method = impl_item
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ImplItemKind::Method(func) => Some(func),
                _ => None,
            })
            .expect("method present");
        assert_eq!(method.sig.inputs.len(), 1);
        match &method.sig.inputs[0].pat.kind {
            hir::PatKind::Binding(name) => assert_eq!(name, "self"),
            other => panic!("expected self binding, got {other:?}"),
        }

        Ok(())
    }

    #[test]
    fn transform_scoped_block_name_resolution() -> Result<()> {
        let printer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(printer.clone());

        let items = shll_parse_items! {
            fn outer(a: i64) -> i64 {
                let b = a;
                {
                    let c = b;
                    c + a
                }
            }
        };

        let ast_file = ast::File {
            path: "scopes.fp".into(),
            items,
        };

        let mut generator = HirGenerator::new();
        let program = generator.transform_file(&ast_file)?;

        let outer = program
            .items
            .iter()
            .find_map(|item| match &item.kind {
                hir::ItemKind::Function(func) if func.sig.name == "outer" => Some(func),
                _ => None,
            })
            .expect("outer function present");

        let body = outer.body.as_ref().expect("outer function has body");

        let mut collected_paths: Vec<&hir::Path> = Vec::new();

        fn collect_paths<'a>(expr: &'a hir::Expr, out: &mut Vec<&'a hir::Path>) {
            match &expr.kind {
                hir::ExprKind::Path(path) => out.push(path),
                hir::ExprKind::Binary(_, lhs, rhs) => {
                    collect_paths(lhs, out);
                    collect_paths(rhs, out);
                }
                hir::ExprKind::Unary(_, inner) => collect_paths(inner, out),
                hir::ExprKind::Call(func, args) => {
                    collect_paths(func, out);
                    for arg in args {
                        collect_paths(arg, out);
                    }
                }
                hir::ExprKind::MethodCall(receiver, _, args) => {
                    collect_paths(receiver, out);
                    for arg in args {
                        collect_paths(arg, out);
                    }
                }
                hir::ExprKind::FieldAccess(inner, _) => collect_paths(inner, out),
                hir::ExprKind::Struct(_, fields) => {
                    for field in fields {
                        collect_paths(&field.expr, out);
                    }
                }
                hir::ExprKind::If(cond, then_branch, else_branch) => {
                    collect_paths(cond, out);
                    collect_paths(then_branch, out);
                    if let Some(else_expr) = else_branch {
                        collect_paths(else_expr, out);
                    }
                }
                hir::ExprKind::Block(block) => collect_paths_from_block(block, out),
                hir::ExprKind::Let(_, _, Some(init)) => collect_paths(init, out),
                hir::ExprKind::Let(_, _, None) => {}
                hir::ExprKind::Assign(lhs, rhs) => {
                    collect_paths(lhs, out);
                    collect_paths(rhs, out);
                }
                hir::ExprKind::Return(expr_opt) | hir::ExprKind::Break(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        collect_paths(expr, out);
                    }
                }
                hir::ExprKind::Loop(block) => collect_paths_from_block(block, out),
                hir::ExprKind::While(cond, block) => {
                    collect_paths(cond, out);
                    collect_paths_from_block(block, out);
                }
                hir::ExprKind::Literal(_) | hir::ExprKind::Continue => {}
            }
        }

        fn collect_paths_from_block<'a>(block: &'a hir::Block, out: &mut Vec<&'a hir::Path>) {
            for stmt in &block.stmts {
                match &stmt.kind {
                    hir::StmtKind::Local(local) => {
                        if let Some(init) = &local.init {
                            collect_paths(init, out);
                        }
                    }
                    hir::StmtKind::Item(item) => collect_paths_from_item(item, out),
                    hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                        collect_paths(expr, out);
                    }
                }
            }
            if let Some(expr) = &block.expr {
                collect_paths(expr, out);
            }
        }

        fn collect_paths_from_item<'a>(item: &'a hir::Item, out: &mut Vec<&'a hir::Path>) {
            match &item.kind {
                hir::ItemKind::Function(func) => {
                    if let Some(body) = &func.body {
                        collect_paths(&body.value, out);
                    }
                }
                hir::ItemKind::Const(const_item) => collect_paths(&const_item.body.value, out),
                hir::ItemKind::Impl(impl_block) => {
                    for impl_item in &impl_block.items {
                        if let hir::ImplItemKind::Method(method) = &impl_item.kind {
                            if let Some(body) = &method.body {
                                collect_paths(&body.value, out);
                            }
                        }
                    }
                }
                hir::ItemKind::Struct(_) => {}
            }
        }

        collect_paths(&body.value, &mut collected_paths);

        let mut name_to_paths: HashMap<String, Vec<&hir::Path>> = HashMap::new();

        for path in collected_paths {
            if let Some(segment) = path.segments.last() {
                name_to_paths
                    .entry(segment.name.clone())
                    .or_default()
                    .push(path);
            }
        }

        for name in ["a", "b", "c"] {
            let paths = name_to_paths
                .get(name)
                .unwrap_or_else(|| panic!("expected paths for {name}"));
            assert!(
                paths
                    .iter()
                    .all(|path| matches!(path.res, Some(hir::Res::Local(_)))),
                "expected {name} to resolve to a local"
            );
        }

        Ok(())
    }
}
