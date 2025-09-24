use fp_core::error::Result;
use fp_core::id::Locator;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::{FileId, Span};
use fp_core::{ast, hir};
use std::path::Path;

use super::IrTransform;

/// Generator for transforming AST to HIR (High-level IR)
pub struct HirGenerator {
    next_hir_id: hir::HirId,
    next_def_id: hir::DefId,
    current_file: FileId,
    current_position: u32,
}

impl HirGenerator {
    /// Create a new HIR generator
    pub fn new() -> Self {
        Self {
            next_hir_id: 0,
            next_def_id: 0,
            current_file: 0, // Default file ID
            current_position: 0,
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
        }
    }

    fn reset_file_context<P: AsRef<Path>>(&mut self, file_path: P) {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        file_path.as_ref().hash(&mut hasher);
        self.current_file = hasher.finish();
        self.current_position = 0;
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
    pub fn transform_expr(&mut self, ast_expr: &ast::AstExpr) -> Result<hir::HirProgram> {
        let mut hir_program = hir::HirProgram::new();

        // Transform the root expression into a main function
        let main_body = self.transform_expr_to_hir(ast_expr)?;
        let main_fn = self.create_main_function(main_body)?;

        // Add main function to program
        let main_item = hir::HirItem {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            kind: hir::HirItemKind::Function(main_fn),
            span: self.create_span(4), // Span for "main" function
        };

        hir_program.items.push(main_item);

        Ok(hir_program)
    }

    /// Transform an AST file into a HIR program
    pub fn transform_file(&mut self, file: &ast::AstFile) -> Result<hir::HirProgram> {
        self.reset_file_context(&file.path);
        let mut program = hir::HirProgram::new();

        for item in &file.items {
            self.append_item(&mut program, item)?;
        }

        Ok(program)
    }

    fn append_item(&mut self, program: &mut hir::HirProgram, item: &ast::AstItem) -> Result<()> {
        match item {
            ast::AstItem::Module(module) => {
                for child in &module.items {
                    self.append_item(program, child)?;
                }
            }
            _ => {
                let hir_item = self.transform_item_to_hir(&Box::new(item.clone()))?;
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                program.items.push(hir_item);
            }
        }
        Ok(())
    }

    /// Transform an AST expression to HIR expression
    fn transform_expr_to_hir(&mut self, ast_expr: &ast::AstExpr) -> Result<hir::HirExpr> {
        use ast::AstExpr;

        let span = self.create_span(1); // Create a span for this expression
        let hir_id = self.next_id();

        let kind = match ast_expr {
            AstExpr::Value(value) => self.transform_value_to_hir(value)?,
            AstExpr::Locator(locator) => hir::HirExprKind::Path(hir::HirPath {
                segments: vec![hir::HirPathSegment {
                    name: locator.to_string(),
                    args: None,
                }],
                res: None,
            }),
            AstExpr::BinOp(binop) => self.transform_binop_to_hir(binop)?,
            AstExpr::UnOp(unop) => self.transform_unop_to_hir(unop)?,
            AstExpr::Invoke(invoke) => self.transform_invoke_to_hir(invoke)?,
            AstExpr::Select(select) => self.transform_select_to_hir(select)?,
            AstExpr::Struct(struct_expr) => self.transform_struct_to_hir(struct_expr)?,
            AstExpr::Block(block) => self.transform_block_to_hir(block)?,
            AstExpr::If(if_expr) => self.transform_if_to_hir(if_expr)?,
            AstExpr::Assign(assign) => self.transform_assign_to_hir(assign)?,
            AstExpr::Paren(paren) => self.transform_paren_to_hir(paren)?,
            AstExpr::Let(let_expr) => self.transform_let_to_hir(let_expr)?,
            AstExpr::Any(_) => {
                // Handle macro expressions and other "any" expressions
                // For now, return a placeholder boolean literal
                hir::HirExprKind::Literal(hir::HirLit::Bool(false))
            }
            AstExpr::FormatString(format_str) => self.transform_format_string_to_hir(format_str)?,
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST expression type for HIR transformation: {:?}",
                    ast_expr
                )));
            }
        };

        Ok(hir::HirExpr { hir_id, kind, span })
    }

    /// Create a main function wrapper for the HIR expression
    fn create_main_function(&mut self, body_expr: hir::HirExpr) -> Result<hir::HirFunction> {
        let body = hir::HirBody {
            hir_id: self.next_id(),
            params: Vec::new(),
            value: body_expr,
        };

        let sig = hir::HirFunctionSig {
            name: "main".to_string(),
            inputs: Vec::new(),
            output: hir::HirTy::new(
                self.next_id(),
                hir::HirTyKind::Tuple(Vec::new()), // Unit type ()
                body.value.span,
            ),
            generics: hir::HirGenerics::default(),
        };

        Ok(hir::HirFunction::new(sig, Some(body), false))
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
    pub fn transform_function(&mut self, func: &ast::ItemDefFunction) -> Result<hir::HirFunction> {
        let params = self.transform_params(&func.sig.params)?;
        let output = if let Some(ret_ty) = &func.sig.ret_ty {
            self.transform_type_to_hir(ret_ty)
        } else {
            Ok(hir::HirTy::new(
                self.next_id(),
                hir::HirTyKind::Tuple(Vec::new()),
                Span::new(self.current_file, 0, 0),
            ))
        }?;

        let sig = hir::HirFunctionSig {
            name: func.name.name.clone(),
            inputs: params.clone(),
            output: output.clone(),
            generics: hir::HirGenerics::default(),
        };

        let body_expr = self.transform_expr_to_hir(&func.body)?;
        let body = hir::HirBody {
            hir_id: self.next_id(),
            params,
            value: body_expr,
        };

        Ok(hir::HirFunction::new(sig, Some(body), false))
    }

    fn transform_params(&mut self, params: &[ast::FunctionParam]) -> Result<Vec<hir::HirParam>> {
        params
            .iter()
            .map(|param| {
                let ty = self.transform_type_to_hir(&param.ty)?;
                let pat = hir::HirPat {
                    hir_id: self.next_id(),
                    kind: hir::HirPatKind::Binding(param.name.name.clone()),
                };

                Ok(hir::HirParam {
                    hir_id: self.next_id(),
                    pat,
                    ty,
                })
            })
            .collect()
    }

    fn transform_impl(&mut self, impl_block: &ast::ItemImpl) -> Result<hir::HirImpl> {
        let self_ty_ast = ast::AstType::expr(impl_block.self_ty.clone());
        let self_ty = self.transform_type_to_hir(&self_ty_ast)?;

        let mut items = Vec::new();
        for item in &impl_block.items {
            match item {
                ast::AstItem::DefFunction(func) => {
                    let method = self.transform_function(func)?;
                    items.push(hir::HirImplItem {
                        hir_id: self.next_id(),
                        name: method.sig.name.clone(),
                        kind: hir::HirImplItemKind::Method(method),
                    });
                }
                _ => {
                    // Skip unsupported impl items for now
                }
            }
        }

        Ok(hir::HirImpl {
            generics: hir::HirGenerics::default(),
            self_ty,
            items,
        })
    }

    /// Transform AST value to HIR expression kind
    fn transform_value_to_hir(&mut self, value: &ast::BValue) -> Result<hir::HirExprKind> {
        use ast::AstValue;

        match value.as_ref() {
            AstValue::Int(i) => Ok(hir::HirExprKind::Literal(hir::HirLit::Integer(i.value))),
            AstValue::Bool(b) => Ok(hir::HirExprKind::Literal(hir::HirLit::Bool(b.value))),
            AstValue::String(s) => Ok(hir::HirExprKind::Literal(hir::HirLit::Str(s.value.clone()))),
            AstValue::Decimal(d) => Ok(hir::HirExprKind::Literal(hir::HirLit::Float(d.value))),
            AstValue::Expr(expr) => self.transform_expr_to_hir(expr).map(|e| e.kind),
            _ => Err(crate::error::optimization_error(format!(
                "Unimplemented AST value type for HIR transformation: {:?}",
                std::mem::discriminant(value.as_ref())
            ))),
        }
    }

    /// Transform binary operation to HIR
    fn transform_binop_to_hir(&mut self, binop: &ast::ExprBinOp) -> Result<hir::HirExprKind> {
        let left = Box::new(self.transform_expr_to_hir(&binop.lhs)?);
        let right = Box::new(self.transform_expr_to_hir(&binop.rhs)?);
        let op = self.convert_binop_kind(&binop.kind);

        Ok(hir::HirExprKind::Binary(op, left, right))
    }

    /// Transform unary operation to HIR
    fn transform_unop_to_hir(&mut self, unop: &ast::ExprUnOp) -> Result<hir::HirExprKind> {
        let operand = Box::new(self.transform_expr_to_hir(&unop.val)?);
        let op = self.convert_unop_kind(&unop.op);

        Ok(hir::HirExprKind::Unary(op, operand))
    }

    /// Transform function call/invoke to HIR
    fn transform_invoke_to_hir(&mut self, invoke: &ast::ExprInvoke) -> Result<hir::HirExprKind> {
        // Special-case: println! macro was parsed into a function invoke
        // If the first argument is a structured format string, expand it into
        // a single Call to println with a literal template + transformed args.
        if let ast::ExprInvokeTarget::Function(loc) = &invoke.target {
            if loc.to_string() == "println" {
                if let Some(ast::AstExpr::FormatString(fmt)) = invoke.args.get(0) {
                    // Build template literal
                    let mut template_str = String::new();
                    for part in &fmt.parts {
                        match part {
                            ast::FormatTemplatePart::Literal(literal) => {
                                template_str.push_str(literal)
                            }
                            ast::FormatTemplatePart::Placeholder(_) => template_str.push_str("{}"),
                        }
                    }

                    // Transform arguments inside the format string
                    let hir_args: Result<Vec<_>> = fmt
                        .args
                        .iter()
                        .map(|a| self.transform_expr_to_hir(a))
                        .collect();
                    let mut call_args = vec![hir::HirExpr {
                        hir_id: self.next_id(),
                        kind: hir::HirExprKind::Literal(hir::HirLit::Str(template_str)),
                        span: Span::new(0, 0, 0),
                    }];
                    call_args.extend(hir_args?);

                    let func = Box::new(hir::HirExpr {
                        hir_id: self.next_id(),
                        kind: hir::HirExprKind::Path(hir::HirPath {
                            segments: vec![hir::HirPathSegment {
                                name: "println".to_string(),
                                args: None,
                            }],
                            res: None,
                        }),
                        span: Span::new(0, 0, 0),
                    });
                    return Ok(hir::HirExprKind::Call(func, call_args));
                }
            }
        }
        let func = Box::new(match &invoke.target {
            ast::ExprInvokeTarget::Function(loc) => hir::HirExpr {
                hir_id: self.next_id(),
                kind: hir::HirExprKind::Path(hir::HirPath {
                    segments: vec![hir::HirPathSegment {
                        name: loc.to_string(),
                        args: None,
                    }],
                    res: None,
                }),
                span: Span::new(0, 0, 0),
            },
            ast::ExprInvokeTarget::Expr(expr) => self.transform_expr_to_hir(expr)?,
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented invoke target type for HIR transformation: {:?}",
                    std::mem::discriminant(&invoke.target)
                )));
            }
        });

        let args = invoke
            .args
            .iter()
            .map(|arg| self.transform_expr_to_hir(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::HirExprKind::Call(func, args))
    }

    /// Transform field selection to HIR
    fn transform_select_to_hir(&mut self, select: &ast::ExprSelect) -> Result<hir::HirExprKind> {
        let expr = Box::new(self.transform_expr_to_hir(&select.obj)?);
        let field = select.field.to_string();

        Ok(hir::HirExprKind::FieldAccess(expr, field))
    }

    /// Transform struct construction to HIR
    fn transform_struct_to_hir(
        &mut self,
        struct_expr: &ast::ExprStruct,
    ) -> Result<hir::HirExprKind> {
        let path = hir::HirPath {
            segments: vec![hir::HirPathSegment {
                name: "StructName".to_string(), // Simplified for now
                args: None,
            }],
            res: None,
        };

        let fields = struct_expr
            .fields
            .iter()
            .map(|field| {
                Ok(hir::HirStructExprField {
                    hir_id: self.next_id(),
                    name: field.name.to_string(),
                    expr: self.transform_expr_to_hir(
                        field.value.as_ref().ok_or("Field must have value")?,
                    )?,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::HirExprKind::Struct(path, fields))
    }

    /// Transform block expression to HIR
    fn transform_block_to_hir(&mut self, block: &ast::ExprBlock) -> Result<hir::HirExprKind> {
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

        Ok(hir::HirExprKind::Block(hir::HirBlock {
            hir_id: self.next_id(),
            stmts,
            expr,
        }))
    }

    /// Transform if expression to HIR
    fn transform_if_to_hir(&mut self, if_expr: &ast::ExprIf) -> Result<hir::HirExprKind> {
        let cond = Box::new(self.transform_expr_to_hir(&if_expr.cond)?);
        let then_branch = Box::new(self.transform_expr_to_hir(&if_expr.then)?);
        let else_branch = if let Some(else_expr) = if_expr.elze.as_ref() {
            Some(Box::new(self.transform_expr_to_hir(else_expr)?))
        } else {
            None
        };

        Ok(hir::HirExprKind::If(cond, then_branch, else_branch))
    }

    /// Transform assignment to HIR
    fn transform_assign_to_hir(&mut self, assign: &ast::ExprAssign) -> Result<hir::HirExprKind> {
        let lhs = Box::new(self.transform_expr_to_hir(&assign.target)?);
        let rhs = Box::new(self.transform_expr_to_hir(&assign.value)?);

        Ok(hir::HirExprKind::Assign(lhs, rhs))
    }

    /// Transform block statement to HIR (using actual AST types)
    fn transform_block_stmt_to_hir(&mut self, stmt: &ast::BlockStmt) -> Result<hir::HirStmt> {
        let kind = match stmt {
            ast::BlockStmt::Expr(expr_stmt) => {
                hir::HirStmtKind::Expr(self.transform_expr_to_hir(&expr_stmt.expr)?)
            }
            ast::BlockStmt::Let(let_stmt) => {
                let pat = hir::HirPat {
                    hir_id: self.next_id(),
                    kind: hir::HirPatKind::Binding("var".to_string()), // Simplified pattern handling
                };

                let init = let_stmt
                    .init
                    .as_ref()
                    .map(|v| self.transform_expr_to_hir(v))
                    .transpose()?;

                let local = hir::HirLocal {
                    hir_id: self.next_id(),
                    pat,
                    ty: None, // Type inference will fill this in later
                    init,
                };

                hir::HirStmtKind::Local(local)
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

        Ok(hir::HirStmt {
            hir_id: self.next_id(),
            kind,
        })
    }

    /// Transform an AST item into a HIR statement
    fn transform_item_to_hir_stmt(&mut self, item: &ast::BItem) -> Result<hir::HirStmtKind> {
        let hir_item = self.transform_item_to_hir(item)?;
        Ok(hir::HirStmtKind::Item(hir_item))
    }

    /// Transform an AST item into a HIR item
    fn transform_item_to_hir(&mut self, item: &ast::BItem) -> Result<hir::HirItem> {
        let kind = match item.as_ref() {
            ast::AstItem::DefConst(const_def) => {
                // Transform const declaration
                let name = const_def.name.name.clone();
                let ty = if let Some(ty) = &const_def.ty {
                    self.transform_type_to_hir(ty)?
                } else {
                    // Use unit type as placeholder for type inference
                    hir::HirTy::new(
                        self.next_id(),
                        hir::HirTyKind::Tuple(Vec::new()),
                        hir::Span::new(0, 0, 0),
                    )
                };
                let body = hir::HirBody {
                    hir_id: self.next_id(),
                    params: vec![],
                    value: self.transform_expr_to_hir(&const_def.value)?,
                };
                hir::HirItemKind::Const(hir::HirConst { name, ty, body })
            }
            ast::AstItem::DefStruct(struct_def) => {
                // Transform struct definition
                let name = struct_def.name.name.clone();
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::HirStructField {
                            hir_id: self.next_id(),
                            name: field.name.name.clone(),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::HirVisibility::Public, // Simplified visibility handling
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let generics = hir::HirGenerics {
                    params: vec![], // Simplified generic handling
                    where_clause: None,
                };

                hir::HirItemKind::Struct(hir::HirStruct {
                    name,
                    fields,
                    generics,
                })
            }
            ast::AstItem::DefFunction(func_def) => {
                let function = self.transform_function(func_def)?;
                hir::HirItemKind::Function(function)
            }
            ast::AstItem::Impl(impl_block) => {
                let hir_impl = self.transform_impl(impl_block)?;
                hir::HirItemKind::Impl(hir_impl)
            }
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST item type for HIR transformation: {:?}",
                    item
                )));
            }
        };

        Ok(hir::HirItem {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            kind,
            span: self.create_span(1), // Proper span for HIR item
        })
    }

    /// Transform an AST type into a HIR type
    fn transform_type_to_hir(&mut self, ty: &ast::AstType) -> Result<hir::HirTy> {
        match ty {
            ast::AstType::Primitive(prim) => {
                let type_name = match prim {
                    ast::TypePrimitive::Bool => "bool",
                    ast::TypePrimitive::Char => "char",
                    ast::TypePrimitive::String => "String",
                    ast::TypePrimitive::Int(int_ty) => match int_ty {
                        ast::TypeInt::I8 => "i8",
                        ast::TypeInt::I16 => "i16",
                        ast::TypeInt::I32 => "i32",
                        ast::TypeInt::I64 => "i64",
                        ast::TypeInt::U8 => "u8",
                        ast::TypeInt::U16 => "u16",
                        ast::TypeInt::U32 => "u32",
                        ast::TypeInt::U64 => "u64",
                        ast::TypeInt::BigInt => "i64", // Map BigInt to i64 for now
                    },
                    ast::TypePrimitive::Decimal(dec_ty) => match dec_ty {
                        ast::DecimalType::F32 => "f32",
                        ast::DecimalType::F64 => "f64",
                        ast::DecimalType::BigDecimal => "f64", // Map to f64 for now
                        ast::DecimalType::Decimal { .. } => "f64", // Map to f64 for now
                    },
                    _ => "unknown",
                };
                Ok(hir::HirTy::new(
                    self.next_id(),
                    hir::HirTyKind::Path(hir::HirPath {
                        segments: vec![hir::HirPathSegment {
                            name: type_name.to_string(),
                            args: None,
                        }],
                        res: None,
                    }),
                    hir::Span::new(0, 0, 0),
                ))
            }
            ast::AstType::Expr(expr) => {
                // Handle type expressions like Locator(#usize)
                // For now, extract the type name if possible
                let type_name = self.extract_type_name_from_expr(expr)?;
                Ok(hir::HirTy::new(
                    self.next_id(),
                    hir::HirTyKind::Path(hir::HirPath {
                        segments: vec![hir::HirPathSegment {
                            name: type_name,
                            args: None,
                        }],
                        res: None,
                    }),
                    hir::Span::new(0, 0, 0),
                ))
            }
            _ => {
                // Fallback to unit type for unsupported types
                Ok(hir::HirTy::new(
                    self.next_id(),
                    hir::HirTyKind::Tuple(Vec::new()),
                    hir::Span::new(0, 0, 0),
                ))
            }
        }
    }

    /// Extract type name from an expression (like Locator(#usize))
    fn extract_type_name_from_expr(&self, expr: &ast::AstExpr) -> Result<String> {
        match expr {
            ast::AstExpr::Locator(locator) => {
                // Extract type name from locator
                match locator {
                    Locator::Ident(ident) => Ok(ident.name.clone()),
                    Locator::Path(path) => {
                        // Use the last segment as the type name
                        if let Some(last_segment) = path.segments.last() {
                            Ok(last_segment.name.clone())
                        } else {
                            Ok("()".to_string())
                        }
                    }
                    _ => Ok("()".to_string()),
                }
            }
            _ => {
                // For now, default to a generic type
                Ok("()".to_string())
            }
        }
    }

    /// Convert AST binary operator to HIR
    fn convert_binop_kind(&self, op: &BinOpKind) -> hir::HirBinOp {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => hir::HirBinOp::Add,
            BinOpKind::Sub => hir::HirBinOp::Sub,
            BinOpKind::Mul => hir::HirBinOp::Mul,
            BinOpKind::Div => hir::HirBinOp::Div,
            BinOpKind::Mod => hir::HirBinOp::Rem,
            BinOpKind::Eq => hir::HirBinOp::Eq,
            BinOpKind::Ne => hir::HirBinOp::Ne,
            BinOpKind::Lt => hir::HirBinOp::Lt,
            BinOpKind::Le => hir::HirBinOp::Le,
            BinOpKind::Gt => hir::HirBinOp::Gt,
            BinOpKind::Ge => hir::HirBinOp::Ge,
            BinOpKind::And => hir::HirBinOp::And,
            BinOpKind::Or => hir::HirBinOp::Or,
            BinOpKind::BitOr => hir::HirBinOp::BitOr,
            BinOpKind::BitAnd => hir::HirBinOp::BitAnd,
            BinOpKind::BitXor => hir::HirBinOp::BitXor,
        }
    }

    /// Convert AST unary operator to HIR
    fn convert_unop_kind(&self, op: &UnOpKind) -> hir::HirUnOp {
        match op {
            UnOpKind::Neg => hir::HirUnOp::Neg,
            UnOpKind::Not => hir::HirUnOp::Not,
            UnOpKind::Deref => hir::HirUnOp::Deref,
            UnOpKind::Any(_) => {
                // For Any variants, default to Neg as a fallback
                // This handles custom unary operators that don't have direct HIR equivalents
                hir::HirUnOp::Neg
            }
        }
    }

    /// Transform parentheses expression to HIR (just unwrap the inner expression)
    fn transform_paren_to_hir(&mut self, paren: &ast::ExprParen) -> Result<hir::HirExprKind> {
        // Parentheses don't change semantics, just unwrap the inner expression
        let inner_expr = self.transform_expr_to_hir(&paren.expr)?;
        Ok(inner_expr.kind)
    }

    /// Transform format string to HIR - keep it as FormatString for later const evaluation
    fn transform_format_string_to_hir(
        &mut self,
        format_str: &ast::ExprFormatString,
    ) -> Result<hir::HirExprKind> {
        tracing::debug!(
            "Preserving structured format string with {} parts, {} args, and {} kwargs for const evaluation",
            format_str.parts.len(),
            format_str.args.len(),
            format_str.kwargs.len()
        );

        // For now, create a special HIR node that preserves the format string structure
        // This will be resolved during const evaluation when variable values are available

        // Transform all arguments to HIR
        let hir_args: Result<Vec<_>> = format_str
            .args
            .iter()
            .map(|arg| self.transform_expr_to_hir(arg))
            .collect();
        let hir_args = hir_args?;

        // Note: kwargs handling simplified for now

        // Create a special println function call that can be handled during const evaluation
        let println_path = hir::HirPath {
            segments: vec![hir::HirPathSegment {
                name: "println".to_string(),
                args: None,
            }],
            res: None,
        };

        // Encode the format string structure as a special argument
        // We'll pass the template as the first argument and the args as remaining arguments
        let mut template_str = String::new();
        for part in &format_str.parts {
            match part {
                ast::FormatTemplatePart::Literal(literal) => {
                    template_str.push_str(literal);
                }
                ast::FormatTemplatePart::Placeholder(_) => {
                    template_str.push_str("{}");
                }
            }
        }

        let mut call_args = vec![hir::HirExpr {
            hir_id: self.next_id(),
            kind: hir::HirExprKind::Literal(hir::HirLit::Str(template_str)),
            span: self.create_span(1),
        }];

        // Add the actual arguments
        call_args.extend(hir_args);

        tracing::debug!(
            "Created println call with template and {} arguments",
            call_args.len() - 1
        );

        // Convert HirPath to HirExpr for the function call
        let println_expr = Box::new(hir::HirExpr {
            hir_id: self.next_id(),
            kind: hir::HirExprKind::Path(println_path),
            span: self.create_span(1),
        });

        Ok(hir::HirExprKind::Call(println_expr, call_args))
    }

    /// Transform let expression to HIR
    fn transform_let_to_hir(&mut self, let_expr: &ast::ExprLet) -> Result<hir::HirExprKind> {
        // For now, create a simple local binding
        // In a more complete implementation, this would handle pattern matching
        let init_expr = self.transform_expr_to_hir(&let_expr.expr)?;

        // Create a simple assignment-like expression
        // This is a simplified implementation for basic let expressions
        Ok(hir::HirExprKind::Block(hir::HirBlock {
            hir_id: self.next_id(),
            stmts: vec![hir::HirStmt {
                hir_id: self.next_id(),
                kind: hir::HirStmtKind::Local(hir::HirLocal {
                    hir_id: self.next_id(),
                    pat: hir::HirPat {
                        hir_id: self.next_id(),
                        kind: hir::HirPatKind::Binding("var".to_string()),
                    },
                    ty: None,
                    init: Some(init_expr),
                }),
            }],
            expr: None,
        }))
    }

    /// Create a simple HIR literal expression
    pub fn create_simple_literal(&mut self, value: i64) -> hir::HirExpr {
        hir::HirExpr::new(
            self.next_id(),
            hir::HirExprKind::Literal(hir::HirLit::Integer(value)),
            Span::new(0, 0, 0),
        )
    }

    /// Create a simple HIR type
    pub fn create_simple_type(&mut self, type_name: &str) -> hir::HirTy {
        hir::HirTy::new(
            self.next_id(),
            hir::HirTyKind::Path(hir::HirPath {
                segments: vec![hir::HirPathSegment {
                    name: type_name.to_string(),
                    args: None,
                }],
                res: None,
            }),
            Span::new(0, 0, 0),
        )
    }
}

impl<'a> IrTransform<&'a ast::AstExpr, hir::HirProgram> for HirGenerator {
    fn transform(&mut self, source: &'a ast::AstExpr) -> Result<hir::HirProgram> {
        self.transform_expr(source)
    }
}

impl<'a> IrTransform<&'a ast::AstFile, hir::HirProgram> for HirGenerator {
    fn transform(&mut self, source: &'a ast::AstFile) -> Result<hir::HirProgram> {
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
            hir::HirExprKind::Literal(hir::HirLit::Integer(value)) => {
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
            hir::HirTyKind::Path(path) => {
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

        let ast_file = ast::AstFile {
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
                hir::HirItemKind::Struct(def) => def.name.clone(),
                hir::HirItemKind::Function(func) => func.sig.name.clone(),
                _ => String::new(),
            })
            .collect();

        assert!(names.contains(&"Point".to_string()));
        assert!(names.contains(&"add".to_string()));

        Ok(())
    }
}
