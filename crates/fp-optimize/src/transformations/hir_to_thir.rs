use fp_core::{hir, thir, types};
use fp_core::error::Result;
use fp_core::span::Span;
use std::collections::HashMap;

/// Generator for transforming HIR to THIR (Typed High-level IR)
/// This transformation performs type checking and inference
pub struct ThirGenerator {
    next_thir_id: thir::ThirId,
    type_context: TypeContext,
    body_map: HashMap<thir::BodyId, thir::ThirBody>,
    next_body_id: u32,
    /// Map constant names to their THIR body IDs for resolution
    const_symbols: HashMap<String, thir::BodyId>,
    /// Map constant names to their original HIR initializer expression for inlining
    const_init_map: HashMap<String, hir::HirExpr>,
}

/// Type checking context
struct TypeContext {
    function_signatures: HashMap<String, types::FnSig>,
}

impl ThirGenerator {
    /// Create a new THIR generator
    pub fn new() -> Self {
        Self {
            next_thir_id: 0,
            type_context: TypeContext::new(),
            body_map: HashMap::new(),
            next_body_id: 0,
            const_symbols: HashMap::new(),
            const_init_map: HashMap::new(),
        }
    }

    /// Transform HIR program to THIR
    pub fn transform(&mut self, hir_program: hir::HirProgram) -> Result<thir::ThirProgram> {
        let mut thir_program = thir::ThirProgram::new();

        // Pass 0: collect type information
        self.collect_type_info(&hir_program)?;

        // Pass 1: transform and register consts first so paths can resolve to them
        for hir_item in &hir_program.items {
            if let hir::HirItemKind::Const(const_def) = &hir_item.kind {
                let thir_const = self.transform_const(const_def.clone())?;
                let thir_id = self.next_id();
                let const_ty = self.hir_ty_to_ty(&const_def.ty)?;
                thir_program.items.push(thir::ThirItem {
                    thir_id,
                    kind: thir::ThirItemKind::Const(thir_const),
                    ty: const_ty,
                    span: hir_item.span,
                });
            }
        }

        // Pass 2: transform remaining items with consts available
        for hir_item in hir_program.items {
            if matches!(hir_item.kind, hir::HirItemKind::Const(_)) {
                continue; // already handled
            }
            let thir_item = self.transform_item(hir_item)?;
            thir_program.items.push(thir_item);
        }

        thir_program.bodies = self.body_map.clone();
        thir_program.next_thir_id = self.next_thir_id;

        Ok(thir_program)
    }

    /// Generate next THIR ID
    fn next_id(&mut self) -> thir::ThirId {
        let id = self.next_thir_id;
        self.next_thir_id += 1;
        id
    }

    /// Generate next body ID
    fn next_body_id(&mut self) -> thir::BodyId {
        let id = thir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;
        id
    }

    /// Collect type information from HIR program
    fn collect_type_info(&mut self, hir_program: &hir::HirProgram) -> Result<()> {
        for item in &hir_program.items {
            match &item.kind {
                hir::HirItemKind::Function(func) => {
                    let input_types = func.sig.inputs.iter()
                        .map(|param| self.hir_ty_to_ty(&param.ty))
                        .collect::<Result<Vec<_>>>()?;
                    
                    let output_type = self.hir_ty_to_ty(&func.sig.output)?;
                    
                    let fn_sig = types::FnSig {
                        inputs: input_types.into_iter().map(Box::new).collect(),
                        output: Box::new(output_type),
                        c_variadic: false,
                        unsafety: types::Unsafety::Normal,
                        abi: types::Abi::Rust,
                    };
                    
                    self.type_context.function_signatures.insert(
                        func.sig.name.clone(),
                        fn_sig
                    );
                }
                _ => {} // Handle other items as needed
            }
        }
        Ok(())
    }

    /// Transform HIR item to THIR
    fn transform_item(&mut self, hir_item: hir::HirItem) -> Result<thir::ThirItem> {
        let thir_id = self.next_id();
        
        let (kind, ty) = match hir_item.kind {
            hir::HirItemKind::Function(func) => {
                let thir_func = self.transform_function(func)?;
                let func_ty = self.get_function_type(&thir_func)?;
                (thir::ThirItemKind::Function(thir_func), func_ty)
            }
            hir::HirItemKind::Struct(struct_def) => {
                let thir_struct = self.transform_struct(struct_def)?;
                let struct_ty = self.get_struct_type(&thir_struct)?;
                (thir::ThirItemKind::Struct(thir_struct), struct_ty)
            }
            hir::HirItemKind::Const(const_def) => {
                // Record initializer for inlining and transform const
                self.const_init_map.insert(const_def.name.to_string(), const_def.body.value.clone());
                let thir_const = self.transform_const(const_def.clone())?;
                let const_ty = thir_const.ty.clone();
                (thir::ThirItemKind::Const(thir_const), const_ty)
            }
            _ => {
                return Ok(thir::ThirItem {
                    thir_id,
                    kind: thir::ThirItemKind::Function(thir::ThirFunction {
                        sig: thir::ThirFunctionSig {
                            inputs: vec![],
                            output: self.create_unit_type(),
                            c_variadic: false,
                        },
                        body_id: None,
                        is_const: false,
                    }),
                    ty: self.create_unit_type(),
                    span: hir_item.span,
                });
            }
        };

        Ok(thir::ThirItem {
            thir_id,
            kind,
            ty,
            span: hir_item.span,
        })
    }

    /// Transform HIR function to THIR
    fn transform_function(&mut self, hir_func: hir::HirFunction) -> Result<thir::ThirFunction> {
        let inputs = hir_func.sig.inputs.iter()
            .map(|param| self.hir_ty_to_ty(&param.ty))
            .collect::<Result<Vec<_>>>()?;
        
        let output = self.hir_ty_to_ty(&hir_func.sig.output)?;
        
        let sig = thir::ThirFunctionSig {
            inputs,
            output,
            c_variadic: false,
        };

        let body_id = if let Some(hir_body) = hir_func.body {
            let body_id = self.next_body_id();
            let thir_body = self.transform_body(hir_body)?;
            self.body_map.insert(body_id, thir_body);
            Some(body_id)
        } else {
            None
        };

        Ok(thir::ThirFunction {
            sig,
            body_id,
            is_const: hir_func.is_const,
        })
    }

    /// Transform HIR struct to THIR
    fn transform_struct(&mut self, hir_struct: hir::HirStruct) -> Result<thir::ThirStruct> {
        let fields = hir_struct.fields.into_iter()
            .map(|field| self.transform_struct_field(field))
            .collect::<Result<Vec<_>>>()?;

        Ok(thir::ThirStruct {
            fields,
            variant_data: thir::VariantData::Struct(Vec::new(), false),
        })
    }

    /// Transform HIR struct field to THIR
    fn transform_struct_field(&mut self, hir_field: hir::HirStructField) -> Result<thir::ThirStructField> {
        let thir_id = self.next_id();
        let ty = self.hir_ty_to_ty(&hir_field.ty)?;
        let vis = self.transform_visibility(hir_field.vis);

        Ok(thir::ThirStructField {
            thir_id,
            ty,
            vis,
        })
    }

    /// Transform HIR const to THIR
    fn transform_const(&mut self, hir_const: hir::HirConst) -> Result<thir::ThirConst> {
        let ty = self.hir_ty_to_ty(&hir_const.ty)?;
        let body_id = self.next_body_id();
        let thir_body = self.transform_body(hir_const.body)?;
        self.body_map.insert(body_id, thir_body);
        // Record constant symbol for later path resolution
        self.const_symbols.insert(hir_const.name.to_string(), body_id);

        Ok(thir::ThirConst {
            ty,
            body_id,
        })
    }

    /// Transform HIR body to THIR
    fn transform_body(&mut self, hir_body: hir::HirBody) -> Result<thir::ThirBody> {
        let value = self.transform_expr(hir_body.value)?;
        
        let params = hir_body.params.into_iter()
            .map(|param| {
                let ty = self.hir_ty_to_ty(&param.ty)?;
                Ok(thir::Param {
                    ty,
                    pat: None, // Simplified for now
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(thir::ThirBody {
            params,
            value,
            locals: Vec::new(), // Will be populated during MIR generation
        })
    }

    /// Transform HIR expression to THIR with type checking
    fn transform_expr(&mut self, hir_expr: hir::HirExpr) -> Result<thir::ThirExpr> {
        let thir_id = self.next_id();
        
        let (kind, ty) = match hir_expr.kind {
            hir::HirExprKind::Literal(lit) => {
                let (thir_lit, ty) = self.transform_literal(lit)?;
                (thir::ThirExprKind::Literal(thir_lit), ty)
            }
            hir::HirExprKind::Path(path) => {
                let ty = self.infer_path_type(&path)?;
                // Preserve simple identifiers as named paths so we can lower calls later
                let name = if path.segments.len() == 1 {
                    path.segments[0].name.clone()
                } else {
                    // Join segments for a simple dotted name, if needed
                    path.segments.iter().map(|s| s.name.clone()).collect::<Vec<_>>().join("::")
                };
                // If this is a reference to a known constant, inline its initializer
                if let Some(init_expr) = self.const_init_map.get(&name) {
                    let inlined = self.transform_expr(init_expr.clone())?;
                    (inlined.kind, inlined.ty)
                } else {
                    (thir::ThirExprKind::Path(name), ty)
                }
            }
            hir::HirExprKind::Binary(op, left, right) => {
                let left_thir = self.transform_expr(*left)?;
                let right_thir = self.transform_expr(*right)?;
                let op_thir = self.transform_binary_op(op);

                // Constant folding for simple integer ops when both sides are literals
                match (&left_thir.kind, &right_thir.kind, &op_thir) {
                    (
                        thir::ThirExprKind::Literal(thir::ThirLit::Int(a, _)),
                        thir::ThirExprKind::Literal(thir::ThirLit::Int(b, _)),
                        thir::BinOp::Add | thir::BinOp::Sub | thir::BinOp::Mul | thir::BinOp::Div,
                    ) => {
                        let val = match op_thir {
                            thir::BinOp::Add => a + b,
                            thir::BinOp::Sub => a - b,
                            thir::BinOp::Mul => a * b,
                            thir::BinOp::Div => if *b != 0 { a / b } else { *a },
                            _ => unreachable!(),
                        };
                        (thir::ThirExprKind::Literal(thir::ThirLit::Int(val, thir::IntTy::I32)), self.create_i32_type())
                    }
                    _ => {
                        // Type checking: ensure operands are compatible
                        let result_ty = self.check_binary_op_types(&left_thir.ty, &right_thir.ty, &op_thir)?;
                        (
                            thir::ThirExprKind::Binary(op_thir, Box::new(left_thir), Box::new(right_thir)),
                            result_ty,
                        )
                    }
                }
            }
            hir::HirExprKind::Call(func, args) => {
                let func_thir = self.transform_expr(*func)?;
                let args_thir: Vec<_> = args.into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                
                let return_ty = self.infer_call_return_type(&func_thir, &args_thir)?;
                
                (thir::ThirExprKind::Call {
                    fun: Box::new(func_thir),
                    args: args_thir,
                    from_hir_call: true,
                }, return_ty)
            }
            hir::HirExprKind::Return(expr) => {
                let return_ty = self.create_never_type();
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                (thir::ThirExprKind::Return {
                    value: expr_thir.map(Box::new),
                }, return_ty)
            }
            hir::HirExprKind::Block(block) => {
                let block_thir = self.transform_block(block)?;
                let block_ty = self.infer_block_type(&block_thir)?;
                (thir::ThirExprKind::Block(block_thir), block_ty)
            }
            hir::HirExprKind::Unary(op, expr) => {
                let expr_thir = self.transform_expr(*expr)?;
                let op_thir = self.transform_unary_op(op);
                let result_ty = self.check_unary_op_type(&expr_thir.ty, &op_thir)?;
                (thir::ThirExprKind::Unary(op_thir, Box::new(expr_thir)), result_ty)
            }
            hir::HirExprKind::MethodCall(receiver, method_name, args) => {
                // Convert method call to function call for simplicity
                let receiver_thir = self.transform_expr(*receiver)?;
                let args_thir: Vec<_> = args.into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                let return_ty = self.infer_method_call_return_type(&receiver_thir, &method_name, &args_thir)?;
                
                // Create a function call with method name
                let func_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Path(method_name),
                    ty: self.create_unit_type(), // Simplified
                    span: Span::new(0, 0, 0),
                };
                
                let mut all_args = vec![receiver_thir];
                all_args.extend(args_thir);
                
                (thir::ThirExprKind::Call {
                    fun: Box::new(func_expr),
                    args: all_args,
                    from_hir_call: true,
                }, return_ty)
            }
            hir::HirExprKind::FieldAccess(expr, field_name) => {
                // If base is a const struct, inline the specific field initializer
                if let hir::HirExprKind::Path(ref p) = expr.kind {
                    if p.segments.len() == 1 {
                        let base_name = p.segments[0].name.to_string();
                        if let Some(init) = self.const_init_map.get(&base_name) {
                            if let hir::HirExprKind::Struct(_path, fields) = &init.kind {
                                if let Some(f) = fields.iter().find(|f| f.name.to_string() == field_name) {
                                    let thir_expr = self.transform_expr(f.expr.clone())?;
                                    return Ok(thir_expr);
                                }
                            }
                        }
                    }
                }
                // Fallback: regular field selection
                let expr_thir = self.transform_expr(*expr)?;
                let field_ty = self.infer_field_type(&expr_thir.ty, &field_name)?;
                (thir::ThirExprKind::Field { base: Box::new(expr_thir), field_idx: 0 }, field_ty)
            }
            hir::HirExprKind::Struct(_path, _fields) => {
                // Struct expressions are complex - simplify to unit for now
                let unit_ty = self.create_unit_type();
                (thir::ThirExprKind::Literal(thir::ThirLit::Int(0, thir::IntTy::I32)), unit_ty)
            }
            hir::HirExprKind::If(cond, then_expr, else_expr) => {
                let cond_thir = self.transform_expr(*cond)?;
                let then_thir = self.transform_expr(*then_expr)?;
                let else_thir = else_expr.map(|e| self.transform_expr(*e)).transpose()?;
                let result_ty = if let Some(ref else_expr) = else_thir {
                    self.unify_types(&then_thir.ty, &else_expr.ty)?
                } else {
                    self.create_unit_type()
                };
                (thir::ThirExprKind::If {
                    cond: Box::new(cond_thir),
                    then: Box::new(then_thir),
                    else_opt: else_thir.map(Box::new),
                }, result_ty)
            }
            hir::HirExprKind::Let(pat, _ty, init) => {
                let init_thir = init.map(|e| self.transform_expr(*e)).transpose()?;
                let unit_ty = self.create_unit_type();
                if let Some(init_expr) = init_thir {
                    (thir::ThirExprKind::Let {
                        expr: Box::new(init_expr),
                        pat: self.transform_pattern(pat)?,
                    }, unit_ty)
                } else {
                    // No initializer, return unit literal
                    (thir::ThirExprKind::Literal(thir::ThirLit::Int(0, thir::IntTy::I32)), unit_ty)
                }
            }
            hir::HirExprKind::Assign(lhs, rhs) => {
                let lhs_thir = self.transform_expr(*lhs)?;
                let rhs_thir = self.transform_expr(*rhs)?;
                let unit_ty = self.create_unit_type();
                (thir::ThirExprKind::Assign {
                    lhs: Box::new(lhs_thir),
                    rhs: Box::new(rhs_thir),
                }, unit_ty)
            }
            hir::HirExprKind::Break(expr) => {
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                let never_ty = self.create_never_type();
                (thir::ThirExprKind::Break {
                    value: expr_thir.map(Box::new),
                }, never_ty)
            }
            hir::HirExprKind::Continue => {
                let never_ty = self.create_never_type();
                (thir::ThirExprKind::Continue, never_ty)
            }
            hir::HirExprKind::Loop(block) => {
                let block_thir = self.transform_block(block)?;
                let never_ty = self.create_never_type();
                // Convert block to expression
                let block_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Block(block_thir),
                    ty: never_ty.clone(),
                    span: Span::new(0, 0, 0),
                };
                (thir::ThirExprKind::Loop {
                    body: Box::new(block_expr),
                }, never_ty)
            }
            hir::HirExprKind::While(cond, block) => {
                // Convert while loop to infinite loop with conditional break
                let cond_thir = self.transform_expr(*cond)?;
                let block_thir = self.transform_block(block)?;
                let unit_ty = self.create_unit_type();
                let cond_span = cond_thir.span;

                let block_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Block(block_thir),
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                let break_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Break { value: None },
                    ty: self.create_never_type(),
                    span: cond_span,
                };

                let if_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::If {
                        cond: Box::new(cond_thir),
                        then: Box::new(block_expr.clone()),
                        else_opt: Some(Box::new(break_expr)),
                    },
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                let loop_body_block = thir::ThirBlock {
                    targeted_by_break: true,
                    region: 0,
                    span: Span::new(0, 0, 0),
                    stmts: vec![thir::ThirStmt {
                        kind: thir::ThirStmtKind::Expr(if_expr),
                    }],
                    expr: None,
                    safety_mode: thir::BlockSafetyMode::Safe,
                };

                let loop_body_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Block(loop_body_block),
                    ty: unit_ty.clone(),
                    span: Span::new(0, 0, 0),
                };

                (thir::ThirExprKind::Loop {
                    body: Box::new(loop_body_expr),
                }, unit_ty)
            }
        };

        Ok(thir::ThirExpr {
            thir_id,
            kind,
            ty,
            span: hir_expr.span,
        })
    }

    /// Transform HIR block to THIR
    fn transform_block(&mut self, hir_block: hir::HirBlock) -> Result<thir::ThirBlock> {
        let stmts = hir_block.stmts.into_iter()
            .map(|stmt| self.transform_stmt(stmt))
            .collect::<Result<Vec<_>>>()?;
        
        let expr = hir_block.expr.map(|e| self.transform_expr(*e)).transpose()?;

        Ok(thir::ThirBlock {
            targeted_by_break: false,
            region: 0, // Simplified scope handling
            span: Span::new(0, 0, 0), // Will be updated with proper span
            stmts,
            expr: expr.map(Box::new),
            safety_mode: thir::BlockSafetyMode::Safe,
        })
    }

    /// Transform HIR statement to THIR
    fn transform_stmt(&mut self, hir_stmt: hir::HirStmt) -> Result<thir::ThirStmt> {
        let kind = match hir_stmt.kind {
            hir::HirStmtKind::Expr(expr) => {
                let thir_expr = self.transform_expr(expr)?;
                thir::ThirStmtKind::Expr(thir_expr)
            }
            hir::HirStmtKind::Semi(expr) => {
                let thir_expr = self.transform_expr(expr)?;
                thir::ThirStmtKind::Expr(thir_expr)
            }
            hir::HirStmtKind::Local(local) => {
                // Record initializer for inlining when encountering Path(name)
                if let hir::HirPatKind::Binding(name) = &local.pat.kind {
                    if let Some(init) = &local.init {
                        self.const_init_map.insert(name.to_string(), init.clone());
                    }
                }
                // Lower to THIR Let with transformed initializer, so evaluation order is preserved
                let init_expr = match &local.init {
                    Some(init) => Some(self.transform_expr(init.clone())?),
                    None => None,
                };
                thir::ThirStmtKind::Let {
                    remainder_scope: 0,
                    init_scope: 0,
                    pattern: self.create_wildcard_pattern(),
                    initializer: init_expr,
                    lint_level: 0,
                }
            }
            _ => thir::ThirStmtKind::Expr(thir::ThirExpr {
                thir_id: self.next_id(),
                kind: thir::ThirExprKind::Literal(thir::ThirLit::Int(0, thir::IntTy::I32)),
                ty: self.create_i32_type(),
                span: Span::new(0, 0, 0),
            }),
        };

        Ok(thir::ThirStmt { kind })
    }

    /// Transform HIR literal to THIR with type inference
    fn transform_literal(&mut self, hir_lit: hir::HirLit) -> Result<(thir::ThirLit, types::Ty)> {
        let (thir_lit, ty) = match hir_lit {
            hir::HirLit::Bool(b) => (thir::ThirLit::Bool(b), types::Ty::bool()),
            hir::HirLit::Integer(i) => (thir::ThirLit::Int(i as i128, thir::IntTy::I32), types::Ty::int(types::IntTy::I32)),
            hir::HirLit::Float(f) => (thir::ThirLit::Float(f, thir::FloatTy::F64), types::Ty::float(types::FloatTy::F64)),
            hir::HirLit::Str(s) => (thir::ThirLit::Str(s), self.create_string_type()),
            hir::HirLit::Char(c) => (thir::ThirLit::Char(c), types::Ty::char()),
        };
        Ok((thir_lit, ty))
    }

    /// Transform HIR binary operator to THIR
    fn transform_binary_op(&self, hir_op: hir::HirBinOp) -> thir::BinOp {
        match hir_op {
            hir::HirBinOp::Add => thir::BinOp::Add,
            hir::HirBinOp::Sub => thir::BinOp::Sub,
            hir::HirBinOp::Mul => thir::BinOp::Mul,
            hir::HirBinOp::Div => thir::BinOp::Div,
            hir::HirBinOp::Rem => thir::BinOp::Rem,
            hir::HirBinOp::BitXor => thir::BinOp::BitXor,
            hir::HirBinOp::BitAnd => thir::BinOp::BitAnd,
            hir::HirBinOp::BitOr => thir::BinOp::BitOr,
            hir::HirBinOp::Shl => thir::BinOp::Shl,
            hir::HirBinOp::Shr => thir::BinOp::Shr,
            hir::HirBinOp::Eq => thir::BinOp::Eq,
            hir::HirBinOp::Ne => thir::BinOp::Ne,
            hir::HirBinOp::Lt => thir::BinOp::Lt,
            hir::HirBinOp::Le => thir::BinOp::Le,
            hir::HirBinOp::Gt => thir::BinOp::Gt,
            hir::HirBinOp::Ge => thir::BinOp::Ge,
            _ => thir::BinOp::Add, // Fallback
        }
    }

    /// Convert HIR type to types::Ty
    fn hir_ty_to_ty(&self, hir_ty: &hir::HirTy) -> Result<types::Ty> {
        let ty_kind = match &hir_ty.kind {
            hir::HirTyKind::Path(path) => {
                // Simple path-to-type mapping
                if let Some(segment) = path.segments.first() {
                    match segment.name.as_str() {
                        "bool" => types::TyKind::Bool,
                        "i32" => types::TyKind::Int(types::IntTy::I32),
                        "i64" => types::TyKind::Int(types::IntTy::I64),
                        "f64" => types::TyKind::Float(types::FloatTy::F64),
                        "str" => return Ok(self.create_string_type()),
                        _ => types::TyKind::Int(types::IntTy::I32), // Default fallback
                    }
                } else {
                    types::TyKind::Int(types::IntTy::I32)
                }
            }
            _ => types::TyKind::Int(types::IntTy::I32), // Simplified fallback
        };

        Ok(types::Ty::new(ty_kind))
    }

    /// Type checking for binary operations
    fn check_binary_op_types(&self, left_ty: &types::Ty, right_ty: &types::Ty, _op: &thir::BinOp) -> Result<types::Ty> {
        // Simplified type checking - assume same types for operands
        if left_ty == right_ty {
            Ok(left_ty.clone())
        } else {
            // Try to find a common type
            Ok(left_ty.clone())
        }
    }

    /// Infer type from HIR path
    fn infer_path_type(&self, _path: &hir::HirPath) -> Result<types::Ty> {
        // Simplified - return i32 for now
        Ok(types::Ty::int(types::IntTy::I32))
    }

    /// Infer return type of function call
    fn infer_call_return_type(&self, _func: &thir::ThirExpr, _args: &[thir::ThirExpr]) -> Result<types::Ty> {
        // Simplified - return i32 for now
        Ok(types::Ty::int(types::IntTy::I32))
    }

    /// Infer type of block expression
    fn infer_block_type(&self, block: &thir::ThirBlock) -> Result<types::Ty> {
        if let Some(expr) = &block.expr {
            Ok(expr.ty.clone())
        } else {
            Ok(self.create_unit_type())
        }
    }

    /// Get function type
    fn get_function_type(&self, _func: &thir::ThirFunction) -> Result<types::Ty> {
        // Simplified - return function type
        Ok(types::Ty::new(types::TyKind::FnDef(0, Vec::new())))
    }

    /// Get struct type
    fn get_struct_type(&self, _struct_def: &thir::ThirStruct) -> Result<types::Ty> {
        // Simplified - return struct type
        Ok(types::Ty::new(types::TyKind::Adt(types::AdtDef {
            did: 0,
            variants: Vec::new(),
            flags: types::AdtFlags::IS_STRUCT,
            repr: types::ReprOptions {
                int: None,
                align: None,
                pack: None,
                flags: types::ReprFlags::empty(),
                field_shuffle_seed: 0,
            },
        }, Vec::new())))
    }

    /// Transform visibility
    fn transform_visibility(&self, hir_vis: hir::HirVisibility) -> thir::Visibility {
        match hir_vis {
            hir::HirVisibility::Public => thir::Visibility::Public,
            hir::HirVisibility::Private => thir::Visibility::Inherited,
        }
    }

    // Helper methods for creating common types
    fn create_unit_type(&self) -> types::Ty {
        types::Ty::new(types::TyKind::Tuple(Vec::new()))
    }

    fn create_never_type(&self) -> types::Ty {
        types::Ty::never()
    }

    fn create_i32_type(&self) -> types::Ty {
        types::Ty::int(types::IntTy::I32)
    }

    fn create_string_type(&self) -> types::Ty {
        // Simplified string type representation
        types::Ty::new(types::TyKind::RawPtr(types::TypeAndMut {
            ty: Box::new(types::Ty::int(types::IntTy::I8)),
            mutbl: types::Mutability::Not,
        }))
    }

    fn create_wildcard_pattern(&mut self) -> thir::ThirPat {
        thir::ThirPat {
            thir_id: self.next_id(),
            kind: thir::ThirPatKind::Wild,
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        }
    }

    /// Transform unary operator
    fn transform_unary_op(&self, op: hir::HirUnOp) -> thir::UnOp {
        match op {
            hir::HirUnOp::Not => thir::UnOp::Not,
            hir::HirUnOp::Neg => thir::UnOp::Neg,
            hir::HirUnOp::Deref => thir::UnOp::Not, // Deref not available, use Not as fallback
        }
    }

    /// Check type for unary operations
    fn check_unary_op_type(&self, operand_ty: &types::Ty, _op: &thir::UnOp) -> Result<types::Ty> {
        // Simplified type checking - return the operand type
        Ok(operand_ty.clone())
    }

    /// Infer method call return type
    fn infer_method_call_return_type(&self, _receiver: &thir::ThirExpr, _method_name: &str, _args: &[thir::ThirExpr]) -> Result<types::Ty> {
        // Simplified - return unit type for method calls
        Ok(self.create_unit_type())
    }

    /// Infer field type
    fn infer_field_type(&self, _struct_ty: &types::Ty, _field_name: &str) -> Result<types::Ty> {
        // Simplified - return i32 for field access
        Ok(types::Ty::int(types::IntTy::I32))
    }


    /// Unify two types
    fn unify_types(&self, ty1: &types::Ty, ty2: &types::Ty) -> Result<types::Ty> {
        // Simplified type unification - return the first type
        if ty1 == ty2 {
            Ok(ty1.clone())
        } else {
            Ok(ty1.clone())
        }
    }

    /// Transform pattern
    fn transform_pattern(&mut self, _pat: hir::HirPat) -> Result<thir::ThirPat> {
        Ok(thir::ThirPat {
            thir_id: self.next_id(),
            kind: thir::ThirPatKind::Wild, // Simplified
            ty: self.create_unit_type(),
            span: Span::new(0, 0, 0),
        })
    }
}

impl TypeContext {
    fn new() -> Self {
        Self {
            function_signatures: HashMap::new(),
        }
    }
}

impl Default for ThirGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_thir_generator_creation() {
        let generator = ThirGenerator::new();
        assert_eq!(generator.next_thir_id, 0);
    }

    #[test]
    fn test_literal_transformation() -> Result<()> {
        let mut generator = ThirGenerator::new();
        let (thir_lit, ty) = generator.transform_literal(hir::HirLit::Integer(42)).unwrap();
        
        match thir_lit {
            thir::ThirLit::Int(value, thir::IntTy::I32) => {
                assert_eq!(value, 42);
            }
            _ => return Err(crate::error::optimization_error("Expected i32 literal".to_string())),
        }
        
        assert!(ty.is_integral());
        Ok(())
    }

    #[test]
    fn test_binary_op_transformation() {
        let generator = ThirGenerator::new();
        let op = generator.transform_binary_op(hir::HirBinOp::Add);
        assert_eq!(op, thir::BinOp::Add);
    }
}
