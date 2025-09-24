use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::Result;
use fp_core::span::Span;
use fp_core::{hir, thir, types};
use std::collections::HashMap;
use std::mem;

use super::IrTransform;

/// Generator for transforming HIR to THIR (Typed High-level IR)
/// This transformation performs type checking and inference
pub struct ThirGenerator {
    next_thir_id: thir::ThirId,
    type_context: TypeContext,
    body_map: HashMap<thir::BodyId, thir::ThirBody>,
    next_body_id: u32,
    /// Map constant names to their THIR body IDs for resolution
    const_symbols: HashMap<types::DefId, thir::BodyId>,
    /// Map constant names to their original HIR initializer expression for inlining
    const_init_map: HashMap<types::DefId, hir::HirExpr>,
    /// Track simple local bindings we can inline by name.
    binding_inits: HashMap<String, hir::HirExpr>,
}

/// Type checking context
#[derive(Clone)]
struct StructInfo {
    adt_def: types::AdtDef,
    field_map: HashMap<String, (usize, types::Ty)>,
}

struct TypeContext {
    function_signatures: HashMap<types::DefId, types::FnSig>,
    method_signatures: HashMap<types::DefId, HashMap<String, types::FnSig>>,
    structs: HashMap<types::DefId, StructInfo>,
    struct_names: HashMap<String, types::DefId>,
    value_names: HashMap<String, types::DefId>,
    const_types: HashMap<types::DefId, types::Ty>,
    next_def_id: types::DefId,
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
            binding_inits: HashMap::new(),
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
                let def_id = hir_item.def_id as types::DefId;
                self.const_init_map
                    .insert(def_id, const_def.body.value.clone());
                let thir_const = self.transform_const(Some(def_id), const_def.clone())?;
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
            if let hir::HirItemKind::Struct(struct_def) = &item.kind {
                self.type_context
                    .declare_struct(item.def_id as types::DefId, &struct_def.name);
            }
        }

        for item in &hir_program.items {
            match &item.kind {
                hir::HirItemKind::Function(func) => {
                    let fn_sig = self.build_fn_sig(&func.sig)?;
                    self.type_context.register_function(
                        item.def_id as types::DefId,
                        &func.sig.name,
                        fn_sig,
                    );
                }
                hir::HirItemKind::Struct(struct_def) => {
                    let fields = struct_def
                        .fields
                        .iter()
                        .map(|field| {
                            let ty = self.hir_ty_to_ty(&field.ty)?;
                            Ok((field.name.clone(), ty))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    self.type_context
                        .init_struct_fields(item.def_id as types::DefId, fields);
                }
                hir::HirItemKind::Const(const_def) => {
                    let ty = self.hir_ty_to_ty(&const_def.ty)?;
                    self.type_context.register_const(
                        item.def_id as types::DefId,
                        &const_def.name,
                        ty,
                    );
                }
                hir::HirItemKind::Impl(impl_block) => {
                    if let Some(owner_def_id) = self.resolve_ty_def_id(&impl_block.self_ty)? {
                        for impl_item in &impl_block.items {
                            if let hir::HirImplItemKind::Method(method) = &impl_item.kind {
                                let fn_sig = self.build_fn_sig(&method.sig)?;
                                self.type_context.register_method(
                                    owner_def_id,
                                    &method.sig.name,
                                    fn_sig,
                                );
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn build_fn_sig(&mut self, sig: &hir::HirFunctionSig) -> Result<types::FnSig> {
        let input_types = sig
            .inputs
            .iter()
            .map(|param| self.hir_ty_to_ty(&param.ty))
            .collect::<Result<Vec<_>>>()?;

        let output_type = self.hir_ty_to_ty(&sig.output)?;

        Ok(types::FnSig {
            inputs: input_types.into_iter().map(Box::new).collect(),
            output: Box::new(output_type),
            c_variadic: false,
            unsafety: types::Unsafety::Normal,
            abi: types::Abi::Rust,
        })
    }

    fn resolve_ty_def_id(&mut self, hir_ty: &hir::HirTy) -> Result<Option<types::DefId>> {
        let ty = self.hir_ty_to_ty(hir_ty)?;
        Ok(Self::type_def_id(&ty))
    }

    fn type_def_id(ty: &types::Ty) -> Option<types::DefId> {
        match &ty.kind {
            types::TyKind::Adt(adt_def, _) => Some(adt_def.did),
            types::TyKind::Ref(_, inner, _) => Self::type_def_id(inner),
            _ => None,
        }
    }

    fn make_primitive_ty(&self, name: &str) -> Option<types::Ty> {
        match name {
            "bool" => Some(types::Ty::bool()),
            "char" => Some(types::Ty::char()),
            "str" => Some(types::Ty::new(types::TyKind::Slice(Box::new(
                types::Ty::char(),
            )))),
            "String" => Some(types::Ty::new(types::TyKind::Adt(
                types::AdtDef {
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
                },
                Vec::new(),
            ))),
            "i8" => Some(types::Ty::int(types::IntTy::I8)),
            "i16" => Some(types::Ty::int(types::IntTy::I16)),
            "i32" => Some(types::Ty::int(types::IntTy::I32)),
            "i64" => Some(types::Ty::int(types::IntTy::I64)),
            "i128" => Some(types::Ty::int(types::IntTy::I128)),
            "isize" => Some(types::Ty::int(types::IntTy::Isize)),
            "u8" => Some(types::Ty::uint(types::UintTy::U8)),
            "u16" => Some(types::Ty::uint(types::UintTy::U16)),
            "u32" => Some(types::Ty::uint(types::UintTy::U32)),
            "u64" => Some(types::Ty::uint(types::UintTy::U64)),
            "u128" => Some(types::Ty::uint(types::UintTy::U128)),
            "usize" => Some(types::Ty::uint(types::UintTy::Usize)),
            "f32" => Some(types::Ty::float(types::FloatTy::F32)),
            "f64" => Some(types::Ty::float(types::FloatTy::F64)),
            _ => None,
        }
    }

    fn path_to_type_info(
        &mut self,
        path: &hir::HirPath,
    ) -> Result<(Option<types::DefId>, String, String, types::SubstsRef)> {
        if let Some(segment) = path.segments.last() {
            let base_name = segment.name.clone();
            let qualified = path
                .segments
                .iter()
                .map(|seg| seg.name.clone())
                .collect::<Vec<_>>()
                .join("::");
            let substs = if let Some(args) = &segment.args {
                self.convert_hir_generic_args(args)?
            } else {
                Vec::new()
            };
            let def_id = match path.res {
                Some(hir::Res::Def(id)) => Some(id as types::DefId),
                _ => None,
            };
            Ok((def_id, qualified, base_name, substs))
        } else {
            Err(crate::error::optimization_error(
                "Encountered empty path while lowering type".to_string(),
            ))
        }
    }

    fn convert_hir_generic_args(&mut self, args: &hir::HirGenericArgs) -> Result<types::SubstsRef> {
        let mut substs = Vec::new();
        for arg in &args.args {
            match arg {
                hir::HirGenericArg::Type(ty) => {
                    substs.push(types::GenericArg::Type(self.hir_ty_to_ty(ty)?));
                }
                hir::HirGenericArg::Const(_) => {
                    substs.push(types::GenericArg::Const(types::ConstKind::Value(
                        types::ConstValue::ZeroSized,
                    )));
                }
            }
        }
        Ok(substs)
    }

    /// Transform HIR item to THIR
    fn transform_item(&mut self, hir_item: hir::HirItem) -> Result<thir::ThirItem> {
        let thir_id = self.next_id();
        let def_id = hir_item.def_id as types::DefId;
        let span = hir_item.span;

        let (kind, ty) = match hir_item.kind {
            hir::HirItemKind::Function(func) => {
                let thir_func = self.transform_function(func)?;
                let func_ty = self.get_function_type(def_id, &thir_func)?;
                (thir::ThirItemKind::Function(thir_func), func_ty)
            }
            hir::HirItemKind::Struct(struct_def) => {
                let struct_ty = self
                    .type_context
                    .make_struct_ty_by_id(def_id, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type());
                let thir_struct = self.transform_struct(struct_def)?;
                (thir::ThirItemKind::Struct(thir_struct), struct_ty)
            }
            hir::HirItemKind::Const(const_def) => {
                // Record initializer for inlining and transform const
                self.const_init_map
                    .insert(def_id, const_def.body.value.clone());
                let thir_const = self.transform_const(Some(def_id), const_def)?;
                let const_ty = thir_const.ty.clone();
                (thir::ThirItemKind::Const(thir_const), const_ty)
            }
            hir::HirItemKind::Impl(impl_block) => {
                let thir_impl = self.transform_impl(impl_block)?;
                let self_ty = thir_impl.self_ty.clone();
                (thir::ThirItemKind::Impl(thir_impl), self_ty)
            }
        };

        Ok(thir::ThirItem {
            thir_id,
            kind,
            ty,
            span,
        })
    }

    /// Transform HIR function to THIR
    fn transform_function(&mut self, hir_func: hir::HirFunction) -> Result<thir::ThirFunction> {
        let inputs = hir_func
            .sig
            .inputs
            .iter()
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

    fn transform_impl(&mut self, hir_impl: hir::HirImpl) -> Result<thir::ThirImpl> {
        let self_ty = self.hir_ty_to_ty(&hir_impl.self_ty)?;

        let mut items = Vec::new();
        for item in hir_impl.items {
            match item.kind {
                hir::HirImplItemKind::Method(method) => {
                    let thir_method = self.transform_function(method)?;
                    items.push(thir::ThirImplItem {
                        thir_id: self.next_id(),
                        kind: thir::ThirImplItemKind::Method(thir_method),
                    });
                }
                hir::HirImplItemKind::AssocConst(const_item) => {
                    let thir_const = self.transform_const(None, const_item)?;
                    items.push(thir::ThirImplItem {
                        thir_id: self.next_id(),
                        kind: thir::ThirImplItemKind::AssocConst(thir_const),
                    });
                }
            }
        }

        Ok(thir::ThirImpl {
            self_ty,
            items,
            trait_ref: hir_impl.trait_ty.map(|_| ()),
        })
    }

    /// Transform HIR struct to THIR
    fn transform_struct(&mut self, hir_struct: hir::HirStruct) -> Result<thir::ThirStruct> {
        let fields = hir_struct
            .fields
            .into_iter()
            .map(|field| self.transform_struct_field(field))
            .collect::<Result<Vec<_>>>()?;

        Ok(thir::ThirStruct {
            fields,
            variant_data: thir::VariantData::Struct(Vec::new(), false),
        })
    }

    /// Transform HIR struct field to THIR
    fn transform_struct_field(
        &mut self,
        hir_field: hir::HirStructField,
    ) -> Result<thir::ThirStructField> {
        let thir_id = self.next_id();
        let ty = self.hir_ty_to_ty(&hir_field.ty)?;
        let vis = self.transform_visibility(hir_field.vis);

        Ok(thir::ThirStructField { thir_id, ty, vis })
    }

    /// Transform HIR const to THIR
    fn transform_const(
        &mut self,
        def_id: Option<types::DefId>,
        hir_const: hir::HirConst,
    ) -> Result<thir::ThirConst> {
        let ty = self.hir_ty_to_ty(&hir_const.ty)?;
        let body_id = self.next_body_id();
        let thir_body = self.transform_body(hir_const.body)?;
        self.body_map.insert(body_id, thir_body);
        if let Some(id) = def_id {
            self.const_symbols.insert(id, body_id);
        }

        Ok(thir::ThirConst { ty, body_id })
    }

    /// Transform HIR body to THIR
    fn transform_body(&mut self, hir_body: hir::HirBody) -> Result<thir::ThirBody> {
        let saved_bindings = mem::take(&mut self.binding_inits);
        let value = self.transform_expr(hir_body.value)?;

        let params = hir_body
            .params
            .into_iter()
            .map(|param| {
                let ty = self.hir_ty_to_ty(&param.ty)?;
                Ok(thir::Param {
                    ty,
                    pat: None, // Simplified for now
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let body = thir::ThirBody {
            params,
            value,
            locals: Vec::new(), // Will be populated during MIR generation
        };

        self.binding_inits = saved_bindings;

        Ok(body)
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
                let (def_id_opt, qualified_name, base_name, _substs) =
                    self.path_to_type_info(&path)?;
                let resolved_def_id = def_id_opt
                    .or_else(|| self.type_context.lookup_value_def_id(&qualified_name))
                    .or_else(|| self.type_context.lookup_value_def_id(&base_name));
                let display_name = qualified_name.clone();

                if let Some(def_id) = resolved_def_id {
                    if let Some(init_expr) = self.const_init_map.get(&def_id) {
                        let inlined = self.transform_expr(init_expr.clone())?;
                        (inlined.kind, inlined.ty)
                    } else {
                        (
                            thir::ThirExprKind::Path(thir::ItemRef {
                                name: display_name,
                                def_id: Some(def_id),
                            }),
                            ty,
                        )
                    }
                } else if let Some(init_expr) = self.binding_inits.get(&base_name) {
                    let inlined = self.transform_expr(init_expr.clone())?;
                    (inlined.kind, inlined.ty)
                } else {
                    (
                        thir::ThirExprKind::Path(thir::ItemRef {
                            name: display_name,
                            def_id: None,
                        }),
                        ty,
                    )
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
                            thir::BinOp::Div => {
                                if *b != 0 {
                                    a / b
                                } else {
                                    *a
                                }
                            }
                            _ => unreachable!(),
                        };
                        (
                            thir::ThirExprKind::Literal(thir::ThirLit::Int(val, thir::IntTy::I32)),
                            self.create_i32_type(),
                        )
                    }
                    _ => {
                        // Type checking: ensure operands are compatible
                        let result_ty =
                            self.check_binary_op_types(&left_thir.ty, &right_thir.ty, &op_thir)?;
                        (
                            thir::ThirExprKind::Binary(
                                op_thir,
                                Box::new(left_thir),
                                Box::new(right_thir),
                            ),
                            result_ty,
                        )
                    }
                }
            }
            hir::HirExprKind::Call(func, args) => {
                let func_thir = self.transform_expr(*func)?;
                let args_thir: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;

                let return_ty = self.infer_call_return_type(&func_thir, &args_thir)?;

                (
                    thir::ThirExprKind::Call {
                        fun: Box::new(func_thir),
                        args: args_thir,
                        from_hir_call: true,
                    },
                    return_ty,
                )
            }
            hir::HirExprKind::Return(expr) => {
                let return_ty = self.create_never_type();
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                (
                    thir::ThirExprKind::Return {
                        value: expr_thir.map(Box::new),
                    },
                    return_ty,
                )
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
                (
                    thir::ThirExprKind::Unary(op_thir, Box::new(expr_thir)),
                    result_ty,
                )
            }
            hir::HirExprKind::MethodCall(receiver, method_name, args) => {
                // Convert method call to function call for simplicity
                let receiver_thir = self.transform_expr(*receiver)?;
                let args_thir: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                let return_ty =
                    self.infer_method_call_return_type(&receiver_thir, &method_name, &args_thir)?;

                // Create a function call with method name
                let func_expr = thir::ThirExpr {
                    thir_id: self.next_id(),
                    kind: thir::ThirExprKind::Path(thir::ItemRef {
                        name: method_name.clone(),
                        def_id: self.type_context.lookup_value_def_id(&method_name),
                    }),
                    ty: self.create_unit_type(), // Simplified
                    span: Span::new(0, 0, 0),
                };

                let mut all_args = vec![receiver_thir];
                all_args.extend(args_thir);

                (
                    thir::ThirExprKind::Call {
                        fun: Box::new(func_expr),
                        args: all_args,
                        from_hir_call: true,
                    },
                    return_ty,
                )
            }
            hir::HirExprKind::FieldAccess(expr, field_name) => {
                // If base is a const struct, inline the specific field initializer
                if let hir::HirExprKind::Path(ref p) = expr.kind {
                    let (base_def_id_opt, qualified_name, base_name, _) =
                        self.path_to_type_info(p)?;
                    let init_expr = base_def_id_opt
                        .or_else(|| self.type_context.lookup_value_def_id(&qualified_name))
                        .or_else(|| self.type_context.lookup_value_def_id(&base_name))
                        .and_then(|id| self.const_init_map.get(&id))
                        .or_else(|| self.binding_inits.get(&base_name));

                    if let Some(init) = init_expr {
                        if let hir::HirExprKind::Struct(_path, fields) = &init.kind {
                            if let Some(field) =
                                fields.iter().find(|f| f.name.to_string() == field_name)
                            {
                                let thir_expr = self.transform_expr(field.expr.clone())?;
                                return Ok(thir_expr);
                            }
                        }
                    }
                }
                // Fallback: regular field selection
                let hir_base = *expr;
                let expr_thir = self.transform_expr(hir_base)?;
                if let Some((idx, field_ty)) = self
                    .type_context
                    .lookup_field_info(&expr_thir.ty, &field_name)
                {
                    (
                        thir::ThirExprKind::Field {
                            base: Box::new(expr_thir),
                            field_idx: idx,
                        },
                        field_ty,
                    )
                } else {
                    (
                        thir::ThirExprKind::Field {
                            base: Box::new(expr_thir),
                            field_idx: 0,
                        },
                        types::Ty::int(types::IntTy::I32),
                    )
                }
            }
            hir::HirExprKind::Struct(path, _fields) => {
                let (def_id_opt, qualified_name, base_name, substs) =
                    self.path_to_type_info(&path)?;
                let struct_ty = def_id_opt
                    .or_else(|| self.type_context.lookup_struct_def_id(&qualified_name))
                    .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
                    .and_then(|id| self.type_context.make_struct_ty_by_id(id, substs))
                    .unwrap_or_else(|| self.create_unit_type());
                (
                    thir::ThirExprKind::Literal(thir::ThirLit::Int(0, thir::IntTy::I32)),
                    struct_ty,
                )
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
                (
                    thir::ThirExprKind::If {
                        cond: Box::new(cond_thir),
                        then: Box::new(then_thir),
                        else_opt: else_thir.map(Box::new),
                    },
                    result_ty,
                )
            }
            hir::HirExprKind::Let(pat, _ty, init) => {
                let init_thir = init.map(|e| self.transform_expr(*e)).transpose()?;
                let unit_ty = self.create_unit_type();
                if let Some(init_expr) = init_thir {
                    (
                        thir::ThirExprKind::Let {
                            expr: Box::new(init_expr),
                            pat: self.transform_pattern(pat)?,
                        },
                        unit_ty,
                    )
                } else {
                    // No initializer, return unit literal
                    (
                        thir::ThirExprKind::Literal(thir::ThirLit::Int(0, thir::IntTy::I32)),
                        unit_ty,
                    )
                }
            }
            hir::HirExprKind::Assign(lhs, rhs) => {
                let lhs_thir = self.transform_expr(*lhs)?;
                let rhs_thir = self.transform_expr(*rhs)?;
                let unit_ty = self.create_unit_type();
                (
                    thir::ThirExprKind::Assign {
                        lhs: Box::new(lhs_thir),
                        rhs: Box::new(rhs_thir),
                    },
                    unit_ty,
                )
            }
            hir::HirExprKind::Break(expr) => {
                let expr_thir = expr.map(|e| self.transform_expr(*e)).transpose()?;
                let never_ty = self.create_never_type();
                (
                    thir::ThirExprKind::Break {
                        value: expr_thir.map(Box::new),
                    },
                    never_ty,
                )
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
                (
                    thir::ThirExprKind::Loop {
                        body: Box::new(block_expr),
                    },
                    never_ty,
                )
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

                (
                    thir::ThirExprKind::Loop {
                        body: Box::new(loop_body_expr),
                    },
                    unit_ty,
                )
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
        let stmts = hir_block
            .stmts
            .into_iter()
            .map(|stmt| self.transform_stmt(stmt))
            .collect::<Result<Vec<_>>>()?;

        let expr = hir_block
            .expr
            .map(|e| self.transform_expr(*e))
            .transpose()?;

        Ok(thir::ThirBlock {
            targeted_by_break: false,
            region: 0,                // Simplified scope handling
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
                        self.binding_inits.insert(name.to_string(), init.clone());
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
            hir::HirLit::Integer(i) => (
                thir::ThirLit::Int(i as i128, thir::IntTy::I32),
                types::Ty::int(types::IntTy::I32),
            ),
            hir::HirLit::Float(f) => (
                thir::ThirLit::Float(f, thir::FloatTy::F64),
                types::Ty::float(types::FloatTy::F64),
            ),
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
    fn hir_ty_to_ty(&mut self, hir_ty: &hir::HirTy) -> Result<types::Ty> {
        match &hir_ty.kind {
            hir::HirTyKind::Primitive(prim) => Ok(self.primitive_ty_to_ty(prim)),
            hir::HirTyKind::Path(path) => {
                let (def_id_opt, qualified_name, base_name, substs) =
                    self.path_to_type_info(path)?;

                if let Some(primitive) = self.make_primitive_ty(&base_name) {
                    return Ok(primitive);
                }

                if let Some(def_id) = def_id_opt {
                    if let Some(ty) = self
                        .type_context
                        .make_struct_ty_by_id(def_id, substs.clone())
                    {
                        return Ok(ty);
                    }

                    if let Some(const_ty) = self.type_context.lookup_const_type(def_id) {
                        return Ok(const_ty.clone());
                    }

                    if self
                        .type_context
                        .lookup_function_signature(def_id)
                        .is_some()
                    {
                        return Ok(types::Ty::new(types::TyKind::FnDef(def_id, substs)));
                    }
                }

                if let Some(struct_id) = self
                    .type_context
                    .lookup_struct_def_id(&qualified_name)
                    .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
                {
                    if let Some(ty) = self
                        .type_context
                        .make_struct_ty_by_id(struct_id, substs.clone())
                    {
                        return Ok(ty);
                    }
                }

                let lookup_name = qualified_name.clone();
                let stub_id = self.type_context.ensure_struct_stub(&lookup_name);
                Ok(self
                    .type_context
                    .make_struct_ty_by_id(stub_id, substs)
                    .unwrap_or_else(|| self.create_unit_type()))
            }
            hir::HirTyKind::Tuple(elements) => {
                let tys = elements
                    .iter()
                    .map(|ty| Ok(Box::new(self.hir_ty_to_ty(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(types::Ty::new(types::TyKind::Tuple(tys)))
            }
            hir::HirTyKind::Ref(inner) => {
                let inner_ty = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::Ref(
                    types::Region::ReStatic,
                    Box::new(inner_ty),
                    types::Mutability::Not,
                )))
            }
            hir::HirTyKind::Array(inner, _) => {
                let elem_ty = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::Array(
                    Box::new(elem_ty),
                    types::ConstKind::Value(types::ConstValue::ZeroSized),
                )))
            }
            hir::HirTyKind::Ptr(inner) => {
                let pointee = self.hir_ty_to_ty(inner)?;
                Ok(types::Ty::new(types::TyKind::RawPtr(types::TypeAndMut {
                    ty: Box::new(pointee),
                    mutbl: types::Mutability::Not,
                })))
            }
            hir::HirTyKind::Never => Ok(types::Ty::never()),
            hir::HirTyKind::Infer => Ok(types::Ty::new(types::TyKind::Infer(
                types::InferTy::FreshTy(0),
            ))),
        }
    }

    fn primitive_ty_to_ty(&mut self, prim: &TypePrimitive) -> types::Ty {
        match prim {
            TypePrimitive::Bool => types::Ty::bool(),
            TypePrimitive::Char => types::Ty::char(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => types::Ty::int(types::IntTy::I8),
                TypeInt::I16 => types::Ty::int(types::IntTy::I16),
                TypeInt::I32 => types::Ty::int(types::IntTy::I32),
                TypeInt::I64 => types::Ty::int(types::IntTy::I64),
                TypeInt::U8 => types::Ty::uint(types::UintTy::U8),
                TypeInt::U16 => types::Ty::uint(types::UintTy::U16),
                TypeInt::U32 => types::Ty::uint(types::UintTy::U32),
                TypeInt::U64 => types::Ty::uint(types::UintTy::U64),
                TypeInt::BigInt => types::Ty::int(types::IntTy::I128),
            },
            TypePrimitive::Decimal(dec_ty) => match dec_ty {
                DecimalType::F32 => types::Ty::float(types::FloatTy::F32),
                DecimalType::F64 => types::Ty::float(types::FloatTy::F64),
                DecimalType::BigDecimal | DecimalType::Decimal { .. } => {
                    types::Ty::float(types::FloatTy::F64)
                }
            },
            TypePrimitive::String => {
                let stub = self.type_context.ensure_struct_stub("String");
                self.type_context
                    .make_struct_ty_by_id(stub, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type())
            }
            TypePrimitive::List => {
                let stub = self.type_context.ensure_struct_stub("List");
                self.type_context
                    .make_struct_ty_by_id(stub, Vec::new())
                    .unwrap_or_else(|| self.create_unit_type())
            }
        }
    }

    /// Type checking for binary operations
    fn check_binary_op_types(
        &self,
        left_ty: &types::Ty,
        right_ty: &types::Ty,
        _op: &thir::BinOp,
    ) -> Result<types::Ty> {
        // Simplified type checking - assume same types for operands
        if left_ty == right_ty {
            Ok(left_ty.clone())
        } else {
            // Try to find a common type
            Ok(left_ty.clone())
        }
    }

    /// Infer type from HIR path
    fn infer_path_type(&mut self, path: &hir::HirPath) -> Result<types::Ty> {
        let (def_id_opt, qualified_name, base_name, substs) = self.path_to_type_info(path)?;

        if let Some(def_id) = def_id_opt {
            if let Some(const_ty) = self.type_context.lookup_const_type(def_id) {
                return Ok(const_ty.clone());
            }
            if let Some(struct_ty) = self
                .type_context
                .make_struct_ty_by_id(def_id, substs.clone())
            {
                return Ok(struct_ty);
            }
            if self
                .type_context
                .lookup_function_signature(def_id)
                .is_some()
            {
                return Ok(types::Ty::new(types::TyKind::FnDef(def_id, substs)));
            }
        }

        if let Some(primitive) = self.make_primitive_ty(&base_name) {
            return Ok(primitive);
        }

        if let Some(struct_id) = self
            .type_context
            .lookup_struct_def_id(&qualified_name)
            .or_else(|| self.type_context.lookup_struct_def_id(&base_name))
        {
            if let Some(struct_ty) = self
                .type_context
                .make_struct_ty_by_id(struct_id, substs.clone())
            {
                return Ok(struct_ty);
            }
        }

        Ok(types::Ty::int(types::IntTy::I32))
    }

    /// Infer return type of function call
    fn infer_call_return_type(
        &self,
        func: &thir::ThirExpr,
        _args: &[thir::ThirExpr],
    ) -> Result<types::Ty> {
        if let thir::ThirExprKind::Path(item_ref) = &func.kind {
            if let Some(def_id) = item_ref.def_id {
                if let Some(sig) = self.type_context.lookup_function_signature(def_id) {
                    return Ok((*sig.output).clone());
                }
            } else if let Some(def_id) = self.type_context.lookup_value_def_id(&item_ref.name) {
                if let Some(sig) = self.type_context.lookup_function_signature(def_id) {
                    return Ok((*sig.output).clone());
                }
            }
        }
        Ok(self.create_unit_type())
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
    fn get_function_type(
        &self,
        def_id: types::DefId,
        _func: &thir::ThirFunction,
    ) -> Result<types::Ty> {
        Ok(types::Ty::new(types::TyKind::FnDef(def_id, Vec::new())))
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
    fn infer_method_call_return_type(
        &self,
        receiver: &thir::ThirExpr,
        method_name: &str,
        _args: &[thir::ThirExpr],
    ) -> Result<types::Ty> {
        if let Some(sig) = self
            .type_context
            .lookup_method_signature(&receiver.ty, method_name)
        {
            return Ok((*sig.output).clone());
        }
        Ok(self.create_unit_type())
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

impl IrTransform<hir::HirProgram, thir::ThirProgram> for ThirGenerator {
    fn transform(&mut self, source: hir::HirProgram) -> Result<thir::ThirProgram> {
        ThirGenerator::transform(self, source)
    }
}

impl TypeContext {
    fn new() -> Self {
        Self {
            function_signatures: HashMap::new(),
            method_signatures: HashMap::new(),
            structs: HashMap::new(),
            struct_names: HashMap::new(),
            value_names: HashMap::new(),
            const_types: HashMap::new(),
            next_def_id: 1,
        }
    }

    fn register_function(&mut self, def_id: types::DefId, name: &str, sig: types::FnSig) {
        self.function_signatures.insert(def_id, sig);
        self.insert_value_name(name, def_id);
    }

    fn register_const(&mut self, def_id: types::DefId, name: &str, ty: types::Ty) {
        self.const_types.insert(def_id, ty);
        self.insert_value_name(name, def_id);
    }

    fn declare_struct(&mut self, def_id: types::DefId, name: &str) {
        self.insert_struct_name(name, def_id);
        if !self.structs.contains_key(&def_id) {
            let adt_def = self.build_adt_def(def_id, name, Vec::new());
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def,
                    field_map: HashMap::new(),
                },
            );
        }
        self.method_signatures.entry(def_id).or_default();
    }

    fn init_struct_fields(&mut self, def_id: types::DefId, fields: Vec<(String, types::Ty)>) {
        if !self.structs.contains_key(&def_id) {
            let struct_name = self
                .struct_names
                .iter()
                .find(|(_, &id)| id == def_id)
                .map(|(name, _)| name.clone())
                .unwrap_or_else(|| format!("<anon:{}>", def_id));
            let adt_def = self.build_adt_def(def_id, &struct_name, Vec::new());
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def,
                    field_map: HashMap::new(),
                },
            );
        }

        let struct_name = self
            .structs
            .get(&def_id)
            .and_then(|info| info.adt_def.variants.first())
            .map(|variant| variant.ident.clone())
            .or_else(|| {
                self.struct_names
                    .iter()
                    .find(|(_, &id)| id == def_id)
                    .map(|(name, _)| name.clone())
            })
            .unwrap_or_else(|| format!("<anon:{}>", def_id));

        let mut field_defs = Vec::new();
        let mut field_map = HashMap::new();
        for (idx, (field_name, field_ty)) in fields.into_iter().enumerate() {
            let field_def = types::FieldDef {
                did: self.allocate_def_id(),
                ident: field_name.clone(),
                vis: types::Visibility::Public,
            };
            field_defs.push(field_def);
            field_map.insert(field_name, (idx, field_ty));
        }

        let new_adt = self.build_adt_def(def_id, &struct_name, field_defs);

        if let Some(info) = self.structs.get_mut(&def_id) {
            info.field_map = field_map;
            info.adt_def = new_adt;
        }
    }

    fn insert_value_name(&mut self, name: &str, def_id: types::DefId) {
        self.value_names.insert(name.to_string(), def_id);
        if let Some(base) = name.rsplit("::").next() {
            self.value_names.entry(base.to_string()).or_insert(def_id);
        }
    }

    fn insert_struct_name(&mut self, name: &str, def_id: types::DefId) {
        self.struct_names.insert(name.to_string(), def_id);
        if let Some(base) = name.rsplit("::").next() {
            self.struct_names.entry(base.to_string()).or_insert(def_id);
        }
    }

    fn register_method(&mut self, owner_def_id: types::DefId, method: &str, sig: types::FnSig) {
        self.method_signatures
            .entry(owner_def_id)
            .or_default()
            .insert(method.to_string(), sig);
    }

    fn make_struct_ty_by_id(
        &self,
        def_id: types::DefId,
        substs: types::SubstsRef,
    ) -> Option<types::Ty> {
        self.structs
            .get(&def_id)
            .map(|info| types::Ty::new(types::TyKind::Adt(info.adt_def.clone(), substs)))
    }

    fn lookup_struct_def_id(&self, name: &str) -> Option<types::DefId> {
        self.struct_names.get(name).copied()
    }

    fn lookup_value_def_id(&self, name: &str) -> Option<types::DefId> {
        self.value_names.get(name).copied()
    }

    fn lookup_function_signature(&self, def_id: types::DefId) -> Option<&types::FnSig> {
        self.function_signatures.get(&def_id)
    }

    fn lookup_const_type(&self, def_id: types::DefId) -> Option<&types::Ty> {
        self.const_types.get(&def_id)
    }

    fn lookup_field_info(&self, ty: &types::Ty, field_name: &str) -> Option<(usize, types::Ty)> {
        match &ty.kind {
            types::TyKind::Adt(adt_def, _) => self
                .structs
                .get(&adt_def.did)
                .and_then(|info| info.field_map.get(field_name).cloned()),
            types::TyKind::Ref(_, inner, _) => self.lookup_field_info(inner, field_name),
            _ => None,
        }
    }

    fn lookup_method_signature(
        &self,
        owner_ty: &types::Ty,
        method_name: &str,
    ) -> Option<&types::FnSig> {
        match &owner_ty.kind {
            types::TyKind::Adt(adt_def, _) => self
                .method_signatures
                .get(&adt_def.did)
                .and_then(|methods| methods.get(method_name)),
            types::TyKind::Ref(_, inner, _) => self.lookup_method_signature(inner, method_name),
            _ => None,
        }
    }

    fn ensure_struct_stub(&mut self, name: &str) -> types::DefId {
        if let Some(def_id) = self.struct_names.get(name) {
            *def_id
        } else {
            let def_id = self.allocate_def_id();
            self.insert_struct_name(name, def_id);
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def: self.build_adt_def(def_id, name, Vec::new()),
                    field_map: HashMap::new(),
                },
            );
            self.method_signatures.entry(def_id).or_default();
            def_id
        }
    }

    fn build_adt_def(
        &self,
        def_id: types::DefId,
        name: &str,
        field_defs: Vec<types::FieldDef>,
    ) -> types::AdtDef {
        let variant = types::VariantDef {
            def_id,
            ctor_def_id: None,
            ident: name.to_string(),
            discr: types::VariantDiscr::Relative(0),
            fields: field_defs,
            ctor_kind: types::CtorKind::Const,
            is_recovered: false,
        };

        types::AdtDef {
            did: def_id,
            variants: vec![variant],
            flags: types::AdtFlags::IS_STRUCT,
            repr: types::ReprOptions {
                int: None,
                align: None,
                pack: None,
                flags: types::ReprFlags::empty(),
                field_shuffle_seed: 0,
            },
        }
    }

    fn allocate_def_id(&mut self) -> types::DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;
        id
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
    use crate::transformations::ast_to_hir::HirGenerator;
    use fp_core::ast::{register_threadlocal_serializer, AstFile};
    use fp_core::Result;
    use fp_rust::{printer::RustPrinter, shll_parse_items};
    use std::sync::Arc;

    #[test]
    fn test_thir_generator_creation() {
        let generator = ThirGenerator::new();
        assert_eq!(generator.next_thir_id, 0);
    }

    #[test]
    fn test_literal_transformation() -> Result<()> {
        let mut generator = ThirGenerator::new();
        let (thir_lit, ty) = generator
            .transform_literal(hir::HirLit::Integer(42))
            .unwrap();

        match thir_lit {
            thir::ThirLit::Int(value, thir::IntTy::I32) => {
                assert_eq!(value, 42);
            }
            _ => {
                return Err(crate::error::optimization_error(
                    "Expected i32 literal".to_string(),
                ))
            }
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

    #[test]
    fn lowers_impl_items() -> Result<()> {
        let printer = Arc::new(RustPrinter::new());
        register_threadlocal_serializer(printer.clone());

        let items = shll_parse_items! {
            struct Point {
                x: i64,
            }

            impl Point {
                fn get_x(&self) -> i64 {
                    self.x
                }
            }
        };

        let ast_file = AstFile {
            path: "thir_test.fp".into(),
            items,
        };

        let mut hir_gen = HirGenerator::new();
        let hir_program = hir_gen.transform_file(&ast_file)?;

        let mut thir_gen = ThirGenerator::new();
        let thir_program = thir_gen.transform(hir_program)?;

        assert!(thir_program
            .items
            .iter()
            .any(|item| matches!(item.kind, thir::ThirItemKind::Struct(_))));
        assert!(thir_program
            .items
            .iter()
            .any(|item| matches!(item.kind, thir::ThirItemKind::Impl(_))));

        let point_id = thir_gen
            .type_context
            .lookup_struct_def_id("Point")
            .expect("point id");
        let point_ty = thir_gen
            .type_context
            .make_struct_ty_by_id(point_id, Vec::new())
            .expect("point type registered");
        let (idx, field_ty) = thir_gen
            .type_context
            .lookup_field_info(&point_ty, "x")
            .expect("field info");
        assert_eq!(idx, 0);
        assert!(matches!(
            field_ty.kind,
            types::TyKind::Int(types::IntTy::I64)
        ));

        let method_sig = thir_gen
            .type_context
            .lookup_method_signature(&point_ty, "get_x")
            .expect("method signature");
        assert!(matches!(
            method_sig.output.kind,
            types::TyKind::Int(types::IntTy::I64)
        ));

        Ok(())
    }
}
