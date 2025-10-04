use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::mir;
use fp_core::mir::ty::{
    ConstKind, ConstValue, ErrorGuaranteed, FloatTy, IntTy, Mutability, Scalar, ScalarInt, Ty,
    TyKind, TypeAndMut, UintTy,
};
use fp_core::span::Span;
use std::collections::HashMap;

use super::IrTransform;

const DIAGNOSTIC_CONTEXT: &str = "hir→mir";

/// Minimal HIR → MIR lowering pass.
///
/// This currently produces skeletal MIR that is sufficient to feed the
/// downstream MIR→LIR/LLVM pipeline. Unsupported constructs surface diagnostics
/// so callers can decide whether to abort or continue.
#[derive(Clone, Debug)]
struct MethodLoweringInfo {
    sig: mir::FunctionSig,
    fn_name: String,
    fn_ty: Ty,
    struct_def: Option<hir::DefId>,
}

#[derive(Clone, Debug)]
struct MethodContext {
    def_id: Option<hir::DefId>,
    path: Vec<hir::PathSegment>,
    mir_self_ty: Ty,
}

struct RegisteredStruct {
    name: String,
    fields: Vec<StructFieldInfo>,
    field_index: HashMap<String, usize>,
    ty: Ty,
}

impl RegisteredStruct {
    fn field(&self, name: &str) -> Option<(usize, &StructFieldInfo)> {
        self.field_index
            .get(name)
            .copied()
            .map(|idx| (idx, &self.fields[idx]))
    }
}

#[derive(Clone)]
struct StructFieldInfo {
    name: String,
    ty: Ty,
    struct_def: Option<hir::DefId>,
}

struct ConstInfo {
    ty: Ty,
    value: mir::Constant,
}

pub struct MirLowering {
    next_mir_id: mir::MirId,
    next_body_id: u32,
    next_error_id: u32,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    struct_defs: HashMap<hir::DefId, RegisteredStruct>,
    enum_def_types: HashMap<hir::DefId, Ty>,
    const_values: HashMap<hir::DefId, ConstInfo>,
    function_sigs: HashMap<hir::DefId, mir::FunctionSig>,
    runtime_functions: HashMap<String, mir::FunctionSig>,
    struct_methods: HashMap<String, HashMap<String, MethodLoweringInfo>>,
    method_lookup: HashMap<String, MethodLoweringInfo>,
    extra_items: Vec<mir::Item>,
    extra_bodies: Vec<(mir::BodyId, mir::Body)>,
}

impl MirLowering {
    fn default_runtime_signatures() -> HashMap<String, mir::FunctionSig> {
        let mut map = HashMap::new();
        map.insert(
            "printf".to_string(),
            mir::FunctionSig {
                inputs: Vec::new(),
                output: Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
            },
        );
        map
    }

    pub fn new() -> Self {
        Self {
            next_mir_id: 0,
            next_body_id: 0,
            next_error_id: 0,
            diagnostics: Vec::new(),
            has_errors: false,
            struct_defs: HashMap::new(),
            enum_def_types: HashMap::new(),
            const_values: HashMap::new(),
            function_sigs: HashMap::new(),
            runtime_functions: Self::default_runtime_signatures(),
            struct_methods: HashMap::new(),
            method_lookup: HashMap::new(),
            extra_items: Vec::new(),
            extra_bodies: Vec::new(),
        }
    }

    fn lower_program(&mut self, program: &hir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id, def);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id, def, item.span);
                }
                hir::ItemKind::Const(const_item) => {
                    let mir_item = self.lower_const(item.def_id, const_item)?;
                    mir_program.items.push(mir_item);
                }
                hir::ItemKind::Function(function) => {
                    let (mir_item, body_id, body) = self.lower_function(program, item, function)?;
                    mir_program.items.push(mir_item);
                    mir_program.bodies.insert(body_id, body);
                }
                hir::ItemKind::Impl(impl_block) => {
                    self.lower_impl(program, item, impl_block, Some(&mut mir_program))?;
                }
            }
        }

        self.flush_extra_items(&mut mir_program);

        Ok(mir_program)
    }

    fn flush_extra_items(&mut self, program: &mut mir::Program) {
        for item in self.extra_items.drain(..) {
            program.items.push(item);
        }
        for (body_id, body) in self.extra_bodies.drain(..) {
            program.bodies.insert(body_id, body);
        }
    }

    fn lower_function(
        &mut self,
        program: &hir::Program,
        item: &hir::Item,
        function: &hir::Function,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let sig = self.lower_function_sig(&function.sig, None);
        self.function_sigs.insert(item.def_id, sig.clone());
        let mir_body = self.lower_body(program, item, function, &sig, None)?;

        let mir_function = mir::Function {
            name: function.sig.name.clone(),
            path: Vec::new(),
            def_id: Some(item.def_id),
            sig,
            body_id,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body))
    }

    fn lower_function_sig(
        &mut self,
        sig: &hir::FunctionSig,
        method_context: Option<&MethodContext>,
    ) -> mir::FunctionSig {
        mir::FunctionSig {
            inputs: sig
                .inputs
                .iter()
                .map(|param| self.lower_type_expr_with_context(&param.ty, method_context))
                .collect(),
            output: self.lower_type_expr_with_context(&sig.output, method_context),
        }
    }

    fn lower_type_expr_with_context(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    return ctx.mir_self_ty.clone();
                }
            }
        }

        match &ty_expr.kind {
            hir::TypeExprKind::Ref(inner) => {
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                let inner_ty = self.lower_type_expr_with_context(inner, method_context);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Mut,
                    ),
                }
            }
            _ => self.lower_type_expr(ty_expr),
        }
    }

    fn lower_body(
        &mut self,
        program: &hir::Program,
        item: &hir::Item,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        method_context: Option<MethodContext>,
    ) -> Result<mir::Body> {
        let span = function
            .body
            .as_ref()
            .map(|body| body.value.span)
            .unwrap_or(item.span);

        BodyBuilder::new(self, program, function, sig, span, method_context).lower()
    }

    fn lower_const(&mut self, def_id: hir::DefId, konst: &hir::Const) -> Result<mir::Item> {
        let ty = self.lower_type_expr(&konst.ty);
        let init_constant = self
            .lower_literal_expr(&konst.body.value)
            .unwrap_or_else(|| self.error_constant(konst.body.value.span));
        let init = mir::Operand::Constant(init_constant.clone());

        self.const_values.insert(
            def_id,
            ConstInfo {
                ty: ty.clone(),
                value: init_constant,
            },
        );

        let mir_static = mir::Static {
            ty,
            init,
            mutability: mir::Mutability::Not,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Static(mir_static),
        };
        self.next_mir_id += 1;

        Ok(mir_item)
    }

    fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        match &ty_expr.kind {
            hir::TypeExprKind::Primitive(primitive) => {
                self.lower_primitive_type(primitive, ty_expr.span)
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
            hir::TypeExprKind::Ref(inner) => {
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
            hir::TypeExprKind::Never => Ty {
                kind: TyKind::Never,
            },
            _ => {
                self.emit_error(ty_expr.span, "type lowering not yet supported");
                self.error_ty()
            }
        }
    }

    fn eval_type_length(&self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    self.const_values
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

    fn lower_primitive_type(&mut self, primitive: &TypePrimitive, span: Span) -> Ty {
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
                    self.emit_error(
                        span,
                        "arbitrary precision decimals are not supported in MIR",
                    );
                    self.error_ty()
                }
            },
            TypePrimitive::String | TypePrimitive::List => {
                self.emit_error(span, "string/list primitives are not yet supported in MIR");
                self.error_ty()
            }
        }
    }

    fn lower_path_type(&mut self, path: &hir::Path, span: Span) -> Ty {
        if let Some(res) = &path.res {
            if let hir::Res::Def(def_id) = res {
                if let Some(info) = self.struct_defs.get(def_id) {
                    return info.ty.clone();
                }
                if let Some(enum_ty) = self.enum_def_types.get(def_id) {
                    return enum_ty.clone();
                }
                if let Some(sig) = self.function_sigs.get(def_id) {
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
                "usize" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    }
                }
                "isize" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::Isize),
                    }
                }
                "bool" => return Ty { kind: TyKind::Bool },
                "char" => return Ty { kind: TyKind::Char },
                "str" => {
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    }
                }
                _ => {}
            }
        }

        let display = path
            .segments
            .iter()
            .map(|seg| seg.name.clone())
            .collect::<Vec<_>>()
            .join("::");
        self.emit_error(
            span,
            format!("type lowering not yet supported for path `{}`", display),
        );
        self.error_ty()
    }

    fn register_struct(&mut self, def_id: hir::DefId, strukt: &hir::Struct) {
        if self.struct_defs.contains_key(&def_id) {
            return;
        }

        let mut fields = Vec::new();
        let mut field_index = HashMap::new();

        #[allow(clippy::needless_collect)]
        let _struct_name = strukt.name.clone();

        for (idx, field) in strukt.fields.iter().enumerate() {
            let field_ty = self.lower_type_expr(&field.ty);
            let struct_def = match &field.ty.kind {
                hir::TypeExprKind::Path(path) => match &path.res {
                    Some(hir::Res::Def(inner_id)) => Some(*inner_id),
                    _ => None,
                },
                _ => None,
            };

            fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty: field_ty,
                struct_def,
            });
            field_index.insert(field.name.clone(), idx);
        }

        let struct_ty = Ty {
            kind: TyKind::Tuple(
                fields
                    .iter()
                    .map(|field| Box::new(field.ty.clone()))
                    .collect(),
            ),
        };

        self.struct_defs.insert(
            def_id,
            RegisteredStruct {
                name: strukt.name.clone(),
                fields,
                field_index,
                ty: struct_ty,
            },
        );
    }

    fn register_enum(&mut self, def_id: hir::DefId, enm: &hir::Enum, span: Span) {
        if self.enum_def_types.contains_key(&def_id) {
            return;
        }

        let enum_ty = Ty {
            kind: TyKind::Int(IntTy::Isize),
        };
        self.enum_def_types.insert(def_id, enum_ty.clone());

        let mut next_value: i64 = 0;
        for variant in &enm.variants {
            let (value, value_span) = if let Some(expr) = &variant.discriminant {
                match self.eval_int_expr(expr) {
                    Some(val) => {
                        next_value = val.saturating_add(1);
                        (val, expr.span)
                    }
                    None => {
                        self.emit_error(
                            expr.span,
                            format!(
                                "unable to evaluate discriminant for enum variant `{}`",
                                variant.name
                            ),
                        );
                        let val = next_value;
                        next_value = next_value.saturating_add(1);
                        (val, expr.span)
                    }
                }
            } else {
                let val = next_value;
                next_value = next_value.saturating_add(1);
                (val, span)
            };

            let constant = mir::Constant {
                span: value_span,
                user_ty: None,
                literal: mir::ConstantKind::Int(value),
            };

            self.const_values.insert(
                variant.def_id,
                ConstInfo {
                    ty: enum_ty.clone(),
                    value: constant,
                },
            );
        }
    }

    fn eval_int_expr(&mut self, expr: &hir::Expr) -> Option<i64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value),
            hir::ExprKind::Unary(hir::UnOp::Neg, inner) => self.eval_int_expr(inner).map(|v| -v),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let left = self.eval_int_expr(lhs)?;
                let right = self.eval_int_expr(rhs)?;
                match op {
                    hir::BinOp::Add => Some(left.saturating_add(right)),
                    hir::BinOp::Sub => Some(left.saturating_sub(right)),
                    hir::BinOp::Mul => Some(left.saturating_mul(right)),
                    hir::BinOp::Div => Some(if right != 0 {
                        left / right
                    } else {
                        return None;
                    }),
                    hir::BinOp::Rem => Some(if right != 0 {
                        left % right
                    } else {
                        return None;
                    }),
                    _ => None,
                }
            }
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(info) = self.const_values.get(def_id) {
                        match &info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value),
                            mir::ConstantKind::UInt(value) => Some(*value as i64),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            hir::ExprKind::Cast(inner, _) => self.eval_int_expr(inner),
            _ => None,
        }
    }

    fn register_const_value(&mut self, def_id: hir::DefId, konst: &hir::Const) {
        if self.const_values.contains_key(&def_id) {
            return;
        }

        let ty = self.lower_type_expr(&konst.ty);
        if let Some(constant) = self.lower_literal_expr(&konst.body.value) {
            self.const_values.insert(
                def_id,
                ConstInfo {
                    ty,
                    value: constant,
                },
            );
        }
    }

    fn struct_name_from_type(&self, ty: &hir::TypeExpr) -> Option<String> {
        match &ty.kind {
            hir::TypeExprKind::Path(path) => path.segments.last().map(|seg| seg.name.clone()),
            hir::TypeExprKind::Ref(inner) | hir::TypeExprKind::Ptr(inner) => {
                self.struct_name_from_type(inner)
            }
            _ => None,
        }
    }

    fn lower_impl(
        &mut self,
        program: &hir::Program,
        item: &hir::Item,
        impl_block: &hir::Impl,
        output: Option<&mut mir::Program>,
    ) -> Result<()> {
        let mut output = output;
        let mut emit_function =
            |this: &mut Self, mir_item: mir::Item, body_id: mir::BodyId, body: mir::Body| {
                if let Some(program_ref) = output.as_mut() {
                    let program: &mut mir::Program = &mut **program_ref;
                    program.items.push(mir_item);
                    program.bodies.insert(body_id, body);
                } else {
                    this.extra_items.push(mir_item);
                    this.extra_bodies.push((body_id, body));
                }
            };

        let struct_name = match self.struct_name_from_type(&impl_block.self_ty) {
            Some(name) => name,
            None => {
                self.emit_error(
                    item.span,
                    "impl self type could not be resolved to a struct",
                );
                return Ok(());
            }
        };

        let method_context = self.make_method_context(&impl_block.self_ty);

        for impl_item in &impl_block.items {
            match &impl_item.kind {
                hir::ImplItemKind::Method(function) => {
                    let (mir_item, body_id, body, sig) =
                        self.lower_method(program, function, item.span, method_context.as_ref())?;
                    emit_function(self, mir_item, body_id, body);

                    let fn_name = function.sig.name.clone();
                    let fn_ty = self.function_pointer_ty(&sig);
                    let struct_def = method_context.as_ref().and_then(|ctx| ctx.def_id);
                    let info = MethodLoweringInfo {
                        sig: sig.clone(),
                        fn_name: fn_name.clone(),
                        fn_ty: fn_ty.clone(),
                        struct_def,
                    };

                    self.method_lookup.insert(fn_name, info.clone());
                    self.method_lookup
                        .insert(format!("{}::{}", struct_name, impl_item.name), info.clone());
                    self.struct_methods
                        .entry(struct_name.clone())
                        .or_default()
                        .insert(impl_item.name.clone(), info);
                }
                hir::ImplItemKind::AssocConst(_const_item) => {
                    // TODO: lower associated constants when needed
                }
            }
        }

        Ok(())
    }

    fn lower_method(
        &mut self,
        program: &hir::Program,
        function: &hir::Function,
        parent_span: Span,
        method_context: Option<&MethodContext>,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body, mir::FunctionSig)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let sig = self.lower_function_sig(&function.sig, method_context);
        let span = function
            .body
            .as_ref()
            .map(|body| body.value.span)
            .unwrap_or(parent_span);
        let mir_body =
            BodyBuilder::new(self, program, function, &sig, span, method_context.cloned())
                .lower()?;

        let mir_function = mir::Function {
            name: function.sig.name.clone(),
            path: Vec::new(),
            def_id: None,
            sig: sig.clone(),
            body_id,
        };

        let mir_item = mir::Item {
            mir_id: self.next_mir_id,
            kind: mir::ItemKind::Function(mir_function),
        };
        self.next_mir_id += 1;

        Ok((mir_item, body_id, mir_body, sig))
    }

    fn make_method_context(&mut self, self_ty: &hir::TypeExpr) -> Option<MethodContext> {
        if let hir::TypeExprKind::Path(path) = &self_ty.kind {
            let def_id = match &path.res {
                Some(hir::Res::Def(def_id)) => Some(*def_id),
                _ => None,
            };
            let mir_self_ty = self.lower_type_expr(self_ty);
            Some(MethodContext {
                def_id,
                path: path.segments.clone(),
                mir_self_ty,
            })
        } else {
            None
        }
    }

    fn struct_field(&self, def_id: hir::DefId, name: &str) -> Option<(usize, &StructFieldInfo)> {
        self.struct_defs
            .get(&def_id)
            .and_then(|info| info.field(name))
    }

    fn function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
        Ty {
            kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                binder: mir::ty::Binder {
                    value: mir::ty::FnSig {
                        inputs: sig.inputs.iter().map(|ty| Box::new(ty.clone())).collect(),
                        output: Box::new(sig.output.clone()),
                        c_variadic: false,
                        unsafety: mir::ty::Unsafety::Normal,
                        abi: mir::ty::Abi::Rust,
                    },
                    bound_vars: Vec::new(),
                },
            }),
        }
    }

    fn c_function_pointer_ty(&self, sig: &mir::FunctionSig) -> Ty {
        Ty {
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
        }
    }

    fn make_local_decl(&mut self, ty: &Ty, span: Span) -> mir::LocalDecl {
        mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty: ty.clone(),
            user_ty: None,
            source_info: span,
        }
    }

    fn lower_literal_expr(&mut self, expr: &hir::Expr) -> Option<mir::Constant> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(mir::Constant {
                span: expr.span,
                user_ty: None,
                literal: self.lower_literal(lit),
            }),
            _ => None,
        }
    }

    fn lower_literal(&self, lit: &hir::Lit) -> mir::ConstantKind {
        match lit {
            hir::Lit::Bool(value) => mir::ConstantKind::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstantKind::Int(*value),
            hir::Lit::Float(value) => mir::ConstantKind::Float(*value),
            hir::Lit::Str(value) => mir::ConstantKind::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstantKind::Int(*value as i64),
        }
    }

    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        self.has_errors = true;
        let diagnostic = Diagnostic::error(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn error_ty(&mut self) -> Ty {
        let error = ErrorGuaranteed {
            index: self.next_error_id,
        };
        self.next_error_id += 1;
        Ty {
            kind: TyKind::Error(error),
        }
    }

    fn error_constant(&mut self, span: Span) -> mir::Constant {
        self.emit_error(span, "unable to lower expression to a constant");
        mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Bool(false),
        }
    }

    pub fn take_diagnostics(&mut self) -> (Vec<Diagnostic>, bool) {
        let diagnostics = std::mem::take(&mut self.diagnostics);
        let has_errors = std::mem::replace(&mut self.has_errors, false);
        (diagnostics, has_errors)
    }
}

impl Default for MirLowering {
    fn default() -> Self {
        Self::new()
    }
}

struct BodyBuilder<'a> {
    lowering: &'a mut MirLowering,
    program: &'a hir::Program,
    function: &'a hir::Function,
    sig: &'a mir::FunctionSig,
    locals: Vec<mir::LocalDecl>,
    local_map: HashMap<hir::HirId, mir::LocalId>,
    local_structs: HashMap<mir::LocalId, hir::DefId>,
    const_items: HashMap<hir::DefId, hir::Const>,
    blocks: Vec<mir::BasicBlockData>,
    current_block: mir::BasicBlockId,
    span: Span,
    method_context: Option<MethodContext>,
}

struct PlaceInfo {
    place: mir::Place,
    ty: Ty,
    struct_def: Option<hir::DefId>,
}

struct OperandInfo {
    operand: mir::Operand,
    ty: Ty,
}

impl<'a> BodyBuilder<'a> {
    fn new(
        lowering: &'a mut MirLowering,
        program: &'a hir::Program,
        function: &'a hir::Function,
        sig: &'a mir::FunctionSig,
        span: Span,
        method_context: Option<MethodContext>,
    ) -> Self {
        let mut locals = Vec::new();
        locals.push(lowering.make_local_decl(&sig.output, span));

        let mut builder = Self {
            lowering,
            program,
            function,
            sig,
            locals,
            local_map: HashMap::new(),
            local_structs: HashMap::new(),
            const_items: HashMap::new(),
            blocks: vec![mir::BasicBlockData::new(None)],
            current_block: 0,
            span,
            method_context,
        };

        let body_params = builder
            .function
            .body
            .as_ref()
            .map(|body| body.params.as_slice())
            .unwrap_or(&[]);

        for (idx, ty) in builder.sig.inputs.iter().enumerate() {
            let mut decl = builder.lowering.make_local_decl(ty, builder.span);
            decl.mutability = mir::Mutability::Not;
            let local_id = builder.push_local(decl);

            if let Some(param) = body_params.get(idx) {
                builder.bind_pattern(&param.pat, local_id, Some(ty));
            }
        }

        builder
    }

    fn push_local(&mut self, decl: mir::LocalDecl) -> mir::LocalId {
        let local_id = self.locals.len() as mir::LocalId;
        self.locals.push(decl);
        local_id
    }

    fn bind_pattern(&mut self, pat: &hir::Pat, local: mir::LocalId, ty: Option<&Ty>) {
        match &pat.kind {
            hir::PatKind::Binding { name, mutable } => {
                self.local_map.insert(pat.hir_id, local);
                if let Some(decl) = self.locals.get_mut(local as usize) {
                    if *mutable {
                        decl.mutability = mir::Mutability::Mut;
                    }
                    let mut struct_def = ty.and_then(|ty| self.struct_def_from_ty(ty));
                    if let Some(ctx) = &self.method_context {
                        if let Some(def_id) = ctx.def_id {
                            let name_matches_self = name.as_str() == "self";
                            let ty_matches_self = ty
                                .map(|ty| self.ty_matches(ty, &ctx.mir_self_ty))
                                .unwrap_or(false);
                            if name_matches_self || ty_matches_self {
                                struct_def = Some(def_id);
                            }
                        }
                    }
                    if let Some(def_id) = struct_def {
                        self.local_structs.insert(local, def_id);
                    }
                }
            }
            hir::PatKind::Wild => {
                self.local_map.insert(pat.hir_id, local);
            }
            _ => {
                self.lowering.emit_error(
                    self.span,
                    "complex pattern bindings are not yet supported in MIR lowering",
                );
            }
        }
    }

    fn struct_def_from_ty(&self, ty: &Ty) -> Option<hir::DefId> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.struct_def_from_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.struct_def_from_ty(type_and_mut.ty.as_ref()),
            _ => self
                .lowering
                .struct_defs
                .iter()
                .find_map(|(def_id, info)| (info.ty == *ty).then_some(*def_id)),
        }
    }

    fn ty_matches(&self, lhs: &Ty, rhs: &Ty) -> bool {
        fn strip_refs<'a>(ty: &'a Ty) -> &'a Ty {
            match &ty.kind {
                TyKind::Ref(_, inner, _) => strip_refs(inner.as_ref()),
                TyKind::RawPtr(type_and_mut) => strip_refs(type_and_mut.ty.as_ref()),
                _ => ty,
            }
        }

        strip_refs(lhs) == strip_refs(rhs)
    }

    fn lower(mut self) -> Result<mir::Body> {
        if let Some(body) = &self.function.body {
            match &body.value.kind {
                hir::ExprKind::Block(block) => self.lower_block(block)?,
                _ => {
                    let return_ty = self.locals[0].ty.clone();
                    let place = mir::Place::from_local(0);
                    self.lower_expr_into_place(&body.value, place, &return_ty)?;
                }
            }
        }

        self.ensure_terminated();
        Ok(mir::Body::new(
            self.blocks,
            self.locals,
            self.sig.inputs.len(),
            self.span,
        ))
    }

    fn ensure_terminated(&mut self) {
        if let Some(block) = self.blocks.last_mut() {
            if block.terminator.is_none() {
                block.terminator = Some(mir::Terminator {
                    source_info: self.span,
                    kind: mir::TerminatorKind::Return,
                });
            }
        }
    }

    fn lower_block(&mut self, block: &hir::Block) -> Result<()> {
        for stmt in &block.stmts {
            self.lower_stmt(stmt)?;
        }

        if let Some(expr) = &block.expr {
            self.lower_tail_expr(expr)?;
        }

        Ok(())
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> Result<()> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => self.lower_local(local),
            hir::StmtKind::Item(item) => self.lower_inner_item(item),
            hir::StmtKind::Semi(expr) | hir::StmtKind::Expr(expr) => {
                self.lower_expr_statement(expr)
            }
        }
    }

    fn lower_tail_expr(&mut self, expr: &hir::Expr) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let place = mir::Place::from_local(0);
        self.lower_expr_into_place(expr, place, &return_ty)
    }

    fn lower_local(&mut self, local: &hir::Local) -> Result<()> {
        let init_span = local
            .init
            .as_ref()
            .map(|expr| expr.span)
            .unwrap_or(self.span);

        let declared_ty = local
            .ty
            .as_ref()
            .map(|ty_expr| self.lowering.lower_type_expr(ty_expr));

        let mut decl = self.lowering.make_local_decl(
            declared_ty.as_ref().unwrap_or(&Ty {
                kind: TyKind::Tuple(Vec::new()),
            }),
            init_span,
        );

        if let hir::PatKind::Binding { mutable, .. } = &local.pat.kind {
            if *mutable {
                decl.mutability = mir::Mutability::Mut;
            }
        }

        let local_id = self.push_local(decl);
        self.bind_pattern(&local.pat, local_id, declared_ty.as_ref());

        if let Some(init_expr) = &local.init {
            self.lower_assignment(local_id, declared_ty.as_ref(), init_expr)?;
        }

        Ok(())
    }

    fn lower_inner_item(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Struct(def) => {
                self.lowering.register_struct(item.def_id, def);
            }
            hir::ItemKind::Enum(enm) => {
                self.lowering.register_enum(item.def_id, enm, item.span);
            }
            hir::ItemKind::Const(konst) => {
                self.lowering.register_const_value(item.def_id, konst);
                self.const_items.insert(item.def_id, konst.clone());
            }
            hir::ItemKind::Impl(impl_block) => {
                self.lowering
                    .lower_impl(self.program, item, impl_block, None)?;
            }
            _ => {
                self.lowering.emit_error(
                    item.span,
                    "only const, struct, enum, and impl items are supported inside function bodies",
                );
            }
        }
        Ok(())
    }

    fn lower_expr_statement(&mut self, expr: &hir::Expr) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Assign(place_expr, value_expr) => {
                let place_info = match self.lower_place(place_expr)? {
                    Some(info) => info,
                    None => {
                        self.lowering
                            .emit_error(place_expr.span, "assignment target is not addressable");
                        return Ok(());
                    }
                };

                let expected_ty = place_info.ty.clone();
                self.lower_expr_into_place(value_expr, place_info.place, &expected_ty)?;
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, None)?;
            }
            _ => {
                // Evaluate then drop result
                let _ = self.lower_operand(expr, None)?;
            }
        }
        Ok(())
    }

    fn lower_assignment(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        expr: &hir::Expr,
    ) -> Result<()> {
        if let hir::ExprKind::Struct(path, fields) = &expr.kind {
            self.lower_struct_literal(local_id, annotated_ty, path, fields, expr.span)
        } else if let hir::ExprKind::Call(callee, args) = &expr.kind {
            let place = mir::Place::from_local(local_id);
            let ty = annotated_ty
                .cloned()
                .unwrap_or_else(|| self.locals[local_id as usize].ty.clone());
            if let Some(info) = self.lower_call(expr, callee, args, Some((place, ty.clone())))? {
                self.locals[local_id as usize].ty = info.ty.clone();
                if let Some(def_id) = info.struct_def {
                    self.local_structs.insert(local_id, def_id);
                }
            }
            Ok(())
        } else {
            let expected_ty = annotated_ty
                .cloned()
                .or_else(|| Some(self.locals[local_id as usize].ty.clone()));
            let value = self.lower_operand(expr, expected_ty.as_ref())?;
            let statement = mir::Statement {
                source_info: expr.span,
                kind: mir::StatementKind::Assign(
                    mir::Place::from_local(local_id),
                    mir::Rvalue::Use(value.operand),
                ),
            };
            self.push_statement(statement);
            let struct_def = expected_ty
                .as_ref()
                .and_then(|ty| self.struct_def_from_ty(ty))
                .or_else(|| self.struct_def_from_ty(&value.ty));
            if let Some(def_id) = struct_def {
                self.local_structs.insert(local_id, def_id);
            }
            self.locals[local_id as usize].ty = value.ty.clone();
            Ok(())
        }
    }

    fn resolve_self_path(&self, path: &mut hir::Path) {
        if let Some(context) = &self.method_context {
            if let Some(first) = path.segments.first() {
                if first.name.as_str() == "Self" {
                    let mut new_segments = context.path.clone();
                    new_segments.extend(path.segments.iter().skip(1).cloned());
                    path.segments = new_segments;
                    if let Some(def_id) = context.def_id {
                        path.res = Some(hir::Res::Def(def_id));
                    }
                }
            }
        }
    }

    fn lower_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        path: &hir::Path,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<()> {
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);
        let def_id = match &resolved_path.res {
            Some(hir::Res::Def(def_id)) => *def_id,
            _ => {
                self.lowering
                    .emit_error(span, "struct literal without resolved definition");
                return Ok(());
            }
        };

        let (struct_fields, struct_ty) = match self.lowering.struct_defs.get(&def_id) {
            Some(info) => (info.fields.clone(), info.ty.clone()),
            None => {
                self.lowering
                    .emit_error(span, "struct not registered during HIR→MIR lowering");
                return Ok(());
            }
        };

        let mut operands = Vec::with_capacity(struct_fields.len());
        let mut field_map: HashMap<String, &hir::StructExprField> = HashMap::new();
        for field in fields {
            field_map.insert(field.name.clone(), field);
        }

        for field_info in struct_fields.iter() {
            let expr = match field_map.get(&field_info.name) {
                Some(field) => &field.expr,
                None => {
                    self.lowering.emit_error(
                        span,
                        format!("missing field `{}` in struct literal", field_info.name),
                    );
                    return Ok(());
                }
            };
            let operand = self.lower_operand(expr, Some(&field_info.ty))?;
            operands.push(operand.operand);
        }

        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                mir::Place::from_local(local_id),
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        };
        self.push_statement(assign);
        self.local_structs.insert(local_id, def_id);

        if let Some(ty) = annotated_ty {
            self.locals[local_id as usize].ty = ty.clone();
        } else {
            self.locals[local_id as usize].ty = struct_ty;
        }

        Ok(())
    }

    fn lower_call(
        &mut self,
        expr: &hir::Expr,
        callee: &hir::Expr,
        args: &[hir::Expr],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<Option<PlaceInfo>> {
        let (func_operand, sig, callee_name) = self.resolve_callee(callee)?;
        let associated_struct = callee_name
            .as_ref()
            .and_then(|name| self.lowering.method_lookup.get(name))
            .and_then(|info| info.struct_def);

        let mut lowered_args = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = sig.inputs.get(idx);
            let operand = self.lower_operand(arg, expected_ty)?;
            lowered_args.push(operand.operand);
        }

        let continue_block = self.new_block();

        let (mir_destination, place_info) = match destination {
            Some((place, _ty)) => {
                let result_ty = sig.output.clone();
                let struct_def = associated_struct.or_else(|| self.struct_def_from_ty(&result_ty));
                if (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                }
                if let Some(def_id) = struct_def {
                    self.local_structs.insert(place.local, def_id);
                }
                let info = PlaceInfo {
                    place: place.clone(),
                    ty: result_ty,
                    struct_def,
                };
                (Some((place, continue_block)), Some(info))
            }
            None => {
                let ty = sig.output.clone();
                let temp = self.allocate_temp(ty, expr.span);
                let place = mir::Place::from_local(temp);
                (Some((place, continue_block)), None)
            }
        };

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args: lowered_args,
                destination: mir_destination.clone(),
                cleanup: None,
                from_hir_call: true,
                fn_span: expr.span,
            },
        };

        self.blocks[self.current_block as usize].terminator = Some(terminator);
        self.current_block = continue_block;

        if place_info.is_none() {
            if let Some((place, _)) = mir_destination {
                let result_ty = sig.output.clone();
                if (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                }
                let struct_def = associated_struct.or_else(|| self.struct_def_from_ty(&result_ty));
                if let Some(def_id) = struct_def {
                    self.local_structs.insert(place.local, def_id);
                }
            }
        }

        Ok(place_info)
    }

    fn resolve_callee(
        &mut self,
        callee: &hir::Expr,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        match &callee.kind {
            hir::ExprKind::Path(path) => self.resolve_callee_path(callee, path),
            _ => {
                self.lowering
                    .emit_error(callee.span, "unsupported call target expression");
                Ok((
                    mir::Operand::Constant(self.lowering.error_constant(callee.span)),
                    mir::FunctionSig {
                        inputs: Vec::new(),
                        output: Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        },
                    },
                    None,
                ))
            }
        }
    }

    fn resolve_callee_path(
        &mut self,
        callee: &hir::Expr,
        path: &hir::Path,
    ) -> Result<(mir::Operand, mir::FunctionSig, Option<String>)> {
        let mut resolved_path = path.clone();
        self.resolve_self_path(&mut resolved_path);
        if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
            if let Some(sig) = self.lowering.function_sigs.get(def_id).cloned() {
                let name = self
                    .program
                    .def_map
                    .get(def_id)
                    .and_then(|item| match &item.kind {
                        hir::ItemKind::Function(func) => Some(func.sig.name.clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| format!("fn#{}", def_id));
                let ty = self.lowering.function_pointer_ty(&sig);
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(name.clone(), ty),
                });
                return Ok((operand, sig, Some(name)));
            }
        }

        let name = resolved_path
            .segments
            .iter()
            .map(|seg| seg.name.clone())
            .collect::<Vec<_>>()
            .join("::");

        if resolved_path.segments.len() >= 2 {
            let method_name = resolved_path
                .segments
                .last()
                .expect("segments len checked")
                .name
                .clone();
            let struct_name = resolved_path
                .segments
                .get(resolved_path.segments.len() - 2)
                .expect("segments len checked")
                .name
                .clone();
            if let Some(info) = self
                .lowering
                .struct_methods
                .get(&struct_name)
                .and_then(|methods| methods.get(&method_name))
            {
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(info.fn_name.clone(), info.fn_ty.clone()),
                });
                let qualified_name = format!("{}::{}", struct_name, method_name);
                return Ok((operand, info.sig.clone(), Some(qualified_name)));
            }
        }

        if let Some(sig) = self.lowering.runtime_functions.get(&name).cloned() {
            let ty = self.lowering.c_function_pointer_ty(&sig);
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Global(name.clone(), ty),
            });
            return Ok((operand, sig, Some(name)));
        }

        if let Some(info) = self.lowering.method_lookup.get(&name) {
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Fn(info.fn_name.clone(), info.fn_ty.clone()),
            });
            return Ok((operand, info.sig.clone(), Some(info.fn_name.clone())));
        }

        self.lowering
            .emit_error(callee.span, format!("unresolved call target `{}`", name));
        Ok((
            mir::Operand::Constant(self.lowering.error_constant(callee.span)),
            mir::FunctionSig {
                inputs: Vec::new(),
                output: Ty {
                    kind: TyKind::Tuple(Vec::new()),
                },
            },
            Some(name),
        ))
    }

    fn lower_operand(&mut self, expr: &hir::Expr, expected: Option<&Ty>) -> Result<OperandInfo> {
        if let Some(place) = self.lower_place(expr)? {
            if let Some(expected_ty) = expected {
                if let TyKind::Ref(_, _, mutability) = &expected_ty.kind {
                    let region = ();
                    let borrow_kind = match mutability {
                        Mutability::Mut => mir::BorrowKind::Mut {
                            allow_two_phase_borrow: false,
                        },
                        Mutability::Not => mir::BorrowKind::Shared,
                    };
                    let temp_local = self.allocate_temp(expected_ty.clone(), expr.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    let assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Ref(region, borrow_kind, place.place.clone()),
                        ),
                    };
                    self.push_statement(assign);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(temp_place),
                        ty: expected_ty.clone(),
                    });
                }
            }
            return Ok(OperandInfo {
                operand: mir::Operand::copy(place.place.clone()),
                ty: place.ty,
            });
        }

        match &expr.kind {
            hir::ExprKind::Literal(lit) => {
                let (literal, ty) = self.lower_literal(lit, expected);
                Ok(OperandInfo {
                    operand: mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        user_ty: None,
                        literal,
                    }),
                    ty,
                })
            }
            hir::ExprKind::Path(path) => {
                let mut resolved_path = path.clone();
                self.resolve_self_path(&mut resolved_path);
                if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                    if let Some(const_info) = self.lowering.const_values.get(def_id) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(const_info.value.clone()),
                            ty: const_info.ty.clone(),
                        });
                    }

                    if let Some(const_item) = self.program.def_map.get(def_id) {
                        if let hir::ItemKind::Const(konst) = &const_item.kind {
                            let ty = self.lowering.lower_type_expr(&konst.ty);
                            let local_id = self.allocate_temp(ty.clone(), expr.span);
                            let place = mir::Place::from_local(local_id);
                            self.lower_expr_into_place(&konst.body.value, place.clone(), &ty)?;
                            if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                                self.local_structs.insert(local_id, struct_def);
                            }
                            return Ok(OperandInfo {
                                operand: mir::Operand::copy(place),
                                ty,
                            });
                        }
                    } else if let Some(konst) = self.const_items.get(def_id).cloned() {
                        let ty = self.lowering.lower_type_expr(&konst.ty);
                        let local_id = self.allocate_temp(ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.lower_expr_into_place(&konst.body.value, place.clone(), &ty)?;
                        if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                            self.local_structs.insert(local_id, struct_def);
                        }
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(place),
                            ty,
                        });
                    }
                }

                let name = resolved_path
                    .segments
                    .iter()
                    .map(|seg| seg.name.clone())
                    .collect::<Vec<_>>()
                    .join("::");
                self.lowering
                    .emit_error(expr.span, format!("unsupported path operand `{}`", name));
                Ok(OperandInfo {
                    operand: mir::Operand::Constant(self.lowering.error_constant(expr.span)),
                    ty: self.lowering.error_ty(),
                })
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = self.lowering.lower_type_expr(ty_expr);
                let local_id = self.allocate_temp(target_ty.clone(), expr.span);
                let place_local = mir::Place::from_local(local_id);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place_local.clone(),
                        mir::Rvalue::Cast(mir::CastKind::Misc, operand.operand, target_ty.clone()),
                    ),
                };
                self.push_statement(statement);
                Ok(OperandInfo {
                    operand: mir::Operand::copy(place_local),
                    ty: target_ty,
                })
            }
            hir::ExprKind::IntrinsicCall(call) => {
                if call.kind == IntrinsicCallKind::ConstBlock {
                    if let IntrinsicCallPayload::Args { args } = &call.payload {
                        if let Some(arg) = args.first() {
                            let ty = expected.cloned().unwrap_or_else(|| Ty {
                                kind: TyKind::Tuple(Vec::new()),
                            });
                            let local_id = self.allocate_temp(ty.clone(), expr.span);
                            let place = mir::Place::from_local(local_id);
                            self.lower_expr_into_place(arg, place.clone(), &ty)?;
                            if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                                self.local_structs.insert(local_id, struct_def);
                            }
                            return Ok(OperandInfo {
                                operand: mir::Operand::copy(place),
                                ty,
                            });
                        }
                    }
                    self.lowering
                        .emit_error(expr.span, "const block intrinsic expects an argument");
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(self.lowering.error_constant(expr.span)),
                        ty: self.lowering.error_ty(),
                    });
                }

                self.lowering.emit_error(
                    expr.span,
                    format!(
                        "intrinsic {:?} is not yet supported in MIR operand lowering",
                        call.kind
                    ),
                );
                Ok(OperandInfo {
                    operand: mir::Operand::Constant(self.lowering.error_constant(expr.span)),
                    ty: self.lowering.error_ty(),
                })
            }
            _ => {
                // Fallback: evaluate into temporary local
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::Tuple(Vec::new()),
                });
                let local_id = self.allocate_temp(ty.clone(), expr.span);
                self.lower_expr_into_place(expr, mir::Place::from_local(local_id), &ty)?;
                let actual_ty = self.locals[local_id as usize].ty.clone();
                Ok(OperandInfo {
                    operand: mir::Operand::copy(mir::Place::from_local(local_id)),
                    ty: actual_ty,
                })
            }
        }
    }

    fn allocate_temp(&mut self, ty: Ty, span: Span) -> mir::LocalId {
        let mut decl = self.lowering.make_local_decl(&ty, span);
        decl.mutability = mir::Mutability::Mut;
        self.push_local(decl)
    }

    fn set_current_terminator(&mut self, terminator: mir::Terminator) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.terminator = Some(terminator);
        }
    }

    fn lower_literal(&mut self, lit: &hir::Lit, expected: Option<&Ty>) -> (mir::ConstantKind, Ty) {
        match lit {
            hir::Lit::Bool(value) => (mir::ConstantKind::Bool(*value), Ty { kind: TyKind::Bool }),
            hir::Lit::Integer(value) => {
                if let Some(expected_ty) = expected {
                    match &expected_ty.kind {
                        TyKind::Uint(_) => {
                            (mir::ConstantKind::UInt(*value as u64), expected_ty.clone())
                        }
                        TyKind::Int(_) => (mir::ConstantKind::Int(*value), expected_ty.clone()),
                        _ => (
                            mir::ConstantKind::Int(*value),
                            Ty {
                                kind: TyKind::Int(IntTy::I64),
                            },
                        ),
                    }
                } else {
                    (
                        mir::ConstantKind::Int(*value),
                        Ty {
                            kind: TyKind::Int(IntTy::I64),
                        },
                    )
                }
            }
            hir::Lit::Float(value) => (
                mir::ConstantKind::Float(*value),
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
            ),
            hir::Lit::Str(value) => (
                mir::ConstantKind::Str(value.clone()),
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                },
            ),
            hir::Lit::Char(value) => (
                mir::ConstantKind::Int(*value as i64),
                Ty {
                    kind: TyKind::Int(IntTy::I32),
                },
            ),
        }
    }

    fn lower_place(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                match &path.res {
                    Some(hir::Res::Local(hir_id)) => {
                        if let Some(local_id) = self.local_map.get(hir_id) {
                            let local_id = *local_id;
                            let ty = self.locals[local_id as usize].ty.clone();
                            let mut struct_def = self.local_structs.get(&local_id).copied();
                            if struct_def.is_none() {
                                if let Some(derived) = self.struct_def_from_ty(&ty) {
                                    self.local_structs.insert(local_id, derived);
                                    struct_def = Some(derived);
                                }
                            }
                            return Ok(Some(PlaceInfo {
                                place: mir::Place::from_local(local_id),
                                ty,
                                struct_def,
                            }));
                        }
                    }
                    Some(hir::Res::Def(def_id)) => {
                        if let Some(const_item) = self.program.def_map.get(def_id) {
                            if let hir::ItemKind::Const(konst) = &const_item.kind {
                                let ty = self.lowering.lower_type_expr(&konst.ty);
                                let local_id = self.allocate_temp(ty.clone(), expr.span);
                                let place_local = mir::Place::from_local(local_id);
                                self.lower_expr_into_place(
                                    &konst.body.value,
                                    place_local.clone(),
                                    &ty,
                                )?;
                                if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                                    self.local_structs.insert(local_id, struct_def);
                                }
                                return Ok(Some(PlaceInfo {
                                    place: place_local,
                                    ty: ty.clone(),
                                    struct_def: self.struct_def_from_ty(&ty),
                                }));
                            }
                        } else if let Some(konst) = self.const_items.get(def_id).cloned() {
                            let ty = self.lowering.lower_type_expr(&konst.ty);
                            let local_id = self.allocate_temp(ty.clone(), expr.span);
                            let place_local = mir::Place::from_local(local_id);
                            self.lower_expr_into_place(
                                &konst.body.value,
                                place_local.clone(),
                                &ty,
                            )?;
                            if let Some(struct_def) = self.struct_def_from_ty(&ty) {
                                self.local_structs.insert(local_id, struct_def);
                            }
                            return Ok(Some(PlaceInfo {
                                place: place_local,
                                ty: ty.clone(),
                                struct_def: self.struct_def_from_ty(&ty),
                            }));
                        }
                    }
                    _ => {}
                }
                Ok(None)
            }
            hir::ExprKind::FieldAccess(base, field) => {
                let base_place = match self.lower_place(base)? {
                    Some(info) => info,
                    None => {
                        self.lowering
                            .emit_error(base.span, "unsupported base expression for field access");
                        return Ok(None);
                    }
                };

                let mut place = base_place.place.clone();
                let mut base_ty = base_place.ty.clone();
                let mut struct_def = base_place.struct_def;

                loop {
                    match &base_ty.kind {
                        TyKind::Ref(_, inner, _) => {
                            place.projection.push(mir::PlaceElem::Deref);
                            base_ty = inner.as_ref().clone();
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            place.projection.push(mir::PlaceElem::Deref);
                            base_ty = type_and_mut.ty.as_ref().clone();
                        }
                        _ => break,
                    }
                }

                if struct_def.is_none() {
                    struct_def = self.struct_def_from_ty(&base_ty);
                }

                let struct_def = match struct_def {
                    Some(def_id) => def_id,
                    None => {
                        self.lowering
                            .emit_error(base.span, "field access on non-struct value");
                        return Ok(None);
                    }
                };

                let (field_index, field_info) =
                    match self.lowering.struct_field(struct_def, field.as_str()) {
                        Some(data) => data,
                        None => {
                            self.lowering
                                .emit_error(expr.span, format!("unknown field `{}`", field));
                            return Ok(None);
                        }
                    };

                place
                    .projection
                    .push(mir::PlaceElem::Field(field_index, field_info.ty.clone()));

                Ok(Some(PlaceInfo {
                    place,
                    ty: field_info.ty.clone(),
                    struct_def: field_info.struct_def,
                }))
            }
            _ => Ok(None),
        }
    }

    fn lower_expr_into_place(
        &mut self,
        expr: &hir::Expr,
        place: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        match &expr.kind {
            hir::ExprKind::Literal(_) | hir::ExprKind::Path(_) => {
                let assignment_place = place.clone();
                let value = self.lower_operand(expr, Some(expected_ty))?;
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        assignment_place.clone(),
                        mir::Rvalue::Use(value.operand),
                    ),
                };
                self.push_statement(statement);
                if assignment_place.projection.is_empty() {
                    self.locals[assignment_place.local as usize].ty = value.ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(&value.ty) {
                        self.local_structs
                            .insert(assignment_place.local, struct_def);
                    }
                }
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = self.lowering.lower_type_expr(ty_expr);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Cast(mir::CastKind::Misc, operand.operand, target_ty.clone()),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = target_ty;
                }
            }
            hir::ExprKind::Struct(path, fields) => {
                let local_id = place.local;
                self.lower_struct_literal(local_id, Some(expected_ty), path, fields, expr.span)?;
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let left = self.lower_operand(lhs, None)?;
                let right = self.lower_operand(rhs, None)?;
                let mir_op = Self::convert_bin_op(op);
                let result_ty = Self::binary_result_ty(op, &left.ty);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::BinaryOp(mir_op, left.operand, right.operand),
                    ),
                };
                self.push_statement(statement);
                if place.projection.is_empty() {
                    self.locals[place.local as usize].ty = result_ty;
                }
            }
            hir::ExprKind::Unary(op, operand_expr) => match op {
                hir::UnOp::Neg | hir::UnOp::Not => {
                    let operand = self.lower_operand(operand_expr, None)?;
                    let mir_op = match Self::convert_un_op(op) {
                        Some(op) => op,
                        None => unreachable!("Neg/Not must convert to MIR op"),
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::UnaryOp(mir_op, operand.operand),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = operand.ty.clone();
                    }
                }
                hir::UnOp::Deref => {
                    self.lowering.emit_error(
                        expr.span,
                        "dereference expressions are not yet supported for MIR assignment",
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Use(mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            )),
                        ),
                    };
                    self.push_statement(statement);
                }
            },
            hir::ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    self.lower_stmt(stmt)?;
                }

                if let Some(expr) = &block.expr {
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                } else {
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
            }
            hir::ExprKind::If(cond, then_expr, else_expr) => {
                let bool_ty = Ty { kind: TyKind::Bool };
                let cond_operand = self.lower_operand(cond, Some(&bool_ty))?;

                let then_block = self.new_block();
                let else_block = self.new_block();
                let continue_block = self.new_block();

                let switch = mir::Terminator {
                    source_info: cond.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand.operand,
                        switch_ty: bool_ty,
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![then_block],
                            otherwise: else_block,
                        },
                    },
                };
                self.set_current_terminator(switch);

                // Then branch
                self.current_block = then_block;
                self.lower_expr_into_place(then_expr, place.clone(), expected_ty)?;
                let then_goto = mir::Terminator {
                    source_info: then_expr.span,
                    kind: mir::TerminatorKind::Goto {
                        target: continue_block,
                    },
                };
                self.set_current_terminator(then_goto);

                // Else branch (if present)
                self.current_block = else_block;
                if let Some(else_expr) = else_expr {
                    self.lower_expr_into_place(else_expr, place, expected_ty)?;
                    let else_goto = mir::Terminator {
                        source_info: else_expr.span,
                        kind: mir::TerminatorKind::Goto {
                            target: continue_block,
                        },
                    };
                    self.set_current_terminator(else_goto);
                } else {
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                    let else_goto = mir::Terminator {
                        source_info: expr.span,
                        kind: mir::TerminatorKind::Goto {
                            target: continue_block,
                        },
                    };
                    self.set_current_terminator(else_goto);
                }

                self.current_block = continue_block;
            }
            hir::ExprKind::IntrinsicCall(call) => {
                if call.kind == IntrinsicCallKind::ConstBlock {
                    if let IntrinsicCallPayload::Args { args } = &call.payload {
                        if let Some(arg) = args.first() {
                            self.lower_expr_into_place(arg, place, expected_ty)?;
                            return Ok(());
                        }
                    }
                    self.lowering
                        .emit_error(expr.span, "const block intrinsic expects an argument");
                } else {
                    self.lowering.emit_error(
                        expr.span,
                        format!(
                            "intrinsic {:?} is not yet supported for MIR assignment",
                            call.kind
                        ),
                    );
                }
            }
            hir::ExprKind::MethodCall(receiver, method_name, args) => {
                let method_key = method_name.clone();
                let mut resolved_info: Option<(MethodLoweringInfo, Option<PlaceInfo>)> = None;

                if let Ok(Some(place_info)) = self.lower_place(receiver) {
                    if let Some(def_id) = place_info
                        .struct_def
                        .or_else(|| self.struct_def_from_ty(&place_info.ty))
                    {
                        if let Some(struct_entry) = self.lowering.struct_defs.get(&def_id) {
                            if let Some(info) = self
                                .lowering
                                .struct_methods
                                .get(&struct_entry.name)
                                .and_then(|methods| methods.get(&method_key))
                            {
                                resolved_info = Some((info.clone(), Some(place_info)));
                            }
                        }
                    }
                }

                if resolved_info.is_none() {
                    let mut matches = self
                        .lowering
                        .struct_methods
                        .iter()
                        .filter_map(|(_struct_name, methods)| {
                            methods.get(&method_key).map(|info| (info.clone()))
                        })
                        .collect::<Vec<_>>();
                    if matches.len() == 1 {
                        let info = matches.remove(0);
                        resolved_info = Some((info, None));
                    }
                }

                if let Some((info, _cached_place)) = resolved_info {
                    let receiver_expected = info.sig.inputs.get(0);
                    let receiver_operand = self.lower_operand(receiver, receiver_expected)?;

                    let mut lowered_args = Vec::with_capacity(args.len() + 1);
                    lowered_args.push(receiver_operand.operand);
                    for (idx, arg) in args.iter().enumerate() {
                        let expected = info.sig.inputs.get(idx + 1);
                        let operand = self.lower_operand(arg, expected)?;
                        lowered_args.push(operand.operand);
                    }

                    let func_operand = mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        user_ty: None,
                        literal: mir::ConstantKind::Fn(info.fn_name.clone(), info.fn_ty.clone()),
                    });

                    let continue_block = self.new_block();
                    let destination = Some((place.clone(), continue_block));
                    let terminator = mir::Terminator {
                        source_info: expr.span,
                        kind: mir::TerminatorKind::Call {
                            func: func_operand,
                            args: lowered_args,
                            destination: destination.clone(),
                            cleanup: None,
                            from_hir_call: true,
                            fn_span: expr.span,
                        },
                    };

                    self.blocks[self.current_block as usize].terminator = Some(terminator);
                    self.current_block = continue_block;

                    let result_ty = info.sig.output.clone();
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = result_ty.clone();
                    }
                    if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                        self.local_structs.insert(place.local, struct_def);
                    }

                    return Ok(());
                }

                if method_name == "len" && args.is_empty() {
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        if let Some(hir::Res::Def(def_id)) = &path.res {
                            if let Some(const_info) = self.lowering.const_values.get(def_id) {
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = &const_info.ty.kind
                                {
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::Int(
                                                        len.data as i64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                            if let Some(konst) = self.const_items.get(def_id) {
                                let ty = self.lowering.lower_type_expr(&konst.ty);
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = ty.kind
                                {
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::Int(
                                                        len.data as i64,
                                                    ),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                            }
                        }
                    }
                    self.lowering.emit_error(
                        expr.span,
                        "len() method is only supported on constant arrays during lowering",
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place,
                            mir::Rvalue::Use(mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            )),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }

                self.lowering.emit_error(
                    expr.span,
                    "method calls are not yet supported in MIR lowering",
                );
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place,
                        mir::Rvalue::Use(mir::Operand::Constant(
                            self.lowering.error_constant(expr.span),
                        )),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, Some((place, expected_ty.clone())))?;
            }
            hir::ExprKind::Array(elements) => {
                let element_ty = self
                    .expect_array_element_ty(expected_ty)
                    .unwrap_or_else(|| {
                        self.lowering
                            .emit_error(expr.span, "array expression expected array type");
                        self.lowering.error_ty()
                    });

                let mut operands = Vec::with_capacity(elements.len());
                for element in elements {
                    let lowered = self.lower_operand(element, Some(&element_ty))?;
                    operands.push(lowered.operand);
                }

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(
                            mir::AggregateKind::Array(element_ty.clone()),
                            operands,
                        ),
                    ),
                };
                self.push_statement(statement);
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let element_ty = self
                    .expect_array_element_ty(expected_ty)
                    .unwrap_or_else(|| {
                        self.lowering
                            .emit_error(expr.span, "array repeat expression expected array type");
                        self.lowering.error_ty()
                    });

                let lowered_elem = self.lower_operand(elem, Some(&element_ty))?;
                let repeat_len = match self.evaluate_array_length(len) {
                    Some(len) => len,
                    None => {
                        self.lowering
                            .emit_error(len.span, "array repeat length must be a constant integer");
                        0
                    }
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Repeat(lowered_elem.operand, repeat_len),
                    ),
                };
                self.push_statement(statement);
            }
            _ => {
                self.lowering.emit_error(
                    expr.span,
                    format!(
                        "expression not yet supported for MIR assignment: {:?}",
                        expr.kind
                    ),
                );
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place,
                        mir::Rvalue::Use(mir::Operand::Constant(
                            self.lowering.error_constant(expr.span),
                        )),
                    ),
                };
                self.push_statement(statement);
            }
        }

        Ok(())
    }

    fn convert_bin_op(op: &hir::BinOp) -> mir::BinOp {
        match op {
            hir::BinOp::Add => mir::BinOp::Add,
            hir::BinOp::Sub => mir::BinOp::Sub,
            hir::BinOp::Mul => mir::BinOp::Mul,
            hir::BinOp::Div => mir::BinOp::Div,
            hir::BinOp::Rem => mir::BinOp::Rem,
            hir::BinOp::And => mir::BinOp::And,
            hir::BinOp::Or => mir::BinOp::Or,
            hir::BinOp::BitXor => mir::BinOp::BitXor,
            hir::BinOp::BitAnd => mir::BinOp::BitAnd,
            hir::BinOp::BitOr => mir::BinOp::BitOr,
            hir::BinOp::Shl => mir::BinOp::Shl,
            hir::BinOp::Shr => mir::BinOp::Shr,
            hir::BinOp::Eq => mir::BinOp::Eq,
            hir::BinOp::Ne => mir::BinOp::Ne,
            hir::BinOp::Lt => mir::BinOp::Lt,
            hir::BinOp::Le => mir::BinOp::Le,
            hir::BinOp::Gt => mir::BinOp::Gt,
            hir::BinOp::Ge => mir::BinOp::Ge,
        }
    }

    fn convert_un_op(op: &hir::UnOp) -> Option<mir::UnOp> {
        match op {
            hir::UnOp::Not => Some(mir::UnOp::Not),
            hir::UnOp::Neg => Some(mir::UnOp::Neg),
            hir::UnOp::Deref => None,
        }
    }

    fn binary_result_ty(op: &hir::BinOp, lhs_ty: &Ty) -> Ty {
        match op {
            hir::BinOp::Add
            | hir::BinOp::Sub
            | hir::BinOp::Mul
            | hir::BinOp::Div
            | hir::BinOp::Rem
            | hir::BinOp::BitXor
            | hir::BinOp::BitAnd
            | hir::BinOp::BitOr
            | hir::BinOp::Shl
            | hir::BinOp::Shr => lhs_ty.clone(),
            hir::BinOp::And
            | hir::BinOp::Or
            | hir::BinOp::Eq
            | hir::BinOp::Ne
            | hir::BinOp::Lt
            | hir::BinOp::Le
            | hir::BinOp::Gt
            | hir::BinOp::Ge => Ty { kind: TyKind::Bool },
        }
    }

    fn expect_array_element_ty(&self, ty: &Ty) -> Option<Ty> {
        match &ty.kind {
            TyKind::Array(elem, _) => Some(*elem.clone()),
            _ => None,
        }
    }

    fn evaluate_array_length(&self, expr: &hir::Expr) -> Option<u64> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Integer(value)) => Some(*value as u64),
            hir::ExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = path.res {
                    if let Some(const_info) = self.lowering.const_values.get(&def_id) {
                        match &const_info.value.literal {
                            mir::ConstantKind::Int(value) => Some(*value as u64),
                            mir::ConstantKind::UInt(value) => Some(*value),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn new_block(&mut self) -> mir::BasicBlockId {
        let id = self.blocks.len() as mir::BasicBlockId;
        self.blocks.push(mir::BasicBlockData::new(None));
        id
    }

    fn push_statement(&mut self, statement: mir::Statement) {
        if let Some(block) = self.blocks.get_mut(self.current_block as usize) {
            block.statements.push(statement);
        }
    }
}

impl IrTransform<hir::Program, mir::Program> for MirLowering {
    fn transform(&mut self, hir_program: hir::Program) -> Result<mir::Program> {
        self.lower_program(&hir_program)
    }
}
