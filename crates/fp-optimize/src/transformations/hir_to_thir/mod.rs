use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::error::Result;
use fp_core::hir::typed as thir;
use fp_core::hir::{self, ty as hir_types};
use fp_core::span::Span;
use std::collections::HashMap;
use std::mem;

use super::IrTransform;

mod context;
mod lowering;
mod type_inference;

#[cfg(test)]
mod tests;

use context::TypeContext;
use type_inference::TypeInferencer;

/// Generator for transforming HIR to THIR (Typed High-level IR)
/// This transformation performs type checking and inference
pub struct ThirGenerator {
    next_thir_id: thir::ThirId,
    type_context: TypeContext,
    body_map: HashMap<thir::BodyId, thir::Body>,
    next_body_id: u32,
    /// Map constant names to their THIR body IDs for resolution
    const_symbols: HashMap<hir_types::DefId, thir::BodyId>,
    /// Map constant names to their original HIR initializer expression for inlining
    const_init_map: HashMap<hir_types::DefId, hir::Expr>,
    /// Stack of block-local const initializers keyed by identifier.
    local_const_inits: Vec<HashMap<String, hir::Expr>>,
    /// Track local variable bindings within nested scopes.
    local_scopes: Vec<HashMap<String, (thir::LocalId, thir::Ty)>>,
    /// Accumulated THIR local declarations for the current body being lowered.
    current_locals: Vec<thir::LocalDecl>,
    /// Counter for allocating fresh THIR local identifiers.
    next_local_id: thir::LocalId,
    /// Inferred expression types keyed by HIR id.
    inferred_expr_types: HashMap<hir::HirId, hir_types::Ty>,
    /// Inferred pattern types keyed by HIR id.
    inferred_pattern_types: HashMap<hir::HirId, hir_types::Ty>,
    /// The `Self` type for the impl currently being lowered, if any.
    current_self_ty: Option<hir_types::Ty>,
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
            local_const_inits: Vec::new(),
            local_scopes: Vec::new(),
            current_locals: Vec::new(),
            next_local_id: 0,
            inferred_expr_types: HashMap::new(),
            inferred_pattern_types: HashMap::new(),
            current_self_ty: None,
        }
    }

    /// Transform HIR program to THIR
    pub fn transform(&mut self, hir_program: hir::Program) -> Result<thir::Program> {
        let mut thir_program = thir::Program::new();

        // Pass 0: collect type information
        self.collect_type_info(&hir_program)?;

        // Pass 1: transform and register consts first so paths can resolve to them
        for hir_item in &hir_program.items {
            if let hir::ItemKind::Const(const_def) = &hir_item.kind {
                let def_id = hir_item.def_id as hir_types::DefId;
                self.const_init_map
                    .insert(def_id, const_def.body.value.clone());
                let thir_const = self.transform_const(Some(def_id), const_def.clone())?;
                let thir_id = self.next_id();
                let const_ty = self.hir_ty_to_ty(&const_def.ty)?;
                thir_program.items.push(thir::Item {
                    thir_id,
                    kind: thir::ItemKind::Const(thir_const),
                    ty: const_ty,
                    span: hir_item.span,
                });
            }
        }

        // Pass 2: transform remaining items with consts available
        for hir_item in hir_program.items {
            if matches!(hir_item.kind, hir::ItemKind::Const(_)) {
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

    pub(super) fn lookup_expr_ty(&self, hir_id: hir::HirId) -> Option<&hir_types::Ty> {
        self.inferred_expr_types.get(&hir_id)
    }

    fn lookup_local_const_init(&self, name: &str) -> Option<hir::Expr> {
        for scope in self.local_const_inits.iter().rev() {
            if let Some(expr) = scope.get(name) {
                return Some(expr.clone());
            }
        }
        None
    }

    pub(super) fn lookup_pattern_ty(&self, hir_id: hir::HirId) -> Option<&hir_types::Ty> {
        self.inferred_pattern_types.get(&hir_id)
    }

    /// Collect type information from HIR program
    fn collect_type_info(&mut self, hir_program: &hir::Program) -> Result<()> {
        for item in &hir_program.items {
            self.predeclare_structs_item(item);
        }

        for item in &hir_program.items {
            self.collect_item_type_info(item)?;
        }
        Ok(())
    }

    fn predeclare_structs_item(&mut self, item: &hir::Item) {
        if let hir::ItemKind::Struct(struct_def) = &item.kind {
            self.type_context
                .declare_struct(item.def_id as hir_types::DefId, &struct_def.name);
        }

        match &item.kind {
            hir::ItemKind::Function(func) => {
                if let Some(body) = &func.body {
                    self.predeclare_structs_expr(&body.value);
                }
            }
            hir::ItemKind::Impl(impl_block) => {
                for impl_item in &impl_block.items {
                    match &impl_item.kind {
                        hir::ImplItemKind::Method(method) => {
                            if let Some(body) = &method.body {
                                self.predeclare_structs_expr(&body.value);
                            }
                        }
                        hir::ImplItemKind::AssocConst(_) => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn predeclare_structs_expr(&mut self, expr: &hir::Expr) {
        use hir::ExprKind;
        match &expr.kind {
            ExprKind::Block(block) => self.predeclare_structs_block(block),
            ExprKind::If(cond, then_expr, else_expr) => {
                self.predeclare_structs_expr(cond);
                self.predeclare_structs_expr(then_expr);
                if let Some(else_expr) = else_expr {
                    self.predeclare_structs_expr(else_expr);
                }
            }
            ExprKind::Binary(_, lhs, rhs) => {
                self.predeclare_structs_expr(lhs);
                self.predeclare_structs_expr(rhs);
            }
            ExprKind::Unary(_, inner) => self.predeclare_structs_expr(inner),
            ExprKind::Call(callee, args) => {
                self.predeclare_structs_expr(callee);
                for arg in args {
                    self.predeclare_structs_expr(arg);
                }
            }
            ExprKind::MethodCall(receiver, _, args) => {
                self.predeclare_structs_expr(receiver);
                for arg in args {
                    self.predeclare_structs_expr(arg);
                }
            }
            ExprKind::FieldAccess(owner, _) => self.predeclare_structs_expr(owner),
            ExprKind::Struct(_, fields) => {
                for field in fields {
                    self.predeclare_structs_expr(&field.expr);
                }
            }
            ExprKind::Assign(lhs, rhs) => {
                self.predeclare_structs_expr(lhs);
                self.predeclare_structs_expr(rhs);
            }
            ExprKind::Return(value) | ExprKind::Break(value) => {
                if let Some(expr) = value {
                    self.predeclare_structs_expr(expr);
                }
            }
            ExprKind::Loop(block) => self.predeclare_structs_block(block),
            ExprKind::While(cond, block) => {
                self.predeclare_structs_expr(cond);
                self.predeclare_structs_block(block);
            }
            ExprKind::IntrinsicCall(call) => match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &template.args {
                        self.predeclare_structs_expr(arg);
                    }
                    for kw in &template.kwargs {
                        self.predeclare_structs_expr(&kw.value);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.predeclare_structs_expr(arg);
                    }
                }
            },
            ExprKind::Let(_, _, init) => {
                if let Some(expr) = init {
                    self.predeclare_structs_expr(expr);
                }
            }
            ExprKind::Path(_) | ExprKind::Literal(_) | ExprKind::Continue => {}
        }
    }

    fn predeclare_structs_block(&mut self, block: &hir::Block) {
        for stmt in &block.stmts {
            self.predeclare_structs_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.predeclare_structs_expr(expr);
        }
    }

    fn predeclare_structs_stmt(&mut self, stmt: &hir::Stmt) {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                if let Some(init) = &local.init {
                    self.predeclare_structs_expr(init);
                }
            }
            hir::StmtKind::Item(item) => {
                self.predeclare_structs_item(item);
            }
            hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                self.predeclare_structs_expr(expr);
            }
        }
    }

    fn collect_item_type_info(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Function(func) => {
                let fn_sig = self.build_fn_sig(&func.sig)?;
                self.type_context.register_function(
                    item.def_id as hir_types::DefId,
                    &func.sig.name,
                    fn_sig,
                );
                if let Some(body) = &func.body {
                    self.collect_body_type_info(body)?;
                }
            }
            hir::ItemKind::Struct(struct_def) => {
                let fields = struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let ty = self.hir_ty_to_ty(&field.ty)?;
                        Ok((field.name.clone(), ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.type_context
                    .init_struct_fields(item.def_id as hir_types::DefId, fields);
            }
            hir::ItemKind::Const(const_def) => {
                let ty = self.hir_ty_to_ty(&const_def.ty)?;
                self.type_context.register_const(
                    item.def_id as hir_types::DefId,
                    &const_def.name,
                    ty,
                );
                self.collect_body_type_info(&const_def.body)?;
            }
            hir::ItemKind::Impl(impl_block) => {
                let self_ty = self.hir_ty_to_ty(&impl_block.self_ty)?;
                let saved_self = self.current_self_ty.clone();
                self.current_self_ty = Some(self_ty.clone());

                if let Some(owner_def_id) = Self::type_def_id(&self_ty) {
                    for impl_item in &impl_block.items {
                        if let hir::ImplItemKind::Method(method) = &impl_item.kind {
                            let fn_sig = self.build_fn_sig(&method.sig)?;
                            self.type_context.register_method(
                                owner_def_id,
                                &method.sig.name,
                                fn_sig,
                            );
                            if let Some(body) = &method.body {
                                self.collect_body_type_info(body)?;
                            }
                        }
                    }
                }

                self.current_self_ty = saved_self;
            }
        }
        Ok(())
    }

    fn collect_body_type_info(&mut self, body: &hir::Body) -> Result<()> {
        self.collect_expr_type_info(&body.value)
    }

    fn collect_block_type_info(&mut self, block: &hir::Block) -> Result<()> {
        for stmt in &block.stmts {
            self.collect_stmt_type_info(stmt)?;
        }
        if let Some(expr) = &block.expr {
            self.collect_expr_type_info(expr)?;
        }
        Ok(())
    }

    fn collect_stmt_type_info(&mut self, stmt: &hir::Stmt) -> Result<()> {
        match &stmt.kind {
            hir::StmtKind::Local(local) => {
                if let Some(init) = &local.init {
                    self.collect_expr_type_info(init)?;
                }
                Ok(())
            }
            hir::StmtKind::Item(item) => self.collect_item_type_info(item),
            hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => {
                self.collect_expr_type_info(expr)
            }
        }
    }

    fn collect_expr_type_info(&mut self, expr: &hir::Expr) -> Result<()> {
        use hir::ExprKind;
        match &expr.kind {
            ExprKind::Literal(_) | ExprKind::Path(_) | ExprKind::Continue => Ok(()),
            ExprKind::Binary(_, lhs, rhs) => {
                self.collect_expr_type_info(lhs)?;
                self.collect_expr_type_info(rhs)
            }
            ExprKind::Unary(_, inner) => self.collect_expr_type_info(inner),
            ExprKind::Call(callee, args) => {
                self.collect_expr_type_info(callee)?;
                for arg in args {
                    self.collect_expr_type_info(arg)?;
                }
                Ok(())
            }
            ExprKind::MethodCall(receiver, _, args) => {
                self.collect_expr_type_info(receiver)?;
                for arg in args {
                    self.collect_expr_type_info(arg)?;
                }
                Ok(())
            }
            ExprKind::FieldAccess(owner, _) => self.collect_expr_type_info(owner),
            ExprKind::Struct(_, fields) => {
                for field in fields {
                    self.collect_expr_type_info(&field.expr)?;
                }
                Ok(())
            }
            ExprKind::If(cond, then_expr, else_expr) => {
                self.collect_expr_type_info(cond)?;
                self.collect_expr_type_info(then_expr)?;
                if let Some(else_expr) = else_expr {
                    self.collect_expr_type_info(else_expr)?;
                }
                Ok(())
            }
            ExprKind::Block(block) => self.collect_block_type_info(block),
            ExprKind::IntrinsicCall(call) => match &call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &template.args {
                        self.collect_expr_type_info(arg)?;
                    }
                    for kw in &template.kwargs {
                        self.collect_expr_type_info(&kw.value)?;
                    }
                    Ok(())
                }
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.collect_expr_type_info(arg)?;
                    }
                    Ok(())
                }
            },
            ExprKind::Let(_, _, init) => {
                if let Some(expr) = init {
                    self.collect_expr_type_info(expr)?;
                }
                Ok(())
            }
            ExprKind::Assign(lhs, rhs) => {
                self.collect_expr_type_info(lhs)?;
                self.collect_expr_type_info(rhs)
            }
            ExprKind::Return(value) | ExprKind::Break(value) => {
                if let Some(expr) = value {
                    self.collect_expr_type_info(expr)?;
                }
                Ok(())
            }
            ExprKind::Loop(block) => self.collect_block_type_info(block),
            ExprKind::While(cond, block) => {
                self.collect_expr_type_info(cond)?;
                self.collect_block_type_info(block)
            }
        }
    }

    fn build_fn_sig(&mut self, sig: &hir::FunctionSig) -> Result<hir_types::FnSig> {
        let input_types = sig
            .inputs
            .iter()
            .map(|param| self.hir_ty_to_ty(&param.ty))
            .collect::<Result<Vec<_>>>()?;

        let output_type = self.hir_ty_to_ty(&sig.output)?;

        Ok(hir_types::FnSig {
            inputs: input_types.into_iter().map(Box::new).collect(),
            output: Box::new(output_type),
            c_variadic: false,
            unsafety: hir_types::Unsafety::Normal,
            abi: hir_types::Abi::Rust,
        })
    }

    fn type_def_id(ty: &hir_types::Ty) -> Option<hir_types::DefId> {
        match &ty.kind {
            hir_types::TyKind::Adt(adt_def, _) => Some(adt_def.did),
            hir_types::TyKind::Ref(_, inner, _) => Self::type_def_id(inner),
            _ => None,
        }
    }

    fn make_primitive_ty(&self, name: &str) -> Option<hir_types::Ty> {
        match name {
            "bool" => Some(hir_types::Ty::bool()),
            "char" => Some(hir_types::Ty::char()),
            "str" => Some(hir_types::Ty {
                kind: hir_types::TyKind::Slice(Box::new(hir_types::Ty::char())),
            }),
            "String" => Some(hir_types::Ty {
                kind: hir_types::TyKind::Adt(
                    hir_types::AdtDef {
                        did: 0,
                        variants: Vec::new(),
                        flags: hir_types::AdtFlags::IS_STRUCT,
                        repr: hir_types::ReprOptions {
                            int: None,
                            align: None,
                            pack: None,
                            flags: hir_types::ReprFlags::empty(),
                            field_shuffle_seed: 0,
                        },
                    },
                    Vec::new(),
                ),
            }),
            "i8" => Some(hir_types::Ty::int(hir_types::IntTy::I8)),
            "i16" => Some(hir_types::Ty::int(hir_types::IntTy::I16)),
            "i32" => Some(hir_types::Ty::int(hir_types::IntTy::I32)),
            "i64" => Some(hir_types::Ty::int(hir_types::IntTy::I64)),
            "i128" => Some(hir_types::Ty::int(hir_types::IntTy::I128)),
            "isize" => Some(hir_types::Ty::int(hir_types::IntTy::Isize)),
            "u8" => Some(hir_types::Ty::uint(hir_types::UintTy::U8)),
            "u16" => Some(hir_types::Ty::uint(hir_types::UintTy::U16)),
            "u32" => Some(hir_types::Ty::uint(hir_types::UintTy::U32)),
            "u64" => Some(hir_types::Ty::uint(hir_types::UintTy::U64)),
            "u128" => Some(hir_types::Ty::uint(hir_types::UintTy::U128)),
            "usize" => Some(hir_types::Ty::uint(hir_types::UintTy::Usize)),
            "f32" => Some(hir_types::Ty::float(hir_types::FloatTy::F32)),
            "f64" => Some(hir_types::Ty::float(hir_types::FloatTy::F64)),
            _ => None,
        }
    }

    fn path_to_type_info(
        &mut self,
        path: &hir::Path,
    ) -> Result<(
        Option<hir_types::DefId>,
        String,
        String,
        hir_types::SubstsRef,
    )> {
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
                Some(hir::Res::Def(id)) => Some(id as hir_types::DefId),
                _ => None,
            };
            Ok((def_id, qualified, base_name, substs))
        } else {
            Err(crate::error::optimization_error(
                "Encountered empty path while lowering type".to_string(),
            ))
        }
    }

    fn convert_hir_generic_args(
        &mut self,
        args: &hir::GenericArgs,
    ) -> Result<hir_types::SubstsRef> {
        let mut substs = Vec::new();
        for arg in &args.args {
            match arg {
                hir::GenericArg::Type(ty) => {
                    substs.push(hir_types::GenericArg::Type(self.hir_ty_to_ty(ty)?));
                }
                hir::GenericArg::Const(_) => {
                    substs.push(hir_types::GenericArg::Const(hir_types::ConstKind::Value(
                        hir_types::ConstValue::ZeroSized,
                    )));
                }
            }
        }
        Ok(substs)
    }
}

impl IrTransform<hir::Program, thir::Program> for ThirGenerator {
    fn transform(&mut self, source: hir::Program) -> Result<thir::Program> {
        ThirGenerator::transform(self, source)
    }
}

impl Default for ThirGenerator {
    fn default() -> Self {
        Self::new()
    }
}
