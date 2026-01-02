// HIR→MIR lowering implementation (moved from mod.rs)
// This file currently contains the full original implementation and will be
// gradually split into stmt/control_flow/types/borrow submodules.

// BEGIN ORIGINAL CONTENT
use fp_core::ast::{DecimalType, TypeInt, TypePrimitive};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result;
use fp_core::hir;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::mir::ty::{
    AdtDef, AdtFlags, ConstKind, ConstValue, CtorKind, ErrorGuaranteed, FloatTy, IntTy, Mutability,
    ReprFlags, ReprOptions, Scalar, ScalarInt, Ty, TyKind, TypeAndMut, UintTy, VariantDef,
    VariantDiscr,
};
use fp_core::mir::{self, Symbol};
use fp_core::span::Span;
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet};
use std::hash::{Hash, Hasher};

use crate::transformations::IrTransform;

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

// TODO(jakku): The current MIR lowering is missing a real monomorphization pass.
// We have to create monomorphic method/function bodies when generic impls are
// invoked with concrete types. The intended flow is:
// - Cache the generic method definition here (method_defs).
// - On call, compute concrete type substitutions from the callee path's
//   generic args and/or the expected return type.
// - Build a specialized MethodLoweringInfo and emit a cloned MIR body using
//   lower_function_sig_with_substs + BodyBuilder (type_substs).
// - Avoid re-emitting by caching MethodSpecializationKey in method_specializations.
// This is required to fix generic enum payloads and to eliminate invalid
// bitcasts (e.g., in examples/17_generics).
#[derive(Clone)]
struct MethodDefinition {
    function: hir::Function,
    impl_generics: hir::Generics,
    self_ty: hir::TypeExpr,
    self_def: Option<hir::DefId>,
    method_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct MethodSpecializationKey {
    method_name: String,
    args: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionSpecializationKey {
    def_id: hir::DefId,
    args: Vec<Ty>,
}

#[derive(Clone, Debug)]
struct FunctionSpecializationInfo {
    name: String,
    sig: mir::FunctionSig,
    fn_ty: Ty,
}

#[derive(Clone, Debug)]
struct EnumLayout {
    tag_ty: Ty,
    payload_tys: Vec<Ty>,
    enum_ty: Ty,
    variant_payloads: HashMap<hir::DefId, Vec<Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct EnumLayoutKey {
    def_id: hir::DefId,
    args: Vec<Ty>,
}

#[derive(Clone, Debug)]
struct EnumDefinition {
    def_id: hir::DefId,
    name: String,
    generics: Vec<String>,
    variants: Vec<EnumVariantDef>,
}

#[derive(Clone, Debug)]
struct EnumVariantDef {
    def_id: hir::DefId,
    name: String,
    discriminant: i64,
    payload: Option<hir::TypeExpr>,
}

#[derive(Clone, Debug)]
struct EnumVariantInfo {
    def_id: hir::DefId,
    enum_def: hir::DefId,
    discriminant: i64,
}

#[derive(Clone, Debug)]
struct MethodContext {
    def_id: Option<hir::DefId>,
    path: Vec<hir::PathSegment>,
    mir_self_ty: Ty,
}

#[derive(Clone, Debug)]
struct StructDefinition {
    def_id: hir::DefId,
    name: String,
    generics: Vec<String>,
    fields: Vec<StructFieldDef>,
    field_index: HashMap<String, usize>,
}

#[derive(Clone, Debug)]
struct StructFieldDef {
    name: String,
    ty: hir::TypeExpr,
}

#[derive(Clone, Debug)]
struct StructLayout {
    ty: Ty,
    field_tys: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StructLayoutKey {
    def_id: hir::DefId,
    args: Vec<Ty>,
}

#[derive(Clone)]
struct StructFieldInfo {
    name: String,
    ty: Ty,
}

#[derive(Clone)]
struct ConstInfo {
    ty: Ty,
    value: mir::Constant,
}

pub struct MirLowering {
    next_mir_id: mir::MirId,
    next_body_id: u32,
    next_error_id: u32,
    next_synthetic_def_id: mir::ty::DefId,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    struct_defs: HashMap<hir::DefId, StructDefinition>,
    struct_layouts: HashMap<StructLayoutKey, StructLayout>,
    struct_layouts_by_ty: HashMap<Ty, StructLayoutKey>,
    enum_defs: HashMap<hir::DefId, EnumDefinition>,
    enum_layouts: HashMap<EnumLayoutKey, EnumLayout>,
    enum_variants: HashMap<hir::DefId, EnumVariantInfo>,
    enum_variant_names: HashMap<String, hir::DefId>,
    const_values: HashMap<hir::DefId, ConstInfo>,
    function_sigs: HashMap<hir::DefId, mir::FunctionSig>,
    generic_function_defs: HashMap<hir::DefId, hir::Function>,
    runtime_functions: HashMap<String, mir::FunctionSig>,
    struct_methods: HashMap<String, HashMap<String, MethodLoweringInfo>>,
    method_lookup: HashMap<String, MethodLoweringInfo>,
    method_defs: HashMap<String, MethodDefinition>,
    method_specializations: HashMap<MethodSpecializationKey, MethodLoweringInfo>,
    function_specializations: HashMap<FunctionSpecializationKey, FunctionSpecializationInfo>,
    extra_items: Vec<mir::Item>,
    extra_bodies: Vec<(mir::BodyId, mir::Body)>,
    opaque_types: HashMap<String, Ty>,
    synthetic_runtime_functions: HashSet<String>,
    tolerate_errors: bool,
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
            next_synthetic_def_id: 1,
            diagnostics: Vec::new(),
            has_errors: false,
            struct_defs: HashMap::new(),
            struct_layouts: HashMap::new(),
            struct_layouts_by_ty: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_layouts: HashMap::new(),
            enum_variants: HashMap::new(),
            enum_variant_names: HashMap::new(),
            const_values: HashMap::new(),
            function_sigs: HashMap::new(),
            generic_function_defs: HashMap::new(),
            runtime_functions: Self::default_runtime_signatures(),
            struct_methods: HashMap::new(),
            method_lookup: HashMap::new(),
            method_defs: HashMap::new(),
            method_specializations: HashMap::new(),
            function_specializations: HashMap::new(),
            extra_items: Vec::new(),
            extra_bodies: Vec::new(),
            opaque_types: HashMap::new(),
            synthetic_runtime_functions: HashSet::new(),
            tolerate_errors: false,
        }
    }

    pub fn set_error_tolerance(&mut self, enabled: bool) {
        self.tolerate_errors = enabled;
    }

    fn lower_program(&mut self, program: &hir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id, def, item.span);
                }
                hir::ItemKind::Const(const_item) => {
                    let mir_item = self.lower_const(item.def_id, const_item)?;
                    mir_program.items.push(mir_item);
                }
                hir::ItemKind::Function(function) => {
                    if !function.sig.generics.params.is_empty() {
                        self.register_generic_function(item.def_id, function);
                    } else {
                        let (mir_item, body_id, body) =
                            self.lower_function(program, item, function)?;
                        mir_program.items.push(mir_item);
                        mir_program.bodies.insert(body_id, body);
                    }
                }
                hir::ItemKind::Impl(impl_block) => {
                    self.lower_impl(program, item, impl_block, Some(&mut mir_program))?;
                }
            }
        }

        self.flush_extra_items(&mut mir_program);
        self.append_runtime_stubs(&mut mir_program);

        Ok(mir_program)
    }

    fn append_runtime_stubs(&mut self, program: &mut mir::Program) {
        let span = Span::new(0, 0, 0);
        for name in self.synthetic_runtime_functions.clone() {
            // C runtime intrinsics are resolved as externs during LIR/LLVM lowering.
            if self.is_extern_runtime_function(&name) {
                continue;
            }
            let exists = program.items.iter().any(|item| match &item.kind {
                mir::ItemKind::Function(func) => func.name.as_str() == name,
                _ => false,
            });
            if exists {
                continue;
            }

            let Some(sig) = self.runtime_functions.get(&name).cloned() else {
                continue;
            };

            let mut locals = Vec::new();
            locals.push(self.make_local_decl(&sig.output, span));
            for input in &sig.inputs {
                locals.push(self.make_local_decl(input, span));
            }

            let mut basic_block = mir::BasicBlockData::new(Some(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Return,
            }));

            if matches!(
                sig.output.kind,
                TyKind::Bool
                    | TyKind::Int(_)
                    | TyKind::Uint(_)
                    | TyKind::Float(_)
                    | TyKind::Ref(_, _, _)
                    | TyKind::RawPtr(_)
            ) {
                let assign = mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(0),
                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                            span,
                            user_ty: None,
                            literal: self.default_constant_for_ty(&sig.output),
                        })),
                    ),
                };
                basic_block.statements.push(assign);
            }

            let body = mir::Body::new(vec![basic_block], locals, sig.inputs.len(), span);
            let body_id = mir::BodyId::new(self.next_body_id);
            self.next_body_id += 1;
            program.bodies.insert(body_id, body);

            let mir_function = mir::Function {
                name: mir::Symbol::new(name.clone()),
                path: Vec::new(),
                def_id: None,
                sig: sig.clone(),
                body_id,
            };

            program.items.push(mir::Item {
                mir_id: self.next_mir_id,
                kind: mir::ItemKind::Function(mir_function),
            });
            self.next_mir_id += 1;
        }
    }

    fn is_extern_runtime_function(&self, name: &str) -> bool {
        matches!(name, "printf")
    }

    fn default_constant_for_ty(&self, ty: &Ty) -> mir::ConstantKind {
        match &ty.kind {
            TyKind::Bool => mir::ConstantKind::Bool(false),
            TyKind::Int(_) => mir::ConstantKind::Int(0),
            TyKind::Uint(_) => mir::ConstantKind::UInt(0),
            TyKind::Float(_) => mir::ConstantKind::Float(0.0),
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => mir::ConstantKind::UInt(0),
            _ => mir::ConstantKind::Int(0),
        }
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
            name: mir::Symbol::from(function.sig.name.clone()),
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

    fn register_generic_function(&mut self, def_id: hir::DefId, function: &hir::Function) {
        if self.generic_function_defs.contains_key(&def_id) {
            return;
        }
        let sig = self.lower_function_sig(&function.sig, None);
        self.function_sigs.insert(def_id, sig);
        self.generic_function_defs.insert(def_id, function.clone());
    }

    fn lower_function_with_substs(
        &mut self,
        program: &hir::Program,
        item: &hir::Item,
        function: &hir::Function,
        sig: &mir::FunctionSig,
        substs: HashMap<String, Ty>,
        name_override: &str,
    ) -> Result<(mir::Item, mir::BodyId, mir::Body)> {
        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let span = function
            .body
            .as_ref()
            .map(|body| body.value.span)
            .unwrap_or(item.span);

        let mir_body = BodyBuilder::new(
            self,
            program,
            function,
            sig,
            span,
            None,
            substs,
        )
        .lower()?;

        let mir_function = mir::Function {
            name: mir::Symbol::new(name_override),
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

        Ok((mir_item, body_id, mir_body))
    }

    fn ensure_function_specialization(
        &mut self,
        program: &hir::Program,
        def_id: hir::DefId,
        function: &hir::Function,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        span: Span,
    ) -> Result<FunctionSpecializationInfo> {
        let generics = function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();
        let substs =
            self.build_substs_from_args(&generics, &function.sig.inputs, explicit_args, arg_types, span)?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let key = FunctionSpecializationKey {
            def_id,
            args: args_in_order.clone(),
        };

        if let Some(info) = self.function_specializations.get(&key) {
            return Ok(info.clone());
        }

        let sig = self.lower_function_sig_with_substs(&function.sig, None, &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}_{}", function.sig.name.as_str(), suffix, def_id);
        let fn_ty = self.function_pointer_ty(&sig);

        let item = program
            .def_map
            .get(&def_id)
            .ok_or_else(|| crate::error::optimization_error("missing function item"))?;
        let (mir_item, body_id, body) =
            self.lower_function_with_substs(program, item, function, &sig, substs, &name)?;
        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, body));

        let info = FunctionSpecializationInfo {
            name: name.clone(),
            sig: sig.clone(),
            fn_ty: fn_ty.clone(),
        };
        self.function_specializations.insert(key, info.clone());
        Ok(info)
    }

    fn ensure_method_specialization(
        &mut self,
        program: &hir::Program,
        def: &MethodDefinition,
        explicit_args: &[Ty],
        arg_types: &[Ty],
        span: Span,
    ) -> Result<MethodLoweringInfo> {
        let impl_generics = def
            .impl_generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let method_generics = def
            .function
            .sig
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string());
        let generics = impl_generics.chain(method_generics).collect::<Vec<_>>();

        let substs =
            self.build_substs_from_args(&generics, &def.function.sig.inputs, explicit_args, arg_types, span)?;
        let args_in_order = generics
            .iter()
            .filter_map(|name| substs.get(name).cloned())
            .collect::<Vec<_>>();
        let key = MethodSpecializationKey {
            method_name: def.method_name.clone(),
            args: args_in_order.clone(),
        };

        if let Some(info) = self.method_specializations.get(&key) {
            return Ok(info.clone());
        }

        let mut method_context = if let hir::TypeExprKind::Path(path) = &def.self_ty.kind {
            let mir_self_ty = self.lower_type_expr_with_substs(&def.self_ty, &substs);
            Some(MethodContext {
                def_id: def.self_def,
                path: path.segments.clone(),
                mir_self_ty,
            })
        } else {
            None
        };

        let sig =
            self.lower_function_sig_with_substs(&def.function.sig, method_context.as_ref(), &substs);
        let suffix = self.specialization_suffix(&args_in_order);
        let name = format!("{}__{}", def.method_name, suffix);
        let fn_ty = self.function_pointer_ty(&sig);

        let body_id = mir::BodyId::new(self.next_body_id);
        self.next_body_id += 1;

        let span = def
            .function
            .body
            .as_ref()
            .map(|body| body.value.span)
            .unwrap_or(span);
        let mir_body = BodyBuilder::new(
            self,
            program,
            &def.function,
            &sig,
            span,
            method_context.take(),
            substs,
        )
        .lower()?;

        let mir_function = mir::Function {
            name: mir::Symbol::new(name.clone()),
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

        self.extra_items.push(mir_item);
        self.extra_bodies.push((body_id, mir_body));

        let info = MethodLoweringInfo {
            sig,
            fn_name: name.clone(),
            fn_ty,
            struct_def: def.self_def,
        };
        self.method_specializations.insert(key, info.clone());
        Ok(info)
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

    fn lower_function_sig_with_substs(
        &mut self,
        sig: &hir::FunctionSig,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
    ) -> mir::FunctionSig {
        mir::FunctionSig {
            inputs: sig
                .inputs
                .iter()
                .map(|param| {
                    self.lower_type_expr_with_context_and_substs(
                        &param.ty,
                        method_context,
                        substs,
                    )
                })
                .collect(),
            output: self.lower_type_expr_with_context_and_substs(&sig.output, method_context, substs),
        }
    }

    fn specialization_suffix(&self, args: &[Ty]) -> String {
        let mut hasher = DefaultHasher::new();
        for ty in args {
            ty.hash(&mut hasher);
        }
        format!("mono_{:x}", hasher.finish())
    }

    fn build_substs_from_args(
        &mut self,
        generics: &[String],
        params: &[hir::Param],
        explicit_args: &[Ty],
        arg_types: &[Ty],
        span: Span,
    ) -> Result<HashMap<String, Ty>> {
        if params.len() != arg_types.len() {
            self.emit_error(
                span,
                format!(
                    "generic call argument count mismatch: expected {}, got {}",
                    params.len(),
                    arg_types.len()
                ),
            );
            return Err(crate::error::optimization_error(
                "generic call argument count mismatch",
            ));
        }
        if !explicit_args.is_empty() && explicit_args.len() != generics.len() {
            self.emit_error(
                span,
                format!(
                    "expected {} generic arguments, got {}",
                    generics.len(),
                    explicit_args.len()
                ),
            );
            return Err(crate::error::optimization_error(
                "generic argument count mismatch",
            ));
        }

        let mut substs = HashMap::new();
        for (name, ty) in generics.iter().zip(explicit_args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }

        for (param, actual_ty) in params.iter().zip(arg_types.iter()) {
            self.infer_generic_from_type_expr(
                &param.ty,
                actual_ty,
                generics,
                &mut substs,
                span,
            )?;
        }

        for name in generics {
            if !substs.contains_key(name) {
                self.emit_error(
                    span,
                    format!(
                        "unable to infer generic parameter `{}`; add explicit type arguments",
                        name
                    ),
                );
                return Err(crate::error::optimization_error(
                    "generic parameter inference failed",
                ));
            }
        }

        Ok(substs)
    }

    fn infer_generic_from_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
        actual_ty: &Ty,
        generics: &[String],
        substs: &mut HashMap<String, Ty>,
        span: Span,
    ) -> Result<()> {
        // Keep inference conservative: only bind direct generic parameters.
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => {
                let name = path
                    .segments
                    .first()
                    .map(|seg| seg.name.as_str())
                    .unwrap_or("");
                if path.segments.len() == 1
                    && path.segments[0].args.is_none()
                    && generics.iter().any(|g| g == name)
                {
                    if let Some(existing) = substs.get(name) {
                        if existing != actual_ty {
                            self.emit_error(
                                span,
                                format!(
                                    "conflicting inference for `{}`: {:?} vs {:?}",
                                    name, existing, actual_ty
                                ),
                            );
                            return Err(crate::error::optimization_error(
                                "conflicting generic inference",
                            ));
                        }
                    } else {
                        substs.insert(name.to_string(), actual_ty.clone());
                    }
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                if let TyKind::Ref(_, actual_inner, _) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                if let TyKind::RawPtr(actual_inner) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.ty.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn lower_type_expr_with_context_and_substs(
        &mut self,
        ty_expr: &hir::TypeExpr,
        method_context: Option<&MethodContext>,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        if let Some(ctx) = method_context {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    return ctx.mir_self_ty.clone();
                }
            }
        }
        if substs.is_empty() {
            return self.lower_type_expr_with_context(ty_expr, method_context);
        }
        self.lower_type_expr_with_substs(ty_expr, substs)
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

        BodyBuilder::new(
            self,
            program,
            function,
            sig,
            span,
            method_context,
            HashMap::new(),
        )
        .lower()
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
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr(elem);
                Ty {
                    kind: TyKind::Slice(Box::new(elem_ty)),
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
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                let inputs = fn_ptr
                    .inputs
                    .iter()
                    .map(|ty| Box::new(self.lower_type_expr(ty)))
                    .collect();
                let output = Box::new(self.lower_type_expr(&fn_ptr.output));
                Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs,
                                output,
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::Rust,
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                }
            }
            hir::TypeExprKind::Never => Ty {
                kind: TyKind::Never,
            },
            hir::TypeExprKind::Infer => {
                self.emit_warning(
                    ty_expr.span,
                    "type inference markers not supported in HIR→MIR; using error type",
                );
                self.error_ty()
            }
            hir::TypeExprKind::Error => self.error_ty(),
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
            TypePrimitive::String => {
                Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                }
            }
            TypePrimitive::List => {
                self.emit_warning(
                    span,
                    "treating list primitive as opaque type during MIR lowering",
                );
                self.opaque_ty("list")
            }
        }
    }

    fn lower_path_type(&mut self, path: &hir::Path, span: Span) -> Ty {
        if let Some(res) = &path.res {
            if let hir::Res::Def(def_id) = res {
                if self.struct_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) = self.struct_layout_for_instance(*def_id, &args, span) {
                        return layout.ty.clone();
                    }
                    return self.error_ty();
                }
                if self.enum_defs.contains_key(def_id) {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lower_generic_args(Some(args), span))
                        .unwrap_or_default();
                    if let Some(layout) = self.enum_layout_for_instance(*def_id, &args, span) {
                        return layout.enum_ty.clone();
                    }
                    return self.error_ty();
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
                "i8" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I8),
                    }
                }
                "i16" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I16),
                    }
                }
                "i32" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I32),
                    }
                }
                "i64" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I64),
                    }
                }
                "i128" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I128),
                    }
                }
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
                "u8" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U8),
                    }
                }
                "u16" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U16),
                    }
                }
                "u32" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U32),
                    }
                }
                "u64" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    }
                }
                "u128" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U128),
                    }
                }
                "bool" => return Ty { kind: TyKind::Bool },
                "char" => return Ty { kind: TyKind::Char },
                "f32" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F32),
                    }
                }
                "f64" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    }
                }
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
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.emit_warning(
            span,
            format!(
                "treating type `{}` as opaque during MIR lowering (bootstrap placeholder)",
                display
            ),
        );
        self.opaque_ty(&display)
    }

    fn register_struct(&mut self, def_id: hir::DefId, strukt: &hir::Struct, span: Span) {
        if self.struct_defs.contains_key(&def_id) {
            return;
        }

        let mut fields = Vec::new();
        let mut field_index = HashMap::new();

        for (idx, field) in strukt.fields.iter().enumerate() {
            fields.push(StructFieldDef {
                name: String::from(field.name.clone()),
                ty: field.ty.clone(),
            });
            field_index.insert(String::from(field.name.clone()), idx);
        }

        let generics = strukt
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();

        self.struct_defs.insert(
            def_id,
            StructDefinition {
                def_id,
                name: String::from(strukt.name.clone()),
                generics,
                fields,
                field_index,
            },
        );

        if strukt.generics.params.is_empty() {
            let _ = self.struct_layout_for_instance(def_id, &[], span);
        }
    }

    fn register_enum(&mut self, def_id: hir::DefId, enm: &hir::Enum, span: Span) {
        if self.enum_defs.contains_key(&def_id) {
            return;
        }

        let generics = enm
            .generics
            .params
            .iter()
            .map(|param| param.name.as_str().to_string())
            .collect::<Vec<_>>();

        let mut variants = Vec::new();
        let mut next_value: i64 = 0;
        for variant in &enm.variants {
            let value = if let Some(expr) = &variant.discriminant {
                match self.eval_int_expr(expr) {
                    Some(val) => {
                        next_value = val.saturating_add(1);
                        val
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
                        val
                    }
                }
            } else {
                let val = next_value;
                next_value = next_value.saturating_add(1);
                val
            };

            variants.push(EnumVariantDef {
                def_id: variant.def_id,
                name: variant.name.as_str().to_string(),
                discriminant: value,
                payload: variant.payload.clone(),
            });

            self.enum_variants.insert(
                variant.def_id,
                EnumVariantInfo {
                    def_id: variant.def_id,
                    enum_def: def_id,
                    discriminant: value,
                },
            );

            let qualified_name = format!("{}::{}", enm.name.as_str(), variant.name.as_str());
            self.enum_variant_names
                .insert(qualified_name.clone(), variant.def_id);
            self.enum_variant_names
                .entry(variant.name.as_str().to_string())
                .or_insert(variant.def_id);

        }

        self.enum_defs.insert(
            def_id,
            EnumDefinition {
                def_id,
                name: enm.name.as_str().to_string(),
                generics,
                variants,
            },
        );

        if enm.generics.params.is_empty() {
            let _ = self.enum_layout_for_instance(def_id, &[], span);
        }
    }

    fn struct_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<StructLayout> {
        let Some(struct_def) = self.struct_defs.get(&def_id).cloned() else {
            self.emit_error(span, "struct definition not registered");
            return None;
        };

        let key = StructLayoutKey {
            def_id,
            args: args.to_vec(),
        };

        if let Some(layout) = self.struct_layouts.get(&key) {
            return Some(layout.clone());
        }

        if struct_def.generics.len() != args.len() {
            self.emit_error(
                span,
                format!(
                    "struct `{}` expects {} generic arguments, got {}",
                    struct_def.name,
                    struct_def.generics.len(),
                    args.len()
                ),
            );
            return None;
        }

        let mut substs = HashMap::new();
        for (name, ty) in struct_def.generics.iter().zip(args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }

        let mut field_tys = Vec::with_capacity(struct_def.fields.len());
        for field in &struct_def.fields {
            field_tys.push(self.lower_type_expr_with_substs(&field.ty, &substs));
        }

        let struct_ty = Ty {
            kind: TyKind::Tuple(field_tys.iter().cloned().map(Box::new).collect()),
        };

        let layout = StructLayout {
            ty: struct_ty.clone(),
            field_tys,
        };

        self.struct_layouts.insert(key.clone(), layout.clone());
        self.struct_layouts_by_ty.insert(struct_ty, key);

        Some(layout)
    }

    fn struct_layout_for_ty(&self, ty: &Ty) -> Option<StructLayout> {
        let key = self.struct_layouts_by_ty.get(ty)?;
        self.struct_layouts.get(key).cloned()
    }

    fn enum_payload_types(
        &mut self,
        payload: &Option<hir::TypeExpr>,
        substs: &HashMap<String, Ty>,
    ) -> Vec<Ty> {
        let Some(payload) = payload else {
            return Vec::new();
        };
        let payload_ty = self.lower_type_expr_with_substs(payload, substs);
        self.enum_payload_types_from_ty(&payload_ty)
    }

    fn enum_payload_types_from_ty(&self, ty: &Ty) -> Vec<Ty> {
        match &ty.kind {
            TyKind::Tuple(fields) => fields.iter().map(|f| (**f).clone()).collect(),
            _ if Self::is_unit_ty(ty) => Vec::new(),
            _ => vec![ty.clone()],
        }
    }

    fn enum_layout_for_instance(
        &mut self,
        def_id: hir::DefId,
        args: &[Ty],
        span: Span,
    ) -> Option<EnumLayout> {
        let Some(enum_def) = self.enum_defs.get(&def_id).cloned() else {
            self.emit_error(span, "enum definition not registered");
            return None;
        };

        let key = EnumLayoutKey {
            def_id,
            args: args.to_vec(),
        };

        if let Some(layout) = self.enum_layouts.get(&key) {
            return Some(layout.clone());
        }

        if enum_def.generics.len() != args.len() {
            self.emit_error(
                span,
                format!(
                    "enum `{}` expects {} generic arguments, got {}",
                    enum_def.name,
                    enum_def.generics.len(),
                    args.len()
                ),
            );
            return None;
        }

        let mut substs = HashMap::new();
        for (name, ty) in enum_def.generics.iter().zip(args.iter().cloned()) {
            substs.insert(name.clone(), ty);
        }

        let tag_ty = Ty {
            kind: TyKind::Int(IntTy::Isize),
        };
        let mut payload_layout: Vec<Ty> = Vec::new();
        let mut variant_payloads = HashMap::new();
        let mut has_payload = false;

        for variant in &enum_def.variants {
            let payload_tys = self.enum_payload_types(&variant.payload, &substs);
            if !payload_tys.is_empty() {
                has_payload = true;
            }
            for (idx, ty) in payload_tys.iter().enumerate() {
                if let Some(existing) = payload_layout.get(idx) {
                    if existing != ty {
                        self.emit_error(
                            span,
                            format!(
                                "enum '{}' has incompatible payload types at index {}",
                                enum_def.name, idx
                            ),
                        );
                    }
                } else {
                    payload_layout.push(ty.clone());
                }
            }
            variant_payloads.insert(variant.def_id, payload_tys);
        }

        let enum_ty = if has_payload {
            let mut fields = Vec::with_capacity(1 + payload_layout.len());
            fields.push(Box::new(tag_ty.clone()));
            fields.extend(payload_layout.iter().cloned().map(Box::new));
            Ty {
                kind: TyKind::Tuple(fields),
            }
        } else {
            tag_ty.clone()
        };

        let layout = EnumLayout {
            tag_ty: tag_ty.clone(),
            payload_tys: payload_layout.clone(),
            enum_ty: enum_ty.clone(),
            variant_payloads,
        };

        self.enum_layouts.insert(key, layout.clone());

        if !has_payload {
            for variant in &enum_def.variants {
                if self.const_values.contains_key(&variant.def_id) {
                    continue;
                }
                let constant = mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Int(variant.discriminant),
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

        Some(layout)
    }

    fn lower_generic_args(&mut self, args: Option<&hir::GenericArgs>, span: Span) -> Vec<Ty> {
        let Some(args) = args else {
            return Vec::new();
        };
        let mut lowered = Vec::new();
        for arg in &args.args {
            match arg {
                hir::GenericArg::Type(ty) => lowered.push(self.lower_type_expr(ty)),
                hir::GenericArg::Const(_) => {
                    self.emit_warning(
                        span,
                        "const generics are ignored during MIR lowering",
                    );
                }
            }
        }
        lowered
    }

    fn lower_type_expr_with_substs(
        &mut self,
        ty_expr: &hir::TypeExpr,
        substs: &HashMap<String, Ty>,
    ) -> Ty {
        match &ty_expr.kind {
            hir::TypeExprKind::Primitive(primitive) => {
                self.lower_primitive_type(primitive, ty_expr.span)
            }
            hir::TypeExprKind::Tuple(elements) => Ty {
                kind: TyKind::Tuple(
                    elements
                        .iter()
                        .map(|elem| Box::new(self.lower_type_expr_with_substs(elem, substs)))
                        .collect(),
                ),
            },
            hir::TypeExprKind::Array(elem, len_expr) => {
                let elem_ty = self.lower_type_expr_with_substs(elem, substs);
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
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr_with_substs(elem, substs);
                Ty {
                    kind: TyKind::Slice(Box::new(elem_ty)),
                }
            }
            hir::TypeExprKind::Ref(inner) => {
                let inner_ty = self.lower_type_expr_with_substs(inner, substs);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Not,
                    ),
                }
            }
            hir::TypeExprKind::Ptr(inner) => {
                let inner_ty = self.lower_type_expr_with_substs(inner, substs);
                Ty {
                    kind: TyKind::Ref(
                        mir::ty::Region::ReErased,
                        Box::new(inner_ty),
                        Mutability::Mut,
                    ),
                }
            }
            hir::TypeExprKind::Path(path) => {
                if let Some(first) = path.segments.first() {
                    if path.segments.len() == 1 && first.args.is_none() {
                        if let Some(subst) = substs.get(first.name.as_str()) {
                            return subst.clone();
                        }
                    }
                }

                if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                    if self.enum_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| {
                                args.args
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        hir::GenericArg::Type(ty) => {
                                            Some(self.lower_type_expr_with_substs(ty, substs))
                                        }
                                        hir::GenericArg::Const(_) => {
                                            self.emit_warning(
                                                ty_expr.span,
                                                "const generics are ignored during MIR lowering",
                                            );
                                            None
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        if let Some(layout) =
                            self.enum_layout_for_instance(*def_id, &args, ty_expr.span)
                        {
                            return layout.enum_ty.clone();
                        }
                        return self.error_ty();
                    }
                    if self.struct_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| {
                                args.args
                                    .iter()
                                    .filter_map(|arg| match arg {
                                        hir::GenericArg::Type(ty) => {
                                            Some(self.lower_type_expr_with_substs(ty, substs))
                                        }
                                        hir::GenericArg::Const(_) => {
                                            self.emit_warning(
                                                ty_expr.span,
                                                "const generics are ignored during MIR lowering",
                                            );
                                            None
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        if let Some(layout) =
                            self.struct_layout_for_instance(*def_id, &args, ty_expr.span)
                        {
                            return layout.ty.clone();
                        }
                        return self.error_ty();
                    }
                }

                self.lower_path_type(path, ty_expr.span)
            }
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                let inputs = fn_ptr
                    .inputs
                    .iter()
                    .map(|ty| Box::new(self.lower_type_expr_with_substs(ty, substs)))
                    .collect();
                let output = Box::new(self.lower_type_expr_with_substs(&fn_ptr.output, substs));
                Ty {
                    kind: TyKind::FnPtr(mir::ty::PolyFnSig {
                        binder: mir::ty::Binder {
                            value: mir::ty::FnSig {
                                inputs,
                                output,
                                c_variadic: false,
                                unsafety: mir::ty::Unsafety::Normal,
                                abi: mir::ty::Abi::C { unwind: false },
                            },
                            bound_vars: Vec::new(),
                        },
                    }),
                }
            }
            hir::TypeExprKind::Never => Ty { kind: TyKind::Never },
            hir::TypeExprKind::Infer => self.error_ty(),
            hir::TypeExprKind::Error => self.error_ty(),
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
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| String::from(seg.name.clone())),
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
        let impl_is_generic = !impl_block.generics.params.is_empty();

        for impl_item in &impl_block.items {
            match &impl_item.kind {
                hir::ImplItemKind::Method(function) => {
                    if impl_is_generic || !function.sig.generics.params.is_empty() {
                        let qualified_name = format!("{}::{}", struct_name, function.sig.name);
                        let def = MethodDefinition {
                            function: function.clone(),
                            impl_generics: impl_block.generics.clone(),
                            self_ty: impl_block.self_ty.clone(),
                            self_def: method_context.as_ref().and_then(|ctx| ctx.def_id),
                            method_name: qualified_name.clone(),
                        };
                        self.method_defs.insert(qualified_name, def);
                        continue;
                    }

                    let (mir_item, body_id, body, sig) =
                        self.lower_method(program, function, item.span, method_context.as_ref())?;
                    emit_function(self, mir_item, body_id, body);

                    let struct_prefix = method_context
                        .as_ref()
                        .and_then(|ctx| {
                            if ctx.path.is_empty() {
                                None
                            } else {
                                Some(
                                    ctx.path
                                        .iter()
                                        .map(|seg| seg.name.as_str())
                                        .collect::<Vec<_>>()
                                        .join("::"),
                                )
                            }
                        })
                        .unwrap_or_else(|| struct_name.clone());
                    let fn_name = format!("{}::{}", struct_prefix, function.sig.name.as_str());
                    let fn_ty = self.function_pointer_ty(&sig);
                    let struct_def = method_context.as_ref().and_then(|ctx| ctx.def_id);
                    let info = MethodLoweringInfo {
                        sig: sig.clone(),
                        fn_name: fn_name.clone(),
                        fn_ty: fn_ty.clone(),
                        struct_def,
                    };

                    self.method_lookup
                        .insert(fn_name.clone(), info.clone());
                    self.method_lookup
                        .insert(format!("{}::{}", struct_name, impl_item.name), info.clone());
                    self.struct_methods
                        .entry(struct_name.clone())
                        .or_default()
                        .insert(String::from(impl_item.name.clone()), info);
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
        let mir_body = BodyBuilder::new(
            self,
            program,
            function,
            &sig,
            span,
            method_context.cloned(),
            HashMap::new(),
        )
        .lower()?;

        let method_name = function.sig.name.as_str();
        let qualified_name = match method_context {
            Some(ctx) if !ctx.path.is_empty() => {
                let path = ctx
                    .path
                    .iter()
                    .map(|seg| seg.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::");
                format!("{}::{}", path, method_name)
            }
            _ => method_name.to_string(),
        };

        let mir_function = mir::Function {
            name: mir::Symbol::new(qualified_name),
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

    fn struct_field(
        &self,
        def_id: hir::DefId,
        struct_ty: &Ty,
        name: &str,
    ) -> Option<(usize, StructFieldInfo)> {
        let def = self.struct_defs.get(&def_id)?;
        let idx = *def.field_index.get(name)?;
        let layout = self.struct_layout_for_ty(struct_ty)?;
        let ty = layout.field_tys.get(idx)?.clone();
        Some((
            idx,
            StructFieldInfo {
                name: name.to_string(),
                ty,
            },
        ))
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
            hir::Lit::Null => mir::ConstantKind::Null,
        }
    }

    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        if self.tolerate_errors {
            let diagnostic = Diagnostic::warning(message.into())
                .with_source_context(DIAGNOSTIC_CONTEXT)
                .with_span(span);
            self.diagnostics.push(diagnostic);
            return;
        }
        self.has_errors = true;
        let diagnostic = Diagnostic::error(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn emit_warning(&mut self, span: Span, message: impl Into<String>) {
        let diagnostic = Diagnostic::warning(message.into())
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(span);
        self.diagnostics.push(diagnostic);
    }

    fn unit_ty() -> Ty {
        Ty {
            kind: TyKind::Tuple(Vec::new()),
        }
    }

    fn is_unit_ty(ty: &Ty) -> bool {
        matches!(&ty.kind, TyKind::Tuple(elements) if elements.is_empty())
    }

    fn pointer_sized_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Uint(UintTy::U8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    fn sanitize_placeholder_ty(&self, ty: &Ty) -> Ty {
        match &ty.kind {
            TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::RawPtr(_)
            | TyKind::Ref(_, _, _)
            | TyKind::FnPtr(_) => ty.clone(),
            _ => self.pointer_sized_ty(),
        }
    }

    fn sanitize_function_sig(&self, sig: &mir::FunctionSig) -> mir::FunctionSig {
        let inputs = sig
            .inputs
            .iter()
            .map(|ty| self.sanitize_placeholder_ty(ty))
            .collect();
        let output = if Self::is_unit_ty(&sig.output) {
            sig.output.clone()
        } else {
            self.sanitize_placeholder_ty(&sig.output)
        };
        mir::FunctionSig { inputs, output }
    }

    fn update_placeholder_signature(
        &mut self,
        name: &str,
        existing_sig: &mir::FunctionSig,
        arg_types: &[Ty],
        destination_ty: Option<&Ty>,
    ) -> mir::FunctionSig {
        let mut inputs: Vec<Ty> = if arg_types.is_empty() {
            existing_sig
                .inputs
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect()
        } else {
            arg_types
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect()
        };

        let mut output = if let Some(expected) = destination_ty {
            self.sanitize_placeholder_ty(expected)
        } else if Self::is_unit_ty(&existing_sig.output) {
            existing_sig.output.clone()
        } else {
            self.sanitize_placeholder_ty(&existing_sig.output)
        };

        if inputs.is_empty() && arg_types.is_empty() && !existing_sig.inputs.is_empty() {
            inputs = existing_sig
                .inputs
                .iter()
                .map(|ty| self.sanitize_placeholder_ty(ty))
                .collect();
        }

        if destination_ty.is_none() && Self::is_unit_ty(&existing_sig.output) {
            output = existing_sig.output.clone();
        }

        let new_sig = mir::FunctionSig { inputs, output };

        let needs_update = self
            .runtime_functions
            .get(name)
            .map(|current| current != &new_sig)
            .unwrap_or(true);

        if needs_update {
            self.runtime_functions
                .insert(name.to_string(), new_sig.clone());
            self.synthetic_runtime_functions.insert(name.to_string());
            new_sig
        } else {
            existing_sig.clone()
        }
    }

    fn opaque_ty(&mut self, name: &str) -> Ty {
        if let Some(existing) = self.opaque_types.get(name) {
            return existing.clone();
        }
        let adt_def_id = self.next_synthetic_def_id;
        self.next_synthetic_def_id = self.next_synthetic_def_id.saturating_add(1);
        let variant_def_id = self.next_synthetic_def_id;
        self.next_synthetic_def_id = self.next_synthetic_def_id.saturating_add(1);

        let symbol = Symbol::new(name);
        let variant = VariantDef {
            def_id: variant_def_id,
            ctor_def_id: None,
            ident: symbol.clone(),
            discr: VariantDiscr::Relative(0),
            fields: Vec::new(),
            ctor_kind: CtorKind::Const,
            is_recovered: false,
        };

        let adt = AdtDef {
            did: adt_def_id,
            variants: vec![variant],
            flags: AdtFlags::IS_STRUCT,
            repr: ReprOptions {
                int: None,
                align: None,
                pack: None,
                flags: ReprFlags::empty(),
                field_shuffle_seed: 0,
            },
        };

        let ty = Ty {
            kind: TyKind::Adt(adt, Vec::new()),
        };
        self.opaque_types.insert(name.to_string(), ty.clone());
        ty
    }

    fn display_type_name(&self, ty: &Ty) -> Option<String> {
        match &ty.kind {
            TyKind::Adt(adt, _) => adt
                .variants
                .first()
                .map(|variant| variant.ident.as_str().to_string()),
            TyKind::Ref(_, inner, _) => self.display_type_name(inner),
            TyKind::RawPtr(type_and_mut) => self.display_type_name(&type_and_mut.ty),
            _ => None,
        }
    }

    fn ensure_runtime_stub(&mut self, name: &str, sig: &mir::FunctionSig) {
        let sanitized = self.sanitize_function_sig(sig);
        self.runtime_functions.insert(name.to_string(), sanitized);
        self.synthetic_runtime_functions.insert(name.to_string());
    }

    fn placeholder_function_sig(&mut self, name: &str) -> mir::FunctionSig {
        let entry = self
            .runtime_functions
            .entry(name.to_string())
            .or_insert_with(|| mir::FunctionSig {
                inputs: Vec::new(),
                output: Self::unit_ty(),
            })
            .clone();
        self.synthetic_runtime_functions.insert(name.to_string());
        entry
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

    fn enum_layout_for_def(&mut self, def_id: hir::DefId, span: Span) -> Option<EnumLayout> {
        let Some(definition) = self.enum_defs.get(&def_id) else {
            return None;
        };
        if !definition.generics.is_empty() {
            // Generic enum layouts must be resolved through a concrete type instance.
            return None;
        }
        self.enum_layout_for_instance(def_id, &[], span)
    }

    fn enum_layout_for_ty(&self, ty: &Ty) -> Option<&EnumLayout> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_layout_for_ty(inner),
            TyKind::RawPtr(type_and_mut) => self.enum_layout_for_ty(&type_and_mut.ty),
            _ => self
                .enum_layouts
                .values()
                .find(|layout| layout.enum_ty == *ty),
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
    fallback_locals: HashMap<String, mir::LocalId>,
    local_structs: HashMap<mir::LocalId, hir::DefId>,
    const_items: HashMap<hir::DefId, hir::Const>,
    blocks: Vec<mir::BasicBlockData>,
    current_block: mir::BasicBlockId,
    span: Span,
    method_context: Option<MethodContext>,
    type_substs: HashMap<String, Ty>,
    loop_stack: Vec<LoopContext>,
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

struct StructRef {
    def_id: hir::DefId,
    args: Vec<Ty>,
}

impl OperandInfo {
    fn constant(span: Span, ty: Ty, literal: mir::ConstantKind) -> Self {
        Self {
            operand: mir::Operand::Constant(mir::Constant {
                span,
                user_ty: None,
                literal,
            }),
            ty,
        }
    }
}

#[derive(Clone)]
struct LoopDestination {
    place: mir::Place,
    ty: Ty,
}

#[derive(Clone)]
struct LoopContext {
    break_block: mir::BasicBlockId,
    continue_block: mir::BasicBlockId,
    break_destination: Option<LoopDestination>,
    break_value_allowed: bool,
}

impl<'a> BodyBuilder<'a> {
    fn new(
        lowering: &'a mut MirLowering,
        program: &'a hir::Program,
        function: &'a hir::Function,
        sig: &'a mir::FunctionSig,
        span: Span,
        method_context: Option<MethodContext>,
        type_substs: HashMap<String, Ty>,
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
            fallback_locals: HashMap::new(),
            local_structs: HashMap::new(),
            const_items: HashMap::new(),
            blocks: vec![mir::BasicBlockData::new(None)],
            current_block: 0,
            span,
            method_context,
            type_substs,
            loop_stack: Vec::new(),
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

    fn lower_type_expr(&mut self, ty_expr: &hir::TypeExpr) -> Ty {
        if let Some(ctx) = self.method_context.as_ref() {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if path.segments.first().map(|seg| seg.name.as_str()) == Some("Self") {
                    return ctx.mir_self_ty.clone();
                }
            }
        }
        // NOTE(jakku): This is the key hook for generic lowering. When
        // type_substs is populated, we substitute generic params so MIR
        // sees concrete types. Otherwise we fall back to the existing
        // lowering (which treats unknown generics as opaque).
        if self.type_substs.is_empty() {
            return self.lowering.lower_type_expr(ty_expr);
        }
        self.lowering
            .lower_type_expr_with_substs(ty_expr, &self.type_substs)
    }

    fn bind_pattern(&mut self, pat: &hir::Pat, local: mir::LocalId, ty: Option<&Ty>) {
        match &pat.kind {
            hir::PatKind::Binding { name, mutable } => {
                self.local_map.insert(pat.hir_id, local);
                self.fallback_locals
                    .insert(name.as_str().to_string(), local);
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
                .struct_layouts_by_ty
                .get(ty)
                .map(|key| key.def_id),
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

        let expected_return_ty = self.sig.output.clone();
        if self.locals[0].ty != expected_return_ty {
            let fallback = self.fallback_operand_for_reference(&expected_return_ty, self.span);
            if let Some(block) = self.blocks.last_mut() {
                block.statements.push(mir::Statement {
                    source_info: self.span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(0),
                        mir::Rvalue::Use(fallback.operand),
                    ),
                });
            }
            self.locals[0].ty = expected_return_ty;
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

    fn lower_loop_expr(
        &mut self,
        span: Span,
        block: &hir::Block,
        destination: Option<LoopDestination>,
        break_value_allowed: bool,
    ) -> Result<()> {
        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_header = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: header_block,
            },
        };
        self.set_current_terminator(goto_header);

        self.current_block = header_block;
        let goto_body = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: body_block },
        };
        self.set_current_terminator(goto_body);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: header_block,
            break_destination: context_destination,
            break_value_allowed,
        });

        self.current_block = body_block;
        self.lower_block(block)?;

        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: header_block,
                },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        Ok(())
    }

    fn lower_while_expr(
        &mut self,
        span: Span,
        cond: &hir::Expr,
        block: &hir::Block,
        destination: Option<LoopDestination>,
    ) -> Result<()> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        let goto_cond = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto { target: cond_block },
        };
        self.set_current_terminator(goto_cond);

        self.current_block = cond_block;
        let bool_ty = Ty { kind: TyKind::Bool };
        let cond_operand = self.lower_operand(cond, Some(&bool_ty))?;
        let switch = mir::Terminator {
            source_info: cond.span,
            kind: mir::TerminatorKind::SwitchInt {
                discr: cond_operand.operand,
                switch_ty: bool_ty.clone(),
                targets: mir::SwitchTargets {
                    values: vec![1],
                    targets: vec![body_block],
                    otherwise: exit_block,
                },
            },
        };
        self.set_current_terminator(switch);

        let context_destination = destination.clone();
        self.loop_stack.push(LoopContext {
            break_block: exit_block,
            continue_block: cond_block,
            break_destination: context_destination,
            break_value_allowed: false,
        });

        self.current_block = body_block;
        self.lower_block(block)?;
        if self.blocks[self.current_block as usize]
            .terminator
            .is_none()
        {
            let goto = mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto { target: cond_block },
            };
            self.set_current_terminator(goto);
        }

        self.loop_stack.pop();
        self.current_block = exit_block;

        if let Some(dest) = destination.as_ref() {
            let assign_unit = mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    dest.place.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            };
            self.push_statement(assign_unit);
            if dest.place.projection.is_empty() {
                self.locals[dest.place.local as usize].ty = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
            }
        }

        Ok(())
    }

    fn lower_break(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`break` used outside of a loop");
                return Ok(());
            }
        };

        if let Some(value_expr) = value {
            if !context.break_value_allowed {
                self.lowering.emit_error(
                    span,
                    "`break` with a value is only supported inside `loop` expressions",
                );
            } else if let Some(dest) = context.break_destination.as_ref() {
                let expected = match &dest.ty.kind {
                    TyKind::Tuple(elements) if elements.is_empty() => None,
                    TyKind::Error(_) => None,
                    _ => Some(&dest.ty),
                };
                let operand = self.lower_operand(value_expr, expected)?;
                let statement = mir::Statement {
                    source_info: value_expr.span,
                    kind: mir::StatementKind::Assign(
                        dest.place.clone(),
                        mir::Rvalue::Use(operand.operand),
                    ),
                };
                self.push_statement(statement);
                if dest.place.projection.is_empty() {
                    self.locals[dest.place.local as usize].ty = operand.ty.clone();
                    if let Some(struct_def) = self.struct_def_from_ty(&operand.ty) {
                        self.local_structs.insert(dest.place.local, struct_def);
                    }
                }
            } else {
                self.lowering.emit_error(
                    span,
                    "`break` with a value requires the surrounding loop to produce a value",
                );
            }
        } else if context.break_value_allowed {
            if let Some(dest) = context.break_destination.as_ref() {
                match &dest.ty.kind {
                    TyKind::Tuple(elements) if elements.is_empty() => {}
                    TyKind::Never => {}
                    _ => {
                        self.lowering.emit_error(
                            span,
                            "`break` without a value in a value-producing loop is not supported",
                        );
                    }
                }
            }
        }

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.break_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
        Ok(())
    }

    fn lower_continue(&mut self, span: Span) -> Result<()> {
        let context = match self.loop_stack.last() {
            Some(ctx) => ctx.clone(),
            None => {
                self.lowering
                    .emit_error(span, "`continue` used outside of a loop");
                return Ok(());
            }
        };

        let goto = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Goto {
                target: context.continue_block,
            },
        };
        self.set_current_terminator(goto);
        self.current_block = self.new_block();
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

    fn lower_match_expr(
        &mut self,
        span: Span,
        scrutinee: &hir::Expr,
        arms: &[hir::MatchArm],
        destination: mir::Place,
        expected_ty: &Ty,
    ) -> Result<()> {
        let scrutinee_info = self.lower_operand(scrutinee, None)?;
        let scrutinee_local = self.allocate_temp(scrutinee_info.ty.clone(), scrutinee.span);
        let scrutinee_place = mir::Place::from_local(scrutinee_local);
        self.push_statement(mir::Statement {
            source_info: scrutinee.span,
            kind: mir::StatementKind::Assign(
                scrutinee_place.clone(),
                mir::Rvalue::Use(scrutinee_info.operand),
            ),
        });

        let continue_block = self.new_block();
        let mut next_block = self.current_block;
        let mut fallthrough_block = None;

        for (idx, arm) in arms.iter().enumerate() {
            let body_block = self.new_block();
            let is_last = idx == arms.len() - 1;
            let mut next_arm_block = self.new_block();
            let always_matches = self.pattern_always_matches(&arm.pat);
            if is_last && always_matches {
                next_arm_block = continue_block;
            } else if is_last {
                fallthrough_block = Some(next_arm_block);
            }

            self.current_block = next_block;
            if always_matches {
                self.set_current_terminator(mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::Goto { target: body_block },
                });
            } else {
                let cond_operand = self.lower_match_condition(
                    &arm.pat,
                    &scrutinee_place,
                    &scrutinee_info.ty,
                    span,
                )?;
                let switch = mir::Terminator {
                    source_info: span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: cond_operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![body_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(switch);
            }

            self.current_block = body_block;
            let saved_locals = self.local_map.clone();
            let saved_fallback = self.fallback_locals.clone();
            self.bind_match_pattern(&arm.pat, &scrutinee_place, &scrutinee_info.ty, span);

            let mut guard_body_block = body_block;
            if let Some(guard) = &arm.guard {
                let guard_operand = self.lower_operand(guard, Some(&Ty { kind: TyKind::Bool }))?;
                let guard_block = self.new_block();
                let guard_switch = mir::Terminator {
                    source_info: guard.span,
                    kind: mir::TerminatorKind::SwitchInt {
                        discr: guard_operand.operand,
                        switch_ty: Ty { kind: TyKind::Bool },
                        targets: mir::SwitchTargets {
                            values: vec![1],
                            targets: vec![guard_block],
                            otherwise: next_arm_block,
                        },
                    },
                };
                self.set_current_terminator(guard_switch);
                guard_body_block = guard_block;
                self.current_block = guard_body_block;
            }

            self.lower_expr_into_place(&arm.body, destination.clone(), expected_ty)?;
            self.set_current_terminator(mir::Terminator {
                source_info: arm.body.span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
            self.local_map = saved_locals;
            self.fallback_locals = saved_fallback;

            next_block = next_arm_block;
        }

        if let Some(fallthrough) = fallthrough_block {
            self.current_block = fallthrough;
            self.lowering
                .emit_warning(span, "match arms did not cover all cases");
            self.push_statement(mir::Statement {
                source_info: span,
                kind: mir::StatementKind::Assign(
                    destination.clone(),
                    mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                ),
            });
            self.set_current_terminator(mir::Terminator {
                source_info: span,
                kind: mir::TerminatorKind::Goto {
                    target: continue_block,
                },
            });
        }

        self.current_block = continue_block;
        Ok(())
    }

    fn pattern_always_matches(&self, pat: &hir::Pat) -> bool {
        matches!(pat.kind, hir::PatKind::Wild | hir::PatKind::Binding { .. })
    }

    fn lower_match_condition(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) -> Result<mir::Operand> {
        let literal = match &pat.kind {
            hir::PatKind::Lit(lit) => {
                let (literal, _) = self.lower_literal(lit, None);
                mir::Operand::Constant(mir::Constant {
                    span,
                    user_ty: None,
                    literal,
                })
            }
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => {
                if let Some(variant) = self.enum_variant_info_from_path(path) {
                    mir::Operand::Constant(mir::Constant {
                        span,
                        user_ty: None,
                        literal: mir::ConstantKind::Int(variant.discriminant),
                    })
                } else {
                    let expr = hir::Expr {
                        hir_id: 0,
                        kind: hir::ExprKind::Path(path.clone()),
                        span,
                    };
                    let operand = self.lower_operand(&expr, None)?;
                    operand.operand
                }
            }
            _ => {
                self.lowering.emit_error(span, "unsupported pattern in match condition");
                mir::Operand::Constant(self.lowering.error_constant(span))
            }
        };

        let scrutinee_operand = if matches!(
            pat.kind,
            hir::PatKind::Variant(_)
                | hir::PatKind::Struct(_, _, _)
                | hir::PatKind::TupleStruct(_, _)
        ) {
            let layout = match &pat.kind {
                hir::PatKind::Variant(path)
                | hir::PatKind::Struct(path, _, _)
                | hir::PatKind::TupleStruct(path, _) => self
                    .enum_variant_info_from_path(path)
                    // Prefer the enum definition for this variant to avoid matching on a
                    // structurally identical enum layout from another type.
                    .and_then(|variant| self.lowering.enum_layout_for_def(variant.enum_def, span))
                    .or_else(|| self.lowering.enum_layout_for_ty(scrutinee_ty).cloned()),
                _ => self.lowering.enum_layout_for_ty(scrutinee_ty).cloned(),
            };
            if let Some(layout) = layout {
                let mut tag_place = scrutinee_place.clone();
                if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                    tag_place.projection.push(mir::PlaceElem::Deref);
                }
                tag_place
                    .projection
                    .push(mir::PlaceElem::Field(0, layout.tag_ty.clone()));
                mir::Operand::Copy(tag_place)
            } else {
                mir::Operand::Copy(scrutinee_place.clone())
            }
        } else {
            mir::Operand::Copy(scrutinee_place.clone())
        };

        let temp = self.allocate_temp(Ty { kind: TyKind::Bool }, span);
        let place = mir::Place::from_local(temp);
        let assign = mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::BinaryOp(
                    mir::BinOp::Eq,
                    scrutinee_operand,
                    literal,
                ),
            ),
        };
        self.push_statement(assign);
        Ok(mir::Operand::Copy(place))
    }

    fn bind_match_pattern(
        &mut self,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        let layout = match &pat.kind {
            hir::PatKind::Variant(path)
            | hir::PatKind::Struct(path, _, _)
            | hir::PatKind::TupleStruct(path, _) => self
                .enum_variant_info_from_path(path)
                .and_then(|variant| self.lowering.enum_layout_for_def(variant.enum_def, span))
                .or_else(|| self.lowering.enum_layout_for_ty(scrutinee_ty).cloned()),
            _ => self.lowering.enum_layout_for_ty(scrutinee_ty).cloned(),
        };
        if let Some(layout) = layout {
            let mut scrutinee_place = scrutinee_place.clone();
            if matches!(scrutinee_ty.kind, TyKind::Ref(_, _, _) | TyKind::RawPtr(_)) {
                scrutinee_place.projection.push(mir::PlaceElem::Deref);
            }
            match &pat.kind {
                hir::PatKind::Variant(path) => {
                    if self.enum_variant_info_from_path(path).is_some() {
                        return;
                    }
                }
                hir::PatKind::TupleStruct(path, parts) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = layout
                            .variant_payloads
                            .get(&variant.def_id)
                            .cloned()
                            .unwrap_or_else(|| {
                                self.lowering.emit_error(
                                    span,
                                    "enum variant payload layout not registered",
                                );
                                Vec::new()
                            });
                        for (idx, part) in parts.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place.projection.push(mir::PlaceElem::Field(
                                idx + 1,
                                field_ty.clone(),
                            ));
                            self.bind_match_pattern(part, &field_place, &field_ty, span);
                        }
                        return;
                    }
                }
                hir::PatKind::Struct(path, fields, _) => {
                    if let Some(variant) = self.enum_variant_info_from_path(path) {
                        let payload_tys = layout
                            .variant_payloads
                            .get(&variant.def_id)
                            .cloned()
                            .unwrap_or_else(|| {
                                self.lowering.emit_error(
                                    span,
                                    "enum variant payload layout not registered",
                                );
                                Vec::new()
                            });
                        for (idx, field) in fields.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place.projection.push(mir::PlaceElem::Field(
                                idx + 1,
                                field_ty.clone(),
                            ));
                            self.bind_match_pattern(&field.pat, &field_place, &field_ty, span);
                        }
                        return;
                    }
                }
                _ => {}
            }
        }
        match &pat.kind {
            hir::PatKind::Binding { name, .. } => {
                self.bind_match_binding(name, pat, scrutinee_place, scrutinee_ty, span);
            }
            hir::PatKind::Tuple(parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::TupleStruct(_, parts) => {
                for part in parts {
                    self.bind_match_pattern(part, scrutinee_place, scrutinee_ty, span);
                }
            }
            hir::PatKind::Struct(_, fields, _) => {
                for field in fields {
                    self.bind_match_pattern(&field.pat, scrutinee_place, scrutinee_ty, span);
                }
            }
            _ => {}
        }
    }

    fn bind_match_binding(
        &mut self,
        name: &hir::Symbol,
        pat: &hir::Pat,
        scrutinee_place: &mir::Place,
        scrutinee_ty: &Ty,
        span: Span,
    ) {
        let mut decl = self.lowering.make_local_decl(scrutinee_ty, span);
        decl.mutability = mir::Mutability::Not;
        let local_id = self.push_local(decl);
        let place = mir::Place::from_local(local_id);
        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place.clone(),
                mir::Rvalue::Use(mir::Operand::Copy(scrutinee_place.clone())),
            ),
        });
        self.local_map.insert(pat.hir_id, local_id);
        self.fallback_locals
            .insert(name.as_str().to_string(), local_id);
        if let Some(def_id) = self.struct_def_from_ty(scrutinee_ty) {
            self.local_structs.insert(local_id, def_id);
        }
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
            .map(|ty_expr| self.lower_type_expr(ty_expr));

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
                self.lowering.register_struct(item.def_id, def, item.span);
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
            hir::ItemKind::Function(function) => {
                let (mir_item, body_id, body) =
                    self.lowering.lower_function(self.program, item, function)?;
                self.lowering.extra_items.push(mir_item);
                self.lowering.extra_bodies.push((body_id, body));
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
            hir::ExprKind::Loop(block) => {
                let temp_unit = Ty {
                    kind: TyKind::Tuple(Vec::new()),
                };
                let temp_local = self.allocate_temp(temp_unit.clone(), expr.span);
                let destination = LoopDestination {
                    place: mir::Place::from_local(temp_local),
                    ty: temp_unit,
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::While(cond, block) => {
                self.lower_while_expr(expr.span, cond, block, None)?;
            }
            hir::ExprKind::Break(value) => {
                self.lower_break(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Continue => {
                self.lower_continue(expr.span)?;
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

    fn enum_variant_info_from_path(&self, path: &hir::Path) -> Option<EnumVariantInfo> {
        if let Some(hir::Res::Def(def_id)) = &path.res {
            if let Some(info) = self.lowering.enum_variants.get(def_id) {
                return Some(info.clone());
            }
        }

        let name = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        self.lowering
            .enum_variant_names
            .get(&name)
            .and_then(|def_id| self.lowering.enum_variants.get(def_id))
            .cloned()
    }

    fn assign_enum_variant(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        args: &[hir::Expr],
        span: Span,
    ) -> Result<()> {
        let payload_tys = layout
            .variant_payloads
            .get(&variant.def_id)
            .cloned()
            .unwrap_or_else(|| {
                self.lowering.emit_error(
                    span,
                    "enum variant payload layout not registered",
                );
                Vec::new()
            });

        if args.len() != payload_tys.len() {
            self.lowering.emit_error(
                span,
                format!(
                    "enum variant expected {} payload values, got {}",
                    payload_tys.len(),
                    args.len()
                ),
            );
        }

        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Int(variant.discriminant),
        }));

        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
            if let Some(arg) = args.get(idx) {
                let expected_ty = payload_tys.get(idx).unwrap_or(slot_ty);
                let operand = self.lower_operand(arg, Some(expected_ty))?;
                operands.push(operand.operand);
            } else {
                operands.push(mir::Operand::Constant(mir::Constant {
                    span,
                    user_ty: None,
                    literal: self.lowering.default_constant_for_ty(slot_ty),
                }));
            }
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::Assign(
                place,
                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
            ),
        });
        Ok(())
    }

    fn lower_enum_variant_value(
        &mut self,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        args: &[hir::Expr],
        span: Span,
    ) -> Result<OperandInfo> {
        let local_id = self.allocate_temp(layout.enum_ty.clone(), span);
        let place = mir::Place::from_local(local_id);
        self.assign_enum_variant(place.clone(), variant, layout, args, span)?;
        Ok(OperandInfo {
            operand: mir::Operand::copy(place),
            ty: layout.enum_ty.clone(),
        })
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
        let generic_args = resolved_path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| self.lowering.lower_generic_args(Some(args), span))
            .unwrap_or_default();
        let def_id = match &resolved_path.res {
            Some(hir::Res::Def(def_id)) => Some(*def_id),
            _ => None,
        };

        if let (Some(expected_ty), Some(variant)) = (
            annotated_ty,
            self.enum_variant_info_from_path(&resolved_path),
        ) {
            if let Some(layout) = self
                .lowering
                .enum_layout_for_def(variant.enum_def, span)
            {
                if layout.enum_ty == *expected_ty {
                    let payload_args: Vec<hir::Expr> =
                        fields.iter().map(|field| field.expr.clone()).collect();
                    self.assign_enum_variant(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        &payload_args,
                        span,
                    )?;
                    self.locals[local_id as usize].ty = layout.enum_ty.clone();
                    return Ok(());
                }
            }
        }

        if let Some(def_id) = def_id {
            if let Some(info) = self.lowering.struct_defs.get(&def_id).cloned() {
                if let Some(layout) =
                    self.lowering
                        .struct_layout_for_instance(def_id, &generic_args, span)
                {
                    return self.lower_registered_struct_literal(
                        local_id,
                        annotated_ty,
                        &info,
                        &layout,
                        fields,
                        span,
                        def_id,
                    );
                }
            }

            if let Some(variant) = self.lowering.enum_variants.get(&def_id).cloned() {
                let layout = annotated_ty
                    .and_then(|ty| self.lowering.enum_layout_for_ty(ty).cloned())
                    .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
                if let Some(layout) = layout {
                    let payload_args: Vec<hir::Expr> =
                        fields.iter().map(|field| field.expr.clone()).collect();
                    self.assign_enum_variant(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        &payload_args,
                        span,
                    )?;
                    self.locals[local_id as usize].ty = layout.enum_ty.clone();
                    return Ok(());
                }
                self.lowering.emit_error(
                    span,
                    "unable to resolve enum layout for struct-like variant",
                );
                return Ok(());
            }

            if let Some(const_info) = self.lowering.const_values.get(&def_id).cloned() {
                if !fields.is_empty() {
                    self.lowering.emit_warning(
                        span,
                        "struct literal for enum variant payload ignored; using discriminant",
                    );
                }
                let statement = mir::Statement {
                    source_info: span,
                    kind: mir::StatementKind::Assign(
                        mir::Place::from_local(local_id),
                        mir::Rvalue::Use(mir::Operand::Constant(const_info.value.clone())),
                    ),
                };
                self.push_statement(statement);
                self.locals[local_id as usize].ty = const_info.ty.clone();
                return Ok(());
            }
        }

        if let Some(variant) = self.enum_variant_info_from_path(&resolved_path) {
            let layout = annotated_ty
                .and_then(|ty| self.lowering.enum_layout_for_ty(ty).cloned())
                .or_else(|| self.lowering.enum_layout_for_def(variant.enum_def, span));
            if let Some(layout) = layout {
                let payload_args: Vec<hir::Expr> =
                    fields.iter().map(|field| field.expr.clone()).collect();
                self.assign_enum_variant(
                    mir::Place::from_local(local_id),
                    &variant,
                    &layout,
                    &payload_args,
                    span,
                )?;
                self.locals[local_id as usize].ty = layout.enum_ty.clone();
                return Ok(());
            }
            self.lowering.emit_error(
                span,
                "unable to resolve enum layout for struct-like variant",
            );
            return Ok(());
        }

        self.lowering.emit_warning(
            span,
            "struct literal without registered definition; using tuple aggregate",
        );
        self.lower_unknown_struct_literal(local_id, annotated_ty, fields, span)
    }

    fn lower_registered_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        struct_def: &StructDefinition,
        layout: &StructLayout,
        fields: &[hir::StructExprField],
        span: Span,
        def_id: hir::DefId,
    ) -> Result<()> {
        let mut operands = Vec::with_capacity(struct_def.fields.len());
        let mut field_map: HashMap<String, &hir::StructExprField> = HashMap::new();
        for field in fields {
            field_map.insert(String::from(field.name.clone()), field);
        }

        let mut struct_fields = Vec::with_capacity(struct_def.fields.len());
        for (idx, field) in struct_def.fields.iter().enumerate() {
            let Some(field_ty) = layout.field_tys.get(idx) else {
                self.lowering.emit_error(
                    span,
                    format!(
                        "struct layout missing field type for `{}`",
                        field.name
                    ),
                );
                return Ok(());
            };
            struct_fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty: field_ty.clone(),
            });
        }

        if let (Some(expected_ty), Some(struct_info)) = (
            annotated_ty,
            self.lowering.struct_defs.get(&def_id),
        ) {
            let enum_layout = self
                .lowering
                .enum_layouts
                .iter()
                .find_map(|(key, layout)| {
                    if layout.enum_ty == *expected_ty {
                        Some((key.def_id, layout.clone()))
                    } else {
                        None
                    }
                });
            if let Some((enum_def_id, layout)) = enum_layout {
                if let Some(enum_def) = self.lowering.enum_defs.get(&enum_def_id) {
                    if let Some(variant_def) = enum_def
                        .variants
                        .iter()
                        .find(|variant| variant.name == struct_info.name)
                    {
                        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
                        operands.push(mir::Operand::Constant(mir::Constant {
                            span,
                            user_ty: None,
                            literal: mir::ConstantKind::Int(variant_def.discriminant),
                        }));

                        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
                            if let Some(field_info) = struct_fields.get(idx) {
                                let expr = match field_map.get(&field_info.name) {
                                    Some(field) => &field.expr,
                                    None => {
                                        operands.push(mir::Operand::Constant(mir::Constant {
                                            span,
                                            user_ty: None,
                                            literal: self.lowering.default_constant_for_ty(slot_ty),
                                        }));
                                        continue;
                                    }
                                };
                                let operand = self.lower_operand(expr, Some(slot_ty))?;
                                operands.push(operand.operand);
                            } else {
                                operands.push(mir::Operand::Constant(mir::Constant {
                                    span,
                                    user_ty: None,
                                    literal: self.lowering.default_constant_for_ty(slot_ty),
                                }));
                            }
                        }

                        self.push_statement(mir::Statement {
                            source_info: span,
                            kind: mir::StatementKind::Assign(
                                mir::Place::from_local(local_id),
                                mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, operands),
                            ),
                        });
                        self.locals[local_id as usize].ty = layout.enum_ty.clone();
                        return Ok(());
                    }
                }
            }
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
            self.locals[local_id as usize].ty = layout.ty.clone();
        }

        Ok(())
    }

    fn lower_unknown_struct_literal(
        &mut self,
        local_id: mir::LocalId,
        annotated_ty: Option<&Ty>,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<()> {
        let mut operands = Vec::with_capacity(fields.len());
        let mut tuple_types: Vec<Box<Ty>> = Vec::with_capacity(fields.len());

        for field in fields {
            let operand = self.lower_operand(&field.expr, None)?;
            tuple_types.push(Box::new(operand.ty.clone()));
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

        if let Some(ty) = annotated_ty {
            self.locals[local_id as usize].ty = ty.clone();
        } else {
            self.locals[local_id as usize].ty = Ty {
                kind: TyKind::Tuple(tuple_types),
            };
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
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some(variant) = self.enum_variant_info_from_path(path) {
                let mut layout = destination
                    .as_ref()
                    .and_then(|(_, ty)| self.lowering.enum_layout_for_ty(ty).cloned());
                if layout.is_none() {
                    let args = path
                        .segments
                        .last()
                        .and_then(|segment| segment.args.as_ref())
                        .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                        .unwrap_or_default();
                    if !args.is_empty() {
                        layout = self
                            .lowering
                            .enum_layout_for_instance(variant.enum_def, &args, expr.span);
                    } else {
                        layout = self.lowering.enum_layout_for_def(variant.enum_def, expr.span);
                    }
                }

                if let Some(layout) = layout {
                    let place = destination
                        .as_ref()
                        .map(|(place, _)| place.clone())
                        .unwrap_or_else(|| {
                            let local_id = self.allocate_temp(layout.enum_ty.clone(), expr.span);
                            mir::Place::from_local(local_id)
                        });
                    self.assign_enum_variant(
                        place.clone(),
                        &variant,
                        &layout,
                        args,
                        expr.span,
                    )?;
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = layout.enum_ty.clone();
                    }
                    if destination.is_some() {
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: layout.enum_ty.clone(),
                            struct_def: None,
                        }));
                    }
                    return Ok(None);
                }

                if !args.is_empty() {
                    self.lowering.emit_error(
                        expr.span,
                        "enum variant does not accept payload values",
                    );
                }
                if let Some(const_info) =
                    self.lowering.const_values.get(&variant.def_id).cloned()
                {
                    if let Some((place, _)) = destination {
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(mir::Operand::Constant(const_info.value.clone())),
                            ),
                        });
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: const_info.ty.clone(),
                            struct_def: None,
                        }));
                    }
                    return Ok(None);
                }
            }
        }
        let mut generic_def_id = None;
        let mut generic_method_def: Option<MethodDefinition> = None;
        let mut explicit_args: Vec<Ty> = Vec::new();
        if let hir::ExprKind::Path(path) = &callee.kind {
            if let Some(args) = path
                .segments
                .iter()
                .find_map(|segment| segment.args.as_ref())
            {
                explicit_args = self.lowering.lower_generic_args(Some(args), expr.span);
            }
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self.lowering.generic_function_defs.contains_key(def_id) {
                    generic_def_id = Some(*def_id);
                }
            }
            let qualified_name = path
                .segments
                .iter()
                .map(|seg| seg.name.as_str())
                .collect::<Vec<_>>()
                .join("::");
            if let Some(def) = self.lowering.method_defs.get(&qualified_name) {
                generic_method_def = Some(def.clone());
            } else if path.segments.len() >= 2 {
                let tail = format!(
                    "{}::{}",
                    path.segments[path.segments.len() - 2].name.as_str(),
                    path.segments[path.segments.len() - 1].name.as_str()
                );
                if let Some(def) = self.lowering.method_defs.get(&tail) {
                    generic_method_def = Some(def.clone());
                }
            }
        }

        let (mut func_operand, mut sig, mut callee_name) = if let Some(def_id) = generic_def_id {
            let function = self
                .lowering
                .generic_function_defs
                .get(&def_id)
                .cloned()
                .ok_or_else(|| crate::error::optimization_error("missing generic function def"))?;
            let sig = self.lowering.lower_function_sig(&function.sig, None);
            let fn_ty = self.lowering.function_pointer_ty(&sig);
            let name = function.sig.name.as_str().to_string();
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Fn(Symbol::new(name.clone()), fn_ty),
            });
            (operand, sig, Some(name))
        } else if let Some(def) = generic_method_def.as_ref() {
            let method_ctx = self.lowering.make_method_context(&def.self_ty);
            let sig = self
                .lowering
                .lower_function_sig(&def.function.sig, method_ctx.as_ref());
            let fn_ty = self.lowering.function_pointer_ty(&sig);
            let name = def.method_name.clone();
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Fn(Symbol::new(name.clone()), fn_ty),
            });
            (operand, sig, Some(name))
        } else {
            self.resolve_callee(callee)?
        };
        let mut associated_struct = callee_name
            .as_ref()
            .and_then(|name| self.lowering.method_lookup.get(name))
            .and_then(|info| info.struct_def);

        let mut lowered_args = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        for (idx, arg) in args.iter().enumerate() {
            let expected_ty = sig.inputs.get(idx);
            let operand = self.lower_operand(arg, expected_ty)?;
            arg_types.push(operand.ty.clone());
            lowered_args.push(operand.operand);
        }

        if let Some(def_id) = generic_def_id {
            if let Some(function) = self.lowering.generic_function_defs.get(&def_id).cloned() {
                let info = self.lowering.ensure_function_specialization(
                    self.program,
                    def_id,
                    &function,
                    &explicit_args,
                    &arg_types,
                    expr.span,
                )?;
                func_operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(
                        Symbol::new(info.name.clone()),
                        info.fn_ty.clone(),
                    ),
                });
                sig = info.sig.clone();
                callee_name = Some(info.name.clone());
            }
        }

        if let Some(def) = generic_method_def {
            let info = self.lowering.ensure_method_specialization(
                self.program,
                &def,
                &explicit_args,
                &arg_types,
                expr.span,
            )?;
            func_operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Fn(
                    Symbol::new(info.fn_name.clone()),
                    info.fn_ty.clone(),
                ),
            });
            sig = info.sig.clone();
            callee_name = Some(info.fn_name.clone());
            associated_struct = info.struct_def;
        }

        if let Some(name) = callee_name.as_ref() {
            if self.lowering.synthetic_runtime_functions.contains(name) {
                let destination_ty = destination.as_ref().map(|(_, ty)| ty);
                let updated_sig = self.lowering.update_placeholder_signature(
                    name,
                    &sig,
                    &arg_types,
                    destination_ty,
                );

                if updated_sig != sig {
                    let symbol = Symbol::new(name.clone());
                    let literal = match &func_operand {
                        mir::Operand::Constant(constant) => match &constant.literal {
                            mir::ConstantKind::Global(_, _) => mir::ConstantKind::Global(
                                symbol.clone(),
                                self.lowering.c_function_pointer_ty(&updated_sig),
                            ),
                            _ => mir::ConstantKind::Fn(
                                symbol.clone(),
                                self.lowering.function_pointer_ty(&updated_sig),
                            ),
                        },
                        _ => mir::ConstantKind::Fn(
                            symbol.clone(),
                            self.lowering.function_pointer_ty(&updated_sig),
                        ),
                    };
                    func_operand = mir::Operand::Constant(mir::Constant {
                        span: callee.span,
                        user_ty: None,
                        literal,
                    });
                    sig = updated_sig;

                    for (idx, expected_input) in sig.inputs.iter().enumerate() {
                        if let Some(original_ty) = arg_types.get(idx) {
                            if MirLowering::is_unit_ty(original_ty)
                                && matches!(
                                    expected_input.kind,
                                    TyKind::Ref(_, _, _) | TyKind::RawPtr(_)
                                )
                            {
                                lowered_args[idx] = mir::Operand::Constant(mir::Constant {
                                    span: callee.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::UInt(0),
                                });
                            }
                        }
                    }

                    for (idx, operand) in lowered_args.iter_mut().enumerate() {
                        if let Some(expected_input) = sig.inputs.get(idx) {
                            match operand {
                                mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty =
                                            expected_input.clone();
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
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

        // Handle local variables (e.g., function parameters) as indirect calls
        if let Some(hir::Res::Local(hir_id)) = &resolved_path.res {
            if let Some(local_id) = self.local_map.get(hir_id) {
                let local_id = *local_id;
                let ty = self.locals[local_id as usize].ty.clone();

                // Extract function signature from function pointer type
                if let TyKind::FnPtr(poly_fn_sig) = &ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    let place = mir::Place::from_local(local_id);
                    let operand = mir::Operand::copy(place);
                    return Ok((operand, sig, None));
                }

                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "local variable is not a function pointer, has type: {:?}",
                        ty
                    ),
                );
            } else {
                self.lowering
                    .emit_error(callee.span, "local variable not found in local_map");
            }
        }

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
                    .unwrap_or_else(|| hir::Symbol::new(format!("fn#{}", def_id)));
                let ty = self.lowering.function_pointer_ty(&sig);
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(mir::Symbol::from(name.clone()), ty),
                });
                return Ok((operand, sig, Some(String::from(name))));
            }
        }

        let name = resolved_path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
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
                .get(&String::from(struct_name.clone()))
                .and_then(|methods| methods.get(&String::from(method_name.clone())))
            {
                let operand = mir::Operand::Constant(mir::Constant {
                    span: callee.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(
                        mir::Symbol::new(info.fn_name.clone()),
                        info.fn_ty.clone(),
                    ),
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
                literal: mir::ConstantKind::Global(mir::Symbol::new(name.clone()), ty),
            });
            return Ok((operand, sig, Some(name)));
        }

        if let Some(info) = self.lowering.method_lookup.get(&name) {
            let operand = mir::Operand::Constant(mir::Constant {
                span: callee.span,
                user_ty: None,
                literal: mir::ConstantKind::Fn(
                    mir::Symbol::new(info.fn_name.clone()),
                    info.fn_ty.clone(),
                ),
            });
            return Ok((operand, info.sig.clone(), Some(info.fn_name.clone())));
        }

        // Search for function by name in def_map (for functions not yet registered or created during earlier phases)
        let name_tail = name.split("::").last().unwrap_or(name.as_str());
        for (def_id, item) in &self.program.def_map {
            if let hir::ItemKind::Function(func) = &item.kind {
                if func.sig.name.as_str() == name || func.sig.name.as_str() == name_tail {
                    let sig = self.lowering.lower_function_sig(&func.sig, None);
                    let ty = self.lowering.function_pointer_ty(&sig);
                    let operand = mir::Operand::Constant(mir::Constant {
                        span: callee.span,
                        user_ty: None,
                        literal: mir::ConstantKind::Fn(mir::Symbol::new(name.clone()), ty),
                    });
                    // Cache it for future lookups
                    self.lowering.function_sigs.insert(*def_id, sig.clone());
                    return Ok((operand, sig, Some(name)));
                }
            }
            if let hir::ItemKind::Impl(impl_block) = &item.kind {
                for impl_item in &impl_block.items {
                    if let hir::ImplItemKind::Method(function) = &impl_item.kind {
                        if function.sig.name.as_str() == name_tail {
                            let method_ctx = self.lowering.make_method_context(&impl_block.self_ty);
                            let sig = self
                                .lowering
                                .lower_function_sig(&function.sig, method_ctx.as_ref());
                            let ty = self.lowering.function_pointer_ty(&sig);
                            let operand = mir::Operand::Constant(mir::Constant {
                                span: callee.span,
                                user_ty: None,
                                literal: mir::ConstantKind::Fn(mir::Symbol::new(name.clone()), ty),
                            });
                            return Ok((operand, sig, Some(name)));
                        }
                    }
                }
            }
        }

        self.lowering.emit_warning(
            callee.span,
            format!(
                "treating call target `{}` as opaque runtime stub during MIR lowering",
                name
            ),
        );
        let sig = self.lowering.placeholder_function_sig(&name);
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let operand = mir::Operand::Constant(mir::Constant {
            span: callee.span,
            user_ty: None,
            literal: mir::ConstantKind::Fn(Symbol::new(name.clone()), fn_ty.clone()),
        });
        Ok((operand, sig, Some(name)))
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
                let fallback_local = resolved_path
                    .segments
                    .first()
                    .filter(|_| resolved_path.segments.len() == 1)
                    .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied());
                if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                if let Some(variant) = self.lowering.enum_variants.get(def_id).cloned() {
                    let mut layout = expected
                        .and_then(|ty| self.lowering.enum_layout_for_ty(ty).cloned());
                    if layout.is_none() {
                        let args = resolved_path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                            .unwrap_or_default();
                        if !args.is_empty() {
                            layout = self
                                .lowering
                                .enum_layout_for_instance(variant.enum_def, &args, expr.span);
                        } else {
                            layout = self
                                .lowering
                                .enum_layout_for_def(variant.enum_def, expr.span);
                        }
                    }
                    if let Some(layout) = layout {
                        return self.lower_enum_variant_value(
                            &variant,
                            &layout,
                            &[],
                            expr.span,
                        );
                    }
                    self.lowering.emit_error(
                        expr.span,
                        "unable to resolve enum layout for variant value",
                    );
                }
                    if let Some(const_info) = self.lowering.const_values.get(def_id) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(const_info.value.clone()),
                            ty: const_info.ty.clone(),
                        });
                    }

                    if let Some(const_item) = self.program.def_map.get(def_id) {
                        if let hir::ItemKind::Const(konst) = &const_item.kind {
                            let ty = self.lower_type_expr(&konst.ty);
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
                        } else if let hir::ItemKind::Function(func) = &const_item.kind {
                            // Function reference - create a function pointer constant
                            let sig = self.lowering.lower_function_sig(&func.sig, None);
                            let fn_ty = self.lowering.function_pointer_ty(&sig);
                            let fn_name = func.sig.name.clone();
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(
                                        mir::Symbol::from(fn_name),
                                        fn_ty.clone(),
                                    ),
                                }),
                                ty: fn_ty,
                            });
                        }
                    } else if let Some(konst) = self.const_items.get(def_id).cloned() {
                        let ty = self.lower_type_expr(&konst.ty);
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

                if let Some(local_id) = fallback_local {
                    let ty = self.locals[local_id as usize].ty.clone();
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(mir::Place::from_local(local_id)),
                        ty,
                    });
                }

                let name = resolved_path
                    .segments
                    .iter()
                    .map(|seg| seg.name.as_str())
                    .collect::<Vec<_>>()
                    .join("::");

                let expected_sig = expected.and_then(|ty| {
                    if let TyKind::FnPtr(poly_fn_sig) = &ty.kind {
                        let sig = &poly_fn_sig.binder.value;
                        Some(mir::FunctionSig {
                            inputs: sig.inputs.iter().map(|t| (**t).clone()).collect(),
                            output: (*sig.output).clone(),
                        })
                    } else {
                        None
                    }
                });
                let mut fallback_candidate: Option<(mir::FunctionSig, String)> = None;
                for (_def_id, item) in &self.program.def_map {
                    if let hir::ItemKind::Function(func) = &item.kind {
                        let func_name = func.sig.name.as_str();
                        if func_name == name || func_name.starts_with(&name) {
                            let sig = self.lowering.lower_function_sig(&func.sig, None);
                            if let Some(expected_sig) = &expected_sig {
                                if &sig == expected_sig {
                                    let fn_ty = self.lowering.function_pointer_ty(&sig);
                                    return Ok(OperandInfo {
                                        operand: mir::Operand::Constant(mir::Constant {
                                            span: expr.span,
                                            user_ty: None,
                                            literal: mir::ConstantKind::Fn(
                                                mir::Symbol::from(func.sig.name.clone()),
                                                fn_ty.clone(),
                                            ),
                                        }),
                                        ty: fn_ty,
                                    });
                                }
                            } else if fallback_candidate.is_none() {
                                fallback_candidate = Some((sig, func.sig.name.to_string()));
                            }
                        }
                    }
                }
                if let Some((sig, func_name)) = fallback_candidate {
                    let fn_ty = self.lowering.function_pointer_ty(&sig);
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            user_ty: None,
                            literal: mir::ConstantKind::Fn(
                                mir::Symbol::from(func_name),
                                fn_ty.clone(),
                            ),
                        }),
                        ty: fn_ty,
                    });
                }
                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating path `{}` as opaque constant during MIR lowering",
                        name
                    ),
                );
                let ty = self.lowering.opaque_ty(&name);
                let operand = mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Global(Symbol::new(name), ty.clone()),
                });
                Ok(OperandInfo { operand, ty })
            }
            hir::ExprKind::Cast(inner, ty_expr) => {
                let operand = self.lower_operand(inner, None)?;
                let target_ty = self.lower_type_expr(ty_expr);
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
                if matches!(
                    call.kind,
                    IntrinsicCallKind::Print | IntrinsicCallKind::Println
                ) {
                    self.emit_printf_call(call, expr.span)?;
                    let unit_ty = MirLowering::unit_ty();
                    let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: unit_ty,
                    });
                }
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
                        .emit_warning(expr.span, "const block intrinsic expects an argument");
                    let unit_ty = MirLowering::unit_ty();
                    let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: unit_ty,
                    });
                }
                if let Some((literal, ty)) = self.lower_intrinsic_constant(call, expr.span) {
                    let operand = mir::Operand::Constant(mir::Constant {
                        span: expr.span,
                        user_ty: None,
                        literal,
                    });
                    return Ok(OperandInfo { operand, ty });
                }

                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating intrinsic {:?} as opaque value during MIR operand lowering",
                        call.kind
                    ),
                );
                let unit_ty = MirLowering::unit_ty();
                let local_id = self.allocate_temp(unit_ty.clone(), expr.span);
                let local_place = mir::Place::from_local(local_id);
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        local_place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                    ),
                };
                self.push_statement(statement);
                Ok(OperandInfo {
                    operand: mir::Operand::copy(local_place),
                    ty: unit_ty,
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

    fn constant_bool_operand(&self, value: bool, span: Span) -> OperandInfo {
        OperandInfo::constant(
            span,
            Ty { kind: TyKind::Bool },
            mir::ConstantKind::Bool(value),
        )
    }

    fn fallback_operand_for_reference(&self, reference_ty: &Ty, span: Span) -> OperandInfo {
        match &reference_ty.kind {
            TyKind::Uint(kind) => OperandInfo::constant(
                span,
                Ty {
                    kind: TyKind::Uint(*kind),
                },
                mir::ConstantKind::UInt(0),
            ),
            TyKind::Int(kind) => OperandInfo::constant(
                span,
                Ty {
                    kind: TyKind::Int(*kind),
                },
                mir::ConstantKind::Int(0),
            ),
            TyKind::Float(kind) => OperandInfo::constant(
                span,
                Ty {
                    kind: TyKind::Float(*kind),
                },
                mir::ConstantKind::Float(0.0),
            ),
            TyKind::Bool => self.constant_bool_operand(false, span),
            _ => self.constant_bool_operand(false, span),
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
            hir::Lit::Null => {
                let ty = expected.cloned().unwrap_or_else(|| Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                });
                (mir::ConstantKind::Null, ty)
            }
        }
    }

    fn lower_intrinsic_constant(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(mir::ConstantKind, Ty)> {
        let args = match &call.payload {
            IntrinsicCallPayload::Args { args } => args,
            IntrinsicCallPayload::Format { .. } => {
                self.lowering.emit_warning(
                    span,
                    "treating formatted intrinsic payload as opaque during MIR lowering",
                );
                return None;
            }
        };

        match call.kind {
            IntrinsicCallKind::SizeOf => {
                let target_expr = match args.get(0) {
                    Some(expr) => expr,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "sizeof! only supports struct types at the moment");
                        return None;
                    }
                };

                let size = match self.compute_struct_size(span, &struct_ref) {
                    Some(value) => value,
                    None => return None,
                };

                Some((
                    mir::ConstantKind::UInt(size),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicCallKind::FieldCount => {
                let target_expr = match args.get(0) {
                    Some(expr) => expr,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "field_count! only supports struct types");
                        return None;
                    }
                };

                let field_count = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.fields.len() as u64,
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::UInt(field_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            IntrinsicCallKind::HasField => {
                if args.len() != 2 {
                    self.lowering
                        .emit_error(span, "hasfield! intrinsic expects a type and field name");
                    return None;
                }

                let struct_ref = match self.resolve_struct_ref(&args[0]) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "hasfield! only supports struct types");
                        return None;
                    }
                };

                let field_name = match self.expect_string_literal(&args[1], span) {
                    Some(name) => name,
                    None => return None,
                };

                let has_field = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.field_index.contains_key(&field_name),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                Some((
                    mir::ConstantKind::Bool(has_field),
                    Ty { kind: TyKind::Bool },
                ))
            }
            IntrinsicCallKind::MethodCount => {
                let target_expr = match args.get(0) {
                    Some(expr) => expr,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! intrinsic expects one argument");
                        return None;
                    }
                };

                let struct_ref = match self.resolve_struct_ref(target_expr) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "method_count! only supports struct types");
                        return None;
                    }
                };

                let struct_name = match self.lowering.struct_defs.get(&struct_ref.def_id) {
                    Some(info) => info.name.clone(),
                    None => {
                        self.lowering
                            .emit_error(span, "struct metadata is unavailable during MIR lowering");
                        return None;
                    }
                };

                let method_count = self
                    .lowering
                    .struct_methods
                    .get(&struct_name)
                    .map(|methods| methods.len() as u64)
                    .unwrap_or(0);

                Some((
                    mir::ConstantKind::UInt(method_count),
                    Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    },
                ))
            }
            _ => None,
        }
    }

    fn emit_printf_call(&mut self, call: &hir::IntrinsicCallExpr, span: Span) -> Result<()> {
        let template = match &call.payload {
            IntrinsicCallPayload::Format { template } => template,
            IntrinsicCallPayload::Args { .. } => {
                self.lowering
                    .emit_error(span, "printf lowering requires format payload");
                return Ok(());
            }
        };

        if !template.kwargs.is_empty() {
            self.lowering
                .emit_error(span, "named arguments are not supported in printf lowering");
            return Ok(());
        }

        let mut lowered_args = Vec::with_capacity(template.args.len());
        for arg in &template.args {
            lowered_args.push(self.lower_operand(arg, None)?);
        }

        let mut prepared_args = Vec::with_capacity(lowered_args.len());
        for arg in lowered_args {
            prepared_args.push(self.prepare_printf_arg(arg, span)?);
        }

        let mut format = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                hir::FormatTemplatePart::Literal(text) => format.push_str(text),
                hir::FormatTemplatePart::Placeholder(placeholder) => {
                    let arg_index = match &placeholder.arg_ref {
                        hir::FormatArgRef::Implicit => {
                            let current = implicit_index;
                            implicit_index += 1;
                            current
                        }
                        hir::FormatArgRef::Positional(index) => *index,
                        hir::FormatArgRef::Named(name) => {
                            self.lowering.emit_error(
                                span,
                                format!("named argument '{name}' is not supported"),
                            );
                            return Ok(());
                        }
                    };

                    let Some((_, _, spec)) = prepared_args.get(arg_index) else {
                        self.lowering.emit_error(
                            span,
                            format!(
                                "format placeholder references missing argument at index {}",
                                arg_index
                            ),
                        );
                        return Ok(());
                    };

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(explicit);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                        }
                    } else {
                        format.push_str(spec);
                    }
                }
            }
        }

        if call.kind == IntrinsicCallKind::Println {
            format.push('\n');
        }

        let (fmt_const, fmt_ty) = self.lower_literal(&hir::Lit::Str(format), None);
        let mut operands = Vec::with_capacity(1 + prepared_args.len());
        let mut arg_types = Vec::with_capacity(1 + prepared_args.len());

        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: fmt_const,
        }));
        arg_types.push(fmt_ty);

        for (operand, ty, _) in prepared_args {
            operands.push(operand);
            arg_types.push(ty);
        }

        let existing_sig = self.lowering.placeholder_function_sig("printf");
        let sig = self.lowering.update_placeholder_signature(
            "printf",
            &existing_sig,
            &arg_types,
            None,
        );
        let fn_ty = self.lowering.c_function_pointer_ty(&sig);
        let func_operand = mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Global(Symbol::new("printf"), fn_ty),
        });

        let continue_block = self.new_block();
        let temp_local = self.allocate_temp(sig.output.clone(), span);
        let destination = Some((mir::Place::from_local(temp_local), continue_block));
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func: func_operand,
                args: operands,
                destination,
                cleanup: None,
                from_hir_call: true,
                fn_span: span,
            },
        };

        self.blocks[self.current_block as usize].terminator = Some(terminator);
        self.current_block = continue_block;
        Ok(())
    }

    fn prepare_printf_arg(
        &mut self,
        arg: OperandInfo,
        span: Span,
    ) -> Result<(mir::Operand, Ty, String)> {
        let (operand, ty) = (arg.operand, arg.ty);
        match &ty.kind {
            TyKind::Bool => Ok((operand, ty.clone(), "%d".to_string())),
            TyKind::Char => Ok((operand, ty.clone(), "%c".to_string())),
            TyKind::Int(int_ty) => Ok((operand, ty.clone(), match int_ty {
                IntTy::I8 => "%hhd",
                IntTy::I16 => "%hd",
                IntTy::I32 => "%d",
                IntTy::I64 => "%lld",
                IntTy::I128 => "%lld",
                IntTy::Isize => "%lld",
            }.to_string())),
            TyKind::Uint(uint_ty) => Ok((operand, ty.clone(), match uint_ty {
                UintTy::U8 => "%hhu",
                UintTy::U16 => "%hu",
                UintTy::U32 => "%u",
                UintTy::U64 => "%llu",
                UintTy::U128 => "%llu",
                UintTy::Usize => "%llu",
            }.to_string())),
            TyKind::Float(_) => Ok((operand, ty.clone(), "%f".to_string())),
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    Ok((operand, ty.clone(), "%s".to_string()))
                } else {
                    self.lowering.emit_error(
                        span,
                        "printf only supports raw pointers to byte strings",
                    );
                    Ok((operand, ty.clone(), "%s".to_string()))
                }
            }
            TyKind::Slice(elem) => {
                if self.is_c_string_ptr(elem.as_ref()) {
                    Ok((operand, ty.clone(), "%s".to_string()))
                } else {
                    self.lowering
                        .emit_error(span, "printf only supports byte slices");
                    Ok((operand, ty.clone(), "%s".to_string()))
                }
            }
            TyKind::Ref(_, inner, _) => {
                if let TyKind::RawPtr(type_and_mut) = &inner.kind {
                    if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if let TyKind::Slice(elem) = &inner.kind {
                    if self.is_c_string_ptr(elem.as_ref()) {
                        let place = match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                            _ => {
                                self.lowering.emit_error(
                                    span,
                                    "printf cannot dereference non-place arguments",
                                );
                                return Ok((operand, ty.clone(), "%s".to_string()));
                            }
                        };
                        let mut deref_place = place.clone();
                        deref_place.projection.push(mir::PlaceElem::Deref);
                        return Ok((
                            mir::Operand::Copy(deref_place),
                            (*inner.as_ref()).clone(),
                            "%s".to_string(),
                        ));
                    }
                }
                if self.is_c_string_ptr(inner.as_ref()) {
                    return Ok((operand, ty.clone(), "%s".to_string()));
                }
                let place = match operand {
                    mir::Operand::Copy(place) | mir::Operand::Move(place) => place,
                    _ => {
                        self.lowering.emit_error(
                            span,
                            "printf cannot dereference non-place arguments",
                        );
                        return Ok((operand, ty.clone(), "%s".to_string()));
                    }
                };
                let mut deref_place = place.clone();
                deref_place.projection.push(mir::PlaceElem::Deref);
                let deref_ty = (*inner.as_ref()).clone();
                let spec = self.printf_spec_for_ty(&deref_ty, span)?;
                Ok((mir::Operand::Copy(deref_place), deref_ty, spec))
            }
            _ => {
                self.lowering
                    .emit_error(span, "printf argument type is not supported");
                Ok((operand, ty.clone(), "%s".to_string()))
            }
        }
    }

    fn printf_spec_for_ty(&mut self, ty: &Ty, span: Span) -> Result<String> {
        let spec = match &ty.kind {
            TyKind::Bool => "%d",
            TyKind::Char => "%c",
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => "%hhd",
                IntTy::I16 => "%hd",
                IntTy::I32 => "%d",
                IntTy::I64 => "%lld",
                IntTy::I128 => "%lld",
                IntTy::Isize => "%lld",
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => "%hhu",
                UintTy::U16 => "%hu",
                UintTy::U32 => "%u",
                UintTy::U64 => "%llu",
                UintTy::U128 => "%llu",
                UintTy::Usize => "%llu",
            },
            TyKind::Float(_) => "%f",
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    "%s"
                } else {
                    self.lowering.emit_error(
                        span,
                        "printf only supports raw pointers to byte strings",
                    );
                    "%s"
                }
            }
            _ => {
                self.lowering
                    .emit_error(span, "printf argument type is not supported");
                "%s"
            }
        };
        Ok(spec.to_string())
    }

    fn is_c_string_ptr(&self, ty: &Ty) -> bool {
        matches!(
            ty.kind,
            TyKind::Int(IntTy::I8) | TyKind::Uint(UintTy::U8)
        )
    }

    fn resolve_struct_ref(&mut self, expr: &hir::Expr) -> Option<StructRef> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };

        let args = path
            .segments
            .iter()
            .find_map(|segment| segment.args.as_ref())
            .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
            .unwrap_or_default();

        if let Some(hir::Res::Def(def_id)) = &path.res {
            return Some(StructRef {
                def_id: *def_id,
                args,
            });
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            let mut matches = self
                .lowering
                .struct_defs
                .iter()
                .filter_map(|(def_id, info)| (info.name == name).then_some(*def_id))
                .collect::<Vec<_>>();
            if matches.len() == 1 {
                return Some(StructRef {
                    def_id: matches.pop()?,
                    args,
                });
            }
        }

        None
    }

    fn compute_struct_size(&mut self, span: Span, struct_ref: &StructRef) -> Option<u64> {
        let layout = match self
            .lowering
            .struct_layout_for_instance(struct_ref.def_id, &struct_ref.args, span)
        {
            Some(layout) => layout,
            None => return None,
        };

        let mut total = 0u64;
        for field_ty in layout.field_tys {
            let field_size = match self.compute_ty_size(span, &field_ty) {
                Some(size) => size,
                None => return None,
            };
            total = total.saturating_add(field_size);
        }
        Some(total)
    }

    fn compute_ty_size(&mut self, span: Span, ty: &Ty) -> Option<u64> {
        match &ty.kind {
            TyKind::Bool => Some(1),
            TyKind::Char => Some(4),
            TyKind::Int(int_ty) => Some(match int_ty {
                IntTy::I8 => 1,
                IntTy::I16 => 2,
                IntTy::I32 => 4,
                IntTy::I64 => 8,
                IntTy::I128 => 16,
                IntTy::Isize => 8,
            }),
            TyKind::Uint(uint_ty) => Some(match uint_ty {
                UintTy::U8 => 1,
                UintTy::U16 => 2,
                UintTy::U32 => 4,
                UintTy::U64 => 8,
                UintTy::U128 => 16,
                UintTy::Usize => 8,
            }),
            TyKind::Float(float_ty) => Some(match float_ty {
                FloatTy::F32 => 4,
                FloatTy::F64 => 8,
            }),
            TyKind::Tuple(elements) => {
                let mut total = 0u64;
                for elem in elements {
                    let size = match self.compute_ty_size(span, elem) {
                        Some(value) => value,
                        None => return None,
                    };
                    total = total.saturating_add(size);
                }
                Some(total)
            }
            TyKind::Array(elem_ty, len) => {
                let len = match self.const_kind_to_u64(span, len) {
                    Some(value) => value,
                    None => return None,
                };
                let elem_size = match self.compute_ty_size(span, elem_ty) {
                    Some(value) => value,
                    None => return None,
                };
                Some(elem_size.saturating_mul(len))
            }
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) | TyKind::FnPtr(_) | TyKind::FnDef(_, _) => {
                Some(8)
            }
            TyKind::Never => Some(0),
            TyKind::Error(_) => None,
            TyKind::Slice(_) => {
                self.lowering
                    .emit_error(span, "size_of for slice types is not yet supported");
                None
            }
            TyKind::Adt(_, _)
            | TyKind::Dynamic(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Projection(_)
            | TyKind::Opaque(_, _)
            | TyKind::Param(_)
            | TyKind::Placeholder(_)
            | TyKind::Bound(_, _)
            | TyKind::Infer(_) => {
                self.lowering.emit_error(
                    span,
                    format!("size_of for type `{:?}` is not supported", ty.kind),
                );
                None
            }
        }
    }

    fn const_kind_to_u64(&mut self, span: Span, konst: &ConstKind) -> Option<u64> {
        match konst {
            ConstKind::Value(ConstValue::Scalar(Scalar::Int(int))) => Some(int.data as u64),
            ConstKind::Value(ConstValue::Scalar(Scalar::Ptr(_))) => {
                self.lowering.emit_error(
                    span,
                    "array length uses a pointer value, which is unsupported",
                );
                None
            }
            ConstKind::Value(ConstValue::ZeroSized) => Some(0),
            _ => {
                self.lowering
                    .emit_error(span, "array length is not a compile-time integer constant");
                None
            }
        }
    }

    fn expect_string_literal(&mut self, expr: &hir::Expr, span: Span) -> Option<String> {
        match &expr.kind {
            hir::ExprKind::Literal(hir::Lit::Str(value)) => Some(value.clone()),
            _ => {
                self.lowering
                    .emit_error(span, "intrinsic argument must be a string literal");
                None
            }
        }
    }

    fn lower_place(&mut self, expr: &hir::Expr) -> Result<Option<PlaceInfo>> {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                let fallback_local = path
                    .segments
                    .first()
                    .filter(|_| path.segments.len() == 1)
                    .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied());
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
                        if let Some(local_id) = fallback_local {
                            let ty = self.locals[local_id as usize].ty.clone();
                            let struct_def = self.struct_def_from_ty(&ty);
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
                                let ty = self.lower_type_expr(&konst.ty);
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
                            let ty = self.lower_type_expr(&konst.ty);
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
                    _ => {
                        if let Some(local_id) = fallback_local {
                            let ty = self.locals[local_id as usize].ty.clone();
                            let struct_def = self.struct_def_from_ty(&ty);
                            return Ok(Some(PlaceInfo {
                                place: mir::Place::from_local(local_id),
                                ty,
                                struct_def,
                            }));
                        }
                    }
                }
                Ok(None)
            }
            hir::ExprKind::Unary(hir::UnOp::Deref, inner) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    self.lowering.emit_error(
                        expr.span,
                        "dereference target is not a place expression",
                    );
                    return Ok(None);
                };

                let mut base_ty = place_info.ty.clone();
                loop {
                    match &base_ty.kind {
                        TyKind::Ref(_, inner_ty, _) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = inner_ty.as_ref().clone();
                            break;
                        }
                        TyKind::RawPtr(type_and_mut) => {
                            place_info.place.projection.push(mir::PlaceElem::Deref);
                            base_ty = type_and_mut.ty.as_ref().clone();
                            break;
                        }
                        _ => {
                            self.lowering.emit_error(
                                expr.span,
                                "dereference requires a reference or pointer type",
                            );
                            return Ok(None);
                        }
                    }
                }

                place_info.ty = base_ty;
                place_info.struct_def = self.struct_def_from_ty(&place_info.ty);
                Ok(Some(place_info))
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
                    match self.lowering.struct_field(struct_def, &base_ty, field.as_str()) {
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
                    struct_def: self.struct_def_from_ty(&field_info.ty),
                }))
            }
            hir::ExprKind::Index(base, index) => {
                let base_place = match self.lower_place(base)? {
                    Some(info) => info,
                    None => {
                        self.lowering
                            .emit_error(base.span, "unsupported base expression for index access");
                        return Ok(None);
                    }
                };

                let index_ty = Ty {
                    kind: TyKind::Uint(UintTy::Usize),
                };
                let index_operand = self.lower_operand(index, Some(&index_ty))?;
                let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                let index_place = mir::Place::from_local(index_local);
                let assign = mir::Statement {
                    source_info: index.span,
                    kind: mir::StatementKind::Assign(
                        index_place.clone(),
                        mir::Rvalue::Use(index_operand.operand),
                    ),
                };
                self.push_statement(assign);

                let mut place = base_place.place.clone();
                let mut base_ty = base_place.ty.clone();

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

                let element_ty = match &base_ty.kind {
                    TyKind::Array(elem, _) => (*elem.clone()),
                    TyKind::Slice(elem) => (*elem.clone()),
                    _ => {
                        self.lowering
                            .emit_error(base.span, "index access requires array or slice type");
                        return Ok(None);
                    }
                };

                place.projection.push(mir::PlaceElem::Index(index_local));
                let struct_def = self.struct_def_from_ty(&element_ty);

                Ok(Some(PlaceInfo {
                    place,
                    ty: element_ty,
                    struct_def,
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
            hir::ExprKind::Literal(_) | hir::ExprKind::Path(_) | hir::ExprKind::Index(_, _) => {
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
                let target_ty = self.lower_type_expr(ty_expr);
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
            hir::ExprKind::Loop(block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_loop_expr(expr.span, block, Some(destination), true)?;
            }
            hir::ExprKind::While(cond, block) => {
                let destination = LoopDestination {
                    place: place.clone(),
                    ty: expected_ty.clone(),
                };
                self.lower_while_expr(expr.span, cond, block, Some(destination))?;
            }
            hir::ExprKind::Break(value) => {
                self.lower_break(expr.span, value.as_deref())?;
            }
            hir::ExprKind::Continue => {
                self.lower_continue(expr.span)?;
            }
            hir::ExprKind::Struct(path, fields) => {
                let local_id = place.local;
                self.lower_struct_literal(local_id, Some(expected_ty), path, fields, expr.span)?;
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let mut left = self.lower_operand(lhs, None)?;
                let mut right = self.lower_operand(rhs, None)?;

                if MirLowering::is_unit_ty(&left.ty) {
                    left = self.fallback_operand_for_reference(&right.ty, expr.span);
                }
                if MirLowering::is_unit_ty(&right.ty) {
                    right = self.fallback_operand_for_reference(&left.ty, expr.span);
                }

                if MirLowering::is_unit_ty(&left.ty) && MirLowering::is_unit_ty(&right.ty) {
                    left = self.constant_bool_operand(false, expr.span);
                    right = self.constant_bool_operand(false, expr.span);
                }

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
                    let place_info = match self.lower_place(expr)? {
                        Some(info) => info,
                        None => {
                            self.lowering.emit_error(
                                expr.span,
                                "dereference expressions must resolve to a place",
                            );
                            return Ok(());
                        }
                    };
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Use(mir::Operand::Copy(place_info.place.clone())),
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = place_info.ty.clone();
                    }
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
            hir::ExprKind::Match(scrutinee, arms) => {
                self.lower_match_expr(expr.span, scrutinee, arms, place, expected_ty)?;
            }
            hir::ExprKind::IntrinsicCall(call) => match call.kind {
                IntrinsicCallKind::ConstBlock => {
                    if let IntrinsicCallPayload::Args { args } = &call.payload {
                        if let Some(arg) = args.first() {
                            self.lower_expr_into_place(arg, place, expected_ty)?;
                            return Ok(());
                        }
                    }
                    self.lowering
                        .emit_warning(expr.span, "const block intrinsic expects an argument");
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                }
                IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                    self.emit_printf_call(call, expr.span)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                    if (place.local as usize) < self.locals.len() {
                        self.locals[place.local as usize].ty = MirLowering::unit_ty();
                    }
                    return Ok(());
                }
                _ => {
                    if let Some((literal, _ty)) = self.lower_intrinsic_constant(call, expr.span) {
                        let statement = mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal,
                                })),
                            ),
                        };
                        self.push_statement(statement);
                        return Ok(());
                    }

                    self.lowering.emit_warning(
                        expr.span,
                        format!(
                            "intrinsic {:?} is not yet supported for MIR assignment",
                            call.kind
                        ),
                    );
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(statement);
                }
            },
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
                                .and_then(|methods| methods.get(&String::from(method_key.clone())))
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
                            methods
                                .get(&String::from(method_key.clone()))
                                .map(|info| info.clone())
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
                        literal: mir::ConstantKind::Fn(
                            mir::Symbol::new(info.fn_name.clone()),
                            info.fn_ty.clone(),
                        ),
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

                if let Ok(Some(place_info)) = self.lower_place(receiver) {
                    if let Some(def_id) = place_info
                        .struct_def
                        .or_else(|| self.struct_def_from_ty(&place_info.ty))
                    {
                        if let Some(struct_entry) = self.lowering.struct_defs.get(&def_id) {
                            let method_key =
                                format!("{}::{}", struct_entry.name, method_name.as_str());
                            if let Some(def) = self.lowering.method_defs.get(&method_key).cloned()
                            {
                                let method_ctx = self.lowering.make_method_context(&def.self_ty);
                                let tentative_sig = self
                                    .lowering
                                    .lower_function_sig(&def.function.sig, method_ctx.as_ref());
                                let receiver_expected = tentative_sig.inputs.get(0);
                                let receiver_operand =
                                    self.lower_operand(receiver, receiver_expected)?;

                                let mut lowered_args = Vec::with_capacity(args.len() + 1);
                                let mut arg_types = Vec::with_capacity(args.len() + 1);
                                arg_types.push(receiver_operand.ty.clone());
                                lowered_args.push(receiver_operand.operand);
                                for (idx, arg) in args.iter().enumerate() {
                                    let expected = tentative_sig.inputs.get(idx + 1);
                                    let operand = self.lower_operand(arg, expected)?;
                                    arg_types.push(operand.ty.clone());
                                    lowered_args.push(operand.operand);
                                }

                                let info = self.lowering.ensure_method_specialization(
                                    self.program,
                                    &def,
                                    &[],
                                    &arg_types,
                                    expr.span,
                                )?;

                                let func_operand = mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(
                                        mir::Symbol::new(info.fn_name.clone()),
                                        info.fn_ty.clone(),
                                    ),
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

                                self.blocks[self.current_block as usize].terminator =
                                    Some(terminator);
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
                        }
                    }
                }

                if method_name.as_str() == "len" && args.is_empty() {
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
                            if let Some(konst) = self.const_items.get(def_id).cloned() {
                                let ty = self.lower_type_expr(&konst.ty);
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

                let receiver_operand = self.lower_operand(receiver, None)?;
                let mut lowered_args = Vec::with_capacity(args.len() + 1);
                let mut input_tys = Vec::with_capacity(args.len() + 1);
                lowered_args.push(receiver_operand.operand.clone());
                input_tys.push(receiver_operand.ty.clone());
                for arg in args {
                    let lowered = self.lower_operand(arg, None)?;
                    input_tys.push(lowered.ty.clone());
                    lowered_args.push(lowered.operand);
                }

                let result_ty = expected_ty.clone();
                let type_name = self
                    .lowering
                    .display_type_name(&receiver_operand.ty)
                    .unwrap_or_else(|| "opaque".to_string());
                let fn_name = format!("{}::{}", type_name, method_name);
                let sig = mir::FunctionSig {
                    inputs: input_tys,
                    output: result_ty.clone(),
                };
                self.lowering.ensure_runtime_stub(&fn_name, &sig);

                let sanitized_sig = self
                    .lowering
                    .runtime_functions
                    .get(&fn_name)
                    .cloned()
                    .unwrap_or_else(|| self.lowering.sanitize_function_sig(&sig));
                let arg_types = sig.inputs.clone();

                for (idx, expected_input) in sanitized_sig.inputs.iter().enumerate() {
                    if let Some(original_ty) = arg_types.get(idx) {
                        if MirLowering::is_unit_ty(original_ty)
                            && matches!(
                                expected_input.kind,
                                TyKind::Ref(_, _, _) | TyKind::RawPtr(_)
                            )
                        {
                            lowered_args[idx] = mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                user_ty: None,
                                literal: mir::ConstantKind::UInt(0),
                            });
                        }
                    }

                    if let Some(operand) = lowered_args.get_mut(idx) {
                        match operand {
                            mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = expected_input.clone();
                                }
                            }
                            _ => {}
                        }
                    }
                }

                let func_operand = mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(
                        Symbol::new(fn_name.clone()),
                        self.lowering.function_pointer_ty(&sanitized_sig),
                    ),
                });

                let continue_block = self.new_block();
                let destination = Some((place.clone(), continue_block));
                self.blocks[self.current_block as usize].terminator = Some(mir::Terminator {
                    source_info: expr.span,
                    kind: mir::TerminatorKind::Call {
                        func: func_operand,
                        args: lowered_args,
                        destination: destination.clone(),
                        cleanup: None,
                        from_hir_call: true,
                        fn_span: expr.span,
                    },
                });

                self.current_block = continue_block;
                if (place.local as usize) < self.locals.len() {
                    self.locals[place.local as usize].ty = result_ty.clone();
                }
                if let Some(struct_def) = self.struct_def_from_ty(&result_ty) {
                    self.local_structs.insert(place.local, struct_def);
                }

                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating method `{}` as opaque runtime stub during MIR lowering",
                        method_name
                    ),
                );
                return Ok(());
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
                self.lowering.emit_warning(
                    expr.span,
                    format!(
                        "treating expression {:?} as unit during MIR assignment",
                        expr.kind
                    ),
                );
                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
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
            TyKind::Slice(elem) => Some(*elem.clone()),
            TyKind::Ref(_, elem, _) => match &elem.kind {
                TyKind::Array(inner, _) => Some(*inner.clone()),
                TyKind::Slice(inner) => Some(*inner.clone()),
                _ => None,
            },
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

// END ORIGINAL CONTENT
