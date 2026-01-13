// HIR→MIR lowering implementation (moved from mod.rs)
// This file currently contains the full original implementation and will be
// gradually split into stmt/control_flow/types/borrow submodules.

// BEGIN ORIGINAL CONTENT
use fp_core::ast::{
    DecimalType, TypeBinaryOpKind, TypeInt, TypePrimitive, Value, ValueList, ValueMap, ValueTuple,
};
use fp_core::diagnostics::Diagnostic;
use fp_core::error::Result;
use fp_core::hir;

fn call_arg_values(args: &[hir::CallArg]) -> Vec<&hir::Expr> {
    args.iter().map(|arg| &arg.value).collect()
}
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::mir::ty::{
    AdtDef, AdtFlags, ConstKind, ConstValue, CtorKind, ErrorGuaranteed, FloatTy, IntTy, Mutability,
    ReprFlags, ReprOptions, Scalar, ScalarInt, Ty, TyKind, TypeAndMut, UintTy, VariantDef,
    VariantDiscr,
};
use fp_core::mir::{self, Symbol};
use fp_core::ops::format_value_with_spec;
use fp_core::span::Span;
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet};
use std::hash::{Hash, Hasher};

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
    payload_def: Option<hir::DefId>,
}

#[derive(Clone, Debug)]
struct MethodContext {
    def_id: Option<hir::DefId>,
    path: Vec<hir::PathSegment>,
    mir_self_ty: Ty,
}

#[derive(Clone, Debug)]
struct StructDefinition {
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
struct StructuralLayoutKey {
    fields: Vec<(String, Ty)>,
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

#[derive(Clone)]
enum ConstContainerArgs {
    List { elem_ty: Ty },
    Map { key_ty: Ty, value_ty: Ty },
}

pub struct MirLowering {
    next_mir_id: mir::MirId,
    next_body_id: u32,
    next_error_id: u32,
    next_synthetic_def_id: mir::ty::DefId,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    struct_defs: HashMap<hir::DefId, StructDefinition>,
    struct_defs_by_name: HashMap<String, hir::DefId>,
    struct_layouts: HashMap<StructLayoutKey, StructLayout>,
    struct_layouts_by_ty: HashMap<Ty, StructLayoutKey>,
    structural_defs: HashMap<StructuralLayoutKey, hir::DefId>,
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
    next_synthetic_hir_def_id: hir::DefId,
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
        map.insert(
            "fp_panic".to_string(),
            mir::FunctionSig {
                inputs: vec![Ty {
                    kind: TyKind::RawPtr(TypeAndMut {
                        ty: Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        }),
                        mutbl: Mutability::Not,
                    }),
                }],
                output: Ty {
                    kind: TyKind::Tuple(Vec::new()),
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
            struct_defs_by_name: HashMap::new(),
            struct_layouts: HashMap::new(),
            struct_layouts_by_ty: HashMap::new(),
            structural_defs: HashMap::new(),
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
            next_synthetic_hir_def_id: 1,
        }
    }

    pub fn transform(&mut self, hir_program: hir::Program) -> Result<mir::Program> {
        self.lower_program(&hir_program)
    }

    pub fn set_error_tolerance(&mut self, enabled: bool) {
        self.tolerate_errors = enabled;
    }

    fn lower_program(&mut self, program: &hir::Program) -> Result<mir::Program> {
        let mut mir_program = mir::Program::new();
        self.next_synthetic_hir_def_id = program
            .items
            .iter()
            .map(|item| item.def_id)
            .max()
            .unwrap_or(0)
            .saturating_add(1);

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id, def, item.span);
                }
                _ => {}
            }
        }

        for item in &program.items {
            if let hir::ItemKind::Const(const_item) = &item.kind {
                self.register_const_value(program, item.def_id, const_item);
            }
        }

        for item in &program.items {
            match &item.kind {
                hir::ItemKind::Struct(def) => {
                    self.register_struct(item.def_id, def, item.span);
                }
                hir::ItemKind::Enum(def) => {
                    self.register_enum(item.def_id, def, item.span);
                }
                hir::ItemKind::Const(const_item) => {
                    let ty = self.lower_type_expr(&const_item.ty);
                    if Self::is_unit_ty(&ty) {
                        // Unit consts don't need a static allocation; keep them as inline constants.
                        self.register_const_value(program, item.def_id, const_item);
                        continue;
                    }
                    let mir_item = self.lower_const(program, item.def_id, const_item)?;
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
        matches!(name, "printf" | "fp_panic")
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

        let mir_body =
            BodyBuilder::new(self, program, function, sig, span, None, substs).lower()?;

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
        let substs = self.build_substs_from_args(
            &generics,
            &function.sig.inputs,
            explicit_args,
            arg_types,
            span,
        )?;
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

        let substs = self.build_substs_from_args(
            &generics,
            &def.function.sig.inputs,
            explicit_args,
            arg_types,
            span,
        )?;
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

        let sig = self.lower_function_sig_with_substs(
            &def.function.sig,
            method_context.as_ref(),
            &substs,
        );
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
                    self.lower_type_expr_with_context_and_substs(&param.ty, method_context, substs)
                })
                .collect(),
            output: self.lower_type_expr_with_context_and_substs(
                &sig.output,
                method_context,
                substs,
            ),
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
            self.infer_generic_from_type_expr(&param.ty, actual_ty, generics, &mut substs, span)?;
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
                    .last()
                    .map(|seg| seg.name.as_str())
                    .unwrap_or("");
                let is_generic = path.segments.iter().all(|seg| seg.args.is_none())
                    && generics.iter().any(|g| g == name);
                if is_generic {
                    let actual_is_opaque = self.is_opaque_ty(actual_ty);
                    if let Some(existing) = substs.get(name) {
                        if existing != actual_ty {
                            let existing_is_opaque = self.is_opaque_ty(existing);
                            if existing_is_opaque && !actual_is_opaque {
                                substs.insert(name.to_string(), actual_ty.clone());
                                return Ok(());
                            }
                            if actual_is_opaque {
                                return Ok(());
                            }
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
                        if actual_is_opaque {
                            substs.insert(name.to_string(), actual_ty.clone());
                            return Ok(());
                        }
                        substs.insert(name.to_string(), actual_ty.clone());
                    }
                    return Ok(());
                }

                if let Some(path_args) = path.segments.last().and_then(|seg| seg.args.as_ref()) {
                    if let TyKind::Adt(_, adt_substs) = &actual_ty.kind {
                        for (arg, subst) in path_args.args.iter().zip(adt_substs.iter()) {
                            let mir::ty::GenericArg::Type(actual_arg_ty) = subst else {
                                continue;
                            };
                            if let hir::GenericArg::Type(type_arg) = arg {
                                self.infer_generic_from_type_expr(
                                    type_arg.as_ref(),
                                    actual_arg_ty,
                                    generics,
                                    substs,
                                    span,
                                )?;
                            }
                        }
                    }
                }

                if let Some(hir::Res::Def(def_id)) = path.res.as_ref() {
                    if let Some(struct_def) = self.struct_defs.get(def_id).cloned() {
                        if let TyKind::Tuple(elements) = &actual_ty.kind {
                            for (field, actual_field_ty) in
                                struct_def.fields.iter().zip(elements.iter())
                            {
                                self.infer_generic_from_type_expr(
                                    &field.ty,
                                    actual_field_ty.as_ref(),
                                    generics,
                                    substs,
                                    span,
                                )?;
                            }
                        }
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
                } else {
                    // HIR does not preserve explicit references, so allow inference from the
                    // underlying value type when a ref is expected.
                    self.infer_generic_from_type_expr(inner, actual_ty, generics, substs, span)?;
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
            hir::TypeExprKind::Tuple(items) => {
                if let TyKind::Tuple(actual_items) = &actual_ty.kind {
                    for (item, actual_item) in items.iter().zip(actual_items.iter()) {
                        self.infer_generic_from_type_expr(
                            item,
                            actual_item.as_ref(),
                            generics,
                            substs,
                            span,
                        )?;
                    }
                }
            }
            hir::TypeExprKind::Array(inner, _) => {
                if let TyKind::Array(actual_inner, _) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::Slice(inner) => {
                if let TyKind::Slice(actual_inner) = &actual_ty.kind {
                    self.infer_generic_from_type_expr(
                        inner,
                        actual_inner.as_ref(),
                        generics,
                        substs,
                        span,
                    )?;
                }
            }
            hir::TypeExprKind::FnPtr(fn_ptr) => {
                match &actual_ty.kind {
                    TyKind::FnPtr(poly_sig) => {
                        let sig = &poly_sig.binder.value;
                        if fn_ptr.inputs.len() != sig.inputs.len() {
                            return Ok(());
                        }
                        for (expected, actual) in fn_ptr.inputs.iter().zip(sig.inputs.iter()) {
                            self.infer_generic_from_type_expr(
                                expected,
                                actual.as_ref(),
                                generics,
                                substs,
                                span,
                            )?;
                        }
                        self.infer_generic_from_type_expr(
                            &fn_ptr.output,
                            sig.output.as_ref(),
                            generics,
                            substs,
                            span,
                        )?;
                        return Ok(());
                    }
                    TyKind::FnDef(def_id, _) => {
                        let sig = match self.function_sigs.get(def_id).cloned() {
                            Some(sig) => sig,
                            None => return Ok(()),
                        };
                        if fn_ptr.inputs.len() != sig.inputs.len() {
                            return Ok(());
                        }
                        for (expected, actual) in fn_ptr.inputs.iter().zip(sig.inputs.iter()) {
                            self.infer_generic_from_type_expr(
                                expected, actual, generics, substs, span,
                            )?;
                        }
                        self.infer_generic_from_type_expr(
                            &fn_ptr.output,
                            &sig.output,
                            generics,
                            substs,
                            span,
                        )?;
                        return Ok(());
                    }
                    _ => {}
                };
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
                if self.is_string_slice_ref(inner) {
                    return self.raw_string_ptr_ty();
                }
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

    fn lower_const(
        &mut self,
        program: &hir::Program,
        def_id: hir::DefId,
        konst: &hir::Const,
    ) -> Result<mir::Item> {
        let ty = self.lower_type_expr(&konst.ty);
        let container_args = self.container_args_from_type_expr(&konst.ty);
        let init_constant = self
            .lower_const_expr(
                program,
                &konst.body.value,
                Some(&ty),
                container_args.as_ref(),
            )
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
            hir::TypeExprKind::Structural(structural) => {
                self.lower_structural_type_expr(structural, ty_expr.span)
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => {
                self.lower_type_binary_op_expr(type_op, ty_expr.span)
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
                if self.is_string_slice_ref(inner) {
                    return self.raw_string_ptr_ty();
                }
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

    fn next_synthetic_def_id(&mut self) -> hir::DefId {
        let id = self.next_synthetic_hir_def_id;
        self.next_synthetic_hir_def_id = self.next_synthetic_hir_def_id.saturating_add(1);
        id
    }

    fn lower_structural_type_expr(&mut self, structural: &hir::TypeStructural, span: Span) -> Ty {
        let mut entries_ty: Option<&hir::TypeExpr> = None;
        if structural.fields.len() == 1 {
            if let Some(field) = structural.fields.first() {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                }
            }
        } else {
            for field in &structural.fields {
                if field.name.as_str() == "entries" {
                    entries_ty = Some(field.ty.as_ref());
                    break;
                }
            }
        }

        if let Some(entries_ty) = entries_ty {
            let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
            match &entries_ty.kind {
                hir::TypeExprKind::Path(path) => {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Vec" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = Some(inner.as_ref());
                                    }
                                }
                            }
                        }
                    }
                }
                hir::TypeExprKind::Slice(inner) => {
                    entry_ty_expr = Some(inner.as_ref());
                }
                _ => {}
            }

            if let Some(mut entry_ty_expr) = entry_ty_expr {
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    if let Some(tail) = path.segments.last() {
                        if tail.name.as_str() == "Expr" {
                            if let Some(args) = &tail.args {
                                if args.args.len() == 1 {
                                    if let hir::GenericArg::Type(inner) = &args.args[0] {
                                        entry_ty_expr = inner.as_ref();
                                    }
                                }
                            }
                        }
                    }
                }

                let mut key_ty_expr = None;
                let mut value_ty_expr = None;
                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        if let Some(tail) = path.segments.last() {
                            if tail.name.as_str() == "HashMapEntry" {
                                if let Some(args) = &tail.args {
                                    if args.args.len() == 2 {
                                        if let (
                                            hir::GenericArg::Type(key),
                                            hir::GenericArg::Type(value),
                                        ) = (&args.args[0], &args.args[1])
                                        {
                                            key_ty_expr = Some(key.as_ref());
                                            value_ty_expr = Some(value.as_ref());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            key_ty_expr = Some(fields[0].as_ref());
                            value_ty_expr = Some(fields[1].as_ref());
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }

                if let (Some(key_ty_expr), Some(value_ty_expr)) = (key_ty_expr, value_ty_expr) {
                    let key_ty = self.lower_type_expr(key_ty_expr);
                    let value_ty = self.lower_type_expr(value_ty_expr);
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Tuple(vec![Box::new(key_ty), Box::new(value_ty)]),
                        })),
                    };
                }
            }
        }

        let mut fields = Vec::with_capacity(structural.fields.len());
        for field in &structural.fields {
            fields.push(StructFieldDef {
                name: field.name.as_str().to_string(),
                ty: (*field.ty).clone(),
            });
        }

        let key_fields = fields
            .iter()
            .map(|field| (field.name.clone(), self.lower_type_expr(&field.ty)))
            .collect::<Vec<_>>();
        let key = StructuralLayoutKey { fields: key_fields };

        let def_id = if let Some(def_id) = self.structural_defs.get(&key).copied() {
            def_id
        } else {
            let def_id = self.next_synthetic_def_id();
            let mut field_index = HashMap::new();
            for (idx, field) in fields.iter().enumerate() {
                if field_index.insert(field.name.clone(), idx).is_some() {
                    self.emit_error(span, format!("duplicate structural field `{}`", field.name));
                }
            }

            self.struct_defs.insert(
                def_id,
                StructDefinition {
                    name: format!("__structural_{}", def_id),
                    generics: Vec::new(),
                    fields: fields.clone(),
                    field_index,
                },
            );
            self.structural_defs.insert(key, def_id);
            def_id
        };

        self.struct_layout_for_instance(def_id, &[], span)
            .map(|layout| layout.ty)
            .unwrap_or_else(|| self.error_ty())
    }

    fn lower_type_binary_op_expr(&mut self, type_op: &hir::TypeBinaryOp, span: Span) -> Ty {
        match type_op.kind {
            TypeBinaryOpKind::Union => self.lower_union_type_expr(&type_op.lhs, &type_op.rhs, span),
            TypeBinaryOpKind::Add | TypeBinaryOpKind::Intersect | TypeBinaryOpKind::Subtract => {
                let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span);
                let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span);
                let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                    self.emit_error(
                        span,
                        "type arithmetic requires structural or named struct operands",
                    );
                    return self.error_ty();
                };

                let combined = match type_op.kind {
                    TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Intersect => self.intersect_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Subtract => self.subtract_structural_fields(span, lhs, rhs),
                    TypeBinaryOpKind::Union => unreachable!("union handled above"),
                };
                let fields = combined
                    .into_iter()
                    .map(|field| hir::TypeStructuralField {
                        name: hir::Symbol::new(field.name),
                        ty: Box::new(field.ty),
                    })
                    .collect::<Vec<_>>();
                self.lower_structural_type_expr(&hir::TypeStructural { fields }, span)
            }
        }
    }

    fn structural_fields_for_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
        span: Span,
    ) -> Option<Vec<StructFieldDef>> {
        match &ty_expr.kind {
            hir::TypeExprKind::Structural(structural) => Some(
                structural
                    .fields
                    .iter()
                    .map(|field| StructFieldDef {
                        name: field.name.as_str().to_string(),
                        ty: (*field.ty).clone(),
                    })
                    .collect(),
            ),
            hir::TypeExprKind::Path(path) => {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if let Some(def) = self.struct_defs.get(def_id) {
                        return Some(def.fields.clone());
                    }
                }
                self.emit_error(
                    span,
                    "type arithmetic requires struct operands with known definitions",
                );
                None
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => match type_op.kind {
                TypeBinaryOpKind::Add
                | TypeBinaryOpKind::Intersect
                | TypeBinaryOpKind::Subtract => {
                    let lhs = self.structural_fields_for_type_expr(&type_op.lhs, span)?;
                    let rhs = self.structural_fields_for_type_expr(&type_op.rhs, span)?;
                    Some(match type_op.kind {
                        TypeBinaryOpKind::Add => self.merge_structural_fields(span, lhs, rhs),
                        TypeBinaryOpKind::Intersect => {
                            self.intersect_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Subtract => {
                            self.subtract_structural_fields(span, lhs, rhs)
                        }
                        TypeBinaryOpKind::Union => unreachable!("union handled separately"),
                    })
                }
                TypeBinaryOpKind::Union => None,
            },
            _ => None,
        }
    }

    fn merge_structural_fields(
        &mut self,
        span: Span,
        mut lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        for rhs_field in rhs {
            if let Some(existing) = lhs.iter().find(|field| field.name == rhs_field.name) {
                if !self.type_exprs_equivalent(&existing.ty, &rhs_field.ty) {
                    self.emit_error(
                        span,
                        format!(
                            "conflicting field types for `{}` in structural merge",
                            rhs_field.name
                        ),
                    );
                }
                continue;
            }
            lhs.push(rhs_field);
        }
        lhs
    }

    fn intersect_structural_fields(
        &mut self,
        span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter_map(|field| {
                rhs.iter()
                    .find(|rhs_field| rhs_field.name == field.name)
                    .map(|rhs_field| {
                        if !self.type_exprs_equivalent(&rhs_field.ty, &field.ty) {
                            self.emit_error(
                                span,
                                format!(
                                    "conflicting field types for `{}` in structural intersect",
                                    field.name
                                ),
                            );
                        }
                        field.clone()
                    })
            })
            .collect()
    }

    fn subtract_structural_fields(
        &mut self,
        _span: Span,
        lhs: Vec<StructFieldDef>,
        rhs: Vec<StructFieldDef>,
    ) -> Vec<StructFieldDef> {
        lhs.into_iter()
            .filter(|field| !rhs.iter().any(|rhs_field| rhs_field.name == field.name))
            .collect()
    }

    fn type_exprs_equivalent(&self, lhs: &hir::TypeExpr, rhs: &hir::TypeExpr) -> bool {
        match (&lhs.kind, &rhs.kind) {
            (hir::TypeExprKind::Primitive(a), hir::TypeExprKind::Primitive(b)) => a == b,
            (hir::TypeExprKind::Path(a), hir::TypeExprKind::Path(b)) => {
                if a.segments.len() != b.segments.len() {
                    return false;
                }
                for (a_seg, b_seg) in a.segments.iter().zip(b.segments.iter()) {
                    if a_seg.name != b_seg.name {
                        return false;
                    }
                    match (&a_seg.args, &b_seg.args) {
                        (None, None) => {}
                        (Some(a_args), Some(b_args)) => {
                            if a_args.args.len() != b_args.args.len() {
                                return false;
                            }
                            for (a_arg, b_arg) in a_args.args.iter().zip(b_args.args.iter()) {
                                match (a_arg, b_arg) {
                                    (hir::GenericArg::Type(a_ty), hir::GenericArg::Type(b_ty)) => {
                                        if !self.type_exprs_equivalent(a_ty, b_ty) {
                                            return false;
                                        }
                                    }
                                    (hir::GenericArg::Const(_), hir::GenericArg::Const(_)) => {}
                                    _ => return false,
                                }
                            }
                        }
                        _ => return false,
                    }
                }
                true
            }
            (hir::TypeExprKind::Structural(a), hir::TypeExprKind::Structural(b)) => {
                if a.fields.len() != b.fields.len() {
                    return false;
                }
                for (a_field, b_field) in a.fields.iter().zip(b.fields.iter()) {
                    if a_field.name != b_field.name {
                        return false;
                    }
                    if !self.type_exprs_equivalent(&a_field.ty, &b_field.ty) {
                        return false;
                    }
                }
                true
            }
            (hir::TypeExprKind::TypeBinaryOp(a), hir::TypeExprKind::TypeBinaryOp(b)) => {
                a.kind == b.kind
                    && self.type_exprs_equivalent(&a.lhs, &b.lhs)
                    && self.type_exprs_equivalent(&a.rhs, &b.rhs)
            }
            (hir::TypeExprKind::Tuple(a), hir::TypeExprKind::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter()
                    .zip(b.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
            }
            (hir::TypeExprKind::Array(a_elem, _), hir::TypeExprKind::Array(b_elem, _)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Slice(a_elem), hir::TypeExprKind::Slice(b_elem)) => {
                self.type_exprs_equivalent(a_elem, b_elem)
            }
            (hir::TypeExprKind::Ptr(a), hir::TypeExprKind::Ptr(b)) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::Ref(a), hir::TypeExprKind::Ref(b)) => {
                self.type_exprs_equivalent(a, b)
            }
            (hir::TypeExprKind::FnPtr(a), hir::TypeExprKind::FnPtr(b)) => {
                if a.inputs.len() != b.inputs.len() {
                    return false;
                }
                if !a
                    .inputs
                    .iter()
                    .zip(b.inputs.iter())
                    .all(|(a_ty, b_ty)| self.type_exprs_equivalent(a_ty, b_ty))
                {
                    return false;
                }
                self.type_exprs_equivalent(&a.output, &b.output)
            }
            (hir::TypeExprKind::Never, hir::TypeExprKind::Never) => true,
            (hir::TypeExprKind::Infer, hir::TypeExprKind::Infer) => true,
            (hir::TypeExprKind::Error, hir::TypeExprKind::Error) => true,
            _ => false,
        }
    }

    fn lower_union_type_expr(
        &mut self,
        lhs: &hir::TypeExpr,
        rhs: &hir::TypeExpr,
        span: Span,
    ) -> Ty {
        let def_id = self.next_synthetic_def_id();
        let enum_name = format!("__union_{}", def_id);

        let lhs_name = self.union_variant_name(lhs, "Left");
        let mut rhs_name = self.union_variant_name(rhs, "Right");
        if lhs_name == rhs_name {
            rhs_name = format!("{}_rhs", rhs_name);
        }

        let lhs_payload = match lhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(lhs) => None,
            _ => Some(lhs.clone()),
        };
        let rhs_payload = match rhs.kind {
            hir::TypeExprKind::Infer | hir::TypeExprKind::Error => None,
            _ if self.is_null_type_expr(rhs) => None,
            _ => Some(rhs.clone()),
        };

        let variants = vec![
            EnumVariantDef {
                def_id: self.next_synthetic_def_id(),
                name: lhs_name,
                discriminant: 0,
                payload: lhs_payload,
            },
            EnumVariantDef {
                def_id: self.next_synthetic_def_id(),
                name: rhs_name,
                discriminant: 1,
                payload: rhs_payload,
            },
        ];

        self.register_synthetic_enum(def_id, enum_name, variants, span);

        self.enum_layout_for_instance(def_id, &[], span)
            .map(|layout| layout.enum_ty)
            .unwrap_or_else(|| self.error_ty())
    }

    fn union_variant_name(&self, ty_expr: &hir::TypeExpr, fallback: &str) -> String {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str().to_string())
                .filter(|name| !name.is_empty())
                .unwrap_or_else(|| fallback.to_string()),
            hir::TypeExprKind::Structural(structural) => {
                let mut matches = self
                    .struct_defs
                    .values()
                    .filter(|def| def.fields.len() == structural.fields.len())
                    .filter(|def| {
                        def.fields.iter().zip(structural.fields.iter()).all(
                            |(def_field, struct_field)| {
                                def_field.name == struct_field.name.as_str()
                                    && self.type_exprs_equivalent(&def_field.ty, &struct_field.ty)
                            },
                        )
                    })
                    .map(|def| def.name.clone())
                    .collect::<Vec<_>>();
                if let Some(name) = matches
                    .iter()
                    .find(|name| !name.starts_with("__structural_"))
                {
                    return name.clone();
                }
                matches.pop().unwrap_or_else(|| fallback.to_string())
            }
            _ => fallback.to_string(),
        }
    }

    fn is_null_type_expr(&self, ty_expr: &hir::TypeExpr) -> bool {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str() == "null")
                .unwrap_or(false),
            _ => false,
        }
    }

    fn register_synthetic_enum(
        &mut self,
        def_id: hir::DefId,
        name: String,
        variants: Vec<EnumVariantDef>,
        span: Span,
    ) {
        if self.enum_defs.contains_key(&def_id) {
            return;
        }

        for variant in &variants {
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(*def_id);
                    }
                }
                None
            });
            self.enum_variants.insert(
                variant.def_id,
                EnumVariantInfo {
                    def_id: variant.def_id,
                    enum_def: def_id,
                    discriminant: variant.discriminant,
                    payload_def,
                },
            );

            let qualified_name = format!("{}::{}", name, variant.name);
            self.enum_variant_names
                .insert(qualified_name.clone(), variant.def_id);
            self.enum_variant_names
                .entry(variant.name.clone())
                .or_insert(variant.def_id);
        }

        self.enum_defs.insert(
            def_id,
            EnumDefinition {
                def_id,
                name,
                generics: Vec::new(),
                variants,
            },
        );

        let _ = self.enum_layout_for_instance(def_id, &[], span);
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
            TypePrimitive::String => Ty {
                kind: TyKind::RawPtr(TypeAndMut {
                    ty: Box::new(Ty {
                        kind: TyKind::Int(IntTy::I8),
                    }),
                    mutbl: Mutability::Not,
                }),
            },
            TypePrimitive::List => {
                self.emit_warning(
                    span,
                    "treating list primitive as opaque type during MIR lowering",
                );
                self.opaque_ty("list")
            }
        }
    }

    fn resolve_path_def_id(&self, path: &hir::Path) -> Option<hir::DefId> {
        if let Some(hir::Res::Def(def_id)) = path.res {
            return Some(def_id);
        }
        let full = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        let resolved = self.struct_defs_by_name.get(&full).copied().or_else(|| {
            self.enum_defs
                .values()
                .find(|def| def.name == full)
                .map(|def| def.def_id)
        });
        if resolved.is_some() {
            return resolved;
        }
        let tail = path.segments.last()?.name.as_str();
        let struct_matches: Vec<hir::DefId> = self
            .struct_defs
            .iter()
            .filter_map(|(def_id, def)| {
                if def.name == tail || def.name.ends_with(&format!("::{}", tail)) {
                    Some(*def_id)
                } else {
                    None
                }
            })
            .collect();
        if struct_matches.len() == 1 {
            return struct_matches.into_iter().next();
        }
        let enum_matches: Vec<hir::DefId> = self
            .enum_defs
            .iter()
            .filter_map(|(def_id, def)| {
                if def.name == tail || def.name.ends_with(&format!("::{}", tail)) {
                    Some(*def_id)
                } else {
                    None
                }
            })
            .collect();
        if enum_matches.len() == 1 {
            return enum_matches.into_iter().next();
        }
        None
    }

    fn lower_path_type(&mut self, path: &hir::Path, span: Span) -> Ty {
        if let Some(def_id) = self.resolve_path_def_id(path) {
            if self.struct_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.struct_layout_for_instance(def_id, &args, span) {
                    return layout.ty.clone();
                }
                return self.error_ty();
            }
            if self.enum_defs.contains_key(&def_id) {
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.enum_layout_for_instance(def_id, &args, span) {
                    return layout.enum_ty.clone();
                }
                return self.error_ty();
            }
            if let Some(sig) = self.function_sigs.get(&def_id) {
                return Ty {
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
                };
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.as_str();
            if name == "Vec" || name == "List" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(elem_ty) = args.first().cloned() {
                    return Ty {
                        kind: TyKind::Slice(Box::new(elem_ty)),
                    };
                }
                self.emit_error(span, "Vec/List requires a single type argument");
                return self.error_ty();
            }
            if name == "HashMap" {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if args.len() == 2 {
                    let entry_ty = Ty {
                        kind: TyKind::Tuple(vec![
                            Box::new(args[0].clone()),
                            Box::new(args[1].clone()),
                        ]),
                    };
                    return Ty {
                        kind: TyKind::Slice(Box::new(entry_ty)),
                    };
                }
                self.emit_error(span, "HashMap requires two type arguments");
                return self.error_ty();
            }
        }

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
            let tail = segment.name.as_str();
            let struct_matches: Vec<hir::DefId> = self
                .struct_defs
                .iter()
                .filter_map(|(def_id, def)| {
                    if def.name == tail || def.name.ends_with(&format!("::{}", tail)) {
                        Some(*def_id)
                    } else {
                        None
                    }
                })
                .collect();
            if struct_matches.len() == 1 {
                let def_id = struct_matches[0];
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.struct_layout_for_instance(def_id, &args, span) {
                    return layout.ty.clone();
                }
                return self.error_ty();
            }

            let enum_matches: Vec<hir::DefId> = self
                .enum_defs
                .iter()
                .filter_map(|(def_id, def)| {
                    if def.name == tail || def.name.ends_with(&format!("::{}", tail)) {
                        Some(*def_id)
                    } else {
                        None
                    }
                })
                .collect();
            if enum_matches.len() == 1 {
                let def_id = enum_matches[0];
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.enum_layout_for_instance(def_id, &args, span) {
                    return layout.enum_ty.clone();
                }
                return self.error_ty();
            }
        }

        if let Some(segment) = path.segments.last() {
            let name = segment.name.clone();
            match name.as_str() {
                "i8" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I8),
                    };
                }
                "i16" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I16),
                    };
                }
                "i32" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I32),
                    };
                }
                "i64" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I64),
                    };
                }
                "i128" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::I128),
                    };
                }
                "usize" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                }
                "isize" => {
                    return Ty {
                        kind: TyKind::Int(IntTy::Isize),
                    };
                }
                "u8" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U8),
                    };
                }
                "u16" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U16),
                    };
                }
                "u32" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U32),
                    };
                }
                "u64" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    };
                }
                "u128" => {
                    return Ty {
                        kind: TyKind::Uint(UintTy::U128),
                    };
                }
                "bool" => return Ty { kind: TyKind::Bool },
                "char" => return Ty { kind: TyKind::Char },
                "f32" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F32),
                    };
                }
                "f64" => {
                    return Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    };
                }
                "str" => {
                    return Ty {
                        kind: TyKind::Slice(Box::new(Ty {
                            kind: TyKind::Int(IntTy::I8),
                        })),
                    };
                }
                "null" => {
                    return self.raw_string_ptr_ty();
                }
                _ => {}
            }

            if let Some(def_id) = self.struct_defs_by_name.get(name.as_str()).copied() {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.struct_layout_for_instance(def_id, &args, span) {
                    return layout.ty.clone();
                }
                return self.error_ty();
            }
            if let Some(def) = self
                .enum_defs
                .values()
                .find(|def| def.name.as_str() == name.as_str())
                .cloned()
            {
                let args = segment
                    .args
                    .as_ref()
                    .map(|args| self.lower_generic_args(Some(args), span))
                    .unwrap_or_default();
                if let Some(layout) = self.enum_layout_for_instance(def.def_id, &args, span) {
                    return layout.enum_ty.clone();
                }
                return self.error_ty();
            }
        }

        let display = path
            .segments
            .iter()
            .map(|seg| seg.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
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
                name: String::from(strukt.name.clone()),
                generics,
                fields,
                field_index,
            },
        );
        self.struct_defs_by_name
            .insert(String::from(strukt.name.clone()), def_id);

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
            let payload_def = variant.payload.as_ref().and_then(|payload| {
                if let hir::TypeExprKind::Path(path) = &payload.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        return Some(*def_id);
                    }
                }
                None
            });
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
                    payload_def,
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
        let is_union_enum = enum_def.name.starts_with("__union_");

        for variant in &enum_def.variants {
            let payload_tys = if is_union_enum {
                if let Some(payload) = variant.payload.as_ref() {
                    let payload_ty = self.lower_type_expr_with_substs(payload, &substs);
                    if let TyKind::Adt(adt, _) = &payload_ty.kind {
                        let _ = self.struct_layout_for_instance(adt.did, &[], span);
                    }
                    if let Some(layout) = self.struct_layout_for_ty(&payload_ty) {
                        layout.field_tys.clone()
                    } else {
                        self.enum_payload_types_from_ty(&payload_ty)
                    }
                } else {
                    Vec::new()
                }
            } else {
                self.enum_payload_types(&variant.payload, &substs)
            };
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
                    self.emit_warning(span, "const generics are ignored during MIR lowering");
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
            hir::TypeExprKind::Structural(structural) => {
                self.lower_structural_type_expr(structural, ty_expr.span)
            }
            hir::TypeExprKind::TypeBinaryOp(type_op) => {
                self.lower_type_binary_op_expr(type_op, ty_expr.span)
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
                if self.is_string_slice_ref(inner) {
                    return self.raw_string_ptr_ty();
                }
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
            hir::TypeExprKind::Never => Ty {
                kind: TyKind::Never,
            },
            hir::TypeExprKind::Infer => self.error_ty(),
            hir::TypeExprKind::Error => self.error_ty(),
        }
    }

    fn raw_string_ptr_ty(&self) -> Ty {
        Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        }
    }

    fn is_string_slice_ref(&self, inner: &hir::TypeExpr) -> bool {
        match &inner.kind {
            hir::TypeExprKind::Primitive(TypePrimitive::String) => true,
            hir::TypeExprKind::Path(path) => path
                .segments
                .last()
                .map(|seg| seg.name.as_str() == "str")
                .unwrap_or(false),
            _ => false,
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

    fn register_const_value(
        &mut self,
        program: &hir::Program,
        def_id: hir::DefId,
        konst: &hir::Const,
    ) {
        if self.const_values.contains_key(&def_id) {
            return;
        }

        let ty = self.lower_type_expr(&konst.ty);
        let container_args = self.container_args_from_type_expr(&konst.ty);
        if let Some(constant) = self.lower_const_expr(
            program,
            &konst.body.value,
            Some(&ty),
            container_args.as_ref(),
        ) {
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
                    let method_name = function.sig.name.as_str();
                    let is_hashmap_impl = struct_name.ends_with("HashMap");
                    let is_hashmap_method = matches!(method_name, "from" | "len" | "get_unchecked")
                        || method_name.ends_with("::from")
                        || method_name.ends_with("::len")
                        || method_name.ends_with("::get_unchecked");
                    if is_hashmap_impl && is_hashmap_method {
                        continue;
                    }
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

                    self.method_lookup.insert(fn_name.clone(), info.clone());
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
        &mut self,
        def_id: hir::DefId,
        struct_ty: &Ty,
        name: &str,
        span: Span,
    ) -> Option<(usize, StructFieldInfo)> {
        let def = self.struct_defs.get(&def_id)?;
        let idx = *def.field_index.get(name)?;
        let layout = self.struct_layout_for_ty(struct_ty).or_else(|| {
            if self.is_opaque_ty(struct_ty) {
                self.struct_layout_for_instance(def_id, &[], span)
            } else {
                None
            }
        })?;
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

    fn lower_const_expr(
        &mut self,
        program: &hir::Program,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
        container_args: Option<&ConstContainerArgs>,
    ) -> Option<mir::Constant> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(mir::Constant {
                span: expr.span,
                user_ty: None,
                literal: self.lower_literal(lit),
            }),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_expr(program, inner, expected_ty, container_args);
                }
                let ty = expected_ty.cloned().unwrap_or_else(Self::unit_ty);
                Some(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Unit, ty),
                })
            }
            hir::ExprKind::Array(elements) => {
                if let Some(container_args) = container_args {
                    return self.lower_container_const(
                        program,
                        expr.span,
                        elements,
                        container_args,
                    );
                }
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(
                        program,
                        element,
                        Some(elem_ty.as_ref()),
                    )?);
                }
                let ty = expected_ty.cloned().unwrap_or_else(Self::unit_ty);
                Some(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered), ty),
                })
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                if let Some(container_args) = container_args {
                    return self.lower_container_repeat_const(
                        program,
                        expr.span,
                        elem,
                        len,
                        container_args,
                    );
                }
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(program, elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                let ty = expected_ty.cloned().unwrap_or_else(Self::unit_ty);
                Some(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(mir::ConstValue::Array(lowered), ty),
                })
            }
            hir::ExprKind::Struct(_, _) => {
                let value = self.lower_const_value(program, expr, expected_ty)?;
                let ty = expected_ty.cloned().unwrap_or_else(Self::unit_ty);
                Some(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(value, ty),
                })
            }
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };
                let item = program.def_map.get(def_id)?;
                let hir::ItemKind::Function(function) = &item.kind else {
                    return None;
                };
                let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) = expected_ty.map(|ty| &ty.kind)?
                else {
                    return None;
                };
                let fn_ty = expected_ty.cloned().unwrap_or_else(Self::unit_ty);
                Some(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Fn(
                        mir::Symbol::new(function.sig.name.as_str()),
                        fn_ty,
                    ),
                })
            }
            _ => None,
        }
    }

    fn lower_const_value(
        &mut self,
        program: &hir::Program,
        expr: &hir::Expr,
        expected_ty: Option<&Ty>,
    ) -> Option<mir::ConstValue> {
        match &expr.kind {
            hir::ExprKind::Literal(lit) => Some(self.const_value_from_lit(lit)),
            hir::ExprKind::Block(block) if block.stmts.is_empty() => {
                if let Some(inner) = &block.expr {
                    return self.lower_const_value(program, inner, expected_ty);
                }
                Some(mir::ConstValue::Unit)
            }
            hir::ExprKind::Array(elements) => {
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(
                        program,
                        element,
                        Some(elem_ty.as_ref()),
                    )?);
                }
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::ArrayRepeat { elem, len } => {
                let repeat_len = self.eval_type_length(len)?;
                let TyKind::Array(elem_ty, _len) = expected_ty.map(|ty| &ty.kind)? else {
                    return None;
                };
                let value = self.lower_const_value(program, elem, Some(elem_ty.as_ref()))?;
                let mut lowered = Vec::with_capacity(repeat_len as usize);
                lowered.resize(repeat_len as usize, value);
                Some(mir::ConstValue::Array(lowered))
            }
            hir::ExprKind::Struct(path, fields) => {
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };
                let struct_def = self.struct_defs.get(def_id)?.clone();
                let args = path
                    .segments
                    .last()
                    .and_then(|segment| segment.args.as_ref())
                    .map(|args| self.lower_generic_args(Some(args), expr.span))
                    .unwrap_or_default();
                let layout = self.struct_layout_for_instance(*def_id, &args, expr.span)?;
                let mut field_map: HashMap<String, &hir::Expr> = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.as_str().to_string(), &field.expr);
                }
                let mut lowered = Vec::with_capacity(struct_def.fields.len());
                for (idx, field_def) in struct_def.fields.iter().enumerate() {
                    let Some(expr) = field_map.get(&field_def.name) else {
                        self.emit_error(
                            expr.span,
                            format!("missing field `{}` in const struct literal", field_def.name),
                        );
                        return None;
                    };
                    let field_ty = layout.field_tys.get(idx)?;
                    lowered.push(self.lower_const_value(program, expr, Some(field_ty))?);
                }
                Some(mir::ConstValue::Struct(lowered))
            }
            hir::ExprKind::Path(path) => {
                let hir::Res::Def(def_id) = path.res.as_ref()? else {
                    return None;
                };
                let item = program.def_map.get(def_id)?;
                let hir::ItemKind::Function(function) = &item.kind else {
                    return None;
                };
                let (TyKind::FnDef(_, _) | TyKind::FnPtr(_)) = expected_ty.map(|ty| &ty.kind)?
                else {
                    return None;
                };
                Some(mir::ConstValue::Fn(mir::Symbol::new(
                    function.sig.name.as_str(),
                )))
            }
            _ => None,
        }
    }

    fn const_value_from_lit(&self, lit: &hir::Lit) -> mir::ConstValue {
        match lit {
            hir::Lit::Bool(value) => mir::ConstValue::Bool(*value),
            hir::Lit::Integer(value) => mir::ConstValue::Int(*value),
            hir::Lit::Float(value) => mir::ConstValue::Float(*value),
            hir::Lit::Str(value) => mir::ConstValue::Str(value.clone()),
            hir::Lit::Char(value) => mir::ConstValue::Int(*value as i64),
            hir::Lit::Null => mir::ConstValue::Null,
        }
    }

    fn lower_container_const(
        &mut self,
        program: &hir::Program,
        span: Span,
        elements: &[hir::Expr],
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_const_value(program, element, Some(elem_ty))?);
                }
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(
                        mir::ConstValue::List {
                            elements: lowered,
                            elem_ty: elem_ty.clone(),
                        },
                        ty,
                    ),
                })
            }
            ConstContainerArgs::Map { key_ty, value_ty } => {
                let mut entries = Vec::with_capacity(elements.len());
                for element in elements {
                    let (key_expr, value_expr) = match &element.kind {
                        hir::ExprKind::Array(pair) if pair.len() == 2 => (&pair[0], &pair[1]),
                        _ => {
                            self.emit_error(
                                span,
                                "HashMap literal expects entries as [key, value]",
                            );
                            return None;
                        }
                    };
                    let key = self.lower_const_value(program, key_expr, Some(key_ty))?;
                    let value = self.lower_const_value(program, value_expr, Some(value_ty))?;
                    entries.push((key, value));
                }
                let entry_ty = Ty {
                    kind: TyKind::Tuple(vec![Box::new(key_ty.clone()), Box::new(value_ty.clone())]),
                };
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(entry_ty)),
                };
                Some(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(
                        mir::ConstValue::Map {
                            entries,
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                        },
                        ty,
                    ),
                })
            }
        }
    }

    fn lower_container_repeat_const(
        &mut self,
        program: &hir::Program,
        span: Span,
        elem: &hir::Expr,
        len: &hir::Expr,
        container_args: &ConstContainerArgs,
    ) -> Option<mir::Constant> {
        match container_args {
            ConstContainerArgs::List { elem_ty } => {
                let repeat_len = self.eval_type_length(len)?;
                let value = self.lower_const_value(program, elem, Some(elem_ty))?;
                let mut elements = Vec::with_capacity(repeat_len as usize);
                elements.resize(repeat_len as usize, value);
                let ty = Ty {
                    kind: TyKind::Slice(Box::new(elem_ty.clone())),
                };
                Some(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Val(
                        mir::ConstValue::List {
                            elements,
                            elem_ty: elem_ty.clone(),
                        },
                        ty,
                    ),
                })
            }
            ConstContainerArgs::Map { .. } => None,
        }
    }

    fn container_args_from_type_expr(
        &mut self,
        ty_expr: &hir::TypeExpr,
    ) -> Option<ConstContainerArgs> {
        match &ty_expr.kind {
            hir::TypeExprKind::Path(path) => {
                let tail = path.segments.last()?;
                let args = tail.args.as_ref()?;
                match tail.name.as_str() {
                    "Vec" if args.args.len() == 1 => {
                        let hir::GenericArg::Type(elem) = &args.args[0] else {
                            return None;
                        };
                        let elem_ty = self.lower_type_expr(elem.as_ref());
                        Some(ConstContainerArgs::List { elem_ty })
                    }
                    "HashMap" if args.args.len() == 2 => {
                        let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                            (&args.args[0], &args.args[1])
                        else {
                            return None;
                        };
                        let key_ty = self.lower_type_expr(key.as_ref());
                        let value_ty = self.lower_type_expr(value.as_ref());
                        Some(ConstContainerArgs::Map { key_ty, value_ty })
                    }
                    _ => None,
                }
            }
            hir::TypeExprKind::Slice(elem) => {
                let elem_ty = self.lower_type_expr(elem.as_ref());
                Some(ConstContainerArgs::List { elem_ty })
            }
            hir::TypeExprKind::Structural(structural) => {
                let mut entries_ty: Option<&hir::TypeExpr> = None;
                for field in &structural.fields {
                    if field.name.as_str() == "entries" {
                        entries_ty = Some(field.ty.as_ref());
                        break;
                    }
                }
                let Some(entries_ty) = entries_ty else {
                    return None;
                };
                let mut entry_ty_expr: Option<&hir::TypeExpr> = None;
                match &entries_ty.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments.last()?;
                        if tail.name.as_str() == "Vec" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 1 {
                                if let hir::GenericArg::Type(inner) = &args.args[0] {
                                    entry_ty_expr = Some(inner.as_ref());
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Slice(inner) => {
                        entry_ty_expr = Some(inner.as_ref());
                    }
                    _ => {}
                }

                let Some(mut entry_ty_expr) = entry_ty_expr else {
                    return None;
                };
                if let hir::TypeExprKind::Path(path) = &entry_ty_expr.kind {
                    let tail = path.segments.last()?;
                    if tail.name.as_str() == "Expr" {
                        let args = tail.args.as_ref()?;
                        if args.args.len() == 1 {
                            if let hir::GenericArg::Type(inner) = &args.args[0] {
                                entry_ty_expr = inner.as_ref();
                            }
                        }
                    }
                }

                match &entry_ty_expr.kind {
                    hir::TypeExprKind::Path(path) => {
                        let tail = path.segments.last()?;
                        if tail.name.as_str() == "HashMapEntry" {
                            let args = tail.args.as_ref()?;
                            if args.args.len() == 2 {
                                if let (hir::GenericArg::Type(key), hir::GenericArg::Type(value)) =
                                    (&args.args[0], &args.args[1])
                                {
                                    let key_ty = self.lower_type_expr(key.as_ref());
                                    let value_ty = self.lower_type_expr(value.as_ref());
                                    return Some(ConstContainerArgs::Map { key_ty, value_ty });
                                }
                            }
                        }
                    }
                    hir::TypeExprKind::Tuple(fields) => {
                        if fields.len() == 2 {
                            let key_ty = self.lower_type_expr(fields[0].as_ref());
                            let value_ty = self.lower_type_expr(fields[1].as_ref());
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    hir::TypeExprKind::Structural(structural) => {
                        let mut key_ty_expr = None;
                        let mut value_ty_expr = None;
                        for field in &structural.fields {
                            match field.name.as_str() {
                                "key" => key_ty_expr = Some(field.ty.as_ref()),
                                "value" => value_ty_expr = Some(field.ty.as_ref()),
                                _ => {}
                            }
                        }
                        if let (Some(key_ty_expr), Some(value_ty_expr)) =
                            (key_ty_expr, value_ty_expr)
                        {
                            let key_ty = self.lower_type_expr(key_ty_expr);
                            let value_ty = self.lower_type_expr(value_ty_expr);
                            return Some(ConstContainerArgs::Map { key_ty, value_ty });
                        }
                    }
                    _ => {}
                }

                None
            }
            hir::TypeExprKind::Ref(inner) => self.container_args_from_type_expr(inner.as_ref()),
            _ => None,
        }
    }

    fn const_len_from_constant(&self, constant: &mir::Constant) -> Option<u64> {
        match &constant.literal {
            mir::ConstantKind::Val(mir::ConstValue::List { elements, .. }, _) => {
                Some(elements.len() as u64)
            }
            mir::ConstantKind::Val(mir::ConstValue::Map { entries, .. }, _) => {
                Some(entries.len() as u64)
            }
            _ => None,
        }
    }

    fn const_index_value(
        &mut self,
        program: &hir::Program,
        span: Span,
        constant: &mir::Constant,
        index: &hir::Expr,
    ) -> Option<(mir::Constant, Ty)> {
        let key = self.lower_const_value(program, index, None)?;
        match &constant.literal {
            mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }, _) => {
                let idx = match key {
                    mir::ConstValue::Int(value) if value >= 0 => value as usize,
                    mir::ConstValue::UInt(value) => value as usize,
                    _ => {
                        self.emit_error(span, "list index must be a non-negative integer");
                        return None;
                    }
                };
                let value = elements.get(idx)?;
                let constant = self.const_value_to_constant(span, value, elem_ty);
                Some((constant, elem_ty.clone()))
            }
            mir::ConstantKind::Val(
                mir::ConstValue::Map {
                    entries,
                    key_ty: _,
                    value_ty,
                },
                _,
            ) => {
                let (_, value) = entries
                    .iter()
                    .find(|(entry_key, _)| self.const_value_matches(entry_key, &key))?;
                let constant = self.const_value_to_constant(span, value, value_ty);
                Some((constant, value_ty.clone()))
            }
            _ => None,
        }
    }

    fn const_value_matches(&self, lhs: &mir::ConstValue, rhs: &mir::ConstValue) -> bool {
        match (lhs, rhs) {
            (mir::ConstValue::Int(a), mir::ConstValue::Int(b)) => a == b,
            (mir::ConstValue::UInt(a), mir::ConstValue::UInt(b)) => a == b,
            (mir::ConstValue::Int(a), mir::ConstValue::UInt(b)) => *a >= 0 && *a as u64 == *b,
            (mir::ConstValue::UInt(a), mir::ConstValue::Int(b)) => *b >= 0 && *a == *b as u64,
            (mir::ConstValue::Bool(a), mir::ConstValue::Bool(b)) => a == b,
            (mir::ConstValue::Str(a), mir::ConstValue::Str(b)) => a == b,
            (mir::ConstValue::Null, mir::ConstValue::Null) => true,
            (mir::ConstValue::Fn(a), mir::ConstValue::Fn(b)) => a == b,
            _ => lhs == rhs,
        }
    }

    fn const_value_to_constant(
        &self,
        span: Span,
        value: &mir::ConstValue,
        ty: &Ty,
    ) -> mir::Constant {
        let literal = match value {
            mir::ConstValue::Bool(value) => mir::ConstantKind::Bool(*value),
            mir::ConstValue::Int(value) => mir::ConstantKind::Int(*value),
            mir::ConstValue::UInt(value) => mir::ConstantKind::UInt(*value),
            mir::ConstValue::Float(value) => mir::ConstantKind::Float(*value),
            mir::ConstValue::Str(value) => mir::ConstantKind::Str(value.clone()),
            mir::ConstValue::Null => mir::ConstantKind::Null,
            mir::ConstValue::Fn(name) => mir::ConstantKind::Fn(name.clone(), ty.clone()),
            _ => mir::ConstantKind::Val(value.clone(), ty.clone()),
        };
        mir::Constant {
            span,
            user_ty: None,
            literal,
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

    fn is_opaque_ty(&self, ty: &Ty) -> bool {
        self.display_type_name(ty)
            .map(|name| self.opaque_types.contains_key(&name))
            .unwrap_or(false)
    }

    fn has_unresolved_ty(&self, ty: &Ty) -> bool {
        if self.is_opaque_ty(ty) {
            return true;
        }
        match &ty.kind {
            TyKind::Infer(_)
            | TyKind::Error(_)
            | TyKind::Param(_)
            | TyKind::Placeholder(_)
            | TyKind::Bound(_, _)
            | TyKind::Opaque(_, _)
            | TyKind::Projection(_)
            | TyKind::Dynamic(_, _)
            | TyKind::Generator(_, _, _)
            | TyKind::GeneratorWitness(_)
            | TyKind::Closure(_, _) => true,
            TyKind::Ref(_, inner, _) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.has_unresolved_ty(type_and_mut.ty.as_ref()),
            TyKind::Slice(inner) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::Array(inner, _) => self.has_unresolved_ty(inner.as_ref()),
            TyKind::Tuple(elements) => elements
                .iter()
                .any(|elem| self.has_unresolved_ty(elem.as_ref())),
            TyKind::FnPtr(poly_sig) => {
                let sig = &poly_sig.binder.value;
                sig.inputs
                    .iter()
                    .any(|input| self.has_unresolved_ty(input.as_ref()))
                    || self.has_unresolved_ty(sig.output.as_ref())
            }
            TyKind::Adt(_, substs) => substs.iter().any(|arg| {
                matches!(arg, mir::ty::GenericArg::Type(inner) if self.has_unresolved_ty(inner))
            }),
            TyKind::Bool
            | TyKind::Char
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::FnDef(_, _)
            | TyKind::Never => false,
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
    container_locals: HashMap<mir::LocalId, mir::ContainerKind>,
    const_items: HashMap<hir::DefId, hir::Const>,
    blocks: Vec<mir::BasicBlockData>,
    current_block: mir::BasicBlockId,
    span: Span,
    method_context: Option<MethodContext>,
    type_substs: HashMap<String, Ty>,
    loop_stack: Vec<LoopContext>,
    null_locals: HashSet<mir::LocalId>,
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
            container_locals: HashMap::new(),
            const_items: HashMap::new(),
            blocks: vec![mir::BasicBlockData::new(None)],
            current_block: 0,
            span,
            method_context,
            type_substs,
            loop_stack: Vec::new(),
            null_locals: HashSet::new(),
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

    fn is_null_literal_expr(expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Literal(hir::Lit::Null))
    }

    fn update_null_tracking(&mut self, place: mir::Place, ty: Option<&Ty>, expr: &hir::Expr) {
        if !place.projection.is_empty() {
            return;
        }
        if let Some(ty) = ty {
            if !matches!(ty.kind, TyKind::Infer(_) | TyKind::Error(_)) {
                self.null_locals.remove(&place.local);
                return;
            }
        }
        if Self::is_null_literal_expr(expr) {
            self.null_locals.insert(place.local);
        } else {
            self.null_locals.remove(&place.local);
        }
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
                .map(|key| key.def_id)
                .or_else(|| {
                    let name = self.lowering.display_type_name(ty)?;
                    let matches: Vec<hir::DefId> = self
                        .lowering
                        .struct_defs
                        .iter()
                        .filter_map(|(def_id, def)| {
                            if def.name == name || def.name.ends_with(&format!("::{}", name)) {
                                Some(*def_id)
                            } else {
                                None
                            }
                        })
                        .collect();
                    if matches.len() == 1 {
                        matches.into_iter().next()
                    } else {
                        None
                    }
                }),
        }
    }

    fn enum_def_from_ty(&self, ty: &Ty) -> Option<hir::DefId> {
        match &ty.kind {
            TyKind::Ref(_, inner, _) => self.enum_def_from_ty(inner.as_ref()),
            TyKind::RawPtr(type_and_mut) => self.enum_def_from_ty(type_and_mut.ty.as_ref()),
            _ => self
                .lowering
                .enum_layouts
                .iter()
                .find_map(|(key, layout)| (layout.enum_ty == *ty).then_some(key.def_id)),
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

    fn lower_return(&mut self, span: Span, value: Option<&hir::Expr>) -> Result<()> {
        let return_ty = self.locals[0].ty.clone();
        let return_place = mir::Place::from_local(0);

        if let Some(value_expr) = value {
            self.lower_expr_into_place(value_expr, return_place.clone(), &return_ty)?;
        } else {
            if !matches!(return_ty.kind, TyKind::Tuple(ref elems) if elems.is_empty()) {
                self.lowering
                    .emit_error(span, "`return` without a value requires unit return type");
            }
        }

        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Return,
        };
        self.set_current_terminator(terminator);
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
                self.current_block = guard_block;
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
                } else if matches!(
                    pat.kind,
                    hir::PatKind::Struct(_, _, _) | hir::PatKind::TupleStruct(_, _)
                ) {
                    self.lowering.emit_warning(
                        span,
                        "struct pattern match is not supported; treating as non-matching",
                    );
                    mir::Operand::Constant(mir::Constant {
                        span,
                        user_ty: None,
                        literal: mir::ConstantKind::Bool(false),
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
            hir::PatKind::Tuple(_) => {
                self.lowering.emit_warning(
                    span,
                    "tuple pattern match is not supported; treating as non-matching",
                );
                mir::Operand::Constant(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(false),
                })
            }
            _ => {
                self.lowering.emit_warning(
                    span,
                    "unsupported pattern in match condition; treating as non-matching",
                );
                mir::Operand::Constant(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(false),
                })
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
                mir::Rvalue::BinaryOp(mir::BinOp::Eq, scrutinee_operand, literal),
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
                                self.lowering
                                    .emit_error(span, "enum variant payload layout not registered");
                                Vec::new()
                            });
                        for (idx, part) in parts.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
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
                                self.lowering
                                    .emit_error(span, "enum variant payload layout not registered");
                                Vec::new()
                            });
                        for (idx, field) in fields.iter().enumerate() {
                            if idx >= payload_tys.len() {
                                break;
                            }
                            let field_ty = payload_tys[idx].clone();
                            let mut field_place = scrutinee_place.clone();
                            field_place
                                .projection
                                .push(mir::PlaceElem::Field(idx + 1, field_ty.clone()));
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

        let mut declared_ty = local
            .ty
            .as_ref()
            .map(|ty_expr| self.lower_type_expr(ty_expr));
        let annotated_enum_def = local.ty.as_ref().and_then(|ty_expr| {
            let hir::TypeExprKind::Path(path) = &ty_expr.kind else {
                return None;
            };
            if let Some(hir::Res::Def(def_id)) = &path.res {
                if self.lowering.enum_defs.contains_key(def_id) {
                    return Some(*def_id);
                }
            }
            let name = path.segments.last()?.name.as_str();
            self.lowering
                .enum_defs
                .values()
                .find(|enm| enm.name == name)
                .map(|enm| enm.def_id)
        });
        if let Some(ty_expr) = local.ty.as_ref() {
            if let hir::TypeExprKind::Path(path) = &ty_expr.kind {
                if let Some(hir::Res::Def(def_id)) = &path.res {
                    if self.lowering.enum_defs.contains_key(def_id) {
                        let args = path
                            .segments
                            .last()
                            .and_then(|segment| segment.args.as_ref())
                            .map(|args| self.lowering.lower_generic_args(Some(args), init_span))
                            .unwrap_or_default();
                        let layout = if args.is_empty() {
                            self.lowering.enum_layout_for_def(*def_id, init_span)
                        } else {
                            self.lowering
                                .enum_layout_for_instance(*def_id, &args, init_span)
                        };
                        if let Some(layout) = layout {
                            declared_ty = Some(layout.enum_ty);
                        }
                    }
                }
            }
        }

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
            self.update_null_tracking(
                mir::Place::from_local(local_id),
                declared_ty.as_ref(),
                init_expr,
            );
            self.lower_assignment(
                local_id,
                declared_ty.as_ref(),
                annotated_enum_def,
                init_expr,
            )?;
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
                self.lowering
                    .register_const_value(self.program, item.def_id, konst);
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

                self.update_null_tracking(
                    place_info.place.clone(),
                    Some(&place_info.ty),
                    value_expr,
                );
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
            hir::ExprKind::Return(value) => {
                self.lower_return(expr.span, value.as_deref())?;
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
        annotated_enum_def: Option<hir::DefId>,
        expr: &hir::Expr,
    ) -> Result<()> {
        // Coerce enum payloads into their tagged layout when assigning from a place.
        let place_info = self.lower_place(expr)?;
        if let Some(place_info) = place_info {
            if let Some(enum_def) = annotated_enum_def {
                if let Some(layout) = self.lowering.enum_layout_for_def(enum_def, expr.span) {
                    if let Some((variant, layout)) = self.enum_variant_for_payload(
                        &layout.enum_ty,
                        &place_info.ty,
                        place_info.struct_def,
                    ) {
                        self.assign_enum_variant_from_place(
                            mir::Place::from_local(local_id),
                            &variant,
                            &layout,
                            place_info.place,
                            expr.span,
                        )?;
                        self.locals[local_id as usize].ty = layout.enum_ty.clone();
                        return Ok(());
                    }
                }
            }
            if let Some(expected_ty) = annotated_ty {
                if let Some((variant, layout)) = self.enum_variant_for_payload(
                    expected_ty,
                    &place_info.ty,
                    place_info.struct_def,
                ) {
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        place_info.place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = layout.enum_ty.clone();
                    return Ok(());
                }
            }
        }
        if let Some(expected_ty) = annotated_ty {
            if self.lowering.enum_layout_for_ty(expected_ty).is_some()
                && matches!(
                    expr.kind,
                    hir::ExprKind::Literal(_)
                        | hir::ExprKind::Index(_, _)
                        | hir::ExprKind::Cast(_, _)
                )
            {
                let value = self.lower_operand(expr, None)?;
                let payload_def = self.struct_def_from_ty(&value.ty);
                if let Some((variant, layout)) =
                    self.enum_variant_for_payload(expected_ty, &value.ty, payload_def)
                {
                    let payload_local = self.allocate_temp(value.ty.clone(), expr.span);
                    let payload_place = mir::Place::from_local(payload_local);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            payload_place.clone(),
                            mir::Rvalue::Use(value.operand),
                        ),
                    });
                    self.assign_enum_variant_from_place(
                        mir::Place::from_local(local_id),
                        &variant,
                        &layout,
                        payload_place,
                        expr.span,
                    )?;
                    self.locals[local_id as usize].ty = layout.enum_ty.clone();
                    return Ok(());
                }
            }
        }
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
            if let (
                Some(expected_ty),
                hir::ExprKind::Array(_) | hir::ExprKind::ArrayRepeat { .. },
            ) = (expected_ty.as_ref(), &expr.kind)
            {
                if self.is_list_container(expected_ty) || self.is_map_container(expected_ty) {
                    let place = mir::Place::from_local(local_id);
                    self.lower_expr_into_place(expr, place, expected_ty)?;
                    return Ok(());
                }
            }
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

    fn enum_variant_for_payload(
        &mut self,
        expected_ty: &Ty,
        payload_ty: &Ty,
        payload_def: Option<hir::DefId>,
    ) -> Option<(EnumVariantInfo, EnumLayout)> {
        let layout = self.lowering.enum_layout_for_ty(expected_ty)?.clone();
        let enum_def = self.enum_def_from_ty(expected_ty);
        for (def_id, payloads) in &layout.variant_payloads {
            let matches = if payloads.is_empty() {
                MirLowering::is_unit_ty(payload_ty)
            } else if payloads.len() == 1 {
                payloads[0] == *payload_ty
            } else {
                let tuple_ty = Ty {
                    kind: TyKind::Tuple(payloads.iter().cloned().map(Box::new).collect()),
                };
                if tuple_ty == *payload_ty {
                    true
                } else if let Some(layout) = self.lowering.struct_layout_for_ty(payload_ty) {
                    layout.field_tys == *payloads
                } else {
                    false
                }
            };

            if matches {
                if let Some(info) = self.lowering.enum_variants.get(def_id) {
                    return Some((info.clone(), layout));
                }
            }
        }
        let payload_len = match &payload_ty.kind {
            TyKind::Tuple(fields) => fields.len(),
            _ if MirLowering::is_unit_ty(payload_ty) => 0,
            _ => self
                .lowering
                .struct_layout_for_ty(payload_ty)
                .map(|layout| layout.field_tys.len())
                .unwrap_or(1),
        };
        let mut len_matches = layout
            .variant_payloads
            .iter()
            .filter(|(_, payloads)| payloads.len() == payload_len)
            .filter_map(|(def_id, _)| self.lowering.enum_variants.get(def_id))
            .filter(|info| {
                enum_def
                    .map(|def_id| info.enum_def == def_id)
                    .unwrap_or(true)
            });
        let first = len_matches.next().cloned();
        if first.is_some() && len_matches.next().is_some() {
            self.lowering.emit_error(
                self.span,
                "ambiguous enum payload coercion: multiple variants share the same payload shape",
            );
            return None;
        }
        if let Some(info) = first {
            return Some((info, layout));
        }
        let payload_struct_def = payload_def.or_else(|| self.struct_def_from_ty(payload_ty));
        if let (Some(enum_def), Some(payload_struct_def)) = (enum_def, payload_struct_def) {
            if let Some(info) = self.lowering.enum_variants.values().find(|info| {
                info.enum_def == enum_def && info.payload_def == Some(payload_struct_def)
            }) {
                return Some((info.clone(), layout));
            }
        }
        None
    }

    fn assign_enum_variant(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        args: &[hir::CallArg],
        span: Span,
    ) -> Result<()> {
        let payload_tys = layout
            .variant_payloads
            .get(&variant.def_id)
            .cloned()
            .unwrap_or_else(|| {
                self.lowering
                    .emit_error(span, "enum variant payload layout not registered");
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
                let operand = self.lower_operand(&arg.value, Some(expected_ty))?;
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

    fn assign_enum_variant_from_place(
        &mut self,
        place: mir::Place,
        variant: &EnumVariantInfo,
        layout: &EnumLayout,
        payload_place: mir::Place,
        span: Span,
    ) -> Result<()> {
        let payload_tys = layout
            .variant_payloads
            .get(&variant.def_id)
            .cloned()
            .unwrap_or_else(|| {
                self.lowering
                    .emit_error(span, "enum variant payload layout not registered");
                Vec::new()
            });

        let mut operands = Vec::with_capacity(1 + layout.payload_tys.len());
        operands.push(mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Int(variant.discriminant),
        }));

        for (idx, slot_ty) in layout.payload_tys.iter().enumerate() {
            if let Some(payload_ty) = payload_tys.get(idx) {
                let mut field_place = payload_place.clone();
                field_place
                    .projection
                    .push(mir::PlaceElem::Field(idx, payload_ty.clone()));
                operands.push(mir::Operand::Copy(field_place));
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
        args: &[hir::CallArg],
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
        let mut generic_args = resolved_path
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
            if let Some(layout) = self.lowering.enum_layout_for_def(variant.enum_def, span) {
                if layout.enum_ty == *expected_ty {
                    let payload_args: Vec<hir::CallArg> = fields
                        .iter()
                        .map(|field| hir::CallArg {
                            name: field.name.clone(),
                            value: field.expr.clone(),
                        })
                        .collect();
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
                if generic_args.is_empty() && !info.generics.is_empty() {
                    if let Some(inferred) =
                        self.infer_struct_generics_from_literals(&info, fields, span)?
                    {
                        generic_args = inferred;
                    }
                }
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
                    let payload_args: Vec<hir::CallArg> = fields
                        .iter()
                        .map(|field| hir::CallArg {
                            name: field.name.clone(),
                            value: field.expr.clone(),
                        })
                        .collect();
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
                let payload_args: Vec<hir::CallArg> = fields
                    .iter()
                    .map(|field| hir::CallArg {
                        name: field.name.clone(),
                        value: field.expr.clone(),
                    })
                    .collect();
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

        if let Some(expected_ty) = annotated_ty {
            if let Some(def_id) = self.struct_def_from_ty(expected_ty) {
                if let Some(info) = self.lowering.struct_defs.get(&def_id).cloned() {
                    if let Some(layout) = self.lowering.struct_layout_for_ty(expected_ty) {
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
            }
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
                    format!("struct layout missing field type for `{}`", field.name),
                );
                return Ok(());
            };
            struct_fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty: field_ty.clone(),
            });
        }

        if let (Some(expected_ty), Some(struct_info)) =
            (annotated_ty, self.lowering.struct_defs.get(&def_id))
        {
            let enum_layout = self.lowering.enum_layouts.iter().find_map(|(key, layout)| {
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

    fn infer_struct_generics_from_literals(
        &mut self,
        struct_def: &StructDefinition,
        fields: &[hir::StructExprField],
        span: Span,
    ) -> Result<Option<Vec<Ty>>> {
        let mut substs: HashMap<String, Ty> = HashMap::new();
        let mut field_map: HashMap<String, &hir::Expr> = HashMap::new();
        for field in fields {
            field_map.insert(String::from(field.name.clone()), &field.expr);
        }

        for field in &struct_def.fields {
            let Some(expr) = field_map.get(&field.name) else {
                continue;
            };
            let hir::ExprKind::Literal(lit) = &expr.kind else {
                continue;
            };
            let (_literal, actual_ty) = self.lower_literal(lit, None);
            self.lowering.infer_generic_from_type_expr(
                &field.ty,
                &actual_ty,
                &struct_def.generics,
                &mut substs,
                span,
            )?;
        }

        if struct_def.generics.is_empty() {
            return Ok(Some(Vec::new()));
        }

        for name in &struct_def.generics {
            if !substs.contains_key(name) {
                return Ok(None);
            }
        }

        Ok(Some(
            struct_def
                .generics
                .iter()
                .filter_map(|name| substs.get(name).cloned())
                .collect(),
        ))
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
        args: &[hir::CallArg],
        destination: Option<(mir::Place, Ty)>,
    ) -> Result<Option<PlaceInfo>> {
        let arg_values = call_arg_values(args);
        if let hir::ExprKind::Path(path) = &callee.kind {
            let segments = &path.segments;
            if segments.len() >= 2
                && segments[segments.len() - 2].name.as_str() == "HashMap"
                && segments[segments.len() - 1].name.as_str() == "from"
            {
                if let Some((place, expected_ty)) = destination {
                    if arg_values.len() != 1 {
                        self.lowering.emit_error(
                            expr.span,
                            "HashMap::from expects a single entries argument",
                        );
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: expected_ty,
                            struct_def: None,
                        }));
                    }

                    let hir::ExprKind::Array(elements) = &arg_values[0].kind else {
                        self.lowering.emit_error(
                            expr.span,
                            "HashMap::from expects an array literal of entries",
                        );
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: expected_ty,
                            struct_def: None,
                        }));
                    };

                    let mut entries = Vec::with_capacity(elements.len());
                    let mut key_ty: Option<Ty> = None;
                    let mut value_ty: Option<Ty> = None;

                    for element in elements {
                        if let hir::ExprKind::Struct(path, fields) = &element.kind {
                            let tail = path.segments.last().map(|seg| seg.name.as_str());
                            if tail == Some("HashMapEntry") {
                                let mut key_expr = None;
                                let mut value_expr = None;
                                for field in fields {
                                    match field.name.as_str() {
                                        "key" => key_expr = Some(&field.expr),
                                        "value" => value_expr = Some(&field.expr),
                                        _ => {}
                                    }
                                }
                                if let (Some(key_expr), Some(value_expr)) = (key_expr, value_expr) {
                                    let key_operand = self.lower_operand(key_expr, None)?;
                                    let value_operand = self.lower_operand(value_expr, None)?;
                                    if key_ty.is_none() {
                                        key_ty = Some(key_operand.ty.clone());
                                    }
                                    if value_ty.is_none() {
                                        value_ty = Some(value_operand.ty.clone());
                                    }
                                    entries.push((key_operand.operand, value_operand.operand));
                                    continue;
                                }
                            }
                        }
                        self.lowering.emit_error(
                            element.span,
                            "HashMap::from expects entries as HashMapEntry { key, value }",
                        );
                    }

                    let key_ty = key_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let value_ty = value_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::Map {
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                        len: entries.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerMapLiteral {
                                kind: kind.clone(),
                                entries,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        if (place.local as usize) < self.locals.len() {
                            self.locals[place.local as usize].ty = expected_ty.clone();
                        }
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(Some(PlaceInfo {
                        place,
                        ty: expected_ty,
                        struct_def: None,
                    }));
                }
            }
        }
        if let hir::ExprKind::Path(path) = &callee.kind {
            let tail = path.segments.last().map(|seg| seg.name.as_str());
            if tail == Some("get_unchecked") || tail == Some("::get_unchecked") {
                let (place, expected_ty) = match destination.as_ref() {
                    Some((place, expected_ty)) => (place.clone(), expected_ty.clone()),
                    None => {
                        self.lowering
                            .emit_error(expr.span, "HashMap::get_unchecked requires a destination");
                        return Ok(None);
                    }
                };
                if args.len() != 2 {
                    self.lowering.emit_error(
                        expr.span,
                        "HashMap::get_unchecked expects a container and key",
                    );
                    return Ok(Some(PlaceInfo {
                        place,
                        ty: expected_ty,
                        struct_def: None,
                    }));
                }

                if let hir::ExprKind::Path(path) = &arg_values[0].kind {
                    let mut resolved_path = path.clone();
                    self.resolve_self_path(&mut resolved_path);
                    let mut const_info = None;
                    let mut const_body_len = None;
                    if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                        if let Some(info) = self.lowering.const_values.get(def_id) {
                            const_info = Some(info.clone());
                        } else if let Some(item) = self.program.def_map.get(def_id) {
                            if let hir::ItemKind::Const(konst) = &item.kind {
                                if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                    const_body_len = Some(elements.len() as u64);
                                }
                                self.lowering
                                    .register_const_value(self.program, *def_id, konst);
                                if let Some(info) = self.lowering.const_values.get(def_id) {
                                    const_info = Some(info.clone());
                                }
                            }
                        }
                    } else if resolved_path.segments.len() == 1 {
                        let name = resolved_path.segments[0].name.as_str();
                        for (def_id, item) in &self.program.def_map {
                            if let hir::ItemKind::Const(konst) = &item.kind {
                                if konst.name.as_str() == name {
                                    if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                        const_body_len = Some(elements.len() as u64);
                                    }
                                    self.lowering.register_const_value(
                                        self.program,
                                        *def_id,
                                        konst,
                                    );
                                    if let Some(info) = self.lowering.const_values.get(def_id) {
                                        const_info = Some(info.clone());
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    if let Some(const_info) = const_info {
                        if let mir::ConstantKind::Val(value, _) = &const_info.value.literal {
                            if let Some((constant, ty)) = self.lowering.const_index_value(
                                self.program,
                                expr.span,
                                &const_info.value,
                                &arg_values[1],
                            ) {
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::Use(mir::Operand::Constant(constant)),
                                    ),
                                });
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = ty.clone();
                                }
                                return Ok(Some(PlaceInfo {
                                    place,
                                    ty,
                                    struct_def: None,
                                }));
                            }
                            let mut map_len: Option<u64> = None;
                            let mut map_key_ty: Option<Ty> = None;
                            let mut map_value_ty: Option<Ty> = None;
                            match value {
                                mir::ConstValue::Map {
                                    entries,
                                    key_ty,
                                    value_ty,
                                } => {
                                    map_len = Some(entries.len() as u64);
                                    map_key_ty = Some(key_ty.clone());
                                    map_value_ty = Some(value_ty.clone());
                                }
                                mir::ConstValue::List { elements, elem_ty } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(elements.len() as u64);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                                mir::ConstValue::Array(elements) => {
                                    if let TyKind::Array(elem_ty, _) = &const_info.ty.kind {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_len = Some(elements.len() as u64);
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                            if map_len.is_none() {
                                map_len = const_body_len;
                            }

                            if let (Some(key_ty), Some(value_ty), Some(len)) =
                                (map_key_ty, map_value_ty, map_len)
                            {
                                if len != 0 {
                                    let key_operand =
                                        self.lower_operand(arg_values[1], Some(&key_ty))?;
                                    self.push_statement(mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place.clone(),
                                            mir::Rvalue::ContainerGet {
                                                kind: mir::ContainerKind::Map {
                                                    key_ty: key_ty.clone(),
                                                    value_ty: value_ty.clone(),
                                                    len,
                                                },
                                                container: mir::Operand::Constant(
                                                    const_info.value.clone(),
                                                ),
                                                key: key_operand.operand,
                                            },
                                        ),
                                    });
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = value_ty.clone();
                                    }
                                    return Ok(Some(PlaceInfo {
                                        place,
                                        ty: value_ty,
                                        struct_def: None,
                                    }));
                                }
                            }
                        }
                    }
                }

                let container_info = self.lower_operand(arg_values[0], None)?;
                let mut map_len: Option<u64> = None;
                let mut map_key_ty: Option<Ty> = None;
                let mut map_value_ty: Option<Ty> = None;

                if let mir::Operand::Constant(constant) = &container_info.operand {
                    if let mir::ConstantKind::Val(value, _) = &constant.literal {
                        match value {
                            mir::ConstValue::Map {
                                entries,
                                key_ty,
                                value_ty,
                            } => {
                                map_len = Some(entries.len() as u64);
                                map_key_ty = Some(key_ty.clone());
                                map_value_ty = Some(value_ty.clone());
                            }
                            mir::ConstValue::List { elements, elem_ty } => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_len = Some(elements.len() as u64);
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                    }
                                }
                            }
                            mir::ConstValue::Array(elements) => {
                                if let TyKind::Array(elem_ty, _) = &container_info.ty.kind {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(elements.len() as u64);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }

                if map_len.is_none() {
                    if let Some(local_id) = self.local_id_from_expr(arg_values[0]) {
                        if let Some(container_kind) = self.container_locals.get(&local_id).cloned()
                        {
                            match container_kind {
                                mir::ContainerKind::Map {
                                    key_ty,
                                    value_ty,
                                    len,
                                } => {
                                    map_len = Some(len);
                                    map_key_ty = Some(key_ty);
                                    map_value_ty = Some(value_ty);
                                }
                                mir::ContainerKind::List { elem_ty, len } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(len);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if map_len.is_none() {
                    if let mir::Operand::Copy(place) = &container_info.operand {
                        if let Some(container_kind) =
                            self.container_locals.get(&place.local).cloned()
                        {
                            match container_kind {
                                mir::ContainerKind::Map {
                                    key_ty,
                                    value_ty,
                                    len,
                                } => {
                                    map_len = Some(len);
                                    map_key_ty = Some(key_ty);
                                    map_value_ty = Some(value_ty);
                                }
                                mir::ContainerKind::List { elem_ty, len } => {
                                    if let TyKind::Tuple(fields) = &elem_ty.kind {
                                        if fields.len() == 2 {
                                            map_len = Some(len);
                                            map_key_ty = Some((*fields[0].clone()).clone());
                                            map_value_ty = Some((*fields[1].clone()).clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if map_len.is_none() {
                    let container_ty = match &container_info.ty.kind {
                        TyKind::Ref(_, inner, _) => inner.as_ref(),
                        _ => &container_info.ty,
                    };
                    match &container_ty.kind {
                        TyKind::Array(elem_ty, len) => {
                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                if fields.len() == 2 {
                                    map_key_ty = Some((*fields[0].clone()).clone());
                                    map_value_ty = Some((*fields[1].clone()).clone());
                                    map_len = self.const_kind_to_u64(expr.span, len);
                                }
                            }
                        }
                        TyKind::Slice(elem_ty) => {
                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                if fields.len() == 2 {
                                    map_key_ty = Some((*fields[0].clone()).clone());
                                    map_value_ty = Some((*fields[1].clone()).clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }

                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                    let len = map_len.unwrap_or(0);
                    if len != 0 {
                        let key_operand = self.lower_operand(arg_values[1], Some(&key_ty))?;
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::ContainerGet {
                                    kind: mir::ContainerKind::Map {
                                        key_ty: key_ty.clone(),
                                        value_ty: value_ty.clone(),
                                        len,
                                    },
                                    container: container_info.operand,
                                    key: key_operand.operand,
                                },
                            ),
                        });
                        if (place.local as usize) < self.locals.len() {
                            self.locals[place.local as usize].ty = value_ty.clone();
                        }
                        return Ok(Some(PlaceInfo {
                            place,
                            ty: value_ty,
                            struct_def: None,
                        }));
                    }
                }
            }
        }
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
                        layout = self.lowering.enum_layout_for_instance(
                            variant.enum_def,
                            &args,
                            expr.span,
                        );
                    } else {
                        layout = self
                            .lowering
                            .enum_layout_for_def(variant.enum_def, expr.span);
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
                    self.assign_enum_variant(place.clone(), &variant, &layout, args, expr.span)?;
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
                    self.lowering
                        .emit_error(expr.span, "enum variant does not accept payload values");
                }
                if let Some(const_info) = self.lowering.const_values.get(&variant.def_id).cloned() {
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
            let operand = self.lower_operand(&arg.value, expected_ty)?;
            let inferred_ty = if let Some(expected_ty) = expected_ty {
                if let TyKind::Ref(_region, _inner, mutability) = &expected_ty.kind {
                    let local_id = match &arg.value.kind {
                        hir::ExprKind::Path(path) => {
                            if let Some(hir::Res::Local(hir_id)) = &path.res {
                                self.local_map.get(hir_id).copied()
                            } else {
                                path.segments
                                    .first()
                                    .filter(|_| path.segments.len() == 1)
                                    .and_then(|seg| {
                                        self.fallback_locals.get(seg.name.as_str()).copied()
                                    })
                            }
                        }
                        _ => None,
                    };
                    if let Some(local_id) = local_id {
                        if let Some(local_decl) = self.locals.get(local_id as usize) {
                            let inferred = local_decl.ty.clone();
                            if matches!(mutability, Mutability::Mut) {
                                self.lowering.emit_warning(
                                    arg.value.span,
                                    "mutable reference taken from non-mutable local in call",
                                );
                            }
                            arg_types.push(inferred.clone());
                            lowered_args.push(operand.operand);
                            continue;
                        }
                    }
                }
                if self.lowering.has_unresolved_ty(expected_ty) {
                    operand.ty.clone()
                } else {
                    expected_ty.clone()
                }
            } else {
                operand.ty.clone()
            };
            lowered_args.push(operand.operand);
            arg_types.push(inferred_ty);
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

                for (idx, arg) in args.iter().enumerate() {
                    let Some(expected_ty) = sig.inputs.get(idx) else {
                        continue;
                    };
                    if !matches!(expected_ty.kind, TyKind::FnPtr(_)) {
                        continue;
                    }
                    let operand = self.lower_operand(&arg.value, Some(expected_ty))?;
                    arg_types[idx] = operand.ty.clone();
                    lowered_args[idx] = operand.operand;
                }
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

        for (idx, operand) in lowered_args.iter_mut().enumerate() {
            let Some(expected_ty) = sig.inputs.get(idx) else {
                continue;
            };
            if self.lowering.enum_layout_for_ty(expected_ty).is_none() {
                continue;
            }

            let place = match operand {
                mir::Operand::Copy(place) | mir::Operand::Move(place) => place.clone(),
                _ => continue,
            };

            let local_ty = self
                .locals
                .get(place.local as usize)
                .map(|local| local.ty.clone())
                .unwrap_or_else(|| expected_ty.clone());
            let struct_def = self.local_structs.get(&place.local).copied();

            if let Some((variant, layout)) =
                self.enum_variant_for_payload(expected_ty, &local_ty, struct_def)
            {
                let local_id = self.allocate_temp(layout.enum_ty.clone(), expr.span);
                let enum_place = mir::Place::from_local(local_id);
                self.assign_enum_variant_from_place(
                    enum_place.clone(),
                    &variant,
                    &layout,
                    place,
                    expr.span,
                )?;
                *operand = mir::Operand::Move(enum_place);
                if let Some(arg_type) = arg_types.get_mut(idx) {
                    *arg_type = layout.enum_ty.clone();
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
            hir::ExprKind::FieldAccess(_, _) => {
                let operand = self.lower_operand(callee, None)?;
                if let TyKind::FnPtr(poly_fn_sig) = &operand.ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    return Ok((operand.operand, sig, None));
                }
                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "call target must be a function pointer, found {:?}",
                        operand.ty.kind
                    ),
                );
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
            _ => {
                let operand = self.lower_operand(callee, None)?;
                if let TyKind::FnPtr(poly_fn_sig) = &operand.ty.kind {
                    let fn_sig = &poly_fn_sig.binder.value;
                    let sig = mir::FunctionSig {
                        inputs: fn_sig.inputs.iter().map(|t| (**t).clone()).collect(),
                        output: (*fn_sig.output).clone(),
                    };
                    return Ok((operand.operand, sig, None));
                }
                self.lowering.emit_error(
                    callee.span,
                    format!(
                        "call target must be a function pointer, found {:?}",
                        operand.ty.kind
                    ),
                );
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
            if let Some(item) = self.program.def_map.get(def_id) {
                if let hir::ItemKind::Function(func) = &item.kind {
                    let sig = self.lowering.lower_function_sig(&func.sig, None);
                    self.lowering.function_sigs.insert(*def_id, sig.clone());
                    let name = func.sig.name.clone();
                    let ty = self.lowering.function_pointer_ty(&sig);
                    let operand = mir::Operand::Constant(mir::Constant {
                        span: callee.span,
                        user_ty: None,
                        literal: mir::ConstantKind::Fn(mir::Symbol::from(name.clone()), ty),
                    });
                    return Ok((operand, sig, Some(String::from(name))));
                }
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

        self.lowering.emit_error(
            callee.span,
            format!("unresolved call target `{}` during MIR lowering", name),
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
                if let Some((variant, layout)) =
                    self.enum_variant_for_payload(expected_ty, &place.ty, place.struct_def)
                {
                    let local_id = self.allocate_temp(layout.enum_ty.clone(), expr.span);
                    let enum_place = mir::Place::from_local(local_id);
                    self.assign_enum_variant_from_place(
                        enum_place.clone(),
                        &variant,
                        &layout,
                        place.place.clone(),
                        expr.span,
                    )?;
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(enum_place),
                        ty: layout.enum_ty.clone(),
                    });
                }
            }
            if let Some(expected_ty) = expected {
                if let TyKind::Ref(region, inner, mutability) = &expected_ty.kind {
                    if matches!(place.ty.kind, TyKind::Ref(_, _, _)) {
                        return Ok(OperandInfo {
                            operand: mir::Operand::copy(place.place.clone()),
                            ty: place.ty,
                        });
                    }

                    let resolved_inner = if self.lowering.is_opaque_ty(inner.as_ref())
                        && !self.lowering.is_opaque_ty(&place.ty)
                    {
                        place.ty.clone()
                    } else {
                        inner.as_ref().clone()
                    };
                    let ref_ty = if resolved_inner == *inner.as_ref() {
                        expected_ty.clone()
                    } else {
                        Ty {
                            kind: TyKind::Ref(
                                region.clone(),
                                Box::new(resolved_inner),
                                *mutability,
                            ),
                        }
                    };
                    let borrow_kind = match mutability {
                        Mutability::Mut => mir::BorrowKind::Mut {
                            allow_two_phase_borrow: false,
                        },
                        Mutability::Not => mir::BorrowKind::Shared,
                    };
                    let temp_local = self.allocate_temp(ref_ty.clone(), expr.span);
                    let temp_place = mir::Place::from_local(temp_local);
                    let assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            temp_place.clone(),
                            mir::Rvalue::Ref((), borrow_kind, place.place.clone()),
                        ),
                    };
                    self.push_statement(assign);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(temp_place),
                        ty: ref_ty,
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
                if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                    if let Some(expected_sig) = expected_sig.as_ref() {
                        if let Some(function) =
                            self.lowering.generic_function_defs.get(def_id).cloned()
                        {
                            let expected_has_opaque = expected_sig
                                .inputs
                                .iter()
                                .any(|ty| self.lowering.is_opaque_ty(ty))
                                || self.lowering.is_opaque_ty(&expected_sig.output);
                            if expected_has_opaque {
                                let fn_ty = self.lowering.function_pointer_ty(expected_sig);
                                return Ok(OperandInfo {
                                    operand: mir::Operand::Constant(mir::Constant {
                                        span: expr.span,
                                        user_ty: None,
                                        literal: mir::ConstantKind::Fn(
                                            mir::Symbol::new(
                                                function.sig.name.as_str().to_string(),
                                            ),
                                            fn_ty.clone(),
                                        ),
                                    }),
                                    ty: fn_ty,
                                });
                            }
                            let info = self.lowering.ensure_function_specialization(
                                self.program,
                                *def_id,
                                &function,
                                &[],
                                &expected_sig.inputs,
                                expr.span,
                            )?;
                            return Ok(OperandInfo {
                                operand: mir::Operand::Constant(mir::Constant {
                                    span: expr.span,
                                    user_ty: None,
                                    literal: mir::ConstantKind::Fn(
                                        mir::Symbol::new(info.name.clone()),
                                        info.fn_ty.clone(),
                                    ),
                                }),
                                ty: info.fn_ty,
                            });
                        }
                    }
                    if let Some(variant) = self.lowering.enum_variants.get(def_id).cloned() {
                        let mut layout =
                            expected.and_then(|ty| self.lowering.enum_layout_for_ty(ty).cloned());
                        if layout.is_none() {
                            let args = resolved_path
                                .segments
                                .last()
                                .and_then(|segment| segment.args.as_ref())
                                .map(|args| self.lowering.lower_generic_args(Some(args), expr.span))
                                .unwrap_or_default();
                            if !args.is_empty() {
                                layout = self.lowering.enum_layout_for_instance(
                                    variant.enum_def,
                                    &args,
                                    expr.span,
                                );
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
                    if let Some(const_info) = self.lowering.const_values.get(def_id).cloned() {
                        if resolved_path.segments.last().map(|seg| seg.name.as_str())
                            == Some("REGISTRY")
                        {
                            self.lowering.emit_warning(
                                expr.span,
                                format!(
                                    "lower_operand resolved REGISTRY to const ty {:?}",
                                    const_info.ty.kind
                                ),
                            );
                        }
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(const_info.value.clone()),
                            ty: const_info.ty.clone(),
                        });
                    }

                    if let Some(const_item) = self.program.def_map.get(def_id) {
                        if let hir::ItemKind::Const(konst) = &const_item.kind {
                            self.lowering
                                .register_const_value(self.program, *def_id, konst);
                            if let Some(const_info) = self.lowering.const_values.get(def_id) {
                                return Ok(OperandInfo {
                                    operand: mir::Operand::Constant(const_info.value.clone()),
                                    ty: const_info.ty.clone(),
                                });
                            }
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

                if resolved_path.res.is_none() {
                    let name = resolved_path
                        .segments
                        .iter()
                        .map(|seg| seg.name.as_str())
                        .collect::<Vec<_>>()
                        .join("::");
                    let tail = resolved_path
                        .segments
                        .last()
                        .map(|seg| seg.name.as_str())
                        .unwrap_or(name.as_str());
                    for (def_id, item) in &self.program.def_map {
                        if let hir::ItemKind::Const(konst) = &item.kind {
                            let konst_name = konst.name.as_str();
                            let konst_tail = konst_name.split("::").last().unwrap_or(konst_name);
                            if konst_name == name || konst_tail == name || konst_tail == tail {
                                self.lowering
                                    .register_const_value(self.program, *def_id, konst);
                                if let Some(const_info) = self.lowering.const_values.get(def_id) {
                                    return Ok(OperandInfo {
                                        operand: mir::Operand::Constant(const_info.value.clone()),
                                        ty: const_info.ty.clone(),
                                    });
                                }
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
            hir::ExprKind::Index(base, index) => {
                if let hir::ExprKind::Path(path) = &base.kind {
                    if let Some(hir::Res::Def(def_id)) = &path.res {
                        if let Some(const_info) = self.lowering.const_values.get(def_id).cloned() {
                            if let Some((constant, ty)) = self.lowering.const_index_value(
                                self.program,
                                expr.span,
                                &const_info.value,
                                index,
                            ) {
                                return Ok(OperandInfo {
                                    operand: mir::Operand::Constant(constant),
                                    ty,
                                });
                            }
                        }
                        if let Some(konst) = self.const_items.get(def_id).cloned() {
                            let ty = self.lowering.lower_type_expr(&konst.ty);
                            if let Some(constant) = self.lowering.lower_const_expr(
                                self.program,
                                &konst.body.value,
                                Some(&ty),
                                None,
                            ) {
                                if let Some((constant, ty)) = self.lowering.const_index_value(
                                    self.program,
                                    expr.span,
                                    &constant,
                                    index,
                                ) {
                                    return Ok(OperandInfo {
                                        operand: mir::Operand::Constant(constant),
                                        ty,
                                    });
                                }
                            }
                        }
                    }
                }
                let base_info = self.lower_operand(base, None)?;
                if self.is_list_container(&base_info.ty) {
                    let index_ty = Ty {
                        kind: TyKind::Uint(UintTy::Usize),
                    };
                    let index_operand = self.lower_operand(index, Some(&index_ty))?;
                    let element_ty = expected
                        .cloned()
                        .or_else(|| self.expect_array_element_ty(&base_info.ty))
                        .unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::List {
                        elem_ty: element_ty.clone(),
                        len: self
                            .local_id_from_expr(base)
                            .and_then(|id| self.container_locals.get(&id))
                            .and_then(|kind| match kind {
                                mir::ContainerKind::List { len, .. } => Some(*len),
                                _ => None,
                            })
                            .unwrap_or(0),
                    };
                    let local_id = self.allocate_temp(element_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::ContainerGet {
                                kind,
                                container: base_info.operand,
                                key: index_operand.operand,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: element_ty,
                    });
                }
                if self.is_map_container(&base_info.ty) {
                    let index_operand = self.lower_operand(index, None)?;
                    let mut value_ty = expected
                        .cloned()
                        .unwrap_or_else(|| self.lowering.error_ty());
                    let mut kind = mir::ContainerKind::Map {
                        key_ty: index_operand.ty.clone(),
                        value_ty: value_ty.clone(),
                        len: 0,
                    };
                    if let Some(local_id) = self.local_id_from_expr(base) {
                        if let Some(container_kind) = self.container_locals.get(&local_id) {
                            if let mir::ContainerKind::Map {
                                key_ty,
                                value_ty: entry_value_ty,
                                len,
                            } = container_kind
                            {
                                kind = mir::ContainerKind::Map {
                                    key_ty: key_ty.clone(),
                                    value_ty: entry_value_ty.clone(),
                                    len: *len,
                                };
                                value_ty = entry_value_ty.clone();
                            }
                        }
                    }
                    if let mir::ContainerKind::Map {
                        key_ty,
                        value_ty: entry_value_ty,
                        len,
                    } = &mut kind
                    {
                        if *len == 0 {
                            if let mir::Operand::Constant(constant) = &base_info.operand {
                                if let mir::ConstantKind::Val(value, _) = &constant.literal {
                                    match value {
                                        mir::ConstValue::Map {
                                            entries,
                                            key_ty: map_key_ty,
                                            value_ty: map_value_ty,
                                        } => {
                                            *len = entries.len() as u64;
                                            *key_ty = map_key_ty.clone();
                                            *entry_value_ty = map_value_ty.clone();
                                            value_ty = map_value_ty.clone();
                                        }
                                        mir::ConstValue::List { elements, elem_ty } => {
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    *len = elements.len() as u64;
                                                    *key_ty = (*fields[0].clone()).clone();
                                                    *entry_value_ty = (*fields[1].clone()).clone();
                                                    value_ty = (*fields[1].clone()).clone();
                                                }
                                            }
                                        }
                                        mir::ConstValue::Array(elements) => {
                                            if let TyKind::Array(elem_ty, _) = &base_info.ty.kind {
                                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                    if fields.len() == 2 {
                                                        *len = elements.len() as u64;
                                                        *key_ty = (*fields[0].clone()).clone();
                                                        *entry_value_ty =
                                                            (*fields[1].clone()).clone();
                                                        value_ty = (*fields[1].clone()).clone();
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    if matches!(kind, mir::ContainerKind::Map { len: 0, .. }) {
                        self.lowering.emit_error(
                            expr.span,
                            "map indexing requires a literal HashMap for now",
                        );
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            ),
                            ty: value_ty,
                        });
                    }
                    let local_id = self.allocate_temp(value_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    self.push_statement(mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::ContainerGet {
                                kind,
                                container: base_info.operand,
                                key: index_operand.operand,
                            },
                        ),
                    });
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: value_ty,
                    });
                }
                let index_ty = Ty {
                    kind: TyKind::Uint(UintTy::Usize),
                };
                let index_operand = self.lower_operand(index, Some(&index_ty))?;
                let index_local = self.allocate_temp(index_operand.ty.clone(), index.span);
                let index_place = mir::Place::from_local(index_local);
                self.push_statement(mir::Statement {
                    source_info: index.span,
                    kind: mir::StatementKind::Assign(
                        index_place.clone(),
                        mir::Rvalue::Use(index_operand.operand),
                    ),
                });

                let (mut place, mut base_ty) = match base_info.operand {
                    mir::Operand::Copy(place) | mir::Operand::Move(place) => {
                        (place, base_info.ty.clone())
                    }
                    other => {
                        let local_id = self.allocate_temp(base_info.ty.clone(), expr.span);
                        let place = mir::Place::from_local(local_id);
                        self.push_statement(mir::Statement {
                            source_info: expr.span,
                            kind: mir::StatementKind::Assign(
                                place.clone(),
                                mir::Rvalue::Use(other),
                            ),
                        });
                        (place, base_info.ty.clone())
                    }
                };

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
                    TyKind::Array(elem, _) => *elem.clone(),
                    TyKind::Slice(elem) => *elem.clone(),
                    _ => {
                        self.lowering.emit_error(
                            expr.span,
                            "index access requires array, slice, or supported container",
                        );
                        let ty = expected
                            .cloned()
                            .unwrap_or_else(|| self.lowering.error_ty());
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(
                                self.lowering.error_constant(expr.span),
                            ),
                            ty,
                        });
                    }
                };

                place.projection.push(mir::PlaceElem::Index(index_local));
                return Ok(OperandInfo {
                    operand: mir::Operand::copy(place),
                    ty: element_ty,
                });
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
                if call.kind == IntrinsicCallKind::Format {
                    let (format, args) = self.prepare_format_call(call, expr.span)?;
                    let string_ty = Ty {
                        kind: TyKind::RawPtr(TypeAndMut {
                            ty: Box::new(Ty {
                                kind: TyKind::Int(IntTy::I8),
                            }),
                            mutbl: Mutability::Not,
                        }),
                    };
                    let local_id = self.allocate_temp(string_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicCallKind::Format,
                                format,
                                args,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: string_ty,
                    });
                }
                if call.kind == IntrinsicCallKind::Panic {
                    self.emit_panic_intrinsic(call, expr.span)?;
                    let unit_ty = MirLowering::unit_ty();
                    return Ok(OperandInfo {
                        operand: mir::Operand::Constant(mir::Constant {
                            span: expr.span,
                            user_ty: None,
                            literal: mir::ConstantKind::Val(mir::ConstValue::Unit, unit_ty.clone()),
                        }),
                        ty: unit_ty,
                    });
                }
                if call.kind == IntrinsicCallKind::CatchUnwind {
                    return self.lower_catch_unwind(expr, call, None);
                }
                if call.kind == IntrinsicCallKind::TimeNow {
                    let args = &call.callargs;
                    if !args.is_empty() {
                        self.lowering
                            .emit_error(expr.span, "time::now intrinsic expects no arguments");
                    }
                    let now_ty = Ty {
                        kind: TyKind::Float(FloatTy::F64),
                    };
                    let local_id = self.allocate_temp(now_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicCallKind::TimeNow,
                                format: String::new(),
                                args: Vec::new(),
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: now_ty,
                    });
                }
                if call.kind == IntrinsicCallKind::Len {
                    let args = &call.callargs;
                    let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

                    let Some(arg) = arg_values.first() else {
                        self.lowering
                            .emit_error(expr.span, "len intrinsic expects one argument");
                        return Ok(OperandInfo {
                            operand: mir::Operand::Constant(mir::Constant {
                                span: expr.span,
                                user_ty: None,
                                literal: mir::ConstantKind::UInt(0),
                            }),
                            ty: Ty {
                                kind: TyKind::Uint(UintTy::U64),
                            },
                        });
                    };

                    if let Some(local_id) = self.local_id_from_expr(arg) {
                        if let Some(kind) = self.container_locals.get(&local_id).cloned() {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::U64),
                            };
                            let local_id_out = self.allocate_temp(len_ty.clone(), expr.span);
                            let local_place = mir::Place::from_local(local_id_out);
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    local_place.clone(),
                                    mir::Rvalue::ContainerLen {
                                        kind,
                                        container: mir::Operand::copy(mir::Place::from_local(
                                            local_id,
                                        )),
                                    },
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(OperandInfo {
                                operand: mir::Operand::copy(local_place),
                                ty: len_ty,
                            });
                        }
                    }

                    let place = if let Some(place_info) = self.lower_place(arg)? {
                        place_info.place
                    } else {
                        let arg_ty = expected.cloned().unwrap_or_else(|| Ty {
                            kind: TyKind::Tuple(Vec::new()),
                        });
                        let local_id = self.allocate_temp(arg_ty.clone(), arg.span);
                        let temp_place = mir::Place::from_local(local_id);
                        self.lower_expr_into_place(arg, temp_place.clone(), &arg_ty)?;
                        temp_place
                    };

                    let len_ty = Ty {
                        kind: TyKind::Uint(UintTy::U64),
                    };
                    let local_id = self.allocate_temp(len_ty.clone(), expr.span);
                    let local_place = mir::Place::from_local(local_id);
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            local_place.clone(),
                            mir::Rvalue::Len(place),
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(OperandInfo {
                        operand: mir::Operand::copy(local_place),
                        ty: len_ty,
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
        let args = &call.callargs;
        if call
            .callargs
            .first()
            .is_some_and(|arg| matches!(arg.value.kind, hir::ExprKind::FormatString(_)))
        {
            self.lowering.emit_warning(
                span,
                "treating formatted intrinsic payload as opaque during MIR lowering",
            );
            return None;
        }
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        match call.kind {
            IntrinsicCallKind::SizeOf => {
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
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
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
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

                let struct_ref = match self.resolve_struct_ref(arg_values[0]) {
                    Some(value) => value,
                    None => {
                        self.lowering
                            .emit_error(span, "hasfield! only supports struct types");
                        return None;
                    }
                };

                let field_name = match self.expect_string_literal(arg_values[1], span) {
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
                let target_expr = match arg_values.get(0) {
                    Some(expr) => *expr,
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
        let Some((template, call_args)) = self.format_call_parts(call, span) else {
            return Ok(());
        };

        let mut lowered_args = Vec::with_capacity(call_args.len());
        for arg in call_args {
            if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                lowered_args.push(formatted);
            } else {
                lowered_args.push(self.lower_operand(&arg.value, None)?);
            }
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
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
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

        let mut operands = Vec::with_capacity(prepared_args.len());
        for (operand, _ty, _spec) in prepared_args {
            operands.push(operand);
        }

        self.push_statement(mir::Statement {
            source_info: span,
            kind: mir::StatementKind::IntrinsicCall {
                kind: call.kind,
                format,
                args: operands,
            },
        });
        Ok(())
    }

    fn prepare_format_call(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Result<(String, Vec<mir::Operand>)> {
        let Some((template, call_args)) = self.format_call_parts(call, span) else {
            return Ok((String::new(), Vec::new()));
        };

        let mut lowered_args = Vec::with_capacity(call_args.len());
        for arg in call_args {
            if let Some(formatted) = self.try_format_const_expr_for_printf(&arg.value, span) {
                lowered_args.push(formatted);
            } else {
                lowered_args.push(self.lower_operand(&arg.value, None)?);
            }
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
                            return Ok((String::new(), Vec::new()));
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
                        return Ok((String::new(), Vec::new()));
                    };

                    if let Some(explicit) = &placeholder.format_spec {
                        let trimmed = explicit.raw.trim();
                        if trimmed.starts_with('%') {
                            format.push_str(&explicit.raw);
                        } else {
                            format.push('%');
                            format.push_str(trimmed);
                            if !trimmed.chars().any(|c| c.is_ascii_alphabetic()) {
                                format.push_str(spec.trim_start_matches('%'));
                            }
                        }
                    } else {
                        format.push_str(spec);
                    }
                }
            }
        }

        let mut operands = Vec::with_capacity(prepared_args.len());
        for (operand, _ty, _spec) in prepared_args {
            operands.push(operand);
        }

        Ok((format, operands))
    }

    fn format_call_parts(
        &mut self,
        call: &hir::IntrinsicCallExpr,
        span: Span,
    ) -> Option<(hir::FormatString, Vec<hir::CallArg>)> {
        let Some(first) = call.callargs.first() else {
            self.lowering
                .emit_error(span, "format intrinsic requires a template argument");
            return None;
        };

        let hir::ExprKind::FormatString(template) = &first.value.kind else {
            self.lowering
                .emit_error(span, "format intrinsic requires a template argument");
            return None;
        };

        let mut positional = Vec::new();
        for arg in &call.callargs[1..] {
            let name = arg.name.as_str();
            if name.starts_with("arg") && name[3..].chars().all(|ch| ch.is_ascii_digit()) {
                positional.push(arg.clone());
            } else {
                self.lowering
                    .emit_error(span, "named arguments are not supported in format lowering");
                return None;
            }
        }

        Some((template.clone(), positional))
    }

    fn emit_panic_intrinsic(&mut self, call: &hir::IntrinsicCallExpr, span: Span) -> Result<()> {
        let message = if call.callargs.is_empty() {
            "panic! macro triggered".to_string()
        } else if call.callargs.len() == 1 {
            match &call.callargs[0].value.kind {
                hir::ExprKind::Literal(hir::Lit::Str(text)) => text.clone(),
                hir::ExprKind::FormatString(template) => {
                    let has_placeholders = template
                        .parts
                        .iter()
                        .any(|part| matches!(part, hir::FormatTemplatePart::Placeholder(_)));
                    if has_placeholders {
                        self.lowering.emit_error(
                            span,
                            "panic format payload is not supported in compiled backends",
                        );
                        "<panic message unavailable>".to_string()
                    } else {
                        template
                            .parts
                            .iter()
                            .map(|part| match part {
                                hir::FormatTemplatePart::Literal(text) => text.as_str(),
                                hir::FormatTemplatePart::Placeholder(_) => "",
                            })
                            .collect::<Vec<_>>()
                            .join("")
                    }
                }
                _ => {
                    self.lowering
                        .emit_error(span, "panic expects a string literal in compiled backends");
                    "<panic message unavailable>".to_string()
                }
            }
        } else {
            self.lowering
                .emit_error(span, "panic expects zero or one argument");
            "<panic message unavailable>".to_string()
        };

        let sig = mir::FunctionSig {
            inputs: vec![self.lowering.raw_string_ptr_ty()],
            output: MirLowering::unit_ty(),
        };
        self.lowering.ensure_runtime_stub("fp_panic", &sig);
        let fn_ty = self.lowering.function_pointer_ty(&sig);
        let func = mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Fn(mir::Symbol::new("fp_panic".to_string()), fn_ty),
        });
        let args = vec![mir::Operand::Constant(mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Str(message),
        })];

        let result_local = self.allocate_temp(MirLowering::unit_ty(), span);
        let after_block = self.new_block();
        let terminator = mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Call {
                func,
                args,
                destination: Some((mir::Place::from_local(result_local), after_block)),
                cleanup: None,
                from_hir_call: true,
                fn_span: span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = after_block;
        self.set_current_terminator(mir::Terminator {
            source_info: span,
            kind: mir::TerminatorKind::Unreachable,
        });
        self.current_block = self.new_block();
        Ok(())
    }

    fn lower_catch_unwind(
        &mut self,
        expr: &hir::Expr,
        call: &hir::IntrinsicCallExpr,
        destination: Option<mir::Place>,
    ) -> Result<OperandInfo> {
        let args = &call.callargs;
        let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

        if args.len() != 1 {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind expects exactly one callable argument",
            );
            return Ok(self.constant_bool_operand(false, expr.span));
        }

        let callee = arg_values[0];
        let mut call_args: Vec<mir::Operand> = Vec::new();
        let (func, sig, _name) = if let hir::ExprKind::Struct(path, _) = &callee.kind {
            let struct_name = path.segments.last().map(|seg| seg.name.as_str());
            let closure_suffix = struct_name.and_then(|name| name.strip_prefix("__Closure"));
            if let Some(suffix) = closure_suffix {
                let env = self.lower_operand(callee, None)?;
                let call_name = format!("__closure{}_call", suffix);
                let path = hir::Path {
                    segments: vec![hir::PathSegment {
                        name: hir::Symbol::new(call_name),
                        args: None,
                    }],
                    res: None,
                };
                let call_expr = hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Path(path),
                    span: expr.span,
                };
                call_args.push(env.operand);
                self.resolve_callee(&call_expr)?
            } else {
                self.resolve_callee(callee)?
            }
        } else {
            self.resolve_callee(callee)?
        };
        if call_args.is_empty() {
            if !sig.inputs.is_empty() {
                self.lowering.emit_error(
                    expr.span,
                    "catch_unwind only supports zero-argument callables",
                );
            }
        } else if sig.inputs.len() != call_args.len() {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind closure must not take user arguments",
            );
        }
        if !MirLowering::is_unit_ty(&sig.output) {
            self.lowering.emit_error(
                expr.span,
                "catch_unwind only supports callables that return unit",
            );
        }

        let result_ty = Ty { kind: TyKind::Bool };
        let result_place = destination.unwrap_or_else(|| {
            let local_id = self.allocate_temp(result_ty.clone(), expr.span);
            mir::Place::from_local(local_id)
        });
        if (result_place.local as usize) < self.locals.len() {
            self.locals[result_place.local as usize].ty = result_ty.clone();
        }

        let call_result_local = self.allocate_temp(sig.output.clone(), expr.span);
        let call_result_place = mir::Place::from_local(call_result_local);

        let ok_block = self.new_block();
        let unwind_block = self.new_block();
        if let Some(block) = self.blocks.get_mut(unwind_block as usize) {
            block.is_cleanup = true;
        }
        let join_block = self.new_block();

        let terminator = mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Call {
                func,
                args: call_args,
                destination: Some((call_result_place, ok_block)),
                cleanup: Some(unwind_block),
                from_hir_call: true,
                fn_span: expr.span,
            },
        };
        self.blocks[self.current_block as usize].terminator = Some(terminator);

        self.current_block = ok_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(true),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = unwind_block;
        self.push_statement(mir::Statement {
            source_info: expr.span,
            kind: mir::StatementKind::Assign(
                result_place.clone(),
                mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                    span: expr.span,
                    user_ty: None,
                    literal: mir::ConstantKind::Bool(false),
                })),
            ),
        });
        self.set_current_terminator(mir::Terminator {
            source_info: expr.span,
            kind: mir::TerminatorKind::Goto { target: join_block },
        });

        self.current_block = join_block;
        Ok(OperandInfo {
            operand: mir::Operand::copy(result_place),
            ty: result_ty,
        })
    }

    fn prepare_printf_arg(
        &mut self,
        arg: OperandInfo,
        span: Span,
    ) -> Result<(mir::Operand, Ty, String)> {
        let (operand, ty) = (arg.operand, arg.ty);
        if let mir::Operand::Constant(constant) = &operand {
            if matches!(constant.literal, mir::ConstantKind::Null) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        if let mir::Operand::Copy(place) | mir::Operand::Move(place) = &operand {
            if place.projection.is_empty() && self.null_locals.contains(&place.local) {
                return Ok((
                    mir::Operand::Constant(mir::Constant {
                        span,
                        user_ty: None,
                        literal: mir::ConstantKind::Str("null".to_string()),
                    }),
                    self.lowering.raw_string_ptr_ty(),
                    "%s".to_string(),
                ));
            }
        }
        match &ty.kind {
            TyKind::Bool => Ok((operand, ty.clone(), "%d".to_string())),
            TyKind::Char => Ok((operand, ty.clone(), "%c".to_string())),
            TyKind::Int(int_ty) => Ok((
                operand,
                ty.clone(),
                match int_ty {
                    IntTy::I8 => "%hhd",
                    IntTy::I16 => "%hd",
                    IntTy::I32 => "%d",
                    IntTy::I64 => "%lld",
                    IntTy::I128 => "%lld",
                    IntTy::Isize => "%lld",
                }
                .to_string(),
            )),
            TyKind::Uint(uint_ty) => Ok((
                operand,
                ty.clone(),
                match uint_ty {
                    UintTy::U8 => "%hhu",
                    UintTy::U16 => "%hu",
                    UintTy::U32 => "%u",
                    UintTy::U64 => "%llu",
                    UintTy::U128 => "%llu",
                    UintTy::Usize => "%llu",
                }
                .to_string(),
            )),
            TyKind::Float(_) => Ok((operand, ty.clone(), "%f".to_string())),
            TyKind::RawPtr(type_and_mut) => {
                if self.is_c_string_ptr(type_and_mut.ty.as_ref()) {
                    Ok((operand, ty.clone(), "%s".to_string()))
                } else {
                    self.lowering
                        .emit_error(span, "printf only supports raw pointers to byte strings");
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
            TyKind::Tuple(elements) if elements.is_empty() => Ok((
                mir::Operand::Constant(mir::Constant {
                    span,
                    user_ty: None,
                    literal: mir::ConstantKind::Str("()".to_string()),
                }),
                self.lowering.raw_string_ptr_ty(),
                "%s".to_string(),
            )),
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
                        self.lowering
                            .emit_error(span, "printf cannot dereference non-place arguments");
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
                if let Some((string_operand, string_ty)) =
                    self.format_const_operand_for_printf(&operand, span)
                {
                    return Ok((string_operand, string_ty, "%s".to_string()));
                }
                if self.lowering.is_opaque_ty(&ty) {
                    return Ok((operand, ty.clone(), "%s".to_string()));
                }
                let ty_name = self
                    .lowering
                    .display_type_name(&ty)
                    .unwrap_or_else(|| format!("{:?}", ty.kind));
                self.lowering.emit_error(
                    span,
                    format!("printf argument type is not supported: {}", ty_name),
                );
                Ok((operand, ty.clone(), "%s".to_string()))
            }
        }
    }

    fn format_const_operand_for_printf(
        &mut self,
        operand: &mir::Operand,
        span: Span,
    ) -> Option<(mir::Operand, Ty)> {
        let mir::Operand::Constant(constant) = operand else {
            return None;
        };
        let mir::ConstantKind::Val(value, _) = &constant.literal else {
            return None;
        };
        let ast_value = self.const_value_to_ast_value(value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        let constant = mir::Constant {
            span,
            user_ty: None,
            literal: mir::ConstantKind::Str(formatted),
        };
        Some((mir::Operand::Constant(constant), ty))
    }

    fn try_format_const_expr_for_printf(
        &mut self,
        expr: &hir::Expr,
        span: Span,
    ) -> Option<OperandInfo> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        let Some(hir::Res::Def(def_id)) = &path.res else {
            return None;
        };
        let const_info = self.lowering.const_values.get(def_id)?;
        let mir::ConstantKind::Val(value, _) = &const_info.value.literal else {
            return None;
        };
        let value = value.clone();
        if !matches!(
            value,
            mir::ConstValue::Array(_)
                | mir::ConstValue::List { .. }
                | mir::ConstValue::Map { .. }
                | mir::ConstValue::Tuple(_)
                | mir::ConstValue::Struct(_)
        ) {
            return None;
        }
        let ast_value = self.const_value_to_ast_value(&value)?;
        let formatted = match format_value_with_spec(&ast_value, None) {
            Ok(text) => text,
            Err(err) => {
                self.lowering.emit_error(
                    span,
                    format!("failed to format const value for printf: {}", err),
                );
                return None;
            }
        };
        let ty = Ty {
            kind: TyKind::RawPtr(TypeAndMut {
                ty: Box::new(Ty {
                    kind: TyKind::Int(IntTy::I8),
                }),
                mutbl: Mutability::Not,
            }),
        };
        Some(OperandInfo::constant(
            span,
            ty,
            mir::ConstantKind::Str(formatted),
        ))
    }

    fn const_value_to_ast_value(&mut self, value: &mir::ConstValue) -> Option<Value> {
        match value {
            mir::ConstValue::Unit => Some(Value::unit()),
            mir::ConstValue::Bool(value) => Some(Value::bool(*value)),
            mir::ConstValue::Int(value) => Some(Value::int(*value)),
            mir::ConstValue::UInt(value) => Some(Value::int(*value as i64)),
            mir::ConstValue::Float(value) => Some(Value::decimal(*value)),
            mir::ConstValue::Str(value) => Some(Value::string(value.clone())),
            mir::ConstValue::Null => Some(Value::null()),
            mir::ConstValue::Fn(_) => None,
            mir::ConstValue::Tuple(values) | mir::ConstValue::Struct(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::Tuple(ValueTuple::new(elements)))
            }
            mir::ConstValue::Array(values) => {
                let mut elements = Vec::with_capacity(values.len());
                for element in values {
                    elements.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(elements)))
            }
            mir::ConstValue::List { elements, .. } => {
                let mut items = Vec::with_capacity(elements.len());
                for element in elements {
                    items.push(self.const_value_to_ast_value(element)?);
                }
                Some(Value::List(ValueList::new(items)))
            }
            mir::ConstValue::Map { entries, .. } => {
                let mut items = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let key_value = self.const_value_to_ast_value(key)?;
                    let value_value = self.const_value_to_ast_value(value)?;
                    items.push((key_value, value_value));
                }
                Some(Value::Map(ValueMap::from_pairs(items)))
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
                    self.lowering
                        .emit_error(span, "printf only supports raw pointers to byte strings");
                    "%s"
                }
            }
            _ => {
                if self.lowering.is_opaque_ty(ty) {
                    "%s"
                } else {
                    self.lowering
                        .emit_error(span, "printf argument type is not supported");
                    "%s"
                }
            }
        };
        Ok(spec.to_string())
    }

    fn is_c_string_ptr(&self, ty: &Ty) -> bool {
        matches!(ty.kind, TyKind::Int(IntTy::I8) | TyKind::Uint(UintTy::U8))
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
        let layout = match self.lowering.struct_layout_for_instance(
            struct_ref.def_id,
            &struct_ref.args,
            span,
        ) {
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
                // Slices are fat pointers (data + length).
                Some(16)
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
                if let TyKind::Adt(_, _) = &ty.kind {
                    if let Some(layout) = self.lowering.struct_layout_for_ty(ty) {
                        let mut total = 0u64;
                        for field in &layout.field_tys {
                            let size = match self.compute_ty_size(span, field) {
                                Some(value) => value,
                                None => return None,
                            };
                            total = total.saturating_add(size);
                        }
                        return Some(total);
                    }
                }
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
                    self.lowering
                        .emit_error(expr.span, "dereference target is not a place expression");
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
            hir::ExprKind::Cast(inner, ty) => {
                let Some(mut place_info) = self.lower_place(inner)? else {
                    return Ok(None);
                };
                let cast_ty = self.lower_type_expr(ty);
                place_info.ty = cast_ty.clone();
                place_info.struct_def = self.struct_def_from_ty(&cast_ty);
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

                let (field_index, field_info) = match self.lowering.struct_field(
                    struct_def,
                    &base_ty,
                    field.as_str(),
                    expr.span,
                ) {
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

                if self.is_list_container(&base_ty) || self.is_map_container(&base_ty) {
                    return Ok(None);
                }

                let element_ty = match &base_ty.kind {
                    TyKind::Array(elem, _) => *elem.clone(),
                    TyKind::Slice(elem) => *elem.clone(),
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
                let container_kind = match &value.operand {
                    mir::Operand::Constant(constant) => match &constant.literal {
                        mir::ConstantKind::Val(mir::ConstValue::List { elements, elem_ty }, _) => {
                            Some(mir::ContainerKind::List {
                                elem_ty: elem_ty.clone(),
                                len: elements.len() as u64,
                            })
                        }
                        mir::ConstantKind::Val(
                            mir::ConstValue::Map {
                                entries,
                                key_ty,
                                value_ty,
                            },
                            _,
                        ) => Some(mir::ContainerKind::Map {
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                            len: entries.len() as u64,
                        }),
                        _ => None,
                    },
                    _ => None,
                };
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
                    if let Some(kind) = container_kind {
                        self.container_locals.insert(assignment_place.local, kind);
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
            hir::ExprKind::Return(value) => {
                self.lower_return(expr.span, value.as_deref())?;
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
                IntrinsicCallKind::Format => {
                    let (format, args) = self.prepare_format_call(call, expr.span)?;
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicCallKind::Format,
                                format,
                                args,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    return Ok(());
                }
                IntrinsicCallKind::Panic => {
                    let unit_assign = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::Aggregate(mir::AggregateKind::Tuple, Vec::new()),
                        ),
                    };
                    self.push_statement(unit_assign);
                    self.emit_panic_intrinsic(call, expr.span)?;
                    return Ok(());
                }
                IntrinsicCallKind::CatchUnwind => {
                    self.lower_catch_unwind(expr, call, Some(place.clone()))?;
                    return Ok(());
                }
                IntrinsicCallKind::TimeNow => {
                    let args = &call.callargs;
                    if !args.is_empty() {
                        self.lowering
                            .emit_error(expr.span, "time::now intrinsic expects no arguments");
                    }
                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::IntrinsicCall {
                                kind: IntrinsicCallKind::TimeNow,
                                format: String::new(),
                                args: Vec::new(),
                            },
                        ),
                    };
                    self.push_statement(statement);
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
                let arg_values: Vec<&hir::Expr> = args.iter().map(|arg| &arg.value).collect();

                if (method_name.as_str() == "get_unchecked"
                    || method_name.as_str().ends_with("::get_unchecked"))
                    && args.len() == 1
                {
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        let mut resolved_path = path.clone();
                        self.resolve_self_path(&mut resolved_path);
                        let mut const_info = None;
                        let mut const_body_len = None;
                        if let Some(hir::Res::Def(def_id)) = &resolved_path.res {
                            if let Some(info) = self.lowering.const_values.get(def_id) {
                                const_info = Some(info.clone());
                            } else if let Some(item) = self.program.def_map.get(def_id) {
                                if let hir::ItemKind::Const(konst) = &item.kind {
                                    if let hir::ExprKind::Array(elements) = &konst.body.value.kind {
                                        const_body_len = Some(elements.len() as u64);
                                    }
                                    self.lowering.register_const_value(
                                        self.program,
                                        *def_id,
                                        konst,
                                    );
                                    if let Some(info) = self.lowering.const_values.get(def_id) {
                                        const_info = Some(info.clone());
                                    }
                                }
                            }
                        } else if resolved_path.segments.len() == 1 {
                            let name = resolved_path.segments[0].name.as_str();
                            for (def_id, item) in &self.program.def_map {
                                if let hir::ItemKind::Const(konst) = &item.kind {
                                    if konst.name.as_str() == name {
                                        if let hir::ExprKind::Array(elements) =
                                            &konst.body.value.kind
                                        {
                                            const_body_len = Some(elements.len() as u64);
                                        }
                                        self.lowering.register_const_value(
                                            self.program,
                                            *def_id,
                                            konst,
                                        );
                                        if let Some(info) = self.lowering.const_values.get(def_id) {
                                            const_info = Some(info.clone());
                                            break;
                                        }
                                    }
                                }
                            }
                        }

                        if let Some(const_info) = const_info {
                            if let mir::ConstantKind::Val(value, _) = &const_info.value.literal {
                                if let Some((constant, ty)) = self.lowering.const_index_value(
                                    self.program,
                                    expr.span,
                                    &const_info.value,
                                    &args[0].value,
                                ) {
                                    self.push_statement(mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place.clone(),
                                            mir::Rvalue::Use(mir::Operand::Constant(constant)),
                                        ),
                                    });
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = ty.clone();
                                    }
                                    return Ok(());
                                }
                                let mut map_len: Option<u64> = None;
                                let mut map_key_ty: Option<Ty> = None;
                                let mut map_value_ty: Option<Ty> = None;
                                match value {
                                    mir::ConstValue::Map {
                                        entries,
                                        key_ty,
                                        value_ty,
                                    } => {
                                        map_len = Some(entries.len() as u64);
                                        map_key_ty = Some(key_ty.clone());
                                        map_value_ty = Some(value_ty.clone());
                                    }
                                    mir::ConstValue::List { elements, elem_ty } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_len = Some(elements.len() as u64);
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                            }
                                        }
                                    }
                                    mir::ConstValue::Array(elements) => {
                                        if let TyKind::Array(elem_ty, _) = &const_info.ty.kind {
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_len = Some(elements.len() as u64);
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                                if map_len.is_none() {
                                    map_len = const_body_len;
                                }

                                if map_key_ty.is_none() {
                                    let key_operand = self.lower_operand(&args[0].value, None)?;
                                    map_key_ty = Some(key_operand.ty);
                                }
                                if map_value_ty.is_none() {
                                    map_value_ty = Some(expected_ty.clone());
                                }

                                if let (Some(key_ty), Some(value_ty), Some(len)) =
                                    (map_key_ty, map_value_ty, map_len)
                                {
                                    if len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len,
                                                    },
                                                    container: mir::Operand::Constant(
                                                        const_info.value.clone(),
                                                    ),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }

                    if let Ok(receiver_info) = self.lower_operand(receiver, None) {
                        if let mir::Operand::Constant(constant) = &receiver_info.operand {
                            if let mir::ConstantKind::Val(
                                mir::ConstValue::Map {
                                    entries,
                                    key_ty,
                                    value_ty,
                                },
                                _,
                            ) = &constant.literal
                            {
                                let key_operand =
                                    self.lower_operand(arg_values[0], Some(key_ty))?;
                                let kind = mir::ContainerKind::Map {
                                    key_ty: key_ty.clone(),
                                    value_ty: value_ty.clone(),
                                    len: entries.len() as u64,
                                };
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind,
                                            container: receiver_info.operand.clone(),
                                            key: key_operand.operand,
                                        },
                                    ),
                                });

                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                        if let Some(local_id) = self.local_id_from_expr(receiver) {
                            if let Some(container_kind) =
                                self.container_locals.get(&local_id).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        let local_place = mir::Place::from_local(local_id);
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: mir::Operand::copy(local_place),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }
                        if let mir::Operand::Copy(place) = &receiver_info.operand {
                            if let Some(container_kind) =
                                self.container_locals.get(&place.local).cloned()
                            {
                                let mut map_key_ty = None;
                                let mut map_value_ty = None;
                                let mut map_len = 0;
                                match container_kind {
                                    mir::ContainerKind::Map {
                                        key_ty,
                                        value_ty,
                                        len,
                                    } => {
                                        map_key_ty = Some(key_ty);
                                        map_value_ty = Some(value_ty);
                                        map_len = len;
                                    }
                                    mir::ContainerKind::List { elem_ty, len } => {
                                        if let TyKind::Tuple(fields) = &elem_ty.kind {
                                            if fields.len() == 2 {
                                                map_key_ty = Some((*fields[0].clone()).clone());
                                                map_value_ty = Some((*fields[1].clone()).clone());
                                                map_len = len;
                                            }
                                        }
                                    }
                                }
                                if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                                    if map_len != 0 {
                                        let key_operand =
                                            self.lower_operand(&args[0].value, Some(&key_ty))?;
                                        self.push_statement(mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place.clone(),
                                                mir::Rvalue::ContainerGet {
                                                    kind: mir::ContainerKind::Map {
                                                        key_ty: key_ty.clone(),
                                                        value_ty: value_ty.clone(),
                                                        len: map_len,
                                                    },
                                                    container: receiver_info.operand.clone(),
                                                    key: key_operand.operand,
                                                },
                                            ),
                                        });

                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = value_ty.clone();
                                        }
                                        return Ok(());
                                    }
                                }
                            }
                        }

                        let mut map_len: Option<u64> = None;
                        let mut map_key_ty: Option<Ty> = None;
                        let mut map_value_ty: Option<Ty> = None;
                        let receiver_ty = match &receiver_info.ty.kind {
                            TyKind::Ref(_, inner, _) => inner.as_ref(),
                            _ => &receiver_info.ty,
                        };
                        match &receiver_ty.kind {
                            TyKind::Array(elem_ty, len) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                        map_len = self.const_kind_to_u64(expr.span, len);
                                    }
                                }
                            }
                            TyKind::Slice(elem_ty) => {
                                if let TyKind::Tuple(fields) = &elem_ty.kind {
                                    if fields.len() == 2 {
                                        map_key_ty = Some((*fields[0].clone()).clone());
                                        map_value_ty = Some((*fields[1].clone()).clone());
                                    }
                                }
                            }
                            _ => {}
                        }

                        if map_len.is_none() {
                            if let mir::Operand::Constant(constant) = &receiver_info.operand {
                                if let mir::ConstantKind::Val(value, _) = &constant.literal {
                                    match value {
                                        mir::ConstValue::Map {
                                            entries,
                                            key_ty,
                                            value_ty,
                                        } => {
                                            map_len = Some(entries.len() as u64);
                                            map_key_ty = Some(key_ty.clone());
                                            map_value_ty = Some(value_ty.clone());
                                        }
                                        mir::ConstValue::List { elements, elem_ty } => {
                                            map_len = Some(elements.len() as u64);
                                            if let TyKind::Tuple(fields) = &elem_ty.kind {
                                                if fields.len() == 2 {
                                                    map_key_ty = Some((*fields[0].clone()).clone());
                                                    map_value_ty =
                                                        Some((*fields[1].clone()).clone());
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }

                        if let (Some(key_ty), Some(value_ty)) = (map_key_ty, map_value_ty) {
                            let len = map_len.unwrap_or(0);
                            if len != 0 {
                                let key_operand =
                                    self.lower_operand(&args[0].value, Some(&key_ty))?;
                                self.push_statement(mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place.clone(),
                                        mir::Rvalue::ContainerGet {
                                            kind: mir::ContainerKind::Map {
                                                key_ty: key_ty.clone(),
                                                value_ty: value_ty.clone(),
                                                len,
                                            },
                                            container: receiver_info.operand,
                                            key: key_operand.operand,
                                        },
                                    ),
                                });
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = value_ty.clone();
                                }
                                return Ok(());
                            }
                        }
                    }
                }

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
                    } else if let Some(enum_def) = self.enum_def_from_ty(&place_info.ty) {
                        if let Some(enum_entry) = self.lowering.enum_defs.get(&enum_def) {
                            if let Some(info) =
                                self.lowering.struct_methods.get(&enum_entry.name).and_then(
                                    |methods| methods.get(&String::from(method_key.clone())),
                                )
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
                        let operand = self.lower_operand(&arg.value, expected)?;
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
                            if let Some(def) = self.lowering.method_defs.get(&method_key).cloned() {
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
                                    let operand = self.lower_operand(&arg.value, expected)?;
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
                    } else if let Some(enum_def) = self.enum_def_from_ty(&place_info.ty) {
                        if let Some(enum_entry) = self.lowering.enum_defs.get(&enum_def) {
                            let method_key =
                                format!("{}::{}", enum_entry.name, method_name.as_str());
                            if let Some(def) = self.lowering.method_defs.get(&method_key).cloned() {
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
                                    let operand = self.lower_operand(&arg.value, expected)?;
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
                    if let Some(local_id) = self.local_id_from_expr(receiver) {
                        if let Some(kind) = self.container_locals.get(&local_id).cloned() {
                            let len_ty = Ty {
                                kind: TyKind::Uint(UintTy::Usize),
                            };
                            if (place.local as usize) < self.locals.len() {
                                self.locals[place.local as usize].ty = len_ty.clone();
                            }
                            let statement = mir::Statement {
                                source_info: expr.span,
                                kind: mir::StatementKind::Assign(
                                    place,
                                    mir::Rvalue::ContainerLen {
                                        kind,
                                        container: mir::Operand::copy(mir::Place::from_local(
                                            local_id,
                                        )),
                                    },
                                ),
                            };
                            self.push_statement(statement);
                            return Ok(());
                        }
                        let array_len = self.locals.get(local_id as usize).and_then(|local| {
                            if let TyKind::Array(_, len) = &local.ty.kind {
                                Some(len.clone())
                            } else {
                                None
                            }
                        });
                        if let Some(len) = array_len {
                            if let Some(len) = self.const_kind_to_u64(expr.span, &len) {
                                let len_ty = Ty {
                                    kind: TyKind::Uint(UintTy::Usize),
                                };
                                if (place.local as usize) < self.locals.len() {
                                    self.locals[place.local as usize].ty = len_ty.clone();
                                }
                                let statement = mir::Statement {
                                    source_info: expr.span,
                                    kind: mir::StatementKind::Assign(
                                        place,
                                        mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                                            span: expr.span,
                                            user_ty: None,
                                            literal: mir::ConstantKind::UInt(len),
                                        })),
                                    ),
                                };
                                self.push_statement(statement);
                                return Ok(());
                            }
                        }
                    }
                    if let hir::ExprKind::Path(path) = &receiver.kind {
                        if let Some(hir::Res::Def(def_id)) = &path.res {
                            if let Some(const_info) = self.lowering.const_values.get(def_id) {
                                if let Some(len) =
                                    self.lowering.const_len_from_constant(&const_info.value)
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(len),
                                                },
                                            )),
                                        ),
                                    };
                                    self.push_statement(statement);
                                    return Ok(());
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = &const_info.ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
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
                                if let Some(constant) = self.lowering.lower_const_expr(
                                    self.program,
                                    &konst.body.value,
                                    Some(&ty),
                                    None,
                                ) {
                                    if let Some(len) =
                                        self.lowering.const_len_from_constant(&constant)
                                    {
                                        let len_ty = Ty {
                                            kind: TyKind::Uint(UintTy::Usize),
                                        };
                                        if (place.local as usize) < self.locals.len() {
                                            self.locals[place.local as usize].ty = len_ty.clone();
                                        }
                                        let statement = mir::Statement {
                                            source_info: expr.span,
                                            kind: mir::StatementKind::Assign(
                                                place,
                                                mir::Rvalue::Use(mir::Operand::Constant(
                                                    mir::Constant {
                                                        span: expr.span,
                                                        user_ty: None,
                                                        literal: mir::ConstantKind::UInt(len),
                                                    },
                                                )),
                                            ),
                                        };
                                        self.push_statement(statement);
                                        return Ok(());
                                    }
                                }
                                if let TyKind::Array(
                                    _,
                                    ConstKind::Value(ConstValue::Scalar(Scalar::Int(len))),
                                ) = ty.kind
                                {
                                    let len_ty = Ty {
                                        kind: TyKind::Uint(UintTy::Usize),
                                    };
                                    if (place.local as usize) < self.locals.len() {
                                        self.locals[place.local as usize].ty = len_ty.clone();
                                    }
                                    let statement = mir::Statement {
                                        source_info: expr.span,
                                        kind: mir::StatementKind::Assign(
                                            place,
                                            mir::Rvalue::Use(mir::Operand::Constant(
                                                mir::Constant {
                                                    span: expr.span,
                                                    user_ty: None,
                                                    literal: mir::ConstantKind::UInt(
                                                        len.data as u64,
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
                    let lowered = self.lower_operand(&arg.value, None)?;
                    input_tys.push(lowered.ty.clone());
                    lowered_args.push(lowered.operand);
                }

                let mut result_ty = expected_ty.clone();
                let mut inferred_output: Option<Ty> = None;
                for methods in self.lowering.struct_methods.values() {
                    if let Some(info) = methods.get(method_name.as_str()) {
                        if let Some(existing) = inferred_output.as_ref() {
                            if existing != &info.sig.output {
                                inferred_output = None;
                                break;
                            }
                        } else {
                            inferred_output = Some(info.sig.output.clone());
                        }
                    }
                }
                if let Some(output) = inferred_output {
                    result_ty = output;
                }
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
                                    let existing = self.locals[place.local as usize].ty.clone();
                                    if MirLowering::is_unit_ty(&existing)
                                        || matches!(
                                            existing.kind,
                                            TyKind::Infer(_) | TyKind::Error(_)
                                        )
                                    {
                                        self.locals[place.local as usize].ty =
                                            expected_input.clone();
                                    }
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

                return Ok(());
            }
            hir::ExprKind::Call(callee, args) => {
                self.lower_call(expr, callee, args, Some((place, expected_ty.clone())))?;
            }
            hir::ExprKind::Array(elements) => {
                if self.is_map_container(expected_ty) {
                    let mut entries = Vec::with_capacity(elements.len());
                    let mut key_ty: Option<Ty> = None;
                    let mut value_ty: Option<Ty> = None;

                    for element in elements {
                        let hir::ExprKind::Array(entry) = &element.kind else {
                            self.lowering
                                .emit_error(element.span, "HashMap literal expects array entries");
                            continue;
                        };
                        if entry.len() != 2 {
                            self.lowering.emit_error(
                                element.span,
                                "HashMap literal expects array entries of length 2",
                            );
                            continue;
                        }
                        let key_operand = self.lower_operand(&entry[0], None)?;
                        let value_operand = self.lower_operand(&entry[1], None)?;
                        if key_ty.is_none() {
                            key_ty = Some(key_operand.ty.clone());
                        }
                        if value_ty.is_none() {
                            value_ty = Some(value_operand.ty.clone());
                        }
                        entries.push((key_operand.operand, value_operand.operand));
                    }

                    let key_ty = key_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let value_ty = value_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::Map {
                        key_ty: key_ty.clone(),
                        value_ty: value_ty.clone(),
                        len: entries.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerMapLiteral {
                                kind: kind.clone(),
                                entries,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                if self.is_list_container(expected_ty) {
                    let mut operands = Vec::with_capacity(elements.len());
                    let mut elem_ty: Option<Ty> = None;
                    for element in elements {
                        let lowered = self.lower_operand(element, None)?;
                        if elem_ty.is_none() {
                            elem_ty = Some(lowered.ty.clone());
                        }
                        operands.push(lowered.operand);
                    }

                    let elem_ty = elem_ty.unwrap_or_else(|| self.lowering.error_ty());
                    let kind = mir::ContainerKind::List {
                        elem_ty: elem_ty.clone(),
                        len: operands.len() as u64,
                    };

                    let statement = mir::Statement {
                        source_info: expr.span,
                        kind: mir::StatementKind::Assign(
                            place.clone(),
                            mir::Rvalue::ContainerLiteral {
                                kind: kind.clone(),
                                elements: operands,
                            },
                        ),
                    };
                    self.push_statement(statement);
                    if place.projection.is_empty() {
                        self.locals[place.local as usize].ty = expected_ty.clone();
                        self.container_locals.insert(place.local, kind);
                    }
                    return Ok(());
                }

                let mut element_ty = self.expect_array_element_ty(expected_ty);
                let mut operands = Vec::with_capacity(elements.len());
                let mut element_types = Vec::with_capacity(elements.len());
                let mut heterogeneous = false;
                if let Some(elem_ty) = element_ty.clone() {
                    for element in elements {
                        let lowered = self.lower_operand(element, Some(&elem_ty))?;
                        if lowered.ty != elem_ty {
                            heterogeneous = true;
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                } else {
                    for element in elements {
                        let lowered = self.lower_operand(element, None)?;
                        if element_ty.is_none() {
                            element_ty = Some(lowered.ty.clone());
                        } else if let Some(existing) = element_ty.as_ref() {
                            if &lowered.ty != existing {
                                heterogeneous = true;
                            }
                        }
                        element_types.push(lowered.ty.clone());
                        operands.push(lowered.operand);
                    }
                }

                let expected_is_array = matches!(&expected_ty.kind, TyKind::Array(_, _))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _) if matches!(inner.kind, TyKind::Array(_, _))
                    );
                if heterogeneous && expected_is_array {
                    self.lowering
                        .emit_error(expr.span, "array literal elements have mismatched types");
                }

                let element_ty = element_ty.unwrap_or_else(|| {
                    self.lowering
                        .emit_error(expr.span, "array expression expected array type");
                    self.lowering.error_ty()
                });

                let expected_is_slice = matches!(&expected_ty.kind, TyKind::Slice(_))
                    || matches!(
                        &expected_ty.kind,
                        TyKind::Ref(_, inner, _)
                            if matches!(inner.kind, TyKind::Slice(_))
                    );
                if (expected_is_slice || matches!(expected_ty.kind, TyKind::Error(_)))
                    && place.projection.is_empty()
                {
                    let array_ty = Ty {
                        kind: TyKind::Array(
                            Box::new(element_ty.clone()),
                            ConstKind::Value(ConstValue::Scalar(Scalar::Int(ScalarInt {
                                data: elements.len() as u128,
                                size: 8,
                            }))),
                        ),
                    };
                    if let Some(local) = self.locals.get_mut(place.local as usize) {
                        local.ty = array_ty;
                    }
                }

                let aggregate_kind = if heterogeneous && !expected_is_array {
                    if place.projection.is_empty() {
                        let tuple_ty = Ty {
                            kind: TyKind::Tuple(element_types.into_iter().map(Box::new).collect()),
                        };
                        if let Some(local) = self.locals.get_mut(place.local as usize) {
                            local.ty = tuple_ty;
                        }
                    }
                    mir::AggregateKind::Tuple
                } else {
                    mir::AggregateKind::Array(element_ty.clone())
                };

                let statement = mir::Statement {
                    source_info: expr.span,
                    kind: mir::StatementKind::Assign(
                        place.clone(),
                        mir::Rvalue::Aggregate(aggregate_kind, operands),
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

    fn container_type_name(&self, ty: &Ty) -> Option<String> {
        self.lowering.display_type_name(ty)
    }

    fn is_list_container(&self, ty: &Ty) -> bool {
        if matches!(ty.kind, TyKind::Slice(_)) {
            return true;
        }
        self.container_type_name(ty)
            .map(|name| {
                let tail = name.split("::").last().unwrap_or(name.as_str());
                matches!(tail, "Vec" | "List" | "list")
            })
            .unwrap_or(false)
    }

    fn is_map_container(&self, ty: &Ty) -> bool {
        self.container_type_name(ty)
            .map(|name| {
                let tail = name.split("::").last().unwrap_or(name.as_str());
                tail == "HashMap"
            })
            .unwrap_or(false)
    }

    fn local_id_from_expr(&self, expr: &hir::Expr) -> Option<mir::LocalId> {
        let hir::ExprKind::Path(path) = &expr.kind else {
            return None;
        };
        if let Some(hir::Res::Local(hir_id)) = &path.res {
            return self.local_map.get(hir_id).copied();
        }
        path.segments
            .first()
            .filter(|_| path.segments.len() == 1)
            .and_then(|seg| self.fallback_locals.get(seg.name.as_str()).copied())
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
