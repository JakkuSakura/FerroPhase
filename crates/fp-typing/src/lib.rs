use std::collections::{HashMap, HashSet};
use fp_core::config;

pub mod typing;
pub use typing::types::{TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome};

use fp_core::ast::*;
use crate::typing::scheme::{TypeScheme, SchemeType};
use fp_core::ast::{Ident, Locator};
use fp_core::ast::{Pattern, PatternKind};
use fp_core::context::SharedScopedContext;
use fp_core::error::{Error, Result};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{BinOpKind, UnOpKind};
fn typing_error(msg: impl Into<String>) -> Error {
    Error::from(msg.into())
}

pub(crate) type TypeVarId = usize;

fn detect_lossy_mode() -> bool { config::lossy_mode() }

use crate::typing::unify::{FunctionTerm, TypeTerm, TypeVar, TypeVarKind};

// TypeScheme moved to typing/scheme.rs

#[derive(Clone, Debug)]
struct MethodRecord {
    receiver_ty: Option<Ty>,
    scheme: Option<TypeScheme>,
}

#[derive(Clone, Debug)]
struct ImplContext {
    struct_name: String,
    self_ty: Ty,
}

// SchemeType moved to typing/scheme.rs

#[derive(Clone)]
enum EnvEntry {
    Mono(TypeVarId),
    Poly(TypeScheme),
}

struct PatternBinding {
    name: String,
    var: TypeVarId,
}

struct PatternInfo {
    var: TypeVarId,
    bindings: Vec<PatternBinding>,
}

impl PatternInfo {
    fn new(var: TypeVarId) -> Self {
        Self {
            var,
            bindings: Vec::new(),
        }
    }

    fn with_binding(mut self, name: String, var: TypeVarId) -> Self {
        self.bindings.push(PatternBinding { name, var });
        self
    }

    #[allow(dead_code)]
    fn extend_bindings(&mut self, other: PatternInfo) {
        self.bindings.extend(other.bindings);
    }
}

// Typing diagnostics/outcome are defined in typing/types.rs and re-exported above.

struct FunctionTypeInfo {
    params: Vec<TypeVarId>,
    ret: TypeVarId,
}

struct LoopContext {
    result_var: TypeVarId,
    saw_break: bool,
}

impl LoopContext {
    fn new(result_var: TypeVarId) -> Self {
        Self {
            result_var,
            saw_break: false,
        }
    }
}

pub struct AstTypeInferencer<'ctx> {
    ctx: Option<&'ctx SharedScopedContext>,
    type_vars: Vec<TypeVar>,
    env: Vec<HashMap<String, EnvEntry>>,
    generic_scopes: Vec<HashSet<String>>,
    struct_defs: HashMap<String, TypeStruct>,
    enum_defs: HashMap<String, TypeEnum>,
    enum_variants: HashMap<String, Vec<String>>,
    struct_methods: HashMap<String, HashMap<String, MethodRecord>>,
    impl_stack: Vec<Option<ImplContext>>,
    current_level: usize,
    diagnostics: Vec<TypingDiagnostic>,
    has_errors: bool,
    literal_ints: HashSet<TypeVarId>,
    loop_stack: Vec<LoopContext>,
    lossy_mode: bool,
}

impl<'ctx> AstTypeInferencer<'ctx> {
    pub fn new() -> Self {
        Self {
            ctx: None,
            type_vars: Vec::new(),
            env: vec![HashMap::new()],
            generic_scopes: vec![HashSet::new()],
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_variants: HashMap::new(),
            struct_methods: HashMap::new(),
            impl_stack: Vec::new(),
            current_level: 0,
            diagnostics: Vec::new(),
            has_errors: false,
            literal_ints: HashSet::new(),
            loop_stack: Vec::new(),
            lossy_mode: detect_lossy_mode(),
        }
    }

    pub fn with_context(mut self, ctx: &'ctx SharedScopedContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn infer(&mut self, node: &mut Node) -> Result<TypingOutcome> {
        match node.kind_mut() {
            NodeKind::Expr(expr) => {
                let var = self.infer_expr(expr)?;
                let ty = self.resolve_to_ty(var)?;
                node.set_ty(ty);
            }
            NodeKind::Item(item) => {
                self.predeclare_item(item);
                self.infer_item(item)?;
                let ty = item.ty().cloned().unwrap_or_else(|| Ty::Unit(TypeUnit));
                node.set_ty(ty);
            }
            NodeKind::File(file) => {
                for item in &file.items {
                    self.predeclare_item(item);
                }
                for item in &mut file.items {
                    self.infer_item(item)?;
                }
                node.set_ty(Ty::Unit(TypeUnit));
            }
            NodeKind::Query(_) => {
                node.set_ty(Ty::any());
            }
            NodeKind::Schema(_) => {
                node.set_ty(Ty::any());
            }
            NodeKind::Workspace(_) => {
                node.set_ty(Ty::any());
            }
        }
        Ok(self.finish())
    }

    /// Initialize the typer with declarations from a node without doing full inference.
    /// This is useful for preparing the typer for incremental type inference.
    pub fn initialize_from_node(&mut self, node: &Node) {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.predeclare_item(item);
                }
            }
            NodeKind::Item(item) => {
                self.predeclare_item(item);
            }
            NodeKind::Expr(_) => {
                // Nothing to predeclare for expressions
            }
            NodeKind::Query(_) | NodeKind::Schema(_) => {
                // Non-AST documents do not participate in type inference yet.
            }
            NodeKind::Workspace(_) => {}
        }
    }

    fn finish(&mut self) -> TypingOutcome {
        TypingOutcome {
            diagnostics: std::mem::take(&mut self.diagnostics),
            has_errors: std::mem::replace(&mut self.has_errors, false),
        }
    }

    fn resolve_impl_context(&mut self, self_ty: &Expr) -> Option<ImplContext> {
        match self.struct_name_from_expr(self_ty) {
            Some(name) => {
                if let Some(def) = self.struct_defs.get(&name).cloned() {
                    Some(ImplContext {
                        struct_name: name,
                        self_ty: Ty::Struct(def),
                    })
                } else {
                    self.emit_error(format!("impl target {} is not a known struct", name));
                    None
                }
            }
            None => {
                self.emit_error("impl self type must resolve to a struct");
                None
            }
        }
    }

    fn ty_for_receiver(&self, ctx: &ImplContext, receiver: &FunctionParamReceiver) -> Ty {
        match receiver {
            FunctionParamReceiver::Implicit
            | FunctionParamReceiver::Value
            | FunctionParamReceiver::MutValue => ctx.self_ty.clone(),
            FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                }
                .into(),
            ),
            FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                }
                .into(),
            ),
        }
    }

    fn register_method_stub(&mut self, ctx: &ImplContext, func: &ItemDefFunction) {
        let receiver_ty = func
            .sig
            .receiver
            .as_ref()
            .map(|receiver| self.ty_for_receiver(ctx, receiver));
        let entry = self
            .struct_methods
            .entry(ctx.struct_name.clone())
            .or_default();
        entry
            .entry(func.name.as_str().to_string())
            .or_insert(MethodRecord {
                receiver_ty,
                scheme: None,
            });
    }

    fn peel_reference(mut ty: Ty) -> Ty {
        loop {
            match ty {
                Ty::Reference(reference) => {
                    ty = (*reference.ty).clone();
                }
                other => return other,
            }
        }
    }

    fn predeclare_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::DefStruct(def) => {
                self.struct_defs
                    .insert(def.name.as_str().to_string(), def.value.clone());
                self.register_symbol(&def.name);
            }
            ItemKind::DefEnum(def) => {
                let enum_name = def.name.as_str().to_string();
                self.enum_defs.insert(enum_name.clone(), def.value.clone());
                self.register_symbol(&def.name);

                let mut variant_keys = Vec::new();
                for variant in &def.value.variants {
                    let qualified = format!("{}::{}", enum_name, variant.name.as_str());
                    variant_keys.push(qualified.clone());
                    if self.lookup_env_var(&qualified).is_none() {
                        let var = self.fresh_type_var();
                        self.insert_env(qualified.clone(), EnvEntry::Mono(var));
                    }
                }
                self.enum_variants.insert(enum_name, variant_keys);
            }
            ItemKind::DefConst(def) => {
                self.register_symbol(&def.name);
            }
            ItemKind::DefStatic(def) => {
                self.register_symbol(&def.name);
                if let Ty::Struct(ty) = &def.ty {
                    self.struct_defs
                        .insert(ty.name.as_str().to_string(), ty.clone());
                }
            }
            ItemKind::DefFunction(def) => {
                let fn_var = self.symbol_var(&def.name);
                self.prebind_function_signature(def, fn_var);
            }
            ItemKind::DeclFunction(decl) => {
                self.register_symbol(&decl.name);
            }
            ItemKind::Module(module) => {
                self.enter_scope();
                for child in &module.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty);
                self.impl_stack.push(ctx.clone());
                self.enter_scope();
                if let Some(ref ctx) = ctx {
                    for child in &impl_block.items {
                        if let ItemKind::DefFunction(func) = child.kind() {
                            self.register_method_stub(ctx, func);
                        }
                    }
                }
                for child in &impl_block.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
                self.impl_stack.pop();
            }
            ItemKind::Expr(expr) => {
                if let ExprKind::Struct(struct_expr) = expr.kind() {
                    if let Some(name) = self.struct_name_from_expr(&struct_expr.name) {
                        if let Some(def) = self.struct_defs.get(&name).cloned() {
                            self.struct_defs.insert(name, def);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn prebind_function_signature(&mut self, func: &ItemDefFunction, fn_var: TypeVarId) {
        if !func.sig.generics_params.is_empty() || func.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        if matches!(
            self.type_vars[root].kind,
            TypeVarKind::Bound(TypeTerm::Function(_))
        ) {
            return;
        }

        let mut param_vars = Vec::new();
        for param in &func.sig.params {
            match self.type_from_ast_ty(&param.ty) {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &func.sig.ret_ty {
            match self.type_from_ast_ty(ret_ty) {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, TypeTerm::Unit);
            unit
        };

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
    }

    fn infer_item(&mut self, item: &mut Item) -> Result<()> {
        let ty = match item.kind_mut() {
            ItemKind::DefStruct(def) => {
                let ty = Ty::Struct(def.value.clone());
                let placeholder = self.symbol_var(&def.name);
                let var = self.type_from_ast_ty(&ty)?;
                self.unify(placeholder, var)?;
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefEnum(def) => {
                let ty = Ty::Enum(def.value.clone());
                let placeholder = self.symbol_var(&def.name);
                let var = self.type_from_ast_ty(&ty)?;
                self.unify(placeholder, var)?;
                self.generalize_symbol(def.name.as_str(), placeholder)?;

                let enum_name = def.name.as_str().to_string();
                if let Some(variant_keys) = self.enum_variants.get(&enum_name).cloned() {
                    let enum_var = placeholder;
                    for qualified in variant_keys {
                        if let Some(variant_var) = self.lookup_env_var(qualified.as_str()) {
                            let _ = self.unify(enum_var, variant_var);
                            let _ = self.generalize_symbol(qualified.as_str(), variant_var);
                        }
                    }
                }

                ty
            }
            ItemKind::DefConst(def) => {
                let placeholder = self.symbol_var(&def.name);
                let expr_var = {
                    let mut value = def.value.as_mut();
                    self.infer_expr(&mut value)?
                };

                if let Some(annot) = &def.ty {
                    let annot_var = self.type_from_ast_ty(annot)?;
                    self.unify(expr_var, annot_var)?;
                }

                self.unify(placeholder, expr_var)?;
                let ty = self.resolve_to_ty(expr_var)?;
                def.ty_annotation = Some(ty.clone());
                def.ty.get_or_insert(ty.clone());
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefStatic(def) => {
                let placeholder = self.symbol_var(&def.name);
                let expr_var = {
                    let mut value = def.value.as_mut();
                    self.infer_expr(&mut value)?
                };
                let ty_var = self.type_from_ast_ty(&def.ty)?;
                self.unify(expr_var, ty_var)?;
                self.unify(placeholder, expr_var)?;
                let ty = self.resolve_to_ty(expr_var)?;
                def.ty_annotation = Some(ty.clone());
                self.generalize_symbol(def.name.as_str(), placeholder)?;
                ty
            }
            ItemKind::DefFunction(func) => self.infer_function(func)?,
            ItemKind::DeclConst(decl) => {
                let ty = decl.ty.clone();
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclStatic(decl) => {
                let ty = decl.ty.clone();
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclType(decl) => {
                let ty = Ty::TypeBounds(decl.bounds.clone());
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::DeclFunction(decl) => {
                let ty = self.ty_from_function_signature(&decl.sig)?;
                decl.ty_annotation = Some(ty.clone());
                ty
            }
            ItemKind::Module(module) => {
                self.enter_scope();
                for child in &module.items {
                    self.predeclare_item(child);
                }
                for child in &mut module.items {
                    self.infer_item(child)?;
                }
                self.exit_scope();
                Ty::Unit(TypeUnit)
            }
            ItemKind::Import(_) => Ty::Unit(TypeUnit),
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty);
                self.impl_stack.push(ctx.clone());
                self.enter_scope();
                for child in &impl_block.items {
                    self.predeclare_item(child);
                }
                for child in &mut impl_block.items {
                    self.infer_item(child)?;
                }
                self.exit_scope();
                self.impl_stack.pop();
                Ty::Unit(TypeUnit)
            }
            ItemKind::Expr(expr) => {
                let var = self.infer_expr(expr)?;
                self.resolve_to_ty(var)?
            }
            _ => {
                self.emit_error("type inference for item not implemented");
                Ty::Unknown(TypeUnknown)
            }
        };

        item.set_ty(ty);
        Ok(())
    }

    fn infer_function(&mut self, func: &mut ItemDefFunction) -> Result<Ty> {
        let fn_var = self.symbol_var(&func.name);
        let param_count = func.sig.params.len();
        let existing_signature = {
            let root = self.find(fn_var);
            match self.type_vars[root].kind.clone() {
                TypeVarKind::Bound(TypeTerm::Function(func_term)) => {
                    if func_term.params.len() == param_count {
                        Some(func_term)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        };

        self.enter_scope();

        let impl_ctx = self.impl_stack.last().cloned().flatten();
        let mut receiver_ty: Option<Ty> = None;
        if let Some(receiver) = func.sig.receiver.as_ref() {
            if let Some(ctx) = impl_ctx.as_ref() {
                let receiver_type = self.ty_for_receiver(ctx, receiver);
                let self_var = self.fresh_type_var();
                let expected = self.type_from_ast_ty(&receiver_type)?;
                self.unify(self_var, expected)?;
                self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
                receiver_ty = Some(receiver_type);
            } else {
                self.emit_error(format!(
                    "method {} defined without an impl context",
                    func.name
                ));
            }
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                // Ignore trait bounds for now; just register the symbol so it can be resolved.
                self.register_generic_param(param.name.as_str());
            }
        }

        let mut param_vars = Vec::new();
        for (idx, param) in func.sig.params.iter_mut().enumerate() {
            let var = existing_signature
                .as_ref()
                .and_then(|sig| sig.params.get(idx).cloned())
                .unwrap_or_else(|| self.fresh_type_var());
            let annot_var = self.type_from_ast_ty(&param.ty)?;
            self.unify(var, annot_var)?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var)?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = {
            let mut body = func.body.as_mut();
            self.infer_expr(&mut body)?
        };

        let ret_var = if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
            self.unify(existing, body_var)?;
            if let Some(ret) = &func.sig.ret_ty {
                let annot_var = self.type_from_ast_ty(ret)?;
                self.unify(existing, annot_var)?;
            }
            existing
        } else if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        let ret_ty = self.resolve_to_ty(ret_var.clone())?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        self.exit_scope();

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var)?);
        }

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars.clone(),
                ret: ret_var,
            }),
        );

        let scheme = self.generalize(fn_var)?;
        let scheme_env = scheme.clone();
        self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme_env));

        if let Some(ctx) = impl_ctx.as_ref() {
            let entry = self
                .struct_methods
                .entry(ctx.struct_name.clone())
                .or_default();
            let record = entry
                .entry(func.name.as_str().to_string())
                .or_insert(MethodRecord {
                    receiver_ty: receiver_ty.clone(),
                    scheme: None,
                });
            if record.receiver_ty.is_none() && receiver_ty.is_some() {
                record.receiver_ty = receiver_ty.clone();
            }
            record.scheme = Some(scheme.clone());
        }

        let func_ty = TypeFunction {
            params: param_tys.clone(),
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty.clone())),
        };

        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    // infer_expr moved to typing::infer_expr

    // infer_block moved to typing::infer_stmt

    // infer_if moved to typing::infer_stmt

    // infer_binop moved to typing::infer_expr

    // infer_unop moved to typing::infer_expr

    // infer_loop moved to typing::infer_stmt

    // infer_while moved to typing::infer_stmt

    fn infer_reference(&mut self, reference: &mut ExprReference) -> Result<TypeVarId> {
        let inner_var = self.infer_expr(reference.referee.as_mut())?;
        let reference_var = self.fresh_type_var();
        self.bind(reference_var, TypeTerm::Reference(inner_var));
        Ok(reference_var)
    }

    fn infer_dereference(&mut self, dereference: &mut ExprDereference) -> Result<TypeVarId> {
        let target_var = self.infer_expr(dereference.referee.as_mut())?;
        self.expect_reference(target_var, "dereference expression")
    }

    fn infer_index(&mut self, index: &mut ExprIndex) -> Result<TypeVarId> {
        let object_var = self.infer_expr(index.obj.as_mut())?;
        let idx_var = self.infer_expr(index.index.as_mut())?;
        self.ensure_integer(idx_var, "index expression")?;

        let elem_vec_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_vec_var));
        if self.unify(object_var, vec_var).is_ok() {
            return Ok(elem_vec_var);
        }

        let elem_slice_var = self.fresh_type_var();
        let slice_var = self.fresh_type_var();
        self.bind(slice_var, TypeTerm::Slice(elem_slice_var));
        if self.unify(object_var, slice_var).is_err() {
            self.emit_error("indexing is only supported on vector or slice types");
            return Ok(self.error_type_var());
        }
        Ok(elem_slice_var)
    }

    fn infer_range(&mut self, range: &mut ExprRange) -> Result<TypeVarId> {
        let element_var = self.fresh_type_var();

        if let Some(start) = range.start.as_mut() {
            let start_var = self.infer_expr(start)?;
            self.unify(element_var, start_var)?;
        }

        if let Some(end) = range.end.as_mut() {
            let end_var = self.infer_expr(end)?;
            self.unify(element_var, end_var)?;
        }

        if let Some(step) = range.step.as_mut() {
            let step_var = self.infer_expr(step)?;
            self.ensure_numeric(step_var, "range step")?;
        }

        self.ensure_numeric(element_var, "range bounds")?;

        let range_var = self.fresh_type_var();
        self.bind(range_var, TypeTerm::Vec(element_var));
        Ok(range_var)
    }

    fn infer_splat(&mut self, splat: &mut ExprSplat) -> Result<TypeVarId> {
        self.infer_expr(splat.iter.as_mut())
    }

    fn infer_splat_dict(&mut self, splat: &mut ExprSplatDict) -> Result<TypeVarId> {
        self.infer_expr(splat.dict.as_mut())
    }

    fn infer_intrinsic(&mut self, call: &mut ExprIntrinsicCall) -> Result<TypeVarId> {
        let mut arg_vars = Vec::new();

        match &mut call.payload {
            IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
            }
            IntrinsicCallPayload::Format { template } => {
                for arg in &mut template.args {
                    arg_vars.push(self.infer_expr(arg)?);
                }
                for kwarg in &mut template.kwargs {
                    arg_vars.push(self.infer_expr(&mut kwarg.value)?);
                }
            }
        }

        match call.kind {
            IntrinsicCallKind::ConstBlock => {
                if let Some(&body_var) = arg_vars.first() {
                    return Ok(body_var);
                }
                self.emit_error("const block intrinsic expects a body expression");
                return Ok(self.error_type_var());
            }
            IntrinsicCallKind::Break => {
                if arg_vars.len() > 1 {
                    self.emit_error("`break` accepts at most one value");
                }
                let value_var = if let Some(&var) = arg_vars.first() {
                    var
                } else {
                    self.unit_type_var()
                };

                let loop_var = if let Some(context) = self.loop_stack.last_mut() {
                    context.saw_break = true;
                    Some(context.result_var)
                } else {
                    None
                };

                if let Some(result_var) = loop_var {
                    self.unify(result_var, value_var)?;
                    return Ok(result_var);
                }

                self.emit_error("`break` used outside of a loop");
                return Ok(self.error_type_var());
            }
            IntrinsicCallKind::Continue => {
                if !arg_vars.is_empty() {
                    self.emit_error("`continue` does not accept a value");
                }
                if self.loop_stack.is_empty() {
                    self.emit_error("`continue` used outside of a loop");
                    return Ok(self.error_type_var());
                }
                return Ok(self.nothing_type_var());
            }
            IntrinsicCallKind::Return => {
                self.emit_error("`return` intrinsic must be lowered before typing");
                return Ok(self.error_type_var());
            }
            _ => {}
        }

        let result_var = self.fresh_type_var();
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::Len
            | IntrinsicCallKind::SizeOf
            | IntrinsicCallKind::FieldCount
            | IntrinsicCallKind::MethodCount
            | IntrinsicCallKind::StructSize => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(
                    result_var,
                    TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
                );
            }
            IntrinsicCallKind::DebugAssertions
            | IntrinsicCallKind::HasField
            | IntrinsicCallKind::HasMethod => {
                let expected = if matches!(call.kind, IntrinsicCallKind::DebugAssertions) {
                    0
                } else {
                    2
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::Bool));
            }
            IntrinsicCallKind::Input => {
                if arg_vars.len() > 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects at most 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::TypeName => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));
            }
            IntrinsicCallKind::ReflectFields => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Any);
            }
            IntrinsicCallKind::CreateStruct
            | IntrinsicCallKind::CloneStruct
            | IntrinsicCallKind::AddField
            | IntrinsicCallKind::FieldType => {
                let expected = match call.kind {
                    IntrinsicCallKind::AddField => 3,
                    IntrinsicCallKind::FieldType => 2,
                    _ => 1,
                };
                if arg_vars.len() != expected {
                    self.emit_error(format!(
                        "intrinsic {:?} expects {} argument(s), found {}",
                        call.kind,
                        expected,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Custom(Ty::Type(TypeType)));
            }
            IntrinsicCallKind::GenerateMethod => {
                if arg_vars.len() != 2 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 2 arguments, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            IntrinsicCallKind::CompileError => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Nothing);
            }
            IntrinsicCallKind::CompileWarning => {
                if arg_vars.len() != 1 {
                    self.emit_error(format!(
                        "intrinsic {:?} expects 1 argument, found {}",
                        call.kind,
                        arg_vars.len()
                    ));
                }
                self.bind(result_var, TypeTerm::Unit);
            }
            _ => {
                self.bind(result_var, TypeTerm::Any);
            }
        }

        Ok(result_var)
    }

    fn infer_closure(&mut self, closure: &mut ExprClosure) -> Result<TypeVarId> {
        self.enter_scope();
        let mut param_vars = Vec::new();
        for param in &mut closure.params {
            let info = self.infer_pattern(param)?;
            param_vars.push(info.var);
        }

        let body_var = self.infer_expr(closure.body.as_mut())?;
        let ret_var = if let Some(ret_ty) = &closure.ret_ty {
            let annot_var = self.type_from_ast_ty(ret_ty)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        let closure_var = self.fresh_type_var();
        self.bind(
            closure_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
        Ok(closure_var)
    }

    // infer_match moved to typing::infer_stmt

    fn infer_invoke(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if let Some(result) = self.try_infer_collection_call(invoke)? {
            return Ok(result);
        }

        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if let Some(ident) = locator.as_ident() {
                if ident.as_str() == "printf" {
                    return self.infer_builtin_printf(invoke);
                }
            }
        }

        let func_var = match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(var) = self.lookup_associated_function(locator)? {
                    var
                } else {
                    self.lookup_locator(locator)?
                }
            }
            ExprInvokeTarget::Expr(expr) => self.infer_expr(expr.as_mut())?,
            ExprInvokeTarget::Closure(_) => {
                let message = "invoking closure values is not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::BinOp(_) => {
                let message = "invoking binary operators as functions is not supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            ExprInvokeTarget::Type(ty) => self.type_from_ast_ty(ty)?,
            ExprInvokeTarget::Method(select) => {
                let obj_var = self.infer_expr(select.obj.as_mut())?;

                if let Some(result) =
                    self.try_infer_primitive_method(obj_var, &select.field, invoke.args.len())?
                {
                    return Ok(result);
                }

                if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                    if let Ok(obj_ty) = self.resolve_to_ty(obj_var) {
                        if Self::is_collection_with_len(&obj_ty) {
                            let result_var = self.fresh_type_var();
                            self.bind(
                                result_var,
                                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)),
                            );
                            return Ok(result_var);
                        }
                    }
                }
                self.lookup_struct_method(obj_var, &select.field)?
            }
        };

        let func_info = self.ensure_function(func_var, invoke.args.len())?;
        for (arg_expr, param_var) in invoke.args.iter_mut().zip(func_info.params.iter()) {
            let arg_var = self.infer_expr(arg_expr)?;
            self.unify(*param_var, arg_var)?;
        }
        Ok(func_info.ret)
    }

    fn try_infer_collection_call(&mut self, invoke: &mut ExprInvoke) -> Result<Option<TypeVarId>> {
        let locator = match &invoke.target {
            ExprInvokeTarget::Function(locator) => locator,
            _ => return Ok(None),
        };

        if Self::locator_matches_suffix(locator, &["Vec", "new"]) {
            return self.infer_vec_new(invoke).map(Some);
        }

        if Self::locator_matches_suffix(locator, &["Vec", "with_capacity"]) {
            return self.infer_vec_with_capacity(invoke).map(Some);
        }

        if Self::locator_matches_suffix(locator, &["Vec", "from"]) {
            return self.infer_vec_from(invoke).map(Some);
        }

        if Self::locator_matches_suffix(locator, &["HashMap", "new"]) {
            return self.infer_hashmap_new(invoke).map(Some);
        }

        if Self::locator_matches_suffix(locator, &["HashMap", "with_capacity"]) {
            return self.infer_hashmap_with_capacity(invoke).map(Some);
        }

        if Self::locator_matches_suffix(locator, &["HashMap", "from"]) {
            return self.infer_hashmap_from(invoke).map(Some);
        }

        Ok(None)
    }

    fn infer_vec_new(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::new does not take arguments");
        }

        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_vec_with_capacity(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr(&mut invoke.args[0])?;
            let expected = self.fresh_type_var();
            self.bind(
                expected,
                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
            );
            self.unify(capacity_var, expected)?;
        }

        let elem_var = self.fresh_type_var();
        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_vec_from(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("Vec::from expects a single iterable argument");
            let elem_var = self.fresh_type_var();
            let vec_var = self.fresh_type_var();
            self.bind(vec_var, TypeTerm::Vec(elem_var));
            return Ok(vec_var);
        }

        let elem_var = match invoke.args[0].kind_mut() {
            ExprKind::Array(array) => self.infer_vec_array_elements(array)?,
            ExprKind::ArrayRepeat(repeat) => self.infer_vec_array_repeat(repeat)?,
            _ => {
                let arg_var = self.infer_expr(&mut invoke.args[0])?;
                let elem_var = self.fresh_type_var();
                let vec_var = self.fresh_type_var();
                self.bind(vec_var, TypeTerm::Vec(elem_var));
                self.unify(arg_var, vec_var)?;
                elem_var
            }
        };

        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_vec_array_elements(&mut self, array: &mut ExprArray) -> Result<TypeVarId> {
        let mut iter = array.values.iter_mut();
        if let Some(first) = iter.next() {
            let first_var = self.infer_expr(first)?;
            for expr in iter {
                let next_var = self.infer_expr(expr)?;
                self.unify(first_var, next_var)?;
            }
            Ok(first_var)
        } else {
            Ok(self.fresh_type_var())
        }
    }

    fn infer_vec_array_repeat(&mut self, repeat: &mut ExprArrayRepeat) -> Result<TypeVarId> {
        let elem_var = self.infer_expr(repeat.elem.as_mut())?;
        let len_var = self.infer_expr(repeat.len.as_mut())?;
        let expected_len = self.fresh_type_var();
        self.bind(
            expected_len,
            TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
        );
        self.unify(len_var, expected_len)?;
        Ok(elem_var)
    }

    fn infer_list_value_as_vec(&mut self, list: &ValueList) -> Result<TypeVarId> {
        let elem_var = if let Some(first) = list.values.first() {
            let first_var = self.infer_value(first)?;
            for value in list.values.iter().skip(1) {
                let next_var = self.infer_value(value)?;
                self.unify(first_var, next_var)?;
            }
            first_var
        } else {
            let fresh = self.fresh_type_var();
            self.bind(fresh, TypeTerm::Any);
            fresh
        };

        let vec_var = self.fresh_type_var();
        self.bind(vec_var, TypeTerm::Vec(elem_var));
        Ok(vec_var)
    }

    fn infer_hashmap_new(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if !invoke.args.is_empty() {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::new does not take arguments");
        }

        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn infer_hashmap_with_capacity(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::with_capacity expects a single capacity argument");
        } else {
            let capacity_var = self.infer_expr(&mut invoke.args[0])?;
            let expected = self.fresh_type_var();
            self.bind(
                expected,
                TypeTerm::Primitive(TypePrimitive::Int(TypeInt::U64)),
            );
            self.unify(capacity_var, expected)?;
        }

        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn infer_hashmap_from(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.len() != 1 {
            for arg in &mut invoke.args {
                let _ = self.infer_expr(arg);
            }
            self.emit_error("HashMap::from expects a single iterable argument");
            let map_var = self.fresh_type_var();
            let map_ty = self.make_hashmap_ty();
            self.bind(map_var, TypeTerm::Custom(map_ty));
            return Ok(map_var);
        }

        let _ = self.infer_expr(&mut invoke.args[0])?;
        let map_var = self.fresh_type_var();
        let map_ty = self.make_hashmap_ty();
        self.bind(map_var, TypeTerm::Custom(map_ty));
        Ok(map_var)
    }

    fn make_hashmap_ty(&self) -> Ty {
        Ty::Struct(TypeStruct {
            name: Ident::new("HashMap"),
            fields: Vec::new(),
        })
    }

    fn locator_matches_suffix(locator: &Locator, suffix: &[&str]) -> bool {
        let segments = Self::locator_segments(locator);
        if segments.len() < suffix.len() {
            return false;
        }

        segments
            .iter()
            .rev()
            .zip(suffix.iter().rev())
            .all(|(segment, expected)| segment == expected)
    }

    fn locator_segments(locator: &Locator) -> Vec<String> {
        match locator {
            Locator::Ident(ident) => vec![ident.as_str().to_string()],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|segment| segment.as_str().to_string())
                .collect(),
            Locator::ParameterPath(path) => path
                .segments
                .iter()
                .map(|segment| segment.ident.as_str().to_string())
                .collect(),
        }
    }

    fn is_collection_with_len(ty: &Ty) -> bool {
        match ty {
            Ty::Array(_) | Ty::Slice(_) | Ty::Vec(_) => true,
            Ty::Struct(struct_ty) => struct_ty.name.as_str() == "HashMap",
            _ => false,
        }
    }

    fn infer_builtin_printf(&mut self, invoke: &mut ExprInvoke) -> Result<TypeVarId> {
        if invoke.args.is_empty() {
            self.emit_error("printf requires a format string argument");
            return Ok(self.error_type_var());
        }

        let format_var = self.infer_expr(&mut invoke.args[0])?;
        let expected_format = self.fresh_type_var();
        self.bind(expected_format, TypeTerm::Primitive(TypePrimitive::String));
        self.unify(format_var, expected_format)?;

        for arg in invoke.args.iter_mut().skip(1) {
            let _ = self.infer_expr(arg)?;
        }

        let result_var = self.fresh_type_var();
        self.bind(result_var, TypeTerm::Unit);
        Ok(result_var)
    }

    fn infer_value(&mut self, value: &Value) -> Result<TypeVarId> {
        let var = self.fresh_type_var();
        match value {
            Value::Int(_) => {
                self.literal_ints.insert(var);
                self.bind(var, TypeTerm::Primitive(TypePrimitive::Int(TypeInt::I64)));
            }
            Value::Bool(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Bool)),
            Value::Decimal(_) => self.bind(
                var,
                TypeTerm::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
            ),
            Value::String(_) => {
                let inner = self.fresh_type_var();
                self.bind(inner, TypeTerm::Primitive(TypePrimitive::String));
                self.bind(var, TypeTerm::Reference(inner));
            }
            Value::List(list) => {
                let elem_var = if let Some(first) = list.values.first() {
                    self.infer_value(first)?
                } else {
                    let fresh = self.fresh_type_var();
                    self.bind(fresh, TypeTerm::Any);
                    fresh
                };
                for value in list.values.iter().skip(1) {
                    let next_var = self.infer_value(value)?;
                    self.unify(elem_var, next_var)?;
                }
                let len = list.values.len() as i64;
                let elem_ty = self.resolve_to_ty(elem_var)?;
                let array_ty = Ty::Array(TypeArray {
                    elem: Box::new(elem_ty),
                    len: Expr::value(Value::int(len)).into(),
                });
                self.bind(var, TypeTerm::Custom(array_ty));
            }
            Value::Char(_) => self.bind(var, TypeTerm::Primitive(TypePrimitive::Char)),
            Value::Unit(_) => self.bind(var, TypeTerm::Unit),
            Value::Null(_) | Value::None(_) => self.bind(var, TypeTerm::Nothing),
            Value::Struct(struct_val) => {
                self.bind(var, TypeTerm::Struct(struct_val.ty.clone()));
            }
            Value::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| StructuralField::new(field.name.clone(), Ty::Any(TypeAny)))
                    .collect();
                self.bind(var, TypeTerm::Structural(TypeStructural { fields }));
            }
            Value::Tuple(tuple) => {
                let mut vars = Vec::new();
                for elem in &tuple.values {
                    vars.push(self.infer_value(elem)?);
                }
                self.bind(var, TypeTerm::Tuple(vars));
            }
            Value::Function(func) => {
                let fn_ty = self.ty_from_function_signature(&func.sig)?;
                let fn_var = self.type_from_ast_ty(&fn_ty)?;
                self.unify(var, fn_var)?;
            }
            Value::Type(ty) => {
                let ty_var = self.type_from_ast_ty(ty)?;
                self.unify(var, ty_var)?;
            }
            Value::Expr(_) => {
                let message = "embedded expression values are not yet supported".to_string();
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
            _ => {
                let message = format!("value {:?} is not supported by type inference", value);
                self.emit_error(message.clone());
                return Ok(self.error_type_var());
            }
        }
        Ok(var)
    }

    fn infer_pattern(&mut self, pattern: &mut Pattern) -> Result<PatternInfo> {
        let existing_ty = pattern.ty().cloned();

        let info = match pattern.kind_mut() {
            PatternKind::Ident(ident) => {
                let var = self.fresh_type_var();
                self.insert_env(ident.ident.as_str().to_string(), EnvEntry::Mono(var));
                PatternInfo::new(var).with_binding(ident.ident.as_str().to_string(), var)
            }
            PatternKind::Type(inner) => {
                let inner_info = self.infer_pattern(inner.pat.as_mut())?;
                let annot_var = self.type_from_ast_ty(&inner.ty)?;
                self.unify(inner_info.var, annot_var)?;
                inner_info
            }
            PatternKind::Wildcard(_) => PatternInfo::new(self.fresh_type_var()),
            PatternKind::Tuple(tuple) => {
                let mut vars = Vec::new();
                let mut bindings = Vec::new();
                for pat in &mut tuple.patterns {
                    let child = self.infer_pattern(pat)?;
                    vars.push(child.var);
                    bindings.extend(child.bindings);
                }
                let tuple_var = self.fresh_type_var();
                self.bind(tuple_var, TypeTerm::Tuple(vars));
                PatternInfo {
                    var: tuple_var,
                    bindings,
                }
            }
            PatternKind::Struct(struct_pat) => {
                let struct_name = struct_pat.name.as_str().to_string();
                let struct_var = self.fresh_type_var();
                if let Some(struct_def) = self.struct_defs.get(&struct_name).cloned() {
                    self.bind(struct_var, TypeTerm::Struct(struct_def.clone()));
                    let mut bindings = Vec::new();
                    for field in &mut struct_pat.fields {
                        if let Some(rename) = field.rename.as_mut() {
                            let child = self.infer_pattern(rename)?;
                            bindings.extend(child.bindings);
                            if let Some(def_field) =
                                struct_def.fields.iter().find(|f| f.name == field.name)
                            {
                                let expected = self.type_from_ast_ty(&def_field.value)?;
                                self.unify(child.var, expected)?;
                            }
                        } else if let Some(def_field) =
                            struct_def.fields.iter().find(|f| f.name == field.name)
                        {
                            let var = self.fresh_type_var();
                            self.insert_env(field.name.as_str().to_string(), EnvEntry::Mono(var));
                            let expected = self.type_from_ast_ty(&def_field.value)?;
                            self.unify(var, expected)?;
                            bindings.push(PatternBinding {
                                name: field.name.as_str().to_string(),
                                var,
                            });
                        }
                    }
                    PatternInfo {
                        var: struct_var,
                        bindings,
                    }
                } else {
                    let message = format!("unknown struct pattern: {}", struct_name);
                    self.emit_error(message.clone());
                    PatternInfo::new(self.error_type_var())
                }
            }
            _ => {
                self.emit_error("pattern is not supported by type inference");
                PatternInfo::new(self.error_type_var())
            }
        };

        if let Some(existing_ty) = existing_ty {
            if !matches!(existing_ty, Ty::Unknown(_)) {
                let annot_var = self.type_from_ast_ty(&existing_ty)?;
                self.unify(info.var, annot_var)?;
            }
        }

        let ty = self.resolve_to_ty(info.var)?;
        pattern.set_ty(ty);
        Ok(info)
    }

    fn apply_pattern_generalization(&mut self, info: &PatternInfo) -> Result<()> {
        for binding in &info.bindings {
            let scheme = self.generalize(binding.var)?;
            self.replace_env_entry(&binding.name, EnvEntry::Poly(scheme));
        }
        Ok(())
    }

    // generalize moved to typing/unify.rs

    // build_scheme_type moved to typing/unify.rs

    // scheme_from_term moved to typing/unify.rs

    // instantiate_scheme moved to typing/unify.rs

    // instantiate_scheme_type moved to typing/unify.rs

    fn register_generic_param(&mut self, name: &str) -> TypeVarId {
        let var = self.fresh_type_var();
        self.insert_env(name.to_string(), EnvEntry::Mono(var));
        if let Some(scope) = self.generic_scopes.last_mut() {
            scope.insert(name.to_string());
        }
        var
    }

    fn generic_name_in_scope(&self, name: &str) -> bool {
        self.generic_scopes
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    fn insert_env(&mut self, name: String, entry: EnvEntry) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, entry);
        }
    }

    fn replace_env_entry(&mut self, name: &str, entry: EnvEntry) {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), entry);
                return;
            }
        }
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name.to_string(), entry);
        }
    }

    fn enter_scope(&mut self) {
        self.current_level += 1;
        self.env.push(HashMap::new());
        self.generic_scopes.push(HashSet::new());
    }

    fn exit_scope(&mut self) {
        self.env.pop();
        self.generic_scopes.pop();
        if self.current_level > 0 {
            self.current_level -= 1;
        }
    }

    // fresh_type_var moved to typing/unify.rs

    // unit_type_var moved to typing/unify.rs

    // nothing_type_var moved to typing/unify.rs

    // bind moved to typing/unify.rs

    // find moved to typing/unify.rs

    // unify moved to typing/unify.rs

    // occurs_in_term moved to typing/unify.rs

    // occurs_in moved to typing/unify.rs

    // unify_terms moved to typing/unify.rs

    // resolve_to_ty moved to typing/unify.rs

    // term_to_ty moved to typing/unify.rs

    // type_from_ast_ty moved to typing/unify.rs

    fn lookup_associated_function(&mut self, locator: &Locator) -> Result<Option<TypeVarId>> {
        if let Locator::Path(path) = locator {
            if path.segments.len() >= 2 {
                if let (Some(struct_segment), Some(method_segment)) = (
                    path.segments.get(path.segments.len() - 2),
                    path.segments.last(),
                ) {
                    let struct_name = struct_segment.as_str();
                    let method_name = method_segment.as_str();
                    if let Some(methods) = self.struct_methods.get(struct_name) {
                        if let Some(record) = methods.get(method_name) {
                            if let Some(scheme) = record.scheme.as_ref() {
                                return Ok(Some(self.instantiate_scheme(&scheme.clone())));
                            }
                            if let Some(var) = self.lookup_env_var(method_name) {
                                return Ok(Some(var));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn lookup_locator(&mut self, locator: &Locator) -> Result<TypeVarId> {
        let key = locator.to_string();
        if let Some(var) = self.lookup_env_var(&key) {
            return Ok(var);
        }
        if let Some(ident) = locator.as_ident() {
            if let Some(var) = self.lookup_env_var(ident.as_str()) {
                return Ok(var);
            }
        }
        if let Locator::Path(path) = locator {
            if let Some(first) = path.segments.first() {
                if let Some(var) = self.lookup_env_var(first.as_str()) {
                    return Ok(var);
                }
            }
        }
        self.emit_error(format!("unresolved symbol: {}", key));
        Ok(self.error_type_var())
    }

    fn lookup_env_var(&mut self, name: &str) -> Option<TypeVarId> {
        for scope in self.env.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(match entry {
                    EnvEntry::Mono(var) => *var,
                    EnvEntry::Poly(scheme) => {
                        let scheme_clone = scheme.clone();
                        self.instantiate_scheme(&scheme_clone)
                    }
                });
            }
        }
        None
    }

    fn symbol_var(&mut self, name: &Ident) -> TypeVarId {
        let key = name.as_str().to_string();
        if let Some(var) = self.lookup_env_var(&key) {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    fn register_symbol(&mut self, name: &Ident) {
        let key = name.as_str().to_string();
        if self.lookup_env_var(&key).is_none() {
            let var = self.fresh_type_var();
            self.insert_env(key, EnvEntry::Mono(var));
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        let message = message.into();
        if self.lossy_mode {
            self.diagnostics.push(TypingDiagnostic::warning(message));
        } else {
            self.has_errors = true;
            self.diagnostics.push(TypingDiagnostic::error(message));
        }
    }

    #[allow(dead_code)]
    fn emit_warning(&mut self, message: impl Into<String>) {
        self.diagnostics.push(TypingDiagnostic::warning(message));
    }

    fn error_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, TypeTerm::Unknown);
        var
    }

    fn primitive_from_name(name: &str) -> Option<TypePrimitive> {
        match name {
            "i8" => Some(TypePrimitive::Int(TypeInt::I8)),
            "u8" => Some(TypePrimitive::Int(TypeInt::U8)),
            "i16" => Some(TypePrimitive::Int(TypeInt::I16)),
            "u16" => Some(TypePrimitive::Int(TypeInt::U16)),
            "i32" => Some(TypePrimitive::Int(TypeInt::I32)),
            "u32" => Some(TypePrimitive::Int(TypeInt::U32)),
            "i64" => Some(TypePrimitive::Int(TypeInt::I64)),
            "u64" => Some(TypePrimitive::Int(TypeInt::U64)),
            "isize" => Some(TypePrimitive::Int(TypeInt::I64)),
            "usize" => Some(TypePrimitive::Int(TypeInt::U64)),
            "f32" => Some(TypePrimitive::Decimal(DecimalType::F32)),
            "f64" => Some(TypePrimitive::Decimal(DecimalType::F64)),
            "bool" => Some(TypePrimitive::Bool),
            "char" => Some(TypePrimitive::Char),
            "str" | "String" => Some(TypePrimitive::String),
            "list" | "List" => Some(TypePrimitive::List),
            _ => None,
        }
    }

    fn expect_reference(&mut self, var: TypeVarId, context: &str) -> Result<TypeVarId> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                let inner = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(inner));
                Ok(inner)
            }
            TypeVarKind::Bound(TypeTerm::Reference(inner)) => Ok(inner),
            TypeVarKind::Link(next) => self.expect_reference(next, context),
            _other => {
                self.emit_error(format!("expected reference value for {}", context));
                let placeholder = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(placeholder));
                Ok(placeholder)
            }
        }
    }

    fn generalize_symbol(&mut self, name: &str, var: TypeVarId) -> Result<()> {
        let scheme = self.generalize(var)?;
        self.replace_env_entry(name, EnvEntry::Poly(scheme));
        Ok(())
    }

    // ensure_numeric moved to typing::solver

    // ensure_bool moved to typing::solver

    // ensure_integer moved to typing::solver

    // ensure_function moved to typing::solver

    fn lookup_struct_method(&mut self, obj_var: TypeVarId, field: &Ident) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty.clone());
        let struct_name = match resolved_ty {
            Ty::Struct(struct_ty) => struct_ty.name.as_str().to_string(),
            other => {
                self.emit_error(format!(
                    "cannot call method {} on value of type {}",
                    field, other
                ));
                return Ok(self.error_type_var());
            }
        };

        let record = self
            .struct_methods
            .get(&struct_name)
            .and_then(|methods| methods.get(field.as_str()))
            .cloned();

        if let Some(record) = record {
            if let Some(expected) = record.receiver_ty.as_ref() {
                let receiver_var = self.type_from_ast_ty(expected)?;
                let expect_ref = matches!(expected, Ty::Reference(_));
                let actual_ref = matches!(ty, Ty::Reference(_));
                if !expect_ref || actual_ref {
                    self.unify(obj_var, receiver_var)?;
                }
            }

            if let Some(scheme) = record.scheme.as_ref() {
                return Ok(self.instantiate_scheme(scheme));
            }
            if let Some(var) = self.lookup_env_var(field.as_str()) {
                return Ok(var);
            }
        }

        self.emit_error(format!(
            "unknown method {} on struct {}",
            field, struct_name
        ));
        Ok(self.error_type_var())
    }

    fn try_infer_primitive_method(
        &mut self,
        obj_var: TypeVarId,
        field: &Ident,
        arg_len: usize,
    ) -> Result<Option<TypeVarId>> {
        if field.name.as_str() != "to_string" || arg_len != 0 {
            return Ok(None);
        }

        let obj_ty = match self.resolve_to_ty(obj_var) {
            Ok(ty) => Self::peel_reference(ty),
            Err(_) => return Ok(None),
        };

        let result_var = self.fresh_type_var();
        self.bind(result_var, TypeTerm::Primitive(TypePrimitive::String));

        match obj_ty {
            Ty::Primitive(TypePrimitive::String)
            | Ty::Primitive(TypePrimitive::Bool)
            | Ty::Primitive(TypePrimitive::Char)
            | Ty::Primitive(TypePrimitive::Int(_))
            | Ty::Primitive(TypePrimitive::Decimal(_)) => Ok(Some(result_var)),
            _ => Ok(None),
        }
    }

    fn lookup_struct_field(&mut self, obj_var: TypeVarId, field: &Ident) -> Result<TypeVarId> {
        let ty = self.resolve_to_ty(obj_var)?;
        let resolved_ty = Self::peel_reference(ty);
        match resolved_ty {
            Ty::Struct(struct_ty) => {
                if let Some(def_field) = struct_ty.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!(
                        "unknown field {} on struct {}",
                        field, struct_ty.name
                    ));
                    Ok(self.error_type_var())
                }
            }
            Ty::Structural(structural) => {
                if let Some(def_field) = structural.fields.iter().find(|f| f.name == *field) {
                    let var = self.type_from_ast_ty(&def_field.value)?;
                    Ok(var)
                } else {
                    self.emit_error(format!("unknown field {}", field));
                    Ok(self.error_type_var())
                }
            }
            other => {
                self.emit_error(format!(
                    "cannot access field {} on value of type {}",
                    field, other
                ));
                Ok(self.error_type_var())
            }
        }
    }

    fn resolve_struct_literal(&mut self, struct_expr: &mut ExprStruct) -> Result<TypeVarId> {
        let struct_name = match self.struct_name_from_expr(&struct_expr.name) {
            Some(name) => name,
            None => {
                self.emit_error("struct literal target could not be resolved");
                return Ok(self.error_type_var());
            }
        };
        if let Some(def) = self.struct_defs.get(&struct_name).cloned() {
            let var = self.fresh_type_var();
            self.bind(var, TypeTerm::Struct(def.clone()));
            for field in &mut struct_expr.fields {
                if let Some(value) = field.value.as_mut() {
                    let value_var = self.infer_expr(value)?;
                    if let Some(struct_field) = def.fields.iter().find(|f| f.name == field.name) {
                        let ty_var = self.type_from_ast_ty(&struct_field.value)?;
                        self.unify(value_var, ty_var)?;
                    } else {
                        self.emit_error(format!(
                            "unknown field {} on struct {}",
                            field.name, def.name
                        ));
                        return Ok(self.error_type_var());
                    }
                }
            }
            Ok(var)
        } else {
            self.emit_error(format!("unknown struct literal target: {}", struct_name));
            Ok(self.error_type_var())
        }
    }

    fn ty_from_function_signature(&mut self, sig: &FunctionSignature) -> Result<Ty> {
        let mut params = Vec::new();
        for param in &sig.params {
            params.push(param.ty.clone());
        }
        let ret_ty = sig.ret_ty.clone().unwrap_or_else(|| Ty::Unit(TypeUnit));
        Ok(Ty::Function(TypeFunction {
            params,
            generics_params: sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        }))
    }

    fn struct_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Locator(locator) => {
                let name = locator.to_string();
                if name == "Self" {
                    self.impl_stack
                        .last()
                        .and_then(|ctx| ctx.as_ref())
                        .map(|ctx| ctx.struct_name.clone())
                } else {
                    Some(name)
                }
            }
            ExprKind::Value(value) => match &**value {
                Value::Type(Ty::Struct(struct_ty)) => Some(struct_ty.name.as_str().to_string()),
                _ => None,
            },
            _ => None,
        }
    }

}

/// Infer the fragment kind for an unkinded quote based on its block shape.
/// - Single trailing expression and no statements => Expr
/// - All items at top level => Item
/// - Otherwise => Stmt
// moved to typing::infer_expr::infer_quote_kind

impl<'ctx> AstTypeInferencer<'ctx> {
    pub fn infer_expression(&mut self, expr: &mut Expr) -> Result<()> {
        let var = self.infer_expr(expr)?;
        let ty = self.resolve_to_ty(var)?;
        expr.set_ty(ty);
        Ok(())
    }

    pub fn push_scope(&mut self) {
        self.env.push(HashMap::new());
        self.generic_scopes.push(HashSet::new());
        self.current_level += 1;
    }

    pub fn pop_scope(&mut self) {
        self.env.pop();
        self.generic_scopes.pop();
        if self.current_level > 0 {
            self.current_level -= 1;
        }
    }

    pub fn bind_variable(&mut self, name: &str, ty: Ty) {
        let type_var = match self.type_from_ast_ty(&ty) {
            Ok(var) => var,
            Err(_) => self.fresh_type_var(),
        };
        if let Some(current_env) = self.env.last_mut() {
            current_env.insert(name.to_string(), EnvEntry::Mono(type_var));
        }
    }
}

pub fn annotate(node: &mut Node) -> Result<TypingOutcome> {
    let mut inferencer = AstTypeInferencer::new();
    inferencer.infer(node)
}
