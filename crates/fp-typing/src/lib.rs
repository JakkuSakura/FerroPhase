use std::collections::{HashMap, HashSet};
use fp_core::config;

pub mod typing;
pub use typing::types::{TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome};

use fp_core::ast::*;
use crate::typing::scheme::TypeScheme;
use fp_core::ast::{Ident, Locator};
use fp_core::context::SharedScopedContext;
use fp_core::error::{Error, Result};
// intrinsic and op kinds handled in submodules
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

    // moved: infer_reference, infer_dereference, infer_index, infer_range, infer_splat, infer_splat_dict

    // moved: infer_intrinsic

    // moved: infer_closure

    // infer_match moved to typing::infer_stmt

    // moved: infer_invoke



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

    // unused: generic_name_in_scope (removed)

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

    // unused: primitive_from_name (removed)

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
