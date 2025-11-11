use std::collections::{HashMap, HashSet};
use std::mem;
use std::sync::Arc;

use crate::error::interpretation_error;
use crate::intrinsics::IntrinsicsRegistry;
use fp_core::ast::Pattern;
use fp_core::ast::TypeQuoteToken;
use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprClosure, ExprFormatString, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, FormatArgRef, FormatTemplatePart, FunctionParam, Item,
    ItemDefFunction, ItemKind, Node, NodeKind, StmtLet, Ty, TypeAny, TypeArray, TypeFunction,
    TypeInt, TypePrimitive, TypeReference, TypeSlice, TypeStruct, TypeTuple, TypeUnit, TypeVec,
    Value, ValueField, ValueFunction, ValueList, ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::ast::{Ident, Locator};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{format_runtime_string, format_value_with_spec, BinOpKind, UnOpKind};
use fp_typing::AstTypeInferencer;
mod interpreter_splicing;
mod blocks;
mod const_regions;
mod intrinsics;
mod eval_expr;
mod eval_stmt;
mod operators;
mod env;
mod closures;

const DEFAULT_DIAGNOSTIC_CONTEXT: &str = "ast-interpreter";

// === Quote fragment representation (interpreter-internal) ===
#[derive(Debug, Clone, PartialEq)]
pub enum QuotedFragment {
    Expr(Expr),
    Stmts(Vec<BlockStmt>),
    Items(Vec<Item>),
    Type(Ty),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterMode {
    CompileTime,
    RunTime,
}

#[derive(Debug, Clone)]
pub struct InterpreterOptions {
    pub mode: InterpreterMode,
    pub debug_assertions: bool,
    pub diagnostics: Option<Arc<DiagnosticManager>>,
    pub diagnostic_context: &'static str,
}

impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            mode: InterpreterMode::CompileTime,
            debug_assertions: false,
            diagnostics: None,
            diagnostic_context: DEFAULT_DIAGNOSTIC_CONTEXT,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct InterpreterOutcome {
    pub evaluated_constants: HashMap<String, Value>,
    pub diagnostics: Vec<Diagnostic>,
    pub stdout: Vec<String>,
    pub has_errors: bool,
    pub mutations_applied: bool,
    pub closure_types: HashMap<String, Ty>,
}

#[derive(Debug, Clone, PartialEq)]
struct ConstClosure {
    params: Vec<Pattern>,
    ret_ty: Option<Ty>,
    body: Expr,
    captured_values: Vec<HashMap<String, StoredValue>>,
    captured_types: Vec<HashMap<String, Ty>>,
    module_stack: Vec<String>,
    function_ty: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
enum StoredValue {
    Plain(Value),
    Closure(ConstClosure),
}

#[derive(Debug, Clone)]
struct GenericTemplate {
    function: ItemDefFunction,
    generics: Vec<String>,
}

pub struct AstInterpreter<'ctx> {
    ctx: &'ctx SharedScopedContext,
    diag_manager: Option<Arc<DiagnosticManager>>,
    intrinsics: IntrinsicsRegistry,
    mode: InterpreterMode,
    debug_assertions: bool,
    diagnostic_context: &'static str,

    module_stack: Vec<String>,
    value_env: Vec<HashMap<String, StoredValue>>,
    type_env: Vec<HashMap<String, Ty>>,
    global_types: HashMap<String, Ty>,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    evaluated_constants: HashMap<String, Value>,
    stdout: Vec<String>,
    functions: HashMap<String, ItemDefFunction>,
    generic_functions: HashMap<String, GenericTemplate>,
    specialization_cache: HashMap<String, HashMap<String, String>>,
    specialization_counter: HashMap<String, usize>,
    pending_items: Vec<Vec<Item>>,
    mutations_applied: bool,
    pending_closure: Option<ConstClosure>,
    pending_expr_ty: Option<Ty>,
    closure_types: HashMap<String, Ty>,
    typer: Option<AstTypeInferencer<'ctx>>,
    in_const_region: usize,
}

impl<'ctx> AstInterpreter<'ctx> {
    pub fn new(ctx: &'ctx SharedScopedContext, options: InterpreterOptions) -> Self {
        Self {
            ctx,
            diag_manager: options.diagnostics.clone(),
            intrinsics: IntrinsicsRegistry::new(),
            mode: options.mode,
            debug_assertions: options.debug_assertions,
            diagnostic_context: options.diagnostic_context,
            module_stack: Vec::new(),
            value_env: vec![HashMap::new()],
            type_env: vec![HashMap::new()],
            global_types: HashMap::new(),
            diagnostics: Vec::new(),
            has_errors: false,
            evaluated_constants: HashMap::new(),
            stdout: Vec::new(),
            functions: HashMap::new(),
            generic_functions: HashMap::new(),
            specialization_cache: HashMap::new(),
            specialization_counter: HashMap::new(),
            pending_items: Vec::new(),
            mutations_applied: false,
            pending_closure: None,
            pending_expr_ty: None,
            closure_types: HashMap::new(),
            typer: None,
            in_const_region: 0,
        }
    }

    pub fn set_typer(&mut self, typer: AstTypeInferencer<'ctx>) {
        self.typer = Some(typer);
    }

    pub fn interpret(&mut self, node: &mut Node) {
        match node.kind_mut() {
            NodeKind::File(file) => {
                self.pending_items.push(Vec::new());
                for item in &mut file.items {
                    self.evaluate_item(item);
                }
                if let Some(mut pending) = self.pending_items.pop() {
                    if !pending.is_empty() {
                        file.items.append(&mut pending);
                        self.mark_mutated();
                    }
                }
            }
            NodeKind::Item(item) => self.evaluate_item(item),
            NodeKind::Expr(expr) => {
                self.eval_expr(expr);
            }
            NodeKind::Query(_) => {
                self.emit_error("Query documents cannot be interpreted");
            }
            NodeKind::Schema(_) => {
                self.emit_error("Schema documents cannot be interpreted");
            }
            NodeKind::Workspace(_) => {
                self.emit_error("Workspace documents cannot be interpreted");
            }
        }
    }

    pub fn execute_main(&mut self) -> Option<Value> {
        let mut body_expr = self
            .functions
            .get("main")
            .map(|func| (*func.body).clone())?;
        self.push_scope();
        let value = self.eval_expr(&mut body_expr);
        self.pop_scope();
        Some(value)
    }

    pub fn take_outcome(&mut self) -> InterpreterOutcome {
        InterpreterOutcome {
            evaluated_constants: std::mem::take(&mut self.evaluated_constants),
            diagnostics: std::mem::take(&mut self.diagnostics),
            stdout: std::mem::take(&mut self.stdout),
            has_errors: self.has_errors,
            mutations_applied: std::mem::take(&mut self.mutations_applied),
            closure_types: std::mem::take(&mut self.closure_types),
        }
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    fn mark_mutated(&mut self) {
        self.mutations_applied = true;
    }

    pub fn evaluate_expression(&mut self, expr: &mut Expr) -> Value {
        self.push_scope();
        let value = self.eval_expr(expr);
        self.pop_scope();
        value
    }

    fn evaluate_item(&mut self, item: &mut Item) {
        match item.kind_mut() {
            ItemKind::Macro(_mac) => {
                // Item macros are compile-time constructs; interpreter skips them.
            }
            ItemKind::DefStruct(def) => {
                let ty = Ty::Struct(def.value.clone());
                self.insert_type(def.name.as_str(), ty);
            }
            ItemKind::DefStructural(def) => {
                let ty = Ty::Structural(def.value.clone());
                self.insert_type(def.name.as_str(), ty);
            }
            ItemKind::DefEnum(def) => {
                let ty = Ty::Enum(def.value.clone());
                self.insert_type(def.name.as_str(), ty);
            }
            ItemKind::DefType(def) => {
                self.insert_type(def.name.as_str(), def.value.clone());
            }
            ItemKind::DefConst(def) => {
                if let Some(inner_ty) = def.ty_annotation_mut().as_mut() {
                    self.evaluate_ty(inner_ty);
                }
                if let Some(inner_ty) = def.ty.as_mut() {
                    self.evaluate_ty(inner_ty);
                }
                let value = {
                    let expr_ref = def.value.as_mut();
                    self.eval_expr(expr_ref)
                };
                if self.pending_closure.is_some() {
                    let mut function_ty = {
                        let expr_ref = def.value.as_mut();
                        self.annotate_pending_closure(None, Some(expr_ref));
                        expr_ref.ty().cloned()
                    };
                    if function_ty.is_none() {
                        function_ty = def.ty.clone().or_else(|| def.ty_annotation().cloned());
                    }

                    if let Some(fn_ty) = function_ty {
                        def.ty = Some(fn_ty.clone());
                        def.ty_annotation = Some(fn_ty.clone());
                        def.value.set_ty(fn_ty.clone());
                    }
                }
                let qualified = self.qualified_name(def.name.as_str());
                self.insert_value(def.name.as_str(), value.clone());
                self.evaluated_constants.insert(qualified, value.clone());

                let should_replace =
                    !matches!(value, Value::Undefined(_)) && !matches!(value, Value::Unit(_));

                if should_replace {
                    let mut expr_value = Expr::value(value);
                    if let Some(ty) = def.ty.clone().or_else(|| def.ty_annotation().cloned()) {
                        expr_value.set_ty(ty);
                    }
                    *def.value = expr_value;
                    self.mark_mutated();
                }
            }
            ItemKind::DefStatic(def) => {
                let value = {
                    let expr_ref = def.value.as_mut();
                    self.eval_expr(expr_ref)
                };
                if self.pending_closure.is_some() {
                    let mut function_ty = {
                        let expr_ref = def.value.as_mut();
                        self.annotate_pending_closure(None, Some(expr_ref));
                        expr_ref.ty().cloned()
                    };
                    if function_ty.is_none() {
                        function_ty = def
                            .ty_annotation()
                            .cloned()
                            .or_else(|| Some(def.ty.clone()));
                    }

                    if let Some(fn_ty) = function_ty {
                        def.ty = fn_ty.clone();
                        def.ty_annotation = Some(fn_ty.clone());
                        def.value.set_ty(fn_ty.clone());
                    }
                }
                let mut expr_value = Expr::value(value.clone());
                expr_value.set_ty(def.ty.clone());
                self.insert_value(def.name.as_str(), value);
                *def.value = expr_value;
                self.mark_mutated();
            }
            ItemKind::Module(module) => {
                self.module_stack.push(module.name.as_str().to_string());
                self.push_scope();
                self.pending_items.push(Vec::new());
                for child in &mut module.items {
                    self.evaluate_item(child);
                }
                if let Some(mut pending) = self.pending_items.pop() {
                    if !pending.is_empty() {
                        module.items.append(&mut pending);
                        self.mark_mutated();
                    }
                }
                self.pop_scope();
                self.module_stack.pop();
            }
            ItemKind::Impl(impl_block) => {
                self.push_scope();
                for child in &mut impl_block.items {
                    self.evaluate_item(child);
                }
                self.pop_scope();
            }
            ItemKind::DefFunction(func) => {
                let base_name = func.name.as_str().to_string();
                let qualified_name = self.qualified_name(func.name.as_str());
                if func.sig.generics_params.is_empty() {
                    self.functions.insert(base_name, func.clone());
                    self.functions.insert(qualified_name, func.clone());
                    self.evaluate_function_body(func.body.as_mut());
                } else {
                    let generics = func
                        .sig
                        .generics_params
                        .iter()
                        .map(|param| param.name.as_str().to_string())
                        .collect();
                    let template = GenericTemplate {
                        function: func.clone(),
                        generics,
                    };
                    self.generic_functions
                        .insert(func.name.as_str().to_string(), template.clone());
                    self.generic_functions.insert(qualified_name, template);
                }
            }
            ItemKind::Expr(expr) => {
                self.eval_expr(expr);
            }
            ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::Import(_)
            | ItemKind::DefTrait(_)
            | ItemKind::Any(_) => {}
        }
    }


    fn cast_value_to_type(&mut self, value: Value, target_ty: &Ty) -> Value {
        match target_ty {
            Ty::Primitive(TypePrimitive::Int(_)) => match value {
                Value::Int(int_val) => Value::int(int_val.value),
                Value::Bool(bool_val) => Value::int(if bool_val.value { 1 } else { 0 }),
                Value::Decimal(decimal_val) => Value::int(decimal_val.value as i64),
                other => {
                    self.emit_error(format!(
                        "cannot cast value {} to integer type {}",
                        other, target_ty
                    ));
                    Value::undefined()
                }
            },
            Ty::Primitive(TypePrimitive::Bool) => match value {
                Value::Bool(bool_val) => Value::bool(bool_val.value),
                Value::Int(int_val) => Value::bool(int_val.value != 0),
                other => {
                    self.emit_error(format!(
                        "cannot cast value {} to bool during const evaluation",
                        other
                    ));
                    Value::undefined()
                }
            },
            _ => {
                self.emit_error(format!(
                    "unsupported cast target type {} in const evaluation",
                    target_ty
                ));
                Value::undefined()
            }
        }
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

    /// Helper to annotate a slice of arguments
    fn annotate_invoke_args_slice(&mut self, args: &mut [Expr], params: &[FunctionParam]) {
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            let should_annotate = match arg.ty() {
                None => true,
                Some(Ty::Unknown(_)) => true,
                Some(Ty::Function(fn_ty)) => {
                    // Replace function types with Unknown parameters
                    fn_ty.params.iter().any(|p| matches!(p, Ty::Unknown(_)))
                        || fn_ty
                            .ret_ty
                            .as_ref()
                            .map_or(false, |r| matches!(r.as_ref(), Ty::Unknown(_)))
                }
                _ => false,
            };

            if should_annotate {
                arg.set_ty(param.ty.clone());
            }
        }
    }

    fn apply_callable(&mut self, callee: Value, args: Vec<Value>) -> Value {
        if matches!(callee, Value::Unit(_)) && args.is_empty() {
            return Value::unit();
        }

        self.emit_error("unsupported callable in const evaluation");
        Value::undefined()
    }

    fn instantiate_generic_function(
        &mut self,
        lookup_name: &str,
        template: GenericTemplate,
        locator: &mut Locator,
        args: &[Expr],
    ) -> Option<ItemDefFunction> {
        let base_name = template.function.name.as_str().to_string();
        let substitution = match self.build_generic_substitution(&template, args) {
            Some(subst) => subst,
            None => {
                return None;
            }
        };
        let key = self.build_specialization_key(&template.generics, &substitution)?;

        if let Some(existing) = self.lookup_cached_specialization(&[lookup_name, &base_name], &key)
        {
            self.update_locator_name(locator, &existing);
            self.mark_mutated();
            return self.functions.get(&existing).cloned();
        }

        let sanitized_base = Self::sanitize_symbol(&base_name);
        let counter_entry = self
            .specialization_counter
            .entry(base_name.clone())
            .or_insert(0);
        let mut new_name;
        loop {
            new_name = format!("{}__spec{}", sanitized_base, *counter_entry);
            *counter_entry += 1;
            if !self.functions.contains_key(&new_name) {
                break;
            }
        }

        let mut specialized = template.function.clone();
        specialized.name = Ident::new(new_name.clone());
        specialized.sig.name = Some(specialized.name.clone());
        specialized.sig.generics_params.clear();

        for param in &mut specialized.sig.params {
            let substituted = self.substitute_ty(&param.ty, &substitution);
            param.ty = substituted.clone();
            param.ty_annotation = Some(substituted);
        }

        if let Some(ret_ty) = specialized.sig.ret_ty.as_mut() {
            let substituted = self.substitute_ty(ret_ty, &substitution);
            *ret_ty = substituted;
        }

        let function_type = TypeFunction {
            params: specialized
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: specialized
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| Box::new(ty.clone())),
        };
        let function_ty = Ty::Function(function_type.clone());
        specialized.ty = Some(function_type.clone());
        specialized.ty_annotation = Some(function_ty.clone());

        let mut local_types: HashMap<String, Ty> = HashMap::new();
        for param in &specialized.sig.params {
            let ty = param.ty.clone();
            local_types.insert(param.name.as_str().to_string(), ty.clone());
            local_types.insert(format!("#{}", param.name.as_str()), ty);
        }

        self.rewrite_expr_types(specialized.body.as_mut(), &substitution, &local_types);

        let mut new_item = Item::new(ItemKind::DefFunction(specialized.clone()));
        new_item.set_ty(function_ty);
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.push(new_item);
        } else {
            self.pending_items.push(vec![new_item]);
        }

        self.functions.insert(new_name.clone(), specialized.clone());
        let qualified_new = self.qualified_name(&new_name);
        self.functions.insert(qualified_new, specialized.clone());

        self.register_specialization(&base_name, lookup_name, key, &new_name);
        self.update_locator_name(locator, &new_name);
        self.mark_mutated();

        Some(specialized)
    }

    /// Specialize a generic function reference based on expected type
    /// Used when a generic function is referenced (not called) and we have type information
    fn specialize_function_reference(
        &mut self,
        locator: &mut Locator,
        expected_ty: &Ty,
    ) -> Option<ItemDefFunction> {
        // Extract function name
        let lookup_name = locator.to_string();
        let candidate_names = if let Some(ident) = locator.as_ident() {
            vec![lookup_name.clone(), ident.as_str().to_string()]
        } else {
            vec![lookup_name.clone()]
        };

        // Check if this is a generic function
        let (name, template) = candidate_names.iter().find_map(|name| {
            self.generic_functions
                .get(name)
                .cloned()
                .map(|t| (name.clone(), t))
        })?;

        // Extract parameter types from expected function type
        let expected_fn_ty = match expected_ty {
            Ty::Function(fn_ty) => fn_ty,
            _ => return None,
        };

        // Build substitution from expected function type
        let substitution = self.build_generic_substitution_from_type(&template, expected_fn_ty)?;

        let base_name = template.function.name.as_str().to_string();
        let key = self.build_specialization_key(&template.generics, &substitution)?;

        // Check if already specialized
        if let Some(existing) = self.lookup_cached_specialization(&[&name, &base_name], &key) {
            self.update_locator_name(locator, &existing);
            self.mark_mutated();
            return self.functions.get(&existing).cloned();
        }

        // Create new specialization
        let sanitized_base = Self::sanitize_symbol(&base_name);
        let counter_entry = self
            .specialization_counter
            .entry(base_name.clone())
            .or_insert(0);
        let mut new_name;
        loop {
            new_name = format!("{}__spec{}", sanitized_base, *counter_entry);
            *counter_entry += 1;
            if !self.functions.contains_key(&new_name) {
                break;
            }
        }

        let mut specialized = template.function.clone();
        specialized.name = Ident::new(new_name.clone());
        specialized.sig.name = Some(specialized.name.clone());
        specialized.sig.generics_params.clear();

        // Substitute parameter types
        for param in &mut specialized.sig.params {
            let substituted = self.substitute_ty(&param.ty, &substitution);
            param.ty = substituted.clone();
            param.ty_annotation = Some(substituted);
        }

        // Substitute return type
        if let Some(ret_ty) = specialized.sig.ret_ty.as_mut() {
            let substituted = self.substitute_ty(ret_ty, &substitution);
            *ret_ty = substituted;
        }

        let function_type = TypeFunction {
            params: specialized
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: specialized
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| Box::new(ty.clone())),
        };
        let function_ty = Ty::Function(function_type.clone());
        specialized.ty = Some(function_type.clone());
        specialized.ty_annotation = Some(function_ty.clone());

        let mut local_types: HashMap<String, Ty> = HashMap::new();
        for param in &specialized.sig.params {
            let ty = param.ty.clone();
            local_types.insert(param.name.as_str().to_string(), ty.clone());
            local_types.insert(format!("#{}", param.name.as_str()), ty);
        }

        self.rewrite_expr_types(specialized.body.as_mut(), &substitution, &local_types);

        let mut new_item = Item::new(ItemKind::DefFunction(specialized.clone()));
        new_item.set_ty(function_ty);
        if let Some(scope_pending) = self.pending_items.last_mut() {
            scope_pending.push(new_item);
        } else {
            self.pending_items.push(vec![new_item]);
        }

        self.functions.insert(new_name.clone(), specialized.clone());
        let qualified_new = self.qualified_name(&new_name);
        self.functions.insert(qualified_new, specialized.clone());

        self.register_specialization(&base_name, &name, key, &new_name);
        self.update_locator_name(locator, &new_name);
        self.mark_mutated();

        Some(specialized)
    }

    /// Build generic substitution from expected function type
    fn build_generic_substitution_from_type(
        &self,
        template: &GenericTemplate,
        expected_fn_ty: &TypeFunction,
    ) -> Option<HashMap<String, Ty>> {
        let generics_set: HashSet<String> = template.generics.iter().cloned().collect();
        let mut subst = HashMap::new();

        // Match parameter types
        if template.function.sig.params.len() != expected_fn_ty.params.len() {
            return None;
        }

        for (param, expected_ty) in template
            .function
            .sig
            .params
            .iter()
            .zip(&expected_fn_ty.params)
        {
            if !self.match_type(&param.ty, expected_ty, &generics_set, &mut subst) {
                return None;
            }
        }

        // Match return type if specified
        if let (Some(template_ret), Some(expected_ret)) =
            (&template.function.sig.ret_ty, &expected_fn_ty.ret_ty)
        {
            if !self.match_type(template_ret, expected_ret, &generics_set, &mut subst) {
                return None;
            }
        }

        // Verify all generics are resolved
        if template
            .generics
            .iter()
            .all(|name| subst.contains_key(name))
        {
            Some(subst)
        } else {
            None
        }
    }

    fn build_generic_substitution(
        &self,
        template: &GenericTemplate,
        args: &[Expr],
    ) -> Option<HashMap<String, Ty>> {
        let generics_set: HashSet<String> = template.generics.iter().cloned().collect();
        let mut subst = HashMap::new();
        for (param, arg) in template.function.sig.params.iter().zip(args.iter()) {
            let arg_ty = arg.ty()?.clone();
            if !self.match_type(&param.ty, &arg_ty, &generics_set, &mut subst) {
                return None;
            }
        }

        if template
            .generics
            .iter()
            .all(|name| subst.contains_key(name))
        {
            Some(subst)
        } else {
            None
        }
    }

    fn build_specialization_key(
        &self,
        generics: &[String],
        subst: &HashMap<String, Ty>,
    ) -> Option<String> {
        let mut parts = Vec::with_capacity(generics.len());
        for name in generics {
            let ty = subst.get(name)?;
            parts.push(format!("{}={}", name, ty));
        }
        Some(parts.join(";"))
    }

    fn lookup_cached_specialization(&self, names: &[&str], key: &str) -> Option<String> {
        for name in names {
            if let Some(cache) = self.specialization_cache.get(*name) {
                if let Some(symbol) = cache.get(key) {
                    return Some(symbol.clone());
                }
            }
        }
        None
    }

    fn register_specialization(
        &mut self,
        base_name: &str,
        lookup_name: &str,
        key: String,
        symbol: &str,
    ) {
        self.specialization_cache
            .entry(base_name.to_string())
            .or_default()
            .insert(key.clone(), symbol.to_string());
        if lookup_name != base_name {
            self.specialization_cache
                .entry(lookup_name.to_string())
                .or_default()
                .insert(key, symbol.to_string());
        }
    }

    fn update_locator_name(&self, locator: &mut Locator, new_name: &str) {
        let mut path = locator.to_path();
        if let Some(last) = path.segments.last_mut() {
            *last = Ident::new(new_name.to_string());
            *locator = Locator::path(path);
        }
    }

    fn sanitize_locator(locator: &mut Locator) -> String {
        match locator {
            Locator::Ident(ident) => {
                let current = ident.as_str().to_string();
                if let Some(trimmed) = current.strip_prefix('#') {
                    let trimmed_owned = trimmed.to_string();
                    *ident = Ident::new(trimmed_owned.clone());
                    trimmed_owned
                } else {
                    current
                }
            }
            Locator::Path(path) => {
                if let Some(last) = path.segments.last_mut() {
                    let current = last.as_str().to_string();
                    if let Some(trimmed) = current.strip_prefix('#') {
                        let trimmed_owned = trimmed.to_string();
                        *last = Ident::new(trimmed_owned.clone());
                        trimmed_owned
                    } else {
                        current
                    }
                } else {
                    String::new()
                }
            }
            Locator::ParameterPath(path) => {
                if let Some(last) = path.segments.last_mut() {
                    let current = last.ident.as_str().to_string();
                    if let Some(trimmed) = current.strip_prefix('#') {
                        let trimmed_owned = trimmed.to_string();
                        last.ident = Ident::new(trimmed_owned.clone());
                        trimmed_owned
                    } else {
                        current
                    }
                } else {
                    String::new()
                }
            }
        }
    }

    fn locator_key(locator: &Locator) -> String {
        match locator {
            Locator::Ident(ident) => ident.as_str().trim_start_matches('#').to_string(),
            Locator::Path(path) => path
                .segments
                .last()
                .map(|ident| ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
            Locator::ParameterPath(path) => path
                .segments
                .last()
                .map(|segment| segment.ident.as_str().trim_start_matches('#').to_string())
                .unwrap_or_default(),
        }
    }

    // === Environment helpers (reintroduced after refactor) ===
    // closure helpers moved to closures.rs

    fn sanitize_symbol(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        for ch in name.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                result.push(ch);
            } else {
                result.push('_');
            }
        }
        if result.is_empty() {
            result.push_str("specialized_fn");
        }
        if result
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        {
            result.insert(0, '_');
        }
        result
    }

    // === Operators (reintroduced for eval_expr) ===

    fn match_type(
        &self,
        pattern: &Ty,
        actual: &Ty,
        generics: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        if let Some(name) = self.generic_name(pattern, generics) {
            if let Some(existing) = subst.get(&name) {
                return self.types_compatible(existing, actual);
            }
            if self.is_unknown(actual) {
                return true;
            }
            self.record_generic_binding(subst, &name, actual);
            return true;
        }

        match (pattern, actual) {
            (Ty::Primitive(p), Ty::Primitive(a)) => p == a,
            (Ty::Unit(_), Ty::Unit(_)) => true,
            (Ty::Nothing(_), Ty::Nothing(_)) => true,
            (Ty::Struct(p), Ty::Struct(a)) => self.match_struct_type(p, a),
            (Ty::Reference(p), Ty::Reference(a)) => self.match_type(&p.ty, &a.ty, generics, subst),
            (Ty::Vec(p), Ty::Vec(a)) => self.match_type(&p.ty, &a.ty, generics, subst),
            (Ty::Slice(p), Ty::Slice(a)) => self.match_type(&p.elem, &a.elem, generics, subst),
            (Ty::Tuple(p), Ty::Tuple(a)) => {
                if p.types.len() != a.types.len() {
                    return false;
                }
                p.types
                    .iter()
                    .zip(a.types.iter())
                    .all(|(pt, at)| self.match_type(pt, at, generics, subst))
            }
            (Ty::Array(p), Ty::Array(a)) => self.match_type(&p.elem, &a.elem, generics, subst),
            (Ty::Function(p), Ty::Function(a)) => {
                if p.params.len() != a.params.len() {
                    return false;
                }
                for (pp, ap) in p.params.iter().zip(a.params.iter()) {
                    if !self.match_type(pp, ap, generics, subst) {
                        return false;
                    }
                }
                match (&p.ret_ty, &a.ret_ty) {
                    (Some(ret_p), Some(ret_a)) => {
                        self.match_type(ret_p.as_ref(), ret_a.as_ref(), generics, subst)
                    }
                    (None, Some(ret_a)) => {
                        self.match_type(&Ty::Unit(TypeUnit), ret_a.as_ref(), generics, subst)
                    }
                    (Some(_), None) => true,
                    (None, None) => true,
                }
            }
            (Ty::Expr(_), Ty::Expr(_)) => pattern == actual,
            (_, other) => self.is_unknown(other) || pattern == other,
        }
    }

    fn match_struct_type(&self, pattern: &TypeStruct, actual: &TypeStruct) -> bool {
        pattern.name == actual.name
    }

    fn record_generic_binding(&self, subst: &mut HashMap<String, Ty>, name: &str, ty: &Ty) {
        subst.insert(name.to_string(), ty.clone());
        if !name.starts_with('#') {
            subst.insert(format!("#{}", name), ty.clone());
        }
    }

    fn substitute_ty(&self, ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Locator(locator) = expr.kind() {
                let name = Self::locator_key(locator);
                if let Some(mapped) = subst.get(&name) {
                    return mapped.clone();
                }
                if let Some(mapped) = subst.get(&format!("#{}", name)) {
                    return mapped.clone();
                }
            }
        }

        match ty {
            Ty::Primitive(_) | Ty::Unit(_) | Ty::Nothing(_) | Ty::Any(_) | Ty::Unknown(_) => {
                ty.clone()
            }
            Ty::Struct(strct) => Ty::Struct(self.substitute_struct(strct, subst)),
            Ty::Reference(reference) => Ty::Reference(TypeReference {
                ty: Box::new(self.substitute_ty(&reference.ty, subst)),
                mutability: reference.mutability,
                lifetime: reference.lifetime.clone(),
            }),
            Ty::Vec(vec_ty) => Ty::Vec(TypeVec {
                ty: Box::new(self.substitute_ty(&vec_ty.ty, subst)),
            }),
            Ty::Slice(slice) => Ty::Slice(TypeSlice {
                elem: Box::new(self.substitute_ty(&slice.elem, subst)),
            }),
            Ty::Tuple(tuple) => Ty::Tuple(TypeTuple {
                types: tuple
                    .types
                    .iter()
                    .map(|ty| self.substitute_ty(ty, subst))
                    .collect(),
            }),
            Ty::Array(array) => Ty::Array(TypeArray {
                elem: Box::new(self.substitute_ty(&array.elem, subst)),
                len: array.len.clone(),
            }),
            Ty::Function(function) => Ty::Function(TypeFunction {
                params: function
                    .params
                    .iter()
                    .map(|ty| self.substitute_ty(ty, subst))
                    .collect(),
                generics_params: Vec::new(),
                ret_ty: function
                    .ret_ty
                    .as_ref()
                    .map(|ret| Box::new(self.substitute_ty(ret.as_ref(), subst))),
            }),
            Ty::Expr(_) => ty.clone(),
            Ty::QuoteToken(qt) => Ty::QuoteToken(Box::new(TypeQuoteToken {
                kind: qt.kind,
                inner: qt
                    .inner
                    .as_ref()
                    .map(|inner| Box::new(self.substitute_ty(inner.as_ref(), subst))),
            })),
            Ty::Structural(structural) => Ty::Structural(structural.clone()),
            Ty::Enum(enm) => Ty::Enum(enm.clone()),
            Ty::ImplTraits(_) | Ty::TypeBounds(_) | Ty::Type(_) | Ty::Value(_) | Ty::AnyBox(_) => {
                ty.clone()
            }
        }
    }

    fn substitute_struct(&self, ty: &TypeStruct, _subst: &HashMap<String, Ty>) -> TypeStruct {
        ty.clone()
    }

    fn generic_name(&self, ty: &Ty, generics: &HashSet<String>) -> Option<String> {
        if let Ty::Expr(expr) = ty {
            if let ExprKind::Locator(locator) = expr.kind() {
                let name = locator.to_string();
                if generics.contains(&name) {
                    return Some(name);
                }
            }
        }
        None
    }

    fn types_compatible(&self, a: &Ty, b: &Ty) -> bool {
        a == b || self.is_unknown(b)
    }

    fn is_unknown(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Unknown(_) | Ty::Any(_))
    }

    fn rewrite_expr_types(
        &self,
        expr: &mut Expr,
        subst: &HashMap<String, Ty>,
        local_types: &HashMap<String, Ty>,
    ) {
        let mut rewritten_ty = expr.ty().cloned().map(|ty| self.substitute_ty(&ty, subst));

        let needs_inference = rewritten_ty.as_ref().map_or(true, |ty| self.is_unknown(ty));

        if needs_inference {
            match expr.kind_mut() {
                ExprKind::Locator(locator) => {
                    if let Some(local) = self.resolve_local_ty(locator, local_types) {
                        rewritten_ty = Some(local);
                    }
                }
                ExprKind::Value(value) => {
                    if matches!(**value, Value::String(_)) {
                        rewritten_ty = Some(Ty::Primitive(TypePrimitive::String));
                    }
                }
                _ => {}
            }
        }

        if let Some(ty) = rewritten_ty {
            expr.set_ty(ty);
        }

        let expr_type_unknown = expr.ty().map_or(true, |ty| self.is_unknown(ty));

        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.rewrite_expr_types(expr_stmt.expr.as_mut(), subst, local_types);
                    }
                }
            }
            ExprKind::If(expr_if) => {
                self.rewrite_expr_types(expr_if.cond.as_mut(), subst, local_types);
                self.rewrite_expr_types(expr_if.then.as_mut(), subst, local_types);
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_expr_types(elze, subst, local_types);
                }
            }
            ExprKind::Loop(expr_loop) => {
                self.rewrite_expr_types(expr_loop.body.as_mut(), subst, local_types);
            }
            ExprKind::While(expr_while) => {
                self.rewrite_expr_types(expr_while.cond.as_mut(), subst, local_types);
                self.rewrite_expr_types(expr_while.body.as_mut(), subst, local_types);
            }
            ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_expr_types(case.cond.as_mut(), subst, local_types);
                    self.rewrite_expr_types(case.body.as_mut(), subst, local_types);
                }
            }
            ExprKind::Let(expr_let) => {
                self.rewrite_expr_types(expr_let.expr.as_mut(), subst, local_types);
            }
            ExprKind::Assign(assign) => {
                self.rewrite_expr_types(assign.target.as_mut(), subst, local_types);
                self.rewrite_expr_types(assign.value.as_mut(), subst, local_types);
            }
            ExprKind::Invoke(invoke) => {
                match &mut invoke.target {
                    ExprInvokeTarget::Expr(inner) => {
                        self.rewrite_expr_types(inner.as_mut(), subst, local_types);
                    }
                    ExprInvokeTarget::Closure(func) => {
                        self.rewrite_expr_types(func.body.as_mut(), subst, local_types);
                    }
                    _ => {}
                }
                for arg in &mut invoke.args {
                    self.rewrite_expr_types(arg, subst, local_types);
                }

                let target_fn_ty = match &mut invoke.target {
                    ExprInvokeTarget::Function(locator) => self
                        .resolve_local_ty(locator, local_types)
                        .and_then(|ty| match ty {
                            Ty::Function(fn_ty) => Some(fn_ty),
                            _ => None,
                        }),
                    ExprInvokeTarget::Expr(inner) => {
                        let ty = inner.ty().cloned().or_else(|| {
                            if let ExprKind::Locator(locator) = inner.kind_mut() {
                                self.resolve_local_ty(locator, local_types)
                            } else {
                                None
                            }
                        });
                        ty.and_then(|ty| match ty {
                            Ty::Function(fn_ty) => Some(fn_ty),
                            _ => None,
                        })
                    }
                    ExprInvokeTarget::Closure(func) => Some(TypeFunction {
                        params: func
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect(),
                        generics_params: Vec::new(),
                        ret_ty: func.sig.ret_ty.as_ref().map(|ret| Box::new(ret.clone())),
                    }),
                    _ => None,
                };

                let mut inferred_ret_ty: Option<Ty> = None;
                let mut param_types: Option<Vec<Ty>> = None;

                if let Some(function_ty) = target_fn_ty {
                    if expr_type_unknown {
                        inferred_ret_ty = Self::type_function_ret_ty(&function_ty)
                            .or_else(|| Some(Ty::Unit(TypeUnit)));
                    }
                    param_types = Some(function_ty.params.clone());
                }

                if let Some(fn_params) = param_types {
                    for (idx, arg) in invoke.args.iter_mut().enumerate() {
                        if let Some(param_ty) = fn_params.get(idx) {
                            if arg.ty().map_or(true, |ty| self.is_unknown(ty)) {
                                let substituted = self.substitute_ty(param_ty, subst);
                                arg.set_ty(substituted);
                            }
                        }
                    }
                }

                if let Some(ret_ty) = inferred_ret_ty {
                    expr.set_ty(ret_ty);
                }
            }
            ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_expr_types(value, subst, local_types);
                    }
                }
            }
            ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_expr_types(value, subst, local_types);
                    }
                }
            }
            ExprKind::Select(select) => {
                self.rewrite_expr_types(select.obj.as_mut(), subst, local_types);
            }
            ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_expr_types(value, subst, local_types);
                }
            }
            ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_expr_types(array_repeat.elem.as_mut(), subst, local_types);
                self.rewrite_expr_types(array_repeat.len.as_mut(), subst, local_types);
            }
            ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_expr_types(value, subst, local_types);
                }
            }
            ExprKind::BinOp(binop) => {
                self.rewrite_expr_types(binop.lhs.as_mut(), subst, local_types);
                self.rewrite_expr_types(binop.rhs.as_mut(), subst, local_types);
            }
            ExprKind::UnOp(unop) => {
                self.rewrite_expr_types(unop.val.as_mut(), subst, local_types);
            }
            ExprKind::Reference(reference) => {
                self.rewrite_expr_types(reference.referee.as_mut(), subst, local_types);
            }
            ExprKind::Dereference(deref) => {
                self.rewrite_expr_types(deref.referee.as_mut(), subst, local_types);
            }
            ExprKind::Index(index) => {
                self.rewrite_expr_types(index.obj.as_mut(), subst, local_types);
                self.rewrite_expr_types(index.index.as_mut(), subst, local_types);
            }
            ExprKind::Splat(splat) => {
                self.rewrite_expr_types(splat.iter.as_mut(), subst, local_types);
            }
            ExprKind::SplatDict(splat) => {
                self.rewrite_expr_types(splat.dict.as_mut(), subst, local_types);
            }
            ExprKind::Try(expr_try) => {
                self.rewrite_expr_types(expr_try.expr.as_mut(), subst, local_types);
            }
            ExprKind::Closure(closure) => {
                self.rewrite_expr_types(closure.body.as_mut(), subst, local_types);
            }
            ExprKind::Closured(closured) => {
                self.rewrite_expr_types(closured.expr.as_mut(), subst, local_types);
            }
            ExprKind::Paren(paren) => {
                self.rewrite_expr_types(paren.expr.as_mut(), subst, local_types);
            }
            ExprKind::FormatString(format) => {
                for arg in &mut format.args {
                    self.rewrite_expr_types(arg, subst, local_types);
                }
                for kwarg in &mut format.kwargs {
                    self.rewrite_expr_types(&mut kwarg.value, subst, local_types);
                }
            }
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for arg in args {
                        self.rewrite_expr_types(arg, subst, local_types);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for arg in &mut template.args {
                        self.rewrite_expr_types(arg, subst, local_types);
                    }
                    for kwarg in &mut template.kwargs {
                        self.rewrite_expr_types(&mut kwarg.value, subst, local_types);
                    }
                }
            },
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(inner) => {
                    self.rewrite_expr_types(inner.as_mut(), subst, local_types);
                }
                Value::Function(func) => {
                    self.rewrite_expr_types(func.body.as_mut(), subst, local_types);
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn resolve_local_ty(
        &self,
        locator: &mut Locator,
        local_types: &HashMap<String, Ty>,
    ) -> Option<Ty> {
        let key = Self::sanitize_locator(locator);
        if let Some(ty) = local_types.get(&key) {
            return Some(ty.clone());
        }
        if let Some(ty) = local_types.get(&format!("#{}", key)) {
            return Some(ty.clone());
        }
        tracing::debug!(target: DEFAULT_DIAGNOSTIC_CONTEXT, "resolve_local_ty: missing key {}", key);
        None
    }

    fn capture_closure(&self, closure: &ExprClosure, ty: Option<Ty>) -> ConstClosure {
        let mut captured = ConstClosure {
            params: closure.params.clone(),
            ret_ty: closure.ret_ty.as_ref().map(|ty| (**ty).clone()),
            body: closure.body.as_ref().clone(),
            captured_values: self.value_env.clone(),
            captured_types: self.type_env.clone(),
            module_stack: self.module_stack.clone(),
            function_ty: ty,
        };

        if let Some(fn_sig) = captured.function_ty.as_ref().and_then(|ty| match ty {
            Ty::Function(fn_sig) => Some(fn_sig.clone()),
            _ => None,
        }) {
            Self::annotate_const_closure(&mut captured, &fn_sig);
        }

        captured
    }

    fn call_const_closure(&mut self, closure: &ConstClosure, args: Vec<Value>) -> Value {
        if closure.params.len() != args.len() {
            self.emit_error(format!(
                "closure expected {} arguments, found {}",
                closure.params.len(),
                args.len()
            ));
            return Value::undefined();
        }

        let saved_values = mem::replace(&mut self.value_env, closure.captured_values.clone());
        let saved_types = mem::replace(&mut self.type_env, closure.captured_types.clone());
        let saved_modules = mem::replace(&mut self.module_stack, closure.module_stack.clone());

        self.push_scope();
        for (pattern, value) in closure.params.iter().zip(args.into_iter()) {
            self.bind_pattern(pattern, value);
        }

        let mut body = closure.body.clone();
        let result = self.eval_expr(&mut body);

        self.pop_scope();

        self.value_env = saved_values;
        self.type_env = saved_types;
        self.module_stack = saved_modules;

        result
    }

    fn call_function(&mut self, function: ItemDefFunction, args: Vec<Value>) -> Value {
        if !function.sig.generics_params.is_empty() {
            self.emit_error(format!(
                "generic functions are not supported in const evaluation: {}",
                function.name.as_str()
            ));
            return Value::undefined();
        }

        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function '{}' expected {} arguments, found {}",
                function.name.as_str(),
                function.sig.params.len(),
                args.len()
            ));
            return Value::undefined();
        }

        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }

        let mut body = function.body.as_ref().clone();
        let result = self.eval_expr(&mut body);

        self.pop_scope();

        result
    }

    fn call_value_function(&mut self, function: &ValueFunction, args: Vec<Value>) -> Value {
        if function.sig.params.len() != args.len() {
            self.emit_error(format!(
                "function literal expected {} arguments, found {}",
                function.sig.params.len(),
                args.len()
            ));
            return Value::undefined();
        }

        self.push_scope();
        for (param, value) in function.sig.params.iter().zip(args.into_iter()) {
            if let Some(scope) = self.type_env.last_mut() {
                scope.insert(param.name.as_str().to_string(), param.ty.clone());
            }
            self.insert_value(param.name.as_str(), value);
        }

        let mut body = function.body.as_ref().clone();
        let result = self.eval_expr(&mut body);

        self.pop_scope();

        result
    }

    fn eval_block(&mut self, block: &mut ExprBlock) -> Value {
        self.push_scope();
        let mut last_value = Value::unit();
        for stmt in &mut block.stmts {
            if let Some(value) = self.eval_stmt(stmt) {
                last_value = value;
            }
        }
        self.pop_scope();
        last_value
    }

    fn evaluate_struct_literal(&mut self, struct_expr: &mut fp_core::ast::ExprStruct) -> Value {
        let ty_value = self.eval_expr(struct_expr.name.as_mut());
        let struct_ty = match ty_value {
            Value::Type(Ty::Struct(struct_ty)) => struct_ty,
            Value::Type(other_ty) => {
                self.emit_error(format!(
                    "expected struct type in literal, found {}",
                    other_ty
                ));
                return Value::undefined();
            }
            other => {
                self.emit_error(format!("expected struct type in literal, found {}", other));
                return Value::undefined();
            }
        };

        let mut fields = Vec::new();
        for field in &mut struct_expr.fields {
            let value = field
                .value
                .as_mut()
                .map(|expr| self.eval_expr(expr))
                .unwrap_or_else(|| {
                    self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                        self.emit_error(format!(
                            "missing initializer for field '{}' in struct literal",
                            field.name
                        ));
                        Value::undefined()
                    })
                });
            fields.push(ValueField::new(field.name.clone(), value));
        }

        Value::Struct(ValueStruct::new(struct_ty, fields))
    }

    fn eval_array_repeat(&mut self, repeat: &mut fp_core::ast::ExprArrayRepeat) -> Value {
        let elem_value = self.eval_expr(repeat.elem.as_mut());
        let len_value = self.eval_expr(repeat.len.as_mut());

        let len = match len_value {
            Value::Int(int) if int.value >= 0 => int.value as usize,
            Value::Int(_) => {
                self.emit_error("array repeat length must be non-negative");
                return Value::undefined();
            }
            Value::Decimal(decimal) if decimal.value >= 0.0 => decimal.value as usize,
            other => {
                self.emit_error(format!(
                    "array repeat length must be an integer constant, found {}",
                    other
                ));
                return Value::undefined();
            }
        };

        let mut values = Vec::with_capacity(len);
        for _ in 0..len {
            values.push(elem_value.clone());
        }
        Value::List(ValueList::new(values))
    }

    fn evaluate_ty(&mut self, ty: &mut Ty) {
        match ty {
            Ty::Array(array_ty) => {
                let len_value = self.eval_expr(array_ty.len.as_mut());
                match len_value {
                    Value::Int(int_value) => {
                        let replacement = Expr::value(Value::Int(int_value.clone()));
                        *array_ty.len = replacement.into();
                    }
                    Value::Decimal(decimal) => {
                        let as_int = decimal.value as i64;
                        let replacement = Expr::value(Value::int(as_int));
                        *array_ty.len = replacement.into();
                    }
                    other => {
                        self.emit_error(format!(
                            "array length type must be an integer literal, found {}",
                            other
                        ));
                    }
                }
            }
            _ => {}
        }
    }

    fn evaluate_select(&mut self, target: Value, field: &str) -> Value {
        match target {
            Value::Struct(value_struct) => value_struct
                .structural
                .fields
                .iter()
                .find(|f| f.name.as_str() == field)
                .map(|f| f.value.clone())
                .unwrap_or_else(|| {
                    self.emit_error(format!("field '{}' not found on struct", field));
                    Value::undefined()
                }),
            Value::Structural(structural) => structural
                .fields
                .iter()
                .find(|f| f.name.as_str() == field)
                .map(|f| f.value.clone())
                .unwrap_or_else(|| {
                    self.emit_error(format!("field '{}' not found", field));
                    Value::undefined()
                }),
            other => {
                self.emit_error(format!(
                    "cannot access field '{}' on value {} in const context",
                    field, other
                ));
                Value::undefined()
            }
        }
    }

    fn function_ret_ty_from_ty(ty: &Ty) -> Option<Ty> {
        match ty {
            Ty::Function(fn_ty) => fn_ty.ret_ty.as_ref().map(|ret| (*ret.clone())),
            _ => None,
        }
    }

    fn type_function_ret_ty(ty: &TypeFunction) -> Option<Ty> {
        ty.ret_ty.as_ref().map(|ret| (*ret.clone()))
    }

    fn lookup_type(&self, name: &str) -> Option<Ty> {
        for scope in self.type_env.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn resolve_qualified(&mut self, symbol: String) -> Value {
        if let Some(value) = self.evaluated_constants.get(&symbol) {
            return value.clone();
        }
        if let Some(ty) = self.global_types.get(&symbol) {
            return Value::Type(ty.clone());
        }
        if symbol == "printf" {
            return Value::unit();
        }
        self.emit_error(format!(
            "unresolved symbol '{}' in const evaluation",
            symbol
        ));
        Value::undefined()
    }

    fn qualified_name(&self, name: &str) -> String {
        if self.is_printf_symbol(name) {
            return name.trim_start_matches("#").to_string();
        }

        if self.module_stack.is_empty() {
            name.to_string()
        } else {
            let mut qualified = self.module_stack.join("::");
            qualified.push_str("::");
            qualified.push_str(name);
            qualified
        }
    }

    fn is_printf_symbol(&self, name: &str) -> bool {
        let base = name.trim_start_matches("#");
        base == "printf" || base.ends_with("::printf")
    }

    fn push_scope(&mut self) {
        self.value_env.push(HashMap::new());
        self.type_env.push(HashMap::new());
        if let Some(typer) = self.typer.as_mut() {
            typer.push_scope();
        }
    }

    fn pop_scope(&mut self) {
        self.value_env.pop();
        self.type_env.pop();
        if let Some(typer) = self.typer.as_mut() {
            typer.pop_scope();
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::error(message.into()).with_source_context(self.diagnostic_context);
        self.push_diagnostic(diagnostic);
    }

    fn emit_warning(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::warning(message.into()).with_source_context(self.diagnostic_context);
        self.push_diagnostic(diagnostic);
    }

    fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        if let Some(manager) = &self.diag_manager {
            manager.add_diagnostic(diagnostic.clone());
        }
        if diagnostic.level == DiagnosticLevel::Error {
            self.has_errors = true;
        }
        self.diagnostics.push(diagnostic);
    }

    fn handle_result(&mut self, result: Result<Value>) -> Value {
        match result {
            Ok(value) => value,
            Err(err) => {
                self.emit_error(err.to_string());
                Value::undefined()
            }
        }
    }

    // build_quoted_fragment moved to interpreter_splicing.rs
}

fn intrinsic_symbol(kind: IntrinsicCallKind) -> Option<&'static str> {
    match kind {
        IntrinsicCallKind::SizeOf => Some("sizeof!"),
        IntrinsicCallKind::ReflectFields => Some("reflect_fields!"),
        IntrinsicCallKind::HasMethod => Some("hasmethod!"),
        IntrinsicCallKind::TypeName => Some("type_name!"),
        IntrinsicCallKind::CreateStruct => Some("create_struct!"),
        IntrinsicCallKind::CloneStruct => Some("clone_struct!"),
        IntrinsicCallKind::AddField => Some("addfield!"),
        IntrinsicCallKind::HasField => Some("hasfield!"),
        IntrinsicCallKind::FieldCount => Some("field_count!"),
        IntrinsicCallKind::MethodCount => Some("method_count!"),
        IntrinsicCallKind::FieldType => Some("field_type!"),
        IntrinsicCallKind::StructSize => Some("struct_size!"),
        IntrinsicCallKind::GenerateMethod => Some("generate_method!"),
        IntrinsicCallKind::CompileError => Some("compile_error!"),
        IntrinsicCallKind::CompileWarning => Some("compile_warning!"),
        _ => None,
    }
}
