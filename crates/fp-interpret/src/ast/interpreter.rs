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

    fn eval_expr(&mut self, expr: &mut Expr) -> Value {
        let expr_ty_snapshot = expr.ty().cloned();

        // Check if we need to specialize a function reference before matching
        let should_specialize_fn_ref = if let ExprKind::Locator(_) = expr.kind() {
            expr_ty_snapshot
                .as_ref()
                .map_or(false, |ty| matches!(ty, Ty::Function(_)))
        } else {
            false
        };

        match expr.kind_mut() {
            ExprKind::IntrinsicContainer(collection) => {
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                return self.eval_expr(expr);
            }
            ExprKind::Quote(_quote) => {
                self.emit_error(
                    "quote cannot be evaluated directly; use it with splice inside const blocks",
                );
                return Value::undefined();
            }
            ExprKind::Splice(_splice) => {
                self.emit_error("splice cannot be evaluated to a value; it is only valid inside const regions to insert code");
                return Value::undefined();
            }
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(inner) => {
                    let mut cloned = inner.as_ref().clone();
                    self.eval_expr(&mut cloned)
                }
                other => other.clone(),
            },
            ExprKind::Locator(locator) => {
                // First, try to specialize generic function reference if we have type info
                if should_specialize_fn_ref {
                    if let Some(expected_ty) = &expr_ty_snapshot {
                        tracing::debug!(
                            "Attempting to specialize function reference {} with type {:?}",
                            locator,
                            expected_ty
                        );
                        if let Some(_specialized) =
                            self.specialize_function_reference(locator, expected_ty)
                        {
                            tracing::debug!("Successfully specialized {} to {}", locator, locator);
                            // Locator has been updated to point to specialized function
                            // Return unit for now, the reference will be used by caller
                            return Value::unit();
                        } else {
                            tracing::debug!("Failed to specialize {}", locator);
                        }
                    }
                }

                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        let mut replacement = Expr::value(value.clone());
                        replacement.ty = expr.ty.clone();
                        // Type is already copied from original expr, no need to re-infer
                        *expr = replacement;
                        value
                    } else if let Some(closure) = self.lookup_closure(ident.as_str()) {
                        self.pending_closure = Some(closure);
                        Value::unit()
                    } else if let Some(ty) = self.lookup_type(ident.as_str()) {
                        Value::Type(ty)
                    } else {
                        self.resolve_qualified(locator.to_string())
                    }
                } else {
                    self.resolve_qualified(locator.to_string())
                }
            }
            ExprKind::BinOp(binop) => {
                let lhs = self.eval_expr(binop.lhs.as_mut());
                let rhs = self.eval_expr(binop.rhs.as_mut());
                self.handle_result(self.evaluate_binop(binop.kind, lhs, rhs))
            }
            ExprKind::UnOp(unop) => {
                let value = self.eval_expr(unop.val.as_mut());
                self.handle_result(self.evaluate_unary(unop.op.clone(), value))
            }
            ExprKind::If(if_expr) => {
                let cond = self.eval_expr(if_expr.cond.as_mut());
                match cond {
                    Value::Bool(b) => {
                        if b.value {
                            self.eval_expr(if_expr.then.as_mut())
                        } else if let Some(else_) = if_expr.elze.as_mut() {
                            self.eval_expr(else_)
                        } else {
                            Value::unit()
                        }
                    }
                    _ => {
                        self.emit_error("expected boolean condition in const expression");
                        Value::undefined()
                    }
                }
            }
            ExprKind::Block(block) => self.eval_block(block),
            ExprKind::Tuple(tuple) => {
                let values = tuple
                    .values
                    .iter_mut()
                    .map(|expr| self.eval_expr(expr))
                    .collect();
                Value::Tuple(ValueTuple::new(values))
            }
            ExprKind::Array(array) => {
                let values = array
                    .values
                    .iter_mut()
                    .map(|expr| self.eval_expr(expr))
                    .collect();
                Value::List(ValueList::new(values))
            }
            ExprKind::ArrayRepeat(repeat) => self.eval_array_repeat(repeat),
            ExprKind::Struct(struct_expr) => self.evaluate_struct_literal(struct_expr),
            ExprKind::Structural(struct_expr) => {
                let fields = struct_expr
                    .fields
                    .iter_mut()
                    .map(|field| {
                        let value = field
                            .value
                            .as_mut()
                            .map(|expr| self.eval_expr(expr))
                            .unwrap_or_else(|| {
                                self.lookup_value(field.name.as_str()).unwrap_or_else(|| {
                                    self.emit_error(format!(
                                        "missing initializer for field '{}' in structural literal",
                                        field.name
                                    ));
                                    Value::undefined()
                                })
                            });
                        ValueField::new(field.name.clone(), value)
                    })
                    .collect();
                Value::Structural(ValueStructural::new(fields))
            }
            ExprKind::Select(select) => {
                let target = self.eval_expr(select.obj.as_mut());
                self.evaluate_select(target, &select.field.name)
            }
            ExprKind::IntrinsicCall(call) => {
                let kind = call.kind;
                let value = self.eval_intrinsic(call);
                if self.should_replace_intrinsic_with_value(kind, &value) {
                    let mut replacement = Expr::value(value.clone());
                    replacement.ty = expr.ty.clone();
                    // Type is already copied from original expr, no need to re-infer
                    *expr = replacement;
                    self.mark_mutated();
                    return value;
                }
                value
            }
            ExprKind::Reference(reference) => self.eval_expr(reference.referee.as_mut()),
            ExprKind::Paren(paren) => self.eval_expr(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.emit_error("assignment is not allowed during AST interpretation");
                self.eval_expr(assign.value.as_mut())
            }
            ExprKind::Let(expr_let) => {
                let value = self.eval_expr(expr_let.expr.as_mut());
                self.bind_pattern(&expr_let.pat, value.clone());
                value
            }
            ExprKind::Invoke(invoke) => {
                let result = self.eval_invoke(invoke);
                if let Some(ty) = self.pending_expr_ty.take() {
                    expr.set_ty(ty);
                }
                result
            }
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` should have been lowered before const evaluation",
                    macro_expr.invocation.path
                ));
                Value::undefined()
            }
            ExprKind::Closure(closure) => {
                if let Some(Ty::Function(ref fn_sig)) = expr_ty_snapshot.as_ref() {
                    Self::annotate_expr_closure(closure, fn_sig);
                }
                let captured = self.capture_closure(closure, expr_ty_snapshot.clone());
                self.pending_closure = Some(captured);
                Value::unit()
            }
            ExprKind::Closured(closured) => {
                let mut inner = closured.expr.as_ref().clone();
                self.eval_expr(&mut inner)
            }
            ExprKind::Cast(cast) => {
                let target_ty = cast.ty.clone();
                let value = self.eval_expr(cast.expr.as_mut());
                let result = self.cast_value_to_type(value, &target_ty);
                expr.set_ty(target_ty);
                result
            }
            ExprKind::Any(_any) => {
                self.emit_error(
                    "expression not supported in AST interpretation: unsupported Raw expression",
                );
                Value::undefined()
            }
            other => {
                self.emit_error(format!(
                    "expression not supported in AST interpretation: {:?}",
                    other
                ));
                Value::undefined()
            }
        }
    }

    fn eval_invoke(&mut self, invoke: &mut ExprInvoke) -> Value {
        match self.mode {
            InterpreterMode::CompileTime => self.eval_invoke_compile_time(invoke),
            InterpreterMode::RunTime => self.eval_invoke_runtime(invoke),
        }
    }

    fn eval_invoke_compile_time(&mut self, invoke: &mut ExprInvoke) -> Value {
        if let ExprInvokeTarget::Method(select) = &mut invoke.target {
            if select.field.name.as_str() == "len" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                return match value {
                    Value::List(list) => Value::int(list.values.len() as i64),
                    Value::Map(map) => Value::int(map.len() as i64),
                    other => {
                        self.emit_error(format!(
                            "'len' is only supported on compile-time arrays, lists, and maps, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            if select.field.name.as_str() == "to_string" && invoke.args.is_empty() {
                let value = self.eval_expr(select.obj.as_mut());
                self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::String));
                return match value {
                    Value::String(_) => value,
                    Value::Int(int_val) => Value::string(int_val.value.to_string()),
                    Value::Decimal(dec_val) => Value::string(dec_val.value.to_string()),
                    Value::Bool(bool_val) => Value::string(bool_val.value.to_string()),
                    Value::Char(char_val) => Value::string(char_val.value.to_string()),
                    other => {
                        self.emit_error(format!(
                            "'to_string' is only supported on primitive compile-time values, found {:?}",
                            other
                        ));
                        Value::undefined()
                    }
                };
            }

            self.emit_error("method calls are not supported in const evaluation yet");
            return Value::undefined();
        }

        match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                if let Some(value) =
                    self.try_handle_const_collection_invoke(locator, &mut invoke.args)
                {
                    return value;
                }

                if let Some(value) = self.lookup_callable_value(locator) {
                    let args = self.evaluate_args(&mut invoke.args);
                    return self.apply_callable(value, args);
                }

                if let Some(ident) = locator.as_ident() {
                    if let Some(closure) = self.lookup_closure(ident.as_str()) {
                        let args = self.evaluate_args(&mut invoke.args);
                        let ret_ty = Self::closure_return_ty(&closure);
                        self.set_pending_expr_ty(ret_ty);
                        return self.call_const_closure(&closure, args);
                    }
                }

                if let Some(function) = self.resolve_function_call(locator, &mut invoke.args) {
                    // Arguments are already annotated by resolve_function_call
                    tracing::debug!(
                        target: DEFAULT_DIAGNOSTIC_CONTEXT,
                        "resolved function: {} with {} args", function.name, invoke.args.len()
                    );
                    let args = self.evaluate_args(&mut invoke.args);
                    let ret_ty = Self::item_function_ret_ty(&function);
                    self.set_pending_expr_ty(ret_ty);
                    return self.call_function(function, args);
                } else {
                    tracing::debug!(target: DEFAULT_DIAGNOSTIC_CONTEXT, "failed to resolve function: {}", locator);
                }

                let _ = self.evaluate_args(&mut invoke.args);
                let name = locator.to_string();
                self.emit_error(format!(
                    "cannot resolve call target '{}' in const evaluation",
                    name
                ));
                Value::undefined()
            }
            ExprInvokeTarget::Expr(expr) => {
                let callee = self.eval_expr(expr.as_mut());
                if let Some(closure) = self.take_pending_closure() {
                    let args = self.evaluate_args(&mut invoke.args);
                    let ret_ty = Self::closure_return_ty(&closure);
                    self.set_pending_expr_ty(ret_ty);
                    return self.call_const_closure(&closure, args);
                }
                let args = self.evaluate_args(&mut invoke.args);
                self.apply_callable(callee, args)
            }
            ExprInvokeTarget::Closure(func) => {
                let args = self.evaluate_args(&mut invoke.args);
                let ret_ty = Self::value_function_ret_ty(func);
                self.set_pending_expr_ty(ret_ty);
                self.call_value_function(func, args)
            }
            ExprInvokeTarget::Type(_) | ExprInvokeTarget::BinOp(_) => {
                self.emit_error("unsupported invocation target in const evaluation");
                Value::undefined()
            }
            ExprInvokeTarget::Method(_) => unreachable!(),
        }
    }

    fn eval_invoke_runtime(&mut self, invoke: &mut ExprInvoke) -> Value {
        if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
            if self.is_printf_symbol(&locator.to_string()) {
                let mut evaluated = Vec::with_capacity(invoke.args.len());
                for arg in invoke.args.iter_mut() {
                    evaluated.push(self.eval_expr(arg));
                }
                if let Some(Value::String(format_str)) = evaluated.first() {
                    match format_runtime_string(&format_str.value, &evaluated[1..]) {
                        Ok(output) => {
                            if !output.is_empty() {
                                self.stdout.push(output);
                            }
                        }
                        Err(err) => self.emit_error(err.to_string()),
                    }
                } else {
                    self.emit_error(
                        "printf expects a string literal as the first argument at runtime",
                    );
                }
                return Value::unit();
            }
        }

        for arg in invoke.args.iter_mut() {
            self.eval_expr(arg);
        }

        self.emit_error("runtime function calls are not yet supported in interpreter mode");
        Value::undefined()
    }

    fn evaluate_args(&mut self, args: &mut Vec<Expr>) -> Vec<Value> {
        args.iter_mut().map(|arg| self.eval_expr(arg)).collect()
    }

    fn evaluate_arg_slice(&mut self, args: &mut [Expr]) -> Vec<Value> {
        args.iter_mut().map(|arg| self.eval_expr(arg)).collect()
    }

    fn lookup_callable_value(&mut self, locator: &Locator) -> Option<Value> {
        locator
            .as_ident()
            .and_then(|ident| self.lookup_value(ident.as_str()))
    }

    fn find_function(&self, locator: &Locator) -> Option<ItemDefFunction> {
        if let Some(ident) = locator.as_ident() {
            if let Some(function) = self.functions.get(ident.as_str()) {
                return Some(function.clone());
            }
        }

        let name = locator.to_string();
        self.functions.get(&name).cloned()
    }

    fn resolve_function_call(
        &mut self,
        locator: &mut Locator,
        args: &mut [Expr],
    ) -> Option<ItemDefFunction> {
        let mut candidate_names = vec![locator.to_string()];
        if let Some(ident) = locator.as_ident() {
            let simple = ident.as_str().to_string();
            if !candidate_names.contains(&simple) {
                candidate_names.push(simple);
            }
        }

        for name in &candidate_names {
            if let Some(function) = self.functions.get(name).cloned() {
                // Annotate arguments with expected parameter types
                self.annotate_invoke_args_slice(args, &function.sig.params);
                return Some(function);
            }
        }

        for name in &candidate_names {
            if let Some(template) = self.generic_functions.get(name).cloned() {
                if let Some(function) =
                    self.instantiate_generic_function(name, template, locator, args)
                {
                    // Annotate arguments now that we know the specialized function signature
                    self.annotate_invoke_args_slice(args, &function.sig.params);
                    return Some(function);
                }
            }
        }

        if let Some(function) = self.find_function(locator) {
            // Annotate arguments with expected parameter types
            self.annotate_invoke_args_slice(args, &function.sig.params);
            Some(function)
        } else {
            None
        }
    }

    fn try_handle_const_collection_invoke(
        &mut self,
        locator: &Locator,
        args: &mut [Expr],
    ) -> Option<Value> {
        let segments = Self::locator_segments(locator);

        let ends_with = |suffix: &[&str]| -> bool {
            if segments.len() < suffix.len() {
                return false;
            }
            segments
                .iter()
                .rev()
                .zip(suffix.iter().rev())
                .all(|(segment, expected)| segment == expected)
        };

        if ends_with(&["Vec", "new"]) {
            if !args.is_empty() {
                let _ = self.evaluate_arg_slice(args);
                self.emit_error("Vec::new does not take arguments in const evaluation");
                return Some(Value::undefined());
            }
            self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                ty: Box::new(Ty::Any(TypeAny)),
            })));
            return Some(Value::List(ValueList::new(Vec::new())));
        }

        if ends_with(&["Vec", "with_capacity"]) {
            let _ = self.evaluate_arg_slice(args);
            self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                ty: Box::new(Ty::Any(TypeAny)),
            })));
            return Some(Value::List(ValueList::new(Vec::new())));
        }

        if ends_with(&["Vec", "from"]) {
            let evaluated = self.evaluate_arg_slice(args);
            if evaluated.len() != 1 {
                self.emit_error("Vec::from expects a single iterable argument in const evaluation");
                return Some(Value::undefined());
            }

            match evaluated.into_iter().next() {
                None => {
                    self.emit_error("Vec::from expects one argument during const evaluation");
                    return Some(Value::undefined());
                }
                Some(val) => match val {
                    Value::List(list) => {
                        self.set_pending_expr_ty(Some(Ty::Vec(TypeVec {
                            ty: Box::new(Ty::Any(TypeAny)),
                        })));
                        return Some(Value::List(ValueList::new(list.values)));
                    }
                    other => {
                        self.emit_error(format!(
                            "Vec::from expects a list literal during const evaluation, got {:?}",
                            other
                        ));
                        return Some(Value::undefined());
                    }
                },
            }
        }

        if ends_with(&["HashMap", "new"]) {
            if !args.is_empty() {
                let _ = self.evaluate_arg_slice(args);
                self.emit_error("HashMap::new does not take arguments in const evaluation");
                return Some(Value::undefined());
            }
            return Some(Value::map(Vec::new()));
        }

        if ends_with(&["HashMap", "with_capacity"]) {
            let _ = self.evaluate_arg_slice(args);
            return Some(Value::map(Vec::new()));
        }

        if ends_with(&["HashMap", "from"]) {
            let evaluated = self.evaluate_arg_slice(args);
            if evaluated.len() != 1 {
                self.emit_error(
                    "HashMap::from expects a single iterable argument in const evaluation",
                );
                return Some(Value::undefined());
            }

            match evaluated.into_iter().next() {
                None => {
                    self.emit_error(
                        "HashMap::from expects one iterable argument during const evaluation",
                    );
                    return Some(Value::undefined());
                }
                Some(val) => match val {
                    Value::List(list) => {
                        let mut pairs = Vec::with_capacity(list.values.len());
                        for entry in list.values {
                            match entry {
                                Value::Tuple(tuple) if tuple.values.len() == 2 => {
                                    let mut iter = tuple.values.into_iter();
                                    if let (Some(key), Some(value)) = (iter.next(), iter.next()) {
                                        pairs.push((key, value));
                                    } else {
                                        self.emit_error(
                                        "HashMap::from tuple entry did not contain two elements",
                                    );
                                        return Some(Value::undefined());
                                    }
                                }
                                Value::List(inner) if inner.values.len() == 2 => {
                                    let mut iter = inner.values.into_iter();
                                    if let (Some(key), Some(value)) = (iter.next(), iter.next()) {
                                        pairs.push((key, value));
                                    } else {
                                        self.emit_error(
                                            "HashMap::from list entry did not contain two elements",
                                        );
                                        return Some(Value::undefined());
                                    }
                                }
                                other => {
                                    self.emit_error(format!(
                                    "HashMap::from expects entries to be tuples or 2-element lists, found {:?}",
                                    other
                                ));
                                    return Some(Value::undefined());
                                }
                            }
                        }
                        return Some(Value::map(pairs));
                    }
                    other => {
                        self.emit_error(format!(
                        "HashMap::from expects a list of key/value pairs during const evaluation, got {:?}",
                        other
                    ));
                        return Some(Value::undefined());
                    }
                },
            }
        }

        None
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

    fn should_replace_intrinsic_with_value(&self, kind: IntrinsicCallKind, value: &Value) -> bool {
        if matches!(value, Value::Undefined(_)) {
            return false;
        }

        matches!(
            kind,
            IntrinsicCallKind::SizeOf
                | IntrinsicCallKind::FieldCount
                | IntrinsicCallKind::FieldType
                | IntrinsicCallKind::StructSize
                | IntrinsicCallKind::TypeName
                | IntrinsicCallKind::HasField
                | IntrinsicCallKind::HasMethod
                | IntrinsicCallKind::MethodCount
                | IntrinsicCallKind::ConstBlock
        )
    }

    fn eval_block(&mut self, block: &mut ExprBlock) -> Value {
        self.push_scope();
        let mut last_value = Value::unit();
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr_stmt) => {
                    let value = self.eval_expr(expr_stmt.expr.as_mut());
                    if expr_stmt.has_value() {
                        last_value = value;
                    }
                }
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = stmt_let.init.as_mut() {
                        let value = self.eval_expr(init);
                        let _ = init;
                        if self.pending_closure.is_some() {
                            let expr_ref = stmt_let.init.as_mut();
                            self.annotate_pending_closure(Some(&mut stmt_let.pat), expr_ref);
                        }
                        self.bind_pattern(&stmt_let.pat, value);
                    } else {
                        self.emit_error(
                            "let bindings without initializer are not supported in const blocks",
                        );
                    }
                }
                BlockStmt::Item(item) => {
                    self.evaluate_item(item.as_mut());
                }
                BlockStmt::Noop => {}
                BlockStmt::Any(_) => {
                    self.emit_error("unsupported statement in const block");
                }
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

    fn eval_intrinsic(&mut self, call: &mut ExprIntrinsicCall) -> Value {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                match self.render_intrinsic_call(call) {
                    Ok(output) => {
                        if call.kind == IntrinsicCallKind::Print {
                            if let Some(last) = self.stdout.last_mut() {
                                last.push_str(&output);
                            } else {
                                self.stdout.push(output);
                            }
                        } else {
                            self.stdout.push(output);
                        }
                    }
                    Err(err) => self.emit_error(err),
                }
                Value::unit()
            }
            IntrinsicCallKind::DebugAssertions => Value::bool(self.debug_assertions),
            IntrinsicCallKind::Break => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if args.len() > 1 {
                        self.emit_error("`break` accepts at most one value in const evaluation");
                    }
                    if let Some(expr) = args.first_mut() {
                        return self.eval_expr(expr);
                    }
                }
                Value::unit()
            }
            IntrinsicCallKind::Continue => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if !args.is_empty() {
                        self.emit_error("`continue` does not accept a value in const evaluation");
                    }
                }
                Value::unit()
            }
            IntrinsicCallKind::ConstBlock => {
                if let fp_core::intrinsics::IntrinsicCallPayload::Args { args } = &mut call.payload
                {
                    if let Some(expr) = args.first_mut() {
                        return self.eval_expr(expr);
                    }
                }
                self.emit_error("const block requires an argument");
                Value::undefined()
            }
            _ => {
                let intrinsic_name = match intrinsic_symbol(call.kind) {
                    Some(name) => name,
                    None => {
                        self.emit_error(format!(
                            "intrinsic {:?} is not supported during const evaluation",
                            call.kind
                        ));
                        return Value::undefined();
                    }
                };

                let args = match &mut call.payload {
                    fp_core::intrinsics::IntrinsicCallPayload::Args { args } => args,
                    fp_core::intrinsics::IntrinsicCallPayload::Format { .. } => {
                        self.emit_error(
                            "format-style intrinsics are not supported in const evaluation",
                        );
                        return Value::undefined();
                    }
                };

                let evaluated: Vec<_> = args.iter_mut().map(|expr| self.eval_expr(expr)).collect();
                if matches!(call.kind, IntrinsicCallKind::CompileWarning) {
                    if let Some(Value::String(message)) = evaluated.first() {
                        self.emit_warning(message.value.clone());
                    }
                }
                match self.intrinsics.get(intrinsic_name) {
                    Some(function) => match function.call(&evaluated, self.ctx) {
                        Ok(value) => value,
                        Err(err) => {
                            self.emit_error(err.to_string());
                            Value::undefined()
                        }
                    },
                    None => {
                        self.emit_error(format!(
                            "intrinsic '{}' is not registered for const evaluation",
                            intrinsic_name
                        ));
                        Value::undefined()
                    }
                }
            }
        }
    }

    fn evaluate_function_body(&mut self, expr: &mut Expr) {
        let expr_ty_snapshot = expr.ty().cloned();
        let mut updated_expr_ty: Option<Ty> = None;

        match expr.kind_mut() {
            ExprKind::Quote(_quote) => {
                // Quoted fragments inside function analysis remain as-is until evaluated (e.g., by splice)
            }
            ExprKind::Splice(splice) => {
                if self.in_const_region == 0 {
                    self.emit_error("splice is only valid inside const { ... } regions");
                    return;
                }
                match splice.token.kind() {
                    ExprKind::Quote(q) => match self.build_quoted_fragment(q) {
                        QuotedFragment::Expr(e) => {
                            *expr = e;
                            self.mark_mutated();
                        }
                        QuotedFragment::Stmts(_)
                        | QuotedFragment::Items(_)
                        | QuotedFragment::Type(_) => {
                            self.emit_error(
                                "cannot splice non-expression token in expression position",
                            );
                        }
                    },
                    _ => {
                        self.emit_error("splice expects a quote token expression");
                    }
                }
            }
            ExprKind::IntrinsicContainer(collection) => {
                let new_expr = collection.clone().into_const_expr();
                *expr = new_expr;
                self.evaluate_function_body(expr);
                return;
            }
            ExprKind::Block(block) => self.evaluate_function_block(block),
            ExprKind::If(if_expr) => {
                self.evaluate_function_body(if_expr.then.as_mut());
                if let Some(else_expr) = if_expr.elze.as_mut() {
                    self.evaluate_function_body(else_expr);
                }
                self.evaluate_function_body(if_expr.cond.as_mut());
            }
            ExprKind::Loop(loop_expr) => self.evaluate_function_body(loop_expr.body.as_mut()),
            ExprKind::While(while_expr) => {
                self.evaluate_function_body(while_expr.cond.as_mut());
                self.evaluate_function_body(while_expr.body.as_mut());
            }
            ExprKind::Match(match_expr) => {
                for case in match_expr.cases.iter_mut() {
                    self.evaluate_function_body(case.cond.as_mut());
                    self.evaluate_function_body(case.body.as_mut());
                }
            }
            ExprKind::Let(expr_let) => self.evaluate_function_body(expr_let.expr.as_mut()),
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Function(locator_ref) = &invoke.target {
                    if let Some(ident) = locator_ref.as_ident() {
                        if self.lookup_closure(ident.as_str()).is_some() {
                            let mut cloned_expr = Expr::new(ExprKind::Invoke(invoke.clone()));
                            if let Some(ref ty) = expr_ty_snapshot {
                                cloned_expr.set_ty(ty.clone());
                            }
                            let saved_mutated = self.mutations_applied;
                            let saved_stdout_len = self.stdout.len();
                            let _ = self.eval_expr(&mut cloned_expr);
                            if let Some(ty) = self.pending_expr_ty.take() {
                                updated_expr_ty = Some(ty);
                            }
                            self.mutations_applied = saved_mutated;
                            if self.stdout.len() > saved_stdout_len {
                                self.stdout.truncate(saved_stdout_len);
                            }
                        }
                    }
                }

                if let ExprInvokeTarget::Function(locator) = &mut invoke.target {
                    let _ = self.resolve_function_call(locator, &mut invoke.args);
                }

                match &mut invoke.target {
                    ExprInvokeTarget::Expr(target) => self.evaluate_function_body(target.as_mut()),
                    ExprInvokeTarget::Method(select) => {
                        self.evaluate_function_body(select.obj.as_mut())
                    }
                    ExprInvokeTarget::Closure(closure) => {
                        self.evaluate_function_body(closure.body.as_mut())
                    }
                    ExprInvokeTarget::Function(_)
                    | ExprInvokeTarget::Type(_)
                    | ExprInvokeTarget::BinOp(_) => {}
                }

                for arg in invoke.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
            }
            ExprKind::Macro(macro_expr) => {
                self.emit_error(format!(
                    "macro `{}` should have been lowered before const evaluation",
                    macro_expr.invocation.path
                ));
            }
            ExprKind::IntrinsicCall(call) => {
                if matches!(call.kind, IntrinsicCallKind::ConstBlock) {
                    // Enter const region and analyze the block body for splices/quotes
                    if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                        if let Some(body) = args.first_mut() {
                            self.in_const_region += 1;
                            self.evaluate_function_body(body);
                            self.in_const_region -= 1;
                        }
                    }
                }
                self.evaluate_intrinsic_for_function_analysis(call);
            }
            ExprKind::Await(await_expr) => {
                self.evaluate_function_body(await_expr.base.as_mut());
            }
            ExprKind::Reference(reference) => {
                self.evaluate_function_body(reference.referee.as_mut());
            }
            ExprKind::Paren(paren) => self.evaluate_function_body(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.evaluate_function_body(assign.target.as_mut());
                self.evaluate_function_body(assign.value.as_mut());
            }
            ExprKind::Select(select) => self.evaluate_function_body(select.obj.as_mut()),
            ExprKind::Value(value) => match value.as_mut() {
                Value::Expr(inner) => self.evaluate_function_body(inner.as_mut()),
                Value::Function(function) => self.evaluate_function_body(function.body.as_mut()),
                _ => {}
            },
            ExprKind::Closured(closured) => self.evaluate_function_body(closured.expr.as_mut()),
            ExprKind::Closure(closure) => self.evaluate_function_body(closure.body.as_mut()),
            ExprKind::Try(expr_try) => self.evaluate_function_body(expr_try.expr.as_mut()),
            ExprKind::FormatString(template) => {
                for arg in template.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.evaluate_function_body(&mut kwarg.value);
                }
            }
            ExprKind::ArrayRepeat(repeat) => {
                self.evaluate_function_body(repeat.elem.as_mut());
                self.evaluate_function_body(repeat.len.as_mut());
            }
            ExprKind::Cast(cast) => self.evaluate_function_body(cast.expr.as_mut()),
            ExprKind::Locator(locator) => {
                // Try to specialize generic function references
                if let Some(expected_ty) = &expr_ty_snapshot {
                    if matches!(expected_ty, Ty::Function(_)) {
                        self.specialize_function_reference(locator, expected_ty);
                    }
                }
            }
            ExprKind::Item(item) => self.evaluate_item(item.as_mut()),
            ExprKind::Any(_)
            | ExprKind::Id(_)
            | ExprKind::UnOp(_)
            | ExprKind::BinOp(_)
            | ExprKind::Dereference(_)
            | ExprKind::Index(_)
            | ExprKind::Range(_)
            | ExprKind::Tuple(_)
            | ExprKind::Array(_)
            | ExprKind::Struct(_)
            | ExprKind::Structural(_)
            | ExprKind::Splat(_)
            | ExprKind::SplatDict(_) => {}
        }

        if let Some(ty) = updated_expr_ty {
            expr.set_ty(ty);
        }
    }

    fn evaluate_function_block(&mut self, block: &mut ExprBlock) {
        self.push_scope();
        // Rebuild statements to allow splice expansion
        let mut new_stmts: Vec<BlockStmt> = Vec::with_capacity(block.stmts.len());
        for mut stmt in std::mem::take(&mut block.stmts) {
            match &mut stmt {
                BlockStmt::Expr(expr_stmt) => {
                    // Handle statement-position splice expansion
                    if let ExprKind::Splice(splice) = expr_stmt.expr.kind_mut() {
                        if self.in_const_region == 0 {
                            self.emit_error("splice is only valid inside const { ... } regions");
                            // Keep original statement to avoid dropping code
                            new_stmts.push(stmt);
                        } else {
                            match splice.token.kind() {
                                ExprKind::Quote(q) => {
                                    match self.build_quoted_fragment(q) {
                                        QuotedFragment::Stmts(stmts) => {
                                            for s in stmts {
                                                new_stmts.push(s.clone());
                                            }
                                            self.mark_mutated();
                                        }
                                        QuotedFragment::Items(_items) => {
                                            // Forbid: item splicing inside function bodies is not supported
                                            // Suggest moving item emission to a module-level const region.
                                            self.emit_error(
                                            "item splicing is not allowed inside function bodies; move item emission to a module-level const block",
                                        );
                                            // Keep the original statement to avoid silently dropping code.
                                            new_stmts.push(stmt);
                                        }
                                        QuotedFragment::Expr(e) => {
                                            let mut es = expr_stmt.clone();
                                            es.expr = e.into();
                                            es.semicolon = Some(true);
                                            new_stmts.push(BlockStmt::Expr(es));
                                            self.mark_mutated();
                                        }
                                        QuotedFragment::Type(_) => {
                                            self.emit_error("cannot splice a type fragment at statement position");
                                            new_stmts.push(stmt);
                                        }
                                    }
                                }
                                _ => {
                                    self.emit_error("splice expects a quote token expression");
                                    new_stmts.push(stmt);
                                }
                            }
                        }
                    } else if let ExprKind::IntrinsicCall(call) = expr_stmt.expr.kind_mut() {
                        if matches!(call.kind, IntrinsicCallKind::ConstBlock) {
                            // Inline const { ... } statements into surrounding block
                            if let IntrinsicCallPayload::Args { args } = &mut call.payload {
                                if let Some(body_expr) = args.first_mut() {
                                    if let ExprKind::Block(inner_block) = body_expr.kind_mut() {
                                        self.in_const_region += 1;
                                        self.evaluate_function_block(inner_block);
                                        self.in_const_region -= 1;
                                        for s in inner_block.stmts.clone() {
                                            new_stmts.push(s);
                                        }
                                        self.mark_mutated();
                                        continue;
                                    }
                                }
                            }
                        }
                        // Generic case
                        self.evaluate_function_body(expr_stmt.expr.as_mut());
                        new_stmts.push(stmt);
                    } else {
                        // Generic case: analyze expression and keep statement
                        self.evaluate_function_body(expr_stmt.expr.as_mut());
                        new_stmts.push(stmt);
                    }
                }
                BlockStmt::Item(item) => {
                    self.evaluate_item(item.as_mut());
                    new_stmts.push(stmt);
                }
                BlockStmt::Let(let_stmt) => {
                    self.evaluate_function_let_stmt(let_stmt);
                    new_stmts.push(stmt);
                }
                BlockStmt::Noop | BlockStmt::Any(_) => new_stmts.push(stmt),
            }
        }
        block.stmts = new_stmts;
        if let Some(last) = block.last_expr_mut() {
            self.evaluate_function_body(last);
        }
        self.pop_scope();
    }

    // Removed: evaluate_function_stmt was unused and duplicative of evaluate_function_block logic.

    fn evaluate_function_let_stmt(&mut self, stmt_let: &mut StmtLet) {
        if let Some(init) = stmt_let.init.as_mut() {
            if Self::expr_is_potential_closure(init) {
                self.evaluate_closure_initializer(&mut stmt_let.pat, init);
            }
            self.evaluate_function_body(init);
        }
        if let Some(diverge) = stmt_let.diverge.as_mut() {
            self.evaluate_function_body(diverge);
        }
    }

    fn expr_is_potential_closure(expr: &Expr) -> bool {
        expr.ty()
            .map(|ty| matches!(ty, Ty::Function(_)))
            .unwrap_or(false)
            || matches!(expr.kind(), ExprKind::Closure(_))
    }

    fn evaluate_closure_initializer(&mut self, pat: &mut Pattern, init: &mut Expr) {
        let mut expr_clone = init.clone();
        let saved_mutated = self.mutations_applied;
        let saved_pending = self.pending_closure.take();
        let saved_stdout_len = self.stdout.len();
        let value = self.eval_expr(&mut expr_clone);
        if self.pending_closure.is_some() {
            self.annotate_pending_closure(Some(pat), Some(init));
        }
        self.bind_pattern(pat, value);
        self.mutations_applied = saved_mutated;
        self.pending_closure = saved_pending;
        if self.stdout.len() > saved_stdout_len {
            self.stdout.truncate(saved_stdout_len);
        }
    }

    fn evaluate_intrinsic_for_function_analysis(&mut self, call: &mut ExprIntrinsicCall) {
        if self.should_replace_intrinsic_with_value(call.kind, &Value::unit()) {
            let value = self.eval_intrinsic(call);
            if !matches!(value, Value::Undefined(_)) {
                return;
            }
        }

        match &mut call.payload {
            IntrinsicCallPayload::Format { template } => {
                for arg in template.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.evaluate_function_body(&mut kwarg.value);
                }
            }
            IntrinsicCallPayload::Args { args } => {
                for arg in args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
            }
        }
    }

    fn render_intrinsic_call(
        &mut self,
        call: &mut ExprIntrinsicCall,
    ) -> std::result::Result<String, String> {
        match &mut call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                self.render_format_template(template)
            }
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                let mut rendered = Vec::with_capacity(args.len());
                for expr in args.iter_mut() {
                    let value = self.eval_expr(expr);
                    let text =
                        format_value_with_spec(&value, None).map_err(|err| err.to_string())?;
                    rendered.push(text);
                }
                Ok(rendered.join(" "))
            }
        }
    }

    fn render_format_template(
        &mut self,
        template: &mut ExprFormatString,
    ) -> std::result::Result<String, String> {
        let positional: Vec<Value> = template
            .args
            .iter_mut()
            .map(|expr| self.eval_expr(expr))
            .collect();

        for (expr, value) in template.args.iter_mut().zip(positional.iter()) {
            if let Some(kind) = match expr.kind() {
                ExprKind::IntrinsicCall(call) => Some(call.kind),
                _ => None,
            } {
                if self.should_replace_intrinsic_with_value(kind, value) {
                    let mut replacement = Expr::value(value.clone());
                    replacement.ty = expr.ty.clone();
                    *expr = replacement;
                    self.mark_mutated();
                }
            }
        }

        let mut named = HashMap::new();
        for kwarg in template.kwargs.iter_mut() {
            let value = self.eval_expr(&mut kwarg.value);
            named.insert(kwarg.name.clone(), value);
        }

        let mut output = String::new();
        let mut implicit_index = 0usize;

        for part in &template.parts {
            match part {
                FormatTemplatePart::Literal(literal) => output.push_str(literal),
                FormatTemplatePart::Placeholder(placeholder) => {
                    let value = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let index = implicit_index;
                            implicit_index += 1;
                            positional.get(index)
                        }
                        FormatArgRef::Positional(index) => positional.get(*index),
                        FormatArgRef::Named(name) => named.get(name),
                    }
                    .ok_or_else(|| match &placeholder.arg_ref {
                        FormatArgRef::Implicit => format!(
                            "implicit format placeholder {{}} has no argument at position {}",
                            implicit_index.saturating_sub(1)
                        ),
                        FormatArgRef::Positional(index) => format!(
                            "format placeholder {{{}}} references missing positional argument",
                            index
                        ),
                        FormatArgRef::Named(name) => format!(
                            "format placeholder {{{name}}} references undefined keyword argument"
                        ),
                    })?;

                    let formatted =
                        format_value_with_spec(value, placeholder.format_spec.as_deref())
                            .map_err(|err| err.to_string())?;
                    output.push_str(&formatted);
                }
            }
        }

        Ok(output)
    }

    fn evaluate_binop(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        match op {
            BinOpKind::Add | BinOpKind::AddTrait => self.binop_add(lhs, rhs),
            BinOpKind::Sub => self.binop_sub(lhs, rhs),
            BinOpKind::Mul => self.binop_mul(lhs, rhs),
            BinOpKind::Div => self.binop_div(lhs, rhs),
            BinOpKind::Mod => self.binop_mod(lhs, rhs),
            BinOpKind::Gt | BinOpKind::Ge | BinOpKind::Lt | BinOpKind::Le => {
                self.binop_ordering(op, lhs, rhs)
            }
            BinOpKind::Eq | BinOpKind::Ne => self.binop_equality(op, lhs, rhs),
            BinOpKind::Or | BinOpKind::And => self.binop_logical(op, lhs, rhs),
            BinOpKind::BitAnd | BinOpKind::BitOr | BinOpKind::BitXor => {
                self.binop_bitwise(op, lhs, rhs)
            }
        }
    }

    fn binop_add(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value + r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value + r.value)),
            (Value::String(l), Value::String(r)) => {
                Ok(Value::string(format!("{}{}", l.value, r.value)))
            }
            other => Err(interpretation_error(format!(
                "unsupported operands for '+': {:?}",
                other
            ))),
        }
    }

    fn binop_sub(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value - r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value - r.value)),
            other => Err(interpretation_error(format!(
                "unsupported operands for '-': {:?}",
                other
            ))),
        }
    }

    fn binop_mul(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value * r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value * r.value)),
            other => Err(interpretation_error(format!(
                "unsupported operands for '*': {:?}",
                other
            ))),
        }
    }

    fn binop_div(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                if r.value == 0 {
                    Err(interpretation_error("division by zero".to_string()))
                } else if l.value % r.value == 0 {
                    Ok(Value::int(l.value / r.value))
                } else {
                    Ok(Value::decimal(l.value as f64 / r.value as f64))
                }
            }
            (Value::Decimal(l), Value::Decimal(r)) => {
                if r.value == 0.0 {
                    Err(interpretation_error("division by zero".to_string()))
                } else {
                    Ok(Value::decimal(l.value / r.value))
                }
            }
            other => Err(interpretation_error(format!(
                "unsupported operands for '/': {:?}",
                other
            ))),
        }
    }

    fn binop_mod(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value % r.value)),
            other => Err(interpretation_error(format!(
                "unsupported operands for '%': {:?}",
                other
            ))),
        }
    }

    fn binop_ordering(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        let ordering = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => l.value.cmp(&r.value),
            (Value::Decimal(l), Value::Decimal(r)) => l.value.total_cmp(&r.value),
            (Value::String(l), Value::String(r)) => l.value.cmp(&r.value),
            other => {
                return Err(interpretation_error(format!(
                    "unsupported operands for comparison: {:?}",
                    other
                )))
            }
        };

        let result = match op {
            BinOpKind::Gt => ordering.is_gt(),
            BinOpKind::Ge => ordering.is_ge(),
            BinOpKind::Lt => ordering.is_lt(),
            BinOpKind::Le => ordering.is_le(),
            _ => unreachable!(),
        };

        Ok(Value::bool(result))
    }

    fn binop_equality(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        let eq = lhs == rhs;
        let result = match op {
            BinOpKind::Eq => eq,
            BinOpKind::Ne => !eq,
            _ => unreachable!(),
        };
        Ok(Value::bool(result))
    }

    fn binop_logical(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        let (l, r) = match (lhs, rhs) {
            (Value::Bool(l), Value::Bool(r)) => (l.value, r.value),
            other => {
                return Err(interpretation_error(format!(
                    "logical operators require booleans, found {:?}",
                    other
                )))
            }
        };

        let result = match op {
            BinOpKind::Or => l || r,
            BinOpKind::And => l && r,
            _ => unreachable!(),
        };
        Ok(Value::bool(result))
    }

    fn binop_bitwise(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                let result = match op {
                    BinOpKind::BitAnd => l.value & r.value,
                    BinOpKind::BitOr => l.value | r.value,
                    BinOpKind::BitXor => l.value ^ r.value,
                    _ => unreachable!(),
                };
                Ok(Value::int(result))
            }
            other => Err(interpretation_error(format!(
                "bitwise operators require integers, found {:?}",
                other
            ))),
        }
    }

    fn evaluate_unary(&self, op: UnOpKind, value: Value) -> Result<Value> {
        match op {
            UnOpKind::Not => match value {
                Value::Bool(b) => Ok(Value::bool(!b.value)),
                other => Err(interpretation_error(format!(
                    "operator '!' requires boolean, found {}",
                    other
                ))),
            },
            UnOpKind::Neg => match value {
                Value::Int(i) => Ok(Value::int(-i.value)),
                Value::Decimal(d) => Ok(Value::decimal(-d.value)),
                other => Err(interpretation_error(format!(
                    "operator '-' requires numeric operand, found {}",
                    other
                ))),
            },
            UnOpKind::Deref | UnOpKind::Any(_) => Err(interpretation_error(
                "unsupported unary operator in AST interpretation".to_string(),
            )),
        }
    }

    fn annotate_pending_closure(&mut self, pattern: Option<&mut Pattern>, expr: Option<&mut Expr>) {
        if self.pending_closure.is_none() {
            return;
        }

        let candidate_ty = self
            .pending_closure
            .as_ref()
            .and_then(|pending| pending.function_ty.clone())
            .or_else(|| expr.as_ref().and_then(|expr| expr.ty().cloned()))
            .or_else(|| pattern.as_ref().and_then(|pat| pat.ty().cloned()));

        let Some(function_ty) = candidate_ty else {
            return;
        };

        if matches!(function_ty, Ty::Unknown(_)) {
            return;
        }

        if let Some(expr) = expr {
            expr.set_ty(function_ty.clone());
            if let Ty::Function(ref fn_sig) = function_ty {
                if let ExprKind::Closure(closure) = expr.kind_mut() {
                    Self::annotate_expr_closure(closure, fn_sig);
                }
            }
        }

        if let Some(pattern) = pattern {
            pattern.set_ty(function_ty.clone());
        }

        if let Some(pending) = self.pending_closure.as_mut() {
            pending.function_ty = Some(function_ty.clone());
            if let Ty::Function(ref fn_sig) = function_ty {
                Self::annotate_const_closure(pending, fn_sig);
            }
        }
    }

    fn annotate_expr_closure(closure: &mut ExprClosure, fn_sig: &TypeFunction) {
        for (param, param_ty) in closure.params.iter_mut().zip(fn_sig.params.iter()) {
            if !matches!(param_ty, Ty::Unknown(_)) {
                param.set_ty(param_ty.clone());
            }
        }

        if let Some(ret_ty) = fn_sig.ret_ty.as_ref() {
            if !matches!(ret_ty.as_ref(), Ty::Unknown(_)) {
                closure.body.set_ty(ret_ty.as_ref().clone());
            }
        }
    }

    fn annotate_const_closure(closure: &mut ConstClosure, fn_sig: &TypeFunction) {
        for (param, param_ty) in closure.params.iter_mut().zip(fn_sig.params.iter()) {
            if !matches!(param_ty, Ty::Unknown(_)) {
                param.set_ty(param_ty.clone());
            }
        }

        closure.function_ty = Some(Ty::Function(fn_sig.clone()));

        if let Some(ret_ty) = fn_sig.ret_ty.as_ref() {
            if !matches!(ret_ty.as_ref(), Ty::Unknown(_)) {
                closure.body.set_ty(ret_ty.as_ref().clone());
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value: Value) {
        if let Some(ident) = pattern.as_ident() {
            self.insert_value(ident.as_str(), value);
        } else {
            self.emit_error("only simple identifier patterns are supported in const evaluation");
        }
    }

    fn insert_value(&mut self, name: &str, value: Value) {
        if let Some(closure) = self.pending_closure.take() {
            if let Some(fn_ty) = closure.function_ty.clone() {
                let key = self.qualified_name(name);
                self.closure_types.insert(key, fn_ty.clone());
                self.insert_type(name, fn_ty);
            }
            if let Some(scope) = self.value_env.last_mut() {
                scope.insert(name.to_string(), StoredValue::Closure(closure));
            }
            return;
        }

        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), StoredValue::Plain(value));
        }
    }

    fn insert_type(&mut self, name: &str, ty: Ty) {
        if let Some(scope) = self.type_env.last_mut() {
            scope.insert(name.to_string(), ty.clone());
        }
        let qualified = self.qualified_name(name);
        self.global_types.insert(qualified, ty.clone());

        // Sync with typer
        if let Some(typer) = self.typer.as_mut() {
            typer.bind_variable(name, ty);
        }
    }

    fn lookup_value(&self, name: &str) -> Option<Value> {
        for scope in self.value_env.iter().rev() {
            match scope.get(name) {
                Some(StoredValue::Plain(value)) => return Some(value.clone()),
                Some(StoredValue::Closure(_)) => return None,
                None => {}
            }
        }
        if self.is_printf_symbol(name) {
            return Some(Value::unit());
        }
        if name.contains("printf") {
            return Some(Value::unit());
        }
        None
    }

    fn lookup_closure(&self, name: &str) -> Option<ConstClosure> {
        for scope in self.value_env.iter().rev() {
            match scope.get(name) {
                Some(StoredValue::Closure(closure)) => return Some(closure.clone()),
                Some(StoredValue::Plain(_)) => return None,
                None => {}
            }
        }
        None
    }

    fn take_pending_closure(&mut self) -> Option<ConstClosure> {
        self.pending_closure.take()
    }

    fn set_pending_expr_ty(&mut self, ty: Option<Ty>) {
        let ty = ty.unwrap_or_else(|| Ty::Unit(TypeUnit));
        self.pending_expr_ty = Some(ty);
    }

    fn closure_return_ty(closure: &ConstClosure) -> Option<Ty> {
        closure.ret_ty.clone().or_else(|| {
            closure
                .function_ty
                .as_ref()
                .and_then(Self::function_ret_ty_from_ty)
        })
    }

    fn item_function_ret_ty(function: &ItemDefFunction) -> Option<Ty> {
        function.sig.ret_ty.clone().or_else(|| {
            function
                .ty
                .as_ref()
                .and_then(|ty| Self::type_function_ret_ty(ty))
        })
    }

    fn value_function_ret_ty(function: &ValueFunction) -> Option<Ty> {
        function.sig.ret_ty.clone()
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

    fn build_quoted_fragment(&mut self, quote: &fp_core::ast::ExprQuote) -> QuotedFragment {
        // If kind is explicitly provided, we still infer from block shape to build fragment
        let block = quote.block.clone();
        // If only a single expression without preceding statements ⇒ Expr fragment
        let is_only_expr = block.first_stmts().is_empty() && block.last_expr().is_some();
        if is_only_expr {
            if let Some(expr) = block.last_expr() {
                return QuotedFragment::Expr(expr.clone());
            }
            // Defensive fallback: should be unreachable given is_only_expr check
            return QuotedFragment::Stmts(block.stmts.clone());
        }
        // If contains only items ⇒ Items fragment
        let only_items = block.stmts.iter().all(|s| matches!(s, BlockStmt::Item(_)));
        if only_items {
            let items: Vec<Item> = block
                .stmts
                .iter()
                .filter_map(|s| match s {
                    BlockStmt::Item(i) => Some((**i).clone()),
                    _ => None,
                })
                .collect();
            return QuotedFragment::Items(items);
        }
        // Fallback ⇒ Stmts fragment (keep entire statement list)
        QuotedFragment::Stmts(block.stmts.clone())
    }
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
