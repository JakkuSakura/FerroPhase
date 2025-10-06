use std::collections::HashMap;
use std::mem;
use std::sync::Arc;

use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprClosure, ExprFormatString, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, FormatArgRef, FormatTemplatePart, Item, ItemDefFunction, ItemKind,
    Node, NodeKind, StmtLet, Ty, TypeFunction, TypeInt, TypePrimitive, TypeUnit, Value, ValueField,
    ValueFunction, ValueList, ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use fp_core::id::Locator;
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};
use fp_core::ops::{format_runtime_string, format_value_with_spec, BinOpKind, UnOpKind};
use fp_core::pat::Pattern;

use crate::error::interpretation_error;
use crate::intrinsics::IntrinsicsRegistry;

const DEFAULT_DIAGNOSTIC_CONTEXT: &str = "ast-interpreter";

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
    mutations_applied: bool,
    pending_closure: Option<ConstClosure>,
    pending_expr_ty: Option<Ty>,
    closure_types: HashMap<String, Ty>,
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
            mutations_applied: false,
            pending_closure: None,
            pending_expr_ty: None,
            closure_types: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, node: &mut Node) {
        match node.kind_mut() {
            NodeKind::File(file) => {
                for item in &mut file.items {
                    self.evaluate_item(item);
                }
            }
            NodeKind::Item(item) => self.evaluate_item(item),
            NodeKind::Expr(expr) => {
                self.eval_expr(expr);
            }
        }
    }

    pub fn execute_main(&mut self) -> Option<Value> {
        if !matches!(self.mode, InterpreterMode::RunTime) {
            return None;
        }

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
                let value = self.eval_expr(def.value.as_mut());
                if let Some(pending) = self.pending_closure.as_mut() {
                    let resolved_ty = pending
                        .function_ty
                        .clone()
                        .or_else(|| def.ty.clone())
                        .or_else(|| def.ty_annotation().cloned());

                    if let Some(fn_ty) = resolved_ty {
                        pending.function_ty = Some(fn_ty.clone());
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
                    let expr_value = Expr::value(value);
                    *def.value = expr_value;
                    self.mark_mutated();
                }
            }
            ItemKind::DefStatic(def) => {
                let value = self.eval_expr(def.value.as_mut());
                if let Some(pending) = self.pending_closure.as_mut() {
                    let resolved_ty = pending
                        .function_ty
                        .clone()
                        .or_else(|| Some(def.ty.clone()))
                        .or_else(|| def.ty_annotation().cloned());

                    if let Some(fn_ty) = resolved_ty {
                        pending.function_ty = Some(fn_ty.clone());
                        def.ty = fn_ty.clone();
                        def.ty_annotation = Some(fn_ty.clone());
                        def.value.set_ty(fn_ty.clone());
                    }
                }
                let expr_value = Expr::value(value.clone());
                self.insert_value(def.name.as_str(), value);
                *def.value = expr_value;
                self.mark_mutated();
            }
            ItemKind::Module(module) => {
                self.module_stack.push(module.name.as_str().to_string());
                self.push_scope();
                for child in &mut module.items {
                    self.evaluate_item(child);
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
                self.functions
                    .insert(func.name.as_str().to_string(), func.clone());
                self.evaluate_function_body(func.body.as_mut());
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
        match expr.kind_mut() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Expr(inner) => {
                    let mut cloned = inner.as_ref().clone();
                    self.eval_expr(&mut cloned)
                }
                other => other.clone(),
            },
            ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if let Some(value) = self.lookup_value(ident.as_str()) {
                        let mut replacement = Expr::value(value.clone());
                        replacement.ty = expr.ty.clone();
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
            ExprKind::Closure(closure) => {
                let captured = self.capture_closure(closure, expr_ty_snapshot.clone());
                self.pending_closure = Some(captured);
                Value::unit()
            }
            ExprKind::Closured(closured) => {
                let mut inner = closured.expr.as_ref().clone();
                self.eval_expr(&mut inner)
            }
            _ => {
                self.emit_error("expression not supported in AST interpretation");
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
                if let Value::List(list) = value {
                    self.pending_expr_ty = Some(Ty::Primitive(TypePrimitive::Int(TypeInt::I64)));
                    return Value::int(list.values.len() as i64);
                }
                self.emit_error("'len' is only supported on compile-time arrays and lists");
                return Value::undefined();
            }

            self.emit_error("method calls are not supported in const evaluation yet");
            return Value::undefined();
        }

        match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
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

                if let Some(function) = self.find_function(locator) {
                    let args = self.evaluate_args(&mut invoke.args);
                    let ret_ty = Self::item_function_ret_ty(&function);
                    self.set_pending_expr_ty(ret_ty);
                    return self.call_function(function, args);
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

    fn apply_callable(&mut self, callee: Value, args: Vec<Value>) -> Value {
        if matches!(callee, Value::Unit(_)) && args.is_empty() {
            return Value::unit();
        }

        self.emit_error("unsupported callable in const evaluation");
        Value::undefined()
    }

    fn capture_closure(&self, closure: &ExprClosure, ty: Option<Ty>) -> ConstClosure {
        ConstClosure {
            params: closure.params.clone(),
            ret_ty: closure.ret_ty.as_ref().map(|ty| (**ty).clone()),
            body: closure.body.as_ref().clone(),
            captured_values: self.value_env.clone(),
            captured_types: self.type_env.clone(),
            module_stack: self.module_stack.clone(),
            function_ty: ty,
        }
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
                        if let Some(pending) = self.pending_closure.as_ref() {
                            if let Some(fn_ty) = pending.function_ty.clone() {
                                stmt_let.pat.set_ty(fn_ty.clone());
                                init.set_ty(fn_ty.clone());
                            }
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
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => Value::unit(),
            IntrinsicCallKind::DebugAssertions => Value::bool(self.debug_assertions),
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
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    if let Some(ident) = locator.as_ident() {
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
            ExprKind::IntrinsicCall(call) => {
                self.evaluate_intrinsic_for_function_analysis(call);
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
            ExprKind::Item(item) => self.evaluate_item(item.as_mut()),
            ExprKind::Any(_)
            | ExprKind::Id(_)
            | ExprKind::Locator(_)
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
        for stmt in block.stmts.iter_mut() {
            self.evaluate_function_stmt(stmt);
        }
        if let Some(last) = block.last_expr_mut() {
            self.evaluate_function_body(last);
        }
        self.pop_scope();
    }

    fn evaluate_function_stmt(&mut self, stmt: &mut BlockStmt) {
        match stmt {
            BlockStmt::Item(item) => self.evaluate_item(item.as_mut()),
            BlockStmt::Expr(expr_stmt) => self.evaluate_function_body(expr_stmt.expr.as_mut()),
            BlockStmt::Let(stmt_let) => self.evaluate_function_let_stmt(stmt_let),
            BlockStmt::Noop | BlockStmt::Any(_) => {}
        }
    }

    fn evaluate_function_let_stmt(&mut self, stmt_let: &mut StmtLet) {
        if let Some(init) = stmt_let.init.as_mut() {
            if Self::expr_is_potential_closure(init) {
                let cloned = init.clone();
                self.evaluate_closure_initializer(&stmt_let.pat, cloned);
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

    fn evaluate_closure_initializer(&mut self, pat: &Pattern, init: Expr) {
        let mut expr_clone = init;
        let saved_mutated = self.mutations_applied;
        let saved_pending = self.pending_closure.take();
        let saved_stdout_len = self.stdout.len();
        let value = self.eval_expr(&mut expr_clone);
        self.bind_pattern(pat, value);
        self.mutations_applied = saved_mutated;
        if let Some(prev) = saved_pending {
            self.pending_closure = Some(prev);
        }
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

    #[allow(dead_code)]
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

    #[allow(dead_code)]
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
        self.global_types.insert(qualified, ty);
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
    }

    fn pop_scope(&mut self) {
        self.value_env.pop();
        self.type_env.pop();
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
