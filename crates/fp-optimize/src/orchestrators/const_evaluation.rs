use std::collections::HashMap;
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, ExprFormatString, ExprInvokeTarget, ExprKind,
    FormatArgRef, FormatTemplatePart, Item, ItemDefFunction, ItemKind, Node, NodeKind, Ty, Value,
    ValueField, ValueList, ValueStruct, ValueStructural, ValueTuple,
};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{format_runtime_string, format_value_with_spec, BinOpKind, UnOpKind};
use fp_core::pat::Pattern;
use fp_interpret::intrinsics::IntrinsicsRegistry;

use crate::error::optimization_error;
use crate::utils::ConstEvalTracker;

const DIAGNOSTIC_CONTEXT: &str = "const-eval";

/// Result of running const evaluation on the typed AST.
#[derive(Debug, Default, Clone)]
pub struct ConstEvalOutcome {
    pub evaluated_constants: HashMap<String, Value>,
    pub mutations_applied: bool,
    pub diagnostics: Vec<Diagnostic>,
    pub has_errors: bool,
    pub stdout: Vec<String>,
}

/// Const-evaluation orchestrator that operates directly on the typed AST.
pub struct ConstEvaluationOrchestrator {
    diagnostics: Option<Arc<DiagnosticManager>>,
    debug_assertions: bool,
    execute_main: bool,
}

impl ConstEvaluationOrchestrator {
    pub fn new(_serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            diagnostics: None,
            debug_assertions: false,
            execute_main: false,
        }
    }

    pub fn with_diagnostics(mut self, manager: Arc<DiagnosticManager>) -> Self {
        self.diagnostics = Some(manager);
        self
    }

    pub fn set_debug_assertions(&mut self, enabled: bool) {
        self.debug_assertions = enabled;
    }

    pub fn set_execute_main(&mut self, enabled: bool) {
        self.execute_main = enabled;
    }

    pub fn evaluate(
        &mut self,
        ast: &mut Node,
        ctx: &SharedScopedContext,
    ) -> Result<ConstEvalOutcome> {
        let mut evaluator =
            AstConstEvaluator::new(ctx, self.diagnostics.clone(), self.debug_assertions);
        evaluator.evaluate(ast);
        if self.execute_main {
            evaluator.execute_main();
        }
        let mutations_applied = evaluator.apply_tracker(ast)?;
        Ok(ConstEvalOutcome {
            evaluated_constants: evaluator.take_constants(),
            mutations_applied,
            diagnostics: evaluator.take_diagnostics(),
            has_errors: evaluator.has_errors(),
            stdout: evaluator.take_stdout(),
        })
    }
}

struct AstConstEvaluator<'ctx> {
    ctx: &'ctx SharedScopedContext,
    diag_manager: Option<Arc<DiagnosticManager>>,
    intrinsics: IntrinsicsRegistry,
    tracker: ConstEvalTracker,
    debug_assertions: bool,

    module_stack: Vec<String>,
    value_env: Vec<HashMap<String, Value>>,
    type_env: Vec<HashMap<String, Ty>>,
    global_types: HashMap<String, Ty>,
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
    evaluated_constants: HashMap<String, Value>,
    stdout: Vec<String>,
    functions: HashMap<String, ItemDefFunction>,
}

impl<'ctx> AstConstEvaluator<'ctx> {
    fn new(
        ctx: &'ctx SharedScopedContext,
        diag_manager: Option<Arc<DiagnosticManager>>,
        debug_assertions: bool,
    ) -> Self {
        Self {
            ctx,
            diag_manager,
            intrinsics: IntrinsicsRegistry::new(),
            tracker: ConstEvalTracker::new(),
            debug_assertions,
            module_stack: Vec::new(),
            value_env: vec![HashMap::new()],
            type_env: vec![HashMap::new()],
            global_types: HashMap::new(),
            diagnostics: Vec::new(),
            has_errors: false,
            evaluated_constants: HashMap::new(),
            stdout: Vec::new(),
            functions: HashMap::new(),
        }
    }

    fn evaluate(&mut self, node: &mut Node) {
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

    fn apply_tracker(&mut self, ast: &mut Node) -> Result<bool> {
        self.tracker.apply(ast)
    }

    fn take_constants(&mut self) -> HashMap<String, Value> {
        std::mem::take(&mut self.evaluated_constants)
    }

    fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    fn has_errors(&self) -> bool {
        self.has_errors
    }

    fn take_stdout(&mut self) -> Vec<String> {
        std::mem::take(&mut self.stdout)
    }

    fn execute_main(&mut self) {
        if let Some(mut body_expr) = self.functions.get("main").map(|func| (*func.body).clone()) {
            self.push_scope();
            let _ = self.eval_expr(&mut body_expr);
            self.pop_scope();
        }
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
                let value = self.eval_expr(def.value.as_mut());
                let qualified = self.qualified_name(def.name.as_str());
                let expr_value = Expr::value(value.clone());
                self.insert_value(def.name.as_str(), value.clone());
                self.evaluated_constants.insert(qualified, value);
                *def.value = expr_value;
            }
            ItemKind::DefStatic(def) => {
                let value = self.eval_expr(def.value.as_mut());
                let expr_value = Expr::value(value.clone());
                self.insert_value(def.name.as_str(), value);
                *def.value = expr_value;
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
        let result = match expr.kind_mut() {
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
                        value
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
                        self.emit_error("expected boolean condition in const if expression");
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
                    return value;
                }
                value
            }
            ExprKind::Reference(reference) => self.eval_expr(reference.referee.as_mut()),
            ExprKind::Paren(paren) => self.eval_expr(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.emit_error("assignment is not allowed in const evaluation");
                self.eval_expr(assign.value.as_mut())
            }
            ExprKind::Let(expr_let) => {
                let value = self.eval_expr(expr_let.expr.as_mut());
                self.bind_pattern(&expr_let.pat, value.clone());
                value
            }
            ExprKind::Invoke(invoke) => {
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    if locator
                        .as_ident()
                        .map(|id| id.as_str() == "printf")
                        .unwrap_or(false)
                    {
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
                            self.emit_error("printf expects a string literal as the first argument during const evaluation");
                        }
                        Value::unit()
                    } else {
                        self.emit_error("function calls are not supported in const evaluation");
                        Value::undefined()
                    }
                } else {
                    self.emit_error(
                        "indirect function calls are not supported in const evaluation",
                    );
                    Value::undefined()
                }
            }
            _ => {
                self.emit_error("expression not supported in const evaluation");
                Value::undefined()
            }
        };

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

    fn eval_intrinsic(&mut self, call: &mut fp_core::ast::ExprIntrinsicCall) -> Value {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                match self.render_intrinsic_call(call) {
                    Ok(mut text) => {
                        if matches!(call.kind, IntrinsicCallKind::Println) {
                            text.push('\n');
                        }
                        self.stdout.push(text);
                    }
                    Err(err) => self.emit_error(err),
                }
                Value::unit()
            }
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
        match expr.kind_mut() {
            ExprKind::Block(block) => {
                for stmt in block.stmts.iter_mut() {
                    match stmt {
                        BlockStmt::Item(item) => self.evaluate_item(item.as_mut()),
                        BlockStmt::Expr(expr_stmt) => {
                            self.evaluate_function_body(expr_stmt.expr.as_mut())
                        }
                        BlockStmt::Let(stmt_let) => {
                            if let Some(init) = stmt_let.init.as_mut() {
                                self.evaluate_function_body(init);
                            }
                            if let Some(diverge) = stmt_let.diverge.as_mut() {
                                self.evaluate_function_body(diverge);
                            }
                        }
                        BlockStmt::Noop | BlockStmt::Any(_) => {}
                    }
                }
            }
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
                match &mut invoke.target {
                    ExprInvokeTarget::Function(locator)
                        if locator
                            .as_ident()
                            .map(|id| id.as_str() == "printf")
                            .unwrap_or(false) =>
                    {
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
                            self.emit_error("printf expects a string literal as the first argument during const evaluation");
                        }
                    }
                    ExprInvokeTarget::Expr(inner) => self.evaluate_function_body(inner.as_mut()),
                    ExprInvokeTarget::Method(select) => {
                        self.evaluate_function_body(select.obj.as_mut())
                    }
                    ExprInvokeTarget::Type(_)
                    | ExprInvokeTarget::Function(_)
                    | ExprInvokeTarget::Closure(_)
                    | ExprInvokeTarget::BinOp(_) => {}
                }

                for arg in invoke.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
            }
            ExprKind::Item(item) => self.evaluate_item(item.as_mut()),
            ExprKind::Closured(closured) => self.evaluate_function_body(closured.expr.as_mut()),
            ExprKind::Closure(closure) => self.evaluate_function_body(closure.body.as_mut()),
            ExprKind::Try(expr_try) => self.evaluate_function_body(expr_try.expr.as_mut()),
            ExprKind::Reference(reference) => {
                self.evaluate_function_body(reference.referee.as_mut())
            }
            ExprKind::Paren(paren) => self.evaluate_function_body(paren.expr.as_mut()),
            ExprKind::Assign(assign) => {
                self.evaluate_function_body(assign.target.as_mut());
                self.evaluate_function_body(assign.value.as_mut());
            }
            ExprKind::Select(select) => self.evaluate_function_body(select.obj.as_mut()),
            ExprKind::IntrinsicCall(call) => {
                if self.should_replace_intrinsic_with_value(call.kind, &Value::unit()) {
                    let value = self.eval_intrinsic(call);
                    if !matches!(value, Value::Undefined(_)) {
                        let ty = expr.ty.clone();
                        *expr = Expr::value(value);
                        expr.ty = ty;
                        return;
                    }
                }

                match &mut call.payload {
                    fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                        for arg in template.args.iter_mut() {
                            self.evaluate_function_body(arg);
                        }
                        for kwarg in template.kwargs.iter_mut() {
                            self.evaluate_function_body(&mut kwarg.value);
                        }
                    }
                    fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                        for arg in args.iter_mut() {
                            self.evaluate_function_body(arg);
                        }
                    }
                }
            }
            ExprKind::FormatString(template) => {
                for arg in template.args.iter_mut() {
                    self.evaluate_function_body(arg);
                }
                for kwarg in template.kwargs.iter_mut() {
                    self.evaluate_function_body(&mut kwarg.value);
                }
            }
            ExprKind::Value(_)
            | ExprKind::Locator(_)
            | ExprKind::Any(_)
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
    }

    fn render_intrinsic_call(
        &mut self,
        call: &mut fp_core::ast::ExprIntrinsicCall,
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
            other => Err(optimization_error(format!(
                "unsupported operands for '+': {:?}",
                other
            ))),
        }
    }

    fn binop_sub(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value - r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value - r.value)),
            other => Err(optimization_error(format!(
                "unsupported operands for '-': {:?}",
                other
            ))),
        }
    }

    fn binop_mul(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value * r.value)),
            (Value::Decimal(l), Value::Decimal(r)) => Ok(Value::decimal(l.value * r.value)),
            other => Err(optimization_error(format!(
                "unsupported operands for '*': {:?}",
                other
            ))),
        }
    }

    fn binop_div(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                if r.value == 0 {
                    Err(optimization_error("division by zero".to_string()))
                } else if l.value % r.value == 0 {
                    Ok(Value::int(l.value / r.value))
                } else {
                    Ok(Value::decimal(l.value as f64 / r.value as f64))
                }
            }
            (Value::Decimal(l), Value::Decimal(r)) => {
                if r.value == 0.0 {
                    Err(optimization_error("division by zero".to_string()))
                } else {
                    Ok(Value::decimal(l.value / r.value))
                }
            }
            other => Err(optimization_error(format!(
                "unsupported operands for '/': {:?}",
                other
            ))),
        }
    }

    fn binop_mod(&self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::int(l.value % r.value)),
            other => Err(optimization_error(format!(
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
                return Err(optimization_error(format!(
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
                return Err(optimization_error(format!(
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
            other => Err(optimization_error(format!(
                "bitwise operators require integers, found {:?}",
                other
            ))),
        }
    }

    fn evaluate_unary(&self, op: UnOpKind, value: Value) -> Result<Value> {
        match op {
            UnOpKind::Not => match value {
                Value::Bool(b) => Ok(Value::bool(!b.value)),
                other => Err(optimization_error(format!(
                    "operator '!' requires boolean, found {}",
                    other
                ))),
            },
            UnOpKind::Neg => match value {
                Value::Int(i) => Ok(Value::int(-i.value)),
                Value::Decimal(d) => Ok(Value::decimal(-d.value)),
                other => Err(optimization_error(format!(
                    "operator '-' requires numeric operand, found {}",
                    other
                ))),
            },
            UnOpKind::Deref | UnOpKind::Any(_) => Err(optimization_error(
                "unsupported unary operator in const evaluation".to_string(),
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
        if let Some(scope) = self.value_env.last_mut() {
            scope.insert(name.to_string(), value);
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
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
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
        let diagnostic = Diagnostic::error(message.into()).with_source_context(DIAGNOSTIC_CONTEXT);
        self.push_diagnostic(diagnostic);
    }

    fn emit_warning(&mut self, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::warning(message.into()).with_source_context(DIAGNOSTIC_CONTEXT);
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
