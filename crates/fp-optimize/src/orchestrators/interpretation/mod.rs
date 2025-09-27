use fp_core::ast::{Value, ValueChar};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::Result;
use fp_core::hir::typed as thir;
use std::collections::HashMap;
use std::sync::Arc;

use crate::error::optimization_error;

macro_rules! propagate_flow {
    ($value:expr) => {
        match $value {
            EvalFlow::Value(v) => v,
            EvalFlow::Return(v) => return Ok(EvalFlow::Return(v)),
            EvalFlow::Break(data) => return Ok(EvalFlow::Break(data)),
            EvalFlow::Continue => return Ok(EvalFlow::Continue),
        }
    };
}

#[derive(Debug)]
enum EvalFlow {
    Value(Value),
    Return(Value),
    Break(Option<Value>),
    Continue,
}

impl EvalFlow {
    fn into_value(self) -> Value {
        match self {
            EvalFlow::Value(v) | EvalFlow::Return(v) => v,
            EvalFlow::Break(_) | EvalFlow::Continue => {
                panic!("Control-flow flow (break/continue) escaped evaluation unexpectedly")
            }
        }
    }
}

#[derive(Clone)]
struct IntrinsicRegistry {
    entries: HashMap<String, IntrinsicKind>,
}

#[derive(Clone)]
enum IntrinsicKind {
    Print { newline: bool },
}

impl IntrinsicRegistry {
    fn new() -> Self {
        let mut registry = Self {
            entries: HashMap::new(),
        };
        registry.register_print("println", true);
        registry.register_print("println!", true);
        registry.register_print("std::io::println", true);
        registry.register_print("print", false);
        registry.register_print("print!", false);
        registry.register_print("std::io::print", false);
        registry
    }

    fn register_print(&mut self, name: impl Into<String>, newline: bool) {
        self.entries
            .insert(name.into(), IntrinsicKind::Print { newline });
    }

    fn invoke(
        &self,
        name: &str,
        interpreter: &InterpretationOrchestrator,
        args: &[Value],
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        match self.entries.get(name) {
            Some(IntrinsicKind::Print { newline }) => {
                let value = interpreter.perform_print(*newline, args, ctx)?;
                Ok(Some(value))
            }
            None => Ok(None),
        }
    }
}

/// Typed interpreter mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterMode {
    Const,
    Runtime,
}

#[derive(Clone)]
struct InterpreterConfig {
    mode: InterpreterMode,
    diagnostics: Option<Arc<DiagnosticManager>>,
    abort_on_error: bool,
    intrinsics: IntrinsicRegistry,
}

impl InterpreterConfig {
    fn new(mode: InterpreterMode) -> Self {
        Self {
            mode,
            diagnostics: None,
            abort_on_error: true,
            intrinsics: IntrinsicRegistry::new(),
        }
    }
}

/// Typed interpreter responsible for evaluating THIR bodies.
pub struct InterpretationOrchestrator {
    config: InterpreterConfig,
}

impl InterpretationOrchestrator {
    pub fn new(mode: InterpreterMode) -> Self {
        Self {
            config: InterpreterConfig::new(mode),
        }
    }

    pub fn with_diagnostics(mut self, manager: Arc<DiagnosticManager>) -> Self {
        self.config.diagnostics = Some(manager);
        self
    }

    pub fn set_diagnostics(&mut self, manager: Option<Arc<DiagnosticManager>>) {
        self.config.diagnostics = manager;
    }

    pub fn set_abort_on_error(&mut self, abort: bool) {
        self.config.abort_on_error = abort;
    }

    /// Evaluate a THIR body in the current mode.
    pub fn evaluate_body(
        &mut self,
        body: &thir::Body,
        program: &thir::Program,
        ctx: &SharedScopedContext,
        const_values: &HashMap<thir::ty::DefId, Value>,
    ) -> Result<Value> {
        let mut locals = HashMap::new();
        let flow = self.evaluate_expr(&body.value, program, ctx, &mut locals, const_values)?;
        Ok(flow.into_value())
    }

    fn evaluate_expr(
        &mut self,
        expr: &thir::Expr,
        program: &thir::Program,
        ctx: &SharedScopedContext,
        locals: &mut HashMap<thir::LocalId, Value>,
        const_values: &HashMap<thir::ty::DefId, Value>,
    ) -> Result<EvalFlow> {
        use thir::ExprKind as EK;

        match &expr.kind {
            EK::Literal(lit) => self.evaluate_literal(lit).map(EvalFlow::Value),
            EK::Local(local_id) | EK::VarRef { id: local_id } => locals
                .get(local_id)
                .cloned()
                .map(EvalFlow::Value)
                .ok_or_else(|| optimization_error(format!("Missing local binding {:?}", local_id))),
            EK::Path(item_ref) => {
                if let Some(def_id) = item_ref.def_id {
                    const_values
                        .get(&def_id)
                        .cloned()
                        .map(EvalFlow::Value)
                        .ok_or_else(|| {
                            optimization_error(format!(
                                "Const {:?} referenced before evaluation",
                                item_ref.name
                            ))
                        })
                } else {
                    Err(optimization_error(format!(
                        "Unsupported path without def_id: {}",
                        item_ref.name
                    )))
                }
            }
            EK::Unary(op, value) => {
                let inner = propagate_flow!(self.evaluate_expr(
                    value,
                    program,
                    ctx,
                    locals,
                    const_values
                )?);
                self.evaluate_unary(op.clone(), inner).map(EvalFlow::Value)
            }
            EK::Binary(op, lhs, rhs) => {
                let left =
                    propagate_flow!(self.evaluate_expr(lhs, program, ctx, locals, const_values)?);
                let right =
                    propagate_flow!(self.evaluate_expr(rhs, program, ctx, locals, const_values)?);
                self.evaluate_binary(op.clone(), left, right)
                    .map(EvalFlow::Value)
            }
            EK::LogicalOp { op, lhs, rhs } => {
                let left =
                    propagate_flow!(self.evaluate_expr(lhs, program, ctx, locals, const_values)?);
                let left_bool = self.expect_bool(left)?;
                match op {
                    thir::LogicalOp::And => {
                        if !left_bool {
                            return Ok(EvalFlow::Value(Value::bool(false)));
                        }
                        let right = propagate_flow!(self.evaluate_expr(
                            rhs,
                            program,
                            ctx,
                            locals,
                            const_values
                        )?);
                        self.expect_bool(right)
                            .map(Value::bool)
                            .map(EvalFlow::Value)
                    }
                    thir::LogicalOp::Or => {
                        if left_bool {
                            return Ok(EvalFlow::Value(Value::bool(true)));
                        }
                        let right = propagate_flow!(self.evaluate_expr(
                            rhs,
                            program,
                            ctx,
                            locals,
                            const_values
                        )?);
                        self.expect_bool(right)
                            .map(Value::bool)
                            .map(EvalFlow::Value)
                    }
                }
            }
            EK::Cast(value, _)
            | EK::Deref(value)
            | EK::Borrow { arg: value, .. }
            | EK::AddressOf { arg: value, .. }
            | EK::Use(value) => self.evaluate_expr(value, program, ctx, locals, const_values),
            EK::If {
                cond,
                then,
                else_opt,
            } => {
                let cond_val = propagate_flow!(self.evaluate_expr(
                    cond,
                    program,
                    ctx,
                    locals,
                    const_values
                )?);
                let cond_bool = self.expect_bool(cond_val)?;
                if cond_bool {
                    self.evaluate_expr(then, program, ctx, locals, const_values)
                } else if let Some(else_expr) = else_opt {
                    self.evaluate_expr(else_expr, program, ctx, locals, const_values)
                } else {
                    Ok(EvalFlow::Value(Value::unit()))
                }
            }
            EK::Block(block) => {
                let mut last = Value::unit();
                for stmt in &block.stmts {
                    match self.evaluate_stmt(stmt, program, ctx, locals, const_values)? {
                        EvalFlow::Value(val) => last = val,
                        EvalFlow::Return(val) => return Ok(EvalFlow::Return(val)),
                        EvalFlow::Break(data) => return Ok(EvalFlow::Break(data)),
                        EvalFlow::Continue => return Ok(EvalFlow::Continue),
                    }
                }
                if let Some(expr) = &block.expr {
                    self.evaluate_expr(expr, program, ctx, locals, const_values)
                } else {
                    Ok(EvalFlow::Value(last))
                }
            }
            EK::Call { fun, args, .. } => {
                self.evaluate_call(fun, args, program, ctx, locals, const_values)
            }
            EK::Scope { value, .. } => {
                self.evaluate_expr(value, program, ctx, locals, const_values)
            }
            EK::Let { expr, pat } => {
                let evaluated = self.evaluate_expr(expr, program, ctx, locals, const_values)?;
                let value = propagate_flow!(evaluated);
                let matched = self.match_pattern(pat, value, locals)?;
                Ok(EvalFlow::Value(Value::bool(matched)))
            }
            EK::Match { scrutinee, arms } => {
                let scrutinee_value = propagate_flow!(self.evaluate_expr(
                    scrutinee,
                    program,
                    ctx,
                    locals,
                    const_values
                )?);

                for arm in arms {
                    let mut arm_locals = locals.clone();
                    if !self.match_pattern(
                        &arm.pattern,
                        scrutinee_value.clone(),
                        &mut arm_locals,
                    )? {
                        continue;
                    }

                    if let Some(guard) = &arm.guard {
                        let guard_flow = self.evaluate_expr(
                            &guard.cond,
                            program,
                            ctx,
                            &mut arm_locals,
                            const_values,
                        )?;
                        let guard_value = propagate_flow!(guard_flow);
                        if !self.expect_bool(guard_value)? {
                            continue;
                        }
                    }

                    let result =
                        self.evaluate_expr(&arm.body, program, ctx, &mut arm_locals, const_values)?;
                    *locals = arm_locals;
                    return Ok(result);
                }

                Err(optimization_error(
                    "No match arm matched during const evaluation",
                ))
            }
            EK::Loop { body } => loop {
                match self.evaluate_expr(body, program, ctx, locals, const_values)? {
                    EvalFlow::Value(_) => continue,
                    EvalFlow::Return(val) => return Ok(EvalFlow::Return(val)),
                    EvalFlow::Break(value) => {
                        return Ok(EvalFlow::Value(value.unwrap_or_else(Value::unit)))
                    }
                    EvalFlow::Continue => continue,
                }
            },
            EK::Return { value } => {
                if let Some(val) = value {
                    let evaluated = propagate_flow!(self.evaluate_expr(
                        val,
                        program,
                        ctx,
                        locals,
                        const_values
                    )?);
                    Ok(EvalFlow::Return(evaluated))
                } else {
                    Ok(EvalFlow::Return(Value::unit()))
                }
            }
            EK::Break { value } => {
                let break_value = if let Some(val) = value {
                    Some(propagate_flow!(self.evaluate_expr(
                        val,
                        program,
                        ctx,
                        locals,
                        const_values
                    )?))
                } else {
                    None
                };
                Ok(EvalFlow::Break(break_value))
            }
            EK::Continue => Ok(EvalFlow::Continue),
            EK::Assign { lhs, rhs } => {
                let target = self.resolve_assignment_target(lhs)?;
                let value =
                    propagate_flow!(self.evaluate_expr(rhs, program, ctx, locals, const_values)?);
                locals.insert(target, value);
                Ok(EvalFlow::Value(Value::unit()))
            }
            EK::AssignOp { op, lhs, rhs } => {
                let target = self.resolve_assignment_target(lhs)?;
                let current = locals.get(&target).cloned().ok_or_else(|| {
                    optimization_error(format!(
                        "Assignment target {:?} has no current value",
                        target
                    ))
                })?;
                let rhs_value =
                    propagate_flow!(self.evaluate_expr(rhs, program, ctx, locals, const_values)?);
                let combined = self.evaluate_binary(op.clone(), current, rhs_value)?;
                locals.insert(target, combined);
                Ok(EvalFlow::Value(Value::unit()))
            }
            EK::Index(_, _) | EK::Field { .. } | EK::UpvarRef { .. } => {
                Err(optimization_error(format!(
                    "Unsupported THIR expression in const evaluation: {:?}",
                    expr.kind
                )))
            }
        }
    }

    fn evaluate_stmt(
        &mut self,
        stmt: &thir::Stmt,
        program: &thir::Program,
        ctx: &SharedScopedContext,
        locals: &mut HashMap<thir::LocalId, Value>,
        const_values: &HashMap<thir::ty::DefId, Value>,
    ) -> Result<EvalFlow> {
        match &stmt.kind {
            thir::StmtKind::Expr(expr) => {
                self.evaluate_expr(expr, program, ctx, locals, const_values)
            }
            thir::StmtKind::Let {
                initializer,
                pattern,
                ..
            } => {
                let value = if let Some(init) = initializer {
                    let evaluated = self.evaluate_expr(init, program, ctx, locals, const_values)?;
                    propagate_flow!(evaluated)
                } else {
                    Value::unit()
                };
                self.bind_pattern(pattern, value, locals)?;
                Ok(EvalFlow::Value(Value::unit()))
            }
        }
    }

    fn evaluate_call(
        &mut self,
        fun: &thir::Expr,
        args: &[thir::Expr],
        program: &thir::Program,
        ctx: &SharedScopedContext,
        locals: &mut HashMap<thir::LocalId, Value>,
        const_values: &HashMap<thir::ty::DefId, Value>,
    ) -> Result<EvalFlow> {
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            let value = self.evaluate_expr(arg, program, ctx, locals, const_values)?;
            let value = propagate_flow!(value);
            evaluated_args.push(value);
        }

        let result = match &fun.kind {
            thir::ExprKind::Path(item_ref) => {
                let name = item_ref.name.as_str();
                if let Some(value) = self.invoke_intrinsic(name, &evaluated_args, ctx)? {
                    Ok(value)
                } else if let Some(def_id) = item_ref.def_id {
                    self.evaluate_function_call(def_id, evaluated_args, program, ctx, const_values)
                } else {
                    Err(optimization_error(format!(
                        "Unsupported function call target '{}' in const evaluation",
                        name
                    )))
                }
            }
            other => Err(optimization_error(format!(
                "Unsupported call target expression in const evaluation: {:?}",
                other
            ))),
        }?;

        Ok(EvalFlow::Value(result))
    }

    fn invoke_intrinsic(
        &self,
        name: &str,
        args: &[Value],
        ctx: &SharedScopedContext,
    ) -> Result<Option<Value>> {
        self.config.intrinsics.invoke(name, self, args, ctx)
    }

    fn perform_print(
        &self,
        newline: bool,
        evaluated_args: &[Value],
        ctx: &SharedScopedContext,
    ) -> Result<Value> {
        let (format_string, format_args) = if let Some((first, rest)) = evaluated_args.split_first()
        {
            match first {
                Value::String(s) => (s.value.clone(), rest),
                other => {
                    return Err(optimization_error(format!(
                        "println-style call expects leading string literal, got {:?}",
                        other
                    )))
                }
            }
        } else {
            (String::new(), &[][..])
        };

        let mut rendered = self.render_printf(&format_string, format_args)?;
        if newline {
            rendered.push('\n');
        }
        ctx.print_str(rendered);
        Ok(Value::unit())
    }

    fn render_printf(&self, template: &str, args: &[Value]) -> Result<String> {
        if template.is_empty() {
            if args.is_empty() {
                return Ok(String::new());
            }
            return Err(optimization_error(
                "printf-style call received arguments but no format string",
            ));
        }

        let mut output = String::new();
        let mut chars = template.chars().peekable();
        let mut remaining = args.iter();

        while let Some(ch) = chars.next() {
            if ch != '%' {
                output.push(ch);
                continue;
            }

            match chars.peek() {
                Some('%') => {
                    output.push('%');
                    chars.next();
                    continue;
                }
                _ => {}
            }

            // Consume the rest of the specifier (e.g. lld, f, s)
            let mut spec_consumed = false;
            while let Some(&next) = chars.peek() {
                chars.next();
                if Self::is_printf_spec_end(next) {
                    spec_consumed = true;
                    break;
                }
            }

            if !spec_consumed {
                return Err(optimization_error(
                    "incomplete printf format specifier in const evaluation",
                ));
            }

            let value = remaining.next().ok_or_else(|| {
                optimization_error("missing argument for printf format specifier")
            })?;
            output.push_str(&Self::value_to_display_string(value));
        }

        if remaining.next().is_some() {
            return Err(optimization_error(
                "too many arguments supplied to printf-style call",
            ));
        }

        Ok(output)
    }

    fn value_to_display_string(value: &Value) -> String {
        match value {
            Value::Bool(b) => b.value.to_string(),
            Value::Int(i) => i.value.to_string(),
            Value::Decimal(d) => d.value.to_string(),
            Value::Char(c) => c.value.to_string(),
            Value::String(s) => s.value.clone(),
            other => other.to_string(),
        }
    }

    fn is_printf_spec_end(ch: char) -> bool {
        matches!(
            ch,
            'd' | 'i'
                | 'u'
                | 'f'
                | 'F'
                | 's'
                | 'c'
                | 'x'
                | 'X'
                | 'o'
                | 'p'
                | 'g'
                | 'G'
                | 'e'
                | 'E'
                | 'a'
                | 'A'
        )
    }

    fn evaluate_function_call(
        &mut self,
        def_id: thir::ty::DefId,
        args: Vec<Value>,
        program: &thir::Program,
        ctx: &SharedScopedContext,
        const_values: &HashMap<thir::ty::DefId, Value>,
    ) -> Result<Value> {
        let (function, body_id) = self.lookup_function(program, def_id).ok_or_else(|| {
            optimization_error(format!(
                "Unable to resolve function with def_id {} in const evaluation",
                def_id
            ))
        })?;

        if self.config.mode == InterpreterMode::Const && !function.is_const {
            return Err(optimization_error(format!(
                "Function with def_id {} is not const and cannot be evaluated at compile time",
                def_id
            )));
        }

        let body = program.bodies.get(&body_id).ok_or_else(|| {
            optimization_error(format!(
                "Missing THIR body {:?} for function def_id {}",
                body_id, def_id
            ))
        })?;

        if body.params.len() != args.len() {
            return Err(optimization_error(format!(
                "Function def_id {} expects {} arguments but received {}",
                def_id,
                body.params.len(),
                args.len()
            )));
        }

        let mut locals = HashMap::new();
        for (param, value) in body.params.iter().zip(args.into_iter()) {
            if let Some(pattern) = &param.pat {
                self.bind_pattern(pattern, value, &mut locals)?;
            }
        }

        let flow = self.evaluate_expr(&body.value, program, ctx, &mut locals, const_values)?;
        Ok(flow.into_value())
    }

    fn resolve_assignment_target(&self, expr: &thir::Expr) -> Result<thir::LocalId> {
        match &expr.kind {
            thir::ExprKind::VarRef { id } => Ok(*id),
            thir::ExprKind::Local(id) => Ok(*id),
            other => Err(optimization_error(format!(
                "Assignments to non-local targets are not supported: {:?}",
                other
            ))),
        }
    }

    fn lookup_function<'a>(
        &'a self,
        program: &'a thir::Program,
        def_id: thir::ty::DefId,
    ) -> Option<(&'a thir::Function, thir::BodyId)> {
        program
            .items
            .iter()
            .find_map(|item| match (&item.kind, &item.ty.kind) {
                (thir::ItemKind::Function(function), thir::ty::TyKind::FnDef(found_id, _))
                    if *found_id == def_id && function.body_id.is_some() =>
                {
                    Some((function, function.body_id.unwrap()))
                }
                _ => None,
            })
    }

    fn bind_pattern(
        &mut self,
        pat: &thir::Pat,
        value: Value,
        locals: &mut HashMap<thir::LocalId, Value>,
    ) -> Result<()> {
        match &pat.kind {
            thir::PatKind::Wild => Ok(()),
            thir::PatKind::Binding { var, .. } => {
                locals.insert(*var, value);
                Ok(())
            }
            _ => Err(optimization_error(format!(
                "Unsupported pattern in const evaluation: {:?}",
                pat.kind
            ))),
        }
    }

    fn match_pattern(
        &mut self,
        pat: &thir::Pat,
        value: Value,
        locals: &mut HashMap<thir::LocalId, Value>,
    ) -> Result<bool> {
        match &pat.kind {
            thir::PatKind::Wild => Ok(true),
            thir::PatKind::Binding { .. } => {
                self.bind_pattern(pat, value, locals)?;
                Ok(true)
            }
            _ => Err(optimization_error(format!(
                "Unsupported pattern in let-expression const evaluation: {:?}",
                pat.kind
            ))),
        }
    }

    fn evaluate_literal(&self, lit: &thir::Lit) -> Result<Value> {
        use thir::Lit as L;
        match lit {
            L::Bool(b) => Ok(Value::bool(*b)),
            L::Int(i, _) => Ok(Value::int(*i as i64)),
            L::Uint(u, _) => {
                let value: i64 = (*u).try_into().map_err(|_| {
                    optimization_error(format!("Unsigned literal too large: {}", u))
                })?;
                Ok(Value::int(value))
            }
            L::Float(f, _) => Ok(Value::decimal(*f)),
            L::Str(s) => Ok(Value::string(s.clone())),
            L::Char(c) => Ok(Value::from(ValueChar::new(*c))),
            L::Byte(b) => Ok(Value::int(*b as i64)),
            L::ByteStr(bytes) => {
                let string = String::from_utf8(bytes.clone())
                    .map_err(|_| optimization_error("Byte string literal is not valid UTF-8"))?;
                Ok(Value::string(string))
            }
        }
    }

    fn evaluate_unary(&self, op: thir::UnOp, value: Value) -> Result<Value> {
        use thir::UnOp;
        match op {
            UnOp::Not => self.expect_bool(value).map(|b| Value::bool(!b)),
            UnOp::Neg => {
                if let Value::Int(i) = &value {
                    Ok(Value::int(-i.value))
                } else if let Value::Decimal(d) = &value {
                    Ok(Value::decimal(-d.value))
                } else {
                    Err(optimization_error("Negation expects numeric value"))
                }
            }
        }
    }

    fn evaluate_binary(&self, op: thir::BinOp, left: Value, right: Value) -> Result<Value> {
        use thir::BinOp;
        match op {
            BinOp::Add => self.binary_int_op(left, right, |a, b| a + b),
            BinOp::Sub => self.binary_int_op(left, right, |a, b| a - b),
            BinOp::Mul => self.binary_int_op(left, right, |a, b| a * b),
            BinOp::Div => self.binary_int_op(left, right, |a, b| if b == 0 { a } else { a / b }),
            BinOp::Rem => self.binary_int_op(left, right, |a, b| a % b),
            BinOp::BitAnd => self.binary_int_op(left, right, |a, b| a & b),
            BinOp::BitOr => self.binary_int_op(left, right, |a, b| a | b),
            BinOp::BitXor => self.binary_int_op(left, right, |a, b| a ^ b),
            BinOp::Shl => self.binary_int_op(left, right, |a, b| a << b),
            BinOp::Shr => self.binary_int_op(left, right, |a, b| a >> b),
            BinOp::Eq => Ok(Value::bool(left == right)),
            BinOp::Ne => Ok(Value::bool(left != right)),
            BinOp::Lt => self.binary_bool_compare(left, right, |a, b| a < b),
            BinOp::Le => self.binary_bool_compare(left, right, |a, b| a <= b),
            BinOp::Gt => self.binary_bool_compare(left, right, |a, b| a > b),
            BinOp::Ge => self.binary_bool_compare(left, right, |a, b| a >= b),
        }
    }

    fn binary_int_op<F>(&self, left: Value, right: Value, op: F) -> Result<Value>
    where
        F: FnOnce(i64, i64) -> i64,
    {
        let l = self.expect_int(left)?;
        let r = self.expect_int(right)?;
        Ok(Value::int(op(l, r)))
    }

    fn binary_bool_compare<F>(&self, left: Value, right: Value, op: F) -> Result<Value>
    where
        F: FnOnce(i64, i64) -> bool,
    {
        let l = self.expect_int(left)?;
        let r = self.expect_int(right)?;
        Ok(Value::bool(op(l, r)))
    }

    fn expect_bool(&self, value: Value) -> Result<bool> {
        match value {
            Value::Bool(b) => Ok(b.value),
            _ => Err(optimization_error("Expected boolean value")),
        }
    }

    fn expect_int(&self, value: Value) -> Result<i64> {
        match value {
            Value::Int(i) => Ok(i.value),
            _ => Err(optimization_error("Expected integer value")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::context::SharedScopedContext;
    use fp_core::hir::typed as thir;
    use fp_core::hir::typed::ty::{
        IntTy as TyInt, Mutability as TyMutability, Ty as TyTy, TyKind as TyTyKind, TypeAndMut,
    };
    use fp_core::span::Span;
    use std::collections::HashMap;

    fn dummy_span() -> Span {
        Span::new(0, 0, 0)
    }

    fn unit_ty() -> TyTy {
        TyTy {
            kind: TyTyKind::Tuple(Vec::new()),
        }
    }

    fn string_ptr_ty() -> TyTy {
        TyTy {
            kind: TyTyKind::RawPtr(TypeAndMut {
                ty: Box::new(TyTy {
                    kind: TyTyKind::Int(TyInt::I8),
                }),
                mutbl: TyMutability::Not,
            }),
        }
    }

    fn i32_ty() -> TyTy {
        TyTy {
            kind: TyTyKind::Int(TyInt::I32),
        }
    }

    fn fn_def_ty(def_id: thir::ty::DefId) -> TyTy {
        TyTy {
            kind: TyTyKind::FnDef(def_id, Vec::new()),
        }
    }

    fn println_path_expr(thir_id: thir::ThirId) -> thir::Expr {
        thir::Expr {
            thir_id,
            kind: thir::ExprKind::Path(thir::ItemRef {
                name: "std::io::println".to_string(),
                def_id: None,
            }),
            ty: unit_ty(),
            span: dummy_span(),
        }
    }

    fn string_literal_expr(thir_id: thir::ThirId, value: &str) -> thir::Expr {
        thir::Expr {
            thir_id,
            kind: thir::ExprKind::Literal(thir::Lit::Str(value.to_string())),
            ty: string_ptr_ty(),
            span: dummy_span(),
        }
    }

    fn int_literal_expr(thir_id: thir::ThirId, value: i128) -> thir::Expr {
        thir::Expr {
            thir_id,
            kind: thir::ExprKind::Literal(thir::Lit::Int(value, thir::IntTy::I32)),
            ty: i32_ty(),
            span: dummy_span(),
        }
    }

    fn local_expr(thir_id: thir::ThirId, local_id: thir::LocalId, ty: TyTy) -> thir::Expr {
        thir::Expr {
            thir_id,
            kind: thir::ExprKind::VarRef { id: local_id },
            ty,
            span: dummy_span(),
        }
    }

    fn program_with_body(call_expr: thir::Expr) -> (thir::Program, thir::BodyId) {
        let body_id = thir::BodyId(0);
        let body = thir::Body {
            params: Vec::new(),
            value: call_expr,
            locals: Vec::new(),
        };
        let mut bodies = HashMap::new();
        bodies.insert(body_id, body);

        (
            thir::Program {
                items: Vec::new(),
                bodies,
                next_thir_id: 10,
            },
            body_id,
        )
    }

    #[test]
    fn println_literal_argument_emits_line() {
        let call_expr = thir::Expr {
            thir_id: 3,
            kind: thir::ExprKind::Call {
                fun: Box::new(println_path_expr(1)),
                args: vec![string_literal_expr(2, "hello")],
                from_hir_call: true,
            },
            ty: unit_ty(),
            span: dummy_span(),
        };

        let (program, body_id) = program_with_body(call_expr);
        let ctx = SharedScopedContext::new();
        let const_values = HashMap::new();
        let mut interpreter = InterpretationOrchestrator::new(InterpreterMode::Const);

        let body = program.bodies.get(&body_id).expect("body present");
        let result = interpreter
            .evaluate_body(body, &program, &ctx, &const_values)
            .expect("evaluation succeeds");

        assert!(matches!(result, Value::Unit(_)));
        assert_eq!(ctx.take_outputs(), vec!["hello\n".to_string()]);
    }

    #[test]
    fn println_formats_basic_integer_specifier() {
        let call_expr = thir::Expr {
            thir_id: 5,
            kind: thir::ExprKind::Call {
                fun: Box::new(println_path_expr(6)),
                args: vec![string_literal_expr(7, "value: %d"), int_literal_expr(8, 42)],
                from_hir_call: true,
            },
            ty: unit_ty(),
            span: dummy_span(),
        };

        let (program, body_id) = program_with_body(call_expr);
        let ctx = SharedScopedContext::new();
        let const_values = HashMap::new();
        let mut interpreter = InterpretationOrchestrator::new(InterpreterMode::Const);

        let body = program.bodies.get(&body_id).expect("body present");
        interpreter
            .evaluate_body(body, &program, &ctx, &const_values)
            .expect("evaluation succeeds");

        assert_eq!(ctx.take_outputs(), vec!["value: 42\n".to_string()]);
    }

    #[test]
    fn const_function_call_returns_value() {
        use thir::BindingMode;
        use thir::Mutability;
        use thir::PatKind;

        let def_id: thir::ty::DefId = 42;

        let param_ty = i32_ty();
        let param_pattern = thir::Pat {
            thir_id: 20,
            kind: PatKind::Binding {
                mutability: Mutability::Not,
                name: "value".to_string(),
                mode: BindingMode::ByValue,
                var: 0,
                ty: param_ty.clone(),
            },
            ty: param_ty.clone(),
            span: dummy_span(),
        };

        let function_body = thir::Body {
            params: vec![thir::Param {
                ty: param_ty.clone(),
                pat: Some(param_pattern),
            }],
            value: thir::Expr {
                thir_id: 25,
                kind: thir::ExprKind::Binary(
                    thir::BinOp::Add,
                    Box::new(local_expr(21, 0, param_ty.clone())),
                    Box::new(int_literal_expr(22, 5)),
                ),
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            locals: Vec::new(),
        };

        let mut bodies = HashMap::new();
        let function_body_id = thir::BodyId(1);
        bodies.insert(function_body_id, function_body);

        let function_item = thir::Item {
            thir_id: 30,
            kind: thir::ItemKind::Function(thir::Function {
                sig: thir::FunctionSig {
                    inputs: vec![param_ty.clone()],
                    output: param_ty.clone(),
                    c_variadic: false,
                },
                body_id: Some(function_body_id),
                is_const: true,
            }),
            ty: fn_def_ty(def_id),
            span: dummy_span(),
        };

        let main_body_id = thir::BodyId(0);
        let main_body = thir::Body {
            params: Vec::new(),
            value: thir::Expr {
                thir_id: 40,
                kind: thir::ExprKind::Call {
                    fun: Box::new(thir::Expr {
                        thir_id: 41,
                        kind: thir::ExprKind::Path(thir::ItemRef {
                            name: "const_add".to_string(),
                            def_id: Some(def_id),
                        }),
                        ty: fn_def_ty(def_id),
                        span: dummy_span(),
                    }),
                    args: vec![int_literal_expr(42, 7)],
                    from_hir_call: true,
                },
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            locals: Vec::new(),
        };

        bodies.insert(main_body_id, main_body);

        let program = thir::Program {
            items: vec![function_item],
            bodies,
            next_thir_id: 100,
        };

        let ctx = SharedScopedContext::new();
        let const_values = HashMap::new();
        let mut interpreter = InterpretationOrchestrator::new(InterpreterMode::Const);

        let body = program
            .bodies
            .get(&main_body_id)
            .expect("main body present");
        let result = interpreter
            .evaluate_body(body, &program, &ctx, &const_values)
            .expect("evaluation succeeds");

        match result {
            Value::Int(i) => assert_eq!(i.value, 12),
            other => panic!("expected integer result, got {:?}", other),
        }
    }

    #[test]
    fn const_function_return_statement_short_circuits() {
        use thir::BindingMode;
        use thir::BlockSafetyMode;
        use thir::Mutability;
        use thir::PatKind;

        let def_id: thir::ty::DefId = 1337;

        let param_ty = i32_ty();
        let param_pattern = thir::Pat {
            thir_id: 200,
            kind: PatKind::Binding {
                mutability: Mutability::Not,
                name: "value".to_string(),
                mode: BindingMode::ByValue,
                var: 0,
                ty: param_ty.clone(),
            },
            ty: param_ty.clone(),
            span: dummy_span(),
        };

        let return_stmt = thir::Stmt {
            kind: thir::StmtKind::Expr(thir::Expr {
                thir_id: 210,
                kind: thir::ExprKind::Return {
                    value: Some(Box::new(local_expr(211, 0, param_ty.clone()))),
                },
                ty: unit_ty(),
                span: dummy_span(),
            }),
        };

        let function_block = thir::Block {
            targeted_by_break: false,
            region: 0,
            span: dummy_span(),
            stmts: vec![return_stmt],
            expr: Some(Box::new(int_literal_expr(212, 999))),
            safety_mode: BlockSafetyMode::Safe,
        };

        let function_body = thir::Body {
            params: vec![thir::Param {
                ty: param_ty.clone(),
                pat: Some(param_pattern),
            }],
            value: thir::Expr {
                thir_id: 215,
                kind: thir::ExprKind::Block(function_block),
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            locals: Vec::new(),
        };

        let mut bodies = HashMap::new();
        let function_body_id = thir::BodyId(2);
        bodies.insert(function_body_id, function_body);

        let function_item = thir::Item {
            thir_id: 220,
            kind: thir::ItemKind::Function(thir::Function {
                sig: thir::FunctionSig {
                    inputs: vec![param_ty.clone()],
                    output: param_ty.clone(),
                    c_variadic: false,
                },
                body_id: Some(function_body_id),
                is_const: true,
            }),
            ty: fn_def_ty(def_id),
            span: dummy_span(),
        };

        let main_body_id = thir::BodyId(0);
        let main_body = thir::Body {
            params: Vec::new(),
            value: thir::Expr {
                thir_id: 230,
                kind: thir::ExprKind::Call {
                    fun: Box::new(thir::Expr {
                        thir_id: 231,
                        kind: thir::ExprKind::Path(thir::ItemRef {
                            name: "const_return".to_string(),
                            def_id: Some(def_id),
                        }),
                        ty: fn_def_ty(def_id),
                        span: dummy_span(),
                    }),
                    args: vec![int_literal_expr(232, 7)],
                    from_hir_call: true,
                },
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            locals: Vec::new(),
        };

        bodies.insert(main_body_id, main_body);

        let program = thir::Program {
            items: vec![function_item],
            bodies,
            next_thir_id: 300,
        };

        let ctx = SharedScopedContext::new();
        let const_values = HashMap::new();
        let mut interpreter = InterpretationOrchestrator::new(InterpreterMode::Const);

        let body = program
            .bodies
            .get(&main_body_id)
            .expect("main body present");
        let result = interpreter
            .evaluate_body(body, &program, &ctx, &const_values)
            .expect("evaluation succeeds");

        match result {
            Value::Int(i) => assert_eq!(i.value, 7),
            other => panic!("expected integer result from return, got {:?}", other),
        }
    }

    #[test]
    fn match_expression_binds_and_evaluates_arm() {
        use thir::BindingMode;
        use thir::Mutability;
        use thir::PatKind;

        let def_id: thir::ty::DefId = 2048;

        let param_ty = i32_ty();
        let param_pattern = thir::Pat {
            thir_id: 400,
            kind: PatKind::Binding {
                mutability: Mutability::Not,
                name: "value".to_string(),
                mode: BindingMode::ByValue,
                var: 0,
                ty: param_ty.clone(),
            },
            ty: param_ty.clone(),
            span: dummy_span(),
        };

        let match_arm = thir::Arm {
            pattern: param_pattern.clone(),
            guard: None,
            body: thir::Expr {
                thir_id: 420,
                kind: thir::ExprKind::Binary(
                    thir::BinOp::Add,
                    Box::new(local_expr(421, 0, param_ty.clone())),
                    Box::new(int_literal_expr(422, 1)),
                ),
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            lint_level: 0,
            scope: 0,
            span: dummy_span(),
        };

        let match_expr = thir::Expr {
            thir_id: 430,
            kind: thir::ExprKind::Match {
                scrutinee: Box::new(local_expr(431, 0, param_ty.clone())),
                arms: vec![match_arm],
            },
            ty: param_ty.clone(),
            span: dummy_span(),
        };

        let function_body = thir::Body {
            params: vec![thir::Param {
                ty: param_ty.clone(),
                pat: Some(param_pattern),
            }],
            value: match_expr,
            locals: Vec::new(),
        };

        let mut bodies = HashMap::new();
        let function_body_id = thir::BodyId(3);
        bodies.insert(function_body_id, function_body);

        let function_item = thir::Item {
            thir_id: 440,
            kind: thir::ItemKind::Function(thir::Function {
                sig: thir::FunctionSig {
                    inputs: vec![param_ty.clone()],
                    output: param_ty.clone(),
                    c_variadic: false,
                },
                body_id: Some(function_body_id),
                is_const: true,
            }),
            ty: fn_def_ty(def_id),
            span: dummy_span(),
        };

        let main_body_id = thir::BodyId(0);
        let main_body = thir::Body {
            params: Vec::new(),
            value: thir::Expr {
                thir_id: 450,
                kind: thir::ExprKind::Call {
                    fun: Box::new(thir::Expr {
                        thir_id: 451,
                        kind: thir::ExprKind::Path(thir::ItemRef {
                            name: "match_increment".to_string(),
                            def_id: Some(def_id),
                        }),
                        ty: fn_def_ty(def_id),
                        span: dummy_span(),
                    }),
                    args: vec![int_literal_expr(452, 10)],
                    from_hir_call: true,
                },
                ty: param_ty.clone(),
                span: dummy_span(),
            },
            locals: Vec::new(),
        };

        bodies.insert(main_body_id, main_body);

        let program = thir::Program {
            items: vec![function_item],
            bodies,
            next_thir_id: 500,
        };

        let ctx = SharedScopedContext::new();
        let const_values = HashMap::new();
        let mut interpreter = InterpretationOrchestrator::new(InterpreterMode::Const);

        let body = program
            .bodies
            .get(&main_body_id)
            .expect("main body present");
        let result = interpreter
            .evaluate_body(body, &program, &ctx, &const_values)
            .expect("evaluation succeeds");

        match result {
            Value::Int(i) => assert_eq!(i.value, 11),
            other => panic!("expected integer result from match arm, got {:?}", other),
        }
    }
}
