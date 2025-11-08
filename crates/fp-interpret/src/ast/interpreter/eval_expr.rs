use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    pub(crate) fn eval_expr(&mut self, expr: &mut Expr) -> Value {
        match expr.kind_mut() {
            ExprKind::Value(v) => v.get().into(),
            ExprKind::Id(_) => Value::unit(),
            ExprKind::Locator(locator) => {
                if let Some(value) = self.lookup_value(locator) {
                    value
                } else {
                    self.emit_error(format!("unknown identifier {}", locator));
                    Value::undefined()
                }
            }
            ExprKind::Invoke(invoke) => self.eval_invoke(invoke),
            ExprKind::Block(block) => self.eval_block(block),
            ExprKind::ArrayRepeat(repeat) => self.eval_array_repeat(repeat),
            ExprKind::Struct(struct_expr) => self.evaluate_struct_literal(struct_expr),
            ExprKind::Select(sel) => {
                let target = self.eval_expr(sel.obj.as_mut());
                if let Some(ident) = sel.field.as_ident() {
                    self.evaluate_select(target, ident.as_str())
                } else {
                    self.emit_error("unsupported field selection");
                    Value::undefined()
                }
            }
            ExprKind::Cast(cast) => {
                self.evaluate_ty(&mut cast.ty);
                let v = self.eval_expr(cast.expr.as_mut());
                self.cast_value_to_type(v, &cast.ty)
            }
            ExprKind::BinOp(binop) => {
                let lhs = self.eval_expr(binop.lhs.as_mut());
                let rhs = self.eval_expr(binop.rhs.as_mut());
                match self.evaluate_binop(binop.kind.clone(), lhs, rhs) {
                    Ok(v) => v,
                    Err(e) => {
                        self.emit_error(e.to_string());
                        Value::undefined()
                    }
                }
            }
            ExprKind::UnOp(unop) => {
                let v = self.eval_expr(unop.val.as_mut());
                match self.evaluate_unary(unop.op.clone(), v) {
                    Ok(v) => v,
                    Err(e) => {
                        self.emit_error(e.to_string());
                        Value::undefined()
                    }
                }
            }
            _ => Value::unit(),
        }
    }

    pub(crate) fn eval_invoke(&mut self, invoke: &mut ExprInvoke) -> Value {
        match self.mode {
            InterpreterMode::CompileTime => self.eval_invoke_compile_time(invoke),
            InterpreterMode::RunTime => self.eval_invoke_runtime(invoke),
        }
    }

    pub(crate) fn eval_invoke_compile_time(&mut self, invoke: &mut ExprInvoke) -> Value {
        match &mut invoke.target {
            ExprInvokeTarget::Function(locator) => {
                let name = locator.to_string();
                match name.as_str() {
                    _ => {
                        let evaluated = self.evaluate_args(&mut invoke.args);
                        Value::Tuple(ValueTuple::new(evaluated))
                    }
                }
            }
            _ => {
                let evaluated = self.evaluate_args(&mut invoke.args);
                Value::Tuple(ValueTuple::new(evaluated))
            }
        }
    }

    pub(crate) fn eval_invoke_runtime(&mut self, invoke: &mut ExprInvoke) -> Value {
        // Fallback runtime behavior until runtime invocation semantics are modeled
        let evaluated = self.evaluate_args(&mut invoke.args);
        Value::Tuple(ValueTuple::new(evaluated))
    }

    pub(crate) fn evaluate_args(&mut self, args: &mut Vec<Expr>) -> Vec<Value> {
        args.iter_mut().map(|e| self.eval_expr(e)).collect()
    }

    pub(crate) fn evaluate_arg_slice(&mut self, args: &mut [Expr]) -> Vec<Value> {
        args.iter_mut().map(|e| self.eval_expr(e)).collect()
    }

    pub(crate) fn evaluate_binop(&self, op: BinOpKind, lhs: Value, rhs: Value) -> Result<Value> {
        match op {
            BinOpKind::Add => Ok(Value::int(lhs.as_int()? + rhs.as_int()?)),
            BinOpKind::Sub => Ok(Value::int(lhs.as_int()? - rhs.as_int()?)),
            _ => Ok(Value::undefined()),
        }
    }

    pub(crate) fn evaluate_unary(&self, op: UnOpKind, value: Value) -> Result<Value> {
        match op {
            UnOpKind::Neg => Ok(Value::int(-value.as_int()?)),
            _ => Ok(Value::undefined()),
        }
    }
}

