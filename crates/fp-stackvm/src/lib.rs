use fp_bytecode::{
    BytecodeBinOp, BytecodeCallee, BytecodeConst, BytecodeInstr, BytecodePlace, BytecodePlaceElem,
    BytecodeProgram, BytecodeTerminator, BytecodeUnOp, IntrinsicCallKind,
};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Str(String),
    Null,
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    List(Vec<Value>),
    Map(Vec<(Value, Value)>),
}

#[derive(Debug, Error)]
pub enum VmError {
    #[error("vm error: {message}")]
    Runtime { message: String },
    #[error("unsupported bytecode: {message}")]
    Unsupported { message: String },
}

pub struct Vm {
    program: BytecodeProgram,
}

impl Vm {
    pub fn new(program: BytecodeProgram) -> Self {
        Self { program }
    }

    pub fn run_main(&self) -> Result<Value, VmError> {
        let entry = self.program.entry.clone().ok_or_else(|| VmError::Runtime {
            message: "no entry function found".to_string(),
        })?;
        self.run_function(&entry, Vec::new())
    }

    pub fn run_function(&self, name: &str, args: Vec<Value>) -> Result<Value, VmError> {
        let function = self
            .program
            .functions
            .iter()
            .find(|f| f.name == name)
            .ok_or_else(|| VmError::Runtime {
                message: format!("missing function {}", name),
            })?;

        if args.len() != function.params as usize {
            return Err(VmError::Runtime {
                message: format!(
                    "function {} expects {} args but got {}",
                    function.name,
                    function.params,
                    args.len()
                ),
            });
        }
        let required_locals = function.params.saturating_add(1);
        if function.locals < required_locals {
            return Err(VmError::Runtime {
                message: format!(
                    "function {} expects {} params but only {} locals",
                    function.name, function.params, function.locals
                ),
            });
        }

        let mut locals = vec![Value::Unit; function.locals as usize];
        for (index, value) in args.into_iter().enumerate() {
            let slot = 1 + index;
            locals[slot] = value;
        }

        let mut stack: Vec<Value> = Vec::new();
        let mut current_block = 0u32;
        loop {
            let block = function
                .blocks
                .iter()
                .find(|b| b.id == current_block)
                .ok_or_else(|| VmError::Runtime {
                    message: format!("missing block {}", current_block),
                })?;

            for instr in &block.code {
                execute_instr(instr, &mut locals, &mut stack, &self.program.const_pool)?;
            }

            match &block.terminator {
                BytecodeTerminator::Return => {
                    return Ok(locals.get(0).cloned().unwrap_or(Value::Unit));
                }
                BytecodeTerminator::Jump { target } => {
                    current_block = *target;
                }
                BytecodeTerminator::JumpIfTrue { target, otherwise } => {
                    let cond = pop_bool(&mut stack)?;
                    if cond {
                        current_block = *target;
                    } else {
                        current_block = *otherwise;
                    }
                }
                BytecodeTerminator::JumpIfFalse { target, otherwise } => {
                    let cond = pop_bool(&mut stack)?;
                    if !cond {
                        current_block = *target;
                    } else {
                        current_block = *otherwise;
                    }
                }
                BytecodeTerminator::SwitchInt {
                    values,
                    targets,
                    otherwise,
                } => {
                    let discr = pop_int(&mut stack)?;
                    let mut matched = false;
                    for (index, value) in values.iter().enumerate() {
                        if *value as i128 == discr as i128 {
                            current_block = targets[index];
                            matched = true;
                            break;
                        }
                    }
                    if !matched {
                        current_block = *otherwise;
                    }
                }
                BytecodeTerminator::Call {
                    callee,
                    arg_count,
                    destination,
                    target,
                } => {
                    let mut args = Vec::with_capacity(*arg_count as usize);
                    for _ in 0..*arg_count {
                        args.push(stack.pop().ok_or_else(|| VmError::Runtime {
                            message: "stack underflow in call".to_string(),
                        })?);
                    }
                    args.reverse();
                    let result = match callee {
                        BytecodeCallee::Function(name) => self.run_function(name, args)?,
                        BytecodeCallee::Local(place) => {
                            let value = load_place(&locals, place)?;
                            match value {
                                Value::Str(name) => self.run_function(&name, args)?,
                                _ => {
                                    return Err(VmError::Runtime {
                                        message: "callee value is not a function".to_string(),
                                    });
                                }
                            }
                        }
                    };
                    if let Some(place) = destination {
                        store_place(&mut locals, place, result)?;
                    }
                    current_block = *target;
                }
                BytecodeTerminator::Abort => {
                    return Err(VmError::Runtime {
                        message: "abort".to_string(),
                    });
                }
                BytecodeTerminator::Unreachable => {
                    return Err(VmError::Runtime {
                        message: "unreachable".to_string(),
                    });
                }
            }
        }
    }
}

fn execute_instr(
    instr: &BytecodeInstr,
    locals: &mut [Value],
    stack: &mut Vec<Value>,
    const_pool: &[BytecodeConst],
) -> Result<(), VmError> {
    match instr {
        BytecodeInstr::LoadConst(id) => {
            let value = const_pool
                .get(*id as usize)
                .ok_or_else(|| VmError::Runtime {
                    message: format!("missing const {}", id),
                })?;
            stack.push(convert_const(value)?);
            Ok(())
        }
        BytecodeInstr::LoadLocal(local) => {
            let value = locals
                .get(*local as usize)
                .cloned()
                .ok_or_else(|| VmError::Runtime {
                    message: format!("missing local {}", local),
                })?;
            stack.push(value);
            Ok(())
        }
        BytecodeInstr::StoreLocal(local) => {
            let value = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow on store".to_string(),
            })?;
            let slot = locals
                .get_mut(*local as usize)
                .ok_or_else(|| VmError::Runtime {
                    message: format!("missing local {}", local),
                })?;
            *slot = value;
            Ok(())
        }
        BytecodeInstr::LoadPlace(place) => {
            let value = load_place(locals, place)?;
            stack.push(value);
            Ok(())
        }
        BytecodeInstr::StorePlace(place) => {
            let value = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow on store".to_string(),
            })?;
            store_place(locals, place, value)
        }
        BytecodeInstr::BinaryOp(op) => {
            let right = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow on binary op".to_string(),
            })?;
            let left = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow on binary op".to_string(),
            })?;
            let value = eval_binop(op, left, right)?;
            stack.push(value);
            Ok(())
        }
        BytecodeInstr::UnaryOp(op) => {
            let value = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow on unary op".to_string(),
            })?;
            let result = eval_unop(op, value)?;
            stack.push(result);
            Ok(())
        }
        BytecodeInstr::IntrinsicCall {
            kind,
            arg_count,
            format,
        } => {
            let mut args = Vec::with_capacity(*arg_count as usize);
            for _ in 0..*arg_count {
                args.push(stack.pop().ok_or_else(|| VmError::Runtime {
                    message: "stack underflow in intrinsic call".to_string(),
                })?);
            }
            args.reverse();
            let result = exec_intrinsic(*kind, format.as_deref(), args)?;
            if let Some(value) = result {
                stack.push(value);
            }
            Ok(())
        }
        BytecodeInstr::MakeTuple(count) => {
            let values = pop_n(stack, *count)?;
            stack.push(Value::Tuple(values));
            Ok(())
        }
        BytecodeInstr::MakeArray(count) => {
            let values = pop_n(stack, *count)?;
            stack.push(Value::Array(values));
            Ok(())
        }
        BytecodeInstr::MakeList(count) => {
            let values = pop_n(stack, *count)?;
            stack.push(Value::List(values));
            Ok(())
        }
        BytecodeInstr::MakeMap(count) => {
            let mut entries = Vec::with_capacity(*count as usize);
            for _ in 0..*count {
                let value = stack.pop().ok_or_else(|| VmError::Runtime {
                    message: "stack underflow in map literal".to_string(),
                })?;
                let key = stack.pop().ok_or_else(|| VmError::Runtime {
                    message: "stack underflow in map literal".to_string(),
                })?;
                entries.push((key, value));
            }
            entries.reverse();
            stack.push(Value::Map(entries));
            Ok(())
        }
        BytecodeInstr::ContainerLen => {
            let value = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow in container len".to_string(),
            })?;
            let len = match value {
                Value::Str(text) => text.len() as i64,
                Value::Array(items) => items.len() as i64,
                Value::List(items) => items.len() as i64,
                Value::Tuple(items) => items.len() as i64,
                Value::Map(items) => items.len() as i64,
                _ => {
                    return Err(VmError::Unsupported {
                        message: "len unsupported for value".to_string(),
                    });
                }
            };
            stack.push(Value::Int(len));
            Ok(())
        }
        BytecodeInstr::ContainerGet => {
            let key = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow in container get".to_string(),
            })?;
            let container = stack.pop().ok_or_else(|| VmError::Runtime {
                message: "stack underflow in container get".to_string(),
            })?;
            let value = match (container, key) {
                (Value::Array(items), Value::Int(index)) => items
                    .get(index as usize)
                    .cloned()
                    .ok_or_else(|| VmError::Runtime {
                        message: "index out of bounds".to_string(),
                    })?,
                (Value::List(items), Value::Int(index)) => items
                    .get(index as usize)
                    .cloned()
                    .ok_or_else(|| VmError::Runtime {
                        message: "index out of bounds".to_string(),
                    })?,
                (Value::Map(items), key) => {
                    let mut found = None;
                    for (k, v) in items {
                        if values_equal(&k, &key) {
                            found = Some(v);
                            break;
                        }
                    }
                    found.ok_or_else(|| VmError::Runtime {
                        message: "key not found".to_string(),
                    })?
                }
                _ => {
                    return Err(VmError::Unsupported {
                        message: "container get unsupported for value".to_string(),
                    });
                }
            };
            stack.push(value);
            Ok(())
        }
        BytecodeInstr::Pop => {
            let _ = stack.pop();
            Ok(())
        }
    }
}

fn eval_binop(op: &BytecodeBinOp, left: Value, right: Value) -> Result<Value, VmError> {
    match (op, left, right) {
        (BytecodeBinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
        (BytecodeBinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
        (BytecodeBinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
        (BytecodeBinOp::Div, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
        (BytecodeBinOp::Rem, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
        (BytecodeBinOp::Add, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a + b)),
        (BytecodeBinOp::Sub, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a - b)),
        (BytecodeBinOp::Mul, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a * b)),
        (BytecodeBinOp::Div, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a / b)),
        (BytecodeBinOp::Rem, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a % b)),
        (BytecodeBinOp::Add, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
        (BytecodeBinOp::Sub, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
        (BytecodeBinOp::Mul, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
        (BytecodeBinOp::Div, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
        (BytecodeBinOp::Rem, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
        (BytecodeBinOp::Eq, a, b) => Ok(Value::Bool(values_equal(&a, &b))),
        (BytecodeBinOp::Ne, a, b) => Ok(Value::Bool(!values_equal(&a, &b))),
        (BytecodeBinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
        (BytecodeBinOp::Le, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
        (BytecodeBinOp::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
        (BytecodeBinOp::Ge, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
        (BytecodeBinOp::Lt, Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a < b)),
        (BytecodeBinOp::Le, Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a <= b)),
        (BytecodeBinOp::Gt, Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a > b)),
        (BytecodeBinOp::Ge, Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a >= b)),
        (BytecodeBinOp::Lt, Value::Int(a), Value::UInt(b)) => {
            Ok(Value::Bool(if a < 0 { true } else { (a as u64) < b }))
        }
        (BytecodeBinOp::Le, Value::Int(a), Value::UInt(b)) => {
            Ok(Value::Bool(if a < 0 { true } else { (a as u64) <= b }))
        }
        (BytecodeBinOp::Gt, Value::Int(a), Value::UInt(b)) => {
            Ok(Value::Bool(if a < 0 { false } else { (a as u64) > b }))
        }
        (BytecodeBinOp::Ge, Value::Int(a), Value::UInt(b)) => {
            Ok(Value::Bool(if a < 0 { false } else { (a as u64) >= b }))
        }
        (BytecodeBinOp::Lt, Value::UInt(a), Value::Int(b)) => {
            Ok(Value::Bool(if b < 0 { false } else { a < b as u64 }))
        }
        (BytecodeBinOp::Le, Value::UInt(a), Value::Int(b)) => {
            Ok(Value::Bool(if b < 0 { false } else { a <= b as u64 }))
        }
        (BytecodeBinOp::Gt, Value::UInt(a), Value::Int(b)) => {
            Ok(Value::Bool(if b < 0 { true } else { a > b as u64 }))
        }
        (BytecodeBinOp::Ge, Value::UInt(a), Value::Int(b)) => {
            Ok(Value::Bool(if b < 0 { true } else { a >= b as u64 }))
        }
        (BytecodeBinOp::Lt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
        (BytecodeBinOp::Le, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
        (BytecodeBinOp::Gt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
        (BytecodeBinOp::Ge, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
        (BytecodeBinOp::BitAnd, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
        (BytecodeBinOp::BitOr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
        (BytecodeBinOp::BitXor, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
        (BytecodeBinOp::Shl, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
        (BytecodeBinOp::Shr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
        (BytecodeBinOp::BitAnd, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a & b)),
        (BytecodeBinOp::BitOr, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a | b)),
        (BytecodeBinOp::BitXor, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a ^ b)),
        (BytecodeBinOp::Shl, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a << b)),
        (BytecodeBinOp::Shr, Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a >> b)),
        (BytecodeBinOp::And, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
        (BytecodeBinOp::Or, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
        _ => Err(VmError::Unsupported {
            message: format!("unsupported binary op {:?} for operands", op),
        }),
    }
}

fn eval_unop(op: &BytecodeUnOp, value: Value) -> Result<Value, VmError> {
    match (op, value) {
        (BytecodeUnOp::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
        (BytecodeUnOp::Neg, Value::Int(value)) => Ok(Value::Int(-value)),
        _ => Err(VmError::Unsupported {
            message: format!("unsupported unary op {:?}", op),
        }),
    }
}

fn exec_intrinsic(
    kind: IntrinsicCallKind,
    format: Option<&str>,
    args: Vec<Value>,
) -> Result<Option<Value>, VmError> {
    match kind {
        IntrinsicCallKind::Println => {
            let rendered = render_args(format, &args);
            if format.is_some() {
                print!("{}", rendered);
            } else {
                println!("{}", rendered);
            }
            Ok(None)
        }
        IntrinsicCallKind::Print => {
            let rendered = render_args(format, &args);
            print!("{}", rendered);
            Ok(None)
        }
        IntrinsicCallKind::Format => Ok(Some(Value::Str(render_args(format, &args)))),
        IntrinsicCallKind::Len => {
            if args.len() != 1 {
                return Err(VmError::Runtime {
                    message: "len expects 1 argument".to_string(),
                });
            }
            let len = match &args[0] {
                Value::Str(value) => value.len() as i64,
                Value::Array(items) => items.len() as i64,
                Value::List(items) => items.len() as i64,
                Value::Tuple(items) => items.len() as i64,
                Value::Map(items) => items.len() as i64,
                _ => {
                    return Err(VmError::Unsupported {
                        message: "len unsupported for value".to_string(),
                    });
                }
            };
            Ok(Some(Value::Int(len)))
        }
        _ => Err(VmError::Unsupported {
            message: format!("unsupported intrinsic {:?}", kind),
        }),
    }
}

fn render_args(format: Option<&str>, args: &[Value]) -> String {
    if let Some(template) = format {
        if template.contains("{}") {
            return render_brace_format(template, args);
        }
        if template.contains('%') {
            return render_printf_format(template, args);
        }
        if args.is_empty() {
            return template.to_string();
        }
        let tail = args.iter().map(render_value).collect::<Vec<_>>().join(" ");
        return format!("{} {}", template, tail);
    }
    args.iter().map(render_value).collect::<Vec<_>>().join(" ")
}

fn render_brace_format(template: &str, args: &[Value]) -> String {
    let mut result = template.to_string();
    for value in args {
        if let Some(pos) = result.find("{}") {
            result.replace_range(pos..pos + 2, &render_value(value));
        }
    }
    result
}

fn render_printf_format(template: &str, args: &[Value]) -> String {
    let mut result = String::new();
    let mut chars = template.chars().peekable();
    let mut arg_index = 0usize;
    let specifiers = "diuoxXfFeEgGaAcspn";

    while let Some(ch) = chars.next() {
        if ch != '%' {
            result.push(ch);
            continue;
        }
        if let Some('%') = chars.peek().copied() {
            chars.next();
            result.push('%');
            continue;
        }
        while let Some(next) = chars.peek().copied() {
            chars.next();
            if specifiers.contains(next) {
                break;
            }
        }
        if let Some(value) = args.get(arg_index) {
            result.push_str(&render_value(value));
            arg_index += 1;
        }
    }

    result
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Unit => "()".to_string(),
        Value::Bool(value) => value.to_string(),
        Value::Int(value) => value.to_string(),
        Value::UInt(value) => value.to_string(),
        Value::Float(value) => value.to_string(),
        Value::Str(value) => value.clone(),
        Value::Null => "null".to_string(),
        Value::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(render_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Array(items) | Value::List(items) => format!(
            "[{}]",
            items
                .iter()
                .map(render_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Map(items) => {
            let rendered = items
                .iter()
                .map(|(k, v)| format!("{}: {}", render_value(k), render_value(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", rendered)
        }
    }
}

fn pop_n(stack: &mut Vec<Value>, count: u32) -> Result<Vec<Value>, VmError> {
    let mut values = Vec::with_capacity(count as usize);
    for _ in 0..count {
        values.push(stack.pop().ok_or_else(|| VmError::Runtime {
            message: "stack underflow".to_string(),
        })?);
    }
    values.reverse();
    Ok(values)
}

fn pop_bool(stack: &mut Vec<Value>) -> Result<bool, VmError> {
    match stack.pop() {
        Some(Value::Bool(value)) => Ok(value),
        _ => Err(VmError::Runtime {
            message: "expected bool on stack".to_string(),
        }),
    }
}

fn pop_int(stack: &mut Vec<Value>) -> Result<i64, VmError> {
    match stack.pop() {
        Some(Value::Int(value)) => Ok(value),
        Some(Value::UInt(value)) => Ok(value as i64),
        Some(Value::Bool(value)) => Ok(i64::from(value)),
        _ => Err(VmError::Runtime {
            message: "expected int on stack".to_string(),
        }),
    }
}

fn load_place(locals: &[Value], place: &BytecodePlace) -> Result<Value, VmError> {
    let mut value = locals
        .get(place.local as usize)
        .cloned()
        .ok_or_else(|| VmError::Runtime {
            message: format!("missing local {}", place.local),
        })?;

    for elem in &place.projection {
        match elem {
            BytecodePlaceElem::Field(index) => {
                value = match value {
                    Value::Tuple(items) | Value::Array(items) | Value::List(items) => items
                        .get(*index as usize)
                        .cloned()
                        .ok_or_else(|| VmError::Runtime {
                            message: "field index out of bounds".to_string(),
                        })?,
                    Value::Str(text) => {
                        let idx = *index as usize;
                        let ch = text.chars().nth(idx).ok_or_else(|| VmError::Runtime {
                            message: "string index out of bounds".to_string(),
                        })?;
                        Value::Str(ch.to_string())
                    }
                    _ => {
                        return Err(VmError::Unsupported {
                            message: "field access unsupported for value".to_string(),
                        });
                    }
                };
            }
            BytecodePlaceElem::Index(local) => {
                let idx = match locals.get(*local as usize) {
                    Some(Value::Int(value)) => *value as usize,
                    Some(Value::UInt(value)) => *value as usize,
                    _ => {
                        return Err(VmError::Runtime {
                            message: "index local is not int".to_string(),
                        });
                    }
                };
                value = match value {
                    Value::Array(items) | Value::List(items) => {
                        items.get(idx).cloned().ok_or_else(|| VmError::Runtime {
                            message: "index out of bounds".to_string(),
                        })?
                    }
                    _ => {
                        return Err(VmError::Unsupported {
                            message: "index access unsupported for value".to_string(),
                        });
                    }
                };
            }
        }
    }

    Ok(value)
}

fn store_place(locals: &mut [Value], place: &BytecodePlace, value: Value) -> Result<(), VmError> {
    if place.projection.is_empty() {
        let slot = locals
            .get_mut(place.local as usize)
            .ok_or_else(|| VmError::Runtime {
                message: format!("missing local {}", place.local),
            })?;
        *slot = value;
        return Ok(());
    }

    let mut base = locals
        .get(place.local as usize)
        .cloned()
        .ok_or_else(|| VmError::Runtime {
            message: format!("missing local {}", place.local),
        })?;
    apply_store(locals, &mut base, &place.projection, value)?;
    if let Some(slot) = locals.get_mut(place.local as usize) {
        *slot = base;
    }
    Ok(())
}

fn apply_store(
    locals: &[Value],
    base: &mut Value,
    projection: &[BytecodePlaceElem],
    value: Value,
) -> Result<(), VmError> {
    if projection.is_empty() {
        *base = value;
        return Ok(());
    }
    match &projection[0] {
        BytecodePlaceElem::Field(index) => match base {
            Value::Tuple(items) | Value::Array(items) | Value::List(items) => {
                let idx = *index as usize;
                if idx >= items.len() {
                    return Err(VmError::Runtime {
                        message: "field index out of bounds".to_string(),
                    });
                }
                apply_store(locals, &mut items[idx], &projection[1..], value)
            }
            _ => Err(VmError::Unsupported {
                message: "field store unsupported for value".to_string(),
            }),
        },
        BytecodePlaceElem::Index(local) => {
            let idx = match locals.get(*local as usize) {
                Some(Value::Int(value)) => *value as usize,
                Some(Value::UInt(value)) => *value as usize,
                _ => {
                    return Err(VmError::Runtime {
                        message: "index local is not int".to_string(),
                    });
                }
            };
            match base {
                Value::Array(items) | Value::List(items) => {
                    if idx >= items.len() {
                        return Err(VmError::Runtime {
                            message: "index out of bounds".to_string(),
                        });
                    }
                    apply_store(locals, &mut items[idx], &projection[1..], value)
                }
                _ => Err(VmError::Unsupported {
                    message: "index store unsupported for value".to_string(),
                }),
            }
        }
    }
}

fn convert_const(value: &BytecodeConst) -> Result<Value, VmError> {
    Ok(match value {
        BytecodeConst::Unit => Value::Unit,
        BytecodeConst::Bool(value) => Value::Bool(*value),
        BytecodeConst::Int(value) => Value::Int(*value),
        BytecodeConst::UInt(value) => Value::UInt(*value),
        BytecodeConst::Float(value) => Value::Float(*value),
        BytecodeConst::Str(value) => Value::Str(value.clone()),
        BytecodeConst::Function(name) => Value::Str(name.clone()),
        BytecodeConst::Null => Value::Null,
        BytecodeConst::Tuple(items) => {
            Value::Tuple(items.iter().map(convert_const).collect::<Result<_, _>>()?)
        }
        BytecodeConst::Array(items) => {
            Value::Array(items.iter().map(convert_const).collect::<Result<_, _>>()?)
        }
        BytecodeConst::List(items) => {
            Value::List(items.iter().map(convert_const).collect::<Result<_, _>>()?)
        }
        BytecodeConst::Map(items) => {
            let mut result = Vec::with_capacity(items.len());
            for (key, value) in items {
                result.push((convert_const(key)?, convert_const(value)?));
            }
            Value::Map(result)
        }
    })
}

fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Unit, Value::Unit) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::UInt(a), Value::UInt(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Null, Value::Null) => true,
        (Value::Tuple(a), Value::Tuple(b))
        | (Value::Array(a), Value::Array(b))
        | (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(l, r)| values_equal(l, r))
        }
        (Value::Map(a), Value::Map(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((lk, lv), (rk, rv))| values_equal(lk, rk) && values_equal(lv, rv))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_bytecode::{BytecodeBlock, BytecodeConst, BytecodeFunction, BytecodeInstr};

    #[test]
    fn runs_simple_addition() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Int(40), BytecodeConst::Int(2)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Add),
                        BytecodeInstr::StoreLocal(0),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let vm = Vm::new(program);
        let result = vm.run_main().expect("vm should run");
        assert!(matches!(result, Value::Int(42)));
    }

    #[test]
    fn rejects_argument_arity_mismatch() {
        let program = BytecodeProgram {
            const_pool: vec![],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 1,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let vm = Vm::new(program);
        let err = vm.run_main().expect_err("should reject missing args");
        assert!(matches!(err, VmError::Runtime { .. }));
    }

    #[test]
    fn supports_uint_and_bool_ops() {
        let program = BytecodeProgram {
            const_pool: vec![
                BytecodeConst::UInt(2),
                BytecodeConst::UInt(1),
                BytecodeConst::Bool(true),
                BytecodeConst::Bool(false),
            ],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 2,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![
                        BytecodeInstr::LoadConst(0),
                        BytecodeInstr::LoadConst(1),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Shl),
                        BytecodeInstr::StoreLocal(0),
                        BytecodeInstr::LoadConst(2),
                        BytecodeInstr::LoadConst(3),
                        BytecodeInstr::BinaryOp(BytecodeBinOp::Or),
                        BytecodeInstr::StoreLocal(1),
                    ],
                    terminator: BytecodeTerminator::Return,
                }],
            }],
            entry: Some("main".to_string()),
        };

        let vm = Vm::new(program);
        let result = vm.run_main().expect("vm should run");
        assert!(matches!(result, Value::UInt(4)));
    }

    #[test]
    fn validate_jump_if_true_targets() {
        let program = BytecodeProgram {
            const_pool: vec![BytecodeConst::Bool(true)],
            functions: vec![BytecodeFunction {
                name: "main".to_string(),
                params: 0,
                locals: 1,
                blocks: vec![BytecodeBlock {
                    id: 0,
                    code: vec![BytecodeInstr::LoadConst(0), BytecodeInstr::StoreLocal(0)],
                    terminator: BytecodeTerminator::JumpIfTrue {
                        target: 0,
                        otherwise: 1,
                    },
                }],
            }],
            entry: Some("main".to_string()),
        };

        let vm = Vm::new(program);
        let err = vm
            .run_main()
            .expect_err("missing otherwise block should fail");
        assert!(matches!(err, VmError::Runtime { .. }));
    }
}
