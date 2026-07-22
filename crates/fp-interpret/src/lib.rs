mod vm;

use std::collections::HashMap;

use fp_core::ast::{Value, ValueList, ValueString, ValueTuple};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstruction, LirInstructionKind,
    LirProgram, LirTerminator, LirType, LirValue, RegisterId,
};

use crate::vm::{
    is_object_type, lir_type_info, mem_load, mem_store, raw_to_value, value_to_raw, ThreadState,
};

pub use crate::vm::VmError;

type LirResult<T> = Result<T, VmError>;

pub struct LirInterpreter {
    state: ThreadState,
    register_types: HashMap<RegisterId, LirType>,
}

impl LirInterpreter {
    pub fn new() -> Self {
        Self {
            state: ThreadState::new(),
            register_types: HashMap::new(),
        }
    }

    pub fn run_main(&mut self, program: &LirProgram) -> LirResult<Value> {
        let entry = program
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .or_else(|| program.functions.first());
        let func = entry.ok_or(VmError::Runtime("no entry point".into()))?;
        self.run_function(program, func, &[])
    }

    pub fn run_function(
        &mut self,
        _program: &LirProgram,
        func: &LirFunction,
        args: &[Value],
    ) -> LirResult<Value> {
        self.state.push_frame(func.name.as_str().to_string());
        self.register_types.clear();
        for local in &func.locals {
            if !local.is_argument {
                let (bits, _) = lir_type_info(&local.ty);
                let size = (bits as u64 + 7) / 8;
                let sp = self.state.regs.sp();
                let addr = self.state.mem.stack_alloc(sp, size, 8)?;
                self.state.regs.set_sp(addr);
                self.state.set_local_addr(local.id, addr);
            }
        }
        for (i, arg) in args.iter().enumerate() {
            let reg = i as RegisterId + 1;
            if i < func.signature.params.len() && is_object_type(&func.signature.params[i]) {
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(arg.clone());
                self.state.regs.write(reg, handle);
            } else {
                self.state.regs.write(reg, value_to_raw(arg));
            }
        }
        let mut current = func.basic_blocks.first().map(|b| b.id).unwrap_or(0);
        let block_map: HashMap<BasicBlockId, &LirBasicBlock> =
            func.basic_blocks.iter().map(|b| (b.id, b)).collect();
        let result = loop {
            let block = block_map
                .get(&current)
                .ok_or(VmError::Runtime(format!("undefined block {current}")))?;
            for instr in &block.instructions {
                self.exec_instruction(instr)?;
            }
            match &block.terminator {
                LirTerminator::Return(val) => {
                    let ret_ty = &func.signature.return_type;
                    let v = match val {
                        Some(v) => self.resolve_typed(v, ret_ty)?,
                        None => Value::unit(),
                    };
                    break Ok(v);
                }
                LirTerminator::Br(dest) => current = *dest,
                LirTerminator::CondBr {
                    condition,
                    if_true,
                    if_false,
                } => {
                    current = if self.resolve_raw(condition)? != 0 {
                        *if_true
                    } else {
                        *if_false
                    };
                }
                LirTerminator::Unreachable => break Err(VmError::Runtime("unreachable".into())),
                other => break Err(VmError::Runtime(format!("terminator: {other:?}"))),
            }
        };
        self.state.pop_frame();
        result
    }

    fn exec_instruction(&mut self, instr: &LirInstruction) -> LirResult<()> {
        let dst = instr.id;
        let result = match &instr.kind {
            LirInstructionKind::Add(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_add(y)),
            LirInstructionKind::Sub(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_sub(y)),
            LirInstructionKind::Mul(a, b) => self.binop(dst, a, b, |x, y| x.wrapping_mul(y)),
            LirInstructionKind::Div(a, b) => self.binop_div(dst, a, b),
            LirInstructionKind::Rem(a, b) => self.binop_rem(dst, a, b),
            LirInstructionKind::Eq(a, b) => self.cmp_raw(dst, a, b, |x, y| (x == y) as u64),
            LirInstructionKind::Ne(a, b) => self.cmp_raw(dst, a, b, |x, y| (x != y) as u64),
            LirInstructionKind::Lt(a, b) => self.cmp_signed(dst, a, b, |x, y| x < y),
            LirInstructionKind::Le(a, b) => self.cmp_signed(dst, a, b, |x, y| x <= y),
            LirInstructionKind::Gt(a, b) => self.cmp_signed(dst, a, b, |x, y| x > y),
            LirInstructionKind::Ge(a, b) => self.cmp_signed(dst, a, b, |x, y| x >= y),
            LirInstructionKind::And(a, b) => self.cmp_raw(dst, a, b, |x, y| x & y),
            LirInstructionKind::Or(a, b) => self.cmp_raw(dst, a, b, |x, y| x | y),
            LirInstructionKind::Xor(a, b) => self.cmp_raw(dst, a, b, |x, y| x ^ y),
            LirInstructionKind::Shl(a, b) => self.shift(dst, a, b, |x, s| x.wrapping_shl(s)),
            LirInstructionKind::Shr(a, b) => self.shift(dst, a, b, |x, s| x.wrapping_shr(s)),
            LirInstructionKind::Not(a) => self.unary(dst, a, |x| !x),
            LirInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                let cond = self.resolve_raw(condition)?;
                let chosen = if cond != 0 { if_true } else { if_false };
                self.unary(dst, chosen, |x| x)
            }
            LirInstructionKind::Phi { incoming } => {
                let raw = incoming
                    .first()
                    .map(|(v, _)| self.resolve_raw(v))
                    .unwrap_or(Ok(0))?;
                self.wr(dst, raw);
                Ok(())
            }
            LirInstructionKind::Alloca { size, alignment } => {
                let raw_size = self.resolve_raw(size)?;
                let sp = self.state.regs.sp();
                let addr = self.state.mem.stack_alloc(sp, raw_size, *alignment)?;
                self.state.regs.set_sp(addr);
                self.wr(dst, addr);
                Ok(())
            }
            LirInstructionKind::Store { value, address, .. } => {
                let val = self.resolve_raw(value)?;
                let addr = self.resolve_raw(address)?;
                let ty = self.infer_type(value);
                mem_store(&mut self.state.mem, addr, val, &ty)
            }
            LirInstructionKind::Load { address, .. } => {
                let addr = self.resolve_raw(address)?;
                let ty = instr.type_hint.as_ref().unwrap_or(&LirType::I64);
                let val = mem_load(&self.state.mem, addr, ty)?;
                self.wr(dst, val);
                Ok(())
            }
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                let base = self.resolve_raw(ptr)?;
                let mut off: u64 = 0;
                for idx in indices {
                    off = off.wrapping_add(self.resolve_raw(idx)?);
                }
                self.wr(dst, base.wrapping_add(off));
                Ok(())
            }
            LirInstructionKind::PtrToInt(v) | LirInstructionKind::IntToPtr(v) => {
                self.unary(dst, v, |x| x)
            }
            LirInstructionKind::Bitcast(v, _)
            | LirInstructionKind::ZExt(v, _)
            | LirInstructionKind::SExt(v, _)
            | LirInstructionKind::SextOrTrunc(v, _)
            | LirInstructionKind::Trunc(v, _)
            | LirInstructionKind::FPTrunc(v, _)
            | LirInstructionKind::FPExt(v, _)
            | LirInstructionKind::FPToUI(v, _)
            | LirInstructionKind::FPToSI(v, _)
            | LirInstructionKind::UIToFP(v, _)
            | LirInstructionKind::SIToFP(v, _) => self.unary(dst, v, |x| x),
            LirInstructionKind::ExtractValue { aggregate, indices } => {
                self.extract_value(dst, aggregate, indices, instr.type_hint.as_ref())
            }
            LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => self.insert_value(dst, aggregate, element, indices, instr.type_hint.as_ref()),
            LirInstructionKind::Call { function, args, .. } => {
                self.handle_call(dst, function, args)
            }
            LirInstructionKind::IntrinsicCall { .. } => {
                self.wr(dst, 0);
                Ok(())
            }
            LirInstructionKind::InlineAsm { .. }
            | LirInstructionKind::LandingPad { .. }
            | LirInstructionKind::Freeze(_)
            | LirInstructionKind::ExecQuery(_) => Err(VmError::Runtime("unsupported".into())),
            _ => Err(VmError::Runtime("unimplemented".into())),
        };
        if result.is_ok() {
            if let Some(ty) = instr.type_hint.as_ref() {
                self.register_types.insert(dst, ty.clone());
            }
        }
        result
    }

    fn wr(&mut self, dst: u32, val: u64) {
        self.state.regs.write(dst, val);
    }

    fn resolve_raw(&self, val: &LirValue) -> LirResult<u64> {
        match val {
            LirValue::Register(id) => Ok(self.state.regs.read(*id)),
            LirValue::Constant(c) => Ok(const_raw(c)),
            LirValue::Local(id) => self.state.mem.load_u64(self.state.local_addr(*id)),
            LirValue::StackSlot(id) => self.state.mem.load_u64(self.state.local_addr(*id)),
            LirValue::Global(..) => Err(VmError::Runtime("global".into())),
            LirValue::Function(_) => Ok(0),
            LirValue::Undef(_) | LirValue::Null(_) => Ok(0),
        }
    }

    fn resolve_typed(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        let raw = self.resolve_raw(val)?;
        if is_object_type(ty) {
            let idx = raw as usize;
            return self
                .state
                .objects
                .get(idx)
                .cloned()
                .ok_or(VmError::Runtime(format!("dangling object handle {idx}")));
        }
        let (bits, signed) = lir_type_info(ty);
        Ok(raw_to_value(raw, signed, bits))
    }

    fn infer_type(&self, val: &LirValue) -> LirType {
        match val {
            LirValue::Constant(c) => const_ty(c),
            LirValue::Global(_, ty) => ty.clone(),
            LirValue::Register(id) => self.register_types.get(id).cloned().unwrap_or(LirType::I64),
            _ => LirType::I64,
        }
    }

    fn insert_value(
        &mut self,
        dst: u32,
        aggregate: &LirValue,
        element: &LirValue,
        indices: &[u32],
        aggregate_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let aggregate_ty = aggregate_ty
            .cloned()
            .unwrap_or_else(|| self.infer_type(aggregate));
        let element_ty = self.aggregate_element_type(&aggregate_ty, indices)?;
        let mut aggregate_value = self.resolve_aggregate_value(aggregate, &aggregate_ty)?;
        let element_value = self.resolve_runtime_value(element, &element_ty)?;
        Self::aggregate_insert(&mut aggregate_value, &element_ty, indices, element_value)?;
        self.store_runtime_value(dst, &aggregate_ty, aggregate_value)
    }

    fn extract_value(
        &mut self,
        dst: u32,
        aggregate: &LirValue,
        indices: &[u32],
        result_ty: Option<&LirType>,
    ) -> LirResult<()> {
        let aggregate_ty = self.infer_type(aggregate);
        let aggregate_value = self.resolve_aggregate_value(aggregate, &aggregate_ty)?;
        let element_ty = if let Some(result_ty) = result_ty {
            result_ty.clone()
        } else {
            self.aggregate_element_type(&aggregate_ty, indices)?
        };
        let value = Self::aggregate_extract(&aggregate_value, indices)?;
        self.store_runtime_value(dst, &element_ty, value)
    }

    fn store_runtime_value(&mut self, dst: u32, ty: &LirType, value: Value) -> LirResult<()> {
        if Self::is_aggregate_runtime_type(ty) {
            let handle = self.state.objects.len() as u64;
            self.state.objects.push(value);
            self.wr(dst, handle);
            return Ok(());
        }
        if matches!(ty, LirType::Ptr(_)) {
            match value {
                Value::String(_)
                | Value::List(_)
                | Value::Tuple(_)
                | Value::Struct(_)
                | Value::Structural(_)
                | Value::Bytes(_)
                | Value::Pointer(_) => {
                    let handle = self.state.objects.len() as u64;
                    self.state.objects.push(value);
                    self.wr(dst, handle);
                }
                Value::Null(_) | Value::Undefined(_) => self.wr(dst, 0),
                other => self.wr(dst, value_to_raw(&other)),
            }
            return Ok(());
        }
        self.wr(dst, value_to_raw(&value));
        Ok(())
    }

    fn resolve_runtime_value(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        if Self::is_aggregate_runtime_type(ty) {
            return self.resolve_aggregate_value(val, ty);
        }
        if matches!(ty, LirType::Ptr(_)) {
            return match val {
                LirValue::Constant(LirConstant::String(text)) => {
                    Ok(Value::String(ValueString::new_ref(text.clone())))
                }
                LirValue::Constant(LirConstant::Null(_)) | LirValue::Null(_) => Ok(Value::null()),
                _ => Ok(Value::uint(self.resolve_raw(val)?)),
            };
        }
        let raw = self.resolve_raw(val)?;
        let (bits, signed) = lir_type_info(ty);
        Ok(raw_to_value(raw, signed, bits))
    }

    fn resolve_aggregate_value(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        match val {
            LirValue::Register(id) => {
                let idx = self.state.regs.read(*id) as usize;
                let value = self
                    .state
                    .objects
                    .get(idx)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling object handle {idx}")))?;
                if Self::is_aggregate_runtime_type(ty) && matches!(value, Value::Unit(_)) {
                    Ok(Self::default_value_for_type(ty))
                } else {
                    Ok(value)
                }
            }
            LirValue::Constant(LirConstant::Undef(_))
            | LirValue::Constant(LirConstant::Null(_))
            | LirValue::Undef(_)
            | LirValue::Null(_) => Ok(Self::default_value_for_type(ty)),
            LirValue::Constant(constant) => Self::constant_to_value(constant),
            _ => Err(VmError::Runtime(format!(
                "expected aggregate value for {ty:?}, found {val:?}"
            ))),
        }
    }

    fn constant_to_value(constant: &LirConstant) -> LirResult<Value> {
        Ok(match constant {
            LirConstant::Int(v, _) => Value::int(*v),
            LirConstant::UInt(v, _) => Value::uint(*v),
            LirConstant::Float(v, _) => Value::decimal(*v),
            LirConstant::Bool(v) => Value::bool(*v),
            LirConstant::String(text) => Value::String(ValueString::new_ref(text.clone())),
            LirConstant::Array(values, _) => Value::List(ValueList::new(
                values
                    .iter()
                    .map(Self::constant_to_value)
                    .collect::<LirResult<Vec<_>>>()?,
            )),
            LirConstant::Struct(values, _) => Value::Tuple(ValueTuple::new(
                values
                    .iter()
                    .map(Self::constant_to_value)
                    .collect::<LirResult<Vec<_>>>()?,
            )),
            LirConstant::Null(_) => Value::null(),
            LirConstant::Undef(ty) => Self::default_value_for_type(ty),
            LirConstant::GlobalRef(_, _, _) | LirConstant::FunctionRef(_, _) => Value::uint(0),
        })
    }

    fn default_value_for_type(ty: &LirType) -> Value {
        match ty {
            LirType::Array(elem, len) => Value::List(ValueList::new(
                (0..*len)
                    .map(|_| Self::default_value_for_type(elem))
                    .collect::<Vec<_>>(),
            )),
            LirType::Vector(elem, len) => Value::List(ValueList::new(
                (0..*len)
                    .map(|_| Self::default_value_for_type(elem))
                    .collect::<Vec<_>>(),
            )),
            LirType::Struct { fields, .. } => Value::Tuple(ValueTuple::new(
                fields
                    .iter()
                    .map(Self::default_value_for_type)
                    .collect::<Vec<_>>(),
            )),
            LirType::Ptr(_) => Value::uint(0),
            LirType::Void => Value::unit(),
            _ => {
                let (bits, signed) = lir_type_info(ty);
                raw_to_value(0, signed, bits)
            }
        }
    }

    fn aggregate_element_type(&self, ty: &LirType, indices: &[u32]) -> LirResult<LirType> {
        let mut current = ty.clone();
        for index in indices {
            current = match current {
                LirType::Struct { fields, .. } => fields.get(*index as usize).cloned().ok_or(
                    VmError::Runtime(format!("struct index {index} out of range")),
                )?,
                LirType::Array(elem, len) => {
                    if (*index as u64) >= len {
                        return Err(VmError::Runtime(format!(
                            "array index {index} out of range"
                        )));
                    }
                    *elem
                }
                LirType::Vector(elem, len) => {
                    if *index >= len {
                        return Err(VmError::Runtime(format!(
                            "vector index {index} out of range"
                        )));
                    }
                    *elem
                }
                other => {
                    return Err(VmError::Runtime(format!(
                        "cannot index into non-aggregate type {other:?}"
                    )));
                }
            };
        }
        Ok(current)
    }

    fn aggregate_insert(
        aggregate: &mut Value,
        element_ty: &LirType,
        indices: &[u32],
        element: Value,
    ) -> LirResult<()> {
        let Some((first, rest)) = indices.split_first() else {
            *aggregate = element;
            return Ok(());
        };
        match aggregate {
            Value::List(list) => {
                let idx = *first as usize;
                let slot = list.values.get_mut(idx).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", first))
                })?;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                if matches!(slot, Value::Null(_) | Value::Undefined(_)) {
                    *slot = Self::default_value_for_type(element_ty);
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            Value::Tuple(tuple) => {
                let idx = *first as usize;
                let slot = tuple.values.get_mut(idx).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", first))
                })?;
                if rest.is_empty() {
                    *slot = element;
                    return Ok(());
                }
                if matches!(slot, Value::Null(_) | Value::Undefined(_)) {
                    *slot = Self::default_value_for_type(element_ty);
                }
                Self::aggregate_insert(slot, element_ty, rest, element)
            }
            other => Err(VmError::Runtime(format!(
                "InsertValue expects aggregate, found {other:?}"
            ))),
        }
    }

    fn aggregate_extract(aggregate: &Value, indices: &[u32]) -> LirResult<Value> {
        let mut current = aggregate;
        for index in indices {
            current = match current {
                Value::List(list) => list.values.get(*index as usize).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", index))
                })?,
                Value::Tuple(tuple) => tuple.values.get(*index as usize).ok_or_else(|| {
                    VmError::Runtime(format!("aggregate index {} out of range", index))
                })?,
                other => {
                    return Err(VmError::Runtime(format!(
                        "ExtractValue expects aggregate, found {other:?}"
                    )));
                }
            };
        }
        Ok(current.clone())
    }

    fn is_aggregate_runtime_type(ty: &LirType) -> bool {
        matches!(ty, LirType::Struct { .. } | LirType::Array(..) | LirType::Vector(..))
    }

    fn binop(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(i64, i64) -> i64,
    ) -> LirResult<()> {
        let lhs = self.resolve_raw(a)? as i64;
        let rhs = self.resolve_raw(b)? as i64;
        self.wr(dst, op(lhs, rhs) as u64);
        Ok(())
    }

    fn cmp_raw(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(u64, u64) -> u64,
    ) -> LirResult<()> {
        self.wr(dst, op(self.resolve_raw(a)?, self.resolve_raw(b)?));
        Ok(())
    }

    fn cmp_signed(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(i64, i64) -> bool,
    ) -> LirResult<()> {
        let lhs = self.resolve_raw(a)? as i64;
        let rhs = self.resolve_raw(b)? as i64;
        self.wr(dst, op(lhs, rhs) as u64);
        Ok(())
    }

    fn shift(
        &mut self,
        dst: u32,
        a: &LirValue,
        b: &LirValue,
        op: fn(u64, u32) -> u64,
    ) -> LirResult<()> {
        self.wr(dst, op(self.resolve_raw(a)?, self.resolve_raw(b)? as u32));
        Ok(())
    }

    fn unary(&mut self, dst: u32, a: &LirValue, op: fn(u64) -> u64) -> LirResult<()> {
        self.wr(dst, op(self.resolve_raw(a)?));
        Ok(())
    }

    fn binop_div(&mut self, dst: u32, a: &LirValue, b: &LirValue) -> LirResult<()> {
        let rhs = self.resolve_raw(b)? as i64;
        if rhs == 0 {
            return Err(VmError::DivisionByZero);
        }
        self.wr(dst, (self.resolve_raw(a)? as i64).wrapping_div(rhs) as u64);
        Ok(())
    }

    fn binop_rem(&mut self, dst: u32, a: &LirValue, b: &LirValue) -> LirResult<()> {
        let rhs = self.resolve_raw(b)? as i64;
        if rhs == 0 {
            return Err(VmError::DivisionByZero);
        }
        self.wr(dst, (self.resolve_raw(a)? as i64).wrapping_rem(rhs) as u64);
        Ok(())
    }

    fn handle_call(&mut self, dst: u32, function: &LirValue, args: &[LirValue]) -> LirResult<()> {
        match function {
            LirValue::Function(name) => {
                let raws: Vec<u64> = args
                    .iter()
                    .map(|a| self.resolve_raw(a))
                    .collect::<LirResult<Vec<_>>>()?;
                let r = self.call_intrinsic(name, &raws)?;
                self.wr(dst, r);
                Ok(())
            }
            _ => Err(VmError::Runtime("indirect call".into())),
        }
    }

    fn call_intrinsic(&mut self, name: &str, args: &[u64]) -> LirResult<u64> {
        match name {
            "println" | "print" | "eprintln" | "eprint" | "printf" => Ok(0),
            "sizeof" | "strlen" => Ok(0),
            "malloc" => {
                let _size = args.first().copied().unwrap_or(0) as usize;
                let obj = Value::Unit(Default::default());
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            "free" => Ok(0),
            "realloc" => {
                let _ptr = args.first().copied().unwrap_or(0);
                let _new_size = args.get(1).copied().unwrap_or(0) as usize;
                let obj = Value::Unit(Default::default());
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            "sin" | "cos" | "tan" | "sqrt" | "pow" => Ok(0),
            "strcmp" => Ok(if args.len() >= 2 && args[0] == args[1] {
                1
            } else {
                0
            }),
            n if n.starts_with("opaque__") || n.starts_with("type__") => {
                let obj = Value::Unit(Default::default());
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            n if n.ends_with("__new") => {
                let obj = Value::Unit(Default::default());
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            _ => Ok(0),
        }
    }
}

fn const_raw(c: &LirConstant) -> u64 {
    match c {
        LirConstant::Int(v, _) => *v as u64,
        LirConstant::UInt(v, _) => *v,
        LirConstant::Float(v, _) => v.to_bits(),
        LirConstant::Bool(v) => {
            if *v {
                1
            } else {
                0
            }
        }
        _ => 0,
    }
}

fn const_ty(c: &LirConstant) -> LirType {
    match c {
        LirConstant::Int(_, ty)
        | LirConstant::UInt(_, ty)
        | LirConstant::Float(_, ty)
        | LirConstant::Null(ty)
        | LirConstant::Undef(ty)
        | LirConstant::Array(_, ty)
        | LirConstant::Struct(_, ty)
        | LirConstant::GlobalRef(_, ty, _)
        | LirConstant::FunctionRef(_, ty) => ty.clone(),
        LirConstant::Bool(_) => LirType::I1,
        LirConstant::String(_) => LirType::Ptr(Box::new(LirType::I8)),
    }
}

impl Default for LirInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{
        CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
        LirInstruction, LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
    };

    fn make(f: LirFunction) -> LirProgram {
        LirProgram {
            functions: vec![f],
            globals: vec![],
            type_definitions: vec![],
            queries: vec![],
        }
    }

    fn int(v: i64) -> LirValue {
        LirValue::Constant(LirConstant::Int(v, LirType::I64))
    }

    fn reg(id: u32) -> LirValue {
        LirValue::Register(id)
    }

    fn ins(k: LirInstructionKind) -> LirInstruction {
        LirInstruction {
            id: 0,
            kind: k,
            type_hint: Some(LirType::I64),
            debug_info: None,
        }
    }

    fn bb(id: u32, instrs: Vec<LirInstruction>, term: LirTerminator) -> LirBasicBlock {
        LirBasicBlock {
            id,
            label: None,
            instructions: instrs,
            terminator: term,
            predecessors: vec![],
            successors: vec![],
        }
    }

    fn ret(v: LirValue) -> LirTerminator {
        LirTerminator::Return(Some(v))
    }

    fn sig(p: &[LirType], r: LirType) -> LirFunctionSignature {
        LirFunctionSignature {
            params: p.to_vec(),
            return_type: r,
            is_variadic: false,
        }
    }

    fn i(id: u32, k: LirInstructionKind) -> LirInstruction {
        LirInstruction {
            id,
            kind: k,
            type_hint: Some(LirType::I64),
            debug_info: None,
        }
    }

    #[test]
    fn constant() {
        let f = LirFunction {
            name: Name::new("main"),
            signature: sig(&[], LirType::I64),
            basic_blocks: vec![bb(
                0,
                vec![ins(LirInstructionKind::Add(int(40), int(2)))],
                ret(reg(0)),
            )],
            locals: vec![],
            stack_slots: vec![],
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::Internal,
            is_declaration: false,
        };
        assert_eq!(
            LirInterpreter::new().run_main(&make(f)).unwrap(),
            Value::int(42)
        );
    }

    #[test]
    fn arith_chain() {
        let f = LirFunction {
            name: Name::new("main"),
            signature: sig(&[], LirType::I64),
            basic_blocks: vec![bb(
                0,
                vec![
                    i(0, LirInstructionKind::Mul(int(5), int(4))),
                    i(1, LirInstructionKind::Mul(reg(0), int(3))),
                    i(2, LirInstructionKind::Mul(reg(1), int(2))),
                    i(3, LirInstructionKind::Mul(reg(2), int(1))),
                ],
                ret(reg(3)),
            )],
            locals: vec![],
            stack_slots: vec![],
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::Internal,
            is_declaration: false,
        };
        assert_eq!(
            LirInterpreter::new().run_main(&make(f)).unwrap(),
            Value::int(120)
        );
    }

    fn cond_br_f(take: bool) -> LirProgram {
        make(LirFunction {
            name: Name::new("main"),
            signature: sig(&[], LirType::I64),
            basic_blocks: vec![
                LirBasicBlock {
                    id: 0,
                    label: None,
                    instructions: vec![ins(LirInstructionKind::Eq(
                        int(if take { 1 } else { 0 }),
                        int(1),
                    ))],
                    terminator: LirTerminator::CondBr {
                        condition: reg(0),
                        if_true: 1,
                        if_false: 2,
                    },
                    predecessors: vec![],
                    successors: vec![1, 2],
                },
                bb(1, vec![], ret(int(7))),
                bb(2, vec![], ret(int(9))),
            ],
            locals: vec![],
            stack_slots: vec![],
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::Internal,
            is_declaration: false,
        })
    }

    #[test]
    fn cond_br_true() {
        assert_eq!(
            LirInterpreter::new().run_main(&cond_br_f(true)).unwrap(),
            Value::int(7)
        );
    }

    #[test]
    fn cond_br_false() {
        assert_eq!(
            LirInterpreter::new().run_main(&cond_br_f(false)).unwrap(),
            Value::int(9)
        );
    }

    #[test]
    fn insert_and_extract_struct_field() {
        let slice_ty = LirType::Struct {
            fields: vec![LirType::Ptr(Box::new(LirType::I8)), LirType::I64],
            packed: false,
            name: Some("slice".into()),
        };
        let f = LirFunction {
            name: Name::new("main"),
            signature: sig(&[], LirType::I64),
            basic_blocks: vec![bb(
                0,
                vec![
                    LirInstruction {
                        id: 0,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: LirValue::Constant(LirConstant::Undef(slice_ty.clone())),
                            element: LirValue::Constant(LirConstant::UInt(0x1234, LirType::I64)),
                            indices: vec![0],
                        },
                        type_hint: Some(slice_ty.clone()),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: reg(0),
                            element: int(5),
                            indices: vec![1],
                        },
                        type_hint: Some(slice_ty),
                        debug_info: None,
                    },
                    i(
                        2,
                        LirInstructionKind::ExtractValue {
                            aggregate: reg(1),
                            indices: vec![1],
                        },
                    ),
                ],
                ret(reg(2)),
            )],
            locals: vec![],
            stack_slots: vec![],
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::Internal,
            is_declaration: false,
        };

        assert_eq!(
            LirInterpreter::new().run_main(&make(f)).unwrap(),
            Value::int(5)
        );
    }

    #[test]
    fn extract_string_pointer_from_aggregate() {
        let array_ty = LirType::Array(Box::new(LirType::Ptr(Box::new(LirType::I8))), 1);
        let f = LirFunction {
            name: Name::new("main"),
            signature: sig(&[], LirType::Ptr(Box::new(LirType::I8))),
            basic_blocks: vec![bb(
                0,
                vec![
                    LirInstruction {
                        id: 0,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: LirValue::Constant(LirConstant::Undef(array_ty.clone())),
                            element: LirValue::Constant(LirConstant::String("abc".into())),
                            indices: vec![0],
                        },
                        type_hint: Some(array_ty),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 1,
                        kind: LirInstructionKind::ExtractValue {
                            aggregate: reg(0),
                            indices: vec![0],
                        },
                        type_hint: Some(LirType::Ptr(Box::new(LirType::I8))),
                        debug_info: None,
                    },
                ],
                ret(reg(1)),
            )],
            locals: vec![],
            stack_slots: vec![],
            calling_convention: CallingConvention::C,
            linkage: fp_core::lir::Linkage::Internal,
            is_declaration: false,
        };

        assert_eq!(
            LirInterpreter::new().run_main(&make(f)).unwrap(),
            Value::String(ValueString::new_ref("abc"))
        );
    }
}
