mod vm;

use std::collections::HashMap;

use fp_core::ast::{Value, ValueList, ValueMapEntry, ValueString, ValueTuple};
use fp_core::lir::{
    BasicBlockId, CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirInstruction,
    LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, RegisterId,
};
use fp_ffi::{FfiRuntime, FfiSignature, FfiType};

use crate::vm::{
    is_object_type, lir_type_info, mem_load, mem_store, raw_to_value, value_to_raw, ThreadState,
};

pub use crate::vm::VmError;

type LirResult<T> = Result<T, VmError>;

pub struct LirInterpreter {
    state: ThreadState,
    register_types: HashMap<RegisterId, LirType>,
    /// Global object handles keyed by name, populated from the LIR
    /// program during run_main / run_function_named.
    global_values: HashMap<String, u64>,
    /// Optional FFI runtime for calling extern C functions.  Set
    /// before running if the program contains extern declarations.
    ffi: Option<FfiRuntime>,
    /// C signatures of extern functions, keyed by function name.
    /// Populated from LIR functions with `is_declaration = true`.
    extern_sigs: HashMap<String, FfiSignature>,
    /// Tracks the predecessor block ID for correct Phi resolution.
    last_predecessor: Option<BasicBlockId>,
}

impl LirInterpreter {
    pub fn new() -> Self {
        Self {
            state: ThreadState::new(),
            register_types: HashMap::new(),
            global_values: HashMap::new(),
            ffi: FfiRuntime::new().ok(),
            extern_sigs: HashMap::new(),
            last_predecessor: None,
        }
    }

    pub fn run_main(&mut self, program: &LirProgram) -> LirResult<Value> {
        self.populate_globals(program);
        let entry = program
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .or_else(|| program.functions.first());
        let func = entry.ok_or(VmError::Runtime("no entry point".into()))?;
        self.run_function(program, func, &[])
    }

    pub fn run_function_named(&mut self, program: &LirProgram, name: &str) -> LirResult<Value> {
        self.populate_globals(program);
        let func = program
            .functions
            .iter()
            .find(|func| func.name.as_str() == name)
            .ok_or_else(|| VmError::Runtime(format!("missing function {name}")))?;
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
                LirTerminator::Br(dest) => {
                    self.last_predecessor = Some(current);
                    current = *dest;
                }
                LirTerminator::CondBr {
                    condition,
                    if_true,
                    if_false,
                } => {
                    self.last_predecessor = Some(current);
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
                let predecessor = self.last_predecessor;
                let raw = incoming
                    .iter()
                    .find(|(_, bb)| predecessor.map_or(true, |p| p == *bb))
                    .map(|(v, _)| v)
                    .or_else(|| incoming.first().map(|(v, _)| v));
                match raw {
                    Some(val) => {
                        let raw = self.resolve_raw(val)?;
                        self.wr(dst, raw);
                        Ok(())
                    }
                    None => Ok(()),
                }
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
                let ty = self.infer_type(value);
                let val = if Self::is_aggregate_runtime_type(&ty) {
                    let aggregate_value = self.resolve_aggregate_value(value, &ty)?;
                    self.value_to_slot_raw(aggregate_value, &ty)
                } else if matches!(ty, LirType::Ptr(_)) {
                    let runtime_value = self.resolve_runtime_value(value, &ty)?;
                    self.value_to_slot_raw(runtime_value, &ty)
                } else {
                    self.resolve_raw(value)?
                };
                let addr = self.resolve_addr(address)?;
                mem_store(&mut self.state.mem, addr, val, &ty)
            }
            LirInstructionKind::Load { address, .. } => {
                let addr = self.resolve_addr(address)?;
                let ty = instr.type_hint.as_ref().unwrap_or(&LirType::I64);
                let val = mem_load(&self.state.mem, addr, ty)?;
                self.wr(dst, val);
                Ok(())
            }
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                let base = self.resolve_raw(ptr)?;
                let ptr_ty = self.infer_type(ptr);
                let elem_size = match &ptr_ty {
                    LirType::Ptr(pointee) => {
                        let (bits, _) = lir_type_info(pointee);
                        ((bits + 7) / 8) as u64
                    }
                    _ => 1u64,
                };
                let mut off: u64 = 0;
                for (i, idx) in indices.iter().enumerate() {
                    let scale = if i == 0 { elem_size.max(1) } else { 1 };
                    off = off.wrapping_add(self.resolve_raw(idx)?.wrapping_mul(scale));
                }
                self.wr(dst, base.wrapping_add(off));
                Ok(())
            }
            LirInstructionKind::PtrToInt(v) | LirInstructionKind::IntToPtr(v) => {
                self.unary(dst, v, |x| x)
            }
            LirInstructionKind::Bitcast(v, _) => self.unary(dst, v, |x| x),
            LirInstructionKind::ZExt(v, dst_ty)
            | LirInstructionKind::Trunc(v, dst_ty)
            | LirInstructionKind::SExt(v, dst_ty)
            | LirInstructionKind::SextOrTrunc(v, dst_ty) => {
                let src_val = self.resolve_raw(v)?;
                let src_ty = self.infer_type(v);
                let (src_bits, _) = lir_type_info(&src_ty);
                let (dst_bits, _dst_signed) = lir_type_info(dst_ty);
                let result = match &instr.kind {
                    LirInstructionKind::ZExt(..) | LirInstructionKind::Trunc(..) => {
                        if dst_bits < 64 {
                            src_val & ((1u64 << dst_bits) - 1)
                        } else {
                            src_val
                        }
                    }
                    LirInstructionKind::SExt(..) | LirInstructionKind::SextOrTrunc(..) => {
                        if src_bits == 0 || src_bits >= 64 {
                            src_val
                        } else {
                            let shift = 64 - src_bits;
                            ((src_val << shift) as i64 >> shift) as u64
                        }
                    }
                    _ => src_val,
                };
                self.wr(dst, result);
                Ok(())
            }
            LirInstructionKind::FPTrunc(v, _)
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
        // r1 is the stack pointer — never overwrite it with
        // instruction results, as some LIR producers may assign
        // instruction IDs that alias the sp register.
        if dst == 1 {
            return;
        }
        self.state.regs.write(dst, val);
    }

    fn resolve_raw(&self, val: &LirValue) -> LirResult<u64> {
        match val {
            LirValue::Register(id) => Ok(self.state.regs.read(*id)),
            LirValue::Constant(LirConstant::GlobalRef(name, _, _)) => self
                .global_values
                .get(name.as_str())
                .copied()
                .ok_or_else(|| VmError::Runtime(format!("missing global {name}"))),
            LirValue::Constant(c) => match c {
                LirConstant::String(_)
                | LirConstant::Array(..)
                | LirConstant::Struct(..)
                | LirConstant::Bytes(_) => Err(VmError::Runtime(format!(
                    "resolve_raw called on non-scalar constant: {c:?}"
                ))),
                _ => Ok(const_raw(c)),
            },
            LirValue::Local(id) => self.state.mem.load_u64(self.state.local_addr(*id)),
            LirValue::StackSlot(id) => self.state.mem.load_u64(self.state.local_addr(*id)),
            LirValue::Global(name, _) => self
                .global_values
                .get(name.as_str())
                .copied()
                .ok_or_else(|| VmError::Runtime(format!("missing global {name}"))),
            LirValue::Function(_) => Ok(0),
            LirValue::Undef(_) | LirValue::Null(_) => Ok(0),
        }
    }

    fn populate_globals(&mut self, program: &LirProgram) {
        for global in &program.globals {
            if let Some(init) = &global.initializer {
                // Push the value into the object heap and store the handle.
                if let Ok(value) = self.constant_to_value(init) {
                    let handle = self.state.objects.len() as u64;
                    self.state.objects.push(value);
                    self.global_values.insert(global.name.to_string(), handle);
                }
            }
        }
        // Collect C signatures from extern function declarations.
        for func in &program.functions {
            if func.is_declaration && func.calling_convention == CallingConvention::C {
                let sig = lir_sig_to_ffi(&func.signature);
                self.extern_sigs.insert(func.name.to_string(), sig);
            }
        }
    }

    /// Inject externally-resolved constant values as globals so that
    /// comptime functions can reference other already-computed consts.
    pub fn inject_globals(&mut self, values: &HashMap<String, Value>) {
        for (name, value) in values {
            let handle = self.state.objects.len() as u64;
            self.state.objects.push(value.clone());
            self.global_values.insert(name.clone(), handle);
        }
    }

    /// Resolve an address operand — for `LirValue::Local`, returns
    /// the pre-allocated stack address rather than the value at it.
    fn resolve_addr(&self, val: &LirValue) -> LirResult<u64> {
        match val {
            LirValue::Local(id) => Ok(self.state.local_addr(*id)),
            other => self.resolve_raw(other),
        }
    }

    fn resolve_typed(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        if let Some(value) = self.try_resolve_i8_slice(val, ty)? {
            return Ok(value);
        }
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
        let raw = self.value_to_slot_raw(value, ty);
        self.wr(dst, raw);
        Ok(())
    }

    fn resolve_runtime_value(&self, val: &LirValue, ty: &LirType) -> LirResult<Value> {
        if let Some(value) = self.try_resolve_i8_slice(val, ty)? {
            return Ok(value);
        }
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
            LirValue::Constant(constant) => self.constant_to_value(constant),
            _ => Err(VmError::Runtime(format!(
                "expected aggregate value for {ty:?}, found {val:?}"
            ))),
        }
    }

    fn constant_to_value(&self, constant: &LirConstant) -> LirResult<Value> {
        Ok(match constant {
            LirConstant::Int(v, _) => Value::int(*v),
            LirConstant::UInt(v, _) => Value::uint(*v),
            LirConstant::Float(v, _) => Value::decimal(*v),
            LirConstant::Bool(v) => Value::bool(*v),
            LirConstant::String(text) => Value::String(ValueString::new_ref(text.clone())),
            LirConstant::Array(values, _) => Value::List(ValueList::new(
                values
                    .iter()
                    .map(|value| self.constant_to_value(value))
                    .collect::<LirResult<Vec<_>>>()?,
            )),
            LirConstant::Struct(values, _) => Value::Tuple(ValueTuple::new(
                values
                    .iter()
                    .map(|value| self.constant_to_value(value))
                    .collect::<LirResult<Vec<_>>>()?,
            )),
            LirConstant::Null(_) => Value::null(),
            LirConstant::Undef(ty) => Self::default_value_for_type(ty),
            LirConstant::GlobalRef(name, _, _) => Value::uint(
                self.global_values
                    .get(name.as_str())
                    .copied()
                    .ok_or_else(|| VmError::Runtime(format!("missing global {name}")))?,
            ),
            LirConstant::FunctionRef(_, _) => Value::uint(0),
            LirConstant::Bytes(bytes) => {
                Value::Bytes(fp_core::ast::ValueBytes::from(bytes.as_slice()))
            }
        })
    }

    fn try_resolve_i8_slice(&self, val: &LirValue, ty: &LirType) -> LirResult<Option<Value>> {
        let LirType::Struct { fields, name, .. } = ty else {
            return Ok(None);
        };
        if name.as_deref() != Some("__slice") || fields.len() != 2 {
            return Ok(None);
        }
        let LirType::Ptr(elem) = &fields[0] else {
            return Ok(None);
        };
        if **elem != LirType::I8 || fields[1] != LirType::I64 {
            return Ok(None);
        }

        let aggregate = self.resolve_aggregate_value(val, ty)?;
        let Value::Tuple(tuple) = aggregate else {
            return Ok(None);
        };
        if tuple.values.len() != 2 {
            return Ok(None);
        }

        let ptr_handle = match &tuple.values[0] {
            Value::UInt(value) => value.value as usize,
            Value::Int(value) if value.value >= 0 => value.value as usize,
            Value::Null(_) => {
                return Ok(Some(Value::Bytes(fp_core::ast::ValueBytes::from(
                    &b"\0"[..],
                ))))
            }
            _ => return Ok(None),
        };
        let len = match &tuple.values[1] {
            Value::UInt(value) => value.value as usize,
            Value::Int(value) if value.value >= 0 => value.value as usize,
            _ => return Ok(None),
        };

        let Some(backing) = self.state.objects.get(ptr_handle) else {
            return Err(VmError::Runtime(format!(
                "dangling object handle {ptr_handle}"
            )));
        };
        let bytes = match backing {
            Value::Bytes(bytes) => bytes.value.as_ref(),
            Value::String(text) => text.value.as_bytes(),
            _ => return Ok(None),
        };
        let clipped_len = len.min(bytes.len());
        let mut out = Vec::with_capacity(clipped_len.saturating_add(1));
        out.extend_from_slice(&bytes[..clipped_len]);
        if !out.ends_with(&[0]) {
            out.push(0);
        }
        Ok(Some(Value::Bytes(fp_core::ast::ValueBytes::from(
            out.as_slice(),
        ))))
    }

    fn value_to_slot_raw(&mut self, value: Value, ty: &LirType) -> u64 {
        if Self::is_aggregate_runtime_type(ty) {
            let handle = self.state.objects.len() as u64;
            self.state.objects.push(value);
            return handle;
        }
        if matches!(ty, LirType::Ptr(_)) {
            return match value {
                Value::String(_)
                | Value::List(_)
                | Value::Tuple(_)
                | Value::Struct(_)
                | Value::Structural(_)
                | Value::Bytes(_)
                | Value::Pointer(_) => {
                    let handle = self.state.objects.len() as u64;
                    self.state.objects.push(value);
                    handle
                }
                Value::Null(_) | Value::Undefined(_) => 0,
                other => value_to_raw(&other),
            };
        }
        value_to_raw(&value)
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
                LirType::Struct { fields, .. } => {
                    fields
                        .get(*index as usize)
                        .cloned()
                        .ok_or(VmError::Runtime(format!(
                            "struct index {index} out of range"
                        )))?
                }
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
        matches!(
            ty,
            LirType::Struct { .. } | LirType::Array(..) | LirType::Vector(..)
        )
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

                // Try FFI dispatch for extern C functions.
                if let Some(sig) = self.extern_sigs.get(name.as_str()) {
                    if let Some(ref mut ffi) = self.ffi {
                        match ffi.call(name, sig, &raws) {
                            Ok(Some(ret)) => {
                                self.wr(dst, ret);
                                return Ok(());
                            }
                            Ok(None) => {
                                self.wr(dst, 0);
                                return Ok(());
                            }
                            Err(e) => {
                                // FFI call failed — fall through to intrinsic
                                // stubs so comptime evaluation keeps working.
                                eprintln!("ffi call '{name}' failed: {e}");
                            }
                        }
                    }
                }

                let r = self.call_intrinsic(name, &raws)?;
                self.wr(dst, r);
                Ok(())
            }
            _ => Err(VmError::Runtime("indirect call".into())),
        }
    }

    fn call_intrinsic(&mut self, name: &str, args: &[u64]) -> LirResult<u64> {
        if let Some(rest) = name.strip_prefix("__bc_") {
            return self.call_bc_intrinsic(rest, args);
        }
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

    fn call_bc_intrinsic(&mut self, name: &str, args: &[u64]) -> LirResult<u64> {
        match name {
            "make_tuple" | "make_array" | "make_list" => {
                let count = args.first().copied().unwrap_or(0) as usize;
                let elements: Vec<Value> = args[1..]
                    .iter()
                    .take(count)
                    .map(|&raw| Value::uint(raw))
                    .collect();
                let obj = match name {
                    "make_tuple" => Value::Tuple(ValueTuple::new(elements)),
                    _ => Value::List(ValueList::new(elements)),
                };
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            "make_map" => {
                let count = args.first().copied().unwrap_or(0) as usize;
                let mut entries = Vec::with_capacity(count);
                let mut i = 1;
                for _ in 0..count {
                    let key_raw = args.get(i).copied().unwrap_or(0);
                    let val_raw = args.get(i + 1).copied().unwrap_or(0);
                    entries.push(ValueMapEntry::new(
                        Value::uint(key_raw),
                        Value::uint(val_raw),
                    ));
                    i += 2;
                }
                let obj = Value::Map(fp_core::ast::ValueMap { entries });
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(obj);
                Ok(handle)
            }
            "tuple_get" | "array_get" => {
                let handle = args.first().copied().unwrap_or(0) as usize;
                let index = args.get(1).copied().unwrap_or(0) as usize;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let element = match &obj {
                    Value::Tuple(t) => t.values.get(index).cloned(),
                    Value::List(l) => l.values.get(index).cloned(),
                    _ => return Err(VmError::Runtime("get on non-container".into())),
                }
                .ok_or(VmError::Runtime(format!("index {index} out of bounds")))?;
                Ok(self.value_to_handle_or_raw(&element))
            }
            "tuple_set" => {
                let handle = args.first().copied().unwrap_or(0) as usize;
                let index = args.get(1).copied().unwrap_or(0) as usize;
                let raw_value = args.get(2).copied().unwrap_or(0);
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let mut values = match &obj {
                    Value::Tuple(t) => t.values.clone(),
                    _ => return Err(VmError::Runtime("set on non-tuple".into())),
                };
                if index >= values.len() {
                    return Err(VmError::Runtime(format!("index {index} out of bounds")));
                }
                values[index] = Value::uint(raw_value);
                let new_handle = self.state.objects.len() as u64;
                self.state
                    .objects
                    .push(Value::Tuple(ValueTuple::new(values)));
                Ok(new_handle)
            }
            "container_len" => {
                let handle = args.first().copied().unwrap_or(0) as usize;
                let obj = self
                    .state
                    .objects
                    .get(handle)
                    .cloned()
                    .ok_or(VmError::Runtime(format!("dangling handle {handle}")))?;
                let len = match &obj {
                    Value::Tuple(t) => t.values.len() as u64,
                    Value::List(l) => l.values.len() as u64,
                    Value::Map(m) => m.entries.len() as u64,
                    Value::String(s) => s.value.len() as u64,
                    _ => return Err(VmError::Runtime("len on non-container".into())),
                };
                Ok(len)
            }
            "str_alloc" => {
                let len = args.first().copied().unwrap_or(0) as usize;
                let s = " ".repeat(len);
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(Value::string(s));
                Ok(handle)
            }
            "println" => {
                for &raw in args {
                    let val = self.raw_to_value(raw);
                    print!("{}", self.render_value(&val));
                }
                println!();
                Ok(0)
            }
            "print" => {
                for &raw in args {
                    let val = self.raw_to_value(raw);
                    print!("{}", self.render_value(&val));
                }
                Ok(0)
            }
            "format" => {
                let mut result = String::new();
                for &raw in args {
                    let val = self.raw_to_value(raw);
                    result.push_str(&self.render_value(&val));
                }
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(Value::string(result));
                Ok(handle)
            }
            "time_now" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let dur = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default();
                Ok(dur.as_secs_f64().to_bits())
            }
            _ => Err(VmError::Runtime(format!("unknown bc intrinsic: {name}"))),
        }
    }

    fn raw_to_value(&self, raw: u64) -> Value {
        Value::uint(raw)
    }

    fn value_to_handle_or_raw(&mut self, v: &Value) -> u64 {
        match v {
            Value::Int(i) => i.value as u64,
            Value::UInt(u) => u.value,
            Value::Bool(b) => {
                if b.value {
                    1
                } else {
                    0
                }
            }
            Value::Decimal(d) => d.value.to_bits(),
            _ => {
                let handle = self.state.objects.len() as u64;
                self.state.objects.push(v.clone());
                handle
            }
        }
    }

    fn render_value(&self, v: &Value) -> String {
        match v {
            Value::Unit(_) => "()".to_string(),
            Value::Bool(b) => b.value.to_string(),
            Value::Int(i) => i.value.to_string(),
            Value::UInt(u) => u.value.to_string(),
            Value::Decimal(d) => d.value.to_string(),
            Value::String(s) => s.value.clone(),
            Value::Null(_) => "null".to_string(),
            Value::Tuple(t) => {
                let items: Vec<String> = t.values.iter().map(|x| self.render_value(x)).collect();
                format!("({})", items.join(", "))
            }
            Value::List(l) => {
                let items: Vec<String> = l.values.iter().map(|x| self.render_value(x)).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Map(m) => {
                let items: Vec<String> = m
                    .entries
                    .iter()
                    .map(|e| {
                        format!(
                            "{}: {}",
                            self.render_value(&e.key),
                            self.render_value(&e.value)
                        )
                    })
                    .collect();
                format!("{{{}}}", items.join(", "))
            }
            _ => format!("{v:?}"),
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
        LirConstant::Bytes(bytes) => LirType::Array(Box::new(LirType::I8), bytes.len() as u64),
    }
}

impl Default for LirInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a LIR function signature to an FFI signature.
fn lir_sig_to_ffi(sig: &fp_core::lir::LirFunctionSignature) -> FfiSignature {
    let args = sig.params.iter().map(lir_ty_to_ffi).collect();
    let ret = lir_ty_to_ffi(&sig.return_type);
    FfiSignature { args, ret }
}

fn lir_ty_to_ffi(ty: &LirType) -> FfiType {
    match ty {
        LirType::Ptr(_) => FfiType::Ptr,
        LirType::Void => FfiType::Void,
        // All scalar types pass through 64-bit registers.
        _ => FfiType::U64,
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
            comptime_entries: vec![],
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
                    i(10, LirInstructionKind::Mul(int(5), int(4))),
                    i(11, LirInstructionKind::Mul(reg(10), int(3))),
                    i(12, LirInstructionKind::Mul(reg(11), int(2))),
                    i(13, LirInstructionKind::Mul(reg(12), int(1))),
                ],
                ret(reg(13)),
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
                        id: 10,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: LirValue::Constant(LirConstant::Undef(slice_ty.clone())),
                            element: LirValue::Constant(LirConstant::UInt(0x1234, LirType::I64)),
                            indices: vec![0],
                        },
                        type_hint: Some(slice_ty.clone()),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 11,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: reg(10),
                            element: int(5),
                            indices: vec![1],
                        },
                        type_hint: Some(slice_ty),
                        debug_info: None,
                    },
                    i(
                        12,
                        LirInstructionKind::ExtractValue {
                            aggregate: reg(11),
                            indices: vec![1],
                        },
                    ),
                ],
                ret(reg(12)),
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
                        id: 10,
                        kind: LirInstructionKind::InsertValue {
                            aggregate: LirValue::Constant(LirConstant::Undef(array_ty.clone())),
                            element: LirValue::Constant(LirConstant::String("abc".into())),
                            indices: vec![0],
                        },
                        type_hint: Some(array_ty),
                        debug_info: None,
                    },
                    LirInstruction {
                        id: 11,
                        kind: LirInstructionKind::ExtractValue {
                            aggregate: reg(10),
                            indices: vec![0],
                        },
                        type_hint: Some(LirType::Ptr(Box::new(LirType::I8))),
                        debug_info: None,
                    },
                ],
                ret(reg(11)),
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
