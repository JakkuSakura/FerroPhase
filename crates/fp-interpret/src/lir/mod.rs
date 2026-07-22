pub mod vm;

use std::collections::HashMap;

use fp_core::ast::Value;
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstruction, LirInstructionKind,
    LirProgram, LirTerminator, LirType, LirValue, RegisterId,
};

use vm::{
    is_object_type, lir_type_info, mem_load, mem_store, raw_to_value, value_to_raw, ThreadState,
    VmError,
};

type LirResult<T> = Result<T, VmError>;

pub struct LirInterpreter {
    state: ThreadState,
}

impl LirInterpreter {
    pub fn new() -> Self {
        Self {
            state: ThreadState::new(),
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
        match &instr.kind {
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
            LirInstructionKind::ExtractValue { aggregate, .. } => self.unary(dst, aggregate, |x| x),
            LirInstructionKind::InsertValue { .. } => {
                Err(VmError::Runtime("InsertValue not supported".into()))
            }
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
        }
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
            _ => LirType::I64,
        }
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
                bb(1, vec![], ret(int(42))),
                bb(2, vec![], ret(int(99))),
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
            Value::int(42)
        );
    }
    #[test]
    fn cond_br_false() {
        assert_eq!(
            LirInterpreter::new().run_main(&cond_br_f(false)).unwrap(),
            Value::int(99)
        );
    }
}
