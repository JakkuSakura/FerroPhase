use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirGlobal,
    LirInstruction, LirInstructionKind, LirIntrinsicKind, LirProgram, LirTerminator, LirType,
    LirValue, RegisterId,
};
use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, MemArg, MemorySection,
    MemoryType, Module, TypeSection, ValType,
};

const STACK_ALIGN: u64 = 8;
const TIME_STEP_SECS: f64 = 0.01;
const PRINT_BUF_SIZE: i64 = 64;
const IOVEC_SIZE: i64 = 16;

pub fn emit_wasm(program: &LirProgram) -> Result<Vec<u8>> {
    let mut emitter = WasmEmitter::new(program);
    emitter.emit_module()
}

struct WasmEmitter<'a> {
    program: &'a LirProgram,
    func_index: HashMap<String, u32>,
    type_index: HashMap<SignatureKey, u32>,
    extern_funcs: HashMap<String, SignatureKey>,
    global_addr: HashMap<String, u64>,
    data_bytes: Vec<u8>,
    fd_write_index: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SignatureKey {
    params: Vec<ValType>,
    results: Vec<ValType>,
}

impl<'a> WasmEmitter<'a> {
    fn new(program: &'a LirProgram) -> Self {
        Self {
            program,
            func_index: HashMap::new(),
            type_index: HashMap::new(),
            extern_funcs: HashMap::new(),
            global_addr: HashMap::new(),
            data_bytes: Vec::new(),
            fd_write_index: None,
        }
    }

    fn emit_module(&mut self) -> Result<Vec<u8>> {
        let mut module = Module::new();
        let mut types = TypeSection::new();
        let mut imports = ImportSection::new();
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();
        let mut globals = GlobalSection::new();
        let mut memory = MemorySection::new();
        let mut data = DataSection::new();

        let fd_write_sig = SignatureKey {
            params: vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            results: vec![ValType::I32],
        };
        let fd_write_type = self.register_type(&mut types, fd_write_sig);
        imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(fd_write_type),
        );
        let import_func_count = 1_u32;
        self.fd_write_index = Some(0);

        self.func_index.clear();
        for (idx, func) in self.program.functions.iter().enumerate() {
            self.func_index
                .insert(func.name.as_str().to_string(), import_func_count + idx as u32);
        }

        self.collect_external_functions();

        let stack_ptr_global = self.emit_globals(&mut globals)?;

        for func in &self.program.functions {
            let signature = self.lower_signature(&func.signature);
            let type_idx = self.register_type(&mut types, signature);
            functions.function(type_idx);
        }
        let externs = self.extern_funcs.clone();
        for (name, signature) in externs {
            let type_idx = self.register_type(&mut types, signature);
            let func_idx = functions.len();
            functions.function(type_idx);
            self.func_index
                .insert(name, import_func_count + func_idx as u32);
        }

        for func in &self.program.functions {
            let mut fn_emitter = FunctionEmitter::new(self, func, stack_ptr_global);
            let wasm_fn = fn_emitter.emit_function()?;
            code.function(&wasm_fn);
        }
        for (_name, signature) in self.extern_funcs.iter() {
            let mut wasm_fn = Function::new(Vec::new());
            if let Some(result) = signature.results.first() {
                match result {
                    ValType::I32 => wasm_fn.instruction(&Instruction::I32Const(0)),
                    ValType::I64 => wasm_fn.instruction(&Instruction::I64Const(0)),
                    ValType::F32 => wasm_fn.instruction(&Instruction::F32Const(0.0)),
                    ValType::F64 => wasm_fn.instruction(&Instruction::F64Const(0.0)),
                    _ => wasm_fn.instruction(&Instruction::I32Const(0)),
                };
            }
            wasm_fn.instruction(&Instruction::Return);
            wasm_fn.instruction(&Instruction::End);
            code.function(&wasm_fn);
        }

        self.emit_memory(&mut memory, &mut data)?;

        if let Some(main_idx) = self.func_index.get("main").copied() {
            exports.export("main", ExportKind::Func, main_idx);
        }
        exports.export("memory", ExportKind::Memory, 0);

        module.section(&types);
        module.section(&imports);
        module.section(&functions);
        module.section(&memory);
        module.section(&globals);
        module.section(&exports);
        module.section(&code);
        module.section(&data);

        Ok(module.finish())
    }

    fn collect_external_functions(&mut self) {
        for func in &self.program.functions {
            let mut reg_types = HashMap::new();
            for block in &func.basic_blocks {
                for instr in &block.instructions {
                    if let Some(ty) = instr.type_hint.clone() {
                        reg_types.insert(instr.id, ty);
                    }
                }
            }
            for block in &func.basic_blocks {
                for instr in &block.instructions {
                    if let LirInstructionKind::Call { function, args, .. } = &instr.kind {
                        let LirValue::Function(name) = function else {
                            continue;
                        };
                        if self.func_index.contains_key(name) || self.extern_funcs.contains_key(name)
                        {
                            continue;
                        }
                        let params = args
                            .iter()
                            .map(|arg| {
                                lower_val_type(&value_type_for(func, &reg_types, arg))
                            })
                            .collect::<Vec<_>>();
                        let results = instr
                            .type_hint
                            .clone()
                            .map(|ty| vec![lower_val_type(&ty)])
                            .unwrap_or_default();
                        self.extern_funcs.insert(
                            name.clone(),
                            SignatureKey { params, results },
                        );
                    }
                }
            }
        }
    }

    fn emit_globals(&mut self, globals: &mut GlobalSection) -> Result<u32> {
        self.build_global_data()?;
        let stack_base = align_to(self.data_bytes.len() as u64, STACK_ALIGN);
        let stack_ptr_idx = globals.len();
        globals.global(
            GlobalType {
                val_type: ValType::I64,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i64_const(stack_base as i64),
        );
        let time_idx = globals.len();
        globals.global(
            GlobalType {
                val_type: ValType::F64,
                mutable: true,
                shared: false,
            },
            &ConstExpr::f64_const(0.0),
        );
        self.global_addr.insert("__fp_stack_ptr".to_string(), stack_base);
        self.global_addr
            .insert("__fp_time_now".to_string(), time_idx as u64);
        Ok(stack_ptr_idx as u32)
    }

    fn emit_memory(&mut self, memory: &mut MemorySection, data: &mut DataSection) -> Result<()> {
        let total_size = align_to(self.data_bytes.len() as u64 + 65536, 65536);
        let pages = ((total_size as f64) / 65536.0).ceil() as u64;
        memory.memory(MemoryType {
            minimum: pages.max(1),
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        if !self.data_bytes.is_empty() {
            data.active(0, &ConstExpr::i32_const(0), self.data_bytes.clone());
        }
        Ok(())
    }

    fn build_global_data(&mut self) -> Result<()> {
        for global in &self.program.globals {
            self.register_global(global)?;
        }
        Ok(())
    }

    fn register_global(&mut self, global: &LirGlobal) -> Result<()> {
        let align = global.alignment.unwrap_or(8) as u64;
        let offset = align_to(self.data_bytes.len() as u64, align);
        if offset > self.data_bytes.len() as u64 {
            self.data_bytes
                .extend(std::iter::repeat(0).take((offset - self.data_bytes.len() as u64) as usize));
        }

        if let Some(init) = global.initializer.clone() {
            let bytes = self.encode_constant_bytes(&init, &global.ty)?;
            self.data_bytes.extend(bytes);
        } else {
            let size = size_of_type(&global.ty);
            self.data_bytes
                .extend(std::iter::repeat(0).take(size as usize));
        }

        self.global_addr
            .insert(global.name.as_str().to_string(), offset);
        Ok(())
    }

    fn encode_constant_bytes(&mut self, constant: &LirConstant, ty: &LirType) -> Result<Vec<u8>> {
        match constant {
            LirConstant::Int(value, ty) => Ok(int_to_bytes(*value as u128, ty)),
            LirConstant::UInt(value, ty) => Ok(int_to_bytes(*value as u128, ty)),
            LirConstant::Float(value, ty) => Ok(float_to_bytes(*value, ty)),
            LirConstant::Bool(value) => Ok(vec![*value as u8]),
            LirConstant::String(value) => {
                let ptr = self.write_string_data(value);
                Ok(int_to_bytes(ptr as u128, ty))
            }
            LirConstant::Array(elements, elem_ty) => {
                let mut out = Vec::new();
                for elem in elements {
                    let bytes = self.encode_constant_bytes(elem, elem_ty)?;
                    out.extend(bytes);
                }
                Ok(out)
            }
            LirConstant::Struct(values, struct_ty) => {
                let LirType::Struct { fields, .. } = struct_ty else {
                    return Err(Error::from("struct constant expects struct type"));
                };
                let mut out = Vec::new();
                for (value, field_ty) in values.iter().zip(fields.iter()) {
                    let bytes = self.encode_constant_bytes(value, field_ty)?;
                    out.extend(bytes);
                }
                Ok(out)
            }
            LirConstant::GlobalRef(name, ty, indices) => {
                let base = self
                    .global_addr
                    .get(name.as_str())
                    .copied()
                    .unwrap_or(0);
                let offset = offset_for_indices(
                    ty,
                    &indices.iter().map(|v| *v as u32).collect::<Vec<_>>(),
                );
                Ok(int_to_bytes((base + offset) as u128, ty))
            }
            LirConstant::FunctionRef(_, ty) => Ok(int_to_bytes(0, ty)),
            LirConstant::Null(ty) | LirConstant::Undef(ty) => {
                Ok(vec![0; size_of_type(ty) as usize])
            }
        }
    }

    fn write_string_data(&mut self, value: &str) -> u64 {
        let offset = self.data_bytes.len() as u64;
        self.data_bytes.extend(value.as_bytes());
        self.data_bytes.push(0);
        offset
    }

    fn register_type(&mut self, section: &mut TypeSection, signature: SignatureKey) -> u32 {
        if let Some(idx) = self.type_index.get(&signature) {
            return *idx;
        }
        let idx = section.len();
        section.function(signature.params.clone(), signature.results.clone());
        self.type_index.insert(signature, idx);
        idx
    }

    fn lower_signature(&self, signature: &LirFunctionSignature) -> SignatureKey {
        let params = signature
            .params
            .iter()
            .map(|ty| lower_val_type(ty))
            .collect::<Vec<_>>();
        let results = if matches!(signature.return_type, LirType::Void) {
            Vec::new()
        } else {
            vec![lower_val_type(&signature.return_type)]
        };
        SignatureKey { params, results }
    }
}

struct FunctionEmitter<'a, 'b> {
    emitter: &'a mut WasmEmitter<'b>,
    func: &'a LirFunction,
    register_types: HashMap<RegisterId, LirType>,
    locals_map: HashMap<u32, u32>,
    register_map: HashMap<RegisterId, u32>,
    stack_slots: HashMap<u32, u64>,
    phi_nodes: HashMap<BasicBlockId, Vec<PhiNode>>,
    struct_load_sources: HashMap<RegisterId, LirValue>,
    next_local: u32,
    bb_local: u32,
    sp_local: u32,
    sp_base_local: u32,
    time_tmp_local: u32,
    tmp_i64_local: u32,
    tmp_i64_local2: u32,
    tmp_i64_local3: u32,
    tmp_i64_local4: u32,
    tmp_i32_local: u32,
    tmp_i32_local2: u32,
    ret_local: Option<u32>,
    stack_ptr_global: u32,
    loop_depth: u32,
}

impl<'a, 'b> FunctionEmitter<'a, 'b> {
    fn new(
        emitter: &'a mut WasmEmitter<'b>,
        func: &'a LirFunction,
        stack_ptr_global: u32,
    ) -> Self {
        Self {
            emitter,
            func,
            register_types: HashMap::new(),
            locals_map: HashMap::new(),
            register_map: HashMap::new(),
            stack_slots: HashMap::new(),
            phi_nodes: HashMap::new(),
            struct_load_sources: HashMap::new(),
            next_local: 0,
            bb_local: 0,
            sp_local: 0,
            sp_base_local: 0,
            time_tmp_local: 0,
            tmp_i64_local: 0,
            tmp_i64_local2: 0,
            tmp_i64_local3: 0,
            tmp_i64_local4: 0,
            tmp_i32_local: 0,
            tmp_i32_local2: 0,
            ret_local: None,
            stack_ptr_global,
            loop_depth: 0,
        }
    }

    fn emit_function(&mut self) -> Result<Function> {
        self.collect_register_types();
        self.collect_phi_nodes();
        self.collect_struct_load_sources();
        let mut locals = Vec::new();
        let params = self
            .func
            .signature
            .params
            .iter()
            .map(|ty| lower_val_type(ty))
            .collect::<Vec<_>>();

        self.next_local = params.len() as u32;

        let mut arg_idx = 0;
        for local in &self.func.locals {
            let wasm_ty = lower_val_type(&local.ty);
            if local.is_argument {
                self.locals_map.insert(local.id, arg_idx);
                arg_idx += 1;
            } else {
                self.locals_map.insert(local.id, self.next_local);
                locals.push((1, wasm_ty));
                self.next_local += 1;
            }
        }

        for (reg, ty) in self.register_types.clone() {
            let wasm_ty = lower_val_type(&ty);
            self.register_map.insert(reg, self.next_local);
            locals.push((1, wasm_ty));
            self.next_local += 1;
        }

        self.bb_local = self.next_local;
        locals.push((1, ValType::I32));
        self.next_local += 1;

        self.sp_base_local = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.sp_local = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.time_tmp_local = self.next_local;
        locals.push((1, ValType::F64));
        self.next_local += 1;

        self.tmp_i64_local = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.tmp_i64_local2 = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.tmp_i64_local3 = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.tmp_i64_local4 = self.next_local;
        locals.push((1, ValType::I64));
        self.next_local += 1;

        self.tmp_i32_local = self.next_local;
        locals.push((1, ValType::I32));
        self.next_local += 1;

        self.tmp_i32_local2 = self.next_local;
        locals.push((1, ValType::I32));
        self.next_local += 1;

        if !matches!(self.func.signature.return_type, LirType::Void) {
            let ret_ty = lower_val_type(&self.func.signature.return_type);
            self.ret_local = Some(self.next_local);
            locals.push((1, ret_ty));
            self.next_local += 1;
        }

        let mut func = Function::new(locals);

        self.emit_stack_slots(&mut func)?;
        self.emit_dispatch_loop(&mut func)?;
        if !matches!(self.func.signature.return_type, LirType::Void) {
            emit_zero_for_type(&mut func, &self.func.signature.return_type);
            func.instruction(&Instruction::Return);
        } else {
            func.instruction(&Instruction::Return);
        }
        func.instruction(&Instruction::End);

        Ok(func)
    }

    fn collect_register_types(&mut self) {
        for block in &self.func.basic_blocks {
            for instr in &block.instructions {
                if let Some(ty) = instr.type_hint.clone() {
                    self.register_types.insert(instr.id, ty);
                }
            }
        }
    }

    fn collect_phi_nodes(&mut self) {
        for block in &self.func.basic_blocks {
            let nodes = collect_phi_nodes(block);
            if !nodes.is_empty() {
                self.phi_nodes.insert(block.id, nodes);
            }
        }
    }

    fn collect_struct_load_sources(&mut self) {
        for block in &self.func.basic_blocks {
            for instr in &block.instructions {
                if let LirInstructionKind::Load { address, .. } = &instr.kind {
                    if let Some(LirType::Struct { .. }) = &instr.type_hint {
                        self.struct_load_sources
                            .insert(instr.id, address.clone());
                    }
                }
            }
        }
    }


    fn emit_stack_slots(&mut self, func: &mut Function) -> Result<()> {
        func.instruction(&Instruction::GlobalGet(self.stack_ptr_global));
        func.instruction(&Instruction::LocalSet(self.sp_base_local));
        func.instruction(&Instruction::LocalGet(self.sp_base_local));
        func.instruction(&Instruction::LocalSet(self.sp_local));

        let mut offset: u64 = 0;
        for slot in &self.func.stack_slots {
            offset = align_to(offset, slot.alignment as u64);
            self.stack_slots.insert(slot.id, offset);
            offset += slot.size as u64;
        }

        if offset > 0 {
            func.instruction(&Instruction::LocalGet(self.sp_local));
            func.instruction(&Instruction::I64Const(offset as i64));
            func.instruction(&Instruction::I64Add);
            func.instruction(&Instruction::LocalSet(self.sp_local));
        }

        Ok(())
    }

    fn emit_dispatch_loop(&mut self, func: &mut Function) -> Result<()> {
        let blocks = self.func.basic_blocks.clone();
        let block_ids = blocks.iter().map(|bb| bb.id).collect::<Vec<_>>();
        let entry_idx = 0_i32;

        func.instruction(&Instruction::I32Const(entry_idx));
        func.instruction(&Instruction::LocalSet(self.bb_local));

        self.loop_depth = 1;
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

        for (idx, block) in blocks.iter().enumerate() {
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::LocalGet(self.bb_local));
            func.instruction(&Instruction::I32Const(idx as i32));
            func.instruction(&Instruction::I32Ne);
            func.instruction(&Instruction::BrIf(0));
            self.emit_block(func, block, block_ids[idx], &block_ids)?;
            func.instruction(&Instruction::Br(self.loop_depth));
            func.instruction(&Instruction::End);
        }

        func.instruction(&Instruction::Unreachable);
        func.instruction(&Instruction::End); // loop
        self.loop_depth = 0;

        Ok(())
    }

    fn emit_block(
        &mut self,
        func: &mut Function,
        block: &LirBasicBlock,
        block_id: BasicBlockId,
        block_ids: &[BasicBlockId],
    ) -> Result<()> {
        let phi_nodes = self
            .phi_nodes
            .get(&block_id)
            .cloned()
            .unwrap_or_default();
        for instr in &block.instructions {
            if matches!(instr.kind, LirInstructionKind::Phi { .. }) {
                continue;
            }
            self.emit_instruction(func, instr, block_id, &phi_nodes)?;
        }
        self.emit_terminator(func, &block.terminator, block_id, &phi_nodes, block_ids)
    }

    fn emit_instruction(
        &mut self,
        func: &mut Function,
        instr: &LirInstruction,
        block_id: BasicBlockId,
        phi_nodes: &[PhiNode],
    ) -> Result<()> {
        match &instr.kind {
            LirInstructionKind::Add(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Add)?
            }
            LirInstructionKind::Sub(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Sub)?
            }
            LirInstructionKind::Mul(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Mul)?
            }
            LirInstructionKind::Div(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Div)?
            }
            LirInstructionKind::Rem(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Rem)?
            }
            LirInstructionKind::And(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::And)?
            }
            LirInstructionKind::Or(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Or)?
            }
            LirInstructionKind::Xor(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Xor)?
            }
            LirInstructionKind::Shl(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Shl)?
            }
            LirInstructionKind::Shr(lhs, rhs) => {
                self.emit_binop(func, lhs, rhs, instr, BinOp::Shr)?
            }
            LirInstructionKind::Not(value) => self.emit_not(func, value, instr)?,
            LirInstructionKind::Eq(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Eq)?
            }
            LirInstructionKind::Ne(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Ne)?
            }
            LirInstructionKind::Lt(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Lt)?
            }
            LirInstructionKind::Le(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Le)?
            }
            LirInstructionKind::Gt(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Gt)?
            }
            LirInstructionKind::Ge(lhs, rhs) => {
                self.emit_cmp(func, lhs, rhs, instr, CmpOp::Ge)?
            }
            LirInstructionKind::Load { address, .. } => {
                self.emit_load(func, address, instr)?
            }
            LirInstructionKind::Store { value, address, .. } => {
                self.emit_store(func, value, address)?
            }
            LirInstructionKind::Alloca { size, alignment } => {
                self.emit_alloca(func, size, *alignment, instr)?
            }
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                self.emit_gep(func, ptr, indices, instr)?
            }
            LirInstructionKind::PtrToInt(value) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::IntToPtr(value) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::Trunc(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::ZExt(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::SExt(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::FPTrunc(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::FPExt(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::FPToUI(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::FPToSI(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::UIToFP(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::SIToFP(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::Bitcast(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::ExtractValue { aggregate, indices } => {
                self.emit_extract(func, aggregate, indices, instr)?
            }
            LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => self.emit_insert(func, aggregate, element, indices, instr)?,
            LirInstructionKind::Call { function, args, .. } => {
                self.emit_call(func, function, args, instr)?
            }
            LirInstructionKind::IntrinsicCall { kind, format, args } => {
                self.emit_intrinsic(func, kind, format, args, instr)?
            }
            LirInstructionKind::SextOrTrunc(value, _) => self.emit_cast(func, value, instr)?,
            LirInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => self.emit_select(func, condition, if_true, if_false, instr)?,
            LirInstructionKind::Phi { .. } => {}
            LirInstructionKind::Unreachable | LirInstructionKind::Freeze(_) => {
                func.instruction(&Instruction::Unreachable);
            }
            LirInstructionKind::InlineAsm { .. }
            | LirInstructionKind::LandingPad { .. } => {
                func.instruction(&Instruction::Unreachable);
            }
        }

        self.flush_phi_assignments(func, block_id, phi_nodes)?;
        Ok(())
    }

    fn emit_terminator(
        &mut self,
        func: &mut Function,
        terminator: &LirTerminator,
        block_id: BasicBlockId,
        phi_nodes: &[PhiNode],
        block_ids: &[BasicBlockId],
    ) -> Result<()> {
        match terminator {
            LirTerminator::Return(value) => {
                if let Some(value) = value {
                    self.emit_value(func, value)?;
                    let ret_local = self.ret_local.unwrap_or(self.bb_local);
                    func.instruction(&Instruction::LocalSet(ret_local));
                    func.instruction(&Instruction::LocalGet(self.sp_base_local));
                    func.instruction(&Instruction::GlobalSet(self.stack_ptr_global));
                    func.instruction(&Instruction::LocalGet(ret_local));
                    func.instruction(&Instruction::Return);
                    return Ok(());
                }
                func.instruction(&Instruction::LocalGet(self.sp_base_local));
                func.instruction(&Instruction::GlobalSet(self.stack_ptr_global));
                func.instruction(&Instruction::Return);
            }
            LirTerminator::Br(dest) => {
                self.emit_phi_for_branch(func, block_id, *dest, phi_nodes)?;
                self.set_next_block(func, *dest, block_ids)?;
            }
            LirTerminator::CondBr {
                condition,
                if_true,
                if_false,
            } => {
                self.emit_condition_as_i32(func, condition)?;
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                self.emit_phi_for_branch(func, block_id, *if_true, phi_nodes)?;
                self.set_next_block(func, *if_true, block_ids)?;
                func.instruction(&Instruction::Else);
                self.emit_phi_for_branch(func, block_id, *if_false, phi_nodes)?;
                self.set_next_block(func, *if_false, block_ids)?;
                func.instruction(&Instruction::End);
            }
            LirTerminator::Switch {
                value,
                default,
                cases,
            } => {
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                for (case_value, target) in cases {
                    self.emit_value(func, value)?;
                    self.emit_switch_case_eq(func, &self.value_type(value), *case_value);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                    self.emit_phi_for_branch(func, block_id, *target, phi_nodes)?;
                    self.set_next_block(func, *target, block_ids)?;
                    func.instruction(&Instruction::Br(1));
                    func.instruction(&Instruction::End);
                }
                self.emit_phi_for_branch(func, block_id, *default, phi_nodes)?;
                self.set_next_block(func, *default, block_ids)?;
                func.instruction(&Instruction::End);
            }
            LirTerminator::Invoke {
                function,
                args,
                normal_dest,
                ..
            } => {
                let dummy = LirInstruction {
                    id: 0,
                    kind: LirInstructionKind::Unreachable,
                    type_hint: None,
                    debug_info: None,
                };
                self.emit_call(func, function, args, &dummy)?;
                self.emit_phi_for_branch(func, block_id, *normal_dest, phi_nodes)?;
                self.set_next_block(func, *normal_dest, block_ids)?;
            }
            LirTerminator::Unreachable
            | LirTerminator::IndirectBr { .. }
            | LirTerminator::Resume(_)
            | LirTerminator::CleanupRet { .. }
            | LirTerminator::CatchRet { .. }
            | LirTerminator::CatchSwitch { .. } => {
                func.instruction(&Instruction::Unreachable);
            }
        }
        Ok(())
    }

    fn set_next_block(
        &mut self,
        func: &mut Function,
        dest: BasicBlockId,
        block_ids: &[BasicBlockId],
    ) -> Result<()> {
        let idx = block_index(dest, block_ids)?;
        func.instruction(&Instruction::I32Const(idx));
        func.instruction(&Instruction::LocalSet(self.bb_local));
        Ok(())
    }

    fn emit_condition_as_i32(
        &mut self,
        func: &mut Function,
        condition: &LirValue,
    ) -> Result<()> {
        let ty = self.value_type(condition);
        self.emit_value(func, condition)?;
        match ty {
            LirType::I1 | LirType::I8 | LirType::I16 | LirType::I32 => {
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Ne);
            }
            LirType::F32 => {
                func.instruction(&Instruction::F32Const(0.0));
                func.instruction(&Instruction::F32Ne);
            }
            LirType::F64 => {
                func.instruction(&Instruction::F64Const(0.0));
                func.instruction(&Instruction::F64Ne);
            }
            _ => {
                func.instruction(&Instruction::I64Const(0));
                func.instruction(&Instruction::I64Ne);
            }
        }
        Ok(())
    }

    fn emit_switch_case_eq(
        &mut self,
        func: &mut Function,
        ty: &LirType,
        value: u64,
    ) {
        match ty {
            LirType::I1 | LirType::I8 | LirType::I16 | LirType::I32 => {
                func.instruction(&Instruction::I32Const(value as i32));
                func.instruction(&Instruction::I32Eq);
            }
            LirType::F32 => {
                func.instruction(&Instruction::F32Const(value as f32));
                func.instruction(&Instruction::F32Eq);
            }
            LirType::F64 => {
                func.instruction(&Instruction::F64Const(value as f64));
                func.instruction(&Instruction::F64Eq);
            }
            _ => {
                func.instruction(&Instruction::I64Const(value as i64));
                func.instruction(&Instruction::I64Eq);
            }
        }
    }

    fn emit_phi_for_branch(
        &mut self,
        func: &mut Function,
        from_block: BasicBlockId,
        dest: BasicBlockId,
        _phi_nodes: &[PhiNode],
    ) -> Result<()> {
        let nodes = self.phi_nodes.get(&dest).cloned().unwrap_or_default();
        for phi in nodes {
            if let Some(value) = phi
                .incoming
                .iter()
                .find(|(_, pred)| *pred == from_block)
                .map(|(val, _)| val)
            {
                self.emit_value(func, value)?;
                let local = self.register_map.get(&phi.result).copied().unwrap_or(0);
                func.instruction(&Instruction::LocalSet(local));
            }
        }
        Ok(())
    }

    fn emit_binop(
        &mut self,
        func: &mut Function,
        lhs: &LirValue,
        rhs: &LirValue,
        instr: &LirInstruction,
        op: BinOp,
    ) -> Result<()> {
        let ty = instr.type_hint.clone().unwrap_or(LirType::I64);
        self.emit_value(func, lhs)?;
        self.emit_value(func, rhs)?;
        match lower_val_type(&ty) {
            ValType::I32 => emit_int_binop(func, op, false),
            ValType::I64 => emit_int_binop(func, op, true),
            ValType::F32 => emit_float_binop(func, op, false),
            ValType::F64 => emit_float_binop(func, op, true),
            _ => emit_int_binop(func, op, true),
        }
        self.store_result(func, instr.id, &ty);
        Ok(())
    }

    fn emit_not(&mut self, func: &mut Function, value: &LirValue, instr: &LirInstruction) -> Result<()> {
        let ty = instr.type_hint.clone().unwrap_or(LirType::I1);
        self.emit_value(func, value)?;
        match lower_val_type(&ty) {
            ValType::I32 => {
                if matches!(ty, LirType::I1) {
                    func.instruction(&Instruction::I32Eqz);
                } else {
                    func.instruction(&Instruction::I32Const(-1));
                    func.instruction(&Instruction::I32Xor);
                }
            }
            ValType::I64 => {
                func.instruction(&Instruction::I64Const(-1));
                func.instruction(&Instruction::I64Xor);
            }
            _ => {
                func.instruction(&Instruction::I32Eqz);
            }
        }
        self.store_result(func, instr.id, &ty);
        Ok(())
    }

    fn emit_cmp(
        &mut self,
        func: &mut Function,
        lhs: &LirValue,
        rhs: &LirValue,
        instr: &LirInstruction,
        op: CmpOp,
    ) -> Result<()> {
        let lhs_ty = self.value_type(lhs);
        let rhs_ty = self.value_type(rhs);
        let is_float = matches!(lhs_ty, LirType::F32 | LirType::F64)
            || matches!(rhs_ty, LirType::F32 | LirType::F64);

        self.emit_value(func, lhs)?;
        self.emit_value(func, rhs)?;

        if is_float {
            emit_float_cmp(func, op, matches!(lhs_ty, LirType::F32));
        } else {
            emit_int_cmp(func, op, matches!(lhs_ty, LirType::I64));
        }
        self.store_result(func, instr.id, &LirType::I1);
        Ok(())
    }

    fn emit_load(&mut self, func: &mut Function, address: &LirValue, instr: &LirInstruction) -> Result<()> {
        let ty = instr.type_hint.clone().unwrap_or(LirType::I64);
        self.emit_value(func, address)?;
        func.instruction(&Instruction::I32WrapI64);
        emit_load_for_type(func, &ty);
        self.store_result(func, instr.id, &ty);
        Ok(())
    }

    fn emit_store(&mut self, func: &mut Function, value: &LirValue, address: &LirValue) -> Result<()> {
        let ty = self.value_type(value);
        self.emit_value(func, address)?;
        func.instruction(&Instruction::I32WrapI64);
        self.emit_value(func, value)?;
        emit_store_for_type(func, &ty);
        Ok(())
    }

    fn emit_alloca(
        &mut self,
        func: &mut Function,
        size: &LirValue,
        alignment: u32,
        instr: &LirInstruction,
    ) -> Result<()> {
        let align = alignment.max(1) as i64;
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::I64Const((align - 1) as i64));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::I64Const(!((align - 1) as i64)));
        func.instruction(&Instruction::I64And);
        func.instruction(&Instruction::LocalTee(self.sp_local));
        self.store_result(func, instr.id, &LirType::Ptr(Box::new(LirType::I8)));
        func.instruction(&Instruction::LocalGet(self.sp_local));
        self.emit_value(func, size)?;
        if matches!(lower_val_type(&self.value_type(size)), ValType::I32) {
            func.instruction(&Instruction::I64ExtendI32S);
        }
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::LocalSet(self.sp_local));
        Ok(())
    }

    fn emit_gep(
        &mut self,
        func: &mut Function,
        ptr: &LirValue,
        indices: &[LirValue],
        instr: &LirInstruction,
    ) -> Result<()> {
        let ty = instr.type_hint.clone().unwrap_or(LirType::Ptr(Box::new(LirType::I8)));
        let LirType::Ptr(inner) = ty.clone() else {
            return Err(Error::from("gep expects pointer type hint"));
        };
        self.emit_value(func, ptr)?;
        for index in indices {
            self.emit_value(func, index)?;
            if matches!(lower_val_type(&self.value_type(index)), ValType::I32) {
                func.instruction(&Instruction::I64ExtendI32S);
            }
            func.instruction(&Instruction::I64Const(size_of_type(&inner) as i64));
            func.instruction(&Instruction::I64Mul);
            func.instruction(&Instruction::I64Add);
        }
        self.store_result(func, instr.id, &ty);
        Ok(())
    }

    fn emit_cast(&mut self, func: &mut Function, value: &LirValue, instr: &LirInstruction) -> Result<()> {
        let target = instr.type_hint.clone().unwrap_or(LirType::I64);
        let source = self.value_type(value);
        self.emit_value(func, value)?;
        match (lower_val_type(&source), lower_val_type(&target)) {
            (ValType::I64, ValType::I32) => func.instruction(&Instruction::I32WrapI64),
            (ValType::I32, ValType::I64) => func.instruction(&Instruction::I64ExtendI32S),
            (ValType::I32, ValType::F32) => func.instruction(&Instruction::F32ConvertI32S),
            (ValType::I64, ValType::F64) => func.instruction(&Instruction::F64ConvertI64S),
            (ValType::F32, ValType::I32) => func.instruction(&Instruction::I32TruncF32S),
            (ValType::F64, ValType::I64) => func.instruction(&Instruction::I64TruncF64S),
            (ValType::F32, ValType::F64) => func.instruction(&Instruction::F64PromoteF32),
            (ValType::F64, ValType::F32) => func.instruction(&Instruction::F32DemoteF64),
            _ => return Ok(()),
        };
        self.store_result(func, instr.id, &target);
        Ok(())
    }

    fn emit_extract(
        &mut self,
        func: &mut Function,
        aggregate: &LirValue,
        indices: &[u32],
        instr: &LirInstruction,
    ) -> Result<()> {
        let agg_ty = self.value_type(aggregate);
        let offset = offset_for_indices(&agg_ty, indices);
        if let LirType::Struct { .. } = agg_ty {
            if let LirValue::Register(reg) = aggregate {
                if let Some(addr) = self.struct_load_sources.get(reg).cloned() {
                    self.emit_value(func, &addr)?;
                } else {
                    self.emit_value(func, aggregate)?;
                }
            } else {
                self.emit_value(func, aggregate)?;
            }
        } else {
            self.emit_value(func, aggregate)?;
        }
        func.instruction(&Instruction::I64Const(offset as i64));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::I32WrapI64);
        let field_ty = match agg_ty {
            LirType::Struct { fields, .. } => fields
                .get(indices.get(0).copied().unwrap_or(0) as usize)
                .cloned()
                .unwrap_or(LirType::I64),
            _ => LirType::I64,
        };
        emit_load_for_type(func, &field_ty);
        self.store_result(func, instr.id, &field_ty);
        Ok(())
    }

    fn emit_insert(
        &mut self,
        func: &mut Function,
        aggregate: &LirValue,
        element: &LirValue,
        indices: &[u32],
        instr: &LirInstruction,
    ) -> Result<()> {
        let agg_ty = instr
            .type_hint
            .clone()
            .unwrap_or(LirType::Ptr(Box::new(LirType::I8)));
        self.emit_value(func, aggregate)?;
        func.instruction(&Instruction::LocalTee(self.tmp_i64_local));
        let offset = offset_for_indices(&agg_ty, indices);
        func.instruction(&Instruction::I64Const(offset as i64));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::I32WrapI64);
        self.emit_value(func, element)?;
        emit_store_for_type(func, &self.value_type(element));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local));
        self.store_result(func, instr.id, &agg_ty);
        Ok(())
    }

    fn emit_call(
        &mut self,
        func: &mut Function,
        function: &LirValue,
        args: &[LirValue],
        instr: &LirInstruction,
    ) -> Result<()> {
        for arg in args {
            self.emit_value(func, arg)?;
        }
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::GlobalSet(self.stack_ptr_global));

        let name = match function {
            LirValue::Function(name) => name.clone(),
            _ => "".to_string(),
        };
        let idx = self
            .emitter
            .func_index
            .get(&name)
            .copied()
            .unwrap_or(0);
        func.instruction(&Instruction::Call(idx));

        if let Some(ty) = instr.type_hint.clone() {
            if !matches!(ty, LirType::Void) {
                self.store_result(func, instr.id, &ty);
            }
        }
        Ok(())
    }

    fn emit_intrinsic(
        &mut self,
        func: &mut Function,
        kind: &LirIntrinsicKind,
        format: &str,
        args: &[LirValue],
        instr: &LirInstruction,
    ) -> Result<()> {
        match kind {
            LirIntrinsicKind::TimeNow => {
                func.instruction(&Instruction::GlobalGet(self.stack_ptr_global + 1));
                func.instruction(&Instruction::LocalSet(self.time_tmp_local));
                func.instruction(&Instruction::LocalGet(self.time_tmp_local));
                func.instruction(&Instruction::F64Const(TIME_STEP_SECS));
                func.instruction(&Instruction::F64Add);
                func.instruction(&Instruction::GlobalSet(self.stack_ptr_global + 1));
                func.instruction(&Instruction::LocalGet(self.time_tmp_local));
                self.store_result(func, instr.id, &LirType::F64);
            }
            LirIntrinsicKind::Print | LirIntrinsicKind::Println => {
                self.emit_printf_like(func, format, args)?;
            }
            _ => {
                if let Some(ty) = instr.type_hint.clone() {
                    if !matches!(ty, LirType::Void) {
                        emit_zero_for_type(func, &ty);
                        self.store_result(func, instr.id, &ty);
                    }
                }
            }
        }
        Ok(())
    }

    fn emit_select(
        &mut self,
        func: &mut Function,
        condition: &LirValue,
        if_true: &LirValue,
        if_false: &LirValue,
        instr: &LirInstruction,
    ) -> Result<()> {
        let ty = instr.type_hint.clone().unwrap_or(LirType::I64);
        self.emit_value(func, condition)?;
        func.instruction(&Instruction::If(block_type_for(&ty)));
        self.emit_value(func, if_true)?;
        func.instruction(&Instruction::Else);
        self.emit_value(func, if_false)?;
        func.instruction(&Instruction::End);
        self.store_result(func, instr.id, &ty);
        Ok(())
    }

    fn emit_printf_like(
        &mut self,
        func: &mut Function,
        format: &str,
        args: &[LirValue],
    ) -> Result<()> {
        let parts = parse_printf_format(format)?;
        let mut arg_index = 0usize;
        for part in parts {
            match part {
                PrintPart::Literal(text) => {
                    if !text.is_empty() {
                        self.emit_print_literal(func, &text)?;
                    }
                }
                PrintPart::Spec(spec) => {
                    let Some(arg) = args.get(arg_index) else {
                        return Err(Error::from("printf format expects more arguments"));
                    };
                    arg_index += 1;
                    self.emit_print_arg(func, spec, arg)?;
                }
            }
        }
        Ok(())
    }

    fn emit_print_literal(&mut self, func: &mut Function, text: &str) -> Result<()> {
        let ptr = self.emitter.write_string_data(text) as i64;
        let len = text.len() as i64;
        func.instruction(&Instruction::I64Const(ptr));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
        func.instruction(&Instruction::I64Const(len));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local));
        self.emit_fd_write_from_tmp(func)?;
        Ok(())
    }

    fn emit_print_arg(
        &mut self,
        func: &mut Function,
        spec: PrintSpec,
        value: &LirValue,
    ) -> Result<()> {
        match spec.kind {
            PrintSpecKind::Signed => {
                self.emit_value(func, value)?;
                self.widen_i32_to_i64(func, value, true);
                func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
                self.emit_print_i64_from_tmp(func, true)?;
            }
            PrintSpecKind::Unsigned => {
                self.emit_value(func, value)?;
                self.widen_i32_to_i64(func, value, false);
                func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
                self.emit_print_i64_from_tmp(func, false)?;
            }
            PrintSpecKind::Float => {
                self.emit_value(func, value)?;
                func.instruction(&Instruction::LocalSet(self.time_tmp_local));
                self.emit_print_f64_from_tmp(func)?;
            }
            PrintSpecKind::Str => {
                self.emit_value(func, value)?;
                self.widen_i32_to_i64(func, value, false);
                func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
                self.emit_print_c_string_from_tmp(func)?;
            }
            PrintSpecKind::Char => {
                self.emit_value(func, value)?;
                self.emit_print_char(func, value)?;
            }
        }
        Ok(())
    }

    fn widen_i32_to_i64(&mut self, func: &mut Function, value: &LirValue, signed: bool) {
        if matches!(lower_val_type(&self.value_type(value)), ValType::I32) {
            if signed {
                func.instruction(&Instruction::I64ExtendI32S);
            } else {
                func.instruction(&Instruction::I64ExtendI32U);
            }
        }
    }

    fn emit_print_char(&mut self, func: &mut Function, value: &LirValue) -> Result<()> {
        self.widen_i32_to_i64(func, value, false);
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
        let write_ptr = self.tmp_i64_local2;
        let value_local = self.tmp_i64_local4;
        self.emit_buffered_bytes(func, 1, |func| {
            func.instruction(&Instruction::LocalGet(write_ptr));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalTee(write_ptr));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::I32Store8(MemArg {
                align: 0,
                offset: 0,
                memory_index: 0,
            }));
        })?;
        Ok(())
    }

    fn emit_print_f64_from_tmp(&mut self, func: &mut Function) -> Result<()> {
        func.instruction(&Instruction::LocalGet(self.time_tmp_local));
        func.instruction(&Instruction::F64Const(0.0));
        func.instruction(&Instruction::F64Lt);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        self.emit_print_literal(func, "-")?;
        func.instruction(&Instruction::LocalGet(self.time_tmp_local));
        func.instruction(&Instruction::F64Neg);
        func.instruction(&Instruction::LocalSet(self.time_tmp_local));
        func.instruction(&Instruction::End);

        func.instruction(&Instruction::LocalGet(self.time_tmp_local));
        func.instruction(&Instruction::F64Trunc);
        func.instruction(&Instruction::I64TruncF64S);
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
        self.emit_print_i64_from_tmp(func, false)?;

        self.emit_print_literal(func, ".")?;

        func.instruction(&Instruction::LocalGet(self.time_tmp_local));
        func.instruction(&Instruction::LocalGet(self.time_tmp_local));
        func.instruction(&Instruction::F64Trunc);
        func.instruction(&Instruction::F64Sub);
        func.instruction(&Instruction::F64Const(1_000_000.0));
        func.instruction(&Instruction::F64Mul);
        func.instruction(&Instruction::I64TruncF64U);
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
        self.emit_print_u64_fixed_from_tmp(func, 6)?;
        Ok(())
    }

    fn emit_print_c_string_from_tmp(&mut self, func: &mut Function) -> Result<()> {
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local));
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local4));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Load8U(MemArg {
            align: 0,
            offset: 0,
            memory_index: 0,
        }));
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::BrIf(1));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local));
        func.instruction(&Instruction::I64Const(1));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End);
        func.instruction(&Instruction::End);

        self.emit_fd_write_from_tmp(func)
    }

    fn emit_print_i64_from_tmp(&mut self, func: &mut Function, signed: bool) -> Result<()> {
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(self.tmp_i32_local2));

        let write_ptr = self.tmp_i64_local2;
        let value_local = self.tmp_i64_local4;
        let digit_local = self.tmp_i32_local;
        let sign_local = self.tmp_i32_local2;

        if signed {
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::LocalSet(sign_local));
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalSet(value_local));
            func.instruction(&Instruction::End);
        }

        self.emit_buffered_bytes(func, PRINT_BUF_SIZE, |func| {
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Eqz);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::LocalGet(write_ptr));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalTee(write_ptr));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::I32Const(48));
            func.instruction(&Instruction::I32Store8(MemArg {
                align: 0,
                offset: 0,
                memory_index: 0,
            }));
            func.instruction(&Instruction::Else);
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Eqz);
            func.instruction(&Instruction::BrIf(1));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Const(10));
            func.instruction(&Instruction::I64RemU);
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::LocalSet(digit_local));
            func.instruction(&Instruction::LocalGet(write_ptr));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalTee(write_ptr));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::LocalGet(digit_local));
            func.instruction(&Instruction::I32Const(48));
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::I32Store8(MemArg {
                align: 0,
                offset: 0,
                memory_index: 0,
            }));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Const(10));
            func.instruction(&Instruction::I64DivU);
            func.instruction(&Instruction::LocalSet(value_local));
            func.instruction(&Instruction::Br(0));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::End);

            func.instruction(&Instruction::LocalGet(sign_local));
            func.instruction(&Instruction::I32Eqz);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::Else);
            func.instruction(&Instruction::LocalGet(write_ptr));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalTee(write_ptr));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::I32Const(45));
            func.instruction(&Instruction::I32Store8(MemArg {
                align: 0,
                offset: 0,
                memory_index: 0,
            }));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::End);
        })?;
        Ok(())
    }

    fn emit_print_u64_fixed_from_tmp(&mut self, func: &mut Function, width: i64) -> Result<()> {
        let write_ptr = self.tmp_i64_local2;
        let value_local = self.tmp_i64_local4;
        let counter_local = self.tmp_i64_local;
        let digit_local = self.tmp_i32_local;
        self.emit_buffered_bytes(func, width, |func| {
            func.instruction(&Instruction::I64Const(width));
            func.instruction(&Instruction::LocalSet(counter_local));
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::LocalGet(counter_local));
            func.instruction(&Instruction::I64Eqz);
            func.instruction(&Instruction::BrIf(1));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Const(10));
            func.instruction(&Instruction::I64RemU);
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::LocalSet(digit_local));
            func.instruction(&Instruction::LocalGet(write_ptr));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalTee(write_ptr));
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::LocalGet(digit_local));
            func.instruction(&Instruction::I32Const(48));
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::I32Store8(MemArg {
                align: 0,
                offset: 0,
                memory_index: 0,
            }));
            func.instruction(&Instruction::LocalGet(value_local));
            func.instruction(&Instruction::I64Const(10));
            func.instruction(&Instruction::I64DivU);
            func.instruction(&Instruction::LocalSet(value_local));
            func.instruction(&Instruction::LocalGet(counter_local));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::LocalSet(counter_local));
            func.instruction(&Instruction::Br(0));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::End);
        })?;
        Ok(())
    }

    fn emit_buffered_bytes<F>(&mut self, func: &mut Function, size: i64, emit: F) -> Result<()>
    where
        F: FnOnce(&mut Function),
    {
        self.emit_align_sp(func);
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local3));
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::I64Const(size));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::LocalSet(self.sp_local));
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local2));

        emit(func);

        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::I64Sub);
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local4));
        self.emit_fd_write_from_tmp(func)?;
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local3));
        func.instruction(&Instruction::LocalSet(self.sp_local));
        Ok(())
    }

    fn emit_fd_write_from_tmp(&mut self, func: &mut Function) -> Result<()> {
        let Some(fd_write_index) = self.emitter.fd_write_index else {
            return Err(Error::from("fd_write import missing"));
        };

        self.emit_align_sp(func);
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local3));
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::LocalSet(self.tmp_i64_local2));
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::I64Const(IOVEC_SIZE));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::LocalSet(self.sp_local));

        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local4));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Store(MemArg {
            align: 0,
            offset: 0,
            memory_index: 0,
        }));

        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Const(4));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Store(MemArg {
            align: 0,
            offset: 0,
            memory_index: 0,
        }));

        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::LocalGet(self.tmp_i64_local2));
        func.instruction(&Instruction::I32WrapI64);
        func.instruction(&Instruction::I32Const(8));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::Call(fd_write_index));
        func.instruction(&Instruction::Drop);

        func.instruction(&Instruction::LocalGet(self.tmp_i64_local3));
        func.instruction(&Instruction::LocalSet(self.sp_local));
        Ok(())
    }

    fn emit_align_sp(&mut self, func: &mut Function) {
        func.instruction(&Instruction::LocalGet(self.sp_local));
        func.instruction(&Instruction::I64Const((STACK_ALIGN - 1) as i64));
        func.instruction(&Instruction::I64Add);
        func.instruction(&Instruction::I64Const(!((STACK_ALIGN - 1) as i64)));
        func.instruction(&Instruction::I64And);
        func.instruction(&Instruction::LocalSet(self.sp_local));
    }

    fn emit_value(&mut self, func: &mut Function, value: &LirValue) -> Result<()> {
        match value {
            LirValue::Register(reg) => {
                let local = self.register_map.get(reg).copied().unwrap_or(0);
                func.instruction(&Instruction::LocalGet(local));
            }
            LirValue::Constant(constant) => self.emit_constant(func, constant)?,
            LirValue::Global(name, _) => {
                let addr = self
                    .emitter
                    .global_addr
                    .get(name)
                    .copied()
                    .unwrap_or(0);
                func.instruction(&Instruction::I64Const(addr as i64));
            }
            LirValue::Function(name) => {
                let idx = self
                    .emitter
                    .func_index
                    .get(name)
                    .copied()
                    .unwrap_or(0);
                func.instruction(&Instruction::I64Const(idx as i64));
            }
            LirValue::Local(id) => {
                let local = self.locals_map.get(id).copied().unwrap_or(0);
                func.instruction(&Instruction::LocalGet(local));
            }
            LirValue::StackSlot(id) => {
                let offset = self.stack_slots.get(id).copied().unwrap_or(0);
                func.instruction(&Instruction::LocalGet(self.sp_base_local));
                func.instruction(&Instruction::I64Const(offset as i64));
                func.instruction(&Instruction::I64Add);
            }
            LirValue::Undef(ty) | LirValue::Null(ty) => {
                emit_zero_for_type(func, ty);
            }
        }
        Ok(())
    }

    fn emit_constant(&mut self, func: &mut Function, constant: &LirConstant) -> Result<()> {
        match constant {
            LirConstant::Int(value, ty) => emit_int_constant(func, *value, ty),
            LirConstant::UInt(value, ty) => emit_int_constant(func, *value as i64, ty),
            LirConstant::Float(value, ty) => emit_float_constant(func, *value, ty),
            LirConstant::Bool(value) => {
                func.instruction(&Instruction::I32Const(*value as i32));
            }
            LirConstant::String(value) => {
                let ptr = self.emitter.write_string_data(value) as i64;
                func.instruction(&Instruction::I64Const(ptr));
            }
            LirConstant::Array(_, _) | LirConstant::Struct(_, _) => {
                func.instruction(&Instruction::I64Const(0));
            }
            LirConstant::GlobalRef(name, _, indices) => {
                let base = self
                    .emitter
                    .global_addr
                    .get(name.as_str())
                    .copied()
                    .unwrap_or(0);
                let offset = offset_for_indices(
                    &self.value_type(&LirValue::Global(
                        name.to_string(),
                        LirType::Ptr(Box::new(LirType::I8)),
                    )),
                    &indices.iter().map(|v| *v as u32).collect::<Vec<_>>(),
                );
                func.instruction(&Instruction::I64Const((base + offset) as i64));
            }
            LirConstant::FunctionRef(_, _) => {
                func.instruction(&Instruction::I64Const(0));
            }
            LirConstant::Null(ty) | LirConstant::Undef(ty) => emit_zero_for_type(func, ty),
        }
        Ok(())
    }

    fn value_type(&self, value: &LirValue) -> LirType {
        match value {
            LirValue::Register(id) => self.register_types.get(id).cloned().unwrap_or(LirType::I64),
            LirValue::Constant(constant) => constant_type(constant),
            LirValue::Global(_, ty) => ty.clone(),
            LirValue::Function(_) => LirType::Ptr(Box::new(LirType::I8)),
            LirValue::Local(id) => self
                .func
                .locals
                .iter()
                .find(|local| local.id == *id)
                .map(|local| local.ty.clone())
                .unwrap_or(LirType::I64),
            LirValue::StackSlot(_) => LirType::Ptr(Box::new(LirType::I8)),
            LirValue::Undef(ty) | LirValue::Null(ty) => ty.clone(),
        }
    }

    fn store_result(&mut self, func: &mut Function, reg: RegisterId, ty: &LirType) {
        let local = self.register_map.get(&reg).copied().unwrap_or(0);
        match lower_val_type(ty) {
            ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 => {
                func.instruction(&Instruction::LocalSet(local));
            }
            _ => {
                func.instruction(&Instruction::LocalSet(local));
            }
        };
    }

    fn flush_phi_assignments(
        &mut self,
        _func: &mut Function,
        _block_id: BasicBlockId,
        _phi_nodes: &[PhiNode],
    ) -> Result<()> {
        Ok(())
    }
}

#[derive(Clone)]
struct PhiNode {
    result: RegisterId,
    incoming: Vec<(LirValue, BasicBlockId)>,
}

fn collect_phi_nodes(block: &LirBasicBlock) -> Vec<PhiNode> {
    let mut nodes = Vec::new();
    for instr in &block.instructions {
        if let LirInstructionKind::Phi { incoming } = &instr.kind {
            nodes.push(PhiNode {
                result: instr.id,
                incoming: incoming.clone(),
            });
        }
    }
    nodes
}

#[derive(Clone, Copy)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Clone, Copy)]
enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

fn lower_val_type(ty: &LirType) -> ValType {
    match ty {
        LirType::I1 | LirType::I8 | LirType::I16 | LirType::I32 => ValType::I32,
        LirType::I64 | LirType::I128 | LirType::Ptr(_) => ValType::I64,
        LirType::F32 => ValType::F32,
        LirType::F64 => ValType::F64,
        LirType::Struct { .. } | LirType::Array(_, _) | LirType::Vector(_, _) => ValType::I64,
        _ => ValType::I64,
    }
}

fn block_type_for(ty: &LirType) -> wasm_encoder::BlockType {
    match lower_val_type(ty) {
        ValType::I32 => wasm_encoder::BlockType::Result(ValType::I32),
        ValType::I64 => wasm_encoder::BlockType::Result(ValType::I64),
        ValType::F32 => wasm_encoder::BlockType::Result(ValType::F32),
        ValType::F64 => wasm_encoder::BlockType::Result(ValType::F64),
        _ => wasm_encoder::BlockType::Empty,
    }
}

fn emit_int_binop(func: &mut Function, op: BinOp, is_i64: bool) {
    let instr = match (op, is_i64) {
        (BinOp::Add, false) => Instruction::I32Add,
        (BinOp::Sub, false) => Instruction::I32Sub,
        (BinOp::Mul, false) => Instruction::I32Mul,
        (BinOp::Div, false) => Instruction::I32DivS,
        (BinOp::Rem, false) => Instruction::I32RemS,
        (BinOp::And, false) => Instruction::I32And,
        (BinOp::Or, false) => Instruction::I32Or,
        (BinOp::Xor, false) => Instruction::I32Xor,
        (BinOp::Shl, false) => Instruction::I32Shl,
        (BinOp::Shr, false) => Instruction::I32ShrS,
        (BinOp::Add, true) => Instruction::I64Add,
        (BinOp::Sub, true) => Instruction::I64Sub,
        (BinOp::Mul, true) => Instruction::I64Mul,
        (BinOp::Div, true) => Instruction::I64DivS,
        (BinOp::Rem, true) => Instruction::I64RemS,
        (BinOp::And, true) => Instruction::I64And,
        (BinOp::Or, true) => Instruction::I64Or,
        (BinOp::Xor, true) => Instruction::I64Xor,
        (BinOp::Shl, true) => Instruction::I64Shl,
        (BinOp::Shr, true) => Instruction::I64ShrS,
    };
    func.instruction(&instr);
}

fn block_index(dest: BasicBlockId, block_ids: &[BasicBlockId]) -> Result<i32> {
    block_ids
        .iter()
        .position(|id| *id == dest)
        .map(|idx| idx as i32)
        .ok_or_else(|| Error::from(format!("unknown basic block id: {dest:?}")))
}

#[derive(Clone, Copy)]
enum PrintSpecKind {
    Signed,
    Unsigned,
    Float,
    Str,
    Char,
}

#[derive(Clone, Copy)]
struct PrintSpec {
    kind: PrintSpecKind,
}

enum PrintPart {
    Literal(String),
    Spec(PrintSpec),
}

fn parse_printf_format(format: &str) -> Result<Vec<PrintPart>> {
    let mut parts = Vec::new();
    let mut literal = String::new();
    let mut chars = format.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '%' {
            literal.push(ch);
            continue;
        }

        if let Some('%') = chars.peek().copied() {
            chars.next();
            literal.push('%');
            continue;
        }

        if !literal.is_empty() {
            parts.push(PrintPart::Literal(std::mem::take(&mut literal)));
        }

        let mut spec_char = None;
        while let Some(next) = chars.next() {
            match next {
                'd' | 'i' | 'u' | 'f' | 's' | 'c' => {
                    spec_char = Some(next);
                    break;
                }
                _ => {}
            }
        }

        let Some(spec_char) = spec_char else {
            return Err(Error::from("unterminated printf format specifier"));
        };

        let kind = match spec_char {
            'd' | 'i' => PrintSpecKind::Signed,
            'u' => PrintSpecKind::Unsigned,
            'f' => PrintSpecKind::Float,
            's' => PrintSpecKind::Str,
            'c' => PrintSpecKind::Char,
            _ => {
                return Err(Error::from(format!(
                    "unsupported printf format specifier: %{spec_char}"
                )))
            }
        };

        parts.push(PrintPart::Spec(PrintSpec { kind }));
    }

    if !literal.is_empty() {
        parts.push(PrintPart::Literal(literal));
    }

    Ok(parts)
}

fn emit_float_binop(func: &mut Function, op: BinOp, is_f64: bool) {
    let instr = match (op, is_f64) {
        (BinOp::Add, false) => Instruction::F32Add,
        (BinOp::Sub, false) => Instruction::F32Sub,
        (BinOp::Mul, false) => Instruction::F32Mul,
        (BinOp::Div, false) => Instruction::F32Div,
        (BinOp::Rem, false) => Instruction::F32Mul,
        (BinOp::Add, true) => Instruction::F64Add,
        (BinOp::Sub, true) => Instruction::F64Sub,
        (BinOp::Mul, true) => Instruction::F64Mul,
        (BinOp::Div, true) => Instruction::F64Div,
        (BinOp::Rem, true) => Instruction::F64Mul,
        _ => Instruction::F64Add,
    };
    func.instruction(&instr);
}

fn emit_int_cmp(func: &mut Function, op: CmpOp, is_i64: bool) {
    let instr = match (op, is_i64) {
        (CmpOp::Eq, false) => Instruction::I32Eq,
        (CmpOp::Ne, false) => Instruction::I32Ne,
        (CmpOp::Lt, false) => Instruction::I32LtS,
        (CmpOp::Le, false) => Instruction::I32LeS,
        (CmpOp::Gt, false) => Instruction::I32GtS,
        (CmpOp::Ge, false) => Instruction::I32GeS,
        (CmpOp::Eq, true) => Instruction::I64Eq,
        (CmpOp::Ne, true) => Instruction::I64Ne,
        (CmpOp::Lt, true) => Instruction::I64LtS,
        (CmpOp::Le, true) => Instruction::I64LeS,
        (CmpOp::Gt, true) => Instruction::I64GtS,
        (CmpOp::Ge, true) => Instruction::I64GeS,
    };
    func.instruction(&instr);
}

fn emit_float_cmp(func: &mut Function, op: CmpOp, is_f32: bool) {
    let instr = match (op, is_f32) {
        (CmpOp::Eq, true) => Instruction::F32Eq,
        (CmpOp::Ne, true) => Instruction::F32Ne,
        (CmpOp::Lt, true) => Instruction::F32Lt,
        (CmpOp::Le, true) => Instruction::F32Le,
        (CmpOp::Gt, true) => Instruction::F32Gt,
        (CmpOp::Ge, true) => Instruction::F32Ge,
        (CmpOp::Eq, false) => Instruction::F64Eq,
        (CmpOp::Ne, false) => Instruction::F64Ne,
        (CmpOp::Lt, false) => Instruction::F64Lt,
        (CmpOp::Le, false) => Instruction::F64Le,
        (CmpOp::Gt, false) => Instruction::F64Gt,
        (CmpOp::Ge, false) => Instruction::F64Ge,
    };
    func.instruction(&instr);
}

fn emit_load_for_type(func: &mut Function, ty: &LirType) {
    let mem = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    match ty {
        LirType::I1 | LirType::I8 => func.instruction(&Instruction::I32Load8U(mem)),
        LirType::I16 => func.instruction(&Instruction::I32Load16U(mem)),
        LirType::I32 => func.instruction(&Instruction::I32Load(mem)),
        LirType::I64 | LirType::Ptr(_) => func.instruction(&Instruction::I64Load(mem)),
        LirType::F32 => func.instruction(&Instruction::F32Load(mem)),
        LirType::F64 => func.instruction(&Instruction::F64Load(mem)),
        _ => func.instruction(&Instruction::I64Load(mem)),
    };
}

fn emit_store_for_type(func: &mut Function, ty: &LirType) {
    let mem = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    match ty {
        LirType::I1 | LirType::I8 => func.instruction(&Instruction::I32Store8(mem)),
        LirType::I16 => func.instruction(&Instruction::I32Store16(mem)),
        LirType::I32 => func.instruction(&Instruction::I32Store(mem)),
        LirType::I64 | LirType::Ptr(_) => func.instruction(&Instruction::I64Store(mem)),
        LirType::F32 => func.instruction(&Instruction::F32Store(mem)),
        LirType::F64 => func.instruction(&Instruction::F64Store(mem)),
        _ => func.instruction(&Instruction::I64Store(mem)),
    };
}

fn emit_zero_for_type(func: &mut Function, ty: &LirType) {
    match lower_val_type(ty) {
        ValType::I32 => func.instruction(&Instruction::I32Const(0)),
        ValType::I64 => func.instruction(&Instruction::I64Const(0)),
        ValType::F32 => func.instruction(&Instruction::F32Const(0.0)),
        ValType::F64 => func.instruction(&Instruction::F64Const(0.0)),
        _ => func.instruction(&Instruction::I32Const(0)),
    };
}

fn emit_int_constant(func: &mut Function, value: i64, ty: &LirType) {
    match lower_val_type(ty) {
        ValType::I32 => func.instruction(&Instruction::I32Const(value as i32)),
        ValType::I64 => func.instruction(&Instruction::I64Const(value)),
        _ => func.instruction(&Instruction::I64Const(value)),
    };
}

fn emit_float_constant(func: &mut Function, value: f64, ty: &LirType) {
    match ty {
        LirType::F32 => func.instruction(&Instruction::F32Const(value as f32)),
        _ => func.instruction(&Instruction::F64Const(value)),
    };
}

fn size_of_type(ty: &LirType) -> u64 {
    match ty {
        LirType::I1 | LirType::I8 => 1,
        LirType::I16 => 2,
        LirType::I32 | LirType::F32 => 4,
        LirType::I64 | LirType::F64 => 8,
        LirType::I128 => 16,
        LirType::Ptr(_) => 8,
        LirType::Array(elem, len) => size_of_type(elem) * *len as u64,
        LirType::Struct { fields, .. } => fields.iter().map(size_of_type).sum(),
        _ => 8,
    }
}

fn offset_for_indices(ty: &LirType, indices: &[u32]) -> u64 {
    let mut offset = 0;
    let mut current = ty.clone();
    for (idx, raw_index) in indices.iter().enumerate() {
        let index = *raw_index as usize;
        match current {
            LirType::Struct { fields, .. } => {
                let mut field_offset = 0;
                for field in fields.iter().take(index) {
                    field_offset += size_of_type(field);
                }
                offset += field_offset;
                current = fields.get(index).cloned().unwrap_or(LirType::I8);
            }
            LirType::Array(elem, _) => {
                offset += size_of_type(&elem) * index as u64;
                current = *elem;
            }
            LirType::Ptr(inner) if idx == 0 => {
                offset += size_of_type(&inner) * index as u64;
                current = *inner;
            }
            _ => {
                offset += size_of_type(&current) * index as u64;
            }
        }
    }
    offset
}

fn align_to(value: u64, alignment: u64) -> u64 {
    if alignment == 0 {
        return value;
    }
    (value + alignment - 1) & !(alignment - 1)
}

fn int_to_bytes(value: u128, ty: &LirType) -> Vec<u8> {
    let size = size_of_type(ty) as usize;
    let mut bytes = vec![0u8; size];
    let mut val = value;
    for byte in &mut bytes {
        *byte = (val & 0xFF) as u8;
        val >>= 8;
    }
    bytes
}

fn float_to_bytes(value: f64, ty: &LirType) -> Vec<u8> {
    match ty {
        LirType::F32 => value.to_le_bytes()[..4].to_vec(),
        _ => value.to_le_bytes().to_vec(),
    }
}

fn constant_type(constant: &LirConstant) -> LirType {
    match constant {
        LirConstant::Int(_, ty) => ty.clone(),
        LirConstant::UInt(_, ty) => ty.clone(),
        LirConstant::Float(_, ty) => ty.clone(),
        LirConstant::Bool(_) => LirType::I1,
        LirConstant::String(_) => LirType::Ptr(Box::new(LirType::I8)),
        LirConstant::Array(_, ty) => LirType::Array(Box::new(ty.clone()), 0),
        LirConstant::Struct(_, ty) => ty.clone(),
        LirConstant::GlobalRef(_, ty, _) => ty.clone(),
        LirConstant::FunctionRef(_, ty) => ty.clone(),
        LirConstant::Null(ty) | LirConstant::Undef(ty) => ty.clone(),
    }
}


fn value_type_for(
    func: &LirFunction,
    reg_types: &HashMap<RegisterId, LirType>,
    value: &LirValue,
) -> LirType {
    match value {
        LirValue::Register(id) => reg_types.get(id).cloned().unwrap_or(LirType::I64),
        LirValue::Constant(constant) => constant_type(constant),
        LirValue::Global(_, ty) => ty.clone(),
        LirValue::Function(_) => LirType::Ptr(Box::new(LirType::I8)),
        LirValue::Local(id) => func
            .locals
            .iter()
            .find(|local| local.id == *id)
            .map(|local| local.ty.clone())
            .unwrap_or(LirType::I64),
        LirValue::StackSlot(_) => LirType::Ptr(Box::new(LirType::I8)),
        LirValue::Undef(ty) | LirValue::Null(ty) => ty.clone(),
    }
}
