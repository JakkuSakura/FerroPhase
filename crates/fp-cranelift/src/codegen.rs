use crate::config::CraneliftConfig;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{AbiParam, Block, Function, InstBuilder, MemFlags, Signature, types};
use cranelift_codegen::isa::{self, CallConv};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Switch, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use fp_core::error::Result;
use fp_core::lir::{
    BasicBlockId, CallingConvention, Linkage as LirLinkage, LirBasicBlock, LirConstant,
    LirConstantAggregate, LirConstantData, LirConstantKind, LirDataLayout, LirFloat, LirFunction,
    LirFunctionRef, LirFunctionSignature, LirGlobalRelocation, LirInstruction, LirInstructionKind,
    LirInteger, LirIntrinsicKind, LirBlob, LirRelocationTarget, LirTerminator, LirType,
    LirValue, LirValueKind,
};
use std::collections::HashMap;
use target_lexicon::Triple;

const FPVALUE_SIZE: i32 = 16;
const FPVALUE_TAG_OFFSET: i32 = 0;
const FPVALUE_DATA_OFFSET: i32 = 8;

fn target_data_layout() -> LirDataLayout {
    LirDataLayout::new(
        64,
        8,
        vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
    )
    .expect("valid Cranelift target data layout")
}

fn size_of(ty: &LirType) -> u64 {
    target_data_layout()
        .size_of(ty)
        .expect("Cranelift type must have a layout")
}

fn struct_layout(ty: &LirType) -> Option<fp_core::lir::layout::StructLayout> {
    target_data_layout().struct_layout(ty).ok().flatten()
}

#[derive(Clone, Copy)]
enum FpValueTag {
    I64 = 0,
    U64 = 1,
    F64 = 2,
    Bool = 3,
    Char = 4,
    Ptr = 5,
}

pub struct CraneliftBackend {
    module: ObjectModule,
    pointer_type: cranelift_codegen::ir::Type,
    func_ids: HashMap<String, FuncId>,
    data_ids: HashMap<String, DataId>,
    string_ids: HashMap<String, DataId>,
}

impl CraneliftBackend {
    pub fn new(config: &CraneliftConfig) -> Result<Self> {
        let triple = if let Some(triple) = config.target_triple.as_deref() {
            triple
                .parse::<Triple>()
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?
        } else {
            Triple::host()
        };

        let mut flag_builder = settings::builder();
        let _ = flag_builder.set("is_pic", "true");
        let flags = settings::Flags::new(flag_builder);
        let isa_builder =
            isa::lookup(triple).map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        let isa = isa_builder
            .finish(flags)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;

        let builder = ObjectBuilder::new(
            isa,
            "fp-cranelift",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        let module = ObjectModule::new(builder);
        let pointer_type = module.isa().pointer_type();

        Ok(Self {
            module,
            pointer_type,
            func_ids: HashMap::new(),
            data_ids: HashMap::new(),
            string_ids: HashMap::new(),
        })
    }

    pub fn emit_object(mut self, program: &LirBlob) -> Result<Vec<u8>> {
        self.declare_globals(program)?;
        self.declare_functions(program)?;
        self.define_globals(program)?;
        self.define_functions(program)?;
        let obj = self
            .module
            .finish()
            .emit()
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        Ok(obj)
    }

    fn declare_globals(&mut self, program: &LirBlob) -> Result<()> {
        for global in &program.globals {
            let name = global.name.to_string();
            if self.data_ids.contains_key(&name) {
                continue;
            }
            let linkage = map_linkage(global.linkage.clone());
            let writable = !global.is_constant;
            let data_id = self
                .module
                .declare_data(&name, linkage, writable, false)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            self.data_ids.insert(name, data_id);
        }
        Ok(())
    }

    fn declare_functions(&mut self, program: &LirBlob) -> Result<()> {
        for func in &program.functions {
            let name = func.name.to_string();
            if self.func_ids.contains_key(&name) {
                continue;
            }
            let sig = signature_from_lir(
                &func.signature,
                self.pointer_type,
                func.calling_convention.clone(),
            );
            let linkage = if func.is_declaration {
                Linkage::Import
            } else {
                map_linkage(func.linkage.clone())
            };
            let func_id = self
                .module
                .declare_function(&name, linkage, &sig)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            self.func_ids.insert(name, func_id);
        }
        Ok(())
    }

    fn define_globals(&mut self, program: &LirBlob) -> Result<()> {
        for global in &program.globals {
            let name = global.name.to_string();
            let Some(data_id) = self.data_ids.get(&name).copied() else {
                continue;
            };
            let mut data_ctx = DataDescription::new();
            if let Some(initializer) = &global.initializer {
                let (bytes, relocations) = if global.relocations.is_empty() {
                    let mut bytes = vec![0u8; size_of(&global.ty) as usize];
                    let mut relocations = Vec::new();
                    encode_constant(&mut bytes, &mut relocations, initializer, &global.ty, 0)?;
                    (bytes, relocations)
                } else {
                    let bytes = match initializer {
                        LirConstant {
                            kind: LirConstantKind::Data(LirConstantData::Bytes(bytes)),
                            ..
                        } => bytes.clone(),
                        _ => {
                            return Err(fp_core::error::Error::from(
                                "LIR global relocations require a byte initializer",
                            ));
                        }
                    };
                    let relocations = global
                        .relocations
                        .iter()
                        .cloned()
                        .map(DataReloc::from)
                        .collect::<Vec<_>>();
                    (bytes, relocations)
                };
                data_ctx.define(bytes.into_boxed_slice());
                for reloc in relocations {
                    match reloc {
                        DataReloc::Data { offset, name } => {
                            if let Some(target) = self.data_ids.get(&name) {
                                let gv = self.module.declare_data_in_data(*target, &mut data_ctx);
                                data_ctx.write_data_addr(offset as u32, gv, 0);
                            }
                        }
                        DataReloc::Func { offset, name } => {
                            if let Some(target) = self.func_ids.get(&name) {
                                let func_ref =
                                    self.module.declare_func_in_data(*target, &mut data_ctx);
                                data_ctx.write_function_addr(offset as u32, func_ref);
                            }
                        }
                    }
                }
            } else {
                data_ctx.define_zeroinit(size_of(&global.ty) as usize);
            }

            self.module
                .define_data(data_id, &data_ctx)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
        }
        Ok(())
    }

    fn define_functions(&mut self, program: &LirBlob) -> Result<()> {
        for func in &program.functions {
            if func.is_declaration {
                continue;
            }
            let func_id = *self.func_ids.get(&func.name.to_string()).ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "missing cranelift function id for {}",
                    func.name
                ))
            })?;
            let mut ctx = self.module.make_context();
            ctx.func = Function::with_name_signature(
                cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32()),
                signature_from_lir(
                    &func.signature,
                    self.pointer_type,
                    func.calling_convention.clone(),
                ),
            );

            let mut func_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let mut lowerer = FunctionLowerer::new(
                &mut builder,
                &mut self.module,
                self.pointer_type,
                &mut self.func_ids,
                &self.data_ids,
                &mut self.string_ids,
            );
            lowerer.lower_function(func)?;

            builder.finalize();
            self.module
                .define_function(func_id, &mut ctx)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            self.module.clear_context(&mut ctx);
        }
        Ok(())
    }
}

struct FunctionLowerer<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut ObjectModule,
    pointer_type: cranelift_codegen::ir::Type,
    func_ids: &'a mut HashMap<String, FuncId>,
    data_ids: &'a HashMap<String, DataId>,
    string_ids: &'a mut HashMap<String, DataId>,
    blocks: HashMap<BasicBlockId, Block>,
    reg_values: HashMap<u32, cranelift_codegen::ir::Value>,
    reg_types: HashMap<u32, LirType>,
    local_values: HashMap<u32, cranelift_codegen::ir::Value>,
    local_types: HashMap<u32, LirType>,
    stack_slots: HashMap<u32, cranelift_codegen::ir::StackSlot>,
    phi_nodes: HashMap<BasicBlockId, Vec<PhiNode>>,
}

#[derive(Clone)]
struct PhiNode {
    instr_id: u32,
    ty: LirType,
    incoming: Vec<(LirValue, BasicBlockId)>,
}

impl<'a, 'b> FunctionLowerer<'a, 'b> {
    fn new(
        builder: &'a mut FunctionBuilder<'b>,
        module: &'a mut ObjectModule,
        pointer_type: cranelift_codegen::ir::Type,
        func_ids: &'a mut HashMap<String, FuncId>,
        data_ids: &'a HashMap<String, DataId>,
        string_ids: &'a mut HashMap<String, DataId>,
    ) -> Self {
        Self {
            builder,
            module,
            pointer_type,
            func_ids,
            data_ids,
            string_ids,
            blocks: HashMap::new(),
            reg_values: HashMap::new(),
            reg_types: HashMap::new(),
            local_values: HashMap::new(),
            local_types: HashMap::new(),
            stack_slots: HashMap::new(),
            phi_nodes: HashMap::new(),
        }
    }

    fn lower_function(&mut self, func: &LirFunction) -> Result<()> {
        for block in &func.basic_blocks {
            let clif_block = self.builder.create_block();
            self.blocks.insert(block.id, clif_block);
        }

        let entry_block = self
            .blocks
            .get(&func.basic_blocks[0].id)
            .copied()
            .ok_or_else(|| fp_core::error::Error::from("missing entry block"))?;

        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);

        self.populate_locals(func, entry_block)?;
        self.populate_stack_slots(func)?;
        self.collect_phi_nodes(func)?;

        for block in &func.basic_blocks {
            let clif_block = self.blocks[&block.id];
            if self.builder.current_block() != Some(clif_block) {
                self.builder.switch_to_block(clif_block);
            }
            self.lower_block(block)?;
            self.builder.seal_block(clif_block);
        }

        Ok(())
    }

    fn populate_locals(&mut self, func: &LirFunction, entry_block: Block) -> Result<()> {
        let params: Vec<_> = self.builder.block_params(entry_block).to_vec();
        let mut param_idx = 0usize;
        for local in &func.locals {
            self.local_types.insert(local.id, local.ty.clone());
            if local.is_argument {
                let param = *params.get(param_idx).ok_or_else(|| {
                    fp_core::error::Error::from(format!("missing param for local {}", local.id))
                })?;
                self.local_values.insert(local.id, param);
                param_idx += 1;
            } else {
                let var = Variable::from_u32(local.id);
                let ty = clif_type_for_lir(&local.ty, self.pointer_type);
                self.builder.declare_var(var, ty);
                let zero = self.zero_value(ty);
                self.builder.def_var(var, zero);
                self.local_values
                    .insert(local.id, self.builder.use_var(var));
            }
        }
        Ok(())
    }

    fn populate_stack_slots(&mut self, func: &LirFunction) -> Result<()> {
        for slot in &func.stack_slots {
            let size = slot.size.max(1) as u32;
            let align = slot.alignment.max(1) as u32;
            let align_pow2 = align.next_power_of_two();
            let align_shift = (align_pow2.trailing_zeros() as u8).min(31);
            let data = cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                size,
                align_shift,
            );
            let clif_slot = self.builder.create_sized_stack_slot(data);
            self.stack_slots.insert(slot.id, clif_slot);
        }
        Ok(())
    }

    fn collect_phi_nodes(&mut self, func: &LirFunction) -> Result<()> {
        for block in &func.basic_blocks {
            let mut phis = Vec::new();
            for instr in &block.instructions {
                if let LirInstructionKind::Phi { incoming } = &instr.kind {
                    let ty = instr
                        .result
                        .as_ref()
                        .map(|result| result.ty.clone())
                        .ok_or_else(|| fp_core::error::Error::from("phi has no result type"))?;
                    phis.push(PhiNode {
                        instr_id: instr.id,
                        ty,
                        incoming: incoming.clone(),
                    });
                }
            }
            if !phis.is_empty() {
                self.phi_nodes.insert(block.id, phis);
            }
        }
        Ok(())
    }

    fn lower_block(&mut self, block: &LirBasicBlock) -> Result<()> {
        if let Some(phis) = self.phi_nodes.get(&block.id) {
            for phi in phis {
                let ty = clif_type_for_lir(&phi.ty, self.pointer_type);
                let param = self.builder.append_block_param(self.blocks[&block.id], ty);
                self.reg_values.insert(phi.instr_id, param);
                self.reg_types.insert(phi.instr_id, phi.ty.clone());
            }
        }

        for instr in &block.instructions {
            if matches!(instr.kind, LirInstructionKind::Phi { .. }) {
                continue;
            }
            self.lower_instruction(instr)?;
        }

        self.lower_terminator(&block.terminator, block.id)?;
        Ok(())
    }

    fn lower_instruction(&mut self, instr: &LirInstruction) -> Result<()> {
        match &instr.kind {
            LirInstructionKind::Add(lhs, rhs) => {
                let result = self.lower_arith(instr, lhs, rhs, ArithOp::Add)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Sub(lhs, rhs) => {
                let result = self.lower_arith(instr, lhs, rhs, ArithOp::Sub)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Mul(lhs, rhs) => {
                let result = self.lower_arith(instr, lhs, rhs, ArithOp::Mul)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Div(lhs, rhs) => {
                let result = self.lower_arith(instr, lhs, rhs, ArithOp::Div)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Rem(lhs, rhs) => {
                let result = self.lower_arith(instr, lhs, rhs, ArithOp::Rem)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::And(lhs, rhs) => {
                let result = self.lower_int_bin(instr, lhs, rhs, IntBinOp::And)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Or(lhs, rhs) => {
                let result = self.lower_int_bin(instr, lhs, rhs, IntBinOp::Or)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Xor(lhs, rhs) => {
                let result = self.lower_int_bin(instr, lhs, rhs, IntBinOp::Xor)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Shl(lhs, rhs) => {
                let result = self.lower_int_bin(instr, lhs, rhs, IntBinOp::Shl)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Shr(lhs, rhs) => {
                let result = self.lower_int_bin(instr, lhs, rhs, IntBinOp::Shr)?;
                self.record_result(instr, result);
            }
            LirInstructionKind::Not(value) => {
                let val = self.value_for(value)?;
                let result = self.builder.ins().bnot(val);
                self.record_result(instr, result);
            }
            LirInstructionKind::Eq(lhs, rhs) => {
                self.lower_cmp(instr, lhs, rhs, IntCC::Equal, FloatCC::Equal)?
            }
            LirInstructionKind::Ne(lhs, rhs) => {
                self.lower_cmp(instr, lhs, rhs, IntCC::NotEqual, FloatCC::NotEqual)?
            }
            LirInstructionKind::Lt(lhs, rhs) => {
                self.lower_cmp(instr, lhs, rhs, IntCC::SignedLessThan, FloatCC::LessThan)?
            }
            LirInstructionKind::Le(lhs, rhs) => self.lower_cmp(
                instr,
                lhs,
                rhs,
                IntCC::SignedLessThanOrEqual,
                FloatCC::LessThanOrEqual,
            )?,
            LirInstructionKind::Gt(lhs, rhs) => self.lower_cmp(
                instr,
                lhs,
                rhs,
                IntCC::SignedGreaterThan,
                FloatCC::GreaterThan,
            )?,
            LirInstructionKind::Ge(lhs, rhs) => self.lower_cmp(
                instr,
                lhs,
                rhs,
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
            )?,
            LirInstructionKind::Load {
                address, alignment, ..
            } => {
                let addr = self.value_for(address)?;
                let ty = instr
                    .result
                    .as_ref()
                    .map(|result| result.ty.clone())
                    .ok_or_else(|| fp_core::error::Error::from("load has no result type"))?;
                let mem_ty = clif_type_for_lir(&ty, self.pointer_type);
                let mut flags = MemFlags::new();
                if alignment.is_some() {
                    flags.set_aligned();
                }
                let value = self.builder.ins().load(mem_ty, flags, addr, 0);
                self.record_result(instr, value);
            }
            LirInstructionKind::Store {
                value,
                address,
                alignment,
                ..
            } => {
                let addr = self.value_for(address)?;
                let stored = self.value_for(value)?;
                let mut flags = MemFlags::new();
                if alignment.is_some() {
                    flags.set_aligned();
                }
                self.builder.ins().store(flags, stored, addr, 0);
            }
            LirInstructionKind::Alloca { size, .. } => {
                let ptr_ty = instr
                    .result
                    .as_ref()
                    .map(|result| result.ty.clone())
                    .ok_or_else(|| fp_core::error::Error::from("alloca has no result type"))?;
                let LirType::Ptr(inner) = ptr_ty.clone() else {
                    return Err(fp_core::error::Error::from("alloca expects pointer type"));
                };
                let count = const_i64(size)?;
                if count < 0 {
                    return Err(fp_core::error::Error::from(
                        "alloca size must be non-negative",
                    ));
                }
                let elem_size = size_of(&inner) as i64;
                let bytes = elem_size.saturating_mul(count).max(1) as u32;
                let data = cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    bytes,
                    3,
                );
                let slot = self.builder.create_sized_stack_slot(data);
                let ptr = self.builder.ins().stack_addr(self.pointer_type, slot, 0);
                self.record_result(instr, ptr);
            }
            LirInstructionKind::GetElementPtr { ptr, indices, .. } => {
                let base = self.value_for(ptr)?;
                let ptr_ty = self.type_for_value(ptr)?;
                let addr = self.lower_gep(base, &ptr_ty, indices)?;
                self.record_result(instr, addr);
            }
            LirInstructionKind::PtrToInt(value) => {
                let val = self.value_for(value)?;
                let target_ty = instr
                    .result
                    .as_ref()
                    .map(|result| result.ty.clone())
                    .ok_or_else(|| fp_core::error::Error::from("ptrtoint has no result type"))?;
                let ty = clif_type_for_lir(&target_ty, self.pointer_type);
                let casted = self.bitcast_value(ty, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::IntToPtr(value) => {
                let val = self.value_for(value)?;
                let casted = self.bitcast_value(self.pointer_type, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::Trunc(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().ireduce(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::ZExt(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().uextend(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::SExt(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().sextend(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::FPTrunc(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fdemote(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::FPExt(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fpromote(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::FPToUI(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fcvt_to_uint(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::FPToSI(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fcvt_to_sint(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::UIToFP(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fcvt_from_uint(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::SIToFP(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.builder.ins().fcvt_from_sint(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::Bitcast(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let casted = self.bitcast_value(target, val);
                self.record_result(instr, casted);
            }
            LirInstructionKind::ExtractValue { aggregate, indices } => {
                let base = self.value_for(aggregate)?;
                let agg_ty = self.type_for_value(aggregate)?;
                let offset = aggregate_offset(&agg_ty, indices)?;
                let elem_ty = aggregate_element_type(&agg_ty, indices)?;
                let mem_ty = clif_type_for_lir(&elem_ty, self.pointer_type);
                let value = self
                    .builder
                    .ins()
                    .load(mem_ty, MemFlags::new(), base, offset as i32);
                self.record_result(instr, value);
            }
            LirInstructionKind::InsertValue {
                aggregate,
                element,
                indices,
            } => {
                let base = self.value_for(aggregate)?;
                let agg_ty = self.type_for_value(aggregate)?;
                let offset = aggregate_offset(&agg_ty, indices)?;
                let element_val = self.value_for(element)?;
                self.builder
                    .ins()
                    .store(MemFlags::new(), element_val, base, offset as i32);
                self.record_result(instr, base);
            }
            LirInstructionKind::Call { function, args, .. } => {
                let call = self.lower_call(
                    function,
                    args,
                    instr.result.as_ref().map(|result| result.ty.clone()),
                )?;
                if let Some(ret) = call {
                    self.record_result(instr, ret);
                }
            }
            LirInstructionKind::ExecQuery(_) => {
                return Err(fp_core::error::Error::from(
                    "LIR ExecQuery is only supported by pxc whole-file lowering",
                ));
            }
            LirInstructionKind::IntrinsicCall { kind, format, args } => {
                let value = self.lower_intrinsic(
                    kind,
                    format,
                    args,
                    instr.result.as_ref().map(|result| result.ty.clone()),
                )?;
                if let Some(val) = value {
                    self.record_result(instr, val);
                }
            }
            LirInstructionKind::SextOrTrunc(value, ty) => {
                let val = self.value_for(value)?;
                let target = clif_type_for_lir(ty, self.pointer_type);
                let source_ty = self.builder.func.dfg.value_type(val);
                let casted = if target.bits() > source_ty.bits() {
                    self.builder.ins().sextend(target, val)
                } else if target.bits() < source_ty.bits() {
                    self.builder.ins().ireduce(target, val)
                } else {
                    val
                };
                self.record_result(instr, casted);
            }
            LirInstructionKind::Select {
                condition,
                if_true,
                if_false,
            } => {
                let cond = self.value_for(condition)?;
                let tval = self.value_for(if_true)?;
                let fval = self.value_for(if_false)?;
                let result = self.builder.ins().select(cond, tval, fval);
                self.record_result(instr, result);
            }
            LirInstructionKind::InlineAsm { .. } => {
                self.builder
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
                if let Some(ty) = instr.result.as_ref().map(|result| result.ty.clone()) {
                    let val = self.zero_value(clif_type_for_lir(&ty, self.pointer_type));
                    self.record_result(instr, val);
                }
            }
            LirInstructionKind::LandingPad { result_type, .. } => {
                let ty = clif_type_for_lir(result_type, self.pointer_type);
                let zero = self.zero_value(ty);
                self.record_result(instr, zero);
            }
            LirInstructionKind::Unreachable | LirInstructionKind::ComptimeOp(_) => {
                self.builder
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
            }
            LirInstructionKind::Freeze(value) => {
                let val = self.value_for(value)?;
                self.record_result(instr, val);
            }
            LirInstructionKind::Phi { .. } => {}
        }
        Ok(())
    }

    fn lower_terminator(
        &mut self,
        term: &LirTerminator,
        current_block: BasicBlockId,
    ) -> Result<()> {
        match term {
            LirTerminator::Return(value) => {
                if let Some(value) = value {
                    let ret = self.value_for(value)?;
                    self.builder.ins().return_(&[ret]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            LirTerminator::Br(dest) => {
                let args = self.phi_args_for(*dest, current_block)?;
                self.builder.ins().jump(self.blocks[dest], &args);
            }
            LirTerminator::CondBr {
                condition,
                if_true,
                if_false,
            } => {
                let cond = self.value_for(condition)?;
                let t_args = self.phi_args_for(*if_true, current_block)?;
                let f_args = self.phi_args_for(*if_false, current_block)?;
                self.builder.ins().brif(
                    cond,
                    self.blocks[if_true],
                    &t_args,
                    self.blocks[if_false],
                    &f_args,
                );
            }
            LirTerminator::Switch {
                value,
                default,
                cases,
            } => {
                let val = self.value_for(value)?;
                let current_bb = current_block;
                let current_block = self.blocks[&current_bb];
                let mut switch = Switch::new();

                let default_shim = self.builder.create_block();
                let default_args = self.phi_args_for(*default, current_bb)?;
                self.builder.switch_to_block(default_shim);
                self.builder.ins().jump(self.blocks[default], &default_args);
                self.builder.seal_block(default_shim);

                for (case_val, dest) in cases {
                    let shim = self.builder.create_block();
                    let args = self.phi_args_for(*dest, current_bb)?;
                    self.builder.switch_to_block(shim);
                    self.builder.ins().jump(self.blocks[dest], &args);
                    self.builder.seal_block(shim);
                    switch.set_entry(*case_val as u128, shim);
                }

                self.builder.switch_to_block(current_block);
                switch.emit(self.builder, val, default_shim);
            }
            LirTerminator::IndirectBr {
                address,
                destinations,
            } => {
                let _ = self.value_for(address)?;
                let _ = destinations;
                self.builder
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
            }
            LirTerminator::Invoke {
                function,
                args,
                normal_dest,
                ..
            } => {
                let _ = self.lower_call(function, args, None)?;
                let normal_args = self.phi_args_for(*normal_dest, current_block)?;
                self.builder
                    .ins()
                    .jump(self.blocks[normal_dest], &normal_args);
            }
            LirTerminator::Resume(_) => {
                self.builder
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
            }
            LirTerminator::Unreachable => {
                self.builder
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
            }
            LirTerminator::CleanupRet { unwind_dest, .. } => {
                if let Some(dest) = unwind_dest {
                    let args = self.phi_args_for(*dest, current_block)?;
                    self.builder.ins().jump(self.blocks[dest], &args);
                } else {
                    self.builder
                        .ins()
                        .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);
                }
            }
            LirTerminator::CatchRet { successor, .. } => {
                let args = self.phi_args_for(*successor, current_block)?;
                self.builder.ins().jump(self.blocks[successor], &args);
            }
            LirTerminator::CatchSwitch {
                handlers,
                unwind_dest,
                ..
            } => {
                if let Some(dest) = unwind_dest {
                    let args = self.phi_args_for(*dest, current_block)?;
                    self.builder.ins().jump(self.blocks[dest], &args);
                }
                for handler in handlers {
                    let args = self.phi_args_for(*handler, current_block)?;
                    self.builder.ins().jump(self.blocks[handler], &args);
                }
            }
        }
        Ok(())
    }

    fn phi_args_for(
        &mut self,
        dest: BasicBlockId,
        current: BasicBlockId,
    ) -> Result<Vec<cranelift_codegen::ir::Value>> {
        let Some(phis) = self.phi_nodes.get(&dest).cloned() else {
            return Ok(Vec::new());
        };
        let mut args = Vec::with_capacity(phis.len());
        for phi in phis {
            let incoming = phi
                .incoming
                .iter()
                .find(|(_, pred)| *pred == current)
                .ok_or_else(|| {
                    fp_core::error::Error::from(format!(
                        "missing phi incoming for block {}",
                        current
                    ))
                })?;
            args.push(self.value_for(&incoming.0)?);
        }
        Ok(args)
    }

    fn lower_call(
        &mut self,
        target: &LirValue,
        args: &[LirValue],
        result_ty: Option<LirType>,
    ) -> Result<Option<cranelift_codegen::ir::Value>> {
        let mut sig = Signature::new(self.builder.func.signature.call_conv);
        if let Some(ret) = result_ty.clone() {
            let ret_ty = clif_type_for_lir(&ret, self.pointer_type);
            if ret_ty != types::INVALID {
                sig.returns.push(AbiParam::new(ret_ty));
            }
        }
        let mut call_args = Vec::with_capacity(args.len());
        for arg in args {
            let val = self.value_for(arg)?;
            let ty = self.builder.func.dfg.value_type(val);
            sig.params.push(AbiParam::new(ty));
            call_args.push(val);
        }

        let call = match &target.kind {
            LirValueKind::Function(LirFunctionRef::Name(name)) => {
                let func_id = *self.func_ids.get(name.as_str()).ok_or_else(|| {
                    fp_core::error::Error::from(format!("unknown function {}", name))
                })?;
                let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                self.builder.ins().call(func_ref, &call_args)
            }
            _ => {
                let callee = self.value_for(target)?;
                let sig_ref = self.builder.import_signature(sig);
                self.builder
                    .ins()
                    .call_indirect(sig_ref, callee, &call_args)
            }
        };

        let results = self.builder.inst_results(call);
        Ok(results.get(0).copied())
    }

    fn lower_intrinsic(
        &mut self,
        kind: &LirIntrinsicKind,
        format: &str,
        args: &[LirValue],
        result_ty: Option<LirType>,
    ) -> Result<Option<cranelift_codegen::ir::Value>> {
        match kind {
            LirIntrinsicKind::TimeNow => {
                let func = self.declare_runtime("fp_cranelift_time_now", &[], Some(LirType::F64));
                let call = self.builder.ins().call(func, &[]);
                let results = self.builder.inst_results(call);
                let value = results.get(0).copied();
                if let (Some(val), Some(ty)) = (value, result_ty) {
                    let target = clif_type_for_lir(&ty, self.pointer_type);
                    if self.builder.func.dfg.value_type(val) != target {
                        let casted = if target == types::F32 {
                            self.builder.ins().fdemote(target, val)
                        } else {
                            self.builder.ins().fpromote(target, val)
                        };
                        return Ok(Some(casted));
                    }
                }
                return Ok(value);
            }
            LirIntrinsicKind::Format => {
                let (args_ptr, count) = self.build_fpvalue_array(args)?;
                let fmt_ptr = self.intern_cstring(format)?;
                let count_val = self.builder.ins().iconst(self.pointer_type, count as i64);
                let func = self.declare_runtime(
                    "fp_cranelift_format",
                    &[self.pointer_type, self.pointer_type, self.pointer_type],
                    Some(LirType::Ptr(Box::new(LirType::I8))),
                );
                let call = self
                    .builder
                    .ins()
                    .call(func, &[fmt_ptr, args_ptr, count_val]);
                let results = self.builder.inst_results(call);
                return Ok(results.get(0).copied());
            }
            LirIntrinsicKind::Print | LirIntrinsicKind::Println => {
                let (args_ptr, count) = self.build_fpvalue_array(args)?;
                let fmt_ptr = self.intern_cstring(format)?;
                let count_val = self.builder.ins().iconst(self.pointer_type, count as i64);
                let newline = matches!(kind, LirIntrinsicKind::Println);
                let newline_val = self
                    .builder
                    .ins()
                    .iconst(types::I8, if newline { 1 } else { 0 });
                let func = self.declare_runtime(
                    "fp_cranelift_print",
                    &[
                        self.pointer_type,
                        self.pointer_type,
                        self.pointer_type,
                        types::I8,
                    ],
                    None,
                );
                self.builder
                    .ins()
                    .call(func, &[fmt_ptr, args_ptr, count_val, newline_val]);
                return Ok(None);
            }
            LirIntrinsicKind::ProcMacroTokenStreamFromStr
            | LirIntrinsicKind::ProcMacroTokenStreamToString => {
                return Err(fp_core::error::Error::from(
                    "proc-macro token stream parsing/printing is not supported by the cranelift backend"
                        .to_string(),
                ));
            }
        }
    }

    fn declare_runtime(
        &mut self,
        name: &str,
        params: &[cranelift_codegen::ir::Type],
        ret: Option<LirType>,
    ) -> cranelift_codegen::ir::FuncRef {
        let mut sig = Signature::new(self.builder.func.signature.call_conv);
        for ty in params {
            sig.params.push(AbiParam::new(*ty));
        }
        if let Some(ret) = ret {
            let ty = clif_type_for_lir(&ret, self.pointer_type);
            sig.returns.push(AbiParam::new(ty));
        }
        let func_id = if let Some(id) = self.func_ids.get(name) {
            *id
        } else {
            let id = self
                .module
                .declare_function(name, Linkage::Import, &sig)
                .expect("declare runtime func");
            self.func_ids.insert(name.to_string(), id);
            id
        };
        self.module.declare_func_in_func(func_id, self.builder.func)
    }

    fn declare_external(
        &mut self,
        name: &str,
        params: &[cranelift_codegen::ir::Type],
        ret: Option<cranelift_codegen::ir::Type>,
    ) -> cranelift_codegen::ir::FuncRef {
        let mut sig = Signature::new(self.builder.func.signature.call_conv);
        for ty in params {
            sig.params.push(AbiParam::new(*ty));
        }
        if let Some(ret) = ret {
            sig.returns.push(AbiParam::new(ret));
        }
        let func_id = if let Some(id) = self.func_ids.get(name) {
            *id
        } else {
            let id = self
                .module
                .declare_function(name, Linkage::Import, &sig)
                .expect("declare external func");
            self.func_ids.insert(name.to_string(), id);
            id
        };
        self.module.declare_func_in_func(func_id, self.builder.func)
    }

    fn build_fpvalue_array(
        &mut self,
        args: &[LirValue],
    ) -> Result<(cranelift_codegen::ir::Value, usize)> {
        let len = args.len();
        if len == 0 {
            let null = self.builder.ins().iconst(self.pointer_type, 0);
            return Ok((null, 0));
        }
        let total_bytes = (len as u32) * (FPVALUE_SIZE as u32);
        let data = cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            total_bytes.max(1),
            3,
        );
        let slot = self.builder.create_sized_stack_slot(data);
        let base = self.builder.ins().stack_addr(self.pointer_type, slot, 0);
        for (idx, arg) in args.iter().enumerate() {
            let offset = (idx as i32) * FPVALUE_SIZE;
            let (tag, value) = self.encode_fpvalue(arg)?;
            let tag_val = self.builder.ins().iconst(types::I8, tag as i64);
            self.builder
                .ins()
                .store(MemFlags::new(), tag_val, base, offset + FPVALUE_TAG_OFFSET);
            self.builder
                .ins()
                .store(MemFlags::new(), value, base, offset + FPVALUE_DATA_OFFSET);
        }
        Ok((base, len))
    }

    fn encode_fpvalue(
        &mut self,
        arg: &LirValue,
    ) -> Result<(FpValueTag, cranelift_codegen::ir::Value)> {
        let val = self.value_for(arg)?;
        let ty = self.type_for_value(arg)?;
        let tag = match (&arg.kind, ty.clone()) {
            (
                LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(
                    LirInteger::I64(_),
                ))),
                _,
            ) => FpValueTag::U64,
            (_, LirType::F32 | LirType::F64) => FpValueTag::F64,
            (_, LirType::I1) => FpValueTag::Bool,
            (_, LirType::I8) => FpValueTag::Char,
            (_, LirType::I16 | LirType::I32 | LirType::I64 | LirType::I128) => FpValueTag::I64,
            (_, LirType::Ptr(_) | LirType::Array(_, _) | LirType::Struct { .. }) => FpValueTag::Ptr,
            (_, LirType::Function { .. }) => FpValueTag::Ptr,
            _ => FpValueTag::I64,
        };

        let data_val = match tag {
            FpValueTag::F64 => {
                let float_ty = self.builder.func.dfg.value_type(val);
                if float_ty == types::F64 {
                    val
                } else {
                    self.builder.ins().fpromote(types::F64, val)
                }
            }
            FpValueTag::U64 => {
                let int_ty = self.builder.func.dfg.value_type(val);
                if int_ty == types::I64 {
                    val
                } else if int_ty.bits() < 64 {
                    self.builder.ins().uextend(types::I64, val)
                } else {
                    self.builder.ins().ireduce(types::I64, val)
                }
            }
            FpValueTag::Ptr => {
                let ptr = if self.builder.func.dfg.value_type(val) == self.pointer_type {
                    val
                } else {
                    self.bitcast_value(self.pointer_type, val)
                };
                self.bitcast_value(types::I64, ptr)
            }
            _ => {
                let int_ty = self.builder.func.dfg.value_type(val);
                if int_ty == types::I64 {
                    val
                } else if int_ty.bits() < 64 {
                    self.builder.ins().sextend(types::I64, val)
                } else {
                    self.builder.ins().ireduce(types::I64, val)
                }
            }
        };
        Ok((tag, data_val))
    }

    fn intern_cstring(&mut self, text: &str) -> Result<cranelift_codegen::ir::Value> {
        let name = format!("__fp_str_{}", self.string_ids.len());
        let data_id = if let Some(id) = self.string_ids.get(text) {
            *id
        } else {
            let id = self
                .module
                .declare_data(&name, Linkage::Local, false, false)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            let mut data_ctx = DataDescription::new();
            let mut bytes = Vec::from(text.as_bytes());
            bytes.push(0);
            data_ctx.define(bytes.into_boxed_slice());
            self.module
                .define_data(id, &data_ctx)
                .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
            self.string_ids.insert(text.to_string(), id);
            id
        };
        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        Ok(self.builder.ins().global_value(self.pointer_type, gv))
    }

    fn value_for(&mut self, value: &LirValue) -> Result<cranelift_codegen::ir::Value> {
        match &value.kind {
            LirValueKind::Register(id) => self
                .reg_values
                .get(id)
                .copied()
                .ok_or_else(|| fp_core::error::Error::from(format!("unknown register {}", id))),
            LirValueKind::Constant(_) => self.constant_value(value),
            LirValueKind::Global(name) => {
                let data_id = *self.data_ids.get(name.as_str()).ok_or_else(|| {
                    fp_core::error::Error::from(format!("unknown global {}", name))
                })?;
                let gv = self.module.declare_data_in_func(data_id, self.builder.func);
                Ok(self.builder.ins().global_value(self.pointer_type, gv))
            }
            LirValueKind::Function(LirFunctionRef::Name(name)) => {
                let func_id = *self.func_ids.get(name.as_str()).ok_or_else(|| {
                    fp_core::error::Error::from(format!("unknown function {}", name))
                })?;
                let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                Ok(self.builder.ins().func_addr(self.pointer_type, func_ref))
            }
            LirValueKind::Function(LirFunctionRef::Package { package_id, name }) => {
                Err(fp_core::error::Error::from(format!(
                    "package-qualified function `{:?}::{name}` is not supported by Cranelift lowering",
                    package_id
                )))
            }
            LirValueKind::Function(LirFunctionRef::Definition(def_id)) => {
                Err(fp_core::error::Error::from(format!(
                    "function definition `{def_id}` is not supported by Cranelift lowering"
                )))
            }
            LirValueKind::Local(id) => self
                .local_values
                .get(id)
                .copied()
                .ok_or_else(|| fp_core::error::Error::from(format!("unknown local {}", id))),
            LirValueKind::StackSlot(id) => {
                let slot = *self.stack_slots.get(id).ok_or_else(|| {
                    fp_core::error::Error::from(format!("unknown stack slot {}", id))
                })?;
                Ok(self.builder.ins().stack_addr(self.pointer_type, slot, 0))
            }
        }
    }

    fn constant_value(&mut self, value: &LirValue) -> Result<cranelift_codegen::ir::Value> {
        let LirValueKind::Constant(kind) = &value.kind else {
            return Err(fp_core::error::Error::from("expected constant"));
        };
        match kind {
            LirConstantKind::Data(LirConstantData::Integer(integer)) => {
                let ty = clif_type_for_lir(&value.ty, self.pointer_type);
                let raw = match integer {
                    LirInteger::I1(v) => i64::from(*v),
                    LirInteger::I8(v) => i64::from(*v),
                    LirInteger::I16(v) => i64::from(*v),
                    LirInteger::I32(v) => i64::from(*v),
                    LirInteger::I64(v) => *v as i64,
                    LirInteger::I128(_) | LirInteger::Arbitrary(_) => {
                        return Err(fp_core::error::Error::from(
                            "wide integer Cranelift constant unsupported",
                        ));
                    }
                };
                Ok(self.builder.ins().iconst(ty, raw))
            }
            LirConstantKind::Data(LirConstantData::Float(float)) => Ok(match float {
                LirFloat::F32(v) => self.builder.ins().f32const(f32::from_bits(*v)),
                LirFloat::F64(v) => self.builder.ins().f64const(f64::from_bits(*v)),
            }),
            LirConstantKind::Null | LirConstantKind::Undef => {
                let clif_ty = clif_type_for_lir(&value.ty, self.pointer_type);
                Ok(self.zero_value(clif_ty))
            }
            LirConstantKind::GlobalAddress { global: name } => {
                let base = LirValue::global(name.clone(), LirType::Ptr(Box::new(LirType::I8)));
                let base_val = self.value_for(&base)?;
                let offset = 0;
                Ok(self.builder.ins().iadd_imm(base_val, offset as i64))
            }
            LirConstantKind::FunctionAddress(LirFunctionRef::Name(name)) => {
                let func_id = *self.func_ids.get(name.as_str()).ok_or_else(|| {
                    fp_core::error::Error::from(format!("unknown function {}", name))
                })?;
                let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                Ok(self.builder.ins().func_addr(self.pointer_type, func_ref))
            }
            LirConstantKind::Data(LirConstantData::Bytes(_)) | LirConstantKind::Aggregate(_) => {
                Err(fp_core::error::Error::from(
                    "aggregate constants must be lowered via globals",
                ))
            }
            _ => Err(fp_core::error::Error::from(
                "unsupported Cranelift constant",
            )),
        }
    }

    fn type_for_value(&self, value: &LirValue) -> Result<LirType> {
        Ok(value.ty.clone())
    }

    fn record_result(&mut self, instr: &LirInstruction, value: cranelift_codegen::ir::Value) {
        self.reg_values.insert(instr.id, value);
        if let Some(ty) = instr.result.as_ref().map(|result| result.ty.clone()) {
            self.reg_types.insert(instr.id, ty);
        }
    }

    fn zero_value(&mut self, ty: cranelift_codegen::ir::Type) -> cranelift_codegen::ir::Value {
        if ty == types::F32 {
            self.builder.ins().f32const(0.0)
        } else if ty == types::F64 {
            self.builder.ins().f64const(0.0)
        } else {
            self.builder.ins().iconst(ty, 0)
        }
    }

    fn lower_arith(
        &mut self,
        instr: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: ArithOp,
    ) -> Result<cranelift_codegen::ir::Value> {
        let lhs_val = self.value_for(lhs)?;
        let rhs_val = self.value_for(rhs)?;
        let ty = instr
            .result
            .as_ref()
            .map(|result| result.ty.clone())
            .ok_or_else(|| fp_core::error::Error::from("arithmetic has no result type"))?;
        let clif_ty = clif_type_for_lir(&ty, self.pointer_type);
        let (l, r) = self.coerce_values(lhs_val, rhs_val, clif_ty);
        let value = match op {
            ArithOp::Add => {
                if clif_ty.is_float() {
                    self.builder.ins().fadd(l, r)
                } else {
                    self.builder.ins().iadd(l, r)
                }
            }
            ArithOp::Sub => {
                if clif_ty.is_float() {
                    self.builder.ins().fsub(l, r)
                } else {
                    self.builder.ins().isub(l, r)
                }
            }
            ArithOp::Mul => {
                if clif_ty.is_float() {
                    self.builder.ins().fmul(l, r)
                } else {
                    self.builder.ins().imul(l, r)
                }
            }
            ArithOp::Div => {
                if clif_ty.is_float() {
                    self.builder.ins().fdiv(l, r)
                } else {
                    self.builder.ins().sdiv(l, r)
                }
            }
            ArithOp::Rem => {
                if clif_ty.is_float() {
                    let name = if clif_ty == types::F32 {
                        "fmodf"
                    } else {
                        "fmod"
                    };
                    let func = self.declare_external(name, &[clif_ty, clif_ty], Some(clif_ty));
                    let call = self.builder.ins().call(func, &[l, r]);
                    *self.builder.inst_results(call).get(0).unwrap_or(&l)
                } else {
                    self.builder.ins().srem(l, r)
                }
            }
        };
        Ok(value)
    }

    fn lower_int_bin(
        &mut self,
        instr: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: IntBinOp,
    ) -> Result<cranelift_codegen::ir::Value> {
        let lhs_val = self.value_for(lhs)?;
        let rhs_val = self.value_for(rhs)?;
        let ty = instr
            .result
            .as_ref()
            .map(|result| result.ty.clone())
            .ok_or_else(|| fp_core::error::Error::from("integer operation has no result type"))?;
        let clif_ty = clif_type_for_lir(&ty, self.pointer_type);
        let (l, r) = self.coerce_values(lhs_val, rhs_val, clif_ty);
        let value = match op {
            IntBinOp::And => self.builder.ins().band(l, r),
            IntBinOp::Or => self.builder.ins().bor(l, r),
            IntBinOp::Xor => self.builder.ins().bxor(l, r),
            IntBinOp::Shl => self.builder.ins().ishl(l, r),
            IntBinOp::Shr => self.builder.ins().sshr(l, r),
        };
        Ok(value)
    }

    fn lower_cmp(
        &mut self,
        instr: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        int_cc: IntCC,
        float_cc: FloatCC,
    ) -> Result<()> {
        let lhs_val = self.value_for(lhs)?;
        let rhs_val = self.value_for(rhs)?;
        let ty = instr
            .result
            .as_ref()
            .map(|result| result.ty.clone())
            .ok_or_else(|| fp_core::error::Error::from("comparison has no result type"))?;
        let clif_ty = clif_type_for_lir(&ty, self.pointer_type);
        let (l, r) = self.coerce_values(lhs_val, rhs_val, clif_ty);
        let value = if clif_ty.is_float() {
            self.builder.ins().fcmp(float_cc, l, r)
        } else {
            self.builder.ins().icmp(int_cc, l, r)
        };
        self.record_result(instr, value);
        Ok(())
    }

    fn lower_gep(
        &mut self,
        base: cranelift_codegen::ir::Value,
        base_ty: &LirType,
        indices: &[LirValue],
    ) -> Result<cranelift_codegen::ir::Value> {
        let LirType::Ptr(inner) = base_ty else {
            return Err(fp_core::error::Error::from("GEP expects pointer base"));
        };
        let mut current = inner.as_ref().clone();
        let mut addr_i64 = self.bitcast_value(types::I64, base);
        for idx in indices {
            let idx_val = self.value_for(idx)?;
            let idx_i64 = match self.builder.func.dfg.value_type(idx_val) {
                ty if ty == types::I64 => idx_val,
                ty if ty.bits() < 64 => self.builder.ins().sextend(types::I64, idx_val),
                _ => self.builder.ins().ireduce(types::I64, idx_val),
            };
            match &current {
                LirType::Struct { fields, .. } => {
                    let index = match idx {
                        LirValue {
                            kind:
                                LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(
                                    LirInteger::I64(v),
                                ))),
                            ..
                        } => *v as usize,
                        _ => {
                            return Err(fp_core::error::Error::from(
                                "struct GEP requires constant index",
                            ));
                        }
                    };
                    let layout = struct_layout(&current)
                        .ok_or_else(|| fp_core::error::Error::from("missing struct layout"))?;
                    let offset = layout.field_offsets[index] as i64;
                    addr_i64 = self.builder.ins().iadd_imm(addr_i64, offset);
                    current = fields[index].clone();
                }
                LirType::Array(elem, _) => {
                    let elem_size = size_of(elem) as i64;
                    let scaled = if elem_size == 1 {
                        idx_i64
                    } else {
                        self.builder.ins().imul_imm(idx_i64, elem_size)
                    };
                    addr_i64 = self.builder.ins().iadd(addr_i64, scaled);
                    current = *elem.clone();
                }
                _ => {
                    let elem_size = size_of(&current) as i64;
                    let scaled = if elem_size == 1 {
                        idx_i64
                    } else {
                        self.builder.ins().imul_imm(idx_i64, elem_size)
                    };
                    addr_i64 = self.builder.ins().iadd(addr_i64, scaled);
                }
            }
        }
        Ok(self.bitcast_value(self.pointer_type, addr_i64))
    }

    fn coerce_values(
        &mut self,
        lhs: cranelift_codegen::ir::Value,
        rhs: cranelift_codegen::ir::Value,
        ty: cranelift_codegen::ir::Type,
    ) -> (cranelift_codegen::ir::Value, cranelift_codegen::ir::Value) {
        let lty = self.builder.func.dfg.value_type(lhs);
        let rty = self.builder.func.dfg.value_type(rhs);
        let l = if lty != ty {
            self.bitcast_value(ty, lhs)
        } else {
            lhs
        };
        let r = if rty != ty {
            self.bitcast_value(ty, rhs)
        } else {
            rhs
        };
        (l, r)
    }

    fn bitcast_value(
        &mut self,
        target: cranelift_codegen::ir::Type,
        val: cranelift_codegen::ir::Value,
    ) -> cranelift_codegen::ir::Value {
        self.builder.ins().bitcast(target, MemFlags::new(), val)
    }
}

enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

enum IntBinOp {
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

fn signature_from_lir(
    sig: &LirFunctionSignature,
    pointer_type: cranelift_codegen::ir::Type,
    calling: CallingConvention,
) -> Signature {
    let mut signature = Signature::new(callconv_for_lir(calling));
    for param in &sig.params {
        let ty = clif_type_for_lir(param, pointer_type);
        signature.params.push(AbiParam::new(ty));
    }
    if !matches!(sig.return_type, LirType::Void) {
        let ret_ty = clif_type_for_lir(&sig.return_type, pointer_type);
        signature.returns.push(AbiParam::new(ret_ty));
    }
    signature
}

fn callconv_for_lir(calling: CallingConvention) -> CallConv {
    match calling {
        CallingConvention::C => CallConv::triple_default(&Triple::host()),
        CallingConvention::Fast => CallConv::Fast,
        CallingConvention::Cold => CallConv::Cold,
        _ => CallConv::triple_default(&Triple::host()),
    }
}

fn clif_type_for_lir(
    ty: &LirType,
    pointer: cranelift_codegen::ir::Type,
) -> cranelift_codegen::ir::Type {
    match ty {
        LirType::Integer(width) => match width {
            1..=8 => types::I8,
            9..=16 => types::I16,
            17..=32 => types::I32,
            33..=64 => types::I64,
            _ => types::I128,
        },
        LirType::I1 => types::I8,
        LirType::I8 => types::I8,
        LirType::I16 => types::I16,
        LirType::I32 => types::I32,
        LirType::I64 => types::I64,
        LirType::I128 => types::I128,
        LirType::F32 => types::F32,
        LirType::F64 => types::F64,
        LirType::Ptr(_) => pointer,
        LirType::Array(_, _) => pointer,
        LirType::Struct { .. } => pointer,
        LirType::Function { .. } => pointer,
        LirType::Vector(_, _) => pointer,
        LirType::Void | LirType::Label | LirType::Token | LirType::Metadata | LirType::Error => {
            types::INVALID
        }
    }
}

fn map_linkage(linkage: LirLinkage) -> Linkage {
    match linkage {
        LirLinkage::External => Linkage::Export,
        LirLinkage::Internal | LirLinkage::Private => Linkage::Local,
        LirLinkage::WeakAny | LirLinkage::WeakOdr | LirLinkage::ExternalWeak => {
            Linkage::Preemptible
        }
        _ => Linkage::Preemptible,
    }
}

enum DataReloc {
    Data { offset: usize, name: String },
    Func { offset: usize, name: String },
}

impl From<LirGlobalRelocation> for DataReloc {
    fn from(value: LirGlobalRelocation) -> Self {
        match value.target {
            LirRelocationTarget::Global(name) => DataReloc::Data {
                offset: value.offset as usize,
                name: name.to_string(),
            },
            LirRelocationTarget::Function(name) => DataReloc::Func {
                offset: value.offset as usize,
                name: name.to_string(),
            },
        }
    }
}

fn encode_constant(
    buf: &mut [u8],
    relocs: &mut Vec<DataReloc>,
    constant: &LirConstant,
    ty: &LirType,
    base: usize,
) -> Result<()> {
    match &constant.kind {
        LirConstantKind::Data(LirConstantData::Integer(integer)) => {
            let value = match integer {
                LirInteger::I1(v) => u128::from(*v),
                LirInteger::I8(v) => u128::from(*v),
                LirInteger::I16(v) => u128::from(*v),
                LirInteger::I32(v) => u128::from(*v),
                LirInteger::I64(v) => u128::from(*v),
                LirInteger::I128(v) => *v,
                LirInteger::Arbitrary(_) => {
                    return Err(fp_core::error::Error::from(
                        "wide integer data encoding unsupported",
                    ));
                }
            };
            write_int(buf, base, value, size_of(ty) as usize, false);
        }
        LirConstantKind::Data(LirConstantData::Float(float)) => match float {
            LirFloat::F32(value) => write_float(
                buf,
                base,
                f32::from_bits(*value) as f64,
                size_of(ty) as usize,
            ),
            LirFloat::F64(value) => {
                write_float(buf, base, f64::from_bits(*value), size_of(ty) as usize)
            }
        },
        LirConstantKind::Data(LirConstantData::Bytes(bytes)) => {
            buf[base..base + bytes.len()].copy_from_slice(bytes);
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Array(elements))
        | LirConstantKind::Aggregate(LirConstantAggregate::Vector(elements)) => {
            let LirType::Array(elem_ty, _) = ty else {
                return Err(fp_core::error::Error::from("array constant type mismatch"));
            };
            let elem_size = size_of(elem_ty) as usize;
            for (idx, element) in elements.iter().enumerate() {
                encode_constant(buf, relocs, element, elem_ty, base + idx * elem_size)?;
            }
        }
        LirConstantKind::Aggregate(LirConstantAggregate::Struct(fields)) => {
            let LirType::Struct {
                fields: field_tys, ..
            } = ty
            else {
                return Err(fp_core::error::Error::from("struct constant type mismatch"));
            };
            let layout = struct_layout(ty)
                .ok_or_else(|| fp_core::error::Error::from("missing struct layout"))?;
            for (idx, field) in fields.iter().enumerate() {
                encode_constant(
                    buf,
                    relocs,
                    field,
                    &field_tys[idx],
                    base + layout.field_offsets[idx] as usize,
                )?;
            }
        }
        LirConstantKind::GlobalAddress { global: name } => {
            relocs.push(DataReloc::Data {
                offset: base,
                name: name.to_string(),
            });
        }
        LirConstantKind::FunctionAddress(LirFunctionRef::Name(name)) => {
            relocs.push(DataReloc::Func {
                offset: base,
                name: name.to_string(),
            });
        }
        LirConstantKind::Null | LirConstantKind::Undef => {}
        _ => return Err(fp_core::error::Error::from("unsupported constant encoding")),
    }
    Ok(())
}

fn write_int(buf: &mut [u8], offset: usize, val: u128, size: usize, signed: bool) {
    let mut value = val;
    if signed {
        let mask = (1u128 << (size * 8)) - 1;
        value &= mask;
    }
    for i in 0..size {
        buf[offset + i] = (value >> (i * 8)) as u8;
    }
}

fn write_float(buf: &mut [u8], offset: usize, val: f64, size: usize) {
    match size {
        4 => {
            let bits = (val as f32).to_bits();
            write_int(buf, offset, bits as u128, 4, false);
        }
        _ => {
            let bits = val.to_bits();
            write_int(buf, offset, bits as u128, 8, false);
        }
    }
}

fn aggregate_offset(agg_ty: &LirType, indices: &[u32]) -> Result<usize> {
    let mut offset = 0usize;
    let mut current = agg_ty.clone();
    for idx in indices {
        match &current {
            LirType::Struct { fields, .. } => {
                let layout = struct_layout(&current)
                    .ok_or_else(|| fp_core::error::Error::from("missing struct layout"))?;
                let index = *idx as usize;
                offset += layout.field_offsets[index] as usize;
                current = fields[index].clone();
            }
            LirType::Array(elem, _) => {
                let elem_size = size_of(elem) as usize;
                offset += elem_size * (*idx as usize);
                current = *elem.clone();
            }
            _ => break,
        }
    }
    Ok(offset)
}

fn aggregate_element_type(agg_ty: &LirType, indices: &[u32]) -> Result<LirType> {
    let mut current = agg_ty.clone();
    for idx in indices {
        match &current {
            LirType::Struct { fields, .. } => {
                current = fields[*idx as usize].clone();
            }
            LirType::Array(elem, _) => {
                current = *elem.clone();
            }
            _ => break,
        }
    }
    Ok(current)
}

fn const_i64(value: &LirValue) -> Result<i64> {
    match &value.kind {
        LirValueKind::Constant(LirConstantKind::Data(LirConstantData::Integer(integer))) => {
            match integer {
                LirInteger::I8(value) => Ok(i64::from(*value)),
                LirInteger::I16(value) => Ok(i64::from(*value)),
                LirInteger::I32(value) => Ok(i64::from(*value)),
                LirInteger::I64(value) => Ok(*value as i64),
                LirInteger::I1(value) => Ok(i64::from(*value)),
                LirInteger::I128(_) | LirInteger::Arbitrary(_) => Err(fp_core::error::Error::from(
                    "wide integer alloca size unsupported",
                )),
            }
        }
        _ => Err(fp_core::error::Error::from("alloca size must be constant")),
    }
}
