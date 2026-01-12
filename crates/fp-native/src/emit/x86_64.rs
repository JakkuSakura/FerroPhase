use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirIntrinsicKind,
    LirProgram, LirTerminator, LirType, LirValue,
};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    R8,
    R9,
    R10,
    R11,
    Rbp,
    Rsp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FReg {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
}

impl FReg {
    fn id(self) -> u8 {
        match self {
            FReg::Xmm0 => 0,
            FReg::Xmm1 => 1,
            FReg::Xmm2 => 2,
            FReg::Xmm3 => 3,
            FReg::Xmm4 => 4,
            FReg::Xmm5 => 5,
            FReg::Xmm6 => 6,
            FReg::Xmm7 => 7,
        }
    }
}

impl Reg {
    fn id(self) -> u8 {
        match self {
            Reg::Rax => 0,
            Reg::Rcx => 1,
            Reg::Rdx => 2,
            Reg::Rdi => 7,
            Reg::Rsi => 6,
            Reg::R8 => 8,
            Reg::R9 => 9,
            Reg::R10 => 10,
            Reg::R11 => 11,
            Reg::Rsp => 4,
            Reg::Rbp => 5,
        }
    }
}

struct FrameLayout {
    vreg_offsets: HashMap<u32, i32>,
    slot_offsets: HashMap<u32, i32>,
    outgoing_size: i32,
    shadow_space: i32,
    frame_size: i32,
}

fn build_frame_layout(func: &LirFunction, format: TargetFormat) -> Result<FrameLayout> {
    let mut vreg_ids = BTreeSet::new();
    let mut max_call_args = 0usize;
    let mut has_calls = false;

    for block in &func.basic_blocks {
        for inst in &block.instructions {
            vreg_ids.insert(inst.id);
            if let LirInstructionKind::Call { args, .. } = &inst.kind {
                has_calls = true;
                max_call_args = max_call_args.max(args.len());
            } else if let LirInstructionKind::IntrinsicCall { args, .. } = &inst.kind {
                has_calls = true;
                max_call_args = max_call_args.max(args.len() + 1);
            }
        }
    }

    let mut vreg_offsets = HashMap::new();
    let mut slot_offsets = HashMap::new();
    let mut offset = 0i32;

    for id in vreg_ids {
        offset += 8;
        vreg_offsets.insert(id, -offset);
    }

    for slot in &func.stack_slots {
        offset += 8;
        slot_offsets.insert(slot.id, -offset);
    }

    let local_size = offset;
    let reg_arg_limit = match format {
        TargetFormat::Coff => 4,
        _ => 6,
    };
    let extra_stack_args = max_call_args.saturating_sub(reg_arg_limit);
    let shadow_space = if matches!(format, TargetFormat::Coff) && has_calls {
        32
    } else {
        0
    };
    let outgoing_size = shadow_space + (extra_stack_args as i32) * 8;
    let base = local_size + outgoing_size;
    let frame_size = if base == 0 {
        if has_calls { 8 } else { 0 }
    } else {
        align16(base + 8) - 8
    };

    Ok(FrameLayout {
        vreg_offsets,
        slot_offsets,
        outgoing_size,
        shadow_space,
        frame_size,
    })
}

fn build_reg_types(func: &LirFunction) -> HashMap<u32, LirType> {
    let mut map = HashMap::new();
    for block in &func.basic_blocks {
        for inst in &block.instructions {
            if let Some(ty) = inst.type_hint.as_ref() {
                map.insert(inst.id, ty.clone());
            }
        }
    }
    map
}

fn align16(value: i32) -> i32 {
    ((value + 15) / 16) * 16
}

pub fn emit_text(program: &LirProgram, format: TargetFormat) -> Result<CodegenOutput> {
    let func_map = build_function_map(program)?;
    let mut asm = Assembler::new();
    let mut rodata = Vec::new();
    let mut rodata_pool = HashMap::new();

    for (index, func) in program.functions.iter().enumerate() {
        asm.bind(Label::Function(index as u32));
        let layout = build_frame_layout(func, format)?;
        let reg_types = build_reg_types(func);
        asm.needs_frame = layout.frame_size > 0;
        if layout.frame_size > 0 {
            emit_prologue(&mut asm, &layout)?;
        }
        for block in &func.basic_blocks {
            asm.bind(Label::Block(index as u32, block.id));
            emit_block(
                &mut asm,
                block,
                format,
                &func_map,
                &layout,
                &reg_types,
                &func.signature.return_type,
                &mut rodata,
                &mut rodata_pool,
            )?;
        }
    }

    let (text, relocs) = asm.finish()?;
    Ok(CodegenOutput {
        text,
        rodata,
        relocs,
    })
}

enum BinOp {
    Add,
    Sub,
    Mul,
}

fn emit_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    if is_float_type(ty) {
        emit_float_binop(asm, layout, dst_id, lhs, rhs, op, ty, reg_types)?;
        return Ok(());
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types)?;
            match op {
                BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
            }
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if let Ok(imm32) = i32::try_from(imm) {
                match op {
                    BinOp::Add => emit_add_ri32(asm, Reg::R10, imm32),
                    BinOp::Sub => emit_sub_ri32(asm, Reg::R10, imm32),
                    BinOp::Mul => {
                        emit_mov_imm64(asm, Reg::R11, imm as u64);
                        emit_imul_rr(asm, Reg::R10, Reg::R11);
                    }
                }
            } else {
                emit_mov_imm64(asm, Reg::R11, imm as u64);
                match op {
                    BinOp::Add => emit_add_rr(asm, Reg::R10, Reg::R11),
                    BinOp::Sub => emit_sub_rr(asm, Reg::R10, Reg::R11),
                    BinOp::Mul => emit_imul_rr(asm, Reg::R10, Reg::R11),
                }
            }
        }
        _ => return Err(Error::from("unsupported RHS for x86_64")),
    }
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn load_value(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    dst: Reg,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, dst, Reg::Rbp, offset);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            emit_mov_imm64(asm, dst, imm as u64);
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types)?;
            Err(Error::from(format!(
                "unsupported LIR value for x86_64: {:?}",
                ty
            )))
        }
    }
}

fn load_value_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    dst: FReg,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match value {
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_movsd_xm64(asm, dst, Reg::Rbp, offset, ty);
            Ok(())
        }
        LirValue::Constant(LirConstant::Float(value, _)) => {
            let bits = if matches!(ty, LirType::F32) {
                (*value as f32).to_bits() as u64
            } else {
                value.to_bits()
            };
            emit_mov_imm64(asm, Reg::R10, bits);
            emit_movq_xmm_r64(asm, dst, Reg::R10);
            Ok(())
        }
        _ => {
            let actual_ty = value_type(value, reg_types)?;
            Err(Error::from(format!(
                "unsupported float value for x86_64: {:?}",
                actual_ty
            )))
        }
    }
}

fn emit_float_binop(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    op: BinOp,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types)?;
    match op {
        BinOp::Add => emit_addsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
        BinOp::Sub => emit_subsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
        BinOp::Mul => emit_mulsd(asm, FReg::Xmm0, FReg::Xmm1, ty),
    }
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
    Ok(())
}

fn emit_float_div(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types)?;
    emit_divsd(asm, FReg::Xmm0, FReg::Xmm1, ty);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
    Ok(())
}

fn emit_float_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    load_value_float(asm, layout, lhs, FReg::Xmm0, ty, reg_types)?;
    load_value_float(asm, layout, rhs, FReg::Xmm1, ty, reg_types)?;
    emit_ucomisd(asm, FReg::Xmm0, FReg::Xmm1, ty);
    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0x2,
        CmpKind::Le => 0x6,
        CmpKind::Gt => 0x7,
        CmpKind::Ge => 0x3,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, Reg::R10, Reg::R11);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_mov_mr64(asm, Reg::Rbp, offset, src);
    Ok(())
}

fn store_vreg_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    id: u32,
    src: FReg,
    ty: &LirType,
) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_movsd_m64x(asm, Reg::Rbp, offset, src, ty);
    Ok(())
}

fn is_float_type(ty: &LirType) -> bool {
    matches!(ty, LirType::F32 | LirType::F64)
}

fn is_integer_type(ty: &LirType) -> bool {
    matches!(
        ty,
        LirType::I1
            | LirType::I8
            | LirType::I16
            | LirType::I32
            | LirType::I64
            | LirType::I128
    )
}

fn constant_type(constant: &LirConstant) -> LirType {
    match constant {
        LirConstant::Int(_, ty) => ty.clone(),
        LirConstant::UInt(_, ty) => ty.clone(),
        LirConstant::Float(_, ty) => ty.clone(),
        LirConstant::Bool(_) => LirType::I1,
        LirConstant::String(_) => LirType::Ptr(Box::new(LirType::I8)),
        LirConstant::Null(ty) => ty.clone(),
        LirConstant::Undef(ty) => ty.clone(),
        LirConstant::Array(_, ty) => ty.clone(),
        LirConstant::Struct(_, ty) => ty.clone(),
        LirConstant::GlobalRef(_, ty, _) => ty.clone(),
        LirConstant::FunctionRef(_, ty) => ty.clone(),
    }
}

fn value_type(value: &LirValue, reg_types: &HashMap<u32, LirType>) -> Result<LirType> {
    match value {
        LirValue::Register(id) => reg_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing register type")),
        LirValue::Constant(constant) => Ok(constant_type(constant)),
        LirValue::Null(ty) | LirValue::Undef(ty) => Ok(ty.clone()),
        LirValue::StackSlot(_) => Ok(LirType::Ptr(Box::new(LirType::I8))),
        LirValue::Local(_) => Ok(LirType::Ptr(Box::new(LirType::I8))),
        LirValue::Global(_, ty) => Ok(ty.clone()),
        LirValue::Function(_) => Ok(LirType::Ptr(Box::new(LirType::I8))),
    }
}

fn constant_to_i64(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        _ => Err(Error::from("unsupported constant for x86_64")),
    }
}

fn vreg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .vreg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing vreg slot"))
}

fn stack_slot_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .slot_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing stack slot"))
}

fn emit_ret(asm: &mut Assembler) {
    asm.push(0xC3);
}

fn emit_exit_syscall(asm: &mut Assembler, code: u32) -> Result<()> {
    if code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    emit_mov_imm64(asm, Reg::Rdi, code as u64);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_rr(asm, Reg::Rdi, reg);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

fn emit_mov_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xB8 + (dst.id() & 0x7));
    asm.extend(&imm.to_le_bytes());
}

fn emit_add_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x01);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_sub_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x29);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_imul_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.extend(&[0x0F, 0xAF]);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_add_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 0, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_sub_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_and_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 4, dst.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_or_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x09);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

fn emit_shr_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.push(imm);
}

fn emit_rex(asm: &mut Assembler, w: bool, reg: u8, rm: u8) {
    let mut rex = 0x40;
    if w {
        rex |= 0x08;
    }
    if (reg & 0x8) != 0 {
        rex |= 0x04;
    }
    if (rm & 0x8) != 0 {
        rex |= 0x01;
    }
    asm.push(rex);
}

fn emit_modrm(asm: &mut Assembler, mode: u8, reg: u8, rm: u8) {
    let byte = ((mode & 0x3) << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
    asm.push(byte);
}

fn emit_cmp_rr(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    emit_rex(asm, true, rhs.id(), lhs.id());
    asm.push(0x39);
    emit_modrm(asm, 0b11, rhs.id(), lhs.id());
}

fn emit_cmp_imm32(asm: &mut Assembler, lhs: Reg, imm: i32) {
    emit_rex(asm, true, 0, lhs.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 7, lhs.id());
    asm.extend(&imm.to_le_bytes());
}

fn emit_setcc(asm: &mut Assembler, cc: u8, dst: Reg) {
    emit_rex(asm, false, 0, dst.id());
    asm.push(0x0F);
    asm.push(0x90 + cc);
    emit_modrm(asm, 0b11, 0, dst.id());
}

fn emit_movzx_r64_rm8(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_block(
    asm: &mut Assembler,
    block: &LirBasicBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    layout: &FrameLayout,
    reg_types: &HashMap<u32, LirType>,
    return_ty: &LirType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    for inst in &block.instructions {
        match &inst.kind {
            LirInstructionKind::Add(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for add"))?;
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Add, ty, reg_types)?
            }
            LirInstructionKind::Sub(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for sub"))?;
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Sub, ty, reg_types)?
            }
            LirInstructionKind::Mul(lhs, rhs) => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for mul"))?;
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Mul, ty, reg_types)?
            }
            LirInstructionKind::Eq(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Eq, reg_types)?
            }
            LirInstructionKind::Ne(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Ne, reg_types)?
            }
            LirInstructionKind::Lt(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Lt, reg_types)?
            }
            LirInstructionKind::Le(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Le, reg_types)?
            }
            LirInstructionKind::Gt(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Gt, reg_types)?
            }
            LirInstructionKind::Ge(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Ge, reg_types)?
            }
            LirInstructionKind::Div(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, false, reg_types)?
            }
            LirInstructionKind::Rem(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, true, reg_types)?
            }
            LirInstructionKind::Alloca { .. } => {}
            LirInstructionKind::Load { address, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for load"))?;
                emit_load(asm, layout, inst.id, address, ty, reg_types)?;
            }
            LirInstructionKind::Store { value, address, .. } => {
                emit_store(asm, layout, value, address, reg_types)?;
            }
            LirInstructionKind::Call { function, args, .. } => {
                let ty = inst
                    .type_hint
                    .as_ref()
                    .ok_or_else(|| Error::from("missing type for call"))?;
                emit_call(
                    asm,
                    layout,
                    inst.id,
                    function,
                    args,
                    func_map,
                    ty,
                    reg_types,
                    format,
                )?;
            }
            LirInstructionKind::IntrinsicCall { kind, format: format_str, args } => {
                emit_intrinsic_call(
                    asm,
                    layout,
                    kind,
                    format_str,
                    args,
                    reg_types,
                    rodata,
                    rodata_pool,
                    format,
                )?;
            }
            LirInstructionKind::SIToFP(value, ty) => {
                emit_int_to_float(asm, layout, inst.id, value, ty, reg_types, true)?;
            }
            LirInstructionKind::UIToFP(value, ty) => {
                emit_int_to_float(asm, layout, inst.id, value, ty, reg_types, false)?;
            }
            LirInstructionKind::FPToSI(value, ty) => {
                emit_float_to_int(asm, layout, inst.id, value, ty, reg_types, true)?;
            }
            LirInstructionKind::FPToUI(value, ty) => {
                emit_float_to_int(asm, layout, inst.id, value, ty, reg_types, false)?;
            }
            LirInstructionKind::FPTrunc(value, ty) => {
                emit_fp_trunc(asm, layout, inst.id, value, ty, reg_types)?;
            }
            LirInstructionKind::FPExt(value, ty) => {
                emit_fp_ext(asm, layout, inst.id, value, ty, reg_types)?;
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported LIR instruction for x86_64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm64(asm, Reg::Rax, 0);
                emit_ret(asm);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if is_float_type(return_ty) {
                load_value_float(asm, layout, value, FReg::Xmm0, return_ty, reg_types)?;
            } else {
                load_value(asm, layout, value, Reg::Rax, reg_types)?;
                exit_reg = Some(Reg::Rax);
            }
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                if let Some(reg) = exit_reg {
                    emit_exit_syscall_reg(asm, reg)?;
                } else {
                    emit_exit_syscall(asm, 0)?;
                }
            } else {
                emit_ret(asm);
            }
        }
        LirTerminator::Br(target) => {
            asm.emit_jmp(Label::Block(asm.current_function, *target));
        }
        LirTerminator::CondBr {
            condition,
            if_true,
            if_false,
        } => {
            emit_cond_branch(
                asm,
                layout,
                condition,
                Label::Block(asm.current_function, *if_true),
                Label::Block(asm.current_function, *if_false),
            )?;
        }
        _ => {}
    }

    Ok(())
}

enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

fn emit_cmp(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    kind: CmpKind,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types)?;
    if is_float_type(&lhs_ty) {
        emit_float_cmp(asm, layout, dst_id, lhs, rhs, kind, &lhs_ty, reg_types)?;
        return Ok(());
    }
    match (lhs, rhs) {
        (LirValue::Register(_), LirValue::Register(_))
        | (LirValue::Register(_), LirValue::Constant(_))
        | (LirValue::Constant(_), LirValue::Register(_))
        | (LirValue::Constant(_), LirValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::R10, reg_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11, reg_types)?;
            emit_cmp_rr(asm, Reg::R10, Reg::R11);
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            let imm32 = i32::try_from(imm).map_err(|_| Error::from("cmp immediate out of range"))?;
            emit_cmp_imm32(asm, Reg::R10, imm32);
        }
        _ => {}
    }

    let cc = match kind {
        CmpKind::Eq => 0x4,
        CmpKind::Ne => 0x5,
        CmpKind::Lt => 0xC,
        CmpKind::Le => 0xE,
        CmpKind::Gt => 0xF,
        CmpKind::Ge => 0xD,
    };
    emit_setcc(asm, cc, Reg::R11);
    emit_movzx_r64_rm8(asm, Reg::R10, Reg::R11);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_cond_branch(
    asm: &mut Assembler,
    layout: &FrameLayout,
    condition: &LirValue,
    if_true: Label,
    if_false: Label,
) -> Result<()> {
    match condition {
        LirValue::Constant(LirConstant::Bool(value)) => {
            if *value {
                asm.emit_jmp(if_true);
            } else {
                asm.emit_jmp(if_false);
            }
        }
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            emit_cmp_imm32(asm, Reg::R10, 0);
            asm.emit_jcc(0x85, if_true);
            asm.emit_jmp(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}

struct Fixup {
    pos: usize,
    target: Label,
}

struct Assembler {
    buf: Vec<u8>,
    labels: HashMap<Label, usize>,
    fixups: Vec<Fixup>,
    needs_frame: bool,
    current_function: u32,
    relocs: Vec<Relocation>,
}

fn emit_prologue(asm: &mut Assembler, layout: &FrameLayout) -> Result<()> {
    asm.push(0x55);
    emit_rex(asm, true, Reg::Rbp.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, Reg::Rbp.id(), Reg::Rsp.id());

    if layout.frame_size > 0 {
        emit_sub_ri32(asm, Reg::Rsp, layout.frame_size);
    }
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler) {
    emit_mov_rr(asm, Reg::Rsp, Reg::Rbp);
    asm.push(0x5D);
}

impl Assembler {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
            labels: HashMap::new(),
            fixups: Vec::new(),
            needs_frame: false,
            current_function: 0,
            relocs: Vec::new(),
        }
    }

    fn bind(&mut self, label: Label) {
        if let Label::Function(id) = label {
            self.current_function = id;
        }
        self.labels.insert(label, self.buf.len());
    }

    fn is_entry(&self) -> bool {
        self.current_function == 0
    }

    fn emit_jmp(&mut self, target: Label) {
        self.buf.push(0xE9);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn emit_jcc(&mut self, opcode: u8, target: Label) {
        self.buf.push(0x0F);
        self.buf.push(opcode);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup {
            pos,
            target,
        });
    }

    fn emit_call(&mut self, target: Label) {
        self.buf.push(0xE8);
        let pos = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.fixups.push(Fixup { pos, target });
    }

    fn emit_call_external(&mut self, symbol: &str) {
        self.buf.push(0xE8);
        let offset = self.buf.len();
        self.buf.extend_from_slice(&0i32.to_le_bytes());
        self.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::CallRel32,
            symbol: symbol.to_string(),
            addend: 0,
        });
    }

    fn emit_mov_imm64_reloc(&mut self, dst: Reg, symbol: &str, addend: i64) {
        emit_rex(self, true, 0, dst.id());
        self.buf.push(0xB8 + (dst.id() & 0x7));
        let offset = self.buf.len();
        self.buf.extend_from_slice(&0u64.to_le_bytes());
        self.relocs.push(Relocation {
            offset: offset as u64,
            kind: RelocKind::Abs64,
            symbol: symbol.to_string(),
            addend,
        });
    }

    fn push(&mut self, byte: u8) {
        self.buf.push(byte);
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<(Vec<u8>, Vec<Relocation>)> {
        for fixup in &self.fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown jump target"))?;
            let origin = fixup.pos;
            let rel = (*target as i64) - (origin as i64 + 4);
            let rel32 = i32::try_from(rel).map_err(|_| Error::from("jump out of range"))?;
            self.buf[origin..origin + 4].copy_from_slice(&rel32.to_le_bytes());
        }
        Ok((self.buf, self.relocs))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Label {
    Function(u32),
    Block(u32, BasicBlockId),
}

fn build_function_map(program: &LirProgram) -> Result<HashMap<String, u32>> {
    let mut map = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        let name = String::from(func.name.clone());
        map.insert(name, idx as u32);
    }
    Ok(map)
}

fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &LirValue,
    ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_movsd_xm64(asm, FReg::Xmm0, Reg::Rbp, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ty)?;
            } else {
                emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
                store_vreg(asm, layout, dst_id, Reg::R10)?;
            }
            Ok(())
        }
        _ => {
            let addr_ty = value_type(address, reg_types)?;
            Err(Error::from(format!(
                "unsupported load address for x86_64: {:?}",
                addr_ty
            )))
        }
    }
}

fn emit_divrem(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    lhs: &LirValue,
    rhs: &LirValue,
    want_rem: bool,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let lhs_ty = value_type(lhs, reg_types)?;
    if is_float_type(&lhs_ty) {
        if want_rem {
            return Err(Error::from("float remainder is not supported on x86_64"));
        }
        emit_float_div(asm, layout, dst_id, lhs, rhs, &lhs_ty, reg_types)?;
        return Ok(());
    }

    load_value(asm, layout, lhs, Reg::Rax, reg_types)?;
    load_value(asm, layout, rhs, Reg::R11, reg_types)?;

    emit_cqo(asm);
    emit_idiv_reg(asm, Reg::R11);

    let src = if want_rem { Reg::Rdx } else { Reg::Rax };
    store_vreg(asm, layout, dst_id, src)?;

    Ok(())
}

enum CallTarget {
    Internal(u32),
    External(String),
}

const SYSV_INT_ARGS: [Reg; 6] = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];
const SYSV_FLOAT_ARGS: [FReg; 8] = [
    FReg::Xmm0,
    FReg::Xmm1,
    FReg::Xmm2,
    FReg::Xmm3,
    FReg::Xmm4,
    FReg::Xmm5,
    FReg::Xmm6,
    FReg::Xmm7,
];
const WIN_INT_ARGS: [Reg; 4] = [Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9];
const WIN_FLOAT_ARGS: [FReg; 4] = [FReg::Xmm0, FReg::Xmm1, FReg::Xmm2, FReg::Xmm3];

fn call_abi(format: TargetFormat) -> (&'static [Reg], &'static [FReg], bool) {
    match format {
        TargetFormat::Coff => (&WIN_INT_ARGS, &WIN_FLOAT_ARGS, false),
        _ => (&SYSV_INT_ARGS, &SYSV_FLOAT_ARGS, true),
    }
}

fn emit_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    function: &LirValue,
    args: &[LirValue],
    func_map: &HashMap<String, u32>,
    ret_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    format: TargetFormat,
) -> Result<()> {
    let target = match function {
        LirValue::Function(name) => func_map
            .get(name)
            .copied()
            .map(CallTarget::Internal)
            .unwrap_or_else(|| CallTarget::External(name.clone())),
        _ => return Err(Error::from("unsupported callee for x86_64")),
    };

    let (arg_regs, float_regs, use_al) = call_abi(format);

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    for arg in args {
        let arg_ty = value_type(arg, reg_types)?;
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(
                    asm,
                    layout,
                    arg,
                    float_regs[float_idx],
                    &arg_ty,
                    reg_types,
                )?;
                float_idx += 1;
            } else {
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types)?;
            int_idx += 1;
        } else {
            let offset = layout.shadow_space + (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
            stack_idx += 1;
        }
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    match target {
        CallTarget::Internal(target) => asm.emit_call(Label::Function(target)),
        CallTarget::External(symbol) => asm.emit_call_external(&symbol),
    }

    if !matches!(ret_ty, LirType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::Xmm0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::Rax)?;
        }
    }

    Ok(())
}

fn emit_intrinsic_call(
    asm: &mut Assembler,
    layout: &FrameLayout,
    kind: &LirIntrinsicKind,
    format: &str,
    args: &[LirValue],
    reg_types: &HashMap<u32, LirType>,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
    target_format: TargetFormat,
) -> Result<()> {
    match kind {
        LirIntrinsicKind::Print | LirIntrinsicKind::Println => {}
        LirIntrinsicKind::Format | LirIntrinsicKind::TimeNow => {
            return Err(Error::from("unsupported intrinsic for x86_64"))
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    let (arg_regs, float_regs, use_al) = call_abi(target_format);

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    if int_idx < arg_regs.len() {
        asm.emit_mov_imm64_reloc(arg_regs[int_idx], ".rodata", format_offset as i64);
        int_idx += 1;
    } else {
        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", format_offset as i64);
        let offset = layout.shadow_space + (stack_idx as i32) * 8;
        emit_mov_mr64_sp(asm, offset, Reg::R10);
        stack_idx += 1;
    }

    for arg in args {
        let arg_ty = value_type(arg, reg_types)?;
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(asm, layout, arg, float_regs[float_idx], &arg_ty, reg_types)?;
                float_idx += 1;
            } else {
                let offset = layout.shadow_space + (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types)?;
            int_idx += 1;
        } else {
            let offset = layout.shadow_space + (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
            stack_idx += 1;
        }
    }

    if use_al {
        emit_mov_al_imm8(asm, float_idx as u8);
    }

    asm.emit_call_external("printf");
    Ok(())
}

fn store_outgoing_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &LirValue,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    let ty = value_type(value, reg_types)?;
    if is_float_type(&ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &ty, reg_types)?;
        emit_movsd_m64x_sp(asm, offset, FReg::Xmm0, &ty);
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types)?;
        emit_mov_mr64_sp(asm, offset, Reg::R10);
    }
    Ok(())
}

fn intern_cstring(
    rodata: &mut Vec<u8>,
    pool: &mut HashMap<String, u64>,
    text: &str,
) -> u64 {
    if let Some(offset) = pool.get(text) {
        return *offset;
    }
    align_rodata(rodata, 8);
    let offset = rodata.len() as u64;
    rodata.extend_from_slice(text.as_bytes());
    rodata.push(0);
    pool.insert(text.to_string(), offset);
    offset
}

fn align_rodata(rodata: &mut Vec<u8>, align: usize) {
    while rodata.len() % align != 0 {
        rodata.push(0);
    }
}

fn emit_store(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    address: &LirValue,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let value_ty = value_type(value, reg_types)?;
    if is_float_type(&value_ty) {
        load_value_float(asm, layout, value, FReg::Xmm0, &value_ty, reg_types)?;
    } else {
        load_value(asm, layout, value, Reg::R10, reg_types)?;
    }

    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                emit_movsd_m64x(asm, Reg::Rbp, offset, FReg::Xmm0, &value_ty);
            } else {
                emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for x86_64")),
    }
}

fn emit_mov_rm64(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x8B);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_mov_mr64(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_mov_mr64_sp(asm: &mut Assembler, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

fn emit_movsd_m64x_sp(asm: &mut Assembler, disp: i32, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), Reg::Rsp.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

fn emit_movsd_xm64(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x10);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

fn emit_movsd_m64x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

fn emit_movq_xmm_r64(asm: &mut Assembler, dst: FReg, src: Reg) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x6E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_addsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x58);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_subsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_mulsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x59);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_divsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_ucomisd(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &LirType) {
    if matches!(ty, LirType::F64) {
        asm.push(0x66);
    }
    emit_rex(asm, false, lhs.id(), rhs.id());
    asm.push(0x0F);
    asm.push(0x2E);
    emit_modrm(asm, 0b11, lhs.id(), rhs.id());
}

fn emit_cvtsi2sd(asm: &mut Assembler, dst: FReg, src: Reg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvttsd2si(asm: &mut Assembler, dst: Reg, src: FReg, ty: &LirType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvtsd2ss(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF2);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_cvtss2sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF3);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

fn emit_mov_al_imm8(asm: &mut Assembler, imm: u8) {
    asm.push(0xB0);
    asm.push(imm);
}

fn emit_int_to_float(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    signed: bool,
) -> Result<()> {
    let src_ty = value_type(value, reg_types)?;
    if !is_integer_type(&src_ty) {
        return Err(Error::from("int to float expects integer source"));
    }
    if matches!(src_ty, LirType::I128) {
        return Err(Error::from("i128 to float is not supported on x86_64"));
    }
    load_value(asm, layout, value, Reg::R10, reg_types)?;
    if signed {
        emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R10, dst_ty);
        store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
        return Ok(());
    }
    emit_uint_to_float(asm, Reg::R10, dst_ty)?;
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_uint_to_float(asm: &mut Assembler, src: Reg, dst_ty: &LirType) -> Result<()> {
    emit_mov_rr(asm, Reg::R11, src);
    emit_shr_imm8(asm, Reg::R11, 1);
    emit_and_ri32(asm, src, 1);
    emit_or_rr(asm, Reg::R11, src);
    emit_cvtsi2sd(asm, FReg::Xmm0, Reg::R11, &LirType::F64);
    emit_addsd(asm, FReg::Xmm0, FReg::Xmm0, &LirType::F64);
    if matches!(dst_ty, LirType::F32) {
        emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    }
    Ok(())
}

fn emit_float_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    _signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    if matches!(dst_ty, LirType::I128) {
        return Err(Error::from("i128 from float is not supported on x86_64"));
    }
    let src_ty = value_type(value, reg_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types)?;
    emit_cvttsd2si(asm, Reg::R10, FReg::Xmm0, &src_ty);
    store_vreg(asm, layout, dst_id, Reg::R10)?;
    Ok(())
}

fn emit_fp_trunc(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types)?;
    if !matches!((&src_ty, dst_ty), (LirType::F64, LirType::F32)) {
        return Err(Error::from("unsupported FPTrunc on x86_64"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types)?;
    emit_cvtsd2ss(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_fp_ext(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
) -> Result<()> {
    let src_ty = value_type(value, reg_types)?;
    if !matches!((&src_ty, dst_ty), (LirType::F32, LirType::F64)) {
        return Err(Error::from("unsupported FPExt on x86_64"));
    }
    load_value_float(asm, layout, value, FReg::Xmm0, &src_ty, reg_types)?;
    emit_cvtss2sd(asm, FReg::Xmm0, FReg::Xmm0);
    store_vreg_float(asm, layout, dst_id, FReg::Xmm0, dst_ty)?;
    Ok(())
}

fn emit_float_prefix(asm: &mut Assembler, ty: &LirType) {
    match ty {
        LirType::F32 => asm.push(0xF3),
        LirType::F64 => asm.push(0xF2),
        _ => {}
    }
}

fn emit_modrm_disp32(asm: &mut Assembler, reg: u8, rm: u8, disp: i32) {
    emit_modrm(asm, 0b10, reg, rm);
    asm.extend(&disp.to_le_bytes());
}

fn emit_sib(asm: &mut Assembler, scale: u8, index: u8, base: u8) {
    let byte = ((scale & 0x3) << 6) | ((index & 0x7) << 3) | (base & 0x7);
    asm.push(byte);
}

fn emit_cqo(asm: &mut Assembler) {
    emit_rex(asm, true, 0, 0);
    asm.push(0x99);
}

fn emit_idiv_reg(asm: &mut Assembler, divisor: Reg) {
    emit_rex(asm, true, 7, divisor.id());
    asm.push(0xF7);
    emit_modrm(asm, 0b11, 7, divisor.id());
}
