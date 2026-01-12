use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirIntrinsicKind,
    LirProgram, LirTerminator, LirType, LirValue,
};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, RelocKind, Relocation, TargetFormat};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X16,
    X17,
    X18,
    X29,
    X30,
    X31,
}

impl Reg {
    fn id(self) -> u32 {
        match self {
            Reg::X0 => 0,
            Reg::X1 => 1,
            Reg::X2 => 2,
            Reg::X3 => 3,
            Reg::X4 => 4,
            Reg::X5 => 5,
            Reg::X6 => 6,
            Reg::X7 => 7,
            Reg::X8 => 8,
            Reg::X16 => 16,
            Reg::X17 => 17,
            Reg::X18 => 18,
            Reg::X29 => 29,
            Reg::X30 => 30,
            Reg::X31 => 31,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FReg {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
}

impl FReg {
    fn id(self) -> u32 {
        match self {
            FReg::V0 => 0,
            FReg::V1 => 1,
            FReg::V2 => 2,
            FReg::V3 => 3,
            FReg::V4 => 4,
            FReg::V5 => 5,
            FReg::V6 => 6,
            FReg::V7 => 7,
        }
    }
}

fn build_frame_layout(func: &LirFunction) -> Result<FrameLayout> {
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

    let outgoing_size = (max_call_args.saturating_sub(8) as i32) * 8;
    let mut vreg_offsets = HashMap::new();
    let mut slot_offsets = HashMap::new();
    let mut offset = outgoing_size;

    for id in vreg_ids {
        vreg_offsets.insert(id, offset);
        offset += 8;
    }

    for slot in &func.stack_slots {
        slot_offsets.insert(slot.id, offset);
        offset += 8;
    }

    let local_size = offset - outgoing_size;
    let base = outgoing_size + local_size;
    let frame_size = if base == 0 && !has_calls {
        0
    } else {
        align16(base + 16)
    };

    Ok(FrameLayout {
        vreg_offsets,
        slot_offsets,
        outgoing_size,
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

struct FrameLayout {
    vreg_offsets: HashMap<u32, i32>,
    slot_offsets: HashMap<u32, i32>,
    outgoing_size: i32,
    frame_size: i32,
}

pub fn emit_text(program: &LirProgram, format: TargetFormat) -> Result<CodegenOutput> {
    let func_map = build_function_map(program)?;
    let mut asm = Assembler::new();
    let mut rodata = Vec::new();
    let mut rodata_pool = HashMap::new();

    for (index, func) in program.functions.iter().enumerate() {
        asm.bind(Label::Function(index as u32));
        let layout = build_frame_layout(func)?;
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
        load_value_float(asm, layout, lhs, FReg::V0, ty, reg_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, ty, reg_types)?;
        match op {
            BinOp::Add => emit_fadd(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Sub => emit_fsub(asm, FReg::V0, FReg::V0, FReg::V1, ty),
            BinOp::Mul => emit_fmul(asm, FReg::V0, FReg::V0, FReg::V1, ty),
        }
        store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
        return Ok(());
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17, reg_types)?;
            match op {
                BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
            }
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                emit_mov_imm16(asm, Reg::X17, (imm as u64 & 0xffff) as u16);
                match op {
                    BinOp::Add => emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Sub => emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                    BinOp::Mul => emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17),
                }
            } else {
                match op {
                    BinOp::Add => emit_add_imm12(asm, Reg::X16, Reg::X16, imm as u32),
                    BinOp::Sub => emit_sub_imm12(asm, Reg::X16, Reg::X16, imm as u32),
                    BinOp::Mul => {
                        emit_mov_imm16(asm, Reg::X17, imm as u16);
                        emit_mul_reg(asm, Reg::X16, Reg::X16, Reg::X17);
                    }
                }
            }
        }
        _ => return Err(Error::from("unsupported RHS for aarch64")),
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
            emit_load_from_sp(asm, dst, offset);
            Ok(())
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > u16::MAX as i64 {
                return Err(Error::from("aarch64 immediate out of range"));
            }
            emit_mov_imm16(asm, dst, imm as u16);
            Ok(())
        }
        _ => {
            let ty = value_type(value, reg_types)?;
            Err(Error::from(format!("unsupported value for aarch64: {:?}", ty)))
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
            emit_load_float_from_sp(asm, dst, offset, ty);
            Ok(())
        }
        LirValue::Constant(LirConstant::Float(value, _)) => {
            if matches!(ty, LirType::F32) {
                let bits = (*value as f32).to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_fmov_s_from_w(asm, dst, Reg::X16);
            } else {
                let bits = value.to_bits();
                emit_mov_imm16(asm, Reg::X16, (bits & 0xffff) as u16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 16) & 0xffff) as u16, 16);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 32) & 0xffff) as u16, 32);
                emit_movk_imm16(asm, Reg::X16, ((bits >> 48) & 0xffff) as u16, 48);
                emit_fmov_d_from_x(asm, dst, Reg::X16);
            }
            Ok(())
        }
        _ => {
            let actual = value_type(value, reg_types)?;
            Err(Error::from(format!(
                "unsupported float value for aarch64: {:?}",
                actual
            )))
        }
    }
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_store_to_sp(asm, src, offset);
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
    emit_store_float_to_sp(asm, src, offset, ty);
    Ok(())
}

fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &LirValue,
    ty: &LirType,
) -> Result<()> {
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(ty) {
                emit_load_float_from_sp(asm, FReg::V0, offset, ty);
                store_vreg_float(asm, layout, dst_id, FReg::V0, ty)?;
            } else {
                emit_load_from_sp(asm, Reg::X16, offset);
                store_vreg(asm, layout, dst_id, Reg::X16)?;
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for aarch64")),
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
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            if is_float_type(&value_ty) {
                load_value_float(asm, layout, value, FReg::V0, &value_ty, reg_types)?;
                emit_store_float_to_sp(asm, FReg::V0, offset, &value_ty);
            } else {
                load_value(asm, layout, value, Reg::X16, reg_types)?;
                emit_store_to_sp(asm, Reg::X16, offset);
            }
            Ok(())
        }
        _ => Err(Error::from("unsupported store address for aarch64")),
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
            return Err(Error::from("float remainder is not supported on aarch64"));
        }
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types)?;
        emit_fdiv(asm, FReg::V0, FReg::V0, FReg::V1, &lhs_ty);
        store_vreg_float(asm, layout, dst_id, FReg::V0, &lhs_ty)?;
        return Ok(());
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types)?;
    load_value(asm, layout, rhs, Reg::X17, reg_types)?;

    emit_sdiv(asm, Reg::X18, Reg::X16, Reg::X17);

    if want_rem {
        emit_msub(asm, Reg::X16, Reg::X18, Reg::X17, Reg::X16);
    } else {
        emit_mov_reg(asm, Reg::X16, Reg::X18);
    }
    store_vreg(asm, layout, dst_id, Reg::X16)?;

    Ok(())
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
        _ => return Err(Error::from("unsupported callee for aarch64")),
    };

    let arg_regs = [
        Reg::X0, Reg::X1, Reg::X2, Reg::X3, Reg::X4, Reg::X5, Reg::X6, Reg::X7,
    ];
    let float_regs = [
        FReg::V0, FReg::V1, FReg::V2, FReg::V3, FReg::V4, FReg::V5, FReg::V6, FReg::V7,
    ];

    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    for arg in args {
        let arg_ty = value_type(arg, reg_types)?;
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(asm, layout, arg, float_regs[float_idx], &arg_ty, reg_types)?;
                float_idx += 1;
            } else {
                let offset = (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types)?;
            int_idx += 1;
        } else {
            let offset = (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
            stack_idx += 1;
        }
    }

    match target {
        CallTarget::Internal(id) => asm.emit_bl(Label::Function(id)),
        CallTarget::External(name) => {
            if matches!(format, TargetFormat::Coff) {
                asm.emit_bl_external(&name);
            } else {
                asm.emit_bl_external(&name);
            }
        }
    }

    if !matches!(ret_ty, LirType::Void) {
        if is_float_type(ret_ty) {
            store_vreg_float(asm, layout, dst_id, FReg::V0, ret_ty)?;
        } else {
            store_vreg(asm, layout, dst_id, Reg::X0)?;
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
            return Err(Error::from("unsupported intrinsic for aarch64"))
        }
    }

    let format_offset = intern_cstring(rodata, rodata_pool, format);
    emit_load_rodata_addr(asm, Reg::X0, format_offset as i64)?;

    let arg_regs = [
        Reg::X0, Reg::X1, Reg::X2, Reg::X3, Reg::X4, Reg::X5, Reg::X6, Reg::X7,
    ];
    let float_regs = [
        FReg::V0, FReg::V1, FReg::V2, FReg::V3, FReg::V4, FReg::V5, FReg::V6, FReg::V7,
    ];

    let mut int_idx = 1usize;
    let mut float_idx = 0usize;
    let mut stack_idx = 0usize;

    for arg in args {
        let arg_ty = value_type(arg, reg_types)?;
        if is_float_type(&arg_ty) {
            if float_idx < float_regs.len() {
                load_value_float(asm, layout, arg, float_regs[float_idx], &arg_ty, reg_types)?;
                float_idx += 1;
            } else {
                let offset = (stack_idx as i32) * 8;
                store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
                stack_idx += 1;
            }
        } else if int_idx < arg_regs.len() {
            load_value(asm, layout, arg, arg_regs[int_idx], reg_types)?;
            int_idx += 1;
        } else {
            let offset = (stack_idx as i32) * 8;
            store_outgoing_arg(asm, layout, offset, arg, reg_types)?;
            stack_idx += 1;
        }
    }

    if matches!(target_format, TargetFormat::Coff) {
        asm.emit_bl_external("printf");
    } else {
        asm.emit_bl_external("printf");
    }
    Ok(())
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
    load_value(asm, layout, value, Reg::X16, reg_types)?;
    emit_scvtf(asm, FReg::V0, Reg::X16, dst_ty, signed);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
    Ok(())
}

fn emit_float_to_int(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    value: &LirValue,
    dst_ty: &LirType,
    reg_types: &HashMap<u32, LirType>,
    signed: bool,
) -> Result<()> {
    if !is_integer_type(dst_ty) {
        return Err(Error::from("float to int expects integer destination"));
    }
    let src_ty = value_type(value, reg_types)?;
    if !is_float_type(&src_ty) {
        return Err(Error::from("float to int expects float source"));
    }
    load_value_float(asm, layout, value, FReg::V0, &src_ty, reg_types)?;
    emit_fcvtzs(asm, Reg::X16, FReg::V0, &src_ty, signed);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
        return Err(Error::from("unsupported FPTrunc on aarch64"));
    }
    load_value_float(asm, layout, value, FReg::V0, &src_ty, reg_types)?;
    emit_fcvt_sd(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
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
        return Err(Error::from("unsupported FPExt on aarch64"));
    }
    load_value_float(asm, layout, value, FReg::V0, &src_ty, reg_types)?;
    emit_fcvt_ds(asm, FReg::V0, FReg::V0);
    store_vreg_float(asm, layout, dst_id, FReg::V0, dst_ty)?;
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

fn emit_load_rodata_addr(asm: &mut Assembler, dst: Reg, addend: i64) -> Result<()> {
    if asm.buf.len() % 8 != 0 {
        emit_nop(asm);
    }
    let ldr_instr = 0x5800_0000u32 | ((2u32 & 0x7ffff) << 5) | dst.id();
    asm.emit_u32(ldr_instr);
    let b_instr = 0x1400_0000u32 | 3;
    asm.emit_u32(b_instr);
    let literal_offset = asm.buf.len();
    asm.relocs.push(Relocation {
        offset: literal_offset as u64,
        kind: RelocKind::Abs64,
        symbol: ".rodata".to_string(),
        addend,
    });
    asm.extend(&0u64.to_le_bytes());
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
        load_value_float(asm, layout, value, FReg::V0, &ty, reg_types)?;
        emit_store_float_to_sp(asm, FReg::V0, offset, &ty);
    } else {
        load_value(asm, layout, value, Reg::X16, reg_types)?;
        emit_store_to_sp(asm, Reg::X16, offset);
    }
    Ok(())
}

fn constant_to_i64(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        LirConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        _ => Err(Error::from("unsupported constant for aarch64")),
    }
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

fn emit_mov_reg(asm: &mut Assembler, dst: Reg, src: Reg) {
    if dst.id() == src.id() {
        return;
    }
    let instr = if dst.is_sp() || src.is_sp() {
        0x9100_0000u32 | (src.id() << 5) | dst.id()
    } else {
        0xAA00_03E0u32 | (src.id() << 16) | dst.id()
    };
    asm.extend(&instr.to_le_bytes());
}

fn emit_mov_imm16(asm: &mut Assembler, dst: Reg, imm: u16) {
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_movk_imm16(asm: &mut Assembler, dst: Reg, imm: u16, shift: u32) {
    let hw = (shift / 16) & 0x3;
    let instr = 0xF280_0000u32 | ((imm as u32) << 5) | (hw << 21) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmov_d_from_x(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x9E67_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmov_s_from_w(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x1E27_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8B00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_mul_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9B00_7C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0x9100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_sub_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0xD100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_ret(asm: &mut Assembler) {
    asm.extend(&0xD65F_03C0u32.to_le_bytes());
}

fn emit_exit_syscall(asm: &mut Assembler, code: u16) -> Result<()> {
    emit_mov_imm16(asm, Reg::X0, code);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_reg(asm, Reg::X0, reg);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

fn emit_svc(asm: &mut Assembler) {
    asm.extend(&0xD400_0001u32.to_le_bytes());
}

fn emit_sub_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0xD100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_add_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0x9100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

fn emit_sdiv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9AC0_0C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_msub(asm: &mut Assembler, dst: Reg, mul_lhs: Reg, mul_rhs: Reg, add: Reg) {
    let instr = 0x9B00_0000u32
        | (mul_rhs.id() << 16)
        | (mul_lhs.id() << 5)
        | dst.id()
        | (add.id() << 10);
    asm.extend(&instr.to_le_bytes());
}
fn emit_load_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) {
    let imm12 = ((offset / 8) as u32) & 0xfff;
    let instr = 0xF940_03E0u32 | (imm12 << 10) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_to_sp(asm: &mut Assembler, src: Reg, offset: i32) {
    let imm12 = ((offset / 8) as u32) & 0xfff;
    let instr = 0xF900_03E0u32 | (imm12 << 10) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_float_from_sp(asm: &mut Assembler, dst: FReg, offset: i32, ty: &LirType) {
    let scale = if matches!(ty, LirType::F32) { 4 } else { 8 };
    let imm12 = ((offset / scale) as u32) & 0xfff;
    let base = if matches!(ty, LirType::F32) {
        0xBD40_03E0u32
    } else {
        0xFD40_03E0u32
    };
    let instr = base | (imm12 << 10) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_float_to_sp(asm: &mut Assembler, src: FReg, offset: i32, ty: &LirType) {
    let scale = if matches!(ty, LirType::F32) { 4 } else { 8 };
    let imm12 = ((offset / scale) as u32) & 0xfff;
    let base = if matches!(ty, LirType::F32) {
        0xBD00_03E0u32
    } else {
        0xFD00_03E0u32
    };
    let instr = base | (imm12 << 10) | src.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_store_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (a.id() << 5) | 31;
    asm.extend(&instr.to_le_bytes());
}

fn emit_load_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (a.id() << 5) | 31;
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_reg(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_001F | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cmp_imm12(asm: &mut Assembler, lhs: Reg, imm12: u32) {
    let instr = 0xF100_001F | ((imm12 & 0xfff) << 10) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_cset(asm: &mut Assembler, dst: Reg, cond: u32) {
    let instr = 0x9A9F_07E0u32 | ((cond & 0xF) << 12) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fadd(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_2800u32
    } else {
        0x1E60_2800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fsub(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_3800u32
    } else {
        0x1E60_3800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fmul(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_0800u32
    } else {
        0x1E60_0800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fdiv(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E20_1800u32
    } else {
        0x1E60_1800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcmp(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &LirType) {
    let base = if matches!(ty, LirType::F32) {
        0x1E21_2000u32
    } else {
        0x1E60_2000u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvt_sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E62_4000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvt_ds(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E22_C000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_scvtf(asm: &mut Assembler, dst: FReg, src: Reg, ty: &LirType, signed: bool) {
    let base = match (ty, signed) {
        (LirType::F32, true) => 0x1E22_0000u32,
        (LirType::F32, false) => 0x1E23_0000u32,
        (LirType::F64, true) => 0x9E62_0000u32,
        (LirType::F64, false) => 0x9E63_0000u32,
        _ => 0x9E62_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_fcvtzs(asm: &mut Assembler, dst: Reg, src: FReg, ty: &LirType, signed: bool) {
    let base = match (ty, signed) {
        (LirType::F32, true) => 0x1E38_0000u32,
        (LirType::F32, false) => 0x1E39_0000u32,
        (LirType::F64, true) => 0x9E78_0000u32,
        (LirType::F64, false) => 0x9E79_0000u32,
        _ => 0x9E78_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

fn emit_nop(asm: &mut Assembler) {
    asm.extend(&0xD503_201Fu32.to_le_bytes());
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
                emit_load(asm, layout, inst.id, address, ty)?;
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
                    "unsupported LIR instruction for aarch64: {other:?}"
                )));
            }
        }
    }

    match &block.terminator {
        LirTerminator::Return(None) => {
            if asm.needs_frame {
                emit_epilogue(asm, layout);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm16(asm, Reg::X0, 0);
                emit_ret(asm);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let mut exit_reg = None;
            if is_float_type(return_ty) {
                load_value_float(asm, layout, value, FReg::V0, return_ty, reg_types)?;
            } else {
                load_value(asm, layout, value, Reg::X0, reg_types)?;
                exit_reg = Some(Reg::X0);
            }
            if asm.needs_frame {
                emit_epilogue(asm, layout);
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
            asm.emit_b(Label::Block(asm.current_function, *target));
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
        load_value_float(asm, layout, lhs, FReg::V0, &lhs_ty, reg_types)?;
        load_value_float(asm, layout, rhs, FReg::V1, &lhs_ty, reg_types)?;
        emit_fcmp(asm, FReg::V0, FReg::V1, &lhs_ty);
        let cond = match kind {
            CmpKind::Eq => 0,
            CmpKind::Ne => 1,
            CmpKind::Lt => 11,
            CmpKind::Le => 13,
            CmpKind::Gt => 12,
            CmpKind::Ge => 10,
        };
        emit_cset(asm, Reg::X16, cond);
        store_vreg(asm, layout, dst_id, Reg::X16)?;
        return Ok(());
    }
    match (lhs, rhs) {
        (LirValue::Register(_), LirValue::Register(_))
        | (LirValue::Register(_), LirValue::Constant(_))
        | (LirValue::Constant(_), LirValue::Register(_))
        | (LirValue::Constant(_), LirValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::X16, reg_types)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17, reg_types)?;
            emit_cmp_reg(asm, Reg::X16, Reg::X17);
        }
        LirValue::Constant(constant) => {
            let imm = constant_to_i64(constant)?;
            if imm < 0 || imm > 4095 {
                return Err(Error::from("cmp immediate out of range"));
            }
            emit_cmp_imm12(asm, Reg::X16, imm as u32);
        }
        _ => {}
    }

    let cond = match kind {
        CmpKind::Eq => 0,
        CmpKind::Ne => 1,
        CmpKind::Lt => 11,
        CmpKind::Le => 13,
        CmpKind::Gt => 12,
        CmpKind::Ge => 10,
    };
    emit_cset(asm, Reg::X16, cond);
    store_vreg(asm, layout, dst_id, Reg::X16)?;
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
                asm.emit_b(if_true);
            } else {
                asm.emit_b(if_false);
            }
        }
        LirValue::Register(id) => {
            let offset = vreg_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            emit_cmp_imm12(asm, Reg::X16, 0);
            asm.emit_b_cond(1, if_true);
            asm.emit_b(if_false);
        }
        _ => return Err(Error::from("unsupported condition value")),
    }
    Ok(())
}

#[derive(Clone, Copy, Debug)]
struct Fixup {
    pos: usize,
    target: Label,
    kind: FixupKind,
}

#[derive(Clone, Copy, Debug)]
enum FixupKind {
    B,
    BCond(u32),
    Bl,
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
    let frame = layout.frame_size as u32;
    emit_sub_sp(asm, frame);
    emit_store_pair(asm, Reg::X29, Reg::X30, layout.frame_size - 16);
    emit_mov_reg(asm, Reg::X29, Reg::X31);
    Ok(())
}

fn emit_epilogue(asm: &mut Assembler, layout: &FrameLayout) {
    emit_mov_reg(asm, Reg::X31, Reg::X29);
    emit_load_pair(asm, Reg::X29, Reg::X30, layout.frame_size - 16);
    emit_add_sp(asm, layout.frame_size as u32);
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

    fn emit_b(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x1400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::B,
        });
    }

    fn emit_b_cond(&mut self, cond: u32, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x5400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::BCond(cond),
        });
    }

    fn emit_bl(&mut self, target: Label) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.fixups.push(Fixup {
            pos,
            target,
            kind: FixupKind::Bl,
        });
    }

    fn emit_bl_external(&mut self, symbol: &str) {
        let pos = self.buf.len();
        self.emit_u32(0x9400_0000);
        self.relocs.push(Relocation {
            offset: pos as u64,
            kind: RelocKind::CallRel32,
            symbol: symbol.to_string(),
            addend: 0,
        });
    }

    fn emit_u32(&mut self, word: u32) {
        self.buf.extend_from_slice(&word.to_le_bytes());
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<(Vec<u8>, Vec<Relocation>)> {
        let fixups = self.fixups.clone();
        for fixup in fixups {
            let target = self
                .labels
                .get(&fixup.target)
                .ok_or_else(|| Error::from("unknown branch target"))?;
            let origin = fixup.pos;
            let delta = (*target as i64) - (origin as i64);
            let imm = delta / 4;
            match fixup.kind {
                FixupKind::B => {
                    let imm26 = i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("branch out of range"));
                    }
                    let encoded = 0x1400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::BCond(cond) => {
                    let imm19 = i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm19 < -(1 << 18) || imm19 > (1 << 18) - 1 {
                        return Err(Error::from("conditional branch out of range"));
                    }
                    let encoded = 0x5400_0000u32
                        | (((imm19 as u32) & 0x7FFFF) << 5)
                        | (cond & 0xF);
                    self.patch_u32(origin, encoded);
                }
                FixupKind::Bl => {
                    let imm26 = i32::try_from(imm).map_err(|_| Error::from("branch out of range"))?;
                    if imm26 < -(1 << 25) || imm26 > (1 << 25) - 1 {
                        return Err(Error::from("call target out of range"));
                    }
                    let encoded = 0x9400_0000u32 | ((imm26 as u32) & 0x03FF_FFFF);
                    self.patch_u32(origin, encoded);
                }
            }
        }
        Ok((self.buf, self.relocs))
    }

    fn patch_u32(&mut self, pos: usize, word: u32) {
        self.buf[pos..pos + 4].copy_from_slice(&word.to_le_bytes());
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Label {
    Function(u32),
    Block(u32, BasicBlockId),
}

enum CallTarget {
    Internal(u32),
    External(String),
}

fn build_function_map(program: &LirProgram) -> Result<HashMap<String, u32>> {
    let mut map = HashMap::new();
    for (idx, func) in program.functions.iter().enumerate() {
        let name = String::from(func.name.clone());
        map.insert(name, idx as u32);
    }
    Ok(map)
}
impl Reg {
    fn is_sp(self) -> bool {
        matches!(self, Reg::X31)
    }
}
