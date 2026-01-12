use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirProgram,
    LirTerminator, LirValue,
};
use std::collections::{BTreeSet, HashMap};

use crate::emit::{CodegenOutput, TargetFormat};

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

    for (index, func) in program.functions.iter().enumerate() {
        asm.bind(Label::Function(index as u32));
        let layout = build_frame_layout(func)?;
        asm.needs_frame = layout.frame_size > 0;
        if layout.frame_size > 0 {
            emit_prologue(&mut asm, &layout)?;
        }
        for block in &func.basic_blocks {
            asm.bind(Label::Block(index as u32, block.id));
            emit_block(&mut asm, block, format, &func_map, &layout)?;
        }
    }

    let text = asm.finish()?;
    Ok(CodegenOutput {
        text,
        rodata: Vec::new(),
        relocs: Vec::new(),
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
) -> Result<()> {
    load_value(asm, layout, lhs, Reg::X16)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17)?;
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
        _ => Err(Error::from("unsupported LIR value for aarch64")),
    }
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_store_to_sp(asm, src, offset);
    Ok(())
}

fn emit_load(
    asm: &mut Assembler,
    layout: &FrameLayout,
    dst_id: u32,
    address: &LirValue,
) -> Result<()> {
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_load_from_sp(asm, Reg::X16, offset);
            store_vreg(asm, layout, dst_id, Reg::X16)?;
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
) -> Result<()> {
    load_value(asm, layout, value, Reg::X16)?;
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_store_to_sp(asm, Reg::X16, offset);
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
) -> Result<()> {
    load_value(asm, layout, lhs, Reg::X16)?;
    load_value(asm, layout, rhs, Reg::X17)?;

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
) -> Result<()> {
    let target = match function {
        LirValue::Function(name) => func_map
            .get(name)
            .copied()
            .ok_or_else(|| Error::from("unknown callee"))?,
        _ => return Err(Error::from("unsupported callee for aarch64")),
    };

    let arg_regs = [
        Reg::X0, Reg::X1, Reg::X2, Reg::X3, Reg::X4, Reg::X5, Reg::X6, Reg::X7,
    ];
    for (idx, arg) in args.iter().enumerate() {
        if idx < arg_regs.len() {
            let reg = arg_regs[idx];
            load_value(asm, layout, arg, reg)?;
        } else {
            let offset = (idx - arg_regs.len()) as i32 * 8;
            store_outgoing_arg(asm, layout, offset, arg)?;
        }
    }

    asm.emit_bl(Label::Function(target));
    store_vreg(asm, layout, dst_id, Reg::X0)?;

    Ok(())
}

fn store_outgoing_arg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    offset: i32,
    value: &LirValue,
) -> Result<()> {
    if offset < 0 || offset + 8 > layout.outgoing_size {
        return Err(Error::from("outgoing arg offset out of range"));
    }
    load_value(asm, layout, value, Reg::X16)?;
    emit_store_to_sp(asm, Reg::X16, offset);
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

fn emit_block(
    asm: &mut Assembler,
    block: &LirBasicBlock,
    format: TargetFormat,
    func_map: &HashMap<String, u32>,
    layout: &FrameLayout,
) -> Result<()> {
    for inst in &block.instructions {
        match &inst.kind {
            LirInstructionKind::Add(lhs, rhs) => {
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Add)?
            }
            LirInstructionKind::Sub(lhs, rhs) => {
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Sub)?
            }
            LirInstructionKind::Mul(lhs, rhs) => {
                emit_binop(asm, layout, inst.id, lhs, rhs, BinOp::Mul)?
            }
            LirInstructionKind::Eq(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Eq)?
            }
            LirInstructionKind::Ne(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Ne)?
            }
            LirInstructionKind::Lt(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Lt)?
            }
            LirInstructionKind::Le(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Le)?
            }
            LirInstructionKind::Gt(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Gt)?
            }
            LirInstructionKind::Ge(lhs, rhs) => {
                emit_cmp(asm, layout, inst.id, lhs, rhs, CmpKind::Ge)?
            }
            LirInstructionKind::Div(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, false)?
            }
            LirInstructionKind::Rem(lhs, rhs) => {
                emit_divrem(asm, layout, inst.id, lhs, rhs, true)?
            }
            LirInstructionKind::Alloca { .. } => {}
            LirInstructionKind::Load { address, .. } => {
                emit_load(asm, layout, inst.id, address)?;
            }
            LirInstructionKind::Store { value, address, .. } => {
                emit_store(asm, layout, value, address)?;
            }
            LirInstructionKind::Call { function, args, .. } => {
                emit_call(asm, layout, inst.id, function, args, func_map)?;
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
            let ret_reg = Reg::X0;
            load_value(asm, layout, value, ret_reg)?;
            if asm.needs_frame {
                emit_epilogue(asm, layout);
            }
            if matches!(format, TargetFormat::Elf) && asm.is_entry() {
                emit_exit_syscall_reg(asm, ret_reg)?;
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
) -> Result<()> {
    match (lhs, rhs) {
        (LirValue::Register(_), LirValue::Register(_))
        | (LirValue::Register(_), LirValue::Constant(_))
        | (LirValue::Constant(_), LirValue::Register(_))
        | (LirValue::Constant(_), LirValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::X16)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::X17)?;
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

    fn emit_u32(&mut self, word: u32) {
        self.buf.extend_from_slice(&word.to_le_bytes());
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<Vec<u8>> {
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
        Ok(self.buf)
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
