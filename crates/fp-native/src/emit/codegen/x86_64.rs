use fp_core::error::{Error, Result};
use fp_core::lir::{
    BasicBlockId, LirBasicBlock, LirConstant, LirFunction, LirInstructionKind, LirProgram,
    LirTerminator, LirValue,
};
use std::collections::{BTreeSet, HashMap};

use crate::emit::TargetFormat;

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
    frame_size: i32,
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
    let outgoing_size = (max_call_args.saturating_sub(6) as i32) * 8;
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
        frame_size,
    })
}

fn align16(value: i32) -> i32 {
    ((value + 15) / 16) * 16
}

pub fn emit_text(program: &LirProgram, format: TargetFormat) -> Result<Vec<u8>> {
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

    asm.finish()
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
    load_value(asm, layout, lhs, Reg::R10)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11)?;
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
        _ => Err(Error::from("unsupported LIR value for x86_64")),
    }
}

fn store_vreg(asm: &mut Assembler, layout: &FrameLayout, id: u32, src: Reg) -> Result<()> {
    let offset = vreg_offset(layout, id)?;
    emit_mov_mr64(asm, Reg::Rbp, offset, src);
    Ok(())
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
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) && asm.is_entry() {
                emit_exit_syscall(asm, 0)?;
            } else {
                emit_mov_imm64(asm, Reg::Rax, 0);
                emit_ret(asm);
            }
        }
        LirTerminator::Return(Some(value)) => {
            let ret_reg = Reg::Rax;
            load_value(asm, layout, value, ret_reg)?;
            if asm.needs_frame {
                emit_epilogue(asm);
            }
            if matches!(format, TargetFormat::Elf | TargetFormat::Coff) && asm.is_entry() {
                emit_exit_syscall_reg(asm, ret_reg)?;
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
) -> Result<()> {
    match (lhs, rhs) {
        (LirValue::Register(_), LirValue::Register(_))
        | (LirValue::Register(_), LirValue::Constant(_))
        | (LirValue::Constant(_), LirValue::Register(_))
        | (LirValue::Constant(_), LirValue::Constant(_)) => {}
        _ => return Err(Error::from("unsupported compare operands")),
    }

    load_value(asm, layout, lhs, Reg::R10)?;
    match rhs {
        LirValue::Register(_) => {
            load_value(asm, layout, rhs, Reg::R11)?;
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

    fn push(&mut self, byte: u8) {
        self.buf.push(byte);
    }

    fn extend(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    fn finish(mut self) -> Result<Vec<u8>> {
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
        Ok(self.buf)
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
) -> Result<()> {
    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R10, Reg::Rbp, offset);
            store_vreg(asm, layout, dst_id, Reg::R10)?;
            Ok(())
        }
        _ => Err(Error::from("unsupported load address for x86_64")),
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
    load_value(asm, layout, lhs, Reg::Rax)?;
    load_value(asm, layout, rhs, Reg::R11)?;

    emit_cqo(asm);
    emit_idiv_reg(asm, Reg::R11);

    let src = if want_rem { Reg::Rdx } else { Reg::Rax };
    store_vreg(asm, layout, dst_id, src)?;

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
        _ => return Err(Error::from("unsupported callee for x86_64")),
    };

    let arg_regs = [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

    for (idx, arg) in args.iter().enumerate() {
        if idx < arg_regs.len() {
            let reg = arg_regs[idx];
            load_value(asm, layout, arg, reg)?;
        } else {
            let offset = (idx - arg_regs.len()) as i32 * 8;
            store_outgoing_arg(asm, layout, offset, arg)?;
        }
    }

    asm.emit_call(Label::Function(target));
    store_vreg(asm, layout, dst_id, Reg::Rax)?;

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
    load_value(asm, layout, value, Reg::R10)?;
    emit_mov_mr64_sp(asm, offset, Reg::R10);
    Ok(())
}

fn emit_store(
    asm: &mut Assembler,
    layout: &FrameLayout,
    value: &LirValue,
    address: &LirValue,
) -> Result<()> {
    load_value(asm, layout, value, Reg::R10)?;

    match address {
        LirValue::StackSlot(id) => {
            let offset = stack_slot_offset(layout, *id)?;
            emit_mov_mr64(asm, Reg::Rbp, offset, Reg::R10);
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
