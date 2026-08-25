use super::*;

pub(super) fn emit_ret(asm: &mut Assembler) {
    asm.push(0xC3);
}

pub(super) fn emit_trap(asm: &mut Assembler) {
    asm.extend(&[0x0F, 0x0B]);
}

pub(super) fn emit_exit_syscall(asm: &mut Assembler, code: u32) -> Result<()> {
    if code > i32::MAX as u32 {
        return Err(Error::from("exit code exceeds i32 range"));
    }
    emit_mov_imm64(asm, Reg::Rdi, code as u64);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

pub(super) fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_rr(asm, Reg::Rdi, reg);
    emit_mov_imm64(asm, Reg::Rax, 60);
    asm.extend(&[0x0F, 0x05]);
    Ok(())
}

pub(super) fn emit_mov_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x89);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xB8 + (dst.id() & 0x7));
    asm.extend(&imm.to_le_bytes());
}

pub(super) fn emit_mov_symbol_addr(
    asm: &mut Assembler,
    dst: Reg,
    symbol: &str,
    addend: i64,
) -> Result<()> {
    asm.emit_mov_imm64_reloc(dst, symbol, addend);
    Ok(())
}

pub(super) fn emit_add_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x01);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_adc_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x11);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_sub_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x29);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_sbb_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x19);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_imul_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.extend(&[0x0F, 0xAF]);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_and_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x21);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_or_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x09);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_xor_rr(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x31);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_not_r64(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xF7);
    emit_modrm(asm, 0b11, 2, dst.id());
}

pub(super) fn emit_add_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 0, dst.id());
    asm.extend(&imm.to_le_bytes());
}

pub(super) fn emit_sub_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.extend(&imm.to_le_bytes());
}

pub(super) fn emit_and_ri32(asm: &mut Assembler, dst: Reg, imm: i32) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 4, dst.id());
    asm.extend(&imm.to_le_bytes());
}

pub(super) fn emit_shl_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 4, dst.id());
    asm.push(imm);
}

pub(super) fn emit_pextrq_r64_xmm_imm8(asm: &mut Assembler, dst: Reg, src: FReg, imm: u8) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x3A);
    asm.push(0x16);
    emit_modrm(asm, 0b11, dst.id(), src.id());
    asm.push(imm);
}

pub(super) fn emit_shr_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 5, dst.id());
    asm.push(imm);
}

pub(super) fn emit_sar_imm8(asm: &mut Assembler, dst: Reg, imm: u8) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xC1);
    emit_modrm(asm, 0b11, 7, dst.id());
    asm.push(imm);
}

pub(super) fn emit_shl_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 4, dst.id());
}

pub(super) fn emit_shr_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 5, dst.id());
}

#[allow(dead_code)]
pub(super) fn emit_sar_cl(asm: &mut Assembler, dst: Reg) {
    emit_rex(asm, true, 0, dst.id());
    asm.push(0xD3);
    emit_modrm(asm, 0b11, 7, dst.id());
}

pub(super) fn emit_rex(asm: &mut Assembler, w: bool, reg: u8, rm: u8) {
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

pub(super) fn emit_modrm(asm: &mut Assembler, mode: u8, reg: u8, rm: u8) {
    let byte = ((mode & 0x3) << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
    asm.push(byte);
}

pub(super) fn emit_cmp_rr(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    emit_rex(asm, true, rhs.id(), lhs.id());
    asm.push(0x39);
    emit_modrm(asm, 0b11, rhs.id(), lhs.id());
}

pub(super) fn emit_cmp_imm32(asm: &mut Assembler, lhs: Reg, imm: i32) {
    emit_rex(asm, true, 0, lhs.id());
    asm.push(0x81);
    emit_modrm(asm, 0b11, 7, lhs.id());
    asm.extend(&imm.to_le_bytes());
}

pub(super) fn emit_setcc(asm: &mut Assembler, cc: u8, dst: Reg) {
    emit_rex(asm, false, 0, dst.id());
    asm.push(0x0F);
    asm.push(0x90 + cc);
    emit_modrm(asm, 0b11, 0, dst.id());
}

pub(super) fn emit_cmovcc(asm: &mut Assembler, cc: u8, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x40 + cc);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_movzx_r64_rm8(asm: &mut Assembler, dst: Reg, src: Reg) {
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}
