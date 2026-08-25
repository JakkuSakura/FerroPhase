use super::*;

pub(super) fn emit_mov_rm64(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x8B);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movzx_rm8(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xB6);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movsx_rm8(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xBE);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movsx_rm16(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0xBF);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movsxd_rm32(asm: &mut Assembler, dst: Reg, base: Reg, disp: i32) {
    emit_rex(asm, true, dst.id(), base.id());
    asm.push(0x63);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_mov_mr64(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_mov_mr32(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_mov_mr16(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    asm.push(0x66);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x89);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_mov_mr8(asm: &mut Assembler, base: Reg, disp: i32, src: Reg) {
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x88);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_mov_mr64_sp(asm: &mut Assembler, disp: i32, src: Reg) {
    emit_rex(asm, true, src.id(), Reg::Rsp.id());
    asm.push(0x89);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

pub(super) fn emit_movsd_m64x_sp(asm: &mut Assembler, disp: i32, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), Reg::Rsp.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm(asm, 0b10, src.id(), 0b100);
    emit_sib(asm, 0b00, 0b100, 0b100);
    asm.extend(&disp.to_le_bytes());
}

pub(super) fn emit_movsd_xm64(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x10);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movsd_m64x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x0F);
    asm.push(0x11);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_movq_xmm_r64(asm: &mut Assembler, dst: FReg, src: Reg) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x6E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_movq_r64_xmm(asm: &mut Assembler, dst: Reg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, true, src.id(), dst.id());
    asm.push(0x0F);
    asm.push(0x7E);
    emit_modrm(asm, 0b11, src.id(), dst.id());
}

pub(super) fn emit_movdqu_xm128(asm: &mut Assembler, dst: FReg, base: Reg, disp: i32) {
    asm.push(0xF3);
    emit_rex(asm, false, dst.id(), base.id());
    asm.push(0x0F);
    asm.push(0x6F);
    emit_modrm_disp32(asm, dst.id(), base.id(), disp);
}

pub(super) fn emit_movdqu_m128x(asm: &mut Assembler, base: Reg, disp: i32, src: FReg) {
    asm.push(0xF3);
    emit_rex(asm, false, src.id(), base.id());
    asm.push(0x0F);
    asm.push(0x7F);
    emit_modrm_disp32(asm, src.id(), base.id(), disp);
}

pub(super) fn emit_punpcklqdq_xmm_xmm(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x6C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_punpcklwd_xmm_xmm(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0x66);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x61);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_pinsrq_xmm_r64_imm8(asm: &mut Assembler, dst: FReg, src: Reg, imm: u8) {
    asm.push(0x66);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x3A);
    asm.push(0x22);
    emit_modrm(asm, 0b11, dst.id(), src.id());
    asm.push(imm);
}

pub(super) fn emit_addsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x58);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_subsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_mulsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x59);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_divsd(asm: &mut Assembler, dst: FReg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5E);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_ucomisd(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &AsmType) {
    if matches!(ty, AsmType::F64) {
        asm.push(0x66);
    }
    emit_rex(asm, false, lhs.id(), rhs.id());
    asm.push(0x0F);
    asm.push(0x2E);
    emit_modrm(asm, 0b11, lhs.id(), rhs.id());
}

pub(super) fn emit_cvtsi2sd(asm: &mut Assembler, dst: FReg, src: Reg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_cvttsd2si(asm: &mut Assembler, dst: Reg, src: FReg, ty: &AsmType) {
    emit_float_prefix(asm, ty);
    emit_rex(asm, true, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x2C);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_cvtsd2ss(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF2);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_cvtss2sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    asm.push(0xF3);
    emit_rex(asm, false, dst.id(), src.id());
    asm.push(0x0F);
    asm.push(0x5A);
    emit_modrm(asm, 0b11, dst.id(), src.id());
}

pub(super) fn emit_mov_al_imm8(asm: &mut Assembler, imm: u8) {
    asm.push(0xB0);
    asm.push(imm);
}
