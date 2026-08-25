use super::*;

pub(super) fn emit_mov_imm16(asm: &mut Assembler, dst: Reg, imm: u16) {
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_mov_imm64(asm: &mut Assembler, dst: Reg, imm: u64) {
    emit_mov_imm16(asm, dst, (imm & 0xffff) as u16);
    emit_movk_imm16(asm, dst, ((imm >> 16) & 0xffff) as u16, 16);
    emit_movk_imm16(asm, dst, ((imm >> 32) & 0xffff) as u16, 32);
    emit_movk_imm16(asm, dst, ((imm >> 48) & 0xffff) as u16, 48);
}

pub(super) fn emit_movk_imm16(asm: &mut Assembler, dst: Reg, imm: u16, shift: u32) {
    let hw = (shift / 16) & 0x3;
    let instr = 0xF280_0000u32 | ((imm as u32) << 5) | (hw << 21) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fmov_d_from_x(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x9E67_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fmov_s_from_w(asm: &mut Assembler, dst: FReg, src: Reg) {
    let instr = 0x1E27_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fmov_x_from_d(asm: &mut Assembler, dst: Reg, src: FReg) {
    let instr = 0x9E66_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fmov_w_from_s(asm: &mut Assembler, dst: Reg, src: FReg) {
    let instr = 0x1E26_0000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_add_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8B00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_adds_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xAB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_and_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x8A00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_or_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xAA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_eor_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_lslv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_lsrv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2400u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_asrv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x1AC0_2800u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_sub_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xCB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_subs_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_adc_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9A00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_sbc_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0xDA00_0000u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_mul_reg(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9B00_7C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_add_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0x9100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_sub_imm12(asm: &mut Assembler, dst: Reg, src: Reg, imm12: u32) {
    let instr = 0xD100_0000u32 | ((imm12 & 0xfff) << 10) | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_ret(asm: &mut Assembler) {
    asm.extend(&0xD65F_03C0u32.to_le_bytes());
}

pub(super) fn emit_trap(asm: &mut Assembler) {
    asm.extend(&0xD420_0000u32.to_le_bytes());
}

pub(super) fn emit_exit_syscall(asm: &mut Assembler, code: u16) -> Result<()> {
    emit_mov_imm16(asm, Reg::X0, code);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

pub(super) fn emit_exit_syscall_reg(asm: &mut Assembler, reg: Reg) -> Result<()> {
    emit_mov_reg(asm, Reg::X0, reg);
    emit_mov_imm16(asm, Reg::X8, 93);
    emit_svc(asm);
    Ok(())
}

pub(super) fn emit_svc(asm: &mut Assembler) {
    emit_svc_imm(asm, 0);
}

pub(super) fn emit_svc_imm(asm: &mut Assembler, imm16: u16) {
    let imm = (imm16 as u32) & 0xFFFF;
    let instr = 0xD400_0001u32 | (imm << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_sub_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0xD100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_add_sp(asm: &mut Assembler, imm: u32) {
    let instr = 0x9100_03FFu32 | ((imm & 0xfff) << 10);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_adjust_sp(asm: &mut Assembler, imm: i32, add: bool) {
    if imm <= 0 {
        return;
    }
    let imm = imm as u32;
    if imm <= 0xfff {
        if add {
            emit_add_sp(asm, imm);
        } else {
            emit_sub_sp(asm, imm);
        }
        return;
    }
    emit_mov_reg(asm, Reg::X16, Reg::X31);
    emit_mov_imm64(asm, Reg::X17, imm as u64);
    if add {
        emit_add_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    } else {
        emit_sub_reg(asm, Reg::X16, Reg::X16, Reg::X17);
    }
    emit_mov_reg(asm, Reg::X31, Reg::X16);
}

pub(super) fn emit_sdiv(asm: &mut Assembler, dst: Reg, lhs: Reg, rhs: Reg) {
    let instr = 0x9AC0_0C00u32 | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_msub(asm: &mut Assembler, dst: Reg, mul_lhs: Reg, mul_rhs: Reg, add: Reg) {
    let instr =
        0x9B00_8000u32 | (mul_rhs.id() << 16) | (mul_lhs.id() << 5) | dst.id() | (add.id() << 10);
    asm.extend(&instr.to_le_bytes());
}
pub(super) fn emit_load_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) {
    emit_load_from_base(asm, dst, Reg::X31, offset);
}

pub(super) fn emit_load8u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load8u_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_load8s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load8s_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_load16u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load16u_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_load16s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load16s_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_load32u_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load32u_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_load32s_from_sp(asm: &mut Assembler, dst: Reg, offset: i32) -> Result<()> {
    emit_load32s_from_base(asm, dst, Reg::X31, offset)
}

pub(super) fn emit_store_to_sp(asm: &mut Assembler, src: Reg, offset: i32) {
    emit_store_to_base(asm, src, Reg::X31, offset);
}

pub(super) fn emit_store8_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    emit_store8_to_base(asm, src, Reg::X31, offset)
}

pub(super) fn emit_store16_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit store on aarch64"));
    }
    asm.log_stack_write(offset, 2, "strh");
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x7900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    let src = match src {
        Reg::X17 => {
            emit_mov_reg(asm, Reg::X16, Reg::X17);
            Reg::X16
        }
        Reg::X9 => {
            emit_mov_reg(asm, Reg::X16, Reg::X9);
            Reg::X16
        }
        other => other,
    };
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store16_to_reg(asm, src, Reg::X17);
    Ok(())
}

pub(super) fn emit_store32_to_sp(asm: &mut Assembler, src: Reg, offset: i32) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit store on aarch64"));
    }
    asm.log_stack_write(offset, 4, "strw");
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB900_03E0u32 | (imm12 << 10) | src.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    let src = match src {
        Reg::X17 => {
            emit_mov_reg(asm, Reg::X16, Reg::X17);
            Reg::X16
        }
        Reg::X9 => {
            emit_mov_reg(asm, Reg::X16, Reg::X9);
            Reg::X16
        }
        other => other,
    };
    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store32_to_reg(asm, src, Reg::X17);
    Ok(())
}

pub(super) fn emit_load_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xF940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load8u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x3940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load8s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x39C0_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load16u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x7940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load16s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0x79C0_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load32u_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xB940_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load32s_from_reg(asm: &mut Assembler, dst: Reg, base: Reg) {
    let instr = 0xB980_0000u32 | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0xF900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store8_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0x3900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store16_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0x7900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store32_to_reg(asm: &mut Assembler, src: Reg, base: Reg) {
    let instr = 0xB900_0000u32 | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load_float_from_sp(asm: &mut Assembler, dst: FReg, offset: i32, ty: &AsmType) {
    let size_of = |ty: &LirType| asm.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| asm.data_layout.align_of(ty).expect("layout query failed");
    let _struct_layout = |ty: &LirType| {
        asm.data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let (scale, base) = match ty {
        AsmType::F32 => (4, 0xBD40_03E0u32),
        AsmType::F64 => (8, 0xFD40_03E0u32),
        AsmType::Vector(_, _) if size_of(ty) == 16 => (16, 0x3DC0_03E0u32),
        _ => unreachable!("unsupported aarch64 fp/vector stack load: {ty:?}"),
    };

    let imm12 = (offset / scale) as u32;
    if imm12 <= 0xfff {
        let instr = base | (imm12 << 10) | dst.id();
        asm.extend(&instr.to_le_bytes());
        return;
    }

    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load_float_from_reg(asm, dst, Reg::X17, ty);
}

pub(super) fn emit_store_float_to_sp(asm: &mut Assembler, src: FReg, offset: i32, ty: &AsmType) {
    let size_of = |ty: &LirType| asm.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| asm.data_layout.align_of(ty).expect("layout query failed");
    let _struct_layout = |ty: &LirType| {
        asm.data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let (scale, base) = match ty {
        AsmType::F32 => (4, 0xBD00_03E0u32),
        AsmType::F64 => (8, 0xFD00_03E0u32),
        AsmType::Vector(_, _) if size_of(ty) == 16 => (16, 0x3D80_03E0u32),
        _ => unreachable!("unsupported aarch64 fp/vector stack store: {ty:?}"),
    };

    asm.log_stack_write(offset, scale, "strf");
    let imm12 = (offset / scale) as u32;
    if imm12 <= 0xfff {
        let instr = base | (imm12 << 10) | src.id();
        asm.extend(&instr.to_le_bytes());
        return;
    }

    emit_mov_reg(asm, Reg::X17, Reg::X31);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store_float_to_reg(asm, src, Reg::X17, ty);
}

pub(super) fn emit_load_float_from_reg(asm: &mut Assembler, dst: FReg, base: Reg, ty: &AsmType) {
    let size_of = |ty: &LirType| asm.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| asm.data_layout.align_of(ty).expect("layout query failed");
    let _struct_layout = |ty: &LirType| {
        asm.data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let base_opcode = match ty {
        AsmType::F32 => 0xBD40_0000u32,
        AsmType::F64 => 0xFD40_0000u32,
        AsmType::Vector(_, _) if size_of(ty) == 16 => 0x3DC0_0000u32,
        _ => unreachable!("unsupported aarch64 fp/vector reg load: {ty:?}"),
    };
    let instr = base_opcode | (base.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store_float_to_reg(asm: &mut Assembler, src: FReg, base: Reg, ty: &AsmType) {
    let size_of = |ty: &LirType| asm.data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| asm.data_layout.align_of(ty).expect("layout query failed");
    let _struct_layout = |ty: &LirType| {
        asm.data_layout
            .struct_layout(ty)
            .expect("layout query failed")
    };
    let base_opcode = match ty {
        AsmType::F32 => 0xBD00_0000u32,
        AsmType::F64 => 0xFD00_0000u32,
        AsmType::Vector(_, _) if size_of(ty) == 16 => 0x3D80_0000u32,
        _ => unreachable!("unsupported aarch64 fp/vector reg store: {ty:?}"),
    };
    let instr = base_opcode | (base.id() << 5) | src.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (31 << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load_pair(asm: &mut Assembler, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (31 << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_store_pair_base(asm: &mut Assembler, base: Reg, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA900_0000u32 | (imm7 << 15) | (b.id() << 10) | (base.id() << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_load_pair_base(asm: &mut Assembler, base: Reg, a: Reg, b: Reg, offset: i32) {
    let imm7 = ((offset / 8) as u32) & 0x7f;
    let instr = 0xA940_0000u32 | (imm7 << 15) | (b.id() << 10) | (base.id() << 5) | a.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_cmp_reg(asm: &mut Assembler, lhs: Reg, rhs: Reg) {
    let instr = 0xEB00_001F | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_cmp_imm12(asm: &mut Assembler, lhs: Reg, imm12: u32) {
    let instr = 0xF100_001F | ((imm12 & 0xfff) << 10) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_bl_reg(asm: &mut Assembler, reg: Reg) {
    let instr = 0xD63F_0000u32 | (reg.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_br_reg(asm: &mut Assembler, reg: Reg) {
    let instr = 0xD61F_0000u32 | (reg.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_cset(asm: &mut Assembler, dst: Reg, cond: u32) {
    let inv = cond ^ 1;
    let instr = 0x9A9F_07E0u32 | ((inv & 0xF) << 12) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_csel(asm: &mut Assembler, dst: Reg, if_true: Reg, if_false: Reg, cond: u32) {
    let instr = 0x9A80_0000u32
        | (if_false.id() << 16)
        | ((cond & 0xF) << 12)
        | (if_true.id() << 5)
        | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fcsel(
    asm: &mut Assembler,
    dst: FReg,
    if_true: FReg,
    if_false: FReg,
    cond: u32,
    ty: &AsmType,
) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E20_0C00u32
    } else {
        0x1E60_0C00u32
    };
    let instr =
        base | (if_false.id() << 16) | ((cond & 0xF) << 12) | (if_true.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fadd(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &AsmType) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E20_2800u32
    } else {
        0x1E60_2800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fsub(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &AsmType) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E20_3800u32
    } else {
        0x1E60_3800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fmul(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &AsmType) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E20_0800u32
    } else {
        0x1E60_0800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fdiv(asm: &mut Assembler, dst: FReg, lhs: FReg, rhs: FReg, ty: &AsmType) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E20_1800u32
    } else {
        0x1E60_1800u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fcmp(asm: &mut Assembler, lhs: FReg, rhs: FReg, ty: &AsmType) {
    let base = if matches!(ty, AsmType::F32) {
        0x1E21_2000u32
    } else {
        0x1E60_2000u32
    };
    let instr = base | (rhs.id() << 16) | (lhs.id() << 5);
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fcvt_sd(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E62_4000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fcvt_ds(asm: &mut Assembler, dst: FReg, src: FReg) {
    let instr = 0x1E22_C000u32 | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_scvtf(asm: &mut Assembler, dst: FReg, src: Reg, ty: &AsmType, signed: bool) {
    let base = match (ty, signed) {
        (AsmType::F32, true) => 0x1E22_0000u32,
        (AsmType::F32, false) => 0x1E23_0000u32,
        (AsmType::F64, true) => 0x9E62_0000u32,
        (AsmType::F64, false) => 0x9E63_0000u32,
        _ => 0x9E62_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_fcvtzs(asm: &mut Assembler, dst: Reg, src: FReg, ty: &AsmType, signed: bool) {
    let base = match (ty, signed) {
        (AsmType::F32, true) => 0x1E38_0000u32,
        (AsmType::F32, false) => 0x1E39_0000u32,
        (AsmType::F64, true) => 0x9E78_0000u32,
        (AsmType::F64, false) => 0x9E79_0000u32,
        _ => 0x9E78_0000u32,
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.extend(&instr.to_le_bytes());
}

pub(super) fn emit_nop(asm: &mut Assembler) {
    asm.extend(&0xD503_201Fu32.to_le_bytes());
}
