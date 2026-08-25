use super::*;

pub(super) fn emit_mov_reg(asm: &mut Assembler, dst: Reg, src: Reg) {
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

pub(super) fn emit_load_from_base(asm: &mut Assembler, dst: Reg, base: Reg, offset: i32) {
    if offset >= 0 && (offset % 8) == 0 {
        let imm12 = (offset / 8) as u32;
        if imm12 <= 0xfff {
            let instr = 0xF940_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return;
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load_from_reg(asm, dst, Reg::X17);
}

pub(super) fn emit_load8u_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x3940_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load8u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_load8s_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x39C0_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load8s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_load16u_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x7940_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load16u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_load16s_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if (offset % 2) != 0 {
        return Err(Error::from("unaligned 16-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 2) as u32;
        if imm12 <= 0xfff {
            let instr = 0x79C0_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load16s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_load32u_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB940_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load32u_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_load32s_from_base(
    asm: &mut Assembler,
    dst: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if (offset % 4) != 0 {
        return Err(Error::from("unaligned 32-bit load on aarch64"));
    }
    if offset >= 0 {
        let imm12 = (offset / 4) as u32;
        if imm12 <= 0xfff {
            let instr = 0xB980_0000u32 | (imm12 << 10) | (base.id() << 5) | dst.id();
            asm.extend(&instr.to_le_bytes());
            return Ok(());
        }
    }
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_load32s_from_reg(asm, dst, Reg::X17);
    Ok(())
}

pub(super) fn emit_store_to_base(asm: &mut Assembler, src: Reg, base: Reg, offset: i32) {
    if base == Reg::X31 {
        asm.log_stack_write(offset, 8, "str");
    }
    if offset >= 0 && (offset % 8) == 0 {
        let imm12 = (offset / 8) as u32;
        if imm12 <= 0xfff {
            let instr = 0xF900_0000u32 | (imm12 << 10) | (base.id() << 5) | src.id();
            asm.extend(&instr.to_le_bytes());
            return;
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
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store_to_reg(asm, src, Reg::X17);
}

pub(super) fn emit_store8_to_base(
    asm: &mut Assembler,
    src: Reg,
    base: Reg,
    offset: i32,
) -> Result<()> {
    if base == Reg::X31 {
        asm.log_stack_write(offset, 1, "strb");
    }
    if offset >= 0 {
        let imm12 = offset as u32;
        if imm12 <= 0xfff {
            let instr = 0x3900_0000u32 | (imm12 << 10) | (base.id() << 5) | src.id();
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
    emit_mov_reg(asm, Reg::X17, base);
    add_immediate_offset(asm, Reg::X17, offset as i64);
    emit_store8_to_reg(asm, src, Reg::X17);
    Ok(())
}

pub(super) fn emit_dup_from_gpr(
    asm: &mut Assembler,
    dst: FReg,
    src: Reg,
    lane_bits: u16,
    lanes: u16,
) -> Result<()> {
    let expected_lanes = match lane_bits {
        8 => 16,
        16 => 8,
        32 => 4,
        64 => 2,
        _ => return Err(Error::from("unsupported lane size for aarch64 dup")),
    };
    if lanes != expected_lanes {
        return Err(Error::from("unsupported lane count for aarch64 dup"));
    }

    let base = match lane_bits {
        8 => 0x4E01_0C00u32,
        16 => 0x4E02_0C00u32,
        32 => 0x4E04_0C00u32,
        64 => 0x4E08_0C00u32,
        _ => unreachable!(),
    };
    let instr = base | (src.id() << 5) | dst.id();
    asm.emit_u32(instr);
    Ok(())
}
