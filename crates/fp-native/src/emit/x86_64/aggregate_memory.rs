use super::*;

#[derive(Clone, Copy)]
pub(super) enum CopyLocation {
    Frame(i32),
    Address(Reg),
}

pub(super) fn copy_sp_to_sp(asm: &mut Assembler, src: i32, dst: i32, size: i32) -> Result<()> {
    emit_memory_copy(
        asm,
        CopyLocation::Frame(src),
        CopyLocation::Frame(dst),
        size,
    )
}

pub(super) fn copy_sp_to_reg(asm: &mut Assembler, src: i32, dst: Reg, size: i32) -> Result<()> {
    emit_memory_copy(
        asm,
        CopyLocation::Frame(src),
        CopyLocation::Address(dst),
        size,
    )
}

pub(super) fn copy_reg_to_sp(asm: &mut Assembler, src: Reg, dst: i32, size: i32) -> Result<()> {
    emit_memory_copy(
        asm,
        CopyLocation::Address(src),
        CopyLocation::Frame(dst),
        size,
    )
}

pub(super) fn copy_reg_to_reg(asm: &mut Assembler, src: Reg, dst: Reg, size: i32) -> Result<()> {
    emit_memory_copy(
        asm,
        CopyLocation::Address(src),
        CopyLocation::Address(dst),
        size,
    )
}

pub(super) fn emit_memory_copy(
    asm: &mut Assembler,
    source: CopyLocation,
    destination: CopyLocation,
    size: i32,
) -> Result<()> {
    if size < 0 {
        return Err(Error::from("aggregate copy size must not be negative"));
    }
    if size == 0 {
        return Ok(());
    }

    let (source_base, destination_base, value) = copy_temporaries(source, destination)?;
    materialize_copy_address(asm, source_base, source);
    materialize_copy_address(asm, destination_base, destination);

    let mut offset = 0;
    while offset + 8 <= size {
        emit_mov_rm64(asm, value, source_base, offset);
        emit_mov_mr64(asm, destination_base, offset, value);
        offset += 8;
    }
    let remaining = size - offset;
    if remaining >= 4 {
        emit_movsxd_rm32(asm, value, source_base, offset);
        emit_mov_mr32(asm, destination_base, offset, value);
        offset += 4;
    }
    if size - offset >= 2 {
        emit_movsx_rm16(asm, value, source_base, offset);
        emit_mov_mr16(asm, destination_base, offset, value);
        offset += 2;
    }
    if size - offset >= 1 {
        emit_movsx_rm8(asm, value, source_base, offset);
        emit_mov_mr8(asm, destination_base, offset, value);
    }
    Ok(())
}

pub(super) fn copy_temporaries(
    source: CopyLocation,
    destination: CopyLocation,
) -> Result<(Reg, Reg, Reg)> {
    const CANDIDATES: [Reg; 9] = [
        Reg::Rax,
        Reg::Rcx,
        Reg::Rdx,
        Reg::Rdi,
        Reg::Rsi,
        Reg::R8,
        Reg::R9,
        Reg::R10,
        Reg::R11,
    ];
    let forbidden = [source, destination];
    let mut selected = CANDIDATES.into_iter().filter(|candidate| {
        !forbidden.iter().any(
            |location| matches!(location, CopyLocation::Address(address) if address == candidate),
        )
    });
    let source_base = selected
        .next()
        .ok_or_else(|| Error::from("no temporary register available for aggregate copy source"))?;
    let destination_base = selected.next().ok_or_else(|| {
        Error::from("no temporary register available for aggregate copy destination")
    })?;
    let value = selected
        .next()
        .ok_or_else(|| Error::from("no temporary register available for aggregate copy value"))?;
    Ok((source_base, destination_base, value))
}

pub(super) fn materialize_copy_address(
    asm: &mut Assembler,
    destination: Reg,
    location: CopyLocation,
) {
    match location {
        CopyLocation::Frame(offset) => {
            emit_mov_rr(asm, destination, Reg::Rbp);
            emit_add_ri32(asm, destination, offset);
        }
        CopyLocation::Address(source) => emit_mov_rr(asm, destination, source),
    }
}

pub(super) fn store_aggregate_from_reg(
    asm: &mut Assembler,
    layout: &FrameLayout,
    source: Reg,
    address: &AsmValue,
    size: i32,
) -> Result<()> {
    match address {
        AsmValue::StackSlot(id) => {
            let dst_offset = stack_slot_offset(layout, *id)?;
            copy_reg_to_sp(asm, source, dst_offset, size)
        }
        AsmValue::Register(id) => {
            let addr_offset = vreg_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            copy_reg_to_reg(asm, source, Reg::R11, size)
        }
        AsmValue::Local(id) => {
            let addr_offset = local_offset(layout, *id)?;
            emit_mov_rm64(asm, Reg::R11, Reg::Rbp, addr_offset);
            copy_reg_to_reg(asm, source, Reg::R11, size)
        }
        _ => Err(Error::from(
            "unsupported aggregate store address for x86_64",
        )),
    }
}

pub(super) fn zero_sp_range(asm: &mut Assembler, dst: i32, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset + 8 <= size {
        emit_mov_mr64(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_mr32(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_mr16(asm, Reg::Rbp, dst + offset, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_mr8(asm, Reg::Rbp, dst + offset, Reg::R10);
    }
    Ok(())
}

pub(super) fn zero_reg_range(asm: &mut Assembler, dst: Reg, size: i32) -> Result<()> {
    if size <= 0 {
        return Ok(());
    }
    let mut offset = 0;
    emit_mov_imm64(asm, Reg::R10, 0);
    while offset + 8 <= size {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr64(asm, Reg::R11, 0, Reg::R10);
        offset += 8;
    }
    let mut remaining = size - offset;
    if remaining >= 4 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr32(asm, Reg::R11, 0, Reg::R10);
        offset += 4;
        remaining -= 4;
    }
    if remaining >= 2 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr16(asm, Reg::R11, 0, Reg::R10);
        offset += 2;
        remaining -= 2;
    }
    if remaining >= 1 {
        emit_mov_rr(asm, Reg::R11, dst);
        emit_add_ri32(asm, Reg::R11, offset);
        emit_mov_mr8(asm, Reg::R11, 0, Reg::R10);
    }
    Ok(())
}
