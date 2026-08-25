use super::*;

pub(super) fn store_constant_aggregate_to_reg(
    asm: &mut Assembler,
    data_layout: &LirDataLayout,
    base: Reg,
    constant: &AsmConstant,
    agg_ty: &AsmType,
    rodata: &mut Vec<u8>,
    rodata_pool: &mut HashMap<String, u64>,
) -> Result<()> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let _align_of = |ty: &LirType| data_layout.align_of(ty).expect("layout query failed");
    let struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    let size = size_of(agg_ty) as i32;
    if size == 0 {
        return Ok(());
    }
    match constant {
        AsmConstant::Undef(_) | AsmConstant::Null(_) => return zero_reg_range(asm, base, size),
        AsmConstant::Int(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::UInt(value, _) if *value == 0 => return zero_reg_range(asm, base, size),
        AsmConstant::GlobalRef(name, _, indices) => {
            let addend = indices.iter().map(|index| *index as i64).sum();
            emit_mov_symbol_addr(asm, Reg::R10, name.as_str(), addend)?;
            return copy_reg_to_reg(asm, Reg::R10, base, size);
        }
        AsmConstant::Struct(values, _) => {
            let AsmType::Struct { fields, .. } = agg_ty else {
                return Err(Error::from("expected struct type for aggregate return"));
            };
            let layout = struct_layout(agg_ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate return"))?;
            for (idx, field) in values.iter().enumerate() {
                let field_offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_ty = fields
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_size = size_of(field_ty);
                if is_aggregate_storage(field_ty, data_layout) {
                    emit_mov_rr(asm, Reg::R9, base);
                    emit_add_ri32(asm, Reg::R9, field_offset as i32);
                    store_constant_aggregate_to_reg(
                        asm,
                        data_layout,
                        Reg::R9,
                        field,
                        field_ty,
                        rodata,
                        rodata_pool,
                    )?;
                    continue;
                }
                match field {
                    AsmConstant::GlobalRef(name, _, indices) => {
                        let addend = indices.iter().map(|index| *index as i64).sum();
                        asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), addend);
                    }
                    AsmConstant::FunctionRef(name, _) => {
                        asm.emit_mov_imm64_reloc(Reg::R10, name.as_str(), 0);
                    }
                    AsmConstant::String(text) => {
                        let offset = intern_cstring(rodata, rodata_pool, text);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", offset as i64);
                    }
                    AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                        emit_mov_imm64(asm, Reg::R10, 0);
                    }
                    other => {
                        let bits = constant_to_u64_bits(other)?;
                        emit_mov_imm64(asm, Reg::R10, bits);
                    }
                }
                let dst = field_offset as i32;
                match field_size {
                    1 => emit_mov_mr8(asm, base, dst, Reg::R10),
                    2 => emit_mov_mr16(asm, base, dst, Reg::R10),
                    4 => emit_mov_mr32(asm, base, dst, Reg::R10),
                    8 => emit_mov_mr64(asm, base, dst, Reg::R10),
                    _ => {
                        return Err(Error::from("unsupported aggregate field size in return"));
                    }
                }
            }
            Ok(())
        }
        AsmConstant::Array(values, elem_ty) => {
            let elem_ty = match agg_ty {
                AsmType::Array(elem, _) => elem.as_ref(),
                _ => elem_ty,
            };
            let elem_size = size_of(elem_ty) as i32;
            if elem_size == 0 {
                return Ok(());
            }
            for (idx, elem) in values.iter().enumerate() {
                let offset = (idx as i32) * elem_size;
                if is_aggregate_storage(elem_ty, data_layout) {
                    emit_mov_rr(asm, Reg::R9, base);
                    emit_add_ri32(asm, Reg::R9, offset);
                    store_constant_aggregate_to_reg(
                        asm,
                        data_layout,
                        Reg::R9,
                        elem,
                        elem_ty,
                        rodata,
                        rodata_pool,
                    )?;
                    continue;
                }
                match elem {
                    AsmConstant::String(text) => {
                        let ro_offset = intern_cstring(rodata, rodata_pool, text);
                        asm.emit_mov_imm64_reloc(Reg::R10, ".rodata", ro_offset as i64);
                    }
                    AsmConstant::Null(_) | AsmConstant::Undef(_) => {
                        emit_mov_imm64(asm, Reg::R10, 0);
                    }
                    other => {
                        let bits = constant_to_u64_bits(other)?;
                        emit_mov_imm64(asm, Reg::R10, bits);
                    }
                }
                match elem_size {
                    1 => emit_mov_mr8(asm, base, offset, Reg::R10),
                    2 => emit_mov_mr16(asm, base, offset, Reg::R10),
                    4 => emit_mov_mr32(asm, base, offset, Reg::R10),
                    8 => emit_mov_mr64(asm, base, offset, Reg::R10),
                    _ => {
                        return Err(Error::from("unsupported array element size in return"));
                    }
                }
            }
            Ok(())
        }
        _ => Err(Error::from(format!(
            "unsupported aggregate constant for return: constant={:?} ty={:?}",
            constant, agg_ty
        ))),
    }
}
