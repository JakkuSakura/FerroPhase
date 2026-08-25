use super::*;

pub(super) fn emit_const_globals(
    program: &AsmProgram,
    data_layout: &LirDataLayout,
    rodata: &mut Vec<u8>,
    rodata_symbols: &mut HashMap<String, u64>,
    data: &mut Vec<u8>,
    data_symbols: &mut HashMap<String, u64>,
    relocs_out: &mut Vec<crate::emit::Relocation>,
) -> Result<()> {
    let align_of = |ty: &LirType| data_layout.align_of(ty).expect("layout query failed");
    fn global_section_kind(
        program: &AsmProgram,
        global: &fp_core::asmir::AsmGlobal,
    ) -> fp_core::asmir::AsmSectionKind {
        let declared = global
            .section
            .as_deref()
            .and_then(|name| {
                program
                    .sections
                    .iter()
                    .find(|section| section.name == name)
                    .map(|section| section.kind.clone())
            })
            .unwrap_or_else(|| {
                if global.is_constant {
                    fp_core::asmir::AsmSectionKind::ReadOnlyData
                } else {
                    fp_core::asmir::AsmSectionKind::Data
                }
            });

        if global.relocations.is_empty() {
            declared
        } else {
            match declared {
                fp_core::asmir::AsmSectionKind::Data | fp_core::asmir::AsmSectionKind::Bss => {
                    declared
                }
                _ => fp_core::asmir::AsmSectionKind::Data,
            }
        }
    }

    let mut emit_global = |global: &fp_core::asmir::AsmGlobal,
                           initializer: Option<&AsmConstant>,
                           bytes_out: &mut Vec<u8>,
                           symbols_out: &mut HashMap<String, u64>,
                           reloc_section: crate::emit::RelocSection|
     -> Result<()> {
        let align = global
            .alignment
            .map(|value| value as i32)
            .unwrap_or_else(|| align_of(&global.ty) as i32);
        let offset = align_to(bytes_out.len() as i32, align) as usize;
        if offset > bytes_out.len() {
            bytes_out.resize(offset, 0);
        }
        let Some(initializer) = initializer else {
            return Ok(());
        };
        let bytes = encode_const_bytes(initializer, &global.ty, data_layout)?;
        bytes_out.extend_from_slice(&bytes);
        symbols_out.insert(global.name.to_string(), offset as u64);

        for reloc in &global.relocations {
            let kind = match reloc.kind {
                fp_core::asmir::AsmRelocationKind::Abs64 => crate::emit::RelocKind::Abs64,
                fp_core::asmir::AsmRelocationKind::PcRel32 => crate::emit::RelocKind::CallRel32,
            };
            relocs_out.push(crate::emit::Relocation {
                offset: offset as u64 + reloc.offset,
                kind,
                section: reloc_section,
                symbol: reloc.symbol.to_string(),
                addend: reloc.addend,
            });
        }
        Ok(())
    };

    for global in &program.globals {
        if global.initializer.is_none() {
            continue;
        }

        let section_kind = global_section_kind(program, global);

        match section_kind {
            fp_core::asmir::AsmSectionKind::Data | fp_core::asmir::AsmSectionKind::Bss => {
                emit_global(
                    global,
                    global.initializer.as_ref(),
                    data,
                    data_symbols,
                    crate::emit::RelocSection::Data,
                )?;
            }
            _ => {
                emit_global(
                    global,
                    global.initializer.as_ref(),
                    rodata,
                    rodata_symbols,
                    crate::emit::RelocSection::Rdata,
                )?;
            }
        }
    }
    Ok(())
}

pub(super) fn encode_const_bytes(
    constant: &AsmConstant,
    ty: &AsmType,
    data_layout: &LirDataLayout,
) -> Result<Vec<u8>> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let _struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    match (constant, ty) {
        (AsmConstant::UInt(value, _), AsmType::I8) => Ok(vec![*value as u8]),
        (AsmConstant::Int(value, _), AsmType::I8) => Ok(vec![*value as u8]),
        (AsmConstant::UInt(value, _), AsmType::I16) => Ok((*value as u16).to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I16) => Ok((*value as i16).to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::I32) => Ok((*value as u32).to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I32) => Ok((*value as i32).to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::I64) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::I64) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::UInt(value, _), AsmType::Ptr(_)) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Int(value, _), AsmType::Ptr(_)) => Ok(value.to_le_bytes().to_vec()),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I8) => Ok(vec![0u8]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I16) => Ok(vec![0u8; 2]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I32) => Ok(vec![0u8; 4]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::I64) => Ok(vec![0u8; 8]),
        (AsmConstant::Null(_) | AsmConstant::Undef(_), AsmType::Ptr(_)) => Ok(vec![0u8; 8]),
        (AsmConstant::Bytes(bytes), AsmType::Array(elem, _)) if **elem == AsmType::I8 => {
            Ok(bytes.clone())
        }
        (AsmConstant::Bytes(bytes), _) => Ok(bytes.clone()),
        (AsmConstant::Array(values, _), AsmType::Array(_, len))
            if values.is_empty() || *len == 0 =>
        {
            Ok(Vec::new())
        }
        (AsmConstant::Array(values, elem_ty), AsmType::Array(elem, _))
            if **elem == AsmType::I8 && *elem_ty == AsmType::I8 =>
        {
            let mut out = Vec::with_capacity(values.len());
            for value in values {
                out.push(const_to_u8(value)?);
            }
            Ok(out)
        }
        (AsmConstant::GlobalRef(_, ptr_ty, _), _) => Ok(vec![0u8; size_of(ptr_ty) as usize]),
        (AsmConstant::FunctionRef(_, ptr_ty), _) => Ok(vec![0u8; size_of(ptr_ty) as usize]),
        _ => Err(Error::from(format!(
            "unsupported global initializer for native rodata: {:?}",
            constant
        ))),
    }
}
