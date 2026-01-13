use super::TargetArch;
use crate::emit::{EmitPlan, RelocKind};
use fp_core::error::{Error, Result};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn align_up(value: u32, align: u32) -> u32 {
    (value + (align - 1)) & !(align - 1)
}

fn put_u16(out: &mut Vec<u8>, x: u16) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_u32(out: &mut Vec<u8>, x: u32) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_u64(out: &mut Vec<u8>, x: u64) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_bytes_fixed<const N: usize>(out: &mut Vec<u8>, s: &str) {
    let mut buf = [0u8; N];
    let b = s.as_bytes();
    let n = b.len().min(N);
    buf[..n].copy_from_slice(&b[..n]);
    out.extend_from_slice(&buf);
}

fn coff_machine(arch: TargetArch) -> u16 {
    match arch {
        TargetArch::X86_64 => 0x8664,
        TargetArch::Aarch64 => 0xAA64,
    }
}

struct ExternSymbol {
    name: String,
}

struct ImportLayout {
    data: Vec<u8>,
    desc_offset: u32,
    desc_size: u32,
    ilt_offset: u32,
    iat_offset: u32,
    name_offsets: Vec<u32>,
    dll_name_offset: u32,
}

fn collect_external_symbols(plan: &EmitPlan) -> Vec<ExternSymbol> {
    let mut seen = HashMap::new();
    let mut externs = Vec::new();
    for reloc in &plan.relocs {
        if reloc.kind != RelocKind::CallRel32 {
            continue;
        }
        if seen.contains_key(&reloc.symbol) {
            continue;
        }
        seen.insert(reloc.symbol.clone(), externs.len());
        externs.push(ExternSymbol {
            name: reloc.symbol.clone(),
        });
    }
    externs
}

fn build_import_layout(externs: &[ExternSymbol]) -> ImportLayout {
    let mut data = Vec::new();
    let desc_offset = 0u32;
    let desc_size = 40u32;
    data.resize(desc_size as usize, 0);

    let ilt_offset = align_up(data.len() as u32, 8);
    let ilt_size = ((externs.len() + 1) * 8) as u32;
    data.resize((ilt_offset + ilt_size) as usize, 0);

    let iat_offset = align_up(data.len() as u32, 8);
    let iat_size = ((externs.len() + 1) * 8) as u32;
    data.resize((iat_offset + iat_size) as usize, 0);

    let mut name_offsets = Vec::with_capacity(externs.len());
    for sym in externs {
        let offset = align_up(data.len() as u32, 2);
        data.resize(offset as usize, 0);
        let name_offset = data.len() as u32;
        data.extend_from_slice(&0u16.to_le_bytes());
        data.extend_from_slice(sym.name.as_bytes());
        data.push(0);
        name_offsets.push(name_offset);
    }

    let dll_name_offset = data.len() as u32;
    data.extend_from_slice(b"msvcrt.dll\0");

    ImportLayout {
        data,
        desc_offset,
        desc_size,
        ilt_offset,
        iat_offset,
        name_offsets,
        dll_name_offset,
    }
}

fn patch_import_layout(layout: &mut ImportLayout, base_rva: u32) {
    let desc_start = layout.desc_offset as usize;
    let ilt_rva = base_rva + layout.ilt_offset;
    let iat_rva = base_rva + layout.iat_offset;
    let name_rva = base_rva + layout.dll_name_offset;

    write_u32(&mut layout.data, desc_start, ilt_rva);
    write_u32(&mut layout.data, desc_start + 12, name_rva);
    write_u32(&mut layout.data, desc_start + 16, iat_rva);

    for (idx, name_offset) in layout.name_offsets.iter().enumerate() {
        let entry_rva = base_rva + *name_offset;
        let ilt_entry = layout.ilt_offset as usize + idx * 8;
        let iat_entry = layout.iat_offset as usize + idx * 8;
        write_u64(&mut layout.data, ilt_entry, entry_rva as u64);
        write_u64(&mut layout.data, iat_entry, entry_rva as u64);
    }
}

fn write_u32(buf: &mut [u8], offset: usize, value: u32) {
    buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn write_u64(buf: &mut [u8], offset: usize, value: u64) {
    buf[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
}

pub fn emit_object_coff(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
    const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
    const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;

    let header_size = 20u32;
    let section_header_size = 40u32;
    let section_offset = header_size + section_header_size;

    let pointer_to_symbols = section_offset + plan.text.len() as u32;
    let symbol_count = 1u32;
    let string_table_size = 4u32;

    let mut out = Vec::new();
    put_u16(&mut out, coff_machine(arch));
    put_u16(&mut out, 1); // sections
    put_u32(&mut out, 0);
    put_u32(&mut out, pointer_to_symbols);
    put_u32(&mut out, symbol_count);
    put_u16(&mut out, 0); // optional header size
    put_u16(&mut out, 0); // characteristics

    put_bytes_fixed::<8>(&mut out, ".text");
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, plan.text.len() as u32);
    put_u32(&mut out, section_offset);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u32(&mut out, IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ);

    out.extend_from_slice(&plan.text);

    // Symbol table
    put_bytes_fixed::<8>(&mut out, "_start");
    put_u32(&mut out, 0);
    put_u16(&mut out, 1);
    put_u16(&mut out, 0x20);
    out.push(IMAGE_SYM_CLASS_EXTERNAL);
    out.push(0);

    put_u32(&mut out, string_table_size);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

pub fn emit_executable_pe64(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x0002;
    const IMAGE_FILE_LARGE_ADDRESS_AWARE: u16 = 0x0020;

    const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
    const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;

    const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 3;

    if plan.text.is_empty() {
        return Err(Error::from("PE executable requires non-empty text"));
    }

    let externs = collect_external_symbols(plan);
    if !externs.is_empty() && !matches!(arch, TargetArch::X86_64) {
        return Err(Error::from(
            "external calls are not supported for AArch64 PE yet",
        ));
    }

    let stub_size = if externs.is_empty() { 0 } else { 6 };
    let mut text = plan.text.clone();
    if stub_size > 0 {
        text.resize(text.len() + stub_size * externs.len(), 0);
    }

    let mut import_layout = if externs.is_empty() {
        None
    } else {
        Some(build_import_layout(&externs))
    };

    let image_base: u64 = 0x1400_0000_0;
    let section_alignment = 0x1000u32;
    let file_alignment = 0x200u32;

    let dos_header_size = 64u32;
    let pe_offset = 0x80u32;
    let coff_header_size = 20u32;
    let optional_header_size = 0xF0u32;
    let section_header_size = 40u32;

    let text_rva = section_alignment;
    let rodata_len = plan.rodata.len() as u32;
    let import_start = if import_layout.is_some() {
        align_up(rodata_len, 8)
    } else {
        rodata_len
    };
    let import_len = import_layout
        .as_ref()
        .map(|layout| layout.data.len() as u32)
        .unwrap_or(0);
    let rdata_len = import_start + import_len;
    let has_rdata = rdata_len > 0;

    let rdata_rva = if has_rdata {
        align_up(
            text_rva + align_up(text.len() as u32, section_alignment),
            section_alignment,
        )
    } else {
        0
    };

    let section_count = if has_rdata { 2u32 } else { 1u32 };
    let headers_size = align_up(
        pe_offset + 4 + coff_header_size + optional_header_size + section_header_size * section_count,
        file_alignment,
    );

    let text_raw_size = align_up(text.len() as u32, file_alignment);
    let rdata_raw_size = if has_rdata {
        align_up(rdata_len, file_alignment)
    } else {
        0
    };

    let text_raw_offset = headers_size;
    let rdata_raw_offset = text_raw_offset + text_raw_size;

    let size_of_image = if has_rdata {
        align_up(
            rdata_rva + align_up(rdata_len, section_alignment),
            section_alignment,
        )
    } else {
        align_up(text_rva + align_up(text.len() as u32, section_alignment), section_alignment)
    };
    let entry_rva = text_rva
        + u32::try_from(plan.entry_offset).map_err(|_| Error::from("entry offset out of range"))?;

    let (import_table_rva, import_table_size, iat_rva) =
        if let Some(layout) = import_layout.as_mut() {
            let base_rva = rdata_rva + import_start;
            patch_import_layout(layout, base_rva);
            (base_rva + layout.desc_offset, layout.desc_size, base_rva + layout.iat_offset)
        } else {
            (0, 0, 0)
        };

    if stub_size > 0 {
        let mut extern_index = HashMap::new();
        for (idx, sym) in externs.iter().enumerate() {
            extern_index.insert(sym.name.clone(), idx);
        }
        for reloc in &plan.relocs {
            if reloc.kind != RelocKind::CallRel32 {
                continue;
            }
            let idx = *extern_index
                .get(&reloc.symbol)
                .ok_or_else(|| Error::from("missing external symbol"))?;
            let stub_rva = text_rva + plan.text.len() as u32 + (idx as u32 * stub_size as u32);
            let call_site = text_rva + reloc.offset as u32;
            let rel = stub_rva as i64 - (call_site as i64 + 4);
            let rel32 = i32::try_from(rel).map_err(|_| Error::from("call target out of range"))?;
            let offset = reloc.offset as usize;
            text[offset..offset + 4].copy_from_slice(&rel32.to_le_bytes());
        }

        for idx in 0..externs.len() {
            let stub_offset = plan.text.len() + idx * stub_size;
            let stub_rva = text_rva + stub_offset as u32;
            let iat_entry_rva = iat_rva + (idx as u32 * 8);
            let disp = iat_entry_rva as i64 - (stub_rva as i64 + 6);
            let disp32 = i32::try_from(disp).map_err(|_| Error::from("IAT out of range"))?;
            text[stub_offset..stub_offset + 6]
                .copy_from_slice(&[0xFF, 0x25, disp32.to_le_bytes()[0], disp32.to_le_bytes()[1], disp32.to_le_bytes()[2], disp32.to_le_bytes()[3]]);
        }
    }

    let mut out = Vec::new();

    // DOS header
    put_u16(&mut out, 0x5A4D);
    out.resize(dos_header_size as usize, 0);
    out.resize(pe_offset as usize, 0);

    // PE signature
    out.extend_from_slice(b"PE\0\0");

    // COFF header
    put_u16(&mut out, coff_machine(arch));
    put_u16(&mut out, section_count as u16);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u16(&mut out, optional_header_size as u16);
    put_u16(
        &mut out,
        IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
    );

    // Optional header (PE32+)
    put_u16(&mut out, 0x20B);
    out.push(0);
    out.push(0);
    put_u32(&mut out, text_raw_size);
    put_u32(&mut out, rdata_raw_size);
    put_u32(&mut out, 0);
    put_u32(&mut out, entry_rva);
    put_u32(&mut out, text_rva);
    put_u64(&mut out, image_base);
    put_u32(&mut out, section_alignment);
    put_u32(&mut out, file_alignment);
    put_u16(&mut out, 6);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 6);
    put_u16(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, size_of_image);
    put_u32(&mut out, headers_size);
    put_u32(&mut out, 0);
    put_u16(&mut out, IMAGE_SUBSYSTEM_WINDOWS_CUI);
    put_u16(&mut out, 0);
    put_u64(&mut out, 0x100000);
    put_u64(&mut out, 0x1000);
    put_u64(&mut out, 0x100000);
    put_u64(&mut out, 0x1000);
    put_u32(&mut out, 0);
    put_u32(&mut out, 16);

    for idx in 0..16 {
        if idx == 1 {
            put_u32(&mut out, import_table_rva);
            put_u32(&mut out, import_table_size);
        } else {
            put_u32(&mut out, 0);
            put_u32(&mut out, 0);
        }
    }

    // Section headers
    put_bytes_fixed::<8>(&mut out, ".text");
    put_u32(&mut out, text.len() as u32);
    put_u32(&mut out, text_rva);
    put_u32(&mut out, text_raw_size);
    put_u32(&mut out, text_raw_offset);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u32(&mut out, IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ);

    if has_rdata {
        put_bytes_fixed::<8>(&mut out, ".rdata");
        put_u32(&mut out, rdata_len);
        put_u32(&mut out, rdata_rva);
        put_u32(&mut out, rdata_raw_size);
        put_u32(&mut out, rdata_raw_offset);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u16(&mut out, 0);
        put_u16(&mut out, 0);
        put_u32(&mut out, IMAGE_SCN_MEM_READ);
    }

    if out.len() > headers_size as usize {
        return Err(Error::from("internal PE header size mismatch"));
    }
    out.resize(headers_size as usize, 0);

    // .text section
    out.extend_from_slice(&text);
    out.resize((text_raw_offset + text_raw_size) as usize, 0);
    if has_rdata {
        out.extend_from_slice(&plan.rodata);
        if let Some(layout) = import_layout.as_ref() {
            let padding = import_start as usize - plan.rodata.len();
            if padding > 0 {
                out.resize(out.len() + padding, 0);
            }
            out.extend_from_slice(&layout.data);
        }
        out.resize((rdata_raw_offset + rdata_raw_size) as usize, 0);
    }

    let text_addr = image_base + text_rva as u64;
    let rodata_addr = image_base + rdata_rva as u64;
    let resolve_symbol = |name: &str, addend: i64| -> Result<u64> {
        if name == ".rodata" {
            Ok(rodata_addr.wrapping_add(addend as u64))
        } else if let Some(offset) = plan.symbols.get(name) {
            Ok(text_addr.wrapping_add(*offset).wrapping_add(addend as u64))
        } else {
            Err(Error::from("unsupported relocation in PE executable"))
        }
    };
    for reloc in &plan.relocs {
        match reloc.kind {
            crate::emit::RelocKind::Abs64 => {
                let value = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let offset = text_raw_offset as usize + reloc.offset as usize;
                if offset + 8 > out.len() {
                    return Err(Error::from("relocation offset out of range"));
                }
                out[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
            }
            crate::emit::RelocKind::CallRel32 => {
                if externs.is_empty() {
                    return Err(Error::from("missing external import for call relocation"));
                }
            }
            crate::emit::RelocKind::Aarch64AdrpAdd => {
                return Err(Error::from("unexpected AArch64 relocation in PE executable"));
            }
        }
    }

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
