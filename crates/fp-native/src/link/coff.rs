use super::TargetArch;
use crate::emit::{EmitPlan, RelocKind, Relocation};
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

fn emit_arm64_stub(buf: &mut [u8], stub_rva: u64, ptr_rva: u64) -> Result<()> {
    let stub_page = stub_rva & !0xfff;
    let ptr_page = ptr_rva & !0xfff;
    let page_delta = ((ptr_page as i64 - stub_page as i64) >> 12) as i64;
    if page_delta < -(1 << 20) || page_delta > (1 << 20) - 1 {
        return Err(Error::from("stub page delta out of range"));
    }
    let imm = page_delta as u32;
    let immlo = (imm & 0x3) << 29;
    let immhi = ((imm >> 2) & 0x7ffff) << 5;
    let adrp = 0x9000_0000u32 | immlo | immhi | 16;
    let pageoff = (ptr_rva & 0xfff) as u32;
    let imm12 = (pageoff / 8) << 10;
    let ldr = 0xF940_0000u32 | imm12 | (16 << 5) | 16;
    let br = 0xD61F_0000u32 | (16 << 5);
    buf[0..4].copy_from_slice(&adrp.to_le_bytes());
    buf[4..8].copy_from_slice(&ldr.to_le_bytes());
    buf[8..12].copy_from_slice(&br.to_le_bytes());
    Ok(())
}

struct ExternSymbol {
    symbol: String,
    dll: String,
    name: String,
}

struct ImportSymbol {
    symbol: String,
    name: String,
    hint_name_offset: u32,
}

struct ImportDll {
    name: String,
    desc_offset: u32,
    ilt_offset: u32,
    iat_offset: u32,
    name_offset: u32,
    symbols: Vec<ImportSymbol>,
}

struct ImportLayout {
    data: Vec<u8>,
    desc_offset: u32,
    desc_size: u32,
    dlls: Vec<ImportDll>,
}

fn split_import_symbol(symbol: &str) -> (String, String) {
    const DEFAULT_DLL: &str = "msvcrt.dll";
    if let Some((dll, name)) = symbol.split_once('!') {
        let mut dll = dll.trim().to_string();
        if !dll.to_ascii_lowercase().ends_with(".dll") {
            dll.push_str(".dll");
        }
        return (dll, name.trim().to_string());
    }
    (DEFAULT_DLL.to_string(), symbol.to_string())
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
        let (dll, name) = split_import_symbol(&reloc.symbol);
        externs.push(ExternSymbol {
            symbol: reloc.symbol.clone(),
            dll,
            name,
        });
    }
    externs
}

fn build_import_layout(externs: &[ExternSymbol]) -> ImportLayout {
    let mut data = Vec::new();
    let desc_offset = 0u32;
    let mut dlls: Vec<ImportDll> = Vec::new();
    let mut dll_index: HashMap<String, usize> = HashMap::new();
    for ext in externs {
        let idx = *dll_index.entry(ext.dll.clone()).or_insert_with(|| {
            dlls.push(ImportDll {
                name: ext.dll.clone(),
                desc_offset: 0,
                ilt_offset: 0,
                iat_offset: 0,
                name_offset: 0,
                symbols: Vec::new(),
            });
            dlls.len() - 1
        });
        dlls[idx].symbols.push(ImportSymbol {
            symbol: ext.symbol.clone(),
            name: ext.name.clone(),
            hint_name_offset: 0,
        });
    }
    let desc_size = ((dlls.len() + 1) * 20) as u32;
    data.resize(desc_size as usize, 0);

    for (idx, dll) in dlls.iter_mut().enumerate() {
        dll.desc_offset = desc_offset + (idx as u32) * 20;

        dll.ilt_offset = align_up(data.len() as u32, 8);
        let ilt_size = ((dll.symbols.len() + 1) * 8) as u32;
        data.resize((dll.ilt_offset + ilt_size) as usize, 0);

        dll.iat_offset = align_up(data.len() as u32, 8);
        let iat_size = ((dll.symbols.len() + 1) * 8) as u32;
        data.resize((dll.iat_offset + iat_size) as usize, 0);

        for sym in &mut dll.symbols {
            let offset = align_up(data.len() as u32, 2);
            data.resize(offset as usize, 0);
            sym.hint_name_offset = data.len() as u32;
            data.extend_from_slice(&0u16.to_le_bytes());
            data.extend_from_slice(sym.name.as_bytes());
            data.push(0);
        }

        dll.name_offset = data.len() as u32;
        data.extend_from_slice(dll.name.as_bytes());
        data.push(0);
    }

    ImportLayout {
        data,
        desc_offset,
        desc_size,
        dlls,
    }
}

fn patch_import_layout(layout: &mut ImportLayout, base_rva: u32) -> HashMap<String, u32> {
    let mut iat_rvas = HashMap::new();
    for dll in &layout.dlls {
        let desc_start = dll.desc_offset as usize;
        let ilt_rva = base_rva + dll.ilt_offset;
        let iat_rva = base_rva + dll.iat_offset;
        let name_rva = base_rva + dll.name_offset;

        write_u32(&mut layout.data, desc_start, ilt_rva);
        write_u32(&mut layout.data, desc_start + 12, name_rva);
        write_u32(&mut layout.data, desc_start + 16, iat_rva);

        for (idx, sym) in dll.symbols.iter().enumerate() {
            let entry_rva = base_rva + sym.hint_name_offset;
            let ilt_entry = dll.ilt_offset as usize + idx * 8;
            let iat_entry = dll.iat_offset as usize + idx * 8;
            write_u64(&mut layout.data, ilt_entry, entry_rva as u64);
            write_u64(&mut layout.data, iat_entry, entry_rva as u64);
            iat_rvas.insert(sym.symbol.clone(), iat_rva + (idx as u32) * 8);
        }
    }
    iat_rvas
}

fn write_u32(buf: &mut [u8], offset: usize, value: u32) {
    buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn write_u64(buf: &mut [u8], offset: usize, value: u64) {
    buf[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
}

struct StringTable {
    data: Vec<u8>,
}

impl StringTable {
    fn new() -> Self {
        Self {
            data: vec![0u8; 4],
        }
    }

    fn insert(&mut self, name: &str) -> u32 {
        let offset = self.data.len() as u32;
        self.data.extend_from_slice(name.as_bytes());
        self.data.push(0);
        offset
    }

    fn finish(mut self) -> Vec<u8> {
        let size = self.data.len() as u32;
        self.data[0..4].copy_from_slice(&size.to_le_bytes());
        self.data
    }
}

struct CoffSymbol {
    name: String,
    value: u32,
    section: i16,
    typ: u16,
    storage_class: u8,
}

struct CoffReloc {
    offset: u32,
    symbol_index: u32,
    reloc_type: u16,
}

fn build_base_relocs(text_rva: u32, relocs: &[Relocation], extra_rvas: &[u32]) -> Vec<u8> {
    const IMAGE_REL_BASED_DIR64: u16 = 10;

    let mut entries: Vec<u32> = relocs
        .iter()
        .filter(|reloc| reloc.kind == RelocKind::Abs64)
        .filter_map(|reloc| u32::try_from(reloc.offset).ok())
        .map(|offset| text_rva + offset)
        .collect();
    entries.extend(extra_rvas.iter().copied());
    if entries.is_empty() {
        return Vec::new();
    }

    entries.sort_unstable();
    let mut out = Vec::new();
    let mut idx = 0;
    while idx < entries.len() {
        let page = entries[idx] & 0xFFFF_F000;
        let mut block_entries = Vec::new();
        while idx < entries.len() && (entries[idx] & 0xFFFF_F000) == page {
            let offset = (entries[idx] & 0xFFF) as u16;
            block_entries.push((IMAGE_REL_BASED_DIR64 << 12) | offset);
            idx += 1;
        }
        let mut block_size = 8 + block_entries.len() * 2;
        if block_size % 4 != 0 {
            block_entries.push(0);
            block_size += 2;
        }
        put_u32(&mut out, page);
        put_u32(&mut out, block_size as u32);
        for entry in block_entries {
            put_u16(&mut out, entry);
        }
    }
    out
}

pub fn emit_object_coff(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
    const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
    const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
    const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;

    const IMAGE_SYM_UNDEFINED: i16 = 0;
    const IMAGE_SYM_TYPE_FUNCTION: u16 = 0x20;

    const IMAGE_REL_AMD64_ADDR64: u16 = 0x0001;
    const IMAGE_REL_AMD64_REL32: u16 = 0x0004;
    const IMAGE_REL_ARM64_ADDR64: u16 = 0x0001;
    const IMAGE_REL_ARM64_BRANCH26: u16 = 0x0003;
    const IMAGE_REL_ARM64_ADR_PREL_PG_HI21: u16 = 0x0005;
    const IMAGE_REL_ARM64_ADD_LO12: u16 = 0x0006;

    let has_rdata = !plan.rodata.is_empty();
    let section_count = if has_rdata { 2u16 } else { 1u16 };

    let header_size = 20u32;
    let section_header_size = 40u32;
    let section_table_size = section_header_size * section_count as u32;
    let text_offset = header_size + section_table_size;
    let text_size = plan.text.len() as u32;
    let rdata_offset = text_offset + text_size;
    let rdata_size = plan.rodata.len() as u32;

    let mut symbol_table: Vec<CoffSymbol> = Vec::new();
    let mut symbol_index: HashMap<String, u32> = HashMap::new();

    for (name, offset) in &plan.symbols {
        let idx = symbol_table.len() as u32;
        symbol_index.insert(name.clone(), idx);
        symbol_table.push(CoffSymbol {
            name: name.clone(),
            value: *offset as u32,
            section: 1,
            typ: IMAGE_SYM_TYPE_FUNCTION,
            storage_class: IMAGE_SYM_CLASS_EXTERNAL,
        });
    }

    if has_rdata {
        let idx = symbol_table.len() as u32;
        symbol_index.insert(".rodata".to_string(), idx);
        symbol_table.push(CoffSymbol {
            name: ".rodata".to_string(),
            value: 0,
            section: 2,
            typ: 0,
            storage_class: IMAGE_SYM_CLASS_EXTERNAL,
        });
    }

    for reloc in &plan.relocs {
        if reloc.kind == RelocKind::CallRel32 {
            if symbol_index.contains_key(&reloc.symbol) {
                continue;
            }
            let idx = symbol_table.len() as u32;
            symbol_index.insert(reloc.symbol.clone(), idx);
            symbol_table.push(CoffSymbol {
                name: reloc.symbol.clone(),
                value: 0,
                section: IMAGE_SYM_UNDEFINED,
                typ: IMAGE_SYM_TYPE_FUNCTION,
                storage_class: IMAGE_SYM_CLASS_EXTERNAL,
            });
        }
    }

    let mut relocs = Vec::new();
    for reloc in &plan.relocs {
        let symbol = if reloc.symbol == ".rodata" && has_rdata {
            ".rodata"
        } else {
            reloc.symbol.as_str()
        };
        let symbol_idx = *symbol_index
            .get(symbol)
            .ok_or_else(|| Error::from("missing symbol for COFF relocation"))?;
        match (arch, reloc.kind) {
            (TargetArch::X86_64, RelocKind::Abs64) => relocs.push(CoffReloc {
                offset: reloc.offset as u32,
                symbol_index: symbol_idx,
                reloc_type: IMAGE_REL_AMD64_ADDR64,
            }),
            (TargetArch::X86_64, RelocKind::CallRel32) => relocs.push(CoffReloc {
                offset: reloc.offset as u32,
                symbol_index: symbol_idx,
                reloc_type: IMAGE_REL_AMD64_REL32,
            }),
            (TargetArch::Aarch64, RelocKind::Abs64) => relocs.push(CoffReloc {
                offset: reloc.offset as u32,
                symbol_index: symbol_idx,
                reloc_type: IMAGE_REL_ARM64_ADDR64,
            }),
            (TargetArch::Aarch64, RelocKind::CallRel32) => relocs.push(CoffReloc {
                offset: reloc.offset as u32,
                symbol_index: symbol_idx,
                reloc_type: IMAGE_REL_ARM64_BRANCH26,
            }),
            (TargetArch::Aarch64, RelocKind::Aarch64AdrpAdd) => {
                relocs.push(CoffReloc {
                    offset: reloc.offset as u32,
                    symbol_index: symbol_idx,
                    reloc_type: IMAGE_REL_ARM64_ADR_PREL_PG_HI21,
                });
                relocs.push(CoffReloc {
                    offset: (reloc.offset + 4) as u32,
                    symbol_index: symbol_idx,
                    reloc_type: IMAGE_REL_ARM64_ADD_LO12,
                });
            }
            (_, RelocKind::Aarch64AdrpAdd) => {
                return Err(Error::from("AArch64 relocations are not supported for x86_64 COFF"));
            }
        }
    }

    let reloc_offset = rdata_offset + rdata_size;
    let reloc_count = relocs.len() as u16;
    let reloc_table_size = relocs.len() as u32 * 10;
    let pointer_to_symbols = reloc_offset + reloc_table_size;

    let mut out = Vec::new();
    put_u16(&mut out, coff_machine(arch));
    put_u16(&mut out, section_count);
    put_u32(&mut out, 0);
    put_u32(&mut out, pointer_to_symbols);
    put_u32(&mut out, symbol_table.len() as u32);
    put_u16(&mut out, 0); // optional header size
    put_u16(&mut out, 0); // characteristics

    put_bytes_fixed::<8>(&mut out, ".text");
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, text_size);
    put_u32(&mut out, text_offset);
    put_u32(&mut out, reloc_offset);
    put_u32(&mut out, 0);
    put_u16(&mut out, reloc_count);
    put_u16(&mut out, 0);
    put_u32(&mut out, IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ);

    if has_rdata {
        put_bytes_fixed::<8>(&mut out, ".rdata");
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u32(&mut out, rdata_size);
        put_u32(&mut out, rdata_offset);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u16(&mut out, 0);
        put_u16(&mut out, 0);
        put_u32(&mut out, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ);
    }

    out.extend_from_slice(&plan.text);
    if has_rdata {
        out.extend_from_slice(&plan.rodata);
    }

    for reloc in &relocs {
        put_u32(&mut out, reloc.offset);
        put_u32(&mut out, reloc.symbol_index);
        put_u16(&mut out, reloc.reloc_type);
    }

    let mut string_table = StringTable::new();
    for sym in &symbol_table {
        if sym.name.len() <= 8 {
            put_bytes_fixed::<8>(&mut out, &sym.name);
        } else {
            let offset = string_table.insert(&sym.name);
            put_u32(&mut out, 0);
            put_u32(&mut out, offset);
        }
        put_u32(&mut out, sym.value);
        put_u16(&mut out, sym.section as u16);
        put_u16(&mut out, sym.typ);
        out.push(sym.storage_class);
        out.push(0);
    }

    out.extend_from_slice(&string_table.finish());

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
    const IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE: u16 = 0x0040;

    const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
    const IMAGE_SCN_MEM_DISCARDABLE: u32 = 0x0200_0000;

    if plan.text.is_empty() {
        return Err(Error::from("PE executable requires non-empty text"));
    }

    let externs = collect_external_symbols(plan);
    let stub_size = if externs.is_empty() {
        0
    } else {
        match arch {
            TargetArch::X86_64 => 6,
            TargetArch::Aarch64 => 12,
        }
    };
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

    let reloc_data = build_base_relocs(text_rva, &plan.relocs, &[]);
    let reloc_len = reloc_data.len() as u32;
    let has_reloc = reloc_len > 0;

    let rdata_rva = if has_rdata {
        align_up(
            text_rva + align_up(text.len() as u32, section_alignment),
            section_alignment,
        )
    } else {
        0
    };

    let reloc_rva = if has_reloc {
        let base = if has_rdata {
            rdata_rva + align_up(rdata_len, section_alignment)
        } else {
            align_up(text_rva + align_up(text.len() as u32, section_alignment), section_alignment)
        };
        base
    } else {
        0
    };

    let section_count =
        1u32 + u32::from(has_rdata) + u32::from(has_reloc);
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
    let reloc_raw_size = if has_reloc {
        align_up(reloc_len, file_alignment)
    } else {
        0
    };

    let text_raw_offset = headers_size;
    let rdata_raw_offset = text_raw_offset + text_raw_size;
    let reloc_raw_offset = rdata_raw_offset + rdata_raw_size;

    let mut size_of_image = if has_rdata {
        align_up(
            rdata_rva + align_up(rdata_len, section_alignment),
            section_alignment,
        )
    } else {
        align_up(text_rva + align_up(text.len() as u32, section_alignment), section_alignment)
    };
    if has_reloc {
        size_of_image = align_up(
            reloc_rva + align_up(reloc_len, section_alignment),
            section_alignment,
        );
    }
    let entry_rva = text_rva
        + u32::try_from(plan.entry_offset).map_err(|_| Error::from("entry offset out of range"))?;

    let (import_table_rva, import_table_size, iat_rvas) =
        if let Some(layout) = import_layout.as_mut() {
            let base_rva = rdata_rva + import_start;
            let iat_rvas = patch_import_layout(layout, base_rva);
            (base_rva + layout.desc_offset, layout.desc_size, iat_rvas)
        } else {
            (0, 0, HashMap::new())
        };

    if stub_size > 0 {
        let mut extern_index = HashMap::new();
        for (idx, sym) in externs.iter().enumerate() {
            extern_index.insert(sym.symbol.clone(), idx);
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
            let offset = reloc.offset as usize;
            match arch {
                TargetArch::X86_64 => {
                    let rel = stub_rva as i64 - (call_site as i64 + 4);
                    let rel32 =
                        i32::try_from(rel).map_err(|_| Error::from("call target out of range"))?;
                    text[offset..offset + 4].copy_from_slice(&rel32.to_le_bytes());
                }
                TargetArch::Aarch64 => {
                    let delta = stub_rva as i64 - call_site as i64;
                    let imm = delta / 4;
                    if imm < -(1 << 25) || imm > (1 << 25) - 1 {
                        return Err(Error::from("call target out of range"));
                    }
                    let encoded = 0x9400_0000u32 | ((imm as u32) & 0x03FF_FFFF);
                    text[offset..offset + 4].copy_from_slice(&encoded.to_le_bytes());
                }
            }
        }

        for idx in 0..externs.len() {
            let stub_offset = plan.text.len() + idx * stub_size;
            let stub_rva = text_rva + stub_offset as u32;
            let iat_entry_rva = *iat_rvas
                .get(&externs[idx].symbol)
                .ok_or_else(|| Error::from("missing IAT entry for external symbol"))?;
            match arch {
                TargetArch::X86_64 => {
                    let disp = iat_entry_rva as i64 - (stub_rva as i64 + 6);
                    let disp32 =
                        i32::try_from(disp).map_err(|_| Error::from("IAT out of range"))?;
                    text[stub_offset..stub_offset + 6].copy_from_slice(&[
                        0xFF,
                        0x25,
                        disp32.to_le_bytes()[0],
                        disp32.to_le_bytes()[1],
                        disp32.to_le_bytes()[2],
                        disp32.to_le_bytes()[3],
                    ]);
                }
                TargetArch::Aarch64 => {
                    let start = stub_offset as usize;
                    let end = start + 12;
                    emit_arm64_stub(
                        &mut text[start..end],
                        image_base + stub_rva as u64,
                        image_base + iat_entry_rva as u64,
                    )?;
                }
            }
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
    let dll_characteristics = if has_reloc {
        IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
    } else {
        0
    };
    put_u16(&mut out, dll_characteristics);
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
        match idx {
            1 => {
                put_u32(&mut out, import_table_rva);
                put_u32(&mut out, import_table_size);
            }
            5 => {
                put_u32(&mut out, reloc_rva);
                put_u32(&mut out, reloc_len);
            }
            _ => {
                put_u32(&mut out, 0);
                put_u32(&mut out, 0);
            }
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
        put_u32(&mut out, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ);
    }

    if has_reloc {
        put_bytes_fixed::<8>(&mut out, ".reloc");
        put_u32(&mut out, reloc_len);
        put_u32(&mut out, reloc_rva);
        put_u32(&mut out, reloc_raw_size);
        put_u32(&mut out, reloc_raw_offset);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u16(&mut out, 0);
        put_u16(&mut out, 0);
        put_u32(
            &mut out,
            IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE,
        );
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
    if has_reloc {
        out.extend_from_slice(&reloc_data);
        out.resize((reloc_raw_offset + reloc_raw_size) as usize, 0);
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
                if !matches!(arch, TargetArch::Aarch64) {
                    return Err(Error::from("AArch64 relocation on non-AArch64 target"));
                }
                let target = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let adrp_addr = text_addr + reloc.offset;
                let pc_page = adrp_addr & !0xfff;
                let target_page = target & !0xfff;
                let delta_pages = (target_page as i64 - pc_page as i64) >> 12;
                if delta_pages < -(1 << 20) || delta_pages > (1 << 20) - 1 {
                    return Err(Error::from("adrp target out of range"));
                }
                let imm = delta_pages as u32;
                let immlo = imm & 0x3;
                let immhi = (imm >> 2) & 0x7ffff;
                let adrp_offset = text_raw_offset as usize + reloc.offset as usize;
                let mut adrp = u32::from_le_bytes(
                    out[adrp_offset..adrp_offset + 4]
                        .try_into()
                        .map_err(|_| Error::from("adrp relocation out of range"))?,
                );
                adrp &= !((0x3 << 29) | (0x7ffff << 5));
                adrp |= (immlo << 29) | (immhi << 5);
                out[adrp_offset..adrp_offset + 4].copy_from_slice(&adrp.to_le_bytes());

                let add_offset = adrp_offset + 4;
                let imm12 = (target & 0xfff) as u32;
                let mut add = u32::from_le_bytes(
                    out[add_offset..add_offset + 4]
                        .try_into()
                        .map_err(|_| Error::from("add relocation out of range"))?,
                );
                add &= !(0xfff << 10);
                add |= imm12 << 10;
                out[add_offset..add_offset + 4].copy_from_slice(&add.to_le_bytes());
            }
        }
    }

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
