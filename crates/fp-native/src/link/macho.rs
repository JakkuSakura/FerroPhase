use fp_core::error::{Error, Result};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::process::Command;

use super::TargetArch;
use crate::emit::{EmitPlan, RelocKind};

pub fn link_executable_macho(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    emit_executable_macho(path, arch, plan)
}

fn align_up(value: u64, align: u64) -> u64 {
    debug_assert!(align.is_power_of_two());
    (value + (align - 1)) & !(align - 1)
}

fn put_u32(out: &mut Vec<u8>, x: u32) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_u16(out: &mut Vec<u8>, x: u16) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_u64(out: &mut Vec<u8>, x: u64) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_i32(out: &mut Vec<u8>, x: i32) {
    out.extend_from_slice(&x.to_le_bytes());
}

fn put_bytes_fixed<const N: usize>(out: &mut Vec<u8>, s: &str) {
    let mut buf = [0u8; N];
    let b = s.as_bytes();
    let n = b.len().min(N);
    buf[..n].copy_from_slice(&b[..n]);
    out.extend_from_slice(&buf);
}

fn ensure(condition: bool, msg: &'static str) -> Result<()> {
    if condition {
        Ok(())
    } else {
        Err(Error::from(msg))
    }
}

struct ExternSymbol {
    original: String,
    name: String,
    stub_offset: u64,
    ptr_offset: u64,
}

fn macho_symbol_name(name: &str) -> String {
    if name.starts_with('_') {
        name.to_string()
    } else {
        format!("_{}", name)
    }
}

fn collect_external_symbols(plan: &EmitPlan, stub_size: u64) -> Vec<ExternSymbol> {
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
            original: reloc.symbol.clone(),
            name: macho_symbol_name(&reloc.symbol),
            stub_offset: 0,
            ptr_offset: 0,
        });
    }
    for (idx, sym) in externs.iter_mut().enumerate() {
        sym.stub_offset = stub_size * idx as u64;
        sym.ptr_offset = 8 * idx as u64;
    }
    externs
}

fn push_uleb128(out: &mut Vec<u8>, mut value: u64) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if value == 0 {
            out.push(byte);
            break;
        } else {
            out.push(byte | 0x80);
        }
    }
}

fn build_bind_info(externs: &[ExternSymbol], data_seg_index: u8) -> Vec<u8> {
    const BIND_OPCODE_DONE: u8 = 0x00;
    const BIND_OPCODE_SET_DYLIB_ORDINAL_IMM: u8 = 0x10;
    const BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM: u8 = 0x40;
    const BIND_OPCODE_SET_TYPE_IMM: u8 = 0x50;
    const BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u8 = 0x70;
    const BIND_OPCODE_DO_BIND: u8 = 0x90;
    const BIND_TYPE_POINTER: u8 = 1;

    let mut out = Vec::new();
    for sym in externs {
        out.push(BIND_OPCODE_SET_DYLIB_ORDINAL_IMM | 1);
        out.push(BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM);
        out.extend_from_slice(sym.name.as_bytes());
        out.push(0);
        out.push(BIND_OPCODE_SET_TYPE_IMM | BIND_TYPE_POINTER);
        out.push(BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB | data_seg_index);
        push_uleb128(&mut out, sym.ptr_offset);
        out.push(BIND_OPCODE_DO_BIND);
    }
    out.push(BIND_OPCODE_DONE);
    out
}

fn build_rebase_info(offsets: &[u64], text_seg_index: u8) -> Vec<u8> {
    const REBASE_OPCODE_DONE: u8 = 0x00;
    const REBASE_OPCODE_SET_TYPE_IMM: u8 = 0x10;
    const REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB: u8 = 0x20;
    const REBASE_OPCODE_DO_REBASE_IMM_TIMES: u8 = 0x50;
    const REBASE_TYPE_POINTER: u8 = 1;

    if offsets.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    out.push(REBASE_OPCODE_SET_TYPE_IMM | REBASE_TYPE_POINTER);
    for offset in offsets {
        out.push(REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB | text_seg_index);
        push_uleb128(&mut out, *offset);
        out.push(REBASE_OPCODE_DO_REBASE_IMM_TIMES | 1);
    }
    out.push(REBASE_OPCODE_DONE);
    out
}

fn build_strtab(externs: &[ExternSymbol]) -> (Vec<u8>, HashMap<String, u32>) {
    let mut out = vec![0u8];
    let mut offsets = HashMap::new();
    offsets.insert("_main".to_string(), out.len() as u32);
    out.extend_from_slice(b"_main\0");
    for sym in externs {
        offsets.insert(sym.name.clone(), out.len() as u32);
        out.extend_from_slice(sym.name.as_bytes());
        out.push(0);
    }
    (out, offsets)
}

fn emit_x86_stub(buf: &mut [u8], stub_addr: u64, ptr_addr: u64) -> Result<()> {
    let disp = ptr_addr as i64 - (stub_addr as i64 + 6);
    let disp32 = i32::try_from(disp).map_err(|_| Error::from("stub target out of range"))?;
    buf.copy_from_slice(&[0xFF, 0x25, disp32.to_le_bytes()[0], disp32.to_le_bytes()[1], disp32.to_le_bytes()[2], disp32.to_le_bytes()[3]]);
    Ok(())
}

fn emit_arm64_stub(buf: &mut [u8], stub_addr: u64, ptr_addr: u64) -> Result<()> {
    let stub_page = stub_addr & !0xfff;
    let ptr_page = ptr_addr & !0xfff;
    let page_delta = ((ptr_page as i64 - stub_page as i64) >> 12) as i64;
    if page_delta < -(1 << 20) || page_delta > (1 << 20) - 1 {
        return Err(Error::from("stub page delta out of range"));
    }
    let imm = page_delta as u32;
    let immlo = (imm & 0x3) << 29;
    let immhi = ((imm >> 2) & 0x7ffff) << 5;
    let adrp = 0x9000_0000u32 | immlo | immhi | 16;
    let pageoff = (ptr_addr & 0xfff) as u32;
    let imm12 = (pageoff / 8) << 10;
    let ldr = 0xF940_0000u32 | imm12 | (16 << 5) | 16;
    let br = 0xD61F_0000u32 | (16 << 5);
    buf[0..4].copy_from_slice(&adrp.to_le_bytes());
    buf[4..8].copy_from_slice(&ldr.to_le_bytes());
    buf[8..12].copy_from_slice(&br.to_le_bytes());
    Ok(())
}

/// Emit a minimal 64-bit Mach-O executable (`MH_EXECUTE`) that starts at `_main`
/// and returns 0.
///
/// This intentionally avoids the system linker. It does not support dynamic
/// libraries, relocations, or any calls into libc yet.
pub fn emit_executable_macho(
    path: &Path,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    // Constants from mach-o/loader.h
    const MH_MAGIC_64: u32 = 0xfeedfacf;
    const MH_EXECUTE: u32 = 0x2;
    const MH_PIE: u32 = 0x200000;
    const MH_DYLDLINK: u32 = 0x4;
    const MH_TWOLEVEL: u32 = 0x80;

    const LC_SEGMENT_64: u32 = 0x19;
    const LC_MAIN: u32 = 0x80000028;
    const LC_DYLD_INFO_ONLY: u32 = 0x8000_0022;
    const LC_SYMTAB: u32 = 0x2;
    const LC_DYSYMTAB: u32 = 0xb;
    const LC_LOAD_DYLINKER: u32 = 0xe;
    const LC_LOAD_DYLIB: u32 = 0xc;
    const LC_BUILD_VERSION: u32 = 0x32;

    const VM_PROT_READ: i32 = 0x1;
    const VM_PROT_EXECUTE: i32 = 0x4;
    const VM_PROT_WRITE: i32 = 0x2;

    const S_REGULAR: u32 = 0x0;
    const S_SYMBOL_STUBS: u32 = 0x8;
    const S_NON_LAZY_SYMBOL_POINTERS: u32 = 0x6;
    const S_ATTR_PURE_INSTRUCTIONS: u32 = 0x8000_0000;
    const S_ATTR_SOME_INSTRUCTIONS: u32 = 0x0000_0400;

    // Choose architecture and machine code.
    // - x86_64: xor eax,eax; ret
    // - arm64: mov w0, #0; ret
    let (cputype, cpusubtype): (u32, u32) = match arch {
        TargetArch::Aarch64 => {
            const CPU_TYPE_ARM64: u32 = 0x0100000c;
            const CPU_SUBTYPE_ARM64_ALL: u32 = 0;
            (CPU_TYPE_ARM64, CPU_SUBTYPE_ARM64_ALL)
        }
        TargetArch::X86_64 => {
            const CPU_TYPE_X86_64: u32 = 0x01000007;
            const CPU_SUBTYPE_X86_64_ALL: u32 = 3;
            (CPU_TYPE_X86_64, CPU_SUBTYPE_X86_64_ALL)
        }
    };

    let page: u64 = 0x4000;
    let vmaddr_text: u64 = 0x1000_0000_0;
    let vmaddr_data: u64;
    let vmaddr_linkedit: u64;

    let stub_size = match arch {
        TargetArch::X86_64 => 6u64,
        TargetArch::Aarch64 => 12u64,
    };
    let stub_align = match arch {
        TargetArch::X86_64 => 1u32,
        TargetArch::Aarch64 => 2u32,
    };
    let externs = collect_external_symbols(plan, stub_size);
    let has_stubs = !externs.is_empty();
    let has_rodata = !plan.rodata.is_empty();

    let header_size = 32u64;
    let segment_cmd_size = 72u64;
    let section_size = 80u64;
    let text_section_count = 1u64 + if has_stubs { 1 } else { 0 } + if has_rodata { 1 } else { 0 };
    let data_section_count = if has_stubs { 1u64 } else { 0u64 };

    let lc_segment_text_size = segment_cmd_size + section_size * text_section_count;
    let lc_segment_data_size = segment_cmd_size + section_size * data_section_count;
    let lc_segment_linkedit_size = segment_cmd_size;
    let lc_main_size = 24u64;
    let lc_dyld_info_size = 48u64;
    let lc_symtab_size = 24u64;
    let lc_dysymtab_size = 80u64;
    let lc_segment_pagezero_size = segment_cmd_size;
    let lc_build_version_size = 24u64;
    let lc_codesig_size = 16u64;

    let dyld_path = b"/usr/lib/dyld\0";
    let dylib_path = b"/usr/lib/libSystem.B.dylib\0";
    let lc_dylinker_size = align_up((12 + dyld_path.len()) as u64, 8);
    let lc_dylib_size = align_up((24 + dylib_path.len()) as u64, 8);

    let ncmds = 11u32;
    let sizeofcmds = (lc_segment_text_size
        + lc_segment_pagezero_size
        + lc_segment_data_size
        + lc_segment_linkedit_size
        + lc_main_size
        + lc_dyld_info_size
        + lc_symtab_size
        + lc_dysymtab_size
        + lc_dylinker_size
        + lc_dylib_size
        + lc_build_version_size) as u32;

    let file_start_of_text = align_up(header_size + sizeofcmds as u64 + lc_codesig_size, 16);
    let text_fileoff = 0u64;
    let text_offset = file_start_of_text;
    let text_size = plan.text.len() as u64;
    let stubs_offset = align_up(text_offset + text_size, 16);
    let stubs_size = if has_stubs { stub_size * externs.len() as u64 } else { 0 };
    let rodata_offset = align_up(stubs_offset + stubs_size, 16);
    let rodata_size = plan.rodata.len() as u64;
    let text_filesize = align_up(rodata_offset + rodata_size, page);
    let text_vmsize = text_filesize;

    let data_fileoff = align_up(text_fileoff + text_filesize, page);
    let la_ptr_offset = data_fileoff;
    let la_ptr_size = if has_stubs { 8u64 * externs.len() as u64 } else { 0 };
    let data_filesize = align_up(la_ptr_size, page);
    let data_vmsize = data_filesize;
    vmaddr_data = vmaddr_text + (data_fileoff - text_fileoff);

    let data_seg_index = 2u8;
    let text_seg_index = 1u8;
    let mut rebase_offsets = Vec::new();
    for reloc in &plan.relocs {
        if reloc.kind == RelocKind::Abs64 {
            rebase_offsets.push(text_offset + reloc.offset);
        }
    }
    if matches!(
        std::env::var("FP_NATIVE_MACHO_RELOC_DEBUG"),
        Ok(value) if value == "1" || value.eq_ignore_ascii_case("true")
    ) {
        let max_reloc = rebase_offsets.iter().copied().max().unwrap_or(0);
        eprintln!(
            "[fp-native][macho] text_size={} max_reloc={} reloc_count={}",
            text_size,
            max_reloc,
            rebase_offsets.len()
        );
        for reloc in &plan.relocs {
            if reloc.kind == RelocKind::Abs64 && reloc.offset >= text_size {
                eprintln!(
                    "[fp-native][macho] reloc offset out of text: offset={} text_size={} symbol={}",
                    reloc.offset, text_size, reloc.symbol
                );
            }
        }
    }
    rebase_offsets.sort_unstable();
    let rebase_info = build_rebase_info(&rebase_offsets, text_seg_index);
    let bind_info = build_bind_info(&externs, data_seg_index);
    let lazy_bind_info = Vec::new();
    let (strtab, str_offsets) = build_strtab(&externs);
    let nsyms = 1 + externs.len() as u32;
    let symtab_size = nsyms as u64 * 16;

    let linkedit_fileoff = align_up(data_fileoff + data_filesize, page);
    let rebase_off = 0u64;
    let rebase_size = rebase_info.len() as u64;
    let bind_off = align_up(rebase_off + rebase_size, 8);
    let bind_size = bind_info.len() as u64;
    let lazy_bind_off = align_up(bind_off + bind_size, 8);
    let lazy_bind_size = lazy_bind_info.len() as u64;
    let symoff = align_up(lazy_bind_off + lazy_bind_size, 8);
    let stroff = symoff + symtab_size;
    let strsize = strtab.len() as u64;
    let indirectsymoff = align_up(stroff + strsize, 8);
    let nindirectsyms = if has_stubs {
        (externs.len() as u32) * 2
    } else {
        0
    };
    let indirectsym_size = (nindirectsyms as u64) * 4;
    let linkedit_filesize = align_up(indirectsymoff + indirectsym_size, 16);
    let linkedit_vmsize = align_up(linkedit_filesize, page);
    vmaddr_linkedit = vmaddr_text + (linkedit_fileoff - text_fileoff);

    let entryoff = text_offset + plan.entry_offset;

    let mut out = Vec::new();

    // mach_header_64
    put_u32(&mut out, MH_MAGIC_64);
    put_u32(&mut out, cputype);
    put_u32(&mut out, cpusubtype);
    put_u32(&mut out, MH_EXECUTE);
    put_u32(&mut out, ncmds);
    put_u32(&mut out, sizeofcmds);
    put_u32(&mut out, MH_PIE | MH_DYLDLINK | MH_TWOLEVEL);
    put_u32(&mut out, 0);

    // LC_SEGMENT_64 (__PAGEZERO)
    put_u32(&mut out, LC_SEGMENT_64);
    put_u32(&mut out, lc_segment_pagezero_size as u32);
    put_bytes_fixed::<16>(&mut out, "__PAGEZERO");
    put_u64(&mut out, 0);
    put_u64(&mut out, vmaddr_text);
    put_u64(&mut out, 0);
    put_u64(&mut out, 0);
    put_i32(&mut out, 0);
    put_i32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);

    // LC_SEGMENT_64 (__TEXT)
    put_u32(&mut out, LC_SEGMENT_64);
    put_u32(&mut out, lc_segment_text_size as u32);
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, vmaddr_text);
    put_u64(&mut out, text_vmsize);
    put_u64(&mut out, text_fileoff);
    put_u64(&mut out, text_filesize);
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE);
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE);
    put_u32(&mut out, text_section_count as u32);
    put_u32(&mut out, 0);

    // __text
    put_bytes_fixed::<16>(&mut out, "__text");
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, vmaddr_text + text_offset);
    put_u64(&mut out, text_size);
    put_u32(&mut out, text_offset as u32);
    put_u32(&mut out, 4);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(
        &mut out,
        S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
    );
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);

    if has_stubs {
        put_bytes_fixed::<16>(&mut out, "__stubs");
        put_bytes_fixed::<16>(&mut out, "__TEXT");
        put_u64(&mut out, vmaddr_text + stubs_offset);
        put_u64(&mut out, stubs_size);
        put_u32(&mut out, stubs_offset as u32);
        put_u32(&mut out, stub_align);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u32(
            &mut out,
            S_SYMBOL_STUBS | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
        );
        put_u32(&mut out, 0);
        put_u32(&mut out, stub_size as u32);
        put_u32(&mut out, 0);
    }

    if has_rodata {
        put_bytes_fixed::<16>(&mut out, "__const");
        put_bytes_fixed::<16>(&mut out, "__TEXT");
        put_u64(&mut out, vmaddr_text + rodata_offset);
        put_u64(&mut out, rodata_size);
        put_u32(&mut out, rodata_offset as u32);
        put_u32(&mut out, 4);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u32(&mut out, S_REGULAR);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
    }

    if has_stubs {
        // LC_SEGMENT_64 (__DATA)
        put_u32(&mut out, LC_SEGMENT_64);
        put_u32(&mut out, lc_segment_data_size as u32);
        put_bytes_fixed::<16>(&mut out, "__DATA");
        put_u64(&mut out, vmaddr_data);
        put_u64(&mut out, data_vmsize);
        put_u64(&mut out, data_fileoff);
        put_u64(&mut out, data_filesize);
        put_i32(&mut out, VM_PROT_READ | VM_PROT_WRITE);
        put_i32(&mut out, VM_PROT_READ | VM_PROT_WRITE);
        put_u32(&mut out, data_section_count as u32);
        put_u32(&mut out, 0);

        put_bytes_fixed::<16>(&mut out, "__nl_symbol_ptr");
        put_bytes_fixed::<16>(&mut out, "__DATA");
        put_u64(&mut out, vmaddr_data);
        put_u64(&mut out, la_ptr_size);
        put_u32(&mut out, la_ptr_offset as u32);
        put_u32(&mut out, 3);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
        put_u32(&mut out, S_NON_LAZY_SYMBOL_POINTERS);
        put_u32(&mut out, externs.len() as u32);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
    } else {
        // LC_SEGMENT_64 (__DATA) empty
        put_u32(&mut out, LC_SEGMENT_64);
        put_u32(&mut out, lc_segment_data_size as u32);
        put_bytes_fixed::<16>(&mut out, "__DATA");
        put_u64(&mut out, vmaddr_data);
        put_u64(&mut out, 0);
        put_u64(&mut out, data_fileoff);
        put_u64(&mut out, 0);
        put_i32(&mut out, VM_PROT_READ | VM_PROT_WRITE);
        put_i32(&mut out, VM_PROT_READ | VM_PROT_WRITE);
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
    }

    // LC_SEGMENT_64 (__LINKEDIT)
    put_u32(&mut out, LC_SEGMENT_64);
    put_u32(&mut out, lc_segment_linkedit_size as u32);
    put_bytes_fixed::<16>(&mut out, "__LINKEDIT");
    put_u64(&mut out, vmaddr_linkedit);
    put_u64(&mut out, linkedit_vmsize);
    put_u64(&mut out, linkedit_fileoff);
    put_u64(&mut out, linkedit_filesize);
    put_i32(&mut out, VM_PROT_READ);
    put_i32(&mut out, VM_PROT_READ);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);

    // LC_DYLD_INFO_ONLY
    put_u32(&mut out, LC_DYLD_INFO_ONLY);
    put_u32(&mut out, lc_dyld_info_size as u32);
    let rebase_off_cmd = if rebase_size == 0 {
        0
    } else {
        linkedit_fileoff + rebase_off
    };
    put_u32(&mut out, rebase_off_cmd as u32);
    put_u32(&mut out, rebase_size as u32);
    let bind_off_cmd = if bind_size == 0 {
        0
    } else {
        linkedit_fileoff + bind_off
    };
    let lazy_bind_off_cmd = if lazy_bind_size == 0 {
        0
    } else {
        linkedit_fileoff + lazy_bind_off
    };
    put_u32(&mut out, bind_off_cmd as u32);
    put_u32(&mut out, bind_size as u32);
    put_u32(&mut out, 0); // weak_bind_off
    put_u32(&mut out, 0);
    put_u32(&mut out, lazy_bind_off_cmd as u32);
    put_u32(&mut out, lazy_bind_size as u32);
    put_u32(&mut out, 0); // export_off
    put_u32(&mut out, 0);

    // LC_SYMTAB
    put_u32(&mut out, LC_SYMTAB);
    put_u32(&mut out, lc_symtab_size as u32);
    put_u32(&mut out, (linkedit_fileoff + symoff) as u32);
    put_u32(&mut out, nsyms);
    put_u32(&mut out, (linkedit_fileoff + stroff) as u32);
    put_u32(&mut out, strsize as u32);

    // LC_DYSYMTAB
    put_u32(&mut out, LC_DYSYMTAB);
    put_u32(&mut out, lc_dysymtab_size as u32);
    put_u32(&mut out, 0); // ilocalsym
    put_u32(&mut out, 0); // nlocalsym
    put_u32(&mut out, 0); // iextdefsym
    put_u32(&mut out, 1); // nextdefsym
    put_u32(&mut out, 1); // iundefsym
    put_u32(&mut out, externs.len() as u32);
    put_u32(&mut out, 0); // tocoff
    put_u32(&mut out, 0); // ntoc
    put_u32(&mut out, 0); // modtaboff
    put_u32(&mut out, 0); // nmodtab
    put_u32(&mut out, 0); // extrefsymoff
    put_u32(&mut out, 0); // nextrefsyms
    if has_stubs {
        put_u32(&mut out, (linkedit_fileoff + indirectsymoff) as u32);
        put_u32(&mut out, nindirectsyms);
    } else {
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
    }
    put_u32(&mut out, 0); // extreloff
    put_u32(&mut out, 0); // nextrel
    put_u32(&mut out, 0); // locreloff
    put_u32(&mut out, 0); // nlocrel

    // LC_LOAD_DYLINKER
    let dylinker_start = out.len() as u64;
    put_u32(&mut out, LC_LOAD_DYLINKER);
    put_u32(&mut out, lc_dylinker_size as u32);
    put_u32(&mut out, 12);
    out.extend_from_slice(dyld_path);
    while (out.len() as u64 - dylinker_start) < lc_dylinker_size {
        out.push(0);
    }

    // LC_LOAD_DYLIB
    let dylib_start = out.len() as u64;
    put_u32(&mut out, LC_LOAD_DYLIB);
    put_u32(&mut out, lc_dylib_size as u32);
    put_u32(&mut out, 24);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    out.extend_from_slice(dylib_path);
    while (out.len() as u64 - dylib_start) < lc_dylib_size {
        out.push(0);
    }

    // LC_BUILD_VERSION
    put_u32(&mut out, LC_BUILD_VERSION);
    put_u32(&mut out, lc_build_version_size as u32);
    put_u32(&mut out, 1); // PLATFORM_MACOS
    put_u32(&mut out, 0x000B_0000); // minos 11.0
    put_u32(&mut out, 0x000B_0000); // sdk 11.0
    put_u32(&mut out, 0); // ntools

    // LC_MAIN
    put_u32(&mut out, LC_MAIN);
    put_u32(&mut out, lc_main_size as u32);
    put_u64(&mut out, entryoff);
    put_u64(&mut out, 0);

    ensure(
        out.len() as u64 == header_size + sizeofcmds as u64,
        "internal Mach-O layout error (sizeofcmds mismatch)",
    )?;

    if out.len() as u64 > text_offset {
        return Err(Error::from("internal Mach-O layout error (text offset)"));
    }
    out.resize(text_offset as usize, 0);
    out.extend_from_slice(&plan.text);
    if has_stubs {
        out.resize(stubs_offset as usize, 0);
        out.resize((stubs_offset + stubs_size) as usize, 0);
    }
    if has_rodata {
        out.resize(rodata_offset as usize, 0);
        out.extend_from_slice(&plan.rodata);
    }
    out.resize((text_fileoff + text_filesize) as usize, 0);

    if has_stubs {
        out.resize(data_fileoff as usize, 0);
        out.resize((data_fileoff + data_filesize) as usize, 0);
    }

    out.resize(linkedit_fileoff as usize, 0);
    out.extend_from_slice(&rebase_info);
    out.resize((linkedit_fileoff + bind_off) as usize, 0);
    out.extend_from_slice(&bind_info);
    out.resize((linkedit_fileoff + lazy_bind_off) as usize, 0);
    out.extend_from_slice(&lazy_bind_info);
    out.resize((linkedit_fileoff + symoff) as usize, 0);

    // symtab
    let main_str = *str_offsets
        .get("_main")
        .ok_or_else(|| Error::from("missing _main in strtab"))?;
    put_u32(&mut out, main_str);
    out.push(0x0f); // N_SECT | N_EXT
    out.push(1);
    put_u16(&mut out, 0);
    put_u64(&mut out, vmaddr_text + text_offset + plan.entry_offset);
    for sym in &externs {
        let str = *str_offsets
            .get(&sym.name)
            .ok_or_else(|| Error::from("missing symbol in strtab"))?;
        put_u32(&mut out, str);
        out.push(0x01); // N_UNDF | N_EXT
        out.push(0);
        put_u16(&mut out, 0);
        put_u64(&mut out, 0);
    }

    out.resize((linkedit_fileoff + stroff) as usize, 0);
    out.extend_from_slice(&strtab);
    if has_stubs {
        out.resize((linkedit_fileoff + indirectsymoff) as usize, 0);
        for idx in 0..externs.len() {
            put_u32(&mut out, 1 + idx as u32);
        }
        for idx in 0..externs.len() {
            put_u32(&mut out, 1 + idx as u32);
        }
    }
    out.resize((linkedit_fileoff + linkedit_filesize) as usize, 0);

    // Patch rodata and call relocations.
    let text_addr = vmaddr_text + text_offset;
    let rodata_addr = vmaddr_text + rodata_offset;
    let stubs_addr = vmaddr_text + stubs_offset;
    let ptr_addr = vmaddr_data;

    if has_stubs {
        for (idx, sym) in externs.iter().enumerate() {
            let stub_addr = stubs_addr + sym.stub_offset;
            let ptr = ptr_addr + sym.ptr_offset;
            let start = stubs_offset as usize + (idx as usize * stub_size as usize);
            let end = start + stub_size as usize;
            match arch {
                TargetArch::X86_64 => emit_x86_stub(&mut out[start..end], stub_addr, ptr)?,
                TargetArch::Aarch64 => emit_arm64_stub(&mut out[start..end], stub_addr, ptr)?,
            }
        }
    }

    let resolve_symbol = |name: &str, addend: i64| -> Result<u64> {
        if name == ".rodata" {
            Ok(rodata_addr.wrapping_add(addend as u64))
        } else if let Some(offset) = plan.symbols.get(name) {
            Ok(text_addr.wrapping_add(*offset).wrapping_add(addend as u64))
        } else {
            Err(Error::from("unsupported relocation in Mach-O executable"))
        }
    };

    for reloc in &plan.relocs {
        match reloc.kind {
            RelocKind::Abs64 => {
                let value = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let offset = text_offset as usize + reloc.offset as usize;
                out[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
            }
            RelocKind::Aarch64AdrpAdd => {
                if !matches!(arch, TargetArch::Aarch64) {
                    return Err(Error::from("AArch64 relocation on non-AArch64 target"));
                }
                let target = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let adrp_addr = vmaddr_text + (text_offset - text_fileoff) + reloc.offset;
                let pc_page = adrp_addr & !0xfff;
                let target_page = target & !0xfff;
                let delta_pages = (target_page as i64 - pc_page as i64) >> 12;
                if delta_pages < -(1 << 20) || delta_pages > (1 << 20) - 1 {
                    return Err(Error::from("adrp target out of range"));
                }
                let imm = delta_pages as u32;
                let immlo = imm & 0x3;
                let immhi = (imm >> 2) & 0x7ffff;
                let adrp_offset = text_offset as usize + reloc.offset as usize;
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
            RelocKind::CallRel32 => {
                let target = externs
                    .iter()
                    .find(|sym| sym.original == reloc.symbol)
                    .ok_or_else(|| Error::from("missing external symbol"))?;
                let call_addr = vmaddr_text + (text_offset - text_fileoff) + reloc.offset;
                let stub_addr = stubs_addr + target.stub_offset;
                let offset = text_offset as usize + reloc.offset as usize;
                match arch {
                    TargetArch::X86_64 => {
                        let rel = stub_addr as i64 - (call_addr as i64 + 4);
                        let rel32 =
                            i32::try_from(rel).map_err(|_| Error::from("call target out of range"))?;
                        out[offset..offset + 4].copy_from_slice(&rel32.to_le_bytes());
                    }
                    TargetArch::Aarch64 => {
                        let delta = stub_addr as i64 - call_addr as i64;
                        let imm = delta / 4;
                        if imm < -(1 << 25) || imm > (1 << 25) - 1 {
                            return Err(Error::from("call target out of range"));
                        }
                        let encoded = 0x9400_0000u32 | ((imm as u32) & 0x03FF_FFFF);
                        out[offset..offset + 4].copy_from_slice(&encoded.to_le_bytes());
                    }
                }
            }
        }
    }

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    codesign_if_needed(path)?;
    Ok(())
}

fn codesign_if_needed(path: &Path) -> Result<()> {
    if !cfg!(target_os = "macos") {
        return Ok(());
    }
    if matches!(
        std::env::var("FP_NATIVE_CODESIGN"),
        Ok(value) if value == "0" || value.eq_ignore_ascii_case("false")
    ) {
        return Ok(());
    }
    let status = Command::new("/usr/bin/codesign")
        .arg("-s")
        .arg("-")
        .arg("-f")
        .arg(path)
        .status()
        .map_err(|e| Error::from(e.to_string()))?;
    if !status.success() {
        return Err(Error::from("codesign failed"));
    }
    Ok(())
}

/// Emit a minimal 64-bit Mach-O object file that defines `_main` and returns 0.
///
/// This is a minimal emitter to validate the pipeline and linker
/// integration without LLVM/Cranelift.
///
/// Limitations:
/// - no debug info, relocations, or external calls
/// - ignores input LIR
pub fn emit_object_macho(
    path: &Path,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    // Constants from mach-o/loader.h
    const MH_MAGIC_64: u32 = 0xfeedfacf;
    const MH_OBJECT: u32 = 0x1;
    const MH_SUBSECTIONS_VIA_SYMBOLS: u32 = 0x2000;

    const LC_SEGMENT_64: u32 = 0x19;
    const LC_SYMTAB: u32 = 0x2;

    const VM_PROT_READ: i32 = 0x1;
    const VM_PROT_EXECUTE: i32 = 0x4;

    const S_REGULAR: u32 = 0x0;
    const S_ATTR_PURE_INSTRUCTIONS: u32 = 0x80000000;
    const S_ATTR_SOME_INSTRUCTIONS: u32 = 0x00000400;

    // nlist_64
    const N_SECT: u8 = 0x0e;
    const N_EXT: u8 = 0x01;

    // Choose architecture and machine code.
    // - x86_64: xor eax,eax; ret
    // - arm64: mov w0, #0; ret
    let (cputype, cpusubtype): (u32, u32) = match arch {
        TargetArch::Aarch64 => {
            // CPU_TYPE_ARM64 = CPU_TYPE_ARM | CPU_ARCH_ABI64
            // CPU_TYPE_ARM = 12
            const CPU_TYPE_ARM64: u32 = 0x0100000c;
            const CPU_SUBTYPE_ARM64_ALL: u32 = 0;
            (CPU_TYPE_ARM64, CPU_SUBTYPE_ARM64_ALL)
        }
        TargetArch::X86_64 => {
            const CPU_TYPE_X86_64: u32 = 0x01000007;
            const CPU_SUBTYPE_X86_64_ALL: u32 = 3;
            (CPU_TYPE_X86_64, CPU_SUBTYPE_X86_64_ALL)
        }
    };

    fn pad_to(v: &mut Vec<u8>, align: usize) {
        while v.len() % align != 0 {
            v.push(0);
        }
    }
    fn put_u32(v: &mut Vec<u8>, x: u32) {
        v.extend_from_slice(&x.to_le_bytes());
    }
    fn put_i32(v: &mut Vec<u8>, x: i32) {
        v.extend_from_slice(&x.to_le_bytes());
    }
    fn put_u64(v: &mut Vec<u8>, x: u64) {
        v.extend_from_slice(&x.to_le_bytes());
    }
    fn put_u16(v: &mut Vec<u8>, x: u16) {
        v.extend_from_slice(&x.to_le_bytes());
    }
    fn put_u8(v: &mut Vec<u8>, x: u8) {
        v.push(x);
    }
    fn put_bytes_fixed<const N: usize>(v: &mut Vec<u8>, s: &str) {
        let mut buf = [0u8; N];
        let b = s.as_bytes();
        let n = b.len().min(N);
        buf[..n].copy_from_slice(&b[..n]);
        v.extend_from_slice(&buf);
    }

    // Build load commands first to compute offsets.
    // Header sizes
    let mach_header_64_size = 32usize;

    // segment_command_64 (72) + section_64 (80) = 152
    let seg_cmd_size = 72usize;
    let sect_size = 80usize;
    let lc_segment_size = seg_cmd_size + sect_size;

    let lc_symtab_size = 24usize;

    let ncmds = 2u32;
    let sizeofcmds = (lc_segment_size + lc_symtab_size) as u32;

    // Offsets: place __text immediately after header+load commands, 16-byte aligned.
    let mut text_offset = mach_header_64_size + sizeofcmds as usize;
    let align_text = 16usize;
    text_offset = (text_offset + (align_text - 1)) & !(align_text - 1);

    let mut symoff = text_offset + plan.text.len();
    symoff = (symoff + 7) & !7;

    // One nlist_64 (16 bytes)
    let nsyms = 1u32;
    let symtab_size = 16usize;

    let stroff = symoff + symtab_size;

    // String table: starts with '\0', then "_main\0"
    let strtab: Vec<u8> = {
        let mut s = Vec::new();
        s.push(0);
        s.extend_from_slice(b"_main\0");
        s
    };
    let strsize = strtab.len() as u32;

    // Build file bytes
    let mut out = Vec::new();

    // mach_header_64
    put_u32(&mut out, MH_MAGIC_64);
    put_u32(&mut out, cputype);
    put_u32(&mut out, cpusubtype);
    put_u32(&mut out, MH_OBJECT);
    put_u32(&mut out, ncmds);
    put_u32(&mut out, sizeofcmds);
    put_u32(&mut out, MH_SUBSECTIONS_VIA_SYMBOLS);
    put_u32(&mut out, 0); // reserved

    // LC_SEGMENT_64
    put_u32(&mut out, LC_SEGMENT_64);
    put_u32(&mut out, lc_segment_size as u32);
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, 0); // vmaddr
    put_u64(&mut out, 0); // vmsize
    put_u64(&mut out, 0); // fileoff
    put_u64(&mut out, 0); // filesize
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE); // maxprot
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE); // initprot
    put_u32(&mut out, 1); // nsects
    put_u32(&mut out, 0); // flags

    // section_64 for __text
    put_bytes_fixed::<16>(&mut out, "__text");
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, 0); // addr (for MH_OBJECT often 0)
    put_u64(&mut out, plan.text.len() as u64); // size
    put_u32(&mut out, text_offset as u32); // offset
    put_u32(&mut out, 4); // align as power-of-two (2^4=16)
    put_u32(&mut out, 0); // reloff
    put_u32(&mut out, 0); // nreloc
    put_u32(
        &mut out,
        S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
    );
    put_u32(&mut out, 0); // reserved1
    put_u32(&mut out, 0); // reserved2
    put_u32(&mut out, 0); // reserved3

    // LC_SYMTAB
    put_u32(&mut out, LC_SYMTAB);
    put_u32(&mut out, lc_symtab_size as u32);
    put_u32(&mut out, symoff as u32);
    put_u32(&mut out, nsyms);
    put_u32(&mut out, stroff as u32);
    put_u32(&mut out, strsize);

    // Padding to text
    if out.len() > text_offset {
        return Err(Error::from(
            "internal Mach-O layout error (header larger than text offset)",
        ));
    }
    out.resize(text_offset, 0);
    out.extend_from_slice(&plan.text);

    // Align to symoff
    pad_to(&mut out, 8);
    if out.len() != symoff {
        if out.len() < symoff {
            out.resize(symoff, 0);
        } else {
            return Err(Error::from("internal Mach-O layout error (symoff mismatch)"));
        }
    }

    // nlist_64
    let n_strx = 1u32; // index in strtab where "_main" starts
    put_u32(&mut out, n_strx);
    put_u8(&mut out, N_SECT | N_EXT);
    put_u8(&mut out, 1); // section index
    put_u16(&mut out, 0);
    put_u64(&mut out, plan.entry_offset);

    // String table
    out.extend_from_slice(&strtab);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
