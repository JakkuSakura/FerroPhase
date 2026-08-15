use super::TargetArch;
use crate::emit::{EmitPlan, RelocKind};
use fp_core::error::{Error, Result};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub fn link_executable_elf64(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    emit_executable_elf64(path, arch, plan)
}

fn align_up(value: usize, align: usize) -> usize {
    (value + (align - 1)) & !(align - 1)
}

fn put_u8(out: &mut Vec<u8>, x: u8) {
    out.push(x);
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

fn elf_machine(arch: TargetArch) -> u16 {
    match arch {
        TargetArch::X86_64 => 62,
        TargetArch::Aarch64 => 183,
    }
}

struct ExternSymbol {
    name: String,
    got_offset: u64,
    plt_offset: u64,
}

fn collect_external_symbols(plan: &EmitPlan, plt_stub_size: usize) -> Vec<ExternSymbol> {
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
            got_offset: 0,
            plt_offset: 0,
        });
    }

    if !seen.contains_key("exit") {
        seen.insert("exit".to_string(), externs.len());
        externs.push(ExternSymbol {
            name: "exit".to_string(),
            got_offset: 0,
            plt_offset: 0,
        });
    }

    for (idx, sym) in externs.iter_mut().enumerate() {
        sym.got_offset = (idx * 8) as u64;
        sym.plt_offset = (idx * plt_stub_size) as u64;
    }
    externs
}

fn dynamic_table_size(extern_count: usize, rela_count: usize) -> usize {
    let mut entries = 1 + 5 + 1; // DT_NEEDED + STRTAB/STRSZ/SYMTAB/SYMENT/HASH + DT_NULL
    entries += 1; // DT_DEBUG
    if rela_count > 0 {
        entries += 3; // DT_RELA/DT_RELASZ/DT_RELAENT
    }
    if extern_count > 0 {
        entries += 1; // DT_PLTGOT
    }
    entries * 16
}

fn build_sysv_hash(symbol_count: usize) -> Vec<u8> {
    let bucket_count = 1u32;
    let mut out = Vec::with_capacity(8 + 4 + symbol_count * 4);
    put_u32(&mut out, bucket_count);
    put_u32(&mut out, symbol_count as u32);
    put_u32(&mut out, u32::from(symbol_count > 1));
    put_u32(&mut out, 0); // The null symbol never participates in a chain.
    for symbol_index in 1..symbol_count {
        let next = if symbol_index + 1 < symbol_count {
            (symbol_index + 1) as u32
        } else {
            0
        };
        put_u32(&mut out, next);
    }
    out
}

fn build_dynstr(externs: &[ExternSymbol]) -> (Vec<u8>, HashMap<String, usize>) {
    let mut offsets = HashMap::new();
    let mut out = vec![0];
    offsets.insert("libc.so.6".to_string(), out.len());
    out.extend_from_slice(b"libc.so.6");
    out.push(0);
    for sym in externs {
        offsets.insert(sym.name.clone(), out.len());
        out.extend_from_slice(sym.name.as_bytes());
        out.push(0);
    }
    (out, offsets)
}

fn put_dyn(out: &mut Vec<u8>, tag: u64, val: u64) {
    put_u64(out, tag);
    put_u64(out, val);
}

fn build_plt_stubs(
    arch: TargetArch,
    externs: &[ExternSymbol],
    plt_addr: u64,
    got_addr: u64,
) -> Vec<u8> {
    match arch {
        TargetArch::X86_64 => {
            let mut out = Vec::with_capacity(externs.len() * 6);
            for sym in externs {
                // jmp *[rip+disp32]
                let stub_addr = plt_addr + sym.plt_offset;
                let target = got_addr + sym.got_offset;
                let disp = (target as i64) - (stub_addr as i64 + 6);
                let disp32 = i32::try_from(disp).unwrap_or(0);
                out.extend_from_slice(&[0xFF, 0x25]);
                out.extend_from_slice(&disp32.to_le_bytes());
            }
            out
        }
        TargetArch::Aarch64 => {
            let mut out = Vec::with_capacity(externs.len() * 24);
            for sym in externs {
                let stub_addr = plt_addr + sym.plt_offset;
                let target = got_addr + sym.got_offset;
                emit_adrp(&mut out, 16, stub_addr, target);
                emit_add_imm12(&mut out, 16, 16, target);
                emit_ldr_reg(&mut out, 16, 16);
                emit_br_reg(&mut out, 16);
            }
            out
        }
    }
}

fn elf_entry_stub_size(arch: TargetArch) -> usize {
    match arch {
        TargetArch::X86_64 => 19,
        TargetArch::Aarch64 => 12,
    }
}

fn build_elf_entry_stub(
    arch: TargetArch,
    entry_addr: u64,
    main_addr: u64,
    exit_addr: u64,
) -> Result<Vec<u8>> {
    match arch {
        TargetArch::X86_64 => {
            let call_site = entry_addr + 5;
            let displacement = main_addr as i64 - (call_site as i64 + 4);
            let displacement = i32::try_from(displacement)
                .map_err(|_| Error::from("ELF entrypoint call target out of range"))?;
            let exit_call_site = entry_addr + 13;
            let exit_displacement = exit_addr as i64 - (exit_call_site as i64 + 4);
            let exit_displacement = i32::try_from(exit_displacement)
                .map_err(|_| Error::from("ELF exit call target out of range"))?;
            let mut out = vec![0x48, 0x83, 0xE4, 0xF0, 0xE8]; // and rsp, -16; call main
            out.extend_from_slice(&displacement.to_le_bytes());
            out.extend_from_slice(&[0x48, 0x89, 0xC7, 0xE8]); // mov rdi, rax; call exit
            out.extend_from_slice(&exit_displacement.to_le_bytes());
            out.extend_from_slice(&[0x0F, 0x0B]); // ud2 if exit unexpectedly returns
            Ok(out)
        }
        TargetArch::Aarch64 => {
            let main_delta = main_addr as i64 - entry_addr as i64;
            let exit_delta = exit_addr as i64 - (entry_addr as i64 + 4);
            if main_delta % 4 != 0 || exit_delta % 4 != 0 {
                return Err(Error::from("unaligned Aarch64 ELF entrypoint"));
            }
            let main_immediate = main_delta / 4;
            let exit_immediate = exit_delta / 4;
            if !(-(1 << 25)..(1 << 25)).contains(&main_immediate)
                || !(-(1 << 25)..(1 << 25)).contains(&exit_immediate)
            {
                return Err(Error::from("ELF entrypoint call target out of range"));
            }
            let main_bl = 0x9400_0000u32 | (main_immediate as u32 & 0x03ff_ffff);
            let exit_bl = 0x9400_0000u32 | (exit_immediate as u32 & 0x03ff_ffff);
            let mut out = Vec::with_capacity(12);
            out.extend_from_slice(&main_bl.to_le_bytes());
            out.extend_from_slice(&exit_bl.to_le_bytes());
            out.extend_from_slice(&0xD420_0000u32.to_le_bytes()); // brk #0 if exit unexpectedly returns
            Ok(out)
        }
    }
}

fn emit_adrp(out: &mut Vec<u8>, rd: u32, pc: u64, target: u64) {
    let pc_page = pc & !0xfffu64;
    let target_page = target & !0xfffu64;
    let delta = target_page.wrapping_sub(pc_page) as i64;
    let imm = delta / 4096;
    let immlo = (imm as u32) & 0x3;
    let immhi = ((imm as u32) >> 2) & 0x7ffff;
    let instr = 0x9000_0000u32 | (immlo << 29) | (immhi << 5) | rd;
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_add_imm12(out: &mut Vec<u8>, rd: u32, rn: u32, target: u64) {
    let imm12 = (target & 0xfff) as u32;
    let instr = 0x9100_0000u32 | (imm12 << 10) | (rn << 5) | rd;
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_ldr_reg(out: &mut Vec<u8>, rt: u32, rn: u32) {
    let instr = 0xF940_0000u32 | (rn << 5) | rt;
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_br_reg(out: &mut Vec<u8>, rn: u32) {
    let instr = 0xD61F_0000u32 | (rn << 5);
    out.extend_from_slice(&instr.to_le_bytes());
}

pub fn emit_executable_elf64(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    let entry_offset = plan.entry_offset.ok_or_else(|| {
        Error::from("native emitter requires a defined main function to produce an executable")
    })?;
    const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
    const ELFCLASS64: u8 = 2;
    const ELFDATA2LSB: u8 = 1;
    const EV_CURRENT: u8 = 1;
    const ET_EXEC: u16 = 2;
    const PT_LOAD: u32 = 1;
    const PT_PHDR: u32 = 6;
    const PT_DYNAMIC: u32 = 2;
    const PT_INTERP: u32 = 3;
    const PF_X: u32 = 1;
    const PF_W: u32 = 2;
    const PF_R: u32 = 4;

    const DT_NULL: u64 = 0;
    const DT_NEEDED: u64 = 1;
    const DT_STRTAB: u64 = 5;
    const DT_SYMTAB: u64 = 6;
    const DT_HASH: u64 = 4;
    const DT_STRSZ: u64 = 10;
    const DT_SYMENT: u64 = 11;
    const DT_RELA: u64 = 7;
    const DT_RELASZ: u64 = 8;
    const DT_RELAENT: u64 = 9;
    const DT_DEBUG: u64 = 21;
    const DT_PLTGOT: u64 = 3;

    const R_X86_64_GLOB_DAT: u32 = 6;
    const R_AARCH64_GLOB_DAT: u32 = 1025;

    let interpreter = match arch {
        TargetArch::X86_64 => b"/lib64/ld-linux-x86-64.so.2\0".as_slice(),
        TargetArch::Aarch64 => b"/lib/ld-linux-aarch64.so.1\0".as_slice(),
    };

    let needs_plt = plan
        .relocs
        .iter()
        .any(|reloc| reloc.kind == RelocKind::CallRel32);
    let plt_stub_size = if needs_plt {
        match arch {
            TargetArch::X86_64 => 6,
            TargetArch::Aarch64 => 24,
        }
    } else {
        0
    };
    let externs = collect_external_symbols(plan, plt_stub_size);
    let ehdr_size = 64usize;
    let phdr_size = 56usize;
    let phnum = 5usize;
    let header_size = ehdr_size + phdr_size * phnum;
    let interp_offset = align_up(header_size, 1);
    let text_offset = align_up(interp_offset + interpreter.len(), 16);
    let entry_stub_size = elf_entry_stub_size(arch);
    let program_text_offset = text_offset + entry_stub_size;
    let rodata_offset = align_up(program_text_offset + plan.text.len(), 16);
    let plt_offset = align_up(rodata_offset + plan.rodata.len(), 16);
    let plt_size = plt_stub_size * externs.len();
    let rx_end = plt_offset + plt_size;

    let data_offset = align_up(rx_end, 0x1000);
    let dynsym_offset = align_up(
        data_offset + dynamic_table_size(externs.len(), externs.len()),
        8,
    );
    let dynsym_size = 24usize * (externs.len() + 1);
    let dynstr_offset = align_up(dynsym_offset + dynsym_size, 1);
    let (dynstr, dynstr_offsets) = build_dynstr(&externs);
    let dynstr_size = dynstr.len();
    let hash = build_sysv_hash(externs.len() + 1);
    let hash_offset = align_up(dynstr_offset + dynstr_size, 4);
    let rela_offset = align_up(hash_offset + hash.len(), 8);
    let rela_size = 24usize * externs.len();
    let got_offset = align_up(rela_offset + rela_size, 8);
    let got_size = 8usize * externs.len();
    let static_data_offset = align_up(got_offset + got_size, 8);
    let data_end = static_data_offset + plan.data.len();

    let _file_size = data_end;

    let base_addr: u64 = 0x400000;
    let entry_addr = base_addr + text_offset as u64;

    let mut out = Vec::new();
    out.extend_from_slice(&ELF_MAGIC);
    put_u8(&mut out, ELFCLASS64);
    put_u8(&mut out, ELFDATA2LSB);
    put_u8(&mut out, EV_CURRENT);
    put_u8(&mut out, 0); // EI_OSABI
    out.resize(16, 0);

    put_u16(&mut out, ET_EXEC);
    put_u16(&mut out, elf_machine(arch));
    put_u32(&mut out, 1);
    put_u64(&mut out, entry_addr);
    put_u64(&mut out, ehdr_size as u64);
    put_u64(&mut out, 0); // e_shoff
    put_u32(&mut out, 0); // e_flags
    put_u16(&mut out, ehdr_size as u16);
    put_u16(&mut out, phdr_size as u16);
    put_u16(&mut out, phnum as u16); // e_phnum
    put_u16(&mut out, 0); // e_shentsize
    put_u16(&mut out, 0); // e_shnum
    put_u16(&mut out, 0); // e_shstrndx

    let rx_filesz = rx_end;
    let rx_memsz = rx_end;
    let data_filesz = data_end - data_offset;
    let data_memsz = data_filesz;
    let dynamic_addr = base_addr + data_offset as u64;
    let dynamic_size = dynamic_table_size(externs.len(), externs.len()) as u64;
    let interp_addr = base_addr + interp_offset as u64;

    // PT_PHDR
    put_u32(&mut out, PT_PHDR);
    put_u32(&mut out, PF_R);
    put_u64(&mut out, ehdr_size as u64);
    put_u64(&mut out, base_addr + ehdr_size as u64);
    put_u64(&mut out, base_addr + ehdr_size as u64);
    put_u64(&mut out, (phdr_size * phnum) as u64);
    put_u64(&mut out, (phdr_size * phnum) as u64);
    put_u64(&mut out, 8);

    // PT_LOAD RX
    put_u32(&mut out, PT_LOAD);
    put_u32(&mut out, PF_R | PF_X);
    put_u64(&mut out, 0);
    put_u64(&mut out, base_addr);
    put_u64(&mut out, base_addr);
    put_u64(&mut out, rx_filesz as u64);
    put_u64(&mut out, rx_memsz as u64);
    put_u64(&mut out, 0x1000);

    // PT_LOAD RW
    put_u32(&mut out, PT_LOAD);
    put_u32(&mut out, PF_R | PF_W);
    put_u64(&mut out, data_offset as u64);
    put_u64(&mut out, base_addr + data_offset as u64);
    put_u64(&mut out, base_addr + data_offset as u64);
    put_u64(&mut out, data_filesz as u64);
    put_u64(&mut out, data_memsz as u64);
    put_u64(&mut out, 0x1000);

    // PT_DYNAMIC
    put_u32(&mut out, PT_DYNAMIC);
    put_u32(&mut out, PF_R | PF_W);
    put_u64(&mut out, data_offset as u64);
    put_u64(&mut out, dynamic_addr);
    put_u64(&mut out, dynamic_addr);
    put_u64(&mut out, dynamic_size);
    put_u64(&mut out, dynamic_size);
    put_u64(&mut out, 8);

    // PT_INTERP
    put_u32(&mut out, PT_INTERP);
    put_u32(&mut out, PF_R);
    put_u64(&mut out, interp_offset as u64);
    put_u64(&mut out, interp_addr);
    put_u64(&mut out, interp_addr);
    put_u64(&mut out, interpreter.len() as u64);
    put_u64(&mut out, interpreter.len() as u64);
    put_u64(&mut out, 1);

    if out.len() > text_offset {
        return Err(Error::from("internal ELF layout error"));
    }

    out.resize(interp_offset, 0);
    out.extend_from_slice(interpreter);
    out.resize(text_offset, 0);
    let plt_addr = base_addr + plt_offset as u64;
    let got_addr = base_addr + got_offset as u64;
    let main_addr = base_addr + program_text_offset as u64 + entry_offset;
    let exit = externs
        .iter()
        .find(|symbol| symbol.name == "exit")
        .ok_or_else(|| Error::from("missing ELF exit symbol"))?;
    let exit_addr = plt_addr + exit.plt_offset;
    out.extend_from_slice(&build_elf_entry_stub(
        arch, entry_addr, main_addr, exit_addr,
    )?);
    out.extend_from_slice(&plan.text);
    out.resize(rodata_offset, 0);
    out.extend_from_slice(&plan.rodata);
    out.resize(plt_offset, 0);
    out.extend_from_slice(&build_plt_stubs(arch, &externs, plt_addr, got_addr));
    out.resize(data_offset, 0);

    let text_addr = base_addr + program_text_offset as u64;
    let rodata_addr = base_addr + rodata_offset as u64;
    let data_addr = base_addr + static_data_offset as u64;
    let resolve_symbol = |name: &str, addend: i64| -> Result<u64> {
        if name == ".rodata" {
            Ok(rodata_addr.wrapping_add(addend as u64))
        } else if let Some(offset) = plan.rodata_symbols.get(name) {
            Ok(rodata_addr
                .wrapping_add(*offset)
                .wrapping_add(addend as u64))
        } else if let Some(offset) = plan.symbols.get(name) {
            Ok(text_addr.wrapping_add(*offset).wrapping_add(addend as u64))
        } else if let Some(offset) = plan.data_symbols.get(name) {
            Ok(data_addr.wrapping_add(*offset).wrapping_add(addend as u64))
        } else {
            Err(Error::from(format!(
                "unsupported relocation in ELF executable: {name}"
            )))
        }
    };

    // .dynamic
    let dynstr_addr = base_addr + dynstr_offset as u64;
    let dynsym_addr = base_addr + dynsym_offset as u64;
    let hash_addr = base_addr + hash_offset as u64;
    let rela_addr = base_addr + rela_offset as u64;
    let mut dynamic = Vec::new();
    let libc_offset = dynstr_offsets
        .get("libc.so.6")
        .copied()
        .ok_or_else(|| Error::from("missing libc in dynstr"))?;
    put_dyn(&mut dynamic, DT_NEEDED, libc_offset as u64);
    put_dyn(&mut dynamic, DT_STRTAB, dynstr_addr);
    put_dyn(&mut dynamic, DT_STRSZ, dynstr_size as u64);
    put_dyn(&mut dynamic, DT_SYMTAB, dynsym_addr);
    put_dyn(&mut dynamic, DT_SYMENT, 24);
    put_dyn(&mut dynamic, DT_HASH, hash_addr);
    put_dyn(&mut dynamic, DT_DEBUG, 0);
    if !externs.is_empty() {
        put_dyn(&mut dynamic, DT_RELA, rela_addr);
        put_dyn(&mut dynamic, DT_RELASZ, rela_size as u64);
        put_dyn(&mut dynamic, DT_RELAENT, 24);
    }
    if !externs.is_empty() {
        put_dyn(&mut dynamic, DT_PLTGOT, got_addr);
    }
    put_dyn(&mut dynamic, DT_NULL, 0);
    out.extend_from_slice(&dynamic);

    // .dynsym
    out.resize(dynsym_offset, 0);
    out.extend_from_slice(&[0u8; 24]);
    for sym in &externs {
        let name_offset = dynstr_offsets
            .get(&sym.name)
            .copied()
            .ok_or_else(|| Error::from("missing symbol in dynstr"))?
            as u32;
        put_u32(&mut out, name_offset);
        out.push(0x12); // STB_GLOBAL | STT_FUNC
        out.push(0);
        put_u16(&mut out, 0);
        put_u64(&mut out, 0);
        put_u64(&mut out, 0);
    }

    // .dynstr
    out.resize(dynstr_offset, 0);
    out.extend_from_slice(&dynstr);

    // .hash
    out.resize(hash_offset, 0);
    out.extend_from_slice(&hash);

    // .rela.dyn
    out.resize(rela_offset, 0);
    let glob_dat_type = match arch {
        TargetArch::X86_64 => R_X86_64_GLOB_DAT,
        TargetArch::Aarch64 => R_AARCH64_GLOB_DAT,
    } as u64;
    for (idx, sym) in externs.iter().enumerate() {
        let r_offset = got_addr + sym.got_offset as u64;
        let sym_index = (idx + 1) as u64;
        let r_info = (sym_index << 32) | glob_dat_type;
        put_u64(&mut out, r_offset);
        put_u64(&mut out, r_info);
        put_u64(&mut out, 0);
    }
    // .got
    out.resize(got_offset, 0);
    out.extend_from_slice(&vec![0u8; got_size]);
    out.resize(static_data_offset, 0);
    out.extend_from_slice(&plan.data);

    // `plan.relocs` are always relative to `plan.text`.
    for reloc in &plan.relocs {
        if reloc.section != crate::emit::RelocSection::Text {
            return Err(Error::from("text relocation has a non-text section"));
        }
        match reloc.kind {
            crate::emit::RelocKind::Abs64 => {
                let value = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let offset = program_text_offset + reloc.offset as usize;
                if offset + 8 > out.len() {
                    return Err(Error::from("relocation offset out of range"));
                }
                out[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
            }
            crate::emit::RelocKind::CallRel32 => {
                let target = externs
                    .iter()
                    .find(|sym| sym.name == reloc.symbol)
                    .ok_or_else(|| Error::from("missing external symbol for relocation"))?;
                let call_site = program_text_offset as i64 + reloc.offset as i64;
                let stub_addr = (base_addr + plt_offset as u64 + target.plt_offset as u64) as i64;
                let offset = program_text_offset + reloc.offset as usize;
                match arch {
                    TargetArch::X86_64 => {
                        let rel = stub_addr - (base_addr as i64 + call_site + 4);
                        let rel32 = i32::try_from(rel)
                            .map_err(|e| Error::from(format!("call target out of range: {e}")))?;
                        out[offset..offset + 4].copy_from_slice(&rel32.to_le_bytes());
                    }
                    TargetArch::Aarch64 => {
                        let delta = stub_addr - (base_addr as i64 + call_site);
                        let imm = delta / 4;
                        if imm < -(1 << 25) || imm > (1 << 25) - 1 {
                            return Err(Error::from("call target out of range"));
                        }
                        let encoded = 0x9400_0000u32 | ((imm as u32) & 0x03FF_FFFF);
                        out[offset..offset + 4].copy_from_slice(&encoded.to_le_bytes());
                    }
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
                let adrp_offset = program_text_offset + reloc.offset as usize;
                let mut adrp = u32::from_le_bytes(
                    out[adrp_offset..adrp_offset + 4]
                        .try_into()
                        .map_err(|e| Error::from(format!("adrp relocation out of range: {e}")))?,
                );
                adrp &= !((0x3 << 29) | (0x7ffff << 5));
                adrp |= (immlo << 29) | (immhi << 5);
                out[adrp_offset..adrp_offset + 4].copy_from_slice(&adrp.to_le_bytes());

                let add_offset = adrp_offset + 4;
                let imm12 = (target & 0xfff) as u32;
                let mut add = u32::from_le_bytes(
                    out[add_offset..add_offset + 4]
                        .try_into()
                        .map_err(|e| Error::from(format!("add relocation out of range: {e}")))?,
                );
                add &= !(0xfff << 10);
                add |= imm12 << 10;
                out[add_offset..add_offset + 4].copy_from_slice(&add.to_le_bytes());
            }
            crate::emit::RelocKind::Aarch64GotLoad => {
                return Err(Error::from(
                    "Aarch64GotLoad relocations are not supported in ELF executables",
                ));
            }
        }
    }

    for reloc in &plan.section_relocs {
        let offset = match reloc.section {
            crate::emit::RelocSection::Text => {
                return Err(Error::from("section relocation targets .text"));
            }
            crate::emit::RelocSection::Rdata => rodata_offset + reloc.offset as usize,
            crate::emit::RelocSection::Data => static_data_offset + reloc.offset as usize,
        };
        match reloc.kind {
            crate::emit::RelocKind::Abs64 => {
                let value = resolve_symbol(&reloc.symbol, reloc.addend)?;
                let bytes = out
                    .get_mut(offset..offset + 8)
                    .ok_or_else(|| Error::from("section relocation offset out of range"))?;
                bytes.copy_from_slice(&value.to_le_bytes());
            }
            _ => return Err(Error::from("unsupported non-text ELF relocation")),
        }
    }

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

pub fn emit_object_elf64(path: &Path, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    crate::link::object_writer::emit_object(path, crate::emit::TargetFormat::Elf, arch, plan)
}
