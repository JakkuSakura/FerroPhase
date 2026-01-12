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

    for (idx, sym) in externs.iter_mut().enumerate() {
        sym.got_offset = (idx * 8) as u64;
        sym.plt_offset = (idx * plt_stub_size) as u64;
    }
    externs
}

fn dynamic_table_size(extern_count: usize) -> usize {
    let mut entries = 1 + 4 + 1; // DT_NEEDED + STRTAB/STRSZ/SYMTAB/SYMENT + DT_NULL
    if extern_count > 0 {
        entries += 4; // DT_RELA/DT_RELASZ/DT_RELAENT/DT_PLTGOT
    }
    entries * 16
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
                let target = got_addr + sym.got_offset;
                emit_movz(&mut out, 16, (target & 0xffff) as u16, 0);
                emit_movk(&mut out, 16, ((target >> 16) & 0xffff) as u16, 16);
                emit_movk(&mut out, 16, ((target >> 32) & 0xffff) as u16, 32);
                emit_movk(&mut out, 16, ((target >> 48) & 0xffff) as u16, 48);
                emit_ldr_reg(&mut out, 16, 16);
                emit_br_reg(&mut out, 16);
            }
            out
        }
    }
}

fn emit_movz(out: &mut Vec<u8>, rd: u32, imm: u16, shift: u32) {
    let hw = shift / 16;
    let instr = 0xD280_0000u32 | ((imm as u32) << 5) | (hw << 21) | rd;
    out.extend_from_slice(&instr.to_le_bytes());
}

fn emit_movk(out: &mut Vec<u8>, rd: u32, imm: u16, shift: u32) {
    let hw = shift / 16;
    let instr = 0xF280_0000u32 | ((imm as u32) << 5) | (hw << 21) | rd;
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

pub fn emit_executable_elf64(
    path: &Path,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
    const ELFCLASS64: u8 = 2;
    const ELFDATA2LSB: u8 = 1;
    const EV_CURRENT: u8 = 1;
    const ET_EXEC: u16 = 2;
    const PT_LOAD: u32 = 1;
    const PT_DYNAMIC: u32 = 2;
    const PT_INTERP: u32 = 3;
    const PF_X: u32 = 1;
    const PF_W: u32 = 2;
    const PF_R: u32 = 4;

    const DT_NULL: u64 = 0;
    const DT_NEEDED: u64 = 1;
    const DT_STRTAB: u64 = 5;
    const DT_SYMTAB: u64 = 6;
    const DT_STRSZ: u64 = 10;
    const DT_SYMENT: u64 = 11;
    const DT_RELA: u64 = 7;
    const DT_RELASZ: u64 = 8;
    const DT_RELAENT: u64 = 9;
    const DT_PLTGOT: u64 = 3;

    const R_X86_64_GLOB_DAT: u32 = 6;
    const R_AARCH64_GLOB_DAT: u32 = 1025;

    let interpreter = match arch {
        TargetArch::X86_64 => b"/lib64/ld-linux-x86-64.so.2\0".as_slice(),
        TargetArch::Aarch64 => b"/lib/ld-linux-aarch64.so.1\0".as_slice(),
    };

    let needs_plt = plan.relocs.iter().any(|reloc| reloc.kind == RelocKind::CallRel32);
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
    let phnum = 4usize;
    let header_size = ehdr_size + phdr_size * phnum;
    let interp_offset = align_up(header_size, 1);
    let text_offset = align_up(interp_offset + interpreter.len(), 16);
    let rodata_offset = align_up(text_offset + plan.text.len(), 16);
    let plt_offset = align_up(rodata_offset + plan.rodata.len(), 16);
    let plt_size = plt_stub_size * externs.len();
    let rx_end = plt_offset + plt_size;

    let data_offset = align_up(rx_end, 0x1000);
    let dynsym_offset = align_up(data_offset + dynamic_table_size(externs.len()), 8);
    let dynsym_size = 24usize * (externs.len() + 1);
    let dynstr_offset = align_up(dynsym_offset + dynsym_size, 1);
    let (dynstr, dynstr_offsets) = build_dynstr(&externs);
    let dynstr_size = dynstr.len();
    let rela_offset = align_up(dynstr_offset + dynstr_size, 8);
    let rela_size = 24usize * externs.len();
    let got_offset = align_up(rela_offset + rela_size, 8);
    let got_size = 8usize * externs.len();
    let data_end = got_offset + got_size;

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
    let dynamic_size = dynamic_table_size(externs.len()) as u64;
    let interp_addr = base_addr + interp_offset as u64;

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
    out.extend_from_slice(&plan.text);
    out.resize(rodata_offset, 0);
    out.extend_from_slice(&plan.rodata);
    out.resize(plt_offset, 0);
    let plt_addr = base_addr + plt_offset as u64;
    let got_addr = base_addr + got_offset as u64;
    out.extend_from_slice(&build_plt_stubs(arch, &externs, plt_addr, got_addr));
    out.resize(data_offset, 0);

    // .dynamic
    let dynstr_addr = base_addr + dynstr_offset as u64;
    let dynsym_addr = base_addr + dynsym_offset as u64;
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
    if !externs.is_empty() {
        put_dyn(&mut dynamic, DT_RELA, rela_addr);
        put_dyn(&mut dynamic, DT_RELASZ, rela_size as u64);
        put_dyn(&mut dynamic, DT_RELAENT, 24);
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
            .ok_or_else(|| Error::from("missing symbol in dynstr"))? as u32;
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

    // .rela.dyn
    out.resize(rela_offset, 0);
    let reloc_type = match arch {
        TargetArch::X86_64 => R_X86_64_GLOB_DAT,
        TargetArch::Aarch64 => R_AARCH64_GLOB_DAT,
    } as u64;
    for (idx, sym) in externs.iter().enumerate() {
        let r_offset = got_addr + sym.got_offset as u64;
        let sym_index = (idx + 1) as u64;
        let r_info = (sym_index << 32) | reloc_type;
        put_u64(&mut out, r_offset);
        put_u64(&mut out, r_info);
        put_u64(&mut out, 0);
    }

    // .got
    out.resize(got_offset, 0);
    out.extend_from_slice(&vec![0u8; got_size]);

    // Patch relocations in text (rodata + calls).
    let rodata_addr = base_addr + rodata_offset as u64;
    for reloc in &plan.relocs {
        match reloc.kind {
            crate::emit::RelocKind::Abs64 => {
                if reloc.symbol != ".rodata" {
                    return Err(Error::from("unsupported relocation in ELF executable"));
                }
                let value = rodata_addr.wrapping_add(reloc.addend as u64);
                let offset = text_offset + reloc.offset as usize;
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
                let call_site = text_offset as i64 + reloc.offset as i64;
                let stub_addr = (base_addr + plt_offset as u64 + target.plt_offset as u64) as i64;
                let offset = text_offset + reloc.offset as usize;
                match arch {
                    TargetArch::X86_64 => {
                        let rel = stub_addr - (base_addr as i64 + call_site + 4);
                        let rel32 = i32::try_from(rel)
                            .map_err(|_| Error::from("call target out of range"))?;
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
        }
    }

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

pub fn emit_object_elf64(
    path: &Path,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
    const ELFCLASS64: u8 = 2;
    const ELFDATA2LSB: u8 = 1;
    const EV_CURRENT: u8 = 1;
    const ET_REL: u16 = 1;
    const SHT_PROGBITS: u32 = 1;
    const SHT_SYMTAB: u32 = 2;
    const SHT_STRTAB: u32 = 3;
    const SHF_ALLOC: u64 = 0x2;
    const SHF_EXECINSTR: u64 = 0x4;

    const STB_GLOBAL: u8 = 1;
    const STT_FUNC: u8 = 2;

    let shstrtab = b"\0.text\0.symtab\0.strtab\0.rodata\0.shstrtab\0";
    let strtab = b"\0_start\0";

    let mut offset = 64usize; // ELF header size
    let text_offset = align_up(offset, 16);
    offset = text_offset + plan.text.len();

    let rodata_offset = align_up(offset, 16);
    offset = rodata_offset + plan.rodata.len();

    let symtab_offset = align_up(offset, 8);
    let symtab_entry_size = 24usize;
    let symtab_count = 2usize;
    offset = symtab_offset + symtab_entry_size * symtab_count;

    let strtab_offset = align_up(offset, 1);
    offset = strtab_offset + strtab.len();

    let shstrtab_offset = align_up(offset, 1);
    offset = shstrtab_offset + shstrtab.len();

    let shoff = align_up(offset, 8);
    let shnum = 6u16;

    let mut out = Vec::new();
    out.extend_from_slice(&ELF_MAGIC);
    put_u8(&mut out, ELFCLASS64);
    put_u8(&mut out, ELFDATA2LSB);
    put_u8(&mut out, EV_CURRENT);
    put_u8(&mut out, 0);
    out.resize(16, 0);

    put_u16(&mut out, ET_REL);
    put_u16(&mut out, elf_machine(arch));
    put_u32(&mut out, 1);
    put_u64(&mut out, 0);
    put_u64(&mut out, 0);
    put_u64(&mut out, shoff as u64);
    put_u32(&mut out, 0);
    put_u16(&mut out, 64);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 64);
    put_u16(&mut out, shnum);
    put_u16(&mut out, 4); // shstrndx

    if out.len() > text_offset {
        return Err(Error::from("internal ELF layout error"));
    }
    out.resize(text_offset, 0);
    out.extend_from_slice(&plan.text);

    out.resize(rodata_offset, 0);
    out.extend_from_slice(&plan.rodata);

    out.resize(symtab_offset, 0);
    // Null symbol
    out.extend_from_slice(&[0u8; 24]);
    // _start symbol
    put_u32(&mut out, 1); // st_name offset in strtab
    put_u8(&mut out, (STB_GLOBAL << 4) | STT_FUNC);
    put_u8(&mut out, 0);
    put_u16(&mut out, 1); // section index (.text)
    put_u64(&mut out, 0);
    put_u64(&mut out, plan.text.len() as u64);

    out.resize(strtab_offset, 0);
    out.extend_from_slice(strtab);

    out.resize(shstrtab_offset, 0);
    out.extend_from_slice(shstrtab);

    out.resize(shoff, 0);

    // Section 0: null
    out.extend_from_slice(&[0u8; 64]);

    // Section 1: .text
    put_u32(&mut out, 1); // sh_name
    put_u32(&mut out, SHT_PROGBITS);
    put_u64(&mut out, SHF_ALLOC | SHF_EXECINSTR);
    put_u64(&mut out, 0);
    put_u64(&mut out, text_offset as u64);
    put_u64(&mut out, plan.text.len() as u64);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u64(&mut out, 16);
    put_u64(&mut out, 0);

    // Section 2: .symtab
    put_u32(&mut out, 7);
    put_u32(&mut out, SHT_SYMTAB);
    put_u64(&mut out, 0);
    put_u64(&mut out, 0);
    put_u64(&mut out, symtab_offset as u64);
    put_u64(&mut out, (symtab_entry_size * symtab_count) as u64);
    put_u32(&mut out, 3); // link to .strtab
    put_u32(&mut out, 1); // one local symbol
    put_u64(&mut out, 8);
    put_u64(&mut out, symtab_entry_size as u64);

    // Section 3: .strtab
    put_u32(&mut out, 15);
    put_u32(&mut out, SHT_STRTAB);
    put_u64(&mut out, 0);
    put_u64(&mut out, 0);
    put_u64(&mut out, strtab_offset as u64);
    put_u64(&mut out, strtab.len() as u64);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u64(&mut out, 1);
    put_u64(&mut out, 0);

    // Section 4: .rodata
    put_u32(&mut out, 23);
    put_u32(&mut out, SHT_PROGBITS);
    put_u64(&mut out, SHF_ALLOC);
    put_u64(&mut out, 0);
    put_u64(&mut out, rodata_offset as u64);
    put_u64(&mut out, plan.rodata.len() as u64);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u64(&mut out, 16);
    put_u64(&mut out, 0);

    // Section 5: .shstrtab
    put_u32(&mut out, 31);
    put_u32(&mut out, SHT_STRTAB);
    put_u64(&mut out, 0);
    put_u64(&mut out, 0);
    put_u64(&mut out, shstrtab_offset as u64);
    put_u64(&mut out, shstrtab.len() as u64);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u64(&mut out, 1);
    put_u64(&mut out, 0);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
