use super::TargetArch;
use fp_core::error::{Error, Result};
use std::fs;
use std::path::Path;

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

pub fn emit_executable_elf64_minimal(
    path: &Path,
    arch: TargetArch,
    text: &[u8],
) -> Result<()> {
    const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];
    const ELFCLASS64: u8 = 2;
    const ELFDATA2LSB: u8 = 1;
    const EV_CURRENT: u8 = 1;
    const ET_EXEC: u16 = 2;
    const PT_LOAD: u32 = 1;
    const PF_X: u32 = 1;
    const PF_R: u32 = 4;

    let ehdr_size = 64usize;
    let phdr_size = 56usize;
    let code_offset = align_up(ehdr_size + phdr_size, 0x1000);
    let file_size = code_offset + text.len();

    let base_addr: u64 = 0x400000;
    let entry_addr = base_addr + code_offset as u64;

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
    put_u16(&mut out, 1); // e_phnum
    put_u16(&mut out, 0); // e_shentsize
    put_u16(&mut out, 0); // e_shnum
    put_u16(&mut out, 0); // e_shstrndx

    // Program header
    put_u32(&mut out, PT_LOAD);
    put_u32(&mut out, PF_R | PF_X);
    put_u64(&mut out, 0);
    put_u64(&mut out, base_addr);
    put_u64(&mut out, base_addr);
    put_u64(&mut out, file_size as u64);
    put_u64(&mut out, file_size as u64);
    put_u64(&mut out, 0x1000);

    if out.len() > code_offset {
        return Err(Error::from("internal ELF layout error"));
    }
    out.resize(code_offset, 0);
    out.extend_from_slice(text);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

pub fn emit_object_elf64_minimal(
    path: &Path,
    arch: TargetArch,
    text: &[u8],
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

    let shstrtab = b"\0.text\0.symtab\0.strtab\0.shstrtab\0";
    let strtab = b"\0_start\0";

    let mut offset = 64usize; // ELF header size
    let text_offset = align_up(offset, 16);
    offset = text_offset + text.len();

    let symtab_offset = align_up(offset, 8);
    let symtab_entry_size = 24usize;
    let symtab_count = 2usize;
    offset = symtab_offset + symtab_entry_size * symtab_count;

    let strtab_offset = align_up(offset, 1);
    offset = strtab_offset + strtab.len();

    let shstrtab_offset = align_up(offset, 1);
    offset = shstrtab_offset + shstrtab.len();

    let shoff = align_up(offset, 8);
    let shnum = 5u16;

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
    out.extend_from_slice(&text);

    out.resize(symtab_offset, 0);
    // Null symbol
    out.extend_from_slice(&[0u8; 24]);
    // _start symbol
    put_u32(&mut out, 1); // st_name offset in strtab
    put_u8(&mut out, (STB_GLOBAL << 4) | STT_FUNC);
    put_u8(&mut out, 0);
    put_u16(&mut out, 1); // section index (.text)
    put_u64(&mut out, 0);
    put_u64(&mut out, text.len() as u64);

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
    put_u64(&mut out, text.len() as u64);
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

    // Section 4: .shstrtab
    put_u32(&mut out, 23);
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
