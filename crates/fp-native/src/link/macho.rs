use fp_core::error::{Error, Result};
use std::fs;
use std::path::Path;

use super::TargetArch;

pub fn link_executable_macho_minimal(path: &Path, arch: TargetArch, text: &[u8]) -> Result<()> {
    emit_executable_macho_minimal(path, arch, text)
}

fn align_up(value: u64, align: u64) -> u64 {
    debug_assert!(align.is_power_of_two());
    (value + (align - 1)) & !(align - 1)
}

fn put_u32(out: &mut Vec<u8>, x: u32) {
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

/// Emit a minimal 64-bit Mach-O executable (`MH_EXECUTE`) that starts at `_main`
/// and returns 0.
///
/// This intentionally avoids the system linker. It does not support dynamic
/// libraries, relocations, or any calls into libc yet.
pub fn emit_executable_macho_minimal(
    path: &Path,
    arch: TargetArch,
    text_bytes: &[u8],
) -> Result<()> {
    // Constants from mach-o/loader.h
    const MH_MAGIC_64: u32 = 0xfeedfacf;
    const MH_EXECUTE: u32 = 0x2;

    const LC_SEGMENT_64: u32 = 0x19;
    const LC_MAIN: u32 = 0x80000028;

    const VM_PROT_READ: i32 = 0x1;
    const VM_PROT_EXECUTE: i32 = 0x4;

    const S_REGULAR: u32 = 0x0;
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

    // File layout:
    // [mach_header_64]
    // [LC_SEGMENT_64(__TEXT) + section_64(__text)]
    // [LC_MAIN]
    // [padding]
    // [__text bytes]

    let header_size = 32u64;
    let segment_cmd_size = 72u64;
    let section_size = 80u64;
    let lc_segment_size = segment_cmd_size + section_size;
    let lc_main_size = 24u64;

    let ncmds = 2u32;
    let sizeofcmds = (lc_segment_size + lc_main_size) as u32;

    // We map __TEXT at the standard macOS base.
    let vmaddr_text: u64 = 0x1000_0000_0;
    let page: u64 = 0x4000; // 16KiB pages on modern macOS / Apple Silicon
    let file_start_of_text = align_up(header_size + sizeofcmds as u64, 16);
    let fileoff_text = align_up(file_start_of_text, page);
    let text_off_in_file = fileoff_text;
    let text_size = text_bytes.len() as u64;
    let filesize_text = align_up(text_size, page);
    let vmsize_text = filesize_text;

    // Entry point is the start of __text.
    let entryoff = text_off_in_file;

    let mut out = Vec::new();

    // mach_header_64
    put_u32(&mut out, MH_MAGIC_64);
    put_u32(&mut out, cputype);
    put_u32(&mut out, cpusubtype);
    put_u32(&mut out, MH_EXECUTE);
    put_u32(&mut out, ncmds);
    put_u32(&mut out, sizeofcmds);
    put_u32(&mut out, 0); // flags
    put_u32(&mut out, 0); // reserved

    // LC_SEGMENT_64 (__TEXT)
    put_u32(&mut out, LC_SEGMENT_64);
    put_u32(&mut out, lc_segment_size as u32);
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, vmaddr_text);
    put_u64(&mut out, vmsize_text);
    put_u64(&mut out, fileoff_text);
    put_u64(&mut out, filesize_text);
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE);
    put_i32(&mut out, VM_PROT_READ | VM_PROT_EXECUTE);
    put_u32(&mut out, 1); // nsects
    put_u32(&mut out, 0); // flags

    // section_64 (__text)
    put_bytes_fixed::<16>(&mut out, "__text");
    put_bytes_fixed::<16>(&mut out, "__TEXT");
    put_u64(&mut out, vmaddr_text); // addr
    put_u64(&mut out, text_size);
    put_u32(&mut out, text_off_in_file as u32);
    put_u32(&mut out, 4); // 2^4 = 16 align
    put_u32(&mut out, 0); // reloff
    put_u32(&mut out, 0); // nreloc
    put_u32(
        &mut out,
        S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
    );
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);

    // LC_MAIN
    put_u32(&mut out, LC_MAIN);
    put_u32(&mut out, lc_main_size as u32);
    put_u64(&mut out, entryoff);
    put_u64(&mut out, 0); // stacksize

    ensure(
        out.len() as u64 == header_size + sizeofcmds as u64,
        "internal Mach-O layout error (sizeofcmds mismatch)",
    )?;

    // Pad to text section file offset
    if out.len() as u64 > text_off_in_file {
        return Err(Error::from("internal Mach-O layout error (text offset)"));
    }
    out.resize(text_off_in_file as usize, 0);
    out.extend_from_slice(text_bytes);
    out.resize((fileoff_text + filesize_text) as usize, 0);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

/// Emit a minimal 64-bit Mach-O object file that defines `_main` and returns 0.
///
/// This is a bootstrapping emitter to validate the pipeline and linker
/// integration without LLVM/Cranelift.
///
/// Limitations:
/// - no debug info, relocations, or external calls
/// - ignores input LIR
pub fn emit_object_macho_minimal(
    path: &Path,
    arch: TargetArch,
    text_bytes: &[u8],
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

    let mut symoff = text_offset + text_bytes.len();
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
    put_u64(&mut out, text_bytes.len() as u64); // size
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
    out.extend_from_slice(text_bytes);

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
    put_u64(&mut out, 0);

    // String table
    out.extend_from_slice(&strtab);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
