use fp_core::error::{Error, Result};
use std::fs;
use std::path::Path;

/// Emit a minimal 64-bit Mach-O object file that defines `_main` and returns 0.
///
/// This is a macOS-only bootstrapping emitter to validate the pipeline and
/// linker integration without LLVM/Cranelift.
///
/// Limitations:
/// - host-arch only (arm64 or x86_64)
/// - no debug info, relocations, or external calls
/// - ignores input LIR
pub fn emit_object_macho_minimal(path: &Path) -> Result<()> {
    if !cfg!(target_os = "macos") {
        return Err(Error::from(
            "fp-native minimal emitter currently supports macOS host only",
        ));
    }

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
    let (cputype, cpusubtype, text_bytes): (u32, u32, Vec<u8>) = if cfg!(target_arch = "aarch64") {
        // CPU_TYPE_ARM64 = CPU_TYPE_ARM | CPU_ARCH_ABI64
        // CPU_TYPE_ARM = 12
        const CPU_TYPE_ARM64: u32 = 0x0100000c;
        const CPU_SUBTYPE_ARM64_ALL: u32 = 0;
        // mov w0, #0  => 0x52800000
        // ret         => 0xD65F03C0
        let mut t = Vec::new();
        t.extend_from_slice(&0x5280_0000u32.to_le_bytes());
        t.extend_from_slice(&0xD65F_03C0u32.to_le_bytes());
        (CPU_TYPE_ARM64, CPU_SUBTYPE_ARM64_ALL, t)
    } else if cfg!(target_arch = "x86_64") {
        const CPU_TYPE_X86_64: u32 = 0x01000007;
        const CPU_SUBTYPE_X86_64_ALL: u32 = 3;
        (CPU_TYPE_X86_64, CPU_SUBTYPE_X86_64_ALL, vec![0x31, 0xC0, 0xC3])
    } else {
        return Err(Error::from(
            "fp-native minimal emitter supports only aarch64 and x86_64 hosts",
        ));
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
    out.extend_from_slice(&text_bytes);

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
