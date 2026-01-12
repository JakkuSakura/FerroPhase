use super::TargetArch;
use fp_core::error::{Error, Result};
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

fn pe_text_bytes_x86_64(iat_rva: u32, text_rva: u32) -> Vec<u8> {
    let mut text = Vec::new();
    text.extend_from_slice(&[0x31, 0xC9]); // xor ecx, ecx
    text.extend_from_slice(&[0xFF, 0x15]); // call qword ptr [rip+disp32]
    let next_rva = text_rva + 8;
    let disp = iat_rva.wrapping_sub(next_rva) as i32;
    text.extend_from_slice(&disp.to_le_bytes());
    text.push(0xC3); // ret (ExitProcess should not return)
    text
}

fn pe_text_bytes_aarch64(iat_va: u64) -> Vec<u8> {
    let mut text = Vec::new();
    text.extend_from_slice(&0x5280_0000u32.to_le_bytes()); // mov w0, #0
    text.extend_from_slice(&0x5800_0050u32.to_le_bytes()); // ldr x16, [pc, #8]
    text.extend_from_slice(&0xF940_0210u32.to_le_bytes()); // ldr x16, [x16]
    text.extend_from_slice(&0xD63F_0200u32.to_le_bytes()); // blr x16
    text.extend_from_slice(&iat_va.to_le_bytes()); // literal: &IAT entry
    text
}

pub fn emit_object_coff_minimal(path: &Path, arch: TargetArch, text: &[u8]) -> Result<()> {
    const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
    const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
    const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;

    let header_size = 20u32;
    let section_header_size = 40u32;
    let section_offset = header_size + section_header_size;

    let pointer_to_symbols = section_offset + text.len() as u32;
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
    put_u32(&mut out, text.len() as u32);
    put_u32(&mut out, section_offset);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u32(&mut out, IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ);

    out.extend_from_slice(&text);

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

struct ImportTable {
    bytes: Vec<u8>,
    import_rva: u32,
    iat_rva: u32,
}

fn build_import_table(idata_rva: u32) -> ImportTable {
    let desc_offset = 0u32;
    let null_desc_offset = 20u32;
    let ilt_offset = align_up(desc_offset + null_desc_offset, 8);
    let iat_offset = ilt_offset + 16;
    let hint_name_offset = align_up(iat_offset + 16, 2);
    let dll_name_offset = hint_name_offset + 2 + "ExitProcess".len() as u32 + 1;

    let mut bytes = Vec::new();

    // IMAGE_IMPORT_DESCRIPTOR
    put_u32(&mut bytes, idata_rva + ilt_offset);
    put_u32(&mut bytes, 0);
    put_u32(&mut bytes, 0);
    put_u32(&mut bytes, idata_rva + dll_name_offset);
    put_u32(&mut bytes, idata_rva + iat_offset);

    // Null descriptor
    bytes.resize((null_desc_offset + 20) as usize, 0);

    if bytes.len() < ilt_offset as usize {
        bytes.resize(ilt_offset as usize, 0);
    }

    let hint_name_rva = idata_rva + hint_name_offset;
    put_u64(&mut bytes, hint_name_rva as u64);
    put_u64(&mut bytes, 0);

    if bytes.len() < iat_offset as usize {
        bytes.resize(iat_offset as usize, 0);
    }
    put_u64(&mut bytes, hint_name_rva as u64);
    put_u64(&mut bytes, 0);

    if bytes.len() < hint_name_offset as usize {
        bytes.resize(hint_name_offset as usize, 0);
    }
    put_u16(&mut bytes, 0);
    bytes.extend_from_slice(b"ExitProcess\0");

    if bytes.len() < dll_name_offset as usize {
        bytes.resize(dll_name_offset as usize, 0);
    }
    bytes.extend_from_slice(b"KERNEL32.dll\0");

    ImportTable {
        bytes,
        import_rva: idata_rva + desc_offset,
        iat_rva: idata_rva + iat_offset,
    }
}

pub fn emit_executable_pe64_minimal(
    path: &Path,
    arch: TargetArch,
    text: &[u8],
) -> Result<()> {
    const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x0002;
    const IMAGE_FILE_LARGE_ADDRESS_AWARE: u16 = 0x0020;

    const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
    const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
    const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
    const IMAGE_SCN_MEM_WRITE: u32 = 0x8000_0000;

    const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 3;

    let image_base: u64 = 0x1400_0000_0;
    let section_alignment = 0x1000u32;
    let file_alignment = 0x200u32;

    let dos_header_size = 64u32;
    let pe_offset = 0x80u32;
    let coff_header_size = 20u32;
    let optional_header_size = 0xF0u32;
    let section_header_size = 40u32;

    let headers_size = align_up(
        pe_offset + 4 + coff_header_size + optional_header_size + section_header_size * 2,
        file_alignment,
    );

    let idata_rva = section_alignment * 2;
    let import_table = build_import_table(idata_rva);
    let iat_rva = import_table.iat_rva;

    let text_rva = section_alignment;
    let text = if text.is_empty() {
        match arch {
            TargetArch::X86_64 => pe_text_bytes_x86_64(iat_rva, text_rva),
            TargetArch::Aarch64 => pe_text_bytes_aarch64(image_base + iat_rva as u64),
        }
    } else {
        text.to_vec()
    };

    let text_raw_size = align_up(text.len() as u32, file_alignment);
    let idata_raw_size = align_up(import_table.bytes.len() as u32, file_alignment);

    let text_raw_offset = headers_size;
    let idata_raw_offset = headers_size + text_raw_size;

    let size_of_image = align_up(idata_rva + import_table.bytes.len() as u32, section_alignment);

    let mut out = Vec::new();

    // DOS header
    put_u16(&mut out, 0x5A4D);
    out.resize(dos_header_size as usize, 0);
    out.resize(pe_offset as usize, 0);

    // PE signature
    out.extend_from_slice(b"PE\0\0");

    // COFF header
    put_u16(&mut out, coff_machine(arch));
    put_u16(&mut out, 2);
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
    put_u32(&mut out, idata_raw_size);
    put_u32(&mut out, 0);
    put_u32(&mut out, text_rva);
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

    // Data directory[0] Export Table
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    // Data directory[1] Import Table
    put_u32(&mut out, import_table.import_rva);
    put_u32(&mut out, 40);
    // Remaining directories
    for _ in 2..16 {
        put_u32(&mut out, 0);
        put_u32(&mut out, 0);
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

    put_bytes_fixed::<8>(&mut out, ".idata");
    put_u32(&mut out, import_table.bytes.len() as u32);
    put_u32(&mut out, idata_rva);
    put_u32(&mut out, idata_raw_size);
    put_u32(&mut out, idata_raw_offset);
    put_u32(&mut out, 0);
    put_u32(&mut out, 0);
    put_u16(&mut out, 0);
    put_u16(&mut out, 0);
    put_u32(
        &mut out,
        IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE,
    );

    if out.len() > headers_size as usize {
        return Err(Error::from("internal PE header size mismatch"));
    }
    out.resize(headers_size as usize, 0);

    // .text section
    out.extend_from_slice(&text);
    out.resize((text_raw_offset + text_raw_size) as usize, 0);

    // .idata section
    out.resize(idata_raw_offset as usize, 0);
    out.extend_from_slice(&import_table.bytes);
    out.resize((idata_raw_offset + idata_raw_size) as usize, 0);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}
